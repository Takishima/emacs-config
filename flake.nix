{
  description = "Damien's Emacs configuration: straight.el without Nix, home-manager with it";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      flake = false;
    };
    # Sources straight clones from GitHub and nixpkgs does not carry.
    # Bump with `nix flake update <name>'.
    claude-code-ide = {
      url = "github:manzaltu/claude-code-ide.el";
      flake = false;
    };
    docker-compose-mode = {
      url = "github:meqif/docker-compose-mode";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }@inputs:
    let
      inherit (nixpkgs) lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = lib.genAttrs systems;
      # nixpkgs' copilot package carries copilot-language-server, which is unfree.
      pkgsFor = lib.genAttrs systems (
        system:
        import nixpkgs {
          inherit system;
          config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "copilot-language-server" ];
        }
      );
      elisp = import ./nix/elisp-packages.nix { inherit inputs; };
      emacsFor =
        pkgs: base:
        ((pkgs.emacsPackagesFor base).overrideScope (elisp.overrides pkgs)).emacsWithPackages (
          epkgs: elisp.packages epkgs ++ [ epkgs.treesit-grammars.with-all-grammars ]
        );
    in
    {
      lib = {
        inherit (elisp) names packages overrides;
        inherit emacsFor;
      };

      homeModules.default = import ./nix/module.nix {
        emacsConfig = self;
        inherit inputs;
      };
      homeManagerModules = self.homeModules;

      packages = forAllSystems (
        system:
        let
          pkgs = pkgsFor.${system};
        in
        rec {
          emacs = emacsFor pkgs pkgs.emacs-pgtk;
          default = emacs;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsFor.${system};
        in
        {
          default = pkgs.mkShell {
            packages = [
              self.packages.${system}.emacs
              pkgs.git
              pkgs.gnumake
            ];
            DN_PACKAGE_MANAGER = "nix";
          };
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = pkgsFor.${system};
        in
        {
          default = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "emacs-config" ''
                export DN_PACKAGE_MANAGER=nix
                export DN_EMACS_LOCAL_DIR="''${XDG_CONFIG_HOME:-$HOME/.config}/emacs-config/"
                mkdir -p "$DN_EMACS_LOCAL_DIR"
                exec ${self.packages.${system}.emacs}/bin/emacs --init-directory ${self} "$@"
              ''
            );
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = pkgsFor.${system};
          tools = import ./nix/tools.nix { inherit pkgs; };
          # A make target in the sandbox: the wrapped Emacs, nix mode, the
          # per-host files in a local directory as from the store, no network.
          batch =
            target:
            pkgs.runCommand "emacs-config-${target}"
              {
                nativeBuildInputs = [
                  self.packages.${system}.emacs
                  pkgs.gnumake
                ]
                ++ tools.check;
                env.DN_PACKAGE_MANAGER = "nix";
              }
              ''
                export HOME=$TMPDIR EMACS_USER_DIRECTORY=$TMPDIR/emacs.d SHELL=${pkgs.bash}/bin/bash
                export DN_EMACS_LOCAL_DIR=$TMPDIR/local/
                mkdir -p "$EMACS_USER_DIRECTORY" "$DN_EMACS_LOCAL_DIR"
                echo '(defvar dn-smoke-local-init-pre t)' > "$DN_EMACS_LOCAL_DIR/init-pre.el"
                cd ${self}
                make ${target} EMACS=emacs
                touch $out
              '';
          homeConfig =
            module:
            home-manager.lib.homeManagerConfiguration {
              inherit pkgs;
              modules = [
                self.homeModules.default
                {
                  home.username = "check";
                  home.homeDirectory = "/home/check";
                  home.stateVersion = "24.05";
                }
                module
              ];
            };
          # Evaluating the activation package proves the module and the
          # package set evaluate; nothing is built.  The generated init.el, or
          # the symlink target, is kept for inspection.
          moduleEval =
            name: module: initOk:
            let
              hm = homeConfig module;
              init = hm.config.home.file.".emacs.d/init.el";
              checkout = hm.config.programs.emacs-config.checkout;
            in
            assert lib.assertMsg (initOk init) "module-${name}: unexpected ~/.emacs.d/init.el";
            builtins.seq hm.activationPackage.drvPath (
              pkgs.writeText "emacs-config-module-${name}" (
                if init.text != null then init.text else "symlink to ${checkout}/init.el"
              )
            );
        in
        {
          smoke = batch "check";
          compile = batch "compile";
          packages = batch "packages";
          package-list =
            pkgs.runCommand "emacs-config-package-list" { nativeBuildInputs = [ pkgs.emacs-nox ]; }
              ''
                emacs --batch -l ${self}/test/packages.el ${self} > elisp.txt
                printf '%s\n' ${lib.escapeShellArgs elisp.names} | LC_ALL=C sort > nix.txt
                diff -u elisp.txt nix.txt
                touch $out
              '';
          module-store =
            moduleEval "store"
              {
                programs.emacs-config = {
                  enable = true;
                  tools.enableAll = true;
                };
              }
              (
                init:
                lib.hasInfix "(setq dn-package-manager 'nix)" init.text
                && lib.hasInfix "(setq config-local-dir \"/home/check/.config/emacs-config/\")" init.text
              );
          module-checkout =
            moduleEval "checkout"
              {
                programs.emacs-config = {
                  enable = true;
                  checkout = "/home/check/src/emacs-config";
                  manageElispPackages = false;
                };
                services.emacs.enable = true;
              }
              (init: init.text == null);
          wrapper =
            let
              emacs = (homeConfig { programs.emacs-config.enable = true; }).config.programs.emacs-config.finalPackage;
            in
            pkgs.runCommand "emacs-config-wrapper" { } ''
              grep -qF "DN_PACKAGE_MANAGER='nix'" ${emacs}/bin/emacs
              grep -qF ${pkgs.delta}/bin ${emacs}/bin/emacs
              touch $out
            '';
        }
      );
    };
}
