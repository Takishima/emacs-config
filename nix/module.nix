# Home-manager module: the configuration, its packages and its tools.
# `programs.emacs-config' builds on home-manager's own `programs.emacs'
# (package, extraPackages, overrides) and hands `services.emacs' the wrapped
# Emacs, so the two can be combined with either of those modules.
{ emacsConfig, inputs }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.emacs-config;
  elisp = import ./elisp-packages.nix { inherit inputs; };
  tools = import ./tools.nix { inherit pkgs; };
  groupNames = builtins.attrNames tools.groups;

  live = cfg.checkout != null;
  src = if live then cfg.checkout else toString emacsConfig;
  mode = if cfg.manageElispPackages then "nix" else "straight";
  # Step 1 with a live checkout: the README's two symlinks, nothing generated.
  plainSymlinks = live && !cfg.manageElispPackages;

  initEl = ''
    ;; Written by home-manager (programs.emacs-config); edit the flake, not this file.
    (setq dn-package-manager '${mode})
    ${lib.optionalString (!live) ''(setq config-local-dir "${cfg.localDir}/")''}
    (load "${src}/init.el" nil 'nomessage)
  '';

  enabledGroups = lib.filter (g: cfg.tools.${g}.enable) groupNames;
  # Fonts go to the profile, where fontconfig looks; the rest on the wrapper's PATH.
  pathTools =
    cfg.tools.base
    ++ cfg.tools.extraPackages
    ++ lib.concatMap (g: cfg.tools.${g}.packages) (lib.remove "fonts" enabledGroups);
  fontPackages = lib.optionals cfg.tools.fonts.enable cfg.tools.fonts.packages;

  base = config.programs.emacs.finalPackage;

  # home-manager's finalPackage with bin/emacs wrapped: the tools on PATH
  # and the mode in the environment, whatever launches it.
  wrapped = pkgs.symlinkJoin {
    name = "emacs-config-${lib.getVersion base}";
    paths = [ base ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      rm $out/bin/emacs
      makeWrapper ${base}/bin/emacs $out/bin/emacs \
        --prefix PATH : ${lib.makeBinPath pathTools} \
        --set DN_PACKAGE_MANAGER ${mode}
    '';
    inherit (base) meta;
  };

  toolOptions = lib.mapAttrs (name: group: {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = cfg.tools.enableAll && group.everyday;
      defaultText = lib.literalExpression (
        if group.everyday then "config.programs.emacs-config.tools.enableAll" else "false"
      );
      description = "Whether to put ${group.description} on the wrapped Emacs's PATH.";
    };
    packages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = group.packages;
      defaultText = lib.literalExpression "(import ./nix/tools.nix { inherit pkgs; }).groups.${name}.packages";
      description = "Packages of the ${name} group.";
    };
  }) tools.groups;
in
{
  options.programs.emacs-config = {
    enable = lib.mkEnableOption "the takishima/emacs-config Emacs configuration";

    package = lib.mkOption {
      type = lib.types.package;
      default = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs else pkgs.emacs-pgtk;
      defaultText = lib.literalExpression "pkgs.emacs-pgtk, or pkgs.emacs on darwin";
      description = ''
        Emacs to configure; the config needs 29.1 or newer.  Sets
        `programs.emacs.package' unless that is set explicitly.
      '';
    };

    treesitGrammars = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Add every tree-sitter grammar in nixpkgs to the Emacs wrapper, where
        `treesit' finds them.  Turn it off if `programs.emacs.extraPackages'
        already adds `treesit-grammars.with-all-grammars'.
      '';
    };

    elispPackages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = elisp.names;
      defaultText = lib.literalExpression "(import ./nix/elisp-packages.nix { inherit inputs; }).names";
      example = lib.literalExpression ''lib.remove "copilot" options.programs.emacs-config.elispPackages.default'';
      description = ''
        Names in `emacsPackages' of the packages built into the Emacs wrapper
        when `manageElispPackages' is on: everything the config declares with
        `:straight', as `test/packages.el' prints it.  A name removed here
        leaves its package out, so its `use-package' form must be gated or
        the init file stops there; `copilot' is the one to drop to avoid the
        unfree copilot-language-server.  Add packages the config does not
        declare through `programs.emacs.extraPackages'.
      '';
    };

    manageElispPackages = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Build every package the config declares with `:straight' into the
        Emacs wrapper and start Emacs with `dn-package-manager' set to `nix',
        so straight.el never runs and nothing is fetched at startup.  Off, the
        files are still placed but straight bootstraps and clones packages as
        it does without Nix; git is added to the profile for it.  nixpkgs'
        copilot package depends on the unfree copilot-language-server, which
        `nixpkgs.config.allowUnfreePredicate' must allow.
      '';
    };

    checkout = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "/home/alice/src/emacs-config";
      description = ''
        Absolute path of a live checkout to load instead of the flake input's
        copy in the store.  Edits apply on the next Emacs start without a
        switch, and the untracked per-host files stay in the checkout.  The
        package list still comes from the input.
      '';
    };

    localDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.xdg.configHome}/emacs-config";
      defaultText = lib.literalExpression "\"\${config.xdg.configHome}/emacs-config\"";
      description = ''
        Directory of custom.el, init-pre.el, init-post.el and module
        overrides when the config is loaded from the store; ignored with
        `checkout'.
      '';
    };

    extraOverrides = lib.mkOption {
      type = lib.types.functionTo (lib.types.functionTo lib.types.attrs);
      default = _: _: { };
      example = lib.literalExpression "self: super: { org = super.org.overrideAttrs (_: { }); }";
      description = ''
        emacsPackages overlay applied after this module's own.  Use it rather
        than a second `programs.emacs.overrides' definition: home-manager
        merges that option by attribute name, so another definition of
        lsp-mode would silently drop the LSP_USE_PLISTS rebuild.
      '';
    };

    finalPackage = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      description = ''
        `programs.emacs.finalPackage' with bin/emacs wrapped to put the
        enabled tools on PATH and select the package manager.  Installed in
        the profile ahead of the unwrapped one, and offered to
        `services.emacs.package'.
      '';
    };

    tools = {
      enableAll = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Enable every everyday group: the tools the language modules call
          for search, version control, shells, C and C++, Go, Python, Nix,
          web, YAML, JSON, TOML, Markdown, Docker, Ansible, CMake and
          Terraform.  The tex, debuggers, ai, gnuplot, povray, windows and
          fonts groups stay separate.
        '';
      };
      base = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = tools.base;
        defaultText = lib.literalExpression "(import ./nix/tools.nix { inherit pkgs; }).base";
        description = ''
          Always on the wrapped Emacs's PATH: aspell with its dictionaries and
          delta, which `:ensure-system-package' would otherwise try to install.
        '';
      };
      installInProfile = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Also add the enabled tools to the profile, at low priority, so
          shells see them.  Off, they are only on the wrapped Emacs's PATH,
          where they come before the profile's.  Fonts always go to the
          profile.
        '';
      };
      extraPackages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
        description = "Extra binaries the config should find on PATH.";
      };
    }
    // toolOptions;
  };

  config = lib.mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = lib.mkDefault cfg.package;
      extraPackages =
        epkgs:
        lib.optionals cfg.manageElispPackages (map (name: epkgs.${name}) cfg.elispPackages)
        ++ lib.optionals cfg.treesitGrammars [ epkgs.treesit-grammars.with-all-grammars ]
        ++ lib.optionals cfg.tools.fonts.enable [ epkgs.nerd-icons ];
      overrides = lib.mkIf cfg.manageElispPackages (
        lib.composeExtensions (elisp.overrides pkgs) cfg.extraOverrides
      );
    };

    programs.emacs-config.finalPackage = wrapped;
    services.emacs.package = lib.mkDefault wrapped;

    home.file =
      if plainSymlinks then
        {
          ".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink "${cfg.checkout}/init.el";
          ".emacs.d/early-init.el".source =
            config.lib.file.mkOutOfStoreSymlink "${cfg.checkout}/early-init.el";
        }
      else
        {
          ".emacs.d/init.el".text = initEl;
          ".emacs.d/early-init.el".source =
            if live then
              config.lib.file.mkOutOfStoreSymlink "${cfg.checkout}/early-init.el"
            else
              "${emacsConfig}/early-init.el";
        };

    home.activation.emacsConfigLocalDir = lib.mkIf (!live) (
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        run mkdir -p ${lib.escapeShellArg cfg.localDir}
      ''
    );

    home.packages = [
      (lib.hiPrio wrapped)
    ]
    ++ fontPackages
    ++ lib.optionals cfg.tools.installInProfile (map lib.lowPrio pathTools)
    ++ lib.optional (!cfg.manageElispPackages) (lib.lowPrio pkgs.git);

    fonts.fontconfig.enable = lib.mkIf cfg.tools.fonts.enable true;
  };
}
