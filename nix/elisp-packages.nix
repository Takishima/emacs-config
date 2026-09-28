# Every package the configuration installs with `:straight', as nixpkgs
# emacsPackages attributes, plus the overrides nixpkgs needs to serve it.
# `names' is what `emacs --batch -l test/packages.el .' prints; the
# package-list flake check keeps the two in step.
{ inputs }:
let
  # "<v>-unstable-YYYY-MM-DD" is the one form melpaBuild normalises to a
  # MELPA version; a dash-less date fails package-build's version parser.
  dateOf =
    input:
    let
      d = input.lastModifiedDate;
    in
    "${builtins.substring 0 4 d}-${builtins.substring 4 2 d}-${builtins.substring 6 2 d}";
in
rec {
  names = [
    "adoc-mode"
    "aio"
    "ansible"
    "ansible-doc"
    "auctex"
    "auth-source-1password"
    "auto-rename-tag"
    "blacken"
    "browse-kill-ring"
    "bufler"
    "clang-format"
    "claude-code-ide"
    "cmake-font-lock"
    "cmake-mode"
    "consult"
    "consult-dash"
    "consult-dir"
    "consult-flycheck"
    "consult-lsp"
    "consult-projectile"
    "copilot"
    "cuda-mode"
    "cython-mode"
    "dap-mode"
    "dash-docs"
    "dashboard"
    "datetime"
    "demangle-mode"
    "devdocs"
    "difftastic"
    "diminish"
    "direnv"
    "docker-compose-mode"
    "dockerfile-mode"
    "dtrt-indent"
    "eat"
    "editorconfig"
    "editorconfig-custom-majormode"
    "editorconfig-domain-specific"
    "editorconfig-generate"
    "embark"
    "embark-consult"
    "exec-path-from-shell"
    "flycheck"
    "flycheck-aspell"
    "flycheck-clang-analyzer"
    "flycheck-clang-tidy"
    "flycheck-elsa"
    "flycheck-yamllint"
    "format-all"
    "gitlab-ci-mode"
    "gnuplot"
    "go-impl"
    "go-mode"
    "go-tag"
    "google-c-style"
    "gotest"
    "hcl-mode"
    "helpful"
    "highlight-indentation"
    "hotfuzz"
    "htmlize"
    "keychain-environment"
    "leuven-theme"
    "logview"
    "lsp-ltex"
    "lsp-mode"
    "lsp-pyright"
    "lsp-treemacs"
    "lsp-ui"
    "magit"
    "magit-delta"
    "magit-gitflow"
    "marginalia"
    "markdown-mode"
    "markdown-toc"
    "matlab-mode"
    "modern-cpp-font-lock"
    "multiple-cursors"
    "nix-mode"
    "nixpkgs-fmt"
    "orderless"
    "pacfiles-mode"
    "pkgbuild-mode"
    "pov-mode"
    "powershell"
    "prescient"
    "projectile"
    "projectile-ripgrep"
    "python-black"
    "python-coverage"
    "python-insert-docstring"
    "python-isort"
    "python-pytest"
    "pyvenv"
    "robot-mode"
    "ros"
    "ruff-format"
    "smart-shift"
    "sops"
    "sphinx-doc"
    "sphinx-mode"
    "system-packages"
    "terraform-mode"
    "treemacs"
    "vertico"
    "vertico-prescient"
    "web-mode"
    "wgrep"
    "which-key"
    "whitespace-cleanup-mode"
    "yaml-mode"
    "yapfify"
    "yasnippet"
    "yasnippet-snippets"
    "ztree"
  ];

  packages = epkgs: map (n: epkgs.${n}) names;

  # lsp-protocol.el reads LSP_USE_PLISTS inside eval-and-compile; every
  # package expanding its macros must be compiled with the same value the
  # config sets at runtime (early-init.el).  The Emacs builders use
  # structured attrs, so the variable has to go through `env'.
  plistPackages = [
    "lsp-mode"
    "lsp-ui"
    "lsp-treemacs"
    "lsp-pyright"
    "lsp-ltex"
    "dap-mode"
    "consult-lsp"
  ];

  # emacs-overlay's overlays/package.nix, minus the archives the merge shadows.
  # It rebuilds the scope's base, so it must precede `overrides', never follow.
  archives =
    _self: super:
    let
      repos = "${inputs.emacs-overlay}/repos";
    in
    super.override {
      melpaPackages = super.melpaPackages.override {
        archiveJson = "${repos}/melpa/recipes-archive-melpa.json";
      };
      elpaPackages = super.elpaPackages.override {
        generated = "${repos}/elpa/elpa-generated.nix";
      };
      nongnuPackages = super.nongnuPackages.override {
        generated = "${repos}/nongnu/nongnu-generated.nix";
      };
    };

  overrides =
    pkgs: self: super:
    let
      withPlists =
        pkg:
        pkg.overrideAttrs (prev: {
          env = (prev.env or { }) // {
            LSP_USE_PLISTS = "true";
          };
        });
    in
    pkgs.lib.genAttrs plistPackages (n: withPlists super.${n})
    // {
      claude-code-ide = self.melpaBuild {
        pname = "claude-code-ide";
        version = "0.3.0-unstable-${dateOf inputs.claude-code-ide}";
        src = inputs.claude-code-ide;
        packageRequires = with self; [
          websocket
          transient
          web-server
        ];
      };
      docker-compose-mode = self.trivialBuild {
        pname = "docker-compose-mode";
        version = "1.1.0-unstable-${dateOf inputs.docker-compose-mode}";
        src = inputs.docker-compose-mode;
        packageRequires = with self; [
          dash
          yaml-mode
        ];
      };
      # The fork keeps nixpkgs' MELPA recipe (:files (:defaults "*.extmap")).
      datetime = super.datetime.overrideAttrs (_: {
        version = "0.10.2-unstable-${dateOf inputs.datetime}";
        src = inputs.datetime;
      });
    };
}
