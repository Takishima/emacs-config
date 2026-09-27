# Binaries the configuration runs, grouped by the module that calls them.
# Each group is a `programs.emacs-config.tools.<name>' option with these
# packages as its default; `everyday' groups are the ones `tools.enableAll'
# turns on.  `base' is always on the wrapped Emacs's PATH.
{ pkgs }:
with pkgs;
let
  # One interpreter for pyright, ipython, debugpy, cython, docutils and
  # sphinx: two python3 derivations at the same priority collide.
  pythonEnv = python3.withPackages (ps: [
    ps.ipython
    ps.debugpy
    ps.cython
    ps.docutils
    ps.sphinx
  ]);
in
{
  base = [
    (aspellWithDicts (d: [
      d.en
      d.fr
      d.de
    ]))
    delta
  ];

  groups = {
    search = {
      description = "ripgrep and fd for consult, projectile and the project commands";
      everyday = true;
      packages = [
        ripgrep
        fd
      ];
    };
    vcs = {
      description = "git, git-flow, difftastic, mergiraf and pre-commit for magit and its extensions";
      everyday = true;
      packages = [
        git
        gitflow
        difftastic
        mergiraf
        pre-commit
      ];
    };
    env = {
      description = "keychain and gnupg for keychain-environment and epg";
      everyday = true;
      packages = [
        keychain
        gnupg
      ];
    };
    lsp = {
      description = "emacs-lsp-booster and typos-lsp, used by every lsp-mode buffer";
      everyday = true;
      packages = [
        emacs-lsp-booster
        typos-lsp
      ];
    };
    docs = {
      description = "sqlite for dash-docs";
      everyday = true;
      packages = [ sqlite ];
    };
    shell = {
      description = "bash-language-server and shellcheck for shell scripts";
      everyday = true;
      packages = [
        bash-language-server
        shellcheck
      ];
    };
    toml = {
      description = "taplo for TOML";
      everyday = true;
      packages = [ taplo ];
    };
    ansible = {
      description = "ansible, its language server and ansible-lint";
      everyday = true;
      packages = [
        ansible
        ansible-language-server
        ansible-lint
      ];
    };
    cmake = {
      description = "cmake, make, cmake-format and cmake-language-server for the build-systems module";
      everyday = true;
      packages = [
        cmake
        gnumake
        cmake-format
        cmake-language-server
      ];
    };
    cpp = {
      description = "clangd, clang-format, clang-tidy, clang and c++filt for C and C++";
      everyday = true;
      packages = [
        clang-tools
        clang
        binutils
      ];
    };
    docker = {
      description = "dockerfile-language-server and hadolint; the docker client comes from the host";
      everyday = true;
      packages = [
        dockerfile-language-server
        hadolint
      ];
    };
    go = {
      description = "go, gopls, gomodifytags and impl for Go";
      everyday = true;
      packages = [
        go
        gopls
        gomodifytags
        impl
      ];
    };
    json = {
      description = "the JSON language server and jq";
      everyday = true;
      packages = [
        vscode-langservers-extracted
        jq
      ];
    };
    markdown = {
      description = "marksman and markdownlint for Markdown";
      everyday = true;
      packages = [
        marksman
        markdownlint-cli
      ];
    };
    nix = {
      description = "nil, nixd, alejandra, nixpkgs-fmt and sops for Nix files";
      everyday = true;
      packages = [
        nil
        nixd
        alejandra
        nixpkgs-fmt
        sops
      ];
    };
    python = {
      description = "python with ipython, debugpy, cython, docutils and sphinx, plus pyright, ruff, black, isort and yapf";
      everyday = true;
      packages = [
        pythonEnv
        pyright
        ruff
        black
        isort
        yapf
      ];
    };
    terraform = {
      description = "terraform-lsp; terraform itself is unfree and left to the profile";
      everyday = true;
      packages = [ terraform-lsp ];
    };
    web = {
      description = "the HTML, CSS and TypeScript language servers";
      everyday = true;
      packages = [
        vscode-langservers-extracted
        typescript-language-server
        typescript
      ];
    };
    yaml = {
      description = "yaml-language-server and yamllint";
      everyday = true;
      packages = [
        yaml-language-server
        yamllint
      ];
    };
    tex = {
      description = "TeX Live with latexmk and biber, texlab and ltex-ls, for AUCTeX and lsp-ltex";
      everyday = false;
      packages = [
        (texlive.combine { inherit (texlive) scheme-medium latexmk biber; })
        texlab
        ltex-ls
      ];
    };
    debuggers = {
      description = "gdb, lldb, delve and debugpy for dap-mode";
      everyday = false;
      packages = [
        gdb
        lldb
        delve
        pythonEnv
      ];
    };
    ai = {
      description = "the claude CLI for claude-code-ide (unfree); copilot's server comes with its elisp package";
      everyday = false;
      packages = [ claude-code ];
    };
    gnuplot = {
      description = "gnuplot, for a language module that is off by default";
      everyday = false;
      packages = [ gnuplot ];
    };
    povray = {
      description = "povray for pov-mode";
      everyday = false;
      packages = [ povray ];
    };
    windows = {
      description = "pwsh for powershell-mode (x86_64 only)";
      everyday = false;
      packages = lib.optionals stdenv.hostPlatform.isx86_64 [ powershell ];
    };
    fonts = {
      description = "Symbols Nerd Font for the dashboard and nerd-icons glyphs, installed in the profile";
      everyday = false;
      packages = [ nerd-fonts.symbols-only ];
    };
  };

  # What the batch tests need on PATH in the sandbox.
  check = [
    delta
    (aspellWithDicts (d: [ d.en ]))
    git
  ];
}
