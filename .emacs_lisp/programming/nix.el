(use-package nixpkgs-fmt
  :straight t)

(use-package lsp-nix
  :straight nil
  :ensure nil
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["alejandra"])
  )

(use-package nix-mode
  :straight t
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(use-package sops
  :straight t
  :ensure t
  :config
  (global-sops-mode t))
