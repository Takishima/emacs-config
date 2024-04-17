(use-package nixpkgs-fmt
  :straight t)

(use-package lsp-nix
  ;; :straight t
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  ;; :custom
  ;; (lsp-nix-nil-formatter ["alejandra"])
  )

(use-package nix-mode
  :straight t
  :hook (nix-mode . lsp-deferred)
  :ensure t)
