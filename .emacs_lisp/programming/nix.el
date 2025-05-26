;; -*- lexical-binding: t; -*-
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
  :bind (("C-c C-c" . sops-save-file)
         ("C-c C-k" . sops-cancel)
         ("C-c C-d" . sops-edit-file))
  :config
  (global-sops-mode 1))
