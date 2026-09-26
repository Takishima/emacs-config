;; -*- lexical-binding: t; -*-
(use-package nixpkgs-fmt
  :straight t)

(use-package lsp-nix
  :straight nil
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["alejandra"])
  )

;; nixd LSP client configuration
(with-eval-after-load 'lsp-mode
  ;; Define nixd customization group
  (defgroup lsp-nix-nixd nil
    "LSP support for Nix, using nixd language server."
    :group 'lsp-mode
    :link '(url-link "https://github.com/nix-community/nixd"))

  ;; nixd server path
  (defcustom lsp-nix-nixd-server-path "nixd"
    "Executable path for the nixd server."
    :group 'lsp-nix-nixd
    :type 'string
    :package-version '(lsp-mode . "8.0.0"))

  ;; nixd formatting command
  (lsp-defcustom lsp-nix-nixd-formatting-command nil
    "External formatter command with arguments."
    :type 'lsp-string-vector
    :group 'lsp-nix-nixd
    :lsp-path "nixd.formatting.command"
    :package-version '(lsp-mode . "9.0.1"))

  ;; nixd nixpkgs expression
  (lsp-defcustom lsp-nix-nixd-nixpkgs-expr nil
    "Expression for nixpkgs toplevel. Provides package, lib completion/information."
    :type 'string
    :group 'lsp-nix-nixd
    :lsp-path "nixd.nixpkgs.expr"
    :package-version '(lsp-mode . "9.0.1"))

  ;; nixd NixOS options expression
  (lsp-defcustom lsp-nix-nixd-nixos-options-expr nil
    "Option set for NixOS option completion."
    :type 'string
    :group 'lsp-nix-nixd
    :lsp-path "nixd.options.nixos.expr"
    :package-version '(lsp-mode . "9.0.1"))

  ;; nixd home-manager options expression
  (lsp-defcustom lsp-nix-nixd-home-manager-options-expr nil
    "Option set for home-manager option completion."
    :type 'string
    :group 'lsp-nix-nixd
    :lsp-path "nixd.options.home-manager.expr"
    :package-version '(lsp-mode . "9.0.1"))

  ;; Register nixd LSP client
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-nix-nixd-server-path))
                    :major-modes '(nix-mode)
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration
                                         (lsp-configuration-section "nixd"))))
                    :synchronize-sections '("nixd")
                    :server-id 'nixd-lsp
                    :priority -1)))

(use-package nix-mode
  :straight t
  :hook (nix-mode . lsp-deferred)
  :config
  (add-hook 'nix-mode-hook
            (lambda () (progn
                         (setq-local devdocs-current-docs '("nix")))
                         (setq-local dash-docs-docsets '("nix" "nixos" "nixpkgs")))
              )
  )

(use-package sops
  :straight t
  :demand t
  :bind (("C-c C-d" . sops-find-file))
  :config
  ;; Workaround: sops--run's make-process + accept-process-output polling
  ;; deadlocks when invoked from find-file-hook context (sentinel never
  ;; drains). Swap to synchronous call-process.
  (advice-add
   'sops--run :override
   (lambda (args &rest keys)
     (let ((input (plist-get keys :input)))
       (with-temp-buffer
         (let* ((stderr-file (make-temp-file "sops-stderr-"))
                (input-file (when input
                              (with-file-modes #o600
                                (make-temp-file "sops-input-"))))
                (process-environment
                 (cons "SOPS_DISABLE_VERSION_CHECK=true" process-environment)))
           (when input
             (let ((coding-system-for-write 'utf-8-unix))
               (write-region input nil input-file nil 'silent)))
           (unwind-protect
               (let* ((full-args (if input-file
                                     (append args (list input-file))
                                   args))
                      (exit (apply #'call-process sops-executable nil
                                   (list t stderr-file) nil full-args))
                      (stdout (buffer-string))
                      (stderr (with-temp-buffer
                                (insert-file-contents stderr-file)
                                (buffer-string))))
                 (list :exit-status exit :stdout stdout :stderr stderr))
             (when (file-exists-p stderr-file) (delete-file stderr-file))
             (when (and input-file (file-exists-p input-file))
               (delete-file input-file))))))))
  (global-sops-mode 1))
