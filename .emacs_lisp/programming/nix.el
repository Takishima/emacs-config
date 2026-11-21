;;; nix.el --- Nix support -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package nixpkgs-fmt
  :straight t)

(use-package lsp-nix
  :straight nil
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nixd-formatting-command ["nixfmt"])
  :config
  ;; nil outranks nixd in lsp-mode's client priorities.
  (add-to-list 'lsp-disabled-clients 'nix-nil))

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
  ;; `sops--run' polls an async process that deadlocks when called from
  ;; `find-file-hook' (the sentinel never drains), so run it synchronously.
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

(provide 'init-prog-nix)

;;; nix.el ends here
