;;; init-lsp.el --- Initialisation for LSP and debugging -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: nil
;; Keywords: init


;; MIT License

;; Copyright (c) 2025 Damien Nguyen

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Commentary:

;;; Code:

;; ========================================================================== ;;

(require 'cl-lib)
(require 'use-package)

;; ========================================================================== ;;

(defcustom dn-lsp-mode-disabled
  (append '(emacs-lisp-mode lisp-mode makefile-mode direnv-envrc-mode bat-mode)
          (unless (memq system-type '(windows-nt ms-dos))
            '(powershell-mode)))
  "Major modes for which `prog-mode-hook' does not start `lsp-deferred'."
  :type '(repeat symbol)
  :group 'dn)

;; -------------------------------------------------------------------------- ;;

(use-package lsp-mode
  :straight t
  :defines lsp-language-id-configuration
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . (lambda ()
                        (unless (cl-some 'derived-mode-p dn-lsp-mode-disabled)
                          (lsp-deferred))
                        ))
         (c++-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-use-plists t)
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 3 1024 1024))

  (lsp-auto-guess-root t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-before-save-edits nil)
  (lsp-idle-delay 0.3)
  (lsp-completion-provider :capf)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-indentation nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-yapf-enabled t)
  :config
  (lsp-defcustom lsp-nix-nil-flake-impure nil
    "Use --impure flag when evaluating flake inputs.
Enable this if your flake or its inputs require impure evaluation."
    :type 'boolean
    :group 'lsp-nix-nil
    :lsp-path "nil.nix.flake.impure"
    :package-version '(lsp-mode . "9.0.0"))
  (setopt lsp-nix-nil-flake-impure t)
  )

;; CMake creates and removes directories such as `__cmake_systeminformation'
;; mid-walk; ignore them and let a vanished directory end the walk quietly.
(with-eval-after-load 'lsp-mode
  (dolist (p '("[/\\\\]__cmake_systeminformation\\'"
               "[/\\\\]CMakeFiles\\'"
               "[/\\\\]_deps\\'"
               "[/\\\\]\\.cmake\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories p))
  (advice-add 'lsp--all-watchable-directories :around
              (lambda (orig-fn &rest args)
                (condition-case nil
                    (apply orig-fn args)
                  (file-missing nil)))))

(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :commands lsp-ui-doc-hide
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-alignment 'at-point)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-update-mode 'line)
  :config
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  (add-hook 'enable-theme-functions
            (lambda (_theme)
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))

  (defun dn-lsp-update-server ()
    "Update an LSP server, as `C-u M-x lsp-install-server' does."
    (interactive)
    (lsp-install-server t))
  )

;; ========================================================================== ;;

(use-package cleanup-lsp-workspaces
  :straight nil
  :after (lsp-mode)
  :commands (dn-lsp-cleanup-workspaces
             dn-lsp-cleanup-workspaces-nonexistent
             dn-lsp-list-workspaces
             dn-lsp-cleanup-workspaces-keep-home-only
             dn-lsp-cleanup-workspaces-remove-all))

;; ========================================================================== ;;

(use-package dap-mode
  :straight t
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :after (lsp-mode)
  :functions dap-hydra/nil
  :custom
  (dap-auto-configure-mode t)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions controls tooltip))
  :hook ((dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
         (python-base-mode . (lambda () (require 'dap-python)))
         (go-ts-mode . (lambda () (require 'dap-go)))
         (diff-mode . (lambda () (dap-mode -1)))
         (powershell-mode . (lambda () (dap-mode -1)))
         (shell-script-mode . (lambda () (dap-mode -1)))
         ((cmake-mode cmake-ts-mode) . (lambda () (dap-mode -1)))
         (powershell-mode . (lambda () (require 'dap-pwsh))))
  :config
  (when (executable-find "python3")
    (setopt dap-python-executable "python3"))
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  )

(when (executable-find "emacs-lsp-booster")
  (defun dn-lsp-booster--advice-json-parse (old-fn &rest args)
    "Read the bytecode emacs-lsp-booster emits, else call OLD-FN with ARGS."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'dn-lsp-booster--advice-json-parse)

  (defun dn-lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster to the command OLD-FN resolves for CMD.
TEST? is non-nil when `lsp-server-present?' only checks for the server."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory)) ; `lsp-resolve-final-command' adds a shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let* ((command-from-exec-path (executable-find (car orig-result)))) ; may be on `exec-path' but not $PATH
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'dn-lsp-booster--advice-final-command)
  )

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :straight t
  :commands lsp-treemacs-errors-list
  :config (lsp-treemacs-sync-mode 1)
  )

(use-package treemacs
  :straight t
  :commands (treemacs)
  :after (lsp-mode))

;; -------------------------------------------------------------------------- ;;

(use-package consult-lsp
  :straight t
  :bind (
         ("M-s l" . consult-lsp-file-symbols)
         )
  )

;; ========================================================================== ;;

(provide 'init-lsp)

;;; init-lsp.el ends here
