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
  ;; :config
  ;; (add-to-list 'lsp-language-id-configuration
  ;;              '(cuda-mode . "cuda"))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection
  ;;                                    'lsp-clients--clangd-command)
  ;;                   :activation-fn (lsp-activate-on "cuda")
  ;;                   :priority -1
  ;;                   :server-id 'clangd
  ;;                   :download-server-fn (lambda (_client callback error-callback _update?)
  ;;                                         (lsp-package-ensure 'clangd callback error-callback))))
  :custom
  (lsp-use-plists t)
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 3 1024 1024))

  ;; (treemacs-space-between-root-nodes nil)

  (lsp-auto-guess-root t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-before-save-edits nil)
  (lsp-idle-delay 0.3)
  (lsp-completion-provider :capf)
  ;; Prevent constant auto-formatting...)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-indentation nil)
  ;; be more ide-ish)
  (lsp-headerline-breadcrumb-enable t)
  ;; python-related settings)
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

;; Ignore transient CMake directories that can disappear mid-walk and harden
;; the directory walk against TOCTOU races (e.g. `__cmake_systeminformation'
;; created and removed during CMake's `enable_language' probe).
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

;; Taken from https://tychoish.com/post/emacs-and-lsp-mode/
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
  ;; (lsp-ui-doc-use-webkit nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-update-mode 'line)
  ;; :custom-face
  ;; (lsp-ui-peek-highlight ((t (:inherit nil :background nil :foreground nil :weight semi-bold :box (:line-width -1)))))
  :config
  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'enable-theme-functions
            (lambda (_theme)
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))

  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t))
  )

;; ========================================================================== ;;

(use-package cleanup-lsp-workspaces
  :straight nil
  :after (lsp-mode)
  :commands (lsp-cleanup-workspaces
             lsp-cleanup-workspaces-nonexistent
             lsp-list-workspaces
             lsp-cleanup-workspaces-keep-home-only
             lsp-cleanup-workspaces-remove-all))

;; ========================================================================== ;;

;; Debug
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
         ;; ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (powershell-mode . (lambda () (require 'dap-pwsh))))
  :config
  (when (executable-find "python3")
    (setq dap-python-executable "python3"))
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  )

(when (executable-find "emacs-lsp-booster")
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
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
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  )

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :straight t
  :commands lsp-treemacs-errors-list
  :config (lsp-treemacs-sync-mode 1)
  ;; :bind (:map lsp-mode-map
  ;;        ("M-9" . lsp-treemacs-errors-list))
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
