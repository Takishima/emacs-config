;;; init-programming.el --- Initialisation for programming -*- lexical-binding: t -*-

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
(require 'config-functions (concat config-dir "functions.el"))

;; ========================================================================== ;;
;; Automatically guess indent offsets, tab, spaces settings, etc.

(use-package dtrt-indent
  :straight t)

;; -------------------------------------------------------------------------- ;;

(use-package project-directory
  :straight nil
  :load-path config-dotemacs-lisp)

;; ========================================================================== ;;

(use-package direnv
  :if (executable-find "direnv")
  :straight t
  :config
  (direnv-mode)
  (defcustom dn-direnv-enabled-hosts nil
    "List of remote hosrs to use direnv on.

     Each host must have the `direnv` executable accessible in the default environment"
    :type '(repeat string)
    :group 'dn)

  (defun tramp-sh-handle-start-file-process@dn-direnv (args)
    "Enable Direnv for hosts in `dn-direnv-enabled-hosts'."
    (message "tramp-sh-handle-start-file-process@dn-direnv")
    (with-parsed-tramp-file-name (expand-file-name default-directory) nil
      (if (member host my-direnv-enabled-hosts)
          (pcase-let ((`(,name ,buffer ,program . ,args) args))
            `(,name
              ,buffer
              "direnv"
              "exec"
              ,localname
              ,program
              ,@args))
        args)))

  (with-eval-after-load "tramp-sh"
    (advice-add 'tramp-sh-handle-start-file-process
                :filter-args #'tramp-sh-handle-start-file-process@dn-direnv))
  )

;; ========================================================================== ;;

(use-package diff-mode
  :straight t
  :mode
  "\\.patch[0-9]*\\'"
  )

;; ========================================================================== ;;

;; ========================================================================== ;;

(use-package flycheck
  :straight t
  :custom
  (flycheck-clang-args '("-std=c++20"))
  )

(use-package datetime
  :straight  (datetime :type git :host github :repo "doublep/datetime"
                       :fork (:host github
                                    :repo "Takishima/datetime")))
(use-package logview
  :straight t
  :custom
  (logview-completing-read-function 'completing-read)
  (logview-additional-submodes '(("ROS2" (format . "[LEVEL] [TIMESTAMP] [NAME]:") (levels . "SLF4J")
                                  (timestamp "ROS2"))))
  (logview-additional-timestamp-formats '(("ROS2" (java-pattern . "A.SSSSSSSSS"))))
  )

;; ========================================================================== ;;
;; Compilation

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      ;; (switch-to-prev-buffer (get-buffer-window buf) 'bury)
                      (when (get-buffer-window buf)
                        (delete-window (get-buffer-window buf))
                        )
                      (kill-buffer buf)
                      )
                    buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; -------------------------------------------------------------------------- ;;

(setq compilation-scroll-output 'first-error)

;; -------------------------------------------------------------------------- ;;

(config-when-system 'darwin
  (defvar compile-in-iterms-command "make")
  ;; (defcustom dn-compile-in-iterms-history nil
  ;;   "History variable for"
  ;;   :type '(repeat string)
  ;;   :group 'dn)
  (defvar compile-in-iterms-history nil)
  (defun compile-in-iterm (command)
    (interactive
     (list
      (read-from-minibuffer (format "Command [%s]: " (car compile-in-iterms-history))
                            nil ;; INITIAL-CONTENT (deprecated)
                            nil ;; KEYMAP
                            nil ;; READ
                            'compile-in-iterms-history
                            (if 'compile-in-iterms-history (car compile-in-iterms-history) ("make")))
      ))
    (progn
      (when (string= "" command) (setq command (car compile-in-iterms-history)))
      (do-applescript
       (concat "tell application \"iTerm\"\ntell current session of current window\nwrite text \""
	       (replace-regexp-in-string "\"" "\\\"" command t t)
	       "\"\nend tell\nend tell")
       )
      ))
  )

;; ========================================================================== ;;

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1)
  )

(use-package editorconfig-generate
  :straight t
  )

(use-package editorconfig-domain-specific
  :straight t
  )

(use-package editorconfig-custom-majormode
  :straight t)

;; ========================================================================== ;;

;; (use-package code-review
;;   :straight t
;;   )

;; ========================================================================== ;;

(use-package gitlab-ci-mode
  :straight t
  )

;; ========================================================================== ;;

(use-package ansible
  :straight t
  )

(use-package ansible-doc
  :straight t
  )

;; ========================================================================== ;;

(use-package format-all
  :straight t
  )

;; ========================================================================== ;;

(use-package hcl-mode
  :straight t
  )

;; ========================================================================== ;;

(use-package nix-mode
  :straight t
  )

;; ========================================================================== ;;

(use-package flycheck-elsa
  :straight t
  )

;; ========================================================================== ;;

;; Built-in treesit configuration (Emacs 29+)
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  
  ;; Auto-install treesit grammars when needed
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Function to install missing grammars
  (defun treesit-install-all-grammars ()
    "Install all treesit grammars defined in `treesit-language-source-alist'."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (let ((lang (car grammar)))
        (unless (treesit-language-available-p lang)
          (message "Installing treesit grammar for %s..." lang)
          (treesit-install-language-grammar lang)))))

  ;; Auto-install grammars on first use
  (advice-add 'treesit-parser-create :before
              (lambda (language &rest _)
                (unless (treesit-language-available-p language)
                  (message "Auto-installing treesit grammar for %s..." language)
                  (treesit-install-language-grammar language))))

  ;; Enable treesit modes by default where available
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)
          (typescript-mode . typescript-ts-mode))))

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
  )

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
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))

  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t))
  )

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
         (python-mode . (lambda () (require 'dap-python)))
         (diff-mode . (lambda () (dap-mode -1)))
         (powershell-mode . (lambda () (dap-mode -1)))
         (shell-script-mode . (lambda () (dap-mode -1)))
         (cmake-mode . (lambda () (dap-mode -1)))
         ;; ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (powershell-mode . (lambda () (require 'dap-pwsh))))
  :config
  (when (executable-find "python3")
    (setq dap-python-executable "python3"))
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  )



;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :init
;;   (global-lsp-bridge-mode))

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

;; (use-package ruff-lsp
;;   :straight t
;;   :commands (treemacs)
;;   :after (lsp-mode))

;; ========================================================================== ;;
;; (defvar eglot-clangd-exe (executable-find "clangd")
;;   "clangd executable path")

;; (use-package eglot
;;   :straight t
;;   :preface
;;   :hook ((c-mode-common . eglot-ensure)
;;          (python-mode   . eglot-ensure)
;;          (ruby-mode     . eglot-ensure)
;;          )
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                `((c++-mode) ,eglot-clangd-exe))
;;   :config
;;   ;; (add-hook 'eglot--managed-mode-hook
;;   ;;           (lambda ()
;;   ;;             (bind-keys :map eglot-mode-map
;;   ;;                        ("C-h o"   . eglot-help-at-point)
;;   ;;                        ("C-c C-r" . eglot-rename)
;;   ;;                        ("C-c f"   . eglot-format)
;;   ;;                        ("C-c C-a" . eglot-code-actions))))
;;   (with-eval-after-load 'company
;;     (make-local-variable 'company-transformers)
;;     (setq company-transformers (remq 'company-sort-by-statistics company-transformers))
;;     (setq company-transformers (remq 'company-flx-transformer company-transformers))
;;     (setq-local company-backends '(company-files
;;                                    (company-capf :separate company-yasnippet)
;;                                    company-keywords)))
;;   )

;; ========================================================================== ;;

(let* (
       (dir-path (file-name-as-directory (concat config-dotemacs-lisp "programming")))
       (skip-file (concat dir-path "skip.txt"))
       (skip-names (list))
       (require-name)
       (dir-list (directory-files dir-path t "^[^#\\.].*\\.el$"))
       )
  (if (file-exists-p skip-file)
      (progn
	(setq skip-names (seq-remove (lambda (el) (or (string-prefix-p "#" el) (string-prefix-p ";" el)))
                                     (with-temp-buffer (insert-file-contents (concat dir-path "skip.txt"))
					               (split-string (buffer-string) "[\n\r]" t "[ \t]+"))
                                     )
              )
	(message "INFO: will be skipping the following: %S" skip-names)
	)
    )
  (dolist (fname dir-list)
    (setq require-name (intern-soft (concat "init-prog-"
        				    (file-name-sans-extension (file-name-nondirectory fname)))))
    (unless (member (file-name-nondirectory fname) skip-names)
      ;; (byte-recompile-file fname nil 0)
      (if require-name
          (progn
            (message "INFO: requiring %s from %s" require-name fname)
            (require require-name fname)
            )
        (progn
          (setq fname (file-name-sans-extension fname))
          (load fname)
          )
        )
      )
    )
  )

;; ========================================================================== ;;

(provide 'init-programming)

;;; init-programming.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
