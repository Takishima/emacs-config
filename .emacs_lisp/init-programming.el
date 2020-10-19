;;; init-programming.el --- Initialisation for programming -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: nil
;; Keywords: init


;; MIT License

;; Copyright (c) 2020 Damien Nguyen

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

(require 'use-package)
(require 'diminish)
(require 'config-functions (concat config-dir "functions.el"))

;; ========================================================================== ;;
;; Automatically guess indent offsets, tab, spaces settings, etc.

(use-package dtrt-indent
  :ensure t)

;; -------------------------------------------------------------------------- ;;

(use-package project-directory
  :ensure nil
  :load-path config-dotemacs-lisp)

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

(config-with-system darwin
  (defvar compile-in-iterms-command "make")
  (config-with-system darwin
    (defun compile-in-iterm (command)
      (interactive
       (list (read-string (format "Say word [%s]: " compile-in-iterms-command) nil nil compile-in-iterms-command)))
      (progn
	(setq compile-in-iterms-command command)
	(do-applescript
	 (concat "tell application \"iTerm\"\ntell current session of current window\nwrite text \""
		 command
		 "\"\nend tell\nend tell")
	 )
	))
    )
  )

;; ========================================================================== ;;

(use-package editorconfig
  :ensure t
  )

(use-package editorconfig-generate
  :ensure t
  )

(use-package editorconfig-domain-specific
  :ensure t
  )

(use-package editorconfig-custom-majormode
  :ensure t)

;; ========================================================================== ;;

(use-package lsp-mode
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         (lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
	read-process-output-max (* 1024 1024)
	treemacs-space-between-root-nodes nil
	company-idle-delay 0.1
	company-minimum-prefix-length 3
	lsp-idle-delay 0.1
	lsp-completion-provider :capf
        ;; Prevent constant auto-formatting...
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
	;; be more ide-ish
	lsp-headerline-breadcrumb-enable t)
  (lsp-register-custom-settings '(("pyls.plugins.flake8.enabled" t t)))
  )

;; Taken from https://tychoish.com/post/emacs-and-lsp-mode/
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-doc-hide
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ("C-c u" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable t
         lsp-ui-doc-use-webkit nil
         lsp-ui-doc-header nil
         lsp-ui-doc-delay 0.2
         lsp-ui-doc-include-signature t
         lsp-ui-doc-alignment 'at-point
         lsp-ui-doc-use-childframe nil
         lsp-ui-doc-border (face-foreground 'default)
         lsp-ui-peek-enable t
         lsp-ui-peek-show-directory t
         lsp-ui-sideline-update-mode 'line
         lsp-ui-sideline-enable t
         lsp-ui-sideline-show-code-actions t
         lsp-ui-sideline-show-hover nil
         lsp-ui-sideline-ignore-duplicate t)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
       (lambda ()
         (setq lsp-ui-doc-border (face-foreground 'default))
         (set-face-background 'lsp-ui-doc-background
                              (face-background 'tooltip))))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))

  (defun lsp-update-server ()
       "Update LSP server."
       (interactive)
       ;; Equals to `C-u M-x lsp-install-server'
       (lsp-install-server t))
  )

;; Debug
(use-package dap-mode
  :ensure t
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :after (lsp-mode)
  :functions dap-hydra/nil
  :hook ((dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
         (python-mode . (lambda () (require 'dap-python)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (powershell-mode . (lambda () (require 'dap-pwsh))))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls))
  (when (executable-find "python3")
    (setq dap-python-executable "python3"))
  )


(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :init (lsp-treemacs-sync-mode 1)
  ;; :bind (:map lsp-mode-map
  ;;        ("M-9" . lsp-treemacs-errors-list))
  )

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

;; Taken from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-lsp.el
;; Python: pyright
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda () (require 'lsp-pyright)))
;;   :init (when (executable-find "python3")
;;           (setq lsp-pyright-python-executable-cmd "python3")))

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
	(setq skip-names (with-temp-buffer (insert-file-contents (concat dir-path "skip.txt"))
					 (split-string (buffer-string) "[\n\r]" t "[ \t]+")))
	(message "INFO: will be skipping the following: %S" skip-names)
	)
    )
  (dolist (fname dir-list)
    (setq require-name (intern-soft (concat "init-prog-"
					    (file-name-sans-extension (file-name-nondirectory fname)))))
    (unless (member (file-name-nondirectory fname) skip-names)
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

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package flycheck-clang-tidy
  :ensure t
  :after flycheck
  :config (flycheck-clang-tidy-setup))


;; ========================================================================== ;;

(provide 'init-programming)

;;; init-programming.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
