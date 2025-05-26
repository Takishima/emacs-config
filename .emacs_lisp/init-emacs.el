;;; init-emacs.el --- Emacs initiali -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: version
;; Package-Requires: (dependencies)
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

(require 'config-functions config-dir)
(eval-when-compile (require 'use-package))

;; ========================================================================== ;;
;; Load leuven theme

(use-package leuven-theme
  :straight t
  :load-path "themes"
  :config
  (load-theme 'leuven t))

;; ========================================================================== ;;

;; which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode +1))

;; ========================================================================== ;;

(use-package browse-kill-ring
  :straight t
  :bind (("s-y" . browse-kill-ring))
  )

;; ========================================================================== ;;

(use-package smart-shift
  :straight t
  :config
  (global-smart-shift-mode +1))

;; ========================================================================== ;;

(use-package projectile
  :straight t
  :config
  (projectile-mode +1))

(use-package projectile-ripgrep
  :straight t)

;; ========================================================================== ;;

(use-package explain-pause-mode
  :straight nil
  :load-path (config-dotemacs-lisp)
)

;; ========================================================================== ;;

(use-package ztree
  :straight t
)

;; ========================================================================== ;;

;; Disable auto backup files
(custom-set-variables '(make-backup-files nil))

;; Remove trailing whitespace in files
(autoload 'nuke-trailing-whitespace "whitespace" nil t)

;; Easier question answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make script file executable by default
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Always show matching paranthesis, line and column number
(custom-set-variables '(show-paren-mode 1)
		      '(line-number-mode t)
		      '(column-number-mode t))

(config-when-system 'darwin
  (add-to-list 'default-frame-alist '(font . "Monaco" )
	       )
  )

(save-place-mode t)

;; (unless (and (version< emacs-version "27")
;;              (require 'so-long nil :noerror))
;;     (package-install 'so-long))
(global-so-long-mode)


(use-package hl-line+
  :load-path (config-dotemacs-lisp)
  :config
  (hl-line-when-idle-interval 0.2)
  (toggle-hl-line-when-idle 1))

(global-hl-line-mode 0)

(global-subword-mode)  ; navigationInCamelCase

(delete-selection-mode)

;; Remove redundant UI
(tool-bar-mode -1)

;; ========================================================================== ;;

(use-package dashboard
  :straight t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '((recents  . 10)
                     (projects . 5)
                     (bookmarks . 5)))
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-modify-heading-icons '((recents . "nf-oct-file")
                                    (bookmarks . "nf-oct-bookmark")
                                    (agenda . "nf-oct-calendar")
                                    (projects . "nf-oct-project")
                                    (registers . "nf-oct-database")))
  :custom-face
  (dashboard-heading-face ((t (:weight bold))))
  :config
  (dashboard-setup-startup-hook)
  :init
  (defun dn-home ()
    "Switch to home (dashboard) buffer."
    (interactive)
    (switch-to-buffer "*dashboard*"))
  )

;; ========================================================================== ;;

(use-package helpful
  :straight t
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point))
  )

;; Enable vertico
(use-package vertico
  :straight t
  ;; :bind (("C-c v r" . vertico-repeat)
  ;;        ("C-c v s" . vertico-suspend))
  ;; :hook (minibuffer-setup . vertico-repeat-save)
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; (completion-ignore-case t)
  ;; (completion-styles '(basic substring partial-completion flex))
  :config
  (vertico-mode))

;;;; Vertico-directory
(use-package vertico-directory
  :after vertico
  :straight nil
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;;; Vertico-multiform
(use-package vertico-multiform
  :requires vertico
  :ensure nil
  :custom
  (vertico-multiform-categories
   '((file buffer grid)
     (imenu (:not indexed mouse))
     (symbol (vertico-sort-function . vertico-sort-alpha))))
  (vertico-multiform-commands
   '((consult-line buffer)
     (consult-git-grep buffer)
     (consult-ripgrep buffer)
     (consult-grep buffer)
     (consult-fd grid)
     (execute-extended-command 'vertical))
   )
  :config
  (vertico-multiform-mode 1))

;;;; Vertico-buffer
(use-package vertico-buffer
  :after vertico
  :straight nil
  :ensure nil
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Vertico-prescient
(use-package vertico-prescient
  :after vertico prescient
  :straight nil
  :ensure nil
  :custom
  ;; Sorting
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil) ; Don't override `display-sort-function'

  ;; Filtering. Below only applies when `vertico-prescient-enable-filtering' is
  ;; non-nil
  (vertico-prescient-enable-filtering nil) ; We want orderless to do the filtering
  (vertico-prescient-completion-styles '(prescient flex))
  ;; Only set if `vertico-prescient-enable-filtering' is non-nil. See also
  ;; `prescient--completion-recommended-overrides'
  (vertico-prescient-completion-category-overrides
   '(;; Include `partial-completion' to enable wildcards and partial paths.
     (file (styles partial-completion prescient))
     ;; Eglot forces `flex' by default.
     (eglot (styles prescient flex))))
  :config
  (vertico-prescient-mode 1))

(use-package ido
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  :config
  (ido-mode 1)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(defun dn-completion-styles-setup ()
  "Set up `completion-styes'."
  (setopt completion-styles (list (if (featurep 'orderless)
                                      'orderless 'basic)
                                  (if (featurep 'hotfuzz)
                                      'hotfuzz 'flex))))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'after-init-hook 'dn-completion-styles-setup))

;;; Orderless
;; Alternative and powerful completion style (i.e. filters candidates)
(use-package orderless
  :straight t
  :custom
  (orderless-matching-styles
   '(orderless-regexp
     orderless-prefixes
     orderless-initialism
     ;; orderless-literal
     ;; orderless-flex
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  :config
  ;; Eglot forces `flex' by default.
  (add-to-list 'completion-category-overrides '(eglot (styles . (orderless flex)))))

;;; Marginalia
;; Enable richer annotations in minibuffer (companion package of consult.el)
(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-field-width 80)
  (marginalia-align-offset -2)          ; Two to the left
  :config
  (marginalia-mode 1))


;;; Hotfuzz
;; Faster version of the flex completion style.  Hotfuzz is a much faster
;; version of the built-in flex style.  See
;; https://github.com/axelf4/emacs-completion-bench#readme
(use-package hotfuzz
  :straight t)


(defun dn--consult-line-thing-at-point ()
  "Do incremental search forward for the \"thing\" found near point.
Like ordinary incremental search except that the \"thing\" found at point
is added to the search string initially.  The \"thing\" is defined by
`bounds-of-thing-at-point'.  You can customize the variable
`isearch-forward-thing-at-point' to define a list of symbols to try
to find a \"thing\" at point.  For example, when the list contains
the symbol `region' and the region is active, then text from the
active region is added to the search string."
  (interactive)
  (let ((bounds (seq-some (lambda (thing)
                            (bounds-of-thing-at-point thing))
                          isearch-forward-thing-at-point)))
    (cond
     (bounds
      (when (use-region-p)
        (deactivate-mark))
      (when (< (car bounds) (point))
	(goto-char (car bounds)))
      (consult-line
       (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (t
      (setq isearch-error "No thing at point")
      (consult-line))))
  )

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (
         ("C-s" . consult-line)
         ;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x C-r" . consult-recent-file)
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s ." . dn--consult-line-thing-at-point)
         ("M-s c" . consult-locate)
         ("M-s d" . consult-fd)
         ("M-s D" . devdocs-lookup)
         ("M-s f" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )


(use-package consult-dir
  :straight (:host github :repo "karthink/consult-dir" :files ("*.el"))
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-lsp
  :straight t
  :bind (
         ("M-s l" . consult-lsp-file-symbols)
         )
  )

(use-package consult-projectile
  :straight t
  :bind (
         ("C-c p" . consult-projectile)
         ("C-c P" . projectile-commander)
         )
  )

(use-package consult-flycheck
  :straight t)


(use-package embark
  :straight t
  :bind
  (("C-:" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; -------------------------------------------------------------------------- ;;

(use-package whitespace-cleanup-mode
  :straight t
  :custom
  (show-trailing-whitespace t)  ; not from whitespace-cleanup-mode.el
  :hook
  (diff-mode . (lambda () (whitespace-cleanup-mode -1)))
  :config
  (global-whitespace-cleanup-mode))

;; ========================================================================== ;;
;; Some emacs function definitions

(defun shutdown-emacs-server ()
  "Kill the emacs daemon"
  (interactive)
  (let (
	(last-nonmenu-event nil)
	(window-system nil)
	)
    (save-buffers-kill-emacs t)))

;; -------------------------------------------------------------------------- ;;

(defun kill-from-line-beginning ()
  "Kills from beginning of line to point"
  (interactive)
  (kill-region (line-beginning-position) (point)))

;; -------------------------------------------------------------------------- ;;

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files.") )

;; -------------------------------------------------------------------------- ;;

(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
	  (list (query-replace-read-to (reb-target-binding reb-regexp)
				       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

;; -------------------------------------------------------------------------- ;;

(defun formatted-copy-buffer ()
  "Export buffer to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (htmlize-buffer))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
	(progn
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
	(kill-buffer buf)
	))))

(defun formatted-copy-region ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (htmlize-region (region-beginning) (region-end)))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
	(progn
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
	(kill-buffer buf)
	))))

;; -------------------------------------------------------------------------- ;;

;; From https://github.com/KaratasFurkan/.emacs.d
(defun dn-async-process (command &optional name filter)
  "Start an async process by running the COMMAND string with bash. Return the
process object for it.

NAME is name for the process. Default is \"async-process\".

FILTER is function that runs after the process is finished, its args should be
\"(process output)\". Default is just messages the output."
  (make-process
   :command `("bash" "-c" ,command)
   :name (if name name
           "async-process")
   :filter (if filter filter
             (lambda (process output) (message (s-trim output)))))
  )

;; -------------------------------------------------------------------------- ;;

(use-package aio
  :straight t)

;; -------------------------------------------------------------------------- ;;

(defcustom dn-default-font-size
  98
  "Default font size."
  :group 'dn
  :type 'integer
  )
(defcustom dn-default-icon-size
  22
  "Default icon size."
  :group 'dn
  :type 'integer
  )

;; From https://github.com/KaratasFurkan/.emacs.d
(defun dn-adjust-font-size (height)
  "Adjust font size by given height. If height is '0', reset font
size. This function also handles icons and modeline font sizes."
  (interactive "nHeight ('0' to reset): ")
  (let ((new-height (if (zerop height)
                        dn-default-font-size
                      (+ height (face-attribute 'default :height)))))
    (set-face-attribute 'default nil :height new-height)
    (set-face-attribute 'mode-line nil :height new-height)
    (set-face-attribute 'mode-line-inactive nil :height new-height)
    (message "Font size: %s (default %s)" new-height (face-attribute 'default :height)))
  (let ((new-size (if (zerop height)
                      dn-default-icon-size
                    (+ (/ height 5) treemacs--icon-size))))
    ;; (when (fboundp 'treemacs-resize-icons)
    ;;   (treemacs-resize-icons new-size))
    (when (fboundp 'company-box-icons-resize)
      (company-box-icons-resize new-size)))
  )

;; ========================================================================== ;;

(defvar dn-script-on-save
  '(
    ;; ("/home/someone/file.txt" . "cat ~/file.txt")
    )
  "File association list with their respective command.")

(defun dn-cmd-after-saved-file ()
  "Maybe execute a shell command after a file is saved."
  (when dn-script-on-save
    (let
        ((command nil)
         (match (assoc (buffer-file-name) dn-script-on-save)))
      (when match
        (if (file-exists-p (cdr match))
            (setq command (cdr match))
          (setq command (list (executable-find (cdr match) nil)))
          (when command
            (add-to-list 'command (buffer-file-name) t))
          )
        (if command
            (shell-command (cdr match))
          (warn "Cannot find command to execute: '%s'" command))
        )
      )
    )
  )

(add-hook 'after-save-hook 'dn-cmd-after-saved-file)

;; ========================================================================== ;;

(provide 'init-emacs)

;;; init-emacs.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
