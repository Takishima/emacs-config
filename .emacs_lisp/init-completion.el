;;; init-completion.el --- Initialise completion -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
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

(require 'use-package)

;; ========================================================================== ;;

(use-package prescient
  :straight t
  :defer t
  :config (prescient-persist-mode))

;; ========================================================================== ;;

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
  :straight nil
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
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Vertico-prescient
(use-package vertico-prescient
  :after vertico prescient
  :straight nil
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
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
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
     orderless-literal))
  (orderless-style-dispatchers
   '(orderless-affix-dispatch))
  (orderless-affix-dispatch-alist
   `((?% . ,#'char-fold-to-regexp)
     (?! . ,#'orderless-not)
     (?& . ,#'orderless-annotation)
     (?, . ,#'orderless-initialism)
     (?= . ,#'orderless-literal)
     (?^ . ,#'orderless-literal-prefix)
     (?~ . ,#'orderless-flex)
     (?$ . ,#'orderless-regexp)))
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
  :straight t
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
         ;; ("M-s D" . devdocs-lookup)
         ("M-s D" . consult-dash)
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

  ;; Custom variables
  :custom
  ;; Configure fd to search hidden files (like ripgrep)
  (consult-fd-args "fd --color=never --full-path --hidden")
  ;; Configure the narrowing key
  (consult-narrow-key "<")
  ;; Register preview configuration
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  ;; The :preface is evaluated earliest (and at byte-compile time), so the
  ;; helper is always defined before anything -- including .dir-locals values
  ;; and `consult--project-root' -- can reference it.
  :preface
  ;; Make consult-grep/ripgrep/fd search from the enclosing Git worktree
  ;; root instead of the nearest project.el/projectile root.  Used via
  ;; .dir-locals in repos with nested project markers (e.g. the paragon
  ;; monorepo, where paragon_workspace/ carries its own package.xml +
  ;; CMakeLists.txt and projectile picks it up as the root).  Referencing a
  ;; named function keeps the dir-local value stable, so it can be
  ;; whitelisted below -- `consult-project-function' is a risky local
  ;; variable (it matches `-function$'), and a `safe-local-variable'
  ;; predicate is ignored for risky vars; only an exact value match in
  ;; `safe-local-variable-values' suppresses the confirmation prompt.
  (defun dn--consult-vc-project-root (&optional _may-prompt)
    "Return the enclosing VC (Git) root as an absolute path, or nil."
    (when-let* ((root (vc-root-dir)))
      (expand-file-name root)))

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (add-to-list 'safe-local-variable-values
               '(consult-project-function . dn--consult-vc-project-root))

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
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

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

  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)

  :init

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

(use-package wgrep
  :straight (:host github :repo "mhayashi1120/Emacs-wgrep" :files ("wgrep.el"))
)

;; ========================================================================== ;;

(provide 'init-completion)

;;; init-completion.el ends here
