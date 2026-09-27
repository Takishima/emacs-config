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

(use-package vertico
  :straight t
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :config
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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

(use-package vertico-buffer
  :after vertico
  :straight nil
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

(use-package vertico-prescient
  :after vertico prescient
  :straight t
  :custom
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil)
  (vertico-prescient-enable-filtering nil) ; Orderless does the filtering.
  ;; The next two only take effect when `vertico-prescient-enable-filtering'
  ;; is non-nil.
  (vertico-prescient-completion-styles '(prescient flex))
  (vertico-prescient-completion-category-overrides
   '(;; Include `partial-completion' to enable wildcards and partial paths.
     (file (styles partial-completion prescient))
     ;; Eglot forces `flex' by default.
     (eglot (styles prescient flex))))
  :config
  (vertico-prescient-mode 1))


;; Vertico sorts by history position.
(use-package savehist
  :straight nil
  :init
  (savehist-mode))

(defun dn-completion-styles-setup ()
  "Set `completion-styles' to orderless and hotfuzz, where loaded.
Fall back on `basic' and `flex' respectively."
  (setopt completion-styles (list (if (featurep 'orderless)
                                      'orderless 'basic)
                                  (if (featurep 'hotfuzz)
                                      'hotfuzz 'flex))))

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  (defun dn-crm-indicator (args)
    "Prefix the `completing-read-multiple' prompt in ARGS with [CRM<separator>].
For example [CRM,] when `crm-separator' is a comma."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'dn-crm-indicator)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'after-init-hook 'dn-completion-styles-setup))

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

(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-field-width 80)
  (marginalia-align-offset -2)
  :config
  (marginalia-mode 1))


(use-package hotfuzz
  :straight t)


(defun dn--consult-line-thing-at-point ()
  "Run `consult-line' with the \"thing\" found near point as initial input.
The \"thing\" is the first of option `isearch-forward-thing-at-point' that
`bounds-of-thing-at-point' finds.  Without one, run `consult-line'
with empty input."
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

(use-package consult
  :straight t
  :bind (
         ("C-s" . consult-line)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s ." . dn--consult-line-thing-at-point)
         ("M-s c" . consult-locate)
         ("M-s d" . consult-fd)
         ("M-s D" . consult-dash)
         ("M-s f" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)                  ; Needed by consult-line to detect isearch.
         ("M-s L" . consult-line-multi)            ; Needed by consult-line to detect isearch.
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (consult-fd-args "fd --color=never --full-path --hidden")
  (consult-narrow-key "<")
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  ;; In :preface, not :init, so the function exists before a `.dir-locals.el'
  ;; value or `consult--project-root' can call it.
  :preface
  (defun dn--consult-vc-project-root (&optional _may-prompt)
    "Return the enclosing VC (Git) root as an absolute path, or nil.
Meant as a directory-local `consult-project-function' in repositories
whose nested project markers make project.el or projectile pick a
subdirectory as the root, so that consult searches the whole worktree."
    (when-let* ((root (vc-root-dir)))
      (expand-file-name root)))

  :init
  (advice-add #'register-preview :override #'consult-register-window)

  ;; `consult-project-function' is a risky local variable, so only an exact
  ;; match in `safe-local-variable-values' spares the confirmation prompt.
  (add-to-list 'safe-local-variable-values
               '(consult-project-function . dn--consult-vc-project-root))

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))
  )


(use-package consult-dir
  :straight (:host github :repo "karthink/consult-dir" :files ("*.el"))
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
  (("C-:" . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :custom
  (prefix-help-command #'embark-prefix-help-command)

  :init
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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
