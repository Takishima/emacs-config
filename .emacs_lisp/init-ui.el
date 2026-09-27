;;; init-ui.el --- Initialisation for the user interface -*- lexical-binding: t -*-

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

(require 'use-package)

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

;; Always show matching paranthesis, line and column number
(custom-set-variables '(show-paren-mode 1)
		      '(line-number-mode t)
		      '(column-number-mode t))

(use-package hl-line+
  :config
  (hl-line-when-idle-interval 0.2)
  (toggle-hl-line-when-idle 1))

(global-hl-line-mode 0)

;; Remove redundant UI
(tool-bar-mode -1)

;; ========================================================================== ;;

(defcustom dn-dashboard-worktrees-path nil
  "Path to the main Git repository for displaying worktrees in dashboard.
If nil, no worktrees will be displayed. Should be the path to the main
Git repository (not a worktree)."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Repository path"))
  :group 'dn)

(use-package dashboard
  :straight t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (worktrees . 10)))
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
  (setq dashboard-worktrees-path dn-dashboard-worktrees-path)
  :init
  (defun dn-home ()
    "Switch to home (dashboard) buffer."
    (interactive)
    (switch-to-buffer "*dashboard*"))
  )
(load-file (expand-file-name "patches/dashboard-worktrees-patch.el" config-dotemacs-lisp))

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

;; ========================================================================== ;;

(use-package display-line-numbers
  :straight nil
  :config
  (global-display-line-numbers-mode 1))

;; ========================================================================== ;;

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
                    (+ (/ height 5) (if (boundp 'treemacs--icon-size)
                                        treemacs--icon-size
                                      dn-default-icon-size)))))
    ;; (when (fboundp 'treemacs-resize-icons)
    ;;   (treemacs-resize-icons new-size))
    (when (fboundp 'company-box-icons-resize)
      (company-box-icons-resize new-size)))
  )

;; ========================================================================== ;;

(provide 'init-ui)

;;; init-ui.el ends here
