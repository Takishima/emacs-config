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

(use-package leuven-theme
  :straight t
  :load-path "themes"
  :config
  (load-theme 'leuven t))

;; ========================================================================== ;;

(use-package which-key
  :straight t
  :config
  (which-key-mode +1))

;; ========================================================================== ;;

(custom-set-variables '(show-paren-mode 1)
                      '(line-number-mode t)
                      '(column-number-mode t))

(use-package hl-line+
  :config
  (hl-line-when-idle-interval 0.2)
  (toggle-hl-line-when-idle 1))

(global-hl-line-mode 0)

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
  (require 'dn-dashboard-worktrees)
  (dn-dashboard-worktrees-setup)
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
  "Adjust the font size by HEIGHT, or reset it to `dn-default-font-size' if 0.
Also resize the mode line and, when `company-box' is loaded, its icons."
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
    (when (fboundp 'company-box-icons-resize)
      (company-box-icons-resize new-size)))
  )

;; ========================================================================== ;;

(use-package printing
  :straight nil
  :config
  (pr-update-menus))

;; ========================================================================== ;;

(provide 'init-ui)

;;; init-ui.el ends here
