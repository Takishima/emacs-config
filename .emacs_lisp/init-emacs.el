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
  :ensure t
  :load-path "themes"
  ;; :init
  ;; (setq molokai-theme-kit t)
  :config
  (load-theme 'leuven t))
	      
;; ========================================================================== ;;

;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

;; ========================================================================== ;;

(use-package browse-kill-ring
  :ensure t
  :bind (("s-y" . browse-kill-ring))
  )

;; ========================================================================== ;;

(use-package smart-shift
  :ensure t
  :config
  (global-smart-shift-mode +1))

;; ========================================================================== ;;

(use-package projectile
  :ensure t
  :bind-keymap ("s-p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy
        projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  :config
  (projectile-mode +1))

;; ========================================================================== ;;

(use-package explain-pause-mode
  :ensure nil
  :load-path (config-dotemacs-lisp)
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

(unless (and (version< emacs-version "27")
             (require 'so-long nil :noerror))
    (package-install 'so-long))
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
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 10)
                     (projects . 5)
                     (bookmarks . 5)))
  :custom-face
  (dashboard-heading-face ((t (:weight bold))))
  :config
  (dashboard-setup-startup-hook)
  (defun dn-home ()
    "Switch to home (dashboard) buffer."
    (interactive)
    (switch-to-buffer "*dashboard*"))
  :hook
  (dashboard-mode . (lambda () (setq cursor-type nil)))
  (dashboard-mode . (lambda () (face-remap-add-relative 'hl-line :weight 'bold)))
  )

;; ========================================================================== ;;

(use-package helpful
  :ensure t
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   :map emacs-lisp-mode-map
   ("C-c C-d" . helpful-at-point))
  )

;; -------------------------------------------------------------------------- ;;

(use-package whitespace-cleanup-mode
  :ensure t
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

(defconst dn-default-font-size 120)
(defconst dn-default-icon-size 15)

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
    (message "Font size: %s" new-height))
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
    (let*
        ((match (assoc (buffer-file-name) dn-script-on-save)))
      (when match
        (shell-command (cdr match))))
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
