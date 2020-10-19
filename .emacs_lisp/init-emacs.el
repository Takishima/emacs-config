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

(config-with-system darwin
  (add-to-list 'default-frame-alist '(font . "Monaco" )
	       )
  )

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

;; ========================================================================== ;;

(provide 'init-emacs)

;;; init-emacs.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
