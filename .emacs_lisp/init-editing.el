;;; init-editing.el --- Initialisation for text editing -*- lexical-binding: t -*-

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

;; Disable auto backup files
(custom-set-variables '(make-backup-files nil))

;; Indent with spaces
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace in files
(autoload 'nuke-trailing-whitespace "whitespace" nil t)

;; Easier question answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make script file executable by default
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(save-place-mode t)

;; (unless (and (version< emacs-version "27")
;;              (require 'so-long nil :noerror))
;;     (package-install 'so-long))
(global-so-long-mode)

(global-subword-mode)  ; navigationInCamelCase

(delete-selection-mode)

;; ========================================================================== ;;

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

(defun dn-display-ansi-colors ()
  "Render the ANSI colour codes in the current buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

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

(global-set-key (kbd "M-s M-l") 'sort-lines)
(global-set-key (kbd "s-R") 'revert-all-buffers)
(global-set-key (kbd "s-r") 'revert-buffer)

;; ========================================================================== ;;

;; Load custom abbrev file
(setq-default abbrev-mode t)
(read-abbrev-file (expand-file-name "abbrev_defs" config-dotemacs-lisp))
(setq save-abbrevs t)

(defun en-abb ()
  (interactive)
  (kill-all-abbrevs)
  (read-abbrev-file)
  (read-abbrev-file (expand-file-name "abbrev_en_defs" config-dotemacs-lisp))
  )

(defun fr-abb ()
  (interactive)
  (kill-all-abbrevs)
  (read-abbrev-file)
  (read-abbrev-file (expand-file-name "abbrev_fr_defs" config-dotemacs-lisp))
  )

;; ========================================================================== ;;

;; Load Yasnippet

(unless (boundp 'config-yasnippet-dir)
  (defconst config-yasnippet-dir (concat config-dotemacs-lisp "snippets/")))

(use-package yasnippet
  :straight t
  :defer 1
  :custom
  (yas-indent-line 'auto)
  (yas-inhibit-overlay-modification-protection t)
  :custom-face
  (yas-field-highlight-face ((t (:inherit region))))
  :init
  (setq yasnippet-snippets-dir "")
  :config
  (add-to-list 'yas-snippet-dirs config-yasnippet-dir)
  (yas-reload-all)
  (yas-global-mode t)
  )

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  :config
  (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir t)
  )

(dn-load-directory (concat config-dotemacs-lisp "yas-lib") "yas-lib-")

;; ========================================================================== ;;

(provide 'init-editing)

;;; init-editing.el ends here
