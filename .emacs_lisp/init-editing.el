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

(custom-set-variables '(make-backup-files nil))

(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(save-place-mode t)

(global-so-long-mode)

(global-subword-mode)

(delete-selection-mode)

;; ========================================================================== ;;

(use-package multiple-cursors
  :straight t
  :bind
  (("M-m e" . mc/edit-lines)
   ("M-m s" . mc/mark-next-like-this-symbol)
   ("M-m w" . mc/mark-next-like-this-word))
  :init
  (keymap-global-unset "M-m")
  )

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

(defun shutdown-emacs-server ()
  "Kill the Emacs daemon, saving modified buffers without asking."
  (interactive)
  (let (
        (last-nonmenu-event nil)
        (window-system nil)
        )
    (save-buffers-kill-emacs t)))

;; -------------------------------------------------------------------------- ;;

(defun kill-from-line-beginning ()
  "Kill from the beginning of the line to point."
  (interactive)
  (kill-region (pos-bol) (point)))

;; -------------------------------------------------------------------------- ;;

(defun revert-all-buffers ()
  "Revert every unmodified buffer whose file still exists."
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
  "Query-replace the current `re-builder' regexp with TO-STRING from point.
Work in `reb-target-buffer' through `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

;; -------------------------------------------------------------------------- ;;

(keymap-global-set "M-s M-l" 'sort-lines)
(keymap-global-set "s-R" 'revert-all-buffers)
(keymap-global-set "s-r" 'revert-buffer)

;; ========================================================================== ;;

(setq-default abbrev-mode t)
(read-abbrev-file (expand-file-name "abbrev_defs" config-dotemacs-lisp))
(setopt save-abbrevs t)

(defun en-abb ()
  "Replace all abbrevs with those of `abbrev-file-name' plus the English ones."
  (interactive)
  (kill-all-abbrevs)
  (read-abbrev-file)
  (read-abbrev-file (expand-file-name "abbrev_en_defs" config-dotemacs-lisp))
  )

(defun fr-abb ()
  "Replace all abbrevs with those of `abbrev-file-name' plus the French ones."
  (interactive)
  (kill-all-abbrevs)
  (read-abbrev-file)
  (read-abbrev-file (expand-file-name "abbrev_fr_defs" config-dotemacs-lisp))
  )

;; ========================================================================== ;;

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
