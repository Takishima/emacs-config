;;; init-custom-functions.el --- Initialise custom-functions -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: nil
;; Keywords: init


;; MIT License

;; Copyright (c) 2023 Damien Nguyen

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

(require 'config-functions (concat config-dir "functions.el"))

;; ============================================================================ ;;

(defun dn-display-ansi-colors ()
  "Display a (text) buffer with ANSI color codes"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; ============================================================================ ;;

(defun dn-recompile-elpa ()
  "Recompile all Emacs lisp files in package directory"
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force)
  )

;; ---------------------------------------------------------------------------- ;;

(defun dn-reinstall-essentials ()
  "Recompile a list of essential Emacs packages."
  (interactive)
  (package-refresh-contents)
  (dolist (pkg '("auctex"
                 "counsel"
                 "direnv"
                 "flycheck"
                 "ivy"
                 "lsp-ivy"
                 "lsp-mode"
                 "lsp-pyright"
                 "lsp-treemacs"
                 "lsp-ui"
                 "magit"
                 "magit-delta"
                 "magit-gitflow"
                 "magit-popup"
                 "magit-section"
                 "multiple-cursors"
                 "smart-shift"
                 "treemacs"
                 "yasnippet"))
    (unless (ignore-errors
              (package-reinstall (intern pkg)))
        (warn "Package %s failed to reinstall" pkg)))
  )

;; ---------------------------------------------------------------------------- ;;

(defun dn-reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))

;; ============================================================================ ;;

(defcustom dn-do-clang-format nil "Do clang-format on files (off by default)")
(defun dn-clang-format-save-hook ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (and dn-do-clang-format (f-exists? (expand-file-name ".clang-format" (projectile-project-root))))
                (clang-format-buffer))
              nil)
            nil
            ;; Buffer local hook.
            t))
(add-hook 'c-mode-common-hook (lambda () (dn-clang-format-save-hook)))
;; (remove-hook 'c-mode-common-hook (lambda () (dn-clang-format-save-hook)))

;; ========================================================================== ;;

(provide 'init-custom-functions)

;;; init-custom-functions.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
