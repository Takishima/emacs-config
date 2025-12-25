;;; init-custom.el --- Initialisation for customizations -*- lexical-binding: t -*-

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

(let (
      (lsp-mode-disabled '(emacs-lisp-mode lisp-mode makefile-mode direnv-envrc-mode))
      )
  (progn
    (unless (member system-type '(windows-nt ms-dos))
      (add-to-list 'lsp-mode-disabled 'powershell-mode t)
      )
    (add-to-list 'lsp-mode-disabled 'bat-mode t)

    (defcustom dn-lsp-mode-disabled lsp-mode-disabled
      "List of modes for which lsp-mode is disabled"
      :type '(repeat symbol)
      :group 'dn)
    )
  )

;; ========================================================================== ;;

(defun dn-editorconfig-major-mode-hook ()
  (when (boundp 'editorconfig-major-mode-hook)
      (editorconfig-major-mode-hook)
      )
  )

;; ========================================================================== ;;

(provide 'init-custom)

;;; init-custom.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
