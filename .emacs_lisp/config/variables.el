;; config-variables.el --- Configuration variables -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
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

(defgroup config nil
  "Emacs configuration"
  :group 'emacs)

(let* (
       (here (file-name-directory (or load-file-name buffer-file-name)))
       (config-root (expand-file-name (file-name-as-directory (concat here "../.."))))
       )

  (defconst config-dotemacs (file-name-as-directory (concat config-root ".emacs"))
    "Path to .emacs file.")

  (defcustom config-dotemacs-d
    (file-name-as-directory (concat config-root ".emacs.d"))
    "Path to .emacs.d directory."
    :group 'config
    :type 'directory
    )

  (defcustom config-dotemacs-lisp
    (file-name-as-directory (concat config-root ".emacs_lisp"))
    "Path to .emacs_lisp dir."
    :group 'config
    :type '(file :must-match t)
    )

  (defcustom config-dir
    (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config"))
    "Path to main configuration directory."
    :set-after '(config-dotemacs-lisp)
    :group 'config
    :type 'directory
    )
  )

(provide 'config-variables)

;;; config-variables.el ends here
