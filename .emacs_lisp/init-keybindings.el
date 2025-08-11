;;; init-keybindings.el --- Initialise keybindings -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
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

(require 'config-functions (concat config-dir "functions.el"))
(require 'use-package)

;; ========================================================================== ;;

(global-set-key (kbd "M-s M-l") 'sort-lines)
(global-set-key (kbd "s-R") 'revert-all-buffers)
(global-set-key (kbd "s-r") 'revert-buffer)

(config-when-system 'darwin
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	default-input-method "MacOSX")

  ;; Apple Swiss keyboard layout...
  (global-set-key (kbd "s-g") (lambda() (interactive) (insert "@")))
  (global-set-key (kbd "s-3") (lambda() (interactive) (insert "#")))
  (global-set-key (kbd "s-4") (lambda() (interactive) (insert "Ç")))
  (global-set-key (kbd "s-5") (lambda() (interactive) (insert "[")))
  (global-set-key (kbd "s-6") (lambda() (interactive) (insert "]")))
  (global-set-key (kbd "s-7") (lambda() (interactive) (insert "|")))
  (global-set-key (kbd "s-8") (lambda() (interactive) (insert "{")))
  (global-set-key (kbd "s-9") (lambda() (interactive) (insert "}")))
  (global-set-key (kbd "s-/") (lambda() (interactive) (insert "\\")))
  (global-set-key (kbd "s-n") (lambda() (interactive) (insert "~")))
  ;; (global-set-key (kbd "±") 'text-scale-increase)
  ;; (global-set-key (kbd "–") 'text-scale-decrease)
  )

;; ========================================================================== ;;

(provide 'init-keybindings)

;;; init-keybindings.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
