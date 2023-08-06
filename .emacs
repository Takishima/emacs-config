;;; .emacs --- Summary
;; My emacs config file
;;
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

(let (
      (config-root (file-name-directory (or load-file-name buffer-file-name)))
      )
  
  (defconst config-root (file-name-directory (or load-file-name buffer-file-name)))

  (defconst config-dotemacs-lisp (file-name-as-directory (concat config-root ".emacs_lisp"))
    "Path to .emacs directory.")

  (defconst config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config"))
    "Path to main configuration directory.")
  )

;; ========================================================================== ;;

(require 'config-variables (concat config-dir "variables.el"))
(require 'config-functions (concat config-dir "functions.el"))

;; -------------------------------------------------------------------------- ;;

(config-load-file-exec-func (concat config-dir "init-pre.el")
			    'config-init-pre
			    nil)

(config-require 'init-custom)
(custom-set-variables '(custom-file (concat config-dotemacs-lisp "custom.el")))
(load custom-file)

;; ========================================================================== ;;

(config-require 'init-package)
(config-require 'init-env)
(config-require 'init-emacs)
(config-require 'init-multiple-cursors)
(config-require 'init-magit)
(config-require 'init-completion)
(config-require 'init-programming)
(config-require 'init-auctex)
(config-require 'init-ispell)

(config-require 'init-org)
(config-require 'init-misc)

(config-require 'init-keybindings)
(config-require 'init-custom-functions)

;; ========================================================================== ;;

(config-load-file-exec-func (concat config-dir "init-post.el")
			    'config-init-post
			    nil)

;; ========================================================================== ;;

;; Bring the window into focus
(when (memq window-system '(mac ns x))
    (x-focus-frame nil))

;; (provide '.emacs)
;;; .emacs ends here

(put 'narrow-to-region 'disabled nil)
