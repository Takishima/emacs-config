;;; init-prog-cpp.el --- C++ support -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: (use-package)
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

(require 'use-package)
(require 'config-functions (concat config-dir "functions.el"))

;; ========================================================================== ;;

(use-package cc-mode
  :straight nil
  :custom
  (c-basic-indent 5)
  (c-basic-offset 5)
  (c-default-style '((c-mode . "stroustrup")
                     (c++-mode . "stroustrup")
                     (java-mode . "java")
                     (awk-mode . "awk")
                     (other . "gnu")))
  :mode
  (
   ("\\.h$" . c++-mode)
   ("\\.hpp$" . c++-mode)
   ("\\.hxx$" . c++-mode)
   ("\\.cc$" . c++-mode)
   ("\\.cpp$" . c++-mode)
   ("\\.cxx$" . c++-mode)
   ("\\.tpp$" . c++-mode)
   ("\\.txx$" . c++-mode))
  )

;; ========================================================================== ;;

(use-package cuda-mode
  :straight t)

;; ========================================================================== ;;

(use-package modern-cpp-font-lock
  :straight t
  :hook (c++-mode . modern-c++-font-lock-mode)
  )

(add-hook 'c++-mode-hook
	  '(lambda()
	     (progn
	       (define-key c-mode-base-map (kbd "C-c c")  'recompile)
	       (define-key c++-mode-map (kbd "C-c i") 'compile-in-iterm)
	       (define-key c++-mode-map (kbd "C-c \\") 'c-backslash-region)
	       )
	     )
	  t)
(add-hook 'c++-mode-hook 'which-function-mode)

;; -------------------------------------------------------------------------- ;;

(custom-set-variables '(c-basic-indent 5)
		      '(indent-tabs-mode nil))

;; ========================================================================== ;;

(use-package google-c-style
  :straight t
  )

;; ========================================================================== ;;

(use-package flycheck-clang-analyzer
  :straight t
  :functions flycheck-clang-analyzer-setup
  :after flycheck
  :config (flycheck-clang-analyzer-setup)
  )

;; -------------------------------------------------------------------------- ;;

(use-package flycheck-clang-tidy
  :straight t
  :functions flycheck-clang-analyzer-setup
  :after flycheck
  :config (flycheck-clang-tidy-setup)
  )

;; ========================================================================== ;;

;; (config-when-system 'darwin
;;   (use-package irony
;;     :straight t
;;     :after company
;;     :hook (
;;            (irony-mode . company-irony-setup-begin-commands)
;; 	   (irony-mode . irony-cdb-autosetup-compile-options)
;;            (c++-mode . irony-mode)
;;            (c-mode . irony-mode)
;;            (objc-mode . irony-mode)
;;            )
;;     :config
;;     (eval-after-load 'company
;;       '(add-to-list 'company-backends 'company-irony))
;;     (if (eq system-type 'darwin)
;;         (progn
;;           (add-to-list 'irony-additional-clang-options
;; 		       (concat "-I"(file-name-as-directory
;; 			            (car (directory-files
;; 				          "/usr/local/Cellar/llvm/"
;; 				          t
;; 				          "[0-9]\\.[0-9]\\.[0-9]")))
;; 			       "include/c++/v1/")
;; 		       t)
;;           (add-to-list 'irony-additional-clang-options
;; 		       "-I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/"
;; 		       t)
;;           )
;;       )

;;     (defun my-irony-mode-hook ()
;;       (define-key irony-mode-map
;;         [remap completion-at-point] 'counsel-irony)
;;       (define-key irony-mode-map
;;         [remap complete-symbol] 'counsel-irony))
;;     (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;     )

;;   (use-package flycheck-irony
;;     :straight t
;;     :after flycheck
;;     :functions flycheck-irony-setup
;;     :config
;;     (global-flycheck-mode)
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;;     )
;;   )

;; ========================================================================== ;;

(use-package clang-format
  :straight t
  :bind
  (:map c++-mode-map
	(("C-c C-f" . clang-format-buffer)
	 ("C-c C-r" . clang-format-region)
	 ))
  )

;; ========================================================================== ;;

(use-package demangle-mode
  :straight t
  :hook asm-mode)

;; ========================================================================== ;;

(provide 'init-prog-cpp)

;;; cpp.el ends here
