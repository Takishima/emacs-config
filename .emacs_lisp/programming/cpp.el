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

;; ========================================================================== ;;

(use-package cc-mode
  :ensure nil
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

(use-package modern-cpp-font-lock
  :ensure t
  :hook c++-mode-hook
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

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup)
  )

;; -------------------------------------------------------------------------- ;;

(use-package flycheck-clang-tidy
  :ensure t
  :after flycheck
  :config (flycheck-clang-tidy-setup)
  )

;; ========================================================================== ;;

(use-package irony
  :ensure t
  :after company
  ;; :hook (
  ;;        (irony-mode . company-irony-setup-begin-commands)
  ;;        (c++-mode-hook . irony-mode)
  ;;        (c-mode-hook . irony-mode)
  ;;        (objc-mode-hook . irony-mode)
  ;;        )
  :config
  ;; (eval-after-load 'company
  ;;   '(add-to-list 'company-backends 'company-irony))
  (config-with-system darwin
    (add-to-list 'irony-additional-clang-options
		 (concat "-I"(file-name-as-directory
			      (car (directory-files
				    "/usr/local/Cellar/llvm/"
				    t
				    "[0-9]\\.[0-9]\\.[0-9]")))
			 "include/c++/v1/")
		 t))
  (config-with-system darwin
    (add-to-list 'irony-additional-clang-options
		 "-I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/"
		 t))
  
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map
      [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map
      [remap complete-symbol] 'counsel-irony))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :functions flycheck-irony-setup
  :config
  (global-flycheck-mode)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )

;; ========================================================================== ;;

(use-package clang-format
  :ensure t
  :bind
  (:map c++-mode-map
	(("C-c C-f" . clang-format-buffer)
	 ("C-c C-r" . clang-format-region)
	 ))  
  )

;; ========================================================================== ;;

(use-package demangle-mode
  :ensure t
  :hook asm-mode)
 
;; ========================================================================== ;;

(provide 'init-prog-cpp)

;;; cpp.el ends here
