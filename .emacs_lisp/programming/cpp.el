;;; cpp.el --- C++ support -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: (use-package)
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
(require 'config-functions (concat config-dir "functions.el"))

;; ========================================================================== ;;

(defun dn-cpp-docs-setup ()
  "Set the devdocs and dash docsets for C/C++ buffers."
  (setq-local devdocs-current-docs '("cpp"))
  (setq-local dash-docs-docsets '("C++" "C")))

(use-package cc-mode
  :straight nil
  :custom
  (c-basic-offset 5)
  (c-default-style '((c-mode . "stroustrup")
                     (c++-mode . "stroustrup")
                     (java-mode . "java")
                     (awk-mode . "awk")
                     (other . "gnu")))
  :hook (c-mode-common . dn-cpp-docs-setup)
  :bind (:map c-mode-base-map
              ("C-c c" . recompile)
              :map c++-mode-map
              ("C-c \\" . c-backslash-region))
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

(use-package c-ts-mode
  :straight nil
  :custom
  (c-ts-mode-indent-offset 5)
  :hook ((c-ts-base-mode . dn-cpp-docs-setup)
         (c++-ts-mode . which-function-mode))
  :bind (:map c-ts-base-mode-map
              ("C-c c" . recompile)))

;; ========================================================================== ;;

(use-package cuda-mode
  :straight t)

;; ========================================================================== ;;

(use-package modern-cpp-font-lock
  :straight t
  :hook (c++-mode . modern-c++-font-lock-mode)
  )

(add-hook 'c++-mode-hook 'which-function-mode)

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
  :functions flycheck-clang-tidy-setup
  :after flycheck
  :config (flycheck-clang-tidy-setup)
  )

;; ========================================================================== ;;

(use-package clang-format
  :straight t
  :bind
  (:map c++-mode-map
	(("C-c C-f" . clang-format-buffer)
	 ("C-c C-r" . clang-format-region)
	 )
   :map c++-ts-mode-map
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
