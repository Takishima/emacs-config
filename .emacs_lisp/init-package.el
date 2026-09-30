;;; init-package.el --- Package initialisation for Emacs -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: (package)
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

(defun dn--system-packages-refuse (pack &rest _)
  "Warn that PACK is missing instead of installing it.
With nix on PATH, `system-packages-install' would run `nix-env -i'
behind home-manager's back."
  (display-warning
   'dn (format "%s is not on PATH: add it to the Nix packages" pack)))

(pcase dn-package-manager
  ('nix
   ;; Without a handler use-package drops every form carrying `:straight'.
   ;; Nix already put the packages on `load-path', so the keyword is a no-op.
   (require 'use-package)
   (unless (memq :straight use-package-keywords)
     (push :straight use-package-keywords)
     (defun use-package-normalize/:straight (_name _keyword args) args)
     (defun use-package-handler/:straight (name _keyword _args rest state)
       (use-package-process-keywords name rest state)))
   (advice-add 'system-packages-install :override #'dn--system-packages-refuse)
   ;; early-init.el turns off startup activation for straight's sake, and
   ;; activation is what loads the autoloads of the wrapper's packages.
   (package-activate-all))
  ('straight
   (defvar bootstrap-version)
   (let ((bootstrap-file
          (expand-file-name
           "straight/repos/straight.el/bootstrap.el"
           (or (bound-and-true-p straight-base-dir)
               user-emacs-directory)))
         (bootstrap-version 7))
     (unless (file-exists-p bootstrap-file)
       (with-current-buffer
           (url-retrieve-synchronously
            "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
            'silent 'inhibit-cookies)
         (goto-char (point-max))
         (eval-print-last-sexp)))
     (load bootstrap-file nil 'nomessage))
   (straight-use-package 'org)
   (straight-use-package 'diminish))
  (other (error "dn-package-manager is %S; expected straight or nix" other)))

(require 'diminish)

(with-eval-after-load 'use-package
  (require 'use-package-diminish))

(use-package use-package
  :straight nil
  :custom
  (use-package-always-defer nil)
  (use-package-expand-minimally t))

(use-package system-packages
  :straight t)

(provide 'init-package)

;;; init-package.el ends here
