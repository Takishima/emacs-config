;;; init-package.el --- Package initialisation for Emacs -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: (package)
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

(require 'package)

(when (< emacs-major-version 27)
  (package-initialize))

(let* (
       (no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/"))
       )
  (add-to-list 'package-archives (cons "melpa" url) t))

;; ========================================================================== ;;

;; use-package
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(use-package use-package-ensure-system-package
  :ensure t)

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  )

;; ========================================================================== ;;

(provide 'init-package)

;;; init-package.el ends here
