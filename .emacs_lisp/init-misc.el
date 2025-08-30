;;; init-misc.el --- Miscellaneous initialisations -*- lexical-binding: t -*-

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

(require 'config-functions (concat config-dir "functions.el"))
(require 'use-package)

;; ========================================================================== ;;

(use-package epg
  :straight t
  :custom
  (epg-pinentry-mode 'loopback))

;; -------------------------------------------------------------------------- ;;

(use-package emojify
  :straight t
  :custom
  (emojify-inhibit-major-modes '(dired-mode
                                 doc-view-mode
                                 debugger-mode
                                 pdf-view-mode
                                 image-mode
                                 help-mode
                                 ibuffer-mode
                                 magit-popup-mode
                                 magit-diff-mode
                                 nix-mode
                                 ert-results-mode
                                 compilation-mode
                                 proced-mode
                                 mu4e-headers-mode
                                 deft-mode
                                 yaml-mode
                                 prog-mode))
  ;; :hook (after-init . global-emojify-mode)
  )

(use-package all-the-icons
  :straight t)

(use-package all-the-icons-ivy
  :straight t)

(use-package pacfiles-mode
  :straight t)

;; -------------------------------------------------------------------------- ;;

(use-package display-line-numbers
  :straight t
  :config
  (global-display-line-numbers-mode 1))

;; -------------------------------------------------------------------------- ;;

(use-package printing
  :straight t
  :config
  (pr-update-menus))

;; -------------------------------------------------------------------------- ;;

(config-when-system 'darwin
  ;; Allow editing of binary .plist files.
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])
  ;;It is necessary to perform an update!
  (jka-compr-update)
  )

;; ========================================================================== ;;

(provide 'init-misc)

;;; init-misc.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
