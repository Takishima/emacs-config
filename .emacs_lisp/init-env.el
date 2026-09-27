;;; init-env.el --- Environment initialisation for Emacs -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
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

;; Set `auth-source-1password-vault' to choose the default 1Password vault.

;;; Code:

;; ========================================================================== ;;

(require 'use-package)

;; ========================================================================== ;;

(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns))
          (memq system-type '(gnu gnu/linux gnu/kfreebsd)))
  :straight t
  :defines (exec-path-from-shell-variables)
  :custom
  (exec-path-from-shell-variables
   (append '("PATH" "MANPATH" "WORKON_HOME" "CPLUS_INCLUDE_PATH" "LSP_USE_PLISTS")
           (when (eq system-type 'darwin)
             '("LC_ALL" "LANG" "LD_LIBRARY_PATH" "DYLD_LIBRARY_PATH"))))
  :config
  (exec-path-from-shell-initialize)
  )

;; ========================================================================== ;;

(use-package keychain-environment
  :if (member system-type '(gnu gnu/linux gnu/kfreebsd))
  :straight t
  :config
  (keychain-refresh-environment))

;; ========================================================================== ;;

(use-package auth-source-1password
  :straight t
  :config
  (auth-source-1password-enable)
  )

(use-package aio
  :straight t)

;; ========================================================================== ;;

(use-package epg
  :straight nil
  :custom
  (epg-pinentry-mode 'loopback))

;; ========================================================================== ;;

(provide 'init-env)

;;; init-env.el ends here
