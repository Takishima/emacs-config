;;; init-project.el --- Initialisation for project handling -*- lexical-binding: t -*-

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

(require 'use-package)

;; ========================================================================== ;;

(use-package projectile
  :straight t
  :config
  (projectile-mode +1))

(use-package projectile-ripgrep
  :straight t)

;; ========================================================================== ;;

(use-package ztree
  :straight t
)

;; ========================================================================== ;;

(use-package bufler
  :straight (:host github :repo "alphapapa/bufler.el"
                   :files (:defaults (:exclude "helm-bufler.el")))
  :bind
  ("C-x C-b" . bufler-list)                ;; orig. list-buffers
  ;; :custom
  ;; (bufler-face-prefix "prism-level-")
  :config
  (bufler-mode t)
  )

;; ========================================================================== ;;

;; Automatically guess indent offsets, tab, spaces settings, etc.

(use-package dtrt-indent
  :straight t)

;; -------------------------------------------------------------------------- ;;

(use-package project-directory
  :straight nil)

;; ========================================================================== ;;

(use-package direnv
  :if (executable-find "direnv")
  :straight t
  :config
  (direnv-mode)
  (defcustom dn-direnv-enabled-hosts nil
    "List of remote hosts to use direnv on.

     Each host must have the `direnv` executable accessible in the default environment"
    :type '(repeat string)
    :group 'dn)

  (defun tramp-sh-handle-start-file-process@dn-direnv (args)
    "Enable Direnv for hosts in `dn-direnv-enabled-hosts'."
    (message "tramp-sh-handle-start-file-process@dn-direnv")
    (with-parsed-tramp-file-name (expand-file-name default-directory) nil
      (if (member host dn-direnv-enabled-hosts)
          (pcase-let ((`(,name ,buffer ,program . ,args) args))
            `(,name
              ,buffer
              "direnv"
              "exec"
              ,localname
              ,program
              ,@args))
        args)))

  (with-eval-after-load "tramp-sh"
    (advice-add 'tramp-sh-handle-start-file-process
                :filter-args #'tramp-sh-handle-start-file-process@dn-direnv))
  )

;; ========================================================================== ;;

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1)
  )

(use-package editorconfig-generate
  :straight t
  )

(use-package editorconfig-domain-specific
  :straight t
  )

(use-package editorconfig-custom-majormode
  :straight t)

;; ========================================================================== ;;

(provide 'init-project)

;;; init-project.el ends here
