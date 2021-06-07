;;; init-prog-build-systems.el --- Initialisation for build systems -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Homepage: nil
;; Keywords: keywords

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

(use-package cmake-mode
  :ensure t
  :mode
  ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  )

;; -------------------------------------------------------------------------- ;;

(use-package cmake-format
  :ensure nil
  :load-path config-dotemacs-lisp
  :bind
  (:map cmake-mode-map
        (("C-c i" . compile-in-iterm)
         ("C-c C-f" . cmake-format-buffer)
         ))
  :hook
  (cmake-mode . cmake-format-mode)
  )

;; -------------------------------------------------------------------------- ;;

;; (use-package rtags
;;   :ensure t
;;   )

;; (use-package cmake-ide
;;   :ensure t
;;   :custom
;;   ((cmake-ide-header-search-other-file nil)
;;    (cmake-ide-header-search-first-including nil)
;;    )
;;   :config
;;   (cmake-ide-setup)
;;   (setq cmake-ide-flags-c++ (append '("-std=c++17")))
;;   )

;; -------------------------------------------------------------------------- ;;

;; Find makefile by going up the directory hierarchy
(require 'project-directory)

;; -------------------------------------------------------------------------- ;;

(use-package make-mode
  :ensure nil
  :mode ("Makefile\\'" "makefile\\'" "Make.obj\\'")
  )

;; ========================================================================== ;;


(use-package pkgbuild-mode
  :ensure t
  :mode "/PKGBUILD$"
  :functions pkgbuild-update-srcinfo
  :config
  (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)

  (defun pkgbuild-gen-srcinfo-before-save-hook ()
    (when (eq major-mode 'pkgbuild-mode)
      (pkgbuild-update-srcinfo)
      )
    )
    (add-hook 'before-save-hook #'pkgbuild-gen-srcinfo-before-save-hook)
  )

;; ========================================================================== ;;

(provide 'init-prog-build-systems)

;;; init-prog-build-systems.el ends here
