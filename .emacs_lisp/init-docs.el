;;; init-docs.el --- Initialisation for programming -*- lexical-binding: t -*-

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

;; ============================================================================================== ;;
;; DevDocs

(use-package devdocs
  :straight t
  :config
  (devdocs-update-all)
  (defun dn-devdocs-install()
    (interactive)
    (devdocs-install "bash")
    (devdocs-install "click")
    (devdocs-install "cpp")
    (devdocs-install "cmake~3.31")
    (devdocs-install "gcc~13_cpp")
    (devdocs-install "git")
    (devdocs-install "nix")
    (devdocs-install "python~3.12")
    )
  )


;; ---------------------------------------------------------------------------------------------- ;;
;; Dash docs

(use-package dash-docs
  :straight t
  :custom
  (dash-docs-enable-debugging nil)
  (dash-docs-browser-func 'eww)
  :config
  (defun dn-dash-docs-install()
    (interactive)
    (dash-docs-install-docset "Boost")
    (dash-docs-install-docset "C")
    (dash-docs-install-docset "C++")
    (dash-docs-install-docset "CMake")
    (dash-docs-install-docset "Emacs_Lisp")
    (dash-docs-install-docset "Haskell")
    (dash-docs-install-docset "Markdown")
    (dash-docs-install-docset "NumPy")
    (dash-docs-install-docset "Python_3")
    (dash-docs-install-docset "SciPy")
    (dash-docs-install-docset-from-file (concat config-dotemacs-lisp "docsets/nix.tgz"))
    (dash-docs-install-docset-from-file (concat config-dotemacs-lisp "docsets/nixpkgs.tgz"))
    (dash-docs-install-docset-from-file (concat config-dotemacs-lisp "docsets/nixos.tgz"))
    (dash-docs-install-docset-from-file (concat config-dotemacs-lisp "docsets/home-manager.tgz"))
    )
  )

(use-package consult-dash
  :straight t
  :after consult)

;; ============================================================================================== ;;

(provide 'init-docs)

;;; init-docs.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
