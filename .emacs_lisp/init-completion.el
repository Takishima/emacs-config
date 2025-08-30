;;; init-completion.el --- Initialise completion -*- lexical-binding: t -*-

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

;;; Code:

;; ========================================================================== ;;

(require 'use-package)

;; ========================================================================== ;;

;; Load custom abbrev file
(setq-default abbrev-mode t)
(read-abbrev-file (expand-file-name "abbrev_defs" config-dotemacs-lisp))
(setq save-abbrevs t)

(defun en-abb ()
  (interactive)
  (kill-all-abbrevs)
  (read-abbrev-file)
  (read-abbrev-file (expand-file-name "abbrev_en_defs" config-dotemacs-lisp))
  )

(defun fr-abb ()
  (interactive)
  (kill-all-abbrevs)
  (read-abbrev-file)
  (read-abbrev-file (expand-file-name "abbrev_fr_defs" config-dotemacs-lisp))
  )

;; ========================================================================== ;;

(use-package prescient
  :straight t
  :defer t
  :config (prescient-persist-mode))

;; ========================================================================== ;;

;; ========================================================================== ;;

(use-package diminish
  :straight (:host github :repo "emacsmirror/diminish" :files ("*.el")))

;; ========================================================================== ;;
;; Load Yasnippet

(unless (boundp 'config-yasnippet-dir)
  (defconst config-yasnippet-dir (concat config-dotemacs-lisp "snippets/")))

(use-package yasnippet
  :straight t
  :custom
  (yas-indent-line 'auto)
  (yas-inhibit-overlay-modification-protection t)
  :custom-face
  (yas-field-highlight-face ((t (:inherit region))))
  :init
  (setq yasnippet-snippets-dir "")
  :config
  (yas-reload-all)
  (yas-global-mode t)
  )

(use-package yasnippet-snippets
  :straight t
  :config
  (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir t)
  )

(let* (
       (dir-path (file-name-as-directory (concat config-dotemacs-lisp "yas-lib")))
       (dir-list (directory-files dir-path t "^[^#\\.].*\\.el$"))
       (require-name)
       )
  (dolist (fname dir-list)
    (setq require-name (intern-soft (concat "yas-lib-"
                                            (file-name-sans-extension (file-name-nondirectory fname)))))
    (if require-name
        (progn
          (message "INFO: requiring %s from %s" require-name fname)
          (require require-name fname)
          )
      (progn
        (setq fname (file-name-sans-extension fname))
        (message "INFO: loading %s" fname)
        (load fname)
        )
      )
    )
  )

;; ========================================================================== ;;

(provide 'init-completion)

;;; init-completion.el ends here
