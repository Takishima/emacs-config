;;; init-completion.el --- Initialise completion -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
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
  :ensure t
  :defer t
  :config (prescient-persist-mode))

;; ========================================================================== ;;

(use-package diminish
  :ensure t)

(use-package ivy
  :ensure t
  :diminish (ivy-mode counsel-mode)
  :custom
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (ivy-height 10)
  ;; does not count candidates
  (ivy-count-format "")
  ;; no regexp by default
  (ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (ivy-re-builders-alist
   ;; allow input not in order
   '((t   . ivy--regex-ignore-order)))

  :bind
  (:map ivy-mode-map
	("C-x C-r" . counsel-recentf)
	("C-s" . swiper)
	("M-s ." . swiper-symbol-at-point)
	("C-x C-g" . counsel-git)
	("C-c j" . counsel-git-grep)
	("C-x l" . counsel-locate)
	("C-x b" . counsel-switch-buffer)
	("C-x C-f" . counsel-find-file)
	("C-'" . ivy-avy))
  :config
  ;; Disable ido mode
  (ido-mode nil)
  ;; Enable ivy mode
  (ivy-mode 1)

  (defun swiper-symbol-at-point ()
    "Call swiper with symbol at point."
    (interactive)
    (let ((bounds (find-tag-default-bounds)))
      (cond
       (bounds
	(when (< (car bounds) (point))
	  (goto-char (car bounds)))
	(swiper (buffer-substring-no-properties (car bounds) (cdr bounds))
		)
	))))
  (setq magit-completing-read-function 'ivy-completing-read)
  )
(ivy-mode 1)

;; -------------------------------------------------------------------------- ;;

(use-package ivy-prescient
  :ensure t
  :after ivy
  :config (ivy-prescient-mode)
  )

;; ========================================================================== ;;
;; Load Yasnippet

(unless (boundp 'config-yasnippet-dir)
  (defconst config-yasnippet-dir (concat config-dotemacs-lisp "snippets/")))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  )

;; ========================================================================== ;;
;; Load Company mode

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 3)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  ;; Disable `single-candidate' and `echo-area' frontends
  (company-frontends '(company-box-frontend))
  :config
  (global-company-mode)
  )

;; -------------------------------------------------------------------------- ;;

(use-package company-box
  :ensure t
  :custom
  (company-box-show-single-candidate t)
  ;;(company-box-frame-behavior 'point)
  (company-box-icon-right-margin 0.5)
  (company-box-backends-colors '((company-yasnippet . (:annotation default))))
  :hook
  (company-mode . company-box-mode)
  )

;; -------------------------------------------------------------------------- ;;

(use-package company-prescient
  :ensure t
  :after company
  :config (company-prescient-mode)
  )

;; ========================================================================== ;;

(provide 'init-completion)

;;; init-completion.el ends here
