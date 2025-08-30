;;; init-org.el --- Initialisation for Org-mode -*- lexical-binding: t -*-

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

;; Not fully tested

;;; Code:

;; ========================================================================== ;;

(require 'use-package)

(straight-use-package 'org)
;; ========================================================================== ;;

(use-package org
  :straight t
  :defines org-directory
  :custom
  (org-hide-leading-stars t)
  (org-enable-table-editor 1)
  (org-log-done t)
  (org-log-done '(done))
  (org-agenda-include-diary t)
  (org-read-date-popup-calendar nil)

  :config
  (custom-set-variables
    '(org-default-notes-file (expand-file-name "notes.org" org-directory))
    '(org-capture-templates
	'(("t" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("n" "Note" entry (file+headline (concat org-directory "/notes.org") "Notes")
	   "* %? :NOTE:\n%U")
	  ))
    )

  (add-hook 'org-mode-hook
	    '(lambda () (smiley-buffer (current-buffer))))

  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (C . t)
     (latex . t)
     (css . t)
     (matlab . t)
     (python . t)
     ))

  ;; Display images in org mode
  (iimage-mode)

  ;; Add the org file link format to the iimage mode regex
  (add-to-list 'iimage-mode-image-regex-alist
	       (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]") 1))

  ;; Add a hook so we can display images on load
  (add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))

  ;; Function to setup images for display on load
  (defun org-turn-on-iimage-in-org ()
    "display images in your org file"
    (interactive)
    (turn-on-iimage-mode)
    (set-face-underline-p 'org-link nil))

  ;; Function to toggle images in a org buffer
  (defun org-toggle-iimage-in-org ()
    "display images in your org file"
    (interactive)
    (if (face-underline-p 'org-link)
	(set-face-underline-p 'org-link nil)
      (set-face-underline-p 'org-link t))
    (call-interactively 'iimage-mode))
  )

(use-package ox-gfm
  :straight t
  :after org)

;; ========================================================================== ;;

(provide 'init-org)

;;; init-org.el ends here
