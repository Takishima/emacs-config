;;; init-magit.el --- Initialisation for MaGit -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
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

(use-package magit
  :ensure t
  :commands magit
  :hook
  (git-commit-setup . git-commit-turn-on-flyspell)
  :config
  (make-local-variable 'split-height-threshold)
  (setq split-height-threshold 200)

  (defun magit-push-to-all-remotes ()
    "Push a branch to all the remotes."
    (interactive)
    (dolist (remote (magit-list-remotes))
      (magit-push-current (concat remote "/" (magit-get-current-branch))
			  (magit-push-arguments))))

  (defun magit-push-to-all-remotes-except-upstream ()
    "Push a branch to all the remotes (except upstream)."
    (interactive)
    (dolist (remote (magit-list-remotes))
      (
       if (not (string= remote "upstream"))
       (magit-push-current (concat remote "/" (magit-get-current-branch))
			   (magit-push-arguments))
       )
      )
    )

  (defun magit-push-to-all-remotes-except-github ()
    "Push a branch to all the remotes (except github)."
    (interactive)
    (dolist (remote (magit-list-remotes))
      (
       if (not (string-match ".*github.*" remote))
       (magit-push-current (concat remote "/" (magit-get-current-branch))
			   (magit-push-arguments))
       )
      )
    )

  ;; ------------------------------------------------------------------------ ;;
  ;; Register new transients

  (transient-append-suffix 'magit-push "e"
    '("A" "All" magit-push-to-all-remotes))

  (transient-append-suffix 'magit-push "A"
    '("a" "All (except upstream)" magit-push-to-all-remotes-except-upstream))

  (transient-append-suffix 'magit-push "a"
    '("g" "All (except github)" magit-push-to-all-remotes-except-github))
  )

;;----------------------------------------------------------------------------;;
;; Git flow support

(use-package magit-gitflow
  :ensure t
  :hook (magit-mode . turn-on-magit-gitflow)
  )

(use-package magit-popup
  :ensure t
  )

;; ========================================================================== ;;

(provide 'init-magit)

;;; init-magit.el ends here
