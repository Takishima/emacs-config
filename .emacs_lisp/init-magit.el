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
(require 'org)

;; ========================================================================== ;;

(use-package magit
  :straight t
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

  (defun magit-org-read-date (&rest _ignored)
    (org-read-date))

  (transient-define-argument magit-log:--since ()
    :description "Show commits more recent than a specific date."
    :class 'transient-option
    :key "?S"
    :argument "--since="
    :reader #'magit-org-read-date)

  (transient-define-argument magit-log:--until ()
    :description "Show commits older than a specific date."
    :class 'transient-option
    :key "?U"
    :argument "--until="
    :reader #'magit-org-read-date)

  ;; ------------------------------------------------------------------------ ;;
  ;; Register new transients

  (transient-append-suffix 'magit-log "-L"
    '(magit-log:--since))

  (transient-append-suffix 'magit-log "?S"
    '(magit-log:--until))

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
  :straight t
  :hook (magit-mode . turn-on-magit-gitflow)
  )

(use-package magit-popup
  :straight t
  )

;; ========================================================================== ;;

(use-package magit-delta
  :straight t
  :ensure-system-package (delta . git-delta)
  :hook (magit-mode . magit-delta-mode)
  :custom
  (magit-delta-delta-args '("--max-line-distance" "0.6" "--true-color" "always" "--color-only" "--features" "magit-delta"))
  )

;; ========================================================================== ;;

(use-package difftastic
  :straight t
  :commands magit
  :after magit
  :bind (:map magit-blame-read-only-mode-map
         ("D" . difftastic-magit-show)
         ("S" . difftastic-magit-show))
  :init
  (transient-append-suffix 'magit-diff '(-1 -1)
      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)]))


;; ========================================================================== ;;

(provide 'init-magit)

;;; init-magit.el ends here
