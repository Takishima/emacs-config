;;; init-magit.el --- Initialisation for MaGit -*- lexical-binding: t; coding: utf-8 -*-

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
  :defer t
  :straight (:host github :repo "pkryger/difftastic.el" :files ("*.el"))
  :commands magit
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :after magit
  :init
  (use-package transient               ; to silence compiler warnings
    :autoload (transient-get-suffix
               transient-parse-suffix))
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
     ("S" "Difftastic show" difftastic-magit-show)])
  )

;; ========================================================================== ;;

(defvar conv-commit-type-desc nil "Type of conventional commit")
(setq conv-commit-type-desc
      '(("build"
         :desc "Changes that affect the build system or external dependencies."
         :icon ?🚧
         :props (:foreground "#00008B" :height 1.2))
        ("chore"
         :desc "Updating grunt tasks."
         :icon ?🧹
         :props (:foreground "gray" :height 1.2))
        ("ci"
         :desc "Changes to CI configuration files and scripts."
         :icon ?🤖
         :props (:foreground "gray" :height 1.2))
        ("docs"
         :desc "Documentation only changes."
         :icon ?📄
         :props (:foreground "dark blue" :height 1.2))
        ("feat"
         :desc "A new feature."
         :icon ?✨
         :props (:foreground "green" :height 1.2))
        ("fix"
         :desc "A bug fix."
         :icon ?🐛
         :props (:foreground "dark red" :height 1.2))
        ("perf"
         :desc "A code change that improves performance."
         :icon ?⚡
         :props (:foreground "dark yellow" :height 1.2))
        ("refactor"
         :desc "A code changes that neither fixes a bug nor adds a feature."
         :icon ?♻
         :props (:foreground "dark green" :height 1.2))
        ("revert"
         :desc "For commits that reverts previous commit(s)."
         :icon ?🔙
         :props (:foreground "dark red" :height 1.2))
        ("style"
         :desc "Changes that do not affect the meaning of the code."
         :icon ?💄
         :props (:foreground "dark green" :height 1.2)
         )
        ("test"
         :desc "Adding missing tests or correcting existing tests."
         :icon ?🧪
         :props (:foreground "dark green" :height 1.2)
         )))

(defun add-conventional-commit-faces (&rest _args)
  "Add face properties and compose symbols for buffer from conv-commit-type-desc."
  (interactive)
  (with-silent-modifications
    (dolist (elt conv-commit-type-desc nil)
      (let*
          (
           (type-data (cdr elt))
           (regex (format "\\<\\(%s\\)\\((.*?)\\)*?[[:space:]]*\\(!\\)?[[:space:]]*:" (car elt)))
           (icon (plist-get type-data :icon))
           (face-props (plist-get type-data :props))
           )
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp regex nil t)
            (compose-region (match-beginning 1) (match-end 1) icon)
            (when face-props
              (add-face-text-property (match-beginning 1) (match-end 1) face-props))
            ;; (when (string-equal (match-string 2) "nix")
            ;;   (compose-region (match-beginning 2) (match-end 2) ?🚨)
            ;;   )
            (when (match-beginning 3)
              (compose-region (match-beginning 3) (match-end 3) ?🚨)
              )
            )
          )
        )
      )
    )
  )

(advice-add 'magit-status :after 'add-conventional-commit-faces)
(advice-add 'magit-refresh-buffer :after 'add-conventional-commit-faces)


(defun conv-commit-type-completion-decorate (type)
  "Decorate the completions candidates with icon prefix and description suffix.

TYPE is the type of conventional commit.
Return a list (candidate, icon, description)."

  (let ((type-data (cdr (assoc type conv-commit-type-desc))))
    (list
     type
     (concat
      (propertize (string (plist-get type-data :icon))
                  'face (plist-get type-data :props))
      "   ")
     (concat
      (string-pad " " (- 10 (length type)))
      (propertize (plist-get type-data :desc) 'face '(:foreground "gray" ))))))


(defun conv-commit-type-prompt ()
  (interactive)
  (consult--read conv-commit-type-desc
                 :prompt "Commit type: "
                 :annotate #'conv-commit-type-completion-decorate
                 )
  )
(defun conv-commit-prompt ()
  "Prompt for a conventional commit. and fill the buffer with the result."
  (interactive)
  (insert (conv-commit-type-prompt))
  (let ((scope (completing-read "Scope: " "")))
    (insert (if (string= scope "") "" (format "(%s)" scope))))
  (insert (if (y-or-n-p "Breaking change? ") "!" ""))
  (insert ": ")
  )

(add-hook 'git-commit-setup-hook
          #'(lambda ()
              (run-with-timer 0.5 nil #'(lambda () (when (eq (point-at-eol) (point-at-bol)) (conv-commit-prompt)))))
          )

;; ========================================================================== ;;

(provide 'init-magit)

;;; init-magit.el ends here
