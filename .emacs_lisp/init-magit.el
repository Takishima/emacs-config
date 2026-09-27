;;; init-magit.el --- Initialisation for MaGit -*- lexical-binding: t; coding: utf-8 -*-

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
(require 'org)

;; ========================================================================== ;;

(defun dn-magit-split-height-threshold ()
  "Set `split-height-threshold' to 200 in the current magit buffer."
  (setq-local split-height-threshold 200))

(use-package magit
  :straight t
  :commands magit
  :hook
  (git-commit-setup . git-commit-setup-flyspell)
  (magit-mode . dn-magit-split-height-threshold)
  :config
  (defun dn-magit-push-to-all-remotes ()
    "Push a branch to all the remotes."
    (interactive)
    (dolist (remote (magit-list-remotes))
      (magit-push-current (concat remote "/" (magit-get-current-branch))
                          (magit-push-arguments))))

  (defun dn-magit-push-to-all-remotes-except-upstream ()
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

  (defun dn-magit-push-to-all-remotes-except-github ()
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

  (defun dn-magit-org-read-date (&rest _ignored)
    (org-read-date))

  (transient-define-argument magit-log:--since ()
    :description "Show commits more recent than a specific date."
    :class 'transient-option
    :key "?S"
    :argument "--since="
    :reader #'dn-magit-org-read-date)

  (transient-define-argument magit-log:--until ()
    :description "Show commits older than a specific date."
    :class 'transient-option
    :key "?U"
    :argument "--until="
    :reader #'dn-magit-org-read-date)

  ;; ------------------------------------------------------------------------ ;;
  ;; Register new transients

  (transient-append-suffix 'magit-log "-L"
    '(magit-log:--since))

  (transient-append-suffix 'magit-log "?S"
    '(magit-log:--until))

  (transient-append-suffix 'magit-push "e"
    '("A" "All" dn-magit-push-to-all-remotes))

  (transient-append-suffix 'magit-push "A"
    '("a" "All (except upstream)" dn-magit-push-to-all-remotes-except-upstream))

  (transient-append-suffix 'magit-push "a"
    '("g" "All (except github)" dn-magit-push-to-all-remotes-except-github))

  ;; ------------------------------------------------------------------------ ;;
  ;; Pre-commit support

  (defun dn-magit-run-precommit-manual ()
    "Run `pre-commit run --hook-stage manual' in the current repository."
    (interactive)
    (let ((default-directory (magit-toplevel)))
      (magit-start-process shell-file-name nil 
                           shell-command-switch 
                           "pre-commit run --hook-stage manual"))
    (magit-process-buffer))

  (transient-append-suffix 'magit-run "b"
    '("P" "Pre-commit manual" dn-magit-run-precommit-manual))

  ;; ------------------------------------------------------------------------ ;;
  ;; Mergiraf support

  (defun dn-magit-run-mergiraf-solve-file ()
    "Run `mergiraf solve' on the file at point or prompt for a file."
    (interactive)
    (let* ((default-directory (magit-toplevel))
           (file (or (magit-file-at-point)
                     (magit-read-file "Solve conflicts in file"))))
      (if file
          (let ((proc (magit-start-process shell-file-name nil
                                          shell-command-switch
                                          (format "mergiraf solve %s" (shell-quote-argument file)))))
            (message "Running mergiraf solve on %s..." file)
            (set-process-sentinel
             proc
             (lambda (process event)
               (when (eq (process-status process) 'exit)
                 (if (= (process-exit-status process) 0)
                     (progn
                       (message "Mergiraf solved conflicts in %s, staging file..." file)
                       (magit-run-git "add" file)
                       (magit-refresh))
                   (progn
                     (message "Mergiraf failed to solve conflicts in %s" file)
                     (magit-refresh))))))
            (magit-process-buffer))
        (user-error "No file selected"))))

  (defun dn-magit-run-mergiraf-solve-all ()
    "Run `mergiraf solve' on all conflicted files in the repository."
    (interactive)
    (let* ((default-directory (magit-toplevel))
           (conflicted-files (magit-git-lines "diff" "--name-only" "--diff-filter=U")))
      (if conflicted-files
          (let ((proc (magit-start-process shell-file-name nil
                                          shell-command-switch
                                          (format "for file in %s; do mergiraf solve \"$file\" || exit 1; done"
                                                  (mapconcat #'shell-quote-argument conflicted-files " ")))))
            (message "Running mergiraf solve on %d conflicted file(s)..."
                     (length conflicted-files))
            (set-process-sentinel
             proc
             (lambda (process event)
               (when (eq (process-status process) 'exit)
                 (if (= (process-exit-status process) 0)
                     (progn
                       (message "Mergiraf solved all conflicts, staging files...")
                       (apply #'magit-run-git "add" conflicted-files)
                       (magit-refresh))
                   (progn
                     (message "Mergiraf encountered errors solving some conflicts")
                     (magit-refresh))))))
            (magit-process-buffer))
        (message "No conflicted files found"))))

  (defun dn-magit-run-mergiraf-review (merge-id)
    "Review mergiraf's automatic conflict resolution with `mergiraf review'.
MERGE-ID is the merge identifier from git output."
    (interactive "sMerge ID: ")
    (let ((default-directory (magit-toplevel)))
      (if (string-empty-p merge-id)
          (user-error "Merge ID cannot be empty")
        (progn
          (message "Running mergiraf review on %s..." merge-id)
          (magit-start-process shell-file-name nil
                              shell-command-switch
                              (format "mergiraf review %s" (shell-quote-argument merge-id)))
          (magit-process-buffer)))))

  (transient-define-prefix dn-magit-run-mergiraf ()
    "Mergiraf commands for resolving merge conflicts."
    ["Mergiraf"
     ("f" "Solve file at point" dn-magit-run-mergiraf-solve-file)
     ("a" "Solve all conflicts" dn-magit-run-mergiraf-solve-all)
     ("r" "Review merge" dn-magit-run-mergiraf-review)])

  (transient-append-suffix 'magit-run "P"
    '("m" "Mergiraf" dn-magit-run-mergiraf))
  )

;;----------------------------------------------------------------------------;;
;; Git flow support

(use-package magit-gitflow
  :straight t
  :hook (magit-mode . turn-on-magit-gitflow)
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

(defvar dn-conv-commit-type-desc nil "Type of conventional commit")
(setq dn-conv-commit-type-desc
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

(defvar dn-conv-commit-scope-icons
  `(("nix" :icon ,#xf1511 :props (:foreground "#7ebae4" :height 1.2))
    ("cmake" :icon ,#xe794 :props (:foreground "#064F8C" :height 1.2)))
  "Icons and face properties for conventional commit scopes.
Icons use Nerd Font codepoints: nf-md-nix (U+F1511) and nf-dev-cmake (U+E794).")

(defun dn-conv-commit-add-faces (&rest _args)
  "Add face properties and compose symbols for buffer from dn-conv-commit-type-desc."
  (interactive)
  (with-silent-modifications
    (dolist (elt dn-conv-commit-type-desc nil)
      (let*
          (
           (type-data (cdr elt))
           (regex (format "\\<\\(%s\\)\\((\\([^)]+\\))\\)?[[:space:]]*\\(!\\)?[[:space:]]*:" (car elt)))
           (icon (plist-get type-data :icon))
           (face-props (plist-get type-data :props))
           )
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp regex nil t)
            (compose-region (match-beginning 1) (match-end 1) icon)
            (when face-props
              (add-face-text-property (match-beginning 1) (match-end 1) face-props))
            ;; Handle scope icons
            (when (match-beginning 3)
              (let* ((scope (match-string 3))
                     (scope-data (cdr (assoc scope dn-conv-commit-scope-icons))))
                (when scope-data
                  (compose-region (match-beginning 3) (match-end 3)
                                  (plist-get scope-data :icon))
                  (when (plist-get scope-data :props)
                    (add-face-text-property (match-beginning 3) (match-end 3)
                                            (plist-get scope-data :props))))))
            (when (match-beginning 4)
              (compose-region (match-beginning 4) (match-end 4) ?🚨)
              )
            )
          )
        )
      )
    )
  )

(advice-add 'magit-status :after 'dn-conv-commit-add-faces)
(advice-add 'magit-refresh-buffer :after 'dn-conv-commit-add-faces)


(defun dn-conv-commit-type-completion-decorate (type)
  "Decorate the completions candidates with icon prefix and description suffix.

TYPE is the type of conventional commit.
Return a list (candidate, icon, description)."

  (let ((type-data (cdr (assoc type dn-conv-commit-type-desc))))
    (list
     type
     (concat
      (propertize (string (plist-get type-data :icon))
                  'face (plist-get type-data :props))
      "   ")
     (concat
      (string-pad " " (- 10 (length type)))
      (propertize (plist-get type-data :desc) 'face '(:foreground "gray" ))))))


(defun dn-conv-commit-type-prompt ()
  (interactive)
  (let ((completion-extra-properties
         (list :affixation-function
               (lambda (types)
                 (mapcar #'dn-conv-commit-type-completion-decorate types)))))
    (completing-read "Commit type: " dn-conv-commit-type-desc)))
(defun dn-conv-commit-prompt ()
  "Prompt for a conventional commit. and fill the buffer with the result."
  (interactive)
  (insert (dn-conv-commit-type-prompt))
  (let ((scope (completing-read "Scope: " nil)))
    (insert (if (string= scope "") "" (format "(%s)" scope))))
  (insert (if (y-or-n-p "Breaking change? ") "!" ""))
  (insert ": ")
  )

(add-hook 'git-commit-setup-hook
          #'(lambda ()
              (run-with-timer 0.5 nil #'(lambda () (when (eq (pos-eol) (pos-bol)) (dn-conv-commit-prompt)))))
          )

;; ========================================================================== ;;
;; Mergiraf support for smerge-mode

(require 'smerge-mode)

;; ========================================================================== ;;

(defun dn-smerge-mergiraf-has-conflicts-p ()
  "Check if the current buffer contains merge conflict markers."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^<<<<<<< " nil t)))

(defun dn-smerge-mergiraf-solve ()
  "Run `mergiraf solve' on the current buffer to resolve merge conflicts.
After running mergiraf, the buffer is reverted and smerge-mode is re-enabled
if conflicts remain."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (unless (dn-smerge-mergiraf-has-conflicts-p)
    (user-error "No merge conflicts found in buffer"))

  (let ((filename (buffer-file-name)))
    ;; Save the buffer before running mergiraf
    (save-buffer)

    (message "Running mergiraf solve on %s..." (file-name-nondirectory filename))

    ;; Run mergiraf synchronously and capture output
    (let* ((output-buffer (generate-new-buffer "*mergiraf output*"))
           (exit-code (call-process "mergiraf" nil output-buffer nil
                                   "solve" filename)))
      (if (= exit-code 0)
          (progn
            ;; Success - revert buffer and check for remaining conflicts
            (revert-buffer t t t)
            (if (dn-smerge-mergiraf-has-conflicts-p)
                (progn
                  (smerge-mode 1)
                  (message "Mergiraf partially resolved conflicts. Manual resolution needed."))
              (progn
                (smerge-mode -1)
                (message "Mergiraf successfully resolved all conflicts!"))))
        (progn
          ;; Failed - show error output
          (message "Mergiraf failed to solve conflicts (exit code %d)" exit-code)
          (with-current-buffer output-buffer
            (goto-char (point-min))
            (when (> (buffer-size) 0)
              (message "Mergiraf output: %s" (buffer-string))))))
      (kill-buffer output-buffer))))

(defun dn-smerge-mergiraf-solve-and-save ()
  "Run `mergiraf solve' on the current buffer and save the result.
This is a convenience command that combines solving and saving."
  (interactive)
  (dn-smerge-mergiraf-solve)
  (when (buffer-modified-p)
    (save-buffer)))

;; ========================================================================== ;;
;; Add mergiraf command to smerge-mode keymap

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "C-c ^ m") 'dn-smerge-mergiraf-solve)
  (define-key smerge-mode-map (kbd "C-c ^ M") 'dn-smerge-mergiraf-solve-and-save))

;; ========================================================================== ;;

;; ========================================================================== ;;

(provide 'init-magit)

;;; init-magit.el ends here
