;;; dn-dashboard-worktrees.el --- Git worktrees section for emacs-dashboard -*- lexical-binding: t -*-

;;; Commentary:

;; Adds a `worktrees' item to `dashboard-items', listing the worktrees of
;; `dn-dashboard-worktrees-path'.  Call `dn-dashboard-worktrees-setup'
;; once dashboard is loaded.

;;; Code:

(require 'cl-lib)
(require 'dashboard)

(defcustom dn-dashboard-worktrees-path nil
  "Path to the main Git repository whose worktrees the dashboard lists.
If nil, no worktrees are listed."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Repository path"))
  :group 'dn)

(defcustom dn-dashboard-worktrees-show-base t
  "Show the worktree name in front of its path."
  :type '(choice
          (const :tag "Don't show the base in front" nil)
          (const :tag "Respect format" t)
          (const :tag "Align from base" align))
  :group 'dn)

(defcustom dn-dashboard-worktrees-item-format "%s  %s"
  "Format to use when showing the base of the worktree name."
  :type 'string
  :group 'dn)

(defvar dn-dashboard-worktrees-alist nil
  "Alist of shortened worktree paths and their full paths.")

(defvar dn-dashboard--worktrees-cache-item-format nil
  "Cache of the generated align format.")

(defun dn-dashboard-worktrees--parse (output)
  "Return the worktree paths with a branch in `git worktree list --porcelain' OUTPUT."
  (let ((worktrees '())
        (current-path nil))
    (dolist (line (split-string output "\n" t))
      (cond
       ((string-prefix-p "worktree " line)
        (setq current-path (substring line 9)))
       ((string-prefix-p "branch " line)
        (when current-path
          (push current-path worktrees)
          (setq current-path nil)))))
    (nreverse worktrees)))

(defun dn-dashboard-worktrees-list ()
  "Return the worktrees of `dn-dashboard-worktrees-path'."
  (when dn-dashboard-worktrees-path
    (condition-case err
        (let ((default-directory (file-name-as-directory dn-dashboard-worktrees-path)))
          (when (file-directory-p default-directory)
            (dn-dashboard-worktrees--parse
             (shell-command-to-string "git worktree list --porcelain"))))
      (error
       (message "Error getting worktrees: %s" (error-message-string err))
       nil))))

(defun dn-dashboard-insert-worktrees (list-size)
  "Insert up to LIST-SIZE Git worktrees into the dashboard."
  (setq dn-dashboard--worktrees-cache-item-format nil)
  (dashboard-insert-section
   "Worktrees:"
   (dashboard-shorten-paths
    (dashboard-subseq (dn-dashboard-worktrees-list) list-size)
    'dn-dashboard-worktrees-alist 'worktrees)
   list-size
   'worktrees
   (dashboard-get-shortcut 'worktrees)
   `(lambda (&rest _)
      (let ((path (dashboard-expand-path-alist ,el dn-dashboard-worktrees-alist)))
        (if (file-directory-p path)
            (let ((default-directory path))
              (if (and (fboundp 'consult-projectile-find-file)
                       (fboundp 'projectile-project-p)
                       (projectile-project-p))
                  (consult-projectile-find-file)
                (if (fboundp 'consult-find)
                    (consult-find)
                  (dired path))))
          (message "Worktree path not found: %s" path))))
   (let* ((file (dashboard-expand-path-alist el dn-dashboard-worktrees-alist))
          (filename (dashboard-f-base file))
          (path (dashboard-extract-key-path-alist el dn-dashboard-worktrees-alist)))
     (cl-case dn-dashboard-worktrees-show-base
       (`align
        (unless dn-dashboard--worktrees-cache-item-format
          (let* ((len-align (dashboard--align-length-by-type 'worktrees))
                 (new-fmt (dashboard--generate-align-format
                           dn-dashboard-worktrees-item-format len-align)))
            (setq dn-dashboard--worktrees-cache-item-format new-fmt)))
        (format dn-dashboard--worktrees-cache-item-format filename path))
       (`nil path)
       (t (format dn-dashboard-worktrees-item-format filename path))))))

(defun dn-dashboard-worktrees-setup ()
  "Register the `worktrees' item with dashboard."
  (add-to-list 'dashboard-item-generators '(worktrees . dn-dashboard-insert-worktrees) t)
  (add-to-list 'dashboard-item-shortcuts '(worktrees . "w") t)
  (pcase dashboard-icon-type
    ('all-the-icons
     (add-to-list 'dashboard-heading-icons '(worktrees . "git-branch") t))
    ('nerd-icons
     (add-to-list 'dashboard-heading-icons '(worktrees . "nf-oct-git-branch") t))))

(provide 'dn-dashboard-worktrees)

;;; dn-dashboard-worktrees.el ends here
