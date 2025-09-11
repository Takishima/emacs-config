;;; dashboard-worktrees-patch.el --- Git worktrees support for emacs-dashboard

;; This file contains the uncommitted changes from emacs-dashboard
;; to add Git worktrees support. Include this in your .emacs file.

(eval-after-load 'dashboard
  '(progn
     ;; Add worktrees to icon associations
     (setq dashboard-heading-icons
           (let ((icons dashboard-heading-icons))
             (pcase dashboard-icon-type
               ('all-the-icons 
                (append icons '((worktrees . "git-branch"))))
               ('nerd-icons
                (append icons '((worktrees . "nf-oct-git-branch")))))))

     ;; Add worktrees to item functions
     (setq dashboard-item-generators
           (append dashboard-item-generators
                   '((worktrees . dashboard-insert-worktrees))))

     ;; Add worktrees to shortcuts
     (setq dashboard-item-shortcuts
           (append dashboard-item-shortcuts
                   '((worktrees . "w"))))


     ;; Worktree configuration variables
     (defvar dashboard-worktrees-path nil
       "Path to repository for displaying Git worktrees.
If nil, no worktrees will be displayed. Should be the path to the main
Git repository (not a worktree).")

     (defcustom dashboard-worktrees-show-base t
       "Show the worktree name in front of its path."
       :type '(choice
               (const :tag "Don't show the base in front" nil)
               (const :tag "Respect format" t)
               (const :tag "Align from base" align))
       :group 'dashboard)

     (defcustom dashboard-worktrees-item-format "%s  %s"
       "Format to use when showing the base of the worktree name."
       :type 'string
       :group 'dashboard)

     (defvar dashboard-worktrees-alist nil
       "Alist records the shortened worktree paths and their full paths.")

     (defvar dashboard--worktrees-cache-item-format nil
       "Cache to record the new generated align format.")

     ;; Worktree functions
     (defun dashboard-worktrees-get-list ()
       "Get list of Git worktrees from configured repository path."
       (when dashboard-worktrees-path
         (condition-case err
             (let ((default-directory (file-name-as-directory dashboard-worktrees-path)))
               (when (file-directory-p default-directory)
                 (let ((output (shell-command-to-string "git worktree list --porcelain")))
                   (dashboard-worktrees-parse-output output))))
           (error
            (message "Error getting worktrees: %s" (error-message-string err))
            nil))))

     (defun dashboard-worktrees-parse-output (output)
       "Parse git worktree list OUTPUT into a list of worktree paths."
       (let ((lines (split-string output "\n" t))
             (worktrees '())
             (current-path nil))
         (dolist (line lines)
           (cond
            ((string-prefix-p "worktree " line)
             (setq current-path (substring line 9)))
            ((string-prefix-p "branch " line)
             (when current-path
               (push current-path worktrees)
               (setq current-path nil)))))
         (reverse worktrees)))

     (defun dashboard-insert-worktrees (list-size)
       "Add the list of LIST-SIZE items of Git worktrees."
       (setq dashboard--worktrees-cache-item-format nil)
       (dashboard-insert-section
        "Worktrees:"
        (dashboard-shorten-paths
         (dashboard-subseq (dashboard-worktrees-get-list) list-size)
         'dashboard-worktrees-alist 'worktrees)
        list-size
        'worktrees
        (dashboard-get-shortcut 'worktrees)
        `(lambda (&rest _)
           (let ((path (dashboard-expand-path-alist ,el dashboard-worktrees-alist)))
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
        (let* ((file (dashboard-expand-path-alist el dashboard-worktrees-alist))
               (filename (dashboard-f-base file))
               (path (dashboard-extract-key-path-alist el dashboard-worktrees-alist)))
          (cl-case dashboard-worktrees-show-base
            (`align
             (unless dashboard--worktrees-cache-item-format
               (let* ((len-align (dashboard--align-length-by-type 'worktrees))
                      (new-fmt (dashboard--generate-align-format
                                dashboard-worktrees-item-format len-align)))
                 (setq dashboard--worktrees-cache-item-format new-fmt)))
             (format dashboard--worktrees-cache-item-format filename path))
            (`nil path)
            (t (format dashboard-worktrees-item-format filename path))))))

     ;; Patch dashboard functions to recognize worktrees section
     (defun dashboard--get-section-by-line (line)
       "Get section name by LINE."
       (cond
        ((string-match-p "Recent Files:" line)      'recents)
        ((string-match-p "Bookmarks:" line)         'bookmarks)
        ((string-match-p "Projects:" line)          'projects)
        ((string-match-p "Agenda for " line)        'agenda)
        ((string-match-p "Registers:" line)         'registers)
        ((string-match-p "Worktrees:" line)         'worktrees)
        ((string-match-p "List Directories:" line)  'ls-directories)
        ((string-match-p "List Files:" line)        'ls-files)
        (t (user-error "Unknown section from dashboard"))))

     (defun dashboard--section-list (section)
       "Return the list from SECTION."
       (cl-case section
         (`recents        recentf-list)
         (`bookmarks      (bookmark-all-names))
         (`projects       (dashboard-projects-backend-load-projects))
         (`worktrees      (dashboard-worktrees-get-list))
         (`ls-directories (dashboard-ls--dirs))
         (`ls-files       (dashboard-ls--files))
         (t (user-error "Unknown section for search: %s" section))))))

;; Configuration is handled in init-emacs.el via dn-dashboard-worktrees-path

(provide 'dashboard-worktrees-patch)
;;; dashboard-worktrees-patch.el ends here