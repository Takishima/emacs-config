;;; cleanup-lsp-workspaces.el --- Clean up LSP workspace folders -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ((lsp-mode "6.0"))
;; Homepage: nil
;; Keywords: lsp, workspace, cleanup


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

;; This package provides utilities to clean up lsp-mode workspace folders.
;; It helps remove problematic folders that can cause LSP servers to crash,
;; such as remote paths, temporary directories, and non-existent folders.
;;
;; Main functions:
;; - `dn-lsp-cleanup-workspaces': Remove problematic folders (recommended)
;; - `dn-lsp-cleanup-workspaces-nonexistent': Remove only non-existent directories
;; - `dn-lsp-list-workspaces': List all workspace folders
;; - `dn-lsp-cleanup-workspaces-keep-home-only': Keep only $HOME folders
;; - `dn-lsp-cleanup-workspaces-remove-all': Remove all workspace folders

;;; Code:

(require 'lsp-mode)

;; ========================================================================== ;;

(defun dn-lsp-cleanup-workspaces-nonexistent ()
  "Remove only non-existent workspace folders.
This is safe to run automatically on startup."
  (interactive)
  (let* ((session (lsp-session))
         (folders (lsp-session-folders session))
         (kept-folders '())
         (removed-count 0))

    (cl-labels ((folder-exists-p (folder)
                  (and (not (or (string-prefix-p "/ssh:" folder)
                               (file-remote-p folder)))  ; Skip remote without connecting
                       (file-directory-p folder))))

      (dolist (folder folders)
        (if (folder-exists-p folder)
            (push folder kept-folders)
          (progn
            (message "Removing non-existent: %s" folder)
            (setq removed-count (1+ removed-count)))))

      (setf (lsp-session-folders session) (reverse kept-folders))

      (let ((server-folders (lsp-session-server-id->folders session)))
        (maphash
         (lambda (server-id folders-list)
           (let ((server-kept-folders '()))
             (dolist (folder folders-list)
               (if (folder-exists-p folder)
                   (push folder server-kept-folders)
                 (progn
                   (message "Removing non-existent from %s: %s" server-id folder)
                   (setq removed-count (1+ removed-count)))))
             (puthash server-id (reverse server-kept-folders) server-folders)))
         server-folders))

      (when (> removed-count 0)
        (lsp--persist-session session)
        (message "LSP workspace cleanup: Removed %d non-existent folders." removed-count)))))

;; ========================================================================== ;;

(defun dn-lsp-cleanup-workspaces ()
  "Remove remote, missing, /tmp and /nix/store folders from LSP workspaces.
This is safe and won't trigger Tramp connections."
  (interactive)
  (let* ((session (lsp-session))
         (folders (lsp-session-folders session))
         (kept-folders '())
         (removed-count 0))

    (cl-labels ((should-keep-folder (folder)
                  (cond
                   ((or (string-prefix-p "/ssh:" folder)
                        (file-remote-p folder))
                    nil)
                   ((not (file-directory-p folder))
                    nil)
                   ((string-prefix-p "/tmp/" folder)
                    nil)
                   ((string-prefix-p "/nix/store/" folder)
                    nil)
                   (t t))))

      (dolist (folder folders)
        (if (should-keep-folder folder)
            (push folder kept-folders)
          (progn
            (message "Removing: %s" folder)
            (setq removed-count (1+ removed-count)))))

      (setf (lsp-session-folders session) (reverse kept-folders))

      (let ((server-folders (lsp-session-server-id->folders session)))
        (maphash
         (lambda (server-id folders-list)
           (let ((server-kept-folders '()))
             (dolist (folder folders-list)
               (if (should-keep-folder folder)
                   (push folder server-kept-folders)
                 (progn
                   (message "Removing from %s: %s" server-id folder)
                   (setq removed-count (1+ removed-count)))))
             (puthash server-id (reverse server-kept-folders) server-folders)))
         server-folders))

      (lsp--persist-session session)

      (message "Cleanup complete! Removed %d folders, %d remaining."
               removed-count (length kept-folders)))))

;; ========================================================================== ;;

(defun dn-lsp-list-workspaces ()
  "List all LSP workspace folders (safe - no Tramp)."
  (interactive)
  (let* ((session (lsp-session))
         (folders (lsp-session-folders session)))
    (with-output-to-temp-buffer "*LSP Workspaces*"
      (princ (format "Total: %d workspace folders\n\n" (length folders)))
      (dolist (folder (sort folders #'string<))
        (let ((icon (cond
                     ((or (string-prefix-p "/ssh:" folder)
                          (file-remote-p folder)) "🌐")
                     ((string-prefix-p "/tmp/" folder) "🗑")
                     ((string-prefix-p "/nix/store/" folder) "📦")
                     ((file-directory-p folder) "✓")
                     (t "✗"))))
          (princ (format "%s %s\n" icon folder)))))))

;; ========================================================================== ;;

(defun dn-lsp-cleanup-workspaces-keep-home-only ()
  "Keep only workspace folders under your home directory."
  (interactive)
  (when (yes-or-no-p "This will remove ALL workspaces outside $HOME. Continue? ")
    (let* ((session (lsp-session))
           (folders (lsp-session-folders session))
           (home (expand-file-name "~"))
           (kept-folders '())
           (removed-count 0))

      (cl-labels ((should-keep-folder (folder)
                    (and (not (file-remote-p folder))
                         (string-prefix-p home folder))))

        (dolist (folder folders)
          (if (should-keep-folder folder)
              (push folder kept-folders)
            (setq removed-count (1+ removed-count))))

        (setf (lsp-session-folders session) (reverse kept-folders))

        (let ((server-folders (lsp-session-server-id->folders session)))
          (maphash
           (lambda (server-id folders-list)
             (let ((server-kept-folders '()))
               (dolist (folder folders-list)
                 (if (should-keep-folder folder)
                     (push folder server-kept-folders)
                   (setq removed-count (1+ removed-count))))
               (puthash server-id (reverse server-kept-folders) server-folders)))
           server-folders))

        (lsp--persist-session session)

        (message "Kept only $HOME folders. Removed %d, %d remaining."
                 removed-count (length kept-folders))))))

;; ========================================================================== ;;

(defun dn-lsp-cleanup-workspaces-remove-all ()
  "Remove ALL workspace folders from LSP session.
Use this to start fresh with a clean workspace list."
  (interactive)
  (when (yes-or-no-p "This will remove ALL workspace folders. Continue? ")
    (let* ((session (lsp-session))
           (folders (lsp-session-folders session))
           (removed-count (length folders)))

      (setf (lsp-session-folders session) '())

      (let ((server-folders (lsp-session-server-id->folders session)))
        (maphash
         (lambda (server-id folders-list)
           (setq removed-count (+ removed-count (length folders-list)))
           (puthash server-id '() server-folders))
         server-folders))

      (lsp--persist-session session)

      (message "Removed all %d workspace folders." removed-count))))

;; ========================================================================== ;;

(provide 'cleanup-lsp-workspaces)

;;; cleanup-lsp-workspaces.el ends here
