;;; project-directory.el --- lets you invoke M-x compile from a deep subdirectory of the project directory  -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Darren Embry

;; Author: Darren Embry <dse@webonastick.com>
;; Keywords: tools, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add the following to your init file (usually ~/.emacs):
;;
;;     (eval-after-load 'compile '(require 'project-directory))
;;
;; or if you want:
;;
;;     (require 'project-directory)

;; NOTE --- This package will override any setting of the
;; `compilation-process-setup-function' variable during compilation.
;; This will probably be fixed soon as I figure out a way to do
;; compilation-process-setup hooks.

;;; Code:

(defun project-directory-find-file-upward (filename)
  "Looks for FILENAME in the current directory (as specified by
the `default-directory' variable) and then each parent directory
up to and including the root directory.

Returns the first instance of FILENAME found, or nil if no such
file is found.

This is primarily designed to allow you to invoke the `compile'
function from a deep subdirectory within a main project directory
containing, e.g., a Makefile."
  (let* ((dir      (expand-file-name default-directory))
	 (pathname (concat dir filename))
	 (parent   (file-name-directory (directory-file-name dir))))
    (cond
     ((file-readable-p pathname) pathname)
     ((string= parent dir) nil)
     (t (let ((default-directory parent))
	  (project-directory-find-file-upward filename))))))

(defun project-directory-find (filename)
  "Returns the project directory of the current directory (as
specified by the `default-directory' variable).

This is basically the directory containing the file whose name is
specified by the return value of the
`project-directory-find-file-upward' function.

This is primarily designed to allow you to invoke the `compile'
function from a deep subdirectory within a main project directory
containing, e.g., a Makefile."
  (let ((pathname (project-directory-find-file-upward filename)))
    (if pathname
	(file-name-directory pathname) nil)))

;; We can't use compilation-mode-hook to set the directory where the
;; compilation process executes, because currently the
;; `compilation-start' function calls it in a context with
;; `default-directory' as a dynamically scoped variable then starts
;; the compilation process outside of that context.  However, we can
;; fake it so the mode setter string (the -*- ... -*- stuff) contains
;; the new directory.

;; We also can't use compilation-start-hook to set the directory where
;; the compilation process executes, because by the time its functions
;; are executed, the process has already started.  :-(

(defun project-directory-build-command-match (build-command
					      command-string)
  "Retruns a true value if the COMMAND-STRING argument is
something that would eventually execute the BUILD-COMMAND
argument (see below on how the matching works), or nil otherwise.

Typically the COMMAND-STRING would be something like the value of
`compile-command'.

Typically the BUILD-COMMAND argument would be a command to invoke
a build system, such as \"make\" or \"ant\".

By \"something that would eventually execute BUILD-COMMAND\", we
mean anything such as the following (we use the example of
\"make\"):

    make -k
    ... <command> ; make <any arguments>
    ... <command> & make <any arguments>
    ... <command> && make <any arguments>
    ... <command> <newline> make <any arguments>

The whitespace before and after the ';' or '&' or '&&' (or
newline) is optional."
  (string-match
   (concat "^\\(.*?\\(\\;|\\&\\&?|\n\\)\\s-\\)?"
	   "\\([/A-Za-z0-9\\+\\,\\-\\.\\:\\=\\@\\_\\~]*\\)?"
	   command-string "\\b")
   command-string))

(defun pd--compilation-start-hook ()
  ;; And we shouldn't just straight up set the
  ;; `compilation-process-setup-function' because the user may already
  ;; have one set.  But until I fix this, this is literally the ONLY
  ;; way we can change a directory for the compilation process:
  (set (make-local-variable 'compilation-process-setup-function) 'pd--setup-function)
  (cond
   ((project-directory-build-command-match "make" compile-command)
    (let ((proj-dir (project-directory-find "Makefile")))
      (if proj-dir (cd proj-dir))))))

(add-hook 'compilation-mode-hook 'pd--compilation-start-hook)

(defun pd--setup-function ()
  (cond
   ((project-directory-build-command-match "make" compile-command)
    (let ((proj-dir (project-directory-find "Makefile")))
      (if proj-dir
	  (progn
	    (cd proj-dir)		;sets default-directory :-)
	    (with-current-buffer (compilation-find-buffer)
	      (save-excursion
		(let ((inhibit-read-only t))
		  (goto-char (point-max))
		  (insert "** project-directory has changed the default directory to:\n"
			  "** " default-directory "\n"))))))))))

(provide 'project-directory)

;;; project-directory.el ends here
