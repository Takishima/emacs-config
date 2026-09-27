;;; compile.el --- Byte-compile the configuration's own files -*- lexical-binding: t -*-

;;; Commentary:

;; Run with `make compile', after early-init.el and init.el have loaded the
;; configuration.  Every own Emacs Lisp file is byte-compiled into a temporary
;; directory; a file that fails to compile fails the run.  Warnings are
;; printed but do not fail it.

;;; Code:

(require 'bytecomp)

(defvar dn-compile-dirs
  (list config-dir
        config-dotemacs-lisp
        (expand-file-name "programming" config-dotemacs-lisp)
        (expand-file-name "lib" config-dotemacs-lisp)
        (expand-file-name "yas-lib" config-dotemacs-lisp))
  "Directories whose *.el files are byte-compiled.")

(defvar dn-compile-out (make-temp-file "dn-compile-" t)
  "Directory receiving the .elc files, deleted afterwards.")

(setq byte-compile-dest-file-function
      (lambda (file)
        (expand-file-name (concat (file-name-nondirectory file) "c")
                          dn-compile-out)))

(let ((failures 0))
  (dolist (dir dn-compile-dirs)
    (dolist (file (directory-files dir t "\\`[^#.].*\\.el\\'"))
      (unless (byte-compile-file file)
        (setq failures (1+ failures))
        (message "FAIL %s" file))))
  (delete-directory dn-compile-out t)
  (message "compile: %d failure(s)" failures)
  (kill-emacs (if (zerop failures) 0 1)))

;;; compile.el ends here
