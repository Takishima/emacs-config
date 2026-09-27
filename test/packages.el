;;; packages.el --- Packages the configuration installs through :straight -*- lexical-binding: t -*-

;;; Commentary:

;; Two uses.  Alone, `emacs --batch -l test/packages.el DIR' prints the
;; name of every package a `use-package' form installs with `:straight',
;; sorted, one per line: the roster a Nix build must provide.  After the
;; configuration has loaded (`make packages'), every name whose form ran
;; must be loadable; the run exits non-zero otherwise.  Forms behind
;; `:if', `:when', `:unless' or `:disabled', and files the configuration
;; did not load, are only reported.

;;; Code:

(defvar dn-packages-extra '(diminish)
  "Packages init-package.el installs outside any `use-package' form.")

(defun dn-packages--name (name value)
  "Package installed by a `use-package' form for NAME with `:straight' VALUE."
  (cond ((eq value t) name)
        ((symbolp value) value)
        ((and (consp value) (symbolp (car value)) (not (keywordp (car value))))
         (car value))
        (t name)))

(defun dn-packages--gated-p (body)
  "Non-nil if the `use-package' BODY may skip its package."
  (seq-some (lambda (key) (memq key body)) '(:if :when :unless :disabled)))

(defun dn-packages--walk (form gated acc)
  "Add (NAME . GATED) for the `use-package' forms in FORM to ACC.
GATED is non-nil inside a form that may not run."
  (when (consp form)
    (if (and (eq (car form) 'use-package) (symbolp (cadr form)))
        (let* ((body (cddr form))
               ;; `memq', not `plist-get': a keyword taking several
               ;; values, such as `:after a b', would hide what follows.
               (cell (memq :straight body))
               (gated (or gated (and (dn-packages--gated-p body) t))))
          (when (and cell (cadr cell))
            (push (cons (dn-packages--name (cadr form) (cadr cell)) gated) acc))
          ;; Companions declared inside :init or :config.
          (while (consp body)
            (setq acc (dn-packages--walk (car body) gated acc))
            (setq body (cdr body))))
      (while (consp form)
        (setq acc (dn-packages--walk (car form) gated acc))
        (setq form (cdr form)))))
  acc)

(defun dn-packages--feature (file)
  "Feature the configuration provides for the module FILE."
  (let ((base (file-name-base file)))
    (intern (if (string-prefix-p "init-" base) base (concat "init-prog-" base)))))

(defun dn-packages-entries (root)
  "Entries (NAME GATED FEATURE) for the `:straight' packages under ROOT."
  (let* ((lisp (expand-file-name ".emacs_lisp" root))
         (files (append (directory-files lisp t "\\`init-.*\\.el\\'")
                        (directory-files (expand-file-name "programming" lisp)
                                         t "\\`[^#.].*\\.el\\'")))
         (acc (mapcar (lambda (name) (list name nil 'init-package))
                      dn-packages-extra)))
    (dolist (file files)
      (let ((feature (dn-packages--feature file))
            (pairs nil))
        (with-temp-buffer
          (insert-file-contents file)
          (condition-case nil
              (while t
                (setq pairs (dn-packages--walk (read (current-buffer)) nil pairs)))
            (end-of-file nil)))
        (dolist (pair pairs)
          (push (list (car pair) (cdr pair) feature) acc))))
    acc))

(defun dn-packages--names (entries)
  "Sorted, unique names of ENTRIES."
  (sort (delete-dups (mapcar (lambda (e) (symbol-name (car e))) entries))
        #'string<))

(if (featurep 'config-variables)
    (let ((missing nil)
          (skipped nil))
      (pcase-dolist (`(,name ,gated ,feature)
                     (dn-packages-entries
                      (file-name-directory (directory-file-name config-dotemacs-lisp))))
        (cond ((locate-library (symbol-name name)))
              ((or gated (not (featurep feature))) (push (list name) skipped))
              (t (push (list name) missing))))
      (dolist (name (dn-packages--names skipped))
        (message "SKIP %s (form did not run)" name))
      (dolist (name (dn-packages--names missing))
        (message "MISSING %s" name))
      (message "packages: %d missing, %d skipped"
               (length (dn-packages--names missing))
               (length (dn-packages--names skipped)))
      (kill-emacs (if missing 1 0)))
  (let ((root (or (pop command-line-args-left) default-directory)))
    (dolist (name (dn-packages--names (dn-packages-entries root)))
      (princ (concat name "\n")))))

;;; packages.el ends here
