;;; smoke.el --- Batch smoke test for this configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Run with `make check'.  Exits non-zero if any check fails.

;;; Code:

(defvar dn-smoke-failures 0)

(defvar dn-smoke-dir (make-temp-file "dn-smoke-" t))

(defun dn-smoke-check (label expected actual)
  "Report a failure for LABEL unless EXPECTED equals ACTUAL."
  (unless (equal expected actual)
    (setq dn-smoke-failures (1+ dn-smoke-failures))
    (message "FAIL %s: expected %S, got %S" label expected actual)))

(defun dn-smoke-visit (name)
  "Visit a new file NAME in `dn-smoke-dir' and return its buffer."
  (find-file-noselect (expand-file-name name dn-smoke-dir)))

;; Keep language servers from starting in batch.
(advice-add 'lsp-deferred :override #'ignore)

(dn-smoke-check "config-require leaks filename" nil (boundp 'filename))
(dn-smoke-check "ispell-extra-args overridden" nil
                (equal (default-value 'ispell-extra-args) '("--reverse")))
(dn-smoke-check "indent-tabs-mode" nil (default-value 'indent-tabs-mode))

(let* ((dir (expand-file-name "programming" config-dotemacs-lisp))
       (skip-file (expand-file-name "skip.txt" dir))
       (skip (when (file-exists-p skip-file)
               (seq-remove (lambda (line)
                             (or (string-prefix-p "#" line)
                                 (string-prefix-p ";" line)))
                           (with-temp-buffer
                             (insert-file-contents skip-file)
                             (split-string (buffer-string) "[\n\r]" t "[ \t]+"))))))
  (dolist (file (directory-files dir nil "^[^#.].*\\.el\\'"))
    (unless (member file skip)
      (let ((feature (concat "init-prog-" (file-name-base file))))
        (dn-smoke-check (concat file " provides " feature) t
                        (featurep (intern feature)))))))

(require 'magit)
(dn-smoke-check "magit split-height-threshold" 200
                (with-temp-buffer (magit-mode) split-height-threshold))

(dn-smoke-check "conv-commit-type-prompt affixation" t
                (cl-letf (((symbol-function 'completing-read)
                           (lambda (_prompt collection &rest _)
                             (funcall (plist-get completion-extra-properties
                                                 :affixation-function)
                                      (list (caar collection))))))
                  (condition-case err
                      (let ((triples (conv-commit-type-prompt)))
                        (and (consp triples)
                             (seq-every-p (lambda (x) (= (length x) 3)) triples)))
                    (error err))))

(with-current-buffer (dn-smoke-visit "PKGBUILD")
  (dn-smoke-check "PKGBUILD local srcinfo hook" t
                  (and (memq 'pkgbuild-update-srcinfo before-save-hook) t))
  (dn-smoke-check "global pkgbuild save hook" nil
                  (seq-some (lambda (f)
                              (and (symbolp f)
                                   (string-prefix-p "pkgbuild" (symbol-name f))))
                            (default-value 'before-save-hook))))

(dolist (spec '(("t.py" python-ts-mode ("Python 3" "NumPy" "SciPy")
                 "C-x tk" python-pytest-close-buffer)
                ("t.cpp" c++-ts-mode ("C++" "C") "C-c c" recompile)
                ("CMakeLists.txt" cmake-ts-mode ("CMake")
                 "C-c C-f" cmake-format-buffer)))
  (pcase-let ((`(,file ,mode ,docsets ,key ,command) spec))
    (with-current-buffer (dn-smoke-visit file)
      (dn-smoke-check (concat file " major-mode") mode major-mode)
      (dn-smoke-check (concat file " dash-docs-docsets") docsets
                      (bound-and-true-p dash-docs-docsets))
      (dn-smoke-check (concat file " " key) command (key-binding (kbd key))))))

(with-current-buffer (dn-smoke-visit "t.tex")
  (dn-smoke-check "t.tex major-mode" 'LaTeX-mode major-mode)
  (dn-smoke-check "t.tex latexmk command" t
                  (and (assoc "latexmk" TeX-command-list) t)))

(delete-directory dn-smoke-dir t)
(message "smoke: %d failure(s)" dn-smoke-failures)
(kill-emacs (if (zerop dn-smoke-failures) 0 1))

;;; smoke.el ends here
