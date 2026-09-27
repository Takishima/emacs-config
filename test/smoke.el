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
(dolist (module dn-modules)
  (dn-smoke-check (format "module %s loaded" module) t (featurep module)))
(dolist (lang dn-disabled-languages)
  (dn-smoke-check (format "disabled language %s loaded" lang) nil
                  (featurep (intern (format "init-prog-%s" lang)))))

(let ((dir (expand-file-name "loader" dn-smoke-dir)))
  (make-directory dir)
  (with-temp-file (expand-file-name "foo.el" dir)
    (insert "(provide 'bar)\n"))
  (dn-smoke-check "dn-load-directory rejects a mismatched provide" t
                  (condition-case nil
                      (progn (dn-load-directory dir "dn-smoke-") nil)
                    (error t))))

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
                 "C-x tk" dn-python-pytest-close-buffer)
                ("t.cpp" c++-ts-mode ("C++" "C") "C-c c" recompile)
                ("CMakeLists.txt" cmake-ts-mode ("CMake")
                 "C-c C-f" cmake-format-buffer)))
  (pcase-let ((`(,file ,mode ,docsets ,key ,command) spec))
    (with-current-buffer (dn-smoke-visit file)
      (dn-smoke-check (concat file " major-mode") mode major-mode)
      (dn-smoke-check (concat file " dash-docs-docsets") docsets
                      (bound-and-true-p dash-docs-docsets))
      (dn-smoke-check (concat file " " key) command (key-binding (kbd key))))))

(require 'yasnippet)
(with-current-buffer (dn-smoke-visit "CMakeLists.txt")
  (dn-smoke-check "own cmake snippets loaded" t
                  (and (member "damien-mit"
                               (mapcar #'yas--template-name
                                       (yas--all-templates (yas--get-snippet-tables))))
                       t)))

(with-current-buffer (dn-smoke-visit "t.tex")
  (dn-smoke-check "t.tex major-mode" 'LaTeX-mode major-mode)
  (dn-smoke-check "t.tex latexmk command" t
                  (and (assoc "latexmk" TeX-command-list) t))
  (dn-smoke-check "TeX-run-Biber is AUCTeX's compiled definition" t
                  (compiled-function-p (symbol-function 'TeX-run-Biber))))

(dn-smoke-check "use-package warnings" nil
                (when-let* ((buf (get-buffer "*Warnings*")))
                  (with-current-buffer buf
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward "(use-package)" nil t)
                        (buffer-substring (pos-bol) (pos-eol)))))))

(delete-directory dn-smoke-dir t)
(message "smoke: %d failure(s)" dn-smoke-failures)
(kill-emacs (if (zerop dn-smoke-failures) 0 1))

;;; smoke.el ends here
