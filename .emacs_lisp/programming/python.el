;;; init-prog-python.el --- Initialisation for Python -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Homepage: nil
;; Keywords: init, programming

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
(require 'config-functions (concat config-dir "functions.el"))

;; ========================================================================== ;;

(use-package highlight-indentation
  :straight t)

;; ========================================================================== ;;

(use-package python
  :straight nil
  :config
  (progn
    (add-hook 'python-mode-hook 'highlight-indentation-mode)
    ;; (add-hook 'python-mode-hook 'eldoc-mode)
    (add-hook 'python-mode-hook 'sphinx-doc-mode))
  :bind (:map python-mode-map
	      (
               ("C-c i" . python-insert-docstring-with-google-style-at-point)
               ))
  :config
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (when (executable-find "ipython")
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
  )

;; -------------------------------------------------------------------------- ;;

(use-package python-insert-docstring
  :straight t)

;; -------------------------------------------------------------------------- ;;

(use-package blacken
  :straight t)

(use-package python-black
  :straight t)

;; -------------------------------------------------------------------------- ;;

(use-package python-isort
  :straight t)

(use-package ruff-format
  :straight t)

(use-package lsp-pylyzer
  :straight (:host github :repo "emacs-lsp/lsp-pylyzer" :files ("*.el"))
  :straight t
  ;; :hook (python-mode . (lambda ()
  ;;                         (require 'lsp-pylyzer)
  ;;                         (lsp-deffered)))
  )

;; -------------------------------------------------------------------------- ;;

;; (use-package elpy
;;   :straight t
;;   :config
;;   (elpy-enable)
;;   :custom
;;   (elpy-eldoc-show-current-function nil)
;;   (elpy-rpc-timeout 5)
;;   (elpy-rpc-virtualenv-path 'default)
;;   :bind (:map elpy-mode-map
;; 	      ("<M-left>" . nil)
;; 	      ("<M-right>" . nil)
;;               ("C-c C-r f" . elpy-format-code)
;; 	      ("C-c C-k" . elpy-custom-close-all)
;; 	      ("C-c k" . python-pytest-close-buffer)
;; 	      ("C-b C-d" . c-hungry-delete-forward))
;;   :config
;;   (defun elpy-test-pytest-runner (top file module test)
;;     "Test the project using the py.test test runner.

;; This requires the pytest package to be installed."
;;     (interactive (elpy-test-at-point))
;;     (cond
;;      (test
;;       (let ((test-list (split-string test "\\.")))
;; 	(python-pytest-function file test python-pytest-arguments)))
;;      (module
;;       (python-pytest-file file python-pytest-arguments))
;;      (t
;;       (python-pytest top python-pytest-arguments))))

;;   (defun elpy-custom-close-all ()
;;     "Close all active python shells and python-pytest buffers"
;;     (interactive)
;;     (progn
;;       (python-pytest-close-buffer)
;;       (elpy-shell-kill-all)
;;       )
;;     )
;;   )

;; -------------------------------------------------------------------------- ;;

(use-package lsp-pyright
  :straight t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (config-unless-system 'darwin
                           (require 'lsp-pyright)
                           (lsp-deferred))))
  :custom
  (lsp-pyright-auto-import-completions nil)
  (lsp-pyright-typechecking-mode "off")
  (lsp-pyright-python-executable-cmd "python3")
  :config
  (dn-async-process
   "npm outdated -g | grep pyright | wc -l" nil
   (lambda (process output)
     (pcase output
       ("0\n" (message "Pyright is up to date."))
       ("1\n" (message "A pyright update is available.")))))
  )

;; -------------------------------------------------------------------------- ;;

(use-package pyvenv
  :straight t
  :after python
  :config
  (defun dn-activate-pyvenv ()
    "Activate python environment according to the `.venv' file."
    (interactive)
    (pyvenv-mode)
    (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
      (if (file-exists-p pfile)
          (pyvenv-workon (with-temp-buffer
                           (insert-file-contents pfile)
                           (nth 0 (split-string (buffer-string))))))))
  )

;; -------------------------------------------------------------------------- ;;

(use-package cython-mode
  :straight t
  :mode (("\\.pyx\\'"  . cython-mode)
         ("\\.spyx\\'" . cython-mode)
         ("\\.pxd\\'"  . cython-mode)
         ("\\.pxi\\'"  . cython-mode)))

;; ========================================================================== ;;

(use-package yapfify
  :straight t
  :bind
  (:map python-mode-map
        ("C-c C-y" . yapfify-region-or-buffer))
  )

;; ========================================================================== ;;

(use-package python-coverage
  :straight t
  :after python)

;; ========================================================================== ;;

(use-package python-pytest
  :straight t
  :after python
  :bind (:map python-mode-map
	      (("C-x tp" . python-pytest-dispatch)
	       ("C-x tt" . python-pytest)
	       ("C-x tf" . python-pytest-file)
	       ("C-x tF" . python-pytest-file-dwim)
	       ("C-x tm" . python-pytest-function)
	       ("C-x tM" . python-pytest-function-dwim)
	       ("C-x tr" . python-pytest-repeat)
	       ("C-x tl" . python-pytest-last-failed)
	       ("C-x tk" . python-pytest-close-buffer)
	       ))
  :custom
  (python-pytest-arguments
   '("--color"          ;; colored output in the buffer
     "--failed-first"   ;; run the previous failed tests first
     "-p no:warnings"   ;; ignore warnings
     "--maxfail=5"))    ;; exit in 5 continuous failures in a run
  :functions python-pytest--project-name
  :config
  (require 'magit-popup)
  (which-key-add-major-mode-key-based-replacements 'python-mode "t" "Testing")
  (defun python-pytest-cov--choose-report-type (prompt _value)
    "Helper to choose a pytest coverage report type using PROMPT."
    (completing-read
     prompt '("term" "term-missing" "annotate" "html") nil t))
  (magit-define-popup-option 'python-pytest-dispatch ?c "Coverage" "--cov=")
  (magit-define-popup-option
    'python-pytest-dispatch
    ?r
    "Coverage report" "--cov-report="
    'python-pytest-cov--choose-report-type)
  (defun python-pytest-close-buffer ()
    "Close the python-pytest buffer (if it exists)."
    (interactive)
    (let ((name python-pytest-buffer-name)
	  (window nil))
      (when python-pytest-project-name-in-buffer-name
	(setq name (format "%s<%s>" name (python-pytest--project-name))))
      (setq window (get-buffer-window name))
      (when window
	(quit-window t window)
	)
      )
    )
  )

;; ========================================================================== ;;

(use-package sphinx-mode
  :straight t
  :bind
  ;; (:map sphinx-mode-map
  ;;       (("C-c i" . compile-in-iterm)
  ;;        ))
  )

;; -------------------------------------------------------------------------- ;;

(use-package sphinx-doc
  :straight t)

;; ========================================================================== ;;

(provide 'init-prog-build-systems)

;;; init-prog-build-systems.el ends here
