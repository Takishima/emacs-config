;;; init-auctex.el --- Initialisation for AUCTeX -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
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

;; Not fully tested!

;;; Code:

(require 'config-functions (concat config-dir "functions.el"))
(require 'use-package)

(require 'cl-lib)

(use-package tex
  :straight auctex
  :defer t
  :defines (latex-help-cmd-alist latex-help-file)
  :functions (TeX-run-command TeX-synchronous-sentinel Info-goto-node)
  :custom
  (TeX-auto-local "tex-tmp")
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-PDF-mode t)

  (LaTeX-clean-intermediate-suffixes '("\\.aux" "\\.bbl" "\\.blg" "\\.brf" "\\.fot" "\\.glo" "\\.gls" "\\.idx" "\\.ilg" "\\.ind" "\\.lof" "\\.log" "\\.lot" "\\.nav" "\\.out" "\\.snm" "\\.toc" "\\.url" "\\.synctex\\.gz" "\\.bcf" "\\.run\\.xml" "\\.fls" "-blx\\.bib" "\\.acn" "\\.acr" "\\.alg" "\\.glg" "\\.xdv" "\\.fdb_latexmk" "\\.ist"))

  (reftex-plug-into-AUCTeX t)

  (reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

  (reftex-use-multiple-selection-buffers t)

  (font-latex-match-reference-keywords
        '(("cite" "[{")
          ("cites" "[{")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{")
          ("citetitle" "[{")
          ("citetitles" "[{")
          ("headlessfullcite" "[{")))

  (font-latex-user-keyword-classes
        '(
          ("todo-command" (("todo" "{")) (:foreground "black" :background "orange") command)
          )
        )

  :config

  (add-to-list 'TeX-command-list
               '("latexmk" "latexmk -xelatex -pv -shell-escape %s" TeX-run-TeX nil t :help "Process file with latexmk"))
  (add-to-list 'TeX-command-list
               '("xelatexmk" "latexmk -shell-escape -pv -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk"))
  (add-to-list 'TeX-command-list
               '("make" "make %s" TeX-run-TeX nil t :help "Process file with GNU make (and makefile)"))
  (add-to-list 'TeX-command-list
               '("Biber" "biber %s" dn-tex-run-biber nil t :help "Run Biber"))

  ;; Replaces the upstream definition of this function with a corrected one.
  (defun latex-help-get-cmd-alist ()
    "Scoop up the commands in the index of the latex info manual.
The values are saved in `latex-help-cmd-alist' for speed."
    (if (not (assoc "\\begin" latex-help-cmd-alist))
        (save-window-excursion
          (setq latex-help-cmd-alist nil)
          (Info-goto-node (concat latex-help-file "Command Index"))
          (goto-char (point-max))
          (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
            (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                  (value (buffer-substring (match-beginning 2)
                                           (match-end 2))))
              (add-to-list 'latex-help-cmd-alist (cons key value))))))
    latex-help-cmd-alist)

  (add-hook 'TeX-mode-hook 'turn-on-orgtbl)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (defun dn-tex-run-biber (name command file)
    "Create a process for NAME using COMMAND to format FILE with Biber."
    (let ((process (TeX-run-command name command file)))
      (setq TeX-sentinel-function 'dn-tex-biber-sentinel)
      (if TeX-process-asynchronous
          process
        (TeX-synchronous-sentinel name file process))))

  (defun dn-tex-biber-sentinel (_process _name)
    "Report the warnings and errors Biber wrote to the TeX output buffer.
_PROCESS and _NAME are the ignored arguments of a `TeX-sentinel-function'."
    (goto-char (point-max))
    (cond
     ((re-search-backward (concat
                           "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                           "\\(warnings?\\|error messages?\\))") nil t)
      (message (concat "Biber finished with %s %s. "
                       "Type `%s' to display output.")
               (match-string 1) (match-string 2)
               (substitute-command-keys
                "\\\\[TeX-recenter-output-buffer]")))
     (t
      (message (concat "Biber finished successfully. "
                       "Run LaTeX again to get citations right."))))
    (setq TeX-command-next TeX-command-default))

  )

(use-package ris
  :straight nil
  :mode ("\\.ris\\'")
  )

(use-package lsp-ltex
  :straight t
  :after (lsp-mode)
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp-deferred)))
  :custom
  (lsp-ltex-version "15.2.0")
  )

(provide 'init-auctex)

;;; init-auctex.el ends here
