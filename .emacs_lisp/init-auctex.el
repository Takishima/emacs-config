;;; init-auctex.el --- Initialisation for AUCTeX -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Homepage: nil
;; Keywords: init


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

;; Not fully tested!

;;; Code:

;; ========================================================================== ;;

(require 'config-functions (concat config-dir "functions.el"))
(require 'use-package)

;; ========================================================================== ;;

(require 'cl-lib)

(use-package tex
  :ensure nil
  )

(use-package tex-site                   ; auctex
  :ensure nil
  :defines (latex-help-cmd-alist latex-help-file)
  ;; :functions (TeX-run-Biber)
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :custom
  (TeX-parse-self t) ; Enable parse on load.
  (TeX-auto-save t) ; Enable parse on save.
  (TeX-PDF-mode t) ; Default to PDFTeX

  (TeX-view-program-selection (append '(output-pdf "PDF Viewer")
				      (remove-if (lambda(el) (eq (car el) 'output-pdf))
						 TeX-view-program-selection)))
  
  (TeX-view-program-list (push '("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
			       TeX-view-program-list))
  
  ;; -----------------------------------
  ;; RefTeX related settings
  
  (reftex-plug-into-AUCTeX t) ; Turn on RefTeX interface to AUCTeX

    ;; Recognize \addbibresource as bibliography command
  (reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

  ;; Make RefTeX faster
  ;;(setq reftex-enable-partial-scans t) ;; parse only current file in multifile doc
  ;;(setq reftex-save-parse-info t) ;; save parse info into file.rel
  (reftex-use-multiple-selection-buffers t)
  (reftex-plug-into-AUCTeX t)
  
  ;; -----------------------------------
  ;; Additional keywords
  
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

  ;; -----------------------------------
  ;; TeX compile commands

  ;; (setq TeX-view-program-selection
  ;; 	(remove-if (lambda(el) (eq (car el) 'output-pdf))
  ;; 		   TeX-view-program-selection))
  ;; (add-to-list 'TeX-view-program-selection
  ;; 	       '(output-pdf "PDF Viewer"))

  (add-to-list 'TeX-view-program-list
	       '("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
  (add-to-list 'TeX-command-list 
	       '("latexmk" "latexmk -xelatex -pv -shell-escape %s" TeX-run-TeX nil t :help "Process file with latexmk"))
  (add-to-list 'TeX-command-list
	       '("xelatexmk" "latexmk -shell-escape -pv -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk"))
  (add-to-list 'TeX-command-list
	       '("make" "make %s" TeX-run-TeX nil t :help "Process file with GNU make (and makefile)"))
  (add-to-list 'TeX-command-list
	       '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))

  ;; -----------------------------------
  ;; Setup PATH

  (config-with-system darwin
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
    (add-to-list 'exec-path "/Library/TeX/texbin" t)
    )
  
  (defun latex-help-get-cmd-alist ()    ;corrected version:
    "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
    ;; mm, does it contain any cached entries
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


  (defun TeX-run-Biber (name command file)
    "Create a process for NAME using COMMAND to format FILE with Biber." 
    (let ((process (TeX-run-command name command file)))
      (setq TeX-sentinel-function 'TeX-Biber-sentinel)
      (if TeX-process-asynchronous
	  process
	(TeX-synchronous-sentinel name file process))))

  (defun TeX-Biber-sentinel (process name)
    "Cleanup TeX output buffer after running Biber."
    (goto-char (point-max))
    (cond
     ;; Check whether Biber reports any warnings or errors.
     ((re-search-backward (concat
			   "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
			   "\\(warnings?\\|error messages?\\))") nil t)
      ;; Tell the user their number so that she sees whether the
      ;; situation is getting better or worse.
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

;; -------------------------------------------------------------------------- ;;

(use-package ris
  :ensure nil
  :load-path config-dotemacs-lisp
  :mode ("\\.ris\\'")
  )

;; ========================================================================== ;;

(provide 'init-auctex)

;;; init-auctex.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End: