;;; init-programming.el --- Initialisation for programming -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
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

;;; Code:

;; ========================================================================== ;;

(require 'use-package)
(require 'config-functions (concat config-dir "functions.el"))

;; ========================================================================== ;;
;; Automatically guess indent offsets, tab, spaces settings, etc.

(use-package dtrt-indent
  :ensure t)

;; -------------------------------------------------------------------------- ;;

(use-package project-directory
  :ensure nil
  :load-path config-dotemacs-lisp)

;; ========================================================================== ;;
;; Compilation

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      ;; (switch-to-prev-buffer (get-buffer-window buf) 'bury)
                      (when (get-buffer-window buf)
                        (delete-window (get-buffer-window buf))
                        )
                      (kill-buffer buf)
                      )
                    buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; -------------------------------------------------------------------------- ;;

(setq compilation-scroll-output 'first-error)

;; -------------------------------------------------------------------------- ;;

(config-with-system darwin
  (defvar compile-in-iterms-command "make")
  (config-with-system darwin
    (defun compile-in-iterm (command)
      (interactive
       (list (read-string (format "Say word [%s]: " compile-in-iterms-command) nil nil compile-in-iterms-command)))
      (progn
	(setq compile-in-iterms-command command)
	(do-applescript
	 (concat "tell application \"iTerm\"\ntell current session of current window\nwrite text \""
		 command
		 "\"\nend tell\nend tell")
	 )
	))
    )
  )

;; ========================================================================== ;;

(use-package editorconfig
  :ensure t
  )

(use-package editorconfig-generate
  :ensure t
  )

(use-package editorconfig-domain-specific
  :ensure t
  )

(use-package editorconfig-custom-majormode
  :ensure t)

;; ========================================================================== ;;

(let* (
       (dir-path (file-name-as-directory (concat config-dotemacs-lisp "programming")))
       (skip-file (concat dir-path "skip.txt"))
       (skip-names (list))
       (require-name)
       (dir-list (directory-files dir-path t "^[^#\\.].*\\.el$"))
       )
  (if (file-exists-p skip-file)
      (progn
	(setq skip-names (with-temp-buffer (insert-file-contents (concat dir-path "skip.txt"))
					 (split-string (buffer-string) "[\n\r]" t "[ \t]+")))
	(message "INFO: will be skipping the following: %S" skip-names)
	)
    )
  (dolist (fname dir-list)
    (setq require-name (intern-soft (concat "init-prog-"
					    (file-name-sans-extension (file-name-nondirectory fname)))))
    (unless (member (file-name-nondirectory fname) skip-names)
      (if require-name
	  (progn
	    (message "INFO: requiring %s from %s" require-name fname)
	    (require require-name fname)
	    )
	(progn
	  (setq fname (file-name-sans-extension fname))
	  (load fname)
	  )
	)
      )
    )
  )

;; ========================================================================== ;;

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package flycheck-clang-tidy
  :ensure t
  :after flycheck
  :config (flycheck-clang-tidy-setup))


;; ========================================================================== ;;

(provide 'init-programming)

;;; init-programming.el ends here

;; Local Variables:
;; eval: (setq config-dotemacs-lisp (file-name-directory (or load-file-name buffer-file-name)))
;; eval: (setq config-dir (file-name-as-directory (concat (file-name-as-directory config-dotemacs-lisp) "config")))
;; End:
