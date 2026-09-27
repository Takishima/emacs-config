;;; init-programming.el --- Initialisation for programming -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
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

;;; Code:

;; ========================================================================== ;;


(require 'cl-lib)
(require 'use-package)
(require 'config-functions (concat config-dir "functions.el"))

;; ========================================================================== ;;

(use-package diff-mode
  :straight nil
  :mode
  "\\.patch[0-9]*\\'"
  )

;; ========================================================================== ;;

;; ========================================================================== ;;

(use-package flycheck
  :straight t
  :custom
  (flycheck-clang-args '("-std=c++20"))
  )

(use-package datetime
  :straight  (datetime :type git :host github :repo "doublep/datetime"
                       :fork (:host github
                                    :repo "Takishima/datetime")))
(use-package logview
  :straight t
  :custom
  (logview-completing-read-function 'completing-read)
  (logview-additional-submodes '(("ROS2" (format . "[LEVEL] [TIMESTAMP] [NAME]:") (levels . "SLF4J")
                                  (timestamp "ROS2"))))
  (logview-additional-timestamp-formats '(("ROS2" (java-pattern . "A.SSSSSSSSS"))))
  )

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

;; ========================================================================== ;;

(use-package format-all
  :straight t
  )

;; ========================================================================== ;;

;; Built-in treesit configuration (Emacs 29+)
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  
  ;; Auto-install treesit grammars when needed
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Function to install missing grammars
  (defun treesit-install-all-grammars ()
    "Install all treesit grammars defined in `treesit-language-source-alist'."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (let ((lang (car grammar)))
        (unless (treesit-language-available-p lang)
          (message "Installing treesit grammar for %s..." lang)
          (treesit-install-language-grammar lang)))))

  ;; Auto-install grammars on first use
  (advice-add 'treesit-parser-create :before
              (lambda (language &rest _)
                (unless (treesit-language-available-p language)
                  (message "Auto-installing treesit grammar for %s..." language)
                  (treesit-install-language-grammar language))))

  ;; Enable treesit modes by default where available
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (css-mode . css-ts-mode)
          (go-mode . go-ts-mode)
          (js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)
          (typescript-mode . typescript-ts-mode))))

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
	(setq skip-names (seq-remove (lambda (el) (or (string-prefix-p "#" el) (string-prefix-p ";" el)))
                                     (with-temp-buffer (insert-file-contents (concat dir-path "skip.txt"))
					               (split-string (buffer-string) "[\n\r]" t "[ \t]+"))
                                     )
              )
	(message "INFO: will be skipping the following: %S" skip-names)
	)
    )
  (dolist (fname dir-list)
    (setq require-name (intern-soft (concat "init-prog-"
        				    (file-name-sans-extension (file-name-nondirectory fname)))))
    (unless (member (file-name-nondirectory fname) skip-names)
      ;; (byte-recompile-file fname nil 0)
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

(provide 'init-programming)

;;; init-programming.el ends here
