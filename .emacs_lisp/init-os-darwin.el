;;; init-os-darwin.el --- macOS-only initialisations -*- lexical-binding: t -*-

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

;; Loaded from .emacs only when `system-type' is `darwin'.

;;; Code:

(require 'use-package)
(require 'cl-lib)

(defvar mac-command-modifier)
(defvar mac-option-modifier)
(defvar c++-ts-mode-map)
(defvar rst-mode-map)
(declare-function do-applescript "nsfns.m" (script))

(add-to-list 'default-frame-alist '(font . "Monaco" ))

(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      default-input-method "MacOSX")

;; Option is Super, so restore the characters the Swiss layout types with Option.
(keymap-global-set "s-g" (lambda() (interactive) (insert "@")))
(keymap-global-set "s-3" (lambda() (interactive) (insert "#")))
(keymap-global-set "s-4" (lambda() (interactive) (insert "Ç")))
(keymap-global-set "s-5" (lambda() (interactive) (insert "[")))
(keymap-global-set "s-6" (lambda() (interactive) (insert "]")))
(keymap-global-set "s-7" (lambda() (interactive) (insert "|")))
(keymap-global-set "s-8" (lambda() (interactive) (insert "{")))
(keymap-global-set "s-9" (lambda() (interactive) (insert "}")))
(keymap-global-set "s-/" (lambda() (interactive) (insert "\\")))
(keymap-global-set "s-n" (lambda() (interactive) (insert "~")))

(add-to-list 'jka-compr-compression-info-list
             ["\\.plist$"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])
;; Changes to `jka-compr-compression-info-list' take effect only after this.
(jka-compr-update)

(defvar dn-compile-in-iterm-command "make"
  "Command `dn-compile-in-iterm' offers when its history is empty.")
(defvar dn-compile-in-iterm-history nil
  "Minibuffer history of `dn-compile-in-iterm'.")

(defun dn-compile-in-iterm (command)
  "Type COMMAND into the current iTerm session.
An empty answer reuses the last command, or `dn-compile-in-iterm-command'."
  (interactive
   (let ((default (or (car dn-compile-in-iterm-history)
                      dn-compile-in-iterm-command)))
     (list (read-from-minibuffer (format "Command [%s]: " default)
                                 nil nil nil 'dn-compile-in-iterm-history default))))
  (when (string= "" command)
    (setq command (or (car dn-compile-in-iterm-history)
                      dn-compile-in-iterm-command)))
  (do-applescript
   (concat "tell application \"iTerm\"\ntell current session of current window\nwrite text \""
           (replace-regexp-in-string "\"" "\\\"" command t t)
           "\"\nend tell\nend tell")))

(with-eval-after-load 'cc-mode
  (keymap-set c++-mode-map "C-c i" #'dn-compile-in-iterm))
(with-eval-after-load 'c-ts-mode
  (keymap-set c++-ts-mode-map "C-c i" #'dn-compile-in-iterm))
(with-eval-after-load 'rst
  (keymap-set rst-mode-map "C-c i" #'dn-compile-in-iterm))

(with-eval-after-load 'tex
  (let (
        (skim-path "/Applications/Skim.app/Contents/SharedSupport/")
        (skim-exec "displayline")
        )
    (push skim-path exec-path)
    (setq skim-exec (executable-find skim-exec))
    (pop exec-path)
    (when skim-exec
      (setq TeX-view-program-selection (append '((output-pdf "macos-skim"))
                                               (cl-remove-if (lambda (el) (eq (car el) 'output-pdf))
                                                             TeX-view-program-selection)))
      (setq TeX-view-program-list (push `("macos-skim" ,(concat skim-exec " -b -g %n %o %b"))
                                        TeX-view-program-list))
      )
    )

  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
  (add-to-list 'exec-path "/Library/TeX/texbin" t)
  )

(use-package htmlize
  :straight t
  :commands (htmlize-buffer htmlize-region))

(defun formatted-copy-buffer ()
  "Export buffer to HTML, and copy it to the clipboard as rich text."
  (interactive)
  (save-window-excursion
    (let ((buf (htmlize-buffer)))
      (with-current-buffer buf
        (progn
          (shell-command-on-region
           (point-min)
           (point-max)
           "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
        (kill-buffer buf)
        ))))

(defun formatted-copy-region ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let ((buf (htmlize-region (region-beginning) (region-end))))
      (with-current-buffer buf
        (progn
          (shell-command-on-region
           (point-min)
           (point-max)
           "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
        (kill-buffer buf)
        ))))

(provide 'init-os-darwin)

;;; init-os-darwin.el ends here
