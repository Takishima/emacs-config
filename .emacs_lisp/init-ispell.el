;;; init-ispell.el --- Initialisation for ispell -*- lexical-binding: t -*-

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

;; Use aspell if installed, otherwise hunspell, always with the British English
;; dictionary.  Largely inspired by
;; https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

;;; Code:

;; ========================================================================== ;;

(require 'use-package)
(require 'ispell)

;; ========================================================================== ;;

(defun dn-flyspell-detect-ispell-args (&optional run-together)
  "Return the ispell arguments for the current spell checker.
If RUN-TOGETHER is non-nil, also spell check CamelCase words."
  (let (args)
    (cond
     ((not (stringp ispell-program-name)))
     ((string-match "aspell$" ispell-program-name)
      (setq args (list "--sug-mode=ultra" "--lang=en_GB"))
      (when run-together
        (cond
         ;; Newer aspell supports camel case directly, see
         ;; https://github.com/redguardtoo/emacs.d/issues/796
         ((string-match-p "--camel-case"
                          (shell-command-to-string (concat ispell-program-name " --help")))
          (setq args (append args '("--camel-case"))))

         (t
          (setq args (append args '("--run-together" "--run-together-limit=16"))))))))
    args))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; hunspell receives `ispell-local-dictionary' as -d, and it is also the key
  ;; into `ispell-local-dictionary-alist'.
  (setq ispell-local-dictionary "en_GB")
  (setopt ispell-local-dictionary-alist
          '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8))))
 (t (setq ispell-program-name nil)))

(setq-default ispell-extra-args (dn-flyspell-detect-ispell-args t))

(defun dn--ispell-with-plain-args (orig-fun &rest args)
  "Call ORIG-FUN with ARGS using the ispell arguments without run-together."
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (dn-flyspell-detect-ispell-args))
    (apply orig-fun args)
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))
(advice-add 'ispell-word :around #'dn--ispell-with-plain-args)
(advice-add 'flyspell-auto-correct-word :around #'dn--ispell-with-plain-args)

(defun dn-ispell-text-mode-setup ()
  "Turn off the run-together option when spell checking text modes."
  (setq-local ispell-extra-args (dn-flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook #'dn-ispell-text-mode-setup)

(setopt ispell-silently-savep t)

(defun fr-dic ()
  "Switch to the Swiss French dictionary."
  (interactive)
  (ispell-change-dictionary "fr_CH"))

(defun en-dic ()
  "Switch to the British English dictionary."
  (interactive)
  (ispell-change-dictionary "en_GB"))

(defun de-dic ()
  "Switch to the German dictionary."
  (interactive)
  (ispell-change-dictionary "de_DE"))

;; ========================================================================== ;;

(use-package flycheck-aspell
  :straight t
  :ensure-system-package aspell
  :config
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic))

;; ========================================================================== ;;

(provide 'init-ispell)

;;; init-ispell.el ends here
