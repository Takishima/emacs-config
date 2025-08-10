;;; init-llm.el --- Initialisation for AI & LLM -*- lexical-binding: t -*-

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

;; ============================================================================================== ;;

(require 'use-package)

;; ============================================================================================== ;;

(use-package vterm
  :straight t)

(use-package aidermacs
  :straight t
  :bind (("C-c a" . aidermacs-transient-menu))
  ;; :config
  ;; (add-hook 'aidermacs-before-run-backend-hook
  ;;           (lambda ()
  ;;             (progn
  ;;               (setenv "OPENAI_API_BASE" "https://api.githubcopilot.com")
  ;;               (setq auth-source-1password-vault "Employee")
  ;;               (setenv "OPENAI_API_KEY" (auth-source-pick-first-password :port "Employee" :host "GitHub Copilot API Token" :user "credential"))
  ;;               )
  ;;             ))
  :custom
  (aidermacs-exit-kills-buffer t)
  (aidermacs-backend 'vterm)
  (aidermacs-comint-multiline-newline-key "S-<return>")
  (aidermacs-vterm-multiline-newline-key "S-<return>")
  (aidermacs-default-chat-mode 'architect))

;; ============================================================================================== ;;

(provide 'init-llm)

;;; init-llm.el ends here
