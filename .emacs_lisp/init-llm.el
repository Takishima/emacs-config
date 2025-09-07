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


;; for eat terminal backend:
(use-package eat :straight t)

(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :config
  (claude-code-mode))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :straight t
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  )

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode)
  :bind (:map global-map
              ("C-c c f" . copilot-chat-fix)
              ("C-c c o" . copilot-chat-optimize)
              ("C-c c y" . copilot-chat-yank)
              ("C-c c M-y" . copilot-chat-yank-pop)
              ("C-c c C-M-y" . (lambda () (interactive) (copilot-chat-yank-pop -1))))
)

;; ============================================================================================== ;;

(provide 'init-llm)

;;; init-llm.el ends here
