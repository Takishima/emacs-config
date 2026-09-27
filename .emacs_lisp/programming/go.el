;;; go.el --- Go support -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: (use-package)
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

(require 'use-package)
(require 'config-functions (concat config-dir "functions.el"))

;; go-mode is installed so third-party utilities that hook go-mode-hook keep
;; working; .go files are remapped to go-ts-mode via major-mode-remap-alist.
(use-package go-mode
  :straight t)

(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

(defun dn-go-enable-format-on-save ()
  "Enable `lsp-format-buffer' in `before-save-hook' (buffer-local)."
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

(use-package go-ts-mode
  :straight nil
  :mode (("\\.go\\'"     . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :hook ((go-ts-mode . lsp-deferred)
         (go-ts-mode . dn-go-enable-format-on-save))
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (setq-local devdocs-current-docs '("go"))
              (setq-local dash-docs-docsets '("Go")))))

(use-package go-tag
  :straight t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c t a" . go-tag-add)
              ("C-c t r" . go-tag-remove)))

(use-package go-impl
  :straight t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c t i" . go-impl)))

(use-package gotest
  :straight t
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-x t t" . go-test-current-project)
              ("C-x t f" . go-test-current-file)
              ("C-x t m" . go-test-current-test)
              ("C-x t r" . go-test-current-run)
              ("C-x t b" . go-test-current-benchmark)
              ("C-x t k" . go-test-current-coverage))
  :config
  (which-key-add-major-mode-key-based-replacements 'go-ts-mode "t" "Testing"))

(with-eval-after-load 'go-ts-mode
  (keymap-set go-ts-mode-map "C-c C-f" #'lsp-format-buffer)
  (keymap-set go-ts-mode-map "C-c C-r" #'lsp-format-region))

(provide 'init-prog-go)

;;; go.el ends here
