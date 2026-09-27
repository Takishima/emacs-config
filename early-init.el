;;; early-init.el --- Settings needed before the init file -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setenv "LSP_USE_PLISTS" "true")
(setq package-enable-at-startup nil)

;; Keep straight's builds and session state in ~/.emacs.d under --init-directory.
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))

;;; early-init.el ends here
