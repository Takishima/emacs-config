;;; early-init.el --- Settings needed before the init file -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setenv "LSP_USE_PLISTS" "true")
(setq package-enable-at-startup nil)

;; Keep straight's builds and session state in ~/.emacs.d under --init-directory.
;; EMACS_USER_DIRECTORY overrides that, e.g. for a throwaway directory in CI.
(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name (or (getenv "EMACS_USER_DIRECTORY") "~/.emacs.d/"))))

;;; early-init.el ends here
