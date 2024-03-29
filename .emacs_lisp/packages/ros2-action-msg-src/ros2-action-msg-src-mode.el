;;; ros2-action-msg-src-mode.el --- ROS2 files support -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: (use-package)
;; Homepage: nil
;; Keywords: init


;; MIT License

;; Copyright (c) 2023 Damien Nguyen

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

(require 'yaml-mode)

(defcustom ros2-action-msg-src-mode-hook nil
  "Hook run when entering ROS2 action/msg/srv mode."
  :type 'hook
  :group 'dn)

(defvar ros2-action-msg-src--keywords
  '("bool" "byte" "char" "float32" "float64" "int8" "uint8" "int16" "uint16" "int32" "uint32" "int64" "uint64")
  "List of keywords for ROS2 action/msg/srv.")

(defvar ros2-action-msg-src--keywords-regexp
  (format "%s" (regexp-opt ros2-action-msg-src--keywords 'symbols))
  "Regex to match keywords")

(defvar ros2-action-msg-src--variable-decl-regexp
  (format "\\(%s\\)[[:space:]]+\\([a-zA-Z0-9_]+\\)" (regexp-opt ros2-action-msg-src--keywords))
  "Regex to match keywords")

(if (fboundp 'defvar-keymap)
    ;; defvar-keymap is an Emacs 29.x addition
    (defvar-keymap ros2-action-msg-src-mode-map
      :doc "Keymap for ROS2 action/msg/srv mode."
      :parent yaml-mode-map
      )
  (defvar ros2-action-msg-src-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map yaml-mode-map)
      map)
    "Keymap for ROS2 action/msg/srv mode.")
  )

(defvar ros2-action-msg-src--font-lock-defaults
  `((
     ("\"\\.\\*\\?" . font-lock-string-face)
     ("#.*" . font-lock-comment-face)
     (,ros2-action-msg-src--variable-decl-regexp
      (1 font-lock-builtin-face)
      (2 font-lock-variable-name-face))
     (,ros2-action-msg-src--keywords-regexp . font-lock-builtin-face)
     ("[0-9]+" . font-lock-constant-face)
     ("\\_<\\(string\\|wstring\\)\\_>" . font-lock-builtin-face)
     ("\\(string\\|wstring\\)\\(<=\\)"
      (1 font-lock-builtin-face)
      (2 font-lock-operator-face))
     ("\\(<=\\|=\\)" . font-lock-operator-face)
     ("\\-\\-\\-" . font-lock-type-face)
     )))

(define-derived-mode ros2-action-msg-src-mode prog-mode "ROS2 action mode"
  "Major mode for editing ROS2 action/msg/srv files."
  (setq font-lock-defaults ros2-action-msg-src--font-lock-defaults)
  (setq comment-start "#")
  (setq comment-end "")
  )

;; ========================================================================== ;;

(provide 'ros2-action-msg-src-mode)

;;; ros2-action-msg-src-mode.el ends here
