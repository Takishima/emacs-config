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

(define-derived-mode ros2-action-msg-src-mode
  yaml-mode "ROS2 action mode"
  "Major mode for editing ROS2 action/msg/srv files."
)

(require 'lsp)

(add-to-list 'lsp-language-id-configuration '("\\.action\\'" . "yaml"))
(add-to-list 'lsp-language-id-configuration '("\\.msg\\'" . "yaml"))
(add-to-list 'lsp-language-id-configuration '("\\.srv\\'" . "yaml"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.action\\'" . ros2-action-msg-src-mode))
(add-to-list 'auto-mode-alist '("\\.msg\\'" . ros2-action-msg-src-mode))
(add-to-list 'auto-mode-alist '("\\.srv\\'" . ros2-action-msg-src-mode))


;; ========================================================================== ;;

(provide 'ros2-action-msg-src-mode)

;;; ros2-action-msg-src-mode.el ends here
