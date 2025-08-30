;;; ros2-keybindings-mode.el --- ROS2 keybindings -*- lexical-binding: t -*-

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

;; ========================================================================== ;;

(require 'ros)

(defvar ros2-keybindings-mode--keymap (make-keymap) "num-mode keymap.")
(define-key ros2-keybindings-mode--keymap (kbd "C-c C-r") 'hydra-ros-main/body)


(define-minor-mode ros2-keybindings-mode
  "Minor mode to set some keybindings with the ROS package"
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " ROS2KBD"
  ;; The minor mode bindings.
  :keymap
  ros2-keybindings-mode--keymap
  )

;; ========================================================================== ;;

(provide 'ros2-keybindings-mode)

;;; ros2-keybindings-mode.el ends here
