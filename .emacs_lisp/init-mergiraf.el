;;; init-mergiraf.el --- Mergiraf integration for smerge-mode -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: nil
;; Keywords: init merge conflicts


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

;; Integrates mergiraf (https://mergiraf.org) with smerge-mode for
;; syntax-aware conflict resolution.
;;
;; Usage:
;;   When a file with merge conflicts is opened, smerge-mode is automatically
;;   enabled. Press C-c ^ m to run mergiraf on the current buffer.
;;
;; Commands:
;;   smerge-mergiraf-solve - Run mergiraf solve on the current buffer

;;; Code:

;; ========================================================================== ;;

(require 'smerge-mode)

;; ========================================================================== ;;

(defun smerge-mergiraf-has-conflicts-p ()
  "Check if the current buffer contains merge conflict markers."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^<<<<<<< " nil t)))

(defun smerge-mergiraf-solve ()
  "Run `mergiraf solve' on the current buffer to resolve merge conflicts.
After running mergiraf, the buffer is reverted and smerge-mode is re-enabled
if conflicts remain."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (unless (smerge-mergiraf-has-conflicts-p)
    (user-error "No merge conflicts found in buffer"))

  (let ((filename (buffer-file-name)))
    ;; Save the buffer before running mergiraf
    (save-buffer)

    (message "Running mergiraf solve on %s..." (file-name-nondirectory filename))

    ;; Run mergiraf synchronously and capture output
    (let* ((output-buffer (generate-new-buffer "*mergiraf output*"))
           (exit-code (call-process "mergiraf" nil output-buffer nil
                                   "solve" filename)))
      (if (= exit-code 0)
          (progn
            ;; Success - revert buffer and check for remaining conflicts
            (revert-buffer t t t)
            (if (smerge-mergiraf-has-conflicts-p)
                (progn
                  (smerge-mode 1)
                  (message "Mergiraf partially resolved conflicts. Manual resolution needed."))
              (progn
                (smerge-mode -1)
                (message "Mergiraf successfully resolved all conflicts!"))))
        (progn
          ;; Failed - show error output
          (message "Mergiraf failed to solve conflicts (exit code %d)" exit-code)
          (with-current-buffer output-buffer
            (goto-char (point-min))
            (when (> (buffer-size) 0)
              (message "Mergiraf output: %s" (buffer-string))))))
      (kill-buffer output-buffer))))

(defun smerge-mergiraf-solve-and-save ()
  "Run `mergiraf solve' on the current buffer and save the result.
This is a convenience command that combines solving and saving."
  (interactive)
  (smerge-mergiraf-solve)
  (when (buffer-modified-p)
    (save-buffer)))

;; ========================================================================== ;;
;; Add mergiraf command to smerge-mode keymap

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "C-c ^ m") 'smerge-mergiraf-solve)
  (define-key smerge-mode-map (kbd "C-c ^ M") 'smerge-mergiraf-solve-and-save))

;; ========================================================================== ;;

(provide 'init-mergiraf)

;;; init-mergiraf.el ends here
