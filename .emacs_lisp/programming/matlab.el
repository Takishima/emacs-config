;;; init-prog-matlab.el --- Initialisation for MATLAB/Octave -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Homepage: nil
;; Keywords: init, programming

;; MIT License

;; Copyright (c) 2020 Damien Nguyen

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

(require 'use-package)

;; ========================================================================== ;;

(use-package matlab
  :ensure t
  :init
  (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
  (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
  :bind
  (:map matlab-mode-map
        ("M-;" . nil)
        ("M-s" . nil)
        ("M-C-s" . matlab-show-matlab-shell-buffer))
  :custom
  (matlab-functions-have-end t)
  (matlab-indent-function-body 'MathWorks-Standard)
  )

(defun string-replace (this withthat in)
  "replace THIS with WITHTHAT' in the string IN."
  (with-temp-buffer
    (insert in)
    (goto-char (point-min))
    (while (search-forward this nil t)
      (replace-match withthat nil t))
    (buffer-substring (point-min) (point-max))))

(defun imdoc-capture-arguments(args_str)
  "Process a string containing the arguments list."
  (let (
        (index 0)
        (end_idx 0)
        (cur_arg "")
        (result 0)
        (arg_list "")
        )
    (progn
      (while result
        (progn
          (setq result (string-match "\\([a-zA-Z0-9_]+\\)" args_str index))
          (when result
            (progn
              (setq end_idx (match-end 0))
              (setq cur_arg (string-as-unibyte (substring args_str result end_idx)))
              (setq arg_list (concat arg_list cur_arg "####"))
              (setq index end_idx)
              )
            )
          )
        )
      (split-string (string-replace "####" " " arg_list) " " t)
      )
    ))

(defun imdoc-capture-return-value(ret_str_raw)
  "Process a string containing the return values list."
  (if ret_str_raw
      (let (
            (index 0)
            (end_idx 0)
            (cur_arg "")
            (result 0)
            (ret_str (string-as-unibyte ret_str_raw))
            (ret_list "")
            )
        (progn
          (while result
            (progn
              (setq result (string-match "\\([a-zA-Z0-9_]+\\)" ret_str index))
              (when result
                (progn
                  (setq end_idx (match-end 0))
                  (setq cur_arg (substring ret_str result end_idx))
                  (setq ret_list (concat ret_list cur_arg "####"))
                  (setq index end_idx)
                  )
                )
              )
            )
          (split-string (string-replace "####" " " ret_list) " " t)
          )
        )
    )
  )

(defun imdoc()
  "Insert matlab documentation template for a function."
  (interactive)
  (let (
        (counter 0)
        (keep-on t)
        (index 0)
        (func_ret "")
        (func_name "")
        (func_args "")
        (length 0)
        )
    (progn
      ;; first extract function name & argument list
      (goto-char 0)
      (condition-case err
          (progn
            (re-search-forward "\\s-*function\\s-*\\(\\[?[a-zA-Z0-9_, ]+\\]?\\)\\s-*=\\s-*\\([a-zA-Z0-9_]+\\)\\s-*(?\\(.*\\))?")
            (setq func_ret  (match-string 1))
            (setq func_name (match-string 2))
            (setq func_args (match-string 3))
            )
        (error
         (progn
           (re-search-forward "\\s-*function\\s-*\\([a-zA-Z0-9_]+\\)\\s-*(?\\(.*\\))?")
           (setq func_ret  nil)
           (setq func_name (match-string 1))
           (setq func_args (match-string 2))
           ))
        )
      ;; (re-search-forward "\\s-*function\\s-*\\(\\[?[a-zA-Z0-9_, ]+\\]?\\)\\s-*=\\s-*\\([a-zA-Z0-9_]+\\)\\s-*(?\\(.*\\))?")

      (setq func_ret (imdoc-capture-return-value func_ret))
      (setq func_args (imdoc-capture-arguments func_args))

      ;; insert data
      (insert "\n")
      (insert (concat "% ======================================"
                      "========================================\n"))
      (insert (concat "% " (upcase func_name) "     Description\n"))
      (when func_args
        (progn
          (insert "%\n%  INPUT:\n"))
        (dolist (var func_args)
          (setq length (- 15 (string-width var)))
          (when (< length 0) (setq length 0))
          (insert (concat "%       '" (format "%s':" var) (make-string length ? ) "\n"))
          ;; (insert (concat "%     " (format "%-15s" var) ": \n"))
          )
        )
      (if func_ret
          (progn
            (insert "%\n%  OUTPUT:\n")
            (dolist (var func_ret)
              (progn
                (setq length (- 15 (string-width var)))
                (when (< length 0) (setq length 0))
                (insert (concat "%       '" (format "%s':" var) (make-string length ? ) "\n"))
                ;; (insert (concat "%     - " (format "%-15s" var) ": \n"))
                )
              )
            )
        )
      (insert (concat "%\n% Revision:\n% "
                      (format-time-string "%y%m%d" (current-time))
                      " - nguyen - initial version\n%\n"))
      (insert (concat "% ======================================"
                      "========================================\n\n"))
      )
    )
  )

;; ========================================================================== ;;

;; Octaveload-path mode
;; (autoload 'octave-mode "octave-mod" nil t)

;; ;; Change comment char to '%'
;; (setq octave-comment-char ?%)
;; (add-hook 'octave-mode-hook
;;        (lambda ()
;;          (modify-syntax-entry ?% "<" octave-mode-syntax-table)))

;; ========================================================================== ;;

(provide 'init-prog-matlab)

;;; init-prog-matlab.el ends here
