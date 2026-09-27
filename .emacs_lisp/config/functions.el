;;; config-functions.el --- Configuration helper functions  -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Homepage: homepage
;; Keywords: keywords


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

(let (
      (here (file-name-directory (or load-file-name buffer-file-name)))
      )
  (require 'config-variables (concat here "variables.el"))
  )

;; ========================================================================== ;;

(defun config-load-file-exec-func (filename function &optional err)
  "Load file FILENAME if it exists and then call a function FUNCTION."
  (let
      (
       (function_name (symbol-name function))
       (error_on_failure (or err nil))
       )
    (progn
      (if (file-exists-p filename)
          (progn
            (load (file-name-sans-extension filename))
            (setq function (intern-soft function_name))
            (if (fboundp function)
                (progn
                  (message "INFO: calling function %s" function_name)
                  (funcall function)
                  )
              (if error_on_failure
                  (user-error "Unable to find function '%s' in file '%s'" function_name filename)
                )
              )
            )
        (if error_on_failure
            (user-error "Unable to find file '%s'" filename)
          )
        )
      )
    )
  )

;; ========================================================================== ;;

(defun config-require (feature)
  "Same as (require FEATURE) but assuming the file is within config-dotemacs-lisp."
  (let* (
        (feature-name (symbol-name feature))
        (filename1 (concat config-dir feature-name ".el"))
        (filename2 (concat config-dotemacs-lisp feature-name ".el"))
        (filename (if (file-exists-p filename1) filename1 filename2))
        )
     (message "INFO: requiring %s from %s" feature-name filename)
     (require feature filename)
    )
  )

(defun dn-load-directory (dir prefix &optional skip)
  "Require each DIR/*.el, in sorted order, as feature PREFIX<base>.
Files whose name starts with # or . are ignored, as are those whose
base name, as a symbol, is in SKIP.  Each file is required by path, so
one that does not provide PREFIX<base> signals an error."
  (dolist (file (directory-files dir t "\\`[^#.].*\\.el\\'"))
    (let ((base (file-name-base file)))
      (unless (memq (intern base) skip)
        (require (intern (concat prefix base)) file)))))

;; ========================================================================== ;;

(defmacro config-when-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(if (symbolp ,type)
       (when (eq system-type (intern-soft ,type))
         ,@body)
     (when (member system-type ,type)
       ,@body)
     )
  )

(defmacro config-unless-system (type &rest body)
  "Evaluate BODY if `system-type' does not equals TYPE or is not contained in TYPE."
  (declare (indent defun))
  `(if (symbolp ,type)
       (unless (eq system-type (intern-soft ,type))
         ,@body)
     (unless (member system-type ,type)
       ,@body)
     )
  )

;; ========================================================================== ;;

;; From https://github.com/KaratasFurkan/.emacs.d
(defun dn-async-process (command &optional name filter)
  "Start an async process by running the COMMAND string with bash. Return the
process object for it.

NAME is name for the process. Default is \"async-process\".

FILTER is function that runs after the process is finished, its args should be
\"(process output)\". Default is just messages the output."
  (make-process
   :command `("bash" "-c" ,command)
   :name (if name name
           "async-process")
   :filter (if filter filter
             (lambda (process output) (message (string-trim output)))))
  )

;; ========================================================================== ;;

(provide 'config-functions)

;;; config-functions.el ends here
