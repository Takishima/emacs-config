;;; cmake-functions.el --- Helper functions for Yasnippet and CMake -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: nil
;; Keywords: init


;; MIT License

;; Copyright (c) 2022 Damien Nguyen

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

(require 'cl-lib)

(defun yas-lib-cmake--find-function-definition (&optional search-point)
  "Find a CMake function/macro definition starting at SEARCH-POINT."
  (save-excursion
    (search-forward-regexp (rx (* whitespace)
                               (or "function" "macro")
                               (* (or whitespace control))
                               "("
                               (* (or whitespace control))
                               (group (+ (any "a-zA-Z0-9_"))) ; function name
                               (opt (seq (* (or whitespace control))
                                         (group (+ (or (any "a-zA-Z0-9_") whitespace control))))
                                    )
                               ")"
                               (group (*? anything))
                               (or "endfunction" "endmacro"))
                           search-point))
  )

(defun yas-lib-cmake--cmake-parse-arguments (cmake-parse-args)
  "Parse the content of a call to cmake_parse_arguments passed in CMAKE-PARSE-ARGS."
  (let* ((bound (string-match (rx (* (or whitespace control))
                                  "cmake_parse_arguments"
                                  (* (or whitespace control))
                                  "("
                                  (* (or whitespace control))
                                  (opt (seq "PARSE_ARGV" (+ (or whitespace control)) (+ digit)))
                                  (* (or whitespace control))
                                  (or (+ (any "a-zA-Z0-9_"))        ; prefix
                                      (seq "${" (* (not "}")) "}")
                                      (seq "\"${" (* (not "}")) "}\""))
                                  (+ (or whitespace control))
                                  (seq "\"" (group (* (not "\""))) "\"") ; options
                                  (+ (or whitespace control))
                                  (seq "\"" (group (* (not "\""))) "\"") ; one_value_keywords
                                  (+ (or whitespace control))
                                  (seq "\"" (group (* (not "\""))) "\"") ; multi_value_keywords
                                  (* (or whitespace control))
                                  ")"
                                  )
                              cmake-parse-args))
         (option (match-string-no-properties 1 cmake-parse-args))
         (one (match-string-no-properties 2 cmake-parse-args))
         (multi (match-string-no-properties 3 cmake-parse-args))
         )
    (when bound
      (list (split-string option ";" t)
            (split-string one ";" t)
            (split-string multi ";" t)
            )
      )
    )
  )

;; -------------------------------------------------------------------------- ;;

(defun yas-lib-cmake-get-function ()
  "Find a function/macro declaration starting at the current point."
  (let ((bound (yas-lib-cmake--find-function-definition))
        (function_name)
        (args)
        (cmake_parse_args)
        )
    (when bound
      (setq function_name (string-trim (match-string-no-properties 1)))
      (setq cmake_parse_args (match-string-no-properties 3))
      (setq args (match-string-no-properties 2))
      (when args (setq args (split-string (replace-regexp-in-string "[\n \t]+" " " args))))
      (when cmake_parse_args (setq cmake_parse_args (string-trim cmake_parse_args)))
      (list function_name args cmake_parse_args))
    )
  )

(defun yas-lib-cmake-function-docstring ()
  "Generate a basic docstring for a CMake function/macro."
  (let (
        (func_data (yas-lib-cmake-get-function))
        (indent (concat "\n" (make-string (current-column) 32) "# "))
        arg_indent
        function_name
        args
        cmake_parse_args
        options
        one_value_keywords
        multi_value_keywords
        )
    (when func_data
      (cl-multiple-value-setq (function_name args cmake_parse_args) func_data)
      (setq arg_indent (concat indent (make-string (+ 1 (string-width function_name)) 32)))
      (when args (setq args (mapcar (lambda(s)(concat "<" s ">")) args)))
      (when cmake_parse_args
        (cl-multiple-value-setq (options one_value_keywords multi_value_keywords)
          (yas-lib-cmake--cmake-parse-arguments cmake_parse_args))
        (when options
          (setq options (concat "[" (string-join options ", ") "]")))
        (when one_value_keywords
          (setq one_value_keywords (mapcar (lambda(s)(concat "[" (upcase s) " "
                                                             "<" (downcase s) ">"
                                                             "]"))
                                           one_value_keywords)))
        (when multi_value_keywords
          (setq multi_value_keywords (mapcar (lambda(s)(concat "[" (upcase s) " "
                                                               "<" (downcase s) ">"
                                                               " [... "
                                                               "<" (downcase s) ">"
                                                               "]]"))
                                             multi_value_keywords)))
        )
      (concat "# "
              function_name
              "("
              (string-join (remove nil (append args (list options) one_value_keywords multi_value_keywords)) arg_indent)
              ")")

      )
    ))

;; ========================================================================== ;;

(provide 'yas-lib-cmake-functions)

;;; cmake-functions.el ends here
