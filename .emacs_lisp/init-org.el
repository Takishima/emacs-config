;;; init-org.el --- Initialisation for Org mode -*- lexical-binding: t -*-

;; Author: Damien Nguyen
;; Maintainer: Damien Nguyen
;; Version: 1.0
;; Package-Requires: ()
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

;; The daily timesheet (lib/dn-timesheet.el).

;;; Code:

;; ========================================================================== ;;

(require 'config-functions (concat config-dir "functions.el"))
(require 'use-package)

;; ========================================================================== ;;

(use-package dn-timesheet
  :straight nil
  :commands (dn-timesheet-check-in dn-timesheet-check-out dn-timesheet-toggle
             dn-timesheet-status dn-timesheet-report dn-timesheet-open)
  :init
  (defvar-keymap dn-timesheet-map
    :doc "Keymap for the daily timesheet, bound to C-c w."
    "i" #'dn-timesheet-check-in
    "o" #'dn-timesheet-check-out
    "t" #'dn-timesheet-toggle
    "s" #'dn-timesheet-status
    "r" #'dn-timesheet-report
    "f" #'dn-timesheet-open)
  (keymap-global-set "C-c w" dn-timesheet-map)
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c w" "timesheet")))

;; ========================================================================== ;;

(provide 'init-org)

;;; init-org.el ends here
