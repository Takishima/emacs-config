;;; ris.el --- major mode for editing RIS bibliography files
;; $Id: ris.el,v 1.9 2005/05/08 19:34:02 mhoenicka Exp $

;; Copyright (C) 2001-2005  Markus Hoenicka

;; Author: Markus Hoenicka <markus@mhoenicka.de>
;; Keywords: bibliography ris

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Still crude but works. This mode does not attempt to fully validate the
;; RIS data but it provides visual clues to avoid the most common errors
;; in addition, it provides some convenience commands for editing RIS
;; datasets.
;;
;; To install this mode, move this file somewhere into your load-path
;; (something like /usr/local/share/emacs/site-lisp), and (optionally)
;; byte-compile-file it.
;;
;; To load the mode automatically at startup and to associate .ris
;; files with this mode, put the following code into your .emacs
;; (or site-start.el):
;; ---8<-------------------------------
;; ;; Turn on syntax coloring
;; (cond ((fboundp 'global-font-lock-mode)
;;       ;; Turn on font-lock in all modes that support it
;;       (global-font-lock-mode t)
;;       ;; maximum colors
;;       (setq font-lock-maximum-decoration t)))
;;
;; ;; ris mode
;; (autoload 'ris-mode "ris" "Major mode for RIS bibliography files." t)
;; (or (assoc "\\.ris$" auto-mode-alist)
;;     (setq auto-mode-alist (cons '("\\.ris$" . ris-mode)
;; 				auto-mode-alist)))
;; ---8<-------------------------------

;;; Code:

; prepare font-locking
(make-face 'ris-main-face)
(make-face 'ris-main-ltd-face)
(make-face 'ris-tag-face)
(make-face 'ris-tyer-tag-face)
(make-face 'ris-author-face)
(make-face 'ris-pubyear-face)
(make-face 'ris-reprint-face)

(set-face-foreground 'ris-main-face "maroon")
(set-face-foreground 'ris-main-ltd-face "dark orchid")
(set-face-foreground 'ris-tag-face "blue")
(set-face-foreground 'ris-tyer-tag-face "red")
(set-face-foreground 'ris-author-face "sea green")
(set-face-foreground 'ris-pubyear-face "IndianRed2")
(set-face-foreground 'ris-reprint-face "deep pink")

;; these regexps do the "validation"
(defvar ris-font-lock-keywords
  '(
;; three possible values. ON REQUEST must be followed by a date in
;; MM/DD/YY notation
    ("^RP  - \\(\\(IN FILE$\\)\\|\\(NOT IN FILE$\\)\\|\\(ON REQUEST ([0-1][0-9]/[0-3][0-9]/[0-9][0-9])$\\)\\)" 1 'ris-reprint-face t)

;; YYYY/MM/DD/othertext, with MM,DD, and othertext optional. The
;; slashes must still be present. Othertext may not exceed 255 chars
    ("^\\(PY\\|Y1\\|Y2\\)  - \\([0-9][0-9][0-9][0-9]/\\([0-1][0-9]\\)?/\\([0-3][0-9]\\)?/.\\{0,255\\}\\)" 2 'ris-pubyear-face t)

;; corporate authors do not use a comma. Other authors may use up to two
;; commas, the first to separate first and middle names from the last name,
;; the second to separate an optional suffix. First and middle names may
;; either be abbreviated (with no space following the period), or spelled
;; out. Currently no length checking is implemented
    ("^\\(AU\\|A[1-3]\\|ED\\)  - \\(\\([^,\n]+,\\([^,\n\\. ]\\(\\.\\|[^,\\.\n]+\\)\\)+\\(,[^,\n ][^,\n]*\\)?$\\)\\|\\([^,\n]+$\\)\\)" 2 'ris-author-face t)

;; fields with a limited length of up to 255 chars
    ("^\\(ID\\|KW\\|SP\\|EP\\|JF\\|JO\\|JA\\|J1\\|J2\\|VL\\|IS\\|CP\\|CY\\|PB\\|U[1-5]\\|SN\\|AV\\|M[1-3]\\)  - \\(.\\{1,255\\}\\)" 2 'ris-main-ltd-face t)

;; fields with unlimited length
    ("^\\(T[1-3]\\|TI\\|CT\\|BT\\|N1\\|AB\\|N2\\|AD\\|UR\\|L[1-4]\\)  - \\(.+\\)" 2 'ris-main-face t)

;; all valid tags except TY and ER
    ("^\\(\\(ID\\|T[1-3]\\|TI\\|CT\\|BT\\|A[1-3]\\|AU\\|Y1\\|PY\\|N1\\|AB\\|KW\\|RP\\|SP\\|EP\\|JF\\|JO\\|JA\\|J1\\|J2\\|VL\\|ED\\|IS\\|CP\\|CY\\|PB\\|U[1-5]\\|N2\\|SN\\|AV\\|Y2\\|M[1-3]\\|AD\\|UR\\|L[1-4]\\)  - \\)\\(.+\\)" 1 'ris-tag-face t)

;; all valid types and the end tag
    ("^\\(TY  - \\(ABST\\|ADVS\\|ART\\|BILL\\|BOOK\\|CASE\\|CHAP\\|COMP\\|CONF\\|CTLG\\|DATA\\|ELEC\\|GEN\\|HEAR\\|ICOMM\\|INPR\\|JFULL\\|JOUR\\|MAP\\|MGZN\\|MPCT\\|MUSIC\\|NEWS\\|PAMP\\|PAT\\|PCOMM\\|RPRT\\|SER\\|SLIDE\\|SOUND\\|STAT\\|THES\\|UNBILL\\|UNPB\\|VIDEO\\)\\)$\\|^\\(ER  - \\)$" 0 'ris-tyer-tag-face t))
  "Keyword highlighting specification for `ris-mode'.")


(require 'derived)

;;;###autoload
(define-derived-mode ris-mode fundamental-mode "RIS"
  "A major mode for editing RIS bibliography files."
  (set (make-local-variable 'font-lock-defaults)
       '(ris-font-lock-keywords))
  (set (make-local-variable 'page-delimiter) "^$")
)

(defun duplicate-tag ()
  "Create new line below the current one with the same tag as the current one."
  (interactive "*")
  (let ((start-point (point-marker)))
    (beginning-of-line)
    (if (string= (buffer-substring (+ (point-marker) 2) (+ (point-marker) 6)) "  - ")
	(let ((tag-start (point-marker)))
	  (end-of-line)
	  (newline)
	  (insert-buffer-substring (current-buffer) tag-start (+ tag-start 6)))
      (goto-char start-point)))
)

(defun insert-tag ()
  "Create a new tag from two user-supplied characters."
  (interactive "*")
  (let ((tag-string (completing-read "Tag:" '(("TY  - " 1)
				    ("ER  - " 2)
				    ("ID  - " 3)
				    ("TI  - " 4)
				    ("CT  - " 5)
				    ("BT  - " 6)
				    ("A1  - " 7)
				    ("AU  - " 8)
				    ("Y1  - " 9)
				    ("PY  - " 10)
				    ("N1  - " 11)
				    ("AB  - " 12)
				    ("KW  - " 13)
				    ("RP  - " 14)
				    ("SP  - " 15)
				    ("EP  - " 16)
				    ("JF  - " 17)
				    ("JO  - " 18)
				    ("JA  - " 19)
				    ("J1  - " 20)
				    ("J2  - " 21)
				    ("VL  - " 22)
				    ("T2  - " 23)
				    ("A2  - " 24)
				    ("ED  - " 25)
				    ("IS  - " 26)
				    ("CP  - " 27)
				    ("CY  - " 28)
				    ("PB  - " 29)
				    ("U1  - " 30)
				    ("U2  - " 31)
				    ("U3  - " 32)
				    ("U4  - " 33)
				    ("U5  - " 34)
				    ("T3  - " 35)
				    ("A3  - " 36)
				    ("N2  - " 37)
				    ("SN  - " 38)
				    ("AV  - " 39)
				    ("Y2  - " 40)
				    ("M1  - " 41)
				    ("M2  - " 42)
				    ("M3  - " 43)
				    ("AD  - " 44)
				    ("UR  - " 45)
				    ("L1  - " 46)
				    ("L2  - " 47)
				    ("L3  - " 48)
				    ("L4  - " 49)) nil t))) 
    (if (not (equal (char-before) 10))
	(progn
	  (end-of-line)
	  (newline)))
    (insert tag-string))
)

(defun insert-set ()
  "Create a new skeleton RIS dataset."
  (interactive "*")
  (let ((set-type (completing-read "Type:" '(("ABST" 1)
					     ("ADVS" 2)
					     ("ART" 3)
					     ("BILL" 4)
					     ("BOOK" 5)
					     ("CASE" 6)
					     ("CHAP" 7)
					     ("COMP" 8)
					     ("CONF" 9)
					     ("CTLG" 10)
					     ("DATA" 11)
					     ("ELEC" 12)
					     ("GEN" 13)
					     ("HEAR" 14)
					     ("ICOMM" 15)
					     ("INPR" 16)
					     ("JFULL" 17)
					     ("JOUR" 18)
					     ("MAP" 19)
					     ("MGZN" 20)
					     ("MPCT" 21)
					     ("MUSIC" 22)
					     ("NEWS" 23)
					     ("PAMP" 24)
					     ("PAT" 25)
					     ("PCOMM" 26)
					     ("RPRT" 27)
					     ("SER" 28)
					     ("SLIDE" 29)
					     ("SOUND" 30)
					     ("STAT" 31)
					     ("THES" 32)
					     ("UNBILL" 33)
					     ("UNPB" 34)
					     ("VIDEO" 35)) nil t)))

    (if (not (or (equal (char-before) 10) (equal (char-before) nil)))
	(progn
	  (end-of-line)
	  (newline)))
    (newline)
    (let ((start-point (point-marker)))
      ;; create a skeleton based on the publication type
      (cond ((or (equal set-type "JOUR")
		 (equal set-type "JFULL")
		 (equal set-type "INPR")
		 (equal set-type "MGZN")
		 (equal set-type "NEWS")
		 (equal set-type "ABST"))
	     (insert "TY  - " set-type "\nTI  - \nAU  - \nJO  - \nVL  - \nIS  - \nSP  - \nEP  - \nPY  - \nER  - \n"))
	    ((or (equal set-type "BOOK")
		 (equal set-type "RPRT")
		 (equal set-type "THES"))
	     (insert "TY  - " set-type "\nBT  - \nAU  - \nPY  - \nPB  - \nCY  - \nER  - \n"))
	    ((or (equal set-type "CHAP")
		 (equal set-type "CONF"))
	     (insert "TY  - " set-type "\nTI  - \nAU  - \nBT  - \nA2  - \nPY  - \nPB  - \nCY  - \nER  - \n"))
	    ((equal set-type "SER")
	     (insert "TY  - " set-type "\nTI  - \nAU  - \nBT  - \nA2  - \nT3  - \nA3  - \nPY  - \nPB  - \nCY  - \nER  - \n"))
	    (t (insert "TY  - " set-type "\nER  - \n")
	       (goto-char start-point)
	       (end-of-line)
	       (newline))))
))

; define aliases to better match what the commands do
(defalias 'backward-set 'backward-page)
(defalias 'forward-set 'forward-page)
(defalias 'narrow-to-set 'narrow-to-page)

(define-key ris-mode-map "\C-x[" 'backward-set)
(define-key ris-mode-map "\C-x]" 'forward-set)
(define-key ris-mode-map "\C-xns" 'narrow-to-set)
(define-key ris-mode-map "\M-\r" 'duplicate-tag)
(define-key ris-mode-map "\C-c\C-t" 'insert-tag)
(define-key ris-mode-map "\C-c\C-s" 'insert-set)

(provide 'ris)
;;; ris.el ends here
