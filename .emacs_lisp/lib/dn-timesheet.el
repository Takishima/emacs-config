;;; dn-timesheet.el --- Daily check-in and check-out in an Org timesheet -*- lexical-binding: t -*-

;;; Commentary:

;; Keeps a timesheet in `dn-timesheet-file', one Org heading per ISO week
;; and one subheading per day, clocked with Org's clock:
;;
;;   * 2025-W25
;;   ** Monday 2025-06-23
;;   :LOGBOOK:
;;   CLOCK: [2025-06-23 Mon 09:00]--[2025-06-23 Mon 18:00] =>  9:00
;;   :END:
;;
;; `dn-timesheet-check-in' clocks in on today's heading, creating the
;; file, the week and the day as needed.  `dn-timesheet-check-out' clocks
;; out again.  Both take a prefix argument to enter the time by hand,
;; for when the check-in or check-out was forgotten.  A clock left open
;; by a previous Emacs session is picked up as well, so check-out works
;; after a restart even without `org-clock-persist'.
;;
;; `dn-timesheet-report' refreshes the clock tables in the file and
;; `dn-timesheet-status' shows the time worked today.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'org-duration)

(defgroup dn-timesheet nil
  "Daily check-in and check-out in an Org timesheet."
  :group 'dn
  :prefix "dn-timesheet-")

(defcustom dn-timesheet-file (locate-user-emacs-file "timesheet.org")
  "Org file holding the timesheet.  Created on the first check-in."
  :type 'file
  :group 'dn-timesheet)

(defcustom dn-timesheet-week-heading-format "%G-W%V"
  "Format, for `format-time-string', of the heading of a week.
The default gives the ISO week, e.g. 2025-W25."
  :type 'string
  :group 'dn-timesheet)

(defcustom dn-timesheet-day-heading-format "%A %Y-%m-%d"
  "Format, for `format-time-string', of the heading of a day.
The day heading is looked up by its exact text, so changing this
format only affects days created afterwards."
  :type 'string
  :group 'dn-timesheet)

(defcustom dn-timesheet-save t
  "Save `dn-timesheet-file' after each check-in and check-out."
  :type 'boolean
  :group 'dn-timesheet)

(defcustom dn-timesheet-clock-tables '(thisweek thismonth)
  "Blocks of the clock tables inserted in a new timesheet.
Each element is a value of the :block parameter of a clocktable."
  :type '(repeat symbol)
  :group 'dn-timesheet)

(defcustom dn-timesheet-file-header
  "#+title: Timesheet
#+startup: overview

Docs: https://orgmode.org/manual/Clocking-Work-Time.html

* Help

- Check in:    =M-x dn-timesheet-check-in=, =C-u= to enter the time
- Check out:   =M-x dn-timesheet-check-out=, =C-u= to enter the time
- Today:       =M-x dn-timesheet-status=
- Report:      =M-x dn-timesheet-report= refreshes the tables below
- Fix a time:  edit the CLOCK line, then =C-c C-c= on it recalculates it

"
  "Text inserted at the top of a new timesheet, before the report."
  :type 'string
  :group 'dn-timesheet)

(defun dn-timesheet-buffer ()
  "Return the buffer visiting `dn-timesheet-file', creating the file if needed."
  (let* ((file (expand-file-name dn-timesheet-file))
         (new (not (file-exists-p file)))
         (buffer (progn
                   (unless (file-directory-p (file-name-directory file))
                     (make-directory (file-name-directory file) t))
                   (find-file-noselect file))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (when new
        (dn-timesheet--insert-template)
        (save-buffer)))
    buffer))

(defun dn-timesheet--insert-template ()
  "Insert the header and report section of a new timesheet in the current buffer."
  (goto-char (point-min))
  (insert dn-timesheet-file-header)
  (insert "* Report\n\n")
  (dolist (block dn-timesheet-clock-tables)
    (insert (format "#+BEGIN: clocktable :scope file :maxlevel 3 :block %s\n#+END:\n\n"
                    block))))

(defun dn-timesheet--find-heading (title level &optional start bound)
  "Move to the LEVEL heading TITLE between START and BOUND.
Return the position of the heading, or nil if there is none."
  (goto-char (or start (point-min)))
  (let ((re (format "^\\*\\{%d\\} +%s[ \t]*$" level (regexp-quote title))))
    (when (re-search-forward re bound t)
      (goto-char (pos-bol)))))

(defun dn-timesheet--day-heading (time)
  "Return the text of the heading of the day of TIME."
  (format-time-string dn-timesheet-day-heading-format time))

(defun dn-timesheet--find-day (time)
  "Move to the heading of the day of TIME and return its position, or nil."
  (dn-timesheet--find-heading (dn-timesheet--day-heading time) 2))

(defun dn-timesheet--goto-day (time)
  "Move to the heading of the day of TIME, creating the week and the day if needed.
Return the position of the heading."
  (let ((week (format-time-string dn-timesheet-week-heading-format time))
        (day (dn-timesheet--day-heading time)))
    (unless (dn-timesheet--find-heading week 1)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (unless (or (= (point) (point-min)) (looking-back "\n\n" (- (point) 2)))
        (insert "\n"))
      (let ((pos (point)))
        (insert "* " week "\n")
        (goto-char pos)))
    (let ((week-start (point))
          (week-end (save-excursion (org-end-of-subtree t t) (point))))
      (unless (dn-timesheet--find-heading day 2 week-start week-end)
        (goto-char week-end)
        (unless (bolp) (insert "\n"))
        (let ((pos (point)))
          (insert "** " day "\n")
          (goto-char pos))))
    (point)))

(defun dn-timesheet--open-clock (day-pos)
  "Return the start time of an unfinished CLOCK line in the entry at DAY-POS.
Return nil if the entry has no open clock.  Point is left on the CLOCK line
when one is found."
  (goto-char day-pos)
  (let ((end (save-excursion (outline-next-heading) (point)))
        (re (concat "^[ \t]*" org-clock-string " *\\(\\[[^]]+\\]\\)[ \t]*$")))
    (when (re-search-forward re end t)
      (goto-char (pos-bol))
      (org-time-string-to-time (match-string 1)))))

(defun dn-timesheet--running-here-p (&optional day-pos)
  "Return non-nil if the running clock is in the timesheet file.
With DAY-POS, the clock must also be on the heading at that position."
  (and (org-clocking-p)
       (marker-buffer org-clock-marker)
       (equal (buffer-file-name (marker-buffer org-clock-marker))
              (expand-file-name dn-timesheet-file))
       (or (null day-pos)
           (= (marker-position org-clock-hd-marker) day-pos))))

(defun dn-timesheet--read-time (prompt)
  "Read a date and time with PROMPT and return it as a time value."
  (org-read-date t t nil prompt))

(defun dn-timesheet--maybe-save ()
  "Save the timesheet if `dn-timesheet-save' is non-nil."
  (when (and dn-timesheet-save (buffer-modified-p))
    (save-buffer)))

(defun dn-timesheet--clock (time)
  "Format TIME as a wall-clock time, for messages."
  (format-time-string "%H:%M" time))

;;;###autoload
(defun dn-timesheet-check-in (&optional time)
  "Clock in on today's heading of the timesheet.
With a prefix argument, or a TIME value, clock in at that time instead
of now.  Do nothing if today already has an open clock."
  (interactive
   (list (when current-prefix-arg (dn-timesheet--read-time "Check in at: "))))
  (let ((time (or time (current-time))))
    (with-current-buffer (dn-timesheet-buffer)
      (save-excursion
        (save-restriction
          (widen)
          (let* ((day (dn-timesheet--goto-day time))
                 (open (dn-timesheet--open-clock day)))
            (cond
             ((dn-timesheet--running-here-p day)
              (message "Already checked in at %s"
                       (dn-timesheet--clock org-clock-start-time)))
             (open
              (message "Already checked in at %s (clock left open)"
                       (dn-timesheet--clock open)))
             (t
              (goto-char day)
              (org-clock-in nil time)
              (message "Checked in at %s" (dn-timesheet--clock time)))))
          (dn-timesheet--maybe-save))))))

;;;###autoload
(defun dn-timesheet-check-out (&optional time)
  "Clock out of the timesheet.
With a prefix argument, or a TIME value, clock out at that time instead
of now.  Also closes a clock left open by a previous Emacs session."
  (interactive
   (list (when current-prefix-arg (dn-timesheet--read-time "Check out at: "))))
  (let ((time (or time (current-time))))
    (with-current-buffer (dn-timesheet-buffer)
      (save-excursion
        (save-restriction
          (widen)
          (let ((open (when-let* ((day (dn-timesheet--find-day time)))
                        (dn-timesheet--open-clock day))))
            (cond
             ((dn-timesheet--running-here-p)
              (let ((org-clock-out-remove-zero-time-clocks t))
                (org-clock-out nil nil time))
              (message "Checked out at %s" (dn-timesheet--clock time)))
             (open
              (end-of-line)
              (insert "--" (format-time-string (org-time-stamp-format t t) time))
              (org-clock-update-time-maybe)
              (message "Checked out at %s (clock left open since %s)"
                       (dn-timesheet--clock time) (dn-timesheet--clock open)))
             ((org-clocking-p)
              (user-error "The running clock is not in %s" dn-timesheet-file))
             (t
              (user-error "Not checked in"))))
          (dn-timesheet--maybe-save))))))

;;;###autoload
(defun dn-timesheet-toggle ()
  "Check in if not checked in today, check out otherwise."
  (interactive)
  (with-current-buffer (dn-timesheet-buffer)
    (if (save-excursion
          (save-restriction
            (widen)
            (or (dn-timesheet--running-here-p)
                (when-let* ((day (dn-timesheet--find-day (current-time))))
                  (dn-timesheet--open-clock day)))))
        (dn-timesheet-check-out)
      (dn-timesheet-check-in))))

(defun dn-timesheet--today-minutes ()
  "Return the minutes clocked today, including the running clock.
Point must be on today's heading."
  (let ((day (point))
        (minutes (org-clock-sum-current-item)))
    (if (dn-timesheet--running-here-p day)
        (+ minutes (floor (float-time (time-since org-clock-start-time)) 60))
      minutes)))

;;;###autoload
(defun dn-timesheet-status ()
  "Show the time worked today, including the running clock."
  (interactive)
  (with-current-buffer (dn-timesheet-buffer)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((day (dn-timesheet--find-day (current-time)))
               (minutes (if day (dn-timesheet--today-minutes) 0)))
          (message "Today: %s, %s"
                   (org-duration-from-minutes minutes 'h:mm)
                   (cond
                    ((and day (dn-timesheet--running-here-p day))
                     (format "checked in since %s"
                             (dn-timesheet--clock org-clock-start-time)))
                    ((and day (dn-timesheet--open-clock day))
                     (format "checked in since %s (clock left open)"
                             (dn-timesheet--clock (dn-timesheet--open-clock day))))
                    (t "checked out"))))))))

;;;###autoload
(defun dn-timesheet-report ()
  "Visit the timesheet and refresh its clock tables."
  (interactive)
  (let ((buffer (dn-timesheet-buffer)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          ;; 27:00 rather than 1d 3:00, which reads badly next to days off.
          (let ((org-duration-format 'h:mm))
            (org-update-all-dblocks))))
      (dn-timesheet--maybe-save))
    (pop-to-buffer buffer)
    (widen)
    (when (dn-timesheet--find-heading "Report" 1)
      (org-fold-show-entry)
      (org-fold-show-children))))

;;;###autoload
(defun dn-timesheet-open ()
  "Visit the timesheet at today's heading, or at its end if there is none yet."
  (interactive)
  (pop-to-buffer (dn-timesheet-buffer))
  (widen)
  (if (dn-timesheet--find-day (current-time))
      (progn (org-fold-show-context)
             (org-fold-show-entry))
    (goto-char (point-max))))

(provide 'dn-timesheet)

;;; dn-timesheet.el ends here
