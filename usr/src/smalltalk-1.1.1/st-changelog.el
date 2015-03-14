;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Change log support routines for Smalltalk.
;;;
;;; Steve Byrne, February 1989.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (C) 1990, 1991 Free Software Foundation, Inc.
;;; Written by Steve Byrne.
;;; 
;;; This file is part of GNU Smalltalk.
;;;  
;;; GNU Smalltalk is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 1, or (at your option) any later 
;;; version.
;;;
;;; GNU Smalltalk is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with GNU Smalltalk; see the file COPYING.  If not, write to the Free
;;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst smalltalk-date-column 13)
(defconst smalltalk-change-column 26)

(defun smalltalk-create-change-log (&optional position-ok)
  "Inserts a changelog template into the current buffer.
Only Smalltalk style changelogs are supported right now."
  (interactive)
  (if (not position-ok)
      (progn
	(message "Move the cursor to where the change log should be")
	(let ((old-cc (key-binding "\C-c\C-c")))
	  (unwind-protect
	      (progn 
		(local-set-key "\C-c\C-c" 'exit-recursive-edit)
		(recursive-edit))
	    (local-set-key "\C-c\C-c" old-cc)))
	(beginning-of-line)
	))
  (insert-string
"\"
|     Change Log
| ============================================================================
| Author       Date       Change 
\"
")
  )

(defun smalltalk-add-change-log-entry ()
  "Allows the user to add a change log entry to the current
buffer.  If there is no change log currently present, the user is informed 
of this fact, and is allowed to position the cursor where the change log
should be placed."
  (interactive)
  (save-excursion
    (while (not (smalltalk-find-change-log))
      (message "Change log not found") (sit-for 5)
      (smalltalk-create-change-log)
    )
    (smalltalk-add-change-log-mode)
    ))



(defun smalltalk-install-change-log-functions ()
  "Adds the change log functions to the current set of character bindings."
  (define-key smalltalk-mode-map "\C-c\C-c" 'smalltalk-add-change-log-entry)
  (define-key smalltalk-mode-map "\C-cC" 'smalltalk-create-change-log)
  )

(defun smalltalk-find-change-log ()
  "Locates the buffer's change log and positions the cursor where the next
entry should appear.  Returns non-nil if the changelog is found, and nil if
it isn't found."
  (beginning-of-buffer)
  (if (re-search-forward "^\|     Change Log" nil t)
      (progn
	(forward-line 3)
	t))
  )

(defun smalltalk-add-change-log-mode ()
  "Go into add change log mode."
  (let ((old-return (key-binding "\r"))
	(old-^c^c (key-binding "\C-c\C-c"))
	(mode-name mode-name)
	(indent-line-function 'smalltalk-changelog-mode-indent)
	(fill-prefix nil)
	(fill-column 79)
	(auto-fill-hook 'do-auto-fill))
    (unwind-protect
	(progn 
	  (local-set-key "\r" 'newline-and-indent)
	  (local-set-key "\C-c\C-c" 'exit-recursive-edit)
	  (setq mode-name "Changelog")
	  (smalltalk-init-change-log-entry)
	  (save-excursion
	    (recursive-edit))
	  (smalltalk-clean-up-after-changing)
	  )
      (local-set-key "\r" old-return)
      (local-set-key "\C-c\C-c" old-^c^c)
      )
    ))

(defun smalltalk-init-change-log-entry ()
  "Inserts the initial change log entry stuff, which
is the user name and the date."
  (insert-string "| " (user-login-name))
  (indent-to smalltalk-date-column)
  (insert-string (string-date))
  (indent-to smalltalk-change-column)
  (save-excursion
    (insert-string "\n|\n")
    )
  )


(defun string-date ()
  "Returns a string date of the form dd mmm yy for the
current date."
  (let ((now (current-time-string)))
    (concat
     (substring now 8 10)			;the day
     " "
     (substring now 4 7)			;the month
     " "
     (substring now 22 24)		;the year
     )))

(defun smalltalk-changelog-mode-indent ()
  "Insert the comment continuation character, and tab to the change log
text column."
  (interactive)
  (insert-string "|")
  (indent-to change-column))

;;; Yuck... I don't like the way I wrote this...I'll bet there is
;;; a cleaner way...

(defun smalltalk-clean-up-after-changing ()
  "Performs cleanup operations such as deleting extraneous blank lines
at the end of a change log entry.  Point is at the start of the text
for the current change log entry."
  (let (dot (num-blanks 0))
    (while (not (smalltalk-line-is-blank))
      (forward-line))
    (setq dot (point))
    (beginning-of-line)
    (if (< (point) dot)			;our first blank line is the
					;change log line, so fake
					;an extra line to be removed
	(setq num-blanks 1))
    (setq dot (point))
    (while (smalltalk-line-is-blank t)
      (setq num-blanks (1+ num-blanks))
      (forward-line))
    (if (> num-blanks 1)
	(progn
	  (goto-char dot)
	  (kill-line (1- num-blanks))))
    ))

(defun smalltalk-line-is-blank (&optional last-isnt-blank)
  "Returns t if the line consists of the comment char followed
by a /, or nothing in the columns past change-column"
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "\"") (not last-isnt-blank))
	  ((looking-at " \|[ \t]*$") t)
	  (t (end-of-line)
	     (<= (current-column) change-column)))
    )
  )
