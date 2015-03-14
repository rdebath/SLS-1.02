;;;
;;; Smalltalk mode for Gnu Emacs
;;;

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

(require 'shell)

(defvar smalltalk-name-regexp "[A-Za-z][A-Za-z0-9]*"
  "A regular expression that matches a Smalltalk identifier")

(defvar smalltalk-name-chars "a-zA-Z0-9"
  "The collection of character that can compose a Smalltalk identifier")

(defvar smalltalk-whitespace " \t\n\f")

(defvar smalltalk-mode-abbrev-table nil
  "Abbrev table in use in smalltalk-mode buffers.")
(define-abbrev-table 'smalltalk-mode-abbrev-table ())

;;; this hack was to play around with adding Smalltalk-specific menu items
;;; to the Emacstool on the Sun.
(if (featurep 'sun-mouse)
    (let (new-menu i)
      (defmenu smalltalk-menu
	("Smalltalk")
	("Do it"))
      (setq new-menu (make-vector (1+ (length emacs-menu)) nil))
      (aset new-menu 0 (aref emacs-menu 0))
      (setq i 1)
      (while (< i (length emacs-menu))
	(aset new-menu (1+ i) (aref emacs-menu i))
	(setq i (1+ i)))
      (aset new-menu 1 '("Smalltalk" . smalltalk-menu))
      (setq emacs-menu new-menu)
      )
  )

(defvar smalltalk-mode-map nil "Keymap used in Smalltalk mode.")
(if smalltalk-mode-map
    ()
  (setq smalltalk-mode-map (make-sparse-keymap))
  (define-key smalltalk-mode-map "\t" 'smalltalk-tab)
  (define-key smalltalk-mode-map "\177" 'backward-delete-char-untabify)
  (define-key smalltalk-mode-map "\n" 'smalltalk-newline-and-indent)
  (define-key smalltalk-mode-map "\C-\M-a" 'smalltalk-begin-of-defun)
  (define-key smalltalk-mode-map "\C-\M-f" 'smalltalk-forward-sexp)
  (define-key smalltalk-mode-map "\C-\M-b" 'smalltalk-backward-sexp)
  (define-key smalltalk-mode-map "!" 	'smalltalk-bang)
  (define-key smalltalk-mode-map ":"	'smalltalk-colon)
  (define-key smalltalk-mode-map "\M-\t"	'smalltalk-reindent)
;; just examples
;;  (define-key c-mode-map "{" 'electric-c-brace)
;;  (define-key c-mode-map "\e\C-h" 'mark-c-function)
;;  (define-key c-mode-map "\e\C-q" 'indent-c-exp)
  )

(defvar smalltalk-mode-syntax-table nil
  "Syntax table in use in smalltalk-mode buffers.")

(if smalltalk-mode-syntax-table
    ()
  (setq smalltalk-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\"" smalltalk-mode-syntax-table)
  ;; GNU Emacs is deficient: there seems to be no way to have a comment char
  ;; that is both the start and end character.  This is going to cause
  ;; me great pain.
  (modify-syntax-entry ?\" "\"" smalltalk-mode-syntax-table)
  (modify-syntax-entry ?+ "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?- "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?* "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?/ "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?= "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?% "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?< "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?> "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?& "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?$ "\\" smalltalk-mode-syntax-table)
  (modify-syntax-entry ?# "'" smalltalk-mode-syntax-table)
  (modify-syntax-entry ?| "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?_ "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?\\ "." smalltalk-mode-syntax-table)
  (modify-syntax-entry ?! "." smalltalk-mode-syntax-table)
  )

(defconst smalltalk-indent-amount 4
  "*'Tab size'; used for simple indentation alignment.")

(autoload 'smalltalk-install-change-log-functions "st-changelog")

(defun stm ()
  (smalltalk-mode))

(defun smalltalk-mode ()
  "Major mode for editing Smalltalk code.
Comments are delimited with \" ... \".
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Of special interest are the commands for interacting with a live Smalltalk 
session: 
\\[mst]
    Invoke the Smalltalk interactor, which basically keeps the current buffer
    in one window, and creates another window with a running Smalltalk in it.
    The other window behaves essentially like a shell-mode window when the
    cursor is in it, but it will receive the operations requested when the
    interactor related commands are used.

\\[smalltalk-doit]
    interactively evaluate the expression that the cursor is in in a Smalltalk
    mode window, or with an argument execute the region as smalltalk code

\\[smalltalk-compile]
    compile the method definition that the cursor is currently in.

\\[smalltalk-snapshot]
    produce a snapshot binary image of the current working Smalltalk system.
    Useful to do periodically as you define new methods to save the state of
    your work.

\\{smalltalk-mode-map}

Turning on Smalltalk mode calls the value of the variable
smalltalk-mode-hook with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map smalltalk-mode-map)
  (setq major-mode 'smalltalk-mode)
  (setq mode-name "Smalltalk")
  (setq local-abbrev-table smalltalk-mode-abbrev-table)
  (set-syntax-table smalltalk-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'smalltalk-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "\"")
  (make-local-variable 'comment-end)
  (setq comment-end "\"")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\" *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'smalltalk-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)	;for interactive f-b sexp
  (smalltalk-install-change-log-functions)
  (run-hooks 'smalltalk-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in Smalltalk code
;; based on its context.
(defun smalltalk-comment-indent ()
  (if (looking-at "^\"")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.

(defun smalltalk-indent-line ()
  (indent-relative-maybe)
  )

(defun smalltalk-previous-nonblank-line ()
  (forward-line -1)
  (while (and (not (bobp))
	      (looking-at "^[ \t]*$"))
    (forward-line -1))
  )

(defun smalltalk-tab ()
  (interactive)
  (let (col)
    ;; round up, with overflow
    (setq col (* (/ (+ (current-column) smalltalk-indent-amount)
		    smalltalk-indent-amount)
		 smalltalk-indent-amount))
  (indent-to-column col)
  ))

(defun smalltalk-begin-of-defun ()
  (interactive)
  (let ((parse-sexp-ignore-comments t) here)
    ;; this routine is fooled by !s in character strings.
    (setq here (point))
    (if (search-backward "!" nil 'to-end)
	(forward-char 1))
    (smalltalk-forward-whitespace)
    ;; yeah, yeah, it's crude, but it gets the job done.
    (if (= here (point))		;do it again
	(progn
	  (if (search-backward "!" nil 'to-end 2)
	      (forward-char 1))
	  (smalltalk-forward-sexp 1)
	  (backward-sexp 1)))
  ))

(defun smalltalk-forward-whitespace ()
  "Skip white space and comments forward, stopping at end of buffer
or non-white space, non-comment character"
  (while (looking-at (concat "[" smalltalk-whitespace "\"]"))
    (skip-chars-forward smalltalk-whitespace)
    (if (= (following-char) ?\")
	(forward-sexp 1)))
  )

(defun smalltalk-backward-whitespace ()
  "Like forward whitespace only going towards the start of the buffer"
  (while (progn (skip-chars-backward smalltalk-whitespace)
		(= (preceding-char) ?\"))
    (backward-sexp 1))
  )

(defun smalltalk-forward-sexp (n)
  (interactive "p")
  (let (i)
    (cond ((null parse-sexp-ignore-comments)
	   (forward-sexp n))
	  ((< n 0)
	   (smalltalk-backward-sexp (- n)))
	  (t
	   (while (> n 0)
	     (smalltalk-forward-whitespace)
	     (forward-sexp 1)
	     (setq n (1- n))
	     )
	   )
	  )
    )
  )

(defun smalltalk-backward-sexp (n)
  (interactive "p")
  (let (i)
    (cond ((null parse-sexp-ignore-comments)
	   (backward-sexp n))
	  ((< n 0)
	   (smalltalk-forward-sexp (- n)))
	  (t
	   (while (> n 0)
	     (smalltalk-backward-whitespace)
	     (backward-sexp 1)
	     (setq n (1- n))
	     )
	  )))
  )

(defun smalltalk-reindent ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space)
  (delete-char -1)
  (smalltalk-newline-and-indent 1))

(defun smalltalk-newline-and-indent (levels)
  "Called basically to do newline and indent.  Sees if the current line is a 
new statement, in which case the indentation is the same as the previous 
statement (if there is one), or is determined by context; or, if the current 
line is not the start of a new statement, in which case the start of the 
previous line is used, except if that is the start of a new line in which case
it indents by smalltalk-indent-amount."
  (interactive "p")
  (let (needs-indent indent-amount done c state start-of-line
		     (parse-sexp-ignore-comments t))
    (save-excursion
      (save-restriction
	(save-excursion
	  (smalltalk-backward-whitespace)
	  (if (or (bobp)
		  (= (preceding-char) ?!))
	      (setq indent-amount 0))
	  )
	(if (null indent-amount)
	    (progn
	      (smalltalk-narrow-to-method)
	      (setq state (parse-partial-sexp (point-min) (point)))
	      (if (nth 3 state)		;in a string or comment
		  (cond ((= (nth 3 state) ?\") ;in a comment
			 (save-excursion
			   (smalltalk-backward-comment)
			   (setq indent-amount (1+ (current-column)))
			   ))
			((= (nth 3 state) ?')	;in a string
			 (setq indent-amount 0))
			)
		(narrow-to-paren state)
		(smalltalk-backward-whitespace)
		(cond ((bobp)			;must be first statment in block or exp
		       (if (nth 1 state)	;we're in a paren exp
			   (setq indent-amount (current-column))
			 ;; we're top level
			 (setq indent-amount smalltalk-indent-amount)))
		      ((= (preceding-char) ?.) ;at end of statement
		       (smalltalk-find-statement-begin)
		       (setq indent-amount (current-column)))
		      ((= (preceding-char) ?:)
		       (beginning-of-line)
		       (smalltalk-forward-whitespace)
		       (setq indent-amount (+ (current-column)
					      smalltalk-indent-amount))
		       )
		      ((= (preceding-char) ?>) ;maybe <primitive: xxx>
		       (setq orig (point))
		       (backward-char 1)
		       (smalltalk-backward-whitespace)
		       (skip-chars-backward "0-9")
		       (smalltalk-backward-whitespace)
		       (if (= (preceding-char) ?:)
			   (progn
			     (backward-char 1)
			     (skip-chars-backward "a-zA-Z")
			     (if (looking-at "primitive:")
				 (progn
				   (smalltalk-backward-whitespace)
				   (if (= (preceding-char) ?<)
				       (setq indent-amount (1- (current-column))))
				   )
			       )
			     )
			 )
		       (if (null indent-amount)
			   (progn
			     (goto-char orig)
			     (smalltalk-find-statement-begin)
			     (setq indent-amount (+ (current-column)
						    smalltalk-indent-amount))
			     )
			 )
		       )
		      (t			;must be a statement continuation
		       (save-excursion
			 (beginning-of-line)
			 (setq start-of-line (point)))
		       (smalltalk-find-statement-begin)
		       (setq indent-amount (+ (current-column)
					      smalltalk-indent-amount))
		       )
		      )
		)
	      ))
	)
      )
    (newline)
    (delete-horizontal-space)		;remove any carried-along whites
    (indent-to indent-amount)
    ))

(defun smalltalk-find-statement-begin ()
  "Leaves the point at the first non-blank, non-comment character of a new
statement.  If begininning of buffer is reached, then the point is left there.
This routine only will return with the point pointing at the first non-blank
on a line; it won't be fooled by multiple statements on a line into stopping
prematurely."
  (let (start)
    (if (= (preceding-char) ?.)		;if we start at eos
	(backward-char 1))		;we find the begin of THAT stmt
    (while (and (null start) (not (bobp)))
      (smalltalk-backward-whitespace)
      (if (= (preceding-char) ?.)
	  (let (saved-point)
	    (setq saved-point (point))
	    (smalltalk-forward-whitespace)
	    (if (smalltalk-white-to-bolp)
		(setq start (point))
	      (goto-char saved-point)
	      (smalltalk-backward-sexp 1))
	    )
	(smalltalk-backward-sexp 1)
	)
      )
    (if (null start)
      (progn
	(goto-char (point-min))
	(smalltalk-forward-whitespace)
	(setq start (point))))
  start))
    

;;; hold on to this code for a little bit, but then flush it
;;;	  
;;;	  ;; not in a comment, so skip backwards for some indication
;;;	  (smalltalk-backward-whitespace)
;;;	  (if (bobp)
;;;	      (setq indent-amount smalltalk-indent-amount)
;;;	    (setq c (preceding-char))
;;;	    (cond ((eq c ?.)		;this is a new statement
;;;		   (smalltalk-backward-statement)
;;;		   (setq indent-amount (current-column)))
;;;		  ((memq c '(?|
;;;			     
;;;			     (smalltalk-narrow-to-method)
;;;			     
;;;			     (smalltalk-backward-whitespace)
;;;			     (setq c (preceding-char))
;;;			     (cond
;;;			      ((memq c '(?. ?| ?\[ ?\( )) (setq done t))
;;;			      ((eq c ?:)
;;;			       (backward-char 1)
;;;			       (skip-chars-backward "a-zA-Z0-9")
;;;			       (setq indent-amount (current-column)))
;;;			      (t
;;;			       (smalltalk-backward-sexp 1)))
;;;			     )
;;;			 
;;;			 )
;;;		   )
;;;		  (if indent-amount
;;;		      (save-excursion
;;;			(beginning-of-line)
;;;			(delete-horizontal-space)
;;;			(indent-to indent-amount))
;;;		    )
;;;		  (insert last-command-char)
;;;		  ))
	  
(defun narrow-to-paren (state)
  (let ((paren-addr (nth 1 state))
	start c done)
    (if (not paren-addr) nil
      (save-excursion
	(goto-char paren-addr)
	(setq c (following-char))
	(cond ((eq c ?\()
	       (setq start (1+ (point))))
	      ((eq c ?\[)
	       (setq done nil)
	       (forward-char 1)
	       (while (not done)
		 (smalltalk-forward-whitespace)
		 (setq c (following-char))
		 (cond ((eq c ?:)
			(smalltalk-forward-sexp 1))
		       ((eq c ?|)
			(forward-char 1) ;skip vbar
			(smalltalk-forward-whitespace) ;move to non-blank
			(setq done t))	;and leave
		       (t
			(setq done t))
		       )
		 )
	       (setq start (point))
	       )
	      )
	)
      (narrow-to-region start (point))
      )
    )
  )



(defun smalltalk-colon ()
  "Possibly reindents a line when a colon is typed.
If the colon appears on a keyword that's at the start of the line (ignoring
whitespace, of course), then the previous line is examined to see if there
is a colon on that line, in which case this colon should be aligned with the 
left most character of that keyword.  This function is not fooled by nested 
expressions."
  (interactive)
  (let (needs-indent indent-amount done c
		     (parse-sexp-ignore-comments t))
    (save-excursion
      (skip-chars-backward "A-Za-z0-9")
      (if (and (looking-at smalltalk-name-regexp) (not (bolp)))
	  (setq needs-indent (smalltalk-white-to-bolp))
	)
      )
    (if needs-indent
	(progn 
	  (save-excursion
	    (save-restriction
	      (smalltalk-narrow-to-method)
	      (beginning-of-line)
	      (while (and (not done)
			  (not (bobp)))
		(smalltalk-backward-whitespace)
		(setq c (preceding-char))
		(cond
		 ((memq c '(?. ?| ?\[ ?\( ?^)) (setq done t))
		 ((eq c ?:)
		  (backward-char 1)
		  (skip-chars-backward "a-zA-Z0-9")
		  (setq indent-amount (current-column)))
		 (t
		  (smalltalk-backward-sexp 1)))
		)
	      
	      )
	    )
	  (if indent-amount
	      (save-excursion
		(beginning-of-line)
		(delete-horizontal-space)
		(indent-to indent-amount))
	    )
	  )
      )
    (expand-abbrev)			;I don't think this is the "correct"
					;way to do this...I suspect that
					;some flavor of "call interactively"
					;is better.
    (insert last-command-char)
    ))

(defun smalltalk-narrow-to-method ()
  "Narrows the buffer to the contents of the method, exclusive of the
method selector and temporaries."
  (let ((end (point))
	(parse-sexp-ignore-comments t)
	done)
    (save-excursion
      (smalltalk-begin-of-defun)
      (if (looking-at "[a-zA-z]")		;either unary or keyword msg
	  ;; or maybe an immediate expression...
	  (progn
	    (forward-sexp)
	    (if (= (following-char) ?:)	;keyword selector
		(progn
		  (backward-sexp 1)	;setup for common code
		  (while (not done)
		    (if (not (looking-at "[a-zA-Z]"))
			(setq done t)
		      (skip-chars-forward smalltalk-name-chars)
		      (if (= (following-char) ?:)
			  (progn
			    (forward-char)
			    (smalltalk-forward-sexp 1)
			    (smalltalk-forward-whitespace))
			(setq done t)
			(backward-sexp 1))
		      )
		    )
		  )
	      ;; else maybe just a unary selector or maybe not
	      ;; see if there's stuff following this guy on the same line
	      (let (here eol-point)
		(setq here (point))
		(end-of-line)
		(setq eol-point (point))
		(goto-char here)
		(smalltalk-forward-whitespace)
		(if (< (point) eol-point) ;if there is, we're not a method
					; (a heuristic guess)
		    (beginning-of-line)
		  (goto-char here)	;else we're a unary method (guess)
		  )
		)
	      )
	    )
	
	;; this must be a binary selector
	(skip-chars-forward (concat "^" smalltalk-whitespace))
	(smalltalk-forward-whitespace)
	(skip-chars-forward smalltalk-name-chars)) ;skip over operand
      (skip-chars-forward smalltalk-whitespace)
      (if (= (following-char) ?|)	;scan for temporaries
	  (progn
	    (forward-char)
	    (while (/= (following-char) ?|)
	      (smalltalk-forward-whitespace)
	      (skip-chars-forward smalltalk-name-chars)
	      )
	    (forward-char)		;skip over trailing |
	    )
	)
      (narrow-to-region (point) end)
      )
    )
  )

(defun smalltalk-white-to-bolp ()
  "Returns T if from the current position to beginning of line is whitespace.
Whitespace is defined as spaces, tabs, and comments."
  (let (done is-white line-start-pos)
    (save-excursion
      (save-excursion
	(beginning-of-line)
	(setq line-start-pos (point)))
      (while (not done)
	(skip-chars-backward " \t")
	(cond ((bolp)
	       (setq done t)
	       (setq is-white t))
	      ((= (char-after (1- (point))) ?\")
	       (backward-sexp)
	       (if (< (point) line-start-pos) ;comment is multi line
		   (setq done t)
		   )
	       )
	      (t
	       (setq done t))
	      )
	)
      is-white)
    ))


(defun smalltalk-bang ()
  (interactive)
  (insert "!")
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]+!")
	(delete-horizontal-space))
    )
  )


(defun smalltalk-backward-comment ()
  (search-backward "\"")	;find its start
  (while (= (preceding-char) ?\") ;skip over doubled ones
    (backward-char 1)
    (search-backward "\""))
  )


(defun st-test ()			;just an experimental testing harness
  (interactive)
  (let (l end)
    (setq end (point))
    (beginning-of-defun)
    (setq l (parse-partial-sexp (point) end nil nil nil))
    (message "%s" (prin1-to-string l)) (read-char)
    (message "depth %s" (nth 1 l)) (goto-char (nth 1 l)) (read-char)
    (message "last sexp %s" (nth 2 l)) (goto-char (nth 2 l)) (read-char)
    (message "lstsx %s stp %s com %s quo %s pdep %s"
	   (nth 3 l)
	   (nth 4 l)
	   (nth 5 l)
	   (nth 6 l)
	   (nth 7 l))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GNU Emacs Smalltalk interactor mode
;;; (initial cut)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *smalltalk-process* nil)
(defvar mst-args "-Vp")

(define-key smalltalk-mode-map "\C-cc" 	'smalltalk-compile)
(define-key smalltalk-mode-map "\C-cd" 	'smalltalk-doit)
(define-key smalltalk-mode-map "\C-ce" 	'smalltalk-eval-region)
(define-key smalltalk-mode-map "\C-cf" 	'smalltalk-filein)
(define-key smalltalk-mode-map "\C-cm" 	'mst)
(define-key smalltalk-mode-map "\C-cp" 	'smalltalk-print)
(define-key smalltalk-mode-map "\C-cq" 	'smalltalk-quit)
(define-key smalltalk-mode-map "\C-cs" 	'smalltalk-snapshot)



(defun mst (args)
  (interactive (list (if (null current-prefix-arg)
			 mst-args
			 (read-string "Invoke Smalltalk: " mst-args))))
  (setq mst-args args)
  (switch-to-buffer-other-window
   (make-mst "mst" mst-args))
  (setq *smalltalk-process* (get-buffer-process (current-buffer)))
  )

(defun make-mst (name &rest switches)
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status size)
    (setq proc (get-buffer-process buffer))
    (if proc (setq status (process-status proc)))
    (save-excursion
      (set-buffer buffer)
      ;;    (setq size (buffer-size))
      (if (memq status '(run stop))
	  nil
	(if proc (delete-process proc))
	(setq proc (apply 'start-process name buffer
			  (concat exec-directory "env")
			  ;; I'm choosing to leave these here
			  (format "TERMCAP=emacs:co#%d:tc=unknown:"
				  (screen-width))
			  "TERM=emacs"
			  "EMACS=t"
			  "-"
			  "mst"
			  switches))
	(setq name (process-name proc)))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (set-process-filter proc 'mst-filter)
      (mst-mode))
    buffer))

(defun mst-filter (process string)
  "Make sure that the window continues to show the most recently output 
text."
  (let (where)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (while (setq where (string-match "\C-a" string))
      (setq string (concat (substring string 0 where)
			   (substring string (1+ where))))
      (setq mode-status "idle"))
    (insert string)
    (if (process-mark process)
	(set-marker (process-mark process) (point-max)))
    )
;;  (if (eq (process-buffer process)
;;	  (current-buffer))
;;      (goto-char (point-max)))
;  (save-excursion
;      (set-buffer (process-buffer process))
;      (goto-char (point-max))
;;      (set-window-dot (get-buffer-window (current-buffer)) (point-max))
;      (sit-for 0))
  (let ((buf (current-buffer)))
    (set-buffer (process-buffer process))
    (goto-char (point-max)) (sit-for 0)
    (set-window-dot (get-buffer-window (current-buffer)) (point-max))
    (set-buffer buf))
  ))


(defun mst-mode ()
  "Major mode for interacting Smalltalk subprocesses.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{shell-mode-map}

Entry to this mode calls the value of mst-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
mst-mode-hook is called after shell-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (setq mode-line-format
	'("" mode-line-modified mode-line-buffer-identification "   "
	  global-mode-string "   %[(" mode-name ": " mode-status
	  "%n" mode-line-process ")%]----" (-3 . "%p") "-%-"))
  (setq major-mode 'mst-mode)
  (setq mode-name "Smalltalk")
;;  (setq mode-line-process '(": %s"))
  (use-local-map shell-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'mode-status)
  (setq mode-status "starting-up")
  (run-hooks 'shell-mode-hook 'mst-mode-hook))



(defun smalltalk-eval-region (start end &optional label)
  "Evaluate START to END as a Smalltalk expression in Smalltalk window.
If the expression does not end with an exclamation point, one will be
added (at no charge)."
  (interactive "r")
  (let (str)
    (setq str (buffer-substring start end))
    (save-excursion
      (goto-char (max start end))
      (smalltalk-backward-whitespace)
      (if (/= (preceding-char) ?!)	;canonicalize
	  (setq str (concat str "!")))
      )
    (send-to-smalltalk str (or label "eval"))
    )
  )


(defun smalltalk-doit (use-region)
  (interactive "P")
  (let (start end rgn)
    (if use-region
	(progn
	  (setq start (min (mark) (point)))
	  (setq end (max (mark) (point)))
	  )
      (setq rgn (smalltalk-bound-expr))
      (setq start (car rgn)
	    end (cdr rgn))
      )
    (smalltalk-eval-region start end "doIt")
    )
  )

(defun smalltalk-bound-expr ()
  "Returns a cons of the region of the buffer that contains a smalltalk expression.
It's pretty dumb right now...looks for a line that starts with ! at the end and
a non-white-space line at the beginning, but this should handle the typical
cases nicely."
  (let (start end here)
    (save-excursion
      (setq here (point))
      (re-search-forward "^!")
      (setq end (point))
      (beginning-of-line)
      (if (looking-at "^[^ \t\"]")
	  (progn
	    (goto-char here)
	    (re-search-backward "^[^ \t\"]")
	    (while (looking-at "^$")		;this is a hack to get around a bug
	      (re-search-backward "^[^ \t\"]");with GNU Emacs's regexp system
	      )
	    )
	)
      (setq start (point))
      (cons start end)
      )
    )
  )

(defun smalltalk-compile (use-region)
  (interactive "P")
  (let (str start end rgn)
    (if use-region
	(progn
	  (setq start (min (point) (mark)))
	  (setq end (max (point) (mark)))
	  (setq str (buffer-substring start end))
	  (save-excursion
	    (goto-char end)
	    (smalltalk-backward-whitespace)
	    (if (/= (preceding-char) ?!)	;canonicalize
		(setq str (concat str "!")))
	    )
	  (send-to-smalltalk str "compile"))
      (setq rgn (smalltalk-bound-method))
      (setq str (buffer-substring (car rgn) (cdr rgn)))
      (save-excursion
	(re-search-backward "^![ \t]*[A-Za-z]")
	(setq start (point))
	(forward-char 1)
	(search-forward "!")
	(setq end (point)))
      (setq str (concat (buffer-substring start end) "\n\n" str "!"))
      (send-to-smalltalk str "compile")
    )
  )
  )


(defun smalltalk-bound-method ()
  (let (start end)
    (save-excursion
      (re-search-forward "^!")
      (setq end (point)))
    (save-excursion
      (re-search-backward "^[^ \t\"]")
      (while (looking-at "^$")		;this is a hack to get around a bug
	(re-search-backward "^[^ \t\"]");with GNU Emacs's regexp system
	)
      (setq start (point)))
    (cons start end))
  )


(defun smalltalk-snapshot (&optional snapshot-name)
  (interactive (if current-prefix-arg
		   (list (setq snapshot-name (expand-file-name (read-file-name "Snapshot to: "))))))
  (if snapshot-name
      (send-to-smalltalk (format "Smalltalk snapshot: '%s'!" "Snapshot"))
  (send-to-smalltalk "Smalltalk snapshot!" "Snapshot"))
  )

(defun smalltalk-print (start end)
  (interactive "r")
  (let (str)
    (setq str (buffer-substring start end))
    (save-excursion
      (goto-char (max start end))
      (smalltalk-backward-whitespace)
      (if (= (preceding-char) ?!)	;canonicalize
	  (setq str (buffer-substring (min start end)  (point)))
	)
      (setq str (format "(%s) printNl!" str))
      (send-to-smalltalk str "print")
      )
    )
  )


(defun smalltalk-quit ()
  (interactive)
  (send-to-smalltalk "Smalltalk quitPrimitive!" "Quitting"))

(defun smalltalk-filein (filename)
  (interactive "fSmalltalk file to load: ")
  (send-to-smalltalk (format "FileStream fileIn: '%s'!"
			     (expand-file-name filename))
		     "fileIn")
  )

(defun send-to-smalltalk (str &optional mode)
  (let (temp-file buf)
    (setq temp-file (concat "/tmp/" (make-temp-name "mst")))
    (save-excursion
      (setq buf (get-buffer-create " zap-buffer "))
      (set-buffer buf)
      (erase-buffer)
      (princ str (current-buffer))
      (write-region (point-min) (point-max) temp-file nil 'no-message)
      )
    (kill-buffer buf)
    (if mode
	(progn
	  (save-excursion
	    (set-buffer (process-buffer *smalltalk-process*))
	    (setq mode-status mode))
	  ))
    (switch-to-buffer-other-window (process-buffer *smalltalk-process*))
    (goto-char (point-max))
    (newline)
    (other-window 1)
      ;;(sit-for 0)
    (process-send-string *smalltalk-process*
			 (concat "FileStream fileIn: '" temp-file "'!\n"))
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GNU Emacs hooks for invoking Emacs on Smalltalk methods
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(setq command-switch-alist
      (append '(("-smalltalk" . smalltalk-edit))
	      command-switch-alist))


(defun smalltalk-edit (rest)
  (let (file pos done)
    (setq file (car command-line-args-left))
    (setq command-line-args-left
	  (cdr command-line-args-left))
    (setq pos (string-to-int (car command-line-args-left)))
    (setq command-line-args-left
	  (cdr command-line-args-left))
    (find-file (expand-file-name file))
    (goto-char pos)
    )
  )
