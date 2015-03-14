;; ISPELL.EL -- Spelling correction interface for GNU EMACS using "ispell".
;; Copyright (C) 1988 Ashwin Ram.
;;
;; This file is not part of the GNU Emacs distribution (yet).
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'ispell)

;; Comments, corrections, and improvements should be sent to:
;;
;;     Ashwin Ram
;;
;;     ARPA:   Ram-Ashwin@cs.yale.edu
;;     UUCP:   {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;;     BITNET: Ram@yalecs


;;; MODIFICATION HISTORY:

;;; Ray Moody       ARPA:    ray@maxwell.physics.purdue.edu
;;;                 UUCP:    ...!pur-phy!ray
;;;                 BITNET:  moody@purccvm  (going away soon....)
;;; Inserted a missing save-excursion in ispell-check-version-compatibility.
;;; 3/30/89.

;;; Ashwin Ram      ARPA:    Ram-Ashwin@cs.yale.edu
;;;                 UUCP:    ...!{decvax, linus, seismo}!yale!Ram-Ashwin
;;;                 BITNET:  Ram@yalecs
;;; Added flag to allow filters that don't produce single column output.
;;; 3/20/88.
;;; Added version compatibility code because of all the confusion.
;;; 11/19/87.
;;; Added variable to control embedded word checking (nice in troff but a pain otherwise).
;;; 10/26/87.
;;; Interactive word completion.
;;; 8/14/87.
;;; Detex before checking spelling.
;;; Made options more mnemonic, prompt and error messages better.
;;; Added highlighting of misspelled word.
;;; Query-replace all occurrences of misspelled word through buffer.
;;; Allow customization of personal dictionary.
;;; Moved temporary file to /tmp.
;;; Added check for dead ispell process to avoid infinite loop.
;;; Avoid repeated querying for same word in same buffer.
;;; 7/6/87.

;;; Walt Buehring
;;; Texas Instruments - Computer Science Center
;;; ARPA:  Buehring%TI-CSL@CSNet-Relay
;;; UUCP:  {smu, texsun, im4u, rice} ! ti-csl ! buehring

;;; ispell-region and associated routines added by
;;; Perry Smith
;;; pedz@bobkat
;;; Tue Jan 13 20:18:02 CST 1987

;;; extensively modified by Mark Davies and Andrew Vignaux
;;; {mark,andrew}@vuwcomp
;;; Sun May 10 11:45:04 NZST 1987

;;; Depends on the ispell program snarfed from MIT-PREP in early 1986.

;;; CUSTOMIZATION:

;;; To fully install this, add this file to your GNU lisp directory and 
;;; compile it with M-X byte-compile-file.  Then add the following to the
;;; appropriate init file:
;;;     (autoload 'ispell-word "ispell" "Check spelling of word at or before point" t)
;;;     (autoload 'ispell-complete-word "ispell" "Complete word at or before point" t)
;;;     (autoload 'ispell-region "ispell" "Check spelling of every word in the region" t)
;;;     (autoload 'ispell-buffer "ispell" "Check spelling of every word in the buffer" t)

;;; You might want to bind ispell-word and ispell-complete word to keys.
;;; You might also want to set the ispell customization variables, e.g.:
;;;     (setq-default ispell-words-have-boundaries t)  ;; To change the default value.
;;; or, (setq ispell-words-have-boundaries t)          ;; To change a local value (perhaps inside a mode hook).
;;; Look at variables starting with "ispell-" to see what you get.

;;; If run on a heavily loaded system, the initial sleep time in ispell-init-process
;;; may need to be increased.

;;; If you count special characters like ' (apostrophe) or - (hyphen) as part of
;;; a word, set their syntax table entries as follows:
;;;     (modify-syntax-entry ?' "w   " ispell-syntax-table)
;;;     (modify-syntax-entry ?- "w   " ispell-syntax-table)
;;; Similarly, if you don't want something to count as a word character, set its
;;; syntax entry to ".   ".

(defconst ispell-version "2.0.02") ;; Check against output of "ispell -v".

(defconst ispell-out-name " *ispell*"
  "Name of the buffer that is associated with the 'ispell' process")

(defconst ispell-temp-name " *ispell-temp*"
  "Name of the temporary buffer that 'ispell-region' uses to hold the
filtered region")

(defvar ispell-program-name "ispell"
  "*Program invoked by ispell-word and ispell-region commands.")

(defvar ispell-dictionary
   nil
   "*Personal dictionary file containing a list of words, one to a line.
If nil, defaults to ispell's normal default (usually ~/.ispell_words).")

(defvar ispell-words-have-boundaries t
   "*If nil, a misspelled word matches embedded words too.  This is useful in
nroff/troff, where a misspelled word may be hidded (e.g., \fIword\fB), and a
pain otherwise.")

(defvar ispell-highlight nil
   "*If non-nil, misspelled words are highlighted.  See the function highlight-region.")

(defvar ispell-syntax-table nil
   "*Syntax table used by ispell to find word boundaries.  You may change syntax
entries to, say, allow ' and - to be part of words, or not, as you prefer.")

(if (null ispell-syntax-table)
    ;; The following assumes that the standard-syntax-table
    ;; is static.  If you add words with funky characters
    ;; to your dictionary, the following may have to change.
    (progn
      (setq ispell-syntax-table (make-syntax-table))
      ;; Make certain characters word constituents
      ;; (modify-syntax-entry ?' "w   " ispell-syntax-table)
      ;; (modify-syntax-entry ?- "w   " ispell-syntax-table)
      ;; Get rid on existing word syntax on certain characters 
      (modify-syntax-entry ?0 ".   " ispell-syntax-table)
      (modify-syntax-entry ?1 ".   " ispell-syntax-table)
      (modify-syntax-entry ?2 ".   " ispell-syntax-table)
      (modify-syntax-entry ?3 ".   " ispell-syntax-table)
      (modify-syntax-entry ?4 ".   " ispell-syntax-table)
      (modify-syntax-entry ?5 ".   " ispell-syntax-table)
      (modify-syntax-entry ?6 ".   " ispell-syntax-table)
      (modify-syntax-entry ?7 ".   " ispell-syntax-table)
      (modify-syntax-entry ?8 ".   " ispell-syntax-table)
      (modify-syntax-entry ?9 ".   " ispell-syntax-table)
      (modify-syntax-entry ?$ ".   " ispell-syntax-table)
      (modify-syntax-entry ?% ".   " ispell-syntax-table)))


(defun ispell-word (&optional quietly)
   "Check spelling of word at or before dot.
If word not found in dictionary, display possible corrections in a window 
and let user select."
   (interactive)
   (let* ((current-syntax (syntax-table))
          start end word poss replace)
      (unwind-protect
            (save-excursion
               (set-syntax-table ispell-syntax-table)            ;; Ensure syntax table is reasonable 
               (if (not (looking-at "\\w"))
                   (re-search-backward "\\w" (point-min) 'stay)) ;; Move backward for word if not already on one
               (re-search-backward "\\W" (point-min) 'stay)      ;; Move to start of word
               (or (re-search-forward "\\w+" nil t)              ;; Find start and end of word
                   (error "No word to check."))
               (setq start (match-beginning 0)
                     end (match-end 0)
                     word (buffer-substring start end)))
         (set-syntax-table current-syntax))
      (ispell-init-process)   ;; erases ispell output buffer
      (or quietly (message "Checking spelling of %s..." (upcase word)))
      (save-excursion
         (set-buffer ispell-out-name)
         (send-string ispell-process (concat word "\n"))
         (while (progn                                         ;; Wait until we have a complete line
                   (goto-char (point-max))
                   (/= (preceding-char) ?\n))
            (accept-process-output ispell-process))
         (goto-char (point-min))
         (setq poss (ispell-parse-output
                       (buffer-substring (point) 
                                         (progn (end-of-line) (point))))))
      (cond ((eq poss t)
             (or quietly (message "Checking spelling of %s... correct" (upcase word))))
            ((stringp poss)
             (or quietly (message "Checking spelling of %s... correct (derived from %s)" (upcase word) (upcase poss))))
;           ((null poss)
;            (or quietly (message "Checking spelling of %s... not found" (upcase word))))
            (t (setq replace (ispell-choose poss word))
               (if replace
                   (progn
                      (goto-char end)
                      (delete-region start end)
                      (insert-string replace)))))
      poss))


(defun ispell-choose (choices word)
  "Display possible corrections from list CHOICES.  Return chosen word
if one is chosen, or nil to keep original WORD."
  (unwind-protect 
      (save-window-excursion
	(let ((count 0)
	      (line 2)
	      (words choices)
	      (window-min-height 2)
	      char num result)
	  (save-excursion
	    (set-buffer (get-buffer-create " *Choices*")) (erase-buffer)
	    (setq mode-line-format (concat "--  %b (Type number to select replacement for "
                                           (upcase word)
                                           ")  --"))
	    (while words
	      (if (<= (+ 7 (current-column) (length (car words)))
		      (window-width))
		  nil
		(insert "\n")
		(setq line (1+ line)))
	      (insert "(" (+ count ?0) ") " (car words) "  ")
	      (setq words (cdr words)
		    count (1+ count)))
            (if (= count 0) (insert "(none)")))
	  (overlay-window line)
	  (switch-to-buffer " *Choices*")
	  (select-window (next-window))
	  (while (eq t
		     (setq result
			   (progn
			     (message "%s: a(dd), c(orrect), r(eplace), space or s(kip) [default], ? (help)" (upcase word)) ; q(uit)
			     (setq char (read-char))
			     (setq num (- char ?0))
			     (cond ((or (= char ? ) (= char ?s))           ; Skip for this invocation
                                    (ispell-ignore-later-occurrences word)
                                    nil)
				   ((= char ?a)                            ; Add to dictionary
 				    (send-string ispell-process
 						 (concat "*" word "\n"))
				    (send-string ispell-process            ; Because ispell isn't reinitialized
						(concat "@" word "\n"))
                                    (ispell-ignore-later-occurrences word)
				    nil)
				   ((= char ?c)                           ; Assume correct but don't add to dict
				    (send-string ispell-process
						(concat "@" word "\n"))
                                    (ispell-ignore-later-occurrences word)
				    nil)
				   ((= char ?r)                           ; Query replace
                                    (ispell-ignore-later-occurrences word)
                                    (read-string (format "Replacement for %s: " (upcase word)) nil))
				   ((and (>= num 0) (< num count))
                                    (ispell-ignore-later-occurrences word)
                                    (nth num choices))
				   ((= char ?\C-r)                        ; Note: does not reset syntax table
				    (save-excursion (recursive-edit)) t)  ; Dangerous
;				   ((= char ?\C-z)
;				    (suspend-emacs) t)
				   ((or (= char help-char) (= char ?\?))
                                    (message "a(dd to dict), c(orrect for this session), r(eplace with your word), or number of replacement")
				    (sit-for 3) t)
				   (t (ding) t))))))
	  result))
    ;; Protected forms...
    (bury-buffer " *Choices*")))

(defun ispell-ignore-later-occurrences (word)
   (if (get-buffer ispell-temp-name)
       (save-excursion
          (set-buffer ispell-temp-name)
          (save-excursion
             (replace-regexp (concat "^" word "$")
                             (concat "+" word))))))

(defun overlay-window (height)
  "Create a (usually small) window with HEIGHT lines and avoid
recentering."
  (save-excursion
    (let ((oldot (save-excursion (beginning-of-line) (dot)))
	  (top (save-excursion (move-to-window-line height) (dot)))
	  newin)
      (if (< oldot top) (setq top oldot))
      (setq newin (split-window-vertically height))
      (set-window-start newin top))))


(defvar ispell-process nil
  "Holds the process object for 'ispell'")

(defun ispell-parse-output (output)
"Parse the OUTPUT string of 'ispell' and return either t for an exact
match, a string containing the root word for a match via suffix
removal, a list of possible correct spellings, or nil for a complete
miss."
  (cond
   ((string= output "*") t)
   ((string= output "#") nil)
   ((string= (substring output 0 1) "+")
    (substring output 2))
   (t
    (let ((choice-list '()))
      (while (not (string= output ""))
	(let* ((start (string-match "[A-z]" output))
	       (end (string-match " \\|$" output start)))
	  (if start
	      (setq choice-list (cons (substring output start end)
				      choice-list)))
	  (setq output (substring output (1+ end)))))
      choice-list))))


(defun ispell-init-process ()
   "Check status of 'ispell' process and start if necessary."
   (if (and ispell-process
            (eq (process-status ispell-process) 'run))
       (save-excursion
          (set-buffer ispell-out-name)
          (erase-buffer))
       (if ispell-process
           (message "Restarting dead ispell process...")
           (message "Starting new ispell process..."))
       (ispell-check-version-compatibility)
       (if (get-buffer ispell-out-name) (kill-buffer ispell-out-name))
       (setq ispell-process
;;           (let ((process-connection-type nil)) ; Don't use pty
                (apply 'start-process "ispell"
                       ispell-out-name ispell-program-name
                       (if ispell-dictionary
                           (list "-p" ispell-dictionary "-A")
                           (list "-A")))
;;              )
             )
       (process-kill-without-query ispell-process)
       (sit-for 3)))

(defun ispell-check-version-compatibility ()
   (let ((buffer (get-buffer-create ispell-out-name)))
      (save-excursion
         (set-buffer buffer)
         (erase-buffer)
         (call-process ispell-program-name nil t nil "-v")
         (goto-char (point-min))
         (if (search-forward ispell-version nil t)
             t
             (if (search-forward "Version " nil 'move)
                 (error "Incompatible version of %s (expected %s, but \"%s -v\" returned %s)"
                        ispell-program-name
                        ispell-version
                        ispell-program-name
                        (buffer-substring (point)
                                          (progn (re-search-forward "[^0-9.]" nil 'move) (- (point) 1))))
                 (error "Old version of %s (doesn't handle -v option)" ispell-program-name))))))

; For TeX users, try "detex -iw" or "detex -iw | tr -cs A-Za-z \012".  Note
; that the output of the filter must be one word per line.

(defvar ispell-filter-hook "tr"
  "*Filter to pass a region through before sending it to ispell.
Must produce output one word per line.  Typically this is set to tr,
deroff, detex, etc.")
(make-variable-buffer-local 'ispell-filter-hook)

(defvar ispell-filter-hook-args '("-cs" "A-Za-z" "\012")
  "*Argument LIST to pass to ispell-filter-hook")
(make-variable-buffer-local 'ispell-filter-hook-args)

(defvar ispell-filter-hook-produces-single-column t
   "*This is nil if your ispell-filter-hook does not output one word per line.")

; This routine has certain limitations brought about by the filter
; hook.  For example, deroff will take ``\fBcat\fR'' and spit out
; ``cat''.  This is hard to search for since word-search-forward will
; not match at all and search-forward for ``cat'' will match
; ``concatenate'' if it happens to occur before.
; `ispell-region' filters the region into `*ispell-temp*', writes the
; buffer to a temporary file, and sends a ``&Include_File&foobar''
; string to the ispell process which is writing into `*ispell*'.
; `ispell-region' then searches `*ispell*' for a spelling error (`#' or
; `&'), checks the `*ispell-temp*' buffer for the misspelled word and
; then skips forward `count' words (the number of correct lines in
; `*ispell*') in the region.  It then searches for the misspelled
; word.  This is not a foolproof heuristic but it is fast and works
; most of the time.
; ... with the unfortunate side-effect that it will sometimes
; pick up the same string in other words too (e.g. if you had the word "food"
; near the "\fIfoo\fP" that you were looking for).
; Another disadvantage is that your "prefobnicator" (deroff or detex or
; whatever) can't delete too many words (and you can't run it through spell(1)
; to cut down on the number of words you want checked) because of the way this
; hack works.
; To get around this, you can setq the variable ispell-words-have-boundaries to
; t (for normal cases) and nil (for embedded-word texts such as for nroff/troff).
; In the first case, your prefobnicator can, for instance, do a "ispell -l" to cut
; down on the number of words you need to "ispell -a" (increasing the program's
; speed considerably).

(defun ispell-region (start end)
   "Check a region for spelling errors interactively.  The variable
which should be buffer or mode specific ispell-filter-hook is called
to filter out text processing commands."
   (interactive "r")
   (let ((this-buf (current-buffer))
         (spell-file (make-temp-name "/tmp/ispell"))
         (spell-buf (get-buffer-create ispell-temp-name))
         (current-syntax (syntax-table))
         (tracker 1)
         word poss replace endbound ispell-out)
      (ispell-init-process)
      (setq ispell-out (get-buffer ispell-out-name))
      (unwind-protect
         (save-excursion
            (save-restriction
               (message "Prefrobnicating...")
               (set-buffer this-buf)
               (narrow-to-region start end)
               (sit-for 0)
               (set-syntax-table ispell-syntax-table)
               (set-buffer spell-buf)
               (erase-buffer)
               (set-buffer this-buf)
               (apply 'call-process-region 
                      (append (list start end ispell-filter-hook nil spell-buf nil)
                              ispell-filter-hook-args))
               (goto-char start)
               (set-buffer spell-buf)
               (if (not ispell-filter-hook-produces-single-column)
                   (call-process-region (point-min) (point-max) "tr" t t nil "-cs" "A-Za-z" "\012")) ;; Make single column.
               (goto-char (point-max))
               (and (/= (preceding-char) ?\n) ; couple of hacks for tr
                    (insert "\n"))
               (goto-char (point-min))
               (while (= (following-char) ?\n)
                  (delete-char 1))
               (write-region (point-min) (point-max) spell-file nil 1)
               (send-string ispell-process 
                            (concat "&Include_File&" spell-file "\n"))
               (message "Looking for a misspelled word... (status: %s)" (process-status ispell-process))
               (sit-for 0)
               (while (and (not (eobp))
                           (eq (process-status ispell-process) 'run))
                  (set-buffer ispell-out)
                  (goto-char (point-max))
                  (beginning-of-line)
                  (setq endbound (point))
                  (goto-char tracker)
                  (if (prog1
                         (not (re-search-forward "^[#&]" endbound 1))
                         (beginning-of-line)
                         (setq count (count-lines tracker (point))
                               tracker (point))
                         (set-buffer spell-buf)
                         (forward-line count)
                         (message "Looking for a misspelled word... (status: %s)"  ;; "(status: %s, at: %s, #%s)"
                                  (process-status ispell-process)
;;                                (upcase (buffer-substring (point) (save-excursion (end-of-line) (point))))
;;                                (count-lines (point-min) (point))
                                  ))
                      (if (not (eobp)) ;; Needed for Sun 260's because of timing errors.
                          (accept-process-output ispell-process))
                      (setq word (buffer-substring (point)
                                                   (progn (end-of-line) (point))))
                      (forward-char 1)
                      (set-buffer ispell-out) ; (goto-char tracker)
                      (setq poss (ispell-parse-output
                                  (buffer-substring (point) 
                                                    (progn (end-of-line) (point)))))
                      (forward-char 1)
                      (setq tracker (point))
                      (set-buffer this-buf)
                      (re-search-forward "\\W*\\(\\w+\\)" nil t (1- count)) ; get close
                      (if (string= "+" (substring word 0 1))
                          (search-forward (substring word 1) nil t)
                          (if (re-search-forward (if ispell-words-have-boundaries
                                                  (concat "\\b" (regexp-quote word) "\\b")
                                                  (regexp-quote word))
                                                 nil t)
                              (let ((end (point)))
                                 (search-backward word nil t)
                                 (save-excursion
                                    (let ((start (point)))
                                       (recenter (/ (window-height) 2)) ; show word in context
                                       (sit-for 0)
                                       (if ispell-highlight (highlight-region start end))
                                       (setq replace (ispell-choose poss word))
                                       (if ispell-highlight (unhighlight-region start end))))
                                 (if replace
                                     (save-excursion
                                        (query-replace-regexp (if ispell-words-have-boundaries
                                                                  (concat "\\b" (regexp-quote word) "\\b")
                                                                  (regexp-quote word))
                                                              replace))))
                              (message "Can't find %s in original text -- Any key to continue" word)
                              (read-char)
;;                            (and (= ?\C-z (read-char)) (suspend-emacs))
                              )
                          (message "Looking for a misspelled word... (status: %s)" (process-status ispell-process))
                          (sit-for 0))
                      (set-buffer spell-buf)))))
         (if (eq (process-status ispell-process) 'run)
             (message "Done.")
             (message "Warning - ispell process died."))
         (set-syntax-table current-syntax)
         (and (file-exists-p spell-file)
              (delete-file spell-file)))))

(defun ispell-buffer () 
  "Check the current buffer for spelling errors interactively.  The variable
which should be buffer or mode specific ispell-filter-hook is called to
filter out text processing commands."
  (interactive)
  (ispell-region (point-min) (point-max)))


; In case you already have this, comment out the following:

(defun highlight-region (p1 p2)
   "Highlight the current region.  You may have to rewrite this for your
particular terminal."
   (interactive "r")
   (let ((s (buffer-substring p1 p2))
         (inverse-video t))
      (delete-region p1 p2)
      (sit-for 0)
      (insert s)
      (sit-for 0)))

(defun unhighlight-region (p1 p2)
   "Unhighlight the current region.  See highlight-region."
   (interactive "r")
   (let ((s (buffer-substring p1 p2))
         (inverse-video nil))
      (delete-region p1 p2)
      (sit-for 0)
      (insert s)
      (sit-for 0)))


;; Interactive word completion.
;; Some code and many ideas tweaked from Peterson's spell-dict.el.
;; Ashwin Ram <Ram@yale>, 8/14/87.

(defvar ispell-words-file "/usr/dict/words"
   "*File used for ispell-complete-word command.  On 4.3bsd systems, try
using \"/usr/dict/web2\" for a larger selection.  Apollo users may want to
try \"/sys/dict\".")

(defun ispell-complete-word ()
   "Look up word before point in dictionary (see the variable
ispell-words-file) and try to complete it.  If in the middle of a word,
replace the entire word."
   (interactive)
   (let* ((current-word (buffer-substring (save-excursion (backward-word 1) (point))
                                          (point)))
          (in-word (looking-at "\\w"))
          (possibilities (save-excursion
                            (set-buffer (get-buffer-create ispell-temp-name))
                            (erase-buffer)
                            (or (string= current-word "") ; Will give you every word in the dictionary!
                                (call-process "look" nil t nil "-df" current-word ispell-words-file))
                            (if (> (buffer-size ) 0)
                                (ispell-parse-output (buffer-string))
                                '())))
          (replacement (ispell-choose possibilities current-word)))
      (cond (replacement
             (if in-word (kill-word 1))        ;; Replace the whole word.
             (search-backward current-word)
             (replace-match replacement)))))   ;; To preserve capitalization etc.

