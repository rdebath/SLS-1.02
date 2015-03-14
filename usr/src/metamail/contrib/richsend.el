; Emacs mode for sending rich text.
; Adds headers, performs a series of replacements, sends.
; Binds the key C-c C-R to sending mail, converting things
; like *foo* to bold, %foo% to italics, and _foo_ to underline.
; Author: Michael Littman <mlittman@breeze.bellcore.com>
; Caveat: does not recognize font markers *after* punctuation.
; In particular, "*foo*!" works but "*foo!*" does not.

(require 'sendmail)

(define-key mail-mode-map "\C-c\C-r" 'rich-send)

(setq rich-substitutions
      '(
	("<"        "<lt>") ; in case some one sends less-thans.
	("\\B%\\b" "</italic>")	; needs to be first to not get closing tags.
	("\\b%\\B" "<italic>")
	("\\B\\*\\b" "<bold>")
	("\\b\\*\\B" "</bold>")
	("
" "
<nl>")
	("\\B_\\b" "<underline>")
	("\\b_\\B" "</underline>")
	))


(defun perform-rich-sub ()
  "Perform the rich substiution."
  (let ((subs rich-substitutions)
	pat rep
	(top (point)))
    (save-excursion
      (while subs
	(setq pat (car (car subs)))
	(setq rep (car (cdr (car subs))))
	(setq subs (cdr subs))
	(goto-char top)
	(while (re-search-forward pat (point-max) t)
	  (replace-match rep))
	))))

(defun rich-send ()
  "Prepares a buffer for a richtext sendoff.  Does the send-off."
  (interactive)
  (mail-position-on-field "to")
  (insert "\nMime-Version: 1.0\nContent-type: text/richtext")
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n") nil t)
  (perform-rich-sub)
  (mail-send-and-exit nil)
  )

; don't add second set of headers.
; 
; handle *foo.* more reasonably.
; </bold>  -> <lt><italic>bold>
