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

(autoload 'my-compile "my-c-mode")

(defvar bison-mode-syntax-table nil
  "Syntax table used while in text mode.")

(if bison-mode-syntax-table
    ()
  (setq bison-mode-syntax-table (copy-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?\" ".   " bison-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " bison-mode-syntax-table)
  (modify-syntax-entry ?' "w   " bison-mode-syntax-table))

(defvar bison-mode-map nil "")
(if bison-mode-map
    ()
  (setq bison-mode-map (make-sparse-keymap))
  (define-key bison-mode-map "\t" 'tab-to-tab-stop)
  (define-key bison-mode-map "\e." 'find-nonterminal)
  (define-key bison-mode-map "\C-xg" 'goto-state)
  (define-key bison-mode-map "\C-cm" 'my-compile)
  )



(defun bison-mode ()
  "Major mode for editing Bison code intended for humans to read.\\{bison-mode-map}
Turning on bison-mode calls the value of the variable bison-mode-hook,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map bison-mode-map)
  (setq mode-name "Bison")
  (setq major-mode 'Bison-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table bison-mode-syntax-table)
  (run-hooks 'bison-mode-hook))

(defun goto-state (n)
  (interactive "NState: ")
  (goto-char (point-min))
  (re-search-forward (concat "^state " (int-to-string n) "\n\n"))
  (forward-line -2)
  (recenter 0)
  )

(defun find-nonterminal ()
  (interactive)
  (let (word)
    (setq word (select-current-word))
    (goto-char (point-min))
    (re-search-forward (concat "^[ \t]*" word "[ \t]*:"))
    (recenter 0)
    ))

(defun select-current-word ()
  "Returns a string that's the entire word that the point is in."
  (let (start end)
    (if (= (char-syntax (char-after (point))) ?w)
	(forward-sexp))
    (setq end (point))
    (backward-sexp)
    (setq start (point))
    (buffer-substring start end))
  )
