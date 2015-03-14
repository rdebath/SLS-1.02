;;;; Rexx Funktionen für CLISP
;;;; Jörg Höhle 27.11.1992

(in-package "LISP")
(export '(rexx-run-command rexx-send-command rexx-wait-sent-command rexx-do-command
          rexx-loop
)        )

;;;; Interface:
;;;
;;; (rexx-loop)
;;;
;;; (rexx-run-command name) -> null
;;;
;;; (rexx-do-command name) -> (rc result)
;;;
;;; (rexx-send-command name) -> handle
;;; (rexx-wait-sent-command handle) -> (rc result)
;;;
;;; name kann ein String (Kommandos in einem String)
;;;      oder ein Pathname (File mit Kommandos) sein.
;;; rc ist der ARexx return Code.
;;; result ist der ARexx return String, nur wenn rc gleich 0.

;;;; ===========================================================================
;;;; Implementation:

(in-package "SYSTEM")

;;; Wir benutzen folgende Funktionen aus REXX.D:
;;; (system::%rexx-wait-input) -> boolean
;;; (system::%rexx-get) -> (handle string) oder (handle rc [result])
;;; (system::%rexx-reply handle rc result) -> null
;;; (system::%rexx-put name :result :string :token :async :io) -> handle
;;; Keyword-Argumente result, string, token, async, io sind Flags:
;;; result: Antwort merken
;;; string: Argument als Befehle statt 1. Token als Dateiname verstehen
;;; token: Tokens erzeugen
;;; async: An AREXX statt REXX Port schicken, für asynchrone Bearbeitung
;;; io: E/A Kanäle übernehmen

;; Wir verwalten eine Aliste  msg -> reply  aller weggeschickten und noch
;; unbearbeiteten Messages und ihrer Antworten (Listen (Code String);
;; NIL für noch unbeantwortete Messages). Beim Abschicken einer Message
;; bekommen wir ein "handle" als Erkennungszeichen (diese werden
;; mit EQUAL verglichen).

(defvar *rexx-outmsg-list* '())

(defun rexx-add-index (handle &optional (value nil))
  (push (cons handle value) *rexx-outmsg-list*)
)
(defun rexx-find-index (handle)
  (assoc handle *rexx-outmsg-list* :test #'equal)
)
(defun rexx-delete-entry (acons)
  (setq *rexx-outmsg-list* (delete acons *rexx-outmsg-list* :test #'eq))
)

;; Startet ein REXX-Kommando, ohne jedoch jetzt auf dessen Beendigung zu warten.
;; Liefert das Handle, damit man später noch auf seine Beendigung warten kann,
;; jedoch NIL, falls das Kommando nicht erfolgreich abgeschickt werden konnte.
(defun rexx-send-command (name &rest keys &key result string token async io)
  (declare (ignore result string token async io))
  "Starts asynchronous execution of a rexx command."
  (let ((handle (apply #'%rexx-put name keys)))
    (when handle
      (rexx-add-index handle)
      handle
) ) )

;; Wartet auf die nächste Nachricht und liefert ihr Handle.
(defun rexx-next-event ()
  (loop ; es fehlt derzeit die Möglichkeit, parallel *STANDARD-INPUT* zu lesen
    ; nächste Nachricht lesen und auswerten, falls vorhanden:
    (let ((event (%rexx-get)))
      (when event (return event))
    )
    ; auf die nächste Nachricht warten:
    (%rexx-wait-input)
) )

;; "Hauptschleife": Wartet auf Nachrichten, interpretiert diese als Fragen,
;; wertet sie aus und schickt die Antwort zurück. Die Schleife wird beendet,
;; wenn eine Antwort auf Handle wait-for kommt.
(defun rexx-loop (&optional wait-for)
  "Rexx driver loop. Optional message to wait for."
  (driver ; driver oder einfaches loop ??
    #'(lambda ()
        (let ((event (rexx-next-event))) ; nächste Nachricht
          (cond ((numberp (second event)) ; ein Reply (handle rc [result])
                 (let ((index (rexx-find-index (first event))))
                   (when index (setf (cdr index) (rest event))) ; Antwort abspeichern
                 )
                 (when (equal (first event) wait-for)
                   (return-from rexx-loop (rest event)) ; evtl. Schleife beenden
                ))
                (t ; ein Befehl (handle string)
                 (let ((result nil))
                   ; warum funktioniert (catch 'debug ...) nicht??
                   (unwind-protect
                     (block try-rep ; Fehlerbehandlung
                       (setq result
                         (with-output-to-string (stream)
                           (let ((*error-handler*
                                   #'(lambda (&rest error-args)
                                       (declare (ignore error-args))
                                       (return-from try-rep nil)
                                ))   )
                             ; primitives READ-EVAL-PRINT :
                             (princ (eval (read-from-string (second event)))
                                    stream
                     ) ) ) ) )
                     (%rexx-reply (first event) (if result 0 5) result) ; portabler machen!??
                )) )
) )   ) ) )

;; Wartet auf die Beendigung eines REXX-Kommandos.
;; Liefert die Antwort (eine Liste (Code String)).
(defun rexx-wait-sent-command (handle)
  "Waits for command termination."
  (loop
    (let ((done (rexx-find-index handle)))
      (unless done
        (error #+DEUTSCH "Kein Warten auf ~S möglich."
               #+ENGLISH "No waiting for ~S possible."
               #+FRANCAIS "Pas d'attente de ~S possible."
               handle
      ) )
      (when (cdr done) (rexx-delete-entry done) (return (cdr done)))
      (rexx-loop handle) ; auf die Antwort warten, Aussprung oben
) ) )

;; Startet ein REXX-Kommando und wartet, bis es beendet ist.
;; Liefert die Antwort (eine Liste (Code String)),
;; jedoch NIL, falls das Kommando nicht erfolgreich abgeschickt werden konnte.
(defun rexx-do-command (name &rest keys &key &allow-other-keys)
  "Executes command, waiting for result."
  (let ((handle (apply #'rexx-send-command name keys)))
    (when handle
      (rexx-wait-sent-command handle)
) ) )

;; Startet ein REXX-Kommando, ohne jedoch auf dessen Beendigung zu warten
;; (asynchron).
;; Liefert /=NIL, falls das Kommando erfolgreich abgeschickt wurde.
(defun rexx-run-command (name &key string token)
  "Runs a rexx command asynchronously, no return code."
  (if (rexx-do-command name :string string :token token :async t) t nil)
)

