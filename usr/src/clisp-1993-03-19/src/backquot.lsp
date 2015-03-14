;;;; Backquote-Readmacro
;;;; Michael Stoll
;;;; umgeschrieben im Juli/August von Bruno Haible
;;;; rekursives Backquote 16.-17.8.1989
;;;; an die übliche Semantik für rekursives Backquote angepaßt am 24.5.1992

(in-package "SYSTEM")

(proclaim '(special *backquote-level*))
; NIL oder Anzahl der erlaubten Kommata
; Wird beim Top-Level-Einsprung in den Reader an NIL gebunden.

(proclaim '(special *nsplice-fun*))
(setq *nsplice-fun* 'NCONC) ; Funktion, die ein NSPLICE ausführt
; (Wird an 'APPEND gebunden für die Produktion der Ausgabe-Form in
; verschachtelten Backquotes.)

; Bug: Bei verschachtelten Backquotes werden manche Teilformen mehrfach
; ausgewertet (nämlich z.B. in der ersten Evaluation Formen, die fürs
; Ausgeben vor der zweiten Evaluation nötig sind) und sollten deshalb
; seiteneffektfrei sein.

(defun \`-reader (stream char)
  (declare (ignore char))
  (let* ((*backquote-level* (1+ (or *backquote-level* 0)))
         (skel (read stream t nil t))
         (form (list 'BACKQUOTE
                     (remove-backquote-third skel)
                     (backquote-1 (unquote-level skel))
        ))     )
    (when (= *backquote-level* 1) (setq form (elim-unquote-dummy form)))
    form
) )

(defun \,-reader (stream char &aux (c (peek-char nil stream)))
  (declare (ignore char))
  (cond ((null *backquote-level*)
         (error #+DEUTSCH "~S: Komma darf nur innerhalb von Backquote auftreten."
                #+ENGLISH "~S: comma is illegal outside of backquote"
                #+FRANCAIS "~S : Une virgule ne peut apparaître qu'à l'intérieur d'un «backquote»."
                'read
        ))
        ((zerop *backquote-level*)
         (error #+DEUTSCH "~S: Es dürfen nicht mehr Kommata als Backquotes auftreten."
                #+ENGLISH "~S: more commas out than backquotes in, is illegal"
                #+FRANCAIS "~S : Il ne peut y avoir plus de virgules que de «backquote»."
                'read
        ))
        (t (let ((*backquote-level* (1- *backquote-level*)))
             (cond ((eql c #\@)
                    (read-char stream)
                    (list 'SPLICE (list 'UNQUOTE (read stream t nil t)))
                   )
                   ((eql c #\.)
                    (read-char stream)
                    (list 'NSPLICE (list 'UNQUOTE (read stream t nil t)))
                   )
                   (t (list 'UNQUOTE (read stream t nil t)))
) )     )  ) )

;(set-macro-character #\` #'\`-reader)
;(set-macro-character #\, #'\,-reader)

; Ausgabe von ...                              als ...
; (backquote original-form [expanded-form])    `original-form
; (splice (unquote form))                      ,@form
; (splice form)                                ,@'form
; (nsplice (unquote form))                     ,.form
; (nsplice form)                               ,.'form
; (unquote form)                               ,form

;(defmacro backquote (original-form expanded-form)
;  (declare (ignore original-form))
;  expanded-form
;)

(defun remove-backquote-third (skel)
  (cond ((atom skel)
         (if (simple-vector-p skel)
           (map 'vector #'remove-backquote-third skel)
           skel
        ))
        ((and (eq (car skel) 'BACKQUOTE) (consp (cdr skel)))
         (list 'BACKQUOTE (second skel)) ; ohne drittes Element der Liste
        )
        (t (cons (remove-backquote-third (car skel))
                 (remove-backquote-third (cdr skel))
) )     )  )

; ersetzt UNQUOTE-DUMMY durch UNQUOTE.
(defun elim-unquote-dummy (skel)
  (if (atom skel)
    (cond ((eq skel 'UNQUOTE-DUMMY) 'UNQUOTE)
          ((simple-vector-p skel) (map 'vector #'elim-unquote-dummy skel))
          (t skel)
    )
    (let* ((car (car skel)) (newcar (elim-unquote-dummy car))
           (cdr (cdr skel)) (newcdr (elim-unquote-dummy cdr)))
      (if (and (eq car newcar) (eq cdr newcdr))
        skel
        (cons newcar newcdr)
) ) ) )

;; wandelt im "Skelett" skel alle UNQUOTEs der Stufe level+1 (d.h. innerhalb
;; von level-fachem UNQUOTE) in UNQUOTE-VALUE um.
(defun unquote-level (skel &optional (level 0))
  (if (atom skel)
    (if (simple-vector-p skel)
      (map 'vector #'(lambda (subskel) (unquote-level subskel level)) skel)
      skel
    )
    ; skel ist ein Cons
    (cond ((and (eq (first skel) 'UNQUOTE) (consp (rest skel)))
           (if (zerop level)
             (list 'UNQUOTE-VALUE (second skel))
             (let ((weiteres (unquote-level (second skel) (1- level))))
               ; Vereinfache (UNQUOTE weiteres):
               (if (and (consp weiteres) (eq (car weiteres) 'QUOTE)
                        (consp (second weiteres))
                        (eq (car (second weiteres)) 'UNQUOTE-VALUE)
                   )
                 ; (UNQUOTE (QUOTE (UNQUOTE-VALUE ...))) -> (UNQUOTE-VALUE ...)
                 (second weiteres)
                 (list 'UNQUOTE weiteres)
          )) ) )
          ((and (eq (first skel) 'BACKQUOTE) (consp (rest skel)))
           (list* 'BACKQUOTE
                  (unquote-level (second skel) (1+ level))
                  (if (consp (cddr skel))
                    (list (unquote-level (third skel) level))
                    nil
          ))      )
          (t ; CAR-CDR-Rekursion
            (cons (unquote-level (car skel) level)
                  (unquote-level (cdr skel) level)
) ) )     ) )

;; stellt fest, ob eine Form zu mehreren expandieren kann.
(defun splicing-p (skel)
  (and (consp skel)
       (let ((h (first skel))) (or (eq h 'splice) (eq h 'nsplice)))
) )

;; wandelt "Skelett" skel (mit UNQUOTE-VALUEs etc.) in passenden Code um.
(defun backquote-1 (skel)
  (if (atom skel)
    (cond ((or (and (symbolp skel) (constantp skel) (eq skel (symbol-value skel)))
               (numberp skel)
               (stringp skel)
               (bit-vector-p skel)
           )
           ; Konstanten, die zu sich selbst evaluieren, bleiben unverändert
           skel
          )
          ((simple-vector-p skel)
           ; Vektoren:
           ; #(... item ...) -> (VECTOR ... item ...)
           ; #(... ,@form ...) ->
           ;   (MULTIPLE-VALUE-CALL #'VECTOR ... (VALUES-LIST form) ...)
           (if (some #'splicing-p skel)
             (list* 'MULTIPLE-VALUE-CALL
                    '(FUNCTION VECTOR)
                    (map 'list
                         #'(lambda (subskel)
                             (if (splicing-p subskel)
                               (if (and (consp (second subskel))
                                        (eq (first (second subskel)) 'UNQUOTE-VALUE)
                                   )
                                 (list 'VALUES-LIST (backquote-1 (second subskel)))
                                 ; SPLICE bzw. NSPLICE für später aufheben
                                 (backquote-cons (backquote-1 (first subskel))
                                                 (backquote-1 (rest subskel))
                               ) )
                               (list 'VALUES (backquote-1 subskel))
                           ) )
                         skel
             )      )
             (let ((einzelne (map 'list #'backquote-1 skel)))
               (if (every #'constantp einzelne)
                 ; alle Teile konstant -> sofort zusammensetzen
                 (list 'QUOTE (map 'vector #'eval einzelne))
                 (cons 'VECTOR einzelne)
             ) )
          ))
          (t
           ; sonstige Atome A in 'A umwandeln
           (list 'QUOTE skel)
    )     )
    (cond ((eq (first skel) 'unquote-value)
           ; ,form im richtigen Level wird zu form
           (second skel)
          )
          ((eq (first skel) 'splice)
           ; ,@form ist verboten
           (error #+DEUTSCH "Die Syntax ,@form ist nur innerhalb von Listen erlaubt."
                  #+ENGLISH "The syntax ,@form is valid only in lists"
                  #+FRANCAIS "La syntaxe ,@form n'est permise qu'à l'intérieur d'une liste."
          ))
          ((eq (first skel) 'nsplice)
           ; ,.form ist verboten
           (error #+DEUTSCH "Die Syntax ,.form ist nur innerhalb von Listen erlaubt."
                  #+ENGLISH "The syntax ,.form is valid only in lists"
                  #+FRANCAIS "La syntaxe ,.form n'est permise qu'à l'intérieur d'une liste."
          ))
          ((and (eq (first skel) 'backquote) (consp (rest skel)))
           ; verschachtelte Backquotes
           (list* 'LIST
                  ''BACKQUOTE
                  (let ((*nsplice-fun* 'APPEND)) (backquote-1 (second skel)))
                  (if (consp (cddr skel))
                    (list (backquote-1 (third skel)))
                    nil
          ))      )
          ((and (consp (first skel))
                (eq (first (first skel)) 'splice)
           )
           ; (  ... ,@EXPR ...  ) behandeln
           (if (and (consp (second (first skel)))
                    (eq (first (second (first skel))) 'UNQUOTE-VALUE)
               )
             (backquote-append (backquote-1 (second (first skel)))
                               (backquote-1 (rest skel))
             )
             ; SPLICE für später aufheben
             (backquote-cons
               (backquote-cons (backquote-1 (first (first skel)))
                               (backquote-1 (rest (first skel)))
               )
               (backquote-1 (rest skel))
          )) )
          ((and (consp (first skel))
                (eq (first (first skel)) 'nsplice)
           )
           ; (  ... ,.EXPR ...  ) behandeln
           (if (and (consp (second (first skel)))
                    (eq (first (second (first skel))) 'UNQUOTE-VALUE)
               )
             (let ((erstes (backquote-1 (second (first skel))))
                   (weiteres (backquote-1 (rest skel))))
               ; (NCONC erstes weiteres) vereinfachen
               (cond ((null weiteres)
                      ; (NCONC expr NIL) -> (NCONC expr) -> expr
                      (if (splicing-p erstes)
                        (list *nsplice-fun* erstes)
                        erstes
                     ))
                     ((and (consp weiteres) (eq (first weiteres) *nsplice-fun*))
                      ; (NCONC expr (NCONC . rest)) -> (NCONC expr . rest)
                      (list* *nsplice-fun* erstes (rest weiteres)) )
                     (t (list *nsplice-fun* erstes weiteres))
             ) )
             ; NSPLICE für später aufheben
             (backquote-cons
               (backquote-cons (backquote-1 (first (first skel)))
                               (backquote-1 (rest (first skel)))
               )
               (backquote-1 (rest skel))
          )) )
          (t ; sonst CAR und CDR zusammensetzen
             (backquote-cons (backquote-1 (first skel)) (backquote-1 (rest skel)))
          )
) ) )

; Liefert die Form, die das Append-Ergebnis der Formen erstes und weiteres
; ergibt.
(defun backquote-append (erstes weiteres)
  ; (APPEND erstes weiteres) vereinfachen
  (cond ((null weiteres)
         ; (APPEND expr NIL) -> (APPEND expr) -> expr
         (if (splicing-p erstes)
           (list 'APPEND erstes)
           erstes
        ))
        ((and (consp weiteres) (eq (first weiteres) 'append))
         ; (APPEND expr (APPEND . rest)) -> (APPEND expr . rest)
         (list* 'APPEND erstes (rest weiteres)) )
        (t (list 'APPEND erstes weiteres))
) )

; Liefert die Form, die das Cons-Ergebnis der Formen erstes und weiteres
; ergibt.
(defun backquote-cons (erstes weiteres)
  ; (CONS erstes weiteres) vereinfachen
  (cond ((and (constantp erstes) (constantp weiteres))
         ; beide Teile konstant -> sofort zusammensetzen
         (setq erstes (eval erstes))
         (setq weiteres (eval weiteres))
         (list 'QUOTE
           (cons (if (eq erstes 'UNQUOTE) 'UNQUOTE-DUMMY erstes) weiteres)
        ))
        ((null weiteres)
         ; (CONS expr NIL) -> (LIST expr)
         (list 'LIST erstes)
        )
        ((atom weiteres)
         (list 'CONS erstes weiteres) ; ohne Vereinfachung
        )
        ((eq (first weiteres) 'LIST)
         ; (CONS expr (LIST . rest)) -> (LIST expr . rest)
         (list* 'LIST erstes (rest weiteres))
        )
        ((or (eq (first weiteres) 'LIST*) (eq (first weiteres) 'CONS))
         ; (CONS expr (LIST* . rest)) -> (LIST* expr . rest)
         ; (CONS expr1 (CONS expr2 expr3)) -> (LIST* expr1 expr2 expr3)
         (list* 'LIST* erstes (rest weiteres))
        )
        (t (list 'CONS erstes weiteres)) ; ohne Vereinfachung
) )

