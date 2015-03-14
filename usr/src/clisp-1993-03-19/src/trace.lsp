;; Tracer
;; Bruno Haible 13.2.1990, 15.3.1991, 4.4.1991

; (TRACE) liefert Liste der getraceten Funktionen
; (TRACE fun ...) tracet die Funktionen fun, ... zusätzlich.
; Format für fun:
;   Entweder ein Symbol
;        symbol
;   oder eine Liste aus einem Symbol und einigen Keyword-Argumenten (paarig!)
;        (symbol
;          [:suppress-if form]   ; kein Trace-Output, solange form erfüllt ist
;          [:step-if form]       ; Trace geht in den Stepper, falls form erfüllt
;          [:pre form]           ; führt vor Funktionsaufruf form aus
;          [:post form]          ; führt nach Funktionsaufruf form aus
;          [:pre-break-if form]  ; Trace geht vor Funktionsaufruf in die Break-Loop,
;                                ; falls form erfüllt
;          [:post-break-if form] ; Trace geht nach Funktionsaufruf in die Break-Loop,
;                                ; falls form erfüllt
;          [:pre-print form]     ; gibt die Werte von form vor Funktionsaufruf aus
;          [:post-print form]    ; gibt die Werte von form nach Funktionsaufruf aus
;          [:print form]         ; gibt die Werte von form vor und nach Funktionsaufruf aus
;        )
;   In all diesen Formen kann auf *TRACE-FUNCTION* (die Funktion selbst)
;   und *TRACE-ARGS* (die Argumente an die Funktion)
;   und *TRACE-FORM* (der Funktions-/Macro-Aufruf als Form)
;   und nach Funktionsaufruf auch auf *TRACE-VALUES* (die Liste der Werte
;   des Funktionsaufrufs) zugegriffen werden,
;   und mit RETURN kann der Aufruf mit gegebenen Werten verlassen werden.
; (UNTRACE) liefert Liste der getraceten Funktionen, streicht sie alle.
; (UNTRACE symbol ...) streicht symbol, ... aus der Liste der getraceten
;   Funktionen.
; TRACE und UNTRACE sind auch auf Macros anwendbar, nicht jedoch auf lokal
;   definierte Funktionen und Macros.

(in-package "LISP")
(export '(trace untrace
          *trace-function* *trace-args* *trace-form* *trace-values*
)        )
(in-package "SYSTEM")

(proclaim '(special *trace-function* *trace-args* *trace-form* *trace-values*))
(defvar *traced-functions* nil) ; Liste der momentan getraceden Symbole
  ; Solange ein Symbol getraced ist, enthält
  ; die Property sys::traced-definition den alten Inhalt der Funktionszelle,
  ; die Property sys::tracing-definition den neuen Inhalt der Funktionszelle,
  ; und ist das Symbol Element der Liste *traced-functions*.
  ; Währenddessen kann sich der Inhalt der Funktionszelle jedoch ändern!
  ; Jedenfalls gilt stets:
  ;        (and (fboundp symbol)
  ;             (eq (symbol-function symbol) (get symbol 'sys::tracing-definition))
  ;        )
  ; ===>   (member symbol *traced-functions* :test #'eq)
  ; <==>   (get symbol 'sys::traced-definition)
(defvar *trace-level* 0) ; Verschachtelungstiefe bei der Trace-Ausgabe

; Funktionen, die der Tracer zur Laufzeit aufruft und die der Benutzer
; tracen könnte, müssen in ihrer ungetraceden Form aufgerufen werden.
; Statt (fun arg ...) verwende daher (SYS::%FUNCALL '#,#'fun arg ...).
; Dies gilt für alle hier verwendeten Funktionen von #<PACKAGE LISP> außer
; CAR, CDR, CONS, APPLY, VALUES-LIST (die alle inline compiliert werden).

(defmacro trace (&rest funs)
  (if (null funs)
    '*traced-functions*
    (cons 'append
      (mapcar #'(lambda (fun)
                  (if (atom fun) (trace1 fun) (apply #'trace1 fun))
                )
              funs
    ) )
) )

(defun trace1 (symbol &key (suppress-if nil) (step-if nil)
                           (pre nil) (post nil)
                           (pre-break-if nil) (post-break-if nil)
                           (pre-print nil) (post-print nil) (print nil)
                      &aux (old-function (gensym)) (macro-flag (gensym))
              )
  (unless (symbolp symbol)
    (error #+DEUTSCH "~S: Funktionsname sollte ein Symbol sein, nicht ~S"
           #+ENGLISH "~S: function name should be a symbol, not ~S"
           #+FRANCAIS "~S : Le nom de la fonction doit être un symbole et non ~S"
           'trace symbol
  ) )
  `(block nil
     (unless (fboundp ',symbol) ; Funktion überhaupt definiert?
       (warn #+DEUTSCH "~S: Funktion ~S ist nicht definiert."
             #+ENGLISH "~S: undefined function ~S"
             #+FRANCAIS "~S : La fonction ~S n'est pas définie."
             'trace ',symbol
       )
       (return nil)
     )
     (when (special-form-p ',symbol) ; Special-Form: nicht tracebar
       (warn #+DEUTSCH "~S: Special-Form ~S kann nicht getraced werden."
             #+ENGLISH "~S: cannot trace special form ~S"
             #+FRANCAIS "~S : La forme spéciale ~S ne peut pas être tracée."
             'trace ',symbol
       )
       (return nil)
     )
     (let* ((,old-function (symbol-function ',symbol))
            (,macro-flag (consp ,old-function)))
       (unless (eq ,old-function (get ',symbol 'sys::tracing-definition)) ; schon getraced?
         (setf (get ',symbol 'sys::traced-definition) ,old-function)
         (pushnew ',symbol *traced-functions*)
       )
       (format t #+DEUTSCH "~&;; ~:[Funktion~;Macro~] ~S wird getraced."
                 #+ENGLISH "~&;; Tracing ~:[function~;macro~] ~S."
                 #+FRANCAIS "~&;; Traçage ~:[de la fonction~;du macro~] ~S."
                 ,macro-flag ',symbol
       )
       (replace-in-fenv (get ',symbol 'sys::traced-definition) ',symbol
         ,old-function
         (setf (get ',symbol 'sys::tracing-definition)
           (setf (symbol-function ',symbol)
             ; neue Funktion, die die ursprüngliche ersetzt:
             ,(let ((newname (concat-pnames "TRACED-" symbol))
                    (body
                      `((declare (compile) (inline car cdr cons apply values-list))
                        (let ((*trace-level* (%funcall '#,#'1+ *trace-level*)))
                          (block nil
                            (unless ,suppress-if
                              (trace-pre-output)
                            )
                            ,@(when pre-print
                                `((trace-print (multiple-value-list ,pre-print)))
                              )
                            ,@(when print
                                `((trace-print (multiple-value-list ,print)))
                              )
                            ,pre
                            ,@(when pre-break-if
                                `((when ,pre-break-if (sys::break-loop t)))
                              )
                            (let ((*trace-values*
                                    (multiple-value-list
                                      (if ,step-if
                                        ;(eval `(step (apply ',*trace-function* ',*trace-args*)))
                                        (%funcall '#,#'eval
                                          (cons 'step
                                           (cons
                                             (cons 'apply
                                              (cons
                                                (cons 'quote (cons *trace-function* nil))
                                               (cons
                                                 (cons 'quote (cons *trace-args* nil))
                                                nil
                                             )))
                                            nil
                                          ))
                                        )
                                        (apply *trace-function* *trace-args*)
                                 )) ) )
                              ,@(when post-break-if
                                  `((when ,post-break-if (sys::break-loop t)))
                                )
                              ,post
                              ,@(when print
                                  `((trace-print (multiple-value-list ,print)))
                                )
                              ,@(when post-print
                                  `((trace-print (multiple-value-list ,post-print)))
                                )
                              (unless ,suppress-if
                                (trace-post-output)
                              )
                              (values-list *trace-values*)
                       )) ) )
                   ))
                `(if (not ,macro-flag)
                   (function ,newname
                     (lambda (&rest *trace-args*
                              &aux (*trace-form* (make-apply-form ',symbol *trace-args*))
                                   (*trace-function* (%funcall '#,#'get ',symbol 'sys::traced-definition))
                             )
                       ,@body
                   ) )
                   (cons 'sys::macro
                     (function ,newname
                       (lambda (&rest *trace-args*
                                &aux (*trace-form* (car *trace-args*))
                                     (*trace-function* (cdr (%funcall '#,#'get ',symbol 'sys::traced-definition)))
                               )
                         ,@body
                   ) ) )
                 )
              )
     ) ) ) )
     '(,symbol)
   )
)

;; Hilfsfunktionen:
; Funktionsreferenzen, die vom LABELS bei DEFUN kommen, ersetzen:
(defun replace-in-fenv (fun symbol old new)
  (when (and (sys::closurep fun) (not (compiled-function-p fun)))
    ; interpretierte Closure
    (let ((fenv (sys::%record-ref fun 5))) ; Funktions-Environment
      (when fenv ; falls nichtleer, durchlaufen:
        (do ((l (length fenv)) ; l = 2 * Anzahl der Bindungen + 1
             (i 1 (+ i 2)))
            ((eql i l))
          (when (and (eq (svref fenv (- i 1)) symbol) (eq (svref fenv i) old))
            (setf (svref fenv i) new)
        ) )
) ) ) )
; Eval-Form bauen, die einem Apply (näherungsweise) entspricht:
(defun make-apply-form (symbol args)
  (declare (inline cons mapcar))
  (cons symbol
    (mapcar #'(lambda (arg)
                ;(list 'quote arg)
                (cons 'quote (cons arg nil))
              )
            args
  ) )
)
; Output vor Aufruf, benutzt *trace-level* und *trace-form*
(defun trace-pre-output ()
  (%funcall '#,#'terpri *trace-output*)
  (%funcall '#,#'write *trace-level* :stream *trace-output* :base 10 :radix t)
  (%funcall '#,#'write-string " Trace: " *trace-output*)
  (%funcall '#,#'prin1 *trace-form* *trace-output*)
)
; Output nach Aufruf, benutzt *trace-level*, *trace-form* und *trace-values*
(defun trace-post-output ()
  (declare (inline car cdr consp atom))
  (%funcall '#,#'terpri *trace-output*)
  (%funcall '#,#'write *trace-level* :stream *trace-output* :base 10 :radix t)
  (%funcall '#,#'write-string " Trace: " *trace-output*)
  (%funcall '#,#'write (car *trace-form*) :stream *trace-output*)
  (%funcall '#,#'write-string " ==> " *trace-output*)
  (trace-print *trace-values* nil)
)
; Output einer Liste von Werten:
(defun trace-print (vals &optional (nl-flag t))
  (when nl-flag (%funcall '#,#'terpri *trace-output*))
  (when (consp vals)
    (loop
      (let ((val (car vals)))
        (%funcall '#,#'prin1 val *trace-output*)
      )
      (setq vals (cdr vals))
      (when (atom vals) (return))
      (%funcall '#,#'write-string ", " *trace-output*)
) ) )

(defmacro untrace (&rest funs)
  `(mapcan #'untrace1 ,(if (null funs) `(copy-list *traced-functions*) `',funs))
)

(defun untrace1 (symbol)
  (unless (symbolp symbol)
    (error #+DEUTSCH "~S: Funktionsname sollte ein Symbol sein, nicht ~S"
           #+ENGLISH "~S: function name should be a symbol, not ~S"
           #+FRANCAIS "~S : Le nom de la fonction doit être un symbole et non ~S"
           'untrace symbol
  ) )
  (let ((old-definition (get symbol 'sys::traced-definition)))
    (prog1
      (if old-definition
        ; symbol war getraced
        (progn
          (if (and (fboundp symbol)
                   (eq (symbol-function symbol) (get symbol 'sys::tracing-definition))
              )
            (progn
              (replace-in-fenv old-definition symbol (symbol-function symbol) old-definition)
              (setf (symbol-function symbol) old-definition)
            )
            (warn #+DEUTSCH "~S: ~S war getraced und wurde umdefiniert!"
                  #+ENGLISH "~S: ~S was traced and has been redefined!"
                  #+FRANCAIS "~S : ~S était tracée et a été redéfinie!"
                  'untrace symbol
          ) )
          `(,symbol)
        )
        ; symbol war nicht getraced
        '()
      )
      (untrace2 symbol)
) ) )

(defun untrace2 (symbol)
  (remprop symbol 'sys::traced-definition)
  (remprop symbol 'sys::tracing-definition)
  (setq *traced-functions* (delete symbol *traced-functions* :test #'eq))
)

