; FORMAT - und was dazugehört.
; Bruno Haible 22.06.1988
; CLISP-Version 16.08.1988, 03.09.1988, 04.08.1989
; Groß umgearbeitet von Bruno Haible am 14.02.1990-15.02.1990

(in-package "SYSTEM")

;-------------------------------------------------------------------------------

; Datenstruktur der Kontrollstring-Direktive:
(defstruct (control-string-directive
             (:copier nil)
             (:conc-name "CSD-")
             (:predicate nil)
             (:constructor make-csd ())
           )
  (type         0 :type fixnum)
  (cs-index     0 :type fixnum)
  (parm-list    nil :type list)
  (v-or-#-p     nil :type symbol)
  (colon-p      nil :type symbol)
  (atsign-p     nil :type symbol)
  (data         nil)
  (clause-chain nil)
)
#+CLISP (remprop 'control-string-directive 'sys::defstruct-description)
; Erläuterung:
; type=0 : Direktive ~<Newline>, nichts auszugeben.
;          Weitere Komponenten bedeutungslos
; type=1 : String auszugeben,
;          von *FORMAT-CS* die Portion :START cs-index :END data.
;          Weitere Komponenten bedeutungslos
; type=2 : Formatier-Direktive auszuführen.
;          data = Name der Direktive (Symbol),
;          colon-p gibt an, ob ein ':' da war,
;          atsign-p gibt an, ob ein '@' da war,
;          parm-list = Parameterliste an die Direktive,
;          v-or-#-p gibt an, ob parm-list vor dem Aufruf noch zu behandeln ist.
;          clause-chain ist eine Verzeigerung: z.B. bei ~[...~;...~;...~]
;          von der ~[-Direktive auf die Liste ab der ersten ~;-Direktive,
;          von da auf die Liste ab der nächsten ~;-Direktive usw.
;          bis schließlich auf die Liste ab der ~]-Direktive.

; Zeigt an, ob ein Character ein Whitespace-Character ist.
(defun whitespacep (char)
  (member char '(#\Space #\Newline #\Linefeed #\Tab #\Return #\Page))
)

; (FORMAT-PARSE-CS control-string startindex csdl stop-at)
; parst einen Kontrollstring (genauer: (subseq control-string startindex))
; und legt die sich ergebende Control-String-Directive-Liste in (cdr csdl) ab.
; Das Parsen muß mit der Direktive stop-at enden (ein Character, oder NIL
; für Stringende).
; Falls stop-at /= NIL, ist in (csd-clause-chain (car csdl)) ein Pointer auf
; die Teilliste ab dem nächsten Separator einzutragen. Diese Pointer bilden
; eine einfach verkettete Liste innerhalb csdl: von einem Separator zum
; nächsten, zum Schluß zum Ende der Clause.
(defun format-parse-cs (control-string startindex csdl stop-at)
  (declare (fixnum startindex))
  (macrolet ((errorstring ()
               #+DEUTSCH "Kontrollstring endet mitten in einer Direktive."
               #+ENGLISH "The control string terminates within a directive."
               #+FRANCAIS "La chaîne de contrôle se termine en plein milieu d'une directive."
            ))
    (prog* ((index startindex) ; cs-index des nächsten Zeichens
            ch ; current character
            intparam ; Integer-Parameter
            newcsd ; aktuelle CSD
            (last-separator-csd (car csdl))
           )
      (declare (type simple-string control-string) (type fixnum index))
      (loop ; neue Direktive insgesamt
        (tagbody
          (when (>= index (length control-string))
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (unless (eql ch #\~)
            ; eventuell noch Stringstück zu einer eingenen Direktive machen
            (setq csdl (setf (cdr csdl) (list (setq newcsd (MAKE-CSD)))))
            (setf (csd-type     newcsd) 1)
            (setf (csd-cs-index newcsd) index)
            (setq index (position #\~ control-string :start index))
            (unless index
              (setf (csd-data newcsd) (setq index (length control-string)))
              (go string-ended)
            )
            (setf (csd-data newcsd) index)
          )
          (setq csdl (setf (cdr csdl) (list (setq newcsd (MAKE-CSD)))))
          (setf (csd-type         newcsd) 2)
          (setf (csd-cs-index     newcsd) index)
          (setf (csd-parm-list    newcsd) nil)
          (setf (csd-v-or-#-p     newcsd) nil)
          (setf (csd-colon-p      newcsd) nil)
          (setf (csd-atsign-p     newcsd) nil)
          (setf (csd-data         newcsd) nil)
          (setf (csd-clause-chain newcsd) nil)

          param ; Parameter einer Direktive kann beginnen
          (incf index)
          (when (>= index (length control-string))
            (format-error control-string index (errorstring))
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (when (digit-char-p ch) (go num-param))
          (case ch
            ((#\+ #\-) (go num-param))
            (#\' (go quote-param))
            ((#\V #\v #\#)
             (push (if (eql ch #\#) ':ARG-COUNT ':NEXT-ARG)
                   (csd-parm-list newcsd)
             )
             (setf (csd-v-or-#-p newcsd) T)
             (go param-ok-1)
            )
            (#\, (push nil (csd-parm-list newcsd)) (go param))
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive))
          )

          num-param ; numerischer Parameter
          (multiple-value-setq (intparam index)
            (parse-integer control-string :start index :junk-allowed t)
          )
          (unless intparam
            (format-error control-string index
                          #+DEUTSCH "~A muß eine Zahl einleiten."
                          #+ENGLISH "~A must introduce a number."
                          #+FRANCAIS "~A doit introduire un nombre."
                          ch
          ) )
          (push intparam (csd-parm-list newcsd))
          (go param-ok-2)

          quote-param ; Quote-Parameter-Behandlung
          (incf index)
          (when (>= index (length control-string))
            (format-error control-string index
              #+DEUTSCH "Kontrollstring endet mitten in einem '-Parameter."
              #+ENGLISH "The control string terminates in the middle of a parameter."
              #+FRANCAIS "La chaîne de contrôle se termine au milieu d'un paramètre."
            )
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (push ch (csd-parm-list newcsd))

          param-ok-1 ; Parameter OK
          (incf index)
          param-ok-2 ; Parameter OK
          (when (>= index (length control-string))
            (format-error control-string index (errorstring))
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (case ch
            (#\, (go param))
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive))
          )

          colon-modifier ; nach :
          (setf (csd-colon-p newcsd) T)
          (go passed-modifier)

          atsign-modifier ; nach @
          (setf (csd-atsign-p newcsd) T)
          (go passed-modifier)

          passed-modifier ; nach : oder @
          (incf index)
          (when (>= index (length control-string))
            (format-error control-string index (errorstring))
            (go string-ended)
          )
          (setq ch (schar control-string index))
          (case ch
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive))
          )

          directive ; Direktive (ihr Name) erreicht
          (setf (csd-parm-list newcsd) (nreverse (csd-parm-list newcsd)))
          (let ((directive-name
                  (cdr (assoc (char-upcase ch)
                         '((#\A . FORMAT-ASCII)
                           (#\S . FORMAT-S-EXPRESSION)
                           (#\W . FORMAT-WRITE)
                           (#\D . FORMAT-DECIMAL)
                           (#\B . FORMAT-BINARY)
                           (#\O . FORMAT-OCTAL)
                           (#\X . FORMAT-HEXADECIMAL)
                           (#\R . FORMAT-RADIX)
                           (#\P . FORMAT-PLURAL)
                           (#\C . FORMAT-CHARACTER)
                           (#\F . FORMAT-FIXED-FLOAT)
                           (#\E . FORMAT-EXPONENTIAL-FLOAT)
                           (#\G . FORMAT-GENERAL-FLOAT)
                           (#\$ . FORMAT-DOLLARS-FLOAT)
                           (#\% . FORMAT-TERPRI)
                           (#\& . FORMAT-FRESH-LINE)      (#\Newline . #\Newline)
                           (#\| . FORMAT-PAGE)
                           (#\~ . FORMAT-TILDE)
                           (#\T . FORMAT-TABULATE)
                           (#\* . FORMAT-GOTO)
                           (#\? . FORMAT-INDIRECTION)
                           (#\( . FORMAT-CASE-CONVERSION) (#\) . FORMAT-CASE-CONVERSION-END)
                           (#\[ . FORMAT-CONDITIONAL)     (#\] . FORMAT-CONDITIONAL-END)
                           (#\{ . FORMAT-ITERATION)       (#\} . FORMAT-ITERATION-END)
                           (#\< . FORMAT-JUSTIFICATION)   (#\> . FORMAT-JUSTIFICATION-END)
                           (#\^ . FORMAT-UP-AND-OUT)      (#\; . FORMAT-SEPARATOR)
                           ; mit Funktionsdefinition      ; ohne Funktionsdefinition
               )) )    )  )
            (if directive-name
              (setf (csd-data newcsd) directive-name)
              (format-error control-string index
                #+DEUTSCH "Diese Direktive gibt es nicht."
                #+ENGLISH "Non-existent directive"
                #+FRANCAIS "Directive non reconnue."
          ) ) )
          (incf index)
          (case ch
            (( #\( #\[ #\{ #\< )
             (multiple-value-setq (index csdl)
               (format-parse-cs control-string index csdl
                 (case ch (#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>) )
             ) )
            )
            (( #\) #\] #\} #\> )
             (unless stop-at
               (format-error control-string index
                 #+DEUTSCH "Schließende Klammer '~A' ohne vorherige öffnende Klammer"
                 #+ENGLISH "The closing directive '~A' does not have a corresponding opening one."
                 #+FRANCAIS "Parenthèse fermante '~A' sans parenthèse ouvrante correspondante."
                 ch
             ) )
             (unless (eql ch stop-at)
               (format-error control-string index
                 #+DEUTSCH "Schließende Klammer '~A' paßt nicht; sollte '~A' lauten."
                 #+ENGLISH "The closing directive '~A' does not match the corresponding opening one. It should read '~A'."
                 #+FRANCAIS "La parenthèse fermante '~A' ne correspond pas à celle ouvrante. Il devrait y avoir '~A'."
                 ch stop-at
             ) )
             (setf (csd-clause-chain last-separator-csd) csdl)
             (go end)
            )
            (#\;
             (unless (or (eql stop-at #\]) (eql stop-at #\>))
               (format-error control-string index
                 #+DEUTSCH "Hier ist keine ~~;-Direktive möglich."
                 #+ENGLISH "The ~~; directive is not allowed at this point."
                 #+FRANCAIS "La directive ~~; n'est pas permise ici."
             ) )
             (setf (csd-clause-chain last-separator-csd) csdl)
             (setq last-separator-csd newcsd)
            )
            (#\Newline
             (setf (csd-type newcsd) 0)
             (if (csd-colon-p newcsd)
               (if (csd-atsign-p newcsd)
                 (format-error control-string index
                   #+DEUTSCH "Die ~~Newline-Direktive ist mit : und @ sinnlos."
                   #+ENGLISH "The ~~newline directive cannot take both modifiers."
                   #+FRANCAIS "La directive ~~Newline est insensée avec les deux qualificateurs : et @."
                 )
                 nil ; ~:<newline> -> Newline ignorieren, Whitespace dalassen
               )
               (progn
                 (when (csd-atsign-p newcsd)
                   ; ~@<newline> -> Stringstück mit Newline zum Ausgeben
                   (setf (csd-type newcsd) 1)
                   (setf (csd-cs-index newcsd) (1- index))
                   (setf (csd-data newcsd) index)
                 )
                 (setq index
                   (or (position-if-not #'whitespacep control-string :start index)
                       (length control-string)
          ) )) ) ) )
        ) ; tagbody zu Ende
      ) ; loop zu Ende

      string-ended
      (when stop-at
        (format-error control-string index
          #+DEUTSCH "Schließende Klammer '~A' fehlt."
          #+ENGLISH "An opening directive is never closed; expecting '~A'."
          #+FRANCAIS "Il manque la borne fermante '~A'."
          stop-at
      ) )

      end
      (return (values index csdl))
) ) )

;-------------------------------------------------------------------------------

(defvar *FORMAT-CS*) ; control-string
(defvar *FORMAT-CSDL*) ; control-string directive list
(defvar *FORMAT-ARG-LIST*) ; argument-list
(defvar *FORMAT-NEXT-ARG*) ; pointer to next argument in argument-list
(defvar *FORMAT-UP-AND-OUT* nil) ; reason for up-and-out

; (format-error controlstring errorpos errorcode . arguments)
; signalisiert einen Error, der bei FORMAT aufgetreten ist. Die Stelle im
; Control-string wird mit einem Pfeil markiert.
(defun format-error (controlstring errorpos errorstring &rest arguments)
  (unless errorpos (setq errorpos (csd-cs-index (car *FORMAT-CSDL*))))
  (setq errorstring
    (sys::string-concat errorstring
      #+DEUTSCH "~%Stelle im Kontrollstring:"
      #+ENGLISH "~%Current point in control string:"
      #+FRANCAIS "~%Position dans la chaîne de contrôle :"
  ) )
  (let ((pos1 0) (pos2 0))
    (declare (simple-string errorstring) (fixnum pos1 pos2))
    (loop
      (setq pos2 (or (position #\Newline controlstring :start pos1)
                     (length controlstring)
      )          )
      (setq errorstring (sys::string-concat errorstring "~%  ~A"))
      (setq arguments
        (nconc arguments (list (sys::substring controlstring pos1 pos2))) )
      (when (<= pos1 errorpos pos2)
        (setq errorstring (sys::string-concat errorstring "~%~VT"))
        (setq arguments (nconc arguments (list (+ (- errorpos pos1) 2))))
      )
      (when (= pos2 (length controlstring)) (return))
      (setq pos1 (+ pos2 1))
  ) )
  (apply #'error errorstring arguments)
)

;-------------------------------------------------------------------------------

(defun format (destination control-string &rest arguments)
  (unless (stringp control-string)
    (error
      #+DEUTSCH "Kontrollstring muß ein String sein, nicht ~S"
      #+ENGLISH "The control-string must be a string, not ~S"
      #+FRANCAIS "La chaîne de contrôle doit être une chaîne et non ~S"
      control-string
  ) )
  ; evtl. noch control-string zu einem Simple-String machen ??
  (let ((node (list control-string)))
    (format-parse-cs control-string 0 node nil)
    (let* ((*FORMAT-CS*         (car node))
           (*FORMAT-CSDL*       (cdr node))
           (*FORMAT-ARG-LIST*   arguments)
           (*FORMAT-NEXT-ARG*   *FORMAT-ARG-LIST*)
           (*FORMAT-UP-AND-OUT* nil))
      (cond ((null destination)
             (let ((stream (make-string-output-stream)))
               (format-interpret stream)
               (get-output-stream-string stream)
            ))
            ((eq destination 'T)
             (format-interpret *STANDARD-OUTPUT*)
             nil
            )
            ((streamp destination)
             (format-interpret destination)
             nil
            )
            ((stringp destination)
             (if (array-has-fill-pointer-p destination)
               (let ((stream (sys::make-string-push-stream destination)))
                 (format-interpret stream)
               )
               (error
                 #+DEUTSCH "String zum Vollschreiben ~S hat keinen Fill-Pointer."
                 #+ENGLISH "The destination string ~S should have a fill pointer."
                 #+FRANCAIS "La chaîne destination n'a pas de pointeur de remplissage."
                 destination
             ) )
             nil
            )
            (t (error
                 #+DEUTSCH "Das ist weder NIL noch T noch ein Stream noch ein String: ~S"
                 #+ENGLISH "The destination argument ~S is invalid (not NIL or T or a stream or a string)."
                 #+FRANCAIS "L'argument de destination n'est ni NIL, ni T, ni un «stream» ni une chaîne : ~S"
                 destination
            )  )
) ) ) )

;-------------------------------------------------------------------------------

; (next-arg) liefert (und verbraucht) das nächste Argument aus der Argument-
; liste *FORMAT-NEXT-ARG*.
(defun next-arg ()
  (if (atom *FORMAT-NEXT-ARG*)
    (format-error *FORMAT-CS* nil
      #+DEUTSCH "Nicht genügend Argumente für diese Direktive übrig."
      #+ENGLISH "There are not enough arguments left for this directive."
      #+FRANCAIS "Il ne reste pas assez d'arguments pour cette directive."
    )
    (pop *FORMAT-NEXT-ARG*)
) )

; (format-interpret stream [endmarker]) interpretiert *FORMAT-CSDL* ab.
; Fluid vars:
;   *FORMAT-ARG-LIST*
;   *FORMAT-NEXT-ARG*
;   *FORMAT-CS*
;   *FORMAT-CSDL*
;   *FORMAT-UP-AND-OUT*
; Abbruch des Interpretierens bei Antreffen der Direktive endmarker
; oder der Direktive ~; .
(defun format-interpret (stream &optional (endmarker nil))
  (loop
    (when *FORMAT-UP-AND-OUT* (return))
    (when (endp *FORMAT-CSDL*) (return))
    (let ((csd (car *FORMAT-CSDL*)))
      (case (csd-type csd)
        (0 )
        (1 (write-string *FORMAT-CS* stream
             :start (csd-cs-index csd) :end (csd-data csd)
        )  )
        (2 (let ((directive-name (csd-data csd)))
             (if (eq directive-name endmarker) (return))
             (if (eq directive-name 'FORMAT-SEPARATOR) (return))
             (apply directive-name
               stream
               (csd-colon-p csd)
               (csd-atsign-p csd)
               (format-resolve-parms csd)
        )  ) )
    ) )
    (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
) )

; liefert die korrekte Argumentliste einer CSD, evtl. mit eingesetzten
; Parametern: V (als :NEXT-ARG) und # (als :ARG-COUNT) werden aufgelöst.
(defun format-resolve-parms (csd)
  (let ((arglist (csd-parm-list csd)))
    (if (csd-v-or-#-p csd)
      (mapcar #'(lambda (arg)
                  (case arg
                    (:NEXT-ARG (next-arg))
                    (:ARG-COUNT (list-length *FORMAT-NEXT-ARG*))
                    (T arg)
                ) )
              arglist
      )
      arglist
) ) )

; Bewegt den Stand des "Pointers in die Argumentliste" in eine Richtung.
(defun format-goto-new-arg (backwardp index)
  (if backwardp
    ; rückwärts
    (setq *FORMAT-NEXT-ARG*
      (nthcdr
        (max (- (list-length *FORMAT-ARG-LIST*) (list-length *FORMAT-NEXT-ARG*) index) 0)
        *FORMAT-ARG-LIST*
    ) )
    ; vorwärts ist einfacher:
    (setq *FORMAT-NEXT-ARG* (nthcdr index *FORMAT-NEXT-ARG*))
) )

; gibt arg als römische Zahl auf stream aus, z.B. 4 als IIII.
(defun format-old-roman (arg stream)
  (unless (and (integerp arg) (<= 1 arg 4999))
    (format-error *FORMAT-CS* nil
      #+DEUTSCH "Die ~~:@R-Direktive erwartet ein Integer zwischen 1 und 4999, nicht ~S"
      #+ENGLISH "The ~~:@R directive requires an integer in the range 1 - 4999, not ~S"
      #+FRANCAIS "La directive ~~:@R requiert un entier compris entre 1 et 4999 et non ~S"
      arg
  ) )
  (do ((charlistr  '(#\M  #\D #\C #\L #\X #\V #\I) (cdr charlistr))
       (valuelistr '(1000 500 100 50  10   5   1) (cdr valuelistr))
       (value arg (multiple-value-bind (multiplicity restvalue)
                      (floor value (first valuelistr))
                    (dotimes (i multiplicity)
                      (write-char (first charlistr) stream)
                    )
                    restvalue
      ))          )
      ((zerop value))
) )

; gibt arg als römische Zahl auf stream aus, z.B. 4 als IV.
(defun format-new-roman (arg stream)
  (unless (and (integerp arg) (<= 1 arg 3999))
    (format-error *FORMAT-CS* nil
      #+DEUTSCH "Die ~~@R-Direktive erwartet ein Integer zwischen 1 und 3999, nicht ~S"
      #+ENGLISH "The ~~@R directive requires an integer in the range 1 - 3999, not ~S"
      #+FRANCAIS "La directive ~~@R requiert un entier compris entre 1 et 3999 et non ~S"
      arg
  ) )
  (do ((charlistr       '(#\M #\D #\C #\L #\X #\V #\I) (cdr charlistr))
       (valuelistr     '(1000 500 100 50  10   5   1 ) (cdr valuelistr))
       (lowercharlistr  '(#\C #\C #\X #\X #\I #\I    ) (cdr lowercharlistr))
       (lowervaluelistr '(100 100 10  10   1   1   0 ) (cdr lowervaluelistr))
       (value arg
         (multiple-value-bind (multiplicity restvalue)
             (floor value (first valuelistr))
           (dotimes (i multiplicity) (write-char (first charlistr) stream))
           (let ((loweredvalue (- (first valuelistr) (first lowervaluelistr))))
             (if (>= restvalue loweredvalue)
               (progn
                 (write-char (first lowercharlistr) stream)
                 (write-char (first charlistr) stream)
                 (- restvalue loweredvalue)
               )
               restvalue
      )) ) ) )
      ((zerop value))
) )

(defconstant FORMAT-CARDINAL-ONES
  '#(NIL "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
     "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
     "seventeen" "eighteen" "nineteen"
)   )

(defconstant FORMAT-CARDINAL-TENS
  '#(NIL NIL "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety")
)

; (format-small-cardinal arg stream) gibt eine ganze Zahl >0, <1000 im
; Klartext auf englisch auf den stream aus. (arg=0 -> gibt nichts aus.)
(defun format-small-cardinal (arg stream)
  (multiple-value-bind (hundreds tens-and-ones) (truncate arg 100)
    (when (> hundreds 0)
      (write-string (svref FORMAT-CARDINAL-ONES hundreds) stream)
      (write-string " hundred" stream)
    )
    (when (> tens-and-ones 0)
      (when (> hundreds 0) (write-string " and " stream))
      (multiple-value-bind (tens ones) (truncate tens-and-ones 10)
        (if (< tens 2)
          (write-string (svref FORMAT-CARDINAL-ONES tens-and-ones) stream)
          (progn
            (write-string (svref FORMAT-CARDINAL-TENS tens) stream)
            (when (> ones 0)
              (write-char #\- stream)
              (write-string (svref FORMAT-CARDINAL-ONES ones) stream)
) ) ) ) ) ) )

; (format-cardinal arg stream) gibt die ganze Zahl arg im Klartext auf englisch
; auf den Stream aus.
(defun format-cardinal (arg stream) ; arg Integer
  (if (zerop arg)
    (write-string "zero" stream)
    (progn
      (when (minusp arg) (write-string "minus " stream) (setq arg (- arg)))
      (labels
        ((blocks1000 (illions-list arg) ; Zerlegung in 1000er-Blöcke
           (when (null illions-list)
             (format-error *FORMAT-CS* nil
               #+DEUTSCH "Zu großes Argument für ~~R-Direktive."
               #+ENGLISH "The argument for the ~~R directive is too large."
               #+FRANCAIS "L'argument pour la directive ~~R est trop grand."
           ) )
           (multiple-value-bind (thousands small) (truncate arg 1000)
             (when (> thousands 0) (blocks1000 (cdr illions-list) thousands))
             (when (> small 0)
               (when (> thousands 0) (write-string ", " stream))
               (format-small-cardinal small stream)
               (write-string (car illions-list) stream)
        )) ) )
        (blocks1000
          ; amerikanisch (billion=10^9)
          '("" " thousand" " million" " billion" " trillion" " quadrillion"
            " quintillion" " sextillion" " septillion" " octillion" " nonillion"
            " decillion" " undecillion" " duodecillion" " tredecillion"
            " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
            " octodecillion" " novemdecillion" " vigintillion")
          arg
) ) ) ) )

(defconstant FORMAT-ORDINAL-ONES
  '#(NIL "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth"
     "ninth" "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
     "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"
)   )

; (format-ordinal arg stream) gibt eine ganze Zahl arg als Abzählnummer im
; Klartext auf englisch auf den stream aus.
(defun format-ordinal (arg stream) ; arg Integer
  (if (zerop arg)
    (write-string "zeroth" stream)
    (progn
      (when (minusp arg) (write-string "minus " stream) (setq arg (- arg)))
      (multiple-value-bind (hundreds tens-and-ones) (floor arg 100)
        (when (> hundreds 0) (format-cardinal (* hundreds 100) stream))
        (if (zerop tens-and-ones)
          (write-string "th" stream)
          (multiple-value-bind (tens ones) (floor tens-and-ones 10)
            (when (> hundreds 0) (write-char #\Space stream))
            (cond ((< tens 2)
                   (write-string (svref FORMAT-ORDINAL-ONES tens-and-ones) stream)
                  )
                  ((zerop ones)
                   (write-string
                     (svref '#(NIL "tenth" "twentieth" "thirtieth" "fortieth" "fiftieth"
                               "sixtieth" "seventieth" "eightieth" "ninetieth")
                            tens
                     )
                     stream
                  ))
                  (t (write-string (svref FORMAT-CARDINAL-TENS tens) stream)
                     (write-char #\- stream)
                     (write-string (svref FORMAT-ORDINAL-ONES ones) stream)
) ) ) ) ) ) )     )

; (format-padding count char stream) gibt count (ein Fixnum >=0) Zeichen char
; auf stream aus.
(defun format-padding (count char stream)
  (dotimes (i count) (write-char char stream))
)

; gibt auf den Stream stream aus:
; den String str, eventuell aufgefüllt mit Padding characters padchar.
; Und zwar so, daß die Breite mindestens mincol ist. Um das zu erreichen,
; werden mindestens minpad Zeichen eingefügt, eventuelle weitere dann in
; Blöcken à colinc Zeichen. Falls padleftflag, werden sie links eingefügt,
; sonst rechts vom String.
(defun format-padded-string
       (mincol colinc minpad padchar padleftflag str stream)
  (let* ((need (+ (length str) minpad)) ; so viele Zeichen mindestens
         (auxpad (if (< need mincol)
                   (* (ceiling (- mincol need) colinc) colinc)
                   0
        ))       ) ; so viele Zeichen zusätzlich
    (unless padleftflag (write-string str stream))
    (format-padding (+ minpad auxpad) padchar stream)
    (when padleftflag (write-string str stream))
) )

; gibt den Integer arg auf den Stream aus:
; in Zahlenbasis base, mit Vorzeichen (+ nur falls >0 und positive-sign-flag),
; bei commaflag alle drei Stellen unterbrochen durch ein Zeichen commachar.
; Das Ganze links aufgefüllt mit padchar's, so daß die Gesamtbreite mindestens
; mincol ist.
(defun format-integer (base
                       mincol
                       padchar
                       commachar
                       commaflag
                       positive-sign-flag
                       arg
                       stream
                      )
  (let* ((*print-base* base)
         (*print-radix* nil))
    (if (and (zerop mincol) (not commaflag) (not positive-sign-flag))
      (princ arg stream) ; normale Ausgabe tut's
      (let* ((oldstring (princ-to-string arg))
             (oldstring-length (length oldstring))
             (number-of-digits
               (if (minusp arg) (1- oldstring-length) oldstring-length) )
             (number-of-commas
               (if commaflag (floor (1- number-of-digits) 3) 0) )
             (positive-sign (and positive-sign-flag (> arg 0)))
             (newstring-length
               (+ (if positive-sign 1 0) ; Vorzeichen
                  oldstring-length number-of-commas ; Ziffern, Kommas
             ) )
             (newstring (make-string newstring-length)) )
        ; Erst Vorzeichen +:
        (when positive-sign (setf (schar newstring 0) #\+))
        ; Dann oldstring in newstring übertragen, dabei Kommata überspringen:
        (let ((oldpos oldstring-length) (newpos newstring-length))
          (loop
            (decf oldpos)
            (when (minusp oldpos) (return))
            (decf newpos)
            (setf (schar newstring newpos) (schar oldstring oldpos))
            (when (and (plusp number-of-commas)
                       (zerop (mod (- oldstring-length oldpos) 3))
                  ) ; noch ein Komma einzufügen?
              (decf newpos)
              (setf (schar newstring newpos) commachar)
              (decf number-of-commas)
        ) ) )
        (if (zerop mincol)
          (write-string newstring stream) ; schneller
          (format-padded-string mincol 1 0 padchar t newstring stream)
) ) ) ) )

; was ~D bei non-Integer-Argument tut: Argument mit ~A, aber dezimal ausgeben
(defun format-ascii-decimal (arg stream)
  (let ((*print-base* 10.)
        (*print-radix* nil))
    (princ arg stream)
) )

; Unterprogramm für ~D, ~B, ~O, ~X:
(defun format-base (base stream colon-modifier atsign-modifier
                    mincol padchar commachar)
  (if (null mincol) (setq mincol 0))
  (if (null padchar) (setq padchar #\Space))
  (if (null commachar) (setq commachar #\,))
  (let ((arg (next-arg)))
    (if (or (and (zerop mincol) (not colon-modifier) (not atsign-modifier))
            (not (integerp arg))
        )
      (let ((*print-base* base)
            (*print-radix* nil))
        (princ arg stream)
      )
      (format-integer base mincol padchar commachar
                      colon-modifier atsign-modifier arg stream
) ) ) )

; (format-scale-exponent-aux arg null eins zehn zehntel lg2)
; liefert zur Floating-Point-Zahl arg >= 0 und
; null = 0.0, eins = 1.0, zehn = 10.0, zehntel = 0.1, lg2 = log(2)/log(10)
; (erste vier in derselben Floating-Point-Precision wie arg)
; zwei Werte: mantissa und n, mit
; ganzem n und mantissa floating-point, 0.1 <= mantissa < 1,
; arg = mantissa * 10^n (also 10^(n-1) <= arg < 10^n ).
; (Bei arg=null: null und n=0.)
(defun format-scale-exponent-aux (arg null eins zehn zehntel lg2)
  (multiple-value-bind (significand expon) (decode-float arg)
    (declare (ignore significand))
    (if (zerop arg)
      (values null 0)
      (let* ((expon10a (truncate (* expon lg2))) ; nicht round, um Überlauf zu vermeiden
             (signif10a (/ arg (expt zehn expon10a))))
        (do ((zehnpot zehn (* zehnpot zehn))
             (signif10b signif10a (/ signif10a zehnpot))
             (expon10b expon10a (1+ expon10b)))
            ((< signif10b eins)
             (do ((zehnpot zehn (* zehnpot zehn))
                  (signif10c signif10b (* signif10c zehnpot))
                  (expon10c expon10b (1- expon10c)))
                 ((>= signif10c zehntel)
                  (values signif10c expon10c)
             )   )
        )   )
) ) ) )

; (format-scale-exponent arg) liefert zur Floating-Point-Zahl arg >= 0
; zwei Werte: mantissa und n, mit
; ganzem n und mantissa floating-point, 0.1 <= mantissa < 1,
; arg = mantissa * 10^n (also 10^(n-1) <= arg < 10^n ).
; (Bei arg=null: 0.0 und n=0.)
(defun format-scale-exponent (arg)
  (cond ((short-float-p arg)
         (format-scale-exponent-aux arg 0.0s0 1.0s0 10.0s0 0.1s0 0.30103s0)
        )
        ((single-float-p arg)
         (format-scale-exponent-aux arg 0.0f0 1.0f0 10.0f0 0.1f0 0.30103s0)
        )
        ((double-float-p arg)
         (format-scale-exponent-aux arg 0.0d0 1.0d0 10.0d0 0.1d0 0.30103s0)
        )
        ((long-float-p arg)
         (format-scale-exponent-aux arg
           (float 0 arg) (float 1 arg) (float 10 arg) (float 1/10 arg)
           0.30102999566d0 ; lg2 wird mit 32 Bit Genauigkeit gebraucht
) )     ))

; (format-float-to-string arg width d k dmin)
; ergibt einen String zum Floating-point arg:
; er hat den Wert von (* (abs arg) (expt 10 k)), dabei mind. d Nachkommastellen
; und höchstens die Länge width (width=nil -> keine Einschränkung).
; Trotzdem wird nicht auf weniger als dmin Stellen gerundet.
(let ((digit-string
        (make-array 20 :element-type 'string-char :adjustable t :fill-pointer t)
     ))
(defun format-float-to-string (arg width d k dmin)
  (if (zerop arg)
    (let ((places (max (or d 0) (or dmin 0))))
      (when width ; width angegeben -> places := (min places (1- width))
        (when (>= places width) (setq places (1- width)))
      )
      (values
        (let ((str (make-string (1+ places) :initial-element #\0)))
          (setf (schar str 0) #\.)
          str          ; ein Punkt und places Nullen
        )
        (1+ places)    ; Stellenzahl
        t              ; Punkt ganz vorne
        (zerop places) ; Punkt ganz hinten ?
        0              ; Position des Punktes
    ) )
    (multiple-value-bind (significand expon) (integer-decode-float arg)
; significand : Integer >0
; expon : Integer
; mantprec : Anzahl der echten Mantissenbits von significand
; (also 2^mantprec <= significand < 2^(mantprec+1))
; width : Anzahl Stellen, die die Zahl (inklusive Punkt) nicht überschreiten
;         soll, oder NIL
; d : Mindestanzahl Nachkommastellen oder NIL
; k : Skalierungsfaktor (siehe CLTL S.394)
; dmin : Mindestanzahl von Dezimaltellen, die (trotz Angabe von width oder d)
;        nicht gerundet werden dürfen.
;        (Nur interessant, falls d <= dmin <= (precision der Zahl).)
; wandelt die Zahl significand*2^expon um in einen Dezimalstring um.
; Es ist kein Exponent dabei.
      (let* ((mantprec (1- (float-digits arg)))
             (numerator significand)
             (denominator 1)
             (abrund-einh 1) ; Abrundungseinheit:
               ; Abrunden um 1 in der letzten abrundbaren Stelle entspricht
               ; einer Erniedrigung von numerator um abrund-einh.
             (aufrund-einh 1) ; Aufrundungseinheit:
               ; Aufrunden um 1 in der letzten aufrundbaren Stelle entspricht
               ; einer Erhöhung von numerator um aufrund-einh.
             ; Stellen: 0 = 1. Stelle vor dem Punkt, -1 = 1. Stelle nach dem Punkt.
             (stelle 0) ; Stelle der als nächstes auszugebenden Ziffer
             (digit-count 0) ; Zahl der bisher in digit-string ausgegebenen
                             ; Ziffern (exklusive den Punkt)
             (point-pos 0) ; Punkt-Position = Zahl führender Stellen
                           ; = Zahl der Ziffern vor dem Punkt
             (letzte-stelle nil) ; NIL oder (falls d oder width angegeben waren)
                           ; Stelle der letzten signifikanten Ziffer
             (halbzahlig nil) ; zeigt an, ob hinten genau ein 0.500000 wegfällt
             digit ; die laufende Ziffer, >=0, <10
             (abrunden nil) ; T falls letzte Ziffer abzurunden ist
             (aufrunden nil) ; T falls letzte Ziffer aufzurunden ist
            )
        (setf (fill-pointer digit-string) 0) ; digit-string leeren
        (cond
          ((> expon 0)
           (setq numerator (ash significand expon))
           (setq aufrund-einh (setq abrund-einh (ash 1 expon)))
          )
          ((< expon 0)
           (setq denominator (ash 1 (- expon))) ; aufrund-einh = abrund-einh = 1
        ) )
        ; Zahl = numerator/denominator
        (when (= significand (ash 1 mantprec))
          ; Ist der Significand=2^mantprec, so ist abrund-einh zu halbieren.
          ; Man kann stattdessen auch alle 3 anderen Grössen verdoppeln:
          (setq aufrund-einh (ash aufrund-einh 1))
          (setq numerator (ash numerator 1))
          (setq denominator (ash denominator 1))
        )
        ; Defaultmäßig: Auf-/Abrunde-Einheit = eine Einheit in der letzten
        ; BINÄRstelle.
        ; Zahl = numerator/denominator
        ; Skalierungsfaktor k in die Zahl mit einbeziehen (vgl. CLTL S.394)
        ; k<0 -> Mantisse durch 10^(abs k) dividieren
        ; k>0 -> Mantisse mit 10^k multiplizieren
        ; Dabei aufrund-einh, abrund-einh im Verhältnis zu numerator beibehalten.
        (when k
          (if (< k 0)
            (let ((skal-faktor (expt 10 (- k))))
              (setq denominator (* denominator skal-faktor))
            )
            (let ((skal-faktor (expt 10 k)))
              (setq numerator (* numerator skal-faktor))
              (setq aufrund-einh (* aufrund-einh skal-faktor))
              (setq abrund-einh (* abrund-einh skal-faktor))
            )
        ) )
        ; auf >= 1/10 adjustieren:
        ; (jeweils numerator mit 10 multiplizieren, eine führende 0 mehr vorsehen)
        (do ()
            ((>= (* numerator 10) denominator))
          (setq stelle (1- stelle))
          (setq numerator (* numerator 10))
          (setq abrund-einh (* abrund-einh 10))
          (setq aufrund-einh (* aufrund-einh 10))
        )
        ; stelle = Stelle der letzten führenden 0
        ;        = 1 + Stelle der 1. signifikanten Ziffer
        ;        oder =0, falls k>=0
        ; Ausführung der Rundung:
        (loop
          ; Solange das Ergebnis auch nach Aufrundung >= 1 bliebe,
          ; eine Vorkommastelle mehr einplanen:
          (do ()
              ((< (+ (ash numerator 1) aufrund-einh) (ash denominator 1)))
            (setq denominator (* denominator 10))
            (setq stelle (1+ stelle))
          )
          ; Falls d oder width angegeben:
          ; letzte-stelle ausrechnen
          (if d
            ; Falls dmin angegeben: (min (- d) (- dmin)) = (- (max d dmin)).
            ; Sonst (- d).
            (progn
              (setq letzte-stelle (- d))
              (when (and dmin (> letzte-stelle (- dmin)))
                (setq letzte-stelle (- dmin))
            ) )
            ; Falls nicht d, nur width angegeben:
            (when width
              (if (< stelle 0)
                ; Es kommen führende Nullen nach dem Punkt -> d:=(1- width)
                (setq letzte-stelle (- 1 width))
                ; Es kommen keine führenden Nullen nach dem Punkt ->
                ; Es wird stelle Vorkommaziffern geben, d:=(- (1- width) stelle)
                (setq letzte-stelle (1+ (- stelle width)))
              )
              ; also letzte-stelle = (- (- (1- width) (max stelle 0)))
              ; wieder dmin berücksichtigen:
              (when (and dmin (> letzte-stelle (- dmin)))
                (setq letzte-stelle (- dmin))
          ) ) )
          (when (or d width)
            (let* ((ziffernzahl (- letzte-stelle stelle))
                   ; ziffernzahl = Zahl signifikanter Stellen oder <0.
                   (dezimal-einh denominator))
              ; dezimal-einh := (ceiling (* dezimal-einh (expt 10 ziffernzahl)))
              (if (>= ziffernzahl 0)
                (dotimes (i ziffernzahl)
                  (setq dezimal-einh (* dezimal-einh 10))
                )
                (dotimes (i (- ziffernzahl))
                  (setq dezimal-einh (ceiling dezimal-einh 10))
                )
              )
              ; dezimal-einh = Um wieviel numerator erhöht bzw. erniedigt werden
              ; müßte, damit sich die Dezimaldarstellung um genau 1 an der
              ; Position letzte-stelle verändert.
              (setq abrund-einh (max dezimal-einh abrund-einh))
              (setq aufrund-einh (max dezimal-einh aufrund-einh))
              ; Jetzt darf auch um eine (halbe) DEZIMAL-Einheit gerundet werden.
              (when (= aufrund-einh dezimal-einh) (setq halbzahlig T))
          ) )
          (when (< (+ (ash numerator 1) aufrund-einh) (ash denominator 1))
            (return)
        ) )
        ; stelle = Position der ersten signifikanten Stelle + 1
        ; Führenden Punkt und nachfolgende Nullen ausgeben:
        (when (< stelle 0)
          (setq point-pos digit-count)
          (vector-push-extend #\. digit-string)
          (dotimes (i (- stelle))
            (incf digit-count)
            (vector-push-extend #\0 digit-string)
        ) )
        ; Ziffern der Mantisse ausgeben:
        (loop
          (when (zerop stelle)
            (vector-push-extend #\. digit-string)
            (setq point-pos digit-count)
          )
          (decf stelle)
          (multiple-value-setq (digit numerator)
            (truncate (* numerator 10) denominator)
          )
          (setq abrund-einh (* abrund-einh 10))
          (setq aufrund-einh (* aufrund-einh 10))
          (setq abrunden (< (ash numerator 1) abrund-einh))
          (if halbzahlig
            (setq aufrunden
              (>= (ash numerator 1) (- (ash denominator 1) aufrund-einh))
            )
            (setq aufrunden
              (> (ash numerator 1) (- (ash denominator 1) aufrund-einh))
            )
          )
          (when (or abrunden aufrunden
                    (and letzte-stelle (<= stelle letzte-stelle))
                )
            (return)
          )
          (vector-push-extend (schar "0123456789" digit) digit-string)
          (incf digit-count)
        )
        ; letzte signifikante Ziffer ausgeben:
        (when (or (null letzte-stelle) (>= stelle letzte-stelle))
          (vector-push-extend
            (schar "0123456789"
              (cond
                ((and abrunden (not aufrunden)) digit)
                ((and aufrunden (not abrunden)) (1+ digit))
                ((<= (ash numerator 1) denominator) digit)
                (t (1+ digit))
            ) )
            digit-string
          )
          (incf digit-count)
        )
        ; Nachfolgende Nullen und Punkt ausgeben
        (when (>= stelle 0)
          (dotimes (i stelle)
            (incf digit-count)
            (vector-push-extend #\0 digit-string)
          )
          (vector-push-extend #\. digit-string)
          (setq point-pos digit-count)
        )
        (when d
          (dotimes (i (- d (- digit-count point-pos)))
            (incf digit-count)
            (vector-push-extend #\0 digit-string)
        ) )
        (values
                  digit-string               ; Ziffern
                  (1+ digit-count)           ; Anzahl der Ziffern
                  (= point-pos 0)            ; Punkt ganz vorne?
                  (= point-pos digit-count)  ; Punkt ganz hinten?
                  point-pos                  ; Position des Punktes
        ) ; 5 Werte
) ) ) )
)

; (format-float-for-f w d k overflowchar padchar plus-sign-flag arg stream)
; gibt die Floating-Point-Zahl arg in Festkommadarstellung auf stream aus.
(defun format-float-for-f (w d k overflowchar padchar plus-sign-flag arg stream)
  (let ((width (if w (if (or plus-sign-flag (minusp arg)) (1- w) w) nil)))
    ; width = zur Verfügung stehende Zeichen ohne Vorzeichen
    (multiple-value-bind (digits digitslength leadingpoint trailingpoint)
        (format-float-to-string arg width d k nil)
      (when (eql d 0) (setq trailingpoint nil)) ; d=0 -> keine Zusatz-Null hinten
      (when w
        (setq width (- width digitslength))
        (when leadingpoint ; evtl. Zusatz-Null vorne einplanen
          (if (> width 0) (setq width (1- width)) (setq leadingpoint nil))
        )
        (when trailingpoint ; evtl. Zusatz-Null hinten einplanen
          (if (> width 0) (setq width (1- width)) (setq trailingpoint nil))
        )
      )
      ; Es bleiben noch width Zeichen übrig.
      (if (and overflowchar w (minusp width))
        (format-padding w overflowchar stream) ; Zu wenig Platz -> overflow
        (progn
          (when (and w (> width 0)) (format-padding width padchar stream))
          (if (minusp arg)
            (write-char #\- stream)
            (if plus-sign-flag (write-char #\+ stream))
          )
          (when leadingpoint (write-char #\0 stream))
          (write-string digits stream)
          (when trailingpoint (write-char #\0 stream))
      ) )
) ) )

; (format-float-for-e w d e k overflowchar padchar exponentchar plus-sign-flag
;                     arg stream)
; gibt die Floating-point-Zahl arg in Exponentialdarstellung auf den stream aus.
; (vgl. CLTL S.392-394)
; Aufteilung der Mantisse:
;   Falls k<=0, erst 1 Null (falls von der Breite her passend), dann der Punkt,
;               dann |k| Nullen, dann d-|k| signifikante Stellen;
;               zusammen also d Nachkommastellen.
;   Falls k>0,  erst k signifikante Stellen, dann der Punkt,
;               dann weitere d-k+1 signifikante Stellen;
;               zusammen also d+1 signifikante Stellen. Keine Nullen vorne.
;   (Der Defaultwert in FORMAT-EXPONENTIAL-FLOAT ist k=1.)
; Vor der Mantisse das Vorzeichen (ein + nur falls arg>=0 und plus-sign-flag).
; Dann der Exponent, eingeleitet durch exponentchar, dann Vorzeichen des
; Exponenten (stets + oder -), dann e Stellen für den Exponenten.
; Dann wird das Ganze mit padchars auf w Zeichen Breite aufgefüllt.
; Sollte das (auch nach evtl. Unterdrückung einer führenden Null) mehr als
; w Zeichen ergeben, so werden statt dessen w overflowchars ausgegeben, oder
; (falls overflowchar = nil) die Zahl mit so vielen Stellen wie nötig
; ausgegeben.
(defun format-float-for-e (w d e k
       overflowchar padchar exponentchar plus-sign-flag arg stream)
  (multiple-value-bind (mantissa oldexponent) (format-scale-exponent (abs arg))
    (let* ((exponent (if (zerop arg) 0 (- oldexponent k))) ; auszugebender Exponent
           (expdigits (write-to-string (abs exponent) :base 10. :radix nil))
           (expdigitsneed (if e (max (length expdigits) e) (length expdigits)))
           ; expdigitsneed = Anzahl der Stellen, die für die Ziffern des
           ; Exponenten nötig sind.
           (mantd (if d (if (> k 0) (1+ (- d k)) d) nil))
           ; mantd = Anzahl der Mantissenstellen hinter dem Punkt
           (dmin (if (minusp k) (- 1 k) nil)) ; nachher: fordere, daß
           ; nicht in die ersten (+ 1 (abs k)) Stellen hineingerundet wird.
           (mantwidth (if w (- w 2 expdigitsneed) nil))
           ; mantwidth = Anzahl der für die Mantisse (inkl. Vorzeichen, Punkt)
           ; zur Verfügung stehenden Zeichen (oder nil)
          )
      (declare (simple-string expdigits) (fixnum exponent expdigitsneed))
      (if (and overflowchar w e (> expdigitsneed e))
        ; Falls Overflowchar und w und e angegeben, Exponent mehr braucht:
        (format-padding w overflowchar stream)
        (progn
          (if w
            (if (or plus-sign-flag (minusp arg)) (setq mantwidth (1- mantwidth)))
          )
          ; mantwidth = Anzahl der für die Mantisse (ohne Vorzeichen,
          ; inklusive Punkt) zur Verfügung stehenden Zeichen (oder nil)
          (multiple-value-bind (mantdigits mantdigitslength
                                leadingpoint trailingpoint)
              (format-float-to-string mantissa mantwidth mantd k dmin)
            (when w
              (setq mantwidth (- mantwidth mantdigitslength))
              (if trailingpoint
                (if (or (null mantd) (> mantd 0))
                  (setq mantwidth (- mantwidth 1))
                  (setq trailingpoint nil)
              ) )
              (if leadingpoint
                (if (> mantwidth 0)
                  (setq mantwidth (- mantwidth 1))
                  (setq leadingpoint nil)
              ) )
            )
            ; Es bleiben noch mantwidth Zeichen übrig.
            (if (and overflowchar w (minusp mantwidth))
              (format-padding w overflowchar stream) ; Zu wenig Platz -> overflow
              (progn
                (when (and w (> mantwidth 0))
                  (format-padding mantwidth padchar stream)
                )
                (if (minusp arg)
                  (write-char #\- stream)
                  (if plus-sign-flag (write-char #\+ stream))
                )
                (if leadingpoint (write-char #\0 stream))
                (write-string mantdigits stream)
                (if trailingpoint (write-char #\0 stream))
                (write-char
                  (cond (exponentchar)
                        ((typep arg *READ-DEFAULT-FLOAT-FORMAT*) #\E)
                        ((short-float-p arg) #\s)
                        ((single-float-p arg) #\f)
                        ((double-float-p arg) #\d)
                        ((long-float-p arg) #\L)
                  )
                  stream
                )
                (write-char (if (minusp exponent) #\- #\+) stream)
                (when (and e (> e (length expdigits)))
                  (format-padding (- e (length expdigits)) #\0 stream)
                )
                (write-string expdigits stream)
          ) ) )
    ) ) )
) )

; Rückt *FORMAT-CSDL* vor bis zum Ende des momentanen ~[ bzw. ~{ bzw. ~< .
(defun format-skip-to-end ()
  (do ()
      ((null (csd-clause-chain (car *FORMAT-CSDL*))))
    (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*)))
) )

; (format-justified-segments mincol colinc minpad justify-left justify-right
;   piecelist) berechnet, an welchen Stellen zwischen den einzelnen Strings in
; piecelist wieviele Leerstellen zu setzen sind.
; Zwischen die einzelnen Strings aus piecelist (auch vorher, falls justify-left;
; auch nachher, falls justify-right) werden mindestens minpad padding-characters
; eingefügt. Dann werden nochmals weitere padding-characters dazugenommen,
; damit die Gesamtbreite >= mincol wird. Ist die Breite > mincol, werden weitere
; padding-characters dazugenommen, so daß die Breite von der Form
; mincol + k * colinc wird. Diese padding-characters werden auf die einzelnen
; Stellen gleichmäßig verteilt.
; 1. Wert: Ein Vektor, der zu jeder Stelle angibt, wieviele padding-characters
; einzufügen sind (NIL = keine).
; Erstes Element: ganz links, zweites: nach 1. String, ..., letztes: rechts.
; 2. Wert: Die sich ergebende Gesamtbreite.
(defun format-justified-segments
       (mincol colinc minpad justify-left justify-right piecelist)
  (declare (fixnum mincol colinc minpad))
  (let ((piecesnumber 0)
        (pieceswidth 0))
    (dolist (piece piecelist)
      (declare (simple-string piece))
      (incf piecesnumber)
      (incf pieceswidth (length piece))
    )
    (let* ((new-justify-left
             (or justify-left (and (= piecesnumber 1) (not justify-right))))
           (padblocks (+ piecesnumber -1       ; Anzahl der Einfüge-Stellen
                         (if new-justify-left 1 0) (if justify-right 1 0)
           )          )
           (width-need (+ pieceswidth (* padblocks minpad)))
           (width (+ mincol
                     (if (<= width-need mincol)
                         0
                         (* (ceiling (- width-need mincol) colinc) colinc)
          ))      )  )
      (declare (fixnum piecesnumber pieceswidth padblocks width-need width))
      (multiple-value-bind (padwidth rest) (floor (- width pieceswidth) padblocks)
        (let ((padblock-lengths
                (make-array (1+ piecesnumber) :initial-element padwidth)
             ))
          (unless new-justify-left (setf (svref padblock-lengths 0) nil))
          (unless justify-right (setf (svref padblock-lengths piecesnumber) nil))
          (do ((i 0 (1+ i)))
              ((zerop rest))
            (when (svref padblock-lengths i)
              (incf (svref padblock-lengths i))
              (decf rest)
          ) )
          (values padblock-lengths width)
) ) ) ) )

;-------------------------------------------------------------------------------

; ~A CLTL S.387-388
(defun format-ascii (stream colon-modifier atsign-modifier
             &optional (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (if (null mincol) (setq mincol 0))
  (if (null colinc) (setq colinc 1))
  (if (null minpad) (setq minpad 0))
  (if (null padchar) (setq padchar #\Space))
  (let ((arg (next-arg)))
    (when (and colon-modifier (null arg)) (setq arg "()"))
    (if (and (zerop mincol) (zerop minpad))
      (princ arg stream)
      (format-padded-string mincol colinc minpad padchar
        atsign-modifier ; =: padleftflag
        (princ-to-string arg)
        stream
) ) ) )

; ~S CLTL S.388
(defun format-s-expression (stream colon-modifier atsign-modifier
             &optional (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (if (null mincol) (setq mincol 0))
  (if (null colinc) (setq colinc 1))
  (if (null minpad) (setq minpad 0))
  (if (null padchar) (setq padchar #\Space))
  (let ((arg (next-arg)))
    (if (and (zerop mincol) (zerop minpad))
      (if (and colon-modifier (null arg))
        (write-string "()" stream)
        (prin1 arg stream)
      )
      (format-padded-string mincol colinc minpad padchar
        atsign-modifier ; =: padleftflag
        (if (and colon-modifier (null arg)) "()" (prin1-to-string arg))
        stream
) ) ) )

; ~W
(defun format-write (stream colon-modifier atsign-modifier
             &optional (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (declare (ignore colon-modifier))
  (if (null mincol) (setq mincol 0))
  (if (null colinc) (setq colinc 1))
  (if (null minpad) (setq minpad 0))
  (if (null padchar) (setq padchar #\Space))
  (let ((arg (next-arg)))
    (if (and (zerop mincol) (zerop minpad))
      (write arg :stream stream)
      (format-padded-string mincol colinc minpad padchar
        atsign-modifier ; =: padleftflag
        (write-to-string arg)
        stream
) ) ) )

; ~D, CLTL S.388
(defun format-decimal (stream colon-modifier atsign-modifier
                       &optional (mincol 0) (padchar #\Space) (commachar #\,))
  (format-base 10 stream colon-modifier atsign-modifier mincol padchar commachar)
)

; ~B, CLTL S.388
(defun format-binary (stream colon-modifier atsign-modifier
                      &optional (mincol 0) (padchar #\Space) (commachar #\,))
  (format-base 2 stream colon-modifier atsign-modifier mincol padchar commachar)
)

; ~O, CLTL S.388
(defun format-octal (stream colon-modifier atsign-modifier
                     &optional (mincol 0) (padchar #\Space) (commachar #\,))
  (format-base 8 stream colon-modifier atsign-modifier mincol padchar commachar)
)

; ~X, CLTL S.388-389
(defun format-hexadecimal (stream colon-modifier atsign-modifier
                        &optional (mincol 0) (padchar #\Space) (commachar #\,))
  (format-base 16 stream colon-modifier atsign-modifier mincol padchar commachar)
)

; ~R, CLTL S.389
(defun format-radix (stream colon-modifier atsign-modifier
            &optional (radix nil) (mincol 0) (padchar #\Space) (commachar #\,))
  (if (null mincol) (setq mincol 0))
  (if (null padchar) (setq padchar #\Space))
  (if (null commachar) (setq commachar #\,))
  (let ((arg (next-arg)))
    (if radix
      (format-integer radix mincol padchar commachar
                      colon-modifier atsign-modifier
                      arg stream
      )
      (if atsign-modifier
        (if (integerp arg)
          (if colon-modifier
            (format-old-roman arg stream)
            (format-new-roman arg stream)
          )
          (format-error *FORMAT-CS* nil
            #+DEUTSCH "Die ~~R- und ~~:R-Direktiven erwarten ein Integer als Argument, nicht ~S"
            #+ENGLISH "The ~~R and ~~:R directives require an integer argument, not ~S"
            #+FRANCAIS "Les directives ~~R et ~~:R nécessitent un argument de type entier et non ~S"
            arg
        ) )
        (if colon-modifier
          (format-ordinal arg stream)
          (format-cardinal arg stream)
) ) ) ) )

; ~P, CLTL S. 389
(defun format-plural (stream colon-modifier atsign-modifier)
  (when colon-modifier (format-goto-new-arg t 1))
  (let ((singular (eql (next-arg) 1)))
    (if atsign-modifier
      (write-string (if singular "y" "ies") stream)
      (unless singular (write-char #\s stream))
) ) )

; ~C, CLTL S.389-390
(defun format-character (stream colon-modifier atsign-modifier)
  (let ((arg (next-arg)))
    (unless (characterp arg)
      (format-error *FORMAT-CS* nil
        #+DEUTSCH "Die ~~C-Direktive erwartet ein Character, nicht ~S"
        #+ENGLISH "The ~~C directive requires a character argument, not ~S"
        #+FRANCAIS "La directive ~~C requiert un caractère et non ~S"
        arg
    ) )
    (flet ((write-charname (arg)
             (let ((name (char-name arg)))
               (if name
                 (write-string (string-capitalize name) stream)
                 (write-char arg stream)
          )) ) )
      (if (not atsign-modifier)
        ; ~C oder ~:C
        (progn
          (dolist (name '(:CONTROL :META :SUPER :HYPER))
            (when (char-bit arg name)
              (write-string (string-capitalize (symbol-name name)) stream
                            :end (if colon-modifier nil 1)
              )
              (write-char #\- stream)
          ) )
          (write-charname (make-char arg))
        )
        (if (not colon-modifier)
          ; ~@C
          (prin1 arg stream)
          ; ~:@C -- hier NUR die Anweisung, wie's zu tippen ist.
          (progn
            (let ((keynames '("Shift-" "Control-" "Alternate-")))
              (dolist (name '(:SUPER :CONTROL :META))
                (when (char-bit arg name)
                  (write-string (car keynames) stream)
                  (setq arg (set-char-bit arg name nil))
                )
                (setq keynames (cdr keynames))
            ) )
            (let* ((hyperkey-alist
                     #+(or ATARI DOS OS/2 UNIX VMS)
                     '(
       #-(or UNIX VMS) (#\Enter  . "Enter" )
                       (#\Insert . "Insert")
                       (#\End    . "End"   )
                       (#\Down   . #-ATARI "Down"  #+ATARI "")
                       (#\PgDn   . "PgDn"  )
                       (#\Left   . #-ATARI "Left"  #+ATARI "")
       #+(or UNIX VMS) (#\Center . "Center")
                       (#\Right  . #-ATARI "Right" #+ATARI "")
                       (#\Home   . #-ATARI "Home"  #+ATARI "Clr/Home")
                       (#\Up     . #-ATARI "Up"    #+ATARI "")
                       (#\PgUp   . "PgUp"  )
               #+ATARI (#\Help   . "Help"  )
               #+ATARI (#\Undo   . "Undo"  )
       #+(or DOS OS/2) (#\Prtscr . "PrtScr")
       #-(or UNIX VMS) (#\Delete . "Delete")
                       (#\F1     . "F1"    )
                       (#\F2     . "F2"    )
                       (#\F3     . "F3"    )
                       (#\F4     . "F4"    )
                       (#\F5     . "F5"    )
                       (#\F6     . "F6"    )
                       (#\F7     . "F7"    )
                       (#\F8     . "F8"    )
                       (#\F9     . "F9"    )
                       (#\F10    . "F10"   )
                       (#\F11    . "F11"   )
                       (#\F12    . "F12"   )
                      )
                     #-(or ATARI DOS OS/2 UNIX VMS)
                     '()
                   )
                   (acons (assoc arg hyperkey-alist)))
              (if acons
                (write-string (cdr acons) stream)
                (progn
                  (when (char-bit arg ':HYPER)
                    (write-string #+DEUTSCH "Ziffernblock-"
                                  #+ENGLISH "Keypad-"
                                  #+FRANCAIS "Keypad-" ; ??
                                  stream
                    )
                    (setq arg (set-char-bit arg :HYPER nil))
                  )
                  (write-charname arg)
          ) ) ) )
) ) ) ) )

; ~F, CLTL S.390-392
(defun format-fixed-float (stream colon-modifier atsign-modifier
       &optional (w nil) (d nil) (k 0) (overflowchar nil) (padchar #\Space))
  (declare (ignore colon-modifier))
  (if (null k) (setq k 0))
  (if (null padchar) (setq padchar #\Space))
  (let ((arg (next-arg)))
    (when (rationalp arg) (setq arg (float arg)))
    (if (floatp arg)
      (format-float-for-f w d k overflowchar padchar atsign-modifier arg stream)
      (format-ascii-decimal arg stream)
) ) )

; ~E, CLTL S.392-395
(defun format-exponential-float (stream colon-modifier atsign-modifier
          &optional (w nil) (d nil) (e nil) (k 1)
                    (overflowchar nil) (padchar #\Space) (exponentchar nil))
  (declare (ignore colon-modifier))
  (if (null k) (setq k 1))
  (if (null padchar) (setq padchar #\Space))
  (let ((arg (next-arg)))
    (when (rationalp arg) (setq arg (float arg)))
    (if (floatp arg)
      (format-float-for-e w d e k overflowchar padchar exponentchar
                          atsign-modifier arg stream
      )
      (format-ascii-decimal arg stream)
) ) )

; ~G, CLTL S.395-396
(defun format-general-float (stream colon-modifier atsign-modifier
          &optional (w nil) (d nil) (e nil) (k 1)
                    (overflowchar nil) (padchar #\Space) (exponentchar nil))
  (declare (ignore colon-modifier))
  (if (null k) (setq k 1))
  (if (null padchar) (setq padchar #\Space))
  (let ((arg (next-arg)))
    (if (rationalp arg) (setq arg (float arg)))
    (if (floatp arg)
      (multiple-value-bind (mantissa n) (format-scale-exponent (abs arg))
        (declare (ignore mantissa))
        (if (null d)
          (setq d
            (multiple-value-bind (digits digitslength)
              (format-float-to-string (abs arg) nil nil nil nil)
              (declare (ignore digits))
              (max (max (1- digitslength) 1) (min n 7))
        ) ) )
        (let* ((ee (if e (+ 2 e) 4))
               (dd (- d n)))
          (if (<= 0 dd d)
            (progn
              (format-float-for-f
                (if w (- w ee) nil)
                dd 0
                overflowchar padchar atsign-modifier arg stream
              )
              (format-padding ee #\Space stream)
            )
            (format-float-for-e w d e k overflowchar padchar exponentchar
                                atsign-modifier arg stream
      ) ) ) )
      (format-ascii-decimal arg stream)
) ) )

; ~$, CLTL S.396-397
(defun format-dollars-float (stream colon-modifier atsign-modifier
          &optional (d 2) (n 1) (w 0) (padchar #\Space) )
  (if (null d) (setq d 2))
  (if (null n) (setq n 1))
  (if (null w) (setq w 0))
  (if (null padchar) (setq padchar #\Space))
  (let ((arg (next-arg)))
    (when (rationalp arg) (setq arg (float arg)))
    (if (floatp arg)
      (multiple-value-bind (digits digitslength
                            leadingpoint trailingpoint leadings)
        (format-float-to-string arg nil d 0 nil)
        (declare (ignore digitslength leadingpoint trailingpoint))
        (let* ((lefts (max leadings n))
               (totalwidth (+ (if (or atsign-modifier (minusp arg)) 1 0)
                              lefts 1 d
               )           )
               (padcount (max (- w totalwidth) 0)))
          (if (not colon-modifier) (format-padding padcount padchar stream))
          (if (minusp arg)
            (write-char #\- stream)
            (if atsign-modifier (write-char #\+ stream))
          )
          (if colon-modifier (format-padding padcount padchar stream))
          (format-padding (- lefts leadings) #\0 stream)
          (write-string digits stream)
      ) )
      (format-ascii-decimal arg stream)
) ) )

; ~%, CLTL S.397
(defun format-terpri (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (terpri stream))
)

; ~&, CLTL S.397
(defun format-fresh-line (stream colon-modifier atsign-modifier
                          &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (when (plusp count)
    (fresh-line stream)
    (dotimes (i (1- count)) (terpri stream))
) )

; ~|, CLTL S.397
(defun format-page (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (write-char #\Page stream))
)

; ~~, CLTL S.397
(defun format-tilde (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (write-char #\~ stream))
)

; ~T, CLTL S.398-399
(defun format-tabulate (stream colon-modifier atsign-modifier
                        &optional (colnum 1) (colinc 1))
  (declare (ignore colon-modifier))
  (if (null colnum) (setq colnum 1))
  (if (null colinc) (setq colinc 1))
  (let* ((new-colnum (max colnum 0))
         (new-colinc (max colinc 1)) ; >0
         (pos (sys::line-position stream))) ; aktuelle Position, Fixnum >=0
    (if atsign-modifier
      (format-padding
        (+ new-colnum (mod (- (+ pos new-colnum)) new-colinc))
        #\Space stream
      )
      (if (< pos new-colnum)
        (format-padding (- new-colnum pos) #\Space stream)
        (unless (zerop colinc)
          (format-padding (+ colinc (mod (- new-colnum pos) (- colinc)))
                          #\Space stream
) ) ) ) ) )

; ~*, CLTL S.399
(defun format-goto (stream colon-modifier atsign-modifier &optional (index nil))
  (declare (ignore stream))
  (if atsign-modifier
    (setq *FORMAT-NEXT-ARG* (nthcdr (or index 0) *FORMAT-ARG-LIST*))
    (format-goto-new-arg colon-modifier (or index 1))
) )

; ~?, CLTL S.399-401
(defun format-indirection (stream colon-modifier atsign-modifier)
  (declare (ignore colon-modifier))
  (let ((csarg (next-arg)))
    (unless (stringp csarg)
      (format-error *FORMAT-CS* nil
        #+DEUTSCH "Als Kontrollstring für ~~? ist das untauglich: ~S"
        #+ENGLISH "The control string argument for the ~~? directive is invalid: ~S"
        #+FRANCAIS "~S ne convient pas comme chaîne de contrôle pour ~~?."
        csarg
    ) )
    ; evtl. noch csarg zu einem Simple-String machen ??
    (let ((node (list csarg)))
      (format-parse-cs csarg 0 node nil)
      (if atsign-modifier
        (let ((*FORMAT-CS* (car node))
              (*FORMAT-CSDL* (cdr node))
              (*FORMAT-UP-AND-OUT* nil))
          (format-interpret stream)
        )
        (let ((arglistarg (next-arg)))
          (unless (listp arglistarg)
            (format-error *FORMAT-CS* nil
              #+DEUTSCH "Das ist keine passende Argumentliste für die ~~?-Direktive: ~S"
              #+ENGLISH "The argument list argument for the ~~? directive is invalid: ~S"
              #+FRANCAIS "Ceci n'est pas une liste d'arguments convenable pour la directive ~~? : ~S"
              arglistarg
          ) )
          (let* ((*FORMAT-CS* (car node))
                 (*FORMAT-CSDL* (cdr node))
                 (*FORMAT-ARG-LIST* arglistarg)
                 (*FORMAT-NEXT-ARG* *FORMAT-ARG-LIST*))
            (format-interpret stream)
) ) ) ) ) )

; ~(, CLTL S.401
(defun format-case-conversion (stream colon-modifier atsign-modifier)
  (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
  (let ((tempstr
          (let ((tempstream (make-string-output-stream (sys::line-position stream))))
            (format-interpret tempstream 'FORMAT-CASE-CONVERSION-END)
            (get-output-stream-string tempstream)
       )) )
    (if colon-modifier
      (if atsign-modifier
        (write-string (nstring-upcase tempstr) stream)
        (write-string (nstring-capitalize tempstr) stream)
      )
      (if atsign-modifier
        (progn
          (setq tempstr (nstring-downcase tempstr))
          (dotimes (i (length tempstr)) ; erstes Zeichen zum Upcase machen
            (when (both-case-p (schar tempstr i))
              (setf (schar tempstr i) (char-upcase (schar tempstr i)))
              (return)
          ) )
          (write-string tempstr stream)
        )
        (write-string (nstring-downcase tempstr) stream)
) ) ) )

; ~[, CLTL S.402-403
(defun format-conditional (stream colon-modifier atsign-modifier
                           &optional (prefix nil))
  (if colon-modifier
    (if atsign-modifier
      (format-error *FORMAT-CS* nil
        #+DEUTSCH "~~[ geht nicht mit : und @ gleichzeitig."
        #+ENGLISH "The ~~[ directive cannot take both modifiers."
        #+FRANCAIS "La directive ~~[ ne peut pas accepter les deux qualificateurs : et @ en même temps."
      )
      (progn
        (when (next-arg)
          (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*)))
        )
        (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
        (format-interpret stream 'FORMAT-CONDITIONAL-END)
      )
    )
    (if atsign-modifier
      (when (next-arg)
        (format-goto-new-arg t 1)
        (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
        (format-interpret stream 'FORMAT-CONDITIONAL-END)
      )
      (let ((index (or prefix (next-arg))))
        (unless (integerp index)
          (format-error *FORMAT-CS* nil
            #+DEUTSCH "Argument für ~~[ muß ein Integer sein, nicht ~S"
            #+ENGLISH "The ~~[ parameter must be an integer, not ~S"
            #+FRANCAIS "L'argument pour ~~[ doit être un entier et non ~S" 
            index
        ) )
        (dotimes (i (if (minusp index) most-positive-fixnum index))
          (when (eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-CONDITIONAL-END)
            (return)
          )
          (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*)))
          (when (csd-colon-p (car *FORMAT-CSDL*)) (return))
        )
        (unless (eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-CONDITIONAL-END)
          (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
        )
        (format-interpret stream 'FORMAT-CONDITIONAL-END)
  ) ) )
  (format-skip-to-end) ; Weiterrücken bis ans Ende der ~[...~]-Direktive
)

; ~{, CLTL S.403-404
(defun format-iteration (stream colon-modifier atsign-modifier
                         &optional (prefix nil))
  (let* ((total-csdl *FORMAT-CSDL*)
         (max-iteration-count prefix))
    (format-skip-to-end) ; Weiterrücken bis ans Ende der ~{...~}-Direktive
    (let* ((min-1-iteration (csd-colon-p (car *FORMAT-CSDL*)))
           (inner-cs (if (eq (cdr total-csdl) *FORMAT-CSDL*)
                       (next-arg)
                       *FORMAT-CS*
           )         )
           (inner-csdl (if (eq (cdr total-csdl) *FORMAT-CSDL*)
                         (let ((node (list inner-cs)))
                           (format-parse-cs inner-cs 0 node nil)
                           (cdr node)
                         )
                         (cdr total-csdl)
           )           )
           (arg-list-rest (if (not atsign-modifier)
                            (let ((arg (next-arg)))
                              (unless (listp arg)
                                (format-error *FORMAT-CS* nil
                                  #+DEUTSCH "Das Argument zu ~~{ muß eine Liste sein, nicht ~S"
                                  #+ENGLISH "The ~~{ directive requires a list argument, not ~S"
                                  #+FRANCAIS "L'argument de ~~{ doit être une liste et non ~S"
                                  arg
                              ) )
                              arg
          ))              ) )
      (do* ((iteration-count 0 (1+ iteration-count)))
           ((or (and max-iteration-count
                     (>= iteration-count max-iteration-count)
                )
                (let ((remaining (if atsign-modifier
                                   *FORMAT-NEXT-ARG*
                                   arg-list-rest
                     ))          )
                  (if min-1-iteration
                    (and (plusp iteration-count) (null remaining))
                    (null remaining)
           ))   ) )
        (if colon-modifier
          (let* ((*FORMAT-ARG-LIST*
                   (if atsign-modifier (next-arg) (pop arg-list-rest))
                 )
                 (*FORMAT-NEXT-ARG* *FORMAT-ARG-LIST*)
                 (*FORMAT-CS* inner-cs)
                 (*FORMAT-CSDL* inner-csdl)
                 (*FORMAT-UP-AND-OUT* nil))
            (format-interpret stream 'FORMAT-ITERATION-END)
            (when (eq *FORMAT-UP-AND-OUT* ':TERMINATE-ALL) (return))
          )
          (if atsign-modifier
            (let* ((*FORMAT-CS* inner-cs)
                   (*FORMAT-CSDL* inner-csdl)
                   (*FORMAT-UP-AND-OUT* nil))
              (format-interpret stream 'FORMAT-ITERATION-END)
              (when (eq *FORMAT-UP-AND-OUT* ':TERMINATE-ALL) (return))
            )
            (let* ((*FORMAT-ARG-LIST* arg-list-rest)
                   (*FORMAT-NEXT-ARG* *FORMAT-ARG-LIST*)
                   (*FORMAT-CS* inner-cs)
                   (*FORMAT-CSDL* inner-csdl)
                   (*FORMAT-UP-AND-OUT* nil))
              (format-interpret stream 'FORMAT-ITERATION-END)
              (setq arg-list-rest *FORMAT-NEXT-ARG*)
              (when (eq *FORMAT-UP-AND-OUT* ':TERMINATE-ALL) (return))
) ) ) ) ) ) )

; ~<, CLTL S.404-406
(defun format-justification (stream colon-modifier atsign-modifier
       &optional (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (if (null mincol) (setq mincol 0))
  (if (null colinc) (setq colinc 1))
  (if (null minpad) (setq minpad 0))
  (if (null padchar) (setq padchar #\Space))
  (let* ((saved-csdl *FORMAT-CSDL*)
         (pos (sys::line-position stream))
         (tempstream (make-string-output-stream pos))
         (check-on-line-overflow nil)
         supplementary-need
         line-length
         (old-piecelist
           (let ((pieces nil))
             (do ((first-piece-flag t nil))
                 ((eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-JUSTIFICATION-END))
               (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
               (let ((*FORMAT-UP-AND-OUT* nil))
                 (format-interpret tempstream 'FORMAT-JUSTIFICATION-END)
                 (when (and first-piece-flag (eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-SEPARATOR))
                   (when (setq check-on-line-overflow (csd-colon-p (car *FORMAT-CSDL*)))
                     (multiple-value-setq (supplementary-need line-length)
                       (values-list (format-resolve-parms (car *FORMAT-CSDL*)))
                 ) ) )
                 (when *FORMAT-UP-AND-OUT*
                   (setq *FORMAT-CSDL* saved-csdl)
                   (format-skip-to-end)
                   (return)
                 )
                 (push (get-output-stream-string tempstream) pieces)
             ) )
             (nreverse pieces)
         ) )
         (piecelist
           (if check-on-line-overflow (cdr old-piecelist) old-piecelist)
        ))
    (if piecelist
      (multiple-value-bind (padblocklengths width)
        (format-justified-segments mincol colinc minpad
          colon-modifier atsign-modifier piecelist)
        (when (and check-on-line-overflow
                   (> (+ pos width (or supplementary-need 0))
                      (or line-length #|(sys::line-length stream)|# 72)
              )    )
          (write-string (first old-piecelist) stream)
        )
        (do ((i 0 (1+ i)))
            (nil)
          (when (svref padblocklengths i)
            (format-padding (svref padblocklengths i) padchar stream)
          )
          (when (null piecelist) (return))
          (write-string (pop piecelist) stream)
      ) )
      (format-padding mincol padchar stream)
    )
) )

; ~^, CLTL S.406-407
(defun format-up-and-out (stream colon-modifier atsign-modifier
                          &optional (a nil) (b nil) (c nil))
  (declare (ignore stream atsign-modifier))
  (if (cond ((and (null a) (null b) (null c)) ; keine Parameter
             (null *FORMAT-NEXT-ARG*)
            )
            ((and (null b) (null c)) (eql a 0)) ; ein Parameter
            ((null c) (eql a b)) ; zwei Parameter
            ((and (integerp a) (integerp b) (integerp c)) (<= a b c))
            ((and (characterp a) (characterp b) (characterp c)) (char<= a b c))
      )
    (setq *FORMAT-UP-AND-OUT* (if colon-modifier ':TERMINATE-ALL ':TERMINATE))
) )

;-------------------------------------------------------------------------------

