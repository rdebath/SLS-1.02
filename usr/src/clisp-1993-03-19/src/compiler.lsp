; CLISP - Compiler
; Bruno Haible 20.-30.09.1988, 05.-07.10.1988, 10.10.1988, 16.12.1988
;   Version für KCL 27.06.1989, 05.-07.07.1989
;   c-VALUES erweitert am 14.07.1989
;   label-operand in assemble-LAP korrigiert am 14.07.1989
;   ANODE-Komponenten SOURCE, STACKZ eliminiert am 14.07.1989
;     (konditionell von #+COMPILER-DEBUG abhängig)
;   Peephole-Optimierung-Protokoll konditionell von #+PEEPHOLE-DEBUG abhängig
;   Version für CLISP 28.07.1989-11.08.1989
;   Variablen-Optimierungen 10.03.1991
; Michael Stoll, September-Dezember 1991:
;   - Bytecode überarbeitet
;   - Code-Optimierung bzgl. Labels/Sprüngen verbessert
;   - kleine Verbesserung bei c-plus/c-minus,
;     Compilation von CxxxR in Folge von (CAR) und (CDR)
;   - SUBR-Aufrufe ohne Argument-Check zur Laufzeit,
;     SUBRs als Konstanten (via #.#'name)
;   - Aufrufe lokaler Funktionen ohne Argument-Check zur Laufzeit
;   - Rekursive Aufrufe durch Unterprogrammaufruf JSR, bei Endrekursion
;     JMPTAIL (entspricht PSETQ mit anschließendem Sprung)
;   - Verbesserung bei Aufruf einer Funktion mit Rest-Parametern via APPLY
; Bruno Haible, Februar-März 1992:
;   - detailliertere seclass, besseres PSETQ
;   - besseres Constant Folding
;   - Cross-Compilation
; Bruno Haible, 03.06.1992:
;   - Inline-Compilation von Aufrufen globaler Funktionen
; Weitere Vorhaben:
;   - Variablen-Environments so verändern, daß Aufruf von lokalen Funktionen
;     mittels JSR/JMPTAIL möglich wird (d.h. nachträgliche Entscheidung, ob
;     Aufruf durch CALLC oder JSR)
;   - evtl. bessere Optimierung durch Datenflußanalyse
;   - Inline-Compilation von Aufrufen lokaler Funktionen

; Zur Cross-Compilation (wahlweise mit #+CLISP oder #-CLISP):
; CROSS, die Sprache und den Maschinenbezeichner in die Liste *features*
; aufnehmen, andere Maschinenbezeichner aus *features* herausnehmen.
; Dann den Compiler laden (evtl. compilieren und laden).
; Dann CROSS wieder aus der Liste *features* herausnehmen, und
; mit (cross:compile-file ...) Files compilieren.

; #-CROSS impliziert #+CLISP.

#-CROSS (in-package "LISP")
#-CROSS (export '(compiler compile compile-file disassemble))
#-CROSS (pushnew 'compiler *features*)

#-CROSS (in-package "COMPILER")
#+CROSS (in-package "CROSS" :nicknames '("CLISP"))
#-CLISP '#.(progn #-(or DEUTSCH ENGLISH FRANCAIS) (pushnew 'ENGLISH *features*))
;; Konvention: Schreibe SYSTEM::PNAME für ein Symbol, das "zufällig" in
;; #<PACKAGE SYSTEM> sitzt, wir das Symbol aber nicht weiter benutzen.
;; Schreibe SYS::PNAME, wenn wir von dem Symbol irgendwelche Eigenschaften
;; voraussetzen. Schreibe COMPILER::PNAME, wenn der Compiler das Symbol
;; deklariert und es von anderen Programmteilen benutzt wird.
#+CLISP (import '(sys::parse-body sys::make-load-time-eval
                  sys::closure-name sys::closure-codevec sys::closure-consts
                  sys::fixnump sys::short-float-p sys::single-float-p
                  sys::double-float-p sys::long-float-p
                  sys::search-file sys::*date-format*
                  sys::%funtabref sys::inlinable
                  sys::*compiling* sys::*compiling-from-file* sys::*inline-functions*
                  sys::*venv* sys::*fenv* sys::*benv* sys::*genv* sys::*denv*
                  sys::*toplevel-denv*
                  COMPILER::C-PROCLAIM COMPILER::C-PROCLAIM-CONSTANT
                  COMPILER::C-DEFUN COMPILER::C-PROVIDE COMPILER::C-REQUIRE
        )        )
#-CROSS (import '(sys::version sys::subr-info))

#+CROSS (shadow '(compile-file))
#+CROSS (export '(compile-file))

#-CLISP (shadow '(macroexpand-1 macroexpand))
#-CLISP
(progn
  (defun macroexpand-1 (form &optional (env nil))
    (if (and (consp form) (symbolp (car form)))
      (multiple-value-bind (a b c) (fenv-search (car form) env)
        (declare (ignore c))
        (cond ((eq a 'system::macro) (values (funcall b form env) t))
              ((macro-function (car form))
               (values (funcall (macro-function (car form)) form env) t)
              )
              (t (values form nil))
      ) )
      (values form nil)
  ) )
  (defun macroexpand (form &optional (env nil))
    (multiple-value-bind (a b) (macroexpand-1 form env)
      (if b
        (loop
          (multiple-value-setq (a b) (macroexpand-1 a env))
          (unless b (return (values a t)))
        )
        (values form nil)
  ) ) )
  (defun parse-body (body &optional docstring-allowed env)
    (do ((bodyr body (cdr bodyr))
         (declarations nil)
         (docstring nil)
         (form nil))
        ((null bodyr) (values bodyr declarations docstring))
      (cond ((and (stringp (car bodyr)) (cdr bodyr) (null docstring) docstring-allowed)
             (setq docstring (car bodyr))
            )
            ((not (listp (setq form (macroexpand (car bodyr) env))))
             (return (values bodyr declarations docstring))
            )
            ((eq (car form) 'DECLARE)
             (dolist (decl (cdr form)) (push decl declarations))
            )
            (t (return (values bodyr declarations docstring)))
  ) ) )
  (defstruct (load-time-eval
              (:print-function
                (lambda (object stream depth)
                  (declare (ignore depth))
                  (write-string "#." stream)
                  (write (load-time-eval-form object) :stream stream)
              ) )
              (:constructor make-load-time-eval (form))
             )
    form
  )
  (defun fixnump (object) (typep object 'FIXNUM))
  (defun short-float-p (object) (typep object 'SHORT-FLOAT))
  (defun single-float-p (object) (typep object 'SINGLE-FLOAT))
  (defun double-float-p (object) (typep object 'DOUBLE-FLOAT))
  (defun long-float-p (object) (typep object 'LONG-FLOAT))
  ; Sucht ein Programm-File. Siehe INIT.LSP :
  (defun search-file (filename extensions
                      &aux (use-extensions (null (pathname-type filename))) )
    (when use-extensions
      (setq extensions ; Case-Konversionen auf den Extensions durchführen
        (mapcar #'pathname-type extensions)
    ) )
    ; Defaults einmergen:
    (setq filename (merge-pathnames filename '#".*"))
    ; Suchen:
    (let ((already-searched nil))
      (dolist (dir (cons '#"" *load-paths*))
        (let ((search-filename
                (merge-pathnames (merge-pathnames filename dir))
             ))
          (unless (member search-filename already-searched :test #'equal)
            (let ((xpathnames (directory search-filename :full t)))
              (when use-extensions
                ; nach passenden Extensions filtern:
                (setq xpathnames
                  (delete-if-not ; hat xpathname eine der gegebenen Extensions?
                    #'(lambda (xpathname)
                        (member (pathname-type (first xpathname)) extensions
                                :test #'string=
                      ) )
                    xpathnames
              ) ) )
              (when xpathnames
                ; nach Datum sortiert, zurückgeben:
                (dolist (xpathname xpathnames)
                  (setf (rest xpathname)
                        (apply #'encode-universal-time (third xpathname))
                ) )
                (return (mapcar #'first (sort xpathnames #'> :key #'rest)))
            ) )
            (push search-filename already-searched)
      ) ) )
  ) )
  (defun make-macro-expander (macrodef)
    (let ((dummysym (make-symbol (symbol-name (car macrodef)))))
      (eval `(DEFMACRO ,dummysym ,@(cdr macrodef)))
      #'(lambda (form &rest env)
          (apply #'lisp:macroexpand-1 (cons dummysym (cdr form)) env)
        )
  ) )
  ; siehe DEFS1.LSP :
  (defconstant *date-format*
    #+DEUTSCH "~1{~3@*~D.~4@*~D.~5@*~D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}"
    #+ENGLISH "~1{~5@*~D/~4@*~D/~3@*~D ~2@*~2,'0D.~1@*~2,'0D.~0@*~2,'0D~:}"
  )
)


; Version des Evaluators:
; CLISP1 : Assembler-Version
; CLISP2 : C-Version mit SP-Manipulierbarkeit
; CLISP3 : C-Version ohne SP-Manipulierbarkeit
#+(and CROSS (not (or CLISP1 CLISP2 CLISP3)))
(eval-when (eval load compile)
  (pushnew
    (if (y-or-n-p #+DEUTSCH "Die C-Version?"
                  #+ENGLISH "The C version?"
        )
      (if (y-or-n-p #+DEUTSCH "Kann man von C aus den SP verändern?"
                    #+ENGLISH "Can C manipulate the SP stack pointer?"
          )
        'CLISP2
        'CLISP3
      )
      'CLISP1
    )
    *features*
) )
#+CLISP1 (defconstant *jmpbuf-size* 1)
#+(and CROSS (not CLISP1))
(defconstant *jmpbuf-size*
  (progn
    (format *query-io* #+DEUTSCH "~%Bitte *jmpbuf-size* eingeben: "
                       #+ENGLISH "~%Please input *jmpbuf-size*: "
    )
    (read *query-io*)
) )
#+CLISP1 (defconstant *big-endian* t)
#+(and CROSS (not CLISP1))
(defconstant *big-endian*
  #+(or ATARI AMIGA SUN3 SUN4) t ; BIG-ENDIAN-Prozessor
  #+(or SUN386 PC386) nil ; LITTLE-ENDIAN-Prozessor
  #-(or ATARI AMIGA SUN3 SUN4 SUN386 PC386)
    (y-or-n-p #+DEUTSCH "Prozessor BIG-ENDIAN?"
              #+ENGLISH "processor big endian?"
    )
)
#+CROSS
(defun version ()
  (list ' #+CLISP1 SYSTEM::CLISP1 #+CLISP2 SYSTEM::CLISP2 #+CLISP3 SYSTEM::CLISP3
        *jmpbuf-size*
        *big-endian*
        '210292
) )

(defconstant *keyword-package* (find-package "KEYWORD"))
(defconstant *lisp-package* (find-package "LISP"))

; Variablen für Top-Level-Aufruf:
(defvar *compiling* nil) ; gibt an, ob gerade beim Compilieren
; (defvar *error-count*) ; Anzahl der aufgetretenen Errors
; (defvar *warning-count*) ; Anzahl der aufgetretenen Warnungen
(defvar *compile-warnings* t) ; ob Compiler-Warnungen ausgegeben werden
(defvar *compile-verbose* t) ; ob Compiler-Kommentare ausgegeben werden
(defvar *compiling-from-file*) ; NIL oder T wenn von COMPILE-FILE aufgerufen
(defvar *c-listing-output*) ; Compiler-Listing-Stream oder nil
(defvar *c-error-output*) ; Compiler-Error-Stream
; Es ist im wesentlichen
; *c-error-output* = (make-broadcast-stream *error-output* *c-listing-output*)
(defvar *known-special-vars*) ; Namen von deklarierten dynamischen Variablen
(defvar *constant-special-vars*) ; Namen und Werte von konstanten Variablen

; Variablen für COMPILE-FILE:
(defvar *liboutput-stream* nil) ; Compiler-Library-Stream oder nil
(defvar *functions-with-errors* nil) ; Namen der Funktionen, wo es Fehler gab
(defvar *known-functions*) ; Namen der bisher bekannten Funktionen,
                               ; wird vom Macroexpander von DEFUN verändert
(defvar *unknown-functions*) ; Namen der bisher unbekannten Funktionen
(defvar *unknown-free-vars*) ; Namen von undeklarierten dynamischen Variablen
(defvar *inline-functions*) ; global inline-deklarierte Funktionssymbole
(defvar *notinline-functions*) ; global notinline-deklarierte Funktionssymbole
(defvar *inline-definitions*) ; Aliste globaler inlinebarer Funktionsdefinitionen
(defvar *user-declaration-types*) ; global definierte zusätzliche Deklarationen
(defvar *compiled-modules*) ; bereits "geladene" (compilierte) Modulnamen
(defvar *package-tasks*) ; noch durchzuführende Package-Anforderungen

#|
Basis für den Zielcode ist eine Stackmaschine mit zwei Stacks:
STACK (Stack für LISP-Objekte und Frames) und SP (Stack für sonstiges).
Mehrfache Werte werden kurzfristig in A0/A1/A2/MV_SPACE (D7.W Werte, bei D7.W=0
ist A0=NIL) gehalten, längerfristig auf dem STACK abgelegt.

1. Pass des Compilers:
Macro-Expansion, Codegenerierung (symbolisch), Allokation von Variablen auf
dem STACK oder in Closures, Optimierung auf LISP-Ebene.
Danach steht für jede beteiligte Funktion das Stack-Layout fest.
Die Information steckt in einem Netz von ANODEs.
2. Pass des Compilers:
Auflösung der Variablenbezüge, Optimierung auf Code-Ebene
(Peephole-Optimierung), Kreation compilierter funktionaler Objekte.
3. Pass des Compilers:
Auflösung von Bezügen zwischen den einzelnen funktionalen Objekten.

Zielsprache:
============

ein Bytecode-Interpreter.

Ein compiliertes funktionales Objekt (Closure) hat folgenden Aufbau:
FUNC = #Closure( Name
                 CODEVEC
                 [VenvConst] {BlockConst}* {TagbodyConst}*
                 {Keyword}* {sonstige Const}*
               )

VenvConst, BlockConst, TagbodyConst : diese LISP-Objekte werden innerhalb der
Funktion als Konstanten betrachtet. Sie werden beim Aufbau der Funktion zur
Laufzeit mitgegeben. Sollten diese drei Teile fehlen (d.h. diese Funktion ist
von der Inkarnation unabhängig, weil sie auf keine lexikalischen Variablen,
Blocks oder Tags zugreift, die im compilierten Code außerhalb von ihr definiert
werden), so heißt die Funktion autonom.

Keyword : die Keywords in der richtigen Reihenfolge. Werden vom Interpreter bei
der Parameterübergabe gebraucht.

sonstige Const: sonstige Konstanten, auf die vom Innern der Funktion aus Bezug
genommen wird. Sie sind untereinander und zu allen Keywords paarweise nicht EQL.

CODEVEC = Code-Vektor, ein SIMPLE-BIT-VECTOR,
                 Falls nicht FAST_SP:
                   2 Bytes : maximale SP-Tiefe
                 2 Bytes : Anzahl der required parameter
                 2 Bytes : Anzahl der optionalen Parameter
                 1 Byte : Flags. Bit 0: ob &REST - Parameter angegeben
                                 Bit 7: ob Keyword-Parameter angegeben
                                 Bit 6: &ALLOW-OTHER-KEYS-Flag
                 1 Byte : Kürzel für den Argumenttyp, für schnelleres FUNCALL
                 Falls Keyword-Parameter angegeben:
                   4 Bytes : 2 Bytes : Anzahl der Keyword-Parameter
                             2 Bytes : Offset in FUNC der Keywords
                 dann
                 eine Folge von Byte-Instruktionen.

|#
; externe Repräsentation einer Closure:
; #Y(name
;    #LängeY(Byte in Hex ... Byte in Hex)
;    weitere Konstanten
;   )

#-CLISP
(progn
  (defstruct (closure (:print-function print-closure))
    name    ; der Name der Closure
    codevec ; Liste der Bytes des Codevektor
    consts  ; Liste der Konstanten
  )
  (defun print-closure (closure stream depth)
    (declare (ignore depth))
    (write-string "#Y(" stream)
    (write (closure-name closure) :stream stream)
    (write-char #\space stream)
    (write-char #\# stream)
    (write (length (closure-codevec closure)) :stream stream :base 10. :radix nil)
    (write-char #\Y stream)
    ;(write (closure-codevec closure) :stream stream :base 16.) ; stattdessen:
    (write-char #\( stream)
    (do ((i 0 (1- i))
         (L (closure-codevec closure) (cdr L)))
        ((endp L))
      (when (zerop i) (write-char #\newline stream) (setq i 25))
      (write-char #\space stream)
      (write (car L) :stream stream :base 16. :radix nil)
    )
    (write-char #\) stream)
    (write-char #\newline stream)
    (dolist (x (closure-consts closure))
      (write-char #\space stream)
      (write x :stream stream)
    )
    (write-char #\) stream)
  )
)

#+CLISP
(progn
  (defsetf sys::%record-ref sys::%record-store)
  (defsetf closure-name (closure) (new-name)
    `(sys::%record-store ,closure 0 ,new-name)
  )
  (defun make-closure (&key name codevec consts)
    (sys::%make-closure name (sys::make-code-vector codevec) consts)
  )
)

#-CLISP
(set-dispatch-macro-character #\# #\Y
  #'(lambda (stream subchar arg)
      (declare (ignore subchar))
      (if arg
        ; Codevector lesen
        (let ((obj (let ((*read-base* 16.)) (read stream t nil t))))
          (unless (= (length obj) arg)
            (error #+DEUTSCH "Falsche Länge eines Closure-Vektors: ~S"
                   #+ENGLISH "Bad length of closure vector: ~S"
                   arg
          ) )
          obj
        )
        ; Closure lesen
        (let ((obj (read stream t nil t)))
          (make-closure :name (first obj) :codevec (second obj) :consts (cddr obj))
    ) ) )
)

#|
Instruktionen:
Instruktionen können Operanden haben.
Operanden, die Sprungziele (labels) darstellen, sind (um Codelänge zu sparen)
relativ angegeben:
    PC := PC(in der Instruktion) + Operand(signed)
Operanden, die Zahlen darstellen, sind Integers >=0.
Format der Operanden:
bei LOAD, ... mit kleinem Operanden: implizit im Code.
bei allen anderen Instruktionen:
  nächstes Byte:
    Bit 7 = 0 --> Bits 6..0 sind der Operand (7 Bits).
    Bit 7 = 1 --> Bits 6..0 und nächstes Byte bilden den Operanden (15 Bits).
                  Bei Sprungdistanzen: Sollte dieser =0 sein, so bilden
                  die nächsten 4 Bytes den Operanden (32 Bits).


(1) Instruktionen für Konstanten:

Mnemonic                      Bedeutung

(NIL)                         A0 := NIL, 1 Wert

(PUSH-NIL n)                  n-mal: -(STACK) := NIL, undefinierte Werte

(T)                           A0 := T, 1 Wert

(CONST n)                     A0 := Konstante Nr. n aus FUNC, 1 Wert


(2) Instruktionen für statische Variablen

Mnemonic                      Bedeutung

(LOAD n)                      A0 := (STACK+4*n), 1 Wert

(LOADI k n)                   A0 := ((SP+4*k)+4*n), 1 Wert

(LOADC n m)                   A0 := (svref (STACK+4*n) 1+m), 1 Wert

(LOADV k m)                   A0 := (svref ... 1+m)
                                    (svref ... 0) ; k mal wiederholt
                                    VenvConst,
                              1 Wert

(LOADIC k n m)                A0 := (svref ((SP+4*k)+4*n) 1+m), 1 Wert

(STORE n)                     (STACK+4*n) := A0, 1 Wert

(STOREI k n)                  ((SP+4*k)+4*n) := A0, 1 Wert

(STOREC n m)                  (svref (STACK+4*n) 1+m) := A0, 1 Wert

(STOREV k m)                  (svref ... 1+m)
                              (svref ... 0) ; k mal wiederholt
                              VenvConst
                              := A0, 1 Wert

(STOREIC k n m)               (svref ((SP+4*k)+4*n) 1+m) := A0, 1 Wert


(3) Instruktionen für dynamische Variablen

Mnemonic                      Bedeutung

(GETVALUE n)                  A0 := (symbol-value (CONST n)), 1 Wert

(SETVALUE n)                  (setf (symbol-value (CONST n)) A0), 1 Wert

(BIND n)                      bindet (CONST n), ein Symbol, dynamisch an A0.
                              Undefinierte Werte.

(UNBIND1)                     löst einen Bindungsframe auf
(UNBIND n)                    löst n Bindungsframes auf

(PROGV)                       bindet dynamisch die Symbole in der Liste
                              (STACK)+ an die Werte in der Liste A0 und baut
                              dabei genau einen Bindungsframe auf,
                              undefinierte Werte


(4) Instruktionen für Stackoperationen

Mnemonic                      Bedeutung

(PUSH)                        -(STACK) := A0, undefinierte Werte

(POP)                         A0 := (STACK)+, 1 Wert

(SKIP n)                      STACK := STACK+4*n

(SKIPI k n)                   STACK := (SP+4*k)+4*n, SP:=SP+4*(k+1)

(SKIPSP k)                    SP := SP+4*k


(5) Instruktionen für Programmfluß und Sprünge

Mnemonic                      Bedeutung

(SKIP&RET n)                  STACK := STACK+4*n, beendet die Funktion
                              mit den Werten A0/...

(JMP label)                   Sprung zu label

(JMPIF label)                 falls A0 /= NIL : Sprung zu label.

(JMPIFNOT label)              falls A0 = NIL : Sprung zu label.

(JMPIF1 label)                falls A0 /= NIL : 1 Wert, Sprung zu label.

(JMPIFNOT1 label)             falls A0 = NIL : 1 Wert, Sprung zu label.

(JMPIFATOM label)             falls A0 kein Cons : Sprung zu label.
                              Undefinierte Werte.

(JMPIFCONSP label)            falls A0 ein Cons : Sprung zu label.
                              Undefinierte Werte.

(JMPIFEQ label)               falls A0 EQ zu (STACK)+ : Sprung zu label.
                              Undefinierte Werte.

(JMPIFNOTEQ label)            falls A0 nicht EQ zu (STACK)+ : Sprung zu label.
                              Undefinierte Werte.

(JMPIFEQTO n label)           falls (STACK)+ EQ zu (CONST n) : Sprung zu label.
                              Undefinierte Werte.

(JMPIFNOTEQTO n label)        falls (STACK)+ nicht EQ zu (CONST n) : Sprung zu label.
                              Undefinierte Werte.

(JMPHASH n label)             Sucht A0 in der EQ- oder EQL-Hash-Tabelle
                              (CONST n). Gefunden: Sprung ans von GETHASH
                              gelieferte Label. Nicht gefunden: Sprung zu
                              label. Undefinierte Werte.

(JSR label)                   Unterprogrammaufruf: lege Closure auf den STACK und
                              springe zu label (mit undefinierten Werten),
                              (RET) setzt das Programm an der Stelle nach
                              dem (JSR label) fort.

(JMPTAIL m n label)           Wiederverwendung eines Stack-Frames: n>=m.
                              Der Stack-Frame der Größe n wird auf Größe m
                              verkleinert, indem die unteren m Einträge um
                              n-m nach oben kopiert werden:
                              (STACK+4*(n-m)...STACK+4*(n-1)) := (STACK+4*0...STACK+4*(m-1)),
                              STACK := STACK + 4*(n-m),
                              dann -(STACK) := Closure,
                              Sprung zu label mit undefinierten Werten.


(6) Instruktionen für Environments und Closures

Mnemonic                      Bedeutung

(VENV)                        A0 := VenvConst aus FUNC, 1 Wert

(MAKE-VECTOR1&PUSH n)         kreiert einen simple-vector mit n+1 (n>=0) Kom-
                              ponenten und steckt A0 als Komponente 0 hinein.
                              -(STACK) := der neue Vektor. Undefinierte Werte.

(COPY-CLOSURE m n)            kopiert die Closure (CONST m) aus FUNC und
                              ersetzt in der Kopie für i=0,...,n-1 (n>0) die
                              Komponente (CONST i) durch (STACK+4*(n-1-i)).
                              STACK := STACK+4*n.
                              A0 := Closure-Kopie, 1 Wert


(7) Instruktionen für Funktionsaufrufe

Mnemonic                      Bedeutung

(CALL k n)                    ruft die Funktion (CONST n) mit k Argumenten
                              (STACK+4*(k-1)),...,(STACK+4*0) auf,
                              STACK:=STACK+4*k,
                              Ergebnis kommt nach A0/...

(CALL0 n)                     ruft die Funktion (CONST n) mit 0 Argumenten
                              auf, Ergebnis kommt nach A0/...

(CALL1 n)                     ruft die Funktion (CONST n) mit einem Argument
                              (STACK)+ auf, Ergebnis kommt nach A0/...

(CALL2 n)                     ruft die Funktion (CONST n) mit zwei Argumenten
                              4(STACK),(STACK) auf, STACK:=STACK+8,
                              Ergebnis kommt nach A0/...

(CALLS1 n)                    ruft die Funktion (FUNTAB n)
(CALLS2 n)                    bzw. (FUNTAB 256+n)
                              (ein SUBR ohne Rest-Parameter) auf,
                              mit der korrekten Argumentezahl auf dem STACK.
                              STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLSR m n)                  ruft die Funktion (FUNTABR n)
                              (ein SUBR mit Rest-Parameter) auf,
                              mit der korrekten Argumentezahl und zusätzlich
                              m restlichen Argumenten auf dem STACK.
                              STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLC)                       ruft die Funktion A0 (eine compilierte Closure
                              ohne Keyword-Parameter) auf. Argumente
                              sind schon im richtigen Format auf dem STACK,
                              STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLCKEY)                    ruft die Funktion A0 (eine compilierte Closure
                              mit Keyword-Parameter) auf. Argumente
                              sind schon im richtigen Format auf dem STACK,
                              STACK wird bereinigt, Ergebnis kommt nach A0/...

(FUNCALL n)                   ruft die Funktion (STACK+4*n) mit n (n>=0)
                              Argumenten (alle auf dem Stack) auf,
                              STACK:=STACK+4*(n+1)
                              Ergebnis kommt nach A0/...

(APPLY n)                     ruft die Funktion (STACK+4*n) mit n (n>=0)
                              Argumenten (alle auf dem Stack) und weiteren
                              Argumenten (Liste in A0) auf,
                              STACK:=STACK+4*(n+1),
                              Ergebnis kommt nach A0/...


(8) Instruktionen für optionale und Keyword-Argumente

Mnemonic                      Bedeutung

(PUSH-UNBOUND n)              n-mal: -(STACK) := #<UNBOUND>, undefinierte Werte

(JMPIFBOUNDP n label)         falls (STACK+4*n) /= #<UNBOUND> :
                                Sprung zu label, A0 := (STACK+4*n), 1 Wert.
                              Sonst undefinierte Werte.

(BOUNDP n)                    A0 := (NIL falls (STACK+4*n)=#<UNBOUND>, T sonst), 1 Wert

(UNBOUND->NIL n)              Falls (STACK+4*n) = #<UNBOUND>: (STACK+4*n) := NIL


(9) Instruktionen zur Behandlung mehrerer Werte

Mnemonic                      Bedeutung

(VALUES0)                     A0 := NIL, 0 Werte

(VALUES1)                     A0 := A0, 1 Wert

(STACK-TO-MV n)               holt n Werte von (STACK)+ herab,
                              STACK:=STACK+4*n

(MV-TO-STACK)                 Multiple Values A0/A1/... auf -(STACK), 1. Wert
                              zuoberst, STACK:=STACK-4*D7.W,
                              danach undefinierte Werte

(NV-TO-STACK n)               die ersten n Werte (n>=0) auf -(STACK), 1. Wert
                              zuoberst, STACK:=STACK-4*n, undefinierte Werte

(MV-TO-LIST)                  Multiple Values A0/... als Liste nach A0, 1 Wert

(LIST-TO-MV)                  A0/... := (values-list A0)

(MVCALLP)                     rette STACK auf -(SP), rette A0 auf -(STACK)

(MVCALL)                      führe einen Funktionsaufruf aus, wobei zwischen
                              STACK und STACK:=(SP)+ die Funktion (ganz oben)
                              und die Argumente stehen,
                              Ergebnis kommt nach A0/...


(10) Instruktionen für BLOCK

Mnemonic                      Bedeutung

(BLOCK-OPEN n label)          Legt einen Block-Cons (mit CAR=(CONST n) und
                              CDR=Framepointer) auf -(STACK) ab, baut einen
                              Block-Frame auf. Bei einem RETURN auf diesen
                              Frame wird zu label gesprungen.

(BLOCK-CLOSE)                 Verlasse den Block und baue dabei einen Block-
                              Frame ab (inklusive der Block-Cons-Variablen)

(RETURN-FROM n)               Verlasse den Block, dessen Block-Cons
                              (CONST n) ist, mit den Werten A0/...


(11) Instruktionen für TAGBODY

Mnemonic                      Bedeutung

(TAGBODY-OPEN m label1 ... labelm)
                              Legt einen Tagbody-Cons (mit CAR = m als Fixnum
                              und CDR=Framepointer) auf -(STACK) ab, baut
                              einen Tagbody-Frame auf. Bei einem GO mit
                              Nummer k wird zu labelk gesprungen.
                              Undefinierte Werte.

(TAGBODY-CLOSE-NIL)           Verlasse den Tagbody und baue dabei einen
                              Tagbody-Frame ab (inklusive der Tagbody-Cons-
                              Variablen).
                              A0 := NIL, 1 Wert

(TAGBODY-CLOSE)               Verlasse den Tagbody und baue dabei
                              einen Tagbody-Frame ab (inklusive der
                              Tagbody-Cons-Variablen).

(GO n k)                      Springe im Tagbody, dessen Tagbody-Cons
                              (CONST n) ist, an Tag Nummer k


(12) Instruktionen für CATCH und THROW

Mnemonic                      Bedeutung

(CATCH-OPEN label)            baut einen CATCH-Frame auf mit A0 als Tag;
                              bei einem THROW auf dieses Tag wird zu label
                              gesprungen

(CATCH-CLOSE)                 löst einen CATCH-Frame auf

(THROW)                       führt ein THROW auf den Catch-Tag (STACK)+ aus,
                              mit den Werten A0/...


(13) Instruktionen für UNWIND-PROTECT

Mnemonic                      Bedeutung

(UNWIND-PROTECT-OPEN label)   baut einen UNWIND-PROTECT-Frame auf;
                              bei einem Unwind wird unter Rettung
                              der Werte zu label gesprungen

(UNWIND-PROTECT-NORMAL-EXIT)  löst einen Unwind-Protect-Frame auf, schreibt
                              eine Weitermach-Adresse auf SP, rettet die
                              Werte und fängt an, den folgenden Cleanup-Code
                              auszuführen

(UNWIND-PROTECT-CLOSE)        beendet den Cleanup-Code: schreibt die
                              geretteten Werte zurück, führt ein RTS aus

(UNWIND-PROTECT-CLEANUP)      löst einen Unwind-Protect-Frame auf,
                              schreibt eine Weitermach-Adresse und
                              den PC auf SP, rettet die Werte und
                              fängt an, den Cleanup-Code auszuführen


(14) Kurz-Instruktionen für einige Funktionen

Mnemonic                      Bedeutung

(NOT)                         A0 := (not A0), 1 Wert

(EQ)                          A0 := (eq (STACK)+ A0), 1 Wert

(CAR)                         A0 := (car A0), 1 Wert

(CDR)                         A0 := (cdr A0), 1 Wert

(CONS)                        A0 := (cons (STACK)+ A0), 1 Wert

(SYMBOL-FUNCTION)             A0 := (symbol-function A0), 1 Wert

(SVREF)                       A0 := (svref (STACK)+ A0), 1 Wert

(SVSET)                       (setf (svref (STACK) A0) (STACK+4)),
                              A0 := (STACK+4), 1 Wert, STACK:=STACK+8

(LIST n)                      Bildet eine Liste aus den untersten n auf dem STACK
                              liegenden Objekten, STACK := STACK + 4*n,
                              Liste nach A0, 1 Wert

(ERROR n)                     ruft ERROR mit n+1 Argumenten (auf dem STACK) auf


(15)
Zusätzlich gibt es kombinierte Operationen im Format
(<OP1>&<OP2>&...&<OPn> <Operanden_1> <Operanden_2> ... <Operanden_n>) .

Mnemonic                           Bedeutung

(NIL&PUSH)                         (NIL) (PUSH)
(T&PUSH)                           (T) (PUSH)
(CONST&PUSH n)                     (CONST n) (PUSH)
(LOAD&PUSH n)                      (LOAD n) (PUSH)
(LOADI&PUSH k n)                   (LOADI k n) (PUSH)
(LOADC&PUSH n m)                   (LOADC n m) (PUSH)
(LOADV&PUSH k m)                   (LOADV k m) (PUSH)
(POP&STORE n)                      (POP) (STORE n)
(GETVALUE&PUSH n)                  (GETVALUE n) (PUSH)
(JSR&PUSH label)                   (JSR label) (PUSH)
(COPY-CLOSURE&PUSH m n)            (COPY-CLOSURE m n) (PUSH)
(CALL&PUSH k n)                    (CALL k n) (PUSH)
(CALL1&PUSH n)                     (CALL1 n) (PUSH)
(CALL2&PUSH n)                     (CALL2 n) (PUSH)
(CALLS1&PUSH n)                    (CALLS1 n) (PUSH)
(CALLS2&PUSH n)                    (CALLS2 n) (PUSH)
(CALLSR&PUSH m n)                  (CALLSR m n) (PUSH)
(CALLC&PUSH)                       (CALLC) (PUSH)
(CALLCKEY&PUSH)                    (CALLCKEY) (PUSH)
(FUNCALL&PUSH n)                   (FUNCALL n) (PUSH)
(APPLY&PUSH n)                     (APPLY n) (PUSH)
(CAR&PUSH)                         (CAR) (PUSH)
(CDR&PUSH)                         (CDR) (PUSH)
(CONS&PUSH)                        (CONS) (PUSH)
(LIST&PUSH n)                      (LIST n) (PUSH)
(NIL&STORE n)                      (NIL) (STORE n)
(T&STORE n)                        (T) (STORE n)
(LOAD&STOREC k n m)                (LOAD k) (STOREC n m)
(CALLS1&STORE n k)                 (CALLS1 n) (STORE k)
(CALLS2&STORE n k)                 (CALLS2 n) (STORE k)
(CALLSR&STORE m n k)               (CALLSR m n) (STORE k)
(LOAD&CDR&STORE n)                 (LOAD n) (CDR) (STORE n)
(LOAD&CONS&STORE n)                (LOAD n+1) (CONS) (STORE n)
(LOAD&INC&STORE n)                 (LOAD n) (CALL1 #'1+) (STORE n)
(LOAD&DEC&STORE n)                 (LOAD n) (CALL1 #'1-) (STORE n)
(LOAD&CAR&STORE m n)               (LOAD m) (CAR) (STORE n)
(CALL1&JMPIF n label)              (CALL1 n) (JMPIF label)
(CALL1&JMPIFNOT n label)           (CALL1 n) (JMPIFNOT label)
(CALL2&JMPIF n label)              (CALL2 n) (JMPIF label)
(CALL2&JMPIFNOT n label)           (CALL2 n) (JMPIFNOT label)
(CALLS1&JMPIF n label)             (CALLS1 n) (JMPIF label)
(CALLS1&JMPIFNOT n label)          (CALLS1 n) (JMPIFNOT label)
(CALLS2&JMPIF n label)             (CALLS2 n) (JMPIF label)
(CALLS2&JMPIFNOT n label)          (CALLS2 n) (JMPIFNOT label)
(CALLSR&JMPIF m n label)           (CALLSR m n) (JMPIF label)
(CALLSR&JMPIFNOT m n label)        (CALLSR m n) (JMPIFNOT label)
(LOAD&JMPIF n label)               (LOAD n) (JMPIF label)
(LOAD&JMPIFNOT n label)            (LOAD n) (JMPIFNOT label)
(LOAD&CAR&PUSH n)                  (LOAD n) (CAR) (PUSH)
(LOAD&CDR&PUSH n)                  (LOAD n) (CDR) (PUSH)
(LOAD&INC&PUSH n)                  (LOAD n) (CALL1 #'1+) (PUSH)
(LOAD&DEC&PUSH n)                  (LOAD n) (CALL1 #'1-) (PUSH)
(CONST&SYMBOL-FUNCTION n)          (CONST n) (SYMBOL-FUNCTION)
(CONST&SYMBOL-FUNCTION&PUSH n)     (CONST n) (SYMBOL-FUNCTION) (PUSH)
(CONST&SYMBOL-FUNCTION&STORE n k)  (CONST n) (SYMBOL-FUNCTION) (STORE k)


|#

; Instruktionen-Klassifikation:
; O = Instruktion ohne Operand
; K = numerischer Operand oder
;     Kurz-Operand
;       (dann ist das Byte=(short-code-opsize)*x+(short-code-offset)+Operand)
; N = numerischer Operand
; B = Byte-Operand
; L = Label-Operand
; NH = numerischer Operand, der eine Hashtable referenziert
; LX = so viele Label-Operanden, wie der vorangehende Operand angibt

; Die Position in der Instruction-Table liefert den eigentlichen Code der
; Instruktion (>= 0, < short-code-offset), Codes >= short-code-offset werden
; von den K-Instruktionen belegt.
(defconstant instruction-table
  '#(; (1) Konstanten
     (NIL O) (PUSH-NIL N) (T O) (CONST K)
     ; (2) statische Variablen
     (LOAD K) (LOADI NN) (LOADC NN) (LOADV NN) (LOADIC NNN)
     (STORE K) (STOREI NN) (STOREC NN) (STOREV NN) (STOREIC NNN)
     ; (3) dynamische Variablen
     (GETVALUE N) (SETVALUE N) (BIND N) (UNBIND1 O) (UNBIND N) (PROGV O)
     ; (4) Stackoperationen
     (PUSH O) (POP O) (SKIP N) (SKIPI NN) (SKIPSP N)
     ; (5) Programmfluß und Sprünge
     (SKIP&RET N) (JMP L) (JMPIF L) (JMPIFNOT L) (JMPIF1 L) (JMPIFNOT1 L)
     (JMPIFATOM L) (JMPIFCONSP L) (JMPIFEQ L) (JMPIFNOTEQ L)
     (JMPIFEQTO NL) (JMPIFNOTEQTO NL) (JMPHASH NHL) (JSR L) (JMPTAIL NNL)
     ; (6) Environments und Closures
     (VENV O) (MAKE-VECTOR1&PUSH N) (COPY-CLOSURE NN)
     ; (7) Funktionsaufrufe
     (CALL NN) (CALL0 N) (CALL1 N) (CALL2 N)
     (CALLS1 B) (CALLS2 B) (CALLSR NB) (CALLC O) (CALLCKEY O)
     (FUNCALL N) (APPLY N)
     ; (8) optionale und Keyword-Argumente
     (PUSH-UNBOUND N) (JMPIFBOUNDP NL) (BOUNDP N) (UNBOUND->NIL N)
     ; (9) Behandlung mehrerer Werte
     (VALUES0 O) (VALUES1 O) (STACK-TO-MV N) (MV-TO-STACK O) (NV-TO-STACK N)
     (MV-TO-LIST O) (LIST-TO-MV O) (MVCALLP O) (MVCALL O)
     ; (10) BLOCK
     (BLOCK-OPEN NL) (BLOCK-CLOSE O) (RETURN-FROM N)
     ; (11) TAGBODY
     (TAGBODY-OPEN NLX) (TAGBODY-CLOSE-NIL O) (TAGBODY-CLOSE O) (GO NN)
     ; (12) CATCH und THROW
     (CATCH-OPEN L) (CATCH-CLOSE O) (THROW O)
     ; (13) UNWIND-PROTECT
     (UNWIND-PROTECT-OPEN L) (UNWIND-PROTECT-NORMAL-EXIT O)
     (UNWIND-PROTECT-CLOSE O) (UNWIND-PROTECT-CLEANUP O)
     ; (14) einige Funktionen
     (NOT O) (EQ O) (CAR O) (CDR O) (CONS O) (SYMBOL-FUNCTION O) (SVREF O)
     (SVSET O) (LIST N) (ERROR N)
     ; (15) kombinierte Operationen
     (NIL&PUSH O) (T&PUSH O) (CONST&PUSH K)
     (LOAD&PUSH K) (LOADI&PUSH NN) (LOADC&PUSH NN) (LOADV&PUSH NN) (POP&STORE N)
     (GETVALUE&PUSH N)
     (JSR&PUSH L)
     (COPY-CLOSURE&PUSH NN)
     (CALL&PUSH NN) (CALL1&PUSH N) (CALL2&PUSH N)
     (CALLS1&PUSH B) (CALLS2&PUSH B) (CALLSR&PUSH NB)
     (CALLC&PUSH O) (CALLCKEY&PUSH O)
     (FUNCALL&PUSH N) (APPLY&PUSH N)
     (CAR&PUSH O) (CDR&PUSH O) (CONS&PUSH O)
     (LIST&PUSH N)
     (NIL&STORE N) (T&STORE N) (LOAD&STOREC NNN)
     (CALLS1&STORE BN) (CALLS2&STORE BN) (CALLSR&STORE NBN)
     (LOAD&CDR&STORE N) (LOAD&CONS&STORE N) (LOAD&INC&STORE N) (LOAD&DEC&STORE N)
     (LOAD&CAR&STORE NN)
     (CALL1&JMPIF NL) (CALL1&JMPIFNOT NL)
     (CALL2&JMPIF NL) (CALL2&JMPIFNOT NL)
     (CALLS1&JMPIF BL) (CALLS1&JMPIFNOT BL)
     (CALLS2&JMPIF BL) (CALLS2&JMPIFNOT BL)
     (CALLSR&JMPIF NBL) (CALLSR&JMPIFNOT NBL)
     (LOAD&JMPIF NL) (LOAD&JMPIFNOT NL)
     (LOAD&CAR&PUSH N) (LOAD&CDR&PUSH N) (LOAD&INC&PUSH N) (LOAD&DEC&PUSH N)
     (CONST&SYMBOL-FUNCTION N) (CONST&SYMBOL-FUNCTION&PUSH N)
     (CONST&SYMBOL-FUNCTION&STORE NN)
)   )
(dotimes (i (length instruction-table))
  (setf (get (first (svref instruction-table i)) 'INSTRUCTION) i)
)
(defconstant instruction-codes
  (let ((hashtable (make-hash-table :test #'eq)))
    (dotimes (i (length instruction-table))
      (setf (gethash (first (svref instruction-table i)) hashtable) i)
    )
    hashtable
) )

; K-Instruktionen:
(defconstant instruction-table-K
 '#(LOAD LOAD&PUSH CONST CONST&PUSH STORE)
)
(defconstant short-code-offset 146)
(defconstant short-code-opsize 22)


#|

Zwischensprache nach dem 1. Pass:
=================================

1. Konstanten:

(NIL)                            A0 := NIL, 1 Wert

(PUSH-NIL n)                     n-mal: -(STACK) := NIL, undefinierte Werte

(T)                              A0 := T, 1 Wert

(CONST const)                    A0 := 'const, 1 Wert

(FCONST fnode)                   A0 := das Compilat des fnode, 1 Wert

(BCONST block)                   A0 := das Block-Cons dieses Blockes (eine
                                 Konstante aus FUNC), 1 Wert

(GCONST tagbody)                 A0 := das Tagbody-Cons dieses Tagbody (eine
                                 Konstante aus FUNC), 1 Wert

2.,3. Variablen:

(GET var venvc stackz)           A0 := var, 1 Wert
                                 (venvc ist das aktuelle Closure-Venv,
                                  stackz der aktuelle Stackzustand)

(SET var venvc stackz)           var := A0, 1 Wert
                                 (venvc ist das aktuelle Closure-Venv,
                                  stackz der aktuelle Stackzustand)

(STORE n)                        (STACK+4*n) := A0, 1 Wert

(GETVALUE symbol)                A0 := (symbol-value 'symbol), 1 Wert

(SETVALUE symbol)                (setf (symbol-value 'symbol) A0), 1 Wert

(BIND const)                     bindet const (ein Symbol) dynamisch an A0.
                                 Undefinierte Werte.

(UNBIND1)                        löst einen Bindungsframe auf

(PROGV)                          bindet dynamisch die Symbole in der Liste
                                 (STACK)+ an die Werte in der Liste A0 und
                                 baut dabei genau einen Bindungsframe auf,
                                 undefinierte Werte

4. Stackoperationen:

(PUSH)                           -(STACK) := A0, undefinierte Werte

(POP)                            A0 := (STACK)+, 1 Wert

(UNWIND stackz1 stackz2 for-value) Führt ein Unwind binnen einer Funktion aus:
                                 Bereinigt den Stack, um vom Stackzustand
                                 stackz1 zum Stackzustand stackz2 zu kommen.
                                 Löst dazwischen liegende Frames auf. for-value
                                 gibt an, ob dabei die Werte A0/... gerettet
                                 werden müssen.

5. Programmfluß und Sprünge:

(RET)                            beendet die Funktion mit den Werten A0/...

(JMP label)                      Sprung zu label

(JMPIF label)                    falls A0 /= NIL : Sprung zu label.

(JMPIFNOT label)                 falls A0 = NIL : Sprung zu label.

(JMPIF1 label)                   falls A0 /= NIL : 1 Wert, Sprung zu label.

(JMPIFNOT1 label)                falls A0 = NIL : 1 Wert, Sprung zu label.

(JMPHASH test ((obj1 . label1) ... (objm . labelm)) label . labels)
                                 Sprung zu labeli, falls A0 = obji (im Sinne
                                 des angegebenen Vergleichs), sonst zu label.
                                 Undefinierte Werte.

(JSR m label)                    ruft den Code ab label als Unterprogramm auf,
                                 mit m Argumenten auf dem Stack

6. Environments und Closures:

(VENV venvc stackz)              A0 := das Venv, das venvc entspricht
                                 (aus dem Stack, als Konstante aus
                                 FUNC, oder NIL, falls in FUNC nicht vorhanden),
                                 1 Wert
                                 (stackz ist der aktuelle Stackzustand)

(MAKE-VECTOR1&PUSH n)            kreiert einen simple-vector mit n+1 (n>=0)
                                 Komponenten und steckt A0 als Komponente 0
                                 hinein. -(STACK) := der neue Vektor.
                                 Undefinierte Werte.

(COPY-CLOSURE fnode n)           kopiert die Closure, die dem fnode entspricht
                                 und ersetzt in der Kopie für i=0,...,n-1 (n>0)
                                 die Komponente (CONST i) durch (STACK+4*(n-1-i)).
                                 STACK := STACK+4*n. A0 := Closure-Kopie, 1 Wert

7. Funktionsaufrufe:

(CALLP)                          beginnt den Aufbau eines Funktionsaufruf-Frames
                                 (wird im 2. Pass ersatzlos gestrichen)

(CALL k const)                   ruft die Funktion const mit k Argumenten
                                 (STACK+4*(k-1)),...,(STACK+4*0) auf,
                                 STACK:=STACK+4*k, Ergebnis kommt nach A0/...

(CALL0 const)                    ruft die Funktion const mit 0 Argumenten auf,
                                 Ergebnis kommt nach A0/...

(CALL1 const)                    ruft die Funktion const mit 1 Argument A0 auf,
                                 Ergebnis kommt nach A0/...

(CALL2 const)                    ruft die Funktion const mit 2 Argumenten (STACK)
                                 und A0 auf, STACK:=STACK+4,
                                 Ergebnis kommt nach A0/...

(CALLS1 n)                       ruft die Funktion (FUNTAB n)
(CALLS2 n)                       bzw. (FUNTAB 256+n)
                                 (ein SUBR ohne Rest-Parameter) auf,
                                 mit der korrekten Argumentezahl auf dem STACK.
                                 STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLSR m n)                     ruft die Funktion (FUNTABR n)
                                 (ein SUBR mit Rest-Parameter) auf,
                                 mit der korrekten Argumentezahl und zusätzlich
                                 m restlichen Argumenten auf dem STACK.
                                 STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLC)                          ruft die Funktion A0 (eine compilierte Closure
                                 ohne Keyword-Parameter) auf. Argumente
                                 sind schon im richtigen Format auf dem STACK,
                                 STACK wird bereinigt, Ergebnis kommt nach A0/...

(CALLCKEY)                       ruft die Funktion A0 (eine compilierte Closure
                                 mit Keyword-Parameter) auf. Argumente
                                 sind schon im richtigen Format auf dem STACK,
                                 STACK wird bereinigt, Ergebnis kommt nach A0/...

(FUNCALLP)                       fängt den Aufbau eines FUNCALL-Frames an,
                                 auszuführende Funktion ist in A0

(FUNCALL n)                      ruft die angegebene Funktion mit n (n>=0)
                                 Argumenten (alle auf dem Stack) auf,
                                 beseitigt den FUNCALL-Frame,
                                 Ergebnis kommt nach A0/...

(APPLYP)                         fängt den Aufbau eines APPLY-Frames an,
                                 auszuführende Funktion ist in A0

(APPLY n)                        ruft die angegebene Funktion mit n (n>=0)
                                 Argumenten (alle auf dem Stack) und weiteren
                                 Argumenten (Liste in A0) auf,
                                 beseitigt den APPLY-Frame,
                                 Ergebnis kommt nach A0/...

8. optionale und Keyword-Argumente:

(PUSH-UNBOUND n)                 n-mal: -(STACK) := #<UNBOUND>, undefinierte Werte

(JMPIFBOUNDP var venvc stackz label)
                                 falls Variable /= #<UNBOUND> :
                                   Sprung zu label, A0 := Variable, 1 Wert.
                                 Sonst undefinierte Werte.
                                 (stackz ist der aktuelle Stackzustand)

(BOUNDP var venvc stackz)        A0 := (NIL falls Variable=#<UNBOUND>, T sonst),
                                 1 Wert
                                 (stackz ist der aktuelle Stackzustand)

9. Behandlung mehrerer Werte:

(VALUES0)                        A0 := NIL, 0 Werte

(VALUES1)                        A0 := A0, 1 Wert

(STACK-TO-MV n)                  holt n Werte von (STACK)+ herab,
                                 STACK:=STACK+4*n, n>1

(MV-TO-STACK)                    Multiple Values A0/A1/... auf -(STACK),
                                 1. Wert zuoberst, STACK:=STACK-4*D7.W,
                                 danach undefinierte Werte

(NV-TO-STACK n)                  die ersten n Werte (n>=0) auf -(STACK),
                                 1. Wert zuoberst, STACK:=STACK-4*n,
                                 undefinierte Werte

(MV-TO-LIST)                     Multiple Values A0/... als Liste nach A0,
                                 1 Wert

(LIST-TO-MV)                     A0/... := (values-list A0)

(MVCALLP)                        bereitet einen MULTIPLE-VALUE-CALL auf die
                                 Funktion in A0 vor

(MVCALL)                         führt einen MULTIPLE-VALUE-CALL mit den im
                                 Stack liegenden Argumenten aus

10. BLOCK:

(BLOCK-OPEN const label)         Legt einen Block-Cons (mit CAR=const und CDR=
                                 Framepointer) auf -(STACK) ab, baut einen
                                 Block-Frame auf. Bei einem RETURN auf diesen
                                 Frame wird zu label gesprungen.

(BLOCK-CLOSE)                    Verlasse den Block und baue dabei einen Block-
                                 Frame ab (inklusive der Block-Cons-Variablen)

(RETURN-FROM const)              Verlasse den Block, dessen Block-Cons angegeben
                                 ist, mit den Werten A0/...
(RETURN-FROM block)              Verlasse den angegebenen Block (sein Block-Cons
                                 kommt unter den BlockConsts von FUNC vor) mit
                                 den Werten A0/...

11. TAGBODY:

(TAGBODY-OPEN m label1 ... labelm)
                                 Legt einen Tagbody-Cons (mit CAR=m als Fixnum
                                 und CDR=Framepointer) auf -(STACK) ab, baut einen
                                 Tagbody-Frame auf. Bei einem GO mit Nummer k
                                 wird zu labelk gesprungen.

(TAGBODY-CLOSE-NIL)              Verlasse den Tagbody und baue dabei einen
                                 Tagbody-Frame ab (inklusive der Tagbody-Cons-
                                 Variablen). A0 := NIL, 1 Wert

(TAGBODY-CLOSE)                  Verlasse den Tagbody und baue dabei einen
                                 Tagbody-Frame ab (inklusive der Tagbody-Cons-
                                 Variablen).

(GO const k)                     Springe im Tagbody, dessen Tagbody-Cons
                                 angegeben ist, an Tag (svref (car const) k)
(GO tagbody k)                   Springe im angegebenen Tagbody an Tag Nummer k
                                 in (tagbody-used-far tagbody)

12. CATCH und THROW:

(CATCH-OPEN label)               baut einen CATCH-Frame auf mit A0 als Tag;
                                 bei einem THROW auf dieses Tag wird zu label
                                 gesprungen

(CATCH-CLOSE)                    löst einen CATCH-Frame auf

(THROW)                          führt ein THROW auf den Catch-Tag (STACK)+
                                 aus, mit den Werten A0/...

13. UNWIND-PROTECT:

(UNWIND-PROTECT-OPEN label)      baut einen UNWIND-PROTECT-Frame auf; bei einem
                                 Unwind wird unter Rettung der Werte zu label
                                 gesprungen

(UNWIND-PROTECT-NORMAL-EXIT)     löst einen Unwind-Protect-Frame auf, schreibt
                                 eine Weitermach-Adresse auf SP, rettet die
                                 Werte und fängt an, den folgenden Cleanup-Code
                                 auszuführen

(UNWIND-PROTECT-CLOSE label)     beendet den Cleanup-Code: schreibt die
                                 geretteten Werte zurück, führt ein RTS aus.
                                 Der Cleanup-Code fängt bei label an.

(UNWIND-PROTECT-CLEANUP)         löst einen Unwind-Protect-Frame auf, schreibt
                                 eine Weitermach-Adresse und den PC auf SP,
                                 rettet die Werte und fängt an, den Cleanup-
                                 Code auszuführen

14. einige Funktionen:

(NOT)                            = (CALL1 #'NOT)

(EQ)                             = (CALL2 #'EQ)

(CAR)                            = (CALL1 #'CAR)

(CDR)                            = (CALL1 #'CDR)

(CONS)                           = (CALL2 #'CONS)

(ATOM)                           = (CALL1 #'ATOM)

(CONSP)                          = (CALL1 #'CONSP)

(SYMBOL-FUNCTION)                = (CALL1 #'SYMBOL-FUNCTION)

(SVREF)                          = (CALL2 #'SVREF)

(SVSET)                          (setf (svref (STACK) A0) (STACK+4)),
                                 A0 := (STACK+4), 1 Wert, STACK:=STACK+8

(LIST n)                         = (CALL n #'LIST), n>0

(ERROR n)                        = (CALL n+1 #'ERROR)


Dabei bedeuten jeweils:

n, m, k     eine ganze Zahl >=0

stackz      einen Stackzustand (siehe STACK-VERWALTUNG).
            Das Stack-Layout steht nach dem 1. Pass fest.

venvc       das Environment der Closure-Variablen (siehe VARIABLEN-VERWALTUNG).
            Dies steht nach dem 1. Pass auch fest.

var         eine Variable (siehe VARIABLEN-VERWALTUNG). Ob sie
            special/konstant/lexikalisch ist, steht nach dem 1. Pass fest.

const       eine Konstante

symbol      ein Symbol

fun         entweder (CONST const) eine Konstante, die ein Symbol ist,
            oder (FUNTAB index) eine Indizierung in die feste Funktionentabelle.

fnode       ein fnode (siehe FUNKTIONEN-VERWALTUNG)

label       ein Label (uninterniertes Symbol)

block       ein Block-Descriptor (siehe BLOCK-VERWALTUNG)

test        EQ oder EQL oder EQUAL

for-value   NIL oder T

|#

#-CLISP ; Die Funktionentabelle steckt in EVAL.
(eval-when (compile load eval)
  ; die Funktionstabelle mit max. 3*256 Funktionen (spart Konstanten in FUNC) :
  (defconstant funtab
    '#(system::%funtabref system::subr-info
       svref system::svstore array-element-type array-rank array-dimension
       array-dimensions array-total-size adjustable-array-p bit-and bit-ior
       bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2 bit-orc1 bit-orc2
       bit-not array-has-fill-pointer-p fill-pointer system::set-fill-pointer
       vector-push vector-pop vector-push-extend make-array adjust-array
       standard-char-p graphic-char-p string-char-p alpha-char-p upper-case-p
       lower-case-p both-case-p digit-char-p alphanumericp char-code char-bits
       char-font code-char make-char character char-upcase char-downcase
       digit-char char-int int-char char-name char-bit set-char-bit char schar
       system::store-char system::store-schar string= string/= string< string>
       string<= string>= string-equal string-not-equal string-lessp
       string-greaterp string-not-greaterp string-not-lessp
       system::search-string= system::search-string-equal make-string
       system::string-both-trim nstring-upcase string-upcase nstring-downcase
       string-downcase nstring-capitalize string-capitalize string name-char
       substring
       symbol-value symbol-function boundp fboundp special-form-p set makunbound
       fmakunbound values-list system::driver system::unwind-to-driver
       macro-function macroexpand macroexpand-1 proclaim eval evalhook applyhook
       constantp system::parse-body system::keyword-test
       room gc
       make-hash-table gethash system::puthash remhash maphash clrhash
       hash-table-count system::hash-table-iterator system::hash-table-iterate
       sxhash
       copy-readtable set-syntax-from-char set-macro-character
       get-macro-character make-dispatch-macro-character
       set-dispatch-macro-character get-dispatch-macro-character read
       read-preserving-whitespace read-delimited-list read-line read-char
       unread-char peek-char listen read-char-no-hang clear-input
       read-from-string parse-integer write prin1 print pprint princ
       write-to-string prin1-to-string princ-to-string write-char write-string
       write-line terpri fresh-line finish-output force-output clear-output
       system::line-position
       car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar
       cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
       cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr cons tree-equal endp
       list-length nth first second third fourth fifth sixth seventh eighth
       ninth tenth rest nthcdr last make-list copy-list copy-alist copy-tree
       revappend nreconc system::list-nreverse butlast nbutlast ldiff rplaca
       system::%rplaca rplacd system::%rplacd subst subst-if subst-if-not nsubst
       nsubst-if nsubst-if-not sublis nsublis member member-if member-if-not
       tailp adjoin acons pairlis assoc assoc-if assoc-if-not rassoc rassoc-if
       rassoc-if-not
       lisp-implementation-type lisp-implementation-version software-type
       software-version identity get-universal-time get-internal-run-time
       get-internal-real-time system::%sleep system::%%time
       make-symbol find-package package-name package-nicknames rename-package
       package-use-list package-used-by-list package-shadowing-symbols
       list-all-packages intern find-symbol unintern export unexport import
       shadowing-import shadow use-package unuse-package make-package in-package
       find-all-symbols system::map-symbols system::map-external-symbols
       system::map-all-symbols
       parse-namestring pathname pathname-host pathname-device
       pathname-directory pathname-name pathname-type pathname-version
       file-namestring directory-namestring host-namestring merge-pathnames
       enough-namestring make-pathname namestring truename probe-file
       delete-file rename-file open directory cd make-dir delete-dir
       file-write-date file-author savemem
       eq eql equal equalp consp atom symbolp stringp numberp
       compiled-function-p null not system::closurep listp integerp
       system::fixnump rationalp floatp system::short-float-p
       system::single-float-p system::double-float-p system::long-float-p
       complexp streamp random-state-p readtablep hash-table-p pathnamep
       characterp functionp packagep arrayp system::simple-array-p bit-vector-p
       vectorp simple-vector-p simple-string-p simple-bit-vector-p commonp
       type-of coerce
       system::%record-ref system::%record-store system::%record-length
       system::%structure-ref system::%structure-store system::%make-structure
       system::%copy-structure system::%structure-type-p system::closure-name
       system::closure-codevec system::closure-consts system::make-code-vector
       system::%make-closure system::make-load-time-eval
       system::sequencep elt system::%setelt subseq copy-seq length reverse
       nreverse make-sequence reduce fill replace remove remove-if remove-if-not
       delete delete-if delete-if-not remove-duplicates delete-duplicates
       substitute substitute-if substitute-if-not nsubstitute nsubstitute-if
       nsubstitute-if-not find find-if find-if-not position position-if
       position-if-not count count-if count-if-not mismatch search sort
       stable-sort merge
       make-synonym-stream make-two-way-stream make-echo-stream
       make-string-input-stream system::string-input-stream-index
       make-string-output-stream get-output-stream-string
       system::make-string-push-stream input-stream-p output-stream-p
       stream-element-type close read-byte write-byte file-position file-length
       system::%putd system::%proclaim-constant get getf get-properties
       system::%putplist system::%put remprop symbol-package symbol-plist
       symbol-name keywordp gensym system::special-variable-p gensym
       system::decimal-string zerop plusp minusp oddp evenp 1+ 1- conjugate exp
       expt log sqrt isqrt abs phase signum sin cos tan cis asin acos atan sinh
       cosh tanh asinh acosh atanh float rational rationalize numerator
       denominator floor ceiling truncate round mod rem ffloor fceiling
       ftruncate fround decode-float scale-float float-radix float-sign
       float-digits float-precision integer-decode-float complex realpart
       imagpart lognand lognor logandc1 logandc2 logorc1 logorc2 boole lognot
       logtest logbitp ash logcount integer-length byte byte-size byte-position
       ldb ldb-test mask-field dpb deposit-field random make-random-state !
       exquo long-float-digits system::%set-long-float-digits system::log2
       system::log10
       vector aref system::store array-in-bounds-p array-row-major-index bit
       sbit char= char/= char< char> char<= char>= char-equal char-not-equal
       char-lessp char-greaterp char-not-greaterp char-not-lessp string-concat
       apply system::%funcall funcall mapcar maplist mapc mapl mapcan mapcon
       values list list* append nconc error concatenate map some every notany
       notevery make-broadcast-stream make-concatenated-stream = /= < > <= >=
       max min + - * / gcd lcm logior logxor logand logeqv
  )   )
  (defun %funtabref (index)
    (if (and (<= 0 index) (< index (length funtab))) (svref funtab index) nil)
  )
)
#+CROSS
(eval-when (compile load eval)
  (defun subr-info (sym)
    (values-list
      (assoc sym
        '(; Das ist die Tabelle aller SUBRs, wie in SUBR.D.
          ; SUBRs, die in verschiedenen Implementationen verschiedene
          ; Signaturen haben und/oder deren Spezifikation sich noch ändern
          ; könnte, sind dabei allerdings auskommentiert.
          (! 1 0 nil nil nil)
          (system::%%time 0 0 nil nil nil)
          (system::%copy-structure 1 0 nil nil nil)
          (system::%defseq 1 0 nil nil nil)
          (system::%exit 0 0 nil nil nil)
          (system::%funcall 1 0 t nil nil)
          (system::%funtabref 1 0 nil nil nil)
          (system::%make-closure 3 0 nil nil nil)
          (system::%make-structure 2 0 nil nil nil)
          (system::%proclaim-constant 2 0 nil nil nil)
          (system::%put 3 0 nil nil nil)
          (system::%putd 2 0 nil nil nil)
          (system::%putplist 2 0 nil nil nil)
          (system::%record-length 1 0 nil nil nil)
          (system::%record-ref 2 0 nil nil nil)
          (system::%record-store 3 0 nil nil nil)
          (system::%rplaca 2 0 nil nil nil)
          (system::%rplacd 2 0 nil nil nil)
          (system::%set-long-float-digits 1 0 nil nil nil)
          (system::%setelt 3 0 nil nil nil)
          ;(system::%sleep 1 0 nil nil nil)
          ;(system::%sleep 2 0 nil nil nil)
          (system::%structure-ref 3 0 nil nil nil)
          (system::%structure-store 4 0 nil nil nil)
          (system::%structure-type-p 2 0 nil nil nil)
          (system::%svstore 3 0 nil nil nil)
          (* 0 0 t nil nil)
          (+ 0 0 t nil nil)
          (- 1 0 t nil nil)
          (/ 1 0 t nil nil)
          (/= 1 0 t nil nil)
          (1+ 1 0 nil nil nil)
          (1- 1 0 nil nil nil)
          (< 1 0 t nil nil)
          (<= 1 0 t nil nil)
          (= 1 0 t nil nil)
          (> 1 0 t nil nil)
          (>= 1 0 t nil nil)
          (abs 1 0 nil nil nil)
          (acons 3 0 nil nil nil)
          (acos 1 0 nil nil nil)
          (acosh 1 0 nil nil nil)
          (adjoin 2 0 nil (:test :test-not :key) nil)
          (adjust-array 2 0 nil (:element-type :initial-element :initial-contents :fill-pointer :displaced-to :displaced-index-offset) nil)
          (adjustable-array-p 1 0 nil nil nil)
          (alpha-char-p 1 0 nil nil nil)
          (alphanumericp 1 0 nil nil nil)
          (append 0 0 t nil nil)
          (apply 2 0 t nil nil)
          (applyhook 4 1 nil nil nil)
          (aref 1 0 t nil nil)
          (array-dimension 2 0 nil nil nil)
          (array-dimensions 1 0 nil nil nil)
          (array-element-type 1 0 nil nil nil)
          (array-has-fill-pointer-p 1 0 nil nil nil)
          (array-in-bounds-p 1 0 t nil nil)
          (array-rank 1 0 nil nil nil)
          (system::array-reader 3 0 nil nil nil)
          (array-row-major-index 1 0 t nil nil)
          (array-total-size 1 0 nil nil nil)
          (arrayp 1 0 nil nil nil)
          (ash 2 0 nil nil nil)
          (asin 1 0 nil nil nil)
          (asinh 1 0 nil nil nil)
          (assoc 2 0 nil (:test :test-not :key) nil)
          (assoc-if 2 0 nil (:key) nil)
          (assoc-if-not 2 0 nil (:key) nil)
          (atan 1 1 nil nil nil)
          (atanh 1 0 nil nil nil)
          (atom 1 0 nil nil nil)
          (system::binary-reader 3 0 nil nil nil)
          (bit 1 0 t nil nil)
          (bit-and 2 1 nil nil nil)
          (bit-andc1 2 1 nil nil nil)
          (bit-andc2 2 1 nil nil nil)
          (bit-eqv 2 1 nil nil nil)
          (bit-ior 2 1 nil nil nil)
          (bit-nand 2 1 nil nil nil)
          (bit-nor 2 1 nil nil nil)
          (bit-not 1 1 nil nil nil)
          (bit-orc1 2 1 nil nil nil)
          (bit-orc2 2 1 nil nil nil)
          (bit-vector-p 1 0 nil nil nil)
          (system::bit-vector-reader 3 0 nil nil nil)
          (bit-xor 2 1 nil nil nil)
          (boole 3 0 nil nil nil)
          (both-case-p 1 0 nil nil nil)
          (boundp 1 0 nil nil nil)
          (butlast 1 1 nil nil nil)
          (byte 2 0 nil nil nil)
          (byte-position 1 0 nil nil nil)
          (byte-size 1 0 nil nil nil)
          (caaaar 1 0 nil nil nil)
          (caaadr 1 0 nil nil nil)
          (caaar 1 0 nil nil nil)
          (caadar 1 0 nil nil nil)
          (caaddr 1 0 nil nil nil)
          (caadr 1 0 nil nil nil)
          (caar 1 0 nil nil nil)
          (cadaar 1 0 nil nil nil)
          (cadadr 1 0 nil nil nil)
          (cadar 1 0 nil nil nil)
          (caddar 1 0 nil nil nil)
          (cadddr 1 0 nil nil nil)
          (caddr 1 0 nil nil nil)
          (cadr 1 0 nil nil nil)
          (car 1 0 nil nil nil)
          (cd 0 1 nil nil nil)
          (cdaaar 1 0 nil nil nil)
          (cdaadr 1 0 nil nil nil)
          (cdaar 1 0 nil nil nil)
          (cdadar 1 0 nil nil nil)
          (cdaddr 1 0 nil nil nil)
          (cdadr 1 0 nil nil nil)
          (cdar 1 0 nil nil nil)
          (cddaar 1 0 nil nil nil)
          (cddadr 1 0 nil nil nil)
          (cddar 1 0 nil nil nil)
          (cdddar 1 0 nil nil nil)
          (cddddr 1 0 nil nil nil)
          (cdddr 1 0 nil nil nil)
          (cddr 1 0 nil nil nil)
          (cdr 1 0 nil nil nil)
          (ceiling 1 1 nil nil nil)
          (char 2 0 nil nil nil)
          (char-bit 2 0 nil nil nil)
          (char-bits 1 0 nil nil nil)
          (char-code 1 0 nil nil nil)
          (char-downcase 1 0 nil nil nil)
          (char-equal 1 0 t nil nil)
          (char-font 1 0 nil nil nil)
          (char-greaterp 1 0 t nil nil)
          (char-int 1 0 nil nil nil)
          (char-lessp 1 0 t nil nil)
          (char-name 1 0 nil nil nil)
          (char-not-equal 1 0 t nil nil)
          (char-not-greaterp 1 0 t nil nil)
          (char-not-lessp 1 0 t nil nil)
          (system::char-reader 3 0 nil nil nil)
          (char-upcase 1 0 nil nil nil)
          (char/= 1 0 t nil nil)
          (char< 1 0 t nil nil)
          (char<= 1 0 t nil nil)
          (char= 1 0 t nil nil)
          (char> 1 0 t nil nil)
          (char>= 1 0 t nil nil)
          (character 1 0 nil nil nil)
          (characterp 1 0 nil nil nil)
          (cis 1 0 nil nil nil)
          (clear-input 0 1 nil nil nil)
          (clear-output 0 1 nil nil nil)
          (close 1 0 nil (:abort) nil)
          (system::closure-codevec 1 0 nil nil nil)
          (system::closure-consts 1 0 nil nil nil)
          (system::closure-name 1 0 nil nil nil)
          (system::closure-reader 3 0 nil nil nil)
          (system::closurep 1 0 nil nil nil)
          (clrhash 1 0 nil nil nil)
          (code-char 1 2 nil nil nil)
          (coerce 2 0 nil nil nil)
          (system::comment-reader 3 0 nil nil nil)
          (commonp 1 0 nil nil nil)
          (compiled-function-p 1 0 nil nil nil)
          (complex 1 1 nil nil nil)
          (system::complex-reader 3 0 nil nil nil)
          (complexp 1 0 nil nil nil)
          (concatenate 1 0 t nil nil)
          (conjugate 1 0 nil nil nil)
          (cons 2 0 nil nil nil)
          (consp 1 0 nil nil nil)
          (constantp 1 0 nil nil nil)
          (copy-alist 1 0 nil nil nil)
          (copy-list 1 0 nil nil nil)
          (copy-readtable 0 2 nil nil nil)
          (copy-seq 1 0 nil nil nil)
          (copy-tree 1 0 nil nil nil)
          (cos 1 0 nil nil nil)
          (cosh 1 0 nil nil nil)
          (count 2 0 nil (:from-end :start :end :key :test :test-not) nil)
          (count-if 2 0 nil (:from-end :start :end :key) nil)
          (count-if-not 2 0 nil (:from-end :start :end :key) nil)
          (system::debug 0 0 nil nil nil)
          (system::decimal-string 1 0 nil nil nil)
          (decode-float 1 0 nil nil nil)
          (delete 2 0 nil (:from-end :start :end :key :test :test-not :count) nil)
          (delete-dir 1 0 nil nil nil)
          (delete-duplicates 1 0 nil (:from-end :start :end :key :test :test-not) nil)
          (delete-file 1 0 nil nil nil)
          (delete-if 2 0 nil (:from-end :start :end :key :count) nil)
          (delete-if-not 2 0 nil (:from-end :start :end :key :count) nil)
          (denominator 1 0 nil nil nil)
          (deposit-field 3 0 nil nil nil)
          (system::describe-frame 1 0 nil nil nil)
          (digit-char 1 2 nil nil nil)
          (digit-char-p 1 1 nil nil nil)
          (directory 0 1 nil (:full) nil)
          (directory-namestring 1 0 nil nil nil)
          (system::double-float-p 1 0 nil nil nil)
          (dpb 3 0 nil nil nil)
          (system::driver 1 0 nil nil nil)
          (eighth 1 0 nil nil nil)
          (elt 2 0 nil nil nil)
          (endp 1 0 nil nil nil)
          (enough-namestring 1 1 nil nil nil)
          (eq 2 0 nil nil nil)
          (eql 2 0 nil nil nil)
          (equal 2 0 nil nil nil)
          (equalp 2 0 nil nil nil)
          (error 1 0 t nil nil)
          (eval 1 0 nil nil nil)
          (system::eval-at 2 0 nil nil nil)
          (system::eval-frame-p 1 0 nil nil nil)
          (evalhook 3 1 nil nil nil)
          (evenp 1 0 nil nil nil)
          (every 2 0 t nil nil)
          ;(execute 1 2 nil nil nil)
          ;(execute 1 0 t nil nil)
          (exp 1 0 nil nil nil)
          (export 1 1 nil nil nil)
          (expt 2 0 nil nil nil)
          (exquo 2 0 nil nil nil)
          (fboundp 1 0 nil nil nil)
          (fceiling 1 1 nil nil nil)
          (system::feature-reader 3 0 nil nil nil)
          (ffloor 1 1 nil nil nil)
          (fifth 1 0 nil nil nil)
          (file-author 1 0 nil nil nil)
          (file-length 1 0 nil nil nil)
          (file-namestring 1 0 nil nil nil)
          (file-position 1 1 nil nil nil)
          (file-write-date 1 0 nil nil nil)
          (fill 2 0 nil (:start :end) nil)
          (fill-pointer 1 0 nil nil nil)
          (find 2 0 nil (:from-end :start :end :key :test :test-not) nil)
          (find-all-symbols 1 0 nil nil nil)
          (find-if 2 0 nil (:from-end :start :end :key) nil)
          (find-if-not 2 0 nil (:from-end :start :end :key) nil)
          (find-package 1 0 nil nil nil)
          (find-symbol 1 1 nil nil nil)
          (finish-output 0 1 nil nil nil)
          (first 1 0 nil nil nil)
          (system::fixnump 1 0 nil nil nil)
          (float 1 1 nil nil nil)
          (float-digits 1 1 nil nil nil)
          (float-precision 1 0 nil nil nil)
          (float-radix 1 0 nil nil nil)
          (float-sign 1 1 nil nil nil)
          (floatp 1 0 nil nil nil)
          (floor 1 1 nil nil nil)
          (fmakunbound 1 0 nil nil nil)
          (force-output 0 1 nil nil nil)
          (fourth 1 0 nil nil nil)
          (system::frame-down 2 0 nil nil nil)
          (system::frame-down-1 2 0 nil nil nil)
          (system::frame-up 2 0 nil nil nil)
          (system::frame-up-1 2 0 nil nil nil)
          (fresh-line 0 1 nil nil nil)
          (fround 1 1 nil nil nil)
          (ftruncate 1 1 nil nil nil)
          (funcall 1 0 t nil nil)
          (system::function-reader 3 0 nil nil nil)
          (functionp 1 0 nil nil nil)
          (gc 0 0 nil nil nil)
          (gcd 0 0 t nil nil)
          (gensym 0 1 nil nil nil)
          (get 2 1 nil nil nil)
          (get-dispatch-macro-character 2 1 nil nil nil)
          (get-internal-real-time 0 0 nil nil nil)
          (get-internal-run-time 0 0 nil nil nil)
          (get-macro-character 1 1 nil nil nil)
          (get-output-stream-string 1 0 nil nil nil)
          (get-properties 2 0 nil nil nil)
          (get-universal-time 0 0 nil nil nil)
          (getf 2 1 nil nil nil)
          (gethash 2 1 nil nil nil)
          (graphic-char-p 1 0 nil nil nil)
          (hash-table-count 1 0 nil nil nil)
          (system::hash-table-iterate 1 0 nil nil nil)
          (system::hash-table-iterator 1 0 nil nil nil)
          (hash-table-p 1 0 nil nil nil)
          (system::hexadecimal-reader 3 0 nil nil nil)
          (host-namestring 1 0 nil nil nil)
          (identity 1 0 nil nil nil)
          (imagpart 1 0 nil nil nil)
          (import 1 1 nil nil nil)
          (in-package 1 0 nil (:nicknames :use) nil)
          (system::initial-contents-aux 1 0 nil nil nil)
          (input-stream-p 1 0 nil nil nil)
          (int-char 1 0 nil nil nil)
          (integer-decode-float 1 0 nil nil nil)
          (integer-length 1 0 nil nil nil)
          (integerp 1 0 nil nil nil)
          (intern 1 1 nil nil nil)
          (isqrt 1 0 nil nil nil)
          (system::keyword-test 2 0 nil nil nil)
          (keywordp 1 0 nil nil nil)
          (system::label-definiion-reader 3 0 nil nil nil)
          (system::label-reference-reader 3 0 nil nil nil)
          (last 1 0 nil nil nil)
          (lcm 0 0 t nil nil)
          (ldb 2 0 nil nil nil)
          (ldb-test 2 0 nil nil nil)
          (ldiff 2 0 nil nil nil)
          (length 1 0 nil nil nil)
          (system::line-comment-reader 2 0 nil nil nil)
          (system::line-position 0 1 nil nil nil)
          (lisp-implementation-type 0 0 nil nil nil)
          (lisp-implementation-version 0 0 nil nil nil)
          (list 0 0 t nil nil)
          (list* 1 0 t nil nil)
          (system::list-access 2 0 nil nil nil)
          (system::list-access-set 3 0 nil nil nil)
          (list-all-packages 0 0 nil nil nil)
          (system::list-elt 2 0 nil nil nil)
          (system::list-endtest 2 0 nil nil nil)
          (system::list-fe-init 1 0 nil nil nil)
          (system::list-fe-init-end 2 0 nil nil nil)
          (system::list-init-start 2 0 nil nil nil)
          (list-length 1 0 nil nil nil)
          (system::list-llength 1 0 nil nil nil)
          (system::list-nreverse 1 0 nil nil nil)
          (system::list-set-elt 3 0 nil nil nil)
          (system::list-upd 2 0 nil nil nil)
          (listen 0 1 nil nil nil)
          (listp 1 0 nil nil nil)
          (system::load-eval-reader 3 0 nil nil nil)
          (log 1 1 nil nil nil)
          (system::log10 1 0 nil nil nil)
          (system::log2 1 0 nil nil nil)
          (logand 0 0 t nil nil)
          (logandc1 2 0 nil nil nil)
          (logandc2 2 0 nil nil nil)
          (logbitp 2 0 nil nil nil)
          (logcount 1 0 nil nil nil)
          (logeqv 0 0 t nil nil)
          (logior 0 0 t nil nil)
          (lognand 2 0 nil nil nil)
          (lognor 2 0 nil nil nil)
          (lognot 1 0 nil nil nil)
          (logorc1 2 0 nil nil nil)
          (logorc2 2 0 nil nil nil)
          (logtest 2 0 nil nil nil)
          (logxor 0 0 t nil nil)
          (long-float-digits 0 0 nil nil nil)
          (system::long-float-p 1 0 nil nil nil)
          (lower-case-p 1 0 nil nil nil)
          (system::lpar-reader 2 0 nil nil nil)
          ;(machine-instance 0 0 nil nil nil)
          ;(machine-type 0 0 nil nil nil)
          ;(machine-version 0 0 nil nil nil)
          (macro-function 1 0 nil nil nil)
          (macroexpand 1 1 nil nil nil)
          (macroexpand-1 1 1 nil nil nil)
          (make-array 1 0 nil (:adjustable :element-type :initial-element :initial-contents :fill-pointer :displaced-to :displaced-index-offset) nil)
          (system::make-bit-vector 1 0 nil nil nil)
          (make-broadcast-stream 0 0 t nil nil)
          (make-buffered-input-stream 2 0 nil nil nil)
          (make-buffered-output-stream 1 0 nil nil nil)
          (make-char 1 2 nil nil nil)
          (system::make-code-vector 1 0 nil nil nil)
          (make-concatenated-stream 0 0 t nil nil)
          (make-dir 1 0 nil nil nil)
          (make-dispatch-macro-character 1 2 nil nil nil)
          (make-echo-stream 2 0 nil nil nil)
          (make-hash-table 0 0 nil (:initial-contents :test :size :rehash-size :rehash-threshold) nil)
          (make-list 1 0 nil (:initial-element) nil)
          (system::make-load-time-eval 1 0 nil nil nil)
          (make-package 1 0 nil (:nicknames :use) nil)
          (make-pathname 0 0 nil (:defaults :host :device :directory :name :type :version) nil)
          #+UNIX (make-pipe-input-stream 1 0 nil nil nil)
          #+UNIX (make-pipe-output-stream 1 0 nil nil nil)
          (make-random-state 0 1 nil nil nil)
          (make-sequence 2 0 nil (:initial-element :update) nil)
          (make-string 1 0 nil (:initial-element) nil)
          (make-string-input-stream 1 2 nil nil nil)
          (make-string-output-stream 0 1 nil nil nil)
          (system::make-string-push-stream 1 0 nil nil nil)
          (make-symbol 1 0 nil nil nil)
          (make-synonym-stream 1 0 nil nil nil)
          (make-two-way-stream 2 0 nil nil nil)
          (makunbound 1 0 nil nil nil)
          (map 3 0 t nil nil)
          (system::map-all-symbols 1 0 nil nil nil)
          (system::map-external-symbols 2 0 nil nil nil)
          (system::map-symbols 2 0 nil nil nil)
          (mapc 2 0 t nil nil)
          (mapcan 2 0 t nil nil)
          (mapcar 2 0 t nil nil)
          (mapcon 2 0 t nil nil)
          (maphash 2 0 nil nil nil)
          (mapl 2 0 t nil nil)
          (maplist 2 0 t nil nil)
          (mask-field 2 0 nil nil nil)
          (max 1 0 t nil nil)
          (member 2 0 nil (:test :test-not :key) nil)
          (member-if 2 0 nil (:key) nil)
          (member-if-not 2 0 nil (:key) nil)
          (merge 4 0 nil (:key) nil)
          (merge-pathnames 1 2 nil nil nil)
          (min 1 0 t nil nil)
          (minusp 1 0 nil nil nil)
          (mismatch 2 0 nil (:from-end :start1 :end1 :start2 :end2 :key :test :test-not) nil)
          (mod 2 0 nil nil nil)
          (name-char 1 0 nil nil nil)
          #-ATARI (namestring 1 0 nil nil nil)
          #+ATARI (namestring 1 1 nil nil nil)
          (nbutlast 1 1 nil nil nil)
          (nconc 0 0 t nil nil)
          (ninth 1 0 nil nil nil)
          (not 1 0 nil nil nil)
          (system::not-feature-reader 3 0 nil nil nil)
          (system::not-readable-reader 3 0 nil nil nil)
          (notany 2 0 t nil nil)
          (notevery 2 0 t nil nil)
          (nreconc 2 0 nil nil nil)
          (nreverse 1 0 nil nil nil)
          (nstring-capitalize 1 0 nil (:start :end) nil)
          (nstring-downcase 1 0 nil (:start :end) nil)
          (nstring-upcase 1 0 nil (:start :end) nil)
          (nsublis 2 0 nil (:test :test-not :key) nil)
          (nsubst 3 0 nil (:test :test-not :key) nil)
          (nsubst-if 3 0 nil (:key) nil)
          (nsubst-if-not 3 0 nil (:key) nil)
          (nsubstitute 3 0 nil (:from-end :start :end :key :test :test-not :count) nil)
          (nsubstitute-if 3 0 nil (:from-end :start :end :key :count) nil)
          (nsubstitute-if-not 3 0 nil (:from-end :start :end :key :count) nil)
          (nth 2 0 nil nil nil)
          (nthcdr 2 0 nil nil nil)
          (null 1 0 nil nil nil)
          (numberp 1 0 nil nil nil)
          (numerator 1 0 nil nil nil)
          (system::octal-reader 3 0 nil nil nil)
          (oddp 1 0 nil nil nil)
          (open 1 0 nil (:direction :element-type :if-exists :if-does-not-exist) nil)
          (output-stream-p 1 0 nil nil nil)
          (package-name 1 0 nil nil nil)
          (package-nicknames 1 0 nil nil nil)
          (package-shadowing-symbols 1 0 nil nil nil)
          (package-use-list 1 0 nil nil nil)
          (package-used-by-list 1 0 nil nil nil)
          (packagep 1 0 nil nil nil)
          (pairlis 2 1 nil nil nil)
          (system::parse-body 1 2 nil nil nil)
          (parse-integer 1 0 nil (:start :end :radix :junk-allowed) nil)
          (parse-namestring 1 2 nil (:start :end :junk-allowed) nil)
          (pathname 1 0 nil nil nil)
          (pathname-device 1 0 nil nil nil)
          (pathname-directory 1 0 nil nil nil)
          (pathname-host 1 0 nil nil nil)
          (pathname-name 1 0 nil nil nil)
          (system::pathname-reader 3 0 nil nil nil)
          (pathname-type 1 0 nil nil nil)
          (pathname-version 1 0 nil nil nil)
          (pathnamep 1 0 nil nil nil)
          (peek-char 0 5 nil nil nil)
          (phase 1 0 nil nil nil)
          (plusp 1 0 nil nil nil)
          (position 2 0 nil (:from-end :start :end :key :test :test-not) nil)
          (position-if 2 0 nil (:from-end :start :end :key) nil)
          (position-if-not 2 0 nil (:from-end :start :end :key) nil)
          (pprint 1 1 nil nil nil)
          (prin1 1 1 nil nil nil)
          (prin1-to-string 1 0 nil nil nil)
          (princ 1 1 nil nil nil)
          (princ-to-string 1 0 nil nil nil)
          (print 1 1 nil nil nil)
          (probe-file 1 0 nil nil nil)
          (proclaim 1 0 nil nil nil)
          (system::puthash 3 0 nil nil nil)
          (system::quote-reader 2 0 nil nil nil)
          (system::radix-reader 3 0 nil nil nil)
          (random 1 1 nil nil nil)
          (random-state-p 1 0 nil nil nil)
          (rassoc 2 0 nil (:test :test-not :key) nil)
          (rassoc-if 2 0 nil (:key) nil)
          (rassoc-if-not 2 0 nil (:key) nil)
          (rational 1 0 nil nil nil)
          (rationalize 1 0 nil nil nil)
          (rationalp 1 0 nil nil nil)
          (read 0 4 nil nil nil)
          (read-byte 1 2 nil nil nil)
          (read-char 0 4 nil nil nil)
          (read-char-no-hang 0 4 nil nil nil)
          (read-delimited-list 1 2 nil nil nil)
          (system::read-eval-print 1 1 nil nil nil)
          (system::read-eval-reader 3 0 nil nil nil)
          (system::read-form 1 1 nil nil nil)
          (read-from-string 1 2 nil (:preserve-whitespace :start :end) nil)
          (read-line 0 4 nil nil nil)
          (read-preserving-whitespace 0 4 nil nil nil)
          (readtablep 1 0 nil nil nil)
          (realpart 1 0 nil nil nil)
          (system::redo-eval-frame 1 0 nil nil nil)
          (reduce 2 0 nil (:from-end :start :end :initial-value) nil)
          (rem 2 0 nil nil nil)
          (remhash 2 0 nil nil nil)
          (remove 2 0 nil (:from-end :start :end :key :test :test-not :count) nil)
          (remove-duplicates 1 0 nil (:from-end :start :end :key :test :test-not) nil)
          (remove-if 2 0 nil (:from-end :start :end :key :count) nil)
          (remove-if-not 2 0 nil (:from-end :start :end :key :count) nil)
          (remprop 2 0 nil nil nil)
          (rename-file 2 0 nil nil nil)
          (rename-package 2 1 nil nil nil)
          (replace 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (rest 1 0 nil nil nil)
          (system::return-from-eval-frame 2 0 nil nil nil)
          (revappend 2 0 nil nil nil)
          (reverse 1 0 nil nil nil)
          (room 0 0 nil nil nil)
          (round 1 1 nil nil nil)
          (system::rpar-reader 2 0 nil nil nil)
          (rplaca 2 0 nil nil nil)
          (rplacd 2 0 nil nil nil)
          (system::same-env-as 2 0 nil nil nil)
          (savemem 1 0 nil nil nil)
          (sbit 1 0 t nil nil)
          (scale-float 2 0 nil nil nil)
          (schar 2 0 nil nil nil)
          (search 2 0 nil (:from-end :start1 :end1 :start2 :end2 :key :test :test-not) nil)
          (system::search-string-equal 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (system::search-string= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (second 1 0 nil nil nil)
          (system::sequencep 1 0 nil nil nil)
          (set 2 0 nil nil nil)
          (set-char-bit 3 0 nil nil nil)
          (set-dispatch-macro-character 3 1 nil nil nil)
          (system::set-fill-pointer 2 0 nil nil nil)
          (set-macro-character 2 2 nil nil nil)
          (set-syntax-from-char 2 2 nil nil nil)
          (seventh 1 0 nil nil nil)
          (shadow 1 1 nil nil nil)
          (shadowing-import 1 1 nil nil nil)
          ;(shell 0 1 nil nil nil)
          (system::short-float-p 1 0 nil nil nil)
          (show-stack 0 0 nil nil nil)
          (signum 1 0 nil nil nil)
          (system::simple-array-p 1 0 nil nil nil)
          (simple-bit-vector-p 1 0 nil nil nil)
          (simple-string-p 1 0 nil nil nil)
          (simple-vector-p 1 0 nil nil nil)
          (sin 1 0 nil nil nil)
          (system::single-float-p 1 0 nil nil nil)
          (sinh 1 0 nil nil nil)
          (sixth 1 0 nil nil nil)
          (software-type 0 0 nil nil nil)
          (software-version 0 0 nil nil nil)
          (some 2 0 t nil nil)
          (sort 2 0 nil (:key :start :end) nil)
          (special-form-p 1 0 nil nil nil)
          (system::special-variable-p 1 0 nil nil nil)
          (sqrt 1 0 nil nil nil)
          (stable-sort 2 0 nil (:key :start :end) nil)
          (standard-char-p 1 0 nil nil nil)
          (system::store 2 0 t nil nil)
          (system::store-char 3 0 nil nil nil)
          (system::store-schar 3 0 nil nil nil)
          (stream-element-type 1 0 nil nil nil)
          (streamp 1 0 nil nil nil)
          (string 1 0 nil nil nil)
          (system::string-both-trim 3 0 nil nil nil)
          (string-capitalize 1 0 nil (:start :end) nil)
          (string-char-p 1 0 nil nil nil)
          (string-concat 0 0 t nil nil)
          (string-downcase 1 0 nil (:start :end) nil)
          (string-equal 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string-greaterp 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (system::string-input-stream-index 1 0 nil nil nil)
          (string-lessp 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string-not-equal 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string-not-greaterp 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string-not-lessp 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (system::string-reader 2 0 nil nil nil)
          (string-upcase 1 0 nil (:start :end) nil)
          (string/= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string< 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string<= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string> 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (string>= 2 0 nil (:start1 :end1 :start2 :end2) nil)
          (stringp 1 0 nil nil nil)
          (system::structure-reader 3 0 nil nil nil)
          (sublis 2 0 nil (:test :test-not :key) nil)
          (system::subr-info 1 0 nil nil nil)
          (subseq 2 1 nil nil nil)
          (subst 3 0 nil (:test :test-not :key) nil)
          (subst-if 3 0 nil (:key) nil)
          (subst-if-not 3 0 nil (:key) nil)
          (substitute 3 0 nil (:from-end :start :end :key :test :test-not :count) nil)
          (substitute-if 3 0 nil (:from-end :start :end :key :count) nil)
          (substitute-if-not 3 0 nil (:from-end :start :end :key :count) nil)
          (substring 2 1 nil nil nil)
          (svref 2 0 nil nil nil)
          (system::svstore 3 0 nil nil nil)
          (sxhash 1 0 nil nil nil)
          (symbol-function 1 0 nil nil nil)
          (symbol-name 1 0 nil nil nil)
          (symbol-package 1 0 nil nil nil)
          (symbol-plist 1 0 nil nil nil)
          (symbol-value 1 0 nil nil nil)
          (symbolp 1 0 nil nil nil)
          (system::syntax-error-reader 3 0 nil nil nil)
          (tailp 2 0 nil nil nil)
          (tan 1 0 nil nil nil)
          (tanh 1 0 nil nil nil)
          (tenth 1 0 nil nil nil)
          (terpri 0 1 nil nil nil)
          (system::the-frame 0 0 nil nil nil)
          (third 1 0 nil nil nil)
          (tree-equal 2 0 nil (:test :test-not) nil)
          (truename 1 0 nil nil nil)
          (truncate 1 1 nil nil nil)
          (type-of 1 0 nil nil nil)
          (unexport 1 1 nil nil nil)
          (unintern 1 1 nil nil nil)
          (system::uninterned-reader 3 0 nil nil nil)
          (unread-char 1 1 nil nil nil)
          (unuse-package 1 1 nil nil nil)
          (system::unwind-to-driver 0 0 nil nil nil)
          (upper-case-p 1 0 nil nil nil)
          (use-package 1 1 nil nil nil)
          (system::use-package-aux 1 0 nil nil nil)
          #+UNIX (user-homedir-pathname 0 1 nil nil nil)
          (values 0 0 t nil nil)
          (values-list 1 0 nil nil nil)
          (vector 0 0 t nil nil)
          (system::vector-endtest 2 0 nil nil nil)
          (system::vector-fe-endtest 2 0 nil nil nil)
          (system::vector-fe-init 1 0 nil nil nil)
          (system::vector-fe-init-end 2 0 nil nil nil)
          (system::vector-fe-upd 2 0 nil nil nil)
          (system::vector-init 1 0 nil nil nil)
          (system::vector-init-start 2 0 nil nil nil)
          (system::vector-length 1 0 nil nil nil)
          (vector-pop 1 0 nil nil nil)
          (vector-push 2 0 nil nil nil)
          (vector-push-extend 2 1 nil nil nil)
          (system::vector-reader 3 0 nil nil nil)
          (system::vector-upd 2 0 nil nil nil)
          (vectorp 1 0 nil nil nil)
          (system::version 0 1 nil nil nil)
          (write 1 0 nil (:case :level :length :gensym :escape :radix :base :array :circle :pretty :closure :stream) nil)
          (write-byte 2 0 nil nil nil)
          (write-char 1 1 nil nil nil)
          (write-line 1 1 nil (:start :end) nil)
          (write-string 1 1 nil (:start :end) nil)
          (write-to-string 1 0 nil (:case :level :length :gensym :escape :radix :base :array :circle :pretty :closure) nil)
          #-CLISP1 (xgcd 0 0 t nil nil)
          (zerop 1 0 nil nil nil)
) ) ) )  )
(defconstant function-codes
  (let ((hashtable (make-hash-table :test #'eq)))
    (dotimes (i (* 3 256))
      (let ((sym (%funtabref i))) ; Name der Funktion FUNTAB[i]
        (when sym (setf (gethash sym hashtable) i))
    ) )
    hashtable
) )
(defconstant funtabR-index ; Startindex der FUNTABR bzgl. FUNTAB
  (dotimes (i (* 3 256))
    (let ((sym (%funtabref i)))
      (multiple-value-bind (name req opt rest-p) (subr-info sym)
        (declare (ignore name req opt))
        (when rest-p (return i))
) ) ) )
(defun CALLS-code (funtab-index)
  (if (< funtab-index 256)
    `(CALLS1 ,funtab-index)
    `(CALLS2 ,(- funtab-index 256))
) )

; Hilfsfunktion: mapcan, aber mit append statt nconc:
#|
#-CLISP
(defun mapcap (fun &rest lists &aux (L nil))
  (loop
    (setq L
      (nconc
        (reverse
          (apply fun
            (maplist #'(lambda (listsr)
                         (if (atom (car listsr))
                           (return)
                           (pop (car listsr))
                       ) )
                     lists
        ) ) )
        L
      )
  ) )
  (nreverse L)
)
|#
#-CLISP
(defun mapcap (fun &rest lists)
  (apply #'append (apply #'mapcar fun lists))
)

; Hilfsfunktion: mapcon, aber mit append statt nconc:
#|
#-CLISP
(defun maplap (fun &rest lists &aux (L nil))
  (loop
    (setq L
      (nconc
        (reverse
          (apply fun
            (maplist #'(lambda (listsr)
                         (if (atom (car listsr))
                           (return)
                           (prog1
                             (car listsr)
                             (setf (car listsr) (cdr (car listsr)))
                       ) ) )
                     lists
        ) ) )
        L
      )
  ) )
  (nreverse L)
)
|#
#-CLISP
(defun maplap (fun &rest lists)
  (apply #'append (apply #'maplist fun lists))
)

; Ersatz für TAILP, auch für dotted lists:
(defun mytailp (sublist list)
  (loop
    (when (eq sublist list) (return t))
    (when (atom list) (return nil))
    (setq list (cdr list))
) )

; (memq item const-symbollist) == (member item const-symbollist :test #'eq),
; nur der boolesche Wert.
(defmacro memq (item list)
  (if (and (constantp list) (listp (eval list)))
    `(case ,item (,(eval list) t) (t nil))
    `(member ,item ,list :test #'eq)
) )

; Fehlermeldungsfunktion
(defun compiler-error (caller &optional where)
  (error #+DEUTSCH "Fehler im Compiler!! Aufgetreten in ~A~@[ bei ~A~]."
         #+ENGLISH "Compiler bug!! Occurred in ~A~@[ at ~A~]."
         caller where
) )



;                      S T A C K - V E R W A L T U N G

; Ein Stackzustand beschreibt, was sich zur Laufzeit alles auf den beiden
; Stacks befinden wird.
; Genaue Struktur:
; (item1 ... itemk . fun)
; Das ist im Speicher in Wirklichkeit eine Baumstruktur!
; Es bedeuten hierbei:
;  fun = FNODE der Funktion, in der gezählt wird.
;  item = eines der folgenden:
;    n (Integer >=0) : n Lisp-Objekte auf dem STACK
;                      belegt n STACK-Einträge
;    (BIND n)        : einen Bindungsframe für n Variablen,
;                      belegt 1+2*n STACK-Einträge und 0 SP-Einträge
;                      Muß bei Unwind explizit aufgelöst werden
;    PROGV           : ein Bindungsframe für beliebig viele Variablen,
;                      belegt ? STACK-Einträge und 1 SP-Eintrag (Pointer über
;                      den Frame = alter STACK)
;                      Muß bei Unwind explizit aufgelöst werden
;    CATCH           : ein CATCH-Frame
;                      belegt 3 STACK-Einträge und 2+*jmpbuf-size* SP-Einträge
;    UNWIND-PROTECT  : ein Unwind-Protect-Frame
;                      belegt 2 STACK-Einträge und 2+*jmpbuf-size* SP-Einträge
;                      Muß bei Unwind aufgelöst und der Cleanup ausgeführt
;                      werden
;    CLEANUP         : während der Cleanup-Phase eines UNWIND-PROTECT
;                      belegt ? STACK-Einträge und 3 SP-Einträge
;                      (der untere ist Pointer über den Frame = alter STACK)
;    BLOCK           : ein BLOCK-Frame
;                      belegt 3 STACK-Einträge und 2+*jmpbuf-size* SP-Einträge
;                      Muß bei Unwind explizit aufgelöst werden
;    (TAGBODY n)     : ein TAGBODY-Frame, der n Tags aufhebt
;                      belegt 3+n STACK-Einträge und 1+*jmpbuf-size* SP-Einträge
;                      Muß bei Unwind explizit aufgelöst werden
;    MVCALLP         : Vorbereitung für MVCALL
;                      belegt 1 STACK-Eintrag und 1 SP-Eintrag (Pointer über
;                      FRAME = STACK)
;    MVCALL          : viele Lisp-Objekte
;                      belegt ? STACK-Einträge und 1 SP-Eintrag (Pointer über
;                      Frame = alter STACK)

(defvar *stackz*)    ; der aktuelle Stackzustand

; (stackz-fun stackz) extrahiert aus einem Stackzustand die Funktion, in der
; gerade gearbeitet wird.
#|
(defun stackz-fun (stackz)
  (loop (when (atom stackz) (return)) (setq stackz (cdr stackz)))
  stackz
)
|#
; äquivalent, aber schneller:
(defun stackz-fun (stackz)
  (if (atom stackz) stackz (cdr (last stackz)))
)

; (in-same-function-p stackz1 stackz2) stellt fest, ob in beiden Stackzuständen
; in derselben Funktion gearbeitet wird.
(defun in-same-function-p (stackz1 stackz2)
  (eq (stackz-fun stackz1) (stackz-fun stackz2))
)

; (zugriff-in-stack stackz1 stackz2)
; Für den Zugriff auf lokale Variablen im Stack:
; ergibt zu zwei Stackzuständen stackz1 und stackz2, die beide innerhalb
; derselben Funktion liegen und wo stackz1 "tiefer" ist als stackz2:
; 2 Werte: NIL und n, falls (stackz2) = (STACK+4*n) von stackz1 aus,
;          k und n, falls (stackz2) = ((SP+4*k)+4*n) von stackz1 aus.
; (Falls stackz2 mit BLOCK oder TAGBODY beginnt, ist immer der Zugriff auf die
;  consvar eines Block- bzw. Tagbody-Frames gemeint.)
(defun zugriff-in-stack (stackz1 stackz2 &aux (k nil) (n 0) (kd 0))
  (loop ; beim Durchlaufen der Stacks nach oben:
    ; momentanes STACK ist STACK+4*n (bei k=NIL) bzw. (SP+4*k)+4*n,
    ; momentanes SP ist SP+4*kd (bei k=NIL) bzw. SP+4*(k+kd).
    (when (eq stackz1 stackz2) (return))
    (when (atom stackz1) (compiler-error 'zugriff-in-stack "STACKZ-END"))
    (let ((item (car stackz1)))
      (cond ((integerp item) (setq n (+ n item)))
            ((consp item)
             (case (first item)
               (BIND    (setq n (+ n (+ 1 (* 2 (second item))))))
               (TAGBODY (setq kd (+ kd (+ 1 *jmpbuf-size*))
                              n (+ n (+ 3 (second item)))
               )        )
               (t (compiler-error 'zugriff-in-stack "STACKZ-LISTITEM"))
            ))
            (t
             (case item
               (PROGV          (setq k (if k (+ k kd) kd) kd 1 n 0))
               (CATCH          (setq kd (+ kd (+ 2 *jmpbuf-size*)) n (+ n 3)))
               (UNWIND-PROTECT (setq kd (+ kd (+ 2 *jmpbuf-size*)) n (+ n 2)))
               (CLEANUP        (setq k (if k (+ k kd) kd) kd 3 n 0))
               (BLOCK          (setq kd (+ kd (+ 2 *jmpbuf-size*)) n (+ n 3)))
               (MVCALLP        (setq kd (+ kd 1) n (+ n 1)))
               (MVCALL         (setq k (if k (+ k kd) kd) kd 1 n 0))
               (t (compiler-error 'zugriff-in-stack "STACKZ-ITEM"))
    ) )     ))
    (setq stackz1 (cdr stackz1))
  )
  (when (and (consp stackz2) ; beim Zugriff auf BLOCK- bzw. TAGBODY-consvar:
             (or (eq (car stackz2) 'BLOCK)
                 (and (consp (car stackz2)) (eq (first (car stackz2)) 'TAGBODY))
        )    )
    (setq n (+ n 2)) ; consvar liegt genau 2 Einträge höher als Frameanfang
  )
  (values k n)
)

; (expand-UNWIND stackz1 stackz2 for-value)
; liefert ein zu (UNWIND stackz1 stackz2 for-value) äquivalentes Codestück,
; bestehend aus
; (SKIP n), (SKIPI k n), (SKIPSP k), (VALUES0), (UNWIND-PROTECT-CLEANUP),
; (UNBIND1), (BLOCK-CLOSE), (TAGBODY-CLOSE).
; Es muß - ausgehend von stackz1 - den Stack so bereinigen, daß danach der
; Stackzustand stackz2 vorliegt. Bei for-value=NIL können die Werte dabei
; weggeworfen werden.
(defun expand-UNWIND (stackz1 stackz2 for-value
                      &aux (k nil) (n 0) (kd 0) (codelist nil))
  (flet ((here () ; bis hierher erst einmal die Stacks hochsetzen
           (if k
             (progn
               (push `(SKIPI ,k ,n) codelist)
               (when (<= kd 0) (compiler-error 'expand-UNWIND "SP-depth"))
               (when (> kd 1) (push `(SKIPSP ,(- kd 1)) codelist))
             )
             (progn
               (when (> n 0) (push `(SKIP ,n) codelist))
               (when (> kd 0) (push `(SKIPSP ,kd) codelist))
           ) )
           (setq k nil n 0 kd 0)
        ))
    (loop ; beim Durchlaufen der Stacks nach oben:
      ; momentanes STACK ist STACK+4*n (bei k=NIL) bzw. (SP+4*k)+4*n,
      ; momentanes SP ist SP+4*kd (bei k=NIL) bzw. SP+4*(k+kd).
      (when (eq stackz1 stackz2) (here) (return))
      (when (atom stackz1) (compiler-error 'expand-UNWIND "STACKZ-END"))
      (let ((item (car stackz1)))
        (cond ((integerp item) (setq n (+ n item)))
              ((consp item)
               (case (first item)
                 (BIND    (here) (push '(UNBIND1) codelist))
                 (TAGBODY (here) (push '(TAGBODY-CLOSE) codelist))
                 (t (compiler-error 'expand-UNWIND "STACKZ-LISTITEM"))
              ))
              (t
               (case item
                 (PROGV (here) (push '(UNBIND1) codelist) (setq kd 1))
                 (CATCH (setq kd (+ kd (+ 2 *jmpbuf-size*)) n (+ n 3)))
                 (UNWIND-PROTECT
                   (here)
                   (unless for-value
                      ; bei for-value=NIL wird beim ersten auftretenden
                      ; UNWIND-PROTECT-Frame ein '(VALUES0) eingefügt.
                     (setq for-value t)
                     (push '(VALUES0) codelist)
                   )
                   (push '(UNWIND-PROTECT-CLEANUP) codelist)
                 )
                 (CLEANUP (setq k (if k (+ k kd) kd) kd 3 n 0))
                 (BLOCK (here) (push '(BLOCK-CLOSE) codelist))
                 (MVCALLP (setq kd (+ kd 1) n (+ n 1)))
                 (MVCALL (setq k (if k (+ k kd) kd) kd 1 n 0))
                 (t (compiler-error 'expand-UNWIND "STACKZ-ITEM"))
      ) )     ))
      (setq stackz1 (cdr stackz1))
    )
    (nreverse codelist)
) )



;        F U N C T I O N - E N V I R O N M E N T - V E R W A L T U N G

; mitgegeben vom Interpreter: %fenv%

; Interpreter-Funktions-Environment hat die Gestalt
; %fenv% = NIL oder #(f1 def1 ... fn defn NEXT-ENV), NEXT-ENV von derselben
; Gestalt.
; Damit ist eine Abbildung fi --> defi realisiert.
; defi = (SYSTEM::MACRO . expander)  bedeutet einen lokalen Macro.
; defi = Closure                     bedeutet, daß defi die lokale
;                                    Funktionsdefinition von fi ist
; defi = NIL                         bedeutet, daß eine lokale Funktions-
;                                    definition noch hineinkommt (vgl. LABELS)

; neu konstruiert:
(defvar *fenv*)
; enthält die neuen lexikalischen Funktionsbindungen.
; *fenv* hat dieselbe Gestalt wie %fenv% und endet mit %fenv%:
; #(f1 def1 ... fn defn NEXT-ENV), was eine Abbildung fi --> defi
; realisiert.
; defi = (SYSTEM::MACRO expander)  bedeutet einen lokalen Makro.
; defi = (fdescr . var)            bedeutet, daß die lokale Funktionsdefinition
;           von fi zur Laufzeit in der lexikalischen Variablen var steckt.
;           fnode ist der zu fi gehörige fnode, anfangs noch NIL.
; defi = (fdescr . const)          bedeutet, daß die lokale Funktionsdefinition
;           von fi autonom ist und in der Konstanten const steckt.
;           fnode ist der zu fi gehörige fnode, anfangs noch NIL.
; Dabei ist fdescr ein Cons (fnode . lambdadescr), fnode der zu fi gehörige
; fnode oder NIL, lambdadescr die Liste der Werte von analyze-lambdalist
; oder NIL.

; Suche die lokale Funktionsdefinition des Symbols f in fenv :
; Ergebnis ist:
; SYSTEM::MACRO, expander           bei einem lokalen Macro,
; GLOBAL, Vektor, Index             wenn defi = (svref Vektor Index)
;                                   (also in %fenv% gefunden)
; LOCAL, def, fdescr                wenn defi = def eine Variable oder Konstante
;                                   (also in *fenv* ohne %fenv% gefunden)
; NIL                               falls nicht lokal definiert.
(defun fenv-search (f &optional (fenv *fenv*))
  (loop
    (when (null fenv) (return-from fenv-search nil))
    (unless (simple-vector-p fenv) (compiler-error 'fenv-search))
    (do ((l (1- (length fenv)))
         (i 0 (+ i 2)))
        ((= i l) (setq fenv (svref fenv i)))
      (if (eq f (svref fenv i))
        (let ((def (svref fenv (1+ i))))
          (return-from fenv-search
            (if (consp def)
              (if (eq (car def) 'SYSTEM::MACRO)
                (values 'SYSTEM::MACRO (cdr def))
                (values 'LOCAL (cdr def) (car def))
              )
              (values 'GLOBAL fenv (1+ i))
  ) ) ) ) ) )
)

; Mit einem solchen Funktions-Environment (verkettete Vektoren, mit
; defi = (SYSTEM::MACRO . expander) für Macro-Definitionen zu fi)
; arbeiten die Funktionen
; MACROEXPAND-1, MACROEXPAND, PARSE-BODY:
#|
(MACROEXPAND-1 form fenv) expandiert die gegebene Form im Funktions-Environment
fenv und liefert die 1 mal expandierte Form und T (oder form und NIL, falls
nicht expandierbar).

(MACROEXPAND form fenv) expandiert die gegebene Form im Funktions-Environment
fenv und liefert die sooft wie möglich expandierte Form und T
(oder form und NIL, falls nicht expandierbar).

(PARSE-BODY body docstring-allowed fenv) analysiert den body und spaltet von
ihm die Deklarationen und den Docstring (falls erlaubt und vorhanden) ab.
3 Werte: der übrige body-rest, eine Liste der vorgekommenen declspecs,
der Docstring (oder NIL).
|#


;           B L O C K - E N V I R O N M E N T - V E R W A L T U N G

; mitgegeben vom Interpreter: %benv%

; Interpreter-Block-Environment hat die Gestalt
; %benv% = ((name1 . status1) ... (namen . statusn))
; wobei namei ein Symbol und statusi der Status dieses lexikalisch umfassenden
; Blocks ist: #<DISABLED> falls der Block bereits verlassen wurde, sonst ein
; Pointer in den Stack auf den zugehörigen Block-Frame.

; neu konstruiert:
(defvar *benv*)

; *benv* hat die Gestalt
; ((name1 . block1) ... (namen . blockn) . %benv%)
; wobei blocki der Descriptor des Blocks mit Namen namei ist:
(defstruct (block (:copier nil))
  fnode                 ; Funktion, in der dieser Block definiert ist, ein FNODE
  label                 ; label, an dem dieser Block zu Ende ist
  stackz                ; Stackzustand nach dem Aufbau des Block-Frames
  consvar               ; Variable, die im Stack im Block-Frame liegt und den
                        ; Block-Cons enthält (dessen CDR beim Verlassen des
                        ; Blockes auf #<DISABLED> gesetzt wird)
  used-far              ; Flag, gibt an, ob dieser Block aus einer anderen
                        ; Funktion heraus mit RETURN-FROM verlassen wird.
  for-value             ; gibt an, ob das gesamte Block-Konstrukt Werte
                        ; zurückliefern soll.
)
#+CLISP (remprop 'block 'sys::defstruct-description)

; Sucht nach einem Block mit dem Namen name und liefert:
; NIL                          falls nicht gefunden,
; Block-Descriptor             falls in *benv* gefunden,
; Block-Cons (name . status)   falls in %benv% gefunden.
(defun benv-search (name &optional (benv *benv*))
  (loop
    (when (atom benv) (return nil))
    (when (eq (caar benv) name)
      (if (block-p (cdar benv))
        (return (cdar benv))
        (return (car benv))
    ) )
    (setq benv (cdr benv))
) )


;         T A G B O D Y - E N V I R O N M E N T - V E R W A L T U N G

; mitgegeben vom Interpreter: %genv%

; Interpreter-Tagbody-Environment hat die Gestalt
; %genv% = ((Tagvektor1 . status1) ... (Tagvektorn . statusn))
; wobei Tagvektori ein simple-vector ist, der die anspringbaren Tags enthält,
; statusi der Status dieses lexikalisch umfassenden Tagbodys
; ist: #<DISABLED> falls der Tagbody bereits verlassen wurde, sonst ein
; Pointer in den Stack auf den zugehörigen Tagbody-Frame.

; neu konstruiert:
(defvar *genv*)

; *genv* hat die Gestalt
; ((Tagvektor1 . tagbody1) ... (Tagvektorn . tagbodyn) . %genv%)
; wobei tagbodyi der Descriptor des Tagbodys i ist:
(defstruct (tagbody (:copier nil))
  fnode               ; Funktion, in der dieser Tagbody definiert ist, ein FNODE
  labellist           ; Liste der Labels, parallel zum Tagvektor
  stackz              ; Stackzustand nach dem Aufbau des Tagbody-Frames
  consvar             ; Variable, die im Stack im Tagbody-Frame liegt und den
                      ; Tagbody-Cons enthält (dessen CDR beim Verlassen des
                      ; Tagbodys auf #<DISABLED> gesetzt wird)
  used-far            ; Vektor mit Fill-Pointer, enthält all die Tags, die
                      ; aus einer anderen Funktion heraus mit GO angesprungen
                      ; werden.
)
#+CLISP (remprop 'tagbody 'sys::defstruct-description)

; Sucht nach einem Tag mit dem Namen name und liefert:
; NIL                                         falls nicht gefunden,
; Tagbody-Descriptor, Index                   falls in *genv* gefunden,
; Tagbody-Cons (Tagvektor . status), Index    falls in %genv% gefunden.
(defun genv-search (name &optional (genv *genv*))
  (loop
    (when (atom genv) (return nil))
    (do* ((v (caar genv))
          (l (length v))
          (i 0 (1+ i)))
         ((= i l))
      (when (eql (svref v i) name)
        (return-from genv-search
          (values (if (tagbody-p (cdar genv)) (cdar genv) (car genv)) i)
    ) ) )
    (setq genv (cdr genv))
) )


;       V A R I A B L E N - E N V I R O N M E N T - V E R W A L T U N G

; mitgegeben vom Interpreter: %venv%

; Interpreter-Variablen-Environment hat die Gestalt
; %venv% = NIL oder #(v1 val1 ... vn valn NEXT-ENV), NEXT-ENV von derselben
; Gestalt.
(defconstant specdecl
  #+CLISP (eval
            '(let ((*evalhook*
                     #'(lambda (form env) (declare (ignore form))
                         (svref (svref env 0) 1)
                         ; Der Evalhook-Mechanismus übergibt das Environment.
                         ; (svref...0) davon ist das Variablen-Environment,
                         ; (svref...1) davon ist von der *evalhook*-Bindung
                         ; der assoziierte "Wert" #<SPECIAL REFERENCE>.
                  ))   )
               0
          )  )
  #-CLISP (cons nil nil)
)
; stellt fest, ob das Symbol var eine Special-Variable darstellt
#+CLISP
(defun proclaimed-special-p (var)
  (or (sys::special-variable-p var)
      (not (null (member var *known-special-vars* :test #'eq)))
) )
#-CLISP
(defun proclaimed-special-p (var)
  (or
    (eq var '*evalhook*)
    (eq var '*applyhook*)
    (eq var '*macroexpand-hook*)
    (let ((obj (cons nil nil)))
      (eval
        `(let ((,var ',obj))
           (and (boundp ',var) (eq (symbol-value ',var) ',obj))
    ) )  )
    (not (null (member var *known-special-vars* :test #'eq)))
) )

; neu konstruiert:
(defvar *venv*)                  ; Variablen-Environment, Feinstruktur
(defvar *venvc*)                 ; Variablen-Environment, Grobstruktur

; *venv* hat die Gestalt
; (var1 ... varn . %venv%),
; wo vari Variablen-Konstrukte sind.

; *venvc* simuliert das Laufzeit-Variablen-Environment zur Laufzeit, soweit
; es sich um Closure-Variablen handelt.
; *venvc* hat die Gestalt
; (item1 ... itemn)
; jedes item ist
;   NIL :            ein LET/LET*/MULTIPLE-VALUE-BIND/Funktionseintritt/
;                    FLET/LABELS, der keine Closure aufmacht
;   fnode :          eine neue Funktion
;   ((var1 ... vark) . stackz) : durch ein LET/LET*/MULTIPLE-VALUE-BIND/
;                    Funktionseintritt/FLET/LABELS kommen die Variablen
;                    Var1, ..., Vark in eine Closure.
;                    Diese Closure liegt im Stack; angegeben der
;                    Stackzustand, an der sie erreichbar ist.

; Eine Variable wird beschrieben dadurch, daß sie entweder special ist oder
; - falls lexikalisch - der Stackaufbau nach dem Anlegen der Variablen im Stack
; bzw. der Ort in der Closure festliegt.
(defstruct (var (:copier nil))
  (name nil :read-only t)     ; Symbol
  (specialp nil :read-only t) ; special deklariert (oder lexikalisch gebunden) ?
  constantp                   ; Konstante ?
  constant-value              ; wenn Konstante: Wert der Konstanten
  usedp                       ; falls lexikalisch:
                              ;   wurde die Variable jemals abgefragt ?
                              ;   (Eine durch NIL oder T beendete Liste der
                              ;    Referenzen auf die Variable)
  really-usedp                ; falls lexikalisch:
                              ;   wurde die Variable jemals wirklich
                              ;   (um den Wert zu wissen) abgefragt ?
  (modified-list '())         ; falls lexikalisch: zu jedem SET auf die Variable
                              ;   ein Cons (value-anode . set-anode)
  (replaceable-list '())      ; falls lexikalisch:
                              ;   zu jeder movable-Variablen, die während ihrer
                              ;   gesamten Existenz denselben Wert wie diese
                              ;   hat und deswegen ersetzbar ist, jeweils eine
                              ;   Liste (var init-anode . bind-anode)
  closurep                    ; falls lexikalisch:
                              ;   NIL falls im Stack, T falls in der Closure
  (stackz nil :read-only t)   ; falls lexikalisch:
                              ;   Stackzustand nach dem Anlegen der Variablen
                              ;   (falls Variable im Stack: ihr Ort im Stack)
  (venvc nil :read-only t)    ; falls lexikalisch und in der Closure:
                              ;   das *venvc*, in dessen erstem Item diese
                              ;   Variable vorkommt.
)
#+CLISP (remprop 'var 'sys::defstruct-description)

; (venv-search v) sucht in *venv* nach einer Variablen mit dem Symbol v.
; Ergebnis ist:
; NIL                   falls nicht gefunden
; SPECIAL               falls als Special-deklarierte Variable gefunden
; LOCAL, vector, index  falls interpretativ lexikalisch gebunden, Wert im Vektor
; T, var                falls lexikalisch gebunden, im Stack oder in der Closure
(defun venv-search (v &optional (venv *venv*))
  (when (or (constantp v) (proclaimed-special-p v))
    (return-from venv-search 'SPECIAL)
  )
  (loop
    (when (atom venv) (return))
    (when (eq (var-name (car venv)) v)
      (return-from venv-search
        (if (var-specialp (car venv)) 'SPECIAL (values T (car venv)))
    ) )
    (setq venv (cdr venv))
  )
  (loop
    (when (null venv) (return-from venv-search 'NIL))
    (unless (simple-vector-p venv) (compiler-error 'venv-search))
    (do ((l (1- (length venv)))
         (i 0 (+ i 2)))
        ((= i l) (setq venv (svref venv i)))
      (if (eq v (svref venv i))
        (let ((val (svref venv (1+ i))))
          (return-from venv-search
            (if (eq val specdecl) 'SPECIAL (values 'LOCAL venv (1+ i)))
  ) ) ) ) )
  'NIL
)

; (zugriff-in-closure var venvc stackz)
; liefert zu einer Closure-Variablen var, wie man auf sie zugreifen kann
; (von einem Ort aus, an der Stack und das Closure-Environment durch stackz und
;  venvc beschrieben werden):
; 3 Werte k, n, m; die Variable sitzt in (svref ... 1+m) von
;     nil, n, m  : (STACK+4*n)
;     k, nil, m  : (svref ... 0)^k VenvConst
;     k, n,   m  : ((SP+4*k)+4*n)
(defun zugriff-in-closure (var venvc stackz &aux (k nil) n)
  ; Grobschleife, stellt die Closure-Tiefe k ab VenvConst fest:
  (loop
    (when (eq venvc (var-venvc var)) (return))
    (let ((item (car venvc)))
      (if (null k)
        (when (fnode-p item) (setq k 0)) ; Zählanfang
        (when (consp item) (incf k)) ; zählen
    ) )
    (setq venvc (cdr venvc))
  )
  (if k
    (setq n nil)
    (multiple-value-setq (k n) (zugriff-in-stack stackz (cdr (first venvc))))
  )
  (let ((m (do ((L (car (first venvc)) (cdr L))
                (i 0 (1+ i)))
               ((eq (car L) var) i)
       ))  )
    (values k n m)
) )


;             K O N S T A N T E N - V E R W A L T U N G

; Eine Konstante ist eine Box mit dem Wert der Konstanten:
(defstruct (const (:copier nil))
  value               ; Wert der Konstanten
  (form nil)          ; falls /= NIL: Symbol, das konstant ist,
    ; bzw. allgemeiner: Form, die bei Auswertung value ergibt.
)
#+CLISP (remprop 'const 'sys::defstruct-description)
; Im 2. Pass werden auch Variablen mit constantp=T als Konstanten behandelt.


;           D E K L A R A T I O N E N - V E R W A L T U N G

(defparameter *declaration-types*
  '(special ; Bindungen
    type ftype function ; Typen
    inline notinline ; Funktionen-Compilation
    ignore optimize ; Compiler-Hinweise
    declaration ; Zusatzdeklarationen
    ; Typen nach Tabelle 4-1 :
    array atom bignum bit bit-vector character common compiled-function
    complex cons double-float fixnum float function hash-table integer keyword
    list long-float nil null number package pathname random-state ratio rational
    readtable sequence short-float simple-array simple-bit-vector simple-string
    simple-vector single-float standard-char stream string string-char symbol t
    vector
    ; zusätzliche Deklarationen:
    compile ; Anweisung, daß die Form bzw. Funktion zu compilieren ist
    sys::source ; der Source-Lambdabody (unexpandiert) innerhalb eines Lambdabody
)  )

; mitgegeben vom Interpreter: %denv%

; neu konstruiert:
(defvar *denv*)
; *denv* hat dieselbe Gestalt wie %denv% und endet mit %denv%.
; *denv* hat die Gestalt (item1 ... itemn), wo jedes item die Bauart
; (declaration-type argument ...) hat.
; Sonderbehandlung von
;   SPECIAL : wird weggelassen, stattdessen in *venv* notiert.
;   IGNORE : wird weggelassen, stattdessen bei der verarbeitenden Form selber
;            verarbeitet.
; Zusätzliche Deklaration (INLINING symbol) gegen rekursives Inlining.

; (process-declarations declspeclist) pusht die Deklarationen (wie sie von
; PARSE-BODY kommen) auf *denv* und liefert:
; eine Liste der Special-deklarierten Symbole,
; eine Liste der Ignore-deklarierten Symbole.
(defun process-declarations (declspeclist &aux (specials nil) (ignores nil))
  (setq declspeclist (nreverse declspeclist))
  (dolist (declspec declspeclist)
    (if (or (atom declspec) (cdr (last declspec)))
      (c-warn #+DEUTSCH "Falsche Deklarationen-Syntax: ~S~%Wird ignoriert."
              #+ENGLISH "Bad declaration syntax: ~S~%Will be ignored."
              declspec
      )
      (let ((declspectype (car declspec)))
        (if (and (symbolp declspectype)
                 (or (member declspectype *declaration-types* :test #'eq)
                     (do ((L *denv* (cdr L)))
                         ((null L) nil)
                       (if (and (eq (first (car L)) 'DECLARATION)
                                (member declspectype (rest (car L)) :test #'eq)
                           )
                         (return t)
                     ) )
                     (and *compiling-from-file*
                       (member declspectype *user-declaration-types* :test #'eq)
            )    )   )
          (cond ((eq declspectype 'SPECIAL)
                 (dolist (x (cdr declspec))
                   (if (symbolp x)
                     (push x specials)
                     (c-warn #+DEUTSCH "Nur Symbole können SPECIAL-deklariert werden, nicht ~S."
                             #+ENGLISH "Non-symbol ~S may not be declared SPECIAL."
                             x
                )) ) )
                ((eq declspectype 'IGNORE)
                 (dolist (x (cdr declspec))
                   (if (symbolp x)
                     (push x ignores)
                     (c-warn #+DEUTSCH "Nur Symbole können IGNORE-deklariert werden, nicht ~S."
                             #+ENGLISH "Non-symbol ~S may not be declared IGNORE."
                             x
                )) ) )
                (t (push declspec *denv*))
          )
          (c-warn #+DEUTSCH "Unbekannte Deklaration ~S.~%Die ganze Deklaration ~S wird ignoriert."
                  #+ENGLISH "Unknown declaration ~S.~%The whole declaration will be ignored."
                  declspectype declspec
  ) ) ) ) )
  (values specials ignores)
)

; (declared-notinline fun denv) stellt fest, ob fun - ein Symbol, das eine
; globale Funktion, die nicht durch eine lokale Funktionsdefinition verdeckt
; ist, benennt - in denv als NOTINLINE deklariert ist.
; Was ist mit lokalen Funktionen ??
(defun declared-notinline (fun &optional (denv *denv*))
  (when (member `(INLINING ,fun) *denv* :test #'equal)
    (return-from declared-notinline t) ; keine Funktion rekursiv inline expandieren!
  )
  (loop
    (when (atom denv)
      (when *compiling-from-file*
        (when (member fun *notinline-functions* :test #'eq) (return t))
        (when (member fun *inline-functions* :test #'eq) (return nil))
      )
      (return (eq (get fun 'inlinable) 'notinline))
    )
    (let ((declspec (car denv)))
      (when (and (eq (car declspec) 'INLINE) (member fun (cdr declspec) :test #'eq))
        (return nil)
      )
      (when (and (eq (car declspec) 'NOTINLINE) (member fun (cdr declspec) :test #'eq))
        (return t)
    ) )
    (setq denv (cdr denv))
) )


;             F U N K T I O N E N - V E R W A L T U N G

; Ein FNODE enthält die nötige Information für eine Funktion:
(defstruct (fnode (:copier nil))
  name            ; Name, ein Symbol
  code            ; Code dieser Funktion (zuerst nichts, dann ein ANODE,
                  ; dann eine Closure)
  ; Ab hier Beschreibungen für die kommende Closure:
  venvconst       ; Flag, ob das Venv dieser Funktion explizit beim Aufbau
                  ; mitgegeben werden muß (oder immer NIL ist)
  venvc           ; Aussehen des Venv, das dieser Funktion beim Aufbau
                  ; mitgegeben werden muß (wenn überhaupt)
  Blocks-Offset   ; Anzahl der Konstanten bis hierher
  (Blocks nil)    ; Liste der Block-Konstrukte, die dieser Funktion beim Aufbau
                  ; mitgegeben werden müssen
  Tagbodys-Offset ; Anzahl der Konstanten bis hierher
  (Tagbodys nil)  ; Liste der Tagbody-Konstrukte, die dieser Funktion beim
                  ; Aufbau mitgegeben werden müssen
  Keyword-Offset  ; Anzahl der lokalen Konstanten bis hierher
                  ; = Anfangsoffset der Keywords in FUNC
                  ; (also =0 genau dann, wenn die Funktion autonom ist)
  (req-anz 0)     ; Anzahl der required parameter
  (opt-anz 0)     ; Anzahl der optionalen Parameter
  (rest-flag nil) ; Flag, ob &REST - Parameter angegeben.
  (keyword-flag nil) ; Flag, ob &KEY - Parameter angegeben.
  (keywords nil)  ; Liste der Keyword-Konstanten (in der richtigen Reihenfolge)
  allow-other-keys-flag ; &ALLOW-OTHER-KEYS-Flag
  Consts-Offset   ; Anzahl der lokalen Konstanten bis hierher
  (consts nil)    ; Liste der sonstigen Konstanten dieser Funktion
                  ; Diese Liste wird erst im 2. Pass aufgebaut.
  (consts-forms nil) ; Liste der evtl. Formen, die diese Konstanten ergeben
  enclosing       ; lexikalisch nächste darüberliegende Funktion (oder NIL)
)
#+CLISP (remprop 'fnode 'sys::defstruct-description)

; die aktuelle Funktion, ein FNODE:
(defvar *func*)
; das Label am Beginn des Codes der aktuellen Funktion:
(defvar *func-start-label*)

; Anzahl der bisher in der aktuellen Funktion aufgetretenen anonymen
; Funktionen (Lambda-Ausdrücke):
(defvar *anonymous-count*)

; *no-code* = T besagt, daß kein Code produziert werden soll:
(defvar *no-code*)
; Dies verhindert, daß Variablen unnötigerweise in die Closure gesteckt oder
; Optimierungen unnötigerweise unterlassen werden.


;                 F O R M E N - V E R W A L T U N G

; Bei jeder Rekursion werden folgende Variablen dynamisch gebunden:
(defvar *form*)      ; die aktuelle Form
(defvar *for-value*) ; ob und welche Werte der Form von Belang sind:
                     ; NIL : Werte sind irrelevant
                     ; ONE : nur der erste Wert ist relevant
                     ; ALL : alle Werte sind relevant

; Ein ANODE ist die Codierung der Information, die beim Compilieren einer Form
; gebraucht wird.
(defstruct (anode
            (:constructor mk-anode (#+COMPILER-DEBUG source
                                    type
                                    #+COMPILER-DEBUG sub-anodes
                                    seclass
                                    code
                                    #+COMPILER-DEBUG stackz
            )                      )
            (:copier nil)
           )
  #+COMPILER-DEBUG
  source              ; die zu dieser Form gehörige Source, meist eine Form
                      ; (nur zu Debugzwecken erforderlich)
  type                ; Typ des ANODE (CALL, PRIMOP, VAR, LET, SETQ, ...)
  #+COMPILER-DEBUG
  sub-anodes          ; alle ANODEs der Unterformen
  seclass             ; Seiteneffekt-Klassifikation
  code                ; erzeuger LAP-Code, eine Liste aus LAP-Anweisungen
                      ; und ANODEs
  #+COMPILER-DEBUG
  stackz              ; Zustand der Stacks beim Eintritt in den zugehörigen
                      ; LAP-Code
)
#+CLISP (remprop 'anode 'sys::defstruct-description)
; (make-anode ...) ist dasselbe wie mk-anode, nur daß dabei die Argumente
; mit Keywords markiert werden und wegen #+COMPILER-DEBUG unnötige
; Komponenten trotzdem dastehen dürfen.
(eval-when (compile eval)
  (defmacro make-anode (&key
                        (source `*form*)
                        type
                        (sub-anodes `'())
                        seclass
                        code
                        (stackz `*stackz*)
                       )
    `(mk-anode #+COMPILER-DEBUG ,source
               ,type
               #+COMPILER-DEBUG ,sub-anodes
               ,seclass
               ,code
               #+COMPILER-DEBUG ,stackz
     )
) )

#|
; Eine Seiteneffekt-Klasse (SECLASS) ist ein Indikator:
; NIL : dieses ANODE produziert keine Seiteneffekte,
;       sein Wert ist nicht von Seiteneffekten beeinflußbar.
; VAL : dieses ANODE produziert keine Seiteneffekte,
;       sein Wert ist aber von Seiteneffekten beeinflußbar.
; T   : dieses ANODE kann Seiteneffekte produzieren.
; Somit:
;   Falls der Wert uninteressant ist, kann ein ANODE mit SECLASS = NIL/VAL
;   weggelassen werden.
;   In der Reihenfolge der Auswertung dürfen vertauscht werden ANODEs mit
;   SECLASS     NIL-NIL, NIL-VAL, NIL-T, VAL-VAL.

; (seclass-or class1 ... classk) bestimmt die Gesamtklasse der Ausführung
; aller Klassen.
(defun seclass-or (&rest args)
  (cond ((member 'T args :test #'eq) 'T)
        ((member 'VAL args :test #'eq) 'VAL)
        (t 'NIL)
) )
; Dito, mit nur 2 Argumenten
(defun seclass-or-2 (seclass1 seclass2)
  (or (eq seclass1 'T) seclass2 seclass1)
)
; Damit die Liste der sub-anodes nicht gebildet werden muß, aber dennoch
; der zu dieser Liste gehörige Seiteneffektklasse berechnet werden kann:
(eval-when (compile eval)
  (defmacro anodes-seclass-or (&rest anodeforms)
    (reduce #'(lambda (form1 form2) `(SECLASS-OR-2 ,form1 ,form2))
            (mapcar #'(lambda (anodeform) `(ANODE-SECLASS ,anodeform))
                    anodeforms
  ) )       )
  (define-modify-macro seclass-or-f (anode) seclass-or-anode)
  (defmacro seclass-or-anode (seclass anode)
    `(SECLASS-OR-2 ,seclass (ANODE-SECLASS ,anode))
  )
)
(defun anodelist-seclass-or (anodelist)
  (apply #'seclass-or (mapcar #'anode-seclass anodelist))
)

; Stellt fest, ob zwei Anodes in der Reihenfolge ihrer Auswertung vertauscht
; werden können - vorausgesetzt, die Stackzustände lassen das zu.
(defun anodes-commute (anode1 anode2)
  (let ((seclass1 (anode-seclass anode1))
        (seclass2 (anode-seclass anode2)))
    (or (eq seclass1 'NIL) (eq seclass2 'NIL)
        (and (eq seclass1 'VAL) (eq seclass2 'VAL))
) ) )
|#

; Eine Seiteneffekt-Klasse (SECLASS) ist ein Indikator (uses . modifies):
; uses = NIL : dieses Anode ist nicht von Seiteneffekten beeinflußbar,
;        Liste : dieses Anode ist vom Wert der Variablen in der Liste abhängig,
;        T : dieses Anode ist möglicherweise von jedem Seiteneffekt beeinflußbar.
; modifies = NIL : dieses Anode produziert keine Seiteneffekte
;            Liste : ... produziert Seiteneffekte nur auf die Werte der
;                    Variablen in der Liste
;            T : ... produziert Seiteneffekte unbekannten Ausmaßes.
; (Variablen sind hier VAR-Structures für lexikalische und Symbole für
; dynamische Variablen.)
; Somit:
;   Falls der Wert uninteressant ist, kann ein ANODE mit SECLASS-modifies=NIL
;   weggelassen werden.
;   In der Reihenfolge der Auswertung dürfen vertauscht werden ANODEs mit
;   SECLASS, deren uses- und modifies-Teil über Kreuz disjunkt sind.

; (seclass-or class1 ... classk) bestimmt die Gesamtklasse der Ausführung
; aller Klassen.
(defun seclass-or (&rest args)
  (if (null args) '(NIL . NIL) (reduce #'seclass-or-2 args))
)
; Dito, mit nur 2 Argumenten
(defun seclass-or-2 (seclass1 seclass2)
  (cons (if (or (eq (car seclass1) 'T) (eq (car seclass2) 'T))
          'T
          (union (car seclass1) (car seclass2))
        )
        (if (or (eq (cdr seclass1) 'T) (eq (cdr seclass2) 'T))
          'T
          (union (cdr seclass1) (cdr seclass2))
) )     )

; Damit die Liste der sub-anodes nicht gebildet werden muß, aber dennoch
; der zu dieser Liste gehörige Seiteneffektklasse berechnet werden kann:
(eval-when (compile eval)
  (defmacro anodes-seclass-or (&rest anodeforms)
    (reduce #'(lambda (form1 form2) `(SECLASS-OR-2 ,form1 ,form2))
            (mapcar #'(lambda (anodeform) `(ANODE-SECLASS ,anodeform))
                    anodeforms
  ) )       )
  (define-modify-macro seclass-or-f (anode) seclass-or-anode)
  (defmacro seclass-or-anode (seclass anode)
    `(SECLASS-OR-2 ,seclass (ANODE-SECLASS ,anode))
  )
)
(defun anodelist-seclass-or (anodelist)
  (apply #'seclass-or (mapcar #'anode-seclass anodelist))
)

; Seiteneffekte auf weiter innen gebundene lexikalische Variablen zählen
; nicht und werden deshalb eliminiert:
(defun seclass-without (seclass varlist)
  (flet ((bound (var) (member var varlist))) ; testet, ob var gebunden wird
    ; (Dynamische Variablen werden nicht eliminiert; sie sind in varlist
    ; als VAR-Structures und in seclass als Symbole enthalten.)
    (cons (if (eq (car seclass) 'T) 'T (remove-if #'bound (car seclass)))
          (if (eq (cdr seclass) 'T) 'T (remove-if #'bound (cdr seclass)))
) ) )

; Stellt fest, ob zwei Anodes in der Reihenfolge ihrer Auswertung vertauscht
; werden können - vorausgesetzt, die Stackzustände lassen das zu.
(defun anodes-commute (anode1 anode2)
  (seclasses-commute (anode-seclass anode1) (anode-seclass anode2))
)
(defun seclasses-commute (seclass1 seclass2)
  (flet ((disjoint-p (uses modifies)
           (or (null uses) (null modifies)
               (and (not (eq uses 'T)) (not (eq modifies 'T))
                    (null (intersection uses modifies))
        )) )   )
    (and (disjoint-p (car seclass1) (cdr seclass2))
         (disjoint-p (car seclass2) (cdr seclass1))
) ) )


;            H I L F S F U N K T I O N E N

; Liefert ein Symbol, das sich aus der Package und dem Printname eines
; gegebenen Symbols, einem Bindestrich und einem Suffix zusammensetzt.
(defun symbol-suffix (symbol suffix)
  (let ((pack (symbol-package symbol)))
    (if (symbolp suffix)
      (if pack
        (setq suffix (symbol-name suffix))
        (return-from symbol-suffix suffix)
      )
      (unless (stringp suffix)
        (setq suffix (write-to-string suffix :escape nil :base 10 :radix nil))
    ) )
    (let ((new-name (concatenate 'string (symbol-name symbol) "-" suffix)))
      (if pack (intern new-name pack) (make-symbol new-name))
) ) )

; (C-COMMENT controlstring . args)
; gibt eine Zusatzinformation des Compilers aus (mittels FORMAT).
(defun c-comment (cstring &rest args)
  (let ((dest (if *compile-verbose* *c-error-output* *c-listing-output*)))
    (when dest (apply #'format dest cstring args))
) )

(defvar *warning-count*)
; (C-WARN controlstring . args)
; gibt eine Compiler-Warnung aus (mittels FORMAT).
(defun c-warn (cstring &rest args)
  (setq cstring
    (concatenate 'string #+DEUTSCH "~%WARNUNG~@[ in Funktion ~S~] :~%"
                         #+ENGLISH "~%WARNING~@[ in function ~S~] :~%"
                         cstring
  ) )
  (incf *warning-count*)
  (let ((dest (if *compile-warnings* *c-error-output* *c-listing-output*)))
    (when dest
      (apply #'format dest cstring
             (and (boundp '*func*) (fnode-p *func*) (fnode-name *func*))
             args
) ) ) )

(defvar *error-count*)
; (C-ERROR controlstring . args)
; gibt einen Compiler-Error aus (mittels FORMAT) und beendet das laufende C-FORM.
(defun c-error (cstring &rest args)
  (setq cstring
    (concatenate 'string #+DEUTSCH "~%ERROR~@[ in Funktion ~S~] :~%"
                         #+ENGLISH "~%ERROR~@[ in function ~S~] :~%"
                         cstring
  ) )
  (incf *error-count*)
  (let ((in-function
          (and (boundp '*func*) (fnode-p *func*) (fnode-name *func*))
       ))
    (when in-function
      (when *compiling-from-file* (pushnew in-function *functions-with-errors*))
    )
    (apply #'format *c-error-output* cstring in-function args)
  )
  (throw 'c-error
    (make-anode :source NIL
                :type 'ERROR
                :sub-anodes '()
                :seclass '(NIL . NIL)
                :code '((NIL))
) ) )

; (c-eval-when-compile form) führt eine Form zur Compile-Zeit aus.
(defun c-eval-when-compile (form)
  (when (and *compiling-from-file* *liboutput-stream*)
    ; Form auf den Liboutput-Stream schreiben:
    (terpri *liboutput-stream*)
    (write form :stream *liboutput-stream* :pretty t
                :closure t :circle t :array t :gensym t
                :escape t :level nil :length nil :radix t
  ) )
  ; Form evaluieren:
  (eval form)
)

; (c-constantp form) stellt fest, ob form im Compiler als Konstante gehandhabt
; wird.
(defun c-constantp (form)
  (if (atom form)
    (or (numberp form) (characterp form) (stringp form) (bit-vector-p form)
        (and (symbolp form)
             (cond ((keywordp form) t)
                   ((eq (symbol-package form) *lisp-package*)
                    (constantp form)
                   )
                   (t (not (null (assoc form *constant-special-vars*))))
    )   )    )
    (and (eq (first form) 'QUOTE) (consp (cdr form)) (null (cddr form)))
) )

; (c-constant-value form) liefert den Wert einer Konstanten
(defun c-constant-value (form)
  (if (atom form)
    (cond ((numberp form) form)
          ((characterp form) form)
          ((stringp form) form)
          ((bit-vector-p form) form)
          ((symbolp form)
           (cond ((keywordp form) form)
                 ((eq (symbol-package form) *lisp-package*)
                  (symbol-value form)
                 )
                 (t (cdr (assoc form *constant-special-vars*)))
    )     ))
    (second form)
) )

; (anode-constantp anode) stellt fest, ob der Anode einen konstanten Wert
; liefert.
(defun anode-constantp (anode)
  ; Anode liefert konstanten Wert jedenfalls dann, wenn sein Code
  ; (nach TRAVERSE-ANODE) genau aus ((CONST ...)) bestehen würde.
  (let ((code (anode-code anode)))
    (and (consp code) (null (cdr code)) ; Liste der Länge 1
         (let ((item (car code)))
            (cond ((consp item) (eq (first item) 'CONST))
                  ((anode-p item) (anode-constantp item))
) ) )    )  )

; (anode-constant-value anode) liefert den Wert eines konstanten Anode.
(defun anode-constant-value (anode)
  (let ((item (car (anode-code anode))))
    (cond ((consp item) (const-value (second item)))
          (t #|(anode-p item)|# (anode-constant-value item))
) ) )

; (new-const value) liefert eine Konstante in *func* mit dem Wert value
; im 1. Pass
(defun new-const (value)
  (make-const :value value)
)

; (make-label for-value) liefert ein neues Label. for-value (NIL/ONE/ALL)
; gibt an, welche der Werte nach dem Label gebraucht werden.
(defun make-label (for-value)
  (let ((label (gensym)))
    (setf (symbol-value label) '()) ; Referenzliste für 2. Pass := leer
    (setf (get label 'for-value) for-value)
    label
) )

; liefert eine Special-Variable
(defun make-special-var (symbol)
  (make-var :name symbol :specialp t
            :constantp (c-constantp symbol)
            :constant-value (if (c-constantp symbol) (c-constant-value symbol))
) )


;                     E R S T E R   P A S S

; (test-list L) stellt fest, ob L eine echte Liste ist, die mit NIL endet
; und mindestens l1, höchstens aber l2 Elemente hat. Sonst Error.
(defun test-list (L &optional (l1 0) (l2 nil))
  (unless (and (listp L) (null (cdr (last L))))
    (c-error #+DEUTSCH "Dotted list im Code: ~S"
             #+ENGLISH "Code contains dotted list ~S"
             L
  ) )
  (unless (>= (length L) l1)
    (c-error #+DEUTSCH "Form zu kurz (zu wenig Argumente): ~S"
             #+ENGLISH "Form too short, too few arguments: ~S"
             L
  ) )
  (when l2
    (unless (<= (length L) l2)
      (c-error #+DEUTSCH "Form zu lang (zu viele Argumente): ~S"
               #+ENGLISH "Form too long, too many arguments: ~S"
               L
  ) ) )
)

; c-form-table enthält zu allen Funktionen/Specialforms/Macros, die speziell
; behandelt werden müssen, die Behandlungsfunktion (ohne Argumente aufzurufen).
(defconstant c-form-table
  (let ((hashtable (make-hash-table :test #'eq)))
    (mapc
      #'(lambda (acons) (setf (gethash (car acons) hashtable) (cdr acons)))
      `(; Special forms:
          (QUOTE . c-QUOTE)
          (PROGN . c-PROGN)
          (LET . ,#'(lambda () (c-LET/LET* nil)))
          (LET* . ,#'(lambda () (c-LET/LET* t)))
          (IF . c-IF)
          (SETQ . c-SETQ)
          (BLOCK . c-BLOCK)
          (RETURN-FROM . c-RETURN-FROM)
          (TAGBODY . c-TAGBODY)
          (GO . c-GO)
          (FUNCTION . c-FUNCTION)
          (MULTIPLE-VALUE-BIND . c-MULTIPLE-VALUE-BIND)
          (MULTIPLE-VALUE-SETQ . c-MULTIPLE-VALUE-SETQ)
          (AND . c-AND)
          (OR . c-OR)
          (WHEN . c-WHEN)
          (UNLESS . c-UNLESS)
          (COND . c-COND)
          (PSETQ . c-PSETQ)
          (MULTIPLE-VALUE-CALL . c-MULTIPLE-VALUE-CALL)
          (PROG1 . c-PROG1)
          (PROG2 . c-PROG2)
          (THE . c-THE)
          (CATCH . c-CATCH)
          (THROW . c-THROW)
          (UNWIND-PROTECT . c-UNWIND-PROTECT)
          (PROGV . c-PROGV)
          (MULTIPLE-VALUE-LIST . c-MULTIPLE-VALUE-LIST)
          (MULTIPLE-VALUE-PROG1 . c-MULTIPLE-VALUE-PROG1)
          (FLET . c-FLET)
          (LABELS . c-LABELS)
          (MACROLET . c-MACROLET)
          (COMPILER-LET . c-COMPILER-LET)
          (EVAL-WHEN . c-EVAL-WHEN)
          (DECLARE . c-DECLARE)
        ; Macros:
          (CASE . c-CASE)
        ; Inline-compilierte Funktionen:
          (FUNCALL . c-FUNCALL)
          (SYS::%FUNCALL . c-FUNCALL)
          (APPLY . c-APPLY)
          (+ . c-PLUS)
          (- . c-MINUS)
          (SYS::SVSTORE . c-SVSTORE)
          (EQ . c-EQ)
          (EQL . c-EQL)
          (EQUAL . c-EQUAL)
          (MAPCAR . c-MAPCAR)
          (MAPLIST . c-MAPLIST)
          (MAPC . c-MAPC)
          (MAPL . c-MAPL)
          (MAPCAN . c-MAPCAN)
          (MAPCON . c-MAPCON)
          (MAPCAP . c-MAPCAP)
          (MAPLAP . c-MAPLAP)
          (TYPEP . c-TYPEP)
    )  )
    hashtable
) )
; Diese Tabelle muß alle Special-Forms enthalten:
(do-all-symbols (sym)
  (when (and (special-form-p sym) (not (gethash sym c-form-table)))
    (compiler-error 'c-form-table)
) )

; compiliert eine Form.
; Dabei ergibt sich kein Code, falls keine Werte gebraucht werden und die Form
; keine Seiteneffekte produziert.
(defun c-form (*form* &optional (*for-value* *for-value*))
 (let
  ((anode
    (catch 'c-error
      (if (atom *form*)
        (cond ((symbolp *form*) (c-var *form*))
              ((or (numberp *form*) (characterp *form*) (stringp *form*)
                   (bit-vector-p *form*)
               )
               (c-CONST)
              )
              (t (c-error #+DEUTSCH "Das ist keine gültige Form: ~S"
                          #+ENGLISH "Invalid form: ~S"
                          *form*
        )     )  )
        (let ((fun (first *form*)))
          (if (symbolp fun)
            (multiple-value-bind (a b c) (fenv-search fun)
              (declare (ignore b))
              (if (null a)
                ; nicht lokal definiert
                (let ((handler (gethash fun c-form-table)))
                  (if handler ; Behandlungsfunktion gefunden?
                    (funcall handler) ; ja -> aufrufen
                    ; nein -> jedenfalls keine Special-Form (die sind ja
                    ; alle in der Tabelle).
                    (if (macro-function fun) ; globaler Macro ?
                      (c-form (macroexpand-1 *form* *fenv*)) ; -> expandieren
                      ; globale Funktion
                      (c-GLOBAL-FUNCTION-CALL fun)
                ) ) )
                (case a
                  (SYSTEM::MACRO ; lokaler Macro
                    (c-form (macroexpand-1 *form* *fenv*)) ; -> expandieren
                  )
                  (GLOBAL ; Funktion im Interpreter-Environment %fenv% gefunden
                    ; (c-form `(SYS::%FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
                    (c-FUNCALL-NOTINLINE `(FUNCTION ,fun) (cdr *form*))
                  )
                  (LOCAL ; lokale Funktion (in *fenv* gefunden)
                    ; (c-form `(SYS::%FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
                    (c-LOCAL-FUNCTION-CALL fun c (cdr *form*))
                  )
                  (t (compiler-error 'c-form))
            ) ) )
            (if (and (consp fun) (eq (car fun) 'LAMBDA))
              (c-form `(SYS::%FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
              #| nicht: (c-LAMBDA-FUNCTION-CALL fun (cdr *form*)) |#
              (c-error #+DEUTSCH "Das ist nicht der Name einer Funktion: ~S"
                       #+ENGLISH "Not the name of a function: ~S"
                       fun
    ) ) ) ) ) )
  ))
  #+COMPILER-DEBUG (setf (anode-source anode) *form*)
  ; Falls keine Werte gebraucht werden und keine Seiteneffekte produziert
  ; werden, kann der dazugehörige Code ganz gestrichen werden:
  (when (and (null *for-value*) (null (cdr (anode-seclass anode))))
    (setf (anode-code anode) '())
    (setf (anode-seclass anode) '(NIL . NIL))
  )
  anode
))

; compiliere NIL (eine Art Notausgang)
(defun c-NIL ()
  (make-anode :type 'NIL
              :sub-anodes '()
              :seclass '(NIL . NIL)
              :code '((NIL)) )
)

; Konstante als Form:
(defun c-CONST ()
  (make-anode :type 'const
              :sub-anodes '()
              :seclass '(NIL . NIL)
              :code `((CONST ,(new-const *form*)))
) )

; Variable als Form:
(defun c-VAR (symbol)
  ; Suche die Variable in *venv* :
  (multiple-value-bind (a b c) (venv-search symbol)
    (when (eq a 'NIL)
      (c-warn #+DEUTSCH "~S ist weder deklariert noch gebunden,~@
                         behandle es als SPECIAL-deklarierte Variable."
              #+ENGLISH "~S is neither declared nor bound,~@
                         it will be treated as if it were declared SPECIAL."
              symbol
      )
      (when *compiling-from-file*
        (pushnew symbol *unknown-free-vars* :test #'eq)
      )
      (setq a 'SPECIAL)
    )
    (case a
      (SPECIAL ; Special-Variable
        (let ((var (make-special-var symbol)))
          (make-anode
            :type 'VAR
            :sub-anodes '()
            :seclass (cons
                       (if (and *for-value* (not (var-constantp var))) (list symbol) 'NIL)
                       'NIL
                     )
            :code (if *for-value*
                    (if (var-constantp var)
                      `((CONST ,(make-const
                                  :value (c-constant-value symbol)
                                  :form (if (keywordp symbol) nil symbol)
                       ))       )
                      `((GETVALUE ,symbol))
                    )
                    '()
      ) ) )       )
      (LOCAL ; interpretativ lexikalisch
        (make-anode
          :type 'VAR
          :sub-anodes '()
          :seclass (cons (if *for-value* 'T 'NIL) 'NIL)
          :code (if *for-value*
                  `((CONST ,(new-const b)) ; Vektor
                    (PUSH)
                    (CONST ,(new-const c)) ; Index
                    (SVREF)
                   )
                  '()
      ) )       )
      ((T) ; lexikalisch in Stack oder Closure
        (let* ((var b)
               (get-anode
                 (make-anode
                   :type 'VAR
                   :sub-anodes '()
                   :seclass (cons (if *for-value* (list var) 'NIL) 'NIL)
                   :code (if *for-value*
                           `((GET ,var ,*venvc* ,*stackz*))
                           '()
              )) )       )
          (push get-anode (var-usedp var))
          (when (and *for-value* (not *no-code*))
            (setf (var-really-usedp var) t)
            (unless (eq (stackz-fun (var-stackz var)) *func*)
              (setf (var-closurep var) t)
            )
            (when (var-closurep var)
              ; aktiviere Venvconst in allen dazwischenliegenden Funktionen
              (do ((venvc *venvc* (cdr venvc)))
                  ((null venvc) (compiler-error 'c-VAR "INVISIBLE"))
                (when (eq venvc (var-venvc var)) (return))
                (when (fnode-p (car venvc))
                  (setf (fnode-Venvconst (car venvc)) t)
          ) ) ) )
          get-anode
      ) )
      (t (compiler-error 'c-VAR 'venv-search))
) ) )

; Variablenzuweisung:
(defun c-VARSET (symbol value-anode)
  ; Suche die Variable in *venv* :
  (multiple-value-bind (a b c) (venv-search symbol)
    (when (eq a 'NIL)
      (c-warn #+DEUTSCH "~S ist weder deklariert noch gebunden,~@
                         behandle es als SPECIAL-deklarierte Variable."
              #+ENGLISH "~S is neither declared nor bound,~@
                         it will be treated as if it were declared SPECIAL."
              symbol
      )
      (setq a 'SPECIAL)
    )
    (case a
      (SPECIAL ; Special-Variable
        (let ((var (make-special-var symbol)))
          (make-anode :type 'VARSET
                      :sub-anodes '()
                      :seclass (cons
                                 'NIL
                                 (if (var-constantp var) 'NIL (list symbol))
                               )
                      :code (if (var-constantp var)
                              (progn
                                (c-warn #+DEUTSCH "Der Konstante ~S kann nicht zugewiesen werden.~@
                                                   Die Zuweisung wird ignoriert."
                                        #+ENGLISH "The constant ~S may not be assigned to.~@
                                                   The assignment will be ignored."
                                        symbol
                                )
                                '((VALUES1))
                              )
                              `((SETVALUE , symbol))
      ) ) )                 )
      (LOCAL ; interpretativ lexikalisch
        (make-anode :type 'VARSET
                    :sub-anodes '()
                    :seclass (cons 'NIL 'T)
                    :code `((PUSH)
                            (CONST ,(new-const b)) ; Vektor
                            (PUSH)
                            (CONST ,(new-const c)) ; Index
                            (SVSET)
      ) )                  )
      ((T) ; lexikalisch in Stack oder Closure
        (let* ((var b)
               (set-anode
                 (make-anode :type 'VARSET
                             :sub-anodes '()
                             :seclass (cons 'NIL (list var))
                             :code `((SET ,var ,*venvc* ,*stackz*))
              )) )
          (unless (var-usedp var) (setf (var-usedp var) t)) ; Zuweisung "benutzt" die Variable
          (unless *no-code*
            (setf (var-constantp var) nil) ; nicht mehr konstant wegen Zuweisung
            (push (cons value-anode set-anode) (var-modified-list var))
            (unless (eq (stackz-fun (var-stackz var)) *func*)
              (setf (var-closurep var) t)
              ; aktiviere Venvconst in allen dazwischenliegenden Funktionen
              (do ((venvc *venvc* (cdr venvc)))
                  ((null venvc) (compiler-error 'c-VARSET "INVISIBLE"))
                (when (eq venvc (var-venvc var)) (return))
                (when (fnode-p (car venvc))
                  (setf (fnode-Venvconst (car venvc)) t)
            ) ) )
            ; Das Ersetzen einer Variablen innervar durch var ist dann
            ; nicht erlaubt, wenn während der Existenzdauer von innervar
            ; an var ein Wert zugewiesen wird.
            (setf (var-replaceable-list var)
              (delete-if #'(lambda (innervar-info) ; innervar gerade aktiv?
                             (let ((innervar (first innervar-info)))
                               (mytailp (var-stackz innervar) *stackz*)
                           ) )
                         (var-replaceable-list var)
            ) )
          )
          set-anode
      ) )
      (t (compiler-error 'c-VARSET 'venv-search))
) ) )

;; Funktionsaufrufe, bei denen die Funktion ein Symbol ist:

; Global function call, normal (notinline): (fun {form}*)
(defun c-NORMAL-FUNCTION-CALL (fun) ; fun ist ein Symbol
  (test-list *form* 1)
  (let* ((n (length (cdr *form*)))
         #+COMPILER-DEBUG (oldstackz *stackz*)
         (*stackz* *stackz*))
    (do ((formlist (cdr *form*))
         #+COMPILER-DEBUG (anodelist '())
         (codelist (list '(CALLP))))
        ((null formlist)
         (push
           `(,@(case n
                 (0 `(CALL0)) (1 `(CALL1)) (2 `(CALL2)) (t `(CALL ,n))
               )
             ,(new-const fun)
            )
           codelist
         )
         (make-anode
           :type 'CALL
           :sub-anodes (nreverse anodelist)
           :seclass '(T . T)
           :code (nreverse codelist)
           :stackz oldstackz
        ))
      (let* ((formi (pop formlist))
             (anodei (c-form formi 'ONE)))
        #+COMPILER-DEBUG (push anodei anodelist)
        (push anodei codelist)
        (push '(PUSH) codelist)
        (push 1 *stackz*)
) ) ) )

; (test-argument-syntax args fun req opt rest-p key-p keylist allow-p)
; überprüft, ob die Argumentliste args als Argumentliste zu fun (Symbol)
; geeignet ist, d.h. ob sie der gegebenen Spezifikation, gegeben durch
; req,opt,rest-p,keylist,allow-p, genügt.
; Gegebenenfalls wird eine Warnung ausgegeben.
; Liefert:
;   NO-KEYS           bei korrekter Syntax, ohne Keywords,
;   STATIC-KEYS       bei korrekter Syntax mit konstanten Keywords,
;   DYNAMIC-KEYS      bei (vermutlich) korrekter Syntax,
;                       mit nicht-konstanten Keywords.
;   NIL               bei fehlerhafter Syntax,
; In den ersten beiden Fällen ist
; req <= (length args) <= (req+opt oder, falls rest-p oder key-p, unendlich).
(defun test-argument-syntax (args fun req opt rest-p key-p keylist allow-p)
  (unless (and (listp args) (null (cdr (last args))))
    (c-error #+DEUTSCH "Argumentliste zu Funktion ~S ist dotted: ~S"
             #+ENGLISH "argument list to function ~S is dotted: ~S"
             fun args
  ) )
  (let ((n (length args))
        (reqopt (+ req opt)))
    (unless (and (<= req n) (or rest-p key-p (<= n reqopt)))
      (c-warn #+DEUTSCH "~S mit ~S Argumenten aufgerufen, braucht aber ~
                         ~:[mindestens ~*~S~;~:[~S bis ~S~;~S~]~] Argumente."
              #+ENGLISH "~S called with ~S arguments, but it requires ~
                         ~:[at least ~*~S~;~:[from ~S to ~S~;~S~]~] arguments."
              fun n
              (or rest-p key-p)  (eql req reqopt) req reqopt
      )
      (return-from test-argument-syntax 'NIL)
    )
    (unless key-p (return-from test-argument-syntax 'NO-KEYS))
    ; Mit Keywords.
    (when (<= n reqopt) (return-from test-argument-syntax 'STATIC-KEYS))
    (when rest-p (return-from test-argument-syntax 'DYNAMIC-KEYS))
    (setq n (- n reqopt) args (nthcdr reqopt args))
    (unless (evenp n)
      (c-warn #+DEUTSCH "Keyword-Argumente zu Funktion ~S sind nicht paarig: ~S"
              #+ENGLISH "keyword arguments to function ~S should occur pairwise: ~S"
              fun args
      )
      (return-from test-argument-syntax 'NIL)
    )
    (do ((keyargs args (cddr keyargs))
         (allow-flag allow-p)
         (wrong-key nil)
        )
        ((null keyargs)
         (if wrong-key
           (c-error #+DEUTSCH "Keyword ~S ist bei Funktion ~S nicht erlaubt.~
                               ~%Erlaubt ~:[sind nur ~{~S~#[~; und ~S~:;, ~]~}~;ist nur ~{~S~}~]."
                    #+ENGLISH "keyword ~S is not allowed for function ~S.~
                               ~%The only allowed keyword~:[s are ~{~S~#[~; and ~S~:;, ~]~}~; is ~{~S~}~]."
                    wrong-key fun (eql (length keylist) 1) keylist
           )
           'STATIC-KEYS
        ))
      (let ((key (first keyargs)))
        (unless (c-constantp key)
          (return-from test-argument-syntax 'DYNAMIC-KEYS)
        )
        (setq key (c-constant-value key))
        (unless (keywordp key)
          (c-warn #+DEUTSCH "Das Argument ~S zu Funktion ~S ist kein Keyword."
                  #+ENGLISH "argument ~S to function ~S is not a keyword"
                  (first keyargs) fun
          )
          (return-from test-argument-syntax 'DYNAMIC-KEYS)
        )
        (when (eq key ':ALLOW-OTHER-KEYS)
          (unless (c-constantp (second keyargs))
            (return-from test-argument-syntax 'DYNAMIC-KEYS)
          )
          (when (c-constant-value (second keyargs)) (setq allow-flag t))
        )
        (unless (or allow-flag (member key keylist :test #'eq))
          (setq wrong-key key)
    ) ) )
) )

; (c-DIRECT-FUNCTION-CALL args fun req opt rest-p key-p keylist
;                         subr-flag call-code-producer)
; compiliert die Abarbeitung der Argumente für den Direktaufruf einer
; Funktion (d.h. ohne Argument-Check zur Laufzeit).
; (test-argument-syntax ...) muß die Argumente bereits erfolgreich (d.h.
; mit Ergebnis NO-KEYS oder STATIC-KEYS) überprüft haben.
; args : Liste der Argumentformen,
; fun : Name der aufzurufenden Funktion (Symbol),
; req,opt,rest-p,key-p,keylist,allow-p : Information über die Lambdaliste von fun
; subr-flag : Flag, ob fun ein SUBR oder aber eine compilierte Closure ist,
; call-code-producer : Funktion, die den Code liefert, der am Ende anzufügen
;                      ist und den Aufruf ausführt.
(defun c-DIRECT-FUNCTION-CALL (args fun req opt rest-p key-p keylist
                               subr-flag call-code-producer)
  (let* ((foldable nil)
         (sideeffects ; Seiteneffektklasse des Funktionsaufrufs selbst
           (if (not subr-flag)
             '(T . T) ; kein SUBR -> kann nichts aussagen
             (case fun ; fun ein SUBR
               (; Seiteneffektklasse (NIL . NIL) haben diejenigen Funktionen,
                ; die ihre Argumente nur anschauen (Pointer, Inhalt nur bei
                ; Zahlen oder ähnlichen unmodifizierbaren Datenstrukturen)
                ; und auf keine globalen Variablen zugreifen.
                ; Eine Funktion, die, zweimal mit denselben Argumenten auf-
                ; gerufen, stets dasselbe Ergebnis liefert (im EQL-Sinne),
                ; erlaubt Constant-Folding: Sind alle Argumente Konstanten
                ; und der Funktionsaufruf durchführbar, so darf der Funktions-
                ; aufruf durch das konstante Funktionsergebnis ersetzt werden.
                (SYSTEM::%FUNTABREF
                 ARRAY-ELEMENT-TYPE ARRAY-RANK ADJUSTABLE-ARRAY-P
                 STANDARD-CHAR-P GRAPHIC-CHAR-P STRING-CHAR-P ALPHA-CHAR-P UPPER-CASE-P
                 LOWER-CASE-P BOTH-CASE-P DIGIT-CHAR-P ALPHANUMERICP CHAR= CHAR/= CHAR< CHAR>
                 CHAR<= CHAR>= CHAR-EQUAL CHAR-NOT-EQUAL CHAR-LESSP CHAR-GREATERP
                 CHAR-NOT-GREATERP CHAR-NOT-LESSP CHAR-CODE CHAR-BITS CHAR-FONT CODE-CHAR
                 MAKE-CHAR CHAR-UPCASE CHAR-DOWNCASE DIGIT-CHAR CHAR-INT INT-CHAR
                 CHAR-NAME CHAR-BIT
                 SPECIAL-FORM-P
                 ENDP
                 IDENTITY
                 EQ EQL CONSP ATOM SYMBOLP STRINGP NUMBERP
                 NULL NOT SYSTEM::CLOSUREP LISTP INTEGERP SYSTEM::FIXNUMP RATIONALP FLOATP
                 SYSTEM::SHORT-FLOAT-P SYSTEM::SINGLE-FLOAT-P SYSTEM::DOUBLE-FLOAT-P SYSTEM::LONG-FLOAT-P
                 COMPLEXP STREAMP RANDOM-STATE-P READTABLEP HASH-TABLE-P PATHNAMEP CHARACTERP
                 PACKAGEP ARRAYP SIMPLE-ARRAY-P BIT-VECTOR-P VECTORP SIMPLE-VECTOR-P
                 SIMPLE-STRING-P SIMPLE-BIT-VECTOR-P
                 ZEROP PLUSP MINUSP ODDP EVENP = /= < > <= >= MAX MIN
                 + - * / 1+ 1- CONJUGATE GCD LCM ISQRT
                 RATIONAL RATIONALIZE NUMERATOR DENOMINATOR FLOOR CEILING TRUNCATE
                 ROUND MOD REM DECODE-FLOAT SCALE-FLOAT
                 FLOAT-RADIX FLOAT-SIGN FLOAT-DIGITS FLOAT-PRECISION INTEGER-DECODE-FLOAT
                 COMPLEX REALPART IMAGPART LOGIOR LOGXOR LOGAND LOGEQV LOGNAND LOGNOR
                 LOGANDC1 LOGANDC2 LOGORC1 LOGORC2 BOOLE LOGNOT LOGTEST LOGBITP ASH LOGCOUNT
                 INTEGER-LENGTH LDB LDB-TEST MASK-FIELD DPB DEPOSIT-FIELD ! EXQUO
                ) ; alle diese sind SUBRs ohne Keyword-Parameter
                (setq foldable t)
                '(NIL . NIL)
               )
               ((VECTOR MAKE-STRING
                 VALUES ; nicht foldable, um Endlosschleife zu verhindern!
                 CONS LIST LIST* MAKE-LIST ACONS
                 LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION SOFTWARE-TYPE
                 SOFTWARE-VERSION
                 SYSTEM::MAKE-LOAD-TIME-EVAL
                 SYMBOL-NAME
                 SYSTEM::DECIMAL-STRING
                )
                '(NIL . NIL)
               )
               ((SYSTEM::SUBR-INFO
                 AREF SVREF ARRAY-DIMENSION ARRAY-DIMENSIONS ARRAY-TOTAL-SIZE
                 ARRAY-IN-BOUNDS-P ARRAY-ROW-MAJOR-INDEX BIT SBIT
                 ARRAY-HAS-FILL-POINTER-P FILL-POINTER MAKE-ARRAY
                 CHARACTER CHAR SCHAR STRING= STRING/= STRING< STRING> STRING<=
                 STRING>= STRING-EQUAL STRING-NOT-EQUAL STRING-LESSP STRING-GREATERP
                 STRING-NOT-GREATERP STRING-NOT-LESSP SYSTEM::SEARCH-STRING=
                 SYSTEM::SEARCH-STRING-EQUAL SYSTEM::STRING-BOTH-TRIM STRING-UPCASE
                 STRING-DOWNCASE STRING-CAPITALIZE STRING NAME-CHAR SUBSTRING STRING-CONCAT
                 MAKE-SYMBOL SYMBOL-VALUE SYMBOL-FUNCTION BOUNDP FBOUNDP
                 VALUES-LIST MACRO-FUNCTION CONSTANTP
                 MAKE-HASH-TABLE GETHASH HASH-TABLE-COUNT SYSTEM::HASH-TABLE-ITERATOR SXHASH
                 GET-MACRO-CHARACTER GET-DISPATCH-MACRO-CHARACTER SYSTEM::LINE-POSITION
                 CAR CDR CAAR CADR CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR CDDAR CDDDR
                 CAAAAR CAAADR CAADAR CAADDR CADAAR CADADR CADDAR CADDDR
                 CDAAAR CDAADR CDADAR CDADDR CDDAAR CDDADR CDDDAR CDDDDR
                 LIST-LENGTH NTH FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH
                 EIGHTH NINTH TENTH REST NTHCDR LAST APPEND COPY-LIST
                 COPY-ALIST COPY-TREE REVAPPEND BUTLAST LDIFF TAILP PAIRLIS
                 GET-UNIVERSAL-TIME GET-INTERNAL-RUN-TIME
                 GET-INTERNAL-REAL-TIME SYSTEM::%%TIME
                 FIND-PACKAGE PACKAGE-NAME PACKAGE-NICKNAMES PACKAGE-USE-LIST
                 PACKAGE-USED-BY-LIST PACKAGE-SHADOWING-SYMBOLS LIST-ALL-PACKAGES FIND-SYMBOL
                 FIND-ALL-SYMBOLS
                 PARSE-NAMESTRING PATHNAME PATHNAME-HOST PATHNAME-DEVICE PATHNAME-DIRECTORY
                 PATHNAME-NAME PATHNAME-TYPE PATHNAME-VERSION FILE-NAMESTRING
                 DIRECTORY-NAMESTRING HOST-NAMESTRING MERGE-PATHNAMES ENOUGH-NAMESTRING
                 MAKE-PATHNAME NAMESTRING TRUENAME PROBE-FILE DIRECTORY FILE-WRITE-DATE
                 FILE-AUTHOR
                 EQUAL EQUALP COMPILED-FUNCTION-P FUNCTIONP COMMONP TYPE-OF COERCE
                 SYSTEM::%RECORD-REF SYSTEM::%RECORD-LENGTH SYSTEM::%STRUCTURE-REF SYSTEM::%MAKE-STRUCTURE
                 SYSTEM::%COPY-STRUCTURE SYSTEM::%STRUCTURE-TYPE-P SYSTEM::CLOSURE-NAME
                 SYSTEM::CLOSURE-CODEVEC SYSTEM::CLOSURE-CONSTS SYSTEM::MAKE-CODE-VECTOR
                 SYSTEM::%MAKE-CLOSURE
                 SYSTEM::SEQUENCEP ELT SUBSEQ COPY-SEQ LENGTH REVERSE CONCATENATE
                 MAKE-SYNONYM-STREAM MAKE-BROADCAST-STREAM MAKE-CONCATENATED-STREAM
                 MAKE-TWO-WAY-STREAM MAKE-ECHO-STREAM MAKE-STRING-INPUT-STREAM
                 SYSTEM::STRING-INPUT-STREAM-INDEX MAKE-STRING-OUTPUT-STREAM
                 SYSTEM::MAKE-STRING-PUSH-STREAM MAKE-BUFFERED-INPUT-STREAM
                 MAKE-BUFFERED-OUTPUT-STREAM INPUT-STREAM-P OUTPUT-STREAM-P
                 STREAM-ELEMENT-TYPE FILE-LENGTH
                 GET GETF GET-PROPERTIES SYMBOL-PACKAGE SYMBOL-PLIST KEYWORDP
                 SYSTEM::SPECIAL-VARIABLE-P GENSYM
                 FFLOOR FCEILING FTRUNCATE FROUND
                 EXP EXPT LOG SQRT ABS PHASE SIGNUM SIN COS TAN CIS ASIN ACOS ATAN
                 SINH COSH TANH ASINH ACOSH ATANH FLOAT BYTE BYTE-SIZE BYTE-POSITION
                 SYSTEM::LOG2 SYSTEM::LOG10
                )
                '(T . NIL)
               )
               (t '(T . T)) ; vielleicht Seiteneffekte
        )) ) )
    (if (and (null *for-value*) (null (cdr sideeffects)))
      ; Brauche die Funktion nicht aufzurufen, nur die Argumente auswerten
      (progn
        (let ((*no-code* t) (*for-value* 'NIL))
          (funcall call-code-producer)
        )
        (c-form `(PROGN ,@args))
      )
      (let ((n (length args))
            (reqopt (+ req opt))
            (seclass sideeffects)
            (codelist '()))
        (let ((*stackz* *stackz*))
          ; required und angegebene optionale Parameter:
          (dotimes (i (min n reqopt))
            (let* ((formi (pop args))
                   (anodei (c-form formi 'ONE)))
              (seclass-or-f seclass anodei)
              (push anodei codelist)
            )
            (push '(PUSH) codelist)
            (push 1 *stackz*)
          )
          ; fehlende optionale Parameter werden mit #<UNBOUND> initialisiert:
          (when (> reqopt n)
            (let ((anz (- reqopt n)))
              (push `(PUSH-UNBOUND ,anz) codelist)
              (push anz *stackz*)
          ) )
          ; &rest-Parameter:
          (when rest-p
            (if subr-flag
              ; Übergabe von restlichen Argumenten an ein SUBR: einzeln
              (loop
                (when (null args) (return))
                (let ((anode (c-form (pop args) 'ONE)))
                  (seclass-or-f seclass anode)
                  (push anode codelist)
                )
                (push '(PUSH) codelist)
                (push 1 *stackz*)
              )
              ; Übergabe von restlichen Argumenten an eine compilierte Closure:
              ; als Liste
              (if (null args)
                ; leere Liste
                (progn
                  (push '(NIL) codelist)
                  (push '(PUSH) codelist)
                  (push 1 *stackz*)
                )
                ; Liste aus allen weiteren Argumenten:
                (progn
                  (let ((*stackz* *stackz*)
                        (rest-args args))
                    (loop
                      (when (null rest-args) (return))
                      (let ((anode (c-form (pop rest-args) 'ONE)))
                        (seclass-or-f seclass anode)
                        (push anode codelist)
                      )
                      (push '(PUSH) codelist)
                      (push 1 *stackz*)
                    )
                    (push `(LIST ,(- n reqopt)) codelist)
                  )
                  (push '(PUSH) codelist)
                  (push 1 *stackz*)
          ) ) ) )
          ; &key-Parameter:
          (when key-p
            ; Nur dann gleichzeitig rest-p und key-p, wenn n <= reqopt, da
            ; test-argument-syntax (ergab STATIC-KEYS) den anderen Fall
            ; bereits ausgeschlossen hat.
            (let ((keyanz (length keylist)))
              ; Erst alle Keys mit #<UNBOUND> vorbelegen, dann die Argumente
              ; in der angegebenen Reihenfolge auswerten und zuordnen?
              ; Das ist uns zu einfach. Wir lassen die Argumente kommutieren,
              ; damit möglichst viele der (STORE ...) durch (PUSH) ersetzt
              ; werden können: Die Argumente zu den ersten Keys werden nach
              ; Möglichkeit zuerst ausgewertet, die zu den letzten Keys
              ; zuletzt. Wir lassen es allerdings bei einem einzigen
              ; (PUSH-UNBOUND ...).
              (let* ((key-positions ; Liste von Tripeln (key stack-depth free-p),
                                    ; wobei stack-depth = keyanz-1...0 läuft und
                                    ; free-p angibt, ob der Slot schon gefüllt ist.
                       (let ((i keyanz))
                         (mapcar #'(lambda (key) (list key (decf i) t)) keylist)
                     ) )
                     (anodes ; Liste von Quadrupeln
                             ; (needed key-position anode stackz), wobei
                             ; key-position die stack-depth des Keyword-Slots
                             ; oder NIL ist, anode der Anode zu diesem Argument.
                             ; Die Liste wird in derselben Reihenfolge gehalten,
                             ; wie sie die Argumentliste vorgibt.
                             ; Ausnahme: needed = NIL bei anodes, deren
                             ; Berechnung man vorgezogen oder verschoben hat.
                       (let ((L '()))
                         (loop
                           (when (null args) (return))
                           (let* ((key (c-constant-value (pop args)))
                                  (tripel (assoc key key-positions :test #'eq)) ; kann =NIL sein!
                                  (for-value (third tripel))
                                  (arg (pop args)))
                             ; for-value /= NIL: Existentes Keyword, und der Slot ist noch leer
                             ; for-value = NIL: ALLOW-erlaubtes Keyword oder Slot schon gefüllt
                             (let* ((*stackz* (cons 0 *stackz*)) ; 0 wird später ersetzt
                                    (anode (c-form arg (if for-value 'ONE 'NIL))))
                               (seclass-or-f seclass anode)
                               (push (list t (second tripel) anode *stackz*) L)
                             )
                             (setf (third tripel) nil)
                         ) )
                         (nreverse L)
                    )) )
                (let ((depth1 0)
                      (depth2 0)
                      (codelist-from-end '()))
                  ; Möglichst viel nach vorne ziehen:
                  (do ((anodesr anodes (cdr anodesr)))
                      ((null anodesr))
                    (let ((anodeetc (car anodesr))) ; nächstes Quadrupel
                      (when (first anodeetc) ; noch was zu tun?
                        (if (and
                              (or ; kein Keyword, d.h. kein (STORE ...) nötig?
                                  (null (second anodeetc))
                                  ; oberstes Keyword?
                                  (= (second anodeetc) (- keyanz depth1 1))
                              )
                              ; kommutiert anodeetc mit allen vorigen anodes?
                              (let ((anode (third anodeetc)))
                                (do ((anodesr2 anodes (cdr anodesr2)))
                                    ((eq anodesr2 anodesr) t)
                                  (unless (anodes-commute anode (third (car anodesr2)))
                                    (return nil)
                              ) ) )
                            )
                          ; vorziehen:
                          (progn
                            (setf (first (fourth anodeetc)) depth1) ; korrekte Stacktiefe
                            (push (third anodeetc) codelist) ; in die Codeliste
                            (when (second anodeetc)
                              (push '(PUSH) codelist)
                              (incf depth1)
                            )
                            (setf (first anodeetc) nil) ; diesen brauchen wir nicht mehr
                          )
                          ; sonst machen wir nichts.
                  ) ) ) )
                  ; Möglichst viel nach hinten ziehen:
                  (setq anodes (nreverse anodes))
                  (do ((anodesr anodes (cdr anodesr)))
                      ((null anodesr))
                    (let ((anodeetc (car anodesr))) ; nächstes Quadrupel
                      (when (first anodeetc) ; noch was zu tun?
                        (if (and
                              (or ; kein Keyword, d.h. kein (STORE ...) nötig?
                                  (null (second anodeetc))
                                  ; unterstes Keyword?
                                  (= (second anodeetc) depth2)
                              )
                              ; kommutiert anodeetc mit allen späteren anodes?
                              (let ((anode (third anodeetc)))
                                (do ((anodesr2 anodes (cdr anodesr2)))
                                    ((eq anodesr2 anodesr) t)
                                  (unless (anodes-commute anode (third (car anodesr2)))
                                    (return nil)
                              ) ) )
                            )
                          ; ans Ende verschieben:
                          (progn
                            (when (second anodeetc)
                              (push '(PUSH) codelist-from-end)
                              (incf depth2)
                            )
                            (setf (first (fourth anodeetc)) (- keyanz depth2)) ; korrekte Stacktiefe
                            (push (third anodeetc) codelist-from-end) ; in die Codeliste
                            (setf (first anodeetc) nil) ; diesen brauchen wir nicht mehr
                          )
                          ; sonst machen wir nichts.
                  ) ) ) )
                  (setq anodes (nreverse anodes))
                  (let ((depth-now (- keyanz depth2))) ; codelist-from-end erniedrigt den Stack um depth2
                    (when (> depth-now depth1)
                      (push `(PUSH-UNBOUND ,(- depth-now depth1)) codelist)
                    )
                    ; In codelist herrscht jetzt Stacktiefe depth-now.
                    (dolist (anodeetc anodes)
                      (when (first anodeetc)
                        (setf (first (fourth anodeetc)) depth-now) ; korrekte Stacktiefe
                        (push (third anodeetc) codelist)
                        (when (second anodeetc)
                          (push `(STORE ,(- (second anodeetc) depth2)) codelist)
                  ) ) ) )
                  ; Nun codelist-from-end:
                  (setq codelist (nreconc codelist-from-end codelist))
              ) )
              ; Jetzt sind alle Key-Argumente auf dem Stack.
              (push keyanz *stackz*)
          ) )
          (setq codelist (nreconc codelist (funcall call-code-producer)))
        )
        ; Constant-Folding: Ist fun foldable (also subr-flag = T und
        ; key-flag = NIL) und besteht codelist außer den (PUSH)s und dem
        ; Call-Code am Schluß nur aus Anodes mit code = ((CONST ...)) ?
        (when (and foldable
                   (every #'(lambda (code)
                              (or (not (anode-p code)) (anode-constantp code))
                            )
                          codelist
              )    )
          ; Funktion aufzurufen versuchen:
          (let ((args (let ((L '())) ; Liste der (konstanten) Argumente
                        (dolist (code codelist)
                          (when (anode-p code)
                            (push (anode-constant-value code) L)
                        ) )
                        (nreverse L)
                )     )
                resulting-values)
            (when (block try-eval
                    (setq resulting-values
                      (let ((*error-handler*
                              #'(lambda (&rest error-args)
                                  (declare (ignore error-args))
                                  (return-from try-eval nil)
                           ))   )
                        (multiple-value-list (apply fun args))
                    ) )
                    t
                  )
              ; Funktion erfolgreich aufgerufen, Constant-Folding durchführen:
              (return-from c-DIRECT-FUNCTION-CALL
                (c-GLOBAL-FUNCTION-CALL-form
                  `(VALUES ,@(mapcar #'(lambda (x) `(QUOTE ,x)) resulting-values))
        ) ) ) ) )
        (make-anode
          :type `(DIRECT-CALL ,fun)
          :sub-anodes (remove-if-not #'anode-p codelist)
          :seclass seclass
          :code codelist
        )
) ) ) )

; Global function call: (fun {form}*)
(defun c-GLOBAL-FUNCTION-CALL-form (*form*)
  (c-GLOBAL-FUNCTION-CALL (first *form*))
)
(defun c-GLOBAL-FUNCTION-CALL (fun) ; fun ist ein Symbol
  (test-list *form* 1)
  (when *compiling-from-file* ; von COMPILE-FILE aufgerufen?
    (unless (or (fboundp fun) (member fun *known-functions* :test #'eq))
      (pushnew fun *unknown-functions* :test #'eq)
    )
    ; PROCLAIM-Deklarationen zur Kenntnis nehmen:
    (when (and (eq fun 'PROCLAIM) (= (length *form*) 2))
      (let ((h (second *form*)))
        (when (c-constantp h)
          (c-form
            `(EVAL-WHEN (COMPILE) (c-PROCLAIM ',(c-constant-value h)))
    ) ) ) )
    ; Modul-Anforderungen zur Kenntnis nehmen:
    (when (and (memq fun '(PROVIDE REQUIRE))
               (every #'c-constantp (rest *form*))
          )
      (c-form
        `(EVAL-WHEN (COMPILE)
           (,(case fun
               (PROVIDE 'c-PROVIDE) ; c-PROVIDE statt PROVIDE
               (REQUIRE 'c-REQUIRE) ; c-REQUIRE statt REQUIRE
             )
            ,@(mapcar
                #'(lambda (x) (list 'QUOTE (c-constant-value x))) ; Argumente quotieren
                (rest *form*)
         ) )  )
    ) )
    ; Package-Anforderungen zur Kenntnis nehmen:
    (when (and (memq fun '(MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
                           EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT
               )          )
               (every #'c-constantp (rest *form*))
          )
      (push
        `(,fun
          ,@(mapcar
              #'(lambda (x) (list 'QUOTE (c-constant-value x))) ; Argumente quotieren
              (rest *form*)
         )  )
        *package-tasks*
  ) ) )
  (let* ((args (cdr *form*)) ; Argumente
         (n (length args))) ; Anzahl der Argumente
    (if (not (declared-notinline fun)) ; darf fun INLINE genommen werden?
      (multiple-value-bind (name req opt rest-p keylist allow-p) (subr-info fun)
        ; Ist fun ein SUBR, so sollte name = fun sein, und das SUBR hat die
        ; Spezifikation req, opt, rest-p, key-p = (not (null keylist)), allow-p.
        ; Sonst ist name = NIL.
        (if (and name (eq fun name)) ; beschreibt fun ein gültiges SUBR?
          (case fun
            ((CAR CDR FIRST REST NOT NULL CONS SVREF VALUES
              CAAR CADR CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR
              CDDAR CDDDR SECOND THIRD FOURTH CAAAAR CAAADR CAADAR CAADDR
              CADAAR CADADR CADDAR CADDDR CDAAAR CDAADR CDADAR CDADDR
              CDDAAR CDDADR CDDDAR CDDDDR ATOM CONSP
              VALUES-LIST SYS::%SVSTORE EQ SYMBOL-FUNCTION LIST ERROR
             )
             ; Diese hier haben keylist=NIL, allow-p=NIL und
             ; (was aber nicht verwendet wird) opt=0.
             (if (and (<= req n) (or rest-p (<= n (+ req opt))))
               ; Wir machen den Aufruf INLINE.
               (let ((sideeffects ; Seiteneffektklasse der Funktionsausführung
                       (case fun
                         ((NOT NULL CONS VALUES ATOM CONSP EQ LIST)
                           '(NIL . NIL)
                         )
                         ((CAR CDR FIRST REST CAAR CADR
                           CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR CDDAR
                           CDDDR SECOND THIRD FOURTH CAAAAR CAAADR CAADAR CAADDR
                           CADAAR CADADR CADDAR CADDDR CDAAAR CDAADR CDADAR CDADDR
                           CDDAAR CDDADR CDDDAR CDDDDR VALUES-LIST
                           SVREF SYMBOL-FUNCTION
                          )
                           '(T . NIL)
                         )
                         (t '(T . T))
                    )) )
                 (if (and (null *for-value*) (null (cdr sideeffects)))
                   ; Brauche die Funktion nicht aufzurufen, nur die Argumente auswerten
                   (c-form `(PROGN ,@args))
                   (if (and (eq fun 'VALUES) (eq *for-value* 'ONE))
                     (if (= n 0) (c-NIL) (c-form `(PROG1 ,@args)))
                     (let ((seclass sideeffects)
                           (codelist '()))
                       (let ((*stackz* *stackz*))
                         ; Argumente auswerten und bis auf das letzte auf den Stack
                         ; (denn das letzte Argument wird in A0 erwartet):
                         (loop
                           (when (null args) (return))
                           (let ((anode (c-form (pop args) 'ONE)))
                             (seclass-or-f seclass anode)
                             (push anode codelist)
                           )
                           (when args ; nicht am Schluß
                             (push '(PUSH) codelist)
                             (push 1 *stackz*)
                         ) )
                         (setq codelist
                           (nreconc codelist
                             (case fun
                               ((CAR FIRST) '((CAR)))
                               ((CDR REST) '((CDR)))
                               (CAAR '((CAR) (CAR)))
                               ((CADR SECOND) '((CDR) (CAR)))
                               (CDAR '((CAR) (CDR)))
                               (CDDR '((CDR) (CDR)))
                               (CAAAR '((CAR) (CAR) (CAR)))
                               (CAADR '((CDR) (CAR) (CAR)))
                               (CADAR '((CAR) (CDR) (CAR)))
                               ((CADDR THIRD) '((CDR) (CDR) (CAR)))
                               (CDAAR '((CAR) (CAR) (CDR)))
                               (CDADR '((CDR) (CAR) (CDR)))
                               (CDDAR '((CAR) (CDR) (CDR)))
                               (CDDDR '((CDR) (CDR) (CDR)))
                               (CAAAAR '((CAR) (CAR) (CAR) (CAR)))
                               (CAAADR '((CDR) (CAR) (CAR) (CAR)))
                               (CAADAR '((CAR) (CDR) (CAR) (CAR)))
                               (CAADDR '((CDR) (CDR) (CAR) (CAR)))
                               (CADAAR '((CAR) (CAR) (CDR) (CAR)))
                               (CADADR '((CDR) (CAR) (CDR) (CAR)))
                               (CADDAR '((CAR) (CDR) (CDR) (CAR)))
                               ((CADDDR FOURTH) '((CDR) (CDR) (CDR) (CAR)))
                               (CDAAAR '((CAR) (CAR) (CAR) (CDR)))
                               (CDAADR '((CDR) (CAR) (CAR) (CDR)))
                               (CDADAR '((CAR) (CDR) (CAR) (CDR)))
                               (CDADDR '((CDR) (CDR) (CAR) (CDR)))
                               (CDDAAR '((CAR) (CAR) (CDR) (CDR)))
                               (CDDADR '((CDR) (CAR) (CDR) (CDR)))
                               (CDDDAR '((CAR) (CDR) (CDR) (CDR)))
                               (CDDDDR '((CDR) (CDR) (CDR) (CDR)))
                               (ATOM '((ATOM)))
                               (CONSP '((CONSP)))
                               ((NOT NULL) '((NOT)))
                               (CONS '((CONS)))
                               (SVREF '((SVREF)))
                               (SYS::%SVSTORE '((SVSET)))
                               (EQ '((EQ)))
                               (VALUES (case n
                                         (0 '((VALUES0)) )
                                         (1 '((VALUES1)) )
                                         (t `((PUSH) ; letztes Argument auch noch in den Stack
                                              (STACK-TO-MV ,n)
                                             )
                               )       ) )
                               (VALUES-LIST '((LIST-TO-MV)))
                               (SYMBOL-FUNCTION '((SYMBOL-FUNCTION)))
                               (LIST (if (plusp n)
                                       `((PUSH) (LIST ,n))
                                       '((NIL))
                               )     )
                               (ERROR `((PUSH) (ERROR ,(1- n))))
                               (t (compiler-error 'c-GLOBAL-FUNCTION-CALL))
                       ) ) ) )
                       (make-anode
                         :type `(PRIMOP ,fun)
                         :sub-anodes (remove-if-not #'anode-p codelist)
                         :seclass seclass
                         :code codelist
                       )
               ) ) ) )
               ; falsche Argumentezahl -> doch nicht INLINE:
               (progn
                 (c-warn #+DEUTSCH "~S mit ~S Argumenten aufgerufen, braucht aber ~
                                    ~:[mindestens ~*~S~;~:[~S bis ~S~;~S~]~] Argumente."
                         #+ENGLISH "~S called with ~S arguments, but it requires ~
                                    ~:[at least ~*~S~;~:[from ~S to ~S~;~S~]~] arguments."
                         fun n
                         rest-p  (eql opt 0) req (+ req opt)
                 )
                 (c-NORMAL-FUNCTION-CALL fun)
            )) )
            (t ; Ist das SUBR fun in der FUNTAB enthalten?
             (let ((index (gethash fun function-codes)))
               (if index
                 (case (test-argument-syntax args
                                    fun req opt rest-p keylist keylist allow-p
                       )
                   ((NO-KEYS STATIC-KEYS)
                    ; korrekte Syntax, Stack-Layout zur Compilezeit vorhersehbar
                    ; -> INLINE
                    (c-DIRECT-FUNCTION-CALL
                      args fun req opt rest-p keylist keylist
                      t ; es handelt sich um ein SUBR
                      (let ((call-code
                              ; Aufruf mit Hilfe der FUNTAB:
                              (if (not rest-p)
                                (list (CALLS-code index))
                                `((CALLSR ,(max 0 (- n req opt)) ; Bei n<req+opt kommt noch ein (PUSH-UNBOUND ...)
                                          ,(- index funtabR-index)
                                 ))
                           )) )
                        #'(lambda () call-code)
                   )) )
                   (t (c-NORMAL-FUNCTION-CALL fun))
                 )
                 (c-NORMAL-FUNCTION-CALL fun)
          ) )) )
          (let ((inline-lambdabody
                  (and *compiling-from-file*
                       (cdr (assoc fun *inline-definitions* :test #'eq))
               )) )
            (if (and inline-lambdabody
                     (inline-callable-function-p `(FUNCTION (LAMBDA ,@inline-lambdabody)) n)
                )
              ; Aufruf einer globalen Funktion INLINE möglich
              (c-FUNCALL-INLINE fun args inline-lambdabody nil)
              (c-NORMAL-FUNCTION-CALL fun)
      ) ) ) )
      (c-NORMAL-FUNCTION-CALL fun)
) ) )

; Hilfsfunktion: PROCLAIM beim Compilieren vom File, vgl. Funktion PROCLAIM
(defun c-PROCLAIM (declspec)
  (when (consp declspec)
    (case (car declspec)
      (SPECIAL
        (dolist (var (cdr declspec))
          (when (symbolp var) (pushnew var *known-special-vars* :test #'eq))
      ) )
      (INLINE
        (dolist (var (cdr declspec))
          (when (symbolp var)
            (pushnew var *inline-functions* :test #'eq)
            (setq *notinline-functions*  (delete var *notinline-functions* :test #'eq))
      ) ) )
      (NOTINLINE
        (dolist (var (cdr declspec))
          (when (symbolp var)
            (pushnew var *notinline-functions* :test #'eq)
            (setq *inline-functions* (delete var *inline-functions* :test #'eq))
      ) ) )
      (DECLARATION
        (dolist (var (cdr declspec))
          (when (symbolp var) (pushnew var *user-declaration-types* :test #'eq))
      ) )
) ) )

; Hilfsfunktion: DEFCONSTANT beim Compilieren
(defun c-PROCLAIM-CONSTANT (symbol initial-value-form)
  (when *compiling-from-file*
    (pushnew symbol *known-special-vars* :test #'eq)
    (when (c-constantp initial-value-form)
      (push (cons symbol (c-constant-value initial-value-form))
            *constant-special-vars*
) ) ) )

; Hilfsfunktion: DEFUN beim Compilieren
(defun c-DEFUN (symbol &optional lambdabody)
  (when *compiling* ; c-DEFUN kann auch vom Expander aus aufgerufen werden!
    (when *compiling-from-file*
      (pushnew symbol *known-functions* :test #'eq)
      (when lambdabody ; Lambdabody angegeben ->
        ; Funktionsdefinition erfolgt im Top-Level-Environment und ist inlinebar.
        (push (cons symbol lambdabody) *inline-definitions*)
) ) ) )

; Hilfsfunktion: PROVIDE beim Compilieren vom File, vgl. Funktion PROVIDE
(defun c-PROVIDE (module-name)
  (pushnew (string module-name) *compiled-modules* :test #'string=)
)

; Hilfsfunktion: REQUIRE beim Compilieren vom File, vgl. Funktion REQUIRE
(defun c-REQUIRE (module-name &optional (pathname nil p-given))
  (unless (member (string module-name) *compiled-modules* :test #'string-equal)
    (unless p-given (setq pathname (pathname module-name)))
    (flet ((load-lib (file)
             (let* ((present-files (search-file file '(#".lsp" #".lib")))
                    (newest-file (first present-files)))
               ; Maximal 2 Files gefunden. Falls das libfile unter diesen
               ; vorkommt und das neueste ist:
               (if (and (consp present-files)
                        (string= (pathname-type newest-file)
                                 '#,(pathname-type '#".lib")
                   )    )
                 (load newest-file :verbose nil :print nil :echo nil) ; libfile laden
                 (compile-file (or newest-file file)) ; file compilieren
          )) ) )
      (if (atom pathname) (load-lib pathname) (mapcar #'load-lib pathname))
) ) )

;;; Hilfsfunktionen für
;;; LET/LET*/MULTIPLE-VALUE-BIND/Lambda-Ausdruck/FLET/LABELS:

;; Syntaxanalyse:

; analysiert eine Parameterliste von LET/LET*, liefert:
; die Liste der Symbole,
; die Liste der Formen.
(defun analyze-letlist (parameters)
  (do ((L parameters (cdr L))
       (symbols nil)
       (forms nil))
      ((null L) (values (nreverse symbols) (nreverse forms)))
    (cond ((symbolp (car L)) (push (car L) symbols) (push nil forms))
          ((and (consp (car L)) (symbolp (caar L))
                (consp (cdar L)) (null (cddar L))
           )
           (push (caar L) symbols) (push (cadar L) forms)
          )
          (t (catch 'c-error
               (c-error #+DEUTSCH "Falsche Syntax in LET/LET*: ~S"
                        #+ENGLISH "Illegal syntax in LET/LET*: ~S"
                        (car L)
    )     )  ) )
) )

; analysiert eine Lambdaliste einer Funktion (CLTL S. 60), liefert 13 Werte:
; 1. Liste der required Parameter
; 2. Liste der optionalen Parameter
; 3. Liste der Initformen der optionalen Parameter
; 4. Liste der Svars zu den optionalen Parametern (0 für die fehlenden)
; 5. Rest-Parameter oder 0
; 6. Flag, ob Keywords erlaubt sind
; 7. Liste der Keywords
; 8. Liste der Keyword-Parameter
; 9. Liste der Initformen der Keyword-Parameter
; 10. Liste der Svars zu den Keyword-Parametern (0 für die fehlenden)
; 11. Flag, ob andere Keywords erlaubt sind
; 12. Liste der Aux-Variablen
; 13. Liste der Initformen der Aux-Variablen
(defun analyze-lambdalist (lambdalist)
  (let ((L lambdalist) ; Rest der Lambdaliste
        (req nil)
        (optvar nil)
        (optinit nil)
        (optsvar nil)
        (rest 0)
        (keyflag nil)
        (keyword nil)
        (keyvar nil)
        (keyinit nil)
        (keysvar nil)
        (allow-other-keys nil)
        (auxvar nil)
        (auxinit nil))
       ; alle in umgedrehter Reihenfolge
    (macrolet ((err-illegal (item)
                 `(catch 'c-error
                    (c-error #+DEUTSCH "Dieser Lambdalistenmarker ist an dieser Stelle nicht erlaubt: ~S"
                             #+ENGLISH "Lambda list marker ~S not allowed here."
                             ,item
                  ) )
               )
               (err-norest ()
                 `(catch 'c-error
                    (c-error #+DEUTSCH "Fehlender &REST-Parameter in der Lambdaliste: ~S"
                             #+ENGLISH "Missing &REST parameter in lambda list ~S"
                             lambdalist
                  ) )
               )
               (err-superflu (item)
                 `(catch 'c-error
                    (c-error #+DEUTSCH "Überflüssiges Lambdalisten-Element: ~S"
                             #+ENGLISH "Lambda list element ~S is superfluous."
                             ,item
                  ) )
              ))
      ; Required Parameter:
      (loop
        (if (atom L) (return))
        (let ((item (car L)))
          (if (symbolp item)
            (if (memq item lambda-list-keywords)
              (if (memq item '(&optional &rest &key &aux))
                (return)
                (err-illegal item)
              )
              (push item req)
            )
            (lambdalist-error item)
        ) )
        (setq L (cdr L))
      )
      ; Hier gilt (or (atom L) (member (car L) '(&optional &rest &key &aux))).
      ; Optionale Parameter:
      (when (and (consp L) (eq (car L) '&optional))
        (setq L (cdr L))
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (if (memq item '(&rest &key &aux))
                  (return)
                  (err-illegal item)
                )
                (progn (push item optvar) (push nil optinit) (push 0 optsvar))
              )
              (if (and (consp item) (symbolp (car item)))
                (if (null (cdr item))
                  (progn (push (car item) optvar) (push nil optinit) (push 0 optsvar))
                  (if (consp (cdr item))
                    (if (null (cddr item))
                      (progn (push (car item) optvar) (push (cadr item) optinit) (push 0 optsvar))
                      (if (and (consp (cddr item)) (symbolp (caddr item)) (null (cdddr item)))
                        (progn (push (car item) optvar) (push (cadr item) optinit) (push (caddr item) optsvar))
                        (lambdalist-error item)
                    ) )
                    (lambdalist-error item)
                ) )
                (lambdalist-error item)
          ) ) )
          (setq L (cdr L))
      ) )
      ; Hier gilt (or (atom L) (member (car L) '(&rest &key &aux))).
      ; Rest-Parameter:
      (when (and (consp L) (eq (car L) '&rest))
        (setq L (cdr L))
        (if (atom L)
          (err-norest)
          (prog ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (progn (err-norest) (return))
                (setq rest item)
              )
              (lambdalist-error item)
            )
            (setq L (cdr L))
      ) ) )
      ; Vorrücken bis zum nächsten &key oder &aux :
      (loop
        (when (atom L) (return))
        (let ((item (car L)))
          (if (memq item lambda-list-keywords)
            (if (memq item '(&key &aux))
              (return)
              (err-illegal item)
            )
            (err-superflu item)
        ) )
        (setq L (cdr L))
      )
      ; Hier gilt (or (atom L) (member (car L) '(&key &aux))).
      ; Keyword-Parameter:
      (when (and (consp L) (eq (car L) '&key))
        (setq L (cdr L))
        (setq keyflag t)
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (if (memq item '(&allow-other-keys &aux))
                  (return)
                  (err-illegal item)
                )
                (progn
                  (push (intern (symbol-name item) *keyword-package*) keyword)
                  (push item keyvar) (push nil keyinit) (push 0 keysvar)
              ) )
              (if (and
                    (consp item)
                    (or
                      (symbolp (car item))
                      (and (consp (car item))
                           (keywordp (caar item))
                           (consp (cdar item))
                           (symbolp (cadar item))
                           (null (cddar item))
                    ) )
                    (or (null (cdr item))
                        (and (consp (cdr item))
                             (or (null (cddr item))
                                 (and (consp (cddr item)) (symbolp (caddr item)) (null (cdddr item)))
                  ) )   )    )
                (progn
                  (if (consp (car item))
                    (progn (push (caar item) keyword) (push (cadar item) keyvar))
                    (progn (push (intern (symbol-name (car item)) *keyword-package*) keyword) (push (car item) keyvar))
                  )
                  (if (consp (cdr item))
                    (progn
                      (push (cadr item) keyinit)
                      (if (consp (cddr item))
                        (push (caddr item) keysvar)
                        (push 0 keysvar)
                    ) )
                    (progn (push nil keyinit) (push 0 keysvar))
                ) )
                (lambdalist-error item)
          ) ) )
          (setq L (cdr L))
        )
        ; Hier gilt (or (atom L) (member (car L) '(&allow-other-keys &aux))).
        (when (and (consp L) (eq (car L) '&allow-other-keys))
          (setq allow-other-keys t)
          (setq L (cdr L))
      ) )
      ; Vorrücken bis zum nächsten &AUX :
      (loop
        (when (atom L) (return))
        (let ((item (car L)))
          (if (memq item lambda-list-keywords)
            (if (memq item '(&aux))
              (return)
              (err-illegal item)
            )
            (err-superflu item)
        ) )
        (setq L (cdr L))
      )
      ; Hier gilt (or (atom L) (member (car L) '(&aux))).
      ; &AUX-Variablen:
      (when (and (consp L) (eq (car L) '&aux))
        (setq L (cdr L))
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (memq item lambda-list-keywords)
                (err-illegal item)
                (progn (push item auxvar) (push nil auxinit))
              )
              (if (and (consp item) (symbolp (car item)))
                (if (null (cdr item))
                  (progn (push (car item) auxvar) (push nil auxinit))
                  (if (and (consp (cdr item)) (null (cddr item)))
                    (progn (push (car item) auxvar) (push (cadr item) auxinit))
                    (lambdalist-error item)
                ) )
                (lambdalist-error item)
          ) ) )
          (setq L (cdr L))
      ) )
      ; Hier gilt (atom L).
      (if L
        (catch 'c-error
          (c-error #+DEUTSCH "Eine Lambdaliste, die einen Punkt enthält, ist nur bei Macros erlaubt, nicht hier: ~S"
                   #+ENGLISH "Lambda lists with dots are only allowed in macros, not here: ~S"
                   lambdalist
      ) ) )
    )
    (values
      (nreverse req)
      (nreverse optvar) (nreverse optinit) (nreverse optsvar)
      rest
      keyflag
      (nreverse keyword) (nreverse keyvar) (nreverse keyinit) (nreverse keysvar)
      allow-other-keys
      (nreverse auxvar) (nreverse auxinit)
) ) )

(defun lambdalist-error (item)
  (catch 'c-error
    (c-error #+DEUTSCH "Unzulässiges Lambdalistenelement: ~S"
             #+ENGLISH "Illegal lambda list element ~S"
             item
) ) )

; (inline-callable-function-p form n) stellt fest, ob form eine Form ist, die
; eine Funktion liefert, die mit n Argumenten Inline aufgerufen werden kann.
; (vorbehaltlich Syntax-Errors in der Lambdaliste)
(defun inline-callable-function-p (form n)
  ; muß von der Bauart (FUNCTION funname) sein
  (and (consp form) (eq (first form) 'FUNCTION)
       (consp (cdr form)) (null (cddr form))
       (let ((funname (second form)))
         ; funname muß von der Bauart (LAMBDA lambdalist ...) sein
         (and (consp funname) (eq (first funname) 'LAMBDA) (consp (cdr funname))
              (let ((lambdalist (second funname)))
                ; lambdalist muß eine Liste sein, die kein &KEY enthält
                ; (Funktionen mit &KEY werden nicht INLINE-expandiert, weil die
                ; Zuordnung von den Argumenten zu den Variablen nur dynamisch,
                ; mit GETF, möglich ist, und das kann die in Assembler
                ; geschriebene APPLY-Routine schneller.)
                (and (listp lambdalist)
                     (not (position '&KEY lambdalist))
                     (not (position '&ALLOW-OTHER-KEYS lambdalist))
                     (let ((&opt-pos (position '&OPTIONAL lambdalist))
                           (&rest-pos (position '&REST lambdalist))
                           (&aux-pos (or (position '&AUX lambdalist)
                                         (length lambdalist)
                          ))         )
                       (if &rest-pos
                         ; &rest angegeben
                         (>= n (or &opt-pos &rest-pos))
                         ; &rest nicht angegeben
                         (if &opt-pos
                           (<= &opt-pos n (- &aux-pos 1))
                           (= n &aux-pos)
                     ) ) )
              ) )
       ) )
) )


;; Special-deklarierte Symbole:

(defvar *specials*) ; Liste aller zuletzt special deklarierten Symbole
(defvar *ignores*) ; Liste aller zuletzt ignore deklarierten Symbole

; pusht alle Symbole von specials als Variablen auf *venv* :
(defun push-specials ()
  (dolist (sym *specials*) (push (make-special-var sym) *venv*))
)

; Überprüft eine Variable, ob sie zu Recht ignore-deklariert ist oder nicht...
(defun ignore-check (var)
  (if (member (var-name var) *ignores* :test #'eq)
    ; var ignore-deklariert
    (if (var-specialp var)
      (c-warn #+DEUTSCH "Binden der Variablen ~S kann trotz IGNORE-Deklaration~%Seiteneffekte haben, weil sie SPECIAL deklariert ist."
              #+ENGLISH "Binding variable ~S can cause side effects despite of IGNORE declaration~%since it is declared SPECIAL."
              (var-name var)
      )
      (if (var-usedp var)
        (c-warn #+DEUTSCH "Variable ~S wird trotz IGNORE-Deklaration benutzt."
                #+ENGLISH "variable ~S is used despite of IGNORE declaration."
                (var-name var)
    ) ) )
    ; var nicht ignore-deklariert
    (unless (or (var-specialp var) (var-usedp var))
      ; var lexikalisch und unbenutzt
      (let ((sym (var-name var)))
        (unless (null (symbol-package sym)) ; sym ein (gensym) ?
          ; (Symbole ohne Home-Package kommen nicht vom Benutzer, die Warnung
          ; würde nur verwirren).
          (c-warn #+DEUTSCH "Variable ~S wird nicht benutzt.~%Schreibfehler oder fehlende IGNORE-Deklaration?"
                  #+ENGLISH "variable ~S is not used.~%Misspelled or missing IGNORE declaration?"
                  sym
) ) ) ) ) )

; liefert den Code, der zum neuen Aufbau einer Closure und ihrer Unterbringung
; im Stack nötig ist:
; Dieser Code erweitert das von (cdr venvc) beschriebene Venv um closurevars,
; (cdr stackz) ist der aktuelle Stackzustand.
; Nach Aufbau der Closure sind venvc bzw. stackz die aktuellen Zustände.
(defun c-MAKE-CLOSURE (closurevars venvc stackz)
  (if closurevars
    `((VENV ,(cdr venvc) ,(cdr stackz))
      (MAKE-VECTOR1&PUSH ,(length closurevars))
     )
    '()
) )

;; Es gibt zwei Arten von Variablen-Bindungs-Vorgehensweisen:
; 1. fixed-var: die Variable hat eine Position im Stack, darf nicht wegoptimiert
;               werden. Ist die Variable dann doch in der Closure, so muß ihr
;               Wert dorthin übertragen werden; ist die Variable dynamisch, so
;               muß ein Bindungsframe aufgemacht werden.
;               Auftreten: MULTIPLE-VALUE-BIND, Lambda-Ausdruck (required,
;               optional, rest, keyword - Parameter)
; 2. movable-var: die Variable darf wegoptimiert werden, falls sie konstant ist
;                 (sie entweder dynamisch und konstant ist oder lexikalisch
;                  und an eine Konstante gebunden und nie geSETQed wird). Hier
;                 spielt also der Init-Wert eine Rolle.
;                 Auftreten: LET, LET*, Lambda-Ausdruck (optional-svar,
;                 keyword-svar, aux-Variablen)

;; 1. fixed-var

; Bindung einer fixed-var:
; symbol --> Variable
; Läßt *stackz* unverändert.
(defun bind-fixed-var-1 (symbol)
  (if (or (constantp symbol)
          (proclaimed-special-p symbol)
          (member symbol *specials* :test #'eq)
      )
    ; muß symbol dynamisch binden:
    (progn
      (when (c-constantp symbol)
        (catch 'c-error
          (c-error #+DEUTSCH "Konstante ~S kann nicht gebunden werden."
                   #+ENGLISH "Constant ~S cannot be bound."
                   symbol
      ) ) )
      (make-special-var symbol)
    )
    ; muß symbol lexikalisch binden:
    (make-var :name symbol :specialp nil :constantp nil
              :usedp nil :really-usedp nil :closurep nil
              :stackz *stackz* :venvc *venvc*
    )
) )

; registriert in *stackz*, daß eine fixed-var gebunden wird
(defun bind-fixed-var-2 (var)
  (when (and (var-specialp var) (not (var-constantp var)))
    (push '(BIND 1) *stackz*)
) )

; liefert den Code, der die Variable var an den Inhalt von stackdummyvar
; bindet. stackz ist der Stackzustand vor dem Binden dieser Variablen.
(defun c-bind-fixed-var (var stackdummyvar stackz)
  (if (var-specialp var)
    (if (var-constantp var)
      '() ; Konstante kann nicht gebunden werden
      `((GET ,stackdummyvar ,*venvc* ,stackz)
        (BIND ,(new-const (var-name var)))
       )
    )
    ; var lexikalisch, nach Definition nicht konstant
    (if (var-closurep var)
      `((GET ,stackdummyvar ,*venvc* ,stackz)
        (SET ,var ,*venvc* ,stackz)
       )
      '() ; var und stackdummyvar identisch
) ) )

; Kreiert je eine Stackvariable und eine Fixed-Variable zu jedem Symbol aus der
; Variablenliste symbols und liefert beide Listen als Werte.
(defun process-fixed-var-list (symbols)
  (do ((symbolsr symbols (cdr symbolsr))
       (varlist nil) ; Liste der Variablen
       (stackvarlist nil)) ; Liste der Stackvariablen (teils Dummys)
      ((null symbolsr) (values (nreverse varlist) (nreverse stackvarlist)))
    (push 1 *stackz*)
    ; (mit constantp=nil und really-usedp=t, um eine Wegoptimierung zu vermeiden)
    (push (make-var :name (gensym) :specialp nil :constantp nil
                    :usedp nil :really-usedp t :closurep nil
                    :stackz *stackz* :venvc *venvc*
          )
          stackvarlist
    )
    (push (bind-fixed-var-1 (car symbolsr)) varlist)
) )

; Eliminiert alle Zuweisungen auf eine unbenutzte Variable.
(defun unmodify-unused-var (var)
  (dolist (modified (var-modified-list var))
    (let ((value-anode (car modified))) ; Anode für zugewiesenen Wert
      (when (null (cdr (anode-seclass value-anode)))
        (setf (anode-code value-anode) '()) ; evtl. Wert-Form entfernen
    ) )
    (let ((set-anode (cdr modified))) ; Anode der Zuweisung selbst
      (setf (anode-code set-anode) '()) ; Zuweisung entfernen
) ) )

; Überprüft und optimiert die Variablen
; und liefert die Liste der Closure-Variablen (in der richtigen Reihenfolge).
(defun checking-fixed-var-list (varlist &aux (closurevarlist '()))
  (dolist (var varlist (nreverse closurevarlist))
    ; 1. Schritt: eventuelle Warnungen ausgeben
    (ignore-check var)
    ; 2. Schritt: Variablen-Ort (Stack oder Closure) endgültig bestimmen,
    ; evtl. optimieren
    (unless (var-specialp var)
      ; nur lexikalische Variablen können in der Closure liegen,
      ; nur bei lexikalischen Variablen kann optimiert werden
      (if (not (var-really-usedp var))
        ; Variable lexikalisch und unbenutzt
        (progn ; Variable eliminieren
          (setf (var-closurep var) nil)
          (unmodify-unused-var var) ; Zuweisungen auf var eliminieren
        )
        (when (var-closurep var)
          ; Variable muß in der Closure liegen
          (push var closurevarlist)
    ) ) )
) )

;; 2. movable-var

; Beim Binden einer Variablen var an einen Anode anode:
; Wird eine lexikalische Variable an den Wert an einer lexikalischen Variablen
; gebunden? Wenn ja, an welche Variable?
(defun bound-to-var-p (var anode)
  (if (var-specialp var)
    nil
    ; var lexikalisch
    (loop
      (unless (eql (length (anode-code anode)) 1) (return nil))
      (setq anode (first (anode-code anode)))
      (unless (anode-p anode)
        (if (and (consp anode) (eq (first anode) 'GET))
          ; Code zum Anode besteht genau aus ((GET outervar ...)).
          (return (second anode))
          (return nil)
    ) ) )
) )

; Bindung einer movable-var:
; symbol form-anode --> Variable
; erweitert *stackz* um genau einen Eintrag
(defun bind-movable-var (symbol form-anode)
  (if (or (constantp symbol)
          (proclaimed-special-p symbol)
          (member symbol *specials* :test #'eq)
      )
    ; muß symbol dynamisch binden:
    (progn
      (if (c-constantp symbol)
        (progn
          (catch 'c-error
            (c-error #+DEUTSCH "Konstante ~S kann nicht gebunden werden."
                     #+ENGLISH "Constant ~S cannot be bound."
                     symbol
          ) )
          (push 0 *stackz*)
        )
        (push '(BIND 1) *stackz*)
      )
      (make-special-var symbol)
    )
    ; muß symbol lexikalisch binden:
    (let ((var
            (progn
              (push 1 *stackz*) ; vorläufig: 1 Platz auf dem Stack
              (make-var :name symbol :specialp nil
                :constantp (anode-constantp form-anode) ; wird bei Zuweisungen auf NIL gesetzt
                :constant-value (if (anode-constantp form-anode) (anode-constant-value form-anode))
                :usedp nil :really-usedp nil :closurep nil ; wird evtl. auf T gesetzt
                :stackz *stackz* :venvc *venvc*
         )) ) )
      (let ((outervar (bound-to-var-p var form-anode)))
        (when outervar ; Wird var an eine Variable outervar gebunden, so
                       ; darf später evtl. jede Referenz zu var in eine
                       ; Referenz zu outervar umgewandelt werden.
          (push (list var form-anode) (var-replaceable-list outervar))
      ) )
      var
) ) )

; liefert den Code, der die Variable var an A0 bindet:
(defun c-bind-movable-var (var)
  (if (var-specialp var)
    (if (var-constantp var)
      '() ; dynamische Konstanten können nicht gebunden werden
      `((BIND ,(new-const (var-name var))))
    )
    (if (var-closurep var)
      ; Closure-Variable schreiben:
      ; (var-stackz var) = (0 . ...) ist der aktuelle Stackzustand.
      `((SET ,var ,*venvc* ,(var-stackz var)))
      ; lexikalische Variable: wurde eventuell aus dem Stack eliminiert
      (if (zerop (first (var-stackz var)))
        '()
        `((PUSH)) ; im Stack: in die nächstuntere Stacklocation schreiben
) ) ) )

; liefert den Code, der die Variable var an das Ergebnis des ANODEs anode bindet
(defun c-bind-movable-var-anode (var anode)
  (let ((binding-anode
          (make-anode :type 'BIND-MOVABLE
                      :sub-anodes '()
                      :seclass '(NIL . NIL)
                      :code (c-bind-movable-var var)
       )) )
    (let ((outervar (bound-to-var-p var anode)))
      (when outervar ; Wird var an eine Variable outervar gebunden, so
                     ; darf später evtl. jede Referenz zu var in eine
                     ; Referenz zu outervar umgewandelt werden.
        (dolist (innervar-info (var-replaceable-list outervar))
          (when (eq (first innervar-info) var)
            (setf (cddr innervar-info) binding-anode) ; binding-anode nachtragen
    ) ) ) )
    (list anode binding-anode)
) )

; (process-movable-var-list symbols initforms *-flag) compiliert die initforms
; (wie bei LET/LET*) und assoziiert sie mit den Variablen zu symbols.
; Verändert *venv* (bei *-flag : incrementell, sonst auf einmal).
; Liefert drei Werte:
; 1. Liste der Variablen,
; 2. Liste der ANODEs zu den initforms,
; 3. Liste der Stackzustände nach dem Binden der Variablen.
(defun process-movable-var-list (symbols initforms *-flag)
  (do ((symbolsr symbols (cdr symbolsr))
       (initformsr initforms (cdr initformsr))
       (varlist '())
       (anodelist '())
       (stackzlist '()))
      ((null symbolsr)
       (unless *-flag (dolist (var varlist) (push var *venv*))) ; Binden bei LET
       (values (nreverse varlist) (nreverse anodelist) (nreverse stackzlist))
      )
    (let* ((initform (car initformsr))
           (anode (c-form initform 'ONE)) ; initform compilieren
           (var (bind-movable-var (car symbolsr) anode)))
      (push anode anodelist)
      (push var varlist)
      (push *stackz* stackzlist)
      (when *-flag (push var *venv*)) ; Binden bei LET*
) ) )

; Überprüft und optimiert die Variablen (wie bei LET/LET*)
; und liefert die Liste der Closure-Variablen (in der richtigen Reihenfolge).
(defun checking-movable-var-list (varlist anodelist)
  (do ((varlistr varlist (cdr varlistr))
       (anodelistr anodelist (cdr anodelistr))
       (closurevarlist '()))
      ((null varlistr) (nreverse closurevarlist))
    (let ((var (car varlistr)))
      (when var
        ; 1. Schritt: eventuelle Warnungen ausgeben
        (ignore-check var)
        ; 2. Schritt: Variablen-Ort (Stack oder Closure oder eliminiert)
        ; endgültig bestimmen
        (unless (var-specialp var)
          ; nur bei lexikalischen Variablen kann optimiert werden
          (if (var-constantp var)
            ; Variable lexikalisch und konstant
            (progn ; Variable eliminieren
              (setf (var-closurep var) nil)
              (setf (first (var-stackz var)) 0) ; aus dem Stack entfernen
              (when (null (cdr (anode-seclass (car anodelistr))))
                (setf (anode-code (car anodelistr)) '()) ; evtl. initform entfernen
            ) )
            (if (not (var-really-usedp var))
              ; Variable lexikalisch und unbenutzt
              (progn ; Variable eliminieren
                (setf (var-closurep var) nil)
                (setf (first (var-stackz var)) 0) ; aus dem Stack entfernen
                (when (null (cdr (anode-seclass (car anodelistr))))
                  (setf (anode-code (car anodelistr)) '()) ; evtl. initform entfernen
                )
                (unmodify-unused-var var) ; Zuweisungen auf var eliminieren
              )
              (when (var-closurep var)
                ; Variable muß in der Closure liegen
                (setf (first (var-stackz var)) 0) ; belegt 0 Stack-Einträge
                (push var closurevarlist)
        ) ) ) )
) ) ) )

; Optimiert eine Liste von Variablen.
; (In der Liste müssen die lexikalisch inneren Variablen zuletzt kommen.)
(defun optimize-var-list (vars)
  (unless *no-code*
    (dolist (var (reverse vars))
      (when var
        ; Optimierung (innere Variablen zuerst):
        ; Wird eine Variable innervar an den Wert von var gebunden, wird
        ; während der Lebensdauer von innervar weder innervar noch var verändert
        ; (um dies sicherstellen zu können, müssen beide lexikalisch und im Stack
        ; sein), so kann innervar durch var ersetzt werden.
        (unless (or (var-specialp var) (var-closurep var))
          ; var ist lexikalisch und im Stack
          (dolist (innervar-info (var-replaceable-list var))
            (let ((innervar (first innervar-info)))
              ; innervar ist eine movable-var, die mit var initialisiert wird.
              ; Während der Lebensdauer von innervar wird var nichts zugewiesen.
              (unless (or (var-specialp innervar) (var-closurep innervar))
                ; innervar ist lexikalisch und im Stack
                (when (null (var-modified-list innervar))
                  ; Während der Lebensdauer von innervar wird auch innervar
                  ; nichts zugewiesen.
                  (unless (eql (first (var-stackz innervar)) 0) ; innervar noch nicht wegoptimiert?
                    (when (cddr innervar-info) ; und innervar-info korrekt dreigliedrig?
                      ; Variable innervar eliminieren:
                      (setf (first (var-stackz innervar)) 0) ; aus dem Stack entfernen
                      ; Initialisierung und Binden von innervar eliminieren:
                      (setf (anode-code (second innervar-info)) '())
                      (setf (anode-code (cddr innervar-info)) '())
                      ; Die Referenzen auf die Variable innervar werden
                      ; in Referenzen auf die Variable var umgewandelt:
                      (let ((using-var (var-usedp var)))
                        (do ((using-innervar (var-usedp innervar) (cdr using-innervar)))
                            ((atom using-innervar))
                          (let* ((anode (car using-innervar)) ; ein Anode vom Typ VAR
                                 (code (anode-code anode))) ; sein Code, () oder ((GET ...))
                            (unless (null code)
                              ; (anode-code anode) ist von der Gestalt ((GET innervar ...))
                              (setf (second (car code)) var)
                              (push anode using-var)
                        ) ) )
                        (setf (var-usedp var) using-var)
                      )
        ) ) ) ) ) ) )
) ) ) )

; Bildet den Code, der eine Liste von Variablen, zusammen mit ihren svars,
; bindet (wie bei Lambdabody- Optional/Key - Variablen).
(defun c-bind-with-svars (-vars -dummys s-vars -anodes s-anodes -stackzs)
  (do ((-varsr -vars (cdr -varsr)) ; fixed-vars
       (-dummysr -dummys (cdr -dummysr))
       (s-varsr s-vars (cdr s-varsr)) ; movable-vars
       (-anodesr -anodes (cdr -anodesr))
       (s-anodesr s-anodes (cdr s-anodesr))
       (-stackzsr -stackzs (cdr -stackzsr))
       (L '()))
      ((null -varsr) (nreverse L))
    (when (car s-varsr)
      (setq L
        (revappend
          (c-bind-movable-var-anode (car s-varsr) (car s-anodesr))
          L
    ) ) )
    (setq L
      (revappend
        (let* ((var (car -varsr))
               (stackdummyvar (car -dummysr))
               (anode (car -anodesr))
               (stackz (car -stackzsr))
               (label (make-label 'ONE)))
          (if (var-specialp var)
            `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
              ,anode
              ,label
              ,@(if (var-constantp var)
                  '() ; Konstante kann nicht gebunden werden
                  `((BIND ,(new-const (var-name var))))
                )
             )
            ; var lexikalisch, nach Definition nicht konstant
            (if (var-closurep var)
              `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                ,anode
                ,label
                (SET ,var ,*venvc* ,stackz)
               )
              (if (not (var-really-usedp var))
                ; Variable wurde in checking-fixed-var-list wegoptimiert
                (if (cdr (anode-seclass anode))
                  `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                    ,anode
                    ,label
                   )
                  '()
                )
                ; im Stack vorhandene Variable
                `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                  ,anode
                  (SET ,var ,*venvc* ,stackz)
                  ,label
                 )
        ) ) ) )
        L
    ) )
) )

; compiliere (name lambdalist {declaration|docstring}* {form}*), liefere FNODE
(defun c-LAMBDABODY (name lambdabody &optional fenv-cons)
  (test-list lambdabody 1)
  (let* ((*func* (make-fnode :name name :enclosing *func* :venvc *venvc*))
         (*stackz* *func*) ; leerer Stack
         (*venvc* (cons *func* *venvc*))
         (*func-start-label* (make-label 'NIL))
         (*anonymous-count* 0)
         (anode (catch 'c-error
    ; ab hier wird's kompliziert
    (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
                          keyflag keyword keyvar keyinit keysvar allow-other-keys
                          auxvar auxinit)
        (if fenv-cons
          (values-list (cdar fenv-cons)) ; Bei c-LABELS wurde analyze-lambdalist schon aufgerufen
          (analyze-lambdalist (car lambdabody))
        )
      (setf (fnode-req-anz *func*) (length reqvar)
            (fnode-opt-anz *func*) (length optvar)
            (fnode-rest-flag *func*) (not (eql restvar 0))
            (fnode-keyword-flag *func*) keyflag
            (fnode-keywords *func*) keyword
            (fnode-allow-other-keys-flag *func*) allow-other-keys
      )
      (when fenv-cons (setf (caar fenv-cons) *func*)) ; Fixup für c-LABELS
      (multiple-value-bind (body-rest declarations)
          (parse-body (cdr lambdabody) t *fenv*)
        (let ((oldstackz *stackz*)
              (*stackz* *stackz*)
              (*denv* *denv*)
              (*venv* *venv*)
              (*venvc* *venvc*)
              *specials* *ignores*
              req-vars req-dummys req-stackzs
              opt-vars opt-dummys opt-anodes opts-vars opts-anodes opt-stackzs
              rest-vars rest-dummys rest-stackzs
              key-vars key-dummys key-anodes keys-vars keys-anodes key-stackzs
              aux-vars aux-anodes
              closuredummy-stackz closuredummy-venvc
             )
          (multiple-value-setq (*specials* *ignores*)
            (process-declarations declarations)
          )
          ; Special-Variable auf *venv* pushen:
          (push-specials)
          ; Sichtbarkeit von Closure-Dummyvar:
          (push nil *venvc*)
          (setq closuredummy-venvc *venvc*)
          ; Stack-Dummy-Variable für die reqvar,optvar,restvar,keyvar bilden:
          (multiple-value-setq (req-vars req-dummys)
            (process-fixed-var-list reqvar)
          )
          (multiple-value-setq (opt-vars opt-dummys)
            (process-fixed-var-list optvar)
          )
          (multiple-value-setq (rest-vars rest-dummys)
            (if (eql restvar 0)
              (values '() '())
              (process-fixed-var-list (list restvar))
          ) )
          (multiple-value-setq (key-vars key-dummys)
            (process-fixed-var-list keyvar)
          )
          ; Platz für die Funktion selbst (unter den Argumenten):
          (push 1 *stackz*)
          ; Platz für Closure-Dummyvar:
          (push 0 *stackz*)
          (setq closuredummy-stackz *stackz*)
          ; Bindungen der required-Parameter aktivieren:
          (dolist (var req-vars (setq req-stackzs (nreverse req-stackzs)))
            (push var *venv*)
            (push *stackz* req-stackzs)
            (bind-fixed-var-2 var)
          )
          ; Bindungen der optional-Parameter/svar aktivieren:
          (do ((opt-varsr opt-vars (cdr opt-varsr))
               (opt-dummysr opt-dummys (cdr opt-dummysr))
               (optinitr optinit (cdr optinitr))
               (optsvarr optsvar (cdr optsvarr)))
              ((null opt-varsr)
               (setq opt-anodes (nreverse opt-anodes))
               (setq opt-stackzs (nreverse opt-stackzs))
               (setq opts-vars (nreverse opts-vars))
               (setq opts-anodes (nreverse opts-anodes))
              )
            (if (eql (car optsvarr) 0)
              (progn (push nil opts-vars) (push nil opts-anodes))
              (let* ((anode
                       (make-anode
                         :type 'OPTIONAL-SVAR
                         :sub-anodes '()
                         :seclass (cons (list (car opt-dummysr)) 'NIL)
                         :code `((BOUNDP ,(car opt-dummysr) ,*venvc* ,*stackz*))
                     ) )
                     (var (bind-movable-var (car optsvarr) anode))
                    )
                (push anode opts-anodes)
                (push var opts-vars)
            ) )
            (push (c-form (car optinitr) 'ONE) opt-anodes)
            (push (car opt-varsr) *venv*)
            (push *stackz* opt-stackzs) (bind-fixed-var-2 (car opt-varsr))
            (unless (eql (car optsvarr) 0) (push (car opts-vars) *venv*))
          )
          ; Bindung des rest-Parameters aktivieren:
          (unless (eql restvar 0)
            (push (car rest-vars) *venv*)
            (push *stackz* rest-stackzs) ; (nreverse rest-stackzs) unnötig
            (bind-fixed-var-2 (car rest-vars))
          )
          ; Bindungen der keyword-Parameter/svar aktivieren:
          (do ((key-varsr key-vars (cdr key-varsr))
               (key-dummysr key-dummys (cdr key-dummysr))
               (keyinitr keyinit (cdr keyinitr))
               (keysvarr keysvar (cdr keysvarr)))
              ((null key-varsr)
               (setq key-anodes (nreverse key-anodes))
               (setq key-stackzs (nreverse key-stackzs))
               (setq keys-vars (nreverse keys-vars))
               (setq keys-anodes (nreverse keys-anodes))
              )
            (if (eql (car keysvarr) 0)
              (progn (push nil keys-vars) (push nil keys-anodes))
              (let* ((anode
                       (make-anode
                         :type 'KEY-SVAR
                         :sub-anodes '()
                         :seclass (cons (list (car key-dummysr)) 'NIL)
                         :code `((BOUNDP ,(car key-dummysr) ,*venvc* ,*stackz*))
                     ) )
                     (var (bind-movable-var (car keysvarr) anode))
                    )
                (push anode keys-anodes)
                (push var keys-vars)
            ) )
            (push (c-form (car keyinitr) 'ONE) key-anodes)
            (push (car key-varsr) *venv*)
            (push *stackz* key-stackzs) (bind-fixed-var-2 (car key-varsr))
            (unless (eql (car keysvarr) 0) (push (car keys-vars) *venv*))
          )
          ; Bindungen der Aux-Variablen aktivieren:
          (do ((auxvarr auxvar (cdr auxvarr))
               (auxinitr auxinit (cdr auxinitr)))
              ((null auxvarr)
               (setq aux-vars (nreverse aux-vars))
               (setq aux-anodes (nreverse aux-anodes))
              )
            (let* ((initform (car auxinitr))
                   (anode (c-form initform 'ONE))
                   (var (bind-movable-var (car auxvarr) anode)))
              (push anode aux-anodes)
              (push var aux-vars)
              (push var *venv*)
          ) )
          (let* ((body-anode (c-form `(PROGN ,@body-rest) 'ALL))
                 ; Überprüfen der Variablen:
                 (closurevars
                   (append
                     (checking-fixed-var-list req-vars)
                     (checking-fixed-var-list opt-vars)
                     (checking-movable-var-list opts-vars opts-anodes)
                     (checking-fixed-var-list rest-vars)
                     (checking-fixed-var-list key-vars)
                     (checking-movable-var-list keys-vars keys-anodes)
                     (checking-movable-var-list aux-vars aux-anodes)
                 ) )
                 (codelist
                   `(,*func-start-label*
                     ,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                     ,@(mapcap #'c-bind-fixed-var req-vars req-dummys req-stackzs)
                     ,@(c-bind-with-svars opt-vars opt-dummys opts-vars opt-anodes opts-anodes opt-stackzs)
                     ,@(mapcap #'c-bind-fixed-var rest-vars rest-dummys rest-stackzs)
                     ,@(c-bind-with-svars key-vars key-dummys keys-vars key-anodes keys-anodes key-stackzs)
                     ,@(mapcap #'c-bind-movable-var-anode aux-vars aux-anodes)
                     ,body-anode
                     (UNWIND ,*stackz* ,oldstackz t)
                     (RET)
                 )  )
                 (anode
                   (make-anode
                     :type 'LAMBDABODY
                     :source lambdabody
                     :sub-anodes `(,@opt-anodes ,@key-anodes ,@aux-anodes ,body-anode)
                     :seclass '(T . T) ; die Seiteneffektklasse dieses Anode ist irrelevant
                     :stackz oldstackz
                     :code codelist
                )) )
            (when closurevars
              (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
              (setf (first closuredummy-venvc)
                (cons closurevars closuredummy-stackz)
            ) )
            (optimize-var-list (append req-vars opt-vars opts-vars rest-vars key-vars keys-vars aux-vars))
            anode
    ) ) ) )
    ; das war die Produktion des Anode
        ))      )
    (setf (fnode-code *func*) anode)
    (when (eq (anode-type anode) 'ERROR)
      ; korrekte, aber nichtstuende Funktion daraus machen
      (setf (fnode-req-anz *func*) 0
            (fnode-opt-anz *func*) 0
            (fnode-rest-flag *func*) t
            (fnode-keyword-flag *func*) nil
            (fnode-keywords *func*) '()
            (fnode-allow-other-keys-flag *func*) nil
            (anode-code (fnode-code *func*)) `((NIL) (SKIP 2) (RET))
    ) )
    (setf (fnode-Consts-Offset *func*)
      (+ (setf (fnode-Keyword-Offset *func*)
           (+ (setf (fnode-Tagbodys-Offset *func*)
                (+ (setf (fnode-Blocks-Offset *func*)
                     (if (fnode-venvconst *func*) 1 0)
                   )
                   (length (fnode-Blocks *func*))
              ) )
              (length (fnode-Tagbodys *func*))
         ) )
         (length (fnode-Keywords *func*))
    ) )
    *func*
) )

; liefert den ANODE, der (bei gegebenem aktuellem Stackzustand)
; die zu einem FNODE gehörende Funktion als Wert liefert.
(defun c-FNODE-FUNCTION (fnode &optional (*stackz* *stackz*))
  (make-anode
    :type 'FUNCTION
    :sub-anodes '()
    :seclass '(NIL . NIL)
    :code (if (zerop (fnode-keyword-offset fnode))
            `((FCONST ,fnode))
            `(,@(if (fnode-Venvconst fnode)
                  (prog1 ; beim Aufbau mitzugebendes Venv
                    `((VENV ,(fnode-venvc fnode) ,*stackz*)
                      (PUSH)
                     )
                    (setq *stackz* (cons 1 *stackz*))
                ) )
              ,@(mapcap ; beim Aufbau mitzugebende Block-Conses
                  #'(lambda (block)
                      (prog1
                        `(,(if (member block (fnode-Blocks *func*) :test #'eq)
                             `(BCONST ,block)
                             `(GET ,(block-consvar block) ,*venvc* ,*stackz*)
                           )
                           (PUSH)
                         )
                        (setq *stackz* (cons 1 *stackz*))
                    ) )
                  (fnode-Blocks fnode)
                )
              ,@(mapcap ; beim Aufbau mitzugebende Tagbody-Conses
                  #'(lambda (tagbody)
                      (prog1
                        `(,(if (member tagbody (fnode-Tagbodys *func*) :test #'eq)
                             `(GCONST ,tagbody)
                             `(GET ,(tagbody-consvar tagbody) ,*venvc* ,*stackz*)
                           )
                           (PUSH)
                         )
                        (setq *stackz* (cons 1 *stackz*))
                    ) )
                  (fnode-Tagbodys fnode)
                )
              (COPY-CLOSURE ,fnode ,(fnode-keyword-offset fnode))
             )
          )
) )


;        ERSTER PASS :   S P E C I A L   F O R M S

; compiliere (PROGN {form}*)
; keine Formen -> NIL, genau eine Form -> diese Form,
; mindestens zwei Formen -> alle der Reihe nach, nur bei der letzten kommt es
; auf die Werte an.
(defun c-PROGN ()
  (test-list *form* 1)
  (let ((L (cdr *form*))) ; Liste der Formen
    (cond ((null L) (c-NIL)) ; keine Form -> NIL
          ((null (cdr L)) (c-form (car L))) ; genau eine Form
          (t (do (#+COMPILER-DEBUG (anodelist '())
                  (seclass '(NIL . NIL))
                  (codelist '())
                  (Lr L)) ; restliche Formenliste
                 ((null Lr)
                  (make-anode
                    :type 'PROGN
                    :sub-anodes (nreverse anodelist)
                    :seclass seclass
                    :code (nreverse codelist)
                 ))
               (let* ((formi (pop Lr)) ; i-te Form
                      (anodei (c-form formi (if (null Lr) *for-value* 'NIL))))
                 #+COMPILER-DEBUG (push anodei anodelist)
                 (seclass-or-f seclass anodei)
                 (push anodei codelist)
) ) )     )  ) )

; compiliere (PROG1 form1 {form}*)
; bei *for-value* muß der Wert von form1 im Stack gerettet werden
(defun c-PROG1 ()
  (test-list *form* 2)
  (if (or (null *for-value*) (and (eq *for-value* 'ONE) (null (cddr *form*))))
    (c-form `(PROGN ,@(cdr *form*)))
    (let ((anode1 (c-form (second *form*) 'ONE))
          (anode2 (let ((*stackz* (cons 1 *stackz*)))
                    (c-form `(PROGN ,@(cddr *form*)) 'NIL)
         ))       )
      (make-anode
        :type 'PROG1
        :sub-anodes (list anode1 anode2)
        :seclass (anodes-seclass-or anode1 anode2)
        :code `(,anode1 (PUSH) ,anode2 (POP))
) ) ) )

; compiliere (PROG2 form1 form2 {form}*)
(defun c-PROG2 ()
  (test-list *form* 3)
  (c-form `(PROGN ,(second *form*) (PROG1 ,(third *form*) ,@(cdddr *form*))))
)

; compiliere (IF form1 form2 [form3])
; ist form1 eine Konstante, so kann der Compiler die Fallunterscheidung treffen.
(defun c-IF ()
  (test-list *form* 3 4)
  (let ((form1 (second *form*))
        (form2 (third *form*))
        (form3 (fourth *form*))) ; = NIL, falls *form* nur 3 lang ist
    (let ((anode1 (c-form form1 'ONE)))
      (if (anode-constantp anode1)
        (if (anode-constant-value anode1)
          (prog1 (c-form form2) (let ((*no-code* t)) (c-form form3 'NIL)))
          (prog2 (let ((*no-code* t)) (c-form form2 'NIL)) (c-form form3))
        )
        (let ((anode2 (c-form form2))
              (anode3 (c-form form3))
              (label1 (make-label 'NIL))
              (label2 (make-label *for-value*)))
          (make-anode
            :type 'IF
            :sub-anodes (list anode1 anode2 anode3)
            :seclass (anodes-seclass-or anode1 anode2 anode3)
            :code
              `(,anode1
                (JMPIFNOT ,label1)
                ,anode2
                (JMP ,label2)
                ,label1
                ,anode3
                ,label2
               )
) ) ) ) ) )

; compiliere (WHEN form1 {form}*)
(defun c-WHEN ()
  (test-list *form* 2)
  (c-form `(IF ,(second *form*) (PROGN ,@(cddr *form*))))
)

; compiliere (UNLESS form1 {form}*)
(defun c-UNLESS ()
  (test-list *form* 2)
  (c-form `(IF ,(second *form*) NIL (PROGN ,@(cddr *form*))))
)

; compiliere (AND {form}*)
(defun c-AND ()
  (test-list *form* 1)
  (cond ((null (cdr *form*)) ; keine Formen
         (make-anode
           :type 'AND
           :sub-anodes '()
           :seclass '(NIL . NIL)
           :code '((T))
        ))
        ((null (cddr *form*)) (c-form (second *form*))) ; genau eine Form
        (t (do (#+COMPILER-DEBUG (anodelist '())
                (seclass '(NIL . NIL))
                (codelist '())
                (Lr (cdr *form*))
                (label (make-label *for-value*))) ; Label am Ende
               ((null Lr)
                (push label codelist)
                (make-anode
                  :type 'AND
                  :sub-anodes (nreverse anodelist)
                  :seclass seclass
                  :code (nreverse codelist)
               ))
             (let* ((formi (pop Lr))
                    (anodei (c-form formi (if (null Lr) *for-value* 'ONE))))
               #+COMPILER-DEBUG (push anodei anodelist)
               (seclass-or-f seclass anodei)
               (if (null Lr)
                 ; letzte Form -> direkt übernehmen
                 (push anodei codelist)
                 ; nicht letzte Form -> Test kreieren
                 (if (anode-constantp anodei)
                   ; Konstante /= NIL -> weglassen, Konstante NIL -> fertig
                   (unless (anode-constant-value anodei)
                     (if *for-value* (push '(NIL) codelist))
                     (let ((*no-code* t)) (dolist (form Lr) (c-form form 'NIL)))
                     (setq Lr nil)
                   )
                   (progn ; normaler Test
                     (push anodei codelist)
                     (push `(,(if *for-value* 'JMPIFNOT1 'JMPIFNOT) ,label)
                           codelist
             ) ) ) ) )
) )     )  )

; compiliere (OR {form}*)
(defun c-OR ()
  (test-list *form* 1)
  (cond ((null (cdr *form*)) ; keine Formen
         (make-anode
           :type 'OR
           :sub-anodes '()
           :seclass '(NIL . NIL)
           :code '((NIL))
        ))
        ((null (cddr *form*)) (c-form (second *form*))) ; genau eine Form
        (t (do (#+COMPILER-DEBUG (anodelist '())
                (seclass '(NIL . NIL))
                (codelist '())
                (Lr (cdr *form*))
                (label (make-label *for-value*))) ; Label am Ende
               ((null Lr)
                (push label codelist)
                (make-anode
                  :type 'OR
                  :sub-anodes (nreverse anodelist)
                  :seclass seclass
                  :code (nreverse codelist)
               ))
             (let* ((formi (pop Lr))
                    (anodei (c-form formi (if (null Lr) *for-value* 'ONE))))
               #+COMPILER-DEBUG (push anodei anodelist)
               (seclass-or-f seclass anodei)
               (if (null Lr)
                 ; letzte Form -> direkt übernehmen
                 (push anodei codelist)
                 ; nicht letzte Form -> Test kreieren
                 (if (anode-constantp anodei)
                   ; Konstante NIL -> weglassen, Konstante /= NIL -> fertig
                   (when (anode-constant-value anodei)
                     (if *for-value* (push anodei codelist))
                     (let ((*no-code* t)) (dolist (form Lr) (c-form form 'NIL)))
                     (setq Lr nil)
                   )
                   (progn ; normaler Test
                     (push anodei codelist)
                     (push `(,(if *for-value* 'JMPIF1 'JMPIF) ,label)
                           codelist
             ) ) ) ) )
) )     )  )

; compiliere (QUOTE object)
(defun c-QUOTE ()
  (test-list *form* 2 2)
  (let ((value (second *form*)))
    (make-anode :type 'QUOTE
                :sub-anodes '()
                :seclass '(NIL . NIL)
                :code (if *for-value* `((CONST ,(new-const value))) '() )
) ) )

; compiliere (THE type form)
(defun c-THE ()
  (test-list *form* 3 3)
  (c-form (third *form*)) ; ignoriere einfach die Typdeklaration
)

; compiliere (DECLARE {declspec}*)
(defun c-DECLARE ()
  (test-list *form* 1)
  (c-error #+DEUTSCH "Deklarationen sind an dieser Stelle nicht erlaubt: ~S"
           #+ENGLISH "Misplaced declaration: ~S"
           *form*
) )

; compiliere (CATCH tag {form}*)
(defun c-CATCH ()
  (test-list *form* 2)
  (let* ((anode1 (c-form (second *form*) 'ONE))
         (anode2 (let ((*stackz* (cons 'CATCH *stackz*)))
                   (c-form `(PROGN ,@(cddr *form*)))
         )       )
         (label (make-label *for-value*)))
    (make-anode :type 'CATCH
                :sub-anodes (list anode1 anode2)
                :seclass (anodes-seclass-or anode1 anode2)
                :code `(,anode1
                        (CATCH-OPEN ,label)
                        ,anode2
                        (CATCH-CLOSE)
                        ,label
) ) )                  )

; compiliere (THROW tag form)
(defun c-THROW ()
  (test-list *form* 3 3)
  (let* ((anode1 (c-form (second *form*) 'ONE))
         (anode2 (let ((*stackz* (cons 1 *stackz*)))
                   (c-form (third *form*) 'ALL)
        ))       )
    (make-anode :type 'THROW
                :sub-anodes (list anode1 anode2)
                :seclass (cons (car (anodes-seclass-or anode1 anode2)) 'T)
                :code `(,anode1 (PUSH) ,anode2 (THROW))
) ) )

; compiliere (UNWIND-PROTECT form1 {form}*)
(defun c-UNWIND-PROTECT ()
  (test-list *form* 2)
  (let* ((anode1 (let ((*stackz* (cons 'UNWIND-PROTECT *stackz*)))
                   (c-form (second *form*))
         )       )
         (anode2 (let ((*stackz* (cons 'CLEANUP *stackz*)))
                   (c-form `(PROGN ,@(cddr *form*)) 'NIL)
         )       )
         (label (make-label 'NIL)))
    (make-anode :type 'UNWIND-PROTECT
                :sub-anodes (list anode1 anode2)
                :seclass (anodes-seclass-or anode1 anode2)
                :code `((UNWIND-PROTECT-OPEN ,label)
                        ,anode1
                        ,@(case *for-value*
                            ((NIL) '((VALUES0)))
                            (ONE '((VALUES1)))
                            ((T) '())
                          )
                        (UNWIND-PROTECT-NORMAL-EXIT)
                        ,label
                        ,anode2
                        (UNWIND-PROTECT-CLOSE ,label)
) ) )                  )

; compiliere (PROGV form1 form2 {form}*)
(defun c-PROGV ()
  (test-list *form* 3)
  (let ((anode1 (c-form (second *form*) 'ONE)))
    ; falls form1 konstant=NIL ist, kann man sich das Binden sparen:
    (if (and (anode-constantp anode1) (null (anode-constant-value anode1)))
      (c-form `(PROGN ,(third *form*) (PROGN ,@(cdddr *form*))))
      (let* ((stackz2 (cons 1 *stackz*))
             (anode2 (let ((*stackz* stackz2))
                       (c-form (third *form*) 'ONE)
             )       )
             (stackz3 (cons 'PROGV *stackz*))
             (anode3 (let ((*stackz* stackz3))
                       (c-form `(PROGN ,@(cdddr *form*)))
             )       )
             (flag t))
        ; falls anode3 von keinen Seiteneffekten abhängig ist, kann man sich das
        ; Binden sparen:
        (when (null (car (anode-seclass anode3)))
          (setf (first stackz2) 0)
          (setf (first stackz3) 0)
          (setq flag nil)
        )
        (make-anode :type 'PROGV
                    :sub-anodes (list anode1 anode2 anode3)
                    :seclass (anodes-seclass-or anode1 anode2 anode3)
                    :code `(,anode1
                            ,@(if flag '((PUSH)))
                            ,anode2
                            ,@(if flag '((PROGV)))
                            ,anode3
                            ,@(if flag
                                `((UNWIND ,stackz3 ,*stackz* ,*for-value*))
                                ; wird expandiert zu '((UNBIND1) (SKIPSP 1))
                           )  )
) ) ) ) )

; compiliere (MULTIPLE-VALUE-PROG1 form1 {form}*)
; falls Werte nicht gebraucht werden: einfaches PROGN. Sonst: falls {form}*
; seiteneffektfrei, nur form1, sonst: Werte von form1 auf den Stack legen und
; nachher mit Funktion VALUES wieder einsammeln.
(defun c-MULTIPLE-VALUE-PROG1 ()
  (test-list *form* 2)
  (case *for-value*
    (ALL
     (let* ((stackz1 (cons 'MVCALLP *stackz*))
            (anode1 (let ((*stackz* stackz1))
                      (c-form (second *form*))
            )       )
            (anode2 (let ((*stackz* (cons 'MVCALL *stackz*)))
                      (c-form `(PROGN ,@(cddr *form*)) 'NIL)
           ))       )
       (make-anode :type 'MULTIPLE-VALUE-PROG1
                   :sub-anodes (list anode1 anode2)
                   :seclass (anodes-seclass-or anode1 anode2)
                   :code
                      (if (cdr (anode-seclass anode2))
                        `((CONST , #+CLISP (make-const :value #'values
                                                       :form '(function values)
                                           )
                                   #-CLISP (new-const 'values)
                          )
                          (MVCALLP)
                          ,anode1
                          (MV-TO-STACK)
                          ,anode2
                          (MVCALL))
                        (prog2 (setf (first stackz1) 0) `(,anode1))
                      )
    )) )
    (ONE (c-form `(PROG1 ,@(cdr *form*))))
    ((NIL) (c-form `(PROGN ,@(cdr *form*))))
) )

; compiliere (MULTIPLE-VALUE-CALL form1 {form}*)
(defun c-MULTIPLE-VALUE-CALL ()
  (test-list *form* 2)
  (if (null (cddr *form*))
    ; (c-form `(SYS::%FUNCALL ,(second *form*))) ; 0 Argumente zu form1
    (c-FUNCTION-CALL (second *form*) '())
    (let* ((anode1 (c-form (second *form*) 'ONE))
           #+COMPILER-DEBUG (anodelist (list anode1))
           (codelist '()))
      (push anode1 codelist)
      (push '(MVCALLP) codelist)
      (do ((Lr (cddr *form*))
           (i 0 (1+ i)))
          ((null Lr))
        (let* ((formi (pop Lr))
               (anodei
                 (let ((*stackz* (cons (if (zerop i) 'MVCALLP 'MVCALL) *stackz*)))
                   (c-form formi 'ALL)
              )) )
          #+COMPILER-DEBUG (push anodei anodelist)
          (push anodei codelist)
          (push '(MV-TO-STACK) codelist)
      ) )
      (push '(MVCALL) codelist)
      (make-anode :type 'MULTIPLE-VALUE-CALL
                  :sub-anodes (nreverse anodelist)
                  :seclass '(T . T)
                  :code (nreverse codelist)
) ) ) )

; compiliere (MULTIPLE-VALUE-LIST form)
(defun c-MULTIPLE-VALUE-LIST ()
  (test-list *form* 2 2)
  (if *for-value*
    (let ((anode1 (c-form (second *form*) 'ALL)))
      (make-anode :type 'MULTIPLE-VALUE-LIST
                  :sub-anodes (list anode1)
                  :seclass (anodes-seclass-or anode1)
                  :code `(,anode1 (MV-TO-LIST))
    ) )
    (c-form (second *form*))
) )

; compiliere (SETQ {symbol form}*)
; alle Zuweisungen nacheinander durchführen
(defun c-SETQ ()
  (test-list *form* 1)
  (when (evenp (length *form*))
    (c-error #+DEUTSCH "Ungerade viele Argumente zu SETQ: ~S"
             #+ENGLISH "Odd number of arguments to SETQ: ~S"
             *form*
  ) )
  (if (null (cdr *form*))
    (c-NIL) ; (SETQ) == (PROGN) == NIL
    (do ((L (cdr *form*) (cddr L))
         #+COMPILER-DEBUG (anodelist '())
         (seclass '(NIL . NIL))
         (codelist '()))
        ((null L)
         (make-anode
           :type 'SETQ
           :sub-anodes (nreverse anodelist)
           :seclass seclass
           :code (nreverse codelist)
        ))
      (let* ((symboli (first L))
             (formi (second L))
             (anodei (c-form formi 'ONE)))
        #+COMPILER-DEBUG (push anodei anodelist)
        (if (symbolp symboli)
          (progn
            (push anodei codelist)
            (seclass-or-f seclass anodei)
            (let ((setteri (c-VARSET symboli anodei)))
              (push setteri codelist)
              (seclass-or-f seclass setteri)
          ) )
          (progn
            (catch 'c-error
              (c-error #+DEUTSCH "Zuweisung auf ~S unmöglich, da kein Symbol."
                       #+ENGLISH "Cannot assign to non-symbol ~S."
                       symboli
            ) )
            (push '(VALUES1) codelist)
    ) ) ) )
) )

; compiliere (PSETQ {symbol form}*)
; alle Zwischenwerte auf dem Stack retten, erst dann zuweisen
(defun c-PSETQ ()
  (test-list *form* 1)
  (when (evenp (length *form*))
    (c-error #+DEUTSCH "Ungerade viele Argumente zu PSETQ: ~S"
             #+ENGLISH "Odd number of arguments to PSETQ: ~S"
             *form*
  ) )
  (if (null (cdr *form*))
    (c-NIL) ; (PSETQ) == (PROGN) == NIL
    (let ((anodelist '())
          (setterlist '()))
      ; Formen und Zuweisungen compilieren:
      (do ((L (cdr *form*)))
          ((null L))
        (let* ((symboli (pop L))
               (formi (pop L))
               (anodei (c-form formi 'ONE)))
          (if (symbolp symboli)
            (progn
              (push anodei anodelist)
              (push (c-VARSET symboli anodei) setterlist)
              (push 0 *stackz*)
            )
            (catch 'c-error
              (c-error #+DEUTSCH "Zuweisung auf ~S unmöglich, da kein Symbol."
                       #+ENGLISH "Cannot assign to non-symbol ~S."
                       symboli
      ) ) ) ) )
      ; Versuche, sie so zu reorganisieren, daß möglichst wenige (PUSH)
      ; und (POP) nötig werden:
      (let ((codelist1 '())
            (codelist2 '())
            ; baue codelist = (nconc codelist1 (nreverse codelist2)) zusammen
            (seclass '(NIL . NIL))) ; Seiteneffektklasse von codelist insgesamt
        (do ((anodelistr anodelist (cdr anodelistr))
             (setterlistr setterlist (cdr setterlistr)))
            ((null anodelistr))
          (let ((anode (car anodelistr))
                (setter (car setterlistr)))
            ; Normalerweise wäre vor codelist der anode und ein (PUSH)
            ; und nach codelist ein (POP) und der setter anzuhängen.
            ; Dies versuchen wir zu vereinfachen:
            (cond ((seclasses-commute (anode-seclass setter) seclass)
                   ; Ziehe den setter nach vorne:
                   (push setter codelist1)
                   (push anode codelist1)
                  )
                  ((seclasses-commute (anode-seclass anode) seclass)
                   ; Ziehe den anode nach hinten:
                   (push anode codelist2)
                   (push setter codelist2)
                  )
                  (t ; keine Vereinfachung möglich
                   (push '(PUSH) codelist1)
                   (push anode codelist1)
                   (push '(POP) codelist2)
                   (push setter codelist2)
                   (setf (car *stackz*) 1) ; brauche eine Variable im Stack
            )     )
            (setq seclass
              (seclass-or-2 seclass
                (seclass-or-2 (anode-seclass anode) (anode-seclass setter))
            ) )
            (setf *stackz* (cdr *stackz*))
        ) )
        ; *stackz* ist nun wieder auf dem alten Niveau.
        (when *for-value* (push '(NIL) codelist2))
        (make-anode
          :type 'PSETQ
          :sub-anodes (nreverse anodelist)
          :seclass seclass
          :code (nconc codelist1 (nreverse codelist2))
) ) ) ) )

; compiliere (MULTIPLE-VALUE-SETQ ({symbol}*) form)
; alle gewünschten Werte auf den Stack, dann einzeln herunternehmen und
; zuweisen.
(defun c-MULTIPLE-VALUE-SETQ ()
  (test-list *form* 3 3)
  (test-list (second *form*) 0)
  (let* ((n (length (second *form*)))
         (anode1 (c-form (third *form*) 'ALL))
         (*stackz* *stackz*))
    (if (zerop n)
      (make-anode :type 'MULTIPLE-VALUE-SETQ
                  :sub-anodes (list anode1)
                  :seclass (anodes-seclass-or anode1)
                  :code `(,anode1
                          ,@(if (eq *for-value* 'ALL) '((VALUES1)) '())
      )                  )
      (do ((L (second *form*) (cdr L))
           #+COMPILER-DEBUG (anodelist (list anode1))
           (seclass (anode-seclass anode1))
           (codelist '()))
          ((null L)
           (if (= n 1)
             (setq codelist (cdr codelist)) ; letztes (POP) streichen
             (setq codelist (cons `(NV-TO-STACK ,n) codelist))
           )
           (make-anode
             :type 'MULTIPLE-VALUE-SETQ
             :sub-anodes (nreverse anodelist)
             :seclass seclass
             :code (cons anode1 codelist)
          ))
        (let ((symbol (car L)))
          (if (symbolp symbol)
            (let ((setter (c-VARSET symbol
                            (make-anode :type 'NOP
                                        :sub-anodes '()
                                        :seclass '(NIL . NIL)
                                        :code '()
                 ))       ) )
              (push setter codelist)
              (seclass-or-f seclass setter)
            )
            (catch 'c-error
              (c-error #+DEUTSCH "Zuweisung auf ~S unmöglich, da kein Symbol."
                       #+ENGLISH "Cannot assign to non-symbol ~S."
                       symbol
        ) ) ) )
        (push '(POP) codelist)
        (push 1 *stackz*)
) ) ) )

; compiliere (LET/LET* ({var|(var value)}*) {declaration}* {form}*)
(defun c-LET/LET* (*-flag)
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (body-rest declarations)
      (parse-body (cddr *form*) nil *fenv*)
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*denv* *denv*)
          (*venv* *venv*)
          (*venvc* *venvc*))
      (multiple-value-bind (*specials* *ignores*) (process-declarations declarations)
        ; Special-Variable auf *venv* pushen:
        (push-specials)
        ; Syntaxtest der Parameterliste:
        (multiple-value-bind (symbols initforms) (analyze-letlist (second *form*))
          (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
          (let ((closuredummy-stackz *stackz*)
                (closuredummy-venvc *venvc*))
            (multiple-value-bind (varlist anodelist stackzlist)
                (process-movable-var-list symbols initforms *-flag)
              (unless *-flag (push 0 *stackz*)) ; Platz für Schluß-Bindungen
              (let ((body-anode (c-form `(PROGN ,@body-rest)))) ; Body compilieren
                ; Überprüfen der Variablen:
                (let* ((closurevars (checking-movable-var-list varlist anodelist))
                       (codelist
                         `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                           ,@(if *-flag
                               ; sequentielles Binden der Variablen
                               (mapcap #'c-bind-movable-var-anode varlist anodelist)
                               ; paralleles Binden der Variablen:
                               ; Variable darf erst am Schluß gebunden werden,
                               ; falls sie SPECIAL ist und nachfolgende Anodes
                               ; von ihrem Wert abhängen können.
                               (let ((bind-afterwards nil))
                                 (append
                                   (maplap
                                     #'(lambda (varlistr anodelistr stackzlistr)
                                         (let ((var (car varlistr))
                                               (anode (car anodelistr)))
                                           (if (and (var-specialp var)
                                                    (let ((symbol (var-name var)))
                                                      (some
                                                        #'(lambda (other-anode)
                                                            ; hängt der Wert von other-anode möglicherweise
                                                            ; vom Wert von var ab?
                                                            (let ((uses (car (anode-seclass other-anode))))
                                                              (or (eq uses 'T) (member symbol uses))
                                                          ) )
                                                        (cdr anodelistr)
                                               )    ) )
                                             (let* ((stackz (car stackzlistr))
                                                    (dummyvar ; Hilfsvariable im Stack
                                                      (make-var :name (gensym) :specialp nil
                                                                :closurep nil :stackz stackz
                                                   )) )
                                               (push (list dummyvar var (cdr *stackz*)) bind-afterwards)
                                               (push (car stackz) (cdr *stackz*)) ; Platz für 1 Schluß-Bindung mehr
                                               (setf (car stackz) 1) ; Platz für Hilfsvariable im Stack merken
                                               (c-bind-movable-var-anode dummyvar anode)
                                             )
                                             (c-bind-movable-var-anode var anode)
                                       ) ) )
                                     varlist anodelist stackzlist
                                   )
                                   (mapcap
                                     #'(lambda (bind)
                                         (let ((dummyvar (first bind)) ; Hilfsvariable im Stack
                                               (var (second bind)) ; SPECIAL-Variable
                                               (stackz (third bind))) ; Stackzustand vor Aufbau der Schluß-Bindung
                                           `((GET ,dummyvar ,*venvc* ,stackz)
                                             ,@(c-bind-movable-var var)
                                            )
                                       ) )
                                     (nreverse bind-afterwards)
                                   )
                             ) ) )
                           ,body-anode
                           (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                       )  )
                       (anode
                         (make-anode
                           :type (if *-flag 'LET* 'LET)
                           :sub-anodes `(,@anodelist ,body-anode)
                           :seclass (seclass-without
                                      (anodelist-seclass-or `(,@anodelist ,body-anode))
                                      varlist
                                    )
                           :stackz oldstackz
                           :code codelist
                      )) )
                  (when closurevars
                    (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
                    (setf (first closuredummy-venvc)
                      (cons closurevars closuredummy-stackz)
                  ) )
                  (optimize-var-list varlist)
                  anode
) ) ) ) ) ) ) ) )

; compiliere (MULTIPLE-VALUE-BIND ({var}*) form1 {declaration}* {form}*)
(defun c-MULTIPLE-VALUE-BIND ()
  (test-list *form* 3)
  (test-list (second *form*) 0)
  (let ((symbols (second *form*)))
    (dolist (sym symbols)
      (unless (symbolp sym)
        (c-error #+DEUTSCH "Nur Symbole können Variable sein, nicht ~S"
                 #+ENGLISH "Only symbols may be used as variables, not ~S"
                 sym
    ) ) )
    (if (= (length symbols) 1)
      (c-form `(LET ((,(first symbols) ,(third *form*))) ,@(cdddr *form*)))
      (multiple-value-bind (body-rest declarations)
          (parse-body (cdddr *form*) nil *fenv*)
        (let ((oldstackz *stackz*)
              (*stackz* *stackz*)
              (*denv* *denv*)
              (*venv* *venv*)
              (*venvc* *venvc*))
          (multiple-value-bind (*specials* *ignores*) (process-declarations declarations)
            ; Special-Variable auf *venv* pushen:
            (push-specials)
            (if (null symbols) ; leere Variablenliste -> gar nichts binden
              (let* ((anode1 (c-form (third *form*) 'NIL))
                     (anode2 (c-form `(PROGN ,@(cdddr *form*)))))
                (make-anode :type 'MULTIPLE-VALUE-BIND
                  :sub-anodes (list anode1 anode2)
                  :seclass (anodes-seclass-or anode1 anode2)
                  :code `(,anode1 ,anode2)
              ) )
              (let ((anode1 (c-form (third *form*) 'ALL)))
                (push nil *venvc*) ; Sichtbarkeit von Closure-Dummyvar
                (multiple-value-bind (varlist stackvarlist)
                    (process-fixed-var-list symbols)
                  (push 0 *stackz*) ; Platz für Closure-Dummyvar
                  (let* ((closuredummy-stackz *stackz*)
                         (closuredummy-venvc *venvc*)
                         (stackzlist
                           (do* ((varlistr varlist (cdr varlistr))
                                 (L '()))
                                ((null varlistr) (nreverse L))
                             (let ((var (car varlistr)))
                               (push var *venv*)
                               (push *stackz* L) (bind-fixed-var-2 var)
                         ) ) )
                         (body-anode ; Body compilieren
                           (c-form `(PROGN ,@body-rest))
                         )
                         ; Überprüfen der Variablen:
                         (closurevars (checking-fixed-var-list varlist))
                         (codelist ; Code generieren
                           `(,anode1
                             (NV-TO-STACK ,(length symbols))
                             ,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                             ,@ ; Binden von special- oder Closure-Variablen:
                               (do ((stackvarlistr stackvarlist (cdr stackvarlistr))
                                    (stackzlistr stackzlist (cdr stackzlistr))
                                    (varlistr varlist (cdr varlistr))
                                    (L '()))
                                   ((null varlistr) (nreverse L))
                                 (setq L
                                   (append
                                     (reverse
                                       (c-bind-fixed-var
                                         (car varlistr)
                                         (car stackvarlistr)
                                         (car stackzlistr)
                                     ) )
                                     L
                               ) ) )
                             ,body-anode
                             (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                         )  )
                         (anode
                           (make-anode
                             :type 'MULTIPLE-VALUE-BIND
                             :sub-anodes (list anode1 body-anode)
                             :seclass (seclass-without
                                        (anodes-seclass-or anode1 body-anode)
                                        varlist
                                      )
                             :stackz oldstackz
                             :code codelist
                        )) )
                    (when closurevars
                      (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
                      (setf (first closuredummy-venvc)
                        (cons closurevars closuredummy-stackz)
                    ) )
                    (optimize-var-list varlist)
                    anode
) ) ) ) ) ) ) ) ) )

; compiliere (COMPILER-LET ({var|(var value)}*) {form}*)
(defun c-COMPILER-LET ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (do ((L (second *form*) (cdr L))
       (varlist '())
       (valueslist '()))
      ((null L)
       (progv (nreverse varlist) (nreverse valueslist)
         (c-form `(PROGN ,@(cddr *form*)) )
      ))
    (cond ((symbolp (car L)) (push (car L) varlist) (push nil valueslist))
          ((and (consp (car L)) (symbolp (caar L)) (consp (cdar L)) (null (cddar L)))
           (push (caar L) varlist) (push (eval (cadar L)) valueslist))
          (t (catch 'c-error
               (c-error #+DEUTSCH "Falsche Syntax in COMPILER-LET: ~S"
                        #+ENGLISH "Illegal syntax in COMPILER-LET: ~S"
                        (car L)
    )     )  ) )
) )

(macrolet ((check-blockname (name)
             `(unless (symbolp ,name)
                (catch 'c-error
                  (c-error #+DEUTSCH "Blockname muß ein Symbol sein, nicht ~S"
                           #+ENGLISH "Block name must be a symbol, not ~S"
                           ,name
                ) )
                (setq ,name NIL) ; Default-Blockname
              )
          ))

; compiliere (BLOCK name {form}*)
(defun c-BLOCK ()
  (test-list *form* 2)
  (let ((name (second *form*)))
    (check-blockname name)
    (let* ((*stackz* (cons 'BLOCK *stackz*)) ; Block-Frame
           (label (make-label *for-value*))
           (block (make-block :fnode *func* :label label
                    :consvar (make-var :name (gensym) :specialp nil
                                       :closurep nil :stackz *stackz*
                             )
                    :stackz *stackz* :used-far nil :for-value *for-value*
           )      )
           (*benv* (cons (cons name block) *benv*)) ; Block aktivieren
           (anode (c-form `(PROGN ,@(cddr *form*))))
          )
      (if (block-used-far block)
        (make-anode :type 'BLOCK
                    :sub-anodes (list anode)
                    :seclass (anodes-seclass-or anode)
                    :code `((BLOCK-OPEN ,(new-const name) ,label)
                            ,anode
                            (BLOCK-CLOSE)
                            ,label
        )                  )
        (progn
          (setf (first *stackz*) 0) ; brauche keinen Blockframe
          (make-anode :type 'BLOCK
                      :sub-anodes (list anode)
                      :seclass (anodes-seclass-or anode)
                      :code `(,anode ,label)
) ) ) ) ) )

; compiliere (RETURN-FROM name [form])
(defun c-RETURN-FROM ()
  (test-list *form* 2 3)
  (let ((name (second *form*)))
    (check-blockname name)
    (let ((a (benv-search name)))
      (cond ((null a) ; dieser Blockname ist unsichtbar
             (c-error #+DEUTSCH "RETURN-FROM auf Block ~S an dieser Stelle nicht möglich."
                      #+ENGLISH "RETURN-FROM block ~S is impossible from here."
                      name
            ))
            ((block-p a) ; in *benv* ohne %benv% sichtbar
             (let ((anode (c-form (third *form*) (block-for-value a))))
               (if (eq (block-fnode a) *func*)
                 ; selbe Funktionen
                 (make-anode
                   :type 'RETURN-FROM
                   :sub-anodes (list anode)
                   :seclass '(T . T)
                   :code `(,anode
                           (UNWIND ,*stackz* ,(cdr (block-stackz a)) ,(block-for-value a))
                           (JMP ,(block-label a))
                 )        )
                 ; verschiedene Funktionen
                 (progn
                   (unless *no-code*
                     ; in alle dazwischenliegenden Funktionen diesen Block eintragen:
                     (do ((fnode *func* (fnode-enclosing fnode)))
                         ((eq fnode (block-fnode a)))
                       (pushnew a (fnode-blocks fnode))
                     )
                     (setf (block-used-far a) t)
                   )
                   (make-anode
                     :type 'RETURN-FROM
                     :sub-anodes (list anode)
                     :seclass '(T . T)
                     :code `(,anode
                             ,@(if (not (block-for-value a)) '((VALUES0)))
                             (RETURN-FROM ,a)
                   )        )
            )) ) )
            ((consp a) ; in %benv% sichtbar
             (let ((anode (c-form (third *form*) 'ALL)))
               (make-anode
                 :type 'RETURN-FROM
                 :sub-anodes (list anode)
                 :seclass '(T . T)
                 :code `(,anode
                         (RETURN-FROM ,(new-const a))
            )) )        )
            (t (compiler-error 'c-RETURN-FROM))
) ) ) )

) ; macrolet

; compiliere (TAGBODY {tag|form}*)
(defun c-TAGBODY ()
  (test-list *form* 1)
  (multiple-value-bind (taglist labellist)
    (do ((L (cdr *form*) (cdr L))
         (taglist '())
         (labellist '()))
        ((null L) (values (nreverse taglist) (nreverse labellist)))
      (let ((item (car L)))
        (if (atom item)
          (if (or (and (symbolp item) (not (null item))) (numberp item))
            ; Symbol NIL wird ausgeschlossen, weil zweideutig (ist auch Liste!).
            ; Andere Zahlen werden zugelassen, damit - ebenso wie 3.3.2 - auch
            ; 3.3 ein zulässiges Sprungziel ist.
            (progn
              (push item taglist)
              (push (make-label 'NIL) labellist)
            )
            (catch 'c-error
              (c-error #+DEUTSCH "Nur Zahlen und Symbole sind zulässige Sprungziele, nicht aber ~S"
                       #+ENGLISH "Only numbers and symbols are valid tags, not ~S"
                       item
    ) ) ) ) ) )
    (let* ((*stackz* (cons 0 *stackz*)) ; evtl. TAGBODY-Frame
           (tagbody (make-tagbody :fnode *func* :labellist labellist
                      :consvar (make-var :name (gensym) :specialp nil
                                         :closurep nil :stackz *stackz*
                               )
                      :stackz *stackz*
                      :used-far (make-array (length taglist) :fill-pointer 0)
           )        )
           (*genv* (cons (cons (apply #'vector taglist) tagbody) *genv*))
             ; Tagbody aktivieren
           (codelist '())
           #+COMPILER-DEBUG (anodelist '())
           (seclass '(NIL . NIL)))
      ; Inneres des Tagbody compilieren:
      (do ((formlistr (cdr *form*) (cdr formlistr))
           (taglistr taglist)
           (labellistr labellist))
          ((null formlistr)
           #+COMPILER-DEBUG (setq anodelist (nreverse anodelist))
           (setq codelist (nreverse codelist))
          )
        (let ((formi (car formlistr)))
          (if (atom formi)
            (when (and (consp taglistr) (eql formi (car taglistr)))
              ; Tag wiedergefunden
              (pop taglistr) (push (pop labellistr) codelist)
            )
            (let ((anodei (c-form formi 'NIL)))
              #+COMPILER-DEBUG (push anodei anodelist)
              (seclass-or-f seclass anodei)
              (push anodei codelist)
      ) ) ) )
      (if (> (length (tagbody-used-far tagbody)) 0)
        (let* ((used-tags (tagbody-used-far tagbody))
               (l (length used-tags))
               (used-label-list
                 (do ((i 0 (1+ i))
                      (l1 '()))
                     ((= i l) (nreverse l1))
                   (push
                     (elt labellist (position (aref used-tags i) taglist :test #'eql))
                     l1
              )) ) )
          (setf (first *stackz*) `(TAGBODY ,l))
          (setq codelist
            `((TAGBODY-OPEN ,l ,@used-label-list)
              ,@codelist
              (TAGBODY-CLOSE-NIL)
        ) )  )
        (when *for-value* (setq codelist `(,@codelist (NIL))))
      )
      (make-anode :type 'TAGBODY
                  :sub-anodes anodelist
                  :seclass seclass
                  :code codelist
) ) ) )

; compiliere (GO tag)
(defun c-GO ()
  (test-list *form* 2 2)
  (let ((tag (second *form*)))
    (unless (or (and (symbolp tag) (not (null tag))) (numberp tag))
      (c-error #+DEUTSCH "Sprungziel muß ein Symbol oder eine Zahl sein, nicht ~S"
               #+ENGLISH "Tag must be a symbol or a number, not ~S"
               tag
    ) )
    (multiple-value-bind (a b) (genv-search tag)
      (cond ((null a) ; dieser Tag ist unsichtbar
             (c-error #+DEUTSCH "GO auf Tag ~S an dieser Stelle nicht möglich."
                      #+ENGLISH "GO to tag ~S is impossible from here."
                      tag
            ))
            ((tagbody-p a) ; in *genv* ohne %genv% sichtbar
             (if (eq (tagbody-fnode a) *func*)
               ; selbe Funktionen
               (make-anode
                 :type 'GO
                 :sub-anodes '()
                 :seclass '(T . T)
                 :code `((UNWIND ,*stackz* ,(tagbody-stackz a) nil)
                         (JMP ,(nth b (tagbody-labellist a)))
               )        )
               ; verschiedene Funktionen
               (let ((index 0))
                 (unless *no-code*
                   (setq index
                     (do* ((v (tagbody-used-far a))
                           (l (length v))
                           (i 0 (1+ i)))
                          ((= i l) (vector-push tag v) l)
                       (if (eql (aref v i) tag) (return i))
                   ) )
                   ; (aref (tagbody-used-far a) index) = tag
                   ; in alle dazwischenliegenden Funktionen diesen Tagbody eintragen:
                   (do ((fnode *func* (fnode-enclosing fnode)))
                       ((eq fnode (tagbody-fnode a)))
                     (pushnew a (fnode-tagbodys fnode))
                 ) )
                 (make-anode
                   :type 'GO
                   :sub-anodes '()
                   :seclass '(T . T)
                   :code `((VALUES0) (GO ,a ,index))
                 )
            )) )
            ((consp a) ; in %genv% sichtbar
             (make-anode
               :type 'GO
               :sub-anodes '()
               :seclass '(T . T)
               :code `((GO ,(new-const a) ,b))
            ))
            (t (compiler-error 'c-GO))
) ) ) )

; compiliere (FUNCTION funname)
(defun c-FUNCTION ()
  (test-list *form* 2 3)
  (let* ((longp (cddr *form*)) ; Flag, ob Langform (FUNCTION name funname)
         (name (second *form*)))
    (if (and (not longp) (symbolp name))
      (multiple-value-bind (a b c) (fenv-search name)
        (case a
          ((NIL)
           (when *compiling-from-file* ; von COMPILE-FILE aufgerufen?
             (unless (or (fboundp name) (member name *known-functions* :test #'eq))
               (pushnew name *unknown-functions* :test #'eq)
           ) )
           (make-anode
             :type 'FUNCTION
             :sub-anodes '()
             :seclass '(T . NIL)
             :code (if (subr-info name)
                     `((CONST ,(make-const :value (symbol-function name)
                                           :form `(FUNCTION ,name)
                      ))       )
                     `((CONST ,(new-const name)) (SYMBOL-FUNCTION))
          ))       )
          (SYSTEM::MACRO
           (c-error #+DEUTSCH "~S ist keine Funktion, sondern ein lokal definierter Macro."
                    #+ENGLISH "~S is not a function. It is a locally defined macro."
                    name
          ))
          (GLOBAL ; gefunden in %fenv%
           (make-anode
             :type 'FUNCTION
             :sub-anodes '()
             :seclass '(T . NIL)
             :code `((CONST ,(new-const b))
                     (PUSH)
                     (CONST ,(new-const c))
                     (SVREF)
          ))        )
          (LOCAL ; gefunden in *fenv* ohne %fenv%
           (if (const-p b)
             (make-anode
               :type 'FUNCTION
               :sub-anodes '()
               :seclass '(NIL . NIL)
               :code `((FCONST ,(const-value b)))
             )
             (c-VAR (var-name b))
          ))
          (t (compiler-error 'c-FUNCTION))
      ) )
      (let ((funname (car (last *form*))))
        (if (and (consp funname) (eq (car funname) 'LAMBDA) (consp (cdr funname)))
          (let ((*no-code* (or *no-code* (null *for-value*))))
            (c-fnode-function
              (c-lambdabody
                (if (and longp (symbolp name))
                  name ; angegebener Funktionsname
                  (symbol-suffix (fnode-name *func*) (incf *anonymous-count*))
                )
                (cdr funname)
          ) ) )
          (c-error #+DEUTSCH "Nur Symbole und Lambda-Ausdrücke sind Namen von Funktionen, nicht ~S"
                   #+ENGLISH "Only symbols and lambda expressions are function names, not ~S"
                   funname
) ) ) ) ) )

(macrolet ((err-syntax (specform fdef)
             `(catch 'c-error
                (c-error #+DEUTSCH "Falsche Syntax einer Funktionsdefinition in ~S: ~S"
                         #+ENGLISH "Illegal function definition syntax in ~S: ~S"
                         ,specform ,fdef
              ) )
          ))

; compiliere (FLET ({fundef}*) {form}*)
(defun c-FLET ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (namelist fnodelist)
      (do ((fdefsr (second *form*) (cdr fdefsr))
           (L1 '())
           (L2 '()))
          ((null fdefsr) (values (nreverse L1) (nreverse L2)))
        (let ((fdef (car fdefsr)))
          (if (and (consp fdef) (symbolp (car fdef)) (consp (cdr fdef)))
            (let ((fnode (c-lambdabody
                           (symbol-suffix (fnode-name *func*) (car fdef))
                           (cdr fdef)
                 ))      )
              (push (car fdef) L1)
              (push fnode L2)
            )
            (err-syntax 'FLET fdef)
      ) ) )
    ; namelist = Liste der Namen, fnodelist = Liste der fnodes der Funktionen
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*venvc* *venvc*)
          (*venv* *venv*))
      (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
      (let ((closuredummy-stackz *stackz*)
            (closuredummy-venvc *venvc*))
        (multiple-value-bind (varlist anodelist *fenv*)
            (do ((namelistr namelist (cdr namelistr))
                 (fnodelistr fnodelist (cdr fnodelistr))
                 (varlist '())
                 (anodelist '())
                 (fenv '()))
                ((null namelistr)
                 (values (nreverse varlist) (nreverse anodelist)
                         (apply #'vector (nreverse (cons *fenv* fenv)))
                ))
              (push (car namelistr) fenv)
              (let ((fnode (car fnodelistr)))
                (if (zerop (fnode-keyword-offset fnode))
                  ; Funktionsdefinition ist autonom
                  (push (cons (list fnode) (make-const :value fnode)) fenv)
                  (progn
                    (push (c-fnode-function fnode) anodelist)
                    (push 1 *stackz*)
                    (let ((var (make-var :name (gensym) :specialp nil
                                 :constantp nil :usedp t :really-usedp nil
                                 :closurep nil ; später evtl. auf T gesetzt
                                 :stackz *stackz* :venvc *venvc*
                         ))    )
                      (push (cons (list fnode) var) fenv)
                      (push var varlist)
            ) ) ) ) )
          (dolist (var varlist) (push var *venv*)) ; Hilfsvariablen aktivieren
          (let* ((body-anode ; restliche Formen compilieren
                   (c-form `(PROGN ,@(cddr *form*)))
                 )
                 (closurevars (checking-movable-var-list varlist anodelist))
                 (anode
                   (make-anode
                     :type 'FLET
                     :sub-anodes `(,@anodelist ,body-anode)
                     :seclass (seclass-without
                                (anodelist-seclass-or `(,@anodelist ,body-anode))
                                varlist
                              )
                     :code `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                             ,@(mapcap #'c-bind-movable-var-anode varlist anodelist)
                             ,body-anode
                             (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                   )        )
                ))
            (when closurevars
              (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
              (setf (first closuredummy-venvc)
                (cons closurevars closuredummy-stackz)
            ) )
            (optimize-var-list varlist)
            anode
) ) ) ) ) )

; compiliere (LABELS ({fundef}*) {form}*)
(defun c-LABELS ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((oldstackz *stackz*)
        (*stackz* *stackz*)
        (*venvc* *venvc*)
        (*venv* *venv*))
    (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
    (let ((closuredummy-stackz *stackz*)
          (closuredummy-venvc *venvc*))
      (multiple-value-bind (namelist varlist lambdanamelist lambdabodylist fenvconslist)
          (do ((fdefsr (second *form*) (cdr fdefsr))
               (L1 '())
               (L2 '())
               (L3 '())
               (L4 '())
               (L5 '()))
              ((null fdefsr)
               (values (nreverse L1) (nreverse L2) (nreverse L3) (nreverse L4) (nreverse L5))
              )
            (let ((fdef (car fdefsr)))
              (if (and (consp fdef) (symbolp (car fdef)) (consp (cdr fdef)))
                (progn
                  (push (car fdef) L1)
                  (push 1 *stackz*)
                  (push (make-var :name (gensym) :specialp nil
                                  :constantp nil :usedp t :really-usedp nil
                                  :closurep nil ; später evtl. auf T gesetzt
                                  :stackz *stackz* :venvc *venvc*
                        )
                        L2
                  )
                  (push (symbol-suffix (fnode-name *func*) (car fdef)) L3)
                  (push (cdr fdef) L4)
                  (push
                    (cons
                      ; fdescr, bestehend aus:
                      (cons nil ; Platz für den FNODE
                            (multiple-value-list ; Werten von analyze-lambdalist
                              (analyze-lambdalist (cadr fdef))
                      )     )
                      ; Variable
                      (car L2)
                    )
                    L5
                ) )
                (err-syntax 'LABELS fdef)
          ) ) )
        ; namelist = Liste der Namen, varlist = Liste der Variablen,
        ; lambdanamelist = Liste der Dummynamen der Funktionen,
        ; lambdabodylist = Liste der Lambdabodys der Funktionen,
        ; fenvconslist = Liste der Conses (fdescr . var) für *fenv*
        ; (jeweils fdescr noch ohne den fnode, der kommt erst später hinein).
        (let ((*fenv* ; Funktionsnamen aktivieren
                (do ((namelistr namelist (cdr namelistr))
                     (fenvconslistr fenvconslist (cdr fenvconslistr))
                     (L nil))
                    ((null namelistr)
                     (push *fenv* L)
                     (apply #'vector (nreverse L))
                    )
                  (push (car namelistr) L)
                  (push (car fenvconslistr) L)
             )) )
          (dolist (var varlist) (push var *venv*)) ; Hilfsvariablen aktivieren
          (let* ((fnodelist ; Funktionen compilieren
                   (mapcar #'c-lambdabody lambdanamelist lambdabodylist fenvconslist)
                 )
                 (anodelist
                   (mapcar #'(lambda (fnode var)
                               (c-fnode-function fnode (cdr (var-stackz var)))
                             )
                           fnodelist varlist
                 ) )
                 (body-anode ; restliche Formen compilieren
                   (c-form `(PROGN ,@(cddr *form*)))
                ))
            ; die Variablen, zu denen die Funktion autonom war, werden nach-
            ; träglich zu Konstanten erklärt:
            (do ((varlistr varlist (cdr varlistr))
                 (fnodelistr fnodelist (cdr fnodelistr)))
                ((null varlistr))
              (let ((var (car varlistr))
                    (fnode (car fnodelistr)))
                (when (zerop (fnode-keyword-offset fnode))
                  ; Funktionsdefinition ist autonom
                  (setf (var-constantp var) t)
                  (setf (var-constant-value var) fnode)
            ) ) )
            (let* ((closurevars (checking-movable-var-list varlist anodelist))
                   (anode
                     (make-anode
                       :type 'LABELS
                       :sub-anodes `(,@anodelist ,body-anode)
                       :seclass (seclass-without
                                  (anodelist-seclass-or `(,@anodelist ,body-anode))
                                  varlist
                                )
                       :code `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                               ,@(mapcap #'c-bind-movable-var-anode varlist anodelist)
                               ,body-anode
                               (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                     )        )
                  ))
              (when closurevars
                (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
                (setf (first closuredummy-venvc)
                  (cons closurevars closuredummy-stackz)
              ) )
              (optimize-var-list varlist)
              anode
) ) ) ) ) ) )

) ; macrolet

; compiliere (MACROLET ({macrodef}*) {form}*)
(defun c-MACROLET ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (do ((L1 (second *form*) (cdr L1))
       (L2 '()))
      ((null L1)
       (push *fenv* L2)
       (let ((*fenv* (apply #'vector (nreverse L2)))) ; *fenv* erweitern
         (c-form `(PROGN ,@(cddr *form*))) ; restliche Formen compilieren
      ))
    (let* ((macrodef (car L1))
           (expander #+CLISP (eval (sys::make-macro-expansion macrodef))
                     #-CLISP (make-macro-expander macrodef)
           )
           (name (car macrodef)))
      (push name L2)
      (push (cons 'SYSTEM::MACRO expander) L2)
) ) )

; compiliere (EVAL-WHEN ({situation}*) {form}*)
(defun c-EVAL-WHEN ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((load-flag nil)
        (compile-flag nil))
    (dolist (situation (second *form*))
      (case situation
        (LOAD (setq load-flag t))
        (COMPILE (setq compile-flag t))
        (EVAL)
        (T (c-error #+DEUTSCH "Situation bei EVAL-WHEN muß EVAL, LOAD oder COMPILE sein, nicht ~S."
                    #+ENGLISH "EVAL-WHEN situation must be EVAL or LOAD or COMPILE, but not ~S"
                    situation
    ) ) )  )
    (let ((form `(PROGN ,@(cddr *form*))))
      (when compile-flag (c-eval-when-compile form))
      (if load-flag (c-form form) (c-NIL))
) ) )

; compiliere (COND {clause}*)
(defun c-COND ()
  (test-list *form* 1)
  (c-form
    (let ((clauses (cdr *form*))) ; (COND . clauses) macroexpandieren
      (if (null clauses)
        'NIL
        (let ((clause (car clauses)))
          (if (atom clause)
            (c-error #+DEUTSCH "COND-Klausel ohne Test: ~S"
                     #+ENGLISH "COND clause without test: ~S"
                     clause
            )
            (let ((test (car clause)))
              (if (cdr clause)
                `(IF ,test (PROGN ,@(cdr clause)) (COND ,@(cdr clauses)))
                `(OR ,test (COND ,@(cdr clauses)))
) ) ) ) ) ) ) )


;               ERSTER PASS :   M A C R O S

; compiliere (CASE keyform {clause}*)
(defun c-CASE ()
  (test-list *form* 1)
  (let ((keyform (second *form*))
        (clauses (cddr *form*))
        ; clauses vereinfachen:
        (newclauses '())
        (allkeys '()))
    (let ((default-passed nil))
      (dolist (clause clauses)
        (if (atom clause)
          (c-error #+DEUTSCH "CASE-Klausel ohne Objekte: ~S"
                   #+ENGLISH "CASE clause without objects: ~S"
                   clause
          )
          (let ((keys (car clause)))
            (if default-passed ; war der Default schon da?
              (setq keys nil)
              (if (or (eq keys 'T) (eq keys 'OTHERWISE))
                (setq keys 'T default-passed t)
                (let ((newkeys '()))
                  (dolist (key (if (listp keys) keys (list keys)))
                    (unless (member key allkeys :test #'eq) ; remove-duplicates
                      (push key allkeys) (push key newkeys)
                  ) )
                  (setq keys (nreverse newkeys))
            ) ) )
            (push (cons keys (cdr clause)) newclauses)
      ) ) )
      (unless default-passed (push '(T NIL) newclauses))
      (setq newclauses (nreverse newclauses))
      (setq allkeys (nreverse allkeys))
    )
    ; newclauses enthält jetzt keine doppelten keys, genau einmal T als keys,
    ; und allkeys ist die Menge aller Keys.
    (if (<= (length allkeys) 2) ; wenige Keys -> direkt EQL verwenden
      (let ((keyvar (gensym)))
        (labels ((ifify (clauses)
                   (if (null clauses)
                     'NIL
                     `(IF ,(let ((keys (caar clauses)))
                             (if (atom keys) ; keys = T, der Default-Fall?
                               'T
                               `(OR ,@(mapcar
                                        #'(lambda (key) `(EQL ,keyvar ',key))
                                        keys
                                )     )
                           ) )
                        (PROGN ,@(cdar clauses))
                        ,(ifify (cdr clauses))
                      )
                )) )
          (c-form
            `(LET ((,keyvar ,keyform)) (PROGN ,keyvar ,(ifify newclauses)))
      ) ) )
      (let ((keyform-anode (c-form keyform 'ONE))
            (default-anode nil)
            (cases '())) ; Liste von Tripeln (keylist label anode)
        (dolist (clause newclauses)
          (if (car clause)
            (let ((anode (c-form `(PROGN ,@(cdr clause)))))
              (if (atom (car clause))
                (setq default-anode anode)
                (push (list (car clause) (make-label 'NIL) anode) cases)
            ) )
            (let ((*no-code* t)) (c-form `(PROGN ,@(cdr clause)) 'NIL))
        ) )
        (setq cases (nreverse cases))
        (if (anode-constantp keyform-anode)
          (let ((value (anode-constant-value keyform-anode)))
            (dolist (case cases default-anode)
              (when (member value (first case) :test #'eql)
                (return (third value))
          ) ) )
          (let ((default-label (make-label 'NIL))
                (end-label (make-label *for-value*))
                (test (if (every #'EQL=EQ allkeys) 'EQ 'EQL)))
            (make-anode
              :type 'CASE
              :sub-anodes `(,keyform-anode ,@(mapcar #'third cases) ,default-anode)
              :seclass
                (anodelist-seclass-or
                  `(,keyform-anode ,@(mapcar #'third cases) ,default-anode)
                )
              :code
                `(,keyform-anode
                  (JMPHASH
                    ,test
                    ,(mapcap ; Aliste (obji -> labeli)
                       #'(lambda (case)
                           (let ((label (second case)))
                             (mapcar #'(lambda (obj) (cons obj label))
                                     (first case)
                         ) ) )
                       cases
                     )
                    ,default-label
                    ,@(mapcar #'second cases) ; alle Labels, ohne Doppelte
                  )
                  ,@(mapcap
                      #'(lambda (case)
                          `(,(second case) ; Label
                            ,(third case) ; Anode
                            (JMP ,end-label)
                           )
                        )
                      cases
                    )
                  ,default-label
                  ,default-anode
                  ,end-label
                 )
          ) )
) ) ) ) )


;   ERSTER PASS :   I N L I N E - F U N K T I O N E N   (PRIMOPS)

; Funktionsaufrufe, die wie special forms behandelt werden:

; Erst FUNCALL bzw. SYS::%FUNCALL.

; (c-FUNCALL-NOTINLINE funform args) compiliert einen Funktionsaufruf
; (SYS::%FUNCALL funform . args),
; für den das STACK-Layout der Argumente nicht zur Compile-Zeit bestimmt
; werden kann.
(defun c-FUNCALL-NOTINLINE (funform args)
  (test-list args 0)
  (let* ((anode1 (c-form funform 'ONE))
         (*stackz* (cons 1 *stackz*)))
    (do ((formlistr args (cdr formlistr))
         #+COMPILER-DEBUG (anodelist (list anode1))
         (codelist (list '(FUNCALLP) anode1)))
        ((null formlistr)
         (push `(FUNCALL ,(length args)) codelist)
         (make-anode
           :type 'FUNCALL
           :sub-anodes (nreverse anodelist)
           :seclass '(T . T)
           :code (nreverse codelist)
        ))
      (let ((anode (c-form (car formlistr) 'ONE)))
        #+COMPILER-DEBUG (push anode anodelist)
        (push anode codelist)
      )
      (push '(PUSH) codelist)
      (push 1 *stackz*)
) ) )

; (c-FUNCALL-INLINE funform args lambdabody sameenv) compiliert einen
; Funktionsaufruf (SYS::%FUNCALL funform . args),
; für den das STACK-Layout der Argumente zur Compile-Zeit bestimmt werden kann.
; sameenv gibt an, ob lambdabody im selben Environment oder im
; Top-Level-Environment zu betrachten ist.
(defun c-FUNCALL-INLINE (funform arglist lambdabody sameenv)
  (test-list lambdabody 1)
  (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
                        keyflag keyword keyvar keyinit keysvar allow-other-keys
                        auxvar auxinit)
      (analyze-lambdalist (pop lambdabody))
    (when (or keyflag keyword keyvar keyinit keysvar allow-other-keys)
      (compiler-error 'c-FUNCALL-INLINE)
    )
    (let ((r (length reqvar)) ; Anzahl der required-Argumente
          (s (length optvar)) ; Anzahl der optionalen Argumente
          (|t| (length arglist))) ; Anzahl der angegebenen Argumente
      (when (and (null restvar) (> |t| (+ r s)))
        ; zu viele Argumente angegeben. Wird beseitigt durch Einführung
        ; mehrerer zusätzlicher optionaler Argumente:
        (catch 'c-error
          (c-error #+DEUTSCH "Zuviele Argumente für ~S"
                   #+ENGLISH "Too many arguments to ~S"
                   funform
        ) )
        (dotimes (i (- |t| (+ r s)))
          (let ((var (gensym)))
            (setq optvar (append optvar (list var)))
            (setq optinit (append optinit (list nil)))
            (setq optsvar (append optsvar (list nil)))
            (incf s)
            (push `(DECLARE (IGNORE ,var)) lambdabody)
      ) ) )
      (when (< |t| r)
        ; zu wenige Argumente angegeben. Wird beseitigt durch Einführung
        ; zusätzlicher Argumente:
        (catch 'c-error
          (c-error #+DEUTSCH "Zuwenig Argumente für ~S"
                   #+ENGLISH "Too few arguments to ~S"
                   funform
        ) )
        (setq arglist (append arglist (make-list (- r |t|) :initial-element nil)))
        (setq |t| r)
      )
      ; Nun ist t>=r und (t<=r+s oder &rest-Parameter da).
      (let ((oldstackz *stackz*)
            (oldvenv *venv*)
            (oldfenv *fenv*)
            (oldbenv *benv*)
            (oldgenv *genv*)
            (olddenv *denv*)
            (*stackz* *stackz*)
            (*venv* (and sameenv *venv*))
            (*venvc* *venvc*)
            (*fenv* (and sameenv *fenv*))
            (*benv* (and sameenv *benv*))
            (*genv* (and sameenv *genv*))
            (*denv* (if sameenv
                      *denv*
                      (cons `(INLINING ,funform)
                            (remove-if-not #'(lambda (declspec) (eq (car declspec) 'INLINING))
                                           *denv*
           ))       ) )     )
        (multiple-value-bind (body-rest declarations)
            (parse-body lambdabody t *fenv*)
          (let (*specials* *ignores*
                req-vars req-anodes
                opt-vars opt-anodes opts-vars opts-anodes opt-all
                rest-vars rest-anodes
                aux-vars aux-anodes
                closuredummy-stackz closuredummy-venvc
               )
            (multiple-value-setq (*specials* *ignores*)
              (process-declarations declarations))
            ; Special-Variable auf *venv* pushen:
            (push-specials)
            (push 0 *stackz*) (push nil *venvc*) ; Platz für Closure-Dummyvar
            (setq closuredummy-stackz *stackz* closuredummy-venvc *venvc*)
            ; required-Parameter binden:
            (do ((reqvarr reqvar (cdr reqvarr)))
                ((null reqvarr)
                 (setq req-vars (nreverse req-vars))
                 (setq req-anodes (nreverse req-anodes))
                )
              (let* ((form (pop arglist))
                     (anode (let ((*venv* oldvenv)
                                  (*fenv* oldfenv)
                                  (*benv* oldbenv)
                                  (*genv* oldgenv)
                                  (*denv* olddenv))
                              (c-form form 'ONE)
                     )      )
                     (var (bind-movable-var (car reqvarr) anode)))
                (push anode req-anodes)
                (push var req-vars)
                (push var *venv*)
            ) )
            ; optionale Parameter und Svars binden:
            (do ((optvarr optvar (cdr optvarr))
                 (optinitr optinit (cdr optinitr))
                 (optsvarr optsvar (cdr optsvarr)))
                ((null optvarr)
                 (setq opt-vars (nreverse opt-vars))
                 (setq opt-anodes (nreverse opt-anodes))
                 (setq opts-vars (nreverse opts-vars))
                 (setq opts-anodes (nreverse opts-anodes))
                 (setq opt-all (nreverse opt-all))
                )
              (let* ((svar-init (not (null arglist))) ; = NIL oder T
                     (anode (if svar-init
                              (progn
                                (let ((*no-code* t))
                                  (c-form (car optinitr) 'NIL)
                                )
                                (let ((*venv* oldvenv)
                                      (*fenv* oldfenv)
                                      (*benv* oldbenv)
                                      (*genv* oldgenv)
                                      (*denv* olddenv))
                                  (c-form (pop arglist) 'ONE)
                              ) )
                              (c-form (car optinitr) 'ONE)
                     )      )
                     (var (bind-movable-var (car optvarr) anode)))
                (push anode opt-anodes)
                (push var opt-vars)
                (push var *venv*)
                (push
                  (cons (list var anode)
                    (if (eql (car optsvarr) 0)
                      nil
                      (let* ((anode (c-form svar-init 'ONE))
                             (var (bind-movable-var (car optsvarr) anode)))
                        (push anode opts-anodes)
                        (push var opts-vars)
                        (push var *venv*)
                        (list var anode)
                  ) ) )
                  opt-all
            ) ) )
            ; Rest-Parameter binden:
            (unless (eql restvar 0)
              (let* ((form (if arglist `(LIST ,@arglist) 'NIL))
                     (anode (let ((*venv* oldvenv)
                                  (*fenv* oldfenv)
                                  (*benv* oldbenv)
                                  (*genv* oldgenv)
                                  (*denv* olddenv))
                              (c-form form 'ONE)
                     )      )
                     (var (bind-movable-var restvar anode)))
                (push anode rest-anodes)
                (push var rest-vars)
                (push var *venv*)
            ) )
            ; Bindungen der Aux-Variablen aktivieren:
            (do ((auxvarr auxvar (cdr auxvarr))
                 (auxinitr auxinit (cdr auxinitr)))
                ((null auxvarr)
                 (setq aux-vars (nreverse aux-vars))
                 (setq aux-anodes (nreverse aux-anodes))
                )
              (let* ((form (car auxinitr))
                     (anode (c-form form 'ONE))
                     (var (bind-movable-var (car auxvarr) anode)))
                (push anode aux-anodes)
                (push var aux-vars)
                (push var *venv*)
            ) )
            (let* ((body-anode (c-form `(PROGN ,@body-rest)))
                   ; Überprüfen der Variablen:
                   (varlist (append req-vars opt-vars opts-vars rest-vars aux-vars))
                   (closurevars
                     (append
                       (checking-movable-var-list req-vars req-anodes)
                       (checking-movable-var-list opt-vars opt-anodes)
                       (checking-movable-var-list opts-vars opts-anodes)
                       (checking-movable-var-list rest-vars rest-anodes)
                       (checking-movable-var-list aux-vars aux-anodes)
                   ) )
                   (codelist
                     `(,@(c-make-closure closurevars closuredummy-venvc closuredummy-stackz)
                       ,@(mapcap #'c-bind-movable-var-anode req-vars req-anodes)
                       ,@(mapcap #'(lambda (opt-both)
                                     (append (apply #'c-bind-movable-var-anode (car opt-both))
                                             (if (cdr opt-both)
                                               (apply #'c-bind-movable-var-anode (cdr opt-both))
                                               '()
                                   ) )       )
                                 opt-all
                         )
                       ,@(mapcap #'c-bind-movable-var-anode rest-vars rest-anodes)
                       ,@(mapcap #'c-bind-movable-var-anode aux-vars aux-anodes)
                       ,body-anode
                       (UNWIND ,*stackz* ,oldstackz ,*for-value*)
                   )  )
                   (anode
                     (make-anode
                       :type 'FUNCALL
                       :sub-anodes
                         `(,@req-anodes ,@opt-anodes ,@opts-anodes
                           ,@rest-anodes ,@aux-anodes ,body-anode)
                       :seclass
                         (seclass-without
                           (anodelist-seclass-or
                             `(,@req-anodes ,@opt-anodes ,@opts-anodes
                               ,@rest-anodes ,@aux-anodes ,body-anode
                           )  )
                           varlist
                         )
                       :stackz oldstackz
                       :code codelist
                  )) )
              (when closurevars
                (setf (first closuredummy-stackz) 1) ; 1 Stackplatz für Dummy
                (setf (first closuredummy-venvc)
                  (cons closurevars closuredummy-stackz)
              ) )
              (optimize-var-list varlist)
              anode
) ) ) ) ) ) )

; compiliert (fun {form}*), wobei fun eine lokale Funktion ist.
; fdescr die zugehörige Information aus *fenv*.
(defun c-LOCAL-FUNCTION-CALL (fun fdescr args)
  ; (test-list args 0) ; das erledigt gleich (test-argument-syntax ...)
  (let ((fnode (car fdescr)))
    ; Aufruf-Spezifikation holen:
    (multiple-value-bind (req opt rest-flag key-flag keylist allow-flag)
        (if (cdr fdescr)
          ; bei LABELS: aus der Lambdalisten-Information
          (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
                                keyflag keyword keyvar keyinit keysvar allow-other-keys
                                auxvar auxinit)
              (values-list (cdr fdescr))
            (declare (ignore optinit optsvar keyvar keyinit keysvar auxvar auxinit))
            (values (length reqvar) (length optvar)
                    (not (eql restvar 0)) keyflag
                    keyword allow-other-keys
          ) )
          ; bei FLET: aus dem fnode
          (values (fnode-req-anz fnode) (fnode-opt-anz fnode)
                  (fnode-rest-flag fnode) (fnode-keyword-flag fnode)
                  (fnode-keywords fnode) (fnode-allow-other-keys-flag fnode)
        ) )
      (case (test-argument-syntax
              args fun req opt rest-flag key-flag keylist allow-flag
            )
        ((NO-KEYS STATIC-KEYS)
         ; Aufruf INLINE
         (c-DIRECT-FUNCTION-CALL
           args fun req opt rest-flag key-flag keylist
           nil ; kein SUBR-, sondern Cclosure-Aufruf
           (if (eq fnode *func*)
             ; rekursiver Aufruf der eigenen Funktion
             (let ((call-code
                     `((JSR ,(+ req opt (if rest-flag 1 0) (length keylist)) ; Zahl der Stack-Einträge
                            ,*func-start-label*
                      ))
                  ))
               #'(lambda () call-code)
             )
             ; eine andere Cclosure aufrufen
             #'(lambda ()
                 (list
                   (c-form `(FUNCTION ,fun) 'ONE)
                   (if key-flag '(CALLCKEY) '(CALLC))
               ) )
        )) )
        (t (c-FUNCALL-NOTINLINE `(FUNCTION ,fun) args))
) ) ) )

; (c-FUNCTION-CALL funform arglist) compiliert einen Funktionsaufruf
; (SYS::%FUNCALL funform . arglist).
(defun c-FUNCTION-CALL (funform arglist)
  (if (inline-callable-function-p funform (length arglist))
    ; Aufruf eines Lambda-Ausdrucks INLINE möglich
    (c-FUNCALL-INLINE funform arglist (cdr (second funform)) t)
    (if (and (consp funform) (eq (first funform) 'FUNCTION)
             ; Ausdrücke der Form (FUNCTION ...) dürfen zu beliebigem
             ; Zeitpunkt ausgewertet werden, also ist
             ; (SYS::%FUNCALL (FUNCTION fun) . arglist)  äquivalent zu
             ; (fun . arglist).
             (consp (rest funform)) (symbolp (second funform)) ; vorerst nur #'sym, sonst Endlosschleife!
        )
      (progn
        (test-list funform 2 2)
        (c-form `(,(second funform) ,@arglist)) ; genauer aufschlüsseln, vgl. c-FUNCTION ??
      )
      ; Aufruf NOTINLINE
      (c-FUNCALL-NOTINLINE funform arglist)
) ) )

(defun c-FUNCALL ()
  (test-list *form* 2)
  (c-FUNCTION-CALL (second *form*) (cddr *form*))
)

(defun c-APPLY ()
  (test-list *form* 3)
  #|
  (when (and (consp (second *form*))
             (eq (first (second *form*)) 'FUNCTION)
        )
    (test-list (second *form*) 2 2)
    (let ((fun (second (second *form*))))
      (when (symbolp fun)
        (multiple-value-bind (a b c) (fenv-search (second (second *form*)))
          (declare (ignore b))
          ; (APPLY #'localfun . args) kann evtl. vereinfacht werden
          (when (and (eq a 'LOCAL)
                     (fnode-rest-flag c)          ; Funktion mit &REST-Arg.
                     (not (fnode-keyword-flag c)) ; und ohne &KEY-Args
                )
            (let* ((opt (fnode-opt-anz c))
                   (reqopt (+ (fnode-req-anz c) opt))
                   (n (- (length *form*) 3))
                  )
              (cond ((eql n reqopt) ; genau passende Zahl einzelner Argumente:
                                    ; Argumente und Restliste können direkt im
                                    ; STACK abgelegt werden
                      (let ((args (cddr *form*))
                            (*stackz* *stackz*)
                            (codelist '())
                           )
                        (dotimes (i (1+ n))
                          (push (c-form (pop args) 'ONE) codelist)
                          (push '(PUSH) codelist)
                          (push 1 *stackz*)
                        )
                        (if (eq c *func*) ; selbe Funktion oder nicht?
                          (push `(JSR ,(1+ n) ,*func-start-label*) codelist)
                          (progn (push (c-form `(FUNCTION ,fun) 'ONE) codelist)
                                 (push '(CALLC) codelist)
                        ) )
                        (return-from c-apply
                          (make-anode :type 'APPLY
                                      :sub-anodes ??
                                      :seclass '(T . T)
                                      :code (nreverse codelist)
                    ) ) ) )
                    ((> n reqopt)
                      ; mehr Einzelargumente als nötig: fasse die überschüssigen
                      ; mit der Restliste zusammen
                      (let* ((revargs (reverse (cddr *form*)))
                             (lastarg (pop revargs))
                            )
                        (dotimes (i (- n reqopt))
                          (setq lastarg `(CONS ,(pop revargs) ,lastarg))
                        )
                        (return-from c-apply
                          (c-form `(APPLY #',fun ,(nreverse revargs) ,lastarg))
                    ) ) )
                    (t (when (eql opt 0)
                         ; weniger als nötig und keine optionalen: hole die
                         ; restlichen von der Restliste
                         (let* ((restlistvar (gensym))
                                (revargs (reverse (cddr *form*)))
                                (lastarg (pop revargs))
                               )
                           (return-from c-apply
                             (c-form
                               `(LET ((,restlistvar ,lastarg))
                                  (APPLY #',fun ,@(nreverse revargs)
                                         ,@(let ((L `((CAR ,restlistvar))))
                                             (dotimes (i (- reqopt n 1) L)
                                               (push `(POP ,restlistvar) L)
                                           ) )
                                         (CDR ,restlistvar)
                    )  ) ) ) )  ) )
  ) ) ) ) ) ) )
  ; keine Vereinfachung möglich
  |#
  (let* ((anode1 (c-form (second *form*) 'ONE))
         (*stackz* (cons 1 *stackz*)))
    (do ((formlistr (cddr *form*) (cdr formlistr))
         #+COMPILER-DEBUG (anodelist (list anode1))
         (codelist (list '(APPLYP) anode1)))
        ((null formlistr)
         (push `(APPLY ,(- (length *form*) 3)) codelist)
         (make-anode
           :type 'FUNCALL
           :sub-anodes (nreverse anodelist)
           :seclass '(T . T)
           :code (nreverse codelist)
        ))
      (let ((anode (c-form (car formlistr) 'ONE)))
        #+COMPILER-DEBUG (push anode anodelist)
        (push anode codelist)
        (when (cdr formlistr)
          (push 1 *stackz*) (push '(PUSH) codelist)
) ) ) ) )

(defun c-PLUS ()
  (test-list *form* 1)
  ; bilde Teilsumme der konstanten Argumente, Rest dann dazu:
  (let ((const-sum 0)
        (other-parts '())
        val
       )
    (dolist (form (cdr *form*))
      (if (and (c-constantp form) (numberp (setq val (c-constant-value form))))
        (setq const-sum (+ const-sum val))
        (push form other-parts)
    ) )
    (case (length other-parts)
      (0 ; nur konstante Summanden
         (c-form const-sum) ; Zahl const-sum wertet zu sich selbst aus
      )
      (1 ; nur ein variabler Summand
         (case const-sum
           (0 (c-form (first other-parts))) ; keine Addition nötig
           (+1 (c-form `(1+ ,(first other-parts))))
           (-1 (c-form `(1- ,(first other-parts))))
           (t (c-GLOBAL-FUNCTION-CALL-form `(+ ,const-sum ,@other-parts)))
      )  )
      (t (setq other-parts (nreverse other-parts))
         (unless (eql const-sum 0) (push const-sum other-parts))
         (c-GLOBAL-FUNCTION-CALL-form `(+ ,@other-parts))
) ) ) )

(defun c-MINUS ()
  (test-list *form* 2)
  (let ((unary-p (= (length *form*) 2)) ; unäres Minus oder nicht?
        (const-sum 0) ; Summe der konstanten Teile
        (first-part 0) ; zu addierende Form
        (other-parts '()) ; abzuziehende Formen
        val
       )
    (unless unary-p
      (let ((form (second *form*)))
        (if (and (c-constantp form) (numberp (setq val (c-constant-value form))))
          (setq const-sum val)
          (setq first-part form)
    ) ) )
    (dolist (form (if unary-p (cdr *form*) (cddr *form*)))
      (if (and (c-constantp form) (numberp (setq val (c-constant-value form))))
        (setq const-sum (- const-sum val))
        (push form other-parts)
    ) )
    (if (null other-parts)
      ; nichts zu subtrahieren
      (let ((*form* `(+ ,const-sum ,first-part))) (c-PLUS))
      ; etwas zu subtrahieren
      (c-GLOBAL-FUNCTION-CALL-form
        `(-
          ,@(if (eql first-part 0) ; variable zu addierende Form?
              (if (and (eql const-sum 0) (null (cdr other-parts)))
                '()
                `(,const-sum)
              )
              (if (eql const-sum 0)
                `(,first-part)
                `(,first-part ,(- const-sum))
            ) )
          ,@(nreverse other-parts)
         )
) ) ) )

(defun c-SVSTORE ()
  (test-list *form* 4 4)
  ; (sys::svstore arg1 arg2 arg3) -> (sys::%svstore arg3 arg1 arg2)
  (let ((arg1 (second *form*)) (arg2 (third *form*)) (arg3 (fourth *form*))
        (argvar1 (gensym)) (argvar2 (gensym)))
    (c-form
      `(LET* ((,argvar1 ,arg1) (,argvar2 ,arg2))
         (sys::%svstore ,arg3 ,argvar1 ,argvar2)
       )
) ) )

(defun c-EQ ()
  (test-list *form* 3 3)
  (let ((arg1 (second *form*)) (arg2 (third *form*)))
    (if (and (c-constantp arg1) (c-constantp arg2))
      (c-form `(QUOTE ,(eq (c-constant-value arg1) (c-constant-value arg2))))
      (progn
        (when (c-constantp arg1)
          (rotatef arg1 arg2) ; Besser arg2 konstant, damit JMPIFEQTO geht
        )
        (if (and (c-constantp arg2) (eq (c-constant-value arg2) 'NIL))
          (c-GLOBAL-FUNCTION-CALL-form `(NULL ,arg1))
          (c-GLOBAL-FUNCTION-CALL-form `(EQ ,arg1 ,arg2))
) ) ) ) )

; bei Symbolen, Fixnums und Characters ist EQL mit EQ gleichbedeutend
(defun EQL=EQ (x) (or (symbolp x) (fixnump x) (characterp x)))

(defun c-EQL ()
  (test-list *form* 3 3)
  (let ((arg1 (second *form*)) (arg2 (third *form*)))
    (cond ((and (c-constantp arg1) (c-constantp arg2))
           (c-form `(QUOTE ,(eql (c-constant-value arg1) (c-constant-value arg2))))
          )
          ((or (and (c-constantp arg1) (EQL=EQ (c-constant-value arg1)))
               (and (c-constantp arg2) (EQL=EQ (c-constant-value arg2)))
           )
           (let ((*form* `(EQ ,arg1 ,arg2))) (c-EQ))
          )
          (t (c-GLOBAL-FUNCTION-CALL 'EQL))
) ) )

; bei Symbolen, Zahlen und Characters ist EQUAL mit EQL gleichbedeutend
(defun EQUAL=EQL (x) (or (symbolp x) (numberp x) (characterp x)))

(defun c-EQUAL ()
  (test-list *form* 3 3)
  (let ((arg1 (second *form*)) (arg2 (third *form*)))
    (cond ((or (and (c-constantp arg1) (EQUAL=EQL (c-constant-value arg1)))
               (and (c-constantp arg2) (EQUAL=EQL (c-constant-value arg2)))
           )
           (let ((*form* `(EQL ,arg1 ,arg2))) (c-EQL))
          )
          (t (c-GLOBAL-FUNCTION-CALL 'EQUAL))
) ) )

; Bildet den inneren Teil einer MAPCAR/MAPC/MAPCAN/MAPCAP-Expansion
(defun c-MAP-on-CARs-inner (innerst-fun blockname restvars &optional (itemvars '()))
  (if (null restvars)
    (funcall innerst-fun (nreverse itemvars))
    (let ((restvar (car restvars))
          (itemvar (gensym)))
      `(IF (CONSP ,restvar)
         (LET ((,itemvar (CAR ,restvar)))
           ,(c-MAP-on-CARs-inner innerst-fun blockname (cdr restvars) (cons itemvar itemvars))
         )
         (RETURN-FROM ,blockname)
) ) )  )

; Bildet eine MAPCAR/MAPCAN/MAPCAP-Expansion
(defun c-MAP-on-CARs (adjoin-fun funform forms)
  (let ((erg (gensym))
        (blockname (gensym))
        (restvars
          (mapcar #'(lambda (form) (declare (ignore form)) (gensym)) forms)
        )
        (tag (gensym)))
    `(LET ((,erg NIL))
       (BLOCK ,blockname
         (LET* ,(mapcar #'list restvars forms)
           (TAGBODY ,tag
             ,(c-MAP-on-CARs-inner
                #'(lambda (itemvars)
                    `(SETQ ,erg (,adjoin-fun (SYS::%FUNCALL ,funform ,@itemvars) ,erg))
                  )
                blockname
                restvars
              )
             (SETQ ,@(mapcap #'(lambda (restvar)
                                 `(,restvar (CDR ,restvar))
                               )
                             restvars
             )       )
             (GO ,tag)
       ) ) )
       (SYS::LIST-NREVERSE ,erg)
) )  )

; Bildet eine MAPLIST/MAPCON/MAPLAP-Expansion
(defun c-MAP-on-LISTs (adjoin-fun funform forms)
  (let ((erg (gensym))
        (blockname (gensym))
        (restvars
          (mapcar #'(lambda (form) (declare (ignore form)) (gensym)) forms)
        )
        (tag (gensym)))
    `(LET ((,erg NIL))
       (BLOCK ,blockname
         (LET* ,(mapcar #'list restvars forms)
           (TAGBODY ,tag
             (IF (OR ,@(mapcar #'(lambda (restvar) `(ATOM ,restvar)) restvars))
               (RETURN-FROM ,blockname)
             )
             (SETQ ,erg (,adjoin-fun (SYS::%FUNCALL ,funform ,@restvars) ,erg))
             (SETQ ,@(mapcap #'(lambda (restvar)
                                 `(,restvar (CDR ,restvar))
                               )
                             restvars
             )       )
             (GO ,tag)
       ) ) )
       (SYS::LIST-NREVERSE ,erg)
) )  )

(defun c-MAPC ()
  (test-list *form* 3)
  (if (inline-callable-function-p (second *form*) (length (cddr *form*)))
    (c-form
      (let* ((tempvar (gensym))
             (forms (cons tempvar (cdddr *form*)))
             (blockname (gensym))
             (restvars
               (mapcar #'(lambda (form) (declare (ignore form)) (gensym)) forms)
             )
             (tag (gensym)))
        `(LET ((,tempvar ,(third *form*)))
           (BLOCK ,blockname
             (LET* ,(mapcar #'list restvars forms)
               (TAGBODY ,tag
                 ,(c-MAP-on-CARs-inner
                    #'(lambda (itemvars) `(SYS::%FUNCALL ,(second *form*) ,@itemvars))
                    blockname
                    restvars
                  )
                 (SETQ ,@(mapcap #'(lambda (restvar)
                                     `(,restvar (CDR ,restvar))
                                   )
                                 restvars
                 )       )
                 (GO ,tag)
           ) ) )
           ,tempvar
    ) )  )
    (c-GLOBAL-FUNCTION-CALL 'MAPC)
) )

(defun c-MAPL ()
  (test-list *form* 3)
  (if (inline-callable-function-p (second *form*) (length (cddr *form*)))
    (c-form
      (let* ((tempvar (gensym))
             (forms (cons tempvar (cdddr *form*)))
             (blockname (gensym))
             (restvars
               (mapcar #'(lambda (form) (declare (ignore form)) (gensym)) forms)
             )
             (tag (gensym)))
        `(LET ((,tempvar ,(third *form*)))
           (BLOCK ,blockname
             (LET* ,(mapcar #'list restvars forms)
               (TAGBODY ,tag
                 (IF (OR ,@(mapcar #'(lambda (restvar) `(ATOM ,restvar)) restvars))
                   (RETURN-FROM ,blockname)
                 )
                 (SYS::%FUNCALL ,(second *form*) ,@restvars)
                 (SETQ ,@(mapcap #'(lambda (restvar)
                                     `(,restvar (CDR ,restvar))
                                   )
                                 restvars
                 )       )
                 (GO ,tag)
           ) ) )
           ,tempvar
    ) )  )
    (c-GLOBAL-FUNCTION-CALL 'MAPL)
) )

(defun c-MAPCAR ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPC ,@(cdr *form*)))) (c-MAPC))
    (if (inline-callable-function-p (second *form*) (length (cddr *form*)))
      (c-form (c-MAP-on-CARs 'CONS (second *form*) (cddr *form*)))
      (c-GLOBAL-FUNCTION-CALL 'MAPCAR)
) ) )

(defun c-MAPLIST ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPL ,@(cdr *form*)))) (c-MAPL))
    (if (inline-callable-function-p (second *form*) (length (cddr *form*)))
      (c-form (c-MAP-on-LISTs 'CONS (second *form*) (cddr *form*)))
      (c-GLOBAL-FUNCTION-CALL 'MAPLIST)
) ) )

(defun c-MAPCAN ()
  (test-list *form* 3)
  (if (inline-callable-function-p (second *form*) (length (cddr *form*)))
    (c-form (c-MAP-on-CARs 'NRECONC (second *form*) (cddr *form*)))
    (c-GLOBAL-FUNCTION-CALL 'MAPCAN)
) )

(defun c-MAPCON ()
  (test-list *form* 3)
  (if (inline-callable-function-p (second *form*) (length (cddr *form*)))
    (c-form (c-MAP-on-LISTs 'NRECONC (second *form*) (cddr *form*)))
    (c-GLOBAL-FUNCTION-CALL 'MAPCON)
) )

(defun c-MAPCAP ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPC ,@(cdr *form*)))) (c-MAPC))
    (if (inline-callable-function-p (second *form*) (length (cddr *form*)))
      (c-form (c-MAP-on-CARs 'REVAPPEND (second *form*) (cddr *form*)))
      (c-GLOBAL-FUNCTION-CALL 'MAPCAP)
) ) )

(defun c-MAPLAP ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPL ,@(cdr *form*)))) (c-MAPL))
    (if (inline-callable-function-p (second *form*) (length (cddr *form*)))
      (c-form (c-MAP-on-LISTs 'REVAPPEND (second *form*) (cddr *form*)))
      (c-GLOBAL-FUNCTION-CALL 'MAPLAP)
) ) )

;; c-TYPEP vgl. TYPEP in type.lsp
; Symbole mit Property TYPE-SYMBOL:
(defconstant c-typep-alist1
  '((ARRAY . arrayp)
    (ATOM . atom)
    (BIT-VECTOR . bit-vector-p)
    (CHARACTER . characterp)
    (COMMON . commonp)
    (COMPILED-FUNCTION . compiled-function-p)
    (COMPLEX . complexp)
    (CONS . consp)
    (DOUBLE-FLOAT . double-float-p)
    (FIXNUM . fixnump)
    (FLOAT . floatp)
    (FUNCTION . functionp)
    (HASH-TABLE . hash-table-p)
    (INTEGER . integerp)
    (KEYWORD . keywordp)
    (LIST . listp)
    (LONG-FLOAT . long-float-p)
    (NULL . null)
    (NUMBER . numberp)
    (PACKAGE . packagep)
    (PATHNAME . pathnamep)
    (RANDOM-STATE . random-state-p)
    (RATIONAL . rationalp)
    (READTABLE . readtablep)
    (REAL . realp)
    (SEQUENCE . sys::sequencep)
    (SHORT-FLOAT . short-float-p)
    (SIMPLE-ARRAY . sys::simple-array-p)
    (SIMPLE-BIT-VECTOR . simple-bit-vector-p)
    (SIMPLE-STRING . simple-string-p)
    (SIMPLE-VECTOR . simple-vector-p)
    (SINGLE-FLOAT . single-float-p)
    (STREAM . streamp)
    (STRING . stringp)
    (SYMBOL . symbolp)
    (VECTOR . vectorp)
)  )
(defconstant c-typep-alist2
  '((BIGNUM . ((x) (and (integerp x) (not (fixnump x)))))
    (BIT . ((x) (or (eql x 0) (eql x 1))))
    (NIL . ((x) (declare (ignore x)) nil))
    (RATIO . ((x) (and (rationalp x) (not (integerp x)))))
    (STANDARD-CHAR . ((x) (and (characterp x) (standard-char-p x))))
    (STRING-CHAR . ((x) (and (characterp x) (string-char-p x))))
    (STRUCTURE .
      ((x)
        (let ((y (type-of x)))
          (and (symbolp y) (get y 'SYS::DEFSTRUCT-DESCRIPTION)
               (SYS::%STRUCTURE-TYPE-P y x)
    ) ) ) )
    (T . ((x) (declare (ignore x)) t))
)  )
(defun c-typep-array (tester)
  #'(lambda (x &optional (el-type '*) (dims '*) &rest illegal-args)
      (declare (ignore illegal-args))
      `(AND (,tester ,x)
            ,@(if (eq el-type '*)
                '()
                `((EQUAL (ARRAY-ELEMENT-TYPE ,x) ',(upgraded-array-element-type el-type)))
              )
            ,@(if (eq dims '*)
                '()
                (if (numberp dims)
                  `((EQL ,dims (ARRAY-RANK ,x)))
                  `((EQL ,(length dims) (ARRAY-RANK ,x))
                    ,@(let ((i 0))
                        (mapcap #'(lambda (dim)
                                    (prog1
                                      (if (eq dim '*)
                                        '()
                                        `((EQL ',dim (ARRAY-DIMENSION ,x ,i)))
                                      )
                                      (incf i)
                                  ) )
                                dims
                      ) )
                   )
              ) )
       )
)   )
(defun c-typep-vector (tester)
  #'(lambda (x &optional (size '*) &rest illegal-args)
      (declare (ignore illegal-args))
      `(AND (,tester ,x)
            ,@(if (eq size '*)
                '()
                `((EQL (ARRAY-DIMENSION ,x 0) ',size))
              )
       )
    )
)
(defun c-typep-number (caller tester)
  #'(lambda (x &optional (low '*) (high '*) &rest illegal-args)
      (declare (ignore illegal-args))
      `(AND (,tester ,x)
            ,@(cond ((eq low '*) '())
                    ((funcall tester low) `((<= ,low ,x)))
                    ((and (consp low) (null (rest low)) (funcall tester (first low)))
                     `((< ,(first low) ,x))
                    )
                    (t (c-warn #+DEUTSCH "~S: Argument zu ~S muß *, ~S oder eine Liste von ~S sein: ~S"
                               #+ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S"
                               #+FRANCAIS "~S : L'argument de ~S doit être *, ~S ou une liste de ~S: ~S"
                               'typep caller caller caller low
                       )
                       (throw 'c-TYPEP nil)
              )     )
            ,@(cond ((eq high '*) '())
                    ((funcall tester high) `((>= ,high ,x)))
                    ((and (consp high) (null (rest high)) (funcall tester (first high)))
                     `((> ,(first high) ,x))
                    )
                    (t (c-warn #+DEUTSCH "~S: Argument zu ~S muß *, ~S oder eine Liste von ~S sein: ~S"
                               #+ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S"
                               #+FRANCAIS "~S : L'argument de ~S doit être *, ~S ou une liste de ~S: ~S"
                               'typep caller caller caller high
                       )
                       (throw 'c-TYPEP nil)
              )     )
       )
    )
)
(defconstant c-typep-alist3
  `((ARRAY . ,(c-typep-array 'ARRAYP))
    (SIMPLE-ARRAY . ,(c-typep-array 'SIMPLE-ARRAY-P))
    (VECTOR .
      ,#'(lambda (x &optional (el-type '*) (size '*) &rest illegal-args)
           (declare (ignore illegal-args))
           `(AND (VECTORP ,x)
                 ,@(if (eq el-type '*)
                     '()
                     `((EQUAL (ARRAY-ELEMENT-TYPE ,x) ',(upgraded-array-element-type el-type)))
                   )
                 ,@(if (eq size '*)
                     '()
                     `((EQL (ARRAY-DIMENSION ,x 0) ',size))
                   )
            )
         )
    )
    (SIMPLE-VECTOR . ,(c-typep-vector 'SIMPLE-VECTOR-P))
    (COMPLEX .
      ,#'(lambda (x &optional (rtype '*) (itype rtype) &rest illegal-args)
           (declare (ignore illegal-args))
           `(AND (COMPLEXP ,x)
                 ,@(if (eq rtype '*)
                     '()
                     `((TYPEP (REALPART ,x) ',rtype))
                   )
                 ,@(if (eq itype '*)
                     '()
                     `((TYPEP (IMAGPART ,x) ',itype))
                   )
            )
         )
    )
    (INTEGER . ,(c-typep-number 'INTEGER 'INTEGERP))
    (MOD .
      ,#'(lambda (x &optional n &rest illegal-args)
           (declare (ignore illegal-args))
           (unless (integerp n)
             (c-warn #+DEUTSCH "~S: Argument zu MOD muß ganze Zahl sein: ~S"
                     #+ENGLISH "~S: argument to MOD must be an integer: ~S"
                     #+FRANCAIS "~S : L'argument de MOD doit être un entier: ~S"
                     'typep n
             )
             (throw 'c-TYPEP nil)
           )
           `(AND (INTEGERP ,x) (NOT (MINUSP ,x)) (< ,x ,n))
         )
    )
    (SIGNED-BYTE .
      ,#'(lambda (x &optional (n '*) &rest illegal-args)
           (declare (ignore illegal-args))
           (unless (or (eq n '*) (integerp n))
             (c-warn #+DEUTSCH "~S: Argument zu SIGNED-BYTE muß ganze Zahl oder * sein: ~S"
                     #+ENGLISH "~S: argument to SIGNED-BYTE must be an integer or * : ~S"
                     #+FRANCAIS "~S : L'argument de SIGNED-BYTE doit être un entier ou bien * : ~S"
                     'typep n
             )
             (throw 'c-TYPEP nil)
           )
           `(AND (INTEGERP ,x)
                 ,@(if (eq n '*) '() `((< (INTEGER-LENGTH ,x) ,n)))
            )
         )
    )
    (UNSIGNED-BYTE .
      ,#'(lambda (x &optional (n '*) &rest illegal-args)
           (declare (ignore illegal-args))
           (unless (or (eq n '*) (integerp n))
             (c-warn #+DEUTSCH "~S: Argument zu UNSIGNED-BYTE muß ganze Zahl oder * sein: ~S"
                     #+ENGLISH "~S: argument to UNSIGNED-BYTE must be an integer or * : ~S"
                     #+FRANCAIS "~S : L'argument de UNSIGNED-BYTE doit être un entier ou bien * : ~S"
                     'typep n
             )
             (throw 'c-TYPEP nil)
           )
           `(AND (INTEGERP ,x) (NOT (MINUSP ,x))
                 ,@(if (eq n '*) '() `((<= (INTEGER-LENGTH ,x) ,n)))
            )
         )
    )
    (REAL . ,(c-typep-number 'REAL 'REALP))
    (RATIONAL . ,(c-typep-number 'RATIONAL 'RATIONALP))
    (FLOAT . ,(c-typep-number 'FLOAT 'FLOATP))
    (SHORT-FLOAT . ,(c-typep-number 'SHORT-FLOAT 'SHORT-FLOAT-P))
    (SINGLE-FLOAT . ,(c-typep-number 'SINGLE-FLOAT 'SINGLE-FLOAT-P))
    (DOUBLE-FLOAT . ,(c-typep-number 'DOUBLE-FLOAT 'DOUBLE-FLOAT-P))
    (LONG-FLOAT . ,(c-typep-number 'LONG-FLOAT 'LONG-FLOAT-P))
    (STRING . ,(c-typep-vector 'STRINGP))
    (SIMPLE-STRING . ,(c-typep-vector 'SIMPLE-STRING-P))
    (BIT-VECTOR . ,(c-typep-vector 'BIT-VECTOR-P))
    (SIMPLE-BIT-VECTOR . ,(c-typep-vector 'SIMPLE-BIT-VECTOR-P))
)  )
(defun c-TYPEP () ; vgl. TYPEP in type.lsp
  (test-list *form* 3 3)
  (let ((objform (second *form*))
        (typeform (third *form*)))
    (when (c-constantp typeform)
      (let ((type (c-constant-value typeform)) h)
        (cond ((symbolp type)
                (cond ; Test auf Property TYPE-SYMBOL:
                      ((setq h (assoc type c-typep-alist1))
                        (setq h (cdr h))
                        (return-from c-TYPEP
                          (c-GLOBAL-FUNCTION-CALL-form `(,h ,objform))
                      ) )
                      ((setq h (assoc type c-typep-alist2))
                        (setq h (cdr h))
                        (return-from c-TYPEP
                          (let ((*form* `(,h ,objform)))
                            (c-FUNCALL-INLINE
                              (symbol-suffix '#:TYPEP (symbol-name type))
                              (list objform)
                              h
                              nil
                      ) ) ) )
                      ; Test auf Property TYPE-LIST:
                      ((setq h (assoc type c-typep-alist3))
                        (setq h (cdr h))
                        (let* ((objvar (gensym))
                               (testform (funcall h objvar))
                               (lambdabody `((,objvar) ,testform)))
                          (return-from c-TYPEP
                            (let ((*form* `((lambda ,@lambdabody) ,objform)))
                              (c-FUNCALL-INLINE
                                (symbol-suffix '#:TYPEP (symbol-name type))
                                (list objform)
                                lambdabody
                                nil
                      ) ) ) ) )
                      #+CLISP ; Test auf Property DEFTYPE-EXPANDER:
                      ((setq h (get type 'SYS::DEFTYPE-EXPANDER))
                        (return-from c-TYPEP
                          (c-form `(TYPEP ,objform ',(funcall h (list type))))
                      ) )
                      #+CLISP ; Test auf Property DEFSTRUCT-DESCRIPTION:
                      ((get type 'SYS::DEFSTRUCT-DESCRIPTION)
                        (return-from c-TYPEP
                          (c-form `(SYS::%STRUCTURE-TYPE-P ',type ,objform))
                      ) )
              ) )
              ((and (consp type) (symbolp (first type)))
                (catch 'c-TYPEP
                  (cond ((and (eq (first type) 'SATISFIES) (eql (length type) 2))
                          (let ((fun (second type)))
                            (unless (symbolp (second type))
                              (c-warn #+DEUTSCH "~S: Argument zu SATISFIES muß Symbol sein: ~S"
                                      #+ENGLISH "~S: argument to SATISFIES must be a symbol: ~S"
                                      #+FRANCAIS "~S : L'argument de SATISFIES doit être un symbole: ~S"
                                      'typep (second type)
                              )
                              (throw 'c-TYPEP nil)
                            )
                            (return-from c-TYPEP
                              (c-GLOBAL-FUNCTION-CALL-form `(,fun ,objform))
                        ) ) )
                        ((eq (first type) 'MEMBER)
                          (return-from c-TYPEP
                            (let ((*form* `(CASE ,objform (,(rest type) T) (t NIL))))
                              (c-CASE)
                        ) ) )
                        ((and (eq (first type) 'NOT) (eql (length type) 2))
                          (return-from c-TYPEP
                            (c-GLOBAL-FUNCTION-CALL-form
                              `(NOT (TYPEP ,objform ',(second type)))
                        ) ) )
                        ((or (eq (first type) 'AND) (eq (first type) 'OR))
                          (return-from c-TYPEP
                            (c-form
                              (let ((objvar (gensym)))
                                `(LET ((,objvar ,objform))
                                   (,(first type) ; AND oder OR
                                    ,@(mapcar #'(lambda (typei) `(TYPEP ,objvar ',typei)) (rest type))
                                 ) )
                        ) ) ) )
                        ((setq h (assoc (first type) c-typep-alist3))
                          (setq h (cdr h))
                          (let* ((objvar (gensym))
                                 (testform (apply h objvar (rest type)))
                                 (lambdabody `((,objvar) ,testform)))
                            (return-from c-TYPEP
                              (let ((*form* `((lambda ,@lambdabody) ,objform)))
                                (c-FUNCALL-INLINE
                                  (symbol-suffix '#:TYPEP (symbol-name (first type)))
                                  (list objform)
                                  lambdabody
                                  nil
                        ) ) ) ) )
              ) ) )
    ) ) )
    (c-GLOBAL-FUNCTION-CALL 'TYPEP)
) )



;                     Z W E I T E R   P A S S

; eine Tabelle von Paaren (fnode n).
; Jedes Paar zeigt an, daß im 3. Pass in der Konstanten Nummer n des
; funktionalen Objektes von fnode der dort stehende fnode durch das durch ihn
; erzeugte funktionale Objekt zu ersetzen ist.
(defvar *fnode-fixup-table*)

; macht aus dem ANODE-Baum zum fnode *func* ein funktionales Objekt:
(defun pass2 (*func*)
  (when (anode-p (fnode-code *func*)) ; falls 2. Pass noch nicht durchgeführt:
    ; erst den Code flachklopfen, optimieren und assemblieren:
    (let* ((code-list (compile-to-LAP)) ; Code flachklopfen und in Stücke zerteilen,
                                        ; optimieren und zu einer Liste machen
           #+CLISP3 (SPdepth (SP-depth code-list))) ; Stackbedarf bestimmen
      (setq code-list (insert-combined-LAPs code-list)) ; kombinierte Operationen einführen
      (create-fun-obj *func* (assemble-LAP code-list) #+CLISP3 SPdepth) ; assemblieren und funkt. Objekt
    )
    ; dann die Sub-Funktionen durch den 2. Pass jagen
    (dolist (x (fnode-Consts *func*)) (if (fnode-p x) (pass2 x)))
) )

#|

pass2 ruft den 1. Schritt auf.

Nach dem 1. Schritt ist der Code in kleine Stücke aufgeteilt, jeweils von
einem Label bis zu einem Wegsprung (JMP, JMPCASE, JMPCASE1-TRUE, JMPCASE1-FALSE,
JMPHASH, RETURN-FROM, GO, RET, THROW, ERROR). Die Teile stecken (jeweils als
Liste in umgekehrter Reihenfolge, mit dem Label als letztem CDR) im Vektor
*code-parts*.
(symbol-value label) enthält eine Liste der Referenzen von label, und zwar in
der Form:
 - Index in *code-parts*, wenn die Referenz der entsprechende Wegsprung ist;
 - opcode sonst, wobei opcode der Befehl ist, in dem label auftritt.
Nach dem 1. Schritt enthält der Code nur noch Tags (Symbole) und Listen aus
Symbolen und Zahlen. Es darf daher mit SUBST und EQUAL gearbeitet werden.

Der 1. Schritt ruft, sobald er mit einem Stück fertig ist, den 2. Schritt
auf.

Dann ruft pass2 den 3. Schritt auf. Es handelt sich hier um Optimierungen,
die, wenn sie erfolgreich waren, weitere dieser Optimierungen aufrufen.

|#

#|
                             1. Schritt:
          Expansion von Code-Teilen, Aufteilen des Codes in Stücke

Verändert werden:

vorher                           nachher

(CONST const)                    (CONST n)
(FCONST fnode)                   (CONST n), Fixup für 3. Pass merken
(BCONST block)                   (CONST n)
(GCONST tagbody)                 (CONST n)
(GET var venvc stackz)           (LOAD n) oder (LOADI k n) oder (LOADC n m)
                                 oder (LOADIC k n m) oder (LOADV k m)
                                 oder (GETVALUE n) oder (CONST n)
(SET var venvc stackz)           (STORE n) oder (STOREI k n) oder (STOREC n m)
                                 oder (STOREIC k n m) oder (STOREV k m)
                                 oder (SETVALUE n)
(SETVALUE symbol)                (SETVALUE n)
(GETVALUE symbol)                (GETVALUE n)
(BIND const)                     (BIND n)
(UNWIND stackz1 stackz2 for-value) eine Folge von
                                 (SKIP n), (SKIPI k n), (SKIPSP k), (VALUES0),
                                 (UNWIND-PROTECT-CLEANUP), (UNBIND1),
                                 (BLOCK-CLOSE), (TAGBODY-CLOSE)
(JMPIF label)                    (JMPCASE label new-label) new-label
(JMPIFNOT label)                 (JMPCASE new-label label) new-label
(JMPIF1 label)                   (JMPCASE1-TRUE label new-label) new-label
(JMPIFNOT1 label)                (JMPCASE1-FALSE new-label label) new-label
(JMPHASH test ((obj1 . label1) ... (objm . labelm)) label . labels)
                                 (JMPHASH n ht label . labels)
                                 wobei ht = Hash-Tabelle (obji -> labeli) ist
(VENV venvc stackz)              (VENV) oder (NIL)
                                 oder (LOAD n) oder (LOADI k n)
(COPY-CLOSURE fnode n)           (COPY-CLOSURE m n), Fixup für 3. Pass merken
(CALLP)                          gestrichen
(CALL k fun)                     (CALL k n)
(CALL0 fun)                      (CALL0 n)
(CALL1 fun)                      (CALL1 n)
(CALL2 fun)                      (CALL2 n)
(FUNCALLP)                       (PUSH)
(APPLYP)                         (PUSH)
(JMPIFBOUNDP var venvc stackz label)
                                 (JMPIFBOUNDP n label)
(BOUNDP var venvc stackz)        (BOUNDP n)
(BLOCK-OPEN const label)         (BLOCK-OPEN n label)
(RETURN-FROM const)              (RETURN-FROM n)
(RETURN-FROM block)              (RETURN-FROM n)
(TAGBODY-OPEN m label1 ... labelm)
                                 (TAGBODY-OPEN m label1 ... labelm)
(GO const k)                     (GO n k)
(GO tagbody k)                   (GO n k)


unverändert bleiben:
(NIL)
(PUSH-NIL n)
(T)
(STORE n)
(UNBIND1)
(PROGV)
(PUSH)
(POP)
(RET)
(JMP label)
(JSR m label)
(MAKE-VECTOR1&PUSH n)
(CALLS1 n)
(CALLS2 n)
(CALLSR m n)
(CALLC)
(CALLCKEY)
(FUNCALL n)
(APPLY n)
(PUSH-UNBOUND n)
(VALUES0)
(VALUES1)
(STACK-TO-MV n)
(MV-TO-STACK)
(NV-TO-STACK n)
(MV-TO-LIST)
(LIST-TO-MV)
(MVCALLP)
(MVCALL)
(BLOCK-CLOSE)
(TAGBODY-CLOSE-NIL)
(TAGBODY-CLOSE)
(CATCH-OPEN label)
(CATCH-CLOSE)
(THROW)
(UNWIND-PROTECT-OPEN label)
(UNWIND-PROTECT-NORMAL-EXIT)
(UNWIND-PROTECT-CLOSE label)
(UNWIND-PROTECT-CLEANUP)
(NOT)
(EQ)
(CAR)
(CDR)
(CONS)
(ATOM)
(CONSP)
(SYMBOL-FUNCTION)
(SVREF)
(SVSET)
(LIST n)
(ERROR n)

Neue Operationen:

(JMP label boolvalue)            Sprung zu label, boolvalue beschreibt den 1.
                                 Wert: FALSE falls =NIL, TRUE falls /=NIL,
                                 NIL falls unbekannt.

(JMPCASE label1 label2)          Sprung zu label1, falls A0 /= NIL,
                                 bzw. zu label2, falls A0 = NIL.

(JMPCASE1-TRUE label1 label2)    Falls A0 /= NIL: Sprung nach label1, 1 Wert.
                                 Falls A0 = NIL: Sprung nach label2.

(JMPCASE1-FALSE label1 label2)   Falls A0 /= NIL: Sprung nach label1.
                                 Falls A0 = NIL: Sprung nach label2, 1 Wert.

(JMPTAIL m n label)              Verkleinerung des Stack-Frames von n auf m,
                                 dann Sprung zu label mit undefinierten Werten.

|#

; Ein Vektor mit Fill-Pointer, der die Codestücke enthält:
(defvar *code-parts*)

; Ein gleichlanger Vektor mit Fill-Pointer, der zu jedem Codestück eine
; "Position" enthält, wo das Stück am Ende landen soll (0 = ganz am Anfang,
; je höher, desto weiter hinten).
(defvar *code-positions*)

; trägt eine Konstante in (fnode-consts *func*) ein und liefert deren Index n.
; value ist der Wert der Konstanten, form eine Form mit diesem Wert oder NIL.
(defun const-index (value form &optional (func *func*))
  (let ((const-list (fnode-consts func))
        (forms-list (fnode-consts-forms func))
        (n (fnode-Consts-Offset func)))
    (if (null const-list)
      (progn
        (setf (fnode-consts func) (list value))
        (setf (fnode-consts-forms func) (list form))
        n
      )
      (loop
        (when (eql (car const-list) value)
          (when (and (null (car forms-list)) form) (setf (car forms-list) form))
          (return n)
        )
        (incf n)
        (when (null (cdr const-list))
          (setf (cdr const-list) (list value))
          (setf (cdr forms-list) (list form))
          (return n)
        )
        (setq const-list (cdr const-list))
        (setq forms-list (cdr forms-list))
) ) ) )

; sucht eine Konstante in (fnode-Keywords *func*) und in (fnode-Consts *func*),
; trägt sie eventuell in (fnode-Consts *func*) ein. Liefert ihren Index n.
(defun kconst-index (value form &optional (func *func*))
  (when (keywordp value) ; nur bei Keywords lohnt sich die Suche
    (do ((n (fnode-Keyword-Offset func) (1+ n))
         (L (fnode-Keywords func) (cdr L)))
        ((null L))
      (if (eq (car L) value) (return-from kconst-index n))
  ) )
  (const-index value form func)
)

; (make-const-code value) liefert den Code, der value als 1 Wert nach A0 bringt
(defun make-const-code (value form)
  (cond ((eq value 'nil) '(NIL) )
        ((eq value 't) '(T) )
        (t `(CONST ,(kconst-index value form)) )
) )

; (bconst-index block) liefert den Index in FUNC, an dem dieser Block steht.
(defun bconst-index (block &optional (func *func*))
; (+ (fnode-Blocks-Offset func)
;    (position block (fnode-Blocks func) :test #'eq)
; )
  (do ((n (fnode-Blocks-Offset func) (1+ n))
       (L (fnode-Blocks func) (cdr L)))
      ((eq (car L) block) n)
) )

; (gconst-index tagbody) liefert den Index in FUNC, an dem dieser Tagbody steht.
(defun gconst-index (tagbody &optional (func *func*))
; (+ (fnode-Tagbodys-Offset func)
;    (position tagbody (fnode-Tagbodys func) :test #'eq)
; )
  (do ((n (fnode-Tagbodys-Offset func) (1+ n))
       (L (fnode-Tagbodys func) (cdr L)))
      ((eq (car L) tagbody) n)
) )

; (fconst-index fnode) liefert den Index in FUNC, an dem dieser fnode in den
; Konstanten steht. Wenn nötig, wird er eingefügt und in *fnode-fixup-table*
; vermerkt.
(defun fconst-index (fnode &optional (func *func*))
  (if (member fnode (fnode-Consts func))
    (const-index fnode nil)
    (let ((n (const-index fnode nil)))
      (push (list func n) *fnode-fixup-table*)
      n
) ) )

; Hilfsvariablen beim rekursiven Aufruf von traverse-anode:

; Das aktuelle Codestück, eine umgedrehte Liste von Instruktionen, die
; mit dem Start-Label als letztem nthcdr endet.
(defvar *code-part*)

; und seine Nummer (Index in *code-parts*)
(defvar *code-index*)

; Flag, ob "toter Code" (d.h. Code, der nicht erreichbar ist) vorliegt
(defvar *dead-code*)

; Für Sprungkettenverkürzung in traverse-anode: Liste aller bereits
; durchgeführten Label-Substitutionen ((old-label . new-label) ...)
(defvar *label-subst*)

; Der aktuelle Wert, interpretiert als boolescher Wert:
; FALSE falls =NIL, TRUE falls /=NIL, NIL falls unbekannt.
; (Keine Einschränkung an die Anzahl der Werte!)
(defvar *current-value*)

; Liste der Variablen/Konstanten, deren Wert mit dem aktuellen übereinstimmt
; (lexikalische Variablen als VARIABLE-Structures, dynamische Variablen als
; Symbole, Konstanten als CONST-Structures).
; Ist diese Liste nichtleer, so liegt auch genau 1 Wert vor.
(defvar *current-vars*)

; Jedes Label (ein Gensym-Symbol) hat als Wert eine Liste aller Referenzen
; auf label, und zwar jeweils entweder als Index i in *code-parts*, wenn es
; sich um den Wegsprung (das Ende) von (aref *code-parts* i) handelt, oder
; als Instruktion (einer Liste) in allen anderen Fällen. Falls das Label
; ein Codestück beginnt, steht unter (get label 'code-part) der Index in
; *code-part* des Codestücks, das mit diesem Label anfängt. Unter
; (get label 'for-value) steht, wieviele Werte bei einem möglichen Sprung
; auf das Label von Bedeutung sind (NIL/ONE/ALL).
; Eine Ausnahme stellt das "Label" NIL dar, das den Einsprungpunkt darstellt.

; Ersetzt alle Referenzen auf old-label durch Referenzen auf new-label.
(defun label-subst (old-label new-label)
  ; alle Refenzen auf old-label verändern:
  (dolist (ref (symbol-value old-label))
    (nsubst new-label old-label
            (rest (if (integerp ref) (first (aref *code-parts* ref)) ref))
  ) )
  ; und als Referenzen auf new-label eintragen:
  (setf (symbol-value new-label)
    (nconc (symbol-value old-label) (symbol-value new-label))
  )
  (setf (symbol-value old-label) '())
  ; Mit old-label fängt kein Codestück mehr an:
  (remprop old-label 'code-part)
)

; Aktuelles Codestück beenden und ein neues Codestück anfangen:
(defun finish-code-part ()
  ; das aktuelle Codestück vereinfachen:
  (simplify *code-part*)
  ; *code-part* in *code-parts* unterbringen:
  (vector-push-extend *code-part* *code-parts*)
  (vector-push-extend (incf *code-index*) *code-positions*)
)

; Einen Wegsprung auf Label label emittieren.
; Dadurch wird ein neues Codestück angefangen.
(defun emit-jmp (label)
  ; mit einem Wegsprung:
  (push `(JMP ,label ,*current-value*) *code-part*)
  (push *code-index* (symbol-value label))
  (finish-code-part)
)

; Läuft durch den Code eines Anode durch, expandiert den Code und baut dabei
; *code-part* weiter. Adjustiert die Variablen *current-value* usw. passend.
(defun traverse-anode (code)
  (dolist (item code)
    (if (atom item)
      (cond ((symbolp item) ; Label
             (if *dead-code*
               ; Code kann angesprungen werden, ist ab jetzt nicht mehr tot
               (setq *dead-code* nil)
               (if (symbolp *code-part*)
                 ; Label item sofort nach Label *code-part*
                 ; -> können identifiziert werden
                 (let ((old-label *code-part*) (new-label item))
                   ; substituiere *code-parts* -> item
                   (label-subst old-label new-label)
                   (setq *label-subst*
                     (acons old-label new-label
                       (nsubst new-label old-label *label-subst*)
                 ) ) )
                 ; Label mitten im Codestück -> aktuelles Codestück beenden
                 (emit-jmp item)
             ) )
             ; jetzt geht das aktuelle Codestück erst richtig los,
             ; mit dem Label item:
             (setq *code-part* item)
             (setf (get item 'code-part) (fill-pointer *code-parts*))
             ; Da noch Sprünge auf dieses Label kommen können, wissen wir
             ; nicht, was A0 enthält:
             (setq *current-value* nil *current-vars* '())
            )
            ((anode-p item) (traverse-anode (anode-code item))) ; Anode -> rekursiv
            (t (compiler-error 'traverse-anode "ITEM"))
      )
      ; item ist eine normale Instruktion
      (unless *dead-code* ; nur erreichbarer Code braucht verarbeitet zu werden
        (nsublis *label-subst* (rest item)) ; bisherige Substitutionen durchführen
        (case (first item)
          (CONST
            (let* ((c (second item))
                   (cv (const-value c)))
              (unless ; ein (CONST cv) schon in *current-vars* enthalten?
                  (dolist (v *current-vars* nil)
                    (when (and (const-p v) (eq (const-value v) cv)) (return t))
                  )
                (push (make-const-code cv (const-form c)) *code-part*)
                (setq *current-value* (if (null cv) 'FALSE 'TRUE)
                      *current-vars* (list c)
          ) ) ) )
          (FCONST
            (push `(CONST ,(fconst-index (second item))) *code-part*)
            (setq *current-value* 'TRUE *current-vars* '())
          )
          (BCONST
            (push `(CONST ,(bconst-index (second item))) *code-part*)
            (setq *current-value* 'TRUE *current-vars* '())
          )
          (GCONST
            (push `(CONST ,(gconst-index (second item))) *code-part*)
            (setq *current-value* 'TRUE *current-vars* '())
          )
          (GET
            (let ((var (second item))
                  (venvc (third item))
                  (stackz (fourth item)))
              (unless (member var *current-vars* :test #'eq) ; Ist bereits der aktuelle Wert = var ?
                (push
                  (if (var-constantp var)
                    (progn
                      (setq *current-value*
                            (if (null (var-constant-value var)) 'FALSE 'TRUE)
                      )
                      (if (fnode-p (var-constant-value var))
                        ; FNODEs als Werte können (fast) nur von LABELS stammen
                        `(CONST ,(fconst-index (var-constant-value var)))
                        (make-const-code (var-constant-value var)
                                         (if (var-specialp var) (var-name var) nil)
                    ) ) )
                    (progn
                      (setq *current-value* nil)
                      (if (var-specialp var)
                        `(GETVALUE ,(kconst-index (setq var (var-name var)) nil))
                        (if (var-closurep var)
                          (multiple-value-bind (k n m)
                              (zugriff-in-closure var venvc stackz)
                            (if n
                              (if k `(LOADIC ,k ,n ,m) `(LOADC ,n ,m))
                              `(LOADV ,k ,m)
                          ) )
                          ; lexikalisch und im Stack, also in derselben Funktion
                          (multiple-value-bind (k n)
                              (zugriff-in-stack stackz (var-stackz var))
                            (if k `(LOADI ,k ,n) `(LOAD ,n) )
                  ) ) ) ) )
                  *code-part*
                )
                (setq *current-vars* (list var))
          ) ) )
          (SET
            (let ((var (second item))
                  (venvc (third item))
                  (stackz (fourth item)))
              (unless (member var *current-vars* :test #'eq) ; Ist bereits der aktuelle Wert = var ?
                (push
                  (if (var-specialp var)
                    `(SETVALUE ,(kconst-index (setq var (var-name var)) nil))
                    (if (var-closurep var)
                      (multiple-value-bind (k n m)
                          (zugriff-in-closure var venvc stackz)
                        (if n
                          (if k `(STOREIC ,k ,n ,m) `(STOREC ,n ,m))
                          `(STOREV ,k ,m)
                      ) )
                      ; lexikalisch und im Stack, also in derselben Funktion
                      (multiple-value-bind (k n)
                          (zugriff-in-stack stackz (var-stackz var))
                        (if k `(STOREI ,k ,n) `(STORE ,n) )
                  ) ) )
                  *code-part*
                )
                (push var *current-vars*) ; *current-value* bleibt unverändert
          ) ) )
          (GETVALUE
            (let ((symbol (second item)))
              (unless (member symbol *current-vars* :test #'eq)
                (push `(GETVALUE ,(kconst-index symbol nil)) *code-part*)
                (setq *current-value* nil *current-vars* (list symbol))
          ) ) )
          (SETVALUE
            (let ((symbol (second item)))
              (unless (member symbol *current-vars* :test #'eq)
                (push `(SETVALUE ,(kconst-index symbol nil)) *code-part*)
                (push symbol *current-vars*) ; *current-value* bleibt unverändert
          ) ) )
          (BIND
            (push `(BIND ,(kconst-index (const-value (second item)) nil)) *code-part*)
            (setq *current-value* nil *current-vars* '()) ; undefinierte Werte
          )
          (UNWIND ; mehrzeilige Umwandlung
            (traverse-anode
              (expand-UNWIND (second item) (third item) (fourth item))
          ) )
          ((JMPIF JMPIFNOT JMPIF1 JMPIFNOT1)
            (if (null *current-value*)
              (let ((label (second item))
                    (new-label (make-label 'NIL)))
                (push
                  (case (first item)
                    (JMPIF `(JMPCASE ,label ,new-label))
                    (JMPIFNOT `(JMPCASE ,new-label ,label))
                    (JMPIF1 `(JMPCASE1-TRUE ,label ,new-label))
                    (JMPIFNOT1 `(JMPCASE1-FALSE ,new-label ,label))
                  )
                  *code-part*
                )
                (push *code-index* (symbol-value (second item)))
                (push *code-index* (symbol-value new-label))
                (finish-code-part)
                (setf (get new-label 'code-part) (fill-pointer *code-parts*))
                (setq *code-part* new-label)
                ; *current-value* und *current-vars* bleiben unverändert.
              )
              ; boolescher Wert beim Wegsprung bekannt
              (if (if (eq *current-value* 'FALSE)
                    (memq (first item) '(JMPIF JMPIF1)) ; Wert=NIL -> JMPIF weglassen
                    (memq (first item) '(JMPIFNOT JMPIFNOT1)) ; Wert/=NIL -> JMPIFNOT weglassen
                  )
                ; Sprung weglassen
                nil
                ; in JMP umwandeln:
                (progn
                  (when (memq (first item) '(JMPIF1 JMPIFNOT1))
                    (push '(VALUES1) *code-part*) ; genau 1 Wert erzwingen
                  )
                  (emit-jmp (second item))
                  (setq *dead-code* t)
          ) ) ) )
          (JMPHASH
            (let ((hashtable (make-hash-table :test (second item)))
                  (labels (cddddr item)))
              (dolist (acons (third item))
                (setf (gethash (car acons) hashtable)
                      (position (cdr acons) labels)
              ) )
              (push `(JMPHASH ,(const-index hashtable nil) ,hashtable
                              ,@(cdddr item)
                     )
                    *code-part*
            ) )
            ; Referenzen vermerken:
            (dolist (label (cdddr item))
              (push *code-index* (symbol-value label))
            )
            (finish-code-part)
            (setq *dead-code* t)
          )
          (VENV
            (let ((venvc (second item))
                  (stackz (third item)))
              (loop ; in venvc die NILs übergehen
                (when (car venvc) (return))
                (setq venvc (cdr venvc))
              )
              (push
                (if (consp (car venvc)) ; aus dem Stack holen
                  (multiple-value-bind (k n)
                      (zugriff-in-stack stackz (cdr (car venvc)))
                    (if k `(LOADI ,k ,n) `(LOAD ,n) )
                  )
                  (if (eq (car venvc) *func*)
                    (if (fnode-Venvconst *func*) '(VENV) '(NIL))
                    (compiler-error 'traverse-anode 'VENV)
                ) )
                *code-part*
              )
              (if (equal (car *code-part*) '(NIL))
                (setq *current-value* 'FALSE *current-vars* (list (make-const :value 'NIL)))
                (setq *current-value* nil *current-vars* '())
              )
          ) )
          (COPY-CLOSURE
            (push `(COPY-CLOSURE ,(fconst-index (second item)) ,(third item))
                   *code-part*
            )
            (setq *current-value* 'TRUE *current-vars* '())
          )
          (CALLP) ; wird gestrichen
          (CALL
            (push `(CALL ,(second item)
                         ,(kconst-index (const-value (third item)) nil)
                   )
                   *code-part*
            )
            (setq *current-value* nil *current-vars* '())
          )
          ((CALL0 CALL1 CALL2)
            (push
              `(,(first item) ,(kconst-index (const-value (second item)) nil))
              *code-part*
            )
            (setq *current-value* nil *current-vars* '())
          )
          ((FUNCALLP APPLYP)
            (push '(PUSH) *code-part*)
            (setq *current-value* nil *current-vars* '())
          )
          ((JMPIFBOUNDP BOUNDP)
            (let ((var (second item))
                  (stackz (fourth item))
                 )
              (when (var-closurep var)
                (compiler-error 'traverse-anode 'var-closurep)
              )
              (multiple-value-bind (k n)
                  (zugriff-in-stack stackz (var-stackz var))
                (when k (compiler-error 'traverse-anode 'var-stackz))
                (push `(,(first item) ,n ,@(cddddr item)) *code-part*)
                (when (eq (first item) 'JMPIFBOUNDP)
                  (push (first *code-part*) (symbol-value (fifth item)))
                )
                (setq *current-value* nil *current-vars* '()) ; undefinierte Werte
          ) ) )
          (BLOCK-OPEN
            (let ((label (third item)))
              (push `(BLOCK-OPEN ,(kconst-index (const-value (second item)) nil)
                                 ,label
                     )
                     *code-part*
              )
              (push (first *code-part*) (symbol-value label))
          ) )
          (RETURN-FROM
            (push
              (if (block-p (second item))
                `(RETURN-FROM ,(bconst-index (second item)))
                `(RETURN-FROM ,(kconst-index (const-value (second item)) nil))
              )
              *code-part*
            )
            (finish-code-part)
            (setq *dead-code* t)
          )
          (TAGBODY-OPEN
            (push item *code-part*)
            (dolist (label (cddr item)) (push item (symbol-value label)))
          )
          (GO
            (push
              (if (tagbody-p (second item))
                `(GO ,(gconst-index (second item)) ,(third item))
                `(GO ,(kconst-index (const-value (second item)) nil)
                     ,(third item)
              )  )
              *code-part*
            )
            (finish-code-part)
            (setq *dead-code* t)
          )
          ((NIL TAGBODY-CLOSE-NIL)
            (push item *code-part*)
            (setq *current-value* 'FALSE *current-vars* (list (make-const :value 'NIL)))
          )
          (VALUES0
            (push item *code-part*)
            (setq *current-value* 'FALSE *current-vars* '())
          )
          ((SKIP SKIPI SKIPSP VALUES1 MVCALLP BLOCK-CLOSE TAGBODY-CLOSE
            CATCH-CLOSE UNWIND-PROTECT-NORMAL-EXIT
            STORE ; STORE nur auf Funktionsargumente innerhalb eines
                  ; Funktionsaufrufs, vgl. c-DIRECT-FUNCTION-CALL
           )
            (push item *code-part*)
          )
          ((T)
            (push item *code-part*)
            (setq *current-value* 'TRUE *current-vars* (list (make-const :value 'T)))
          )
          ((RET THROW ERROR)
            (push item *code-part*)
            (finish-code-part)
            (setq *dead-code* t)
          )
          (JMP
            (emit-jmp (second item))
            (setq *dead-code* t)
          )
          (JSR
            (push item *code-part*)
            (push item (symbol-value (third item)))
            (setq *current-value* nil *current-vars* '())
          )
          ((CATCH-OPEN UNWIND-PROTECT-OPEN)
            (push item *code-part*)
            (push item (symbol-value (second item)))
          )
          (UNWIND-PROTECT-CLOSE
            (push item *code-part*)
            (push item (symbol-value (second item)))
            (setq *current-value* nil *current-vars* '()) ; Werte werden weggeworfen
          )
          ((PUSH-NIL PROGV PUSH POP MAKE-VECTOR1&PUSH CALLS1 CALLS2 CALLSR
            CALLC CALLCKEY FUNCALL APPLY PUSH-UNBOUND STACK-TO-MV MV-TO-STACK
            NV-TO-STACK MV-TO-LIST LIST-TO-MV MVCALL NOT EQ CAR CDR ATOM
            CONSP SYMBOL-FUNCTION SVREF SVSET
           )
            (push item *code-part*)
            (setq *current-value* nil *current-vars* '())
          )
          ((CONS LIST)
            (push item *code-part*)
            (setq *current-value* 'TRUE *current-vars* '())
          )
          ((UNWIND-PROTECT-CLEANUP)
            (push item *code-part*)
            (setq *current-vars* '()) ; Kann Variablenwerte zerstören
          )
          ((UNBIND1)
            (push item *code-part*)
            (setq *current-vars* (delete-if #'symbolp *current-vars*)) ; Kann Werte dynamischer Variablen zerstören
          )
          (t (compiler-error 'traverse-anode "LISTITEM"))
) ) ) ) )

; Hilfsfunktionen nach dem 1. Schritt:

; Kommt eine Instruktion item dazu, die vielleicht Label-Referenzen enthält,
; so ist note-references aufzurufen. Dieses notiert die Label-Referenzen in
; item. item gehöre zu (aref *code-parts* index).
; Wird eine Instruktion item entfernt, die vielleicht Label-Referenzen enthält,
; so ist remove-references aufzurufen. Dieses notiert das Wegfallen der
; Label-Referenzen in item. item gehöre zu (aref *code-parts* index).
; Liefert auch die Liste der in item enthaltenen Labels.
(macrolet ((references ()
             `(case (first item)
                (JMP (end-ref (second item)))
                ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                 (end-ref (second item)) (end-ref (third item))
                )
                (JMPHASH (dolist (label (cdddr item)) (end-ref label)))
                ((JMPIFBOUNDP CATCH-OPEN UNWIND-PROTECT-OPEN UNWIND-PROTECT-CLOSE)
                 (mid-ref (second item))
                )
                ((BLOCK-OPEN JSR) (mid-ref (third item)))
                (JMPTAIL (mid-ref (fourth item)))
                (TAGBODY-OPEN (dolist (label (cddr item)) (mid-ref label)))
              )
          ))
  (defun note-references (item &optional index)
    (macrolet ((end-ref (label) `(push index (symbol-value ,label)))
               (mid-ref (label) `(push item (symbol-value ,label))))
      (references)
  ) )
  (defun remove-references (item &optional index &aux (labellist '()))
    (macrolet ((end-ref (label)
                 (let ((labelvar (gensym)))
                   `(let ((,labelvar ,label))
                      (setf (symbol-value ,labelvar) (delete index (symbol-value ,labelvar)))
                      (pushnew ,labelvar labellist)
                    )
               ) )
               (mid-ref (label)
                 (let ((labelvar (gensym)))
                   `(let ((,labelvar ,label))
                      (setf (symbol-value ,labelvar) (delete item (symbol-value ,labelvar)))
                      (pushnew ,labelvar labellist)
                    )
              )) )
      (references)
      labellist
  ) )
)

#|
                              2. Schritt
                Vereinfachung von Folgen von Operationen

Dieses spielt sich auf umgedrehten Codestücken ab; sie werden dabei destruktiv
verändert.

Vereinfachungsregeln für Operationen:

1. (VALUES1) darf nach allen Instruktionen gestrichen werden, die sowieso nur
   einen Wert produzieren, und vor allen, die sowieso nur einen verwenden.

2. (SKIP n1) (SKIP n2)               --> (SKIP n1+n2)
   (SKIPI k n1) (SKIP n2)            --> (SKIPI k n1+n2)
   (SKIP n1) (SKIPI k n2)            --> (SKIPI k n2)
   (SKIPI k1 n1) (SKIPI k2 n2)       --> (SKIPI k1+k2+1 n2)
   (SKIPSP k1) (SKIPI k2 n)          --> (SKIPI k1+k2 n)
   (SKIPSP k1) (SKIPSP k2)           --> (SKIPSP k1+k2)

3. (NOT) (NOT) (NOT)                 --> (NOT)
   (ATOM) (NOT)                      --> (CONSP)
   (CONSP) (NOT)                     --> (ATOM)

4. (LOAD 0) (SKIP n)                 --> (POP) (SKIP n-1)  für n>1
   (LOAD 0) (SKIP 1)                 --> (POP)             für n=1
   (PUSH) (SKIP n)                   --> (SKIP n-1)  für n>1
   (PUSH) (SKIP 1)                   -->             für n=1
   (NV-TO-STACK n) (SKIP n)          -->
   (NV-TO-STACK n+m) (SKIP n)        --> (NV-TO-STACK m)
   (NV-TO-STACK n) (SKIP n+m)        --> (SKIP m)
   (STORE m) (SKIP n)                --> (VALUES1) (SKIP n) für n>m
   (STORE 0) (POP)                   --> (VALUES1) (SKIP 1)
   (PUSH) (POP)                      --> (VALUES1)
   (POP) (PUSH)                      -->
   (SKIP n) (PUSH)                   --> (SKIP n-1) (STORE 0) für n>1
   (SKIP 1) (PUSH)                   --> (STORE 0)            für n=1

5. (VALUES1)/... (MV-TO-STACK)       --> (VALUES1)/... (PUSH)
   (VALUES0) (MV-TO-STACK)           -->
   (STACK-TO-MV n) (MV-TO-STACK)     -->
   (STACK-TO-MV m) (NV-TO-STACK n)   --> (PUSH-NIL n-m)  für m<n
                                     -->                 für m=n
                                     --> (SKIP m-n)      für m>n
   (NIL)/(VALUES0) (NV-TO-STACK n)   --> (PUSH-NIL n)
   (VALUES1)/... (NV-TO-STACK n)     --> (VALUES1)/... (PUSH) (PUSH-NIL n-1)

6. (PUSH-UNBOUND n) (PUSH-UNBOUND m) --> (PUSH-UNBOUND n+m)

|#

; Die Hash-Tabelle one-value-ops enthält diejenigen Befehle,
; die genau einen Wert erzeugen.
(defconstant one-value-ops
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (op '(NIL T CONST LOAD LOADI LOADC LOADV LOADIC STORE STOREI
                  STOREC STOREV STOREIC GETVALUE SETVALUE POP VENV
                  COPY-CLOSURE BOUNDP VALUES1 MV-TO-LIST TAGBODY-CLOSE-NIL
                  NOT EQ CAR CDR CONS ATOM CONSP SYMBOL-FUNCTION SVREF SVSET
                  LIST
            )    )
      (setf (gethash op ht) t)
    )
    ht
) )

; Der Wert zu einem Key in dieser Hash-Tabelle gibt an, wieviele Werte bei
; der Ausführung der entsprechenden Operation benötigt werden
; (vgl. *for-value*):
; NIL : Werte werden weggeworfen.
; ONE : Ein Wert wird verwendet, die übrigen weggeworfen.
; ALL : Alle Werte werden verwendet.
; Operationen, die ihre Werte nicht verändern, werden hierin nicht
; aufgeführt.
(defconstant for-value-table
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (op '(NIL PUSH-NIL T CONST LOAD LOADI LOADC LOADV LOADIC
                  GETVALUE POP JSR JMPTAIL VENV COPY-CLOSURE CALL CALL0
                  CALLS1 CALLS2 CALLSR FUNCALL PUSH-UNBOUND JMPIFBOUNDP
                  BOUNDP VALUES0 STACK-TO-MV MVCALL
                  BLOCK-OPEN TAGBODY-OPEN TAGBODY-CLOSE-NIL GO
                  UNWIND-PROTECT-OPEN UNWIND-PROTECT-CLOSE LIST ERROR
            )    )
      (setf (gethash op ht) 'NIL)
    )
    (dolist (op '(STORE STOREI STOREC STOREV STOREIC SETVALUE BIND PROGV PUSH
                  MAKE-VECTOR1&PUSH CALL1 CALL2 CALLC CALLCKEY APPLY
                  VALUES1 LIST-TO-MV MVCALLP CATCH-OPEN NOT EQ CAR CDR CONS
                  ATOM CONSP SYMBOL-FUNCTION SVREF SVSET
            )    )
      (setf (gethash op ht) 'ONE)
    )
    (dolist (op '(MV-TO-STACK NV-TO-STACK MV-TO-LIST RETURN-FROM THROW
                  UNWIND-PROTECT-NORMAL-EXIT
            )    )
      (setf (gethash op ht) 'ALL)
    )
    ; Nicht in der Tabelle, weil sie die Werte unverändert lassen:
    ;           '(UNBIND1 SKIP SKIPI SKIPSP BLOCK-CLOSE TAGBODY-CLOSE
    ;             CATCH-CLOSE UNWIND-PROTECT-CLEANUP
    ;            )
    ; Nicht in der Tabelle, weil es Wegsprünge sind:
    ;   ONE:    '(JMPHASH)
    ;   ALL:    '(RET JMP JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
    ht
) )

; Vereinfacht ein Codestück (in umgedrehter Reihenfolge!).
; Obige Vereinfachungsregeln werden durchgeführt, solange es geht.
; Ergebnis ist meist NIL, oder aber (um anzuzeigen, daß weitere Optimierungen
; möglich sind) das Anfangslabel, falls sich dessen Property for-value
; abgeschwächt hat.
(defun simplify (codelist)
  (let ((for-value-at-end
          (let ((item (car codelist)))
            (case (first item)
              (JMP (get (second item) 'for-value))
              ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                (if (or (and (not (eq (first item) 'JMPCASE1-TRUE))
                             (eq (get (second item) 'for-value) 'ALL)
                        )
                        (and (not (eq (first item) 'JMPCASE1-FALSE))
                             (eq (get (third item) 'for-value) 'ALL)
                    )   )
                  'ALL
                  'ONE
              ) )
              (JMPHASH 'ONE)
              ((ERROR GO JMPTAIL) 'NIL)
              ((RETURN-FROM RET THROW) 'ALL)
              (t (compiler-error 'simplify "AT-END"))
        ) ) )
        (result nil)) ; evtl. das Anfangslabel
    ; for-value-at-end zeigt an, welche Werte vor dem Wegsprung benötigt werden.
    (loop
      (let ((modified nil))
        (let* ((links codelist) (mitte (cdr links)) rechts (for-value for-value-at-end))
          ; Es wandern drei Pointer durch die Codeliste: ...links.mitte.rechts...
          ; for-value zeigt an, was für Werte nach Ausführung von (car mitte),
          ; vor Ausführung von (car links), gebraucht werden.
          (loop
            nochmal
            (when (atom mitte) (return))
            (setq rechts (cdr mitte))
            (macrolet ((ersetze1 (new) ; ersetze (car mitte) durch new
                         `(progn
                            (setf (car mitte) ,new)
                            (setq modified t) (go nochmal)
                          )
                       )
                       (ersetze2 (new) ; ersetze (car mitte) und (car rechts) durch new
                         `(progn
                            ,@(unless (equal new '(car mitte))
                                `((setf (car mitte) ,new))
                              )
                            (setf (cdr mitte) (cdr rechts))
                            (setq modified t) (go nochmal)
                          )
                       )
                       (streiche1 () ; streiche (car mitte) ersatzlos
                         `(progn
                            (setf (cdr links) (setq mitte rechts))
                            (setq modified t) (go nochmal)
                          )
                       )
                       (streiche2 () ; streiche (car mitte) und (car rechts) ersatzlos
                         `(progn
                            (setf (cdr links) (setq mitte (cdr rechts)))
                            (setq modified t) (go nochmal)
                          )
                       )
                       (erweitere2 (new1 new2) ; ersetze (car mitte) durch new1 und new2
                         `(progn
                            (setf (car mitte) ,new1)
                            (setf (cdr mitte) (cons ,new2 rechts))
                            (setq modified t) (go nochmal)
                          )
                      ))
              (when (eq for-value 'NIL)
                ; vor einer Operation, die keine Werte braucht:
                (case (first (car mitte))
                  ((NIL T CONST LOAD LOADI LOADC LOADV LOADIC GETVALUE VENV
                    BOUNDP VALUES0 VALUES1 MV-TO-LIST LIST-TO-MV NOT CAR CDR
                    SYMBOL-FUNCTION ATOM CONSP
                   )
                    (streiche1)
                  )
                  ((LIST STACK-TO-MV) ; (LIST n) --> (SKIP n), n>0
                                      ; (STACK-TO-MV n) --> (SKIP n), n>0
                    (ersetze1 `(SKIP ,(second (car mitte))))
                  )
                  ((POP EQ CONS SVREF) (ersetze1 '(SKIP 1)))
              ) )
              (when (eq for-value 'ONE)
                ; vor einer Operation, die nur einen Wert braucht:
                (case (first (car mitte))
                  (VALUES1 (streiche1))
                  (VALUES0 (ersetze1 '(NIL)))
                  (LIST-TO-MV (ersetze1 '(CAR)))
                  (STACK-TO-MV ; (STACK-TO-MV n) --> (SKIP n-1) (POP) für n>1
                    (let ((n (second (car mitte))))
                      (erweitere2 '(POP) `(SKIP ,(- n 1)))
              ) ) ) )
              (when (consp rechts)
                ; Gucklock umfaßt (car mitte) und (car rechts), evtl. auch mehr.
                (case (first (car mitte))
                  (VALUES1 ; Regel 1
                    (when (gethash (first (car rechts)) one-value-ops nil)
                      ; (op ...) (VALUES1) --> (op ...)
                      (streiche1)
                  ) )
                  (NOT ; Regel 3
                    (case (first (car rechts))
                      (NOT
                        (when (and (consp (cdr rechts))
                                   (equal (cadr rechts) '(NOT))
                              )
                          ; (NOT) (NOT) (NOT) --> (NOT)
                          (streiche2)
                      ) )
                      (ATOM (ersetze2 '(CONSP))) ; (ATOM) (NOT) --> (CONSP)
                      (CONSP (ersetze2 '(ATOM))) ; (CONSP) (NOT) --> (ATOM)
                  ) )
                  (SKIP
                    (let ((n2 (second (car mitte)))) ; n2 > 0
                      (case (first (car rechts))
                        ; Regel 2
                        (SKIP ; (SKIP n1) (SKIP n2) --> (SKIP n1+n2)
                          (let ((n1 (second (car rechts))))
                            (ersetze2 `(SKIP ,(+ n1 n2)))
                        ) )
                        (SKIPI ; (SKIPI k n1) (SKIP n2) --> (SKIPI k n1+n2)
                          (let ((k (second (car rechts)))
                                (n1 (third (car rechts))))
                            (ersetze2 `(SKIPI ,k ,(+ n1 n2)))
                        ) )
                        ; Regel 4
                        (LOAD ; (LOAD 0) (SKIP n) --> (POP) [(SKIP n-1)]
                          (when (eql (second (car rechts)) 0)
                            (if (eql n2 1)
                              (ersetze2 '(POP))
                              (progn (setf (car rechts) '(POP))
                                     (ersetze1 `(SKIP ,(- n2 1)))
                        ) ) ) )
                        (PUSH ; (PUSH) (SKIP n) --> [(SKIP n-1)]
                          (if (eql n2 1)
                            (streiche2)
                            (ersetze2 `(SKIP ,(- n2 1)))
                        ) )
                        (NV-TO-STACK
                          (let ((n1 (second (car rechts))))
                            (cond ((> n1 n2) (ersetze2 `(NV-TO-STACK ,(- n1 n2))))
                                  ((< n1 n2) (ersetze2 `(SKIP ,(- n2 n1))))
                                  (t (streiche2))
                        ) ) )
                        (STORE ; (STORE m) (SKIP n) --> (VALUES1) (SKIP n) für n>m
                          (let ((m (second (car rechts))))
                            (when (> n2 m)
                              (setf (car rechts) '(VALUES1))
                              (setq modified t) (go nochmal)
                  ) ) ) ) ) )
                  (SKIPI ; Regel 2
                    (case (first (car rechts))
                      (SKIP ; (SKIP n1) (SKIPI k n2) --> (SKIPI k n2)
                        (ersetze2 (car mitte))
                      )
                      (SKIPI ; (SKIPI k1 n1) (SKIPI k2 n2) --> (SKIPI k1+k2+1 n2)
                        (let ((k1 (second (car rechts)))
                              (k2 (second (car mitte)))
                              (n2 (third (car mitte))))
                          (ersetze2 `(SKIPI ,(+ k1 k2 1) ,n2))
                      ) )
                      (SKIPSP ; (SKIPSP k1) (SKIPI k2 n) --> (SKIPI k1+k2 n)
                        (let ((k1 (second (car rechts)))
                              (k2 (second (car mitte)))
                              (n2 (third (car mitte))))
                          (ersetze2 `(SKIPI ,(+ k1 k2) ,n2))
                  ) ) ) )
                  (SKIPSP ; Regel 2
                    (case (first (car rechts))
                      (SKIPSP ; (SKIPSP k1) (SKIPSP k2) --> (SKIPSP k1+k2)
                        (let ((k1 (second (car rechts)))
                              (k2 (second (car mitte))))
                          (ersetze2 `(SKIPSP ,(+ k1 k2)))
                  ) ) ) )
                  (POP ; Regel 4
                    (cond ((equal (car rechts) '(STORE 0))
                            ; (STORE 0) (POP) --> (VALUES1) (SKIP 1)
                            (setf (car rechts) '(VALUES1))
                            (ersetze1 '(SKIP 1))
                          )
                          ((equal (car rechts) '(PUSH))
                            ; (PUSH) (POP) --> (VALUES1)
                            (ersetze2 '(VALUES1))
                  ) )     )
                  (PUSH ; Regel 4
                    (case (first (car rechts))
                      (POP (streiche2)) ; (POP) (PUSH) streichen
                      (SKIP ; (SKIP n) (PUSH) --> [(SKIP n-1)] (STORE 0)
                        (let ((n (second (car rechts))))
                          (if (eql n 1)
                            (unless (and (consp (cdr rechts)) (equal (cadr rechts) '(LOAD 0)))
                              ; (LOAD 0) (SKIP 1) (PUSH) wird anders behandelt
                              (ersetze2 '(STORE 0))
                            )
                            (progn (setf (car rechts) `(SKIP ,(- n 1)))
                                   (ersetze1 '(STORE 0))
                  ) ) ) ) ) )
                  (MV-TO-STACK ; Regel 5
                    (when (gethash (first (car rechts)) one-value-ops nil)
                      ; (car rechts) liefert nur einen Wert -->
                      ; (MV-TO-STACK) durch (PUSH) ersetzen:
                      (ersetze1 '(PUSH))
                    )
                    (case (first (car rechts))
                      ((VALUES0 STACK-TO-MV) (streiche2))
                  ) )
                  (NV-TO-STACK ; Regel 5
                    (let ((n (second (car mitte))))
                      (case (first (car rechts))
                        (STACK-TO-MV
                          (let ((m (second (car rechts))))
                            (cond ((> n m) (ersetze2 `(PUSH-NIL ,(- n m))))
                                  ((< n m) (ersetze2 `(SKIP ,(- m n))))
                                  (t (streiche2))
                        ) ) )
                        ((VALUES0 NIL) (ersetze2 `(PUSH-NIL ,n)))
                        (t (when (gethash (first (car rechts)) one-value-ops nil)
                             (erweitere2 `(PUSH) `(PUSH-NIL ,(- n 1)))
                  ) ) ) )  )
                  (PUSH-UNBOUND ; Regel 6
                    (case (first (car rechts))
                      (PUSH-UNBOUND ; (PUSH-UNBOUND n) (PUSH-UNBOUND m) --> (PUSH-UNBOUND n+m)
                        (let ((n (second (car rechts)))
                              (m (second (car mitte))))
                          (ersetze2 `(PUSH-UNBOUND ,(+ n m)))
                  ) ) ) )
            ) ) )
            (when (atom mitte) (return))
            ; Neues for-value berechnen, in Abhängigkeit von (car mitte):
            (setq for-value
              (gethash (first (car mitte)) for-value-table for-value)
            )
            ; weiterrücken:
            (setq links mitte mitte rechts)
          )
          ; Codestück zu Ende: (atom mitte)
          (when mitte
            ; mitte ist das Anfangslabel
            (let ((old-for-value (get mitte 'for-value)))
              ; Ist for-value besser als old-for-value ?
              (when (and (not (eq for-value old-for-value))
                         (or (eq old-for-value 'ALL) (eq for-value 'NIL))
                    )
                ; ja -> Anfangslabel nachher als Ergebnis bringen:
                (setf (get mitte 'for-value) for-value result mitte)
          ) ) )
        ) ; end let*
        (unless modified (return))
    ) ) ; end let, loop
    (let (codelistr)
      (when (and (eq (first (first codelist)) 'RET)
                 (consp (setq codelistr (cdr codelist)))
                 (or (eq (first (first codelistr)) 'JSR)
                     (and (eq (first (second codelist)) 'SKIP)
                          (consp (setq codelistr (cddr codelist)))
                          (eq (first (first codelistr)) 'JSR)
            )    )   )
        ; (JSR n label) [(SKIP m)] (RET) --> (JMPTAIL n n+m label)
        (let ((n (second (first codelistr)))
              (label (third (first codelistr)))
              (m (if (eq codelistr (cdr codelist)) 0 (second (second codelist)))))
          (setf (first codelist) `(JMPTAIL ,n ,(+ n m) ,label))
        )
        (remove-references (first codelistr)) ; (JSR ...) wird gestrichen
        (note-references (first codelist)) ; (JMPTAIL ...) wird eingefügt
        (setf (cdr codelist) (cdr codelistr)) ; ein bzw. zwei Listenelemente streichen
        (setq for-value-at-end 'NIL) ; JMPTAIL braucht keine Werte
    ) )
    result
) )

#|
                            3. Schritt:
                      Allgemeine Optimierungen

Wird eine Optimierung erfolgreich durchgeführt, so werden alle weiteren
Optimierungen nochmal probiert, die sich deswegen ergeben könnten.

optimize-part    - ruft den 2. Schritt auf:
                   Peephole-Optimierung normaler Operationen.

optimize-label   - Codestücke zu Labels, die nicht (mehr) referenziert werden,
                   werden entfernt.
                 - Wird ein Label nur von einem einzigen JMP referenziert,
                   der nicht vom selben Codestück kommt, können die beiden
                   betroffenen Stücke aneinandergehängt werden.

optimize-short   - Liegt ein Codestück vor, wo auf das Anfangslabel label1
                   sofort ein (JMP label2) folgt, so werden alle Referenzen
                   von label1 durch label2 ersetzt und das Codestück entfernt.
                 - Liegt ein Codestück vor, wo auf das Anfangslabel label
                   sofort ein
                   (JMPCASE/JMPCASE1-TRUE/JMPCASE1-FALSE label_true label_false)
                   folgt, so können Referenzen (JMPCASE1-TRUE label l) und
                   (JMPCASE1-FALSE l label) vereinfacht werden.
                 - Ein kurzes Codestück wird direkt an zugehörige JMPs auf
                   sein Anfangslabel angehängt. (Ein Codestück heißt "kurz",
                   wenn es höchstens 2 Befehle umfaßt und nicht mit einem
                   JMPHASH (den man nicht duplizieren sollte) abgeschlossen
                   ist.)

optimize-jmpcase - (JMPCASE label label) wird vereinfacht zu (JMP label).
                 - (NOT) [...] (JMPCASE label_true label_false) wird
                   vereinfacht zu [...] (JMPCASE label_false label_true),
                   wobei [...] nur Befehle enthalten darf, die den 1. Wert
                   nicht verändern, und bei label_true und label_false keine
                   Werte gebraucht werden.

optimize-value   - Ein Wegsprung JMPCASE1-TRUE/JMPCASE1-FALSE kann durch
                   JMPCASE ersetzt werden, wenn am Ziel-Label der Wert
                   nicht gebraucht oder nur der 1. Wert gebraucht wird.
                 - Ein Wegsprung JMPCASE/JMPCASE1-TRUE/JMPCASE1-FALSE kann
                   durch ein JMP ersetzt werden, wenn der aktuelle Wert an
                   dieser Stelle als =NIL oder als /=NIL nachgewiesen werden
                   kann.
                 - Ein JMP kann die Information, welcher Wert gerade vorliegt,
                   zu seinem Ziel-Label weitertragen.

coalesce         - Lege Codeteile mit gleichem Ende (mind. 3 Befehle) zusammen.

|#

(defun optimize-part (code)
  (let ((label (simplify code)))
    (when label
      ; Die Property for-value von label wurde verbessert.
      (dolist (ref (symbol-value label))
        (when (integerp ref) (optimize-value ref))
) ) ) )

(defun optimize-label (label &optional (index (get label 'code-part))
                                       (code (aref *code-parts* index))
                                       (lastc (last code))
                      )
  (unless (eq label (cdr lastc)) (compiler-error 'optimize-label))
  (when label
    ; label ist ein Label, es beginnt den Code
    ; code = (aref *code-parts* index), und es ist lastc = (last code).
    (let ((refs (symbol-value label))) ; Liste der Referenzen darauf
      (cond ((null refs)
              ; nicht referenziertes Label: Codestück entfernen,
              ; Referenzen aus diesem Codestück heraus eliminieren.
              (let ((labellist '())) ; Liste von Labels, die Referenzen
                                     ; verloren haben
                (loop
                  (when (atom code) (return))
                  (setq labellist
                    (nreconc labellist (remove-references (pop code) index))
                ) )
                (setf (aref *code-parts* index) nil) ; Codestück entfernen
                ; Bei Labels mit weniger Referenzen weiteroptimieren:
                ; (Vorsicht: Hierdurch kann sich *code-parts* verändern.)
                (dolist (olabel labellist)
                  (let* ((oindex (get olabel 'code-part))
                         (ocode (aref *code-parts* oindex)))
                    (when ocode
                      (optimize-label olabel oindex ocode)
                ) ) )
            ) )
            ((null (cdr refs))
              ; Label mit nur einer Referenz, und zwar durch JMP ?
              (let ((ref (first refs)))
                (when (and (integerp ref) ; Ein JMP ist ein Wegsprung
                           (eq (first (car (aref *code-parts* ref))) 'JMP)
                           (not (eql index ref)) ; aus anderem Codestück
                      )
                  ; Anhängen:
                  ; (aref *code-parts* ref) wird in die Schublade
                  ; (aref *code-parts* index) gesteckt.
                  (setf (cdr lastc) (rest (aref *code-parts* ref)))
                  (setf (aref *code-parts* ref) nil)
                  (let ((new-startlabel (cdr (last lastc)))) ; neues Startlabel von (aref *code-parts* index)
                    (when new-startlabel
                      (setf (get new-startlabel 'code-part) index)
                  ) )
                  (setf (symbol-value label) '()) ; altes Startlabel von (aref *code-parts* index) deaktivieren
                  ; neues Codestück vereinfachen:
                  (optimize-part code)
) ) ) )     ) ) )

(defun optimize-short (index &optional (code (aref *code-parts* index))
                                       (lastc (last code))
                                       (label (cdr lastc))
                      )
  (when label
    ; label ist ein Label, es beginnt den Code
    ; code = (aref *code-parts* index), und es ist lastc = (last code).
    (when (eq code lastc)
      ; Eine einzige Operation nach dem Label.
      (let ((item (car code)))
        (case (first item)
          (JMP ; (JMP ...) sofort nach dem Label
            (let ((to-label (second item)))
              (unless (eq label to-label)
                (label-subst label to-label) ; Referenzen umbiegen
                (setf (aref *code-parts* index) nil) ; Codestück entfernen
                (setf (symbol-value to-label)
                      (delete index (symbol-value to-label)) ; Referenz fällt weg
                )
                (optimize-label to-label) ; mögliche Optimierung
            ) )
            (return-from optimize-short)
          )
          ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
            (let ((true-label (second item))
                  (false-label (third item)))
              (unless (or (eq label true-label) (eq label false-label))
                (macrolet ((err () `(compiler-error 'optimize-short)))
                  ; JMPCASE1-Referenzen auf label vereinfachen:
                  (let ((modified-indices '())) ; Indizes von modifizierten Codestücken
                    (dolist (refindex (symbol-value label))
                      (when (integerp refindex)
                        (let* ((refcode (aref *code-parts* refindex))
                               (ref (car refcode)))
                          (case (first ref)
                            (JMP
                              ; (JMP label) --> (JMPCASE/... true-label false-label)
                              (setf (car refcode) item)
                              ; neue Verweise auf true-label und false-label:
                              (push refindex (symbol-value true-label))
                              (push refindex (symbol-value false-label))
                              (push refindex modified-indices)
                            )
                            ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                              ; (JMPCASE/... label1 label2)
                              (let ((label1 (second ref)) ; im TRUE-Fall: wohin springen
                                    (label2 (third ref)) ; im FALSE-Fall: wohin springen
                                    (1-true (eq (first ref) 'JMPCASE1-TRUE)) ; im TRUE-Fall: mit (VALUES1) ?
                                    (1-false (eq (first ref) 'JMPCASE1-FALSE))) ; im FALSE-Fall: mit (VALUES1) ?
                                (when (eq label label1)
                                  ; Der (JMPCASE/... label ...) wird vereinfacht zu
                                  ; (JMPCASE/... true-label ...).
                                  (setq label1 true-label)
                                  ; neuer Verweis auf true-label:
                                  (push refindex (symbol-value true-label))
                                  (push refindex modified-indices)
                                  (when (eq (first item) 'JMPCASE1-TRUE)
                                    (setq 1-true t)
                                ) )
                                (when (eq label label2)
                                  ; Der (JMPCASE/... ... label) wird vereinfacht zu
                                  ; (JMPCASE/... ... false-label).
                                  (setq label2 false-label)
                                  ; neuer Verweis auf false-label:
                                  (push refindex (symbol-value false-label))
                                  (push refindex modified-indices)
                                  (when (eq (first item) 'JMPCASE1-FALSE)
                                    (setq 1-false t)
                                ) )
                                (unless (eq (get label1 'for-value) 'ALL)
                                  (setq 1-true nil)
                                )
                                (unless (eq (get label2 'for-value) 'ALL)
                                  (setq 1-false nil)
                                )
                                (when (and 1-true 1-false)
                                  (push '(VALUES1) (cdr refcode))
                                  (setq 1-true nil 1-false nil)
                                )
                                (setf (car refcode)
                                  `(,(cond (1-true 'JMPCASE1-TRUE)
                                           (1-false 'JMPCASE1-FALSE)
                                           (t 'JMPCASE)
                                     )
                                    ,label1
                                    ,label2
                                   )
                            ) ) )
                            (JMPHASH (err)) ; JMPHASH hat undefinierte Werte
                        ) )
                        ; später:
                        ; (setf (symbol-value label) (delete refindex (symbol-value label)))
                    ) )
                    (setf (symbol-value label)
                          (delete-if #'integerp (symbol-value label))
                    )
                    ; evtl. Optimierung wegen verringerter Referenzen möglich:
                    (optimize-label label)
                    ; evtl. weitere Optimierung in veränderten Codeteilen:
                    (dolist (refindex modified-indices)
                      (simplify (aref *code-parts* refindex))
                      (optimize-value refindex)
                      (optimize-jmpcase refindex (aref *code-parts* refindex))
                    )
          ) ) ) ) )
    ) ) )
    ; Sonstige "kurze" Codestücke, maximal 2 Operationen lang:
    (when (and (or (eq code lastc) (eq (cdr code) lastc))
               (not (eq (first (car code)) 'JMPHASH))
          )
      (let ((indices '())) ; Liste der Indizes der Codestücke, an die wir code anhängen
        (setf (cdr lastc) '()) ; code vorläufig ohne das Label am Schluß
        (dolist (refindex (symbol-value label))
          (when (and (integerp refindex) (not (eql refindex index)))
            (let ((refcode (aref *code-parts* refindex)))
              (when (eq (first (car refcode)) 'JMP)
                ; anhängen:
                (let ((new-code (mapcar #'copy-list code)))
                  (dolist (op new-code) (note-references op refindex))
                  (setf (aref *code-parts* refindex) (nconc new-code (cdr refcode)))
                )
                (setf (symbol-value label) (delete refindex (symbol-value label)))
                (push refindex indices)
        ) ) ) )
        (setf (cdr lastc) label) ; wieder das Label ans Listenende setzen
        (when indices
          ; mögliche weitere Optimierungen:
          (dolist (refindex indices)
            (optimize-part (aref *code-parts* refindex))
          )
          (optimize-label label) ; label hat weniger Referenzen -> optimieren
    ) ) )
) )

; get-boolean-value versucht zu einem Anfangsstück eines Codestücks
; (einem (nthcdr n codelist) mit n>=1) zu bestimmen, welcher boolesche Wert
; nach seiner Ausführung vorliegt:
; FALSE     sicher A0 = NIL,
; TRUE      sicher A0 /= NIL,
; NIL       keine Aussage.
(defun get-boolean-value (code)
  (macrolet ((err () `(compiler-error 'get-boolean-value)))
    (let ((invert nil)) ; ob von hier bis zum Ende der boolesche Wert invertiert wird
      ((lambda (value)
         (if invert
           (case value (TRUE 'FALSE) (FALSE 'TRUE) (t NIL))
           value
       ) )
       (block value
         (loop ; Codeliste durchlaufen
           (when (atom code) (return))
           (case (first (car code))
             ((NIL VALUES0 TAGBODY-CLOSE-NIL) ; produzieren Wert NIL
               (return-from value 'FALSE) ; Damit können wir die Schleife abbrechen
             )
             ((T CONST CONS LIST) ; produzieren Wert /= NIL
               ; (CONST n), weil 1. man davon ausgehen kann, daß der Wert
               ; schon zur Compile-Zeit bekannt ist (siehe c-constantp und
               ; c-constant-value) und 2. die Konstante NIL in
               ; make-const-code bereits speziell behandelt wurde.
               ; (LIST n) wegen n>0.
               (return-from value 'TRUE) ; Damit können wir die Schleife abbrechen
             )
             (NOT (setq invert (not invert))) ; invertiere später den booleschen Wert
             ((UNBIND1 SKIP SKIPI SKIPSP STORE STOREI STOREV STOREC STOREIC SETVALUE
               VALUES1 BLOCK-CLOSE TAGBODY-CLOSE CATCH-CLOSE UNWIND-PROTECT-CLEANUP
             )) ; keine Änderung des 1. Werts -> weiter in der Codeliste
             (t (return-from value nil))
           )
           (setq code (cdr code))
         )
         (when code
           ; code ist das Anfangslabel.
           ; Inspiziere alle Sprünge auf das Label code:
           (let ((bisher nil))
             ; bisher = FALSE, falls bisher alle Sprünge den booleschen Wert
             ;                 FALSE mitbringen,
             ; bisher = TRUE, falls bisher alle Sprünge den booleschen Wert
             ;                TRUE mitbringen,
             ; bisher = NIL am Anfang.
             ; Falls ein Sprung einen unbekannten booleschen Wert mitbringt,
             ; kann man die Schleife gleich verlassen.
             (flet ((neu (value)
                      (cond ((null bisher) (setq bisher value))
                            ((not (eq value bisher)) (return-from value nil))
                   )) )
               (dolist (ref (symbol-value code))
                 (if (integerp ref)
                   (let ((refcode (first (aref *code-parts* ref)))) ; der Wegsprung hierher
                     ; Ein Wegsprung mit undefinierten Werten kann das nicht sein.
                     (case (first refcode)
                       (JMP
                         (if (third refcode)
                           ; Wert vor dem Sprung bekannt
                           (neu (third refcode))
                           ; Wert vor dem Sprung unbekannt
                           (return-from value nil)
                       ) )
                       ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                         (when (eq code (second refcode)) (neu 'TRUE))
                         (when (eq code (third refcode)) (neu 'FALSE))
                       )
                       (t (err)) ; JMPHASH hat undefinierte Werte, und die
                                 ; anderen Wegsprünge enthalten keine Labels.
                   ) )
                   (case (first ref)
                     ((JMPIFBOUNDP BLOCK-OPEN CATCH-OPEN)
                       (return-from value nil) ; Da können wir nichts aussagen
                     )
                     (t (err)) ; An den Labels in TAGBODY-OPEN, JSR,
                               ; UNWIND-PROTECT-OPEN, UNWIND-PROTECT-CLOSE
                               ; liegen undefinierte Werte vor.
         ) ) ) ) ) )
         nil ; Default: nichts aussagbar
      ))
) ) )

(defun optimize-jmpcase (index code)
  (when (eq (first (car code)) 'JMPCASE)
    ; Code endet mit (JMPCASE ...)
    (let ((true-label (second (car code)))
          (false-label (third (car code))))
      (if (eq true-label false-label)
        ; (JMPCASE label label) --> (JMP label ..)
        (progn
          (setf (car code) `(JMP ,true-label ,(get-boolean-value (cdr code))))
          ; doppelte Referenz wird zu einer einfachen:
          (setf (symbol-value true-label)
                (delete index (symbol-value true-label) :count 1)
          )
          ; und weiter optimieren:
          (optimize-part code)
          (optimize-short (get true-label 'code-part))
        )
        (when (and (null (get true-label 'for-value))
                   (null (get false-label 'for-value))
              )
          ; Versuche NOTs zu eliminieren:
          (let ((invert 0)
                (cr1 code)
                (cr2 (cdr code))) ; stets cr2 = (cdr cr1)
            (loop
              (when (atom cr2) (return))
              (case (first (car cr2))
                ((UNBIND1 SKIP SKIPI SKIPSP VALUES1 BLOCK-CLOSE TAGBODY-CLOSE
                  CATCH-CLOSE UNWIND-PROTECT-CLEANUP
                 ) ; diese Operationen brauchen keine Werte und lassen
                   ; den 1. Wert unverändert
                 (shiftf cr1 cr2 (cdr cr2))
                )
                (NOT
                  (setf (cdr cr1) (setq cr2 (cdr cr2))) ; (NOT) streichen
                  (incf invert)
                )
                (t (return))
            ) )
            ; invert = Anzahl, wie oft (NOT) gestrichen wurde
            (when (oddp invert)
              ; true-label und false-label vertauschen:
              (setf (car code) `(JMPCASE ,false-label ,true-label))
            )
            (when (plusp invert)
              ; und weiter optimieren:
              (optimize-part code)
              (optimize-short index)
        ) ) )
) ) ) )

(defun optimize-value (index &optional (code (aref *code-parts* index)))
  (let ((item (car code)))
    (case (first item)
      ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
        ; (JMPCASE/... true-label false-label)
        (let ((true-label (second item))
              (false-label (third item)))
          (when (or (and (eq (first item) 'JMPCASE1-TRUE)
                         (not (eq (get true-label 'for-value) 'ALL))
                         ; Wertezahl 1 wird bei true-label nicht gebraucht
                         ; (JMPCASE1-TRUE ...) --> (JMPCASE ...)
                    )
                    (and (eq (first item) 'JMPCASE1-FALSE)
                         (not (eq (get false-label 'for-value) 'ALL))
                         ; Wertezahl 1 wird bei false-label nicht gebraucht
                         ; (JMPCASE1-FALSE ...) --> (JMPCASE ...)
                )   )
            (setq item (setf (car code) `(JMPCASE ,@(rest item))))
            ; Weitere mögliche Optimierungen:
            (optimize-jmpcase index code)
          )
          ; Versuche, den booleschen Wert an dieser Stelle zu ermitteln
          ; und vereinfache gegebenenfalls:
          (case (get-boolean-value (cdr code))
            (TRUE ; Sprung geht immer auf true-label
              ; Referenz auf false-label streichen:
              (setf (symbol-value false-label)
                (delete index (symbol-value false-label))
              )
              (setf (car code) `(JMP ,true-label TRUE))
              (when (eq (first item) 'JMPCASE1-TRUE)
                (push '(VALUES1) (cdr code))
                (simplify code)
              )
              (optimize-part code) ; weitere mögliche Optimierung
              ; weitere mögliche Optimierungen:
              (optimize-label false-label) ; wegen verringerter Referenzen
              (optimize-short index code) ; wegen obigem optimize-part
            )
            (FALSE
              ; Referenz auf true-label streichen
              (setf (symbol-value true-label)
                (delete index (symbol-value true-label))
              )
              (setf (car code) `(JMP ,false-label FALSE))
              (when (eq (first item) 'JMPCASE1-FALSE)
                (push '(VALUES1) (cdr code))
                (simplify code)
              )
              (optimize-part code) ; weitere mögliche Optimierung
              ; weitere mögliche Optimierungen:
              (optimize-label true-label) ; wegen verringerter Referenzen
              (optimize-short index code) ; wegen obigem optimize-part
      ) ) ) )
      (JMP
        (let ((label (second item)))
          (when (get label 'for-value)
            ; Wert wird benötigt
            (when (null (third item))
              ; aber er ist unbekannt.
              ; Vielleicht läßt sich der Wert herausbekommen ?
              (let ((value (get-boolean-value (cdr code))))
                (when value
                  (setf (car code) `(JMP ,label ,value))
                  ; Wert jetzt bekannt, läßt sich vielleicht verwenden:
                  (optimize-value (get label 'code-part))
) ) ) ) ) ) ) ) )

; coalesce legt gleiche Codeteile in den gegebenen Codestücken soweit wie
; möglich zusammen und liefert als Ergebnis ein Flag, ob etwas geändert wurde.
(defun coalesce (&optional (indexlist
                             ; Liste aller möglichen Indizes
                             (let ((L '()))
                               (dotimes (i (fill-pointer *code-parts*)) (push i L))
                               (nreverse L)
                )          ) )
  (let ((parts-ht ; Eine Hashtabelle, die eine Abbildung realisiert:
                  ; Codeende --> Liste aller Indizes von Codestücken,
                  ;              die damit enden
          (let ((ht (make-hash-table :test #'equal :size (length indexlist))))
            (dolist (index indexlist)
              (let ((code (aref *code-parts* index))) ; ein Codestück
                ; Wegen der Vereinfachungsregel für "kurze" Codestücke werden
                ; nur Teile zusammengelegt, die in mindestens den letzten 3
                ; Operationen übereinstimmen.
                (when (and (consp code) (consp (cdr code)) (consp (cddr code)))
                  (push index
                    (gethash (list* (first code) (second code) (third code))
                             ht '()
                  ) )
            ) ) )
            ht
        ) )
        (modified nil))
    ; Dann über die möglichen Codeenden iterieren:
    (maphash
      #'(lambda (code-beginning indices)
          (declare (ignore code-beginning))
          (when (cdr indices) ; mindestens zwei Indizes mit diesem Codeende?
            ; Versuche, möglichst langes Codestück zusammenzulegen:
            (let ((codes ; Liste der zusammenzulegenden Codestücke
                    (mapcar #'(lambda (i) (aref *code-parts* i)) indices)
                  )
                  (new-code '()) ; hier wird der gemeinsame Code gesammelt
                  (new-index (fill-pointer *code-parts*)) ; Index dafür
                  (new-order ; das gemeinsame Stück wird beim letzten Teil einzusortiert
                    (reduce #'max (mapcar #'(lambda (i) (aref *code-positions* i)) indices))
                 ))
              (loop
                ; stimmen noch alle überein?
                (unless (every #'consp codes) (return))
                (let* ((code1 (first codes)) ; ein beliebiges der Codestücke
                       (code11 (car code1))) ; dessen letzte Operation
                  (unless (every #'(lambda (code) (equal (car code) code11))
                                 (rest codes)
                          )
                    (return)
                  )
                  ; ja. Alle Codestücke aus codes um eine Operation verkürzen:
                  (mapc #'(lambda (code index) ; Referenzen löschen
                            (remove-references (car code) index)
                          )
                        codes indices
                  )
                  ; verkürzen: (setq codes (mapcar #'cdr codes)), oder:
                  (mapl #'(lambda (codesr)
                            (setf (car codesr) (cdr (car codesr)))
                          )
                        codes
                  )
                  (push code11 new-code) ; new-code verlängern
                  (note-references code11 new-index)
              ) )
              (let* ((new-label (make-label 'ALL))
                     ; Alle Codestücke aus codes wurden verkürzt, sie werden
                     ; jetzt verlängert um ein (JMP new-label NIL).
                     (jmpop `(JMP ,new-label NIL)))
                (mapc #'(lambda (code index)
                          (setf (aref *code-parts* index) (cons jmpop code))
                        )
                      codes indices
                )
                (setf (symbol-value new-label) indices) ; Referenzen auf new-label
                (setf (get new-label 'code-part) new-index)
                (vector-push-extend (nreconc new-code new-label) *code-parts*)
                (vector-push-extend new-order *code-positions*)
              )
              ; weitere mögliche Optimierungen:
              (optimize-part (aref *code-parts* new-index))
              (coalesce indices)
              (setq modified t) ; Veränderung hat stattgefunden
        ) ) )
      parts-ht
    )
    modified
) )

; Die Hauptfunktion des 2. Schritts:
; Führt alle Optimierungen durch, und faßt dann alle Codestücke wieder zu
; einer einzigen Codeliste zusammen und liefert diese.
(defun optimize-all ()
  ; Optimierungen:
  (loop
    ; Optimierungen aufrufen:
    ; Wird eine fündig, so ruft sie auch gleich die Optimierungs-
    ; schritte auf, die sich dadurch ergeben könnten. Daher brauchen
    ; sie hier nur einmal aufgeführt zu werden.
    ; Vorsicht hier: durch die Optimierungen können *code-parts* und sein
    ; Inhalt sich völlig verändern.
    (do ((index 0 (1+ index)))
        ((eql index (fill-pointer *code-parts*)))
      (let ((code (aref *code-parts* index)))
        (when code
          (let* ((lastc (last code))
                 (label (cdr lastc)))
            (when label
              (unless (eql index (get label 'code-part))
                (compiler-error 'optimize-all 'code-part)
            ) )
            (optimize-label label index code lastc)
      ) ) )
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-jmpcase index code)
      ) )
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-value index code)
      ) )
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-short index code)
    ) ) )
    (unless (coalesce) (return)) ; (coalesce) tat nichts -> fertig
  )
  ; Zu einer einzigen Codeliste zusammenfassen:
  ; (Dabei werden die Labels nun Listenelemente im Code statt nur NTHCDRs.)
  (let ((start-index 0)) ; Start-"Label" NIL beginnt Codestück Nr. 0
    ; Erst jeweils ein Codestück, das mit label anfängt, wenn möglich an ein
    ; Codestück anhängen, das mit einem JMP oder JMPCASE/... zu label endet.
    (do ((index (fill-pointer *code-parts*)))
        ((eql (decf index) 0)) ; index durchläuft die Indizes von *code-parts*
                               ; von oben nach unten, ausgenommen start-index=0.
      (let ((code (aref *code-parts* index)))
        (when code
          (loop
            ; Betrachte das Label am Ende von code, im Codestück Nr. index:
            (let* ((lastc (last code)) ; letztes Cons von code
                   (label (cdr lastc)) ; Label am Ende von code
                   (refs (symbol-value label)) ; Referenzen darauf
                   (pos (aref *code-positions* index)) ; Position von code
                   (jmp-ref nil) ; bisher beste gefundene JMP-Referenz auf label
                   (jmpcase-ref nil) ; bisher beste gefundene JMPCASE-Referenz auf label
                   (jmpcase1-ref nil)) ; bisher beste gefundene JMPCASE1-...-Referenz auf label
              (if (null label)
                ; Das Start-Code-Stück wurde umgehängt!
                (progn
                  (setq start-index index)
                  (return) ; zum nächsten Index
                )
                (flet ((better (new-ref old-ref)
                         ; Eine Referenz new-ref ist "besser" als eine andere
                         ; old-ref, wenn sie näher dran ist. Dabei haben
                         ; Vorwärtsreferenzen generell Priorität gegenüber
                         ; Rückwärtsreferenzen.
                         (or (null old-ref) ; noch gar kein old-ref?
                             (let ((old-pos (aref *code-positions* old-ref))
                                   (new-pos (aref *code-positions* new-ref)))
                               (if (> old-pos pos) ; Habe bisher nur Rückwärtssprung?
                                 ; ja: new-pos ist besser, falls es
                                 ; < pos (Vorwärtssprung) oder
                                 ; >=pos, <=old-pos (kürzerer Rückwärtssprung) ist.
                                 (<= new-pos old-pos)
                                 ; nein: new-pos ist besser, falls es
                                 ; <=pos, >=old-pos (kürzerer Vorwärtssprung) ist.
                                 (<= old-pos new-pos pos)
                      )) )   ) )
                  (macrolet ((update (old-ref new-ref) ; zur Bestimmung des bisher Besten
                               `(when (better ,new-ref ,old-ref)
                                  (setq ,old-ref ,new-ref)
                                )
                            ))
                    ; Bestimme die beste Referenz, an die das Codestück
                    ; gehängt werden kann:
                    (dolist (refindex refs)
                      (when (and (integerp refindex)
                                 (not (eql refindex index)) ; nicht an sich selber hängen!
                            )
                        (let ((refcode1 (car (aref *code-parts* refindex))))
                          (case (first refcode1)
                            (JMP ; mögliches Anhängen an (JMP label ...)
                              (update jmp-ref refindex)
                            )
                            (JMPCASE ; mögliches Anhängen an (JMPCASE ... label ...)
                              (update jmpcase-ref refindex)
                            )
                            (JMPCASE1-TRUE ; mögliches Anhängen an (JMPCASE1-TRUE ... label)
                              (when (eq label (third refcode1))
                                (update jmpcase1-ref refindex)
                            ) )
                            (JMPCASE1-FALSE ; mögliches Anhängen an (JMPCASE1-FALSE label ...)
                              (when (eq label (second refcode1))
                                (update jmpcase1-ref refindex)
                            ) )
                    ) ) ) )
                    (cond (jmp-ref ; an (JMP label) anhängen
                            (setf (cdr lastc)
                                  (cons label (cdr (aref *code-parts* jmp-ref)))
                            )
                            (setf (aref *code-parts* jmp-ref) nil)
                            (setq code lastc)
                          )
                          (jmpcase1-ref
                            (let* ((refcode (aref *code-parts* jmpcase1-ref))
                                   (refcode1 (car refcode))
                                   (jmpop
                                     (if (eq label (second refcode1))
                                       `(JMPIFNOT1 ,(third refcode1))
                                       `(JMPIF1 ,(second refcode1))
                                  )) )
                              (setf (cdr lastc) (list* label jmpop (cdr refcode)))
                              (setf (aref *code-parts* jmpcase1-ref) nil)
                              (setq code lastc)
                          ) )
                          (jmpcase-ref
                            (let* ((refcode (aref *code-parts* jmpcase-ref))
                                   (refcode1 (car refcode))
                                   (for-value (or (get (second refcode1) 'for-value)
                                                  (get (third refcode1) 'for-value)
                                   )          )
                                   (jmpop
                                     (if (eq label (second refcode1))
                                       `(JMPIFNOT ,(third refcode1) ,for-value)
                                       `(JMPIF ,(second refcode1) ,for-value)
                                  )) )
                              (setf (cdr lastc) (list* label jmpop (cdr refcode)))
                              (setf (aref *code-parts* jmpcase-ref) nil)
                              (setq code lastc)
                          ) )
                          (t ; kein Anhängen möglich
                            (return) ; zum nächsten Index
          ) ) ) ) ) )     )
    ) ) )
    ; Sicherstellen, daß das Anfangs-Stück auch an den Anfang kommt:
    ; (Das würde auch gehen, indem bei jeder der obigen Anhängungen
    ; ein (setf (aref *code-positions* index) (aref *code-positions* jmp..-ref))
    ; gemacht würde. Wieso tun wir das nicht??)
    (setf (aref *code-positions* start-index) 0)
    ; Codeliste zusammensetzen:
    (let ((code-parts (map 'list #'cons *code-parts* *code-positions*)))
      (setq code-parts (delete-if-not #'car code-parts)) ; code=nil bedeutet: gestrichen
      (setq code-parts (sort code-parts #'> :key #'cdr)) ; nach Reihenfolge sortieren
      ; Die Teile sind jetzt in der richtigen Ordnung, nur umgekehrt.
      (let ((codelist '()))
        (dolist (code-part code-parts)
          (let ((code (car code-part)))
            ; code an codelist anhängen, dabei aber den Wegsprung umwandeln:
            (let ((item (car code)))
              (case (first item)
                (JMP (setf (car code) `(JMP ,(second item))))
                (JMPCASE ; (JMPCASE true-label false-label)
                         ; --> (JMPIFNOT false-label fv) (JMP true-label)
                  (setq code
                    (list* `(JMP ,(second item))
                           `(JMPIFNOT ,(third item)
                                      ,(or (get (second item) 'for-value)
                                           (get (third item) 'for-value)
                                       )
                            )
                           (cdr code)
                ) ) )
                (JMPCASE1-TRUE ; (JMPCASE1-TRUE true-label false-label)
                               ; --> (JMPIF1 true-label) (JMP false-label)
                  (setq code
                    (list* `(JMP ,(third item))
                           `(JMPIF1 ,(second item))
                           (cdr code)
                ) ) )
                (JMPCASE1-FALSE ; (JMPCASE1-FALSE true-label false-label)
                                ; --> (JMPIFNOT1 false-label) (JMP true-label)
                  (setq code
                    (list* `(JMP ,(second item))
                           `(JMPIFNOT1 ,(third item))
                           (cdr code)
            ) ) ) ) )
            ; Label zum Listenelement machen:
            (let ((lastc (last code)))
              (when (cdr lastc)
                (setf (cdr lastc) (list (cdr lastc)))
            ) )
            ; Umdrehen und vor codelist hängen (deswegen wurde vorhin
            ; mit #'> statt #'< sortiert):
            (setq codelist (nreconc code codelist))
        ) )
        codelist
) ) ) )

#| Was ist mit den folgenden möglichen Optimierungen??

10. Kommt vor einem (JMP label) ein (UNWIND-PROTECT-CLEANUP) und vor dem
   label ein (UNWIND-PROTECT-3 cleanup-label), so muß es sich um denselben
   UNWIND-PROTECT-Frame handeln, und man kann (UNWIND-PROTECT-CLEANUP)
   streichen und (JMP label) durch (JMP newlabel) ersetzen, wobei newlabel
   ein neues Label ist, das vor dem (evtl. zu ergänzenden) (UNWIND-PROTECT-2)
   vor cleanup-label sitzt:
   (UNWIND-PROTECT-CLEANUP) (JMP label) ...
   ... [(UNWIND-PROTECT-2)] cleanup-label ... (UNWIND-PROTECT-3 cleanup-label) label
   -->
   (JMP newlabel) ...
   ... newlabel (UNWIND-PROTECT-2) cleanup-label ... (UNWIND-PROTECT-3 cleanup-label) label

11. Kommt nach einem Label label ein (NIL), so darf jeder (JMPIFNOT label)
   und jeder (JMPIFNOT1 label) durch ein (JMPIFNOT1 z) ersetzt werden,
   wo z ein neues Label nach dem (NIL) ist:
          (JMPIFNOT label) ... label (NIL) ...
   -->       (JMPIFNOT1 z) ... label (NIL) z ...

|#

; Führt den 1. und 2. Schritt aus:
(defun compile-to-LAP ()
  (let ((*code-parts* (make-array 10 :adjustable t :fill-pointer 0))
        (*code-positions* (make-array 10 :adjustable t :fill-pointer 0)))
    ; Expandiert den Code des Fnode *func* und teilt ihn in Stücke auf.
    ; Hinterläßt seine Werte in *code-parts* und *code-positions*.
    (let ((*code-part* (list '(START))) ; NIL als Start-"Label"
          (*code-index* 0)
          (*dead-code* nil)
          (*label-subst* '())
          (*current-value* nil)
          (*current-vars* '()))
      (traverse-anode (anode-code (fnode-code *func*)))
    )
    ; Optimiert in *code-parts* und *code-positions*, faßt dann den Code
    ; in einer Liste zusammen und liefert diese:
    (let ((code-list (optimize-all)))
      (unless (equal (pop code-list) '(START))
        (compiler-error 'compile-to-LAP 'start)
      )
      code-list
) ) )


#|
                            4. Schritt:
                   Bestimmung des Stackbedarfs

Dieser Schritt bestimmt, wieviel SP-Einträge die Funktion maximal braucht.
|#

#+CLISP3
(defun SP-depth (code-list)
  (let ((max-depth 0) ; bisherige Maximal-Tiefe
        (unseen-label-alist '()) ; Labels, ab denen noch verfolgt werden muß
        (seen-label-alist '()) ; Labels, die schon verfolgt wurden
          ; jeweils Aliste ((label . depth) ...)
          ; Es ist durchaus möglich, daß dasselbe Codestück mit unterschied-
          ; lichen SP-Tiefen durchgeführt werden kann (nämlich dann, wenn es
          ; mit einem Wegsprung THROW, RETURN-FROM, GO oder ERROR endet)!
          ; seen-label-alist enthält zu jedem Label die maximale Tiefe, mit
          ; der ab diesem Label schon verfolgt wurde.
          ; unsee-label-alist enthält zu jedem Label die maximale bisher
          ; notierte Tiefe, mit der ab diesem Label noch verfolgt werden muß.
        (mitte code-list) ; restliche Codeliste
        (depth 0) ; aktuelle Tiefe
       )
    (macrolet ((check-depth (wanted-depth)
                 ; überprüft, ob depth gleich der Tiefe wanted-depth ist
                 `(unless (eql depth ,wanted-depth)
                    (compiler-error 'SP-depth)
                  )
              ))
      (loop
        ; mitte läuft durch die Codeliste, von der aktuellen Position
        ; bis zum nächsten Wegsprung, und zählt die Tiefe mit.
        (loop
          (when (null mitte) (return))
          (let ((item (car mitte)))
            (if (atom item)
              ; Label
              (let ((h (assoc item seen-label-alist)))
                (if h
                  (if (<= depth (cdr h)) (return) (setf (cdr h) depth))
                  (push (cons item depth) seen-label-alist)
              ) )
              ; Instruktion
              (macrolet ((note-label (labelform)
                           ; notiere, daß zu label gesprungen werden kann
                           (let ((label (gensym)))
                             `(let* ((,label ,labelform)
                                     (h (assoc ,label seen-label-alist)))
                                (unless (and h (<= depth (cdr h)))
                                  (setq h (assoc ,label unseen-label-alist))
                                  (if h
                                    (unless (<= depth (cdr h)) (setf (cdr h) depth))
                                    (push (cons ,label depth) unseen-label-alist)
                              ) ) )
                         ) )
                         (note-inc (amount)
                           ; notiere, daß depth um amount erhöht wird
                           `(progn
                              (incf depth ,amount)
                              (when (> depth max-depth) (setq max-depth depth))
                            )
                         )
                         (note-dec (amount)
                           ; notiere, daß depth um amount erniedrigt wird
                           `(progn
                              (decf depth ,amount)
                              (when (minusp depth) (compiler-error 'SP-depth "<0"))
                            )
                         )
                         (note-jmp ()
                           ; notiere, daß weggesprungen wird
                           `(return)
                        ))
                (case (first item)
                  (JMP ; (JMP label)
                    (note-label (second item))
                    (note-jmp)
                  )
                  ((JMPIF JMPIF1 JMPIFNOT JMPIFNOT1 JMPIFBOUNDP) ; (JMP... label)
                    (note-label (second item))
                  )
                  ((JMPHASH JMPTAIL) ; (JMPHASH n ht label . labels), (JMPTAIL m n label)
                    (dolist (label (cdddr item)) (note-label label))
                    (note-jmp)
                  )
                  (JSR ; (JSR n label)
                    (let ((depth 0)) (note-label (third item)))
                  )
                  ((THROW RETURN-FROM GO ERROR) ; (THROW), (RETURN-FROM n), (GO n k), (ERROR n)
                    (note-jmp)
                  )
                  (RET ; (RET)
                    (check-depth 0)
                    (note-jmp)
                  )
                  (PROGV ; (PROGV)
                    (note-inc 1)
                  )
                  (CATCH-OPEN ; (CATCH-OPEN label)
                    (note-label (second item))
                    (note-inc (+ 2 *jmpbuf-size*))
                  )
                  (CATCH-CLOSE ; (CATCH-CLOSE)
                    (note-dec (+ 2 *jmpbuf-size*))
                  )
                  (UNWIND-PROTECT-OPEN ; (UNWIND-PROTECT-OPEN label)
                    ; eigentlich: (note-inc (+ 2 *jmpbuf-size*))
                    (note-inc 3) (note-label (second item)) (note-dec 3)
                    (note-inc (+ 2 *jmpbuf-size*))
                  )
                  (UNWIND-PROTECT-NORMAL-EXIT ; (UNWIND-PROTECT-NORMAL-EXIT), danach kommt label
                    (note-dec (+ 2 *jmpbuf-size*)) (note-inc 3)
                  )
                  (UNWIND-PROTECT-CLOSE ; (UNWIND-PROTECT-CLOSE label)
                    ; eigentlich: (note-dec 3)
                    (note-label (second item)) (note-dec 3)
                  )
                  (UNWIND-PROTECT-CLEANUP ; (UNWIND-PROTECT-CLEANUP)
                    ; eigentlich: (note-dec (+ 2 *jmpbuf-size*)) (note-inc 3) ... (note-dec 3)
                    (note-dec (+ 2 *jmpbuf-size*))
                  )
                  (BLOCK-OPEN ; (BLOCK-OPEN n label)
                    (note-label (third item))
                    (note-inc (+ 2 *jmpbuf-size*))
                  )
                  (BLOCK-CLOSE ; (BLOCK-CLOSE)
                    (note-dec (+ 2 *jmpbuf-size*))
                  )
                  (TAGBODY-OPEN ; (TAGBODY-OPEN m label1 ... labelm)
                    (note-inc (+ 1 *jmpbuf-size*))
                    (dolist (label (cddr item)) (note-label label))
                  )
                  ((TAGBODY-CLOSE-NIL TAGBODY-CLOSE) ; (TAGBODY-CLOSE-NIL), (TAGBODY-CLOSE)
                    (note-dec (+ 1 *jmpbuf-size*))
                  )
                  (MVCALLP ; (MVCALLP)
                    (note-inc 1)
                  )
                  (MVCALL ; (MVCALL)
                    (note-dec 1)
                  )
                  (SKIPSP ; (SKIPSP k)
                    (note-dec (second item))
                  )
                  (SKIPI ; (SKIPI k n)
                    (note-dec (+ (second item) 1))
                  )
              ) )
          ) )
          (setq mitte (cdr mitte))
        )
        ; Nächstes zu verfolgendes Label suchen:
        (loop
          (when (null unseen-label-alist) ; fertig ?
            (return-from SP-depth max-depth)
          )
          (let* ((unseen (pop unseen-label-alist)) ; nächstes zu verfolgendes
                 (label (car unseen))) ; Label
            (setq depth (cdr unseen))
            (let ((h (assoc label seen-label-alist)))
              (unless (and h (<= depth (cdr h)))
                ; Ab diesem Label die Codeliste abarbeiten:
                ; (Dadurch wird (label . depth) in seen-label-alist aufgenommen,
                ; es ist bereits aus unseen-label-alist entfernt.)
                (setq mitte (member label code-list :test #'eq))
                (return)
        ) ) ) )
) ) ) )


#|
                            5. Schritt:
                 Einführung von Kurz-Operationen

Dieser Schritt arbeitet auf der Codeliste und verändert sie dabei destruktiv.

1. (ATOM) (JMPIF label NIL)             --> (JMPIFATOM label)
   (ATOM) (JMPIFNOT label NIL)          --> (JMPIFCONSP label)
   (CONSP) (JMPIF label NIL)            --> (JMPIFCONSP label)
   (CONSP) (JMPIFNOT label NIL)         --> (JMPIFATOM label)
   (ATOM)                               --> (PUSH) (CALLS ATOM)
   (CONSP)                              --> (PUSH) (CALLS CONSP)

2. (NIL) (PUSH)                         --> (NIL&PUSH)
   (NIL) (PUSH) ... (NIL) (PUSH)        --> (PUSH-NIL n)
   (NIL) (STORE n)                      --> (NIL&STORE n)
   (PUSH-NIL 1)                         --> (NIL&PUSH)

3. (T) (PUSH)                           --> (T&PUSH)
   (T) (STORE n)                        --> (T&STORE n)

4. (CONST n) (PUSH)                     --> (CONST&PUSH n)
   (CONST n) (SYMBOL-FUNCTION) (PUSH)   --> (CONST&SYMBOL-FUNCTION&PUSH n)
   (CONST n) (SYMBOL-FUNCTION) (STORE m)--> (CONST&SYMBOL-FUNCTION&STORE n m)
   (CONST n) (SYMBOL-FUNCTION)          --> (CONST&SYMBOL-FUNCTION n)

5. (COPY-CLOSURE n m) (PUSH)            --> (COPY-CLOSURE&PUSH n m)

6. (LOAD n) (PUSH)                      --> (LOAD&PUSH n)
   (LOAD k) (STOREC n m)                --> (LOAD&STOREC k n m)
   (LOAD n) (JMPIF label fv)            --> (LOAD&JMPIF n label)
   (LOAD n) (JMPIFNOT label fv)         --> (LOAD&JMPIFNOT n label)
   (LOAD n) (CAR) (PUSH)                --> (LOAD&CAR&PUSH n)
   (LOAD n) (CDR) (PUSH)                --> (LOAD&CDR&PUSH n)
   (LOAD n) (CDR) (STORE n)             --> (LOAD&CDR&STORE n)
   (LOAD n+1) (CONS) (STORE n)          --> (LOAD&CONS&STORE n)
   (LOAD n) (PUSH) (CALLS 1+) (STORE n) --> (LOAD&INC&STORE n)
   (LOAD n) (PUSH) (CALLS 1-) (STORE n) --> (LOAD&DEC&STORE n)
   (LOAD n) (PUSH) (CALLS 1+) (PUSH)    --> (LOAD&INC&PUSH n)
   (LOAD n) (PUSH) (CALLS 1-) (PUSH)    --> (LOAD&DEC&PUSH n)
   (LOAD n) (CAR) (STORE m)             --> (LOAD&CAR&STORE n m)

7. (JMPIFBOUNDP n l) (NIL) (STORE n) l  --> (UNBOUND->NIL n) l

8. (LOADI n1 n2) (PUSH)                 --> (LOADI&PUSH n1 n2)
   (LOADC n1 n2) (PUSH)                 --> (LOADC&PUSH n1 n2)
   (LOADV n1 n2) (PUSH)                 --> (LOADV&PUSH n1 n2)

9. (GETVALUE n) (PUSH)                  --> (GETVALUE&PUSH n)

10. (UNBIND1) ... (UNBIND1)             --> (UNBIND n)

11. (CAR) (PUSH)                        --> (CAR&PUSH)
    (CDR) (PUSH)                        --> (CDR&PUSH)
    (CONS) (PUSH)                       --> (CONS&PUSH)
    (LIST n) (PUSH)                     --> (LIST&PUSH n)
    (FUNCALL n) (PUS)                   --> (FUNCALL&PUSH n)
    (APPLY n) (PUSH)                    --> (APPLY&PUSH n)

12. (POP) (STORE n)                      --> (POP&STORE n)

13. (SKIP n) (RET)                      --> (SKIP&RET n)
    ; (RET)                             --> (SKIP&RET 0)
    ; kommt nicht vor, da im Stack stets noch die Closure selbst sitzt

14. (UNWIND-PROTECT-CLOSE label)        --> (UNWIND-PROTECT-CLOSE)

15. (JMPHASH n ht label . labels)       --> (JMPHASH n ht label)

16. (JSR n label)                       --> (JSR label)
    (JSR n label) (PUSH)                --> (JSR&PUSH label)

17. (CALL m n) (PUSH)                   --> (CALL&PUSH m n)
    (CALL1 n) (PUSH)                    --> (CALL1&PUSH n)
    (CALL2 n) (PUSH)                    --> (CALL2&PUSH n)
    (CALLS1 n) (PUSH)                   --> (CALLS1&PUSH n)
    (CALLS2 n) (PUSH)                   --> (CALLS2&PUSH n)
    (CALLSR m n) (PUSH)                 --> (CALLSR&PUSH m n)
    (CALLC) (PUSH)                      --> (CALLC&PUSH)
    (CALLCKEY) (PUSH)                   --> (CALLCKEY&PUSH)

18. (CALL1 n) (JMPIF label fv)          --> (CALL1&JMPIF n label)
    (CALL1 n) (JMPIFNOT label fv)       --> (CALL1&JMPIFNOT n label)
    (CALL2 n) (JMPIF label fv)          --> (CALL2&JMPIF n label)
    (CALL2 n) (JMPIFNOT label fv)       --> (CALL2&JMPIFNOT n label)
    (CALLS1 n) (JMPIF label fv)         --> (CALLS1&JMPIF n label)
    (CALLS1 n) (JMPIFNOT label fv)      --> (CALLS1&JMPIFNOT n label)
    (CALLS2 n) (JMPIF label fv)         --> (CALLS2&JMPIF n label)
    (CALLS2 n) (JMPIFNOT label fv)      --> (CALLS2&JMPIFNOT n label)
    (CALLSR m n) (JMPIF label fv)       --> (CALLSR&JMPIF m n label)
    (CALLSR m n) (JMPIFNOT label fv)    --> (CALLSR&JMPIFNOT m n label)

19. (CALLS1 n) (STORE k)                --> (CALLS1&STORE n k)
    (CALLS2 n) (STORE k)                --> (CALLS2&STORE n k)
    (CALLSR m n) (STORE k)              --> (CALLSR&STORE m n k)

20. (EQ) (JMPIF label NIL)              --> (JMPIFEQ label)
    (EQ) (JMPIFNOT label NIL)           --> (JMPIFNOTEQ label)
    (CONST n) (EQ) (JMPIF label NIL)    --> (JMPIFEQTO n label)
    (CONST n) (EQ) (JMPIFNOT label NIL) --> (JMPIFNOTEQTO n label)

|#

(let ((CALLS-1+ (CALLS-code (gethash '1+ function-codes)))
      (CALLS-1- (CALLS-code (gethash '1- function-codes)))
      (CALLS-atom (CALLS-code (gethash 'atom function-codes)))
      (CALLS-consp (CALLS-code (gethash 'consp function-codes))))
  (defun insert-combined-LAPs (code-list)
    ; Zunächst die ATOM/CONSP-Umwandlung, weil diese PUSHs einführen kann:
    (do ((crest code-list (cdr crest)))
        ((null crest))
      (let ((item (car crest)))
        (when (and (consp item)
                   (memq (setq item (first item)) '(ATOM CONSP))
              )
          (if (and #| (consp (cdr crest)) |#
                   (consp (cadr crest))
                   (memq (first (cadr crest)) '(JMPIF JMPIFNOT))
                   (null (third (cadr crest)))
              )
            ; z.B. (ATOM) (JMPIF label NIL) --> (JMPIFATOM label)
            (setf (car crest)
                  `(,(if (eq (first (cadr crest)) 'JMPIF)
                       (if (eq item 'ATOM) 'JMPIFATOM 'JMPIFCONSP)
                       (if (eq item 'ATOM) 'JMPIFCONSP 'JMPIFATOM)
                     )
                    ,(second (cadr crest))
                   )
                  (cdr crest) (cddr crest)
            )
            ; z.B. (ATOM) --> (PUSH) (CALLS ATOM)
            (setf (car crest) '(PUSH)
                  (cdr crest) (cons (if (eq item 'ATOM) CALLS-atom CALLS-consp)
                                    (cdr crest)
            )                 )
    ) ) ) )
    ; Nun die sonstigen Umformungen: Ein einziger Durchlauf.
    ; Zwei Pointer laufen durch die Codeliste: ...mitte.rechts...
    (do* ((mitte code-list rechts)
          (rechts (cdr mitte) (cdr rechts)))
         ((null mitte))
      (macrolet ((ersetze (length new-code)
                   ; ersetzt die nächsten length Elemente
                   ; (nth 0 mitte) ... (nth (- length 1) mitte)
                   ; durch ein einziges Element new-code.
                   (assert (typep length '(INTEGER 0 4)))
                   `(progn
                      ,(case length
                         (0 `(setf (cdr mitte) (setq rechts (cons (car mitte) rechts))
                                   (car mitte) ,new-code
                         )   )
                         (1 `(setf (car mitte) ,new-code))
                         (t `(setf (car mitte) ,new-code
                                   (cdr mitte) ,(setq rechts
                                                  (case length
                                                    (2 `(cdr rechts))
                                                    (3 `(cddr rechts))
                                                    (4 `(cdddr rechts))
                                                ) )
                       ) )   )
                      (go weiter)
                    )
                ))
        (let ((item (car mitte)))
          (when (consp item)
            ; Untersuchung des Befehls item und der nachfolgenden:
            (when (and #| (consp rechts) |# (consp (car rechts)))
              ; normale Umwandlungen, mit Aneinanderhängen der Argumente:
              (let ((new-op
                      (cdr (assoc (first item)
                                  (case (first (car rechts))
                                    (PUSH  '((T        . T&PUSH)
                                             (CONST    . CONST&PUSH)
                                             (LOADI    . LOADI&PUSH)
                                             (LOADC    . LOADC&PUSH)
                                             (LOADV    . LOADV&PUSH)
                                             (GETVALUE . GETVALUE&PUSH)
                                             (CALL     . CALL&PUSH)
                                             (CALL1    . CALL1&PUSH)
                                             (CALL2    . CALL2&PUSH)
                                             (CALLS1   . CALLS1&PUSH)
                                             (CALLS2   . CALLS2&PUSH)
                                             (CALLSR   . CALLSR&PUSH)
                                             (CALLC    . CALLC&PUSH)
                                             (CALLCKEY . CALLCKEY&PUSH)
                                             (CAR      . CAR&PUSH)
                                             (CDR      . CDR&PUSH)
                                             (CONS     . CONS&PUSH)
                                             (LIST     . LIST&PUSH)
                                             (FUNCALL  . FUNCALL&PUSH)
                                             (APPLY    . APPLY&PUSH)
                                             (COPY-CLOSURE . COPY-CLOSURE&PUSH)
                                    )       )
                                    (JMPIF
                                      (let ((alist
                                              '((EQ     . JMPIFEQ)
                                                (LOAD   . LOAD&JMPIF)
                                                (CALL1  . CALL1&JMPIF)
                                                (CALL2  . CALL2&JMPIF)
                                                (CALLS1 . CALLS1&JMPIF)
                                                (CALLS2 . CALLS2&JMPIF)
                                                (CALLSR . CALLSR&JMPIF)
                                               )
                                           ))
                                        (when (third (car rechts))
                                          (setq alist (cdr alist))
                                        )
                                        (setf (cddr (car rechts)) '())
                                        alist
                                    ) )
                                    (JMPIFNOT
                                      (let ((alist
                                              '((EQ     . JMPIFNOTEQ)
                                                (LOAD   . LOAD&JMPIFNOT)
                                                (CALL1  . CALL1&JMPIFNOT)
                                                (CALL2  . CALL2&JMPIFNOT)
                                                (CALLS1 . CALLS1&JMPIFNOT)
                                                (CALLS2 . CALLS2&JMPIFNOT)
                                                (CALLSR . CALLSR&JMPIFNOT)
                                               )
                                           ))
                                        (when (third (car rechts))
                                          (setq alist (cdr alist))
                                        )
                                        (setf (cddr (car rechts)) '())
                                        alist
                                    ) )
                                    (STORE '((NIL    . NIL&STORE)
                                             (T      . T&STORE)
                                             (POP    . POP&STORE)
                                             (CALLS1 . CALLS1&STORE)
                                             (CALLS2 . CALLS2&STORE)
                                             (CALLSR . CALLSR&STORE)
                                    )       )
                                    (STOREC '((LOAD . LOAD&STOREC)))
                                    (RET '((SKIP . SKIP&RET)))
                                  )
                                  :test #'eq
                   )) )    )
                (when new-op
                  (ersetze 2 `(,new-op ,@(rest item) ,@(rest (car rechts))))
            ) ) )
            ; weitere Umwandlungen:
            (case (first item)
              ((NIL PUSH-NIL)
                (flet ((nilpusher-p (coder)
                         ; Kommt (NIL) (PUSH) --> 1,
                         ; kommt (PUSH-NIL n) --> n,
                         ; sonst nil.
                         (and #| (consp coder) |# (consp (car coder))
                              (case (first (car coder))
                                (PUSH-NIL (second (car coder)))
                                ((NIL) (when (equal (cadr coder) '(PUSH))
                                         (setf (cdr coder) (cddr coder))
                                         1
                                )      )
                                (t nil)
                      )) )    )
                  (let ((count (nilpusher-p mitte)))
                    (when count
                      (setq rechts (cdr mitte))
                      (loop
                        (let ((next-count (nilpusher-p rechts)))
                          (unless next-count (return))
                          (incf count next-count)
                        )
                        (setq rechts (cdr rechts))
                      )
                      (setf (car mitte) (if (eql count 1) '(NIL&PUSH) `(PUSH-NIL ,count))
                            (cdr mitte) rechts
                      )
                      (go weiter)
              ) ) ) )
              (CONST
                (when (and #| (consp rechts) |# (consp (car rechts)))
                  (case (first (car rechts))
                    (SYMBOL-FUNCTION
                      (let ((n (second item)))
                        (cond ((and #| (consp (cdr rechts)) |#
                                    (equal (cadr rechts) '(PUSH))
                               )
                               (ersetze 3 `(CONST&SYMBOL-FUNCTION&PUSH ,n))
                              )
                              ((and #| (consp (cdr rechts)) |#
                                    (consp (cadr rechts))
                                    (eq (first (cadr rechts)) 'STORE)
                               )
                               (ersetze 3
                                 `(CONST&SYMBOL-FUNCTION&STORE ,n ,(second (cadr rechts)))
                              ))
                              (t (ersetze 2 `(CONST&SYMBOL-FUNCTION ,n)))
                    ) ) )
                    (EQ
                      (when (and #| (consp (cdr rechts)) |#
                                 (consp (cadr rechts))
                                 (memq (first (cadr rechts)) '(JMPIF JMPIFNOT))
                                 (null (third (cadr rechts)))
                            )
                        (ersetze 3
                          `(,(if (eq (first (cadr rechts)) 'JMPIF)
                               'JMPIFEQTO
                               'JMPIFNOTEQTO
                             )
                            ,(second item)
                            ,(second (cadr rechts))
                           )
              ) ) ) ) ) )
              (LOAD
                (when (and #| (consp rechts) |# (consp (car rechts)))
                  (let ((n (second item)))
                    (case (first (car rechts))
                      (CAR
                        (when (and #| (consp (cdr rechts)) |# (consp (cadr rechts)))
                          (case (first (cadr rechts))
                            (PUSH (ersetze 3 `(LOAD&CAR&PUSH ,n)))
                            (STORE
                              (ersetze 3
                                `(LOAD&CAR&STORE ,n ,(second (cadr rechts)))
                      ) ) ) ) )
                      (CDR
                        (when (and #| (consp (cdr rechts)) |# (consp (cadr rechts)))
                          (case (first (cadr rechts))
                            (PUSH (ersetze 3 `(LOAD&CDR&PUSH ,n)))
                            (STORE
                              (when (eql n (second (cadr rechts)))
                                (ersetze 3 `(LOAD&CDR&STORE ,n))
                      ) ) ) ) )
                      (CONS
                        (when (and #| (consp (cdr rechts)) |# (consp (cadr rechts))
                                   (eq (first (cadr rechts)) 'STORE)
                                   (eql (second (cadr rechts)) (- n 1))
                              )
                          (ersetze 3 `(LOAD&CONS&STORE ,(- n 1)))
                      ) )
                      (PUSH
                        (when (and #| (consp (cdr rechts)) |# (consp (cadr rechts))
                                   (or (equal (cadr rechts) CALLS-1+)
                                       (equal (cadr rechts) CALLS-1-)
                                   )
                                   #| (consp (cddr rechts)) |# (consp (caddr rechts))
                              )
                          (when (equal (caddr rechts) '(PUSH))
                            (ersetze 4
                              `(,(if (equal (cadr rechts) CALLS-1+)
                                   'LOAD&INC&PUSH
                                   'LOAD&DEC&PUSH
                                 )
                                ,n
                               )
                          ) )
                          (when (and (eq (first (caddr rechts)) 'STORE)
                                     (eql (second (caddr rechts)) n)
                                )
                            (ersetze 4
                              `(,(if (equal (cadr rechts) CALLS-1+)
                                   'LOAD&INC&STORE
                                   'LOAD&DEC&STORE
                                 )
                                ,n
                               )
                        ) ) )
                        (ersetze 2 `(LOAD&PUSH ,n))
              ) ) ) ) )
              (JMPIFBOUNDP ; vereinfache (JMPIFBOUNDP n l) (NIL) (STORE n) l
                (when (and #| (consp rechts) |#
                           (equal (car rechts) '(NIL))
                           #| (consp (cdr rechts)) |#
                           (consp (cadr rechts))
                           (eq (first (cadr rechts)) 'STORE)
                           (eql (second (cadr rechts)) (second item))
                           #| (consp (cddr rechts)) |#
                           (eq (caddr rechts) (third item))
                      )
                  (ersetze 3 `(UNBOUND->NIL ,(second item)))
              ) )
              (JSR
                (if (and #| (consp rechts) |# (equal (car rechts) '(PUSH)))
                  (ersetze 2 `(JSR&PUSH ,(third item)))
                  (ersetze 1 `(JSR ,(third item)))
              ) )
              (UNBIND1
                (let ((count 1))
                  (loop
                    (unless (and #| (consp rechts) |#
                                 (equal (car rechts) '(UNBIND1))
                            )
                      (return)
                    )
                    (incf count)
                    (setq rechts (cdr rechts))
                  )
                  (unless (eql count 1)
                    (setf (car mitte) `(UNBIND ,count))
                    (setf (cdr mitte) rechts)
                    (go weiter)
              ) ) )
              ;(RET (ersetze 1 '(SKIP&RET 0))) ; kommt nicht vor!
              (UNWIND-PROTECT-CLOSE (ersetze 1 '(UNWIND-PROTECT-CLOSE)))
              ((JMPIF JMPIFNOT) (ersetze 1 `(,(first item) ,(second item))))
              (JMPHASH
                (let ((hashtable (third item))
                      (labels (cddddr item)))
                  (maphash
                    #'(lambda (obj index) ; (gethash obj hashtable) = index
                        (setf (gethash obj hashtable) (nth index labels))
                      )
                    hashtable
                ) )
                (setf (cddddr (car mitte)) '())
              )
      ) ) ) )
      weiter ; Hier ist man mit (car mitte) fertig.
    )
    code-list
  )
)


#|
                                6. Schritt:
                Umwandlung der Instruktionen in eine Byte-Folge

Erster Teilschritt: jeder Instruktion wird eine Klassifikation der Instruktion
und die Länge der Instruktion (Label-Operanden nicht mitgezählt)
vorangestellt, jedem Label wird sein PC als Wert zugewiesen.
Dabei werden die Operandenlängen - soweit möglich - bestimmt, in Instruktionen
auftretende Labels werden durch (vermutliche Verweislänge . label) ersetzt.
So wird aus (BLOCK-OPEN 2 #:G7) --> (NL 2 . (67 2 (1 . #:G7))) .
Weitere Teilschritte:
Immer wieder wird die Codeliste durchlaufen, dabei werden Sprungverweise
eventuell von 1 auf 2 oder 6 Byte verlängert. Dadurch kann der Code insgesamt
nur länger werden.
Letzter Teilschritt:
Die Sprungverweise werden in Distanzen umgesetzt, und die Codeliste wird
als Liste von Bytes neu aufgebaut.
|#
; gibt an, wieviel Bytes ein numerischer Operand braucht:
(defun num-operand-length (n)
  (cond ((< n 128) 1) ; 7 Bit in 1 Byte
        ((< n 32768) 2) ; 15 Bit in 2 Bytes
        (t 6) ; sonst 6 Bytes
) )
; assembliert eine Code-Liste und liefert eine Bytecode-Liste:
(defun assemble-LAP (code-list)
  ; erster Teilschritt:
  (do ((code-listr code-list (cdr code-listr))
       (PC 0))
      ((null code-listr))
    (let ((item (car code-listr)))
      (if (atom item)
        (setf (symbol-value item) PC)
        (let ((instr-code (gethash (first item) instruction-codes)))
          (unless instr-code (compiler-error 'assemble-LAP "ILLEGAL INSTRUCTION"))
          (let ((instr-class (second (svref instruction-table instr-code)))
                (instr-length 1))
            (if (and (eq instr-class 'K) (< (second item) short-code-opsize))
              (progn
                (setq instr-code
                  (+ short-code-offset
                     (* short-code-opsize
                        (position (first item) instruction-table-K)
                     )
                     (second item)
                ) )
                (setq instr-class 'O)
                (setq item (list (first item)))
              )
              (case instr-class
                (O)
                ((K N) (incf instr-length (num-operand-length (second item))))
                (B (incf instr-length 1))
                (L (incf PC 1) (push 1 (second item)))
                (NN (incf instr-length (num-operand-length (second item)))
                    (incf instr-length (num-operand-length (third item))) )
                (NB (incf instr-length (num-operand-length (second item)))
                    (incf instr-length 1) )
                (BN (incf instr-length 1)
                    (incf instr-length (num-operand-length (third item))) )
                (NNN (incf instr-length (num-operand-length (second item)))
                     (incf instr-length (num-operand-length (third item)))
                     (incf instr-length (num-operand-length (fourth item))) )
                (NBN (incf instr-length (num-operand-length (second item)))
                     (incf instr-length 1)
                     (incf instr-length (num-operand-length (fourth item))) )
                (NL (incf instr-length (num-operand-length (second item)))
                    (incf PC 1) (push 1 (third item)) )
                (BL (incf instr-length 1)
                    (incf PC 1) (push 1 (third item)) )
                (NNL (incf instr-length (num-operand-length (second item)))
                     (incf instr-length (num-operand-length (third item)))
                     (incf PC 1) (push 1 (fourth item)) )
                (NBL (incf instr-length (num-operand-length (second item)))
                     (incf instr-length 1)
                     (incf PC 1) (push 1 (fourth item)) )
                (NHL (incf instr-length (num-operand-length (second item)))
                     (incf PC 1) (push 1 (fourth item)) )
                (NLX (incf instr-length (num-operand-length (second item)))
                     (do ((L (cddr item) (cdr L)))
                         ((null L))
                       (incf PC 1) (push 1 (car L))
                )    )
            ) )
            (incf PC instr-length)
            (setf (car code-listr)
              (list* instr-class instr-length instr-code (cdr item))
            )
  ) ) ) ) )
  ; weitere Teilschritte:
  (loop
    (unless
      (let ((modified nil) (PC 0))
        (dolist (item code-list)
          (if (atom item)
            (setf (symbol-value item) PC)
            (progn
              (incf PC (cadr item))
              (when (memq (car item) '(L NL BL NNL NBL NHL NLX))
                (let ((itemargs (cdddr item)))
                  (dolist (x (case (car item)
                               (L itemargs)
                               ((NL BL NLX) (cdr itemargs))
                               ((NNL NBL NHL) (cddr itemargs))
                          )  )
                    (incf PC (car x))
                    (let ((new-dist (- (symbol-value (cdr x)) PC)))
                      ; bisher angenommene Sprunglänge und neu errechnete abgleichen:
                      (if (<= -64 new-dist 63) ; 7 Bits in 1 Byte
                        () ; Sprunglänge bleibt 1
                        (if (<= -16384 new-dist 16383) ; 15 Bits in 2 Bytes
                          (case (car x)
                            (1 (setf (car x) 2) ; neue Sprunglänge=2
                               (incf PC 1) ; gibt 2-1=1 Bytes Verlängerung
                               (setq modified t)
                          ) )
                          ; 32 Bits in 6 Bytes
                          (case (car x)
                            (1 (setf (car x) 6) ; neue Sprunglänge=6
                               (incf PC 5) ; gibt 6-1=5 Bytes Verlängerung
                               (setq modified t)
                            )
                            (2 (setf (car x) 6) ; neue Sprunglänge=6
                               (incf PC 4) ; gibt 6-2=4 Bytes Verlängerung
                               (setq modified t)
                      ) ) ) )
              ) ) ) )
        ) ) )
        modified
      )
      (return) ; nichts mehr verändert -> alle Sprunglängen optimal
  ) )
  ; letzter Teilschritt:
  (let ((byte-list '()) (PC 0))
    (flet ((new-byte (n) (push n byte-list)))
      (flet ((num-operand (n)
               (cond ((< n 128) (new-byte n))
                     ((< n 32768) (new-byte (+ 128 (ldb (byte 7 8) n)))
                                  (new-byte (ldb (byte 8 0) n))
                     )
                     (t (compiler-error 'assemble-LAP "15 BIT"))
             ) )
             (label-operand (x)
               (incf PC (car x))
               (let ((dist (- (symbol-value (cdr x)) PC)))
                 (case (car x)
                   (1 (new-byte (ldb (byte 7 0) dist)))
                   (2 (new-byte (+ 128 (ldb (byte 7 8) dist)))
                      (new-byte (ldb (byte 8 0) dist))
                   )
                   (6 (new-byte 128) (new-byte 0)
                      (new-byte (ldb (byte 8 24) dist))
                      (new-byte (ldb (byte 8 16) dist))
                      (new-byte (ldb (byte 8 8) dist))
                      (new-byte (ldb (byte 8 0) dist))
                 ) )
            )) )
        (dolist (item code-list)
          (when (consp item)
            (incf PC (cadr item))
            (new-byte (caddr item))
            (case (car item)
              (O) ; darin fallen auch die 1-Byte-Befehle vom Typ K
              ((K N) (num-operand (second (cddr item))))
              (B (new-byte (second (cddr item))))
              (L (label-operand (second (cddr item))))
              (NN (num-operand (second (cddr item)))
                  (num-operand (third (cddr item))) )
              (NB (num-operand (second (cddr item)))
                  (new-byte (third (cddr item))) )
              (BN (new-byte (second (cddr item)))
                  (num-operand (third (cddr item))) )
              (NNN (num-operand (second (cddr item)))
                   (num-operand (third (cddr item)))
                   (num-operand (fourth (cddr item))) )
              (NBN (num-operand (second (cddr item)))
                   (new-byte (third (cddr item)))
                   (num-operand (fourth (cddr item))) )
              (NL (num-operand (second (cddr item)))
                  (label-operand (third (cddr item))) )
              (BL (new-byte (second (cddr item)))
                  (label-operand (third (cddr item))) )
              (NNL (num-operand (second (cddr item)))
                   (num-operand (third (cddr item)))
                   (label-operand (fourth (cddr item))) )
              (NBL (num-operand (second (cddr item)))
                   (new-byte (third (cddr item)))
                   (label-operand (fourth (cddr item))) )
              (NHL (num-operand (second (cddr item)))
                   (let ((ht (third (cddr item))))
                     (maphash
                       #'(lambda (obj x) ; x = (gethash obj ht)
                           (setf (gethash obj ht) (- (symbol-value x) PC))
                         )
                       ht
                   ) )
                   (label-operand (fourth (cddr item)))
              )
              (NLX (num-operand (second (cddr item)))
                   (dolist (x (cddr (cddr item))) (label-operand x)) )
            )
        ) )
    ) )
    (nreverse byte-list)
) )

; die Umkehrung zu assemble-LAP : liefert zu einer Bytecode-Liste die dazu
; gehörige Codeliste. In dieser steht allerdings vor jedem Item noch der PC.
(defun disassemble-LAP (byte-list const-list)
  (let ((code-list '()) (PC 0) instr-PC (label-alist '()))
    ; label-alist ist eine Liste von Conses (PC . label), in der die PCs streng
    ; fallend geordnet sind.
    (flet ((PC->label-a (PC)
             (cons PC (make-symbol
                        (concatenate 'string "L" (prin1-to-string PC))
           ) )        )
           (next-byte () (incf PC) (pop byte-list))
          )
      (flet ((num-operand ()
               (let ((a (next-byte)))
                 (cond ((< a 128) a)
                       (t (+ (* 256 (- a 128)) (next-byte)))
             ) ) )
             (label-operand
                  (&optional
                    (dist
                      (let ((a (next-byte)))
                        (cond ((< a 128) (if (< a 64) a (- a 128)))
                              (t (setq a (- a 128))
                                 (unless (< a 64) (setq a (- a 128)))
                                 (setq a (+ (* 256 a) (next-byte)))
                                 (if (zerop a)
                                   (+ (* 256 (+ (* 256 (+ (* 256 (next-byte))
                                                          (next-byte)
                                                )      )
                                                (next-byte)
                                      )      )
                                      (next-byte)
                                   )
                                   a
                    ) ) )     )  )
                   &aux
                    (label-PC (+ PC dist))
                  )
               ; Suche label-PC in label-alist:
               (do* ((L1 nil L2)
                     (L2 label-alist (cdr L2))) ; L1 = nil oder L2 = (cdr L1)
                    ((cond
                       ((or (null L2) (> label-PC (caar L2))) ; einfügen
                        (setq L2 (cons (PC->label-a label-PC) L2))
                        (if L1 (setf (cdr L1) L2) (setq label-alist L2))
                        t)
                       ((= label-PC (caar L2)) t)
                       (t nil)
                     )
                     (cdar L2)
            )) )    )
        (loop
          (when (null byte-list) (return))
          (setq instr-PC PC) ; PC beim Start der Instruktion
          (let ((instruction
                  (let ((instr-code (next-byte)))
                    (if (>= instr-code short-code-offset)
                      (multiple-value-bind (q r) (floor (- instr-code short-code-offset) short-code-opsize)
                        (list (svref instruction-table-K q) r)
                      )
                      (let* ((table-entry (svref instruction-table instr-code))
                             (instr-name (first table-entry)))
                        (case (second table-entry)
                          (O (list instr-name))
                          ((K N) (list instr-name (num-operand)))
                          (B (list instr-name (next-byte)))
                          (L (list instr-name (label-operand)))
                          (NN (list instr-name (num-operand) (num-operand)))
                          (NB (list instr-name (num-operand) (next-byte)))
                          (BN (list instr-name (next-byte) (num-operand)))
                          (NNN (list instr-name (num-operand) (num-operand) (num-operand)))
                          (NBN (list instr-name (num-operand) (next-byte) (num-operand)))
                          (NL (list instr-name (num-operand) (label-operand)))
                          (BL (list instr-name (next-byte) (label-operand)))
                          (NNL (list instr-name (num-operand) (num-operand) (label-operand)))
                          (NBL (list instr-name (num-operand) (next-byte) (label-operand)))
                          (NHL (let* ((n (num-operand))
                                      (ht (nth n const-list))
                                      (labels '()))
                                 (maphash
                                   #'(lambda (obj dist)
                                       (declare (ignore obj))
                                       (push (label-operand dist) labels)
                                     )
                                   ht
                                 )
                                 (list* instr-name n (label-operand) labels)
                          )    )
                          (NLX (let ((n (num-operand))
                                     (L '()))
                                 (dotimes (i n) (push (label-operand) L))
                                 (list* instr-name n (nreverse L))
                          )    )
               )) ) ) ) )
            (push (cons instr-PC instruction) code-list)
        ) )
    ) )
    ; (setq label-alist (sort label-alist #'> :key #'car))
    ; code-list umdrehen und dabei die Labels einfügen:
    (let ((new-code-list '()))
      (loop
        (when (and new-code-list label-alist
                   (= (caar new-code-list) (caar label-alist))
              )
          (push (car label-alist) new-code-list)
          (setq label-alist (cdr label-alist))
        )
        (when (null code-list) (return))
        ; eine Instruktion von code-list in new-code-list übernehmen:
        (psetq code-list (cdr code-list)
               new-code-list (rplacd code-list new-code-list)
      ) )
      new-code-list
) ) )


#|
                           7. Schritt:
                    funktionales Objekt bilden

Die Funktion make-closure wird dazu vorausgesetzt.
|#
; trägt eine Byteliste als Code in fnode ein.
(defun create-fun-obj (fnode byte-list #+CLISP3 SPdepth)
  (setf (fnode-code fnode)
    (make-closure
      :name (fnode-name fnode)
      :codevec
        (macrolet ((as-word (anz)
                     (if *big-endian*
                       ; BIG-ENDIAN-Prozessor
                       `(floor ,anz 256)
                       ; LITTLE-ENDIAN-Prozessor
                       `(multiple-value-bind (q r) (floor ,anz 256) (values r q))
                  )) )
          (multiple-value-call #'list*
            #+CLISP3 (as-word SPdepth)
            (as-word (fnode-req-anz fnode))
            (as-word (fnode-opt-anz fnode))
            (+ (if (fnode-rest-flag fnode) 1 0)
               (if (fnode-keyword-flag fnode)
                 (+ 128 (if (fnode-allow-other-keys-flag fnode) 64 0))
                 0
            )  )
            (values ; Argumenttyp-Kürzel
              (let ((req-anz (fnode-req-anz fnode))
                    (opt-anz (fnode-opt-anz fnode))
                    (rest (fnode-rest-flag fnode))
                    (key (fnode-keyword-flag fnode)))
                (cond ((and (not rest) (not key) (< (+ req-anz opt-anz) 6))
                       (+ (svref '#(1 7 12 16 19 21) opt-anz) req-anz)
                      )
                      ((and rest (not key) (zerop opt-anz) (< req-anz 5))
                       (+ 22 req-anz)
                      )
                      ((and (not rest) key (< (+ req-anz opt-anz) 5))
                       (+ (svref '#(27 32 36 39 41) opt-anz) req-anz)
                      )
                      (t 0)
            ) ) )
            (if (fnode-keyword-flag fnode)
              (multiple-value-call #'values
                (as-word (length (fnode-keywords fnode)))
                (as-word (fnode-Keyword-Offset fnode))
              )
              (values)
            )
            byte-list
        ) )
      :consts (append (make-list (fnode-Keyword-Offset fnode))
                      (fnode-keywords fnode)
                      (if *compiling-from-file*
                        (mapcar #'(lambda (value form)
                                    (if form (make-load-time-eval form) value)
                                  )
                                (fnode-Consts fnode) (fnode-Consts-forms fnode)
                        )
                        (fnode-Consts fnode)
  ) )         )       )
  fnode
)

; Liefert die Signatur eines funktionalen Objekts,
; als Werte:
; 1. req-anz
; 2. opt-anz
; 3. rest-p
; 4. key-p
; 5. keyword-list
; 6. allow-other-keys-p
; und zusätzlich
; 7. byte-list
; 8. const-list
(defun signature (closure)
  (let ((const-list (closure-consts closure))
        (byte-list (closure-codevec closure)))
    (macrolet ((pop2 (listvar)
                 (if *big-endian*
                   ; BIG-ENDIAN-Prozessor
                   `(+ (* 256 (pop ,listvar)) (pop ,listvar))
                   ; LITTLE-ENDIAN-Prozessor
                   `(+ (pop ,listvar) (* 256 (pop ,listvar)))
              )) )
      #+CLISP3 (progn (pop byte-list) (pop byte-list))
      (let* ((req-anz (pop2 byte-list))
             (opt-anz (pop2 byte-list))
             (h (pop byte-list))
             (key-p (logbitp 7 h)))
        (pop byte-list)
        (values
          req-anz
          opt-anz
          (logbitp 0 h)
          key-p
          (when key-p
            (let ((kw-count (pop2 byte-list))
                  (kw-offset (pop2 byte-list)))
              (subseq (closure-consts closure) kw-offset (+ kw-offset kw-count))
          ) )
          (logbitp 6 h)
          byte-list
          const-list
) ) ) ) )


;                  D R I T T E R   P A S S

(defun pass3 ()
  (dolist (pair *fnode-fixup-table*)
    (let ((code (fnode-code (first pair))) (n (second pair)))
      (macrolet ((closure-const (code n)
                   #-CLISP `(nth ,n (closure-consts ,code))
                   #+CLISP `(sys::%record-ref ,code (+ 2 ,n))
                ))
        (setf (closure-const code n) (fnode-code (closure-const code n)))
) ) ) )


;             T O P - L E V E L - A U F R U F

; compiliert einen Lambdabody und liefert seinen Code.
(defun compile-lambdabody (name lambdabody)
  (let ((fnode (c-lambdabody name lambdabody)))
    (unless *no-code*
      (let ((*fnode-fixup-table* '()))
        (pass2 fnode)
        (pass3)
      )
      (fnode-code fnode)
) ) )

; wird bei (lambda (...) (declare (compile)) ...) aufgerufen und liefert ein
; zu diesem Lambda-Ausdruck äquivalentes funktionales Objekt.
(defun compile-lambda (name lambdabody %venv% %fenv% %benv% %genv% %denv%
                       &optional (recursive-flag nil)
                      )
  (let ((*compiling* t)
        (*compiling-from-file* nil)
        (*c-listing-output* nil)
        (*c-error-output* *error-output*)
        (*known-special-vars* '())
        (*constant-special-vars* '())
        (*func* nil)
        (*fenv* %fenv%)
        (*benv* %benv%)
        (*genv* %genv%)
        (*venv* %venv%)
        (*venvc* nil)
        (*denv* %denv%)
        (*error-count* 0) (*warning-count* 0)
        (*no-code* nil)
       )
    (when recursive-flag
      (setq lambdabody ; vgl. DEFUN-Macroexpander:
        ; statt `(() (FUNCTION (LAMBDA ,@lambdabody)))
        `(() (LABELS ((,name ,@lambdabody)) (FUNCTION ,name)))
    ) )
    (let ((funobj (compile-lambdabody name lambdabody)))
      (unless (zerop *error-count*)
        (return-from compile-lambda (compile-lambdabody name '(() NIL)))
      )
      (when recursive-flag
        (setq funobj (funcall funobj)) ; LABELS-definierte Funktion holen
        (setf (closure-name funobj) name) ; und mit einfacherem Namen versehen
      )
      funobj
) ) )

; wird bei (let/let*/multiple-value-bind ... (declare (compile)) ...) aufgerufen
; und liefert ein funktionales Objekt, das - mit 0 Argumenten aufgerufen - diese
; Form ausführt.
(let ((form-count 0))
  (defun compile-form (form %venv% %fenv% %benv% %genv% %denv%)
    (compile-lambda (symbol-suffix '#:COMPILED-FORM (incf form-count))
                    `(() ,form)
                    %venv% %fenv% %benv% %genv% %denv%
  ) )
)

; Common-Lisp-Funktion COMPILE
#-CROSS
(defun compile (name &optional (definition nil svar)
                     &aux (macro-flag nil) (trace-flag nil))
  (unless (symbolp name)
    (error #+DEUTSCH "Name einer zu compilierenden Funktion muß ein Symbol sein, nicht: ~S"
           #+ENGLISH "Name of function to be compiled must be a symbol, not ~S"
           name
  ) )
  (if svar
    ; Neudefinition von name als Funktion.
    (progn
      ; Ist name getraced -> falls vorher Macro, erst untracen.
      (when (and name (setq svar (get name 'sys::traced-definition)))
        (if (consp svar)
          (progn
            (warn #+DEUTSCH "~S: ~S war getraced und wird umdefiniert!"
                  #+ENGLISH "~S: redefining ~S; it was traced!"
                  'compile name
            )
            (sys::untrace2 name)
          )
          (setq trace-flag t)
      ) )
      (when (compiled-function-p definition)
        (warn #+DEUTSCH "~S ist schon compiliert."
              #+ENGLISH "~S is already compiled."
              definition
        )
        (when name
          (if trace-flag
            (setf (get name 'sys::traced-definition) definition)
            (setf (symbol-function name) definition)
        ) )
        (return-from compile name)
    ) )
    ; Compilierung der vorhandenen Funktions-/Macro-Definition.
    (progn
      (unless (fboundp name)
        (error #+DEUTSCH "Funktion ~S ist undefiniert."
               #+ENGLISH "Undefined function ~S"
               name
      ) )
      (if (setq definition (get name 'sys::traced-definition))
        (setq trace-flag t)
        (setq definition (symbol-function name))
      )
      (when (and (consp definition) (eq (car definition) 'system::macro))
        (setq macro-flag t)
        (setq definition (cdr definition))
      )
      (when (compiled-function-p definition)
        (warn #+DEUTSCH "~S ist schon compiliert."
              #+ENGLISH "~S is already compiled."
              name
        )
        (return-from compile name)
  ) ) )
  (unless (or (and (consp definition) (eq (car definition) 'lambda))
              (sys::closurep definition)
          )
    (error #+DEUTSCH "Das ist weder ein Lambda-Ausdruck noch ein funktionales Objekt:~%~S"
           #+ENGLISH "Not a lambda expression nor a function: ~S"
           definition
  ) )
  (let ((*compiling* t)
        (*error-count* 0)
        (*warning-count* 0)
        (*compiling-from-file* nil)
        (*c-listing-output* nil)
        (*c-error-output* *error-output*)
        (*known-special-vars* '())
        (*constant-special-vars* '())
        (*func* nil)
        (*fenv* (if (sys::closurep definition)
                  (sys::%record-ref definition 5)
                  nil
        )       )
        (*benv* (if (sys::closurep definition)
                  (sys::%record-ref definition 6)
                  nil
        )       )
        (*genv* (if (sys::closurep definition)
                  (sys::%record-ref definition 7)
                  nil
        )       )
        (*venv* (if (sys::closurep definition)
                  (sys::%record-ref definition 4)
                  nil
        )       )
        (*venvc* nil)
        (*denv* (if (sys::closurep definition)
                  (sys::%record-ref definition 8)
                  *toplevel-denv*
        )       )
        (*no-code* nil))
    (let ((lambdabody (if (sys::closurep definition)
                        (sys::%record-ref definition 1)
                        (cdr definition)
          )           )
          (recursive-flag ; Flag, ob Tail-Rekursion entrekursiviert wird
            (and name
                 (sys::closurep definition) ; nur bei fertigen Iclosures
                 (eq (sys::%record-ref definition 0) name) ; mit selbem Namen
                 (not macro-flag) ; nicht aber bei Macros
         )) )
      (when recursive-flag
        (setq lambdabody ; vgl. DEFUN-Macroexpander:
          ; statt `(() (FUNCTION (LAMBDA ,@lambdabody)))
          `(() (LABELS ((,name ,@lambdabody)) (FUNCTION ,name)))
      ) )
      (let ((funobj (compile-lambdabody name lambdabody)))
        (unless (zerop *error-count*) (return-from compile nil))
        (when recursive-flag
          (setq funobj (funcall funobj)) ; LABELS-definierte Funktion holen
          (setf (closure-name funobj) name) ; und mit einfacherem Namen versehen
        )
        (if name
          (progn
            (when macro-flag (setq funobj (cons 'system::macro funobj)))
            (if trace-flag
              (setf (get name 'sys::traced-definition) funobj)
              (setf (symbol-function name) funobj)
            )
            name
          )
          funobj
) ) ) ) )

; Common-Lisp-Funktion COMPILE-FILE
; file          sollte ein Pathname/String/Symbol sein.
; :output-file  sollte nil oder t oder ein Pathname/String/Symbol oder
;               ein Output-Stream sein. Default: t.
; :listing      sollte nil oder t oder ein Pathname/String/Symbol oder
;               ein Output-Stream sein. Default: nil.
; :warnings     gibt an, ob die Warnings auch auf dem Bildschirm erscheinen
;               sollen.
; :verbose      gibt an, ob die Errors auch auf dem Bildschirm erscheinen
;               sollen.
(defun compile-file (file &key (output-file 'T) listing
                               ((:warnings *compile-warnings*) *compile-warnings*)
                               ((:verbose *compile-verbose*) *compile-verbose*)
                          &aux (top-call nil) liboutput-file
                               (new-output-stream nil) (new-listing-stream nil)
                    )
  (setq file (or (first (search-file file '(#".lsp")))
                 (merge-pathnames file (merge-pathnames '#".lsp"))
  )          )
  (when (and output-file (not (streamp output-file)))
    (setq output-file (if (eq output-file 'T)
                        (merge-pathnames '#".fas" file)
                        (merge-pathnames output-file)
    )                 )
    (setq liboutput-file (merge-pathnames '#".lib" output-file))
    (setq new-output-stream t)
  )
  (when (and listing (not (streamp listing)))
    (setq listing (if (eq listing 'T)
                    (merge-pathnames '#".lis" file)
                    (merge-pathnames listing)
    )             )
    (setq new-listing-stream t)
  )
  (with-open-file (istream file :direction :input)
    (let ((listing-stream (if new-listing-stream
                            (open listing :direction :output)
                            (if (streamp listing) listing nil)
         ))               ) ; ein Stream oder NIL
      (unwind-protect
        (let ((output-stream (if new-output-stream
                               (open output-file :direction :output)
                               (if (streamp output-file) output-file nil)
              )              ) ; ein Stream oder NIL
              (*liboutput-stream* (if new-output-stream
                                    (open liboutput-file :direction :output)
                                    nil
              )                   ) ; ein Stream oder NIL
              (compilation-successful nil))
          (unwind-protect
            (progn
              (when listing-stream
                (format listing-stream
                  #+DEUTSCH "~&Listing der Compilation von File ~A~%am ~@? durch ~A in der Version ~A"
                  #+ENGLISH "~&Listing of compilation of file ~A~%on ~@? by ~A, version ~A"
                  file
                  *date-format*
                  (multiple-value-list (get-decoded-time))
                    ; Liste (sec min hour day month year ...)
                  (lisp-implementation-type) (lisp-implementation-version)
              ) )
              (unless *compiling* ; Variablen setzen, nicht binden!
                (setq *functions-with-errors* '())
                (setq *known-special-vars* '()) (setq *unknown-free-vars* '())
                (setq *constant-special-vars* '())
                (setq *known-functions* '()) (setq *unknown-functions* '())
                (setq *inline-functions* '()) (setq *notinline-functions* '())
                (setq *inline-definitions* '())
                (setq *user-declaration-types* '())
                (setq *compiled-modules* '())
                (setq top-call t)
              )
              (let ((*compiling* t)
                    (*compiling-from-file* t)
                    (*package* *package*)
                    (*c-listing-output* listing-stream)
                    (*c-error-output*
                      (if listing-stream
                        (make-broadcast-stream *error-output* listing-stream)
                        *error-output*
                    ) )
                    (*func* nil)
                    (*fenv* nil)
                    (*benv* nil)
                    (*genv* nil)
                    (*venv* nil)
                    (*venvc* nil)
                    (*denv* *toplevel-denv*)
                    (*error-count* 0) (*warning-count* 0)
                    (*no-code* (and (null output-stream) (null listing-stream)))
                    (eof-value "EOF")
                    (form-count 0)
                   )
                (c-comment #+DEUTSCH "~%File ~A wird compiliert..."
                           #+ENGLISH "~%Compiling file ~A ..."
                           file
                )
                (when output-stream
                  (let ((*package* *keyword-package*))
                    (write `(SYSTEM::VERSION ',(version)) :stream output-stream
                           :escape t :level nil :length nil :radix t
                ) ) )
                (loop
                  (let ((form (read istream nil eof-value)))
                    (when (eql form eof-value) (return))
                    (let ((*package-tasks* '()))
                      (setq form
                        (compile-lambdabody
                          (symbol-suffix '#:TOP-LEVEL-FORM (incf form-count))
                          `(() ,form)
                      ) )
                      (when listing-stream
                        (disassemble-closures form listing-stream)
                      )
                      (when output-stream
                        (terpri output-stream)
                        (write form :stream output-stream :pretty t
                                    :closure t :circle t :array t :gensym t
                                    :escape t :level nil :length nil :radix t
                      ) )
                      (when *package-tasks*
                        (c-eval-when-compile
                          `(PROGN ,@(nreverse *package-tasks*))
                      ) )
                ) ) )
                (c-comment #+DEUTSCH "~&~%Compilation von File ~A beendet."
                           #+ENGLISH "~&~%Compilation of file ~A is finished."
                           file
                )
                (c-comment #+DEUTSCH "~%~D Error~:P, ~D Warnung~:[en~;~]"
                           #+ENGLISH "~%~D error~:P, ~D warning~:P"
                           *error-count* *warning-count* #-ENGLISH (eql *warning-count* 1)
                )
                (when top-call
                  (when *functions-with-errors*
                    (c-comment #+DEUTSCH "~%Es gab Errors in den folgenden Funktionen:~%~{~<~%~:; ~S~>~^~}"
                               #+ENGLISH "~%There were errors in the following functions:~%~{~<~%~:; ~S~>~^~}"
                               (nreverse *functions-with-errors*)
                  ) )
                  (setq *unknown-functions*
                    (nset-difference *unknown-functions* *known-functions*)
                  )
                  (when *unknown-functions*
                    (c-comment #+DEUTSCH "~%Folgende Funktionen wurden verwendet, aber nicht definiert:~%~{~<~%~:; ~S~>~^~}"
                               #+ENGLISH "~%The following functions were used but not defined:~%~{~<~%~:; ~S~>~^~}"
                               (nreverse *unknown-functions*)
                  ) )
                  (let ((unknown-vars (set-difference *unknown-free-vars* *known-special-vars*))
                        (too-late-vars (intersection *unknown-free-vars* *known-special-vars*)))
                    (when unknown-vars
                      (c-comment #+DEUTSCH "~%Folgende Special-Variablen wurden nicht definiert:~%~{~<~%~:; ~S~>~^~}"
                                 #+ENGLISH "~%The following special variables were not defined:~%~{~<~%~:; ~S~>~^~}"
                                 (nreverse unknown-vars)
                    ) )
                    (when too-late-vars
                      (c-comment #+DEUTSCH "~%Folgende Special-Variablen wurden zu spät definiert:~%~{~<~%~:; ~S~>~^~}"
                                 #+ENGLISH "~%The following special variables were defined too late:~%~{~<~%~:; ~S~>~^~}"
                                 (nreverse too-late-vars)
                ) ) ) )
                (c-comment "~%")
                (setq compilation-successful
                  (zerop *error-count*) ; Wert T, falls Compilation erfolgreich
            ) ) )
            (when new-output-stream
              (close output-stream) (close *liboutput-stream*)
              (unless compilation-successful
                (delete-file output-file) (delete-file liboutput-file)
            ) )
        ) )
        (when new-listing-stream (close listing-stream))
) ) ) )

(defun disassemble-closures (closure stream)
  (let ((closures '()))
    (labels ((mark (cl) ; trägt eine Closure cl (rekursiv) in closures ein.
               (push cl closures) ; cl markieren
               (dolist (c (closure-consts cl)) ; und alle Teil-Closures
                 (when #+CLISP (and (sys::closurep c) (compiled-function-p c))
                       #-CLISP (closure-p c)
                   (unless (member c closures) (mark c)) ; ebenfalls markieren
            )) ) )
      (mark closure) ; Haupt-Closure markieren
    )
    (dolist (c (nreverse closures)) ; alle Closures disassemblieren
      (disassemble-closure c stream)
) ) )

#-CLISP
(defun disassemble-closure (closure &optional (stream *standard-output*))
  (format stream #+DEUTSCH "~%~%Disassembly von Funktion ~S"
                 #+ENGLISH "~%~%Disassembly of function ~S"
                 (closure-name closure)
  )
  (multiple-value-bind (req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p
                        byte-list const-list)
      (signature closure)
    (do ((L const-list (cdr L))
         (i 0 (1+ i)))
        ((null L))
      (format stream "~%(CONST ~S) = ~S" i (car L))
    )
    (format stream #+DEUTSCH "~%~S notwendige Argumente"
                   #+ENGLISH "~%~S required arguments"
                   req-anz
    )
    (format stream #+DEUTSCH "~%~S optionale Argumente"
                   #+ENGLISH "~%~S optional arguments"
                   opt-anz
    )
    (format stream #+DEUTSCH "~%~:[Kein Rest-Parameter~;Rest-Parameter vorhanden~]"
                   #+ENGLISH "~%~:[No rest parameter~;Rest parameter~]"
                   rest-p
    )
    (if key-p
      (let ((kw-count (length keyword-list)))
        (format stream #+DEUTSCH "~%~S Keyword-Parameter: ~{~S~^, ~}."
                       #+ENGLISH "~%~S keyword parameter~:P: ~{~S~^, ~}."
                       kw-count keyword-list
        )
        (when allow-other-keys-p
          (format stream #+DEUTSCH "~%Andere Keywords sind zugelassen."
                         #+ENGLISH "~%Other keywords are allowed."
      ) ) )
      (format stream #+DEUTSCH "~%Keine Keyword-Parameter"
                     #+ENGLISH "~%No keyword parameters"
    ) )
    (let ((const-string-list (mapcar #'write-to-string const-list)))
      (do ((L (disassemble-LAP byte-list const-list) (cdr L)))
          ((null L))
        (let ((PC (caar L))
              (instr (cdar L)))
          (format stream "~%~S~6T~A" PC instr)
          (multiple-value-bind ... ; siehe unten
            ...
    ) ) ) )
    (format stream "~%")
) )
#+CLISP
(defun disassemble-closure (closure &optional (stream *standard-output*))
  (terpri stream)
  (terpri stream)
  (write-string #+DEUTSCH "Disassembly von Funktion "
                #+ENGLISH "Disassembly of function "
                stream
  )
  (prin1 (closure-name closure) stream)
  (multiple-value-bind (req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p
                        byte-list const-list)
      (signature closure)
    (do ((L const-list (cdr L))
         (i 0 (1+ i)))
        ((null L))
      (terpri stream)
      (write-string "(CONST " stream)
      (prin1 i stream)
      (write-string ") = " stream)
      (prin1 (car L) stream)
    )
    (terpri stream)
    (prin1 req-anz stream)
    (write-string #+DEUTSCH " notwendige Argumente"
                  #+ENGLISH " required arguments"
                  stream
    )
    (terpri stream)
    (prin1 opt-anz stream)
    (write-string #+DEUTSCH " optionale Argumente"
                  #+ENGLISH " optional arguments"
                  stream
    )
    (terpri stream)
    (if rest-p
      (write-string #+DEUTSCH "Rest-Parameter vorhanden"
                    #+ENGLISH "Rest parameter"
                    stream
      )
      (write-string #+DEUTSCH "Kein Rest-Parameter"
                    #+ENGLISH "No rest parameter"
                    stream
    ) )
    (if key-p
      (let ((kw-count (length keyword-list)))
        (terpri stream)
        (prin1 kw-count stream)
        #+DEUTSCH (write-string " Keyword-Parameter: " stream)
        #+ENGLISH (progn
                    (write-string " keyword parameter" stream)
                    (unless (eql kw-count 1) (write-string "s" stream))
                    (write-string ": " stream)
                  )
        (do ((L keyword-list))
            ((endp L))
          (prin1 (pop L) stream)
          (if (endp L) (write-string "." stream) (write-string ", " stream))
        )
        (when allow-other-keys-p
          (terpri stream)
          (write-string #+DEUTSCH "Andere Keywords sind zugelassen."
                        #+ENGLISH "Other keywords are allowed."
                        stream
      ) ) )
      (progn
        (terpri stream)
        (write-string #+DEUTSCH "Keine Keyword-Parameter"
                      #+ENGLISH "No keyword parameters"
                      stream
    ) ) )
    (let ((const-string-list
            (mapcar #'(lambda (x) (sys::write-to-short-string x 35)) const-list)
         ))
      (do ((L (disassemble-LAP byte-list const-list) (cdr L)))
          ((null L))
        (let ((PC (caar L))
              (instr (cdar L)))
          (terpri stream)
          (prin1 PC stream)
          (dotimes (i (- 6 (sys::line-position stream))) (write-char #\Space stream)) ; Tab 6
          (princ instr stream) ; instr ausgeben, Symbole ohne Package-Marker!
          (multiple-value-bind (commentp comment)
            (when (consp instr)
              (case (first instr)
                ((CALLS1 CALLS1&PUSH CALLS1&STORE CALLS1&JMPIFNOT CALLS1&JMPIF)
                  (values t (%funtabref (second instr)))
                )
                ((CALLS2 CALLS2&PUSH CALLS2&STORE CALLS2&JMPIFNOT CALLS2&JMPIF)
                  (values t (%funtabref (+ 256 (second instr))))
                )
                ((CALLSR CALLSR&PUSH CALLSR&STORE CALLSR&JMPIFNOT CALLSR&JMPIF)
                  (values t (%funtabref (+ funtabR-index (third instr))))
                )
                ((CALL CALL&PUSH)
                  (values 'string (nth (third instr) const-string-list))
                )
                ((CALL0 CALL1 CALL1&PUSH CALL1&JMPIFNOT CALL1&JMPIF
                  CALL2 CALL2&PUSH CALL2&JMPIFNOT CALL2&JMPIF
                  JMPIFEQTO JMPIFNOTEQTO CONST CONST&PUSH SETVALUE GETVALUE
                  GETVALUE&PUSH BIND CONST&STORE CONST&SYMBOL-FUNCTION&PUSH
                  CONST&SYMBOL-FUNCTION COPY-CLOSURE&PUSH COPY-CLOSURE
                  CONST&SYMBOL-FUNCTION&STORE
                 )
                  (values 'string (nth (second instr) const-string-list))
            ) ) )
            (when commentp
              (dotimes (i (max 1 (- 42 (sys::line-position stream)))) (write-char #\Space stream)) ; Tab 42
              (write-string "; " stream)
              (if (eq commentp 'string)
                (write-string comment stream)
                (prin1 comment stream)
    ) ) ) ) ) )
    (terpri stream)
) )

#-CROSS
(defun disassemble (object &aux (recursive-flag nil) name)
  (when (symbolp object)
    (unless (fboundp object)
      (error #+DEUTSCH "Funktion ~S ist undefiniert."
             #+ENGLISH "Undefined function ~S"
             object
    ) )
    (setq recursive-flag t name object)
    (setq object (or (get object 'sys::traced-definition)
                     (symbol-function object)
  ) )            )
  (when (and (consp object) (eq (car object) 'system::macro))
    (setq object (cdr object)) (setq recursive-flag nil)
  )
  (unless (sys::closurep object)
    (error #+DEUTSCH "~S kann nicht disassembliert werden."
           #+ENGLISH "Cannot disassemble ~S"
           object
  ) )
  ; object ist eine Closure.
  (unless (compiled-function-p object)
    (setq object
      (compile-lambda (sys::%record-ref object 0) ; name
                      (sys::%record-ref object 1) ; lambdabody
                      (sys::%record-ref object 4) ; venv
                      (sys::%record-ref object 5) ; fenv
                      (sys::%record-ref object 6) ; benv
                      (sys::%record-ref object 7) ; genv
                      (sys::%record-ref object 8) ; denv
                      ; Flag, ob Tail-Rekursion entrekursiviert wird:
                      (and recursive-flag ; war object ein Symbol und kein Macro?
                           (eq (sys::%record-ref object 0) name) ; Funktion hat selben Namen?
  ) ) )               )
  ; object ist eine compilierte Closure.
  (disassemble-closure object) ; Disassemblieren
  object ; compilierte Closure als Wert
)
