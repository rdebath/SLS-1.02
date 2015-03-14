;;;;   INITIALISIERUNGS-FILE

(in-package "LISP")

;;; Exportierungen:
(export '(
;; Typen:
array atom bignum bit bit-vector character common compiled-function
complex cons double-float fixnum float function hash-table integer keyword
list long-float nil null number package pathname random-state ratio
rational readtable real sequence short-float simple-array simple-bit-vector
simple-string simple-vector single-float standard-char stream string
string-char symbol t vector satisfies values mod signed-byte unsigned-byte
;; Konstanten:
lambda-list-keywords lambda-parameters-limit nil t call-arguments-limit
multiple-values-limit pi boole-clr boole-set boole-1 boole-2 boole-c1 boole-c2
boole-and boole-ior boole-xor boole-eqv boole-nand boole-nor boole-andc1
boole-andc2 boole-orc1 boole-orc2 most-positive-fixnum most-negative-fixnum
most-positive-short-float least-positive-short-float least-negative-short-float
most-negative-short-float most-positive-single-float
least-positive-single-float least-negative-single-float
most-negative-single-float most-positive-double-float
least-positive-double-float least-negative-double-float
most-negative-double-float most-positive-long-float least-positive-long-float
least-negative-long-float most-negative-long-float short-float-epsilon
single-float-epsilon double-float-epsilon long-float-epsilon
short-float-negative-epsilon single-float-negative-epsilon
double-float-negative-epsilon long-float-negative-epsilon
char-code-limit char-font-limit char-bits-limit char-control-bit char-meta-bit
char-super-bit char-hyper-bit array-rank-limit array-dimension-limit
array-total-size-limit internal-time-units-per-second
;; Variablen:
*macroexpand-hook* *package* *modules* *random-state* *evalhook* *applyhook*
+ ++ +++ - * ** *** / // /// *standard-input* *standard-output* *error-output*
*query-io* *debug-io* *terminal-io* *trace-output* *read-base* *read-suppress*
*readtable* *print-escape* *print-pretty* *print-circle* *print-base*
*print-radix* *print-case* *print-gensym* *print-level* *print-length*
*print-array* *read-default-float-format* *default-pathname-defaults*
*load-paths* *load-verbose* *load-print* *load-echo* *break-on-warnings*
*features*
;; Funktionen:
coerce type-of upgraded-array-element-type typep subtypep null symbolp
atom consp listp numberp integerp rationalp floatp realp complexp characterp
stringp bit-vector-p vectorp simple-vector-p simple-string-p
simple-bit-vector-p arrayp packagep functionp compiled-function-p commonp eq
eql equal equalp not symbol-value symbol-function boundp fboundp
special-form-p set makunbound fmakunbound get-setf-method
get-setf-method-multiple-value apply funcall mapcar maplist mapc mapl mapcan
mapcon values values-list macro-function macroexpand macroexpand-1 proclaim
get remprop symbol-plist getf get-properties symbol-name make-symbol
copy-symbol gensym gentemp symbol-package keywordp make-package in-package
find-package package-name package-nicknames rename-package package-use-list
package-used-by-list package-shadowing-symbols list-all-packages intern
find-symbol unintern export unexport import shadowing-import shadow
use-package unuse-package find-all-symbols provide require zerop plusp minusp
oddp evenp = /= < > <= >= max min + - * / 1+ 1- conjugate gcd lcm exp expt
log sqrt isqrt abs phase signum sin cos tan cis asin acos atan sinh cosh tanh
asinh acosh atanh float rational rationalize numerator denominator floor
ceiling truncate round mod rem ffloor fceiling ftruncate fround decode-float
scale-float float-radix float-sign float-digits float-precision
integer-decode-float complex realpart imagpart logior logxor logand logeqv
lognand lognor logandc1 logandc2 logorc1 logorc2 lognot logtest logbitp ash
logcount integer-length byte byte-size byte-position ldb ldb-test mask-field
dpb deposit-field random make-random-state random-state-p standard-char-p
graphic-char-p string-char-p alpha-char-p upper-case-p lower-case-p
both-case-p digit-char-p alphanumericp char= char/= char< char> char<= char>=
char-equal char-not-equal char-lessp char-greaterp char-not-greaterp
char-not-lessp char-code char-bits char-font code-char make-char character
char-upcase char-downcase digit-char char-int int-char char-name name-char
char-bit set-char-bit elt subseq copy-seq length reverse nreverse
make-sequence concatenate map some every notany notevery reduce fill replace
remove remove-if remove-if-not delete delete-if delete-if-not
remove-duplicates delete-duplicates substitute substitute-if
substitute-if-not nsubstitute nsubstitute-if nsubstitute-if-not find find-if
find-if-not position position-if position-if-not count count-if count-if-not
mismatch search sort stable-sort merge car cdr caar cadr cdar cddr caaar
caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar
cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
cons tree-equal endp list-length nth first second third fourth fifth sixth
seventh eighth ninth tenth rest nthcdr last list list* make-list append
copy-list copy-alist copy-tree revappend nconc nreconc butlast nbutlast ldiff
rplaca rplacd subst subst-if subst-if-not nsubst nsubst-if-not sublis nsublis
member member-if member-if-not tailp adjoin union nunion intersection
nintersection set-difference nset-difference set-exclusive-or
nset-exclusive-or subsetp acons pairlis assoc assoc-if assoc-if-not rassoc
rassoc-if rassoc-if-not make-hash-table hash-table-p gethash remhash maphash
clrhash hash-table-count sxhash make-array vector aref svref
array-element-type array-rank array-dimension array-dimensions
array-total-size array-in-bounds-p array-row-major-index adjustable-array-p
bit sbit bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2
bit-orc1 bit-orc2 bit-not array-has-fill-pointer-p fill-pointer vector-push
vector-push-extend vector-pop adjust-array char schar string= string-equal
string< string> string<= string>= string/= string-lessp string-greaterp
string-not-greaterp string-not-lessp string-not-equal make-string string-trim
string-left-trim string-right-trim string-upcase string-downcase
string-capitalize nstring-upcase nstring-downcase nstring-capitalize string
eval evalhook applyhook constantp make-synonym-stream make-broadcast-stream
make-concatenated-stream make-two-way-stream make-echo-stream
make-string-input-stream make-string-output-stream get-output-stream-string
streamp input-stream-p output-stream-p stream-element-type interactive-stream-p
close copy-readtable readtablep set-syntax-from-char set-macro-character
get-macro-character make-dispatch-macro-character
set-dispatch-macro-character get-dispatch-macro-character read
read-preserving-whitespace read-delimited-list read-line read-char
unread-char peek-char listen read-char-no-hang clear-input read-from-string
parse-integer read-byte write prin1 print pprint princ write-to-string
prin1-to-string princ-to-string write-char write-string write-line terpri
fresh-line finish-output force-output clear-output write-byte format y-or-n-p
yes-or-no-p pathname truename parse-namestring merge-pathnames make-pathname
pathnamep pathname-host pathname-device pathname-directory pathname-name
pathname-type pathname-version namestring file-namestring
directory-namestring host-namestring enough-namestring user-homedir-pathname
open rename-file delete-file probe-file file-write-date file-author
file-position file-length load directory error cerror warn break compile
compile-file disassemble
documentation  variable structure type ; drei Dokumentations-Typen
describe inspect room ed dribble apropos apropos-list get-decoded-time
get-universal-time decode-universal-time encode-universal-time
get-internal-run-time get-internal-real-time sleep lisp-implementation-type
lisp-implementation-version machine-type machine-version machine-instance
software-type software-version short-site-name long-site-name identity
;; Special-forms:
eval-when quote function setq progn let let* compiler-let progv flet labels
macrolet if block return-from tagbody go multiple-value-call
multiple-value-prog1 catch unwind-protect throw declare the
;; Macros:
deftype defun defvar defparameter defconstant and or psetq setf psetf shiftf
rotatef define-modify-macro defsetf define-setf-method prog1 prog2
when unless cond
case typecase  otherwise ; otherwise als Marker für die catchall-clause
return loop do do* dolist dotimes prog prog* multiple-value-list
multiple-value-bind multiple-value-setq defmacro locally remf do-symbols
do-external-symbols do-all-symbols incf decf push pushnew pop defstruct
with-open-stream with-input-from-string with-output-to-string with-open-file
check-type assert etypecase ctypecase ecase ccase trace untrace step time
;; sonstige Markierer:
eval load compile ; EVAL-WHEN-Situationen
special type ftype function inline notinline ignore optimize speed space
safety compilation-speed declaration compile ; DECLARE-Specifier
interpreter compiler ; Features
))

(sys::%proclaim-constant 'lambda-list-keywords
  '(&optional &rest &key &allow-other-keys &aux &body &whole &environment)
)
(export lambda-list-keywords)

(sys::%putd 'exit #'sys::%exit)
(sys::%putd 'quit #'sys::%exit)
(sys::%putd 'bye #'sys::%exit)
(export '(exit quit bye))

(proclaim '(special *features*))
; Nach der Initialisierung (in IO.Q bzw. SPVW.D) enthält *features*
; als drittes Symbol  (first (sys::version)) = SYS::CLISP1/2/3
; und als letztes Symbol  (intern *language* "LISP").
(import *features*)
(export *features*)

(in-package "SYSTEM" :nicknames '("SYS" "COMPILER"))
(setq compiler::*compiling* nil)

(in-package "SYSTEM")

#-COMPILER ; nur beim Bootstrappen
(progn

; vorläufig soll bei GET_CLOSURE nicht expandiert werden:
(sys::%putd '%expand-lambdabody-main
  (function %expand-lambdabody-main
    (lambda (lambdabody fenv)
      (declare (source nil) (ignore fenv))
      lambdabody
) ) )

; vorläufig soll defun ganz trivial expandiert werden:
(sys::%putd 'defun
  (cons 'sys::macro
    (function defun
      (lambda (form env)
        (declare (ignore env))
        #|
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          `(SYS::%PUTD ',name (FUNCTION ,name (LAMBDA ,lambdalist ,@body)))
        )
        |#
        (let ((name (cadr form)))
          (list 'sys::%putd (list 'quote name)
            (list 'function name (cons 'lambda (cddr form)))
        ) )
    ) )
) )

)

(sys::%putd 'sys::remove-old-definitions
  (function sys::remove-old-definitions
    (lambda (symbol) ; entfernt die alten Funktionsdefinitionen eines Symbols
      (if (special-form-p symbol)
        (error #+DEUTSCH "~S ist eine Special-Form und darf nicht umdefiniert werden."
               #+ENGLISH "~S is a special form and may not be redefined."
               #+FRANCAIS "~S est une forme spéciale et ne peut pas être redéfinie."
               symbol
      ) )
      (if (and (or (fboundp symbol) (macro-function symbol))
               (let ((pack (symbol-package symbol)))
                 (and pack (equal (package-name pack) "LISP"))
          )    )
        (cerror #+DEUTSCH "Die alte Definition wird weggeworfen."
                #+ENGLISH "The old definition will be lost"
                #+FRANCAIS "L'ancienne définition sera perdue."
                #+DEUTSCH "D~2@*~:[ie~;er~]~0@* COMMON-LISP-~A ~S wird umdefiniert."
                #+ENGLISH "Redefining the COMMON LISP ~A ~S"
                #+FRANCAIS "L~2@*~:[a~;e~]~0@* ~A ~S de COMMON-LISP va être redéfini~:[e~;~]."
                (fbound-string symbol) ; "Funktion" bzw. "Macro"
                symbol
                #+(or DEUTSCH FRANCAIS) (macro-function symbol)
      ) )
      (fmakunbound symbol) ; Funktions-/Macro-Definition streichen
      ; Property sys::definition wird nicht entfernt, da sie sowieso
      ; bald neu gesetzt wird.
      (remprop symbol 'sys::macro) ; Macro-Definition streichen
      (when (get symbol 'sys::documentation-strings) ; Dokumentation streichen
        (sys::%set-documentation symbol 'FUNCTION nil)
      )
      (when (get symbol 'sys::inline-expansion)
        (sys::%put symbol 'sys::inline-expansion t)
      )
      (when (get symbol 'sys::traced-definition) ; Trace streichen
        (warn #+DEUTSCH "DEFUN/DEFMACRO: ~S war getraced und wird umdefiniert!"
              #+ENGLISH "DEFUN/DEFMACRO: redefining ~S; it was traced!"
              #+FRANCAIS "DEFUN/DEFMACRO : ~S était tracée et est redéfinie!"
              symbol
        )
        (untrace2 symbol)
    ) )
) )

;;; Funktionen zum Expandieren von Macros innerhalb eines Codestückes
;;;
;;; Insgesamt wird der gesamte Code (einer Funktion) durchgegangen und
;;; globale und lokale Macros expandiert.
;;; Aus       #'(lambda lambdalist . body)
;;; wird so   #'(lambda expanded-lambdalist
;;;               (declare (source (lambdalist . body))) . expanded-body
;;;             )
;;; Durch diese Deklaration ist gewährleistet, daß eine bereits einmal
;;; durchlaufene Funktion als solche erkannt und nicht unnötigerweise ein
;;; zweites Mal durchlaufen wird.

; Vorsicht! Fürs Bootstrappen (erkennbar an #-COMPILER) müssen manche der
; Funktionen in primitiverem Lisp (ohne do, do*, case) geschrieben werden.

(PROGN

(proclaim '(special *keyword-package*))
(setq *keyword-package* (find-package "KEYWORD"))

(proclaim '(special *fenv*))
; *fenv* = Das aktuelle Function-Environment während der Expansion
; einer Form. Struktur: NIL oder ein 2n+1-elementiger Vektor
; (n1 f1 ... nn fn next), wo die ni Symbole sind, die fi ihre funktionale
; Bedeutung sind (Closure oder (MACRO . Closure) oder noch NIL); bei next
; geht's ebenso weiter.

; (fenv-assoc s fenv) sucht Symbol s in Function-Environment fenv.
(defun fenv-assoc (s fenv)
  (if fenv
    (if (simple-vector-p fenv)
      #+COMPILER
      (do ((l (1- (length fenv)))
           (i 0 (+ i 2)))
          ((= i l) (fenv-assoc s (svref fenv i)))
        (if (eq s (svref fenv i))
          (return (svref fenv (1+ i)))
      ) )
      #-COMPILER
      (let ((l (1- (length fenv)))
            (i 0))
        (block nil
          (tagbody
            1 (if (= i l) (return-from nil (fenv-assoc s (svref fenv i))))
              (if (eq s (svref fenv i))
                (return-from nil (svref fenv (1+ i)))
              )
              (setq i (+ i 2))
              (go 1)
      ) ) )
      (error #+DEUTSCH "~S ist kein korrektes Function-Environment."
             #+ENGLISH "~S is an invalid function environment"
             #+FRANCAIS "~S n'est pas un environnement de fonction correct."
             fenv
    ) )
    'T ; nicht gefunden
) )

; Die meisten Expansionsfunktionen liefern zwei Werte: Das Expansions-
; ergebnis, der zweite Wert (NIL oder T) zeigt an, ob darin etwas verändert
; wurde.

; (%expand-cons ...) setzt ein cons zusammen. 2 Werte.
; form=alte Form,
; expf,flagf = Expansion des First-Teils,
; expr,flagr = Expansion des Rest-Teils.
(defun %expand-cons (form expf flagf expr flagr)
  (if (or flagf flagr)
    (values (cons expf expr) t)
    (values form nil)
) )

#+COMPILER

; (%expand-form form) expandiert eine ganze Form. 2 Werte.
(defun %expand-form (form)
  (if (atom form)
    (values form nil)
    ; form ist CONS
    (let ((f (first form)))
      (if (symbolp f)
        (let ((h (fenv-assoc f *fenv*)))
          ; f ist in *fenv* assoziiert zu h
          (if (eq h 'T)
            ; f hat keine lokale Definition
            ; Nun die einzelnen Expander für die Special-forms:
            (case f
              ((RETURN-FROM THE MULTIPLE-VALUE-SETQ MULTIPLE-VALUE-BIND
                DEFVAR DEFPARAMETER DEFCONSTANT
               )
                ; 1. Argument lassen, alle weiteren expandieren
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (multiple-value-call #'%expand-cons (rest form)
                    (second form) nil
                    (%expand-list (cddr form))
              ) ) )
              ((QUOTE GO DECLARE) ; nichts expandieren
                (values form nil)
              )
              (FUNCTION
                ; Falls erstes bzw. zweites Argument Liste,
                ; als Lambda-Ausdruck expandieren.
                (multiple-value-call #'%expand-cons form
                  'FUNCTION nil
                  (if (atom (cddr form))
                    (if (atom (second form))
                      (if (symbolp (second form))
                        (let ((h (fenv-assoc (second form) *fenv*)))
                          (cond ((or (eq h 'T) (closurep h) (null h)) (values (rest form) nil))
                                ((and (consp h) (eq (first h) 'MACRO))
                                 (error #+DEUTSCH "~S: ~S unzulässig, da ~S ein lokaler Macro ist"
                                        #+ENGLISH "~S: ~S is illegal since ~S is a local macro"
                                        #+FRANCAIS "~S : ~S est illégal car ~S est un macro local"
                                        '%expand form (second form)
                                ))
                                (t (error #+DEUTSCH "~S: Falscher Aufbau eines Function-Environment: ~S"
                                          #+ENGLISH "~S: invalid function environment ~S"
                                          #+FRANCAIS "~S : mauvais environnement de fonction ~S"
                                          '%expand *fenv*
                                )  )
                        ) )
                        (error #+DEUTSCH "~S: ~S unzulässig, da ~S kein Symbol"
                               #+ENGLISH "~S: ~S is invalid since ~S is not a symbol"
                               #+FRANCAIS "~S : ~S est illégal car ~S n'est pas un symbole"
                               '%expand form (second form)
                      ) )
                      (multiple-value-call #'%expand-cons (rest form)
                        (%expand-lambda (second form))
                        (cddr form) nil
                    ) )
                    (multiple-value-call #'%expand-cons (rest form)
                      (second form) nil
                      (multiple-value-call #'%expand-cons (cddr form)
                        (%expand-lambda (third form))
                        (cdddr form) nil
              ) ) ) ) )
              (EVAL-WHEN
                ; Falls die Situation COMPILE angegeben ist, führe den Body
                ; als PROGN aus, gib eine Form zurück, die ohne Seiteneffekte
                ; dieselben Werte liefert.
                ; Sonst expandiere alle Argumente ab dem zweiten als Formen.
                (if (member 'COMPILE (second form))
                  (values
                    (list 'values-list
                      (list 'quote
                        (multiple-value-list (eval (cons 'PROGN (cddr form))))
                    ) )
                    t
                  )
                  (multiple-value-call #'%expand-cons form
                    (first form) nil
                    (multiple-value-call #'%expand-cons (rest form)
                      (second form) nil
                      (%expand-list (cddr form))
              ) ) ) )
              ((LET LET*) ; Variablenliste und Body expandieren
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (multiple-value-call #'%expand-cons (rest form)
                    (%expand-varspez (second form))
                    (%expand-list (cddr form))
              ) ) )
              (COMPILER-LET
                ; Variablenliste im leeren Environment und Body expandieren
                (progv
                  (mapcar #'%expand-varspec-var (second form))
                  (mapcar #'%expand-varspec-val (second form))
                  (values (%expand-form (cons 'PROGN (cddr form))) t)
              ) )
              (COND ; Alle Teilformen der Klauseln expandieren:
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (%expand-cond (rest form))
              ) )
              (BLOCK
                ; Body expandieren. Falls darin ein RETURN-FROM auf diesen
                ; Block vorkommt, behalte BLOCK. Sonst mache ein PROGN daraus.
                (multiple-value-bind (body flagb) (%expand-list (cddr form))
                  (if (%return-p (second form) body)
                    (multiple-value-call #'%expand-cons form
                      (first form) nil
                      (multiple-value-call #'%expand-cons (rest form)
                        (second form) nil
                        body flagb
                    ) )
                    (values
                      (cond ((atom body) body)
                            ((null (cdr body)) (car body))
                            (t (cons 'progn body))
                      )
                      t
              ) ) ) )
              ((SETQ PSETQ) ; jedes zweite Argument expandieren
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (%expand-setqlist (rest form))
              ) )
              (TAGBODY
                ; alle Argumente expandieren, dabei entstehende Atome weglassen
                (multiple-value-call #'%expand-cons form
                  (first form) nil
                  (%expand-tagbody (rest form))
              ) )
              (PROGN ; alle Argumente expandieren, evtl. vereinfachen.
                (if (null (rest form))
                  (values nil t)
                  (if (null (cddr form))
                    (values (%expand-form (second form)) t)
                    (multiple-value-call #'%expand-cons form
                      (first form) nil
                      (%expand-list (rest form))
              ) ) ) )
              (FLET ; Funktionsdefinitionen expandieren,
                    ; Body im erweiterten Environment expandieren
                (if (null (second form))
                  (values (%expand-form (cons 'PROGN (cddr form))) t)
                  (let ((newfenv (%expand-fundefs-1 (second form))))
                    (multiple-value-call #'%expand-cons form
                      (first form) nil
                      (multiple-value-call #'%expand-cons (rest form)
                        (%expand-fundefs-2 (second form))
                        (let ((*fenv* (apply #'vector newfenv)))
                          (%expand-list (cddr form))
              ) ) ) ) ) )
              (LABELS ; Funktionsdefinitionen und Body im erweiterten Environment expandieren
                (if (null (second form))
                  (values (%expand-form (cons 'PROGN (cddr form))) t)
                  (let ((newfenv (%expand-fundefs-1 (second form))))
                    (let ((*fenv* (apply #'vector newfenv)))
                      (multiple-value-call #'%expand-cons form
                        (first form) nil
                        (multiple-value-call #'%expand-cons (rest form)
                          (%expand-fundefs-2 (second form))
                          (%expand-list (cddr form))
              ) ) ) ) ) )
              (MACROLET ; Body im erweiterten Environment expandieren
                (do ((L1 (second form) (cdr L1))
                     (L2 nil))
                    ((atom L1)
                     (if L1
                       (error #+DEUTSCH "Dotted list im Code von MACROLET, endet mit ~S"
                              #+ENGLISH "code after MACROLET contains a dotted list, ending with ~S"
                              #+FRANCAIS "Le code de MACROLET contient une paire pointée, terminée par ~S"
                              L1
                       )
                       (let ((*fenv* (apply #'vector (nreverse (cons *fenv* L2)))))
                         (values (%expand-form (cons 'PROGN (cddr form))) t)
                    )) )
                  (let ((macrodef (car L1)))
                    (if (and (consp macrodef)
                             (symbolp (car macrodef))
                             (consp (cdr macrodef))
                        )
                      (setq L2
                        (cons (cons 'MACRO
                                    (eval (make-macro-expansion macrodef))
                              )
                              (cons (car macrodef) L2)
                      ) )
                      (error #+DEUTSCH "Falsche Syntax in MACROLET: ~S"
                             #+ENGLISH "illegal syntax in MACROLET: ~S"
                             #+FRANCAIS "syntaxe illégale dans MACROLET : ~S"
                             macrodef
              ) ) ) ) )
              (t
                (cond ((special-form-p f)
                       ; sonstige Special-forms,
                       ; z.B. IF, CATCH, THROW, PROGV, UNWIND-PROTECT, PROGN,
                       ; PROG1, PROG2, WHEN, UNLESS, MULTIPLE-VALUE-LIST,
                       ; MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, AND, OR:
                       (multiple-value-call #'%expand-cons form
                         f nil
                         (%expand-list (rest form))
                      ))
                      ((setq h (macro-function f)) ; globale Macro-Definition
                       (values (%expand-form (funcall h form *fenv*)) t)
                      )
                      (t ; normaler Funktionsaufruf
                       (multiple-value-call #'%expand-cons form
                         f nil
                         (%expand-list (rest form))
            ) ) )     ))
            ; f hat eine lokale Definition
            (cond ((or (closurep h) (null h)); aufzurufende Funktion
                   (multiple-value-call #'%expand-cons form
                     f nil
                     (%expand-list (rest form))
                  ))
                  ((and (consp h) (eq (car h) 'MACRO)) ; zu expandierender Macro
                   (values (%expand-form (funcall (cdr h) form *fenv*)) t)
                  ) ; Expander aufrufen
                  (t (error #+DEUTSCH "Falscher Aufbau eines Function-Environment in ~S: ~S"
                            #+ENGLISH "bad function environment occurred in ~S: ~S"
                            #+FRANCAIS "mauvais environnement de fonction dans ~S : ~S"
                            '%expand-form *fenv*
        ) ) )     )  )
        (if (consp f)
          (multiple-value-call #'%expand-cons form
            (%expand-lambda f)
            (%expand-list (rest form))
          )
          (error #+DEUTSCH "~S: ~S ist keine korrekte Form"
                 #+ENGLISH "~S: invalid form ~S"
                 #+FRANCAIS "~S : forme Lisp incorrecte ~S"
                 '%expand-form form
) ) ) ) ) )

#-COMPILER
(progn

; (%expand-form form) expandiert eine ganze Form. 2 Werte.
(defun %expand-form (form)
  (if (atom form)
    (values form nil)
    ; form ist CONS
    (let ((f (first form)))
      (if (symbolp f)
        (let ((h (fenv-assoc f *fenv*)))
          ; f ist in *fenv* assoziiert zu h
          (if (eq h 'T)
            ; f hat keine lokale Definition
            (cond ((setq h (get '%expand f)) ; special forms u.ä.
                   (funcall h form)
                  )
                  ((special-form-p f)
                   ; sonstige Special-forms,
                   ; z.B. IF, CATCH, THROW, PROGV, UNWIND-PROTECT, PROGN,
                   ; PROG1, PROG2, WHEN, UNLESS, MULTIPLE-VALUE-LIST,
                   ; MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, AND, OR:
                   (multiple-value-call #'%expand-cons form
                     f nil
                     (%expand-list (rest form))
                  ))
                  ((setq h (macro-function f)) ; globale Macro-Definition
                   (values (%expand-form (funcall h form *fenv*)) t)
                  )
                  (t ; normaler Funktionsaufruf
                   (multiple-value-call #'%expand-cons form
                     f nil
                     (%expand-list (rest form))
            )     ))
            ; f hat eine lokale Definition
            (cond ((or (closurep h) (null h)); aufzurufende Funktion
                   (multiple-value-call #'%expand-cons form
                     f nil
                     (%expand-list (rest form))
                  ))
                  ((and (consp h) (eq (car h) 'MACRO)) ; zu expandierender Macro
                   (values (%expand-form (funcall (cdr h) form *fenv*)) t)
                  ) ; Expander aufrufen
                  (t (error #+DEUTSCH "Falscher Aufbau eines Function-Environment in ~S: ~S"
                            #+ENGLISH "bad function environment occurred in ~S: ~S"
                            #+FRANCAIS "mauvais environnement de fonction dans ~S : ~S"
                            '%expand-form *fenv*
        ) ) )     )  )
        (if (consp f)
          (multiple-value-call #'%expand-cons form
            (%expand-lambda f)
            (%expand-list (rest form))
          )
          (error #+DEUTSCH "~S: ~S ist keine korrekte Form"
                 #+ENGLISH "~S: invalid form ~S"
                 #+FRANCAIS "~ : forme Lisp incorrecte ~S"
                 '%expand-form form
) ) ) ) ) )

; Nun die einzelnen Expander für die Special-forms:

; RETURN-FROM, THE, MULTIPLE-VALUE-SETQ, MULTIPLE-VALUE-BIND,
; DEFVAR, DEFPARAMETER, DEFCONSTANT:
; 1. Argument lassen, alle weiteren expandieren
(defun %expand-ab2 (form)
  (multiple-value-call #'%expand-cons form
      (first form) nil
      (multiple-value-call #'%expand-cons (rest form)
          (second form) nil
          (%expand-list (cddr form))
) )   )
(%put '%expand 'RETURN-FROM #'%expand-ab2)
(%put '%expand 'THE #'%expand-ab2)
(%put '%expand 'MULTIPLE-VALUE-SETQ #'%expand-ab2)
(%put '%expand 'MULTIPLE-VALUE-BIND #'%expand-ab2)
(%put '%expand 'DEFVAR #'%expand-ab2)
(%put '%expand 'DEFPARAMETER #'%expand-ab2)
(%put '%expand 'DEFCONSTANT #'%expand-ab2)

; QUOTE, GO, DECLARE: nichts expandieren
(let ((fun
        (function %expand-quote/go/declare (lambda (form) (values form nil)))
     ))
  (%put '%expand 'QUOTE fun)
  (%put '%expand 'GO fun)
  (%put '%expand 'DECLARE fun)
)

; FUNCTION:
; Falls erstes bzw. zweites Argument Liste, als Lambda-Ausdruck expandieren.
(%put '%expand 'FUNCTION
  (function %expand-function
    (lambda (form)
      (multiple-value-call #'%expand-cons form
          'FUNCTION nil
          (if (atom (cddr form))
            (if (atom (second form))
              (if (symbolp (second form))
                (let ((h (fenv-assoc (second form) *fenv*)))
                  (cond ((or (eq h 'T) (closurep h) (null h)) (values (rest form) nil))
                        ((and (consp h) (eq (first h) 'MACRO))
                         (error #+DEUTSCH "~S: ~S unzulässig, da ~S ein lokaler Macro ist"
                                #+ENGLISH "~S: ~S is illegal since ~S is a local macro"
                                #+FRANCAIS "~S : n'est pas permis car ~S est un macro local"
                                '%expand form (second form)
                        ))
                        (t (error #+DEUTSCH "~S: Falscher Aufbau eines Function-Environment: ~S"
                                  #+ENGLISH "~S: invalid function environment ~S"
                                  #+FRANCAIS "~S : mauvais environnement de fonction ~S"
                                  '%expand *fenv*
                        )  )
                ) )
                (error #+DEUTSCH "~S: ~S unzulässig, da ~S kein Symbol"
                       #+ENGLISH "~S: ~S is invalid since ~S is not a symbol"
                       #+FRANCAIS "~S : ~S est inadmissible car ~S n'est pas un symbole"
                       '%expand form (second form)
              ) )
              (multiple-value-call #'%expand-cons (rest form)
                  (%expand-lambda (second form))
                  (cddr form) nil
            ) )
            (multiple-value-call #'%expand-cons (rest form)
                (second form) nil
                (multiple-value-call #'%expand-cons (cddr form)
                    (%expand-lambda (third form))
                    (cdddr form) nil
  ) ) )   ) )   )
)

; EVAL-WHEN:
; Falls die Situation COMPILE angegeben ist, führe den Body als PROGN aus,
;   gib eine Form zurück, die ohne Seiteneffekte dieselben Werte liefert.
; Sonst expandiere alle Argumente ab dem zweiten als Formen.
(%put '%expand 'EVAL-WHEN
  (function %expand-eval-when
    (lambda (form)
      (if (member 'COMPILE (second form))
        (values
          (list 'values-list
            (list 'quote
              (multiple-value-list (eval (cons 'PROGN (cddr form))))
          ) )
          t
        )
        (%expand-ab2 form)
  ) ) )
)

; LET, LET*: Variablenliste und Body expandieren
(let ((fun
        (function %expand-let/let*
          (lambda (form)
            (multiple-value-call #'%expand-cons form
                (first form) nil
                (multiple-value-call #'%expand-cons (rest form)
                    (%expand-varspez (second form))
                    (%expand-list (cddr form))
        ) ) )   )
     ))
  (%put '%expand 'LET fun)
  (%put '%expand 'LET* fun)
)

; COMPILER-LET: Variablenliste im leeren Environment und Body expandieren
(%put '%expand 'COMPILER-LET
  (function %expand-compiler-let
    (lambda (form)
      (progv
        (mapcar #'%expand-varspec-var (second form))
        (mapcar #'%expand-varspec-val (second form))
        (values (%expand-form (cons 'PROGN (cddr form))) t)
  ) ) )
)

; COND: Alle Teilformen der Klauseln expandieren:
(%put '%expand 'cond
  (function %expand-cond
    (lambda (form)
      (multiple-value-call #'%expand-cons form
          (first form) nil
          (%expand-cond (rest form))
  ) ) )
)

; BLOCK: Body expandieren. Falls darin ein RETURN-FROM auf diesen Block
; vorkommt, behalte BLOCK. Sonst mache ein PROGN daraus.
(%put '%expand 'block
  (function %expand-block
    (lambda (form)
      (multiple-value-bind (body flagb) (%expand-list (cddr form))
        (if (%return-p (second form) body)
          (multiple-value-call #'%expand-cons form
              (first form) nil
              (multiple-value-call #'%expand-cons (rest form)
                  (second form) nil
                  body flagb
          )   )
          (values
            (cond ((atom body) body)
                  ((null (cdr body)) (car body))
                  (t (cons 'progn body))
            )
            t
  ) ) ) ) )
)

; SETQ, PSETQ: jedes zweite Argument expandieren
(let ((fun
        (function %expand-setq/psetq
          (lambda (form)
            (multiple-value-call #'%expand-cons form
                (car form) nil
                (%expand-setqlist (cdr form))
        ) ) )
     ))
  (%put '%expand 'SETQ fun)
  (%put '%expand 'PSETQ fun)
)

; TAGBODY: alle Argumente expandieren, dabei entstehende Atome weglassen
(%put '%expand 'tagbody
  (function %expand-tagbody
    (lambda (form)
      (multiple-value-call #'%expand-cons form
          (first form) nil
          (%expand-tagbody (rest form))
  ) ) )
)

; PROGN: alle Argumente expandieren, evtl. vereinfachen.
(%put '%expand 'progn
  (function %expand-progn
    (lambda (form)
      (if (null (rest form))
        (values nil t)
        (if (null (cddr form))
          (values (%expand-form (second form)) t)
          (multiple-value-call #'%expand-cons form
              (first form) nil
              (%expand-list (rest form))
  ) ) ) ) )
)

; FLET: Funktionsdefinitionen expandieren,
; Body im erweiterten Environment expandieren
(%put '%expand 'flet
  (function %expand-flet
    (lambda (form)
      (if (null (second form))
        (values (%expand-form (cons 'PROGN (cddr form))) t)
        (let ((newfenv (%expand-fundefs-1 (second form))))
          (multiple-value-call #'%expand-cons form
            (car form) nil
            (multiple-value-call #'%expand-cons (cdr form)
              (%expand-fundefs-2 (second form))
              (let ((*fenv* (apply #'vector newfenv)))
                (%expand-list (cddr form))
  ) ) ) ) ) ) )
)

; LABELS: Funktionsdefinitionen und Body im erweiterten Environment expandieren
(%put '%expand 'labels
  (function %expand-labels
    (lambda (form)
      (if (null (second form))
        (values (%expand-form (cons 'PROGN (cddr form))) t)
        (let ((newfenv (%expand-fundefs-1 (second form))))
          (let ((*fenv* (apply #'vector newfenv)))
            (multiple-value-call #'%expand-cons form
              (car form) nil
              (multiple-value-call #'%expand-cons (cdr form)
                (%expand-fundefs-2 (second form))
                (%expand-list (cddr form))
  ) ) ) ) ) ) )
)

; MACROLET: Body im erweiterten Environment expandieren
(%put '%expand 'macrolet
  (function %expand-macrolet
    (lambda (form)
      (do ((L1 (second form) (cdr L1))
           (L2 nil))
          ((atom L1)
           (if L1
             (error #+DEUTSCH "Dotted list im Code von MACROLET, endet mit ~S"
                    #+ENGLISH "code after MACROLET contains a dotted list, ending with ~S"
                    #+FRANCAIS "Le code de MACROLET contient une paire pointée, terminée par ~S"
                    L1
             )
             (let ((*fenv* (apply #'vector (nreverse (cons *fenv* L2)))))
               (values (%expand-form (cons 'PROGN (cddr form))) t)
          )) )
        (let ((macrodef (car L1)))
          (if (and (consp macrodef) (symbolp (car macrodef)) (consp (cdr macrodef)))
            (setq L2
              (cons (cons 'MACRO (eval (make-macro-expansion macrodef)))
                    (cons (car macrodef) L2)
            ) )
            (error #+DEUTSCH "Falsche Syntax in MACROLET: ~S"
                   #+ENGLISH "illegal syntax in MACROLET: ~S"
                   #+FRANCAIS "syntaxe illégale dans MACROLET : ~S"
                   macrodef
  ) ) ) ) ) )
)

)

; Hilfsfunktionen für die Expansion:

; expandiert eine Liste von Formen. 2 Werte.
(defun %expand-list (l)
  (if (atom l)
    (if l
      (error #+DEUTSCH "Dotted list im Code, endet mit ~S"
             #+ENGLISH "code contains a dotted list, ending with ~S"
             #+FRANCAIS "une paire pointée dans le code, terminée par ~S"
             l
      )
      (values nil nil)
    )
    (multiple-value-call #'%expand-cons l
                         (%expand-form (first l))
                         (%expand-list (rest l))
) ) )

; expandiert einen Funktionsnamen, der ein Cons ist (das muß ein
; Lambda-Ausdruck sein). 2 Werte.
(defun %expand-lambda (l)
  (unless (eq (first l) 'lambda)
    (error #+DEUTSCH "~S: ~S sollte LAMBDA-Ausdruck sein"
           #+ENGLISH "~S: ~S should be a lambda expression"
           #+FRANCAIS "~S : ~S devrait être une expression LAMBDA"
           '%expand-form l
  ) )
  (multiple-value-call #'%expand-cons l
      'lambda nil ; LAMBDA
      (%expand-lambdabody (rest l))
) )

; expandiert den CDR eines Lambda-Ausdrucks, ein (lambdalist . body). 2 Werte.
(defun %expand-lambdabody (lambdabody)
  (let ((body (rest lambdabody)))
    (if (and (consp body)
             (let ((form (car body)))
               (and (consp form)
                    (eq (car form) 'DECLARE)
                    (let ((declspecs (cdr form)))
                      (and (consp declspecs)
                           (let ((declspec (car declspecs)))
                             (and (consp declspec)
                                  (eq (car declspec) 'SOURCE)
        )    ) )    ) )    ) )
      (values lambdabody nil) ; bereits expandiert -> unberührt lassen
      (values (list*
                (%expand-lambdalist (first lambdabody))
                (list 'DECLARE (list 'SOURCE lambdabody))
                (%expand-list (rest lambdabody))
              )
              t
) ) ) )

; expandiert eine Lambdaliste. 2 Werte.
(defun %expand-lambdalist (ll)
  (if (atom ll)
    (if ll
      (error #+DEUTSCH "Lambdaliste darf nicht mit dem Atom ~S enden"
             #+ENGLISH "lambda list must not end with the atom ~S"
             #+FRANCAIS "La liste lambda ne peut pas se terminer par l'atome ~S"
             ll
      )
      (values nil nil)
    )
    (multiple-value-call #'%expand-cons ll
                         (%expand-parspez (first ll))
                         (%expand-lambdalist (rest ll))
) ) )

; expandiert ein Element einer Lambdaliste. 2 Werte.
; (Expandiert dabei nur bei Listen, und dann auch nur das zweite Element.)
(defun %expand-parspez (ps)
  (if (or (atom ps) (atom (rest ps)))
    (values ps nil)
    (multiple-value-call #'%expand-cons ps
        (first ps) nil
        (multiple-value-call #'%expand-cons (rest ps)
            (%expand-form (second ps))
            (cddr ps) nil
) ) )   )

; expandiert eine Variablenliste. 2 Werte.
(defun %expand-varspez (vs)
  (if (atom vs)
    (if vs
      (error #+DEUTSCH "Variablenliste endet mit dem Atom ~S"
             #+ENGLISH "variable list ends with the atom ~S"
             #+FRANCAIS "La liste de variables se termine par l'atome ~S"
             vs
      )
      (values nil nil)
    )
    (multiple-value-call #'%expand-cons vs
        (%expand-parspez (first vs)) ; Bei Liste 2. Element expandieren
        (%expand-varspez (rest vs))
) ) )

(defun %expand-varspec-var (varspec)
  (if (atom varspec) varspec (first varspec))
)

(defun %expand-varspec-val (varspec)
  (if (atom varspec) nil (eval (second varspec)))
)

; Expandiert eine Cond-Klausel-Liste. 2 Werte.
(defun %expand-cond (clauses)
  (if (atom clauses)
    (values clauses nil)
    (multiple-value-call #'%expand-cons clauses
        (%expand-list (first clauses))
        (%expand-cond (rest clauses))
) ) )

; Auf den bereits expandierten Body wird folgendes angewandt:
; (%return-p name list) stellt fest, ob die Formenliste list irgendwo ein
; (RETURN-FROM name ...) enthält.
(defun %return-p (name body)
  (block return-p
    (tagbody 1
      (if (atom body) (return-from return-p nil))
      (let ((form (car body)))
        (if
          ; stelle fest, ob form ein (RETURN-FROM name ...) enthält:
          (and (consp form)
               (or (and (eq (first form) 'return-from) ; (RETURN-FROM name ...)
                        (eq (second form) name)
                   )
                   (and (consp (first form))           ; Lambdaliste
                        (%return-p name (first form))
                   )
                   (and (not ; keine neue Definition desselben Blocks ?
                          (and (eq (first form) 'block) (eq (second form) name))
                        )
                        (%return-p name (rest form)) ; Funktionsaufruf
          )    )   )
          (return-from return-p t)
      ) )
      (setq body (cdr body))
      (go 1)
) ) )

(defun %expand-setqlist (l)
  (if (or (atom l) (atom (cdr l)))
    (values l nil)
    (multiple-value-call #'%expand-cons l
        (first l) nil
        (multiple-value-call #'%expand-cons (rest l)
            (%expand-form (second l))
            (%expand-setqlist (cddr l))
) ) )   )

; (%expand-tagbody list) expandiert die Elemente einer Liste und läßt dabei
; entstehende Atome fest (damit keine neuen Tags entstehen, die andere Tags
; verdecken könnten). 2 Werte.
(defun %expand-tagbody (body)
  (cond ((atom body) (values body nil))
        ((atom (first body))
         (multiple-value-call #'%expand-cons body
             (first body) nil
             (%expand-tagbody (rest body))
        ))
        (t (multiple-value-bind (exp flag) (%expand-form (first body))
             (if (atom exp)
               (values (%expand-tagbody (rest body)) t) ; weglassen
               (multiple-value-call #'%expand-cons body
                   exp flag
                   (%expand-tagbody (rest body))
) )     )  ) ) )
; (%expand-fundefs-1 fundefs) liefert eine Liste (name1 nil ... namek nil *fenv*)
(defun %expand-fundefs-1 (fundefs)
  (if (atom fundefs)
    (if fundefs
      (error #+DEUTSCH "FLET/LABELS: Dotted list im Code, endet mit ~S"
             #+ENGLISH "FLET/LABELS: code contains a dotted list, ending with ~S"
             #+FRANCAIS "FLET/LABELS : une paire pointée dans le code, terminée par ~S"
             fundefs
      )
      (list *fenv*)
    )
    (let ((fundef (car fundefs)))
      (if (and (consp fundef) (symbolp (car fundef)) (consp (cdr fundef)))
        (list* (car fundef) nil (%expand-fundefs-1 (cdr fundefs)))
        (error #+DEUTSCH "Falsche Syntax in FLET/LABELS: ~S"
               #+ENGLISH "illegal syntax in FLET/LABELS: ~S"
               #+FRANCAIS "syntaxe incorrecte dans FLET/LABELS : ~S"
               fundef
) ) ) ) )
; (%expand-fundefs-2 fundefs) expandiert eine Funktionsdefinitionenliste,
; wie in FLET, LABELS. 2 Werte.
(defun %expand-fundefs-2 (fundefs)
  (if (atom fundefs)
    (values fundefs nil)
    (let ((fundef (car fundefs)))
      (multiple-value-call #'%expand-cons fundefs
             (multiple-value-call #'%expand-cons fundef
                     (car fundef) nil
                     (%expand-lambdabody (cdr fundef))
             )
             (%expand-fundefs-2 (rest fundefs))
) ) ) )

#|
; expandiert eine Form in einem gegebenen Function-Environment
; Kann bei Bedarf von EVAL aufgerufen werden.
(defun %expand-form-main (form *fenv*)
  (%expand-form form)
)
|#

; expandiert (lambdalist . body) in einem gegebenen Function-Environment.
; Wird von GET_CLOSURE aufgerufen.
(defun %expand-lambdabody-main (lambdabody *fenv*)
  (%expand-lambdabody lambdabody)
)

(VALUES) )

;; ab hier ist FUNCTION funktionsfähig, soweit kein MACROLET darin vorkommt.

(PROGN

(proclaim '(special *load-paths*))
(setq *load-paths* nil)

; vorläufig brauchen die Files nicht gesucht zu werden:
(defun search-file (filename extensions)
  (mapcan #'(lambda (extension)
              (let ((filename (merge-pathnames filename extension)))
                (if (probe-file filename) (list filename) '())
            ) )
          (reverse extensions)
) )

(proclaim '(special *load-verbose*))
(setq *load-verbose* t)
(proclaim '(special *load-print*))
(setq *load-print* nil)
(proclaim '(special *load-echo*))
(setq *load-echo* nil)

; (LOAD filename [:verbose] [:print] [:if-does-not-exist] [:echo] [:compiling]),
; CLTL S. 426
(fmakunbound 'load)
(defun load (filename
             &key (verbose *load-verbose*) (print *load-print*) (if-does-not-exist t)
                  (echo *load-echo*) (compiling nil))
  (let ((stream
          (if (streamp filename)
            filename
            (or (open (setq filename (pathname filename))
                  :direction :input
                  :element-type 'string-char
                  :if-does-not-exist nil
                )
                ; Datei mit genau diesem Namen nicht vorhanden.
                ; Suche unter den Dateien mit demselben Namen und den
                ; Extensions "LSP", "FAS" die neueste:
                (let ((present-files
                        (search-file filename '(#".lsp" #".fas"))
                     ))
                  (if (endp present-files)
                    nil
                    (open (setq filename (first present-files))
                          :direction :input :element-type 'string-char
       )) ) )   ) ) )
    (if stream
      (let ((input-stream
              (if echo
                (make-echo-stream stream *standard-output*)
                stream
            ) )
            ; :verbose, :print und :echo wirken nicht rekursiv - dazu
            ; hat man ja gerade die Special-Variablen *load-verbose* etc.
            ;(*load-verbose* verbose)
            ;(*load-print* print)
            ;(*load-echo* echo)
            (*package* *package*) ; *PACKAGE* binden
            (*readtable* *readtable*) ; *READTABLE* binden
            (end-of-file "EOF")) ; einmaliges Objekt
        (when verbose
          (fresh-line)
          (write-string #+DEUTSCH ";; Datei "
                        #+ENGLISH ";; Loading file "
                        #+FRANCAIS ";; Chargement du fichier "
          )
          (princ filename)
          (write-string #+DEUTSCH " wird geladen..."
                        #+ENGLISH " ..."
                        #+FRANCAIS " ..."
        ) )
        (block nil
          (unwind-protect
            (tagbody weiter
              (when echo (fresh-line))
              (let ((obj (read input-stream nil end-of-file)))
                (when (eql obj end-of-file) (return-from nil))
                (setq obj
                  (cond ((compiled-function-p obj) (funcall obj))
                        (compiling (funcall (compile-form obj nil nil nil nil nil)))
                        (t (eval obj))
                ) )
                (when print (print obj))
              )
              (go weiter)
            )
            (close stream) (close input-stream)
        ) )
        (when verbose
          (fresh-line)
          (write-string #+DEUTSCH ";; Datei "
                        #+ENGLISH ";; Loading of file "
                        #+FRANCAIS ";; Le fichier "
          )
          (princ filename)
          (write-string #+DEUTSCH " ist geladen."
                        #+ENGLISH " is finished."
                        #+FRANCAIS " est chargé."
        ) )
        t
      )
      (if if-does-not-exist
        (error #+DEUTSCH "Ein Datei mit Namen ~A gibt es nicht."
               #+ENGLISH "A file with name ~A does not exist"
               #+FRANCAIS "Il n'existe pas de fichier de nom ~A."
               filename
        )
        nil
      )
) ) )

; vorläufig:
(sys::%putd 'defun
  (cons 'sys::macro
    (function defun
      (lambda (form env)
        (unless (and (consp (cdr form)) (consp (cddr form)))
          (error #+DEUTSCH "~S: Funktionsname und/oder Parameterliste fehlt"
                 #+ENGLISH "~S: missing function name and/or parameter list"
                 #+FRANCAIS "~S : Le nom de fonction et/ou la liste de paramètre manque"
                 'defun
        ) )
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          (unless (symbolp name)
            (error #+DEUTSCH "~S: ~S ist kein Symbol."
                   #+ENGLISH "~S: ~S is not a symbol."
                   #+FRANCAIS "~S : ~S n'est pas un symbole."
                   'defun name
          ) )
          (when (special-form-p name)
            (error #+DEUTSCH "~S: Spezialform ~S kann nicht umdefiniert werden."
                   #+ENGLISH "~S: special form ~S cannot be redefined."
                   #+FRANCAIS "~S : La forme spéciale ~S ne peut pas être redéfinie."
                   'defun name
          ) )
          (multiple-value-bind (body-rest declarations docstring)
                               (sys::parse-body body t env)
            (declare (ignore docstring))
            (if declarations
              (setq declarations (list (cons 'DECLARE declarations)))
            )
            #|
            `(PROGN
               (SYS::%PUT ',name 'SYS::DEFINITION ',form)
               (SYS::%PUTD ',name
                 (LABELS ((,name ,lambdalist ,@declarations
                            (BLOCK ,name ,@body-rest)
                         ))
                   (FUNCTION ,name)
               ) )
               ',name
             )
            |#
            (list 'progn
              (list 'sys::%put (list 'quote name) ''sys::definition
                    (list 'quote form)
              )
              (list 'sys::%putd (list 'quote name)
                (list 'labels
                  (list
                    (list* name lambdalist
                      (append declarations
                        (list (list* 'BLOCK name body-rest))
                  ) ) )
                  (list 'function name)
              ) )
              (list 'quote name)
            )
    ) ) ) )
) )

; vorläufige Definition des Macros DO :
(sys::%putd 'do
  (cons 'sys::macro
    (function do
      (lambda (form env)
        (let ((varclauselist (second form))
              (exitclause (third form))
              (body (cdddr form)))
          (when (atom exitclause)
            (error #+DEUTSCH "Exitclause in ~S muß Liste sein."
                   #+ENGLISH "exit clause in ~S must be a list"
                   #+FRANCAIS "La clause de sortie dans ~S doit être une liste."
                   'do
          ) )
          (let ((bindlist nil)
                (reinitlist nil)
                (bodytag (gensym))
                (exittag (gensym)))
            (multiple-value-bind (body-rest declarations)
                                 (sys::parse-body body nil env)
              (block do
                (tagbody 1
                  (when (atom varclauselist)
                    (return-from do
                      #|
                      `(block nil
                         (let ,(nreverse bindlist)
                           (declare ,@declarations)
                           (tagbody
                             (go ,exittag)
                             ,bodytag
                             ,@body-rest
                             (psetq ,@(nreverse reinitlist))
                             ,exittag
                             (or ,(first exitclause) (go ,bodytag))
                             (return-from nil (progn ,@(rest exitclause)))
                       ) ) )
                      |#
                      (list 'block 'nil
                        (list 'let (nreverse bindlist)
                          (cons 'declare declarations)
                          (list* 'tagbody
                            (list 'go exittag)
                            bodytag
                            (append body-rest
                              (list
                                (cons 'psetq (nreverse reinitlist))
                                exittag
                                (list 'or (first exitclause) (list 'go bodytag))
                                (list 'return-from 'nil
                                  (cons 'progn (rest exitclause))
                      ) ) ) ) ) )
                  ) )
                  (let ( (varclause (first varclauselist)) )
                       (setq varclauselist (rest varclauselist))
                       (cond ( (atom varclause)
                                  (setq bindlist
                                        (cons varclause bindlist)) )
                             ( (atom (cdr varclause))
                                  (setq bindlist
                                        (cons (first varclause) bindlist)) )
                             ( (atom (cddr varclause))
                                  (setq bindlist
                                        (cons varclause bindlist)) )
                             ( t (setq bindlist
                                       (cons (list (first varclause)
                                                   (second varclause))
                                             bindlist))
                                 (setq reinitlist
                                       (list* (third varclause)
                                              (first varclause)
                                              reinitlist)) )))
                  (go 1)
    ) ) ) ) ) ) )
) )

; vorläufige Definition des Macros DOTIMES :
(sys::%putd 'dotimes
  (cons 'sys::macro
    (function dotimes
      (lambda (form env)
        (let ((var (first (second form)))
              (countform (second (second form)))
              (resultform (third (second form)))
              (body (cddr form)))
          (multiple-value-bind (body-rest declarations)
                               (sys::parse-body body nil env)
            (let ((g (gensym)))
              #|
              `(DO ((,var 0 (1+ ,var))
                    (,g ,countform))
                   ((>= ,var ,g) ,resultform)
                 (declare ,@declarations)
                 ,@body-rest
               )
              |#
              (list* 'do (list (list var '0 (list '1+ var)) (list g countform))
                         (list (list '>= var g) resultform)
                     (cons 'declare declarations)
                     body-rest
              )
    ) ) ) ) )
) )

(VALUES) )

;; ab hier sind LOAD, DEFUN, DO, DOTIMES (eingeschränkt) funktionsfähig.

(LOAD "defseq")   ;; Definitionen von Standard-Sequences

(LOAD "backquot") ;; Backquote-Readmacro

(PROGN

(sys::%putd 'sys::backquote
  (cons 'sys::macro
    (function sys::backquote
      (lambda (form &optional env) (declare (ignore env)) (third form))
) ) )

(VALUES) )

;; ab hier ist Backquote funktionsfähig

(LOAD "defmacro")

;; ab hier ist FUNCTION (uneingeschränkt) funktionsfähig.

(PROGN

(sys::%putd 'defmacro
  (cons 'sys::macro
    (function defmacro
      (lambda (form &optional env)
        (declare (ignore env))
        (multiple-value-bind (expansion name lambdalist docstring)
                             (sys::make-macro-expansion (cdr form))
          (declare (ignore lambdalist))
          `(PROGN
             (EVAL-WHEN (COMPILE LOAD EVAL)
               (SYSTEM::REMOVE-OLD-DEFINITIONS ',name)
               ,@(if docstring
                   `((SYSTEM::%SET-DOCUMENTATION ',name 'FUNCTION ',docstring))
                   '()
                 )
               (SYSTEM::%PUTD ',name (CONS 'SYSTEM::MACRO ,expansion))
             )
             (EVAL-WHEN (EVAL) (SYSTEM::%PUT ',name 'SYSTEM::DEFINITION ',form))
             ',name
           )
    ) ) )
) )

(sys::%putd 'defun
  (cons 'sys::macro
    (function defun
      (lambda (form env)
        (if (atom (cdr form))
          (error #+DEUTSCH "~S: Daraus kann keine Funktion definiert werden: ~S"
                 #+ENGLISH "~S: cannot define a function from that: ~S"
                 #+FRANCAIS "~S : Pas de définition de fonction possible à partir de: ~S"
                 'defun (cdr form)
        ) )
        (unless (symbolp (cadr form))
          (error #+DEUTSCH "~S: Der Name einer Funktion muß ein Symbol sein, nicht: ~S"
                 #+ENGLISH "~S: the name of a function must be a symbol, not ~S"
                 #+FRANCAIS "~S : Le nom d'une fonction doit être un symbole et non ~S"
                 'defun (cadr form)
        ) )
        (if (atom (cddr form))
          (error #+DEUTSCH "~S: Die Funktion ~S hat keine Lambdaliste."
                 #+ENGLISH "~S: function ~S is missing a lambda list"
                 #+FRANCAIS "~S : Il manque une liste lambda à la fonction ~S."
                 'defun (cadr form)
        ) )
        (let ((name (cadr form))
              (lambdalist (caddr form))
              (body (cdddr form)))
          (multiple-value-bind (body-rest declarations docstring)
                               (sys::parse-body body t env)
            (if declarations
              (setq declarations (list (cons 'DECLARE declarations)))
            )
            (let ((lambdabody
                    `(,lambdalist ,@declarations (BLOCK ,name ,@body-rest))
                 ))
              `(PROGN
                 (EVAL-WHEN (COMPILE)
                   (COMPILER::C-DEFUN ',name
                     ,@(if (and compiler::*compiling*
                                compiler::*compiling-from-file*
                                (member name compiler::*inline-functions* :test #'eq)
                                (null compiler::*venv*)
                                (null compiler::*fenv*)
                                (null compiler::*benv*)
                                (null compiler::*genv*)
                                (eql compiler::*denv* *toplevel-denv*)
                           )
                         ; Lambdabody für Inline-Compilation aufheben:
                         `(',lambdabody)
                         '()
                       )
                 ) )
                 (SYSTEM::REMOVE-OLD-DEFINITIONS ',name)
                 ,@(if docstring
                     `((SYSTEM::%SET-DOCUMENTATION ',name 'FUNCTION ',docstring))
                     '()
                   )
                 (SYSTEM::%PUTD ',name
                   (LABELS ((,name ,@lambdabody))
                     (FUNCTION ,name)
                 ) )
                 (EVAL-WHEN (EVAL) (SYSTEM::%PUT ',name 'SYSTEM::DEFINITION ',form))
                 ',name
               )
    ) ) ) ) )
) )

(VALUES) )

;; ab hier sind DEFMACRO und DEFUN funktionsfähig.

; (MACRO-EXPANDER . macrodef)                                         [Macro]
; expandiert zum Macro-Expander als Programmtext (FUNCTION ... (LAMBDA ...)).
(defmacro MACRO-EXPANDER (&body macrodef)
  (make-macro-expansion macrodef)
)

(LOAD "macros1")  ;; Kontrollstrukturen - Macros
(LOAD "macros2")  ;; weitere Macros

(LOAD "defs1")    ;; Definitionen zu Symbolen, Zahlen, Characters, Zeit

#+CLISP1 (LOAD "array") ;; Hilfsfunktionen für Arrays

(LOAD "places")   ;; SETF-Places: Definitionen und Macros

;; ab hier ist SETF u.ä. funktionsfähig.

(LOAD "floatpri") ;; Ausgabe von Floating-Points

(LOAD "type")     ;; TYPEP

(LOAD "defstruc") ;; DEFSTRUCT-Macro

(LOAD "format")   ;; FORMAT

; Ein Stückchen "DO-WHAT-I-MEAN":
; Sucht ein Programm-File.
; Gesucht wird im aktuellen Directory und dann in den Directories
; aus *load-paths*.
; Ist eine Extension angegeben, so wird nur nach Files mit genau dieser
; Extension gesucht. Ist keine Extension angegeben, so wird nur nach Files
; mit einer Extension aus der gegebenen Liste gesucht.
; Man erhält alle Files aus dem ersten passenden Directory, als Pathnames,
; in einer Liste, nach fallendem FILE-WRITE-DATE sortiert, oder NIL.
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
                              :test #-(or AMIGA OS/2) #'string=
                                    #+(or AMIGA OS/2) #'string-equal
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

(LOAD "user1")    ;; User-Interface, Teil 1: Break-Loop, Stepper

(LOAD "user2")    ;; User-Interface, Teil 2: Apropos, Describe, Dribble, Ed

(LOAD "trace")    ;; User-Interface, Teil 3: TRACE

;(LOAD "macros3")  ;; weitere Macros, optional

(LOAD "config")   ;; Konfigurations-Parameter

(LOAD "compiler") ;; Compiler

#+AMIGA (LOAD "rexx") ;; Rexx-Schnittstelle, optional

#+ATARI
(when (y-or-n-p #+DEUTSCH "Editor laden?"
                #+ENGLISH "Load editor?"
                #+FRANCAIS "Charger l'éditeur?"
      )
  (LOAD "editor") ;; Editor
)

(in-package "USER") ;; Default-Package aktuell machen

