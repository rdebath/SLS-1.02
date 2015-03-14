;;;; TYPEP und Verwandtes
;;;; Michael Stoll, 21. 10. 1988
;;;; Bruno Haible, 10.6.1989

;;; Datenstrukturen für TYPEP:
;;; - Ein Type-Specifier-Symbol hat auf seiner Propertyliste unter dem
;;;   Indikator SYS::TYPE-SYMBOL eine Funktion von einem Argument, die
;;;   testet, ob ein Objekt vom richtigen Typ ist.
;;; - Ein Symbol, das eine Type-Specifier-Liste beginnen kann, hat auf seiner
;;;   Propertyliste unter dem Indikator SYS::TYPE-LIST eine Funktion von
;;;   einem Argument für das zu testende Objekt und zusätzlichen Argumenten
;;;   für die Listenelemente.
;;; - Ein Symbol, das als Typmacro definiert wurde, hat auf seiner Property-
;;;   liste unter dem Indikator SYSTEM::DEFTYPE-EXPANDER den zugehörigen
;;;   Expander: eine Funktion, die den zu expandierenden Type-Specifier (eine
;;;   mindestens einelementige Liste) als Argument bekommt.

(in-package "SYSTEM")

(defun type-error (fun type)
  (error #+DEUTSCH "~S: ~S ist keine zugelassene Typspezifikation."
         #+ENGLISH "~S: invalid type specification ~S"
         #+FRANCAIS "~S : ~S n'est pas une spécification de type légale."
         fun type
) )

;;; TYPEP, CLTL S. 72, S. 42-51
(defun typep (x y &aux f) ; x = Objekt, y = Typ
  (cond
    ((symbolp y)
       (cond ((setq f (get y 'TYPE-SYMBOL)) (funcall f x))
             ((setq f (get y 'TYPE-LIST)) (funcall f x))
             ((setq f (get y 'DEFTYPE-EXPANDER)) (typep x (funcall f (list y))))
             ((get y 'DEFSTRUCT-DESCRIPTION) (%STRUCTURE-TYPE-P y x))
             (t (type-error 'typep y))
    )  )
    ((and (consp y) (symbolp (first y)))
       (cond
         ((and (eq (first y) 'SATISFIES) (eql (length y) 2))
            (unless (symbolp (second y))
              (error #+DEUTSCH "~S: Argument zu SATISFIES muß Symbol sein: ~S"
                     #+ENGLISH "~S: argument to SATISFIES must be a symbol: ~S"
                     #+FRANCAIS "~S : L'argument de SATISFIES doit être un symbole: ~S"
                     'typep (second y)
            ) )
            (if (funcall (symbol-function (second y)) x) t nil)
         )
         ((eq (first y) 'MEMBER)
            (if (member x (rest y)) t nil)
         )
         ((and (eq (first y) 'NOT) (eql (length y) 2))
            (not (typep x (second y)))
         )
         ((eq (first y) 'AND)
            (dolist (type (rest y) t)
              (unless (typep x type) (return nil))
         )  )
         ((eq (first y) 'OR)
            (dolist (type (rest y) nil)
              (when (typep x type) (return t))
         )  )
         ((setq f (get (first y) 'TYPE-LIST)) (apply f x (rest y)))
         ((setq f (get (first y) 'DEFTYPE-EXPANDER)) (typep x (funcall f y)))
         (t (type-error 'typep y))
    )  )
    (t (type-error 'typep y))
) )

; CLTL S. 43
(%put 'ARRAY 'TYPE-SYMBOL #'arrayp)
(%put 'ATOM 'TYPE-SYMBOL #'atom)
(%put 'BIGNUM 'TYPE-SYMBOL
  (function type-symbol-bignum
    (lambda (x) (and (integerp x) (not (fixnump x))))
) )
(%put 'BIT 'TYPE-SYMBOL
  (function type-symbol-bit
    (lambda (x) (or (eql x 0) (eql x 1)))
) )
(%put 'BIT-VECTOR 'TYPE-SYMBOL #'bit-vector-p)
(%put 'CHARACTER 'TYPE-SYMBOL #'characterp)
(%put 'COMMON 'TYPE-SYMBOL #'commonp)
(%put 'COMPILED-FUNCTION 'TYPE-SYMBOL #'compiled-function-p)
(%put 'COMPLEX 'TYPE-SYMBOL #'complexp)
(%put 'CONS 'TYPE-SYMBOL #'consp)
(%put 'DOUBLE-FLOAT 'TYPE-SYMBOL #'double-float-p)
(%put 'FIXNUM 'TYPE-SYMBOL #'fixnump)
(%put 'FLOAT 'TYPE-SYMBOL #'floatp)
(%put 'FUNCTION 'TYPE-SYMBOL #'functionp)
(%put 'HASH-TABLE 'TYPE-SYMBOL #'hash-table-p)
(%put 'INTEGER 'TYPE-SYMBOL #'integerp)
(%put 'KEYWORD 'TYPE-SYMBOL #'keywordp)
(%put 'LIST 'TYPE-SYMBOL #'listp)
(%put 'LONG-FLOAT 'TYPE-SYMBOL #'long-float-p)
(%put 'NIL 'TYPE-SYMBOL
  (function type-symbol-nil
    (lambda (x) (declare (ignore x)) nil)
) )
(%put 'NULL 'TYPE-SYMBOL #'null)
(%put 'NUMBER 'TYPE-SYMBOL #'numberp)
(%put 'PACKAGE 'TYPE-SYMBOL #'packagep)
(%put 'PATHNAME 'TYPE-SYMBOL #'pathnamep)
(%put 'RANDOM-STATE 'TYPE-SYMBOL #'random-state-p)
(%put 'RATIO 'TYPE-SYMBOL
  (function type-symbol-ratio
    (lambda (x) (and (rationalp x) (not (integerp x))))
) )
(%put 'RATIONAL 'TYPE-SYMBOL #'rationalp)
(%put 'READTABLE 'TYPE-SYMBOL #'readtablep)
(%put 'REAL 'TYPE-SYMBOL #'realp)
(%put 'SEQUENCE 'TYPE-SYMBOL #'sequencep)
(%put 'SHORT-FLOAT 'TYPE-SYMBOL #'short-float-p)
(%put 'SIMPLE-ARRAY 'TYPE-SYMBOL #'simple-array-p)
(%put 'SIMPLE-BIT-VECTOR 'TYPE-SYMBOL #'simple-bit-vector-p)
(%put 'SIMPLE-STRING 'TYPE-SYMBOL #'simple-string-p)
(%put 'SIMPLE-VECTOR 'TYPE-SYMBOL #'simple-vector-p)
(%put 'SINGLE-FLOAT 'TYPE-SYMBOL #'single-float-p)
(%put 'STANDARD-CHAR 'TYPE-SYMBOL
  (function type-symbol-standard-char
    (lambda (x) (and (characterp x) (standard-char-p x)))
) )
(%put 'STREAM 'TYPE-SYMBOL #'streamp)
(%put 'STRING 'TYPE-SYMBOL #'stringp)
(%put 'STRING-CHAR 'TYPE-SYMBOL
  (function type-symbol-string-char
    (lambda (x) (and (characterp x) (string-char-p x)))
) )
(%put 'STRUCTURE 'TYPE-SYMBOL
  (function type-symbol-structure
    (lambda (x)
      (let ((y (type-of x)))
        (and (symbolp y) (get y 'DEFSTRUCT-DESCRIPTION)
             (%STRUCTURE-TYPE-P y x)
) ) ) ) )
(%put 'SYMBOL 'TYPE-SYMBOL #'symbolp)
(%put 'T 'TYPE-SYMBOL
  (function type-symbol-t
    (lambda (x) (declare (ignore x)) t)
) )
(%put 'VECTOR 'TYPE-SYMBOL #'vectorp)

; CLTL S. 46-50
(defun upgraded-array-element-type (type)
  #+CLISP1 ; siehe ARRAY.Q
  (case type
    ((STRING-CHAR BIT) type)
    (t 'T)
  )
  #-CLISP1 ; siehe ARRAY.D
  (case type
    ((BIT STRING-CHAR T) type)
    (t (multiple-value-bind (low high) (sys::subtype-integer type)
         ; Es gilt (or (null low) (subtypep type `(INTEGER ,low ,high))
         (if (and (integerp low) (not (minusp low)) (integerp high))
           (let ((l (integer-length high)))
             ; Es gilt (subtypep type `(UNSIGNED-BYTE ,l))
             (cond ((<= l 1) 'BIT)
                   ((<= l 2) '(UNSIGNED-BYTE 2))
                   ((<= l 4) '(UNSIGNED-BYTE 4))
                   ((<= l 8) '(UNSIGNED-BYTE 8))
                   ((<= l 16) '(UNSIGNED-BYTE 16))
                   ((<= l 32) '(UNSIGNED-BYTE 32))
                   (t 'T)
           ) )
           'T
  ) )  ) )
)
(%put 'ARRAY 'TYPE-LIST
  (function type-list-array
    (lambda (x &optional (el-type '*) (dims '*))
      (and (arrayp x)
           (or (eq el-type '*)
               (equal (array-element-type x) (upgraded-array-element-type el-type))
           )
           (or (eq dims '*)
               (if (numberp dims)
                 (eql dims (array-rank x))
                 (and (eql (length dims) (array-rank x))
                      (every #'(lambda (a b) (or (eq a '*) (eql a b)))
                             dims (array-dimensions x)
  ) ) )    )   ) )    )
)
(%put 'SIMPLE-ARRAY 'TYPE-LIST
  (function type-list-simple-array
    (lambda (x &optional (el-type '*) (dims '*))
      (and (simple-array-p x)
           (or (eq el-type '*)
               (equal (array-element-type x) (upgraded-array-element-type el-type))
           )
           (or (eq dims '*)
               (if (numberp dims)
                 (eql dims (array-rank x))
                 (and (eql (length dims) (array-rank x))
                      (every #'(lambda (a b) (or (eq a '*) (eql a b)))
                             dims (array-dimensions x)
  ) ) )    )   ) )    )
)
(%put 'VECTOR 'TYPE-LIST
  (function type-list-vector
    (lambda (x &optional (el-type '*) (size '*))
      (and (vectorp x)
           (or (eq el-type '*)
               (equal (array-element-type x) (upgraded-array-element-type el-type))
           )
           (or (eq size '*) (eql (array-dimension x 0) size))
  ) ) )
)
(%put 'SIMPLE-VECTOR 'TYPE-LIST
  (function type-list-simple-vector
    (lambda (x &optional (size '*))
      (and (simple-vector-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'COMPLEX 'TYPE-LIST
  (function type-list-complex
    (lambda (x &optional (rtype '*) (itype rtype))
      (and (complexp x)
           (or (eq rtype '*) (typep (realpart x) rtype))
           (or (eq itype '*) (typep (imagpart x) itype))
  ) ) )
)
(%put 'INTEGER 'TYPE-LIST
  (function type-list-integer
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'integerp 'INTEGER)
  ) )
)
(defun typep-number-test (x low high test type)
  (and (funcall test x)
       (cond ((eq low '*))
             ((funcall test low) (<= low x))
             ((and (consp low) (null (rest low)) (funcall test (first low)))
                (< (first low) x)
             )
             (t (error #+DEUTSCH "~S: Argument zu ~S muß *, ~S oder eine Liste von ~S sein: ~S"
                       #+ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S"
                       #+FRANCAIS "~S : L'argument de ~S doit être *, ~S ou une liste de ~S: ~S"
                       'typep type type type low
       )     )  )
       (cond ((eq high '*))
             ((funcall test high) (>= high x))
             ((and (consp high) (null (rest high)) (funcall test (first high)))
                (> (first high) x)
             )
             (t (error #+DEUTSCH "~S: Argument zu ~S muß *, ~S oder eine Liste von ~S sein: ~S"
                       #+ENGLISH "~S: argument to ~S must be *, ~S or a list of ~S: ~S"
                       #+FRANCAIS "~S : L'argument de ~S doit être *, ~S ou une liste de ~S: ~S"
                       'typep type type type high
) )    )     )  )
(%put 'MOD 'TYPE-LIST
  (function type-list-mod
    (lambda (x n)
      (unless (integerp n)
        (error #+DEUTSCH "~S: Argument zu MOD muß ganze Zahl sein: ~S"
               #+ENGLISH "~S: argument to MOD must be an integer: ~S"
               #+FRANCAIS "~S : L'argument de MOD doit être un entier: ~S"
               'typep n
      ) )
      (and (integerp x) (<= 0 x) (< x n))
  ) )
)
(%put 'SIGNED-BYTE 'TYPE-LIST
  (function type-list-signed-byte
    (lambda (x &optional (n '*))
      (unless (or (eq n '*) (integerp n))
        (error #+DEUTSCH "~S: Argument zu SIGNED-BYTE muß ganze Zahl oder * sein: ~S"
               #+ENGLISH "~S: argument to SIGNED-BYTE must be an integer or * : ~S"
               #+FRANCAIS "~S : L'argument de SIGNED-BYTE doit être un entier ou bien * : ~S"
               'typep n
      ) )
      (and (integerp x) (or (eq n '*) (< (integer-length x) n)))
  ) )
)
(%put 'UNSIGNED-BYTE 'TYPE-LIST
  (function type-list-unsigned-byte
    (lambda (x &optional (n '*))
      (unless (or (eq n '*) (integerp n))
        (error #+DEUTSCH "~S: Argument zu UNSIGNED-BYTE muß ganze Zahl oder * sein: ~S"
               #+ENGLISH "~S: argument to UNSIGNED-BYTE must be an integer or * : ~S"
               #+FRANCAIS "~S : L'argument de UNSIGNED-BYTE doit être un entier ou bien * : ~S"
               'typep n
      ) )
      (and (integerp x)
           (not (minusp x))
           (or (eq n '*) (<= (integer-length x) n))
  ) ) )
)
(%put 'REAL 'TYPE-LIST
  (function type-list-real
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'realp 'REAL)
  ) )
)
(%put 'RATIONAL 'TYPE-LIST
  (function type-list-rational
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'rationalp 'RATIONAL)
  ) )
)
(%put 'FLOAT 'TYPE-LIST
  (function type-list-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'floatp 'FLOAT)
  ) )
)
(%put 'SHORT-FLOAT 'TYPE-LIST
  (function type-list-short-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'short-float-p 'SHORT-FLOAT)
  ) )
)
(%put 'SINGLE-FLOAT 'TYPE-LIST
  (function type-list-single-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'single-float-p 'SINGLE-FLOAT)
  ) )
)
(%put 'DOUBLE-FLOAT 'TYPE-LIST
  (function type-list-double-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'double-float-p 'DOUBLE-FLOAT)
  ) )
)
(%put 'LONG-FLOAT 'TYPE-LIST
  (function type-list-long-float
    (lambda (x &optional (low '*) (high '*))
      (typep-number-test x low high #'long-float-p 'LONG-FLOAT)
  ) )
)
(%put 'STRING 'TYPE-LIST
  (function type-list-string
    (lambda (x &optional (size '*))
      (and (stringp x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'SIMPLE-STRING 'TYPE-LIST
  (function type-list-simple-string
    (lambda (x &optional (size '*))
      (and (simple-string-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'BIT-VECTOR 'TYPE-LIST
  (function type-list-bit-vector
    (lambda (x &optional (size '*))
      (and (bit-vector-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)
(%put 'SIMPLE-BIT-VECTOR 'TYPE-LIST
  (function type-list-simple-bit-vector
    (lambda (x &optional (size '*))
      (and (simple-bit-vector-p x)
           (or (eq size '*) (eql size (array-dimension x 0)))
  ) ) )
)

; Testet eine Liste von Werten auf Erfüllen eines Type-Specifiers. Für THE.
(defun %the (values type)
  (if (and (consp type) (eq (car type) 'VALUES))
    (macrolet ((type-error ()
                 '(error #+DEUTSCH "Falsch aufgebauter Type-Specifier: ~S"
                         #+ENGLISH "Invalid type specifier ~S"
                         #+FRANCAIS "Spécificateur de type mal formé : ~S"
                         type
              ))  )
      (let ((vals values)
            (types (cdr type)))
        ; required-Werte:
        (loop
          (when (or (atom types) (member (car types) lambda-list-keywords :test #'eq))
            (return)
          )
          (unless (and (consp vals) (typep (car vals) (car types)))
            (return-from %the nil)
          )
          (setq vals (cdr vals))
          (setq types (cdr types))
        )
        ; optionale Werte:
        (when (and (consp types) (eq (car types) '&optional))
          (setq types (cdr types))
          (loop
            (when (or (atom types) (member (car types) lambda-list-keywords :test #'eq))
              (return)
            )
            (when (consp vals)
              (unless (typep (car vals) (car types)) (return-from %the nil))
              (setq vals (cdr vals))
            )
            (setq types (cdr types))
        ) )
        ; restliche Werte:
        (if (atom types)
          (when (consp vals) (return-from %the nil))
          (case (car types)
            (&rest
              (setq types (cdr types))
              (when (atom types) (type-error))
              (unless (typep vals (car types)) (return-from %the nil))
              (setq types (cdr types))
            )
            (&key)
            (t (type-error))
        ) )
        ; Keyword-Werte:
        (when (consp types)
          (if (eq (car types) '&key)
            (progn
              (setq types (cdr types))
              (when (oddp (length vals)) (return-from %the nil))
              (let ((keywords nil))
                (loop
                  (when (or (atom types) (member (car types) lambda-list-keywords :test #'eq))
                    (return)
                  )
                  (let ((item (car types)))
                    (unless (and (listp item) (eql (length item) 2) (symbolp (first item)))
                      (type-error)
                    )
                    (let ((kw (intern (symbol-name (first item)) *keyword-package*)))
                      (unless (typep (getf vals kw) (second item))
                        (return-from %the nil)
                      )
                      (push kw keywords)
                  ) )
                  (setq types (cdr types))
                )
                (if (and (consp types) (eq (car types) '&allow-other-keys))
                  (setq types (cdr types))
                  (unless (getf vals ':allow-other-keys)
                    (do ((L vals (cddr L)))
                        ((atom L))
                      (unless (member (car L) keywords :test #'eq)
                        (return-from %the nil)
                ) ) ) )
            ) )
            (when (consp types) (type-error))
        ) )
        t
    ) )
    (typep (if (consp values) (car values) nil) type) ; 1. Wert abtesten
) )

;;; SUBTYPEP, vorläufige Version
(defun canonicalize-type (type) ; type ein wenig vereinfachen, nicht rekursiv
  (cond ((symbolp type)
         (let ((f (get type 'DEFTYPE-EXPANDER)))
           (if f
             (canonicalize-type (funcall f (list type))) ; macroexpandieren
             (case type
               (ATOM '(NOT CONS))
               (BIGNUM '(AND INTEGER (NOT FIXNUM)))
               (BIT '(INTEGER 0 1))
               (COMMON '(OR CONS SYMBOL NUMBER ARRAY STANDARD-CHAR
                         STREAM PACKAGE HASH-TABLE READTABLE PATHNAME RANDOM-STATE
                         STRUCTURE
               )        )
               (FIXNUM `(INTEGER ,most-negative-fixnum ,most-positive-fixnum))
               (KEYWORD '(AND SYMBOL (SATISFIES KEYWORDP)))
               (LIST '(OR CONS (MEMBER NIL)))
               ((NIL) '(OR))
               (NULL '(MEMBER NIL))
               (RATIO '(AND RATIONAL (NOT INTEGER)))
               (SEQUENCE '(OR LIST VECTOR)) ; user-defined sequences??
               (STANDARD-CHAR '(AND CHARACTER (SATISFIES STRING-CHAR-P) (SATISFIES STANDARD-CHAR-P)))
               (STRING-CHAR '(AND CHARACTER (SATISFIES STRING-CHAR-P)))
               ((T) '(AND))
               ((ARRAY SIMPLE-ARRAY BIT-VECTOR SIMPLE-BIT-VECTOR
                 STRING SIMPLE-STRING VECTOR SIMPLE-VECTOR
                 COMPLEX REAL INTEGER RATIONAL FLOAT
                 SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT
                )
                 (canonicalize-type (list type))
               )
               (t type)
        )) ) )
        ((and (consp type) (symbolp (first type)))
         (let ((f (get (first type) 'DEFTYPE-EXPANDER)))
           (if f
             (canonicalize-type (funcall f type)) ; macroexpandieren
             (case (first type)
               (MEMBER ; (MEMBER &rest objects)
                 (if (null (rest type)) '(OR) type)
               )
               (MOD ; (MOD n)
                 (let ((n (second type)))
                   (unless (and (integerp n) (>= n 0)) (type-error 'subtypep type))
                   `(INTEGER 0 (,n))
               ) )
               (SIGNED-BYTE ; (SIGNED-BYTE &optional s)
                 (let ((s (or (second type) '*)))
                   (if (eq s '*)
                     'INTEGER
                     (progn
                       (unless (and (integerp s) (plusp s)) (type-error 'subtypep type))
                       (let ((n (expt 2 (1- s))))
                         `(INTEGER ,(- n) (,n))
               ) ) ) ) )
               (UNSIGNED-BYTE ; (UNSIGNED-BYTE &optional s)
                 (let ((s (or (second type) '*)))
                   (if (eq s '*)
                     '(INTEGER 0 *)
                     (progn
                       (unless (and (integerp s) (>= s 0)) (type-error 'subtypep type))
                       (let ((n (expt 2 s)))
                         `(INTEGER 0 (,n))
               ) ) ) ) )
               (SIMPLE-BIT-VECTOR ; (SIMPLE-BIT-VECTOR &optional size)
                 (let ((size (or (second type) '*)))
                   `(SIMPLE-ARRAY BIT (,size))
               ) )
               (SIMPLE-STRING ; (SIMPLE-STRING &optional size)
                 (let ((size (or (second type) '*)))
                   `(SIMPLE-ARRAY STRING-CHAR (,size))
               ) )
               (SIMPLE-VECTOR ; (SIMPLE-VECTOR &optional size)
                 (let ((size (or (second type) '*)))
                   `(SIMPLE-ARRAY T (,size))
               ) )
               (BIT-VECTOR ; (BIT-VECTOR &optional size)
                 (let ((size (or (second type) '*)))
                   `(ARRAY BIT (,size))
               ) )
               (STRING ; (STRING &optional size)
                 (let ((size (or (second type) '*)))
                   `(ARRAY STRING-CHAR (,size))
               ) )
               (VECTOR ; (VECTOR &optional el-type size)
                 (let ((el-type (or (second type) '*))
                       (size (or (third type) '*)))
                   `(ARRAY ,el-type (,size))
               ) )
               (t type)
        )) ) )
) )
(defun subtypep (type1 type2)
  (macrolet ((yes () '(return-from subtypep (values t t)))
             (no () '(return-from subtypep (values nil t)))
             (unknown () '(return-from subtypep (values nil nil))))
    (setq type1 (canonicalize-type type1))
    (setq type2 (canonicalize-type type2))
    (when (equal type1 type2) (yes)) ; (subtypep type type) stimmt immer
    (when (consp type1)
      (cond ;; über SATISFIES-Typen kann man nichts aussagen
            ;((and (eq (first type1) 'SATISFIES) (eql (length type1) 2))
            ; (unknown)
            ;)
            ;; MEMBER: alle Elemente müssen vom Typ type2 sein
            ((eq (first type1) 'MEMBER)
             (dolist (x (rest type1) (yes))
               (unless (typep x type2) (return (no)))
            ))
            ;; NOT: (subtypep `(NOT ,type1) `(NOT ,type2)) ist äquivalent
            ;; zu (subtypep type2 type1), sonst ist Entscheidung schwierig
            ((and (eq (first type1) 'NOT) (eql (length type1) 2))
             (return-from subtypep
               (if (and (consp type2) (eq (first type2) 'NOT) (eql (length type2) 2))
                 (subtypep (second type2) (second type1))
                 (unknown)
            )) )
            ;; OR: Jeder Typ muß Subtyp von type2 sein
            ((eq (first type1) 'OR)
             (dolist (type (rest type1) (yes))
               (multiple-value-bind (is known) (subtypep type type2)
                 (unless is (return-from subtypep (values nil known)))
            )) )
    ) )
    (when (consp type2)
      (cond ;; über SATISFIES-Typen kann man nichts aussagen
            ;((and (eq (first type2) 'SATISFIES) (eql (length type2) 2))
            ; (unknown)
            ;)
            ;; NOT: siehe oben
            ((and (eq (first type2) 'NOT) (eql (length type2) 2))
             (unknown)
            )
            ;; AND: type1 muß Subtyp jedes der Typen sein
            ((eq (first type2) 'AND)
             (dolist (type (rest type2) (yes))
               (multiple-value-bind (is known) (subtypep type1 type)
                 (unless is (return-from subtypep (values nil known)))
            )) )
            ;; OR: Falls type1 Subtyp eines der Typen ist, sonst nicht bekannt
            ((eq (first type2) 'OR)
             (dolist (type (rest type2) (unknown))
               (when (subtypep type1 type) (return (yes)))
            ))
    ) )
    (when (consp type1)
      (cond ;; AND: Falls ein Typ Subtyp von type2 ist, sonst nicht bekannt
            ((eq (first type1) 'AND)
             (dolist (type (rest type1) (unknown))
               (when (subtypep type type2) (return (yes)))
            ))
    ) )
    (when (and (symbolp type1) (get type1 'DEFSTRUCT-DESCRIPTION)
               (symbolp type2)
          )
      (when (eq type2 'STRUCTURE) (yes))
      (when (get type2 'DEFSTRUCT-DESCRIPTION)
        (let ((inclist1 (svref (get type1 'DEFSTRUCT-DESCRIPTION) 0))
              (inclist2 (svref (get type2 'DEFSTRUCT-DESCRIPTION) 0)))
          (loop
            (when (eq inclist1 inclist2) (return (yes)))
            (when (atom inclist1) (return))
            (setq inclist1 (cdr inclist1))
      ) ) )
    )
    (when (atom type1) (setq type1 (list type1)))
    (case (first type1)
      ((ARRAY SIMPLE-ARRAY)
        (macrolet ((array-p (type)
                     `(or (eq ,type 'ARRAY) (eq ,type (first type1)))
                  ))
          (let ((el-type1 (if (rest type1) (second type1) '*))
                (dims1 (if (cddr type1) (third type1) '*)))
            (values
              (cond ((array-p type2) t)
                    ((and (consp type2) (array-p (first type2)))
                     (let ((el-type2 (if (rest type2) (second type2) '*))
                           (dims2 (if (cddr type2) (third type2) '*)))
                       (and (or (eq el-type2 '*)
                                (and (not (eq el-type1 '*))
                                     (equal (upgraded-array-element-type el-type1)
                                            (upgraded-array-element-type el-type2)
                            )   )    )
                            (or (eq dims2 '*)
                                (and (listp dims1) (listp dims2)
                                     (eql (length dims1) (length dims2))
                                     (every #'(lambda (a b) (or (eq b '*) (= a b)))
                                              dims1 dims2
                    )) )    )   )    )
                    (t nil)
              )
              t
      ) ) ) )
      (COMPLEX
        (let* ((rtype1 (if (rest type1) (second type1) '*))
               (itype1 (if (cddr type1) (third type1) rtype1)))
          (values
            (cond ((or (eq type2 'COMPLEX) (eq type2 'NUMBER)) t)
                  ((and (consp type2) (eq (first type2) 'COMPLEX))
                   (let* ((rtype2 (if (rest type2) (second type2) '*))
                          (itype2 (if (cddr type2) (third type2) rtype2)))
                     (and (or (eq rtype2 '*)
                              (and (not (eq rtype1 '*))
                                   (subtypep rtype1 rtype2)
                          )   )
                          (or (eq itype2 '*)
                              (and (not (eq itype1 '*))
                                   (subtypep itype1 itype2)
                  )) )    )   )
                  (t nil)
            )
            t
      ) ) )
      ((REAL INTEGER RATIONAL FLOAT SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
        (let ((typelist
                (cons (first type1)
                  (case (first type1)
                    (REAL '(NUMBER))
                    (INTEGER '(RATIONAL REAL NUMBER))
                    ((RATIONAL FLOAT) '(REAL NUMBER))
                    ((SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT) '(FLOAT REAL NUMBER))
              ) ) )
              (low1 (if (rest type1) (second type1) '*))
              (high1 (if (cddr type1) (third type1) '*))
              (integer-flag1 (eq (first type1) 'INTEGER))
              (efl t)
              (efh t))
          (when (consp low1)
            (setq low1 (first low1))
            (if integer-flag1 (when (numberp low1) (incf low1)) (setq efl nil))
          )
          (when (consp high1)
            (setq high1 (first high1))
            (if integer-flag1 (when (numberp high1) (decf high1)) (setq efh nil))
          )
          ; efl gibt an, ob low1 zu type1 dazugehört.
          ; efh gibt an, ob high1 zu type1 dazugehört.
          (cond ((and (numberp low1) (numberp high1)
                      (not (or (< low1 high1) (and (= low1 high1) efl efh)))
                 ) ; type1 leer?
                 (yes)
                )
                ((member type2 typelist) (yes))
                ((and (consp type2) (member (first type2) typelist))
                 (let ((low2 (if (rest type2) (second type2) '*))
                       (high2 (if (cddr type2) (third type2) '*))
                       (integer-flag2 (eq (first type2) 'INTEGER)))
                   (if (consp low2)
                     (progn (setq low2 (first low2))
                            (when integer-flag2 (when (numberp low2) (incf low2)) (setq efl nil))
                     )
                     (setq efl nil)
                   )
                   (if (consp high2)
                     (progn (setq high2 (first high2))
                            (when integer-flag2 (when (numberp high2) (decf high2)) (setq efh nil))
                     )
                     (setq efh nil)
                   )
                   ; efl gibt an, ob low1 zu type1 dazugehört und low2 zu type2 nicht dazugehört.
                   ; efh gibt an, ob high1 zu type1 dazugehört und high2 zu type2 nicht dazugehört.
                   (values
                     (and (or (eq low2 '*)
                              (and (numberp low1)
                                   (if efl (> low1 low2) (>= low1 low2))
                          )   )
                          (or (eq high2 '*)
                              (and (numberp high1)
                                   (if efh (< high1 high2) (<= high1 high2))
                     )    )   )
                     t
                )) )
                (t (values nil (not integer-flag1)))
      ) ) )
      (t (unknown))
) ) )

;; Bestimmt zwei Werte low,high so, daß (subtypep type `(INTEGER ,low ,high))
;; gilt und low möglichst groß und high möglichst klein ist.
;; low = * bedeutet -unendlich, high = * bedeutet unendlich.
;; Werte sind NIL,NIL falls (subtypep type 'INTEGER) falsch ist.
;; Wir brauchen diese Funktion nur für MAKE-ARRAY und UPGRADED-ARRAY-ELEMENT-TYPE,
;; dürfen also oBdA  type  durch  `(OR ,type (MEMBER 0))  ersetzen.
(defun subtype-integer (type)
  (macrolet ((yes () '(return-from subtype-integer (values low high)))
             (no () '(return-from subtype-integer nil))
             (unknown () '(return-from subtype-integer nil)))
    (setq type (canonicalize-type type))
    (if (consp type)
      (macrolet ((min* (x y) `(if (or (eq ,x '*) (eq ,y '*)) '* (min ,x ,y)))
                 (max* (x y) `(if (or (eq ,x '*) (eq ,y '*)) '* (max ,x ,y))))
        (case (first type)
          (MEMBER ;; MEMBER: alle Elemente müssen vom Typ INTEGER sein
            (let ((low 0) (high 0)) ; oBdA!
              (dolist (x (rest type) (yes))
                (unless (typep x 'INTEGER) (return (no)))
                (setq low (min low x) high (max high x))
          ) ) )
          (OR ;; OR: Jeder Typ muß Subtyp von INTEGER sein
            (let ((low 0) (high 0)) ; oBdA!
              (dolist (type1 (rest type) (yes))
                (multiple-value-bind (low1 high1) (subtype-integer type1)
                  (unless low1 (return (no)))
                  (setq low (min* low low1) high (max* high high1))
          ) ) ) )
          (AND ;; AND: Falls ein Typ Subtyp von INTEGER ist, sonst nicht bekannt
            ;; Hier könnte man die verschiedenen Integer-Subtypen schneiden.
            (dolist (type1 (rest type) (unknown))
              (multiple-value-bind (low high) (subtype-integer type1)
                (when low (return (yes)))
          ) ) )
      ) )
      (setq type (list type))
    )
    (if (eq (first type) 'INTEGER)
      (let ((low (if (rest type) (second type) '*))
            (high (if (cddr type) (third type) '*)))
        (when (consp low)
          (setq low (first low))
          (when (numberp low) (incf low))
        )
        (when (consp high)
          (setq high (first high))
          (when (numberp high) (decf high))
        )
        (when (and (numberp low) (numberp high) (not (<= low high))) ; type leer?
          (setq low 0 high 0)
        )
        (yes)
      )
      (unknown)
) ) )
