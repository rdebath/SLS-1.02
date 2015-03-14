;;;; Definitionen für Kontrollstrukturen etc.
;;;; 29. 4. 1988, 3. 9. 1988

(in-package "LISP")
(export '(mapcap maplap))
(in-package "SYSTEM")

(defmacro defvar (symbol &optional (initial-value nil svar) docstring)
  (unless (symbolp symbol)
    (error #+DEUTSCH "~S: Nur Symbole können Variablen sein, nicht ~S"
           #+ENGLISH "~S: non-symbol ~S can't be a variable"
           #+FRANCAIS "~S : Seuls les symboles peuvent servir de variable et non ~S"
           'defvar symbol
  ) )
  (if (constantp symbol)
    (error #+DEUTSCH "~S: Die Konstante ~S darf nicht zu einer Variablen umdefiniert werden."
           #+ENGLISH "~S: the constant ~S must not be redefined to be a variable"
           #+FRANCAIS "~S : La constante ~S ne peut pas être redéfinie en variable."
           'defvar symbol
  ) )
  `(PROGN
     (PROCLAIM '(SPECIAL ,symbol))
     ,@(if svar
         `((UNLESS (BOUNDP ',symbol) (SET ',symbol ,initial-value)))
       )
     ,@(if docstring `((SYS::%SET-DOCUMENTATION ',symbol 'VARIABLE ',docstring)))
     ',symbol
   )
)

(defmacro defparameter (symbol initial-value &optional docstring)
  (unless (symbolp symbol)
    (error #+DEUTSCH "~S: Nur Symbole können Variablen sein, nicht ~S"
           #+ENGLISH "~S: non-symbol ~S can't be a variable"
           #+FRANCAIS "~S : Seuls les symboles peuvent servir de variable et non ~S."
           'defparameter symbol
  ) )
  (if (constantp symbol)
    (error #+DEUTSCH "~S: Die Konstante ~S darf nicht zu einer Variablen umdefiniert werden."
           #+ENGLISH "~S: the constant ~S must not be redefined to be a variable"
           #+FRANCAIS "~S : La constante ~S ne peut pas être redéfinie en variable."
           'defparameter symbol
  ) )
  `(PROGN
     (PROCLAIM '(SPECIAL ,symbol))
     (SET ',symbol ,initial-value)
     ,@(if docstring `((SYS::%SET-DOCUMENTATION ',symbol 'VARIABLE ',docstring)))
     ',symbol
   )
)

(defmacro defconstant (&whole form symbol initial-value &optional docstring)
  (unless (symbolp symbol)
    (error #+DEUTSCH "~S: Nur Symbole können als Konstanten definiert werden, nicht ~S"
           #+ENGLISH "~S: non-symbol ~S can't be a defined constant"
           #+FRANCAIS "~S : Seuls les symboles peuvent servir de constante et non ~S."
           'defconstant symbol
  ) )
  `(PROGN
     (EVAL-WHEN (COMPILE)
       (COMPILER::C-PROCLAIM-CONSTANT ',symbol ',initial-value)
     )
     (IF (CONSTANTP ',symbol)
       (WARN #+DEUTSCH "In ~S wird die Konstante ~S umdefiniert. Ihr alter Wert war ~S."
             #+ENGLISH "~S redefines the constant ~S. Its old value was ~S."
             #+FRANCAIS "~S redéfinit la constante ~S. Son ancienne valeur était ~S."
             ',form ',symbol (SYMBOL-VALUE ',symbol)
     ) )
     (SYS::%PROCLAIM-CONSTANT ',symbol ,initial-value)
     ,@(if docstring `((SYS::%SET-DOCUMENTATION ',symbol 'VARIABLE ',docstring)))
     ',symbol
   )
)

(sys::%put 'and 'sys::macro
  (sys::macro-expander and (&body args)
    (cond ((null args) T)
          ((null (cdr args)) (car args))
          (t (let ((L (mapcar #'(lambda (x) `((NOT ,x) NIL) ) args)))
               (rplaca (last L) `(T ,(car (last args))))
               (cons 'COND L)
  ) )     )  )
)

(sys::%put 'or 'sys::macro
  (sys::macro-expander or (&body args)
    (cond ((null args) NIL)
          ((null (cdr args)) (car args))
          (t (let ((L (mapcar #'list args)))
               (rplaca (last L) `(T ,(car (last args))))
               (cons 'COND L)
  ) )     )  )
)

(sys::%put 'prog1 'sys::macro
  (sys::macro-expander prog1 (form1 &rest moreforms)
    (let ((g (gensym)))
      `(LET ((,g ,form1)) ,@moreforms ,g)
  ) )
)

(sys::%put 'prog2 'sys::macro
  (sys::macro-expander prog2 (form1 form2 &rest moreforms)
    (let ((g (gensym)))
      `(PROGN ,form1 (LET ((,g ,form2)) ,@moreforms ,g))
  ) )
)

(sys::%put 'when 'sys::macro
  (sys::macro-expander when (test &body forms)
    `(IF ,test (PROGN ,@forms))
  )
)

(sys::%put 'unless 'sys::macro
  (sys::macro-expander unless (test &body forms)
    `(IF (NOT ,test) (PROGN ,@forms))
  )
)

(defmacro return (&optional return-value)
  `(RETURN-FROM NIL ,return-value)
)

(defmacro loop (&body body)
  (let ((tag (gensym)))
    `(BLOCK NIL (TAGBODY ,tag ,@body (GO ,tag)))
) )

(defun do/do*-expand (varclauselist exitclause body env do let psetq)
  (when (atom exitclause)
    (error #+DEUTSCH "Exitclause in ~S muß Liste sein."
           #+ENGLISH "exit clause in ~S must be a list"
           #+FRANCAIS "La clause de sortie dans ~S doit être une liste."
           do
  ) )
  (let ((bindlist nil)
        (reinitlist nil)
        (testtag (gensym))
        (exittag (gensym)))
    (multiple-value-bind (body-rest declarations doc)
                         (sys::parse-body body nil env)
      (declare (ignore doc))
      (if declarations
        (setq declarations (list (cons 'DECLARE declarations)))
      )
      (loop
        (when (atom varclauselist) (return))
        (let ((varclause (first varclauselist)))
          (setq varclauselist (rest varclauselist))
          (cond ((atom varclause)
                 (setq bindlist (cons varclause bindlist))
                )
                ((atom (cdr varclause))
                 (setq bindlist (cons (first varclause) bindlist))
                )
                ((atom (cddr varclause))
                 (setq bindlist (cons varclause bindlist))
                )
                (t (setq bindlist
                     (cons (list (first varclause) (second varclause))
                           bindlist
                   ) )
                   (setq reinitlist
                     (list* (third varclause) (first varclause) reinitlist)
      ) ) )     )  )
      `(BLOCK NIL
         (,let ,(nreverse bindlist)
           ,@declarations
           (TAGBODY
             ,testtag
             (IF ,(first exitclause) (GO ,exittag))
             ,@body-rest
             (,psetq ,@(nreverse reinitlist))
             (GO ,testtag)
             ,exittag
             (RETURN-FROM NIL (PROGN ,@(rest exitclause)))
       ) ) )
) ) )

(fmakunbound 'do)
(defmacro do (varclauselist exitclause &body body &environment env)
  (do/do*-expand varclauselist exitclause body env 'DO 'LET 'PSETQ)
)

(defmacro do* (varclauselist exitclause &body body &environment env)
  (do/do*-expand varclauselist exitclause body env 'DO* 'LET* 'SETQ)
)

(defmacro dolist ((var listform &optional resultform) &body body &environment env)
  (multiple-value-bind (body-rest declarations)
                       (sys::parse-body body nil env)
    (let ((g (gensym)))
      `(DO* ((,g ,listform (CDR ,g))
             (,var NIL))
            ((ENDP ,g)
             ,(if (constantp resultform)
               ; Ist resultform konstant, so ist es /= var. Daher braucht var
               ; während Auswertung von resultform nicht an NIL gebunden zu sein:
               `,resultform
               `(LET ((,var NIL))
                  ,@(if declarations (list (cons 'DECLARE declarations)))
                  ,var ; var wird nur zum Schein ausgewertet
                  ,resultform
                )
              )
            )
         (DECLARE (LIST ,g) ,@declarations)
         (SETQ ,var (CAR ,g))
         ,@body-rest
       )
) ) )

(fmakunbound 'dotimes)
(defmacro dotimes ((var countform &optional resultform) &body body &environment env)
  (multiple-value-bind (body-rest declarations)
                       (sys::parse-body body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations)))
    )
    (if (constantp countform)
      `(DO ((,var 0 (1+ ,var)))
           ((>= ,var ,countform) ,resultform)
         ,@declarations
         ,@body-rest
       )
      (let ((g (gensym)))
        `(DO ((,var 0 (1+ ,var))
              (,g ,countform))
             ((>= ,var ,g) ,resultform)
           ,@declarations
           ,@body-rest
) ) ) )  )

(sys::%put 'psetq 'sys::macro
  (sys::macro-expander psetq (&whole form &rest args)
    (do* ((setlist nil)
          (bindlist nil)
          (arglist args (cddr arglist)))
         ((null arglist)
          (setq setlist (cons 'NIL setlist))
          (cons 'LET (cons (nreverse bindlist) (nreverse setlist)))
         )
      (if (null (cdr arglist))
        (error #+DEUTSCH "~S mit einer ungeraden Anzahl von Argumenten aufgerufen: ~S"
               #+ENGLISH "~S called with an odd number of arguments: ~S"
               #+FRANCAIS "~S fut appellé avec un nombre impair d'arguments : ~S"
               'psetq form
      ) )
      (let ((g (gensym)))
        (setq setlist (cons `(SETQ ,(first arglist) ,g) setlist))
        (setq bindlist (cons `(,g ,(second arglist)) bindlist))
  ) ) )
)

(sys::%put 'multiple-value-list 'sys::macro
  (sys::macro-expander multiple-value-list (form)
    `(MULTIPLE-VALUE-CALL #'LIST ,form)
  )
)

(sys::%put 'multiple-value-bind 'sys::macro
  (sys::macro-expander multiple-value-bind (varlist form &body body)
    (let ((g (gensym))
          (poplist nil))
      (dolist (var varlist) (setq poplist (cons `(,var (POP ,g)) poplist)))
      `(LET* ((,g (MULTIPLE-VALUE-LIST ,form)) ,@(nreverse poplist))
         ,@body
  ) )  )
)

(sys::%put 'multiple-value-setq 'sys::macro
  (sys::macro-expander multiple-value-setq (varlist form)
    (let ((g (gensym))
          (poplist nil))
      (dolist (var varlist) (setq poplist (cons `(SETQ ,var (POP ,g)) poplist)))
      `(LET* ((,g (MULTIPLE-VALUE-LIST ,form)))
         ,(if poplist `(PROG1 ,(nreverse poplist)) NIL)
  ) )  )
)

(defmacro locally (&body body)
  `(LET () ,@body)
)

(defmacro case (keyform &body body)
           ;; Common LISP, S. 117
  (let ((var (gensym)))
    `(LET ((,var ,keyform))
       (COND
         ,@(mapcar
             #'(lambda (cl)
                 (unless (consp cl)
                   (error #+DEUTSCH "~S: Keylist fehlt."
                          #+ENGLISH "~S: missing key list"
                          #+FRANCAIS "~S : la liste d'objects-clé manque."
                          'case
                 ) )
                 (let ((kl (first cl)))
                   `(,(cond ((or (eq kl 'T) (eq kl 'OTHERWISE)) 'T)
                            ((listp kl) `(MEMBER ,var ',kl))
                            (t `(EQL ,var ',kl))
                      )
                     ,@(rest cl)
               ) )  )
             body
) )  ) )   )

(defmacro prog (varlist &body body &environment env)
  (multiple-value-bind (body-rest declarations)
                       (sys::parse-body body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations)))
    )
    `(BLOCK NIL
       (LET ,varlist
         ,@declarations
         (TAGBODY ,@body-rest)
) )  ) )

(defmacro prog* (varlist &body body &environment env)
  (multiple-value-bind (body-rest declarations)
                       (sys::parse-body body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations)))
    )
    `(BLOCK NIL
       (LET* ,varlist
         ,@declarations
         (TAGBODY ,@body-rest)
) )  ) )


;;; Macro-Expander für COND:

#|
;; Dieser hier ist zwar kürzer, aber er reduziert COND auf OR,
;; das seinerseits wieder auf COND reduziert, ...
(sys::%put 'cond 'sys::macro
  (sys::macro-expander cond (&body clauses)
    (ifify clauses)
  )
)
; macht eine clauselist von COND zu verschachtelten IFs und ORs.
(defun ifify (clauselist)
  (cond ((null clauselist) NIL)
        ((atom clauselist)
         (error #+DEUTSCH "Das ist keine Liste von COND-Klauseln: ~S"
                #+ENGLISH "Not a list of COND clauses: ~S"
                #+FRANCAIS "Ceci n'est pas une liste de clauses COND : ~S"
                clauselist
        ))
        ((atom (car clauselist))
         (error #+DEUTSCH "Das ist ein Atom und daher nicht als COND-Klausel verwendbar: ~S"
                #+ENGLISH "The atom ~S must not be used as a COND clause."
                #+FRANCAIS "Ceci est une atome et n'est donc pas utilisable comme clause COND : ~S"
                (car clauselist)
        ))
        (t (let ((ifif (ifify (cdr clauselist))))
             (if (cdar clauselist)
               ; mindestens zweielementige Klausel
               (if (constantp (caar clauselist))
                 (if (eval (caar clauselist)) ; Test zur Expansionszeit auswerten
                   (if (cddar clauselist)
                     `(PROGN ,@(cdar clauselist))
                     (cadar clauselist)
                   )
                   ifif
                 )
                 `(IF ,(caar clauselist)
                    ,(if (cddar clauselist) `(PROGN ,@(cdar clauselist)) (cadar clauselist))
                    ,ifif
                  )
               )
               ; einelementige Klausel
               (if (constantp (caar clauselist))
                 (if (eval (caar clauselist)) ; Test zur Expansionszeit auswerten
                   (caar clauselist)
                   ifif
                 )
                 `(OR ,(caar clauselist) ,ifif)
) )     )  ) ) )
|#

;; Noch einfacher ginge es auch so:
#|
(sys::%put 'cond 'sys::macro
  (sys::macro-expander cond (&body clauses)
    (cond ((null clauses) 'NIL)
          ((atom clauses)
           (error #+DEUTSCH "Dotted List im Code von COND, endet mit ~S"
                  #+ENGLISH "COND code contains a dotted list, ending with ~S"
                  #+FRANCAIS "Occurence d'une paire pointée dans le code de COND, terminée en : ~S."
                  clauses
          ))
          (t (let ((clause (car clauses)))
               (if (atom clause)
                 (error #+DEUTSCH "COND-Klausel ohne Test: ~S"
                        #+ENGLISH "COND clause without test: ~S"
                        #+FRANCAIS "Clause COND sans aucun test : ~S"
                        clause
                 )
                 (let ((test (car clause)))
                   (if (cdr clause)
                     `(IF ,test (PROGN ,@(cdr clause)) (COND ,@(cdr clauses)))
                     `(OR ,test (COND ,@(cdr clauses)))
) ) )     )  ) ) ) )
|#

;; Dieser hier reduziert COND etwas umständlicher auf IF-Folgen:
(sys::%put 'cond 'sys::macro
  (sys::macro-expander cond (&body clauses)
    (let ((g (gensym)))
      (multiple-value-bind (ifif needed-g) (ifify clauses g)
        (if needed-g
          `(LET (,g) ,ifif)
          ifif
  ) ) ) )
)
; macht eine clauselist von COND zu verschachtelten IFs.
; Zwei Werte: die neue Form, und ob die Dummyvariable g benutzt wurde.
(defun ifify (clauselist g)
  (cond ((null clauselist) (values NIL nil))
        ((atom clauselist)
         (error #+DEUTSCH "Das ist keine Liste von COND-Klauseln: ~S"
                #+ENGLISH "Not a list of COND clauses: ~S"
                #+FRANCAIS "Ceci n'est pas une liste de clauses COND : ~S"
                clauselist
        ))
        ((atom (car clauselist))
         (error #+DEUTSCH "Das ist ein Atom und daher nicht als COND-Klausel verwendbar: ~S"
                #+ENGLISH "The atom ~S must not be used as a COND clause."
                #+FRANCAIS "Ceci est une atome et n'est donc pas utilisable comme clause COND : ~S"
                (car clauselist)
        ))
        (t (multiple-value-bind (ifif needed-g) (ifify (cdr clauselist) g)
             (if (cdar clauselist)
               ; mindestens zweielementige Klausel
               (if (constantp (caar clauselist))
                 (if (eval (caar clauselist)) ; Test zur Expansionszeit auswerten
                   (if (cddar clauselist)
                     (values `(PROGN ,@(cdar clauselist)) nil)
                     (values (cadar clauselist) nil)
                   )
                   (values ifif needed-g)
                 )
                 (values
                   `(IF ,(caar clauselist)
                        ,(if (cddar clauselist) `(PROGN ,@(cdar clauselist)) (cadar clauselist))
                        ,ifif
                    )
                   needed-g
               ) )
               ; einelementige Klausel
               (if (constantp (caar clauselist))
                 (if (eval (caar clauselist)) ; Test zur Expansionszeit auswerten
                   (values (caar clauselist) nil)
                   (values ifif needed-g)
                 )
                 (if (atom (caar clauselist))
                   (values ; ein Atom produziert nur einen Wert und darf
                     `(IF ,(caar clauselist) ; mehrfach hintereinander
                          ,(caar clauselist) ; ausgewertet werden!
                          ,ifif
                      )
                     needed-g
                   )
                   (values
                     `(IF (SETQ ,g ,(caar clauselist)) ,g ,ifif)
                     t
) )     )  ) ) ) ) )

;;; Mapping (Kapitel 7.8.4)

; Hilfsfunktion: mapcan, aber mit append statt nconc:
; (mapcap fun &rest lists) ==  (apply #'append (apply #'mapcar fun lists))
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
  (sys::list-nreverse L)
)

; Hilfsfunktion: mapcon, aber mit append statt nconc:
; (maplap fun &rest lists) == (apply #'append (apply #'maplist fun lists))
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
  (sys::list-nreverse L)
)

