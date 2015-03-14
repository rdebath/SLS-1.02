; CLISP - PLACES.LSP
; CLISP-spezifisch: string-concat, %rplaca, %rplacd, store, %setelt, ...

(in-package "SYSTEM")
;-------------------------------------------------------------------------------
; Funktionen zur Definition und zum Ausnutzen von places:
;-------------------------------------------------------------------------------
(defun get-setf-method-multiple-value (form &optional (env nil))
  (do* ((newformbackup nil newform)
        (newform form (macroexpand-1 newform env)))
       ((eq newformbackup newform)
        (error #+DEUTSCH "Das Argument muß eine 'SETF-place' sein, ist aber keine: ~S"
               #+ENGLISH "Argument ~S is not a SETF place."
               #+FRANCAIS "L'argument ~S doit représenter une place modifiable."
               newform
       ))
    (when (symbolp newform)
      (let ((storevar (gensym)))
        (return (values nil
                        nil
                        `(,storevar)
                        `(SETQ ,newform ,storevar)
                        `,newform
    ) ) )       )
    (when (and (consp newform) (symbolp (car newform)))
      (let ((plist-info (get (first newform) 'SYSTEM::SETF-EXPANDER)))
        (when plist-info
          (if (symbolp plist-info) ; Symbol kommt von kurzem DEFSETF
            (return
              (do* ((storevar (gensym))
                    (tempvars nil (cons (gensym) tempvars))
                    (tempforms nil)
                    (formr (cdr newform) (cdr formr)))
                   ((atom formr)
                    (setq tempforms (nreverse tempforms))
                    (values tempvars
                            tempforms
                            `(,storevar)
                            `(,plist-info ,@tempvars ,storevar)
                            `(,(first newform) ,@tempvars)
                   ))
                (setq tempforms (cons (car formr) tempforms))
            ) )
            (let ((argcount (car plist-info)))
              (if (eql argcount -5)
                (return ; (-5 . fun) kommt von DEFINE-SETF-METHOD
                  (funcall (cdr plist-info) newform env)
                )
                (return ; (argcount . fun) kommt von langem DEFSETF
                  (let ((access-form newform)
                        (tempvars '())
                        (tempforms '())
                        (new-access-form '()))
                    (let ((i 0)) ; Argumente-Zähler
                      ; argcount = -1 falls keine Keyword-Argumente existieren
                      ; bzw.     = Anzahl der einzelnen Argumente vor &KEY,
                      ;          = nil nachdem diese abgearbeitet sind.
                      (dolist (argform (cdr access-form))
                        (when (eql i argcount) (setf argcount nil i 0))
                        (if (and (null argcount) (evenp i))
                          (if (keywordp argform)
                            (push argform new-access-form)
                            (error #+DEUTSCH "Das Argument ~S zu ~S sollte ein Keyword sein."
                                   #+ENGLISH "The argument ~S to ~S should be a keyword."
                                   #+FRANCAIS "L'argument ~S de ~S doit être un mot-clé."
                                   argform (car access-form)
                          ) )
                          (let ((tempvar (gensym)))
                            (push tempvar tempvars)
                            (push argform tempforms)
                            (push tempvar new-access-form)
                        ) )
                        (incf i)
                    ) )
                    (setq new-access-form
                      (cons (car access-form) (nreverse new-access-form))
                    )
                    (let ((newval-var (gensym)))
                      (values
                        (nreverse tempvars)
                        (nreverse tempforms)
                        (list newval-var)
                        (funcall (cdr plist-info) new-access-form newval-var)
                        new-access-form
            ) ) ) ) ) )
    ) ) ) )
) )
;-------------------------------------------------------------------------------
(defun get-setf-method (form &optional (env nil))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form env)
    (unless (and (consp stores) (null (cdr stores)))
      (error #+DEUTSCH "Diese 'SETF-place' produziert mehrere 'Store-Variable': ~S"
             #+ENGLISH "SETF place ~S produces more than one store variable."
             #+FRANCAIS "La place modifiable ~S produit plusieurs variables de résultat."
             form
    ) )
    (values vars vals stores store-form access-form)
) )
;-------------------------------------------------------------------------------
(defun documentation (symbol doctype)
  (unless (symbolp symbol)
    (error #+DEUTSCH "~S: Das ist als erstes Argument unzulässig, da kein Symbol: ~S"
           #+ENGLISH "~S: first argument ~S is illegal, not a symbol"
           #+FRANCAIS "~S : Le premier argument ~S est invalide car ce n'est pas un symbole."
           'documentation symbol
  ) )
  (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
)
(defun SYSTEM::%SET-DOCUMENTATION (symbol doctype value)
  (unless (symbolp symbol)
    (error #+DEUTSCH "~S: Das ist als erstes Argument unzulässig, da kein Symbol: ~S"
           #+ENGLISH "~S: first argument ~S is illegal, not a symbol"
           #+FRANCAIS "~S : Le premier argument ~S est invalide car ce n'est pas un symbole."
           'documentation symbol
  ) )
  (if (null value)
    (when (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
      (remf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
    )
    (setf (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype) value)
) )
;-------------------------------------------------------------------------------
(defmacro push (item place &environment env)
  (if (symbolp place)
    `(SETQ ,place (CONS ,item ,place))
    (let ((itemvar (gensym)))
      (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
        (do* ((SM1r SM1 (cdr SM1r))
              (SM2r SM2 (cdr SM2r))
              (bindlist `((,itemvar ,item)) ))
             ((null SM1r)
              (push `(,(first SM3) (CONS ,itemvar ,SM5)) bindlist)
              `(LET* ,(nreverse bindlist)
                 ,SM4
             ) )
          (push `(,(first SM1r) ,(first SM2r)) bindlist)
) ) ) ) )
;-------------------------------------------------------------------------------
(defmacro define-setf-method (accessfn lambdalist &body body &environment env)
  (unless (symbolp accessfn)
    (error #+DEUTSCH "Der Name der Access-Function muß ein Symbol sein und nicht ~S."
           #+ENGLISH "The name of the access function must be a symbol, not ~S"
           #+FRANCAIS "Le nom de la fonction d'accès doit être un symbole et non ~S."
           accessfn
  ) )
  (multiple-value-bind (body-rest declarations docstring)
      (system::parse-body body t env)
    (if (null body-rest) (setq body-rest '(NIL)))
    (let ((name (make-symbol (string-concat "SETF-" (symbol-name accessfn)))))
      (multiple-value-bind (newlambdalist envvar) (remove-env-arg lambdalist name)
        (let ((SYSTEM::%ARG-COUNT 0)
              (SYSTEM::%MIN-ARGS 0)
              (SYSTEM::%RESTP nil)
              (SYSTEM::%LET-LIST nil)
              (SYSTEM::%KEYWORD-TESTS nil)
              (SYSTEM::%DEFAULT-FORM nil)
             )
          (SYSTEM::ANALYZE1 newlambdalist '(CDR SYSTEM::%LAMBDA-LIST)
                            name 'SYSTEM::%LAMBDA-LIST
          )
          (if (null newlambdalist)
            (push `(IGNORE SYSTEM::%LAMBDA-LIST) declarations)
          )
          (let ((lengthtest (sys::make-length-test 'SYSTEM::%LAMBDA-LIST))
                (mainform
                  `(LET* ,(nreverse SYSTEM::%LET-LIST)
                     ,@(if declarations `(,(cons 'DECLARE declarations)))
                     ,@SYSTEM::%KEYWORD-TESTS
                     ,@body-rest
                   )
               ))
            (if lengthtest
              (setq mainform
                `(IF ,lengthtest
                   (ERROR #+DEUTSCH "Der SETF-Expander für ~S kann nicht mit ~S Argumenten aufgerufen werden."
                          #+ENGLISH "The SETF expander for ~S may not be called with ~S arguments."
                          #+FRANCAIS "L'«expandeur» SETF pour ~S ne peut pas être appelé avec ~S arguments."
                          (QUOTE ,accessfn) (1- (LENGTH SYSTEM::%LAMBDA-LIST))
                   )
                   ,mainform
              )  )
            )
            `(EVAL-WHEN (LOAD COMPILE EVAL)
               (DEFUN ,name (SYSTEM::%LAMBDA-LIST ,(or envvar 'SYSTEM::ENV))
                 ,@(if envvar '() '((DECLARE (IGNORE SYSTEM::ENV))))
                 ,mainform
               )
               (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER
                 (CONS -5 (FUNCTION ,name))
               )
               (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF ',docstring)
               ',accessfn
             )
) ) ) ) ) )
;-------------------------------------------------------------------------------
(defmacro defsetf (accessfn &rest args &environment env)
  (cond ((and (consp args) (not (listp (first args))) (symbolp (first args)))
         `(EVAL-WHEN (LOAD COMPILE EVAL)
            (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER ',(first args))
            (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF
              ,(if (and (null (cddr args))
                        (or (null (second args)) (stringp (second args)))
                   )
                 (second args)
                 (if (cddr args)
                   (error #+DEUTSCH "Zu viele Argumente für DEFSETF: ~S"
                          #+ENGLISH "Too many arguments to DEFSETF: ~S"
                          #+FRANCAIS "Trop d'arguments pour DEFSETF : ~S"
                          (cdr args)
                   )
                   (error #+DEUTSCH "Der Dok.-String zu DEFSETF muß ein String sein: ~S"
                          #+ENGLISH "The doc string to DEFSETF must be a string: ~S"
                          #+FRANCAIS "La documentation pour DEFSETF doit être un chaîne : ~S"
                          (second args)
               ) ) )
            )
            ',accessfn
          )
        )
        ((and (consp args) (listp (first args)) (consp (cdr args)) (listp (second args)))
         (cond ((= (length (second args)) 1))
               ((= (length (second args)) 0)
                (error #+DEUTSCH "Bei DEFSETF muß genau eine 'Store-Variable' angegeben werden."
                       #+ENGLISH "Missing store variable in DEFSETF."
                       #+FRANCAIS "Une variable de résultat doit être précisée dans DEFSETF."
               ))
               (t (cerror #+DEUTSCH "Die überzähligen Variablen werden ignoriert."
                          #+ENGLISH "The excess variables will be ignored."
                          #+FRANCAIS "Les variables en excès seront ignorées."
                          #+DEUTSCH "Bei DEFSETF ist nur eine 'Store-Variable' erlaubt."
                          #+ENGLISH "Only one store variable is allowed in DEFSETF."
                          #+FRANCAIS "Une seule variable de résultat est permise dans DEFSETF."
         )     )  )
         (multiple-value-bind (body-rest declarations docstring)
             (system::parse-body (cddr args) t env)
           (let* (arg-count
                  (setter
                    (let* ((lambdalist (first args))
                           (storevar (first (second args)))
                           (SYSTEM::%ARG-COUNT 0)
                           (SYSTEM::%MIN-ARGS 0)
                           (SYSTEM::%RESTP nil)
                           (SYSTEM::%LET-LIST nil)
                           (SYSTEM::%KEYWORD-TESTS nil)
                           (SYSTEM::%DEFAULT-FORM nil))
                      (SYSTEM::ANALYZE1 lambdalist '(CDR SYSTEM::%ACCESS-ARGLIST)
                                        accessfn 'SYSTEM::%ACCESS-ARGLIST
                      )
                      (setq arg-count (if (member '&KEY lambdalist) SYSTEM::%ARG-COUNT -1))
                      `(LAMBDA (SYSTEM::%ACCESS-ARGLIST ,storevar)
                         ,@(if (null lambdalist)
                             `((DECLARE (IGNORE SYSTEM::%ACCESS-ARGLIST)))
                           )
                         (LET* ,(nreverse SYSTEM::%LET-LIST)
                           ,@(if declarations `(,(cons 'DECLARE declarations)))
                           ,@SYSTEM::%KEYWORD-TESTS
                           ,@body-rest
                       ) )
                 )) )
             `(EVAL-WHEN (LOAD COMPILE EVAL)
                (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER
                  (CONS ,arg-count
                        (FUNCTION ,(concat-pnames "SETF-" accessfn) ,setter)
                ) )
                (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF ,docstring)
                ',accessfn
              )
        )) )
        (t (error #+DEUTSCH "DEFSETF-Aufruf für ~S ist falsch aufgebaut."
                  #+ENGLISH "Illegal syntax in DEFSETF for ~S"
                  #+FRANCAIS "Le DEFSETF ~S est mal formé."
                  accessfn
) )     )  )
;-------------------------------------------------------------------------------
(defmacro pop (place &environment env)
  (if (symbolp place)
    `(PROG1 (CAR ,place) (SETQ ,place (CDR ,place)))
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
      (do* ((SM1r SM1 (cdr SM1r))
            (SM2r SM2 (cdr SM2r))
            (bindlist nil))
           ((null SM1r)
            (push `(,(first SM3) ,SM5) bindlist)
            `(LET* ,(nreverse bindlist)
               (PROG1
                 (CAR ,(first SM3))
                 (SETQ ,(first SM3) (CDR ,(first SM3)))
                 ,SM4
             ) )
           )
        (push `(,(first SM1r) ,(first SM2r)) bindlist)
) ) ) )
;-------------------------------------------------------------------------------
(defmacro psetf (&whole form &rest args &environment env)
  (do ((arglist args (cddr arglist))
       (bindlist nil)
       (storelist nil))
      ((atom arglist)
       `(LET* ,(nreverse bindlist)
          ,@storelist
          NIL
      ) )
    (when (atom (cdr arglist))
      (error #+DEUTSCH "~S mit einer ungeraden Zahl von Argumenten aufgerufen: ~S"
             #+ENGLISH "~S called with an odd number of arguments: ~S"
             #+FRANCAIS "~S fut appelé avec un nombre impair d'arguments : ~S"
             'psetf form
    ) )
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method (first arglist) env)
      (declare (ignore SM5))
      (do* ((SM1r SM1 (cdr SM1r))
            (SM2r SM2 (cdr SM2r)))
           ((null SM1r))
        (push `(,(first SM1r) ,(first SM2r)) bindlist)
      )
      (push `(,(first SM3) ,(second arglist)) bindlist)
      (push SM4 storelist)
) ) )
;-------------------------------------------------------------------------------
(defmacro pushnew (item place &rest keylist &environment env)
  (if (symbolp place)
    `(SETQ ,place (ADJOIN ,item ,place ,@keylist))
    (let ((itemvar (gensym)))
      (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
        (do* ((SM1r SM1 (cdr SM1r))
              (SM2r SM2 (cdr SM2r))
              (bindlist `((,itemvar ,item)) ))
             ((null SM1r)
              (push `(,(first SM3) (ADJOIN ,itemvar ,SM5 ,@keylist)) bindlist)
              `(LET* ,(nreverse bindlist)
                 ,SM4
             ) )
          (push `(,(first SM1r) ,(first SM2r)) bindlist)
) ) ) ) )
;-------------------------------------------------------------------------------
(defmacro remf (place indicator &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
    (do* ((SM1r SM1 (cdr SM1r))
          (SM2r SM2 (cdr SM2r))
          (bindlist nil)
          (indicatorvar (gensym))
          (var1 (gensym))
          (var2 (gensym)))
         ((null SM1r)
          (push `(,(first SM3) ,SM5) bindlist)
          (push `(,indicatorvar ,indicator) bindlist)
          `(LET* ,(nreverse bindlist)
             (DO ((,var1 ,(first SM3) (CDDR ,var1))
                  (,var2 NIL ,var1))
                 ((ATOM ,var1) NIL)
               (COND ((ATOM (CDR ,var1))
                      (ERROR #+DEUTSCH "REMF: Property-Liste ungerader Länge aufgetreten."
                             #+ENGLISH "REMF: property list with an odd length"
                             #+FRANCAIS "REMF : Occurence d'une liste de propriétés de longueur impaire."
                     ))
                     ((EQ (CAR ,var1) ,indicatorvar)
                      (IF ,var2
                        (RPLACD (CDR ,var2) (CDDR ,var1))
                        (PROGN (SETQ ,(first SM3) (CDDR ,(first SM3))) ,SM4)
                      )
                      (RETURN T)
           ) ) )     )
         )
      (push `(,(first SM1r) ,(first SM2r)) bindlist)
) ) )
;-------------------------------------------------------------------------------
(defmacro rotatef (&rest args &environment env)
  (cond ((null args) NIL)
        ((null (cdr args)) `(PROGN ,(car args) NIL) )
        (t (do* ((arglist args (cdr arglist))
                 (bindlist nil)
                 (storelist nil)
                 (lastvar nil)
                 (firstbind nil))
                ((atom arglist)
                 (setf (car firstbind) lastvar)
                 `(LET* ,(nreverse bindlist) ,@(nreverse storelist) NIL)
                )
             (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
                 (get-setf-method (first arglist) env)
               (do* ((SM1r SM1 (cdr SM1r))
                     (SM2r SM2 (cdr SM2r)))
                    ((null SM1r))
                 (push `(,(first SM1r) ,(first SM2r)) bindlist)
               )
               (push `(,lastvar ,SM5) bindlist)
               (if (null firstbind) (setq firstbind (first bindlist)))
               (push SM4 storelist)
               (setq lastvar (first SM3))
) )     )  ) )
;-------------------------------------------------------------------------------
(defmacro define-modify-macro (name lambdalist function &optional docstring)
  (let* ((varlist nil)
         (restvar nil))
    (do* ((lambdalistr lambdalist (cdr lambdalistr))
          (next))
         ((null lambdalistr))
      (setq next (first lambdalistr))
      (cond ((eq next '&OPTIONAL))
            ((eq next '&REST)
             (if (symbolp (second lambdalistr))
               (setq restvar (second lambdalistr))
               (error #+DEUTSCH "In der Definition von ~S ist die &REST-Variable kein Symbol: ~S"
                      #+ENGLISH "In the definition of ~S: &REST variable ~S should be a symbol."
                      #+FRANCAIS "Dans la définition de ~S la variable pour &REST n'est pas un symbole : ~S."
                      name (second lambdalistr)
             ) )
             (if (null (cddr lambdalistr))
               (return)
               (error #+DEUTSCH "Nach &REST ist nur eine Variable erlaubt; es kam: ~S"
                      #+ENGLISH "Only one variable is allowed after &REST, not ~S"
                      #+FRANCAIS "Une seule variable est permise pour &REST et non ~S."
                      lambdalistr
            )) )
            ((or (eq next '&KEY) (eq next '&ALLOW-OTHER-KEYS) (eq next '&AUX))
             (error #+DEUTSCH "In einer DEFINE-MODIFY-MACRO-Lambdaliste ist ~S unzulässig."
                    #+ENGLISH "Illegal in a DEFINE-MODIFY-MACRO lambda list: ~S"
                    #+FRANCAIS "~S n'est pas permis dans une liste lambda pour DEFINE-MODIFY-MACRO."
                    next
            ))
            ((symbolp next) (push next varlist))
            ((and (listp next) (symbolp (first next)))
             (push (first next) varlist)
            )
            (t (error #+DEUTSCH "Lambdalisten dürfen nur Symbole und Listen enthalten, nicht aber ~S"
                      #+ENGLISH "lambda list may only contain symbols and lists, not ~S"
                      #+FRANCAIS "Les listes lambda ne peuvent contenir que des symboles et des listes et non ~S."
                      next
            )  )
    ) )
    (setq varlist (nreverse varlist))
    `(DEFMACRO ,name (%REFERENCE ,@lambdalist &ENVIRONMENT ENV) ,docstring
       (MULTIPLE-VALUE-BIND (DUMMIES VALS NEWVAL SETTER GETTER)
           (GET-SETF-METHOD %REFERENCE ENV)
         (DO ((D DUMMIES (CDR D))
              (V VALS (CDR V))
              (LET-LIST NIL (CONS (LIST (CAR D) (CAR V)) LET-LIST)))
             ((NULL D)
              (WHEN (SYMBOLP GETTER)
                (RETURN
                  (SUBST
                    (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                    (CAR NEWVAL)
                    SETTER
              ) ) )
              (PUSH
                (LIST
                  (CAR NEWVAL)
                  (IF (AND (LISTP %REFERENCE) (EQ (CAR %REFERENCE) 'THE))
                    (LIST 'THE (CADR %REFERENCE)
                      (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                    )
                    (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                ) )
                LET-LIST
              )
              (LIST 'LET* (NREVERSE LET-LIST) SETTER)
     ) ) ) )
) )
;-------------------------------------------------------------------------------
(define-modify-macro decf (&optional (delta 1)) -)
;-------------------------------------------------------------------------------
(define-modify-macro incf (&optional (delta 1)) +)
;-------------------------------------------------------------------------------
(defmacro setf (&whole form &rest args &environment env)
  (let ((dummy (length args)))
    (cond ((= dummy 2)
           (let* ((place (first args))
                  (value (second args)))
             (do* ((oldplaceform nil newplaceform)
                   (newplaceform place (macroexpand-1 newplaceform env)))
                  ((eq newplaceform oldplaceform)
                   (error #+DEUTSCH "Das ist keine erlaubte 'SETF-Place' : ~S"
                          #+ENGLISH "Illegal SETF place: ~S"
                          #+FRANCAIS "Ceci n'est pas une place modifiable valide : ~S"
                          place
                  ))
               (cond ((atom newplaceform)
                      (return `(SETQ ,newplaceform ,value))
                     )
                     ((and (setq dummy
                             (get (first newplaceform) 'SYSTEM::SETF-EXPANDER)
                           )
                           (symbolp dummy)
                      )
                      (return `(,dummy ,@(cdr newplaceform) ,value))
                     )
                     ((and (eq (first newplaceform) 'THE)
                           (eql (length newplaceform) 3)
                      )
                      (return `(SETF ,(third newplaceform)
                                     (THE ,(second newplaceform) ,value)
                     ))        )
                     ((and (eq (first newplaceform) 'VALUES-LIST)
                           (eql (length newplaceform) 2)
                      )
                      (return `(VALUES-LIST
                                 (SETF ,(second newplaceform)
                                       (MULTIPLE-VALUE-LIST ,value)
                     ))        ) )
                     (dummy
                       (return
                         (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
                             (get-setf-method-multiple-value newplaceform env)
                           (declare (ignore SM5))
                           (do* ((SM1r SM1 (cdr SM1r))
                                 (SM2r SM2 (cdr SM2r))
                                 (bindlist nil))
                                ((null SM1r)
                                 (if (eql (length SM3) 1) ; eine Store-Variable
                                   `(LET* ,(nreverse
                                             (cons `(,(first SM3) ,value)
                                                   bindlist
                                           ) )
                                      ,SM4
                                    )
                                   ; mehrere Store-Variable
                                   (if
                                     ; Hat SM4 die Gestalt
                                     ; (VALUES (SETQ v1 store1) ...) ?
                                     (and (consp SM4) (eq (car SM4) 'VALUES)
                                       (do ((SM3r SM3 (cdr SM3r))
                                            (SM4r (cdr SM4) (cdr SM4r)))
                                           ((or (null SM3r) (null SM4r))
                                            (and (null SM3r) (null SM4r))
                                           )
                                         (unless (and (consp (car SM4r))
                                                      (eq (caar SM4r) 'SETQ)
                                                      (symbolp (cadar SM4r))
                                                      (eq (caddar SM4r) (car SM3r))
                                                 )
                                           (return nil)
                                     ) ) )
                                     (let ((vlist (mapcar #'second (rest SM4))))
                                       `(LET* ,(nreverse bindlist)
                                          (MULTIPLE-VALUE-SETQ ,vlist ,value)
                                          (VALUES ,@vlist)
                                     )  )
                                     `(LET* ,(nreverse bindlist)
                                        (MULTIPLE-VALUE-BIND ,SM3 ,value
                                          ,SM4
                                      ) )
                                )) )
                             (push `(,(first SM1r) ,(first SM2r)) bindlist)
          )) ) )     ) ) ) )
          ((oddp dummy)
           (error #+DEUTSCH "~S mit einer ungeraden Zahl von Argumenten aufgerufen: ~S"
                  #+ENGLISH "~S called with an odd number of arguments: ~S"
                  #+FRANCAIS "~S fut appelé avec un nombre impair d'arguments : ~S"
                  'setf form
          ))
          (t (do* ((arglist args (cddr arglist))
                   (L nil))
                  ((null arglist) `(PROGN ,@(nreverse L)))
               (push `(SETF ,(first arglist) ,(second arglist)) L)
          )  )
) ) )
;-------------------------------------------------------------------------------
(defmacro shiftf (&whole form &rest args &environment env)
  (when (< (length args) 2)
    (error #+DEUTSCH "SHIFTF mit zu wenig Argumenten aufgerufen: ~S"
           #+ENGLISH "SHIFTF called with too few arguments: ~S"
           #+FRANCAIS "SHIFTF fut appelé avec trop peu d'arguments : ~S"
           form
  ) )
  (do* ((resultvar (gensym))
        (arglist args (cdr arglist))
        (bindlist nil)
        (storelist nil)
        (lastvar resultvar))
       ((atom (cdr arglist))
        (push `(,lastvar ,(first arglist)) bindlist)
        `(LET* ,(nreverse bindlist) ,@(nreverse storelist) ,resultvar)
       )
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method (first arglist) env)
      (do* ((SM1r SM1 (cdr SM1r))
            (SM2r SM2 (cdr SM2r)))
           ((null Sm1r))
        (push `(,(first SM1r) ,(first SM2r)) bindlist)
      )
      (push `(,lastvar ,SM5) bindlist)
      (push SM4 storelist)
      (setq lastvar (first SM3))
) ) )
;-------------------------------------------------------------------------------
; Definition von places:
;-------------------------------------------------------------------------------
(defsetf aref (array &rest indices) (value)
  `(SYSTEM::STORE ,array ,@indices ,value)
)
;-------------------------------------------------------------------------------
(defun SYSTEM::%SETNTH (index list value)
  (let ((pointer (nthcdr index list)))
    (if (null pointer)
      (error #+DEUTSCH "(SETF (NTH ...) ...) : Index ~S ist zu groß für ~S."
             #+ENGLISH "(SETF (NTH ...) ...) : index ~S is too large for ~S"
             #+FRANCAIS "(SETF (NTH ...) ...) : L'index ~S est trop grand pour ~S."
             index list
      )
      (rplaca pointer value)
    )
    value
) )
(defsetf nth SYSTEM::%SETNTH)
;-------------------------------------------------------------------------------
(defsetf elt SYSTEM::%SETELT)
;-------------------------------------------------------------------------------
(defsetf rest SYSTEM::%RPLACD)
(defsetf first SYSTEM::%RPLACA)
(defsetf second (list) (value) `(SYSTEM::%RPLACA (CDR ,list) ,value))
(defsetf third (list) (value) `(SYSTEM::%RPLACA (CDDR ,list) ,value))
(defsetf fourth (list) (value) `(SYSTEM::%RPLACA (CDDDR ,list) ,value))
(defsetf fifth (list) (value) `(SYSTEM::%RPLACA (CDDDDR ,list) ,value))
(defsetf sixth (list) (value) `(SYSTEM::%RPLACA (CDR (CDDDDR ,list)) ,value))
(defsetf seventh (list) (value) `(SYSTEM::%RPLACA (CDDR (CDDDDR ,list)) ,value))
(defsetf eighth (list) (value) `(SYSTEM::%RPLACA (CDDDR (CDDDDR ,list)) ,value))
(defsetf ninth (list) (value) `(SYSTEM::%RPLACA (CDDDDR (CDDDDR ,list)) ,value))
(defsetf tenth (list) (value) `(SYSTEM::%RPLACA (CDR (CDDDDR (CDDDDR ,list))) ,value))

(defsetf car SYSTEM::%RPLACA)
(defsetf cdr SYSTEM::%RPLACD)
(defsetf caar (list) (value) `(SYSTEM::%RPLACA (CAR ,list) ,value))
(defsetf cadr (list) (value) `(SYSTEM::%RPLACA (CDR ,list) ,value))
(defsetf cdar (list) (value) `(SYSTEM::%RPLACD (CAR ,list) ,value))
(defsetf cddr (list) (value) `(SYSTEM::%RPLACD (CDR ,list) ,value))
(defsetf caaar (list) (value) `(SYSTEM::%RPLACA (CAAR ,list) ,value))
(defsetf caadr (list) (value) `(SYSTEM::%RPLACA (CADR ,list) ,value))
(defsetf cadar (list) (value) `(SYSTEM::%RPLACA (CDAR ,list) ,value))
(defsetf caddr (list) (value) `(SYSTEM::%RPLACA (CDDR ,list) ,value))
(defsetf cdaar (list) (value) `(SYSTEM::%RPLACD (CAAR ,list) ,value))
(defsetf cdadr (list) (value) `(SYSTEM::%RPLACD (CADR ,list) ,value))
(defsetf cddar (list) (value) `(SYSTEM::%RPLACD (CDAR ,list) ,value))
(defsetf cdddr (list) (value) `(SYSTEM::%RPLACD (CDDR ,list) ,value))
(defsetf caaaar (list) (value) `(SYSTEM::%RPLACA (CAAAR ,list) ,value))
(defsetf caaadr (list) (value) `(SYSTEM::%RPLACA (CAADR ,list) ,value))
(defsetf caadar (list) (value) `(SYSTEM::%RPLACA (CADAR ,list) ,value))
(defsetf caaddr (list) (value) `(SYSTEM::%RPLACA (CADDR ,list) ,value))
(defsetf cadaar (list) (value) `(SYSTEM::%RPLACA (CDAAR ,list) ,value))
(defsetf cadadr (list) (value) `(SYSTEM::%RPLACA (CDADR ,list) ,value))
(defsetf caddar (list) (value) `(SYSTEM::%RPLACA (CDDAR ,list) ,value))
(defsetf cadddr (list) (value) `(SYSTEM::%RPLACA (CDDDR ,list) ,value))
(defsetf cdaaar (list) (value) `(SYSTEM::%RPLACD (CAAAR ,list) ,value))
(defsetf cdaadr (list) (value) `(SYSTEM::%RPLACD (CAADR ,list) ,value))
(defsetf cdadar (list) (value) `(SYSTEM::%RPLACD (CADAR ,list) ,value))
(defsetf cdaddr (list) (value) `(SYSTEM::%RPLACD (CADDR ,list) ,value))
(defsetf cddaar (list) (value) `(SYSTEM::%RPLACD (CDAAR ,list) ,value))
(defsetf cddadr (list) (value) `(SYSTEM::%RPLACD (CDADR ,list) ,value))
(defsetf cdddar (list) (value) `(SYSTEM::%RPLACD (CDDAR ,list) ,value))
(defsetf cddddr (list) (value) `(SYSTEM::%RPLACD (CDDDR ,list) ,value))
;-------------------------------------------------------------------------------
(defsetf svref SYSTEM::SVSTORE)
;-------------------------------------------------------------------------------
(defsetf GET (symbol indicator &optional default) (value)
  (let ((storeform `(SYSTEM::%PUT ,symbol ,indicator ,value)))
    (if default
      `(PROGN ,default ,storeform) ; default wird nur zum Schein ausgewertet
      `,storeform
) ) )
;-------------------------------------------------------------------------------
; Schreibt zu einem bestimmten Indicator einen Wert in eine gegebene
; Propertyliste. Wert ist NIL falls erfolgreich getan oder die neue
; (erweiterte) Propertyliste.
(defun sys::%putf (plist indicator value)
  (do ((plistr plist (cddr plistr)))
      ((atom plistr) (list* indicator value plist))
    (when (atom (cdr plistr))
      (error #+DEUTSCH "(SETF (GETF ...) ...) : Property-Liste ungerader Länge aufgetaucht."
             #+ENGLISH "(SETF (GETF ...) ...) : property list with an odd length"
             #+FRANCAIS "(SETF (GETF ...) ...) : Occurence d'une liste de propriétés de longueur impaire."
    ))
    (when (eq (car plistr) indicator)
      (rplaca (cdr plistr) value)
      (return nil)
) ) )
(define-setf-method getf (place indicator &optional default &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
    (let* ((storevar (gensym))
           (indicatorvar (gensym))
           (defaultvar-list (if default (list (gensym)) `()))
          )
      (values
        `(,@SM1 ,indicatorvar ,@defaultvar-list)
        `(,@SM2 ,indicator    ,@(if default `(,default) `()))
        `(,storevar)
        `(LET ((,(first SM3) (SYS::%PUTF ,SM5 ,indicatorvar ,storevar)))
           ,@defaultvar-list ; defaultvar zum Schein auswerten
           (WHEN ,(first SM3) ,SM4)
           ,storevar
         )
        `(GETF ,SM5 ,indicatorvar ,@defaultvar-list)
) ) ) )
;-------------------------------------------------------------------------------
(defsetf GETHASH (key hashtable &optional default) (value)
  (let ((storeform `(SYSTEM::PUTHASH ,key ,hashtable ,value)))
    (if default
      `(PROGN ,default ,storeform) ; default wird nur zum Schein ausgewertet
      `,storeform
) ) )
;-------------------------------------------------------------------------------
#| ; siehe oben:
(defun SYSTEM::%SET-DOCUMENTATION (symbol doctype value)
  (unless (symbolp symbol)
    (error #+DEUTSCH "Das ist als erstes Argument unzulässig, da kein Symbol: ~S"
           #+ENGLISH "first argument ~S is illegal, not a symbol"
           #+FRANCAIS "Le premier argument ~S est invalide car ce n'est pas un symbole."
           symbol
  ) )
  (if (null value)
    (remf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype)
    (setf (getf (get symbol 'SYSTEM::DOCUMENTATION-STRINGS) doctype) value)
) )
|#
(defsetf documentation SYSTEM::%SET-DOCUMENTATION)
;-------------------------------------------------------------------------------
(defsetf fill-pointer SYSTEM::SET-FILL-POINTER)
;-------------------------------------------------------------------------------
(defsetf SYMBOL-VALUE SET)
;-------------------------------------------------------------------------------
(defsetf SYMBOL-FUNCTION SYSTEM::%PUTD)
;-------------------------------------------------------------------------------
(defsetf SYMBOL-PLIST SYSTEM::%PUTPLIST)
;-------------------------------------------------------------------------------
(defsetf MACRO-FUNCTION (symbol) (value)
  `(PROGN
     (SETF (SYMBOL-FUNCTION ,symbol) (CONS 'SYSTEM::MACRO ,value))
     (REMPROP ,symbol 'SYSTEM::MACRO)
     ,value
   )
)
;-------------------------------------------------------------------------------
(defsetf CHAR SYSTEM::STORE-CHAR)
(defsetf SCHAR SYSTEM::STORE-SCHAR)
(defsetf BIT SYSTEM::STORE)
(defsetf SBIT SYSTEM::STORE)
(defsetf SUBSEQ (sequence start &optional end) (value)
  `(PROGN (REPLACE ,sequence ,value :START1 ,start :END1 ,end) ,value)
)
;-------------------------------------------------------------------------------
(define-setf-method char-bit (char name &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method char env)
    (let* ((namevar (gensym))
           (storevar (gensym)))
      (values `(,@SM1 ,namevar)
              `(,@SM2 ,name)
              `(,storevar)
              `(LET ((,(first SM3) (SET-CHAR-BIT ,SM5 ,namevar ,storevar)))
                 ,SM4
                 ,storevar
               )
              `(CHAR-BIT ,SM5 ,namevar)
) ) ) )
;-------------------------------------------------------------------------------
(define-setf-method LDB (bytespec integer &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method integer env)
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar SM1)
              (cons bytespec SM2)
              `(,storevar)
              `(LET ((,(first SM3) (DPB ,storevar ,bytespecvar ,SM5)))
                 ,SM4
                 ,storevar
               )
              `(LDB ,bytespecvar ,SM5)
) ) ) )
;-------------------------------------------------------------------------------
(define-setf-method MASK-FIELD (bytespec integer &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method integer env)
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar SM1)
              (cons bytespec SM2)
              `(,storevar)
              `(LET ((,(first SM3) (DEPOSIT-FIELD ,storevar ,bytespecvar ,SM5)))
                 ,SM4
                 ,storevar
               )
              `(MASK-FIELD ,bytespecvar ,SM5)
) ) ) )
;-------------------------------------------------------------------------------
(define-setf-method THE (type place &environment env)
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method place env)
    (values SM1 SM2 SM3
            (subst `(THE ,type ,(first SM3)) (first SM3) SM4)
            `(THE ,type ,SM5)
) ) )
;-------------------------------------------------------------------------------
(define-setf-method APPLY (fun &rest args &environment env)
  (if (and (listp fun)
           (eq (list-length fun) 2)
           (eq (first fun) 'FUNCTION)
           (symbolp (second fun))
      )
    (setq fun (second fun))
    (error #+DEUTSCH "SETF von APPLY ist nur für Funktionen der Form #'symbol als Argument definiert."
           #+ENGLISH "SETF APPLY is only defined for functions of the form #'symbol."
           #+FRANCAIS "Un SETF de APPLY n'est défini que pour les fonctions de la forme #'symbole."
  ) )
  (multiple-value-bind (SM1 SM2 SM3 SM4 SM5) (get-setf-method (cons fun args) env)
    (unless (eq (car (last args)) (car (last SM2)))
      (error #+DEUTSCH "APPLY von ~S kann nicht als 'SETF-Place' aufgefaßt werden."
             #+ENGLISH "APPLY on ~S is not a SETF place."
             #+FRANCAIS "APPLY de ~S ne peux pas être considéré comme une place modifiable."
             fun
    ) )
    (let ((item (car (last SM1)))) ; 'item' steht für eine Argumentliste!
      (labels ((splice (arglist)
                 ; Würde man in (LIST . arglist) das 'item' nicht als 1 Element,
                 ; sondern gespliced, sozusagen als ',@item', haben wollen, so
                 ; bräuchte man die Form, die (splice arglist) liefert.
                 (if (endp arglist)
                   'NIL
                   (let ((rest (splice (cdr arglist))))
                     (if (eql (car arglist) item)
                       ; ein (APPEND item ...) davorhängen, wie bei Backquote
                       (backquote-append item rest)
                       ; ein (CONS (car arglist) ...) davorhängen, wie bei Backquote
                       (backquote-cons (car arglist) rest)
              )) ) ) )
        (flet ((call-splicing (form)
                 ; ersetzt einen Funktionsaufruf form durch einen, bei dem
                 ; 'item' nicht 1 Argument, sondern eine Argumentliste liefert
                 (let ((fun (first form))
                       (argform (splice (rest form))))
                   ; (APPLY #'fun argform) vereinfachen:
                   ; (APPLY #'fun NIL) --> (fun)
                   ; (APPLY #'fun (LIST ...)) --> (fun ...)
                   ; (APPLY #'fun (CONS x y)) --> (APPLY #'fun x y)
                   ; (APPLY #'fun (LIST* ... z)) --> (APPLY #'fun ... z)
                   (if (or (null argform)
                           (and (consp argform) (eq (car argform) 'LIST))
                       )
                     (cons fun (cdr argform))
                     (list* 'APPLY
                            (list 'FUNCTION fun)
                            (if (and (consp argform)
                                     (or (eq (car argform) 'LIST*)
                                         (eq (car argform) 'CONS)
                                )    )
                              (cdr argform)
                              (list argform)
              )) ) ) )      )
          (values SM1 SM2 SM3 (call-splicing SM4) (call-splicing SM5))
) ) ) ) )
;-------------------------------------------------------------------------------
; Zusätzliche Definitionen von places
;-------------------------------------------------------------------------------
(define-setf-method funcall (fun &rest args &environment env)
  (unless (and (listp fun)
               (eq (list-length fun) 2)
               (let ((fun1 (first fun)))
                 (or (eq fun1 'FUNCTION) (eq fun1 'QUOTE))
               )
               (symbolp (second fun))
               (setq fun (second fun))
          )
    (error #+DEUTSCH "SETF von FUNCALL ist nur für Funktionen der Form #'symbol definiert."
           #+ENGLISH "SETF FUNCALL is only defined for functions of the form #'symbol."
           #+FRANCAIS "Un SETF de FUNCALL n'est défini que pour les fonctions de la forme #'symbole."
  ) )
  (get-setf-method (cons fun args) env)
)
;-------------------------------------------------------------------------------
(defsetf GET-DISPATCH-MACRO-CHARACTER
         (disp-char sub-char &optional (readtable '*READTABLE*)) (value)
  `(PROGN (SET-DISPATCH-MACRO-CHARACTER ,disp-char ,sub-char ,value ,readtable) ,value)
)
;-------------------------------------------------------------------------------
(defsetf long-float-digits SYSTEM::%SET-LONG-FLOAT-DIGITS)
;-------------------------------------------------------------------------------
;(defsetf default-directory SYSTEM::%SET-DEFAULT-DIRECTORY)
;-------------------------------------------------------------------------------
; Handhabung von (SETF (VALUES place1 ... placek) form)
; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;       (SETF place1 dummy1 ... placek dummyk)
;       (VALUES dummy1 ... dummyk)
;     )
(define-setf-method VALUES (&rest subplaces &environment env)
  (do ((temps nil)
       (vals nil)
       (stores nil)
       (storeforms nil)
       (accessforms nil)
       (subplacesr subplaces))
      ((atom subplacesr)
       (setq temps (nreverse temps))
       (setq vals (nreverse vals))
       (setq stores (nreverse stores))
       (setq storeforms (nreverse storeforms))
       (setq accessforms (nreverse accessforms))
       (values temps
               vals
               stores
               `(VALUES ,@storeforms)
               `(VALUES ,@accessforms)
      ))
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-method (pop subplacesr) env)
      (setq temps (revappend SM1 temps))
      (setq vals (revappend SM2 vals))
      (setq stores (revappend SM3 stores))
      (setq storeforms (cons SM4 storeforms))
      (setq accessforms (cons SM5 accessforms))
) ) )
;-------------------------------------------------------------------------------

