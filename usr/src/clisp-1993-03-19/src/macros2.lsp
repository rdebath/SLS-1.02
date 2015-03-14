(in-package "SYSTEM")
;-------------------------------------------------------------------------------
(defmacro typecase (keyform &rest typeclauselist)
  (let* ((tempvar (gensym))
         (condclauselist nil))
    (do ((typeclauselistr typeclauselist (cdr typeclauselistr)))
        ((atom typeclauselistr))
      (cond ((atom (car typeclauselistr))
             (error #+DEUTSCH "Unzulässige Klausel in ~S: ~S"
                    #+ENGLISH "Invalid clause in ~S: ~S"
                    #+FRANCAIS "Clause inadmissible dans ~S : ~S"
                    'typecase (car typeclauselistr)
            ))
            ((let ((type (caar typeclauselistr)))
               (or (eq type T) (eq type 'OTHERWISE))
             )
             (push `(T ,@(or (cdar typeclauselistr) '(NIL))) condclauselist)
             (return)
            )
            (t (push `((TYPEP ,tempvar (QUOTE ,(caar typeclauselistr)))
                       ,@(or (cdar typeclauselistr) '(NIL))
                      )
                     condclauselist
            )  )
    ) )
    `(LET ((,tempvar ,keyform)) (COND ,@(nreverse condclauselist)))
) )
;-------------------------------------------------------------------------------
(defmacro check-type (place typespec &optional (string nil))
  (let ((tag1 (gensym))
        (tag2 (gensym)))
    `(TAGBODY
       ,tag1
       (WHEN (TYPEP ,place ',typespec) (GO ,tag2))
       (CERROR #+DEUTSCH "Sie dürfen einen neuen Wert eingeben."
               #+ENGLISH "You may input a new value."
               #+FRANCAIS "Vous avez l'occasion d'entrer une nouvelle valeur."
         #+DEUTSCH "~A~%Der Wert ist: ~S"
         #+ENGLISH "~A~%The value is: ~S"
         #+FRANCAIS "~A~%La valeur est : ~S"
         ,(format nil #+DEUTSCH "Der Wert von ~S sollte ~:[vom Typ ~S~;~:*~A~] sein."
                      #+ENGLISH "The value of ~S should be ~:[of type ~S~;~:*~A~]."
                      #+FRANCAIS "La valeur de ~S devrait être ~:[de type ~S~;~:*~A~]."
                      place string typespec
          )
         ,place
       )
       (WRITE-STRING
         ,(format nil #+DEUTSCH "~%Neues ~S: "
                      #+ENGLISH "~%New ~S: "
                      #+FRANCAIS "~%Nouveau ~S : "
                      place
          )
         *QUERY-IO*
       )
       (SETF ,place (READ *QUERY-IO*))
       (GO ,tag1)
       ,tag2
     )
) )
;-------------------------------------------------------------------------------
(defmacro assert (test-form &optional (place-list nil) (string nil) &rest args)
  (let ((tag1 (gensym))
        (tag2 (gensym)))
    `(TAGBODY
       ,tag1
       (WHEN ,test-form (GO ,tag2))
       (CERROR ,(case (length place-list)
                  (0 #+DEUTSCH "Neuer Anlauf"
                     #+ENGLISH "Retry"
                     #+FRANCAIS "Reéssayer"
                  )
                  (1 #+DEUTSCH "Sie dürfen einen neuen Wert eingeben."
                     #+ENGLISH "You may input a new value."
                     #+FRANCAIS "Vous pouvez entrer une nouvelle valeur."
                  )
                  (t #+DEUTSCH "Sie dürfen neue Werte eingeben."
                     #+ENGLISH "You may input new values."
                     #+FRANCAIS "Vous pouvez entrer de nouvelles valeurs."
                ) )
               ',(or string "~A")
               ,@(if string
                   args
                   (list (format nil #+DEUTSCH "Der Wert von ~S darf nicht NIL sein."
                                     #+ENGLISH "~S must evaluate to a non-NIL value."
                                     #+FRANCAIS "La valeur de ~S ne peut pas être NIL."
                                     test-form
                 ) )     )
       )
       ,@(mapcan
           #'(lambda (place)
               (list `(WRITE-STRING
                        ,(format nil #+DEUTSCH "~%Neues ~S: "
                                     #+ENGLISH "~%New ~S: "
                                     #+FRANCAIS "~%Nouveau ~S : "
                                     place
                         )
                        *QUERY-IO*
                      )
                     `(SETF ,place (READ *QUERY-IO*))
             ) )
           place-list
         )
       (GO ,tag1)
       ,tag2
     )
) )
;-------------------------------------------------------------------------------
(flet ((typecase-errorstring (keyform keyclauselist)
         (format nil #+DEUTSCH "Der Wert von ~S muß einem der Typen ~{~S~^, ~} angehören."
                     #+ENGLISH "The value of ~S must be of one of the types ~{~S~^, ~}"
                     #+FRANCAIS "La valeur de ~S doit appartenir à l'un des types ~{~S~^, ~}."
                     keyform (mapcar #'first keyclauselist)
       ) )
       (case-errorstring (keyform keyclauselist)
         (format nil #+DEUTSCH "Der Wert von ~S muß einer der folgenden sein: ~{~S~^, ~}"
                     #+ENGLISH "The value of ~S must be one of ~{~S~^, ~}"
                     #+FRANCAIS "La valeur de ~S doit être l'une des suivantes : ~{~S~^, ~}"
                     keyform
                     (mapcap #'(lambda (keyclause)
                                 (setq keyclause (car keyclause))
                                 (if (listp keyclause) keyclause (list keyclause))
                               )
                             keyclauselist
       ) )           )
       (simple-error (casename form clauselist errorstring)
         (let ((var (gensym)))
           `(LET ((,var ,form))
              (,casename ,var
                ,@clauselist
                (OTHERWISE
                  (ERROR #+DEUTSCH "~A~%Der Wert ist: ~S"
                         #+ENGLISH "~A~%The value is: ~S"
                         #+FRANCAIS "~A~%La valeur est : ~S"
                         ,errorstring ,var
            ) ) ) )
       ) )
       (retry-loop (casename place clauselist errorstring)
         (let ((g (gensym))
               (h (gensym)))
           `(BLOCK ,g
              (TAGBODY
                ,h
                (RETURN-FROM ,g
                  (,casename ,place
                    ,@clauselist
                    (OTHERWISE
                      (CERROR #+DEUTSCH "Sie dürfen einen neuen Wert eingeben."
                              #+ENGLISH "You may input a new value."
                              #+FRANCAIS "Vous pouvez entrer une nouvelle valeur."
                              #+DEUTSCH "~A~%Der Wert ist: ~S"
                              #+ENGLISH "~A~%The value is: ~S"
                              #+FRANCAIS "~A~%La valeur est : ~S"
                              ,errorstring
                              ,place
                      )
                      (WRITE-STRING
                        ,(format nil #+DEUTSCH "~%Neues ~S: "
                                     #+ENGLISH "~%New ~S: "
                                     #+FRANCAIS "~%Nouveau ~S : "
                                     place
                         )
                        *QUERY-IO*
                      )
                      (SETF ,place (READ *QUERY-IO*))
                      (GO ,h)
            ) ) ) ) )
      )) )
  (defmacro etypecase (keyform &rest keyclauselist)
    (simple-error 'TYPECASE keyform keyclauselist
                  (typecase-errorstring keyform keyclauselist)
  ) )
  (defmacro ctypecase (keyplace &rest keyclauselist)
    (retry-loop 'TYPECASE keyplace keyclauselist
                (typecase-errorstring keyplace keyclauselist)
  ) )
  (defmacro ecase (keyform &rest keyclauselist)
    (simple-error 'CASE keyform keyclauselist
                  (case-errorstring keyform keyclauselist)
  ) )
  (defmacro ccase (keyform &rest keyclauselist)
    (retry-loop 'CASE keyform keyclauselist
                (case-errorstring keyform keyclauselist)
  ) )
)
;-------------------------------------------------------------------------------
(defmacro deftype (name lambdalist &body body &environment env)
  (unless (symbolp name)
    (error #+DEUTSCH "Typname muß ein Symbol sein, nicht ~S"
           #+ENGLISH "type name should be a symbol, not ~S"
           #+FRANCAIS "Le type doit être un symbole et non ~S"
           name
  ) )
  (if (or (get name 'TYPE-SYMBOL) (get name 'TYPE-LIST))
    (error #+DEUTSCH "~S ist ein eingebauter Typ und darf nicht umdefiniert werden."
           #+ENGLISH "~S is a built-in type and may not be redefined."
           #+FRANCAIS "~S est un type prédéfini et ne peut pas être redéfini."
           name
  ) )
  (multiple-value-bind (body-rest declarations docstring)
      (SYSTEM::PARSE-BODY body t env)
    (if declarations (setq declarations (list (cons 'DECLARE declarations))))
    (let ((%arg-count 0) (%min-args 0) (%restp nil)
          (%let-list nil) (%keyword-tests nil) (%default-form '(QUOTE *)))
      (analyze1 lambdalist '(CDR <DEFTYPE-FORM>) name '<DEFTYPE-FORM>)
      (let ((lengthtest (make-length-test '<DEFTYPE-FORM>))
            (mainform `(LET* ,(nreverse %let-list)
                         ,@declarations
                         ,@(nreverse %keyword-tests)
                         ,@body-rest
           ))          )
        (if lengthtest
          (setq mainform
            `(IF ,lengthtest
               (ERROR #+DEUTSCH "Der Deftype-Expander für ~S kann nicht mit ~S Argumenten aufgerufen werden."
                      #+ENGLISH "The deftype expander for ~S may not be called with ~S arguments."
                      #+FRANCAIS "L'«expandeur» de DEFTYPE pour ~S ne peut pas être appelé avec ~S arguments."
                      ',name (1- (LENGTH <DEFTYPE-FORM>))
               )
               ,mainform
        ) )  )
        `(EVAL-WHEN (COMPILE LOAD EVAL)
           (%PUT ',name 'DEFTYPE-EXPANDER
             (FUNCTION ,(make-symbol (string-concat "DEFTYPE-" (string name)))
               (LAMBDA (<DEFTYPE-FORM>) ,mainform)
           ) )
           (SETF (DOCUMENTATION ',name 'TYPE) ',docstring)
           ',name
         )
) ) ) )
;-------------------------------------------------------------------------------
(defmacro time (form)
  (let ((vars (list (gensym) (gensym) (gensym) (gensym) (gensym) (gensym)
                    (gensym) (gensym) (gensym)
       ))     )
    `(MULTIPLE-VALUE-BIND ,vars (%%TIME)
       (UNWIND-PROTECT ,form (MULTIPLE-VALUE-CALL #'%TIME (%%TIME) ,@vars))
     ) ; Diese Konstruktion verbraucht zur Laufzeit nur Stackplatz!
) )
;-------------------------------------------------------------------------------
(defmacro with-input-from-string
    ((var string &key (index nil sindex) (start '0 sstart) (end 'NIL send))
     &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations)))
    )
    `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string 
                   ,@(if (or sstart send)
                       `(,start ,@(if send `(,end) '()))
                       '()
          ))     )   )
       ,@declarations
       (UNWIND-PROTECT
         (PROGN ,@body-rest)
         ,@(if sindex `((SETF ,index (SYSTEM::STRING-INPUT-STREAM-INDEX ,var))) '())
         (CLOSE ,var)
     ) )
) )
;-------------------------------------------------------------------------------
(defmacro with-open-file ((stream &rest options) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations)))
    )
    `(LET ((,stream (OPEN ,@options)))
       ,@declarations
       (UNWIND-PROTECT
         (MULTIPLE-VALUE-PROG1 (PROGN ,@body-rest)
           (WHEN ,stream (CLOSE ,stream))
         )
         (WHEN ,stream (CLOSE ,stream :ABORT T))
     ) )
) )
;-------------------------------------------------------------------------------
(defmacro with-open-stream ((var stream) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations)))
    )
    `(LET ((,var ,stream))
       ,@declarations
       (UNWIND-PROTECT
         (MULTIPLE-VALUE-PROG1 (PROGN ,@body-rest) (CLOSE ,var))
         (CLOSE ,var :ABORT T)
     ) )
) )
;-------------------------------------------------------------------------------
(defmacro with-output-to-string
    ((var &optional (string nil sstring)) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations)))
    )
    (if sstring
      `(LET ((,var (SYS::MAKE-STRING-PUSH-STREAM ,string)))
         ,@declarations
         (UNWIND-PROTECT
           (PROGN ,@body-rest)
           (CLOSE ,var)
       ) )
      `(LET ((,var (MAKE-STRING-OUTPUT-STREAM)))
         ,@declarations
         (UNWIND-PROTECT
           (PROGN ,@body-rest (GET-OUTPUT-STREAM-STRING ,var))
           (CLOSE ,var)
       ) )
) ) )
;-------------------------------------------------------------------------------
(in-package "LISP")
(export 'with-output-to-printer)
(in-package "SYSTEM")
(defmacro with-output-to-printer ((var) &body body &environment env)
  #+ATARI
    `(LET ((,var *PRINTER-OUTPUT*)) ,@body)
  #-ATARI
    (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body nil env)
      (if declarations
        (setq declarations (list (cons 'DECLARE declarations)))
      )
      #+VMS
        (let ((filenamevar (gensym)))
          `(LET (,filenamevar)
             (WITH-OPEN-FILE (,var "SYS$SCRATCH:LISP_TO_PRINTER.TXT"
                                   :DIRECTION :OUTPUT
                                   :IF-EXISTS :NEW-VERSION
                             )
               ,@declarations
               (SETQ ,filenamevar (TRUENAME ,var))
               ,@body-rest
             )
             (SHELL (FORMAT NIL "PRINT /DELETE ~A" ,filenamevar))
           )
        )
      #-VMS
        `(LET ((,var #+UNIX (MAKE-PIPE-OUTPUT-STREAM "lpr")
                     #-UNIX (SYS::MAKE-PRINTER-STREAM)
              ))
           ,@declarations
           (UNWIND-PROTECT
             (PROGN ,@body-rest)
             (CLOSE ,var)
         ) )
    )
)
;-------------------------------------------------------------------------------

