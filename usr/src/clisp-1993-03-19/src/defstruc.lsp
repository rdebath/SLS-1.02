; Sources für DEFSTRUCT Macro.
; Bruno Haible 13.04.1988, 22.08.1988
; umgeschrieben am 02.09.1989 von Bruno Haible

(in-package "SYSTEM")

(defsetf %structure-ref %structure-store)

#| Erklärung der auftretenden Datentypen:

   (get name 'DEFSTRUCT-DESCRIPTION) =
     #(names type keyword-constructor slotlist defaultfun0 defaultfun1 ...)

   names ist eine Codierung der INCLUDE-Verschachtelung für Structure name:
   names = (name_1 ... name_i-1 . name_i) wobei name=name_1,
     name_1 enthält name_2, ..., name_i-1 enthält name_i.

   type (wenn der Typ der ganzen Structure gemeint ist):
      = T                      Abspeicherung als normale Structure
      = LIST                   Abspeicherung als Liste
      = VECTOR                 Abspeicherung als (simple-)Vector
      = (VECTOR element-type)  Abspeicherung als Vector mit Element-Typ

   keyword-constructor = NIL oder der Name des Keyword-Constructor

   slotlist ist eine gepackte Beschreibung der einzelnen slots einer Structure:
   slotlist = ({slot}*)
   slot = (name offset default type readonly)
   wobei name der Slotname ist,
              (NIL für den Slot, in dem der Structure-Name steht)
         default der Defaultwert ist:
              entweder eine Konstante, die zum Defaultwert evaluiert,
              oder eine Form (ein Symbol oder eine Liste (SVREF ...)), die
              bei Auswertung in einem beliebigen Environment eine Funktion
              liefert, die bei Aufruf den Defaultwert liefert.
         type der deklarierte Type für diesen Slot ist,
         readonly = NIL oder = T angibt, ob dieser Slot readonly ist, d.h.
              nach dem Aufbau der Structure nicht mehr mit (setf ...)
              verändert werden kann.
   Bei type = T belegt der Structure-Name den Slot 0, wird aber nicht in der
     slotlist aufgeführt, da zu seiner Initialisierung nichts zu tun ist.

|#


#| (ds-symbol-or-error x) liefert eine Fehlermeldung, falls x kein Symbol ist.
|#
(defun ds-symbol-or-error (x)
  (unless (symbolp x)
    (error #+DEUTSCH "~S: Das ist kein Symbol: ~S"
           #+ENGLISH "~S: this is not a symbol: ~S"
           #+FRANCAIS "~S : Ceci n'est pas un symbole: ~S"
           'defstruct x
) ) )

#| Hilfsfunktion für beide Konstruktoren:
   (ds-arg-default arg slot)
   liefert zu einem Argument arg (Teil einer Argumentliste) den Teil der
   Argumentliste, der dieses Argument mit dem Default für slot bindet.
|#

(defun ds-arg-default (arg slot)
  (let ((default (third slot)))
    ; Default ist entweder Konstante oder Funktion oder Symbol
    (if (constantp default)
      (if (null default) arg `(,arg ,default))
      `(,arg (SYS::%FUNCALL ,default))
) ) )

#| Hilfsfunktion für beide Konstruktoren:
   (ds-make-constructor-body type name names size slotlist)
   liefert den Ausdruck, der eine Structure vom vorgegebenen Typ
   kreiert und füllt.
|#
(defun ds-make-constructor-body (type name names size slotlist)
  `(LET ((OBJECT
           ,(cond ((eq type 'T) `(%MAKE-STRUCTURE ',names ,size))
                  ((eq type 'LIST) `(MAKE-LIST ,size))
                  ((consp type) `(MAKE-ARRAY ,size :ELEMENT-TYPE ',(second type)))
                  (t `(MAKE-ARRAY ,size))
            )
        ))
     ,@(mapcar
         #'(lambda (slot &aux (offset (second slot)))
             `(SETF
                ,(cond ((eq type 'T)
                        `(%STRUCTURE-REF ',name OBJECT ,offset) )
                       ((eq type 'LIST)
                        `(NTH ,offset OBJECT) )
                       ((eq type 'VECTOR)
                        `(SVREF OBJECT ,offset) )
                       (t `(AREF OBJECT ,offset) )
                 )
                ,(if (first slot)
                   `(THE ,(fourth slot) ,(first slot))
                   `(QUOTE ,(third slot))
              )  )
           )
         slotlist
       )
     OBJECT
)  )

#| Hilfsfunktion für ds-make-boa-constructor:

   (ds-arg-with-default arg slotlist)
   liefert zu einem Argument arg (Teil einer Argumentliste) den Teil der
   Argumentliste, der dieses Argument mit dem richtigen Defaultwert bindet.
|#

(defun ds-arg-with-default (arg slotlist)
  (if (listp arg)
    ; Defaultwert ist bereits mitgegeben
    arg
    ; nur ein Symbol
    (let ((slot (find arg slotlist :key #'first :test #'eq)))
      (if slot
        ; Slot gefunden -> dessen Defaultwert nehmen
        (ds-arg-default arg slot)
        ; Slot nicht gefunden, kein Defaultwert
        arg
) ) ) )

#| (ds-make-boa-constructor descriptor type name names size slotlist)
   liefert die Form, die den BOA-Konstrukor definiert.
|#
(defun ds-make-boa-constructor (descriptor type name names size slotlist)
  (let ((constructorname (first descriptor))
        (arglist (second descriptor)))
    ; auf &KEY und &ALLOW-OTHER-KEYS testen:
    (let ((keying (or (member '&KEY arglist :test #'eq)
                      (member '&ALLOW-OTHER-KEYS arglist :test #'eq)
         ))       )
      (when keying
        (error #+DEUTSCH "~S ~S: Die Argumentliste für eine keywordfreie Konstruktorfunktion ~S darf kein ~S enthalten: ~S"
               #+ENGLISH "~S ~S: the argument list for the BOA contructor ~S must not contain ~S: ~S"
               #+FRANCAIS "~S ~S : La liste d'arguments pour un constructeur ~S libre de mot-clés ne peux pas contenir ~S: ~S"
               'defstruct name constructorname (car keying) arglist
    ) ) )
    ; angegebene Argumente sammeln:
    (let* ((argnames
             (let ((L nil))
               (dolist (arg arglist)
                 (unless (member arg lambda-list-keywords :test #'eq)
                   (push (if (listp arg) (first arg) arg) L)
               ) )
               (nreverse L)
           ) )
           ; argnames ist die Liste aller bereits in der Paramterliste mit
           ; Werten versehenen Argumente.
           (new-arglist ; neue Argumentliste
             `(; required args:
               ,@(do ((arglistr arglist (cdr arglistr))
                      (arg)
                      (required-args nil))
                     ((or (endp arglistr)
                          (member (setq arg (car arglistr)) lambda-list-keywords :test #'eq)
                      )
                      (nreverse required-args)
                     )
                   (push arg required-args)
                 )
               ; optional args:
               ,@(do ((arglistr (cdr (member '&optional arglist :test #'eq)) (cdr arglistr))
                      (arg)
                      (optionals nil))
                     ((or (endp arglistr)
                          (member (setq arg (car arglistr)) lambda-list-keywords :test #'eq)
                      )
                      (if (null optionals) nil (cons '&optional (nreverse optionals)))
                     )
                   (push (ds-arg-with-default arg slotlist) optionals)
                 )
               ; rest arg:
               ,@(let ((arglistr (member '&rest arglist :test #'eq)))
                   (if arglistr `(&rest ,(second arglistr)) '())
                 )
               ; aux args:
               &aux
               ,@(do ((aux-args-r (cdr (member '&aux arglist :test #'eq)) (cdr aux-args-r))
                      (aux-arg)
                      (new-aux-args nil))
                     ((or (null aux-args-r)
                          (member (setq aux-arg (car aux-args-r)) lambda-list-keywords :test #'eq)
                      )
                      (nreverse new-aux-args)
                     )
                   (push (ds-arg-with-default aux-arg slotlist) new-aux-args)
                 )
               ,@(let ((slotinitlist nil))
                   (dolist (slot slotlist)
                     (when (first slot)
                       (unless (member (first slot) argnames :test #'eq)
                         (push (ds-arg-with-default (first slot) slotlist) slotinitlist)
                   ) ) )
                   (nreverse slotinitlist)
              )  )
          ))
      `(DEFUN ,constructorname ,new-arglist
         ,(ds-make-constructor-body type name names size slotlist)
       )
) ) )

#| (ds-make-keyword-constructor descriptor type name names size slotlist)
   liefert die Form, die den Keyword-Konstruktor definiert.
|#
(defun ds-make-keyword-constructor (descriptor type name names size slotlist)
  `(DEFUN ,descriptor
     (&KEY
      ,@(mapcap
          #'(lambda (slot)
              (if (first slot) (list (ds-arg-default (first slot) slot)) '())
            )
          slotlist
     )  )
     ,(ds-make-constructor-body type name names size slotlist)
)  )

#| (ds-make-pred predname type name name-offset)
   liefert die Form, die das Typtestprädikat für die Structure name kreiert.
   Dabei ist:
   type         der Typ der Structure,
   name         der Name der Structure,
   predname     der Name des Typtestprädikats,
   name-offset  (nur bei type /= T maßgeblich)
                die Stelle, an der der Name abgespeichert wird.
|#
(defun ds-make-pred (predname type name name-offset)
  `(,@(if (eq type 'T) `((PROCLAIM '(INLINE ,predname))) '())
    (DEFUN ,predname (OBJECT)
      ,(if (eq type 'T)
         `(%STRUCTURE-TYPE-P ',name OBJECT)
         (if (eq type 'LIST)
           `(AND (CONSP OBJECT)
                 ,@(if (eql name-offset 0)
                     `((EQ (CAR OBJECT) ',name))
                     `((> (LENGTH OBJECT) ,name-offset)
                       (EQ (NTH ,name-offset OBJECT) ',name)
                      )
            )      )
           `(AND (SIMPLE-VECTOR-P OBJECT)
                 (> (LENGTH OBJECT) ,name-offset)
                 (EQ (SVREF OBJECT ,name-offset) ',name)
            )
       ) )
   ))
)

(defun ds-make-copier (copiername name type)
  (declare (ignore name))
  `(,@(if (or (eq type 'T) (eq type 'LIST))
        `((PROCLAIM '(INLINE ,copiername)))
        '()
      )
    (DEFUN ,copiername (STRUCTURE)
      ,(if (eq type 'T)
         '(%COPY-STRUCTURE STRUCTURE)
         (if (eq type 'LIST)
           '(COPY-LIST STRUCTURE)
           (if (consp type)
             `(LET* ((OBJ-LENGTH (ARRAY-TOTAL-SIZE STRUCTURE))
                     (OBJECT (MAKE-ARRAY OBJ-LENGTH :ELEMENT-TYPE (QUOTE ,(second type))))
                    )
                (DOTIMES (I OBJ-LENGTH OBJECT)
                  (SETF (AREF OBJECT I) (AREF STRUCTURE I))
              ) )
             `(LET* ((OBJ-LENGTH (LENGTH STRUCTURE))
                     (OBJECT (MAKE-ARRAY OBJ-LENGTH)))
                (DOTIMES (I OBJ-LENGTH OBJECT)
                   (SETF (SVREF OBJECT I) (SVREF STRUCTURE I))
              ) )
       ) ) )
)  ))

(defun ds-make-accessors (name type concname slotlist)
  (mapcap
    #'(lambda (slot)
        (if (first slot)
          (let ((accessorname (concat-pnames concname (first slot)))
                (offset (second slot))
                (slottype (fourth slot)))
            `((PROCLAIM '(FUNCTION ,accessorname (,name) ,slottype))
              (PROCLAIM '(INLINE ,accessorname))
              (DEFUN ,accessorname (OBJECT)
                (THE ,slottype
                  ,(if (eq type 'T)
                     `(%STRUCTURE-REF ',name OBJECT ,offset)
                     (if (eq type 'LIST)
                       `(NTH ,offset OBJECT)
                       (if (consp type)
                         `(AREF OBJECT ,offset)
                         `(SVREF OBJECT ,offset)
             )) )  ) ) )
          )
          '()
      ) )
    slotlist
) )

(defun ds-make-defsetfs (name type concname slotlist)
  (mapcap
    #'(lambda (slot)
        (if (and (first slot) (not (fifth slot))) ; not READ-ONLY
          (let ((accessorname (concat-pnames concname (first slot)))
                (offset (second slot))
                (slottype (fourth slot)))
            `((DEFSETF ,accessorname (STRUCT) (VALUE)
                ,(if (eq type 'T)
                   `(LIST '%STRUCTURE-STORE '',name
                      STRUCT
                      ,offset
                      ,(if (eq 'T slottype)
                         `VALUE
                         `(LIST 'THE ',slottype VALUE)
                    )  )
                   (if (eq type 'LIST)
                     `(LIST 'SETF (LIST 'NTH ,offset STRUCT) VALUE)
                     (if (consp type)
                       `(LIST 'SETF (LIST 'AREF STRUCT ,offset) VALUE)
                       `(LIST 'SETF (LIST 'SVREF STRUCT ,offset) VALUE)
             ))  ) ) )
      ) ) )
    slotlist
) )

(defmacro defstruct (name-and-options . docstring-and-slotargs)
  (let ((name                              name-and-options)
        (options                           nil)
        (conc-name-option                  t)
        (constructor-option-list           nil)
        (keyword-constructor               nil)
        (copier-option                     t)
        (predicate-option                  0)
        (include-option                    nil)
         names
        (print-function-option             nil)
        (type-option                       t)
        (named-option                      0)
        (initial-offset-option             0)
        (initial-offset                    0)
        (docstring                         nil)
        (slotargs                          docstring-and-slotargs)
         size
        (include-skip                      0)
        (slotlist                          nil)
        (slotdefaultvars                   nil)
        (slotdefaultfuns                   nil)
         constructor-forms                      )
    ;; name-and-options überprüfen:
    (when (listp name-and-options)
      (setq name (first name-and-options))
      (setq options (rest name-and-options))
    ) ; andernfalls sind name und options schon korrekt.
    (unless (and (symbolp name) (not (keywordp name)))
      (error #+DEUTSCH "~S: Falsche Syntax für Name und Optionen: ~S"
             #+ENGLISH "~S: invalid syntax for name and options: ~S"
             #+FRANCAIS "~S : Mauvaise syntaxe pour un nom et des options: ~S"
             'defstruct name-and-options
    ) )
    ; name ist ein Symbol, options die Liste der Optionen.
    ;; Abarbeitung der Optionen:
    (dolist (option options)
      (when (keywordp option) (setq option (list option))) ; Option ohne Argumente
      (if (listp option)
        (if (keywordp (car option))
          (case (first option)
            (:CONC-NAME
               (setq conc-name-option (or (second option) ""))
            )
            (:CONSTRUCTOR
               (if (atom (cdr option))
                 ; Default-Keyword-Constructor
                 (push (concat-pnames "MAKE-" name) constructor-option-list)
                 (let ((arg (second option)))
                   (ds-symbol-or-error arg)
                   (push
                     (if (atom (cddr option))
                       arg ; Keyword-Constructor
                       (if (not (listp (third option)))
                         (error #+DEUTSCH "~S ~S: Argumentliste muß eine Liste sein: ~S"
                                #+ENGLISH "~S ~S: argument list should be a list: ~S"
                                #+FRANCAIS "~S ~S : La liste d'arguments doit être une liste: ~S"
                                'defstruct name (third option)
                         )
                         (rest option) ; BOA-Constructor
                     ) )
                     constructor-option-list
            )  ) ) )
            (:COPIER
               (when (consp (cdr option))
                 (let ((arg (second option)))
                   (ds-symbol-or-error arg)
                   (setq copier-option arg)
            )  ) )
            (:PREDICATE
               (when (consp (cdr option))
                 (let ((arg (second option)))
                   (ds-symbol-or-error arg)
                   (setq predicate-option arg)
            )  ) )
            (:INCLUDE
               (if (null include-option)
                 (setq include-option option)
                 (error #+DEUTSCH "~S ~S: Es darf nur ein :INCLUDE-Argument geben: ~S"
                        #+ENGLISH "~S ~S: At most one :INCLUDE argument may be specified: ~S"
                        #+FRANCAIS "~S ~S : Il ne peut y avoir qu'un argument :INCLUDE: ~S"
                        'defstruct name options
            )  ) )
            (:PRINT-FUNCTION
               (let ((arg (second option)))
                 (when (and (consp arg) (eq (first arg) 'FUNCTION))
                   (warn #+DEUTSCH "~S: Bei :PRINT-FUNCTION ist FUNCTION bereits implizit.~@
                                    Verwende daher ~S statt ~S."
                         #+ENGLISH "~S: Use of :PRINT-FUNCTION implicitly applies FUNCTION.~@
                                    Therefore using ~S instead of ~S."
                         #+FRANCAIS "~S : FUNCTION est déjà implicite avec :PRINT-FUNCTION.~@
                                     C'est pourquoi ~S est utilisé au lieu de ~S."
                         'defstruct (second arg) arg
                   )
                   (setq arg (second arg))
                 )
                 (setq print-function-option
                   (if (symbolp arg)
                     ; ein Ausdruck, der eine eventuelle lokale Definition
                     ; von arg mitberücksichtigt, aber nicht erfordert:
                     `(FUNCTION ,(concat-pnames name "-PRINT-FUNCTION")
                        (LAMBDA (STRUCT STREAM DEPTH)
                          (,arg STRUCT STREAM DEPTH)
                      ) )
                     `#',arg
            )  ) ) )
            (:TYPE (setq type-option (second option)))
            (:NAMED (setq named-option t))
            (:INITIAL-OFFSET (setq initial-offset-option (or (second option) 0)))
            (T (error #+DEUTSCH "~S ~S: Die Option ~S gibt es nicht."
                      #+ENGLISH "~S ~S: unknown option ~S"
                      #+FRANCAIS "~S ~S : Option ~S non reconnue."
                      'defstruct name (first option)
          ) )  )
          (error #+DEUTSCH "~S ~S: Falsche Syntax in ~S-Option: ~S"
                 #+ENGLISH "~S ~S: invalid syntax in ~S option: ~S"
                 #+FRANCAIS "~S ~S : Mauvaise syntaxe dans l'option ~S: ~S"
                 'defstruct name 'defstruct option
        ) )
        (error #+DEUTSCH "~S ~S: Das ist keine ~S-Option: ~S"
               #+ENGLISH "~S ~S: not a ~S option: ~S"
               #+FRANCAIS "~S ~S : Ceci n'est pas une option ~S: ~S"
               'defstruct name 'defstruct option
    ) ) )
    ; conc-name-option ist entweder T oder "" oder das :CONC-NAME-Argument.
    ; constructor-option-list ist eine Liste aller :CONSTRUCTOR-Argumente,
    ;   jeweils in der Form  symbol  oder  (symbol arglist . ...).
    ; copier-option ist entweder T oder das :COPIER-Argument.
    ; predicate-option ist entweder 0 oder das :PREDICATE-Argument.
    ; include-option ist entweder NIL oder die gesamte :INCLUDE-Option.
    ; print-function-option ist NIL oder eine Form, die die Print-Function
    ;   liefert.
    ; type-option ist entweder T oder das :TYPE-Argument.
    ; named-option ist entweder 0 oder T.
    ; initial-offset-option ist entweder 0 oder das :INITIAL-OFFSET-Argument.
    ;; Überprüfung der Optionen:
    (setq named-option (or (eq type-option 'T) (eq named-option 'T)))
    ; named-option (NIL oder T) gibt an, ob der Name in der Structure steckt.
    (if named-option
      (when (eql predicate-option 0)
        (setq predicate-option (concat-pnames name "-P")) ; Defaultname
      )
      (unless (or (eql predicate-option 0) (eq predicate-option 'NIL))
        (error #+DEUTSCH "~S ~S: Bei unbenannten Structures kann es kein :PREDICATE geben."
               #+ENGLISH "~S ~S: There is no :PREDICATE on unnamed structures."
               #+FRANCAIS "~S ~S : Il ne peut pas y avoir de :PREDICATE avec des structures anonymes."
               'defstruct name
    ) ) )
    ; predicate-option ist
    ;   bei named-option=T: entweder NIL oder der Name des Typtestprädikats,
    ;   bei named-option=NIL bedeutungslos.
    (if (eq conc-name-option 'T)
      (setq conc-name-option (string-concat (string name) "-"))
    )
    ; conc-name-option ist der Namensprefix.
    (if (null constructor-option-list)
      (setq constructor-option-list (list (concat-pnames "MAKE-" name)))
      (setq constructor-option-list (remove 'NIL constructor-option-list))
    )
    ; constructor-option-list ist eine Liste aller zu kreierenden Konstruktoren,
    ;   jeweils in der Form  symbol  oder  (symbol arglist . ...).
    (if (eq copier-option 'T)
      (setq copier-option (concat-pnames "COPY-" name))
    )
    ; copier-option ist entweder NIL oder der Name der Kopierfunktion.
    (unless (or (eq type-option 'T)
                (eq type-option 'VECTOR)
                (eq type-option 'LIST)
                (and (consp type-option) (eq (first type-option) 'VECTOR))
            )
      (error #+DEUTSCH "~S ~S: Unzulässige :TYPE-Option ~S"
             #+ENGLISH "~S ~S: invalid :TYPE option ~S"
             #+FRANCAIS "~S ~S : Option :TYPE inadmissible: ~S"
             'defstruct name type-option
    ) )
    ; type-option ist entweder T oder LIST oder VECTOR oder (VECTOR ...)
    (unless (and (integerp initial-offset-option) (>= initial-offset-option 0))
      (error #+DEUTSCH "~S ~S: Der :INITIAL-OFFSET muß ein Integer >=0 sein, nicht ~S"
             #+ENGLISH "~S ~S: The :INITIAL-OFFSET must be a nonnegative integer, not ~S"
             #+FRANCAIS "~S ~S : :INITIAL-OFFSET doit être un entier positif ou zéro et non ~S"
             'defstruct name initial-offset-option
    ) )
    ; initial-offset-option ist ein Integer >=0.
    (when (and (plusp initial-offset-option) (eq type-option 'T))
      (error #+DEUTSCH "~S ~S: :INITIAL-OFFSET darf nur zusammen mit :TYPE angegeben werden: ~S"
             #+ENGLISH "~S ~S: :INITIAL-OFFSET must not be specified without :TYPE : ~S"
             #+FRANCAIS "~S ~S : :INITIAL-OFFSET ne peut être précisé qu'ensemble avec :TYPE: ~S"
             'defstruct name options
    ) )
    ; Bei type-option=T ist initial-offset-option=0.
    (when (eq type-option 'T) (setq include-skip 1))
    ; include-skip ist 1 bei type-option=T, 0 sonst.
    (when (stringp (first docstring-and-slotargs))
      (setq docstring (first docstring-and-slotargs))
      (setq slotargs (rest docstring-and-slotargs))
    ) ; sonst stimmen docstring und slotargs bereits.
    ; docstring ist entweder NIL oder ein String.
    ; slotargs sind die restlichen Argumente.
    (if include-option
      (let* ((option (rest include-option))
             (subname (first option))
             (incl-desc (get subname 'DEFSTRUCT-DESCRIPTION)))
        (when (null incl-desc)
          (error #+DEUTSCH "~S ~S: Teilstruktur ~S ist nicht definiert."
                 #+ENGLISH "~S ~S: included structure ~S has not been defined."
                 #+FRANCAIS "~S ~S : La structure incluse ~S n'est pas définie."
                 'defstruct name subname
        ) )
        (setq names (cons name (svref incl-desc 0)))
        (unless (equalp (svref incl-desc 1) type-option)
          (error #+DEUTSCH "~S ~S: Teilstruktur ~S muß vom selben Typ ~S sein."
                 #+ENGLISH "~S ~S: included structure ~S must be of the same type ~S."
                 #+FRANCAIS "~S ~S : La structure incluse ~S doit être du même type ~S."
                 'defstruct name subname type-option
        ) )
        (setq slotlist (nreverse (mapcar #'copy-list (svref incl-desc 3))))
        ; slotlist ist die umgedrehte Liste der vererbten Slots
        (when slotlist (setq include-skip (1+ (second (first slotlist)))))
        ; include-skip >=0 ist die Anzahl der bereits von der Teilstruktur
        ;   verbrauchten Slots, das "size" der Teilstruktur.
        ; Weitere Argumente der :INCLUDE-Option abarbeiten:
        (dolist (slotarg (rest option))
          (let* ((slotname (if (atom slotarg) slotarg (first slotarg)))
                 (slot (find slotname slotlist :key #'first :test #'eq)))
            (when (null slot)
              (error #+DEUTSCH "~S ~S: Teilstruktur ~S hat keine Komponente namens ~S."
                     #+ENGLISH "~S ~S: included structure ~S has no component with name ~S."
                     #+FRANCAIS "~S ~S : La structure incluse ~S n'a pas de composante de nom ~S."
                     'defstruct name subname slotname
            ) )
            (if (atom slotarg)
              (setf (third slot) 'NIL) ; Default auf NIL überschreiben
              (progn
                (let ((default (second slotarg)))
                  (unless (constantp default)
                    (push
                      `(FUNCTION ,(concat-pnames "DEFAULT-" slotname)
                         (LAMBDA () ,default)
                       )
                      slotdefaultfuns
                    )
                    (setq default (gensym))
                    (push default slotdefaultvars)
                  )
                  (setf (third slot) default)
                )
                ; slot-options dieses Slot-Specifier abarbeiten:
                (do ((slot-arglistr (cddr slotarg) (cddr slot-arglistr)))
                    ((endp slot-arglistr))
                  (let ((slot-keyword (first slot-arglistr))
                        (slot-key-value (second slot-arglistr)))
                    (cond ((eq slot-keyword ':READ-ONLY)
                           (if slot-key-value
                             (setf (fifth slot) t)
                             (if (fifth slot)
                               (error #+DEUTSCH "~S ~S: Der READ-ONLY-Slot ~S von Teilstruktur ~S muß auch in ~S READ-ONLY bleiben."
                                      #+ENGLISH "~S ~S: The READ-ONLY slot ~S of the included structure ~S must remain READ-ONLY in ~S."
                                      #+FRANCAIS "~S ~S : Le composant READ-ONLY ~S de la structure incluse ~S doit rester READ-ONLY dans ~S."
                                      'defstruct name slotname subname name
                               )
                               (setf (fifth slot) nil)
                          )) )
                          ((eq slot-keyword ':TYPE)
                           (unless (subtypep slot-key-value (fourth slot))
                             (error #+DEUTSCH "~S ~S: Der Typ ~S von Slot ~S muß ein Untertyp des in Teilstruktur ~S definierten Typs ~S sein."
                                    #+ENGLISH "~S ~S: The type ~S of slot ~S should be a subtype of the type defined for the included strucure ~S, namely ~S."
                                    #+FRANCAIS "~S ~S : Le type ~S du composant ~S doit être un sous-type du type défini dans la structure incluse ~S, c'est-à-dire ~S."
                                    'defstruct name slot-key-value slotname subname (fourth slot)
                           ) )
                           (setf (fourth slot) slot-key-value)
                          )
                          (t (error #+DEUTSCH "~S ~S: ~S ist keine Slot-Option."
                                    #+ENGLISH "~S ~S: ~S is not a slot option."
                                    #+FRANCAIS "~S ~S : ~S n'est pas un option de composant."
                                    'defstruct name slot-keyword
                          )  )
                ) ) )
        ) ) ) )
      )
      (setq names name)
    )
    ; names ist die Include-Verschachtelung.
    ; slotlist ist die bisherige Slotliste, umgedreht.
    (when (and named-option ; benannte Structure
               (consp type-option) ; vom Typ (VECTOR ...)
               ; muß den/die Namen enthalten können:
               (not (typep names (second type-option)))
          )
      (error #+DEUTSCH "~S ~S: Structure vom Typ ~S kann den Namen nicht enthalten."
             #+ENGLISH "~S ~S: structure of type ~S can't hold the name."
             #+FRANCAIS "~S ~S : Une structure de type ~S ne peut pas contenir le nom."
             'defstruct name type-option
    ) )
    ; Aufbau der Structure:
    ; names, evtl. include-Slots, initial-offset-option mal NIL, Slots.
    ; Aufbau von Vektor oder Liste:
    ; include-Anteil, initial-offset-option mal NIL, evtl. Name, Slots.
    (setq initial-offset (+ include-skip initial-offset-option))
    (unless (eq type-option 'T)
      (when named-option
        (push
          (list nil ; Kennzeichen für Typerkennungs-Slot
                (setq initial-offset-option initial-offset)
                name ; "Defaultwert" = name
                'SYMBOL ; Typ = Symbol
                T) ; Read-Only
          slotlist
        )
        (setq initial-offset (1+ initial-offset))
    ) )
    ; Die einzelnen Slots kommen ab initial-offset.
    ; Bei type/=T (also Vektor oder Liste) und named-option sitzt
    ;   der Name in Slot Nummer  initial-offset-option = (1- initial-offset).
    ; Abarbeitung der einzelnen Slots:
    (let ((offset initial-offset))
      (dolist (slotarg slotargs)
        (let (slotname
              default)
          (if (atom slotarg)
            (setq slotname slotarg  default nil)
            (setq slotname (first slotarg)  default (second slotarg))
          )
          (unless (constantp default)
            (push
              `(FUNCTION ,(concat-pnames "DEFAULT-" slotname)
                 (LAMBDA () ,default)
               )
              slotdefaultfuns
            )
            (setq default (gensym))
            (push default slotdefaultvars)
          )
          (when (find slotname slotlist :key #'first :test #'eq)
            (error #+DEUTSCH "~S ~S: Es kann nicht mehrere Slots mit demselben Namen ~S geben."
                   #+ENGLISH "~S ~S: There may be only one slot with the name ~S."
                   #+FRANCAIS "~S ~S : Il ne peut pas y avoir plusieurs composants avec le même nom ~S."
                   'defstruct name slotname
          ) )
          (let ((type t) (read-only nil))
            (when (consp slotarg)
              (do ((slot-arglistr (cddr slotarg) (cddr slot-arglistr)))
                  ((endp slot-arglistr))
                (let ((slot-keyword (first slot-arglistr))
                      (slot-key-value (second slot-arglistr)))
                  (cond ((eq slot-keyword ':READ-ONLY)
                         (setq read-only (if slot-key-value t nil))
                        )
                        ((eq slot-keyword ':TYPE) (setq type slot-key-value))
                        (t (error #+DEUTSCH "~S ~S: ~S ist keine Slot-Option."
                                  #+ENGLISH "~S ~S: ~S is not a slot option."
                                  #+FRANCAIS "~S ~S : ~S n'est pas une option de composant."
                                  'defstruct name slot-keyword
                        )  )
            ) ) ) )
            (push (list slotname offset default type read-only) slotlist)
        ) )
        (incf offset)
      )
      (setq size offset)
    )
    ; size = Gesamtlänge der Structure
    (setq slotlist (nreverse slotlist))
    (setq slotdefaultfuns (nreverse slotdefaultfuns))
    (setq slotdefaultvars (nreverse slotdefaultvars))
    ; Die slots in slotlist sind jetzt wieder aufsteigend geordnet.
    (setq constructor-forms
      (mapcar
        #'(lambda (constructor-option)
            (if (consp constructor-option)
              (ds-make-boa-constructor
                constructor-option type-option name names size slotlist
              )
              (progn
                (if (null keyword-constructor)
                  (setq keyword-constructor constructor-option)
                )
                (ds-make-keyword-constructor
                  constructor-option type-option name names size slotlist
          ) ) ) )
        constructor-option-list
    ) )
    ; constructor-forms = Liste der Formen, die die Konstruktoren definieren.
    (let ((index 4))
      (dolist (defaultvar slotdefaultvars)
        (setf (third (find defaultvar slotlist :key #'third :test #'eq))
              `(SVREF (GET ',name 'DEFSTRUCT-DESCRIPTION) ,index)
        )
        (incf index)
    ) )
    ; slotlist enthält nun keine der slotdefaultvars mehr.
    `(EVAL-WHEN (LOAD COMPILE EVAL)
       (LET ,(mapcar #'list slotdefaultvars slotdefaultfuns)
         ,@constructor-forms
         (%PUT ',name 'DEFSTRUCT-DESCRIPTION
               (VECTOR ',names ',type-option ',keyword-constructor ',slotlist
                       ,@slotdefaultvars
       ) )     )
       ,@(if (and named-option predicate-option)
           (ds-make-pred predicate-option type-option name initial-offset-option)
         )
       ,@(if copier-option (ds-make-copier copier-option name type-option))
       ,@(ds-make-accessors name type-option conc-name-option slotlist)
       ,@(ds-make-defsetfs name type-option conc-name-option slotlist)
       (SETF (DOCUMENTATION ',name 'STRUCTURE) ,docstring)
       ,(if print-function-option
          `(%PUT ',name 'STRUCTURE-PRINT ,print-function-option)
          `(REMPROP ',name 'STRUCTURE-PRINT)
        )
       ',name
) )  )

