;;;; User-Interface, Teil 2
;;;; Funktionen fürs Debugging (Kapitel 25.3)
;;;; Apropos, Describe, Dribble, Ed
;;;; 27.6.1992

(in-package "LISP")
(export '(*editor* editor-tempfile edit-file saveinitmem))
(in-package "SYSTEM")

;-------------------------------------------------------------------------------
;; APROPOS

(defun apropos-list (string &optional (package nil))
  (let* ((L nil)
         (fun #'(lambda (sym)
                  (when
                      #| (search string (symbol-name sym) :test #'char-equal) |#
                      (sys::search-string-equal string sym) ; 15 mal schneller!
                    (push sym L)
                ) )
        ))
    (if package
      (system::map-symbols fun package)
      (system::map-all-symbols fun)
    )
    (sort L #'string< :key #'symbol-name)
) )

(defun fbound-string (sym) ; liefert den Typ eines Symbols sym mit (fboundp sym)
  (cond ((special-form-p sym)
         #+DEUTSCH "Spezialform"
         #+ENGLISH "special form"
         #+FRANCAIS "forme spéciale"
        )
        ((functionp (symbol-function sym))
         #+DEUTSCH "Funktion"
         #+ENGLISH "function"
         #+FRANCAIS "fonction"
        )
        (t #+DEUTSCH "Macro"
           #+ENGLISH "macro"
           #+FRANCAIS "macro"
) )     )

(defun apropos (string &optional (package nil))
  (dolist (sym (apropos-list string package))
    (print sym)
    (when (fboundp sym)
      (write-string "   ")
      (write-string (fbound-string sym))
    )
    (when (boundp sym)
      (write-string "   ")
      (if (constantp sym)
        (write-string #+DEUTSCH "Konstante"
                      #+ENGLISH "constant"
                      #+FRANCAIS "constante"
        )
        (write-string #+DEUTSCH "Variable"
                      #+ENGLISH "variable"
                      #+FRANCAIS "variable"
  ) ) ) )
  (values)
)

;-------------------------------------------------------------------------------
;; DESCRIBE

(defun describe (obj &aux (more '()))
  (format t #+DEUTSCH "~%Beschreibung von~%"
            #+ENGLISH "~%Description of~%"
            #+FRANCAIS "~%Description de~%"
  )
  (format t "~A" (write-to-short-string obj sys::*prin-linelength*))
  (format t #+DEUTSCH "~%Das ist "
            #+ENGLISH "~%This is "
            #+FRANCAIS "~%Ceci est "
  )
  (let ((type (type-of obj)))
    ; Dispatch nach den möglichen Resultaten von TYPE-OF:
    (if (atom type)
      (case type
        (CONS
          (flet ((list-length (list)  ; vgl. CLTL, S. 265
                   (do ((n 0 (+ n 2))
                        (fast list (cddr fast))
                        (slow list (cdr slow))
                       )
                       (nil)
                     (when (atom fast) (return n))
                     (when (atom (cdr fast)) (return (1+ n)))
                     (when (eq (cdr fast) slow) (return nil))
                )) )
            (let ((len (list-length obj)))
              (if len
                (if (null (nthcdr len obj))
                  (format t #+DEUTSCH "eine Liste der Länge ~S."
                            #+ENGLISH "a list of length ~S."
                            len
                  )
                  (if (> len 1)
                    (format t #+DEUTSCH "eine punktierte Liste der Länge ~S."
                              #+ENGLISH "a dotted list of length ~S."
                              len
                    )
                    (format t #+DEUTSCH "ein Cons."
                              #+ENGLISH "a cons."
                ) ) )
                (format t #+DEUTSCH "eine zyklische Liste."
                          #+ENGLISH "a cyclic list."
        ) ) ) ) )
        ((SYMBOL NULL)
          (when (null obj)
            (format t #+DEUTSCH "die leere Liste, "
                      #+ENGLISH "the empty list, "
          ) )
          (format t #+DEUTSCH "das Symbol ~S"
                    #+ENGLISH "the symbol ~S"
                    obj
          )
          (when (keywordp obj)
            (format t #+DEUTSCH ", ein Keyword"
                      #+ENGLISH ", a keyword"
          ) )
          (when (boundp obj)
            (if (constantp obj)
              (format t #+DEUTSCH ", eine Konstante"
                        #+ENGLISH ", a constant"
              )
              (if (sys::special-variable-p obj)
                (format t #+DEUTSCH ", eine SPECIAL-deklarierte Variable"
                          #+ENGLISH ", a variable declared SPECIAL"
                )
                (format t #+DEUTSCH ", eine Variable"
                          #+ENGLISH ", a variable"
            ) ) )
            (push `,obj more)
            (push `(SYMBOL-VALUE ',obj) more)
          )
          (when (fboundp obj)
            (format t #+DEUTSCH ", benennt "
                      #+ENGLISH ", names "
            )
            (cond ((special-form-p obj)
                   (format t #+DEUTSCH "eine Special-Form"
                             #+ENGLISH "a special form"
                   )
                   (when (macro-function obj)
                     (format t #+DEUTSCH " mit Macro-Definition"
                               #+ENGLISH " with macro definition"
                  )) )
                  ((functionp (symbol-function obj))
                   (format t #+DEUTSCH "eine Funktion"
                             #+ENGLISH "a function"
                   )
                   (push `#',obj more)
                   (push `(SYMBOL-FUNCTION ',obj) more)
                  )
                  (t ; (macro-function obj)
                   (format t #+DEUTSCH "einen Macro"
                             #+ENGLISH "a macro"
                  ))
          ) )
          (when (symbol-plist obj)
            (let ((properties
                    (do ((l nil)
                         (pl (symbol-plist obj) (cddr pl)))
                        ((null pl) (nreverse l))
                      (push (car pl) l)
                 )) )
              (format t #+DEUTSCH ", hat die Propert~@P ~{~S~^, ~}"
                        #+ENGLISH ", has the propert~@P ~{~S~^, ~}"
                        (length properties) properties
            ) )
            (push `(SYMBOL-PLIST ',obj) more)
          )
          (format t #+DEUTSCH "."
                    #+ENGLISH "."
          )
          (format t #+DEUTSCH "~%Das Symbol "
                    #+ENGLISH "~%The symbol "
          )
          (let ((home (symbol-package obj)))
            (if home
              (format t #+DEUTSCH "liegt in ~S"
                        #+ENGLISH "lies in ~S"
                        home
              )
              (format t #+DEUTSCH "ist uninterniert"
                        #+ENGLISH "is uninterned"
            ) )
            (let ((accessible-packs nil))
              (let ((normal-printout ; externe Repräsentation ohne Package-Marker
                      (if home
                        (let ((*package* home)) (prin1-to-string obj))
                        (let ((*print-gensym* nil)) (prin1-to-string obj))
                   )) )
                (dolist (pack (list-all-packages))
                  (when ; obj in pack accessible?
                        (string=
                          (let ((*package* pack)) (prin1-to-string obj))
                          normal-printout
                        )
                    (push pack accessible-packs)
              ) ) )
              (when accessible-packs
                (format t #+DEUTSCH " und ist in ~:[der Package~;den Packages~] ~{~A~^, ~} accessible"
                          #+ENGLISH " and is accessible in the package~:[~;s~] ~{~A~^, ~}"
                          (cdr accessible-packs)
                          (sort (mapcar #'package-name accessible-packs) #'string<)
          ) ) ) )
          (format t #+DEUTSCH "."
                    #+ENGLISH "."
        ) )
        ((FIXNUM BIGNUM)
          (format t #+DEUTSCH "eine ganze Zahl, belegt ~S Bits, ist als ~:(~A~) repräsentiert."
                    #+ENGLISH "an integer, uses ~S bits, is represented as a ~(~A~)."
                    (integer-length obj) type
        ) )
        (RATIO
          (format t #+DEUTSCH "eine rationale, nicht ganze Zahl."
                    #+ENGLISH "a rational, not integral number."
        ) )
        ((SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
          (format t #+DEUTSCH "eine Fließkommazahl mit ~S Mantissenbits (~:(~A~))."
                    #+ENGLISH "a float with ~S bits of mantissa (~(~A~))."
                    (float-digits obj) type
        ) )
        (COMPLEX
          (format t #+DEUTSCH "eine komplexe Zahl "
                    #+ENGLISH "a complex number "
          )
          (let ((x (realpart obj))
                (y (imagpart obj)))
            (if (zerop y)
              (if (zerop x)
                (format t #+DEUTSCH "im Ursprung"
                          #+ENGLISH "at the origin"
                )
                (format t #+DEUTSCH "auf der ~:[posi~;nega~]tiven reellen Achse"
                          #+ENGLISH "on the ~:[posi~;nega~]tive real axis"
                          (minusp x)
              ) )
              (if (zerop x)
                (format t #+DEUTSCH "auf der ~:[posi~;nega~]tiven imaginären Achse"
                          #+ENGLISH "on the ~:[posi~;nega~]tive imaginary axis"
                          (minusp y)
                )
                (format t #+DEUTSCH "im ~:[~:[ers~;vier~]~;~:[zwei~;drit~]~]ten Quadranten"
                          #+ENGLISH "in ~:[~:[first~;fourth~]~;~:[second~;third~]~] the quadrant"
                          (minusp x) (minusp y)
          ) ) ) )
          (format t #+DEUTSCH " der Gaußschen Zahlenebene."
                    #+ENGLISH " of the Gaussian number plane."
        ) )
        (CHARACTER
          (format t #+DEUTSCH "ein Zeichen"
                    #+ENGLISH "a character"
          )
          (unless (zerop (char-bits obj))
            (format t #+DEUTSCH " mit Zusatzbits"
                      #+ENGLISH " with additional bits"
          ) )
          (unless (zerop (char-font obj))
            (format t #+DEUTSCH " aus Zeichensatz ~S"
                      #+ENGLISH " from font ~S"
                      (char-font obj)
          ) )
          (format t #+DEUTSCH "."
                    #+ENGLISH "."
          )
          (format t #+DEUTSCH "~%Es ist ein ~:[nicht ~;~]druckbares Zeichen."
                    #+ENGLISH "~%It is a ~:[non-~;~]printable character."
                    (graphic-char-p obj)
          )
          (unless (standard-char-p obj)
            (format t #+DEUTSCH "~%Seine Verwendung ist nicht portabel."
                      #+ENGLISH "~%Its use is non-portable."
          ) )
        )
        (FUNCTION ; (SYS::CLOSUREP obj) ist erfüllt
          (let ((compiledp (compiled-function-p obj)))
            (format t #+DEUTSCH "eine ~:[interpret~;compil~]ierte Funktion."
                      #+ENGLISH "an ~:[interpret~;compil~]ed function."
                      compiledp
            )
            (if compiledp
              (multiple-value-bind (req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p)
                  (sys::signature obj)
                (describe-signature req-anz opt-anz rest-p key-p keyword-list allow-other-keys-p)
                (push `(DISASSEMBLE #',(sys::closure-name obj)) more)
                (push `(DISASSEMBLE ',obj) more)
              )
              (progn
                (format t #+DEUTSCH "~%Argumentliste: ~S"
                          #+ENGLISH "~%argument list: ~S"
                          (car (sys::%record-ref obj 1))
                )
                (let ((doc (sys::%record-ref obj 2)))
                  (when doc
                    (format t #+DEUTSCH "~%Dokumentation: ~A"
                              #+ENGLISH "~%documentation: ~A"
                              doc
              ) ) ) )
        ) ) )
        (COMPILED-FUNCTION ; nur SUBRs und FSUBRs
          (if (functionp obj)
            ; SUBR
            (progn
              (format t #+DEUTSCH "eine eingebaute System-Funktion."
                        #+ENGLISH "a built-in system function."
              )
              (multiple-value-bind (name req-anz opt-anz rest-p keywords allow-other-keys)
                  (sys::subr-info obj)
                (when name
                  (describe-signature req-anz opt-anz rest-p keywords keywords allow-other-keys)
            ) ) )
            ; FSUBR
            (format t #+DEUTSCH "ein Special-Form-Handler."
                      #+ENGLISH "a special form handler."
        ) ) )
        (STREAM
          (format t #+DEUTSCH "ein ~:[~:[geschlossener ~;Output-~]~;~:[Input-~;bidirektionaler ~]~]Stream."
                    #+ENGLISH "a~:[~:[ closed ~;n output-~]~;~:[n input-~;n input/output-~]~]stream."
                    (input-stream-p obj) (output-stream-p obj)
        ) )
        (PACKAGE
          (format t #+DEUTSCH "die Package mit Namen ~A"
                    #+ENGLISH "the package named ~A"
                    (package-name obj)
          )
          (let ((nicknames (package-nicknames obj)))
            (when nicknames
              (format t #+DEUTSCH " und zusätzlichen Namen ~{~A~^, ~}"
                        #+ENGLISH ". It has the nicknames ~{~A~^, ~}"
                        nicknames
          ) ) )
          (format t #+DEUTSCH "."
                    #+ENGLISH "."
          )
          (let ((use-list (package-use-list obj))
                (used-by-list (package-used-by-list obj)))
            (format t #+DEUTSCH "~%Sie "
                      #+ENGLISH "~%It "
            )
            (when use-list
              (format t #+DEUTSCH "importiert die externen Symbole der Package~:[~;s~] ~{~A~^, ~} und "
                        #+ENGLISH "imports the external symbols of the package~:[~;s~] ~{~A~^, ~} and "
                        (cdr use-list) (mapcar #'package-name use-list)
            ) )
            (format t #+DEUTSCH "exportiert ~:[keine Symbole~;die Symbole ~:*~{~S~^ ~}~]"
                      #+ENGLISH "exports ~:[no symbols~;the symbols ~:*~{~S~^ ~}~]"
                      ; Liste aller exportierten Symbole:
                      (let ((L nil))
                        (do-external-symbols (s obj) (push s L))
                        (sort L #'string< :key #'symbol-name)
            )         )
            (when used-by-list
              (format t #+DEUTSCH " an die Package~:[~;s~] ~{~A~^, ~}"
                        #+ENGLISH " to the package~:[~;s~] ~{~A~^, ~}"
                        (cdr used-by-list) (mapcar #'package-name used-by-list)
            ) )
            (format t #+DEUTSCH "."
                      #+ENGLISH "."
        ) ) )
        (HASH-TABLE
          (format t #+DEUTSCH "eine Hash-Tabelle mit ~S Eintr~:*~[ägen~;ag~:;ägen~]."
                    #+ENGLISH "a hash table with ~S entr~:@P."
                    (hash-table-count obj)
        ) )
        (READTABLE
          (format t #+DEUTSCH "~:[eine ~;die Common-Lisp-~]Readtable."
                    #+ENGLISH "~:[a~;the Common Lisp~] readtable."
                    (equalp obj (copy-readtable))
        ) )
        (PATHNAME
          (format t #+DEUTSCH "ein Pathname~:[.~;~:*, aufgebaut aus:~{~A~}~]"
                    #+ENGLISH "a pathname~:[.~;~:*, with the following components:~{~A~}~]"
                    (mapcan #'(lambda (kw component)
                                (when component
                                  (list (format nil "~%~A = ~A"
                                                    (symbol-name kw)
                                                    (make-pathname kw component)
                              ) ) )     )
                      '(:host :device :directory :name :type :version)
                      (list
                        (pathname-host obj)
                        (pathname-device obj)
                        (pathname-directory obj)
                        (pathname-name obj)
                        (pathname-type obj)
                        (pathname-version obj)
        ) )         ) )
        (RANDOM-STATE
          (format t #+DEUTSCH "ein Random-State."
                    #+ENGLISH "a random-state."
        ) )
        (BYTE
          (format t #+DEUTSCH "ein Byte-Specifier, bezeichnet die ~S Bits ab Bitposition ~S eines Integers."
                    #+ENGLISH "a byte specifier, denoting the ~S bits starting at bit position ~S of an integer."
                    (byte-size obj) (byte-position obj)
        ) )
        (LOAD-TIME-EVAL
          (format t #+DEUTSCH "eine Absicht der Evaluierung zur Ladezeit." ; ??
                    #+ENGLISH "a load-time evaluation promise." ; ??
        ) )
        (READ-LABEL
          (format t #+DEUTSCH "eine Markierung zur Auflösung von #~D#-Verweisen bei READ."
                    #+ENGLISH "a label used for resolving #~D# references during READ."
                    (logand (sys::address-of obj) '#,(ash most-positive-fixnum -1))
        ) )
        (FRAME-POINTER
          (format t #+DEUTSCH "ein Pointer in den Stack. Er zeigt auf:"
                    #+ENGLISH "a pointer into the stack. It points to:"
          )
          (sys::describe-frame obj)
        )
        (SYSTEM-INTERNAL
          (format t #+DEUTSCH "ein Objekt mit besonderen Eigenschaften."
                    #+ENGLISH "a special-purpose object."
        ) )
        (ADDRESS
          (format t #+DEUTSCH "eine Maschinen-Adresse."
                    #+ENGLISH "a machine address."
        ) )
        (t ; Structure
          (format t #+DEUTSCH "eine Structure vom Typ ~S."
                    #+ENGLISH "a structure of type ~S."
                    type
          )
          (let ((type (sys::%record-ref obj 0)))
            (when (consp type)
              (format t #+DEUTSCH "~%Als solche ist sie auch eine Structure vom Typ ~{~S~^, ~}."
                        #+ENGLISH "~%As such, it is also a structure of type ~{~S~^, ~}."
                        (cdr (nreverse (cons (cdr (last type)) (reverse type))))
        ) ) ) )
      )
      ; Array-Typen
      (let ((rank (array-rank obj))
            (eltype (array-element-type obj)))
        (format t #+DEUTSCH "ein~:[~; einfacher~] ~A-dimensionaler Array"
                  #+ENGLISH "a~:[~; simple~] ~R dimensional array"
                  (simple-array-p obj) rank
        )
        (when (eql rank 1)
          (format t #+DEUTSCH " (Vektor)"
                    #+ENGLISH " (vector)"
        ) )
        (unless (eq eltype 'T)
          (format t #+DEUTSCH " von ~:(~A~)s"
                    #+ENGLISH " of ~(~A~)s"
                    eltype
        ) )
        (when (adjustable-array-p obj)
          (format t #+DEUTSCH ", adjustierbar"
                    #+ENGLISH ", adjustable"
        ) )
        (when (plusp rank)
          (format t #+DEUTSCH ", der Größe ~{~S~^ x ~}"
                    #+ENGLISH ", of size ~{~S~^ x ~}"
                    (array-dimensions obj)
          )
          (when (array-has-fill-pointer-p obj)
            (format t #+DEUTSCH " und der momentanen Länge (Fill-Pointer) ~S"
                      #+ENGLISH " and current length (fill-pointer) ~S"
                      (fill-pointer obj)
        ) ) )
        (format t #+DEUTSCH "."
                  #+ENGLISH "."
      ) )
  ) )
  (when more
    (format t #+DEUTSCH "~%Mehr Information durch Auswerten von ~{~S~^ oder ~}."
              #+ENGLISH "~%For more information, evaluate ~{~S~^ or ~}."
              (nreverse more)
  ) )
  (values)
)

(defun describe-signature (req-anz opt-anz rest-p keyword-p keywords allow-other-keys)
  (format t #+DEUTSCH "~%Argumentliste: "
            #+ENGLISH "~%argument list: "
  )
  (format t "(~{~A~^ ~})"
    (let ((args '()) (count 0))
      (dotimes (i req-anz)
        (incf count)
        (push (format nil "ARG~D" count) args)
      )
      (when (plusp opt-anz)
        (push '&OPTIONAL args)
        (dotimes (i opt-anz)
          (incf count)
          (push (format nil "ARG~D" count) args)
      ) )
      (when rest-p
        (push '&REST args)
        (push "OTHER-ARGS" args)
      )
      (when keyword-p
        (push '&KEY args)
        (dolist (kw keywords) (push (prin1-to-string kw) args))
        (when allow-other-keys (push '&ALLOW-OTHER-KEYS args))
      )
      (nreverse args)
) ) )
;; DOCUMENTATION mit abfragen und ausgeben??
;; function, variable, type, structure, setf

; Gibt object in einen String aus, der nach Möglichkeit höchstens max Zeichen
; lang sein soll.
(defun write-to-short-string (object max)
  ; Methode: probiere
  ; level = 0: length = 0,1,2
  ; level = 1: length = 1,2,3,4
  ; level = 2: length = 2,...,6
  ; usw. bis maximal level = 16.
  ; Dabei level möglichst groß, und bei festem level length möglichst groß.
  (if (or (numberp object) (symbolp object)) ; von length und level unbeeinflußt?
    (write-to-string object)
    (macrolet ((minlength (level) `,level)
               (maxlength (level) `(* 2 (+ ,level 1))))
      ; Um level möglist groß zu bekommen, dabei length = minlength wählen.
      (let* ((level ; Binärsuche nach dem richtigen level
               (let ((level1 0) (level2 16))
                 (loop
                   (when (= (- level2 level1) 1) (return))
                   (let ((levelm (floor (+ level1 level2) 2)))
                     (if (<= (length (write-to-string object :level levelm :length (minlength levelm))) max)
                       (setq level1 levelm) ; levelm paßt, probiere größere
                       (setq level2 levelm) ; levelm paßt nicht, probiere kleinere
                 ) ) )
                 level1
             ) )
             (length ; Binärsuche nach dem richtigen length
               (let ((length1 (minlength level)) (length2 (maxlength level)))
                 (loop
                   (when (= (- length2 length1) 1) (return))
                   (let ((lengthm (floor (+ length1 length2) 2)))
                     (if (<= (length (write-to-string object :level level :length lengthm)) max)
                       (setq length1 lengthm) ; lengthm paßt, probiere größere
                       (setq length2 lengthm) ; lengthm paßt nicht, probiere kleinere
                 ) ) )
                 length1
            )) )
        (write-to-string object :level level :length length)
) ) ) )

;-------------------------------------------------------------------------------
;; DRIBBLE

(let ((dribble-file nil) (dribbled-input nil) (dribbled-output nil))
  (defun dribble (&optional file)
    (if file
      (progn
        (if dribble-file
          (warn #+DEUTSCH "Es wird bereits auf ~S protokolliert."
                #+ENGLISH "Already dribbling to ~S"
                #+FRANCAIS "Le protocole est déjà écrit sur ~S."
                dribble-file
          )
          (setq dribble-file (open file :direction :output)
                dribbled-input *standard-input*
                dribbled-output *standard-output*
                *standard-input* (make-echo-stream *standard-input* dribble-file)
                *standard-output* (make-broadcast-stream *standard-output* dribble-file)
        ) )
        dribble-file
      )
      (if dribble-file
        (prog2
          (setq *standard-input* dribbled-input
                *standard-output* dribbled-output
                dribbled-input nil
                dribbled-input nil
          )
          dribble-file
          (close dribble-file)
          (setq dribble-file nil)
        )
        (warn #+DEUTSCH "Es wird zur Zeit nicht protokolliert."
              #+ENGLISH "Currently not dribbling."
              #+FRANCAIS "Aucun protocole n'est couramment écrit."
) ) ) ) )

;-------------------------------------------------------------------------------
;; ED

;; *editor* und editor-tempfile sind in CONFIG.LSP definiert.
;; Hier stehen nur die Defaults.

;; Der Name des Editors:
(defparameter *editor* nil)

;; Das temporäre File, das LISP beim Editieren anlegt:
(defun editor-tempfile ()
  #+(or ATARI DOS) "LISPTEMP.LSP"
  #+OS/2 "lisptemp.lsp"
  #+AMIGA "T:lisptemp.lsp"
  #+(or UNIX VMS) (merge-pathnames "lisptemp.lsp" (user-homedir-pathname))
)

;; (edit-file file) editiert ein File.
(defun edit-file (file)
  (unless *editor*
    (error #+DEUTSCH "Kein externer Editor installiert."
           #+ENGLISH "No external editor installed."
           #+FRANCAIS "Un éditeur externe n'est pas installé."
  ) )
  #+ATARI
    (prog1
      (execute *editor* ; das ist der Name des Editors
               (namestring file t) ; file als String, im GEMDOS-Format
               (round (* 0.99 (gc))) ; Editor kriegt 99% des freien Speichers
      )
      (write-string (coerce '(#\Escape #\E) 'string) ; Bildschirm löschen
                    *terminal-io*
    ) )
  #+(or DOS OS/2)
    (execute *editor* ; das ist der Name des Editors
             (namestring file) ; file als String
    )
  #+UNIX
    (shell (format nil "~A ~A" *editor* (truename file)))
  #+AMIGA
    (execute (format nil "~A \"~A\"" *editor* (truename file)))
)

(defun ed (&optional arg &aux sym fun def)
  (if (null arg)
    (edit-file "")
    (if (or (pathnamep arg) (stringp arg))
      (edit-file arg)
      (if (and (cond ((symbolp arg) (setq sym arg) t)
                     ((functionp arg) (setq sym (sys::%record-ref arg 0)) t)
                     (t nil)
               )
               (fboundp sym)
               (or (setq fun (macro-function sym))
                   (setq fun (symbol-function sym))
               )
               (functionp fun)
               (not (compiled-function-p fun))
               (or (symbolp arg) (eql fun arg))
               (setq def (get sym 'sys::definition))
          )
        (let ((env (vector (sys::%record-ref fun 4) ; venv
                           (sys::%record-ref fun 5) ; fenv
                           (sys::%record-ref fun 6) ; benv
                           (sys::%record-ref fun 7) ; genv
                           (sys::%record-ref fun 8) ; denv
              )    )
              (tempfile (editor-tempfile)))
          (with-open-file (f tempfile :direction :output)
            (pprint def f)
            (terpri f) (terpri f)
          )
          (edit-file tempfile)
          (with-open-file (f tempfile :direction :input)
            (let ((*package* *package*) ; *PACKAGE* binden
                  (end-of-file "EOF")) ; einmaliges Objekt
              (loop
                (let ((obj (read f nil end-of-file)))
                  (when (eql obj end-of-file) (return))
                  (print (evalhook obj nil nil env))
          ) ) ) )
          sym
        )
        (error #+DEUTSCH "~S ist nicht editierbar."
               #+ENGLISH "~S cannot be edited."
               #+FRANCAIS "~S ne peut pas être édité."
               arg
) ) ) ) )

;-------------------------------------------------------------------------------

; speichert den momentanen Speicherinhalt unter Weglassen überflüssiger
; Objekte ab als LISPINIT.MEM
(defun saveinitmem ()
  (do-all-symbols (sym) (remprop sym 'sys::definition))
  (setq - nil + nil ++ nil +++ nil * nil ** nil *** nil / nil // nil /// nil)
  (savemem "lispinit.mem")
  (room)
)

;-------------------------------------------------------------------------------

; Vervollständigungs-Routine in Verbindung mit der GNU Readline-Library:
; Input: string die Eingabezeile, (subseq string start end) das zu vervoll-
; ständigende Textstück.
; Output: eine Liste von Simple-Strings. Leer, falls keine sinnvolle Vervoll-
; ständigung. Sonst CDR = Liste aller sinnvollen Vervollständigungen, CAR =
; sofortige Ersetzung.
#+(or UNIX DOS OS/2)
(defun completion (string start end)
  ; quotiert vervollständigen?
  (let ((start1 start) (quoted nil))
    (when (and (>= start 1) (member (char string (- start 1)) '(#\" #\|)))
      (decf start1) (setq quoted t)
    )
    (let (; Hilfsvariablen beim Sammeln der Symbole:
          knownpart ; Anfangsstück
          knownlen  ; dessen Länge
          (L '())   ; sammelnde Liste
         )
      (let ((gatherer
              (if ; Vervollständigung in funktionaler Position?
                (or (and (>= start1 1)
                         (equal (subseq string (- start1 1) start1) "(")
                    )
                    (and (>= start1 2)
                         (equal (subseq string (- start1 2) start1) "#'")
                )   )
                #'(lambda (sym)
                    (when (fboundp sym)
                      (let ((name (symbol-name sym)))
                        (when (and (>= (length name) knownlen) (string-equal name knownpart :end1 knownlen))
                          (push name L)
                  ) ) ) )
                #'(lambda (sym)
                    (let ((name (symbol-name sym)))
                      (when (and (>= (length name) knownlen) (string-equal name knownpart :end1 knownlen))
                        (push name L)
                  ) ) )
            ) )
            (package *package*)
            (mapfun #'sys::map-symbols)
            (prefix nil))
        ; Evtl. Packagenamen abspalten:
        (unless quoted
          (let ((colon (position #\: string :start start :end end)))
            (when colon
              (unless (setq package (find-package (string-upcase (subseq string start colon))))
                (return-from completion nil)
              )
              (incf colon)
              (if (and (< colon end) (eql (char string colon) #\:))
                (incf colon)
                (setq mapfun #'sys::map-external-symbols)
              )
              (setq prefix (subseq string start colon))
              (setq start colon)
        ) ) )
        (setq knownpart (subseq string start end))
        (setq knownlen (length knownpart))
        (funcall mapfun gatherer package)
        (when (null L) (return-from completion nil))
        (unless quoted
          (setq L (mapcar #'string-downcase L))
        )
        ; sortieren:
        (setq L (sort L #'string<))
        ; größtes gemeinsames Anfangsstück suchen:
        (let ((imax ; (reduce #'min (mapcar #'length L))
                (let ((i (length (first L))))
                  (dolist (s (rest L)) (setq i (min i (length s))))
                  i
             )) )
          (do ((i 0 (1+ i)))
              ((or (eql i imax)
                   (let ((c (char (first L) i)))
                     (dolist (s (rest L) nil) (unless (eql (char s i) c) (return t)))
               )   )
               (push (subseq (first L) 0 i) L)
        ) )   )
        ; Präfix wieder ankleben:
        (when prefix
          (mapl #'(lambda (l)
                    (setf (car l) (string-concat prefix (car l)))
                  )
                L
        ) )
        L
) ) ) )

;-------------------------------------------------------------------------------

#+ATARI
; Unsere eigene kleine "Shell" interpretiert das erste Wort als
; auszuführendes Programm, den Rest als Argumentzeile.
(defun myshell (command)
  (declare (string command))
  ; Whitespace zu Beginn der Zeile entfernen:
  (let ((index (position-if-not #'whitespacep command)))
    (unless index (return-from myshell))
    (unless (eql index 0) (setq command (subseq command index)))
  )
  ; Nun ist (char command 0) kein Whitespace.
  ; Aufspalten in Programm und Argumentzeile:
  (let* ((index (or (position-if #'whitespacep command) (length command)))
         (prog (subseq command 0 index))
         proglist
         (tail (subseq command
                       (or (position-if-not #'whitespacep command :start index)
                           (length command)
        ))     )       )
    (setq prog (pathname prog))
    (setq proglist
      (if (member :absolute (pathname-directory prog))
        ; relativer Pfadname -> muß Programm im PATH suchen:
        (let* ((pathstring (sys::getenv "PATH"))
               (pathlist ; pathstring an den Strichpunkten aufspalten
                 (and pathstring
                   (let ((i 0) (l '()))
                     (loop
                       (let ((j (position #\; pathstring :start i)))
                         (unless j (push (subseq pathstring i) l) (return))
                         (push (subseq pathstring i j) l)
                         (setq i (+ j 1))
                     ) )
                     (nreverse l)
              )) ) )
          (push "" pathlist) ; aktuelles Directory zuerst
          (setq pathlist (delete-duplicates pathlist :from-end t :test #'equal))
          (setq pathlist
            (mapcar #'(lambda (path)
                        (pathname
                          (if (and (plusp (length path))
                                   (not (eql (char path (1- (length path))) #\\))
                              )
                            (string-concat path "\\")
                            path
                      ) ) )
                    pathlist
          ) )
          (mapcar #'(lambda (path) (merge-pathnames prog path)) pathlist)
        )
        ; absoluter Pfadname -> brauche nicht zu suchen:
        (list prog)
    ) )
    ; Extensions ergänzen:
    (when (null (pathname-type prog))
      (setq proglist
        (mapcan #'(lambda (prog)
                    (list (merge-pathnames prog '#".prg")
                          (merge-pathnames prog '#".ttp")
                          (merge-pathnames prog '#".tos")
                  ) )
                proglist
    ) ) )
    ; Programm suchen:
    (setq prog (find-if #'probe-file proglist))
    (when prog
      (execute prog tail)
) ) )

