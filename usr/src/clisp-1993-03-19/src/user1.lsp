;;;; User-Interface, Teil 1
;;;; Eval-Env, Debugger, Stepper, Errors, Query-User
;;;; Bruno Haible 4.2.1990, 4.11.1991

(in-package "LISP")
(export '(the-environment eval-env with-keyboard *keyboard-input*))
(in-package "SYSTEM")

;-------------------------------------------------------------------------------
;;                       THE-ENVIRONMENT und EVAL-ENV

; THE-ENVIRONMENT wie in SCHEME
(defvar *COMPILING* nil)
(defun %the-environment (form env)
  (declare (ignore form))
  (setf (svref env 0) (svref (svref env 0) 2)) ; *evalhook*-Bindung streichen
  env
)
(defmacro the-environment ()
  (if *COMPILING*
    (error #+DEUTSCH "~S ist in compiliertem Code unmöglich."
           #+ENGLISH "~S is impossible in compiled code"
           #+FRANCAIS "~S est impossible dans du code compilé."
           'the-environment
    )
    `(let ((*evalhook* #'%the-environment)) 0)
) )

; Das Toplevel-Environment
(defparameter *toplevel-environment* (eval '(the-environment)))
(defparameter *toplevel-denv* (svref *toplevel-environment* 4))

; Evaluiert eine Form in einem Environment  
(defun eval-env (form &optional (env *toplevel-environment*))
  (evalhook form nil nil env)
)

;-------------------------------------------------------------------------------
;;                                 Debugger

(defvar *break-count* 0) ; Anzahl der aktiven Break-Schleifen (Fixnum >=0)

; Hauptschleife:
; (driver
;   #'(lambda () (read-eval-print "> "))
; )

; Help-Funktion:
(defvar *key-bindings* nil) ; Liste von Tasten-Bindungen und Helpstrings
(defun help ()
  (dolist (s (reverse (remove-if-not #'stringp *key-bindings*)))
    (write-string s #|*debug-io*|#)
) )

; Bausteine der Break-Schleife:
(defvar *debug-frame*)
(defvar *debug-mode*)
(defvar *frame-limit1* nil) ; untere Grenze für frame-down und frame-down-1
(defvar *frame-limit2* nil) ; obere Grenze für frame-up und frame-up-1
(defun frame-limit1 (frames-to-skip)
  (let ((frame (the-frame)))
    (let ((*frame-limit1* nil)
          (*frame-limit2* nil))
      (dotimes (i frames-to-skip) (setq frame (frame-up-1 frame 1)))
    )
    frame
) )
(defun frame-limit2 ()
  (let ((frame (the-frame)))
    (let ((*frame-limit1* nil)
          (*frame-limit2* nil))
      (loop
        (let ((nextframe (frame-up-1 frame 1)))
          (when (or (eq nextframe frame) (driver-frame-p nextframe)) (return))
          (setq frame nextframe)
      ) )
      (dotimes (i 2) (setq frame (frame-down-1 frame 1)))
    )
    frame
) )
(defun debug-help () (help) (throw 'debug 'continue))
(defun debug-unwind () (throw 'debug 'unwind))
(defun debug-mode-1 () (setq *debug-mode* 1) (throw 'debug 'continue))
(defun debug-mode-2 () (setq *debug-mode* 2) (throw 'debug 'continue))
(defun debug-mode-3 () (setq *debug-mode* 3) (throw 'debug 'continue))
(defun debug-mode-4 () (setq *debug-mode* 4) (throw 'debug 'continue))
(defun debug-mode-5 () (setq *debug-mode* 5) (throw 'debug 'continue))
(defun debug-where () (describe-frame *debug-frame*) (throw 'debug 'continue))
(defun debug-up ()
  (describe-frame
    (setq *debug-frame* (frame-up-1 *debug-frame* *debug-mode*))
  )
  (throw 'debug 'continue)
)
(defun debug-top ()
  (describe-frame
    (setq *debug-frame* (frame-up *debug-frame* *debug-mode*))
  )
  (throw 'debug 'continue)
)
(defun debug-down ()
  (describe-frame
    (setq *debug-frame* (frame-down-1 *debug-frame* *debug-mode*))
  )
  (throw 'debug 'continue)
)
(defun debug-bottom ()
  (describe-frame
    (setq *debug-frame* (frame-down *debug-frame* *debug-mode*))
  )
  (throw 'debug 'continue)
)
(defun debug-backtrace (&optional (mode *debug-mode*))
  (let ((frame (frame-down-1 (frame-up-1 *frame-limit1* mode) mode)))
    (loop
      (describe-frame frame)
      (when (eq frame (setq frame (frame-up-1 frame mode))) (return))
  ) )
  (throw 'debug 'continue)
)
(defun debug-backtrace-1 () (debug-backtrace 1))
(defun debug-backtrace-2 () (debug-backtrace 2))
(defun debug-backtrace-3 () (debug-backtrace 3))
(defun debug-backtrace-4 () (debug-backtrace 4))
(defun debug-backtrace-5 () (debug-backtrace 5))
(defun debug-redo ()
  (redo-eval-frame *debug-frame*)
  (throw 'debug 'continue)
)
(defun debug-return ()
  (return-from-eval-frame *debug-frame*
    (read-form #+DEUTSCH "Werte: "
               #+ENGLISH "values: "
               #+FRANCAIS "Valeurs : "
  ) )
  (throw 'debug 'continue)
)
(defun debug-continue () (throw 'debug 'quit))

#+ATARI (progn
(defconstant commands0
             (list
               #+DEUTSCH "
Help        = diese Sondertasten-Liste
Backspace     ein Zeichen nach links löschen
Delete        ein Zeichen nach rechts löschen
Insert        eine Leerstelle einfügen
             Cursor ein Zeichen nach links
             Cursor ein Zeichen nach rechts
Shift-       Cursor an den Zeilenanfang
Shift-       Cursor ans Zeilenende
Return, Enter beendet das Editieren dieser Zeile"
               #+ENGLISH "
Help        = this key list
Backspace     deletes one character to the left
Delete        deletes one character to the right
Insert        inserts a space
             moves the cursor one character to the left
             moves the cursor one character to the right
Shift-       moves the cursor to the beginning of the line
Shift-       moves the cursor to the end of the line
Return, Enter finishes editing of this line"
               #+FRANCAIS "
Help        = cette liste de touches spéciales
Backspace     effacer un caractère vers la gauche
Delete        effacer un caractère vers la droite
Insert        ajouter un espace
             Cursor vers la gauche
             Cursor vers la droite
Shift-       Cursor au début de la ligne
Shift-       Cursor à la fin de la ligne
Return, Enter finit les changements de cette ligne"
               (cons #\Help   #'debug-help  )
)            )
(defconstant commands1
             (list
               #+DEUTSCH "
Help   = dieses Menü
Undo   = Abbruch, Rücksprung zur nächsthöheren Eingabeschleife
F1     = alle Stack-Elemente inspizieren
F2     = alle Frames inspizieren
F3     = nur EVAL- und APPLY-Frames inspizieren (Default)
F4     = nur APPLY-Frames inspizieren
.      = diesen Frame inspizieren
      = nächsthöheren Frame inspizieren
Shift = obersten Frame inspizieren
      = nächstneueren Frame inspizieren
Shift = neuesten Frame inspizieren
ShiftF1= alle Stack-Elemente auflisten
ShiftF2= alle Frames auflisten
ShiftF3= alle EVAL- und APPLY-Frames auflisten
ShiftF4= alle APPLY-Frames auflisten
F5     = Redo: Form im EVAL-Frame erneut auswerten
F6     = Return: EVAL-Frame mit gegebenen Werten verlassen"
               #+ENGLISH "
Help   = this command list
Undo   = abort to the next recent input loop
F1     = inspect all the stack elements
F2     = inspect all the frames
F3     = inspect only EVAL and APPLY frames (default)
F4     = inspect only APPLY frames
.      = inspect this frame
      = go up one frame, inspect it
Shift = go to top frame, inspect it
      = go down one frame, inspect it
Shift = go to bottom (most recent) frame, inspect it
ShiftF1= list all stack elements
ShiftF2= list all frames
ShiftF3= list all EVAL and APPLY frames
ShiftF4= list all APPLY frames
F5     = redo: re-evaluate form in EVAL frame
F6     = return: leave EVAL frame, prescribing the return values"
               #+FRANCAIS "
Help   = ce menu-ci
Undo   = arrêt, retour au niveau supérieur
F1     = examiner tous les éléments de la pile
F2     = examiner tous les «frames»
F3     = examiner uniquement les «frames» EVAL et APPLY (par défaut)
F4     = examiner uniquement les «frames» APPLY
.      = examiner ce «frame»
      = examiner un «frame» supérieur
Shift = examiner le «frame» le plus élevé
      = examiner un prochain «frame» plus récent (inférieur)
Shift = examiner le «frame» le plus récent (le plus bas)
ShiftF1= montrer tous les éléments de la pile
ShiftF2= montrer tous les «frames»
ShiftF3= montrer tous les «frames» EVAL et APPLY
ShiftF4= montrer tous les «frames» APPLY
F5     = Redo: réévaluer la forme dans le «frame» EVAL
F6     = Return: quitter le «frame» EVAL avec certaines valeurs"
               (cons #\Help   #'debug-help  )
               (cons #\?      #'debug-help  )
               (cons #\Undo   #'debug-unwind)
               (cons #\F1     #'debug-mode-1)
               (cons #\F2     #'debug-mode-2)
               (cons #\F3     #'debug-mode-4)
               (cons #\F4     #'debug-mode-5)
               (cons #\.      #'debug-where )
               (cons #\Up     #'debug-up    )
               (cons #\S-Up   #'debug-top   )
               (cons #\Down   #'debug-down  )
               (cons #\S-Down #'debug-bottom)
               (cons #\S-F1   #'debug-backtrace-1)
               (cons #\S-F2   #'debug-backtrace-2)
               (cons #\S-F3   #'debug-backtrace-4)
               (cons #\S-F4   #'debug-backtrace-5)
)            )
(defconstant commands2
             (list
               (cons #\F5     #'debug-redo  )
               (cons #\F6     #'debug-return)
)            )
(defconstant commands3
             (list
               #+DEUTSCH "
F10    = Continue: Rest weiter abarbeiten"
               #+ENGLISH "
F10    = continue: continue evaluation"
               #+FRANCAIS "
F10    = Continue: continuer l'évaluation"
               (cons #\F10  #'debug-continue)
)            )
)
#-ATARI (progn
(defconstant commands0
             (list
               #+DEUTSCH "
Help = diese Liste
Benutzen Sie die üblichen Editiermöglichkeiten."
               #+ENGLISH "
Help = this list
Use the usual editing capabilities."
               #+FRANCAIS "
Help = cette liste
Éditez de la façon habituelle."
               (cons "Help"   #'debug-help  )
)            )
(defconstant commands1
             (list
               #+DEUTSCH "
Help   = dieses Menü
Abort  = Abbruch, Rücksprung zur nächsthöheren Eingabeschleife
Unwind = Abbruch, Rücksprung zur nächsthöheren Eingabeschleife
Mode-1 = alle Stack-Elemente inspizieren
Mode-2 = alle Frames inspizieren
Mode-3 = nur lexikalische Frames inspizieren
Mode-4 = nur EVAL- und APPLY-Frames inspizieren (Default)
Mode-5 = nur APPLY-Frames inspizieren
Where  = diesen Frame inspizieren
Up     = nächsthöheren Frame inspizieren
Top    = obersten Frame inspizieren
Down   = nächstneueren Frame inspizieren
Bottom = neuesten Frame inspizieren
Backtrace-1 = alle Stack-Elemente auflisten
Backtrace-2 = alle Frames auflisten
Backtrace-3 = alle lexikalische Frames auflisten
Backtrace-4 = alle EVAL- und APPLY-Frames auflisten
Backtrace-5 = alle APPLY-Frames auflisten
Backtrace   = Stack auflisten im aktuellen Mode
Redo   = Form im EVAL-Frame erneut auswerten
Return = EVAL-Frame mit gegebenen Werten verlassen"
               #+ENGLISH "
Help   = this command list
Abort  = abort to the next recent input loop
Unwind = abort to the next recent input loop
Mode-1 = inspect all the stack elements
Mode-2 = inspect all the frames
Mode-3 = inspect only lexical frames
Mode-4 = inspect only EVAL and APPLY frames (default)
Mode-5 = inspect only APPLY frames
Where  = inspect this frame
Up     = go up one frame, inspect it
Top    = go to top frame, inspect it
Down   = go down one frame, inspect it
Bottom = go to bottom (most recent) frame, inspect it
Backtrace-1 = list all stack elements
Backtrace-2 = list all frames
Backtrace-3 = list all lexical frames
Backtrace-4 = list all EVAL and APPLY frames
Backtrace-5 = list all APPLY frames
Backtrace   = list stack in current mode
Redo   = re-evaluate form in EVAL-Frame
Return = leave EVAL-Frame, prescribing the return values"
               #+FRANCAIS "
Help   = ce menu-ci
Abort  = arrêt, retour au niveau supérieur
Unwind = arrêt, retour au niveau supérieur
Mode-1 = examiner tous les éléments de la pile
Mode-2 = examiner tous les «frames»
Mode-3 = examiner uniquement les «frames» lexicaux
Mode-4 = examiner uniquement les «frames» EVAL et APPLY (par défaut)
Mode-5 = examiner uniquement les «frames» APPLY
Where  = examiner ce «frame»
Up     = examiner un «frame» supérieur
Top    = examiner le «frame» le plus élevé
Down   = examiner un prochain «frame» plus récent (inférieur)
Bottom = examiner le «frame» le plus récent (le plus bas)
Backtrace-1 = montrer tous les éléments de la pile
Backtrace-2 = montrer tous les «frames»
Backtrace-3 = montrer tous les «frames» lexicaux
Backtrace-4 = montrer tous les «frames» EVAL et APPLY
Backtrace-5 = montrer tous les «frames» APPLY
Backtrace   = montrer la pile en mode actuel
Redo   = réévaluer la forme dans le «frame» EVAL
Return = quitter le «frame» EVAL avec certaines valeurs"
               (cons "Help"   #'debug-help  )
               (cons "?"      #'debug-help  )
               (cons "Abort"  #'debug-unwind)
               (cons "Unwind" #'debug-unwind)
               (cons "Mode-1" #'debug-mode-1)
               (cons "Mode-2" #'debug-mode-2)
               (cons "Mode-3" #'debug-mode-3)
               (cons "Mode-4" #'debug-mode-4)
               (cons "Mode-5" #'debug-mode-5)
               (cons "Where"  #'debug-where )
               (cons "Up"     #'debug-up    )
               (cons "Top"    #'debug-top   )
               (cons "Down"   #'debug-down  )
               (cons "Bottom" #'debug-bottom)
               (cons "Backtrace-1" #'debug-backtrace-1)
               (cons "Backtrace-2" #'debug-backtrace-2)
               (cons "Backtrace-3" #'debug-backtrace-3)
               (cons "Backtrace-4" #'debug-backtrace-4)
               (cons "Backtrace-5" #'debug-backtrace-5)
               (cons "Backtrace"   #'debug-backtrace  )
)            )
(defconstant commands2
             (list
               (cons "Redo"   #'debug-redo  )
               (cons "Return" #'debug-return)
)            )
(defconstant commands3
             (list
               #+DEUTSCH "
Continue = Rest weiter abarbeiten"
               #+ENGLISH "
Continue = continue evaluation"
               #+FRANCAIS "
Continue = continuer l'évaluation"
               (cons "Continue" #'debug-continue)
)            )
)

;; um Help-Kommando erweiterte Hauptschleife.
(defun main-loop ()
  (setq *break-count* 0)
  (driver ; Driver-Frame aufbauen und folgende Funktion (endlos) ausführen:
    #'(lambda ()
        (catch 'debug ; die (throw 'debug ...) abfangen
          (if ; Eingabezeile verlangen
              (read-eval-print "> " (copy-list commands0))
            ; T -> #<EOF>
            (exit)
            ; NIL -> Form bereits ausgewertet und ausgegeben
) )   ) ) )
(setq *driver* #'main-loop)

;; komfortable Break-Schleife. (Läuft nur in compiliertem Zustand!)
(defun break-loop (continuable)
  (tagbody
    (let* ((*break-count* (1+ *break-count*))
           (stream (make-synonym-stream '*debug-io*))
           (*standard-input* stream)
           (*standard-output* stream)
           (prompt (with-output-to-string (s)
                      (write *break-count* :stream s)
                      (write-string ". Break" s)
                      (write-string "> " s)
           )       )
           (*frame-limit1* (frame-limit1 12))
           (*frame-limit2* (frame-limit2))
           (*debug-mode* 4)
           (*debug-frame* (frame-down-1 (frame-up-1 *frame-limit1* *debug-mode*) *debug-mode*))
          )
      (driver ; Driver-Frame aufbauen und folgende Funktion (endlos) ausführen:
        #'(lambda ()
            (case
                (catch 'debug ; die (throw 'debug ...) abfangen und analysieren
                  (same-env-as *debug-frame* ; bei *debug-frame* gültiges Environment aufbauen
                    #'(lambda ()
                        (if ; Eingabezeile verlangen
                            (read-eval-print prompt
                              (nconc (copy-list commands1)
                                     (when (eval-frame-p *debug-frame*) (copy-list commands2))
                                     (when continuable (copy-list commands3))
                            ) )
                          ; T -> #<EOF>
                          #|(throw 'debug 'quit)|# (go quit)
                          ; NIL -> Form bereits ausgewertet und ausgegeben
                          #|(throw 'debug 'continue)|#
                ) )   ) )
              (unwind (go unwind))
              (quit (go quit)) ; nur erreicht, falls continuable
              (t ) ; alles andere, insbesondere continue
    ) )   ) )
    unwind (unwind-to-driver)
    quit
) )
(setq *break-driver* #'break-loop)

;-------------------------------------------------------------------------------
;;        komfortabler Stepper. (Läuft nur in compiliertem Zustand!)

(defvar *step-level* 0) ; momentane Step-Tiefe
(defvar *step-quit* most-positive-fixnum) ; kritische Step-Tiefe:
  ; sobald diese unterschritten wird, wacht der Stepper wieder auf.
(defvar *step-watch* nil) ; Abbruchbedingung

; (STEP form), CLTL S. 441
(defmacro step (form)
  `(let* ((*step-level* 0)
          (*step-quit* most-positive-fixnum)
          (*step-watch* nil)
          (*evalhook* #'step-hook-fn))
     ,form
   )
)

#+ATARI
(defconstant commands4
             (list
               #+DEUTSCH "
F7     = Step into form: diese Form im Einzelschrittmodus ausführen
F8     = Step over form: diese Form auf einmal ausführen
F9     = Step over this level: bis zum Aufrufer auf einmal ausführen
F10    = Continue: Einzelschrittmodus abschalten, Rest ausführen
Shift F7-F10: dito, jedoch mit Angabe einer Abbruchbedingung"
               #+ENGLISH "
F7     = step into form: evaluate this form in single step mode
F8     = step over form: evaluate this form at once
F9     = step over this level: evaluate at once up to the next return
F10    = continue: switch off single step mode, continue evaluation
Shift F7-F10: same as above, specify a condition when to stop"
               (cons #\F7    #'(lambda () (throw 'stepper 'into)))
               (cons #\F8    #'(lambda () (throw 'stepper 'over)))
               (cons #\F9    #'(lambda () (throw 'stepper 'over-this-level)))
               (cons #\F10   #'(lambda () (throw 'stepper 'continue)))
               (cons #\S-F7  #'(lambda () (throw 'stepper (values 'into t))))
               (cons #\S-F8  #'(lambda () (throw 'stepper (values 'over t))))
               (cons #\S-F9  #'(lambda () (throw 'stepper (values 'over-this-level t))))
               (cons #\S-F10 #'(lambda () (throw 'stepper (values 'continue t))))
)            )
#-ATARI
(defconstant commands4
             (list
               #+DEUTSCH "
Step     = Step into form: diese Form im Einzelschrittmodus ausführen
Next     = Step over form: diese Form auf einmal ausführen
Over     = Step over this level: bis zum Aufrufer auf einmal ausführen
Continue = Einzelschrittmodus abschalten, Rest ausführen
Step-until, Next-until, Over-until, Continue-until:
           dito, jedoch mit Angabe einer Abbruchbedingung"
               #+ENGLISH "
Step     = step into form: evaluate this form in single step mode
Next     = step over form: evaluate this form at once
Over     = step over this level: evaluate at once up to the next return
Continue = switch off single step mode, continue evaluation
Step-until, Next-until, Over-until, Continue-until:
           same as above, specify a condition when to stop"
               (cons "Step"     #'(lambda () (throw 'stepper 'into)))
               (cons "Next"     #'(lambda () (throw 'stepper 'over)))
               (cons "Over"     #'(lambda () (throw 'stepper 'over-this-level)))
               (cons "Continue" #'(lambda () (throw 'stepper 'continue)))
               (cons "Step-until"     #'(lambda () (throw 'stepper (values 'into t))))
               (cons "Next-until"     #'(lambda () (throw 'stepper (values 'over t))))
               (cons "Over-until"     #'(lambda () (throw 'stepper (values 'over-this-level t))))
               (cons "Continue-until" #'(lambda () (throw 'stepper (values 'continue t))))
)            )

(defun step-values (values)
  (let ((*standard-output* *debug-io*))
    (terpri #|*debug-io*|#)
    (write-string #+DEUTSCH "Step "
                  #+ENGLISH "step "
                  #|*debug-io*|#
    )
    (write *step-level* #|:stream *debug-io*|#)
    (write-string " ==> " #|*debug-io*|#)
    (case (length values)
      (0 (write-string #+DEUTSCH "Keine Werte"
                       #+ENGLISH "no values"
                       #|*debug-io*|#
      )  )
      (1 (write-string #+DEUTSCH "Wert: "
                       #+ENGLISH "value: "
                       #|*debug-io*|#
         )
         (write (car values) #|:stream *debug-io*|#)
      )
      (t (write (length values) #|:stream *debug-io*|#)
         (write-string #+DEUTSCH " Werte: "
                       #+ENGLISH " values: "
                       #|*debug-io*|#
         )
         (do ((L values))
             ((endp L))
           (write (pop L) #|:stream *debug-io*|#)
           (unless (endp L) (write-string ", " #|*debug-io*|#))
      )  )
  ) )
  (values-list values)
)

(defun step-hook-fn (form &optional (env *toplevel-environment*))
  (let ((*step-level* (1+ *step-level*)))
    (when (>= *step-level* *step-quit*) ; Solange *step-level* >= *step-quit*
      (if (and *step-watch* (funcall *step-watch*)) ; und kein Breakpoint,
        (setq *step-quit* most-positive-fixnum)
        (return-from step-hook-fn ; ist der Stepper passiv
          (evalhook form nil nil env) ; (d.h. er evaluiert die Form einfach)
    ) ) )
    (tagbody
      (let* ((stream (make-synonym-stream '*debug-io*))
             (*standard-input* stream)
             (*standard-output* stream)
             (prompt (with-output-to-string (s)
                       (write-string "Step " s)
                       (write *step-level* :stream s)
                       (write-string "> " s)
             )       )
             (*frame-limit1* (frame-limit1 11))
             (*frame-limit2* (frame-limit2))
             (*debug-mode* 4)
             (*debug-frame* (frame-down-1 (frame-up-1 *frame-limit1* *debug-mode*) *debug-mode*))
            )
        (fresh-line #|*debug-io*|#)
        (write-string #+DEUTSCH "Step "
                      #+ENGLISH "step "
                      #|*debug-io*|#
        )
        (write *step-level* #|:stream *debug-io*|#)
        (write-string " --> " #|*debug-io*|#)
        (write form #|:stream *debug-io*|# :length 4 :level 3)
        (loop
          (multiple-value-bind (what watchp)
            (catch 'stepper ; die (throw 'stepper ...) abfangen und analysieren
              (driver ; Driver-Frame aufbauen und folgende Funktion endlos ausführen:
                #'(lambda ()
                    (case
                        (catch 'debug ; die (throw 'debug ...) abfangen und analysieren
                          (same-env-as *debug-frame* ; bei *debug-frame* gültiges Environment aufbauen
                            #'(lambda ()
                                (if ; Eingabezeile verlangen
                                    (read-eval-print prompt
                                      (nconc (copy-list commands1)
                                             (when (eval-frame-p *debug-frame*) (copy-list commands2))
                                             (copy-list commands4)
                                    ) )
                                  ; T -> #<EOF>
                                  (go continue)
                                  ; NIL -> Form bereits ausgewertet und ausgegeben
                                  #|(throw 'debug 'continue)|#
                        ) )   ) )
                      (unwind (go unwind))
                      (t ) ; alles andere, insbesondere continue
            ) )   ) )
            (when watchp
              (let ((form (read-form #+DEUTSCH "Abbruchbedingung: "
                                     #+ENGLISH "condition when to stop: "
                   ))     )
                (setq *step-watch* ; Funktion, die 'form' bei *debug-frame* auswertet
                  (eval-at *debug-frame* `(function (lambda () ,form)))
            ) ) )
            (case what
              (into (go into))
              (over (go over))
              (over-this-level (go over-this-level))
              (continue (go continue))
            )
      ) ) )
      unwind
        (unwind-to-driver)
      into
        (return-from step-hook-fn
          (step-values
            (multiple-value-list (evalhook form #'step-hook-fn nil env))
        ) )
      over-this-level
        (setq *step-quit* *step-level*) ; Stepper in Schlafzustand schalten
      over
        (return-from step-hook-fn
          (step-values
            (multiple-value-list (evalhook form nil nil env))
        ) )
      continue
        (setq *step-quit* 0)
        (go over)
) ) )

;-------------------------------------------------------------------------------
;;                                  Errors

; *ERROR-HANDLER* sollte NIL oder eine Funktion sein, die übergeben bekommt:
; - NIL (bei ERROR) bzw. continue-format-string (bei CERROR),
; - error-format-string,
; - Argumente dazu,
; und die nur zurückkehren sollte, falls das erstere /=NIL ist.
(defvar *error-handler* nil)

; (CERROR continue-format-string error-format-string {arg}*), CLTL S. 430
(defun cerror (continue-format-string error-format-string &rest args)
  (if *error-handler*
    (apply *error-handler*
           (or continue-format-string t) error-format-string args
    )
    (progn
      (terpri *error-output*)
      (write-string "** - Continuable Error" *error-output*)
      (terpri *error-output*)
      (apply #'format *error-output* error-format-string args)
      (terpri *error-output*)
      (if (interactive-stream-p *debug-io*)
        (progn
          #+ATARI (write-string #+DEUTSCH "Wenn Sie (mit F10) fortfahren: "
                                #+ENGLISH "If you continue (by pressing F10): "
                                *error-output*
                  )
          #-ATARI (write-string #+DEUTSCH "Wenn Sie (mit Continue) fortfahren: "
                                #+ENGLISH "If you continue (by typing 'continue'): "
                                *error-output*
                  )
          (apply #'format *error-output* continue-format-string args)
          (funcall *break-driver* t)
        )
        (apply #'format *error-output* continue-format-string args)
  ) ) )
  nil
)

(defvar *break-on-warnings* nil)
; (WARN format-string {arg}*), CLTL S. 432
(defun warn (format-string &rest args)
  (terpri *error-output*)
  (write-string #+DEUTSCH "WARNUNG:"
                #+ENGLISH "WARNING:"
                *error-output*
  )
  (terpri *error-output*)
  (apply #'format *error-output* format-string args)
  (when *break-on-warnings* (funcall *break-driver* t))
  nil
)

; (BREAK [format-string {arg}*]), CLTL S. 432
(defun break (&optional (format-string "*** - Break") &rest args)
  (terpri *error-output*)
  (apply #'format *error-output* format-string args)
  (funcall *break-driver* t)
  nil
)

;-------------------------------------------------------------------------------
;;                            Querying the user

; (Y-OR-N-P [format-string {arg}*]), CLTL S. 407
(defun y-or-n-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string #+DEUTSCH " (j/n) "
                  #+ENGLISH " (y/n) "
                  *query-io*
  ) )
  (loop
    (let ((line (string-left-trim " " (read-line *query-io*))))
      (when (plusp (length line))
        (case (char-upcase (char line 0))
          (#\N (return nil))
          ((#\J #\Y) (return t))
    ) ) )
    (terpri *query-io*)
    (write-string #+DEUTSCH "Bitte mit j oder n antworten: "
                  #+ENGLISH "Please answer with y or n : "
                  *query-io*
) ) )

; (YES-OR-NO-P [format-string {arg}*]), CLTL S. 408
(defun yes-or-no-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string #+DEUTSCH " (ja/nein) "
                  #+ENGLISH " (yes/no) "
                  *query-io*
  ) )
  (loop
    (clear-input *query-io*)
    (let* ((line (string-trim " " (read-line *query-io*)))
           (h (assoc line '(("ja" . t) ("nein" . nil) ("yes" . t) ("no" . nil))
                          :test #'string-equal
          ))  )
      (when h (return (cdr h)))
    )
    (terpri *query-io*)
    (write-string #+DEUTSCH "Bitte mit ja oder nein antworten: "
                  #+ENGLISH "Please answer with yes or no : "
                  *query-io*
) ) )

#-AMIGA
(progn
  (defvar *keyboard-input*)
  (defmacro with-keyboard (&body body)
    #+(or ATARI DOS OS/2) ; *keyboard-input* existiert schon
      `(PROGN ,@body)
    #+UNIX
      `(UNWIND-PROTECT
         (PROGN
           (SYS::TERMINAL-RAW *TERMINAL-IO* T)
           ,@body
         )
         (SYS::TERMINAL-RAW *TERMINAL-IO* NIL)
       )
  )
)

