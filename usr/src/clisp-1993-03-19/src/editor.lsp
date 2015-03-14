(in-package "LISP")
(export '(editor ed *use-ed*))
(pushnew 'editor *features*)
#+(or DOS OS/2) (eval-when (compile load eval) (pushnew 'dose *features*))
(in-package "EDITOR")

;###############################################################################
;;;; Screen-Verwaltung, dritte Version
;;;;
;;;; Michael Stoll, Februar 1992
;;;; Bruno Haible, Mai 1992
;;;;
;;;; Spezifikation siehe SCREEN2.DOC

(defvar *window*) ; aktuelles Ausgabefenster
(defvar global-screen-height) ; Höhe des Fensters
(defvar global-screen-width)  ; Breite des Fensters
(defvar blanks) ; Array voller Spaces

(defmacro with-window (&body body)
  `(LET ((*WINDOW* (SYS::MAKE-WINDOW)))
     (UNWIND-PROTECT
       (MULTIPLE-VALUE-BIND (GLOBAL-SCREEN-HEIGHT GLOBAL-SCREEN-WIDTH) (SYS::WINDOW-SIZE *WINDOW*)
         (LET ((BLANKS (MAKE-STRING GLOBAL-SCREEN-WIDTH :INITIAL-ELEMENT #\SPACE)))
           ,@body
       ) )
       (CLOSE *WINDOW*)
   ) )
)

;;; Zunächst einige Macros zur Bildschirmsteuerung

(defmacro bell () `(WRITE-CHAR #\Bell *TERMINAL-IO*))

(defmacro screen-set-cursor (lin col)
  `(SYS::SET-WINDOW-CURSOR-POSITION *WINDOW* ,lin ,col)
)

(defmacro screen-home ()
  `(SCREEN-SET-CURSOR 0 0)
)

(defmacro screen-clear-screen ()
  `(SYS::CLEAR-WINDOW *WINDOW*)
)

(defmacro screen-clear-end-of-screen ()
  `(SYS::CLEAR-WINDOW-TO-EOT *WINDOW*)
)

(defmacro screen-clear-end-of-line ()
  `(SYS::CLEAR-WINDOW-TO-EOL *WINDOW*)
)

(defmacro screen-insert-line ()
  `(SYS::INSERT-WINDOW-LINE *WINDOW*)
)

(defmacro screen-delete-line ()
  `(SYS::DELETE-WINDOW-LINE *WINDOW*)
)

(defmacro screen-cursor-on ()
  `(SYS::WINDOW-CURSOR-ON *WINDOW*)
)

(defmacro screen-cursor-off ()
  `(SYS::WINDOW-CURSOR-OFF *WINDOW*)
)

(defmacro screen-reverse-on ()
  `(SYS::HIGHLIGHT-ON *WINDOW*)
)

(defmacro screen-reverse-off ()
  `(SYS::HIGHLIGHT-OFF *WINDOW*)
)

;-------------------------------------------------------------------------------

;;; Es werden drei Arten von Koordinaten verwendet:
;;; (Immer zuerst Zeile, dann Spalte)
;;;
;;; (a) Bildschirmkoordinaten
;;; =========================
;;;    Sie bezeichnen den Ort auf dem Bildschirm. Der erlaubte Bereich ist
;;;    [0..global-screen-height[ x [0..global-screen-width[. Dabei ist Zeile 0
;;;    die oberste Zeile, Spalte 0 die linkeste Spalte.
;;;
;;; (b) Fensterkoordinaten
;;; ======================
;;;    Sie beziehen sich jeweils auf ein Fenster. Der Ursprung ist dabei die
;;;    linke obere Ecke des Fensterinneren (d.h. ohne Rahmen). Der erlaubte
;;;    Bereich ist für ein Fenster screen im Falle, daß es nicht der ganze
;;;    Bildschirm ist (d.h. screen.full? = nil)
;;;    [-1..screen.height+1[ x [-1..screen.width+1[, wobei die Randwerte sich
;;;    auf Orte im Rahmen beziehen. Umfaßt das Fenster den ganzen Bildschirm,
;;;    sind die Fensterkoordinaten mit den Bildschirmkoordinaten identisch.
;;;
;;; (c) Textkoordinaten
;;; ===================
;;;    Sie beziehen sich auf den Text, der in einem Fenster dargestellt wird.
;;;    Die Zeilenkoordinate läuft im Bereich [0..length(screen.text)[, die
;;;    zur Zeilenkoordineate lin gehörige Spaltenkoordinate läuft im Bereich
;;;    [0..length(screen.text[lin])[ (manchmal auch einschließlich der rechten
;;;    Grenze).
;;;
;;; Umrechnung:
;;; ===========
;;; (a) -> (b):
;;;   (lin, col) --> (lin - screen.phys-top-lin, col - screen.phys-left-col)
;;; (b) -> (c):
;;;   (lin, col) --> (lin + screen.top-lin, col + screen.left-col)

;-------------------------------------------------------------------------------

;;; Datenstrukturen für Screens

;; Eine ZEILE ist ein String, adjustable mit Fill-pointer.

;; Liefert neue Zeile der Größe >= size und der Länge size
(defun get-new-line (size)
  (make-array size
              :element-type 'string-char
              :adjustable t :fill-pointer size
) )

;; Ein TEXT ist ein Push-Vektor von Zeilen.
(defun make-empty-text (&optional (len global-screen-height))
  (let ((text (make-array len :adjustable t :fill-pointer 0)))
    (vector-push (get-new-line 0) text)
    text
) )

;; Eine MARKE besteht aus zwei Integers >= 0 (Zeile, Spalte)
(defmacro make-mark (lin col) `(CONS ,lin ,col))
(defmacro mark-lin (mark) `(CAR ,mark))
(defmacro mark-col (mark) `(CDR ,mark))

;; Die Marke (lin,col) heißt für den Text text GÜLTIG, wenn gilt
;; 0 <= lin < length(text), 0 <= col <= length(text[lin])
;; (Marken sind immer in Textkoordinaten angegeben.)

;; Ein SCREEN besteht u.a. aus einem Text mit Cursorposition und Marken, sowie
;; Angaben über den Fensterausschnitt und die physikalische Lage auf dem Schirm
(defstruct (screen (:copier nil) (:constructor mk-screen))
  (text (make-empty-text))    ; Text des Screens
  (lin 0 :type integer)       ; Cursorzeile
  (col 0 :type integer)       ; Cursorspalte, (lin,col) ist für den Text gültig
  (saved-col 0 :type integer) ; gemerkte Spalte
  (marks (make-array 12 :adjustable t :fill-pointer 12 :initial-element nil))
    ; Vektor von Marken, die für den Text gültig sind, oder NIL; Länge >= 12.
    ; Die ersten beiden bestimmen den markierten Block.
  (height global-screen-height :type integer) ; Höhe des Bildausschnitts
  (width  global-screen-width  :type integer) ; Breite des Bildausschnitts
  (top-lin  0 :type integer) ; Index der obersten Zeile im Fenster
  (left-col 0 :type integer) ; Index der linkesten Spalte im Fenster
                             ; (Textkoordinaten)
  (visibility nil :type vector) ; Vektor von Listen von Conses: Zu jeder Zeile
                                ;  die sichtbaren Abschnitte
  (full? t)                  ; Flag, ob ganzer Schirm
  (phys-left-col 0 :type integer) ; physikalische Koordinaten der linken oberen
  (phys-top-lin  0 :type integer) ; Fensterecke (ohne Rahmen)
                                  ; (Bildschirmkoordinaten)
  (title "" :type string)    ; Titel, nur wenn nicht full?
)

;; Bedingungen:
;; 0 <= top-lin < length(text)
;; 0 <= left-col

;; 0 <= phys-left-col
;; phys-left-col + width <= global-screen-width
;; 0 <= phys-top-lin
;; phys-top-lin + height <= global-screen-height
;; Falls not full?: jeweils < statt <=

;; visibility ist ein Vektor der Länge height + 2, Einträge sind Listen
;; ((l_1 . r_1) (l_2 . r_2) ... (l_n . r_n)) mit
;; -1 <= l_1 < r_1 < l_2 < r_2 < ... < l_n < r_n <= width + 1.
;; Bedeutung der Liste visibility[i]: Von Zeile i-1 (Zeile -1 ist die
;; Titelzeile, Zeile height die untere Rahmenzeile, analog für Spalten
;; -1, width; das sind Fensterkoordinaten) sind die Abschnitte
;; [l_1..r_1[, [l_2..r_2[, ..., [l_n..r_n[ sichtbar.

;; make-screen erzeugt einen Screen. Ohne Argumente erhält man einen Screen,
;; der den ganzen Bildschirm umfaßt, ansonsten einen mit Rahmen.
(defun make-screen (&key height width left-col top-lin title)
  (if (or height width left-col top-lin title)
    ;; wenigstens ein Argument angegeben
    (let ((min-height 1) (min-width 10))
      (setq height
            (max min-height ; Höhe in den erlaubten Bereich bringen (>= min-height)
              (if height
                (min height (- global-screen-height 2))
                ;; Default: Zwei Drittel der Bildschirmhöhe
                (- (floor (* global-screen-height 0.67s0)) 2)
      )     ) )
      (setq width
            (max min-width ; Breite in den erlaubten Bereich bringen (>= min-width)
              (if width
                (min width (- global-screen-width 2))
                ;; Default: Halbe Bildschirmbreite
                (- (ash global-screen-width -1) 2)
      )     ) )
      (if top-lin
        ;; Oberste Zeile in den erlaubten Bereich bringen und ggfs. Höhe
        ;; anpassen
        (setq top-lin (min (max 1 top-lin) (- global-screen-height min-height 1))
              height (min height (- global-screen-height top-lin 1))
        )
        ;; Default: So, daß Fenster in der Mitte sitzt
        (setq top-lin (max 1 (ash (- global-screen-height height) -1)))
      )
      (if left-col
        ;; Linkeste Spalte in den erlaubten Bereich bringen und ggfs. Breite
        ;; anpassen
        (setq left-col (min (max 1 left-col) (- global-screen-width min-width 1))
              width (min width (- global-screen-width left-col 1))
        )
        ;; Default: So, daß Fenster in der Mitte sitzt
        (setq left-col (max 1 (ash (- global-screen-width width) -1)))
      )
      (mk-screen :height height :width width :full? nil :title (or title "")
                 :phys-left-col left-col :phys-top-lin top-lin
                 :text (make-empty-text height)
                 :visibility (make-array (+ height 2) :initial-element '())
    ) )
    (mk-screen :visibility
               (make-array (+ global-screen-height 2) :initial-element '())
) ) )

;-------------------------------------------------------------------------------

;; Hilfsfunktion: Testet, ob gegebener adjustable Array mit Fillpointer
;; groß genug ist, und vergrößert, wenn nicht
;; Fill-pointer wird auf neue Größe gesetzt
(defun resize-array (array size &optional (increment 10))
  (if (>= (array-dimension array 0) size)
    (setf (fill-pointer array) size)
    (adjust-array array (+ size increment) :fill-pointer size)
) )

;; Hilfsfunktion: verringert den Fill-Pointer eines gegebenen
;; adjustable Array und löscht die dabei wegfallenden Elemente.
(defun shrink-array (array delta)
  (let* ((end (fill-pointer array))
         (start (- end delta)))
    (setf (fill-pointer array) start)
    (when (eq (array-element-type array) 'T)
      (do ((index start (1+ index)))
          ((eql index end))
        (setf (aref array index) nil)
) ) ) )

;-------------------------------------------------------------------------------

;;; Funktionen für das Textfenster (intern)

;; Ausgabe eines mit Leerstellen gefüllten Zeilenstücks:
(defun display-blanks (left-col right-col)
  #+ATARI
  (write-string blanks *window* :end (- right-col left-col))
  #-ATARI
  ; Auf Terminals sind diese vielen Leerstellen laangsaam...
  (let ((count (- right-col left-col)))
    (if (and (> count 3) (eql right-col global-screen-width))
      (screen-clear-end-of-line)
      (write-string blanks *window* :end count)
  ) )
)

;; Ausgabe einer Zeile:
;; line:     auszugebende Zeile
;; mark-start, mark-end:   NIL oder zu markierender Bereich der Zeile
;; [left-col..right-col[:  darzustellendes Intervall der Zeile
;; left-arrow?: Flag, ob in der ersten Spalte ein Pfeil nach rechts ausgegeben
;;              werden soll, wenn dort ein Zeichen stünde
;; right-arrow? : Analog für die letzte Spalte
;; Cursor muß sich an der richtigen Position auf dem Bildschirm befinden,
;; reverse off, wrap off
;; right-col - left-col >= [left-arrow?] + [right-arrow?]
(defun display-line (line mark-start mark-end left-col right-col
                     #+(or ATARI DOSE) left-arrow? #+(or ATARI DOSE) right-arrow?
                    )
  (unless (> (length line) left-col) ; Zeile vorher zu Ende
    (display-blanks left-col right-col)
    (return-from display-line)
  )
  #+(or ATARI DOSE)
  (when left-arrow? ; Pfeil nach links ist evtl. auszugeben
    (write-char #+ATARI #\Code4 #+DOSE #\Code17 *window*) ; Pfeil nach links
    (incf left-col) ; jetzt right-col - left-col >= [right-arrow?]
  ) ; hier stets length(line) >= left-col
  (let ((right-col-1 right-col))
    #+(or ATARI DOSE)
    (when right-arrow? (decf right-col-1)) ; Pfeil nach rechts ist evtl. auszugeben
    (let ((end-col (min (length line) right-col-1))) ; stets end-col >= left-col
      (cond
        ((or (null mark-start) (null mark-end)
             (<= mark-end left-col) (>= mark-start end-col)
         )
          ;; Zeile ganz außerhalb des markierten Bereichs
          (write-string line *window* :start left-col :end end-col)
        )
        ((and (<= mark-start left-col) (<= end-col mark-end))
          ;; Zeile ganz innerhalb des markierten Bereichs: reverse darstellen
          (screen-reverse-on)
          (write-string line *window* :start left-col :end end-col)
          (screen-reverse-off)
        )
        (t ;; sonst: markierten Teil herauspicken und reverse darstellen
           (setq mark-start (max mark-start left-col))
           (setq mark-end (min mark-end end-col))
           (write-string line *window* :start left-col :end mark-start)
           (screen-reverse-on)
           (write-string line *window* :start mark-start :end mark-end)
           (screen-reverse-off)
           (write-string line *window* :start mark-end :end end-col)
      ) )
      (if (eql end-col (length line)) ; Zeile vor dem rechten Rand zu Ende?
        (display-blanks end-col right-col)
        #+(or ATARI DOSE)
        (when right-arrow?
          (write-char #+ATARI #\Code3 #+DOSE #\Code16 *window*) ; Pfeil nach rechts
        )
) ) ) )

;; Ausgabe eines Zeilenstücks:
;; Zeile lin des screens von Spalte left (einschl.) bis right (ausschl.)
;; anzeigen (Fensterkoordinaten)
(let ((ohchar #-DOSE #\= #+DOSE #\Code205) ; oberer horizontaler Balken
      (olchar #-DOSE #\# #+DOSE #\Code213) ; obere linke Ecke
      (orchar #-DOSE #\# #+DOSE #\Code184) ; obere rechte Ecke
      (uhchar #-DOSE #\- #+DOSE #\Code196) ; unterer horizontaler Balken
      (ulchar #-DOSE #\+ #+DOSE #\Code192) ; untere linke Ecke
      (urchar #-DOSE #\+ #+DOSE #\Code217) ; untere rechte Ecke
      (lvchar #-DOSE #\| #+DOSE #\Code179) ; linker vertikaler Balken
      (rvchar #-DOSE #\| #+DOSE #\Code179) ; rechter vertikaler Balken
     )
  (defun show-screen-line (screen lin left right)
    (let ((height (screen-height screen)) ; Größe und Position des Screens
          (width (screen-width screen))
          (phys-left-col (screen-phys-left-col screen))
          (phys-top-lin (screen-phys-top-lin screen))
         )
      ;; Bereichsüberschreitungen abfangen:
      (if (screen-full? screen)
        (setq left (max left 0) right (min right width))
        (setq left (max left -1) right (min right (+ width 1)))
      )
      (when (and (> right left) ; Trifft angegebener Bereich das Fenster?
                 (if (screen-full? screen) (< -1 lin height) (<= -1 lin height))
            )
        ;; Cursor positionieren
        (screen-set-cursor (+ phys-top-lin lin) (+ phys-left-col left))
        (cond
          ((eql lin -1) ; Titelzeile
            (let* ((title (screen-title screen))
                   (tstr (string-concat
                           (string olchar)
                           (if (< (length title) width)
                             (format nil "~V,,0,V:@<~A~>" width ohchar title)
                             (subseq title 0 width)
                           )
                           (string orchar)
                  ))     )
              (write-string tstr *window* :start (1+ left) :end (1+ right))
          ) )
          ((eql lin height) ; untere Rahmenzeile
            (when (eql left -1) (write-char ulchar *window*) (setq left 0))
            (dotimes (i (- (if (eql right (+ width 1)) width right) left))
              (write-char uhchar *window*)
            )
            (when (eql right (+ width 1)) (write-char urchar *window*))
          )
          (t (let* ((text (screen-text screen))
                    (text-lin (+ lin (screen-top-lin screen)))
                    (left-col (screen-left-col screen))
                    (line (if (< text-lin (length text))
                            (aref text text-lin)
                            ""
                    )     )
                    (marks (screen-marks screen))
                    (mark-start (aref marks 0)) ; Blockanfang
                    (mark-end (aref marks 1))   ; Blockende
                   )
               ;; evtl. Stück vom linken Rahmen
               (when (eql left -1) (write-char lvchar *window*) (setq left 0))
               ;; Teil der Zeile ausgeben
               (display-line
                 line
                 ;; Beginn Markierung oder nil
                 (and mark-start
                      (cond ((eql (mark-lin mark-start) text-lin)
                              (mark-col mark-start)
                            )
                            ((< (mark-lin mark-start) text-lin) 0)
                            (t nil)
                 )    )
                 ;; Ende Markierung oder nil
                 (and mark-end
                      (cond ((eql (mark-lin mark-end) text-lin)
                              (mark-col mark-end)
                            )
                            ((> (mark-lin mark-end) text-lin) (length line))
                            (t nil)
                 )    )
                 ;; linke Spalte (Textkoord.)
                 (+ left-col left)
                 ;; rechte Spalte + 1 (Textkoord.)
                 (+ left-col (min right width))
                 ;; Left-Arrow, falls left-col > 0 und erste Fensterspalte
                 ;; dargestellt wird
                 #+(or ATARI DOSE) (and (plusp left-col) (eql left 0))
                 ;; Right-Arrow, falls letzte Fensterspalte
                 ;; dargestellt wird
                 #+(or ATARI DOSE) (>= right width)
               )
               ;; evtl. Stück vom rechten Rahmen
               (when (eql right (+ width 1)) (write-char rvchar *window*))
  ) ) ) ) )  )
)

;; Ausgabe eines Zeilenstücks:
;; Zeile lin des screens (im Inneren) von Spalte left (einschl.) bis right
;; (ausschl.) (Fensterkoordinaten) anzeigen unter Berücksichtigung des
;; visibility-Vektors.
(defun show-screen-line-v (screen lin left right)
  (let ((height (screen-height screen))
        (width (screen-width screen))
        (visibility (screen-visibility screen))
       )
    ;; Bereichsüberschreitungen abfangen:
    (setq left (max left 0) right (min right width))
    (when (and (< left right) (< -1 lin height))
      ;; trifft angegebener Bereich das Fensterinnere?
      ;; Ja: dann die einzelnen Abschnitte abarbeiten
      (dolist (part (aref visibility (1+ lin)))
        (when (and (> (cdr part) left) (< (car part) right))
          (show-screen-line screen lin (max left (car part))
                                       (min right (cdr part))
) ) ) ) ) )

;; Ausgabe eines Fensters:
;; screen: Auszugebendes Textfenster
;; start-lin: Zeile, ab der angezeigt werden soll
;; end-lin: Zeile, bis vor die angezeigt werden soll (Fensterkoordinaten)
;; 0 <= start-lin <= end-lin <= screen.height
;; Liefert screen zurück.
;; reverse off, wrap off
(defun display-screen (screen &optional (start-lin 0)
                                        (end-lin (screen-height screen))
                      )
  (do ((width (screen-width screen))
       (screen-lin start-lin (1+ screen-lin))
      )
      ((eql screen-lin end-lin) t)
    (show-screen-line-v screen screen-lin 0 width)
) )

;;; Funktionen zur Verwaltung der visibility-Vektoren

;; Nimm aus einer visibility-Liste das Intervall [left..right[ heraus
(defun update-visibility-list-1 (vl left right)
  ;; Entferne die Einträge, die ganz verdeckt werden
  (setq vl (delete-if #'(lambda (pair)
                          (and (<= left (car pair)) (<= (cdr pair) right))
                        )
                      vl
  )        )
  ;; Bestimme die Einträge (falls vorhanden), in deren Bereich eine der Grenzen
  ;; fällt: diese müssen verkürzt werden
  (let ((left-v (member-if #'(lambda (pair) (< (car pair) left (cdr pair))) vl))
        (right-v (member-if #'(lambda (pair) (< (car pair) right (cdr pair))) vl)))
    ;; (car left-v) und (car right-v) sind zu verkürzen:
    (if (and left-v right-v (eq left-v right-v))
      ;; zu entfernender Bereich innerhalb eines Teilintervalls: in zwei teilen
      ; (... (A . B) ...) --> (... (A . left) (right . B) ...)
      (push (cons right (shiftf (cdr (car left-v)) left)) (cdr left-v))
      (progn
        (when left-v (setf (cdr (car left-v)) left))
        (when right-v (setf (car (car right-v)) right))
  ) ) )
  ;; veränderte Liste zurückgeben
  vl
)

;; Füge in eine visibility-Liste das Intervall [left..right[ ein (unter der
;; Annahme, daß es zu den vorhandenen Intervallen disjunkt ist).
(defun update-visibility-list-2 (vl left right)
  (let ((vl1 nil) (vl2 vl))
    (loop ; vl1 und vl2 laufen durch die Liste vl.
          ; Entweder vl1 = nil oder (cdr vl1) = vl2.
          ; Das Intervall [left..right[ ist jedenfalls nach vl1 einzufügen.
      (when (or (null vl2) (<= right (caar vl2))) (return))
      (shiftf vl1 vl2 (cdr vl2))
    )
    ; Das Intervall ist zwischen vl1 und vl2 einzukleben.
    (if (or (null vl2) (< right (caar vl2)))
      (push (cons left right) vl2)
      (setf (caar vl2) left) ; ersetze (caar vl2) = right durch left
    )
    ; Nun ist (caar vl2) = left. vl2 ist an vl1 anzuschließen.
    (if (null vl1)
      (setq vl vl2)
      (if (eql (cdar vl1) left)
        ; (car vl1) und (car vl2) vereinigen:
        (setf (cdar vl1) (cdar vl2) (cdr vl1) (cdr vl2))
        ; vl2 als (cdr vl1) anschließen:
        (setf (cdr vl1) vl2)
  ) ) )
  vl
)

;; Nimm aus dem visibility-Vektor von Screen den Bereich heraus, der durch
;; [top-lin..bot-lin[ x [left-col..right-col[ (in Bildschirmkoordinaten)
;; gegeben ist.
(defun update-visibility (screen top-lin bot-lin left-col right-col)
  (let* ((s-top-lin (screen-phys-top-lin screen))
         (s-left-col (screen-phys-left-col screen))
         (visibility (screen-visibility screen))
         ;; Umrechnen auf Fensterkoordinaten
         (rel-top-lin (max -1 (- top-lin s-top-lin)))
         (rel-bot-lin (min (+ (screen-height screen) 1) (- bot-lin s-top-lin)))
         (rel-left-col (max -1 (- left-col s-left-col)))
         (rel-right-col (min (+ (screen-width screen) 1) (- right-col s-left-col)))
        )
    (when (and (> rel-bot-lin rel-top-lin) (> rel-right-col rel-left-col))
      ;; Schnitt ist nicht leer
      (do ((index (1+ rel-top-lin) (1+ index))
           (end-index (1+ rel-bot-lin))
          )
          ((eql index end-index))
        ;; Für jede Zeile im Schnitt visibility-Liste updaten
        (setf (aref visibility index)
              (update-visibility-list-1 (aref visibility index)
                                        rel-left-col rel-right-col
) ) ) ) )     )

;; Mache alle Screens der Liste screens im Bereich lin, [left..right[
;; (Bildschirmkoordinaten) sichtbar, soweit sie sich nicht überlappen.
;; (Vorher waren sie dort nicht sichtbar gewesen.)
;; Die visibility-Listen werden entsprechend aktualisiert.
(defun show-newly-visible-line-parts (screens lin left right)
  (unless (null screens) ; nur etwas zu tun, wenn Screens vorhanden
    (let* ((screen (first screens))
           (screens (rest screens))
           ;; Wir können hier davon ausgehen, daß jeder Screen einen Rand
           ;; der Breite 1 hat, denn der einzige Screen mit full? = nil
           ;; ist der ganze Bildschirm, und dessen "Rand" wäre unsichtbar.
           ;; (Es ist ja 0 <= left < right <= global-screen-width und
           ;; und 0 <= lin < global-screen-height.)
           (height (screen-height screen))
           (width+1 (+ (screen-width screen) 1))
           (left-col (screen-phys-left-col screen))
           (visibility (screen-visibility screen))
           ;; Umrechnen auf Fensterkoordinaten
           (rel-lin (- lin (screen-phys-top-lin screen)))
           (rel-left (- left left-col))
           (rel-right (- right left-col))
          )
      (if (and (<= -1 rel-lin height) (<= 0 rel-right) (< rel-left width+1))
        ;; Screen screen ist betroffen
        (progn
          ;; visibility-Liste updaten
          (setf (aref visibility (1+ rel-lin))
                (update-visibility-list-2 (aref visibility (1+ rel-lin))
                                          (max -1 rel-left)
                                          (min width+1 rel-right)
          )     )
          ;; falls nötig, links darunter liegende Screens ansprechen
          (when (< rel-left -1)
            (show-newly-visible-line-parts screens lin left (1- left-col))
          )
          ;; betroffenes Zeilenstück ausgeben
          (show-screen-line screen rel-lin rel-left rel-right)
          ;; falls nötig, rechts darunter liegende Screens ansprechen
          (when (> rel-right width+1)
            (show-newly-visible-line-parts screens lin (+ left-col width+1) right)
        ) )
        ;; sonst direkt zu den nächsten Screens weitergehen
        (show-newly-visible-line-parts screens lin left right)
) ) ) )

;-------------------------------------------------------------------------------

;;; Implementierung der Interface-Funktionen

;; Liste der auf dem Bildschirm dargestellten Screens, geordnet nach ihrer
;; Verdeckungs-Rangfolge (d.h. der oberste zuerst).
(defvar *screens* '())

;; Cursorposition im screen setzen (Textkoordinaten), Wert T.
(defun set-cursor (screen lin &optional (col (screen-saved-col screen) col-s))
  (let* ((text (screen-text screen))
         (text-len (length text)))
    ;; Bereichsüberschreitungen abfangen:
    (setq lin (max 0 (min lin (1- text-len))))
    (setq col (max 0 (min col (length (aref text lin)))))
    ;; neue Position vermerken
    (setf (screen-lin screen) lin (screen-col screen) col)
    ;; falls Spalte angegeben, gemerkte Spalte setzen
    (when col-s (setf (screen-saved-col screen) col))
    t
) )

;; vertikales Scrollen eines Textfensters; upgedateter screen wird zurück-
;; gegeben
;; n > 0: n Zeilen nach oben scrollen
;; n = 0: nichts tun
;; n < 0: -n Zeilen nach unten scrollen
;; flag /= nil: Cursor mitverschieben
(defun scroll-vertical (screen n &optional (flag nil))
  (let* ((text (screen-text screen))
         (text-len (length text))
         (top-lin (screen-top-lin screen))
        )
    ;; evtl. Cursor updaten
    (when flag (set-cursor screen (+ (screen-lin screen) n)))
    ;; Bereichsüberschreitungen abfangen:
    (setq n (max (- top-lin) (min n (- text-len 1 top-lin))))
    ;; Datenstruktur updaten
    (setf (screen-top-lin screen) (+ top-lin n))
    (when (eql n 0) (return-from scroll-vertical screen))
    (cond ((or (> (abs n) 10)
               (not (screen-full? screen))
               (null *screens*)
               (not (eq screen (first *screens*)))
           )
            ;; n groß oder nicht der ganze Bildschirm oder nicht oberster
            ;; Screen: Fenster neu schreiben
            (display-screen screen)
          )
          ((plusp n) ; nach oben
            (screen-home)
            (dotimes (i n) (screen-delete-line))
            (display-screen screen (- (screen-height screen) n))
          )
          (t ; nach unten
            (screen-home)
            (dotimes (i (- n)) (screen-insert-line))
            (display-screen screen 0 (- n))
) ) )     )

;; horizontales Scrollen des Textfensters; upgedateter screen zurück
;; n > 0: um n Spalten nach links scrollen
;; n = 0: nichts tun
;; n < 0: um -n Spalten nach rechts scrollen
(defun scroll-horizontal (screen n)
  (let ((left-col (screen-left-col screen)))
    (when (minusp (+ left-col n)) (setq n (- left-col)))
    (if (eql n 0)
      screen
      (progn (setf (screen-left-col screen) (+ left-col n))
             (display-screen screen)
) ) ) )

;; Cursor setzen und Textfenster ggfs. so verändern, daß Cursor im Fenster ist,
;; Cursor einschalten - nur wenn oberster Screen
;; center: Flag, ob Cursor möglichst in der Mitte erscheinen soll
;; liefert T zurück
(defun set-cursor-visible (screen &optional (center nil))
  (let* ((lin (screen-lin screen))
         (col (screen-col screen))
         (top-lin (screen-top-lin screen))
         (left-col (screen-left-col screen))
         (height (screen-height screen))
         (width (screen-width screen))
        )
    (cond
      ((<= (if (eql left-col 0) 0 (1+ left-col)) col (+ left-col width -2))
        ;; Cursorspalte im Fensterbereich
        (cond
          ((< lin top-lin)
            ;; Cursorzeile über dem Fenster -> nach unten scrollen
            (scroll-vertical screen
                             (- lin top-lin (if center (ash height -1) 0))
          ) )
          ((>= lin (+ top-lin height))
            ;; Cursorzeile unter dem Fenster -> nach oben scrollen
            (scroll-vertical screen
                        (- lin top-lin -1 (if center (ash height -1) height))
      ) ) ) )
      ((<= top-lin lin (+ top-lin height -1))
        ;; Cursorzeile im Fensterbereich, Cursorspalte aber nicht ->
        ;;  nach rechts oder links scrollen
        (scroll-horizontal screen
            (- col left-col
               (if (or center (< width 40))
                 (ash width -1)
                 (if (<= col left-col) (- width 20) 20)
      ) )   )  )
      ;; sonst: Fensterausschnitt neu setzen
      (t (let ((new-left-col (if (< col (1- width))
                               0
                               (- col (if (or center (< width 40))
                                        (ash width -1)
                                        20
               )             ) )      )
               (new-top-lin (max 0 (- lin (ash height -1))))
              )
           (setf (screen-left-col screen) new-left-col
                 (screen-top-lin screen) new-top-lin
           )
           (display-screen screen)
  ) ) )  )
  (when (and *screens* (eq screen (first *screens*))) ; oberster Screen?
    (screen-set-cursor                    ; Cursor setzen
      (+ (- (screen-lin screen) (screen-top-lin screen))
         (screen-phys-top-lin screen)
      )
      (+ (- (screen-col screen) (screen-left-col screen))
         (screen-phys-left-col screen)
    ) )
    (screen-cursor-on)                    ; und einschalten
  )
  t
)

;; Zeile lin ab Spalte col (Textkoordinaten) auffrischen, Wert T.
(defun refresh-line (screen lin col)
  (show-screen-line-v screen (- lin (screen-top-lin screen))
                             (- col (screen-left-col screen))
                             (screen-width screen)
  )
  t
)

;; Fenster ab Zeile lin bis vor Zeile end-lin (Textkoordinaten) auffrischen,
;; ab Zeile end-lin um |n| Zeilen scrollen (n>0: nach oben, n<0: nach unten),
;; Wert T.
(defun refresh-screen (screen lin end-lin &optional (n 0))
  (let ((top-lin (screen-top-lin screen))
        (height (screen-height screen)))
    (when (<= (+ top-lin height) lin)
      ;; Bildschirminhalt kann unverändert bleiben
      (return-from refresh-screen t)
    )
    (when (<= end-lin top-lin)
      ;; Bildschirminhalt kann unverändert bleiben
      (setf (screen-top-lin screen) (+ top-lin n))
      (return-from refresh-screen t)
    )
    ;; Bildschirminhalt muß teilweise gescrollt werden
    (when (or (> (abs n) 10)
              (not (screen-full? screen))
              (null *screens*)
              (not (eq screen (first *screens*)))
          )
      ;; n groß oder nicht der ganze Bildschirm oder nicht oberster
      ;; Screen: Fenster neu schreiben
      (display-screen screen)
      (return-from refresh-screen t)
    )
    ;; Scrollen
    (cond ((minusp n) ; nach unten
            (setq end-lin (max end-lin (- top-lin n)))
            ; Wir haben  end-lin >= top-lin + |n|  erzwungen.
            (let ((scroll-top (- (+ end-lin n) top-lin))) ; >=0
              (when (< (- scroll-top n) height)
                (screen-set-cursor scroll-top 0)
                (dotimes (i (- n)) (screen-insert-line))
          ) ) )
          ((plusp n) ; nach oben
            (let ((scroll-top (- end-lin top-lin))) ; >0
              (when (< scroll-top height)
                (if (>= (+ scroll-top n) height)
                  (display-screen screen scroll-top height)
                  (progn
                    (screen-set-cursor scroll-top 0)
                    (dotimes (i n) (screen-delete-line))
                    (display-screen screen (- height n) height)
    )     ) ) ) ) )
    ;; Bereich zwischen lin und end-lin anzeigen
    (let ((screen-lin (max 0 (- lin top-lin)))
          (screen-end-lin (min (- end-lin top-lin) height)))
      (when (< screen-lin screen-end-lin)
        (display-screen screen screen-lin screen-end-lin)
  ) ) )
  t
)

;; Fenster vom Bildschirm nehmen, Wert: neuer oberster Screen, falls vorhanden,
;; sonst NIL
(defun hide-screen (screen)
  (let* ((height+2 (+ (screen-height screen) 2))
         (top-lin (screen-phys-top-lin screen))
         (left-col (screen-phys-left-col screen))
         (visibility (screen-visibility screen))
         ;; screen in *screens* suchen
         (screens (member screen *screens* :test #'eq))
        )
    (when screens ; wenn nicht da, ist nichts zu tun
      (do ((index 0 (1+ index))
           (lin (1- top-lin) (1+ lin))
          )
          ((eql index height+2))
        ;; Zeilen einzeln durchgehen
        (dolist (part (aref visibility index))
          ;; freiwerdende Teile anzeigen
          (show-newly-visible-line-parts
            (rest screens) lin (+ left-col (car part)) (+ left-col (cdr part))
        ) )
        ;; Sichtbarkeit löschen
        (setf (aref visibility index) '())
      )
      ;; screen aus den aktiven Screens entfernen
      (setq *screens* (delete screen *screens* :test #'eq))
    )
    (first *screens*)
) )

;; Fenster nach oben bringen
(defun activate-screen (screen)
  (let* ((height (screen-height screen))
         (width (screen-width screen))
         (top-lin (screen-phys-top-lin screen))
         (left-col (screen-phys-left-col screen))
         (bot-lin (+ top-lin height))
         (right-col (+ left-col width))
         (visibility (screen-visibility screen))
         (left 0)
         (right width)
        )
    (unless (and (not (null *screens*)) (eq screen (first *screens*)))
      ;; falls schon oben, ist nichts zu tun
      (unless (screen-full? screen)
        ;; Rahmen berücksichtigen
        (decf top-lin) (incf bot-lin)
        (decf left-col) (incf right-col)
        (decf left) (incf right)
      )
      ;; [top-lin..bot-lin[ x [left-col..right-col[ ist Screenbereich auf
      ;; dem Bildschirm (in Bildschirmkoordinaten)
      (do ((screens *screens* (rest screens)))
          ((or (null screens) (eq (first screens) screen)))
        ;; visibility updaten für darüber gewesenen Screen
        (update-visibility (first screens) top-lin bot-lin left-col right-col)
      )
      ;; screen in der Liste nach vorne bringen
      (setq *screens* (cons screen (delete screen *screens* :test #'eq)))
      ;; visibility-Listen setzen und Zeilen anzeigen, wenn nötig
      (if (screen-full? screen)
        (dotimes (lin height)
          (let ((new-vl (list (cons left right))))
            (unless (equal (aref visibility (1+ lin)) new-vl)
              (setf (aref visibility (1+ lin)) new-vl)
              (show-screen-line screen lin left right)
        ) ) )
        (dotimes (lin (+ height 2))
          (let ((new-vl (list (cons left right))))
            (unless (equal (aref visibility lin) new-vl)
              (setf (aref visibility lin) new-vl)
              (show-screen-line screen (1- lin) left right)
  ) ) ) ) ) )
  t
)

;; Cursor und Marken mitführen bei Einfüge- und Löschoperationen
(defun update-marks (screen lin1 col1 lin2 col2)
  (flet ((new-lin-col (lin col) ; Berechne neue Koordinaten
           (cond
             ((eql lin1 lin2) ; alles in einer Zeile
               (if (eql lin lin1) ; ändert sich nur, wenn in dieser Zeile
                 (if (< col1 col)
                   (values lin (+ col (- col2 col1)))
                   (values lin (min col col2))
                 )
                 (values lin col)
             ) )
             ((> lin1 lin2) ; Löschen eines Textteils über mehrere Zeilen
               (cond ((eql lin lin2) (values lin (min col col2)))
                     ((eql lin lin1)
                       (values lin2 (max (+ col (- col2 col1)) col2))
                     )
                     ((< lin2 lin lin1) (values lin2 col2))
                     ((< lin1 lin) (values (+ lin (- lin2 lin1)) col))
                     (t (values lin col))
             ) )
             (t (cond ((eql lin lin1) ; Einfügen eines Textteils über mehrere
                        (if (> col col1) ; Zeilen
                          (values lin2 (+ col (- col2 col1)))
                          (values lin col)
                      ) )
                      ((< lin1 lin) (values (+ lin (- lin2 lin1)) col))
                      (t (values lin col))
        )) ) )  )
    (let ((lin (screen-lin screen))
          (col (screen-col screen))
         )
      ;; Cursor updaten
      (if (and (eql lin lin1) (eql col col1))
        (setf (screen-lin screen) lin2
              (screen-col screen) col2
              (screen-saved-col screen) col2
        )
        (multiple-value-bind (new-lin new-col) (new-lin-col lin col)
          (setf (screen-lin screen) new-lin
                (screen-col screen) new-col
    ) ) ) )
    (let ((marks (screen-marks screen)))
      ;; Marken updaten
      (dotimes (i (length marks))
        (let ((mark (aref marks i)))
          (when mark
            (multiple-value-bind (new-lin new-col)
                (new-lin-col (mark-lin mark) (mark-col mark))
              (setf (mark-lin mark) new-lin
                    (mark-col mark) new-col
) ) ) ) ) ) ) )

;; Screen scrollen um n nach oben, dabei Cursor mitführen
(defun scroll-screen (screen n)
  (scroll-vertical screen n t)
  t
)

;###############################################################################
;;;; Full-Screen-Editor
;;;;
;;;; Michael Stoll, Jan./Feb. 1992
;;;; Bruno Haible 30.3.1992, 13.5.1992

;===========================================================================
;  G R U N D F U N K T I O N E N   Z U R   T E X T M A N I P U L A T I O N
;===========================================================================

;; Liste der bei Undo durchzuführenden Aktionen:
(defvar *undo* '())

#|
; erstrangige, alles Bisherige überschattende Undo-Aktion:
(defun undo1 (function)
  (setq *undo* (list function))
)

; zweitrangige, akkumulierende Undo-Aktion:
(defun undo2 (function)
  (push function *undo*)
)

; drittrangige, nur Cursor-bewegende, Undo-Aktion:
(defun undo3 (screen)
  (let ((function
          (let ((lin (screen-lin screen))
                (col (screen-col screen)))
            #'(lambda () (set-cursor screen lin col))
       )) )
    (undo2 function)
) )
|# ; vorerst:
(defun undo1 (function) (declare (ignore function)))
(defun undo2 (function) (declare (ignore function)))
(defun undo3 (screen) (declare (ignore screen)))

;-------------------------------------------------------------------------------

;;; Cursor-Bewegung

(defun cursor-up (screen)
  #+DEUTSCH "Cursor nach oben"
  #+ENGLISH "cursor up"
  (let ((lin (screen-lin screen)))
    (and (plusp lin)
         (progn (undo3 screen) (set-cursor screen (1- lin)))
) ) )

(defun cursor-down (screen)
  #+DEUTSCH "Cursor nach unten"
  #+ENGLISH "cursor down"
  (let ((lin (screen-lin screen)))
    (and (< lin (1- (length (screen-text screen))))
         (progn (undo3 screen) (set-cursor screen (1+ lin)))
) ) )

(defun cursor-left (screen)
  #+DEUTSCH "Cursor nach links"
  #+ENGLISH "cursor left"
  (let ((lin (screen-lin screen))
        (col (screen-col screen)))
    (cond ((plusp col) (decf col))
          ((plusp lin)
            (decf lin) (setq col (length (aref (screen-text screen) lin))) )
          (t (return-from cursor-left nil))
    )
    (undo3 screen)
    (set-cursor screen lin col)
) )

(defun cursor-right (screen)
  #+DEUTSCH "Cursor nach rechts"
  #+ENGLISH "cursor right"
  (let ((text (screen-text screen))
        (lin (screen-lin screen))
        (col (screen-col screen)))
    (cond ((< col (length (aref text lin))) (incf col))
          ((< lin (1- (length text))) (incf lin) (setq col 0))
          (t (return-from cursor-right nil))
    )
    (undo3 screen)
    (set-cursor screen lin col)
) )

(defun cursor-to-start-of-line (screen)
  #+DEUTSCH "Cursor an den Zeilenanfang"
  #+ENGLISH "cursor to start of line"
  (let ((lin (screen-lin screen)))
    (undo3 screen)
    (set-cursor screen lin 0)
) )

(defun cursor-to-end-of-line (screen)
  #+DEUTSCH "Cursor ans Zeilenende"
  #+ENGLISH "cursor to end of line"
  (let ((lin (screen-lin screen)))
    (undo3 screen)
    (set-cursor screen lin (length (aref (screen-text screen) lin)))
) )

(defun cursor-to-start-of-text (screen)
  #+DEUTSCH "Cursor an den Textanfang"
  #+ENGLISH "cursor to start of text"
  (undo3 screen)
  (set-cursor screen 0 0)
)

(defun cursor-to-end-of-text (screen)
  #+DEUTSCH "Cursor ans Textende"
  #+ENGLISH "cursor to end of text"
  (undo3 screen)
  (let* ((text (screen-text screen))
         (text-len-1 (1- (length text))))
    (set-cursor screen text-len-1 (length (aref text text-len-1)))
) )

(defun page-up (screen)
  #+DEUTSCH "Seite nach oben"
  #+ENGLISH "page up"
  (undo3 screen)
  (scroll-screen screen (- 1 (screen-height screen)))
)

(defun page-down (screen)
  #+DEUTSCH "Seite nach unten"
  #+ENGLISH "page down"
  (undo3 screen)
  (scroll-screen screen (- (screen-height screen) 1))
)

(defun line-up (screen)
  #+DEUTSCH "Zeile nach oben"
  #+ENGLISH "line up"
  (undo3 screen)
  (scroll-screen screen -1)
)

(defun line-down (screen)
  #+DEUTSCH "Zeile nach unten"
  #+ENGLISH "line down"
  (undo3 screen)
  (scroll-screen screen 1)
)

;-------------------------------------------------------------------------------

;; Marken

(defun set-mark-fn (n)
  (let ((index (+ n 2)))
    (labels ((set-mark (screen &optional (lin (screen-lin screen))
                                         (col (screen-col screen)) )
               (undo2 (let ((mark-n (aref (screen-marks screen) index)))
                        (if mark-n
                          #'(lambda () (setf (aref (screen-marks screen) index) nil))
                          (let ((old-lin (mark-lin mark-n)) (old-col (mark-col mark-n)))
                            #'(lambda () (set-mark screen old-lin old-col))
               )      ) ) )
               (setf (aref (screen-marks screen) index) (make-mark lin col))
            ))
      #'set-mark
) ) )
(defun set-mark-doc (n)
  (format nil #+DEUTSCH "Marke ~D setzen"
              #+ENGLISH "set mark ~D"
              n
) )

(defun cursor-to-mark-fn (n)
  (let ((index (+ n 2)))
    #'(lambda (screen)
        (undo3 screen)
        (let ((mark (aref (screen-marks screen) index)))
          (and mark (set-cursor screen (mark-lin mark) (mark-col mark)))
      ) )
) )
(defun cursor-to-mark-doc (n)
  (format nil #+DEUTSCH "Cursor zu Marke ~D"
              #+ENGLISH "cursor to mark ~D"
              n
) )

;-------------------------------------------------------------------------------

;; Region (start-lin start-col end-lin end-col) = Der Textbereich
;; von (make-mark start-lin start-col) bis (make-mark end-lin end-col).

;; Eine linelist ist eine umgedrehte nichtleere Liste von Zeilen, die keine
;; Newlines enthalten und zwischen denen jeweils ein Newline zu denken ist:
;; (stringn ... string0) mit n>=0 steht für den String
;; (string-concat string0 newline-as-string ... newline-as-string stringn).

(defconstant newline-as-string (string #\Newline))

;; Eine Region in eine Liste von Zeilen umwandeln
(defun region-to-linelist (screen start-lin start-col end-lin end-col)
  (let ((text (screen-text screen))
        (linelist '()))
    (if (eql start-lin end-lin)
      (push (subseq (aref text start-lin) start-col end-col) linelist)
      (progn
        (push (subseq (aref text start-lin) start-col) linelist)
        (do ((index (1+ start-lin) (1+ index)))
            ((eql index end-lin))
          (push (copy-seq (aref text index)) linelist)
        )
        (push (subseq (aref text end-lin) 0 end-col) linelist)
    ) )
    linelist
) )

;; String (der Newlines enthalten kann) in Linelist umwandeln:
(defun string-to-linelist (string)
  (let ((nlpos (position #\Newline string)))
    (if (null nlpos)
      (list string)
      (macrolet ((subseq (string a b)
                   `(make-array (- ,b ,a) :element-type 'string-char
                      :displaced-to ,string :displaced-index-offset ,a
                    )
                ))
        (let ((linelist (list (subseq string 0 nlpos))))
          (loop
            (let ((pos (1+ nlpos)))
              (when (null (setq nlpos (position #\Newline string :start pos)))
                (push (subseq string pos (length string)) linelist)
                (return)
              )
              (push (subseq string pos nlpos) linelist)
          ) )
          linelist
      ) )
) ) )

;-------------------------------------------------------------------------------

;;; Löschfunktionen

;; delete-char löscht das Zeichen unter dem Cursor und liefert T zurück,
;; wenn nicht am Zeilenende gewesen und Zeichen gelöscht, sonst NIL.
(defun delete-char (screen)
  #+DEUTSCH "Zeichen unter dem Cursor löschen"
  #+ENGLISH "delete character at cursor"
  (let* ((text (screen-text screen))
         (lin (screen-lin screen))
         (col (screen-col screen))
         (line (aref text lin))
         (line-len (length line))
        )
    ;; Am Zeilenende?
    (when (eql col line-len) (return-from delete-char nil))
    ;; Zeichen löschen
    (undo2 (let ((c (aref line col)))
             #'(lambda () (insert-char screen c) (cursor-left screen))
    )      )
    (replace line line :start1 col :start2 (1+ col))
    (decf (fill-pointer line))
    ;; Updaten
    (update-marks screen lin (1+ col) lin col)
    (refresh-line screen lin col)
) )

;; combine-lines vereinigt die Cursorzeile mit der folgenden
;; liefert T zurück, wenn Cursorzeile nicht die letzte war, sonst NIL.
(defun combine-lines (screen)
  #+DEUTSCH "Cursorzeile mit der nächsten vereinigen"
  #+ENGLISH "combine two lines"
  (let* ((text (screen-text screen))
         (lin (screen-lin screen))
         (lin+1 (1+ lin))
         (line (aref text lin))
         (line-len (length line))
        )
    ;; Letzte Zeile?
    (when (eql lin+1 (length text)) (return-from combine-lines nil))
    ;; Zeilen zusammenhängen
    (undo2
      (let ((col (screen-col screen)))
        #'(lambda ()
            (set-cursor screen lin line-len)
            (insert-line screen)
            (set-cursor screen lin col)
    ) )   )
    (let ((second-line (aref text lin+1)))
      (resize-array line (+ line-len (length second-line)))
      (replace line second-line :start1 line-len)
    )
    ;; Zeilen darunter hinaufschieben
    (replace text text :start1 lin+1 :start2 (1+ lin+1))
    (shrink-array text 1)
    ;; Updaten
    (update-marks screen lin+1 0 lin line-len)
    (refresh-screen screen lin lin+1 1)
) )

(defun delete-char-1 (screen)
  #+DEUTSCH "Zeichen unter dem Cursor löschen, zeilenübergreifend"
  #+ENGLISH "delete character at cursor, across lines"
  (or (delete-char screen) (combine-lines screen))
)

(defun backspace (screen)
  #+DEUTSCH "Zeichen links vom Cursor löschen"
  #+ENGLISH "delete character before cursor"
  (and (plusp (screen-col screen))
       (cursor-left screen)
       (delete-char screen)
) )

(defun backspace-1 (screen)
  #+DEUTSCH "Zeichen links vom Cursor löschen, zeilenübergreifend"
  #+ENGLISH "delete character before cursor, across lines"
  (and (cursor-left screen) (delete-char-1 screen))
)

;; Eine Region löschen
(defun delete-region (screen start-lin start-col end-lin end-col)
  (let ((text (screen-text screen)))
    (undo3 screen)
    (undo2
      (let ((linelist (region-to-linelist screen start-lin start-col end-lin end-col)))
        #'(lambda ()
            (set-cursor screen start-lin start-col)
            (insert-linelist screen linelist)
    ) )   )
    (cond
      ((eql start-lin end-lin) ; innerhalb einer Zeile
        (let ((line (aref text start-lin)))
          ;; Stück der Zeile löschen
          (replace line line :start1 start-col :start2 end-col)
          (decf (fill-pointer line) (- end-col start-col))
          ;; Updaten
          (update-marks screen end-lin end-col start-lin start-col)
          (refresh-line screen start-lin start-col)
      ) )
      (t (let* ((line1 (aref text start-lin))
                (line2 (aref text end-lin))
                (new-size-1 (+ start-col (- (length line2) end-col))))
           ;; Teile der ersten und letzten Zeile zusammenhängen
           (resize-array line1 new-size-1)
           (replace line1 line2 :start1 start-col :start2 end-col)
           ;; Zeilen dazwischen werden frei
           ;; Zeilen darunter hochschieben
           (replace text text :start1 (1+ start-lin) :start2 (1+ end-lin))
           (shrink-array text (- end-lin start-lin))
           ;; Updaten
           (update-marks screen end-lin end-col start-lin start-col)
           (refresh-screen screen start-lin (1+ start-lin) (- end-lin start-lin))
) ) ) )  )

;; Eine Zeile löschen (Zeile, in der der Cursor steht)
(defun delete-line (screen)
  #+DEUTSCH "Zeile löschen"
  #+ENGLISH "delete line"
  (let* ((text (screen-text screen))
         (lin (screen-lin screen)))
    (if (eql lin (1- (length text)))
      (delete-region screen lin 0 lin (length (aref text lin)))
      (delete-region screen lin 0 (1+ lin) 0)
) ) )

(defun clear-start-of-line (screen)
  #+DEUTSCH "Vom Zeilenanfang bis Cursorposition löschen"
  #+ENGLISH "delete part of line left to the cursor"
  (let ((lin (screen-lin screen))
        (col (screen-col screen)))
    (delete-region screen lin 0 lin col)
) )

(defun clear-end-of-line (screen)
  #+DEUTSCH "Bis zum Zeilenende löschen"
  #+ENGLISH "delete up to end of line"
  (let ((text (screen-text screen))
        (lin (screen-lin screen))
        (col (screen-col screen)))
    (delete-region screen lin col lin (length (aref text lin)))
) )

;-------------------------------------------------------------------------------

;;; Einfügefunktionen

;; insert-char fügt an der Cursorpos. ein Zeichen ein, Cursor nach rechts,
;; liefert T zurück.
(defun insert-char (screen char)
  (let* ((text (screen-text screen))
         (lin (screen-lin screen))
         (col (screen-col screen))
         (line (aref text lin))
         (line-len (length line)))
    ;; Zeichen einfügen
    (undo2 #'(lambda () (backspace screen)))
    (resize-array line (1+ line-len))
    (replace line line :start1 (1+ col) :start2 col)
    (setf (aref line col) char)
    ;; Updaten
    (update-marks screen lin col lin (1+ col))
    (refresh-line screen lin col)
) )

;; An Cursorpos. einen Zeilenumbruch einfügen und Cursor an den Anfang
;; der neuen Zeile setzen
(defun insert-line (screen)
  #+DEUTSCH "Zeilenumbruch einfügen"
  #+ENGLISH "begin new line at cursor"
  (let* ((text (screen-text screen))
         (lin (screen-lin screen))
         (lin+1 (1+ lin))
         (col (screen-col screen))
         (line (aref text lin)))
    ;; Neue Zeile einfügen
    (undo2 #'(lambda () (backspace-1 screen)))
    (let ((new-line (get-new-line (- (length line) col))))
      (replace new-line line :start2 col)
      (setf (fill-pointer line) col)
      (resize-array text (1+ (length text)))
      (replace text text :start1 (1+ lin+1) :start2 lin+1)
      (setf (aref text lin+1) new-line)
    )
    ;; Updaten
    (update-marks screen lin col lin+1 0)
    (refresh-screen screen lin (1+ lin+1) -1)
) )

;; Eine Liste von Zeilen in umgekehrter Reihenfolge an Cursorposition einfügen
(defun insert-linelist (screen linelist)
  (let ((text (screen-text screen))
        (lin (screen-lin screen))
        (col (screen-col screen)))
    (cond
      ((null linelist) t)
      ((null (rest linelist))
        ;; kein Zeilenumbruch: String in Zeile einbauen
        (let* ((line (aref text lin))
               (piece (first linelist))
               (piece-len (length piece))
               (new-col (+ col piece-len)))
          ;; Zeile um piece-len verlängern
          (resize-array line (+ (length line) piece-len))
          ;; Platz freimachen
          (replace line line :start1 new-col :start2 col)
          ;; und String einkopieren
          (replace line piece :start1 col)
          ;; Updaten
          (update-marks screen lin col lin new-col)
          (undo2 #'(lambda () (delete-region screen lin col lin new-col)))
          (refresh-line screen lin col)
      ) )
      (t
        (let* ((nl-count (1- (length linelist)))
               (last-lin (+ lin nl-count)))
          ;; Text-Buffer vergrößern
          (resize-array text (+ (length text) nl-count))
          ;; Platz freimachen
          (replace text text :start1 (1+ last-lin) :start2 (1+ lin))
          ;; und Zeilen einfügen
          (let* ((line (aref text lin))
                 (index last-lin)
                 (last-line (pop linelist))
                 (last-len (length last-line)))
            ;; Letzte neue Zeile mit Rest der Cursorzeile verbinden
            (let ((new-line (get-new-line (+ last-len (- (length line) col)))))
              (replace new-line last-line)
              (replace new-line line :start1 last-len :start2 col)
              (setf (aref text index) new-line)
            )
            ;; Die mittleren Zeilen einfügen
            (loop
              (when (null (rest linelist)) (return))
              (decf index)
              (let* ((curr-line (pop linelist))
                     (new-line (get-new-line (length curr-line))))
                (replace new-line curr-line)
                (setf (aref text index) new-line)
            ) )
            ;; Cursorzeilenanfang mit erster einzufügender Zeile kombinieren
            (let ((first-line (first linelist)))
              (resize-array line (+ col (length first-line)))
              (replace line first-line :start1 col)
            )
            ;; Updaten
            (update-marks screen lin col last-lin last-len)
            (undo2 #'(lambda () (delete-region screen lin col last-lin last-len)))
            (refresh-screen screen lin (1+ last-lin) (- nl-count))
) ) ) ) ) )

;; An Cursorpos. einen String einfügen und Cursor an das Ende des eingefügten
;; Textes setzen
(defun insert-string (screen string)
  (insert-linelist screen (string-to-linelist string))
)

;-------------------------------------------------------------------------------

;; Eine Region auf einen Stream schreiben
(defun write-region (screen start-lin start-col end-lin end-col stream)
  (let ((text (screen-text screen)))
    (if (eql start-lin end-lin)
      (write-string (aref text start-lin) stream :start start-col :end end-col)
      (progn
        (write-line (aref text start-lin) stream :start start-col)
        (do ((index (1+ start-lin) (1+ index)))
            ((eql index end-lin))
          (write-line (aref text index) stream)
        )
        (write-string (aref text end-lin) stream :end end-col)
  ) ) )
  t
)

;; Von einem Stream lesen und einfügen an Cursorposition
(defun insert-stream (screen stream)
  (insert-linelist screen
    (let ((eof "EOF")
          (linelist '()))
      (loop
        (multiple-value-bind (line eof-reached) (read-line stream nil eof)
          (when (eq line eof) (push "" linelist) (return))
          (push line linelist)
          (when eof-reached (return))
      ) )
      linelist
) ) )

;-------------------------------------------------------------------------------

;;; Block

(defun cursor-to-start-of-block (screen)
  #+DEUTSCH "Cursor zum Blockanfang"
  #+ENGLISH "cursor to start of block"
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1)))
    (and mark1 mark2
         (progn (undo3 screen)
                (set-cursor screen (mark-lin mark1) (mark-col mark1))
) ) )    )

(defun cursor-to-end-of-block (screen)
  #+DEUTSCH "Cursor zum Blockende"
  #+ENGLISH "cursor to end of block"
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1)))
    (and mark1 mark2
         (progn (undo3 screen)
                (set-cursor screen (mark-lin mark2) (mark-col mark2))
) ) )    )

(defun set-block-start (screen &optional (lin (screen-lin screen))
                                         (col (screen-col screen)) )
  #+DEUTSCH "Blockanfang setzen"
  #+ENGLISH "set block start"
  (undo-blockmarks screen)
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1))
         (end-lin (and mark2 (mark-lin mark2)))
         (end-col (and mark2 (mark-col mark2)))
         (start-lin (if mark1 (min (mark-lin mark1) lin) lin)))
    (unless ; existiert mark2 und liegt hinter (lin,col) ?
            (and mark2 (or (> end-lin lin)
                           (and (= end-lin lin) (>= end-col col))
            )          )
      (let ((text (screen-text screen)))
        (setq end-lin (1- (length text)))
        (setq end-col (length (aref text end-lin)))
        (setf (aref marks 1) (make-mark end-lin end-col))
    ) )
    (setf (aref marks 0) (make-mark lin col))
    (refresh-screen screen start-lin (1+ end-lin))
) )

(defun set-block-end (screen &optional (lin (screen-lin screen))
                                       (col (screen-col screen)) )
  #+DEUTSCH "Blockende setzen"
  #+ENGLISH "set block end"
  (undo-blockmarks screen)
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1))
         (start-lin (and mark1 (mark-lin mark1)))
         (start-col (and mark1 (mark-col mark1)))
         (end-lin (if mark2 (max (mark-lin mark2) lin) lin)))
    (unless ; existiert mark1 und liegt vor (lin,col) ?
            (and mark1 (or (< start-lin lin)
                           (and (= start-lin lin) (<= start-col col))
            )          )
      (setq start-lin 0)
      (setq start-col 0)
      (setf (aref marks 0) (make-mark start-lin start-col))
    )
    (setf (aref marks 1) (make-mark lin col))
    (refresh-screen screen start-lin (1+ end-lin))
) )

(defun hide-block (screen)
  #+DEUTSCH "Block demarkieren"
  #+ENGLISH "remove block marks"
  (undo-blockmarks screen)
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1)))
    (setf (aref marks 0) nil (aref marks 1) nil)
    (and mark1 mark2
         (refresh-screen screen (mark-lin mark1) (1+ (mark-lin mark2)))
) ) )

(defun undo-blockmarks (screen)
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1)))
    (when mark2
      (undo2 (let ((lin (mark-lin mark2)) (col (mark-col mark2)))
               #'(lambda () (set-block-end screen lin col))
    ) )      )
    (when mark1
      (undo2 (let ((lin (mark-lin mark1)) (col (mark-col mark1)))
               #'(lambda () (set-block-start screen lin col))
    ) )      )
    (undo2 #'(lambda () (hide-block screen)))
) )

(defun mark-region (screen lin1 col1 lin2 col2)
  (and lin1
       (let* ((marks (screen-marks screen))
              (mark1 (aref marks 0))
              (mark2 (aref marks 1)))
         (setf (aref marks 0) (make-mark lin1 col1)
               (aref marks 1) (make-mark lin2 col2)
         )
         (when (and mark1 mark2)
           (setq lin1 (min lin1 (mark-lin mark1))
                 lin2 (max lin2 (mark-lin mark2))
         ) )
         (refresh-screen screen lin1 (1+ lin2))
) )    )

(defun get-block (screen)
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1)))
    (if (and mark1 mark2)
      (values (mark-lin mark1) (mark-col mark1)
              (mark-lin mark2) (mark-col mark2)
      )
      (values nil nil nil nil)
) ) )

(defun delete-block (screen)
  #+DEUTSCH "Block löschen"
  #+ENGLISH "delete block"
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1)))
    (unless (and mark1 mark2) (return-from delete-block nil))
    (undo-blockmarks screen)
    (setf (aref marks 0) nil (aref marks 1) nil)
    (delete-region screen (mark-lin mark1) (mark-col mark1)
                          (mark-lin mark2) (mark-col mark2)
) ) )

; Undo ab hier implementieren??

(defun move-block (screen) ; Block an Cursorposition verschieben
  #+DEUTSCH "Block an Cursorposition verschieben"
  #+ENGLISH "move block to cursor position"
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1)))
    (unless (and mark1 mark2) (return-from move-block nil))
    (let* ((lin1 (mark-lin mark1))
           (col1 (mark-col mark1))
           (lin2 (mark-lin mark2))
           (col2 (mark-col mark2))
           ;; Block in Zeilenliste packen:
           (linelist (region-to-linelist screen lin1 col1 lin2 col2)))
      ;; und löschen:
      (delete-region screen lin1 col1 lin2 col2)
      (let ((lin (screen-lin screen)) ; Cursorpos. merken
            (col (screen-col screen)))
        (insert-linelist screen linelist) ; Block an Cursorpos. einfügen
        (setf (mark-lin mark1) lin    ; alte Cursorpos. = Anfang
              (mark-col mark1) col
        )
        (setf (mark-lin mark2) (screen-lin screen) ; neue Cursorpos. = Ende
              (mark-col mark2) (screen-col screen)
        )
        (refresh-screen screen lin (1+ (screen-lin screen)))
) ) ) )

(defun copy-block (screen) ; Block kopieren (ohne Marken)
  #+DEUTSCH "Block an Cursorposition kopieren"
  #+ENGLISH "copy block to cursor position"
  (multiple-value-bind (lin1 col1 lin2 col2) (get-block screen)
    (and lin1
         (insert-linelist screen
                          (region-to-linelist screen lin1 col1 lin2 col2)
) ) )    )

;-------------------------------------------------------------------------------

;;; Block und Cut-and-Paste-Buffer

;; Enthält umgedrehte Zeilenliste
(defvar *cut-and-paste-buffer* '())

(defun copy-block-buffer (screen)
  #+DEUTSCH "Block in Cut-and-Paste-Buffer übertragen"
  #+ENGLISH "copy block into cut-and-paste buffer"
  (multiple-value-bind (lin1 col1 lin2 col2) (get-block screen)
    (and lin1
         (progn
           (setq *cut-and-paste-buffer*
                 (region-to-linelist screen lin1 col1 lin2 col2)
           )
           t
) ) )    )

(defun delete-block-buffer (screen)
  #+DEUTSCH "Block löschen und in Cut-and-Paste-Buffer übertragen"
  #+ENGLISH "yank block into cut-and-paste buffer"
  (let* ((marks (screen-marks screen))
         (mark1 (aref marks 0))
         (mark2 (aref marks 1)))
    (and mark1 mark2
         (progn
           (setf (aref marks 0) nil (aref marks 1) nil)
           (let ((lin1 (mark-lin mark1))
                 (col1 (mark-col mark1))
                 (lin2 (mark-lin mark2))
                 (col2 (mark-col mark2)))
             (setq *cut-and-paste-buffer*
                   (region-to-linelist screen lin1 col1 lin2 col2)
             )
             (delete-region screen lin1 col1 lin2 col2)
) ) )    ) )

(defun paste-buffer (screen)
  #+DEUTSCH "Inhalt des Cut-and-Paste-Buffer einfügen"
  #+ENGLISH "insert cut-and-paste buffer contents"
  (insert-linelist screen *cut-and-paste-buffer*)
)

;-------------------------------------------------------------------------------

;; Macro zum Auswerten von Formen, wobei Fehler abgefangen werden und den Wert
;; von errorval liefern
(defconstant errorval "ERROR")

(defmacro with-ignored-errors (&body body)
  (let ((blockvar (gensym)))
    `(BLOCK ,blockvar
       (LET ((*ERROR-HANDLER*
               #'(LAMBDA (&REST ARGS)
                   (DECLARE (IGNORE ARGS))
                   (RETURN-FROM ,blockvar ERRORVAL)
            ))   )
         ,@body
     ) )
) )

;; Dito, mit Ausgabe der Fehlermeldung auf *error-output*
(defmacro with-trapped-errors (&body body)
  (let ((blockvar (gensym)))
    `(BLOCK ,blockvar
       (LET ((*ERROR-HANDLER*
               #'(LAMBDA (CONTINUE ERRSTR &REST ARGS)
                   (DECLARE (IGNORE CONTINUE)) ; vorläufig
                   #-CLISP1 (FRESH-LINE *ERROR-OUTPUT*)
                   #-CLISP1 (APPLY #'FORMAT *ERROR-OUTPUT* ERRSTR ARGS)
                   #+CLISP1 ; Workaround um ein Bug in ADJUST-ARRAY, das in
                            ; Erscheinung tritt, wenn der Speicher voll ist:
                            (LET ((STREAM *ERROR-OUTPUT*))
                              (MULTIPLE-VALUE-BIND (NEED ROOM) (ROOM)
                                (DECLARE (IGNORE NEED))
                                (WHEN (< ROOM 1024)
                                  (GC)
                                  (MULTIPLE-VALUE-BIND (NEED ROOM) (ROOM)
                                    (DECLARE (IGNORE NEED))
                                    (WHEN (< ROOM 1024)
                                      (SETQ STREAM *DEBUG-IO*)
                              ) ) ) )
                              (FRESH-LINE STREAM)
                              (APPLY #'FORMAT STREAM ERRSTR ARGS)
                            )
                   (RETURN-FROM ,blockvar ERRORVAL)
            ))  )
         ,@body
     ) )
) )

;===========================================================================
;                        E D I T O R - T O P L E V E L
;===========================================================================

;; Eine key-table ist eine Hashtable  char -> fun,  die Tastendrücken Prozeduren
;; zuordnet. fun ist eine Funktion von einem screen-Argument und gibt einen
;; booleschen Wert zurück: t bei Erfolg, nil bei Mißerfolg

;; Full-Screen-Tabelle
(defconstant full-table (make-hash-table :test #'eql))
;; Read-Only-Tabelle
(defconstant half-table (make-hash-table :test #'eql))
;; Tabelle für Line-Edit
(defconstant line-edit-table (make-hash-table :test #'eql))

;; Control-Table-Default: Nur Escape
(defconstant null-table (make-hash-table :test #'eql))
(setf (gethash #\Escape null-table) '(:LEAVE))
;; Volle Control-Table des Editors
(defconstant control-table (make-hash-table :test #'eql))

(defconstant docstrings-table (make-hash-table :test #'eql))

(defun bind-key (keys flag fun &optional (docstring nil))
  (unless (listp keys) (setq keys (list keys)))
  (when (and (symbolp fun) (not (null fun)))
    (unless docstring (setq docstring (documentation fun 'function)))
    (setq fun (symbol-function fun))
  )
  (let ((tables
          (case flag
            (:CONTROL (list control-table))
            (:ALL (list full-table half-table line-edit-table))
            (:WRITABLE (list full-table line-edit-table))
            (:MULTILINE (list full-table half-table))
            (:AND-WRITABLE-MULTILINE (list full-table))
            (:AND-WRITABLE-NOT-MULTILINE (list line-edit-table))
       )) )
    (dolist (key keys)
      (dolist (table tables)
        (setf (gethash key table) fun)
      )
      (when docstring
        (setf (gethash key docstrings-table) docstring)
    ) )
) )

;; ob der Editor aktiv ist
(defvar *editor-active* nil)
;; Vektor aller Screens des Editors
(defvar *edit-screens* (make-array 13 :initial-element nil))
;; Vektor dazugehöriger Pathnames bzw. Conses (package . env)
(defvar *screen-paths* (make-array 13 :initial-element nil))
;; momentan aktiver Screen
(defvar *active-screen*)

;; Fenster für Fehlermeldungen
(defvar error-screen)
;; Fenster für Traces
(defvar trace-screen)
;; Hilfefenster, enthält Tastenzuordnungen
(defvar help-screen)
;; Hauptfenster (ganzer Bildschirm)
(defvar main-screen)

;; Editor
(defun edit (&optional start-command)
  (if *editor-active*
    (throw 'editor-active start-command) ; Editor nicht rekursiv aufrufen!
    (with-keyboard
      (with-window
        (unless (boundp 'main-screen)
          (setf (aref *edit-screens* 0)
            (setf main-screen (make-screen))
        ) )
        (unless (boundp 'error-screen)
          (setf (aref *edit-screens* 10)
            (setf error-screen
              (make-screen :title " Errors: " :height 10 :width 50
                           :top-lin 2 :left-col (- global-screen-width 53)
        ) ) ) )
        (unless (boundp 'trace-screen)
          (setf (aref *edit-screens* 11)
            (setf trace-screen
              (make-screen :title " Trace: " :height 15 :width 70
                           :top-lin 8 :left-col 3
        ) ) ) )
        (unless (boundp 'help-screen)
          (setf (aref *edit-screens* 12)
            (setf help-screen
              (make-screen :title #+DEUTSCH " Tastenzuordnung "
                                  #+ENGLISH " Key bindings "
                           :height 15 :width 78
          ) ) )
          (insert-linelist help-screen
            (reverse
              '(" ==========================================================================="
                #+DEUTSCH "                         T A S T E N B E L E G U N G"
                #+ENGLISH "                           K E Y   B I N D I N G S"
                " ==========================================================================="
                ""
                ""
          ) )  )
          ;(maphash #'(lambda (key docstring)
          ;             (insert-string help-screen (format nil "~:@C~25T --> ~A~%" key docstring))
          ;             (line-up help-screen)
          ;           )
          ;         docstrings-table
          ;)
          ; Das ist reichlich langsam! Geht's so schneller?
          (insert-linelist help-screen
            (reverse
              (let ((lines '()))
                (maphash #'(lambda (key docstring)
                             (push (format nil "~:@C~25T --> ~A" key docstring) lines)
                           )
                         docstrings-table
                )
                lines
          ) ) )
          (set-cursor help-screen 0 0)
        )
        (unless (boundp '*active-screen*)
          (setf *active-screen* 0)
        )
        (unwind-protect
          (block editor
            ; Ab hier kann der Editor als aktiv angesehen werden.
            ; Schleife zum Abfangen und Behandeln der Kommandos:
            (flet ((handle-command (command &rest args)
                     (catch 'handle-command
                       (case command
                         (:LEAVE (return-from editor))
                         (:ERROR (setq *active-screen* 10))
                         (:TRACE (setq *active-screen* 11))
                         (:HELP (setq *active-screen* 12))
                         (:TOP
                           (if (null (aref *edit-screens* (first args)))
                             (bell)
                             (setq *active-screen* (first args))
                         ) )
                         (:HIDE
                           (if (null (aref *edit-screens* (first args)))
                             (bell)
                             (let ((new-active
                                     (hide-screen (aref *edit-screens* (first args)))
                                  ))
                               (setq *active-screen*
                                     (or (and (not (null new-active))
                                              (position new-active *edit-screens*
                                                        :test #'eq
                                         )    )
                                         0
                         ) ) ) )     )
                         (:DELETE
                           (cond
                             ((< 0 *active-screen* 10)
                               (let ((new-active
                                       (hide-screen (aref *edit-screens* *active-screen*))
                                    ))
                                 (setf (aref *edit-screens* *active-screen*) nil)
                                 (setf (aref *screen-paths* *active-screen*) nil)
                                 (setq *active-screen*
                                       (or (and (not (null new-active))
                                                (position new-active *edit-screens*
                                                          :test #'eq
                                           )    )
                                           0
                             ) ) )     )
                             (t (bell))
                         ) )
                         (:SAVE
                           (unless (aref *screen-paths* *active-screen*)
                             (setf (aref *screen-paths* *active-screen*) (get-save-path))
                           )
                           (let ((screen (aref *edit-screens* *active-screen*))
                                 (destination (aref *screen-paths* *active-screen*)))
                             (if (atom destination) ; Pathname oder Cons?
                               (screen-to-file screen destination)
                               ; Load vom Screen:
                               (let ((f (make-read-from-screen-stream screen 0 0))
                                     (*package* (car destination)) ; *PACKAGE* binden
                                     (env (cdr destination)) ; Evaluator-Environment
                                     (end-of-file "EOF")) ; einmaliges Objekt
                                 (loop
                                   (let ((obj (read f nil end-of-file)))
                                     (when (eql obj end-of-file) (return))
                                     (evalhook obj nil nil env)
                               ) ) )
                         ) ) )
                         (:SAVE-AS
                           (screen-to-file (aref *edit-screens* *active-screen*) (get-save-path))
                         )
                         (:LOAD ; (:LOAD path)
                           (let ((new-active (position nil *edit-screens*)))
                             (if (null new-active)
                               (bell)
                               (let ((path
                                       (if args
                                         (first args)
                                         (line-edit #+DEUTSCH " Lade: "
                                                    #+ENGLISH " File to load: "
                                    )) ) )
                                 (setq path (with-ignored-errors (pathname path)))
                                 (if (eq path errorval)
                                   (bell)
                                   (progn
                                     (setf (aref *edit-screens* new-active)
                                           (file-to-screen path new-active)
                                     )
                                     (setf (aref *screen-paths* new-active) path)
                                     (setq *active-screen* new-active)
                         ) ) ) ) ) )
                         (:FORM ; (:FORM sym package env string)
                           (let ((new-active (position nil *edit-screens*)))
                             (if (null new-active)
                               (bell)
                               (let ((screen (make-screen
                                               :title (format nil " ~A " (first args))
                                               :top-lin new-active :width 78 :height 13
                                    ))       )
                                 (insert-string screen (fourth args))
                                 (insert-line screen)
                                 (set-cursor screen 0 0)
                                 (setf (aref *edit-screens* new-active) screen)
                                 (setf (aref *screen-paths* new-active) (cons (second args) (third args)))
                                 (setq *active-screen* new-active)
                         ) ) ) )
                         (t (bell))
                  )) ) )
              (loop
                (setq start-command
                  (catch 'editor-active
                    (let ((*editor-active* t))
                      ; nächstes Kommando holen und abarbeiten:
                      (apply #'handle-command
                        (or start-command
                            (edit1 (aref *edit-screens* *active-screen*)
                                   control-table
                                   (if (< *active-screen* 10) full-table half-table)
                      ) )   )
                      nil
              ) ) ) )
          ) )
          (doseq (screen *edit-screens*)
            (unless (null screen) (hide-screen screen))
          )
          (screen-clear-screen)
) ) ) ) )

#|
;; Editierfunktion: Editiere ein Fenster
(defun edit-screen (screen &optional (key-table-1 null-table)
                                     (key-table-2 full-table)
                   )
  (edit1 screen key-table-1 key-table-2)
)
|#

;; Defaultfunktion für Tastenzuordnung: Nichts tun, Mißerfolg melden (= NIL)
(defun return-nil (&rest args)
  (declare (ignore args))
  nil
)

;; Editier-Hauptschleife
(defun edit1 (screen key-table-1 key-table-2)
  (activate-screen screen)
  (catch 'edit
    (flet ((read-edit-command ()
             (prog2
               (set-cursor-visible screen) ; Cursor ins Fenster und einschalten
               (read-char *keyboard-input*)
               (screen-cursor-off) ; Cursor abschalten
           ) )
           (execute-edit-command (char)
             (catch 'handle-command
               (if (and (string-char-p char) (char>= char #\Space))
                 ;; normales Zeichen: unter Key :string-char nachschauen
                 (or (funcall (gethash :string-char key-table-2 #'return-nil)
                              screen char
                     )
                     (bell)
                 )
                 ;; sonst: erst Bedeutung für Editier-Ende nachsehen
                 (multiple-value-bind (return-value presentp)
                     (gethash char key-table-1)
                   (when presentp (throw 'edit return-value))
                   ;; sonst Editierfunktion ausführen
                   (or (funcall (gethash char key-table-2 #'return-nil) screen)
                       (bell) ; falls undefiniert oder ohne Erfolg
          )) ) ) ) )
      (loop (execute-edit-command (read-edit-command)))
) ) )

;; Einen Pfad fürs Abspeichern erfragen
(defun get-save-path ()
  (let (path)
    (loop
      (setq path (line-edit #+DEUTSCH " Abspeichern als: "
                            #+ENGLISH " Save as: "
      )          )
      (setq path (with-ignored-errors (pathname path)))
      (unless (eq path errorval) (return))
      (bell)
    )
    path
) )

;; Eine Zeile editieren und Ergebnis zurückliefern
(defun line-edit (title &optional (old ""))
  (let ((query-screen (make-screen :height 1 :width 40 :title title)))
    (insert-string query-screen old)
    (let ((command
            (edit1 query-screen null-table line-edit-table)
         ))
      (hide-screen query-screen)
      (when (eq (first command) ':LEAVE) ; bei Escape
        (throw 'handle-command nil) ; aktuelles Kommando abbrechen
      )
      (copy-seq (aref (screen-text query-screen) 0))
) ) )

;===========================================================================
;                      A R B E I T E N   M I T   F I L E S
;===========================================================================

;; Ein File in einen Screen einlesen, leerer Screen, falls File nicht vorhanden
(defun file-to-screen (path number) ; 1 <= number <= 9
  (let ((screen (make-screen
                  :title (format nil " ~A " (enough-namestring path))
                  :top-lin number :width 78 :height 13
       ))       )
    (when (probe-file path)
      (with-open-file (s path :direction :input) (insert-stream screen s))
      (set-cursor screen 0 0)
    )
    screen
) )

;; Screen in ein File schreiben
(defun screen-to-file (screen file)
  (let* ((text (screen-text screen))
         (text-len-1 (1- (length text)))
        )
    (with-open-file (s file :direction :output :if-exists :rename)
      (write-region screen 0 0 text-len-1 (length (aref text text-len-1)) s)
  ) )
  t
)

;===========================================================================
;  E I N Z E L F U N K T I O N E N   F Ü R   T A S T E N Z U O R D N U N G
;===========================================================================

(defun finish (screen)
  (declare (ignore screen))
  (throw 'edit '(:FINISH))
)

;; Erzeuge einen Stream, der aus dem screen ab Position lin1,col1 bis Position
;; lin2,col2 (optional, Default Textende) liest;
;; Zweiter Wert ist eine Funktion von 0 Argumenten, die die Position, bis zu
;; der gelesen wurde, angibt (als (values lin col)).
;; Solange der Stream verwendet wird, sollten Modifikationen des screen
;; unterbleiben.
(defun make-read-from-screen-stream (screen lin1 col1 &optional lin2 col2)
  (let ((text (screen-text screen)))
    (unless lin2 (setq lin2 (1- (length text))))
    (unless col2 (setq col2 (length (aref text lin2))))
    ; Region von (lin1,col1) bis (lin2,col2) lesen:
    (let* ((lastlin nil)
           (lastcol nil)
           (stream
             (make-buffered-input-stream
               ; Funktion, die abwechselnd ein Textstück und ein Newline
               ; durchreicht, bis die Region zu Ende ist:
               #'(lambda ()
                   ; lin1, col1 laufen.
                   (if (or (> lin1 lin2) (and (= lin1 lin2) (>= col1 col2)))
                     nil ; Ende der Region
                     (let ((line (aref text lin1)))
                       (setq lastlin lin1 lastcol col1)
                       (if (>= col1 (length line)) ; am Zeilenende?
                         ; Zeilenende: Newline durchreichen
                         (progn
                           (incf lin1) (setq col1 0)
                           (values newline-as-string 0 1)
                         )
                         ; sonst: Zeile bzw. Zeilenrest durchreichen
                         (values line col1 (setq col1 (length line)))
                 ) ) ) )
               nil
          )) )
      (values
        stream
        ; Funktion, die die Position im Screen liefert, an der der Stream
        ; sich gerade befindet:
        ; Stream hat einen String und einen internen Index.
        ; Zustand 1 (sofort nach Initialisierung):
        ;           String = "", Index = 0, liefere (lin1,col1).
        ; Zustand 2 (nach Zeilen-Übergabe):
        ;           String = Zeile, lastcol <= index <= col1, lastlin = lin1,
        ;           liefere (lin1,index).
        ; Zustand 3 (nach Newline-Übergabe):
        ;           String = Newline-as-String, col1 = 0,
        ;           bei Index = 0 liefere (lastlin,lastcol),
        ;           bei Index = 1 liefere (lin1,col1).
        #'(lambda ()
            (let ((index (sys::buffered-input-stream-index stream)))
              (if (eql index 0)
                ; Zustand 1 oder 2 oder 3a
                (values lastlin lastcol)
                ; Zustand 2 oder 3b
                (values lin1 (min index col1))
          ) ) )
      )
) ) )

;; Erzeuge einen Stream, der ab Cursorposition in den screen schreibt
(defun make-write-to-screen-stream (screen)
  (make-buffered-output-stream
    #'(lambda (string) (insert-string screen string))
    (screen-col screen)
) )

;; Erzeuge einen Stream, der ab Textende in den screen schreibt und ein
;; Flag setzt, wenn etwas geschrieben wurde
(defmacro make-write-to-screen-stream-with-flag (screenform flagvar)
  (let ((stringvar (gensym)) (screenvar (gensym)))
    `(LET ((,screenvar ,screenform))
       (CURSOR-TO-END-OF-TEXT ,screenvar)
       (MAKE-BUFFERED-OUTPUT-STREAM
         #'(LAMBDA (,stringvar)
             (INSERT-STRING ,screenvar ,stringvar)
             (WHEN (PLUSP (LENGTH ,stringvar)) (SETQ ,flagvar T))
           )
         (SCREEN-COL ,screenvar)
) )  ) )

;; Lies ein Objekt aus dem angegebenen Bereich, werte es aus und schreibe das
;; Ergebnis in den Haupt-Text.
;; Vorläufige Version: Keine Umleitung von *query-io* und *debug-io* auf
;; Fenster.
(defun eval-region (screen lin1 col1 lin2 col2)
  (unless lin1 (return-from eval-region nil))
  (let* ((errorflag nil)
         (traceflag nil)
         (instream (make-read-from-screen-stream screen lin1 col1 lin2 col2))
         (*standard-output* (make-write-to-screen-stream main-screen))
         (*error-output*
           (make-write-to-screen-stream-with-flag error-screen errorflag))
         (*trace-output*
           (make-write-to-screen-stream-with-flag trace-screen traceflag))
         (results
           (multiple-value-list (with-trapped-errors (eval (read instream))))
        ))
    (close instream)
    ;; Werte dazu
    (unless (or (null results) errorflag)
      (fresh-line)
      (loop
        (prin1 (pop results))
        (when (null results) (return))
        (write-char #\Space) (write-char #\;) (terpri)
    ) )
    (fresh-line)
    (close *standard-output*)
    (close *error-output*)
    (close *trace-output*)
    (when errorflag (throw 'edit '(:ERROR)))
    (when traceflag (throw 'edit '(:TRACE)))
    t
) )

;; Lies ein Objekt aus dem Block, werte es aus und schreibe das Ergebnis
;; in den Text.
(defun eval-block (screen)
  #+DEUTSCH "Block-Inhalt auswerten"
  #+ENGLISH "evaluate block contents"
  (multiple-value-call #'eval-region screen (get-block screen))
)

(defun get-whitespace-right (screen &optional (lin (screen-lin screen))
                                              (col (screen-col screen)) )
  (let* ((text (screen-text screen))
         (text-len-1 (1- (length text)))
         (line (aref text lin)))
    (loop
      (let ((col1 (position #\Space line :start col :test-not #'eql)))
        (when col1 (return (values lin col1)))
      )
      (when (eql lin text-len-1) (return nil))
      (incf lin)
      (setq col 0)
      (setq line (aref text lin))
) ) )

(defun skip-whitespace-right (screen)
  #+DEUTSCH "Whitespace nach rechts überspringen"
  #+ENGLISH "skip whitespace right"
  (multiple-value-bind (lin col) (get-whitespace-right screen)
    (and lin
         (set-cursor screen lin col)
) ) )

; Eine Kopie der Readtable *readtable*, modifiziert für den Syntaxcheck.
(defun modified-readtable ()
  (let ((readtable (copy-readtable)))
    (set-macro-character #\|
      #'(lambda (stream char)
          (declare (ignore char))
          (when (eql (peek-char nil stream nil) #\#)
            (error #+DEUTSCH "~S von ~S: |# ist nur nach #| zulässig."
                   #+ENGLISH "~S from ~S: |# is legal only after #|"
                   'read stream
        ) ) )
      nil ; terminating macro character
      readtable
    )
    readtable
) )

(defun get-next-object (screen &optional (old-lin (screen-lin screen))
                                         (old-col (screen-col screen))
                                         (readtable (modified-readtable)) )
  (multiple-value-bind (lin col) (get-whitespace-right screen old-lin old-col)
    (if lin
      (multiple-value-bind (instream get-end-pos)
          (make-read-from-screen-stream screen lin col)
        (unwind-protect
          (if (eq (with-ignored-errors ; Errors abfangen
                    (let ((*read-suppress* t) ; nur Syntaxcheck
                          (sys::*backquote-level* most-positive-fixnum) ; Bei Komma kein Error!
                          (*readtable* readtable)) ; |# soll Error liefern
                      (read-preserving-whitespace instream t nil t)
                  ) )
                  errorval
              )
            (values nil nil nil nil)
            (multiple-value-call #'values lin col (funcall get-end-pos))
          )
          (close instream)
      ) )
      (values nil nil nil nil)
) ) )

(defun mark-next-object (screen)
  #+DEUTSCH "Nächstes LISP-Objekt markieren"
  #+ENGLISH "mark next Lisp object"
  (multiple-value-call #'mark-region screen (get-next-object screen))
)

(defun get-toplevel-form (screen)
  (let ((text (screen-text screen))
        (lin (screen-lin screen))
        (col (screen-col screen)))
    ;; Klettere Zeilen hoch. Zeilen, die (nach evtl. Spaces) mit Semikolon
    ;; oder Klammer zu beginnen, werden ignoriert. Zeilen, deren Einrücktiefe
    ;; größer als eine weiter unten angetroffene ist, werden ebenfalls
    ;; ignoriert. Passiert eine Zeile diese Kriterien, wird versucht, ab ihr
    ;; zu lesen, und zwischen dem Ende der dabei erkannten Form und der
    ;; aktuellen Position darf nur Whitespace vorkommen.
    ; 1. Schritt: Whitespace nach links übergehen:
    (let ((line (aref text lin)))
      (loop
        (let ((col1 (position #\Space line :end col :test-not #'eql :from-end t)))
          (when col1 ; Non-Space gefunden, col verkleinern
            (setq col (1+ col1))
            (return)
          )
          ; Keines gefunden, probiere Zeile davor:
          (when (eql lin 0) (setq col 0) (return))
          (decf lin)
          (setq line (aref text lin))
          (setq col (length line))
    ) ) )
    ; 2. Schritt: Hochklettern:
    (let ((readtable (modified-readtable)) ; modifizierte Readtable pre-allozieren
          (lin1 lin)
          (older-marks '())
          (older-indent most-positive-fixnum))
      (loop
        (let* ((line (aref text lin1))
               (indent (position #\Space line :test-not #'eql)))
          (when (and indent
                     (not (member (char line indent) '( #\; #\) ))) ; (
                     (<= indent older-indent)
                )
            (setq older-indent indent)
            (multiple-value-bind (lin0 col0 lin2 col2)
                (get-next-object screen lin1 indent readtable)
              (when lin0
                ; Ein Objekt geht von (lin0,col0) bis (lin2,col2).
                (when (or (< lin0 lin)
                          (and (= lin0 lin) (<= col0 col))
                      )
                  ; Es fängt vor (lin,col) an.
                  (when (or (< lin lin2)
                            (and (= lin lin2) (<= col col2))
                        )
                    ; Es hört hinter (lin,col) auf.
                    (push (list lin0 col0 lin2 col2) older-marks)
        ) ) ) ) ) )
        (when (eql lin1 0) (return))
        (decf lin1)
      )
      ; Wenn passende Objekte gefunden wurden, dann liefere den äußersten:
      (if older-marks
        (values-list (first older-marks))
        (values nil nil nil nil)
      )
) ) )

(defun mark-toplevel-form (screen)
  #+DEUTSCH "Toplevel-Form markieren"
  #+ENGLISH "mark surrounding top level form"
  (multiple-value-call #'mark-region screen (get-toplevel-form screen))
)

(defun eval-toplevel-form (screen)
  #+DEUTSCH "Toplevel-Form auswerten"
  #+ENGLISH "evaluate surrounding top level form"
  (multiple-value-call #'eval-region screen (get-toplevel-form screen))
)

(defun get-next-tab-pos (screen lin col)
  (let* ((text (screen-text screen))
         (line (aref text lin))
         (line-len (length line))
        )
    (cond ((>= col line-len) line-len)
          ((eql (char line col) #\Space)
            (or (position #\Space line :start col :test-not #'eql) line-len)
          )
          ((eql (char line col) #\( ) ; )
            (min (+ col 2) line-len)
          )
          (t (let ((col1 (position #\Space line :start col)))
               (if col1
                 (or (position #\Space line :start col1 :test-not #'eql)
                     line-len
                 )
                 line-len
) ) )     )  ) )

(defun cursor-to-col (screen col)
  (let* ((text (screen-text screen))
         (lin (screen-lin screen))
         (line (aref text lin))
         (line-len (length line))
        )
    (when (> col line-len)
      (resize-array line col)
      (fill line #\Space :start line-len)
      (refresh-line screen lin line-len)
    )
    (set-cursor screen lin col)
) )

(defun next-indent (screen)
  #+DEUTSCH "Leerstellen bis zur nächsten Einrückung"
  #+ENGLISH "insert spaces up to next tab stop"
  (let ((lin (screen-lin screen))
        (col (screen-col screen)))
    (if (eql lin 0)
      (cursor-to-col screen (+ col 2))
      (cursor-to-col screen (get-next-tab-pos screen (1- lin) col))
) ) )

(defvar *search-string* "") ; String, nach dem gesucht werden soll

(defun search-first (screen)
  #+DEUTSCH "Nach Textstück Suchen"
  #+ENGLISH "search for a string"
  (setq *search-string*
    (line-edit #+DEUTSCH " Suche: "
               #+ENGLISH " Search: "
               *search-string*
  ) )
  (search-next screen)
)

(defun search-next (screen)
  #+DEUTSCH "Weitersuchen"
  #+ENGLISH "continue searching"
  (let* ((text (screen-text screen))
         (lin (screen-lin screen))
         (col (screen-col screen))
         (text-len (length text))
         (index (if (< col (length (aref text lin)))
                  (search *search-string* (aref text lin) :start2 (1+ col))
                  nil
        ))      )
    (if index
      (set-cursor screen lin index)
      (loop (when (eql (incf lin) text-len) (return nil))
            (setq index (search *search-string* (aref text lin)))
            (when index (return (set-cursor screen lin index)))
) ) ) )

;===========================================================================
;                        T A S T E N B E L E G U N G
;===========================================================================

;; Brunos Tastenbelegung:

#+(or ATARI DOSE)
(progn

(defun C-H-doc (n)
  (format nil #+DEUTSCH "Fenster Nr. ~D nach oben bringen"
              #+ENGLISH "show window ~D"
          (1+ n)
) )
(defun M-H-doc (n)
  (format nil #+DEUTSCH "Fenster Nr. ~D unsichtbar machen"
              #+ENGLISH "hide window ~D"
          (1+ n)
) )

(bind-key #\C-F1       :control '(:TOP 0) (C-H-doc 0))
(bind-key #\C-F2       :control '(:TOP 1) (C-H-doc 1))
(bind-key #\C-F3       :control '(:TOP 2) (C-H-doc 2))
(bind-key #\C-F4       :control '(:TOP 3) (C-H-doc 3))
(bind-key #\C-F5       :control '(:TOP 4) (C-H-doc 4))
(bind-key #\C-F6       :control '(:TOP 5) (C-H-doc 5))
(bind-key #\C-F7       :control '(:TOP 6) (C-H-doc 6))
(bind-key #\C-F8       :control '(:TOP 7) (C-H-doc 7))
(bind-key #\C-F9       :control '(:TOP 8) (C-H-doc 8))
(bind-key #\C-F10      :control '(:TOP 9) (C-H-doc 9))

#+ATARI
(bind-key #\Help       :control '(:HELP) #+DEUTSCH "Hilfefenster (diesen Text) nach oben bringen"
                                         #+ENGLISH "show help window (this text)"
)
#+DOSE
(bind-key #\M-H        :control '(:HELP) #+DEUTSCH "Hilfefenster (diesen Text) nach oben bringen"
                                         #+ENGLISH "show help window (this text)"
)

(bind-key #\C-E        :control '(:ERROR) #+DEUTSCH "Errorfenster nach oben bringen"
                                          #+ENGLISH "show error window"
)
(bind-key #\C-T        :control '(:TRACE) #+DEUTSCH "Tracefenster nach oben bringen"
                                          #+ENGLISH "show trace window"
)

(bind-key #\M-F2       :control '(:HIDE 1) (M-H-doc 1))
(bind-key #\M-F3       :control '(:HIDE 2) (M-H-doc 2))
(bind-key #\M-F4       :control '(:HIDE 3) (M-H-doc 3))
(bind-key #\M-F5       :control '(:HIDE 4) (M-H-doc 4))
(bind-key #\M-F6       :control '(:HIDE 5) (M-H-doc 5))
(bind-key #\M-F7       :control '(:HIDE 6) (M-H-doc 6))
(bind-key #\M-F8       :control '(:HIDE 7) (M-H-doc 7))
(bind-key #\M-F9       :control '(:HIDE 8) (M-H-doc 8))
(bind-key #\M-F10      :control '(:HIDE 9) (M-H-doc 9))

(bind-key #\M-Q        :control '(:DELETE) #+DEUTSCH "oberes Fenster wegwerfen"
                                           #+ENGLISH "delete current window"
)
(bind-key #\M-X        :control '(:LOAD) #+DEUTSCH "File laden"
                                         #+ENGLISH "load file"
)
(bind-key #\M-S        :control '(:SAVE) #+DEUTSCH "oberes Fenster abspeichern"
                                         #+ENGLISH "store to file"
)
(bind-key #\M-W        :control '(:SAVE-AS) #+DEUTSCH "oberes Fenster als neues File abspeichern"
                                            #+ENGLISH "store to new file"
)

(bind-key #\Escape     :control '(:LEAVE) #+DEUTSCH "Editor verlassen"
                                          #+ENGLISH "quit editor"
)

(bind-key :string-char :writable #'insert-char)

;; Ziffernblock wie gewöhnliche Tasten behandeln, dazu Shift-Space
(dolist (c '(#\( #\) #\+ #\- #\* #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\,))
  (let* ((c c)
         (keypad-c (set-char-bit c :hyper t))
         (sh-keypad-c (set-char-bit c :super t)))
    (bind-key (list keypad-c sh-keypad-c)
              :writable
              #'(lambda (screen) (insert-char screen c))
) ) )
#-DOSE (bind-key #\S-Space     :writable  #'(lambda (screen) (insert-char screen #\Space)))
#+DOSE (progn
(bind-key #\F11         :writable  #'(lambda (screen) (insert-char screen #\()))
(bind-key #\F12         :writable  #'(lambda (screen) (insert-char screen #\))))
)

(bind-key #\Up          :multiline 'cursor-up)
(bind-key #\Down        :multiline 'cursor-down)
(bind-key #\Left        :all       'cursor-left)
(bind-key #\Right       :all       'cursor-right)
#-DOSE (progn
(bind-key #\S-Up        :multiline 'line-up)
(bind-key #\S-Down      :multiline 'line-down)
(bind-key #\C-Up        :multiline 'page-up)
(bind-key #\C-Down      :multiline 'page-down)
(bind-key #\S-Left      :all       'cursor-to-start-of-line)
(bind-key #\S-Right     :all       'cursor-to-end-of-line)
)
#+DOSE (progn
(bind-key #\C-Up        :multiline 'line-up)
(bind-key #\C-Down      :multiline 'line-down)
(bind-key #\PgUp        :multiline 'page-up)
(bind-key #\PgDn        :multiline 'page-down)
(bind-key #\Home        :all       'cursor-to-start-of-line)
(bind-key #\End         :all       'cursor-to-end-of-line)
)

; Emacs-like:
(bind-key #\C-P         :multiline 'cursor-up)
(bind-key #\C-N         :multiline 'cursor-down)
(bind-key #\C-S         :all       'cursor-left)
(bind-key #\C-D         :all       'cursor-right)
#-DOSE (progn
(bind-key #\S-C-P       :multiline 'line-up)
(bind-key #\S-C-N       :multiline 'line-down)
(bind-key #\S-C-S       :all       'cursor-to-start-of-line)
(bind-key #\S-C-D       :all       'cursor-to-end-of-line)
)

#+ATARI (progn
(bind-key #\Home        :multiline 'cursor-to-start-of-text)
(bind-key #\S-Home      :multiline 'cursor-to-end-of-text)
)
#+DOSE (progn
(bind-key #\C-PgUp      :multiline 'cursor-to-start-of-text)
(bind-key #\C-PgDn      :multiline 'cursor-to-end-of-text)
)

(bind-key #\C-B         :all       'set-block-start)
(bind-key #\C-K         :all       'set-block-end)
(bind-key #\M-B         :all       'cursor-to-start-of-block)
(bind-key #\M-K         :all       'cursor-to-end-of-block)
(bind-key #\C-H         :all       'hide-block)

(bind-key '(#\Return #\Enter) :and-writable-multiline 'insert-line)
(bind-key '(#\Return #\Enter) :and-writable-not-multiline 'finish)
(bind-key #\C-Y         :and-writable-multiline 'delete-line)
(bind-key #\C-J         :and-writable-multiline 'combine-lines)
(bind-key #\Delete      :writable  'delete-char)
#+ATARI (progn
(bind-key #\S-Delete    :and-writable-multiline 'delete-char-1)
(bind-key #\S-Delete    :and-writable-not-multiline 'delete-char)
)
#+DOSE (progn
(bind-key #\C-Delete    :and-writable-multiline 'delete-char-1)
(bind-key #\C-Delete    :and-writable-not-multiline 'delete-char)
)
(bind-key #\Backspace   :writable  'backspace)
#+ATARI (progn
(bind-key #\S-Backspace :and-writable-multiline 'backspace-1)
(bind-key #\S-Backspace :and-writable-not-multiline 'backspace)
)
#+DOSE (progn
(bind-key #\C-Backspace :and-writable-multiline 'backspace-1)
(bind-key #\C-Backspace :and-writable-not-multiline 'backspace)
)

#+ATARI
(bind-key #\C-Space     :all       'skip-whitespace-right)
(bind-key #\C-Right     :all       'mark-next-object)
(bind-key #\C-Enter     :all       'mark-toplevel-form)
#+ATARI
(bind-key #\S-Enter     :and-writable-multiline 'eval-toplevel-form)
#+DOSE
(bind-key '(#\C-Return #\C-Enter) :and-writable-multiline 'eval-toplevel-form)
;(bind-key #\C-E         :and-writable-multiline 'eval-block)
;(bind-key #\C-E         :and-writable-multiline 'eval-buffer)

(bind-key #\C-X         :writable  'delete-block-buffer)
(bind-key #\C-C         :all       'copy-block-buffer)
(bind-key #\C-V         :writable  'paste-buffer)
#+ATARI
(bind-key #\S-C-X       :writable  'delete-block)
;(bind-key #\M-C         :writable  'copy-block)
;(bind-key #\M-V         :writable  'move-block)

(bind-key #\Tab         :writable  'next-indent)

(bind-key #\C-0         :all       (set-mark-fn 0) (set-mark-doc 0))
(bind-key #\C-1         :all       (set-mark-fn 1) (set-mark-doc 1))
(bind-key #\C-2         :all       (set-mark-fn 2) (set-mark-doc 2))
(bind-key #\C-3         :all       (set-mark-fn 3) (set-mark-doc 3))
(bind-key #\C-4         :all       (set-mark-fn 4) (set-mark-doc 4))
(bind-key #\C-5         :all       (set-mark-fn 5) (set-mark-doc 5))
(bind-key #\C-6         :all       (set-mark-fn 6) (set-mark-doc 6))
(bind-key #\C-7         :all       (set-mark-fn 7) (set-mark-doc 7))
(bind-key #\C-8         :all       (set-mark-fn 8) (set-mark-doc 8))
(bind-key #\C-9         :all       (set-mark-fn 9) (set-mark-doc 9))
(bind-key #\M-0         :all       (cursor-to-mark-fn 0) (cursor-to-mark-doc 0))
(bind-key #\M-1         :all       (cursor-to-mark-fn 1) (cursor-to-mark-doc 1))
(bind-key #\M-2         :all       (cursor-to-mark-fn 2) (cursor-to-mark-doc 2))
(bind-key #\M-3         :all       (cursor-to-mark-fn 3) (cursor-to-mark-doc 3))
(bind-key #\M-4         :all       (cursor-to-mark-fn 4) (cursor-to-mark-doc 4))
(bind-key #\M-5         :all       (cursor-to-mark-fn 5) (cursor-to-mark-doc 5))
(bind-key #\M-6         :all       (cursor-to-mark-fn 6) (cursor-to-mark-doc 6))
(bind-key #\M-7         :all       (cursor-to-mark-fn 7) (cursor-to-mark-doc 7))
(bind-key #\M-8         :all       (cursor-to-mark-fn 8) (cursor-to-mark-doc 8))
(bind-key #\M-9         :all       (cursor-to-mark-fn 9) (cursor-to-mark-doc 9))

#+ATARI (progn
(bind-key #\S-C-Left    :writable  'clear-start-of-line)
(bind-key #\S-C-Right   :writable  'clear-end-of-line)
)
#+DOSE (progn
(bind-key #\M-Left      :writable  'clear-start-of-line)
(bind-key #\M-Right     :writable  'clear-end-of-line)
)

#+ATARI
(bind-key #\S-C-L       :multiline 'search-first)
#+DOSE
(bind-key #\M-L         :multiline 'search-first)
(bind-key #\C-L         :multiline 'search-next)

)

#+UNIX
(progn ; noch sehr rudimentär und unvollständig! ??

(defun C-H-doc (n)
  (format nil #+DEUTSCH "Fenster Nr. ~D nach oben bringen"
              #+ENGLISH "show window ~D"
          (1+ n)
) )

(bind-key #\F1         :control '(:TOP 0) (C-H-doc 0))
(bind-key #\F2         :control '(:TOP 1) (C-H-doc 1))
(bind-key #\F3         :control '(:TOP 2) (C-H-doc 2))
(bind-key #\F4         :control '(:TOP 3) (C-H-doc 3))
(bind-key #\F5         :control '(:TOP 4) (C-H-doc 4))
(bind-key #\F6         :control '(:TOP 5) (C-H-doc 5))
(bind-key #\F7         :control '(:TOP 6) (C-H-doc 6))
(bind-key #\F8         :control '(:TOP 7) (C-H-doc 7))
(bind-key #\F9         :control '(:TOP 8) (C-H-doc 8))
(bind-key #\F10        :control '(:TOP 9) (C-H-doc 9))

(bind-key #\C-G        :control '(:HELP) #+DEUTSCH "Hilfefenster (diesen Text) nach oben bringen"
                                         #+ENGLISH "show help window (this text)"
)
(bind-key #\C-E        :control '(:ERROR) #+DEUTSCH "Errorfenster nach oben bringen"
                                          #+ENGLISH "show error window"
)
(bind-key #\C-T        :control '(:TRACE) #+DEUTSCH "Tracefenster nach oben bringen"
                                          #+ENGLISH "show trace window"
)

(bind-key #\C-Q        :control '(:DELETE) #+DEUTSCH "oberes Fenster wegwerfen"
                                           #+ENGLISH "delete current window"
)
(bind-key #\C-X        :control '(:LOAD) #+DEUTSCH "File laden"
                                         #+ENGLISH "load file"
)
(bind-key #\C-W        :control '(:SAVE-AS) #+DEUTSCH "oberes Fenster als neues File abspeichern"
                                            #+ENGLISH "store to new file"
)

(bind-key '#\Escape    :control '(:LEAVE) #+DEUTSCH "Editor verlassen"
                                          #+ENGLISH "quit editor"
)

(bind-key :string-char :writable #'insert-char)

(bind-key #\Up          :multiline 'cursor-up)
(bind-key #\Down        :multiline 'cursor-down)
(bind-key #\Left        :all       'cursor-left)
(bind-key #\Right       :all       'cursor-right)
(bind-key #\PgUp        :multiline 'page-up)
(bind-key #\PgDn        :multiline 'page-down)

; Emacs-like:
(bind-key #\C-P         :multiline 'cursor-up)
(bind-key #\C-N         :multiline 'cursor-down)
(bind-key #\C-S         :all       'cursor-left)
(bind-key #\C-D         :all       'cursor-right)
(bind-key #\C-A         :all       'cursor-to-start-of-line)
(bind-key #\C-F         :all       'cursor-to-end-of-line)

(bind-key #\C-B         :all       'set-block-start)
(bind-key #\C-K         :all       'set-block-end)
(bind-key #\C-U         :all       'hide-block)

(bind-key #\Return      :and-writable-multiline 'insert-line)
(bind-key #\Return      :and-writable-not-multiline 'finish)
(bind-key #\C-Y         :and-writable-multiline 'delete-line)
(bind-key #\C-J         :and-writable-multiline 'combine-lines)
(bind-key '(#\Backspace #\Delete) :writable  'backspace)

(bind-key #\C-V         :all       'skip-whitespace-right)
(bind-key #\C-R         :all       'mark-next-object)
(bind-key '(#\C-O #\F11) :all      'mark-toplevel-form)
(bind-key '(#\C-L #\F12) :and-writable-multiline 'eval-toplevel-form)

(bind-key #\Tab         :writable  'next-indent)

)

;; *undo* behandeln ??
;; #\C-R für Repeat ??

;###############################################################################

;; ob der eingebaute Editor benutzt wird:
(defparameter *use-ed* t)

(fmakunbound 'ed)
; Erweiterte Version von ED in DEFS1.LSP:
(defun ed (&optional arg &aux sym fun def)
  (if (null arg)
    (if *use-ed*
      (edit)
      (edit-file "")
    )
    (if (or (pathnamep arg) (stringp arg))
      (if *use-ed*
        (edit `(:LOAD ,(namestring arg)))
        (edit-file arg)
      )
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
             ))    )
          (if *use-ed*
            (edit `(:FORM ,sym ,*package* ,env
                          ,(write-to-string def :escape t :pretty t)
            )      )
            (let ((tempfile (editor-tempfile)))
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
          ) )
          sym
        )
        (error #+DEUTSCH "~S ist nicht editierbar."
               #+ENGLISH "~S cannot be edited."
               #+FRANCAIS "~S ne peut pas être édité."
               arg
) ) ) ) )

