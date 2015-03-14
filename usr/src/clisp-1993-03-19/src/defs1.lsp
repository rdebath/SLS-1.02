;;;; Einige Definitionen von Standard-Funktionen in LISP
;;;; 1.8.1989, 2.9.1989, 8.10.1989

(in-package "LISP")
(export '(doseq dohash #-UNIX *default-time-zone* dir))
(in-package "SYSTEM")


;;; Funktionen für Symbole (Kapitel 10)

(defun copy-symbol (symbol &optional flag)
                   ;; Common LISP, S. 169
  (let ((sym (make-symbol (symbol-name symbol))))
    (when flag
      (when (boundp symbol) (set sym (symbol-value symbol)))
      (when (fboundp symbol) (sys::%putd sym (symbol-function symbol)))
      (sys::%putplist sym (copy-list (symbol-plist symbol)))
    )
    sym
) )

(let ((gentemp-count 0))
  (defun gentemp (&optional (prefix "T") (package *package*))
                 ;; Common LISP, S. 170
    (loop
      (setq gentemp-count (1+ gentemp-count))
      (multiple-value-bind (sym flag)
        (intern
          (string-concat prefix
            (write-to-string gentemp-count :base 10 :radix nil)
          )
          package
        )
        (unless flag (return sym))
) ) ) )


;;; Macros für Packages (Kapitel 11), S. 187-188

(defmacro do-symbols ((var &optional (packageform '*package*) (resultform nil))
                      &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (setq declarations (if declarations `((DECLARE ,@declarations)) '()))
    (let ((packvar (gensym)))
      `(BLOCK NIL
         (LET ((,packvar ,packageform))
           (LET ((,var NIL))
             ,@declarations
             ,var ; var wird nur zum Schein ausgewertet
             (SYSTEM::MAP-SYMBOLS #'(LAMBDA (,var) ,@declarations ,@body-rest) ,packvar)
             ,resultform
       ) ) )
) ) )

(defmacro do-external-symbols ((var &optional (packageform '*package*) (resultform nil))
                               &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (setq declarations (if declarations `((DECLARE ,@declarations)) '()))
    (let ((packvar (gensym)))
      `(BLOCK NIL
         (LET ((,packvar ,packageform))
           (LET ((,var NIL))
             ,@declarations
             ,var ; var wird nur zum Schein ausgewertet
             (SYSTEM::MAP-EXTERNAL-SYMBOLS #'(LAMBDA (,var) ,@declarations ,@body-rest) ,packvar)
             ,resultform
       ) ) )
) ) )

(defmacro do-all-symbols ((var &optional (resultform nil))
                          &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (setq declarations (if declarations `((DECLARE ,@declarations)) '()))
    `(BLOCK NIL
       (LET ((,var NIL))
         ,@declarations
         ,var ; var wird nur zum Schein ausgewertet
         (SYSTEM::MAP-ALL-SYMBOLS #'(LAMBDA (,var) ,@declarations ,@body-rest))
         ,resultform
     ) )
) )

;;; Modulverwaltung (Kapitel 11.8), CLTL S. 188

(defvar *modules* nil)

(defun provide (module-name)
  (setq *modules* (adjoin (string module-name) *modules* :test #'string=))
)

(defun require (module-name &optional (pathname nil p-given))
  (unless (member (string module-name) *modules* :test #'string-equal)
    (unless p-given (setq pathname (pathname module-name)))
    (let (#-CLISP(*default-pathname-defaults* '#""))
      (if (atom pathname) (load pathname) (mapcar #'load pathname))
    )
) )


;;; Konstanten für Zahlen (Kapitel 12)

; vgl. File INTLOG.TXT
(defconstant boole-clr 0)
(defconstant boole-set 15)
(defconstant boole-1 10)
(defconstant boole-2 12)
(defconstant boole-c1 5)
(defconstant boole-c2 3)
(defconstant boole-and 8)
(defconstant boole-ior 14)
(defconstant boole-xor 6)
(defconstant boole-eqv 9)
(defconstant boole-nand 7)
(defconstant boole-nor 1)
(defconstant boole-andc1 4)
(defconstant boole-andc2 2)
(defconstant boole-orc1 13)
(defconstant boole-orc2 11)

; Zum Wiedereinlesen von BYTEs:
(defun make-byte (&key size position) (byte size position))


;;; Konstanten für Zeichen (Kapitel 13)

(defconstant char-code-limit 256)
(defconstant char-font-limit 16)
(defconstant char-bits-limit 16)
                   ;; Common LISP, S. 233, 234

(defconstant char-control-bit 1)
(defconstant char-meta-bit 2)
(defconstant char-super-bit 4)
(defconstant char-hyper-bit 8)
                   ;; Common LISP, S. 243


;;; Funktionen für Sequences (Kapitel 14)

(defmacro doseq ((var seqform &optional resultform) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (setq declarations (if declarations `((DECLARE ,@declarations)) '()))
    (let ((seqvar (gensym)))
      `(BLOCK NIL
         (LET ((,seqvar ,seqform))
           (LET ((,var NIL))
             ,@declarations
             ,var ; var wird nur zum Schein ausgewertet
             (MAP NIL
                  #'(LAMBDA (,var) ,@declarations (TAGBODY ,@body-rest))
                  ,seqvar
             )
             ,resultform
       ) ) )
) ) )


;;; Funktionen für Listen (Kapitel 15)

; Hilfsversion von MEMBER, die das :KEY-Argument auch auf item anwendet:
(defun sys::member1 (item list &rest rest &key test test-not key)
  (declare (ignore test test-not))
  (unless key (setq key #'identity))
  (apply #'member (funcall key item) list rest)
)

(defun union (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((endp list1) list2)
        ((apply #'sys::member1 (car list1) list2 rest)
         (apply #'union (cdr list1) list2 rest))
        (t (cons (car list1) (apply #'union (cdr list1) list2 rest)))
) )

(defun nunion (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((endp list1) list2)
        ((apply #'sys::member1 (car list1) list2 rest)
         (apply #'nunion (cdr list1) list2 rest))
        (t (rplacd list1 (apply #'nunion (cdr list1) list2 rest)))
) )

(defun intersection (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((endp list1) nil)
        ((apply #'sys::member1 (car list1) list2 rest)
         (cons (car list1)
               (apply #'intersection (cdr list1) list2 rest)))
        (t (apply #'intersection (cdr list1) list2 rest))
) )

(defun nintersection (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((endp list1) nil)
        ((apply #'sys::member1 (car list1) list2 rest)
         (rplacd list1 (apply #'nintersection (cdr list1) list2 rest)) )
        (t (apply #'nintersection (cdr list1) list2 rest))
) )

(defun set-difference (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((endp list1) nil)
        ((not (apply #'sys::member1 (car list1) list2 rest))
         (cons (car list1)
               (apply #'set-difference (cdr list1) list2 rest)
        ))
        (t (apply #'set-difference (cdr list1) list2 rest))
) )

(defun nset-difference (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((endp list1) nil)
        ((not (apply #'sys::member1 (car list1) list2 rest))
         (rplacd list1 (apply #'nset-difference (cdr list1) list2 rest)) )
        (t (apply #'nset-difference (cdr list1) list2 rest))
) )

(defun set-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (append (apply #'set-difference list1 list2 rest)
          (apply #'set-difference list2 list1 rest)
) )

(defun nset-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (nconc (apply #'set-difference list1 list2 rest)
         (apply #'nset-difference list2 list1 rest)
) )

(defun subsetp (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (do ((l list1 (cdr l)))
      ((endp l) t)
    (if (not (apply #'sys::member1 (car l) list2 rest)) (return nil))
) )


;;; Funktionen für Hash-Tabellen (Kapitel 16)

(defmacro dohash ((keyvar valuevar HTform &optional resultform) &body body &environment env)
  (multiple-value-bind (body-rest declarations) (system::parse-body body nil env)
    (setq declarations (if declarations `((DECLARE ,@declarations)) '()))
    (let ((HTvar (gensym)))
      `(BLOCK NIL
         (LET ((,HTvar ,HTform))
           (LET ((,keyvar NIL) (,valuevar NIL))
             ,@declarations
             ,keyvar ,valuevar ; werden nur zum Schein ausgewertet
             (MAPHASH
               #'(LAMBDA (,keyvar ,valuevar) ,@declarations (TAGBODY ,@body-rest))
               ,HTvar
             )
             ,resultform
       ) ) )
) ) )


;;; Funktionen für Strings (Kapitel 18)

(defun string-trim (character-bag string)
  (sys::string-both-trim character-bag character-bag string)
)

(defun string-left-trim (character-bag string)
  (sys::string-both-trim character-bag nil string)
)

(defun string-right-trim (character-bag string)
  (sys::string-both-trim nil character-bag string)
)


;;; Funktionen für Zeit (Kapitel 25.4.1)

#+CLISP1 (defconstant internal-time-units-per-second 200)

; Hilfsfunktion für Macro TIME
(defun %time (new-real1 new-real2 new-run1 new-run2 new-gc1 new-gc2
              new-space1 new-space2 new-gccount
              old-real1 old-real2 old-run1 old-run2 old-gc1 old-gc2
              old-space1 old-space2 old-gccount)
  (macrolet ((merge-2-values (val1 val2)
               #+(or ATARI AMIGA DOS OS/2 VMS) `(dpb ,val1 (byte 16 16) ,val2)
               #+UNIX `(+ (* ,val1 internal-time-units-per-second) ,val2)
            ))
    (let ((Real-Time (- (merge-2-values new-real1 new-real2)
                        (merge-2-values old-real1 old-real2)
          )          )
          (Run-Time (- (merge-2-values new-run1 new-run2)
                       (merge-2-values old-run1 old-run2)
          )         )
          (GC-Time (- (merge-2-values new-gc1 new-gc2)
                      (merge-2-values old-gc1 old-gc2)
          )        )
          (Space (- (dpb new-space1 (byte 24 24) new-space2)
                    (dpb old-space1 (byte 24 24) old-space2)
          )      )
          (GC-Count (- new-gccount old-gccount))
         )
      (terpri)
      (write-string "Real time: ")
      (write (float (/ Real-Time internal-time-units-per-second)))
      (write-string " sec.")
      (terpri)
      (write-string "Run time: ")
      (write (float (/ Run-Time internal-time-units-per-second)))
      (write-string " sec.")
      (terpri)
      (write-string "Space: ") (write Space) (write-string " Bytes")
      (when (or (plusp GC-Count) (plusp GC-Time))
        (terpri)
        (write-string "GC: ") (write GC-Count)
        (write-string ", GC time: ")
        (write (float (/ GC-Time internal-time-units-per-second)))
        (write-string " sec.")
      )
) ) )

; (sleep seconds) macht seconds Sekunden Pause. CLTL S. 447
(defun sleep (time)
  (if (and (numberp time) (not (complexp time)) (not (minusp time)))
    (progn
      #+(or ATARI AMIGA DOS OS/2 VMS)
      (if (> time '#,(floor (expt 2 31) internal-time-units-per-second))
        ; Mehr als 248 bzw. 994 bzw. 497 Tage? (Denn sys::%sleep akzeptiert nur
        ; Argumente < 2^32, bei #+(or DOS OS/2 VMS) sogar nur Argumente < 2^31.)
        (loop ; ja -> Endlosschleife
          #+(or AMIGA OS/2 VMS)
          (sys::%sleep '#,(* 86400 internal-time-units-per-second))
        )
        (sys::%sleep (round (* time internal-time-units-per-second)))
      )
      #+UNIX
      (if (> time 16700000) ; mehr als 193 Tage?
        (loop (sys::%sleep 86400 0)) ; ja -> Endlosschleife
        (multiple-value-bind (seconds rest) (floor time)
          (sys::%sleep seconds (round (* rest internal-time-units-per-second)))
      ) )
    )
    (error #+DEUTSCH "~S: Argument muß eine Zahl >=0 sein, nicht ~S"
           #+ENGLISH "~S: argument ~S should be a nonnegative number"
           #+FRANCAIS "~S : L'argument doit être un nombre positif ou zéro et non ~S"
           'sleep time
) ) )


;; Funktionen für Zeit-Umrechnung und Zeitzonen (CLTL Kapitel 25.4.1)
;; Version 2, beinhaltet mehr Mathematik und basiert auf März-Jahren

; Ein März-Jahr sei die Periode vom 1.3. bis 28/29.2.
; Vorteil: Umrechnung Monat/Tag <--> Jahrtag wird einfacher.
; Skizze:
;   1.1.1900            1.1.1901            1.1.1902
;                                         
;   |-------------------|-------------------|-------------------|
;   |     Jahr 1900     |     Jahr 1901     |     Jahr 1902     |
;   |--|----------------|--|----------------|--|----------------|--|
;      |  März-Jahr 1900   |  März-Jahr 1901   |  März-Jahr 1902   |
;      |-------------------|-------------------|-------------------|
;                                            
;      1.3.1900            1.3.1901            1.3.1902

; (UTag Jahr) = Nummer des Tages 1.3.Jahr (gegenüber 1.1.1900)
; UTag(J) = 365*J + floor(J/4) - floor(J/100) + floor(J/400) - 693901
; damit  UTag(J) - UTag(J-1) = 365 + [1 falls J Schaltjahr]
; und    UTag(1899) = -306
; gelten.
(defun UTag (Jahr)
  (+ (* 365 Jahr) (floor Jahr 4) (- (floor Jahr 100)) (floor Jahr 400) -693901)
)

; Näherungwert:
; 365+1/4-1/100+1/400 = 365.2425 = 146097/400 .
; Durch Betrachtung einer Wertetabelle der 400-periodischen Funktion
; (J -> UTag(J)-146097/400*J) sieht man:
;   146097/400*J - 693902.4775 <= UTag(J) <= 146097/400*J - 693900.28

; Bestimmt zu einem Tag (0 = 1.1.1900) das März-Jahr und den Tag im März-Jahr.
; (Jahr&Tag UTTag) ==> Jahr, Jahrtag
; mit (= UTTag (+ (UTag Jahr) Jahrtag))
(defun Jahr&Tag (UTTag)
  ; Gesucht ist das größte Jahr mit UTag(Jahr) <= UTTag.
  ; Für dieses Jahr J gilt
  ; 146097/400*J - 693902.4775 <= UTag(J) <= UTTag < UTag(J+1) <= 146097/400*J - 693535.0375,
  ; also 146097*J - 277560991 <= 400*UTTag < 146097*J - 277414015,
  ; also 146097*(J-1900) + 23309 <= 400*UTTag < 146097*(J-1900) + 170285,
  ; also J + 0.159544... <= 1900 + UTTag/(146097/400) < J + 1.165561... .
  (let* ((Jahr (+ 1900 (floor (- UTTag 58) 146097/400)))
         (Jahresanfang (UTag Jahr)))
    ; Wegen 146097*(J-1900) + 109 <= 400*(UTTag-58) < 146097*(J-1900) + 147084,
    ; also J <= 1900 + (UTTag-58)/(146097/400) < J+1.006755...,
    ; ist die Schätzung  Jahr := floor(1900 + (UTTag-58)/(146097/400))
    ; meist richtig und jedenfalls nicht zu klein und um höchstens 1 zu groß.
    (when (< UTTag Jahresanfang) ; zu groß?
      (decf Jahr)
      (setq Jahresanfang (UTag Jahr))
    )
    (values Jahr (- UTTag Jahresanfang))
) )

; Die Grenzen der Sommerzeit:
; Es gibt folgende Typen der Sommerzeit-Berechnung:
;   0    DST_NONE: Daylight Savings Time not observed
;   1    DST_USA: United States DST
;   2    DST_AUST: Australian DST
;   3    DST_WET: Western European DST
;   4    DST_MET: Middle European DST
;   5    DST_EET: Eastern European DST
;   6    DST_CAN: Canadian DST
;   7    DST_GB: Great Britain and Eire DST
;   8    DST_RUM: Rumanian DST
;   9    DST_TUR: Turkish DST
;  10    DST_AUSTALT: Australian-style DST with shift in 1986

; Stellt fest, ob bei gegebenem März-Jahr und Tag und Stunde Sommerzeit gilt.
(defun NONE-Sommerzeit-p (Jahr Jahrtag Stunde)
  (declare (ignore Jahr Jahrtag Stunde))
  nil
)

; In Deutschland (in welchen Jahren ??)
; beginnt sie am letzten Märzsonntag, 2h MEZ nachts (inklusive)
; und endet am letzten Septembersonntag, 2h MEZ nachts (exklusive).
#|
; (MET-Sommerzeit-Grenzen Jahr) liefert:
;       Märzjahresstunde des letzten Sonntags im März, 2h
; und   Märzjahresstunde des letzten Sonntags im September, 2h, minus 1.
(defun MET-Sommerzeit-Grenzen (Jahr)
  (let ((Jahresanfang (UTag Jahr)))
    (flet ((letzter-Sonntag-vor (Tag) ; liefert den letzten Sonntag vorher (inklusive)
             (- Tag (mod (- Tag 6) 7))
          ))
      (cons
        (+ (* 24 (- (letzter-Sonntag-vor (+ Jahresanfang 30)) ; Sonntag vor 31. März
                    Jahresanfang
           )     )
           2
        )
        (1-
          (+ (* 24 (- (letzter-Sonntag-vor (+ Jahresanfang 213)) ; Sonntag vor 30. September
                      Jahresanfang
             )     )
             2
      ) ) )
) ) )
|#

; Stellt fest, ob bei gegebenem März-Jahr und Tag und Stunde (MEZ)
; in der Bundesrepublik Deutschland Sommerzeit gilt.
(defun MET-Sommerzeit-p (Jahr Jahrtag Stunde)
  (and (<= 1980 Jahr 2000)
    (let ((Jahresstunde (+ (* 24 Jahrtag) Stunde))
          (Grenzen
            (svref '#( ; Sommerzeit-Intervalle (vorausberechnet)
                       ;; War in den 30er/40er Jahren schon Sommerzeit??
                     ; (674 . 5041) ; 1970 : 29.3. 2h bis 27.9. 2h
                     ; (650 . 5017) ; 1971 : 28.3. 2h bis 26.9. 2h
                     ; (602 . 4969) ; 1972 : 26.3. 2h bis 24.9. 2h
                     ; (578 . 5113) ; 1973 : 25.3. 2h bis 30.9. 2h
                     ; (722 . 5089) ; 1974 : 31.3. 2h bis 29.9. 2h
                     ; (698 . 5065) ; 1975 : 30.3. 2h bis 28.9. 2h
                     ; (650 . 5017) ; 1976 : 28.3. 2h bis 26.9. 2h
                     ; (626 . 4993) ; 1977 : 27.3. 2h bis 25.9. 2h
                     ; (602 . 4969) ; 1978 : 26.3. 2h bis 24.9. 2h
                     ; (578 . 5113) ; 1979 : 25.3. 2h bis 30.9. 2h
                     ; In Deutscland wurde die Sommerzeit 1980 eingeführt.
                       (698 . 5065) ; 1980 : 30.3. 2h bis 28.9. 2h
                       (674 . 5041) ; 1981 : 29.3. 2h bis 27.9. 2h
                       (650 . 5017) ; 1982 : 28.3. 2h bis 26.9. 2h
                       (626 . 4993) ; 1983 : 27.3. 2h bis 25.9. 2h
                       (578 . 5113) ; 1984 : 25.3. 2h bis 30.9. 2h
                       (722 . 5089) ; 1985 : 31.3. 2h bis 29.9. 2h
                       (698 . 5065) ; 1986 : 30.3. 2h bis 28.9. 2h
                       (674 . 5041) ; 1987 : 29.3. 2h bis 27.9. 2h
                       (626 . 4993) ; 1988 : 27.3. 2h bis 25.9. 2h
                       (602 . 4969) ; 1989 : 26.3. 2h bis 24.9. 2h
                       (578 . 5113) ; 1990 : 25.3. 2h bis 30.9. 2h
                       (722 . 5089) ; 1991 : 31.3. 2h bis 29.9. 2h
                       (674 . 5041) ; 1992 : 29.3. 2h bis 27.9. 2h
                       (650 . 5017) ; 1993 : 28.3. 2h bis 26.9. 2h
                       (626 . 4993) ; 1994 : 27.3. 2h bis 25.9. 2h
                       (602 . 4969) ; 1995 : 26.3. 2h bis 24.9. 2h
                       (722 . 5089) ; 1996 : 31.3. 2h bis 29.9. 2h
                       (698 . 5065) ; 1997 : 30.3. 2h bis 28.9. 2h
                       (674 . 5041) ; 1998 : 29.3. 2h bis 27.9. 2h
                       (650 . 5017) ; 1999 : 28.3. 2h bis 26.9. 2h
                       (602 . 4969) ; 2000 : 26.3. 2h bis 24.9. 2h
                     )
                   (- Jahr 1980)
         )) )
      (<= (car Grenzen) Jahresstunde (cdr Grenzen))
) ) )

#-UNIX
(defvar *default-time-zone* -1) ; 1 h östlich GMT = MEZ
; NB: Zeitzone muß nicht ganzzahlig sein, sollte aber Vielfaches
; einer Viertelstunde sein.
#-UNIX
(defvar *default-dst-check* #'MET-Sommerzeit-p) ; MEZ-Sommerzeit

; andere Abbildung  Jahrtag -> Monat  für decode-universal-time:
; Seien Monat und Jahrtag auf den 1. März bezogen
; (d.h. Jahrtag = 0 am 1. März, = 364 am 28. Februar, usw.,
;  und März=0,...,Dezember=9,Januar=10,Februar=11).
; Dann ist
;                Monat = floor(a*Jahrtag+b)
; sofern a und b so gewählt sind, daß die Ungleichungen
;   122*a+b >= 4, 275*a+b >= 9, 30*a+b < 1, 336*a+b < 11
; gelten. Dies ist ein Viereck im Bereich
; 0.032653... = 8/245 <= a <= 7/214 = 0.032710...,
; 0.009345... = 1/107 <= b <= 1/49 = 0.020408...,
; in dem z.B. der Punkt (a=5/153,b=2/153) liegt:
;                Monat = floor((5*Jahrtag+2)/153).

; andere Abbildung  Monat -> Jahrtag
; für encode-universal-time und decode-universal-time:
; Seien Monat und Jahrtag auf den 1. März bezogen
; (d.h. Jahrtag = 0 am 1. März, = 364 am 28. Februar, usw.,
;  und März=0,...,Dezember=9,Januar=10,Februar=11).
; Die Abbildung
;      Monat   0  1  2  3  4   5   6   7   8   9   10  11
;      Jahrtag 0 31 61 92 122 153 184 214 245 275 306 337
; kann man schreiben
;                Jahrtag = floor(a*Monat+b)
; sofern a und b so gewählt sind, daß die Ungleichungen
;   a+b >= 31, 11*a+b >= 337, 4*a+b < 123, 9*a+b < 276
; gelten. Dies ist ein Viereck im Bereich
; 30.5714... = 214/7 <= a <= 245/8 = 30.625,
; 0.375      = 3/8   <= b <= 5/7   = 0.7142...,
; in dem z.B. der Punkt (a=153/5,b=2/5) liegt:
;                Jahrtag = floor((153*Monat+2)/5).
; Dies ist allerdings langsamer als ein Tabellenzugriff.

(macrolet ((Monat->Jahrtag (Monat) ; 0 <= Monat < 12, 0=März,...,11=Februar
             `(svref '#(0 31 61 92 122 153 184 214 245 275 306 337) ,Monat)
          ))

; (encode-universal-time second minute hour date month year [time-zone]),
; CLTL S. 446
(defun encode-universal-time
              (Sekunde Minute Stunde Tag Monat Jahr &optional (Zeitzone nil)
               &aux Monat3 Jahr3 Jahrtag UTTag)
  (unless (and (and (integerp Jahr)
                    (progn
                      (when (<= 0 Jahr 99)
                        (multiple-value-bind (i1 i2 i3 i4 i5 Jahrjetzt) (get-decoded-time)
                          (declare (ignore i1 i2 i3 i4 i5))
                          (setq Jahr
                            (+ Jahr (* 100 (ceiling (- Jahrjetzt Jahr 50) 100)))
                      ) ) )
                      (<= 1900 Jahr)
               )    )
               (and (integerp Monat) (<= 1 Monat 12))
               (progn
                 (if (< Monat 3)
                   (setq Jahr3 (1- Jahr)  Monat3 (+ Monat 9)) ; Monat3 10..11
                   (setq Jahr3 Jahr       Monat3 (- Monat 3)) ; Monat3 0..9
                 )
                 (and (and (integerp Tag) (<= 1 Tag))
                      (progn
                        (setq Jahrtag (+ (1- Tag) (Monat->Jahrtag Monat3)))
                        (setq UTTag (+ Jahrtag (UTag Jahr3)))
                        (and (if (not (eql Monat3 11))
                               (< Jahrtag (Monat->Jahrtag (1+ Monat3)))
                               (< UTTag (UTag (1+ Jahr3)))
                             )
                             (and (integerp Stunde) (<= 0 Stunde 23))
                             (and (integerp Minute) (<= 0 Minute 59))
                             (and (integerp Sekunde) (<= 0 Sekunde 59))
                             (and (progn
                                    (unless Zeitzone
                                      (setq Zeitzone
                                        #-UNIX (- *default-time-zone*
                                                  (if (funcall *default-dst-check* Jahr3 Jahrtag Stunde) 1 0)
                                               )
                                        #+UNIX (default-time-zone)
                                    ) )
                                    (when (floatp Zeitzone) (setq Zeitzone (rational Zeitzone)))
                                    (or (integerp Zeitzone)
                                        (and (rationalp Zeitzone) (integerp (* 4 Zeitzone)))
                                  ) )
                                  (<= -12 Zeitzone 12)
          )    ) )    ) )    )
    (error #+DEUTSCH "Inkorrektes Datum: ~S.~S.~S, ~Sh~Sm~Ss, Zeitzone ~S"
           #+ENGLISH "incorrect date: ~S.~S.~S, ~Sh~Sm~Ss, time zone ~S"
           #+FRANCAIS "Date incorrecte : ~S/~S/~S, ~Sh~Sm~Ss, heure ~S"
           Tag Monat Jahr Stunde Minute Sekunde Zeitzone
  ) )
  (+ Sekunde
     (* 60 (+ Minute
              (* 60 (+ Stunde Zeitzone
                       (* 24 UTTag)
  )  )     )  )     )
)

; (decode-universal-time universal-time [time-zone]), CLTL S. 445
(defun decode-universal-time (UT &optional (time-zone nil)
                              &aux Sommerzeit Zeitzone)
  (if time-zone
    (setq Sommerzeit nil Zeitzone time-zone)
    #-UNIX
    (setq time-zone *default-time-zone*
          Sommerzeit (let ((UT (- UT (round (* 3600 time-zone)))))
                       (multiple-value-bind (UTTag Stunde) (floor (floor UT 3600) 24)
                         (multiple-value-bind (Jahr Jahrtag) (Jahr&Tag UTTag)
                           (funcall *default-dst-check* Jahr Jahrtag Stunde)
                     ) ) )
          Zeitzone (- time-zone (if Sommerzeit 1 0))
    )
    #+UNIX
    (setq time-zone (default-time-zone) Sommerzeit nil Zeitzone time-zone)
  )
  ; time-zone = Zeitzone ohne Sommerzeitberücksichtigung,
  ; Zeitzone = Zeitzone mit Sommerzeitberücksichtigung.
  (let ((UTSekunden (- UT (round (* 3600 Zeitzone)))))
    (multiple-value-bind (UTMinuten Sekunde) (floor UTSekunden 60)
      (multiple-value-bind (UTStunden Minute) (floor UTMinuten 60)
        (multiple-value-bind (UTTage Stunde) (floor UTStunden 24)
          (multiple-value-bind (Jahr Jahrtag) (Jahr&Tag UTTage)
            (let* ((Monat (floor (+ (* 5 Jahrtag) 2) 153))
                   (Tag (1+ (- Jahrtag (Monat->Jahrtag Monat)))))
              (if (< Monat 10) ; Monat März..Dezember?
                (setq Monat (+ Monat 3)) ; Monat 3..12
                (setq Monat (- Monat 9) Jahr (+ Jahr 1)) ; Monat 1..2
              )
              (values Sekunde Minute Stunde Tag Monat Jahr (mod UTTage 7)
                      Sommerzeit time-zone
) ) ) ) ) ) ) )

) ; Ende von macrolet

; (get-decoded-time), CLTL S. 445
(defun get-decoded-time ()
  (decode-universal-time (get-universal-time))
)


;;; Verschiedenes

; (concat-pnames obj1 obj2) liefert zu zwei Objekten (Symbolen oder Strings)
;  ein Symbol, dessen Printname sich aus den beiden Objekten zusammensetzt.
(defun concat-pnames (obj1 obj2)
  (let ((str (string-concat (string obj1) (string obj2))))
    (if (and (plusp (length str)) (eql (char str 0) #\:))
      (intern (subseq str 1) *keyword-package*)
      (intern str)
) ) )

; FORMAT-Control-String zur Datumsausgabe,
; anwendbar auf eine Liste (sec min hour day month year ...),
; belegt 17-19 Zeichen
(defconstant *date-format*
  #+DEUTSCH "~1{~3@*~D.~4@*~D.~5@*~D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}"
  #+ENGLISH "~1{~5@*~D/~4@*~D/~3@*~D ~2@*~2,'0D.~1@*~2,'0D.~0@*~2,'0D~:}"
  #+FRANCAIS "~1{~3@*~D/~4@*~D/~5@*~D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}"
)

; zeigt ein Directory an.
(defun dir (&optional (pathnames #+(or ATARI DOS) '("*.*\\" "*.*")
                                 #+(or AMIGA UNIX OS/2) '("*/" "*")
                                 #+VMS '("[.*]" "*.*")
           )          )
  (flet ((onedir (pathname)
           (let ((pathname-list (directory pathname :full t)))
             (format t
               '#,(concatenate 'string
                    "~:[~:{~%~0@*~A~40T~3@*~7D~52T~2@*~21<" *date-format* "~>~}~;~{~%~A~}~]"
                  )
               (every #'atom pathname-list) pathname-list
        )) ) )
    (if (listp pathnames) (mapc #'onedir pathnames) (onedir pathnames))
  )
  (values)
)

