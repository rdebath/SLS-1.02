;; Ausgabe von Floating-Point-Zahlen mit PRINT und FORMAT
;; Michael Stoll 10.2.1990 - 26.3.1990
;; Bruno Haible 8.9.1990 - 10.9.1990

;; Grundgedanken:
;; Jede Real-Zahl /= 0 repräsentiert ein (offenes) Intervall. Es wird die-
;; jenige Dezimalzahl mit möglichst wenig Stellen ausgegeben, die in diesem
;; Intervall liegt.
;; Um auch große Exponenten zu behandeln, werden Zweier- in Zehnerpotenzen
;; erst einmal näherungsweise umgerechnet. Nötigenfalls wird die Rechen-
;; genauigkeit erhöht. Hierbei wird von den Long-Floats beliebiger
;; Genauigkeit Gebrauch gemacht.

(in-package "SYSTEM")

; Stützt sich auf:
; (sys::log2 digits) liefert ln(2) mit mindestens digits Mantissenbits.
; (sys::log10 digits) liefert ln(10) mit mindestens digits Mantissenbits.
; (sys::decimal-string integer) liefert zu einem Integer >0
;   einen Simple-String mit seiner Dezimaldarstellung.
; (sys::substring string start [end]) wie subseq, jedoch für Strings schneller.

;; Hauptfunktion zur Umwandlung von Floats ins Dezimalsystem:
;; Zu einem Float x werden ein Simple-String as und drei Integers k,e,s
;; berechnet mit folgenden Eigenschaften:
;; s = sign(x).
;; Falls x/=0, betrachte |x| statt x. Also oBdA x>0.
;;   Seien x1 und x2 die nächstkleinere bzw. die nächstgrößere Zahl zu x
;;   vom selben Floating-Point-Format. Die Zahl x repräsentiert somit das
;;   offene Intervall von (x+x1)/2 bis (x+x2)/2.
;;   a ist ein Integer >0, mit genau k Dezimalstellen (k>=1), und es gilt
;;   (x+x1)/2 < a*10^(-k+e) < (x+x2)/2 .
;;   Dabei ist k minimal, also a nicht durch 10 teilbar.
;; Falls x=0: a=0, k=1, e=0.
;; as ist die Ziffernfolge von a, der Länge k.
(defun decode-float-decimal (x)
  (declare (type float x))
  (multiple-value-bind (binmant binexpo sign) (integer-decode-float x)
    (if (eql binmant 0) ; x=0 ?
      (values "0" 1 0 0) ; a=0, k=1, e=0, s=0
      ; x/=0, also ist sign das Vorzeichen von x und
      ; |x| = 2^binexpo * float(binmant,x) . Ab jetzt oBdA x>0.
      ; Also x = 2^binexpo * float(binmant,x) .
      (let* ((l (integer-length binmant)) ; Anzahl der Bits von binmant
             (2*binmant (ash binmant 1)) ; 2*binmant
             (oben (1+ 2*binmant)) ; obere Intervallgrenze ist
                                   ; (x+x2)/2 = 2^(binexpo-1) * oben
             (unten (1- 2*binmant)) ; untere Intervallgrenze ist
             (untenshift 0)         ; (x+x1)/2 = 2^(binexpo-1-untenshift) * unten
            )
        (when (eql (integer-length unten) l)
          ; Normalerweise integerlength(unten) = 1+integerlength(binmant).
          ; Hier integerlength(unten) = l = integerlength(binmant),
          ; also war binmant eine Zweierpotenz. In diesem Fall ist die
          ; die Toleranz nach oben 1/2 Einheit, aber die Toleranz nach unten
          ; nur 1/4 Einheit: (x+x1)/2 = 2^(binexpo-2) * (4*binmant-1)
          (setq unten (1- (ash 2*binmant 1)) untenshift 1)
        )
        ; Bestimme d (ganz) und a1,a2 (ganz, >0) so, daß
        ; die ganzen a mit (x+x1)/2 < 10^d * a < (x+x2)/2 genau
        ; die ganzen a mit a1 <= a <= a2 sind und 0 <= a2-a1 < 20 gilt.
        ; Wandle dazu 2^e := 2^(binexpo-1) ins Dezimalsystem um.
        (let* ((e (- binexpo 1))
               (e-gross (> (abs e) (ash l 1))) ; Ist |e| recht groß, >2*l ?
               g f     ; Hilfsvariablen für den Fall, daß |e| groß ist
               zehn-d  ; Hilfsvariable 10^|d| für den Fall, daß |e| klein ist
               d a1 a2 ; Ergebnisvariablen
              )
          (if e-gross ; Ist |e| recht groß ?
            ; Da 2^e nur näherungsweise gehen kann, braucht man Schutzbits.
            (prog ((h 16)) ; Anzahl der Schutzbits, muß >= 3 sein
              neue-schutzbits
              ; Ziel: 2^e ~= 10^d * f/2^g, wobei 1 <= f/2^g < 10.
              (setq g (+ l h)) ; Anzahl der gültigen Bits von f
              ; Schätze d = floor(e*lg(2))
              ; mit Hilfe der Näherungsbrüche von lg(2):
              ; (0 1/3 3/10 28/93 59/196 146/485 643/2136 4004/13301
              ;  8651/28738 12655/42039 21306/70777 76573/254370 97879/325147
              ;  1838395/6107016 1936274/6432163 13456039/44699994
              ;  15392313/51132157 44240665/146964308 59632978/198096465
              ;  103873643/345060773 475127550/1578339557 579001193/1923400330
              ; )
              ; e>=0 : wähle lg(2) < a/b < lg(2) + 1/e,
              ;        dann ist d <= floor(e*a/b) <= d+1 .
              ; e<0  : wähle lg(2) - 1/abs(e) < a/b < lg(2),
              ;        dann ist d <= floor(e*a/b) <= d+1 .
              ; Es ist bekannt, daß abs(e) <= 2^31 + 2^20 .
              ; Unser d sei := floor(e*a/b)-1.
              (setq d (1- (if (minusp e)
                            (if (>= e -970)
                              (floor (* e 3) 10) ; Näherungsbruch 3/10
                              (floor (* e 21306) 70777) ; Näherungsbruch 21306/70777
                            )
                            (if (<= e 22000)
                              (floor (* e 28) 93) ; Näherungsbruch 28/93
                              (floor (* e 12655) 42039) ; Näherungsbruch 12655/42039
              )       )   ) )
              ; Das wahre d wird durch diese Schätzung entweder getroffen
              ; oder um 1 unterschätzt.
              ; Anders ausgedrückt: 0 < e*log(2)-d*log(10) < 2*log(10).
              ; Nun f/2^g als exp(e*log(2)-d*log(10)) berechnen.
              ; Da f < 100*2^g < 2^(g+7), sind g+7 Bits relative Genauigkeit
              ; des Ergebnisses, also g+7 Bits absolute Genauigkeit von
              ; e*log(2)-d*log(10) nötig. Dazu mit l'=integerlength(e)
              ; für log(2): g+7+l' Bits abs. Gen., g+7+l' Bits rel. Gen.,
              ; für log(10): g+7+l' Bits abs. Gen., g+7+l'+2 Bist rel. Gen.
              (let ((f/2^g (let ((gen (+ g (integer-length e) 9))) ; Genauigkeit
                             (exp (- (* e (sys::log2 gen)) (* d (sys::log10 gen))))
                   ))      )
                ; Das so berechnete f/2^g ist >1, <100.
                ; Mit 2^g multiplizieren und auf eine ganze Zahl runden:
                (setq f (round (scale-float f/2^g g))) ; liefert f
              )
              ; Eventuell f und d korrigieren:
              (when (>= f (ash 10 g)) ; f >= 10*2^g ?
                (setq f (floor f 10) d (+ d 1))
              )
              ; Nun ist 2^e ~= 10^d * f/2^g, wobei 1 <= f/2^g < 10 und
              ; f ein Integer ist, der um höchstens 1 vom wahren Wert abweicht:
              ; 10^d * (f-1)/2^g < 2^e < 10^d * (f+1)/2^g
              ; Wir verkleinern nun das offene Intervall
              ; von (x+x1)/2 = 2^(binexpo-1-untenshift) * unten
              ; bis (x+x2)/2 = 2^(binexpo-1) * oben
              ; zu einem abgeschlossenen Intervall
              ; von 10^d * (f+1)/2^(g+untenshift) * unten
              ; bis 10^d * (f-1)/2^g * oben
              ; und suchen darin Zahlen der Form 10^d * a mit ganzem a.
              ; Wegen  oben - unten/2^untenshift >= 3/2
              ; und  oben + unten/2^untenshift <= 4*binmant+1 < 2^(l+2) <= 2^(g-1)
              ; ist die Intervall-Länge
              ; = 10^d * ((f-1)*oben - (f+1)*unten/2^untenshift) / 2^g
              ; = 10^d * ( f * (oben - unten/2^untenshift)
              ;            - (oben + unten/2^untenshift) ) / 2^g
              ; >= 10^d * (2^g * 3/2 - 2^(g-1)) / 2^g
              ; = 10^d * (3/2 - 2^(-1)) = 10^d
              ; und daher gibt es in dem Intervall mindestens eine Zahl
              ; dieser Form.
              ; Die Zahlen der Form 10^d * a in diesem Intervall sind die
              ; mit a1 <= a <= a2, wobei a2 = floor((f-1)*oben/2^g) und
              ; a1 = ceiling((f+1)*unten/2^(g+untenshift))
              ;    = floor(((f+1)*unten-1)/2^(g+untenshift))+1 .
              ; Wir haben eben gesehen, daß a1 <= a2 sein muß.
              (setq a1 (1+ (ash (1- (* (+ f 1) unten)) (- (+ g untenshift)))))
              (setq a2 (ash (* (- f 1) oben) (- g)))
              ; Wir können auch das offene Intervall
              ; von (x+x1)/2 = 2^(binexpo-1-untenshift) * unten
              ; bis (x+x2)/2 = 2^(binexpo-1) * oben
              ; in das (abgeschlossene) Intervall
              ; von 10^d * (f-1)/2^(g+untenshift) * unten
              ; bis 10^d * (f+1)/2^g * oben
              ; einschachteln. Hierin sind die Zahlen der Form 10^d * a
              ; die mit a1' <= a <= a2', wobei a1' <= a1 <= a2 <= a2' ist
              ; und sich a1' und a2' analog zu a1 und a2 berechnen.
              ; Da (f-1)*oben/2^g und (f+1)*oben/2^g sich um 2*oben/2^g
              ; < 2^(l+2-g) < 1 unterscheiden, unterscheiden sich a2 und
              ; a2' um höchstens 1.
              ; Ebenso, wenn 'oben' durch 'unten/2^untenshift' ersetzt
              ; wird: a1' und a1 unterscheiden sich um höchstens 1.
              ; Ist nun a1' < a1 oder a2 < a2' , so ist die Zweierpotenz-
              ; Näherung 10^d * f/2^g für 2^e nicht genau genug gewesen,
              ; und man hat das Ganze mit erhöhtem h zu wiederholen.
              ; Ausnahme (da hilft auch keine höhere Genauigkeit):
              ;   Wenn die obere oder untere Intervallgrenze (x+x2)/2 bzw.
              ;   (x+x1)/2 selbst die Gestalt 10^d * a mit ganzem a hat.
              ;   Dies testet man so:
              ;     (x+x2)/2 = 2^e * oben == 10^d * a  mit ganzem a, wenn
              ;     - für e>=0, (dann 0 <= d <= e): 5^d | oben,
              ;     - für e<0, (dann e <= d < 0): 2^(d-e) | oben, was
              ;                nur für d-e=0 der Fall ist.
              ;     (x+x1)/2 = 2^(e-untenshift) * unten == 10^d * a
              ;     mit ganzem a, wenn
              ;     - für e>0, (dann 0 <= d < e): 5^d | unten,
              ;     - für e<=0, (dann e <= d <= 0): 2^(d-e+untenshift) | unten,
              ;                 was nur für d-e+untenshift=0 der Fall ist.
              ; Da wir es jedoch mit großem |e| zu tun haben, kann dieser
              ; Ausnahmefall hier gar nicht eintreten!
              ; Denn im Falle e>=0: Aus e>=2*l und l>=11 folgt
              ;   e >= (l+2)*ln(10)/ln(5) + ln(10)/ln(2),
              ;   d >= e*ln(2)/ln(10)-1 >= (l+2)*ln(2)/ln(5),
              ;   5^d >= 2^(l+2),
              ;   und wegen 0 < unten < 2^(l+2) und 0 < oben < 2^(l+1)
              ;   sind unten und oben nicht durch 5^d teilbar.
              ; Und im Falle e<=0: Aus -e>=2*l und l>=6 folgt
              ;   -e >= (l+2)*ln(10)/ln(5),
              ;   d-e >= e*ln(2)/ln(10)-1-e = (1-ln(2)/ln(10))*(-e)-1
              ;          = (-e)*ln(5)/ln(10)-1 >= l+1,
              ;   2^(d-e) >= 2^(l+1),
              ;   und wegen 0 < unten < 2^(l+1+untenshift) ist unten nicht
              ;   durch 2^(d-e+untenshift) teilbar, und wegen
              ;   0 < oben < 2^(l+1) ist oben nicht durch 2^(d-e) teilbar.
              (when (or (< (1+ (ash (1- (* (- f 1) unten)) (- (+ g untenshift)))) ; a1'
                           a1
                        )
                        (< a2 (ash (* (- f 1) oben) (- g))) ; a2<a2'
                    )
                (setq h (ash h 1)) ; h verdoppeln
                (go neue-schutzbits) ; und alles wiederholen
              )
              ; Jetzt ist a1 der kleinste und a2 der größte Wert, der
              ; für a möglich ist.
              ; Wegen  oben - unten/2^untenshift <= 2
              ; ist die obige Intervall-Länge
              ; = 10^d * ((f-1)*oben - (f+1)*unten/2^untenshift) / 2^g
              ; < 10^d * ((f-1)*oben - (f-1)*unten/2^untenshift) / 2^g
              ; = 10^d * (f-1)/2^g * (oben - unten/2^untenshift)
              ; < 10^d * 10 * 2,
              ; also gibt es höchstens 20 mögliche Werte für a.
            )
            ; |e| ist recht klein -> man kann 2^e und 10^d exakt ausrechnen
            (if (not (minusp e))
              ; e >= 0. Schätze d = floor(e*lg(2)) wie oben.
              ; Es ist e<=2*l<2^21.
              (progn
                (setq d (if (<= e 22000)
                          (floor (* e 28) 93) ; Näherungsbruch 28/93
                          (floor (* e 4004) 13301) ; Näherungsbruch 4004/13301
                )       )
                ; Das wahre d wird durch diese Schätzung entweder getroffen
                ; oder um 1 überschätzt, aber das können wir leicht feststellen.
                (setq zehn-d (expt 10 d)) ; zehn-d = 10^d
                (when (< (ash 1 e) zehn-d) ; falls 2^e < 10^d,
                  (setq d (- d 1) zehn-d (floor zehn-d 10)) ; Schätzung korrigieren
                )
                ; Nun ist 10^d <= 2^e < 10^(d+1) und zehn-d = 10^d.
                ; a1 sei das kleinste ganze a > 2^(e-untenshift) * unten / 10^d,
                ; a2 sei das größte ganze a < 2^e * oben / 10^d.
                ; a1 = 1+floor(unten*2^e/(2^untenshift*10^d)),
                ; a2 = floor((oben*2^e-1)/10^d).
                (setq a1 (1+ (floor (ash unten e) (ash zehn-d untenshift))))
                (setq a2 (floor (1- (ash oben e)) zehn-d))
              )
              ; e < 0. Schätze d = floor(e*lg(2)) wie oben.
              ; Es ist |e|<=2*l<2^21.
              (progn
                (setq d (if (>= e -970)
                          (floor (* e 3) 10) ; Näherungsbruch 3/10
                          (floor (* e 643) 2136) ; Näherungsbruch 643/2136
                )       )
                ; Das wahre d wird durch diese Schätzung entweder getroffen
                ; oder um 1 überschätzt, aber das können wir leicht feststellen.
                (setq zehn-d (expt 10 (- d))) ; zehn-d = 10^(-d)
                (when (<= (integer-length zehn-d) (- e)) ; falls 2^e < 10^d,
                  (setq d (- d 1) zehn-d (* zehn-d 10)) ; Schätzung korrigieren
                )
                ; Nun ist 10^d <= 2^e < 10^(d+1) und zehn-d = 10^(-d).
                ; a1 sei das kleinste ganze a > 2^(e-untenshift) * unten / 10^d,
                ; a2 sei das größte ganze a < 2^e * oben / 10^d.
                ; a1 = 1+floor(unten*10^(-d)/2^(-e+untenshift)),
                ; a2 = floor((oben*10^(-d)-1)/2^(-e))
                (setq a1 (1+ (ash (* unten zehn-d) (- e untenshift))))
                (setq a2 (ash (1- (* oben zehn-d)) e))
              )
          ) )
          ; Nun sind die ganzen a mit (x+x1)/2 < 10^d * a < (x+x2)/2 genau
          ; die ganzen a mit a1 <= a <= a2. Deren gibt es höchstens 20.
          ; Diese werden in drei Schritten auf einen einzigen reduziert:
          ; 1. Enthält der Bereich eine durch 10 teilbare Zahl a ?
          ;    ja -> setze a1:=ceiling(a1/10), a2:=floor(a2/10), d:=d+1.
          ; Danach enthält der Bereich a1 <= a <= a2 höchstens 10
          ; mögliche Werte für a.
          ; 2. Falls jetzt einer der möglichen Werte durch 10 teilbar ist
          ;    (es kann nur noch einen solchen geben),
          ;    wird er gewählt, die anderen vergessen.
          ; 3. Sonst wird unter allen noch möglichen Werten der zu x
          ;    nächstgelegene gewählt.
          (prog ((d-shift nil) ; Flag, ob im 1. Schritt d incrementiert wurde
                 a             ; das ausgewählte a
                )
            ; 1.
            (let ((b1 (ceiling a1 10))
                  (b2 (floor a2 10)))
              (if (<= b1 b2) ; noch eine durch 10 teilbare Zahl a ?
                (setq a1 b1 a2 b2 d (+ d 1) d-shift t)
                (go keine-10-mehr)
            ) )
            ; 2.
            (when (>= (* 10 (setq a (floor a2 10))) a1)
              ; Noch eine durch 10 teilbare Zahl -> durch 10 teilen.
              (setq d (+ d 1)) ; noch d erhöhen, zehn-d wird nicht mehr gebraucht
              ; Nun a in einen Dezimalstring umwandeln
              ; und dann Nullen am Schluß streichen:
              (let* ((as (sys::decimal-string a)) ; Ziffernfolge zu a>0
                     (las (length as)) ; Länge der Ziffernfolge
                     (k las) ; Länge ohne die gestrichenen Nullen am Schluß
                     (ee (+ k d))) ; a * 10^d = a * 10^(-k+ee)
                (loop
                  (let ((k-1 (- k 1)))
                    (unless (eql (schar as k-1) #\0) (return)) ; eine 0 am Schluß?
                    ; ja -> a := a / 10 (wird aber nicht mehr gebraucht),
                    ; d := d+1 (wird aber nicht mehr gebraucht),
                    (setq k k-1) ; k := k-1.
                ) )
                (when (< k las) (setq as (sys::substring as 0 k))) ; Teilstring nehmen
                (return (values as k ee sign))
            ) )
            ; 3.
            keine-10-mehr
            (setq a
              (if (eql a1 a2)
                ; a1=a2 -> keine Frage der Auswahl mehr:
                a1
                ; a1<a2 -> zu x nächstgelegenes 10^d * a wählen:
                (if e-gross
                  ; a = round(f*2*binmant/2^g/(1oder10)) (beliebige Rundung)
                  ;   = ceiling(floor(f*2*binmant/(1oder10)/2^(g-1))/2) wählen:
                  (ash (1+ (ash
                             (let ((z (* f 2*binmant)))
                               (if d-shift (floor z 10) z)
                             )
                             (- 1 g)
                       )   )
                       -1
                  )
                  ; |e| klein -> analog wie oben a2 berechnet wurde
                  (if (not (minusp e))
                    ; e>=0: a = round(2^e*2*binmant/10^d)
                    (round (ash 2*binmant e)
                           (if d-shift (* 10 zehn-d) zehn-d) ; 10^d je nach d-shift
                    )
                    ; e<0, also war d<0, jetzt (wegen Schritt 1) d<=0.
                    ; a = round(2*binmant*10^(-d)/2^(-e))
                    (ash (1+ (ash
                               (* 2*binmant
                                  (if d-shift (floor zehn-d 10) zehn-d) ; 10^(-d) je nach d-shift
                               )
                               (+ e 1)
                         )   )
                         -1
                    )
            ) ) ) )
            (let* ((as (sys::decimal-string a)) ; Ziffernfolge von a
                   (k (length as)))
              (return (values as k (+ k d) sign))
) ) ) ) ) ) )

; Ausgabefunktion für PRINT/WRITE von Floats:
(defun write-float (stream arg #| &optional (plus-sign-flag nil) |# )
  (unless (floatp arg)
    (error #+DEUTSCH "Argument ist kein Float: ~S"
           #+ENGLISH "argument is not a float: ~S"
           #+FRANCAIS "L'argument n'est pas un FLOAT : ~S"
           arg
  ) )
  (multiple-value-bind (mantstring mantlen expo sign)
      (decode-float-decimal arg)
    ; arg in Dezimaldarstellung: +/- 0.mant * 10^expo, wobei
    ;  mant die Mantisse: als Simple-String mantstring mit Länge mantlen,
    ;  expo der Dezimal-Exponent,
    ;  sign das Vorzeichen (-1 oder 0 oder 1).
    (if (eql sign -1) ; arg < 0 ?
      (write-char #\- stream)
      #| (if plus-sign-flag (write-char #\+ stream)) |#
    )
    (let ((flag (<= -2 expo 7))) ; arg=0 oder 10^-3 <= (abs arg) < 10^7 ?
      ; Was ist auszugeben? Fallunterscheidung:
      ; flag gesetzt -> "fixed-point notation":
      ;   expo <= 0 -> Null, Punkt, -expo Nullen, alle Ziffern
      ;   0 < expo < mantlen ->
      ;     die ersten expo Ziffern, Punkt, die restlichen Ziffern
      ;   expo >= mantlen -> alle Ziffern, expo-mantlen Nullen, Punkt, Null
      ;   Nach Möglichkeit kein Exponent; wenn nötig, Exponent 0.
      ; flag gelöscht -> "scientific notation":
      ;   erste Ziffer, Punkt, die restlichen Ziffern, bei mantlen=1 eine Null
      ;   Exponent.
      (if (and flag (not (plusp expo)))
        ; "fixed-point notation" mit expo <= 0
        ; erst Null und Punkt, dann -expo Nullen, dann alle Ziffern
        (progn
          (write-char #\0 stream)
          (write-char #\. stream)
          (do ((i expo (+ i 1)))
              ((eql i 0))
            (write-char #\0 stream)
          )
          (write-string mantstring stream)
          (setq expo 0) ; auszugebender Exponent ist 0
        )
        ; "fixed-point notation" mit expo > 0 oder "scientific notation"
        (let ((scale (if flag expo 1)))
          ; Der Dezimalpunkt wird um scale Stellen nach rechts geschoben,
          ; d.h. es gibt scale Vorkommastellen. scale > 0.
          (if (< scale mantlen)
            ; erst scale Ziffern, dann Punkt, dann restliche Ziffern:
            (progn
              (write-string mantstring stream :end scale)
              (write-char #\. stream)
              (write-string mantstring stream :start scale)
            )
            ; scale>=mantlen -> es bleibt nichts für die Nachkommastellen.
            ; alle Ziffern, dann scale-mantlen Nullen, dann Punkt und Null
            (progn
              (write-string mantstring stream)
              (do ((i mantlen (+ i 1)))
                  ((eql i scale))
                (write-char #\0 stream)
              )
              (write-char #\. stream)
              (write-char #\0 stream)
          ) )
          (decf expo scale) ; der auszugebende Exponent ist um scale kleiner.
      ) )
      ; Nun geht's zum Exponenten:
      (let ((e-marker
              (cond ((case *READ-DEFAULT-FLOAT-FORMAT*
                       (SHORT-FLOAT (short-float-p arg))
                       (SINGLE-FLOAT (single-float-p arg))
                       (DOUBLE-FLOAT (double-float-p arg))
                       (LONG-FLOAT (long-float-p arg))
                     ) #\E )
                    ((short-float-p arg) #\s)
                    ((single-float-p arg) #\f)
                    ((double-float-p arg) #\d)
                    ((long-float-p arg) #\L)
           )) )
        (unless (and flag (eql e-marker #\E)) ; evtl. Exponent ganz weglassen
          (write-char e-marker stream)
          (write expo :base 10 :radix nil :stream stream)
) ) ) ) )

