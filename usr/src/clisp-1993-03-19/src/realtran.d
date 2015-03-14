# Transzendente Funktionen für reelle Zahlen

# pi_F_float_F(f) liefert die Zahl pi im selben Float-Format wie f.
# kann GC auslösen
  local object pi_F_float_F (object f);
  local object pi_F_float_F(f)
    var reg5 object f;
    { floatcase(f,
                { return O(SF_pi); },
                { return O(FF_pi); },
                { return O(DF_pi); },
                ;
               );
     {var reg6 object pi = O(LF_pi);
      var reg4 uintC f_len = TheLfloat(f)->len; # gewünschte Länge von Pi
      {var reg1 uintC len = TheLfloat(pi)->len; # vorhandene Länge
       if (f_len < len) { return LF_shorten_LF(pi,f_len); }
       if (f_len == len) { return pi; }
      }
      # gewünschte > vorhandene Länge -> muß nachberechnen:
      # Methode:
      # [Richard P. Brent: Fast multiple-precision evaluation of elementary
      #  functions. J. ACM 23(1976), 242-251.]
      # d=f_len, n:=16*d. Verwende Long-Floats mit 16*(d+1) Mantissenbits.
      # (let* ((a (coerce 1 'long-float)) ; 1
      #        (b (sqrt (scale-float a -1))) ; 2^-(1/2)
      #        (eps (scale-float a (- n))) ; 2^-n
      #        (t (scale-float a -2)) ; 1/4
      #        (x 0)
      #       )
      #   (loop
      #     (when (< (- a b) eps) (return))
      #     (let ((y a))
      #       (setq a (scale-float (+ a b) -1))
      #       (setq b (sqrt (* b y)))
      #       (setq t (- t (scale-float (expt (- a y) 2) x)))
      #     )
      #     (incf x)
      #   )
      #   (/ (expt a 2) t)
      # )
      {var reg3 uintC len = f_len + 1; # Arbeite mit Long-Floats mit len Digits
       if (uintCoverflow(len)) { fehler_LF_toolong(); }
       {var reg2 uintL uexp_limit = LF_exp_mid - intDsize*(uintL)f_len; # LF_exp_mid - n
        # Ein Long-Float ist genau dann betragsmäßig <2^-n, wenn
        # sein Exponent < LF_exp_mid-n = uexp_limit ist.
        {var reg1 object temp = I_to_LF(Fixnum_1,len); # 1 als Long-Float
         pushSTACK(temp); # =: a
         temp = LF_I_scale_float_LF(temp,Fixnum_minus1); # (scale-float a -1)
         pushSTACK(LF_sqrt_LF(temp)); # daraus die Wurzel, =: b
         pushSTACK(Fixnum_0); # x:=0
         temp = LF_I_scale_float_LF(STACK_2,sfixnum(-2)); # (scale-float a -2)
         pushSTACK(temp); # =: t
        }# Stackaufbau: a, b, x, t.
        loop
          {{var reg1 object temp;
            temp = LF_LF_minus_LF(STACK_3,STACK_2); # (- a b)
            if (TheLfloat(temp)->expo < uexp_limit) # Exponent < uexp_limit
              break; } # ja -> |a-b| < 2^-n -> fertig
           {var reg1 object temp;
            temp = LF_LF_plus_LF(STACK_3,STACK_2); # a+b
            temp = LF_I_scale_float_LF(temp,Fixnum_minus1); # (a+b)/2
            pushSTACK(temp); } # neues a
            STACK_(2+1) = LF_sqrt_LF(LF_LF_mal_LF(STACK_(3+1),STACK_(2+1))); # b := sqrt(a*b)
           {var reg1 object temp;
            temp = STACK_(3+1); # altes a
            temp = LF_LF_minus_LF(STACK_(3+1) = STACK_0, temp); # neues a - altes a
            temp = LF_LF_mal_LF(temp,temp); # quadieren
            temp = LF_I_scale_float_LF(temp,STACK_(1+1)); # mal 2^x
            skipSTACK(1);
            STACK_0 = LF_LF_minus_LF(STACK_0,temp); # von t subtrahieren
            STACK_1 = fixnum_inc(STACK_1,1); # x:=x+1
          }}
        {var reg1 object temp;
         temp = STACK_3; temp = LF_LF_mal_LF(temp,temp); # a quadieren
         temp = LF_LF_durch_LF(temp,STACK_0); # durch t dividieren
         skipSTACK(4);
         # temp = Pi ist fertig.
         return O(LF_pi) = LF_shorten_LF(temp,f_len); # wieder verkürzen, als LF_pi abspeichern
    }}}}}

# pi() liefert die Zahl pi im Default-Float-Format.
# kann GC auslösen
  local object pi (void);
  local object pi()
    { defaultfloatcase(S(default_float_format),
                       return O(SF_pi); , # pi als SF
                       return O(FF_pi); , # pi als FF
                       return O(DF_pi); , # pi als DF
                       return O(pi); , # pi als LF der Defaultlänge
                       ,); # nichts zu retten
    }

# F_atanhx_F(x) liefert zu einem Float x (betragsmäßig <1/2) atanh(x) als Float.
# kann GC auslösen
  local object F_atanhx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<=-d/2 liefere x
#   (denn bei e<=-d/2 ist x^2 < 2^(-d), also
#   1 <= atanh(x)/x = 1+x^2/3+x^4/5+... < 1+x^2/2 < 1+2^(-d-1) < 1+2^(-d),
#   also ist atanh(x)/x, auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   atanh(x)/x = sum(j=0..inf,(x^2)^j/(2j+1)):
#   a:=x^2, b:=1, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum (/ b i)))) do i:=i+2, b:=b*a.
#   Ergebnis x*sum.
# Sonst setze y := x/(1+sqrt(1-x^2)), berechne rekursiv z:=atanh(y)
#   und liefere 2*z = (scale-float z 1).
# Diese Rekursion wird entrekursiviert. Statt k mal hintereinander
#   x := x/(1+sqrt(1-x^2)) zu bilden, arbeitet man lieber mit den Kehrwerten,
#   setzt also x := 1/|x|, dann k mal x := x+sqrt(x^2-1), dann x := +- 1/x.
# Aufwand: asymptotisch d^2.5 .

# F_atanx_F(x) liefert zu einem Float x (betragsmäßig <=1) atan(x) als Float.
# kann GC auslösen
  local object F_atanx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<=-d/2 liefere x
#   (denn bei e<=-d/2 ist x^2/3 < x^2/2 < 2^(-d)/2 = 2^(-d-1), also
#   1 >= atan(x)/x > 1-x^2/3 > 1-2^(-d-1),
#   also ist atan(x)/x, auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   atan(x)/x = sum(j=0..inf,(-x^2)^j/(2j+1)):
#   a:=-x^2, b:=1, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum (/ b i)))) do i:=i+2, b:=b*a.
#   Ergebnis x*sum.
# Sonst setze y := x/(1+sqrt(1+x^2)), berechne rekursiv z:=atan(y)
#   und liefere 2*z = (scale-float z 1).
# Diese Rekursion wird entrekursiviert. Statt k mal hintereinander
#   x := x/(1+sqrt(1+x^2)) zu bilden, arbeitet man lieber mit den Kehrwerten,
#   setzt also x := 1/|x|, dann k mal x := x+sqrt(x^2+1), dann x := +- 1/x.
# Aufwand: asymptotisch d^2.5 .

# Generiert eine Funktion wie F_atanx_F
  #define GEN_F_atanx(name,Fixnum_plusminus1,F_plusminus_F)                            \
    local object CONCAT3(F_,name,_F) (x)                                               \
      var reg3 object x;                                                               \
      { if (R_zerop(x)) { return x; }                                                  \
       {var reg8 uintL d = F_float_digits(x);                                          \
        var reg7 sintL e = F_exponent_L(x);                                            \
        if (e <= (sintL)(-d)>>1) # e <= -d/2 <==> e <= -ceiling(d/2)                   \
          { return x; } # ja -> x als Ergebnis                                         \
        pushSTACK(x);                                                                  \
        # Stackaufbau: x.                                                              \
        {var reg4 object k = Fixnum_0; # Rekursionszähler k:=0                         \
         var reg6 uintL sqrt_d = UL_sqrt_UW(d); # floor(sqrt(d))                       \
         # Bei e <= -1-floor(sqrt(d)) kann die Potenzreihe angewandt werden.           \
         if (e >= (sintL)(-sqrt_d))                                                             \
           # e > -1-floor(sqrt(d)) -> muß |x| verkleinern.                             \
           { var reg5 sintL e_limit = 1+sqrt_d; # 1+floor(sqrt(d))                     \
             pushSTACK(x = F_durch_F(F_abs_F(x))); # 1/|x|                             \
             # Stackaufbau: originales x, neues x.                                     \
             loop                                                                      \
               { # nächstes x nach der Formel x := x+sqrt(x^2 +- 1) berechnen:         \
                 x = F_sqrt_F(R_R_plus_R(F_F_mal_F(x,x),Fixnum_plusminus1));           \
                 STACK_0 = x = F_F_plus_F(STACK_0,x);                                  \
                 k = fixnum_inc(k,1); # k:=k+1                                         \
                 if (F_exponent_L(x) > e_limit) break;                                 \
               }                                                                       \
             # Schleifenende mit Exponent(x) > 1+floor(sqrt(d)), also                  \
             # x >= 2^(1+floor(sqrt(d))), also 1/x <= 2^(-1-floor(sqrt(d))).           \
             # Nun kann die Potenzreihe auf 1/x angewandt werden.                      \
            {var reg1 object x = F_durch_F(popSTACK());                                \
             if (R_mminusp(STACK_0)) { x = F_minus_F(x); } # Vorzeichen wieder rein    \
             STACK_0 = x; # neues x ersetzt altes x                                    \
           }}                                                                          \
         # Stackaufbau: neues x.                                                       \
         # Potenzreihe anwenden:                                                       \
         {var reg2 object i = Fixnum_1;                                                \
          pushSTACK(F_plusminus_F(F_F_mal_F(STACK_0,STACK_0))); # a := -x^2 bzw. x^2   \
          pushSTACK(I_F_float_F(Fixnum_1,STACK_1)); # b := (float 1 x)                 \
          pushSTACK(I_F_float_F(Fixnum_0,STACK_2)); # sum := (float 0 x)               \
          # Stackaufbau: x, a, b, sum.                                                 \
          loop                                                                         \
            { var reg1 object temp;                                                    \
              temp = R_R_durch_R(STACK_1,i); # (/ b i)                                 \
              temp = F_F_plus_F(STACK_0,temp); # (+ sum (/ b i))                       \
              if (eql(STACK_0,temp)) # = sum ?                                         \
                break; # ja -> Potenzreihe abbrechen                                   \
              STACK_0 = temp;                                                          \
              STACK_1 = F_F_mal_F(STACK_1,STACK_2); # b := b*a                         \
              i = fixnum_inc(i,2); # i := i+2                                          \
         }  }                                                                          \
         {var reg1 object erg = F_F_mal_F(STACK_0,STACK_3); # sum*x als Ergebnis       \
          skipSTACK(4);                                                                \
          return F_I_scale_float_F(erg,k); # wegen Rekursion noch mal 2^k              \
      }}}}
# F_atanx_F : mit x -> x+sqrt(x^2-1), a = -x^2
  GEN_F_atanx(atanx,Fixnum_1,F_minus_F)
# F_atanhx_F : mit x -> x+sqrt(x^2+1), a = x^2
  GEN_F_atanx(atanhx,Fixnum_minus1,)

# R_R_atan_R(x,y) liefert zu zwei reellen Zahlen x, y den Winkel von (x,y)
# in Polarkoordinaten. Ergebnis rational nur, wenn x>0 und y=0.
# kann GC auslösen
  local object R_R_atan_R (object x, object y);
# Methode:
# y=0 -> bei x>0: 0 als Ergebnis,
#        bei x<0: pi als Ergebnis.
#        bei x=0: Error.
# x=0 -> bei y>0: pi/2 als Ergebnis.
#        bei y<0: -pi/2 als Ergebnis.
#        bei y=0: Error.
# Falls x und y beide rational: beide in Floats umwandeln.
# 0 <= |y| <= x  ->  atan(y/x)
# 0 <= |x| <= y  ->  pi/2 - atan(x/y)
# 0 <= |x| <= -y  ->  -pi/2 - atan(x/y)
# 0 <= |y| <= -x  ->  für y>=0: pi + atan(y/x), für y<0: -pi + atan(y/x)
  local object R_R_atan_R(x,y)
    var reg2 object x;
    var reg3 object y;
    { if (eq(y,Fixnum_0))
        # y=0 (exakt)
        { if (R_zerop(x)) { divide_0(); } # x=0 -> Error
          if (R_minusp(x)) { return pi(); } # x<0 -> pi in Default-Float-Genauigkeit
          else { return Fixnum_0; } # x>0 -> 0
        }
      elif (eq(x,Fixnum_0))
        # x=0 (exakt)
        { if (R_zerop(y)) { divide_0(); } # y=0 -> Error
          if (R_minusp(x)) # x<0 -> -pi/2
            { return F_minus_F(F_I_scale_float_F(pi(),Fixnum_minus1)); }
          else # x>0 -> pi/2
            { return F_I_scale_float_F(pi(),Fixnum_minus1); }
        }
      pushSTACK(x); pushSTACK(y);
      # Stackaufbau: x, y.
      if (R_rationalp(x) && R_rationalp(y))
        # x,y in Floats umwandeln:
        { STACK_1 = RA_float_F(x); STACK_0 = RA_float_F(STACK_0); }
      # x,y nicht exakt =0, x/y und y/x werden Floats sein.
      pushSTACK(R_abs_R(STACK_1)); y = R_abs_R(STACK_(0+1)); x = popSTACK();
      if (R_R_comp(x,y) >= 0) # (abs x) und (abs y) vergleichen
        # |x| >= |y|
        { var reg1 object z = F_atanx_F(R_R_durch_R(STACK_0,STACK_1)); # atan(y/x)
          # Division war erfolgreich, also x/=0.
          if (R_mminusp(STACK_1))
            # x<0 -> pi bzw. -pi addieren:
            { STACK_1 = z; # atan(y/x) retten
              z = pi_F_float_F(z); # pi im selben Float-Format
              if (!R_mminusp(STACK_0))
                { z = F_F_plus_F(STACK_1,z); } # y>=0 -> atan(y/x) + pi
                else
                { z = F_F_minus_F(STACK_1,z); } # y<0 -> atan(y/x) - pi
            }
          skipSTACK(2);
          return z;
        }
        else
        # |x| < |y|
        { var reg1 object z = F_atanx_F(R_R_durch_R(STACK_1,STACK_0)); # atan(x/y)
          # von pi/2 bzw. -pi/2 subtrahieren:
          STACK_1 = z; # atan(x/y) retten
          z = pi_F_float_F(z); # pi im selben Float-Format
          z = F_I_scale_float_F(z,Fixnum_minus1); # pi/2
          if (R_mminusp(STACK_0)) { z = F_minus_F(z); } # y<0 -> -pi/2 statt pi/2
          z = F_F_minus_F(z,STACK_1); # +-pi/2 - atan(x/y)
          skipSTACK(2);
          return z;
        }
    }

# R_atan_R(x) liefert den Arctan einer reellen Zahl x.
# Ergebnis rational nur, wenn x=0.
# kann GC auslösen
  local object R_atan_R (object x);
# Methode:
# arctan(x) = arctan(X=1,Y=x).
#if 0
  local object R_atan_R(x)
    var reg1 object x;
    { return R_R_atan_R(Fixnum_1,x); }
#else # Macro spart Code
  #define R_atan_R(x)  R_R_atan_R(Fixnum_1,x)
#endif

# F_sinx_F(x) liefert zu einem Float x (betragsmäßig <2) (sin(x)/x)^2 als Float.
# kann GC auslösen
  local object F_sinx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<=-d/2 liefere 1.0
#   (denn bei e<=-d/2 ist x^2/6 < x^2/4 < 2^(-d)/4 = 2^(-d-2), also
#   1 >= sin(x)/x > 1-x^2/6 > 1-2^(-d-2), also 1 >= (sin(x)/x)^2 > 1-2^(-d-1),
#   also ist (sin(x)/x)^2, auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   sin(x)/x = sum(j=0..inf,(-x^2)^j/(2j+1)!):
#   a:=-x^2, b:=1, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum b))) do b:=b*a/((i+1)*(i+2)), i:=i+2.
#   Ergebnis sum^2.
# Sonst setze y := x/2 = (scale-float x -1),
#   berechne rekursiv z:=(sin(y)/y)^2 und liefere z*(1-y^2*z).
# [Die Grenze sqrt(d) ergibt sich so:
#  Man braucht bei der Potenzreihe mit x=2^-k etwa j Glieder, mit
#  k*j*ln 2 + j*(ln j - 1) = d, und der Aufwand beträgt etwa 2.8*(j/2)
#  Multiplikationen von d-Bit-Zahlen. Bei Halbierungen bis x=2^-k ist der
#  Gesamtaufwand etwa 2*(k+e)+1.4*j(k). Dieses minimieren nach k: Soll sein
#  -1.4 = d/dk j(k) = (d/dj k(j))^-1 = - j^2/(d+j)*ln 2, also j^2=2(d+j),
#  grob j=sqrt(2d) und damit k=sqrt(d).]
# Aufwand: asymptotisch d^2.5 .

# F_sinhx_F(x) liefert zu einem Float x (betragsmäßig <2) (sinh(x)/x)^2 als Float.
# kann GC auslösen
  local object F_sinhx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<=(1-d)/2 liefere 1.0
#   (denn bei e<=(1-d)/2 ist x^2/6 < x^2/4 < 2^(1-d)/4 = 2^(-d-1), also
#   1 <= sinh(x)/x = 1+x^2/6+... < 1+2^(-d-1), also 1 <= (sinh(x)/x)^2 < 1+2^(-d),
#   also ist (sinh(x)/x)^2, auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   sinh(x)/x = sum(j=0..inf,(x^2)^j/(2j+1)!):
#   a:=x^2, b:=1, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum b))) do b:=b*a/((i+1)*(i+2)), i:=i+2.
#   Ergebnis sum^2.
# Sonst setze y := x/2 = (scale-float x -1),
#   berechne rekursiv z:=(sinh(y)/y)^2 und liefere z*(1+y^2*z).
# [Die Grenze sqrt(d) ergibt sich so:
#  Man braucht bei der Potenzreihe mit x=2^-k etwa j Glieder, mit
#  k*j*ln 2 + j*(ln j - 1) = d, und der Aufwand beträgt etwa 2.8*(j/2)
#  Multiplikationen von d-Bit-Zahlen. Bei Halbierungen bis x=2^-k ist der
#  Gesamtaufwand etwa 2*(k+e)+1.4*j(k). Dieses minimieren nach k: Soll sein
#  -1.4 = d/dk j(k) = (d/dj k(j))^-1 = - j^2/(d+j)*ln 2, also j^2=2(d+j),
#  grob j=sqrt(2d) und damit k=sqrt(d).]
# Aufwand: asymptotisch d^2.5 .

# Generiert eine Funktion wie F_sinx_F
  #define GEN_F_sinx(name,f,flag,R_R_plusminus_R)                                               \
    local object CONCAT3(F_,name,_F) (x)                                                        \
      var reg4 object x;                                                                        \
      { if (R_zerop(x)) { return I_F_float_F(Fixnum_1,x); }                                     \
       {var reg6 uintL d = F_float_digits(x);                                                   \
        var reg5 sintL e = F_exponent_L(x);                                                     \
        if (e <= (sintL)(f-d)>>1) # e <= (f-d)/2 <==> e <= -ceiling((d-f)/2) ?                  \
          { return I_F_float_F(Fixnum_1,x); } # ja -> 1.0 als Ergebnis                          \
        pushSTACK(x); pushSTACK(NIL); pushSTACK(F_F_mal_F(x,x));                                \
        # Stackaufbau: x, y^2-Liste, x^2.                                                       \
        {# Bei e <= -1-floor(sqrt(d)) kann die Potenzreihe angewandt werden.                    \
         var reg3 sintL e_limit = -1-UL_sqrt_UW(d); # -1-floor(sqrt(d))                         \
         while (e > e_limit)                                                                    \
           # e > -1-floor(sqrt(d)) -> muß |x| verkleinern.                                      \
           { STACK_2 = F_I_scale_float_F(STACK_2,Fixnum_minus1); # x := x/2                     \
             STACK_0 = F_I_scale_float_F(STACK_0,sfixnum(-2)); # x^2 := x^2/4                   \
             e = e-1; # e := e-1                                                                \
             # (push x^2 y^2-Liste) :                                                           \
            {var reg1 object new_cons = allocate_cons();                                        \
             Car(new_cons) = STACK_0; Cdr(new_cons) = STACK_1;                                  \
             STACK_1 = new_cons;                                                                \
        }  }}                                                                                   \
        # Potenzreihe anwenden:                                                                 \
        if (flag) { STACK_0 = F_minus_F(STACK_0); } # a := -x^2 bzw. x^2                        \
        {var reg2 object i = Fixnum_1;                                                          \
         pushSTACK(I_F_float_F(Fixnum_1,STACK_2)); # b := (float 1 x)                           \
         pushSTACK(I_F_float_F(Fixnum_0,STACK_3)); # sum := (float 0 x)                         \
         # Stackaufbau: x, y^2-Liste, a, b, sum.                                                \
         loop                                                                                   \
           { var reg1 object temp;                                                              \
             temp = F_F_plus_F(STACK_0,STACK_1); # (+ sum b)                                    \
             if (eql(STACK_0,temp)) # = sum ?                                                   \
               break; # ja -> Potenzreihe abbrechen                                             \
             STACK_0 = temp;                                                                    \
             STACK_1 = F_F_mal_F(STACK_1,STACK_2); # b := b*a                                   \
             temp = I_I_mal_I(fixnum_inc(i,1),fixnum_inc(i,2)); # (i+1)*(i+2)                   \
             i = fixnum_inc(i,2); # i := i+2                                                    \
             STACK_1 = R_R_durch_R(STACK_1,temp); # b := b/((i+1)*(i+2))                        \
        }  }                                                                                    \
        {var reg1 object z = F_F_mal_F(STACK_0,STACK_0); # sum^2 als Ergebnis                   \
         # Stackaufbau: x, y^2-Liste, -, -, -.                                                  \
         # Wegen Rekursion noch die y^2-Liste abarbeiten:                                       \
         while (mconsp(STACK_3))                                                                \
           { var reg2 object temp;                                                              \
             temp = STACK_3; STACK_3 = Cdr(temp); temp = Car(temp); # nächstes y^2              \
             STACK_4 = z; # z retten                                                            \
             z = R_R_plusminus_R(Fixnum_1,F_F_mal_F(temp,z)); # 1 +- y^2*z                      \
             z = F_F_mal_F(STACK_4,z); # mit z multiplizieren                                   \
           }                                                                                    \
         skipSTACK(5);                                                                          \
         return z;                                                                              \
      }}}
# F_sinx_F : mit z -> z*(1-y^2*z), a = -x^2, -d/2
  GEN_F_sinx(sinx,0,TRUE,R_R_minus_R)
# F_sinhx_F : mit z -> z*(1+y^2*z), a = x^2, (1-d)/2
  GEN_F_sinx(sinhx,1,FALSE,R_R_plus_R)

# F_pi_round_I_F(x) dividiert ein Float x mit Rest durch pi.
# Beide Werte von (round x (float pi x)) auf den Stack.
# kann GC auslösen
  local void F_pi_round_I_F (object x);
  local void F_pi_round_I_F(x)
    var reg1 object x;
    { if (F_exponent_L(x) <= 0)
        # Exponent <=0 -> |x|<1 -> |x/pi| < 1/2, also Division unnötig
        { pushSTACK(Fixnum_0); pushSTACK(x); } # Quotient 0, Rest x
        else
        { pushSTACK(x); # x retten
         {var reg2 object pi = pi_F_float_F(x); # pi mit hinreichender Genauigkeit
          R_R_round_I_R(popSTACK(),pi); # x durch pi dividieren
        }}
    }

# F_pi2_round_I_F(x) dividiert ein Float x mit Rest durch pi/2.
# Beide Werte von (round x (float pi/2 x)) auf den Stack.
# kann GC auslösen
  local void F_pi2_round_I_F (object x);
  local void F_pi2_round_I_F(x)
    var reg1 object x;
    { if (F_exponent_L(x) < 0)
        # Exponent <0 -> |x|<1/2 -> |x/(pi/2)| < 1/2, also Division unnötig
        { pushSTACK(Fixnum_0); pushSTACK(x); } # Quotient 0, Rest x
        else
        { pushSTACK(x); # x retten
         {var reg2 object pi = pi_F_float_F(x); # pi mit hinreichender Genauigkeit
          pi = F_I_scale_float_F(pi,Fixnum_minus1); # pi/2 mit hinreichender Genauigkeit
          R_R_round_I_R(popSTACK(),pi); # x durch pi/2 dividieren
        }}
    }

# R_sin_R(x) liefert den Sinus (sin x) einer reellen Zahl x.
# kann GC auslösen
  local object R_sin_R (object x);
# Methode:
# x rational -> bei x=0 0 als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   (q,r) := (round x (float pi x)), so daß |r|<=pi/2.
#   (sin(r)/r)^2 errechnen, Wurzel ziehen, mit r multiplizieren
#   und - falls q ungerade - Vorzeichenwechsel.
  local object R_sin_R(x)
    var reg1 object x;
    { if (R_rationalp(x))
        { if (eq(x,Fixnum_0)) { return x; } # x=0 -> 0 als Ergebnis
          x = RA_float_F(x); # sonst in Float umwandeln
        }
      # x Float
      pushSTACK(x); # x retten
      x = F_extend_F(x); # Rechengenauigkeit erhöhen
      F_pi_round_I_F(x); # durch pi dividieren
      # Stackaufbau: Argument, q, r.
     {var reg2 object x;
      x = F_sqrt_F(F_sinx_F(STACK_0)); # Wurzel aus (sin(r)/r)^2
      x = F_F_mal_F(x,STACK_0); # mit r multiplizieren
      x = F_F_float_F(x,STACK_2); # und wieder runden
      if (I_oddp(STACK_1)) { x = F_minus_F(x); } # q ungerade -> mal -1
      skipSTACK(3);
      return x;
    }}

# R_cos_R(x) liefert den Cosinus (cos x) einer reellen Zahl x.
# kann GC auslösen
  local object R_cos_R (object x);
# Methode:
# x rational -> bei x=0 1 als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   (q,r) := (round x (float pi x)), so daß |r|<=pi/2.
#   e := Exponent aus (decode-float r), d := (float-digits r)
#   Bei r=0.0 oder e<=-d/2 liefere 1.0
#     (denn bei e<=-d/2 ist r^2/2 < 2^(-d)/2 = 2^(-d-1), also
#     1 >= cos(r) > 1-r^2/2 > 1-2^(-d-1),
#     also ist cos(r), auf d Bits gerundet, gleich 1.0).
#   Sonst s := r/2 = (scale-float r -1),
#     (sin(s)/s)^2 errechnen, cos(r) = 1-r*s*(sin(s)/s)^2 errechnen.
#   Falls q ungerade: Vorzeichenwechsel.
  local object R_cos_R(x)
    var reg1 object x;
    { if (R_rationalp(x))
        { if (eq(x,Fixnum_0)) { return Fixnum_1; } # x=0 -> 1 als Ergebnis
          x = RA_float_F(x); # sonst in Float umwandeln
        }
      # x Float
      pushSTACK(x); # x retten
      x = F_extend_F(x); # Rechengenauigkeit erhöhen
      F_pi_round_I_F(x); # durch pi dividieren
      # Stackaufbau: Argument, q, r.
     {var reg2 object x;
      x = STACK_0;
      if (R_zerop(x) # r=0.0 -> 1.0
          || (F_exponent_L(x) <= (sintL)(-F_float_digits(x))>>1) # e <= -d/2 <==> e <= -ceiling(d/2) ?
         )
        { x = I_F_float_F(Fixnum_1,STACK_2); } # (cos r) = 1.0
        else
        { var reg1 object s = F_I_scale_float_F(STACK_0,Fixnum_minus1); # s := r/2
          pushSTACK(s);
          s = F_sinx_F(s); # (sin(s)/s)^2
          s = F_F_mal_F(popSTACK(),s); # mit s multiplizieren
          s = F_F_mal_F(STACK_0,s); # mit r multiplizieren
          s = R_R_minus_R(Fixnum_1,s); # von 1 subtrahieren
          x = F_F_float_F(s,STACK_2); # und wieder runden
        }
      if (I_oddp(STACK_1)) { x = F_minus_F(x); } # q ungerade -> mal -1
      skipSTACK(3);
      return x;
    }}

# R_cos_sin_R_R(x) liefert ((cos x),(sin x)), beide Werte auf dem Stack.
# kann GC auslösen
  local void R_cos_sin_R_R (object x);
# Methode:
# x rational -> bei x=0 (1,0) als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   (q,r) := (round x (float pi/2 x)), so daß |r|<=pi/4.
#   y:=(sin(r)/r)^2 errechnen.
#   (cos r) berechnen:
#     e := Exponent aus (decode-float r), d := (float-digits r)
#     Bei r=0.0 oder e<=-d/2 liefere 1.0
#       (denn bei e<=-d/2 ist r^2/2 < 2^(-d)/2 = 2^(-d-1), also
#       1 >= cos(r) > 1-r^2/2 > 1-2^(-d-1),
#       also ist cos(r), auf d Bits gerundet, gleich 1.0).
#     Sonst sqrt(1-r^2*y).
#   (sin r) berechnen: r*sqrt(y).
#   Genauigkeit wieder verringern.
#   Falls q = 0 mod 4: ((cos r), (sin r))
#   Falls q = 1 mod 4: ((- (sin r)), (cos r))
#   Falls q = 2 mod 4: ((- (cos r)), (- (sin r)))
#   Falls q = 3 mod 4: ((sin r), (- (cos r)))
  local void R_cos_sin_R_R(x)
    var reg3 object x;
    { if (R_rationalp(x))
        { if (eq(x,Fixnum_0)) # x=0 -> (1,0) als Ergebnis
            { pushSTACK(Fixnum_1); pushSTACK(Fixnum_0); return; }
          x = RA_float_F(x); # sonst in Float umwandeln
        }
      # x Float
      pushSTACK(x); # x retten
      x = F_extend_F(x); # Rechengenauigkeit erhöhen
      F_pi2_round_I_F(x); # durch pi/2 dividieren
      # Stackaufbau: Argument, q, r.
      pushSTACK(F_sinx_F(STACK_0)); # y := (sin(r)/r)^2
      # Stackaufbau: Argument, q, r, y.
      # erste Komponente cos(r) berechnen:
      {var reg2 object x;
       x = STACK_0;
       if (R_zerop(x) # r=0.0 -> 1.0
           || (F_exponent_L(x) <= (sintL)(-F_float_digits(x))>>1) # e <= -d/2 <==> e <= -ceiling(d/2) ?
          )
         { x = I_F_float_F(Fixnum_1,STACK_3); } # (cos r) = 1.0
         else
         { var reg1 object r = STACK_1;
           r = F_F_mal_F(r,r); # r^2
           r = F_F_mal_F(r,STACK_0); # r^2*y
           r = R_R_minus_R(Fixnum_1,r); # 1-r^2*y
           r = F_sqrt_F(r); # sqrt(1-r^2*y)
           x = F_F_float_F(r,STACK_3); # und wieder runden
         }
       pushSTACK(x); # cos(r) retten
      }
      # zweite Komponente sin(r) berechnen:
      {var reg1 object x = F_sqrt_F(STACK_(0+1)); # Wurzel aus y
       x = F_F_mal_F(STACK_(1+1),x); # mit r multiplizieren
       x = F_F_float_F(x,STACK_(3+1)); # und wieder runden
      # evtl. Vorzeichenwechsel oder Vertauschen:
       {var reg2 object q = STACK_(2+1);
        switch (I_fixnump(q) # q mod 4, je nachdem ob q Fixnum oder Bignum
                ? ((oint)q >> oint_addr_shift) & (bit(2)-1)
                : TheBignum(q)->data[(uintL)(TheBignum(q)->length)-1] & (bit(2)-1)
               )
          { case 0:
              STACK_(2+1) = x; STACK_(3+1) = STACK_0; break;
            case 1:
              STACK_(3+1) = F_minus_F(x); STACK_(2+1) = STACK_0; break;
            case 2:
              STACK_(2+1) = F_minus_F(x); STACK_(3+1) = F_minus_F(STACK_0); break;
            case 3:
              STACK_(3+1) = x; STACK_(2+1) = F_minus_F(STACK_0); break;
      }}  }
      skipSTACK(2+1);
    }

# F_lnx_F(x) liefert zu einem Float x (>=1/2, <=2) ln(x) als Float.
# kann GC auslösen
  local object F_lnx_F (object x);
# Methode:
# y:=x-1, e := Exponent aus (decode-float y), d := (float-digits y)
# Bei y=0.0 oder e<=-d liefere y
#   (denn bei e<=-d ist y/2 < 2^(-d)/2 = 2^(-d-1), also
#   0 <= y - ln(x) < y^2/2 < 2^(-d-1)*y
#   also ist ln(x)/y, auf d Bits gerundet, gleich y).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   ln(x) = sum(j=0..inf,(-1)^j*y^(j+1)/(j+1)):
#   a:=-y, b:=y, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum (/ b i)))) do i:=i+1, b:=b*a.
#   Ergebnis sum.
# Sonst setze y := sqrt(x), berechne rekursiv z:=ln(y)
#   und liefere 2*z = (scale-float z 1).
# Aufwand: asymptotisch d^2.5 .
  local object F_lnx_F(x)
    var reg4 object x;
    { pushSTACK(x);
      x = R_R_minus_R(x,Fixnum_1); # y := (- x 1)
      if (R_zerop(x)) { skipSTACK(1); return x; } # y=0.0 -> y als Ergebnis
      pushSTACK(x);
      # Stackaufbau: x, y.
     {var reg6 uintL d = F_float_digits(x);
      var reg5 sintL e = F_exponent_L(x);
       if (e <= (sintL)(-d)) # e <= -d ?
         { x = STACK_0; skipSTACK(2); return x; } # ja -> y als Ergebnis
      { var reg2 object k = Fixnum_0; # Rekursionszähler k:=0
       {# Bei e <= -1-floor(sqrt(d)) kann die Potenzreihe angewandt werden.
        var reg3 sintL e_limit = -1-UL_sqrt_UW(d); # -1-floor(sqrt(d))
        while (e > e_limit)
          # e > -1-floor(sqrt(d)) -> muß |y| verkleinern.
          { var reg1 object x = F_sqrt_F(STACK_1); STACK_1 = x; # x := (sqrt x)
            x = R_R_minus_R(x,Fixnum_1); STACK_0 = x; # y := (- x 1) und
            e = F_exponent_L(x); # e neu berechnen
            k = fixnum_inc(k,1); # k:=k+1
       }  }
       # Stackaufbau: x, y.
       # Potenzreihe anwenden:
       {var reg2 object i = Fixnum_1;
        pushSTACK(I_F_float_F(Fixnum_0,STACK_1)); # sum := (float 0 x)
        STACK_2 = F_minus_F(STACK_1); # a := -y, b := y
        # Stackaufbau: a, b, sum.
        loop
          { var reg1 object temp;
            temp = R_R_durch_R(STACK_1,i); # (/ b i)
            temp = F_F_plus_F(STACK_0,temp); # (+ sum (/ b i))
            if (eql(STACK_0,temp)) # = sum ?
              break; # ja -> Potenzreihe abbrechen
            STACK_0 = temp;
            STACK_1 = F_F_mal_F(STACK_1,STACK_2); # b := b*a
            i = fixnum_inc(i,1); # i := i+1
       }  }
       {var reg1 object erg = STACK_0; # sum als Ergebnis
        skipSTACK(3);
        return F_I_scale_float_F(erg,k); # wegen Rekursion noch mal 2^k
    }}}}

# ln2_F_float_F(f) liefert die Zahl ln(2) im selben Float-Format wie f.
# kann GC auslösen
  local object ln2_F_float_F (object f);
  local object ln2_F_float_F(f)
    var reg3 object f;
    { var reg4 object ln2 = O(LF_ln2);
      floatcase(f,
                { return LF_to_SF(ln2); },
                { return LF_to_FF(ln2); },
                { return LF_to_DF(ln2); },
                ;
               );
     {var reg2 uintC f_len = TheLfloat(f)->len; # gewünschte Länge von ln(2)
      {var reg1 uintC len = TheLfloat(ln2)->len; # vorhandene Länge
       if (f_len < len) { return LF_shorten_LF(ln2,f_len); }
       if (f_len == len) { return ln2; }
      }
      # gewünschte > vorhandene Länge -> muß nachberechnen:
      {var reg4 uintC len = lf_len_extend(f_len); # einige Digits mehr verlangen
       var reg1 object temp = F_lnx_F(I_to_LF(fixnum(2),len)); # (ln 2.0)
       # temp = ln(2) ist fertig.
       return O(LF_ln2) = LF_shorten_LF(temp,f_len); # wieder verkürzen, als LF_ln2 abspeichern
    }}}

# Vergrößert eine Long-Float-Länge n, so daß aus d = intDsize*n
# mindestens d+sqrt(d)+2+(LF_exp_len-1) wird.
# Allgemein: intDsize*n + sqrt(intDsize*n) + 2 + 31 < intDsize*(n+inc)
# <==>       sqrt(intDsize*n) + 33 < intDsize*inc
# <==>       sqrt(intDsize*n) < intDsize*inc - 33
# <==>       intDsize*n < intDsize^2*inc^2 - 66*intDsize*inc + 1089
# <==>       n <= intDsize*inc^2 - 66*inc + floor(1089/intDsize)
  local uintC lf_len_extend2 (uintC n);
  local uintC lf_len_extend2(n)
    var reg1 uintC n;
    { var reg2 uintC inc =
        #define FITS(n,k)  ((n) <= (uintL)((intDsize*(k)-66)*(k)+floor(1089,intDsize)))
        #define n_max  (uintL)(bitm(intCsize)-1)
        #define TEST(i)  FITS(n_max,1UL<<i) || FITS(n,1UL<<i) ? 1UL<<i :
        TEST(0) TEST(1) TEST(2) TEST(3) TEST(4) TEST(5) TEST(6) TEST(7)
        TEST(8) TEST(9) TEST(10) TEST(11) TEST(12) TEST(13)
        (fehler_LF_toolong(),0);
        #undef TEST
        #undef n_max
        #undef FITS
      if ((n = n+inc) < inc) { fehler_LF_toolong(); }
      return n;
    }

# F_extend2_F(x) erweitert die Genauigkeit eines Floats x um eine Stufe
# SF -> FF -> DF -> LF(4) -> LF(5) -> LF(6) -> ...
# Ein Float mit d Mantissenbits und l Exponentenbits wird so zu einem Float
# mit mindestens d+sqrt(d)+2+(l-1) Mantissenbits.
# SF -> DF wegen 17+sqrt(17)+2+7 = 30.2 < 53
# FF -> DF wegen 24+sqrt(24)+2+7 = 37.9 < 53
# DF -> LF(5) wegen 53+sqrt(53)+2+10 = 72.3 < 80
# kann GC auslösen
  local object F_extend2_F (object x);
  local object F_extend2_F(x)
    var reg1 object x;
    { floatcase(x,
                { return SF_to_DF(x); }, # 17+sqrt(17)+2+7 = 30.2 < 53
                { return FF_to_DF(x); }, # 24+sqrt(24)+2+7 = 37.9 < 53
                { return DF_to_LF(x,ceiling(73,intDsize)); }, # 53+sqrt(53)+2+10 = 72.3 < 73
                { return LF_extend_LF(x,lf_len_extend2(TheLfloat(x)->len)); }
               );
    }

# R_ln_R(x) liefert zu einer reellen Zahl x>0 die Zahl ln(x).
# kann GC auslösen
  local object R_ln_R (object x);
# Methode:
# x rational -> bei x=1 0 als Ergebnis, sonst x in Float umwandeln.
# x Float ->
#   d := (float-digits x),
#   Genauigkeit um sqrt(d)+max(integer-length(e)) Bits erhöhen,
#   (m,e) := (decode-float x), so daß 1/2 <= m < 1.
#   m<2/3 -> m:=2m, e:=e-1, so daß 2/3 <= m <= 4/3.
#   ln(m) errechnen, ln(x)=ln(m)+e*ln(2) als Ergebnis.
  local object R_ln_R(x)
    var reg2 object x;
    { if (R_rationalp(x))
        { if (eq(x,Fixnum_1)) { return Fixnum_0; } # x=1 -> 0 als Ergebnis
          x = RA_float_F(x); # sonst in Float umwandeln
        }
      # x Float
      pushSTACK(x); # x retten
      x = F_extend2_F(x); # Rechengenauigkeit erhöhen
      F_decode_float_F_I_F(x); # m,e,s bestimmen
      # Stackaufbau: x, m, e, s.
      if (F_F_comp(STACK_2,
                   make_SF(0,0+SF_exp_mid,floor(bit(SF_mant_len+2),3)) # Short-Float 2/3
                  )
          < 0
         ) # m < 2/3 ->
        { STACK_2 = F_I_scale_float_F(STACK_2,Fixnum_1); # m verdoppeln
          STACK_1 = I_minus1_plus_I(STACK_1); # e decrementieren
        }
      STACK_2 = F_lnx_F(STACK_2); # ln(m) im genaueren Float-Format errechnen
     {var reg1 object temp;
      temp = ln2_F_float_F(STACK_0); # ln(2) im genaueren Float-Format
      temp = R_R_mal_R(STACK_1,temp); # e*ln(2)
      temp = R_R_plus_R(STACK_2,temp); # ln(m)+e*ln(2)
      temp = F_F_float_F(temp,STACK_3); # (float ... x)
      skipSTACK(4);
      return temp;
    }}
  #define F_ln_F  R_ln_R

# I_I_log_RA(a,b) liefert zu Integers a>0, b>1 den Logarithmus log(a,b)
# als exakte rationale Zahl, oder nullobj wenn er irrational ist.
# kann GC auslösen
  local object I_I_log_RA (object a, object b);
# Methode:
# log(a,b) soll Bruch c/d mit teilerfremdem c>=0,d>0 ergeben.
# a=1 -> c=0, d=1.
# a>=b -> Dividiere a durch b. Rest da -> geht nicht.
#         Sonst log(a,b) = 1+log(a/b,b).
#         Berechne also c/d := log(a/b,b) und setze c:=c+d.
# 1<a<b -> log(a,b) = 1/log(b,a).
#         Berechne also c/d := log(b,a) und vertausche c und d.
# Man konstruiert hierbei eigentlich die Kettenbruchentwicklung von c/d.
# Wegen a>=2^c, b>=2^d sind c,d < (integer-length a,b) < intDsize*2^intCsize.
# Entrekursiviert:
# Wir werden (a,b) und damit auch c/d = log(a/b) verändern.
# Invariante: Statt (c,d) wollen wir (uc*c+ud*d,vc*c+vd*d) zurückliefern.
# uc:=1, ud:=0, vc:=0, vd:=1.
# Solange a>1,
#   a>=b -> Dividiere a durch b. Rest da -> geht nicht.
#           Sonst a:=a/b, und (für später c:=c+d) uc:=uc+vc, ud:=ud+vd.
#   1<a<b -> vertausche a und b, uc und vc, ud und vd.
# Liefere (ud,vd), der Bruch ud/vd ist gekürzt.
# Offenbar braucht man uc und vc nicht.
  local object I_I_log_RA(a,b)
    var reg1 object a;
    var reg2 object b;
    { var reg3 uintL u = 0; # ud
      var reg4 uintL v = 1; # vd
      loop
        { if (eq(a,Fixnum_1)) break; # a=1 -> Rekursion zu Ende
          if (I_I_comp(a,b) >=0)
            # a>=b
            { pushSTACK(b);
              I_I_divide_I_I(a,b); # a durch b dividieren
              if (!eq(STACK_0,Fixnum_0)) # Rest /=0 ?
                { skipSTACK(3); return nullobj; } # -> fertig
              a = STACK_1; b = STACK_2; skipSTACK(3); # a:=a/b
              u = u + v; # ud:=ud+vd
            }
            else
            # 1<a<b -> a und b vertauschen
            {{var reg1 object temp = a; a = b; b = temp; }
             {var reg1 uintL temp = u; u = v; v = temp; } # ud und vd vertauschen
            }
        }
      # a=1 -> c=0,d=1 -> Ergebnis ud/vd
      pushSTACK(UL_to_I(u)); # u als Integer
     {var reg5 object y = UL_to_I(v); # v als Integer
      var reg6 object x = popSTACK();
      return I_I_to_RA(x,y);
    }}

# R_R_log_R(a,b) liefert zu reellen Zahlen a>0, b>0 die Zahl
# log(a,b)=ln(a)/ln(b).
# Ergebnis rational nur, wenn a=1 oder a und b rational.
# kann GC auslösen
  local object R_R_log_R (object a, object b);
# Methode:
# a und b rational:
#   b=1 -> Error
#   a=1 -> Ergebnis 0
#   b Integer:
#     a Integer: log(a,b) rational errechenbar -> liefern
#     a Ratio: a=a1/a2 mit a1>0, a2>1.
#              a1=1 und log(a2,b) rational errechenbar -> -log(a2,b) liefern
#   b Ratio: a=a1/a2, b=b1/b2 mit a1>0, a2>0, b1>0, b2>1.
#            log(a2,b2) rational errechenbar ->
#               b1=1 -> bei a1=1 liefern, sonst nicht.
#               b1>1 -> log(a1,b1) rational errechenbar und
#                       log(a1,b1)=log(a2,b2) -> liefern, sonst nicht.
#            sonst a1,a2 vertauschen:
#              log(a2/a1,b1/b2) versuchen (wie oben) ->
#                -log(a2/a1,b1/b2) liefern
#   Sonst a und b in Floats umwandeln.
# a Float, b rational -> bei b=1 Error, sonst b := (float b a)
# a rational, b Float -> bei a=1 Ergebnis 0, sonst a := (float a b)
# a,b Floats -> log(a,b) = ln(a)/ln(b)
  local object R_R_log_R(a,b)
    var reg2 object a;
    var reg1 object b;
    { pushSTACK(a); pushSTACK(b);
      # Stackaufbau: a, b.
      if (R_rationalp(b))
        # b rational
        { if (eq(b,Fixnum_1)) { divide_0(); } # b=1 -> Error
          if (R_rationalp(a))
            # a,b beide rational
            { if (eq(a,Fixnum_1)) { skipSTACK(2); return Fixnum_0; } # a=1 -> Ergebnis 0
              if (RA_integerp(b))
                # b Integer
                { if (RA_integerp(a))
                    # a,b beide Integers
                    { var reg3 object x = I_I_log_RA(a,b); # rationalen log(a,b) versuchen
                      if (!(x==nullobj)) { skipSTACK(2); return x; }
                    }
                    else
                    # a Ratio, b Integer
                    { if (eq(TheRatio(a)->rt_num,Fixnum_1)) # a1=1
                        { var reg3 object x = I_I_log_RA(TheRatio(a)->rt_den,b); # rationalen log(a2,b) versuchen
                          if (!(x==nullobj)) { skipSTACK(2); return RA_minus_RA(x); }
                }   }   }
                else
                # a rational, b Ratio
                { if (RA_integerp(a))
                    { pushSTACK(a); pushSTACK(a=Fixnum_1); }
                    else
                    { pushSTACK(TheRatio(a)->rt_num); pushSTACK(a=TheRatio(a)->rt_den); }
                  pushSTACK(TheRatio(b)->rt_num); pushSTACK(b=TheRatio(b)->rt_den);
                  # Stackaufbau: a, b, a1>0, a2>0, b1>0, b2>1.
                  {var reg3 object x = I_I_log_RA(a,b); # rationalen log(a2,b2) versuchen
                   if (!(x==nullobj))
                     { if (eq(STACK_1,Fixnum_1)) # b1=1 ?
                         { if (eq(STACK_3,Fixnum_1)) # a1=1 ?
                             { skipSTACK(6); return x; } # ja -> x liefern
                         }
                         else
                         { pushSTACK(x);
                          {var reg4 object y = I_I_log_RA(STACK_(3+1),STACK_(1+1)); # rationalen log(a1,b1) versuchen
                           if ((!(y==nullobj)) && eql(STACK_0,y)) # x=y ?
                             { x = STACK_0; skipSTACK(6+1); return x; }
                           skipSTACK(1);
                  }  }   }}
                  {var reg3 object x = I_I_log_RA(STACK_3,STACK_0); # rationalen log(a1,b2) versuchen
                   if (!(x==nullobj))
                     { if (eq(STACK_1,Fixnum_1)) # b1=1 ?
                         { if (eq(STACK_2,Fixnum_1)) # a2=1 ?
                             { skipSTACK(6); return RA_minus_RA(x); } # ja -> -x liefern
                         }
                         else
                         { pushSTACK(x);
                          {var reg4 object y = I_I_log_RA(STACK_(2+1),STACK_(1+1)); # rationalen log(a2,b1) versuchen
                           if ((!(y==nullobj)) && eql(STACK_0,y)) # x=y ?
                             { x = STACK_0; skipSTACK(6+1); return RA_minus_RA(x); }
                           skipSTACK(1);
                  }  }   }}
                  skipSTACK(4);
                }
              # a,b beide in Floats umwandeln:
              STACK_1 = RA_float_F(STACK_1); STACK_0 = RA_float_F(STACK_0);
            }
            else
            # a Float
            { STACK_0 = RA_F_float_F(b,a); } # b := (float b a)
        }
        else
        # b Float
        { if (R_rationalp(a))
            # a rational
            { if (eq(a,Fixnum_1)) { skipSTACK(2); return Fixnum_0; } # a=1 -> Ergebnis 0
              STACK_1 = RA_F_float_F(a,b); # a := (float a b)
        }   }
      # Nun a,b beide Floats.
      STACK_1 = R_ln_R(STACK_1); # (ln a) errechnen
     {var reg3 object lnb = R_ln_R(popSTACK()); # (ln b) errechnen
      return F_F_durch_F(popSTACK(),lnb); # (/ (ln a) (ln b)) als Ergebnis
    }}

# F_expx_F(x) liefert zu einem Float x (betragsmäßig <1) exp(x) als Float.
# kann GC auslösen
  local object F_expx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<-d liefere 1.0
#   (denn bei e<=-d-1 ist abs(exp(x)-1) = abs(x)+O(x^2) < 2^(-d-1),
#    also ist exp(x), auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   exp(x) = sum(j=0..inf,x^j/j!):
#   b:=1, i:=0, sum:=0,
#   while (/= sum (setq sum (+ sum b))) do b:=b*x/(i+1), i:=i+1.
#   Ergebnis sum.
# Sonst setze y := x/2 = (scale-float x -1),
#   berechne rekursiv z:=exp(y) und liefere z^2.
# Aufwand: asymptotisch d^2.5 .
  local object F_expx_F (x)
    var reg4 object x;
    { if (R_zerop(x)) { return I_F_float_F(Fixnum_1,x); }
     {var reg6 uintL d = F_float_digits(x);
      var reg5 sintL e = F_exponent_L(x);
      if (e < (sintL)(-d)) # e < -d ?
        { return I_F_float_F(Fixnum_1,x); } # ja -> 1.0 als Ergebnis
      pushSTACK(x);
      # Stackaufbau: x.
      {var reg3 uintL k = 0; # Rekursionszähler k:=0
       {# Bei e <= -1-floor(sqrt(d)) kann die Potenzreihe angewandt werden.
        var reg1 sintL e_limit = -1-UL_sqrt_UW(d); # -1-floor(sqrt(d))
        if (e > e_limit)
          # e > -1-floor(sqrt(d)) -> muß |x| verkleinern.
          { k = e - e_limit;
           {var reg1 object temp = L_to_I((sintL)(-k));
            STACK_0 = F_I_scale_float_F(STACK_0,temp); # x := x/2^k
            # Neuer Exponent = e-k = e_limit.
       }  }}
       # Potenzreihe anwenden:
       {var reg2 object i = Fixnum_0;
        pushSTACK(I_F_float_F(Fixnum_1,STACK_0)); # b := (float 1 x)
        pushSTACK(I_F_float_F(Fixnum_0,STACK_1)); # sum := (float 0 x)
        # Stackaufbau: x, b, sum.
        loop
          { var reg1 object temp;
            temp = F_F_plus_F(STACK_0,STACK_1); # (+ sum b)
            if (eql(STACK_0,temp)) # = sum ?
              break; # ja -> Potenzreihe abbrechen
            STACK_0 = temp;
            temp = F_F_mal_F(STACK_1,STACK_2); # b*x
            i = fixnum_inc(i,1); # i := i+1
            STACK_1 = R_R_durch_R(temp,i); # b := b*x/i
       }  }
       {var reg1 object z = STACK_0; # sum als Ergebnis
        skipSTACK(3);
        # Wegen Rekursion noch k mal quadrieren:
        dotimesL(k,k, { z = F_F_mal_F(z,z); } );
        return z;
    }}}}

# R_exp_R(x) liefert zu einer reellen Zahl x die Zahl exp(x).
# kann GC auslösen
  local object R_exp_R (object x);
# Methode:
# x rational -> bei x=0 1 als Ergebnis, sonst x in Float umwandeln.
# x Float ->
#   d := (float-digits x),
#   Genauigkeit um sqrt(d)+max(integer-length(e)) Bits erhöhen,
#   (q,r) := (floor x ln(2))
#   Ergebnis ist exp(q*ln(2)+r) = (scale-float exp(r) q).
  local object R_exp_R(x)
    var reg1 object x;
    { if (R_rationalp(x))
        # x rational
        { if (eq(x,Fixnum_0)) { return Fixnum_1; } # x=0 -> 1 als Ergebnis
          x = RA_float_F(x); # sonst in Float umwandeln
        }
      # x Float
      pushSTACK(x); # x retten
      x = F_extend2_F(x); # Genauigkeit vergrößern
      # durch ln(2) dividieren (bei 0<=x<1/2 kann man sofort q:=0 setzen)
      if ((!R_minusp(x)) && (F_exponent_L(x)<0))
        { pushSTACK(Fixnum_0); pushSTACK(x); } # x>=0, Exponent <0 -> 0<=x<1/2 -> Division unnötig
        else
        { pushSTACK(x);
         {var reg1 object ln2 = ln2_F_float_F(x); # ln(2) mit hinreichender Genauigkeit
          x = popSTACK();
          F_F_floor_I_F(x,ln2); # x durch ln(2) dividieren
        }}
      # Stackaufbau: originales x, q, r.
     {var reg2 object temp = F_expx_F(STACK_0); # exp(r)
      temp = F_I_scale_float_F(temp,STACK_1); # mal 2^q
      temp = F_F_float_F(temp,STACK_2); # (float ... x) als Ergebnis
      skipSTACK(3);
      return temp;
    }}

# R_sinh_R(x) liefert zu einer reellen Zahl x die Zahl sinh(x).
# kann GC auslösen
  local object R_sinh_R (object x);
# Methode:
# x rational -> bei x=0 0 als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   e := Exponent aus (decode-float x)
#   falls e<=0: (sinh(x)/x)^2 errechnen, Wurzel ziehen, mit x multiplizieren.
#   falls e>0: y:=exp(x) errechnen, (scale-float (- y (/ y)) -1) bilden.
  local object R_sinh_R(x)
    var reg2 object x;
    { if (R_rationalp(x))
        # x rational
        { if (eq(x,Fixnum_0)) { return x; } # x=0 -> 0 als Ergebnis
          x = RA_float_F(x); # sonst in Float umwandeln
        }
      # x Float
      if (F_exponent_L(x)<=0) # Exponent e abtesten
        # e<=0
        { var reg1 object temp;
          pushSTACK(x);
          pushSTACK(temp = F_extend_F(x)); # Rechengenauigkeit erhöhen
          temp = F_sqrt_F(F_sinhx_F(x)); # Wurzel aus (sinh(x)/x)^2
          temp = F_F_mal_F(temp,STACK_0); # mit genauerem x multiplizieren
          temp = F_F_float_F(temp,STACK_1); # und wieder runden
          skipSTACK(2);
          return temp;
        }
        else
        # e>0 -> verwende exp(x)
        { var reg1 object temp;
          pushSTACK(temp = R_exp_R(x)); # y:=exp(x)
          temp = F_durch_F(temp); # (/ y)
          temp = F_F_minus_F(popSTACK(),temp); # von y subtrahieren
          return F_I_scale_float_F(temp,Fixnum_minus1); # (scale-float ... -1)
    }   }

# R_cosh_R(x) liefert zu einer reellen Zahl x die Zahl cosh(x).
# kann GC auslösen
  local object R_cosh_R (object x);
# Methode:
# x rational -> bei x=0 1 als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   e := Exponent aus (decode-float x), d := (float-digits x)
#   falls x=0.0 oder e<=(1-d)/2 liefere 1.0
#     (denn bei e<=(1-d)/2 ist 1 <= cosh(x) = 1+x^2/2+... < 1+2^(-d),
#      also ist cosh(x), auf d Bits gerundet, gleich 1.0).
#   falls e<=0:
#     y := x/2 = (scale-float x -1), (sinh(y)/y)^2 errechnen,
#     cosh(x) = 1+x*y*(sinh(y)/y)^2 errechnen.
#   falls e>0: y:=exp(x) errechnen, (scale-float (+ y (/ y)) -1) bilden.
  local object R_cosh_R(x)
    var reg2 object x;
    { if (R_rationalp(x))
        # x rational
        { if (eq(x,Fixnum_0)) { return Fixnum_1; } # x=0 -> 1 als Ergebnis
          x = RA_float_F(x); # sonst in Float umwandeln
        }
      # x Float
      {var reg3 sintL e = F_exponent_L(x);
       if (e > 0)
         # e>0 -> verwende exp(x)
         { var reg1 object temp;
           pushSTACK(temp = R_exp_R(x)); # y:=exp(x)
           temp = F_durch_F(temp); # (/ y)
           temp = F_F_plus_F(popSTACK(),temp); # zu y addieren
           return F_I_scale_float_F(temp,Fixnum_minus1); # (scale-float ... -1)
         }
         else
         # e<=0
         { if (R_zerop(x)) { return I_F_float_F(Fixnum_1,x); }
          {var reg1 uintL d = F_float_digits(x);
           if (e <= (sintL)(1-d)>>1) # e <= (1-d)/2 <==> e <= -ceiling((d-1)/2) ?
             { return I_F_float_F(Fixnum_1,x); } # ja -> 1.0 als Ergebnis
          }
          {var reg1 object temp;
           pushSTACK(x);
           pushSTACK(temp = F_extend_F(x)); # Rechengenauigkeit erhöhen
           pushSTACK(temp = F_I_scale_float_F(temp,Fixnum_minus1)); # y=(scale-float x -1)
           temp = F_sinhx_F(temp); # (sinh(y)/y)^2
           temp = F_F_mal_F(STACK_0,temp); # mit y multiplizieren
           temp = F_F_mal_F(STACK_1,temp); # mit x multiplizieren
           temp = R_R_plus_R(Fixnum_1,temp); # 1 addieren
           temp = F_F_float_F(temp,STACK_2); # und wieder runden
           skipSTACK(3);
           return temp;
         }}
    } }

# R_cosh_sinh_R_R(x) liefert ((cosh x),(sinh x)), beide Werte auf dem Stack.
# kann GC auslösen
  local void R_cosh_sinh_R_R (object x);
# Methode:
# x rational -> bei x=0 (1,0) als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   e := Exponent aus (decode-float x), d := (float-digits x)
#   falls x=0.0 oder e<=(1-d)/2 liefere (1.0,x)
#     (denn bei e<=(1-d)/2 ist
#      1 <= sinh(x)/x < cosh(x) = 1+x^2/2+... < 1+2^(-d),
#      also ist cosh(x), auf d Bits gerundet, gleich 1.0
#      und sinh(x), auf d Bits gerundet, gleich x).
#   falls e<=0:
#     y:=(sinh(x)/x)^2 errechnen,
#     cosh(x) = sqrt(1+x^2*y) und sinh(x) = x*sqrt(y) errechnen.
#   falls e>0: y:=exp(x) errechnen,
#     (scale-float (+ y (/ y)) -1) und (scale-float (- y (/ y)) -1) bilden.
#   Genauigkeit wieder verringern.
  local void R_cosh_sinh_R_R(x)
    var reg3 object x;
    { if (R_rationalp(x))
        # x rational
        { if (eq(x,Fixnum_0)) { pushSTACK(Fixnum_1); pushSTACK(Fixnum_0); return; } # x=0 -> (1,0) als Ergebnis
          x = RA_float_F(x); # sonst in Float umwandeln
        }
      # x Float
      {var reg4 sintL e = F_exponent_L(x);
       if (e > 0)
         # e>0 -> verwende exp(x)
         { var reg1 object temp;
           pushSTACK(temp = R_exp_R(x)); # y:=exp(x)
           pushSTACK(temp = F_durch_F(temp)); # (/ y)
           # Stackaufbau: exp(x), exp(-x).
           temp = F_F_minus_F(STACK_1,temp); # von y subtrahieren
           temp = F_I_scale_float_F(temp,Fixnum_minus1); # (scale-float ... -1)
          {var reg2 object temp2 = STACK_0;
           STACK_0 = temp;
           temp = F_F_plus_F(STACK_1,temp2); # zu y addieren
           STACK_1 = F_I_scale_float_F(temp,Fixnum_minus1); # (scale-float ... -1)
           return;
         }}
         else
         # e<=0
         { if (R_zerop(x)
               || (e <= (sintL)(1-F_float_digits(x))>>1) # e <= (1-d)/2 <==> e <= -ceiling((d-1)/2) ?
              )
             { pushSTACK(x); pushSTACK(x); STACK_1 = I_F_float_F(Fixnum_1,x); return; }
          {var reg1 object temp;
           pushSTACK(x);
           pushSTACK(temp = F_extend_F(x)); # Rechengenauigkeit erhöhen
           pushSTACK(F_F_mal_F(temp,temp)); # x*x
           pushSTACK(temp = F_sinhx_F(STACK_1)); # y:=(sinh(x)/x)^2
           # Stackaufbau: originales x, x, x^2, y.
           temp = F_sqrt_F(temp); # sqrt(y) = sinh(x)/x
           temp = F_F_mal_F(STACK_2,temp); # x*sqrt(y) = sinh(x)
           STACK_2 = F_F_float_F(temp,STACK_3); # und wieder runden
           temp = F_F_mal_F(STACK_1,STACK_0); # x^2*y
           temp = F_sqrt_F(R_R_plus_R(Fixnum_1,temp)); # sqrt(1+x^2*y)
           STACK_3 = F_F_float_F(temp,STACK_3); # und wieder runden
           skipSTACK(2); return;
         }}
    } }

