# Transzendente Funktionen für komplexe Zahlen

# N_phase_R(x) liefert (phase x), wo x eine Zahl ist.
# Ergebnis rational nur wenn (= x 0) oder wenn x reell und >0.
# kann GC auslösen
  local object N_phase_R (object x);
# Methode:
# (= x 0) -> willkürliches Ergebnis 0
# x reell -> Winkel von (x,0) in Polarkoordinaten
# x komplex -> Winkel von ((realpart x),(imagpart x)) in Polarkoordinaten
  local object N_phase_R(x)
    var reg1 object x;
    { if (N_zerop(x)) { return Fixnum_0; }
      elif (N_realp(x)) { return R_R_atan_R(x,Fixnum_0); }
      else { return R_R_atan_R(TheComplex(x)->c_real,TheComplex(x)->c_imag); }
    }

# N_exp_N(x) liefert (exp x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_exp_N (object x);
# Methode:
# x reell -> klar.
# x = a+bi -> (exp a) mit (cos b) + i (sin b) multiplizieren:
#             (complex (* (exp a) (cos b)) (* (exp a) (sin b)))
  local object N_exp_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_exp_R(x); }
        else
        # x=a+bi
        { pushSTACK(TheComplex(x)->c_real); # a retten
          R_cos_sin_R_R(TheComplex(x)->c_imag); # (cos b) und (sin b) errechnen
          # Stackaufbau: a, cos(b), sin(b).
          # Da b nicht = Fixnum 0 ist, ist auch sin(b) nicht = Fixnum 0.
          STACK_2 = x = R_exp_R(STACK_2); # (exp a)
          # Stackaufbau: exp(a), cos(b), sin(b).
          STACK_0 = R_R_mal_R(x,STACK_0); # (* (exp a) (sin b)), nicht = Fixnum 0
          x = R_R_mal_R(STACK_2,STACK_1); # (* (exp a) (cos b))
          x = R_R_complex_C(x,STACK_0); # (complex ... ...)
          skipSTACK(3); return x;
        }
    }

# N_log_N(x) liefert (log x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_log_N (object x);
# Methode:
# (complex (log (abs x)) (phase x))
  local object N_log_N(x)
    var reg1 object x;
    { pushSTACK(x); # x retten
     {var reg2 object r = N_abs_R(x); # (abs x)
      if (R_zerop(r)) { divide_0(); } # (abs x) = 0 -> Error
      r = R_ln_R(r); # (log (abs x))
      x = STACK_0; STACK_0 = r;
      x = N_phase_R(x); # (phase x)
      return R_R_complex_N(popSTACK(),x); # (complex r (phase x))
    }}

# N_N_log_N(a,b) liefert (log a b), wo a und b Zahlen sind.
# kann GC auslösen
  local object N_N_log_N (object a, object b);
# Methode:
# (log a b) =
#   falls b reell, >0:
#     (complex (/ (log (abs a)) (log b)) (/ (phase a) (log b))), genauer:
#     falls a reell, >0: bekannt
#     falls (= a 0): Error
#     sonst: (phase a) errechnen, ein Float.
#            b (falls rational) ins selbe Float-Format umwandeln,
#            Imaginärteil := (/ (phase a) (log dieses_b)).
#            Falls a rational: (log (abs a) b).
#            Falls a komplex mit rationalem Real- und Imaginärteil,
#              Betragsquadrat  (expt (abs a) 2)  exakt ausrechnen als
#              (+ (expt (realpart a) 2) (expt (imagpart a) 2)).
#              Setze  Realteil := (/ (log Betragsquadrat b) 2).
#              [Eventuell wird hierbei (log b) ein zweites Mal ausgerechnet,
#               aber dies sowieso nur in Single-Precision.]
#            Sonst bilde (abs a), ein Float, und (log (abs a)), ein Float,
#              wandle b (falls rational) ins selbe Float-Format um,
#              setze  Realteil := (/ (log (abs a)) (log dieses_b)).
#   sonst: (/ (log a) (log b))
  local object N_N_log_N(a,b)
    var reg1 object a;
    var reg2 object b;
    { if (N_realp(b) && R_plusp(b))
        # b ist reell und >0
        { if (N_realp(a) && R_plusp(a))
            # a und b sind beide reell und >0
            { return R_R_log_R(a,b); }
            else
            # b ist reell und >0, a aber nicht
            { pushSTACK(a); pushSTACK(b); # a,b retten
              # Imaginärteil (/ (phase a) (log b)) errechnen:
              {var reg3 object angle = N_phase_R(a); # (phase a)
               if (eq(angle,Fixnum_0)) # = Fixnum 0 <==> (= a 0) -> Error
                 { divide_0(); }
               # durch (log b) dividieren, liefert den Imaginärteil:
               pushSTACK(angle);
               b = STACK_1; if (R_rationalp(b)) { b = RA_F_float_F(b,angle); }
               b = F_ln_F(b); STACK_0 = F_F_durch_F(STACK_0,b);
              }
              # Stackaufbau: a, b, Imaginärteil.
              # Realteil (/ (log (abs a)) (log b)) errechnen:
              a = STACK_2;
              if (N_realp(a))
                { if (R_rationalp(a))
                    # a rational -> (log (abs a) b) errechnen:
                    { a = R_abs_R(a); # Betrag (>0)
                      pushSTACK(R_R_log_R(a,STACK_1));
                      goto real_ok;
                }   }
                else
                { if (R_mrationalp(TheComplex(a)->c_real)
                      && R_mrationalp(TheComplex(a)->c_imag)
                     )
                    # a komplex mit rationalem Real- und Imaginärteil a1,a2
                    { # Betragsquadrat a1^2+a2^2 errechnen:
                      pushSTACK(TheComplex(a)->c_imag);
                      {var reg3 object a1 = TheComplex(a)->c_real;
                       a1 = RA_RA_mal_RA(a1,a1); # a1*a1
                       {var reg4 object a2 = STACK_0; STACK_0 = a1;
                        a1 = RA_RA_mal_RA(a2,a2); # a2*a2
                        a = RA_RA_plus_RA(STACK_0,a1);
                      }}
                      # davon der Logarithmus zur Basis b, durch 2:
                      STACK_0 = R_R_durch_R(R_R_log_R(a,STACK_2),fixnum(2));
                      goto real_ok;
                }   }
              # Keine Chance für rationalen Realteil
              pushSTACK(F_ln_F(N_abs_R(a))); # (log (abs a)), ein Float
              # durch (log b) dividieren, liefert den Realteil:
              b = STACK_2; if (R_rationalp(b)) { b = RA_F_float_F(b,STACK_0); }
              b = F_ln_F(b); STACK_0 = F_F_durch_F(STACK_0,b);
              real_ok:
              # Stackaufbau: a, b, Imaginärteil, Realteil.
             {var reg3 object erg = R_R_complex_C(STACK_0,STACK_1);
              skipSTACK(4); return erg;
            }}
        }
        else
        # normaler komplexer Fall
        { pushSTACK(b); a = N_log_N(a); b = STACK_0; # (log a)
          STACK_0 = a; b = N_log_N(b); a = popSTACK(); # (log b)
          return N_N_durch_N(a,b); # dividieren
        }
    }

# N_I_expt_N(x,y) = (expt x y), wo x eine Zahl und y ein Integer ist.
# kann GC auslösen
  local object N_I_expt_N (object x, object y);
  # Methode:
  # Für y>0:
  #   a:=x, b:=y.
  #   Solange b gerade, setze a:=a*a, b:=b/2. [a^b bleibt invariant, = x^y.]
  #   c:=a.
  #   Solange b:=floor(b/2) >0 ist,
  #     setze a:=a*a, und falls b ungerade, setze c:=a*c.
  #   Ergebnis c.
  # Für y=0: Ergebnis 1.
  # Für y<0: (/ (expt x (- y))).
  local object N_I_expt_N(x,y)
    var reg3 object x;
    var reg1 object y;
    { if (N_realp(x)) { return R_I_expt_R(x,y); } # x reell -> schnellere Routine
      if (eq(y,Fixnum_0)) { return Fixnum_1; } # y=0 -> Ergebnis 1
      pushSTACK(x);
     {var reg4 boolean y_negative = FALSE;
      if (R_minusp(y)) { y = I_minus_I(y); y_negative = TRUE; } # Betrag von y nehmen
      # Nun ist y>0.
      pushSTACK(y);
      # Stackaufbau: a, b.
      while (!I_oddp(y))
        { var reg2 object a = STACK_1; STACK_1 = N_N_mal_N(a,a); # a:=a*a
          STACK_0 = y = I_I_ash_I(STACK_0,Fixnum_minus1); # b := (ash b -1)
        }
      pushSTACK(STACK_1); # c:=a
      # Stackaufbau: a, b, c.
      until (eq(y=STACK_1,Fixnum_1)) # Solange b/=1
        { STACK_1 = I_I_ash_I(y,Fixnum_minus1); # b := (ash b -1)
         {var reg2 object a = STACK_2; STACK_2 = a = N_N_mal_N(a,a); # a:=a*a
          if (I_oddp(STACK_1)) { STACK_0 = N_N_mal_N(a,STACK_0); } # evtl. c:=a*c
        }}
      x = STACK_0; skipSTACK(3);
      # (expt x (abs y)) ist jetzt in x.
      return (y_negative ? N_durch_N(x) : x); # evtl. noch Kehrwert nehmen
    }}

# N_N_expt_N(x,y) = (expt x y), wo x und y Zahlen sind.
# kann GC auslösen
  local object N_N_expt_N (object x, object y);
  # Methode:
  # Falls y rational:
  #   Falls y Integer:
  #     Falls y=0: Ergebnis 1,
  #       [Nach CLTL folgendermaßen:
  #         x reell:
  #           x rational -> Fixnum 1
  #           x Float -> (float 1 x)
  #         x komplex:
  #           x komplex rational -> Fixnum 1
  #           sonst: #C(1.0 0.0) im Float-Format des Real- bzw. Imaginärteils von x
  #       ]
  #     Falls x rational oder komplex rational oder |y| klein:
  #       x^|y| durch wiederholtes Quadrieren und Multiplizieren und evtl.
  #       Kehrwert-Bilden ermitteln.
  #     Sonst wie bei 'y Float'.
  #   Falls y Ratio m/n:
  #     Es gilt (expt x m/n) = (expt (expt x 1/n) m).
  #     Falls x in Q(i) liegt (also rational oder komplex rational ist):
  #       Sollte x^(m/n) in Q(i) liegen, so auch eine n-te Wurzel x^(1/n)
  #       (und bei n=2 oder n=4 damit auch alle n-ten Wurzeln x^(1/n) ).
  #       Falls x rational >=0: n-te Wurzel aus x nehmen. Ist sie rational,
  #         deren m-te Potenz als Ergebnis.
  #       Falls x rational <=0 oder komplex rational und n Zweierpotenz:
  #         n-te Wurzel aus x nehmen (mehrfaches sqrt). Ist sie rational oder
  #         komplex rational, deren m-te Potenz als Ergebnis.
  #         [Beliebige n betrachten!??]
  #     Falls n Zweierpotenz und |m|,n klein: n-te Wurzel aus x nehmen
  #       (mehrfaches sqrt), davon die m-te Potenz durch wiederholtes
  #       Quadrieren und Multiplizieren und evtl. Kehrwert-Bilden.
  #     Sonst wie bei 'y Float'.
  # Falls y Float oder komplex:
  #   Falls (zerop x):
  #     Falls Realteil von y >0 :
  #       liefere 0.0 falls x und y reell, #C(0.0 0.0) sonst.
  #     Sonst Error.
  #   Falls y=0.0:
  #     liefere 1.0 falls x und y reell, #C(1.0 0.0) sonst.
  #   Sonst: (exp (* (log x) y))
  # Das Ergebnis liegt in Q(i), falls x in Q(i) liegt und 4y ein Integer ist.??
  # Genauigkeit erhöhen, log2(|y|) Bits mehr??
  # Bei x oder y rational und der andere Long-Float: bitte kein Single-Float!??
  local object N_N_expt_N(x,y)
    var reg2 object x;
    var reg1 object y;
    { if (N_realp(y) && R_rationalp(y))
        # y rational
        { if (RA_integerp(y))
            # y Integer
            { if (eq(y,Fixnum_0))
                #ifdef STRICT_CLTL
                # y=0 -> 1 im Format von x.
                { if (N_realp(x))
                    { if (R_rationalp(x))
                        { return Fixnum_1; }
                        else
                        { return I_F_float_F(Fixnum_1,x); }
                    }
                    else
                    { x = R_R_contagion_R(TheComplex(x)->c_real,TheComplex(x)->c_imag);
                      if (R_rationalp(x))
                        { return Fixnum_1; }
                        else
                        { x = I_F_float_F(Fixnum_0,x); # 0.0
                          pushSTACK(x);
                          x = I_F_float_F(Fixnum_1,x); # 1.0
                          return R_R_complex_C(x,popSTACK()); # #C(1.0 0.0)
                    }   }
                }
                #else
                # Exponent exakt 0 -> Ergebnis exakt 1
                { return Fixnum_1; }
                #endif
              if (I_fixnump(y)) { return N_I_expt_N(x,y); } # |y| klein ?
              if (N_realp(x))
                { if (R_rationalp(x)) { return R_I_expt_R(x,y); } }
                else
                { if (R_rationalp(TheComplex(x)->c_real) && R_rationalp(TheComplex(x)->c_imag))
                    { return N_I_expt_N(x,y); }
                }
            }
            else
            # y Ratio
            { if (N_realp(x))
                { if (R_rationalp(x))
                    { if (R_minusp(x)) goto complex_rational;
                      # x rational >=0
                      pushSTACK(x); pushSTACK(y);
                      {var reg1 object temp = RA_rootp(x,TheRatio(y)->rt_den); # n-te Wurzel versuchen
                       if (!(temp==nullobj)) # Wurzel rational?
                         {var reg1 object m = TheRatio(STACK_0)->rt_num;
                          skipSTACK(2);
                          return R_I_expt_R(temp,m); # (x^(1/n))^m
                      }  }
                      y = popSTACK(); x = popSTACK();
                }   }
                else
                { if (R_rationalp(TheComplex(x)->c_real) && R_rationalp(TheComplex(x)->c_imag))
                    complex_rational: # x in Q(i)
                    {var reg1 uintL k = I_power2p(TheRatio(y)->rt_den);
                     if (!(k==0))
                       # n Zweierpotenz = 2^(k-1). n>1, also k>1
                       { pushSTACK(TheRatio(y)->rt_num); # m retten
                         dotimespL(k,k-1, { x = N_sqrt_N(x); } ); # k-1 mal Quadratwurzel
                         return N_I_expt_N(x,popSTACK()); # dann hoch m
                }   }  }
              if (I_fixnump(TheRatio(y)->rt_num) # |m| klein
                  && I_fixnump(TheRatio(y)->rt_den) # n klein
                 )
                {var reg1 uintL n = posfixnum_to_L(TheRatio(y)->rt_den);
                 if ((n & (n-1)) == 0) # n Zweierpotenz?
                   { pushSTACK(TheRatio(y)->rt_num); # m retten
                     until ((n = n>>1) ==0) { x = N_sqrt_N(x); } # n-te Wurzel ziehen
                     return N_I_expt_N(x,popSTACK()); # dann hoch m
                }  }
            }
        }
      # allgemeiner Fall (z.B. y Float oder komplex):
      if (N_zerop(x)) # x=0.0 ?
        { if (!R_plusp(N_realpart_R(y))) # Realteil von y <=0 ?
            { divide_0(); } # ja -> Error
          if (N_realp(x) && N_realp(y))
            { x = R_R_contagion_R(x,y); # ein Float, da sonst x = Fixnum 0 gewesen wäre
              return I_F_float_F(Fixnum_0,x); # 0.0
            }
            else
            { if (!N_realp(x)) { x = R_R_contagion_R(TheComplex(x)->c_real,TheComplex(x)->c_imag); }
              if (!N_realp(y)) { y = R_R_contagion_R(TheComplex(y)->c_real,TheComplex(y)->c_imag); }
              x = R_R_contagion_R(x,y); # ein Float, da sonst x = Fixnum 0 gewesen wäre
              x = I_F_float_F(Fixnum_0,x); # 0.0
              return R_R_complex_C(x,x); # #C(0.0 0.0)
            }
        }
      if (N_zerop(y)) # y=0.0 ?
        { if (N_realp(x) && N_realp(y))
            { x = R_R_contagion_R(x,y); # ein Float, da sonst y = Fixnum 0 gewesen wäre
              return I_F_float_F(Fixnum_1,x); # 1.0
            }
            else
            { if (!N_realp(x)) { x = R_R_contagion_R(TheComplex(x)->c_real,TheComplex(x)->c_imag); }
              if (!N_realp(y)) { y = R_R_contagion_R(TheComplex(y)->c_real,TheComplex(y)->c_imag); }
              x = R_R_contagion_R(x,y); # ein Float, da sonst y = Fixnum 0 gewesen wäre
              x = I_F_float_F(Fixnum_0,x); # 0.0
              pushSTACK(x);
              x = I_F_float_F(Fixnum_1,x); # 1.0
              return R_R_complex_C(x,popSTACK()); # #C(1.0 0.0)
            }
        }
      pushSTACK(y);
     {var reg1 object temp = N_log_N(x); # (log x)
      temp = N_N_mal_N(temp,popSTACK()); # mal y
      return N_exp_N(temp); # exp davon
    }}

# N_sin_N(x) liefert (sin x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_sin_N (object x);
# Methode:
# x reell -> klar
# x = a+bi -> (complex (* (sin a) (cosh b)) (* (cos a) (sinh b)))
  local object N_sin_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_sin_R(x); }
        else
        # x=a+bi
        { pushSTACK(TheComplex(x)->c_real); # a retten
          R_cosh_sinh_R_R(TheComplex(x)->c_imag); # cosh(b), sinh(b) errechnen
          # Stackaufbau: a, cosh(b), sinh(b).
          # Da b nicht = Fixnum 0 ist, ist auch sinh(b) nicht = Fixnum 0.
          R_cos_sin_R_R(STACK_2); # cos(a), sin(a) errechnen, cos(a) /= Fixnum 0
          # Stackaufbau: a, cosh(b), sinh(b), cos(a), sin(a).
          STACK_0 = R_R_mal_R(STACK_0,STACK_3); # sin(a)*cosh(b)
         {var reg2 object erg = R_R_mal_R(STACK_1,STACK_2); # cos(a)*sinh(b), nicht Fixnum 0
          erg = R_R_complex_C(STACK_0,erg); skipSTACK(5); return erg;
        }}
    }

# N_cos_N(x) liefert (cos x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_cos_N (object x);
# Methode:
# x reell -> klar
# x = a+bi -> (complex (* (cos a) (cosh b)) (- (* (sin a) (sinh b))))
  local object N_cos_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_cos_R(x); }
        else
        # x=a+bi
        { pushSTACK(TheComplex(x)->c_real); # a retten
          R_cosh_sinh_R_R(TheComplex(x)->c_imag); # cosh(b), sinh(b) errechnen
          # Stackaufbau: a, cosh(b), sinh(b).
          R_cos_sin_R_R(STACK_2); # cos(a), sin(a) errechnen
          # Stackaufbau: a, cosh(b), sinh(b), cos(a), sin(a).
          STACK_0 = R_minus_R(R_R_mal_R(STACK_0,STACK_2)); # -sin(a)*sinh(b)
         {var reg2 object erg = R_R_mal_R(STACK_1,STACK_3); # cos(a)*cosh(b)
          erg = R_R_complex_N(erg,STACK_0); skipSTACK(5); return erg;
        }}
    }

# N_tan_N(x) liefert (tan x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_tan_N (object x);
# Methode:
# x reell -> (/ (sin x) (cos x))
# x = a+bi -> (/ (complex (* (sin a) (cosh b)) (* (cos a) (sinh b)))
#                (complex (* (cos a) (cosh b)) (- (* (sin a) (sinh b)))) )
  local object N_tan_N(x)
    var reg2 object x;
    { if (N_realp(x))
        { R_cos_sin_R_R(x);
          # Stackaufbau: cos(x), sin(x).
         {var reg1 object erg = R_R_durch_R(STACK_0,STACK_1);
          skipSTACK(2); return erg;
        }}
        else
        # x=a+bi
        { pushSTACK(TheComplex(x)->c_real); # a retten
          R_cosh_sinh_R_R(TheComplex(x)->c_imag); # cosh(b), sinh(b) errechnen
          # Stackaufbau: a, cosh(b), sinh(b).
          R_cos_sin_R_R(STACK_2); # cos(a), sin(a) errechnen
          # Stackaufbau: a, cosh(b), sinh(b), cos(a), sin(a).
         {var reg1 object temp;
          STACK_4 = R_R_mal_R(STACK_0,STACK_3); # sin(a)*cosh(b)
          temp = R_R_mal_R(STACK_1,STACK_2); # cos(a)*sinh(b) /= Fixnum 0
          STACK_4 = R_R_complex_C(STACK_4,temp); # Zähler
          # Stackaufbau: Zähler, cosh(b), sinh(b), cos(a), sin(a).
          STACK_3 = R_R_mal_R(STACK_1,STACK_3); # cos(a)*cosh(b)
          temp = R_minus_R(R_R_mal_R(STACK_0,STACK_2)); # -sin(a)*sinh(b)
          temp = R_R_complex_N(STACK_3,temp); # Nenner
          temp = N_N_durch_N(STACK_4,temp); # Zähler/Nenner
          skipSTACK(5); return temp;
        }}
    }

# N_cis_N(x) liefert (cis x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_cis_N (object x);
# Methode:
# x reell -> (complex (cos x) (sin x))
# x = a+bi -> (complex (* (exp (- b)) (cos a)) (* (exp (- b)) (sin a)))
  local object N_cis_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { R_cos_sin_R_R(x);
          # Stackaufbau: cos(x), sin(x).
         {var reg1 object erg = R_R_complex_N(STACK_1,STACK_0);
          skipSTACK(2); return erg;
        }}
        else
        # x=a+bi
        { pushSTACK(TheComplex(x)->c_imag); # b retten
          R_cos_sin_R_R(TheComplex(x)->c_real); # (cos a) und (sin a) errechnen
          # Stackaufbau: b, cos(a), sin(a).
          STACK_2 = x = R_exp_R(R_minus_R(STACK_2)); # (exp (- b))
          # Stackaufbau: exp(-b), cos(a), sin(a).
          STACK_0 = R_R_mal_R(x,STACK_0); # (* (exp (- b)) (sin a))
          x = R_R_mal_R(STACK_2,STACK_1); # (* (exp (- b)) (cos a))
          x = R_R_complex_N(x,STACK_0); # (complex ... ...)
          skipSTACK(3); return x;
        }
    }

# N_sinh_N(x) liefert (sinh x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_sinh_N (object x);
# Methode:
# x reell -> klar
# x = a+bi -> (complex (* (sinh a) (cos b)) (* (cosh a) (sin b)))
  local object N_sinh_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_sinh_R(x); }
        else
        # x=a+bi
        { pushSTACK(TheComplex(x)->c_real); # a retten
          R_cos_sin_R_R(TheComplex(x)->c_imag); # cos(b), sin(b) errechnen
          # Stackaufbau: a, cos(b), sin(b).
          # Da b nicht = Fixnum 0 ist, ist auch sin(b) nicht = Fixnum 0.
          R_cosh_sinh_R_R(STACK_2); # cosh(a), sinh(a) errechnen, cosh(a) /= Fixnum 0
          # Stackaufbau: a, cos(b), sin(b), cosh(a), sinh(a).
          STACK_0 = R_R_mal_R(STACK_0,STACK_3); # sinh(a)*cos(b)
         {var reg2 object erg = R_R_mal_R(STACK_1,STACK_2); # cosh(a)*sin(b), nicht Fixnum 0
          erg = R_R_complex_C(STACK_0,erg); skipSTACK(5); return erg;
        }}
    }

# N_cosh_N(x) liefert (cosh x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_cosh_N (object x);
# Methode:
# x reell -> klar
# x = a+bi -> (complex (* (cosh a) (cos b)) (* (sinh a) (sin b)))
  local object N_cosh_N(x)
    var reg1 object x;
    { if (N_realp(x))
        { return R_cosh_R(x); }
        else
        # x=a+bi
        { pushSTACK(TheComplex(x)->c_real); # a retten
          R_cos_sin_R_R(TheComplex(x)->c_imag); # cos(b), sin(b) errechnen
          # Stackaufbau: a, cos(b), sin(b).
          R_cosh_sinh_R_R(STACK_2); # cosh(a), sinh(a) errechnen
          # Stackaufbau: a, cos(b), sin(b), cosh(a), sinh(a).
          STACK_0 = R_R_mal_R(STACK_0,STACK_2); # sinh(a)*sin(b)
         {var reg2 object erg = R_R_mal_R(STACK_1,STACK_3); # cosh(a)*cos(b)
          erg = R_R_complex_N(erg,STACK_0); skipSTACK(5); return erg;
        }}
    }

# N_tanh_N(x) liefert (tanh x), wo x eine Zahl ist.
# kann GC auslösen
  local object N_tanh_N (object x);
# Methode:
# x reell -> (/ (sinh x) (cosh x))
# x = a+bi -> (/ (complex (* (sinh a) (cos b)) (* (cosh a) (sin b)))
#                (complex (* (cosh a) (cos b)) (* (sinh a) (sin b))) )
  local object N_tanh_N(x)
    var reg2 object x;
    { if (N_realp(x))
        { R_cosh_sinh_R_R(x);
          # Stackaufbau: cosh(x), sinh(x).
         {var reg1 object erg = R_R_durch_R(STACK_0,STACK_1);
          skipSTACK(2); return erg;
        }}
        else
        # x=a+bi
        { pushSTACK(TheComplex(x)->c_real); # a retten
          R_cos_sin_R_R(TheComplex(x)->c_imag); # cos(b), sin(b) errechnen
          # Stackaufbau: a, cos(b), sin(b).
          R_cosh_sinh_R_R(STACK_2); # cosh(a), sinh(a) errechnen
          # Stackaufbau: a, cos(b), sin(b), cosh(a), sinh(a).
         {var reg1 object temp;
          STACK_4 = R_R_mal_R(STACK_0,STACK_3); # sinh(a)*cos(b)
          temp = R_R_mal_R(STACK_1,STACK_2); # cosh(a)*sin(b) /= Fixnum 0
          STACK_4 = R_R_complex_C(STACK_4,temp); # Zähler
          # Stackaufbau: Zähler, cos(b), sin(b), cosh(a), sinh(a).
          STACK_3 = R_R_mal_R(STACK_1,STACK_3); # cosh(a)*cos(b)
          temp = R_R_mal_R(STACK_0,STACK_2); # sinh(a)*sin(b)
          temp = R_R_complex_N(STACK_3,temp); # Nenner
          temp = N_N_durch_N(STACK_4,temp); # Zähler/Nenner
          skipSTACK(5); return temp;
        }}
    }

# N_atanh_N(z) liefert den Artanh einer Zahl z.
# kann GC auslösen
  local object N_atanh_N (object z);
# Methode:
# Wert und Branch Cuts nach der Formel CLTL2, S. 315:
#   artanh(z) = (log(1+z)-log(1-z)) / 2
# Sei z=x+iy, Ergebnis u+iv.
# Falls x=0 und y=0: u=0, v=0.
# Falls x=0: u = 0, v = atan(X=1,Y=y).
# Falls y=0:
#   x rational -> x in Float umwandeln.
#   |x|<1/2: u = atanh(x), v = 0.
#   |x|>=1/2: (1+x)/(1-x) errechnen,
#             =0 -> Error,
#             >0 (also |x|<1) -> u = 1/2 log((1+x)/(1-x)), v = 0.
#             <0 (also |x|>1) -> u = 1/2 log(-(1+x)/(1-x)),
#                                v = (-pi/2 für x>1, pi/2 für x<-1).
# Sonst:
#   1+x und 1-x errechnen.
#   x und y in Floats umwandeln.
#   |4x| und 1+x^2+y^2 errechnen,
#   |4x| < 1+x^2+y^2 -> u = 1/2 atanh(2x/(1+x^2+y^2)),
#   |4x| >= 1+x^2+y^2 -> u = 1/4 ln ((1+x^2+y^2)+2x)/((1+x^2+y^2)-2x)
#                        oder besser (an der Singularität: |x|-1,|y| klein):
#                        u = 1/4 ln ((1+x)^2+y^2)/((1-x)^2+y^2).
#   v = 1/2 atan(X=(1-x)(1+x)-y^2,Y=2y) * (-1 falls Y=0.0 und X<0.0, 1 sonst)
# Ergebnis ist reell nur, wenn z reell.
# Real- und Imaginärteil des Ergebnisses sind Floats, außer wenn z reell oder
# rein imaginär ist.

# N_atan_N(z) liefert den Arctan einer Zahl z.
# kann GC auslösen
  local object N_atan_N (object z);
# Methode:
# Wert und Branch Cuts nach der Formel CLTL2, S. 307/312/313:
#   arctan(z) = (log(1+iz)-log(1-iz)) / 2i
# Sei z=x+iy, errechne u+iv = artanh(-y+ix) wie oben, Ergebnis v-iu.
# Real- und Imaginärteil des Ergebnisses sind Floats, außer wenn z reell oder
# rein imaginär ist.

# Hilfsfunktion für beide: u+iv := artanh(x+iy), u,v beide auf den Stack.
  local void R_R_atanh_R_R (object x, object y);
  local void R_R_atanh_R_R(x,y)
    var reg2 object x;
    var reg3 object y;
    { if (eq(x,Fixnum_0)) # x=0 -> u=0, v=atan(X=1,Y=y) (Fall y=0 ist inbegriffen)
        { pushSTACK(x); pushSTACK(R_R_atan_R(Fixnum_1,y)); return; }
      if (eq(y,Fixnum_0))
        { if (R_rationalp(x)) { x = RA_float_F(x); } # x in Float umwandeln
          # x Float
          if (R_zerop(x)) # x=0.0 -> x als Ergebnis
            { pushSTACK(x); pushSTACK(Fixnum_0); return; }
          if (F_exponent_L(x) < 0)
            # Exponent e<0, also |x|<1/2
            { pushSTACK(F_atanhx_F(x)); pushSTACK(Fixnum_0); return; }
          # e>=0, also |x|>=1/2
          pushSTACK(x);
          pushSTACK(R_R_minus_R(Fixnum_1,x)); # 1-x
          # Stackaufbau: x, 1-x.
          {var reg1 object temp;
           temp = R_R_plus_R(Fixnum_1,STACK_1); # 1+x
           temp = F_F_durch_F(temp,STACK_0); # (1+x)/(1-x)
           if (!R_minusp(temp))
             { STACK_1 = temp; STACK_0 = Fixnum_0; # Imaginärteil:=0
               if (R_zerop(temp)) { divide_0(); } # x = -1 -> Error
             }
             else
             # (1+x)/(1-x) < 0 -> Betrag nehmen, Imaginärteil berechnen:
             { STACK_1 = F_minus_F(temp);
               temp = F_I_scale_float_F(pi(),Fixnum_minus1); # (scale-float pi -1) = pi/2
               if (R_mminusp(STACK_0)) { temp = F_minus_F(temp); } # 1-x<0 -> dann -pi/2
               STACK_0 = temp;
             }
           # Stackaufbau: |(1+x)/(1-x)| (>0), Imaginärteil.
           STACK_1 = F_I_scale_float_F(R_ln_R(STACK_1),Fixnum_minus1); # ln bilden, durch 2
           return;
        } }
      pushSTACK(x); pushSTACK(y);
      pushSTACK(R_R_plus_R(Fixnum_1,x)); # 1+x
      pushSTACK(R_R_minus_R(Fixnum_1,STACK_2)); # 1-x
      # Stackaufbau: x, y, 1+x, 1-x.
      # x und y in Floats umwandeln:
      if (R_mrationalp(STACK_3))
        { if (R_mrationalp(STACK_2)) { STACK_2 = RA_float_F(STACK_2); }
          STACK_3 = RA_F_float_F(STACK_3,STACK_2);
        }
        else
        { if (R_mrationalp(STACK_2)) { STACK_2 = RA_F_float_F(STACK_2,STACK_3); } }
      {var reg1 object temp = STACK_2; pushSTACK(R_R_mal_R(temp,temp)); } # y^2
      {var reg1 object temp = STACK_4; temp = R_R_mal_R(temp,temp); # x^2
       pushSTACK(R_R_plus_R(Fixnum_1,R_R_plus_R(temp,STACK_0))); # 1+x^2+y^2
      }
      # Stackaufbau: x, y, 1+x, 1-x, y^2, 1+x^2+y^2.
      {var reg1 object temp = # |4x|
         F_abs_F(F_I_scale_float_F(STACK_5,fixnum(2)));
       if (F_F_comp(temp,STACK_0) < 0) # |4x| < 1+x^2+y^2 ?
         { temp = F_I_scale_float_F(STACK_5,Fixnum_1); # 2x
           temp = F_F_durch_F(temp,STACK_0); # 2x/(1+x^2+y^2)
           temp = F_atanhx_F(temp); # atanh davon
           STACK_5 = F_I_scale_float_F(temp,Fixnum_minus1); # .../2 =: u
         }
         else
         { temp = STACK_3; temp = R_R_mal_R(temp,temp); # (1+x)^2
           STACK_0 = R_R_plus_R(temp,STACK_1); # (1+x)^2+y^2, ein Float >=0
           temp = STACK_2; temp = R_R_mal_R(temp,temp); # (1-x)^2
           temp = R_R_plus_R(temp,STACK_1); # (1-x)^2+y^2, ein Float >=0
           temp = F_F_durch_F(STACK_0,temp); # ((1+x)^2+y^2)/((1-x)^2+y^2), ein Float >=0
           if (R_zerop(temp)) { divide_0(); } # sollte >0 sein
           temp = R_ln_R(temp); # ln davon, ein Float
           STACK_5 = F_I_scale_float_F(temp,sfixnum(-2)); # .../4 =: u
         }
       # Stackaufbau: u, y, 1+x, 1-x, y^2, -.
       temp = R_R_mal_R(STACK_3,STACK_2); # (1+x)(1-x)
       STACK_0 = R_R_minus_R(temp,STACK_1); # (1+x)(1-x)-y^2, ein Float
       temp = F_I_scale_float_F(STACK_4,Fixnum_1); # 2y, ein Float
       temp = R_R_atan_R(STACK_0,temp); # atan(X=(1-x)(1+x)-y^2,Y=2y), ein Float
       if (R_mminusp(STACK_0) && R_zerop(STACK_4)) # X<0.0 und Y=0.0 ?
         { temp = F_minus_F(temp); } # ja -> Vorzeichenwechsel
       STACK_4 = F_I_scale_float_F(temp,Fixnum_minus1); # .../2 =: v
      }
      # Stackaufbau: u, v, 1+x, 1-x, y^2, -.
      skipSTACK(4); return;
    }
  #
  local object N_atanh_N(z)
    var reg1 object z;
    { if (N_realp(z))
        { R_R_atanh_R_R(z,Fixnum_0); }
        else
        { R_R_atanh_R_R(TheComplex(z)->c_real,TheComplex(z)->c_imag); }
      # Stackaufbau: u, v.
      z = R_R_complex_N(STACK_1,STACK_0); skipSTACK(2); return z;
    }
  #
  local object N_atan_N(z)
    var reg1 object z;
    { # atanh(iz) errechnen:
      if (N_realp(z))
        { R_R_atanh_R_R(Fixnum_0,z); }
        else
        { pushSTACK(TheComplex(z)->c_real);
          z = R_minus_R(TheComplex(z)->c_imag);
          R_R_atanh_R_R(z,popSTACK());
        }
      # Stackaufbau: u, v.
      z = R_minus_R(STACK_1); z = R_R_complex_N(STACK_0,z); # z := v-iu
      skipSTACK(2); return z;
    }

# Um für zwei Zahlen u,v mit u^2-v^2=1 und u,v beide in Bild(sqrt)
# (d.h. Realteil>0.0 oder Realteil=0.0 und Imaginärteil>=0.0)
# log(u+v) zu berechnen:
#               log(u+v) = 2 artanh(v/(u+1))                            (!)
# (Beweis: 2 artanh(v/(u+1)) = log(1+(v/(u+1))) - log(1-(v/(u+1)))
#  = log((1+u+v)/(u+1)) - log((1+u-v)/(u+1)) == log((1+u+v)/(1+u-v))
#  = log(u+v) mod 2 pi i, und beider Imaginärteil ist > -pi und <= pi.)

# N_asinh_N(z) liefert den Arsinh einer Zahl z.
# kann GC auslösen
  local object N_asinh_N (object z);
# Methode:
# Wert und Branch Cuts nach der Formel CLTL2, S. 313:
#   arsinh(z) = log(z+sqrt(1+z^2))
# z=x+iy, Ergebnis u+iv.
# Falls x=0 und y=0: u=0, v=0.
# Falls x=0: arsinh(iy) = i arcsin(y).
#   y rational ->
#     Bei y=1: u = 0, v = pi/2.
#     Bei y=1/2: u = 0, v = pi/6.
#     Bei y=0: u = 0, v = 0.
#     Bei y=-1/2: u = 0, v = -pi/6.
#     Bei y=-1: u = 0, v = -pi/2.
#     Sonst y in Float umwandeln.
#   e := Exponent aus (decode-float y), d := (float-digits y)
#   Bei y=0.0 oder e<=-d/2 liefere u = 0, v = y
#     (denn bei e<=-d/2 ist y^2/3 < y^2/2 < 2^(-d)/2 = 2^(-d-1), also
#     1 <= asin(y)/y < 1+y^2/3 < 1+2^(-d-1) < 1+2^(-d),
#     also ist asin(y)/y, auf d Bits gerundet, gleich 1.0).
#   Berechne 1-y^2.
#   Bei y>1 liefere  u = ln(y+sqrt(y^2-1)), v = pi/2.
#   Bei y<-1 liefere  u = -ln(|y|+sqrt(|y|^2-1)), v = -pi/2.
#   Bei |y|<=1 liefere  u = 0, v = atan(X=sqrt(1-y^2),Y=y).
# Falls y=0:
#   x rational -> x in Float umwandeln.
#   |x|<1/2: u = atanh(x/sqrt(1+x^2)),
#   x>=1/2: u = ln(x+sqrt(1+x^2)),
#   x<=-1/2: u = -ln(-x+sqrt(1+x^2)).
#   v = 0.
# Sonst:
#   z in Bild(sqrt) -> log(sqrt(1+z^2)+z) = (!) = 2 artanh(z/(1+sqrt(1+z^2))).
#   z nicht in Bild(sqrt) ->
#     arsinh(z) = -arsinh(-z).
#     (Denn arsinh(z)+arsinh(-z) == log((z+sqrt(1+z^2))(-z+sqrt(1+z^2)))
#           = log((1+z^2)-z^2) = log(1) = 0 mod 2 pi i, und links ist
#      der Imaginärteil betragsmäßig <=pi.)
#     Also arsinh(z) = -arsinh(-z) = - 2 artanh(-z/(1+sqrt(1+z^2)))
#          = (wegen -artanh(-w) = artanh(w)) = 2 artanh(z/(1+sqrt(1+z^2))).
# Real- und Imaginärteil des Ergebnisses sind Floats, außer wenn z reell oder
# rein imaginär ist.

# N_asin_N(z) liefert den Arcsin einer Zahl z.
# kann GC auslösen
  local object N_asin_N (object z);
# Methode:
# Wert und Branch Cuts nach der Formel CLTL2, S. 311:
#   arcsin(z) = log(iz+sqrt(1-z^2))/i
# Sei z=x+iy, errechne u+iv = arsinh(-y+ix) wie oben, Ergebnis v-iu.
# Real- und Imaginärteil des Ergebnisses sind Floats, außer wenn z reell oder
# rein imaginär ist.

# Hilfsfunktion für beide: u+iv := arsinh(x+iy), u,v beide auf den Stack.
  local void R_R_asinh_R_R (object x, object y);
  local void R_R_asinh_R_R(x,y)
    var reg2 object x;
    var reg3 object y;
    { if (eq(x,Fixnum_0)) # x=0 ?
        { pushSTACK(x); pushSTACK(y);
          if (R_rationalp(y))
            # y rational
            { if (eq(y,Fixnum_0)) { return; } # x=0, y=0 -> u=0, v=0 bereits im Stack
              if (RA_integerp(y))
                # y Integer
                { if (eq(y,Fixnum_1)) # x=0, y=1 -> v = pi/2
                    { STACK_0 = F_I_scale_float_F(pi(),Fixnum_minus1); return; }
                  if (eq(y,Fixnum_minus1)) # x=0, y=-1 -> v = -pi/2
                    { STACK_0 = F_minus_F(F_I_scale_float_F(pi(),Fixnum_minus1)); return; }
                  STACK_0 = y = I_float_F(y); # y in Float umwandeln
                }
                else
                # y Ratio
                { if (eq(TheRatio(y)->rt_den,fixnum(2))) # Nenner = 2 ?
                    { var reg1 object temp = TheRatio(y)->rt_num; # Zähler
                      if (eq(temp,Fixnum_1)) # x=0, y=1/2 -> v = pi/6
                        { STACK_0 = R_R_durch_R(pi(),fixnum(6)); return; }
                      if (eq(temp,Fixnum_minus1)) # x=0, y=-1/2 -> v = -pi/6
                        { STACK_0 = F_minus_F(R_R_durch_R(pi(),fixnum(6))); return; }
                    }
                  STACK_0 = y = RA_float_F(y); # y in Float umwandeln
            }   }
          # y Float
          if (R_zerop(y) # y=0.0 -> arcsin(y) = y als Ergebnis
              || (F_exponent_L(y) <= (sintL)(-F_float_digits(y))>>1) # e <= -d/2 <==> e <= -ceiling(d/2) ?
             )
            { return; } # u=0, v=y bereits im Stack
          # Stackaufbau: 0, y.
          {var reg1 object temp = R_R_minus_R(Fixnum_1,F_F_mal_F(y,y)); # 1-y*y
           if (!R_minusp(temp))
             # 1-y*y>=0, also |y|<=1
             { temp = F_sqrt_F(temp); # sqrt(1-y*y)
               STACK_0 = R_R_atan_R(temp,STACK_0); # v = atan(X=sqrt(1-y*y),Y=y)
             }
             else
             # 1-y*y<0, also |y|>1
             { temp = F_sqrt_F(F_minus_F(temp)); # sqrt(y*y-1)
               y = STACK_0; # |y| zu temp addieren:
               if (R_minusp(y)) { temp = F_F_minus_F(temp,y); } else { temp = F_F_plus_F(temp,y); }
               # temp = sqrt(y^2-1)+|y|, ein Float >1
               STACK_1 = R_ln_R(temp); # ln(|y|+sqrt(y^2-1)), ein Float >0
               temp = F_I_scale_float_F(pi(),Fixnum_minus1); # (scale-float pi -1) = pi/2
               if (!R_mminusp(STACK_0)) # Vorzeichen von y
                 # y>1 -> v = pi/2
                 { STACK_0 = temp; }
                 else
                 # y<-1 -> v = -pi/2, u = -ln(...)
                 { STACK_0 = F_minus_F(temp); STACK_1 = F_minus_F(STACK_1); }
             }
           return;
        } }
      if (eq(y,Fixnum_0)) # y=0 ?
        { if (R_rationalp(x)) { x = RA_float_F(x); } # x in Float umwandeln
          # x Float
          pushSTACK(x); pushSTACK(Fixnum_0); # x retten, v = 0
          if (R_zerop(x)) { return; } # x=0.0 -> u=x, v=0.
          {var reg1 object temp = # sqrt(1+x^2)
             F_sqrt_F(R_R_plus_R(Fixnum_1,F_F_mal_F(x,x)));
           x = STACK_1;
           if (F_exponent_L(x) < 0) # Exponent e (von x/=0) <0 ?
             # |x|<1/2
             { STACK_1 = F_atanhx_F(F_F_durch_F(x,temp)); } # u = atanh(x/sqrt(1+x^2))
             else
             # |x|>=1/2
             { if (!R_minusp(x))
                 # x>=1
                 { STACK_1 = R_ln_R(F_F_plus_F(temp,x)); } # u = ln(x+sqrt(1+x^2))
                 else
                 # x<=-1
                 { STACK_1 = F_minus_F(R_ln_R(F_F_minus_F(temp,x))); } # u = -ln(-x+sqrt(1+x^2))
             }
           return;
        } }
     {var reg1 object z = R_R_complex_C(x,y); # z=x+iy
      pushSTACK(z);
      z = N_1_plus_N(N_sqrt_N(N_1_plus_N(N_N_mal_N(z,z)))); # 1+sqrt(1+z^2)
      z = N_N_durch_N(popSTACK(),z); # z/(1+sqrt(1+z^2))
      # Da z=x+iy weder reell noch rein imaginär ist, ist auch
      # w := z/(1+sqrt(1+z^2)) weder reell noch rein imaginär.
      # (Beweis: Sollte sqrt(1+z^2) rationalen Real- und Imaginärteil haben,
      # so auch z, also auch w, und die Formel z = 2w/(1-w^2) zeigt, daß dann
      # z reell oder rein imaginär sein müßte. Also hat sqrt(1+z^2) ein
      # Float als Real- oder Imaginärteil, das Betragsquadrat des Nenners
      # ist also ein Float, und da Real- und Imaginärteil von z /=0 sind,
      # sind Real- und Imaginärteil von w Floats.)
      # Daher hat dann atanh(...) Floats als Realteil u und Imaginärteil v.
      R_R_atanh_R_R(TheComplex(z)->c_real,TheComplex(z)->c_imag); # atanh nehmen
      # u und v mit 2 multiplizieren:
      STACK_1 = F_I_scale_float_F(STACK_1,Fixnum_1); # u:=2*u
      STACK_0 = F_I_scale_float_F(STACK_0,Fixnum_1); # v:=2*v
      return;
    }}
  #
  local object N_asinh_N(z)
    var reg1 object z;
    { if (N_realp(z))
        { R_R_asinh_R_R(z,Fixnum_0); }
        else
        { R_R_asinh_R_R(TheComplex(z)->c_real,TheComplex(z)->c_imag); }
      # Stackaufbau: u, v.
      z = R_R_complex_N(STACK_1,STACK_0); skipSTACK(2); return z;
    }
  #
  local object N_asin_N(z)
    var reg1 object z;
    { # asinh(iz) errechnen:
      if (N_realp(z))
        { R_R_asinh_R_R(Fixnum_0,z); }
        else
        { pushSTACK(TheComplex(z)->c_real);
          z = R_minus_R(TheComplex(z)->c_imag);
          R_R_asinh_R_R(z,popSTACK());
        }
      # Stackaufbau: u, v.
      z = R_minus_R(STACK_1); z = R_R_complex_N(STACK_0,z); # z := v-iu
      skipSTACK(2); return z;
    }

# N_acos_N(z) liefert den Arccos einer Zahl z.
# kann GC auslösen
  local object N_acos_N (object z);
# Methode:
# Wert und Branch Cuts nach der Formel CLTL2, S. 312:
#   arccos(z) = log(z+i*sqrt(1-z^2))/i = pi/2 - arcsin(z)
# Sei z=x+iy.
# Falls y=0:
#   Falls x rational:
#     Bei x=1: Ergebnis 0.
#     Bei x=1/2: Ergebnis pi/3.
#     Bei x=0: Ergebnis pi/2.
#     Bei x=-1/2: Ergebnis 2pi/3.
#     Bei x=-1: Ergebnis pi.
#     Sonst x in Float umwandeln.
#   Falls x>1: Ergebnis i ln(x+sqrt(x^2-1)).
# Sonst errechne u+iv = arsinh(-y+ix) wie oben, Ergebnis (pi/2-v)+iu.
  local object N_acos_N(z)
    var reg1 object z;
    { if (N_realp(z)) # y=0 ?
        { if (R_rationalp(z))
            # z rational
            { if (RA_integerp(z))
                # z Integer
                { if (eq(z,Fixnum_0)) # x=0 -> Ergebnis pi/2
                    { return F_I_scale_float_F(pi(),Fixnum_minus1); }
                  if (eq(z,Fixnum_1)) # x=1 -> Ergebnis 0
                    { return Fixnum_0; }
                  if (eq(z,Fixnum_minus1)) # x=-1 -> Ergebnis pi
                    { return pi(); }
                  z = I_float_F(z); # z in Float umwandeln
                }
                else
                # z Ratio
                { if (eq(TheRatio(z)->rt_den,fixnum(2))) # Nenner = 2 ?
                    { var reg2 object temp = TheRatio(z)->rt_num; # Zähler
                      if (eq(temp,Fixnum_1)) # x=1/2 -> Ergebnis pi/3
                        { return R_R_durch_R(pi(),fixnum(3)); }
                      if (eq(temp,Fixnum_minus1)) # x=1/2 -> Ergebnis 2pi/3
                        { return R_R_durch_R(F_I_scale_float_F(pi(),Fixnum_1),fixnum(3)); }
                    }
                  z = RA_float_F(z); # z in Float umwandeln
            }   }
          # z Float
          pushSTACK(z);
          if (R_R_comp(Fixnum_1,z)<0) # 1<z ?
            { var reg1 object temp = STACK_0; # z
              temp = R_R_minus_R(F_F_mal_F(temp,temp),Fixnum_1); # z^2-1, ein Float >=0
              temp = F_sqrt_F(temp); # sqrt(z^2-1), ein Float >=0
              temp = F_F_plus_F(popSTACK(),temp); # z+sqrt(z^2-1), ein Float >1
              temp = R_ln_R(temp); # ln(z+sqrt(z^2-1)), ein Float >=0
              return R_R_complex_C(Fixnum_0,temp);
            }
          R_R_asinh_R_R(Fixnum_0,popSTACK());
        }
        else
        { pushSTACK(TheComplex(z)->c_real);
          z = R_minus_R(TheComplex(z)->c_imag);
          R_R_asinh_R_R(z,popSTACK());
        }
      # Stackaufbau: u, v.
      # Bilde pi/2-v :
      z = STACK_0;
      z = (R_rationalp(z) ? pi() : pi_F_float_F(z)); # pi im Float-Format von v
      z = F_I_scale_float_F(z,Fixnum_minus1); # pi/2
      z = R_R_minus_R(z,STACK_0); # pi/2-v
      z = R_R_complex_N(z,STACK_1); # (pi/2-v)+iu
      skipSTACK(2); return z;
    }

# N_acosh_N(z) liefert den Arcosh einer Zahl z.
# kann GC auslösen
  local object N_acosh_N (object z);
# Methode:
# Wert und Branch Cuts nach der Formel CLTL2, S. 314:
#   arcosh(z) = 2 log(sqrt((z+1)/2)+sqrt((z-1)/2))
# Sei z=x+iy.
# Falls y=0:
#   Falls x rational:
#     Bei x=1: Ergebnis 0.
#     Bei x=1/2: Ergebnis pi/3 i.
#     Bei x=0: Ergebnis pi/2 i.
#     Bei x=-1/2: Ergebnis 2pi/3 i.
#     Bei x=-1: Ergebnis pi i.
#   Falls x<-1:
#     x in Float umwandeln, Ergebnis log(sqrt(x^2-1)-x) + i pi.
# Sonst nach (!) mit u = sqrt((z+1)/2) und v = sqrt((z-1)/2) :
# arcosh(z) = 4 artanh(v/(u+1)) = 4 artanh(sqrt((z-1)/2)/(1+sqrt((z+1)/2)))
  local object N_acosh_N(z)
    var reg1 object z;
    { if (N_realp(z)) # y=0 ?
        { if (R_rationalp(z))
            # z rational
            { if (RA_integerp(z))
                # z Integer
                { if (eq(z,Fixnum_0)) # x=0 -> Ergebnis pi/2 i
                    { return R_R_complex_C(Fixnum_0,F_I_scale_float_F(pi(),Fixnum_minus1)); }
                  if (eq(z,Fixnum_1)) # x=1 -> Ergebnis 0
                    { return Fixnum_0; }
                  if (eq(z,Fixnum_minus1)) # x=-1 -> Ergebnis pi i
                    { return R_R_complex_C(Fixnum_0,pi()); }
                }
                else
                # z Ratio
                { if (eq(TheRatio(z)->rt_den,fixnum(2))) # Nenner = 2 ?
                    { var reg2 object temp = TheRatio(z)->rt_num; # Zähler
                      if (eq(temp,Fixnum_1)) # x=1/2 -> Ergebnis pi/3 i
                        { return R_R_complex_C(Fixnum_0,R_R_durch_R(pi(),fixnum(3))); }
                      if (eq(temp,Fixnum_minus1)) # x=1/2 -> Ergebnis 2pi/3 i
                        { return R_R_complex_C(Fixnum_0,R_R_durch_R(F_I_scale_float_F(pi(),Fixnum_1),fixnum(3))); }
                    }
            }   }
          pushSTACK(z);
          if (R_R_comp(z,Fixnum_minus1)<0) # z<-1 ?
            { z = STACK_0;
              if (R_rationalp(z)) { STACK_0 = z = RA_float_F(z); }
              # z Float <= -1
              z = F_sqrt_F(R_R_minus_R(F_F_mal_F(z,z),Fixnum_1)); # sqrt(z^2-1), ein Float >=0
              STACK_0 = R_ln_R(F_F_minus_F(z,STACK_0)); # log(sqrt(z^2-1)-z), ein Float >=0
              z = pi(); # und Imaginärteil pi
              return R_R_complex_C(popSTACK(),z);
            }
          z = popSTACK();
        }
      pushSTACK(z);
     {var reg2 object temp;
      temp = N_sqrt_N(N_N_durch_N(N_minus1_plus_N(z),fixnum(2))); # Zähler
      z = STACK_0; STACK_0 = temp;
      temp = N_1_plus_N(N_sqrt_N(N_N_durch_N(N_1_plus_N(z),fixnum(2)))); # Nenner
      return N_N_mal_N(fixnum(4),N_atanh_N(N_N_durch_N(popSTACK(),temp)));
    }}

