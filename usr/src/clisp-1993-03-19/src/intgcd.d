# ggT und kgV von Integers

# Liefert den ggT zweier Integers.
# I_I_gcd_I(a,b)
# > a,b: zwei Integers
# < ergebnis: (gcd a b), ein Integer >=0
# kann GC auslˆsen
  local object I_I_gcd_I (object a, object b);
  #define GCD_ALGO 3  # 1: bin‰r, 2: Schulmethode, 3: Lehmer

# Liefert den ggT zweier Integers samt Beifaktoren.
# I_I_xgcd_I_I_I(a,b)
# > a,b: zwei Integers
# < STACK_2=u, STACK_1=v, STACK_0=g: Integers mit u*a+v*b = g >= 0
# erniedrigt STACK um 3
# kann GC auslˆsen
  local void I_I_xgcd_I_I_I (object a, object b);
  #define XGCD_ALGO 3  # 2: Schulmethode, 3: Lehmer
# Im Fall A/=0, B/=0 gen¸gt das Ergebnis (g,u,v) den Ungleichungen:
#   Falls |A| = |B| : g = |A|, u = (signum A), v = 0.
#   Falls |B| | |A|, |B| < |A| : g = |B|, u = 0, v = (signum B).
#   Falls |A| | |B|, |A| < |B| : g = |A|, u = (signum A), v = 0.
#   Sonst: |u| <= |B| / 2*g, |v| <= |A| / 2*g.
#   In jedem Fall |u| <= |B|/g, |v| < |A|/g.
# (Beweis: Im Prinzip macht man ja mehrere Euklid-Schritte auf einmal. Im
# letzten Fall - oBdA |A| > |B| - braucht man mindestens zwei Euklid-Schritte,
# also gilt im Euklid-Tableau
#                 i         |A|            |B|         Erg.
#                --------------------------------------------
#                 0          1              0          |A|
#                 1          0              1          |B|
#                ...        ...            ...         ...
#                n-1  -(-1)^n*x[n-1]  (-1)^n*y[n-1]   z[n-1]
#                 n    (-1)^n*x[n]    -(-1)^n*y[n]     z[n]
#                n+1  -(-1)^n*x[n+1]  (-1)^n*y[n+1]   z[n+1] = 0
#                --------------------------------------------
#                       g = z[n], |u|=x[n], |v|=y[n]
# n>=2, z[0] > ... > z[n-1] > z[n] = g, g | z[n-1], also z[n-1] >= 2*g.
# Da aber mit  (-1)^i*x[i]*|A| - (-1)^i*y[i]*|B| = z[i]  f¸r i=0..n+1
# und            x[i]*y[i+1] - x[i+1]*y[i] = (-1)^i  f¸r i=0..n,
#                x[i]*z[i+1] - x[i+1]*z[i] = (-1)^i*|B|  f¸r i=0..n,
#                y[i]*z[i+1] - y[i+1]*z[i] = -(-1)^i*|A|  f¸r i=0..n
# auch |A| = y[i+1]*z[i] + y[i]*z[i+1], |B| = x[i+1]*z[i] + x[i]*z[i+1]
# f¸r i=0..n (Cramersche Regel), folgt
# |A| = y[n]*z[n-1] + y[n-1]*z[n] >= y[n]*2*g + 0 = |v|*2*g,
# |B| = x[n]*z[n-1] + x[n-1]*z[n] >= x[n]*2*g + 0 = |u|*2*g.)

# Liefert den ggT zweier Langworte.
# UL_UL_gcd_UL(a,b)
# > uintL a,b: zwei Langworte >0
# < ergebnis: (gcd a b), ein Langwort >0
#if GCD_ALGO==2 # nur dann brauchen wir's
  local uintL UL_UL_gcd_UL (uintL a, uintL b);
# bin‰re Methode:
# (gcd a b) :==
#   (prog ((j 0))
#     1 {a,b >0}
#       (when (oddp a) (go 4))
#       (when (oddp b) (go 3))
#       (incf j) (setq a (/ a 2)) (setq b (/ b 2))
#       (go 1)
#     2 {a,b >0, beide ungerade}
#       (cond ((> a b) (setq a (- a b)) (go 3))
#             ((= a b) (go 5))
#             ((< a b) (setq b (- b a)) (go 4))
#       )
#     3 {a,b >0, a gerade, b ungerade}
#       (repeat (setq a (/ a 2)) (until (oddp a)))
#       (go 2)
#     4 {a,b >0, a ungerade, b gerade}
#       (repeat (setq b (/ b 2)) (until (oddp b)))
#       (go 2)
#     5 {a=b>0}
#       (return (ash a j))
#   )
# Statt j zu erhˆhen und immer Bit 0 von a und b abfragen,
# fragen wir stattdessen immer Bit j von a und b ab; Bits j-1..0 sind =0.
  local uintL UL_UL_gcd_UL(a,b)
    var reg1 uintL a;
    var reg2 uintL b;
    {
      #ifdef DUMMER_GGT # so macht's ein Mathematiker:
      var reg3 uintL bit_j = bit(0);
      loop
        { # a,b >0
          if (!((a & bit_j) ==0)) goto odd_even;
          if (!((b & bit_j) ==0)) goto even_odd;
          # a,b >0 gerade
          bit_j = bit_j<<1;
        }
      #else # Trick von B. Degel:
      var reg3 uintL bit_j = (a | b); # endet mit einer 1 und j Nullen
      bit_j = bit_j ^ (bit_j - 1); # Maske = bit(j) | bit(j-1) | ... | bit(0)
      if (!((a & bit_j) ==0)) goto odd_even;
      if (!((b & bit_j) ==0)) goto even_odd;
      #endif
      loop
        { # a,b >0, beide ungerade
          # Vergleiche a und b:
          if (a == b) break; # a=b>0 -> fertig
          if (a > b) # a>b ?
            { a = a-b;
              even_odd: # a,b >0, a gerade, b ungerade
              do { a = a>>1; } while ((a & bit_j) ==0);
            }
            else # a<b
            { b = b-a;
              odd_even: # a,b >0, a ungerade, b gerade
              do { b = b>>1; } while ((b & bit_j) ==0);
            }
        }
      # a=b>0
      return a;
    }
#endif

# bin‰re Methode:
# (gcd a b) :==
# b=0 --> (abs a)
# a=0 --> (abs b)
# sonst:
#   (abs a) und (abs b) in zwei Buffer packen, als Unsigned Digit Sequences.
#   [Schreibe oBdA wieder a,b]
#   (prog ((j 0))
#     1 {a,b >0}
#       (if (evenp a)
#         (if (evenp b)
#           (progn (incf j) (setq a (/ a 2)) (setq b (/ b 2)) (go 1))
#           (go 4)
#         )
#         (while (evenp b) (setq b (/ b 2)))
#       )
#     2 {a,b >0, beide ungerade}
#       (cond ((> a b))
#             ((= a b) (go 5))
#             ((< a b) (rotatef a b))
#       )
#     3 {a,b >0, beide ungerade, a>b}
#       (setq a (- a b))
#     4 {a,b >0, a gerade, b ungerade}
#       (repeat (setq a (/ a 2)) (until (oddp a)))
#       (go 2)
#     5 {a=b>0}
#       (return (ash a j))
#   )
# weil es oft auftritt (insbesondere bei GCD's mehrerer Zahlen):
# a=1 oder b=1 --> 1
#if GCD_ALGO==1
  local object I_I_gcd_I(a,b)
    var reg8 object a;
    var reg9 object b;
    { if (eq(a,Fixnum_1)) { return a; } # a=1 -> 1
      if (eq(b,Fixnum_1)) { return b; } # b=1 -> 1
      if (eq(b,Fixnum_0)) { return I_abs_I(a); } # b=0 -> (abs a)
      if (eq(a,Fixnum_0)) { return I_abs_I(b); } # a=0 -> (abs b)
     {SAVE_NUM_STACK # num_stack retten
      var reg1 uintD* a_MSDptr;
      var reg3 uintC a_len;
      var reg5 uintD* a_LSDptr;
      var reg2 uintD* b_MSDptr;
      var reg4 uintC b_len;
      var reg6 uintD* b_LSDptr;
      # Macro: erzeugt die NUDS zu (abs x), erniedrigt num_stack
      #define I_abs_to_NUDS(x)  \
        { I_to_NDS_1(x, x##_MSDptr = , x##_len = , x##_LSDptr = ); # (nichtleere) NDS holen \
          if ((sintD)x##_MSDptr[0] < 0) # falls <0, negieren:                               \
            { neg_loop_down(x##_LSDptr,x##_len); }                                          \
          if (x##_MSDptr[0] == 0) # normalisieren (max. 1 Nulldigit entfernen)              \
            { x##_MSDptr++; x##_len--; }                                                    \
        }
      I_abs_to_NUDS(a); # (abs a) als NUDS erzeugen
      I_abs_to_NUDS(b); # (abs b) als NUDS erzeugen
      # Jetzt ist a = a_MSDptr/a_len/a_LSDptr, b = b_MSDptr/b_len/b_LSDptr,
      # beides NUDS, und a_len>0, b_len>0.
      # Macro: Halbiere x.
      #define halb(x)  \
        { shift1right_loop_up(x##_MSDptr,x##_len,0); # um 1 Bit rechts schieben \
          if (x##_MSDptr[0] == 0) { x##_MSDptr++; x##_len--; } # normalisieren  \
        }
      # Macro: Ob x gerade ist.
      #define evenp(x)  \
        ((x##_LSDptr[-1] & bit(0)) ==0)
      { var reg7 uintL j = 0;
        label_1: # a,b >0
          if (evenp(a))
            { if (evenp(b))
                { j++; halb(a); halb(b); goto label_1; }
                else
                goto label_4;
            }
          while (evenp(b)) { halb(b); }
        label_2: # a,b >0, beide ungerade
          # Vergleiche a und b:
          if (a_len > b_len) goto label_3; # a>b ?
          if (a_len == b_len)
            { var reg1 signean vergleich = compare_loop_up(a_MSDptr,b_MSDptr,a_len);
              if (vergleich > 0) goto label_3; # a>b ?
              if (vergleich == 0) goto label_5; # a=b ?
            }
          # a<b -> a,b vertauschen:
          swap(reg1 uintD*, a_MSDptr,b_MSDptr);
          swap(reg1 uintC, a_len,b_len);
          swap(reg1 uintD*, a_LSDptr,b_LSDptr);
        label_3: # a,b >0, beide ungerade, a>b
          # subtrahiere a := a - b
          if (!( subfrom_loop_down(b_LSDptr,a_LSDptr,b_len) ==0))
            { dec_loop_down(&a_LSDptr[-(uintL)b_len],a_len-b_len); }
          while (a_MSDptr[0] == 0) { a_MSDptr++; a_len--; } # normalisieren
        label_4: # a,b >0, a gerade, b ungerade
          do { halb(a); } while (evenp(a));
          goto label_2;
        label_5: # a=b>0
          # a zu einer NDS machen:
          RESTORE_NUM_STACK # num_stack (vorzeitig) zur¸ck
          a = NUDS_to_I(a_MSDptr,a_len); # ggT der ungeraden Anteile als Integer
          return I_I_ash_I(a,fixnum(j)); # (ash a j) als Ergebnis
      }
      #undef evenp
      #undef halb
      #undef I_abs_to_NUDS
    }}
#endif

# bin‰re Methode:
# (xgcd A B) :==
# B=0 --> g = (abs A), u = (signum A), v = 0
# A=0 --> g = (abs B), u = 0, v = (signum B)
# sonst:
# a := (abs A) und b := (abs B) in zwei Buffer packen,
# als Unsigned Digit Sequences.
# (xgcd a b) :==
#   (prog ((j 0) (ua 1) (va 0) (ub 0) (vb 1))
#     {Stets |A|*ua-|B|*va=a*2^j, -|A|*ub+|B|*vb=b*2^j,
#            ua>0, va>=0, ub>=0, vb>0.}
#     1 {a,b >0}
#       (when (oddp a) (setq Aj a Bj b) (go 4))
#       (when (oddp b) (setq Aj a Bj b) (go 3))
#       (incf j) (setq a (/ a 2)) (setq b (/ b 2))
#       (go 1)
#     {Ab hier |A| = Aj*2^j, |B| = Bj*2^j, Aj oder Bj ungerade,
#              Aj*ua-Bj*va=a, -Aj*ub+Bj*vb=b, a oder b ungerade,
#              0<ua<=Bj, 0<=va<Aj, 0<=ub<Bj, 0<vb<=Aj. (wieso??)}
#     2 {a,b >0, beide ungerade}
#       (cond ((> a b) (setq a (- a b) ua (+ ua ub) va (+ va vb)) (go 3))
#             ((= a b) (go 5))
#             ((< a b) (setq b (- b a) ub (+ ub ua) vb (+ vb va)) (go 4))
#       )
#     3 {a,b >0, a gerade, b ungerade}
#       (repeat (when (or (oddp ua) (oddp va))
#                 { Falls ua gerade, muﬂ (da Bj*va==0 mod 2) Bj==0, Aj==1 sein.
#                   Falls va gerade, muﬂ (da Aj*ua==0 mod 2) Aj==0, Bj==1 sein.
#                   Falls ua,va beide ungerade, m¸ssen (da Aj*1-Bj*1==0 mod 2)
#                                               Aj und Bj beide ungerade sein.}
#                 (setq ua (+ ua Bj) va (+ va Aj))
#               )
#               {ua,va beide gerade}
#               (setq a (/ a 2) ua (/ ua 2) va (/ va 2))
#               (until (oddp a))
#       )
#       (go 2)
#     4 {a,b >0, a ungerade, b gerade}
#       (repeat (when (or (oddp ub) (oddp vb))
#                 { Falls ub gerade, muﬂ (da Bj*vb==0 mod 2) Bj==0, Aj==1 sein.
#                   Falls vb gerade, muﬂ (da Aj*ub==0 mod 2) Aj==0, Bj==1 sein.
#                   Falls ub,vb beide ungerade, m¸ssen (da -Aj*1+Bj*1==0 mod 2)
#                                               Aj und Bj beide ungerade sein.}
#                 (setq ub (+ ub Bj) vb (+ vb Aj))
#               )
#               {ub,vb beide gerade}
#               (setq b (/ b 2) ub (/ ub 2) vb (/ vb 2))
#               (until (oddp b))
#       )
#       (go 2)
#     5 {a=b>0}
#       (return (values (ash a j) (* (signum A) ua) (- (* (signum B) va))))
#       (return (values (ash a j) (- (* (signum A) ub)) (* (signum B) vb)))
#   )
#if XGCD_ALGO==1
??
#endif

# Schulmethode:
#   (gcd a b) :==
#   [a:=(abs a), b:=(abs b), while b>0 do (a,b) := (b,(mod a b)), -> a]
# verbessert:
# a=0 -> (abs b)
# b=0 -> (abs a)
# a=1 -> 1
# b=1 -> 1
# a:=(abs a), b:=(abs b)
# Falls a=b: return a; falls a<b: vertausche a und b.
# (*) {Hier a>b>0}
# Falls b=1, return 1. {spart eine Division durch 1}
# Sonst dividieren (divide a b), a:=b, b:=Rest.
#       Falls b=0, return a, sonst goto (*).
#if GCD_ALGO==2
  local object I_I_gcd_I(a,b)
    var reg1 object a;
    var reg2 object b;
    { if (eq(a,Fixnum_1)) { return a; } # a=1 -> 1
      if (eq(b,Fixnum_1)) { return b; } # b=1 -> 1
      if (eq(b,Fixnum_0)) { return I_abs_I(a); } # b=0 -> (abs a)
      if (eq(a,Fixnum_0)) { return I_abs_I(b); } # a=0 -> (abs b)
      # Betr‰ge nehmen:
      pushSTACK(b); pushSTACK(I_abs_I(a)); STACK_1 = I_abs_I(STACK_1);
      # Stackaufbau: (abs b), (abs a).
      a = popSTACK(); b = STACK_0;
      # ab jetzt: b in STACK_0.
      if (I_fixnump(a) && I_fixnump(b)) # ggT zweier Fixnums >0
        { # bleibt Fixnum, da (gcd a b) <= (min a b)
          skipSTACK(1);
          return fixnum(UL_UL_gcd_UL(posfixnum_to_L(a),posfixnum_to_L(b)));
        }
      { var reg3 signean vergleich = I_I_comp(a,b);
        if (vergleich == 0) { skipSTACK(1); return a; } # a=b -> fertig
        if (vergleich < 0) { STACK_0 = a; a = b; } # a<b -> a,b vertauschen
      }
      loop # Hier a > STACK_0 > 0
        { if (eq(STACK_0,Fixnum_1)) { return popSTACK(); } # b=1 -> Ergebnis 1
          I_I_divide_I_I(a,STACK_0); b = STACK_0; skipSTACK(2); # b := Rest der Division a / STACK_0
          a = STACK_0; # neues a = altes b
          if (eq(b,Fixnum_0)) { skipSTACK(1); return a; }
          STACK_0 = b;
        }
    }
#endif

# Schulmethode:
#   (gcd A B) :==
#   [a:=(abs A), b:=(abs B), while b>0 do (a,b) := (b,(mod a b)), -> a]
# verbessert:
# A=1 -> return g=1, (u,v)=(1,0)
# B=1 -> return g=1, (u,v)=(0,1)
# a:=(abs A), ua:=(signum A), va:=0
# b:=(abs B), ub:=0, vb:=(signum B)
# A=0 -> return g=b, (u,v) = (ub,vb)
# B=0 -> return g=a, (u,v) = (ua,va)
# {Stets ua*A+va*B=a, ub*A+vb*B=b, ua*vb-ub*va = +/- 1.}
# Falls a=b: return a,ua,va;
# falls a<b: vertausche a und b, ua und ub, va und vb.
# (*) {Hier a>b>0}
# Falls b=1, return 1,ub,vb. {spart eine Division durch 1}
# Sonst dividieren (divide a b) -> q,r.
#       Falls r=0, return b,ub,vb.
#       a:=b, b := Rest r = a-q*b, (ua,va,ub,vb) := (ub,vb,ua-q*ub,va-q*vb).
#       goto (*).
#if XGCD_ALGO==2
  local void I_I_xgcd_I_I_I(a,b)
    var reg3 object a;
    var reg2 object b;
    { if (eq(a,Fixnum_1)) # a=1 -> g=1, (u,v)=(1,0)
        { pushSTACK(Fixnum_1); pushSTACK(Fixnum_0);
          pushSTACK(a); return;
        }
      if (eq(b,Fixnum_1)) # b=1 -> g=1, (u,v)=(0,1)
        { pushSTACK(Fixnum_0); pushSTACK(Fixnum_1);
          pushSTACK(b); return;
        }
      # Vorzeichen nehmen:
      pushSTACK(R_minusp(a) ? Fixnum_minus1 : Fixnum_1); # ua := +/- 1
      pushSTACK(Fixnum_0); # va := 0
      pushSTACK(Fixnum_0); # ub := 0
      pushSTACK(R_minusp(b) ? Fixnum_minus1 : Fixnum_1); # vb := +/- 1
      # Betr‰ge nehmen:
      pushSTACK(b); pushSTACK(I_abs_I(a)); STACK_1 = I_abs_I(STACK_1);
      # Stackaufbau: ua, va, ub, vb, b, a.
      a = STACK_0; b = STACK_1;
      if (eq(b,Fixnum_0)) # b=0 -> g=a, (u,v) = (ua,va)
        { skipSTACK(4); pushSTACK(a); return; }
      if (eq(a,Fixnum_0)) # a=0 -> g=b, (u,v) = (ub,vb)
        { STACK_5 = STACK_3; STACK_4 = STACK_2; STACK_3 = STACK_1;
          skipSTACK(3); return;
        }
      { var reg4 signean vergleich = I_I_comp(a,b);
        if (vergleich == 0) # a=b -> fertig
          { skipSTACK(4); pushSTACK(a); return; }
        if (vergleich < 0) # a<b -> a,b vertauschen
          { STACK_0 = b; b = STACK_1 = a; a = STACK_0;
            swap(STACK_5,STACK_3); swap(STACK_4,STACK_2);
      }   }
      loop # Hier a>b>0
        { if (eq(b,Fixnum_1)) # b=1 -> g=b, (u,v) = (ub,vb)
            { STACK_5 = STACK_3; STACK_4 = STACK_2; STACK_3 = b;
              skipSTACK(3); return;
            }
          I_I_divide_I_I(a,b); # Division a / b
          # Stackaufbau: ..., q, r.
          if (eq(STACK_0,Fixnum_0)) # r=0 -> fertig
            { STACK_5 = STACK_3; STACK_4 = STACK_2; STACK_3 = STACK_1;
              skipSTACK(3); return;
            }
          {var reg1 object x = I_I_mal_I(STACK_1,STACK_(3+2)); # q*ub
           x = I_I_minus_I(STACK_(5+2),x); # ua-q*ub
           STACK_(5+2) = STACK_(3+2); STACK_(3+2) = x; # ua := ub, ub := x
          }
          {var reg1 object x = I_I_mal_I(STACK_1,STACK_(2+2)); # q*vb
           x = I_I_minus_I(STACK_(4+2),x); # va-q*vb
           STACK_(4+2) = STACK_(2+2); STACK_(2+2) = x; # va := vb, vb := x
          }
          STACK_(0+2) = a = STACK_(1+2); # a := altes b
          STACK_(1+2) = b = STACK_0; # b := r
          skipSTACK(2);
        }
    }
#endif

# Lehmer-Methode:
# vgl. [ D. E. Knuth: The Art of Computer Programming, Vol. 2: Seminumerical
#        Algorithms, Sect. 4.5.2., Algorithm L ]
# und [ Collins, Loos: SAC-2, Algorithms IGCD, DPCC ].
# (gcd a b) :==
# a=0 -> (abs b)
# b=0 -> (abs a)
# a=1 -> 1
# b=1 -> 1
# a:=(abs a), b:=(abs b)
# (*) {Hier a,b>0}
# Falls a=b: return a; falls a<b: vertausche a und b.
# {Hier a>b>0}
# Falls (- (integer-length a) (integer-length b)) >= intDsize/2,
#   lohnt sich eine Division: (a,b) := (b , a mod b). Falls b=0: return a.
# Falls dagegen 0 <= (- (integer-length a) (integer-length b)) < intDsize/2,
#   seien a' die f¸hrenden intDsize Bits von a
#   (2^(intDsize-1) <= a' < 2^intDsize) und b' die entsprechenden Bits von b
#   (2^(intDsize/2) <= b' <= a' < 2^intDsize).
#   Rechne den Euklid-Algorithmus mit Beifaktoren f¸r ALLE Zahlen (a,b) aus,
#   die mit a' bzw. b' anfangen; das liefert x1,y1,x2,y2, so daﬂ
#   ggT(a,b) = ggT(x1*a-y1*b,-x2*a+y2*b) und x1*a-y1*b>=0,-x2*a+y2*b>=0.
#   Genauer: Mit offensichtlicher Skalierung betrachten wir
#            a als beliebiges Element des Intervalls [a',a'+1) und
#            b als beliebiges Element des Intervalls [b',b'+1) und
#            f¸hren den Euklid-Algorithmus schrittweise durch:
#            (x1,y1,z1) := (1,0,a'), (x2,y2,z2) := (0,1,b'),
#            Schleife:
#            {Hier x1*a'-y1*b'=z1, x1*a-y1*b in [z1-y1,z1+x1), z1-y1>=0, z1>0,
#             und -x2*a'+y2*b'=z2, -x2*a+y2*b in [z2-x2,z2+y2), z2-x2>=0, z2>0,
#             x1*y2-x2*y1=1, x1*z2+x2*z1=b', y1*z2+y2*z1=a'.}
#            Falls z1-y1>=z2+y2:
#              (x1,y1,z1) := (x1+x2,y1+y2,z1-z2), goto Schleife.
#            Falls z2-x2>=z1+x1:
#              (x2,y2,z2) := (x2+x1,y2+y1,z2-z1), goto Schleife.
#            Sonst muﬂ man abbrechen.
#            {Zu den Schleifeninvarianten:
#             1. Die Gleichungen x1*a'-y1*b'=z1, -x2*a'+y2*b'=z2,
#                x1*y2-x2*y1=1, x1*z2+x2*z1=b', y1*z2+y2*z1=a' mit Induktion.
#             2. Die Ungleichungen x1>0, y1>=0, x2>=0, y2>0 mit Induktion.
#             3. Die Ungleichungen z1>=0, z2>=0 nach Fallauswahl.
#             4. Die Ungleichung x1+x2>0 aus x1*z2+x2*z1=b'>0,
#                die Ungleichung y1+y2>0 aus y1*z2+y2*z1=a'>0.
#             5. Die Ungleichung z1>0 wegen Fallauswahl und y1+y2>0,
#                Die Ungleichung z2>0 wegen Fallauswahl und x1+x2>0.
#             6. Die Ungleichungen z1-y1>=0, z2-x2>=0 wegen Fallauswahl.
#             7. Die Ungleichung max(z1,z2) <= a' mit Induktion.
#             8. Die Ungleichung x1+x2 <= x1*z2+x2*z1 = b',
#                die Ungleichung y1+y2 <= y1*z2+y2*z1 = a'.
#             Damit bleiben alle Grˆﬂen im Intervall [0,beta), kein ‹berlauf.
#             9. Die Ungleichungen z1+x1<=beta, z2+y2<=beta mit Induktion.
#             10. x1*a-y1*b in (z1-y1,z1+x1) (bzw. [z1,z1+x1) bei y1=0),
#                -x2*a+y2*b in (z2-x2,z2+y2) (bzw. [z2,z2+y2) bei x2=0),
#                da a in a'+[0,1) und b in b'+[0,1).
#                Jedenfalls 0 < x1*a-y1*b < z1+x1 <= x2*z1+x1*z2 = b' falls x2>0,
#                und        0 < -x2*a+y2*b < z2+y2 <= y1*z2+y2*z1 = a' falls y1>0.}
#            Man kann nat¸rlich auch mehrere Subtraktionsschritte auf einmal
#            durchf¸hren:
#            Falls q := floor((z1-y1)/(z2+y2)) > 0 :
#              (x1,y1,z1) := (x1+q*x2,y1+q*y2,z1-q*z2), goto Schleife.
#            Falls q := floor((z2-x2)/(z1+x1)) > 0 :
#              (x2,y2,z2) := (x2+q*x1,y2+q*y1,z2-q*z1), goto Schleife.
#            {Am Schluﬂ gilt -(x1+x2) < z1-z2 < y1+y2 und daher
#             z2-x2 <= b'/(x1+x2) < z1+x1, z1-y1 <= a'/(y1+y2) < z2+y2,
#             und - unter Ber¸cksichtigung von x1*y2-x2*y1=1 -
#             z1-y1 <= b'/(x1+x2) < z2+y2, z2-x2 <= a'/(y1+y2) < z1+x1,
#             also  max(z1-y1,z2-x2) <= min(b'/(x1+x2),a'/(y1+y2))
#                          <= max(b'/(x1+x2),a'/(y1+y2)) < min(z1+x1,z2+y2).}
#   Im Fall y1=x2=0 => x1=y2=1 (der nur bei a'=z1=z2=b' eintreten kann)
#     ersetze (a,b) := (a-b,b). {Beide >0, da a>b>0 war.}
#   Der Fall y1=0,x2>0 => x1=y2=1 => a' = z1 < z2+x2*z1 = b'
#     kann nicht eintreten.
#   Im Fall x2=0,y1>0 => x1=y2=1 ersetze (a,b) := (a-y1*b,b).
#     {Das ist OK, da 0 <= z1-y1 = a'-y1*(b'+1) < a-y1*b < a.}
#   Sonst (y1>0,x2>0) ersetze (a,b) := (x1*a-y1*b,-x2*a+y2*b).
#     {Das ist OK, da 0 <= z1-y1 = x1*a'-y1*(b'+1) < x1*a-y1*b
#                  und 0 <= z2-x2 = -x2*(a'+1)+y2*b' < -x2*a+y2*b
#      und x1*a-y1*b < x1*(a'+1)-y1*b' = z1+x1 <= x2*z1+x1*z2 = b' <= b
#      und -x2*a+y2*b < -x2*a'+y2*(b'+1) = z2+y2 <= y1*z2+y2*z1 = a' <= a.}
# goto (*).
#if (GCD_ALGO==3) || (XGCD_ALGO==3)
  # Teilfunktion f¸r die Durchf¸hrung des Euklid-Algorithmus auf
  # den f¸hrenden Ziffern a' und b':
  # partial_gcd(a',b',&erg); mit a'>b'
  # liefert in erg: x1,y1,x2,y2 mit den oben angegebenen Invarianten.
  typedef struct { uintD x1,y1,x2,y2; } partial_gcd_result;
  local void partial_gcd (uintD z1, uintD z2, partial_gcd_result* erg);
  local void partial_gcd(z1,z2,erg) # z1:=a', z2:=b'
    var reg1 uintD z1;
    var reg2 uintD z2;
    var reg7 partial_gcd_result* erg;
    { var reg5 uintD x1 = 1;
      var reg3 uintD y1 = 0;
      var reg6 uintD x2 = 0;
      var reg4 uintD y2 = 1;
      subtract_from_1: # Hier ist z1-y1>=z2+y2.
        # Bestimme q := floor((z1-y1)/(z2+y2)) >= 1 :
        { var reg2 uintD zaehler = z1-y1;
          var reg1 uintD nenner = z2+y2; # z2+y2 <= z1-y1 < beta !
          if (floor(zaehler,8) >= nenner) # zaehler >= 8*nenner ?
            # ja -> Dividieren lohnt sich wohl
            { var reg3 uintD q = floorD(zaehler,nenner);
              x1 += muluD_unchecked(q,x2); # x1 := x1+q*x2
              y1 += muluD_unchecked(q,y2); # y1 := y1+q*y2
              z1 -= muluD_unchecked(q,z2); # z1 := z1-q*z2
            }
            else
            # nein -> ein paarmal subtrahieren ist wohl schneller
            do { x1 += x2; y1 += y2; z1 -= z2; } # (x1,y1,z1) := (x1+x2,y1+y2,z1-z2)
               while (z1-y1 >= nenner);
        }
      if (z2-x2 <= z1+x1-1) goto no_more_subtract;
      subtract_from_2: # Hier ist z2-x2>=z1+x1.
        # Bestimme q := floor((z2-x2)/(z1+x1)) >= 1 :
        { var reg2 uintD zaehler = z2-x2;
          var reg1 uintD nenner = z1+x1; # z1+x1 <= z2-x2 < beta !
          if (floor(zaehler,8) >= nenner) # zaehler >= 8*nenner ?
            # ja -> Dividieren lohnt sich wohl
            { var reg3 uintD q = floorD(zaehler,nenner);
              x2 += muluD_unchecked(q,x1); # x2 := x2+q*x1
              y2 += muluD_unchecked(q,y1); # y2 := y2+q*y1
              z2 -= muluD_unchecked(q,z1); # z2 := z2-q*z1
            }
            else
            # nein -> ein paarmal subtrahieren ist wohl schneller
            do { x2 += x1; y2 += y1; z2 -= z1; } # (x2,y2,z2) := (x2+x1,y2+y1,z2-z1)
               while (z2-x2 >= nenner);
        }
      if (z1-y1 <= z2+y2-1) goto no_more_subtract;
      goto subtract_from_1;
      no_more_subtract: # Keine Subtraktion mehr mˆglich.
      erg->x1 = x1; erg->y1 = y1; erg->x2 = x2; erg->y2 = y2; # Ergebnis
    }
#endif
#if GCD_ALGO==3
  # Los geht's:
  local object I_I_gcd_I(a,b)
    var reg10 object a;
    var reg10 object b;
    { if (eq(a,Fixnum_1)) { return a; } # a=1 -> 1
      if (eq(b,Fixnum_1)) { return b; } # b=1 -> 1
      if (eq(b,Fixnum_0)) { return I_abs_I(a); } # b=0 -> (abs a)
      if (eq(a,Fixnum_0)) { return I_abs_I(b); } # a=0 -> (abs b)
     {SAVE_NUM_STACK # num_stack retten
      var reg6 uintD* a_MSDptr;
      var reg8 uintC a_len;
      var reg10 uintD* a_LSDptr;
      var reg7 uintD* b_MSDptr;
      var reg9 uintC b_len;
      var reg10 uintD* b_LSDptr;
      # Macro: erzeugt die NUDS zu (abs x), erniedrigt num_stack
      #define I_abs_to_NUDS(x)  \
        { I_to_NDS_1(x, x##_MSDptr = , x##_len = , x##_LSDptr = ); # (nichtleere) NDS holen \
          if ((sintD)x##_MSDptr[0] < 0) # falls <0, negieren:                               \
            { neg_loop_down(x##_LSDptr,x##_len); }                                          \
          if (x##_MSDptr[0] == 0) # normalisieren (max. 1 Nulldigit entfernen)              \
            { x##_MSDptr++; x##_len--; }                                                    \
        }
      I_abs_to_NUDS(a); # (abs a) als NUDS erzeugen
      I_abs_to_NUDS(b); # (abs b) als NUDS erzeugen
      # Jetzt ist a = a_MSDptr/a_len/a_LSDptr, b = b_MSDptr/b_len/b_LSDptr,
      # beides NUDS, und a_len>0, b_len>0.
      # Platz f¸r zwei Rechenregister besorgen, mit je max(a_len,b_len)+1 Digits:
      {var reg10 uintD* divroomptr; # Platz f¸r Divisionsergebnis
       var reg10 uintD* c_LSDptr;
       var reg10 uintD* d_LSDptr;
       {var reg10 uintL c_len = (uintL)(a_len>=b_len ? a_len : b_len) + 1;
        num_stack_need(c_len,divroomptr=,c_LSDptr=);
        num_stack_need(c_len,,d_LSDptr=);
        # Jetzt ist ../c_len/c_LSDptr, ../c_len/d_LSDptr frei.
       }
       loop
         { # Hier a,b>0, beides NUDS.
           # Vergleiche a und b:
           if (a_len > b_len) goto a_greater_b; # a>b ?
           if (a_len == b_len)
             { var reg1 signean vergleich = compare_loop_up(a_MSDptr,b_MSDptr,a_len);
               if (vergleich > 0) goto a_greater_b; # a>b ?
               if (vergleich == 0) break; # a=b ?
             }
           # a<b -> a,b vertauschen:
           swap(reg1 uintD*, a_MSDptr,b_MSDptr);
           swap(reg1 uintC, a_len,b_len);
           swap(reg1 uintD*, a_LSDptr,b_LSDptr);
           a_greater_b:
           # Hier a>b>0, beides NUDS.
           # Entscheidung, ob Division oder Linearkombination:
           { var reg1 uintD a_msd; # f¸hrende intDsize Bits von a
             var reg2 uintD b_msd; # entsprechende Bits von b
             { var reg4 uintC len_diff = a_len-b_len; # L‰ngendifferenz
               if (len_diff > 1) goto divide; # >=2 -> Bitl‰ngendifferenz>intDsize -> dividieren
               #define bitlendiff_limit  (intDsize/2) # sollte >0,<intDsize sein
              {var reg3 uintC a_msd_size;
               a_msd = a_MSDptr[0]; # f¸hrendes Digit von a
               integerlengthD(a_msd,a_msd_size=); # dessen Bit-L‰nge (>0,<=intDsize) berechnen
               b_msd = b_MSDptr[0];
               #if HAVE_DD
               {var reg5 uintDD b_msdd = # 2 f¸hrende Digits von b
                  (len_diff==0
                   ? highlowDD(b_msd, (b_len==1 ? 0 : b_MSDptr[1]))
                   : (uintDD)b_msd
                  );
                # a_msd_size+intDsize - b_msdd_size >= bitlendiff_limit -> dividieren:
                b_msdd = b_msdd >> a_msd_size;
                if (b_msdd < bit(intDsize-bitlendiff_limit)) goto divide;
                b_msd = lowD(b_msdd);
               }
               {var reg5 uintDD a_msdd = # 2 f¸hrende Digits von a
                  highlowDD(a_msd, (a_len==1 ? 0 : a_MSDptr[1]));
                a_msd = lowD(a_msdd >> a_msd_size);
               }
               #else
               if (len_diff==0)
                 { # a_msd_size - b_msd_size >= bitlendiff_limit -> dividieren:
                   if ((a_msd_size > bitlendiff_limit)
                       && (b_msd < bit(a_msd_size-bitlendiff_limit))
                      )
                     goto divide;
                   # Entscheidung f¸r Linearkombination ist gefallen.
                   # a_msd und b_msd so erweitern, daﬂ a_msd die f¸hrenden
                   # intDsize Bits von a enth‰lt:
                  {var reg5 uintC shiftcount = intDsize-a_msd_size; # Shiftcount nach links (>=0, <intDsize)
                   if (shiftcount>0)
                     { a_msd = a_msd << shiftcount;
                       b_msd = b_msd << shiftcount;
                       if (a_len>1)
                         { a_msd |= a_MSDptr[1] >> a_msd_size;
                           b_msd |= b_MSDptr[1] >> a_msd_size;
                     }   }
                   if (a_msd == b_msd) goto subtract;
                 }}
                 else
                 # len_diff=1
                 { # a_msd_size+intDsize - b_msd_size >= bitlendiff_limit -> dividieren:
                   if ((a_msd_size >= bitlendiff_limit)
                       || (b_msd < bit(a_msd_size+intDsize-bitlendiff_limit))
                      )
                     goto divide;
                   # Entscheidung f¸r Linearkombination ist gefallen.
                   # a_msd und b_msd so erweitern, daﬂ a_msd die f¸hrenden
                   # intDsize Bits von a enth‰lt:
                   # 0 < a_msd_size < b_msd_size + bitlendiff_limit - intDsize <= bitlendiff_limit < intDsize.
                   a_msd = (a_msd << (intDsize-a_msd_size)) | (a_MSDptr[1] >> a_msd_size);
                   b_msd = b_msd >> a_msd_size;
                 }
               #endif
               #undef bitlendiff_limit
             }}
             # Nun ist a_msd = a' > b' = b_msd.
             { # Euklid-Algorithmus auf den f¸hrenden Digits durchf¸hren:
               var partial_gcd_result likobi;
               partial_gcd(a_msd,b_msd,&likobi); # liefert x1,y1,x2,y2
               # Hier y1>0.
               if (likobi.x2==0)
                 { # Ersetze (a,b) := (a-y1*b,b).
                   if (likobi.y1==1) goto subtract; # einfacherer Fall
                   # Dazu evtl. a um 1 Digit erweitern, so daﬂ a_len=b_len+1:
                   if (a_len == b_len) { *--a_MSDptr = 0; a_len++; }
                   # und y1*b von a subtrahieren:
                   a_MSDptr[0] -= mulusub_loop_down(likobi.y1,b_LSDptr,a_LSDptr,b_len);
                 }
                 else
                 { # Ersetze (a,b) := (x1*a-y1*b,-x2*a+y2*b).
                   # Dazu evtl. b um 1 Digit erweitern, so daﬂ a_len=b_len:
                   if (!(a_len==b_len)) { *--b_MSDptr = 0; b_len++; }
                   # c := x1*a-y1*b bilden:
                   mulu_loop_down(likobi.x1,a_LSDptr,c_LSDptr,a_len);
                   /* c_LSDptr[-(uintL)a_len-1] -= */
                     mulusub_loop_down(likobi.y1,b_LSDptr,c_LSDptr,a_len);
                   # d := -x2*a+y2*b bilden:
                   mulu_loop_down(likobi.y2,b_LSDptr,d_LSDptr,a_len);
                   /* d_LSDptr[-(uintL)a_len-1] -= */
                     mulusub_loop_down(likobi.x2,a_LSDptr,d_LSDptr,a_len);
                   # Wir wissen, daﬂ 0 < c < b und 0 < d < a. Daher m¸ﬂten
                   # c_LSDptr[-a_len-1] und d_LSDptr[-a_len-1] =0 sein.
                   # a := c und b := d kopieren:
                   copy_loop_down(c_LSDptr,a_LSDptr,a_len);
                   copy_loop_down(d_LSDptr,b_LSDptr,a_len);
                   # b normalisieren:
                   while (b_MSDptr[0]==0) { b_MSDptr++; b_len--; }
             }   }
             if (FALSE)
               { subtract: # Ersetze (a,b) := (a-b,b).
                 if (!( subfrom_loop_down(b_LSDptr,a_LSDptr,b_len) ==0))
                   # ‹bertrag nach b_len Stellen, muﬂ also a_len=b_len+1 sein.
                   { a_MSDptr[0] -= 1; }
               }
             # a normalisieren:
             while (a_MSDptr[0]==0) { a_MSDptr++; a_len--; }
           }
           if (FALSE)
             { divide: # Ersetze (a,b) := (b , a mod b).
              {var reg10 uintD* old_a_LSDptr = a_LSDptr;
               var DS q;
               var DS r;
               UDS_divide_(a_MSDptr,a_len,a_LSDptr,b_MSDptr,b_len,b_LSDptr, divroomptr, &q,&r);
               a_MSDptr = b_MSDptr; a_len = b_len; a_LSDptr = b_LSDptr; # a := b
               b_len = r.len; if (b_len==0) break; # b=0 -> fertig
               b_LSDptr = old_a_LSDptr; # b ¸bernimmt den vorherigen Platz von a
               b_MSDptr = copy_loop_down(r.LSDptr,b_LSDptr,b_len); # b := r kopieren
               goto a_greater_b; # Nun ist a>b>0
             }}
      }  }
      RESTORE_NUM_STACK # num_stack (vorzeitig) zur¸ck
      return NUDS_to_I(a_MSDptr,a_len); # NUDS a als Ergebnis
      #undef I_abs_to_NUDS
    }}
#endif
#if XGCD_ALGO==3
# (xgcd A B) :==
# wie oben bei (gcd A B).
# Zus‰tzlich werden Variablen sA,sB,sk,uAa,uBa,uAb,uBb gef¸hrt,
# wobei sA,sB,sk Vorzeichen (+/- 1) und uAa,uBa,uAb,uBb Integers >=0 sind mit
#     uAa * sA*A - uBa * sB*B = a,
#   - uAb * sA*A + uBb * sB*B = b,
# ferner  uAa * uBb - uAb * uBa = sk  und daher (Cramersche Regel)
#   uBb * a + uBa * b = sk*sA*A, uAb * a + uAa * b = sk*sB*B.
# Zu Beginn (a,b) := (|A|,|B|), (sA,sB) := ((signum A), (signumB)),
#           (uAa,uBa,uAb,uBb) := (1,0,0,1).
# Beim Ersetzen (a,b) := (a-b,b)
#   ersetzt man (uAa,uBa,uAb,uBb) := (uAa+uAb,uBa+uBb,uAb,uBb).
# Beim Ersetzen (a,b) := (a-y1*b,b)
#   ersetzt man (uAa,uBa,uAb,uBb) := (uAa+y1*uAb,uBa+y1*uBb,uAb,uBb).
# Beim Ersetzen (a,b) := (x1*a-y1*b,-x2*a+y2*b) mit x1*y2-x2*y1=1
#   ersetzt man (uAa,uBa,uAb,uBb) :=
#               (x1*uAa+y1*uAb,x1*uBa+y1*uBb,x2*uAa+y2*uAb,x2*uBa+y2*uBb).
# Beim Ersetzen (a,b) := (b,a)
#   ersetzt man (uAa,uBa,uAb,uBb) := (uAb,uBb,uAa,uBa),
#               sk := -sk, (sA,sB) := (-sA,-sB).
# Beim Ersetzen (a,b) := (b,a-q*b)
#   ersetzt man (uAa,uBa,uAb,uBb) := (uAb,uBb,uAa+q*uAb,uBa+q*uBb),
#               sk := -sk, (sA,sB) := (-sA,-sB).
# Zum Schluﬂ ist a der ggT und a = uAa*sA * A + -uBa*sB * B
# die gew¸nschte Linearkombination.
# Da stets gilt sk*sA*A = |A|, sk*sB*B = |B|, a>=1, b>=1,
# folgt 0 <= uAa <= |B|, 0 <= uAb <= |B|, 0 <= uBa <= |A|, 0 <= uBb <= |A|.
# Ferner wird sk nie benutzt, braucht also nicht mitgef¸hrt zu werden.
  # Bildet u := u + v, wobei f¸r u gen¸gend Platz sei:
  # (Benutzt v.LSDptr nicht.)
  local void NUDS_likobi0_NUDS (DS* u, DS* v);
  local void NUDS_likobi0_NUDS(u,v)
    var reg1 DS* u;
    var reg2 DS* v;
    { var reg3 uintC u_len = u->len;
      var reg4 uintC v_len = v->len;
      if (u_len >= v_len)
        { if (!( addto_loop_down(v->LSDptr,u->LSDptr,v_len) ==0))
            { if (!( inc_loop_down(&(u->LSDptr)[-(uintL)v_len],u_len-v_len) ==0))
                { *--(u->MSDptr) = 1; u->len++; }
        }   }
        else # u_len <= v_len
        { u->MSDptr = copy_loop_down(&(v->LSDptr)[-(uintL)u_len],&(u->LSDptr)[-(uintL)u_len],v_len-u_len);
          u->len = v_len;
          if (!( addto_loop_down(v->LSDptr,u->LSDptr,u_len) ==0))
            { if (!( inc_loop_down(&(u->LSDptr)[-(uintL)u_len],v_len-u_len) ==0))
                { *--(u->MSDptr) = 1; u->len++; }
        }   }
    }
  # Bildet u := u + q*v, wobei f¸r u gen¸gend Platz sei:
  # (Dabei sei nachher u>0.)
  local void NUDS_likobi1_NUDS (DS* u, DS* v, uintD q);
  local void NUDS_likobi1_NUDS(u,v,q)
    var reg2 DS* u;
    var reg1 DS* v;
    var reg6 uintD q;
    { var reg3 uintC v_len = v->len;
      if (v_len>0) # nur nˆtig, falls v /=0
        { var reg4 uintC u_len = u->len;
          var reg5 uintD carry;
          if (u_len <= v_len) # evtl. u vergrˆﬂern
            { u->MSDptr = clear_loop_down(u->MSDptr,v_len-u_len+1);
              u->len = u_len = v_len+1;
            } # Nun ist u_len > v_len.
          carry = muluadd_loop_down(q,v->LSDptr,u->LSDptr,v_len);
          if (!(carry==0))
            { var reg5 uintD* ptr = &(u->LSDptr)[-(uintL)v_len-1];
              if ((ptr[0] += carry) < carry)
                { if (!( inc_loop_down(ptr,u_len-v_len-1) ==0))
                    { *--(u->MSDptr) = 1; u->len++; }
            }   }
          while ((u->MSDptr)[0]==0) { (u->MSDptr)++; u->len--; } # normalisieren
    }   }
  # Bildet (u,v) := (x1*u+y1*v,x2*u+y2*v), wobei f¸r u,v gen¸gend Platz sei:
  # (Dabei sei u>0 oder v>0, nachher u>0 und v>0.)
  local void NUDS_likobi2_NUDS (DS* u, DS* v, partial_gcd_result* q, uintD* c_LSDptr, uintD* d_LSDptr);
  local void NUDS_likobi2_NUDS(u,v,q,c_LSDptr,d_LSDptr)
    var reg1 DS* u;
    var reg2 DS* v;
    var reg3 partial_gcd_result* q;
    var reg9 uintD* c_LSDptr;
    var reg10 uintD* d_LSDptr;
    { var reg4 uintC u_len = u->len;
      var reg5 uintC v_len = v->len;
      var reg6 uintC c_len;
      var reg7 uintC d_len;
      if (u_len >= v_len)
        { mulu_loop_down(q->x1,u->LSDptr,c_LSDptr,u_len); c_len = u_len+1;
          mulu_loop_down(q->x2,u->LSDptr,d_LSDptr,u_len); d_len = u_len+1;
          if (!(v_len==0))
            {{var reg6 uintD carry =
                muluadd_loop_down(q->y1,v->LSDptr,c_LSDptr,v_len);
              if (!(carry==0))
                { var reg1 uintD* ptr = &c_LSDptr[-(uintL)v_len-1];
                  if ((ptr[0] += carry) < carry)
                    { if (!( inc_loop_down(ptr,u_len-v_len) ==0))
                        { c_len++; c_LSDptr[-(uintL)c_len] = 1; }
             }  }   }
             {var reg6 uintD carry =
                muluadd_loop_down(q->y2,v->LSDptr,d_LSDptr,v_len);
              if (!(carry==0))
                { var reg1 uintD* ptr = &d_LSDptr[-(uintL)v_len-1];
                  if ((ptr[0] += carry) < carry)
                    { if (!(inc_loop_down(ptr,u_len-v_len) ==0))
                        { d_len++; d_LSDptr[-(uintL)d_len] = 1; }
            }}  }   }
        }
        else
        { mulu_loop_down(q->y1,v->LSDptr,c_LSDptr,v_len); c_len = v_len+1;
          mulu_loop_down(q->y2,v->LSDptr,d_LSDptr,v_len); d_len = v_len+1;
          if (!(u_len==0))
            {{var reg6 uintD carry =
                muluadd_loop_down(q->x1,u->LSDptr,c_LSDptr,u_len);
              if (!(carry==0))
                { var reg1 uintD* ptr = &c_LSDptr[-(uintL)u_len-1];
                  if ((ptr[0] += carry) < carry)
                    { if (!( inc_loop_down(ptr,v_len-u_len) ==0))
                        { c_len++; c_LSDptr[-(uintL)c_len] = 1; }
             }  }   }
             {var reg6 uintD carry =
                muluadd_loop_down(q->x2,u->LSDptr,d_LSDptr,u_len);
              if (!(carry==0))
                { var reg1 uintD* ptr = &d_LSDptr[-(uintL)u_len-1];
                  if ((ptr[0] += carry) < carry)
                    { if (!( inc_loop_down(ptr,v_len-u_len) ==0))
                        { d_len++; d_LSDptr[-(uintL)d_len] = 1; }
            }}  }   }
        }
      u->MSDptr = copy_loop_down(c_LSDptr,u->LSDptr,c_len);
      while ((u->MSDptr)[0]==0) { u->MSDptr++; c_len--; }
      u->len = c_len;
      v->MSDptr = copy_loop_down(d_LSDptr,v->LSDptr,d_len);
      while ((v->MSDptr)[0]==0) { v->MSDptr++; d_len--; }
      v->len = d_len;
    }
  # Los geht's:
  local void I_I_xgcd_I_I_I(a,b)
    var reg10 object a;
    var reg10 object b;
    { if (eq(a,Fixnum_1)) # a=1 -> g=1, (u,v)=(1,0)
        { pushSTACK(Fixnum_1); pushSTACK(Fixnum_0);
          pushSTACK(a); return;
        }
      if (eq(b,Fixnum_1)) # b=1 -> g=1, (u,v)=(0,1)
        { pushSTACK(Fixnum_0); pushSTACK(Fixnum_1);
          pushSTACK(b); return;
        }
     {var reg10 boolean sA = (R_minusp(a) ? ~0 : 0); # Vorzeichen von A
      var reg10 boolean sB = (R_minusp(b) ? ~0 : 0); # Vorzeichen von B
      SAVE_NUM_STACK # num_stack retten
      var reg6 uintD* a_MSDptr;
      var reg8 uintC a_len;
      var reg10 uintD* a_LSDptr;
      var reg7 uintD* b_MSDptr;
      var reg9 uintC b_len;
      var reg10 uintD* b_LSDptr;
      # Macro: erzeugt die NUDS zu (abs x), erniedrigt num_stack
      #define I_abs_to_NUDS(x,zero_statement)  \
        { I_to_NDS_1(x, x##_MSDptr = , x##_len = , x##_LSDptr = ); # (nichtleere) NDS holen \
          if (x##_len == 0) { zero_statement } # falls =0, fertig                           \
          if ((sintD)x##_MSDptr[0] < 0) # falls <0, negieren:                               \
            { neg_loop_down(x##_LSDptr,x##_len); }                                          \
          if (x##_MSDptr[0] == 0) # normalisieren (max. 1 Nulldigit entfernen)              \
            { x##_MSDptr++; x##_len--; }                                                    \
        }
      I_abs_to_NUDS(a, # (abs A) als NUDS erzeugen
                    # A=0 -> g=|B|, (u,v) = (0,sB)
                    { pushSTACK(Fixnum_0); # u
                      pushSTACK(sB==0 ? Fixnum_1 : Fixnum_minus1); # v
                      pushSTACK(I_abs_I(b)); # g
                      return;
                    });
      I_abs_to_NUDS(b, # (abs B) als NUDS erzeugen
                    # B=0 -> g=|A|, (u,v) = (sA,0)
                    { pushSTACK(sA==0 ? Fixnum_1 : Fixnum_minus1); # u
                      pushSTACK(Fixnum_0); # v
                      pushSTACK(I_abs_I(a)); # g
                      return;
                    });
      # Jetzt ist a = a_MSDptr/a_len/a_LSDptr, b = b_MSDptr/b_len/b_LSDptr,
      # beides NUDS, und a_len>0, b_len>0.
      {# Beifaktoren:
       var DS uAa;
       var DS uBa;
       var DS uAb;
       var DS uBb;
       # Rechenregister:
       var reg10 uintD* divroomptr; # Platz f¸r Divisionsergebnis
       var reg10 uintD* c_LSDptr;
       var reg10 uintD* d_LSDptr;
       # Platz f¸r uAa,uBa,uAb,uBb besorgen:
       {var reg1 uintC u_len = b_len+1;
        num_stack_need(u_len,,uAa.LSDptr=); uAa.MSDptr = uAa.LSDptr;
        num_stack_need(u_len,,uAb.LSDptr=); uAb.MSDptr = uAb.LSDptr;
       }
       {var reg1 uintC u_len = a_len+1;
        num_stack_need(u_len,,uBa.LSDptr=); uBa.MSDptr = uBa.LSDptr;
        num_stack_need(u_len,,uBb.LSDptr=); uBb.MSDptr = uBb.LSDptr;
       }
       *--uAa.MSDptr = 1; uAa.len = 1; # uAa := 1
       uBa.len = 0; # uBa := 0
       uAb.len = 0; # uAb := 0
       *--uBb.MSDptr = 1; uBb.len = 1; # uBb := 1
       # Jetzt ist uAa = uAa.MSDptr/uAa.len/uAa.LSDptr,
       #           uBa = uBa.MSDptr/uBa.len/uBa.LSDptr,
       #           uAb = uAb.MSDptr/uAb.len/uAb.LSDptr,
       #           uBb = uBb.MSDptr/uBb.len/uBb.LSDptr,
       # alles NUDS.
       # Platz f¸r zwei Rechenregister besorgen, mit je max(a_len,b_len)+1 Digits:
       {var reg10 uintL c_len = (uintL)(a_len>=b_len ? a_len : b_len) + 1;
        num_stack_need(c_len,divroomptr=,c_LSDptr=);
        num_stack_need(c_len,,d_LSDptr=);
        # Jetzt ist ../c_len/c_LSDptr, ../c_len/d_LSDptr frei.
       }
       loop
         { # Hier a,b>0, beides NUDS.
           # Vergleiche a und b:
           if (a_len > b_len) goto a_greater_b; # a>b ?
           if (a_len == b_len)
             { var reg1 signean vergleich = compare_loop_up(a_MSDptr,b_MSDptr,a_len);
               if (vergleich > 0) goto a_greater_b; # a>b ?
               if (vergleich == 0) break; # a=b ?
             }
           # a<b -> a,b vertauschen:
           swap(reg1 uintD*, a_MSDptr,b_MSDptr);
           swap(reg1 uintC, a_len,b_len);
           swap(reg1 uintD*, a_LSDptr,b_LSDptr);
           a_greater_b_swap:
           swap(DS, uAa,uAb); # und uAa und uAb vertauschen
           swap(DS, uBa,uBb); # und uBa und uBb vertauschen
           sA = ~sA; sB = ~sB; # und sA und sB umdrehen
           a_greater_b:
           # Hier a>b>0, beides NUDS.
           # Entscheidung, ob Division oder Linearkombination:
           { var reg1 uintD a_msd; # f¸hrende intDsize Bits von a
             var reg2 uintD b_msd; # entsprechende Bits von b
             { var reg4 uintC len_diff = a_len-b_len; # L‰ngendifferenz
               if (len_diff > 1) goto divide; # >=2 -> Bitl‰ngendifferenz>intDsize -> dividieren
               #define bitlendiff_limit  (intDsize/2) # sollte >0,<intDsize sein
              {var reg3 uintC a_msd_size;
               a_msd = a_MSDptr[0]; # f¸hrendes Digit von a
               integerlengthD(a_msd,a_msd_size=); # dessen Bit-L‰nge (>0,<=intDsize) berechnen
               b_msd = b_MSDptr[0];
               #if HAVE_DD
               {var reg5 uintDD b_msdd = # 2 f¸hrende Digits von b
                  (len_diff==0
                   ? highlowDD(b_msd, (b_len==1 ? 0 : b_MSDptr[1]))
                   : (uintDD)b_msd
                  );
                # a_msd_size+intDsize - b_msdd_size >= bitlendiff_limit -> dividieren:
                b_msdd = b_msdd >> a_msd_size;
                if (b_msdd < bit(intDsize-bitlendiff_limit)) goto divide;
                b_msd = lowD(b_msdd);
               }
               {var reg5 uintDD a_msdd = # 2 f¸hrende Digits von a
                  highlowDD(a_msd, (a_len==1 ? 0 : a_MSDptr[1]));
                a_msd = lowD(a_msdd >> a_msd_size);
               }
               #else
               if (len_diff==0)
                 { # a_msd_size - b_msd_size >= bitlendiff_limit -> dividieren:
                   if ((a_msd_size > bitlendiff_limit)
                       && (b_msd < bit(a_msd_size-bitlendiff_limit))
                      )
                     goto divide;
                   # Entscheidung f¸r Linearkombination ist gefallen.
                   # a_msd und b_msd so erweitern, daﬂ a_msd die f¸hrenden
                   # intDsize Bits von a enth‰lt:
                  {var reg5 uintC shiftcount = intDsize-a_msd_size; # Shiftcount nach links (>=0, <intDsize)
                   if (shiftcount>0)
                     { a_msd = a_msd << shiftcount;
                       b_msd = b_msd << shiftcount;
                       if (a_len>1)
                         { a_msd |= a_MSDptr[1] >> a_msd_size;
                           b_msd |= b_MSDptr[1] >> a_msd_size;
                     }   }
                   if (a_msd == b_msd) goto subtract;
                 }}
                 else
                 # len_diff=1
                 { # a_msd_size+intDsize - b_msd_size >= bitlendiff_limit -> dividieren:
                   if ((a_msd_size >= bitlendiff_limit)
                       || (b_msd < bit(a_msd_size+intDsize-bitlendiff_limit))
                      )
                     goto divide;
                   # Entscheidung f¸r Linearkombination ist gefallen.
                   # a_msd und b_msd so erweitern, daﬂ a_msd die f¸hrenden
                   # intDsize Bits von a enth‰lt:
                   # 0 < a_msd_size < b_msd_size + bitlendiff_limit - intDsize <= bitlendiff_limit < intDsize.
                   a_msd = (a_msd << (intDsize-a_msd_size)) | (a_MSDptr[1] >> a_msd_size);
                   b_msd = b_msd >> a_msd_size;
                 }
               #endif
               #undef bitlendiff_limit
             }}
             # Nun ist a_msd = a' > b' = b_msd.
             { # Euklid-Algorithmus auf den f¸hrenden Digits durchf¸hren:
               var partial_gcd_result likobi;
               partial_gcd(a_msd,b_msd,&likobi); # liefert x1,y1,x2,y2
               # Hier y1>0.
               if (likobi.x2==0)
                 { # Ersetze (a,b) := (a-y1*b,b).
                   if (likobi.y1==1) goto subtract; # einfacherer Fall
                   # Dazu evtl. a um 1 Digit erweitern, so daﬂ a_len=b_len+1:
                   if (a_len == b_len) { *--a_MSDptr = 0; a_len++; }
                   # und y1*b von a subtrahieren:
                   a_MSDptr[0] -= mulusub_loop_down(likobi.y1,b_LSDptr,a_LSDptr,b_len);
                   NUDS_likobi1_NUDS(&uAa,&uAb,likobi.y1); # uAa := uAa + y1 * uAb
                   NUDS_likobi1_NUDS(&uBa,&uBb,likobi.y1); # uBa := uBa + y1 * uBb
                 }
                 else
                 { # Ersetze (uAa,uAb) := (x1*uAa+y1*uAb,x2*uAa+y2*uAb) :
                   NUDS_likobi2_NUDS(&uAa,&uAb,&likobi,c_LSDptr,d_LSDptr);
                   # Ersetze (uBa,uBb) := (x1*uBa+y1*uBb,x2*uBa+y2*uBb) :
                   NUDS_likobi2_NUDS(&uBa,&uBb,&likobi,c_LSDptr,d_LSDptr);
                   # Ersetze (a,b) := (x1*a-y1*b,-x2*a+y2*b).
                   # Dazu evtl. b um 1 Digit erweitern, so daﬂ a_len=b_len:
                   if (!(a_len==b_len)) { *--b_MSDptr = 0; b_len++; }
                   # c := x1*a-y1*b bilden:
                   mulu_loop_down(likobi.x1,a_LSDptr,c_LSDptr,a_len);
                   /* c_LSDptr[-(uintL)a_len-1] -= */
                     mulusub_loop_down(likobi.y1,b_LSDptr,c_LSDptr,a_len);
                   # d := -x2*a+y2*b bilden:
                   mulu_loop_down(likobi.y2,b_LSDptr,d_LSDptr,a_len);
                   /* d_LSDptr[-(uintL)a_len-1] -= */
                     mulusub_loop_down(likobi.x2,a_LSDptr,d_LSDptr,a_len);
                   # Wir wissen, daﬂ 0 < c < b und 0 < d < a. Daher m¸ﬂten
                   # c_LSDptr[-a_len-1] und d_LSDptr[-a_len-1] =0 sein.
                   # a := c und b := d kopieren:
                   copy_loop_down(c_LSDptr,a_LSDptr,a_len);
                   copy_loop_down(d_LSDptr,b_LSDptr,a_len);
                   # b normalisieren:
                   while (b_MSDptr[0]==0) { b_MSDptr++; b_len--; }
             }   }
             if (FALSE)
               { subtract: # Ersetze (a,b) := (a-b,b).
                 NUDS_likobi0_NUDS(&uAa,&uAb); # uAa := uAa + uAb
                 NUDS_likobi0_NUDS(&uBa,&uBb); # uBa := uBa + uBb
                 if (!( subfrom_loop_down(b_LSDptr,a_LSDptr,b_len) ==0))
                   # ‹bertrag nach b_len Stellen, muﬂ also a_len=b_len+1 sein.
                   { a_MSDptr[0] -= 1; }
               }
             # a normalisieren:
             while (a_MSDptr[0]==0) { a_MSDptr++; a_len--; }
           }
           if (FALSE)
             { divide: # Ersetze (a,b) := (b , a mod b).
              {var reg10 uintD* old_a_LSDptr = a_LSDptr;
               var DS q;
               var DS r;
               UDS_divide_(a_MSDptr,a_len,a_LSDptr,b_MSDptr,b_len,b_LSDptr, divroomptr, &q,&r);
               a_MSDptr = b_MSDptr; a_len = b_len; a_LSDptr = b_LSDptr; # a := b
               b_len = r.len; if (b_len==0) break; # b=0 -> fertig
               b_LSDptr = old_a_LSDptr; # b ¸bernimmt den vorherigen Platz von a
               b_MSDptr = copy_loop_down(r.LSDptr,b_LSDptr,b_len); # b := r kopieren
               # (uAa,uAb) := (uAb,uAa+q*uAb) :
               if (!(uAb.len==0))
                 { mulu_2loop_down(q.LSDptr,q.len,uAb.LSDptr,uAb.len,c_LSDptr); # q * uAb
                  {var DS c;
                   c.LSDptr = c_LSDptr; c.len = q.len + uAb.len;
                   if (c_LSDptr[-(uintL)c.len]==0) { c.len--; } # normalisieren
                   NUDS_likobi0_NUDS(&uAa,&c); # zu uAa addieren
                 }} # noch uAa,uAb vertauschen (sp‰ter)
               # (uBa,uBb) := (uBb,uBa+q*uBb) :
               if (!(uBb.len==0))
                 { mulu_2loop_down(q.LSDptr,q.len,uBb.LSDptr,uBb.len,c_LSDptr); # q * uBb
                  {var DS c;
                   c.LSDptr = c_LSDptr; c.len = q.len + uBb.len;
                   if (c_LSDptr[-(uintL)c.len]==0) { c.len--; } # normalisieren
                   NUDS_likobi0_NUDS(&uBa,&c); # zu uBa addieren
                 }} # noch uBa,uBb vertauschen (sp‰ter)
               goto a_greater_b_swap; # Nun ist a>b>0
             }}
         }
       # uAb mit Vorfaktor -sA versehen:
       *--uAb.MSDptr = 0; uAb.len++;
       if (sA==0) { neg_loop_down(uAb.LSDptr,uAb.len); }
       # uBb mit Vorfaktor sB versehen:
       *--uBb.MSDptr = 0; uBb.len++;
       if (!(sB==0)) { neg_loop_down(uBb.LSDptr,uBb.len); }
       pushSTACK(DS_to_I(uAb.MSDptr,uAb.len)); # DS uAb als Vorfaktor von A
       pushSTACK(DS_to_I(uBb.MSDptr,uBb.len)); # DS uBb als Vorfaktor von B
      }
      pushSTACK(NUDS_to_I(a_MSDptr,a_len)); # NUDS a als ggT
      RESTORE_NUM_STACK # num_stack zur¸ck
      #undef I_abs_to_NUDS
    }}
#endif

# Liefert das kgV zweier Integers.
# I_I_lcm_I(a,b)
# > a,b: zwei Integers
# < ergebnis: (lcm a b), ein Integer >=0
# kann GC auslˆsen
  local object I_I_lcm_I (object a, object b);
# Methode:
# a=0 oder b=0 -> Ergebnis 0.
# a:=(abs a), b:=(abs b).
# g:=ggT(a,b)>0.
# Falls g=1, Ergebnis a*b, sonst Ergebnis (a/g)*b.
  local object I_I_lcm_I(a,b)
    var reg1 object a;
    var reg3 object b;
    { if (eq(a,Fixnum_0)) { return a; }
      if (eq(b,Fixnum_0)) { return b; }
      # Betr‰ge nehmen:
      pushSTACK(b); pushSTACK(I_abs_I(a)); STACK_1 = b = I_abs_I(STACK_1);
      # Stackaufbau: b := (abs b), a := (abs a).
     {var reg2 object g = I_I_gcd_I(STACK_0,b); # g = (gcd a b)
      a = popSTACK();
      if (!eq(g,Fixnum_1)) { a = I_I_exquopos_I(a,g); } # a durch g (beide >0) dividieren
      return I_I_mal_I(a,popSTACK()); # mit b multiplizieren
    }}

