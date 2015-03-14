# Logische Operationen auf Integers

# Liefert die Anzahl Digits, die ein Integer als DS bräuchte.
# (Leicht aufgerundet.)
  local uintC I_to_DS_need (object obj);
  local uintC I_to_DS_need(obj)
    var reg1 object obj;
    { if (I_fixnump(obj))
        return FN_maxlength; # das wird reichen
        else
        return TheBignum(obj)->length;
    }

# Integer to Digit sequence, n Digits
# I_to_DS_n(obj,n,ptr=);
# Integer obj zu einer Digit sequence MSDptr/n/LSDptr machen,
# die genau n Digits hat (sollte n >= Bedarf und >= FN_maxlength sein).
# Die neue Digit-sequence darf modifiziert werden.
# < ptr: MSDptr der neuen DS
# Dabei wird num_stack erniedrigt.
  #define I_to_DS_n(obj,n,ptr_zuweisung)  \
    {var reg1 uintD* destptr;                 \
     num_stack_need(n,,destptr=);             \
     ptr_zuweisung I_to_DS_n_(obj,n,destptr); \
    }
  local uintD* I_to_DS_n_ (object obj, uintC n, uintD* destptr);
  local uintD* I_to_DS_n_(obj,n,destptr)
    var reg4 object obj;
    var reg3 uintC n;
    var reg1 uintD* destptr;
    { # Nun sind unterhalb von destptr n Digits Platz.
      # oberen Teil der DS aus obj füllen, dabei destptr erniedrigen:
      if (I_fixnump(obj))
        # Fixnum:
        { var reg2 uint32 wert = FN_to_L(obj);
          #define FN_maxlength_a  (intLsize/intDsize)
          #define FN_maxlength_b  (FN_maxlength<=FN_maxlength_a ? FN_maxlength : FN_maxlength_a)
          # FN_maxlength Digits ablegen. Davon kann man FN_maxlength_b Digits aus wert nehmen.
          #if (FN_maxlength_b > 1)
          doconsttimes(FN_maxlength_b-1,
            *--destptr = (uintD)wert; wert = wert >> intDsize;
            );
          #endif
          *--destptr = (uintD)wert;
          #if (FN_maxlength > FN_maxlength_b)
          # Es ist oint_addr_len = intLsize, brauche
          # noch FN_maxlength-FN_maxlength_b = 1 Digit.
          *--destptr = (sintD)R_sign(obj);
          #endif
          n -= FN_maxlength;
        }
        else
        # Bignum:
        { var reg2 uintC len = TheBignum(obj)->length;
          # Pointer bestimmen:
          var reg1 uintD* ptr = &TheBignum(obj)->data[(uintL)len];
          n -= len;
          destptr = copy_loop_down(ptr,destptr,len); # DS kopieren
        }
      # unteren Teil mit Fülldigits, gebildet aus dem Vorzeichen, füllen:
      if (!(n==0))
        { destptr = fill_loop_down(destptr,n,sign_of_sintD(destptr[0])); }
      # destptr zeigt nun aufs untere Ende der DS.
      return destptr;
    }

# Logische Operationen auf Integers:
# Methode: aus den Längen der beiden Argumente eine obere Schranke für
# die Länge des Ergebnisses berechnen (das Maximum der beiden Längen und
# FN_maxlength), so daß das MSD für unendlich viele Bits steht.
# Dann beide Argumente in gleichgroße Digit sequences umwandeln, Operation
# mit einer einfachen Schleife durchführen.

# (LOGIOR x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_I_logior_I (object x, object y);
  local object I_I_logior_I(x,y)
    var reg3 object x;
    var reg3 object y;
    { if (I_fixnump(x) && I_fixnump(y)) # Beides Fixnums -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 ((oint)x | (oint)y);
        }
        else
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintC n; # Anzahl der Digits
         {var reg5 uintC nx = I_to_DS_need(x);
          var reg5 uintC ny = I_to_DS_need(y);
          n = (nx>=ny ? nx : ny);
         }
         {  var reg1 uintD* xptr; I_to_DS_n(x,n,xptr=); # Pointer in DS zu x
          { var reg2 uintD* yptr; I_to_DS_n(y,n,yptr=); # Pointer in DS zu y
           {var reg4 uintD* zptr = xptr; # Pointer aufs Ergebnis
            or_loop_up(xptr,yptr,n); # mit OR verknüpfen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            return DS_to_I(zptr,n); # Ergebnis als Integer
    }   }}}}

# (LOGXOR x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_I_logxor_I (object x, object y);
  local object I_I_logxor_I(x,y)
    var reg3 object x;
    var reg3 object y;
    { if (I_fixnump(x) && I_fixnump(y)) # Beides Fixnums -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 (((oint)x ^ (oint)y) | ((oint)fixnum_type << oint_type_shift));
        }
        else
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintC n; # Anzahl der Digits
         {var reg5 uintC nx = I_to_DS_need(x);
          var reg5 uintC ny = I_to_DS_need(y);
          n = (nx>=ny ? nx : ny);
         }
         {  var reg1 uintD* xptr; I_to_DS_n(x,n,xptr=); # Pointer in DS zu x
          { var reg2 uintD* yptr; I_to_DS_n(y,n,yptr=); # Pointer in DS zu y
           {var reg4 uintD* zptr = xptr; # Pointer aufs Ergebnis
            xor_loop_up(xptr,yptr,n); # mit XOR verknüpfen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            return DS_to_I(zptr,n); # Ergebnis als Integer
    }   }}}}

# (LOGAND x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_I_logand_I (object x, object y);
  local object I_I_logand_I(x,y)
    var reg3 object x;
    var reg3 object y;
    { if (I_fixnump(x) && I_fixnump(y)) # Beides Fixnums -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 ((oint)x & (oint)y);
        }
        else
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintC n; # Anzahl der Digits
         {var reg5 uintC nx = I_to_DS_need(x);
          var reg5 uintC ny = I_to_DS_need(y);
          n = (nx>=ny ? nx : ny);
         }
         {  var reg1 uintD* xptr; I_to_DS_n(x,n,xptr=); # Pointer in DS zu x
          { var reg2 uintD* yptr; I_to_DS_n(y,n,yptr=); # Pointer in DS zu y
           {var reg4 uintD* zptr = xptr; # Pointer aufs Ergebnis
            and_loop_up(xptr,yptr,n); # mit AND verknüpfen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            return DS_to_I(zptr,n); # Ergebnis als Integer
    }   }}}}

# (LOGEQV x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_I_logeqv_I (object x, object y);
  local object I_I_logeqv_I(x,y)
    var reg3 object x;
    var reg3 object y;
    { if (I_fixnump(x) && I_fixnump(y)) # Beides Fixnums -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 ( ~((oint)x ^ (oint)y)
                  & (((oint)fixnum_type << oint_type_shift) | FN_value_vz_mask) );
        }
        else
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintC n; # Anzahl der Digits
         {var reg5 uintC nx = I_to_DS_need(x);
          var reg5 uintC ny = I_to_DS_need(y);
          n = (nx>=ny ? nx : ny);
         }
         {  var reg1 uintD* xptr; I_to_DS_n(x,n,xptr=); # Pointer in DS zu x
          { var reg2 uintD* yptr; I_to_DS_n(y,n,yptr=); # Pointer in DS zu y
           {var reg4 uintD* zptr = xptr; # Pointer aufs Ergebnis
            eqv_loop_up(xptr,yptr,n); # mit NOT XOR verknüpfen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            return DS_to_I(zptr,n); # Ergebnis als Integer
    }   }}}}

# (LOGNAND x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_I_lognand_I (object x, object y);
  local object I_I_lognand_I(x,y)
    var reg3 object x;
    var reg3 object y;
    { if (I_fixnump(x) && I_fixnump(y)) # Beides Fixnums -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 (((oint)x & (oint)y) ^ FN_value_vz_mask);
        }
        else
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintC n; # Anzahl der Digits
         {var reg5 uintC nx = I_to_DS_need(x);
          var reg5 uintC ny = I_to_DS_need(y);
          n = (nx>=ny ? nx : ny);
         }
         {  var reg1 uintD* xptr; I_to_DS_n(x,n,xptr=); # Pointer in DS zu x
          { var reg2 uintD* yptr; I_to_DS_n(y,n,yptr=); # Pointer in DS zu y
           {var reg4 uintD* zptr = xptr; # Pointer aufs Ergebnis
            nand_loop_up(xptr,yptr,n); # mit NOT AND verknüpfen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            return DS_to_I(zptr,n); # Ergebnis als Integer
    }   }}}}

# (LOGNOR x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_I_lognor_I (object x, object y);
  local object I_I_lognor_I(x,y)
    var reg3 object x;
    var reg3 object y;
    { if (I_fixnump(x) && I_fixnump(y)) # Beides Fixnums -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 (((oint)x | (oint)y) ^ FN_value_vz_mask);
        }
        else
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintC n; # Anzahl der Digits
         {var reg5 uintC nx = I_to_DS_need(x);
          var reg5 uintC ny = I_to_DS_need(y);
          n = (nx>=ny ? nx : ny);
         }
         {  var reg1 uintD* xptr; I_to_DS_n(x,n,xptr=); # Pointer in DS zu x
          { var reg2 uintD* yptr; I_to_DS_n(y,n,yptr=); # Pointer in DS zu y
           {var reg4 uintD* zptr = xptr; # Pointer aufs Ergebnis
            nor_loop_up(xptr,yptr,n); # mit NOT OR verknüpfen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            return DS_to_I(zptr,n); # Ergebnis als Integer
    }   }}}}

# (LOGANDC2 x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_I_logandc2_I (object x, object y);
  local object I_I_logandc2_I(x,y)
    var reg3 object x;
    var reg3 object y;
    { if (I_fixnump(x) && I_fixnump(y)) # Beides Fixnums -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 (((oint)x & ~(oint)y) | ((oint)fixnum_type << oint_type_shift));
        }
        else
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintC n; # Anzahl der Digits
         {var reg5 uintC nx = I_to_DS_need(x);
          var reg5 uintC ny = I_to_DS_need(y);
          n = (nx>=ny ? nx : ny);
         }
         {  var reg1 uintD* xptr; I_to_DS_n(x,n,xptr=); # Pointer in DS zu x
          { var reg2 uintD* yptr; I_to_DS_n(y,n,yptr=); # Pointer in DS zu y
           {var reg4 uintD* zptr = xptr; # Pointer aufs Ergebnis
            andc2_loop_up(xptr,yptr,n); # mit AND NOT verknüpfen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            return DS_to_I(zptr,n); # Ergebnis als Integer
    }   }}}}

# (LOGANDC1 x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
#if 1 # Macro spart Code
  #define I_I_logandc1_I(x,y)  I_I_logandc2_I(y,x)
#else
  local object I_I_logandc1_I (object x, object y);
  local object I_I_logandc1_I(x,y)
    var reg1 object x;
    var reg1 object y;
    { return I_I_logandc2_I(y,x); }
#endif

# (LOGORC2 x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_I_logorc2_I (object x, object y);
  local object I_I_logorc2_I(x,y)
    var reg3 object x;
    var reg3 object y;
    { if (I_fixnump(x) && I_fixnump(y)) # Beides Fixnums -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 ( ((oint)x | ~(oint)y)
                  & (((oint)fixnum_type << oint_type_shift) | FN_value_vz_mask) );
        }
        else
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintC n; # Anzahl der Digits
         {var reg5 uintC nx = I_to_DS_need(x);
          var reg5 uintC ny = I_to_DS_need(y);
          n = (nx>=ny ? nx : ny);
         }
         {  var reg1 uintD* xptr; I_to_DS_n(x,n,xptr=); # Pointer in DS zu x
          { var reg2 uintD* yptr; I_to_DS_n(y,n,yptr=); # Pointer in DS zu y
           {var reg4 uintD* zptr = xptr; # Pointer aufs Ergebnis
            orc2_loop_up(xptr,yptr,n); # mit OR NOT verknüpfen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            return DS_to_I(zptr,n); # Ergebnis als Integer
    }   }}}}

# (LOGORC1 x y), wenn x, y Integers sind.
# Ergebnis Integer.
# kann GC auslösen.
#if 1 # Macro spart Code
  #define I_I_logorc1_I(x,y)  I_I_logorc2_I(y,x)
#else
  local object I_I_logorc1_I (object x, object y);
  local object I_I_logorc1_I(x,y)
    var reg1 object x;
    var reg1 object y;
    { return I_I_logorc2_I(y,x); }
#endif

# (LOGNOT x), wenn x ein Integer sind.
# Ergebnis Integer.
# kann GC auslösen.
  local object I_lognot_I (object x);
  local object I_lognot_I(x)
    var reg3 object x;
    { if (I_fixnump(x)) # Fixnum -> ganz einfach:
        { return (object) # bitweise als Fixnum zurück
                 ((oint)x ^ FN_value_vz_mask);
        }
        else
        # Bignum:
        { SAVE_NUM_STACK # num_stack retten
          var reg5 uintD* MSDptr;
          var reg5 uintC n;
          BN_to_NDS(x, MSDptr=,n=,); # NDS zu x bilden
          # Es ist n>=bn_minlength,
          # und die ersten intDsize+1 Bit sind nicht alle gleich.
          not_loop_up(MSDptr,n); # mit NOT komplementieren,
                          # wegen n>0 wird auch das Vorzeichenbit umgedreht
          # MSDptr/n/LSDptr ist immer noch eine NDS, da n>=bn_minlength
          # und die ersten intDsize+1 Bit nicht alle gleich sind.
          RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
          return NDS_to_I(MSDptr,n); # Ergebnis als Integer
    }   }

# Konstanten für BOOLE:
# Bit-wert in 'integer1' + 2 * Bit-wert in 'integer2' = k
# Fixnum mit 4 Bits: Bit k gibt an, was bei diesen zwei Bit-werten kommt.
#           Name             k=0 k=1 k=2 k=3 (Bitwerte: [00] [10] [01] [11])
  #define boole_clr     0  #  0   0   0   0
  #define boole_set    15  #  1   1   1   1
  #define boole_1      10  #  0   1   0   1
  #define boole_2      12  #  0   0   1   1
  #define boole_c1      5  #  1   0   1   0
  #define boole_c2      3  #  1   1   0   0
  #define boole_and     8  #  0   0   0   1
  #define boole_ior    14  #  0   1   1   1
  #define boole_xor     6  #  0   1   1   0
  #define boole_eqv     9  #  1   0   0   1
  #define boole_nand    7  #  1   1   1   0
  #define boole_nor     1  #  1   0   0   0
  #define boole_andc1   4  #  0   0   1   0
  #define boole_andc2   2  #  0   1   0   0
  #define boole_orc1   13  #  1   0   1   1
  #define boole_orc2   11  #  1   1   0   1

# (BOOLE op x y), wenn x und y Integers und op ein Objekt sind.
# Ergebnis Integer.
# OP_I_I_boole_I(op,x,y)
# kann GC auslösen
  local object OP_I_I_boole_I (object op, object x, object y);
  local object OP_I_I_boole_I(op,x,y)
    var reg1 object op;
    var reg2 object x;
    var reg3 object y;
    { switch ((oint)op)
        { case (oint)fixnum( boole_clr ):
            return Fixnum_0;
          case (oint)fixnum( boole_set ):
            return Fixnum_minus1;
          case (oint)fixnum( boole_1 ):
            return x;
          case (oint)fixnum( boole_2 ):
            return y;
          case (oint)fixnum( boole_c1 ):
            return I_lognot_I(x);
          case (oint)fixnum( boole_c2 ):
            return I_lognot_I(y);
          case (oint)fixnum( boole_and ):
            return I_I_logand_I(x,y);
          case (oint)fixnum( boole_ior ):
            return I_I_logior_I(x,y);
          case (oint)fixnum( boole_xor ):
            return I_I_logxor_I(x,y);
          case (oint)fixnum( boole_eqv ):
            return I_I_logeqv_I(x,y);
          case (oint)fixnum( boole_nand ):
            return I_I_lognand_I(x,y);
          case (oint)fixnum( boole_nor ):
            return I_I_lognor_I(x,y);
          case (oint)fixnum( boole_andc1 ):
            return I_I_logandc1_I(x,y);
          case (oint)fixnum( boole_andc2 ):
            return I_I_logandc2_I(x,y);
          case (oint)fixnum( boole_orc1 ):
            return I_I_logorc1_I(x,y);
          case (oint)fixnum( boole_orc2 ):
            return I_I_logorc2_I(x,y);
          default: # falscher Operator
            pushSTACK(op); pushSTACK(S(boole));
            fehler(
                   DEUTSCH ? "~: ~ ist keine gültige Boolesche Operation." :
                   ENGLISH ? "~: ~ is not a valid boolean operation" :
                   FRANCAIS ? "~ : ~ n'est pas une opération booléenne admise." :
                   ""
                  );
    }   }

# Prüft, ob (LOGTEST x y), wo x und y Integers sind.
# (LOGTEST x y) = (NOT (ZEROP (LOGAND x y))).
# I_I_logtest(x,y)
# < ergebnis: /=0, falls ja; =0, falls nein.
  local boolean I_I_logtest (object x, object y);
  local boolean I_I_logtest(x,y)
    var reg4 object x;
    var reg5 object y;
    # Methode:
    #  Fixnums separat behandeln.
    #  Sei oBdA x die kürzere der beiden Zahlen (in Digits).
    #  x echt kürzer und x<0 -> [eines der most signif. intDsize+1 Bits von y ist 1] Ja.
    #  Beide gleich lang oder x>=0 ->
    #   Kann mich auf die untersten length(x) Digits beschraenken.
    #   Mit AND durchlaufen, abbrechen (mit "Ja") falls /=0. Am Ende: Nein.
    { if (I_fixnump(x))
        if (I_fixnump(y))
          # beides Fixnums
          { if (((oint)x & (oint)y & FN_value_vz_mask)==0)
              return FALSE;
              else
              return TRUE;
          }
          else
          # x Fixnum, y Bignum, also ist x echt kürzer
          { xFN_yBN:
            if (R_minusp(x)) return TRUE; # x<0 -> ja.
            # x>=0. Kombiniere x mit den pFN_maxlength letzten Digits von y.
           {var reg7 uintD* yLSDptr;
            var reg6 uintL x_ = posfixnum_to_L(x);
            BN_to_NDS_nocopy(y, ,,yLSDptr=);
            #if (pFN_maxlength > 1)
            doconsttimes(pFN_maxlength-1,
              if (*--yLSDptr & (uintD)x_) return TRUE;
              x_ = x_ >> intDsize;
              );
            #endif
            if (*--yLSDptr & (uintD)x_) return TRUE;
            return FALSE;
          }}
        else
        if (I_fixnump(y))
          # x Bignum, y Fixnum
          {{var reg1 object h = x; x = y; y = h; } # x und y vertauschen
           goto xFN_yBN; # und weiter wie oben
          }
          else
          # x,y Bignums
          { var reg6 uintD* xMSDptr;
            var reg6 uintC xlen;
            var reg6 uintD* yMSDptr;
            var reg6 uintC ylen;
            BN_to_NDS_nocopy(x, xMSDptr=,xlen=,);
            BN_to_NDS_nocopy(y, yMSDptr=,ylen=,);
            # Beachte: xlen>0, ylen>0.
            if (!(xlen==ylen))
              # beide verschieden lang
              { if (xlen>ylen)
                  # vertauschen
                  {{var reg1 uintD* temp = xMSDptr; xMSDptr = yMSDptr; yMSDptr = temp; }
                   xlen = ylen;
                  }
                # jetzt ist x die echt kürzere DS.
                if ((sintD)xMSDptr[0]<0) # der echt kürzere ist negativ?
                  return TRUE;
                # Der echt kürzere ist positiv.
              }
            # xMSDptr/xlen/.. ist die kürzere DS, yMSDptr/../.. ist die längere DS.
            return and_test_loop_up(xMSDptr,yMSDptr,xlen);
    }     }

# Prüft, ob (LOGBITP x y), wo x und y Integers sind.
# I_I_logbitp(x,y)
# Ergebnis: /=0, wenn ja; =0, wenn nein.
  local boolean I_I_logbitp (object x, object y);
  local boolean I_I_logbitp(x,y)
    var reg4 object x;
    var reg5 object y;
    # Methode:
    # Falls x<0, Error.
    # Falls x>=0: Falls x>=intDsize*Länge(y), teste Vorzeichen von y.
    #             Sonst x=intDsize*k+i, Teste Bit i vom Worte Nr. k+1 (von oben herab).
    { if (!R_minusp(x)) # x>=0 ?
        { if (I_bignump(x)) goto sign;
         {var reg1 uintL x_ = posfixnum_to_L(x);
          var reg2 uintC ylen;
          var reg3 uintD* yLSDptr;
          I_to_NDS_nocopy(y, ,ylen=,yLSDptr=); # DS zu y
          if (x_ < intDsize*(uintL)ylen)
            # x ist ein Fixnum >=0, < intDsize*ylen
            { if (yLSDptr[-floor(x_,intDsize)-1] & bit(x_%intDsize))
                return TRUE;
                else
                return FALSE;
            }
            else
            sign: # Vorzeichen von y testen
            { if (R_minusp(y))
                return TRUE;
                else
                return FALSE;
            }
        }}
        else
        # x<0
        { pushSTACK(x); pushSTACK(S(logbitp));
          fehler(
                 DEUTSCH ? "~: Negativer Index: ~" :
                 ENGLISH ? "~: index ~ is negative" :
                 FRANCAIS ? "~ : Index négatif: ~" :
                 ""
                );
    }   }

# Prüft, ob (ODDP x), wo x ein Integer ist.
# I_oddp(x)
# Ergebnis: /=0, falls ja; =0, falls nein.
  local boolean I_oddp (object x);
  local boolean I_oddp(x)
    var reg1 object x;
    { if (I_fixnump(x))
        # Fixnum: Bit 0 abprüfen
        { if ((oint)x & wbit(0+oint_addr_shift))
            return TRUE;
            else
            return FALSE;
        }
        else
        # Bignum: Bit 0 im letzten Digit abprüfen
        { var reg1 Bignum x_ = TheBignum(x);
          if (x_->data[(uintL)(x_->length)-1] & bit(0))
            return TRUE;
            else
            return FALSE;
    }   }

# (ASH x y), wo x und y Integers sind. Ergebnis Integer.
# I_I_ash_I(x,y)
# kann GC auslösen
  global object I_I_ash_I (object x, object y);
  global object I_I_ash_I(x,y)
    var reg3 object x;
    var reg4 object y;
    # Methode:
    # x = 0 -> 0 als Ergebnis
    # y = 0 -> x als Ergebnis
    # y > 0 -> y = intDsize*k + i, j=k+(1 falls i>0, 0 falls i=0).
    #          j Wörter mehr reservieren, k Nullwörter, dann übertragen,
    #          bei i>0: um i Bits links schieben (i=1 geht einfacher).
    # y < 0 -> y <= - intDsize * (Länge(A0) in Digits) -> Ergebnis = 0 oder -1.
    #          Sonst: -y = intDsize*k + i mit k<Länge(A0).
    #                  Übertrage die (Länge(A0)-k) MSDigits,
    #                  falls i>0: schiebe sie um i Bits nach rechts (i=1 geht einfacher).
    { if (eq(x,Fixnum_0)) return x; # x=0 -> 0 als Ergebnis
      else
      if (eq(y,Fixnum_0)) return x; # y=0 -> x als Ergebnis
      else
     {SAVE_NUM_STACK # num_stack retten
      if (!(R_minusp(y)))
        # y>0
        if (I_bignump(y) # y ein Bignum
            || ((log2_intDsize+intCsize < oint_addr_len) # intDsize*2^intCsize < 2^oint_addr_len ?
                && ((oint)y >= (oint)fixnum(intDsize*bitc(intCsize))) # ein Fixnum > Bitlänge aller Integers
           )   )
          # y so groß, daß selbst (ASH 1 y) einen Überlauf gäbe.
          goto badamount;
          else
          { var reg2 uintL y_ = ((oint)y-(oint)Fixnum_0)>>oint_addr_shift; # Wert von y, >=0, <intDsize*2^intCsize
            var reg2 uintL i = y_%intDsize; # i = y mod intDsize, >=0, <intDsize
            var reg2 uintL k = floor(y_,intDsize); # k = y div intDsize, >=0, <2^intCsize
            var reg5 uintD* LSDptr;
            var reg6 uintC len;
            var reg7 uintD* x_LSDptr;
            I_to_NDS_nocopy(x, ,len=,x_LSDptr=); # DS zu x bilden.
            if (len >= (uintC)(~(uintC)k)) # kann len+k+1 Überlauf geben?
              goto badamount; # ja -> Fehler
            num_stack_need_1(len+(uintC)k,,LSDptr=);
            LSDptr = clear_loop_down(LSDptr,k); # k Nulldigits
           {var reg5 uintD* MSDptr = copy_loop_down(x_LSDptr,LSDptr,len);
            # Nun ist MSDptr/len/LSDptr die DS zu x.
            # Oberhalb von ihr liegen k Nulldigits, unterhalb ist 1 Digit Platz.
            # MSDptr/len+k/.. ist jetzt die Gesamt-DS.
            # Noch um i Bits nach links schieben:
            if (!(i==0)) # Bei i>0
              { # noch ein weiteres Digit dazunehmen (Vorzeichen)
                {var reg1 uintD sign = sign_of_sintD(MSDptr[0]);
                 *--MSDptr = sign;
                 len++;
                }
                # Schiebeschleife: die unteren len Digits um i Bits schieben
                if (i==1)
                  { shift1left_loop_down(LSDptr,len); }
                  else
                  { shiftleft_loop_down(LSDptr,len,i,0); }
              }
            x = DS_to_I(MSDptr,len+(uintC)k);
          }}
        else
        # y<0
        if (I_bignump(y)) goto sign; # y ein Bignum -> Vorzeichen von x zurück
          else
          { var reg2 uintL y_ = (((oint)Fixnum_minus1-(oint)y)>>oint_addr_shift)+1; # Wert von -y, >0
            var reg2 uintL i = y_%intDsize; # i = (-y) mod intDsize, >=0, <intDsize
            var reg2 uintL k = floor(y_,intDsize); # k = (-y) div intDsize, >=0
            # DS zu x bilden:
            var reg5 uintD* MSDptr;
            var reg6 uintC len;
            I_to_NDS(x, MSDptr=,len=,); # DS zu x bilden.
            if (k>=len) goto sign; # -y >= intDsize*len -> Vorzeichen von x zurück
            len -= k; # rechte k Digits einfach streichen
            # Noch ist len>0. Um i Bits nach rechts schieben:
            if (!(i==0)) # Bei i>0:
              { # Schiebe len Digits ab MSDptr um i Bits nach rechts:
                if (i==1)
                  { shift1right_loop_up(MSDptr,len,sign_of_sintD(MSDptr[0])); }
                  else
                  { shiftrightsigned_loop_up(MSDptr,len,i); }
              }
            x = DS_to_I(MSDptr,len);
          }
      if (FALSE)
        sign: # Ergebnis ist 0, falls x>=0, und -1, falls x<0:
        { x = (R_minusp(x) ? Fixnum_minus1 : Fixnum_0 ); }
      RESTORE_NUM_STACK # num_stack zurück
      return x;
      badamount:
        pushSTACK(y); pushSTACK(S(ash));
        fehler(
               DEUTSCH ? "~: Zu große Schiebezahl ~" :
               ENGLISH ? "~: too large shift amount ~" :
               FRANCAIS ? "~ : Décalage ~ trop grand" :
               ""
              );
    }}

# (LOGCOUNT x), wo x ein Integer ist. Ergebnis Integer >=0.
# I_logcount_I(x)
# kann GC auslösen
  local object I_logcount_I (object x);
  # Bits von x8 zählen: (Input x8, Output x8)
  #define logcount_8()  \
    ( # x8 besteht aus 8 1-Bit-Zählern (0,1).       \
      x8 = (x8 & 0x55U) + ((x8 & 0xAAU) >> 1),      \
      # x8 besteht aus 4 2-Bit-Zählern (0,1,2).     \
      x8 = (x8 & 0x33U) + ((x8 & 0xCCU) >> 2),      \
      # x8 besteht aus 2 4-Bit-Zählern (0,1,2,3,4). \
      x8 = (x8 & 0x0FU) + (x8 >> 4)                 \
      # x8 besteht aus 1 8-Bit-Zähler (0,...,8).    \
    )
  # Bits von x16 zählen: (Input x16, Output x16)
  #define logcount_16()  \
    ( # x16 besteht aus 16 1-Bit-Zählern (0,1).       \
      x16 = (x16 & 0x5555U) + ((x16 & 0xAAAAU) >> 1), \
      # x16 besteht aus 8 2-Bit-Zählern (0,1,2).      \
      x16 = (x16 & 0x3333U) + ((x16 & 0xCCCCU) >> 2), \
      # x16 besteht aus 4 4-Bit-Zählern (0,1,2,3,4).  \
      x16 = (x16 & 0x0F0FU) + ((x16 & 0xF0F0U) >> 4), \
      # x16 besteht aus 2 8-Bit-Zählern (0,...,8).    \
      x16 = (x16 & 0x00FFU) + (x16 >> 8)              \
      # x16 besteht aus 1 16-Bit-Zähler (0,...,16).   \
    )
  # Bits von x32 zählen: (Input x32, Output x16)
  #define logcount_32()  \
    ( # x32 besteht aus 32 1-Bit-Zählern (0,1).                 \
      x32 = (x32 & 0x55555555UL) + ((x32 & 0xAAAAAAAAUL) >> 1), \
      # x32 besteht aus 16 2-Bit-Zählern (0,1,2).               \
      x32 = (x32 & 0x33333333UL) + ((x32 & 0xCCCCCCCCUL) >> 2), \
      # x32 besteht aus 8 4-Bit-Zählern (0,1,2,3,4).            \
      x16 = high16(x32)+low16(x32),                             \
      # x16 besteht aus 4 4-Bit-Zählern (0,...,8).              \
      x16 = (x16 & 0x0F0FU) + ((x16 & 0xF0F0U) >> 4),           \
      # x16 besteht aus 2 8-Bit-Zählern (0,...,16).             \
      x16 = (x16 & 0x00FFU) + (x16 >> 8)                        \
      # x16 besteht aus 1 16-Bit-Zähler (0,...,32).             \
    )
  #if (intWLsize==intLsize)
    #define x16  x32
  #endif
  local object I_logcount_I(x)
    var reg3 object x;
    { if (I_fixnump(x))
        { var reg1 uint16 x16; # Hilfsvariable
         {var reg1 uint32 x32 = FN_to_L(x); # x als 32-Bit-Zahl
          if (FN_L_minusp(x,(sint32)x32)) { x32 = ~ x32; } # falls <0, komplementieren
          logcount_32(); # Bits von x32 zählen
          return fixnum((uintL)x16);
        }}
        else
        { var reg6 uintD* MSDptr;
          var reg3 uintC len;
          BN_to_NDS_nocopy(x, MSDptr=,len=,); # DS zu x bilden, len>0.
         {var reg4 uintL bitcount = 0; # Bitzähler
          var reg2 uintD* ptr = MSDptr; # läuft durch die Digits durch
          var reg5 uintD sign = sign_of_sintD(ptr[0]); # Vorzeichen
          #if (intDsize==8)
          dotimespC(len,len,
            { var reg1 uintD x8 = (*ptr++) ^ sign; # nächstes intDsize-Bit-Paket,
                                    # bei negativen Zahlen komplementiert
              # Bits von x8 zählen, Gesamtzähler erhöhen:
              bitcount += (uintL)(logcount_8(), x8);
            });
          #endif
          #if (intDsize==16)
          dotimespC(len,len,
            { var reg1 uintD x16 = (*ptr++) ^ sign; # nächstes intDsize-Bit-Paket,
                                    # bei negativen Zahlen komplementiert
              # Bits von x16 zählen, Gesamtzähler erhöhen:
              bitcount += (uintL)(logcount_16(), x16);
            });
          #endif
          #if (intDsize==32)
          dotimespC(len,len,
            { var reg1 uint16 x16; # Hilfsvariable
             {var reg1 uintD x32 = (*ptr++) ^ sign; # nächstes intDsize-Bit-Paket,
                                    # bei negativen Zahlen komplementiert
              # Bits von x32 zählen, Gesamtzähler erhöhen:
              bitcount += (uintL)(logcount_32(), x16);
            }});
          #endif
          # 0 <= bitcount < intDsize*2^intCsize, paßt evtl. in ein Fixnum.
          if (log2_intDsize+intCsize<=oint_addr_len) # intDsize*2^intCsize <= 2^oint_addr_len ?
            return fixnum(bitcount);
            else
            return UL_to_I(bitcount);
    }   }}
  #undef x16
  #undef logcount_32
  #undef logcount_16
  #undef logcount_8

# Bits eines Digit zählen:
# integerlengthD(digit,size=);
# setzt size auf die höchste in digit vorkommende Bitnummer.
# > digit: ein uintD >0
# < size: >0, <=intDsize, mit 2^(size-1) <= digit < 2^size
#if defined(GNU) && defined(MC680Y0)
  #define integerlength8(digit,size_zuweisung)  \
    { var reg1 uintL zero_counter; # zählt die führenden Nullbits in digit          \
      __asm__("bfffo %1{#0:#8},%0" : "=d" (zero_counter) : "dm" ((uint8)(digit)) ); \
      size_zuweisung (8-zero_counter);                                              \
    }
#elif defined(SPARC)
  #define integerlength8(digit,size_zuweisung)  \
    integerlength32((uint32)(digit),size_zuweisung) # siehe unten
#elif defined(GNU) && defined(I80Z86)
  #define integerlength8(digit,size_zuweisung)  \
    { var reg1 uintL one_position; # Position der führenden 1              \
      __asm__("bsrb %1,%0" : "=r" (one_position) : "r" ((uint8)(digit)) ); # ?? \
      size_zuweisung (1+one_position);                                     \
    }
#else
  #define integerlength8(digit,size_zuweisung)  \
    { var reg2 uintC bitsize = 1;                            \
      var reg1 uintBWL x8 = (uint8)(digit);                  \
      # x8 hat höchstens 8 Bits.                             \
      if (x8 >= bit(4)) { x8 = x8>>4; bitsize += 4; }        \
      # x8 hat höchstens 4 Bits.                             \
      if (x8 >= bit(2)) { x8 = x8>>2; bitsize += 2; }        \
      # x8 hat höchstens 2 Bits.                             \
      if (x8 >= bit(1)) { /* x8 = x8>>1; */ bitsize += 1; }  \
      # x8 hat höchstens 1 Bit. Dieses Bit muß gesetzt sein. \
      size_zuweisung bitsize;                                \
    }
#endif
#if defined(GNU) && defined(MC680Y0)
  #define integerlength16(digit,size_zuweisung)  \
    { var reg1 uintL zero_counter; # zählt die führenden Nullbits in digit            \
      __asm__("bfffo %1{#0:#16},%0" : "=d" (zero_counter) : "dm" ((uint16)(digit)) ); \
      size_zuweisung (16-zero_counter);                                               \
    }
#elif defined(SPARC)
  #define integerlength16(digit,size_zuweisung)  \
    integerlength32((uint32)(digit),size_zuweisung) # siehe unten
#elif defined(GNU) && defined(I80Z86)
  #define integerlength16(digit,size_zuweisung)  \
    { var reg1 uintL one_position; # Position der führenden 1               \
      __asm__("bsrw %1,%0" : "=r" (one_position) : "r" ((uint16)(digit)) ); # ?? \
      size_zuweisung (1+one_position);                                      \
    }
#else
  #define integerlength16(digit,size_zuweisung)  \
    { var reg2 uintC bitsize = 1;                              \
      var reg1 uintWL x16 = (uint16)(digit);                   \
      # x16 hat höchstens 16 Bits.                             \
      if (x16 >= bit(8)) { x16 = x16>>8; bitsize += 8; }       \
      # x16 hat höchstens 8 Bits.                              \
      if (x16 >= bit(4)) { x16 = x16>>4; bitsize += 4; }       \
      # x16 hat höchstens 4 Bits.                              \
      if (x16 >= bit(2)) { x16 = x16>>2; bitsize += 2; }       \
      # x16 hat höchstens 2 Bits.                              \
      if (x16 >= bit(1)) { /* x16 = x16>>1; */ bitsize += 1; } \
      # x16 hat höchstens 1 Bit. Dieses Bit muß gesetzt sein.  \
      size_zuweisung bitsize;                                  \
    }
#endif
#if defined(GNU) && defined(MC680Y0)
  #define integerlength32(digit,size_zuweisung)  \
    { var reg1 uintL zero_counter; # zählt die führenden Nullbits in digit            \
      __asm__("bfffo %1{#0:#32},%0" : "=d" (zero_counter) : "dm" ((uint32)(digit)) ); \
      size_zuweisung (32-zero_counter);                                               \
    }
#elif defined(SPARC) && defined(FAST_DOUBLE)
  #define integerlength32(digit,size_zuweisung)  \
    {var union { double f; uint32 i[2]; } __fi;                     \
     # Bilde 2^52 + digit:                                          \
     __fi.i[0] = (uint32)(DF_mant_len+1+DF_exp_mid) << (DF_mant_len-32); # Vorzeichen 0, Exponent 53 \
     __fi.i[1] = (digit); # untere 32 Bits setzen (benutzt BIG_ENDIAN_P !) \
     # subtrahiere 2^52:                                            \
     __fi.f = __fi.f - (double)(4503599627370496.0L);               \
     # Hole davon den Exponenten:                                   \
     size_zuweisung ((__fi.i[0] >> (DF_mant_len-32)) - DF_exp_mid); \
    }
#elif defined(GNU) && defined(I80Z86)
  #define integerlength32(digit,size_zuweisung)  \
    { var reg1 uintL one_position; # Position der führenden 1               \
      __asm__("bsrl %1,%0" : "=r" (one_position) : "r" ((uint32)(digit)) ); \
      size_zuweisung (1+one_position);                                      \
    }
#elif defined(HPPA)
  #define integerlength32(digit,size_zuweisung)  \
    size_zuweisung length32(digit);
  extern uintL length32 (uintL digit); # extern in Assembler
#else
  #if (intWLsize==intLsize)
    #define integerlength32(digit,size_zuweisung)  \
      { var reg2 uintC bitsize = 1;                              \
        var reg1 uintL x32 = (uint32)(digit);                    \
        # x32 hat höchstens 32 Bits.                             \
        if (x32 >= bit(16)) { x32 = x32>>16; bitsize += 16; }    \
        # x32 hat höchstens 16 Bits.                             \
        if (x32 >= bit(8)) { x32 = x32>>8; bitsize += 8; }       \
        # x32 hat höchstens 8 Bits.                              \
        if (x32 >= bit(4)) { x32 = x32>>4; bitsize += 4; }       \
        # x32 hat höchstens 4 Bits.                              \
        if (x32 >= bit(2)) { x32 = x32>>2; bitsize += 2; }       \
        # x32 hat höchstens 2 Bits.                              \
        if (x32 >= bit(1)) { /* x32 = x32>>1; */ bitsize += 1; } \
        # x32 hat höchstens 1 Bit. Dieses Bit muß gesetzt sein.  \
        size_zuweisung bitsize;                                  \
      }
  #else
    #define integerlength32(digit,size_zuweisung)  \
      { var reg3 uintC bitsize = 1;                              \
        var reg2 uintL x32 = (digit);                            \
        var reg1 uintWL x16;                                     \
        # x32 hat höchstens 32 Bits.                             \
        if (x32 >= bit(16)) { x16 = x32>>16; bitsize += 16; } else { x16 = x32; } \
        # x16 hat höchstens 16 Bits.                             \
        if (x16 >= bit(8)) { x16 = x16>>8; bitsize += 8; }       \
        # x16 hat höchstens 8 Bits.                              \
        if (x16 >= bit(4)) { x16 = x16>>4; bitsize += 4; }       \
        # x16 hat höchstens 4 Bits.                              \
        if (x16 >= bit(2)) { x16 = x16>>2; bitsize += 2; }       \
        # x16 hat höchstens 2 Bits.                              \
        if (x16 >= bit(1)) { /* x16 = x16>>1; */ bitsize += 1; } \
        # x16 hat höchstens 1 Bit. Dieses Bit muß gesetzt sein.  \
        size_zuweisung bitsize;                                  \
      }
  #endif
#endif
#if (intDsize==8)
  #define integerlengthD  integerlength8
#endif
#if (intDsize==16)
  #define integerlengthD  integerlength16
#endif
#if (intDsize==32)
  #define integerlengthD  integerlength32
#endif

# (INTEGER-LENGTH x), wo x ein Integer ist. Ergebnis uintL.
# I_integer_length(x)
  global uintL I_integer_length (object x);
  global uintL I_integer_length(x)
    var reg3 object x;
    { if (I_fixnump(x))
        { var reg2 uintL bitcount = 0;
          var reg1 uint32 x_ = FN_to_L(x); # x als 32-Bit-Zahl
          if (FN_L_minusp(x,(sint32)x_)) { x_ = ~ x_; } # falls <0, komplementieren
          if (!(x_==0)) { integerlength32(x_,bitcount=); }
          return bitcount; # 0 <= bitcount < 32.
        }
        else
        { var reg4 uintD* MSDptr;
          var reg5 uintC len;
          BN_to_NDS_nocopy(x, MSDptr=,len=,); # normalisierte DS zu x bilden.
         {var reg2 uintL bitcount = intDsize*(uintL)(len-1); # Anzahl Digits mal intDsize
          # MSDigit nehmen, testen, welches das höchste Bit ist, das vom
          # Vorzeichenbit abweicht:
          var reg1 uintD msd = MSDptr[0]; # MSDigit
          if ((sintD)msd < 0) { msd = ~msd; } # falls negativ, invertieren
          # Position des höchsten Bits in msd suchen und entsprechend bit_count
          # erhöhen (um höchstens intDsize-1):
          if (!(msd == 0)) { integerlengthD(msd, bitcount += ); }
          return bitcount; # 0 <= bitcount < intDsize*2^intCsize.
    }   }}

# (INTEGER-LENGTH x), wo x ein Integer ist. Ergebnis Integer >=0.
# I_integer_length_I(x)
# kann GC auslösen
  local object I_integer_length_I (object x);
  local object I_integer_length_I(x)
    var reg3 object x;
    { if (I_fixnump(x))
        { var reg2 uintL bitcount = 0;
          var reg1 uint32 x_ = FN_to_L(x); # x als 32-Bit-Zahl
          if (FN_L_minusp(x,(sint32)x_)) { x_ = ~ x_; } # falls <0, komplementieren
          if (!(x_==0)) { integerlength32(x_,bitcount=); }
          # 0 <= bitcount < 32, paßt in ein Fixnum.
          return fixnum(bitcount);
        }
        else
        { var reg4 uintD* MSDptr;
          var reg5 uintC len;
          BN_to_NDS_nocopy(x, MSDptr=,len=,); # normalisierte DS zu x bilden.
         {var reg2 uintL bitcount = intDsize*(uintL)(len-1); # Anzahl Digits mal intDsize
          # MSDigit nehmen, testen, welches das höchste Bit ist, das vom
          # Vorzeichenbit abweicht:
          var reg1 uintD msd = MSDptr[0]; # MSDigit
          if ((sintD)msd < 0) { msd = ~msd; } # falls negativ, invertieren
          # Position des höchsten Bits in msd suchen und entsprechend bit_count
          # erhöhen (um höchstens intDsize-1):
          if (!(msd == 0)) { integerlengthD(msd, bitcount += ); }
          # 0 <= bitcount < intDsize*2^intCsize, paßt evtl. in ein Fixnum.
          if (log2_intDsize+intCsize<=oint_addr_len) # intDsize*2^intCsize <= 2^oint_addr_len ?
            return fixnum(bitcount);
            else
            return UL_to_I(bitcount);
    }   }}

# Hintere Nullbits eines 32-Bit-Wortes zählen:
# ord2_32(digit,count=);
# setzt size auf die kleinste in digit vorkommende Bitnummer.
# > digit: ein uint32 >0
# < count: >=0, <32, mit 2^count | digit, digit/2^count ungerade
  #if defined(GNU) && defined(I80Z86) && (intDsize==32)
    #define ord2_32(digit,count_zuweisung)  \
      { var reg1 uintL one_position; # Position der letzten 1                 \
        __asm__("bsfl %1,%0" : "=r" (one_position) : "r" ((uint32)(digit)) ); \
        count_zuweisung one_position;                                         \
      }
  #endif

# (ORD2 x) = max{n>=0: 2^n | x }, wo x ein Integer /=0 ist. Ergebnis uintL.
# I_ord2(x)
  local uintL I_ord2 (object x);
# Methode 1a:
#   Sei n = ord2(x). Dann ist logxor(x,x-1) = 2^n + (2^n-1) = 2^(n+1)-1.
#   Also  (ord2 x) = (1- (integer-length (logxor x (1- x)))) .
# Methode 1b:
#   Sei n = ord2(x). Dann ist logand(x,-x) = 2^n.
#   Also  (ord2 x) = (1- (integer-length (logand x (- x)))) .
# Methode 1c:
#   Sei n = ord2(x). Dann ist lognot(logior(x,-x)) = 2^n-1.
#   Also  (ord2 x) = (integer-length (lognot (logior x (- x)))) .
# Methode 2:
#   Nullbits am Schluß von x abzählen:
#   (ord2 x) = intDsize * Anzahl der Nulldigits am Schluß
#              + Anzahl der Nullbits am Ende des letzten Digits /=0.
  #ifndef ord2_32
    # Hier muß digit eine Variable sein. digit wird verändert!
    #define ord2_32(digit,count_zuweisung)  \
      digit = digit ^ (digit - 1); # Methode 1a \
      integerlength32(digit,count_zuweisung -1 + )
  #endif
  local uintL I_ord2(x)
    var reg4 object x;
    { if (I_fixnump(x))
        { var reg1 uint32 x_ = FN_to_L(x); # x als 32-Bit-Zahl
          #if (oint_addr_len < 32)
          ord2_32(x_,return);
          #else # oint_addr_len=32, x_ kann auch =0 sein.
          # Bei x = most-negative-fixnum funktioniert nur Methode 1c.
          x_ = x_ | (- x_); x_ = ~ x_;
          integerlength32(x_,return);
          #endif
        }
        else
        { var reg2 uintL bitcount = 0;
          var reg1 uintD* ptr;
          BN_to_NDS_nocopy(x, ,,ptr=); # normalisierte DS zu x bilden.
          while (*--ptr == 0) { bitcount += intDsize; } # Nulldigits abzählen
         {var reg3 uintD lsd = *ptr; # letztes Digit /=0
          #if (intDsize==32)
          ord2_32(lsd,bitcount +=);
          #else
          lsd = lsd ^ (lsd - 1);
          integerlengthD(lsd, bitcount += -1 + ); # dessen Nullbits abzählen
          #endif
          return bitcount;
        }}
    }

# I_power2p(x) stellt fest, ob ein Integer x>0 eine Zweierpotenz ist.
# Ergebnis: n>0, wenn x=2^(n-1), 0 sonst.
  local uintL I_power2p (object x);
# Methode 1: Wenn ord2(x) = integer_length(x)-1.
# Methode 2: Wenn logand(x,x-1) = 0.
# Methode 3: Wenn das erste Digit /=0 eine Zweierpotenz ist und alle weiteren
#            Digits Null sind.
  local uintL I_power2p(x)
    var reg2 object x;
    { if (I_fixnump(x))
        { var reg1 uintL x_ = posfixnum_to_L(x);
          if (!((x_ & (x_-1)) == 0)) return 0; # keine Zweierpotenz
          integerlength32(x_,return); # Zweierpotenz: n = integer_length(x)
        }
        else
        { var reg1 uintD* MSDptr;
          var reg4 uintC len;
          BN_to_NDS_nocopy(x, MSDptr=,len=,); # normalisierte DS zu x bilden.
         {var reg3 uintD msd = MSDptr[0];
          if (msd==0) { MSDptr++; msd = MSDptr[0]; len--; }
          # len = Anzahl der Digits ab MSDptr, len>0, msd = erstes Digit (/=0)
          if (!((msd & (msd-1)) == 0)) return 0; # erstes Digit muß Zweierpotenz sein
          if (test_loop_up(&MSDptr[1],len-1)) return 0; # danach alles Nullen
          {var reg5 uintL msdlen;
           integerlengthD(msd, msdlen=);
           return intDsize*(uintL)len + msdlen; # integer_length(x) als Ergebnis
        }}}
    }

