# Addition/Subtraktion von Integers

# Macro: In der DS MSDptr/len/LSDptr wird eine 1 unterhalb des Pointers ptr
# addiert. Unterhalb von MSDptr muß 1 Digit Platz sein.
# Dabei ist  ptr - MSDptr = count  und  0 < count <= len .
# Eventuell wird MSDptr erniedrigt und len erhöht.
  #define DS_1_plus(ptr,count)  \
    {var reg1 uintD* ptr_from_DS_1_plus = (ptr);                    \
     var reg2 uintC count_from_DS_1_plus = (count);                 \
     loop { if (--count_from_DS_1_plus==0) # Zähler erniedrigen     \
              { # Beim Most Significant Digit angelangt             \
                *(--ptr_from_DS_1_plus) += 1;                       \
                # jetzt ist ptr_from_DS_1_plus = MSDptr             \
                if (*ptr_from_DS_1_plus == bit(intDsize-1))         \
                  { # 7FFF + 1 muß zu 00008000 werden:              \
                    *--MSDptr = 0;                                  \
                    len++; if (uintCoverflow(len)) BN_ueberlauf();  \
                  }                                                 \
                break;                                              \
              }                                                     \
            if (!((*(--ptr_from_DS_1_plus) += 1) == 0)) # weiterincrementieren \
              break; # kein weiterer Übertrag -> Schleife abbrechen \
    }     }

# Macro: In der DS MSDptr/len/LSDptr wird eine 1 unterhalb des Pointers ptr
# subtrahiert. Unterhalb von MSDptr muß 1 Digit Platz sein.
# Dabei ist  ptr - MSDptr = count  und  0 < count <= len .
# Eventuell wird MSDptr erniedrigt und len erhöht.
  #define DS_minus1_plus(ptr,count)  \
    {var reg1 uintD* ptr_from_DS_minus1_plus = (ptr);                \
     var reg2 uintC count_from_DS_minus1_plus = (count);             \
     loop { if (--count_from_DS_minus1_plus==0) # Zähler erniedrigen \
              { # Beim Most Significant Digit angelangt              \
                *(--ptr_from_DS_minus1_plus) -= 1;                   \
                # jetzt ist ptr_from_DS_minus1_plus = MSDptr         \
                if (*ptr_from_DS_minus1_plus == bit(intDsize-1)-1)   \
                  { # 8000 - 1 muß zu FFFF7FFF werden:               \
                    *--MSDptr = -1;                                  \
                    len++; if (uintCoverflow(len)) BN_ueberlauf();   \
                  }                                                  \
                break;                                               \
              }                                                      \
            if (!((sintD)(*(--ptr_from_DS_minus1_plus) -= 1) == -1)) # weiterdecrementieren \
              break; # kein weiterer Übertrag -> Schleife abbrechen  \
    }     }

# (1+ x), wo x ein Integer ist. Ergebnis Integer.
# kann GC auslösen
  global object I_1_plus_I (object x);
  global object I_1_plus_I(x)
    var reg3 object x;
    { if (I_fixnump(x))
        { # x ist Fixnum
          if (x==Fixnum_minus1) { return Fixnum_0; } # (1+ -1) = 0
          if (!(x==Fixnum_mpos)) { return fixnum_inc(x,1); } # bleibt Fixnum: direkt 1 addieren
        }
      # die sichere Methode
      { SAVE_NUM_STACK # num_stack retten
        var uintD* MSDptr;
        var uintC len;
        var uintD* LSDptr;
        I_to_NDS_1(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden.
        DS_1_plus(LSDptr,len); # zur NDS 1 addieren
        RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
        return DS_to_I(MSDptr,len); # wieder zum Integer machen
    } }

# (1- x), wo x ein Integer ist. Ergebnis Integer.
# kann GC auslösen
  global object I_minus1_plus_I (object x);
  global object I_minus1_plus_I(x)
    var reg3 object x;
    { if (I_fixnump(x))
        { # x ist Fixnum
          if (x==Fixnum_0) { return Fixnum_minus1; } # (1- 0) = -1
          if (!(x==Fixnum_mneg)) { return fixnum_inc(x,-1); } # bleibt Fixnum: direkt 1 subtrahieren
        }
      # die sichere Methode
      { SAVE_NUM_STACK # num_stack retten
        var uintD* MSDptr;
        var uintC len;
        var uintD* LSDptr;
        I_to_NDS_1(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden.
        DS_minus1_plus(LSDptr,len); # von der NDS 1 subtrahieren
        RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
        return DS_to_I(MSDptr,len); # wieder zum Integer machen
    } }

# (+ x y), wo x und y Integers sind. Ergebnis Integer.
# kann GC auslösen
  global object I_I_plus_I (object x, object y);
  global object I_I_plus_I(x,y)
    var reg3 object x;
    var reg3 object y;
    # Methode:
    # x Fixnum ->
    #   y Fixnum -> beide direkt addieren, mit L_to_I beenden
    #   y Bignum -> falls x=0, y; sonst beide zu DS machen, addieren.
    # x Bignum ->
    #   y Fixnum -> falls y=0, x; sonst beide zu DS machen, addieren.
    #   y Bignum -> beide zu DS machen, addieren.
    { var reg4 uintD* MSDptr;
      var reg4 uintC len;
      var reg4 uintD* LSDptr;
      # MSDptr/len/LSDptr bilden die DS des Ergebnisses.
      if (I_fixnump(x))
        { # x ist Fixnum
          if (I_fixnump(y))
            { # x,y sind Fixnums
              #if (oint_addr_len+1 < intLsize)
              return L_to_I( FN_to_L(x) + FN_to_L(y) ); # als 32-Bit-Zahlen addieren
              #else
              var reg2 sint32 xhi = R_sign(x);
              var reg1 uint32 xlo = FN_to_L(x);
              var reg5 sint32 yhi = R_sign(y);
              var reg4 uint32 ylo = FN_to_L(y);
              xhi += yhi;
              xlo += ylo;
              if (xlo < ylo) { xhi += 1; }
              return L2_to_I(xhi,xlo);
              #endif
            }
            else
            { # x ist Fixnum, y ist Bignum
             {var reg1 object h; h = x; x = y; y = h; } # x und y vertauschen
             goto xBN_yFN;
            }
        }
        else
        { # x ist Bignum
          if (I_fixnump(y))
            xBN_yFN:
            { # x ist Bignum, y ist Fixnum, also x länger
              var reg2 sint32 y_ = FN_to_L(y); # Wert von y
              if (FN_L_zerop(y,y_)) { return x; } # bei y=0 Ergebnis x
             {SAVE_NUM_STACK # num_stack retten
              BN_to_NDS_1(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden.
              # len>=bn_minlength. len>pFN_maxlength erzwingen:
              if ((bn_minlength==pFN_maxlength) && (len==pFN_maxlength))
                { var reg1 sintD sign = sign_of_sintD(MSDptr[0]);
                  *--MSDptr = sign; len++;
                }
              # y_ zu den oberen pFN_maxlength Digits von x addieren:
              { var reg2 uintD* midptr = &LSDptr[-pFN_maxlength];
                var reg1 uint32 x_ = pFN_maxlength_digits_at(midptr);
                var reg1 uint32 x_new = x_+(uint32)y_;
                set_pFN_maxlength_digits_at(midptr,x_new);
                if (x_new < x_)
                  { # Carry.
                    if (!FN_L_minusp(y,y_)) # kürzerer Summand war positiv
                      # Dann ist ein positiver Übertrag weiterzutragen
                      # (Beispiel: 0002FFFC + 0007 = 00030003)
                      { DS_1_plus(midptr,len-pFN_maxlength); }
                  }
                  else
                  { # Kein Carry.
                    if (FN_L_minusp(y,y_)) # kürzerer Summand war negativ
                      # Dann ist ein negativer Übertrag weiterzutragen
                      # (Beispiel: 00020003 + FFF5 = 0001FFF8)
                      { DS_minus1_plus(midptr,len-pFN_maxlength); }
              }   }
              RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
              return DS_to_I(MSDptr,len); # DS wieder zum Integer machen
            }}
            else
            { # x und y sind Bignums
              SAVE_NUM_STACK # num_stack retten
              if (TheBignum(x)->length < TheBignum(y)->length)
                {var reg1 object h; h = x; x = y; y = h; } # x und y vertauschen
              # Nun ist x das längere von beiden.
              BN_to_NDS_1(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden.
             {var reg4 uintD* yMSDptr;
              var reg2 uintC ylen;
              var reg1 uintD* yLSDptr;
              BN_to_NDS_nocopy(y, yMSDptr=,ylen=,yLSDptr=); # NDS zu y bilden.
              # yMSDptr/ylen/yLSDptr bilden die DS des kürzeren Arguments y.
              # len>ylen erzwingen:
              if (len==ylen)
                { var reg1 sintD sign = sign_of_sintD(MSDptr[0]);
                  *--MSDptr = sign; len++; if (uintCoverflow(len)) BN_ueberlauf();
                }
              # addieren:
              { var reg4 uintD* midptr = LSDptr-(uintL)ylen;
                var reg5 uintD carry = addto_loop_down(yLSDptr,LSDptr,ylen);
                if (carry)
                  { # Carry.
                    if ((sintD)yMSDptr[0] >=0) # kürzerer Summand war positiv
                      # Dann ist ein positiver Übertrag weiterzutragen
                      # (Beispiel: 0002FFFC + 0007 = 00030003)
                      { DS_1_plus(midptr,len-ylen); }
                  }
                  else
                  { # Kein Carry.
                    if ((sintD)yMSDptr[0] <0) # kürzerer Summand war negativ
                      # Dann ist ein negativer Übertrag weiterzutragen
                      # (Beispiel: 00020003 + FFF5 = 0001FFF8)
                      { DS_minus1_plus(midptr,len-ylen); }
              }   }
              RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
              return DS_to_I(MSDptr,len); # DS wieder zum Integer machen
            }}
    }   }

# (- x), wenn x ein Integer ist. Ergebnis Integer.
# kann GC auslösen
  local object I_minus_I (object x);
  local object I_minus_I(x)
    var reg2 object x;
    { if (I_fixnump(x))
        { # Fixnum -> Long, negieren, -> Integer
          #if (oint_addr_len+1 < intLsize)
          return L_to_I(- FN_to_L(x));
          #else
          var reg3 sint32 xhi = R_sign(x);
          var reg1 uint32 xlo = FN_to_L(x);
          if (xlo==0) { xhi = -xhi; } else { xlo = -xlo; xhi = ~xhi; }
          return L2_to_I(xhi,xlo);
          #endif
        }
        else
        { # x Bignum
          SAVE_NUM_STACK # num_stack retten
          var reg4 uintD* MSDptr;
          var reg4 uintC len;
          var reg4 uintD* LSDptr;
          BN_to_NDS_1(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden, len>0
          # vorsorglich 1 Digit mehr belegen:
          { var reg1 sintD sign = sign_of_sintD(MSDptr[0]);
            *--MSDptr = sign; len++; if (uintCoverflow(len)) BN_ueberlauf();
          }
          # Negierschleife:
          neg_loop_down(LSDptr,len);
          # MSDigit ist nun = 0x0000 oder = 0xFFFF
          RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
          return DS_to_I(MSDptr,len); # DS wieder zum Integer machen
    }   }

# (- x y), wo x und y Integers sind. Ergebnis Integer.
# kann GC auslösen
  global object I_I_minus_I (object x, object y);
  global object I_I_minus_I(x,y)
    var reg3 object x;
    var reg3 object y;
    # Methode:
    # x Fixnum ->
    #   y Fixnum -> beide direkt subtrahieren, mit L_to_I beenden
    #   y Bignum -> falls x=0, (- y); sonst beide zu DS machen, subtrahieren.
    # x Bignum ->
    #   y Fixnum -> falls y=0, x; sonst beide zu DS machen, subtrahieren.
    #   y Bignum -> beide zu DS machen, subtrahieren.
    { var reg4 uintD* MSDptr;
      var reg4 uintC len;
      var reg4 uintD* LSDptr;
      # MSDptr/len/LSDptr bilden die DS des Ergebnisses.
      if (I_fixnump(x))
        { # x ist Fixnum
          if (I_fixnump(y))
            { # x,y sind Fixnums
              #if (oint_addr_len+1 < intLsize)
              return L_to_I( FN_to_L(x) - FN_to_L(y) ); # als 32-Bit-Zahlen subtrahieren
              #else
              var reg2 sint32 xhi = R_sign(x);
              var reg1 uint32 xlo = FN_to_L(x);
              var reg5 sint32 yhi = R_sign(y);
              var reg4 uint32 ylo = FN_to_L(y);
              xhi -= yhi;
              if (xlo < ylo) { xhi -= 1; }
              xlo -= ylo;
              return L2_to_I(xhi,xlo);
              #endif
            }
            else
            { # x ist Fixnum, y ist Bignum, also y länger
              var reg2 sint32 x_ = FN_to_L(x); # Wert von x
              if (FN_L_zerop(x,x_)) { return I_minus_I(y); } # bei x=0 Ergebnis (- y)
             {SAVE_NUM_STACK # num_stack retten
              BN_to_NDS_1(y, MSDptr=,len=,LSDptr=); # NDS zu y bilden.
              # vorsorglich 1 Digit mehr belegen:
              { var reg1 sintD sign = sign_of_sintD(MSDptr[0]);
                *--MSDptr = sign; len++; if (uintCoverflow(len)) BN_ueberlauf();
              }
              # Negierschleife:
              neg_loop_down(LSDptr,len);
              # MSDigit ist nun = 0x0000 oder = 0xFFFF
              # x_ zu den oberen pFN_maxlength Digits von -y addieren:
              { var reg2 uintD* midptr = &LSDptr[-pFN_maxlength];
                var reg1 uint32 y_ = pFN_maxlength_digits_at(midptr);
                var reg1 uint32 y_new = y_+(uint32)x_;
                set_pFN_maxlength_digits_at(midptr,y_new);
                if (y_new < y_)
                  { # Carry.
                    if (!FN_L_minusp(x,x_)) # kürzerer Summand war positiv
                      # Dann ist ein positiver Übertrag weiterzutragen
                      # (Beispiel: 0002FFFC + 0007 = 00030003)
                      { DS_1_plus(midptr,len-pFN_maxlength); }
                  }
                  else
                  { # Kein Carry.
                    if (FN_L_minusp(x,x_)) # kürzerer Summand war negativ
                      # Dann ist ein negativer Übertrag weiterzutragen
                      # (Beispiel: 00020003 + FFF5 = 0001FFF8)
                      { DS_minus1_plus(midptr,len-pFN_maxlength); }
              }   }
              RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
              return DS_to_I(MSDptr,len); # DS wieder zum Integer machen
            }}
        }
        else
        { # x ist Bignum
          if (I_fixnump(y))
            { # x ist Bignum, y ist Fixnum, also x länger
              var reg2 sint32 y_ = FN_to_L(y); # Wert von y
              if (FN_L_zerop(y,y_)) { return x; } # bei y=0 Ergebnis x
             {SAVE_NUM_STACK # num_stack retten
              BN_to_NDS_1(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden.
              # len>=bn_minlength. len>pFN_maxlength erzwingen:
              if ((bn_minlength==pFN_maxlength) && (len==pFN_maxlength))
                { var reg1 sintD sign = sign_of_sintD(MSDptr[0]);
                  *--MSDptr = sign; len++;
                }
              # y_ von den oberen pFN_maxlength Digits von x subtrahieren:
              { var reg2 uintD* midptr = &LSDptr[-pFN_maxlength];
                var reg1 uint32 x_ = pFN_maxlength_digits_at(midptr);
                var reg1 uint32 x_new = x_-(uint32)y_;
                set_pFN_maxlength_digits_at(midptr,x_new);
                if (x_new > x_) # bzw. (x_ < (uint32)y_), da y_>0
                  { # Carry.
                    if (!FN_L_minusp(y,y_)) # kürzerer Summand war positiv
                      # Dann ist ein negativer Übertrag weiterzutragen
                      # (Beispiel: 00030003 - 0007 = 0002FFFC)
                      { DS_minus1_plus(midptr,len-pFN_maxlength); }
                  }
                  else
                  { # Kein Carry.
                    if (FN_L_minusp(y,y_)) # kürzerer Summand war negativ
                      # Dann ist ein positiver Übertrag weiterzutragen
                      # (Beispiel: 0002FFF8 - FFF5 = 00030003)
                      { DS_1_plus(midptr,len-pFN_maxlength); }
              }   }
              RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
              return DS_to_I(MSDptr,len); # DS wieder zum Integer machen
            }}
            else
            { # x und y sind Bignums
              if (TheBignum(x)->length > TheBignum(y)->length)
                { # x das längere von beiden.
                  SAVE_NUM_STACK # num_stack retten
                  BN_to_NDS_1(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden.
                 {var reg4 uintD* yMSDptr;
                  var reg2 uintC ylen;
                  var reg1 uintD* yLSDptr;
                  BN_to_NDS_nocopy(y, yMSDptr=,ylen=,yLSDptr=); # NDS zu y bilden.
                  # yMSDptr/ylen/yLSDptr bilden die DS des kürzeren Arguments y.
                  # Es ist len>ylen.
                  # subtrahieren:
                  { var reg4 uintD* midptr = LSDptr-(uintL)ylen;
                    var reg5 uintD carry = subfrom_loop_down(yLSDptr,LSDptr,ylen);
                    if (carry)
                      { # Carry.
                        if ((sintD)yMSDptr[0] >=0) # kürzerer Summand war positiv
                          # Dann ist ein negativer Übertrag weiterzutragen
                          # (Beispiel: 00030003 - 0007 = 0002FFFC)
                          { DS_minus1_plus(midptr,len-ylen); }
                      }
                      else
                      { # Kein Carry.
                        if ((sintD)yMSDptr[0] <0) # kürzerer Summand war negativ
                          # Dann ist ein positiver Übertrag weiterzutragen
                          # (Beispiel: 0002FFF8 - FFF5 = 00030003)
                          { DS_1_plus(midptr,len-ylen); }
                  }   }
                  RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
                  return DS_to_I(MSDptr,len); # DS wieder zum Integer machen
                }}
                else
                { # y das längere von beiden.
                  SAVE_NUM_STACK # num_stack retten
                  BN_to_NDS_1(y, MSDptr=,len=,LSDptr=); # NDS zu y bilden.
                  # vorsorglich 1 Digit mehr belegen:
                  { var reg1 sintD sign = sign_of_sintD(MSDptr[0]);
                    *--MSDptr = sign; len++; if (uintCoverflow(len)) BN_ueberlauf();
                  }
                  # Negierschleife:
                  neg_loop_down(LSDptr,len);
                  # MSDigit ist nun = 0x0000 oder = 0xFFFF
                 {var reg4 uintD* xMSDptr;
                  var reg2 uintC xlen;
                  var reg1 uintD* xLSDptr;
                  BN_to_NDS_nocopy(x, xMSDptr=,xlen=,xLSDptr=); # NDS zu x bilden.
                  # xMSDptr/xlen/xLSDptr bilden die DS des kürzeren Arguments x.
                  # Es ist jetzt len>xlen.
                  # addieren:
                  { var reg4 uintD* midptr = LSDptr-(uintL)xlen;
                    var reg5 uintD carry = addto_loop_down(xLSDptr,LSDptr,xlen);
                    if (carry)
                      { # Carry.
                        if ((sintD)xMSDptr[0] >=0) # kürzerer Summand war positiv
                          # Dann ist ein positiver Übertrag weiterzutragen
                          # (Beispiel: 0002FFFC + 0007 = 00030003)
                          { DS_1_plus(midptr,len-xlen); }
                      }
                      else
                      { # Kein Carry.
                        if ((sintD)xMSDptr[0] <0) # kürzerer Summand war negativ
                          # Dann ist ein negativer Übertrag weiterzutragen
                          # (Beispiel: 00020003 + FFF5 = 0001FFF8)
                          { DS_minus1_plus(midptr,len-xlen); }
                  }   }
                  RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
                  return DS_to_I(MSDptr,len); # DS wieder zum Integer machen
                }}
    }   }   }

# (abs x), wenn x ein Integer ist. Ergebnis Integer.
# kann GC auslösen
  local object I_abs_I (object x);
  local object I_abs_I(x)
    var reg1 object x;
    { # Methode:
      # Bei x<0: (- x), sonst x.
      if (R_minusp(x)) return I_minus_I(x); else return x;
    }

