# Grundfunktionen für Long-Floats

# Fehlermeldung bei zu langen Long-FLoats
  local nonreturning void fehler_LF_toolong (void);
  local nonreturning void fehler_LF_toolong()
    { fehler(
             DEUTSCH ? "Zu lange Long-Floats" :
             ENGLISH ? "long float too long" :
             FRANCAIS ? "LONG-FLOAT trop long" :
             ""
            );
    }

# Entpacken eines Long-Float:
# LF_decode(obj, zero_statement, sign=,exp=,mantMSDptr=,mantlen=,mantLSDptr=);
# zerlegt ein Long-Float obj.
# Ist obj=0.0, wird zero_statement ausgeführt.
# Sonst: signean sign = Vorzeichen (0 = +, -1 = -),
#        sintL exp = Exponent (vorzeichenbehaftet),
#        UDS mantMSDptr/mantlen/mantLSDptr = Mantisse
#          (>= 2^(intDsize*mantlen-1), < 2^(intDsize*mantlen)),
#          mit mantlen>=LF_minlen.
  #define LF_decode(obj, zero_statement, sign_zuweisung,exp_zuweisung,mantMSDptr_zuweisung,mantlen_zuweisung,mantLSDptr_zuweisung)  \
    { var reg3 object _obj = (obj);                                               \
      var reg1 Lfloat _x = TheLfloat(_obj);                                       \
      var reg2 uintL uexp = _x->expo;                                             \
      if (uexp==0)                                                                \
        { mantlen_zuweisung _x->len; zero_statement } # e=0 -> Zahl 0.0           \
        else                                                                      \
        { exp_zuweisung (sintL)(uexp - LF_exp_mid); # Exponent                    \
          sign_zuweisung R_sign(_obj);              # Vorzeichen                  \
          mantMSDptr_zuweisung &(_x->data[0]);      # Mantissen-UDS               \
          mantLSDptr_zuweisung &(_x->data[(uintL)( mantlen_zuweisung _x->len )]); \
    }   }

# Einpacken eines Long-Float:
# encode_LF0(len,erg_zuweisung) liefert ein Long-Float 0.0 mit len Digits.
# > uintC len: Anzahl der Digits
# < object erg: neues Long-Float 0.0 mit len Digits
# kann GC auslösen
  #define encode_LF0(len,erg_zuweisung)  \
    { var reg2 uintC _len = (len);                                                 \
      var reg1 object _erg = allocate_lfloat(_len,0,0); # Exponent 0, Vorzeichen + \
      clear_loop_up(&TheLfloat(_erg)->data[0],_len); # Mantisse := 0               \
      erg_zuweisung _erg;                                                          \
    }

# Einpacken eines Long-Float:
# encode_LF1s(sign,len,erg_zuweisung) liefert ein Long-Float +-1.0 mit len Digits.
# > signean sign: Vorzeichen
# > uintC len: Anzahl der Digits
# < object erg: neues Long-Float +1.0 oder -1.0 mit len Digits
# kann GC auslösen
  #define encode_LF1s(sign,len,erg_zuweisung)  \
    { var reg2 uintC _len = (len);                                                   \
      var reg1 object _erg = allocate_lfloat(_len,LF_exp_mid+1,(sign)); # Exponent 1 \
      TheLfloat(_erg)->data[0] = bit(intDsize-1); # Mantisse := 2^(intDsize*len-1)   \
      clear_loop_up(&TheLfloat(_erg)->data[1],_len-1);                               \
      erg_zuweisung _erg;                                                            \
    }

# Einpacken eines Long-Float:
# encode_LF1(len,erg_zuweisung) liefert ein Long-Float 1.0 mit len Digits.
# > uintC len: Anzahl der Digits
# < object erg: neues Long-Float 1.0 mit len Digits
# kann GC auslösen
  #define encode_LF1(len,erg_zuweisung)  encode_LF1s(0,len,erg_zuweisung)

# Einpacken eines Long-Float:
# encode_LFu(sign,uexp,mantMSDptr,mantlen, erg_zuweisung) liefert ein Long-Float
# > signean sign: Vorzeichen
# > uintL exp: Exponent + LF_exp_mid
# > uintD* mantMSDptr: Pointer auf eine NUDS mit gesetztem höchstem Bit
# > uintC mantlen: Anzahl der Digits, >= LF_minlen
# < object erg: neues Long-Float mit der UDS mantMSDptr/mantlen/.. als Mantisse
# Der Exponent wird nicht auf Überlauf/Unterlauf getestet.
# kann GC auslösen
  #define encode_LFu(sign,uexp,mantMSDptr,mantlen,erg_zuweisung)  \
    { var reg2 uintC _len = (mantlen);                                                 \
      var reg1 object _erg = allocate_lfloat(_len,uexp,(sign)); # Exponent             \
      copy_loop_up((mantMSDptr),&TheLfloat(_erg)->data[0],_len); # Mantisse übertragen \
      erg_zuweisung _erg;                                                              \
    }

# Einpacken eines Long-Float:
# encode_LF(sign,exp,mantMSDptr,mantlen, erg_zuweisung) liefert ein Long-Float
# > signean sign: Vorzeichen
# > sintL exp: Exponent
# > uintD* mantMSDptr: Pointer auf eine NUDS mit gesetztem höchstem Bit
# > uintC mantlen: Anzahl der Digits, >= LF_minlen
# < object erg: neues Long-Float mit der UDS mantMSDptr/mantlen/.. als Mantisse
# Der Exponent wird nicht auf Überlauf/Unterlauf getestet.
# kann GC auslösen
  #define encode_LF(sign,exp,mantMSDptr,mantlen,erg_zuweisung)  \
    encode_LFu(sign,LF_exp_mid+(uintL)(exp),mantMSDptr,mantlen,_EMA_ erg_zuweisung)

# Hash-Code eines Long-Float: Mischung aus Exponent, Länge, erste 32 Bit
  global uint32 hashcode_lfloat (object obj);
  global uint32 hashcode_lfloat(obj)
    var reg1 object obj;
    { return TheLfloat(obj)->expo + TheLfloat(obj)->len
             + get_32_Dptr(&TheLfloat(obj)->data[0]);
    }

# LF_zerop(x) stellt fest, ob ein Long-Float x = 0.0 ist.
  #define LF_zerop(x)  (TheLfloat(x)->expo == 0)

# Liefert zu einem Long-Float x : (ftruncate x), ein LF.
# LF_ftruncate_LF(x)
# x wird zur 0 hin zur nächsten ganzen Zahl gerundet.
# kann GC auslösen
  local object LF_ftruncate_LF (object x);
# Methode:
# x = 0.0 oder e<=0 -> Ergebnis 0.0
# 1<=e<=16n -> letzte (16n-e) Bits der Mantisse auf 0 setzen,
#              Exponent und Vorzeichen beibehalten
# e>=16n -> Ergebnis x
#if 0
  local object LF_ftruncate_LF(x)
    var reg2 object x;
    { var reg8 signean sign;
      var reg1 sintL exp;
      var reg9 uintD* mantMSDptr;
      var reg4 uintC mantlen;
      LF_decode(x, { return x; }, sign=,exp=,mantMSDptr=,mantlen=,);
      if (exp<=0) { encode_LF0(mantlen, return); } # e<=0 -> Ergebnis 0.0
      if ((uintL)exp >= intDsize*(uintL)mantlen) # e>=16n -> x als Ergebnis
        { return x; }
        else
        # 0 < e < 16n
        # neue NUDS erzeugen mit e Bits aus mant und 16n-e Nullbits:
        { SAVE_NUM_STACK # num_stack retten
          var reg7 uintD* MSDptr;
          num_stack_need(mantlen, MSDptr=,);
          { var reg5 uintC count = floor((uintL)exp,intDsize); # zu kopierende Digits, < mantlen
            var reg6 uintC bitcount = ((uintL)exp) % intDsize; # zu kopierende Bits danach, >=0, <intDsize
            var reg3 uintD* ptr =
              copy_loop_up(mantMSDptr,MSDptr,count); # count ganze Digits kopieren
            *ptr++ = mantMSDptr[count] & minus_bitm(intDsize-bitcount); # dann bitcount Bits kopieren
            clear_loop_up(ptr,mantlen-count-1); # Rest mit Nullen füllen
          }
          RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
          encode_LF(sign,exp,MSDptr,mantlen, return);
    }   }
#else
  local object LF_ftruncate_LF(x)
    var reg4 object x;
    { var reg2 uintC len = TheLfloat(x)->len;
      var reg1 uintL uexp = TheLfloat(x)->expo;
      if (uexp <= LF_exp_mid)
        { if (uexp == 0) { return x; } # x=0.0 -> Ergebnis 0.0
          encode_LF0(len, return); # e<=0 -> Ergebnis 0.0
        }
     {var reg3 uintL exp = uexp - LF_exp_mid;
      if (exp >= intDsize*(uintL)len) # e>=16n -> x als Ergebnis
        { return x; }
      # 0 < e < 16n
      pushSTACK(x);
      {var reg5 object y = allocate_lfloat(len,uexp,R_sign(x)); # neues Long-Float
       x = popSTACK();
       # y_mant := NUDS mit e Bits aus x_mant und 16n-e Nullbits:
       {var reg7 uintC count = floor(exp,intDsize); # zu kopierende Digits, < mantlen
        var reg9 uintC bitcount = exp % intDsize; # zu kopierende Bits danach, >=0, <intDsize
        var reg8 uintD* x_mantMSDptr = &TheLfloat(x)->data[0];
        var reg6 uintD* ptr =
          copy_loop_up(x_mantMSDptr,&TheLfloat(y)->data[0],count); # count ganze Digits kopieren
        *ptr++ = x_mantMSDptr[count] & minus_bitm(intDsize-bitcount); # dann bitcount Bits kopieren
        clear_loop_up(ptr,len-count-1); # Rest mit Nullen füllen
       }
       return y;
    }}}
#endif

# Liefert zu einem Long-Float x : (futruncate x), ein LF.
# LF_futruncate_LF(x)
# x wird von der 0 weg zur nächsten ganzen Zahl gerundet.
# kann GC auslösen
  local object LF_futruncate_LF (object x);
# Methode:
# x = 0.0 -> Ergebnis 0.0
# e<=0 -> Ergebnis 1.0 oder -1.0, je nach Vorzeichen von x.
# 1<=e<16n -> Greife die letzten (16n-e) Bits von x heraus.
#             Sind sie alle =0 -> Ergebnis x.
#             Sonst setze sie alle auf 0 und erhöhe dann die vorderen e Bits
#             um 1.
#             Kein Überlauf -> fertig.
#             Sonst (Ergebnis eine Zweierpotenz): Mantisse := .1000...000,
#               e:=e+1. (Test auf Überlauf wegen e<=16n überflüssig)
# e>=16n -> Ergebnis x.
#if 0
  local object LF_futruncate_LF(x)
    var reg1 object x;
    { var reg9 signean sign;
      var reg3 sintL exp;
      var reg2 uintD* mantMSDptr;
      var reg7 uintC mantlen;
      LF_decode(x, { return x; }, sign=,exp=,mantMSDptr=,mantlen=,);
      if (exp<=0) { encode_LF1s(sign,mantlen, return); } # e<=0 -> Ergebnis +-1.0
      if ((uintL)exp >= intDsize*(uintL)mantlen) # e>=16n -> x als Ergebnis
        { return x; }
        else
        # 0 < e < 16n
        { # Testen, ob alle hinteren 16n-e Bits =0 sind:
          var reg6 uintC count = floor((uintL)exp,intDsize); # zu kopierende Digits, < mantlen
          var reg5 uintC bitcount = ((uintL)exp) % intDsize; # zu kopierende Bits danach, >=0, <intDsize
          var reg5 uintD mask = minus_bitm(intDsize-bitcount); # Maske mit bitcount Bits
          var reg2 uintD* mantptr = &mantMSDptr[count];
          if (   ((mantptr[0] & ~mask) ==0)
              && !test_loop_up(&mantptr[1],mantlen-count-1)
             )
            { return x; }
          # neue NUDS erzeugen mit e Bits aus mant mit Increment
          # und 16n-e Nullbits:
         {SAVE_NUM_STACK # num_stack retten
          var reg8 uintD* MSDptr;
          num_stack_need(mantlen, MSDptr=,);
          { var reg4 uintD* ptr =
              copy_loop_up(mantMSDptr,MSDptr,count); # count ganze Digits kopieren
            if ((ptr[0] = ((mantptr[0] & mask) - mask)) == 0) # dann bitcount Bits kopieren und incrementieren
              { if (!( inc_loop_down(ptr,count) ==0)) # evtl. weiterincrementieren
                  { MSDptr[0] = bit(intDsize-1); exp = exp+1; } # evtl. Exponenten erhöhen
              }
            clear_loop_up(&ptr[1],mantlen-count-1); # Rest mit Nullen füllen
          }
          RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
          encode_LF(sign,exp,MSDptr,mantlen, return);
    }   }}
#else
  local object LF_futruncate_LF(x)
    var reg1 object x;
    { var reg3 uintC len = TheLfloat(x)->len;
      var reg7 uintL uexp = TheLfloat(x)->expo;
      if (uexp <= LF_exp_mid)
        { if (uexp == 0) { return x; } # x=0.0 -> Ergebnis 0.0
          encode_LF1s(R_sign(x),len, return); # e<=0 -> Ergebnis +-1.0
        }
     {var reg7 uintL exp = uexp - LF_exp_mid;
      if (exp >= intDsize*(uintL)len) # e>=16n -> x als Ergebnis
        { return x; }
      # 0 < e < 16n
      # Testen, ob alle hinteren 16n-e Bits =0 sind:
      {var reg5 uintC count = floor(exp,intDsize); # zu kopierende Digits, < mantlen
       var reg6 uintC bitcount = exp % intDsize; # zu kopierende Bits danach, >=0, <intDsize
       var reg3 uintD mask = minus_bitm(intDsize-bitcount); # Maske mit bitcount Bits
       {var reg2 uintD* mantptr = &TheLfloat(x)->data[count];
        if (   ((mantptr[0] & ~mask) ==0)
            && !test_loop_up(&mantptr[1],len-count-1)
           )
          { return x; }
       }
       # Nein -> neues Long-Float produzieren:
       pushSTACK(x);
       {var reg2 object y = allocate_lfloat(len,uexp,R_sign(x)); # neues Long-Float
        x = popSTACK();
        # y_mant := NUDS mit e Bits aus x_mant mit Increment und 16n-e Nullbits:
        {var reg8 uintD* x_mantMSDptr = &TheLfloat(x)->data[0];
         var reg9 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
         var reg4 uintD* ptr =
           copy_loop_up(x_mantMSDptr,y_mantMSDptr,count); # count ganze Digits kopieren
         if ((ptr[0] = ((x_mantMSDptr[count] & mask) - mask)) == 0) # dann bitcount Bits kopieren und incrementieren
           { if (!( inc_loop_down(ptr,count) ==0)) # evtl. weiterincrementieren
               { y_mantMSDptr[0] = bit(intDsize-1); (TheLfloat(y)->expo)++; } # evtl. Exponenten erhöhen
           }
         clear_loop_up(&ptr[1],len-count-1); # Rest mit Nullen füllen
        }
        return y;
    }}}}
#endif

# Liefert zu einem Long-Float x : (fround x), ein LF.
# LF_fround_LF(x)
# x wird zur nächsten ganzen Zahl gerundet.
# kann GC auslösen
  local object LF_fround_LF (object x);
# Methode:
# x = 0.0 oder e<0 -> Ergebnis 0.0
# 0<=e<16n -> letzte (16n-e) Bits der Mantisse wegrunden,
#             Exponent und Vorzeichen beibehalten.
# e>=16n -> Ergebnis x
#if 0
  local object LF_fround_LF(x)
    var reg4 object x;
    { var reg9 signean sign;
      var reg5 sintL exp;
      var reg1 uintD* mantMSDptr;
      var reg7 uintC mantlen;
      LF_decode(x, { return x; }, sign=,exp=,mantMSDptr=,mantlen=,);
      if (exp<0) { encode_LF0(mantlen, return); } # e<0 -> Ergebnis 0.0
      if ((uintL)exp >= intDsize*(uintL)mantlen) # e>=16n -> x als Ergebnis
        { return x; }
        else
        # 0 <= e < 16n
        { # alle hinteren 16n-e Bits wegrunden:
          var reg6 uintC count = floor((uintL)exp,intDsize); # zu kopierende Digits, < mantlen
          var reg6 uintC bitcount = ((uintL)exp) % intDsize; # zu kopierende Bits danach, >=0, <intDsize
          var reg3 uintD mask = minus_bit(intDsize-bitcount-1); # Maske mit bitcount+1 Bits
          var reg1 uintD* mantptr = &mantMSDptr[count];
          if ((mantptr[0] & -mask) ==0) goto ab; # Bit 16n-e-1 =0 -> abrunden
          if (!((mantptr[0] & ~mask) ==0)) goto auf; # Bit 16n-e-1 =1 und Bits 16n-e-2..0 >0 -> aufrunden
          if (test_loop_up(&mantptr[1],mantlen-count-1)) goto auf;
          # round-to-even, je nach Bit 16n-e :
          if (bitcount>0)
            { if ((mantptr[0] & (-2*mask)) ==0) goto ab; else goto auf; }
            elif (count>0)
              { if ((mantptr[-1] & bit(0)) ==0) goto ab; else goto auf; }
              else
              # bitcount=0, count=0, also exp=0: Abrunden von +-0.5 zu 0.0
              { encode_LF0(mantlen, return); }
          ab: # abrunden
          { SAVE_NUM_STACK # num_stack retten
            var reg8 uintD* MSDptr;
            num_stack_need(mantlen, MSDptr=,);
           {var reg2 uintD* ptr =
              copy_loop_up(mantMSDptr,MSDptr,count); # count ganze Digits kopieren
            *ptr++ = mantMSDptr[count] & mask; # dann bitcount Bits kopieren
            clear_loop_up(ptr,mantlen-count-1); # Rest mit Nullen füllen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            encode_LF(sign,exp,MSDptr,mantlen, return);
          }}
          auf: # aufrunden
          { SAVE_NUM_STACK # num_stack retten
            var reg8 uintD* MSDptr;
            num_stack_need(mantlen, MSDptr=,);
           {var reg2 uintD* ptr =
              copy_loop_up(mantMSDptr,MSDptr,count); # count ganze Digits kopieren
            if ((ptr[0] = ((mantptr[0] & mask) - mask)) == 0) # dann bitcount Bits kopieren und incrementieren
              { if (!( inc_loop_down(ptr,count) ==0)) # evtl. weiterincrementieren
                  { MSDptr[0] = bit(intDsize-1); exp = exp+1; } # evtl. Exponenten erhöhen
              }
            clear_loop_up(&ptr[1],mantlen-count-1); # Rest mit Nullen füllen
            RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
            encode_LF(sign,exp,MSDptr,mantlen, return);
          }}
    }   }
#else
  local object LF_fround_LF(x)
    var reg1 object x;
    { var reg4 uintC len = TheLfloat(x)->len;
      var reg5 uintL uexp = TheLfloat(x)->expo;
      if (uexp < LF_exp_mid)
        { if (uexp == 0) { return x; } # x=0.0 -> Ergebnis 0.0
          encode_LF0(len, return); # e<0 -> Ergebnis 0.0
        }
     {var reg7 uintL exp = uexp - LF_exp_mid;
      if (exp >= intDsize*(uintL)len) # e>=16n -> x als Ergebnis
        { return x; }
      # 0 <= e < 16n
      # alle hinteren 16n-e Bits wegrunden:
      {var reg6 uintC count = floor(exp,intDsize); # zu kopierende Digits, < mantlen
       var reg10 uintC bitcount = exp % intDsize; # zu kopierende Bits danach, >=0, <intDsize
       var reg3 uintD mask = minus_bit(intDsize-bitcount-1); # Maske mit bitcount+1 Bits
       {var reg2 uintD* mantptr = &TheLfloat(x)->data[count];
        if ((mantptr[0] & -mask) ==0) goto ab; # Bit 16n-e-1 =0 -> abrunden
        if (!((mantptr[0] & ~mask) ==0)) goto auf; # Bit 16n-e-1 =1 und Bits 16n-e-2..0 >0 -> aufrunden
        if (test_loop_up(&mantptr[1],len-count-1)) goto auf;
        # round-to-even, je nach Bit 16n-e :
        if (bitcount>0)
          { if ((mantptr[0] & (-2*mask)) ==0) goto ab; else goto auf; }
          elif (count>0)
            { if ((mantptr[-1] & bit(0)) ==0) goto ab; else goto auf; }
            else
            # bitcount=0, count=0, also exp=0: Abrunden von +-0.5 zu 0.0
            { encode_LF0(len, return); }
       }
       ab: # abrunden
         pushSTACK(x);
         {var reg8 object y = allocate_lfloat(len,uexp,R_sign(x)); # neues Long-Float
          x = popSTACK();
          # y_mant := NUDS mit e Bits aus x_mant und 16n-e Nullbits:
          {var reg9 uintD* x_mantMSDptr = &TheLfloat(x)->data[0];
           var reg2 uintD* ptr =
             copy_loop_up(x_mantMSDptr,&TheLfloat(y)->data[0],count); # count ganze Digits kopieren
           *ptr++ = x_mantMSDptr[count] & mask; # dann bitcount Bits kopieren
           clear_loop_up(ptr,len-count-1); # Rest mit Nullen füllen
          }
          return y;
         }
       auf: # aufrunden
         pushSTACK(x);
         {var reg8 object y = allocate_lfloat(len,uexp,R_sign(x)); # neues Long-Float
          x = popSTACK();
          # y_mant := NUDS mit e Bits aus x_mant mit Increment und 16n-e Nullbits:
          {var reg9 uintD* x_mantMSDptr = &TheLfloat(x)->data[0];
           var reg9 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
           var reg2 uintD* ptr =
             copy_loop_up(x_mantMSDptr,y_mantMSDptr,count); # count ganze Digits kopieren
           if ((ptr[0] = ((x_mantMSDptr[count] & mask) - mask)) == 0) # dann bitcount Bits kopieren und incrementieren
             { if (!( inc_loop_down(ptr,count) ==0)) # evtl. weiterincrementieren
                 { y_mantMSDptr[0] = bit(intDsize-1); (TheLfloat(y)->expo)++; } # evtl. Exponenten erhöhen
             }
           clear_loop_up(&ptr[1],len-count-1); # Rest mit Nullen füllen
          }
          return y;
         }
    }}}
#endif

# Liefert zu einem Long-Float x : (- x), ein LF.
# LF_minus_LF(x)
# kann GC auslösen
  local object LF_minus_LF (object x);
# Methode:
# Falls x=0.0, fertig. Sonst Vorzeichenbit umdrehen und Pointer beibehalten.
  local object LF_minus_LF(x)
    var reg1 object x;
    { if (TheLfloat(x)->expo == 0)
        { return x; }
        else
        #ifdef SPVW_MIXED
        { return (object)((oint)x ^ wbit(vorz_bit_o)); }
        #else
        { var reg3 uintC len = TheLfloat(x)->len;
          pushSTACK(x);
         {var reg2 object mx = allocate_lfloat(len,TheLfloat(x)->expo,~R_sign(x));
          x = popSTACK();
          copy_loop_up(&TheLfloat(x)->data[0],&TheLfloat(mx)->data[0],len);
          return mx;
        }}
        #endif
    }

# LF_LF_comp(x,y) vergleicht zwei Long-Floats x und y.
# Ergebnis: 0 falls x=y, +1 falls x>y, -1 falls x<y.
  local signean LF_LF_comp (object x, object y);
# Methode:
# x und y haben verschiedenes Vorzeichen ->
#    x < 0 -> x < y
#    x >= 0 -> x > y
# x und y haben gleiches Vorzeichen ->
#    x >=0 -> vergleiche x und y (die rechten 24 Bits)
#    x <0 -> vergleiche y und x (die rechten 24 Bits)
  local signean LF_LF_comp(x,y)
    var reg1 object x;
    var reg2 object y;
    { if (!R_minusp(y))
        # y>=0
        { if (!R_minusp(x))
            # y>=0, x>=0
            { # Vergleiche Exponenten und Mantissen:
              { var reg3 uintL x_uexp = TheLfloat(x)->expo;
                var reg4 uintL y_uexp = TheLfloat(y)->expo;
                if (x_uexp < y_uexp) return signean_minus; # x<y
                if (x_uexp > y_uexp) return signean_plus; # x>y
              }
              { var reg5 uintC x_len = TheLfloat(x)->len;
                var reg6 uintC y_len = TheLfloat(y)->len;
                var reg4 uintC len = (x_len<y_len ? x_len : y_len); # min(x_len,y_len)
                # len Digits vergleichen:
                var reg3 signean erg =
                  compare_loop_up(&TheLfloat(x)->data[0],&TheLfloat(y)->data[0],len);
                if (!(erg==0)) { return erg; } # verschieden -> fertig
                # gemeinsames Teilstück war gleich
                if (x_len == y_len) { return signean_null; } # gleiche Länge -> fertig
                if (x_len > y_len)
                  # x länger als y
                  { if (test_loop_up(&TheLfloat(x)->data[y_len],x_len-y_len))
                      { return signean_plus; } # x>y
                      else
                      { return signean_null; }
                  }
                  else
                  # y länger als x
                  { if (test_loop_up(&TheLfloat(y)->data[x_len],y_len-x_len))
                      { return signean_minus; } # x<y
                      else
                      { return signean_null; }
                  }
            } }
            else
            # y>=0, x<0
            { return signean_minus; } # x<y
        }
        else
        { if (!R_minusp(x))
            # y<0, x>=0
            { return signean_plus; } # x>y
            else
            # y<0, x<0
            { # Vergleiche Exponenten und Mantissen:
              { var reg3 uintL x_uexp = TheLfloat(x)->expo;
                var reg4 uintL y_uexp = TheLfloat(y)->expo;
                if (x_uexp < y_uexp) return signean_plus; # |x|<|y| -> x>y
                if (x_uexp > y_uexp) return signean_minus; # |x|>|y| -> x<y
              }
              { var reg5 uintC x_len = TheLfloat(x)->len;
                var reg6 uintC y_len = TheLfloat(y)->len;
                var reg4 uintC len = (x_len<y_len ? x_len : y_len); # min(x_len,y_len)
                # len Digits vergleichen:
                var reg3 signean erg =
                  compare_loop_up(&TheLfloat(y)->data[0],&TheLfloat(x)->data[0],len);
                if (!(erg==0)) { return erg; } # verschieden -> fertig
                # gemeinsames Teilstück war gleich
                if (x_len == y_len) { return signean_null; } # gleiche Länge -> fertig
                if (x_len > y_len)
                  # x länger als y
                  { if (test_loop_up(&TheLfloat(x)->data[y_len],x_len-y_len))
                      { return signean_minus; } # |x|>|y| -> x<y
                      else
                      { return signean_null; }
                  }
                  else
                  # y länger als x
                  { if (test_loop_up(&TheLfloat(y)->data[x_len],y_len-x_len))
                      { return signean_plus; } # |x|<|y| -> x>y
                      else
                      { return signean_null; }
                  }
            } }
        }
    }

# LF_shorten_LF(x,len) verkürzt ein Long-Float x auf gegebene Länge len
# und rundet dabei.
# > object x: ein Long-FLoat
# > uintC len: gewünschte Länge (>= LF_minlen, < TheLfloat(x)->len)
# < object ergebnis: verkürztes Long-Float
# kann GC auslösen
  local object LF_shorten_LF (object x, uintC len);
  local object LF_shorten_LF(x,len)
    var reg2 object x;
    var reg4 uintC len;
    { # x = 0.0 braucht nicht abgefangen zu werden, da bei Mantisse 0 dann
      # sowieso abgerundet wird, die Mantisse also 0 bleibt.
      pushSTACK(x);
     {var reg3 object y = allocate_lfloat(len,TheLfloat(x)->expo,R_sign(x)); # neues LF
      x = popSTACK();
      { var reg5 uintC oldlen = TheLfloat(x)->len; # alte Länge, > len
        # Mantisse von x nach y kopieren:
        copy_loop_up(&TheLfloat(x)->data[0],&TheLfloat(y)->data[0],len);
        # Entscheiden, ob auf- oder abrunden:
       {var reg1 uintD* ptr = &TheLfloat(x)->data[len];
        if ( ((ptr[0] & bit(intDsize-1)) ==0) # nächstes Bit eine 0 -> abrunden
             || ( ((ptr[0] & (bit(intDsize-1)-1)) ==0) # eine 1 und alles weitere Nullen?
                  && !test_loop_up(&ptr[1],oldlen-len-1)
                  # round-to-even
                  && ((ptr[-1] & bit(0)) ==0)
           )    )
          # abrunden
          {}
          else
          # aufrunden
          { if ( inc_loop_down(&TheLfloat(y)->data[len],len) )
              # Übertrag durch Aufrunden
              { TheLfloat(y)->data[0] = bit(intDsize-1); # Mantisse := 10...0
                # Exponent erhöhen:
                if ((TheLfloat(y)->expo)++ == LF_exp_high+1) { fehler_overflow(); }
          }   }
      }}
      return y;
    }}

# LF_extend_LF(x,len) verlängert ein Long-Float x auf gegebene Länge len.
# > object x: ein Long-FLoat
# > uintC len: gewünschte Länge (> TheLfloat(x)->len)
# < object ergebnis: verlängertes Long-Float
# kann GC auslösen
  local object LF_extend_LF (object x, uintC len);
  local object LF_extend_LF(x,len)
    var reg1 object x;
    var reg4 uintC len;
    { pushSTACK(x);
     {var reg2 object y = allocate_lfloat(len,TheLfloat(x)->expo,R_sign(x)); # neues LF
      x = popSTACK();
      { var reg5 uintC oldlen = TheLfloat(x)->len; # alte Länge, < len
        # Mantisse von x nach y kopieren:
        var reg3 uintD* ptr =
          copy_loop_up(&TheLfloat(x)->data[0],&TheLfloat(y)->data[0],oldlen);
        # und mit Null-Digits ergänzen:
        clear_loop_up(ptr,len-oldlen);
      }
      return y;
    }}

# LF_to_LF(x,len) wandelt ein Long-Float x in ein Long-Float gegebener Länge
# len um und rundet dabei nötigenfalls.
# > object x: ein Long-FLoat
# > uintC len: gewünschte Länge (>= LF_minlen)
# < object ergebnis: Long-Float gegebener Länge
# kann GC auslösen
  local object LF_to_LF (object x, uintC len);
  local object LF_to_LF(x,len)
    var reg2 object x;
    var reg3 uintC len;
    { var reg1 uintC oldlen = TheLfloat(x)->len;
      if (len < oldlen) { return LF_shorten_LF(x,len); }
      if (len > oldlen) { return LF_extend_LF(x,len); }
      # len = oldlen
      return x;
    }

# Liefert zu zwei gleichlangen Long-Float x und y : (+ x y), ein LF.
# LF_LF_plus_LF(x)
# kann GC auslösen
  local object LF_LF_plus_LF (object x, object y);
# Methode (nach [Knuth, II, Seminumerical Algorithms, Abschnitt 4.2.1., S.200]):
# Falls e1<e2, vertausche x1 und x2.
# Also e1 >= e2.
# Falls e2=0, also x2=0.0, Ergebnis x1.
# Falls e1 - e2 >= 16n+2, Ergebnis x1.
# Erweitere die Mantissen rechts um 3 Bits (Bit -1 als Schutzbit, Bits -2,-3
#   als Rundungsbits: 00 exakt, 01 1.Hälfte, 10 exakte Mitte, 11 2.Hälfte.)
# Schiebe die Mantisse von x2 um e0-e1 Bits nach rechts. (Dabei die Rundung
# ausführen: Bit -3 ist das logische Oder der Bits -3,-4,-5,...)
# Falls x1,x2 selbes Vorzeichen haben: Addiere dieses zur Mantisse von x1.
# Falls x1,x2 verschiedenes Vorzeichen haben: Subtrahiere dieses von der
#   Mantisse von x1. <0 -> (Es war e1=e2) Vertausche die Vorzeichen, negiere.
#                    =0 -> Ergebnis 0.0
# Exponent ist e1.
# Normalisiere, fertig.
  local object LF_LF_plus_LF(x1,x2)
    var reg9 object x1;
    var reg9 object x2;
    { var reg9 uintL uexp1 = TheLfloat(x1)->expo;
      var reg9 uintL uexp2 = TheLfloat(x2)->expo;
      if (uexp1 < uexp2)
        # x1 und x2 vertauschen
        { swap(reg1 object, x1,x2); swap(reg1 uintL, uexp1,uexp2); }
      # uexp1 >= uexp2
      if (uexp2==0) { return x1; } # x2=0.0 -> x1 als Ergebnis
     {var reg9 uintC len = TheLfloat(x1)->len; # Länge n von x1 und x2
      var reg9 uintL expdiff = uexp1-uexp2; # e1-e2
      #if !defined(SPVW_MIXED)
      if ((expdiff == 0) && (wbit_test((oint)x1 ^ (oint)x2, vorz_bit_o)))
        # verschiedene Vorzeichen, aber gleicher Exponent
        { # Vorzeichen des Ergebnisses festlegen:
          var reg1 signean erg = # Mantissen (je len Digits) vergleichen
            compare_loop_up(&TheLfloat(x1)->data[0],&TheLfloat(x2)->data[0],len);
          if (erg==0) # Mantissen gleich
            { return allocate_lfloat(len,0,0); } # Ergebnis 0.0
          if (erg<0) # |x1| < |x2|
            # x1 und x2 vertauschen, expdiff bleibt =0
            { swap(reg1 object, x1,x2); swap(reg1 uintL, uexp1,uexp2); }
        }
      #endif
      if (expdiff >= intDsize * (uintL)len + 2) # e1-e2 >= 16n+2 ?
        { return x1; } # ja -> x1 als Ergebnis
      # neues Long-Float allozieren:
      pushSTACK(x1); pushSTACK(x2);
      { var reg7 object y = allocate_lfloat(len,uexp1,R_sign(x1));
        x2 = popSTACK(); x1 = popSTACK();
       {var reg9 uintL i = floor(expdiff,intDsize); # e1-e2 div 16 (>=0, <=n)
        var reg9 uintL j = expdiff % intDsize; # e1-e2 mod 16 (>=0, <16)
        # Mantisse von x2 muß um intDsize*i+j Bits nach rechts geschoben werden.
        var reg8 uintC x2_len = len - i; # n-i Digits von x2 gebraucht
        # x2_len Digits um j Bits nach rechts schieben und dabei kopieren:
        SAVE_NUM_STACK # num_stack retten
        var reg9 uintD* x2_MSDptr;
        var reg9 uintD* x2_LSDptr;
        var reg2 uintD rounding_bits;
        num_stack_need(x2_len, x2_MSDptr=,x2_LSDptr=); # x2_len Digits Platz
        if (j==0)
          { copy_loop_up(&TheLfloat(x2)->data[0],x2_MSDptr,x2_len); rounding_bits = 0; }
          else
          { rounding_bits = shiftrightcopy_loop_up(&TheLfloat(x2)->data[0],x2_MSDptr,x2_len,j,0); }
        # x2_MSDptr/x2_len/x2_LSDptr sind die essentiellen Digits von x2.
        # rounding_bits enthält die letzten j herausgeschobenen Bits.
        # Aus rounding_bits und den nächsten i Digits die 3 Rundungsbits
        # (als Bits intDsize-1..intDsize-3 von rounding_bits) aufbauen:
        if (j>=2)
          # j>=2 -> Bits -1,-2 sind OK, Bit -3 bestimmen:
          { if ((rounding_bits & (bit(intDsize-3)-1)) ==0)
              { if (test_loop_up(&TheLfloat(x2)->data[x2_len],i))
                  { rounding_bits |= bit(intDsize-3); } # Rundungsbit -3 setzen
              }
              else
              { rounding_bits |= bit(intDsize-3); # Rundungsbit -3 setzen
                rounding_bits &= bitm(intDsize)-bit(intDsize-3); # andere Bits löschen
          }   }
          else
          # j<=3 -> Bits intDsize-4..0 von rounding_bits sind bereits Null.
          # nächstes und weitere i-1 Digits heranziehen:
          { if (i > 0) # i=0 -> Bits -1,-2,-3 sind OK.
              { var reg1 uintD* ptr = &TheLfloat(x2)->data[x2_len];
                rounding_bits |= (ptr[0] >> j); # weitere relevante Bits des nächsten Digit dazu
                if ((rounding_bits & (bit(intDsize-3)-1)) ==0) # Alle Bits -3,-4,... =0 ?
                  { if (   (!((ptr[0] & (bit(3)-1)) ==0)) # j (<=3) untere Bits von ptr[0] alle =0 ?
                        || test_loop_up(&ptr[1],i-1)
                       )
                      { rounding_bits |= bit(intDsize-3); } # Rundungsbit -3 setzen
                  }
                  else
                  { rounding_bits |= bit(intDsize-3); # Rundungsbit -3 setzen
                    rounding_bits &= bitm(intDsize)-bit(intDsize-3); # andere Bits löschen
          }   }   }
        # x2 liegt in verschobener Form in der UDS x2_MSDptr/x2_len/x2_LSDptr
        # vor, mit Rundungsbits in Bit intDsize-1..intDsize-3 von rounding_bits.
        {var reg3 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
         var reg4 uintD* y_mantLSDptr = &y_mantMSDptr[(uintL)len];
         if (!wbit_test((oint)x1 ^ (oint)x2, vorz_bit_o))
           # gleiche Vorzeichen -> Mantissen addieren
           { # erst rechten Mantissenteil (x2_len Digits) durch Addition:
             var reg5 uintD carry =
               add_loop_down(&TheLfloat(x1)->data[(uintL)len],x2_LSDptr,
                             y_mantLSDptr, x2_len
                            );
             # dann linken Mantissenteil (i Digits) direkt kopieren:
             var reg1 uintD* ptr =
               copy_loop_up(&TheLfloat(x1)->data[0],y_mantMSDptr,i);
             # dann Übertrag vom rechten zum linken Mantissenteil addieren:
             if (!(carry==0))
               { if ( inc_loop_down(ptr,i) )
                   # Übertrag über das erste Digit hinaus
                   { # Exponent von y incrementieren:
                     if ( (TheLfloat(y)->expo)++ == LF_exp_high+1 ) { fehler_overflow(); }
                     # normalisiere durch Schieben um 1 Bit nach rechts:
                    {var reg6 uintD carry_rechts =
                       shift1right_loop_up(y_mantMSDptr,len,-1);
                     rounding_bits = carry_rechts | (rounding_bits>>1); # Rundungsbits mitschieben
               }   }}
           }
           else
           # verschiedene Vorzeichen -> Mantissen subtrahieren
           { # erst rechten Mantissenteil (x2_len Digits) durch Subtraktion:
             rounding_bits = -rounding_bits;
             {var reg5 uintD carry =
                subx_loop_down(&TheLfloat(x1)->data[(uintL)len],x2_LSDptr,
                               y_mantLSDptr, x2_len,
                               (rounding_bits==0 ? 0 : -1L)
                              );
              # dann linken Mantissenteil (i Digits) direkt kopieren:
              var reg1 uintD* ptr =
                copy_loop_up(&TheLfloat(x1)->data[0],y_mantMSDptr,i);
              # dann Übertrag des rechten vom linken Mantissenteil subtrahieren:
              if (!(carry==0))
                { if ( dec_loop_down(ptr,i) )
                    # Übertrag über das erste Digit hinaus, also e1=e2
                    #if !defined(SPVW_MIXED)
                    { NOTREACHED } # diesen Fall haben wir schon behandelt
                    #else
                    { # Negieren:
                      y = (object)((oint)y ^ wbit(vorz_bit_o));
                      rounding_bits = -rounding_bits;
                      if (rounding_bits==0)
                        # Negieren ohne Carry
                        { neg_loop_down(y_mantLSDptr,len); }
                        else
                        # Negieren mit Carry von rechts
                        { # not_loop_down(y_mantLSDptr,len); # oder
                          not_loop_up(y_mantMSDptr,len);
                        }
                    }
                    #endif
                }
             }
             # UDS y_mantMSDptr/len/y_mantLSDptr/rounding_bits normalisieren:
             {var reg1 uintD* ptr = y_mantMSDptr;
              var reg5 uintL k = 0;
              var reg6 uintC count;
              dotimesC(count,len,
                { if (!(ptr[0]==0)) goto nonzero_found;
                  ptr++; k++;
                });
              if (!(rounding_bits==0)) goto nonzero_found;
              # Die UDS ist ganz Null. Also war e1=e2, keine Rundungsbits.
              #if !defined(SPVW_MIXED)
              { NOTREACHED } # diesen Fall haben wir schon behandelt
              #else
              TheLfloat(y)->expo = 0; # 0.0 als Ergebnis
              return (object)((oint)y & ~wbit(vorz_bit_o));
              #endif
              nonzero_found: # Digit /=0 gefunden
              # UDS von ptr nach y_mantMSDptr um k Digits nach unten kopieren:
              if (k>0)
                # mindestens ein führendes Nulldigit. Also war e1-e2 = 0 oder 1.
                { ptr = copy_loop_up(ptr,y_mantMSDptr,len-k); # len-k Digits verschieben
                  *ptr++ = rounding_bits; # Rundungsbits als weiteres Digit
                  clear_loop_up(ptr,k-1); # dann k-1 Nulldigits
                  rounding_bits = 0; # und keine weiteren Rundungsbits
                  # Exponenten um intDsize*k erniedrigen:
                  k = intDsize*k;
                 {var reg1 uintL uexp = TheLfloat(y)->expo;
                  #if !(LF_exp_low==1)
                  if (uexp < k+LF_exp_low) { fehler_underflow(); }
                  #else
                  if (uexp <= k) { fehler_underflow(); }
                  #endif
                  TheLfloat(y)->expo = uexp - k;
                }}
             }
             # NUDS y_mantMSDptr/len/y_mantLSDptr/rounding_bits normalisieren:
             {var reg5 uintL s;
              integerlengthD(y_mantMSDptr[0], s = intDsize - );
              # s = Anzahl der führenden Nullbits im ersten Word (>=0, <intDsize)
              if (s > 0)
                { # Muß die NUDS y_mantMSDptr/len/y_mantLSDptr/rounding_bits
                  # um s Bits nach links schieben.
                  # (Bei e1-e2>1 ist dabei zwangsläufig s=1.)
                  if (s==1)
                    { shift1left_loop_down(y_mantLSDptr,len);
                      if (rounding_bits & bit(intDsize-1))
                        { y_mantLSDptr[-1] |= bit(0); }
                      rounding_bits = rounding_bits << 1;
                    }
                    else # s>1, also e1-e2 <= 1 <= s.
                    { shiftleft_loop_down(y_mantLSDptr,len,s,rounding_bits>>(intDsize-s));
                      rounding_bits = 0; # = rounding_bits << s;
                    }
                  # Exponenten um s erniedrigen:
                 {var reg1 uintL uexp = TheLfloat(y)->expo;
                  #if !(LF_exp_low==1)
                  if (uexp < s+LF_exp_low) { fehler_underflow(); }
                  #else
                  if (uexp <= s) { fehler_underflow(); }
                  #endif
                  TheLfloat(y)->expo = uexp - s;
                }}
           } }
         # Hier enthält rounding_bits Bit -1 als Bit intDsize-1, Bit -2 als
         # Bit intDsize-2, Bit -3 als Oder(Bits intDsize-3..0) !
         # Runden. Dazu rounding_bits inspizieren:
         if ((rounding_bits & bit(intDsize-1)) ==0) goto ab; # Bit -1 gelöscht -> abrunden
         rounding_bits = rounding_bits<<1; # Bits -2,-3
         if (!(rounding_bits==0)) goto auf; # Bit -2 oder Bit -3 gesetzt -> aufrunden
         # round-to-even:
         if ((y_mantLSDptr[-1] & bit(0)) ==0) goto ab;
         auf: # aufrunden
           if ( inc_loop_down(y_mantLSDptr,len) )
             { # Übertrag durchs Aufrunden
               y_mantMSDptr[0] = bit(intDsize-1); # Mantisse := 10...0
               # Exponent erhöhen:
               if ((TheLfloat(y)->expo)++ == LF_exp_high+1) { fehler_overflow(); }
             }
         ab: # abrunden
           ;
        }
        RESTORE_NUM_STACK # num_stack zurück
        # y fertig.
        return y;
    }}}}

# Liefert zu zwei gleichlangen Long-Float x und y : (- x y), ein LF.
# LF_LF_minus_LF(x)
# kann GC auslösen
  local object LF_LF_minus_LF (object x, object y);
# Methode:
# (- x1 x2) = (+ x1 (- x2))
  local object LF_LF_minus_LF(x1,x2)
    var reg2 object x1;
    var reg1 object x2;
    { if (TheLfloat(x2)->expo == 0)
        { return x1; }
        else
        #ifdef SPVW_MIXED
        { return LF_LF_plus_LF(x1, (object)((oint)x2 ^ wbit(vorz_bit_o)) ); }
        #else
        { var reg4 uintC len2 = TheLfloat(x2)->len;
          pushSTACK(x1); pushSTACK(x2);
         {var reg3 object mx2 = allocate_lfloat(len2,TheLfloat(x2)->expo,~R_sign(x2));
          x2 = popSTACK();
          copy_loop_up(&TheLfloat(x2)->data[0],&TheLfloat(mx2)->data[0],len2);
          return LF_LF_plus_LF(popSTACK(),mx2);
        }}
        #endif
    }

# Liefert zu zwei gleichlangen Long-Float x und y : (* x y), ein LF.
# LF_LF_mal_LF(x)
# kann GC auslösen
  local object LF_LF_mal_LF (object x, object y);
# Methode:
# Falls x1=0.0 oder x2=0.0 -> Ergebnis 0.0
# Sonst: Ergebnis-Vorzeichen = VZ von x1 xor VZ von x2.
#        Ergebnis-Exponent = Summe der Exponenten von x1 und x2.
#        Produkt der Mantissen bilden (2n Digits).
#        Falls das führende Bit =0 ist: Mantissenprodukt um 1 Bit nach links
#          schieben (die vorderen n+1 Digits genügen)
#          und Exponent decrementieren.
#        Runden auf n Digits liefert die Ergebnis-Mantisse.
  local object LF_LF_mal_LF(x1,x2)
    var reg9 object x1;
    var reg9 object x2;
    { var reg6 uintL uexp1 = TheLfloat(x1)->expo;
      if (uexp1==0) { return x1; } # x1=0.0 -> Ergebnis 0.0
     {var reg8 uintL uexp2 = TheLfloat(x2)->expo;
      if (uexp2==0) { return x2; } # x2=0.0 -> Ergebnis 0.0
      # Exponenten addieren:
      # (uexp1-LF_exp_mid) + (uexp2-LF_exp_mid) = (uexp1+uexp2-LF_exp_mid)-LF_exp_mid
      uexp1 = uexp1 + uexp2;
      if (uexp1 >= uexp2)
        # kein Carry
        { if (uexp1 < LF_exp_mid+LF_exp_low) { fehler_underflow(); } }
        else
        # Carry
        { if (uexp1 > (uintL)(LF_exp_mid+LF_exp_high+1)) { fehler_overflow(); } }
      uexp1 = uexp1 - LF_exp_mid;
      # Nun ist LF_exp_low <= uexp1 <= LF_exp_high+1.
      # neues Long-Float allozieren:
      pushSTACK(x1); pushSTACK(x2);
      {var reg2 uintC len = TheLfloat(x1)->len; # Länge n von x1 und x2
       var reg1 object y = allocate_lfloat(len,uexp1,
                                           R_sign((oint)x1 ^ (oint)x2) # Vorzeichen kombinieren
                                          );
       x2 = popSTACK(); x1 = popSTACK();
       # Produkt bilden:
       {var reg4 uintD* MSDptr;
        {SAVE_NUM_STACK # num_stack retten
         UDS_UDS_mal_UDS(len,&TheLfloat(x1)->data[(uintL)len],
                         len,&TheLfloat(x2)->data[(uintL)len],
                         MSDptr=,,);
         RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
        }
        {var reg3 uintD* midptr = &MSDptr[(uintL)len]; # Pointer in die Mitte der 2n Digits
         if ((sintD)MSDptr[0] >= 0) # führendes Bit abtesten
           { # erste n+1 Digits um 1 Bit nach links schieben:
             shift1left_loop_down(&midptr[1],len+1);
             # Exponenten decrementieren:
             if ((TheLfloat(y)->expo)-- == LF_exp_low-1) { fehler_underflow(); }
           }
         # erste Hälfte des Mantissenprodukts übertragen:
         {var reg5 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
          var reg7 uintD* y_mantLSDptr =
            copy_loop_up(MSDptr,y_mantMSDptr,len);
          # Runden:
          if ( ((sintD)midptr[0] >= 0) # nächstes Bit =0 -> abrunden
               || ( ((midptr[0] & (bit(intDsize-1)-1)) ==0) # Bit =1, weitere Bits >0 -> aufrunden
                    && !test_loop_up(&midptr[1],len-1)
                    # round-to-even
                    && ((midptr[-1] & bit(0)) ==0)
             )    )
            # abrunden
            {}
            else
            # aufrunden
            { if ( inc_loop_down(y_mantLSDptr,len) )
                { # Übertrag durchs Aufrunden (kann nur auftreten,
                  # wenn vorhin um 1 Bit nach links geschoben wurde)
                  y_mantMSDptr[0] = bit(intDsize-1); # Mantisse := 10...0
                  (TheLfloat(y)->expo)++; # Exponent wieder zurück-erhöhen
            }   }
          # LF_exp_low <= exp <= LF_exp_high sicherstellen:
          if (TheLfloat(y)->expo == LF_exp_high+1) { fehler_overflow(); }
       }}}
       return y;
    }}}

# Liefert zu zwei gleichlangen Long-Float x und y : (/ x y), ein LF.
# LF_LF_durch_LF(x)
# kann GC auslösen
  local object LF_LF_durch_LF (object x, object y);
# Methode:
# x2 = 0.0 -> Error
# x1 = 0.0 -> Ergebnis 0.0
# Sonst:
# Ergebnis-Vorzeichen = xor der beiden Vorzeichen von x1 und x2
# Ergebnis-Exponent = Differenz der beiden Exponenten von x1 und x2
# Ergebnis-Mantisse = Mantisse mant1 / Mantisse mant2, gerundet.
#   mant1/mant2 > 1/2, mant1/mant2 < 2;
#   nach Rundung mant1/mant2 >=1/2, <=2*mant1<2.
#   Bei mant1/mant2 >=1 brauche 16n-1 Nachkommabits,
#   bei mant1/mant2 <1 brauche 16n Nachkommabits.
#   Fürs Runden: brauche ein Rundungsbit (Rest gibt an, ob exakt).
#   Brauche daher insgesamt 16n+1 Nachkommabits von mant1/mant2.
#   Dividiere daher (als Unsigned Integers)
#     2^16(n+1)*(2^16n*m0) durch (2^16n*m1).
#   Falls der Quotient >=2^16(n+1) ist, schiebe ihn um 1 Bit nach rechts,
#     erhöhe den Exponenten um 1 und runde das letzte Digit weg.
#   Falls der Quotient <2^16(n+1) ist, runde das letzte Digit weg. Bei rounding
#     overflow schiebe um 1 Bit nach rechts und erhöhe den Exponenten um 1.
  local object LF_LF_durch_LF(x1,x2)
    var reg7 object x1;
    var reg9 object x2;
    { var reg8 uintL uexp2 = TheLfloat(x2)->expo;
      if (uexp2==0) { divide_0(); } # x2=0.0 -> Error
     {var reg6 uintL uexp1 = TheLfloat(x1)->expo;
      if (uexp1==0) { return x1; } # x1=0.0 -> Ergebnis 0.0
      # Exponenten subtrahieren:
      # (uexp1-LF_exp_mid) - (uexp2-LF_exp_mid) = (uexp1-uexp2+LF_exp_mid)-LF_exp_mid
      if (uexp1 >= uexp2)
        { uexp1 = uexp1 - uexp2; # kein Carry
          if (uexp1 > LF_exp_high-LF_exp_mid) { fehler_overflow(); }
          uexp1 = uexp1 + LF_exp_mid;
        }
        else
        { uexp1 = uexp1 - uexp2; # Carry
          if (uexp1 < (uintL)(LF_exp_low-1-LF_exp_mid)) { fehler_underflow(); }
          uexp1 = uexp1 + LF_exp_mid;
        }
      # Nun ist LF_exp_low-1 <= uexp1 <= LF_exp_high.
      # neues Long-Float allozieren:
      pushSTACK(x1); pushSTACK(x2);
      {var reg2 uintC len = TheLfloat(x1)->len; # Länge n von x1 und x2
       var reg1 object y = allocate_lfloat(len,uexp1,
                                           R_sign((oint)x1 ^ (oint)x2) # Vorzeichen kombinieren
                                          );
       x2 = popSTACK(); x1 = popSTACK();
       # Zähler bilden:
       {SAVE_NUM_STACK # num_stack retten
        var reg9 uintD* z_MSDptr;
        var reg9 uintL z_len;
        var reg9 uintD* z_LSDptr;
        z_len = 2*(uintL)len + 1;
        if ((intCsize < 32) && (z_len > (uintL)(bitc(intCsize)-1))) { fehler_LF_toolong(); }
        num_stack_need(z_len, z_MSDptr=,z_LSDptr=);
        {var reg3 uintD* ptr =
           copy_loop_up(&TheLfloat(x1)->data[0],z_MSDptr,len); # n Digits kopieren
         clear_loop_up(ptr,len+1); # und n+1 Null-Digits
        }
        # Quotienten bilden: 2n+1-Digit-Zahl durch n-Digit-Zahl dividieren
        {var DS q;
         var DS r;
         {var reg4 uintD* x2_mantMSDptr = &TheLfloat(x2)->data[0];
          UDS_divide(z_MSDptr,z_len,z_LSDptr,
                     x2_mantMSDptr,len,&x2_mantMSDptr[(uintL)len],
                     &q, &r
                    );
         }
         # q ist der Quotient mit n+1 oder n+2 Digits, r der Rest.
         RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
         if (q.len > len+1)
           # Quotient hat n+2 Digits -> um 1 Bit nach rechts schieben:
           { var reg5 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
             var reg4 uintD carry_rechts =
               shiftrightcopy_loop_up(&q.MSDptr[1],y_mantMSDptr,len,1,
                                      /* carry links = q.MSDptr[0] = 1 */ 1 );
             # Exponenten incrementieren:
             if ((TheLfloat(y)->expo)++ == LF_exp_high+1) { fehler_overflow(); }
             # Runden:
             if ( (carry_rechts == 0) # herausgeschobenes Bit =0 -> abrunden
                  || ( (q.LSDptr[-1]==0) # =1 und weitere Bits >0 oder Rest >0 -> aufrunden
                       && (r.len==0)
                       # round-to-even
                       && ((q.LSDptr[-2] & bit(1)) ==0)
                )    )
               # abrunden
               {}
               else
               # aufrunden
               { inc_loop_down(&y_mantMSDptr[(uintL)len],len); }
           }
           else
           # Quotient hat n+1 Digits -> nur kopieren:
           { var reg4 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
             copy_loop_up(q.MSDptr,y_mantMSDptr,len);
             # Runden:
             if ( ((sintD)(q.LSDptr[-1]) >= 0) # nächstes Bit =0 -> abrunden
                  || ( ((q.LSDptr[-1] & (bit(intDsize-1)-1)) ==0) # =1 und weitere Bits >0 oder Rest >0 -> aufrunden
                       && (r.len==0)
                       # round-to-even
                       && ((q.LSDptr[-2] & bit(0)) ==0)
                )    )
               # abrunden
               {}
               else
               # aufrunden
               { if ( inc_loop_down(&y_mantMSDptr[(uintL)len],len) )
                   # Übertrag durchs Aufrunden
                   { y_mantMSDptr[0] = bit(intDsize-1); # Mantisse := 10...0
                     # Exponenten incrementieren:
                     if ((TheLfloat(y)->expo)++ == LF_exp_high+1) { fehler_overflow(); }
               }   }
           }
        }
        # LF_exp_low <= exp <= LF_exp_high sicherstellen:
        if (TheLfloat(y)->expo == LF_exp_low-1) { fehler_underflow(); }
       }
       return y;
    }}}

# Liefert zu einem Long-Float x>=0 : (sqrt x), ein LF.
# LF_sqrt_LF(x)
# kann GC auslösen
  local object LF_sqrt_LF (object x);
# Methode:
# x = 0.0 -> Ergebnis 0.0
# Ergebnis-Vorzeichen := positiv,
# Ergebnis-Exponent := ceiling(e/2),
# Ergebnis-Mantisse:
#   Erweitere die Mantisse (n Digits) um n+2 Nulldigits nach hinten.
#   Bei ungeradem e schiebe dies (oder nur die ersten n+1 Digits davon)
#     um 1 Bit nach rechts.
#   Bilde daraus die Ganzzahl-Wurzel, eine n+1-Digit-Zahl mit einer
#     führenden 1.
#   Runde das letzte Digit weg:
#     Bit 15 = 0 -> abrunden,
#     Bit 15 = 1, Rest =0 und Wurzel exakt -> round-to-even,
#     sonst aufrunden.
#   Bei rounding overflow Mantisse um 1 Bit nach rechts schieben
#     und Exponent incrementieren.
  local object LF_sqrt_LF(x)
    var reg4 object x;
    { var reg3 uintL uexp = TheLfloat(x)->expo;
      if (uexp==0) { return x; } # x=0.0 -> 0.0 als Ergebnis
     {var reg2 uintC len = TheLfloat(x)->len;
      # Radikanden bilden:
      SAVE_NUM_STACK # num_stack retten
      var reg7 uintD* r_MSDptr;
      var reg9 uintD* r_LSDptr;
      var reg6 uintL r_len = 2*(uintL)len+2; # Länge des Radikanden
      if ((intCsize < 32) && (r_len > (uintL)(bitc(intCsize)-1))) { fehler_LF_toolong(); }
      num_stack_need(r_len, r_MSDptr=,r_LSDptr=);
      uexp = uexp - LF_exp_mid + 1;
      if (uexp & bit(0))
        # Exponent gerade
        {var reg1 uintD* ptr =
           copy_loop_up(&TheLfloat(x)->data[0],r_MSDptr,len); # n Digits kopieren
         clear_loop_up(ptr,len+2); # n+2 Nulldigits anhängen
        }
        else
        # Exponent ungerade
        {var reg5 uintD carry_rechts = # n Digits kopieren und um 1 Bit rechts shiften
           shiftrightcopy_loop_up(&TheLfloat(x)->data[0],r_MSDptr,len,1,0);
         var reg1 uintD* ptr = &r_MSDptr[(uintL)len];
         *ptr++ = carry_rechts; # Übertrag und
         clear_loop_up(ptr,len+1); # n+1 Nulldigits anhängen
        }
      uexp = (sintL)((sintL)uexp >> 1); # Exponent halbieren
      uexp = uexp + LF_exp_mid;
      {# Ergebnis allozieren:
       var reg1 object y = allocate_lfloat(len,uexp,0);
       var reg5 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
       # Wurzel ziehen:
       var DS w;
       var reg8 boolean exactp;
       UDS_sqrt(r_MSDptr,r_len,r_LSDptr, &w, exactp=);
       # w ist die Ganzzahl-Wurzel, eine n+1-Digit-Zahl.
       RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
       copy_loop_up(w.MSDptr,y_mantMSDptr,len); # NUDS nach y kopieren
       # Runden:
       if ( ((sintD)(w.LSDptr[-1]) >= 0) # nächstes Bit =0 -> abrunden
            || ( ((w.LSDptr[-1] & (bit(intDsize-1)-1)) ==0) # =1 und weitere Bits >0 oder Rest >0 -> aufrunden
                 && exactp
                 # round-to-even
                 && ((w.LSDptr[-2] & bit(0)) ==0)
          )    )
         # abrunden
         {}
         else
         # aufrunden
         { if ( inc_loop_down(&y_mantMSDptr[(uintL)len],len) )
             # Übertrag durchs Aufrunden
             { y_mantMSDptr[0] = bit(intDsize-1); # Mantisse := 10...0
               (TheLfloat(y)->expo)++; # Exponenten incrementieren
         }   }
       return y;
    }}}

# LF_to_I(x) wandelt ein Long-Float x, das eine ganze Zahl darstellt,
# in ein Integer um.
# kann GC auslösen
  local object LF_to_I (object x);
# Methode:
# Falls x=0.0, Ergebnis 0.
# Sonst (ASH Vorzeichen*Mantisse (e-16n)).
  local object LF_to_I(x)
    var reg4 object x;
    { var reg5 uintL uexp = TheLfloat(x)->expo;
      if (uexp==0) { return Fixnum_0; } # x=0.0 -> Ergebnis 0
      # Mantisse zu einem Integer machen:
     {SAVE_NUM_STACK # num_stack retten
      var reg1 uintD* MSDptr;
      var reg8 uintD* LSDptr;
      var reg2 uintC len = TheLfloat(x)->len;
      var reg3 uintC len1 = len+1; # brauche 1 Digit mehr
      if (uintCoverflow(len1)) { fehler_LF_toolong(); }
      num_stack_need(len1, MSDptr=,LSDptr=);
      copy_loop_up(&TheLfloat(x)->data[0],&MSDptr[1],len); # Mantisse kopieren
      MSDptr[0] = 0; # und zusätzliches Nulldigit
      # Mantisse ist die UDS MSDptr/len1/LSDptr.
      if (R_minusp(x))
        # x<0 -> Mantisse negieren:
        { neg_loop_down(LSDptr,len1); }
      # Vorzeichen*Mantisse ist die DS MSDptr/len1/LSDptr.
      pushSTACK(DS_to_I(MSDptr,len1)); # Vorzeichen*Mantisse als Integer
      RESTORE_NUM_STACK # num_stack zurück
      # e-16n = uexp-LF_exp_mid-16n als Integer bilden:
      {var reg6 uintL sub = LF_exp_mid + intDsize*(uintL)len;
       var reg7 object shiftcount = L2_to_I( (uexp<sub ? -1L : 0), uexp-sub);
       # (ASH Vorzeichen*Mantisse (- e 16n)) durchführen:
       return I_I_ash_I(popSTACK(),shiftcount);
    }}}

# I_to_LF(x,len) wandelt ein Integer x in ein Long-Float mit len Digits um
# und rundet dabei.
# kann GC auslösen
  local object I_to_LF (object x, uintC len);
# Methode:
# x=0 -> Ergebnis 0.0
# Merke Vorzeichen von x.
# x:=(abs x)
# Exponent:=(integer-length x)
# Mantisse enthalte die höchstwertigen 16n Bits des Integers x (wobei die
#   führenden 16-(e mod 16) Nullbits zu streichen sind).
# Runde die weiteren Bits weg:
#   Kommen keine mehr -> abrunden,
#   nächstes Bit = 0 -> abrunden,
#   nächstes Bit = 1 und Rest =0 -> round-to-even,
#   nächstes Bit = 1 und Rest >0 -> aufrunden.
# Bei Aufrundung: rounding overflow -> Mantisse um 1 Bit nach rechts schieben
#   und Exponent incrementieren.
  local object I_to_LF(x,len)
    var reg7 object x;
    var reg3 uintC len;
    { if (eq(x,Fixnum_0)) { encode_LF0(len, return); } # x=0 -> Ergebnis 0.0
     {var reg9 signean sign = R_sign(x); # Vorzeichen von x
      if (!(sign==0)) { x = I_minus_I(x); } # Betrag von x nehmen
      {var reg9 uintL exp = I_integer_length(x); # (integer-length x) < intDsize*2^intCsize
       # Teste, ob exp <= LF_exp_high-LF_exp_mid :
       if (   (log2_intDsize+intCsize < 32)
           && ((uintL)(intDsize*bitc(intCsize)-1) <= (uintL)(LF_exp_high-LF_exp_mid))
          )
         {} # garantiert exp <= intDsize*2^intCsize-1 <= LF_exp_high-LF_exp_mid
         else
         { if (!(exp <= (uintL)(LF_exp_high-LF_exp_mid))) { fehler_overflow(); } }
       # Long-Float bauen:
       pushSTACK(x);
       {var reg4 object y = allocate_lfloat(len,exp+LF_exp_mid,sign);
        var reg1 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
        var reg2 uintD* x_MSDptr;
        var reg6 uintC x_len;
        I_to_NDS_nocopy(popSTACK(), x_MSDptr=,x_len=,); # NDS zu x bilden, x_len>0
        # x_MSDptr/x_len/.. um (exp mod 16) Bits nach rechts shiften und in
        # y einfüllen (genauer: nur maximal len Digits davon):
        {var reg8 uintL shiftcount = exp % intDsize;
         # Die NDS fängt mit intDsize-shiftcount Nullbits an, dann kommt eine 1.
         if (x_len > len)
           { x_len -= 1+len;
             if (shiftcount>0)
               { var reg5 uintD carry_rechts =
                   shiftrightcopy_loop_up(&x_MSDptr[1],y_mantMSDptr,len,shiftcount,x_MSDptr[0]);
                 # Mantisse ist gefüllt. Runden:
                 if ( ((sintD)carry_rechts >= 0) # nächstes Bit =0 -> abrunden
                      || ( ((carry_rechts & (bit(intDsize-1)-1)) ==0) # =1, Rest >0 -> aufrunden
                           && !test_loop_up(&x_MSDptr[1+(uintL)len],x_len)
                           # round-to-even
                           && ((y_mantMSDptr[(uintL)len-1] & bit(0)) ==0)
                    )    )
                   goto ab; # aufrunden
                   else
                   goto auf; # aufrunden
               }
               else
               { copy_loop_up(&x_MSDptr[1],y_mantMSDptr,len);
                 # Mantisse ist gefüllt. Runden:
                {var reg5 uintD* ptr = &x_MSDptr[1+(uintL)len];
                 if ( (x_len==0) # keine Bits mehr -> abrunden
                      || ((sintD)ptr[0] >= 0) # nächstes Bit =0 -> abrunden
                      || ( ((ptr[0] & (bit(intDsize-1)-1)) ==0) # =1, Rest >0 -> aufrunden
                           && !test_loop_up(&ptr[1],x_len-1)
                           # round-to-even
                           && ((ptr[-1] & bit(0)) ==0)
                    )    )
                   goto ab; # aufrunden
                   else
                   goto auf; # aufrunden
               }}
             auf: # aufrunden
               if ( inc_loop_down(&y_mantMSDptr[(uintL)len],len) )
                 # Übertrag durchs Aufrunden
                 { y_mantMSDptr[0] = bit(intDsize-1); # Mantisse := 10...0
                   # Exponenten incrementieren:
                   if (   (log2_intDsize+intCsize < 32)
                       && ((uintL)(intDsize*bitc(intCsize)-1) < (uintL)(LF_exp_high-LF_exp_mid))
                      )
                     # garantiert exp < intDsize*2^intCsize-1 <= LF_exp_high-LF_exp_mid
                     { (TheLfloat(y)->expo)++; } # jetzt exp <= LF_exp_high-LF_exp_mid
                     else
                     { if ((TheLfloat(y)->expo)++ == LF_exp_high+1) { fehler_overflow(); } }
                 }
             ab: # abrunden
               ;
           }
           else # x_len <= len
           { var reg5 uintD carry_rechts;
             len -= x_len;
             x_len -= 1;
             if (shiftcount>0)
               { carry_rechts = shiftrightcopy_loop_up(&x_MSDptr[1],y_mantMSDptr,x_len,shiftcount,x_MSDptr[0]); }
               else
               { copy_loop_up(&x_MSDptr[1],y_mantMSDptr,x_len); carry_rechts = 0; }
            {var reg9 uintD* y_ptr = &y_mantMSDptr[x_len];
             *y_ptr++ = carry_rechts; # Carry als nächstes Digit
             clear_loop_up(y_ptr,len); # dann len-x_len Nulldigits
           }}
        }
        return y;
    }}}}

# RA_to_LF(x,len) wandelt eine rationale Zahl x in ein Long-Float
# mit len Digits um und rundet dabei.
# kann GC auslösen
  local object RA_to_LF (object x, uintC len);
# Methode:
# x ganz -> klar.
# x = +/- a/b mit Integers a,b>0:
#   Sei k,m so gewählt, daß
#     2^(k-1) <= a < 2^k, 2^(m-1) <= b < 2^m.
#   Dann ist 2^(k-m-1) < a/b < 2^(k-m+1).
#   Ergebnis-Vorzeichen := Vorzeichen von x.
#   Berechne k=(integer-length a) und m=(integer-length b).
#   Ergebnis-Exponent := k-m.
#   Ergebnis-Mantisse:
#     Berechne floor(2^(-k+m+16n+1)*a/b) :
#       Bei k-m>=16n+1 dividiere a durch (ash b (k-m-16n-1)),
#       bei k-m<16n+1 dividiere (ash a (-k+m+16n+1)) durch b.
#     Der erste Wert ist >=2^16n, <2^(16n+2).
#     Falls er >=2^(16n+1) ist, erhöhe Exponent um 1,
#       runde 2 Bits weg und schiebe dabei um 2 Bits nach rechts;
#     falls er <2^(16n+1) ist,
#       runde 1 Bit weg und schiebe dabei um 1 Bit nach rechts.
  local object RA_to_LF(x,len)
    var reg5 object x;
    var reg4 uintC len;
    { if (RA_integerp(x)) { return I_to_LF(x,len); }
      # x Ratio
      pushSTACK(TheRatio(x)->rt_den); # b
      x = TheRatio(x)->rt_num; # +/- a
     {var reg8 signean sign = R_sign(x); # Vorzeichen
      if (!(sign==0)) { x = I_minus_I(x); } # Betrag nehmen, liefert a
      pushSTACK(x);
      # Stackaufbau: b, a.
      {var reg3 sintL lendiff = I_integer_length(x) # (integer-length a)
                                - I_integer_length(STACK_1); # (integer-length b)
       # |lendiff| < intDsize*2^intCsize. Da für LF-Exponenten ein sintL zur
       # Verfügung steht, braucht man keinen Test auf Overflow oder Underflow.
       {var reg6 uintL difflimit = intDsize*(uintL)len + 1; # 16n+1
        var reg1 object zaehler;
        var reg2 object nenner;
        if (lendiff > (sintL)difflimit)
          # 0 <= k-m-16n-1 < k < intDsize*2^intCsize
          { nenner = I_I_ash_I(STACK_1,
                               (log2_intDsize+intCsize<=oint_addr_len # intDsize*2^intCsize <= 2^oint_addr_len ?
                                 ? fixnum( (uintL)(lendiff - difflimit))
                                 : UL_to_I((uintL)(lendiff - difflimit))
                              ));
            zaehler = popSTACK(); # a
            skipSTACK(1);
          }
          else
          # 0 < -k+m+16n+1 <= m+1 + 16n < intDsize*2^intCsize + intDsize*2^intCsize
          { var reg7 object shiftcount = # -k+m+16n+1
              (log2_intDsize+intCsize+1<=oint_addr_len # 2*intDsize*2^intCsize <= 2^oint_addr_len ?
                ? fixnum( (uintL)(difflimit - lendiff))
                : UL_to_I((uintL)(difflimit - lendiff))
              );
            zaehler = I_I_ash_I(popSTACK(),shiftcount); # (ash a -k+m+16n+1)
            nenner = popSTACK(); # b
          }
        # Division zaehler/nenner durchführen:
        I_I_divide_I_I(zaehler,nenner);
       }
       # Stackaufbau: q, r.
       # 2^16n <= q < 2^(16n+2), also ist q Bignum mit n+1 Digits.
       {var reg7 object y = allocate_lfloat(len,lendiff+LF_exp_mid,sign); # neues Long-Float
        var reg2 uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
        {var reg1 uintD* q_MSDptr = &TheBignum(STACK_1)->data[0];
         if (q_MSDptr[0] == 1) # erstes Digit =1 oder =2,3 ?
           # 2^16n <= q < 2^(16n+1), also 2^(k-m-1) < a/b < 2^(k-m).
           { # Mantisse mit einer Schiebeschleife um 1 Bit nach rechts füllen:
             var reg6 uintD rounding_bit =
               shiftrightcopy_loop_up(&q_MSDptr[1],y_mantMSDptr,len,1,1);
             if ( (rounding_bit == 0) # herausgeschobenes Bit =0 -> abrunden
                  || ( eq(STACK_0,Fixnum_0) # =1 und Rest r > 0 -> aufrunden
                       # round-to-even
                       && ((y_mantMSDptr[(uintL)len-1] & bit(0)) ==0)
                )    )
               goto ab; # abrunden
               else
               goto auf; # aufrunden
           }
           else
           # 2^(16n+1) <= q < 2^(16n+2), also 2^(k-m) < a/b < 2^(k-m+1).
           { # Mantisse mit einer Schiebeschleife um 2 Bit nach rechts füllen:
             var reg6 uintD rounding_bits =
               shiftrightcopy_loop_up(&q_MSDptr[1],y_mantMSDptr,len,2,q_MSDptr[0]);
             (TheLfloat(y)->expo)++; # Exponenten incrementieren auf k-m+1
             if ( ((sintD)rounding_bits >= 0) # herausgeschobenes Bit =0 -> abrunden
                  || ( ((rounding_bits & bit(intDsize-2)) ==0) # =1 und nächstes Bit =1 oder Rest r > 0 -> aufrunden
                       && eq(STACK_0,Fixnum_0)
                       # round-to-even
                       && ((y_mantMSDptr[(uintL)len-1] & bit(0)) ==0)
                )    )
               goto ab; # abrunden
               else
               goto auf; # aufrunden
           }
        }
        auf: # aufrunden
          { if ( inc_loop_down(&y_mantMSDptr[(uintL)len],len) )
              # Übertrag durchs Aufrunden
              { y_mantMSDptr[0] = bit(intDsize-1); # Mantisse := 10...0
                (TheLfloat(y)->expo)++; # Exponenten incrementieren
          }   }
        ab: # abrunden
        skipSTACK(2);
        return y;
    }}}}

