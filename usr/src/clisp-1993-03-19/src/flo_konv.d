# Konversionen zwischen Floating-Points

# Konversionen ohne Rundung:

# SF_to_FF(x) wandelt ein Short-Float x in ein Single-Float um.
# kann GC auslösen
  local object SF_to_FF (object x);
  local object SF_to_FF(x)
    var reg1 object x;
    { # Falls
      # 1. Keine Konversion im Exponenten nötig,
      # 2. Vorzeichen/Exponent/Mantisse ist im SF (wie im FF) dicht gepackt,
      # 3. der Shift, der die Mantissen erweitert, schiebt das Vorzeichen nach
      #    Bit 31,
      # kann einfach geshiftet werden.
      #if (SF_exp_len==FF_exp_len) && (SF_exp_low>=FF_exp_low) && (SF_exp_mid==FF_exp_mid) && (SF_exp_high<=FF_exp_high) && (vorz_bit_o==SF_exp_len+SF_exp_shift)
        # Dadurch auch 31-vorz_bit_o = 31-SF_exp_len-SF_exp_shift
        #                            = 31-FF_exp_len-SF_mant_len-SF_mant_shift
        #                            = FF_mant_len-SF_mant_len-SF_mant_shift
        { return
            allocate_ffloat(
              ((uint32)((oint)(x) >> SF_mant_shift) << (FF_mant_len-SF_mant_len))
                           );
        }
      #else
        # x entpacken:
        var reg4 signean sign;
        var reg3 sintWL exp;
        var reg2 uint32 mant;
        SF_decode(x, { return FF_0; }, sign=,exp=,mant=);
        # Mantisse um 23-16=7 Bits nach links schieben:
        encode_FF(sign,exp,mant<<(FF_mant_len-SF_mant_len), return);
      #endif
    }

# SF_to_DF(x) wandelt ein Short-Float x in ein Double-Float um.
# kann GC auslösen
  local object SF_to_DF (object x);
  local object SF_to_DF(x)
    var reg1 object x;
    { # x entpacken:
      var reg4 signean sign;
      var reg3 sintWL exp;
      var reg2 uint32 mant;
      SF_decode(x, { return DF_0; }, sign=,exp=,mant=);
      # Mantisse um 52-16=36 Nullbits erweitern:
      encode_DF(sign,exp,mant<<(DF_mant_len-SF_mant_len-32),0, return);
    }

# SF_to_LF(x,len) wandelt ein Short-Float x in ein Long-Float mit len Digits um.
# > uintC len: gewünschte Anzahl Digits, >=LF_minlen
# kann GC auslösen
  local object SF_to_LF (object x, uintC len);
  local object SF_to_LF(x,len)
    var reg1 object x;
    var reg2 uintC len;
    { # x entpacken:
      var reg5 signean sign;
      var reg4 sintL exp;
      var reg3 uint32 mant;
      SF_decode(x, { encode_LF0(len, return); }, sign=,exp=(sintL),mant=);
      # Long-Float allozieren,
      # Mantisse mit intDsize*len-SF_mant_len-1 Nullbits auffüllen:
     {var reg6 object y = allocate_lfloat(len,exp+LF_exp_mid,sign);
      var reg1 uintD* ptr = &TheLfloat(y)->data[0];
      # erste k := ceiling(SF_mant_len+1,intDsize) Digits mit mant füllen:
      mant = mant << (ceiling(SF_mant_len+1,intDsize)*intDsize-(SF_mant_len+1));
      set_max32_Dptr(SF_mant_len+1,ptr,mant);
      clear_loop_up(&ptr[ceiling(SF_mant_len+1,intDsize)],len-ceiling(SF_mant_len+1,intDsize));
      return y;
    }}

# FF_to_DF(x) wandelt ein Single-Float x in ein Double-Float um.
# kann GC auslösen
  local object FF_to_DF (object x);
  local object FF_to_DF(x)
    var reg1 object x;
    { # x entpacken:
      var reg4 signean sign;
      var reg3 sintWL exp;
      var reg2 uint32 mant;
      FF_decode(x, { return DF_0; }, sign=,exp=,mant=);
      # Mantisse um 52-23=29 Nullbits erweitern:
      encode_DF(sign,exp,mant>>(32-(DF_mant_len-FF_mant_len)),mant<<(DF_mant_len-FF_mant_len), return);
    }

# FF_to_LF(x,len) wandelt ein Single-Float x in ein Long-Float mit len Digits um.
# > uintC len: gewünschte Anzahl Digits, >=LF_minlen
# kann GC auslösen
  local object FF_to_LF (object x, uintC len);
  local object FF_to_LF(x,len)
    var reg1 object x;
    var reg2 uintC len;
    { # x entpacken:
      var reg5 signean sign;
      var reg4 sintL exp;
      var reg3 uint32 mant;
      FF_decode(x, { encode_LF0(len, return); }, sign=,exp=(sintL),mant=);
      # Long-Float allozieren,
      # Mantisse mit intDsize*len-FF_mant_len-1 Nullbits auffüllen:
     {var reg6 object y = allocate_lfloat(len,exp+LF_exp_mid,sign);
      var reg1 uintD* ptr = &TheLfloat(y)->data[0];
      # erste k := ceiling(FF_mant_len+1,intDsize) Digits mit mant füllen:
      mant = mant << (ceiling(FF_mant_len+1,intDsize)*intDsize-(FF_mant_len+1));
      set_max32_Dptr(FF_mant_len+1,ptr,mant);
      clear_loop_up(&ptr[ceiling(FF_mant_len+1,intDsize)],len-ceiling(FF_mant_len+1,intDsize));
      return y;
    }}

# DF_to_LF(x,len) wandelt ein Double-Float x in ein Long-Float mit len Digits um.
# > uintC len: gewünschte Anzahl Digits, >=LF_minlen
# kann GC auslösen
  local object DF_to_LF (object x, uintC len);
  local object DF_to_LF(x,len)
    var reg1 object x;
    var reg2 uintC len;
    { # x entpacken:
      var reg5 signean sign;
      var reg4 sintL exp;
      var reg3 uint32 manthi;
      var reg3 uint32 mantlo;
      DF_decode(x, { encode_LF0(len, return); }, sign=,exp=(sintL),manthi=,mantlo=);
      # Long-Float allozieren,
      # Mantisse mit intDsize*len-DF_mant_len-1 Nullbits auffüllen:
     {var reg6 object y = allocate_lfloat(len,exp+LF_exp_mid,sign);
      var reg1 uintD* ptr = &TheLfloat(y)->data[0];
      # erste k := ceiling(DF_mant_len+1,intDsize) Digits mit mant füllen:
      #define shiftcount  (ceiling(DF_mant_len+1,intDsize)*intDsize-(DF_mant_len+1))
      manthi = (manthi<<shiftcount) | (mantlo>>(32-shiftcount));
      mantlo = mantlo<<shiftcount;
      #undef shiftcount
      set_max32_Dptr(DF_mant_len+1-32,ptr,manthi);
      set_32_Dptr(&ptr[ceiling(DF_mant_len+1-32,intDsize)],mantlo);
      clear_loop_up(&ptr[ceiling(DF_mant_len+1,intDsize)],len-ceiling(DF_mant_len+1,intDsize));
      return y;
    }}

# Konversionen mit Rundung:

# FF_to_SF(x) wandelt ein Single-Float x in ein Short-Float um.
  local object FF_to_SF (object x);
  local object FF_to_SF(x)
    var reg1 object x;
    { # x entpacken:
      var reg4 signean sign;
      var reg3 sintWL exp;
      var reg2 uint32 mant;
      FF_decode(x, { return SF_0; }, sign=,exp=,mant=);
      # 23-16 Bits wegrunden:
      #define shiftcount  (FF_mant_len-SF_mant_len)
      if ( ((mant & bit(shiftcount-1)) ==0) # Bit 6 war 0 -> abrunden
           || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 5..0 >0 -> aufrunden
                # round-to-even
                && ((mant & bit(shiftcount)) ==0)
         )    )
        # abrunden
        { mant = mant >> shiftcount; }
        else
        # aufrunden
        { mant = mant >> shiftcount;
          mant = mant+1;
          if (mant >= bit(SF_mant_len+1))
            # Überlauf durchs Runden
            { mant = mant>>1; exp = exp+1; } # Mantisse rechts schieben
        }
      #undef shiftcount
      encode_SF(sign,exp,mant, return);
    }

# DF_to_SF(x) wandelt ein Double-Float x in ein Short-Float um.
  local object DF_to_SF (object x);
  local object DF_to_SF(x)
    var reg1 object x;
    { # x entpacken:
      var reg4 signean sign;
      var reg3 sintWL exp;
      var reg2 uint32 manthi;
      var reg2 uint32 mantlo;
      DF_decode(x, { return SF_0; }, sign=,exp=,manthi=,mantlo=);
      # 52-16=36 Bits wegrunden:
      #define shiftcount  (DF_mant_len-SF_mant_len-32)
      if ( ((manthi & bit(shiftcount-1)) ==0) # Bit 35 war 0 -> abrunden
           || ( ((manthi & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 34..0 >0 -> aufrunden
                && (mantlo==0)
                # round-to-even
                && ((manthi & bit(shiftcount)) ==0)
         )    )
        # abrunden
        { manthi = manthi >> shiftcount; }
        else
        # aufrunden
        { manthi = manthi >> shiftcount;
          manthi = manthi+1;
          if (manthi >= bit(SF_mant_len+1))
            # Überlauf durchs Runden
            { manthi = manthi>>1; exp = exp+1; } # Mantisse rechts schieben
        }
      #undef shiftcount
      encode_SF(sign,exp,manthi, return);
    }

# LF_to_SF(x) wandelt ein Long-Float x in ein Short-Float um.
  local object LF_to_SF (object x);
  local object LF_to_SF(x)
    var reg2 object x;
    { # x entpacken:
      var reg6 signean sign;
      var reg5 sintL exp;
      var reg1 uintD* ptr;
      var reg4 uintC len;
      var reg3 uint32 mant;
      LF_decode(x, { return SF_0; }, sign=,exp=,ptr=,len=,);
      # intDsize*len-SF_mant_len-1 Bits der Mantisse wegrunden:
      # erste k := ceiling(SF_mant_len+2,intDsize) Digits nach mant holen:
      mant = get_max32_Dptr(SF_mant_len+2,ptr);
      ptr += ceiling(SF_mant_len+2,intDsize);
      #define shiftcount  (ceiling(SF_mant_len+2,intDsize)*intDsize-(SF_mant_len+1))
      if ( ((mant & bit(shiftcount-1)) ==0) # Bit 14 war 0 -> abrunden
           || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 13..0 >0 -> aufrunden
                && !test_loop_up(ptr,len-ceiling(SF_mant_len+2,intDsize)) # weitere Bits /=0 -> aufrunden
                # round-to-even
                && ((mant & bit(shiftcount)) ==0)
         )    )
        # abrunden
        { mant = mant >> shiftcount; }
        else
        # aufrunden
        { mant = mant >> shiftcount;
          mant = mant+1;
          if (mant >= bit(SF_mant_len+1))
            # Überlauf durchs Runden
            { mant = mant>>1; exp = exp+1; } # Mantisse rechts schieben
        }
      #undef shiftcount
      encode_SF(sign,exp,mant, return);
    }

# DF_to_FF(x) wandelt ein Double-Float x in ein Single-Float um.
# kann GC auslösen
  local object DF_to_FF (object x);
  local object DF_to_FF(x)
    var reg1 object x;
    { # x entpacken:
      var reg4 signean sign;
      var reg3 sintWL exp;
      var reg2 uint32 manthi;
      var reg2 uint32 mantlo;
      DF_decode(x, { return FF_0; }, sign=,exp=,manthi=,mantlo=);
      # 52-23=29 Bits wegrunden:
      #define shiftcount  (DF_mant_len-FF_mant_len)
      manthi = (manthi << (32-shiftcount)) | (mantlo >> shiftcount);
      if ( ((mantlo & bit(shiftcount-1)) ==0) # Bit 28 war 0 -> abrunden
           || ( ((mantlo & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 27..0 >0 -> aufrunden
                # round-to-even
                && ((mantlo & bit(shiftcount)) ==0)
         )    )
        # abrunden
        {}
        else
        # aufrunden
        { manthi = manthi+1;
          if (manthi >= bit(FF_mant_len+1))
            # Überlauf durchs Runden
            { manthi = manthi>>1; exp = exp+1; } # Mantisse rechts schieben
        }
      #undef shiftcount
      encode_FF(sign,exp,manthi, return);
    }

# LF_to_FF(x) wandelt ein Long-Float x in ein Single-Float um.
# kann GC auslösen
  local object LF_to_FF (object x);
  local object LF_to_FF(x)
    var reg2 object x;
    { # x entpacken:
      var reg6 signean sign;
      var reg5 sintL exp;
      var reg1 uintD* ptr;
      var reg4 uintC len;
      var reg3 uint32 mant;
      LF_decode(x, { return FF_0; }, sign=,exp=,ptr=,len=,);
      # intDsize*len-FF_mant_len-1 Bits der Mantisse wegrunden:
      # erste k := ceiling(FF_mant_len+2,intDsize) Digits nach mant holen:
      mant = get_max32_Dptr(FF_mant_len+2,ptr);
      ptr += ceiling(FF_mant_len+2,intDsize);
      #define shiftcount  (ceiling(FF_mant_len+2,intDsize)*intDsize-(FF_mant_len+1))
      if ( ((mant & bit(shiftcount-1)) ==0) # Bit 7 war 0 -> abrunden
           || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 6..0 >0 -> aufrunden
                && !test_loop_up(ptr,len-ceiling(FF_mant_len+2,intDsize)) # weitere Bits /=0 -> aufrunden
                # round-to-even
                && ((mant & bit(shiftcount)) ==0)
         )    )
        # abrunden
        { mant = mant >> shiftcount; }
        else
        # aufrunden
        { mant = mant >> shiftcount;
          mant = mant+1;
          if (mant >= bit(FF_mant_len+1))
            # Überlauf durchs Runden
            { mant = mant>>1; exp = exp+1; } # Mantisse rechts schieben
        }
      #undef shiftcount
      encode_FF(sign,exp,mant, return);
    }

# LF_to_DF(x) wandelt ein Long-Float x in ein Double-Float um.
# kann GC auslösen
  local object LF_to_DF (object x);
  local object LF_to_DF(x)
    var reg2 object x;
    { # x entpacken:
      var reg6 signean sign;
      var reg5 sintL exp;
      var reg1 uintD* ptr;
      var reg4 uintC len;
      var reg3 uint32 manthi;
      var reg3 uint32 mantlo;
      LF_decode(x, { return DF_0; }, sign=,exp=,ptr=,len=,);
      # intDsize*len-DF_mant_len-1 Bits der Mantisse wegrunden:
      # erste k := ceiling(DF_mant_len+2,intDsize) Digits nach manthi,mantlo holen:
      manthi = get_max32_Dptr(DF_mant_len+2-32,ptr);
      mantlo = get_32_Dptr(&ptr[ceiling(DF_mant_len+2-32,intDsize)]);
      ptr += ceiling(DF_mant_len+2,intDsize);
      #define shiftcount  (ceiling(DF_mant_len+2,intDsize)*intDsize-(DF_mant_len+1))
      if ( ((mantlo & bit(shiftcount-1)) ==0) # Bit 10 war 0 -> abrunden
           || ( ((mantlo & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 9..0 >0 -> aufrunden
                && !test_loop_up(ptr,len-ceiling(DF_mant_len+2,intDsize)) # weitere Bits /=0 -> aufrunden
                # round-to-even
                && ((mantlo & bit(shiftcount)) ==0)
         )    )
        # abrunden
        { mantlo = (manthi << (32-shiftcount)) | (mantlo >> shiftcount);
          manthi = manthi >> shiftcount;
        }
        else
        # aufrunden
        { mantlo = (manthi << (32-shiftcount)) | (mantlo >> shiftcount);
          manthi = manthi >> shiftcount;
          mantlo = mantlo+1;
          if (mantlo==0)
            { manthi = manthi+1;
              if (manthi >= bit(DF_mant_len+1-32))
                # Überlauf durchs Runden
                { manthi = manthi>>1; exp = exp+1; } # Mantisse rechts schieben
        }   }
      #undef shiftcount
      encode_DF(sign,exp,manthi,mantlo, return);
    }

