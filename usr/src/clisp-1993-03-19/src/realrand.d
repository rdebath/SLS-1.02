# Funktionen für Zufallszahlen

# Zufallszahlengenerator nach [Knuth: The Art of Computer Programming, Vol. II,
# Seminumerical Algorithms, 3.3.4., Table 1, Line 30], nach C. Haynes:
# X eine 64-Bit-Zahl. Iteration X := (a*X+c) mod m
# mit m=2^64, a=6364136223846793005, c=1.

# random_L(randomstate) liefert eine neue Zufallszahl.
# > randomstate: ein Random-State, wird verändert
# < ergebnis: eine 32-Bit-Zufallszahl
  local uint32 random_L (object randomstate);
  local uint32 random_L(randomstate)
    var reg4 object randomstate;
    { var reg5 object seed = # letzte Zahl, ein Simple-Bit-Vektor mit 64 Bits
        The_Random_state(randomstate)->random_state_seed;
      var reg1 uintD* seedMSDptr = (uintD*)(&TheSbvector(seed)->data[0]);
      # Multiplikator a=6364136223846793005 = 0x5851F42D4C957F2D :
      local var uintD multiplier[64/intDsize] =
        { D(0x58,0x51,0xF4,0x2D,) D(0x4C,0x95,0x7F,0x2D,) } ;
      var uintD product[128/intDsize]; # Produkt
      # multiplizieren:
      mulu_2loop_down(&seedMSDptr[64/intDsize],64/intDsize,
                      &multiplier[64/intDsize],64/intDsize,
                      &product[128/intDsize]
                     );
      # letzte 64 Bits holen:
     {var reg3 uint32 seed_hi = get_32_Dptr(&product[64/intDsize]);
      var reg2 uint32 seed_lo = get_32_Dptr(&product[96/intDsize]);
      seed_lo += 1; if (seed_lo==0) { seed_hi += 1; } # um 1 erhöhen
      # seed neu füllen:
      set_32_Dptr(seedMSDptr,seed_hi); set_32_Dptr(&seedMSDptr[32/intDsize],seed_lo);
      # mittlere 32 Bits als Ergebnis:
      return highlow32(low16(seed_hi),high16(seed_lo));
    }}

# random_UDS(randomstate,MSDptr,len) füllt die UDS MSDptr/len/..
# mit len Zufallsdigits.
# > randomstate: ein Random-State, wird verändert
# > MSDptr/len/..: wo die Zufallsdigits abgelegt werden sollen
# > len: gewünschte Anzahl von Zufallsdigits
  local void random_UDS (object randomstate, uintD* MSDptr, uintC len);
  local void random_UDS(randomstate,ptr,len)
    var reg4 object randomstate;
    var reg1 uintD* ptr;
    var reg5 uintC len;
    { var reg3 uintC count;
      dotimesC(count,floor(len,32/intDsize),
        { var reg2 uint32 next = random_L(randomstate); # weitere 32/intDsize Digits besorgen
          set_32_Dptr(ptr,next); ptr += 32/intDsize;
        });
      len = len % (32/intDsize); # Anzahl noch fehlender Digits
      if (len>0)
        { var reg2 uint32 next =  random_L(randomstate); # weitere 32/intDsize Digits besorgen
          set_max32_Dptr(intDsize*len,ptr,next);
        }
    }

# I_random_I(randomstate,n) liefert zu einem Integer n>0 ein zufälliges
# Integer x mit 0 <= x < n.
# > randomstate: ein Random-State, wird verändert
# kann GC auslösen
  local object I_random_I (object randomstate, object n);
  local object I_random_I(randomstate,n)
    var reg7 object randomstate;
    var reg2 object n;
    { SAVE_NUM_STACK # num_stack retten
      var reg5 uintD* n_MSDptr;
      var reg3 uintC n_len;
      var reg6 uintD* n_LSDptr;
      I_to_NDS_nocopy(n, n_MSDptr=,n_len=,n_LSDptr=); # Digit sequence >0 zu n
     {var reg4 uintD* MSDptr;
      var reg2 uintC len = n_len + ceiling(16,intDsize); # 16 Bits mehr
      if ((intCsize < 32) && (len < n_len)) { BN_ueberlauf(); }
      # neue UDS mit len Zufallsdigits bilden:
      num_stack_need(len,MSDptr=,);
      random_UDS(randomstate,MSDptr,len);
      # und durch n dividieren:
      {var DS q;
       var DS r;
       UDS_divide(MSDptr,len,&MSDptr[(uintL)len], n_MSDptr,n_len,n_LSDptr, &q,&r);
       RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
       # Rest in Integer umwandeln:
       return NUDS_to_I(r.MSDptr,r.len);
    }}}

# F_random_F(randomstate,n) liefert zu einem Float n>0 ein zufälliges
# Float x mit 0 <= x < n.
# > randomstate: ein Random-State, wird verändert
# kann GC auslösen
  local object F_random_F (object randomstate, object n);
  local object F_random_F(randomstate,n)
    var reg7 object randomstate;
    var reg5 object n;
    {  pushSTACK(n);
     { var reg2 uintL d = F_float_digits(n); # d = (float-digits n) > 0
       # Bilde neue UDS mit d Zufallsbits:
       SAVE_NUM_STACK # num_stack retten
       var reg3 uintL len = ceiling(d,intDsize);
       var reg4 uintD* MSDptr;
       num_stack_need_1(len,MSDptr=,);
       random_UDS(randomstate,MSDptr,len); # len (>0) Zufallsdigits
       # von intDsize*ceiling(d/intDsize) auf d Bits herunterschneiden:
       { var reg9 uintL dr = d % intDsize; if (dr>0) { MSDptr[0] &= (bit(dr)-1); } }
       # in Integer umwandeln:
      {var reg1 object mant = UDS_to_I(MSDptr,len);
       RESTORE_NUM_STACK # num_stack zurück
       # Bilde  Zufalls-Float zwischen 0 und 1
       #        = (scale-float (float Zufalls-Integer,d_Bits n) (- d)) :
       mant = I_F_float_F(mant,STACK_0); # in Float vom Typ von n umwandeln
       pushSTACK(mant);
       {var reg4 object minus_d = L_to_I(-d); # (- d)
        mant = popSTACK();
        mant = F_I_scale_float_F(mant,minus_d);
       }
       # Multipliziere es mit n :
       mant = F_F_mal_F(mant,STACK_0);
       # mant ist ein Zufalls-Float >=0, <=n.
       if (eql(mant,popSTACK())) # mit n vergleichen
         # falls (durch Rundung) mant=n, durch 0 ersetzen:
         { mant = I_F_float_F(Fixnum_0,mant); }
       return mant;
    }}}

