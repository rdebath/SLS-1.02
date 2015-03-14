# Hilfsfunktion zur Eingabe von Integers

# Wandelt eine Ziffernfolge in ein Integer >=0 um.
# DIGITS_to_I(MSBptr,len,base)
# > base: Stellenwertsystem-Basis, >=2, <=36
# > MSBptr/len/..: Ziffernfolge, bestehend aus Punkten (werden überlesen)
#     und Ziffern/Buchstaben mit Wert < base.
# < ergebnis: der dargestellte Integer >=0
# kann GC auslösen
  local object DIGITS_to_I (uintB* MSBptr, uintL len, uintD base);
  local object DIGITS_to_I(MSBptr,len,base)
    var reg2 uintB* MSBptr;
    var reg3 uintL len;
    var reg8 uintD base;
    { SAVE_NUM_STACK # num_stack retten
      var reg6 uintD* erg_MSDptr;
      var reg7 uintC erg_len;
      var reg5 uintD* erg_LSDptr;
      # Platz fürs Ergebnis:
      # 1+ceiling(len*log(base)/(intDsize*log(2))) oder etwas mehr Digits
      var reg9 uintL need = 1+floor(len,intDsize*256); # > len/(intDsize*256) >=0
      switch (base) # need mit ceiling(256*log(base)/log(2)) multiplizieren:
        { case 2: need = 256*need; break;
          case 3: need = 406*need; break;
          case 4: need = 512*need; break;
          case 5: need = 595*need; break;
          case 6: need = 662*need; break;
          case 7: need = 719*need; break;
          case 8: need = 768*need; break;
          case 9: need = 812*need; break;
          case 10: need = 851*need; break;
          case 11: need = 886*need; break;
          case 12: need = 918*need; break;
          case 13: need = 948*need; break;
          case 14: need = 975*need; break;
          case 15: need = 1001*need; break;
          case 16: need = 1024*need; break;
          case 17: need = 1047*need; break;
          case 18: need = 1068*need; break;
          case 19: need = 1088*need; break;
          case 20: need = 1107*need; break;
          case 21: need = 1125*need; break;
          case 22: need = 1142*need; break;
          case 23: need = 1159*need; break;
          case 24: need = 1174*need; break;
          case 25: need = 1189*need; break;
          case 26: need = 1204*need; break;
          case 27: need = 1218*need; break;
          case 28: need = 1231*need; break;
          case 29: need = 1244*need; break;
          case 30: need = 1257*need; break;
          case 31: need = 1269*need; break;
          case 32: need = 1280*need; break;
          case 33: need = 1292*need; break;
          case 34: need = 1303*need; break;
          case 35: need = 1314*need; break;
          case 36: need = 1324*need; break;
          default: NOTREACHED
        }
      # Nun gilt need >= len*log(base)/(intDsize*log(2)).
      need += 1;
      if ((intCsize < 32) && (need > (uintL)(bitc(intCsize)-1))) { BN_ueberlauf(); }
      num_stack_need(need,,erg_LSDptr=);
      erg_MSDptr = erg_LSDptr; erg_len = 0;
      # Ziffern einzeln draufaddieren:
      dotimesL(len,len,
        { # erg_MSDptr/erg_len/erg_LSDptr ist eine NUDS, erg_len < need.
          var reg1 uintB ch = *MSBptr++; # nächstes Character
          if (!(ch=='.')) # Punkt überlesen
            { # Wert von ch ('0'-'9','A'-'Z','a'-'z') bilden:
              ch = ch - '0';
              if (ch > '9'-'0') # keine Ziffer?
                { ch = ch+'0'-'A'+10;
                  if (ch > 'Z'-'A'+10) # kein Großbuchstabe?
                    { ch = ch+'A'-'a'; } # dann ein Kleinbuchstabe
                }
              # multipliziere erg mit base und addiere ch:
             {var reg4 uintD carry = mulusmall_loop_down(base,erg_LSDptr,erg_len,ch);
              if (!(carry==0))
                # muß NUDS vergrößern:
                { *--erg_MSDptr = carry; erg_len++; }
        }   }});
      RESTORE_NUM_STACK # num_stack (vorzeitig) zurück
      return NUDS_to_I(erg_MSDptr,erg_len);
    }

