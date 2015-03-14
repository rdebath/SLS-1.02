# Hash-Tabellen in CLISP
# Bruno Haible 29.1.1993

#include "lispbibl.c"
#include "arilev0.c" # für Hashcode-Berechnung
#include "aridecl.c" # für Short-Floats


# Aufbau einer Hash-Tabelle:
# Es werden Paare (Key . Value) abgelegt.
# In einem Vektor, der durch (hashcode Key) indiziert wird.
# Damit ein laufendes MAPHASH von einer GC unbeeinflußt bleibt, wird dieser
# Vektor bei GC nicht reorganisiert. Da aber bei GC jeder (hashcode Key)
# sich ändern kann, bauen wir eine weitere Indizierungsstufe ein:
# (hashcode Key) indiziert einen Index-Vektor; dort steht ein Index in
# den Key-Value-Vektor, und dort befindet sich (Key . Value).
# Um Speicherplatz zu sparen, legen wir nicht ein Cons (Key . Value)
# im Vektor ab, sondern einfach Key und Value hintereinander.
# Kollisionen [mehrere Keys haben denselben (hascode Key)] möchte man durch
# Listen beheben. Da aber der Key-Value-Vektor (wieder wegen MAPHASH) bei GC
# unbeeinflußt bleiben soll und GC die Menge der Kollisionen verändert,
# brauchen wir einen weiteren Index-Vektor, genannt Next-Vektor, der
# "parallel" zum Key-Value-Vektor liegt und eine "Listen"struktur enthält.
# Skizze:
#   Key --> (hashcode Key) als Index in Index-Vaktor.
#   Key1 --> 3, Key2 --> 1, Key4 --> 3.
#   Index-Vektor      #( nix {IndexKey2} nix {IndexKey1,IndexKey4} nix ... )
#                   = #( nix 1 nix 0 nix ... )
#   Next-Vektor       #(     3        nix       leer      nix      leer   )
#   Key-Value-Vektor  #( Key1 Val1 Key2 Val2 leer leer Key4 Val4 leer leer)
# Zugriff auf ein (Key . Value) - Paar geschieht also folgendermaßen:
#   index := (aref Index-Vektor (hashcode Key))
#   until index = nix
#     if (eql Key (aref KVVektor 2*index)) return (aref KVVektor 2*index+1)
#     index := (aref Next-Vektor index) ; "CDR" der Liste nehmen
#   return notfound.
# Wird Index-Vektor vergrößert, müssen alle Hashcodes und der Inhalt von
# Index-Vektor und der Inhalt von Next-Vektor neu berechnet werden.
# Werden Next-Vektor und Key-Value-Vektor vergrößert, so können die
# restlichen Elemente mit "leer" gefüllt werden, ohne daß ein Hashcode neu
# berechnet werden müßte.
# Damit nach CLRHASH oder vielfachem REMHASH, wenn eine Tabelle viel
# weniger Elemente enthält als ihre Kapazität, ein MAPHASH schnell geht,
# könnte man die Einträge im Key-Value-Vektor "links gepackt" halten, d.h.
# alle "leer" kommen rechts. Dann braucht man bei MAPHASH nur die Elemente
# count-1,...,1,0 des Key-Value-Vektors abzugrasen. Aber REMHASH muß
# - nachdem es eine Lücke gelassen hat - das hinterste Key-Value-Paar
# (Nummer count-1) in die Lücke umfüllen.
# Wir behandeln solche Fälle dadurch, daß wir bei CLRHASH und REMHASH
# eventuell den Key-Value-Vektor und den Next-Vektor verkleinern.
# Damit PUTHASH einen freien Eintrag findet, halten wir die "leer" im
# Next-Vektor in einer Frei"liste".
# Die Längen von Index-Vektor und Next-Vektor sind unabhängig voneinander.
# Wir wählen sie hier im Verhältnis 2:1.
# Die Hash-Tabelle wird vergrößert, wenn die Freiliste leer ist, d.h.
# COUNT > MAXCOUNT wird. Dabei werden MAXCOUNT und SIZE mit REHASH-SIZE (>1)
# multipliziert.
# Die Hash-Tabelle wird verkleinert, wenn COUNT < MINCOUNT wird. Dabei
# werden MAXCOUNT und SIZE mit 1/REHASH-SIZE (<1) multipliziert. Damit nach
# einer Vergrößerung der Tabelle COUNT gleichviel nach oben wie nach unten
# variieren kann (auf einer logarithmischen Skala), wählen wir
# MINCOUNT = MAXCOUNT / REHASH-SIZE^2 .

# Datenstruktur der Hash-Tabelle (siehe LISPBIBL.D):
# recflags codiert den Typ und den Zustand der Hash-Tabelle:
#   Bit 0 gesetzt, wenn EQ-Hashtabelle
#   Bit 1 gesetzt, wenn EQL-Hashtabelle
#   Bit 2 gesetzt, wenn EQUAL-Hashtabelle
#   Bit 3-6 =0
#   Bit 7 gesetzt, wenn Tabelle nach GC reorganisiert werden muß
# ht_size                Fixnum>0 = Länge der ITABLE
# ht_maxcount            Fixnum>0 = Länge der NTABLE
# ht_itable              Index-Vektor der Länge SIZE, enthält Indizes
# ht_ntable              Next-Vektor der Länge MAXCOUNT, enthält Indizes
# ht_kvtable             Key-Value-Vektor, Vektor der Länge 2*MAXCOUNT
# ht_freelist            Start-Index der Freiliste im Next-Vektor
# ht_count               Anzahl der Einträge in der Table, Fixnum >=0, <=MAXCOUNT
# ht_rehash_size         Wachstumsrate bei Reorganisation. Float >1.1
# ht_mincount_threshold  Verhältnis MINCOUNT/MAXCOUNT = 1/rehash-size^2
# ht_mincount            Fixnum>=0, untere Grenze für COUNT
# Eintrag "leer" im Key-Value-Vektor ist = #<UNBOUND>.
# Eintrag "leer" im Next-Vektor ist durch die Freiliste gefüllt.
# Eintrag "nix" im Index-Vektor und im Next-Vektor ist = #<UNBOUND>.
  #define leer  unbound
  #define nix   unbound

# Rotiert einen Hashcode x um n Bits nach links (0<n<32).
  #define rotate_left(n,x)  (((x) << (n)) | ((x) >> (32-(n))))

# Mischt zwei Hashcodes.
# Der eine wird um 5 Bit rotiert, dann der andere draufgeXORt.
  #define misch(x1,x2) (rotate_left(5,x1) ^ (x2))

# UP: Berechnet den EQ-Hashcode eines Objekts.
# hashcode1(obj)
# Er ist nur bis zur nächsten GC gültig.
# Aus (eq X Y) folgt (= (hashcode1 X) (hashcode1 Y)).
# > obj: ein Objekt
# < ergebnis: Hashcode, eine 32-Bit-Zahl
  local uint32 hashcode1 (object obj);
  #if 0
  local uint32 hashcode1(obj)
    var reg1 object obj;
    { return (uint32)obj; } # Adresse (Bits 23..0) und Typinfo
  #else
  #define hashcode1(obj)  ((uint32)(obj))
  #endif

# UP: Berechnet den EQL-Hashcode eines Objekts.
# hashcode2(obj)
# Er ist nur bis zur nächsten GC gültig.
# Aus (eql X Y) folgt (= (hashcode2 X) (hashcode2 Y)).
# > obj: ein Objekt
# < ergebnis: Hashcode, eine 32-Bit-Zahl
  local uint32 hashcode2 (object obj);
# Hilfsfunktionen bei bekanntem Typ:
  # Fixnum: Fixnum-Wert
  local uint32 hashcode_fixnum (object obj);
  #if 0
  local uint32 hashcode_fixnum(obj)
    var reg1 object obj;
    { return hashcode1(obj); }
  #else
  #define hashcode_fixnum(obj)  hashcode1(obj)
  #endif
  # Bignum: Länge*2 + (MSD*2^16 + LSD)
  local uint32 hashcode_bignum (object obj);
  local uint32 hashcode_bignum(obj)
    var reg1 object obj;
    { var reg2 uintL len = (uintL)TheBignum(obj)->length; # Anzahl Words
      return
        #if (intDsize==32)
          misch(TheBignum(obj)->data[0], # MSD
                TheBignum(obj)->data[len-1]) # und LSD
        #else # (intDsize<32)
          highlow32(TheBignum(obj)->data[0], # MSD
                    TheBignum(obj)->data[len-1]) # und LSD
        #endif
        + 2*len; # und Länge*2
    }
  # Short-Float: Interne Repräsentation
  local uint32 hashcode_sfloat (object obj);
  #if 0
  local uint32 hashcode_sfloat(obj)
    var reg1 object obj;
    { return hashcode1(obj); }
  #else
  #define hashcode_sfloat(obj)  hashcode1(obj)
  #endif
  # Single-Float: 32 Bit
  local uint32 hashcode_ffloat (object obj);
  local uint32 hashcode_ffloat(obj)
    var reg1 object obj;
    { return ffloat_value(obj); }
  # Double-Float: führende 32 Bit
  local uint32 hashcode_dfloat (object obj);
  local uint32 hashcode_dfloat(obj)
    var reg1 object obj;
    { return TheDfloat(obj)->float_value.semhi; }
  # Long-Float: Mischung aus Exponent, Länge, erste 32 Bit
  extern uint32 hashcode_lfloat (object obj); # siehe LFLOAT.D
# allgemein:
  local uint32 hashcode2(obj)
    var reg1 object obj;
    { if (!numberp(obj)) # eine Zahl?
        # nein -> EQ-Hashcode nehmen (bei Characters ist ja EQL == EQ) :
        { return hashcode1(obj); }
        # ja -> nach Typcode unterscheiden:
        { switch (typecode(obj) & ~(bit(number_bit_t)|bit(sign_bit_t)))
            { case fixnum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Fixnum
                return hashcode_fixnum(obj);
              case bignum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Bignum
                return hashcode_bignum(obj);
              case sfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Short-Float
                return hashcode_sfloat(obj);
              case ffloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Single-Float
                return hashcode_ffloat(obj);
              case dfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Double-Float
                return hashcode_dfloat(obj);
              case lfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Long-Float
                return hashcode_lfloat(obj);
              case ratio_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Ratio
                # beide Komponenten hashen, mischen
                { var reg2 uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
                  var reg3 uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
                  return misch(code1,code2);
                }
              case complex_type & ~(bit(number_bit_t)|bit(sign_bit_t)): # Complex
                # beide Komponenten hashen, mischen
                { var reg2 uint32 code1 = hashcode2(TheComplex(obj)->c_real);
                  var reg3 uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
                  return misch(code1,code2);
                }
              default: NOTREACHED
        }   }
    }

# UP: Berechnet den EQUAL-Hashcode eines Objekts.
# hashcode3(obj)
# Er ist nur bis zur nächsten GC oder der nächsten Modifizierung des Objekts
# gültig.
# Aus (equal X Y) folgt (= (hashcode3 X) (hashcode3 Y)).
# > obj: ein Objekt
# < ergebnis: Hashcode, eine 32-Bit-Zahl
  local uint32 hashcode3 (object obj);
# Hilfsfunktionen bei bekanntem Typ:
  # String -> Länge, erste max. 31 Zeichen, letztes Zeichen verwerten
  local uint32 hashcode_string (object obj);
  local uint32 hashcode_string(obj)
    var reg5 object obj;
    { var uintL len;
      var reg1 uintB* ptr = unpack_string(obj,&len); # ab ptr kommen len Zeichen
      var reg2 uint32 bish_code = 0x33DAE11FUL + len; # Länge verwerten
      if (len > 0)
        { bish_code ^= (uint32)(ptr[len-1]); # letztes Zeichen dazu
         {var reg3 uintC count = (len <= 31 ? len : 31); # min(len,31)
          dotimespC(count,count,
            { var reg4 uint32 next_code = (uint32)(*ptr++); # nächstes Zeichen
              bish_code = misch(bish_code,next_code); # dazunehmen
            });
        }}
      return bish_code;
    }
  # Bit-Vektor -> Länge, erste 16 Bits, letzte 16 Bits verwerten
  local uint32 hashcode_bvector (object obj);
  local uint32 hashcode_bvector(obj)
    var reg8 object obj;
    { var reg6 uintL len = vector_length(obj); # Länge
      var uintL index = 0;
      var reg7 object sbv = array_displace_check(obj,len,&index);
      # sbv der Datenvektor, index ist der Index in den Datenvektor.
      if (!simple_bit_vector_p(sbv))
        # Bei Byte-Vektoren schauen wir in deren Bitvektor hinein.
        { len = len << (TheArray(sbv)->flags /* & arrayflags_atype_mask */ );
          sbv = TheArray(sbv)->data;
        }
      #if BIG_ENDIAN_P && (Varobject_alignment%2 == 0)
        # Bei Big-Endian-Maschinen kann man gleich mit 16 Bit auf einmal arbeiten
        # (sofern Varobject_alignment durch 2 Byte teilbar ist):
        #define bitpack  16
        #define uint_bitpack  uint16
        #define get32bits_at  highlow32_at
      #else
        # Sonst kann man nur 8 Bit auf einmal nehmen:
        #define bitpack  8
        #define uint_bitpack  uint8
        #define get32bits_at(p) \
          (((((((uint32)((p)[0])<<8)|(uint32)((p)[1]))<<8)|(uint32)((p)[2]))<<8)|(uint32)((p)[3]))
      #endif
     {var reg1 uint_bitpack* ptr = # Pointer aufs erste benutzte Word
               (uint_bitpack*)(&TheSbvector(sbv)->data[0]) + floor(index,bitpack);
      var reg5 uintL offset = index%bitpack; # Offset innerhalb des Word
      if (len <= 32)
        # Länge <= 32 -> alle Bits nehmen:
        if (len == 0)
          { return 0x8FA1D564UL; }
          else
          # 0<len<=32
          { var reg4 uintL need = offset+len; # Brauche erstmal need Bits
            # need < 48
            var reg2 uint32 akku12 = 0; # 48-Bit-Akku, Teil 1 und 2
            var reg3 uint32 akku3 = 0; # 48-Bit-Akku, Teil 3
            #if (bitpack==16)
            if (need > 0)
              { akku12 = highlow32_0(*ptr++); # erste 16 Bits
                if (need > 16)
                  { akku12 |= (uint32)(*ptr++); # nächste 16 Bits
                    if (need > 32)
                      { akku3 = (uint32)(*ptr++); # letzte 16 Bits
              }   }   }
            #endif
            #if (bitpack==8)
            if (need > 0)
              { akku12 = (uint32)(*ptr++)<<24; # erste 8 Bits
                if (need > 8)
                  { akku12 |= (uint32)(*ptr++)<<16; # nächste 8 Bits
                    if (need > 16)
                      { akku12 |= (uint32)(*ptr++)<<8; # nächste 8 Bits
                        if (need > 24)
                          { akku12 |= (uint32)(*ptr++); # nächste 8 Bits
                            if (need > 32)
                              { akku3 = (uint32)(*ptr++)<<8; # nächste 8 Bits
                                if (need > 40)
                                  { akku3 |= (uint32)(*ptr++); # letzte 8 Bits
              }   }   }   }   }   }
            #endif
            # need Bits in akku12,akku3 um offset Bits nach links schieben:
            akku12 = (akku12 << offset) | (uint32)high16(akku3 << offset);
            # 32 Bits in akku12 fertig.
            # irrelevante Bits ausmaskieren:
            akku12 = akku12 & ~(bit(32-len)-1);
            # Länge verwerten:
            return akku12+len;
          }
        else
        # Länge > 32 -> erste und letzte 16 Bits nehmen:
        { var reg2 uint32 akku12 = # 32-Bit-Akku
            get32bits_at(ptr) << offset; # enthält mind. die ersten 16 Bits
          offset += len; # End-Offset des Bitvektor
          ptr += floor(offset,bitpack); # zeigt aufs letzte benutzte Word
          offset = offset%bitpack; # End-Offset innerhalb des Word
         {var reg3 uint32 akku34 = # 32-Bit-Akku
            get32bits_at(ptr-(16/bitpack)) << offset; # enthält mind. die letzten 16 Bits
          # erste 16, letzte 16 Bits herausgreifen und Länge verwerten:
          return highlow32(high16(akku12),high16(akku34)) + len;
        }}
      #undef get32bits_at
      #undef uint_bitpack
      #undef bitpack
    }}
  # Atom -> Fallunterscheidung nach Typ
  local uint32 hashcode_atom (object obj);
  local uint32 hashcode_atom(obj)
    var reg1 object obj;
    { if (symbolp(obj)) # ein Symbol?
        { return hashcode1(obj); } # ja -> EQ-Hashcode nehmen
      elif (numberp(obj)) # eine Zahl?
        { return hashcode2(obj); } # ja -> EQL-Hashcode nehmen
      else
        { var reg2 tint type = typecode(obj) # Typinfo
                               & ~bit(notsimple_bit_t); # ob simple oder nicht, ist irrelevant
          if (type == (sbvector_type & ~bit(notsimple_bit_t))) # Bit-Vektor ?
            { return hashcode_bvector(obj); } # komponentenweise ansehen
          if (type == (sstring_type & ~bit(notsimple_bit_t))) # String ?
            { return hashcode_string(obj); } # komponentenweise ansehen
          if (pathnamep(obj))
            # Pathname -> komponentenweise ansehen:
            { check_SP();
             {var reg4 uint32 bish_code = 0xB0DD939EUL;
              var reg3 object* ptr = &((Record)ThePathname(obj))->recdata[0];
              var reg6 uintC count;
              dotimesC(count,pathname_length,
                { var reg5 uint32 next_code = hashcode3(*ptr++); # Hashcode der nächsten Komponente
                  bish_code = misch(bish_code,next_code); # dazunehmen
                });
              return bish_code;
            }}
          # sonst: EQ-Hashcode nehmen (bei Characters ist ja EQL == EQ)
          return hashcode1(obj);
    }   }
# Cons -> Inhalt bis zur Tiefe 4 ansehen:
# Jeweils Hashcode des CAR und Hashcode des CDR bestimmen
# und geshiftet kombinieren. Als Shifts passen z.B. 16,7,5,3,
# da {0,16} + {0,7} + {0,5} + {0,3} = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
# aus 16 verschiedenen Elementen von {0,...,31} besteht.
  # Objekt, bei Cons nur bis Tiefe 0
  local uint32 hashcode_cons0 (object obj);
  local uint32 hashcode_cons0(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return hashcode_atom(obj); }
        else
        # Cons -> Hashcode := 1
        { return 1; }
    }
  # Objekt, bei Cons nur bis Tiefe 1
  local uint32 hashcode_cons1 (object obj);
  local uint32 hashcode_cons1(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return hashcode_atom(obj); }
        else
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        { var reg2 uint32 code1 = hashcode_cons0(Car(obj));
          var reg3 uint32 code2 = hashcode_cons0(Cdr(obj));
          return rotate_left(3,code1) ^ code2;
    }   }
  # Objekt, bei Cons nur bis Tiefe 2
  local uint32 hashcode_cons2 (object obj);
  local uint32 hashcode_cons2(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return hashcode_atom(obj); }
        else
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        { var reg2 uint32 code1 = hashcode_cons1(Car(obj));
          var reg3 uint32 code2 = hashcode_cons1(Cdr(obj));
          return rotate_left(5,code1) ^ code2;
    }   }
  # Objekt, bei Cons nur bis Tiefe 3
  local uint32 hashcode_cons3 (object obj);
  local uint32 hashcode_cons3(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return hashcode_atom(obj); }
        else
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        { var reg2 uint32 code1 = hashcode_cons2(Car(obj));
          var reg3 uint32 code2 = hashcode_cons2(Cdr(obj));
          return rotate_left(7,code1) ^ code2;
    }   }
  # Objekt, bei Cons nur bis Tiefe 4
  local uint32 hashcode3(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return hashcode_atom(obj); }
        else
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        { var reg2 uint32 code1 = hashcode_cons3(Car(obj));
          var reg3 uint32 code2 = hashcode_cons3(Cdr(obj));
          return rotate_left(16,code1) ^ code2;
    }   }

# UP: Berechnet den Hashcode eines Objekts bezüglich einer Hashtabelle.
# hashcode(ht,obj)
# > ht: Hash-Table
# > obj: Objekt
# < ergebnis: Index in den Index-Vektor
  local uintL hashcode (object ht, object obj);
  local uintL hashcode(ht,obj)
    var reg1 object ht;
    var reg5 object obj;
    { # Hashcode je nach Hashtabellen-Typ:
      var reg2 uintB flags = TheHashtable(ht)->recflags;
      var reg3 uint32 code =
        (flags & bit(0) ? hashcode1(obj) : # EQ-Hashcode
         flags & bit(1) ? hashcode2(obj) : # EQL-Hashcode
         flags & bit(2) ? hashcode3(obj) : # EQUAL-Hashcode
         0 /*NOTREACHED*/
        );
      # dann durch SIZE dividieren:
      var reg4 uint32 rest;
      divu_3232_3232(code,posfixnum_to_L(TheHashtable(ht)->ht_size),,rest = );
      return rest;
    }

# UP: Reorganisiert eine Hash-Tabelle, nachdem durch eine GC die Hashcodes
# der Keys verändert wurden.
# rehash(ht);
# > ht: Hash-Table
  local void rehash (object ht);
  local void rehash(ht)
    var reg9 object ht;
    { # Index-Vektor mit "nix" füllen:
      var reg2 object Ivektor = TheHashtable(ht)->ht_itable; # Index-Vektor
      { var reg1 object* ptr = &TheSvector(Ivektor)->data[0];
        var reg3 uintL count = posfixnum_to_L(TheHashtable(ht)->ht_size); # SIZE, >0
        dotimespL(count,count, { *ptr++ = nix; } );
      }
      # "Listen"struktur elementweise aufbauen:
     {var reg9 object Nvektor = TheHashtable(ht)->ht_ntable; # Next-Vektor
      var reg9 object KVvektor = TheHashtable(ht)->ht_kvtable; # Key-Value-Vektor
      var reg5 object index = TheHashtable(ht)->ht_maxcount; # MAXCOUNT
      var reg9 uintL maxcount = posfixnum_to_L(index);
      var reg3 object* Nptr = &TheSvector(Nvektor)->data[maxcount];
      var reg4 object* KVptr = &TheSvector(KVvektor)->data[2*maxcount];
      var reg8 object freelist = nix;
      var reg7 object count = Fixnum_0;
      loop
        # Schleife, läuft durch den Key-Value-Vektor und den Next-Vektor.
        # index = MAXCOUNT,...,0 (Fixnum),
        # Nptr = &TheSvector(Nptr)->data[index],
        # KVptr = &TheSvector(KVptr)->data[index],
        # freelist = bisherige Freiliste,
        # count = Paare-Zähler als Fixnum.
        { if (eq(index,Fixnum_0)) break; # index=0 -> Schleife fertig
          index = fixnum_inc(index,-1); # index decrementieren
          KVptr -= 2;
         {var reg6 object key = KVptr[0]; # nächster Key
          if (!eq(key,leer)) # /= "leer" ?
            { var reg3 uintL hashindex = hashcode(ht,key); # Hashcode dazu
              # "Liste", die bei Eintrag hashindex anfängt, um index erweitern:
              # Eintrag im Index-Vektor in den Next-Vektor kopieren
              # und durch index (ein Pointer auf diese Stelle) ersetzen:
              var reg1 object* Iptr = &TheSvector(Ivektor)->data[hashindex];
              *--Nptr = *Iptr; # Eintrag in den Next-Vektor kopieren
              *Iptr = index; # und durch Zeiger darauf ersetzen
              count = fixnum_inc(count,1); # mitzählen
            }
            else
            # Freiliste im Next-Vektor verlängern:
            { *--Nptr = freelist; freelist = index; }
        }}
      TheHashtable(ht)->ht_freelist = freelist; # Freiliste abspeichern
      TheHashtable(ht)->ht_count = count; # Paare-Zahl abspeichern (konsistenzhalber)
      TheHashtable(ht)->recflags &= ~bit(7); # Hashtabelle ist nun fertig organisiert
    }}

# UP: Sucht ein Key in einer Hash-Tabelle.
# hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr)
# > ht: Hash-Tabelle
# > obj: Objekt
# < falls gefunden: ergebnis=TRUE,
#     KVptr[0], KVptr[1] : Key, Value im Key-Value-Vektor,
#     *Nptr : zugehöriger Eintrag im Next-Vektor,
#     *Iptr : auf *Nptr zeigender vorheriger Index
# < falls nicht gefunden: ergebnis=FALSE,
#     *Iptr : zum Key gehöriger Eintrag im Index-Vektor
#             oder ein beliebiges Element der dort beginnenden "Liste"
  local boolean hash_lookup (object ht, object obj, object** KVptr_, object** Nptr_, object** Iptr_);
  local boolean hash_lookup(ht,obj,KVptr_,Nptr_,Iptr_)
    var reg1 object ht;
    var reg7 object obj;
    var reg10 object** KVptr_;
    var reg10 object** Nptr_;
    var reg10 object** Iptr_;
    { var reg4 uintB flags = TheHashtable(ht)->recflags;
      if (flags & bit(7))
        # Hash-Tabelle muß erst noch reorganisiert werden
        { rehash(ht); }
     {var reg9 uintL hashindex = hashcode(ht,obj); # Hashcode berechnen
      var reg2 object* Nptr = # Pointer auf den aktuellen Eintrag
        &TheSvector(TheHashtable(ht)->ht_itable)->data[hashindex];
      loop
        { # "Liste" weiterverfolgen:
          if (eq(*Nptr,nix)) break; # "Liste" zu Ende -> nicht gefunden
          { var reg3 uintL index = posfixnum_to_L(*Nptr); # nächster Index
            var reg8 object* Iptr = Nptr;
            Nptr = # Pointer auf Eintrag im Next-Vektor
              &TheSvector(TheHashtable(ht)->ht_ntable)->data[index];
           {var reg5 object* KVptr = # Pointer auf Einträge im Key-Value-Vektor
              &TheSvector(TheHashtable(ht)->ht_kvtable)->data[2*index];
            var reg6 object key = KVptr[0];
            # key mit obj vergleichen:
            if (flags & bit(0) ? eq(key,obj) : # mit EQ vergleichen
                flags & bit(1) ? eql(key,obj) : # mit EQL vergleichen
                flags & bit(2) ? equal(key,obj) : # mit EQUAL vergleichen
                FALSE
               )
              # Objekt obj gefunden
              { *KVptr_ = KVptr; *Nptr_ = Nptr; *Iptr_ = Iptr; return TRUE; }
        } }}
      # nicht gefunden
      *Iptr_ = Nptr; return FALSE;
    }}

# Macro: Trägt ein Key-Value-Paar in einer Hash-Tabelle ein.
# hash_store(key,value);
# > object ht: Hash-Tabelle
# > object freelist: Anfang der Freiliste im Next-Vektor, /= nix
# > key: Key
# > value: Value
# > object* Iptr: beliebiges Element der "Liste", die zu Key gehört
  #define hash_store(key,value)  \
    { var reg6 uintL index = posfixnum_to_L(freelist); # freier Index          \
      var reg5 object* Nptr = # Adresse des freien Eintrags im Next-Vektor       \
        &TheSvector(TheHashtable(ht)->ht_ntable)->data[index];                 \
      var reg4 object* KVptr = # Adresse der freien Einträge im Key-Value-Vektor \
        &TheSvector(TheHashtable(ht)->ht_kvtable)->data[2*index];              \
      set_break_sem_2(); # Vor Unterbrechungen schützen                        \
      # COUNT incrementieren:                                                  \
      TheHashtable(ht)->ht_count = fixnum_inc(TheHashtable(ht)->ht_count,1);   \
      # Freiliste verkürzen:                                                   \
      TheHashtable(ht)->ht_freelist = *Nptr;                                   \
      # Key und Value abspeichern:                                             \
      *KVptr++ = key; *KVptr++ = value;                                        \
      # freies Listenelement index in die "Liste" einfügen                     \
      # (nach resize an den Listenanfang, da Iptr in den Index-Vektor zeigt,   \
      # sonst ans Listenende, da hash_lookup mit *Iptr=nix beendet wurde):     \
      *Nptr = *Iptr; *Iptr = freelist;                                         \
      clr_break_sem_2(); # Unterbrechungen wieder zulassen                     \
    }

# UP: Stellt die Zahlen und Vektoren für eine neue Hash-Tabelle bereit.
# prepare_resize(maxcount,mincount_threshold)
# > maxcount: gewünschte neue Größe MAXCOUNT
# > mincount_threshold: Short-Float MINCOUNT-THRESHOLD
# < ergebnis: maxcount
# < Stackaufbau: MAXCOUNT, SIZE, MINCOUNT,
#                Index-Vektor, Next-Vektor, Key-Value-Vektor.
# Erniedrigt STACK um 6
# kann GC auslösen
  local uintL prepare_resize (object maxcount, object mincount_threshold);
  local uintL prepare_resize(maxcount,mincount_threshold)
    var reg3 object maxcount;
    var reg4 object mincount_threshold;
    { # Überprüfe, ob maxcount ein nicht zu großes Fixnum >0 ist:
      if (!posfixnump(maxcount)) goto fehler_maxcount;
     {var reg1 uintL maxcountL = posfixnum_to_L(maxcount);
      var reg2 uintL sizeL = 2*maxcountL+1;
      # SIZE ungerade, damit die Hashfunktion besser wird!
      if (!(sizeL <= (uintL)(bitm(oint_addr_len)-1))) # sizeL sollte in ein Fixnum passen
        goto fehler_maxcount;
      # Zahlen auf den Stack:
      pushSTACK(maxcount); # MAXCOUNT
      pushSTACK(fixnum(sizeL)); # SIZE
      { # MINCOUNT := (floor (* maxcount mincount-threshold))
        pushSTACK(maxcount); pushSTACK(mincount_threshold); funcall(L(mal),2);
        pushSTACK(value1); funcall(L(floor),1);
        pushSTACK(value1);
      }
      # Stackaufbau: MAXCOUNT, SIZE, MINCOUNT.
      # neue Vektoren allozieren:
      pushSTACK(allocate_vector(sizeL)); # Index-Vektor beschaffen
      pushSTACK(allocate_vector(maxcountL)); # Next-Vektor beschaffen
      pushSTACK(allocate_vector(2*maxcountL)); # Key-Value-Vektor beschaffen
      # fertig.
      return maxcountL;
     }
      fehler_maxcount: # maxcount kein Fixnum oder zu groß
        pushSTACK(maxcount);
        fehler(
               DEUTSCH ? "Zu große Hashtabellengröße ~" :
               ENGLISH ? "Hash table size ~ too large" :
               FRANCAIS ? "La taille ~ est trop grande pour une table de hachage." :
               ""
              );
    }

# UP: Vergrößert oder verkleinert eine Hash-Tabelle
# resize(ht,maxcount)
# > ht: Hash-Table
# > maxcount: gewünschte neue Größe MAXCOUNT
# < ergebnis: Hash-Table, EQ zur alten
# kann GC auslösen
  local object resize (object ht, object maxcount);
  local object resize(ht,maxcount)
    var reg8 object ht;
    var reg9 object maxcount;
    { pushSTACK(ht);
     {var reg9 uintL maxcountL = 
        prepare_resize(maxcount,TheHashtable(ht)->ht_mincount_threshold);
      # Ab jetzt keine GC mehr!
      var reg9 object KVvektor = popSTACK(); # neuer Key-Value-Vektor
      var reg10 object Nvektor = popSTACK(); # Next-Vektor
      var reg10 object Ivektor = popSTACK(); # Index-Vektor
      var reg10 object mincount = popSTACK(); # MINCOUNT
      var reg10 object size = popSTACK(); # SIZE
      maxcount = popSTACK();
      ht = popSTACK();
      # Neuen Key-Value-Vektor füllen:
      # Durch den alten Key-Value-Vektor durchlaufen und
      # alle Key-Value-Paare mit Key /= "leer" kopieren:
      { # Zum Durchlaufen des alten Key-Value-Vektors:
        var reg3 uintL oldcount = posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
        var reg1 object* oldKVptr = &TheSvector(TheHashtable(ht)->ht_kvtable)->data[0];
        # Zum Durchlaufen des neuen Key-Value-Vektors:
        var reg4 uintL count = maxcountL;
        var reg2 object* KVptr = &TheSvector(KVvektor)->data[0];
        # Zum Mitzählen:
        var reg7 object counter = Fixnum_0;
        dotimesL(oldcount,oldcount,
          { var reg5 object nextkey = *oldKVptr++; # nächster Key
            var reg6 object nextvalue = *oldKVptr++; # und Value
            if (!eq(nextkey,leer))
              # Eintrag in den neuen Key-Value-Vektor übernehmen:
              { if (count==0) # Ist der neue Vektor schon voll?
                  # Der Platz reicht nicht!!
                  { pushSTACK(ht); # Hash-Table
                    fehler(
                           DEUTSCH ? "Interner Fehler beim Reorganisieren von ~." :
                           ENGLISH ? "internal error occured while resizing ~" :
                           FRANCAIS ? "Une erreur interne s'est produite au moment de la réorganisation de ~." :
                           ""
                          );
                  }
                count--;
                *KVptr++ = nextkey; *KVptr++ = nextvalue; # im neuen Vektor ablegen
                counter = fixnum_inc(counter,1); # und mitzählen
              }
          });
        # Noch count Paare des neuen Key-Value-Vektors als "leer" markieren:
        dotimesL(count,count, { *KVptr++ = leer; *KVptr++ = leer; } );
        # Hash-Tabelle modifizieren:
        set_break_sem_2(); # Vor Unterbrechungen schützen
        TheHashtable(ht)->recflags |= bit(7); # Tabelle muß erst noch reorganisiert werden
        TheHashtable(ht)->ht_size = size; # neues SIZE eintragen
        TheHashtable(ht)->ht_itable = Ivektor; # neuen Index-Vektor eintragen
        TheHashtable(ht)->ht_maxcount = maxcount; # neues MAXCOUNT eintragen
        TheHashtable(ht)->ht_freelist = nix; # Dummy als Freiliste
        TheHashtable(ht)->ht_ntable = Nvektor; # neuen Next-Vektor eintragen
        TheHashtable(ht)->ht_kvtable = KVvektor; # neuen Key-Value-Vektor eintragen
        TheHashtable(ht)->ht_count = counter; # COUNT eintragen (konsistenzhalber)
        TheHashtable(ht)->ht_mincount = mincount; # neues MINCOUNT eintragen
        clr_break_sem_2(); # Unterbrechungen wieder zulassen
        return ht;
    }}}

# Macro: Vergrößert eine Hash-Tabelle so lange, bis freelist /= nix
# hash_prepare_store();
# > object key: Key (im Stack)
# > object ht: Hash-Tabelle
# < object ht: Hash-Tabelle
# < object freelist: Anfang der Freiliste im Next-Vektor, /= nix
# < object* Iptr: beliebiges Element der "Liste", die zu Key gehört
# kann GC auslösen
  #define hash_prepare_store(key)  \
    { retry:                                                                    \
      freelist = TheHashtable(ht)->ht_freelist;                                 \
      if (eq(freelist,nix)) # Freiliste = leere "Liste" ?                       \
        # ja -> muß die Hash-Tabelle vergrößern:                                \
        { pushSTACK(ht); # Hashtable retten                                     \
          # neues maxcount ausrechnen:                                          \
          pushSTACK(TheHashtable(ht)->ht_maxcount);                             \
          pushSTACK(TheHashtable(ht)->ht_rehash_size); # REHASH-SIZE (>1)       \
          funcall(L(mal),2); # (* maxcount rehash-size), ist > maxcount         \
          pushSTACK(value1);                                                    \
          funcall(L(ceiling),1); # (ceiling ...), Integer > maxcount            \
          ht = resize(popSTACK(),value1); # Tabelle vergrößern                  \
          rehash(ht); # und reorganisieren                                      \
          # Adresse des Eintrags im Index-Vektor neu ausrechnen:                \
         {var reg3 uintL hashindex = hashcode(ht,key); # Hashcode berechnen     \
          Iptr = &TheSvector(TheHashtable(ht)->ht_itable)->data[hashindex];     \
          goto retry;                                                           \
        }}                                                                      \
    }

# UP: Löscht den Inhalt einer Hash-Tabelle.
# clrhash(ht);
# > ht: Hash-Tabelle
  local void clrhash (object ht);
  local void clrhash(ht)
    var reg3 object ht;
    { set_break_sem_2(); # Vor Unterbrechungen schützen
      {var reg1 object* KVptr = &TheSvector(TheHashtable(ht)->ht_kvtable)->data[0];
       var reg2 uintL count = posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
       dotimesL(count,count, # in jedem Eintrag
         { *KVptr++ = leer; *KVptr++ = leer; # Key und Value leeren
         });
      }
      TheHashtable(ht)->ht_count = Fixnum_0; # COUNT := 0
      TheHashtable(ht)->recflags |= bit(7); # Hashtabelle später noch reorganisieren
      clr_break_sem_2(); # Unterbrechungen wieder zulassen
    }

# (MAKE-HASH-TABLE [:test] [:size] [:rehash-size] [:rehash-threshold]
#                  [:initial-contents]), CLTL S. 283
LISPFUN(make_hash_table,0,0,norest,key,5,\
        (kw(initial_contents),\
         kw(test),kw(size),kw(rehash_size),kw(rehash_threshold)) )
  { # Dem Rehash-Threshold entspricht in unserer Implementation das
    # Verhältnis MAXCOUNT : SIZE = ca. 1 : 2.
    # Wir ignorieren das rehash-threshold-Argument, da sowohl zu große als
    # auch zu kleine Werte davon schädlich wären: 0.99 bewirkt im Durchschnitt
    # zu lange Zugriffszeiten; 0.00001 bewirkt, daß SIZE = MAXCOUNT/threshold
    # zu schnell ein Bignum werden könnte.
    # Das zusätzliche initial-contents-Argument ist eine Aliste = Liste von
    # (Key . Value) - Paaren, mit denen die Tabelle initialisiert wird.
    # Stackaufbau: initial-contents, test, size, rehash-size, rehash-threshold.
    var reg3 uintB flags;
    # test-Argument überprüfen:
    { var reg1 object test = STACK_3;
      if (eq(test,unbound))
        { flags = bit(1); } # EQL als Default
      elif (eq(test,S(eq)) || eq(test,L(eq)))
        { flags = bit(0); } # EQ
      elif (eq(test,S(eql)) || eq(test,L(eql)))
        { flags = bit(1); } # EQL
      elif (eq(test,S(equal)) || eq(test,L(equal)))
        { flags = bit(2); } # EQUAL
      else
        { pushSTACK(test);
          pushSTACK(S(make_hash_table));
          fehler(
                 DEUTSCH ? "~: Unzulässiges :TEST-Argument ~" :
                 ENGLISH ? "~: illegal :TEST argument ~" :
                 FRANCAIS ? "~: Argument pour :TEST illicite : ~" :
                 ""
                );
    }   }
    # flags enthält die Flags zum Test.
    # size-Argument überprüfen:
    { var reg1 object size = STACK_2;
      if (eq(size,unbound))
        { STACK_2 = Fixnum_1; } # 1 als Default
        else
        { if (!posfixnump(size))
            { pushSTACK(size);
              pushSTACK(S(make_hash_table));
              fehler(
                     DEUTSCH ? "~: :SIZE-Argument sollte ein Fixnum >=0 sein, nicht ~" :
                     ENGLISH ? "~: :SIZE argument should be a fixnum >=0, not ~" :
                     FRANCAIS ? "~: L'argument :SIZE doit être de type FIXNUM positif ou zéro et non ~." :
                     ""
                    );
            }
          # size ist ein Fixnum >=0
          if (eq(size,Fixnum_0)) { STACK_2 = Fixnum_1; } # aus 0 mache 1
    }   }
    # size ist jetzt ein Fixnum >0.
    # rehash-size überprüfen:
    { if (eq(STACK_1,unbound))
        # Default-Rehash-Size = 1.5s0
        { STACK_1 = make_SF(0,SF_exp_mid+1,(bit(SF_mant_len)*3)/2); }
        else
        { if (!mfloatp(STACK_1)) # Float ist OK
            { if (!mposfixnump(STACK_1)) # sonst sollte es ein Fixnum >=0 sein
                { fehler_rehash_size:
                  pushSTACK(STACK_1);
                  pushSTACK(S(make_hash_table));
                  fehler(
                         DEUTSCH ? "~: :REHASH-SIZE-Argument sollte ein Float > 1 sein, nicht ~" :
                         ENGLISH ? "~: :REHASH-SIZE argument should be a float > 1, not ~" :
                         FRANCAIS ? "~: L'argument :REHASH-SIZE devrait être un nombre à virgule flottante supérieur à 1 et non ~." :
                         ""
                        );
                }
              # Da es sinnlos ist, eine Tabelle immer nur um eine feste
              # Anzahl von Elementen größer zu machen (führt zu katastrophaler
              # Effizienz), wird rehash-size := min(1 + rehash-size/size , 2.0)
              # gesetzt.
              pushSTACK(STACK_1); # rehash-size
              pushSTACK(STACK_(2+1)); # size
              funcall(L(durch),2); # (/ rehash-size size)
              pushSTACK(value1);
              funcall(L(einsplus),1); # (1+ ...)
              pushSTACK(value1);
              pushSTACK(make_SF(0,SF_exp_mid+2,bit(SF_mant_len))); # 2.0s0
              funcall(L(min),2); # (MIN ... 2.0s0)
              STACK_1 = value1; # =: rehash-size
            }
          # (> rehash-size 1) überprüfen:
          pushSTACK(STACK_1); # rehash-size
          pushSTACK(Fixnum_1); # 1
          funcall(L(groesser),2); # (> rehash-size 1)
          if (nullp(value1)) goto fehler_rehash_size;
          # rehash-size in ein Short-Float umwandeln:
          pushSTACK(STACK_1); # rehash-size
          pushSTACK(SF_0); # 0.0s0
          funcall(L(float),2); # (FLOAT rehash-size 0.0s0) = (COERCE rehash-size 'SHORT-FLOAT)
          # (>= rehash-size 1.125s0) erzwingen:
          pushSTACK(value1);
          pushSTACK(make_SF(0,SF_exp_mid+1,(bit(SF_mant_len)/8)*9)); # 1.125s0
          funcall(L(max),2); # (max rehash-size 1.125s0)
          STACK_1 = value1; # =: rehash-size
    }   }
    # rehash-size ist ein Short-Float >= 1.125 .
    # rehash-threshold überprüfen: sollte ein Float >=0, <=1 sein
    { var reg1 object rehash_threshold = STACK_0;
      if (!eq(rehash_threshold,unbound)) # nicht angegeben -> OK
        { if (!floatp(rehash_threshold))
            { fehler_rehash_threshold:
              # Argument bereits in STACK_0
              pushSTACK(S(make_hash_table));
              fehler(
                     DEUTSCH ? "~: :REHASH-THRESHOLD-Argument sollte ein Float zwischen 0 und 1 sein, nicht ~" :
                     ENGLISH ? "~: :REHASH-THRESHOLD argument should be a float between 0 and 1, not ~" :
                     FRANCAIS ? "~: L'argument :REHASH-THRESHOLD devrait être un nombre à virgule flottante compris entre 0 et 1 et non ~." :
                     ""
                    );
            }
          pushSTACK(Fixnum_1);
          pushSTACK(rehash_threshold);
          pushSTACK(Fixnum_0);
          funcall(L(grgleich),3); # (>= 1 rehash-threshold 0)
          if (nullp(value1)) goto fehler_rehash_threshold;
    }   }
    # Nun sind alle Argumente überprüft.
    # Ist das initial-contents-Argument angegeben, so wird
    # size := (max size (length initial-contents)) gesetzt, damit nachher beim
    # Eintragen des initial-contents die Tabelle nicht vergrößert werden muß:
    { var reg1 object initial_contents = STACK_4;
      if (!eq(initial_contents,unbound)) # angegeben ?
        { var reg1 uintL initial_length = llength(initial_contents); # Länge der Aliste
          if (initial_length > posfixnum_to_L(STACK_2)) # > size ?
            { STACK_2 = fixnum(initial_length); } # ja -> size vergrößern
    }   }
    # size ist ein Fixnum >0, >= (length initial-contents) .
    # MINCOUNT-THRESHOLD = 1/rehash-size^2 errechnen:
    { var reg1 object rehash_size = STACK_1;
      pushSTACK(rehash_size);
      pushSTACK(rehash_size);
      funcall(L(mal),2); # (* rehash-size rehash-size)
      pushSTACK(value1);
      funcall(L(durch),1); # (/ ...)
      STACK_0 = value1;
    }
    # Stackaufbau: initial-contents, test, size, rehash-size, mincount-threshold.
    # Vektoren beschaffen usw., mit size als MAXCOUNT:
    prepare_resize(STACK_2,STACK_0);
    { var reg1 object ht = allocate_hash_table(); # neue Hash-Tabelle
      # füllen:
      TheHashtable(ht)->ht_kvtable = popSTACK(); # Key-Value-Vektor
      TheHashtable(ht)->ht_ntable = popSTACK(); # Next-Vektor
      TheHashtable(ht)->ht_itable = popSTACK(); # Index-Vektor
      TheHashtable(ht)->ht_mincount = popSTACK(); # MINCOUNT
      TheHashtable(ht)->ht_size = popSTACK(); # SIZE
      TheHashtable(ht)->ht_maxcount = popSTACK(); # MAXCOUNT
      # Stackaufbau: initial-contents, test, size, rehash-size, mincount-threshold.
      TheHashtable(ht)->ht_mincount_threshold = popSTACK(); # MINCOUNT-THRESHOLD
      TheHashtable(ht)->ht_rehash_size = popSTACK(); # REHASH-SIZE
      TheHashtable(ht)->ht_freelist = nix; # Dummy als Freiliste
      TheHashtable(ht)->recflags = flags;
      clrhash(ht); # Tabelle leeren, COUNT := 0
      skipSTACK(2);
      # Stackaufbau: initial-contents.
      { var reg2 object alist = popSTACK(); # initial-contents
        while (consp(alist)) # Wenn es angegeben war, solange es ein Cons ist:
          { var reg3 object next = Car(alist); # Alistenelement
            if (consp(next)) # ein Cons (Key . Value) ?
              # (SYSTEM::PUTHASH (car next) hashtable (cdr next)) ausführen,
              # wobei die Tabelle nicht wachsen kann:
              { var reg8 object key = Car(next);
                var object* KVptr;
                var object* Nptr;
                var object* Iptr;
                if (hash_lookup(ht,key,&KVptr,&Nptr,&Iptr)) # in der Hash-Tabelle suchen
                  # schon gefunden -> war in der Aliste weiter links schon
                  # enthalten, und in Alisten verdeckt die erste Assoziation
                  # (links) alle anderen Assoziationen zum selben Key.
                  {}
                  else
                  # nicht gefunden -> neuen Eintrag basteln:
                  { var reg7 object freelist = # Anfang der Freiliste im Next-Vektor
                      TheHashtable(ht)->ht_freelist;
                    if (eq(freelist,nix)) # leere "Liste" ?
                      { pushSTACK(ht); # Hash-Tabelle
                        pushSTACK(S(make_hash_table));
                        fehler(
                               DEUTSCH ? "~: Interner Fehler beim Aufbauen von ~" :
                               ENGLISH ? "~: internal error while building ~" :
                               FRANCAIS ? "~: Une erreur interne s'est produite lors de la construction de ~." :
                               ""
                              );
                      }
                    hash_store(key,Cdr(next)); # Eintrag basteln
              }   }
            alist = Cdr(alist);
          }
      }
      value1 = ht; mv_count=1; # Hash-Tabelle als Wert
  } }

# UP: Sucht ein Objekt in einer Hash-Tabelle.
# gethash(obj,ht)
# > obj: Objekt, als Key
# > ht: Hash-Tabelle
# < ergebnis: zugehöriger Value, falls gefunden, nullobj sonst
  global object gethash (object obj, object ht);
  global object gethash(obj,ht)
    var reg2 object obj;
    var reg1 object ht;
    { var object* KVptr;
      var object* Nptr;
      var object* Iptr;
      if (hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr))
        { return KVptr[1]; } # gefunden -> Value
        else
        { return nullobj; }
    }

# Fehler, wenn ein Argument keine Hash-Table ist
# fehler_hashtable(obj);
# > obj: Objekt
# > subr_self: Aufrufer (ein SUBR)
  local nonreturning void fehler_hashtable (object obj);
  local nonreturning void fehler_hashtable(obj)
    var reg1 object obj;
    { pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(
             DEUTSCH ? "~: Argument ~ ist keine Hash-Table." :
             ENGLISH ? "~: argument ~ is not a hash-table" :
             FRANCAIS ? "~: L'argument ~ n'est pas une table de hachage." :
             ""
            );
    }

# (GETHASH key hashtable [default]), CLTL S. 284
LISPFUN(gethash,2,1,norest,nokey,0,NIL)
  { var reg1 object ht = STACK_1; # hashtable-Argument
    if (!hash_table_p(ht)) { fehler_hashtable(ht); } # überprüfen
   {var object* KVptr;
    var object* Nptr;
    var object* Iptr;
    # Key STACK_2 in der Hash-Tabelle suchen:
    if (hash_lookup(ht,STACK_2,&KVptr,&Nptr,&Iptr))
      # gefunden -> Value als Wert:
      { value1 = KVptr[1]; value2 = T; mv_count=2; # und T als 2. Wert
        skipSTACK(3);
      }
      else
      # nicht gefunden -> default oder NIL als Wert
      { var reg2 object def = popSTACK(); # default
        value1 = (eq(def,unbound) ? NIL : def); value2 = NIL; mv_count=2; # NIL als 2. Wert
        skipSTACK(2);
      }
  }}

# (SYSTEM::PUTHASH key hashtable value) =
# (SETF (GETHASH key hashtable) value), CLTL S. 284
LISPFUNN(puthash,3)
  { var reg1 object ht = STACK_1; # hashtable-Argument
    if (!hash_table_p(ht)) { fehler_hashtable(ht); } # überprüfen
   {var object* KVptr;
    var object* Nptr;
    var object* Iptr;
    # Key STACK_2 in der Hash-Tabelle suchen:
    if (hash_lookup(ht,STACK_2,&KVptr,&Nptr,&Iptr))
      # gefunden -> Value ersetzen:
      { value1 = KVptr[1] = popSTACK(); mv_count=1; skipSTACK(2); }
      else
      # nicht gefunden -> neuen Eintrag basteln:
      { var reg2 object freelist;
        hash_prepare_store(STACK_2);
        hash_store(STACK_2,STACK_0); # Eintrag basteln
        value1 = popSTACK(); mv_count=1; # value als Wert
        skipSTACK(2);
      }
  }}

# UP: Sucht ein Key in einer Hash-Tabelle und liefert den vorigen Wert.
# shifthash(ht,obj,value) == (SHIFTF (GETHASH obj ht) value)
# > ht: Hash-Tabelle
# > obj: Objekt
# > value: neuer Wert
# < ergebnis: alter Wert
# kann GC auslösen
  global object shifthash (object ht, object obj, object value);
  global object shifthash(ht,obj,value)
    var reg1 object ht;
    var reg3 object obj;
    var reg4 object value;
    { var object* KVptr;
      var object* Nptr;
      var object* Iptr;
      # Key obj in der Hash-Tabelle suchen:
      if (hash_lookup(ht,obj,&KVptr,&Nptr,&Iptr))
        # gefunden -> Value ersetzen:
        { var reg2 object oldvalue = KVptr[1];
          KVptr[1] = value;
          return oldvalue;
        }
        else
        # nicht gefunden -> neuen Eintrag basteln:
        { pushSTACK(obj); pushSTACK(value); # Key und Value retten
         {var reg2 object freelist;
          hash_prepare_store(STACK_1);
          hash_store(STACK_1,STACK_0); # Eintrag basteln
          skipSTACK(2);
          return NIL; # Default für den alten Wert ist NIL
        }}
    }

# (REMHASH key hashtable), CLTL S. 284
LISPFUNN(remhash,2)
  { var reg1 object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) { fehler_hashtable(ht); } # überprüfen
   {var reg2 object key = popSTACK(); # key-Argument
    var object* KVptr;
    var object* Nptr;
    var object* Iptr;
    # Key in der Hash-Tabelle suchen:
    if (hash_lookup(ht,key,&KVptr,&Nptr,&Iptr))
      # gefunden -> aus der Hashtabelle streichen:
      { var reg3 object index = *Iptr; # Index im Next-Vektor
        # mit Nptr = &TheSvector(TheHashtable(ht)->ht_ntable)->data[index]
        # und KVptr = &TheSvector(TheHashtable(ht)->ht_kvtable)->data[2*index]
        set_break_sem_2(); # Vor Unterbrechungen schützen
        *Iptr = *Nptr; # "Liste" verkürzen
        *KVptr++ = leer; *KVptr = leer; # Key und Value leeren
        # Freiliste verlängern:
        *Nptr = TheHashtable(ht)->ht_freelist;
        TheHashtable(ht)->ht_freelist = index;
        # COUNT decrementieren:
        TheHashtable(ht)->ht_count = fixnum_inc(TheHashtable(ht)->ht_count,-1);
        clr_break_sem_2(); # Unterbrechungen wieder zulassen
        # Bei COUNT < MINCOUNT die Hash-Tabelle verkleinern:
        if (posfixnum_to_L(TheHashtable(ht)->ht_count) < posfixnum_to_L(TheHashtable(ht)->ht_mincount))
          # Hash-Tabelle verkleinern:
          { # maxcount := (max (floor (/ maxcount rehash-size)) 1)
            pushSTACK(ht); # Hashtable retten
            pushSTACK(TheHashtable(ht)->ht_maxcount);
            pushSTACK(TheHashtable(ht)->ht_rehash_size); # REHASH-SIZE (>1)
            funcall(L(durch),2); # (/ maxcount rehash-size), ist < maxcount
            pushSTACK(value1);
            funcall(L(floor),1); # (floor ...), ein Integer >=0, < maxcount
           {var reg4 object maxcount = value1;
            if (eq(maxcount,Fixnum_0)) { maxcount = Fixnum_1; } # aus 0 mache 1
            resize(popSTACK(),maxcount); # Tabelle verkleinern
          }}
        value1 = T; mv_count=1; # T als Wert
      }
      else
      # nicht gefunden
      { value1 = NIL; mv_count=1; } # NIL als Wert
  }}

# (MAPHASH function hashtable), CLTL S. 285
LISPFUNN(maphash,2)
  { var reg3 object ht = STACK_0; # hashtable-Argument
    if (!hash_table_p(ht)) { fehler_hashtable(ht); } # überprüfen
    # Durch den Key-Value-Vektor von hinten durchlaufen und
    # für alle Key-Value-Paare mit Key /= "leer" die Funktion aufrufen:
   {var reg2 uintL index = 2*posfixnum_to_L(TheHashtable(ht)->ht_maxcount);
    STACK_0 = TheHashtable(ht)->ht_kvtable; # Key-Value-Vektor
    # Stackaufbau: function, Key-Value-Vektor.
    loop
      { if (index==0) break;
        index -= 2;
       {var reg1 object* KVptr = &TheSvector(STACK_0)->data[index];
        if (!eq(KVptr[0],leer)) # Key /= "leer" ?
          { pushSTACK(KVptr[0]); # Key als 1. Argument
            pushSTACK(KVptr[1]); # Value als 2. Argument
            funcall(STACK_(1+2),2); # (FUNCALL function Key Value)
      }}  }
    skipSTACK(2);
    value1 = NIL; mv_count=1; # NIL als Wert
  }}

# (CLRHASH hashtable), CLTL S. 285
LISPFUNN(clrhash,1)
  { var reg1 object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) { fehler_hashtable(ht); } # überprüfen
    clrhash(ht); # Tabelle leeren
    # Bei MINCOUNT > 0 die Hash-Tabelle verkleinern:
    if (!eq(TheHashtable(ht)->ht_mincount,Fixnum_0))
      { ht = resize(ht,Fixnum_1); } # auf MAXCOUNT:=1 verkleinern, so daß MINCOUNT:=0
    value1 = ht; mv_count=1; # Hash-Tabelle als Wert
  }

# (HASH-TABLE-COUNT hashtable), CLTL S. 285
LISPFUNN(hash_table_count,1)
  { var reg1 object ht = popSTACK(); # hashtable-Argument
    if (!hash_table_p(ht)) { fehler_hashtable(ht); } # überprüfen
    value1 = TheHashtable(ht)->ht_count; mv_count=1; # Fixnum COUNT als Wert
  }

# Hilfsfunktionen für WITH-HASH-TABLE-ITERATOR, CLTL2 S. 439:
# (SYSTEM::HASH-TABLE-ITERATOR hashtable) liefert einen internen Zustand
# für das Iterieren durch eine Hash-Tabelle.
# (SYSTEM::HASH-TABLE-ITERATE internal-state) iteriert durch eine Hash-Tabelle
# um eins weiter, verändert dabei internal-state und liefert: 3 Werte
# T, key, value des nächsten Hash-Tabellen-Eintrags bzw. 1 Wert NIL am Schluß.

LISPFUNN(hash_table_iterator,1)
  { var reg1 object ht = STACK_0; # hashtable-Argument
    if (!hash_table_p(ht)) { fehler_hashtable(ht); } # überprüfen
    # Ein interner Zustand besteht aus dem Key-Value-Vektor und einem Index.
    STACK_0 = TheHashtable(ht)->ht_kvtable; # Key-Value-Vektor
   {var reg3 object maxcount = TheHashtable(ht)->ht_maxcount; # maxcount
    var reg2 object state = allocate_cons();
    Car(state) = popSTACK(); # Key-Value-Vektor als Car
    Cdr(state) = maxcount; # maxcount als Cdr
    value1 = state; mv_count=1; # state als Wert
  }}

LISPFUNN(hash_table_iterate,1)
  { var reg1 object state = popSTACK(); # interner Zustand
    if (consp(state)) # hoffentlich ein Cons
      { var reg4 object table = Car(state); # Key-Value-Vektor
        loop
          { var reg3 uintL index = posfixnum_to_L(Cdr(state));
            if (index==0) break; # index=0 -> keine Elemente mehr
            Cdr(state) = fixnum_inc(Cdr(state),-1); # Index decrementieren
           {var reg2 object* KVptr = &TheSvector(table)->data[2*index-2];
            if (!eq(KVptr[0],leer)) # Key /= "leer" ?
              { value2 = KVptr[0]; # Key als 2. Wert
                value3 = KVptr[1]; # Value als 3. Wert
                value1 = T; mv_count=3; return;
      }   }}  }
    value1 = NIL; mv_count=1; return; # 1 Wert NIL
  }

# UP: Berechnet einen portablen EQUAL-Hashcode eines Objekts.
# sxhash(obj)
# Er ist nur bis zur nächsten Modifizierung des Objekts gültig.
# Aus (equal X Y) folgt (= (sxhash X) (sxhash Y)).
# > obj: ein Objekt
# < ergebnis: Hashcode, eine 32-Bit-Zahl
  local uint32 sxhash (object obj);
# Hilfsfunktionen bei bekanntem Typ:
  # Atom -> Fallunterscheidung nach Typ
  local uint32 sxhash_atom (object obj);
  local uint32 sxhash_atom(obj)
    var reg1 object obj;
    { switch (typecode(obj)) # je nach Typ
        { case_symbol: # Symbol
            # Printname verwerten
            # (nicht auch die Home-Package, da sie sich bei UNINTERN verändert)
            return hashcode_string(Symbol_name(obj))+0x339B0E4CUL;
          case_machine: # Maschinenpointer
          default:
            # Adresse darf nicht verwendet werden, nur den Typ verwerten
            return highlow32(typecode(obj),0xDABE); # Typinfo*2^16+Kennung
          case_bvector: # bit-vector
            # Bit-Vektor-Inhalt
            return hashcode_bvector(obj);
          case_string: # String
            # String-Inhalt
            return hashcode_string(obj);
          case_svector: # Simple-Vector
            # nur die Länge verwerten
            return TheSvector(obj)->length + 0x4ECD0A9FUL;
          case_ovector: # (vector t)
          case_array1: # allgemeiner Array
            # mehrdimensionaler Array -> nur Rang verwerten
            return TheArray(obj)->rank + 0xAAFAFAAEUL;
          case_structure: # Structure
            # nur Structure-Typ (Liste (name_1 name_2 ... . name_n)) verwerten
            { check_SP();
              return sxhash(TheStructure(obj)->structure_types) + 0xAD2CD2AEUL;
            }
          case_stream: # Stream
            # nur Streamtyp verwerten
            return TheStream(obj)->strmtype + 0x3DAEAE55UL;
         {var reg3 uint32 bish_code;
          case_closure: # Closure
            # alle Elemente verwerten
            bish_code = 0xB0DD939EUL; goto record_all;
          case_orecord: # OtherRecord
            # Record-Typ verwerten, außerdem:
            # Package: Package-Name verwerten (nicht ganz OK, da eine
            #          Package mit RENAME-PACKAGE umbenannt werden kann!)
            # Pathname, Byte, LoadTimeEval: alle Komponenten verwerten
            # Hash-Table, Readtable, Random-State: nichts weiter
            { var reg6 uintB rectype = TheRecord(obj)->rectype;
              bish_code = 0xB04D939EUL + rectype;
              if (rectype == Rectype_Package) # Package ?
                # Package-Name verwerten
                { var reg4 uint32 next_code = hashcode_string(ThePackage(obj)->pack_name);
                  return rotate_left(1,next_code) + bish_code;
                }
              elif ((rectype == Rectype_Pathname) # Pathname ?
                    || (rectype == Rectype_Byte) # Byte ?
                    || (rectype == Rectype_Loadtimeeval) # LoadTimeEval ?
                   )
                { record_all:
                  #  Record, in dem man alle Elemente verwerten kann
                  check_SP();
                 {var reg2 object* ptr = &TheRecord(obj)->recdata[0];
                  var reg5 uintC count = TheRecord(obj)->reclength;
                  dotimespC(count,count,
                    # Hashcode der nächsten Komponente dazunehmen:
                    { var reg4 uint32 next_code = sxhash(*ptr++);
                      bish_code = misch(bish_code,next_code);
                    });
                  return bish_code;
                }}
              else
                { return bish_code; }
         }  }
          case_char: # Character
            # EQ-Hashcode nehmen (bei Characters ist ja EQUAL == EQL == EQ)
            return hashcode1(obj);
          case_subr: # SUBR
            # Namen verwerten
            check_SP(); return sxhash(TheSubr(obj)->name) + 0xFF3319BAUL;
          case_fsubr: # FSUBR
            # Namen verwerten
            check_SP(); return sxhash(TheFsubr(obj)->name) + 0xFF3319BAUL;
          case_system: # Frame-Pointer, Read-Label, System
            # Adresse verwenden
            return hashcode1(obj);
          # Zahlen: nach Inhalt, wie bei EQL
          case_fixnum: # Fixnum
            return hashcode_fixnum(obj);
          case_bignum: # Bignum
            return hashcode_bignum(obj);
          case_sfloat: # Short-Float
            return hashcode_sfloat(obj);
          case_ffloat: # Single-Float
            return hashcode_ffloat(obj);
          case_dfloat: # Double-Float
            return hashcode_dfloat(obj);
          case_lfloat: # Long-Float
            return hashcode_lfloat(obj);
          case_ratio: # Ratio
            # beide Komponenten hashen, mischen
            { var reg2 uint32 code1 = sxhash(TheRatio(obj)->rt_num);
              var reg3 uint32 code2 = sxhash(TheRatio(obj)->rt_den);
              return misch(code1,code2);
            }
          case_complex: # Complex
            # beide Komponenten hashen, mischen
            { var reg2 uint32 code1 = sxhash(TheComplex(obj)->c_real);
              var reg3 uint32 code2 = sxhash(TheComplex(obj)->c_imag);
              return misch(code1,code2);
            }
    }   }
# Cons -> Inhalt bis zur Tiefe 4 ansehen:
# Jeweils Hashcode des CAR und Hashcode des CDR bestimmen
# und geshiftet kombinieren. Als Shifts passen z.B. 16,7,5,3,
# da {0,16} + {0,7} + {0,5} + {0,3} = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
# aus 16 verschiedenen Elementen von {0,...,31} besteht.
  # Objekt, bei Cons nur bis Tiefe 0
  local uint32 sxhash_cons0 (object obj);
  local uint32 sxhash_cons0(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return sxhash_atom(obj); }
        else
        # Cons -> Hashcode := 1
        { return 1; }
    }
  # Objekt, bei Cons nur bis Tiefe 1
  local uint32 sxhash_cons1 (object obj);
  local uint32 sxhash_cons1(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return sxhash_atom(obj); }
        else
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        { var reg2 uint32 code1 = sxhash_cons0(Car(obj));
          var reg3 uint32 code2 = sxhash_cons0(Cdr(obj));
          return rotate_left(3,code1) ^ code2;
    }   }
  # Objekt, bei Cons nur bis Tiefe 2
  local uint32 sxhash_cons2 (object obj);
  local uint32 sxhash_cons2(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return sxhash_atom(obj); }
        else
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        { var reg2 uint32 code1 = sxhash_cons1(Car(obj));
          var reg3 uint32 code2 = sxhash_cons1(Cdr(obj));
          return rotate_left(5,code1) ^ code2;
    }   }
  # Objekt, bei Cons nur bis Tiefe 3
  local uint32 sxhash_cons3 (object obj);
  local uint32 sxhash_cons3(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return sxhash_atom(obj); }
        else
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        { var reg2 uint32 code1 = sxhash_cons2(Car(obj));
          var reg3 uint32 code2 = sxhash_cons2(Cdr(obj));
          return rotate_left(7,code1) ^ code2;
    }   }
  # Objekt, bei Cons nur bis Tiefe 4
  local uint32 sxhash(obj)
    var reg1 object obj;
    { if (atomp(obj))
        { return sxhash_atom(obj); }
        else
        # Cons -> Hashcode des CAR und des CDR bestimmen und mischen:
        { var reg2 uint32 code1 = sxhash_cons3(Car(obj));
          var reg3 uint32 code2 = sxhash_cons3(Cdr(obj));
          return rotate_left(16,code1) ^ code2;
    }   }

# (SXHASH object), CLTL S. 285
LISPFUNN(sxhash,1)
  { value1 = UL_to_I(sxhash(popSTACK())); mv_count=1; } # Hashcode als Integer

