# Arithmetik, Level 1
# operiert auf Digit Sequences (DS) und Unsigned Digit Sequences (UDS).


# Aus LISPBIBL.D importiere:
# intDsize        Anzahl Bits in einem Digit
# uintD, sintD    Integer-Typen für ein Digit
# log2_intDsize   log2(intDsize)
# HAVE_DD         Flag, das anzeigt, ob ein Integertyp für Doppel-Digits da ist
# intDDsize       Anzahl Bits in einem Doppel-Digit
# uintDD,sintDD   Integer-Typen für ein Doppel-Digit

#if !((32%intDsize)==0)
  #error "intDsize sollte ein Teiler von 32 sein!"
#endif


# Vorzeichen eines Digit bestimmen
# sign_of_sintD(wert)
# > wert: ein Digit
# < sintD ergebnis: 0 falls wert>=0, -1 falls wert<0.
  global sint32 sign_of_sintD (sintD wert);
#if (intDsize==8)
  #define sign_of_sintD(x)  (sintD)(sign_of_sint16((sint16)(sint8)(x)))
#endif
#if (intDsize==16)
  #define sign_of_sintD(x)  (sintD)(sign_of_sint16(x))
#endif
#if (intDsize==32)
  #define sign_of_sintD(x)  (sintD)(sign_of_sint32(x))
#endif

# High-Digit eines Doppel-Digit bestimmen
# highD(wert)
#if HAVE_DD
  #if (!(intDsize==16))
    #define highD(x)  ((uintD)((uintDD)(x)>>intDsize))
  #else
    #define highD  high16
  #endif
#endif

# Low-Digit eines Doppel-Digit bestimmen
# lowD(wert)
#if HAVE_DD
  #define lowD(x)  ((uintD)(uintDD)(x))
#endif

# Ein Doppel-Digit aus ihrem High-Digit und ihrem Low-Digit bestimmen:
# highlowDD(uintD high, uintD low)
#if HAVE_DD
  #if (!(intDsize==16))
    #define highlowDD(x,y)  (((uintDD)(uintD)(x)<<intDsize)|(uintDD)(uintD)(y))
  #else
    #define highlowDD  highlow32
  #endif
#endif

# Ein Doppel-Digit aus ihrem High-Digit und ihrem Low-Digit 0 bestimmen:
# highlowDD_0(uintD high)
#if HAVE_DD
  #if (!(intDsize==16))
    #define highlowDD_0(x)  ((uintDD)(uintD)(x)<<intDsize)
  #else
    #define highlowDD_0(x)  highlow32_0
  #endif
#endif

# Zwei Digits multiplizieren:
# (uintDD)hilo = muluD(uintD arg1, uintD arg2)
# bzw.
# muluD(uintD arg1, uintD arg2, uintD hi =, uintD lo =);
#if HAVE_DD
  #if (intDsize==8)
    #ifdef GNU
      #define muluD(arg1,arg2)  ((uintDD)((uintD)(arg1)*(uintD)(arg2)))
    #else
      #define muluD(arg1,arg2)  ((uintDD)(uintD)(arg1)*(uintDD)(uintD)(arg2))
    #endif
  #endif
  #if (intDsize==16)
    #define muluD  mulu16
  #endif
#else
  #if (intDsize==32)
    #define muluD  mulu32
  #endif
#endif

# Zwei Digits multiplizieren, mit einem Digit als Ergebnis.
# (uintD)lo = muluD_unchecked(uintD arg1, uintD arg2)
# Es wird vorausgesetzt, daß arg1*arg2 < 2^intDsize.
  #if (intDsize==8) || (intDsize==16)
    #define muluD_unchecked(arg1,arg2)  ((uintD)((uintD)(arg1)*(uintD)(arg2)))
  #endif
  #if (intDsize==32)
    #define muluD_unchecked(arg1,arg2)  mulu32_unchecked(arg1,arg2)
  #endif

# Durch ein Digit dividieren:
# divuD(uintDD x, uintD y, uintD q =, uintD r =);
# bzw.
# divuD(uintD xhi, uintD xlo, uintD y, uintD q =, uintD r =);
# dividiert x/y und liefert q = floor(x/y) und r = (x mod y). x = q*y+r.
# Es wird vorausgesetzt, daß 0 <= x < 2^intDsize*y.
#if HAVE_DD
  #if (intDsize==8)
    #define divuD  divu_1616_1616
  #endif
  #if (intDsize==16)
    #define divuD  divu_3216_1616
  #endif
#else
  #if (intDsize==32)
    #define divuD  divu_6432_3232
  #endif
#endif

# Durch ein Digit dividieren:
# floorD(uintD x, uintD y)
# dividiert x/y und liefert q = floor(x/y).
# Es wird vorausgesetzt, daß y > 0.
  #if (intDsize==8) || (intDsize==16)
    #define floorD(arg1,arg2)  (floor((uintD)(arg1),(uintD)(arg2)))
  #endif
  #if (intDsize==32)
    #define floorD  divu_3232_3232_
  #endif

# Digit sequence (DS) - nur intern verwendet -
# Zusammenhängender Speicherbereich mit n (ein uintC) Digits,
# zwischen zwei Pointer MSDptr und LSDptr.
#  MSDptr                  LSDptr
# | MSD ............. LSW |
# [abgekürzt: MSDptr/n/LSDptr ]
# In 68000-Manier (vgl. ADDX, SUBX) ist das Most significant Digit an der
# untersten Adresse, nämlich MSDptr. LSDptr = MSDptr + n zeigt hinter die DS.
# Falls n = 0, wird die Zahl 0 dargestellt.
# Falls n > 0, ist das höchstwertige Bit (nämlich  Bit (intDsize-1) von
#              *MSDptr) das Vorzeichenbit. Schreibt man es noch unendlich
#              oft an, so erhält man die "unendliche Bitfolge".
# Normalisierte Digit sequence (NDS) ist eine solche, bei der das MSD nötig
# ist, also n = 0 oder (n > 0 und nicht alle höchstwertigen intDsize+1 Bits
# sind gleich).
# In C:
#   uintD* MSWptr und uintC len.
#   MSWptr[0] ... MSWptr[(uintL)len-1] sind die Digits.

# Unsigned Digit sequence (UDS) - nur intern verwendet -
# wie DS (MSD unten, LSD oben), nur ohne Vorzeichen.
# Normalized Unsigned Digit sequence (NUDS):
# wie UDS, nur ist entweder n=0 (Zahl 0) oder bei n>0 : *MSDptr >0.
# (d.h. die Zahl >=0 kann nicht mit weniger Digits als UDS dargestellt werden).
# In C:
#   uintD* MSWptr und uintC len.
#   MSWptr[0] ... MSWptr[(uintL)len-1] sind die Digits.

# Zur Konstruktion konstanter DS: D(byte0,byte1,byte2,byte3,) liefert
# die 32 Bits von {byte0,byte1,byte2,byte3} als 32/intDsize Digits.
  #if (intDsize==8)
    #define D(byte0,byte1,byte2,byte3,dummy)  byte0,byte1,byte2,byte3,
  #endif
  #if (intDsize==16)
    #define D(byte0,byte1,byte2,byte3,dummy)  ((byte0<<8)|byte1),((byte2<<8)|byte3),
  #endif
  #if (intDsize==32)
    #define D(byte0,byte1,byte2,byte3,dummy)  \
      (((uintD)(byte0)<<24)|((uintD)(byte1)<<16)|((uintD)(byte2)<<8)|((uintD)(byte3))),
  #endif

typedef struct { uintD* MSDptr; uintC len; uintD* LSDptr; } DS;


# Es gibt für die innersten Schleifen vier Möglichkeiten:
# LOOP_EXTERN_C     Alle Schleifen als externe C-compilierte Routinen.
#                   Portabel, aber evtl. ineffizient.
# LOOP_INLINE_C     Schleifen ohne Wert (mit GNU-Compiler: alle Schleifen)
#                   als Macros.
#                   Portabel, aber evtl. ineffizient.
# LOOP_EXTERN_ASM   Alle Schleifen als externe Assembler-Routinen.
#                   Effizienter, aber immer noch Function-Call-Overhead.
# LOOP_INLINE_ASM   Schleifen ohne Wert (mit GNU-Compiler: alle Schleifen)
#                   als macroexpandierte Assembler-Routinen inline.
#                   Ganz effizient.

#if defined(MC680X0) || defined(SPARC)  || defined(I80Z86) || defined(MIPS) || defined(VAX)
  # diese Assembler beherrsche ich
  #if (defined(GNU) && defined(WANT_LOOP_INLINE))
    # der GNU-Compiler kann Inline-Assembler
    #define LOOP_INLINE_ASM
  #else
    # sonst mit externen Routinen arbeiten
    #define LOOP_EXTERN_ASM
  #endif
#else
  # sonst die portable Lösung
  #define LOOP_INLINE_C
#endif


#ifdef LOOP_EXTERN_C
  # Die Definitionen samt portablem C-Code:
  #include "arilev1c.c"
#endif

# Die Inline-Macros
#ifdef LOOP_INLINE_ASM
  # sind momentan nicht implementiert
  #define LOOP_EXTERN_ASM  # stattdessen extern in Assembler
#endif

#ifdef LOOP_EXTERN_ASM
  # Die Assembler-Definitionen:
    #define INCLUDED_FROM_C
    #if defined(MC680X0)
      #if !defined(MC680Y0)
        #include "ari68000.c"
      #else
        #include "ari68020.c"
      #endif
    #endif
    #if defined(SPARC)
      #include "arisparc.c"
    #endif
    #if defined(I80Z86)
      #include "ari80386.c"
    #endif
    #if defined(MIPS)
      #include "arimips.c"
    #endif
    #if defined(VAX)
      #include "arivax.c"
    #endif
    #undef INCLUDED_FROM_C
  # Die Extern-Deklarationen:
    #include "arilev1e.c"
  # Die nicht in Assembler geschriebenen Teile nehmen wir vom portablen C-Code:
    #define LOOP_INLINE_C
#endif

#ifdef LOOP_INLINE_C
  # Die Definitionen samt portablem C-Code und
  # - für den GNU-Compiler - Inline-Deklarationen:
  #include "arilev1i.c"
#endif

