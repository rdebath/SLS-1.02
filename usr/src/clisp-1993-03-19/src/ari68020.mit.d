# Externe Routinen zu ARILEV1.D
# Compiler: CC oder GNU-C auf SUN3 oder AMIGA
# Parameter-Übergabe:
#   auf dem Stack: sp@(4), sp@(8), ... (.W-Größen belegen 4 Byte!),
#   Rückgabewert in d0.
# Register a0-a1,d0-d1 frei verwendbar,
# Register a2-a4,d2-d7 müssen gerettet werden.
# Einstellungen: intCsize=16, intDsize=32.

#ifdef INCLUDED_FROM_C

  #define COPY_LOOPS
  #define FILL_LOOPS
  #define CLEAR_LOOPS
  #define LOG_LOOPS
  #define TEST_LOOPS
  #define ADDSUB_LOOPS
  #define SHIFT_LOOPS
  #define MUL_LOOPS
  #define DIV_LOOPS

#else

           .text

           .globl _copy_loop_up,_copy_loop_down,_fill_loop_up,_fill_loop_down
           .globl _clear_loop_up,_clear_loop_down
           .globl _or_loop_up,_xor_loop_up,_and_loop_up,_eqv_loop_up
           .globl _nand_loop_up,_nor_loop_up,_andc2_loop_up,_orc2_loop_up
           .globl _not_loop_up
           .globl _and_test_loop_up,_test_loop_up,_compare_loop_up
           .globl _add_loop_down,_addto_loop_down,_inc_loop_down
           .globl _sub_loop_down,_subx_loop_down,_subfrom_loop_down,_dec_loop_down
           .globl _neg_loop_down
           .globl _shift1left_loop_down,_shiftleft_loop_down,_shiftleftcopy_loop_down
           .globl _shift1right_loop_up,_shiftright_loop_up,_shiftrightsigned_loop_up,_shiftrightcopy_loop_up
           .globl _mulusmall_loop_down,_mulu_loop_down,_muluadd_loop_down,_mulusub_loop_down
           .globl _divu_loop_up,_divucopy_loop_up

#ifndef __GNUC__ /* mit GNU-C machen wir mulu32() als Macro, der inline multipliziert */
           .globl _mulu32_
! extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
! 2^32*hi+lo := arg1*arg2.
_mulu32_:  ! Input in d0,d1, Output in d0,mulu32_high
           movel sp@(4),d0
           movel sp@(8),d1
           mulul d1,d1:d0
           movel d1,(_mulu32_high) ! Adressierung?? Deklaration??
           rts
#endif

#ifndef __GNUC__ /* mit GNU-C machen wir divu_6432_3232() als Macro, der inline dividiert */
           .globl _divu_6432_3232_
! extern struct { uint32 q; uint32 r; } divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
! x = 2^32*xhi+xlo = q*y+r schreiben. Sei bekannt, daß 0 <= x < 2^32*y .
_divu_6432_3232_: ! Input in d1,d0,d2, Output in d0,divu_32_rest
           movel sp@(4),d1
           movel sp@(8),d0
           divul sp@(12),d1:d0 ! x = d1|d0 durch y dividieren
           movel d1,(_divu_32_rest) ! Rest ablegen ! Adressierung?? Deklaration??
           rts
#endif

| extern uintD* copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
_copy_loop_up: | Input in a0,a1,d0.W, Output in d0
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a0@+,a1@+
    2:       dbra d0,1b
           movel a1,d0
           rts

| extern uintD* copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_copy_loop_down: | Input in a0,a1,d0.W, Output in d0
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a0@-,a1@-
    2:       dbra d0,1b
           movel a1,d0
           rts

| extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
_fill_loop_up: | Input in a0,d0.W,d1, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           movel sp@(12),d1
           bras 2f
    1:       movel d1,a0@+
    2:       dbra d0,1b
           movel a0,d0
           rts

| extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
_fill_loop_down: | Input in a0,d0.W,d1, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           movel sp@(12),d1
           bras 2f
    1:       movel d1,a0@-
    2:       dbra d0,1b
           movel a0,d0
           rts

| extern uintD* clear_loop_up (uintD* destptr, uintC count);
_clear_loop_up: | Input in a0,d0.W, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           bras 2f
    1:       clrl a0@+
    2:       dbra d0,1b
           movel a0,d0
           rts

| extern uintD* clear_loop_down (uintD* destptr, uintC count);
_clear_loop_down: | Input in a0,d0.W, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           bras 2f
    1:       clrl a0@-
    2:       dbra d0,1b
           movel a0,d0
           rts

| extern void or_loop_up (uintD* xptr, uintD* yptr, uintC count);
_or_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a1@+,d1
             orl d1,a0@+
    2:       dbra d0,1b
           rts

| extern void xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
_xor_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a1@+,d1
             eorl d1,a0@+
    2:       dbra d0,1b
           rts

| extern void and_loop_up (uintD* xptr, uintD* yptr, uintC count);
_and_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a1@+,d1
             andl d1,a0@+
    2:       dbra d0,1b
           rts

| extern void eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
_eqv_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a1@+,d1
             eorl d1,a0@
             notl a0@+
    2:       dbra d0,1b
           rts

| extern void nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
_nand_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a1@+,d1
             andl d1,a0@
             notl a0@+
    2:       dbra d0,1b
           rts

| extern void nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
_nor_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a1@+,d1
             orl d1,a0@
             notl a0@+
    2:       dbra d0,1b
           rts

| extern void andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
_andc2_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a1@+,d1
             notl d1
             andl d1,a0@+
    2:       dbra d0,1b
           rts

| extern void orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
_orc2_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a1@+,d1
             notl d1
             orl d1,a0@+
    2:       dbra d0,1b
           rts

| extern void not_loop_up (uintD* xptr, uintC count);
_not_loop_up: | Input in a0,d0.W
           movel sp@(4),a0
           movew sp@(8+2),d0
           bras 2f
    1:       notl a0@+
    2:       dbra d0,1b
           rts

| extern boolean and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
_and_test_loop_up: | Input in a0,a1,d0.W, verändert d1, Output in d0.W=d0.L
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movel a0@+,d1
             andl a1@+,d1
             bnes 3f
    2:       dbra d0,1b
           clrl d0
           rts
    3:     moveq #1,d0
           rts

| extern boolean test_loop_up (uintD* ptr, uintC count);
_test_loop_up: | Input in a0,d0.W, Output in d0.W=d0.L
           movel sp@(4),a0
           movew sp@(8+2),d0
           bras 2f
    1:       tstl a0@+
             bnes 3f
    2:       dbra d0,1b
           clrl d0
           rts
    3:     moveq #1,d0
           rts

| extern signean compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
_compare_loop_up: | Input in a0,a1,d0.W, Output in d0.W=d0.L
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       cmpml a1@+,a0@+
             bnes 3f
    2:       dbra d0,1b
           clrl d0
           rts
    3:     bcss 4f
           moveq #1,d0
           rts
    4:     moveq #-1,d0
           rts

| extern uintD add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
_add_loop_down: | Input in a0,a1,a2,d0.W, verändert d1,d2, Output in d0
           moveml a2/d2,sp@-
           movel sp@(8+4),a0
           movel sp@(8+8),a1
           movel sp@(8+12),a2
           movew sp@(8+16+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       movel a0@-,d1
             movel a1@-,d2
             addxl d2,d1
             movel d1,a2@-
    2:       dbra d0,1b
           subxl d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           moveml sp@+,a2/d2
           rts

| extern uintD addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_addto_loop_down: | Input in a0,a1,d0.W, Output in d0
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       addxl a0@-,a1@-
    2:       dbra d0,1b
           subxl d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD inc_loop_down (uintD* ptr, uintC count);
_inc_loop_down: | Input in a0,d0.W, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           bras 2f
    1:       addql #1,a0@-
    2:       dbcc d0,1b
           subxl d0,d0       | kein Carry -> d0.W=0, sonst d0.W=-1 für Übertrag
           rts

| extern uintD sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
_sub_loop_down: | Input in a0,a1,a2,d0.W, verändert d1,d2, Output in d0
           moveml a2/d2,sp@-
           movel sp@(8+4),a0
           movel sp@(8+8),a1
           movel sp@(8+12),a2
           movew sp@(8+16+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       movel a0@-,d1
             movel a1@-,d2
             subxl d2,d1
             movel d1,a2@-
    2:       dbra d0,1b
           subxl d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           moveml sp@+,a2/d2
           rts

| extern uintD subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
_subx_loop_down: | Input in a0,a1,a2,d0.W,d1, verändert d2, Output in d0
           moveml a2/d2,sp@-
           movel sp@(8+4),a0
           movel sp@(8+8),a1
           movel sp@(8+12),a2
           movew sp@(8+16+2),d0
           movel sp@(8+20),d1
           roxrl #1,d1      | X-Bit initialisieren
           bras 2f
    1:       movel a0@-,d1
             movel a1@-,d2
             subxl d2,d1
             movel d1,a2@-
    2:       dbra d0,1b
           subxl d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           moveml sp@+,a2/d2
           rts

| extern uintD subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_subfrom_loop_down: | Input in a0,a1,d0.W, Output in d0
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       subxl a0@-,a1@-
    2:       dbra d0,1b
           subxl d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD dec_loop_down (uintD* ptr, uintC count);
_dec_loop_down: | Input in a0,d0.W, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           bras 2f
    1:       subql #1,a0@-
    2:       dbcc d0,1b       | kein Carry -> Schleife abbrechen
           subxl d0,d0       | kein Carry -> d0.W=0, sonst d0.W=-1 als Übertrag
           rts

| extern uintD neg_loop_down (uintD* ptr, uintC count);
_neg_loop_down: | Input in a0,d0.W, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       negxl a0@-
    2:       dbra d0,1b
           subxl d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD shift1left_loop_down (uintD* ptr, uintC count);
_shift1left_loop_down: | Input in a0,d0.W, Output in d0.L
           movel sp@(4),a0
           movew sp@(8+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       roxlw a0@-     | Digit a0@- um 1 Bit links schieben, X-Bit als Buffer
             roxlw a0@-
    2:       dbra d0,1b
           subxl d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
_shiftleft_loop_down: | Input in a0,d0.W,d1.W,d2, Output in d0
#if 1
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movew sp@(16+8+2),d0
           movew sp@(16+12+2),d1
           movel sp@(16+16),d2
           moveq #32,d5
           subw d1,d5
           | a0 = ptr, d0.W = count, d1.W = i, d5.W = 32-i,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           bras 2f
    1:       movel a0@-,d3  | d3.L = neues Digit
             movel d3,d4
             lsll d1,d4      | um i Bits nach links schieben
             orl d2,d4       | mit vorigem Übertrag kombinieren
             movel d4,a0@   | 32 Bits ablegen
             movel d3,d2
             lsrl d5,d2      | neuen Übertrag bilden
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#else
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movew sp@(16+8+2),d0
           movew sp@(16+12+2),d1
           movel sp@(16+16),d2
           moveq #1,d5
           lsll d1,d5
           subql #1,d5
           | a0 = ptr, d0.W = count, d1.W = i, d5.L = 2^i-1
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           bras 2f
    1:       movel a0@-,d3  | d3.L = neues Digit
             roll d1,d3      | um i Bits links rotieren
             movel d3,d4
             andl d5,d3      | untere i Bits in d3
             eorl d3,d4      | obere 32-i Bits in d4
             orl d2,d4       | mit vorigem übertrag kombinieren
             movel d4,a0@   | 32 Bits ablegen
             movel d3,d2     | neuer Übertrag
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#endif

| extern uintD shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
_shiftleftcopy_loop_down: | Input in a0,a1,d0.W,d1.W, Output in d0
#if 1
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movel sp@(16+8),a1
           movew sp@(16+12+2),d0
           movew sp@(16+16+2),d1
           moveq #32,d5
           subw d1,d5
           clrl d2
           | a0 = sourceptr, a1 = destptr, d0.W = count, d1.W = i, d5.W = 32-i,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           bras 2f
    1:       movel a0@-,d3  | d3.L = neues Digit
             movel d3,d4
             lsll d1,d4      | um i Bits nach links schieben
             orl d2,d4       | mit vorigem Übertrag kombinieren
             movel d4,a1@-  | 32 Bits ablegen
             movel d3,d2
             lsrl d5,d2      | neuen Übertrag bilden
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#else
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movel sp@(16+8),a1
           movew sp@(16+12+2),d0
           movew sp@(16+16+2),d1
           moveq #1,d5
           lsll d1,d5
           subql #1,d5
           | a0 = sourceptr, a1 = destptr, d0.W = count, d1.W = i, d5.L = 2^i-1
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           bras 2f
    1:       movel a0@-,d3  | d3.L = neues Digit
             roll d1,d3      | um i Bits links rotieren
             movel d3,d4
             andl d5,d3      | untere i Bits in d3
             eorl d3,d4      | obere 32-i Bits in d4
             orl d2,d4       | mit vorigem übertrag kombinieren
             movel d4,a1@-  | 32 Bits ablegen
             movel d3,d2     | neuer Übertrag
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#endif

| extern uintD shift1right_loop_up (uintD* ptr, uintC count, uintC carry);
_shift1right_loop_up: | Input in a0,d0.W,d1, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           movel sp@(12),d1
           roxrl #1,d1       | X-Bit löschen oder setzen, je nach d1
           bras 2f
    1:       roxrw a0@+     | Digit a0@+ um 1 Bit rechts schieben, X-Bit als Buffer
             roxrw a0@+
    2:       dbra d0,1b
           subxl d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);
_shiftright_loop_up: | Input in a0,d0.W,d1.W, Output in d0
#if 1
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movew sp@(16+8+2),d0
           movew sp@(16+12+2),d1
           moveq #32,d5
           subw d1,d5
           | a0 = ptr, d0.W = count, d1.W = i, d5.W = 32-i,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           clrl d2
           bras 2f
    1:       | a0 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i, d5.W = 32-i,
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             movel a0@,d3   | neue Daten
             movel d3,d4
             lsrl d1,d3      | um i Bits rechts schieben
             orl d2,d3       | und mit vorigem Übertrag kombinieren
             movel d3,a0@+  | ablegen
             lsll d5,d4      | um (32-i) Bits links geschoben
             movel d4,d2     | liefert neuen Übertrag
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#else
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movew sp@(16+8+2),d0
           movew sp@(16+12+2),d1
           moveq #-1,d5
           lsrl d1,d5
           | a0 = ptr, d0.W = count, d1.W = i, d5.L = 2^(32-i)-1,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           clrl d2
           bras 2f
    1:       | a0 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i, d5.L = 2^(32-i)-1,
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             movel a0@,d3   | neue Daten
             rorl d1,d3      | um i Bits rechts rotieren
             movel d3,d4
             andl d5,d3      | untere 32-i Bits
             eorl d3,d4      | obere i Bits
             orl d2,d3       | und mit vorigem Übertrag kombinieren
             movel d4,d2     | neuer Übertrag
             movel d3,a0@+  | ablegen
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#endif

| extern uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
_shiftrightsigned_loop_up: | Input in a0,d0.W,d1.W, Output in d0
#if 1
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movew sp@(16+8+2),d0
           movew sp@(16+12+2),d1
           moveq #32,d5
           subw d1,d5
           | a0 = ptr, d0.W = count, d1.W = i, d5.W = 32-i,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           subqw #1,d0
           movel a0@,d3     | erstes Digit
           movel d3,d4
           asrl d1,d3        | um i Bits rechts schieben
           bras 2f
    1:       | a0 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i, d5.W = 32-i,
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             movel a0@,d3   | neue Daten
             movel d3,d4
             lsrl d1,d3      | um i Bits rechts schieben
             orl d2,d3       | und mit vorigem Übertrag kombinieren
    2:       movel d3,a0@+  | ablegen
             lsll d5,d4      | um (32-i) Bits links geschoben
             movel d4,d2     | liefert neuen Übertrag
             dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#else
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movew sp@(16+8+2),d0
           movew sp@(16+12+2),d1
           moveq #-1,d5
           lsrl d1,d5
           | a0 = ptr, d0.W = count, d1.W = i, d5.L = 2^(32-i)-1,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           subqw #1,d0
           movel a0@,d3     | erstes Digit
           movel d3,d4
           rorl d1,d4        | um i Bits rechts rotieren
           movel d5,d2
           notl d2
           andl d4,d2        | obere 32-i Bits
           asrl d1,d3        | erstes Digit um i Bits rechts shiften
           bras 2f
    1:       | a0 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i, d5.L = 2^(32-i)-1,
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             movel a0@,d3   | neue Daten
             rorl d1,d3      | um i Bits rechts rotieren
             movel d3,d4
             andl d5,d3      | untere 32-i Bits
             eorl d3,d4      | obere i Bits
             orl d2,d3       | und mit vorigem Übertrag kombinieren
             movel d4,d2     | neuer Übertrag
    2:       movel d3,a0@+  | ablegen
             dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#endif

| extern uintD shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
_shiftrightcopy_loop_up: | Input in a0,a1,d0.W,d1.W,d2, Output in d0
#if 1
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movel sp@(16+8),a1
           movew sp@(16+12+2),d0
           movew sp@(16+16+2),d1
           movel sp@(16+20),d2
           moveq #32,d5
           subw d1,d5
           | a0 = ptr, d0.W = count, d1.W = i, d5.W = 32-i
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           bras 2f
    1:       | a0,a1 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i, d5.W = 32-i
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             movel a0@+,d3  | neue Daten
             movel d3,d4
             lsrl d1,d3      | um i Bits rechts schieben
             orl d2,d3       | und mit vorigem Übertrag kombinieren
             movel d3,a1@+  | ablegen
             lsll d5,d4      | um (32-i) Bits links geschoben
             movel d4,d2     | liefert neuen Übertrag
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#else
           moveml d2-d5,sp@-
           movel sp@(16+4),a0
           movel sp@(16+8),a1
           movew sp@(16+12+2),d0
           movew sp@(16+16+2),d1
           movel sp@(16+20),d2
           moveq #-1,d5
           lsrl d1,d5
           | a0 = ptr, d0.W = count, d1.W = i, d5.L = 2^(32-i)-1
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           bras 2f
    1:       | a0,a1 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i, d5.L = 2^(32-i)-1
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             movel a0@+,d3  | neue Daten
             rorl d1,d3      | um i Bits rechts rotieren
             movel d3,d4
             andl d5,d3      | untere 32-i Bits
             eorl d3,d4      | obere i Bits
             orl d2,d3       | und mit vorigem Übertrag kombinieren
             movel d4,d2     | neuer Übertrag
             movel d3,a1@+  | ablegen
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movel d2,d0
           moveml sp@+,d2-d5
           rts
#endif

| extern uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
_mulusmall_loop_down: # Input in d0,a0,d1.W,d2, Output in d0
           moveml d2-d4,sp@-
           movel sp@(12+4),d0
           movel sp@(12+8),a0
           movew sp@(12+12+2),d1
           movel sp@(12+16),d2
           addw #0,d1        | X-Bit löschen
           bras 2f
    1:       movel a0@-,d3  | nächstes Digit
             mulul d0,d4:d3  | mit digit multiplizieren
             addxl d2,d3     | und bisherigen Carry und X-Bit addieren
             movel d3,a0@   | Low-Digit ablegen
             movel d4,d2     | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           clrl d0
           addxl d2,d0       | letzter Carry (incl. X-Bit)
           moveml sp@+,d2-d4
           rts

| extern void mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
_mulu_loop_down: | Input in d0,a0,a1,d1.W
#if 1
           moveml d2-d4,sp@-
           movel sp@(12+4),d0
           movel sp@(12+8),a0
           movel sp@(12+12),a1
           movew sp@(12+16+2),d1
           subl d2,d2        | carry := 0, X-Bit löschen
           bras 2f
    1:       movel a0@-,d3  | nächstes Digit
             mulul d0,d4:d3  | mit digit multiplizieren
             addxl d2,d3     | und bisherigen Carry und X-Bit addieren
             movel d3,a1@-  | Low-Digit ablegen
             movel d4,d2     | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           clrl d3
           addxl d3,d2       | letztes X-Bit verarbeiten
           movel d2,a1@-    | letzten Carry ablegen
           moveml sp@+,d2-d4
           rts
#else
           moveml d2-d5,sp@-
           movel sp@(16+4),d0
           movel sp@(16+8),a0
           movel sp@(16+12),a1
           movew sp@(16+16+2),d1
           clrl d5           | 0
           clrl d2           | carry
           bras 2f
    1:       movel a0@-,d3  | nächstes Digit
             mulul d0,d4:d3  | mit digit multiplizieren
             addl d2,d3      | und bisherigen Carry addieren
             addxl d5,d4
             movel d3,a1@-  | Low-Digit ablegen
             movel d4,d2     | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           movel d2,a1@-    | letzten Carry ablegen
           moveml sp@+,d2-d5
           rts
#endif

| extern uintD muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
_muluadd_loop_down: | Input in d0,a0,a1,d1.W, Output in d0
           moveml d2-d5,sp@-
           movel sp@(16+4),d0
           movel sp@(16+8),a0
           movel sp@(16+12),a1
           movew sp@(16+16+2),d1
           clrl d5           | 0
           subl d2,d2        | carry := 0, X-Bit löschen
           bras 2f
    1:       movel a0@-,d3  | nächstes Digit
             mulul d0,d4:d3  | mit digit multiplizieren
             addxl d2,d3     | und bisherigen Carry und X-Bit addieren
             addxl d5,d4
             addl d3,a1@-   | Low-Digit zum dest-Digit addieren, X als Übertrag
             movel d4,d2     | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           addxl d5,d2       | letztes X-Bit addieren
           movel d2,d0       | letzten Carry als Ergebnis
           moveml sp@+,d2-d5
           rts

| extern uintD mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
_mulusub_loop_down: | Input in d0,a0,a1,d1.W, Output in d0
           moveml d2-d5,sp@-
           movel sp@(16+4),d0
           movel sp@(16+8),a0
           movel sp@(16+12),a1
           movew sp@(16+16+2),d1
           clrl d5           | 0
           subl d2,d2        | carry := 0, X-Bit löschen
           bras 2f
    1:       movel a0@-,d3  | nächstes Digit
             mulul d0,d4:d3  | mit digit multiplizieren
             addxl d2,d3     | und bisherigen Carry und X-Bit addieren
             addxl d5,d4
             subl d3,a1@-   | Low-Digit vom dest-Digit subtrahieren, X als Übertrag
             movel d4,d2     | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           clrl d0
           addxl d2,d0       | letzter Carry und letztes X-Bit
           moveml sp@+,d2-d5
           rts

| extern uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);
_divu_loop_up: # Input in d0,a0,d1.W, Output in d0
           moveml d2-d3,sp@-
           movel sp@(8+4),d0
           movel sp@(8+8),a0
           movew sp@(8+12+2),d1
           clrl d2           | Rest := 0
           bras 2f
    1:       movel a0@,d3   | nächst-niedriges Digit
             divull d0,d2:d3  | mit Rest kombinieren und durch digit dividieren
             movel d3,a0@+  | Quotient ablegen, Rest in d2
    2:       dbra d1,1b
           movel d2,d0       | Rest
           moveml sp@+,d2-d3
           rts

| extern uintD divucopy_loop_up (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
_divucopy_loop_up: # Input in d0,a0,a1,d1.W, Output in d0
           moveml d2-d3,sp@-
           movel sp@(8+4),d0
           movel sp@(8+8),a0
           movel sp@(8+12),a1
           movew sp@(8+16+2),d1
           clrl d2           | Rest := 0
           bras 2f
    1:       movel a0@+,d3  | nächst-niedriges Digit
             divull d0,d2:d3  | mit Rest kombinieren und durch digit dividieren
             movel d3,a1@+  | Quotient ablegen, Rest in d2
    2:       dbra d1,1b
           movel d2,d0       | Rest
           moveml sp@+,d2-d3
           rts

#endif

