# Externe Routinen zu ARILEV1.D
# Compiler: TURBO-C
# Parameter-Übergabe: in Registern A0-A1,D0-D2, Rest auf dem Stack.
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

           .xdef mulu32_,divu_6432_3232_
           .xdef copy_loop_up,copy_loop_down,fill_loop_up,fill_loop_down
           .xdef clear_loop_up,clear_loop_down
           .xdef or_loop_up,xor_loop_up,and_loop_up,eqv_loop_up
           .xdef nand_loop_up,nor_loop_up,andc2_loop_up,orc2_loop_up
           .xdef not_loop_up
           .xdef and_test_loop_up,test_loop_up,compare_loop_up
           .xdef add_loop_down,addto_loop_down,inc_loop_down
           .xdef sub_loop_down,subx_loop_down,subfrom_loop_down,dec_loop_down
           .xdef neg_loop_down
           .xdef shift1left_loop_down,shiftleft_loop_down,shiftleftcopy_loop_down
           .xdef shift1right_loop_up,shiftright_loop_up,shiftrightsigned_loop_up,shiftrightcopy_loop_up
           .xdef mulusmall_loop_down,mulu_loop_down,muluadd_loop_down,mulusub_loop_down
           .xdef divu_loop_up,divucopy_loop_up

; extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
; 2^32*hi+lo := arg1*arg2.
mulu32_:   ; Input in D0,D1, Output in D0,mulu32_high
           MULU.L D1,D1:D0
           MOVE.L D1,(mulu32_high) ; Adressierung?? Deklaration??
           RTS

; extern struct { uint32 q; uint32 r; } divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
; x = 2^32*xhi+xlo = q*y+r schreiben. Sei bekannt, daß 0 <= x < 2^32*y .
divu_6432_3232_: ; Input in D0,D1,D2, Output in D0,divu_32_rest
           DIVU.L D2,D0:D1 ; x = D0|D1 durch y dividieren
           MOVE.L D0,(divu_32_rest) ; Rest ablegen ; Adressierung?? Deklaration??
           MOVE.L D1,D0 ; Quotient als Ergebnis
           RTS

; extern uintD* copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
copy_loop_up: ; Input in A0,A1,D0.W, Output in A0
           BRA.S \2
    \1:      MOVE.L (A0)+,(A1)+
    \2:      DBF D0,\1
           MOVE.L A1,A0
           RTS

; extern uintD* copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
copy_loop_down: ; Input in A0,A1,D0.W, Output in A0
           BRA.S \2
    \1:      MOVE.L -(A0),-(A1)
    \2:      DBF D0,\1
           MOVE.L A1,A0
           RTS

; extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
fill_loop_up: ; Input in A0,D0.W,D1, Output in A0
           BRA.S \2
    \1:      MOVE.L D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
fill_loop_down: ; Input in A0,D0.W,D1, Output in A0
           BRA.S \2
    \1:      MOVE.L D1,-(A0)
    \2:      DBF D0,\1
           RTS

; extern uintD* clear_loop_up (uintD* destptr, uintC count);
clear_loop_up: ; Input in A0,D0.W, Output in A0
           BRA.S \2
    \1:      CLR.L (A0)+
    \2:      DBF D0,\1
           RTS

; extern uintD* clear_loop_down (uintD* destptr, uintC count);
clear_loop_down: ; Input in A0,D0.W, Output in A0
           BRA.S \2
    \1:      CLR.L -(A0)
    \2:      DBF D0,\1
           RTS

; extern void or_loop_up (uintD* xptr, uintD* yptr, uintC count);
or_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.L (A1)+,D1
             OR.L D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
xor_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.L (A1)+,D1
             EOR.L D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void and_loop_up (uintD* xptr, uintD* yptr, uintC count);
and_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.L (A1)+,D1
             AND.L D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
eqv_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.L (A1)+,D1
             EOR.L D1,(A0)
             NOT.L (A0)+
    \2:      DBF D0,\1
           RTS

; extern void nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
nand_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.L (A1)+,D1
             AND.L D1,(A0)
             NOT.L (A0)+
    \2:      DBF D0,\1
           RTS

; extern void nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
nor_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.L (A1)+,D1
             OR.L D1,(A0)
             NOT.L (A0)+
    \2:      DBF D0,\1
           RTS

; extern void andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
andc2_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.L (A1)+,D1
             NOT.L D1
             AND.L D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
orc2_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.L (A1)+,D1
             NOT.L D1
             OR.L D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void not_loop_up (uintD* xptr, uintC count);
not_loop_up: ; Input in A0,D0.W
           BRA.S \2
    \1:      NOT.L (A0)+
    \2:      DBF D0,\1
           RTS

; extern boolean and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
and_test_loop_up: ; Input in A0,A1,D0.W, verändert D1, Output in D0.W=D0.L
           BRA.S \2
    \1:      MOVE.L (A0)+,D1
             AND.L (A1)+,D1
             BNE.S \3
    \2:      DBF D0,\1
           CLR.L D0
           RTS
    \3:    MOVEQ.L #1,D0
           RTS

; extern boolean test_loop_up (uintD* ptr, uintC count);
test_loop_up: ; Input in A0,D0.W, Output in D0.W=D0.L
           BRA.S \2
    \1:      TST.L (A0)+
             BNE.S \3
    \2:      DBF D0,\1
           CLR.L D0
           RTS
    \3:    MOVEQ.L #1,D0
           RTS

; extern signean compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
compare_loop_up: ; Input in A0,A1,D0.W, Output in D0.W=D0.L
           BRA.S \2
    \1:      CMPM.L (A1)+,(A0)+
             BNE.S \3
    \2:      DBF D0,\1
           CLR.L D0
           RTS
    \3:    BLO.S \4
           MOVEQ.L #1,D0
           RTS
    \4:    MOVEQ.L #-1,D0
           RTS

; extern uintD add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
add_loop_down: ; Input in A0,A1,A2,D0.W, verändert D1,D2, Output in D0
           MOVE.L A2,-(SP)
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      MOVE.L -(A0),D1
             MOVE.L -(A1),D2
             ADDX.L D2,D1
             MOVE.L D1,-(A2)
    \2:      DBF D0,\1
           SUBX.L D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           MOVE.L (SP)+,A2
           RTS

; extern uintD addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
addto_loop_down: ; Input in A0,A1,D0.W, Output in D0
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      ADDX.L -(A0),-(A1)
    \2:      DBF D0,\1
           SUBX.L D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD inc_loop_down (uintD* ptr, uintC count);
inc_loop_down: ; Input in A0,D0.W, Output in D0
           BRA.S \2
    \1:      ADDQ.L #1,-(A0)
    \2:      DBCC D0,\1
           SUBX.L D0,D0       ; kein Carry -> D0.W=0, sonst D0.W=-1 für Übertrag
           RTS

; extern uintD sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
sub_loop_down: ; Input in A0,A1,A2,D0.W, verändert D1,D2, Output in D0
           MOVE.L A2,-(SP)
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      MOVE.L -(A0),D1
             MOVE.L -(A1),D2
             SUBX.L D2,D1
             MOVE.L D1,-(A2)
    \2:      DBF D0,\1
           SUBX.L D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           MOVE.L (SP)+,A2
           RTS

; extern uintD subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
subx_loop_down: ; Input in A0,A1,A2,D0.W,D1, verändert D2, Output in D0
           MOVE.L A2,-(SP)
           ROXR.L #1,D1       ; X-Bit initialisieren
           BRA.S \2
    \1:      MOVE.L -(A0),D1
             MOVE.L -(A1),D2
             SUBX.L D2,D1
             MOVE.L D1,-(A2)
    \2:      DBF D0,\1
           SUBX.L D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           MOVE.L (SP)+,A2
           RTS

; extern uintD subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
subfrom_loop_down: ; Input in A0,A1,D0.W, Output in D0
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      SUBX.L -(A0),-(A1)
    \2:      DBF D0,\1
           SUBX.L D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD dec_loop_down (uintD* ptr, uintC count);
dec_loop_down: ; Input in A0,D0.W, Output in D0
           BRA.S \2
    \1:      SUBQ.L #1,-(A0)
    \2:      DBCC D0,\1       ; kein Carry -> Schleife abbrechen
           SUBX.L D0,D0       ; kein Carry -> D0.W=0, sonst D0.W=-1 als Übertrag
           RTS

; extern uintD neg_loop_down (uintD* ptr, uintC count);
neg_loop_down: ; Input in A0,D0.W, Output in D0
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      NEGX.L -(A0)
    \2:      DBF D0,\1
           SUBX.L D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD shift1left_loop_down (uintD* ptr, uintC count);
shift1left_loop_down: ; Input in A0,D0.W, Output in D0.L
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      ROXL.W -(A0)     ; Digit -(A0) um 1 Bit links schieben, X-Bit als Buffer
             ROXL.W -(A0)
    \2:      DBF D0,\1
           SUBX.L D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
shiftleft_loop_down: ; Input in A0,D0.W,D1.W,D2, Output in D0
#if 1
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #32,D5
           SUB.W D1,D5
           ; A0 = ptr, D0.W = count, D1.W = i, D5.W = 32-i,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; D3.L = neues Digit
             MOVE.L D3,D4
             LSL.L D1,D4      ; um i Bits nach links schieben
             OR.L D2,D4       ; mit vorigem Übertrag kombinieren
             MOVE.L D4,(A0)   ; 32 Bits ablegen
             MOVE.L D3,D2
             LSR.L D5,D2      ; neuen Übertrag bilden
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#else
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #1,D5
           LSL.L D1,D5
           SUBQ.L #1,D5
           ; A0 = ptr, D0.W = count, D1.W = i, D5.L = 2^i-1
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; D3.L = neues Digit
             ROL.L D1,D3      ; um i Bits links rotieren
             MOVE.L D3,D4
             AND.L D5,D3      ; untere i Bits in D3
             EOR.L D3,D4      ; obere 32-i Bits in D4
             OR.L D2,D4       ; mit vorigem übertrag kombinieren
             MOVE.L D4,(A0)   ; 32 Bits ablegen
             MOVE.L D3,D2     ; neuer Übertrag
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#endif

; extern uintD shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
shiftleftcopy_loop_down: ; Input in A0,A1,D0.W,D1.W, Output in D0
#if 1
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #32,D5
           SUB.W D1,D5
           CLR.L D2
           ; A0 = sourceptr, A1 = destptr, D0.W = count, D1.W = i, D5.W = 32-i,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; D3.L = neues Digit
             MOVE.L D3,D4
             LSL.L D1,D4      ; um i Bits nach links schieben
             OR.L D2,D4       ; mit vorigem Übertrag kombinieren
             MOVE.L D4,-(A1)  ; 32 Bits ablegen
             MOVE.L D3,D2
             LSR.L D5,D2      ; neuen Übertrag bilden
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#else
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #1,D5
           LSL.L D1,D5
           SUBQ.L #1,D5
           ; A0 = sourceptr, A1 = destptr, D0.W = count, D1.W = i, D5.L = 2^i-1
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; D3.L = neues Digit
             ROL.L D1,D3      ; um i Bits links rotieren
             MOVE.L D3,D4
             AND.L D5,D3      ; untere i Bits in D3
             EOR.L D3,D4      ; obere 32-i Bits in D4
             OR.L D2,D4       ; mit vorigem übertrag kombinieren
             MOVE.L D4,-(A1)  ; 32 Bits ablegen
             MOVE.L D3,D2     ; neuer Übertrag
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#endif

; extern uintD shift1right_loop_up (uintD* ptr, uintC count, uintC carry);
shift1right_loop_up: ; Input in A0,D0.W,D1, Output in D0
           ROXR.L #1,D1       ; X-Bit löschen oder setzen, je nach D1
           BRA.S \2
    \1:      ROXR.W (A0)+     ; Digit (A0)+ um 1 Bit rechts schieben, X-Bit als Buffer
             ROXR.W (A0)+
    \2:      DBF D0,\1
           SUBX.L D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);
shiftright_loop_up: ; Input in A0,D0.W,D1.W, Output in D0
#if 1
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #32,D5
           SUB.W D1,D5
           ; A0 = ptr, D0.W = count, D1.W = i, D5.W = 32-i,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           CLR.L D2
           BRA.S \2
    \1:      ; A0 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i, D5.W = 32-i,
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             MOVE.L (A0),D3   ; neue Daten
             MOVE.L D3,D4
             LSR.L D1,D3      ; um i Bits rechts schieben
             OR.L D2,D3       ; und mit vorigem Übertrag kombinieren
             MOVE.L D3,(A0)+  ; ablegen
             LSL.L D5,D4      ; um (32-i) Bits links geschoben
             MOVE.L D4,D2     ; liefert neuen Übertrag
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#else
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #-1,D5
           LSR.L D1,D5
           ; A0 = ptr, D0.W = count, D1.W = i, D5.L = 2^(32-i)-1,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           CLR.L D2
           BRA.S \2
    \1:      ; A0 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i, D5.L = 2^(32-i)-1,
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             MOVE.L (A0),D3   ; neue Daten
             ROR.L D1,D3      ; um i Bits rechts rotieren
             MOVE.L D3,D4
             AND.L D5,D3      ; untere 32-i Bits
             EOR.L D3,D4      ; obere i Bits
             OR.L D2,D3       ; und mit vorigem Übertrag kombinieren
             MOVE.L D4,D2     ; neuer Übertrag
             MOVE.L D3,(A0)+  ; ablegen
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#endif

; extern uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
shiftrightsigned_loop_up: ; Input in A0,D0.W,D1.W, Output in D0
#if 1
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #32,D5
           SUB.W D1,D5
           ; A0 = ptr, D0.W = count, D1.W = i, D5.W = 32-i,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           SUBQ.W #1,D0
           MOVE.L (A0),D3     ; erstes Digit
           MOVE.L D3,D4
           ASR.L D1,D3        ; um i Bits rechts schieben
           BRA.S \2
    \1:      ; A0 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i, D5.W = 32-i,
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             MOVE.L (A0),D3   ; neue Daten
             MOVE.L D3,D4
             LSR.L D1,D3      ; um i Bits rechts schieben
             OR.L D2,D3       ; und mit vorigem Übertrag kombinieren
    \2:      MOVE.L D3,(A0)+  ; ablegen
             LSL.L D5,D4      ; um (32-i) Bits links geschoben
             MOVE.L D4,D2     ; liefert neuen Übertrag
             DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#else
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #-1,D5
           LSR.L D1,D5
           ; A0 = ptr, D0.W = count, D1.W = i, D5.L = 2^(32-i)-1,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           SUBQ.W #1,D0
           MOVE.L (A0),D3     ; erstes Digit
           MOVE.L D3,D4
           ROR.L D1,D4        ; um i Bits rechts rotieren
           MOVE.L D5,D2
           NOT.L D2
           AND.L D4,D2        ; obere 32-i Bits
           ASR.L D1,D3        ; erstes Digit um i Bits rechts shiften
           BRA.S \2
    \1:      ; A0 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i, D5.L = 2^(32-i)-1,
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             MOVE.L (A0),D3   ; neue Daten
             ROR.L D1,D3      ; um i Bits rechts rotieren
             MOVE.L D3,D4
             AND.L D5,D3      ; untere 32-i Bits
             EOR.L D3,D4      ; obere i Bits
             OR.L D2,D3       ; und mit vorigem Übertrag kombinieren
             MOVE.L D4,D2     ; neuer Übertrag
    \2:      MOVE.L D3,(A0)+  ; ablegen
             DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#endif

; extern uintD shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
shiftrightcopy_loop_up: ; Input in A0,A1,D0.W,D1.W,D2, Output in D0
#if 1
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #32,D5
           SUB.W D1,D5
           ; A0 = ptr, D0.W = count, D1.W = i, D5.W = 32-i
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           BRA.S \2
    \1:      ; A0,A1 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i, D5.W = 32-i
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             MOVE.L (A0)+,D3  ; neue Daten
             MOVE.L D3,D4
             LSR.L D1,D3      ; um i Bits rechts schieben
             OR.L D2,D3       ; und mit vorigem Übertrag kombinieren
             MOVE.L D3,(A1)+  ; ablegen
             LSL.L D5,D4      ; um (32-i) Bits links geschoben
             MOVE.L D4,D2     ; liefert neuen Übertrag
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#else
           MOVEM.L D3-D5,-(SP)
           MOVEQ.L #-1,D5
           LSR.L D1,D5
           ; A0 = ptr, D0.W = count, D1.W = i, D5.L = 2^(32-i)-1
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           BRA.S \2
    \1:      ; A0,A1 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i, D5.L = 2^(32-i)-1
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             MOVE.L (A0)+,D3  ; neue Daten
             ROR.L D1,D3      ; um i Bits rechts rotieren
             MOVE.L D3,D4
             AND.L D5,D3      ; untere 32-i Bits
             EOR.L D3,D4      ; obere i Bits
             OR.L D2,D3       ; und mit vorigem Übertrag kombinieren
             MOVE.L D4,D2     ; neuer Übertrag
             MOVE.L D3,(A1)+  ; ablegen
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.L D2,D0
           MOVEM.L (SP)+,D3-D5
           RTS
#endif

; extern uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
mulusmall_loop_down: # Input in D0,A0,D1.W,D2, Output in D0
           MOVEM.L D3-D4,-(SP)
           ADD.W #0,D1        ; X-Bit löschen
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; nächstes Digit
             MULU.L D0,D4:D3  ; mit digit multiplizieren
             ADDX.L D2,D3     ; und bisherigen Carry und X-Bit addieren
             MOVE.L D3,(A0)   ; Low-Digit ablegen
             MOVE.L D4,D2     ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           CLR.L D0
           ADDX.L D2,D0       ; letzter Carry (incl. X-Bit)
           MOVEM.L (SP)+,D3-D4
           RTS

; extern void mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
mulu_loop_down: ; Input in D0,A0,A1,D1.W
#if 1
           MOVEM.L D3-D4,-(SP)
           SUB.L D2,D2        ; carry := 0, X-Bit löschen
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; nächstes Digit
             MULU.L D0,D4:D3  ; mit digit multiplizieren
             ADDX.L D2,D3     ; und bisherigen Carry und X-Bit addieren
             MOVE.L D3,-(A1)  ; Low-Digit ablegen
             MOVE.L D4,D2     ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           CLR.L D3
           ADDX.L D3,D2       ; letztes X-Bit verarbeiten
           MOVE.L D2,-(A1)    ; letzten Carry ablegen
           MOVEM.L (SP)+,D3-D4
           RTS
#else
           MOVEM.L D3-D5,-(SP)
           CLR.L D5           ; 0
           CLR.L D2           ; carry
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; nächstes Digit
             MULU.L D0,D4:D3  ; mit digit multiplizieren
             ADD.L D2,D3      ; und bisherigen Carry addieren
             ADDX.L D5,D4
             MOVE.L D3,-(A1)  ; Low-Digit ablegen
             MOVE.L D4,D2     ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           MOVE.L D2,-(A1)    ; letzten Carry ablegen
           MOVEM.L (SP)+,D3-D5
           RTS
#endif

; extern uintD muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
muluadd_loop_down: ; Input in D0,A0,A1,D1.W, Output in D0
           MOVEM.L D3-D5,-(SP)
           CLR.L D5           ; 0
           SUB.L D2,D2        ; carry := 0, X-Bit löschen
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; nächstes Digit
             MULU.L D0,D4:D3  ; mit digit multiplizieren
             ADDX.L D2,D3     ; und bisherigen Carry und X-Bit addieren
             ADDX.L D5,D4
             ADD.L D3,-(A1)   ; Low-Digit zum dest-Digit addieren, X als Übertrag
             MOVE.L D4,D2     ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           ADDX.L D5,D2       ; letztes X-Bit addieren
           MOVE.L D2,D0       ; letzten Carry als Ergebnis
           MOVEM.L (SP)+,D3-D5
           RTS

; extern uintD mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
mulusub_loop_down: ; Input in D0,A0,A1,D1.W, Output in D0
           MOVEM.L D3-D5,-(SP)
           CLR.L D5           ; 0
           SUB.L D2,D2        ; carry := 0, X-Bit löschen
           BRA.S \2
    \1:      MOVE.L -(A0),D3  ; nächstes Digit
             MULU.L D0,D4:D3  ; mit digit multiplizieren
             ADDX.L D2,D3     ; und bisherigen Carry und X-Bit addieren
             ADDX.L D5,D4
             SUB.L D3,-(A1)   ; Low-Digit vom dest-Digit subtrahieren, X als Übertrag
             MOVE.L D4,D2     ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           CLR.L D0
           ADDX.L D2,D0       ; letzter Carry und letztes X-Bit
           MOVEM.L (SP)+,D3-D5
           RTS

; extern uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);
divu_loop_up: # Input in D0,A0,D1.W, Output in D0
           MOVE.L D3,-(SP)
           CLR.L D2           ; Rest := 0
           BRA.S \2
    \1:      MOVE.L (A0),D3   ; nächst-niedriges Digit
             DIVU.L D0,D2:D3  ; mit Rest kombinieren und durch digit dividieren
             MOVE.L D3,(A0)+  ; Quotient ablegen, Rest in D2
    \2:      DBF D1,\1
           MOVE.L D2,D0       ; Rest
           MOVE.L (SP)+,D3
           RTS

; extern uintD divucopy_loop_up (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
divucopy_loop_up: # Input in D0,A0,A1,D1.W, Output in D0
           MOVE.L D3,-(SP)
           CLR.L D2           ; Rest := 0
           BRA.S \2
    \1:      MOVE.L (A0)+,D3  ; nächst-niedriges Digit
             DIVU.L D0,D2:D3  ; mit Rest kombinieren und durch digit dividieren
             MOVE.L D3,(A1)+  ; Quotient ablegen, Rest in D2
    \2:      DBF D1,\1
           MOVE.L D2,D0       ; Rest
           MOVE.L (SP)+,D3
           RTS

          .end

#endif

