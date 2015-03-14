# Externe Routinen zu ARILEV1.D
# Compiler: TURBO-C
# Parameter-Übergabe: in Registern A0-A1,D0-D2, Rest auf dem Stack.
# Einstellungen: intCsize=16, intDsize=16.

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

           .xdef mulu32_
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
           MOVE.L D3,A0
           MOVE.L D4,A1
           ; D0.L = 2^16*a+b, D1.L = 2^16*c+d -> Produkt
           ; (2^16*a+b)*(2^16*c+d) = 2^32*a*c + 2^16*(a*d+b*c) + b*d
           MOVE.L D0,D2
           SWAP D2      ; D2.W = a
           MOVE.L D1,D3
           SWAP D1      ; D1.W = c
           MOVE.L D1,D4
           MULU D2,D1   ; D1.L = a*c
           MULU D3,D2   ; D2.L = a*d
           MULU D0,D4   ; D4.L = b*c
           MULU D3,D0   ; D0.L = b*d
           CLR.L D3     ; Hilfsregister für Zero-Extend
           SWAP D2
           MOVE.W D2,D3
           ADD.L D3,D1  ; high16(a*d) zu D1.L addieren
           SWAP D4
           MOVE.W D4,D3
           ADD.L D3,D1  ; high16(b*c) zu D1.L addieren
           CLR.W D2
           ADD.L D2,D0  ; 2^16*low16(a*d) zu D0.L addieren
           BCC.S \1
           ADDQ.L #1,D1
    \1:    CLR.W D4
           ADD.L D4,D0  ; 2^16*low16(b*c) zu D0.L addieren
           BCC.S \2
           ADDQ.L #1,D1
    \2:    ; D0.L = lo, D1.L = hi fertig.
           MOVE.L D1,(mulu32_high) ; Adressierung?? Deklaration??
           MOVE.L A1,D4
           MOVE.L A0,D3
           RTS

; extern uintD* copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
copy_loop_up: ; Input in A0,A1,D0.W, Output in A0
           BRA.S \2
    \1:      MOVE.W (A0)+,(A1)+
    \2:      DBF D0,\1
           MOVE.L A1,A0
           RTS

; extern uintD* copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
copy_loop_down: ; Input in A0,A1,D0.W, Output in A0
           BRA.S \2
    \1:      MOVE.W -(A0),-(A1)
    \2:      DBF D0,\1
           MOVE.L A1,A0
           RTS

; extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
fill_loop_up: ; Input in A0,D0.W,D1.W, Output in A0
           BRA.S \2
    \1:      MOVE.W D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
fill_loop_down: ; Input in A0,D0.W,D1.W, Output in A0
           BRA.S \2
    \1:      MOVE.W D1,-(A0)
    \2:      DBF D0,\1
           RTS

; extern uintD* clear_loop_up (uintD* destptr, uintC count);
clear_loop_up: ; Input in A0,D0.W, Output in A0
           BRA.S \2
    \1:      CLR.W (A0)+
    \2:      DBF D0,\1
           RTS

; extern uintD* clear_loop_down (uintD* destptr, uintC count);
clear_loop_down: ; Input in A0,D0.W, Output in A0
           BRA.S \2
    \1:      CLR.W -(A0)
    \2:      DBF D0,\1
           RTS

; extern void or_loop_up (uintD* xptr, uintD* yptr, uintC count);
or_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.W (A1)+,D1
             OR.W D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
xor_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.W (A1)+,D1
             EOR.W D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void and_loop_up (uintD* xptr, uintD* yptr, uintC count);
and_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.W (A1)+,D1
             AND.W D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
eqv_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.W (A1)+,D1
             EOR.W D1,(A0)
             NOT.W (A0)+
    \2:      DBF D0,\1
           RTS

; extern void nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
nand_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.W (A1)+,D1
             AND.W D1,(A0)
             NOT.W (A0)+
    \2:      DBF D0,\1
           RTS

; extern void nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
nor_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.W (A1)+,D1
             OR.W D1,(A0)
             NOT.W (A0)+
    \2:      DBF D0,\1
           RTS

; extern void andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
andc2_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.W (A1)+,D1
             NOT.W D1
             AND.W D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
orc2_loop_up: ; Input in A0,A1,D0.W, verändert D1
           BRA.S \2
    \1:      MOVE.W (A1)+,D1
             NOT.W D1
             OR.W D1,(A0)+
    \2:      DBF D0,\1
           RTS

; extern void not_loop_up (uintD* xptr, uintC count);
not_loop_up: ; Input in A0,D0.W
           BRA.S \2
    \1:      NOT.W (A0)+
    \2:      DBF D0,\1
           RTS

; extern boolean and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
and_test_loop_up: ; Input in A0,A1,D0.W, verändert D1, Output in D0.W=D0.L
           BRA.S \2
    \1:      MOVE.W (A0)+,D1
             AND.W (A1)+,D1
             BNE.S \3
    \2:      DBF D0,\1
           CLR.L D0
           RTS
    \3:    MOVEQ.L #1,D0
           RTS

; extern boolean test_loop_up (uintD* ptr, uintC count);
test_loop_up: ; Input in A0,D0.W, Output in D0.W=D0.L
           BRA.S \2
    \1:      TST.W (A0)+
             BNE.S \3
    \2:      DBF D0,\1
           CLR.L D0
           RTS
    \3:    MOVEQ.L #1,D0
           RTS

; extern signean compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
compare_loop_up: ; Input in A0,A1,D0.W, Output in D0.W=D0.L
           BRA.S \2
    \1:      CMPM.W (A1)+,(A0)+
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
add_loop_down: ; Input in A0,A1,A2,D0.W, verändert D1,D2, Output in D0.W
           MOVE.L A2,-(SP)
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      MOVE.W -(A0),D1
             MOVE.W -(A1),D2
             ADDX.W D2,D1
             MOVE.W D1,-(A2)
    \2:      DBF D0,\1
           SUBX.W D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           MOVE.L (SP)+,A2
           RTS

; extern uintD addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
addto_loop_down: ; Input in A0,A1,D0.W, Output in D0.W
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      ADDX.W -(A0),-(A1)
    \2:      DBF D0,\1
           SUBX.W D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD inc_loop_down (uintD* ptr, uintC count);
inc_loop_down: ; Input in A0,D0.W, Output in D0.W
           BRA.S \2
    \1:      ADDQ.W #1,-(A0)
    \2:      DBCC D0,\1       ; kein Carry -> Schleife abbrechen
           SUBX.W D0,D0       ; kein Carry -> D0.W=0, sonst D0.W=-1 für Übertrag
           RTS

; extern uintD sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
sub_loop_down: ; Input in A0,A1,A2,D0.W, verändert D1,D2, Output in D0.W
           MOVE.L A2,-(SP)
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      MOVE.W -(A0),D1
             MOVE.W -(A1),D2
             SUBX.W D2,D1
             MOVE.W D1,-(A2)
    \2:      DBF D0,\1
           SUBX.W D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           MOVE.L (SP)+,A2
           RTS

; extern uintD subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
subx_loop_down: ; Input in A0,A1,A2,D0.W,D1.W, verändert D2, Output in D0.W
           MOVE.L A2,-(SP)
           ROXR.W #1,D1       ; X-Bit initialisieren
           BRA.S \2
    \1:      MOVE.W -(A0),D1
             MOVE.W -(A1),D2
             SUBX.W D2,D1
             MOVE.W D1,-(A2)
    \2:      DBF D0,\1
           SUBX.W D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           MOVE.L (SP)+,A2
           RTS

; extern uintD subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
subfrom_loop_down: ; Input in A0,A1,D0.W, Output in D0.W
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      SUBX.W -(A0),-(A1)
    \2:      DBF D0,\1
           SUBX.W D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD dec_loop_down (uintD* ptr, uintC count);
dec_loop_down: ; Input in A0,D0.W, Output in D0.W
           BRA.S \2
    \1:      SUBQ.W #1,-(A0)
    \2:      DBCC D0,\1       ; kein Carry -> Schleife abbrechen
           SUBX.W D0,D0       ; kein Carry -> D0.W=0, sonst D0.W=-1 als Übertrag
           RTS

; extern uintD neg_loop_down (uintD* ptr, uintC count);
neg_loop_down: ; Input in A0,D0.W, Output in D0.W
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      NEGX.W -(A0)
    \2:      DBF D0,\1
           SUBX.W D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD shift1left_loop_down (uintD* ptr, uintC count);
shift1left_loop_down: ; Input in A0,D0.W, Output in D0.W
           ANDI #%01110,CCR   ; X-Bit löschen
           BRA.S \2
    \1:      ROXL.W -(A0)     ; Digit -(A0) um 1 Bit links schieben, X-Bit als Buffer
    \2:      DBF D0,\1
           SUBX.W D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
shiftleft_loop_down: ; Input in A0,D0.W,D1.W,D2.W, Output in D0.W
           MOVE.L D3,-(SP)
           ; A0 = ptr, D0.W = count, D1.W = i,
           ; D2.W = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           BRA.S \2
    \1:      CLR.L D3
             MOVE.W -(A0),D3  ; D3.L = D3.W = neues Digit
             LSL.L D1,D3      ; um i Bits nach links schieben
             OR.W D2,D3       ; D3 enthält die letzten 16+i Bits
             MOVE.W D3,(A0)   ; 16 Bits ablegen
             SWAP D3
             MOVE.W D3,D2     ; neuen Übertrag bilden
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.W D2,D0
           MOVE.L (SP)+,D3
           RTS

; extern uintD shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
shiftleftcopy_loop_down: ; Input in A0,A1,D0.W,D1.W, Output in D0.W
           MOVE.L D3,-(SP)
           CLR.W D2
           ; A0 = sourceptr, A1 = destptr, D0.W = count, D1.W = i,
           ; D2.W = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           BRA.S \2
    \1:      CLR.L D3
             MOVE.W -(A0),D3  ; D3.L = D3.W = neues Digit
             LSL.L D1,D3      ; um i Bits nach links schieben
             OR.W D2,D3       ; D3 enthält die letzten 16+i Bits
             MOVE.W D3,-(A1)  ; 16 Bits ablegen
             SWAP D3
             MOVE.W D3,D2     ; neuen Übertrag bilden
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           MOVE.W D2,D0
           MOVE.L (SP)+,D3
           RTS

; extern uintD shift1right_loop_up (uintD* ptr, uintC count, uintC carry);
shift1right_loop_up: ; Input in A0,D0.W,D1.W, Output in D0.W
           ROXR.W #1,D1       ; X-Bit löschen oder setzen, je nach D1.W
           BRA.S \2
    \1:      ROXR.W (A0)+     ; Digit (A0)+ um 1 Bit rechts schieben, X-Bit als Buffer
    \2:      DBF D0,\1
           SUBX.W D0,D0       ; -1 falls X gesetzt, 0 falls X gelöscht
           RTS

; extern uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);
shiftright_loop_up: ; Input in A0,D0.W,D1.W, Output in D0.W
           MOVE.L D3,-(SP)
           ; A0 = ptr, D0.W = count, D1.W = i,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           CLR.L D2           ; Übertrag = 0
           BRA.S \2
    \1:      ; A0 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i,
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             CLR.L D3
             MOVE.W (A0),D3   ; neue Daten
             SWAP D3          ; nach Bit 31..16(D3), D3.W = 0
             LSR.L D1,D3      ; Bits 31-i..16-i(D3) sind die neuen Daten
             OR.L D3,D2       ; Bits 31..16-i(D3) sind die bisherigen Daten
             SWAP D2          ; untere 16 Bit ergeben neuen Übertrag,
             MOVE.W D2,(A0)+  ; obere 16 Bit werden abgespeichert
             CLR.W D2         ; D2.L = neuer Übertrag
    \2:      DBF D0,\1        ; Schleife D0.W mal durchlaufen
           SWAP D2
           MOVE.W D2,D0
           MOVE.L (SP)+,D3
           RTS

; extern uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
shiftrightsigned_loop_up: ; Input in A0,D0.W,D1.W, Output in D0.W
           MOVE.L D3,-(SP)
           ; A0 = ptr, D0.W = count, D1.W = i,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           MOVE.W (A0),D2     ; erstes Digit
           EXT.L D2           ; Vorzeichenbit nach Bit 31..16(D2)
           CLR.W D2           ; Rest von D2.L löschen
           LSR.L D1,D2        ; D2.W enthält in seinen oberen i Bits das Vorzeichen
           SWAP D2            ; Übertrag mit i Vorzeichenbits initialisiert
           BRA.S \2
    \1:      ; A0 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i,
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             CLR.L D3
             MOVE.W (A0),D3   ; neue Daten
             SWAP D3          ; nach Bit 31..16(D3), D3.W = 0
             LSR.L D1,D3      ; Bits 31-i..16-i(D3) sind die neuen Daten
             OR.L D3,D2       ; Bits 31..16-i(D3) sind die bisherigen Daten
             SWAP D2          ; untere 16 Bit ergeben neuen Übertrag,
             MOVE.W D2,(A0)+  ; obere 16 Bit werden abgespeichert
    \2:      CLR.W D2         ; D2.L = neuer Übertrag
             DBF D0,\1        ; Schleife D0.W mal durchlaufen
           SWAP D2
           MOVE.W D2,D0
           MOVE.L (SP)+,D3
           RTS

; extern uintD shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
shiftrightcopy_loop_up: ; Input in A0,A1,D0.W,D1.W,D2.W, Output in D0.W
           MOVE.L D3,-(SP)
           ; A0 = ptr, D0.W = count, D1.W = i,
           ; D2.L = Schiebe-Übertrag (i Bits), D3.L = Schiebe-Akku
           SWAP D2            ; carry nach D2.HW
           CLR.W D2           ; Rest von D2.L löschen
           LSR.L D1,D2        ; D2.W enthält in seinen oberen i Bits das Vorzeichen
           SWAP D2            ; Übertrag mit i Vorzeichenbits initialisiert
           BRA.S \2
    \1:      ; A0,A1 = Aufwärtszähler Adresse, D0.W = Herabzähler, D1.W = i,
             ; D2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             ; D3.L = Schiebe-Akku
             CLR.L D3
             MOVE.W (A0)+,D3  ; neue Daten
             SWAP D3          ; nach Bit 31..16(D3), D3.W = 0
             LSR.L D1,D3      ; Bits 31-i..16-i(D3) sind die neuen Daten
             OR.L D3,D2       ; Bits 31..16-i(D3) sind die bisherigen Daten
             SWAP D2          ; untere 16 Bit ergeben neuen Übertrag,
             MOVE.W D2,(A1)+  ; obere 16 Bit werden abgespeichert
    \2:      CLR.W D2         ; D2.L = neuer Übertrag
             DBF D0,\1        ; Schleife D0.W mal durchlaufen
           SWAP D2
           MOVE.W D2,D0
           MOVE.L (SP)+,D3
           RTS

; extern uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
mulusmall_loop_down: # Input in D0.W,A0,D1.W,D2.W, Output in D0.W
           MOVE.L D3,-(SP)
           EXT.L D2           ; carry
           BRA.S \2
    \1:      MOVE.W -(A0),D3  ; nächstes Digit
             MULU D0,D3       ; mit digit multiplizieren
             ADD.L D3,D2      ; und zum bisherigen Carry addieren. Kein Überlauf!
             MOVE.W D2,(A0)   ; Low-Digit ablegen
             CLR.W D2
             SWAP D2          ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           MOVE.W D2,D0       ; letzter Carry
           MOVE.L (SP)+,D3
           RTS

; extern void mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
mulu_loop_down: ; Input in D0.W,A0,A1,D1.W
           MOVE.L D3,-(SP)
           CLR.L D2           ; carry
           BRA.S \2
    \1:      MOVE.W -(A0),D3  ; nächstes Digit
             MULU D0,D3       ; mit digit multiplizieren
             ADD.L D3,D2      ; und zum bisherigen Carry addieren
             MOVE.W D2,-(A1)  ; Low-Digit ablegen
             CLR.W D2
             SWAP D2          ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           MOVE.W D2,-(A1)    ; letzten Carry ablegen
           MOVE.L (SP)+,D3
           RTS

; extern uintD muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
muluadd_loop_down: ; Input in D0.W,A0,A1,D1.W, benutzt D2,D3,D4, Output in D0.W
#if 1
           MOVE.L D3,-(SP)
           SUB.L D2,D2        ; carry := 0, X-Bit löschen, D2.HW stets =0
           BRA.S \2
    \1:      MOVE.W -(A0),D3  ; nächstes Digit
             MULU D0,D3       ; mit digit multiplizieren
             ADDX.L D2,D3     ; und bisherigen Carry und X-Bit addieren
             ADD.W D3,-(A1)   ; Low-Digit zum dest-Digit addieren, X als Übertrag
             SWAP D3
             MOVE.W D3,D2     ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           MOVE.W D2,D0       ; letzten Carry und
           SWAP D2            ; 0.W und
           ADDX.W D2,D0       ; letztes X-Bit addieren
           MOVE.L (SP)+,D3
           RTS
#else
           MOVEM.L D3-D4,-(SP)
           CLR.L D2           ; carry
           CLR.L D4           ; D4.HW stets =0
           BRA.S \2
    \1:      MOVE.W -(A0),D3  ; nächstes Digit
             MULU D0,D3       ; mit digit multiplizieren
             ADD.L D3,D2      ; und zum bisherigen Carry addieren
             MOVE.W -(A1),D4  ; nächstes dest-Digit
             ADD.L D4,D2      ; dazuaddieren
             MOVE.W D2,(A1)   ; Low-Digit ablegen
             CLR.W D2
             SWAP D2          ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           MOVE.W D2,D0       ; letzten Carry als Ergebnis
           MOVEM.L (SP)+,D3-D4
           RTS
#endif

#if 0
; extern void mulu_2loop_down (uintD* sourceptr1, uintC len1,
;                              uintD* sourceptr2, uintC len2,
;                              uintD* destptr, uintC len);
mulu2_loop_down: ; Input in A0,D0.W,A1,D1.W,4(SP),D2.W
           MOVEM.L A2-A5/D3-D5,-(SP)
           MOVE.L 32(SP),A2
           ; A0 = sourceptr1, D0.W = len1,
           ; A1 = sourceptr2, D1.W = len2,
           ; A2 = destptr, D2.W = len
           CMP.W D0,D1
           BHS.S \0
           EXG D1,D0
           EXG A1,A0
    \0:    ; jetzt ist len1<=len2
           ; erst D2.W+2 Nulldigits ablegen:
           MOVE.L A2,A3
           MOVE.W D2,D3
           BRA.S \2
    \1:      CLR.W -(A3)
    \2:      DBF D3,\1
           CLR.L -(A3)
           ; A3+4/D2.W/A2 ist die Ergebnis-UDS
           ; Unsigned multiplizieren:
           ; Überträge werden sofort nach dem Erzeugen im Speicher
           ; durchgeschoben. (Das geschieht recht selten - im Durchschnitt nur
           ; jedes vierte Mal - und dauert nicht lang,
           ; beschleunigt aber den inneren Schleifenkern enorm.)
           ; Unverändert bleibt A1 in source2.
           ; Äußere Schleife zählt mit D0.W, A0 in source1, A2 in dest.
           ; Innere Schleife zählt mit D3.W, A3 in source2, A4 in dest.
           ; Ganz innere Schleife zählt mit A5 in dest.
           ; D4, D5 temporäre Daten.
           BRA.S \7
    \3:      MOVE.W D1,D3
             MOVE.L A1,A3
             SUBQ.L #2,A2
             MOVE.L A2,A4
             MOVE.W -(A0),D4
             ; Innere Schleife:
             ; D4 = Multiplikator-Ziffer, D3.W mal wird
             ; jeweils -2(A3).W * D4 zu (A4).W addiert, Überträge darunter.
             ; Benutzt A3,D3,A4,D4, verändert D5,A5
             BRA.S \6
    \4:        MOVE.W D4,D5
               MULU -(A3),D5 ; D5.L := D5.W * -(A3).W
               SUBQ.L #2,A4 ; nächstes Zielwort ansteuern
               ADD.L D5,(A4) ; Wort zu 2(A4).W addieren, Übertrag zu (A4).W
               BCC.S \6
               ; Übertrag nach links propagieren:
               MOVE.L A4,A5
    \5:          ADD.W #1,-(A5)
                 BCS.S \5
    \6:        DBF D3,\4
             ; Innere Schleife beendet.
    \7:      DBF D0,\3
           MOVEM.L (SP)+,A2-A5/D3-D5
           RTS
#endif

; extern uintD mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
mulusub_loop_down: ; Input in D0.W,A0,A1,D1.W, benutzt D2,D3,D4, Output in D0.W
           MOVE.L D3,-(SP)
           SUB.L D2,D2        ; carry := 0, X-Bit löschen, D2.HW stets =0
           BRA.S \2
    \1:      MOVE.W -(A0),D3  ; nächstes Digit
             MULU D0,D3       ; mit digit multiplizieren
             ADDX.L D2,D3     ; und bisherigen Carry und X-Bit addieren
             SUB.W D3,-(A1)   ; Low-Digit vom dest-Digit subtrahieren, X als Übertrag
             SWAP D3
             MOVE.W D3,D2     ; High-Digit gibt neuen Carry
    \2:      DBF D1,\1
           CLR.W D0
           ADDX.W D2,D0       ; letzter Carry und letztes X-Bit
           MOVE.L (SP)+,D3
           RTS

; extern uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);
divu_loop_up: # Input in D0.W,A0,D1.W, Output in D0.W
           CLR.L D2           ; Rest D2.HW := 0
           BRA.S \2
    \1:      MOVE.W (A0),D2   ; nächst-niedriges Digit mit Rest kombinieren
             DIVU D0,D2       ; und durch digit dividieren
             MOVE.W D2,(A0)+  ; Quotient ablegen, Rest in D2.HW
    \2:      DBF D1,\1
           SWAP D2
           MOVE.W D2,D0       ; Rest
           RTS

; extern uintD divucopy_loop_up (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
divucopy_loop_up: # Input in D0.W,A0,A1,D1.W, Output in D0.W
           CLR.L D2           ; Rest D2.HW := 0
           BRA.S \2
    \1:      MOVE.W (A0)+,D2  ; nächst-niedriges Digit mit Rest kombinieren
             DIVU D0,D2       ; und durch digit dividieren
             MOVE.W D2,(A1)+  ; Quotient ablegen, Rest in D2.HW
    \2:      DBF D1,\1
           SWAP D2
           MOVE.W D2,D0       ; Rest
           RTS

           .end

#endif

