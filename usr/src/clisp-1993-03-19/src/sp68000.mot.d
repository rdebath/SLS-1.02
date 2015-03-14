; Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

           .text

           .xdef getSP
           .xdef setSP

;    extern void* getSP (void);
getSP:     LEA 4(SP),A0    ; aktueller Wert von SP + 4 wegen Unterprogrammaufruf
           RTS             ; in A0 = Adressen-Ergebnisregister von TURBO-C

;    extern void setSP (void* sp_init_address);
setSP:     MOVE.L (SP)+,A1 ; Returnadresse nach A1
           MOVE.L A0,SP    ; SP auf den übergebenen Wert setzen
           JMP (A1)        ; zurückspringen

           .end

