# Externe Routinen zu ARILEV1.D
# Prozessor: HPPA, wegen XMPYU nur auf HPPA 1.1 (etwa HP9000/720)
# Compiler: GNU-C oder HP-C
# Parameter-Übergabe: in Registern %arg0,%arg1,%arg2, Rückgabewert in %ret0.
# Einstellungen: intCsize=32, intDsize=32.

#ifdef INCLUDED_FROM_C

#else

# Großenteils abgeschrieben von hppa.s aus der PARI/GP-Distribution.

                .SHORTDATA
                .IMPORT $global$,DATA


#if 0 # brauchen wir nicht
                .CODE
                .EXPORT bfffo
# Liefert die Anzahl der führenden Nullbits von x.
bfffo           .PROC
                .CALLINFO
                .ENTER
                # if (x==0) goto L$0;
                COMB,=,N        %r0,%arg0,L$0
                # y = 31;
                LDI             31,%ret0
                # if (x & (bit(31-15)*(bit(16)-1)) == 0)
                EXTRU,<>        %arg0,15,16,%r0
                SHD,TR          %arg0,%r0,16,%arg0      # x = x<<(32-16); else
                ADDI            -16,%ret0,%ret0         # y = y-16;
                # if (x & (bit(31-7)*(bit(8)-1)) == 0)
                EXTRU,<>        %arg0,7,8,%r0
                SHD,TR          %arg0,%r0,24,%arg0      # x = x<<(32-24); else
                ADDI            -8,%ret0,%ret0          # y = y-8;
                # if (x & (bit(31-3)*(bit(4)-1)) == 0)
                EXTRU,<>        %arg0,3,4,%r0
                SHD,TR          %arg0,%r0,28,%arg0      # x = x<<(32-28); else
                ADDI            -4,%ret0,%ret0          # y = y-4;
                # if (x & (bit(31-1)*(bit(2)-1)) == 0)
                EXTRU,<>        %arg0,1,2,%r0
                SHD,TR          %arg0,%r0,30,%arg0      # x = x<<(32-30); else
                ADDI            -2,%ret0,%ret0          # y = y-2;
                # if (x & (bit(31-0)*(bit(1)-1)) != 0)
                EXTRU,=         %arg0,0,1,%r0
                ADDI            -1,%ret0,%ret0          # y = y-1;
                # goto L$1;
                B,N             L$1
L$0             LDI             32,%ret0
L$1             .LEAVE
                .PROCEND
#endif

                .CODE
                .EXPORT length32
# Liefert integer-size (>=1, <=32) des Arguments /=0.
length32        .PROC
                .CALLINFO
                .ENTER          # Input in %arg0, Output in %ret0
                # y = 1;
                LDI             1,%ret0
                # if (x & (bit(31-15)*(bit(16)-1)) == 0)
                EXTRU,<>        %arg0,15,16,%r0
                SHD,TR          %arg0,%r0,16,%arg0      # x = x<<(32-16); else
                ADDI            16,%ret0,%ret0          # y = y+16;
                # if (x & (bit(31-7)*(bit(8)-1)) == 0)
                EXTRU,<>        %arg0,7,8,%r0
                SHD,TR          %arg0,%r0,24,%arg0      # x = x<<(32-24); else
                ADDI            8,%ret0,%ret0           # y = y+8;
                # if (x & (bit(31-3)*(bit(4)-1)) == 0)
                EXTRU,<>        %arg0,3,4,%r0
                SHD,TR          %arg0,%r0,28,%arg0      # x = x<<(32-28); else
                ADDI            4,%ret0,%ret0           # y = y+4;
                # if (x & (bit(31-1)*(bit(2)-1)) == 0)
                EXTRU,<>        %arg0,1,2,%r0
                SHD,TR          %arg0,%r0,30,%arg0      # x = x<<(32-30); else
                ADDI            2,%ret0,%ret0           # y = y+2;
                # if (x & (bit(31-0)*(bit(1)-1)) != 0)
                EXTRU,=         %arg0,0,1,%r0
                ADDI            1,%ret0,%ret0           # y = y+1;
                .LEAVE
                .PROCEND


#ifndef __GNUC__ /* mit GNU-C machen wir mulu32() als Macro, der inline multipliziert */

                .SHORTDATA
                .EXPORT mulu32_high
                .ALIGN 8
mulu32_high     .WORD           # 8 Byte Platz
                .WORD

                .CODE
                .EXPORT mulu32_
# extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
# 2^32*hi+lo := arg1*arg2.
mulu32_         .PROC
                .CALLINFO
                .ENTER  # Input in %arg0,%arg1, Output in %ret0,mulu32_high
                LDIL    L'mulu32_high-$global$,%r1
                LDO     R'mulu32_high-$global$(%r1),%r1
                                                # %r1 = &x
                STW     %arg0,0(%r1)            # x abspeichern
                FLDWS   0(%r1),%fr4             # und in den Coprozessor laden
                STW     %arg1,0(%r1)            # y abspeichern
                FLDWS   0(%r1),%fr5             # und in den Coprozessor laden
                XMPYU   %fr4,%fr5,%fr6          # beides multiplizieren
                FSTDS   %fr6,0(%r1)             # Ergebnis (64 Bit) abspeichern
                LDWS    4(%r1),%ret0            # low 32 Bit als Ergebnis
                .LEAVE
                .PROCEND

#endif


#if 0 # Das funktioniert noch nicht.
      # Wenn das mal geht (insbes. UDS_to_DIGITS testen!), dann
      # in arilev0.d HPPA_DIV_WORKS -> HPPA ändern.

                .CODE
                .EXPORT divu_6432_3232_
                .IMPORT divu_32_rest,DATA
# extern struct { uint32 q; uint32 r; } divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
# x = 2^32*xhi+xlo = q*y+r schreiben. Sei bekannt, daß 0 <= x < 2^32*y .
divu_6432_3232_ .PROC
                .CALLINFO
                .ENTER  # Input in %arg0,%arg1,%arg2, Output in %ret0,divu_32_rest
# Registerbezeichnungen:
hirem           .REG    %arg0   # xhi
lorem           .REG    %arg1   # xlo
div             .REG    %arg2   # y bzw. y'
origdiv         .REG    %arg3
quo             .REG    %ret0   # q
temp1           .REG    %r20
temp2           .REG    %r21
temp3           .REG    %r22

# Divisions-Einzelschritt:
#               DS      hirem,div,hirem
# div sollte >0 und <2^31 sein.
#if 0 # so steht's dokumentiert
# Schiebt hirem um 1 Bit nach links, wobei der Carry rechts hineingeschoben
# wird. Dann wird div addiert bzw. - falls V gesetzt - (-div) addiert.
# Der Übertrag dieser Addition kommt sowohl in den Carry als auch ins V-Bit
# (ins V-Bit invertiert, falls div >= 2^31).
#else # so scheint's aber zu stimmen
# Schiebt hirem um 1 Bit nach links, wobei der Carry rechts hineingeschoben
# wird. Dann wird div subtrahiert bzw. - falls V gelöscht - (-div) subtrahiert.
# Der Übertrag dieser Subtraktion kommt sowohl in den Carry als auch
# invertiert ins V-Bit (nicht invertiert, falls div >= 2^31).
#endif

# Der Algorithmus ist wie der von meinem arisparc.s:_divu_3216_1616_ :
# Wenn man y zu Unrecht subtrahiert hat, so gleicht man dies dadurch aus,
# daß man nach dem nächsten 1-Bit-Shift - statt y zu subtrahieren -
# 2*y addiert und y subtrahiert, d.h. y addiert.

# 1 Divisions-Einzelschritt:
# hirem|lorem um 1 Bit nach links schieben und (falls V gesetzt)
# div = y subtrahieren bzw. (falls V gelöscht) div = y addieren.
DS1             .MACRO
                ADDC    lorem,lorem,lorem
                DS      hirem,div,hirem
                .ENDM

# 4 Divisions-Einzelschritte:
DS4             .MACRO
                DS1
                DS1
                DS1
                DS1
                .ENDM

# 32 Divisions-Einzelschritte:
DS32            .MACRO
                DS4
                DS4
                DS4
                DS4
                DS4
                DS4
                DS4
                DS4
                .ENDM

                COMB,<          div,0,L$50              # y>=0(signed), d.h. y < 2^31 ?
                # ja -> "kleine" Division
                SUB             0,div,temp1             # temp1 = -y > 2^31
                DS              0,temp1,0               # V-Bit setzen (erfolgreiche Subtraktion vortäuschen)
                DS32                                    # 32 mal hirem|lorem shiften,
                                                        # jeweils den Carry in lorem hineinshiften
                ADDC            lorem,lorem,lorem       # letzten Carry in lorem hineinshiften
                # Nun enthält hirem den Rest r oder r-y, lorem den Quotienten q.
                ADD,>=          0,hirem,0               # hirem < 0 (signed) ?
                ADD             hirem,div,hirem         # ja -> muß noch y addieren
                ADDIL           L'divu_32_rest-$global$,%dp
                STW             hirem,R'divu_32_rest-$global$(%r1)
                                                        # Rest r abspeichern
                COPY            lorem,quo               # Quotient q
                .LEAVE

L$50            # y >= 2^31. Reduktion durch Halbieren von x und y.
                COPY            div,origdiv             # y in origdiv retten
                EXTRU,<>        div,31,1,temp2          # temp2 := Bit 0 von y
                B               L$51
                # Division durch ungerade Zahl:
                # Schreibe y = 2*y'-1.
                # floor(x / (2*y'-1)) = floor(floor(x/2) / y') + (0 oder 1 oder 2)
                # da  0 <= x/(2*y'-1) - x/(2*y') = x/(2*y'-1) / (2*y') = x/y / (2*y')
                #       < 2^32 / (2*y') < 2^32/y <= 2 .
                EXTRU           div,30,31,div           # div := floor(y/2) = Bits 31..1 von div
                ADDB,NSV        temp2,div,L$52          # div := div+1 = y'
                EXTRU           lorem,31,1,temp3        # s.u. (delay slot)
                # Spezialfall: signed-Überlauf bei der Addition, d.h. y' = 2^31.
                # Division durch 2*y' ist hier besonders einfach:
                COPY            hirem,quo               # Quotient := hirem
                B               L$53
                COPY            lorem,hirem             # Rest := lorem

L$51            # Division durch gerades y.
                # x/2 durch y/2 dividieren, Quotient OK, Rest mit 2 multiplizieren, evtl. + 1.
                EXTRU           lorem,31,1,temp3        # temp3 := Bit 0 von xlo
L$52            SHD             hirem,lorem,1,lorem     # hirem|lorem um 1 Bit ...
                EXTRU           hirem,30,31,hirem       # ... nach rechts schieben
                # kleine Division (wie oben):
                SUB             0,div,temp1
                DS              0,temp1,0
                DS32
                ADDC            lorem,lorem,lorem
                ADD,>=          0,hirem,0
                ADD             hirem,div,hirem
                # Quotient in lorem fertig.
                COPY            lorem,quo               # Quotient q
                COMB,=          0,temp2,L$55            # war temp2=0, d.h. y gerade?
                SH1ADD          hirem,temp3,hirem         # ja -> nur noch hirem:=2*hirem+temp3 und fertig
L$53            # Es war y gerade, nun ist quo = floor(x / 2*y').
                # Quotient und Rest umrechnen:
                # x = quo * 2*y' + hirem = quo * (2*y'-1) + (quo+hirem)
                # Also Quotient = quo, Rest = quo+hirem.
                # Noch maximal 2 mal: Quotient += 1, Rest -= y.
                ADDB,NUV,N      quo,hirem,L$54          # Additions-Überlauf -> Quotient erhöhen
                                                        # kein delay-slot wegen ,N !
                SUB             hirem,origdiv,hirem     # Rest -= y
                ADDI            1,quo,quo               # Quotient += 1
L$54            # Wegen y>=2^31 muß der Quotient noch höchstens 1 mal erhöht werden:
                COMB,<<,N       hirem,origdiv,L$55      # hirem < y -> Quotient erhöhen
                                                        # kein delay-slot wegen ,N !
                SUB             hirem,origdiv,hirem     # Rest -= y
                ADDI            1,quo,quo               # Quotient += 1

L$55            ADDIL           L'divu_32_rest-$global$,%dp
                STW             hirem,R'divu_32_rest-$global$(%r1)
                                                        # Rest r abspeichern
                .LEAVE  
                .PROCEND

#endif


                .END

#endif
