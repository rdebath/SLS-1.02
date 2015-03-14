! Macros für Zugriff auf SP, Stack und globale Register
! Zur Verwendung als zusätzlicher Input an 'cc' auf SPARC.
! Für die Konventionen siehe INLINE(1).
! (Wenn "cc -O" funktionieren würde, müßte man dieses File allerdings
! stattdessen als Input an '/usr/lib/inline' verwenden, denn die Optimierungs-
! phase von "cc -O1" mißversteht die von uns benutzten globalen Register.)
! Bruno Haible 7.9.1991, 16.12.1992

! extern void* getSP (void);
        .inline _getSP,0
        mov %sp,%o0
        .end

! Zugriff auf globale Register g4,g5,g6,g7:
! extern long _get_g4(); extern long _set_g4 (long value);
! usw.
!       .inline __get_g4,0
!       mov %g4,%o0
!       .end
!       .inline __set_g4,1
!       mov %o0,%g4
!       .end
!       .inline __get_g5,0
!       mov %g5,%o0
!       .end
!       .inline __set_g5,1
!       mov %o0,%g5
!       .end
!       .inline __get_g6,0
!       mov %g6,%o0
!       .end
!       .inline __set_g6,1
!       mov %o0,%g6
!       .end
!       .inline __get_g7,0
!       mov %g7,%o0
!       .end
!       .inline __set_g7,1
!       mov %o0,%g7
!       .end

! Die globalen Register %g1..%g5 werden vom 'cc' (in dieser Reihenfolge,
! mit abnehmender Häufigkeit) als Scratch-Register verwendet.
! Uns bleiben nur %g6 und %g7.

! STACK: im Register %g7, es gilt STACK_UP.

! extern void* _getSTACK (void);
        .inline __getSTACK,0
        mov %g7,%o0
        .end

! extern void* _setSTACK (void* new_STACK);
        .inline __setSTACK,1
        mov %o0,%g7
        .end

! extern void _skipSTACK (uintL skipcount);
        .inline __skipSTACK,1
        sub %g7,%o0,%g7
        .end

! extern void pushSTACK (object obj);
        .inline _pushSTACK,1
        st %o0,[%g7]
        add %g7,4,%g7
        .end

! extern object popSTACK (void);
        .inline _popSTACK,0
        sub %g7,4,%g7
        ld [%g7],%o0
        .end

! Holen eines Multiplikations-Ergebnisses:
        .inline __get_g1,0
        mov %g1,%o0
        .end

