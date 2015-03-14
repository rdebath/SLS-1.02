# Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

  #if defined(__EMX__) || defined(__DJGCC__) || defined(linux)
    # GNU-Assembler
    #ifdef __STDC__
      #define C(entrypoint) _##entrypoint
    #else
      #define C(entrypoint) _/**/entrypoint
    #endif
  #else /* defined(sun) || ... */
    # SUN-Assembler oder Consensys-Assembler
    #define C(entrypoint) entrypoint
  #endif

        .text
        .align 2

        .globl C(getSP)

#    extern void* getSP (void);
C(getSP:)
        leal    4(%esp),%eax    # aktueller Wert von ESP + 4 wegen Unterprogrammaufruf
        ret

