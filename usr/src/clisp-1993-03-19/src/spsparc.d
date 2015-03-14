# Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

  # (diese werden VOR der vorigen Instruktion ausgeführt):
  #define _             # Instruktion, die stets ausgeführt wird
  #define __            # Instruktion, die nur im Sprung-Fall ausgeführt wird
  # Abkürzungen für Anweisungen:
  #define ret   jmp %i7+8    # return from subroutine
  #define retl  jmp %o7+8    # return from leaf subroutine (no save/restore)

        .seg "text"

        .global _getSP

#    extern void* getSP (void);
_getSP: retl
       _ mov %sp,%o0

