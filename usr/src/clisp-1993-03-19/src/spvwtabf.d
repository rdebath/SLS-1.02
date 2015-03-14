# Aus der Speicherverwaltung ausgelagert:
# Tabelle aller FSUBRs und SUBRs
# Bruno Haible 27.2.1993

#include "lispbibl.c"

#undef LISPSPECFORM
#undef LISPFUN

#if defined(UNIX) || defined(DJUNIX) || defined(EMUNIX) || defined(VMS)
  # Ein kleines Shadowing-Problem. Grr...
  #undef read
#endif

# Tabelle aller FSUBRs:
  global struct fsubr_tab_ fsubr_tab_data
    #if defined(INIT_SUBR_TAB)
    = {
        #if NIL_IS_CONSTANT
          #define LISPSPECFORM  LISPSPECFORM_G
        #else
          #define LISPSPECFORM  LISPSPECFORM_F
        #endif
        #include "fsubr.c"
        #undef LISPSPECFORM
      }
    #endif
    ;

# Tabelle aller SUBRs:
  global struct subr_tab_ subr_tab_data
    #if defined(INIT_SUBR_TAB)
    = {
        #if NIL_IS_CONSTANT
          #define LISPFUN  LISPFUN_G
        #else
          #define LISPFUN  LISPFUN_F
        #endif
        #include "subr.c"
        #undef LISPFUN
      }
    #endif
    ;

