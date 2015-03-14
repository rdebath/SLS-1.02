# Aus der Speicherverwaltung ausgelagert:
# Tabelle aller festen Objekte
# Bruno Haible 7.11.1992

#include "lispbibl.c"

#undef LISPOBJ

# Tabelle aller festen Objekte:
  global struct object_tab_ object_tab
    #if defined(INIT_OBJECT_TAB) && NIL_IS_CONSTANT
    = {
        #define LISPOBJ  LISPOBJ_C
        #include "constobj.c"
        #undef LISPOBJ
      }
    #endif
    ;

