# Hilfsdeklarationen für CLISP auf Atari ST mit GCC
# Bruno Haible 22.11.1992

# lineA globals for both inline and non-inline lineA bindings

#include <linea.h>


# Global vars:

# Pointer to line a parameter block returned by init
 __LINEA *__aline;

# Array of pointers to the three system font  headers
# returned by init (in register A1)
 __FONT  **__fonts;

# Array of pointers to the 16 line a functions returned
# by init (in register A2) only valid in ROM'ed TOS
 short  (**__funcs)();


# stab for gcc 2.0
void __main () {}

