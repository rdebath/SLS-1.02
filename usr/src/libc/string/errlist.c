#include <gnu-stabs.h>

#undef  sys_errlist
#undef  sys_nerr

symbol_alias (_sys_errlist, sys_errlist);
symbol_alias (_sys_nerr, sys_nerr);
