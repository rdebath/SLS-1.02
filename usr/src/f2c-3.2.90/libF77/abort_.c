#include "stdio.h"
#include "f2c.h"

extern VOID abort();

VOID abort_()
{
fprintf(stderr, "Fortran abort routine called\n");
_cleanup();
abort();
}
