#include_next <netinet/in.h>

/* not implemented yet */

#undef IP_OPTIONS

/* concession to stupidity */

#define sin_zero __pad

