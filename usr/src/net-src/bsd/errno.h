/* make sure EWOULDBLOCK doesn't screw us up */

#include_next <errno.h>
#undef EWOULDBLOCK
#define EWOULDBLOCK	EAGAIN

