#ifdef MSDOS
#include <stdlib.h>
#include <io.h>
#endif /* MSDOS */

#if defined(USG) || defined(VMS)
#include <string.h>
#else /* not USG */
#ifdef MSDOS
#include <string.h>
#else
#include <strings.h>
#endif /* not MSDOS */
#endif /* not USG */
