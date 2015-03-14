/* bsd.h - simplify porting BSD programs to Linux - rick sladkey */

/* make sure BSD features are enabled, i.e. __USE_BSD and _FAVOR_BSD */

#define _BSD_SOURCE
#include <features.h>

/* some BSD progs expect MIN and MAX to be defined */

#define MIN(a, b)	((a) < (b) ? (a) : (b))
#define MAX(a, b)	((a) > (b) ? (a) : (b))

/* make sure we get L_SET and L_INCR, which is in a different place */

#include <sys/file.h>

/* BSD has slight non-POSIX names (and meanings :-) for some things */

#define FAPPEND		O_APPEND 

/* some BSD net code uses these definitions for byte-order dependencies */

#define BYTE_ORDER	1234
#define LITTLE_ENDIAN	1234

/* ftp uses this, I don't remember exactly what is is for :-) */

#define NCARGS		1024

/* ftpd uses this as bits per byte, I don't know why it's called NBBY */

#define NBBY		8

/* gloss over slight differences between BSD direct and POSIX dirent */
  
#define d_namlen	d_reclen

