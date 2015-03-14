#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>

void *malloc();
void *realloc();
void free();

#ifndef O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif

#define SEEK_SET 0
#define SEEK_CUR 1

/* This is a hack.  This is only a hack.  Were this a common source file,
   rather than a config file specific to BSD on HP 68k's, you would have
   been instructed to clean this up.  As it is, clean it up if FSF's 
   HP's-running-ancient-BSD ever go away.  */
#ifdef	EPROCUNAVAIL
#include <machine/param.h>		/* BSD 4.4 alpha or better */
#include <stdlib.h>			/* Get this too, for abort(). */
#define	NO_CORE_COMMAND			/* Core files don't have command name */
#else
#include <machine/machparam.h>		/* BSD 4.3 or worse */
#endif

#define	HOST_PAGE_SIZE		NBPG
#define	HOST_SEGMENT_SIZE	NBPG	/* Data seg start addr rounds to NBPG */
#define	HOST_MACHINE_ARCH	bfd_arch_m68k
/* #define	HOST_MACHINE_MACHINE	 */

#define	HOST_TEXT_START_ADDR		0
#define	HOST_STACK_END_ADDR		0xfff00000
#define	HOST_BIG_ENDIAN_P

/* EXACT TYPES */
typedef char int8e_type;
typedef unsigned char uint8e_type;
typedef short int16e_type;
typedef unsigned short uint16e_type;
typedef int int32e_type;
typedef unsigned int uint32e_type;

/* CORRECT SIZE OR GREATER */
typedef char int8_type;
typedef unsigned char uint8_type;
typedef short int16_type;
typedef unsigned short uint16_type;
typedef int int32_type;
typedef unsigned int uint32_type;
#include "fopen-same.h"
