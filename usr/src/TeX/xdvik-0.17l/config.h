/* config.h: master configuration file, included first by all compilable
   source files (not headers).  */

#ifndef CONFIG_H
#define CONFIG_H

/* System dependencies that are figured out by `configure'.  */
#include "c-auto.h"

/* `paths.h' is made by the Makefile from `paths.h.in'.  */
#include "paths.h"

/* ``Standard'' headers.  */
#include "c-std.h"

/* strchr vs. index, memcpy vs. bcopy, etc.  */
#include "c-memstr.h"

/* Error numbers and errno declaration.  */
#include "c-errno.h"

#include <setjmp.h>

/* Magic characters to the filesystem.  */
#include "c-pathch.h"

/* Maximum length of an entire pathname. */
#include "c-pathmx.h"

/* Maximum length of any component in a pathname.  */
#include "c-namemx.h"

/* Minima and maxima.  */
#include "c-minmax.h"

/* The arguments for fseek.  */
#include "c-seek.h"

/* How to open a binary file.  */
#include "c-fopen.h"

/* Macros to discard or keep prototypes.  */
#include "c-proto.h"


/* Some definitions of our own.  */
#include "xdvi.h"

#endif /* not CONFIG_H */
