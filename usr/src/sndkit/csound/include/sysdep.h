#ifdef THINK_C                                                  /* SYSDEP.H */
#include <stdlib.h>
#define __STDC__
#define  SYS5
#define  SFDIGDES
#define  WINDOWS	/* with winmac.c */
#else
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#endif

#ifdef SYS5
#define  index(A,B) strchr(A,B)
#include <fcntl.h>
#include <string.h>
#else
#include <strings.h>
#endif
