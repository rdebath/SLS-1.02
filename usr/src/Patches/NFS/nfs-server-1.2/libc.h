/* libc.h - prototypes for system functions - rick sladkey */

#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>

extern int h_errno;

#ifndef MAXNAMLEN
#define MAXNAMLEN	NAME_MAX
#endif

