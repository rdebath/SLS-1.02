/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : amiga.h
 *  Author    : M.Tomlinson & I.Lea
 *  Created   : 17-09-92
 *  Updated   : 24-11-92
 *  Notes     : Directory support for AmigaDOS
 *  Copyright : (c) Copyright 1991-92 by Mark Tomlinson & Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#ifdef AMIGA

#ifndef AMIGA_H
#define AMIGA_H

#include <dos/dos.h>

#define ST_DIRECT 0x10   /* Aztec's stat() doesn't give this information */

struct dirent { 
	char *d_name; 
	long d_reclen;
};

typedef struct
{
	BPTR	Lock;
	struct  FileInfoBlock fib;
	int	first;
} DIR;

DIR *opendir ();
struct dirent *readdir ();

#endif	/* AMIGA_H */

#endif	/* AMIGA */

