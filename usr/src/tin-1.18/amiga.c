/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : amiga.c
 *  Author    : M.Tomlinson & I.Lea
 *  Created   : 01-04-91
 *  Updated   : 04-12-92
 *  Notes     : Extra functions for Amiga port
 *  Copyright : (c) Copyright 1991-92 by Mark Tomlinson & Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#ifdef AMIGA

#include	"tin.h"
#include	<exec/libraries.h>
#include	<libraries/dos.h>
#include	<clib/dos_protos.h>
#include	<pragmas/dos_lib.h>
#include	<ctype.h>
#include	<fcntl.h>

extern struct DosLibrary *DOSBase;

int optind = 1;
char *optarg;

/* 
 * dummy
 */

chmod ()
{
}

/* 
 * stub for tputs 
 */

#ifndef INDEX_DAEMON
tputs (str, zzz, func)
	register char *str;
	int zzz;
	void (*func)(int);
{
	if (! str) {
		return;
	}
	
	while (*str) {
		if (*str == '\n') 
			func('\r'); 
		func(*str++);
	}
}
#endif

/*
 * stub for tzset
 */

void tzset (void)
{
}
 
/* 
 * joinpath tacks a file (or sub dir) on to the end of a directory name.
 * Not just as simple as putting a '/' between the two, as the directory
 * name may be an assign! 
 */

joinpath (str,dir,file)
	char *str; 
	char *dir; 
	char *file;
{	
	char c;

	if (strlen (dir) == 0) {
		strcpy (str, file);
		return;
	}
	c = dir[strlen(dir)-1];
	if (c=='/' || c==':') {
		sprintf (str, "%s%s", dir, file);
	} else {
		sprintf (str, "%s/%s", dir, file);
	}	
}


sleep (seconds)
	int seconds;
{
	Delay (50*seconds);
}

/* 
 * I'm not really sure how well popen and pclose work, but they seem OK 
 */

FILE *popen (command, mode)
	char *command; 
	char *mode;
{
	char cmd[256];

	if (mode[0] == 'w') {
		sprintf (cmd, "run %s <PIPE:", command);
		Execute (cmd, 0L, 0L);
		return fopen ("PIPE:", mode);
	} else {
		FILE *rp;
		rp = fopen ("PIPE:", mode);
		sprintf (cmd,"run %s >PIPE:",command);
		Execute (cmd, 0L, 0L);
		return rp;
	}
}


pclose (pipe)
	FILE *pipe;
{	
	fclose (pipe);
}

/* 
 * Directory stuff 
 */

DIR *opendir (name)
	char *name;
{
	DIR *di;

	di = calloc (1, sizeof (DIR));
	di->Lock = Lock (name,ACCESS_READ);
	if (di->Lock == 0) {
		free(di); 
		return 0;
	}
	if (Examine(di->Lock,&di->fib)==0) {
		UnLock(di->Lock); 
		free(di); 
		return 0;
	}
	return di;
}


struct dirent *readdir (di)
	DIR *di;
{
	static struct dirent de;

	if (ExNext (di->Lock, &di->fib) == 0) {
		return 0;
	}
	de.d_name = di->fib.fib_FileName;
	de.d_reclen = strlen (de.d_name);
	return &de;
}


void closedir (di)
	DIR *di;
{	
	UnLock (di->Lock);
	free (di);
}


char getopt (argc, argv, options)
	int argc;
	char *argv[];
	char *options;
{
	char c,*z;
	static int subind = 0;

	for (;optind < argc ; optind++) {	
		if (subind == 0) {
			c = argv[optind][0];
			if (c != '-') {
				return EOF;
			}
			subind = 1;
		}

		c = argv[optind][subind];
		if (c != 0) {
			break;
		}
	}

	if (optind == argc) {
		return EOF;
	}

	/* get rid of funnies */
	if (c == ':' || c == '?') { 
		return '?'; 
	}

	if ((z = strchr (options,c)) == 0) {
		return '?';
	}

	if (z[1] == ':') {
		if (argv[optind][subind+1]) {
			optarg = &argv[optind][subind+1];
		} else {
			optarg = argv[++optind];
		}
		optind++;
		subind = 0;
		return c;
	}
	subind++;
	return c;
}


int system (str)
	const char *str;
{
	if (DOSBase->dl_lib.lib_Version >= 36) {
		return (System ((char *)str, 0L));
	} else {
		return (!Execute((char *)str, 0L, 0L));
	}
}

/* 
 * The stat call in Aztec C doesn't tell us if the entry is a directory
 * or not. This one does. You will have to change <stat.h> to define
 * ST_DIRECT though 
 */

int stat (name, buf)
	char *name;
	register struct stat *buf;
{
	BPTR dirlock;
	register struct FileInfoBlock *inf;

	if (! (dirlock = Lock (name, ACCESS_READ))) {
		return -1;
	}
	if (! (inf = malloc(sizeof(*inf)))) { 
		UnLock (dirlock); 
		return -1;
	}
	Examine (dirlock,inf);
	UnLock (dirlock);
	buf->st_attr = ((inf->fib_EntryType>0) ? ST_DIRECT : 0) 
			| (inf->fib_Protection & 0xf);
	buf->st_size = inf->fib_Size;
	buf->st_mtime = ((inf->fib_Date.ds_Days + 2922) * (24 * 60) + 
			inf->fib_Date.ds_Minute) * 60
			+ inf->fib_Date.ds_Tick / TICKS_PER_SECOND;
	free (inf);
	return 0;
}

/* 
 * This getenv and setenv will use the WB2.0 calls if you have the new
 * rom. If not, it resorts to looking in the ENV: directory. 
 */

char *getenv (name)
	register const char *name;
{
	register FILE *fp;
	register char *ptr;
	static char buf[256];
	static char value[256];

	/* 2.0 style? */
	if (DOSBase->dl_lib.lib_Version >= 36) {
		if (GetVar ((char *)name,value,256,0L) == -1) {
			return 0;
		}
	} else {
		if (strlen (name) > 252) {
			return 0;
		}
		strcpy (buf,"ENV:");
		strcpy (&buf[4],name);
		if (! (fp = fopen(buf,"r"))) {
			return 0;
		}
		for (ptr = value; (*ptr=getc(fp))!=EOF && ++ptr < &value[256];);
		fclose(fp);
		*ptr = 0;
	}
	return value;
}

/* 
 * Setenv isn't actually used by TIN 
 */
 
/*
setenv (name, value)
	const char *name;
	const char *value; 
{
	if (DOSBase->dl_lib.lib_Version >= 36) {
		SetVar ((char *)name,(char *)value,strlen(value)+1,GVF_LOCAL_ONLY);
	}
}
*/

#else

/*
 * The ';' is to satisfy a really picky Ansi compiler 
 */

;

#endif	/* AMIGA */

