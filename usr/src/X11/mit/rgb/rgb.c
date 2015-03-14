/* $Header: /home/x_cvs/mit/rgb/rgb.c,v 1.9 1992/09/17 13:41:45 dawes Exp $ */
/* Copyright 1985, Massachusetts Institute of Technology */

/* reads from standard input lines of the form:
	red green blue name
   where red/green/blue are decimal values, and inserts them in a database.
 */
#ifndef lint
static char *rcsid_rgb_c = "$XConsortium: rgb.c,v 11.14 91/06/30 16:38:56 rws Exp $";
#endif

#ifdef __386BSD__
#include <sys/types.h>
#endif

#ifdef NDBM
#ifdef SDBM
#include <sdbm.h>
#else
#include <ndbm.h>
#endif
#else
#if defined(SVR4) || defined(ISC)
#include <rpcsvc/dbm.h>
#else
#include <dbm.h>
#endif
#define dbm_open(name,flags,mode) (!dbminit(name))
#define dbm_store(db,key,content,flags) (store(key,content))
#define dbm_close(db) dbmclose()
#endif

#undef NULL
#include <stdio.h>
#include <X11/Xos.h>
#include "rgb.h"
#include "site.h"
#include <ctype.h>

#include <errno.h>
extern int errno;			/* some systems are still stupid */
extern int sys_nerr;
#ifndef linux
extern char *sys_errlist[];
#endif

char *ProgramName;

char *SysError ()
{
    return ((errno >= 0 && errno < sys_nerr) ? sys_errlist[errno] : "?");
}

main(argc, argv)
    int argc;
    char **argv;
{
    char *dbname;
    char line[512];
    int red, green, blue;
    RGB rgb;
    datum key, content;
    char name[512];
    int items;
    int lineno;
    int i, n;
    int fd;
#ifdef NDBM
    DBM *rgb_dbm;
#else
    int rgb_dbm;
#endif

    ProgramName = argv[0];

    if (argc == 2)
	dbname = argv[1];
    else
	dbname = RGB_DB;

#ifndef __386BSD__
    strcpy (name, dbname);
    strcat (name, ".dir");
    fd = open (name, O_WRONLY|O_CREAT, 0666);
    if (fd < 0) {
	fprintf (stderr, 
		 "%s:  unable to create dbm file \"%s\" (error %d, %s)\n",
		 ProgramName, name, errno, SysError());
	exit (1);
    }
    (void) close (fd);

    strcpy (name, dbname);
    strcat (name, ".pag");
    fd = open (name, O_WRONLY|O_CREAT, 0666);
    if (fd < 0) {
	fprintf (stderr, 
		 "%s:  unable to create dbm file \"%s\" (error %d, %s)\n",
		 ProgramName, name, errno, SysError());
	exit (1);
    }
    (void) close (fd);

    rgb_dbm = dbm_open (dbname, O_RDWR, 0666);
#else
    rgb_dbm = dbm_open (dbname, O_RDWR | O_CREAT, 0666);
#endif
    if (!rgb_dbm) {
	fprintf (stderr,
		 "%s:  unable to open dbm database \"%s\" (error %d, %s)\n",
		 ProgramName, dbname, errno, SysError());
	exit (1);
    }

    key.dptr = name;
    content.dptr = (char *) &rgb;
    content.dsize = sizeof (rgb);
    lineno = 0;
    while (fgets (line, sizeof (line), stdin)) {
	lineno++;
	items = sscanf (line, "%d %d %d %[^\n]\n", &red, &green, &blue, name);
	if (items != 4) {
	    fprintf (stderr, "syntax error on line %d\n", lineno);
	    fflush (stderr);
	    continue;
	}
	if (red < 0 || red > 0xff ||
	    green < 0 || green > 0xff ||
	    blue < 0 || blue > 0xff) {
	    fprintf (stderr, "value for %s out of range\n", name);
	    fflush (stderr);
	    continue;
	}
	n = strlen (name);
	for (i = 0; i < n; i++) {
	    if (isupper (name[i]))
		name[i] = tolower (name[i]);
	}
	key.dsize = n;
	rgb.red = (red * 65535) / 255;
	rgb.green = (green * 65535) / 255;
	rgb.blue = (blue * 65535) / 255;
	if (dbm_store (rgb_dbm, key, content, DBM_REPLACE)) {
	    fprintf (stderr, "%s:  store of entry \"%s\" failed\n",
		     ProgramName, name);
	    fflush (stderr);
	}
    }

    dbm_close(rgb_dbm);

    exit(0);
}
