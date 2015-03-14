
/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering 
 * teams as part of the course "Introduction to Software Engineering" 
 * under the supervision of Professor G. Davida.
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *     		soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations. Selling this software for profit without
 * the permission of the Board of Regents of the University
 * of Wisconsin is strictly forbidden. 
 *
 * Contact:	soft-eng@cs.uwm.edu
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *    		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 *
 *		HISTORY,CLAIMS and CONTRIBUTIONS
 */

/*
 * Encryption utilites
 * Bradley Williams	
 * {allegra,ihnp4,uiucdcs,ctvax}!convex!williams
 * $Revision: 6.1 $
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
/* #inculde <cursesX.h> */

#if defined(BSD42) || defined(BSD43)
#include <sys/file.h>
#else
#include <fcntl.h>
#endif

#include "sc.h"

char        *strcpy();

#ifdef SYSV3
void exit();
#endif

int         Crypt = 0;

creadfile (save, eraseflg)
char *save;
int  eraseflg;
{
    register FILE *f;
    int pipefd[2];
    int fildes;
    int pid;

    if (eraseflg && strcmp(save, curfile) && modcheck(" first")) return;

    fildes = open (save, O_RDONLY, 0);
    if (fildes < 0)
    {
	error ("Can't read file \"%s\"", save);
	return;
    }

    if (eraseflg) erasedb ();

    if (pipe(pipefd) < 0) {
	error("Can't make pipe to child");
	return;
    }

    /* deraw(); */
    if ((pid=fork()) == 0)			  /* if child  */
    {
	(void) close (0);		  /* close stdin */
	(void) close (1);		  /* close stdout */
	(void) close (pipefd[0]);	  /* close pipe input */
	(void) dup (fildes);		  /* standard in from file */
	(void) dup (pipefd[1]);		  /* connect to pipe */
	(void) fprintf (stderr, " ");
	(void) execl ("/bin/sh", "sh", "-c", "crypt", 0);
	exit (-127);
    }
    else				  /* else parent */
    {
	(void) close (fildes);
	(void) close (pipefd[1]);	  /* close pipe output */
	f = fdopen (pipefd[0], "r");
	if (f == 0)
	{
	    (void) kill (pid, -9);
	    error ("Can't fdopen file \"%s\"", save);
	    (void) close (pipefd[0]);
	    return;
	}
    }

    loading++;
    while (fgets(line,sizeof line,f)) {
	linelim = 0;
	if (line[0] != '#') (void) yyparse ();
    }
    --loading;
    (void) fclose (f);
    (void) close (pipefd[0]);
    while (pid != wait(&fildes)) /**/;
    /* goraw(); */
    linelim = -1;
    modflg++;
    if (eraseflg) {
	(void) strcpy (curfile, save);
	modflg = 0;
    }
    EvalAll();
}

cwritefile (fname, r0, c0, rn, cn)
char *fname;
int r0, c0, rn, cn;
{
    register FILE *f;
    int pipefd[2];
    int fildes;
    int pid;
    char save[1024];
    char *fn;


    if (*fname == 0) fname = &curfile[0];

    fn = fname;
    while (*fn && (*fn == ' '))  /* Skip leading blanks */
	fn++;

    if ( *fn == '|' ) {
	error ("Can't have encrypted pipe");
	return(-1);
	}

    (void) strcpy(save,fname);

    fildes = open (save, O_WRONLY|O_CREAT, 0600);
    if (fildes < 0)
    {
	error ("Can't create file \"%s\"", save);
	return(-1);
    }

    if (pipe (pipefd) < 0) {
	error ("Can't make pipe to child\n");
	return(-1);
    }

    /* deraw();*/
    if ((pid=fork()) == 0)			  /* if child  */
    {
	(void) close (0);			  /* close stdin */
	(void) close (1);			  /* close stdout */
	(void) close (pipefd[1]);		  /* close pipe output */
	(void) dup (pipefd[0]);			  /* connect to pipe input */
	(void) dup (fildes);			  /* standard out to file */
	(void) fprintf (stderr, " ");
	(void) execl ("/bin/sh", "sh", "-c", "crypt", 0);
	exit (-127);
    }
    else				  /* else parent */
    {
	(void) close (fildes);
	(void) close (pipefd[0]);		  /* close pipe input */
	f = fdopen (pipefd[1], "w");
	if (f == 0)
	{
	    (void) kill (pid, -9);
	    error ("Can't fdopen file \"%s\"", save);
	    (void) close (pipefd[1]);
	    return(-1);
	}
    }

    write_fd(f, r0, c0, rn, cn);

    (void) fclose (f);
    (void) close (pipefd[1]);
    while (pid != wait(&fildes)) /**/;
    (void) strcpy(curfile,save);

    modflg = 0;
    error ("File \"%s\" written", curfile);
    /* goraw(); */
    return(0);
}

