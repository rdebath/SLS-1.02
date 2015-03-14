/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel. 
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <strings.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/types.h>

#include "../../lib/libutil/tcp.h"
#include "../../lib/libfax/libfax.h"

#define ESC	'\033'	/* escape	*/
#define FS	'\034'	/* end of page  */
#define GS	'\035'	/* end of file	*/

static int cp_fp(old, new)
     FILE *old;
     FILE *new;
{
    char buf[BUFSIZ];
    int bytes;
    
    while ((bytes = fread(buf, sizeof(char), sizeof(buf), old)) > 0) {
	int i;

	/*
	 * Write out the data, being careful with escapes.
	 */
	for (i = 0; i < bytes; i++) {
	    if (buf[i] == ESC)
	      fputc(ESC, new);
	    fputc(buf[i], new);
	}
    }

    return (0);
}

static int cp(old_file, new)
     char *old_file;
     FILE *new;
{
    FILE *old;
    int rc;

    if ((old = fopen(old_file, "r")) == NULL)
      return (-1);

    rc = cp_fp(old, new);
    
    fclose(old);

    return (rc);
}

main(argc,argv)
     int argc;
     char *argv[];
{
    int c;
    int errflg = 0;
    extern char *optarg;
    extern int optind;

    char *host = NULL;
    int mail_notification = FALSE;
    char *user_name = NULL;
    struct passwd *pwd;
    int phones = 0;
    int files = 0;
    int file;
    int ind;
    FILE *qf_fp;
    int fd;
    FILE *fp;

    while ((c = getopt(argc, argv, "mu:h:")) != -1)
      switch (c) {
	case 'm':
	  mail_notification = TRUE;
	  break;
	case 'u':
	  user_name = optarg;
	  break;
	case 'h':
	  host = optarg;
	  break;
	case '?':
	  errflg++;;
	  break;
      }

    /*
     * Count the files and phone numbers given.
     */
    for (ind = optind; ind < argc; ind++) {
	if (strcmp(argv[ind], "---") == 0) {
	    files = argc - ind - 1;
	    break;
	}
	phones++;
    }

    /*
     * Must have at least one fax phone.
     */
    if (errflg || phones == 0) {
	fprintf(stderr, "usage: %s: -m phone-list --- file-list\n", argv[0]);
	exit(1);
    }

    /*
     * Get the fax host:
     */
    if (host == NULL) {
	if ((host = getenv("FAXHOST")) == NULL)
	  host = FAX_HOST;
    }

    /*
     * Find out who we are.
     */
    if (user_name == NULL) {
	if ((pwd = getpwuid(getuid())) == NULL) {
	    fprintf(stderr, "can't figure out who I am\n");
	    exit(1);
	}
	user_name = pwd->pw_name;
    }

    /*
     * Connect to the fax spooler, and open an fp to the fd for
     * easy writing.
     */
    if ((fd = tcp_make_connection(host, FAX_SERVICE)) < 0) {
	fprintf(stderr, "can't connect to fax daemon\n");
	exit(1);
    }
    if ((fp = fdopen(fd, "w")) == NULL) {
	perror("fdopen");
	exit(1);
    }

    /*
     * Send down the command to start a new fax job.
     */
    fprintf(fp, "ENQUEUE\n");
    
    /*
     * Add the fields to the queue file.
     */
    fprintf(fp, "USER: %s\n", user_name);
    if (mail_notification)
      fprintf(fp, "EMAIL:\n");
    for (ind = optind; ind < optind+phones; ind++)
      fprintf(fp, "RECIP: 0 0 %s 0 0 0\n", argv[ind]);
    fprintf(fp, "DATA:\n");

    /*
     * Copy the G3 files to the fax spooler directory.
     */
    if (files > 0) {
	for (file = 0; file < files; file++) {
	    char *g3_in_name = argv[optind+phones+file+1];
	    char *colon = index(g3_in_name, ':');
	    /*
	     * OK, this is a bit of a hack.
	     */
	    if (colon == NULL) {
		fprintf(fp, "%c%c", ESC, FS);
		if (cp(g3_in_name, fp) < 0) {
		    fprintf(stderr, "can't open file %s\n", g3_in_name);
		    exit(2);
		}
	    } else {
		int page, pages;
		*colon = '\0';
		pages = atoi(colon+1);
		for (page = 1; page <= pages; page++) {
		    char g3file[MAXPATHLEN];
		    sprintf(g3file, "%s.%d", g3_in_name, page);
		    fprintf(fp, "%c%c", ESC, FS);
		    if (cp(g3file, fp) < 0) {
			fprintf(stderr, "can't open file %s\n", file);
			exit(2);
		    }
		}
	    }
	}
    } else {
	fprintf(fp, "%c%c", ESC, FS);
	if (cp_fp(stdin, fp) < 0) {
	    fprintf(stderr, "can't copy G3 file\n");
	    exit(2);
	}
    }

    /*
     * All done!
     */
    fprintf(fp, "%c%c", ESC, GS);
    fclose(fp);

    exit(0);
}
