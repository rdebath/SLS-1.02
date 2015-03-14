/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
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

#include "../../include/conf.h"

main(argc, argv)
     int argc;
     char *argv[];
{
    int c;
    int errflg = 0;
    extern char *optarg;
    extern int optind;
    
    char cmd[2048];
    char args[2048];
    char *font = "Courier-Bold12";

    while ((c = getopt(argc, argv, "cp:r:s:S:mu:h:f:")) != -1)
      switch (c) {
	case 'c':
	case 'm':
	  sprintf(&args[strlen(args)], "-%c ", c);
	  break;
	case 'p':
	case 'r':
	case 's':
	case 'S':
	case 'u':
	case 'h':
	  sprintf(&args[strlen(args)], "-%c \"%s\" ", c, optarg);
	  break;
	case 'f':
	  font = optarg;
	  break;
	case '?':
	  errflg++;;
	  break;
      }

    if (errflg || argc < 2) {
	fprintf(stderr, "usage: %s -p phone [filenames]\n", argv[0]);
	fprintf(stderr, "\tone or more -p argument may be given\n");
	fprintf(stderr, "\tif no filenames are given, input is from stdin\n");
	fprintf(stderr, "optional args:\n");
	fprintf(stderr, "\t-m will send email notification on delivery\n");
	fprintf(stderr, "\t-h <host> uses the spooler on the given host\n");
	fprintf(stderr, "\t-u <user> spools job with the given user name\n");
	fprintf(stderr, "\t-c generates a coverpage\n");
	fprintf(stderr, "\t-s <name> specifies the coverpage sender\n");
	fprintf(stderr, "\t-S <phone> specifies the coverpage return fax\n");
	fprintf(stderr, "\t-r <name> specifies the coverpage recipient\n");
	exit(1);
    }

    if (optind == argc) {
	/*
	 * Text from standard in.
	 */
	chdir("/tmp");
	sprintf(cmd, "/usr/local/bin/enscript -f%s -p - | %s %s", 
		font, FAX_PS_PROG, args);
    } else {
	/*
	 * Text from some files.
	 */
	char files[2048];
	strcpy(files, "");
	for (; optind < argc; optind++) {
	    strcat(files, argv[optind]);
	    strcat(files, " ");
	}
	sprintf(cmd, "/usr/local/bin/enscript -f%s -p - %s | %s %s", 
		font, files, FAX_PS_PROG, args);
    }

    exit(system(cmd));
}
