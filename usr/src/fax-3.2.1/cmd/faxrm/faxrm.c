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

#include "../../lib/libutil/tcp.h"
#include "../../lib/libfax/libfax.h"

main(argc,argv)
     int argc;
     char *argv[];
{
    int c;
    int errflg = 0;
    extern char *optarg;
    extern int optind;

    char *host = NULL;
    char *file;
    char buf[BUFSIZ];
    int bytes;
    FILE *fp;
    int fd;
      
    while ((c = getopt(argc, argv, "h:")) != -1)
      switch (c) {
	case 'h':
	  host = optarg;
	  break;
	case '?':
	  errflg++;;
	  break;
      }

    if (errflg || optind >= argc) {
	fprintf(stderr, "usage: %s [-h host] job\n", argv[0]);
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
     * Get the file to delete.
     */
    file = argv[optind];

    /*
     * Connect to the fax spooler.
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
     * Send down the delete request.
     */
    fprintf(fp, "DELETE\n");
    fprintf(fp, "%s\n", file);
    fflush(fp);

    while ((bytes = read(fd, buf, sizeof(buf))) > 0)
      fwrite(buf, sizeof(char), bytes, stdout);

    exit (0);
}
