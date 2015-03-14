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
#include <unistd.h>
#include <fcntl.h>

#include "../../lib/libfax/libfax.h"

static int verbose = FALSE;

#define MAX_TRIES  3

#define EXIT_ERROR		1
#define EXIT_NO_FILE		2
#define EXIT_OPEN_FAILED	3
#define EXIT_CALL_FAILED	4
#define EXIT_SEND_FAILED	5
#define EXIT_FINISH_FAILED	6

static int do_send(fmp, fd, page, last_page)
     FaxModem *fmp;
     int fd;
     int page;
     int last_page;
{
    long start = time(0);

    if (faxmodem_send_page(fmp, fd, last_page, MAX_TRIES) < 0)
      return (-1);

    if (verbose)
      printf("page %d sent in %d seconds\n", page, time(0) - start);

    return (0);
}

main(argc, argv)
     int argc;
     char *argv[];
{
    int c;
    extern char *optarg;
    extern int optind;
    int errflg = 0;

    char *fax_device = FAX_DEVICE;
    char *phone;
    FaxModem fm;
    long start;

    log_set_level(LOG_WARNING);

    while ((c = getopt(argc, argv, "l:vf:")) != -1)
      switch (c) {
	case 'l':
	  log_set_level(atoi(optarg));
	  break;
	case 'v':
	  verbose = TRUE;
	  break;
	case 'f':
	  fax_device = optarg;
	  break;
	case '?':
	  errflg++;
	  break;
      }

    if (errflg || optind >= argc) {
	fprintf(stderr, "usage: %s phone [files..]\n", argv[0]);
	exit(EXIT_ERROR);
    }

    phone = argv[optind++];

    if (optind < argc) {
	int ind;
	for (ind = optind; ind < argc; ind++) {
	    if (access(argv[ind], R_OK) < 0) {
		fprintf(stderr, "can't access file: %s\n", argv[ind]);
		exit(EXIT_NO_FILE);
	    }
	}
    }

    if (faxmodem_open(&fm, fax_device) < 0) {
	fprintf(stderr, "open of fax failed\n");
	exit(EXIT_OPEN_FAILED);
    }

    if (faxmodem_initiate_call(&fm, phone) < 0 || !FAX_CONNECTED(&fm)) {
	fprintf(stderr, "call to %s failed\n", phone);
	exit(EXIT_CALL_FAILED);
    }

    start = time(0);

    if (verbose) {
	printf("connected to %s\n", phone);
	faxmodem_print_id_strings(&fm, stdout);
    }

    if (optind < argc) {
	int page = 1;
	for (; optind < argc; optind++) {
	    int fd;
	    if ((fd = open(argv[optind], O_RDONLY)) < 0) {
		fprintf(stderr, "can't access file: %s\n", argv[optind]);
		exit(EXIT_NO_FILE);
	    }
	    if (do_send(&fm, fd, page++, optind+1 == argc) < 0) {
		fprintf(stderr, "sending failed on: %s\n", argv[optind]);
		exit(EXIT_SEND_FAILED);
	    }
	}
    } else
      if (do_send(&fm, 0, 1, TRUE) < 0) {
	  fprintf(stderr, "sending failed stdin\n");
	  exit(EXIT_SEND_FAILED);
      }
      

    if (faxmodem_hangup(&fm) < 0) {
	fprintf(stderr, "hangup failed\n");
	exit(EXIT_FINISH_FAILED);
    }

    if (verbose)
      printf("total connect time was %d seconds\n", time(0) - start);

    if (faxmodem_close(&fm) < 0) {
	fprintf(stderr, "close failed\n");
	exit(EXIT_FINISH_FAILED);
    }

    exit(0);
}
