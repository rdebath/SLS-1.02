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
#include <fcntl.h>
#include <pwd.h>
#include <syslog.h>
#include <sys/ioctl.h>

#include "../../lib/libfax/libfax.h"

/*
 * Make a default for this:
 */
#ifndef INCOMING_EMAIL_ADDR
#define INCOMING_EMAIL_ADDR	NULL
#endif

static void
daemonize()
{
    int s;
    int pid;

    if ((pid = fork()) != 0) {
      if (pid < 0)
	  perror("start_daemon: fork");
      exit(0);
    }

    for (s = getdtablesize()-1; s >= 0; --s)
      close(s);

    open("/dev/null", O_RDONLY);
    dup2(0, 1);
    dup2(0, 2);
	
    /* disassociate server from controlling terminal */
    if ((s = open("/dev/tty", O_RDWR)) >= 0) {
	ioctl(s, TIOCNOTTY, 0);
	close(s);
    }
}

main(argc,argv)
     int argc;
     char *argv[];
{
    int c;
    int errflg = 0;
    extern char *optarg;
    extern int optind;

    char *fax_device = FAX_DEVICE;
    int be_daemon = FALSE;
    int max_delivery_time = MAX_DELIVERY_TIME;
    int min_retry_wait = MIN_RETRY_WAIT;
    char *out_qdir = OUTGOING_QUEUE;
    char *in_qdir = INCOMING_QUEUE;
    char *in_email = INCOMING_EMAIL_ADDR;
    int setuid_fax = TRUE;
    FaxModem fm[1];

    /*
     * This is a friendly umask.
     */
    umask(0002);

    /*
     * Start with a reasonable log level.
     */
    log_set_level(LOG_WARNING);

    while ((c = getopt(argc, argv, "l:f:d:r:DI:O:E:U")) != -1)
      switch (c) {
	case 'l':
	  log_set_level(atoi(optarg));
	  break;
	case 'f':
	  fax_device = optarg;
	  break;
	case 'd':
	  max_delivery_time = atoi(optarg);
	  break;
	case 'r':
	  min_retry_wait = atoi(optarg);
	  break;
	case 'D':
	  be_daemon = TRUE;
	  break;
	case 'I':
	  in_qdir = optarg;
	  break;
	case 'O':
	  out_qdir = optarg;
	  break;
	case 'E':
	  in_email = optarg;
	  break;
	case 'U':
	  setuid_fax = FALSE;
	  break;
	case '?':
	  errflg++;;
	  break;
      }

    if (errflg) {
	fprintf(stderr, "usage: %s\n", argv[0]);
	fprintf(stderr, "optional args:\n");
	fprintf(stderr, "\t-l <level> sets the log level\n");
	fprintf(stderr, "\t-f <device> sets the fax device\n");
	fprintf(stderr, "\t-d <time> sets the max delivery time\n");
	fprintf(stderr, "\t-r <time> sets the max retry period\n");
	fprintf(stderr, "\t-D runs the spooler as a daemon\n");	
	fprintf(stderr, "\t-I <dir> sets the incoming queue directory\n");
	fprintf(stderr, "\t-O <dir> sets the outgoing queue directory\n");
	fprintf(stderr, "\t-E <addr> sets the incoming email notice addr\n");
	fprintf(stderr, "\t-U don't setuid to user fax\n");
	exit(1);
    }

    if (be_daemon) {
	daemonize();
	log_enable_syslog(LOG_LOCAL0);
    }

    if (setuid_fax) {
	struct passwd *pwd;
	if ((pwd = getpwnam("fax")) != NULL) {
	    if (setgid(pwd->pw_gid) < 0)
	      log(L_ALERT, "can't setgid to fax: %m");
	    if (setuid(pwd->pw_uid) < 0)
	      log(L_ALERT, "can't setuid to fax: %m");
	} else
	  log(L_ALERT, "fax user unknown, running anyway");
    }
	    
    if (faxmodem_open(&fm[0], fax_device) < 0) {
	log(L_ALERT, "open of fax failed: %m");
	exit(2);
    }

    if (faxmodem_sync(&fm[0], FAXMODEM_SYNC_TRIES) < 0) {
	log(L_ALERT, "can't sync to the modem");
	exit(2);
    }
    
    /*
     * Start running the queue.  This never should return under normal
     * operations.
     */
    process_queue(out_qdir, in_qdir, in_email, fm, 1, 
		  max_delivery_time, min_retry_wait);

    exit(3);
}
