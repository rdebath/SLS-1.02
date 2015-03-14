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
#include <signal.h>
#include <pwd.h>
#include <sys/param.h>

#include "../../lib/libutil/list.h"
#include "../../lib/libfax/libfax.h"

/*
 * Some flags that we share with the signal handlers.
 */
static int pages = -1;
static int coversheet = FALSE;

/*
 * Base filenames for the PPM files that Ghostscript generates.
 */
static char out_base[MAXPATHLEN];
static char coversheet_base[MAXPATHLEN];

/*
 * This is the Postscript file that is being spooled.
 */
static int delete_ps_file = FALSE;
static char *ps_file;

/*
 * Delete generated G3 files.
 */
static void cleanup_g3_files(base, pages)
     char *base;
     int pages;
{
    int page;

    for (page = 1; page <= pages; page++) {
	char file[MAXPATHLEN];
	sprintf(file, "%s.g3.%d", base, page);
	unlink(file);
    }
}

/*
 * Cleanup temporary files.
 */
static void cleanup()
{
    if (delete_ps_file)
      unlink(ps_file);
    if (pages > 0 && strlen(out_base) > 0)
      cleanup_g3_files(out_base, pages);
    if (coversheet && strlen(coversheet_base) > 0)
      cleanup_g3_files(coversheet_base, 1);
}

/*
 * Called when we get a variety of signals.
 */
static void signal_handler()
{
    fprintf(stderr, "transmission aborted\n");
    cleanup();
    exit(1);
}

/*
 * Install the signals that we want to trap for cleanup.
 */
static void install_signal_handlers()
{
    signal(SIGHUP, signal_handler);
    signal(SIGINT, signal_handler);
    signal(SIGQUIT, signal_handler);
}

main(argc,argv)
     int argc;
     char *argv[];
{
    int c;
    int errflg = 0;
    extern char *optarg;
    extern int optind;

    int pid = getpid();
    int now = time(0);

    int got_recip = 0;
    int rc;

    char fax_cmd[2048];
    char recips[2048];
    char *recipient = "";
    char *sender = "";
    char *sender_fax = "";
    int mail_notification = FALSE;
    char *host = NULL;
    char *user_name = NULL;

    strcpy(recips, "");
    strcpy(out_base, "");
    strcpy(coversheet_base, "");

    while ((c = getopt(argc, argv, "cp:r:s:S:mu:h:")) != -1)
      switch (c) {
	case 'c':
	  coversheet = TRUE;
	  break;
	case 'p':
	  got_recip++;
	  strcat(recips, " ");
	  strcat(recips, optarg);
	  break;
	case 'r':
	  recipient = optarg;
	  break;
	case 's':
	  sender = optarg;
	  break;
	case 'S':
	  sender_fax = optarg;
	  break;
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

    if (errflg || got_recip == 0) {
	fprintf(stderr, "usage: %s -p phone [filename]\n", argv[0]);
	fprintf(stderr, "\tone or more -p argument may be given\n");
	fprintf(stderr, "\tif no filename is given, input is from stdin\n");
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

    /*
     * Catch some signals so we can do a proper cleanup.
     */
    install_signal_handlers();

    /*
     * Do we get input from stdin?
     */
    if (optind == argc) {
	char *mktemp();
	char buf[BUFSIZ];
	int bytes;
	int fd;
	
	ps_file = mktemp("/tmp/faxdXXXXXX");
	delete_ps_file = TRUE;
	
	if ((fd = open(ps_file, O_WRONLY|O_CREAT, 0666)) < 0) {
	    perror("temp file");
	    exit(2);
	}

	while ((bytes = read(0, buf, sizeof(buf))) > 0)
	  write(fd, buf, bytes);
	
	close(fd);
    } else
      ps_file = argv[optind];

    /*
     * Make the G3 files.
     */
    sprintf(out_base, "/tmp/fax%d%d", pid, now);
    if ((pages = cvt_postscript_to_g3(ps_file, out_base)) <= 0) {
	fprintf(stderr, "can't convert postscript file\n");
	exit(3);
    }

    /*
     * Make the coversheet.
     */
    if (coversheet) {
	sprintf(coversheet_base, "/tmp/faxc%d%d", pid, now);
	if (cvt_coversheet_to_g3(coversheet_base, recipient, sender, 
				 sender_fax, pages) < 0) {
	    fprintf(stderr, "can't convert coversheet\n");
	    cleanup_g3_files(out_base, pages);
	    exit(3);
	}
    }

    /*
     * Build the fax command.
     */
    sprintf(fax_cmd, "%s", FAX_ENQ_PROG);
    if (user_name != NULL)
      sprintf(&fax_cmd[strlen(fax_cmd)], " -u %s", user_name);
    if (host != NULL)
      sprintf(&fax_cmd[strlen(fax_cmd)], " -h %s", host);
    if (mail_notification)
      strcat(fax_cmd, " -m");
    strcat(fax_cmd, recips);
    strcat(fax_cmd, " ---");
    if (coversheet)
      sprintf(&fax_cmd[strlen(fax_cmd)], " %s.g3.1", coversheet_base);
    sprintf(&fax_cmd[strlen(fax_cmd)], " %s.g3:%d", out_base, pages);

    /*
     * Run the fax command.
     */
    rc = system(fax_cmd);

    /*
     * All done!  Cleanup and exit.
     */
    cleanup();

    exit(rc);
}
