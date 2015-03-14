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
#include <termios.h>
#include <fcntl.h>
#include <malloc.h>
#include <sys/param.h>
#include <sys/wait.h>

#include "../../lib/libutil/dispatch.h"
#include "../../lib/libutil/reap.h"
#include "../../lib/libfax/libfax.h"
#include "spooler.h"
#include "seq.h"
#include "receive.h"

/*
 * This should also write stuff to the modem.
 */
int recv_disable(sd, fm)
     spooler_data *sd;
     int fm;
{
    dispatch_unregister_fd(DIO_READ, sd->fmp[fm].fd);
    return (0);
}

/*
 * This should also write stuff to the modem.
 */
int recv_enable(sd, fm)
     spooler_data *sd;
     int fm;
{
    int recv_handler();

    /*
     * Make sure nothing is left to read.
     */
    tcflush(sd->fmp[fm].fd, TCIFLUSH);

    dispatch_register_fd(DIO_READ, 0, sd->fmp[fm].fd, recv_handler,
			 (char *)sd);
    return (0);
}

typedef struct _recv_exit_data {
    spooler_data *sd;
    int fm;
} recv_exit_data;

/* ARGSUSED */
static int recv_exit_handler(pid, status, data)
     int pid;
     int status;
     char *data;
{
    recv_exit_data *d = (recv_exit_data *)data;

    /*
     * We don't care about stopped children.
     */
    if (WIFSTOPPED(status))
      return (0);

    /*
     * Modem is no longer busy.
     */
    d->sd->fm_busy[d->fm] = FALSE;

    /*
     * Add a read handler again.
     */
    recv_enable(d->sd, d->fm);

    /*
     * All done with the data.
     */
    cfree(d);    
    
    log(L_INFO, "recv process has exited");
    
    return (0);
}

static void print_fdcs_params(f, fp)
     FaxModem *f;
     FILE *fp;
{
    if (FAX_ISSET(f, FAX_F_FDCS)) {
	fprintf(fp, " vr=%s br=%s wd=%s ln=%s",
		t30_vr_string(&f->fdcs_params),
		t30_br_string(&f->fdcs_params),
		t30_wd_string(&f->fdcs_params),
		t30_ln_string(&f->fdcs_params));
      }
}

static int receive_pages(fmp, in_dir, info_fp)
     FaxModem *fmp;
     char *in_dir;
     FILE *info_fp;
{
    int page = 0;

    for (;;) {
	time_t start = time(0);
	char file[MAXPATHLEN];
	int fd;

	/*
	 * Tell the modem we are ready to go.
	 */
	switch (faxmodem_start_recv(fmp)) {
	  case RECV_OK:
	    break;
	  case RECV_FAILED:
	    fprintf(info_fp, "  page %d failed\n", page+1);
	    fprintf(info_fp, "\nReceived %d pages succesfully\n", page);
	    return (-1);
	  case RECV_DONE:
	    fprintf(info_fp, "\nReceived %d pages succesfully\n", page);
	    return (page);
	}

	/*
	 * Open the file to hold the G3 data.
	 */
	sprintf(file, "%s/%d", in_dir, page++);
	if ((fd = open(file, O_WRONLY|O_CREAT, 0666)) < 0) {
	    log(L_ALERT, "output file open failed: %m");
	    fprintf(info_fp, "  page %d failed\n", page);
	    return (-1);
	}
	
	/*
	 * Read in the page.
	 */
	if (faxmodem_recv_page(fmp, fd) < 0) {
	    log(L_WARNING, "receive of page failed");
	    fprintf(info_fp, "  page %d failed\n", page);
	    close(fd);
	    return (-1);
	}

	/*
	 * All done with the page.
	 */
	log(L_INFO, "received page %d", page);
	fprintf(info_fp, "  page %d ok", page);
	print_fdcs_params(fmp, info_fp);
	fprintf(info_fp, " time=%d\n", time(0) - start);
	close(fd);
    }
}

/*
 * This function does the bulk of the work for receiving a new
 * fax.  It answers the modem, reads the fax, writes the G3 files,
 * and mails notification.
 */
static int handle_incoming_fax(sd, fmp)
     spooler_data *sd;
     FaxModem *fmp;
{
    time_t start = time(0);
    char in_dir[MAXPATHLEN];
    char seq_file[MAXPATHLEN];
    char info_file[MAXPATHLEN];
    FILE *info_fp;
    int seq;
    int rc;

    log(L_INFO, "handling an incoming fax");

    if (faxmodem_answer(fmp) < 0) {
	log(L_NOTICE, "can't anwser the phone");
	faxmodem_hangup(fmp);
	return (-1);
    }

    sprintf(seq_file, "%s/%s", sd->in_qdir, SEQ_FILE);
    if ((seq = seq_next(seq_file)) < 0) {
	log(L_ALERT, "can't get incoming sequence number: %m");
	faxmodem_hangup(fmp);
	return (-1);
    }

    sprintf(in_dir, "%s/%d", sd->in_qdir, seq);
    if (mkdir(in_dir, 0775) < 0) {
	log(L_ALERT, "can't make incoming directory: %m");
	faxmodem_hangup(fmp);
	return (-1);
    }

    sprintf(info_file, "%s/LOG", in_dir);
    if ((info_fp = fopen(info_file, "w+")) == NULL) {
	log(L_ALERT, "can't make info file: %m");
	faxmodem_hangup(fmp);
	return (-1);
    }

    /*
     * Print the header for the info file.
     */
    fprintf(info_fp, "Reception of fax %d started at %s", seq, ctime(&start));
    faxmodem_print_id_strings(fmp, info_fp);
    fprintf(info_fp, "\n");

    /*
     * Receive the pages.
     */
    rc = receive_pages(fmp, in_dir, info_fp);
    fprintf(info_fp, "\nTotal connect time was %d seconds.\n", 
	    time(0) - start);

    /*
     * All done.
     */
    faxmodem_hangup(fmp);
    fclose(info_fp);
    if (sd->in_email) {
	char cmd[128];

	sprintf(cmd, "/usr/ucb/Mail -s \"incoming fax report\" %s < %s", 
		sd->in_email, info_file);
	system(cmd);
    }
    return (rc);
}

/* ARGSUSED */
static int recv_handler(fd, client_data, dtype, dio)
     int fd;
     char *client_data;
     int dtype;
     DIO *dio;
{
    spooler_data *sd = (spooler_data *)client_data;
    recv_exit_data *d;
    int fm;

    log(L_INFO, "the phone is (probably) ringing");

    /*
     * We don't want to be bothered with any more events from
     * this modem until we are done receiving the fax.
     */
    dispatch_unregister(dio);

    /*
     * Find the modem number.
     */
    for (fm = 0; fm < sd->numb_fm; fm++) {
	if (fd == sd->fmp[fm].fd)
	  break;
    }
    if (fm == sd->numb_fm) {
	log(L_ALERT, "can't find the modem");
	return (-1);
    }

    d = (recv_exit_data *)calloc(1, sizeof(recv_exit_data));
    d->sd = sd;
    d->fm = fm;

    switch(reap_fork(recv_exit_handler, (char *)d)) {
      case -1:
	/* error */
	cfree(d);
	return (-1);
      case 0:
	/* child */
	break;
      default:
	/* parent */
	sd->fm_busy[fm] = TRUE;
	return (0);
    }

    /*
     * We are now in the child.
     */
    handle_incoming_fax(sd, &sd->fmp[fm]);

    /*
     * Resync resync the modem, just in case.
     */
    if (faxmodem_sync(&sd->fmp[d->fm], FAXMODEM_SYNC_TRIES) < 0)
      log(L_EMERG, "can't resync to the modem");

    /*
     * All done!
     */
    exit(0);

    /* we never get here, but this keeps lint happy */
    return (0);
}

/*
 * Setup for incoming faxes.  Basically, we register read handlers
 * for all the modem fds.  If any data arrives on the fd, we know
 * we have an incoming fax, and we handle the call.
 */
int init_for_recv(sd)
     spooler_data *sd;
{
    int fm;

    for (fm = 0; fm < sd->numb_fm; fm++)
      recv_enable(sd, fm);

    return (0);
}
