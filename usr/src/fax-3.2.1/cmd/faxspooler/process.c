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
#include <malloc.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "../../lib/libutil/dispatch.h"
#include "../../lib/libutil/reap.h"
#include "../../lib/libfax/libfax.h"
#include "spooler.h"
#include "queue_entry.h"
#include "queue.h"
#include "send.h"
#include "receive.h"
#include "jobs.h"
#include "process.h"

/*
 * Mail a notification report to the user listed in the queue entry.
 * This function should only be called if the notification bit in
 * the flags field has been set.
 */
static void mail_notification(qe)
     QueueEntry *qe;
{
    char cmd[256];
    FILE *fp;
    NODE *node = NULL;
    Recip *r;

    sprintf(cmd, "/usr/ucb/Mail -s \"fax delivery report\" %s", qe->user);
    if ((fp = popen(cmd, "w")) == NULL) {
	log(L_ALERT, "can't open mail program: %m");
	return;
    }

    fprintf(fp, "Fax job was submitted by %s.\n", qe->user);
    fprintf(fp, "Job had a total of %d pages.\n\n", qe->pages);

    while ((r = (Recip *)list_next(qe->recip_list, &node)) != NULL) {
	fprintf(fp, "Recipient %s: ", r->phone);
	if (r->status == SEND_STATUS_SENT)
	  fprintf(fp, "sent in %d seconds.\n", r->total_time);
	else
	  fprintf(fp, "delivery failed.\n");
    }

    pclose(fp);
}

typedef enum {
    RECIP_READY,
    RECIP_NOT_READY,
    RECIP_DONE,
} recip_stat;

/*
 * Determine if a recipient is ready to be run.
 *
 * Return codes:
 *	RECIP_READY	ok to send to this recipient
 *	RECIP_NOT_READY	haven't reached retry time
 *	RECIP_DONE	all done with this recipient
 */
static int is_recip_ready(sd, r, now)
     spooler_data *sd;
     Recip *r;
     time_t now;
{
    /*
     * Initialize delivery time, if necessary.
     */
    if (r->time_first == 0)
      r->time_first = now;

    /*
     * Has the fax been sent to this recipient yet?
     */
    if (r->status == SEND_STATUS_SENT || r->status == SEND_STATUS_GIVEUP)
      return (RECIP_DONE);

    /*
     * Make sure the job hasn't timed out, and that the
     * minimum retry interval has elapsed.  If so, then
     * make a delivery attempt.
     */
    if (now - r->time_first > sd->max_delivery_time) {
	/*
	 * We've exceeded the max retry interval.
	 */
	log(L_INFO, "giving up on delivery to %s", r->phone);
	r->status = SEND_STATUS_GIVEUP;
	return (RECIP_DONE);
    } else if (now - r->time_last > sd->min_retry_wait) {
	/*
	 * Ready to try sending the job (again).
	 */
	return (RECIP_READY);
    } else
      return (RECIP_NOT_READY);
}

static void send_log(sd, qe, r)
     spooler_data *sd;
     QueueEntry *qe;
     Recip *r;
{
    char log_file[MAXPATHLEN];
    FILE *log_fp;

    sprintf(log_file, "%s/.log", sd->out_qdir);
    if ((log_fp = fopen(log_file, "a")) == NULL)
      return;

    fprintf(log_fp, "%s sent a %d page fax to %s (%d seconds)\n",
	    qe->user, qe->pages, r->phone, r->total_time);

    fclose(log_fp);
}

typedef struct _recip_exit_data {
    spooler_data *sd;
    Recip *r;
    QueueEntry *qe;
    int result_fds[2];
    int fm;
} recip_exit_data;

/* ARGSUSED */
static int recip_exit_handler(pid, status, data)
     int pid;
     int status;
     char *data;
{
    recip_exit_data *d = (recip_exit_data *)data;
    spooler_data *sd;

    /*
     * We don't care about stopped children.
     */
    if (WIFSTOPPED(status))
      return (0);

    /*
     * Only perform the read if the child really exited.
     */
    if (WIFEXITED(status)) {
	int rc = WEXITSTATUS(status);
    
	/*
	 * Copy back the recip info.
	 */
	read(d->result_fds[1], (char *)d->r, sizeof(Recip));

	/*
	 * Log the result, if it was send.
	 */
	if (d->r->status == SEND_STATUS_SENT)
	  send_log(d->sd, d->qe, d->r);

	log(L_INFO, "child exited with rc=%d, status=%d", rc, d->r->status);
    } else {
	d->r->status = SEND_STATUS_FAILED;
	log(L_ALERT, "child didn't exit properly");
    }

    /*
     * Indicate the modem is now free.
     */
    sd = d->sd;
    sd->fm_busy[d->fm] = FALSE;
    recv_enable(sd, d->fm);

    /*
     * We are no longer running this queue entry.
     */
    d->qe->flags &= ~QUEUE_F_RUNNING;

    /*
     * Close things up.
     */
    close(d->result_fds[0]);
    close(d->result_fds[1]);
    cfree(d);

    /*
     * Run the queue, for good measures.
     */
    run_queue(sd);

    return (0);
}

/*
 * Attempt to send a fax to the given recipient.  This spawns a new
 * sending process, which actually does the sending work.  The
 * send process write the recip and faxmodem struct to a file,
 * which the parent will read in when the job complete.  This is
 * the easiest way to pass the status information back and forth.
 *
 * Return codes:
 *	0	fax is being sent
 *     -1	sending fork failed
 */
static int run_recip(sd, qe, r, qdir, fmp, fm)
     spooler_data *sd;
     QueueEntry *qe;
     Recip *r;
     char *qdir;
     FaxModem *fmp;
     int fm;
{
    recip_exit_data *d;

    log(L_INFO, "sending to %s", r->phone);

    /*
     * Disable incoming faxes from this modem.
     */
    recv_disable(sd, fm);

    /*
     * Data we pass to the fork reaper.
     */
    if ((d = (recip_exit_data *)calloc(1, sizeof(recip_exit_data))) == NULL) {
	log(L_ALERT, "calloc of exit data failed: %m");
	return (-1);
    }
    d->qe = qe;
    d->r = r;
    d->fm = fm;
    d->sd = sd;

    /*
     * Setup the communication link.
     */
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, d->result_fds) < 0) {
	log(L_ALERT, "can't create socket pair: %m");
	recv_enable(sd, fm);
	cfree(d);
	return (-1);
    }

    /*
     * Indicate that we are currently sending this fax.
     */
    r->status = SEND_STATUS_SENDING;

    /*
     * Fork off the child process to handling sending this fax.
     */
    switch(reap_fork(recip_exit_handler, (char *)d)) {
      case -1:
	/* error */
	log(L_ALERT, "spawn of sending agent failed: %m");
	recv_enable(sd, fm);
	close(d->result_fds[0]);
	close(d->result_fds[1]);
	cfree(d);
	return (-1);
      case 0:
	/* child */
	break;
      default:
	/* parent */
	qe->flags |= QUEUE_F_RUNNING;
	return (0);
    }
    
    /*
     * We are now in a child process. Start sending the fax. 
     * Write return status information down the socketpair
     * pipe.  Return 0 to indicate we can read result.
     */
    if (send_fax(fmp, r, qdir, qe->file, qe->pages) == 0) {
	r->attempts++;
	if (r->status != SEND_STATUS_SENT) {
	    log(L_INFO, "unable to send fax to %s", r->phone);
	}
    }
    write(d->result_fds[0], r, sizeof(Recip));

    /*
     * Resync the modem, just in case.
     */
    if (faxmodem_sync(&sd->fmp[d->fm], FAXMODEM_SYNC_TRIES) < 0)
      log(L_EMERG, "can't resync to the modem");

    /*
     * All done!
     */
    exit (0);
    
    /* we never get here, but this keeps lint happy */
    return (0);
}

/*
 * Delete qe from the queue q, along with all the associated
 * queue files.  What's the best thing to do if the delete fails?
 *
 * Return codes:
 *	0	delete was ok
 *     -1	delete failed
 */
static int delete_job(q, qe, qdir)
     LIST *q;
     QueueEntry *qe;
     char *qdir;
{
    log(L_INFO, "deleting job %s from %s", qe->file, qe->user);

    if (qe->flags & QUEUE_F_EMAIL)
      mail_notification(qe);

    queue_entry_delete_files(qe->file, qdir);

    list_delete(q, (char *)qe);

    return (0);
}

static int get_modem(sd)
     spooler_data *sd;
{
    int fm;
    
    /*
     * Is a modem free?
     */
    for (fm = 0; fm < sd->numb_fm; fm++)
      if (!sd->fm_busy[fm])
	return (fm);

    return (-1);
}

/*
 * Run the queue.  This routine invokes the sending routine on
 * any faxes outstanding in the queue that are ready.  The routine
 * maintains state, and is invoked when jobs finish, and by a 
 * dispatch timer.
 *
 * Return codes:
 *	0	no queue entries worked on
 *     -1	failure
 */
int run_queue(sd)
     spooler_data *sd;
{
    int wrapped = FALSE;

    /*
     * See if we can get some new work, if not just return.
     */
    if (sd->qe == NULL)
      if ((sd->qe = (QueueEntry *)list_next(sd->q, &sd->q_node)) == NULL)
	return (0);

    /*
     * Scan all recipients in the queue, and invoke the
     * sending routine on any that have not yet received
     * the fax.
     */
    for (;;) {
	time_t now = time(0);
	int fm = get_modem(sd);

	/*
	 * Assume we were left on the last recipient processed,
	 * move on to the next one.  If we hit the last one,
	 * move to the next queue entry.
	 */
	if ((sd->r = (Recip *)list_next(sd->qe->recip_list, &sd->r_node))
	    == NULL) {
	    QueueEntry *qe_last = sd->qe;
	    /*
	     * Carefully delete this queue entry if no recipients are
	     * left in the entry.
	     */
	    sd->qe = (QueueEntry *)list_next(sd->q, &sd->q_node);
	    /*
	     * Only do the delete if we are not running any of
	     * the recips, of course.
	     */
	    if (!(qe_last->flags & QUEUE_F_RUNNING) && sd->recips_left == 0)
	      delete_job(sd->q, qe_last, sd->out_qdir);
	    else
	      queue_entry_write(qe_last, sd->out_qdir);
	    /*
	     * If the queue has emptied out, then we are all done.
	     */
	    if (list_length(sd->q) == 0)
	      return (0);
	    /*
	     * Have we wrapped around the queue?  If so, start from the top.
	     */
	    if (sd->qe == NULL) {
		if (wrapped)
		  return (0);
		else
		  wrapped = TRUE;
		sd->q_node = NULL;
		sd->qe = (QueueEntry *)list_next(sd->q, &sd->q_node);
	    }
	    /*
	     * Get the first receipient in the queue entry.
	     */
	    sd->r_node = NULL;
	    sd->r = (Recip *)list_next(sd->qe->recip_list, &sd->r_node);
	    sd->recips_left = 0;
	}

	/*
	 * At this point we've got a new recipient to consider,
	 * see if its ready for some work.
	 */
	if (!(sd->qe->flags & QUEUE_F_DELETED)) {
	    switch (is_recip_ready(sd, sd->r, now)) {
	      case RECIP_READY:
		sd->recips_left++;
		/*
		 * Run job only if we have a modem.
		 */
		if (fm >= 0) {
		    sd->r->time_last = now;
		    if (run_recip(sd, sd->qe, sd->r, sd->out_qdir,
				  &sd->fmp[fm], fm) == 0)
		      sd->fm_busy[fm] = TRUE;
		}
		break;
	      case RECIP_NOT_READY:
		sd->recips_left++;
		break;
	      case RECIP_DONE:
		break;
	    }
	}
    }
}

/*
 * This is this the main work function.  It periodically scans
 * the queue for new work.  Otherwise, it runs the queue and
 * checks for incoming faxes.  It is passed an initial queue.
 */
int process_queue(out_qdir, in_qdir, in_email, fmp, numb_fm, 
		  max_delivery_time, min_retry_wait)
     char *out_qdir;
     char *in_qdir;
     char *in_email;
     FaxModem *fmp;
     int numb_fm;
     int max_delivery_time;
     int min_retry_wait;
{
    spooler_data *sd;

    sd = (spooler_data *)calloc(1, sizeof(spooler_data));

    sd->out_qdir = out_qdir;
    sd->in_qdir = in_qdir;
    sd->in_email = in_email;
    sd->fmp = fmp;
    sd->numb_fm = numb_fm;
    sd->fm_busy = (int *)calloc(numb_fm, sizeof(int));
    sd->max_delivery_time = max_delivery_time;
    sd->min_retry_wait = min_retry_wait;

    /*
     * Initialize the global state.
     */
    if ((sd->q = list_make(NULL, free)) == NULL) {
	log(L_EMERG, "can't initialize queue: %m");
	return (-1);
    }

    /*
     * Load up whatever work was left in the work queue.
     */
    queue_read(sd->q, sd->out_qdir);

    /*
     * Setup for incoming faxes.
     */
    init_for_recv(sd);

    /*
     * Setup for TCP connection for queue and enqueuing requests.
     */
    init_for_jobs(sd);

    /*
     * Add the periodic queue scanner and go.
     */
    dispatch_register_periodic(0, 10.0, run_queue, (char *)sd);
    dispatch_run();

    return (-1);
}
