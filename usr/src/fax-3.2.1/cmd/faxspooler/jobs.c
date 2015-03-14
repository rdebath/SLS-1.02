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
#include <time.h>
#include <sys/param.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "../../lib/libutil/dispatch.h"
#include "../../lib/libutil/reap.h"
#include "../../lib/libfax/libfax.h"
#include "spooler.h"
#include "jobs.h"

#define ESC	'\033'	/* escape	*/
#define FS	'\034'	/* end of page  */
#define GS	'\035'	/* end of file	*/

typedef enum {
    JOB_DONE,			/* job done, no action required  */
    JOB_ERROR,			/* job done, error occured	 */
    JOB_ENQUEUED,		/* job done, scan queue for work */
    JOB_DELETED,		/* job deleted, get number	 */
} job_exit_code;

typedef struct _job_exit_data {
    spooler_data *sd;
    int result_fds[2];
} job_exit_data;

/*
 * Handler for exiting request children.  Do nothing for queue
 * requests.  Add the new queue entry for enqueuing requests.
 */
/* ARGSUSED */
static int job_exit_handler(pid, status, data)
     int pid;
     int status;
     char *data;
{
    job_exit_data *d = (job_exit_data *)data;
    char file[MAXPATHLEN];

    log(L_INFO, "job exit handler");

    /*
     * We don't care about stopped children.
     */
    if (WIFSTOPPED(status))
      return (0);

    if (WIFEXITED(status)) {
	/*
	 * Only use exit code if job really exited.
	 */
	switch ((job_exit_code)WEXITSTATUS(status)) {
	  case JOB_DONE:
	    break;
	  case JOB_ERROR:
	    break;
	  case JOB_ENQUEUED:
	    if (tread(d->result_fds[1], file, sizeof(file), 5) > 0)
	      queue_add_file(d->sd->q, d->sd->out_qdir, file);
	    else
	      log(L_ERR, "error reading enqueue result");
	    break;
	  case JOB_DELETED:
	    if (tread(d->result_fds[1], file, sizeof(file), 5) > 0)
	      queue_delete_file(d->sd->q, file);
	    else
	      log(L_ERR, "error reading delete result");
	    break;
	}
    } else
      log(L_ALERT, "child didn't exit properly");

    /*
     * Close things up.
     */
    close(d->result_fds[0]);
    close(d->result_fds[1]);

    /*
     * Run the queue, for good measures.
     */
    run_queue(d->sd);

    /*
     * Done with the handle.
     */
    cfree(d);

    return (0);
}

/*
 * Handle a request to show the fax queue.
 */
/* ARGSUSED */
static job_exit_code show_queue(sd, in_fp, out_fp)
     spooler_data *sd;
     FILE *in_fp;
     FILE *out_fp;
{
    NODE *q_node = NULL;
    NODE *r_node = NULL;
    QueueEntry *qe;
    Recip *r;

    log(L_INFO, "printing the queue");

    if (list_length(sd->q) == 0) {
	fprintf(out_fp, "fax queue is empty\n");
	return (JOB_DONE);
    }

    fprintf(out_fp, "%-5s %-10s %-5s %-12s %-10s %-4s %-11s %-11s\n",
	    "job", "user", "pages", "recipient", "status", "trys",
	    "submitted", "last");

    while ((qe = (QueueEntry *)list_next(sd->q, &q_node)) != NULL) {
	int first = TRUE;
	fprintf(out_fp, "%-5s %-10.10s %-5d ", qe->file, qe->user, qe->pages);
	while ((r = (Recip *)list_next(qe->recip_list, &r_node)) != NULL) {
	    char time_first_str[128], time_last_str[128];
	    if (first)
	      first = FALSE;
	    else
	      fprintf(out_fp, "%-5s %-10s %-5s ", "", "", "");
	    if (r->phone[0] == '@')
	      fprintf(out_fp, "%-12s ", "*");
	    else
	      fprintf(out_fp, "%-12.12s ", r->phone);
	    strftime(time_first_str, sizeof(time_first_str), "%m/%e %R",
		     localtime(&r->time_first));
	    strftime(time_last_str, sizeof(time_last_str), "%m/%e %R",
		     localtime(&r->time_last));
	    switch (r->status) {
	      case SEND_STATUS_PENDING:
		fprintf(out_fp, "%-10s\n", "pending");
		break;
	      case SEND_STATUS_SENDING:
		fprintf(out_fp, "%-10s %-4d %-11s %-11s\n", "sending", 
			r->attempts, time_first_str, time_last_str);
		break;
	      case SEND_STATUS_SENT:
		fprintf(out_fp, "%-10s %-4d %-11s %-11s\n", "sent",
			r->attempts, time_first_str, time_last_str);
		break;
	      case SEND_STATUS_GIVEUP:
		fprintf(out_fp, "%-10s %-4d %-11s %-11s\n", "failed",
			r->attempts, time_first_str, time_last_str);
		break;
	      case SEND_STATUS_FAILED:
		fprintf(out_fp, "%-10s %-4d %-11s %-11s\n", "failed",
			r->attempts, time_first_str, time_last_str);
		break;
	      case SEND_STATUS_DIALER:
		fprintf(out_fp, "%-10s %-4d %-11s %-11s\n",
			hayes_result_msg(r->dialer_code),
			r->attempts, time_first_str, time_last_str);
		break;
	    }
	}
    }

    return (JOB_DONE);
}

/*
 * Do a massive cleanup for enqueue requests that failed.
 */
static void cleanup(dir)
     char *dir;
{
    char cmd[128];

    sprintf(cmd, "/bin/rm -r -f %s", dir);
    system(cmd);
}

static FILE *fopen_page(dir_name, page)
     char *dir_name;
     int page;
{
    char page_name[MAXPATHLEN];
    FILE *page_fp;

    sprintf(page_name, "%s/%d", dir_name, page);
    if ((page_fp = fopen(page_name, "w")) == NULL) {
	log(L_ALERT, "can't open page: %s: %m", page_name);
	return (NULL);
    }

    return (page_fp);
}

/*
 * Handle a request to enqueue a new fax job.
 */
/* ARGSUSED */
static job_exit_code enqueue_fax(sd, in_fp, out_fp, result_fd)
     spooler_data *sd;
     FILE *in_fp;
     FILE *out_fp;
     int result_fd;
{
    char seq_file[MAXPATHLEN];
    char dir_name[MAXPATHLEN];
    char qf_name[MAXPATHLEN];
    char file[MAXPATHLEN];
    int seq;
    FILE *qf_fp;
    int escaped = FALSE;
    FILE *page_fp = NULL;
    int page = 0;
    char buf[BUFSIZ];
    int bytes;

    log(L_INFO, "enqueuing a job");

    /*
     * Get the next job number.
     */
    sprintf(seq_file, "%s/%s", sd->out_qdir, SEQ_FILE);
    if ((seq = seq_next(seq_file)) < 0) {
	log(L_ALERT, "can't get outgoing sequence number: %m");
	return (JOB_ERROR);
    }
    sprintf(file, "%d", seq);

    /*
     * Create the queue file directory.
     */
    sprintf(dir_name, "%s/%d", sd->out_qdir, seq);
    if (mkdir(dir_name, 0775) < 0) {
	log(L_ALERT, "can't make job directory: %m");
	return (JOB_ERROR);
    }

    /*
     * Create the queue file itself.
     */
    sprintf(qf_name, "%s/QF", dir_name);
    if ((qf_fp = fopen(qf_name, "w")) == NULL) {
	log(L_ALERT, "can't make queue file: %m");
	cleanup(dir_name);
	return (JOB_ERROR);
    }

    /*
     * Add the FILE field to the queue file, with the file name.
     */
    fprintf(qf_fp, "FILE: %d\n", seq);

    /*
     * Read in the queue file header.
     */
    for (;;) {
	if (fgets(buf, sizeof(buf), in_fp) == NULL) {
	    log(L_ERR, "premature end of enqueue job");
	    fclose(qf_fp);
	    cleanup(dir_name);
	    return (JOB_ERROR);
	}

	if (strncmp(buf, "DATA:", 5) == 0)
	  break;

	fputs(buf, qf_fp);
    }

    /*
     * Read in the queue file data.
     */
    while ((bytes = fread(buf, sizeof(char), sizeof(buf), in_fp)) > 0) {
	int i;

	/*
	 * Read in the page data, processing the escapes.  Protocol
	 * start with an ESC FS, to start things off.
	 */
	for (i = 0; i < bytes; i++) {
	    if (escaped) {
		escaped = FALSE;
		switch (buf[i]) {
		  case ESC:
		    fputc(ESC, page_fp);
		    break;
		  case FS:
		    /* open next page */
		    if (page_fp != NULL)
		      fclose(page_fp);
		    if ((page_fp = fopen_page(dir_name, page++)) == NULL) {
			cleanup(dir_name);
			return (JOB_ERROR);
		    }
		    break;
		  case GS:
		    /* all done */
		    fclose(page_fp);
		    fprintf(qf_fp, "PAGES: %d\n", page);
		    fclose(qf_fp);
		    write(result_fd, file, strlen(file)+1);
		    return (JOB_ENQUEUED);
		  default:
		    /* error */
		    log(L_ALERT, "unknown escape received in input");
		    fclose(page_fp);
		    cleanup(dir_name);
		    return (JOB_ERROR);
		}
	    } else {
		switch (buf[i]) {
		  case ESC:
		    escaped = TRUE;
		    break;
		  default:
		    putc(buf[i], page_fp);
		    break;
		}
	    }
	}
    }

    /*
     * If we fall into here, an error has occured.
     */
    
    log(L_ALERT, "premature end of data");

    if (page_fp != NULL)
      fclose(page_fp);
    cleanup(dir_name);

    return (JOB_ERROR);
}

/*
 * Handle a request to delete a fax job.
 */
/* ARGSUSED */
static job_exit_code delete_fax(sd, in_fp, out_fp, result_fd)
     spooler_data *sd;
     FILE *in_fp;
     FILE *out_fp;
     int result_fd;
{
    char file[MAXPATHLEN];
    NODE *q_node = NULL;
    QueueEntry *qe;

    /*
     * Get the job that we're supposed to delete.
     */
    fscanf(in_fp, "%s", file);

    log(L_INFO, "deleting job: %s", file);

    /*
     * See if it is in the queue.
     */
    while ((qe = (QueueEntry *)list_next(sd->q, &q_node)) != NULL) {
	if (strcmp(file, qe->file) == 0) {
	    fprintf(out_fp, "job %s has been deleted\n", file);
	    write(result_fd, file, strlen(file)+1);
	    return (JOB_DELETED);
	}
    }

    fprintf(out_fp, "Job %s not in queue\n", file);
    return (JOB_ERROR);
}

/*
 * Figure out what the request is, and run the appropriate
 * handler.  Since we are in the child, we can be sloppy
 * with closing off fds.
 */
static job_exit_code process_job(sd, client, result_fd)
     spooler_data *sd;
     int client;
     int result_fd;
{
    FILE *client_in_fp;
    int client_out_fd;
    FILE *client_out_fp;
    char buf[1024];

    /*
     * Create the read/write files.
     */
    if ((client_in_fp = fdopen(client, "r")) == NULL) {
	log(L_ALERT, "can't open client fd: %m");
	return (JOB_ERROR);
    }
    if ((client_out_fd = dup(client)) < 0) {
	log(L_ALERT, "dup of client fd failed: %m");
	return (JOB_ERROR);
    }
    if ((client_out_fp = fdopen(client_out_fd, "w")) == NULL) {
	log(L_ALERT, "can't open client fd: %m");
	return (JOB_ERROR);
    }

    if (fgets(buf, sizeof(buf), client_in_fp) == NULL)
      return (JOB_ERROR);

    if (strncmp(buf, "QUEUE", 5) == 0)
      return (show_queue(sd, client_in_fp, client_out_fp));
    else if (strncmp(buf, "ENQUEUE", 7) == 0)
      return (enqueue_fax(sd, client_in_fp, client_out_fp, result_fd));
    else if (strncmp(buf, "DELETE", 6) == 0)
      return (delete_fax(sd, client_in_fp, client_out_fp, result_fd));
    else {
	fprintf(client_out_fp, "invalid request\n");
	return (JOB_ERROR);
    }
}

/*
 * This function is called from the dispatcher to run client
 * requests.  Basically, we accept the TCP connection and
 * fork a process to handle the request.
 */
/* ARGSUSED */
static int job_handler(fd, client_data, dtype, dio)
     int fd;
     char *client_data;
     int dtype;
     DIO *dio;
{
    spooler_data *sd = (spooler_data *)client_data;
    job_exit_data *d;
    int client;

    log(L_INFO, "received job request");

    /*
     * Accept a client connection.
     */
    if ((client = tcp_accept_connection(fd)) < 0) {
	log(L_NOTICE, "client accept failed: %m");
	return (-1);
    }

    /*
     * Create the handle to pass data to the reap handler.
     */
    if ((d = (job_exit_data *)calloc(1, sizeof(job_exit_data))) == NULL) {
	log(L_ALERT, "calloc of exit data failed: %m");
	return (-1);
    }
    d->sd = sd;

    /*
     * Setup the communication link.
     */
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, d->result_fds) < 0) {
	log(L_ALERT, "can't create socket pair: %m");
	cfree(d);
	return (-1);
    }

    /*
     * Now create a child handler for this request.
     */
    switch(reap_fork(job_exit_handler, (char *)d)) {
      case -1:
	/* error */
	log(L_ALERT, "spawn of job agent failed: %m");
	close(d->result_fds[0]);
	close(d->result_fds[1]);
	cfree(d);
	return (-1);
      case 0:
	/* child */
	break;
      default:
	/* parent */
	close (client);
	return (0);
    }

    /*
     * We are now in the child.
     */

    exit ((int)process_job(sd, client, d->result_fds[0]));

    /* to make lint happy */
    return (0);
}

/*
 * This function is called once, at startup time, to setup
 * for processing client requests.
 */
int init_for_jobs(sd)
     spooler_data *sd;
{
    int fd;

    if ((fd = tcp_make_listener(FAX_SERVICE)) < 0) {
	log(L_EMERG, "can't make listener: %m");
	return (-1);
    }

    dispatch_register_fd(DIO_READ, 0, fd, job_handler, (char *)sd);

    return (0);
}
