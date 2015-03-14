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
#include <strings.h>
#include <dirent.h>
#include <sys/param.h>

#include "../../lib/libfax/libfax.h"
#include "spooler.h"
#include "queue_entry.h"

static int parse_string(), write_string(), parse_recip(), write_recip(),
  parse_int(), write_int(), write_bit();

#define arg(f)  ((int)(&(((QueueEntry *)NULL)->f)))
#define sarg(f) ((int)(((QueueEntry *)NULL)->f))

static struct _queue_switch {
    char *ident;
    int (*parse_func)();
    int (*write_func)();
    int offset;
    int flag;
} queue_switch[] = {
    {"FILE",  parse_string, write_string, sarg(file), QUEUE_F_FILE},
    {"USER",  parse_string, write_string, sarg(user), QUEUE_F_USER},
    {"EMAIL", NULL,         write_bit, 	  NULL,       QUEUE_F_EMAIL},
    {"RETRY", parse_int,    write_int,	  arg(retry), QUEUE_F_RETRY}, 
    {"TIME",  parse_int,    write_int,	  arg(time),  QUEUE_F_TIME},
    {"PAGES", parse_int,    write_int,	  arg(pages), QUEUE_F_PAGES},
    {"RECIP", parse_recip,  write_recip,  0,          QUEUE_F_RECIP},
};
#define queue_switch_len (sizeof(queue_switch)/sizeof(struct _queue_switch))

#undef arg
#undef sarg

/* ARGSUSED */
static int write_bit(qe, offset, key, fp)
     QueueEntry *qe;
     char *offset;
     char *key;
     FILE *fp;
{
    fprintf(fp, "%s:\n", key);

    return (0);
}

/* ARGSUSED */
static int parse_string(qe, offset, start)
     QueueEntry *qe;
     char *offset;
     char *start;
{
    if (sscanf(start, "%s", offset) != 1)
      return (-1);
    
    return (0);
}

/* ARGSUSED */
static int write_string(qe, offset, key, fp)
     QueueEntry *qe;
     char *offset;
     char *key;
     FILE *fp;
{
    fprintf(fp, "%s: %s\n", key, offset);

    return (0);
}

/* ARGSUSED */
static int parse_int(qe, offset, start)
     QueueEntry *qe;
     char *offset;
     char *start;
{
    int *value = (int *)offset;
    
    if (sscanf(start, "%d", value) != 1)
      return (-1);

    return (0);
}

/* ARGSUSED */
static int write_int(qe, offset, key, fp)
     QueueEntry *qe;
     char *offset;
     char *key;
     FILE *fp;
{
    int *value = (int *)offset;

    fprintf(fp, "%s: %d\n", key, *value);

    return (0);
}

/* ARGSUSED */
static int parse_recip(qe, offset, start)
     QueueEntry *qe;
     char *offset;
     char *start;
{
    Recip *r;

    if ((r = (Recip *)calloc(1, sizeof(Recip))) == NULL) {
	log(L_EMERG, "parse_recip: malloc failed: %m");
	return (-1);
    }

    /*
     * Perform a simple parse of the recipient args.
     */
    if (sscanf(start, "%d %d %s %d %d %d",
	       &r->status, &r->dialer_code, r->phone, 
	       &r->attempts, &r->time_first, &r->time_last) != 6) {
	log(L_WARNING, "parse recip failed");
	return (-1);
    }

    /*
     * Perhaps this is a kludge, but when we restart we don't
     * want to leave the entry in a state of sending.
     */
    if (r->status == SEND_STATUS_SENDING)
      r->status = SEND_STATUS_PENDING;

    /*
     * Add the recipient to the list of recipients.
     */
    list_add(qe->recip_list, (char *)r);

    return (0);
}

static void write_a_recip(r, fp, key)
     Recip *r;
     FILE *fp;
     char *key;
{
    fprintf(fp, "%s: %d %d %s %d %d %d\n",
	    key, r->status, r->dialer_code, r->phone, r->attempts, 
	    r->time_first, r->time_last);
}

/* ARGSUSED */
static int write_recip(qe, offset, key, fp)
     QueueEntry *qe;
     char *offset;
     char *key;
     FILE *fp;
{
    list_map(qe->recip_list, write_a_recip, fp, key);

    return (0);
}

static int validate(qe)
     QueueEntry *qe;
{
    if (!(qe->flags & QUEUE_F_FILE)) {
	log(L_WARNING, "queue entry missing file name");
	return (-1);
    }

    if (!(qe->flags & QUEUE_F_USER)) {
	log(L_WARNING, "queue entry missing user name");
	return (-1);
    }

    if (!(qe->flags & QUEUE_F_RECIP)) {
	log(L_WARNING, "queue entry has no recipients");
	return (-1);
    }

    return (0);
}

void queue_entry_free(qe)
     QueueEntry *qe;
{
    list_free(qe->recip_list);
    free(qe->recip_list);
    cfree(qe);
}

QueueEntry *queue_entry_read(filename)
     char *filename;
{
    FILE *fp;
    QueueEntry *qe;
    char buf[1024];

    log(L_DEBUG, "reading queue file: %s", filename);

    if ((fp = fopen(filename, "r")) == NULL) {
	log(L_WARNING, "can't open queue file %s for reading: %m", filename);
	return (NULL);
    }

    if ((qe = (QueueEntry *)calloc(1, sizeof(QueueEntry))) == NULL) {
	log(L_EMERG, "can't malloc queue file storage: %m");
	return (NULL);
    }

    if ((qe->recip_list = list_make(NULL, free)) == NULL) {
	log(L_EMERG, "can't make recipient list: %m");
	return (NULL);
    }

    while (fgets(buf, sizeof(buf), fp) != NULL) {
	char *delim;
	int i;

	if ((delim = index(buf, ':')) == NULL) {
	    log(L_WARNING, "missing ':' in queue file: %s", buf);
	    fclose(fp);
	    return (NULL);
	}

	for (i = 0; i < queue_switch_len; i++) {
	    struct _queue_switch *qs = &queue_switch[i];
	    if (strncmp(qs->ident, buf, strlen(qs->ident)) == 0) {
		qe->flags |= qs->flag;
		if (qs->parse_func != NULL)
		  if ((*qs->parse_func)(qe, &((char *)qe)[qs->offset], delim+2)
		      < 0)
		    log(L_WARNING, "error reading qf \"%s\"", buf);
	    }
	}
    }

    fclose(fp);

    if (validate(qe) < 0) {
	log(L_WARNING, "validation of qf %s failed", filename);
	queue_entry_free(qe);
	return (NULL);
    }

    return (qe);
}

int queue_entry_write(qe, qdir)
     QueueEntry *qe;
     char *qdir;
{
    char pathname[MAXPATHLEN];
    FILE *fp;
    int i;

    sprintf(pathname, "%s/%s/QF", qdir, qe->file);
    if ((fp = fopen(pathname, "w+")) == NULL) {
	log(L_WARNING, "can't open queue file %s for writing: %m", pathname);
	return (-1);
    }

    for (i = 0; i < queue_switch_len; i++) {
	struct _queue_switch *qs = &queue_switch[i];
	if (qs->flag & qe->flags)
	  if ((*qs->write_func)(qe, &((char *)qe)[qs->offset], qs->ident, fp)
	      < 0)
	    log(L_WARNING, "error writing qf");
    }

    fclose(fp);

    return (0);
}

/*
 * Removes all the files associated with a queue entry.  Returns -1
 * if something couldn't be deleted, though the function tries
 * to delete as much as it can.
 */
int queue_entry_delete_files(file, qdir)
     char *file;
     char *qdir;
{
    char pathname[MAXPATHLEN];
    DIR *dirp;
    struct dirent *dp;
    int rc = 0;
    
    sprintf(pathname, "%s/%s", qdir, file);

    log(L_DEBUG, "deleting queue files from: %s", pathname);

    if ((dirp = opendir(pathname)) == NULL) {
	log(L_INFO, "error opening dir %s: %m", pathname);
	rc = -1;
    } else {
	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	    char file[MAXPATHLEN];
	    if (strcmp(dp->d_name, ".") == 0)
	      continue;
	    if (strcmp(dp->d_name, "..") == 0)
	      continue;
	    sprintf(file, "%s/%s", pathname, dp->d_name);
	    if (unlink(file) < 0) {
		log(L_INFO, "error deleting %s: %m", file);
		rc = -1;
	    }
	}
    }
    if (rmdir(pathname) < 0) {
	log(L_INFO, "error deleting %s: %m", pathname);
	return (-1);
    }

    return (rc);
}

static void print_a_recip(r)
     Recip *r;
{
    printf("status=%d; dialer_code=%d; phone=%s; attempts=%d; times=%d,%d\n",
	   r->status, r->dialer_code, r->phone, r->attempts, 
	   r->time_first, r->time_last);
}

void queue_entry_print(qe)
     QueueEntry *qe;
{
    printf("*** %s\n", qe->file);
    printf("flags=0x%x\n", qe->flags);
    printf("recipients:\n");
    list_map(qe->recip_list, print_a_recip);
}
