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
#include <dirent.h>
#include <sys/param.h>

#include "../../lib/libfax/libfax.h"
#include "spooler.h"
#include "queue_entry.h"
#include "queue.h"

/*
 * This function reads in the entire queue directory, and returns a
 * list of queue files.  This is used to both scan the working queue, 
 * and the new queue.  It is the job of this function to keep the
 * queue clean.  If malformed queue files are found, they should be
 * cleaned out.
 */
int queue_read(q, qdir)
     LIST *q;
     char *qdir;
{
    DIR *dirp;
    struct dirent *dp;
    int count = 0;

    if ((dirp = opendir(qdir)) == NULL) {
	log(L_EMERG, "can't open queue directory: %m");
	return (-1);
    }

    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	char pathname[MAXPATHLEN];
	QueueEntry *qe;

	if (dp->d_name[0] == '.')
	  continue;

	sprintf(pathname, "%s/%s/QF", qdir, dp->d_name);
	if ((qe = queue_entry_read(pathname)) != NULL) {
	    list_add(q, (char *)qe);
	    count++;
	} else
	  queue_entry_delete_files(dp->d_name, qdir);
    }

    closedir(dirp);

    log(L_DEBUG, "read %d queue file from %s", count, qdir);

    return (0);
}

/*
 * This function adds a single queue entry, in the given file,
 * to the current spooler queue.
 */
int queue_add_file(q, qdir, file)
     LIST *q;
     char *qdir;
     char *file;
{
    char pathname[MAXPATHLEN];
    QueueEntry *qe;

    sprintf(pathname, "%s/%s/QF", qdir, file);
    if ((qe = queue_entry_read(pathname)) != NULL) {
	list_add(q, (char *)qe);
	return (0);
    } else
      queue_entry_delete_files(file, qdir);

    return (-1);
}

/*
 * This function deletes a single entry from the fax queue,
 * simply by marking the entry as deleted.
 */
int queue_delete_file(q, file)
     LIST *q;
     char *file;
{
    NODE *q_node = NULL;
    QueueEntry *qe;

    /*
     * See if it is in the queue.
     */
    while ((qe = (QueueEntry *)list_next(q, &q_node)) != NULL) {
	if (strcmp(file, qe->file) == 0) {
	    qe->flags |= QUEUE_F_DELETED;
	    return (0);
	}
    }

    log(L_INFO, "delete failed: %s", file);

    return (-1);
}
