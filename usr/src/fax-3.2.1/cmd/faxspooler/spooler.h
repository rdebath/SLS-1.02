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

#ifndef in_spooler_h
#define in_spooler_h 1

#include <sys/types.h>
#include <sys/time.h>

#include "../../lib/libutil/list.h"

#define MAX_USER_LEN 	  16
#define MAX_FILENAME_LEN  128
#define MAX_PHONE_LEN	  128

#define RECIP_F_SENT	  00000001	/* fax was sent			*/
#define RECIP_F_DIALER 	  00000002	/* dialer failure, code valid	*/
#define RECIP_F_FAILED	  00000004	/* other send failure occured	*/

typedef enum {
    SEND_STATUS_PENDING,		/* fax hasn't been sent yet	*/
    SEND_STATUS_SENDING,		/* fax is being sent		*/
    SEND_STATUS_SENT,			/* fax was sent			*/
    SEND_STATUS_DIALER,			/* dialer failure, code valid	*/
    SEND_STATUS_FAILED,			/* other send failure occured	*/
    SEND_STATUS_GIVEUP,			/* given up on trying to send	*/
} send_status;

typedef struct _recip {
    send_status status;			/* status status		*/
    int	     dialer_code;		/* dialer failure code		*/
    char     phone[MAX_PHONE_LEN];	/* phone number of recipient	*/
    int	     attempts;			/* number of delivery attempts  */
    time_t   time_first;		/* time of first delivery try	*/
    time_t   time_last;			/* time of last delivery try	*/
    time_t   total_time;		/* total connect time for job	*/
} Recip;

#define QUEUE_F_USER	  00000001	/* got user field		*/
#define QUEUE_F_EMAIL	  00000002	/* email delivery notification	*/
#define QUEUE_F_RECIP	  00000004	/* got at least one receipient	*/
#define QUEUE_F_RETRY	  00000010	/* retry interval		*/
#define QUEUE_F_TIME	  00000020	/* last time any delivery tried	*/
#define QUEUE_F_PAGES	  00000040	/* got pages field		*/
#define QUEUE_F_FILE	  00000100	/* got file field		*/
#define QUEUE_F_DELETED	  00000200	/* entry has been deleted	*/
#define QUEUE_F_RUNNING	  00000400	/* a recip is being run		*/

typedef struct _queue_entry {
    int	     flags;			/* status flags			*/
    char     file[MAX_FILENAME_LEN];	/* queue base filename		*/
    char     user[MAX_USER_LEN];	/* computer username of sender	*/
    int	     retry;			/* retry interval, in seconds	*/
    int	     pages;			/* total pages to send		*/
    time_t   time;			/* time of last delivery	*/
    LIST *   recip_list;		/* list of recipients		*/
} QueueEntry;

typedef struct _spooler_data {
    int numb_fm;		/* number of faxmodems in system	*/
    FaxModem *fmp;		/* pointer to data for the modems	*/
    int *fm_busy;		/* list of modem busy flags		*/
    LIST *q;			/* spooler job queue			*/
    char *out_qdir;		/* outgoing queue directory		*/
    char *in_qdir;		/* incoming qeuue directory		*/
    char *in_email;		/* email addr for recv notification	*/
    NODE *q_node;		/* current queue node being processed	*/
    NODE *r_node;		/* current recip nodebeing processed	*/
    QueueEntry *qe;		/* current queue entry			*/
    Recip *r;			/* current recipient			*/
    int recips_left;		/* number of recips in cur entry left	*/
    int max_delivery_time;	/* how long to try delivery faxes	*/
    int min_retry_wait;		/* how long to wait between tries	*/
} spooler_data;

#endif
