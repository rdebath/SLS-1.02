/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
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
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <rpc/rpc.h>

#include "alloc.h"
#include "list.h"
#include "tv.h"
#include "hash.h"
#include "dispatch.h"

#define DIO_MASK_READ   0
#define DIO_MASK_WRITE  1
#define DIO_MASK_EXCEPT 2
#define DIO_MASKS    	3

struct dqueue {
    int priority;
    LIST *dio_list;
};

struct fd_map_key {
    int fd;
    int dtype;
};

static LIST *dqueue_list;
static LIST *alarm_list;
static HTTABLE *fd_map;
static fd_set dispatch_masks[DIO_MASKS];
static int recompute_timeout;

static int
dqueue_cmp(v1, v2)
struct dqueue *v1, *v2;
{
    return (v2->priority - v1->priority);
}

static fd_set *
fd_or(fdset1, fdset2)
fd_set *fdset1;
fd_set *fdset2;
{
    int i;

    for (i=0; i<howmany(FD_SETSIZE, NFDBITS); i++)
      fdset1->fds_bits[i] |= fdset2->fds_bits[i];

    return (fdset1);
}

static fd_set *
fd_and(fdset1, fdset2)
fd_set *fdset1;
fd_set *fdset2;
{
    int i;

    for (i=0; i<howmany(FD_SETSIZE, NFDBITS); i++)
      fdset1->fds_bits[i] &= fdset2->fds_bits[i];

    return (fdset1);
}

static int
fd_any(fdset)
fd_set *fdset;
{
    int i;
    
    for (i=0; i<howmany(FD_SETSIZE, NFDBITS); i++)
      if (fdset->fds_bits[i] != 0)
	return (TRUE);

    return (FALSE);
}

static int
init()
{
    int dtbsz = getdtablesize();
    int cfree();

    if (dqueue_list == NULL)
      if ((dqueue_list = list_make(dqueue_cmp, NULL)) == NULL)
	return (-1);

    if (alarm_list == NULL)
      if ((alarm_list = list_make(NULL, NULL)) == NULL)
	return (-1);

    if (fd_map == NULL)
      if ((fd_map = htinit(dtbsz, sizeof(struct fd_map_key))) == NULL)
	return (-1);

    return (0);
}

static struct dqueue *
get_dq(priority)
int priority;
{
    struct dqueue dqi, *dq;

    dqi.priority = priority;
    if ((dq = (struct dqueue *)list_find(dqueue_list, (char *)&dqi)) == NULL) {
	if ((dq = salloc(1, struct dqueue)) == NULL)
	  return (NULL);
	dq->priority = priority;
	if ((dq->dio_list = list_make(NULL, NULL)) == NULL)
	  return (NULL);
	list_insert(dqueue_list, (char *)dq, LIST_ASCENDING);
    }

    return (dq);
}

static DIO *
get_dio(dtype, fd)
int dtype;
int fd;
{
    struct fd_map_key key;
    if (fd_map == NULL)
      return (NULL);

    key.dtype = DIO_FD_EVENT(dtype);
    key.fd = fd;

    return ((DIO *)htgetdata((char *)&key, fd_map));
}

int
dispatch_unregister(d)
DIO *d;
{
    struct fd_map_key key;

    if (DIO_FD_EVENT(d->dtype)) {
	key.dtype = DIO_FD_EVENT(d->dtype);
	key.fd = d->fd;
	htdelete((char *)&key, fd_map);
	FD_CLR(d->fd, &dispatch_masks[DIO_FD_MASK(d->dtype)]);
    }

    d->dtype |= DIO_DELETED;
    
    return (0);
}

static DIO *
add_dio(d, priority)
DIO *d;
int priority;
{
    struct fd_map_key key;
    struct dqueue *dq;

    if (init() < 0)
      return (NULL);

    if ((dq = get_dq(priority)) == NULL)
      return (NULL);

    list_add(dq->dio_list, (char *)d);

    if (DIO_FD_EVENT(d->dtype)) {
	key.dtype = DIO_FD_EVENT(d->dtype);
	key.fd = d->fd;
	htadd((char *)&key, fd_map, (char *)d);
	FD_SET(d->fd, &dispatch_masks[DIO_FD_MASK(d->dtype)]);
    }

    if (d->dtype & DIO_TIMEOUT)
      list_add(alarm_list, (char *)d);

    return (d);
}

static DIO *
register_generic(dtype, priority, fd, tp, routine, data)
int dtype;
int priority;
int fd;
struct timeval *tp;
int (*routine)();
char *data;
{
    struct timeval now;
    DIO *d;

    if (DIO_FD_EVENT(dtype))
      if ((d = get_dio(dtype, fd)) != NULL)
	dispatch_unregister(d);

    if ((d = salloc(1, DIO)) == NULL)
      return (NULL);

    d->dtype = dtype;
    d->fd = fd;
    d->routine = routine;
    d->data = data;

    if (dtype & DIO_TIMEOUT) {
	d->time_interval = *tp;
	d->time_next = *tp;
	tv_add(&d->time_next, tv_current(&now));
	recompute_timeout = TRUE;
    }

    if (add_dio(d, priority) == NULL) {
	cfree(d);
	return (NULL);
    }

    return (d);
}

DIO *
dispatch_register_fd(dtype, priority, fd, routine, data)
int dtype;
int priority;
int fd;
int (*routine)();
char *data;
{
    if (!DIO_FD_EVENT(dtype))
      return (NULL);

    dtype |= DIO_REPEAT;

    return (register_generic(dtype, priority, fd, NULL, routine, data));
}

DIO *
dispatch_register_fd_timeout(dtype, priority, fd, timeout, routine, data)
int dtype;
int priority;
int fd;
double timeout;
int (*routine)();
char *data;
{
    struct timeval tv;

    if (!DIO_FD_EVENT(dtype))
      return (NULL);

    dtype |= (DIO_TIMEOUT|DIO_REPEAT);

    tv_period_to_tv(timeout, &tv);

    return (register_generic(dtype, priority, fd, &tv, routine, data));
}

DIO *
dispatch_register_alarm(priority, period, routine, data)
int priority;
double period;
int (*routine)();
char *data;
{
    struct timeval tv;

    tv_period_to_tv(period, &tv);

    return (register_generic(DIO_TIMEOUT, priority, -1, &tv, routine, data));
}

DIO *
dispatch_register_periodic(priority, period, routine, data)
int priority;
double period;
int (*routine)();
char *data;
{
    struct timeval tv;

    tv_period_to_tv(period, &tv);

    return (register_generic(DIO_TIMEOUT|DIO_REPEAT, priority, -1, &tv,
			     routine, data));
}

DIO *
dispatch_register_rpc(priority)
int priority;
{
    return (register_generic(DIO_RPC, priority, -1, NULL, NULL, NULL));
}

int
dispatch_unregister_fd(dtype, fd)
int dtype;
int fd;
{
    DIO *d;

    if ((d = get_dio(dtype, fd)) == NULL)
      return (-1);

    return (dispatch_unregister(d));
}

DIO *
dispatch_change_handler(d, routine)
DIO *d;
int (*routine)();
{
    d->routine = routine;
    
    return (d);
}

DIO *
dispatch_change_priority(d, priority)
DIO *d;
int priority;
{
    DIO *new;

    if ((new = salloc(1, DIO)) == NULL)
      return (NULL);

    /* copy the stuff over */
    *new = *d;

    if (dispatch_unregister(d) < 0) {
	cfree(new);
	return (NULL);
    }

    if (add_dio(d, priority) == NULL) {
	cfree(new);
	return (NULL);
    }

    return (new);
}

int
dispatch_print_queues()
{
    NODE *dq_node = NULL;
    NODE *d_node;
    struct dqueue *dq;
    DIO *d;

    if (dqueue_list == NULL)
      return (0);

    while ((dq = (struct dqueue *)list_next(dqueue_list, &dq_node)) != NULL) {
	if (dq->dio_list != NULL) {
	    printf("priority: %d\n", dq->priority);
	    d_node = NULL;
	    while ((d = (DIO *)list_next(dq->dio_list, &d_node)) != NULL) {
		printf("  ");
		if (d->dtype & DIO_READ)
		  printf("read ");
		if (d->dtype & DIO_WRITE)
		  printf("write ");
		if (d->dtype & DIO_EXCEPT)
		  printf("except ");
		if (d->dtype & DIO_TIMEOUT)
		  printf("timeout ");
		if (d->dtype & DIO_REPEAT)
		  printf("repeat ");
		if (d->dtype & DIO_DELETED)
		  printf("deleted ");
		if (d->dtype & DIO_RPC)
		  printf("rpc ");
		printf("fd=%d, next=%d %d, int=%d %d\n",
		       d->fd,
		       d->time_next.tv_sec, d->time_next.tv_usec,
		       d->time_interval.tv_sec, d->time_interval.tv_usec);
	    }
	}
    }
    
    return (0);
}

int
dispatch_run()
{
    int dtbsz = getdtablesize();
    fd_set masks[DIO_MASKS];
    NODE *d_node, *dq_node;
    struct dqueue *dq;
    struct timeval time_current, time_next, time_zero, time_timeout;
    struct timeval *timeout;
    DIO *d;
    int events;
    int count;
    int i;

    recompute_timeout = TRUE;

    tv_zero(&time_zero);

    for (;;) {
      reselect:
	for (i=0; i<DIO_MASKS; i++)
	  masks[i] = dispatch_masks[i];
	
	/* add in the rpc svc set */
	fd_or(&masks[DIO_MASK_READ], &svc_fdset);

	/* get alarm wait period */
	if (recompute_timeout) {
	    tv_zero(&time_next);
	    if (alarm_list != NULL) {
		d_node = NULL;
		while ((d = (DIO *)list_next(alarm_list, &d_node)) != NULL) {
		    if (tv_iszero(&time_next)) {
			time_next = d->time_next;
		    } else {
			if (tv_cmp(&d->time_next, &time_next) < 0)
			  time_next = d->time_next;
		    }
		}
	    }
	    recompute_timeout = FALSE;
	}

	if (tv_iszero(&time_next))
	  timeout = NULL;
	else {
	    if (tv_current(&time_current) == NULL)
	      return (-1);
	    time_timeout = time_next;
	    tv_subtract(&time_timeout, &time_current);
	    if (tv_cmp(&time_timeout, &time_zero) < 0)
	      timeout = &time_zero;
	    else
	      timeout = &time_timeout;
	}

	count = select(dtbsz, &masks[DIO_MASK_READ], &masks[DIO_MASK_WRITE],
		       &masks[DIO_MASK_EXCEPT], timeout);
	if (count < 0) {
	    /* system call interrupted us */
	    if (errno == EINTR)
	      continue;
	    else
	      return (-1);
	}

	if (tv_current(&time_current) == NULL)
	  return (-1);

	dq_node = NULL; events = 0;
	if (dqueue_list != NULL)
	  while ((dq = (struct dqueue *)list_next(dqueue_list, &dq_node)) 
		 != NULL){
	      if (dq->dio_list != NULL) {
		  d_node = NULL;
		  d = (DIO *)list_next(dq->dio_list, &d_node);
		  while (d) {
		      int delete = FALSE; 
		      int timeout_changed = FALSE;
		      if (d->dtype & DIO_DELETED) {
			  delete = TRUE;
		      } else if (DIO_FD_EVENT(d->dtype) && 
				 (FD_ISSET(d->fd, 
					   &masks[DIO_FD_MASK(d->dtype)]))) {
			  if (d->routine != NULL)
			    (*(d->routine))(d->fd, d->data,
					    DIO_FD_EVENT(d->dtype), d);
			  events++;
			  count--;
			  if (d->dtype & DIO_TIMEOUT)
			    timeout_changed = TRUE;
			  if (!(d->dtype & DIO_REPEAT))
			    delete = TRUE;
			  /* make sure we don't run again, if re-added */
			  FD_CLR(d->fd, &masks[DIO_FD_MASK(d->dtype)]);
		      } else if (d->dtype & DIO_TIMEOUT) {
#ifdef DEBUG
			  printf("checking for %s\n", d->data);
#endif
			  if (tv_cmp(&d->time_next, &time_current) <= 0) {
#ifdef DEBUG
			      printf("  running %s\n", d->data);
#endif
			      if (d->routine != NULL) {
				  if (DIO_FD_EVENT(d->dtype))
				    (*(d->routine))(d->fd, d->data,
						    DIO_TIMEOUT|
						    DIO_FD_EVENT(d->dtype), d);
				  else
				    (*(d->routine))(d->data, d);
			      }
			      if (!(d->dtype & DIO_REPEAT))
				delete = TRUE;
			      timeout_changed = TRUE;
			  }
		      } else if (d->dtype & DIO_RPC) {
			  fd_set mask;
			  mask = masks[DIO_MASK_READ];
			  fd_and(&mask, &svc_fdset);
			  if (fd_any(&mask))
			    svc_getreqset(&mask);
		      }
		      if (delete) {
			  DIO *hold = d;
#ifdef DEBUG
			  printf("deleting %s\n", d->data);
#endif
			  if (d->dtype & DIO_TIMEOUT) {
			      list_delete(alarm_list, (char *)d);
			      recompute_timeout = TRUE;
			  }
			  d = (DIO *)list_delete_next(dq->dio_list, &d_node);
			  cfree((char *)hold);
		      } else {
			  if (timeout_changed) {
			      d->time_next = time_current;
			      tv_add(&d->time_next, &d->time_interval);
			      recompute_timeout = TRUE;
			  }
			  d = (DIO *)list_next(dq->dio_list, &d_node);
#ifdef DEBUG
			  if (d == NULL)
			    printf("\n");
#endif
		      }

		  }
		  if (events > 0)
		    goto reselect;
	      }
	  }
    }
}

#ifdef DEBUG
DIO *p, *t;
int fd;

handler(data)
char *data;
{
    printf("foo %s\n", data);
}

/*ARGSUSED*/
terminator(data)
char *data;
{
    printf("bar %s\n", data);
    dispatch_unregister(p);
}

timeout(fd, data)
int fd;
char *data;
{
    printf("timeout %d %s\n", fd, data);
}

#include <sys/file.h>

dispatch_test()
{
    dqueue_list = NULL;

    if ((fd = open("/dev/tty", O_RDONLY)) < 0)
      return;

    p = dispatch_register_periodic(0, 5.0, handler, "periodic");
    dispatch_register_alarm(0, 10.0, handler, "10 seconds");
    dispatch_register_alarm(0, 30.0, handler, "30 seconds");
    dispatch_register_alarm(0, 40.0, terminator, "40 seconds");
    t = dispatch_register_fd_timeout(DIO_EXCEPT, 0, fd, 5.0, timeout, "timeout test");

    return (dispatch_run());
}
#endif
