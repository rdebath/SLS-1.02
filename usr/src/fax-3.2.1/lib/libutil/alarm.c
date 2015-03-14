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
#include <sys/time.h>

#include "alloc.h"
#include "list.h"
#include "alarm.h"

#define ALARM_GRANULARITY 200			/* in msecs */

static int granularity = ALARM_GRANULARITY;

typedef struct _alarm_event {
    int rate;
    int count;
    int (*routine)();
    char *data;
} alarm_event;

static LIST *event_list = NULL;

int
alarm_add_event(routine, interval, data)
int (*routine)();
float interval;
char *data;
{
    alarm_event *event;
    int cfree();

    if ((event = salloc(1, alarm_event)) == NULL)
      return (-1);

    /* setup event info */
    event->rate = (int)((interval * 1000) / granularity);
    event->routine = routine;
    event->data = data;

    if (event_list == NULL)
      event_list = list_make(NULL, cfree);

    /* append to list of all alarms */
    list_add(event_list, (char *)event);

    return (0);
}

static alarm_event *
find_routine(event, routine)
alarm_event *event;
int (*routine)();
{
    if (event->routine == routine)
      return (event);
    else
      return (NULL);
}

int
alarm_delete_event(routine)
int (*routine)();
{
    alarm_event *event;

    /* find event for the routine */
    event = (alarm_event *)list_map(event_list, find_routine, routine);

    if (event == NULL)
      return (-1);

    list_delete(event_list, (char *)event);

    return (0);
}

static int
do_update(event)
alarm_event *event;
{
    ++(event->count);

    /* run event when rate count exceeded */
    if (event->count >= event->rate) {

	/* reset the rate counter */
	event->count = 0;

	/* run the event */
	(*event->routine)(event->data, event->rate);
    }

    return (0);
}

static
alarm_handler()
{
    /* update and run each event */
    list_map(event_list, do_update);
}

int
alarm_block()
{
    return (sigblock(sigmask(SIGALRM)));
}

int
alarm_restore(mask)
int mask;
{
    return (sigsetmask(mask));
}

int
alarm_start()
{
    struct itimerval value;

    if (event_list == NULL)
      event_list = list_make(NULL, cfree);

    /*SUPPRESS 544*/
    if (signal(SIGALRM, alarm_handler) < 0)
      return (-1);

    value.it_value.tv_sec  = value.it_interval.tv_sec  = 0;
    value.it_value.tv_usec = value.it_interval.tv_usec = granularity*1000;

    if (setitimer(ITIMER_REAL, &value, NULL) < 0)
      return (-1);

    return (0);
}
