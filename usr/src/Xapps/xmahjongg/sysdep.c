/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#ifndef	VMS

#include <pwd.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "xmahjongg.h"
#include "variables.h"

get_user(name)
char	*name;
{
	struct passwd *pwp;

	if ((pwp = getpwuid(getuid())) == NULL) {
		fprintf(stderr, "can't getpwuid\n");
		exit(1);
	} else {
		strcpy(name, pwp->pw_name);
	};

	return(0);
}

get_seed()
{
	struct timeval tv;
	struct timezone tz;

	if (seed == 0) {
		gettimeofday(&tv, &tz);
		seed = 1 + (tv.tv_sec%100000);
	} else if (seed < 0) {
		seed = (-seed)%100000;
	} else {
		seed = 1 + (random_next(seed)%100000);
	};

	return(0);
}

event_wait()
{
	int readfds;

/*
 *	If there are no events pending, then sleep until one occurs.
 */
	while (XEventsQueued(XGameDisplay, QueuedAfterReading) == 0) {
		readfds = (1 << XGameFD) | playfds;
		select(maxfds, &readfds, NULL, NULL, NULL);

		if ((readfds & playfds) != 0) {
			event_packet(readfds);
		};
	};

	return(0);
}

#else

#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "xmahjongg.h"
#include "variables.h"


get_user(name)
char	*name;
{

	if (cuserid(name) == NULL) {
		fprintf(stderr, "can't cuserid\n");
		exit(1);
	};

	return(0);
}

get_seed()
{
	struct tm *tv;
	time_t clock_time;

	if (seed == 0) {
		time(&clock_time);
		tv = localtime(&clock_time);
		seed = 1 + (tv->tm_sec%100000);
	} else if (seed < 0) {
		seed = (-seed)%100000;
	} else {
		seed = 1 + (random_next(seed)%100000);
	};

	return(0);
}

event_wait()
{

/*
 *	Since VMS doesn't have tournament mode, this is really a dummy 
 *	routine.
 */

	return(0);
}

#endif
