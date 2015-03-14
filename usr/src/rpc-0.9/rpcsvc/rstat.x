%/*
% * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
% * unrestricted use provided that this legend is included on all tape
% * media and as a part of the software program in whole or part.  Users
% * may copy or modify Sun RPC without charge, but are not authorized
% * to license or distribute it to anyone else except as part of a product or
% * program developed by the user or with the express written consent of
% * Sun Microsystems, Inc.
% *
% * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
% * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
% * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
% *
% * Sun RPC is provided with no support and without any obligation on the
% * part of Sun Microsystems, Inc. to assist in its use, correction,
% * modification or enhancement.
% *
% * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
% * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
% * OR ANY PART THEREOF.
% *
% * In no event will Sun Microsystems, Inc. be liable for any lost revenue
% * or profits or other special, indirect and consequential damages, even if
% * Sun has been advised of the possibility of such damages.
% *
% * Sun Microsystems, Inc.
% * 2550 Garcia Avenue
% * Mountain View, California  94043
% */

%/*
% * Copyright (c) 1985, 1990 by Sun Microsystems, Inc.
% */
%
%/* from @(#)rstat.x	1.9 91/03/11 TIRPC 1.0 */

#ifdef RPC_HDR
%
%#ifndef _rpcsvc_rstat_h
%#define _rpcsvc_rstat_h
%
%#ifndef FSCALE
%/*
% * Scale factor for scaled integers used to count load averages.
% */
%#define FSHIFT  8               /* bits to right of fixed binary point */
%#define FSCALE  (1<<FSHIFT)
%
%#endif /* ndef FSCALE */

%#ifndef DST_NONE
%#include <sys/time.h>          /* The time struct defined below is meant to */
%#endif                         /* match struct timeval.                     */
%
%

#elif RPC_SVC
%
%/*  Server side stub routines for the rstat daemon
% */
%
#elif RPC_XDR
%/*
% * XDR routines for the rstat daemon, rup and perfmeter.
% */
%
#endif

const RSTAT_CPUSTATES = 4;
const RSTAT_DK_NDRIVE = 4;

/*
 * GMT since 0:00, January 1, 1970
 */
struct rstat_timeval {
      unsigned int tv_sec;    /* seconds */
      unsigned int tv_usec;   /* and microseconds */
};


struct statsvar {				/* version 4 */
	int cp_time<>; 		/* variable number of CPU states */
	int dk_xfer<>;		/* variable number of disks */
	unsigned v_pgpgin;	/* these are cumulative sum */
	unsigned v_pgpgout;
	unsigned v_pswpin;
	unsigned v_pswpout;
	unsigned v_intr;
	int if_ipackets;
	int if_ierrors;
	int if_opackets;
	int if_oerrors;
	int if_collisions;
	unsigned v_swtch;
	long avenrun[3];
	rstat_timeval boottime;
	rstat_timeval curtime;
};

struct statstime {				/* RSTATVERS_TIME */
	int cp_time[RSTAT_CPUSTATES];
	int dk_xfer[RSTAT_DK_NDRIVE];
	unsigned int v_pgpgin;	/* these are cumulative sum */
	unsigned int v_pgpgout;
	unsigned int v_pswpin;
	unsigned int v_pswpout;
	unsigned int v_intr;
	int if_ipackets;
	int if_ierrors;
	int if_oerrors;
	int if_collisions;
	unsigned int v_swtch;
	int avenrun[3];
	rstat_timeval boottime;
	rstat_timeval curtime;
	int if_opackets;
};

struct statsswtch {			/* RSTATVERS_SWTCH */
	int cp_time[RSTAT_CPUSTATES];
	int dk_xfer[RSTAT_DK_NDRIVE];
	unsigned int v_pgpgin;	/* these are cumulative sum */
	unsigned int v_pgpgout;
	unsigned int v_pswpin;
	unsigned int v_pswpout;
	unsigned int v_intr;
	int if_ipackets;
	int if_ierrors;
	int if_oerrors;
	int if_collisions;
	unsigned int v_swtch;
	unsigned int avenrun[3];
	rstat_timeval boottime;
	int if_opackets;
};

struct stats {				/* RSTATVERS_ORIG */
	int cp_time[RSTAT_CPUSTATES];
	int dk_xfer[RSTAT_DK_NDRIVE];
	unsigned int v_pgpgin;	/* these are cumulative sum */
	unsigned int v_pgpgout;
	unsigned int v_pswpin;
	unsigned int v_pswpout;
	unsigned int v_intr;
	int if_ipackets;
	int if_ierrors;
	int if_oerrors;
	int if_collisions;
	int if_opackets;
};

program RSTATPROG {
        /*
         * Version 4 allows for variable number of disk and RSTAT_CPU states.
         */
	version RSTATVERS_VAR {
		statsvar
		RSTATPROC_STATS (void) = 1;
		unsigned int
		RSTATPROC_HAVEDISK (void) = 2;
	} = 4;
	/*
	 * Version 3 includes current time and context switching info
	 */
	version RSTATVERS_TIME {
		statstime
		RSTATPROC_STATS(void) = 1;

		unsigned int
		RSTATPROC_HAVEDISK(void) = 2;
	} = 3;
	/*
	 * Does not have current time
	 */
	version RSTATVERS_SWTCH {
		statsswtch
		RSTATPROC_STATS(void) = 1;

		unsigned int
		RSTATPROC_HAVEDISK(void) = 2;
	} = 2;
	/*
	 * Original version has no info about current time or context switching
	 */
	version RSTATVERS_ORIG {
		stats
		RSTATPROC_STATS(void) = 1;

		unsigned int
		RSTATPROC_HAVEDISK(void) = 2;
	} = 1;
} = 100001;

#ifdef RPC_HDR
%int havedisk();
%
%#endif /*!_rpcsvc_rstat_h*/
#endif
