/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* $Header: /home/x_cvs/mit/server/os/WaitFor.c,v 1.6 1992/09/12 08:32:08 dawes Exp $ */
/* $XConsortium: WaitFor.c,v 1.57 92/03/13 15:47:39 rws Exp $ */

/*****************************************************************
 * OS Depedent input routines:
 *
 *  WaitForSomething,  GetEvent
 *
 *****************************************************************/

#include "Xos.h"			/* for strings, fcntl, time */

#include <errno.h>
#include <stdio.h>
#include "X.h"
#include "misc.h"

#include <sys/param.h>
#include <signal.h>
#include "osdep.h"
#include "dixstruct.h"
#include "opaque.h"

extern long AllSockets[];
extern long AllClients[];
extern long LastSelectMask[];
extern long WellKnownConnections;
extern long EnabledDevices[];
extern long ClientsWithInput[];
extern long ClientsWriteBlocked[];
extern long OutputPending[];

extern long ScreenSaverTime;               /* milliseconds */
extern long ScreenSaverInterval;               /* milliseconds */
extern int ConnectionTranslation[];

extern Bool NewOutputPending;
extern Bool AnyClientsWriteBlocked;

extern WorkQueuePtr workQueue;

extern void CheckConnections();
extern Bool EstablishNewConnections();
extern void SaveScreens();
extern void ResetOsBuffers();
extern void ProcessInputEvents();
extern void BlockHandler();
extern void WakeupHandler();

extern int errno;

#ifdef apollo
extern long apInputMask[];

static long LastWriteMask[mskcnt];
#endif

#ifdef XTESTEXT1
/*
 * defined in xtestext1dd.c
 */
extern int playback_on;
#endif /* XTESTEXT1 */

/*****************
 * WaitForSomething:
 *     Make the server suspend until there is
 *	1. data from clients or
 *	2. input events available or
 *	3. ddx notices something of interest (graphics
 *	   queue ready, etc.) or
 *	4. clients that have buffered replies/events are ready
 *
 *     If the time between INPUT events is
 *     greater than ScreenSaverTime, the display is turned off (or
 *     saved, depending on the hardware).  So, WaitForSomething()
 *     has to handle this also (that's why the select() has a timeout.
 *     For more info on ClientsWithInput, see ReadRequestFromClient().
 *     pClientsReady is an array to store ready client->index values into.
 *****************/

static long timeTilFrob = 0;		/* while screen saving */

int
WaitForSomething(pClientsReady)
    int *pClientsReady;
{
    int i;
    struct timeval waittime, *wt;
    long timeout;
    long clientsReadable[mskcnt];
    long clientsWritable[mskcnt];
    long curclient;
    int selecterr;
    int nready;
    long devicesReadable[mskcnt];

    CLEARBITS(clientsReadable);

    /* We need a while loop here to handle 
       crashed connections and the screen saver timeout */
    while (1)
    {
	/* deal with any blocked jobs */
	if (workQueue)
	    ProcessWorkQueue();

	if (ANYSET(ClientsWithInput))
	{
	    COPYBITS(ClientsWithInput, clientsReadable);
	    break;
	}
	if (ScreenSaverTime)
	{
	    timeout = (ScreenSaverTime -
		       (GetTimeInMillis() - lastDeviceEventTime.milliseconds));
	    if (timeout <= 0) /* may be forced by AutoResetServer() */
	    {
		long timeSinceSave;

		timeSinceSave = -timeout;
		if ((timeSinceSave >= timeTilFrob) && (timeTilFrob >= 0))
		{
		    ResetOsBuffers(); /* not ideal, but better than nothing */
		    SaveScreens(SCREEN_SAVER_ON, ScreenSaverActive);
		    if (ScreenSaverInterval)
			/* round up to the next ScreenSaverInterval */
			timeTilFrob = ScreenSaverInterval *
				((timeSinceSave + ScreenSaverInterval) /
					ScreenSaverInterval);
		    else
			timeTilFrob = -1;
		}
		timeout = timeTilFrob - timeSinceSave;
	    }
	    else
	    {
		if (timeout > ScreenSaverTime)
		    timeout = ScreenSaverTime;
		timeTilFrob = 0;
	    }
	    if (timeTilFrob >= 0)
	    {
		waittime.tv_sec = timeout / MILLI_PER_SECOND;
		waittime.tv_usec = (timeout % MILLI_PER_SECOND) *
					(1000000 / MILLI_PER_SECOND);
		wt = &waittime;
	    }
	    else
	    {
		wt = NULL;
	    }
	}
	else
	{
	    wt = NULL;
	}
	COPYBITS(AllSockets, LastSelectMask);
#ifdef apollo
        COPYBITS(apInputMask, LastWriteMask);
#endif
	BlockHandler((pointer)&wt, (pointer)LastSelectMask);
	if (NewOutputPending)
	    FlushAllOutput();
#ifdef XTESTEXT1
	/* XXX how does this interact with new write block handling? */
	if (playback_on) {
	    wt = &waittime;
	    XTestComputeWaitTime (&waittime);
	}
#endif /* XTESTEXT1 */
	/* keep this check close to select() call to minimize race */
	if (dispatchException)
	    i = -1;
	else if (AnyClientsWriteBlocked)
	{
	    COPYBITS(ClientsWriteBlocked, clientsWritable);
	    i = select (MAXSOCKS, (int *)LastSelectMask,
			(int *)clientsWritable, (int *) NULL, wt);
	}
	else
#ifdef apollo
	    i = select (MAXSOCKS, (int *)LastSelectMask,
			(int *)LastWriteMask, (int *) NULL, wt);
#else
	    i = select (MAXSOCKS, (int *)LastSelectMask,
			(int *) NULL, (int *) NULL, wt);
#endif
	selecterr = errno;
	WakeupHandler((unsigned long)i, (pointer)LastSelectMask);
#ifdef XTESTEXT1
	if (playback_on) {
	    i = XTestProcessInputAction (i, &waittime);
	}
#endif /* XTESTEXT1 */
	if (i <= 0) /* An error or timeout occurred */
	{

	    if (dispatchException)
		return 0;
	    CLEARBITS(clientsWritable);
	    if (i < 0) 
		if (selecterr == EBADF)    /* Some client disconnected */
		{
		    CheckConnections ();
		    if (! ANYSET (AllClients))
			return 0;
		}
		else if (selecterr != EINTR)
		    ErrorF("WaitForSomething(): select: errno=%d\n",
			selecterr);
	    if (*checkForInput[0] != *checkForInput[1])
		return 0;
	}
	else
	{
	    if (AnyClientsWriteBlocked && ANYSET (clientsWritable))
	    {
		NewOutputPending = TRUE;
		ORBITS(OutputPending, clientsWritable, OutputPending);
		UNSETBITS(ClientsWriteBlocked, clientsWritable);
		if (! ANYSET(ClientsWriteBlocked))
		    AnyClientsWriteBlocked = FALSE;
	    }

	    MASKANDSETBITS(devicesReadable, LastSelectMask, EnabledDevices);
#ifdef	hpux
		    /* call the HIL driver to gather inputs. 	*/
	    if (ANYSET(devicesReadable)) store_inputs (devicesReadable);
#endif /* hpux */

	    MASKANDSETBITS(clientsReadable, LastSelectMask, AllClients); 
	    if (LastSelectMask[0] & WellKnownConnections) 
		QueueWorkProc(EstablishNewConnections, NULL,
			      (pointer)LastSelectMask[0]);
	    if (ANYSET (devicesReadable) || ANYSET (clientsReadable))
		break;
	}
    }

    nready = 0;
    if (ANYSET(clientsReadable))
    {
	for (i=0; i<mskcnt; i++)
	{
	    while (clientsReadable[i])
	    {
		curclient = ffs (clientsReadable[i]) - 1;
		pClientsReady[nready++] = 
			ConnectionTranslation[curclient + (i << 5)];
		clientsReadable[i] &= ~(1 << curclient);
	    }
	}	
    }
    return nready;
}

#ifndef ANYSET
/*
 * This is not always a macro.
 */
ANYSET(src)
    long	*src;
{
    int i;

    for (i=0; i<mskcnt; i++)
	if (src[ i ])
	    return (TRUE);
    return (FALSE);
}
#endif
