/* $Header: /home/x_cvs/mit/lib/Xt/NextEvent.c,v 1.3 1992/09/16 14:51:20 dawes Exp $ */
/* $XConsortium: NextEvent.c,v 1.108 91/07/12 11:00:21 rws Exp $ */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
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

#include "IntrinsicI.h"
#include <stdio.h>
#include <errno.h>

extern int errno;

static TimerEventRec* freeTimerRecs;
static WorkProcRec* freeWorkRecs;

#undef MIN
#undef MAX
#ifndef OPEN_MAX
#ifndef NOFILE
#include <sys/param.h>
#endif
#ifdef NOFILE
#define OPEN_MAX NOFILE
#endif
#endif

/* Some systems running NTP daemons are known to return strange usec
 * values from gettimeofday.  At present (3/90) this has only been
 * reported on SunOS...
 */

#ifndef NEEDS_NTPD_FIXUP
# if defined(sun) || defined(MOTOROLA)
#  define NEEDS_NTPD_FIXUP 1
# else
#  define NEEDS_NTPD_FIXUP 0
# endif
#endif

#if NEEDS_NTPD_FIXUP
#define FIXUP_TIMEVAL(t) { \
	while ((t).tv_usec >= 1000000) { \
	    (t).tv_usec -= 1000000; \
	    (t).tv_sec++; \
	} \
	while ((t).tv_usec < 0) { \
	    if ((t).tv_sec > 0) { \
		(t).tv_usec += 1000000; \
		(t).tv_sec--; \
	    } else { \
		(t).tv_usec = 0; \
		break; \
	    } \
	}}
#else
#define FIXUP_TIMEVAL(t)
#endif /*NEEDS_NTPD_FIXUP*/



/*
 * Private routines
 */
#define ADD_TIME(dest, src1, src2) { \
	if(((dest).tv_usec = (src1).tv_usec + (src2).tv_usec) >= 1000000) {\
	      (dest).tv_usec -= 1000000;\
	      (dest).tv_sec = (src1).tv_sec + (src2).tv_sec + 1 ; \
	} else { (dest).tv_sec = (src1).tv_sec + (src2).tv_sec ; \
	   if(((dest).tv_sec >= 1) && (((dest).tv_usec <0))) { \
	    (dest).tv_sec --;(dest).tv_usec += 1000000; } } }


#define TIMEDELTA(dest, src1, src2) { \
	if(((dest).tv_usec = (src1).tv_usec - (src2).tv_usec) < 0) {\
	      (dest).tv_usec += 1000000;\
	      (dest).tv_sec = (src1).tv_sec - (src2).tv_sec - 1;\
	} else 	(dest).tv_sec = (src1).tv_sec - (src2).tv_sec;  }

#define IS_AFTER(t1, t2) (((t2).tv_sec > (t1).tv_sec) \
	|| (((t2).tv_sec == (t1).tv_sec)&& ((t2).tv_usec > (t1).tv_usec)))

#define IS_AT_OR_AFTER(t1, t2) (((t2).tv_sec > (t1).tv_sec) \
	|| (((t2).tv_sec == (t1).tv_sec)&& ((t2).tv_usec >= (t1).tv_usec)))

static void QueueTimerEvent(app, ptr)
    XtAppContext app;
    TimerEventRec *ptr;
{
        TimerEventRec *t,**tt;
        tt = &app->timerQueue;
        t  = *tt;
        while (t != NULL &&
                IS_AFTER(t->te_timer_value, ptr->te_timer_value)) {
          tt = &t->te_next;
          t  = *tt;
         }
         ptr->te_next = t;
         *tt = ptr;
}

/* 
 * Routine to block in the toolkit.  This should be the only call to select.
 *
 * This routine returns when there is something to be done.
 *
 * Before calling this with ignoreInputs==False, app->outstandingQueue should
 * be checked; this routine will not verify that an alternate input source
 * has not already been enqueued.
 *
 *
 * _XtwaitForSomething( ignoreTimers, ignoreInputs, ignoreEvents,
 *			block, howlong, appContext)
 * Boolean ignoreTimers;     (Don't return if a timer would fire
 *				Also implies forget timers exist)
 *
 * Boolean ignoreInputs;     (Ditto for input callbacks )
 *
 * Boolean ignoreEvents;     (Ditto for X events)
 *
 * Boolean block;	     (Okay to block)
 * TimeVal howlong;	     (howlong to wait for if blocking and not
 *				doing Timers... Null mean forever.
 *				Maybe should mean shortest of both)
 * XtAppContext app;	     (Displays to check wait on)
 * Returns display for which input is available, if any
 * and if ignoreEvents==False, else returns -1
 *
 * if ignoring everything && block=True && howlong=NULL, you'll have
 * lots of time for coffee; better not try it!  In fact, it probably
 * makes little sense to do this regardless of the value of howlong
 * (bottom line is, we don't bother checking here).
 */
#if NeedFunctionPrototypes
int _XtwaitForSomething(
	_XtBoolean ignoreTimers,
	_XtBoolean ignoreInputs,
	_XtBoolean ignoreEvents,
	_XtBoolean block,
	unsigned long *howlong,
	XtAppContext app
        )
#else
int _XtwaitForSomething(ignoreTimers, ignoreInputs, ignoreEvents,
			block, howlong, app)
	Boolean ignoreTimers;
	Boolean ignoreInputs;
	Boolean ignoreEvents;
	Boolean block;
	unsigned long *howlong;
	XtAppContext app;
#endif
{
	struct timeval  cur_time;
	struct timeval  start_time;
	struct timeval  wait_time;
	struct timeval  new_time;
	struct timeval  time_spent;
	struct timeval	max_wait_time;
	static struct timeval  zero_time = { 0 , 0};
	register struct timeval *wait_time_ptr;
	Fd_set rmaskfd, wmaskfd, emaskfd;
	static Fd_set zero_fd = { 0 };
	int nfound, i, d;
	
 	if (block) {
		(void) gettimeofday (&cur_time, NULL);
		FIXUP_TIMEVAL(cur_time);
		start_time = cur_time;
		if(howlong == NULL) { /* special case for ever */
			wait_time_ptr = 0;
		} else { /* block until at most */
			max_wait_time.tv_sec = *howlong/1000;
			max_wait_time.tv_usec = (*howlong %1000)*1000;
			wait_time_ptr = &max_wait_time;
		}
	} else {  /* don't block */
		max_wait_time = zero_time;
		wait_time_ptr = &max_wait_time;
	}

      WaitLoop:
	while (1) {
		if (app->timerQueue != NULL && !ignoreTimers && block) {
		    if(IS_AFTER(cur_time, app->timerQueue->te_timer_value)) {
			TIMEDELTA (wait_time, app->timerQueue->te_timer_value, 
				   cur_time);
			if(howlong==NULL || IS_AFTER(wait_time,max_wait_time)){
				wait_time_ptr = &wait_time;
			} else {
				wait_time_ptr = &max_wait_time;
			}
		    } else wait_time_ptr = &zero_time;
		} 
		if( !ignoreInputs ) {
			rmaskfd = app->fds.rmask;
			wmaskfd = app->fds.wmask;
			emaskfd = app->fds.emask;
		} else {
			rmaskfd = zero_fd;
			wmaskfd = zero_fd;
			emaskfd = zero_fd;
		}
		if (!ignoreEvents) {
		    for (d = 0; d < app->count; d++) {
			FD_SET (ConnectionNumber(app->list[d]), (fd_set *)&rmaskfd);
		    }
		}
		nfound = select (app->fds.nfds, (int *) &rmaskfd,
			(int *) &wmaskfd, (int *) &emaskfd, wait_time_ptr);
		if (nfound == -1) {
			/*
			 *  interrupt occured recalculate time value and select
			 *  again.
			 */
			if (errno == EINTR) {
			    errno = 0;  /* errno is not self reseting */
			    if (block) {
				if (wait_time_ptr == NULL) /*howlong == NULL*/
				    continue;
				(void)gettimeofday (&new_time, NULL);
				FIXUP_TIMEVAL(new_time);
				TIMEDELTA(time_spent, new_time, cur_time);
				cur_time = new_time;
				if(IS_AFTER(time_spent, *wait_time_ptr)) {
					TIMEDELTA(wait_time, *wait_time_ptr,
						  time_spent);
					wait_time_ptr = &wait_time;
					continue;
				} else {
					/* time is up anyway */
					nfound = 0;
				}
			    }
			} else {
			    char Errno[12];
			    String param = Errno;
			    Cardinal param_count = 1;

			    if (!ignoreEvents) {
				/* get Xlib to detect a bad connection */
				for (d = 0; d < app->count; d++) {
				    if (XEventsQueued(app->list[d],
						      QueuedAfterReading))
					return d;
				}
			    }
			    sprintf( Errno, "%d", errno);
			    XtAppWarningMsg(app, "communicationError","select",
			       XtCXtToolkitError,"Select failed; error code %s",
			       &param, &param_count);
			    continue;
			}
		} /* timed out or input available */
		break;
	}
	
	if (nfound == 0) {
		if(howlong) *howlong = (unsigned long)0;  /* Timed out */
		return -1;
	}
	if(block && howlong != NULL) { /* adjust howlong */
	    (void) gettimeofday (&new_time, NULL);
	    FIXUP_TIMEVAL(new_time);
	    TIMEDELTA(time_spent, new_time, start_time);
	    if(*howlong <= (time_spent.tv_sec*1000+time_spent.tv_usec/1000))
		*howlong = (unsigned long)0;  /* Timed out */
	    else
		*howlong -= (time_spent.tv_sec*1000+time_spent.tv_usec/1000);
	}
	if(ignoreInputs) {
	    if (ignoreEvents) return -1; /* then only doing timers */
	    for (d = 0; d < app->count; d++) {
		if (FD_ISSET(ConnectionNumber(app->list[d]), (fd_set *)&rmaskfd)) {
		    if (XEventsQueued( app->list[d], QueuedAfterReading ))
			return d;
		    /*
		     * An error event could have arrived
		     * without any real events, or events
		     * could have been swallowed by Xlib,
		     * or the connection may be broken.
		     * We can't tell the difference, so
		     * ssume Xlib will eventually discover
		     * a broken connection.
		     */
		}
	    }
	    goto WaitLoop;	/* must have been only error events */
        }
	{
	int ret = -1;
	Boolean found_input = False;

	for (i = 0; i < app->fds.nfds && nfound > 0; i++) {
	    XtInputMask condition = 0;
	    if (FD_ISSET (i, (fd_set *)&rmaskfd)) {
		nfound--;
		if (!ignoreEvents) {
		    for (d = 0; d < app->count; d++) {
			if (i == ConnectionNumber(app->list[d])) {
			    if (ret == -1) {
				if (XEventsQueued( app->list[d],
						   QueuedAfterReading ))
				    ret = d;
				/*
				 * An error event could have arrived
				 * without any real events, or events
				 * could have been swallowed by Xlib,
				 * or the connection may be broken.
				 * We can't tell the difference, so
				 * assume Xlib will eventually discover
				 * a broken connection.
				 */
			    }
			    goto ENDILOOP;
			}
		    }
		}
		condition = XtInputReadMask;
	    }
	    if (FD_ISSET (i, (fd_set *)&wmaskfd)) {
		condition |= XtInputWriteMask;
		nfound--;
	    }
	    if (FD_ISSET (i, (fd_set *)&emaskfd)) {
		condition |= XtInputExceptMask;
		nfound--;
	    }
	    if (condition) {
		InputEvent *ep;
		for (ep = app->input_list[i]; ep; ep = ep->ie_next) {
		    if (condition & ep->ie_condition) {
			ep->ie_oq = app->outstandingQueue;
			app->outstandingQueue = ep;
		    }
		}
		found_input = True;
	    }
ENDILOOP:   ;
	} /* endfor */
	if (ret >= 0 || found_input)
	    return ret;
	goto WaitLoop;		/* must have been only error events */
	}
}

#define IeCallProc(ptr) \
    (*ptr->ie_proc) (ptr->ie_closure, &ptr->ie_source, (XtInputId*)&ptr);

#define TeCallProc(ptr) \
    (*ptr->te_proc) (ptr->te_closure, (XtIntervalId*)&ptr);

/*
 * Public Routines
 */

XtIntervalId XtAddTimeOut(interval, proc, closure)
	unsigned long interval;
	XtTimerCallbackProc proc;
	XtPointer closure;
{
	return XtAppAddTimeOut(_XtDefaultAppContext(), 
		interval, proc, closure); 
}


XtIntervalId XtAppAddTimeOut(app, interval, proc, closure)
	XtAppContext app;
	unsigned long interval;
	XtTimerCallbackProc proc;
	XtPointer closure;
{
	TimerEventRec *tptr;
        struct timeval current_time;

	if (freeTimerRecs) {
	    tptr = freeTimerRecs;
	    freeTimerRecs = tptr->te_next;
	}
	else tptr = XtNew(TimerEventRec);

	tptr->te_next = NULL;
	tptr->te_closure = closure;
	tptr->te_proc = proc;
	tptr->app = app;
	tptr->te_timer_value.tv_sec = interval/1000;
	tptr->te_timer_value.tv_usec = (interval%1000)*1000;
        (void) gettimeofday(&current_time, NULL);
	FIXUP_TIMEVAL(current_time);
        ADD_TIME(tptr->te_timer_value,tptr->te_timer_value,current_time);
	QueueTimerEvent(app, tptr);
	return( (XtIntervalId) tptr);
}

void  XtRemoveTimeOut(id)
    XtIntervalId id;
{
   TimerEventRec *t, *last, *tid = (TimerEventRec *) id;

   /* find it */

   for(t = tid->app->timerQueue, last = NULL;
	   t != NULL && t != tid;
	   t = t->te_next) last = t;

   if (t == NULL) return; /* couldn't find it */
   if(last == NULL) { /* first one on the list */
       t->app->timerQueue = t->te_next;
   } else last->te_next = t->te_next;

   t->te_next = freeTimerRecs;
   freeTimerRecs = t;
   return;
}

XtWorkProcId XtAddWorkProc(proc, closure)
	XtWorkProc proc;
	XtPointer closure;
{
	return XtAppAddWorkProc(_XtDefaultAppContext(), proc, closure);
}

XtWorkProcId XtAppAddWorkProc(app, proc, closure)
	XtAppContext app;
	XtWorkProc proc;
	XtPointer closure;
{
	WorkProcRec *wptr;

	if (freeWorkRecs) {
	    wptr = freeWorkRecs;
	    freeWorkRecs = wptr->next;
	} else wptr = XtNew(WorkProcRec);

	wptr->next = app->workQueue;
	wptr->closure = closure;
	wptr->proc = proc;
	wptr->app = app;
	app->workQueue = wptr;

	return (XtWorkProcId) wptr;
}

void  XtRemoveWorkProc(id)
	XtWorkProcId id;
{
	WorkProcRec *wid= (WorkProcRec *) id, *w, *last;

	/* find it */
	for(w = wid->app->workQueue, last = NULL; w != NULL && w != wid; w = w->next) last = w;

	if (w == NULL) return; /* couldn't find it */

	if(last == NULL) wid->app->workQueue = w->next;
	else last->next = w->next;

	w->next = freeWorkRecs;
	freeWorkRecs = w;
}

XtInputId XtAddInput( source, Condition, proc, closure)
	int source;
	XtPointer Condition;
	XtInputCallbackProc proc;
	XtPointer closure;
{
	return XtAppAddInput(_XtDefaultAppContext(),
		source, Condition, proc, closure);
}

XtInputId XtAppAddInput(app, source, Condition, proc, closure)
	XtAppContext app;
	int source;
	XtPointer Condition;
	XtInputCallbackProc proc;
	XtPointer closure;
{
	InputEvent* sptr;
	XtInputMask condition = (XtInputMask) Condition;
	
	if (!condition ||
	    condition & ~(XtInputReadMask|XtInputWriteMask|XtInputExceptMask))
	    XtAppErrorMsg(app,"invalidParameter","xtAddInput",XtCXtToolkitError,
			  "invalid condition passed to XtAppAddInput",
			  (String *)NULL, (Cardinal *)NULL);

	if (app->input_list == NULL) {
#ifdef OPEN_MAX
	    app->input_max = OPEN_MAX;
#else
	    app->input_max = sysconf(_SC_OPEN_MAX);
#endif
	    app->input_list = (InputEvent**)
	       _XtHeapAlloc(&app->heap,(Cardinal)app->input_max*sizeof(InputEvent*));
	    bzero((char*)app->input_list,(unsigned)app->input_max*sizeof(InputEvent*));
	}
	sptr = XtNew(InputEvent);
	sptr->ie_proc = proc;
	sptr->ie_closure = closure;
	sptr->app = app;
	sptr->ie_oq = NULL;
	sptr->ie_source = source;
	sptr->ie_condition = condition;
	sptr->ie_next = app->input_list[source];
	app->input_list[source] = sptr;

	if (condition & XtInputReadMask)   FD_SET(source, (fd_set *)&app->fds.rmask);
	if (condition & XtInputWriteMask)  FD_SET(source, (fd_set *)&app->fds.wmask);
	if (condition & XtInputExceptMask) FD_SET(source, (fd_set *)&app->fds.emask);

	if (app->fds.nfds < (source+1)) app->fds.nfds = source+1;
	app->fds.count++;
	return((XtInputId)sptr);

}

void XtRemoveInput( id )
	register XtInputId  id;
{
  	register InputEvent *sptr, *lptr;
	XtAppContext app = ((InputEvent *)id)->app;
	register int source = ((InputEvent *)id)->ie_source;
	Boolean found = False;

	sptr = app->outstandingQueue;
	lptr = NULL;
	for (; sptr != NULL; sptr = sptr->ie_oq) {
	    if (sptr == (InputEvent *)id) {
		if (lptr == NULL) app->outstandingQueue = sptr->ie_oq;
		else lptr->ie_oq = sptr->ie_oq;
	    }
	    lptr = sptr;
	}

	if(app->input_list && (sptr = app->input_list[source]) != NULL) {
		for( lptr = NULL ; sptr; sptr = sptr->ie_next ){
			if(sptr == (InputEvent *) id) {
				XtInputMask condition;
				if(lptr == NULL) {
				    app->input_list[source] = sptr->ie_next;
				} else {
				    lptr->ie_next = sptr->ie_next;
				}
				for (condition = 0, lptr = sptr->ie_next;
				     lptr; lptr = lptr->ie_next)
				    condition |= lptr->ie_condition;
				if ((sptr->ie_condition & XtInputReadMask) &&
				    !(condition & XtInputReadMask))
				   FD_CLR(source, (fd_set *)&app->fds.rmask);
				if ((sptr->ie_condition & XtInputWriteMask) &&
				    !(condition & XtInputWriteMask))
				   FD_CLR(source, (fd_set *)&app->fds.wmask);
				if ((sptr->ie_condition & XtInputExceptMask) &&
				    !(condition & XtInputExceptMask))
				   FD_CLR(source, (fd_set *)&app->fds.emask);
				XtFree((char *) sptr);
				found = True;
				break;
			}
			lptr = sptr;	      
		}
	}

    if (found)
	app->fds.count--;
    else
	XtAppWarningMsg(app, "invalidProcedure","inputHandler",XtCXtToolkitError,
                   "XtRemoveInput: Input handler not found",
		   (String *)NULL, (Cardinal *)NULL);
}

void _XtRemoveAllInputs(app)
    XtAppContext app;
{
    int i;
    for (i = 0; i < app->input_max; i++) {
	InputEvent* ep = app->input_list[i];
	while (ep) {
	    InputEvent *next = ep->ie_next;
	    XtFree( (char*)ep );
	    ep = next;
	}
    }
}

/* Do alternate input and timer callbacks if there are any */

static void DoOtherSources(app)
	XtAppContext app;
{
	TimerEventRec *te_ptr;
	InputEvent *ie_ptr;
	struct timeval  cur_time;

#define DrainQueue() \
	for (ie_ptr = app->outstandingQueue; ie_ptr != NULL;) { \
	    app->outstandingQueue = ie_ptr->ie_oq;		\
	    ie_ptr ->ie_oq = NULL;				\
	    IeCallProc(ie_ptr);					\
	    ie_ptr = app->outstandingQueue;			\
	}
/*enddef*/
	DrainQueue();
	if (app->fds.count > 0) {
	    /* Call _XtwaitForSomething to get input queued up */
	    (void) _XtwaitForSomething(TRUE, FALSE, TRUE, FALSE,
		(unsigned long *)NULL, app);
	    DrainQueue();
	}
	if (app->timerQueue != NULL) {	/* check timeout queue */
	    (void) gettimeofday (&cur_time, NULL);
	    FIXUP_TIMEVAL(cur_time);
	    while(IS_AT_OR_AFTER (app->timerQueue->te_timer_value, cur_time)) {
		te_ptr = app->timerQueue;
		app->timerQueue = te_ptr->te_next;
		te_ptr->te_next = NULL;
		if (te_ptr->te_proc != NULL)
		    TeCallProc(te_ptr);
		te_ptr->te_next = freeTimerRecs;
		freeTimerRecs = te_ptr;
              if (app->timerQueue == NULL) break;
	    }
	}
#undef DrainQueue
}

/* If there are any work procs, call them.  Return whether we did so */

static Boolean CallWorkProc(app)
	XtAppContext app;
{
	register WorkProcRec *w = app->workQueue;
	Boolean delete;

	if (w == NULL) return FALSE;

	app->workQueue = w->next;

	delete = (*(w->proc)) (w->closure);

	if (delete) {
	    w->next = freeWorkRecs;
	    freeWorkRecs = w;
	}
	else {
	    w->next = app->workQueue;
	    app->workQueue = w;
	}
	return TRUE;
}

/*
 * XtNextEvent()
 * return next event;
 */

void XtNextEvent(event)
	XEvent *event;
{
	XtAppNextEvent(_XtDefaultAppContext(), event);
}

void _XtRefreshMapping(event, dispatch)
    XEvent *event;
    Boolean dispatch;
{
    XtPerDisplay pd = _XtGetPerDisplay(event->xmapping.display);

    if (event->xmapping.request != MappingPointer &&
	pd && pd->keysyms && (event->xmapping.serial >= pd->keysyms_serial))
	_XtBuildKeysymTables( event->xmapping.display, pd );
    XRefreshKeyboardMapping(&event->xmapping);
    if (dispatch && pd && pd->mapping_callbacks)
	XtCallCallbackList((Widget) NULL,
			   (XtCallbackList)pd->mapping_callbacks,
			   (XtPointer)event );
}

void XtAppNextEvent(app, event)
	XtAppContext app;
	XEvent *event;
{
    int i, d;

    for (;;) {
	if (app->count == 0)
	    DoOtherSources(app);
	else {
	    for (i = 1; i <= app->count; i++) {
		d = (i + app->last) % app->count;
		if (d == 0) DoOtherSources(app);
		if (XEventsQueued(app->list[d], QueuedAfterReading))
		    goto GotEvent;
	    }
	    for (i = 1; i <= app->count; i++) {
		d = (i + app->last) % app->count;
		if (XEventsQueued(app->list[d], QueuedAfterFlush))
		    goto GotEvent;
	    }
	}

	/* We're ready to wait...if there is a work proc, call it */
	if (CallWorkProc(app)) continue;

	d = _XtwaitForSomething(FALSE, FALSE, FALSE, TRUE,
				(unsigned long *) NULL, app);

	if (d != -1) {
	  GotEvent:
	    XNextEvent (app->list[d], event);
	    app->last = d;
	    if (event->xany.type == MappingNotify)
		_XtRefreshMapping(event, False);
	    return;
	} 

    } /* for */
}
    
void XtProcessEvent(mask)
	XtInputMask mask;
{
	XtAppProcessEvent(_XtDefaultAppContext(), mask);
}

void XtAppProcessEvent(app, mask)
	XtAppContext app;
	XtInputMask mask;
{
	int i, d;
	XEvent event;
	struct timeval cur_time;

	if (mask == 0) return;

	for (;;) {
	    if (mask & XtIMTimer && app->timerQueue != NULL) {
		(void) gettimeofday (&cur_time, NULL);
		FIXUP_TIMEVAL(cur_time);
		if (IS_AT_OR_AFTER(app->timerQueue->te_timer_value, cur_time)){
		    TimerEventRec *te_ptr = app->timerQueue;
		    app->timerQueue = app->timerQueue->te_next;
		    te_ptr->te_next = NULL;
                    if (te_ptr->te_proc != NULL)
		        TeCallProc(te_ptr);
		    te_ptr->te_next = freeTimerRecs;
		    freeTimerRecs = te_ptr;
		    return;
		}
	    }
    
	    if (mask & XtIMAlternateInput) {
		if (app->fds.count > 0 && app->outstandingQueue == NULL) {
		    /* Call _XtwaitForSomething to get input queued up */
		    (void) _XtwaitForSomething(TRUE, FALSE, TRUE, FALSE,
			    (unsigned long *)NULL, app);
		}
		if (app->outstandingQueue != NULL) {
		    InputEvent *ie_ptr = app->outstandingQueue;
		    app->outstandingQueue = ie_ptr->ie_oq;
		    ie_ptr->ie_oq = NULL;
		    IeCallProc(ie_ptr);
		    return;
		}
	    }
    
	    if (mask & XtIMXEvent) {
		for (i = 1; i <= app->count; i++) {
		    d = (i + app->last) % app->count;
		    if (XEventsQueued(app->list[d], QueuedAfterReading))
			goto GotEvent;
		}
		for (i = 1; i <= app->count; i++) {
		    d = (i + app->last) % app->count;
		    if (XEventsQueued(app->list[d], QueuedAfterFlush))
			goto GotEvent;
		}
	    }

	    /* Nothing to do...wait for something */

	    if (CallWorkProc(app)) continue;

	    d = _XtwaitForSomething(
				    (mask & XtIMTimer ? FALSE : TRUE),
				    (mask & XtIMAlternateInput ? FALSE : TRUE),
				    (mask & XtIMXEvent ? FALSE : TRUE),
				    TRUE,
				    (unsigned long *) NULL, app);

	    if (mask & XtIMXEvent && d != -1) {
	      GotEvent:
		XNextEvent(app->list[d], &event);
		app->last = d;
		if (event.xany.type == MappingNotify) {
		    _XtRefreshMapping(&event, False);
		}
		XtDispatchEvent(&event);
		return;
	    } 
	
	}    
}

XtInputMask XtPending()
{
	return XtAppPending(_XtDefaultAppContext());
}

XtInputMask XtAppPending(app)
	XtAppContext app;
{
	struct timeval cur_time;
	int d;
	XtInputMask ret = 0;

/*
 * Check for pending X events
 */
	for (d = 0; d < app->count; d++) {
	    if (XEventsQueued(app->list[d], QueuedAfterReading)) {
		ret = XtIMXEvent;
		break;
	    }
	}
	if (ret == 0) {
	    for (d = 0; d < app->count; d++) {
		if (XEventsQueued(app->list[d], QueuedAfterFlush)) {
		    ret = XtIMXEvent;
		    break;
		}
	    }
	}

/*
 * Check for pending alternate input
 */
	if (app->timerQueue != NULL) {	/* check timeout queue */ 
	    (void) gettimeofday (&cur_time, NULL);
	    FIXUP_TIMEVAL(cur_time);
	    if ((IS_AT_OR_AFTER(app->timerQueue->te_timer_value, cur_time))  &&
                (app->timerQueue->te_proc != 0)) {
		ret |= XtIMTimer;
	    }
	}

	if (app->outstandingQueue != NULL) ret |= XtIMAlternateInput;
	else {
	    /* This won't cause a wait, but will enqueue any input */

	    if(_XtwaitForSomething(TRUE, FALSE, FALSE, FALSE, (unsigned long *) NULL,
		    app) != -1) ret |= XtIMXEvent;
	    if (app->outstandingQueue != NULL) ret |= XtIMAlternateInput;
	}
	return ret;
}

/* Peek at alternate input and timer callbacks if there are any */

static Boolean PeekOtherSources(app)
	XtAppContext app;
{
	struct timeval  cur_time;

	if (app->outstandingQueue != NULL) return TRUE;

	if (app->fds.count > 0) {
	    /* Call _XtwaitForSomething to get input queued up */
	    (void) _XtwaitForSomething(TRUE, FALSE, TRUE, FALSE,
		    (unsigned long *)NULL, app);
	    if (app->outstandingQueue != NULL) return TRUE;
	}

	if (app->timerQueue != NULL) {	/* check timeout queue */
	    (void) gettimeofday (&cur_time, NULL);
	    FIXUP_TIMEVAL(cur_time);
	    if (IS_AT_OR_AFTER (app->timerQueue->te_timer_value, cur_time))
		return TRUE;
	}

	return FALSE;
}

Boolean XtPeekEvent(event)
	XEvent *event;
{
	return XtAppPeekEvent(_XtDefaultAppContext(), event);
}

Boolean XtAppPeekEvent(app, event)
	XtAppContext app;
	XEvent *event;
{
	int i, d;
	Boolean foundCall = FALSE;
	
	for (i = 1; i <= app->count; i++) {
	    d = (i + app->last) % app->count;
	    if (d == 0) foundCall = PeekOtherSources(app);
	    if (XEventsQueued(app->list[d], QueuedAfterReading))
		goto GotEvent;
	}
	for (i = 1; i <= app->count; i++) {
	    d = (i + app->last) % app->count;
	    if (XEventsQueued(app->list[d], QueuedAfterFlush))
		goto GotEvent;
	}
	
	if (foundCall) {
	    event->xany.type = 0;
	    event->xany.display = NULL;
	    event->xany.window = 0;
	    return FALSE;
	}
	
	d = _XtwaitForSomething(FALSE, FALSE, FALSE, TRUE,
				(unsigned long *) NULL, app);
	
	if (d != -1) {
	  GotEvent:
	    XPeekEvent(app->list[d], event);
	    app->last = (d == 0 ? app->count : d) - 1;
	    return TRUE;
	}
	event->xany.type = 0;	/* Something else must be ready */
	event->xany.display = NULL;
	event->xany.window = 0;
	return FALSE;
}	
