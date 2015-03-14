/*
 * $XConsortium: ReqMach.c,v 1.10 91/07/24 15:23:52 converse Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Davor Matic, MIT X Consortium
 */

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/Xfuncs.h>
#include "BitmapP.h"
    
#include <stdio.h>
#include <string.h>
#include <math.h>

#ifndef abs
#define abs(x)                        (((x) > 0) ? (x) : -(x))
#endif
#define min(x, y)                     (((x) < (y)) ? (x) : (y))
#define max(x, y)                     (((x) > (y)) ? (x) : (y))


extern Boolean DEBUG;

/*****************************************************************************\
 * Request Machine: stacks up and handles requests from application calls.   * 
\*****************************************************************************/

/*
 * Searches for a request record of a request specified by its name.
 * Returns a pointer to the record or NULL if the request was not found.
 */
BWRequestRec *FindRequest(name)
    BWRequest name;
{
    int i;

    for (i = 0; i < bitmapClassRec.bitmap_class.num_requests; i++)
	if (!strcmp(name, bitmapClassRec.bitmap_class.requests[i].name))
	    return &bitmapClassRec.bitmap_class.requests[i];
    
    return NULL;
}

/*
 * Adds a request to the request stack and does proper initializations.
 * Returns TRUE if the request was found and FALSE otherwise.
 */
Boolean BWAddRequest(w, name, trap, call_data, call_data_size)
    Widget    w;
    BWRequest name;
    Boolean   trap;
    caddr_t   call_data;
    Cardinal  call_data_size;
{
    BitmapWidget BW = (BitmapWidget) w;
    BWRequestRec *request;
    
    request = FindRequest(name);
    if(request) {
	if (DEBUG)
	  fprintf(stderr, "Adding... Cardinal: %d\n", BW->bitmap.cardinal + 1);

	BW->bitmap.request_stack = (BWRequestStack *)
	    XtRealloc((char *)BW->bitmap.request_stack,
		      (++BW->bitmap.cardinal + 1) * sizeof(BWRequestStack));
	
	BW->bitmap.request_stack[BW->bitmap.cardinal].request = request;
	BW->bitmap.request_stack[BW->bitmap.cardinal].status = 
	    XtMalloc(request->status_size);
	BW->bitmap.request_stack[BW->bitmap.cardinal].trap = trap;
	BW->bitmap.request_stack[BW->bitmap.cardinal].call_data = 
	    XtMalloc(call_data_size);
	bcopy(call_data, 
	      BW->bitmap.request_stack[BW->bitmap.cardinal].call_data,
	      call_data_size);

	return True;
    }
    else {
	XtWarning("bad request name.  BitmapWidget");
	return False;
    }
}

/*
 * Engages the request designated by the current parameter.
 * Returnes TRUE if the request has an engage function and FALSE otherwise.
 */
Boolean Engage(BW, current)
    BitmapWidget BW;
    Cardinal current;
{
    BW->bitmap.current = current;
    
    if (DEBUG)
	fprintf(stderr, "Request: %s\n", 
		BW->bitmap.request_stack[current].request->name);
  
    if (BW->bitmap.request_stack[current].request->engage) {
	(*BW->bitmap.request_stack[current].request->engage)
	    ((Widget) BW,
	     BW->bitmap.request_stack[current].status,
	     BW->bitmap.request_stack[current].request->engage_client_data,
	     BW->bitmap.request_stack[current].call_data);
	return True;
    }
    else
	return False;
}

Boolean BWTerminateRequest();
Boolean BWRemoveRequest();

/*
 * Scans down the request stack removing all requests untill it finds 
 * one to be trapped.
 */
void TrappingLoop(BW)
    BitmapWidget BW;
{

    if (DEBUG)
	fprintf(stderr, "Scanning... Current: %d\n", BW->bitmap.current);
    if ((BW->bitmap.current > 0) 
	&& 
	(!BW->bitmap.request_stack[BW->bitmap.current--].trap)) {
	BWRemoveRequest((Widget) BW);
	TrappingLoop(BW);
    }
    else
	if (BW->bitmap.cardinal > 0) {
	    if (DEBUG)
		fprintf(stderr, "Trapping... Current: %d\n", BW->bitmap.current+1);
	    if(!Engage(BW, ++BW->bitmap.current))
		BWTerminateRequest((Widget) BW, True);
	}
}
/*
 * Terimantes the current request and continues with next request if con = TRUE
 * Returnes TRUE if there is any number of requests left on the stack.
 */
Boolean BWTerminateRequest(w, cont)
    Widget w;
    Boolean cont;
{
    BitmapWidget BW = (BitmapWidget) w;
    
    if (BW->bitmap.current > 0) {
	if (DEBUG)
	    fprintf(stderr, "Terminating... Current: %d\n", BW->bitmap.current);
    	if (BW->bitmap.request_stack[BW->bitmap.current].request->terminate)
	    (*BW->bitmap.request_stack[BW->bitmap.current].request->terminate)
		(w,
		 BW->bitmap.request_stack[BW->bitmap.current].status,
		 BW->bitmap.request_stack[BW->bitmap.current].request->terminate_client_data,
		 BW->bitmap.request_stack[BW->bitmap.current].call_data);
	
	if (cont) {
	    if (BW->bitmap.current == BW->bitmap.cardinal)
		TrappingLoop(BW);
	    else {
		if (DEBUG)
		    fprintf(stderr, "Continuing... Current: %d\n", BW->bitmap.current+1);
		if (!Engage(BW, ++BW->bitmap.current))
		    BWTerminateRequest(w, True);
	    }
	}
	else
	    BW->bitmap.current = 0;
    }
    
    return BW->bitmap.current;
}

/*
 * Simple interface to BWTerminateRequest that takes only a widget.
 */
void BWAbort(w)
    Widget w;
{
    BWTerminateRequest(w, True);
}

/*
 * Removes the top request from the request stack. If the request is active
 * it will terminate it.
 * Returns TRUE if the number of requests left on the stack != 0.
 */
Boolean BWRemoveRequest(w)
    Widget w;
{
    BitmapWidget BW = (BitmapWidget) w;
    
    if (BW->bitmap.cardinal > 0) {
	if (DEBUG)
	    fprintf(stderr, "Removing... Cardinal: %d\n", BW->bitmap.cardinal);
	if (BW->bitmap.current == BW->bitmap.cardinal)
	    BWTerminateRequest(w, False);
	
	if (BW->bitmap.request_stack[BW->bitmap.cardinal].request->remove)
	    (*BW->bitmap.request_stack[BW->bitmap.cardinal].request->remove)
		(w,
		 BW->bitmap.request_stack[BW->bitmap.cardinal].status,
		 BW->bitmap.request_stack[BW->bitmap.cardinal].request->remove_client_data,
		 BW->bitmap.request_stack[BW->bitmap.cardinal].call_data);
	
	XtFree(BW->bitmap.request_stack[BW->bitmap.cardinal].status);
	XtFree(BW->bitmap.request_stack[BW->bitmap.cardinal].call_data);
	BW->bitmap.request_stack = (BWRequestStack *)
	    XtRealloc((char *)BW->bitmap.request_stack,
		      (--BW->bitmap.cardinal + 1) * sizeof(BWRequestStack));
	
	return True;
    }
    else 
	return False;
}

void BWRemoveAllRequests(w)
    Widget w;
{				/* SUPPRESS 530 */
    while (BWRemoveRequest(w)) {/* removes all requests from the stack */}
}

/*
 * Adds the request to the stack and performs engaging ritual.
 * Returns TRUE if the request was found, FALSE otherwise.
 */
Boolean BWEngageRequest(w, name, trap, call_data, call_data_size)
    Widget w;
    BWRequest name;
    Boolean trap;
    caddr_t call_data;
    Cardinal call_data_size;
{
    BitmapWidget BW = (BitmapWidget) w;
    
    if (BWAddRequest(w, name, trap, call_data, call_data_size)) {
	BWTerminateRequest(w, False);
	if (DEBUG)
	    fprintf(stderr, "Engaging... Cardinal: %d\n", BW->bitmap.cardinal);
	if (!Engage(BW, BW->bitmap.cardinal))
	    BWTerminateRequest(w, True);
	
	return True;
    }
    else
	return False;
}

/************************* End of the Request Machine ************************/
