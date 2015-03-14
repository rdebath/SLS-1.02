/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)Select.c	26.13	91/09/14 SMI"

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include "olwm.h"
#include "win.h"
#include "list.h"
#include "mem.h"

static List *selectList = NULL_LIST;

Time SelectionTime;

/* Externals */
extern Atom	AtomAtomPair;
extern Atom	AtomLength;
extern Atom	AtomListLength;
extern Atom	AtomName;
extern Atom	AtomMultiple;
extern Atom	AtomOlwmTimestamp;
extern Atom	AtomTargets;
extern Atom	AtomTimestamp;

/*
 * IsSelected -- returns boolean telling whether or not client is selected.
 *
 * Input:
 *
 *	cli	- client which might be on selection list.
 *
 * Output:
 *
 *	Returns True if the client cli is on the selection list.
 *	Returns False if it is not.
 *
 * Globals:
 *
 *	selectList	- The list of selected windows.
 */
Bool
IsSelected(cli)
Client *cli;
{
	return cli->isSelected;
}


/*
 * AddSelection -- add this client to the selection list if it is not 
 *	already present.  This routine also tells the server it is the current 
 *	XA_PRIMARY selection owner.
 *
 * Input:
 *
 *	cli	- The client we are adding to the selectList.
 *
 * Output:
 *
 *	None.
 *
 * Globals:
 *
 *	selectList	- The list of selected windows.
 *	The current selection owner in the server is also adjusted.
 */
int
AddSelection(cli, timestamp)
Client *cli;
Time timestamp;
{
	List *l = selectList;
	Client *tc;

	if (selectList == NULL_LIST)
	{
		/* Tell the server that we are now the 
		 * current selection owner.
		 * We really only need to call this 
		 * if this is the first window
		 * in the SelectList.  Calling this sends the
		 * previous owner of the XA_PRIMARY selection 
		 * a SelectionClear event, unless the previous 
		 * owner was us already.
	 	 */
		XSetSelectionOwner(cli->dpy, XA_PRIMARY, 
				   NoFocusWin,
				   timestamp);
	}
	else
	{
		/* First look to see if window is already listed. */
		for(tc = ListEnum(&l); tc != NULL; tc = ListEnum(&l))
		{
			if (tc == cli)
				return;
		}
	}

	/* If we get here the window wasn't already in the list. */
	selectList = ListCons(cli,selectList);
	cli->isSelected = True;

	/* Tell the window it is selected. */
	WinCallSelect(cli, True);
	SelectionTime = timestamp;
}

/*
 * RemoveSelection -- de-select the window
 *
 * Purpose:
 * 	This routine will remove an entry from the SelectList.
 * Input:
 *
 *	winInfo	- The window we are removing from the SelectList.
 *
 * Output:
 *	Returns True if client was deselected; false if the
 *	client was not already selected.
 *
 * Globals:
 *
 *	selectList	- The list of selected windows.
 */
Bool
RemoveSelection(cli)
Client *cli;
{
	List **l;

	for (l = &selectList ; *l != NULL; l = &((*l)->next))
	{
		if ((*l)->value == cli)
		{
			ListDestroyCell(l);
			cli->isSelected = False;
			WinCallSelect(cli,False);
			return True;
		}
	}
	return False;
}



/*
 * ToggleSelection -- if the winInfo is already selected, then de-select it.
 *	If not, then select it.
 *	Returns whether it is selected or not.
 */
Bool
ToggleSelection(cli, timestamp)
Client *cli;
Time timestamp;
{

	/* If already present, we want to deselect. */
	if (RemoveSelection(cli))
		return False;
	else
	{
		AddSelection(cli, timestamp);
		return True;
	}
}


/*
 * ClearSelections -- clear the selection list
 */
/*ARGSUSED*/
void
ClearSelections(dpy)
Display	*dpy;
{
	List *l;
	Client *cli;

	l = selectList;
	for(cli = ListEnum(&l); cli != NULL; cli = ListEnum(&l))
	{
		cli->isSelected = False;
		WinCallSelect(cli,False);
	}
	ListDestroy(selectList);
	selectList = NULL_LIST;
}

/*
 * EnumSelections --- enumerate the selected client structures.  
 *	Pass NULL to begin enumeration; any non-NULL value thereafter
 *	will continue enumeration where it left off.  This function
 *	uses static data, so only one enumeration can happen at any
 *	given time.
 *	Returns NULL when list is exhausted.
 */
Client *
EnumSelections(foo)
void *foo;
{
	static List *l;
	Client *ct;

	if (foo == NULL)
		l = selectList;

	if (l != NULL)
	{
		ct = l->value;
		l = l->next;
		return ct;
	}
	return NULL;
}


/*
 * Handle the conversion of a single target.  Used for both single requests 
 * and for MULTIPLE requests.  Returns True if the conversion was successful, 
 * otherwise False.
 */
static Bool
handleTarget(dpy, requestor, target, property)
    Display *dpy;
    Window requestor;
    Atom target;
    Atom property;
{
    unsigned long data[10];	/* long enough for most things */
    unsigned char *propdata = (unsigned char *) data;
    int format, nelements, i;
    Client *cli;
    Atom type;
    Bool freedata = False;
    Window *wp;

    if (target == AtomTargets) {
	data[0] = AtomTargets;
	data[1] = AtomTimestamp;
	data[2] = AtomListLength;
	data[3] = XA_DRAWABLE;
	data[4] = AtomLength;
	data[5] = AtomMultiple;
	data[6] = AtomName;
	nelements = 7;
	type = XA_ATOM;
	format = 32;
    } else if (target == AtomTimestamp) {
	data[0] = SelectionTime;
	nelements = 1;
	type = XA_INTEGER;
	format = 32;
    } else if (target == AtomListLength) {
	data[0] = ListCount(selectList);
	nelements = 1;
	type = XA_INTEGER;
	format = 32;
    } else if (target == AtomLength) {
	data[0] = ListCount(selectList)*sizeof(long);
	nelements = 1;
	type = XA_INTEGER;
	format = 32;
    } else if (target == XA_DRAWABLE) {
	nelements = ListCount(selectList);
	propdata = (unsigned char *)
			MemCalloc(nelements, sizeof(unsigned long));
	freedata = True;
	wp = (Window *) propdata;
	i = 0;
	cli = NULL;
	while ((cli = EnumSelections(cli)) && (i<nelements))
	    *wp++ = PANEWINOFCLIENT(cli);
	type = XA_DRAWABLE;
	format = 32;
    } else if (target == AtomName) {
	int curlen = 0;
	int maxlen = 100;
	int tmplen;
	char *cp, *tmp;

	propdata = MemAlloc(maxlen);
	cp = (char *) propdata;

	cli = NULL;
	while (cli = EnumSelections(cli)) {
	    if (cli->framewin != NULL && cli->framewin->fcore.name != NULL)
		tmp = cli->framewin->fcore.name;
	    else
		tmp = "";

	    tmplen = strlen(tmp) + 1;
	    if (curlen + tmplen > maxlen) {
		maxlen += 100;
		propdata = MemRealloc(propdata, maxlen);
	    }
	    strcpy((char *) propdata+curlen, tmp);
	    curlen += tmplen;
	}

	nelements = curlen;
	type = XA_STRING;
	format = 8;
    } else {
	return False;
    }

    XChangeProperty(dpy, requestor, property, type, format, PropModeReplace,
		    (unsigned char *)propdata, nelements);

    if (freedata)
	MemFree(propdata);

    return True;
}


/*
 * SelectionResponse
 * Respond to a selection request in the manner specified by the ICCCM.
 * If we understand the target, write the appropriate property and send the 
 * event.  If we don't understand the target, just send the event.
 */
void
SelectionResponse(request)
    XSelectionRequestEvent *request;
{
    XSelectionEvent response;
    Atom *pairs;
    Atom zeroatom = None;
    unsigned long nitems, remain;
    int i;
    Bool writeback = False;

    response.type = SelectionNotify;
    response.serial = request->serial;
    response.requestor = request->requestor;
    response.selection = request->selection;
    response.time = request->time;
    response.target = None;
    response.property = None;

    if (request->target == AtomMultiple) {
	if (request->property != None) {
	    pairs = GetWindowProperty(request->display, request->requestor,
				      request->property, 0L, 100000L,
				      AtomAtomPair, 32, &nitems, &remain);

	    if (pairs != NULL) {

		/*
		 * Process each pair of atoms (target, property).  Watch out 
		 * for an odd last atom, and for property atoms of None.  If 
		 * the conversion fails, replace it with None in the original 
		 * property.
		 */

		for (i = 0; i+1 < nitems; i += 2) {

		    if (pairs[i+1] == None)
			continue;

		    if (!handleTarget(request->display, request->requestor,
				      pairs[i], pairs[i+1]))
		    {
			pairs[i+1] = None;
			writeback = True;
		    }
		}

		if (writeback)
		    XChangeProperty(request->display, request->requestor,
			request->property, AtomAtomPair, 32, PropModeReplace,
			(unsigned char *) pairs, nitems);

		XFree((char *) pairs);
	    }

	    response.target = AtomMultiple;
	    response.property = request->property;

	}
    } else {
	/*
	 * If the property field is None, the requestor is using an obsolete
	 * draft of the ICCCM.  Per the suggestion in ICCCM section 2.2, use
	 * the target name as the property name.
	 */
	if (request->property == None)
	    request->property = request->target;

	if (handleTarget(request->display, request->requestor,
			 request->target, request->property))
	{
	    response.target = request->target;
	    response.property = request->property;
	}
    }

    XSendEvent(request->display, request->requestor, False,
	       NoEventMask, (XEvent *)&response);
}

/* TimeFresh -- get a fresh timestamp from the server
 */
Time
TimeFresh(win)
WinGeneric *win;
{
	XEvent e;
	Time timestamp;

	XChangeProperty(win->core.client->dpy, win->core.self,
		AtomOlwmTimestamp, XA_INTEGER,
		32, PropModeReplace, (unsigned char *)win, 0);
	XSync(win->core.client->dpy,0);
	if (XCheckTypedWindowEvent(win->core.client->dpy, win->core.self,
	    PropertyNotify, &e))
		timestamp = e.xproperty.time;
	else
		timestamp = CurrentTime;

	return timestamp;
}
