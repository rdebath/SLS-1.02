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
/* $XConsortium: property.c,v 5.5 91/05/09 16:00:36 rws Exp $ */

#include "X.h"
#define NEED_REPLIES
#define NEED_EVENTS
#include "Xproto.h"
#include "windowstr.h"
#include "propertyst.h"
#include "dixstruct.h"

extern void (*ReplySwapVector[]) ();
extern void CopySwap16Write(), CopySwap32Write(), Swap32Write();
extern int WriteToClient();

/*****************************************************************
 * Property Stuff
 *
 *    ChangeProperty, DeleteProperty, GetProperties,
 *    ListProperties
 *
 *   Properties below to windows.  A allocate slots each time
 *   a property is added.  No fancy searching done.
 *
 *****************************************************************/

#ifdef notdef
static void
PrintPropertys(pWin)
    WindowPtr pWin;
{
    PropertyPtr pProp;
    register int j;

    pProp = pWin->userProps;
    while (pProp)
    {
        ErrorF(  "%x %x\n", pProp->propertyName, pProp->type);
        ErrorF("property format: %d\n", pProp->format);
        ErrorF("property data: \n");
        for (j=0; j<(pProp->format/8)*pProp->size; j++)
           ErrorF("%c\n", pProp->data[j]);
        pProp = pProp->next;
    }
}
#endif

int
ProcRotateProperties(client)
    ClientPtr client;
{
    int     i, j, delta;
    REQUEST(xRotatePropertiesReq);
    WindowPtr pWin;
    register    Atom * atoms;
    PropertyPtr * props;               /* array of pointer */
    PropertyPtr pProp;
    xEvent event;

    REQUEST_FIXED_SIZE(xRotatePropertiesReq, stuff->nAtoms << 2);
    UpdateCurrentTime();
    pWin = (WindowPtr) LookupWindow(stuff->window, client);
    if (!pWin)
        return(BadWindow);
    if (!stuff->nAtoms)
	return(Success);
    atoms = (Atom *) & stuff[1];
    props = (PropertyPtr *)ALLOCATE_LOCAL(stuff->nAtoms * sizeof(PropertyPtr));
    if (!props)
	return(BadAlloc);
    for (i = 0; i < stuff->nAtoms; i++)
    {
        if (!ValidAtom(atoms[i]))
        {
            DEALLOCATE_LOCAL(props);
	    client->errorValue = atoms[i];
            return BadAtom;
        }
        for (j = i + 1; j < stuff->nAtoms; j++)
            if (atoms[j] == atoms[i])
            {
                DEALLOCATE_LOCAL(props);
                return BadMatch;
            }
        pProp = wUserProps (pWin);
        while (pProp)
        {
            if (pProp->propertyName == atoms[i])
                goto found;
	    pProp = pProp->next;
        }
        DEALLOCATE_LOCAL(props);
        return BadMatch;
found: 
        props[i] = pProp;
    }
    delta = stuff->nPositions;

    /* If the rotation is a complete 360 degrees, then moving the properties
	around and generating PropertyNotify events should be skipped. */

    if ( (stuff->nAtoms != 0) && (abs(delta) % stuff->nAtoms) != 0 ) 
    {
	while (delta < 0)                  /* faster if abs value is small */
            delta += stuff->nAtoms;
    	for (i = 0; i < stuff->nAtoms; i++)
 	{
	    /* Generate a PropertyNotify event for each property whose value
		is changed in the order in which they appear in the request. */
 
 	    event.u.u.type = PropertyNotify;
            event.u.property.window = pWin->drawable.id;
    	    event.u.property.state = PropertyNewValue;
	    event.u.property.atom = props[i]->propertyName;	
	    event.u.property.time = currentTime.milliseconds;
	    DeliverEvents(pWin, &event, 1, (WindowPtr)NULL);
	
            props[i]->propertyName = atoms[(i + delta) % stuff->nAtoms];
	}
    }
    DEALLOCATE_LOCAL(props);
    return Success;
}

int 
ProcChangeProperty(client)
    ClientPtr client;
{	      
    WindowPtr pWin;
    char format, mode;
    unsigned long len;
    int sizeInBytes;
    int totalSize;
    int err;
    REQUEST(xChangePropertyReq);

    REQUEST_AT_LEAST_SIZE(xChangePropertyReq);
    UpdateCurrentTime();
    format = stuff->format;
    mode = stuff->mode;
    if ((mode != PropModeReplace) && (mode != PropModeAppend) &&
	(mode != PropModePrepend))
    {
	client->errorValue = mode;
	return BadValue;
    }
    if ((format != 8) && (format != 16) && (format != 32))
    {
	client->errorValue = format;
        return BadValue;
    }
    len = stuff->nUnits;
    sizeInBytes = format>>3;
    totalSize = len * sizeInBytes;
    REQUEST_FIXED_SIZE(xChangePropertyReq, totalSize);

    pWin = (WindowPtr)LookupWindow(stuff->window, client);
    if (!pWin)
	return(BadWindow);
    if (!ValidAtom(stuff->property))
    {
	client->errorValue = stuff->property;
	return(BadAtom);
    }
    if (!ValidAtom(stuff->type))
    {
	client->errorValue = stuff->type;
	return(BadAtom);
    }

    err = ChangeWindowProperty(pWin, stuff->property, stuff->type, (int)format,
			       (int)mode, len, (pointer)&stuff[1], TRUE);
    if (err != Success)
	return err;
    else
	return client->noClientException;
}

int
ChangeWindowProperty(pWin, property, type, format, mode, len, value, sendevent)
    WindowPtr	pWin;
    Atom	property, type;
    int		format, mode;
    unsigned long len;
    pointer	value;
    Bool	sendevent;
{
    PropertyPtr pProp;
    xEvent event;
    int sizeInBytes;
    int totalSize;
    pointer data;

    sizeInBytes = format>>3;
    totalSize = len * sizeInBytes;

    /* first see if property already exists */

    pProp = wUserProps (pWin);
    while (pProp)
    {
	if (pProp->propertyName == property)
	    break;
	pProp = pProp->next;
    }
    if (!pProp)   /* just add to list */
    {
	if (!pWin->optional && !MakeWindowOptional (pWin))
	    return(BadAlloc);
        pProp = (PropertyPtr)xalloc(sizeof(PropertyRec));
	if (!pProp)
	    return(BadAlloc);
        data = (pointer)xalloc(totalSize);
	if (!data && len)
	{
	    xfree(pProp);
	    return(BadAlloc);
	}
        pProp->propertyName = property;
        pProp->type = type;
        pProp->format = format;
        pProp->data = data;
	if (len)
	    bcopy((char *)value, (char *)data, totalSize);
	pProp->size = len;
        pProp->next = pWin->optional->userProps;
        pWin->optional->userProps = pProp;
    }
    else
    {
	/* To append or prepend to a property the request format and type
		must match those of the already defined property.  The
		existing format and type are irrelevant when using the mode
		"PropModeReplace" since they will be written over. */

        if ((format != pProp->format) && (mode != PropModeReplace))
	    return(BadMatch);
        if ((pProp->type != type) && (mode != PropModeReplace))
            return(BadMatch);
        if (mode == PropModeReplace) 
        {
	    if (totalSize != pProp->size * (pProp->format >> 3))
	    {
	    	data = (pointer)xrealloc(pProp->data, totalSize);
	    	if (!data && len)
		    return(BadAlloc);
            	pProp->data = data;
	    }
	    if (len)
		bcopy((char *)value, (char *)pProp->data, totalSize);
	    pProp->size = len;
    	    pProp->type = type;
	    pProp->format = format;
	}
	else if (len == 0)
	{
	    /* do nothing */
	}
        else if (mode == PropModeAppend)
        {
	    data = (pointer)xrealloc(pProp->data,
				     sizeInBytes * (len + pProp->size));
	    if (!data)
		return(BadAlloc);
            pProp->data = data;
	    bcopy((char *)value,
		  &((char *)data)[pProp->size * sizeInBytes], 
		  totalSize);
            pProp->size += len;
	}
        else if (mode == PropModePrepend)
        {
            data = (pointer)xalloc(sizeInBytes * (len + pProp->size));
	    if (!data)
		return(BadAlloc);
	    bcopy((char *)pProp->data, &((char *)data)[totalSize], 
		  (int)(pProp->size * sizeInBytes));
            bcopy((char *)value, (char *)data, totalSize);
	    xfree(pProp->data);
            pProp->data = data;
            pProp->size += len;
	}
    }
    event.u.u.type = PropertyNotify;
    event.u.property.window = pWin->drawable.id;
    event.u.property.state = PropertyNewValue;
    event.u.property.atom = pProp->propertyName;
    event.u.property.time = currentTime.milliseconds;
    DeliverEvents(pWin, &event, 1, (WindowPtr)NULL);

    return(Success);
}

DeleteProperty(pWin, propName)
    WindowPtr pWin;
    Atom propName;
{
    PropertyPtr pProp, prevProp;
    xEvent event;

    if (!(pProp = wUserProps (pWin)))
	return(Success);
    prevProp = (PropertyPtr)NULL;
    while (pProp)
    {
	if (pProp->propertyName == propName)
	    break;
        prevProp = pProp;
	pProp = pProp->next;
    }
    if (pProp) 
    {		    
        if (prevProp == (PropertyPtr)NULL)      /* takes care of head */
        {
            if (!(pWin->optional->userProps = pProp->next))
		CheckWindowOptionalNeed (pWin);
        }
	else
        {
            prevProp->next = pProp->next;
        }
	event.u.u.type = PropertyNotify;
	event.u.property.window = pWin->drawable.id;
	event.u.property.state = PropertyDelete;
        event.u.property.atom = pProp->propertyName;
	event.u.property.time = currentTime.milliseconds;
	DeliverEvents(pWin, &event, 1, (WindowPtr)NULL);
	xfree(pProp->data);
        xfree(pProp);
    }
    return(Success);
}

DeleteAllWindowProperties(pWin)
    WindowPtr pWin;
{
    PropertyPtr pProp, pNextProp;
    xEvent event;

    pProp = wUserProps (pWin);
    while (pProp)
    {
	event.u.u.type = PropertyNotify;
	event.u.property.window = pWin->drawable.id;
	event.u.property.state = PropertyDelete;
	event.u.property.atom = pProp->propertyName;
	event.u.property.time = currentTime.milliseconds;
	DeliverEvents(pWin, &event, 1, (WindowPtr)NULL);
	pNextProp = pProp->next;
        xfree(pProp->data);
        xfree(pProp);
	pProp = pNextProp;
    }
}

/*****************
 * GetProperty
 *    If type Any is specified, returns the property from the specified
 *    window regardless of its type.  If a type is specified, returns the
 *    property only if its type equals the specified type.
 *    If delete is True and a property is returned, the property is also
 *    deleted from the window and a PropertyNotify event is generated on the
 *    window.
 *****************/

int
ProcGetProperty(client)
    ClientPtr client;
{
    PropertyPtr pProp, prevProp;
    unsigned long n, len, ind;
    WindowPtr pWin;
    xGetPropertyReply reply;
    REQUEST(xGetPropertyReq);

    REQUEST_SIZE_MATCH(xGetPropertyReq);
    if (stuff->delete)
	UpdateCurrentTime();
    pWin = (WindowPtr)LookupWindow(stuff->window, client);
    if (pWin)
    {
	if (!ValidAtom(stuff->property))
	{
	    client->errorValue = stuff->property;
	    return(BadAtom);
	}
	if ((stuff->delete != xTrue) && (stuff->delete != xFalse))
	{
	    client->errorValue = stuff->delete;
	    return(BadValue);
	}
	if ((stuff->type == AnyPropertyType) || ValidAtom(stuff->type))
	{
	    pProp = wUserProps (pWin);
            prevProp = (PropertyPtr)NULL;
            while (pProp)
            {
	        if (pProp->propertyName == stuff->property) 
	            break;
		prevProp = pProp;
		pProp = pProp->next;
            }
	    reply.type = X_Reply;
	    reply.sequenceNumber = client->sequence;
            if (pProp) 
            {

		/* If the request type and actual type don't match. Return the
		property information, but not the data. */

                if ((stuff->type != pProp->type) &&
		    (stuff->type != AnyPropertyType))
		{
		    reply.bytesAfter = pProp->size;
		    reply.format = pProp->format;
		    reply.length = 0;
		    reply.nItems = 0;
		    reply.propertyType = pProp->type;
		    WriteReplyToClient(client, sizeof(xGenericReply), &reply);
		    return(Success);
		}

	    /*
             *  Return type, format, value to client
             */
		n = (pProp->format/8) * pProp->size; /* size (bytes) of prop */
		ind = stuff->longOffset << 2;        

               /* If longOffset is invalid such that it causes "len" to
                        be negative, it's a value error. */

		if (n < ind)
		{
		    client->errorValue = stuff->longOffset;
		    return BadValue;
		}

		len = min(n - ind, 4 * stuff->longLength);

		reply.bytesAfter = n - (ind + len);
		reply.format = pProp->format;
		reply.length = (len + 3) >> 2;
		reply.nItems = len / (pProp->format / 8 );
		reply.propertyType = pProp->type;

                if (stuff->delete && (reply.bytesAfter == 0))
                { /* send the event */
		    xEvent event;
		
		    event.u.u.type = PropertyNotify;
		    event.u.property.window = pWin->drawable.id;
		    event.u.property.state = PropertyDelete;
		    event.u.property.atom = pProp->propertyName;
		    event.u.property.time = currentTime.milliseconds;
		    DeliverEvents(pWin, &event, 1, (WindowPtr)NULL);
		}

		WriteReplyToClient(client, sizeof(xGenericReply), &reply);
		if (len)
		{
		    switch (reply.format) {
		    case 32: client->pSwapReplyFunc = CopySwap32Write; break;
		    case 16: client->pSwapReplyFunc = CopySwap16Write; break;
		    default: client->pSwapReplyFunc = (void (*) ())WriteToClient; break;
		    }
		    WriteSwappedDataToClient(client, len, pProp->data + ind);
		}

                if (stuff->delete && (reply.bytesAfter == 0))
                { /* delete the Property */
                    if (prevProp == (PropertyPtr)NULL) /* takes care of head */
		    {
                        if (!(pWin->optional->userProps = pProp->next))
			    CheckWindowOptionalNeed (pWin);
		    }
	            else
                        prevProp->next = pProp->next;
		    xfree(pProp->data);
                    xfree(pProp);
		}
	    }
            else 
	    {   
                reply.nItems = 0;
		reply.length = 0;
		reply.bytesAfter = 0;
		reply.propertyType = None;
		reply.format = 0;
		WriteReplyToClient(client, sizeof(xGenericReply), &reply);
	    }
            return(client->noClientException);

	}
        else
	{
	    client->errorValue = stuff->type;
            return(BadAtom);
	}
    }
    else            
        return (BadWindow); 
}

int
ProcListProperties(client)
    ClientPtr client;
{
    Atom *pAtoms, *temppAtoms;
    xListPropertiesReply xlpr;
    int	numProps = 0;
    WindowPtr pWin;
    PropertyPtr pProp;
    REQUEST(xResourceReq);

    REQUEST_SIZE_MATCH(xResourceReq);
    pWin = (WindowPtr)LookupWindow(stuff->id, client);
    if (!pWin)
        return(BadWindow);

    pProp = wUserProps (pWin);
    while (pProp)
    {        
        pProp = pProp->next;
	numProps++;
    }
    if (numProps)
        if(!(pAtoms = (Atom *)ALLOCATE_LOCAL(numProps * sizeof(Atom))))
            return(BadAlloc);

    xlpr.type = X_Reply;
    xlpr.nProperties = numProps;
    xlpr.length = (numProps * sizeof(Atom)) >> 2;
    xlpr.sequenceNumber = client->sequence;
    pProp = wUserProps (pWin);
    temppAtoms = pAtoms;
    while (pProp)
    {
	*temppAtoms++ = pProp->propertyName;
	pProp = pProp->next;
    }
    WriteReplyToClient(client, sizeof(xGenericReply), &xlpr);
    if (numProps)
    {
        client->pSwapReplyFunc = Swap32Write;
        WriteSwappedDataToClient(client, numProps * sizeof(Atom), pAtoms);
        DEALLOCATE_LOCAL(pAtoms);
    }
    return(client->noClientException);
}
