/* $Header: xlistdev.c,v 1.12 91/07/17 16:37:42 rws Exp $ */

/************************************************************
Copyright (c) 1989 by Hewlett-Packard Company, Palo Alto, California, and the 
Massachusetts Institute of Technology, Cambridge, Massachusetts.

			All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Hewlett-Packard or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
HEWLETT-PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

/***********************************************************************
 *
 * Extension function to list the available input devices.
 *
 */

#define	 NEED_EVENTS
#define	 NEED_REPLIES
#include "X.h"				/* for inputstr.h    */
#include "Xproto.h"			/* Request macro     */
#include "inputstr.h"			/* DeviceIntPtr	     */
#include "XI.h"
#include "XIproto.h"

extern InputInfo inputInfo;
extern	int 	IReqCode;
extern	int	BadDevice;
extern	void	(*ReplySwapVector[256]) ();
DeviceIntPtr	LookupDeviceIntRec();

void		CopySwapKeyClass ();
void		CopySwapButtonClass ();
void		CopySwapValuatorClass ();
void		SizeDeviceInfo ();
void		ListDeviceInfo ();
void		AddOtherInputDevices ();

/***********************************************************************
 *
 * This procedure lists the input devices available to the server.
 *
 */

int
SProcXListInputDevices(client)
    register ClientPtr client;
    {
    register char n;

    REQUEST(xListInputDevicesReq);
    swaps(&stuff->length, n);
    return(ProcXListInputDevices(client));
    }

/***********************************************************************
 *
 * This procedure lists the input devices available to the server.
 *
 */

ProcXListInputDevices (client)
    register ClientPtr client;
    {
    xListInputDevicesReply	rep;
    int			numdevs;
    int 		namesize = 1;	/* need 1 extra byte for strcpy */
    int 		size = 0;
    int 		total_length;
    char		*devbuf;
    char		*classbuf;
    char		*namebuf;
    char		*savbuf;
    xDeviceInfo 	*dev;
    DeviceIntPtr 	d;
    void CopyDeviceName ();
    void CopySwapDevice ();

    REQUEST(xListInputDevicesReq);
    REQUEST_SIZE_MATCH(xListInputDevicesReq);

    rep.repType = X_Reply;
    rep.RepType = X_ListInputDevices;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;

    AddOtherInputDevices ();
    numdevs = inputInfo.numDevices;

    for (d=inputInfo.devices; d; d=d->next)
	SizeDeviceInfo (d, &namesize, &size);
    for (d=inputInfo.off_devices; d; d=d->next)
	SizeDeviceInfo (d, &namesize, &size);

    total_length = numdevs * sizeof (xDeviceInfo) + size + namesize;
    devbuf = (char *) Xalloc (total_length);
    classbuf = devbuf + (numdevs * sizeof (xDeviceInfo));
    namebuf = classbuf + size;
    savbuf = devbuf;

    dev = (xDeviceInfoPtr) devbuf;
    for (d=inputInfo.devices; d; d=d->next,dev++)
        ListDeviceInfo (client, d, dev, &devbuf, &classbuf, &namebuf);
    for (d=inputInfo.off_devices; d; d=d->next,dev++)
        ListDeviceInfo (client, d, dev, &devbuf, &classbuf, &namebuf);

    rep.ndevices = numdevs;
    rep.length = (total_length + 3) >> 2;
    WriteReplyToClient (client, sizeof (xListInputDevicesReply), &rep);
    WriteToClient(client, total_length, savbuf);
    Xfree (savbuf);
    return Success;
    }

/***********************************************************************
 *
 * This procedure calculates the size of the information to be returned
 * for an input device.
 *
 */

void
SizeDeviceInfo (d, namesize, size)
    DeviceIntPtr d;
    int *namesize;
    int *size;
    {
    *namesize += strlen (d->name) + 1;
    if (d->key != NULL)
	*size += sizeof (xKeyInfo);
    if (d->button != NULL)
	*size += sizeof (xButtonInfo);
    if (d->valuator != NULL)
	*size += (sizeof(xValuatorInfo) + 
		d->valuator->numAxes * sizeof(xAxisInfo));
    }

/***********************************************************************
 *
 * This procedure lists information to be returned for an input device.
 *
 */

void
ListDeviceInfo (client, d, dev, devbuf, classbuf, namebuf)
    ClientPtr client;
    DeviceIntPtr d;
    xDeviceInfoPtr dev;
    char **devbuf;
    char **classbuf;
    char **namebuf;
    {
    CopyDeviceName (namebuf, d->name);
    CopySwapDevice (client, d, 0, devbuf);
    if (d->key != NULL)
	{
	CopySwapKeyClass(client, d->key, classbuf);
	dev->num_classes++;
	}
    if (d->button != NULL)
	{
	CopySwapButtonClass(client, d->button, classbuf);
	dev->num_classes++;
	}
    if (d->valuator != NULL)
	{
	CopySwapValuatorClass(client, d->valuator, classbuf);
	dev->num_classes++;
	}
    }

/***********************************************************************
 *
 * This procedure copies data to the DeviceInfo struct, swapping if necessary.
 *
 * We need the extra byte in the allocated buffer, because the trailing null
 * hammers one extra byte, which is overwritten by the next name except for
 * the last name copied.
 *
 */

void
CopyDeviceName (namebuf, name)
    char **namebuf;
    char *name;
    {
    char *nameptr = (char *) *namebuf;

    *nameptr++ = strlen (name);
    strcpy (nameptr, name);
    *namebuf += (strlen (name)+1);
    }

/***********************************************************************
 *
 * This procedure copies data to the DeviceInfo struct, swapping if necessary.
 *
 */

void
CopySwapDevice (client, d, num_classes, buf)
    register ClientPtr 	client;
    DeviceIntPtr	d;
    int			num_classes;
    char 		**buf;
    {
    register char 	n;
    xDeviceInfoPtr dev;

    dev = (xDeviceInfoPtr) *buf;

    dev->id = d->id;
    dev->type = d->type;
    dev->num_classes = num_classes;
    if (d == inputInfo.keyboard)
	dev->use = IsXKeyboard;
    else if (d == inputInfo.pointer)
	dev->use = IsXPointer;
    else
	dev->use = IsXExtensionDevice;
    if (client->swapped)
	{
	swapl(&dev->type, n);	/* macro - braces are required */
	}
    *buf += sizeof (xDeviceInfo);
    }

/***********************************************************************
 *
 * This procedure copies KeyClass information, swapping if necessary.
 *
 */

void
CopySwapKeyClass (client, k, buf)
    register ClientPtr 	client;
    KeyClassPtr 	k;
    char 		**buf;
    {
    register char 	n;
    xKeyInfoPtr 	k2;

    k2 = (xKeyInfoPtr) *buf;
    k2->class = KeyClass;
    k2->length = sizeof (xKeyInfo);
    k2->min_keycode = k->curKeySyms.minKeyCode;
    k2->max_keycode = k->curKeySyms.maxKeyCode;
    k2->num_keys = k2->max_keycode - k2->min_keycode + 1;
    if (client->swapped)
	{
	swaps(&k2->num_keys,n);
	}
    *buf += sizeof (xKeyInfo);
    }

/***********************************************************************
 *
 * This procedure copies ButtonClass information, swapping if necessary.
 *
 */

void
CopySwapButtonClass (client, b, buf)
    register ClientPtr 	client;
    ButtonClassPtr 	b;
    char 		**buf;
    {
    register char 	n;
    xButtonInfoPtr 	b2;

    b2 = (xButtonInfoPtr) *buf;
    b2->class = ButtonClass;
    b2->length = sizeof (xButtonInfo);
    b2->num_buttons = b->numButtons;
    if (client->swapped)
	{
	swaps(&b2->num_buttons,n);	/* macro - braces are required */
	}
    *buf += sizeof (xButtonInfo);
    }

/***********************************************************************
 *
 * This procedure copies ValuatorClass information, swapping if necessary.
 *
 */

void
CopySwapValuatorClass (client, v, buf)
    register ClientPtr 	client;
    ValuatorClassPtr 	v;
    char 		**buf;
    {
    int			j;
    register char 	n;
    xValuatorInfoPtr 	v2;
    AxisInfo 		*a;
    xAxisInfoPtr 	a2;

    v2 = (xValuatorInfoPtr) *buf;
    v2->class = ValuatorClass;
    v2->length = sizeof (xValuatorInfo) + v->numAxes *
	sizeof (xAxisInfo);
    v2->num_axes  = v->numAxes;
    v2->mode  = v->mode & DeviceMode;
    v2->motion_buffer_size  = v->numMotionEvents;
    if (client->swapped)
	{
	swapl(&v2->motion_buffer_size,n);
	}
    *buf += sizeof (xValuatorInfo);
    a = v->axes;
    a2 = (xAxisInfoPtr) *buf;
    for (j=0; j<v->numAxes; j++)
	{
	a2->min_value = a->min_value;
	a2->max_value = a->max_value;
	a2->resolution = a->resolution;
    	if (client->swapped)
	    {
	    swapl(&a2->min_value,n);
	    swapl(&a2->max_value,n);
	    swapl(&a2->resolution,n);
	    }
	a2++;
	a++;
	*buf += sizeof (xAxisInfo);
	}
    }

/***********************************************************************
 *
 * This procedure writes the reply for the XListInputDevices function,
 * if the client and server have a different byte ordering.
 *
 */

SRepXListInputDevices (client, size, rep)
    ClientPtr	client;
    int		size;
    xListInputDevicesReply	*rep;
    {
    register char n;

    swaps(&rep->sequenceNumber, n);
    swapl(&rep->length, n);
    WriteToClient(client, size, rep);
    }
