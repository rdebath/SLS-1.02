/* $XConsortium: XSendEv.c,v 1.3 91/07/17 16:17:26 rws Exp $ */
/************************************************************************
 *
 * XSendEv.c.
 * This program does the following:
 *	- opens all extension input devices 
 *	- creates a test window and selects input from those devices in
 *		that window.
 *	- warps the pointer to that window.
 *	- uses XSendExtensionEvent to send all valid extension events to
 *		that window as if they had come from the extension
 *		input devices.
 * 	- compares the contents of extension events it receives to what it sent.
 *
 */

#include <X11/Xlib.h>
#include <X11/extensions/XInput.h>
#include <X11/Xutil.h>
#include "stdio.h"

#define NOPRINT					0
#define PRINTTITLE				1
#define PRINT					2

#define XGetExtensionVersion_code		1
#define XListInputDevices_code			2
#define XOpenDevice_code			3
#define XCloseDevice_code			4
#define XSetDeviceMode_code			5
#define XSelectExtensionEvent_code  		6
#define XGetSelectedExtensionEvents_code	7
#define XChangeDeviceDontPropagateList_code 	8
#define XGetDeviceDontPropagateList_code 	9
#define XGetDeviceMotionEvents_code 		10
#define XChangeKeyboardDevice_code		11
#define XChangePointerDevice_code		12
#define XGrabDevice_code 			13
#define XUngrabDevice_code  			14
#define XGrabDeviceKey_code			15
#define XUngrabDeviceKey_code			16
#define XGrabDeviceButton_code			17
#define XUngrabDeviceButton_code		18
#define XAllowDeviceEvents_code			19
#define XGetDeviceFocus_code			20
#define XSetDeviceFocus_code			21
#define XGetFeedbackControl_code		22
#define XChangeFeedbackControl_code		23
#define XGetDeviceKeyMapping_code		24
#define XChangeDeviceKeyMapping_code		25
#define XGetDeviceModifierMapping_code		26
#define XSetDeviceModifierMapping_code		27
#define XGetDeviceButtonMapping_code		28
#define XSetDeviceButtonMapping_code		29
#define XQueryDeviceState_code 			30

char *ext_errors[] = {"BadDevice","BadEvent","BadMode","DeviceBusy","BadClass"};

char *std_errors[] = {"Success","BadRequest","BadValue","BadWindow","BadPixmap",
		"BadAtom","BadCursor","BadFont","BadMatch","BadDrawable",
		"BadAccess","BadAlloc","BadColor","BadGC","BadIDChoice",
		"BadName","BadLength","BadImplementation"};

char *extfuncs[] = {"undefined", "GetExtensionVersion","ListInputDevices",
"OpenDevice", "CloseDevice","SetDeviceMode","SelectExtensionEvent",
"GetSelectedExtensionEvents", "ChangeDeviceDontPropagateList",
"GetDeviceDontPropagateList","GetDeviceMotionEvents", "ChangeKeyboardDevice",
"ChangePointerDevice","GrabDevice","UngrabDevice", "GrabDeviceKey",
"UngrabDeviceKey","GrabDeviceButton","UngrabDeviceButton", "AllowDeviceEvents",
"GetDeviceFocus","SetDeviceFocus", "GetFeedbackControl",
"ChangeFeedbackControl","GetDeviceKeyMapping","ChangeDeviceKeyMapping",
"GetDeviceModifierMapping","SetDeviceModifierMapping","GetDeviceButtonMapping",
"SetDeviceButtonMapping","QueryDeviceState"};

int	Dflag = 0;
int	gstatus;
int	expect = Success;
int	error_code;
int	major_code;
int	first_err;
Time	currenttime;

int	devicekeypress;
int	devicekeyrelease;
int	devicebuttonpress;
int	devicebuttonrelease;
int	devicemotionnotify;
int	devicefocusin;
int	devicefocusout;
int	proximityin;
int	proximityout;
int	devicemappingnotify;
int	devicestatenotify;
int	changedevicenotify;

struct	classes
    {
    int valid;
    XEventClass class[15];
    } class[256];

int 		ndevices;
XEvent		savev[16];
XDeviceInfoPtr	savlist[256];
XDevice		*sdev[256];

main(argc,argv)
    int argc;
    char *argv[];
    {
    int 		i,j;
    char		*name;
    Display		*display;
    Window		root, my, my2, my3;
    XDeviceInfoPtr 	list_input_devices ();
    XDeviceInfoPtr	list, slist;
    XInputClassInfo	*ip;
    XEvent		event;

    name = argv[0];
    display = XOpenDisplay ("");
    if (display == NULL)
	{
	printf ("No connection to server - aborting test.\n");
	exit(1);
	}
    root = RootWindow (display,0);

    init_error_handler (display);
    create_window (display, root, &my);
    XSelectInput (display, my, PointerMotionMask | EnterWindowMask);
    XSync (display,0);
    XWarpPointer (display, None, my, 0, 0, 200, 200, 10, 10);
    XSync (display,0);
    process_events (display, NOPRINT);
    slist = list_input_devices (display, &ndevices);
    list = slist;
    open_all_devices (display, list, ndevices, my);

    for (i=0; i<ndevices; i++, list++)
	if (list->use != IsXPointer && list->use != IsXKeyboard)
	    select_all_input (display, my, list->name, sdev[i]);

    list = slist;
    for (i=0; i<ndevices; i++, list++)
	if (list->use != IsXPointer && list->use != IsXKeyboard)
	    {
	    send_any_event (display, list->name, sdev[i], savlist[i], my,root);
	    }

    while (XPending (display) > 0)
	{
	XSync (display,0);
	XNextEvent (display,&event);
	process_device_events (&event, PRINT);
	}
    close_input_devices (display, sdev, ndevices);
    if (gstatus == 0)
	printf ("Test of XSendExtensionEvent completed successfully.\n");
    else
	printf ("Test of XSendExtensionEvent failed.\n");
    }

/******************************************************************
 *
 * This function closes all open input devices.
 *
 */

close_input_devices (display, devices, count)
    Display	*display;
    XDevice	*devices[];
    int		count;
    {
    int		i;

    for (i=2; i<count-2; i++)
        XCloseDevice (display, devices[i]);
    }

/******************************************************************
 *
 * This function creates two test windows.
 *
 */

create_window (display, root, my)
    Display *display;
    Window root, *my;
    {   
    XSetWindowAttributes attributes;
    unsigned long 	attribute_mask;
    int 		status;
    XSizeHints 		hints;
    Screen		*screen = XDefaultScreenOfDisplay (display);

    attribute_mask = CWBackPixmap; 
    attribute_mask = CWBackPixel; 
    attribute_mask |= CWEventMask; 
    attribute_mask |= CWDontPropagate; 
    attributes.do_not_propagate_mask = 0;
    attributes.background_pixmap = None;
    attributes.background_pixel = WhitePixel(display, 0);
    attributes.event_mask = ExposureMask;
    
    *my = XCreateWindow (display, root, 100,100, 400,200,1,
	DefaultDepthOfScreen (screen),
	InputOutput, CopyFromParent, attribute_mask, &attributes);

    if (*my == 0) {
	fprintf (stderr, "can't create window!\n");
	exit (1);
    }
    status = XGetNormalHints (display, *my, &hints);
    hints.flags |= (PPosition | PSize | PMinSize);
    hints.x = 100;
    hints.y = 100;
    hints.width = 400;
    hints.height = 200;
    hints.min_width = 400;
    hints.min_height = 200;
    XSetNormalHints (display, *my, &hints);
    XMapWindow (display, *my);
    XFlush(display);

    }

/******************************************************************
 *
 * This function lists all available input devices.
 *
 */

XDeviceInfoPtr
list_input_devices (display, ndevices)
    Display *display;
    int	    *ndevices;
    {
    int			i,j,k;
    XDeviceInfoPtr	list, slist;
    XAnyClassPtr	any;
    XKeyInfoPtr		K;
    XButtonInfoPtr	b;
    XValuatorInfoPtr	v;
    XAxisInfoPtr	a;

    list = (XDeviceInfoPtr) XListInputDevices (display, ndevices);
    slist = list;
    if (Dflag)
	printf ("The number of available input devices is %d\n",*ndevices);
    for (i=0; i<*ndevices; i++, list++)
	{
	savlist[i] = list;
	if (Dflag)
	    {
	    printf ("\nid is %d\n",list->id);
	    printf ("type is %d\n",list->type);
	    printf ("name is %s\n",list->name);
	    printf ("num_classes is %d\n\n",list->num_classes);
	    }
	if (list->num_classes > 0)
	    {
	    any = (XAnyClassPtr) (list->inputclassinfo);
	    for (j=0; j<list->num_classes; j++)
		{
		if (Dflag)
		    {
		    printf ("input class is %d\n", any->class);
		    printf ("length is %d\n", any->length);
		    }
		switch (any->class)
		    {
		    case KeyClass:
			{
			K = (XKeyInfoPtr) any;

			if (Dflag)
			    {
			    printf ("num_keys is %d\n",K->num_keys);
			    printf ("min_keycode is %d\n",K->min_keycode);
			    printf ("max_keycode is %d\n\n",K->max_keycode);
			    }
			}
			break;
		    case ButtonClass:
			{
			b = (XButtonInfoPtr) any;
			if (Dflag)
			    printf ("num_buttons is %d\n\n",b->num_buttons);
			}
			break;
		    case ValuatorClass:
			v = (XValuatorInfoPtr) any;
			a = (XAxisInfoPtr) ((char *) v + 
				sizeof (XValuatorInfo));
			if (Dflag)
			    printf ("num_axes is %d\n\n",v->num_axes);
			for (k=0; k<v->num_axes; k++,a++)
			    {
			    if (Dflag)
				{
				printf ("min_value is %d\n",a->min_value);
				printf ("max_value is %d\n",a->max_value);
				printf ("resolution is %d\n\n",a->resolution);
				}
			    }
			break;
		    default:
			printf ("unknown class\n");
		    }
		any = (XAnyClassPtr) ((char *) any + any->length);
		}
	    }
	}
    return (slist);
    }


/******************************************************************
 *
 * This function finds input class information of a given class.
 *
 */

XAnyClassPtr
FindInputClass (list, class)
    XDeviceInfoPtr	list;
    int			class;
    {
    int			i;
    XAnyClassPtr   	any;

    any = (XAnyClassPtr) (list->inputclassinfo);
    for (i=0; i<list->num_classes; i++)
	{
	if (any->class == class)
	    return (any);
	any = (XAnyClassPtr) ((char *) any + any->length);
	}
    }

/******************************************************************
 *
 * This function opens all extension input devices.
 *
 */

open_all_devices (display, list, ndevices, win)
    Display		*display;
    XDeviceInfoPtr 	list;
    int			ndevices;
    Window		win;
    {
    int			valid;
    int			i,j;
    XDevice		*dev;
    XDevice		*XOpenDevice();
    XInputClassInfo	*ip;

    for (i=0; i<ndevices; i++, list++)
	if (list->use != IsXPointer && list->use != IsXKeyboard)
	    {
	    dev = XOpenDevice (display, list->id);
	    sdev[i] = dev;
	    if (Dflag)
		printf ("\nOpened device %s id is %d\n",
		    list->name, dev->device_id);
	    for (ip= dev->classes, j=0; j<dev->num_classes; j++, ip++)
		{
		if (Dflag)
		    {
		    printf ("class is %d\n",ip->input_class);
		    printf ("event type base is %x\n\n",ip->event_type_base);
		    }
		if (ip->input_class == KeyClass)
		    {
		    XDeviceKeyEvent *kev = (XDeviceKeyEvent *) &(savev[i]);
		    valid = class[dev->device_id].valid++;
	    	    DeviceKeyPress (dev, devicekeypress, 
			class[dev->device_id].class[valid]);
		    valid = class[dev->device_id].valid++;
	    	    DeviceKeyRelease (dev, devicekeyrelease, 
			class[dev->device_id].class[valid]);
		    kev->type = devicekeypress;
		    kev->state = 0;
		    kev->keycode = 0xd;
		    kev->axes_count = 0;
		    kev->window = win;
		    kev->x_root = 160;
		    kev->y_root = 160;
		    kev->deviceid = list->id;
		    kev->time = currenttime;
		    if (Dflag)
			{
			printf ("DeviceKeyPress reports: type=%x class=%x\n",
			    devicekeypress, 
			    class[dev->device_id].class[valid]);
			printf ("DeviceKeyRelease reports: type=%x class=%x\n",
			    devicekeyrelease, 
			    class[dev->device_id].class[valid]);
    		        printf("\n");
			}
		    }
		else if (ip->input_class == ButtonClass)
		    {
		    XDeviceButtonEvent *bev =(XDeviceButtonEvent *) &(savev[i]);
		    valid = class[dev->device_id].valid++;
	    	    DeviceButtonPress (dev, devicebuttonpress, 
			class[dev->device_id].class[valid]);
		    valid = class[dev->device_id].valid++;
	    	    DeviceButtonRelease (dev, devicebuttonrelease, 
			class[dev->device_id].class[valid]);
		    bev->type = devicebuttonpress;
		    bev->state = 0;
		    bev->button = 1;
		    bev->axes_count = 0;
		    bev->window = win;
		    bev->x_root = 160;
		    bev->y_root = 160;
		    bev->deviceid = list->id;
		    bev->time = currenttime;
		    if (Dflag)
			{
			printf ("DeviceButtonPress reports: type=%x class=%x\n",
			    devicebuttonpress, 
			    class[dev->device_id].class[valid-1]);
			printf ("DeviceButtonRelease: type=%x class=%x\n",
			    devicebuttonrelease, 
			    class[dev->device_id].class[valid]);
			printf("\n");
			}
		    }
		else if (ip->input_class == ValuatorClass)
		    {
		    valid = class[dev->device_id].valid++;
	    	    DeviceMotionNotify (dev, devicemotionnotify, 
			class[dev->device_id].class[valid]);
		    if (Dflag)
			printf ("DeviceMotionNotify: type=%x class=%x\n",
			    devicemotionnotify, 
			    class[dev->device_id].class[valid]);
		    }
		else if (ip->input_class == FocusClass)
		    {
		    valid = class[dev->device_id].valid++;
	    	    DeviceFocusIn (dev, devicefocusin, 
			class[dev->device_id].class[valid]);
		    valid = class[dev->device_id].valid++;
	    	    DeviceFocusOut (dev, devicefocusout, 
			class[dev->device_id].class[valid]);
		    if (Dflag)
			{
			printf ("DeviceFocusIn: type=%x class=%x\n",
			    devicefocusin, 
			    class[dev->device_id].class[valid-1]);
			printf ("DeviceFocusOut: type=%x class=%x\n",
			    devicefocusout, class[dev->device_id].class[valid]);
			}
		    }
		else if (ip->input_class == ProximityClass)
		    {
		    valid = class[dev->device_id].valid++;
	    	    ProximityIn (dev, proximityin, 
			class[dev->device_id].class[valid]);
		    valid = class[dev->device_id].valid++;
	    	    ProximityOut (dev, proximityout, 
			class[dev->device_id].class[valid]);
		    if (Dflag)
			{
			printf ("ProximityIn: type=%x class=%x\n",
			    proximityin, class[dev->device_id].class[valid-1]);
			printf ("ProximityOut: type=%x class=%x\n",
			    proximityout, class[dev->device_id].class[valid]);
			}
		    }
		else if (ip->input_class == OtherClass)
		    {
		    valid = class[dev->device_id].valid++;
	    	    DeviceMappingNotify (dev, devicemappingnotify, 
			class[dev->device_id].class[valid]);
		    valid = class[dev->device_id].valid++;
	    	    DeviceStateNotify (dev, devicestatenotify, 
			class[dev->device_id].class[valid]);
		    valid = class[dev->device_id].valid++;
	    	    ChangeDeviceNotify (dev, changedevicenotify, 
			class[dev->device_id].class[valid]);
		    if (Dflag)
			{
			printf ("DeviceMappingNotify: type=%x class=%x\n",
			    devicemappingnotify, 
			    class[dev->device_id].class[valid-2]);
			printf ("DeviceStateNotify: type=%x class=%x\n",
			    devicestatenotify, 
			    class[dev->device_id].class[valid-1]);
			printf ("ChangeDeviceNotify: type=%x class=%x\n",
			    changedevicenotify, 
			    class[dev->device_id].class[valid]);
		        }
		    }
		}
	    }
    }

/******************************************************************
 *
 * This function selects all available input from an extension 
 * device.
 *
 */

select_all_input (display, win, name, dev)
    Display	*display;
    Window	win;
    char	*name;
    XDevice	*dev;
    {
    int			i, j;
    int			status = 0;
    int			this_client_count;
    int			all_clients_count;
    XEventClass		*this_client;
    XEventClass		*all_clients;

    if (Dflag)
	printf ("Selecting input from %s.\n", name);
    XSelectExtensionEvent (display, win, 
	&(class[dev->device_id].class[0]), 
	class[dev->device_id].valid);
    XGetSelectedExtensionEvents (display, win, &this_client_count,
	&this_client, &all_clients_count, &all_clients);
    for (i=0; i<2; i++)
	{
        for (j=0; j<this_client_count; j++)
	    if (*this_client++ == class[dev->device_id].class[i])
		break;
	if (j==this_client_count)
	    status = -1;
	}

    if (Dflag)
	{
	printf ("This_client_count is %d, all_clients_count is %d\n",
	    this_client_count, all_clients_count);
	for (i=0; i<this_client_count; i++)
	    printf ("This_client class[i] is %x\n", *this_client++);
	for (i=0; i<all_clients_count; i++)
	    printf ("All_clients class[i] is %x\n", *all_clients++);
	printf ("\n");
	}

    if (status == 0 && Dflag)
	printf ("Test of XSelect/ XGetSelectedExtensionEvents passed.\n");
    else if (Dflag)
	printf ("Test of XSelect/ XGetSelectedExtensionEvents failed.\n");
    }

/******************************************************************
 *
 * This function ungrabs extension input devices.
 *
 */

send_any_event (display, name, dev, info, win, root)
    Display		*display;
    char		*name;
    XDevice		*dev;
    XDeviceInfoPtr	info;
    Window		win;
    Window		root;
    {
    char		*p1;
    Status		status;
    int			i;
    int			ret;
    XEvent		sendk, sendb, sendm, sendf, sendp, sends,
			sendM, sendc;
    XDeviceKeyEvent		*kev = (XDeviceKeyEvent *) &sendk;
    XDeviceButtonEvent		*bev = (XDeviceButtonEvent *) &sendb;
    XDeviceMotionEvent		*mev = (XDeviceMotionEvent *) &sendm;
    XDeviceFocusChangeEvent	*fev = (XDeviceFocusChangeEvent *) &sendf;
    XProximityNotifyEvent	*pev = (XProximityNotifyEvent *) &sendp;
    XDeviceStateNotifyEvent	*sev = (XDeviceStateNotifyEvent *) &sends;
    XDeviceMappingEvent		*Mev = (XDeviceMappingEvent *) &sendM;
    XChangeDeviceNotifyEvent	*cev = (XChangeDeviceNotifyEvent *) &sendc;

    XAnyClassPtr		any;
    XValuatorStatus		*xv;
    XKeyStatus			*xk;

    for (i=0,p1=(char *) kev; i<sizeof (XEvent); i++)
	*p1++ = 0;
    kev->type = devicekeypress;
    kev->serial =  1;
    kev->send_event = 0;
    kev->display = display;
    kev->root = root;
    kev->x = 10;
    kev->y = 10;
    kev->same_screen = 1;
    kev->subwindow = win;
    kev->deviceid = dev->device_id;
    kev->state = 0;
    kev->keycode = 0xd;
    kev->axes_count = 0;
    kev->window = win;
    kev->x_root = 160;
    kev->y_root = 160;
    kev->time = currenttime;
    kev->device_state = 1;
    kev->axes_count = 6;
    kev->first_axis = 0;
    for (i=0; i<6; i++)
        kev->axis_data[i] = i;

    for (i=0,p1=(char *) bev; i<sizeof (XEvent); i++)
	*p1++ = 0;
    bev->type = devicebuttonpress;
    bev->deviceid = dev->device_id;
    bev->state = 0;
    bev->serial =  1;
    bev->send_event = 0;
    bev->display = display;
    bev->root = root;
    bev->x = 10;
    bev->y = 10;
    bev->same_screen = 1;
    bev->subwindow = win;
    bev->button = 1;
    bev->axes_count = 0;
    bev->window = win;
    bev->x_root = 160;
    bev->y_root = 160;
    bev->time = currenttime;
    bev->device_state = 1;
    bev->axes_count = 6;
    bev->first_axis = 0;
    for (i=0; i<6; i++)
        bev->axis_data[i] = i;

    for (i=0,p1=(char *) mev; i<sizeof (XEvent); i++)
	*p1++ = 0;
    mev->type = devicemotionnotify;
    mev->deviceid = dev->device_id;
    mev->state = 0;
    mev->serial =  1;
    mev->send_event = 0;
    mev->display = display;
    mev->root = root;
    mev->x = 10;
    mev->y = 10;
    mev->same_screen = 1;
    mev->subwindow = win;
    mev->is_hint = 1;
    mev->axes_count = 0;
    mev->window = win;
    mev->x_root = 160;
    mev->y_root = 160;
    mev->time = currenttime;
    mev->device_state = 1;
    mev->axes_count = 6;
    mev->first_axis = 0;
    for (i=0; i<6; i++)
        mev->axis_data[i] = i;

    for (i=0,p1=(char *) pev; i<sizeof (XEvent); i++)
	*p1++ = 0;
    pev->type = proximityin;
    pev->deviceid = dev->device_id;
    pev->state = 0;
    pev->serial =  1;
    pev->send_event = 0;
    pev->display = display;
    pev->root = root;
    pev->x = 10;
    pev->y = 10;
    pev->same_screen = 1;
    pev->subwindow = win;
    pev->axes_count = 0;
    pev->window = win;
    pev->x_root = 160;
    pev->y_root = 160;
    pev->time = currenttime;
    pev->device_state = 1;
    pev->axes_count = 6;
    pev->first_axis = 0;
    for (i=0; i<6; i++)
        pev->axis_data[i] = i;


    for (i=0,p1=(char *) fev; i<sizeof (XEvent); i++)
	*p1++ = 0;
    fev->type = devicefocusin;
    fev->deviceid = dev->device_id;
    fev->serial =  1;
    fev->send_event = 0;
    fev->display = display;
    fev->window = win;
    fev->time = currenttime;
    fev->mode = NotifyGrab;
    fev->detail = NotifyPointerRoot;

    for (i=0,p1=(char *) Mev; i<sizeof (XEvent); i++)
	*p1++ = 0;
    Mev->type = devicemappingnotify;
    Mev->deviceid = dev->device_id;
    Mev->serial =  1;
    Mev->send_event = 0;
    Mev->display = display;
    Mev->time = currenttime;
    Mev->first_keycode = 10;
    Mev->request = MappingKeyboard;
    Mev->count = 100;

    for (i=0,p1=(char *) cev; i<sizeof (XEvent); i++)
	*p1++ = 0;
    cev->type = changedevicenotify;
    cev->deviceid = dev->device_id;
    cev->serial =  1;
    cev->send_event = 0;
    cev->display = display;
    cev->time = currenttime;
    cev->request = NewKeyboard;

    for (i=0,p1=(char *) sev; i<sizeof (XEvent); i++)
	*p1++ = 0;
    sev->type = devicestatenotify;
    sev->deviceid = dev->device_id;
    sev->display = display;
    sev->time = currenttime;
    sev->num_classes = 2;

    xk = (XKeyStatus *) sev->data;
    xk-> class = KeyClass;
    xk-> length = sizeof (XKeyStatus);
    xk-> num_keys = 256;
    for (i=0; i<32; i++)
        xk->keys[i] = i;

    xv = (XValuatorStatus *) ++xk;
    xv-> class = ValuatorClass;
    xv-> length = sizeof (XValuatorStatus);
    xv-> num_valuators = 6;
    for (i=0; i<6; i++)
        xv->valuators[i] = i;

    any = (XAnyClassPtr) (info->inputclassinfo);
    for (i=0; i<info->num_classes; i++)
	{
	switch (any->class)
	    {
	    case KeyClass:
		status = XSendExtensionEvent (display, dev, win, True, 
			class[dev->device_id].valid, 
			&class[dev->device_id].class[0], kev);
    		XSync (display, 0);
		if (status == 0)
		    printf ("Event Conversion failed.\n");
    		else
		    compare_events (display, kev, sizeof(XDeviceKeyEvent));

		status = XSendExtensionEvent (display, dev, win, True, 
			class[dev->device_id].valid, 
			&class[dev->device_id].class[0], sev);
    		XSync (display, 0);
		if (status == 0)
		    printf ("Event Conversion failed.\n");
    		else
		    compare_events (display, sev,
			sizeof(XDeviceStateNotifyEvent));
	        break;
	    case ValuatorClass:
		status = XSendExtensionEvent (display, dev, win, True, 
			class[dev->device_id].valid, 
			&class[dev->device_id].class[0], mev);
    		XSync (display, 0);
		if (status == 0)
		    printf ("Event Conversion failed.\n");
    		else
		    compare_events (display, mev, sizeof(XDeviceMotionEvent));

		if (proximityin)
		    {
		    status = XSendExtensionEvent (display, dev, win, True, 
			class[dev->device_id].valid, 
			&class[dev->device_id].class[0], pev);
    		    XSync (display, 0);
		    if (status == 0)
		        printf ("Event Conversion failed.\n");
    		    else
		        compare_events (display, pev, 
				sizeof (XProximityNotifyEvent));
		    }
	        break;
	    case ButtonClass:
		status = XSendExtensionEvent (display, dev, win, True, 
			class[dev->device_id].valid, 
			&class[dev->device_id].class[0], bev);
    		XSync (display, 0);
		if (status == 0)
		    printf ("Event Conversion failed.\n");
    		else
		    compare_events (display, bev, sizeof (XDeviceButtonEvent));
	        break;
	    }
	any = (XAnyClassPtr) ((char *) any + any->length);
	}

    status = XSendExtensionEvent (display, dev, win, True, 
	class[dev->device_id].valid, &class[dev->device_id].class[0], fev);
    XSync (display, 0);
    if (status == 0)
	printf ("Event Conversion failed.\n");
    else
	compare_events (display, fev, sizeof (XDeviceFocusChangeEvent));

    status = XSendExtensionEvent (display, dev, win, True, 
	class[dev->device_id].valid, &class[dev->device_id].class[0], Mev);
    XSync (display, 0);
    if (status == 0)
	printf ("Event Conversion failed.\n");
    else
	compare_events (display, Mev, sizeof (XDeviceMappingEvent));

    status = XSendExtensionEvent (display, dev, win, True, 
	class[dev->device_id].valid, &class[dev->device_id].class[0], cev);
    XSync (display, 0);
    if (status == 0)
	printf ("Event Conversion failed.\n");
    else
	compare_events (display, cev, sizeof (XChangeDeviceNotifyEvent));
    }

compare_events (display, sendp, expect)
    Display *display;
    XEvent *sendp;
    int expect;
    {
    XEvent receive;
    int i, status;
    char *p1, *p2;

    while (XPending (display) > 0)
	{
	XNextEvent (display, &receive);
	p1 = (char *) sendp;
	p2 = (char *) &receive;
	for (i=0; i<expect; i++,p1++,p2++)
	    if (*p1 != *p2 && (i<4 || i>11))
		break;
	if (i == expect)
	    status = 0;
	else
	    status = -1;
	}
    if (status != 0)
	{
	printf ("Sent and received events did not compare correctly.\n");
	gstatus = status;
	}
    }

/******************************************************************
 *
 * This function displays the contents of extension events.
 *
 */

process_device_events (event, mode)
    XEvent	*event;
    int		mode;
    {
    int				i, j;
    char			*buf;
    XKeyStatus			*kdata;
    XButtonStatus		*bdata;
    XValuatorStatus		*vdata;
    XDeviceKeyEvent		*k;
    XDeviceButtonEvent		*b;
    XDeviceMappingEvent		*m;
    XDeviceMotionEvent		*M;
    XDeviceFocusChangeEvent	*f;
    XProximityNotifyEvent	*p;
    XChangeDeviceNotifyEvent	*c;
    XDeviceStateNotifyEvent	*s;
    XInputClass 		*anyclass;

    if (event->type == MotionNotify)
	{
	XMotionEvent	*mo;
	mo = (XMotionEvent * ) event;
	currenttime = mo->time;
	}
    else if (event->type == EnterNotify)
	{
	XEnterWindowEvent	*en;
	en = (XEnterWindowEvent * ) event;
	currenttime = en->time;
	}
    else if (event->type == devicekeypress)
	{
	k = (XDeviceKeyEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device key press event device=%d\n", k->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =        %d\n", k->type);
	    printf ("     serial =      %ld\n", k->serial);
	    printf ("     send_event =  %ld\n", k->send_event);
	    printf ("     display =     %x\n", k->display);
	    printf ("     window =      %x\n", k->window);
	    printf ("     root =        %x\n", k->root);
	    printf ("     subwindow =   %x\n", k->subwindow);
	    printf ("     time =        %x\n", k->time);
	    printf ("     x =           %d\n", k->x);
    	    printf ("     y =           %d\n", k->y);
	    printf ("     x_root =      %d\n", k->x_root);
	    printf ("     y_root =      %d\n", k->y_root);
	    printf ("     state =       %d\n", k->state);
	    printf ("     keycode =     %x\n", k->keycode);
	    printf ("     same_screen = %d\n", k->same_screen);
	    printf ("     first_axis  = %d\n", k->first_axis);
	    printf ("     axes_count  = %d\n", k->axes_count);
	    for (i=0; i<k->axes_count; i++)
	        printf ("     axis_data[%d]= %d\n", i, k->axis_data[i]);
	    }
        if (k->keycode == 0xd)
	    return (-1);
	}
    else if (event->type == devicekeyrelease)
	{
	k = (XDeviceKeyEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device key release event device=%d\n", k->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =        %d\n", k->type);
	    printf ("     serial =      %ld\n", k->serial);
	    printf ("     send_event =  %ld\n", k->send_event);
	    printf ("     display =     %x\n", k->display);
	    printf ("     window =      %x\n", k->window);
	    printf ("     root =        %x\n", k->root);
	    printf ("     subwindow =   %x\n", k->subwindow);
	    printf ("     time =        %x\n", k->time);
	    printf ("     x =           %d\n", k->x);
	    printf ("     y =           %d\n", k->y);
	    printf ("     x_root =      %d\n", k->x_root);
	    printf ("     y_root =      %d\n", k->y_root);
	    printf ("     state =       %d\n", k->state);
	    printf ("     keycode =     %x\n", k->keycode);
	    printf ("     same_screen = %d\n", k->same_screen);
	    printf ("     first_axis  = %d\n", k->first_axis);
	    printf ("     axes_count  = %d\n", k->axes_count);
	    for (i=0; i<k->axes_count; i++)
	        printf ("     axis_data[%d]= %d\n", i, k->axis_data[i]);
	    }
        if (k->keycode == 0xd)
	    return (-1);
	}
    else if (event->type == devicebuttonpress)
	{
	b = (XDeviceButtonEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device button press event device=%d\n", b->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =        %d\n", b->type);
	    printf ("     serial =      %ld\n", b->serial);
	    printf ("     send_event =  %ld\n", b->send_event);
	    printf ("     display =     %x\n", b->display);
	    printf ("     window =      %x\n", b->window);
	    printf ("     root =        %x\n", b->root);
	    printf ("     subwindow =   %x\n", b->subwindow);
	    printf ("     time =        %x\n", b->time);
	    printf ("     x =           %d\n", b->x);
	    printf ("     y =           %d\n", b->y);
	    printf ("     x_root =      %d\n", b->x_root);
	    printf ("     y_root =      %d\n", b->y_root);
	    printf ("     state =       %d\n", b->state);
	    printf ("     button =      %x\n", b->button);
	    printf ("     same_screen = %d\n", b->same_screen);
	    printf ("     first_axis  = %d\n", b->first_axis);
	    printf ("     axes_count  = %d\n", b->axes_count);
	    for (i=0; i<b->axes_count; i++)
	        printf ("     axis_data[%d]= %d\n", i, b->axis_data[i]);
	    }
	}
    else if (event->type == devicebuttonrelease)
	 {
	b = (XDeviceButtonEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device button release event device=%d\n", b->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =        %d\n", b->type);
	    printf ("     serial =      %ld\n", b->serial);
	    printf ("     send_event =  %ld\n", b->send_event);
	    printf ("     display =     %x\n", b->display);
	    printf ("     window =      %x\n", b->window);
	    printf ("     root =        %x\n", b->root);
	    printf ("     subwindow =   %x\n", b->subwindow);
	    printf ("     time =        %x\n", b->time);
	    printf ("     x =           %d\n", b->x);
	    printf ("     y =           %d\n", b->y);
	    printf ("     x_root =      %d\n", b->x_root);
	    printf ("     y_root =      %d\n", b->y_root);
	    printf ("     state =       %d\n", b->state);
	    printf ("     button =      %x\n", b->button);
	    printf ("     same_screen = %d\n", b->same_screen);
	    printf ("     first_axis  = %d\n", b->first_axis);
	    printf ("     axes_count  = %d\n", b->axes_count);
	    for (i=0; i<b->axes_count; i++)
	        printf ("     axis_data[%d]= %d\n", i, b->axis_data[i]);
            }
        }
    else if (event->type == devicemotionnotify)
	{
	M = (XDeviceMotionEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device motion event device=%d\n", M->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =          %d\n", M->type);
	    printf ("     serial =        %ld\n", M->serial);
	    printf ("     send_event =    %ld\n", M->send_event);
	    printf ("     display =       %x\n", M->display);
	    printf ("     window =        %x\n", M->window);
	    printf ("     root =        %x\n", M->root);
	    printf ("     subwindow =   %x\n", M->subwindow);
	    printf ("     time =        %x\n", M->time);
	    printf ("     x =           %d\n", M->x);
	    printf ("     y =           %d\n", M->y);
	    printf ("     x_root =      %d\n", M->x_root);
	    printf ("     y_root =      %d\n", M->y_root);
	    printf ("     state =       %d\n", M->state);
	    printf ("     is_hint =     %x\n", M->is_hint);
	    printf ("     same_screen = %d\n", M->same_screen);
	    printf ("     axes_count  = %d\n", M->axes_count);
	    printf ("     first_axis  = %d\n", M->first_axis);
	    for (i=0; i<M->axes_count; i++)
	        printf ("     axis_data[%d]= %d\n", i, M->axis_data[i]);
	    }
	}
    else if (event->type == devicefocusin || event->type == devicefocusout)
	{
	f = (XDeviceFocusChangeEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device focus event device=%d\n", f->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =          %d\n", f->type);
	    printf ("     serial =        %ld\n", f->serial);
	    printf ("     send_event =    %ld\n", f->send_event);
	    printf ("     display =       %x\n", f->display);
	    printf ("     window =        %x\n", f->window);
	    printf ("     time =          %x\n", f->time);
	    printf ("     mode =          %x\n", f->mode);
	    printf ("     detail =        %x\n", f->detail);
	    }
	}
    else if (event->type == proximityin || event->type == proximityout)
	{
	p = (XProximityNotifyEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device proximity event device=%d\n", p->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =          %d\n", p->type);
	    printf ("     serial =        %ld\n", p->serial);
	    printf ("     send_event =    %ld\n", p->send_event);
	    printf ("     display =       %x\n", p->display);
	    printf ("     window =        %x\n", p->window);
	    printf ("     root =        %x\n", p->root);
	    printf ("     subwindow =   %x\n", p->subwindow);
	    printf ("     time =        %x\n", p->time);
	    printf ("     x =           %d\n", p->x);
	    printf ("     y =           %d\n", p->y);
	    printf ("     x_root =      %d\n", p->x_root);
	    printf ("     y_root =      %d\n", p->y_root);
	    printf ("     state =       %d\n", p->state);
	    printf ("     same_screen = %d\n", p->same_screen);
	    printf ("     axes_count  = %d\n", p->axes_count);
	    printf ("     first_axis  = %d\n", p->first_axis);
	    for (i=0; i<p->axes_count; i++)
	        printf ("     axis_data[%d]= %d\n", i, p->axis_data[i]);
	    }
	}
    else if (event->type == devicemappingnotify)
	{
	m = (XDeviceMappingEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device mapping event device=%d\n", m->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =          %d\n", m->type);
	    printf ("     serial =        %ld\n", m->serial);
	    printf ("     send_event =    %ld\n", m->send_event);
	    printf ("     display =       %x\n", m->display);
	    printf ("     window =        %x\n", m->window);
	    printf ("     time =          %x\n", m->time);
	    printf ("     request =       %x\n", m->request);
	    printf ("     first_keycode = %x\n", m->first_keycode);
	    printf ("     count =         %x\n", m->count);
	    }
	}
    else if (event->type == devicestatenotify)
	{
	s = (XDeviceStateNotifyEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device state notify event device=%d\n", s->deviceid);
	if (mode == PRINT)
	{
	printf ("     type =          %d\n", s->type);
	printf ("     serial =        %ld\n", s->serial);
	printf ("     send_event =    %ld\n", s->send_event);
	printf ("     display =       %x\n", s->display);
	printf ("     window =        %x\n", s->window);
	printf ("     time =          %x\n", s->time);
	printf ("     num_classes =   %x\n", s->num_classes);
	printf ("     data =          %x\n", s->data);
	anyclass = (XInputClass *) s->data;
	for (i=0; i<s->num_classes; i++)
	    {
	    switch (anyclass->class)
		{
		case KeyClass:
		    kdata = (XKeyStatus *) anyclass;
		    printf ("num_keys is %d\n",kdata->num_keys);
		    for (j=0; j<kdata->num_keys/8; j++)
			printf ("%x ", *(kdata->keys+j));
		    printf ("\n");
		    anyclass = (XInputClass *) (++kdata);
		    break;
		case ButtonClass:
		    bdata = (XButtonStatus *) anyclass;
		    printf ("num_buttons is %d\n",bdata->num_buttons);
		    for (j=0; j<bdata->num_buttons/8; j++)
			printf ("%x ", *(bdata->buttons+j));
		    printf ("\n");
		    anyclass = (XInputClass *) (++bdata);
		    break;
		case ValuatorClass:
		    vdata = (XValuatorStatus *) anyclass;
		    printf ("num_valuators is %d\n",vdata->num_valuators);
		    for (j=0; j<vdata->num_valuators; j++)
		        printf ("valuator %d has value %x\n",
				j, *(vdata->valuators+j));
		    anyclass = (XInputClass *) (++vdata);
		    break;
	        }
	    }
	}
	}
    else if (event->type == changedevicenotify)
	{
	c = (XChangeDeviceNotifyEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Device change event device=%d\n", c->deviceid);
	if (mode == PRINT)
	    {
	    printf ("     type =          %d\n", c->type);
	    printf ("     serial =        %ld\n", c->serial);
	    printf ("     send_event =    %ld\n", c->send_event);
	    printf ("     display =       %x\n", c->display);
	    printf ("     window =        %x\n", c->window);
	    printf ("     time =          %x\n", c->time);
	    printf ("     request =       %x\n", c->request);
	    }
	}
    else if (event->type == Expose)
	{
	if (mode > NOPRINT)
	    printf ("Expose event\n");
	return (-1);
	}
    else 
	{
	buf = (char *) event;
	printf ("unknown event!\n");
	for (i=0; i<32; i++)
	    printf ("%x ",buf[i]);
        return (1);
	}
    return (0);
    }

/***********************************************************************
 *
 * This function initializes an X error handler.
 *
 */

init_error_handler (disp)
    Display	*disp;
    {
    int 		event;
    int 		handle_x_errors();

    XQueryExtension (disp, "XInputExtension", &major_code, &event, &first_err);
    XSetErrorHandler (handle_x_errors);
    }

/***********************************************************************
 *
 * This function handles X errors.
 *
 */

handle_x_errors (disp, error)
    Display	*disp;
    XErrorEvent *error;
    {
    char buf[256];

    if (error->request_code == major_code &&
        error->minor_code == error_code &&
        error->error_code == expect)
        if (error->error_code >= first_err &&
            error->error_code <= first_err+4)
	    {
	    if (Dflag)
                printf ("%s returned %s correctly.\n",
		    extfuncs[error->minor_code],
		    ext_errors[error->error_code-first_err]);
	    return;
	    }
	else
	    {
	    if (Dflag)
                printf ("%s returned %s correctly.\n",
		    extfuncs[error->minor_code],
	    	    std_errors[error->error_code]);
	    return;
	    }
    if (error->request_code == major_code)
        printf ("    Minor code=%s.\n",extfuncs[error->minor_code]);
    else
        printf ("    Minor code=%d.\n",error->minor_code);
	
    if (error->error_code >= Success &&
        error->error_code <= BadImplementation)
        printf ("    Error code=%s.\n",std_errors[error->error_code]);
    else if (error->error_code >= first_err &&
        error->error_code <= first_err+4)
        printf ("    Error code=%s.\n",ext_errors[error->error_code-first_err]);
    else
        printf ("    Error code=%d.\n",error->error_code);
    printf ("    Resource id=%d.\n",error->resourceid);
    return (0);
    }

process_events (display, mode)
    Display *display;
    int	mode;
    {
    int	ret;
    int	count = 0;
    XEvent event;

    XSync (display,0);
    while (XPending (display) > 0)
	{
	XNextEvent (display,&event);
	process_device_events (&event, mode);
	count++;
	}
    }
