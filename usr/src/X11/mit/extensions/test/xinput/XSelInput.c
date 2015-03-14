/* $XConsortium: XSelInput.c,v 1.2 91/02/20 09:17:58 rws Exp $ */
/************************************************************************
 *
 * XSelInput.c   - Test case for XSelectInput function.
 * Purpose:	 - Turns on all available input devices and selects input from
 * 		   them.
 */

#include <X11/Xlib.h>
#include <X11/extensions/XI.h>
#include <X11/extensions/XInput.h>
#include <X11/Xutil.h>
#include "stdio.h"
#include <X11/keysym.h>


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
"GetDeviceFocus","SetDeviceFocus","GetFeedbackControl",
"ChangeFeedbackControl","GetDeviceKeyMapping","ChangeDeviceKeyMapping",
"GetDeviceModifierMapping","SetDeviceModifierMapping","GetDeviceButtonMapping",
"SetDeviceButtonMapping","QueryDeviceState"};

int	Dflag = 0;

int	gstatus;
int	expect = Success;
int	error_code;
int	major_code;
int	first_err;
int 	ndevices;

int	devicekeypressflag;
int	devicekeyreleaseflag;
int	devicebuttonpressflag;
int	devicebuttonreleaseflag;
int	devicebuttonpressgrabflag;
int	devicebutton1motionflag;
int	devicebutton2motionflag;
int	devicebutton3motionflag;
int	devicebutton4motionflag;
int	devicebutton5motionflag;
int	devicebuttonmotionflag;
int	devicemotionnotifyflag;
int	devicemotionhintflag;
int	devicefocuschangeflag;
int	proximityinflag;
int	proximityoutflag;
int	devicemappingnotifyflag;
int	devicestatenotifyflag;
int	changedevicenotifyflag;
int	deviceownergrabbuttonflag;
int	devicebuttonpressgrabflag;

int	devicekeypress;
int	devicekeyrelease;
int	devicebuttonpress;
int	devicebuttonrelease;
int	devicebuttonpressgrab;
int	devicebutton1motion;
int	devicemotionnotify;
int	devicepointermotionhint;
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

XDeviceInfoPtr	savlist[256];
XDevice		*sdev[256];
XDevice		*newpointer = NULL;
XDevice		*newkeyboard = NULL;

Window		root;
int		printmode = 1;

main(argc,argv)
    int argc;
    char *argv[];
    {
    int 		i,j,count;
    char		*name;
    Display		*display, *display2, *display3;
    Window		my;
    XDeviceInfoPtr 	list_input_devices ();
    XDeviceInfoPtr	list, slist;
    XInputClassInfo	*ip;
    XEvent		event;

    name = argv[0];
    process_args (argc, argv);
    display = XOpenDisplay ("");
    if (display == NULL)
	{
	printf ("No connection to server - aborting test.\n");
	exit(1);
	}
    display2 = XOpenDisplay ("");
    if (display2 == NULL)
	{
	printf ("No second connection to server - aborting test.\n");
	exit(1);
	}
    display3 = XOpenDisplay ("");
    if (display3 == NULL)
	{
	printf ("No second connection to server - aborting test.\n");
	exit(1);
	}
    root = RootWindow (display,0);

    init_error_handler (display);
    create_window (display, root, &my);
    XSelectInput (display, my, KeyReleaseMask | KeyReleaseMask);
    slist = list_input_devices (display, &ndevices);
    list = slist;
    open_all_devices (display, list, ndevices);

    for (i=0; i<ndevices; i++, list++)
	if (list->use != IsXKeyboard && list->use != IsXPointer)
	    {
	    select_all_input (display, my, list->name, sdev[i]);
	    select_all_input (display2, my, list->name, sdev[i]);
	    select_all_input (display3, my, list->name, sdev[i]);
	    }

    for (count=0;;count++)
	{
	while (XPending (display) > 0)
	    {
	    XNextEvent (display,&event);
	    if (process_device_events (display, 1, &event, printmode) == -1)
		{
		XDestroyWindow (display, my);
		XSync (display,0);
        	close_input_devices (display, slist, sdev, ndevices);
		exit(1);
		}
	    }
	while (XPending (display2) > 0)
	    {
	    XNextEvent (display2,&event);
	    process_device_events (display, 2, &event, printmode);
	    }
	while (XPending (display3) > 0)
	    {
	    XNextEvent (display3,&event);
	    process_device_events (display, 3, &event, printmode);
	    }
	}
    }

/******************************************************************
 *
 * This function closes all open input devices.
 *
 */

close_input_devices (display, list, devices, count)
    Display		*display;
    XDeviceInfoPtr	list;
    XDevice		*devices[];
    int			count;
    {
    int		i;

    for (i=0; i<count; i++, list++)
	if (list->use != IsXKeyboard && list->use != IsXPointer)
            XCloseDevice (display, devices[i]);
    }

/******************************************************************
 *
 * This function creates a test windows.
 *
 */

create_window (display, root, my)
    Display *display;
    Window root, *my;
    {   
    XWindowAttributes attr;
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
    hints.x = 100;
    hints.y = 100;
    hints.width = 400;
    hints.height = 200;
    hints.min_width = 400;
    hints.min_height = 200;
    hints.flags |= (PPosition | PSize | PMinSize);
    XSetNormalHints (display, *my, &hints);
    XMapWindow (display, *my);
    XFlush(display);
    for (;;)
	{
	XGetWindowAttributes (display, *my, &attr);
	if (attr.map_state == IsViewable)
	    break;
	}
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
	    if (list->use == IsXKeyboard)
	        printf ("Device %s is the X keyboard.\n",list->name);
	    else if (list->use == IsXPointer)
	        printf ("Device %s is the X pointer.\n",list->name);
	    else if (list->use == IsXExtensionDevice)
	        printf ("Device %s is an extension device.\n",list->name);
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
			K = (XKeyInfoPtr) any;
			if (Dflag)
			    {
			    printf ("num_keys is %d\n",K->num_keys);
			    printf ("min_keycode is %d\n",K->min_keycode);
			    printf ("max_keycode is %d\n\n",K->max_keycode);
			    }
			break;
		    case ButtonClass:
			b = (XButtonInfoPtr) any;
			if (Dflag)
			    printf ("num_buttons is %d\n\n",b->num_buttons);
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
 * This function opens all extension input devices.
 *
 */

open_all_devices (display, list, ndevices)
    Display		*display;
    XDeviceInfoPtr 	list;
    int			ndevices;
    {
    struct		classes	*cp;
    int			i,j;
    XDevice		*dev;
    XDevice		*XOpenDevice();
    XInputClassInfo	*ip;

    for (i=0; i<ndevices; i++, list++)
	if (list->use != IsXKeyboard &&
	    list->use != IsXPointer)
	    {
	    dev = XOpenDevice (display, list->id);
	    sdev[i] = dev;
	    if (Dflag)
		printf ("\nOpened device %s id is %d\n",
		    list->name, dev->device_id);
	    cp = &class[dev->device_id];
	    for (ip= dev->classes, j=0; j<dev->num_classes; j++, ip++)
		{
		if (Dflag)
		    {
		    printf ("class is %d\n",ip->input_class);
		    printf ("event type base is %x\n\n",ip->event_type_base);
		    }
		if (ip->input_class == KeyClass)
		    {
		    if (newkeyboard == NULL)
		        newkeyboard=dev;
	    	    DeviceKeyPress (dev, devicekeypress, cp->class[cp->valid]);
		    if (devicekeypressflag)
		        cp->valid++;
	    	    DeviceKeyRelease (dev, devicekeyrelease,  
			cp->class[cp->valid]);
		    if (devicekeyreleaseflag)
			cp->valid++;
		    if (Dflag)
			{
			printf ("DeviceKeyPress reports: type=%x class=%x\n",
			    devicekeypress, cp->class[cp->valid-1]);
			printf ("DeviceKeyRelease reports: type=%x class=%x\n",
			    devicekeyrelease, cp->class[cp->valid]);
    		        printf("\n");
			}
		    }
		else if (ip->input_class == ButtonClass)
		    {
		    if (newpointer == NULL)
		        newpointer=dev;
	    	    DeviceButtonPress (dev, devicebuttonpress, 
			cp->class[cp->valid]);
		    if (devicebuttonpressflag)
			cp->valid++;
	    	    DeviceButtonRelease (dev, devicebuttonrelease, 
			cp->class[cp->valid]);
		    if (devicebuttonreleaseflag)
			cp->valid++;
	    	    DeviceButtonPressGrab (dev, devicebuttonpressgrab, 
			cp->class[cp->valid]);
		    if (devicebuttonpressgrabflag)
			cp->valid++;
	    	    DeviceButton1Motion (dev, devicebutton1motion, 
			cp->class[cp->valid]);
		    if (devicebutton1motionflag)
			cp->valid++;
		    if (Dflag)
			{
			printf ("DeviceButtonPress reports: type=%x class=%x\n",
			    devicebuttonpress, cp->class[cp->valid-2]);
			printf ("DeviceButtonRelease: type=%x class=%x\n",
			    devicebuttonrelease, cp->class[cp->valid-1]);
			printf ("DeviceButtonPressGrab: type=%x class=%x\n",
			    devicebuttonpressgrab, cp->class[cp->valid]);
			printf("\n");
			}
		    }
		else if (ip->input_class == ValuatorClass)
		    {
	    	    DeviceMotionNotify (dev, devicemotionnotify, 
			cp->class[cp->valid]);
		    if (devicemotionnotifyflag)
			cp->valid++;
	    	    DevicePointerMotionHint (dev, devicepointermotionhint, 
			cp->class[cp->valid]);
		    if (devicemotionhintflag)
			cp->valid++;
		    if (Dflag)
			{
			printf ("DeviceMotionNotify: type=%x class=%x\n",
			    devicemotionnotify, cp->class[cp->valid-1]);
			printf ("DevicePointerMotionHint: type=%x class=%x\n",
			    devicepointermotionhint, cp->class[cp->valid]);
			}
		    }
		else if (ip->input_class == FocusClass)
		    {
	    	    DeviceFocusIn (dev, devicefocusin, cp->class[cp->valid]);
		    if (devicefocuschangeflag)
			cp->valid++;
	    	    DeviceFocusOut (dev, devicefocusout,  cp->class[cp->valid]);
		    if (devicefocuschangeflag)
			cp->valid++;
		    if (Dflag)
			{
			printf ("DeviceFocusIn: type=%x class=%x\n",
			    devicefocusin, cp->class[cp->valid-1]);
			printf ("DeviceFocusOut: type=%x class=%x\n",
			    devicefocusout, cp->class[cp->valid-1]);
			}
		    }
		else if (ip->input_class == ProximityClass)
		    {
	    	    ProximityIn (dev, proximityin, cp->class[cp->valid]);
		    if (proximityinflag)
			cp->valid++;
	    	    ProximityOut (dev, proximityout, cp->class[cp->valid]);
		    if (proximityoutflag)
			cp->valid++;
		    if (Dflag)
			{
			printf ("ProximityIn: type=%x class=%x\n",
			    proximityin, cp->class[cp->valid-1]);
			printf ("ProximityOut: type=%x class=%x\n",
			    proximityout, cp->class[cp->valid]);
			}
		    }
		else if (ip->input_class == OtherClass)
		    {
	    	    DeviceMappingNotify (dev, devicemappingnotify, 
			cp->class[cp->valid]);
		    if (devicemappingnotifyflag)
			cp->valid++;
	    	    DeviceStateNotify (dev, devicestatenotify, 
			cp->class[cp->valid]);
		    if (devicestatenotifyflag)
			cp->valid++;
	    	    ChangeDeviceNotify (dev, changedevicenotify, 
			cp->class[cp->valid]);
		    if (changedevicenotifyflag)
			cp->valid++;
		    if (Dflag)
			{
			printf ("DeviceMappingNotify: type=%x class=%x\n",
			    devicemappingnotify, cp->class[cp->valid-2]);
			printf ("DeviceStateNotify: type=%x class=%x\n",
			    devicestatenotify, cp->class[cp->valid-1]);
			printf ("ChangeDeviceNotify: type=%x class=%x\n",
			    changedevicenotify, cp->class[cp->valid]);
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
    XEventClass		*this_client, *s_this;
    XEventClass		*all_clients, *s_all;

    if (Dflag)
	printf ("Selecting input from %s.\n", name);
    XSelectExtensionEvent (display, win, 
	&(class[dev->device_id].class[0]), 
	class[dev->device_id].valid);
    XGetSelectedExtensionEvents (display, win, &this_client_count,
	&this_client, &all_clients_count, &all_clients);
   
    s_this = this_client;
    s_all = all_clients;
    for (i=0; i<class[dev->device_id].valid; i++)
	{
	this_client = s_this;
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
	this_client = s_this;
	all_clients = s_all;
	for (i=0; i<this_client_count; i++)
	    printf ("This_client class[i] is %x\n", *this_client++);
	for (i=0; i<all_clients_count; i++)
	    printf ("All_clients class[i] is %x\n", *all_clients++);
	printf ("\n");
	}

    if (status == 0)
	printf ("Test of XSelect/ XGetSelectedExtensionEvents passed.\n");
    else
	printf ("Test of XSelect/ XGetSelectedExtensionEvents failed.\n");
    }

/******************************************************************
 *
 * This function displays the contents of extension events.
 *
 */

process_device_events (display, clientno, event, mode)
    Display	*display;
    int		clientno;
    XEvent	*event;
    int		mode;
    {
    int				i, j;
    char			*buf;
    XKeyStatus			*kdata;
    XButtonStatus		*bdata;
    XValuatorStatus		*vdata;
    XKeyEvent			*K;
    XDeviceKeyEvent		*k;
    XDeviceButtonEvent		*b;
    XDeviceMappingEvent		*m;
    XDeviceMotionEvent		*M;
    XDeviceFocusChangeEvent	*f;
    XProximityNotifyEvent	*p;
    XChangeDeviceNotifyEvent	*c;
    XDeviceStateNotifyEvent	*s;
    XInputClass 		*anyclass;
    KeySym			sym;

    if (event->type == KeyRelease)
	{
	K = (XKeyReleasedEvent *) event;
	sym = XKeycodeToKeysym (display, K->keycode, 0);
	if (sym == XK_space)
	    return (-1);
	}
    else if (event->type == MappingNotify)
	{
	if (mode > NOPRINT)
	    printf ("MappingNotify event.\n");
	}
    else if (event->type == devicekeypress)
	{
	k = (XDeviceKeyEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Client: %d Device key press event device=%d\n", 
		clientno, k->deviceid);
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
	    printf ("     device_state =%d\n", k->device_state);
	    printf ("     axes_count  = %d\n", k->axes_count);
	    printf ("     first_axis  = %d\n", k->first_axis);
	    }
	}
    else if (event->type == devicekeyrelease)
	{
	k = (XDeviceKeyEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Client: %d Device key release event device=%d\n", 
		clientno, k->deviceid);
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
	    }
        if (k->keycode == 0xd)
	    return (-1);
	}
    else if (event->type == devicebuttonpress)
	{
	b = (XDeviceButtonEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Client: %d Device button press event device=%d\n", 
		clientno, b->deviceid);
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
	    printf ("     device_state =%d\n", b->device_state);
	    printf ("     axes_count  = %d\n", b->axes_count);
	    printf ("     first_axis  = %d\n", b->first_axis);
	    }
	}
    else if (event->type == devicebuttonrelease)
	 {
	b = (XDeviceButtonEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Client: %d Device button release event device=%d\n", 
		clientno, b->deviceid);
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
            }
        }
    else if (event->type == devicemotionnotify)
	{
	M = (XDeviceMotionEvent * ) event;
	if (mode > NOPRINT)
	    {
	    printf ("Client: %d Device motion event device=%d\n", 
		clientno, M->deviceid);
	    printf ("     is_hint =     %x\n", M->is_hint);
	    }
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
	    printf ("     same_screen = %d\n", M->same_screen);
	    printf ("     device_state =%d\n", M->device_state);
	    printf ("     axes_count  = %d\n", M->axes_count);
	    printf ("     first_axis  = %d\n", M->first_axis);
	    }
	}
    else if (event->type == devicefocusin || event->type == devicefocusout)
	{
	f = (XDeviceFocusChangeEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Client: %d Device focus event device=%d\n", 
		clientno, f->deviceid);
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
	    printf ("Client: %d Device proximity event device=%d\n", 
		clientno, p->deviceid);
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
	    }
	}
    else if (event->type == devicemappingnotify)
	{
	m = (XDeviceMappingEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Client: %d Device mapping event device=%d\n", 
		clientno, m->deviceid);
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
	    printf ("Client: %d Device state notify event device=%d\n", 
		clientno, s->deviceid);
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
	anyclass = (XInputClass *) &s->data[0];
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
		    anyclass = (XInputClass *) ++kdata;
		    break;
		case ButtonClass:
		    bdata = (XButtonStatus *) anyclass;
		    printf ("num_buttons is %d\n",bdata->num_buttons);
		    for (j=0; j<bdata->num_buttons/8; j++)
			printf ("%x ", *(bdata->buttons+j));
		    printf ("\n");
		    anyclass = (XInputClass *) ++bdata;
		    break;
		case ValuatorClass:
		    vdata = (XValuatorStatus *) anyclass;
		    printf ("num_valuators is %d\n",vdata->num_valuators);
		    for (j=0; j<vdata->num_valuators; j++)
		        printf ("valuator %d has value %x\n",
				j, *(vdata->valuators+j));
		    anyclass = (XInputClass *) ++vdata;
		    break;
	        }
	    }
	}
	}
    else if (event->type == changedevicenotify)
	{
	c = (XChangeDeviceNotifyEvent * ) event;
	if (mode > NOPRINT)
	    printf ("Client: %d Device change event device=%d\n", 
		clientno, c->deviceid);
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
	return (0);
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
	process_device_events (display, &event, mode);
	count++;
	}
    if (Dflag)
	printf ("processed %d events\n",count);
    }

process_args (argc, argv)
    int argc;
    char *argv[];
    {
    int i;
    char *c;

    if (argc == 1)
	{
	printf ("Usage: -s [desired events]\n");
	printf ("           K  = DeviceKeyPress\n");
	printf ("           k  = DeviceKeyRelease\n");
	printf ("           B  = DeviceButtonPress\n");
	printf ("           b  = DeviceButtonRelease\n");
	printf ("           M  = DeviceMotionNotify\n");
	printf ("           P  = ProximityIn\n");
	printf ("           p  = ProximityOut\n");
	printf ("           F  = DeviceFocusChange\n");
	printf ("           S  = DeviceStateNotify\n");
	printf ("           m  = DeviceMappingNotify\n");
	printf ("           C  = ChangeDeviceNotify\n");
	printf ("           H  = DeviceMotionHint\n");
	printf ("           G  = auto-grab on DeviceButtonPress\n");
	printf ("           O  = DeviceOwnerGrabButton\n");
	printf ("           1  = DeviceButton1Motion\n");
	printf ("           2  = DeviceButton2Motion\n");
	printf ("           3  = DeviceButton3Motion\n");
	printf ("           4  = DeviceButton4Motion\n");
	printf ("           5  = DeviceButton5Motion\n");
	printf ("           6  = DeviceButtonMotion\n\n");
	printf ("Usage: -p [event fields print level]\n");
	printf ("           0  = nothing\n");
	printf ("           1  = 1 line per event (default)\n");
	printf ("           2  = all event fields\n\n");
	printf ("Usage: -D [turn debug info on.]\n\n");
	printf ("Press space bar on the X keyboard to terminate.\n");
	}
    while (--argc > 0)				/* while parameters are left  */
	{
	if ((++argv)[0][0] == '-')
	switch ((argv)[0][1])
	    {
	    case 's':
		printf ("setting Event selections.\n");
		for (i=0,c=(++argv)[0]; i<strlen(argv[0]); i++,c++)
		    switch (*c)
			{
			case 'K':		/* DeviceKeyPress 	*/
			    devicekeypressflag++;
			    break;
			case 'k':		/* DeviceKeyRelase	*/
			    devicekeyreleaseflag++;
			    break;
			case 'B':		/* DeviceButtonPress 	*/
			    devicebuttonpressflag++;
			    break;
			case 'b':		/* DeviceButtonRelease 	*/
			    devicebuttonreleaseflag++;
			    break;
			case 'M':		/* DeviceMotionNotify	*/
			    devicemotionnotifyflag++;
			    break;
			case 'P':		/* Proximity		*/
			    proximityinflag++;
			    break;
			case 'p':		/* Proximity		*/
			    proximityoutflag++;
			    break;
			case 'F':		/* DeviceFocusChange	*/
			    devicefocuschangeflag++;
			    break;
			case 'S':		/* DeviceStateNotify	*/
			    devicestatenotifyflag++;
			    break;
			case 'm':		/* DeviceMappingNotify	*/
			    devicemappingnotifyflag++;
			    break;
			case 'C':		/* ChangeDeviceNotify 	*/
			    changedevicenotifyflag++;
			    break;
			case 'H':		/* DeviceMotionHint	*/
			    devicemotionhintflag++;
			    break;
			case 'G':		/* DeviceButtonPressGrab*/
			    devicebuttonpressgrabflag++;
			    break;
			case 'O':		/* DeviceOwnerGrabButton*/
			    deviceownergrabbuttonflag++;
			    break;
			case '1':		/* DeviceButton1Motion  */
			    devicebutton1motionflag++;
			    break;
			case '2':		/* DeviceButton2Motion  */
			    devicebutton2motionflag++;
			    break;
			case '3':		/* DeviceButton3Motion  */
			    devicebutton3motionflag++;
			    break;
			case '4':		/* DeviceButton4Motion  */
			    devicebutton4motionflag++;
			    break;
			case '5':		/* DeviceButton5Motion  */
			    devicebutton5motionflag++;
			    break;
			case '6':		/* DeviceButtonMotion	*/
			    devicebuttonmotionflag++;
			    break;
			default:
			    break;
			}
		break;
	    case 'p':
		printf ("setting print level.\n");
		printmode = atoi((++argv)[0]);
		break;
	    case 'D':
		printf ("setting debug flag.\n");
		Dflag = 1;
		break;
	    default:
		printf ("Unknown argument.\n");
	    }
	}
    }
