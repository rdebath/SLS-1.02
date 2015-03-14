/* $XConsortium: XTouch.c,v 1.3 91/07/17 16:17:34 rws Exp $ */
/************************************************************************
 *
 * XTouch.c
 * This test attempts to invoke all of the functions provided by the
 * input extension.  This is a "touch test", i.e, no attempt is made to
 * exercise all permutations of parameters, and the result of the invocation
 * may not be verified.  This test just invokes each function with one set
 * of valid parameters.
 *
 */

#include <X11/Xlib.h>
#include <X11/extensions/XInput.h>
#include <X11/Xutil.h>
#include "stdio.h"

#define PRINT					0
#define NOPRINT					1

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

XDeviceInfoPtr	savlist[256];
XDevice		*sdev[256];
XDevice		*keydevice;

Window		root;

main(argc,argv)
    int argc;
    char *argv[];
    {
    int 		i,j;
    int 		ndevices;
    char		*name;
    Display		*display;
    Window		my, my2, my3;
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
    query_extension_version (display);
    create_two_windows (display, root, &my, &my2, &my3);
    slist = list_input_devices (display, &ndevices);
    list = slist;
    open_all_devices (display, list, ndevices);

    for (i=0; i<ndevices; i++, list++)
	if (list->use != IsXPointer && list->use != IsXKeyboard)
	    {
	    select_all_input (display, root, list->name, sdev[i]);
	    select_all_input (display, my2, list->name, sdev[i]);
	    for (ip= sdev[i]->classes, j=0; j<sdev[i]->num_classes; j++, ip++)
	        {
		if (ip->input_class == KeyClass)
		    {
		    do_key_mapping (display, list->name, sdev[i], savlist[i]);
		    do_modifier_mapping (display, list->name, sdev[i]);
	    	    grab_device_key (display, list->name, sdev[i], my, my2);
	            ungrab_device_key (display, list->name, sdev[i], my, my2);
		    }
		else if (ip->input_class == ButtonClass)
		    {
		    do_button_mapping (display, sdev[i], savlist[i]);
	    	    grab_device_button (display, list->name, sdev[i], my, my2);
	            ungrab_device_button (display, list->name, sdev[i], my, 
			my2);
		    }
		else if (ip->input_class == ValuatorClass)
		    {
		    get_motion_history (display, list->name, sdev[i]);
		    set_device_mode (display, name, sdev[i]);
		    }
		else if (ip->input_class == FeedbackClass)
		    {
		    do_feedback_control (display, name, sdev[i]);
		    }
		}
	    do_propagate_list (display, my2, list->name, sdev[i]);
	    focus_input_device (display, list->name, sdev[i], my3);
	    grab_input_device (display, list->name, sdev[i], my, my2, my3);
	    allow_device_event (display, list->name, sdev[i]);
            ungrab_input_device (display, list->name, sdev[i]);
	    query_device_state (display, list->name, sdev[i]);
	    send_extension_event (display, name, sdev[i], my2);
	    }

    close_input_devices (display, slist, sdev, ndevices);
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
 * This function creates two test windows.
 *
 */

create_two_windows (display, root, my, my2, my3)
    Display *display;
    Window root, *my, *my2, *my3;
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

    attribute_mask = CWBackPixmap;
    attribute_mask = CWBackPixel; 
    attribute_mask |= CWEventMask; 
    attributes.background_pixmap = None;
    attributes.background_pixel = BlackPixel(display, 0);
    attributes.event_mask = ExposureMask;

    *my2 = XCreateWindow (display, *my, 50,50, 300,100,1,
	DefaultDepthOfScreen (screen),
	InputOutput, CopyFromParent, attribute_mask, &attributes);
    if (my2 == 0) {
	fprintf (stderr, "can't create window!\n");
	exit (1);
    }
    status = XGetNormalHints (display, *my2, &hints);
    hints.flags |= (PPosition | PSize | PMinSize);
    hints.x = 50;
    hints.y = 50;
    hints.width = 300;
    hints.height = 100;
    hints.min_width = 300;
    hints.min_height = 100;
    XSetNormalHints (display, *my2, &hints);
    XMapWindow (display, *my2);

    attribute_mask = CWBackPixmap;
    attribute_mask = CWBackPixel; 
    attribute_mask |= CWEventMask; 
    attributes.background_pixmap = None;
    attributes.background_pixel = BlackPixel(display, 0);
    attributes.event_mask = ExposureMask;

    *my3 = XCreateWindow (display, *my2, 50,50, 200,50,1,
	DefaultDepthOfScreen (screen),
	InputOutput, CopyFromParent, attribute_mask, &attributes);
    if (my3 == 0) {
	fprintf (stderr, "can't create window!\n");
	exit (1);
    }
    status = XGetNormalHints (display, *my3, &hints);
    hints.flags |= (USPosition | USSize | PPosition | PSize);
    XSetNormalHints (display, *my3, &hints);
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

open_all_devices (display, list, ndevices)
    Display		*display;
    XDeviceInfoPtr 	list;
    int			ndevices;
    {
    int			valid;
    int			i,j;
    XDevice		*dev;
    XDevice		*XOpenDevice();
    XInputClassInfo	*ip;

    for (i=0; i<ndevices; i++, list++)
	if (list->use != IsXPointer &&
	    list->use != IsXKeyboard)
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
		    keydevice = dev;
		    valid = class[dev->device_id].valid++;
	    	    DeviceKeyPress (dev, devicekeypress, 
			class[dev->device_id].class[valid]);
		    valid = class[dev->device_id].valid++;
	    	    DeviceKeyRelease (dev, devicekeyrelease, 
			class[dev->device_id].class[valid]);
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
		    valid = class[dev->device_id].valid++;
	    	    DeviceButtonPress (dev, devicebuttonpress, 
			class[dev->device_id].class[valid]);
		    valid = class[dev->device_id].valid++;
	    	    DeviceButtonRelease (dev, devicebuttonrelease, 
			class[dev->device_id].class[valid]);
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
 * This function manipulates the button mapping of an extension
 * device that supports input class buttons.
 *
 */

do_button_mapping (display, dev, list)
    Display		*display;
    XDevice		*dev;
    XDeviceInfoPtr	*list;
    {
    int			status = 0;
    int			i;
    int 		len;
    unsigned char	map[256];
    unsigned char	map2[256];
    unsigned char	smap[256];
    XButtonInfoPtr 	b;
    XEvent 		event;
    XDeviceMappingEvent *m = (XDeviceMappingEvent *) &event;;


    for (i=0; i<256; i++)
	map2[i] = 255-i;
    b = (XButtonInfoPtr) FindInputClass (list, ButtonClass);
    if (b == NULL)
	return;

    len = XGetDeviceButtonMapping (display, dev, smap, b->num_buttons);
    if (Dflag)
	{
	printf ("\n");
	for (i=0; i<len; i++)
	    printf ("len is %d map[%d] is %d\n", len, i, smap[i]);
	printf ("\n");
	}
    status = XSetDeviceButtonMapping (display, dev, map2, len);
    XSync (display, 0);
    if (status != MappingBusy)
	{
	status = -1;
	while (XPending (display) > 0)
	    {
	    XNextEvent (display, &event);
	    process_device_events (m, NOPRINT);
	    if (m->type == devicemappingnotify &&
	        m->request == MappingPointer)
		    status = 0;
	    }
	}
    XGetDeviceButtonMapping (display, dev, map, len);
    for (i=0; i<len; i++)
	if (map[i] != map2[i])
	    status = -1;
    if (Dflag)
	{
	printf ("\n");
	for (i=0; i<len; i++)
	    printf ("len is %d map[%d] is %d\n", len, i, map[i]);
    	printf ("\n");
	}
    status = XSetDeviceButtonMapping (display, dev, smap, len);
    XSync (display, 0);
    if (status != MappingBusy)
	{
	status = -1;
	while (XPending (display) > 0)
	    {
	    XNextEvent (display,&event);
	    process_device_events (m, NOPRINT);
	    if (m->type == devicemappingnotify &&
	        m->request == MappingPointer)
		    status = 0;
	    }
	}
    if (status == 0)
	printf ("Test of XGet/ SetDeviceButtonMapping passed.\n");
    else
	printf ("Test of XGet/ SetDeviceButtonMapping failed.\n");
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
 * This function manipulates the do_not_propagate list for a window.
 *
 */

do_propagate_list (display, win, name, dev)
    Display	*display;
    Window	win;
    char	*name;
    XDevice	*dev;
    {
    int		i;
    int		status = 0;
    int		count, scount;
    XEventClass	tlist[2];
    XEventClass	*list, *slist;

    slist = (XEventClass *) 
	XGetDeviceDontPropagateList (display, win, &count);
    scount = count;

    XChangeDeviceDontPropagateList (display, win, count, slist, 
	DeleteFromList);

    list = (XEventClass *) 
	XGetDeviceDontPropagateList (display, win, &count);

    if (count > 0)
	status = -1;

    XChangeDeviceDontPropagateList (display, win, scount, slist, 
	AddToList);

    list = (XEventClass *) 
	XGetDeviceDontPropagateList (display, win, &count);

    if (count != scount)
	status = -1;

    for (i=0; i<scount; i++)
	if (*list != *slist)
	    status = -1;

    if (status == 0)
	printf ("Test of XGet/ XChangeDeviceDontPropagateList passed.\n");
    else
	printf ("Test of XGet/ XChangeDeviceDontPropagateList failed.\n");
    }

/******************************************************************
 *
 * This function gets and sets the focus for extension input devices.
 *
 * Valid cases:
 *    1). focus = None
 *    2). focus = PointerRoot
 *    3). focus = FollowKeyboard
 *    4). focus = window id
 *    5). time invalid
 *    6). while grabbed & not grabbed.
 */

focus_input_device (display, name, dev, win)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win;
    {
    int			status = -1;
    Window		sfocus;
    Window		focus;
    int			revert_to;
    int			focus_time;
    Window		root = RootWindow (display, 0);
    XEvent		event;
    XDeviceFocusChangeEvent	*f = (XDeviceFocusChangeEvent *) &event;

    XGetDeviceFocus (display, dev, &sfocus, &revert_to, &focus_time);
    if (Dflag)
	printf ("focus for %s is %x, revert_to is %x time is %x\n",
	    name, sfocus, revert_to, focus_time);

    XSetDeviceFocus (display, dev, None, RevertToNone, CurrentTime);
    XSync (display, 0);
    while (XPending (display) > 0)
	{
	XNextEvent (display,&event);
	process_device_events (&event, NOPRINT);
	if ((f->type == devicefocusin || f->type == devicefocusout) &&
	    f->deviceid == dev->device_id)
		status = 0;
	}
    XGetDeviceFocus (display, dev, &focus, &revert_to, &focus_time);
    if (focus != None || revert_to != RevertToNone)
	status = -1;

    XSetDeviceFocus (display, dev, root, RevertToParent, 
	CurrentTime);
    XSync (display, 0);
    while (XPending (display) > 0)
	{
	XNextEvent (display,&event);
	process_device_events (&event, NOPRINT);
	if ((f->type == devicefocusin || f->type == devicefocusout) &&
	    f->deviceid == dev->device_id)
		status = 0;
	}
    XGetDeviceFocus (display, dev, &focus, &revert_to, &focus_time);
    if (focus != root || revert_to != RevertToParent)
	status = -1;

    XSetDeviceFocus (display, dev, PointerRoot, RevertToPointerRoot, 
	CurrentTime);
    XSync (display, 0);
    while (XPending (display) > 0)
	{
	XNextEvent (display,&event);
	process_device_events (&event, NOPRINT);
	if ((f->type == devicefocusin || f->type == devicefocusout) &&
	    f->deviceid == dev->device_id)
		status = 0;
	}
    XGetDeviceFocus (display, dev, &focus, &revert_to, &focus_time);
    if (focus != PointerRoot || revert_to != RevertToPointerRoot)
	status = -1;

    XSetDeviceFocus (display, dev, FollowKeyboard, RevertToFollowKeyboard, 
	CurrentTime);
    XSync (display, 0);
    while (XPending (display) > 0)
	{
	XNextEvent (display,&event);
	process_device_events (&event, NOPRINT);
	if ((f->type == devicefocusin || f->type == devicefocusout) &&
	    f->deviceid == dev->device_id)
		status = 0;
	}
    XGetDeviceFocus (display, dev, &focus, &revert_to, &focus_time);
    if (focus != FollowKeyboard || revert_to != RevertToFollowKeyboard)
	status = -1;

    XSetDeviceFocus (display, dev, FollowKeyboard, RevertToPointerRoot, 
	CurrentTime);
    XSync (display, 0);
    while (XPending (display) > 0)
	{
	XNextEvent (display,&event);
	process_device_events (&event, NOPRINT);
	if ((f->type == devicefocusin || f->type == devicefocusout) &&
	    f->deviceid == dev->device_id)
		status = 0;
	}
    XGetDeviceFocus (display, dev, &focus, &revert_to, &focus_time);
    if (focus != FollowKeyboard || revert_to != RevertToPointerRoot)
	status = -1;
    if (Dflag)
	printf ("focus for %s is %x, revert_to is %x time is %x\n",
	    name, focus, revert_to, focus_time);

    if (status == 0)
	printf ("Test of XGet/ XSetDeviceFocus passed.\n");
    else
	printf ("Test of XGet/ XSetDeviceFocus failed.\n");
    }

/******************************************************************
 *
 * This function grabs extension input devices.
 *
 * Valid cases:
 *    1). AlreadyGrabbed.
 *    2). GrabInvalidTime.
 *    3). GrabFrozen.
 *    4). GrabNotViewable.
 *    5). focus state was FollowKeyboard.
 *
 */

grab_input_device (display, name, dev, win1, win2, win3)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win1, win2, win3;
    {
    int		i;
    char	*p1, *p2;
    XEvent 	event;
    XDeviceKeyEvent 	*k = (XDeviceKeyEvent *) &event;
    int		status = -1;
    int		tstatus = -1;
    int		ret = -1;
    XEvent	saveevent;
    XDeviceFocusChangeEvent *f = (XDeviceFocusChangeEvent *) &event;
    Time	time;

    tstatus = XGrabDevice (display, dev, win1, True,
	class[dev->device_id].valid, &class[dev->device_id].class[0], 
	GrabModeAsync, GrabModeAsync, CurrentTime);
    XSync (display,0);
    if (tstatus == Success)
	{
	status = -1;
	while (XPending (display) > 0)
	    {
	    XNextEvent (display,&event);
	    if (f->type == devicefocusin)
		{
		time = f->time;
		status = 0;
		}
	    }
	}
    else
	printf ("GrabDevice failed.\n");

    tstatus = XGrabDevice (display, dev, win1, True,
	class[dev->device_id].valid, &class[dev->device_id].class[0], 
	GrabModeAsync, GrabModeAsync, time - 1);
    XSync (display,0);
    if (tstatus != GrabInvalidTime)
	{
	printf ("GrabDevice did not return Invalidtime as it should have.\n");
	status = tstatus;
	}

    tstatus = XGrabDevice (display, dev, win3, True,
	class[dev->device_id].valid, 
	&class[dev->device_id].class[0], 
	GrabModeAsync, GrabModeAsync, CurrentTime);
    if (tstatus != GrabNotViewable)
	{
	printf ("GrabDevice didn't return GrabNotViewable as it should.\n");
	printf ("Status = %d\n",tstatus);
	status = tstatus;
	}

    status = XGrabDevice (display, dev, win1, True,
	class[dev->device_id].valid, 
	&class[dev->device_id].class[0], 
	GrabModeAsync, GrabModeAsync, CurrentTime);

    status = XGrabDevice (display, dev, win1, True,
	class[dev->device_id].valid, 
	&class[dev->device_id].class[0], 
	GrabModeSync, GrabModeAsync, CurrentTime);

    status = XGrabDevice (display, dev, win1, True,
	class[dev->device_id].valid, 
	&class[dev->device_id].class[0], 
	GrabModeAsync, GrabModeSync, CurrentTime);

    status = XGrabDevice (display, dev, win1, True,
	class[dev->device_id].valid, 
	&class[dev->device_id].class[0], 
	GrabModeSync, GrabModeSync, CurrentTime);

    if (Dflag)
	printf ("grab status is %x.\n", status);

    if (status == GrabSuccess)
	printf ("Test of XGrabDevice passed.\n");
    else
	printf ("Test of XGrabDevice failed.\n");
    }

/******************************************************************
 *
 * This function grabs keys on extension input devices.
 *
 */

grab_device_key (display, name, dev, win1, win2)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win1, win2;
    {
    int		status = -1;

    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, dev, win1, True, 
	class[dev->device_id].valid,
	&class[dev->device_id].class[0], GrabModeAsync, GrabModeAsync);
    }

/******************************************************************
 *
 * This function ungrabs keys on extension input devices.
 *
 */

ungrab_device_key (display, name, dev, win1, win2)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win1, win2;
    {

    XUngrabDeviceKey (display, dev, AnyKey, AnyModifier, dev, win1);
    }

/******************************************************************
 *
 * This function grabs buttons on extension input devices.
 *
 */

grab_device_button (display, name, dev, win1, win2)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win1, win2;
    {
    int		status = -1;

    XGrabDeviceButton (display, dev, AnyButton, AnyModifier, keydevice, win1, 
	True, 
	class[dev->device_id].valid, 
	&class[dev->device_id].class[0], GrabModeAsync, GrabModeAsync);
    }

/******************************************************************
 *
 * This function ungrabs buttons on extension input devices.
 *
 */

ungrab_device_button (display, name, dev, win1, win2)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win1, win2;
    {

    XUngrabDeviceButton (display, dev, AnyButton, AnyModifier, keydevice, win1);
    }

/******************************************************************
 *
 * This function ungrabs extension input devices.
 *
 */

ungrab_input_device (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int		ret;

    XUngrabDevice (display, dev, CurrentTime);
    }

/******************************************************************
 *
 * This function exercises the device key mapping functions.
 *
 */

do_key_mapping (display, name, dev, list)
    Display		*display;
    char		*name;
    XDevice		*dev;
    XDeviceInfoPtr	list;
    {
    int		status = 0;
    int		l=0;
    int		i, j, iret;
    int		ksyms_per_code = 2;
    XKeyInfoPtr k;
    KeySym	save[1024];
    KeySym	*ret = NULL;
    KeySym	*sret = NULL;
    XEvent	event;
    XDeviceMappingEvent	*m = (XDeviceMappingEvent *) &event;

    k = (XKeyInfoPtr) FindInputClass (list, KeyClass);
    if (k == NULL)
	return;

    ret = XGetDeviceKeyMapping (display, dev, k->min_keycode, 
	k->num_keys, &ksyms_per_code);

    sret = ret;

    if (Dflag)
	{
        printf ("keysyms per keycode for device %s is %d\n",
	    name, ksyms_per_code);
        printf ("before values for device %s:\n", name);
        printf ("    syms_per_code is: %d\n",ksyms_per_code);
        for (i=0; i<k->num_keys; i++)
	    {
	    printf ("syms for code %d: ",k->min_keycode+i);
	    for (j=0; j<ksyms_per_code; j++,ret++)
	       printf ("%x ",(*ret)++);
	    printf ("\n");
	    }
	}

    ret = sret;
    for (i=0; i<k->num_keys; i++)
	for (j=0; j<ksyms_per_code; j++,ret++)
	    {
	    save[l++] = *ret;
	    *ret = (KeySym) i*10+j;
	    }

    iret = XChangeDeviceKeyMapping (display, dev, k->min_keycode, 
	ksyms_per_code, sret, k->num_keys);
    XSync (display, 0);
    status = -1;
    while (XPending (display) > 0)
	{
	XNextEvent (display,&event);
	process_device_events (m, NOPRINT);
	if (m->type == devicemappingnotify &&
	    m->request == MappingKeyboard &&
	    m->first_keycode == k->min_keycode)
		status = 0;
	}
    ret = XGetDeviceKeyMapping (display, dev, k->min_keycode, 
	k->num_keys, &ksyms_per_code);
    sret = ret;

    for (i=0; i<k->num_keys; i++)
        for (j=0; j<ksyms_per_code; j++)
	    {
	    if (*ret != (KeySym) i*10+j)
		status = -1;
	    ret++;
	    }

    ret = sret;

    if (Dflag)
	{
        printf ("after values for device %s:\n", name);
        for (i=0; i<k->num_keys; i++)
	    {
	    printf ("syms for code %d: ",k->min_keycode+i);
            for (j=0; j<ksyms_per_code; j++)
	       printf ("%x ",(*ret)++);
            printf ("\n");
	    }
        printf ("\n");
	}

    iret = XChangeDeviceKeyMapping (display, dev, k->min_keycode, 
	ksyms_per_code, save, k->num_keys);
    process_events (display,NOPRINT);
    if (status == 0)
	printf ("Test of XGet/ XChangeDeviceKeyMapping passed.\n");
    else
	printf ("Test of XGet/ XChangeDeviceKeyMapping failed.\n");
    }

/******************************************************************
 *
 * This function exercises the device modifier mapping functions.
 *
 */

do_modifier_mapping (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int			tstatus = 0;
    int			status = 0;
    int			i,j;
    int			k=0;
    int			iret;
    XModifierKeymap	*ret = NULL;
    KeyCode		*first;
    KeyCode		savcodes[256];
    XEvent 		event;
    XDeviceMappingEvent *m = (XDeviceMappingEvent *) &event;

    ret = XGetDeviceModifierMapping (display, dev);
    if (Dflag)
	{
	printf ("before values for device %s:\n", name);
	printf ("    max_keypermod is: %d\n",ret->max_keypermod);
	printf ("codes for modifiers\n");
	first = ret->modifiermap;
	for (i=0; i<ret->max_keypermod; i++)
	    {
	    for (j=0; j<8; j++)
	        printf ("%x ",(*first)++);
	    printf ("\n");
	    }
	}

    first = ret->modifiermap;
    for (i=0; i<ret->max_keypermod; i++)
	for (j=0; j<8; j++,first++)
	    {
	    savcodes[k++] = *first;
	    *first = (KeyCode) (i+1)*10+j;
	    }

    tstatus = XSetDeviceModifierMapping (display, dev, ret);
    XSync (display, 0);
    if (tstatus == MappingSuccess)
	{
	status = -1;
	while (XPending (display) > 0)
	    {
	    XNextEvent (display,&event);
	    process_device_events (m, NOPRINT);
	    if (m->type == devicemappingnotify &&
	        m->request == MappingModifier)
		    status = 0;
	    }
	}
    ret = XGetDeviceModifierMapping (display, dev);

    if (Dflag)
	{
	printf ("after values for device %s:\n", name);
	printf ("    max_keypermod is: %d\n",ret->max_keypermod);
	printf ("codes for modifiers\n");
	}
    first = ret->modifiermap;

    for (i=0; i<ret->max_keypermod; i++)
	for (j=0; j<8; j++,first++)
	    if (*first != (KeyCode) (i+1)*10+j)
		status = -1;

    if (Dflag)
	{
        first = ret->modifiermap;
	for (i=0; i<ret->max_keypermod; i++)
	    {
	    for (j=0; j<8; j++)
		printf ("%x ",(*first)++);
	    printf ("\n");
	    }
	printf ("\n");
	}

    ret->modifiermap = savcodes;
    tstatus = XSetDeviceModifierMapping (display, dev, ret);
    if (tstatus == MappingSuccess)
	process_events (display, NOPRINT);

    if (status == 0)
	printf ("Test of XGet/ SetDeviceModifierMap passed.\n");
    else
	printf ("Test of XGet/ SetDeviceModifierMap failed.\n");
    }

/******************************************************************
 *
 * This function gets the motion history for a device that supports
 * input class Valuators.
 *
 */

get_motion_history (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    Time		start = CurrentTime;
    Time		stop = CurrentTime;
    int			nevents;
    int			mode;
    int			axis_count;
    XDeviceTimeCoord	*events;

    events = XGetDeviceMotionEvents (display, dev, start, stop, &nevents,
	&mode, &axis_count);
    if (Dflag)
	printf ("nevents is %d mode is %d axis_count is %d\n",
	   nevents, mode, axis_count);
    XFreeDeviceMotionEvents (events);
    }

/******************************************************************
 *
 * This function queries the extension version.
 *
 */

query_extension_version (display)
    {
    int			status = 0;
    XExtensionVersion 	*ext;

    ext = XGetExtensionVersion(display, "XInputExtension");

    if (ext->present && status == 0)
	printf ("Test of XGetExtensionVersion passed.\n");
    else
	printf ("Test of XGetExtensionVersion failed.\n");

    if (Dflag)
	{
        printf ("present is %d\n", ext->present);
        printf ("major version is %d\n", ext->major_version);
        printf ("minor version is %d\n", ext->minor_version);
	}
    }

/******************************************************************
 *
 * This function queries the state of a device.
 *
 */

query_device_state (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int			i;
    XDeviceState 	*state;
    XInputClass		*class;

    state = XQueryDeviceState (display, dev);
    if (Dflag)
	{
	printf ("state information: device id is %d\n",state->device_id);
	printf ("     num_class is %d\n",state->num_classes);
	}
    class = state->data;
    if (Dflag)
	for (i=0; i<state->num_classes; i++)
	    {
	    printf ("class id is %d\n",class->class);
	    printf ("length is %d\n",class->length);
	    class = (XInputClass *) ((char *) class + class->length);
	    }
    XFreeDeviceState (state);
    if (state != NULL)
	printf ("Test of XQueryDeviceState passed.\n");
    else
	printf ("Test of XQueryDeviceState failed.\n");
    }

/******************************************************************
 *
 * This function allows a frozen device to be thawed.
 *
 */

allow_device_event (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    XAllowDeviceEvents (display, dev, SyncAll, CurrentTime);
    XAllowDeviceEvents (display, dev, SyncThisDevice, CurrentTime);
    XAllowDeviceEvents (display, dev, ReplayThisDevice, CurrentTime);
    XAllowDeviceEvents (display, dev, AsyncThisDevice, CurrentTime);
    XAllowDeviceEvents (display, dev, AsyncOtherDevices, CurrentTime);
    XAllowDeviceEvents (display, dev, AsyncAll, CurrentTime);
    }

/******************************************************************
 *
 * This function gets the feedback control values of a device.
 *
 */

do_feedback_control (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int			i,j;
    int			num_feedbacks;
    int			mask=0;
    XFeedbackState	*state, *sav;
    XKbdFeedbackState	*kbd;
    XPtrFeedbackState	*ptr;
    XKbdFeedbackControl	kbdf;
    XPtrFeedbackControl	ptrf;

    state = XGetFeedbackControl (display, dev, &num_feedbacks);
    sav = state;
    if (Dflag)
	printf ("num_feedbacks is %d\n",num_feedbacks);

    for (i=0; i<num_feedbacks; i++)
	{
	if (Dflag)
	    {
	    printf ("feedback class is %d\n",state->class);
	    printf ("feedback length is %d\n",state->length);
	    }
	if (state->class == KbdFeedbackClass)
	    {
	    kbd = (XKbdFeedbackState *) state;
	    if (Dflag)
		{
	        printf ("bell_pitch is %d\n",kbd->pitch);
	        printf ("bell_percent is %d\n",kbd->percent);
	        printf ("bell_duration is %d\n",kbd->duration);
	        printf ("led_mask is %d\n",kbd->led_mask);
	        printf ("global_auto_repeat is %d\n",kbd->global_auto_repeat);
	        printf ("key_click_percent is %d\n",kbd->click);
	        printf ("auto_repeats are:\n");
	        for (j=0; j<32; j++)
	            printf ("%x ",kbd->auto_repeats[j]);
	        printf ("\n");
		}
	    state = (XFeedbackState *) ((char *) state + state->length);
	    }
	else if (state->class == PtrFeedbackClass)
	    {
	    ptr = (XPtrFeedbackState *) state;
	    if (Dflag)
		{
	        printf ("accelNumerator is %d\n",ptr->accelNum);
	        printf ("accelDenominator is %d\n",ptr->accelDenom);
	        printf ("threshold is %d\n",ptr->threshold);
		}
	    state = (XFeedbackState *) ((char *) state + state->length);
	    }
	else 
	    printf ("bogus state\n");
	}

    for (i=0; i<num_feedbacks; i++)
	{
	state = sav;
	if (state->class == KbdFeedbackClass)
	    {
	    mask = DvPercent | DvPitch | DvDuration;
	    mask |= DvLed | DvKeyClickPercent | DvKey | DvAutoRepeatMode;
	    kbdf.class = KbdFeedbackClass;
	    kbdf.length = sizeof (XKbdFeedbackControl);
	    kbdf.pitch = 33;
	    kbdf.percent = 55;
	    kbdf.duration = 44;
	    kbdf.click = 99;
	    kbdf.led_mask = 0xf0f0; 
	    kbdf.led_value = 0xcf3f; 
	    kbdf.key = 0x81; 
	    kbdf.auto_repeat_mode = AutoRepeatModeOff; 
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    }
	else if (state->class == PtrFeedbackClass)
	    {
	    mask = DvAccelNum | DvAccelDenom | DvThreshold;
	    ptrf.class = PtrFeedbackClass;
	    ptrf.length = sizeof (XPtrFeedbackControl);
	    ptrf.accelNum = 10;
	    ptrf.accelDenom = 2;
	    ptrf.threshold = 6;
	    XChangeFeedbackControl (display, dev, mask, &ptrf);
	    }
	}
    XFreeFeedbackList (state);
    }

/******************************************************************
 *
 * This function changes the mode of a device.
 *
 */

set_device_mode (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int	status;

    status = XSetDeviceMode (display, dev, Absolute);
    status = XSetDeviceMode (display, dev, Relative);
    }

/******************************************************************
 *
 * This function sends extension events to this client.
 *
 */

send_extension_event (display, name, dev, win)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win;
    {
    int			i;
    int			expect = sizeof (XDeviceKeyEvent);
    char		*p1, *p2;
    int			status;
    Bool		propagate = True;
    XEvent		send, receive;
    XDeviceKeyEvent	*sendp = (XDeviceKeyEvent *) &send;
    XDeviceKeyEvent	*receivep = (XDeviceKeyEvent *) &receive;

    for (i=0,p1=(char *) sendp; i<sizeof (XEvent); i++)
	*p1++ = 0;
    sendp->type = devicekeypress;
    sendp->serial =  1;
    sendp->send_event = 0;
    sendp->display = display;
    sendp->root = root;
    sendp->x = 10;
    sendp->y = 10;
    sendp->same_screen = 1;
    sendp->subwindow = win;
    sendp->deviceid = dev->device_id;
    sendp->state = 0;
    sendp->keycode = 0xd;
    sendp->axes_count = 0;
    sendp->window = win;
    sendp->x_root = 160;
    sendp->y_root = 160;
    sendp->time = 0x50505050;
    sendp->device_state = 1;
    sendp->axes_count = 6;
    sendp->first_axis = 0;
    for (i=0; i<6; i++)
        sendp->axis_data[i] = i;

    process_events (display, NOPRINT);
    status = XSendExtensionEvent (display, dev, win, propagate,
	class[dev->device_id].valid, &class[dev->device_id].class[0], sendp);
    XSync (display, 0);
    if (status == 0)
	{
	printf ("Test of XSendExtensionEvent failed. (Conversion failed.)\n");
	return;
	}

    while (XPending (display) > 0)
	{
	XNextEvent (display,&receive);
	p1 = (char *) sendp;
	p2 = (char *) receivep;
	for (i=0; i<expect; i++,p1++,p2++)
	    if (*p1 != *p2 && (i<4 || i>11))
		break;
	if (i == expect)
	    status = 0;
	else
	    status = -1;
	}
    if (status != 0)
	printf ("Test of XSendExtensionEvent failed.\n");
    else
	printf ("Test of XSendExtensionEvent passed.\n");
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

    if (event->type == devicekeypress)
	{
	k = (XDeviceKeyEvent * ) event;
	if (mode == PRINT)
	    {
	    printf ("Device key press event device=%d\n", k->deviceid);
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
    else if (event->type == devicekeyrelease)
	{
	k = (XDeviceKeyEvent * ) event;
	if (mode == PRINT)
	    {
	    printf ("Device key release event device=%d\n", k->deviceid);
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
	if (mode == PRINT)
	    {
	    b = (XDeviceButtonEvent * ) event;
	    printf ("Device button press event device=%d\n", b->deviceid);
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
    else if (event->type == devicebuttonrelease)
	 {
	if (mode == PRINT)
	    {
	    b = (XDeviceButtonEvent * ) event;
	    printf ("Device button release event device=%d\n", b->deviceid);
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
	if (mode == PRINT)
	    {
	    M = (XDeviceMotionEvent * ) event;
	    printf ("Device motion event device=%d\n", M->deviceid);
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
	    }
	}
    else if (event->type == devicefocusin || event->type == devicefocusout)
	{
	if (mode == PRINT)
	    {
	    f = (XDeviceFocusChangeEvent * ) event;
	    printf ("Device focus event device=%d\n", f->deviceid);
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
	if (mode == PRINT)
	    {
	    p = (XProximityNotifyEvent * ) event;
	    printf ("Device proximity event device=%d\n", p->deviceid);
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
	if (mode == PRINT)
	    {
	    m = (XDeviceMappingEvent * ) event;
	    printf ("Device mapping event device=%d\n", m->deviceid);
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
	if (mode == PRINT)
	{
	s = (XDeviceStateNotifyEvent * ) event;
	printf ("Device state notify event device=%d\n", s->deviceid);
	printf ("     type =          %d\n", s->type);
	printf ("     serial =        %ld\n", s->serial);
	printf ("     send_event =    %ld\n", s->send_event);
	printf ("     display =       %x\n", s->display);
	printf ("     window =        %x\n", s->window);
	printf ("     time =          %x\n", s->time);
	printf ("     num_classes =   %x\n", s->num_classes);
	printf ("     data =          %x\n", s->data);
	anyclass = (XInputClass *) (&s->data[0]);
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
	if (mode == PRINT)
	    {
	    c = (XChangeDeviceNotifyEvent * ) event;
	    printf ("Device change event device=%d\n", c->deviceid);
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
	return (-1);
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
    if (Dflag)
	printf ("processed %d events\n",count);
    }
