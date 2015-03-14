/* $XConsortium: XPErrors.c,v 1.2 91/02/20 09:17:37 rws Exp $ */
/************************************************************************
 *
 * XProtoErrors.c - attempt to force all of the protocol errors defined
 * 	by the input extension.
 *
 */

#include <X11/X.h>
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
#define XSendExtensionEvent_code 		31

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
"SetDeviceButtonMapping","QueryDeviceState","SendExtensionEvent"};

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
XDeviceInfoPtr	xkeyboard;
XDeviceInfoPtr	xpointer;
XDevice		*keydevice;
XDevice		*sdev[256];
XEventClass	bogusclass[] = {(unsigned long)~0L,(unsigned long)~0L,(unsigned long)~0L};

int	baddevice;
int	badclass;
int	badmode;
Window	root;

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

    printf ("This test attempts to force protocol errors defined by the input extension.\n");
    printf ("A status message is printed when the test finishes.\n");
    name = argv[0];
    display = XOpenDisplay ("");
    if (display == NULL)
	{
	printf ("No connection to server - aborting test.\n");
	exit(1);
	}
    root = RootWindow (display,0);
    init_error_handler (display);
    BadDevice(display,baddevice);
    BadClass(display,badclass);
    BadMode(display,badmode);
    query_extension_version (display);
    create_two_windows (display, root, &my, &my2, &my3);
    slist = list_input_devices (display, &ndevices);
    list = slist;
    open_all_devices (display, list, ndevices);

    for (i=0; i<ndevices; i++, list++)
	if (list->use != IsXKeyboard && list->use != IsXPointer)
	    {
	    select_all_input (display, root, list->name, sdev[i]);
	    select_all_input (display, my2, list->name, sdev[i]);
	    for (ip= sdev[i]->classes, j=0; j<sdev[i]->num_classes; j++, ip++)
	        {
		if (ip->input_class == KeyClass)
		    {
		    create_button_errors (display, sdev[i], savlist[i]);
		    create_valuator_errors (display, sdev[i], savlist[i]);
		    do_key_mapping (display, list->name, sdev[i], savlist[i]);
		    do_modifier_mapping (display, list->name, sdev[i]);
	    	    grab_device_key (display, list->name, sdev[i], my, my2);
		    change_keyboard_device (display, list->name, sdev[i], 
			savlist[i]);
		    }
		else if (ip->input_class == ButtonClass)
		    {
		    create_key_errors (display, sdev[i], savlist[i]);
		    do_button_mapping (display, sdev[i], savlist[i]);
	    	    grab_device_button (display, list->name, sdev[i], my, my2);
		    }
		else if (ip->input_class == ValuatorClass)
		    {
		    get_motion_history (display, list->name, sdev[i]);
		    change_pointer_device (display, list->name, sdev[i]);
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
	    query_device_state (display, list->name, sdev[i]);
	    send_extension_event (display, list->name, sdev[i],my2);
	    }

    for (i=0,list=slist; i<ndevices; i++, list++)
	if (list->use != IsXKeyboard && list->use != IsXPointer)
	{
	ungrab_input_device (display, list->name, sdev[i]);
	for (ip= sdev[i]->classes, j=0; j<sdev[i]->num_classes; 
		j++, ip++)
	    if (ip->input_class == KeyClass)
		ungrab_device_key (display, list->name, sdev[i], my, my2);
	    else if (ip->input_class == ButtonClass)
		ungrab_device_button (display, list->name, sdev[i], my, my2);
	}
    close_input_devices (display, sdev, ndevices);
    if (gstatus == 0)
	printf ("Test XProtoErrrors succeeeded.\n");
    else
	printf ("Test XProtoErrrors failed.\n");
    }

/******************************************************************
 *
 * This function closes all open input devices.
 * X Errors:
 *    1). BadDevice
 *
 */

close_input_devices (display, devices, count)
    Display	*display;
    XDevice	*devices[];
    int		count;
    {
    int		i;
    int		saveid;

    expect = baddevice;
    error_code = XCloseDevice_code;

    for (i=0; i<count; i++)
	{
	if (!devices[i])
	    continue;
	devices[i]->device_id = -1;
	XCloseDevice (display, devices[i]);
	XSync (display, 0);
	}
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
    hints.x = 50;
    hints.y = 50;
    hints.width = 300;
    hints.height = 100;
    hints.min_width = 300;
    hints.min_height = 100;
    hints.flags |= (PPosition | PSize | PMinSize);
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
	    printf ("use is %s\n",list->use);
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
 * X Errors:
 *    1). BadDevice
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

    expect = baddevice;
    error_code = XOpenDevice_code;

    XOpenDevice (display, -1);
    XOpenDevice (display, xpointer->id);
    XOpenDevice (display, xkeyboard->id);
    XSync (display, 0);

    for (i=0; i<ndevices; i++, list++)
	if (list->use == IsXKeyboard ||
	    list->use == IsXPointer)
	    continue;
	else
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
 * This function creates errors caused by invoking functions that 
 * require the input device to support class Buttons.
 *
 */

create_button_errors (display, dev, list)
    Display		*display;
    XDevice		*dev;
    XDeviceInfoPtr	*list;
    {
    unsigned char	smap[256];
    int 		len;
    XButtonInfoPtr 	b;
    Window		root = RootWindow (display,0);


    b = (XButtonInfoPtr) FindInputClass (list, ButtonClass);
    if (b != NULL)
	return;

    expect = BadMatch;
    error_code = XGetDeviceButtonMapping_code;
    len = XGetDeviceButtonMapping (display, dev, smap, 1);

    error_code = XSetDeviceButtonMapping_code;
    XSetDeviceButtonMapping (display, dev, smap, 1);
    XSync (display, 0);

    error_code = XUngrabDeviceButton_code;
    XUngrabDeviceButton (display, dev, AnyButton, AnyModifier, keydevice, root);
    XSync (display, 0);
    expect = Success;
    error_code = 0;
    }

/******************************************************************
 *
 * This function creates errors caused by invoking functions that 
 * require the input device to support class Valuators.
 *
 */

create_valuator_errors (display, dev, list)
    Display		*display;
    XDevice		*dev;
    XDeviceInfoPtr	*list;
    {
    Time		start = CurrentTime;
    Time		stop = CurrentTime;
    int			nevents;
    int			mode;
    int 		axis_count;
    XDeviceTimeCoord	*events;
    XValuatorInfoPtr 	v;
    Window		root = RootWindow (display,0);


    v = (XValuatorInfoPtr) FindInputClass (list, ValuatorClass);
    if (v != NULL)
	return;

    expect = BadMatch;
    error_code = XGetDeviceMotionEvents_code;
    events = XGetDeviceMotionEvents (display, dev, start, stop, &nevents,
	&mode, &axis_count);
    XSync (display, 0);

    error_code = XChangePointerDevice_code;
    XChangePointerDevice (display, dev, 0, 1);
    XSync (display, 0);

    expect = Success;
    error_code = 0;
    }

/******************************************************************
 *
 * This function creates errors caused by invoking functions that 
 * require the input device to support class Keys.
 *
 */

create_key_errors (display, dev, list)
    Display		*display;
    XDevice		*dev;
    XDeviceInfoPtr	*list;
    {
    int 		iret;
    int 		ksyms_per_code;
    XKeyInfoPtr 	k;
    KeySym		*sret = NULL;
    XModifierKeymap	*ret = NULL;
    unsigned long	kvaluemask;
    Window		root = RootWindow (display,0);


    k = (XKeyInfoPtr) FindInputClass (list, KeyClass);
    if (k != NULL)
	return;

    expect = BadMatch;
    error_code = XGetDeviceKeyMapping_code;
    sret = XGetDeviceKeyMapping (display, dev, 0, 1, &ksyms_per_code);
    XSync (display, 0);

    error_code = XChangeDeviceKeyMapping_code;
    iret = XChangeDeviceKeyMapping (display, dev, 0, 1, sret, 1); 
    XSync (display, 0);

    error_code = XGetDeviceModifierMapping_code;
    ret = XGetDeviceModifierMapping (display, dev);
    XSync (display, 0);

    error_code = XSetDeviceModifierMapping_code;
    iret = XSetDeviceModifierMapping (display, dev, ret);
    XSync (display, 0);

    error_code = XGrabDeviceKey_code;
    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, dev, root, True, 
	class[dev->device_id].valid, 
	&class[dev->device_id].class[0], GrabModeAsync, GrabModeAsync);

    error_code = XUngrabDeviceKey_code;
    XUngrabDeviceKey (display, dev, AnyButton, AnyModifier, dev, root);
    XSync (display, 0);

    expect = Success;
    error_code = 0;
    }

/******************************************************************
 *
 * This function manipulates the button mapping of an extension
 * device that supports input class buttons.
 * X Errors:
 *    XGetDeviceButtonMapping
 *        1). BadDevice
 *        2). BadMatch - see create_button_errors
 *    XSetDeviceButtonMapping
 *        1). BadDevice
 *        2). BadMatch - see create_button_errors
 *
 */

do_button_mapping (display, dev, list)
    Display		*display;
    XDevice		*dev;
    XDeviceInfoPtr	*list;
    {
    int			saveid;
    int			status = 0;
    int			i;
    int 		len;
    unsigned char	map[256];
    unsigned char	map2[256];
    unsigned char	smap[256];
    XButtonInfoPtr 	b;


    for (i=0; i<256; i++)
	map2[i] = 255-i;
    b = (XButtonInfoPtr) FindInputClass (list, ButtonClass);
    if (b == NULL)
	return;

    expect = baddevice;
    error_code = XGetDeviceButtonMapping_code;
    saveid = dev->device_id;
    dev->device_id = -1;

    len = XGetDeviceButtonMapping (display, dev, smap, b->num_buttons);
    dev->device_id = xpointer->id;
    len = XGetDeviceButtonMapping (display, dev, smap, b->num_buttons);
    dev->device_id = xkeyboard->id;
    len = XGetDeviceButtonMapping (display, dev, smap, b->num_buttons);
    XSync (display, 0);

    error_code = XSetDeviceButtonMapping_code;
    XSetDeviceButtonMapping (display, dev, smap, b->num_buttons);
    dev->device_id = xpointer->id;
    XSetDeviceButtonMapping (display, dev, smap, b->num_buttons);
    dev->device_id = xkeyboard->id;
    XSetDeviceButtonMapping (display, dev, smap, b->num_buttons);
    dev->device_id = saveid;
    XSync (display, 0);

    expect = BadValue;
    XSetDeviceButtonMapping (display, dev, smap, b->num_buttons+1);
    XSync (display, 0);
    }

/******************************************************************
 *
 * This function selects all available input from an extension device.
 *
 * X Errors:
 *     XSelectExtensionEvent
 *         1). BadWindow
 *         2). BadLength - require bogus library.
 *         3). BadClass
 *     XGetSelectedExtensionEvent
 *         1). BadWindow
 *
 */

select_all_input (display, win, name, dev)
    Display	*display;
    Window	win;
    char	*name;
    XDevice	*dev;
    {
    int			i, j;
    XEventClass		tlist[2];
    int			status = 0;
    int			this_client_count;
    int			all_clients_count;
    XEventClass		*this_client;
    XEventClass		*all_clients;

    tlist[0] = -1;

    expect = BadWindow;
    error_code = XSelectExtensionEvent_code;
    XSelectExtensionEvent (display, -1, 
	&(class[dev->device_id].class[0]), 
	class[dev->device_id].valid);
    XSync (display, 0);

    expect = badclass;
    XSelectExtensionEvent (display, win, tlist, 1);
    XSync (display, 0);

    expect = BadWindow;
    error_code = XGetSelectedExtensionEvents_code;
    XGetSelectedExtensionEvents (display, -1, &this_client_count,
	&this_client, &all_clients_count, &all_clients);
    XSync (display, 0);
    }

/******************************************************************
 *
 * This function manipulates the do_not_propagate list for a window.
 *
 * X Errors:
 *    XGetDeviceDontPropagateList
 *        1). BadWindow
 *    XChangeDeviceDontPropagateList
 *        1). BadWindow
 *        2). BadMode
 *        3). BadClass
 *        4). BadLength - requires bogus library.
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

    tlist[0] = -1;
    error_code = XGetDeviceDontPropagateList_code;
    expect = BadWindow;
    slist = (XEventClass *) 
	XGetDeviceDontPropagateList (display, -1, &count);

    error_code = XChangeDeviceDontPropagateList_code;
    XChangeDeviceDontPropagateList (display, -1, 0, NULL, 
	DeleteFromList);
    XSync (display, 0);

    expect = badmode;
    XChangeDeviceDontPropagateList (display, win, 0, NULL, 
	-1);
    XSync (display, 0);

    expect = badclass;
    XChangeDeviceDontPropagateList (display, win, 1, tlist, 
	AddToList);
    XSync (display, 0);

    }

/******************************************************************
 *
 * This function gets and sets the focus for extension input devices.
 *
 * X Errors:
 *    XGetDeviceFocus
 *        1). BadDevice
 *    XSetDeviceFocus
 *        1). BadDevice
 *        2). BadValue
 *        3). BadMatch
 *        4). BadMatch
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
    {
    int		saveid = dev->device_id;
    int			status = 0;
    Window		sfocus;
    Window		focus;
    int			revert_to;
    int			focus_time;
    Window		root = RootWindow (display, 0);

    expect = baddevice;
    error_code = XGetDeviceFocus_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XGetDeviceFocus (display, dev, &sfocus, &revert_to, &focus_time);
    dev->device_id = xpointer->id;
    XGetDeviceFocus (display, dev, &sfocus, &revert_to, &focus_time);
    dev->device_id = xkeyboard->id;
    XGetDeviceFocus (display, dev, &sfocus, &revert_to, &focus_time);
    XSync (display, 0);

    error_code = XSetDeviceFocus_code;
    expect = baddevice;
    dev->device_id = -1;
    XSetDeviceFocus (display, dev, None, RevertToNone, CurrentTime);
    dev->device_id = xpointer->id;
    XSetDeviceFocus (display, dev, None, RevertToNone, CurrentTime);
    dev->device_id = xkeyboard->id;
    XSetDeviceFocus (display, dev, None, RevertToNone, CurrentTime);
    XSync (display, 0);

    dev->device_id = saveid;
    expect = BadWindow;
    XSetDeviceFocus (display, dev, -1, RevertToNone, CurrentTime);
    XSync (display, 0);

    expect = BadValue;
    XSetDeviceFocus (display, dev, None, -1, CurrentTime);
    XSync (display, 0);

    expect = BadMatch;
    XSetDeviceFocus (display, dev, win, RevertToPointerRoot, CurrentTime);
    XSync (display, 0);

    }

/******************************************************************
 *
 * This function grabs extension input devices.
 *
 * X Errors:
 *    1). BadDevice
 *    2). BadWindow
 *    3). BadValue (each of the two modes)
 *    4). BadClass
 *    5). BadLength (requires bogus library).
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
    int		saveid = dev->device_id;
    int		status = -1;
    int		tstatus = -1;
    int		ret = -1;
    XEvent	saveevent;
    XDeviceFocusChangeEvent *f = (XDeviceFocusChangeEvent *) &event;
    Time	time;

    event.type = 0;
    expect = baddevice;
    error_code = XGrabDevice_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    status = XGrabDevice (display, dev, win1, True, 
	class[saveid].valid, 
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync, CurrentTime);
    dev->device_id = xpointer->id;
    status = XGrabDevice (display, dev, win1, True, 
	class[saveid].valid, 
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync, CurrentTime);
    dev->device_id = xkeyboard->id;
    status = XGrabDevice (display, dev, win1, True,
	class[saveid].valid, 
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync, CurrentTime);
    XSync (display, 0);

    dev->device_id = saveid;
    expect = BadWindow;
    status = XGrabDevice (display, dev, -1, True,
	class[saveid].valid, 
	&class[dev->device_id].class[0], 
	GrabModeAsync, GrabModeAsync, CurrentTime);
    XSync (display, 0);

    expect = BadValue;
    status = XGrabDevice (display, dev, win1, True,
	class[saveid].valid, 
	&class[dev->device_id].class[0], -1, GrabModeAsync, CurrentTime);
    status = XGrabDevice (display, dev, win1, True,
	class[saveid].valid, 
	&class[dev->device_id].class[0], GrabModeAsync, -1, CurrentTime);
    XSync (display, 0);

    expect = badclass;
    status = XGrabDevice (display, dev, win1, True, 2,
	bogusclass, GrabModeAsync, GrabModeAsync, CurrentTime);
    XSync (display, 0);

    tstatus = XGrabDevice (display, dev, win1, True, class[saveid].valid, 
	&class[dev->device_id].class[0], GrabModeAsync, GrabModeAsync, 
	CurrentTime);
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

    tstatus = XGrabDevice (display, dev, win1, True, class[saveid].valid, 
	&class[dev->device_id].class[0], GrabModeAsync, GrabModeAsync, 
	time - 1);
    if (tstatus != GrabInvalidTime)
	{
	printf ("GrabDevice did not return Invalidtime as it should have.\n");
	status = tstatus;
	}

    tstatus = XGrabDevice (display, dev, win3, True,
	class[saveid].valid, 
	&class[dev->device_id].class[0], 
	GrabModeAsync, GrabModeAsync, CurrentTime);
    if (tstatus != GrabNotViewable)
	{
	printf ("GrabDevice didn't return GrabNotViewable as it should.\n");
	status = tstatus;
	}

    }

/******************************************************************
 *
 * This function grabs keys on extension input devices.
 *
 * X Errors:
 *    1). BadDevice
 *    2). BadWindow
 *    3). BadValue (each of the two modes)
 *    4). BadClass
 *    5). BadLength (requires bogus library).
 *
 */

grab_device_key (display, name, dev, win1, win2)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win1, win2;
    {
    int		saveid = dev->device_id;
    int		status = -1;
    XDevice	dev2;

    dev2 = *dev;
    expect = baddevice;
    error_code = XGrabDeviceKey_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XGrabDeviceKey (display, &dev2, AnyKey, AnyModifier, dev, win1, True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = xpointer->id;
    XGrabDeviceKey (display, &dev2, AnyKey, AnyModifier, dev, win1, True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = xkeyboard->id;
    XGrabDeviceKey (display, &dev2, AnyKey, AnyModifier, dev, win1, True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = -1;
    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, &dev2, win1, True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = xpointer->id;
    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, &dev2, win1, True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = xkeyboard->id;
    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, &dev2, win1, True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    XSync (display, 0);

    dev->device_id = saveid;
    expect = BadWindow;
    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, dev, -1, True, 
	class[dev->device_id].valid,
	&class[dev->device_id].class[0], GrabModeAsync, GrabModeAsync);
    XSync (display, 0);

    expect = BadValue;
    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, dev, win1, True, 
	class[dev->device_id].valid,
	&class[dev->device_id].class[0], -1, GrabModeAsync);
    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, dev, win1, True, 
	class[dev->device_id].valid,
	&class[dev->device_id].class[0], GrabModeAsync, -1);
    XSync (display, 0);

    expect = badclass;
    XGrabDeviceKey (display, dev, AnyKey, AnyModifier, dev, win1, True, 
	2,
	bogusclass, GrabModeAsync, GrabModeAsync);
    XSync (display, 0);

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
    int		saveid;
    XDevice	dev2;

    dev2 = *dev;
    expect = baddevice;
    error_code = XUngrabDeviceKey_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XUngrabDeviceKey (display, dev, AnyButton, AnyModifier, &dev2, win1);
    dev->device_id = xpointer->id;
    XUngrabDeviceKey (display, dev, AnyButton, AnyModifier, &dev2, win1);
    dev->device_id = xkeyboard->id;
    XUngrabDeviceKey (display, dev, AnyButton, AnyModifier, &dev2, win1);
    dev->device_id = -1;
    XUngrabDeviceKey (display, &dev2, AnyButton, AnyModifier, dev, win1);
    dev->device_id = xpointer->id;
    XUngrabDeviceKey (display, &dev2, AnyButton, AnyModifier, dev, win1);
    dev->device_id = xkeyboard->id;
    XUngrabDeviceKey (display, &dev2, AnyButton, AnyModifier, dev, win1);
    XSync (display, 0);

    dev->device_id = saveid;
    expect = BadWindow;
    XUngrabDeviceKey (display, dev, AnyButton, AnyModifier, dev, -1);
    XSync (display, 0);

    }

/******************************************************************
 *
 * This function grabs buttons on extension input devices.
 *
 * X Errors:
 *    1). BadDevice
 *    2). BadWindow
 *    3). BadValue (each of the two modes)
 *    4). BadClass
 *    5). BadLength (requires bogus library).
 *
 */

grab_device_button (display, name, dev, win1, win2)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win1, win2;
    {
    int		saveid = dev->device_id;
    int		status = -1;
    XDevice	dev2;

    dev2 = *keydevice;
    expect = baddevice;
    error_code = XGrabDeviceButton_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XGrabDeviceButton (display, dev, AnyButton, AnyModifier, &dev2, win1, 
	True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = xpointer->id;
    XGrabDeviceButton (display, dev, AnyButton, AnyModifier, &dev2, win1, 
	True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = xkeyboard->id;
    XGrabDeviceButton (display, dev, AnyButton, AnyModifier, &dev2, win1, 
	True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = -1;
    XGrabDeviceButton (display, &dev2, AnyButton, AnyModifier, dev, win1, 
	True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = xpointer->id;
    XGrabDeviceButton (display, &dev2, AnyButton, AnyModifier, dev, win1, 
	True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    dev->device_id = xkeyboard->id;
    XGrabDeviceButton (display, &dev2, AnyButton, AnyModifier, dev, win1, 
	True, 
	class[saveid].valid,
	&class[saveid].class[0], GrabModeAsync, GrabModeAsync);
    XSync (display, 0);

    dev->device_id = saveid;
    expect = BadWindow;
    XGrabDeviceButton (display, dev, AnyButton, AnyModifier, keydevice, -1, 
	True, 
	class[saveid].valid,
	&class[dev->device_id].class[0], GrabModeAsync, GrabModeAsync);
    XSync (display, 0);

    expect = BadValue;
    XGrabDeviceButton (display, dev, AnyButton, AnyModifier, keydevice, win1, 
	True, 
	class[saveid].valid,
	&class[dev->device_id].class[0], -1, GrabModeAsync);
    XGrabDeviceButton (display, dev, AnyButton, AnyModifier, keydevice, win1, 
	True, 
	class[saveid].valid,
	&class[dev->device_id].class[0], GrabModeAsync, -1);
    XSync (display, 0);

    expect = badclass;
    XGrabDeviceButton (display, dev, AnyButton, AnyModifier, keydevice, win1, 
	True, 2,
	bogusclass, GrabModeAsync, GrabModeAsync);
    XSync (display, 0);

    }

/******************************************************************
 *
 * This function ungrabs buttons on extension input devices.
 * X Errors:
 *    1). BadDevice
 *    2). BadWindow
 *    3). BadLength - requires bogus library.
 *
 */

ungrab_device_button (display, name, dev, win1, win2)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win1, win2;
    {
    XDevice	dev2;
    int		saveid;

    dev2 = *dev;
    expect = baddevice;
    error_code = XUngrabDeviceButton_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XUngrabDeviceButton (display, dev, AnyButton, AnyModifier, &dev2, win1);
    dev->device_id = xpointer->id;
    XUngrabDeviceButton (display, dev, AnyButton, AnyModifier, &dev2, win1);
    dev->device_id = xkeyboard->id;
    XUngrabDeviceButton (display, dev, AnyButton, AnyModifier, &dev2, win1);
    dev->device_id = -1;
    XUngrabDeviceButton (display, &dev2, AnyButton, AnyModifier, dev, win1);
    dev->device_id = xpointer->id;
    XUngrabDeviceButton (display, &dev2, AnyButton, AnyModifier, dev, win1);
    dev->device_id = xkeyboard->id;
    XUngrabDeviceButton (display, &dev2, AnyButton, AnyModifier, dev, win1);
    XSync (display, 0);

    dev->device_id = saveid;
    expect = BadWindow;
    XUngrabDeviceButton (display, dev, AnyButton, AnyModifier, keydevice, -1);
    XSync (display, 0);
    }

/******************************************************************
 *
 * This function ungrabs extension input devices.
 * X Errors:
 *    1). BadDevice
 *
 */

ungrab_input_device (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int		ret;
    int		saveid;

    expect = baddevice;
    error_code = XUngrabDevice_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XUngrabDevice (display, dev, CurrentTime);
    dev->device_id = xpointer->id;
    XUngrabDevice (display, dev, CurrentTime);
    dev->device_id = xkeyboard->id;
    XUngrabDevice (display, dev, CurrentTime);
    XSync (display, 0);
    dev->device_id = saveid;

    }

/******************************************************************
 *
 * This function exercises the device key mapping functions.
 * X Errors:
 *    XGetDeviceKeyMapping
 *        1). BadDevice
 *        2). BadValue
 *        3). BadMatch - see create_key_errors
 *    XChangeDeviceKeyMapping
 *        1). BadDevice
 *        2). BadValue
 *        3). BadMatch - see create_key_errors
 *
 */

do_key_mapping (display, name, dev, list)
    Display		*display;
    char		*name;
    XDevice		*dev;
    XDeviceInfoPtr	list;
    {
    int		saveid;
    int		status = 0;
    int		l=0;
    int		i, j, iret;
    int		ksyms_per_code = 2;
    XKeyInfoPtr k;
    KeySym	save[1024];
    KeySym	*ret = NULL;
    KeySym	*sret = NULL;

    k = (XKeyInfoPtr) FindInputClass (list, KeyClass);
    if (k == NULL)
	return;

    ret = XGetDeviceKeyMapping (display, dev, k->min_keycode, 
	k->num_keys, &ksyms_per_code);

    expect = baddevice;
    error_code = XGetDeviceKeyMapping_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    ret = XGetDeviceKeyMapping (display, dev, k->min_keycode, 
	k->num_keys, &ksyms_per_code);
    dev->device_id = xpointer->id;
    ret = XGetDeviceKeyMapping (display, dev, k->min_keycode, 
	k->num_keys, &ksyms_per_code);
    dev->device_id = xkeyboard->id;
    ret = XGetDeviceKeyMapping (display, dev, k->min_keycode, 
	k->num_keys, &ksyms_per_code);
    XSync (display, 0);

    dev->device_id = saveid;
    expect = BadValue;
    ret = XGetDeviceKeyMapping (display, dev, k->min_keycode-1, 
	k->num_keys, &ksyms_per_code);
    ret = XGetDeviceKeyMapping (display, dev, k->max_keycode+1, 
	k->num_keys, &ksyms_per_code);
    ret = XGetDeviceKeyMapping (display, dev, k->min_keycode, 
	k->max_keycode - k->min_keycode + 2, &ksyms_per_code);
    XSync (display, 0);

    expect = baddevice;
    error_code = XChangeDeviceKeyMapping_code;
    dev->device_id = -1;
    iret = XChangeDeviceKeyMapping (display, dev, k->min_keycode, 
	ksyms_per_code, sret, k->num_keys);
    dev->device_id = xpointer->id;
    iret = XChangeDeviceKeyMapping (display, dev, k->min_keycode, 
	ksyms_per_code, sret, k->num_keys);
    dev->device_id = xkeyboard->id;
    iret = XChangeDeviceKeyMapping (display, dev, k->min_keycode, 
	ksyms_per_code, sret, k->num_keys);
    dev->device_id = saveid;
    XSync (display, 0);

    expect = BadValue;
    iret = XChangeDeviceKeyMapping (display, dev, k->min_keycode-1, 
	ksyms_per_code, sret, k->num_keys);
    iret = XChangeDeviceKeyMapping (display, dev, k->min_keycode, 
	ksyms_per_code, sret, k->max_keycode - k->min_keycode + 2);
    iret = XChangeDeviceKeyMapping (display, dev, k->min_keycode, 
	0, sret, k->num_keys);
    XSync (display, 0);

    }

/******************************************************************
 *
 * This function exercises the device modifier mapping functions.
 * X Errors:
 *    XGetDeviceModifierMapping
 *        1). BadDevice
 *    XSetDeviceModifierMapping
 *        1). BadDevice
 *        2). BadValue
 *        3). BadLength - requires bogus library.
 *
 */

do_modifier_mapping (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int			saveid = 0;
    int			status = 0;
    int			i,j;
    int			k=0;
    int			iret;
    XModifierKeymap	*ret = NULL;
    KeyCode		*first;
    KeyCode		savcodes[256];

    expect = baddevice;
    error_code = XGetDeviceModifierMapping_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    ret = XGetDeviceModifierMapping (display, dev);
    dev->device_id = xpointer->id;
    ret = XGetDeviceModifierMapping (display, dev);
    dev->device_id = xkeyboard->id;
    ret = XGetDeviceModifierMapping (display, dev);
    XSync (display, 0);

    dev->device_id = -1;
    error_code = XSetDeviceModifierMapping_code;
    iret = XSetDeviceModifierMapping (display, dev, ret);
    dev->device_id = xpointer->id;
    iret = XSetDeviceModifierMapping (display, dev, ret);
    dev->device_id = xkeyboard->id;
    iret = XSetDeviceModifierMapping (display, dev, ret);
    XSync (display, 0);
    dev->device_id = saveid;

    expect = BadValue;
    ret = XGetDeviceModifierMapping (display, dev);
    ret->modifiermap[0] = 7;
    iret = XSetDeviceModifierMapping (display, dev, ret);
    XSync (display, 0);
    }

/******************************************************************
 *
 * This function gets the motion history for a device that supports
 * input class Valuators.
 * X Errors:
 *    1). BadDevice
 *    2). BadMatch - see create_valuator_errors.
 *
 */

get_motion_history (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int			saveid;
    Time		start = CurrentTime;
    Time		stop = CurrentTime;
    int			nevents;
    int			mode;
    int			axis_count;
    XDeviceTimeCoord	*events;

    expect = baddevice;
    error_code = XGetDeviceMotionEvents_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    events = XGetDeviceMotionEvents (display, dev, start, stop, &nevents,
	&mode, &axis_count);
    dev->device_id = xpointer->id;
    events = XGetDeviceMotionEvents (display, dev, start, stop, &nevents,
	&mode, &axis_count);
    dev->device_id = xkeyboard->id;
    events = XGetDeviceMotionEvents (display, dev, start, stop, &nevents,
	&mode, &axis_count);
    XSync (display, 0);
    dev->device_id = saveid;
    }

/******************************************************************
 *
 * This function changes the X pointer to the specified device.
 * X Errors:
 *    1). BadDevice
 *    2). BadMatch - see also create_valuator_errors.
 *
 */

change_pointer_device (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int	saveid;

    expect = baddevice;
    error_code = XChangePointerDevice_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XChangePointerDevice (display, dev, 0, 1);
    dev->device_id = xpointer->id;
    XChangePointerDevice (display, dev, 0, 1);
    dev->device_id = xkeyboard->id;
    XChangePointerDevice (display, dev, 0, 1);
    XSync (display, 0);
    dev->device_id = saveid;

    expect = BadMatch;
    XChangePointerDevice (display, dev, -1, 1);
    XChangePointerDevice (display, dev, 0, -1);
    XChangePointerDevice (display, dev, 999, 1);
    XChangePointerDevice (display, dev, 0, 999);
    XSync (display, 0);
    }

/******************************************************************
 *
 * This function changes the X keyboard to the specified device.
 * X Errors:
 *    1). BadDevice
 *    2). BadMatch - see also create_valuator_errors.
 *
 */

change_keyboard_device (display, name, dev, list)
    Display		*display;
    char		*name;
    XDevice		*dev;
    XDeviceInfoPtr	list;
    {
    int			saveid;
    int			i;
    int			ndevices;
    XKeyInfoPtr 	k;
    XDeviceInfoPtr	list2;
    XDevice		*dev2;

    expect = baddevice;
    error_code = XChangeKeyboardDevice_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XChangeKeyboardDevice (display, dev, -1, 1);
    dev->device_id = xpointer->id;
    XChangeKeyboardDevice (display, dev, -1, 1);
    dev->device_id = xkeyboard->id;
    XChangeKeyboardDevice (display, dev, -1, 1);
    XSync (display, 0);
    dev->device_id = saveid;

    }

/******************************************************************
 *
 * This function queries the extension version.
 * X Errors:
 *    none.
 *
 */

query_extension_version (display)
    {
    int			status = 0;
    XExtensionVersion 	*ext;

    ext = XGetExtensionVersion(display, "");
    if (ext->present)
	status = -1;

    ext = XGetExtensionVersion(display, "XInputExtension");

    }

/******************************************************************
 *
 * This function queries the state of a device.
 * X Errors:
 *    BadDevice.
 *
 */

query_device_state (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int			saveid;
    XDeviceState 	*state;

    expect = baddevice;
    error_code = XQueryDeviceState_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    state = XQueryDeviceState (display, dev);
    dev->device_id = xpointer->id;
    state = XQueryDeviceState (display, dev);
    dev->device_id = xkeyboard->id;
    state = XQueryDeviceState (display, dev);
    XSync (display, 0);
    dev->device_id = saveid;

    }

/******************************************************************
 *
 * This function allows a frozen device to be thawed.
 * X Errors:
 *    BadDevice.
 *    BadValue.
 *
 */

allow_device_event (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int		saveid;

    expect = baddevice;
    error_code = XAllowDeviceEvents_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XAllowDeviceEvents (display, dev, AsyncThisDevice, CurrentTime);
    dev->device_id = xpointer->id;
    XAllowDeviceEvents (display, dev, AsyncThisDevice, CurrentTime);
    dev->device_id = xkeyboard->id;
    XAllowDeviceEvents (display, dev, AsyncThisDevice, CurrentTime);
    XSync (display, 0);
    dev->device_id = saveid;

    expect = BadValue;
    XAllowDeviceEvents (display, dev, -1, CurrentTime);
    XSync (display, 0);
    }

/******************************************************************
 *
 * This function gets the feedback control values of a device.
 * X Errors:
 *    XGetFeedbackControl
 *	1). BadDevice.
 *    XChangeFeedbackControl
 *	1). BadDevice.
 *	2). BadMatch.
 *	3). BadValue.
 *
 */

do_feedback_control (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int			saveid;
    int			i,j;
    int			num_feedbacks;
    int			mask=0;
    XFeedbackState	*state, *sav;
    XKbdFeedbackState	*kbd;
    XPtrFeedbackState	*ptr;
    XKbdFeedbackControl	kbdf;
    XPtrFeedbackControl	ptrf;

    expect = baddevice;
    error_code = XGetFeedbackControl_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    state = XGetFeedbackControl (display, dev, &num_feedbacks);
    dev->device_id = xpointer->id;
    state = XGetFeedbackControl (display, dev, &num_feedbacks);
    dev->device_id = xkeyboard->id;
    state = XGetFeedbackControl (display, dev, &num_feedbacks);
    XSync (display, 0);
    dev->device_id = saveid;

    state = XGetFeedbackControl (display, dev, &num_feedbacks);
    sav = state;

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

	    expect = baddevice;
    	    error_code = XChangeFeedbackControl_code;
	    dev->device_id = -1;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    dev->device_id = xpointer->id;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    dev->device_id = xkeyboard->id;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    XSync (display, 0);
	    dev->device_id = saveid;

	    expect = BadValue;
	    kbdf.pitch = -1;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    kbdf.pitch = 1;
	    kbdf.percent = -2;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    kbdf.percent = 1;
	    kbdf.duration = -2;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    kbdf.duration = 1;
	    kbdf.click = 101;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    kbdf.click = 100;
	    kbdf.key = 7;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    kbdf.key = 8;
	    kbdf.auto_repeat_mode = -2;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    XSync (display,0);

	    expect = BadMatch;
	    mask &= ~DvAutoRepeatMode;
	    XChangeFeedbackControl (display, dev, mask, &kbdf);
	    XSync (display,0);
	    }
	else if (state->class == PtrFeedbackClass)
	    {
	    mask = DvAccelNum | DvAccelDenom | DvThreshold;
	    ptrf.class = PtrFeedbackClass;
	    ptrf.length = sizeof (XPtrFeedbackControl);
	    ptrf.accelNum = 10;
	    ptrf.accelDenom = 2;
	    ptrf.threshold = 6;

	    expect = baddevice;
    	    error_code = XChangeFeedbackControl_code;
	    dev->device_id = -1;
	    XChangeFeedbackControl (display, dev, mask, &ptrf);
	    dev->device_id = xpointer->id;
	    XChangeFeedbackControl (display, dev, mask, &ptrf);
	    dev->device_id = xkeyboard->id;
	    XChangeFeedbackControl (display, dev, mask, &ptrf);
	    XSync (display, 0);
	    dev->device_id = saveid;

	    expect = BadValue;
	    ptrf.accelNum = -2;
	    XChangeFeedbackControl (display, dev, mask, &ptrf);
	    ptrf.accelNum = 1;
	    ptrf.accelDenom = -2;
	    XChangeFeedbackControl (display, dev, mask, &ptrf);
	    ptrf.threshold = -2;
	    XChangeFeedbackControl (display, dev, mask, &ptrf);
	    XSync (display, 0);
	    }
	}
    }

/******************************************************************
 *
 * This function changes the mode of a device.
 * X Errors:
 *    1). BadDevice.
 *    2). BadMode.
 *
 */

set_device_mode (display, name, dev)
    Display	*display;
    char	*name;
    XDevice	*dev;
    {
    int		saveid;

    expect = baddevice;
    error_code = XSetDeviceMode_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XSetDeviceMode (display, dev, Absolute);
    dev->device_id = xpointer->id;
    XSetDeviceMode (display, dev, Absolute);
    dev->device_id = xkeyboard->id;
    XSetDeviceMode (display, dev, Absolute);
    XSync (display, 0);
    dev->device_id = saveid;

    expect = badmode;
    XSetDeviceMode (display, dev, -1);
    XSync (display, 0);
    }

/******************************************************************
 *
 * This function sends an extension event to a client.
 * X Errors:
 *    1). BadDevice.
 *    2). BadMode.
 *
 */

send_extension_event (display, name, dev, win)
    Display	*display;
    char	*name;
    XDevice	*dev;
    Window	win;
    {
    int			i;
    int			saveid;
    XEvent		event;
    XDeviceKeyEvent	kev;

    kev.type = devicekeypress;
    kev.serial =  1;
    kev.send_event = 0;
    kev.display = display;
    kev.root = root;
    kev.x = 10;
    kev.y = 10;
    kev.same_screen = 1;
    kev.subwindow = win;
    kev.deviceid = dev->device_id;
    kev.state = 0;
    kev.keycode = 0xd;
    kev.axes_count = 0;
    kev.window = win;
    kev.x_root = 160;
    kev.y_root = 160;
    kev.time = 0xf5f5f5f5;
    kev.axes_count = 6;
    kev.first_axis = 0;
    for (i=0; i<6; i++)
        kev.axis_data[i] = i;

    expect = baddevice;
    error_code = XSendExtensionEvent_code;
    saveid = dev->device_id;

    dev->device_id = -1;
    XSendExtensionEvent (display, dev, win, True, 
			class[saveid].valid, 
			&class[saveid].class[0], &kev);
    dev->device_id = xpointer->id;
    XSendExtensionEvent (display, dev, win, True, 
			class[saveid].valid, 
			&class[saveid].class[0], &kev);
    dev->device_id = xkeyboard->id;
    XSendExtensionEvent (display, dev, win, True, 
			class[saveid].valid, 
			&class[saveid].class[0], &kev);
    XSync (display, 0);

    dev->device_id = saveid;
    expect = BadWindow;
    XSendExtensionEvent (display, dev, -1, True, 
			class[saveid].valid, 
			&class[saveid].class[0], &kev);
    XSync (display, 0);

    expect = badclass;
    XSendExtensionEvent (display, dev, win, True, 2,
	bogusclass, &kev);
    XSync (display, 0);

    kev.type = KeyPress;
    expect = BadValue;
    XSendExtensionEvent (display, dev, win, True, 
			class[saveid].valid, 
			&class[saveid].class[0], &kev);
    XSync (display, 0);

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
    gstatus = -1;
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

