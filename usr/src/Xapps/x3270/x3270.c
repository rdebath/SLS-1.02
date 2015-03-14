/*
 * Copyright 1989 by Georgia Tech Research Corporation, Atlanta, GA.
 * Copyright 1988, 1989 by Robert Viduya.
 * Copyright 1990 Jeff Sparkes.
 *
 *                         All Rights Reserved
 */

/*
 *	x3270.c
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Core.h>
#include <X11/Shell.h>
#include "X.h"
#include <stdio.h>

Display		*display;
Widget		toplevel;
XtAppContext	appcontext;
Atom		protocol, delete_me;
int		net_sock;
int		background, foreground;
Font		ibmfont;
XFontStruct     *ibmfontinfo;
int		ROWS, COLS;
int		model_num;
extern XtActionsRec actions[];
extern int	actioncount;


static XrmOptionDescRec options[]= {
	{ "-keymap",	".keymap",	XrmoptionSepArg,	"ncd" },
	{ "-model",	".model",	XrmoptionSepArg,	"4" },
	{ "-mono",	".mono",	XrmoptionNoArg,		"true" },
};

#define offset(field) XtOffset(AppResptr, field)
static XtResource resources[] = {
	{ XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
	  offset(foreground), XtRString, "XtDefaultForeground", },
	{ XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
	  offset(background), XtRString, "XtDefaultBackground", },
	{ "colorBackground", "ColorBackground", XtRPixel, sizeof(Pixel),
	  offset(colorbg), XtRString, "grey70", },
	{ "normalColor", "NormalColor", XtRPixel, sizeof(Pixel),
	  offset(normal), XtRString, "green", },
	{ "inputColor", "InputColor", XtRPixel, sizeof(Pixel),
	  offset(select), XtRString, "white", },
	{ "boldColor", "BoldColor", XtRPixel, sizeof(Pixel),
	  offset(bold), XtRString, "blue", },
	{ "mono", "Mono", XtRBoolean, sizeof(Boolean),
	  offset(mono), XtRString, "false" },
	{ XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	  offset(fontstruct), XtRString, "3270", },
	{ "model", "Model", XtRString, sizeof(char *),
	  offset(model), XtRString, "4", },
	{ "keymap", "Keymap", XtRString, sizeof(char *),
	  offset(keymap), XtRString, 0, },
};
#undef offset

extern void net_input();
char *get_resource();
AppRes	appres;
static char *trans = "<Expose>:Redraw()\n";


main (argc, argv)
int	argc;
char	*argv[];
{
    Arg		args[12];
    int		i = 0;
    XrmDatabase rdb;
    char	buf[100];
    char	*kbdtrans = 0, *usertrans = 0;
    XtTranslations table;

    XtSetArg(args[i], XtNinput, True); i++;
    XtSetArg(args[i], XtNallowShellResize, False); i++;

    toplevel = XtAppInitialize(&appcontext, "X3270", options, 
	XtNumber(options), &argc, argv, 0, args, i);

    display = XtDisplay(toplevel);

    XtGetApplicationResources(toplevel, &appres, resources,
    		XtNumber(resources), 0, 0);

    XtAppAddActions(appcontext, actions, actioncount);
    if (argc != 2) {
	sprintf (buf, "usage: %s ibm-host\n", argv[0]);
	XtError(buf);
	exit (1);
    }
    foreground = appres.foreground;
    background = appres.background;
    ibmfontinfo = appres.fontstruct;
    ibmfont = appres.fontstruct->fid;
    switch (atoi(appres.model)) {
	case 2:
	    ROWS = 24; COLS = 80;
	    model_num = 2;
	    break;
	case 3:
	    ROWS = 32; COLS = 80;
	    model_num = 3;
	    break;
	case 4:
	    ROWS = 43; COLS = 80;
	    model_num = 4;
	    break;
	case 5:
	    ROWS = 27; COLS = 132;
	    model_num = 5;
	    break;
	default:
	    {
		sprintf(buf, "unknown model %s, defaulting to 4", appres.model);
	        XtWarning(buf);
	        ROWS = 43; COLS = 80;
	        model_num = 4;
	    }
	    break;
    }
    net_sock = connect_net (argv[1]);
    XtAppAddInput(appcontext, net_sock, XtInputReadMask, net_input, net_sock);
    XtAppAddInput(appcontext, net_sock, XtInputExceptMask, net_input, 0);

    if (appres.keymap == NULL)
	appres.keymap = (char *)getenv("KEYMAP");
    if (appres.keymap == NULL)
	appres.keymap = (char *)getenv("KEYBD");
    if (appres.keymap == NULL) {
        XtWarning("Using default keymap only");
    } else {
        rdb = XtDatabase(display);
	sprintf(buf, "keymap.%s", appres.keymap);
	kbdtrans = get_resource(rdb, argv[0], buf);
	sprintf(buf, "keymap.%s.user", appres.keymap);
	usertrans = get_resource(rdb, argv[0], buf);
    }
    screen_init (kbdtrans, usertrans);
    protocol = XInternAtom(display, "WM_PROTOCOLS", False);
    delete_me = XInternAtom(display, "WM_DELETE_WINDOW", False);
    XSetWMProtocols(display, XtWindow(toplevel), &delete_me, 1);

    table = XtParseTranslationTable(trans);
    XtOverrideTranslations(toplevel, table);

    while (1) {
	XtInputMask	im;
	XEvent		event;

	while ((im = XtAppPending(appcontext)) == XtIMXEvent) {
	    XtAppNextEvent(appcontext, &event);
	    /*
	    if (event.type = ClientMessage) {
		XClientMessageEvent *e = (XClientMessageEvent *)&event;

		if (e->message_type == protocol && e->data.l[0] == delete_me)
		    exit(0);
	    }
	    */
	    XtDispatchEvent(&event);
	}
	screen_disp();
	XtAppProcessEvent(appcontext, XtIMAll);
    }
    /*
    (void) shutdown (net_sock, 2);
    (void) close (net_sock);
    exit (0);
    */
}

/*
 * A way to work around bugs with Xt resources.  It seems to be impossible
 * to get arbitrarily named resources.  Someday this should be hacked to
 * add classes too.
 */
char *
get_resource(rdb, prog, name)
    XrmDatabase rdb;
    char	*prog, *name;
{
    XrmValue	value;
    char	*type[20];
    char	buf[20], str[1024];
    char	*s;

    if ((s = rindex(prog, '/')) != 0)
	prog = s+1;
    sprintf(str, "%s.%s", prog, name);
    if (XrmGetResource(rdb, str, 0, type, &value) == True) {
	return XtNewString(value.addr);
    } else {
	return 0;
    }
}
