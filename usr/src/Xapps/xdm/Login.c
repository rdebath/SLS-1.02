/*
 * xdm - display manager daemon
 *
 * $XConsortium: Login.c,v 1.21 89/12/12 13:58:52 rws Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * Login.c
 */

# include <stdio.h>

# include <X11/IntrinsicP.h>
# include <X11/StringDefs.h>

# include "LoginP.h"

extern void	bcopy ();

#define offset(field) XtOffset(LoginWidget,login.field)
#define goffset(field) XtOffset(Widget,core.field)

static XtResource resources[] = {
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	goffset(width), XtRString,	"0"},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	goffset(height), XtRString,	"0"},
    {XtNx, XtCX, XtRPosition, sizeof (Position),
	goffset(x), XtRString,		"-1"},
    {XtNy, XtCY, XtRPosition, sizeof (Position),
	goffset(y), XtRString,		"-1"},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(textpixel), XtRString,	"Black"},
    {XtNpromptColor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(promptpixel), XtRString,	"Black"},
    {XtNgreetColor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(greetpixel), XtRString,	"Black"},
    {XtNfailColor, XtCForeground, XtRPixel, sizeof (Pixel),
	offset(failpixel), XtRString,	"Black"},
    {XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    	offset (font), XtRString,	"*-new century schoolbook-medium-r-normal-*-180-*"},
    {XtNpromptFont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    	offset (promptFont), XtRString, "*-new century schoolbook-bold-r-normal-*-180-*"},
    {XtNgreetFont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    	offset (greetFont), XtRString,	"*-new century schoolbook-bold-i-normal-*-240-*"},
    {XtNfailFont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	offset (failFont), XtRString,	"*-new century schoolbook-bold-r-normal-*-180-*"},
    {XtNgreeting, XtCGreeting, XtRString, sizeof (char *),
    	offset(greeting), XtRString, "Welcome to the X Window System"},
    {XtNunsecureGreeting, XtCGreeting, XtRString, sizeof (char *),
	offset(unsecure_greet), XtRString, "This is an unsecure session"},
    {XtNnamePrompt, XtCNamePrompt, XtRString, sizeof (char *),
	offset(namePrompt), XtRString, "Login:  "},
    {XtNpasswdPrompt, XtCNamePrompt, XtRString, sizeof (char *),
	offset(passwdPrompt), XtRString, "Password:  "},
    {XtNfail, XtCFail, XtRString, sizeof (char *),
	offset(fail), XtRString, "Login failed, please try again."},
    {XtNfailTimeout, XtCFailTimeout, XtRInt, sizeof (int),
	offset(failTimeout), XtRString, "10"},
    {XtNnotifyDone, XtCCallback, XtRFunction, sizeof (caddr_t),
	offset(notify_done), XtRFunction, (caddr_t) 0},
    {XtNsessionArgument, XtCSessionArgument, XtRString,	sizeof (char *),
	offset(sessionArg), XtRString, (char *) 0 },
    {XtNsecureSession, XtCSecureSession, XtRBoolean, sizeof (Boolean),
	offset(secure_session), XtRString, "false" },
    {XtNallowAccess, XtCAllowAccess, XtRBoolean, sizeof (Boolean),
	offset(allow_access), XtRString, "false" }
};

#undef offset
#undef goffset

# define TEXT_X_INC(w)	((w)->login.font->max_bounds.width)
# define TEXT_Y_INC(w)	((w)->login.font->max_bounds.ascent +\
			 (w)->login.font->max_bounds.descent)
# define PROMPT_X_INC(w)	((w)->login.promptFont->max_bounds.width)
# define PROMPT_Y_INC(w)	((w)->login.promptFont->max_bounds.ascent +\
			 (w)->login.promptFont->max_bounds.descent)
# define GREET_X_INC(w)	((w)->login.greetFont->max_bounds.width)
# define GREET_Y_INC(w)	((w)->login.greetFont->max_bounds.ascent +\
			 (w)->login.greetFont->max_bounds.descent)
# define FAIL_X_INC(w)	((w)->login.failFont->max_bounds.width)
# define FAIL_Y_INC(w)	((w)->login.failFont->max_bounds.ascent +\
			 (w)->login.failFont->max_bounds.descent)

# define Y_INC(w)	max (TEXT_Y_INC(w), PROMPT_Y_INC(w))

# define LOGIN_PROMPT_W(w) (XTextWidth (w->login.promptFont,\
				 w->login.namePrompt,\
				 strlen (w->login.namePrompt)))
# define PASS_PROMPT_W(w) (XTextWidth (w->login.promptFont,\
				 w->login.passwdPrompt,\
				 strlen (w->login.passwdPrompt)))
# define PROMPT_W(w)	(max(LOGIN_PROMPT_W(w), PASS_PROMPT_W(w)))
# define GREETING(w)	((w)->login.secure_session  && !(w)->login.allow_access ?\
				(w)->login.greeting : (w)->login.unsecure_greet)
# define GREET_X(w)	((w->core.width - XTextWidth (w->login.greetFont,\
			GREETING(w), strlen (GREETING(w)))) / 2)
# define GREET_Y(w)	(GREETING(w)[0] ? 2 * GREET_Y_INC (w) : 0)
# define GREET_W(w)	(max (XTextWidth (w->login.greetFont,\
			      w->login.greeting, strlen (w->login.greeting)), \
			      XTextWidth (w->login.greetFont,\
			      w->login.unsecure_greet, strlen (w->login.unsecure_greet))))
# define LOGIN_X(w)	(2 * PROMPT_X_INC(w))
# define LOGIN_Y(w)	(GREET_Y(w) + GREET_Y_INC(w) +\
			 w->login.greetFont->max_bounds.ascent + Y_INC(w))
# define LOGIN_W(w)	(w->core.width - 6 * TEXT_X_INC(w))
# define LOGIN_H(w)	(3 * Y_INC(w) / 2)
# define LOGIN_TEXT_X(w)(LOGIN_X(w) + PROMPT_W(w))
# define PASS_X(w)	(LOGIN_X(w))
# define PASS_Y(w)	(LOGIN_Y(w) + 8 * Y_INC(w) / 5)
# define PASS_W(w)	(LOGIN_W(w))
# define PASS_H(w)	(LOGIN_H(w))
# define PASS_TEXT_X(w)	(PASS_X(w) + PROMPT_W(w))
# define FAIL_X(w)	((w->core.width - XTextWidth (w->login.failFont,\
				w->login.fail, strlen (w->login.fail))) / 2)
# define FAIL_Y(w)	(PASS_Y(w) + 2 * FAIL_Y_INC (w) +\
			w->login.failFont->max_bounds.ascent)
# define FAIL_W(w)	(XTextWidth (w->login.failFont,\
			 w->login.fail, strlen (w->login.fail)))

# define PAD_X(w)	(2 * (LOGIN_X(w) + max (GREET_X_INC(w), FAIL_X_INC(w))))

# define PAD_Y(w)	(max (max (Y_INC(w), GREET_Y_INC(w)),\
			     FAIL_Y_INC(w)))
	
static void Initialize(), Realize(), Destroy(), Redisplay();
static Boolean SetValues();
static void draw_it ();

static void ClassInitialize();

static int max (a,b) { return a > b ? a : b; }

static void
EraseName (w, cursor)
    LoginWidget	w;
    int		cursor;
{
    int	x;

    x = LOGIN_TEXT_X (w);
    if (cursor > 0)
	x += XTextWidth (w->login.font, w->login.data.name, cursor);
    XDrawString (XtDisplay(w), XtWindow (w), w->login.bgGC, x, LOGIN_Y(w),
		w->login.data.name + cursor, strlen (w->login.data.name + cursor));
}

static void
DrawName (w, cursor)
    LoginWidget	w;
    int		cursor;
{
    int	x;

    x = LOGIN_TEXT_X (w);
    if (cursor > 0)
	x += XTextWidth (w->login.font, w->login.data.name, cursor);
    XDrawString (XtDisplay(w), XtWindow (w), w->login.textGC, x, LOGIN_Y(w),
		w->login.data.name + cursor, strlen (w->login.data.name + cursor));
}

static void
realizeCursor (w, gc)
    LoginWidget	w;
    GC		gc;
{
    int	x, y;
    int height, width;

    switch (w->login.state) {
    case GET_NAME:
	x = LOGIN_TEXT_X (w);
	y = LOGIN_Y (w);
	height = w->login.font->max_bounds.ascent + w->login.font->max_bounds.descent;
	width = 1;
	if (w->login.cursor > 0)
	    x += XTextWidth (w->login.font, w->login.data.name, w->login.cursor);
	break;
    case GET_PASSWD:
	x = PASS_TEXT_X (w);
	y = PASS_Y (w);
	height = w->login.font->max_bounds.ascent + w->login.font->max_bounds.descent;
	width = 1;
	break;
    default:
	return;
    }
    XFillRectangle (XtDisplay (w), XtWindow (w), gc,
		    x, y - w->login.font->max_bounds.ascent, width, height);
}

static void
EraseFail (w)
    LoginWidget	w;
{
    int x = FAIL_X(w);
    int y = FAIL_Y(w);

    XSetForeground (XtDisplay (w), w->login.failGC,
			w->core.background_pixel);
    XDrawString (XtDisplay (w), XtWindow (w), w->login.failGC,
		x, y,
		w->login.fail, strlen (w->login.fail));
    w->login.failUp = 0;
    XSetForeground (XtDisplay (w), w->login.failGC,
			w->login.failpixel);
}

static void
XorCursor (w)
    LoginWidget	w;
{
    realizeCursor (w, w->login.xorGC);
}

static void
RemoveFail (w)
    LoginWidget	w;
{
    if (w->login.failUp)
	EraseFail (w);
}

static void
EraseCursor (w)
    LoginWidget (w);
{
    realizeCursor (w, w->login.bgGC);
}

/*ARGSUSED*/
failTimeout (client_data, id)
    caddr_t	client_data;
    XtIntervalId	id;
{
    LoginWidget	w = (LoginWidget)client_data;

    Debug ("failTimeout\n");
    EraseFail (w);
}

DrawFail (ctx)
    Widget	ctx;
{
    LoginWidget	w;

    w = (LoginWidget) ctx;
    XorCursor (w);
    ResetLogin (w);
    XorCursor (w);
    w->login.failUp = 1;
    RedrawFail (w);
    if (w->login.failTimeout > 0) {
	Debug ("failTimeout: %d\n", w->login.failTimeout);
	XtAppAddTimeOut(XtWidgetToApplicationContext (w),
			w->login.failTimeout * 1000,
		        failTimeout, (caddr_t) w);
    }
}

RedrawFail (w)
    LoginWidget w;
{
    int x = FAIL_X(w);
    int y = FAIL_Y(w);

    if (w->login.failUp)
        XDrawString (XtDisplay (w), XtWindow (w), w->login.failGC,
		    x, y,
		    w->login.fail, strlen (w->login.fail));
}

static void
draw_it (w)
    LoginWidget	w;
{
    EraseCursor (w);
    if (GREETING(w)[0])
	    XDrawString (XtDisplay (w), XtWindow (w), w->login.greetGC,
			GREET_X(w), GREET_Y(w),
			GREETING(w), strlen (GREETING(w)));
    XDrawString (XtDisplay (w), XtWindow (w), w->login.promptGC,
		LOGIN_X(w), LOGIN_Y(w),
		w->login.namePrompt, strlen (w->login.namePrompt));
    XDrawString (XtDisplay (w), XtWindow (w), w->login.promptGC,
		PASS_X(w), PASS_Y(w),
		w->login.passwdPrompt, strlen (w->login.passwdPrompt));
    RedrawFail (w);
    DrawName (w, 0);
    XorCursor (w);
    /*
     * The GrabKeyboard here is needed only because of
     * a bug in the R3 server -- the keyboard is grabbed on
     * the root window, and the server won't dispatch events
     * to the focus window unless the focus window is a ancestor
     * of the grab window.  Bug in server already found and fixed,
     * compatibility until at least R4.
     */
    if (XGrabKeyboard (XtDisplay (w), XtWindow (w), False, GrabModeAsync,
		       GrabModeAsync, CurrentTime) != GrabSuccess)
    {
	XSetInputFocus (XtDisplay (w), XtWindow (w),
			RevertToPointerRoot, CurrentTime);
    }
}

/*ARGSUSED*/
static void
DeleteBackwardChar (ctx, event)
    LoginWidget ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    if (ctx->login.cursor > 0) {
	ctx->login.cursor--;
	switch (ctx->login.state) {
	case GET_NAME:
	    EraseName (ctx, ctx->login.cursor);
	    strcpy (ctx->login.data.name + ctx->login.cursor,
		    ctx->login.data.name + ctx->login.cursor + 1);
	    DrawName (ctx, ctx->login.cursor);
	    break;
	case GET_PASSWD:
	    strcpy (ctx->login.data.passwd + ctx->login.cursor,
		    ctx->login.data.passwd + ctx->login.cursor + 1);
	    break;
	}
    }
    XorCursor (ctx);	
}

/*ARGSUSED*/
static void
DeleteForwardChar (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    switch (ctx->login.state) {
    case GET_NAME:
	if (ctx->login.cursor < strlen (ctx->login.data.name)) {
	    EraseName (ctx, ctx->login.cursor);
	    strcpy (ctx->login.data.name + ctx->login.cursor,
		    ctx->login.data.name + ctx->login.cursor + 1);
	    DrawName (ctx, ctx->login.cursor);
	}
	break;
    case GET_PASSWD:
    	if (ctx->login.cursor < strlen (ctx->login.data.passwd)) {
	    strcpy (ctx->login.data.passwd + ctx->login.cursor,
		    ctx->login.data.passwd + ctx->login.cursor + 1);
	}
	break;
    }
    XorCursor (ctx);	
}

/*ARGSUSED*/
static void
MoveBackwardChar (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    if (ctx->login.cursor > 0)
    	ctx->login.cursor--;
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
MoveForwardChar (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    switch (ctx->login.state) {
    case GET_NAME:
    	if (ctx->login.cursor < strlen (ctx->login.data.name))
	    ++ctx->login.cursor;
	break;
    case GET_PASSWD:
    	if (ctx->login.cursor < strlen (ctx->login.data.passwd))
	    ++ctx->login.cursor;
	break;
    }
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
MoveToBegining (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    ctx->login.cursor = 0;
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
MoveToEnd (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    switch (ctx->login.state) {
    case GET_NAME:
    	ctx->login.cursor = strlen (ctx->login.data.name);
	break;
    case GET_PASSWD:
    	ctx->login.cursor = strlen (ctx->login.data.passwd);
	break;
    }
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
EraseToEndOfLine (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    switch (ctx->login.state) {
    case GET_NAME:
	EraseName (ctx, ctx->login.cursor);
	ctx->login.data.name[ctx->login.cursor] = '\0';
	break;
    case GET_PASSWD:
	ctx->login.data.passwd[ctx->login.cursor] = '\0';
	break;
    }
    XorCursor (ctx);
}

static void
EraseLine (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    MoveToBegining (ctx, event);
    EraseToEndOfLine (ctx, event);
}

/*ARGSUSED*/
static void
FinishField (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    switch (ctx->login.state) {
    case GET_NAME:
	ctx->login.state = GET_PASSWD;
	ctx->login.cursor = 0;
	break;
    case GET_PASSWD:
	ctx->login.state = DONE;
	ctx->login.cursor = 0;
	(*ctx->login.notify_done) (ctx, &ctx->login.data, NOTIFY_OK);
	break;
    }
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
AllowAccess (ctx, event, params, num_params)
    LoginWidget	ctx;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    Arg	arglist[1];
    Boolean allow;

    RemoveFail (ctx);
    XtSetArg (arglist[0], XtNallowAccess, (char *) &allow);
    XtGetValues ((Widget) ctx, arglist, 1);
    XtSetArg (arglist[0], XtNallowAccess, !allow);
    XtSetValues ((Widget) ctx, arglist, 1);
}

/*ARGSUSED*/
static void
SetSessionArgument (ctx, event, params, num_params)
    LoginWidget	ctx;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    RemoveFail (ctx);
    if (ctx->login.sessionArg)
	XtFree (ctx->login.sessionArg);
    ctx->login.sessionArg = 0;
    if (*num_params > 0) {
	ctx->login.sessionArg = XtMalloc (strlen (params[0]) + 1);
	if (ctx->login.sessionArg)
	    strcpy (ctx->login.sessionArg, params[0]);
	else
	    LogOutOfMem ("set session argument");
    }
}

/*ARGSUSED*/
static void
RestartSession (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    ctx->login.state = DONE;
    ctx->login.cursor = 0;
    (*ctx->login.notify_done) (ctx, &ctx->login.data, NOTIFY_RESTART);
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
AbortSession (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    ctx->login.state = DONE;
    ctx->login.cursor = 0;
    (*ctx->login.notify_done) (ctx, &ctx->login.data, NOTIFY_ABORT);
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
AbortDisplay (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    XorCursor (ctx);
    RemoveFail (ctx);
    ctx->login.state = DONE;
    ctx->login.cursor = 0;
    (*ctx->login.notify_done) (ctx, &ctx->login.data, NOTIFY_ABORT_DISPLAY);
    XorCursor (ctx);
}

ResetLogin (w)
    LoginWidget	w;
{
    EraseName (w, 0);
    w->login.cursor = 0;
    w->login.data.name[0] = '\0';
    w->login.data.passwd[0] = '\0';
    w->login.state = GET_NAME;
}

static void
InsertChar (ctx, event)
    LoginWidget	ctx;
    XEvent	*event;
{
    char strbuf[128];
    int  len;

    len = XLookupString (&event->xkey, strbuf, sizeof (strbuf), 0, 0);
    strbuf[len] = '\0';
    if (len + ctx->login.cursor >= NAME_LEN - 1)
    	len = NAME_LEN - ctx->login.cursor - 2;
    if (len == 0)
	return;
    XorCursor (ctx);
    RemoveFail (ctx);
    switch (ctx->login.state) {
    case GET_NAME:
	EraseName (ctx, ctx->login.cursor);
	bcopy (ctx->login.data.name + ctx->login.cursor,
	       ctx->login.data.name + ctx->login.cursor + len,
	       strlen (ctx->login.data.name + ctx->login.cursor) + 1);
	bcopy (strbuf, ctx->login.data.name + ctx->login.cursor, len);
	DrawName (ctx, ctx->login.cursor);
	ctx->login.cursor += len;
	break;
    case GET_PASSWD:
	bcopy (ctx->login.data.passwd + ctx->login.cursor,
	       ctx->login.data.passwd + ctx->login.cursor + len,
	       strlen (ctx->login.data.passwd + ctx->login.cursor) + 1);
	bcopy (strbuf, ctx->login.data.passwd + ctx->login.cursor, len);
	ctx->login.cursor += len;
	break;
    }
    XorCursor (ctx);
}

static void
ClassInitialize ()
{
}

/* ARGSUSED */
static void Initialize (greq, gnew)
    Widget greq, gnew;
{
    LoginWidget w = (LoginWidget)gnew;
    XtGCMask	valuemask;
    XGCValues	myXGCV;
    Arg		position[2];
    Position	x, y;

    myXGCV.foreground = w->login.textpixel;
    myXGCV.background = w->core.background_pixel;
    valuemask = GCForeground | GCBackground;
    if (w->login.font) {
	myXGCV.font = w->login.font->fid;
	valuemask |= GCFont;
    }
    w->login.textGC = XtGetGC(gnew, valuemask, &myXGCV);
    myXGCV.foreground = w->core.background_pixel;
    w->login.bgGC = XtGetGC(gnew, valuemask, &myXGCV);

    myXGCV.foreground = w->login.textpixel ^ w->core.background_pixel;
    myXGCV.function = GXxor;
    valuemask |= GCFunction;
    w->login.xorGC = XtGetGC (gnew, valuemask, &myXGCV);

    /*
     * Note that the second argument is a GCid -- QueryFont accepts a GCid and
     * returns the curently contained font.
     */

    if (w->login.font == NULL)
	w->login.font = XQueryFont (XtDisplay (w),
		XGContextFromGC (DefaultGCOfScreen (XtScreen (w))));

    if (w->login.promptFont == NULL)
        w->login.promptFont = w->login.font;

    if (w->login.greetFont == NULL)
    	w->login.greetFont = w->login.font;

    if (w->login.failFont == NULL)
	w->login.failFont = w->login.font;

    valuemask = GCForeground | GCBackground | GCFont;
    myXGCV.foreground = w->login.promptpixel;
    myXGCV.font = w->login.promptFont->fid;
    w->login.promptGC = XtGetGC (gnew, valuemask, &myXGCV);

    myXGCV.foreground = w->login.greetpixel;
    myXGCV.font = w->login.greetFont->fid;
    w->login.greetGC = XtGetGC (gnew, valuemask, &myXGCV);

    myXGCV.foreground = w->login.failpixel;
    myXGCV.font = w->login.failFont->fid;
    w->login.failGC = XtGetGC (gnew, valuemask, &myXGCV);

    w->login.data.name[0] = '\0';
    w->login.data.passwd[0] = '\0';
    w->login.state = GET_NAME;
    w->login.cursor = 0;
    w->login.failUp = 0;
    if (w->core.width == 0)
	w->core.width = max (GREET_W(w), FAIL_W(w)) + PAD_X(w);
    if (w->core.height == 0) {
	int fy = FAIL_Y(w);
	int pady = PAD_Y(w);

	w->core.height = fy + pady;	/* for stupid compilers */
    }
    x = (WidthOfScreen (XtScreen (w)) - w->core.width) / 2;
    y = (HeightOfScreen (XtScreen (w)) - w->core.height) / 3;
    XtSetArg (position[0], XtNx, x);
    XtSetArg (position[1], XtNy, y);
    XtSetValues (XtParent (w), position, (Cardinal) 2);
}

 
static void Realize (gw, valueMask, attrs)
     Widget gw;
     XtValueMask *valueMask;
     XSetWindowAttributes *attrs;
{
    XtCreateWindow( gw, (unsigned)InputOutput, (Visual *)CopyFromParent,
		     *valueMask, attrs );
}

static void Destroy (gw)
     Widget gw;
{
    LoginWidget w = (LoginWidget)gw;
    bzero (w->login.data.name, NAME_LEN);
    bzero (w->login.data.passwd, NAME_LEN);
    XtDestroyGC (w->login.textGC);
    XtDestroyGC (w->login.bgGC);
    XtDestroyGC (w->login.xorGC);
    XtDestroyGC (w->login.promptGC);
    XtDestroyGC (w->login.greetGC);
}

/* ARGSUSED */
static void Redisplay(gw, event, region)
     Widget gw;
     XEvent *event;
     Region region;
{
    draw_it ((LoginWidget) gw);
}

/*ARGSUSED*/
static Boolean SetValues (current, request, new)
    Widget  current, request, new;
{
    LoginWidget currentL, newL, w;
    
    currentL = (LoginWidget) current;
    newL = (LoginWidget) new;
    if (GREETING (currentL) != GREETING (newL)) {
	w = currentL;
	XSetForeground (XtDisplay (w), w->login.greetGC,
			w->core.background_pixel);
	XDrawString (XtDisplay (w), XtWindow (w), w->login.greetGC,
			GREET_X(w), GREET_Y(w),
			GREETING(w), strlen (GREETING(w)));
	w = newL;
	XSetForeground (XtDisplay (w), w->login.greetGC,
			w->login.greetpixel);
	XDrawString (XtDisplay (w), XtWindow (w), w->login.greetGC,
			GREET_X(w), GREET_Y(w),
			GREETING(w), strlen (GREETING(w)));
    }
    return 0;
}

char defaultLoginTranslations [] =
"\
Ctrl<Key>H:	delete-previous-character() \n\
Ctrl<Key>D:	delete-character() \n\
Ctrl<Key>B:	move-backward-character() \n\
Ctrl<Key>F:	move-forward-character() \n\
Ctrl<Key>A:	move-to-begining() \n\
Ctrl<Key>E:	move-to-end() \n\
Ctrl<Key>K:	erase-to-end-of-line() \n\
Ctrl<Key>U:	erase-line() \n\
Ctrl<Key>X:	erase-line() \n\
Ctrl<Key>C:	restart-session() \n\
Ctrl<Key>\\\\:	abort-session() \n\
:Ctrl<Key>plus:	allow-all-access() \n\
<Key>BackSpace:	delete-previous-character() \n\
<Key>Delete:	delete-previous-character() \n\
<Key>Return:	finish-field() \n\
<Key>:		insert-char() \
";

XtActionsRec loginActionsTable [] = {
  {"delete-previous-character",	DeleteBackwardChar},
  {"delete-character",		DeleteForwardChar},
  {"move-backward-character",	MoveBackwardChar},
  {"move-forward-character",	MoveForwardChar},
  {"move-to-begining",		MoveToBegining},
  {"move-to-end",		MoveToEnd},
  {"erase-to-end-of-line",	EraseToEndOfLine},
  {"erase-line",		EraseLine},
  {"finish-field", 		FinishField},
  {"abort-session",		AbortSession},
  {"abort-display",		AbortDisplay},
  {"restart-session",		RestartSession},
  {"insert-char", 		InsertChar},
  {"set-session-argument",	SetSessionArgument},
  {"allow-all-access",		AllowAccess},
};

LoginClassRec loginClassRec = {
    { /* core fields */
    /* superclass		*/	&widgetClassRec,
    /* class_name		*/	"Login",
    /* size			*/	sizeof(LoginRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	loginActionsTable,
    /* num_actions		*/	XtNumber (loginActionsTable),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	defaultLoginTranslations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
    }
};

WidgetClass loginWidgetClass = (WidgetClass) &loginClassRec;

