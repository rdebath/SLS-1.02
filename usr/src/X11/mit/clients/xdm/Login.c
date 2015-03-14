/*
 * xdm - display manager daemon
 *
 * $XConsortium: Login.c,v 1.35 91/07/18 19:31:10 rws Exp $
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

# include <X11/IntrinsicP.h>
# include <X11/StringDefs.h>
# include <X11/keysym.h>
# include <X11/Xfuncs.h>

# include <stdio.h>

# include "LoginP.h"

#define offset(field) XtOffsetOf(LoginRec, login.field)
#define goffset(field) XtOffsetOf(WidgetRec, core.field)

static XtResource resources[] = {
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
	goffset(width), XtRImmediate,	(XtPointer) 0},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
	goffset(height), XtRImmediate,	(XtPointer) 0},
    {XtNx, XtCX, XtRPosition, sizeof (Position),
	goffset(x), XtRImmediate,	(XtPointer) -1},
    {XtNy, XtCY, XtRPosition, sizeof (Position),
	goffset(y), XtRImmediate,	(XtPointer) -1},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(textpixel), XtRString,	XtDefaultForeground},
    {XtNpromptColor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(promptpixel), XtRString,	XtDefaultForeground},
    {XtNgreetColor, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(greetpixel), XtRString,	XtDefaultForeground},
    {XtNfailColor, XtCForeground, XtRPixel, sizeof (Pixel),
	offset(failpixel), XtRString,	XtDefaultForeground},
    {XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    	offset (font), XtRString,	"*-new century schoolbook-medium-r-normal-*-180-*"},
    {XtNpromptFont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    	offset (promptFont), XtRString, "*-new century schoolbook-bold-r-normal-*-180-*"},
    {XtNgreetFont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    	offset (greetFont), XtRString,	"*-new century schoolbook-bold-i-normal-*-240-*"},
    {XtNfailFont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
	offset (failFont), XtRString,	"*-new century schoolbook-bold-r-normal-*-180-*"},
    {XtNgreeting, XtCGreeting, XtRString, sizeof (char *),
    	offset(greeting), XtRString, "X Window System"},
    {XtNunsecureGreeting, XtCGreeting, XtRString, sizeof (char *),
	offset(unsecure_greet), XtRString, "This is an unsecure session"},
    {XtNnamePrompt, XtCNamePrompt, XtRString, sizeof (char *),
	offset(namePrompt), XtRString, "Login:  "},
    {XtNpasswdPrompt, XtCNamePrompt, XtRString, sizeof (char *),
	offset(passwdPrompt), XtRString, "Password:  "},
    {XtNfail, XtCFail, XtRString, sizeof (char *),
	offset(fail), XtRString, "Login incorrect"},
    {XtNfailTimeout, XtCFailTimeout, XtRInt, sizeof (int),
	offset(failTimeout), XtRImmediate, (XtPointer) 10},
    {XtNnotifyDone, XtCCallback, XtRFunction, sizeof (XtPointer),
	offset(notify_done), XtRFunction, (XtPointer) 0},
    {XtNsessionArgument, XtCSessionArgument, XtRString,	sizeof (char *),
	offset(sessionArg), XtRString, (XtPointer) 0 },
    {XtNsecureSession, XtCSecureSession, XtRBoolean, sizeof (Boolean),
	offset(secure_session), XtRImmediate, False },
    {XtNallowAccess, XtCAllowAccess, XtRBoolean, sizeof (Boolean),
	offset(allow_access), XtRImmediate, False }
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
# define GREET_X(w)	((int)(w->core.width - XTextWidth (w->login.greetFont,\
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
# define FAIL_X(w)	((int)(w->core.width - XTextWidth (w->login.failFont,\
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
void failTimeout (client_data, id)
    XtPointer	client_data;
    XtIntervalId *	id;
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
	XtAppAddTimeOut(XtWidgetToApplicationContext ((Widget)w),
			w->login.failTimeout * 1000,
		        failTimeout, (XtPointer) w);
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
DeleteBackwardChar (ctxw, event, params, num_params)
    Widget ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

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
DeleteForwardChar (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

    XorCursor (ctx);
    RemoveFail (ctx);
    switch (ctx->login.state) {
    case GET_NAME:
	if (ctx->login.cursor < (int)strlen (ctx->login.data.name)) {
	    EraseName (ctx, ctx->login.cursor);
	    strcpy (ctx->login.data.name + ctx->login.cursor,
		    ctx->login.data.name + ctx->login.cursor + 1);
	    DrawName (ctx, ctx->login.cursor);
	}
	break;
    case GET_PASSWD:
    	if (ctx->login.cursor < (int)strlen (ctx->login.data.passwd)) {
	    strcpy (ctx->login.data.passwd + ctx->login.cursor,
		    ctx->login.data.passwd + ctx->login.cursor + 1);
	}
	break;
    }
    XorCursor (ctx);	
}

/*ARGSUSED*/
static void
MoveBackwardChar (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget	ctx = (LoginWidget)ctxw;

    XorCursor (ctx);
    RemoveFail (ctx);
    if (ctx->login.cursor > 0)
    	ctx->login.cursor--;
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
MoveForwardChar (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

    XorCursor (ctx);
    RemoveFail (ctx);
    switch (ctx->login.state) {
    case GET_NAME:
    	if (ctx->login.cursor < (int)strlen(ctx->login.data.name))
	    ++ctx->login.cursor;
	break;
    case GET_PASSWD:
    	if (ctx->login.cursor < (int)strlen(ctx->login.data.passwd))
	    ++ctx->login.cursor;
	break;
    }
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
MoveToBegining (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

    XorCursor (ctx);
    RemoveFail (ctx);
    ctx->login.cursor = 0;
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
MoveToEnd (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

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
EraseToEndOfLine (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

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

/*ARGSUSED*/
static void
EraseLine (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    MoveToBegining (ctxw, event, params, num_params);
    EraseToEndOfLine (ctxw, event, params, num_params);
}

/*ARGSUSED*/
static void
FinishField (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

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
AllowAccess (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;
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
SetSessionArgument (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

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
RestartSession (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

    XorCursor (ctx);
    RemoveFail (ctx);
    ctx->login.state = DONE;
    ctx->login.cursor = 0;
    (*ctx->login.notify_done) (ctx, &ctx->login.data, NOTIFY_RESTART);
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
AbortSession (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

    XorCursor (ctx);
    RemoveFail (ctx);
    ctx->login.state = DONE;
    ctx->login.cursor = 0;
    (*ctx->login.notify_done) (ctx, &ctx->login.data, NOTIFY_ABORT);
    XorCursor (ctx);
}

/*ARGSUSED*/
static void
AbortDisplay (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

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

/* ARGSUSED */
static void
InsertChar (ctxw, event, params, num_params)
    Widget	ctxw;
    XEvent	*event;
    String	*params;
    Cardinal	*num_params;
{
    LoginWidget ctx = (LoginWidget)ctxw;

    char strbuf[128];
    int  len;

    len = XLookupString (&event->xkey, strbuf, sizeof (strbuf), 0, 0);
    strbuf[len] = '\0';
    if (len + (int)strlen(ctx->login.data.name) >= NAME_LEN - 1)
    	len = NAME_LEN - strlen(ctx->login.data.name) - 2;
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

/*ARGSUSED*/
static void FetchDisplayArg(widget, size, value)
    Widget widget;
    Cardinal *size;
    XrmValue* value;
{
    value->size = sizeof(Display*);
    value->addr = (caddr_t)&DisplayOfScreen(XtScreenOfObject(widget));
}

static XtConvertArgRec displayConvertArg[] = {
    {XtProcedureArg, (XtPointer)FetchDisplayArg, 0},
};

/* ARGSUSED */
static void Initialize (greq, gnew, args, num_args)
    Widget greq, gnew;
    ArgList args;
    Cardinal *num_args;
{
    LoginWidget w = (LoginWidget)gnew;
    XtGCMask	valuemask, xvaluemask;
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
    xvaluemask = valuemask | GCFunction;
    w->login.xorGC = XtGetGC (gnew, xvaluemask, &myXGCV);

    /*
     * Note that the second argument is a GCid -- QueryFont accepts a GCid and
     * returns the curently contained font.
     */

    if (w->login.font == NULL)
	w->login.font = XQueryFont (XtDisplay (w),
		XGContextFromGC (DefaultGCOfScreen (XtScreen (w))));

    xvaluemask = valuemask;
    if (w->login.promptFont == NULL)
        w->login.promptFont = w->login.font;
    else
	xvaluemask |= GCFont;

    myXGCV.foreground = w->login.promptpixel;
    myXGCV.font = w->login.promptFont->fid;
    w->login.promptGC = XtGetGC (gnew, xvaluemask, &myXGCV);

    xvaluemask = valuemask;
    if (w->login.greetFont == NULL)
    	w->login.greetFont = w->login.font;
    else
	xvaluemask |= GCFont;

    myXGCV.foreground = w->login.greetpixel;
    myXGCV.font = w->login.greetFont->fid;
    w->login.greetGC = XtGetGC (gnew, xvaluemask, &myXGCV);

    xvaluemask = valuemask;
    if (w->login.failFont == NULL)
	w->login.failFont = w->login.font;
    else
	xvaluemask |= GCFont;

    myXGCV.foreground = w->login.failpixel;
    myXGCV.font = w->login.failFont->fid;
    w->login.failGC = XtGetGC (gnew, xvaluemask, &myXGCV);

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
    if ((x = w->core.x) == -1)
	x = (int)(WidthOfScreen (XtScreen (w)) - w->core.width) / 2;
    if ((y = w->core.y) == -1)
	y = (int)(HeightOfScreen (XtScreen (w)) - w->core.height) / 3;
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
    XtReleaseGC(gw, w->login.textGC);
    XtReleaseGC(gw, w->login.bgGC);
    XtReleaseGC(gw, w->login.xorGC);
    XtReleaseGC(gw, w->login.promptGC);
    XtReleaseGC(gw, w->login.greetGC);
    XtReleaseGC(gw, w->login.failGC);
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
static Boolean SetValues (current, request, new, args, num_args)
    Widget  current, request, new;
    ArgList args;
    Cardinal *num_args;
{
    LoginWidget currentL, newL;
    
    currentL = (LoginWidget) current;
    newL = (LoginWidget) new;
    if (GREETING (currentL) != GREETING (newL))
	return True;
    return False;
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
    /* class_initialize		*/	NULL,
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
