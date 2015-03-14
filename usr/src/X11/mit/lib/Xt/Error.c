/* $XConsortium: Error.c,v 1.33 91/11/09 15:38:01 keith Exp $ */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
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

#include "IntrinsicI.h"
#include <stdio.h>

/* The error handlers in the application context aren't used since we can't
   come up with a uniform way of using them.  If you can, define
   GLOBALERRORS to be FALSE (or 0). */

#ifndef GLOBALERRORS
#define GLOBALERRORS 1
#endif

#if GLOBALERRORS
static XrmDatabase errorDB = NULL;
static Boolean error_inited = FALSE;
void _XtDefaultErrorMsg(), _XtDefaultWarningMsg(), 
	_XtDefaultError(), _XtDefaultWarning();
static XtErrorMsgHandler errorMsgHandler = _XtDefaultErrorMsg;
static XtErrorMsgHandler warningMsgHandler = _XtDefaultWarningMsg;
static XtErrorHandler errorHandler = _XtDefaultError;
static XtErrorHandler warningHandler = _XtDefaultWarning;
#endif /* GLOBALERRORS */

XrmDatabase *XtGetErrorDatabase()
{
#if GLOBALERRORS
    return &errorDB;
#else
    return XtAppGetErrorDatabase(_XtDefaultAppContext());
#endif /* GLOBALERRORS */
}

XrmDatabase *XtAppGetErrorDatabase(app)
	XtAppContext app;
{
#if GLOBALERRORS
	return &errorDB;
#else
	return &app->errorDB;
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtGetErrorDatabaseText(
    register _Xconst char* name,
    register _Xconst char* type,
    register _Xconst char* class,
    _Xconst char* defaultp,
    String buffer,
    int nbytes
    )
#else
void XtGetErrorDatabaseText(name,type,class,defaultp, buffer, nbytes)
    register String name, type, class;
    String defaultp;
    String buffer;
    int nbytes;
#endif
{
#if GLOBALERRORS
    XtAppGetErrorDatabaseText(NULL,
	    name,type,class,defaultp, buffer, nbytes, NULL);
#else
    XtAppGetErrorDatabaseText(_XtDefaultAppContext(),
	    name,type,class,defaultp, buffer, nbytes, NULL);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtAppGetErrorDatabaseText(
    XtAppContext app,
    register _Xconst char* name,
    register _Xconst char* type,
    register _Xconst char* class,
    _Xconst char* defaultp,
    String buffer,
    int nbytes,
    XrmDatabase db
    )
#else
void XtAppGetErrorDatabaseText(app, name,type,class,defaultp,
	buffer, nbytes, db)
    XtAppContext app;
    register String name, type, class;
    String defaultp;
    String buffer;
    int nbytes;
    XrmDatabase db;
#endif
{
    String str_class;
    String type_str;
    XrmValue result;
    char str_name[BUFSIZ];
    char temp[BUFSIZ];

#if GLOBALERRORS
    if (error_inited == FALSE) {
        _XtInitErrorHandling (&errorDB);
        error_inited = TRUE;
    }
#else
    if (app->error_inited == FALSE) {
        _XtInitErrorHandling (&app->errorDB);
        app->error_inited = TRUE;
    }
#endif /* GLOBALERRORS */
    (void) sprintf(str_name, "%s.%s", name, type);
    /* XrmGetResource requires the name and class to be fully qualified
     * and to have the same number of components. */
    str_class = (char *)class;
    if (! strchr(class, '.')) {
	(void) sprintf(temp, "%s.%s", class, class);
	str_class = temp;
    }
    if (db == NULL) {
#if GLOBALERRORS
	(void) XrmGetResource(errorDB, str_name, str_class, &type_str,
			      &result);
#else
	(void) XrmGetResource(app->errorDB, str_name, str_class, &type_str,
			      &result);
#endif /* GLOBALERRORS */
    } else (void) XrmGetResource(db, str_name, str_class, &type_str, &result);
    if (result.addr) {
        (void) strncpy (buffer, result.addr, nbytes);
        if (result.size > nbytes) buffer[nbytes-1] = 0;
    } else {
	int len = strlen(defaultp);
	if (len >= nbytes) len = nbytes-1;
	bcopy(defaultp, buffer, len);
	buffer[len] = '\0';
    }
}

_XtInitErrorHandling (db)
    XrmDatabase *db;
{
    XrmDatabase errordb;

    errordb = XrmGetFileDatabase(ERRORDB);
    XrmMergeDatabases(errordb, db);
}

void _XtDefaultErrorMsg (name,type,class,defaultp,params,num_params)
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
{
    char buffer[1000], message[1000];
    XtGetErrorDatabaseText(name,type,class,defaultp, buffer, 1000);
/*need better solution here, perhaps use lower level printf primitives? */
    if (params == NULL || num_params == NULL || *num_params == 0)
	XtError(buffer);
    else {
	int i = *num_params;
	String par[10];
	if (i > 10) i = 10;
	bcopy( (char*)params, (char*)par, i * sizeof(String) );
	bzero( &par[i], (10-i) * sizeof(String) );
        (void) sprintf(message, buffer, par[0], par[1], par[2], par[3],
		       par[4], par[5], par[6], par[7], par[8], par[9]);
	XtError(message);
	if (i != *num_params)
	    XtWarning( "some arguments in previous message were lost" );
    }
}

void _XtDefaultWarningMsg (name,type,class,defaultp,params,num_params)
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
{

    char buffer[1000], message[1000];
    XtGetErrorDatabaseText(name,type,class,defaultp, buffer, 1000);
/*need better solution here*/
    if (params == NULL || num_params == NULL || *num_params == 0)
	XtWarning(buffer);
    else {
	int i = *num_params;
	String par[10];
	if (i > 10) i = 10;
	bcopy( (char*)params, (char*)par, i * sizeof(String) );
	bzero( &par[i], (10-i) * sizeof(String) );
        (void) sprintf(message, buffer, par[0], par[1], par[2], par[3],
		       par[4], par[5], par[6], par[7], par[8], par[9]);
	XtWarning(message); 
	if (i != *num_params)
	    XtWarning( "some arguments in previous message were lost" );
   }
}

#if NeedFunctionPrototypes
void XtErrorMsg(
    _Xconst char* name,
    _Xconst char* type,
    _Xconst char* class,
    _Xconst char* defaultp,
    String* params,
    Cardinal* num_params
    )
#else
void XtErrorMsg(name,type,class,defaultp,params,num_params)
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
#endif
{
#if GLOBALERRORS
    (*errorMsgHandler)((String)name,(String)type,(String)class,
		       (String)defaultp,params,num_params);
#else
    XtAppErrorMsg(_XtDefaultAppContext(),name,type,class,
	    defaultp,params,num_params);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtAppErrorMsg(
    XtAppContext app,
    _Xconst char* name,
    _Xconst char* type,
    _Xconst char* class,
    _Xconst char* defaultp,
    String* params,
    Cardinal* num_params
    )
#else
void XtAppErrorMsg(app, name,type,class,defaultp,params,num_params)
    XtAppContext app;
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
#endif
{
#if GLOBALERRORS
    (*errorMsgHandler)((String)name,(String)type,(String)class,
		       (String)defaultp,params,num_params);
#else
    (*app->errorMsgHandler)(name,type,class,defaultp,params,num_params);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtWarningMsg(
    _Xconst char* name,
    _Xconst char* type,
    _Xconst char* class,
    _Xconst char* defaultp,
    String* params,
    Cardinal* num_params
    )
#else
void XtWarningMsg(name,type,class,defaultp,params,num_params)
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
#endif
{
#if GLOBALERRORS
    (*warningMsgHandler)((String)name,(String)type,(String)class,
			 (String)defaultp,params,num_params);
#else
    XtAppWarningMsg(_XtDefaultAppContext(),name,type,class,
	    defaultp,params,num_params);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtAppWarningMsg(
    XtAppContext app,
    _Xconst char* name,
    _Xconst char* type,
    _Xconst char* class,
    _Xconst char* defaultp,
    String* params,
    Cardinal* num_params
    )
#else
void XtAppWarningMsg(app,name,type,class,defaultp,params,num_params)
    XtAppContext app;
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
#endif
{
#if GLOBALERRORS
    (*warningMsgHandler)((String)name,(String)type,(String)class,
			 (String)defaultp,params,num_params);
#else
    (*app->warningMsgHandler)(name,type,class,defaultp,params,num_params);
#endif /* GLOBALERRORS */
}

void XtSetErrorMsgHandler(handler)
    XtErrorMsgHandler handler;
{
#if GLOBALERRORS
    if (handler != NULL) errorMsgHandler = handler;
    else errorMsgHandler  = _XtDefaultErrorMsg;
#else
    XtAppSetErrorMsgHandler(_XtDefaultAppContext(), handler);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
XtErrorMsgHandler XtAppSetErrorMsgHandler(
    XtAppContext app,
    XtErrorMsgHandler handler)
#else
XtErrorMsgHandler XtAppSetErrorMsgHandler(app,handler)
    XtAppContext app;
    XtErrorMsgHandler handler;
#endif
{
    XtErrorMsgHandler old;
#if GLOBALERRORS
    old = errorMsgHandler;
    if (handler != NULL) errorMsgHandler = handler;
    else errorMsgHandler  = _XtDefaultErrorMsg;
#else
    old = app->errorMsgHandler;
    if (handler != NULL) app->errorMsgHandler = handler;
    else app->errorMsgHandler  = _XtDefaultErrorMsg;
#endif /* GLOBALERRORS */
    return old;
}

void XtSetWarningMsgHandler(handler)
    XtErrorMsgHandler handler;
{
#if GLOBALERRORS
    if (handler != NULL) warningMsgHandler  = handler;
    else warningMsgHandler = _XtDefaultWarningMsg;
#else
    XtAppSetWarningMsgHandler(_XtDefaultAppContext(),handler);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
XtErrorMsgHandler XtAppSetWarningMsgHandler(
    XtAppContext app,
    XtErrorMsgHandler handler)
#else
XtErrorMsgHandler XtAppSetWarningMsgHandler(app,handler)
    XtAppContext app;
    XtErrorMsgHandler handler;
#endif
{
    XtErrorMsgHandler old;
#if GLOBALERRORS
    old = warningMsgHandler;
    if (handler != NULL) warningMsgHandler  = handler;
    else warningMsgHandler = _XtDefaultWarningMsg;
#else
    old = app->warningMsgHandler;
    if (handler != NULL) app->warningMsgHandler  = handler;
    else app->warningMsgHandler = _XtDefaultWarningMsg;
#endif /* GLOBALERRORS */
    return old;
}

void _XtDefaultError(message)
    String message;
{
    extern void exit();

    (void)fprintf(stderr, "%sError: %s\n", XTERROR_PREFIX, message);
    exit(1);
}

void _XtDefaultWarning(message)
    String message;
{
    if (message && *message)
       (void)fprintf(stderr, "%sWarning: %s\n", XTWARNING_PREFIX, message); 
    return;
}

#if NeedFunctionPrototypes
void XtError(
    _Xconst char* message
    )
#else
void XtError(message)
    String message;
#endif
{
#if GLOBALERRORS
    (*errorHandler)((String)message);
#else
    XtAppError(_XtDefaultAppContext(),message);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtAppError(
    XtAppContext app,
    _Xconst char* message
    )
#else
void XtAppError(app,message)
    XtAppContext app;
    String message;
#endif
{
#if GLOBALERRORS
    (*errorHandler)((String)message);
#else
    (*app->errorHandler)(message);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtWarning(
    _Xconst char* message
    )
#else
void XtWarning(message)
    String message;
#endif
{
#if GLOBALERRORS
    (*warningHandler)((String)message);
#else
    XtAppWarning(_XtDefaultAppContext(),message);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtAppWarning(
    XtAppContext app,
    _Xconst char* message
    )
#else
void XtAppWarning(app,message)
    XtAppContext app;
    String message;
#endif
{
#if GLOBALERRORS
    (*warningHandler)((String)message);
#else
    (*app->warningHandler)(message);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
void XtSetErrorHandler(XtErrorHandler handler)
#else
void XtSetErrorHandler(handler)
    XtErrorHandler handler;
#endif
{
#if GLOBALERRORS
    if (handler != NULL) errorHandler = handler;
    else errorHandler  = _XtDefaultError;
#else
    XtAppSetErrorHandler(_XtDefaultAppContext(),handler);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
XtErrorHandler XtAppSetErrorHandler(
    XtAppContext app,
    XtErrorHandler handler)
#else
XtErrorHandler XtAppSetErrorHandler(app,handler)
    XtAppContext app;
    XtErrorHandler handler;
#endif
{
    XtErrorHandler old;
#if GLOBALERRORS
    old = errorHandler;
    if (handler != NULL) errorHandler = handler;
    else errorHandler  = _XtDefaultError;
#else
    old = app->errorHandler;
    if (handler != NULL) app->errorHandler = handler;
    else app->errorHandler  = _XtDefaultError;
#endif /* GLOBALERRORS */
    return old;
}

#if NeedFunctionPrototypes
void XtSetWarningHandler(XtErrorHandler handler)
#else
void XtSetWarningHandler(handler)
    XtErrorHandler handler;
#endif
{
#if GLOBALERRORS
    if (handler != NULL) warningHandler = handler;
    else warningHandler = _XtDefaultWarning;
#else
    XtAppSetWarningHandler(_XtDefaultAppContext(),handler);
#endif /* GLOBALERRORS */
}

#if NeedFunctionPrototypes
XtErrorHandler XtAppSetWarningHandler(
    XtAppContext app,
    XtErrorHandler handler)
#else
XtErrorHandler XtAppSetWarningHandler(app,handler)
    XtAppContext app;
    XtErrorHandler handler;
#endif
{
    XtErrorHandler old;
#if GLOBALERRORS
    old = warningHandler;
    if (handler != NULL) warningHandler  = handler;
    else warningHandler = _XtDefaultWarning;
#else
    old = app->warningHandler;
    if (handler != NULL) app->warningHandler  = handler;
    else app->warningHandler = _XtDefaultWarning;
#endif /* GLOBALERRORS */
    return old;
}

void _XtSetDefaultErrorHandlers(errMsg, warnMsg, err, warn)
	XtErrorMsgHandler *errMsg, *warnMsg;
	XtErrorHandler *err, *warn;
{
#ifndef GLOBALERRORS
	*errMsg = _XtDefaultErrorMsg;
	*warnMsg = _XtDefaultWarningMsg;
	*err = _XtDefaultError;
	*warn = _XtDefaultWarning;
#endif /* GLOBALERRORS */
}
