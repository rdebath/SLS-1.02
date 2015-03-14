/* $XConsortium: VarCreate.c,v 1.24 91/12/10 18:57:25 converse Exp $ */

/*

Copyright 1985, 1986, 1987, 1988, 1989 by the
Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/

#include "IntrinsicI.h"
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include "VarargsI.h"

#if (defined(SUNSHLIB) || defined(AIXSHLIB)) && defined(SHAREDCODE)
#define XtToolkitInitialize _XtToolkitInitialize
#endif /* (SUNSHLIB || AIXSHLIB) && SHAREDCODE */

extern Widget _XtCreateWidget();
extern Widget _XtAppCreateShell();
extern Widget _XtCreatePopupShell();

static Widget
_XtVaCreateWidget(name, widget_class, parent, var, count)
    String      name;
    WidgetClass widget_class;
    Widget      parent;
    va_list     var;
    int		count;
{
    register Widget         widget;
    XtTypedArgList	    typed_args = NULL;
    Cardinal		    num_args;

    _XtVaToTypedArgList(var, count, &typed_args, &num_args);

    widget = _XtCreateWidget(name, widget_class, parent, (ArgList)NULL, 
		    (Cardinal)0, typed_args, num_args);

    if (typed_args != NULL) {
        XtFree((XtPointer)typed_args);
    }    

    return(widget);
}


#if NeedVarargsPrototypes
Widget
XtVaCreateWidget(
    _Xconst char* name,
    WidgetClass widget_class,
    Widget parent,
    ...)
#else
/*VARARGS3*/
Widget XtVaCreateWidget(name, widget_class, parent, va_alist)
    String name;
    WidgetClass widget_class;
    Widget parent;
    va_dcl
#endif
{
    va_list                 var;
    register Widget         widget;
    int			    total_count, typed_count;

    Va_start(var,parent);
    _XtCountVaList(var, &total_count, &typed_count);
    va_end(var);

    Va_start(var,parent);
    widget = _XtVaCreateWidget(name, widget_class, parent, var, total_count);
    va_end(var);

    return(widget);
}


#if NeedVarargsPrototypes
Widget
XtVaCreateManagedWidget(
    _Xconst char* name,
    WidgetClass widget_class,
    Widget parent,
    ...)
#else
/*VARARGS3*/
Widget XtVaCreateManagedWidget(name, widget_class, parent, va_alist)
    String name;
    WidgetClass widget_class;
    Widget parent;
    va_dcl
#endif
{
    va_list		var;
    register Widget	widget;
    int			total_count, typed_count;

    Va_start(var,parent);
    _XtCountVaList(var, &total_count, &typed_count);
    va_end(var);

    Va_start(var,parent);
    widget = _XtVaCreateWidget(name, widget_class, parent, var, total_count);
    XtManageChild(widget);
    va_end(var);

    return (widget);
}


#if NeedVarargsPrototypes
Widget
XtVaAppCreateShell(
    _Xconst char* name,
    _Xconst char* class,
    WidgetClass widget_class,
    Display* display,
    ...)
#else
/*VARARGS4*/
Widget XtVaAppCreateShell(name, class, widget_class, display, va_alist)
    String name;
    String class;
    WidgetClass widget_class;
    Display* display;
    va_dcl
#endif
{
    va_list                 var;
    register Widget         widget;
    XtTypedArgList          typed_args = NULL;
    Cardinal                num_args;
    int			    total_count, typed_count;

    Va_start(var,display);
    _XtCountVaList(var, &total_count, &typed_count);
    va_end(var);

    Va_start(var,display);

    _XtVaToTypedArgList(var, total_count, &typed_args, &num_args);
    widget = _XtAppCreateShell(name, class, widget_class, display,
		(ArgList)NULL, (Cardinal)0, typed_args, num_args);
    if (typed_args != NULL) {
	XtFree((XtPointer)typed_args);
    }
 
    va_end(var);
    return(widget);
}


#if NeedVarargsPrototypes
Widget
XtVaCreatePopupShell(
    _Xconst char* name,
    WidgetClass widget_class,
    Widget parent,
    ...)
#else
/*VARARGS3*/
Widget XtVaCreatePopupShell(name, widget_class, parent, va_alist)
    String name;
    WidgetClass widget_class;
    Widget parent;
    va_dcl
#endif
{
    va_list                 var;
    register Widget         widget;
    XtTypedArgList          typed_args = NULL;
    Cardinal                num_args;
    int			    total_count, typed_count;

    Va_start(var,parent);
    _XtCountVaList(var, &total_count, &typed_count);
    va_end(var);

    Va_start(var,parent);

    _XtVaToTypedArgList(var, total_count, &typed_args, &num_args);
    widget = _XtCreatePopupShell(name, widget_class, parent,
		(ArgList)NULL, (Cardinal)0, typed_args, num_args);
    if (typed_args != NULL) {
	XtFree((XtPointer)typed_args);
    }

    va_end(var);
    return widget;
}

#if NeedVarargsPrototypes
void
XtVaSetValues(Widget widget, ...)
#else
/*VARARGS1*/
void XtVaSetValues(widget, va_alist)
    Widget widget;
    va_dcl
#endif
{
    va_list                 var;
    ArgList                 args = NULL;
    Cardinal                num_args;
    int			    total_count, typed_count;

    Va_start(var,widget);
    _XtCountVaList(var, &total_count, &typed_count);
    va_end(var);

    Va_start(var,widget);

    _XtVaToArgList(widget, var, total_count, &args, &num_args);
    XtSetValues(widget, args, num_args);
    if (args != NULL) {
	XtFree((XtPointer)args);
    }

    va_end(var);
}


#if NeedVarargsPrototypes
void
XtVaSetSubvalues(XtPointer base, XtResourceList resources, Cardinal num_resources, ...)
#else
/*VARARGS3*/
void XtVaSetSubvalues(base, resources, num_resources, va_alist)
    XtPointer base;
    XtResourceList resources;
    Cardinal num_resources;
    va_dcl
#endif
{
    va_list	var;
    ArgList    	args;
    Cardinal   	num_args;
    int		total_count, typed_count;		

    Va_start(var, num_resources);
    _XtCountVaList(var, &total_count, &typed_count);
    va_end(var);

    if (typed_count != 0) {
	XtWarning("XtVaTypedArg is not valid in XtVaSetSubvalues()\n");
    }

    Va_start(var, num_resources);
    _XtVaToArgList((Widget)NULL, var, total_count, &args, &num_args);

    XtSetSubvalues(base, resources, num_resources, args, num_args);

    if (num_args != 0) {
        XtFree((XtPointer)args);
    }    

    va_end(var);
}

#if NeedVarargsPrototypes
Widget
_XtVaAppInitialize(
    XtAppContext *app_context_return,
    _Xconst char* application_class,
    XrmOptionDescList options,
    Cardinal num_options,
    int *argc_in_out,
    String *argv_in_out,
    String *fallback_resources,
    va_list var_args)
#else
/*VARARGS7*/
Widget _XtVaAppInitialize(app_context_return, application_class, options,
			  num_options, argc_in_out, argv_in_out,
			  fallback_resources, var_args)
    XtAppContext *app_context_return;
    char *application_class;
    XrmOptionDescList options;
    Cardinal num_options;
    int *argc_in_out;
    String *argv_in_out;
    String *fallback_resources;
    va_list var_args;
#endif
{
    va_list var;
    XtAppContext app_con;
    Display * dpy;
    register int saved_argc = *argc_in_out;
    Widget root;
    String attr;
    int count = 0;
    XtTypedArgList typed_args;

    XtToolkitInitialize(); /* cannot be moved into _XtAppInit */
    
    dpy = _XtAppInit(&app_con, (String)application_class, options, num_options,
		     argc_in_out, &argv_in_out, fallback_resources);

    var = var_args;
    for(attr = va_arg(var,String); attr != NULL; attr = va_arg(var,String)) {
        ++count;
        if (strcmp(attr, XtVaTypedArg) == 0) {
            va_arg(var, String);
            va_arg(var, String);
            va_arg(var, XtArgVal);
            va_arg(var, int);
        } else {
            va_arg(var, XtArgVal);
        }
    }
    va_end(var);

    var = var_args;
    typed_args = _XtVaCreateTypedArgList(var, count);
    va_end(var);

    root =
	XtVaAppCreateShell( NULL, application_class, 
			    applicationShellWidgetClass, dpy,
			    XtNscreen, (XtArgVal)DefaultScreenOfDisplay(dpy),
			    XtNargc, (XtArgVal)saved_argc,
			    XtNargv, (XtArgVal)argv_in_out,
			    XtVaNestedList, (XtVarArgsList)typed_args,
			    NULL );
   
    if (app_context_return != NULL)
	*app_context_return = app_con;

    XtFree((XtPointer)typed_args);
    XtFree((XtPointer)argv_in_out);
    return(root);
}

#if !((defined(SUNSHLIB) || defined(AIXSHLIB)) && defined(SHAREDCODE))

/*
 * If not used as a shared library, we still need a front end to 
 * _XtVaAppInitialize.
 */

#if NeedVarargsPrototypes
Widget
XtVaAppInitialize(
    XtAppContext *app_context_return,
    _Xconst char* application_class,
    XrmOptionDescList options,
    Cardinal num_options,
    int *argc_in_out,
    String *argv_in_out,
    String *fallback_resources,
    ...)
#else
Widget XtVaAppInitialize(app_context_return, application_class, options,
			 num_options, argc_in_out, argv_in_out,
			 fallback_resources, va_alist)
    XtAppContext *app_context_return;
    String application_class;
    XrmOptionDescList options;
    Cardinal num_options;
    int *argc_in_out;
    String *argv_in_out;
    String *fallback_resources;
    va_dcl
#endif
{
    va_list	var;

    Va_start(var, fallback_resources);    
    return _XtVaAppInitialize(app_context_return, (String)application_class,
			      options, num_options, argc_in_out, argv_in_out,
			      fallback_resources, var);
}

#endif /* !((SUNSHLIB || AIXSHLIB) && SHAREDCODE) */

