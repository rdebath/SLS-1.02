/*
 * $XConsortium: sharedlib.c,v 1.14 92/01/06 17:01:45 gildea Exp $
 * 
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * This file is used to force shared libraries to get the right routines.  For
 * Sun shared libraries, this only wants to be compiled when we are *not*
 * generating shared code so that this gets copied into the application binary.
 */

#if (defined(SUNSHLIB) || defined(AIXSHLIB)) && !defined(SHAREDCODE)
#include "IntrinsicI.h"
#include "VarargsI.h"
#include "ShellP.h"
#include "VendorP.h"


#ifdef AIXSHLIB
WidgetClass vendorShellWidgetClass = (WidgetClass) &vendorShellClassRec;

static void _XtVendorInitialize()
{
    transientShellWidgetClass->core_class.superclass =
	(WidgetClass) &vendorShellClassRec;
    topLevelShellWidgetClass->core_class.superclass =
	(WidgetClass) &vendorShellClassRec;
}

#define VENDORINIT _XtVendorInitialize();

#else

#define VENDORINIT /* as nothing */

#endif

#ifdef SUNSHLIB
/*
 * _XtInherit needs to be statically linked since it is compared against as
 * well as called.
 */
void _XtInherit()
{
    extern void __XtInherit();
    __XtInherit();
}
#endif

/*
 * The following routine will be called by every toolkit
 * application, forcing this file to be statically linked.
 *
 * Note: Both XtInitialize and XtAppInitialize call XtToolkitInitialize.
 */

void XtToolkitInitialize()
{
    extern void _XtToolkitInitialize();
    VENDORINIT
    _XtToolkitInitialize();
}

#if NeedFunctionPrototypes
Widget 
XtInitialize(
_Xconst char* name,
_Xconst char* classname,
XrmOptionDescRec *options,
Cardinal num_options,
int *argc,
String *argv
)
#else
Widget 
XtInitialize(name, classname, options, num_options, argc, argv)
String name, classname;
XrmOptionDescRec *options;
Cardinal num_options;
String *argv;
int *argc;
#endif
{
    extern Widget _XtInitialize();
    VENDORINIT
    return _XtInitialize (name, classname, options, num_options, argc, argv);
}

#if NeedFunctionPrototypes
Widget
XtAppInitialize(
XtAppContext * app_context_return,
_Xconst char* application_class,
XrmOptionDescRec *options,
Cardinal num_options,
int *argc_in_out,
String *argv_in_out,
String *fallback_resources,
ArgList args_in,
Cardinal num_args_in
)
#else
Widget
XtAppInitialize(app_context_return, application_class, options, num_options,
		argc_in_out, argv_in_out, fallback_resources, 
		args_in, num_args_in)
XtAppContext * app_context_return;
String application_class;
XrmOptionDescRec *options;
Cardinal num_options, num_args_in;
int *argc_in_out;
String *argv_in_out, * fallback_resources;     
ArgList args_in;
#endif
{
    extern Widget _XtAppInitialize();
    VENDORINIT
    return _XtAppInitialize (app_context_return, application_class, options,
			     num_options, argc_in_out, argv_in_out, 
			     fallback_resources, args_in, num_args_in);
}

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
    extern Widget _XtVaAppInitialize();

    VENDORINIT
    Va_start(var, fallback_resources);
    return _XtVaAppInitialize(app_context_return, application_class, options,
			      num_options, argc_in_out, argv_in_out,
			      fallback_resources, var);
}

#else

#ifndef lint
static int dummy;			/* avoid warning from ranlib */
#endif

#endif /* SUNSHLIB or AIXSHLIB */

#if defined(SUNSHLIB) && !defined(SHAREDCODE)

int _XtInheritTranslations = 0;

extern CompositeClassRec compositeClassRec;
WidgetClass compositeWidgetClass = (WidgetClass) &compositeClassRec;

extern ConstraintClassRec constraintClassRec;
WidgetClass constraintWidgetClass = (WidgetClass) &constraintClassRec;

extern WidgetClassRec widgetClassRec;
WidgetClass widgetClass = &widgetClassRec;
WidgetClass coreWidgetClass = &widgetClassRec;

extern ObjectClassRec objectClassRec;
WidgetClass objectClass = (WidgetClass)&objectClassRec;

extern RectObjClassRec rectObjClassRec;
WidgetClass rectObjClass = (WidgetClass)&rectObjClassRec;

extern ShellClassRec shellClassRec;
WidgetClass shellWidgetClass = (WidgetClass) &shellClassRec;

extern OverrideShellClassRec overrideShellClassRec;
WidgetClass overrideShellWidgetClass = (WidgetClass) &overrideShellClassRec;

extern WMShellClassRec wmShellClassRec;
WidgetClass wmShellWidgetClass = (WidgetClass) &wmShellClassRec;

extern TransientShellClassRec transientShellClassRec;
WidgetClass transientShellWidgetClass = (WidgetClass) &transientShellClassRec;

extern TopLevelShellClassRec topLevelShellClassRec;
WidgetClass topLevelShellWidgetClass = (WidgetClass) &topLevelShellClassRec;

extern ApplicationShellClassRec applicationShellClassRec;
WidgetClass applicationShellWidgetClass = (WidgetClass) &applicationShellClassRec;

#endif /* SUNSHLIB */
