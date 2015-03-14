/* $XConsortium: Shell.c,v 1.127 92/06/08 14:28:33 converse Exp $ */

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

#define SHELL

#ifndef DEFAULT_WM_TIMEOUT
#define DEFAULT_WM_TIMEOUT 5000
#endif

#include "IntrinsicI.h"
#include "StringDefs.h"
#include "Shell.h"
#include "ShellP.h"
#include "Vendor.h"
#include "VendorP.h"
#include <X11/Xatom.h>
#include <X11/Xlocale.h>
#include <stdio.h>

/***************************************************************************
 *
 * Note: per the Xt spec, the Shell geometry management assumes in
 * several places that there is only one managed child.  This is
 * *not* a bug.  Any subclass that assumes otherwise is broken.
 *
 ***************************************************************************/

#define WM_CONFIGURE_DENIED(w) (((WMShellWidget) (w))->wm.wm_configure_denied)
#define WM_MOVED(w) (((WMShellWidget) (w))->wm.wm_moved)

#define BIGSIZE ((Dimension)32767)

/***************************************************************************
 *
 * Default values for resource lists
 *
 ***************************************************************************/

#ifdef CRAY
void _XtShellDepth();
void _XtShellColormap();
void _XtShellAncestorSensitive();
void _XtTitleEncoding();
#else
static void _XtShellDepth();
static void _XtShellColormap();
static void _XtShellAncestorSensitive();
static void _XtTitleEncoding();
#endif

/***************************************************************************
 *
 * Shell class record
 *
 ***************************************************************************/

#define Offset(x)	(XtOffsetOf(ShellRec, x))
static XtResource shellResources[]=
{
	{XtNx, XtCPosition, XtRPosition, sizeof(Position),
	    Offset(core.x), XtRImmediate, (XtPointer)BIGSIZE},
	{XtNy, XtCPosition, XtRPosition, sizeof(Position),
	    Offset(core.y), XtRImmediate, (XtPointer)BIGSIZE},
	{ XtNdepth, XtCDepth, XtRInt, sizeof(int),
	    Offset(core.depth), XtRCallProc, (XtPointer) _XtShellDepth},
	{ XtNcolormap, XtCColormap, XtRColormap, sizeof(Colormap),
	    Offset(core.colormap), XtRCallProc, (XtPointer) _XtShellColormap},
	{ XtNancestorSensitive, XtCSensitive, XtRBoolean, sizeof(Boolean),
	    Offset(core.ancestor_sensitive), XtRCallProc,
	    (XtPointer) _XtShellAncestorSensitive},
	{ XtNallowShellResize, XtCAllowShellResize, XtRBoolean,
	    sizeof(Boolean), Offset(shell.allow_shell_resize),
	    XtRImmediate, (XtPointer)False},
	{ XtNgeometry, XtCGeometry, XtRString, sizeof(String), 
	    Offset(shell.geometry), XtRString, (XtPointer)NULL},
	{ XtNcreatePopupChildProc, XtCCreatePopupChildProc, XtRFunction,
	    sizeof(XtCreatePopupChildProc), Offset(shell.create_popup_child_proc),
	    XtRFunction, NULL},
	{ XtNsaveUnder, XtCSaveUnder, XtRBoolean, sizeof(Boolean),
	    Offset(shell.save_under), XtRImmediate, (XtPointer)False},
	{ XtNpopupCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	    Offset(shell.popup_callback), XtRCallback, (XtPointer) NULL},
	{ XtNpopdownCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	    Offset(shell.popdown_callback), XtRCallback, (XtPointer) NULL},
	{ XtNoverrideRedirect, XtCOverrideRedirect,
	    XtRBoolean, sizeof(Boolean), Offset(shell.override_redirect),
	    XtRImmediate, (XtPointer)False},
	{ XtNvisual, XtCVisual, XtRVisual, sizeof(Visual*),
	    Offset(shell.visual), XtRImmediate, CopyFromParent}
};

static void ClassPartInitialize(), Initialize();
static void Realize();
static void Resize();
static Boolean SetValues();
static void GetValuesHook();
static void ChangeManaged(); /* XXX */
static XtGeometryResult GeometryManager(), RootGeometryManager();
static void Destroy();

static ShellClassExtensionRec shellClassExtRec = {
    NULL,
    NULLQUARK,
    XtShellExtensionVersion,
    sizeof(ShellClassExtensionRec),
    RootGeometryManager
};

externaldef(shellclassrec) ShellClassRec shellClassRec = {
  {   /* Core */
    /* superclass	  */	(WidgetClass) &compositeClassRec,
    /* class_name	  */	"Shell",
    /* size		  */	sizeof(ShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	ClassPartInitialize,
    /* Class init'ed ?	  */	FALSE,
    /* initialize	  */	Initialize,
    /* initialize_notify  */	NULL,		
    /* realize		  */	Realize,
    /* actions		  */	NULL,
    /* num_actions	  */	0,
    /* resources	  */	shellResources,
    /* resource_count	  */	XtNumber(shellResources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	FALSE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	FALSE,
    /* visible_interest	  */	FALSE,
    /* destroy		  */	Destroy,
    /* resize		  */	Resize,
    /* expose		  */	NULL,
    /* set_values	  */	SetValues,
    /* set_values_hook	  */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook	  */	GetValuesHook,
    /* accept_focus	  */	NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets	  */	NULL,
    /* tm_table		  */	NULL,
    /* query_geometry	  */	NULL,
    /* display_accelerator*/	NULL,
    /* extension	  */	NULL
  },{ /* Composite */
    /* geometry_manager	  */	GeometryManager,
    /* change_managed	  */	ChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	NULL
  },{ /* Shell */
    /* extension	  */	(XtPointer)&shellClassExtRec
  }
};

externaldef(shellwidgetclass) WidgetClass shellWidgetClass = (WidgetClass) (&shellClassRec);

/***************************************************************************
 *
 * OverrideShell class record
 *
 ***************************************************************************/

static XtResource overrideResources[]=
{
	{ XtNoverrideRedirect, XtCOverrideRedirect,
	    XtRBoolean, sizeof(Boolean), Offset(shell.override_redirect),
	    XtRImmediate, (XtPointer)True},
	{ XtNsaveUnder, XtCSaveUnder, XtRBoolean, sizeof(Boolean),
	    Offset(shell.save_under), XtRImmediate, (XtPointer)True},
};

externaldef(overrideshellclassrec) OverrideShellClassRec overrideShellClassRec = {
  {
    /* superclass         */    (WidgetClass) &shellClassRec,
    /* class_name         */    "OverrideShell",
    /* size               */    sizeof(OverrideShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL,
    /* Class init'ed ?    */	FALSE,
    /* initialize         */    NULL,
    /* initialize_notify    */	NULL,		
    /* realize            */    XtInheritRealize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    overrideResources,
    /* resource_count     */	XtNumber(overrideResources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/ 	FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    XtInheritResize,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook      */	NULL,			
    /* set_values_almost    */	XtInheritSetValuesAlmost,  
    /* get_values_hook      */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		    */  NULL,
    /* query_geometry	    */  NULL,
    /* display_accelerator  */  NULL,
    /* extension	    */  NULL
  },{
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	    */  NULL
  },{
    /* extension	    */  NULL
  },{
    /* extension	    */  NULL
  }
};

externaldef(overrideshellwidgetclass) WidgetClass overrideShellWidgetClass = 
	(WidgetClass) (&overrideShellClassRec);

/***************************************************************************
 *
 * WMShell class record
 *
 ***************************************************************************/

#undef Offset
#define Offset(x)	(XtOffsetOf(WMShellRec, x))

static int default_unspecified_shell_int = XtUnspecifiedShellInt;
/*
 * Warning, casting XtUnspecifiedShellInt (which is -1) to an (XtPointer)
 * can result is loss of bits on some machines (i.e. crays)
 */

static XtResource wmResources[]=
{
	{ XtNtitle, XtCTitle, XtRString, sizeof(String),
	    Offset(wm.title), XtRString, NULL},
	{ XtNtitleEncoding, XtCTitleEncoding, XtRAtom, sizeof(Atom),
	    Offset(wm.title_encoding),
	    XtRCallProc, (XtPointer) _XtTitleEncoding},
	{ XtNwmTimeout, XtCWmTimeout, XtRInt, sizeof(int),
	    Offset(wm.wm_timeout), XtRImmediate,(XtPointer)DEFAULT_WM_TIMEOUT},
	{ XtNwaitForWm, XtCWaitForWm, XtRBoolean, sizeof(Boolean),
	    Offset(wm.wait_for_wm), XtRImmediate, (XtPointer)True},
	{ XtNtransient, XtCTransient, XtRBoolean, sizeof(Boolean),
	    Offset(wm.transient), XtRImmediate, (XtPointer)False},
/* size_hints minus things stored in core */
	{ XtNbaseWidth, XtCBaseWidth, XtRInt, sizeof(int),
	    Offset(wm.base_width),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNbaseHeight, XtCBaseHeight, XtRInt, sizeof(int),
	    Offset(wm.base_height),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNwinGravity, XtCWinGravity, XtRInt, sizeof(int),
	    Offset(wm.win_gravity),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNminWidth, XtCMinWidth, XtRInt, sizeof(int),
	    Offset(wm.size_hints.min_width),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNminHeight, XtCMinHeight, XtRInt, sizeof(int),
	    Offset(wm.size_hints.min_height),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNmaxWidth, XtCMaxWidth, XtRInt, sizeof(int),
	    Offset(wm.size_hints.max_width),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNmaxHeight, XtCMaxHeight, XtRInt, sizeof(int),
	    Offset(wm.size_hints.max_height),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNwidthInc, XtCWidthInc, XtRInt, sizeof(int),
	    Offset(wm.size_hints.width_inc),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNheightInc, XtCHeightInc, XtRInt, sizeof(int),
	    Offset(wm.size_hints.height_inc),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNminAspectX, XtCMinAspectX, XtRInt, sizeof(int),
	    Offset(wm.size_hints.min_aspect.x),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNminAspectY, XtCMinAspectY, XtRInt, sizeof(int),
	    Offset(wm.size_hints.min_aspect.y),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNmaxAspectX, XtCMaxAspectX, XtRInt, sizeof(int),
	    Offset(wm.size_hints.max_aspect.x),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNmaxAspectY, XtCMaxAspectY, XtRInt, sizeof(int),
	    Offset(wm.size_hints.max_aspect.y),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
/* wm_hints */
	{ XtNinput, XtCInput, XtRBool, sizeof(Bool),
	    Offset(wm.wm_hints.input), XtRImmediate, (XtPointer)False},
	{ XtNinitialState, XtCInitialState, XtRInitialState, sizeof(int),
	    Offset(wm.wm_hints.initial_state),
	    XtRImmediate, (XtPointer)NormalState},
	{ XtNiconPixmap, XtCIconPixmap, XtRBitmap, sizeof(Pixmap),
	    Offset(wm.wm_hints.icon_pixmap), XtRPixmap, NULL},
	{ XtNiconWindow, XtCIconWindow, XtRWindow, sizeof(Window),
	    Offset(wm.wm_hints.icon_window), XtRWindow,   (XtPointer) NULL},
	{ XtNiconX, XtCIconX, XtRInt, sizeof(int),
	    Offset(wm.wm_hints.icon_x),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNiconY, XtCIconY, XtRInt, sizeof(int),
	    Offset(wm.wm_hints.icon_y),
	    XtRInt, (XtPointer) &default_unspecified_shell_int},
	{ XtNiconMask, XtCIconMask, XtRBitmap, sizeof(Pixmap),
	    Offset(wm.wm_hints.icon_mask), XtRPixmap, NULL},
	{ XtNwindowGroup, XtCWindowGroup, XtRWindow, sizeof(Window),
	    Offset(wm.wm_hints.window_group),
	    XtRImmediate, (XtPointer)XtUnspecifiedWindow}
};

static void WMInitialize();
static Boolean WMSetValues();
static void WMDestroy();

externaldef(wmshellclassrec) WMShellClassRec wmShellClassRec = {
  {
    /* superclass         */    (WidgetClass) &shellClassRec,
    /* class_name         */    "WMShell",
    /* size               */    sizeof(WMShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL,
    /* Class init'ed ?    */	FALSE,
    /* initialize         */    WMInitialize,
    /* initialize_notify    */	NULL,		
    /* realize            */    XtInheritRealize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    wmResources,
    /* resource_count     */	XtNumber(wmResources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/	FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    WMDestroy,
    /* resize             */    XtInheritResize,
    /* expose             */    NULL,
    /* set_values         */    WMSetValues,
    /* set_values_hook      */	NULL,			
    /* set_values_almost    */	XtInheritSetValuesAlmost,  
    /* get_values_hook      */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		    */  NULL,
    /* query_geometry	    */  NULL,
    /* display_accelerator  */  NULL,
    /* extension	    */  NULL
  },{
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	    */  NULL
  },{
    /* extension	    */  NULL
  },{
    /* extension	    */  NULL
  }
};

externaldef(wmshellwidgetclass) WidgetClass wmShellWidgetClass = (WidgetClass) (&wmShellClassRec);

/***************************************************************************
 *
 * TransientShell class record
 *
 ***************************************************************************/

#undef Offset
#define Offset(x)	(XtOffsetOf(TransientShellRec, x))

static XtResource transientResources[]=
{
	{ XtNtransient, XtCTransient, XtRBoolean, sizeof(Boolean),
	    Offset(wm.transient), XtRImmediate, (XtPointer)True},
	{ XtNtransientFor, XtCTransientFor, XtRWidget, sizeof(Widget),
	    Offset(transient.transient_for), XtRWidget, NULL},
	{ XtNsaveUnder, XtCSaveUnder, XtRBoolean, sizeof(Boolean),
	    Offset(shell.save_under), XtRImmediate, (XtPointer)True},
};

static void TransientRealize();
static Boolean TransientSetValues();

externaldef(transientshellclassrec) TransientShellClassRec transientShellClassRec = {
  {
    /* superclass	  */	(WidgetClass) &vendorShellClassRec,
    /* class_name	  */	"TransientShell",
    /* size		  */	sizeof(TransientShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL,
    /* Class init'ed ?	  */	FALSE,
    /* initialize	  */	NULL,
    /* initialize_notify  */	NULL,		
    /* realize		  */	TransientRealize,
    /* actions		  */	NULL,
    /* num_actions	  */	0,
    /* resources	  */	transientResources,
    /* resource_count	  */	XtNumber(transientResources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	FALSE,
    /* compress_exposure  */	TRUE,
    /* compress_enterleave*/	FALSE,
    /* visible_interest	  */	FALSE,
    /* destroy		  */	NULL,
    /* resize		  */	XtInheritResize,
    /* expose		  */	NULL,
    /* set_values	  */	TransientSetValues,
    /* set_values_hook	  */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook	  */	NULL,			
    /* accept_focus	  */	NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets	  */	NULL,
    /* tm_table		  */	XtInheritTranslations,
    /* query_geometry	  */	NULL,
    /* display_accelerator*/	NULL,
    /* extension	  */	NULL
  },{
    /* geometry_manager	  */	XtInheritGeometryManager,
    /* change_managed	  */	XtInheritChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	NULL
  },{
    /* extension	  */	NULL
  },{
    /* extension	  */	NULL
  },{
    /* extension	  */	NULL
  },{
    /* extension	  */	NULL
  }
};

externaldef(transientshellwidgetclass) WidgetClass transientShellWidgetClass =
	(WidgetClass) (&transientShellClassRec);

/***************************************************************************
 *
 * TopLevelShell class record
 *
 ***************************************************************************/

#undef Offset
#define Offset(x)	(XtOffsetOf(TopLevelShellRec, x))

static XtResource topLevelResources[]=
{
	{ XtNiconName, XtCIconName, XtRString, sizeof(String),
	    Offset(topLevel.icon_name), XtRString, (XtPointer) NULL},
	{ XtNiconNameEncoding, XtCIconNameEncoding, XtRAtom, sizeof(Atom),
	    Offset(topLevel.icon_name_encoding),
	    XtRCallProc, (XtPointer) _XtTitleEncoding},
	{ XtNiconic, XtCIconic, XtRBoolean, sizeof(Boolean),
	    Offset(topLevel.iconic), XtRImmediate, (XtPointer)False}
};

static void TopLevelInitialize();
static Boolean TopLevelSetValues();
static void TopLevelDestroy();

externaldef(toplevelshellclassrec) TopLevelShellClassRec topLevelShellClassRec = {
  {
    /* superclass         */    (WidgetClass) &vendorShellClassRec,
    /* class_name         */    "TopLevelShell",
    /* size               */    sizeof(TopLevelShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL,
    /* Class init'ed ?    */	FALSE,
    /* initialize         */    TopLevelInitialize,
    /* initialize_notify    */	NULL,		
    /* realize            */    XtInheritRealize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    topLevelResources,
    /* resource_count     */	XtNumber(topLevelResources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/ 	FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    TopLevelDestroy,
    /* resize             */    XtInheritResize,
    /* expose             */    NULL,
    /* set_values         */    TopLevelSetValues,
    /* set_values_hook      */	NULL,			
    /* set_values_almost    */	XtInheritSetValuesAlmost,  
    /* get_values_hook      */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		    */  XtInheritTranslations,
    /* query_geometry	    */  NULL,
    /* display_accelerator  */  NULL,
    /* extension	    */  NULL
  },{
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	    */  NULL
  },{
    /* extension	    */  NULL
  },{
    /* extension	    */  NULL
  },{
    /* extension	    */  NULL
  },{
    /* extension	    */  NULL
  }
};

externaldef(toplevelshellwidgetclass) WidgetClass topLevelShellWidgetClass =
	(WidgetClass) (&topLevelShellClassRec);

/***************************************************************************
 *
 * ApplicationShell class record
 *
 ***************************************************************************/

#undef Offset
#define Offset(x)	(XtOffsetOf(ApplicationShellRec, x))

static XtResource applicationResources[]=
{
	{ XtNargc, XtCArgc, XtRInt, sizeof(int),
	    Offset(application.argc), XtRImmediate, (XtPointer)0}, 
	{ XtNargv, XtCArgv, XtRStringArray, sizeof(String*),
	    Offset(application.argv), XtRPointer, (XtPointer) NULL}
};

static void ApplicationInitialize();
static void ApplicationDestroy();
static void ApplicationShellInsertChild();

static CompositeClassExtensionRec compositeClassExtension = {
    /* next_extension	*/	NULL,
    /* record_type	*/	NULLQUARK,
    /* version		*/	XtCompositeExtensionVersion,
    /* record_size	*/	sizeof(CompositeClassExtensionRec),
    /* accepts_objects	*/	TRUE
};


externaldef(applicationshellclassrec) ApplicationShellClassRec applicationShellClassRec = {
  {
    /* superclass         */    (WidgetClass) &topLevelShellClassRec,
    /* class_name         */    "ApplicationShell",
    /* size               */    sizeof(ApplicationShellRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL,
    /* Class init'ed ?    */	FALSE,
    /* initialize         */    ApplicationInitialize,
    /* initialize_notify  */	NULL,		
    /* realize            */    XtInheritRealize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    applicationResources,
    /* resource_count     */	XtNumber(applicationResources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/    FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    ApplicationDestroy,
    /* resize             */    XtInheritResize,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook    */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,
    /* get_values_hook    */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		  */	XtInheritTranslations,
    /* query_geometry	  */	NULL,
    /* display_accelerator*/	NULL,
    /* extension	  */	NULL
  },{
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    XtInheritChangeManaged,
    /* insert_child	  */	ApplicationShellInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	(XtPointer)&compositeClassExtension
  },{
    /* extension	  */	NULL
  },{
    /* extension	  */	NULL
  },{
    /* extension	  */	NULL
  },{
    /* extension	  */	NULL
  },{
    /* extension	  */	NULL
  }
};

externaldef(applicationshellwidgetclass) WidgetClass applicationShellWidgetClass =
	(WidgetClass) (&applicationShellClassRec);

/****************************************************************************
 * Whew!
 ****************************************************************************/

static void ComputeWMSizeHints(w, hints)
    WMShellWidget w;
    XSizeHints *hints;
{
    register long flags;
    hints->flags = flags = w->wm.size_hints.flags;
#define copy(field) hints->field = w->wm.size_hints.field
    if (flags & (USPosition | PPosition)) {
	copy(x);
	copy(y);
    }
    if (flags & (USSize | PSize)) {
	copy(width);
	copy(height);
    }
    if (flags & PMinSize) {
	copy(min_width);
	copy(min_height);
    }
    if (flags & PMaxSize) {
	copy(max_width);
	copy(max_height);
    }
    if (flags & PResizeInc) {
	copy(width_inc);
	copy(height_inc);
    }
    if (flags & PAspect) {
	copy(min_aspect.x);
	copy(min_aspect.y);
	copy(max_aspect.x);
	copy(max_aspect.y);
    }
#undef copy
#define copy(field) hints->field = w->wm.field
    if (flags & PBaseSize) {
	copy(base_width);
	copy(base_height);
    }
    if (flags & PWinGravity)
	copy(win_gravity);
#undef copy
}

static void _SetWMSizeHints(w)
    WMShellWidget w;
{
    XSizeHints *size_hints = XAllocSizeHints();

    if (size_hints == NULL) _XtAllocError("XAllocSizeHints");
    ComputeWMSizeHints(w, size_hints);
    XSetWMNormalHints(XtDisplay((Widget)w), XtWindow((Widget)w), size_hints);
    XFree((char*)size_hints);
}

static ShellClassExtension _FindClassExtension(widget_class)
    WidgetClass widget_class;
{
    ShellClassExtension ext;
    for (ext = (ShellClassExtension)((ShellWidgetClass)widget_class)
	       ->shell_class.extension;
	 ext != NULL && ext->record_type != NULLQUARK;
	 ext = (ShellClassExtension)ext->next_extension);

    if (ext != NULL) {
	if (  ext->version == XtShellExtensionVersion
	      && ext->record_size == sizeof(ShellClassExtensionRec)) {
	    /* continue */
	} else {
	    String params[1];
	    Cardinal num_params = 1;
	    params[0] = widget_class->core_class.class_name;
	    XtErrorMsg( "invalidExtension", "shellClassPartInitialize",
		        XtCXtToolkitError,
		 "widget class %s has invalid ShellClassExtension record",
		 params, &num_params);
	}
    }
    return ext;
}

static void ClassPartInitialize(widget_class)
    WidgetClass widget_class;
{
    ShellClassExtension ext = _FindClassExtension(widget_class);
    if (ext != NULL) {
	if (ext->root_geometry_manager == XtInheritRootGeometryManager) {
	    ext->root_geometry_manager =
		_FindClassExtension(widget_class->core_class.superclass)
		    ->root_geometry_manager;
	}
    } else {
	/* if not found, spec requires XtInheritRootGeometryManager */
	XtPointer *extP
	    = &((ShellWidgetClass)widget_class)->shell_class.extension;
	ext = XtNew(ShellClassExtensionRec);
	bcopy((char*)_FindClassExtension(widget_class->core_class.superclass),
	      (char*)ext,
	      sizeof(ShellClassExtensionRec));
	ext->next_extension = *extP;
	*extP = (XtPointer)ext;
    }
}


static void EventHandler();
static void _popup_set_prop();


/*ARGSUSED*/
static void XtCopyDefaultDepth(widget, offset, value)
    Widget      widget;
    int		offset;
    XrmValue    *value;
{
    value->addr = (XPointer)(&DefaultDepthOfScreen(XtScreenOfObject(widget)));
}

#ifndef CRAY
static
#endif
void _XtShellDepth(widget,closure,value)
    Widget widget;
    int closure;
    XrmValue *value;
{
   if (widget->core.parent == NULL) XtCopyDefaultDepth(widget,closure,value);
   else _XtCopyFromParent (widget,closure,value);
}

/*ARGSUSED*/
static void XtCopyDefaultColormap(widget, offset, value)
    Widget      widget;
    int		offset;
    XrmValue    *value;
{
    value->addr = (XPointer)(&DefaultColormapOfScreen(XtScreenOfObject(widget)));
}

#ifndef CRAY
static
#endif
void _XtShellColormap(widget,closure,value)
    Widget widget;
    int closure;
    XrmValue *value;
{
   if (widget->core.parent == NULL)
	   XtCopyDefaultColormap(widget,closure,value);
   else _XtCopyFromParent (widget,closure,value);
}

#ifndef CRAY
static
#endif
void _XtShellAncestorSensitive(widget,closure,value)
    Widget widget;
    int closure;
    XrmValue *value;
{
   static Boolean true = True;
   if (widget->core.parent == NULL) value->addr = (XPointer)(&true);
   else _XtCopyFromParent (widget,closure,value);
}

/*ARGSUSED*/
#ifndef CRAY
static
#endif
void _XtTitleEncoding(widget, offset, value)
    Widget widget;
    int offset;
    XrmValue *value;
{
    static Atom atom;
    if (XtWidgetToApplicationContext(widget)->langProcRec.proc) atom = None;
    else atom = XA_STRING;
    value->addr = (XPointer) &atom;
}


/* ARGSUSED */
static void Initialize(req, new, args, num_args)
	Widget req, new;
	ArgList args;		/* unused */
	Cardinal *num_args;	/* unused */
{
	ShellWidget w = (ShellWidget) new;

	w->shell.popped_up = FALSE;
	w->shell.client_specified =
	    _XtShellNotReparented | _XtShellPositionValid;

	if (w->core.x == BIGSIZE) {
	    w->core.x = 0;
	    if (w->core.y == BIGSIZE) w->core.y = 0;
	} else {
	    if (w->core.y == BIGSIZE) w->core.y = 0;
	    else w->shell.client_specified |= _XtShellPPositionOK;
	}

	XtAddEventHandler(new, (EventMask) StructureNotifyMask,
		TRUE, EventHandler, (XtPointer) NULL);
}

/* ARGSUSED */
static void WMInitialize(req, new, args, num_args)
	Widget req,new;
	ArgList args;		/* unused */
	Cardinal *num_args;	/* unused */
{
	WMShellWidget w = (WMShellWidget) new;
	TopLevelShellWidget tls = (TopLevelShellWidget) new;	/* maybe */

	if(w->wm.title == NULL) {
	    if (XtIsTopLevelShell(new) &&
		    tls->topLevel.icon_name != NULL &&
		    strlen(tls->topLevel.icon_name) != 0) {
		w->wm.title = XtNewString(tls->topLevel.icon_name);
	    } else {
		w->wm.title = XtNewString(w->core.name);
	    }
	} else {
	    w->wm.title = XtNewString(w->wm.title);
	}
	w->wm.size_hints.flags = 0;
	w->wm.wm_hints.flags = 0;

	/* Find the values of the atoms, somewhere... */

	for (new = new->core.parent;
		new != NULL && !XtIsWMShell(new);
		new = new->core.parent) {}
	if (new == NULL) {
	    w->wm.wm_configure_denied =
		    XInternAtom(XtDisplay(w), "WM_CONFIGURE_DENIED", FALSE);
	    w->wm.wm_moved = XInternAtom(XtDisplay(w), "WM_MOVED", FALSE);
	} else {
	    w->wm.wm_configure_denied = WM_CONFIGURE_DENIED(new);
	    w->wm.wm_moved = WM_MOVED(new);
	}
}


/* ARGSUSED */
static void TopLevelInitialize(req, new, args, num_args)
	Widget req, new;
	ArgList args;		/* unused */
	Cardinal *num_args;	/* unused */
{
	TopLevelShellWidget w = (TopLevelShellWidget) new;

	if (w->topLevel.icon_name == NULL) {
	    w->topLevel.icon_name = XtNewString(w->core.name);
	} else {
	    w->topLevel.icon_name = XtNewString(w->topLevel.icon_name);
	}

	if (w->topLevel.iconic)
	    w->wm.wm_hints.initial_state = IconicState;
}

/* ARGSUSED */
static void ApplicationInitialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;		/* unused */
    Cardinal *num_args;		/* unused */
{
    ApplicationShellWidget w = (ApplicationShellWidget)new;
    /* copy the argv if passed */
    if (w->application.argc > 0) {
	int i = w->application.argc;
	char **argv = (char**)XtMalloc( (unsigned)i*sizeof(char*) );
	char **argp = w->application.argv + i;
	while (--i >= 0) {
	    argv[i] = *--argp;
	}
	w->application.argv = argv;
    }
}

static void Resize(w)
    Widget w;
{
    register ShellWidget sw = (ShellWidget)w;    
    Widget childwid;
    int i;
    for(i = 0; i < sw->composite.num_children; i++) {
        if (XtIsManaged(sw->composite.children[i])) {
             childwid = sw->composite.children[i];
             XtResizeWidget(childwid, sw->core.width, sw->core.height,
                           childwid->core.border_width);
	     break;		/* can only be one managed child */
        }
    }
}

static void GetGeometry();

static void Realize(wid, vmask, attr)
	Widget wid;
	Mask *vmask;
	XSetWindowAttributes *attr;
{
	ShellWidget w = (ShellWidget) wid;
        Mask mask = *vmask;

	if (! (w->shell.client_specified & _XtShellGeometryParsed)) {
	    /* we'll get here only if there was no child the first
	       time we were realized.  If the shell was Unrealized
	       and then re-Realized, we probably don't want to
	       re-evaluate the defaults anyway.
	     */
	    GetGeometry(wid, (Widget)NULL);
	}
	else if (w->core.background_pixmap == XtUnspecifiedPixmap) {
	    /* I attempt to inherit my child's background to avoid screen flash
	     * if there is latency between when I get resized and when my child
	     * is resized.  Background=None is not satisfactory, as I want the
	     * user to get immediate feedback on the new dimensions (most
	     * particularly in the case of a non-reparenting wm).  It is
	     * especially important to have the server clear any old cruft
	     * from the display when I am resized larger.
	     */
	    register Widget *childP = w->composite.children;
	    int i;
	    for (i = w->composite.num_children; i; i--, childP++) {
		if (XtIsWidget(*childP) && XtIsManaged(*childP)) {
		    if ((*childP)->core.background_pixmap
			    != XtUnspecifiedPixmap) {
			mask &= ~(CWBackPixel);
			mask |= CWBackPixmap;
			attr->background_pixmap =
			    w->core.background_pixmap =
				(*childP)->core.background_pixmap;
		    } else {
			attr->background_pixel = 
			    w->core.background_pixel = 
				(*childP)->core.background_pixel;
		    }
		    break;
		}
	    }
	}

	if(w->shell.save_under) {
		mask |= CWSaveUnder;
		attr->save_under = TRUE;
	}
	if(w->shell.override_redirect) {
		mask |= CWOverrideRedirect;
		attr->override_redirect = TRUE;
	}
	if (wid->core.width == 0 || wid->core.height == 0) {
	    Cardinal count = 1;
	    XtErrorMsg("invalidDimension", "shellRealize", XtCXtToolkitError,
		       "Shell widget %s has zero width and/or height",
		       &wid->core.name, &count);
	}
	wid->core.window = XCreateWindow(XtDisplay(wid),
		wid->core.screen->root, (int)wid->core.x, (int)wid->core.y,
		(unsigned int)wid->core.width, (unsigned int)wid->core.height,
		(unsigned int)wid->core.border_width, (int) wid->core.depth,
		(unsigned int) InputOutput, w->shell.visual,
		mask, attr);

	_popup_set_prop(w);
}


static void _SetTransientForHint(w, delete)
     TransientShellWidget w;
     Boolean delete;
{
    Window window_group;

    if (w->wm.transient) {
	if (w->transient.transient_for != NULL
	    && XtIsRealized(w->transient.transient_for))
	    window_group = XtWindow(w->transient.transient_for);
	else if ((window_group = w->wm.wm_hints.window_group)
		 == XtUnspecifiedWindowGroup) {
	    if (delete)
		XDeleteProperty( XtDisplay((Widget)w),
				 XtWindow((Widget)w),
				 XA_WM_TRANSIENT_FOR
				);
	    return;
	}

	XSetTransientForHint( XtDisplay((Widget)w),
			      XtWindow((Widget)w),
			      window_group
			     );
    }
}


static void TransientRealize(w, vmask, attr)
     Widget w;
     Mask *vmask;
     XSetWindowAttributes *attr;
{
    (*transientShellWidgetClass->core_class.superclass->
     core_class.realize) (w, vmask, attr);

    _SetTransientForHint((TransientShellWidget)w, False);
}


static void EvaluateWMHints(w)
    WMShellWidget w;
{
	XWMHints *hintp = &w->wm.wm_hints;

	hintp->flags = StateHint | InputHint;

	if (hintp->icon_x == XtUnspecifiedShellInt)
	    hintp->icon_x = -1;
	else
	    hintp->flags |= IconPositionHint;

	if (hintp->icon_y == XtUnspecifiedShellInt)
	    hintp->icon_y = -1;
	else
	    hintp->flags |= IconPositionHint;

	if (hintp->icon_pixmap != None) hintp->flags |= IconPixmapHint;
	if (hintp->icon_mask != None)   hintp->flags |= IconMaskHint;
	if (hintp->icon_window != None) hintp->flags |= IconWindowHint;

	if (hintp->window_group == XtUnspecifiedWindow) {
	    if(w->core.parent) {
		Widget p;
		for (p = w->core.parent; p->core.parent; p = p->core.parent);
		if (XtIsRealized(p)) {
		    hintp->window_group = XtWindow(p);
		    hintp->flags |=  WindowGroupHint;
		}
	    }
	} else if (hintp->window_group != XtUnspecifiedWindowGroup)
	    hintp->flags |=  WindowGroupHint;
}


static void EvaluateSizeHints(w)
    WMShellWidget w;
{
	struct _OldXSizeHints *sizep = &w->wm.size_hints;

	sizep->x = w->core.x;
	sizep->y = w->core.y;
	sizep->width = w->core.width;
	sizep->height = w->core.height;

	if (sizep->flags & USSize) {
	    if (sizep->flags & PSize) sizep->flags &= ~PSize;
	} else
	    sizep->flags |= PSize;

	if (sizep->flags & USPosition) {
	    if (sizep->flags & PPosition) sizep->flags &= ~PPosition;
	} else if (w->shell.client_specified & _XtShellPPositionOK)
	    sizep->flags |= PPosition;

	if (sizep->min_aspect.x != XtUnspecifiedShellInt
	    || sizep->min_aspect.y != XtUnspecifiedShellInt
	    || sizep->max_aspect.x != XtUnspecifiedShellInt
	    || sizep->max_aspect.y != XtUnspecifiedShellInt) {
	    sizep->flags |= PAspect;
	}
	if (sizep->flags & PBaseSize
	    || w->wm.base_width != XtUnspecifiedShellInt
	    || w->wm.base_height != XtUnspecifiedShellInt) {
	    sizep->flags |= PBaseSize;
	    if (w->wm.base_width == XtUnspecifiedShellInt)
		w->wm.base_width = 0;
	    if (w->wm.base_height == XtUnspecifiedShellInt)
		w->wm.base_height = 0;
	}
	if (sizep->flags & PResizeInc
	    || sizep->width_inc != XtUnspecifiedShellInt
	    || sizep->height_inc != XtUnspecifiedShellInt) {
	    if (sizep->width_inc < 1) sizep->width_inc = 1;
	    if (sizep->height_inc < 1) sizep->height_inc = 1;
	    sizep->flags |= PResizeInc;
	}
	if (sizep->flags & PMaxSize
	    || sizep->max_width != XtUnspecifiedShellInt
	    || sizep->max_height != XtUnspecifiedShellInt) {
	    sizep->flags |= PMaxSize;
	    if (sizep->max_width == XtUnspecifiedShellInt)
		sizep->max_width = BIGSIZE;
	    if (sizep->max_height == XtUnspecifiedShellInt)
		sizep->max_height = BIGSIZE;
	}
	if (sizep->flags & PMinSize
	    || sizep->min_width != XtUnspecifiedShellInt
	    || sizep->min_height != XtUnspecifiedShellInt) {
	    sizep->flags |= PMinSize;
	    if (sizep->min_width == XtUnspecifiedShellInt)
		sizep->min_width = 1;
	    if (sizep->min_height == XtUnspecifiedShellInt)
		sizep->min_height = 1;
	}
}

static void _popup_set_prop(w)
	ShellWidget w;
{
	Widget p;
	WMShellWidget wmshell = (WMShellWidget) w;
	TopLevelShellWidget tlshell = (TopLevelShellWidget) w;
	ApplicationShellWidget appshell = (ApplicationShellWidget) w;
	XTextProperty icon_name;
	XTextProperty window_name;
	char **argv;
	int argc;
	XSizeHints *size_hints;
	Window window_group;
	XClassHint classhint;
	Boolean copied_iname, copied_wname;

	if (!XtIsWMShell((Widget)w) || w->shell.override_redirect) return;

	if ((size_hints = XAllocSizeHints()) == NULL)
	    _XtAllocError("XAllocSizeHints");

	copied_iname = copied_wname = False;
        if (wmshell->wm.title_encoding == None &&
	    XmbTextListToTextProperty(XtDisplay((Widget)w),
				      (char**)&wmshell->wm.title,
				      1, XStdICCTextStyle,
				      &window_name) >= Success) {
	    copied_wname = True;
	} else {
	    window_name.value = (unsigned char*)wmshell->wm.title;
	    window_name.encoding = wmshell->wm.title_encoding;
	    window_name.format = 8;
	    window_name.nitems = strlen((char *)window_name.value);
	}

	if (XtIsTopLevelShell((Widget)w)) {
            if (tlshell->topLevel.icon_name_encoding == None &&
		XmbTextListToTextProperty(XtDisplay((Widget)w),
					  (char**)&tlshell->topLevel.icon_name,
					  1, XStdICCTextStyle,
					  &icon_name) >= Success) {
		copied_iname = True;
	    } else {
		icon_name.value = (unsigned char*)tlshell->topLevel.icon_name;
		icon_name.encoding = tlshell->topLevel.icon_name_encoding;
		icon_name.format = 8;
		icon_name.nitems = strlen((char *)icon_name.value);
	    }
	}

	EvaluateWMHints(wmshell);
	EvaluateSizeHints(wmshell);
	ComputeWMSizeHints(wmshell, size_hints);

	if (wmshell->wm.transient
	    && !XtIsTransientShell((Widget)w)
	    && (window_group = wmshell->wm.wm_hints.window_group)
	       != XtUnspecifiedWindowGroup) {

	    XSetTransientForHint(XtDisplay((Widget)w),
				 XtWindow((Widget)w),
				 window_group
				 );
	}

	classhint.res_name = w->core.name;
	/* For the class, look up to the top of the tree */
	for (p = (Widget)w; p->core.parent != NULL; p = p->core.parent);
	if (XtIsApplicationShell(p)) {
	    classhint.res_class =
		((ApplicationShellWidget)p)->application.class;
	} else classhint.res_class = XtClass(p)->core_class.class_name;

	if (XtIsApplicationShell((Widget)w)
	    && (argc = appshell->application.argc) != -1)
	    argv = (char**)appshell->application.argv;
	else {
	    argv = NULL;
	    argc = 0;
	}

	XSetWMProperties(XtDisplay((Widget)w), XtWindow((Widget)w),
			 &window_name,
			 (XtIsTopLevelShell((Widget)w)) ? &icon_name : NULL,
			 argv, argc,
			 size_hints,
			 &wmshell->wm.wm_hints,
			 &classhint);
	XFree((char*)size_hints);
	if (copied_wname)
	    XFree((XPointer)window_name.value);
	if (copied_iname)
	    XFree((XPointer)icon_name.value);

	if (XtWidgetToApplicationContext((Widget)w)->langProcRec.proc) {
	    char *locale = setlocale(LC_CTYPE, (char *)NULL);
	    if (locale)
		XChangeProperty(XtDisplay((Widget)w), XtWindow((Widget)w),
				XInternAtom(XtDisplay((Widget)w),
					    "WM_LOCALE_NAME", False),
				XA_STRING, 8, PropModeReplace,
				(unsigned char *)locale, strlen(locale));
	}
}

/* ARGSUSED */
static void EventHandler(wid, closure, event, continue_to_dispatch)
	Widget wid;
	XtPointer closure;	/* unused */
	XEvent *event;
        Boolean *continue_to_dispatch; /* unused */
{
	register ShellWidget w = (ShellWidget) wid;
	WMShellWidget wmshell = (WMShellWidget) w;
	Boolean  sizechanged = FALSE;
	unsigned int width, height, border_width, tmpdepth;
	int tmpx, tmpy, tmp2x, tmp2y;
	Window tmproot, tmpchild;

	if(w->core.window != event->xany.window) {
		XtAppErrorMsg(XtWidgetToApplicationContext(wid),
			"invalidWindow","eventHandler",XtCXtToolkitError,
                        "Event with wrong window",
			(String *)NULL, (Cardinal *)NULL);
		return;
	}

	switch(event->type) {
	    case ConfigureNotify:
	        if (w->core.window != event->xconfigure.window)
		    return;  /* in case of SubstructureNotify */
#define NEQ(x)	( w->core.x != event->xconfigure.x )
		if( NEQ(width) || NEQ(height) || NEQ(border_width) ) {
			sizechanged = TRUE;
#undef NEQ
			w->core.width = event->xconfigure.width;
			w->core.height = event->xconfigure.height;
			w->core.border_width = event->xconfigure.border_width;
		}
		if (event->xany.send_event /* ICCCM compliant synthetic ev */
		    /* || w->shell.override_redirect */
		    || w->shell.client_specified & _XtShellNotReparented)
	        {
		    w->core.x = event->xconfigure.x;
		    w->core.y = event->xconfigure.y;
		    w->shell.client_specified |= _XtShellPositionValid;
		}
		else w->shell.client_specified &= ~_XtShellPositionValid;
		if (XtIsWMShell(wid) && !wmshell->wm.wait_for_wm) {
		    /* Consider trusting the wm again */
		    register struct _OldXSizeHints *hintp
			= &wmshell->wm.size_hints;
#define EQ(x) (hintp->x == w->core.x)
		    if (EQ(x) && EQ(y) && EQ(width) && EQ(height)) {
			wmshell->wm.wait_for_wm = TRUE;
		    }
#undef EQ
		}		    
		break;

	    case ClientMessage:
		if( event->xclient.message_type == WM_CONFIGURE_DENIED(wid)
		    && XtIsWMShell(wid)) {

		    /* 
		     * UT Oh! the window manager has come back alive
		     * This means either I didn't wait long enough or
		     * The WM is sick.
		     * Query my real size and position, and adjust my child
		     * it needs be.
		     */

		    if(wmshell->wm.wait_for_wm) {
			XtAppWarningMsg(XtWidgetToApplicationContext(wid),
				"communicationError","windowManager",
                                  XtCXtToolkitError,
                                  "Window Manager is confused",
				  (String *)NULL, (Cardinal *)NULL);
		    }
		    wmshell->wm.wait_for_wm = TRUE;
		    (void) XGetGeometry(XtDisplay(w), XtWindow(w), &tmproot,
			    &tmpx, &tmpy, &width, &height, &border_width,
			    &tmpdepth);
		    (void) XTranslateCoordinates(XtDisplay(w), XtWindow(w), 
			    tmproot, (int) tmpx, (int) tmpy,
			    &tmp2x, &tmp2y, &tmpchild);
		    w->core.x = tmp2x;
		    w->core.y = tmp2y;
		    if( width != w->core.width || height != w->core.height
		       || border_width != w->core.border_width ) {
			    w->core.width = width;
			    w->core.height = height;
			    w->core.border_width = border_width;
			    sizechanged = TRUE;
		    }

		    break;
		}
		if(event->xclient.message_type == WM_MOVED(wid)) {
		    w->core.x = event->xclient.data.s[0];
		    w->core.y  = event->xclient.data.s[1];
		    if (XtIsWMShell((Widget)w)) {
			WMShellWidget wmshell = (WMShellWidget) w;
			/* Any window manager which sends this must be 
			   good guy.  Let's reset our flag. */
			wmshell->wm.wait_for_wm = TRUE;
		    }
		}
		break;

	      case ReparentNotify:
		if (event->xreparent.window == XtWindow(w)) {
		   if (event->xreparent.parent != RootWindowOfScreen(XtScreen(w)))
		       w->shell.client_specified &= ~_XtShellNotReparented;
		   else
		       w->shell.client_specified |= _XtShellNotReparented;
		   w->shell.client_specified &= ~_XtShellPositionValid;
	        }
		return;

	      case UnmapNotify:
		{
		    XtPerDisplayInput	pdi;
		    XtDevice		device;
		    Widget		p;

		    pdi = _XtGetPerDisplayInput(event->xunmap.display);

		    device = &pdi->pointer;
		    if (device->grabType == XtPassiveServerGrab) {
			p = device->grab.widget;
			while (p && !(XtIsShell(p)))
			    p = p->core.parent;
			if (p == wid)
			    device->grabType = XtNoServerGrab;
		    }

		    device = &pdi->keyboard;
		    if (IsEitherPassiveGrab(device->grabType)) {
			p = device->grab.widget;
			while (p && !(XtIsShell(p)))
			    p = p->core.parent;
			if (p == wid) {
			    device->grabType = XtNoServerGrab;
			    pdi->activatingKey = 0;
			}
		    }

		    return;
		}
	      default:
		 return;
	 } 

	 if (sizechanged && 
                 XtClass(wid)->core_class.resize != (XtWidgetProc) NULL)
                    (*(XtClass(wid)->core_class.resize))(wid);

}

static void Destroy(wid)
	Widget wid;
{
	if (XtIsRealized(wid))
	    XDestroyWindow( XtDisplay(wid), XtWindow(wid) );
}

static void WMDestroy(wid)
	Widget wid;
{
	WMShellWidget w = (WMShellWidget) wid;

	XtFree((char *) w->wm.title);
}

static void TopLevelDestroy(wid)
	Widget wid;
{
	TopLevelShellWidget w = (TopLevelShellWidget) wid;

	XtFree((char *) w->topLevel.icon_name);
}

static void ApplicationDestroy(wid)
	Widget wid;
{
	ApplicationShellWidget w = (ApplicationShellWidget) wid;

	if(w->application.argv != NULL) XtFree((char *) w->application.argv);
	w->application.argv = NULL;
}

/*
 * If the Shell has a width and a height which are zero, and as such
 * suspect, and it has not yet been realized then it will grow to
 * match the child before parsing the geometry resource.
 *
 */
static void GetGeometry(W, child)
    Widget W, child;
{
    register ShellWidget w = (ShellWidget)W;
    Boolean is_wmshell = XtIsWMShell(W);
    int x, y, width, height, win_gravity = -1, flag;
    XSizeHints hints;

    if (child != NULL) {
	/* we default to our child's size */
	if (is_wmshell && (w->core.width == 0 || w->core.height == 0))
	    ((WMShellWidget)W)->wm.size_hints.flags |= PSize;
	if (w->core.width == 0)	    w->core.width = child->core.width;
	if (w->core.height == 0)    w->core.height = child->core.height;
    }
    if(w->shell.geometry != NULL) {
	char def_geom[64];
	x = w->core.x;
	y = w->core.y;
	width = w->core.width;
	height = w->core.height;
	if (is_wmshell) {
	    WMShellPart* wm = &((WMShellWidget)w)->wm;
	    EvaluateSizeHints((WMShellWidget)w);
	    bcopy((char*)&wm->size_hints, (char*)&hints,
		  sizeof(struct _OldXSizeHints));
	    hints.win_gravity = wm->win_gravity;
	    if (wm->size_hints.flags & PBaseSize) {
		width -= wm->base_width;
		height -= wm->base_height;
		hints.base_width = wm->base_width;
		hints.base_height = wm->base_height;
	    }
	    else if (wm->size_hints.flags & PMinSize) {
		width -= wm->size_hints.min_width;
		height -= wm->size_hints.min_height;
	    }
	    if (wm->size_hints.flags & PResizeInc) {
		width /= wm->size_hints.width_inc;
		height /= wm->size_hints.height_inc;
	    }
	}
	else hints.flags = 0;

	sprintf( def_geom, "%dx%d+%d+%d", width, height, x, y );
	flag = XWMGeometry( XtDisplay(W),
			    XScreenNumberOfScreen(XtScreen(W)),
			    w->shell.geometry, def_geom,
			    (unsigned int)w->core.border_width,
			    &hints, &x, &y, &width, &height,
			    &win_gravity
			   );
	if (flag) {
	    if (flag & XValue) w->core.x = (Position)x;
	    if (flag & YValue) w->core.y = (Position)y;
	    if (flag & WidthValue) w->core.width = (Dimension)width;
	    if (flag & HeightValue) w->core.height = (Dimension)height;
	}
	else {
	    String params[2];
	    Cardinal num_params = 2;
	    params[0] = XtName(W);
	    params[1] = w->shell.geometry;
	    XtAppWarningMsg(XtWidgetToApplicationContext(W),
       "badGeometry", "shellRealize", XtCXtToolkitError,
       "Shell widget \"%s\" has an invalid geometry specification: \"%s\"",
			    params, &num_params);
	}
    }
    else
	flag = 0;

    if (is_wmshell) {
	WMShellWidget wmshell = (WMShellWidget) w;
	if (wmshell->wm.win_gravity == XtUnspecifiedShellInt) {
	    if (win_gravity != -1)
		wmshell->wm.win_gravity = win_gravity;
	    else
		wmshell->wm.win_gravity = NorthWestGravity;
	}
	wmshell->wm.size_hints.flags |= PWinGravity;
	if ((flag & (XValue|YValue)) == (XValue|YValue))
	    wmshell->wm.size_hints.flags |= USPosition;
	if ((flag & (WidthValue|HeightValue)) == (WidthValue|HeightValue))
	    wmshell->wm.size_hints.flags |= USSize;
    }
    w->shell.client_specified |= _XtShellGeometryParsed;
}


static void ChangeManaged(wid)
    Widget wid;
{
    ShellWidget w = (ShellWidget) wid;
    Widget child = NULL;
    int i;

    for (i = 0; i < w->composite.num_children; i++) {
	if (XtIsManaged(w->composite.children[i])) {
	    child = w->composite.children[i];
	    break;		/* there can only be one of them! */
	}
    }

    if (!XtIsRealized (wid))	/* then we're about to be realized... */
	GetGeometry(wid, child);

    if (child != NULL)
	XtConfigureWidget (child, (Position)0, (Position)0,
			   w->core.width, w->core.height, (Dimension)0 );
}

/*
 * This is gross, I can't wait to see if the change happened so I will ask
 * the window manager to change my size and do the appropriate X work.
 * I will then tell the requester that he can.  Care must be taken because
 * it is possible that some time in the future the request will be
 * asynchronusly denied and the window reverted to it's old size/shape.
 */
 
/*ARGSUSED*/
static XtGeometryResult GeometryManager( wid, request, reply )
	Widget wid;
	XtWidgetGeometry *request;
	XtWidgetGeometry *reply;
{
	ShellWidget shell = (ShellWidget)(wid->core.parent);
	XtWidgetGeometry my_request;

	if(shell->shell.allow_shell_resize == FALSE && XtIsRealized(wid))
		return(XtGeometryNo);

	if (request->request_mode & (CWX | CWY))
	    return(XtGeometryNo);

	/* %%% worry about XtCWQueryOnly */
	my_request.request_mode = 0;
	if (request->request_mode & CWWidth) {
	    my_request.width = request->width;
	    my_request.request_mode |= CWWidth;
	}
	if (request->request_mode & CWHeight) {
	    my_request.height = request->height;
	    my_request.request_mode |= CWHeight;
	}
	if (request->request_mode & CWBorderWidth) {
	    my_request.border_width = request->border_width;
	    my_request.request_mode |= CWBorderWidth;
	}
	if (XtMakeGeometryRequest((Widget)shell, &my_request, NULL)
		== XtGeometryYes) {
	    /* assert: if (request->request_mode & CWWidth) then
	     * 		  shell->core.width == request->width
	     * assert: if (request->request_mode & CWHeight) then
	     * 		  shell->core.height == request->height
	     *
	     * so, whatever the WM sized us to (if the Shell requested
	     * only one of the two) is now the correct child size
	     */
	    
	    wid->core.width = shell->core.width;
	    wid->core.height = shell->core.height;
	    if (request->request_mode & CWBorderWidth) {
		wid->core.x = wid->core.y = -request->border_width;
	    }
	    return XtGeometryYes;
	} else return XtGeometryNo;
}

typedef struct {
	Widget  w;
	unsigned long request_num;
	Boolean done;
} QueryStruct;

static Bool isMine(dpy, event, arg)
	Display *dpy;
	register XEvent  *event;
	char *arg;
{
	QueryStruct *q = (QueryStruct *) arg;
	register Widget w = q->w;
	
	if ( (dpy != XtDisplay(w)) || (event->xany.window != XtWindow(w)) ) {
	    return FALSE;
	}
	if (event->xany.serial >= q->request_num) {
	    if (event->type == ConfigureNotify) {
		q->done = TRUE;
		return TRUE;
	    } else
		/* This is draft-ICCCM stuff; here for compatibility */
		if (event->type == ClientMessage &&
		    (event->xclient.message_type == WM_CONFIGURE_DENIED(w) ||
		     event->xclient.message_type == WM_MOVED(w))) {
		    q->done = TRUE;
		    return TRUE;
		}
	}
	else if (event->type == ConfigureNotify ||
		 (event->type == ClientMessage &&
		  (event->xclient.message_type == WM_CONFIGURE_DENIED(w) ||
		   event->xclient.message_type == WM_MOVED(w))))
	    return TRUE;	/* flush old events */
	if (event->type == ReparentNotify
		 && event->xreparent.window == XtWindow(w)) {
	    /* we might get ahead of this event, so just in case someone
	     * asks for coordinates before this event is dispatched...
	     */
	    register ShellWidget s = (ShellWidget)w;
	    if (event->xreparent.parent != RootWindowOfScreen(XtScreen(w)))
		s->shell.client_specified &= ~_XtShellNotReparented;
	    else
		s->shell.client_specified |= _XtShellNotReparented;
	}
	return FALSE;
}

static _wait_for_response(w, event, request_num)
	ShellWidget	w;
	XEvent		*event;
        unsigned long	request_num;
{
	XtAppContext app = XtWidgetToApplicationContext((Widget) w);
	QueryStruct q;
	unsigned long timeout;

	if (XtIsWMShell((Widget)w))
	    timeout = ((WMShellWidget)w)->wm.wm_timeout;
	else
	    timeout = DEFAULT_WM_TIMEOUT;

	XFlush(XtDisplay(w));
	q.w = (Widget) w;
	q.request_num = request_num;
	q.done = FALSE;
	
	for(;;) {
 	    /*
 	     * look for match event and discard all prior configures
 	     */
	    if (XCheckIfEvent( XtDisplay(w), event, isMine, (char*)&q)) {
		if (q.done)
		    return TRUE;
		else
		    continue;	/* flush old events */
	    }

	    if (_XtwaitForSomething(TRUE, TRUE, FALSE, TRUE, &timeout, app)
		!= -1) continue;
	    if (timeout == 0)
		return FALSE;
	}
}

/*ARGSUSED*/
static XtGeometryResult RootGeometryManager(gw, request, reply)
    Widget gw;
    XtWidgetGeometry *request, *reply;
{
    register ShellWidget w = (ShellWidget)gw;
    XWindowChanges values;
    unsigned int mask = request->request_mode;
    XEvent event;
    Boolean wm;
    register struct _OldXSizeHints *hintp;
    int oldx, oldy, oldwidth, oldheight, oldborder_width;
    unsigned long request_num;

    if (XtIsWMShell(gw)) {
	wm = True;
	hintp = &((WMShellWidget)w)->wm.size_hints;
	/* for draft-ICCCM wm's, need to make sure hints reflect
	   (current) reality so client can move and size separately. */
  	hintp->x = w->core.x;
  	hintp->y = w->core.y;
  	hintp->width = w->core.width;
   	hintp->height = w->core.height;
    } else
	wm = False;
    
    oldx = w->core.x;
    oldy = w->core.y;
    oldwidth = w->core.width;
    oldheight = w->core.height;
    oldborder_width = w->core.border_width;

#define PutBackGeometry() \
	{ w->core.x = oldx; \
	  w->core.y = oldy; \
	  w->core.width = oldwidth; \
	  w->core.height = oldheight; \
	  w->core.border_width = oldborder_width; }

    if (mask & CWX) {
	    if (w->core.x == request->x) mask &= ~CWX;
	    else {
		w->core.x = values.x = request->x;
		if (wm) {
		    hintp->flags &= ~USPosition;
		    hintp->flags |= PPosition;
		    hintp->x = values.x;
		}
	    }
    }
    if (mask & CWY) {
	    if (w->core.y == request->y) mask &= ~CWY;
	    else {
		w->core.y = values.y = request->y;
		if (wm) {
		    hintp->flags &= ~USPosition;
		    hintp->flags |= PPosition;
		    hintp->y = values.y;
		}
	    }
    }
    if (mask & CWBorderWidth) {
	    if (w->core.border_width == request->border_width) {
		    mask &= ~CWBorderWidth;
	    } else
		w->core.border_width =
		    values.border_width =
			request->border_width;
    }
    if (mask & CWWidth) {
	    if (w->core.width == request->width) mask &= ~CWWidth;
	    else {
		w->core.width = values.width = request->width;
		if (wm) {
		    hintp->flags &= ~USSize;
		    hintp->flags |= PSize;
		    hintp->width = values.width;
		}
	    }
    }
    if (mask & CWHeight) {
	    if (w->core.height == request->height) mask &= ~CWHeight;
	    else {
		w->core.height = values.height = request->height;
		if (wm) {
		    hintp->flags &= ~USSize;
		    hintp->flags |= PSize;
		    hintp->height = values.height;
		}
	    }
    }
    if (mask & CWStackMode) {
	values.stack_mode = request->stack_mode;
	if (mask & CWSibling)
	    values.sibling = XtWindow(request->sibling);
    }

    if (!XtIsRealized((Widget)w)) return XtGeometryYes;

    request_num = NextRequest(XtDisplay(w));
    XConfigureWindow(XtDisplay((Widget)w), XtWindow((Widget)w), mask,&values);

    if (wm && !w->shell.override_redirect
	&& mask & (CWX | CWY | CWWidth | CWHeight | CWBorderWidth)) {
	_SetWMSizeHints((WMShellWidget)w);
    }

    if (w->shell.override_redirect) return XtGeometryYes;

    /* If no non-stacking bits are set, there's no way to tell whether
       or not this worked, so assume it did */

    if (!(mask & ~(CWStackMode | CWSibling))) return XtGeometryYes;

    if (wm && ((WMShellWidget)w)->wm.wait_for_wm == FALSE) {
	    /* the window manager is sick
	     * so I will do the work and 
	     * say no so if a new WM starts up,
	     * or the current one recovers
	     * my size requests will be visible
	     */
	    PutBackGeometry();
	    return XtGeometryNo;
    }

    if (_wait_for_response(w, &event, request_num)) {
	/* got an event */
	if (event.type == ConfigureNotify) {

#define NEQ(x, msk) ((mask & msk) && (values.x != event.xconfigure.x))	
	    if (NEQ(x, CWX) ||
		NEQ(y, CWY) ||
		NEQ(width, CWWidth) ||
		NEQ(height, CWHeight) ||
		NEQ(border_width, CWBorderWidth)) {
#undef NEQ
		XPutBackEvent(XtDisplay(w), &event);
		PutBackGeometry();
		/*
		 * We just potentially re-ordered the event queue
		 * w.r.t. ConfigureNotifies with some trepidation.
		 * But this is probably a Good Thing because we
		 * will know the new true state of the world sooner
		 * this way.
		 */
		return XtGeometryNo;
	    }
	    else {
		w->core.width = event.xconfigure.width;
		w->core.height = event.xconfigure.height;
		w->core.border_width = event.xconfigure.border_width;
		if (event.xany.send_event || /* ICCCM compliant synth */
		    w->shell.client_specified & _XtShellNotReparented) {

		    w->core.x = event.xconfigure.x;
		    w->core.y = event.xconfigure.y;
		    w->shell.client_specified |= _XtShellPositionValid;
		}
		else w->shell.client_specified &= ~_XtShellPositionValid;
		return XtGeometryYes;
	    }
	} else if (!wm ||
		   (event.type == ClientMessage &&
		    event.xclient.message_type == WM_CONFIGURE_DENIED(w))) {
	    PutBackGeometry();
	    return XtGeometryNo;
	} else if (event.type == ClientMessage &&
		    event.xclient.message_type == WM_MOVED(w)) {
	    w->core.x = event.xclient.data.s[0];
	    w->core.y = event.xclient.data.s[1];
	    w->shell.client_specified |= _XtShellPositionValid;
	    return XtGeometryYes;
	} else XtAppWarningMsg(XtWidgetToApplicationContext((Widget)w),
			       "internalError", "shell", XtCXtToolkitError,
			       "Shell's window manager interaction is broken",
			       (String *)NULL, (Cardinal *)NULL);
    } else if (wm) { /* no event */ 
	((WMShellWidget)w)->wm.wait_for_wm = FALSE; /* timed out; must be broken */
    }
    PutBackGeometry();
#undef PutBackGeometry
    return XtGeometryNo;
}

/* ARGSUSED */
static Boolean SetValues(old, ref, new, args, num_args)
	Widget old, ref, new;
	ArgList args;		/* unused */
	Cardinal *num_args;	/* unused */
{
	ShellWidget nw = (ShellWidget) new;
	ShellWidget ow = (ShellWidget) old;
	Mask mask = 0;
	XSetWindowAttributes attr;

	if (ow->shell.save_under != nw->shell.save_under) {
	    mask = CWSaveUnder;
	    attr.save_under = nw->shell.save_under;
	}

	if (ow->shell.override_redirect != nw->shell.override_redirect) {
	    mask |= CWOverrideRedirect;
	    attr.override_redirect = nw->shell.override_redirect;
	}

	if (mask && XtIsRealized(new)) {
	    XChangeWindowAttributes(XtDisplay(new),XtWindow(new), mask, &attr);
	    if ((mask & CWOverrideRedirect) && !nw->shell.override_redirect)
		_popup_set_prop(nw);
	}
	return FALSE;
}

/* ARGSUSED */
static Boolean WMSetValues(old, ref, new, args, num_args)
	Widget old, ref, new;
	ArgList args;		/* unused */
	Cardinal *num_args;	/* unused */
{
	WMShellWidget nwmshell = (WMShellWidget) new;
	WMShellWidget owmshell = (WMShellWidget) old;
	Boolean set_prop
	    = XtIsRealized(new) && !nwmshell->shell.override_redirect;
	Boolean title_changed;

	EvaluateSizeHints(nwmshell);

#define NEQ(f) (nwmshell->wm.size_hints.f != owmshell->wm.size_hints.f)

	if (set_prop
	    && (NEQ(flags) || NEQ(min_width) || NEQ(min_height)
		|| NEQ(max_width) || NEQ(max_height)
		|| NEQ(width_inc) || NEQ(height_inc)
		|| NEQ(min_aspect.x) || NEQ(min_aspect.y)
		|| NEQ(max_aspect.x) || NEQ(max_aspect.y)
#undef NEQ
#define NEQ(f) (nwmshell->wm.f != owmshell->wm.f)

		|| NEQ(base_width) || NEQ(base_height) || NEQ(win_gravity))) {
	    _SetWMSizeHints(nwmshell);
	}
#undef NEQ

	if (nwmshell->wm.title != owmshell->wm.title) {
	    XtFree(owmshell->wm.title);
	    if (! nwmshell->wm.title) nwmshell->wm.title = "";
	    nwmshell->wm.title = XtNewString(nwmshell->wm.title);
	    title_changed = True;
	} else
	    title_changed = False;

	if (set_prop
	    && (title_changed ||
		nwmshell->wm.title_encoding != owmshell->wm.title_encoding)) {

	    XTextProperty title;
	    Boolean copied = False;

            if (nwmshell->wm.title_encoding == None &&
		XmbTextListToTextProperty(XtDisplay(new),
					  (char**)&nwmshell->wm.title,
					  1, XStdICCTextStyle,
					  &title) >= Success) {
		copied = True;
	    } else {
		title.value = (unsigned char*)nwmshell->wm.title;
		title.encoding = nwmshell->wm.title_encoding;
		title.format = 8;
		title.nitems = strlen(nwmshell->wm.title);
	    }
	    XSetWMName(XtDisplay(new), XtWindow(new), &title);
	    if (copied)
		XFree((XPointer)title.value);
	}

	EvaluateWMHints(nwmshell);

#define NEQ(f)	(nwmshell->wm.wm_hints.f != owmshell->wm.wm_hints.f)

	if (set_prop
	    && (NEQ(flags) || NEQ(input) || NEQ(initial_state)
		|| NEQ(icon_x) || NEQ(icon_y)
		|| NEQ(icon_pixmap) || NEQ(icon_mask) || NEQ(icon_window)
		|| NEQ(window_group))) {

	    XSetWMHints(XtDisplay(new), XtWindow(new), &nwmshell->wm.wm_hints);
	}
#undef NEQ

 	if (XtIsRealized(new) &&
	    nwmshell->wm.transient != owmshell->wm.transient) {
 	    if (nwmshell->wm.transient) {
		if (!XtIsTransientShell(new) &&
		    !nwmshell->shell.override_redirect &&
		    nwmshell->wm.wm_hints.window_group != 
		       XtUnspecifiedWindowGroup)
		    XSetTransientForHint(XtDisplay(new), XtWindow(new),
					 nwmshell->wm.wm_hints.window_group);
	    }
 	    else XDeleteProperty(XtDisplay(new), XtWindow(new),
 				 XA_WM_TRANSIENT_FOR);
 	}
	
	return FALSE;
}

/*ARGSUSED*/
static Boolean TransientSetValues(oldW, refW, newW, args, num_args)
     Widget oldW, refW, newW;
     ArgList args;		/* unused */
     Cardinal *num_args;	/* unused */
{
    TransientShellWidget old = (TransientShellWidget)oldW;
    TransientShellWidget new = (TransientShellWidget)newW;
    
    if (XtIsRealized(newW)
	&& ((new->wm.transient && !old->wm.transient)
	    || ((new->transient.transient_for != old->transient.transient_for)
		|| (new->transient.transient_for == NULL
		    && (new->wm.wm_hints.window_group
			!= old->wm.wm_hints.window_group))))) {

	_SetTransientForHint(new, True);
    }
    return False;
}


/* ARGSUSED */
static Boolean TopLevelSetValues(oldW, refW, newW, args, num_args)
     Widget oldW, refW, newW;
     ArgList args;		/* unused */
     Cardinal *num_args;	/* unused */
{
    TopLevelShellWidget old = (TopLevelShellWidget)oldW;
    TopLevelShellWidget new = (TopLevelShellWidget)newW;
    Boolean name_changed;

    if (old->topLevel.icon_name != new->topLevel.icon_name) {
	XtFree((XtPointer)old->topLevel.icon_name);
	if (! new->topLevel.icon_name) new->topLevel.icon_name = "";
	new->topLevel.icon_name = XtNewString(new->topLevel.icon_name);
	name_changed = True;
    } else
	name_changed = False;

    if (XtIsRealized(newW)) {
	if (new->topLevel.iconic != old->topLevel.iconic) {
	    if (new->topLevel.iconic)
		XIconifyWindow(XtDisplay(newW),
			       XtWindow(newW),
			       XScreenNumberOfScreen(XtScreen(newW))
			       );
	    else {
		Boolean map = new->shell.popped_up;
		XtPopup(newW, XtGrabNone);
		if (map) XMapWindow(XtDisplay(newW), XtWindow(newW));
	    }
	}

	if (!new->shell.override_redirect &&
	    (name_changed ||
	     (old->topLevel.icon_name_encoding
	      != new->topLevel.icon_name_encoding))) {

	    XTextProperty icon_name;
	    Boolean copied = False;

            if (new->topLevel.icon_name_encoding == None &&
		XmbTextListToTextProperty(XtDisplay(newW),
					  (char**) &new->topLevel.icon_name,
					  1, XStdICCTextStyle,
					  &icon_name) >= Success) {
		copied = True;
	    } else {
		icon_name.value = (unsigned char *)new->topLevel.icon_name;
		icon_name.encoding = new->topLevel.icon_name_encoding;
		icon_name.format = 8;
		icon_name.nitems = strlen((char *)icon_name.value);
	    }
	    XSetWMIconName(XtDisplay(newW), XtWindow(newW), &icon_name);
	    if (copied)
		XFree((XPointer)icon_name.value);
	}
    }
    return False;
}


void _XtShellGetCoordinates( widget, x, y)
    Widget widget;
    Position* x;
    Position* y;
{
    ShellWidget w = (ShellWidget)widget;
    if (!(w->shell.client_specified & _XtShellPositionValid)) {
	int tmpx, tmpy;
	Window tmpchild;
	(void) XTranslateCoordinates(XtDisplay(w), XtWindow(w), 
				     RootWindowOfScreen(XtScreen(w)),
				     (int) -w->core.border_width,
				     (int) -w->core.border_width,
				     &tmpx, &tmpy, &tmpchild);
	w->core.x = tmpx;
	w->core.y = tmpy;
	w->shell.client_specified |= _XtShellPositionValid;
    }
    *x = w->core.x;
    *y = w->core.y;
}

static void GetValuesHook(widget, args, num_args)
    Widget	widget;
    ArgList	args;
    Cardinal*	num_args;
{
    ShellWidget w = (ShellWidget) widget;
    extern void _XtCopyToArg();

    /* x and y resource values may be invalid after a shell resize */
    if (! (w->shell.client_specified & _XtShellPositionValid)) {
	Cardinal	n;
	Position	x, y;

	for (n = *num_args; n; n--, args++) {
	    if (strcmp(XtNx, args->name) == 0) {
		 if (! (w->shell.client_specified & _XtShellPositionValid))
		     _XtShellGetCoordinates(widget, &x, &y);
		_XtCopyToArg((char *) &x, &args->value, sizeof(Position));
	    } else if (strcmp(XtNy, args->name) == 0) {
		 if (! (w->shell.client_specified & _XtShellPositionValid))
		     _XtShellGetCoordinates(widget, &x, &y);
		_XtCopyToArg((char *) &y, &args->value, sizeof(Position));
	    }
	}
    }
}

static void ApplicationShellInsertChild(widget)
    Widget widget;
{
    if (! XtIsWidget(widget) && XtIsRectObj(widget)) {
	XtAppWarningMsg(XtWidgetToApplicationContext(widget),
	       "invalidClass", "applicationShellInsertChild", XtCXtToolkitError,
	       "ApplicationShell does not accept RectObj children; ignored",
	       (String*)NULL, (Cardinal*)NULL);
    }
    else {
	(*((CompositeWidgetClass)applicationShellClassRec.core_class.
	   superclass)->composite_class.insert_child) (widget);
    }
}
