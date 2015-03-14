/* $XConsortium: Intrinsic.c,v 1.172 92/04/15 19:15:24 rws Exp $ */

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

#define INTRINSIC_C

#include "IntrinsicI.h"
#include "StringDefs.h"
#ifndef NO_IDENTIFY_WINDOWS
#include <X11/Xatom.h>
#endif
#ifndef VMS
#include <sys/stat.h>
#endif /* VMS */

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *getenv();
#endif

String XtCXtToolkitError = "XtToolkitError";

Boolean XtIsSubclass(widget, widgetClass)
    Widget    widget;
    WidgetClass widgetClass;
{
    register WidgetClass w;

    for (w = widget->core.widget_class; w != NULL; w = w->core_class.superclass)
	if (w == widgetClass) return (TRUE);
    return (FALSE);
} /* XtIsSubclass */


#if NeedFunctionPrototypes
Boolean _XtCheckSubclassFlag(
    Widget object,
    _XtXtEnum flag
    )
#else
Boolean _XtCheckSubclassFlag(object, flag)
    Widget object;
    XtEnum flag;
#endif
{
    if (object->core.widget_class->core_class.class_inited & flag)
	return True;
    else
	return False;

} /*_XtVerifySubclass */


#if NeedFunctionPrototypes
Boolean _XtIsSubclassOf(
    Widget object,
    WidgetClass widgetClass,
    WidgetClass superClass,
    _XtXtEnum flag
    )
#else
Boolean _XtIsSubclassOf(object, widgetClass, superClass, flag)
    Widget object;
    WidgetClass widgetClass, superClass;
    XtEnum flag;
#endif
{
    if (!(object->core.widget_class->core_class.class_inited & flag))
	return False;
    else {
	register WidgetClass c = object->core.widget_class;
	while (c != superClass) {
	    if (c == widgetClass)
		return True;
	    c = c->core_class.superclass;
	}
	return False;
    }
} /*_XtIsSubclassOf */


static void ComputeWindowAttributes(widget,value_mask,values)
    Widget		 widget;
    XtValueMask		 *value_mask;
    XSetWindowAttributes *values;
{
    *value_mask = CWEventMask | CWColormap;
    (*values).event_mask = XtBuildEventMask(widget);
    (*values).colormap = widget->core.colormap;
    if (widget->core.background_pixmap != XtUnspecifiedPixmap) {
	*value_mask |= CWBackPixmap;
	(*values).background_pixmap = widget->core.background_pixmap;
    } else {
	*value_mask |= CWBackPixel;
	(*values).background_pixel = widget->core.background_pixel;
    }
    if (widget->core.border_pixmap != XtUnspecifiedPixmap) {
	*value_mask |= CWBorderPixmap;
	(*values).border_pixmap = widget->core.border_pixmap;
    } else {
	*value_mask |= CWBorderPixel;
	(*values).border_pixel = widget->core.border_pixel;
    }
    if (widget->core.widget_class->core_class.expose == (XtExposeProc) NULL) {
	/* Try to avoid redisplay upon resize by making bit_gravity the same
	   as the default win_gravity */
	*value_mask |= CWBitGravity;
	(*values).bit_gravity = NorthWestGravity;
    }
} /* ComputeWindowAttributes */

static void CallChangeManaged(widget)
    register Widget		widget;
{
    register Cardinal		i;
    XtWidgetProc		change_managed;
    register WidgetList		children;
    int    			managed_children = 0;

    register CompositePtr cpPtr;
    register CompositePartPtr clPtr;
   
    if (XtIsComposite (widget)) {
	cpPtr = (CompositePtr)&((CompositeWidget) widget)->composite;
        clPtr = (CompositePartPtr)&((CompositeWidgetClass)
                   widget->core.widget_class)->composite_class;
    } else return;

    children = cpPtr->children;
    change_managed = clPtr->change_managed;

    /* CallChangeManaged for all children */
    for (i = cpPtr->num_children; i != 0; --i) {
	CallChangeManaged (children[i-1]);
	if (XtIsManaged(children[i-1])) managed_children++;
    }

    if (change_managed != NULL && managed_children != 0) {
	(*change_managed) (widget);
    }
} /* CallChangeManaged */


static void MapChildren(cwp)
    CompositePart *cwp;
{
    Cardinal i;
    WidgetList children;
    register Widget child;

    children = cwp->children;
    for (i = 0; i <  cwp->num_children; i++) {
	child = children[i];
	if (XtIsWidget (child)){
	    if (child->core.managed && child->core.mapped_when_managed) {
		XtMapWidget (children[i]);
	    }
	}
    }
} /* MapChildren */


static Boolean ShouldMapAllChildren(cwp)
    CompositePart *cwp;
{
    Cardinal i;
    WidgetList children;
    register Widget child;

    children = cwp->children;
    for (i = 0; i < cwp->num_children; i++) {
	child = children[i];
	if (XtIsWidget(child)) {
	    if (XtIsRealized(child) && (! (child->core.managed 
					  && child->core.mapped_when_managed))){
		    return False;
	    }
	}
    }

    return True;
} /* ShouldMapAllChildren */


static void RealizeWidget(widget)
    register Widget		widget;
{
    XtValueMask			value_mask;
    XSetWindowAttributes	values;
    XtRealizeProc		realize;
    Window			window;

    if (!XtIsWidget(widget) || XtIsRealized(widget)) return;

    _XtInstallTranslations(widget);

    ComputeWindowAttributes (widget, &value_mask, &values);
    realize = widget->core.widget_class->core_class.realize;
    if (realize == NULL)
	XtAppErrorMsg(XtWidgetToApplicationContext(widget),
		      "invalidProcedure","realizeProc",XtCXtToolkitError,
		      "No realize class procedure defined",
		      (String *)NULL, (Cardinal *)NULL);
    else (*realize) (widget, &value_mask, &values);
    window = XtWindow(widget);
#ifndef NO_IDENTIFY_WINDOWS
    if (_XtGetPerDisplay(XtDisplay(widget))->appContext->identify_windows) {
	int len_nm, len_cl;
	char *s;

	len_nm = widget->core.name ? strlen(widget->core.name) : 0;
	len_cl = strlen(widget->core.widget_class->core_class.class_name);
	s = XtMalloc((unsigned) (len_nm + len_cl + 2));
	s[0] = '\0';
	if (len_nm)
	    strcpy(s, widget->core.name);
	strcpy(s + len_nm + 1,
	       widget->core.widget_class->core_class.class_name);
	XChangeProperty(XtDisplay(widget), window,
			XInternAtom(XtDisplay(widget), "_MIT_OBJ_CLASS",
				    False),
			XA_STRING, 8, PropModeReplace, (unsigned char *) s, 
			len_nm + len_cl + 2);
	XtFree(s);
    }
#endif
#ifdef notdef
    _XtRegisterAsyncHandlers(widget);
#endif
    _XtRegisterGrabs(widget);
    _XtRegisterWindow (window, widget);

    if (XtIsComposite (widget)) {
	register Cardinal		i;
	register CompositePart *cwp = &(((CompositeWidget)widget)->composite);
	register WidgetList children = cwp->children;
	/* Realize all children */
	for (i = cwp->num_children; i != 0; --i) {
	    RealizeWidget (children[i-1]);
	}
	/* Map children that are managed and mapped_when_managed */

	if (cwp->num_children != 0) {
	    if (ShouldMapAllChildren(cwp)) {
		XMapSubwindows (XtDisplay(widget), window);
	    } else {
		MapChildren(cwp);
	    }
	}
    }

    /* If this is the application's popup shell, map it */
    if (widget->core.parent == NULL && widget->core.mapped_when_managed) {
	XtMapWidget (widget);
    }
} /* RealizeWidget */

void XtRealizeWidget (widget)
    register Widget		widget;
{
    if (XtIsRealized (widget)) return;

    CallChangeManaged(widget);
    RealizeWidget(widget);
} /* XtRealizeWidget */


static void UnrealizeWidget(widget)
    register Widget		widget;
{
    register CompositeWidget	cw;
    register Cardinal		i;
    register WidgetList		children;

    if (!XtIsWidget(widget) || !XtIsRealized(widget)) return;

    /* If this is the application's popup shell, unmap it? */
    /* no, the window is being destroyed */

    /* Recurse on children */
    if (XtIsComposite (widget)) {
	cw = (CompositeWidget) widget;
	children = cw->composite.children;
	/* Unrealize all children */
	for (i = cw->composite.num_children; i != 0; --i) {
	    UnrealizeWidget (children[i-1]);
	}
	/* Unmap children that are managed and mapped_when_managed? */
	/* No, it's ok to be managed and unrealized as long as your parent */
	/* is unrealized. XtUnrealize widget makes sure the "top" widget */
	/* is unmanaged, we can ignore all descendents */
    }

    if (XtHasCallbacks(widget, XtNunrealizeCallback) == XtCallbackHasSome)
	XtCallCallbacks(widget, XtNunrealizeCallback, NULL);

    /* Unregister window */
    _XtUnregisterWindow(XtWindow(widget), widget);

    /* Remove Event Handlers */
    /* remove grabs. Happens automatically when window is destroyed. */

    /* Destroy X Window, done at outer level with one request */
    widget->core.window = None;

    /* Removing the event handler here saves having to keep track if
     * the translation table is changed while the widget is unrealized.
     */
    _XtRemoveTranslations(widget);
} /* UnrealizeWidget */


void XtUnrealizeWidget (widget)
    register Widget		widget;
{
    Window window = XtWindow(widget);

    if (! XtIsRealized (widget)) return;

    if (widget->core.parent != NULL) XtUnmanageChild(widget);

    UnrealizeWidget(widget);

    if (window != None) XDestroyWindow(XtDisplay(widget), window);
} /* XtUnrealizeWidget */


void XtCreateWindow(widget, window_class, visual, value_mask, attributes)
    Widget		 widget;
    unsigned int	 window_class;
    Visual		 *visual;
    XtValueMask		 value_mask;
    XSetWindowAttributes *attributes;
{
    if (widget->core.window == None) {
	if (widget->core.width == 0 || widget->core.height == 0) {
	    Cardinal count = 1;
	    XtAppErrorMsg(XtWidgetToApplicationContext(widget),
		       "invalidDimension", "xtCreateWindow", XtCXtToolkitError,
		       "Widget %s has zero width and/or height",
		       &widget->core.name, &count);
	}
	widget->core.window =
	    XCreateWindow (
		XtDisplay (widget),
		(widget->core.parent ?
		    widget->core.parent->core.window :
		    widget->core.screen->root),
		(int)widget->core.x, (int)widget->core.y,
		(unsigned)widget->core.width, (unsigned)widget->core.height,
		(unsigned)widget->core.border_width, (int) widget->core.depth,
		window_class, visual, value_mask, attributes);
    }
} /* XtCreateWindow */


/* ---------------- XtNameToWidget ----------------- */

static Widget NameListToWidget();

typedef Widget (*NameMatchProc)();

static Widget MatchExactChildren(names, bindings, children, num,
	in_depth, out_depth, found_depth)
    XrmNameList     names;
    XrmBindingList  bindings;
    register WidgetList children;
    register int num;
    int in_depth, *out_depth, *found_depth;
{
    register Cardinal   i;
    register XrmName    name = *names;
    Widget w, result = NULL;
    int d, min = 10000;

    for (i = 0; i < num; i++) {
	if (name == children[i]->core.xrm_name) {
	    w = NameListToWidget(children[i], &names[1], &bindings[1],
		    in_depth+1, &d, found_depth);
	    if (w != NULL && d < min) {result = w; min = d;}
	}
    }
    *out_depth = min;
    return result;
}

static Widget MatchWildChildren(names, bindings, children, num,
	in_depth, out_depth, found_depth)
    XrmNameList     names;
    XrmBindingList  bindings;
    register WidgetList children;
    register int num;
    int in_depth, *out_depth, *found_depth;
{
    register Cardinal   i;
    Widget w, result = NULL;
    int d, min = 10000;

    for (i = 0; i < num; i++) {
	w = NameListToWidget(children[i], names, bindings,
		in_depth+1, &d, found_depth);
	if (w != NULL && d < min) {result = w; min = d;}
    }
    *out_depth = min;
    return result;
}

static Widget SearchChildren(root, names, bindings, matchproc,
	in_depth, out_depth, found_depth)
    Widget root;
    XrmNameList     names;
    XrmBindingList  bindings;
    NameMatchProc matchproc;
    int in_depth, *out_depth, *found_depth;
{
    Widget w1, w2;
    int d1, d2;

    if (XtIsComposite(root)) {
	w1 = (*matchproc)(names, bindings,
		((CompositeWidget) root)->composite.children,
		((CompositeWidget) root)->composite.num_children,
		in_depth, &d1, found_depth);
    } else d1 = 10000;
    w2 = (*matchproc)(names, bindings, root->core.popup_list,
	    root->core.num_popups, in_depth, &d2, found_depth);
    *out_depth = (d1 < d2 ? d1 : d2);
    return (d1 < d2 ? w1 : w2);
}

static Widget NameListToWidget(root, names, bindings,
	in_depth, out_depth, found_depth)
    register Widget root;
    XrmNameList     names;
    XrmBindingList  bindings;
    int in_depth, *out_depth, *found_depth;
{
    Widget w1, w2;
    int d1, d2;

    if (in_depth >= *found_depth) {
	*out_depth = 10000;
	return NULL;
    }

    if (names[0] == NULLQUARK) {
	*out_depth = *found_depth = in_depth;
	return root;
    }

    if (! XtIsWidget(root)) {
	*out_depth = 10000;
	return NULL;
    }

    if (*bindings == XrmBindTightly) {
	return SearchChildren(root, names, bindings, MatchExactChildren,
		in_depth, out_depth, found_depth);

    } else {	/* XrmBindLoosely */
	w1 = SearchChildren(root, names, bindings, MatchExactChildren,
		in_depth, &d1, found_depth);
	w2 = SearchChildren(root, names, bindings, MatchWildChildren,
		in_depth, &d2, found_depth);
	*out_depth = (d1 < d2 ? d1 : d2);
	return (d1 < d2 ? w1 : w2);
    }
} /* NameListToWidget */

#if NeedFunctionPrototypes
Widget XtNameToWidget(
    Widget root,
    _Xconst char* name
    )
#else
Widget XtNameToWidget(root, name)
    Widget root;
    String name;
#endif
{
    XrmName *names;
    XrmBinding *bindings;
    int len, depth, found = 10000;
    Widget result;

    len = strlen(name);
    if (len == 0) return NULL;

    names = (XrmName *) ALLOCATE_LOCAL((unsigned) (len+1) * sizeof(XrmName));
    bindings = (XrmBinding *)
	ALLOCATE_LOCAL((unsigned) (len+1) * sizeof(XrmBinding));
    if (names == NULL || bindings == NULL) _XtAllocError(NULL);

    XrmStringToBindingQuarkList(name, bindings, names);
    if (names[0] == NULLQUARK) {
	DEALLOCATE_LOCAL((char *) bindings);
	DEALLOCATE_LOCAL((char *) names);
	return NULL;
    }

    result = NameListToWidget(root, names, bindings, 0, &depth, &found);

    DEALLOCATE_LOCAL((char *) bindings);
    DEALLOCATE_LOCAL((char *) names);
    return result;
} /* XtNameToWidget */

/* Define user versions of intrinsics macros */

#undef XtDisplayOfObject
Display *XtDisplayOfObject(object)
     Widget object;
{
    return XtDisplay(XtIsWidget(object) ? object : _XtWindowedAncestor(object));
}

#undef XtDisplay
Display *XtDisplay(widget)
	Widget widget;
{
    return DisplayOfScreen(widget->core.screen);
}

#undef XtScreenOfObject
Screen *XtScreenOfObject(object)
     Widget object;
{
    return XtScreen(XtIsWidget(object) ? object : _XtWindowedAncestor(object));
}

#undef XtScreen
Screen *XtScreen(widget)
	Widget widget;
{
    return widget->core.screen;
}

#undef XtWindowOfObject
Window XtWindowOfObject(object)
     Widget object;
{
    return XtWindow(XtIsWidget(object) ? object : _XtWindowedAncestor(object));
}


#undef XtWindow
Window XtWindow(widget)
	Widget widget;
{
    return widget->core.window;
}

#undef XtSuperclass
WidgetClass XtSuperclass(widget)
	Widget widget;
{
	return XtClass(widget)->core_class.superclass;
}

#undef XtClass
WidgetClass XtClass(widget)
	Widget widget;
{
	return widget->core.widget_class;
}

#undef XtIsManaged
Boolean XtIsManaged(object)
	Widget object;
{
    if (XtIsRectObj(object))
	return object->core.managed;
    else
	return False;
}

#undef XtIsRealized
Boolean XtIsRealized (object)
	Widget   object;
{
    return XtWindowOfObject(object) != None;
} /* XtIsRealized */

#undef XtIsSensitive
Boolean XtIsSensitive(object)
	Widget	object;
{
    if (XtIsRectObj(object))
	return object->core.sensitive && object->core.ancestor_sensitive;
    else
	return False;
}

/*
 * Internal routine; must be called only after XtIsWidget returns false
 */
Widget _XtWindowedAncestor(object)
    register Widget object;
{
    Widget obj = object;
    for (object = XtParent(object); object && !XtIsWidget(object);)
	object = XtParent(object);

    if (object == NULL) {
	String params = XtName(obj);
	Cardinal num_params = 1;
	XtErrorMsg("noWidgetAncestor", "windowedAncestor", XtCXtToolkitError,
		   "Object \"%s\" does not have windowed ancestor",
		   &params, &num_params);
    }

    return object;
}

#undef XtParent
Widget XtParent(widget)
	Widget widget;
{
	return widget->core.parent;
}

#undef XtName
String XtName(object)
     Widget object;
{
    return XrmQuarkToString(object->core.xrm_name);
}


Boolean XtIsObject(object)
    Widget object;
{
    WidgetClass wc;
    String class_name;

    /* perform basic sanity checks */
    if (object->core.self != object || object->core.xrm_name == NULLQUARK)
	return False;

    wc = object->core.widget_class;
    if (wc->core_class.class_name == NULL ||
	wc->core_class.xrm_class == NULLQUARK ||
	(class_name = XrmClassToString(wc->core_class.xrm_class)) == NULL ||
	strcmp(wc->core_class.class_name, class_name) != 0)
	    return False;

    if (XtIsWidget(object)) {
	if (object->core.name == NULL ||
	    (class_name = XrmNameToString(object->core.xrm_name)) == NULL ||
	    strcmp(object->core.name, class_name) != 0)
	    return False;
    }
    return True;
}


static Boolean TestFile(path)
    String path;
{
#ifdef VMS
    return TRUE;	/* Who knows what to do here? */
#else
    struct stat status;

    return (access(path, R_OK) == 0 &&		/* exists and is readable */
	    stat(path, &status) == 0 &&		/* get the status */
#ifndef X_NOT_POSIX
	    S_ISDIR(status.st_mode) == 0);	/* not a directory */
#else
	    (status.st_mode & S_IFDIR) == 0);	/* not a directory */
#endif /* X_NOT_POSIX else */
#endif /* VMS */
}

/* return of TRUE = resolved string fit, FALSE = didn't fit.  Not
   null-terminated and not collapsed if it didn't fit */

static Boolean Resolve(source, len, sub, num, buf, collapse)
    register char *source;	/* The source string */
    register int len;		/* The length in bytes of *source */
    Substitution sub;	/* Array of string values to substitute */
    Cardinal num;	/* Number of substitution entries */
    char *buf;		/* Where to put the resolved string; */
    char collapse;	/* Character to collapse */
{
    register int bytesLeft = PATH_MAX;
    register char* bp = buf;
#ifndef DONT_COLLAPSE
    Boolean atBeginning = TRUE;
    Boolean prevIsCollapse = FALSE;

#define PUT(ch) \
    { \
	if (--bytesLeft == 0) return FALSE; \
        if (prevIsCollapse) \
	    if ((*bp = ch) != collapse) { \
		prevIsCollapse = FALSE; \
		bp++; \
	    } \
	    else bytesLeft++; \
        else if ((*bp++ = ch) == collapse && !atBeginning) \
	    prevIsCollapse = TRUE; \
    }
#else /* DONT_COLLAPSE */

#define PUT(ch) \
    { \
	if (--bytesLeft == 0) return FALSE; \
	*bp++ = ch; \
    }
#endif /* DONT_COLLAPSE */
#define escape '%'

    while (len--) {
#ifndef DONT_COLLAPSE
	if (*source == collapse) {
	    PUT(*source);
	    source++;
	    continue;
	}
	else
#endif /* DONT_COLLAPSE */
	    if (*source != escape) {
		PUT(*source);
	}
	else {
	    source++;
	    if (len-- == 0) {
		PUT(escape);
		break;
	    }

	    if (*source == ':' || *source == escape)
		PUT(*source)
	    else {
		/* Match the character against the match array */
		register int j;

		for (j = 0; j < num && sub[j].match != *source; j++) {}

		/* Substitute the substitution string */

		if (j >= num) PUT(*source)
		else if (sub[j].substitution != NULL) {
		    char *sp = sub[j].substitution;
		    while (*sp) {
			PUT(*sp);
			sp++;
		    }
		}
	    }
	}
	source++;
#ifndef DONT_COLLAPSE
	atBeginning = FALSE;
#endif /* DONT_COLLAPSE */
    }
    PUT('\0');

    return TRUE;
#undef PUT
#undef escape
}


#if NeedFunctionPrototypes
String XtFindFile(
    _Xconst char* path,
    Substitution substitutions,
    Cardinal num_substitutions,
    XtFilePredicate predicate
    )
#else
String XtFindFile(path, substitutions, num_substitutions, predicate)
    String path;
    Substitution substitutions;
    Cardinal num_substitutions;
    XtFilePredicate predicate;
#endif
{
    char *buf, *buf1, *buf2, *colon, *start;
    int len;
    Boolean firstTime = TRUE;

    buf = buf1 = XtMalloc((unsigned)PATH_MAX);
    buf2 = XtMalloc((unsigned)PATH_MAX);

    if (predicate == NULL) predicate = TestFile;

    while (1) {
	start = (String)path;
	while (1) {
	    colon = index(start, ':');
	    if (colon == NULL) break;
	    if (colon == path) {start++; path++; continue; }
	    if (*(colon-1) != '%') break;
	    start = colon+1;
	}
	if (colon != NULL)
	    len = colon - path;
	else
	    len = strlen(path);
	if (Resolve(path, len, substitutions, num_substitutions,
		    buf, '/')) {
	    if (firstTime || strcmp(buf1,buf2) != 0) {
#ifdef XNL_DEBUG
		printf("Testing file %s\n", buf);
#endif /* XNL_DEBUG */

		/* Check out the file */
		if ((*predicate) (buf)) {
		    /* We've found it, return it */
#ifdef XNL_DEBUG
		    printf("File found.\n");
#endif /* XNL_DEBUG */
		    if (buf == buf1) XtFree(buf2);
		    else XtFree(buf1);
		    return buf;
		}
		if (buf == buf1)
		    buf = buf2;
		else
		    buf = buf1;
		firstTime = FALSE;
	    }
	}

	/* Nope...any more paths? */

	if (colon == NULL) break;
	path = colon+1;
    }

    /* No file found */

    XtFree(buf1);
    XtFree(buf2);
    return NULL;
}


/* The implementation of this routine is operating system dependent */

static char *ExtractLocaleName(lang)
    String	lang;
{

#ifdef hpux	 /* hpux-specific parsing of the locale string */
#define MAXLOCALE       64      /* buffer size of locale name */

    char           *start;
    char           *end;
    int             len;
    static char     buf[MAXLOCALE];

    /*  If lang has a substring ":<category>;", extract <category>
     *  from the first such occurrence as the locale name.
     */

    start = lang;
    if (start = strchr (lang, ':')) {
        start++;
        if (end = strchr (start, ';')) {
            len = end - start;
            strncpy(buf, start, len);
            *(buf + len) = '\0';
            lang = buf;
      }
    }
#endif	/* hpux */

    /*  If result is "C", return NULL instead. */

    if (strcmp(lang, "C"))
        return lang;
    else
      return NULL;
}

static void FillInLangSubs(subs, pd)
    Substitution subs;
    XtPerDisplay pd;
{
    int len;
    char *string, *p1, *p2, *p3;
    char **rest;
    char *ch;

    if (pd->language == NULL || pd->language[0] == '\0') {
	subs[0].substitution = subs[1].substitution =
		subs[2].substitution = subs[3].substitution = NULL;
	return;
    }

    string = ExtractLocaleName(pd->language);

    if (string == NULL || string[0] == '\0') {
	subs[0].substitution = subs[1].substitution =
		subs[2].substitution = subs[3].substitution = NULL;
	return;
    }

    len = strlen(string) + 1;
    subs[0].substitution = string;
    p1 = subs[1].substitution = XtMalloc((Cardinal) 3*len);
    p2 = subs[2].substitution = subs[1].substitution + len;
    p3 = subs[3].substitution = subs[2].substitution + len;

    /* Everything up to the first "_" goes into p1.  From "_" to "." in
       p2.  The rest in p3.  If no delimiters, all goes into p1.  We
       assume p1, p2, and p3 are large enough. */

    *p1 = *p2 = *p3 = '\0';

    ch = index(string, '_');
    if (ch != NULL) {
	len = ch - string;
	(void) strncpy(p1, string, len);
	p1[len] = '\0';
	string = ch + 1;
	rest = &p2;
    } else rest = &p1;

    /* Rest points to where we put the first part */

    ch = index(string, '.');
    if (ch != NULL) {
	len = ch - string;
	strncpy(*rest, string, len);
	(*rest)[len] = '\0';
	(void) strcpy(p3, ch+1);
    } else (void) strcpy(*rest, string);
}

static SubstitutionRec defaultSubs[] = {
    {'N', NULL},
    {'T', NULL},
    {'S', NULL},
    {'C', NULL},
    {'L', NULL},
    {'l', NULL}, 
    {'t', NULL},
    {'c', NULL}
};


#if NeedFunctionPrototypes
String XtResolvePathname(
    Display *dpy,
    _Xconst char* type,
    _Xconst char* filename,
    _Xconst char* suffix,
    _Xconst char* path,
    Substitution substitutions,
    Cardinal num_substitutions,
    XtFilePredicate predicate
    )
#else
String XtResolvePathname(dpy, type, filename, suffix, path, substitutions,
			 num_substitutions, predicate)
    Display *dpy;
    String type, filename, suffix, path;
    Substitution substitutions;
    Cardinal num_substitutions;
    XtFilePredicate predicate;
#endif
{
    XtPerDisplay pd = _XtGetPerDisplay(dpy);
    static char *defaultPath = NULL;
    char *massagedPath;
    int bytesAllocd, bytesLeft;
    char *ch, *result;
    Substitution merged_substitutions;
    XrmRepresentation db_type;
    XrmValue value;
    XrmName name_list[3];
    XrmClass class_list[3];
    Boolean pathMallocd = False;

    if (path == NULL) {
#ifndef VMS
	if (defaultPath == NULL) {
	    defaultPath = getenv("XFILESEARCHPATH");
	    if (defaultPath == NULL) defaultPath = XFILESEARCHPATHDEFAULT;
	}
	path = defaultPath;
#else
	path = "";	/* NULL would kill us later */
#endif /* VMS */
    }	

    if (filename == NULL) {
	filename = XrmClassToString(pd->class);
    }

    bytesAllocd = bytesLeft = 1000;
    massagedPath = ALLOCATE_LOCAL(bytesAllocd);
    if (massagedPath == NULL) _XtAllocError(NULL);

    if (path[0] == ':') {
	strcpy(massagedPath, "%N%S");
	ch = &massagedPath[4];
	bytesLeft -= 4;
    } else ch = massagedPath;

    /* Insert %N%S between adjacent colons */

    while (*path != '\0') {
	if (bytesLeft < 8) {
	    int bytesUsed = bytesAllocd - bytesLeft;
	    char *new;
	    bytesAllocd +=1000;
	    new = XtMalloc((Cardinal) bytesAllocd);
	    strncpy( new, massagedPath, bytesUsed );
	    ch = new + bytesUsed;
	    if (pathMallocd)
		XtFree(massagedPath);
	    else
		DEALLOCATE_LOCAL(massagedPath);
	    pathMallocd = True;
	    massagedPath = new;
	    bytesLeft = bytesAllocd - bytesUsed;
	}
	if (*path == '%' && *(path+1) == ':') {
	    *ch++ = '%';
	    *ch++ = ':';
	    path += 2;
	    bytesLeft -= 2;
	    continue;
	}
	if (*path == ':' && *(path+1) == ':') {
	    strcpy(ch, ":%N%S:");
	    ch += 6;
	    bytesLeft -= 6;
	    while (*path == ':') path++;
	    continue;
	}
	*ch++ = *path++;
	bytesLeft--;
    }
    *ch = '\0';
#ifdef XNL_DEBUG
    printf("Massaged path: %s\n", massagedPath);
#endif /* XNL_DEBUG */

    if (num_substitutions == 0)
	merged_substitutions = defaultSubs;
    else {
	int i = XtNumber(defaultSubs);
	Substitution sub, def;
	merged_substitutions = sub = (Substitution)
	    ALLOCATE_LOCAL((unsigned)(num_substitutions+i)*sizeof(SubstitutionRec));
	if (sub == NULL) _XtAllocError(NULL);
	for (def = defaultSubs; i--; sub++, def++) sub->match = def->match;
	for (i = num_substitutions; i--; ) *sub++ = *substitutions++;
    }
    merged_substitutions[0].substitution = (String)filename;
    merged_substitutions[1].substitution = (String)type;
    merged_substitutions[2].substitution = (String)suffix;
    name_list[0] = pd->name;
    name_list[1] = XrmPermStringToQuark("customization");
    name_list[2] = NULLQUARK;
    class_list[0] = pd->class;
    class_list[1] = XrmPermStringToQuark("Customization");
    class_list[2] = NULLQUARK;
    if (XrmQGetResource(XrmGetDatabase(dpy), name_list, class_list,
			&db_type, &value) &&
	db_type == _XtQString)
	merged_substitutions[3].substitution = (char *)value.addr;
    else
	merged_substitutions[3].substitution = NULL;
    FillInLangSubs(&merged_substitutions[4], pd);

    result = XtFindFile(massagedPath, merged_substitutions,
			num_substitutions + XtNumber(defaultSubs),
			predicate);

    if (merged_substitutions[5].substitution != NULL)
	XtFree( (XtPointer)merged_substitutions[5].substitution );

    if (merged_substitutions != defaultSubs) 
	DEALLOCATE_LOCAL(merged_substitutions);

    if (pathMallocd)
	XtFree(massagedPath);
    else
	DEALLOCATE_LOCAL(massagedPath);

    return result;
}


Boolean XtCallAcceptFocus(widget, time)
    Widget widget;
    Time *time;
{
    XtAcceptFocusProc ac = XtClass(widget)->core_class.accept_focus;

    if (ac != NULL) return (*ac) (widget, time);
    else return FALSE;
}
