/* $XConsortium: Initialize.c,v 1.201 92/06/08 11:15:22 converse Exp $ */

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

/* Make sure all wm properties can make it out of the resource manager */

#include "IntrinsicI.h"
#include "StringDefs.h"
#include "CoreP.h"
#include "ShellP.h"
#include <pwd.h>
#include <stdio.h>
#include <X11/Xlocale.h>

#if __STDC__
#define Const const
#else
#define Const /**/
#endif

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *getenv();
#endif

extern void _XtConvertInitialize();

#if (defined(SUNSHLIB) || defined(AIXSHLIB)) && defined(SHAREDCODE)
/*
 * If used as a shared library, generate code under a different name so that
 * the stub routines in sharedlib.c get loaded into the application binary.
 */
#define XtToolkitInitialize _XtToolkitInitialize
#define XtAppInitialize _XtAppInitialize
#define XtInitialize _XtInitialize
#endif /* (SUNSHLIB || AIXSHLIB) && SHAREDCODE */

/*
 * hpux
 * Hand-patched versions of HP-UX prior to version 7.0 can usefully add
 * -DUSE_UNAME in the appropriate config file to get long hostnames.
 */

#ifdef USG
#define USE_UNAME
#endif

#ifdef USE_UNAME
#include <sys/utsname.h>
#endif

/* some unspecified magic number of expected search levels for Xrm */
#define SEARCH_LIST_SIZE 1000

/*
 This is a set of default records describing the command line arguments that
 Xlib will parse and set into the resource data base.
 
 This list is applied before the users list to enforce these defaults.  This is
 policy, which the toolkit avoids but I hate differing programs at this level.
*/

static XrmOptionDescRec Const opTable[] = {
{"+rv",		"*reverseVideo", XrmoptionNoArg,	(XtPointer) "off"},
{"+synchronous","*synchronous",	XrmoptionNoArg,		(XtPointer) "off"},
{"-background",	"*background",	XrmoptionSepArg,	(XtPointer) NULL},
{"-bd",		"*borderColor",	XrmoptionSepArg,	(XtPointer) NULL},
{"-bg",		"*background",	XrmoptionSepArg,	(XtPointer) NULL},
{"-bordercolor","*borderColor",	XrmoptionSepArg,	(XtPointer) NULL},
{"-borderwidth",".borderWidth",	XrmoptionSepArg,	(XtPointer) NULL},
{"-bw",		".borderWidth",	XrmoptionSepArg,	(XtPointer) NULL},
{"-display",	".display",     XrmoptionSepArg,	(XtPointer) NULL},
{"-fg",		"*foreground",	XrmoptionSepArg,	(XtPointer) NULL},
{"-fn",		"*font",	XrmoptionSepArg,	(XtPointer) NULL},
{"-font",	"*font",	XrmoptionSepArg,	(XtPointer) NULL},
{"-foreground",	"*foreground",	XrmoptionSepArg,	(XtPointer) NULL},
{"-geometry",	".geometry",	XrmoptionSepArg,	(XtPointer) NULL},
{"-iconic",	".iconic",	XrmoptionNoArg,		(XtPointer) "on"},
{"-name",	".name",	XrmoptionSepArg,	(XtPointer) NULL},
{"-reverse",	"*reverseVideo", XrmoptionNoArg,	(XtPointer) "on"},
{"-rv",		"*reverseVideo", XrmoptionNoArg,	(XtPointer) "on"},
{"-selectionTimeout",
		".selectionTimeout", XrmoptionSepArg,	(XtPointer) NULL},
{"-synchronous","*synchronous",	XrmoptionNoArg,		(XtPointer) "on"},
{"-title",	".title",	XrmoptionSepArg,	(XtPointer) NULL},
{"-xnllanguage",".xnlLanguage",	XrmoptionSepArg,	(XtPointer) NULL},
{"-xrm",	NULL,		XrmoptionResArg,	(XtPointer) NULL},
};


/*
 * _XtGetHostname - emulates gethostname() on non-bsd systems.
 */

static int _XtGetHostname (buf, maxlen)
    char *buf;
    int maxlen;
{
    int len;

#ifdef USE_UNAME
    struct utsname name;

    uname (&name);
    len = strlen (name.nodename);
    if (len >= maxlen) len = maxlen - 1;
    (void) strncpy (buf, name.nodename, len);
    buf[len] = '\0';
#else
    buf[0] = '\0';
    (void) gethostname (buf, maxlen);
    buf [maxlen - 1] = '\0';
    len = strlen(buf);
#endif
    return len;
}


#ifdef SUNSHLIB
void _XtInherit()
{
    extern void __XtInherit();
    __XtInherit();
}
#define _XtInherit __XtInherit
#endif

void _XtInherit()
{
    XtErrorMsg("invalidProcedure","inheritanceProc",XtCXtToolkitError,
            "Unresolved inheritance operation",
              (String *)NULL, (Cardinal *)NULL);
}


void XtToolkitInitialize()
{
    extern void _XtResourceListInitialize();

    /* Resource management initialization */
    XrmInitialize();
    _XtResourceListInitialize();

    /* Other intrinsic intialization */
    _XtConvertInitialize();
    _XtEventInitialize();
    _XtTranslateInitialize();
}


static String XtGetRootDirName(buf)
     String buf;
{
#ifndef X_NOT_POSIX
     uid_t uid;
#else
     int uid;
     extern int getuid();
#ifndef SYSV386
     extern struct passwd *getpwuid(), *getpwnam();
#endif
#endif
     struct passwd *pw;
     static char *ptr = NULL;

     if (ptr == NULL) {
	if (!(ptr = getenv("HOME"))) {
	    if (ptr = getenv("USER")) pw = getpwnam(ptr);
	    else {
		uid = getuid();
 		pw = getpwuid(uid);
	    }
	    if (pw) ptr = pw->pw_dir;
	    else {
		ptr = NULL;
		*buf = '\0';
	    }
	}
     }

     if (ptr)
 	(void) strcpy(buf, ptr);

     buf += strlen(buf);
     *buf = '/';
     buf++;
     *buf = '\0';
     return buf;
}

static void CombineAppUserDefaults(dpy, pdb)
    Display *dpy;
    XrmDatabase *pdb;
{
    char* filename;
    char* path;
    Boolean deallocate = False;

    if (!(path = getenv("XUSERFILESEARCHPATH"))) {
	char *old_path;
	char homedir[PATH_MAX];
	XtGetRootDirName(homedir);
	if (!(old_path = getenv("XAPPLRESDIR"))) {
	    char *path_default = "%s/%%L/%%N%%C:%s/%%l/%%N%%C:%s/%%N%%C:%s/%%L/%%N:%s/%%l/%%N:%s/%%N";
	    if (!(path =
		  ALLOCATE_LOCAL(6*strlen(homedir) + strlen(path_default))))
		_XtAllocError(NULL);
	    sprintf( path, path_default,
		    homedir, homedir, homedir, homedir, homedir, homedir );
	} else {
	    char *path_default = "%s/%%L/%%N%%C:%s/%%l/%%N%%C:%s/%%N%%C:%s/%%N%%C:%s/%%L/%%N:%s/%%l/%%N:%s/%%N:%s/%%N";
	    if (!(path =
		  ALLOCATE_LOCAL( 6*strlen(old_path) + 2*strlen(homedir)
				 + strlen(path_default))))
		_XtAllocError(NULL);
	    sprintf(path, path_default, old_path, old_path, old_path, homedir,
		    old_path, old_path, old_path, homedir );
	}
	deallocate = True;
    }

    filename = XtResolvePathname(dpy, NULL, NULL, NULL, path, NULL, 0, NULL);
    if (filename) {
	(void)XrmCombineFileDatabase(filename, pdb, False);
	XtFree(filename);
    }

    if (deallocate) DEALLOCATE_LOCAL(path);
}

static void CombineUserDefaults(dpy, pdb)
    Display *dpy;
    XrmDatabase *pdb;
{
    char *dpy_defaults = XResourceManagerString(dpy);

    if (dpy_defaults) {
	XrmCombineDatabase(XrmGetStringDatabase(dpy_defaults), pdb, False);
    } else {
	char filename[PATH_MAX];
	(void) XtGetRootDirName(filename);
	(void) strcat(filename, ".Xdefaults");
	(void)XrmCombineFileDatabase(filename, pdb, False);
    }
}

/*ARGSUSED*/
static Bool StoreDBEntry(db, bindings, quarks, type, value, data)
    XrmDatabase		*db;
    XrmBindingList      bindings;
    XrmQuarkList	quarks;
    XrmRepresentation   *type;
    XrmValuePtr		value;
    XPointer		data;
{
    XrmQPutResource((XrmDatabase *)data, bindings, quarks, *type, value);
    return False;
}

static XrmDatabase CopyDB(db)
    XrmDatabase db;
{
    XrmDatabase copy = NULL;
    XrmQuark empty = NULLQUARK;

    XrmEnumerateDatabase(db, &empty, &empty, XrmEnumAllLevels,
			 StoreDBEntry, (XPointer)&copy);
    return copy;
}

/*ARGSUSED*/
static String _XtDefaultLanguageProc(dpy, xnl, closure)
    Display   *dpy;	/* unused */
    String     xnl;
    XtPointer  closure;	/* unused */
{
    if (! setlocale(LC_ALL, xnl))
	XtWarning("locale not supported by C library, locale unchanged");

    if (! XSupportsLocale()) {
	XtWarning("locale not supported by Xlib, locale set to C");
	setlocale(LC_ALL, "C");
    }
    if (! XSetLocaleModifiers(""))
	XtWarning("X locale modifiers not supported, using default");

    return setlocale(LC_ALL, NULL); /* re-query in case overwritten */
}

#if NeedFunctionPrototypes
XtLanguageProc XtSetLanguageProc(
    XtAppContext      app,
    XtLanguageProc    proc,
    XtPointer         closure
    )
#else
XtLanguageProc XtSetLanguageProc(app, proc, closure)
    XtAppContext      app;
    XtLanguageProc    proc;
    XtPointer         closure;
#endif
{
    XtLanguageProc    old;

    if (!proc) {
	proc = _XtDefaultLanguageProc;
	closure = NULL;
    }

    if (app) {
	/* set langProcRec only for this application context */
        old = app->langProcRec.proc;
        app->langProcRec.proc = proc;
        app->langProcRec.closure = closure;
    } else {    
	/* set langProcRec for all application contexts */
        ProcessContext process = _XtGetProcessContext();

        old = process->globalLangProcRec.proc;
	process->globalLangProcRec.proc = proc;
	process->globalLangProcRec.closure = closure;
        app = process->appContextList;
        while (app) {
            app->langProcRec.proc = proc;
            app->langProcRec.closure = closure;
	    app = app->next;
        }
    }
    return (old ? old : _XtDefaultLanguageProc);
}

XrmDatabase XtScreenDatabase(screen)
    Screen *screen;
{
    Display *dpy;
    int scrno;
    Bool doing_def;
    XrmDatabase db, olddb;
    XtPerDisplay pd;
    Status do_fallback;
    char *scr_resources;

    dpy = DisplayOfScreen(screen);
    if (screen == DefaultScreenOfDisplay(dpy)) {
	scrno = DefaultScreen(dpy);
	doing_def = True;
    } else {
	scrno = XScreenNumberOfScreen(screen);
	doing_def = False;
    }
    pd = _XtGetPerDisplay(dpy);
    if (db = pd->per_screen_db[scrno])
	return db;
    scr_resources = XScreenResourceString(screen);

    if (ScreenCount(dpy) == 1) {
	db = pd->cmd_db;
	pd->cmd_db = NULL;
    } else {
	db = CopyDB(pd->cmd_db);
    }
    {   /* Environment defaults */
	char	filenamebuf[PATH_MAX];
	char	*filename;

	if (!(filename = getenv("XENVIRONMENT"))) {
	    int len;
	    (void) XtGetRootDirName(filename = filenamebuf);
	    (void) strcat(filename, ".Xdefaults-");
	    len = strlen(filename);
	    (void) _XtGetHostname (filename+len, PATH_MAX-len);
	}
	(void)XrmCombineFileDatabase(filename, &db, False);
    }
    if (scr_resources)
    {   /* Screen defaults */
	XrmCombineDatabase(XrmGetStringDatabase(scr_resources), &db, False);
	XFree(scr_resources);
    }
    /* Server or host defaults */
    if (!pd->server_db)
	CombineUserDefaults(dpy, &db);
    else {
	(void) XrmCombineDatabase(pd->server_db, &db, False);
	pd->server_db = NULL;
    }

    if (!db)
	db = XrmGetStringDatabase("");
    pd->per_screen_db[scrno] = db;
    olddb = XrmGetDatabase(dpy);
    /* set database now, for XtResolvePathname to use */
    XrmSetDatabase(dpy, db);
    CombineAppUserDefaults(dpy, &db);
    do_fallback = 1;
    {   /* System app-defaults */
	char	*filename;

	if (filename = XtResolvePathname(dpy, "app-defaults",
					 NULL, NULL, NULL, NULL, 0, NULL)) {
	    do_fallback = !XrmCombineFileDatabase(filename, &db, False);
	    XtFree(filename);
	}
    }
    /* now restore old database, if need be */
    if (!doing_def)
	XrmSetDatabase(dpy, olddb);
    if (do_fallback && pd->appContext->fallback_resources)
    {   /* Fallback defaults */
        XrmDatabase fdb = NULL;
	String *res;

	for (res = pd->appContext->fallback_resources; *res; res++)
	    XrmPutLineResource(&fdb, *res);
	(void)XrmCombineDatabase(fdb, &db, False);
    }
    return db;
}

/*
 * Merge two option tables, allowing the second to over-ride the first,
 * so that ambiguous abbreviations can be noticed.  The merge attempts
 * to make the resulting table lexicographically sorted, but succeeds
 * only if the first source table is sorted.  Though it _is_ recommended
 * (for optimizations later in XrmParseCommand), it is not required
 * that either source table be sorted.
 *
 * Caller is responsible for freeing the returned option table.
 */

static void _MergeOptionTables(src1, num_src1, src2, num_src2, dst, num_dst)
    XrmOptionDescRec *src1, *src2;
    Cardinal num_src1, num_src2;
    XrmOptionDescRec **dst;
    Cardinal *num_dst;
{
    XrmOptionDescRec *table, *endP;
    register XrmOptionDescRec *opt1, *opt2, *whereP, *dstP; 
    int i1, i2, dst_len, order;
    Boolean found;
    enum {Check, NotSorted, IsSorted} sort_order = Check;

    *dst = table = (XrmOptionDescRec*)
	XtMalloc( sizeof(XrmOptionDescRec) * (num_src1 + num_src2) );

    bcopy( src1, table, sizeof(XrmOptionDescRec) * num_src1 );
    if (num_src2 == 0) {
	*num_dst = num_src1;
	return;
    }
    endP = &table[dst_len = num_src1];
    for (opt2 = src2, i2= 0; i2 < num_src2; opt2++, i2++) {
	found = False;
	whereP = endP-1;	/* assume new option goes at the end */
	for (opt1 = table, i1 = 0; i1 < dst_len; opt1++, i1++) {
	    /* have to walk the entire new table so new list is ordered
	       (if src1 was ordered) */
	    if (sort_order == Check && i1 > 0
		&& strcmp(opt1->option, (opt1-1)->option) < 0)
		sort_order = NotSorted;
	    if ((order = strcmp(opt1->option, opt2->option)) == 0) {
		/* same option names; just overwrite opt1 with opt2 */
		*opt1 = *opt2;
		found = True;
		break;
		}
	    /* else */
	    if (sort_order == IsSorted && order > 0) {
		/* insert before opt1 to preserve order */
		/* shift rest of table forward to make room for new entry */
		for (dstP = endP++; dstP > opt1; dstP--)
		    *dstP = *(dstP-1);
		*opt1 = *opt2;
		dst_len++;
		found = True;
		break;
	    }
	    /* else */
	    if (order < 0)
		/* opt2 sorts after opt1, so remember this position */
		whereP = opt1;
	}
	if (sort_order == Check && i1 == dst_len)
	    sort_order = IsSorted;
	if (!found) {
	   /* when we get here, whereP points to the last entry in the
	      destination that sorts before "opt2".  Shift rest of table
	      forward and insert "opt2" after whereP. */
	    whereP++;
	    for (dstP = endP++; dstP > whereP; dstP--)
		*dstP = *(dstP-1);
	    *whereP = *opt2;
	    dst_len++;
	}
    }
    *num_dst = dst_len;
}


/* NOTE: name, class, and type must be permanent strings */
static Boolean _GetResource(dpy, list, name, class, type, value)
    Display *dpy;
    XrmSearchList list;
    String name, class, type;
    XrmValue* value;
{
    XrmRepresentation db_type;
    XrmValue db_value;
    XrmName Qname = XrmPermStringToQuark(name);
    XrmClass Qclass = XrmPermStringToQuark(class);
    XrmRepresentation Qtype = XrmPermStringToQuark(type);

    if (XrmQGetSearchResource(list, Qname, Qclass, &db_type, &db_value)) {
	if (db_type == Qtype) {
	    if (Qtype == _XtQString)
		*(String*)value->addr = db_value.addr;
	    else
		bcopy( db_value.addr, value->addr, value->size );
	    return True;
	} else {
	    WidgetRec widget; /* hack, hack */
	    bzero( &widget, sizeof(widget) );
	    widget.core.self = &widget;
	    widget.core.widget_class = coreWidgetClass;
	    widget.core.screen = (Screen*)DefaultScreenOfDisplay(dpy);
	    XtInitializeWidgetClass(coreWidgetClass);
	    if (_XtConvert(&widget,db_type,&db_value,Qtype,value,NULL)) {
		return True;
	    }
	}
    }
    return False;
}

XrmDatabase _XtPreparseCommandLine(urlist, num_urs, argc, argv, applName,
				   displayName, language)
    XrmOptionDescRec *urlist;
    Cardinal num_urs;
    int argc;
    String *argv;
    String *applName, *displayName, *language;	/* return */
{
    XrmDatabase db = 0;
    XrmOptionDescRec *options;
    Cardinal num_options;
    XrmName name_list[3];
    XrmName class_list[3];
    XrmRepresentation type;
    XrmValue val;
    String *targv;
    int targc = argc;

    targv = (String *) XtMalloc(sizeof(char *) * argc);
    bcopy(argv, targv, sizeof(char *) * argc);
    _MergeOptionTables(opTable, XtNumber(opTable), urlist, num_urs,
		       &options, &num_options);
    name_list[0] = class_list[0] = XrmPermStringToQuark(".");
    name_list[2] = class_list[2] = NULLQUARK;
    XrmParseCommand(&db, options, num_options, ".", &targc, targv);
    if (applName) {
	name_list[1] = XrmPermStringToQuark("name");
	if (XrmQGetResource(db, name_list, name_list, &type, &val) &&
	    type == _XtQString)
	    *applName = val.addr;
    }
    if (displayName) {
	name_list[1] = XrmPermStringToQuark("display");
	if (XrmQGetResource(db, name_list, name_list, &type, &val) &&
	    type == _XtQString)
	    *displayName = val.addr;
    }
    if (language) {
	name_list[1] = XrmPermStringToQuark("xnlLanguage");
	class_list[1] = XrmPermStringToQuark("XnlLanguage");
	if (XrmQGetResource(db, name_list, class_list, &type, &val) &&
	    type == _XtQString)
	    *language = val.addr;
    }

    XtFree((char *)targv);
    XtFree((char *)options);
    return db;
}

  
static void GetLanguage(dpy, pd)
    Display *dpy;
    XtPerDisplay pd;
{
    XrmRepresentation type;
    XrmValue value;
    XrmName name_list[3];
    XrmName class_list[3];

    if (! pd->language) {
	name_list[0] = pd->name;
	name_list[1] = XrmPermStringToQuark("xnlLanguage");
	class_list[0] = pd->class;
	class_list[1] = XrmPermStringToQuark("XnlLanguage");
	name_list[2] = class_list[2] = NULLQUARK;
	if (!pd->server_db)
	    CombineUserDefaults(dpy, &pd->server_db);
	if (pd->server_db &&
	    XrmQGetResource(pd->server_db,name_list,class_list, &type, &value)
	    && type == _XtQString)
	    pd->language = (char *) value.addr;
    }

    if (pd->appContext->langProcRec.proc) {
	if (! pd->language) pd->language = "";
	pd->language = (*pd->appContext->langProcRec.proc)
	    (dpy, pd->language, pd->appContext->langProcRec.closure);
    }
    else if (! pd->language || pd->language[0] == '\0') /* R4 compatibility */
	pd->language = getenv("LANG");

    if (pd->language) pd->language = XtNewString(pd->language);
}


#if NeedFunctionPrototypes
void _XtDisplayInitialize(
	Display *dpy,
        XtPerDisplay pd,
	_Xconst char* name,
	XrmOptionDescRec *urlist,
	Cardinal num_urs,
	int *argc,
	char **argv)
#else
void _XtDisplayInitialize(dpy, pd, name, urlist, num_urs, argc, argv)
	Display *dpy;
        XtPerDisplay pd;
	String name;
	XrmOptionDescRec *urlist;
	Cardinal num_urs;
	int *argc;
	char **argv;
#endif
{
	Boolean tmp_bool;
	XrmValue value;
	XrmOptionDescRec *options;
	Cardinal num_options;
	XrmDatabase db;
	XrmName name_list[2];
	XrmClass class_list[2];
	XrmHashTable* search_list;
	int search_list_size = SEARCH_LIST_SIZE;

	GetLanguage(dpy, pd);

	/* Parse the command line and remove Xt arguments from argv */
	_MergeOptionTables( opTable, XtNumber(opTable), urlist, num_urs,
			    &options, &num_options );
	XrmParseCommand(&pd->cmd_db, options, num_options, name, argc, argv);

	db = XtScreenDatabase(DefaultScreenOfDisplay(dpy));

	if (!(search_list = (XrmHashTable*)
		       ALLOCATE_LOCAL( SEARCH_LIST_SIZE*sizeof(XrmHashTable))))
	    _XtAllocError(NULL);
	name_list[0] = pd->name;
	class_list[0] = pd->class;
	name_list[1] = NULLQUARK;
	class_list[1] = NULLQUARK;

	while (!XrmQGetSearchList(db, name_list, class_list,
				  search_list, search_list_size)) {
	    XrmHashTable* old = search_list;
	    Cardinal size = (search_list_size*=2)*sizeof(XrmHashTable);
	    if (!(search_list = (XrmHashTable*)ALLOCATE_LOCAL(size)))
		_XtAllocError(NULL);
	    bcopy( (char*)old, (char*)search_list, (size>>1) );
	    DEALLOCATE_LOCAL(old);
	}

	value.size = sizeof(tmp_bool);
	value.addr = (XtPointer)&tmp_bool;
	if (_GetResource(dpy, search_list, "synchronous", "Synchronous",
			 XtRBoolean, &value)) {
	    int i;
	    Display **dpyP = pd->appContext->list;
	    pd->appContext->sync = tmp_bool;
	    for (i = pd->appContext->count; i; dpyP++, i--) {
		(void) XSynchronize(*dpyP, (Bool)tmp_bool);
	    }
	} else {
	    (void) XSynchronize(dpy, (Bool)pd->appContext->sync);
	}

	if (_GetResource(dpy, search_list, "reverseVideo", "ReverseVideo",
			 XtRBoolean, &value)
	        && tmp_bool) {
	    pd->rv = True;
	}

	value.size = sizeof(pd->multi_click_time);
	value.addr = (XtPointer)&pd->multi_click_time;
	if (!_GetResource(dpy, search_list,
			  "multiClickTime", "MultiClickTime",
			  XtRInt, &value)) {
	    pd->multi_click_time = 200;
	}

	value.size = sizeof(pd->appContext->selectionTimeout);
	value.addr = (XtPointer)&pd->appContext->selectionTimeout;
	(void)_GetResource(dpy, search_list,
			   "selectionTimeout", "SelectionTimeout",
			   XtRInt, &value);

#ifndef NO_IDENTIFY_WINDOWS
	value.size = sizeof(pd->appContext->identify_windows);
	value.addr = (XtPointer)&pd->appContext->identify_windows;
	(void)_GetResource(dpy, search_list,
			   "xtIdentifyWindows", "XtDebug",
			   XtRBoolean, &value);
#endif

	XtFree( (XtPointer)options );
	DEALLOCATE_LOCAL( search_list );
}

/*	Function Name: XtAppSetFallbackResources
 *	Description: Sets the fallback resource list that will be loaded
 *                   at display initialization time.
 *	Arguments: app_context - the app context.
 *                 specification_list - the resource specification list.
 *	Returns: none.
 */

#if NeedFunctionPrototypes
void
XtAppSetFallbackResources(
XtAppContext app_context,
String *specification_list
)
#else
void
XtAppSetFallbackResources(app_context, specification_list)
XtAppContext app_context;
String *specification_list;
#endif
{
    app_context->fallback_resources = specification_list;
}

/*	Function Name: XtAppInitialize
 *	Description: A convience routine for Initializing the toolkit.
 *	Arguments: app_context_return - The application context of the
 *                                      application
 *                 application_class  - The class of the application.
 *                 options            - The option list.
 *                 num_options        - The number of options in the above list
 *                 argc_in_out, argv_in_out - number and list of command line
 *                                            arguments.
 *                 fallback_resource  - The fallback list of resources.
 *                 args, num_args     - Arguements to use when creating the 
 *                                      shell widget.
 *	Returns: The shell widget.
 */
	
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
    XtAppContext app_con;
    Display * dpy;
    register int saved_argc = *argc_in_out;
    Widget root;
    Arg args[3], *merged_args;
    Cardinal num = 0;
    
    XtToolkitInitialize(); /* cannot be moved into _XtAppInit */
    
    dpy = _XtAppInit(&app_con, (String)application_class, options, num_options,
		     argc_in_out, &argv_in_out, fallback_resources);

    XtSetArg(args[num], XtNscreen, DefaultScreenOfDisplay(dpy)); num++;
    XtSetArg(args[num], XtNargc, saved_argc);	                 num++;
    XtSetArg(args[num], XtNargv, argv_in_out);	                 num++;

    merged_args = XtMergeArgLists(args_in, num_args_in, args, num);
    num += num_args_in;

    root = XtAppCreateShell(NULL, application_class, 
			    applicationShellWidgetClass,dpy, merged_args, num);
    
    if (app_context_return)
	*app_context_return = app_con;

    XtFree((XtPointer)merged_args);
    XtFree((XtPointer)argv_in_out);
    return(root);
}

/*	Function Name: XtInitialize
 *	Description: This function can be used to initialize the toolkit.
 *		     It is obsolete; XtAppInitialize is more useful.
 *	Arguments: name - ** UNUSED **
 *                 classname - name of the application class.
 *                 options, num_options - the command line option info.
 *                 argc, argc - the command line args from main().
 *	Returns: a shell widget.
 */
	
/*ARGSUSED*/
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
    Widget root;
    XtAppContext app_con;
    register ProcessContext process = _XtGetProcessContext();

    root = XtAppInitialize(&app_con, classname, options, num_options,
			   argc, argv, NULL, NULL, (Cardinal) 0);

    process->defaultAppContext = app_con;
    
    return(root);
}
