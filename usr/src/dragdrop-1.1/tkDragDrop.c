/*
 * ------------------------------------------------------------------------
 *  APPLICATION:  tkDragDrop.c
 *      PURPOSE:  drag & drop widget registration facility
 *
 *  Allows widgets to be registered as drag&drop sources and targets
 *  for handling "drag-and-drop" operations between applications.
 *
 *  USAGE:
 *    drag&drop source
 *    drag&drop source <pathName> ?options...?
 *    drag&drop target
 *    drag&drop target <pathName> handler
 *    drag&drop target <pathName> handler {<dataType> <command>}...
 *    drag&drop target <pathName> handle <dataType>
 *
 *    drag&drop drag <pathName> <x> <y>
 *    drag&drop drop <pathName> <x> <y>
 *    drag&drop active
 *    drag&drop location ?<x> <y>?
 *
 * ------------------------------------------------------------------------
 *  AUTHOR:  Michael J. McLennan       Phone: (215)770-2842
 *           AT&T Bell Laboratories   E-mail: alux2!mmc@att.com
 *
 *    SCCS:  %W% (%G%)
 * ========================================================================
 *                 Copyright (c) 1992  AT&T Bell Laboratories
 * ========================================================================
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that the copyright notice and warranty disclaimer appear in
 * supporting documentation, and that the names of AT&T Bell Laboratories
 * any of their entities not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.
 * 
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data or
 * profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 * ========================================================================
 */

#include "tk.h"
#include "tclHash.h"
#include "X11/Xatom.h"

/*
 *  -------------  The following color definitions are taken directly
 *  >> WARNING <<  from "default.h" in the Tk distribution, and should
 *  -------------  be kept up to date with those conventions.
 */
#define BLACK       "Black"
#define WHITE       "White"
#define GRAY        "#b0b0b0"

#define BISQUE1     "#ffe4c4"
#define BISQUE2     "#eed5b7"
#define BISQUE3     "#cdb79e"

#define DEF_BUTTON_ACTIVE_BG_COLOR  BISQUE2
#define DEF_BUTTON_ACTIVE_BG_MONO   BLACK
#define DEF_BUTTON_ACTIVE_FG_COLOR  BLACK
#define DEF_BUTTON_ACTIVE_FG_MONO   WHITE
#define DEF_BUTTON_BG_COLOR     BISQUE1
#define DEF_BUTTON_BG_MONO      WHITE

/*
 *  DRAG&DROP REGISTRATION DATA
 */
typedef struct {
	Tcl_Interp *interp;       /* interpreter managing this drag&drop command */
	Tk_Window root;           /* main window for application */
	Tcl_HashTable srcList;    /* list of source widgets */
	Tcl_HashTable trgList;    /* list of target widgets */
	int numactive;            /* number of active drag&drop operations */
	int locx, locy;           /* last location point */
} DD_RegList;

typedef struct {
	DD_RegList *ddlist;       /* parent registration list */
	Tk_Window tkwin;          /* registered window */
} DD_RegEntry;

/*
 *  DRAG&DROP SOURCE REGISTRATION RECORD
 */
typedef struct {
	Tcl_Interp *interp;           /* interpreter for registered widget */

	Display *display;             /* drag&drop source window display */
	Tk_Window tkwin;              /* drag&drop source window */
	int button;                   /* button used to invoke drag for sources */

	Tk_Window tokenwin;           /* window representing drag item */
	Tk_3DBorder tokenNormBorder;  /* border/background (inactive) */
	Tk_3DBorder tokenActBorder;   /* border/background (active) */
	int tokenBorderWidth;         /* border width in pixels */
	int tokenNormRelief;          /* border relief (inactive) */
	int tokenActRelief;           /* border relief (active) */
	int overTargetWin;            /* non-zero => over target window */
	int tokenx, tokeny;           /* last position of token window */
	Tk_TimerToken hidetoken;      /* token for routine to hide tokenwin */
	XColor *rejectFg;             /* color used to draw rejection fg: (\) */
	XColor *rejectBg;             /* color used to draw rejection bg: (\) */
	Pixmap rejectSt;              /* stipple used to draw rejection: (\) */
	GC rejectFgGC;                /* GC used to draw rejection fg: (\) */
	GC rejectBgGC;                /* GC used to draw rejection bg: (\) */
	int tokencmdInProg;           /* non-zero => executing tokencmd */

	char *tokencmd;               /* command executed before send by sender */
	char *tokencmdResult;         /* result returned by recent tokencmd */
	char *sendcmd;                /* command executed to send data by sender */
} DD_Source;

/*
 *  CONFIG PARAMETERS
 */
static Tk_ConfigSpec SourceConfigSpecs[] = {

	{TK_CONFIG_INT,
		"-button", "buttonBinding", "ButtonBinding",
		"3", Tk_Offset(DD_Source, button),
		0},

	{TK_CONFIG_COLOR,
		"-rejectbg", "rejectBackground", "Background",
		DEF_BUTTON_BG_COLOR, Tk_Offset(DD_Source, rejectBg),
		TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,
		"-rejectbg", "rejectBackground", "Background",
		"white", Tk_Offset(DD_Source, rejectBg),
		TK_CONFIG_MONO_ONLY},

	{TK_CONFIG_COLOR,
		"-rejectfg", "rejectForeground", "Foreground",
		"red", Tk_Offset(DD_Source, rejectFg),
		TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_COLOR,
		"-rejectfg", "rejectForeground", "Foreground",
		"black", Tk_Offset(DD_Source, rejectFg),
		TK_CONFIG_MONO_ONLY},

	{TK_CONFIG_BITMAP,
		"-rejectstipple", "rejectStipple", "Stipple",
		(char*)NULL, Tk_Offset(DD_Source, rejectSt),
		TK_CONFIG_COLOR_ONLY | TK_CONFIG_NULL_OK},
	{TK_CONFIG_BITMAP,
		"-rejectstipple", "rejectStipple", "Stipple",
		"gray50", Tk_Offset(DD_Source, rejectSt),
		TK_CONFIG_MONO_ONLY},

	{TK_CONFIG_STRING,
		"-sendcmd", "sendCommand", "Command",
		NULL, Tk_Offset(DD_Source, sendcmd),
		TK_CONFIG_NULL_OK},

	{TK_CONFIG_BORDER,
		"-tokenactivebg", "tokenActiveBackground", "Foreground",
		DEF_BUTTON_ACTIVE_BG_COLOR, Tk_Offset(DD_Source, tokenActBorder),
		TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_BORDER,
		"-tokenactivebg", "tokenActiveBackground", "Foreground",
		DEF_BUTTON_ACTIVE_BG_MONO, Tk_Offset(DD_Source, tokenActBorder),
		TK_CONFIG_MONO_ONLY},

	{TK_CONFIG_RELIEF,
		"-tokenactiverelief", "tokenActiveRelief", "ActiveRelief",
		"raised", Tk_Offset(DD_Source, tokenActRelief),
		0},

	{TK_CONFIG_PIXELS,
		"-tokenborderwidth", "tokenBorderWidth", "BorderWidth",
		"3", Tk_Offset(DD_Source, tokenBorderWidth),
		0},

	{TK_CONFIG_BORDER,
		"-tokenbg", "tokenBackground", "Background",
		DEF_BUTTON_BG_COLOR, Tk_Offset(DD_Source, tokenNormBorder),
		TK_CONFIG_COLOR_ONLY},
	{TK_CONFIG_BORDER,
		"-tokenbg", "tokenBackground", "Background",
		DEF_BUTTON_BG_MONO, Tk_Offset(DD_Source, tokenNormBorder),
		TK_CONFIG_MONO_ONLY},

	{TK_CONFIG_STRING,
		"-tokencmd", "tokenCommand", "Command",
		NULL, Tk_Offset(DD_Source, tokencmd),
		TK_CONFIG_NULL_OK},

	{TK_CONFIG_RELIEF,
		"-tokenrelief", "tokenRelief", "Relief",
		"flat", Tk_Offset(DD_Source, tokenNormRelief),
		0},

	{TK_CONFIG_END,
		(char*)NULL, (char*)NULL, (char*)NULL,
		(char*)NULL, 0,
		0},
};

/*
 *  CLASS NAME for token window
 */
static char *TokenClassName = "DragDrop";


/*
 *  DRAG&DROP TARGET REGISTRATION RECORD
 */
typedef struct DD_TargetHndl {
	char *dataType;               /* name of data type */
	char *command;                /* command to handle data type */
	struct DD_TargetHndl* next;   /* next handler in linked list */
} DD_TargetHndl;

typedef struct {
	Tcl_Interp *interp;           /* interpreter for registered widget */
	Display *display;             /* drag&drop target window display */
	Tk_Window tkwin;              /* drag&drop target window */
	DD_TargetHndl *handlers;      /* list of data handlers */
} DD_Target;

/*
 * Each "drag&drop" widget window is tagged with a "DragDropInfo"
 * property in XA_STRING format.  This property identifies the
 * window as a "drag&drop" widget, and contains the following:
 *
 *     "<drag&drop-path> <interp-name>"
 *
 * The <drag&drop-path> is the window path name of the drag&drop
 * widget, and <interp-name> is the name of the interpreter
 * controlling the drag&drop widget (useful for the "send" command).
 *
 * When the user invokes the "drop" operation on a drag&drop widget,
 * the widget tries to identify the window receiving the request.
 * It does this by querying the entire window tree from the root
 * window, walking down the tree to find the most specific window
 * containing the drop point, and checking to see if this window
 * is another drag&drop widget.  If it is, then the drop information
 * is created and sent to the application via one or more of the usual
 * "send" command.  If communication fails, the drag&drop facility
 * automatically posts a rejection symbol on the token window.
 */

/*
 *  Maximum size property that can be read at one time:
 */
#define MAX_PROP_SIZE 1000

/*
 *  FORWARD DECLARATIONS
 */
void Tk_AddDragDropCmd _ANSI_ARGS_((Tcl_Interp* interp, Tk_Window tkwin));
void Tk_DelDragDrop _ANSI_ARGS_((ClientData clientData));
int Tk_DragDropCmd _ANSI_ARGS_((ClientData clientData, Tcl_Interp *interp, int argc, char **argv));
static DD_Source* GetSourceInfo _ANSI_ARGS_((DD_RegList *ddlist, char *pathname, int *newEntry));
static void DestroySourceInfo _ANSI_ARGS_((DD_RegList *ddlist, char *pathName));
static int ConfigSource _ANSI_ARGS_((Tcl_Interp *interp, DD_Source *dsPtr, int argc, char **argv, int flags));
static void UnregSource _ANSI_ARGS_((ClientData clientData, XEvent *eventPtr));
static DD_Target* GetTargetInfo _ANSI_ARGS_((DD_RegList *ddlist, char *pathname, int *newEntry));
static void DestroyTargetInfo _ANSI_ARGS_((DD_RegList *ddlist, char *pathName));
static char* FindTargetHandler _ANSI_ARGS_((DD_Target* dtPtr, char *dtname));
static void PutTargetHandler _ANSI_ARGS_((DD_Target* dtPtr, char *dtname, char *cmd));
static DD_TargetHndl* CreateTargetHandler _ANSI_ARGS_((char *dtname, char *cmd));
static void DestroyTargetHandler _ANSI_ARGS_((DD_TargetHndl *dtHndl));
static void UnregTarget _ANSI_ARGS_((ClientData clientData, XEvent *eventPtr));
static void DragDropSend _ANSI_ARGS_((DD_Source *dsPtr, int x, int y));
static Window FindWinAtXY _ANSI_ARGS_((Display *disp, int x, int y, Window ignore));
static int GetDragDropInfo _ANSI_ARGS_((Display *disp, Window win, char *interpName, char *ddName));
static void UpdateTokenWindow _ANSI_ARGS_((ClientData clientData));
static void DDTokenEventProc _ANSI_ARGS_((ClientData, XEvent*));
static void HideDragDropToken _ANSI_ARGS_((ClientData clientData));
static void RejectDragDropToken _ANSI_ARGS_((DD_Source* dsPtr));


/*
 * ------------------------------------------------------------------------
 *  Tk_AddDragDropCmd()
 *
 *  Adds the "drag&drop" command to the given interpreter.  Should be
 *  invoked to properly install the command whenever a new interpreter
 *  is created.
 * ------------------------------------------------------------------------
 */
void
Tk_AddDragDropCmd(interp, tkwin)
	Tcl_Interp *interp;  /* interpreter to be updated */
	Tk_Window tkwin;     /* main window of application */
{
	DD_RegList *ddlist;

    ddlist = (DD_RegList*)ckalloc(sizeof(DD_RegList));
    ddlist->interp = interp;
    ddlist->root = tkwin;
    Tcl_InitHashTable(&ddlist->srcList,TCL_STRING_KEYS);
    Tcl_InitHashTable(&ddlist->trgList,TCL_STRING_KEYS);
    ddlist->numactive = 0;
    ddlist->locx = ddlist->locy = 0;

	Tcl_CreateCommand(interp, "drag&drop", Tk_DragDropCmd,
		(ClientData)ddlist, Tk_DelDragDrop);
}

/*
 * ------------------------------------------------------------------------
 *  Tk_DelDragDrop()
 *
 *  Invoked when the "drag&drop" command is removed from an interpreter
 *  to free up allocated memory.
 * ------------------------------------------------------------------------
 */
void
Tk_DelDragDrop(cdata)
	ClientData cdata;    /* client data for drag&drop command */
{
	DD_RegList *ddlist = (DD_RegList*)cdata;

    Tcl_DeleteHashTable(&ddlist->srcList);
    Tcl_DeleteHashTable(&ddlist->trgList);
	ckfree((char*)ddlist);
}

/*
 * ------------------------------------------------------------------------
 *  Tk_DragDropCmd()
 *
 *  Invoked by TCL whenever the user issues a "drag&drop" widget command.
 *  Handle the following syntax:
 *
 *    drag&drop source
 *    drag&drop source <pathName> ?options...?
 *    drag&drop target
 *    drag&drop target <pathName> handler
 *    drag&drop target <pathName> handler {<dataType> <command>}...
 *    drag&drop target <pathName> handle <dataType>
 *
 *    drag&drop drag <pathName> <x> <y>
 *    drag&drop drop <pathName> <x> <y>
 *    drag&drop active
 *    drag&drop location ?<x> <y>?
 *
 * ------------------------------------------------------------------------
 */
int
Tk_DragDropCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* main window associated with interp */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	DD_RegList *ddlist = (DD_RegList*)clientData;
	DD_RegEntry *ddentry;
	register DD_Source *dsPtr;
	register DD_Target *dtPtr;

	int status, length, x, y, max, newEntry;
	char c;

	Tk_Window tkwin, tokenwin;
	XSetWindowAttributes atts;
	Atom ddProperty;
	char buffer[MAX_PROP_SIZE];

	if (argc < 2)
	{
		sprintf(interp->result,
			"wrong # args: should be \"%s option ?args?\"", argv[0]);
		return TCL_ERROR;
	}
	c = argv[1][0];
	length = strlen(argv[1]);

	/*
	 *  HANDLE:  drag&drop source ?<pathName>? ?options...?
	 */
	if ((c == 's') && strncmp(argv[1], "source", length) == 0)
	{
		/*
		 *  HANDLE:  drag&drop source
		 */
		if (argc == 2)
		{
			Tcl_HashSearch pos;
			Tcl_HashEntry *entry = Tcl_FirstHashEntry(&ddlist->srcList,&pos);
			while (entry)
			{
				Tcl_AppendElement(interp,
					Tcl_GetHashKey(&ddlist->srcList,entry), 0);
				entry = Tcl_NextHashEntry(&pos);
			}
			return TCL_OK;
		}

		/*
		 *  HANDLE:  drag&drop source <pathName> ?options...?
		 */
		dsPtr = GetSourceInfo(ddlist,argv[2],&newEntry);
		dsPtr->interp = interp;
		dsPtr->tkwin  = Tk_NameToWindow(interp, argv[2], ddlist->root);

		if (!dsPtr->tkwin)
		{
			sprintf(interp->result, "window does not exist: %.50s",argv[2]);
			DestroySourceInfo(ddlist,argv[2]);
			return TCL_ERROR;
		}
		dsPtr->display = Tk_Display(dsPtr->tkwin);

		if (!newEntry)
		{
			if (argc == 3)
				status = Tk_ConfigureInfo(interp, dsPtr->tokenwin,
					SourceConfigSpecs, (char*)dsPtr, (char*)NULL, 0);

			else if (argc == 4)
				status = Tk_ConfigureInfo(interp, dsPtr->tokenwin,
					SourceConfigSpecs, (char*)dsPtr, argv[2], 0);

			else
				status = ConfigSource(interp, dsPtr, argc-3, argv+3,
					TK_CONFIG_ARGV_ONLY);
		}
		else
		{
			if (ConfigSource(interp, dsPtr, argc-3, argv+3, 0) != TCL_OK)
			{
				DestroySourceInfo(ddlist,argv[2]);
				return TCL_ERROR;
			}

			/*
			 *  Create the window for the drag&drop token...
			 */
			sprintf(buffer, "dd-token%x", (int)dsPtr);
			tokenwin = Tk_CreateWindow(dsPtr->interp, dsPtr->tkwin,
				buffer, "");

			if (!tokenwin)
			{
				sprintf(interp->result, "could not create token window");
				DestroySourceInfo(ddlist,argv[2]);
				return TCL_ERROR;
			}
			Tk_SetClass(tokenwin,TokenClassName);
			Tk_CreateEventHandler(tokenwin, ExposureMask|StructureNotifyMask,
				DDTokenEventProc, (ClientData)dsPtr);

			atts.override_redirect = True;
			atts.save_under = True;
			Tk_ChangeWindowAttributes(tokenwin,
				CWOverrideRedirect|CWSaveUnder, &atts);

			Tk_SetInternalBorder(tokenwin, 2*dsPtr->tokenBorderWidth);
			dsPtr->tokenwin = tokenwin;

			sprintf(buffer,
				"bind %s <ButtonPress-%d> {drag&drop drag %s %%X %%Y}; \
				bind %s <B%d-Motion> {drag&drop drag %s %%X %%Y}; \
				bind %s <ButtonRelease-%d> {drag&drop drop %s %%X %%Y}",
				argv[2], dsPtr->button, argv[2],
				argv[2], dsPtr->button, argv[2],
				argv[2], dsPtr->button, argv[2]);

			if (Tcl_Eval(interp, buffer, 0, (char**)NULL) != TCL_OK)
			{
				Tk_DestroyWindow(tokenwin);
				DestroySourceInfo(ddlist,argv[2]);
				return TCL_ERROR;
			}

			/*
			 *  Arrange for the window to unregister itself when it
			 *  is destroyed.
			 */
			ddentry = (DD_RegEntry*)ckalloc(sizeof(DD_RegEntry));
			ddentry->ddlist = ddlist;
			ddentry->tkwin = dsPtr->tkwin;
			Tk_CreateEventHandler(dsPtr->tkwin, StructureNotifyMask,
				UnregSource, (ClientData)ddentry);
		}
	}

	/*
	 *  HANDLE:  drag&drop target ?<pathName>? ?handling info...?
	 */
	else if ((c == 't') && strncmp(argv[1], "target", length) == 0)
	{
		/*
		 *  HANDLE:  drag&drop target
		 */
		if (argc == 2)
		{
			Tcl_HashSearch pos;
			Tcl_HashEntry *entry = Tcl_FirstHashEntry(&ddlist->trgList,&pos);
			while (entry)
			{
				Tcl_AppendElement(interp,
					Tcl_GetHashKey(&ddlist->trgList,entry), 0);
				entry = Tcl_NextHashEntry(&pos);
			}
			return TCL_OK;
		}
		else
		{
			dtPtr = GetTargetInfo(ddlist,argv[2],&newEntry);
			dtPtr->interp = interp;
			dtPtr->tkwin  = Tk_NameToWindow(interp, argv[2], ddlist->root);

			if (!dtPtr->tkwin)
			{
				sprintf(interp->result, "window does not exist: %.50s",argv[2]);
				DestroyTargetInfo(ddlist,argv[2]);
				return TCL_ERROR;
			}
			dtPtr->display = Tk_Display(dtPtr->tkwin);
		}

		/*
		 *  HANDLE:  drag&drop target <pathName> handler
		 *           drag&drop target <pathName> handler {<data> <cmd>}...
		 */
		if ((argc >= 4) && (strcmp(argv[3], "handler") == 0))
		{
			if (argc == 4)
			{
				DD_TargetHndl *dtHndl = dtPtr->handlers;
				while (dtHndl)
				{
					Tcl_AppendElement(interp, dtHndl->dataType, 0);
					dtHndl = dtHndl->next;
				}
				return TCL_OK;
			}

			/*
			 *  Attach a property to identify window as "drag&drop" target...
			 */
			if (newEntry)
			{
				if (Tcl_Eval(interp, "winfo name .", 0, (char**)NULL) == TCL_OK)
					sprintf(buffer, "%s}%s", interp->result, argv[2]);
				else
					sprintf(buffer, "}%s", argv[2]);

				ddProperty = XInternAtom(dtPtr->display, "DragDropInfo", False);

				Tk_MakeWindowExist(dtPtr->tkwin);
				XChangeProperty(dtPtr->display, Tk_WindowId(dtPtr->tkwin),
					ddProperty, XA_STRING, 8, PropModeReplace,
					(unsigned char*)buffer, strlen(buffer)+1);

				/*
				 *  Arrange for the window to unregister itself when it
				 *  is destroyed.
				 */
				ddentry = (DD_RegEntry*)ckalloc(sizeof(DD_RegEntry));
				ddentry->ddlist = ddlist;
				ddentry->tkwin = dtPtr->tkwin;
				Tk_CreateEventHandler(dtPtr->tkwin, StructureNotifyMask,
					UnregTarget, (ClientData)ddentry);
			}

			/*
			 *  Process handler definitions
			 */
			status = TCL_OK;
			for (x=4; (x < argc) && (status == TCL_OK); x++)
			{
				int elemc;
				char **elemv;
				if (Tcl_SplitList(interp, argv[x], &elemc, &elemv) == TCL_OK)
				{
					if (elemc == 2)
						PutTargetHandler(dtPtr, elemv[0], elemv[1]);
					else
						status = TCL_ERROR;

					free((char*)elemv);
				}
				else
					status = TCL_ERROR;

				if (status != TCL_OK)
					sprintf(interp->result, "bad target handler: {%s}\nshould be {name command}", argv[x]);
			}
			return status;
		}

		/*
		 *  HANDLE:  drag&drop target <pathName> handle <data>
		 */
		else if ((argc == 5) && (strcmp(argv[3], "handle") == 0))
		{
			char *cmd;
			if ((cmd=FindTargetHandler(dtPtr, argv[4])) != NULL)
				return Tcl_Eval(interp, cmd, 0, (char**)NULL);

			sprintf(interp->result,"target cannot handle datatype: %s",argv[4]);
			return TCL_ERROR;  /* no handler found */
		}
		else
		{
			sprintf(interp->result,"usage: %s target %s handler ?defns?\n   or: %s target %s handle <data>", argv[0], argv[2], argv[0], argv[2]);
			return TCL_ERROR;
		}
	}

	/*
	 *  HANDLE:  drag&drop drag <path> <x> <y>
	 */
	else if ((c == 'd') && strncmp(argv[1], "drag", length) == 0)
	{
		if (argc < 5)
		{
			sprintf(interp->result,
				"wrong # args: should be \"%s drag pathname x y\"",
				argv[0]);
			return TCL_ERROR;
		}

		dsPtr = GetSourceInfo(ddlist,argv[2],&newEntry);
		if (newEntry)
		{
			sprintf(interp->result, "not a drag&drop source: %.50s", argv[2]);
			DestroySourceInfo(ddlist,argv[2]);
			return TCL_ERROR;
		}
		if ((Tcl_GetInt(interp, argv[3], &x) != TCL_OK) ||
			(Tcl_GetInt(interp, argv[4], &y) != TCL_OK))
			return TCL_ERROR;

		sprintf(buffer, "drag&drop location %d %d", x, y);
		Tcl_Eval(dsPtr->interp, buffer, 0, (char**)NULL);

		/*
		 *  If HideDragDropToken() is pending, then do it now!
		 */
		if (dsPtr->hidetoken)
		{
			Tk_DeleteTimerHandler(dsPtr->hidetoken);
			HideDragDropToken((ClientData)dsPtr);
		}

		/*
		 *  If tokencmd is in progress, then ignore subsequent calls
		 *  until it completes.  Only perform drag if tokencmd
		 *  completed successfully and token window is mapped.
		 */
		if (!Tk_IsMapped(dsPtr->tokenwin) && !dsPtr->tokencmdInProg)
		{
			if (!dsPtr->tokencmd)
			{
				sprintf(interp->result, "drag&drop missing -tokencmd: %.50s",
					argv[2]);
				return TCL_ERROR;
			}
			sprintf(buffer, "%s %s", dsPtr->tokencmd,
				Tk_PathName(dsPtr->tokenwin));

			dsPtr->tokencmdInProg = ~0;
			status = Tcl_Eval(dsPtr->interp, buffer, 0, (char**)NULL);
			dsPtr->tokencmdInProg = 0;

			/*
			 *  Save result of token command for send command.
			 */
			if (dsPtr->tokencmdResult)
				ckfree(dsPtr->tokencmdResult);
			dsPtr->tokencmdResult
				= (char*)ckalloc((unsigned)(strlen(interp->result)+1));
			strcpy(dsPtr->tokencmdResult, interp->result);

			/*
			 *  Token building failed?  Then abort drag quietly
			 */
			if (status != TCL_OK)
				return TCL_OK;

			/*
			 *  Otherwise, map token window to begin drag operation.
			 */
			ddlist->numactive++;   /* one more drag&drop window active */
			Tk_MapWindow(dsPtr->tokenwin);
			XRaiseWindow(Tk_Display(dsPtr->tokenwin),
				Tk_WindowId(dsPtr->tokenwin));
		}

		/*
		 *  Arrange to update status of token window...
		 */
		Tk_CancelIdleCall(UpdateTokenWindow, (ClientData)dsPtr);
		Tk_DoWhenIdle(UpdateTokenWindow, (ClientData)dsPtr);
		dsPtr->tokenx = x;
		dsPtr->tokeny = y;

		/*
		 *  Move the token window to the current drag point...
		 */
		tkwin = ddlist->root;
		tokenwin = dsPtr->tokenwin;
		Tk_MakeWindowExist(tokenwin);

		max = WidthOfScreen(Tk_Screen(tkwin)) - Tk_Width(tokenwin);
		x   = (x > max) ? max : x;
		x   = (x < 0)   ? 0 : x;

		max = HeightOfScreen(Tk_Screen(tkwin)) - Tk_Height(tokenwin);
		y   = (y > max) ? max : y;
		y   = (y < 0)   ? 0 : y;

		if ((x != Tk_X(tokenwin)) || (y != Tk_Y(tokenwin)))
			Tk_MoveWindow(tokenwin, x, y);
	}

	/*
	 *  HANDLE:  drag&drop drop <path> <x> <y>
	 */
	else if ((c == 'd') && strncmp(argv[1], "drop", length) == 0)
	{
		if (argc < 5)
		{
			sprintf(interp->result,
				"wrong # args: should be \"%s drop pathname x y\"",
				argv[0]);
			return TCL_ERROR;
		}

		dsPtr = GetSourceInfo(ddlist,argv[2],&newEntry);
		if (newEntry)
		{
			sprintf(interp->result, "not a drag&drop source: %.50s", argv[2]);
			DestroySourceInfo(ddlist,argv[2]);
			return TCL_ERROR;
		}
		if ((Tcl_GetInt(interp, argv[3], &x) != TCL_OK) ||
			(Tcl_GetInt(interp, argv[4], &y) != TCL_OK))
			return TCL_ERROR;

		sprintf(buffer, "drag&drop location %d %d", x, y);
		Tcl_Eval(dsPtr->interp, buffer, 0, (char**)NULL);

		Tk_CancelIdleCall(UpdateTokenWindow, (ClientData)dsPtr);

		/*
		 *  Make sure that token window was not dropped before it
		 *  was either mapped or packed with info.
		 */
		if (Tk_IsMapped(dsPtr->tokenwin) && !dsPtr->tokencmdInProg)
		{
			dsPtr->tokenx = x;
			dsPtr->tokeny = y;
			UpdateTokenWindow((ClientData)dsPtr);

			if (dsPtr->sendcmd)
			{
				if (dsPtr->overTargetWin)
					DragDropSend(dsPtr, x, y);
				else
					HideDragDropToken((ClientData)dsPtr);
			}
			ddlist->numactive--;  /* one fewer active token window */
		}
	}

	/*
	 *  HANDLE:  drag&drop active
	 */
	else if ((c == 'a') && strncmp(argv[1], "active", length) == 0)
	{
		if (argc != 2)
		{
			sprintf(interp->result, "usage: %s active", argv[0]);
			return TCL_ERROR;
		}
		sprintf(interp->result, "%d", (ddlist->numactive > 0) ? 1 : 0);
		return TCL_OK;
	}

	/*
	 *  HANDLE:  drag&drop location ?<x> <y>?
	 */
	else if ((c == 'l') && strncmp(argv[1], "location", length) == 0)
	{
		if (argc == 2)
		{
			sprintf(interp->result, "%d %d", ddlist->locx, ddlist->locy);
			return TCL_OK;
		}
		else if ((argc == 4) &&
			(Tcl_GetInt(interp, argv[2], &x) == TCL_OK) &&
			(Tcl_GetInt(interp, argv[3], &y) == TCL_OK))
		{
			ddlist->locx = x;
			ddlist->locy = y;
			sprintf(interp->result, "%d %d", ddlist->locx, ddlist->locy);
			return TCL_OK;
		}
		else
		{
			sprintf(interp->result, "usage: %s location ?x y?", argv[0]);
			return TCL_ERROR;
		}
	}

	/*
	 *  Report improper command arguments
	 */
	else
	{
		sprintf(interp->result, "bad option \"%.50s\": should be source, target, drag, drop or location", argv[1]);
		return TCL_ERROR;
	}
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  GetSourceInfo()
 *
 *  Looks for a DD_Source record in the hash table for drag&drop source
 *  widgets.  Creates a new record if the widget name is not already
 *  registered.  Returns a pointer to the desired record.
 * ------------------------------------------------------------------------
 */
static DD_Source*
GetSourceInfo(ddlist,pathname,newEntry)
	DD_RegList* ddlist;  /* drag&drop records for all registered widgets */
	char* pathname;      /* widget pathname for desired record */
	int* newEntry;       /* returns non-zero => new record created */
{
	DD_Source *dsPtr;
	Tcl_HashEntry *ddEntry;

	ddEntry = Tcl_CreateHashEntry(&ddlist->srcList, pathname, newEntry);
	if (*newEntry)
	{
		/*
		 *  Initialize a data structure for the widget...
		 */
		dsPtr = (DD_Source*)ckalloc(sizeof(DD_Source));
		dsPtr->interp = ddlist->interp;
		dsPtr->button = 0;
		dsPtr->display = NULL;
		dsPtr->tkwin = NULL;
		dsPtr->tokenwin = NULL;
		dsPtr->tokenNormBorder = NULL;
		dsPtr->tokenActBorder = NULL;
		dsPtr->tokenBorderWidth = 0;
		dsPtr->tokenNormRelief = 0;
		dsPtr->tokenActRelief = 0;
		dsPtr->overTargetWin = 0;
		dsPtr->tokenx = 0;
		dsPtr->tokeny = 0;
		dsPtr->hidetoken = NULL;
		dsPtr->rejectFg = NULL;
		dsPtr->rejectBg = NULL;
		dsPtr->rejectSt = None;
		dsPtr->rejectFgGC = None;
		dsPtr->rejectBgGC = None;
		dsPtr->tokencmdInProg = 0;
		dsPtr->tokencmd = NULL;
		dsPtr->tokencmdResult = NULL;
		dsPtr->sendcmd = NULL;

		Tcl_SetHashValue(ddEntry, (ClientData)dsPtr);
	}
	return (DD_Source*)Tcl_GetHashValue(ddEntry);
}

/*
 * ------------------------------------------------------------------------
 *  DestroySourceInfo()
 *
 *  Looks for a DD_Source record in the hash table for drag&drop source
 *  widgets.  Destroys the record if found.
 * ------------------------------------------------------------------------
 */
static void
DestroySourceInfo(ddlist,pathname)
	DD_RegList* ddlist;  /* drag&drop records for all registered widgets */
	char* pathname;      /* widget pathname for desired record */
{
	DD_Source *dsPtr;
	Tcl_HashEntry *ddEntry;

	ddEntry = Tcl_FindHashEntry(&ddlist->srcList, pathname);
	dsPtr = (ddEntry) ? (DD_Source*)Tcl_GetHashValue(ddEntry) : NULL;

	if (dsPtr)
	{
		Tk_CancelIdleCall(UpdateTokenWindow, (ClientData)dsPtr);

		if (dsPtr->tokenNormBorder)    Tk_Free3DBorder(dsPtr->tokenNormBorder);
		if (dsPtr->tokenActBorder)     Tk_Free3DBorder(dsPtr->tokenActBorder);
		if (dsPtr->hidetoken)          Tk_DeleteTimerHandler(dsPtr->hidetoken);
		if (dsPtr->rejectFg)           Tk_FreeColor(dsPtr->rejectFg);
		if (dsPtr->rejectBg)           Tk_FreeColor(dsPtr->rejectBg);
		if (dsPtr->rejectSt != None)   Tk_FreeBitmap(dsPtr->display,
		                                 dsPtr->rejectSt);
		if (dsPtr->rejectFgGC != None) Tk_FreeGC(dsPtr->display,
		                                 dsPtr->rejectFgGC);
		if (dsPtr->rejectBgGC != None) Tk_FreeGC(dsPtr->display,
		                                 dsPtr->rejectBgGC);
		if (dsPtr->tokencmd)           ckfree(dsPtr->tokencmd);
		if (dsPtr->tokencmdResult)     ckfree(dsPtr->tokencmdResult);
		if (dsPtr->sendcmd)            ckfree(dsPtr->sendcmd);

		ckfree((char*)dsPtr);
	}
	if (ddEntry) Tcl_DeleteHashEntry(ddEntry);
}

/*
 * ------------------------------------------------------------------------
 *  ConfigSource()
 *
 *  Called to process an (argc,argv) list to configure (or reconfigure)
 *  a drag&drop source widget.
 * ------------------------------------------------------------------------
 */
static int
ConfigSource(interp, dsPtr, argc, argv, flags)
	Tcl_Interp *interp;        /* current interpreter */
	register DD_Source *dsPtr; /* drag&drop source widget record */
	int argc;                  /* number of arguments */
	char **argv;               /* argument strings */
	int flags;                 /* flags controlling interpretation */
{
	unsigned long gcMask;
	XGCValues gcValues;
	GC newGC;

	/*
	 *  Handle the bulk of the options...
	 */
	if (Tk_ConfigureWidget(interp, dsPtr->tkwin, SourceConfigSpecs,
		argc, argv, (char*)dsPtr, flags) != TCL_OK)
		return TCL_ERROR;

	/*
	 *  Set up the rejection foreground GC for the token window...
	 */
	gcValues.foreground = dsPtr->rejectFg->pixel;
	gcValues.subwindow_mode = IncludeInferiors;
	gcValues.graphics_exposures = False;
	gcMask = GCForeground|GCSubwindowMode|GCGraphicsExposures;

	if (dsPtr->rejectSt != None)
	{
		gcValues.stipple = dsPtr->rejectSt;
		gcValues.fill_style = FillStippled;
		gcMask |= GCForeground|GCStipple|GCFillStyle;
	}
	newGC = Tk_GetGC(dsPtr->tkwin, gcMask, &gcValues);

	if (dsPtr->rejectFgGC != None)
		Tk_FreeGC(dsPtr->display, dsPtr->rejectFgGC);
	dsPtr->rejectFgGC = newGC;

	/*
	 *  Set up the rejection background GC for the token window...
	 */
	gcValues.foreground = dsPtr->rejectBg->pixel;
	gcValues.subwindow_mode = IncludeInferiors;
	gcValues.graphics_exposures = False;
	gcMask = GCForeground|GCSubwindowMode|GCGraphicsExposures;

	newGC = Tk_GetGC(dsPtr->tkwin, gcMask, &gcValues);

	if (dsPtr->rejectBgGC != None)
		Tk_FreeGC(dsPtr->display, dsPtr->rejectBgGC);
	dsPtr->rejectBgGC = newGC;

	/*
	 *  Reset the border width in case it has changed...
	 */
	if (dsPtr->tokenwin)
		Tk_SetInternalBorder(dsPtr->tokenwin, 2*dsPtr->tokenBorderWidth);

	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  UnregSource()
 *
 *  Invoked by Tk_HandleEvent whenever a DestroyNotify event is received
 *  on a registered drag&drop source widget.
 * ------------------------------------------------------------------------
 */
static void
UnregSource(cdata, eventPtr)
	ClientData cdata;   /* drag&drop registration list */
	XEvent *eventPtr;   /* event description */
{
	DD_RegEntry *ddentry = (DD_RegEntry*)cdata;
	DD_RegList *ddlist = ddentry->ddlist;
	char *ddname = Tk_PathName(ddentry->tkwin);

	if (eventPtr->type == DestroyNotify)
	{
		DestroySourceInfo(ddlist,ddname);
		ckfree((char*)ddentry);
	}
}

/*
 * ------------------------------------------------------------------------
 *  GetTargetInfo()
 *
 *  Looks for a DD_Target record in the hash table for drag&drop target
 *  widgets.  Creates a new record if the widget name is not already
 *  registered.  Returns a pointer to the desired record.
 * ------------------------------------------------------------------------
 */
static DD_Target*
GetTargetInfo(ddlist,pathname,newEntry)
	DD_RegList* ddlist;  /* drag&drop records for all registered widgets */
	char* pathname;      /* widget pathname for desired record */
	int* newEntry;       /* returns non-zero => new record created */
{
	DD_Target *dtPtr;
	Tcl_HashEntry *ddEntry;

	ddEntry = Tcl_CreateHashEntry(&ddlist->trgList, pathname, newEntry);
	if (*newEntry)
	{
		/*
		 *  Initialize a data structure for the widget...
		 */
		dtPtr = (DD_Target*)ckalloc(sizeof(DD_Target));
		dtPtr->interp = ddlist->interp;
		dtPtr->display = NULL;
		dtPtr->tkwin = NULL;
		dtPtr->handlers = NULL;

		Tcl_SetHashValue(ddEntry, (ClientData)dtPtr);
	}
	return (DD_Target*)Tcl_GetHashValue(ddEntry);
}

/*
 * ------------------------------------------------------------------------
 *  DestroyTargetInfo()
 *
 *  Looks for a DD_Target record in the hash table for drag&drop target
 *  widgets.  Destroys the record if found.
 * ------------------------------------------------------------------------
 */
static void
DestroyTargetInfo(ddlist,pathname)
	DD_RegList* ddlist;  /* drag&drop records for all registered widgets */
	char* pathname;      /* widget pathname for desired record */
{
	DD_Target *dtPtr;
	DD_TargetHndl *dtHndl, *next;
	Tcl_HashEntry *ddEntry;

	ddEntry = Tcl_FindHashEntry(&ddlist->trgList, pathname);
	dtPtr = (ddEntry) ? (DD_Target*)Tcl_GetHashValue(ddEntry) : NULL;

	if (dtPtr)
	{
		dtHndl = dtPtr->handlers;
		while (dtHndl)
		{
			next = dtHndl->next;
			DestroyTargetHandler(dtHndl);
			dtHndl = next;
		}
		ckfree((char*)dtPtr);
	}
	if (ddEntry) Tcl_DeleteHashEntry(ddEntry);
}

/*
 * ------------------------------------------------------------------------
 *  FindTargetHandler()
 *
 *  Looks for the requested data type of the list of handlers for the
 *  given drag&drop target.
 * ------------------------------------------------------------------------
 */
static char*
FindTargetHandler(dtPtr, dtname)
	register DD_Target *dtPtr; /* drag&drop target widget record */
	char *dtname;              /* name of requested data type */
{
	DD_TargetHndl *dtHndl;

	for (dtHndl=dtPtr->handlers; dtHndl; dtHndl=dtHndl->next)
		if (strcmp(dtHndl->dataType,dtname) == 0)
			return dtHndl->command;

	return NULL;
}

/*
 * ------------------------------------------------------------------------
 *  PutTargetHandler()
 *
 *  Looks for the requested data type of the list of handlers for the
 *  given drag&drop target.  If found, then its associated command is
 *  changed to the given command.  If not found, then a new handler
 *  is created.
 * ------------------------------------------------------------------------
 */
static void
PutTargetHandler(dtPtr, dtname, cmd)
	register DD_Target *dtPtr; /* drag&drop target widget record */
	char *dtname;              /* name of data type */
	char *cmd;                 /* command string for data type */
{
	DD_TargetHndl *tail = NULL;
	DD_TargetHndl *dtHndl;

	for (dtHndl=dtPtr->handlers; dtHndl; tail=dtHndl,dtHndl=dtHndl->next)
		if (strcmp(dtHndl->dataType,dtname) == 0)
		{
			if (*cmd == '\0')
			{
				if (tail)
					tail->next = dtHndl->next;
				else
					dtPtr->handlers = dtHndl->next;

				DestroyTargetHandler(dtHndl);
				return;
			}
			else
			{
				ckfree(dtHndl->command);
				dtHndl->command = (char*)ckalloc(strlen(cmd)+1);
				strcpy(dtHndl->command, cmd);
				return;
			}
		}

	if (tail)
		tail->next = CreateTargetHandler(dtname,cmd);
	else
		dtPtr->handlers = CreateTargetHandler(dtname,cmd);
}

/*
 * ------------------------------------------------------------------------
 *  CreateTargetHandler()
 *
 *  Creates a new target handler record and returns a pointer to it.
 * ------------------------------------------------------------------------
 */
static DD_TargetHndl*
CreateTargetHandler(dtname, cmd)
	char *dtname;              /* name of data type */
	char *cmd;                 /* command string for data type */
{
	DD_TargetHndl *retn;
	retn = (DD_TargetHndl*)ckalloc(sizeof(DD_TargetHndl));

	retn->dataType = (char*)ckalloc(strlen(dtname)+1);
	strcpy(retn->dataType, dtname);

	retn->command = (char*)ckalloc(strlen(cmd)+1);
	strcpy(retn->command, cmd);

	retn->next = NULL;
	return retn;
}

/*
 * ------------------------------------------------------------------------
 *  DestroyTargetHandler()
 *
 *  Destroys a target handler record.
 * ------------------------------------------------------------------------
 */
static void
DestroyTargetHandler(dtHndl)
	DD_TargetHndl *dtHndl;
{
	ckfree(dtHndl->dataType);
	ckfree(dtHndl->command);
	ckfree((char*)dtHndl);
}

/*
 * ------------------------------------------------------------------------
 *  UnregTarget()
 *
 *  Invoked by Tk_HandleEvent whenever a DestroyNotify event is received
 *  on a registered drag&drop target widget.
 * ------------------------------------------------------------------------
 */
static void
UnregTarget(cdata, eventPtr)
	ClientData cdata;   /* drag&drop registration list */
	XEvent *eventPtr;   /* event description */
{
	DD_RegEntry *ddentry = (DD_RegEntry*)cdata;
	DD_RegList *ddlist = ddentry->ddlist;
	char *ddname = Tk_PathName(ddentry->tkwin);

	if (eventPtr->type == DestroyNotify)
	{
		DestroyTargetInfo(ddlist,ddname);
		ckfree((char*)ddentry);
	}
}

/*
 * ------------------------------------------------------------------------
 *  DragDropSend()
 *
 *  Invoked after a drop operation to send data to the drop application.
 * ------------------------------------------------------------------------
 */
static void
DragDropSend(dsPtr,x,y)
	register DD_Source *dsPtr;  /* drag&drop source record */
	int x, y;                   /* drop location */
{
	int status;
	char interpName[MAX_PROP_SIZE];
	char ddName[MAX_PROP_SIZE];

	Display *disp = Tk_Display(dsPtr->tokenwin);

	/*
	 *  See if current position is over drop point...
	 */
	Window ignore = Tk_WindowId(dsPtr->tokenwin);
	Window w = FindWinAtXY(disp, dsPtr->tokenx, dsPtr->tokeny, ignore);

	if (w && (w != Tk_WindowId(dsPtr->tkwin)) &&
	    GetDragDropInfo(disp, w, interpName, ddName))
	{
		char cmd[1024];

		sprintf(cmd, "send {%s} drag&drop location %d %d", interpName, x, y);
		status = Tcl_Eval(dsPtr->interp, cmd, 0, (char**)NULL);

		if ((status == TCL_OK) && (dsPtr->sendcmd))
		{
			sprintf(cmd, "%s {%s} {%s} {%s}", dsPtr->sendcmd,
				interpName, ddName, dsPtr->tokencmdResult);
			status = Tcl_Eval(dsPtr->interp, cmd, 0, (char**)NULL);
		}

		/*
		 *  Give success/failure feedback to user.
		 */
		if (status == TCL_OK)
			HideDragDropToken((ClientData)dsPtr);
		else
			RejectDragDropToken(dsPtr);
	}
}

/*
 * ------------------------------------------------------------------------
 *  FindWinAtXY()
 *
 *  Searches through the given window heirarchy the find the most
 *  specific window that contains the given point coordinate.  Returns
 *  the X-window ID for this window.
 * ------------------------------------------------------------------------
 */
static Window
FindWinAtXY(disp,x,y,ignore)
	Display *disp;          /* X window display server */
	int x,y;                /* desired point coordinate */
	Window ignore;          /* window to ignore during search */
{
	int i, xorg, yorg;
	Window root, parent, next, *kids;
	unsigned int nkids;
	XWindowAttributes winInfo;

	Window w = DefaultRootWindow(disp);
	XGetWindowAttributes(disp, w, &winInfo);

	xorg = winInfo.x;  /* origin starts at ul-corner of root window */
	yorg = winInfo.y;

	while (w &&
		XQueryTree(disp, w, &root, &parent, &kids, &nkids) &&
		(nkids > 0))
	{
		next = NULL;

		for (i=nkids-1; (i >= 0) && !next; i--)
			if ((kids[i] != ignore) &&
				XGetWindowAttributes(disp, kids[i], &winInfo) &&
	 		    (winInfo.map_state == IsViewable) &&
			    (x-xorg >= winInfo.x) &&
			    (x-xorg <= winInfo.x+winInfo.width) &&
			    (y-yorg >= winInfo.y) &&
			    (y-yorg <= winInfo.y+winInfo.height))
			{
				next = kids[i];
				xorg += winInfo.x;  /* offset origin to parent location */
				yorg += winInfo.y;
			}

		XFree(kids);  /* done with list of kids */
		w = next;     /* search this window next */
	}
	return w;
}

/*
 * ------------------------------------------------------------------------
 *  GetDragDropInfo()
 *
 *  Queries the given window for a "DragDropInfo" property.  If one is
 *  found having the proper format, then the drag&drop information is
 *  extracted and returned in the given arrays, and the routine returns
 *  a non-zero status.  A zero status is returned on failure.
 * ------------------------------------------------------------------------
 */
static int
GetDragDropInfo(disp, win, interpName, ddName)
	Display *disp;       /* X window display server */
	Window win;          /* X window */
	char *interpName;    /* interpreter name is copied into this array */
	char *ddName;        /* drag&drop widget name is copied into this array */
{
	int status = 0;  /* assume this will fail */

	char *propInfo = NULL;
	int result, actualFormat;
	Atom ddProperty, actualType;
	unsigned long numItems, bytesAfter;
	char buffer[MAX_PROP_SIZE];

	if (disp && win)
	{
		ddProperty = XInternAtom(disp, "DragDropInfo", False);

		result = XGetWindowProperty(disp, win, ddProperty,
			0, MAX_PROP_SIZE, False, XA_STRING, &actualType, &actualFormat,
			&numItems, &bytesAfter, (unsigned char**) &propInfo);

		/*
		 *  If the window has a "DragDropInfo" property, then
		 *  extract the interpreter name and drag&drop widget name.
		 */
		if ((result == Success) &&
		    (actualFormat == 8) &&
			(actualType == XA_STRING))
		{
			char *idata = (interpName) ? interpName : buffer;
			char *ddata = (ddName) ? ddName : buffer;
			char *p = propInfo;

			while ((*p != '\0') && (*p != '}'))
				*idata++ = *p++;
			*idata = '\0';

			if (*p != '\0')
			{
				p++;
				status = ~0;  /* success! */
			}
			while (*p != '\0')
				*ddata++ = *p++;
			*ddata = '\0';
		}
		XFree(propInfo);
	}
	return status;
}

/*
 * ------------------------------------------------------------------------
 *  UpdateTokenWindow()
 *
 *  Invoked when the event loop is idle to determine whether or not
 *  the current drag&drop token position is over another drag&drop
 *  target.
 * ------------------------------------------------------------------------
 */
static void
UpdateTokenWindow(clientData)
	ClientData clientData;  /* widget data */
{
	int status, bd;
	register DD_Source *dsPtr = (DD_Source*)clientData;
	Tk_Window tkwin = dsPtr->tokenwin;
	Window ignore = Tk_WindowId(dsPtr->tokenwin);

	Window w = FindWinAtXY(dsPtr->display,
		dsPtr->tokenx, dsPtr->tokeny, ignore);

	if (w == Tk_WindowId(dsPtr->tkwin))
		status = 0;
	else
		status = GetDragDropInfo(dsPtr->display, w, (char*)NULL, (char*)NULL);

	bd = dsPtr->tokenBorderWidth;
	if (dsPtr->overTargetWin && !status)
	{
		Tk_Fill3DRectangle(dsPtr->display, Tk_WindowId(tkwin),
			dsPtr->tokenNormBorder,
			bd, bd, Tk_Width(tkwin)-2*bd, Tk_Height(tkwin)-2*bd,
			dsPtr->tokenBorderWidth, dsPtr->tokenNormRelief);
	}
	else if (!dsPtr->overTargetWin && status)
	{
		Tk_Fill3DRectangle(dsPtr->display, Tk_WindowId(tkwin),
			dsPtr->tokenActBorder,
			bd, bd, Tk_Width(tkwin)-2*bd, Tk_Height(tkwin)-2*bd,
			dsPtr->tokenBorderWidth, dsPtr->tokenActRelief);
	}
	dsPtr->overTargetWin = status;
}

/*
 * ------------------------------------------------------------------------
 *  DDTokenEventProc()
 *
 *  Invoked by the Tk dispatcher to handle widget events.
 *  Manages redraws for the drag&drop token window.
 * ------------------------------------------------------------------------
 */
static void
DDTokenEventProc(clientData, eventPtr)
	ClientData clientData;     /* data associated with widget */
	XEvent *eventPtr;          /* information about event */
{
	register DD_Source *dsPtr = (DD_Source*)clientData;

	if ((eventPtr->type == Expose) && (eventPtr->xexpose.count == 0))
	{
		if (dsPtr->tokenwin)
		{
			Tk_Window tkwin = dsPtr->tokenwin;
			Tk_3DBorder border;

			border = (dsPtr->overTargetWin)
				? dsPtr->tokenActBorder
				: dsPtr->tokenNormBorder;

			Tk_Fill3DRectangle(Tk_Display(tkwin), Tk_WindowId(tkwin),
				border, 0, 0, Tk_Width(tkwin), Tk_Height(tkwin),
				dsPtr->tokenBorderWidth, TK_RELIEF_FLAT);
		}
	}
	else if (eventPtr->type == DestroyNotify)
		dsPtr->tokenwin = NULL;
}

/*
 * ------------------------------------------------------------------------
 *  HideDragDropToken()
 *
 *  Unmaps the drag&drop token.  Invoked directly at the end of a
 *  successful communication, or after a delay if the communication
 *  fails (allowing the user to see a graphical picture of failure).
 * ------------------------------------------------------------------------
 */
static void
HideDragDropToken(clientData)
	ClientData clientData;  /* widget data */
{
	register DD_Source *dsPtr = (DD_Source*)clientData;

	if (dsPtr->tokenwin)
		Tk_UnmapWindow(dsPtr->tokenwin);

	dsPtr->hidetoken = NULL;
}

/*
 * ------------------------------------------------------------------------
 *  RejectDragDropToken()
 *
 *  Draws a rejection mark on the current drag&drop token, and arranges
 *  for the token to be unmapped after a small delay.
 * ------------------------------------------------------------------------
 */
static void
RejectDragDropToken(dsPtr)
	DD_Source* dsPtr;  /* widget data */
{
	int div = 6;      /* controls size of rejection symbol */
	int w,h,lwid,x,y,margin;

	Tk_Window tkwin = dsPtr->tokenwin;

	margin = 2*dsPtr->tokenBorderWidth;
	w = Tk_Width(tkwin) - 2*margin;
	h = Tk_Height(tkwin) - 2*margin;

	lwid = (w < h) ? w/div : h/div;
	lwid = (lwid < 1) ? 1 : lwid;

	w = h = lwid*(div-1);
	x = (Tk_Width(tkwin) - w)/2;
	y = (Tk_Height(tkwin) - h)/2;

	/*
	 *  Draw the rejection symbol background (\) on the token window...
	 */
	XSetLineAttributes(Tk_Display(tkwin), dsPtr->rejectBgGC,
		lwid+4, LineSolid, CapButt, JoinBevel);

	XDrawArc(Tk_Display(tkwin), Tk_WindowId(tkwin), dsPtr->rejectBgGC,
		x, y, w, h, 0, 23040);

	XDrawLine(Tk_Display(tkwin), Tk_WindowId(tkwin), dsPtr->rejectBgGC,
		x+lwid, y+lwid, x+w-lwid, y+h-lwid);

	/*
	 *  Draw the rejection symbol foreground (\) on the token window...
	 */
	XSetLineAttributes(Tk_Display(tkwin), dsPtr->rejectFgGC,
		lwid, LineSolid, CapButt, JoinBevel);

	XDrawArc(Tk_Display(tkwin), Tk_WindowId(tkwin), dsPtr->rejectFgGC,
		x, y, w, h, 0, 23040);

	XDrawLine(Tk_Display(tkwin), Tk_WindowId(tkwin), dsPtr->rejectFgGC,
		x+lwid, y+lwid, x+w-lwid, y+h-lwid);

	/*
	 *  Arrange for token window to disappear eventually.
	 */
	dsPtr->hidetoken
		= Tk_CreateTimerHandler(1000, HideDragDropToken, (ClientData)dsPtr);
}
