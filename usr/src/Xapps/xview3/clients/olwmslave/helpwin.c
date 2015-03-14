/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */
/* ----------------------------------------------------------------------
 *	helpwin.c
 * ---------------------------------------------------------------------*/
#ident	"@(#)helpwin.c	1.7 91/09/14 helpwin.c SMI"

#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/text.h>
#include <xview/svrimage.h>
#include <xview/scrollbar.h>
#include <xview/notice.h>
#include <xview/cms.h>
#include "olwmslave.h"
#include "mem.h"

/* ----------------------------------------------------------------------
 *			Data Structure Definitions
 * ---------------------------------------------------------------------*/

/*
 *	HelpWindow
 *	
 *	There is one HelpWindow structure for each screen that a
 *	display server handles.  It consists of a Help Frame that
 *	contains a Text window for the help text and a Panel Message
 *	item with an image for the magnifying glass.  This image
 *	is rendered into using an mag-glass image and it's mask along
 *	with the actual image under the pointer to create the
 *	complete magnifiying glass image.  A handle on the scrollbar
 *	in the text window is needed since the scrollbar is visible
 *	when the help/info text exceeds the length of the scrollbar.
 */
typedef struct _HelpWindow {
	int		hwScreenNo;
	Frame		hwFrame;
	Panel		hwMGlassPanel;
	Server_image	hwMGlassRender;
	Panel_item	hwMGlassItem;
	Textsw		hwText;
	Scrollbar	hwTextScroll;
	Panel		hwMorePanel;
	Panel_item	hwMoreHelp;
	char		*hwMoreHelpCmd;
	Server_image	hwMGlassImage;
	Server_image	hwMGlassMask;
	GC		hwFillGC;
	GC		hwCopyGC;
	GC		hwImageGC;
} HelpWindow;

/* ----------------------------------------------------------------------
 *			Local Data
 * ---------------------------------------------------------------------*/

static HelpWindow	*helpWinList ;	/* dynamic array of HelpWindows
				   	   one for each screen */

				/* convience macro to access list as array */
#define HelpWin(nscr)		(&helpWinList[(nscr)])

#define	HS_HELP		0
#define	HS_MORE		1
#define HS_OK		2
#define HS_NOHELPERR	3
#define HS_LENGTH	4
static char		*helpStrings[HS_LENGTH];

/* ----------------------------------------------------------------------
 *	The following is taken from XView: libxvol/help/help.c
 *	This is to ensure window size consistency.
 * ---------------------------------------------------------------------*/
/*
 * There is a maximum of 10 lines of text of 50 chars each visible in the
 * help text subwindow.  If the help text exceeds 10 lines, a scrollbar is
 * shown.
 */
#define HELPTEXTCOLS 50
#define HELPTEXTLINES 10
#define HELP_CANVAS_MARGIN 10
#define HELP_IMAGE_X 35
#define HELP_IMAGE_Y 5
#define HELP_IMAGE_WIDTH 80
#define HELP_IMAGE_HEIGHT 73
#define MORE_BUTTON_OFFSET 30
 
#define MAX_HELP_STRING_LENGTH 128
#define MAX_FILE_KEY_LENGTH 64

#define HELPWIN_KEY	1

/* ----------------------------------------------------------------------
 *	Magnify glass image and mask
 * ----------------------------------------------------------------------*/
static unsigned short    hwmglass_data[] = {
#include <images/mglass.icon>
};
mpr_static(hwmglass_pr, 199, 133, 1, hwmglass_data);
 
static unsigned short    hwmglass_mask_data[] = {
#include <images/mglass_mask.icon>
};
mpr_static(hwmglass_mask_pr, 199, 133, 1, hwmglass_mask_data);

/* ----------------------------------------------------------------------
 *	External Functions
 * ---------------------------------------------------------------------*/
int		help_get_arg();		/* from help_file.c */
char		*help_get_text();

/* ----------------------------------------------------------------------
 *	Local Forward Functions
 * ---------------------------------------------------------------------*/
int		ShowHelpWindow();
void		InitHelpWindow();
Bool		CreateHelpWindow();
Notify_value	DestroyHelpWindow();
void		ResetHelpWindow();
Bool		TextHelpWindow();
Bool		ImageHelpWindow();
void		MoreHelp();
void		ErrorNotice();

/* ----------------------------------------------------------------------
 *	ShowHelpWindow
 * ---------------------------------------------------------------------*/
int
ShowHelpWindow(nscreen,mousex,mousey,helpkey)
	int		nscreen;
	int		mousex,mousey;
	char		*helpkey;
{
	HelpWindow	*hw;

	if (!ScreenUsed(nscreen))
		return False;

	if (!helpWinList)
		InitHelpWindow();

	hw = HelpWin(nscreen);

	if (!hw->hwFrame) {
		if (!CreateHelpWindow(hw,helpkey))
			goto Error;
	} else {
		ResetHelpWindow(hw);
	}

	if (!TextHelpWindow(hw,helpkey)) {
		goto Error;
	}

	if (!ImageHelpWindow(hw,mousex,mousey)) {
		goto Error;
	}
	
	xv_set(hw->hwFrame,
		XV_SHOW,	TRUE,
		WIN_FRONT,
		NULL);

	return True;
Error:
	ErrorNotice(nscreen,helpStrings[HS_NOHELPERR],helpkey);
	return False;
}

/* ----------------------------------------------------------------------
 *	InitHelpWindow		- Inits helpWinList array.
 * ----------------------------------------------------------------------*/
static	void
InitHelpWindow()
{
	HelpWindow	*hw;
	int		i;

	helpWinList = (HelpWindow *)MemAlloc(
		sizeof(HelpWindow)*NumScreens());
	for (i=0; i<NumScreens(); i++) {
		hw = HelpWin(i);
		hw->hwScreenNo = i;
		hw->hwFrame = (Frame)NULL;
	}
	helpStrings[HS_HELP] 	= LOCALIZE("Help");
	helpStrings[HS_MORE] 	= LOCALIZE("More");
	helpStrings[HS_OK] 	= LOCALIZE("OK");
	helpStrings[HS_NOHELPERR] = LOCALIZE("No Help Available For ");
}

/* ----------------------------------------------------------------------
 *	CreateHelpWindow	- Creates the help window frame and etc.
 * ----------------------------------------------------------------------*/
static 	Bool
CreateHelpWindow(hw,helpkey)
	HelpWindow	*hw;
	char		*helpkey;
{
	Cms		controlcms;
	XGCValues	gcv;
	char		flabel[128];
	int		len;

	len = (int)strchr(helpkey,':') - (int)helpkey;
	if (len > 80) len = 80;
	strncpy(flabel,helpkey,len);
	flabel[len++] = ':';
	flabel[len++] = ' ';
	flabel[len] = 0;
	strcat(flabel,helpStrings[HS_HELP]);

	hw->hwFrame = (Frame)xv_create(XVRoot(hw->hwScreenNo),FRAME_HELP,
		XV_X,			0,
		XV_Y,			0,
		FRAME_LABEL,		flabel,
		XV_KEY_DATA,		HELPWIN_KEY, hw,
		XV_SHOW,		FALSE,
		NULL);
	notify_interpose_destroy_func(hw->hwFrame,DestroyHelpWindow);

	hw->hwText = (Textsw)xv_create(hw->hwFrame,TEXTSW,
		XV_X,			hwmglass_pr.pr_width,
		XV_Y,			0,
		WIN_COLUMNS,		HELPTEXTCOLS,
		WIN_ROWS,		HELPTEXTLINES,
		TEXTSW_IGNORE_LIMIT,	TEXTSW_INFINITY,
		TEXTSW_LINE_BREAK_ACTION, TEXTSW_WRAP_AT_WORD,
		TEXTSW_LOWER_CONTEXT,	-1,	/* disable scroll on insert */
		TEXTSW_DISABLE_LOAD,	TRUE,
		TEXTSW_READ_ONLY,	TRUE,
		NULL);
	hw->hwTextScroll = (Scrollbar)xv_get(hw->hwText,
		OPENWIN_VERTICAL_SCROLLBAR,
		xv_get(hw->hwText,OPENWIN_NTH_VIEW,0));
	xv_set(hw->hwTextScroll,SCROLLBAR_SPLITTABLE,FALSE,
		NULL);

	hw->hwMGlassPanel = (Panel)xv_create(hw->hwFrame,PANEL,
		XV_X,			0,
		XV_Y,			0,
		XV_WIDTH,		hwmglass_pr.pr_width,
		XV_HEIGHT,		xv_get(hw->hwText,XV_HEIGHT),
		NULL);
	hw->hwMGlassItem = 
	    	(Panel_item)xv_create(hw->hwMGlassPanel,PANEL_MESSAGE,
		XV_HELP_DATA,		"xview:helpMagnifyingGlass",
		NULL);
	hw->hwMGlassRender = (Server_image)NULL;

	hw->hwMorePanel = (Panel)xv_create(hw->hwFrame,PANEL,
		XV_X,			0,
		WIN_BELOW,		hw->hwText,
		XV_WIDTH,		
			hwmglass_pr.pr_width + xv_get(hw->hwText,XV_WIDTH),
		NULL);
	hw->hwMoreHelp = (Panel)xv_create(hw->hwMorePanel,PANEL_BUTTON,
		XV_X,			hwmglass_pr.pr_width+MORE_BUTTON_OFFSET,
		PANEL_LABEL_STRING,	helpStrings[HS_MORE],
		PANEL_NOTIFY_PROC,	MoreHelp,
		XV_KEY_DATA,		HELPWIN_KEY, hw,
		NULL);

	window_fit_height(hw->hwMorePanel);
	window_fit(hw->hwFrame);

	hw->hwMGlassImage = 
		(Server_image)xv_create(XVScreen(hw->hwScreenNo),SERVER_IMAGE,
		XV_WIDTH,		hwmglass_pr.pr_width,
		XV_HEIGHT,		hwmglass_pr.pr_height,
		SERVER_IMAGE_DEPTH,	hwmglass_pr.pr_depth,
		SERVER_IMAGE_BITS,	hwmglass_data,
		NULL);
	hw->hwMGlassMask = 
		(Server_image)xv_create(XVScreen(hw->hwScreenNo),SERVER_IMAGE,
		XV_WIDTH,		hwmglass_mask_pr.pr_width,
		XV_HEIGHT,		hwmglass_mask_pr.pr_height,
		SERVER_IMAGE_DEPTH,	hwmglass_mask_pr.pr_depth,
		SERVER_IMAGE_BITS,	hwmglass_mask_data,
		NULL);

	controlcms = (Cms)xv_get(hw->hwMGlassPanel,WIN_CMS);

	gcv.function = GXclear;
	gcv.background = (unsigned long)xv_get(controlcms,CMS_FOREGROUND_PIXEL);
	gcv.foreground = (unsigned long)xv_get(controlcms,CMS_BACKGROUND_PIXEL);
	hw->hwFillGC = XCreateGC(display,RootWin(hw->hwScreenNo),
		GCFunction|GCForeground|GCBackground,&gcv);

	gcv.foreground = (unsigned long)xv_get(controlcms,CMS_FOREGROUND_PIXEL);
	gcv.background = (unsigned long)xv_get(controlcms,CMS_BACKGROUND_PIXEL);
	hw->hwCopyGC = XCreateGC(display,RootWin(hw->hwScreenNo),
		GCForeground|GCBackground,&gcv);

	gcv.foreground = (unsigned long)xv_get(controlcms,CMS_FOREGROUND_PIXEL);
	gcv.background = (unsigned long)xv_get(controlcms,CMS_BACKGROUND_PIXEL);
	gcv.fill_style = FillOpaqueStippled;
	gcv.stipple = (XID)xv_get(hw->hwMGlassImage,XV_XID);
	gcv.clip_mask = (XID)xv_get(hw->hwMGlassMask,XV_XID);
	hw->hwImageGC = XCreateGC(display,RootWin(hw->hwScreenNo),
		GCForeground|GCBackground|GCFillStyle|GCStipple|GCClipMask,&gcv);

	return True;
}

/* ----------------------------------------------------------------------
 *	ResetHelpWindow
 * ---------------------------------------------------------------------*/
static	void
ResetHelpWindow(hw)
	HelpWindow	*hw;
{
	textsw_reset(hw->hwText,0,0);
}

/* ----------------------------------------------------------------------
 *	DestroyHelpWindow	- Destroy the help window
 * ---------------------------------------------------------------------*/
static Notify_value
DestroyHelpWindow(client,status)
	Notify_client	client;
	Destroy_status	status;
{
	HelpWindow	*hw;

	if (status != DESTROY_CHECKING) {
		hw = (HelpWindow *)xv_get(client,XV_KEY_DATA,HELPWIN_KEY);
		if (hw) {
			xv_destroy(hw->hwMGlassRender);
			hw->hwMGlassRender = (Server_image)NULL;
			xv_destroy(hw->hwMGlassImage);
			xv_destroy(hw->hwMGlassMask);
			XFreeGC(display,hw->hwFillGC);
			XFreeGC(display,hw->hwCopyGC);
			XFreeGC(display,hw->hwImageGC);
			hw->hwFrame = (Frame)NULL;
		}
	}
	return (notify_next_destroy_func(client,status));
}

/* ----------------------------------------------------------------------
 *	TextHelpWindow		- Inserts the help text.
 * ---------------------------------------------------------------------*/
static	Bool
TextHelpWindow(hw,helpkey)
	HelpWindow	*hw;
	char		*helpkey;
{
	char	*moreHelpCmd;
	char	*helpText;
	int	i;
	
	/* 	
	 *	Find the appropriate help file
	 */
	if (help_get_arg(helpkey,&moreHelpCmd) != XV_OK) {
		return False;
	}

	/*
	 *	If there is a MoreHelp command string save it
	 *	and set the visibility of the More help button to match
	 */
	if (hw->hwMoreHelpCmd)
		MemFree(hw->hwMoreHelpCmd);

	if (moreHelpCmd) {
		hw->hwMoreHelpCmd = MemNewString(moreHelpCmd);
		xv_set(hw->hwMoreHelp,XV_SHOW,TRUE,0);
	} else {
		hw->hwMoreHelpCmd = (char *)NULL;
		xv_set(hw->hwMoreHelp,XV_SHOW,FALSE,0);
	}

	/*
 	 *	Insert the help text into the text window
	 */
	helpText = help_get_text();
	for (i=0; helpText; i++) {
		(void)textsw_insert(hw->hwText,helpText,strlen(helpText));
		helpText = help_get_text();
	}

	/*
	 *	Scroll back to the top and only show the scrollbar
	 *	if there are more than HELPTEXTLINES number of lines.
	 */
	xv_set(hw->hwText,
		TEXTSW_FIRST,		0,
		NULL);
	xv_set(hw->hwTextScroll,
		XV_SHOW,		i>HELPTEXTLINES,
		NULL);
	
	return True;
}

/* ----------------------------------------------------------------------
 *	ImageHelpWindow
 * ---------------------------------------------------------------------*/
static	Bool
ImageHelpWindow(hw,mousex,mousey)
	HelpWindow	*hw;
{
	XImage		*scrimage;
	XID		renderxid;
	int		srcx,srcy;

	ConstrainMousePos(hw->hwScreenNo,mousex,mousey,&srcx,&srcy);

	scrimage = XGetImage(display,RootWin(hw->hwScreenNo),srcx,srcy,
		HELP_IMAGE_WIDTH,HELP_IMAGE_HEIGHT,AllPlanes,ZPixmap);

	if (!hw->hwMGlassRender) {
		hw->hwMGlassRender = (Server_image)xv_create(
			XVScreen(hw->hwScreenNo),SERVER_IMAGE,
			XV_WIDTH,		hwmglass_pr.pr_width,
			XV_HEIGHT,		hwmglass_pr.pr_height,
			SERVER_IMAGE_DEPTH,	scrimage->depth,
			NULL);
	}
	renderxid = (XID)xv_get(hw->hwMGlassRender,XV_XID);

	XPutImage(display,renderxid,hw->hwCopyGC,scrimage,0,0,
		HELP_IMAGE_X,HELP_IMAGE_Y,HELP_IMAGE_WIDTH,HELP_IMAGE_HEIGHT);
	XDestroyImage(scrimage);

	XFillRectangle(display,renderxid,hw->hwImageGC,
		0,0,hwmglass_pr.pr_width,hwmglass_pr.pr_height);

	xv_set(hw->hwMGlassItem,
		PANEL_LABEL_IMAGE,	hw->hwMGlassRender,
		NULL);
}

/* ----------------------------------------------------------------------
 *	ConstrainMousePos
 * ---------------------------------------------------------------------*/
static int
ConstrainMousePos(nscreen,mousex,mousey,imagex,imagey)
	int		nscreen;
	int		mousex,mousey;
	int		*imagex,*imagey;
{
	int		srcx,srcy;

	srcx = mousex - HELP_IMAGE_WIDTH / 2;
	if (srcx < 0) {
		srcx = 0;
	} else if (srcx + HELP_IMAGE_WIDTH > DisplayWidth(display,nscreen)) {
		srcx = DisplayWidth(display,nscreen) - HELP_IMAGE_WIDTH;
	}
	srcy = mousey - HELP_IMAGE_HEIGHT / 2;
	if (srcy < 0) {
		srcy = 0;
	} else if (srcy + HELP_IMAGE_HEIGHT > DisplayHeight(display,nscreen)) {
		srcy = DisplayHeight(display,nscreen) - HELP_IMAGE_HEIGHT;
	}
	*imagex = srcx;
	*imagey = srcy;
}

/* ----------------------------------------------------------------------
 *	MoreHelp
 * ---------------------------------------------------------------------*/
/*ARGSUSED*/
static void
MoreHelp(item,event)
	Panel_item	item;
	Event		*event;
{
	HelpWindow	*hw;

	hw  = (HelpWindow *)xv_get(item,XV_KEY_DATA,HELPWIN_KEY);

	if (hw->hwMoreHelpCmd) {
		putenv(DpyEnvString(hw->hwScreenNo));
		system(hw->hwMoreHelpCmd);
	}

}

/* ----------------------------------------------------------------------
 *	ErrorNotice
 * ---------------------------------------------------------------------*/
static	void
ErrorNotice(nscreen,errmsg,helpkey)
	int		nscreen;
	char		*errmsg;
	char		*helpkey;
{
	Xv_Window	notice_window;
	char		msgbuf[128];

	msgbuf[0] = 0;
	strcat(msgbuf,errmsg);
	strcat(msgbuf,helpkey);
	notice_window = xv_create(XVRoot(nscreen),FRAME,
		XV_X,		-100,
		XV_Y,		-100,
		XV_WIDTH,	1,
		XV_HEIGHT,	1,
		XV_SHOW,	TRUE,
		0);
	notice_prompt(notice_window,(Event *)NULL,
		NOTICE_MESSAGE_STRINGS,	msgbuf, NULL,
		NOTICE_BUTTON_YES,	helpStrings[HS_OK],
		NULL);
}
