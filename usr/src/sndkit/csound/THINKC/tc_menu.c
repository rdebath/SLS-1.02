/*
 * Macintosh front end user interface for CSound. The front end provides
 * a somewhat standard Mac interface to set up the CSound files and options
 * and then forms a UNIX command line (argc and argv) to invoke CSound.
 * This module contains all the menu and dialog code, as well as all the code
 * needed to set up the command line.
 *
 * TO BE DONE:
 *  This module should probably be broken up into smaller chunks.
 *  Input file should be added to Open dialog.
 *
 * Bill Gardner, August, 1990.
 */
#include	<stdio.h>
#include	<stdlib.h>
#include	<console.h>
#include	<ctype.h>
#include	<pascal.h>
#include	<MenuMgr.h>
#include	<DialogMgr.h>
#include	<QuickDraw.h>
#include	<WindowMgr.h>
#include	<SegmentLdr.h>
#include	<ResourceMgr.h>
#include	<MemoryMgr.h>
#include	<EventMgr.h>
#include	"tc_misc.h"

/*
 * Set this flag to TRUE if you want to test the CSound front end by itself.
 */
#define FRONT_END_ONLY	FALSE

/*
 * Menu IDs.
 */
#define APPLE_ID		1		/* Apple */

#define FILE_ID			2		/* File (really "CSound") */
#define F_OPEN			1
#define F_OPTIONS		3
#define F_OUTPUT_FMT	4
#define F_SFDIR			5
#define F_COMMAND		6
#define F_SAVE			8
#define F_QUIT			9

#define EDIT_ID			3		/* Edit */

/*
 * Dialog IDs and item numbers
 */
#define ABOUT_ID		128

#define OPEN_DLG_ID		129
#define OPEN_SEL_SCO	3
#define OPEN_SCO		4
#define OPEN_SEL_ORC	5
#define OPEN_ORC		6
#define OPEN_SEL_SND	9		/* really "SFDir" */
#define OPEN_SND		10
#define OPEN_CHK_LST	14
#define OPEN_SEL_LST	12
#define OPEN_LST		13
#define OPEN_CHK_EXT	17
#define OPEN_SEL_EXT	15
#define OPEN_EXT		16
#define OPEN_SMP_FMT	18
#define OPEN_OPTS		19

#define FMT_DLG_ID		130
#define FMT_8			4
#define FMT_ALAW		5
#define FMT_ULAW		6
#define FMT_16			7
#define FMT_32			8
#define FMT_32F			9
#define FMT_NOHDR		10
#define FMT_BLKSIZE		12

#define OPT_DLG_ID		131
#define OPT_VERBOSE		4
#define OPT_NOTES		6
#define OPT_RANGE		7
#define OPT_WARNINGS	8
#define OPT_QUIET		9
#define OPT_INIT		10
#define OPT_NOOUT		11

#define CMD_DLG_ID		132
#define CMD_CMD			3

#define SFD_DLG_ID		133
#define SFD_SELECT		3
#define SFD_SFDIR		4
#define SFD_SAVE		6

#define ENABLE_CTL		0
#define DISABLE_CTL		255

/*
 * Currently selected files. The score and orchestra files are necessary,
 * and each may come from a different directory. The output file is
 * usually generated, and goes in the "Sound File Directory", which is
 * established with the SFDIR shell environment variable in UNIX
 * implementations, and here with the sfdir_path[] pathname.
 * The extract and listing files are optional and may be in any directory.
 */
typedef struct {
	char sco[128];		/* score file name */
	short sco_vrn;		/* score volume reference number */
	char orc[128];		/* orchestra file name */
	short orc_vrn;		/* orchestra volume reference number */
	char snd[128];		/* output sound file name */
	char lst[128];		/* listing (stdout) file name */
	short lst_vrn;		/* listing file volume reference number */
	char ext[128];		/* extract file name */
	short ext_vrn;		/* extract file volume refernce number */
	int flags;			/* flags for validity of files */
} SO_FILE;

#define SCO_VALID		0x01	/* set if score file valid */
#define ORC_VALID		0x02	/* set if orchestra file valid */
#define LST_VALID		0x08	/* set if there's a listing file */
#define EXT_VALID		0x10	/* set if there's an extract file */
#define SND_VREF_VALID	0x20	/* set if snd vrefnum (sfdir) valid */

SO_FILE so_file = {
	"", 0,
	"", 0,
	"", 0,
	"", 0,
	"", 0,
	0
};

/*
 * Option flags
 */
#define OF_VERBOSE		0x01
#define OF_NOTES		0x02
#define OF_RANGE		0x04
#define OF_WARNINGS		0x08
#define OF_QUIET		0x10
#define OF_INIT			0x40
#define OF_NOOUT		0x80

int opt_flags;

/*
 * Current command line.
 */
char cmd_line[512] = "";

/*
 * Output format.
 */
typedef struct {
	int type;		/* output format type - same as item number */
	int	nohdr;		/* TRUE if no header */
	int blksize;	/* blocksize */
} OUT_FMT;

OUT_FMT out_fmt;

/*
 * Sound file directory path. The sound file directory is where CSound
 * looks for input sample files and creates output sample files.
 */
char sfdir_path[256];

/*
 * Configuration recource structure. Everything that gets remembered
 * between invocations must go here.
 */
#define	CONFIG_TYPE	((long)'CSND')
#define CONFIG_ID	128

typedef struct {
	int	opt_flags;
	OUT_FMT out_fmt;
	char sfdir_path[256];
} CONFIG;

CONFIG **Config;

#define FILE_TYPE		((long) 'TEXT')
#define FILE_CREATOR	((long) 'VRCO')

/*
 * argv and argc for passing to CSound main().
 */
#define MAX_ARG		32
static char *argv[MAX_ARG];
static int argc;

/*
 * Text that appears in "About CSound..." dialog.
 */
char *info[] = {
"\p Copyright 1990 by the Massachusetts Institute of Technology.",
"\p                    All rights reserved.",
"\p",
"\pDeveloped by Barry L. Vercoe at the Music and Cognition Group,",
"\p     Media Laboratory, M.I.T., Cambridge, Massachusetts,",
"\p with partial support from the System Development Foundation",
"\p and from the National Science Foundation Grant #IRI-8704665.",
"\p",
"\p       Macintosh version by Dan Ellis and Bill Gardner,",
"\p          with assistance from Dr. Richard Boulanger."
};
#define N_INFO (sizeof(info) / sizeof(char *))

/*
 * Process "About CSound..." dialog.
 */
doAboutBox()
{
DialogPtr dp;
short itemHit;
short itemType;
Rect itemBox;
Handle item;
FontInfo fi;
int height;
int i;
	dp = GetNewDialog(ABOUT_ID, 0, -1);
	SetPort(dp);
	TextFont(monaco);
	TextSize(9);
	GetFontInfo(&fi);
	height = fi.ascent + fi.descent + fi.leading;
	GetDItem(dp,2,&itemType,&item,&itemBox);
	for (i = 0; i < N_INFO; i++) {
		MoveTo(itemBox.left,itemBox.top + (i + 1) * height);
		DrawString(info[i]);
	}
	TextFont(systemFont);
	TextSize(12);
	ModalDialog(0, &itemHit);
	DisposDialog(dp);
}

/*
 * Returns encoded selection for File Open menu item.
 */
long fileOpenSelect()
{
	return (((long) FILE_ID) << 16) | F_OPEN;
}

/*
 * Process menu event.
 */
DoMenuClick(select)
long select;
{
int menu;				/* menu ID */
int item;				/* menu item */
char itemName[64];		/* item name for desk acc. */
GrafPtr port;
	
	if (select == 0) return;
	
	menu = HiWord(select);
	item = LoWord(select);
	
	HiliteMenu(menu);
	switch(menu) {
	case APPLE_ID:
		if (item > 1) {
			GetPort(&port);
			GetItem(GetMenu(APPLE_ID), item, itemName);
			OpenDeskAcc(itemName);
			SetPort(port);
		}
		else {
			GetPort(&port);
			doAboutBox();
			SetPort(port);
		}
		break;	

	case FILE_ID:
		cursWait();
		switch(item) {
		case F_OPEN:
			menuFileOpen();
			break;
		case F_OUTPUT_FMT:
			menuFileOutputFmt();
			break;
		case F_OPTIONS:
			menuFileOptions();
			coerceFileMenu();
			break;
		case F_SFDIR:
			menuFileSFDir();
			break;
		case F_COMMAND:
			menuFileCommand();
			break;
		case F_SAVE:
			saveConfig();
			break;
		default:
		case F_QUIT:
			ExitToShell();
			break;
		}
		cursPop();
		break;

	case EDIT_ID:
		switch(item) {
		case 1: case 3: case 4: case 5: case 6:
			SystemEdit(item-1);
			break;
		}
		break;
	default:
		break;
	}
	HiliteMenu(0);
}

/*
 * Returns TRUE if output samples to be generated, else FALSE.
 */
int isOutput()
{
	return !(opt_flags & OF_NOOUT);
}

/*
 * Enable/Disable Output Format menu item.
 */
coerceFileMenu()
{
MenuHandle h;
	h = GetMHandle(FILE_ID);
	if (isOutput())
		EnableItem(h,F_OUTPUT_FMT);
	else
		DisableItem(h,F_OUTPUT_FMT);
}

/*
 * Initialize menus.
 */
setupMenuBar()
{
	SetMenuBar(GetNewMBar(1));
	AddResMenu(GetMHandle(1), 'DRVR');
	coerceFileMenu();
	DrawMenuBar();
}

/*
 * Set the contents of a dialog text item via sprintf.
 */
SetDlgStrItem(dp,itemHit,fmt,a,b)
DialogPtr dp;
short itemHit;
char *fmt;
long a, b;		/* variable args HACK */
{
short itemType;
Rect itemBox;
Handle item;
char buf[256];
	GetDItem(dp,itemHit,&itemType,&item,&itemBox);
	sprintf(buf,fmt,a,b);
	SetIText(item,CtoPstr(buf));
}

/*
 * Scan the contents of a dialog text item via sscanf.
 */
GetDlgStrItem(dp,itemHit,fmt,p)
DialogPtr dp;
short itemHit;
char *fmt;
char *p;
{
short itemType;
Rect itemBox;
Handle item;
char buf[256];
	GetDItem(dp,itemHit,&itemType,&item,&itemBox);
	GetIText(item,buf);
	PtoCstr(buf);
	sscanf(buf,fmt,p);
}

/*
 * Get the contents of a dialog string item. Note that
 * you can't use GetDlgStrItem() if the text contains spaces.
 */
GetDlgString(dp,itemHit,p)
DialogPtr dp;
short itemHit;
char *p;			/* C string */
{
short itemType;
Rect itemBox;
Handle item;
	GetDItem(dp,itemHit,&itemType,&item,&itemBox);
	GetIText(item,p);
	PtoCstr(p);
}

/*
 * Process "Open Score and Orchestra..." menu item.
 */
menuFileOpen()
{
int stat;
	if (dlgFileOpen(&so_file)) {
		formCmdLine();
		parseCmdLine();
		callCSound();
	}
}

/*
 * Replaces ".suffix" with x.
 */
addFileExt(s,x)
char *s;
char *x;
{
char *t;
	for (t = s; *t && *t != '.'; t++);
	strcpy(t, x);
}

/*
 * Returns TRUE if file exists, else FALSE.
 */
int exists(fname,vrefnum)
char *fname;
short vrefnum;
{
char path[256];
	return getPath(path,fname,vrefnum) == noErr;
}

/*
 * Called after the score file has been set up to form an output file name.
 * If dp provided, set up the edit text item for the output file.
 */
formOutputFile(dp,p)
DialogPtr dp;
SO_FILE *p;
{
	strcpy(p->snd,p->sco);
	addFileExt(p->snd,".snd");
	if (dp) {
		SetDlgStrItem(dp,OPEN_SND,"%s",p->snd);
		SelIText(dp,OPEN_SND,0,32767);
	}
}

/*
 * Called after score file set up.
 */
setScoreFile(dp,p)
DialogPtr dp;
SO_FILE *p;
{
	p->flags |= SCO_VALID;
	if (!(p->flags & ORC_VALID)) {
		/*
		 * Synthesize probable name of orchestra file and test for
		 * existence. If found, make orchestra file valid.
		 */
		strcpy(p->orc,p->sco);
		p->orc_vrn = p->sco_vrn;
		addFileExt(p->orc,".orc");
		if (exists(p->orc,p->orc_vrn))
			p->flags |= ORC_VALID;
	}
	formOutputFile(dp,p);
}

/*
 * Called after orchestra file set up.
 */
setOrchestraFile(dp,p)
DialogPtr dp;
SO_FILE *p;
{
	p->flags |= ORC_VALID;
	if (!(p->flags & SCO_VALID)) {
		/*
		 * Synthesize probable name of score file and test for
		 * existence. If found, make score file valid.
		 */
		strcpy(p->sco,p->orc);
		p->sco_vrn = p->orc_vrn;
		addFileExt(p->sco,".sco");
		if (exists(p->sco,p->sco_vrn)) {
			p->flags |= SCO_VALID;
			formOutputFile(dp,p);
		}
	}
}

static int ok_enabled;		/* TRUE if allow \r to mean OK button */

/*
 * Event filter for dlgFileOpen() modal dialog.
 */
static pascal Boolean myFilter(dp,ev,itemHit)
DialogPtr dp;
EventRecord *ev;
int *itemHit;
{
#define ENTER 0x03
	if (ev->what == keyDown)
		switch(ev->message & charCodeMask) {
		case '\r':
		case ENTER:
			*itemHit = OK;
			return ok_enabled;
		default:
			return FALSE;
		}
	else return FALSE;
}

/*
 * Process "Open Score and Orchestra..." dialog.
 */
int dlgFileOpen(so)
SO_FILE *so;
{
DialogPtr dp;
short itemHit;
short itemType;
Rect itemBox;
Handle item;
int InDialog = TRUE;
GrafPtr port;
int stat;
SO_FILE tmp;
int dispFlag = TRUE;

	/* copy argument to temporary */
	tmp = *so;

	GetPort(&port);
	if ((dp = GetNewDialog(OPEN_DLG_ID, NULL, -1)) == NULL) return FALSE;
	SetPort(dp);

	if (tmp.snd[0]) {
		SetDlgStrItem(dp,OPEN_SND,"%s",tmp.snd);
		SelIText(dp,OPEN_SND,0,32767);
	}

	cursNorm();
	while(InDialog) {

		if (dispFlag) {
			/* frame rectangles around static text items */
			GetDItem(dp,OPEN_SCO,&itemType,&item,&itemBox);
			InsetRect(&itemBox,-3,-3);
			FrameRect(&itemBox);
			GetDItem(dp,OPEN_ORC,&itemType,&item,&itemBox);
			InsetRect(&itemBox,-3,-3);
			FrameRect(&itemBox);
			GetDItem(dp,OPEN_LST,&itemType,&item,&itemBox);
			InsetRect(&itemBox,-3,-3);
			FrameRect(&itemBox);
			GetDItem(dp,OPEN_EXT,&itemType,&item,&itemBox);
			InsetRect(&itemBox,-3,-3);
			FrameRect(&itemBox);
		
			/* display current parameters */
			if (tmp.flags & SCO_VALID) SetDlgStrItem(dp,OPEN_SCO,"%s",tmp.sco);
			if (tmp.flags & ORC_VALID) SetDlgStrItem(dp,OPEN_ORC,"%s",tmp.orc);
	
			if (tmp.flags & LST_VALID) {
				SetDlgStrItem(dp,OPEN_LST,"%s",tmp.lst);
				GetDItem(dp,OPEN_CHK_LST,&itemType,&item,&itemBox);
				SetCtlValue(item,1);
				GetDItem(dp,OPEN_SEL_LST,&itemType,&item,&itemBox);
				HiliteControl(item, ENABLE_CTL);
			}
			else {
				SetDlgStrItem(dp,OPEN_LST,"",tmp.lst);
				GetDItem(dp,OPEN_CHK_LST,&itemType,&item,&itemBox);
				SetCtlValue(item,0);
				GetDItem(dp,OPEN_SEL_LST,&itemType,&item,&itemBox);
				HiliteControl(item, DISABLE_CTL);
			}
	
			if (tmp.flags & EXT_VALID) {
				SetDlgStrItem(dp,OPEN_EXT,"%s",tmp.ext);
				GetDItem(dp,OPEN_CHK_EXT,&itemType,&item,&itemBox);
				SetCtlValue(item,1);
				GetDItem(dp,OPEN_SEL_EXT,&itemType,&item,&itemBox);
				HiliteControl(item, ENABLE_CTL);
			}
			else {
				SetDlgStrItem(dp,OPEN_EXT,"",tmp.ext);
				GetDItem(dp,OPEN_CHK_EXT,&itemType,&item,&itemBox);
				SetCtlValue(item,0);
				GetDItem(dp,OPEN_SEL_EXT,&itemType,&item,&itemBox);
				HiliteControl(item, DISABLE_CTL);
			}
	
			/*
			 * Enable OK button only if sco/orc files valid, else disable.
			 * The ok_enabled flag and the custom event filter are needed
			 * because ModalDialog does not check for the OK button
			 * being disabled.
			 */
			GetDItem(dp,OK,&itemType,&item,&itemBox);
			if ((tmp.flags & SCO_VALID) && (tmp.flags & ORC_VALID)) {
				HiliteControl(item, ENABLE_CTL);
				ok_enabled = TRUE;
			}
			else {
				HiliteControl(item, DISABLE_CTL);
				ok_enabled = FALSE;
			}
	
			/*
			 * Enable Smp Fmt button only if generating output. Note that
			 * the Output file must still be enabled in order to select sfdir.
			 */
			GetDItem(dp,OPEN_SMP_FMT,&itemType,&item,&itemBox);
			HiliteControl(item, isOutput() ? ENABLE_CTL : DISABLE_CTL);
		}
		dispFlag = TRUE;

		ModalDialog(myFilter,&itemHit);
		switch(itemHit) {
		case OK:
			stat = TRUE;
			InDialog = FALSE;
			break;
		case Cancel:
			stat = FALSE;
			InDialog = FALSE;
			break;
		case OPEN_SEL_SCO:
			if (getFile(tmp.sco,&tmp.sco_vrn,1,(long)'TEXT'))
				setScoreFile(dp,&tmp);
			break;
		case OPEN_SEL_ORC:
			if (getFile(tmp.orc,&tmp.orc_vrn,1,(long)'TEXT'))
				setOrchestraFile(dp,&tmp);
			break;
		case OPEN_SEL_SND:		/* really SFDir... */
			menuFileSFDir();
			break;
		case OPEN_SND:
			dispFlag = FALSE;	/* prevent flickering while typing */
			break;
		case OPEN_CHK_LST:
			if (tmp.flags & LST_VALID) {
				tmp.flags &= ~LST_VALID;
				break;
			}
			if ((tmp.flags & SCO_VALID) && (!(tmp.flags & LST_VALID))) {
				strcpy(tmp.lst,tmp.sco);
				addFileExt(tmp.lst,".lst");
				tmp.flags |= LST_VALID;
			}
			/* FALLS THROUGH! */
		case OPEN_SEL_LST:
			if (putFile(NULL,tmp.lst,&tmp.lst_vrn))
				tmp.flags |= LST_VALID;
			break;
		case OPEN_CHK_EXT:
			if (tmp.flags & EXT_VALID) {
				tmp.flags &= ~EXT_VALID;
				break;
			}	/* else FALLS THROUGH! */
		case OPEN_SEL_EXT:
			if (getFile(tmp.ext,&tmp.ext_vrn,1,(long)'TEXT'))
				tmp.flags |= EXT_VALID;
			break;
		case OPEN_SMP_FMT:
			menuFileOutputFmt();
			break;
		case OPEN_OPTS:
			menuFileOptions();
			break;
		default:
			break;
		}
	}
	cursPop();

	if (stat) {
		GetDlgString(dp,OPEN_SND,&tmp.snd);
		if (!tmp.snd[0]) formOutputFile(NULL,&tmp);
	}
	*so = tmp;		/* always save results of dialog */
	DisposDialog(dp);
	SetPort(port);
	return stat;
}

/*
 * Returns TRUE if s ends with ".orc" or ".ORC", else FALSE.
 */
isOrcFile(s)
char *s;
{
	while (*s && *s != '.') s++;
	return !strcmp(s,".orc") ? TRUE : !strcmp(s,".ORC");
}

/*
 * Process any files opened with application. For each file, determine
 * if it is a score file or an orchestra file, and set up globals.
 * Returns TRUE if any files processed, else FALSE.
 */
int processAppFiles()
{
short msg;
short count;
int i;
AppFile af;
	CountAppFiles(&msg,&count);
	if (count <= 0) return FALSE;
	for (i = 1; i <= count; i++) {
		GetAppFiles(i,&af);
		PtoCstr((char *)af.fName);
		if (isOrcFile(af.fName)) {
			strcpy(so_file.orc,af.fName);
			so_file.orc_vrn = af.vRefNum;
			setOrchestraFile(NULL,&so_file);
		}
		else {
			strcpy(so_file.sco,af.fName);
			so_file.sco_vrn = af.vRefNum;
			setScoreFile(NULL,&so_file);
		}
	}
	return TRUE;
}

/*
 * Process "Options..." menu item.
 */
menuFileOptions()
{
	dlgFileOptions(&opt_flags);
}

/*
 * Process "Options..." dialog.
 */
dlgFileOptions(flags)
int *flags;
{
DialogPtr dp;
short itemHit;
short itemType;
Rect itemBox;
Handle item;
int InDialog = TRUE;
GrafPtr port;
int stat;
int tmp;

	/* copy argument to temporary */
	tmp = *flags;

	GetPort(&port);
	if ((dp = GetNewDialog(OPT_DLG_ID, NULL, -1)) == NULL) return FALSE;
	SetPort(dp);

	cursNorm();
	while(InDialog) {
		/* display flags in checkboxes */
		GetDItem(dp,OPT_VERBOSE,&itemType,&item,&itemBox);
		SetCtlValue(item,(tmp & OF_VERBOSE) ? 1 : 0);
		GetDItem(dp,OPT_NOTES,&itemType,&item,&itemBox);
		SetCtlValue(item,(tmp & OF_NOTES) ? 1 : 0);
		GetDItem(dp,OPT_RANGE,&itemType,&item,&itemBox);
		SetCtlValue(item,(tmp & OF_RANGE) ? 1 : 0);
		GetDItem(dp,OPT_WARNINGS,&itemType,&item,&itemBox);
		SetCtlValue(item,(tmp & OF_WARNINGS) ? 1 : 0);
		GetDItem(dp,OPT_QUIET,&itemType,&item,&itemBox);
		SetCtlValue(item,(tmp & OF_QUIET) ? 1 : 0);
		GetDItem(dp,OPT_INIT,&itemType,&item,&itemBox);
		SetCtlValue(item,(tmp & OF_INIT) ? 1 : 0);
		/*
		 * If INIT flag set, set and disable the "no output" control,
		 * but do not change its option flag bit.
		 */
		GetDItem(dp,OPT_NOOUT,&itemType,&item,&itemBox);
		if (tmp & OF_INIT) {
			HiliteControl(item,DISABLE_CTL);
			SetCtlValue(item,1);
		}
		else HiliteControl(item,ENABLE_CTL);
		GetDItem(dp,OPT_NOOUT,&itemType,&item,&itemBox);
		SetCtlValue(item,(tmp & OF_NOOUT) ? 1 : 0);

		ModalDialog(NULL,&itemHit);
		switch(itemHit) {
		case OK:
			stat = TRUE;
			InDialog = FALSE;
			break;
		case Cancel:
			stat = FALSE;
			InDialog = FALSE;
			break;
		case OPT_VERBOSE:
			tmp ^= OF_VERBOSE;
			break;
		case OPT_NOTES:
			tmp ^= OF_NOTES;
			break;
		case OPT_RANGE:
			tmp ^= OF_RANGE;
			break;
		case OPT_WARNINGS:
			tmp ^= OF_WARNINGS;
			break;
		case OPT_QUIET:
			tmp ^= OF_QUIET;
			break;
		case OPT_INIT:
			tmp ^= OF_INIT;
			break;
		case OPT_NOOUT:
			tmp ^= OF_NOOUT;
			break;
		default:
			break;
		}
	}
	cursPop();

	if (stat) {
		if (tmp & OF_INIT) tmp |= OF_NOOUT;
		*flags = tmp;
	}
	DisposDialog(dp);
	SetPort(port);
	return stat;
}

/*
 * Process "Output Sample Format..." menu item.
 */
menuFileOutputFmt()
{
	dlgFileOutputFmt(&out_fmt);
}

/*
 * Process "Output Sample Format..." dialog.
 */
dlgFileOutputFmt(of)
OUT_FMT *of;
{
DialogPtr dp;
short itemHit;
short itemType;
Rect itemBox;
Handle item;
int InDialog = TRUE;
GrafPtr port;
int stat;
OUT_FMT tmp;

	/* copy argument to temporary */
	tmp = *of;

	GetPort(&port);
	if ((dp = GetNewDialog(FMT_DLG_ID, NULL, -1)) == NULL) return FALSE;
	SetPort(dp);

	/* display current parameters */
	SetDlgStrItem(dp,FMT_BLKSIZE,"%d",tmp.blksize);
	SelIText(dp,FMT_BLKSIZE,0,32767);

	cursNorm();
	while(InDialog) {

		/* display format buttons and nohdr checkbox */
		GetDItem(dp,FMT_8,&itemType,&item,&itemBox);
		SetCtlValue(item,tmp.type == FMT_8);
		GetDItem(dp,FMT_ALAW,&itemType,&item,&itemBox);
		SetCtlValue(item,tmp.type == FMT_ALAW);
		HiliteControl(item,DISABLE_CTL);
		GetDItem(dp,FMT_ULAW,&itemType,&item,&itemBox);
		SetCtlValue(item,tmp.type == FMT_ULAW);
		GetDItem(dp,FMT_16,&itemType,&item,&itemBox);
		SetCtlValue(item,tmp.type == FMT_16);
		GetDItem(dp,FMT_32,&itemType,&item,&itemBox);
		SetCtlValue(item,tmp.type == FMT_32);
		GetDItem(dp,FMT_32F,&itemType,&item,&itemBox);
		SetCtlValue(item,tmp.type == FMT_32F);
		GetDItem(dp,FMT_NOHDR,&itemType,&item,&itemBox);
		SetCtlValue(item,tmp.nohdr ? 1 : 0);

		ModalDialog(NULL,&itemHit);
		switch(itemHit) {
		case OK:
			stat = TRUE;
			InDialog = FALSE;
			break;
		case Cancel:
			stat = FALSE;
			InDialog = FALSE;
			break;
		case FMT_8:
			tmp.type = FMT_8;
			break;
		case FMT_ALAW:
			tmp.type = FMT_ALAW;
			break;
		case FMT_ULAW:
			tmp.type = FMT_ULAW;
			break;
		case FMT_16:
			tmp.type = FMT_16;
			break;
		case FMT_32:
			tmp.type = FMT_32;
			break;
		case FMT_32F:
			tmp.type = FMT_32F;
			break;
		case FMT_NOHDR:
			tmp.nohdr = !tmp.nohdr;
			break;
		default:
			break;
		}
	}
	cursPop();

	if (stat) {
		GetDlgStrItem(dp,FMT_BLKSIZE,"%d",&tmp.blksize);
		*of = tmp;
	}
	DisposDialog(dp);
	SetPort(port);
	return stat;
}

/*
 * Process "Sound File Directory..." menu item.
 */
menuFileSFDir()
{
	if (dlgFileSFDir(sfdir_path) == SFD_SAVE)
		saveConfig();
}

/*
 * Process "Sound File Directory..." dialog.
 */
dlgFileSFDir(sfdir)
char *sfdir;
{
DialogPtr dp;
short itemHit;
short itemType;
Rect itemBox;
Handle item;
int InDialog = TRUE;
GrafPtr port;
int stat;
short vrefnum;
char buf[256];

	GetPort(&port);
	if ((dp = GetNewDialog(SFD_DLG_ID, NULL, -1)) == NULL) return FALSE;
	SetPort(dp);

	/* display current parameters */
	SetDlgStrItem(dp,SFD_SFDIR,"%s",sfdir);
	SelIText(dp,SFD_SFDIR,0,32767);

	cursNorm();
	while(InDialog) {

		ModalDialog(NULL,&itemHit);
		switch(itemHit) {
		case OK:
			stat = TRUE;
			InDialog = FALSE;
			break;
		case Cancel:
			stat = FALSE;
			InDialog = FALSE;
			break;
		case SFD_SELECT:
			strcpy(buf,"Filename doesn't matter");
			if (putFile("\pSelect SFDir:",buf,&vrefnum)) {
				getPath(buf,NULL,vrefnum);
				SetDlgStrItem(dp,SFD_SFDIR,"%s",buf);
				SelIText(dp,SFD_SFDIR,0,32767);
			}
			break;
		case SFD_SAVE:
			stat = SFD_SAVE;
			InDialog = FALSE;
			break;
		case SFD_SFDIR:
		default:
			break;
		}
	}
	cursPop();

	if (stat)
		GetDlgString(dp,SFD_SFDIR,sfdir);
	DisposDialog(dp);
	SetPort(port);
	return stat;
}

/*
 * Free any existing arguments in argv[].
 */
freeArgs()
{
int i;
	for (i = 0; i < argc; i++) {
		free(argv[i]);
		argv[i] = 0;
	}
	argc = 0;
}

/*
 * Append the supplied argument to the current argv[] array.
 */
appendArg(s)
char *s;
{
char *arg;
	if (argc < MAX_ARG) {
		if ((arg = malloc((size_t)strlen(s)+1)) == NULL) {
			SysBeep(10L);
			return;
		}
		strcpy(arg,s);
		argv[argc++] = arg;
	}
	else SysBeep(10L);
}

/*
 * Returns TRUE if string contains a space, else FALSE.
 */
int spaceInStr(s)
char *s;
{
	while (*s) {
		if (isspace(*s)) return TRUE;
		s++;
	}
	return FALSE;
}

/*
 * Append the (fmt, args) to the current command line using sprintf.
 */
appendCmd(fmt,a,b)
char *fmt;
long a, b;	/* variable args (HACK) */
{
	sprintf(&cmd_line[strlen(cmd_line)],fmt,a,b);
}

/*
 * Using the current options and files, create a UNIX-style command line
 * to invoke CSound. If pathnames contain spaces, double-quote them.
 */
formCmdLine()
{
int msg;
int fmt_char;
char path[256];
#define MSG_DEFAULT	7
	cmd_line[0] = 0;
	/*
	 * program name
	 */
	appendCmd("Csound ");
	/*
	 * options
	 */
	if (opt_flags & OF_VERBOSE) appendCmd("-v ");
	msg = 0;
	if (opt_flags & OF_NOTES) msg += 1;
	if (opt_flags & OF_RANGE) msg += 2;
	if (opt_flags & OF_WARNINGS) msg += 4;
	if (msg != MSG_DEFAULT) appendCmd("-m%d ",msg);
	if (opt_flags & OF_QUIET) appendCmd("-d ");
	if (opt_flags & OF_INIT) appendCmd("-I ");
	else if (opt_flags & OF_NOOUT) appendCmd("-n ");
	/*
	 * output format
	 */
	switch (out_fmt.type) {
	case FMT_8:
		fmt_char = 'c';
		break;
	case FMT_ALAW:
		fmt_char = 'a';
		break;
	case FMT_ULAW:
		fmt_char = 'u';
		break;
	default:
	case FMT_16:
		fmt_char = 's';
		break;
	case FMT_32:
		fmt_char = 'l';
		break;
	case FMT_32F:
		fmt_char = 'f';
		break;
	}
	appendCmd("-%c ",fmt_char);
	if (out_fmt.nohdr) appendCmd("-h ");
	appendCmd("-b%d\r",out_fmt.blksize);
	/*
	 * Score, Orchestra, output, and extract files.
	 */
	if (so_file.flags & ORC_VALID) {
		getPath(path,so_file.orc,so_file.orc_vrn);
		if (spaceInStr(path)) appendCmd("\"%s\"\r",path);
		else appendCmd("%s\r",path);
	}
	if (so_file.flags & SCO_VALID) {
		getPath(path,so_file.sco,so_file.sco_vrn);
		if (spaceInStr(path)) appendCmd("\"%s\"\r",path);
		else appendCmd("%s\r",path);
	}
	if (isOutput() && so_file.snd[0]) {
		strcpy(path,so_file.snd);
		if (spaceInStr(path)) appendCmd("\"-o%s\"\r",path);
		else appendCmd("-o%s\r",path);
	}
	if (so_file.flags & EXT_VALID) {
		getPath(path,so_file.ext,so_file.ext_vrn);
		if (spaceInStr(path)) appendCmd("\"-x%s\"\r",path);
		else appendCmd("-x%s\r",path);
	}
}

/*
 * Uses state machine to parse command lines into space separated arguments.
 * Handles double quotes to enclose spaces within an argument.
 */
parseCmdLine()
{
enum {
	read_spaces,
	read_chars,
	read_quotes
} state = read_spaces;
char *t;
char *s;
char buf[256];
int running = TRUE;
	freeArgs();
	s = cmd_line;
	while (running) {
		switch (state) {
		case read_spaces:
			if (*s == 0) running = FALSE;
			else if (isspace(*s)) s++;
			else {
				t = buf;
				*t = 0;
				state = read_chars;
			}
			break;
		case read_chars:
			if (isspace(*s) || *s == 0) {
				*t = 0;
				appendArg(buf);
				state = read_spaces;
			}
			else if (*s == '"') {
				s++;
				state = read_quotes;
			}
			else *t++ = *s++;
			break;
		case read_quotes:
			if (*s == 0) state = read_chars;
			else if (*s == '"') {
				s++;
				state = read_chars;
			}
			else *t++ = *s++;
			break;
		}
	}
}

/*
 * Process "Enter Command Line..." dialog.
 */
dlgFileCommand(cl)
char *cl;
{
DialogPtr dp;
short itemHit;
int InDialog = TRUE;
GrafPtr port;
int stat;

	GetPort(&port);
	if ((dp = GetNewDialog(CMD_DLG_ID, NULL, -1)) == NULL) return FALSE;
	SetPort(dp);

	/* display current parameters */
	SetDlgStrItem(dp,CMD_CMD,"%s",cl);
	SelIText(dp,CMD_CMD,0,32767);

	cursNorm();
	while(InDialog) {

		ModalDialog(NULL,&itemHit);
		switch(itemHit) {
		case OK:
			stat = TRUE;
			InDialog = FALSE;
			break;
		case Cancel:
			stat = FALSE;
			InDialog = FALSE;
			break;
		default:
			break;
		}
	}
	cursPop();

	if (stat) {
		/*
		 * grab cmd
		 */
		GetDlgString(dp,CMD_CMD,cl);
	}
	DisposDialog(dp);
	SetPort(port);
	return stat;
}

/*
 * Change files' creator to 'VRCO'.
 */
chgFileTypes()
{
	if ((so_file.flags & SCO_VALID) && !chgFInfo(so_file.sco,
		so_file.sco_vrn,FILE_TYPE,FILE_CREATOR)) SysBeep(10L);
	if ((so_file.flags & ORC_VALID) && !chgFInfo(so_file.orc,
		so_file.orc_vrn,FILE_TYPE,FILE_CREATOR)) SysBeep(10L);
	if ((so_file.flags & LST_VALID) && !chgFInfo(so_file.lst,
		so_file.lst_vrn,FILE_TYPE,FILE_CREATOR)) SysBeep(10L);
	if ((so_file.flags & EXT_VALID) && !chgFInfo(so_file.ext,
		so_file.ext_vrn,FILE_TYPE,FILE_CREATOR)) SysBeep(10L);
}

/*
 * Redirect stdout to a file. Returns TRUE if OK, FALSE if error.
 */
int redirect_stdout(fname,vrefnum)
char *fname;
short vrefnum;
{
	SetVol(0L,vrefnum);
	return freopen(fname,"w",stdout) != NULL;
}

/*
 * Redirect stdout from file back to console. Returns TRUE if OK,
 * FALSE if error.
 */
int unredirect_stdout()
{
	return freopenc(stderr,stdout) != NULL;
}

/*
 * CSound tc_main() test stub.
 */
#if FRONT_END_ONLY
tc_main(argc,argv)
int argc;
char **argv;
{
int i;
	printf("argc %d\n",argc);
	for (i = 0; i < argc; i++) printf("argv[%d] '%s'\n",i,argv[i]);
}
#endif


static int theyHavePaused = 0;
void MaybePause()	/* plugged into AtExit list */
	{
	if(!theyHavePaused)
		{
		cursNorm();
		SysBeep(10L);
		printf("\n*** PRESS MOUSE BUTTON TO EXIT ***\n");
		while (!Button() && !isAbortEvent());
		}
	}
		
/*
 * Redirect output, and call CSound.
 */
callCSound()
{
	int main_st;
	
	atexit(MaybePause);		/* set up trap for Csound's 'exit()'s */

	/*
	 * Redirect stdout to listing file if necessary. Change file
	 * types of input files. Change cursor to wristwatch. Call
	 * CSound main().
	 */
	if ((so_file.flags & LST_VALID) &&
		!redirect_stdout(so_file.lst,so_file.lst_vrn)) SysBeep(10L);
	chgFileTypes();
	cursWait();
	main_st = tc_main(argc,argv);
	cursPop();
	cursNorm();
	/*
	 * Close listing file, reopen stdout console
	 * if listing file in use.
	 */
	if ((so_file.flags & LST_VALID) && !unredirect_stdout())
		SysBeep(10L);
	/*
	 * Now for something completely different! Because CSound main()
	 * is not re-entrant, we need to quit. Beep, wait for mouse down,
	 * and exit.
	 */
	if(!main_st)
		{
/*		SysBeep(10L);
		printf("\n*** PRESS MOUSE BUTTON TO EXIT ***\n");
		while (!Button() && !isAbortEvent());
		*/
		theyHavePaused = 1;		/* set semaphore for fn called by exit() */
		}
/*	ExitToShell(); */
	exit(0);
	}

/*
 * Process "Enter Command Line..." menu item.
 */
menuFileCommand()
{
	formCmdLine();
	if (dlgFileCommand(cmd_line)) {
		parseCmdLine();
		callCSound();
	}
}

/*
 * Stuff to deal with saving the current settings.
 */

/*
 * Reads configuration resource, creates it if not found.
 */
initConfig()
{
	if ((Config = (CONFIG **)
			GetResource(CONFIG_TYPE,CONFIG_ID)) == NULL) {
		/* create resource */
		if ((Config = (CONFIG **)
				NewHandle((long)sizeof(CONFIG))) == NULL) {
			SysBeep(10L);
			ExitToShell();
		}
		HLock(Config);
		/*
		 * Initialize globals first time here.
		 */
		opt_flags = OF_NOTES | OF_RANGE | OF_WARNINGS;
		out_fmt.type = FMT_16;
		out_fmt.nohdr = TRUE;
		out_fmt.blksize = 4096;
		sfdir_path[0] = 0;
		AddResource(Config,CONFIG_TYPE,CONFIG_ID,"\pCSound settings");
		HUnlock(Config);
		saveConfig();
	}
	/*
	 * Copy from Config to globals.
	 */
	HLock(Config);
	opt_flags = (*Config)->opt_flags;
	out_fmt = (*Config)->out_fmt;
	strcpy(sfdir_path,(*Config)->sfdir_path);
	HUnlock(Config);
}

/*
 * Update configuration resource.
 */
saveConfig()
{
	/*
	 * Copy globals back to Config.
	 */
	HLock(Config);
	(*Config)->opt_flags = opt_flags;
	(*Config)->out_fmt = out_fmt;
	strcpy((*Config)->sfdir_path,sfdir_path);
	ChangedResource(Config);
	if (ResError() != noErr)
		SysBeep(10L);		/* disk locked */
	else
		WriteResource(Config);
	HUnlock(Config);
}

#define PERIOD_KEY_BIT	47
#define COMMAND_KEY_BIT	55

int tstKey(keys,bit)
KeyMap *keys;
int bit;
{
	return !!(keys->Key[bit >> 5] & (1L << (bit & 0x1F)));
}

int isAbortEvent()
{
KeyMap keys;
	GetKeys(&keys);
	return (tstKey(&keys,PERIOD_KEY_BIT) && tstKey(&keys,COMMAND_KEY_BIT));
}

#if 0
/*
 * Return TRUE if command-. or command-Q keyboard event, else FALSE.
 */
int isAbortEvent()
{
EventRecord ev;
int ch;
	GetNextEvent(keyDownMask,&ev);
	if ((ev.what == keyDown) && (ev.modifiers & controlKey)) {
		ch = (ev.message & charCodeMask);
		if (ch == '.' || ch == 'q' || ch == 'Q') return TRUE;
	}
	return FALSE;
}
#endif
