/*
 * $XConsortium: externs.h,v 2.38 91/07/22 21:29:13 converse Exp $
 *
 *
 *		       COPYRIGHT 1987, 1989
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT
 * RIGHTS, APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN
 * ADDITION TO THAT SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Digital Equipment Corporation not be
 * used in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 */

#ifndef X_NOT_STDC_ENV
#include <errno.h>
#include <stdlib.h>
#else
extern int errno;
extern char *getenv();
extern void exit();
extern void free();
#endif

/* Action routines are declared in actions.h */
/* Functions which begin with `Do' are the corresponding callbacks. */

	/* from command.c */

extern char *	DoCommandToFile		(/* Widget, XtPointer, XtPointer */);
extern char *	DoCommandToString	(/* Widget, XtPointer, XtPointer */);

	/* from compfuncs. */

extern void 	DoResetCompose		(/* Widget, XtPointer, XtPointer */);

	/* from folder.c */

extern void	DoClose			(/* Widget, XtPointer, XtPointer */);
extern void	DoComposeMessage	(/* Widget, XtPointer, XtPointer */);
extern void	DoOpenFolder		(/* Widget, XtPointer, XtPointer */);
extern void 	DoOpenFolderInNewWindow	(/* Widget, XtPointer, XtPointer */);
extern void	DoCreateFolder		(/* Widget, XtPointer, XtPointer */);
extern void 	DoDeleteFolder		(/* Widget, XtPointer, XtPointer */);
extern void	Push			(/* Stack, char* */);
extern char *	Pop			(/* Stack */);

	/* from menu.c */

extern void	AttachMenuToButton	(/* Button, Widget, char * */);
extern void 	AddMenuEntry		(/* Widget, char *, ... */);
extern void	DoRememberMenuSelection (/* Widget, XtPointer, XtPointer */);
extern void	SendMenuEntryEnableMsg	(/* Button, char *, int */);
extern void	ToggleMenuItem		(/* Widget, Boolean */);

	/* from msg.c */

extern Widget   CreateFileSource	(/* Widget, String, Boolean */);

	/* from popup.c */

extern void	DestroyPopup	(/* Widget, XtPointer, XtPointer */);
extern void	WMDeletePopup	(/* Widget, XEvent* */);
extern void	PopupPrompt	(/* Widget, String, XtCallbackProc */);
extern void	PopupConfirm	(/* Widget, String, XtCallbackList, ... */);
extern void	PopupNotice	(/* char *, XtCallbackProc, XtPointer */);
extern void 	PopupError	(/* Widget, String */);
extern void	PopupWarningHandler();	/* for Xt to call */

	/* from screen.c */

extern void	EnableProperButtons	(/* Scrn */);
extern Scrn	CreateNewScrn		(/* ScrnKind */);
extern Scrn	NewViewScrn		(/* void */);
extern Scrn	NewCompScrn		(/* void */);
extern void	ScreenSetAssocMsg	(/* Scrn, Msg */);
extern void	DestroyScrn		(/* Scrn */);
extern void	MapScrn			(/* Scrn */);
extern Scrn	ScrnFromWidget		(/* Widget */);

	/* from tocfuncs.c */

extern Boolean	UserWantsAction		(/* Widget, Scrn */);
extern void 	DoIncorporateNewMail	(/* Widget, XtPointer, XtPointer */);
extern void 	DoCommit		(/* Widget, XtPointer, XtPointer */);
extern void	DoPack			(/* Widget, XtPointer, XtPointer */);
extern void	DoSort			(/* Widget, XtPointer, XtPointer */);
extern void 	DoForceRescan		(/* Widget, XtPointer, XtPointer */);
extern void 	DoReverseReadOrder	(/* Widget, XtPointer, XtPointer */);
extern void	DoNextView		(/* Widget, XtPointer, XtPointer */);
extern void	DoPrevView		(/* Widget, XtPointer, XtPointer */);
extern void	DoDelete		(/* Widget, XtPointer, XtPointer */);
extern void	DoMove			(/* Widget, XtPointer, XtPointer */);
extern void	DoCopy			(/* Widget, XtPointer, XtPointer */);
extern void	DoUnmark		(/* Widget, XtPointer, XtPointer */);
extern void	DoViewNew		(/* Widget, XtPointer, XtPointer */);
extern void	DoReply			(/* Widget, XtPointer, XtPointer */);
extern void	DoForward		(/* Widget, XtPointer, XtPointer */);
extern void	DoTocUseAsComp		(/* Widget, XtPointer, XtPointer */);
extern void	DoPrint			(/* Widget, XtPointer, XtPointer */);
extern void	DoPickMessages		(/* Widget, XtPointer, XtPointer */);
extern void	DoSelectSequence	(/* Widget, XtPointer, XtPointer */);
extern void	DoOpenSeq		(/* Widget, XtPointer, XtPointer */);
extern void 	DoAddToSeq		(/* Widget, XtPointer, XtPointer */);
extern void 	DoRemoveFromSeq		(/* Widget, XtPointer, XtPointer */);
extern void	DoDeleteSeq		(/* Widget, XtPointer, XtPointer */);

	/* from util.c */

extern void	Punt			(/* char * */);
extern int	myopen			(/* char *, int, int */);
extern FILE *	myfopen			(/* char *, char * */);
extern int	myclose			(/* int */);
extern int	myfclose		(/* FILE * */);
extern char *	MakeNewTempFileName	(/* void */);
extern char **	MakeArgv		(/* int */);
extern char **	ResizeArgv		(/* char **, int */);
extern FILEPTR	FOpenAndCheck		(/* char *, char * */);
extern char *	ReadLine		(/* FILE * */);
extern char *	ReadLineWithCR		(/* FILE * */);
extern void	DeleteFileAndCheck	(/* char * */);
extern void	CopyFileAndCheck	(/* char *, char * */);
extern void	RenameAndCheck		(/* char *, char * */);
extern char *	CreateGeometry		(/* int, int, int, int, int */);
extern int	FileExists		(/* char * */);
extern Boolean	IsSubfolder		(/* char * */);
extern void 	SetCurrentFolderName	(/* Scrn, char * */);
extern void	ChangeLabel		(/* Widget, char * */);
extern Widget	CreateTextSW	(/* Scrn, char *, ArgList, Cardinal */);
extern Widget	CreateTitleBar		(/* Scrn, char * */);
extern void	Feep			(/* void */);
extern MsgList	CurMsgListOrCurMsg	(/* Toc */);
extern int	GetWidth		(/* Widget */);
extern int	GetHeight		(/* Widget */);
extern Toc	SelectedToc		(/* Scrn */);
extern Toc	CurrentToc		(/* Scrn */);
extern int	strncmpIgnoringCase();
extern void 	StoreWindowName		(/* Scrn, char * */);
extern void	InitBusyCursor		(/* Scrn */);
extern void	ShowBusyCursor		(/* void */);
extern void 	UnshowBusyCursor	(/* void */);
extern void 	SetCursorColor		(/* Widget, Cursor, unsigned long */);

	/* from viewfuncs.c */

extern void	DoCloseView		(/* Widget, XtPointer, XtPointer */);
extern void	DoViewReply		(/* Widget, XtPointer, XtPointer */);
extern void 	DoViewForward		(/* Widget, XtPointer, XtPointer */);
extern void	DoViewUseAsComposition	(/* Widget, XtPointer, XtPointer */);
extern void	DoEditView		(/* Widget, XtPointer, XtPointer */);
extern void	DoSaveView		(/* Widget, XtPointer, XtPointer */);
extern void	DoPrintView		(/* Widget, XtPointer, XtPointer */);
