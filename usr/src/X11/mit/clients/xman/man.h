/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: man.h,v 1.29 91/07/10 10:41:54 dave Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   October 22, 1987
 */

/* X toolkit header files */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Cardinals.h>

/* Std system and C header files */

#include <stdio.h>

#include <X11/Xfuncs.h>
#include <X11/Xos.h>

/* X include files */

#include <X11/Xatom.h>

/* Widget header files. */

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Viewport.h>

/* program specific header files. */

#include "ScrollByL.h"

#include "version.h"
#include "defs.h"

typedef void (*fcall)();	/* function pointer typedef */

/* 
 * Assigning values here allows the user of Bitwise Or.
 */

typedef struct _XmanFonts {
  XFontStruct * directory;		/* The font for the directory.  */
} XmanFonts;

typedef struct _XmanCursors {
  Cursor top,			/* The top Cursor, default for xman. */
    help,			/* The top cursor for the help menu. */
    manpage,			/* The cursor for the Manpage. */
    search_entry;		/* The cursor for the text widget in the
				   search box.*/
  Pixel fg_color;		/* foreground color of cursors. */
  Pixel bg_color;		/* background color of cursors. */
} XmanCursors;

typedef struct _ManPageWidgets {
  Widget manpage,		/* The manual page window (scrolled) */
    directory,			/* The widget in which all directories will
				   appear. */
    *box;			/* The boxes containing the sections. */
} ManPageWidgets;

typedef struct _MenuStruct {
  caddr_t data;
  int number;
  XrmQuark quark;
} MenuStruct;

/*
 * The manual sections and entries
 */

typedef struct tManual {
  char * blabel;		/* The button label. */
  char ** entries; 		/* The individual man page file names. */
  char ** entries_less_paths;   /* Entry names only */
  int nentries;			/* how many (TOTAL)*/
  int nalloc;			/* how much space allocated */
  int flags;			/* suffix, fold */
} Manual;
   
/* psuedo Globals that are specific to each manpage created. */

typedef struct _ManpageGlobals{
  int current_directory;	/* The directory currently being shown 
				   on this manpage. */
  Boolean dir_shown,		/* True if the directory is then current
				   visable screen */
    both_shown;			/* If true then both the manpage and
				   the directory are to be shown.*/
  Widget label,			/* The label widget at the top of the page. */
    standby,			/* The please standby widget. */
    save,			/* The "would you like to save?" widget. */
    search_widget,		/* The search widget popup. */
    help_button,		/* The help button. */
    option_menu,		/* The option menu. */
    text_widget;		/* text widget containing search string. */

  /* Widgets (Objects really) for the command menu entries. */

  Widget dir_entry, manpage_entry, help_entry,
    search_entry, both_screens_entry, remove_entry, open_entry,
    version_entry, quit_entry;

  char manpage_title[80];       /* The label to use for the current manpage. */

  char save_file[80];		/* the name of the file to save fomatted 
				   page into. */
  char tmpfile[80];		/* the name of the file to copy the formatted
				   page from. */
  Boolean compress;		/* Compress file on save? */
  char ** section_name;		/* The name of each of the sections */

  ManPageWidgets manpagewidgets; /* The manpage widgets. */

  /* Things to remember when cleaning up whne killing manpage. */

  Widget This_Manpage;		/* a pointer to the root of
				   this manpage. */

} ManpageGlobals;


/* Resource manager sets these. */

typedef struct _Xman_Resources {
  XmanFonts fonts;		/* The fonts used for the man pages. */
  XmanCursors cursors;		/* The cursors for xman. */
  Boolean show_help_syntax;	/* True of syntax message should be dumped to
				   stdout. */
  Boolean both_shown_initial;	/* The initial state of the manual pages
				   show two screens or only one. */
  Boolean top_box_active;	/* Put up the Top Box. */
  Boolean clear_search_string;	/* clear the search string each time it
				   is popped down? */
  int directory_height;	        /* The default height of directory in 
				   both_shown mode. */
  char * help_file;		/* The name of the help file. */
  char * title;	    	        /* The title for topBox */
  Boolean iconic;		/* Should topBox come up in an iconic state */
} Xman_Resources;

/************************************************************
 *
 * Function Defintions 
 * 
 ************************************************************/

/*
 * This is easier than trying to find all calls to StrAlloc().
 */

#define StrAlloc(ptr) XtNewString(ptr)

/* Standard library function definitions. */

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *getenv(), *malloc(), *realloc();
void exit();
#endif

/* Toolkit standard definitions. */

void XtResizeWidget(), XtMoveWidget();

/* buttons.c */

void MakeTopBox(), FormUpWidgets();
void CreateManpageWidget(), MakeSaveWidgets(), WriteLabel();
void MakeTopPopUpWidget(),MakeDirPopUpWidget(), MakeDirectoryBox();
char * CreateManpageName();
Widget CreateManpage();

/* handler.c */

void DirectoryHandler(), PopUpMenu(), SaveCallback(), OptionCallback();
void Popup(),ManpageButtonPress(), GotoManpage(), DirPopupCallback();

/* Action Routines. */

void GotoPage(), PopupHelp(), PopupSearch(), Quit(), SaveFormattedPage();
void CreateNewManpage(), RemoveThisManpage(), Search(), ShowVersion();

/* help.c */

Boolean MakeHelpWidget(), OpenHelpfile();

/* main.c */

void main();

/* man.c */

int Man();

/* misc.c */

void PrintError(),PrintWarning(), PopupWarning(), ChangeLabel(), OpenFile();
void RemovePixmaps(),PositionCenter(),AddCursor(),ParseEntry();
FILE *FindManualFile(),*Format(), *OpenEntryFile();
ManpageGlobals * GetGlobals();
void SaveGlobals(), RemoveGlobals();

/* pages.c */

Boolean InitManpage();
void PrintManpage();
Boolean Boldify();

/* search */

void MakeSearchWidget();
FILE * DoSearch();

/* tkfunctions.c */

int Width(), Height(), BorderWidth();
Widget PopupChild(), Child();
char * Name();
Boolean MakeLong();
