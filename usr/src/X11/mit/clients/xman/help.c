/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: help.c,v 1.9 91/07/01 14:05:09 dave Exp $
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
 * Created:   January 19, 1988
 */

#include "globals.h"

static Atom wm_delete_window;

ManpageGlobals * InitPsuedoGlobals();

/*	Function Name: MakeHelpWidget.
 *	Description: This function creates the help widget so that it will be
 *                   ready to be displayed.
 *	Arguments: none.
 *	Returns: none.
 */

Boolean
MakeHelpWidget()
{

  ManpageGlobals * man_globals;	/* The psuedo global structure. */
  
  if (help_widget != NULL)	/* If we already have a help widget. 
				   then do not create one. */
    return(TRUE);

  man_globals = InitPsuedoGlobals();

  CreateManpageWidget(man_globals, HELPNAME, FALSE);
  help_widget = man_globals->This_Manpage;

  if (OpenHelpfile(man_globals) == FALSE) {
    XtDestroyWidget(help_widget);
    help_widget = NULL;
    return(FALSE);
  }

  ChangeLabel(man_globals->label, "Xman Help");

  XtManageChild( man_globals->manpagewidgets.manpage );
  XtRealizeWidget(  help_widget );
  SaveGlobals( man_globals->This_Manpage, man_globals );
  AddCursor( help_widget, resources.cursors.manpage);

/*
 * Set up ICCCM delete window.
 */
  wm_delete_window = XInternAtom(XtDisplay(help_widget), "WM_DELETE_WINDOW",
				 False);
  XtOverrideTranslations
      (man_globals->This_Manpage, 
       XtParseTranslationTable ("<Message>WM_PROTOCOLS: RemoveThisManpage()"));
  (void) XSetWMProtocols (XtDisplay(man_globals->This_Manpage),
			  XtWindow(man_globals->This_Manpage),
			  &wm_delete_window, 1);

  return(TRUE);
}

/*	Function Name: OpenHelpfile
 *	Description: opens the helpfile.
 *	Arguments: man_globals - the psuedo globals structure.
 *	Returns: False if no helpfile was found.
 */

Boolean
OpenHelpfile(man_globals)
ManpageGlobals * man_globals;
{
  FILE * help_file_ptr;

  if( (help_file_ptr = fopen(resources.help_file, "r")) == NULL ) {
    PopupWarning(man_globals,
		 "Could not open help file, NO HELP WILL BE AVALIABLE.");
    return(FALSE);
  }
    
  OpenFile(man_globals, help_file_ptr);
  return(TRUE);
}
