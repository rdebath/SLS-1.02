/* $XConsortium: LibTest.c,v 1.3 91/05/14 15:02:45 dave Exp $ */

/*
 *	Copyright 1989 1990, Tektronix, Inc.
 *	Copyright 1989 1990 by The Massachusetts Institute of Technology
 *	
 *	                    All Rights Reserved
 *	
 *	Permission to use, copy, modify, and distribute this
 *	software and its documentation for any purpose and without
 *	fee is hereby granted, provided that the above copyright
 *	notice appear in all copies and that both that copyright
 *	notice and this permission notice appear in supporting
 *	documentation, and that the names of MIT and Tektronix not be
 *	used in advertising or publicity pertaining to distribution
 *	of the software without specific prior written permission.
 *	M.I.T. and Tektronix make no representation about the
 *	suitability of this software for any purpose. It is provided
 *	"as is" without any express or implied warranty.
 *	
 *	MIT AND TEKTRONIX DISCLAIM ALL WARRANTIES WITH REGARD TO  THIS
 *	SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 *	AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL MIT OR
 *	TEKTRONIX BE LIABLE FOR  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 *	DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 *	DATA  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 *	OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
 *	THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *	NAME
 *		cmstest - Interactive Library Testing Interface.
 *
 *	SYNOPSIS
 *		cmstest
 *
 *	DESCRIPTION
 *		This program is a front-end for testing the library.
 *		It takes commands from stdin.
 *
 *	FILES
 *		Command Source File:
 *			C source file containing a) definition for the
 *			Command Table (table of command string / function
 *			pointer tuples) and b) function source.
 *		Command Header File:
 *			Header file as a result from running 'autohdr'
 *			on the Command Source File.
 *		Command Object File:
 *			The result of compiling the Command Source File
 *		Library:
 *			The library under test.
 *		Makefile:
 *			All the above files are defined in the Makefile
 *			and generated and appropriately linked to this
 *			program.
 */

/*
 *      EXTERNAL INCLUDES
 *              Include files that must be exported to any package or
 *              program using this package.
 */
#include "LibTest.h"
#include <X11/Xlib.h>
#include <X11/Xlibint.h>      

/*
 *	EXTERNALS
 *		Declarations that are needed by calling modules.
 *		When using 'autohdr', these declaration will be placed
 *		in the resulting header file.
 */
int CommandArgc;	/* GLOBAL */
char **CommandArgv;	/* GLOBAL */
int EchoInput = 0;	/* GLOBAL */

/*
 *      INTERNAL INCLUDES
 *              Include files that need NOT be exported to any package or
 *              program using this package.
 */
#include <stdio.h>
#include "CmdTbl.h"

/*
 *	INTERNALS
 *		Declarations that are local to this module.
 *		(ignored by 'autohdr').
 */
#ifndef LIBTEST_PROMPT
#define LIBTEST_PROMPT	"What! >"
#endif

#ifndef LIBTEST_COMMENT_CHAR
#define LIBTEST_COMMENT_CHAR	'#'
#endif

#define LIBTEST_EOLN_CHAR	'\n'

PFStatus LtStrToFuncPtr();

int errno;

extern Display *pDpy ;


/************************************************************************
 *									*
 *			   	M A I N					*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		main
 *
 *	SYNOPSIS
 */
main(argc, argv)
    int argc;
    char **argv;
/*
 *	DESCRIPTION
 *		<complete external description of the function>
 *
 *	RETURNS
 *		<value returned by the function -- not required for void functions>
 *
 */
{
    char buf[BUFSIZ];
    PFStatus pfunc;
    char command[BUFSIZ];

    /*
     * Set global CommandArgc and CommandArgv
     */
    CommandArgc = argc;
    CommandArgv = argv;

    /*
     * Process args as necessary here.
     */

    for (;argc--;argv++) {
	if (strcmp(*argv, "-echo") == 0) {
	    EchoInput = 1;
	}
    }

    for(;;) {
	printf("%s", LIBTEST_PROMPT);
	if (fgets(buf, BUFSIZ, stdin) == NULL) {
	    return;
	}
	if (EchoInput) {
	    printf("%s", buf);
	    fflush(stdout);
	}
	if ((buf[0] == LIBTEST_COMMENT_CHAR) ||
		(buf[0] == LIBTEST_EOLN_CHAR)) {
	    continue;
	}
	if (sscanf(buf, "%s", command) != 0) {
	  if ((pfunc = LtStrToFuncPtr(LIBTEST_CMDTBL, command)) 
		== (PFStatus) -1) {
		printf("Error: Invalid command\n\n");
		fflush(stdout);
		continue;
	    }
	    (*pfunc)(&buf[strlen(command)]);
	    fflush(stdout);
	}
    }
}


/************************************************************************
 *									*
 *			   PRIVATE ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		_XPrintDefaultError
 *
 *	SYNOPSIS
 */
static int
_XPrintDefaultError (dpy, event, fp)
    Display *dpy;
    XErrorEvent *event;
    FILE *fp;
/*
 *	DESCRIPTION
 *		Prints error message to stderr
 *		This code lifted directly from Xlib Source
 *		XlibInt.c,v 11.124 90/06/15 13:09:20 rws
 *
 *	RETURNS
 *		int
 *
 */
{
    char buffer[BUFSIZ];
    char mesg[BUFSIZ];
    char number[32];
    char *mtype = "XlibMessage";
    register _XExtension *ext = (_XExtension *)NULL;
    XGetErrorText(dpy, event->error_code, buffer, BUFSIZ);
    XGetErrorDatabaseText(dpy, mtype, "XError", "X Error", mesg, BUFSIZ);
    (void) fprintf(fp, "%s:  %s\n  ", mesg, buffer);
    XGetErrorDatabaseText(dpy, mtype, "MajorCode", "Request Major code %d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->request_code);
    if (event->request_code < 128) {
	sprintf(number, "%d", event->request_code);
	XGetErrorDatabaseText(dpy, "XRequest", number, "", buffer, BUFSIZ);
    } else {
	for (ext = dpy->ext_procs;
	     ext && (ext->codes.major_opcode != event->request_code);
	     ext = ext->next) /* SUPPRESS 530 */
	  ;
	if (ext)
	    strcpy(buffer, ext->name);
	else
	    buffer[0] = '\0';
    }
    (void) fprintf(fp, " (%s)\n  ", buffer);
    if (event->request_code >= 128) {
	XGetErrorDatabaseText(dpy, mtype, "MinorCode", "Request Minor code %d",
			      mesg, BUFSIZ);
	(void) fprintf(fp, mesg, event->minor_code);
	if (ext) {
	    sprintf(mesg, "%s.%d", ext->name, event->minor_code);
	    XGetErrorDatabaseText(dpy, "XRequest", mesg, "", buffer, BUFSIZ);
	    (void) fprintf(fp, " (%s)", buffer);
	}
	fputs("\n  ", fp);
    }
    if (event->error_code >= 128) {
	/* kludge, try to find the extension that caused it */
	buffer[0] = '\0';
	for (ext = dpy->ext_procs; ext; ext = ext->next) {
	    if (ext->error_string) 
		(*ext->error_string)(dpy, event->error_code, &ext->codes,
				     buffer, BUFSIZ);
	    if (buffer[0])
		break;
	}    
	if (buffer[0])
	    sprintf(buffer, "%s.%d", ext->name,
		    event->error_code - ext->codes.first_error);
	else
	    strcpy(buffer, "Value");
	XGetErrorDatabaseText(dpy, mtype, buffer, "Value 0x%x", mesg, BUFSIZ);
	if (*mesg) {
	    (void) fprintf(fp, mesg, event->resourceid);
	    fputs("\n  ", fp);
	}
    } else if ((event->error_code == BadWindow) ||
	       (event->error_code == BadPixmap) ||
	       (event->error_code == BadCursor) ||
	       (event->error_code == BadFont) ||
	       (event->error_code == BadDrawable) ||
	       (event->error_code == BadColor) ||
	       (event->error_code == BadGC) ||
	       (event->error_code == BadIDChoice) ||
	       (event->error_code == BadValue) ||
	       (event->error_code == BadAtom)) {
	if (event->error_code == BadValue)
	    XGetErrorDatabaseText(dpy, mtype, "Value", "Value 0x%x",
				  mesg, BUFSIZ);
	else if (event->error_code == BadAtom)
	    XGetErrorDatabaseText(dpy, mtype, "AtomID", "AtomID 0x%x",
				  mesg, BUFSIZ);
	else
	    XGetErrorDatabaseText(dpy, mtype, "ResourceID", "ResourceID 0x%x",
				  mesg, BUFSIZ);
	(void) fprintf(fp, mesg, event->resourceid);
	fputs("\n  ", fp);
    }
    XGetErrorDatabaseText(dpy, mtype, "ErrorSerial", "Error Serial #%d", 
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->serial);
    fputs("\n  ", fp);
    XGetErrorDatabaseText(dpy, mtype, "CurrentSerial", "Current Serial #%d",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, dpy->request);
    fputs("\n", fp);
    if (event->error_code == BadImplementation) return 0;
    return 1;
}



/************************************************************************
 *									*
 *			   PUBLIC ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		LtErrorHandler - Special LibTest error handler
 *
 *	SYNOPSIS
 */
int
LtErrorHandler (pDpy, pError)
    Display	*pDpy ;
    XErrorEvent	*pError ;
/*
 *	DESCRIPTION
 *		Traps XErrorEvents
 *		Prints a message to stderr,
 *		Sets a global variable,
 *		Synchronizes output
 *
 *	RETURNS
 *		int
 *
 */
{
    _XPrintDefaultError (pDpy, pError, stderr);

    return (1) ;
}


/*
 *	NAME
 *		LtStrToFuncPtr - convert a string to a function pointer
 *
 *	SYNOPSIS
 */
PFStatus
LtStrToFuncPtr(pde,pstring)
    FuncTableEntry	*pde;	/* IN: table of string/functionptr  pairs
			     *     last entry must contain pair "", 0 */
    char	*pstring;	/* IN: string to be looked up in that table  */
/*
 *	DESCRIPTION
 *		Converts a string to an integer define.
 *
 *		Looks up the string in the table and returns the integer
 *		associated with the string.
 *
 *		Later may need similar function for unsigned long define.
 *
 *	RETURNS
 *		The int equivalent of the defined string.
 *		-1 if the string is not found in table
 *
 */
{
    if (!pde) return((PFStatus) -1); /* defend against NULL pointer */
    while( strcmp(pde->pstring,"") != 0 ){
	if( strcmp(pde->pstring, pstring) == 0){
	    return(pde->pfunc);
	}
	pde++;
    }
    return((PFStatus) -1);
}


/*
 *	NAME
 *		LtDefineToStr - convert a define to a string
 *
 *	SYNOPSIS
 */
char *
LtDefineToStr(pde,define)
LtDefineEntry	pde[];	/* IN: table of X string-define pairs
			 *     last entry must contain pair "", 0
			 */
int		define;	/* IN: define to be looked up in that table	*/
/*
 *	DESCRIPTION
 *		Converts an integer define to a string pointer.
 *		Later may need similar function for unsigned long define.
 *
 *		Check XuDefineString.h for predefined tables.
 *
 *	RETURNS
 *		Pointer to a valid string.
 *		If no string is found, returns a pointer to a 0 length string.
 *
 */
{
    while( strcmp(pde->pstring,"") != 0 ){
	if( pde->define == define) {
	    return(pde->pstring);
	}
	pde++;
    }
    return("");
}

/*
 *	NAME
 *		LtStrToDefine - convert a string to a define
 *
 *	SYNOPSIS
 */
int
LtStrToDefine(pde,pstring)
    LtDefineEntry	pde[];	/* IN: table of X string-define pairs
				 *     last entry must contain pair "", 0
				 */
    char	*pstring;	/* IN: string to be looked up in that table */
/*
 *	DESCRIPTION
 *		Converts a string to an integer define.
 *
 *		Looks up the string in the table and returns the integer
 *		associated with the string.
 *
 *		Later may need similar function for unsigned long define.
 *
 *		Check XuDefineString.h for predefined tables.
 *
 *
 *	RETURNS
 *		The int equivalent of the defined string.
 *		-1 if the string is not found in table
 *
 */
{
    while( strcmp(pde->pstring,"") != 0 ){
	if( strcmp(pde->pstring,pstring) == 0){
	    return(pde->define);
	}
	pde++;
    }
    return(-1);
}
