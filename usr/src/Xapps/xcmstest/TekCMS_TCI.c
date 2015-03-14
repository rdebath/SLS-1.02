/* $XConsortum: TekCMS_TCI.c,v 1.1 91/02/11 19:40:35 dave Exp $ */


/*
 * (c) Copyright 1990, Tektronix Inc.
 * 	All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Tektronix not be used
 * in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 *
 * Tektronix disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness, in no event shall
 * Tektronix be liable for any special, indirect or consequential damages or
 * any damages whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious action,
 * arising out of or in connection with the use or performance of this
 * software.
 *
 *	NAME
 *		TekCmsTCI.c -- TekCMS Test Case Interface
 *
 *	DESCRIPTION
 *		This file contains the definition of the Test Case Table.
 *		For each test case, two things are necessary:
 *		    1. Entry into the Test Case Table
 *			Create an entry into the Test Case Table (TekCMS_TestCaseTbl)
 *			which is a command string and then a pointer to
 *			the test case function.  String entry should
 *			conform to the Test Case Identifier specified in
 *			the test design specifications.
 *		    2. Test Case Function
 *			Create the test case function.
 */


/*
 *      EXTERNAL INCLUDES
 *              Include files that must be exported to any package or
 *              program using this package.
 */
#include "LibTest.h"


/*
 *      INTERNAL INCLUDES
 *              Include files that need NOT be exported to any package or
 *              program using this package.
 */
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xcms.h>
#ifdef AUTOHEADER
#  include "TekCMS_LT.ah"
#  include "TestInit.ah"
#else
#  include "TekCMS_LT.h"
#  include "TestInit.h"
#endif

#ifdef DONE
#  include "TekCmsTC.h"
#endif /* DONE */

/*
 *	INTERNALS
 *		Declarations that are local to this module.
 *		(ignored by 'autohdr').
 */

/*
 *	EXTERNALS
 *		Declarations that are needed by calling modules.
 *		When using 'autohdr', these declaration will be placed
 *		in the resulting header file.
 */
char	TekCMS_idir[BUFSIZ];
char	TekCMS_vdir[BUFSIZ];
char	TekCMS_rdir[BUFSIZ];


/************************************************************************
 *									*
 *			 PRIVATE ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		filecmp
 *
 *	SYNOPSIS
 */
static int
filecmp(fp1, fp2)
    FILE *fp1, *fp2;
/*
 *	DESCRIPTION
 *		CompareResults
 *
 *	RETURNS
 *		1 if contents of vfile identical to rfile
 *		0 otherwise
 *
 */
{
    char *line1, *line2, buf[BUFSIZ * 2] ;
    int different ;
    different = 0 ;

    while (! different) {
	line1 = fgets(buf, BUFSIZ, fp1);
	line2 = fgets(buf, BUFSIZ, fp2);
	if ((line1 == NULL) && (line2 == NULL)) {
	    return(1);
	}
	if ((line1 == NULL) || (line2 == NULL)) {
	    return(0);
	}
	different = strcmp(line1, line2) ;
    }
    return(0) ;
}


/************************************************************************
 *									*
 *			 PUBLIC ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		TCI
 *
 *	SYNOPSIS
 */
int
TCI(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		TCI
 *
 *	RETURNS
 *		void
 *
 */
{
#ifndef DONE
    printf("Not yet implemented \n");
    return(0);
#else
    PFStatus pfunc;
    char tmpstr[BUFSIZ];
    char ifile[BUFSIZ], vfile[BUFSIZ], rfile[BUFSIZ];
    int i=0;

    if(!Check_init()) {
	return(0);
    }

    if ((sscanf(buf, "%s", tmpstr)<=0) || (strcmp("-help", tmpstr) == 0)) {
	goto TCI_Usage;
    }

    ifile[0] = vfile[0] = rfile[0] = '\0';

    while (sscanf(&buf[i], "%s", tmpstr) > 0) {
	if (!strcmp(tmpstr, "-ifile")) {
	    i += strlen(tmpstr) + 1;
	    if (sscanf(&buf[i], "%s", tmpstr) > 0) {
		if (tmpstr[0] == '/') {
		    /* full path */
		    strcpy(ifile, tmpstr);
		} else {
		    /* relative path */
		    strcpy(ifile, TekCMS_idir);
		    if (ifile[0] != '\0') {
			strcat(ifile, "/");
		    }
		    strcat(ifile, tmpstr);
		}
		i += strlen(tmpstr) + 1;
		printf("ifile = %s\n",ifile);
		continue;
	    } else {
		goto TCI_Usage;
	    }
	} 

	if (!strcmp(tmpstr, "-vfile")) {
	    i += strlen(tmpstr) + 1;
	    if (sscanf(&buf[i], "%s", tmpstr) > 0) {
		if (tmpstr[0] == '/') {
		    /* full path */
		    strcpy(vfile, tmpstr);
		} else {
		    /* relative path */
		    strcpy(vfile, TekCMS_vdir);
		    if (vfile[0] != '\0') {
			strcat(vfile, "/");
		    }
		    strcat(vfile, tmpstr);
		}
		i += strlen(tmpstr) + 1;
		printf("vfile = %s\n",vfile);
		continue;
	    } else {
		goto TCI_Usage;
	    }
	} 

	if (!strcmp(tmpstr, "-rfile")) {
	    i += strlen(tmpstr) + 1;
	    if (sscanf(&buf[i], "%s", tmpstr) > 0) {
		if (tmpstr[0] == '/') {
		    /* full path */
		    strcpy(rfile, tmpstr);
		} else {
		    /* relative path */
		    strcpy(rfile, TekCMS_rdir);
		    if (rfile[0] != '\0') {
			strcat(rfile, "/");
		    }
		    strcat(rfile, tmpstr);
		}
		i += strlen(tmpstr) + 1;
		printf("rfile = %s\n",rfile);
		continue;
	    } else {
		goto TCI_Usage;
	    }
	}
	if ((pfunc = LtStrToFuncPtr(LIBTEST_TCTBL, tmpstr)) == (PFStatus) -1) {
	    printf("Error: Invalid Test Case Identifier - %s\n", tmpstr);
	    return(0);
	}
	(*pfunc)(
		ifile, /* Input File */
		vfile, /* Verification File */
		rfile, /* Results File */
		&buf[i + strlen(tmpstr)]); /* Any test case arguments */
	return(1);
    }
TCI_Usage:
    printf("Usage:\n");
    printf("\tTestCase [-ifile input_file] [-vfile valid_file] [-rfile result_file] <testcase_identifier> [testcase args]\n\n");
    return(0);
#endif /* DONE */
}

/*
 *	NAME
 *		CompareResults
 *
 *	SYNOPSIS
 */
int
TC_CompareResults(tc_id, vfile, rfile)
    char *tc_id, *vfile, *rfile;
/*
 *	DESCRIPTION
 *		CompareResults
 *
 *	RETURNS
 *		1 if contents of vfile identical to rfile
 *		0 if different
 *	       -1 otherwise
 *
 */
{
    FILE *fp1 = NULL;
    FILE *fp2 = NULL;
    int retval = -1;

    if ((fp1 = fopen(vfile, "r")) && (fp2 = fopen(rfile, "r")))
	retval = filecmp(fp1, fp2);
    switch (retval) {
      case 0:
	printf("%s:  FAILED\n", tc_id);
	break;
      case 1:
	printf("%s:  passed\n", tc_id);
	break;
      default:
	printf("%s:  unable to verify results\n", tc_id);
	break;
    }
    if (fp1)
	fclose(fp1);
    if (fp2)
	fclose(fp2);
    return(retval);
}

/*
 *	NAME
 *		Cmd_ListTC
 *
 *	SYNOPSIS
 */
int
Cmd_ListTC(argbuf)
    char *argbuf;
/*
 *	DESCRIPTION
 *		Get TekCMS_rdir
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
#ifndef DONE
    fprintf(stderr, "ListTC not yet implemented\n");
#else
    char tmpstr[BUFSIZ];
    FuncTableEntry	*pTCEntry;

    if (sscanf(argbuf, "%s", tmpstr) > 0) {
	    printf("Usage:\n");
	    printf("\tListTC\n\n");
	    return(0);
    } 

    for (pTCEntry = TekCMS_TestCaseTbl; *pTCEntry->pstring != '\0'; pTCEntry++) {
	printf("\t%s\n", pTCEntry->pstring);
    }
    printf("\n");

    return(1);
#endif /* DONE */
}
