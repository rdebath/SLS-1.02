/* $XConsortium: TekCMS_LT.c,v 1.7 91/05/31 19:14:10 rws Exp $ */

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
 *		TekCMS_LT.c -- TekCMS API LibTest Command Source
 *
 *	DESCRIPTION
 *		TekCMS API library tester.
 *
 *		For each command, two things are necessary:
 *		    1. Entry into the Command Table
 *			Create an entry into the Command Table (LibTstCmdTbl)
 *			which is a command string and then a pointer to
 *			the command function.
 *		    2. Command Function
 *			Create the command function which should process
 *			the remainder of the command line.  Don't forget
 *			to predefine the function in the INTERNALS section.
 *			Each command function receives one (1) argument
 *			which is a pointer to a string of characters that
 *			contains the remainder of the command.
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
#include <sys/stat.h>
#include <X11/Xcms.h>

#ifdef AUTOHEADER
#  include "TestInit.ah"
#  include "TekCMS_TCI.ah"
#  include "TekCMSglob.ah"
#  include "TekCMSutil.ah"
#else
#  include "TestInit.h"
#  include "TekCMS_TCI.h"
#  include "TekCMSglob.h"
#  include "TekCMSutil.h"
#endif

/*
 *	INTERNALS
 *		Declarations that are local to this module.
 *		(ignored by 'autohdr').
 */
#define MAXCOLORS	256
#define MAXCMAPS	8

#define NO_COMP_FLAG 0
#define COMP_FLAG    1

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern int atoi();
extern double atof();
char *calloc();
void exit();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *calloc();
#endif /* macII */
extern XcmsColorSpace XcmsCIELabColorSpace;
extern XcmsColorSpace XcmsCIELuvColorSpace;
extern XcmsColorSpace XcmsTekHVCColorSpace;
extern int EchoInput;
extern void TestInit();
Status Cmd_AddDIColorSpace();
Status Cmd_AdjustValue();
Status Cmd_AllocColor();
Status Cmd_AllocNamedColor();
Status Cmd_ConvertColor();
Status Cmd_CreateColormap();
Status Cmd_FreeColormap();
Status Cmd_GetInputDir();
Status Cmd_GetResultDir();
Status Cmd_GetVerificationDir();
Status Cmd_FormatOfPrefix();
Status Cmd_init();
Status Cmd_List();
Status Cmd_LookupColor();
Status Cmd_MaxChroma();
Status Cmd_MaxValue();
Status Cmd_MaxValueChroma();
Status Cmd_MaxValueSamples();
Status Cmd_MinValue();
Status Cmd_PrefixOfId();
Status Cmd_QueryColor();
Status Cmd_QueryColors();
Status Cmd_quit();
Status Cmd_ReduceChroma();
Status Cmd_SetInputDir();
Status Cmd_SetResultDir();
Status Cmd_SetVerificationDir();
Status Cmd_ShortestValueChroma();
Status Cmd_StoreColor();
Status Cmd_StoreColors();
Status Cmd_XAllocNamedColor();
Status Cmd_XLookupColor();
Status Cmd_XParseColor();
Status Cmd_XStoreNamedColor();
Status Cmd_XSynchronize();

typedef struct {
    Colormap cmap;
    int active;
    char name[BUFSIZ];
} CmapTblEntry;

CmapTblEntry CmapTbl[MAXCMAPS];
int	Initialized = 0;
XcmsCCC  _test_ccc = NULL;


/*
 *	EXTERNALS
 *		Declarations that are needed by calling modules.
 *		When using 'autohdr', these declaration will be placed
 *		in the resulting header file.
 */
#define LIBTEST_CMDTBL	LibTstCmdTbl
#define LIBTEST_COMMENT_CHAR    '#'
#define LIBTEST_PROMPT	"XCMS > "


FuncTableEntry LibTstCmdTbl[] = {
/*
 * Admin Commands
 */

    /*
     * Initialize Test
     */
    "init",		Cmd_init,

    /*
     * Quit
     */
    "quit",		Cmd_quit,
    "bye",		Cmd_quit,
    "halt",		Cmd_quit,
    "q",		Cmd_quit,

    /*
     * List Commands
     */
    "list",		Cmd_List,
    "?",		Cmd_List,

    /*
     * Create Test Colormaps
     */
    "CreateColormap",	Cmd_CreateColormap,
    "FreeColormap",	Cmd_FreeColormap,

    /*
     * Set Input Directory
     */
    "Set_idir",	Cmd_SetInputDir,

    /*
     * Set Verification Directory
     */
    "Set_vdir",	Cmd_SetVerificationDir,

    /*
     * Set Result Directory
     */
    "Set_rdir",	Cmd_SetResultDir,

    /*
     * Get Input Directory
     */
    "Get_idir",	Cmd_GetInputDir,

    /*
     * Get Verification Directory
     */
    "Get_vdir",	Cmd_GetVerificationDir,

    /*
     * Get Results Directory
     */
    "Get_rdir",	Cmd_GetResultDir,

    /*
     * Set XSynchronize
     */
    "XSynchronize",	Cmd_XSynchronize,

    /*
     * Execute Test Case
     */
    "TestCase",		TCI,
    "tc",		TCI,

    /*
     * List Test Cases
     */
    "ListTC",		Cmd_ListTC,

/*
 * API Testing Commands
 */

    /*
     * XcmsAllocColor
     */
    "XcmsAllocColor",	Cmd_AllocColor,
    "AllocColor",	Cmd_AllocColor,

    /*
     * XcmsAllocNamedColor
     */
    "XcmsAllocNamedColor", Cmd_AllocNamedColor,
    "AllocNamedColor",	Cmd_AllocNamedColor,

    /*
     * XcmsConvertColors
     */
    "XcmsConvertColor", Cmd_ConvertColor,
    "ConvertColor",	Cmd_ConvertColor,

    /*
     * XcmsLookupColor
     */
    "XcmsLookupColor",	Cmd_LookupColor,
    "LookupColor",	Cmd_LookupColor,

    /*
     * XcmsQueryColor
     */
    "XcmsQueryColor",	Cmd_QueryColor,
    "QueryColor",	Cmd_QueryColor,

    /*
     * XcmsQueryColors
     */
    "XcmsQueryColors",	Cmd_QueryColors,
    "QueryColors",	Cmd_QueryColors,


    /*
     * XcmsStoreColor
     */
    "XcmsStoreColor",	Cmd_StoreColor,
    "StoreColor",	Cmd_StoreColor,

    /*
     * XcmsStoreColors
     */
    "XcmsStoreColors",	Cmd_StoreColors,
    "StoreColors",	Cmd_StoreColors,


    /*
     * Color Space Extension
     */

	"XcmsAddColorSpace",	Cmd_AddDIColorSpace,
	"AddDIColorSpace",	Cmd_AddDIColorSpace,
	"XcmsFormatOfPrefix",	Cmd_FormatOfPrefix,
	"FormatOfPrefix",		Cmd_FormatOfPrefix,
	"XcmsPrefixOfFormat",	Cmd_PrefixOfId,
	"PrefixOfId",		Cmd_PrefixOfId,
    /*
     * Query Gamut Boundaries
     */

	/* TekHVC MaxChroma */
	"XcmsTekHVCQueryMaxC",		Cmd_MaxChroma,
	"MaxChroma",			Cmd_MaxChroma,

	/* TekHVC MaxValue */
	"XcmsTekHVCQueryMaxV",	Cmd_MaxValue,
	"MaxValue",			Cmd_MaxValue,

	/* TekHVC MaxValueSamples */
	"XcmsTekHVCQueryMaxVSamples", 	Cmd_MaxValueSamples,
	"MaxValueSamples",		Cmd_MaxValueSamples,

	/* TekHVC MaxValueChroma */
	"XcmsTekHVCQueryMaxVC", Cmd_MaxValueChroma,
	"MaxValueChroma",		Cmd_MaxValueChroma,

	/* TekHVC MinValue */
	"XcmsTekHVCQueryMinV",		Cmd_MinValue,
	"MinValue",			Cmd_MinValue,

	/* TekHVC AdjustValue */
	"XcmsTekHVCClipV",	Cmd_AdjustValue,
	"AdjustValue",			Cmd_AdjustValue,

	/* TekHVC ReduceChroma */
	"XcmsTekHVCClipC",	Cmd_ReduceChroma,
	"ReduceChroma",			Cmd_ReduceChroma,

	/* TekHVC ShortestValueChroma */
	"XcmsTekHVCClipVC",	Cmd_ShortestValueChroma,
	"ShortestValueChroma",		Cmd_ShortestValueChroma,
	"ShortestVC",			Cmd_ShortestValueChroma,
	"Shortest",			Cmd_ShortestValueChroma,
    /*
     * XLib Modifications
     */
	"XAllocNamedColor",	Cmd_XAllocNamedColor,
	"XLookupColor",	Cmd_XLookupColor,
	"XParseColor",	Cmd_XParseColor,
	"XStoreNamedColor",	Cmd_XStoreNamedColor,

    "",			0
};


/************************************************************************
 *									*
 *			PRIVATE ROUTINES				*
 *									*
 *									*
 ************************************************************************/



/*
 *	NAME
 *		StrToCmap - convert a string to a colormap
 *
 *	SYNOPSIS
 */
static int
StrToCmap(pCmapTbl, pstring, pcmap)
    CmapTblEntry pCmapTbl[];	
    char *pstring;
    Colormap *pcmap;
/*
 *	DESCRIPTION
 *		Returns the colormap associated to a string.
 *
 *	RETURNS
 *		0 if failed; non-zero otherwise.
 *		Also the associated colormap via pcmap.
 *
 */
{
    int i;

    if (strcmp("default", pstring) == 0) {
	*pcmap = XDefaultColormap(pDpy,XDefaultScreen(pDpy));
	return(1);
    }
    for (i = 0; i < MAXCMAPS; pCmapTbl++, i++) {
	if((strcmp(pCmapTbl->name, pstring) == 0) && (pCmapTbl->active)) {
	    *pcmap = pCmapTbl->cmap;
	    return(1);
	}
    }
    return(0);
}

/*
 *	NAME
 *		NextFreeCmapEntry - convert a string to a define
 *
 *	SYNOPSIS
 */
static int
NextFreeCmapEntry(pCmapTbl)
    CmapTblEntry	pCmapTbl[];	
/*
 *	DESCRIPTION
 *		Obtains the index to the next free CmapTblEntry.
 *
 *	RETURNS
 *		Returns the index to the next free CmapTblEntry.
 *
 */
{
    int i;

    for (i = 0; i < MAXCMAPS; pCmapTbl++, i++) {
	if(!pCmapTbl->active) {
	    return(i);
	}
    }
    return(-1);
}

/*
 *      NAME
 *              CheckPrintRetVal - check the retval and print readable result
 *
 *      SYNOPSIS
 */
static int
CheckPrintRetVal (pWord, retval, compFlag) 
    char *pWord;
    Status retval;
    int compFlag;
/*
 *      DESCRIPTION
 *		Checks the Status return value and prints the readable
 *		result.
 *
 *      RETURNS
 *		0 if failed; non-zero otherwise.
 *
 */
{
    if (compFlag) {
	printf ("\t%s() returned %s", pWord, 
	        (retval == XcmsFailure) ? "XcmsFailure" :
		 ((retval == XcmsSuccess) ? "XcmsSuccess" :
		 ((retval == XcmsSuccessWithCompression) ? 
		  "XcmsSuccessWithCompression" : "invalid value ")));
	if (retval == XcmsSuccess ||
	    retval == XcmsSuccessWithCompression) {
		printf ("\n");
		return(1);
	} else if (retval != XcmsFailure) {
	    printf ("%d\n");
	} else {
	    printf ("\n");
	}
	return(0);
    } else {
	printf ("\t%s() returned %s", pWord,
		(retval == XcmsFailure) ? "XcmsFailure" :
		((retval == XcmsSuccess) ? "XcmsSuccess" : "invalid value"));
	if (retval == XcmsSuccess) {
	    printf ("\n");
	    return(1);
	} else if (retval != XcmsFailure) {
	    printf ("%d\n");
	} else {
	    printf ("\n");
	}
	return(0);
    }
}


/************************************************************************
 *									*
 *			 PUBLIC ROUTINES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		Check_init - Check if Initialized
 *
 *	SYNOPSIS
 */
int
Check_init()
/*
 *	DESCRIPTION
 *		Check if this package is initialized
 *
 *	RETURNS
 *		Status: 0=fail, 1=Success
 *
 */
{
    if (Initialized) {
	return(1);
    }
#ifdef NO_AUTO_INIT
    printf("Error: Test not initialized\n");
    return(0);
#else
    TekCMS_idir[0] = TekCMS_vdir[0] = TekCMS_rdir[0] = '\0';
    TestInit();
    _test_ccc = XcmsDefaultCCC(pDpy, DefaultScreen(pDpy));
    Initialized = 1;
    if ((Cmd_CreateColormap("TstCmap_AllocColor AllocNone") == 0) ||
	    (Cmd_CreateColormap("TstCmap_StoreColor AllocAll") == 0)) {
	printf("Error: Unable to create TstCmap_AllocColor or TstCmap_StoreColor colormaps\n");
	return(0);
    }
    return(1);
#endif /* NO_AUTO_INIT */
}


/************************************************************************
 *									*
 *			 ADMIN COMMANDS					*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		Cmd_List
 *
 *	SYNOPSIS
 */
int
Cmd_List(argbuf)
    char *argbuf;
/*
 *	DESCRIPTION
 *		List TekCMS test interface commands
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];
    FuncTableEntry	*pCmdEntry;

    tmpstr[0] = '\0';

    if (sscanf(argbuf, "%s", tmpstr) > 0) {
	    printf("Usage:\n");
	    printf("\tlist\n\n");
	    return(0);
    } 

    for (pCmdEntry = LibTstCmdTbl; *pCmdEntry->pstring != '\0'; pCmdEntry++) {
	printf("\t%s\n", pCmdEntry->pstring);
    }
    printf("\n");

    return(1);
}

/*
 *	NAME
 *		Cmd_SetInputDir
 *
 *	SYNOPSIS
 */
int
Cmd_SetInputDir(argbuf)
    char *argbuf;
/*
 *	DESCRIPTION
 *		Set TekCMS_idir
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];
    struct stat statbuf;

    if ((sscanf(argbuf, "%s", tmpstr) < 1) || (strcmp("-help", tmpstr) == 0)) {
SetInputDir_Usage:
	    printf("Usage:\n");
	    printf("\tSet_idir dirname \n\n");
	    return(0);
    } 

    /* Verify that it is a directory */
    if (stat(tmpstr, &statbuf) == 0) {
	/* stat succeeded */
	if (statbuf.st_mode & S_IFDIR) {
	    strcpy(TekCMS_idir, tmpstr);
	    return(1);
	}
	printf("Error: %s not a directory\n", tmpstr);
	return(0);
    } else {
	/* stat failed */
	printf("Error: cannot find directory %s\n", tmpstr);
	return(0);
    }
}

/*
 *	NAME
 *		Cmd_SetVerificationDir
 *
 *	SYNOPSIS
 */
int
Cmd_SetVerificationDir(argbuf)
    char *argbuf;
/*
 *	DESCRIPTION
 *		Set TekCMS_vdir
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];
    struct stat statbuf;

    if ((sscanf(argbuf, "%s", tmpstr) < 1) || (strcmp("-help", tmpstr) == 0)) {
SetVerificationDir_Usage:
	    printf("Usage:\n");
	    printf("\tSet_vdir dirname \n\n");
	    return(0);
    } 

    /* Verify that it is a directory */
    if (stat(tmpstr, &statbuf) == 0) {
	/* stat succeeded */
	if (statbuf.st_mode & S_IFDIR) {
	    strcpy(TekCMS_vdir, tmpstr);
	    return(1);
	}
	printf("Error: %s not a directory\n", tmpstr);
	return(0);
    } else {
	/* stat failed */
	printf("Error: cannot find directory %s\n", tmpstr);
	return(0);
    }
}

/*
 *	NAME
 *		Cmd_SetResultDir
 *
 *	SYNOPSIS
 */
int
Cmd_SetResultDir(argbuf)
    char *argbuf;
/*
 *	DESCRIPTION
 *		Set TekCMS_rdir
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];
    struct stat statbuf;

    if ((sscanf(argbuf, "%s", tmpstr) < 1) || (strcmp("-help", tmpstr) == 0)) {
SetResultDir_Usage:
	    printf("Usage:\n");
	    printf("\tSet_rdir dirname \n\n");
	    return(0);
    } 

    /* Verify that it is a directory */
    if (stat(tmpstr, &statbuf) == 0) {
	/* stat succeeded */
	if (statbuf.st_mode & S_IFDIR) {
	    strcpy(TekCMS_rdir, tmpstr);
	    return(1);
	}
	printf("Error: %s not a directory\n", tmpstr);
	return(0);
    } else {
	/* stat failed */
	printf("Error: cannot find directory %s\n", tmpstr);
	return(0);
    }
}

/*
 *	NAME
 *		Cmd_GetInputDir
 *
 *	SYNOPSIS
 */
int
Cmd_GetInputDir(argbuf)
    char *argbuf;
/*
 *	DESCRIPTION
 *		Get TekCMS_idir
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];

    if (sscanf(argbuf, "%s", tmpstr) > 0) {
GetInputDir_Usage:
	    printf("Usage:\n");
	    printf("\tGet_idir\n\n");
	    return(0);
    } 

    printf("\tInput Directory = %s\n", TekCMS_idir);
    return(1);
}

/*
 *	NAME
 *		Cmd_GetVerificationDir
 *
 *	SYNOPSIS
 */
int
Cmd_GetVerificationDir(argbuf)
    char *argbuf;
/*
 *	DESCRIPTION
 *		Get TekCMS_vdir
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];

    if (sscanf(argbuf, "%s", tmpstr) > 0) {
GetVerificationDir_Usage:
	    printf("Usage:\n");
	    printf("\tGet_vdir\n\n");
	    return(0);
    } 

    printf("\tVerification Directory = %s\n", TekCMS_vdir);
    return(1);
}

/*
 *	NAME
 *		Cmd_GetResultDir
 *
 *	SYNOPSIS
 */
int
Cmd_GetResultDir(argbuf)
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
    char tmpstr[BUFSIZ];

    if (sscanf(argbuf, "%s", tmpstr) > 0) {
GetResultDir_Usage:
	    printf("Usage:\n");
	    printf("\tGet_rdir\n\n");
	    return(0);
    } 

    printf("\tResult Directory = %s\n", TekCMS_rdir);
    return(1);
}


/*
 *	NAME
 *		Cmd_CreateColormap - LibTest "CreateColormap" command
 *
 *	SYNOPSIS
 */
int
Cmd_CreateColormap(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"CreateColormap" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cMap;
    int iCmap;
    char allocStr[BUFSIZ];

    allocStr[0] = '\0';

    if (!Check_init()) {
	return(0);
    }
    if ((iCmap = NextFreeCmapEntry(CmapTbl)) == -1) {
	return(0);
    }

    if (sscanf(buf, "%s %s",
	    CmapTbl[iCmap].name,
	    allocStr
	    ) < 2) {
	printf("Invalid argument(s): %s\n", buf);
CreateColormap_Usage:
	printf("Usage:\n");
	printf("    CreateColormap <Name> <AllocFlag>\n\n");
	printf("\tWhere:\n");
	printf("\t    Name :== string\n");
	printf("\t    AllocFlag :== AllocNone | AllocAll\n");
	CmapTbl[iCmap].active = 0;
	return(0);
    }

    cMap = XCreateColormap(pDpy,
	    RootWindow(pDpy, DefaultScreen(pDpy)),
	    DefaultVisual(pDpy, DefaultScreen(pDpy)),
	    LtStrToDefine(AllocTbl, allocStr));
    XSync(pDpy, False);
    CmapTbl[iCmap].cmap = cMap;
    CmapTbl[iCmap].active = 1;
    return(1);
}


/*
 *	NAME
 *		Cmd_FreeColormap - LibTest "FreeColormap" command
 *
 *	SYNOPSIS
 */
int
Cmd_FreeColormap(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"FreeColormap" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char name[BUFSIZ];
    int i;

    if (!Check_init()) {
	return(0);
    }

    name[0] = '\0';

    if (sscanf(buf, "%s",
	    name
	    ) < 1) {
	printf("Invalid argument(s): %s\n", buf);
FreeColormap_Usage:
	printf("Usage:\n");
	printf("    FreeColormap <Name>\n\n");
	printf("\tWhere:\n");
	printf("\t    Name :== string\n");
	return(0);
    }
    for (i = 0; i < MAXCMAPS; i++) {
	if((strcmp(CmapTbl[i].name, name) == 0) && (CmapTbl[i].active)) {
	    XFreeColormap(pDpy, CmapTbl[i].cmap);
	    XSync(pDpy, False);
	    CmapTbl[i].active = 0;
	    return(1);
	}
    }
    printf("Warning: colormap %s already freed or is non-existent\n", name);
    return(0);
}


/*
*	NAME
*		Cmd_init - LibTest "init" command
*
*	SYNOPSIS
*/
/* ARGSUSED */
int
Cmd_init(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"init" command for this test package.
 *
 *	RETURNS
 *		Status: 0=fail, 1=Success
 *
 */
{
    int i;

    if (Initialized) {
	printf("Error: Test already initialized\n");
	return(0);
    }
    TekCMS_idir[0] = TekCMS_vdir[0] = TekCMS_rdir[0] = '\0';
    TestInit();
    Initialized = 1;
    for (i = 0; i < MAXCMAPS; i++) {
	CmapTbl[i].active = 0;
    }
    if ((Cmd_CreateColormap("TstCmap_AllocColor AllocNone") == 0) ||
	    (Cmd_CreateColormap("TstCmap_StoreColor AllocAll") == 0)) {
	printf("Error: Unable to create TstCmap_AllocColor or TstCmap_StoreColor colormaps\n");
	return(0);
    }
    return(1);
}


/*
 *	NAME
 *		Cmd_quit - "quit" command
 *
 *	SYNOPSIS
 */
/* ARGSUSED */
int
Cmd_quit(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"quit" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    int i;

    if (Initialized) {
	for (i = 0; i < MAXCMAPS; i++) {
	    if (CmapTbl[i].active) {
		XFreeColormap(pDpy, CmapTbl[i].cmap);
		XSync(pDpy, False);
	    }
	}
    }
    exit(1);
}


/*
 *	NAME
 *		Cmd_XSynchronize - Set X synchronization
 *
 *	SYNOPSIS
 */
int
Cmd_XSynchronize(buf)
    char *buf;
/*
 *	DESCRIPTION
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Bool setting = 0;
    char settingStr[BUFSIZ];
    char tmpstr[BUFSIZ];

    if (!Check_init()) {
	return(0);
    }

    settingStr[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto XSynchronize_Usage;
    }

    if (!sscanf(buf, "%s", settingStr)) {
	printf("Invalid argument(s): %s\n", buf);
XSynchronize_Usage:
	printf("Usage:\n");
	printf("    XSynchronize <ON | OFF>\n\n");
	return(0);
    }

    if (strcmp(settingStr, "ON") == 0) {
	setting = 1;
    } else if (strcmp(settingStr, "OFF") == 0) {
	setting = 0;
    } else {
	printf("Invalid format argument: %s\n", tmpstr);
	goto XSynchronize_Usage;
    }

    printf("\tREQUESTED:\n");
	printf("XSynchronize = %s\n", settingStr);
    XSynchronize(pDpy, setting);
    return(1);
}

/************************************************************************
 *									*
 *			 API TESTING COMMANDS				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		Cmd_AllocColor - LibTest "AllocColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_AllocColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"AllocColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    XcmsColor color;
    char formatStr[BUFSIZ];
    char resultFormatStr[BUFSIZ];
    XcmsColorFormat result_format;
    char cmapname[BUFSIZ];
    char stim1[BUFSIZ];
    char stim2[BUFSIZ];
    char stim3[BUFSIZ];
    char tmpstr[BUFSIZ];
    int nargs;
    Status retval;

    if (!Check_init()) {
	return(0);
    }

    formatStr[0] = '\0';
    cmapname[0] = '\0';
    stim1[0] = '\0';
    stim2[0] = '\0';
    stim3[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto AllocColor_Usage;
    }

    color.pixel = 0;
    if ((nargs = sscanf(buf, "%s %s %s %s %s %s",
	    formatStr,
	    stim1,
	    stim2,
	    stim3,
	    resultFormatStr,
	    cmapname
	    )) < 5) {
	printf("Invalid argument(s): %s\n", buf);
AllocColor_Usage:
	printf("Usage:\n");
	printf("    AllocColor <Format> <Stim1> <Stim2> <Stim3> <Result_Format> [Colormap]\n\n");
	printf("    Where:\n");
	printf("\tStim1,Stim2,Stim3 :== int for RGB, otherwise float\n");
	printf("\tFormat, Result_Format :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	printf("\tStim1,Stim2,Stim3 :== int for RGB, otherwise float\n");
	printf("\tColormap :== string\n");
	return(0);
    }
    color.format = LtStrToDefine(FormatTbl, formatStr);
    result_format = LtStrToDefine(FormatTbl, resultFormatStr);
    if (color.format == XcmsRGBFormat) {
	color.spec.RGB.red = atoi(stim1);
	color.spec.RGB.green = atoi(stim2);
	color.spec.RGB.blue = atoi(stim3);
    } else {
	color.spec.CIEXYZ.X = atof(stim1);
	color.spec.CIEXYZ.Y = atof(stim2);
	color.spec.CIEXYZ.Z = atof(stim3);
    }
    if (nargs < 6) {
	strcpy(cmapname, "TstCmap_AllocColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tColormap:\t%s\n", cmapname);
    PrintXcmsColorSpec(&color);
    printf("\tresult_format:\t%s\n", resultFormatStr);
    printf("    RESULT:\n");
    retval = XcmsAllocColor(pDpy, cmap, &color, result_format);
    if (!CheckPrintRetVal ("XcmsAllocColor", retval, COMP_FLAG))
	return(0);
    PrintXcmsColor(&color);
    return(1);
}


/*
 *	NAME
 *		Cmd_StoreColor - LibTest "StoreColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_StoreColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"StoreColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    XcmsColor color;
    char formatStr[BUFSIZ];
    char cmapname[BUFSIZ];
    char stim1[BUFSIZ];
    char stim2[BUFSIZ];
    char stim3[BUFSIZ];
    char tmpstr[BUFSIZ];
    int nargs;
    Status retval;

    if (!Check_init()) {
	return(0);
    }

    formatStr[0] = '\0';
    cmapname[0] = '\0';
    stim1[0] = '\0';
    stim2[0] = '\0';
    stim3[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto StoreColor_Usage;
    }

    color.pixel = 0;
    if ((nargs = sscanf(buf, "%d %s %s %s %s %s",
	    &color.pixel,
	    formatStr,
	    stim1,
	    stim2,
	    stim3,
	    cmapname
	    )) < 5) {
	printf("Invalid argument(s): %s\n", buf);
StoreColor_Usage:
	printf("Usage:\n");
	printf("    StoreColor <Pixel> <Format> <Stim1> <Stim2> <Stim3> [Colormap]\n\n");
	printf("    Where:\n");
	printf("\tPixel :== integer\n");
	printf("\tFormat :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	printf("\tStim1,Stim2,Stim3 :== int for RGB, otherwise float\n");
	printf("\tColormap :== string\n");
	return(0);
    }
    color.format = LtStrToDefine(FormatTbl, formatStr);
    if (color.format == XcmsRGBFormat) {
	color.spec.RGB.red = atoi(stim1);
	color.spec.RGB.green = atoi(stim2);
	color.spec.RGB.blue = atoi(stim3);
    } else {
	color.spec.CIEXYZ.X = atof(stim1);
	color.spec.CIEXYZ.Y = atof(stim2);
	color.spec.CIEXYZ.Z = atof(stim3);
    }
    if (nargs < 6) {
	strcpy(cmapname, "TstCmap_StoreColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tColormap:\t%s\n", cmapname);
    PrintXcmsColor(&color);
    printf("    RESULT:\n");
    retval = XcmsStoreColor(pDpy, cmap, &color);
    if (!CheckPrintRetVal ("XcmsStoreColor", retval, COMP_FLAG))
	return(0);
    return(1);
}



/*
 *	NAME
 *		Cmd_QueryColor - LibTest "QueryColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_QueryColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"QueryColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    XcmsColor color;
    char formatStr[BUFSIZ];
    XcmsColorFormat result_format;
    char cmapname[BUFSIZ];
    char stim1[BUFSIZ];
    char stim2[BUFSIZ];
    char stim3[BUFSIZ];
    char tmpstr[BUFSIZ];
    int nargs;
    Status retval;

    if (!Check_init()) {
	return(0);
    }

    formatStr[0] = '\0';
    cmapname[0] = '\0';
    stim1[0] = '\0';
    stim2[0] = '\0';
    stim3[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto QueryColor_Usage;
    }

    color.pixel = 0;
    if ((nargs = sscanf(buf, "%d %s %s",
	    &color.pixel,
	    formatStr,
	    cmapname
	    )) < 2) {
	printf("Invalid argument(s): %s\n", buf);
QueryColor_Usage:
	printf("Usage:\n");
	printf("    QueryColor <Pixel> <result_format> [Colormap]\n\n");
	printf("    Where:\n");
	printf("\tPixel :== integer\n");
	printf("\tresult_format :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	printf("\tColormap :== string\n");
	return(0);
    }
    result_format = LtStrToDefine(FormatTbl, formatStr);
    if (nargs < 3) {
	strcpy(cmapname, "TstCmap_StoreColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tQuery pixel:\t%lu\n", color.pixel);
    printf("\tresult_format:\t%s\n", formatStr);
    printf("\tColormap:\t%s\n", cmapname);
    printf("    RESULT:\n");
    retval = XcmsQueryColor(pDpy, cmap, &color, result_format);
    if (!CheckPrintRetVal ("XcmsQueryColor", retval, NO_COMP_FLAG))
	return(0);
    PrintXcmsColor(&color);
    return(1);
}


/*
 *	NAME
 *		Cmd_QueryColors - LibTest "QueryColors" command
 *
 *	SYNOPSIS
 */
int
Cmd_QueryColors(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"QueryColors" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    XcmsColor color[MAXCOLORS];
    char resultFormatStr[BUFSIZ];
    XcmsColorFormat result_format;
    char cmapname[BUFSIZ];
    char tmpstr[BUFSIZ];
    int begin, ncolors;
    Status retval;

    if (!Check_init()) {
	return(0);
    }

    cmapname[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto QueryColors_Usage;
    }

    if (sscanf(buf, "%s %s", resultFormatStr, cmapname) < 2) {
	strcpy(cmapname, "TstCmap_AllocColor");
    }
    result_format = LtStrToDefine(FormatTbl, resultFormatStr);


    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tresult_format:\t%s\n", resultFormatStr);
    printf("\tColormap:\t%s\n", cmapname);
    printf("\tPixel\tFormat\n");
    for(ncolors = 0;;) {
	if (ncolors >= MAXCOLORS) {
	    return(0);
	}
	if (fgets(buf, BUFSIZ, stdin) == NULL) {
	    return(0);
	}
	if (EchoInput) {
	    printf("%s", buf);
	}
	if (buf[0] == LIBTEST_COMMENT_CHAR) {
	    continue;
	}
	if (strcmp(buf, "BEGIN\n") == 0) {
	    begin = 1;
	    continue;
	}
	if (begin && (strcmp(buf, "END\n") == 0)) {
	    break;
	}
	if (!begin) {
	    goto QueryColors_Usage;
	} 
	if (sscanf(buf, "%d",
		&color[ncolors].pixel
		) < 1) {
	    printf("Invalid argument(s): %s\n", buf);
QueryColors_Usage:
	    printf("Usage:\n");
	    printf("    QueryColors <result_format> [Colormap]\n");
	    printf("\tBEGIN\n");
	    printf("\t<Pixel>\n");
	    printf("\t<Pixel>\n");
	    printf("\t.\t.\n");
	    printf("\t.\t\n");
	    printf("\t.\t\n");
	    printf("\tEND\n\n");
	    printf("    Where:\n");
	    printf("\tColormap :== string\n");
	    printf("\tPixel :== integer\n");
	    printf("\tFormat :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	    printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	    return(0);
	}
	printf("\t%5lu\n", color[ncolors].pixel);
	ncolors++;
    }

    printf("    RESULT:\n");
    retval = XcmsQueryColors(pDpy, cmap, color, ncolors, result_format);
    if (!CheckPrintRetVal ("XcmsQueryColors", retval, NO_COMP_FLAG))
	return(0);
    PrintXcmsColors(color, ncolors);
    return(1);
}


/*
 *	NAME
 *		Cmd_ConvertColor - LibTest "ConvertColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_ConvertColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"ConvertColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor color;
    char fromFormatStr[BUFSIZ];
    char toFormatStr[BUFSIZ];
    char stim1[BUFSIZ];
    char stim2[BUFSIZ];
    char stim3[BUFSIZ];
    char tmpstr[BUFSIZ];
    XcmsColorFormat	toFormat;
    Bool compressed;
    Status retval;

    if (!Check_init()) {
	return(0);
    }

    fromFormatStr[0] = '\0';
    toFormatStr[0] = '\0';
    stim1[0] = '\0';
    stim2[0] = '\0';
    stim3[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto ConvertColor_Usage;
    }

    if (sscanf(buf, "%s %s %s %s %s",
	    fromFormatStr,
	    stim1,
	    stim2,
	    stim3,
	    toFormatStr
	    ) != 5) {
	printf("Invalid argument(s): %s\n", buf);
ConvertColor_Usage:
	printf("Usage:\n");
	printf("    ConvertColor <From_Format> <Stim1> <Stim2> <Stim3> <To_Format>\n\n");
	printf("    Where:\n");
	printf("\tFromFormat, ToFormat :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	printf("\tStim1,Stim2,Stim3 :== int for RGB, otherwise float\n");
	return(0);
    }
    color.format = LtStrToDefine(FormatTbl, fromFormatStr);
    if (color.format == XcmsRGBFormat) {
	color.spec.RGB.red = atoi(stim1);
	color.spec.RGB.green = atoi(stim2);
	color.spec.RGB.blue = atoi(stim3);
    } else {
	color.spec.CIEXYZ.X = atof(stim1);
	color.spec.CIEXYZ.Y = atof(stim2);
	color.spec.CIEXYZ.Z = atof(stim3);
    }
    toFormat = LtStrToDefine(FormatTbl, toFormatStr);

    printf("    FROM:\n");
    PrintXcmsColorSpec(&color);
    retval = XcmsConvertColors(_test_ccc, &color, 1, toFormat, &compressed);
    if (!CheckPrintRetVal ("XcmsConvertColors", retval, COMP_FLAG))
	return(0);
    printf("    TO:\n");
    PrintXcmsColorSpec(&color);
    return(1);
}


/*
 *	NAME
 *		Cmd_LookupColor - LibTest "LookupColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_LookupColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"LookupColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor scrnColor, exactColor;
    Colormap cmap;
    char colorStr[BUFSIZ], cmapname[BUFSIZ];
    char tmpstr[BUFSIZ];
    char resultFormatStr[BUFSIZ];
    int nargs;
    Status retval;
    XcmsColorFormat result_format;

    if (!Check_init()) {
	return(0);
    }

    colorStr[0] = '\0';
    cmapname[0] = '\0';
    tmpstr[0] = '\0';
    resultFormatStr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto LookupColor_Usage;
    }

    if ((nargs = sscanf(buf, "%s %s",
	    colorStr,
	    resultFormatStr
	    )) < 2) {
	printf("Invalid argument(s): %s\n", buf);
LookupColor_Usage:
	printf("Usage:\n");
	printf("    LookupColor <color_string> <result_format> [Colormap]\n\n");
	printf("    Where:\n");
	printf("\tcolor_string :== string\n");
	printf("\tresult_format :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	printf("\tColormap :== string\n");
	return(0);
    }
    result_format = LtStrToDefine(FormatTbl, resultFormatStr);
    if (nargs < 3) {
	strcpy(cmapname, "TstCmap_AllocColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tLookup:\t%s\n", colorStr);
    printf("\tresult_format:\t%s\n", resultFormatStr);
    printf("\tColormap:\t%s\n", cmapname);
    printf("    RESULT:\n");
    retval = XcmsLookupColor(pDpy, cmap, colorStr, &exactColor,
			     &scrnColor, result_format);
    if (!CheckPrintRetVal ("XcmsLookupColor", retval, NO_COMP_FLAG))
	return(0);
    printf("\tScreen:\n");
    PrintXcmsColor(&scrnColor);
    printf("\tExact:\n");
    PrintXcmsColorSpec(&exactColor);
    return(1);
}


/*
 *	NAME
 *		Cmd_AllocNamedColor - LibTest "AllocNamedColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_AllocNamedColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"AllocNamedColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor scrnColor;
    XcmsColor exactColor;
    Colormap cmap;
    int nargs;
    char colorStr[BUFSIZ], cmapname[BUFSIZ];
    XcmsColorFormat result_format;
    char resultFormatStr[BUFSIZ];
    char tmpstr[BUFSIZ];
    Status retval;

    if (!Check_init()) {
	return(0);
    }

    colorStr[0] = '\0';
    cmapname[0] = '\0';
    resultFormatStr[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto AllocNamedColor_Usage;
    }

    if ((nargs = sscanf(buf, "%s %s %s",
	    colorStr,
	    resultFormatStr,
	    cmapname
	    )) < 2) {
	printf("Invalid argument(s): %s\n", buf);
AllocNamedColor_Usage:
	printf("Usage:\n");
	printf("    AllocNamedColor <color_string> <result_format> [Colormap]\n\n");
	printf("    Where:\n");
	printf("\tcolor_string :== string\n");
	printf("\tresult_format :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	printf("\tColormap :== string\n");
	return(0);
    }
    result_format = LtStrToDefine(FormatTbl, resultFormatStr);
    if (nargs < 3) {
	strcpy(cmapname, "TstCmap_AllocColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tStore:\t%s\n", colorStr);
    printf("\tresult_format:\t%s\n", resultFormatStr);
    printf("\tColormap:\t%s\n", cmapname);

    printf("    RESULT:\n");
    retval = XcmsAllocNamedColor(pDpy, cmap, colorStr, &scrnColor, 
				 &exactColor, result_format);
    if (!CheckPrintRetVal ("XcmsAllocNamedColor", retval, COMP_FLAG))
	return(0);
    printf("\tScreen:\n");
    PrintXcmsColor(&scrnColor);
    printf("\tExact:\n");
    PrintXcmsColorSpec(&exactColor);
    return(1);
}

    /****************************************************
     *							*
     *		QUERY GAMUT BOUNDARIES			*
     *							*
     ****************************************************/

/*
 *	NAME
 *		Cmd_MaxChroma - LibTest "MaxChroma" command
 *
 *	SYNOPSIS
 */
int
Cmd_MaxChroma(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"MaxChroma" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor HVC_return;
    char tmpstr[BUFSIZ];

    if (!Check_init()) {
	return(0);
    }

    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto MaxChroma_Usage;
    }

    if (sscanf(buf, "%lf %lf",
	    &HVC_return.spec.TekHVC.H,
	    &HVC_return.spec.TekHVC.V
	    ) < 2) {
	printf("Invalid argument(s): %s", buf);
MaxChroma_Usage:
	printf("Usage:\n");
	printf("    MaxChroma <Hue> <Value>\n\n");
	return(0);
    }
    HVC_return.spec.TekHVC.C = 0;
    HVC_return.format = XcmsTekHVCFormat;
    HVC_return.pixel = 0;
    printf("\tHUE VALUE IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsTekHVCQueryMaxC(_test_ccc,
	    HVC_return.spec.TekHVC.H,
	    HVC_return.spec.TekHVC.V,
	    &HVC_return)) == 0) {
	printf("\tXcmsTekHVCQueryMaxC() returned FAIL\n");
	return(0);
    }
    printf("\tRESULT:\n");
    PrintXcmsColorSpec(&HVC_return);
    return(1);
}


/*
 *	NAME
 *		Cmd_MaxValue - LibTest "MaxValue" command
 *
 *	SYNOPSIS
 */
int
Cmd_MaxValue(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"MaxValue" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor HVC_return;
    char tmpstr[BUFSIZ];

    if (!Check_init()) {
	return(0);
    }

    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto MaxValue_Usage;
    }

    if (sscanf(buf, "%lf %lf",
	    &HVC_return.spec.TekHVC.H,
	    &HVC_return.spec.TekHVC.C
	    ) < 2) {
	printf("Invalid argument(s): %s", buf);
MaxValue_Usage:
	printf("Usage:\n");
	printf("    MaxValue <Hue> <Chroma>\n\n");
	return(0);
    }
    HVC_return.spec.TekHVC.V = 0;
    HVC_return.format = XcmsTekHVCFormat;
    HVC_return.pixel = 0;
    printf("\tHUE CHROMA IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsTekHVCQueryMaxV(_test_ccc,
	    HVC_return.spec.TekHVC.H,
	    HVC_return.spec.TekHVC.C,
	    &HVC_return)) == 0) {
	printf("\tXcmsTekHVCQueryMaxV() returned FAIL\n");
	return(0);
    }
    printf("\tRESULT:\n");
    PrintXcmsColorSpec(&HVC_return);
    return(1);
}


/*
 *	NAME
 *		Cmd_MaxValueSamples - LibTest "MaxValueSamples" command
 *
 *	SYNOPSIS
 */
int
Cmd_MaxValueSamples(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"MaxValueSamples" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor *pHVC_return;
    XcmsFloat hue;
    char tmpstr[BUFSIZ];
    int nSamples;

    if (!Check_init()) {
	return(0);
    }

    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto MaxValueSamples_Usage;
    }

    if (sscanf(buf, "%lf %d",
	    &hue,
	    &nSamples
	    ) < 2) {
	printf("Invalid argument(s): %s", buf);
MaxValueSamples_Usage:
	printf("Usage:\n");
	printf("    MaxValueSamples <Hue> <nSamples>\n\n");
	return(0);
    }
    printf("\tHUE IN:\n");
    printf("\t    %lf\n", hue);
    pHVC_return = (XcmsColor *)calloc(nSamples, sizeof(XcmsColor));
    if (XcmsTekHVCQueryMaxVSamples(_test_ccc, hue, pHVC_return, nSamples) == 0) {
	printf("\tXcmsTekHVCQueryMaxVSamples() returned FAIL\n");
	free(pHVC_return);
	return(0);
    }
    printf("\tRESULT:\n");
    PrintXcmsColorSpecs(pHVC_return, nSamples);
    free(pHVC_return);
    return(1);
}


/*
 *	NAME
 *		Cmd_MaxValueChroma - LibTest "MaxValueChroma" command
 *
 *	SYNOPSIS
 */
int
Cmd_MaxValueChroma(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"MaxValueChroma" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor HVC_return;
    char tmpstr[BUFSIZ];

    if (!Check_init()) {
	return(0);
    }

    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto MaxValueChroma_Usage;
    }

    if (sscanf(buf, "%lf",
	    &HVC_return.spec.TekHVC.H
	    ) < 1) {
	printf("Invalid argument(s): %s", buf);
MaxValueChroma_Usage:
	printf("Usage:\n");
	printf("    MaxValueChroma <Hue>\n\n");
	return(0);
    }
    HVC_return.spec.TekHVC.V = HVC_return.spec.TekHVC.C = 0;
    HVC_return.format = XcmsTekHVCFormat;
    HVC_return.pixel = 0;
    printf("\tHUE IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsTekHVCQueryMaxVC(_test_ccc, HVC_return.spec.TekHVC.H,
	    &HVC_return)) == 0) {
	printf("\tXcmsTekHVCQueryMaxVC() returned FAIL\n");
	return(0);
    }
    printf("\tRESULT:\n");
    PrintXcmsColorSpec(&HVC_return);
    return(1);
}


/*
 *	NAME
 *		Cmd_MinValue - LibTest "MinValue" command
 *
 *	SYNOPSIS
 */
int
Cmd_MinValue(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"MinValue" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor HVC_return;
    char tmpstr[BUFSIZ];

    if (!Check_init()) {
	return(0);
    }

    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto MinValue_Usage;
    }

    if (sscanf(buf, "%lf %lf",
	    &HVC_return.spec.TekHVC.H,
	    &HVC_return.spec.TekHVC.C
	    ) < 2) {
	printf("Invalid argument(s): %s", buf);
MinValue_Usage:
	printf("Usage:\n");
	printf("    MinValue <Hue> <Chroma>\n\n");
	return(0);
    }
    HVC_return.spec.TekHVC.V = 0;
    HVC_return.format = XcmsTekHVCFormat;
    HVC_return.pixel = 0;
    printf("\tHUE CHROMA IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsTekHVCQueryMinV(_test_ccc,
	    HVC_return.spec.TekHVC.H,
	    HVC_return.spec.TekHVC.C,
	    &HVC_return)) == 0) {
	printf("\tXcmsTekHVCQueryMinV() returned FAIL\n");
	return(0);
    }
    printf("\tRESULT:\n");
    PrintXcmsColorSpec(&HVC_return);
    return(1);
}


/*
 *	NAME
 *		Cmd_AdjustValue - LibTest "AdjustValue" command
 *
 *	SYNOPSIS
 */
int
Cmd_AdjustValue(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"AdjustValue" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor HVC_return;
    char tmpstr[BUFSIZ];
    Bool compressed;

    if (!Check_init()) {
	return(0);
    }

    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto AdjustValue_Usage;
    }

    if (sscanf(buf, "%lf %lf %lf",
	    &HVC_return.spec.TekHVC.H,
	    &HVC_return.spec.TekHVC.V,
	    &HVC_return.spec.TekHVC.C
	    ) < 3) {
	printf("Invalid argument(s): %s", buf);
AdjustValue_Usage:
	printf("Usage:\n");
	printf("    AdjustValue <Hue> <Value> <Chroma>\n\n");
	return(0);
    }
    HVC_return.format = XcmsTekHVCFormat;
    HVC_return.pixel = 0;
    printf("\tHUE CHROMA IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsConvertColors(_test_ccc, &HVC_return, 1, 
			   XcmsCIEXYZFormat, NULL))
	== XcmsFailure) {
	printf("\tXcmsConvertColors returned FAIL\n");
	return(0);
    }
    printf("\t  CIEXYZ IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsTekHVCClipV(_test_ccc, &HVC_return, 1, 0, &compressed)) == 0) {
	printf("\tXcmsTekHVCClipV() returned FAIL\n");
	return(0);
    }
    printf("\tRESULT:\n");
    printf("\t  CIEXYZ RETURN:\n");
    PrintXcmsColorSpec(&HVC_return);
    printf("\t  TekHVC RETURN:\n");
    if ((XcmsConvertColors(_test_ccc, &HVC_return, 1, XcmsTekHVCFormat, NULL))
	== XcmsFailure) {
	printf("\tXcmsConvertColors returned FAIL\n");
	return(0);
    }
    PrintXcmsColorSpec(&HVC_return);
    return(1);
}


/*
 *	NAME
 *		Cmd_ReduceChroma - LibTest "ReduceChroma" command
 *
 *	SYNOPSIS
 */
int
Cmd_ReduceChroma(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"ReduceChroma" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor HVC_return;
    char tmpstr[BUFSIZ];
    Bool compressed;

    if (!Check_init()) {
	return(0);
    }

    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto ReduceChroma_Usage;
    }

    if (sscanf(buf, "%lf %lf %lf",
	    &HVC_return.spec.TekHVC.H,
	    &HVC_return.spec.TekHVC.V,
	    &HVC_return.spec.TekHVC.C
	    ) < 3) {
	printf("Invalid argument(s): %s", buf);
ReduceChroma_Usage:
	printf("Usage:\n");
	printf("    ReduceChroma <Hue> <Value> <Chroma>\n\n");
	return(0);
    }
    HVC_return.format = XcmsTekHVCFormat;
    HVC_return.pixel = 0;
    printf("\tHUE CHROMA IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsConvertColors(_test_ccc, &HVC_return, 1, XcmsCIEXYZFormat, NULL))
	== XcmsFailure) {
	printf("\tXcmsConvertColors returned FAIL\n");
	return(0);
    }
    printf("\t  CIEXYZ IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsTekHVCClipC(_test_ccc, &HVC_return, 1, 0, &compressed)) == 0) {
	printf("\tXcmsTekHVCClipC() returned FAIL\n");
	return(0);
    }
    printf("\tRESULT:\n");
    printf("\t    CIEXYZ RETURN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsConvertColors(_test_ccc, &HVC_return, 1, XcmsTekHVCFormat, NULL))
	== XcmsFailure) {
	printf("\tXcmsConvertColors returned FAIL\n");
	return(0);
    }
    printf("\t  TekHVC RETURN:\n");
    PrintXcmsColorSpec(&HVC_return);
    return(1);
}


/*
 *	NAME
 *		Cmd_ShortestValueChroma - LibTest "ShortestValueChroma" command
 *
 *	SYNOPSIS
 */
int
Cmd_ShortestValueChroma(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"ShortestValueChroma" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    XcmsColor HVC_return;
    char tmpstr[BUFSIZ];
    Bool compressed;

    if (!Check_init()) {
	return(0);
    }

    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto ShortestValueChroma_Usage;
    }

    if (sscanf(buf, "%lf %lf %lf",
	    &HVC_return.spec.TekHVC.H,
	    &HVC_return.spec.TekHVC.V,
	    &HVC_return.spec.TekHVC.C
	    ) < 3) {
	printf("Invalid argument(s): %s", buf);
ShortestValueChroma_Usage:
	printf("Usage:\n");
	printf("    ShortestValueChroma <Hue> <Value> <Chroma>\n\n");
	return(0);
    }
    HVC_return.format = XcmsTekHVCFormat;
    HVC_return.pixel = 0;
    printf("\tHUE CHROMA IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsConvertColors(_test_ccc, &HVC_return, 1, XcmsCIEXYZFormat, NULL))
	== XcmsFailure) {
	printf("\tXcmsConvertColors returned FAIL\n");
	return(0);
    }
    printf("\t  CIEXYZ IN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsTekHVCClipVC(_test_ccc, &HVC_return, 1, 0,
	&compressed)) == 0) {
	printf("\tXcmsTekHVCClipVC() returned FAIL\n");
	return(0);
    }
    printf("\tRESULT:\n");
    printf("\t    CIEXYZ RETURN:\n");
    PrintXcmsColorSpec(&HVC_return);
    if ((XcmsConvertColors(_test_ccc, &HVC_return, 1, XcmsTekHVCFormat, NULL))
	== XcmsFailure) {
	printf("\tXcmsConvertColors returned FAIL\n");
	return(0);
    }
    printf("\t  TekHVC RETURN:\n");
    PrintXcmsColorSpec(&HVC_return);
    return(1);
}


    /****************************************************
     *							*
     *		COLOR SPACE EXTENSION			*
     *							*
     ****************************************************/

/*
 *	NAME
 *		Cmd_PrefixOfId - LibTest "PrefixOfId" command
 *
 *	SYNOPSIS
 */
int
Cmd_PrefixOfId(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"PrefixOfId" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];
    char formatStr[BUFSIZ];
    char *prefix;
    XcmsColorFormat formatID;

    tmpstr[0] = '\0';
    formatStr[0] = '\0';

    if (!Check_init()) {
	return(0);
    }

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto PrefixOfId_Usage;
    }

    if (sscanf(buf, "%s", formatStr) < 1) {
	printf("Invalid argument(s): %s\n", buf);
PrefixOfId_Usage:
	printf("Usage:\n");
	printf("    PrefixOfId <Format>\n\n");
	printf("    Where:\n");
	printf("\tFormat :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	return(0);
    }
    formatID = LtStrToDefine(FormatTbl, formatStr);

    printf("    REQUESTED:\n");
    printf("\tFormat:\t%s\n", formatStr);

    printf("    RESULT:\n");
    if ((prefix = XcmsPrefixOfFormat(formatID)) == NULL) {
	printf("\tXcmsPrefixOfFormat() returned NULL\n");
	return(0);
    }
    printf("\tXcmsPrefixOfFormat() returned %s\n", prefix);
    return(1);
}

/*
 *	NAME
 *		Cmd_FormatOfPrefix - LibTest "FormatOfPrefix" command
 *
 *	SYNOPSIS
 */
int
Cmd_FormatOfPrefix(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"FormatOfPrefix" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];
    char prefix[BUFSIZ];
    XcmsColorFormat formatID;

    tmpstr[0] = '\0';
    prefix[0] = '\0';

    if (!Check_init()) {
	return(0);
    }

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto FormatOfPrefix_Usage;
    }

    if (sscanf(buf, "%s", prefix) < 1) {
	printf("Invalid argument(s): %s\n", buf);
FormatOfPrefix_Usage:
	printf("Usage:\n");
	printf("    FormatOfPrefix <prefix>\n\n");
	printf("    Where:\n");
	printf("\tprefix :== string\n");
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tprefix:\t%s\n", prefix);

    printf("    RESULT:\n");
    formatID = XcmsFormatOfPrefix(prefix);
    printf("\tXcmsFormatOfPrefix() returned %u\n", formatID);
    printf("\t    %s\n", LtDefineToStr(FormatTbl, (int)formatID));
    return(1);
}

/*
 *	NAME
 *		Cmd_AddDIColorSpace - LibTest "AddDIColorSpace" command
 *
 *	SYNOPSIS
 */
int
Cmd_AddDIColorSpace(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"AddDIColorSpace" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    char tmpstr[BUFSIZ];
    char colorspaceStr[BUFSIZ];
    XcmsColorSpace *pSCCFuncSet = NULL;
    Status retval;

    tmpstr[0] = '\0';
    colorspaceStr[0] = '\0';

    if (!Check_init()) {
	return(0);
    }

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto AddDIColorSpace_Usage;
    }

    if (sscanf(buf, "%s", colorspaceStr) < 1) {
	printf("Invalid argument(s): %s\n", buf);
AddDIColorSpace_Usage:
	printf("Usage:\n");
	printf("    AddDIColorSpace <Format>\n\n");
	printf("    Where:\n");
	printf("\tFormat :== CIELab | CIELuv | TekHVC\n");
	return(0);
    }
    if (strcmp(colorspaceStr, "CIELab") == 0) {
	pSCCFuncSet = &XcmsCIELabColorSpace;
    } else if (strcmp(colorspaceStr, "CIELuv") == 0) {
	pSCCFuncSet = &XcmsCIELuvColorSpace;
    } else if (strcmp(colorspaceStr, "TekHVC") == 0) {
	pSCCFuncSet = &XcmsTekHVCColorSpace;
    }

    printf("    REQUESTED:\n");
    printf("\tFormat:\t%s\n", colorspaceStr);

    printf("    RESULT:\n");
    retval = XcmsAddColorSpace(pSCCFuncSet);
    return(CheckPrintRetVal("XcmsAddColorSpace", retval, NO_COMP_FLAG));
}

/*
 *	NAME
 *		Cmd_XAllocNamedColor - LibTest "XAllocNamedColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_XAllocNamedColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"XAllocNamedColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    XColor hardware_def;
    XColor rgb_db_def;
    int nargs;
    char exactFormatStr[BUFSIZ];
    char colorStr[BUFSIZ];
    char cmapname[BUFSIZ];
    char tmpstr[BUFSIZ];

    if (!Check_init()) {
	return(0);
    }

    exactFormatStr[0] = '\0';
    colorStr[0] = '\0';
    cmapname[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto XAllocNamedColor_Usage;
    }

    if ((nargs = sscanf(buf, "%s %s", colorStr, cmapname)) < 1) {
	printf("Invalid argument(s): %s\n", buf);
XAllocNamedColor_Usage:
	printf("Usage:\n");
	printf("    XAllocNamedColor <color_string> [Colormap]\n\n");
	printf("    Where:\n");
	printf("\tcolor_string :== string\n");
	printf("\tColormap :== string\n");
	return(0);
    }
    if (nargs < 2) {
	strcpy(cmapname, "TstCmap_AllocColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tColor String:\t%s\n", colorStr);
    printf("\tColormap:\t%s\n", cmapname);
    printf("    RESULT:\n");
    if ((XAllocNamedColor(pDpy, cmap, colorStr, &hardware_def,
	    &rgb_db_def)) == 0) {
	printf("\tXcmsAllocNamedColor() returned FAIL\n");
	return(0);
    }
    printf("\t    rgb_db_def.red   = 0x%x\n", rgb_db_def.red);
    printf("\t    rgb_db_def.green = 0x%x\n", rgb_db_def.green);
    printf("\t    rgb_db_def.blue  = 0x%x\n", rgb_db_def.blue);
    printf("\t    hardware_def.pixel   = %d\n", hardware_def.pixel);
    printf("\t    hardware_def.red   = 0x%x\n", hardware_def.red);
    printf("\t    hardware_def.green = 0x%x\n", hardware_def.green);
    printf("\t    hardware_def.blue  = 0x%x\n", hardware_def.blue);
    return(1);
}


/*
 *	NAME
 *		Cmd_XLookupColor - LibTest "XLookupColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_XLookupColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"XLookupColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    XColor hardware_def;
    XColor rgb_db_def;
    int nargs;
    char colorStr[BUFSIZ];
    char cmapname[BUFSIZ];
    char tmpstr[BUFSIZ];

    if (!Check_init()) {
	return(0);
    }

    colorStr[0] = '\0';
    cmapname[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto XLookupColor_Usage;
    }

    if ((nargs = sscanf(buf, "%s %s %s", colorStr, cmapname)) < 1) {
	printf("Invalid argument(s): %s\n", buf);
XLookupColor_Usage:
	printf("Usage:\n");
	printf("    XLookupColor <color_string> [Colormap]\n\n");
	printf("    Where:\n");
	printf("\tcolor_string :== string\n");
	printf("\tColormap :== string\n");
	return(0);
    }
    if (nargs < 3) {
	strcpy(cmapname, "TstCmap_AllocColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tColor String:\t%s\n", colorStr);
    printf("\tColormap:\t%s\n", cmapname);
    printf("    RESULT:\n");
    if ((XLookupColor(pDpy, cmap, colorStr, &rgb_db_def, &hardware_def)) == 0) {
	printf("\tXLookupColor() returned FAIL\n");
	return(0);
    }
    printf("\t    rgb_db_def.red   = 0x%x\n", rgb_db_def.red);
    printf("\t    rgb_db_def.green = 0x%x\n", rgb_db_def.green);
    printf("\t    rgb_db_def.blue  = 0x%x\n", rgb_db_def.blue);
    printf("\t    hardware_def.red   = 0x%x\n", hardware_def.red);
    printf("\t    hardware_def.green = 0x%x\n", hardware_def.green);
    printf("\t    hardware_def.blue  = 0x%x\n", hardware_def.blue);
    return(1);
}


/*
 *	NAME
 *		Cmd_XParseColor - LibTest "XParseColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_XParseColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"XParseColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    XColor exactSpec;
    int nargs;
    char exactFormatStr[BUFSIZ];
    char colorStr[BUFSIZ];
    char cmapname[BUFSIZ];
    char tmpstr[BUFSIZ];

    if (!Check_init()) {
	return(0);
    }

    exactFormatStr[0] = '\0';
    colorStr[0] = '\0';
    cmapname[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto XParseColor_Usage;
    }

    if ((nargs = sscanf(buf, "%s %s", colorStr, cmapname)) < 1) {
	printf("Invalid argument(s): %s\n", buf);
XParseColor_Usage:
	printf("Usage:\n");
	printf("    XParseColor <color_string> [colormap]\n\n");
	return(0);
    }
    if (nargs < 2) {
	strcpy(cmapname, "TstCmap_AllocColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tParse:\t%s\n", colorStr);
    printf("\tColormap:\t%s\n", cmapname);
    printf("    RESULT:\n");
    if ((XParseColor(pDpy, cmap, colorStr, &exactSpec)) == 0) {
	printf("\tXcmsParseColor() returned FAIL\n");
	return(0);
    }
    printf("\t    spec.red   = 0x%x\n", exactSpec.red);
    printf("\t    spec.green = 0x%x\n", exactSpec.green);
    printf("\t    spec.blue  = 0x%x\n", exactSpec.blue);
    return(1);
}


/*
 *	NAME
 *		Cmd_XStoreNamedColor - LibTest "StoreNamedColor" command
 *
 *	SYNOPSIS
 */
int
Cmd_XStoreNamedColor(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"XStoreNamedColor" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    int nargs;
    char colorStr[BUFSIZ], cmapname[BUFSIZ];
    char tmpstr[BUFSIZ];
    unsigned long pixel;

    colorStr[0] = '\0';
    cmapname[0] = '\0';
    tmpstr[0] = '\0';


    if (!Check_init()) {
	return(0);
    }

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto XStoreNamedColor_Usage;
    }

    if ((nargs = sscanf(buf, "%s %lu %s %s",
	    colorStr,
	    &pixel,
	    cmapname
	    )) < 2) {
	printf("Invalid argument(s): %s\n", buf);
XStoreNamedColor_Usage:
	printf("Usage:\n");
	printf("    XStoreNamedColor <color_string> <pixel> [Colormap]\n\n");
	printf("    Where:\n");
	printf("\tcolor_string :== string\n");
	printf("\tpixel :== unsigned int\n");
	printf("\tColormap :== string\n");
	return(0);
    }
    if (nargs < 3) {
	strcpy(cmapname, "TstCmap_StoreColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tStore:\t%s\n", colorStr);
    printf("\tpixel:\t%lu\n", pixel);
    printf("\tColormap:\t%s\n\n", cmapname);

    XStoreNamedColor(pDpy, cmap, colorStr, pixel, DoRed | DoGreen | DoBlue);
    printf("\tXStoreNamedColor() has no return value\n");
    printf("\t    Use QueryColor() to see what was stored\n");
    return(1);
}

/*
 *	NAME
 *		Cmd_StoreColors - LibTest "StoreColors" command
 *
 *	SYNOPSIS
 */
int
Cmd_StoreColors(buf)
    char *buf;
/*
 *	DESCRIPTION
 *		"StoreColors" command for LibTest.
 *
 *	RETURNS
 *		0=fail, 1=Success
 *
 */
{
    Colormap cmap;
    XcmsColor color[MAXCOLORS];
    Bool compressed[MAXCOLORS];
    char firstFormatStr[BUFSIZ];
    char formatStr[BUFSIZ];
    char cmapname[BUFSIZ];
    char stim1[BUFSIZ];
    char stim2[BUFSIZ];
    char stim3[BUFSIZ];
    char tmpstr[BUFSIZ];
    int begin;
    Status retval;
    int ncolors;

    if (!Check_init()) {
	return(0);
    }

    firstFormatStr[0] = '\0';
    formatStr[0] = '\0';
    cmapname[0] = '\0';
    stim1[0] = '\0';
    stim2[0] = '\0';
    stim3[0] = '\0';
    tmpstr[0] = '\0';

    if (sscanf(buf, "%s", tmpstr) && (strcmp("-help", tmpstr) == 0)) {
	goto StoreColors_Usage;
    }

    if (sscanf(buf, "%s", cmapname) < 1) {
	strcpy(cmapname, "TstCmap_StoreColor");
    }
    if (StrToCmap(CmapTbl, cmapname, &cmap) == 0) {
	printf("Error: Colormap named %s not found\n", cmapname);
	return(0);
    }

    printf("    REQUESTED:\n");
    printf("\tColormap:\t%s\n", cmapname);
    printf("\tPixel\tFormat\tStim1\tStim2\tStim3\n");
    for(ncolors = 0;;) {
	if (ncolors >= MAXCOLORS) {
	    return(0);
	}
	if (fgets(buf, BUFSIZ, stdin) == NULL) {
	    return(0);
	}
	if (EchoInput) {
	    printf("%s", buf);
	}
	if (buf[0] == LIBTEST_COMMENT_CHAR) {
	    continue;
	}
	if (strcmp(buf, "BEGIN\n") == 0) {
	    begin = 1;
	    continue;
	}
	if (begin && (strcmp(buf, "END\n") == 0)) {
	    break;
	}
	if (!begin) {
	    goto StoreColors_Usage;
	} 
	if (sscanf(buf, "%d %s %s %s %s",
		&color[ncolors].pixel,
		formatStr,
		stim1,
		stim2,
		stim3
		) < 5) {
	    printf("Invalid argument(s): %s\n", buf);
StoreColors_Usage:
	    printf("Usage:\n");
	    printf("    StoreColors [Colormap]\n");
	    printf("\tBEGIN\n");
	    printf("\t<Pixel>\t<Format>\t<Stim1>\t<Stim2>\t<Stim3>\n");
	    printf("\t<Pixel>\t<Format>\t<Stim1>\t<Stim2>\t<Stim3>\n");
	    printf("\t.\t.\t.\t.\t.\n");
	    printf("\t.\t.\t.\t.\t.\n");
	    printf("\t.\t.\t.\t.\t.\n");
	    printf("\tEND\n\n");
	    printf("    Where:\n");
	    printf("\tPixel :== integer\n");
	    printf("\tFormat :== UNDEFINED | CIEXYZ | CIExyY | CIEuvY\n");
	    printf("\t    | CIELab | CIELuv | TekHVC | RGBi | RGB\n");
	    printf("\tStim1,Stim2,Stim3 :== int for RGB, otherwise float\n");
	    printf("\tColormap :== string\n");
	    return(0);
	}
	color[ncolors].format = LtStrToDefine(FormatTbl, formatStr);
	if (color[ncolors].format == XcmsRGBFormat) {
	    color[ncolors].spec.RGB.red = atoi(stim1);
	    color[ncolors].spec.RGB.green = atoi(stim2);
	    color[ncolors].spec.RGB.blue = atoi(stim3);
	} else {
	    color[ncolors].spec.CIEXYZ.X = atof(stim1);
	    color[ncolors].spec.CIEXYZ.Y = atof(stim2);
	    color[ncolors].spec.CIEXYZ.Z = atof(stim3);
	}
	printf("\t%5lu\t%s\t%s\t%s\t%s\n", color[ncolors].pixel, formatStr,
		stim1, stim2, stim3);
	if (ncolors == 0)
	    strcpy (firstFormatStr, formatStr);
	ncolors++;
    }

    printf("\tFormat:\t%s\n", firstFormatStr);
    printf("    REQUESTED:\n");
    printf("\tColormap:\t%s\n", cmapname);
    PrintXcmsColors(color, ncolors);
    printf("    RESULT:\n");
    retval = XcmsStoreColors(pDpy, cmap, color, ncolors, compressed);
    if (!CheckPrintRetVal ("XcmsStoreColors", retval, COMP_FLAG))
	return(0);
    return(1);
}
