/* $XConsortum: TekCMSglob.c,v 1.1 91/02/11 19:40:44 dave Exp $ */

/*
 * (c) Copyright 1988, Tektronix Inc.
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
 *		globals.c -- globals
 *
 *	DESCRIPTION
 */

/*
 *      EXTERNAL INCLUDES
 *              Include files that must be exported to any package or
 *              program using this package.
 */
/* this file contains the typedef for LtDefineEntry */
#include "LibTest.h"


/*
 *      INTERNAL INCLUDES
 *              Include files that need NOT be exported to any package or
 *              program using this package.
 */
#include <X11/Xlib.h>
#include <X11/Xcms.h>

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

#define THOUSAND	1000

LtDefineEntry ErrorTbl[] = {
    "Success",		Success,
    "BadRequest",	BadRequest,
    "BadValue",		BadValue,
    "BadWindow",	BadWindow,
    "BadPixmap",	BadPixmap,
    "BadAtom",		BadAtom,
    "BadCursor",	BadCursor,
    "BadFont",		BadFont,
    "BadMatch",		BadMatch,
    "BadDrawable",	BadDrawable,
    "BadAccess",	BadAccess,
    "BadAlloc",		BadAlloc,
    "BadColor",		BadColor,
    "BadGC",		BadGC,
    "BadIDChoice",	BadIDChoice,
    "BadName",		BadName,
    "BadLength",	BadLength,
    "BadImplementation",    BadImplementation,
    "FirstExtensionError",  FirstExtensionError,
    "LastExtensionError",   LastExtensionError,
    "",			    0
};

LtDefineEntry AllocTbl[] = {
    "AllocNone",	AllocNone,
    "AllocAll",		AllocAll,
    "illegala1",	THOUSAND,
    "",			0
};

LtDefineEntry FormatTbl[] = {
    "RGBi",		XcmsRGBiFormat,
    "RGB",		XcmsRGBFormat,
    "UNDEFINED",	XcmsUndefinedFormat,
    "CIEXYZ",		XcmsCIEXYZFormat,
    "CIExyY",		XcmsCIExyYFormat,
    "CIEuvY",		XcmsCIEuvYFormat,
    "CIELab",		XcmsCIELabFormat,
    "CIELuv",		XcmsCIELuvFormat,
    "TekHVC",		XcmsTekHVCFormat,
    "",			0
};
