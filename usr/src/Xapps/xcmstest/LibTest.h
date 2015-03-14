/*
 * (c) Copyright 1990 Tektronix Inc.
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
 *
 *	NAME
 *		LibTest.h
 *
 *	DESCRIPTION
 *		Public include file for the LibTest test interface tool
 *
 *	REVISION
 *		$Header: LibTest.h,v 1.2 91/07/31 22:46:40 keith Exp $
 */
#ifndef LIBTEST_H
#define LIBTEST_H

/*
 *	DEFINES
 */
#ifndef	GLOBAL
#  define	GLOBAL
#endif

/*
 *	EXTERNS
 */
extern	int CommandArgc;	/* GLOBAL */
extern	char **CommandArgv;	/* GLOBAL */

/*
 *	TYPEDEFS
 */
#ifndef Status
typedef int Status;
#endif

typedef Status (*PFStatus)();

typedef struct {
    char *pstring;
    PFStatus pfunc;
} FuncTableEntry;

typedef struct _LtDefineEntry{
    char		*pstring;
    unsigned long	define;
} LtDefineEntry;

extern PFStatus LtStrToFuncPtr();
extern char *LtDefineToStr();
extern int  LtStrToDefine();

#endif /* LIBTEST_H */
