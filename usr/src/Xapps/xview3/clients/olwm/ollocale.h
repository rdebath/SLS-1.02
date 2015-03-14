/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)ollocale.h	1.6	91/09/14 SMI"

#ifndef _OLLOCALE_H
#define _OLLOCALE_H


#ifdef OW_I18N_L3

#include <locale.h>
/*
 * OPEN LOOK Locale Categories.  Basic Locale must be first item
 * (least number).
 */
#define	OLLC_LC_BASIC_LOCALE	0
#define	OLLC_LC_DISPLAY_LANG	1
#define	OLLC_LC_INPUT_LANG	2
#define	OLLC_LC_NUMERIC		3
#define	OLLC_LC_DATE_FORMAT	4
#define	OLLC_LC_MAX		5

/*
 * OPEN LOOK Locale priority (small number has higher priority).
 * Could not use enum data type, since we will do the comparison among
 * this numbers.
 */
#define	OLLC_SRC_PROGRAM	0
#define	OLLC_SRC_COMMAND_LINE	1
#define	OLLC_SRC_RESOURCE	2
#define	OLLC_SRC_POSIX		3

typedef struct _OLLCItem {
	char	*locale;
	int	priority;
	int	posix_category;		/* Will initialize in InitGRVLV() */
} OLLCItem;

/*
 * This struct sometimes access as array.  Watch out!
 */
typedef struct _OLLC {
	OLLCItem	BasicLocale;
	OLLCItem	DisplayLang;
	OLLCItem	InputLang;
	OLLCItem	Numeric;
	OLLCItem	DateFormat;
} OLLC;

#endif /* OW_I18N_L3 */

#endif /* _OLLOCALE_H */
