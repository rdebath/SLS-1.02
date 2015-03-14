/*       @(#)charset.h 50.5 91/09/14 SMI        */

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

/* Misc definition about character set, EUC wide characters, and such. */
/* @(#)charset.h	50.5 91/09/14 */

#ifndef xview_charset_DEFINED
#define xview_charset_DEFINED

#ifdef OW_I18N

#include <widec.h>
/* Wide characters... */
#ifndef	WCHAR_CS0	
/* Following macros are defined in <wchar.h> via <stdlib.h> in ALE
 * except JLE 1.0.3 and eariler.
 */
#define	WCHAR_CSMASK	0x8080
#define	WCHAR_CS0	0x0000
#define	WCHAR_CS1	0x8080
#define	WCHAR_CS2	0x0080
#define	WCHAR_CS3	0x8000
#endif

#undef WCHAR_BYTE_OF
#ifdef LONG_WCHAR_T
#define WCHAR_BYTE_OF(wc,i) (((wc&~0x60000000)>>(7*i))&0x7f)
#else
#define	WCHAR_BYTE_OF(wc,i) (((wc&~0x8080)>>(8*i))&0x7f)
#endif
	/* NOTE: WCHAR_BYTE_OF in eariler versoins of xLE is wrong.*/

/* Following values are used to record the current character set
 * assigned for GL and GR.
 * The value is two byte of which the first byte tells the type
 * of character set and the second byte is the "final" character
 * of the designation sequnce for the character set.
 * This scheme works fine until someone invent a character set
 * for which one or more intermediate character is needed to
 * designate.
 */
#define	_94S		0x100		/* Single byte 94 glyph set */
#define	_96S		0x200		/* Single byte 96 glyph set */
#define	_94M		0x400		/* Multi byte 94*94 glyph set */
#define	ASCII		_94S+0x42
#define	JISX0201R	_94S+0x49	
#define	JISX0201L	_94S+0x4a	/* Handled as though ASCII here.*/
#define	ISO8859_1R	_96S+0x41
#define	ISO8859_2R	_96S+0x42
#define	ISO8859_3R	_96S+0x43
#define	GB2312		_94M+0x41	/* PRC Hanzi. */
#define	JISX0208	_94M+0x42	/* Japanese Level 1+2. */
#define	KSC5601		_94M+0x43	/* Korean. */
#define CNS1		_94M+0x30	/* tchinese */
#define CNS2		_94M+0x31	/* tchinese */
#define CNS3		_94M+0x32	/* tchinese */

#define	Esc		27

#endif OW_I18N

#endif xview_charset_DEFINED
