/* $XConsortium: XimpXConv.c,v 1.3 92/04/14 13:30:36 rws Exp $ */
/*
 * Copyright 1990, 1991, 1992 by TOSHIBA Corp.
 * Copyright 1990, 1991, 1992 by SORD Computer Corp.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of TOSHIBA Corp. and SORD Computer Corp.
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  TOSHIBA Corp. and
 * SORD Computer Corp. make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *
 * TOSHIBA CORP. AND SORD COMPUTER CORP. DISCLAIM ALL WARRANTIES WITH REGARD
 * TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL TOSHIBA CORP. OR SORD COMPUTER CORP. BE LIABLE
 * FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author: Katsuhisa Yano	TOSHIBA Corp.
 *				mopi@ome.toshiba.co.jp
 *	   Osamu Touma		SORD Computer Corp.
 */

/******************************************************************

              Copyright 1991, 1992 by FUJITSU LIMITED

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED
not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.
FUJITSU LIMITED makes no representations about the suitability of
this software for any purpose.  It is provided "as is" without
express or implied warranty.

FUJITSU LIMITED DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
IN NO EVENT SHALL FUJITSU LIMITED BE LIABLE FOR ANY SPECIAL, INDIRECT
OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE
OR PERFORMANCE OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 

******************************************************************/
/*
	HISTORY:

	An sample implementation for Xi18n function of X11R5
	based on the public review draft 
	"Xlib Changes for internationalization,Nov.1990"
	by Katsuhisa Yano,TOSHIBA Corp. and Osamu Touma,SORD Computer Corp..

	Modification to the high level pluggable interface is done
	by Takashi Fujiwara,FUJITSU LIMITED.
*/

#include "Xlibint.h"
#include "Xlcint.h"
#include "Ximplc.h"

static XFontStruct *
getFont(fontset, str, xstr)
    FontSetRec *fontset;
    unsigned char *str;
    unsigned char *xstr;
{
    register ExtFontRec *ext_font = fontset->ext_font_list;
    int ext_font_num = fontset->ext_font_num;
    register unsigned char ch;
    unsigned char ch_L, ch_R;

    ch_L = *str & 0x7f;
    ch_R = ch_L | 0x80;

    while (ext_font_num--) {
	ch = ext_font->msb_mask ? ch_R : ch_L;
	if (ch > ext_font->min_char && ch < ext_font->max_char) {
	    *xstr = ch;
	    return ext_font->font;
	}
	ext_font++;
    }

    *xstr = fontset->msb_mask ? ch_R : ch_L;

    return fontset->font;
}

static XFontStruct *
getFont16(fontset, str, xstr)
    FontSetRec *fontset;
    unsigned char *str;
    XChar2b *xstr;
{
    register ExtFontRec *ext_font = fontset->ext_font_list;
    int ext_font_num = fontset->ext_font_num;
    register unsigned xchar;
    unsigned xchar_L, xchar_R;

    xchar_L = ((unsigned) *str << 8 | *(str+1)) & 0x7f7f;
    xchar_R = xchar_L | 0x8080;

    while (ext_font_num--) {
	xchar = ext_font->msb_mask ? xchar_R : xchar_L;
	if (xchar > ext_font->min_char && xchar < ext_font->max_char)
	    goto done;
	ext_font++;
    }

    xchar = ext_font->msb_mask ? xchar_R : xchar_L;
done:
    xstr->byte1 = xchar >> 8;
    xstr->byte2 = (unsigned char) xchar;

    return fontset->font;
}

int
_Ximp_cstoxchar(fontset, string, length, ret_buf, ret_len, ret_font)
    FontSetRec *fontset;
    unsigned char *string;
    register int length;
    unsigned char *ret_buf;
    int *ret_len;
    XFontStruct **ret_font;
{
    register unsigned char mask, *strptr = string;
    register unsigned char *bufptr = ret_buf;
    XFontStruct *font;

    if (fontset->font == NULL)
	return -1;
    
    if (length > *ret_len)
	length = *ret_len;
    if (length < 1)
	return 0;

    if (fontset->ext_font_num == 0) {
	font = fontset->font;
	mask = fontset->msb_mask;
	while (length--)
	    *bufptr++ = (*strptr++ & 0x7f) | mask;
	goto done;
    }

    font = getFont(fontset, strptr, bufptr);
    strptr++;
    bufptr++;
    length--;

    for ( ; length > 0; length--, strptr++, bufptr++)
	if (font != getFont(fontset, strptr, bufptr))
	    break;

done:
    *ret_len = bufptr - ret_buf;
    *ret_font = font;

    return strptr - string;
}

int
_Ximp_cstoxchar2b(fontset, string, length, ret_buf, ret_len, ret_font)
    FontSetRec *fontset;
    unsigned char *string;
    register int length;
    XChar2b *ret_buf;
    int *ret_len;
    XFontStruct **ret_font;
{
    register unsigned char mask, *strptr = string;
    register XChar2b *bufptr = ret_buf;
    XFontStruct *font;

    if (fontset->font == NULL)
	return -1;
    
    length >>= 1;
    if (length > *ret_len)
	length = *ret_len;
    if (length < 1)
	return 0;

    if (fontset->ext_font_num == 0) {
	font = fontset->font;
	mask = fontset->msb_mask;
	while (length--) {
	    bufptr->byte1 = (*strptr++ & 0x7f) | mask;
	    bufptr->byte2 = (*strptr++ & 0x7f) | mask;
	    bufptr++;
	}
	goto done;
    }

    font = getFont16(fontset, strptr, bufptr);
    strptr += 2;
    bufptr++;
    length--;

    for ( ; length > 0; length--, strptr += 2, bufptr++)
	if (font != getFont16(fontset, strptr, bufptr))
	    break;

done:
    *ret_len = bufptr - ret_buf;
    *ret_font = font;

    return strptr - string;
}
