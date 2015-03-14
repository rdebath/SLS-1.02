/* $XConsortium: XimpTxtEsc.c,v 1.2 92/04/14 13:30:02 rws Exp $ */
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

/*
 *	_Ximp_mb_escapement()
 *	_Ximp_wc_escapement()
 */

{
    Ximp_XLCd lcd = (Ximp_XLCd) xfont_set->core.lcd;
    unsigned char *strptr, strbuf[BUFSIZE];
    unsigned char xchar_buf[BUFSIZE];
    XChar2b xchar2b_buf[BUFSIZE];
    FontSetRec *fontset;
    XFontStruct *font;
    int (*cnv_func)();
    int cset_num, char_length;
    int count, length, tmp_len;
    int width = 0;

    cnv_func = lcd->ximp_lcpart->methods->CNV_FUNC;

    (*lcd->ximp_lcpart->methods->cnv_start)(lcd);

    while (text_length > 0) {
        length = BUFSIZE;
	count = (*cnv_func)(lcd, text, text_length, strbuf, &length,
			    &cset_num, &char_length);
	if (count <= 0)
	    break;

	text += count;
	text_length -= count;

	strptr = strbuf;
	fontset = ((Ximp_XFontSet) xfont_set)->ximp_fspart->fontset + cset_num;
	if (fontset == NULL)
	    continue;
	while (length > 0) {
	    tmp_len = BUFSIZE;
	    if (char_length < 2)
	    	count = _Ximp_cstoxchar(fontset, strptr, length,
					xchar_buf, &tmp_len, &font);
	    else
		count = _Ximp_cstoxchar2b(fontset, strptr, length,
					  xchar2b_buf, &tmp_len, &font);
	    if (count <= 0)
		break;

	    if (char_length < 2)
		width += XTextWidth(font, (char *)xchar_buf, tmp_len);
            else
		width += XTextWidth16(font, xchar2b_buf, tmp_len);

	    strptr += count;
	    length -= count;
	}
    }

    (*lcd->ximp_lcpart->methods->cnv_end)(lcd);

    return width;
}
