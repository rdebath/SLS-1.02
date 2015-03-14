/* $XConsortium: XimpTxtPr.c,v 1.3 92/04/14 13:30:11 rws Exp $ */
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
	by Katsuhisa Yano,TOSHIBA Corp..

	Modification to the high level pluggable interface is done
	by Takashi Fujiwara,FUJITSU LIMITED.
*/

/*
 *	XmbTextListToTextProperty()
 *	XwcTextListToTextProperty()
 */

{
    Ximp_XLCd lcd;
    unsigned char *value, *buf, *buf_ptr;
    Atom encoding;
    int nitems, unconv_num;
    int tmp_len, tmp_num;
    int (*cnv_func)();

    if ((lcd = (Ximp_XLCd) _XlcCurrentLC()) == NULL)
	return XLocaleNotSupported;

    switch (style) {
	case XStringStyle:
	case XStdICCTextStyle:
	    encoding = XA_STRING;
	    cnv_func = CNV_STR_FUNC;
	    break;
	case XCompoundTextStyle:
	    encoding = XInternAtom(dpy, "COMPOUND_TEXT", False);
	    cnv_func = CNV_CTEXT_FUNC;
	    break;
	case XTextStyle:
	    encoding = XInternAtom(dpy, "TEXT", False);
	    cnv_func = CNV_TEXT_FUNC;
	    break;
	default:
	    return XConverterNotFound;
    }

    if ((buf = (unsigned char *) Xmalloc(buf_len)) == NULL)
	return XNoMemory;
retry:
    buf_ptr = buf;
    unconv_num = 0;
    for (i = 0; i < count && buf_len > 0; i++, list_ptr++) {
	tmp_len = buf_len;
	if ((*cnv_func)(lcd, *list_ptr, STRLEN_FUNC(*list_ptr), 
			buf_ptr, &tmp_len, &tmp_num) == -1)
	    continue;

	if (tmp_num > 0 && style == XStdICCTextStyle && encoding == XA_STRING) {
	    encoding = XInternAtom(dpy, "COMPOUND_TEXT", False);
	    cnv_func = CNV_CTEXT_FUNC;
	    list_ptr = list;
	    goto retry;
	}
	buf_ptr += tmp_len++;
	*buf_ptr++ = 0;
	buf_len -= tmp_len;
	unconv_num += tmp_num;
    }

    if ((nitems = buf_ptr - buf) <= 0)
	nitems = 1;
    if ((value = (unsigned char *) Xmalloc(nitems)) == NULL) {
	Xfree(buf);
	return XNoMemory;
    }
    if (nitems == 1)
	*value = 0;
    else
    	bcopy(buf, value, nitems);
    nitems--;
    Xfree(buf);

    text_prop->value = value;
    text_prop->encoding = encoding;
    text_prop->format = 8;
    text_prop->nitems = nitems;

    return unconv_num;
}
