/* $XConsortium: XimpWPrTxt.c,v 1.3 92/04/14 13:30:30 rws Exp $ */
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

#include "Xlibint.h"
#include "Xlcint.h"
#include "Ximplc.h"
#include <X11/Xutil.h>
#include <X11/Xatom.h>

extern int _Ximp_mbstowcs(), _Ximp_cttowcs();
extern wchar_t *_Xwcscpy();

int
XwcTextPropertyToTextList(dpy, text_prop, list_ret, count_ret)
    Display *dpy;
    XTextProperty *text_prop;
    wchar_t ***list_ret;
    int *count_ret;
{
    wchar_t **list, *wstr_ptr;
    wchar_t *buf, *buf_ptr;
    unsigned char *str_ptr;
    int i, count, unconv_num, tmp_len, buf_len;
#define CNV_STR_FUNC	_Ximp_mbstowcs
#define CNV_CTEXT_FUNC	_Ximp_cttowcs
#define CNV_TEXT_FUNC	_Ximp_mbstowcs

    /* XXX */
    buf_len = text_prop->nitems + 1;
    buf_len = (buf_len / BUFSIZE + 1) * BUFSIZE;
    if ((buf = (wchar_t *) Xmalloc(buf_len * sizeof(wchar_t))) == NULL)
	return XNoMemory;
    /* XXX */

#include "XimpPrTxt.c"

    if ((list = (wchar_t **) Xmalloc(count * sizeof(wchar_t *))) == NULL)
	goto no_mem;
    wstr_ptr = (wchar_t *) Xmalloc((buf_ptr - buf) * sizeof(wchar_t));
    if (wstr_ptr == NULL) {
no_mem:
	Xfree(buf);
	if (list)
	    Xfree(list);
	return XNoMemory;
    }
    
    buf_ptr = buf;
    for (i = 0; i < count; i++) {
	list[i] = wstr_ptr;
	_Xwcscpy(wstr_ptr, buf_ptr);
	tmp_len = _Xwcslen(wstr_ptr) + 1;
	wstr_ptr += tmp_len;
	buf_ptr += tmp_len;
    }
    Xfree(buf);

    *list_ret = list;
    *count_ret = count;

    return unconv_num;
}

void XwcFreeStringList(list)
    wchar_t **list;
{
    if (list) {
        if (*list)
	     Xfree(*list);
        Xfree(list);
    }
}

