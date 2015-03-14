/* $XConsortium: XimpWCT.c,v 1.5 92/04/14 13:30:14 rws Exp $ */
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

extern int _Ximp_cstostring();
extern int _Ximp_cstoct();


int
Ximp_wcstostring(wcstr, wcstr_len, string, string_len, unconv_num)
    wchar_t *wcstr;
    int wcstr_len;
    unsigned char *string;
    int *string_len;
    int *unconv_num;
{
    Ximp_XLCd lcd = (Ximp_XLCd) _XlcCurrentLC();

    if (lcd == NULL)
	return -1;

    return _Ximp_wstrtostr(lcd, lcd->ximp_lcpart->methods->wcstocs, wcstr,
                           wcstr_len, _Ximp_cstostring, string, string_len,
                           unconv_num);
}

int
_Ximp_wcstostring(lcd, wcstr, wcstr_len, string, string_len, unconv_num)
    Ximp_XLCd lcd;
    wchar_t *wcstr;
    int wcstr_len;
    unsigned char *string;
    int *string_len;
    int *unconv_num;
{
    return _Ximp_wstrtostr(lcd, lcd->ximp_lcpart->methods->wcstocs, wcstr,
                           wcstr_len, _Ximp_cstostring, string, string_len,
                           unconv_num);
}


int
Ximp_wcstoct(wcstr, wcstr_len, ctext, ctext_len, unconv_num)
    wchar_t *wcstr;
    int wcstr_len;
    unsigned char *ctext;
    int *ctext_len;
    int *unconv_num;
{
    Ximp_XLCd lcd = (Ximp_XLCd) _XlcCurrentLC();

    if (lcd == NULL)
	return -1;

    return _Ximp_wstrtostr(lcd, lcd->ximp_lcpart->methods->wcstocs, wcstr,
			   wcstr_len, _Ximp_cstoct, ctext, ctext_len,
			   unconv_num);
}

int
_Ximp_wcstoct(lcd, wcstr, wcstr_len, ctext, ctext_len, unconv_num)
    Ximp_XLCd lcd;
    wchar_t *wcstr;
    int wcstr_len;
    unsigned char *ctext;
    int *ctext_len;
    int *unconv_num;
{
    return _Ximp_wstrtostr(lcd, lcd->ximp_lcpart->methods->wcstocs, wcstr,
			   wcstr_len, _Ximp_cstoct, ctext, ctext_len,
			   unconv_num);
}


int
Ximp_cttowcs(ctext, ctext_len, wcstr, wcstr_len, unconv_num)
    unsigned char *ctext;
    int ctext_len;
    wchar_t *wcstr;
    int *wcstr_len;
    int *unconv_num;
{
    Ximp_XLCd lcd = (Ximp_XLCd) _XlcCurrentLC();

    if (lcd == NULL)
	return -1;

    return _Ximp_cttowcs(lcd, ctext, ctext_len, wcstr, wcstr_len, unconv_num);
}

int
_Ximp_cttowcs(lcd, ctext, ctext_len, wcstr, wcstr_len, unconv_num)
    Ximp_XLCd lcd;
    unsigned char *ctext;
    int ctext_len;
    wchar_t *wcstr;
    int *wcstr_len;
    int *unconv_num;
{
    unsigned char ch, *ctptr = ctext;
    wchar_t *bufptr = wcstr;
    unsigned char *tmpptr;
    unsigned char msb_mask;
    int GL_codeset, GR_codeset, codeset_number;
    int buf_len, tmp_len, skip_size;
    int ret = -1;
    int (*cstowcs)();

    if (wcstr_len)
	buf_len = *wcstr_len;
    else
	buf_len = MAXINT;
    if (unconv_num)
	*unconv_num = 0;
    
    cstowcs = lcd->ximp_lcpart->methods->cstowcs;
    GL_codeset = _get_codeset_number(lcd, ISO8859_1, GL);
    GR_codeset = _get_codeset_number(lcd, ISO8859_1, GR);

    (*lcd->ximp_lcpart->methods->cnv_start)(lcd);

    while (ctext_len > 0 && buf_len > 0) {
	ch = *ctptr;
	if (ch == 0x1b) {
	    tmp_len = _check_ESC_sequence(lcd, ctptr, ctext_len, 
					  &GL_codeset, &GR_codeset);
	} else if (ch == 0x9b) {
	    tmp_len =_check_CSI_sequence(lcd, ctptr, ctext_len);
	} else {
	    tmpptr = ctptr;
	    msb_mask = ch & 0x80;
	    for ( ; ctext_len; tmpptr++, ctext_len--) {
		ch = *tmpptr;
		if (msb_mask != (ch & 0x80) || ch == '\033' || ch == 0x9b)
		    break;
	        if ((ch < 0x20 && ch != '\n' && ch != '\t') ||
			(ch >= 0x80 && ch < 0xa0))
		    goto error;
	    }

	    codeset_number = msb_mask ? GR_codeset : GL_codeset;
	    if (codeset_number > -1) {
		tmp_len = buf_len;
		skip_size = (*cstowcs)(lcd, ctptr, tmpptr - ctptr,
				       bufptr, &tmp_len, codeset_number);
		if (skip_size < 0 || skip_size != tmpptr - ctptr)
			goto error;

		bufptr += tmp_len;
		buf_len -= tmp_len;
	    } else if (unconv_num)
		*unconv_num += tmpptr - ctptr;
	    ctptr = tmpptr;
	    continue;
	}
	if (tmp_len < 0)
	    goto error;
	ctptr += tmp_len;
	ctext_len -= tmp_len;
    }
    if (wcstr_len)
	*wcstr_len = bufptr - wcstr;
    ret = ctptr - ctext;
error:
    (*lcd->ximp_lcpart->methods->cnv_end)(lcd);

    return ret;
}

int
_Ximp_ct_wcslen(lcd, ctext, ctext_len, unconv_num)
    Ximp_XLCd lcd;
    unsigned char *ctext;
    int ctext_len;
    int *unconv_num;
{
    unsigned char ch, *ctptr = ctext;
    unsigned char *tmpptr;
    unsigned char msb_mask;
    wchar_t buf[BUFSIZE];
    int GL_codeset, GR_codeset, codeset_number;
    int tmp_len, skip_size;
    int ret = 0;
    int (*cstowcs)();

    if (unconv_num)
	*unconv_num = 0;
    
    cstowcs = lcd->ximp_lcpart->methods->cstowcs;
    GL_codeset = _get_codeset_number(lcd, ISO8859_1, GL);
    GR_codeset = _get_codeset_number(lcd, ISO8859_1, GR);

    (*lcd->ximp_lcpart->methods->cnv_start)(lcd);

    while (ctext_len > 0) {
	ch = *ctptr;
	if (ch == 0x1b) {
	    tmp_len = _check_ESC_sequence(lcd, ctptr, ctext_len, 
					  &GL_codeset, &GR_codeset);
	} else if (ch == 0x9b) {
	    tmp_len =_check_CSI_sequence(lcd, ctptr, ctext_len);
	} else {
	    tmpptr = ctptr;
	    msb_mask = ch & 0x80;
	    for ( ; ctext_len; tmpptr++, ctext_len--) {
		ch = *tmpptr;
		if (msb_mask != (ch & 0x80) || ch == '\033' || ch == 0x9b)
		    break;
	        if ((ch < 0x20 && ch != '\n' && ch != '\t') ||
			(ch >= 0x80 && ch < 0xa0)) {
		    ret = -1;
		    goto error;
		}
	    }

	    codeset_number = msb_mask ? GR_codeset : GL_codeset;
	    if (codeset_number > -1) {
		tmp_len = BUFSIZE;
		skip_size = (*cstowcs)(lcd, ctptr, tmpptr - ctptr,
				       buf, &tmp_len, codeset_number);
		if (skip_size < 0) {
		    ret = -1;
		    goto error;
		}
		ret += tmp_len;
	    } else if (unconv_num)
		*unconv_num += tmpptr - ctptr;
	    ctptr = tmpptr;
	    continue;
	}
	if (tmp_len < 0) {
	    ret = -1;
	    goto error;
	}
	ctptr += tmp_len;
	ctext_len -= tmp_len;
    }
error:
    (*lcd->ximp_lcpart->methods->cnv_end)(lcd);

    return ret;
}
