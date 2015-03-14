/* $XConsortium: XimpEUC.c,v 1.4 92/04/14 13:28:56 rws Exp $ */
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
#include <ctype.h>

#include "Ximplc.h"

static Bool euc_initialize();
static void euc_cnv_start(), euc_cnv_end();
static char euc_mbchar();
static int euc_mbstocs(), euc_wcstocs(), euc_cstombs(), euc_cstowcs();

typedef struct {
    int max_char_length;
} EUCExtRec;

XLCdXimpMethods euc_lc_methods =
{
    euc_initialize,
    euc_cnv_start,
    euc_cnv_end,
    euc_mbchar,
    euc_mbstocs,
    euc_wcstocs,
    euc_cstombs,
    euc_cstowcs,
};

#ifndef SS2
#define SS2		0x8e
#define SS3		0x8f
#endif

#define WC_MASK		0x8080
#define CS1_WC_MASK	0x8080
#define CS2_WC_MASK	0x0080
#define CS3_WC_MASK	0x8000
#define SHIFT_BITS	8
#define CODE_MASK	0x7f7f

#define WC_MASK_L	0x30000000
#define CS1_WC_MASK_L	0x30000000
#define CS2_WC_MASK_L	0x10000000
#define CS3_WC_MASK_L	0x20000000
#define SHIFT_BITS_L	7
#define CODE_MASK_L	0x0fffffff

static int CS1_length, CS2_length, CS3_length, Shift_bits;
static wchar_t WC_mask, CS1_WC_mask, CS2_WC_mask, CS3_WC_mask, Code_mask;

static Bool
euc_initialize(lcd)
    Ximp_XLCd lcd;
{
    register int codeset_num;
    register CodeSetRec *codeset;
    EUCExtRec *extension;
    int mb_cur_max, max_char_length;

    mb_cur_max = max_char_length = 1;
    codeset = lcd->ximp_lcpart->codeset;
    if ((codeset_num = lcd->ximp_lcpart->codeset_num) > 4)
	codeset_num = lcd->ximp_lcpart->codeset_num = 4;

    switch (codeset_num) {
	case 4:
	    mb_cur_max = max(mb_cur_max, codeset[3].char_length + 1);
	    max_char_length = max(max_char_length, codeset[3].char_length);
	case 3:
	    mb_cur_max = max(mb_cur_max, codeset[2].char_length + 1);
	    max_char_length = max(max_char_length, codeset[2].char_length);
	case 2:
	    mb_cur_max = max(mb_cur_max, codeset[1].char_length);
	    max_char_length = max(max_char_length, codeset[1].char_length);
    }

    lcd->ximp_lcpart->mb_cur_max = mb_cur_max;
    lcd->ximp_lcpart->state_dependent = False;

    if ((extension = (EUCExtRec *) Xmalloc(sizeof(EUCExtRec))) == NULL)
	return False;
    extension->max_char_length = max_char_length;
    lcd->ximp_lcpart->extension = (XPointer) extension;

    return True;
}

static void
euc_cnv_start(lcd)
    Ximp_XLCd lcd;
{
    register int codeset_num;
    register CodeSetRec *codeset;
    static Ximp_XLCd last_lcd = NULL;
    int max_char_length;

    if (lcd == last_lcd)
	return;

    codeset = lcd->ximp_lcpart->codeset;
    codeset_num = lcd->ximp_lcpart->codeset_num;

#ifdef FIX_EUC32
    WC_mask = WC_MASK_L;
    CS1_WC_mask = CS1_WC_MASK_L;
    CS2_WC_mask = CS2_WC_MASK_L;
    CS3_WC_mask = CS3_WC_MASK_L;
    Shift_bits = SHIFT_BITS_L;
    Code_mask = CODE_MASK_L;
#else
    max_char_length = 
		((EUCExtRec *) lcd->ximp_lcpart->extension)->max_char_length;
    if (max_char_length < 3) {
	WC_mask = WC_MASK;
	CS1_WC_mask = CS1_WC_MASK;
	CS2_WC_mask = CS2_WC_MASK;
	CS3_WC_mask = CS3_WC_MASK;
	Shift_bits = SHIFT_BITS;
	Code_mask = CODE_MASK;
    } else {
	WC_mask = WC_MASK_L;
	CS1_WC_mask = CS1_WC_MASK_L;
	CS2_WC_mask = CS2_WC_MASK_L;
	CS3_WC_mask = CS3_WC_MASK_L;
	Shift_bits = SHIFT_BITS_L;
	Code_mask = CODE_MASK_L;
    }
#endif

    CS1_length = CS2_length = CS3_length = 0;

    switch (codeset_num) {
	case 4:
	    CS3_length = codeset[3].char_length;
	case 3:
	    CS2_length = codeset[2].char_length;
	case 2:
	    CS1_length = codeset[1].char_length;
    }

    last_lcd = lcd;
}

static void
euc_cnv_end(lcd)
    Ximp_XLCd lcd;
{
}

static char
euc_mbchar(lcd, str, lenp)
    Ximp_XLCd lcd;
    register char *str;
    register int *lenp;
{
    register unsigned char ch = (unsigned char) *str++;

    if ((ch & 0x80) == NULL) {
cset0:
	*lenp = 1;
	return (char) ch;
    }

    if (ch == SS2) {
	if (CS2_length == 0)
	    goto cset0;
	*lenp = CS2_length + 1;
	return *str;
    }

    if (ch == SS3) {
	if (CS3_length == 0)
	    goto cset0;
	*lenp = CS3_length + 1;
	return *str;
    }

    if (CS1_length == 0)
	goto cset0;
    *lenp = CS1_length;
    return (char) ch;
}


static int
euc_mbstocs(lcd, mbstr, mbstr_len, csbuf, csbuf_len, cs_number, char_length)
    Ximp_XLCd lcd;
    unsigned char *mbstr;
    int mbstr_len;
    unsigned char *csbuf;
    int *csbuf_len;
    int *cs_number;
    int *char_length;
{
    register unsigned char *mbptr = mbstr;
    register unsigned char *bufptr = csbuf;
    register unsigned char ch;
    unsigned char *tmpptr;
    int buf_len;
    register length;
    int codeset_number = -1, tmp_number;

    if (csbuf_len)
	buf_len = *csbuf_len;
    else
	buf_len = MAXINT;

    while (mbstr_len > 0 && buf_len > 0) {
	ch = *mbptr;
	if ((ch & 0x80) == NULL) {
cset0:
	    if (codeset_number) {
		if (codeset_number != -1)
		    break;
		codeset_number = 0;
		if (char_length)
		    *char_length = 1;
	    }
		
	    *bufptr++ = ch;
	    mbptr++;
	    mbstr_len--;
	    buf_len--;
	    continue;
	}

	tmpptr = mbptr;

	if (ch == SS2 || ch == SS3) {
	    if (ch == SS2) {
		length = CS2_length;
		tmp_number = 2;
	    } else {
		length = CS3_length;
		tmp_number = 3;
	    }
	    if (length < 1)
		goto cset0;

	    if (mbstr_len < length + 1 || buf_len < length)
		break;
	    ++mbptr;
	    mbstr_len--;
	} else {
	    if ((length = CS1_length) < 1)
		goto cset0;

	    if (mbstr_len < length || buf_len < length)
		break;
	    tmp_number = 1;
	}

	if (codeset_number != tmp_number) {
	    if (codeset_number != -1) {
		mbptr = tmpptr;
		break;
	    }
	    codeset_number = tmp_number;
	    if (char_length)
		*char_length = length;
	}

	mbstr_len -= length;
	buf_len -= length;

	while (length--)
	    *bufptr++ = *mbptr++;

    }

    if (csbuf_len)
	*csbuf_len = bufptr - csbuf;
    if (cs_number)
	*cs_number = codeset_number;

    return mbptr - mbstr;
}


static int
euc_wcstocs(lcd, wcstr, wcstr_len, csbuf, csbuf_len, cs_number, char_length)
    Ximp_XLCd lcd;
    wchar_t *wcstr;
    int wcstr_len;
    unsigned char *csbuf;
    int *csbuf_len;
    int *cs_number;
    int *char_length;
{
    wchar_t *wcptr = wcstr, wc_mask;
    register unsigned char *bufptr = csbuf;
    register wchar_t wch;
    unsigned char *tmpptr;
    int buf_len;
    register length;
    int codeset_number = -1, tmp_number;

    if (csbuf_len)
	buf_len = *csbuf_len;
    else
	buf_len = MAXINT;

    for ( ; wcstr_len > 0 && buf_len > 0; wcptr++, wcstr_len--) {
	wch = *wcptr;
	wc_mask = wch & WC_mask;
	if (wc_mask == NULL) {
cset0:
	    if (codeset_number) {
		if (codeset_number != -1)
		    break;
		codeset_number = 0;
		if (char_length)
		    *char_length = 1;
	    }
	    *bufptr++ = (unsigned char) wch;
	    buf_len--;
	    continue;
	} else if (wc_mask == CS1_WC_mask) {
	    length = CS1_length;
	    tmp_number = 1;
	} else if (wc_mask == CS2_WC_mask) {
	    length = CS2_length;
	    tmp_number = 2;
	} else {
	    length = CS3_length;
	    tmp_number = 3;
	}

	if (length < 1)
	    goto cset0;
	if (buf_len < length)
	    break;
	if (codeset_number != tmp_number) {
	    if (codeset_number != -1)
		break;
	    codeset_number = tmp_number;
	    if (char_length)
		*char_length = length;
	}
	buf_len -= length;
	bufptr += length;

	tmpptr = bufptr - 1;
	while (length--) {
	    *tmpptr-- = (unsigned char) (wch | 0x80);
	    wch >>= Shift_bits;
	}
    }

    if (csbuf_len)
	*csbuf_len = bufptr - csbuf;
    if (cs_number)
	*cs_number = codeset_number;

    return wcptr - wcstr;
}


static int
euc_cstombs(lcd, csstr, csstr_len, mbbuf, mbbuf_len, cs_number)
    Ximp_XLCd lcd;
    unsigned char *csstr;
    int csstr_len;
    unsigned char *mbbuf;
    int *mbbuf_len;
    int cs_number;
{
    register unsigned char *csptr = csstr;
    register unsigned char *bufptr = mbbuf;
    register buf_len;
    int length;
    unsigned char ss;
    int num;


    if (mbbuf_len)
	buf_len = *mbbuf_len;
    else
	buf_len = MAXINT;

    if (cs_number == 0) {
cset0:
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	while (buf_len--)
	    *bufptr++ = *csptr++ & 0x7f;
    } else if (cs_number == 1) {
	if (CS1_length < 1)
	    goto cset0;
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	buf_len -= buf_len % CS1_length;
	while (buf_len--)
	    *bufptr++ = *csptr++ | 0x80;
    } else {
	if (cs_number == 2) {
	    length = CS2_length;
	    ss = SS2;
	} else {
	    length = CS3_length;
	    ss = SS3;
	}
	csstr_len /= length;
	buf_len /= (length + 1);
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	while (buf_len--) {
	    *bufptr++ = ss;
	    num = length;
	    while (num--)
		*bufptr++ = *csptr++ | 0x80;
	}
    }

    if (mbbuf_len)
	*mbbuf_len = bufptr - mbbuf;

    return csptr - csstr;
}


static int
euc_cstowcs(lcd, csstr, csstr_len, wcbuf, wcbuf_len, cs_number)
    Ximp_XLCd lcd;
    unsigned char *csstr;
    int csstr_len;
    wchar_t *wcbuf;
    int *wcbuf_len;
    int cs_number;
{
    register unsigned char *csptr = csstr;
    wchar_t *bufptr = wcbuf, wc_mask;
    register wchar_t wch;
    register buf_len;
    int length;
    int num;

    if (wcbuf_len)
	buf_len = *wcbuf_len;
    else
	buf_len = MAXINT;

    if (cs_number == 0) {
cset0:
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	while (buf_len--)
	    *bufptr++ = (wchar_t) (*csptr++ & 0x7f);
    } else {
	if (cs_number == 1) {
	    length = CS1_length;
	    wc_mask = CS1_WC_mask;
	} else if (cs_number == 2) {
	    length = CS2_length;
	    wc_mask = CS2_WC_mask;
	} else {
	    length = CS3_length;
	    wc_mask = CS3_WC_mask;
	}
        if (length < 1)
	    goto cset0;

	csstr_len /= length;
	if (csstr_len < buf_len)
	    buf_len = csstr_len;

	while (buf_len--) {
	    wch = *csptr++;
	    num = length - 1;
	    while (num--)
		wch = (wch << Shift_bits) | *csptr++;

	    *bufptr++ = (wch & Code_mask) | wc_mask;
	}
    }

    if (wcbuf_len)
	*wcbuf_len = bufptr - wcbuf;

    return csptr - csstr;
}
