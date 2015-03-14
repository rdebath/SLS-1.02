/* $XConsortium: XimpSJIS.c,v 1.4 92/04/14 13:29:59 rws Exp $ */
/******************************************************************

              Copyright 1991, 1992 Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Sony not be used
in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
Sony makes no representations about the suitability of this software
for any purpose.  It is provided "as is" without express or implied
warranty.
 
SONY DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SONY BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

  Author: Masaki Takeuchi       Sony Corporation

******************************************************************/

#include "Xlibint.h"
#include "Xlcint.h"
#include <ctype.h>

#include "Ximplc.h"

#ifdef USE_SJIS

static Bool sjis_initialize();
static void null_proc();
static char sjis_mbchar();
static int sjis_mbstocs(), sjis_wcstocs(), sjis_cstombs(), sjis_cstowcs();

XLCdXimpMethods sjis_lc_methods =
{
    sjis_initialize,
    null_proc,
    null_proc,
    sjis_mbchar,
    sjis_mbstocs,
    sjis_wcstocs,
    sjis_cstombs,
    sjis_cstowcs,
};

#define WC_MASK		0x8080
#define CS1_WC_MASK	0x8080
#define CS2_WC_MASK	0x0080
#define CS3_WC_MASK	0x8000

#ifndef iskanji
#define iskanji(x)	((0x81<=(x) && (x)<=0x9f) || (0xe0<=(x) && (x)<=0xfc))
#endif /* !iskanji */
#ifndef iskana
#define iskana(x)	(0xa1<=(x) && (x)<=0xdf)
#endif /* !iskana */

static void
null_proc(lcd)
    Ximp_XLCd lcd;
{
}

static Bool
sjis_initialize(lcd)
    Ximp_XLCd lcd;
{
    lcd->ximp_lcpart->mb_cur_max = 2;
    lcd->ximp_lcpart->state_dependent = False;

    return True;
}

static char
sjis_mbchar(lcd, str, lenp)
    Ximp_XLCd lcd;
    register char *str;
    register int *lenp;
{
    register unsigned char ch = (unsigned char) *str++;

    if (ch & 0x80 && iskanji(ch)) {
	*lenp = 2;
    } else
	*lenp = 1;

    return (char) ch;
}

static int
sjis_mbstocs(lcd, mbstr, mbstr_len, csbuf, csbuf_len, cs_number, char_length)
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
    register unsigned char ch, ch2;
    int buf_len;
    int codeset_number = 0;

    if (csbuf_len)
	buf_len = *csbuf_len;
    else
	buf_len = MAXINT;
    if (char_length)
	*char_length = 1;
    
    ch = *mbptr;
    if (ch & 0x80) {
	if (iskanji(ch)) {
    	    codeset_number = 1;
	    if (char_length)
		*char_length = 2;
	} else if (iskana(ch))
    	    codeset_number = 2;
    }

    while (mbstr_len > 0 && buf_len > 0) {
	ch = *mbptr;
	if (iskanji(ch)) {
	    if (codeset_number != 1 || mbstr_len < 2 || buf_len < 2)
		break;

	    ch -= (ch >= 0xa0) ? 0xc1 : 0x81;
	    mbptr++;
	    if ((ch2 = *mbptr++) >= 0x9f) {
		*bufptr++ = (ch << 1) + 0x22;
		*bufptr++ = ch2 - 0x7e;
	    } else {
		*bufptr++ = (ch << 1) + 0x21;
		*bufptr++ = ch2 - ((ch2 <= 0x7e) ? 0x1f : 0x20 );
	    }

	    mbstr_len -= 2;
	    buf_len -= 2;
	    continue;
	} else if (iskana(ch)) {
	    if (codeset_number != 2)
		break;
	} else
	    if (codeset_number != 0)
		break;
	
	*bufptr++ = ch;
	mbptr++;
	mbstr_len--;
	buf_len--;
    }

    if (csbuf_len)
	*csbuf_len = bufptr - csbuf;
    if (cs_number)
	*cs_number = codeset_number;

    return mbptr - mbstr;
}


static int
sjis_wcstocs(lcd, wcstr, wcstr_len, csbuf, csbuf_len, cs_number, char_length)
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
    int buf_len;
    int codeset_number = 0;
#ifdef sony_news
    unsigned char ch, ch2;
#endif

    if (csbuf_len)
	buf_len = *csbuf_len;
    else
	buf_len = MAXINT;
    if (char_length)
	*char_length = 1;

    wch = *wcptr;
#ifdef sony_news
    ch = (unsigned char) (wch >> 8);
    ch2 = (unsigned char) wch;
    if (ch) {
	codeset_number = 1;
	if (char_length)
	    *char_length = 2;
    } else if (iskana(ch2))
	codeset_number = 2;
    
    for ( ; wcstr_len > 0 && buf_len > 0; wcptr++, wcstr_len--) {
	wch = *wcptr;
	ch = (unsigned char) (wch >> 8);
	ch2 = (unsigned char) wch;
	if (ch) {
	    if (codeset_number != 1 || buf_len < 2)
		break;
	    ch -= (ch >= 0xa0) ? 0xc1 : 0x81;
	    if (ch2 >= 0x9f) {
		*bufptr++ = (ch << 1) + 0x22;
		*bufptr++ = ch2 - 0x7e;
	    } else {
		*bufptr++ = (ch << 1) + 0x21;
		*bufptr++ = ch2 - ((ch2 <= 0x7e) ? 0x1f : 0x20 );
	    }
	    buf_len -= 2;
	    continue;
	}
	if (iskana(ch2)) {
	    if (codeset_number != 2)
		break;
	} else
	    if (codeset_number != 0)
		break;
	
	*bufptr++ = ch2;
	buf_len--;
    }
#else
    wc_mask = wch & WC_MASK;
    if (wc_mask == CS1_WC_MASK) {
	codeset_number = 1;
	if (char_length)
	    *char_length = 2;
    } else if (wc_mask == CS2_WC_MASK)
	codeset_number = 2;

    for ( ; wcstr_len > 0 && buf_len > 0; wcptr++, wcstr_len--) {
	wch = *wcptr;
	wc_mask = wch & WC_MASK;
	if (wc_mask == CS1_WC_MASK) {
	    if (codeset_number != 1 || buf_len < 2)
		break;
	    *bufptr++ = (unsigned char) (wch >> 8);
	    *bufptr++ = (unsigned char) wch ;
	    buf_len -= 2;
	    continue;
	}
	if (wc_mask == CS2_WC_MASK) {
	    if (codeset_number != 2)
		break;
	} else
	    if (codeset_number != 0)
		break;

	*bufptr++ = (unsigned char) wch;
	buf_len--;
    }
#endif

    if (csbuf_len)
	*csbuf_len = bufptr - csbuf;
    if (cs_number)
	*cs_number = codeset_number;

    return wcptr - wcstr;
}


static int
sjis_cstombs(lcd, csstr, csstr_len, mbbuf, mbbuf_len, cs_number)
    Ximp_XLCd lcd;
    unsigned char *csstr;
    int csstr_len;
    unsigned char *mbbuf;
    int *mbbuf_len;
    int cs_number;
{
    register unsigned char *csptr = csstr;
    register unsigned char *bufptr = mbbuf;
    unsigned char high, low, ch1, ch2;
    register buf_len;

    if (mbbuf_len)
	buf_len = *mbbuf_len;
    else
	buf_len = MAXINT;
    if (csstr_len < buf_len)
	buf_len = csstr_len;

    if (cs_number == 1) {
	buf_len >>= 1;
	while (buf_len--) {
	    high = *csptr++ & 0x7f;
	    low = *csptr++ & 0x7f;
	    ch1 = ((int)(high - 0x21) >> 1) + 0x81;
	    if (ch1 > 0x9f)
		ch1 += 0x40;
	    if (high & 1) {
		ch2 = low + 0x1f;
		if (low > 0x5f)
		    ch2++;
	    } else
		ch2 = low + 0x7e;

	    *bufptr++ = ch1;
	    *bufptr++ = ch2;
	}
    } else if(cs_number == 2) {
	while (buf_len--)
	    *bufptr++ = *csptr++ | 0x80;
    } else
	while (buf_len--)
	    *bufptr++ = *csptr++ & 0x7f;

    if (mbbuf_len)
	*mbbuf_len = bufptr - mbbuf;

    return csptr - csstr;
}


static int
sjis_cstowcs(lcd, csstr, csstr_len, wcbuf, wcbuf_len, cs_number)
    Ximp_XLCd lcd;
    unsigned char *csstr;
    int csstr_len;
    wchar_t *wcbuf;
    int *wcbuf_len;
    int cs_number;
{
    register unsigned char *csptr = csstr;
    wchar_t *bufptr = wcbuf;
    register wchar_t wch;
    register buf_len;
#ifdef sony_news
    unsigned char high, low, ch1, ch2;
#endif

    if (wcbuf_len)
	buf_len = *wcbuf_len;
    else
	buf_len = MAXINT;

#ifdef sony_news
    if (cs_number == 1) {
	csstr_len >>= 1;
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	
	while (buf_len--) {
	    high = *csptr++ & 0x7f;
	    low = *csptr++ & 0x7f;
	    ch1 = ((int)(high - 0x21) >> 1) + 0x81;
	    if (ch1 > 0x9f)
		ch1 += 0x40;
	    if (high & 1) {
		ch2 = low + 0x1f;
		if (low > 0x5f)
		    ch2++;
	    } else
		ch2 = low + 0x7e;
	    *bufptr++ = (ch1 << 8) | ch2;
	}
    } else if (cs_number == 2) {
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	while (buf_len--)
	    *bufptr++ = (wchar_t) (*csptr++ | 0x80);
    } else {
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	while (buf_len--)
	    *bufptr++ = (wchar_t) (*csptr++ & 0x7f);
    }
#else
    if (cs_number == 1) {
	csstr_len >>= 1;
	if (csstr_len < buf_len)
	    buf_len = csstr_len;

	while (buf_len--) {
	    wch = *csptr++;
	    wch = (wch << 8) | *csptr++;
	    *bufptr++ = wch | CS1_WC_MASK;
	}
    } else if (cs_number == 2) {
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	while (buf_len--)
	    *bufptr++ = (wchar_t) (*csptr++ | CS2_WC_MASK);
    } else {
	if (csstr_len < buf_len)
	    buf_len = csstr_len;
	while (buf_len--)
	    *bufptr++ = (wchar_t) (*csptr++ & 0x7f);
    }
#endif

    if (wcbuf_len)
	*wcbuf_len = bufptr - wcbuf;

    return csptr - csstr;
}

#endif	/* USE_SJIS */
