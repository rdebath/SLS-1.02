/* $XConsortium: XimpCT.c,v 1.4 92/04/14 13:28:38 rws Exp $ */
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

_get_codeset_number(lcd, lindex, msb_mask)
    Ximp_XLCd lcd;
    int lindex;
    unsigned char msb_mask;
{
    int i, codeset_num, index_num;
    register CodeSetRec *codeset;
    register EncodingIndexRec *index_ptr;

    codeset_num = lcd->ximp_lcpart->codeset_num;
    codeset = lcd->ximp_lcpart->codeset;

    for (i = 0; i < codeset_num; i++, codeset++) {
	index_ptr = codeset->encoding_index;
	index_num = codeset->index_num;

	while (index_num--) {
	    if (index_ptr->lindex == lindex && index_ptr->msb_mask == msb_mask)
		return i;
	    index_ptr++;
	}
    }

    return -1;
}

EncodingRec *
_get_encoding_rec(lindex)
    int lindex;
{
    EncodingRec *encoding_ptr, **table_ptr;

    table_ptr = encoding_table;

    while (encoding_ptr = *table_ptr++)
	if (encoding_ptr->lindex == lindex)
	    return encoding_ptr;
    
    return NULL;
}

static int
get_encoding_index(encoding, msb_mask, char_length, gc_size)
    unsigned char *encoding;
    unsigned char msb_mask;
    int char_length;
    int gc_size;
{
    EncodingRec *encoding_ptr, **table_ptr;

    table_ptr = encoding_table;

    if (msb_mask) {
	while (encoding_ptr = *table_ptr++)
	    if (encoding_ptr->GR_encoding) {
	    	if (encoding_ptr->char_length == char_length &&
		    !strcmp(encoding_ptr->GR_encoding, (char *)encoding) &&
		    encoding_ptr->GR_gc_size == gc_size)

		    return encoding_ptr->lindex;
	    }
    } else {
	while (encoding_ptr = *table_ptr++)
	    if (encoding_ptr->GL_encoding) {
	    	if (encoding_ptr->char_length == char_length &&
		    !strcmp(encoding_ptr->GL_encoding, (char *)encoding) &&
		    encoding_ptr->GL_gc_size == gc_size)

		    return encoding_ptr->lindex;
	    }
    }

    return -1;
}


static int
check_ESC_IF_char(encoding, len)
unsigned char *encoding;
register len;
{
    register unsigned char ch, *strptr = encoding;

    if (len < 1)
	return -1;

    ch = *strptr++;
    if (ch >= 0x30 && ch < 0x7f)
	return 1;

    while (--len > 0) {
	if (ch < 0x20 || ch > 0x2f)
	    break;
	ch = *strptr++;
    }

    if (len == 0 || ch < 0x30 || ch > 0x7e)
	return -1;

    return strptr - encoding;
}

int
_check_ESC_sequence(lcd, ctext, ctext_len, GL_codeset, GR_codeset)
    Ximp_XLCd lcd;
    unsigned char *ctext;
    int ctext_len;
    int *GL_codeset, *GR_codeset;
{
    unsigned char ch, msb_mask, *ctptr = ctext;
    unsigned char encoding[BUFSIZE];
    int gc_size, char_length, lindex, encoding_len;
    int buf_len, tmp_len, skip_size;

    if (--ctext_len < 1 || *ctptr++ != 0x1b)
	return -1;
    tmp_len = ctext_len;

    msb_mask = GL;
    char_length = 1;
    gc_size = 94;

    ctext_len--;
    switch (ch = *ctptr++) {
	case '-': gc_size = 96;
	case ')': msb_mask = GR;
	case '(':
	    break;
	case '$':
	    if (ctext_len-- < 1)
		return -1;

	    switch (ch = *ctptr++) {
		case ')': msb_mask = GR;
		case '(':
    		    char_length = 2;
		    break;
		default:
		    goto unknown;
	    }
	    break;
	case '%':
	    if (ctext_len < 1)
		return -1;

	    if (ctext_len > 4 && *ctptr++ == '/') {
		ch = *ctptr++;
		if (ch < 0x30 || ch > 0x3f)
		    goto unknown;
		ch = *ctptr++;
		skip_size = (ch & 0x7f) * 128 + (*ctptr++ & 0x7f);
		ctext_len -= 4;
		if (ctext_len < skip_size)
		    return -1;
		ctptr += skip_size;
		return ctptr - ctext;
	    }
	    goto unknown;
	default:
unknown:
	    ctptr = ctext + 1;
	    ctext_len = tmp_len;
	    msb_mask = char_length = gc_size = 0;
	    break;

    }

    encoding_len = check_ESC_IF_char(ctptr, ctext_len);
    if (encoding_len < 0)
	return -1;

    strncpy((char *)encoding, (char *)ctptr, encoding_len);
    encoding[encoding_len] = 0;
    ctptr += encoding_len;

    if (char_length == 2) {
	ch = encoding[encoding_len - 1];
	if (ch >= 0x60 && ch <=0x6f)
	    char_length = 3;
	else if (ch >= 0x70 && ch <=0x7f)
	    char_length = 4;
    }

    lindex = get_encoding_index(encoding, msb_mask, char_length, gc_size);
    if (lindex >= 0) {
	if (msb_mask)
	    *GR_codeset = _get_codeset_number(lcd, lindex, GR);
	else
	    *GL_codeset = _get_codeset_number(lcd, lindex, GL);
    }
    
    return ctptr - ctext;
}

int
_check_CSI_sequence(lcd, ctext, ctext_len)
    Ximp_XLCd lcd;
    unsigned char *ctext;
    int ctext_len;
{
    unsigned char ch, *ctptr = ctext;

    if (--ctext_len < 1 || *ctptr++ != 0x9b)
	return -1;

    ch = *ctptr++;
    while (ch >= 0x30 && ch < 0x40 && ctext_len > 0) {
	ctext_len--;
	ch = *ctptr++;
    }

    while (ch >= 0x20 && ch < 0x30 && ctext_len > 0) {
	ctext_len--;
	ch = *ctptr++;
    }

    if (ch < 0x40 || ch > 0x7e || ctext_len <= 0)
	return -1;

    return ctptr - ctext;
}


int
_Ximp_cstostring(lcd, csstr, csstr_len, string, string_len, cs_num)
    Ximp_XLCd lcd;
    unsigned char *csstr;
    int csstr_len;
    unsigned char *string;
    int *string_len;
    int cs_num;
{
    unsigned char *csptr = csstr;
    unsigned char *string_ptr = string;
    unsigned char ch;
    CodeSetRec *codeset;
    EncodingIndexRec *encoding_index;
    int str_len, lindex, index_num;

    if (string_len)
	str_len = *string_len;
    else
	str_len = MAXINT;

    codeset = lcd->ximp_lcpart->codeset + cs_num;
    encoding_index = codeset->encoding_index;
    index_num = codeset->index_num;


    while (index_num--) {
	lindex = encoding_index->lindex;
	if (encoding_index->msb_mask == GL &&
		lindex >= ISO8859_1 && lindex <= JISX0201_1976_0)
	    break;

	encoding_index++;
    }
    if (index_num < 0)
	return -1;

    while (csstr_len > 0 && str_len > 0) {
	ch = *csptr++ & 0x7f;
	if (ch < 0x20 || ch > 0x7e)
	    if (ch != 0x09 && ch != 0x0a && ch != 0x0b)
		return -1;
	*string_ptr++ = ch;
	csstr_len--;
	str_len--;
    }

    if (string_len)
	*string_len = string_ptr - string;
    
    return csptr - csstr;
}


int
_Ximp_cstoct(lcd, csstr, csstr_len, ctext, ctext_len, cs_num)
    Ximp_XLCd lcd;
    unsigned char *csstr;
    int csstr_len;
    unsigned char *ctext;
    int *ctext_len;
    int cs_num;
{
    unsigned char encoding[BUFSIZE], *encoding_ptr;
    unsigned char *csptr = csstr;
    unsigned char *ctptr = ctext;
    unsigned char ch, msb_mask, min_ch, max_ch;
    EncodingRec *encoding_rec;
    CodeSetRec *codeset;
    int char_length, gc_size;
    int ct_len, tmp_len;

    if (ctext_len)
	ct_len = *ctext_len;
    else
	ct_len = MAXINT;

    codeset = lcd->ximp_lcpart->codeset + cs_num;

    if (codeset->index_num <= 0)
	return -1;

    encoding_rec = _get_encoding_rec(codeset->encoding_index[0].lindex);
    if (encoding_rec == NULL)
	return -1;

    msb_mask = codeset->encoding_index[0].msb_mask;
    char_length = encoding_rec->char_length;
    gc_size = (msb_mask == GR) ? 
		encoding_rec->GR_gc_size : encoding_rec->GL_gc_size;

    encoding_ptr = encoding;
    *encoding_ptr++ = '\033';
    if (char_length > 1)
	*encoding_ptr++ = '$';
    if (gc_size == 94) {
	if (msb_mask == GL)
	    *encoding_ptr++ = '(';
	else if (msb_mask == GR)
	    *encoding_ptr++ = ')';
    } else if (gc_size == 96) {
	if (msb_mask == GR)
	    *encoding_ptr++ = '-';
    }
    strcpy((char *)encoding_ptr, (msb_mask == GR) ? 
		encoding_rec->GR_encoding : encoding_rec->GL_encoding);
    tmp_len = strlen((char *)encoding);
    if ((ct_len -= tmp_len) < 0)
	return -1;
    strcpy((char *)ctptr, (char *)encoding);
    ctptr += tmp_len;

    min_ch = 0x20;
    max_ch = 0x7f;

    if (gc_size == 94) {
	max_ch--;
	if (char_length > 1 || msb_mask == GR)
	    min_ch++;
    }

    while (csstr_len > 0 && ct_len > 0) {
	ch = *csptr++ & 0x7f;
	if (ch < min_ch || ch > max_ch)
	    if (ch != 0x09 && ch != 0x0a && ch != 0x0b)
		return -1;
	*ctptr++ = ch | msb_mask;
	csstr_len--;
	ct_len--;
    }

    if (ctext_len)
	*ctext_len = ctptr - ctext;
    
    return csptr - csstr;
}
