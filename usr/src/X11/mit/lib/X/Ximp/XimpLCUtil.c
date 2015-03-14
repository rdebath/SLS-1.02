/* $XConsortium: XimpLCUtil.c,v 1.7 92/04/14 13:29:21 rws Exp $ */
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
#include <X11/Xos.h>
#include "Ximplc.h"
#include <stdio.h>
#include <ctype.h>

#ifdef X_NOT_STDC_ENV
extern char *getenv();
#endif

enum {
    CODESET,
    ENCODING,
    EXT_FONT,
    FONT,
    GL_ENCODING,
    GR_ENCODING,
    LENGTH,
    NAME,
    STRING
} keyword_value;

typedef struct KeywordRec {
    char *name;
    int cmp_len;
    int value;
} KeywordRec;

static KeywordRec keyword_tbl[] = {
    { "CODESET", 7, CODESET },
    { "ENCODING", 8, ENCODING },
    { "EXT_FONT", 8, EXT_FONT },
    { "FONT", 4, FONT },
    { "GL", 2, GL_ENCODING },
    { "GR", 2, GR_ENCODING },
    { "LENGTH", 6, LENGTH },
    { "NAME", 4, NAME },
    0,
};

static int
get_token(buf, len)
    char *buf;
    int len;
{
    KeywordRec *keyword = keyword_tbl;

    while (keyword->name) {
	if (keyword->cmp_len <= len)
	    if (_Ximp_NCompareISOLatin1(buf,keyword->name,keyword->cmp_len)==0)
	    	return keyword->value;

	keyword++;
    }
    return STRING;
}

static char *
get_word(str, len)
    register char *str;
    int *len;
{
    register char ch, *strptr;

    ch = *str;
    while (ch == ' ' || ch == '\t' || ch == '\n')
	ch = *(++str);

    strptr = str;
    ch = *strptr;
    while (ch != ' ' && ch != '\t' && ch != '\n' && ch != 0)
	ch = *(++strptr);

    if (strptr == str)
	return NULL;

    *len = strptr - str;
    return str;
}

static int def_line_no;

static char *default_data[] = {
    "NAME C",
    "CODESET0",
    "Length 1",
    "GL",
    "Encoding",
    "ISO8859-1 GL",
    "Font",
    "ISO8859-1 GL",
    0,
};

static char *
get_line(fd, buf, buf_len)
    FILE *fd;
    char *buf;
    int buf_len;
{
    int length, token;

    if (fd) {
	while (fgets(buf, buf_len, fd)) {
	    if (buf[0] == '#')
	    	continue;
	    return buf;
	}
	return NULL;
    } else {
	if (default_data[def_line_no]) {
	    strcpy(buf, default_data[def_line_no]);
	    def_line_no++;
	    return buf;
	}
	return NULL;
    }
}

#ifndef XFILESEARCHPATHDEFAULT
#define XFILESEARCHPATHDEFAULT "/usr/lib/X11/%L/%T/%N%S:/usr/lib/X11/%l/%T/%N%S:/usr/lib/X11/%T/%N%S"
#endif
#ifndef LIBDIR
#define LIBDIR	"/usr/lib/X11"
#endif

#define MAX_PATH	512

static FILE *
open_codeset_file(locale, name)
char *locale;
char *name;
{
    char ch, buf[BUFSIZE], *bufptr, *strptr1, *strptr2, *path;
    char lang[MAX_PATH], territory[MAX_PATH], codeset[MAX_PATH];
    Bool first;
    FILE *fd;

    strptr1 = index(locale, '_');
    strptr2 = index(locale, '.');

    *lang = *territory = *codeset = 0;
    if (strptr1) {
	strncpy(lang, locale, strptr1 - locale);
	lang[strptr1 - locale] = 0;
	strptr1++;
	if (strptr2) {
	    strncpy(territory, strptr1, strptr2 - strptr1);
	    territory[strptr2 - strptr1] = 0;
	    strcpy(codeset, strptr2 + 1);
	} else
	    strcpy(territory, strptr1);
    } else {
	if (strptr2) {
	    strncpy(lang, locale, strptr2 - locale);
	    lang[strptr2 - locale] = 0;
	    strcpy(codeset, strptr2 + 1);
	} else 
	    strcpy(lang, locale);
    }

    if ((path = getenv("XFILESEARCHPATH")) == NULL)
    	path = XFILESEARCHPATHDEFAULT;
    first = True;
    bufptr = buf;
    while (1) {
	ch = *path++;
	if (ch != ':')
	    first = False;
	switch (ch) {
	    case ':':
	    	if (first) {
		    strcpy(buf, name);
		    bufptr += strlen(strptr1);
		}
	    case '\0':
end:
		*bufptr = 0;
		if (fd = fopen(buf, "r"))
		    return fd;
		if (ch == '\0') {
		    sprintf(buf, "%s/%s/%s", LIBDIR, locale, name);
		    return fopen(buf, "r");
		}
	    	first = True;
	    	bufptr = buf;
		continue;
	    case '%':
		switch (ch = *path++) {
		    case 'N': 
			strptr1 = name;
			break;
		    case 'L':
			strptr1 = locale;
			break;
		    case 'l':
			strptr1 = lang;
			break;
		    case 't':
			strptr1 = territory;
			break;
		    case 'c':
			strptr1 = codeset;
			break;
		    case '\0':
			goto end;
		    case '%':
		    case ':':
			*bufptr++ = ch;
		    default:
			continue;
		}
		strcpy(bufptr, strptr1);
		bufptr += strlen(strptr1);
		continue;
	    default:
		*bufptr++ = ch;
		break;
	}
    }
}

static int
get_encoding(name, len)
    char *name;
    int len;
{
    EncodingRec *encoding_ptr, **table_ptr;

    table_ptr = encoding_table;

    while (encoding_ptr = *table_ptr++)
	if (_Ximp_NCompareISOLatin1(encoding_ptr->charset_name, name, len) == 0)
	    return encoding_ptr->lindex;
    
    return -1;
}

#define CODESET_FILE	"Codeset"

Bool
_Ximp_load_codeset_data(lcd)
    Ximp_XLCd lcd;
{
    XLCdXimpRec *lcpart = lcd->ximp_lcpart;
    CodeSetRec codeset_tbl[MAX_CODESET], *codeset;
    FontSetDataRec font_data_tbl[MAX_FONTSET], *font_data;
    EncodingIndexRec *index_ptr;
    char buf[BUFSIZE], *bufptr;
    char name_buf[BUFSIZE], *name_bufptr;
    int token, last_token, word_len, lindex;
    int i, tmp, font_data_num, cur_num, max_num = -1;
    FILE *fd;

    fd = open_codeset_file(lcd->core.name, CODESET_FILE);
    if (fd == NULL) {
	if (strcmp(lcd->core.name, "C"))
	    return False;
	def_line_no = 0;
    }

    codeset = codeset_tbl;
    for (i = 0; i < MAX_CODESET; i++, codeset++) {
	codeset->char_length = 1;
	codeset->msb_mask = GL;
	codeset->index_num = 0;
	codeset->encoding_index = NULL;
    }
    lcpart->codeset_name = NULL;
    lcpart->codeset = NULL;
    lcpart->fontset_data = NULL;
    codeset = codeset_tbl;
    font_data_num = 0;
    font_data = font_data_tbl;
    name_bufptr = name_buf;
    cur_num = 0;

    while (get_line(fd, buf, BUFSIZE)) {
	bufptr = buf;
	if ((bufptr = get_word(bufptr, &word_len)) == 0)
	    continue;
	token = get_token(bufptr, word_len);
	bufptr += word_len;
	switch (token) {
	    case NAME:
		if ((bufptr = get_word(bufptr, &word_len)) == 0)
		    continue;
		if ((lcpart->codeset_name = Xmalloc(word_len + 1)) == NULL)
		    goto error;
		strncpy(lcpart->codeset_name, bufptr, word_len);
		lcpart->codeset_name[word_len] = 0;
		break;
	    case CODESET:
		bufptr--;
		tmp = *bufptr - '0';
		if (tmp >= 0 && tmp <= 9) {
		    cur_num = tmp;
		    max_num = max(cur_num, max_num);
		    codeset = codeset_tbl + cur_num;
		}
		break;
	    case GL_ENCODING:
	    case GR_ENCODING:
		codeset->msb_mask = (token == GL_ENCODING) ? GL : GR;
		break;
	    case LENGTH:
		if ((bufptr = get_word(bufptr, &word_len)) == 0)
		    continue;
		tmp = *bufptr - '0';
		if (tmp >= 0 && tmp <= 9)
		    codeset->char_length = tmp;
		break;
	    case ENCODING:
	    case FONT:
	    case EXT_FONT:
		break;
	    case STRING:
		bufptr -= word_len;
		if (last_token == ENCODING) {
		    if ((lindex = get_encoding(bufptr, word_len)) == -1)
			continue;

		    bufptr += word_len;
		    if ((bufptr = get_word(bufptr, &word_len)) == 0)
			continue;
		    token = get_token(bufptr, word_len);
		    if (token != GL_ENCODING && token != GR_ENCODING)
			continue;

		    tmp = codeset->index_num;
		    if (tmp == 0)
			index_ptr = (EncodingIndexRec *) 
					Xmalloc(sizeof(EncodingIndexRec));
		    else
			index_ptr = (EncodingIndexRec *)
					Xrealloc(codeset->encoding_index,
						 sizeof(EncodingIndexRec) * 
						 (tmp + 1));
		    if (index_ptr == NULL)
			goto error;
		    codeset->encoding_index = index_ptr;
		    index_ptr += tmp;
		    index_ptr->lindex = lindex;
		    index_ptr->msb_mask = (token == GL_ENCODING) ? GL : GR;
		    codeset->index_num = tmp + 1;
		} else if (last_token == FONT || last_token == EXT_FONT) {
		    strncpy(name_bufptr, bufptr, word_len);
		    name_bufptr[word_len] = 0;

		    bufptr += word_len;
		    if ((bufptr = get_word(bufptr, &word_len)) == 0)
			continue;
		    token = get_token(bufptr, word_len);
		    if (token != GL_ENCODING && token != GR_ENCODING)
			continue;

		    font_data->cset_number = cur_num;
		    font_data->font_name = name_bufptr;
		    font_data->msb_mask = (token == GL_ENCODING) ? GL : GR;
		    font_data->ext_flag = (last_token == EXT_FONT);
		    font_data++;
		    font_data_num++;

		    name_bufptr += strlen(name_bufptr) + 1;
		}
		continue;
	}
	last_token = token;
    }

    max_num++;
    if (max_num == 0)
	goto error;
    if ((codeset = (CodeSetRec *) Xmalloc(max_num*sizeof(CodeSetRec))) == NULL)
	goto error;
    if ((font_data = (FontSetDataRec *) Xmalloc(font_data_num * 
					sizeof(FontSetDataRec))) == NULL)
	goto error;
    if ((name_bufptr = (char *) Xmalloc(name_bufptr - name_buf)) == NULL)
	goto error;

    lcpart->codeset_num = max_num;
    lcpart->codeset = codeset;
    lcpart->fontset_data_num = font_data_num;
    lcpart->fontset_data = font_data;
    for (i = 0; i < max_num; i++, codeset++)
	*codeset = codeset_tbl[i];
    for (i = 0; i < font_data_num; i++, font_data++) {
	*font_data = font_data_tbl[i];
	strcpy(name_bufptr, font_data->font_name);
	font_data->font_name = name_bufptr;
	name_bufptr += strlen(name_bufptr) + 1;
    }

    return True;

error:
    codeset = codeset_tbl;
    for (i = 0; i < MAX_CODESET; i++, codeset++)
	if (codeset->encoding_index)
	    Xfree(codeset->encoding_index);

    if (lcpart->codeset_name)
	Xfree(lcpart->codeset_name);
    if (lcpart->codeset)
	Xfree(lcpart->codeset);
    if (lcpart->fontset_data)
	Xfree(lcpart->fontset_data);

    return False;
}

void
_Ximp_free_codeset(lcd)
    Ximp_XLCd lcd;
{
    XLCdXimpRec *lcpart = lcd->ximp_lcpart;
    CodeSetRec *codeset;
    FontSetDataRec *font_data;
    int i;

    if (lcpart->codeset_name)
	Xfree(lcpart->codeset_name);
    if (codeset = lcpart->codeset) {
	for (i = 0; i < lcpart->codeset_num; i++, codeset++)
	    if (codeset->index_num)
		Xfree(codeset->encoding_index);
	Xfree(lcpart->codeset);
    }
    if (font_data = lcpart->fontset_data) {
	if (font_data->font_name)
	    Xfree(font_data->font_name);
	Xfree(font_data);
    }
}

#ifdef X_NOT_STDC_ENV
#ifndef toupper
#define toupper(c)	((int)(c) - 'a' + 'A')
#endif
#endif

int 
_Ximp_CompareISOLatin1(str1, str2)
    char *str1, *str2;
{
    register char ch1, ch2;

    for ( ; (ch1 = *str1) && (ch2 = *str2); str1++, str2++) {
	if (islower(ch1))
	    ch1 = toupper(ch1);
	if (islower(ch2))
	    ch2 = toupper(ch2);

	if (ch1 != ch2)
	    break;
    }

    return *str1 - *str2;
}
int 
_Ximp_NCompareISOLatin1(str1, str2, len)
    char *str1, *str2;
    int len;
{
    register char ch1, ch2;

    for ( ; (ch1 = *str1) && (ch2 = *str2) && len; str1++, str2++, len--) {
	if (islower(ch1))
	    ch1 = toupper(ch1);
	if (islower(ch2))
	    ch2 = toupper(ch2);

	if (ch1 != ch2)
	    break;
    }

    if (len == 0)
	return 0;

    return *str1 - *str2;
}
