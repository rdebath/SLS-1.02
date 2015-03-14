/* $XConsortium: XimpLCd.c,v 1.5 92/04/22 11:53:17 rws Exp $ */
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

EncodingRec default_encoding[] =
{
    { ISO8859_1,       "ISO8859-1",         1,  "B",   94,  "A",  96 },
    { ISO8859_2,       "ISO8859-2",         1,  "B",   94,  "B",  96 },
    { ISO8859_3,       "ISO8859-3",         1,  "B",   94,  "C",  96 },
    { ISO8859_4,       "ISO8859-4",         1,  "B",   94,  "D",  96 },
    { ISO8859_7,       "ISO8859-7",         1,  "B",   94,  "F",  96 },
    { ISO8859_6,       "ISO8859-6",         1,  "B",   94,  "G",  96 },
    { ISO8859_8,       "ISO8859-8",         1,  "B",   94,  "H",  96 },
    { ISO8859_5,       "ISO8859-5",         1,  "B",   94,  "L",  96 },
    { ISO8859_9,       "ISO8859-9",         1,  "B",   94,  "M",  96 },
    { JISX0201_1976_0, "JISX0201.1976-0",   1,  "J",   94,  "I",  94 },

    { GB2312_1980_0,    "GB2312.1980-0",    2,  "A",   94,  "A",  94 },
    { GB2312_1980_1,    "GB2312.1980-1",    2,  NULL,  0,   "A",  94 },
    { JISX0208_1983_0,  "JISX0208.1983-0",  2,  "B",   94,  "B",  94 },
    { JISX0208_1983_1,  "JISX0208.1983-1",  2,  NULL,  0,   "B",  94 },
    { KSC5601_1987_0,   "KSC5601.1987-0",   2,  "C",   94,  "C",  94 },
    { KSC5601_1987_1,   "KSC5601.1987-1",   2,  NULL,  0,   "C",  94 }
} ; 

EncodingRec **encoding_table = NULL;

typedef struct {
    char *name;
    XLCdXimpMethods *methods;
} LCMethodsRec;

extern XLCdXimpMethods def_lc_methods, euc_lc_methods;
#ifdef USE_SJIS
extern XLCdXimpMethods sjis_lc_methods;
#endif

static LCMethodsRec default_lc_methods[] = {
    { "C",	    &def_lc_methods },
    { "ISO8859-1",  &def_lc_methods },
    { "EUC",	    &euc_lc_methods },
#ifdef USE_SJIS
    { "SJIS",	    &sjis_lc_methods },
#endif
};

static LCMethodsRec **lc_methods_table = NULL;


static Bool
init_encoding_table()
{
    int table_size;
    EncodingRec **table_ptr, *def_ptr;

    table_size = sizeof(default_encoding) / sizeof(EncodingRec) + 1;
    encoding_table = (EncodingRec **) Xmalloc(sizeof(EncodingRec *) * 
					      table_size);
    if (encoding_table == NULL)
	return False;

    def_ptr = default_encoding;
    table_ptr = encoding_table;
    while (--table_size)
	*table_ptr++ = def_ptr++;
    *table_ptr = NULL;

    return True;
}

static Bool
set_lc_methods(lcd)
    Ximp_XLCd lcd;
{
    char *name = lcd->ximp_lcpart->codeset_name;
    LCMethodsRec **table_ptr, *methods_tbl;
    int table_size;

    if (lc_methods_table == NULL) {
	table_size = sizeof(default_lc_methods) / sizeof(LCMethodsRec) + 1;
	lc_methods_table = (LCMethodsRec **) Xmalloc(sizeof(LCMethodsRec *) *
						     table_size);
	if (lc_methods_table == NULL)
	    return False;

	methods_tbl = default_lc_methods;
	table_ptr = lc_methods_table;
	while (--table_size)
	    *table_ptr++ = methods_tbl++;
	*table_ptr = NULL;
    }

    table_ptr = lc_methods_table;
    while (methods_tbl = *table_ptr++) {
	if (_Ximp_CompareISOLatin1(name, methods_tbl->name) == 0) {
	    lcd->ximp_lcpart->methods = methods_tbl->methods;
	    return (*methods_tbl->methods->initialize)(lcd);
	}
    }

    return False;
}

static XLCd
_Ximp_init_locale(locale)
    char *locale;
{
    Ximp_XLCd lcd;

    if ((lcd = (Ximp_XLCd) Xmalloc(sizeof(Ximp_XLCdRec))) == NULL)
	goto error;

    lcd->core.name = locale;	/* not allocation here */

    if ((lcd->ximp_lcpart = 
		(XLCdXimpRec *) Xmalloc(sizeof(XLCdXimpRec))) == NULL)
	goto error;
    lcd->ximp_lcpart->codeset = NULL;
    
    if (encoding_table == NULL)
	if (init_encoding_table() == False)
	    goto error;

    if (_Ximp_load_codeset_data(lcd) == False)
	goto error;

    if (set_lc_methods(lcd) == False)
	goto error;

    return (XLCd) lcd;

error:
    if (lcd) {
	if (lcd->ximp_lcpart) {
	    if (lcd->ximp_lcpart->codeset)
		(void) _Ximp_free_codeset(lcd);
	    Xfree(lcd->ximp_lcpart);
	}
	Xfree(lcd);
    }

    return NULL;
}

extern XFontSet		_Ximp_Create_fontset();
extern XIM 		_Ximp_OpenIM();

static XLCdMethodsRec lcd_methods = {
	_XlcDefaultMapModifiers,
	_Ximp_Create_fontset,
	_Ximp_OpenIM,
	};

XLCd
_XlcDefaultLoader(name)
	char		*name;
	{
	XLCd	 xlcd;
#if !defined(X_NOT_STDC_ENV) && !defined(X_LOCALE)
	char siname[256];
	char *_XlcMapOSLocaleName();

	_XlcMapOSLocaleName(name, siname);
	if(!(xlcd = (XLCd) _Ximp_init_locale(siname)))
		return((XLCd)NULL);
#else
	if(!(xlcd = (XLCd) _Ximp_init_locale(name)))
		return((XLCd)NULL);
#endif

	xlcd->methods       = &lcd_methods;
	xlcd->core.name     = Xmalloc(strlen(name) + 1);
	if(!xlcd->core.name) {
		Xfree(xlcd);
		return((XLCd)NULL);
		}
	strcpy(xlcd->core.name, name);
	xlcd->core.modifiers = NULL;
	return(xlcd);
	}

static void
_Ximp_mbinit(state)
    XPointer state;
{
    Ximp_XLCd lcd = (Ximp_XLCd) state;

    (*lcd->ximp_lcpart->methods->cnv_start)(lcd);
}

static char
_Ximp_mbchar(state, str, lenp)
    XPointer state;
    char *str;
    int *lenp;
{
    Ximp_XLCd lcd = (Ximp_XLCd) state;

    return (*lcd->ximp_lcpart->methods->mbchar)(lcd, str, lenp);
}

static void
_Ximp_mbfinish(state)
    XPointer state;
{
    Ximp_XLCd lcd = (Ximp_XLCd) state;

    (*lcd->ximp_lcpart->methods->cnv_end)(lcd);
}

static char *
_Ximp_lcname(state)
    XPointer state;
{
    Ximp_XLCd lcd = (Ximp_XLCd) state;

    return lcd->core.name;
}

static void
_Ximp_destroy(state)
    XPointer state;
{
}

static XrmMethodsRec Ximp_mb_methods = {
    _Ximp_mbinit,
    _Ximp_mbchar,
    _Ximp_mbfinish,
    _Ximp_lcname,
    _Ximp_destroy
} ;

XrmMethods
_XrmInitParseInfo(state)
    XPointer *state;
{
    Ximp_XLCd lcd;
    
    if ((lcd = (Ximp_XLCd) _XlcCurrentLC()) == NULL)
	return NULL;
    
    *state = (XPointer) lcd;

    return &Ximp_mb_methods;
}
