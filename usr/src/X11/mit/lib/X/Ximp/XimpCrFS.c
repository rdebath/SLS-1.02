/* $XConsortium: XimpCrFS.c,v 1.6 92/04/14 13:28:48 rws Exp $ */
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
#include <X11/Xos.h>
#include <X11/Xatom.h>

#define MAXFONTS		1000
#define AVERAGE_WIDTH_FIELD	12
#define CHARSET_REGISTRY_FIELD	(AVERAGE_WIDTH_FIELD + 1)
#define MAX_EXT_FONT	10

static void _Ximp_free_fontset();
extern int _Ximp_mb_escapement(), _Ximp_wc_escapement();
extern int _Ximp_mb_extents(), _Ximp_wc_extents();
extern Status _Ximp_mb_extents_per_char(), _Ximp_wc_extents_per_char();
extern int _Ximp_mb_draw_string(), _Ximp_wc_draw_string();
extern void _Ximp_mb_draw_image_string(), _Ximp_wc_draw_image_string();

typedef struct {
    char *charset_name;
    char *font_name;
    XFontStruct *font;
    FontSetDataRec *font_data_list[MAX_CODESET];
    unsigned min_char;
    unsigned max_char;
} FontInfoRec;

/* method list */
static XFontSetMethodsRec fontset_methods = {
    _Ximp_free_fontset,
    _Ximp_mb_escapement,
    _Ximp_mb_extents,
    _Ximp_mb_extents_per_char,
    _Ximp_mb_draw_string,
    _Ximp_mb_draw_image_string,
    _Ximp_wc_escapement,
    _Ximp_wc_extents,
    _Ximp_wc_extents_per_char,
    _Ximp_wc_draw_string,
    _Ximp_wc_draw_image_string
};

static Ximp_XFontSet
initFontSet(lcd)
    Ximp_XLCd lcd;
{
    Ximp_XFontSet ximp_fontset;
    XFontSetXimpRec *fspart;
    FontSetRec *fontset;
    int codeset_num = lcd->ximp_lcpart->codeset_num;

    ximp_fontset = (Ximp_XFontSet) Xmalloc(sizeof(Ximp_XFontSetRec));
    if (ximp_fontset == NULL)
	return NULL;

    fspart = (XFontSetXimpRec *) Xmalloc(sizeof(XFontSetXimpRec));
    if (fspart == NULL)
	goto error;

    fontset = (FontSetRec *) Xmalloc(sizeof(FontSetRec) * codeset_num);
    if (fontset == NULL)
	goto error;

    bzero(ximp_fontset, sizeof(Ximp_XFontSetRec));
    bzero(fspart, sizeof(XFontSetXimpRec));
    bzero(fontset, sizeof(FontSetRec) * codeset_num);

    fspart->fontset_num = codeset_num;
    fspart->fontset = fontset;

    ximp_fontset->methods = &fontset_methods;
    ximp_fontset->core.lcd = (XLCd) lcd;
    ximp_fontset->ximp_fspart = fspart;

    return ximp_fontset;

error:
    if (fspart)
	Xfree(fspart);
    Xfree(ximp_fontset);

    return NULL;
}

static int
getCharsetName(lcd, font_info)
    Ximp_XLCd lcd;
    FontInfoRec *font_info;
{
    FontSetDataRec *data = lcd->ximp_lcpart->fontset_data;
    FontInfoRec *info_ptr;
    char *name;
    int	count, data_num;
    int	i;

    count = 0;
    data_num = lcd->ximp_lcpart->fontset_data_num;
    while (data_num--) {
	name = data->font_name;
	info_ptr = font_info;
	for (i = 0; i < count; i++, info_ptr++)
	    if (!strcmp(name, info_ptr->charset_name)) {
		info_ptr->font_data_list[data->cset_number] = data;
		break;
	    }

	if (i == count) {	/* not found same font name */
	    info_ptr->charset_name = name;
	    info_ptr->font_data_list[data->cset_number] = data;
	    if (++count >= MAX_FONTSET)
		return count;
	}
	data++;
    }

    return count;
}

static char *
getFontName(dpy, fs)
    Display *dpy;
    XFontStruct	*fs;
{
    unsigned long fp;
    char *fname = NULL;

    if (XGetFontProperty(fs, XA_FONT, &fp))
	fname = XGetAtomName(dpy, fp); 

    return fname;
}

static Bool
checkCharSet(xlfd_name, charset)
    char *xlfd_name;
    char *charset;
{
    char *charset_field;
    int len1, len2;

    len1 = strlen(xlfd_name);
    len2 = strlen(charset);
    if (len1 < len2)
	return False;

    /* XXX */
    charset_field = xlfd_name + (len1 - len2);
    if (!_Ximp_CompareISOLatin1(charset_field, charset))
	return True;
    return False;
}

static int
matchingCharSet(dpy, font_info, font_info_num, fn_list, fs_list, list_num, 
		found_num)
    Display *dpy;
    FontInfoRec *font_info;
    int font_info_num;
    char **fn_list;
    XFontStruct *fs_list;
    int list_num;
    int found_num;
{
    FontInfoRec *info_ptr;
    char *fname, *prop_fname;
    int	i;

    while (list_num--) {
	fname = *fn_list++;
 	prop_fname = getFontName(dpy, fs_list);
	info_ptr = font_info;

	for (i = 0; i < font_info_num; i++, info_ptr++) {
	    if (info_ptr->font_name)
		continue;

	    if (checkCharSet(fname, info_ptr->charset_name))
		goto found;
	    else if (prop_fname && 
		     checkCharSet(prop_fname, info_ptr->charset_name)) {
		fname = prop_fname;
found:
		info_ptr->font_name = Xmalloc(strlen(fname) + 1);
		strcpy(info_ptr->font_name, fname);
		info_ptr->min_char = (fs_list->min_byte1 << 8) + 
				     fs_list->min_char_or_byte2;
		info_ptr->max_char = (fs_list->max_byte1 << 8) + 
				     fs_list->max_char_or_byte2;
		found_num++;
		break;
	    }
	}
	if (prop_fname)
	    Xfree(prop_fname);
	if (found_num == font_info_num)
	    return found_num;
	fs_list++;
    }

    return found_num;
}

static Bool
check_ext_font(font_info, ext_font, ext_font_num)
    FontInfoRec *font_info;
    register ExtFontRec *ext_font;
    int ext_font_num;
{
    register unsigned min_char = font_info->min_char;
    register unsigned max_char = font_info->max_char;

    while (ext_font_num--) {
	if (min_char > ext_font->min_char && max_char < ext_font->max_char)
	    return False;
	ext_font++;
    }

    return True;
}

static Bool
setInternalPartData(lcd, dpy, xfont_set, font_info, font_info_num)
    Ximp_XLCd lcd;
    Display *dpy;
    Ximp_XFontSet xfont_set;
    FontInfoRec *font_info;
    int font_info_num;
{
    FontSetRec *fontset = xfont_set->ximp_fspart->fontset;
    ExtFontRec ext_font[MAX_EXT_FONT], *ext_font_ptr;
    FontInfoRec *info_ptr;
    FontSetDataRec *data;
    char *font_name;
    int i,j, codeset_num, ext_font_num;

    codeset_num = lcd->ximp_lcpart->codeset_num;
    for (i = 0; i < codeset_num; i++, fontset++) {
	info_ptr = font_info;
	ext_font_ptr = ext_font;
	ext_font_num = 0;
	for (j = 0; j < font_info_num; j++, info_ptr++) {
	    data = info_ptr->font_data_list[i];
	    if (data && (font_name = info_ptr->font_name)) {
		if (data->ext_flag) {
		    if (ext_font_num < MAX_EXT_FONT &&
			check_ext_font(info_ptr, ext_font, ext_font_num)) {

			if (info_ptr->font == NULL)
			    info_ptr->font = XLoadQueryFont(dpy, font_name);
			if (ext_font_ptr->font = info_ptr->font) {
			    ext_font_ptr->msb_mask = data->msb_mask;
			    ext_font_ptr->min_char = info_ptr->min_char;
			    ext_font_ptr->max_char = info_ptr->max_char;
			    ext_font_ptr++;
			    ext_font_num++;
			}
		    }
		} else if (fontset->font == NULL) {
		    if (info_ptr->font == NULL)
			info_ptr->font = XLoadQueryFont(dpy, font_name);
		    fontset->font = info_ptr->font;
		    fontset->msb_mask = data->msb_mask;
		}
	    }
	}

	if (fontset->font && ext_font_num) {
	    ext_font_ptr = (ExtFontRec *) Xmalloc(ext_font_num *
						  sizeof(ExtFontRec));
	    if (ext_font_ptr == NULL)
		return False;
	    
	    fontset->ext_font_list = ext_font_ptr;
	    fontset->ext_font_num = ext_font_num;
	    for (j = 0; j < ext_font_num; j++)
		*ext_font_ptr = ext_font[j];
	}
    }

    return True;
}

static void
setFontSetExtents(font_set)
    Ximp_XFontSet font_set;
{
    XRectangle *ink = &font_set->core.font_set_extents.max_ink_extent;
    XRectangle *logical = &font_set->core.font_set_extents.max_logical_extent;
    XFontStruct **font_list, *font;
    XCharStruct overall;
    int logical_ascent, logical_descent;
    int	num = font_set->core.num_of_fonts;

    font_list = font_set->core.font_struct_list;
    font = *font_list++;
    overall = font->max_bounds;
    overall.lbearing = font->min_bounds.lbearing;
    logical_ascent = font->ascent;
    logical_descent = font->descent;

    while (--num > 0) {
	font = *font_list++;
	overall.lbearing = min(overall.lbearing, font->min_bounds.lbearing);
	overall.rbearing = max(overall.rbearing, font->max_bounds.rbearing);
	overall.ascent = max(overall.ascent, font->max_bounds.ascent);
	overall.descent = max(overall.descent, font->max_bounds.descent);
	overall.width = max(overall.width, font->max_bounds.width);
	logical_ascent = max(logical_ascent, font->ascent);
	logical_descent = max(logical_descent, font->descent);
    }

    ink->x = overall.lbearing;
    ink->y = -(overall.ascent);
    ink->width = overall.rbearing - overall.lbearing;
    ink->height = overall.ascent + overall.descent;

    logical->x = 0;
    logical->y = -(logical_ascent);
    logical->width = overall.width;
    logical->height = logical_ascent + logical_descent;
}

static Bool
setCorePartData(font_set, font_info, font_info_num)
    Ximp_XFontSet font_set;
    FontInfoRec *font_info;
    int font_info_num;
{
    FontInfoRec *info_ptr;
    XFontStruct **font_struct_list;
    char **font_name_list, *font_name_buf;
    int	i, count, length;

    count = length = 0;
    info_ptr = font_info;
    for (i = 0; i < font_info_num; i++, info_ptr++)
	if (info_ptr->font) {
	    count++;
	    length += strlen(info_ptr->font_name) + 1;
	}
    if (count == 0)
        return False;

    font_struct_list = (XFontStruct **) Xmalloc(sizeof(XFontStruct *) * count);
    if (font_struct_list == NULL)
	return False;

    if ((font_name_list = (char **) Xmalloc(sizeof(char *) * count)) == NULL)
	goto error;

    if ((font_name_buf = Xmalloc(length)) == NULL)
	goto error;

    count = 0;
    info_ptr = font_info;
    for (i = 0; i < font_info_num; i++, info_ptr++) {
	if (info_ptr->font) {
	    font_struct_list[count] = info_ptr->font;
	    strcpy(font_name_buf, info_ptr->font_name);
	    font_name_list[count++] = font_name_buf;
	    font_name_buf += strlen(font_name_buf) + 1;
	}
	if (info_ptr->font_name) {
	    Xfree(info_ptr->font_name);
	    info_ptr->font_name = 0;
	}
    }

    font_set->core.num_of_fonts = count;
    font_set->core.font_name_list = font_name_list;
    font_set->core.font_struct_list = font_struct_list;
    font_set->core.context_dependent = False;

    setFontSetExtents(font_set);

    return True;

error:
    if (font_name_list)
	Xfree(font_name_list);
    Xfree(font_struct_list);

    return False;
}

static Bool
setMissingList(lcd, xfont_set, font_info, font_info_num, 
	       missing_charset_list, missing_charset_count)
    Ximp_XLCd lcd;
    Ximp_XFontSet xfont_set;
    FontInfoRec *font_info;
    int font_info_num;
    char ***missing_charset_list;
    int *missing_charset_count;
{
    FontSetRec *fontset = xfont_set->ximp_fspart->fontset;
    FontSetDataRec *data;
    char *name_list[MAX_CODESET], **charset_list, *charset_buf;
    int missing_cset_num[MAX_CODESET], missing_cset_count;
    int	i, j, count, length, codeset_num;

    missing_cset_count = 0;
    codeset_num = lcd->ximp_lcpart->codeset_num;
    for (i = 0; i < codeset_num; i++)
	if (fontset[i].font == NULL)
	    missing_cset_num[missing_cset_count++] = i;

    count = length = 0;
    for (i = 0; i < font_info_num; i++, font_info++) {
	if (font_info->font)
	    continue;
	for (j = 0; j < missing_cset_count; j++) {
	    if ((codeset_num = missing_cset_num[j]) < 0)
		continue;
	    data = font_info->font_data_list[codeset_num];
	    if (data && data->ext_flag == False) {
		name_list[count++] = font_info->charset_name;
		length += strlen(font_info->charset_name) + 1;
		missing_cset_num[j] = -1;
		break;
	    }
	}
    }

    if (count > 0) {
	if ((charset_list = (char **) Xmalloc(sizeof(char *) * count)) == NULL)
	    return False;
	if ((charset_buf = Xmalloc(length)) == NULL) {
	    Xfree(charset_list);
	    return False;
	}

	*missing_charset_list = charset_list;
	*missing_charset_count = count;

	for (i = 0; i < count; i++) {
	    strcpy(charset_buf, name_list[i]);
	    *charset_list++ = charset_buf;
	    charset_buf += strlen(charset_buf) + 1;
	}
    } 

    return True;
}

XFontSet
_Ximp_Create_fontset(xlcd, dpy, base_name, name_list, count,
		     missing_charset_list, missing_charset_count)
    XLCd xlcd;
    Display *dpy;
    char *base_name;
    char **name_list;		
    int count;
    char ***missing_charset_list;
    int *missing_charset_count;	
{
    Ximp_XLCd lcd = (Ximp_XLCd) xlcd;
    Ximp_XFontSet font_set;
    FontInfoRec font_info[MAX_FONTSET];
    char *name, **name_list_ptr, **fn_list, buf[BUFSIZE];
    XFontStruct *fs_list;
    int i, fn_num, font_info_num, found_num = 0;

    *missing_charset_list = NULL;
    *missing_charset_count = 0;

    if ((font_set = initFontSet(lcd)) == NULL)
        return (XFontSet) NULL;

    bzero(font_info, sizeof(FontInfoRec) * MAX_FONTSET);
    if ((font_info_num = getCharsetName(lcd, font_info)) == 0)
	goto error;

    name_list_ptr = name_list;
    while (count--) {
        name = *name_list_ptr++;
	fn_list = XListFontsWithInfo(dpy, name, MAXFONTS, &fn_num, &fs_list);
	if (fn_num == 0) {
	    char *p;
	    int n = 0;

	    (void) strcpy(buf, name);

	    p = name = buf;
	    while (p = index(p, '-')) {
		p++;
		n++;
	    }
	    p = name + strlen(name) - 1;
	    if (n == AVERAGE_WIDTH_FIELD && *p != '-')
		(void) strcat(name, "-*");
	    else if (n == CHARSET_REGISTRY_FIELD && *p == '-')
		(void) strcat(name, "*");
	    else
		continue;

	    fn_list = XListFontsWithInfo(dpy, name, MAXFONTS,
							&fn_num, &fs_list);
	    if (fn_num == 0)
		continue;
	}
	found_num = matchingCharSet(dpy, font_info, font_info_num, fn_list,
				    fs_list, fn_num, found_num);
	XFreeFontInfo(fn_list, fs_list, fn_num);
	if (found_num == font_info_num)
	    break;
    }

    if (found_num == 0)
	goto error;

    if (setInternalPartData(lcd, dpy, font_set, font_info, font_info_num)
	    == False)
	goto error;

    if (setCorePartData(font_set, font_info, font_info_num) == False)
	goto error;
    /* XXX */
    font_set->core.base_name_list = base_name;

    if (setMissingList(lcd, font_set, font_info, font_info_num,
		       missing_charset_list, missing_charset_count) == False)
	goto error;

    XFreeStringList(name_list);		

    return (XFontSet)font_set;

error:
    for (i = 0; i < font_info_num; i++)
	if (name = font_info[i].font_name)
	    Xfree(name);

    if (font_set->core.font_name_list)
	XFreeStringList(font_set->core.font_name_list);
    if (font_set->core.font_struct_list)
	Xfree(font_set->core.font_struct_list);

    _Ximp_free_fontset(dpy, font_set);

    return (XFontSet) NULL;
}

static void
_Ximp_free_fontset(dpy, xfont_set)
    Display *dpy;
    XFontSet xfont_set;
{
    XFontSetXimpRec *fspart = ((Ximp_XFontSet) xfont_set)->ximp_fspart;
    FontSetRec *fontset = fspart->fontset;
    int	num = fspart->fontset_num;

    while (num-- > 0) {
	if (fontset->ext_font_list)
	   Xfree(fontset->ext_font_list);
	fontset++;
    }

    Xfree(fspart->fontset);
    Xfree(fspart);
}
