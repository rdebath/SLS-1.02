/* $XConsortium: Ximplc.h,v 1.6 92/07/29 10:16:27 rws Exp $ */
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
              Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED
and Sony Corporation not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.
FUJITSU LIMITED and Sony Corporation make no representations about
the suitability of this software for any purpose.  It is provided
"as is" without express or implied warranty.

FUJITSU LIMITED AND SONY CORPORATION DISCLAIM ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL FUJITSU LIMITED AND
SONY CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE
OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
          Makoto Wakamatsu     Sony Corporation

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

#include "XIMProto.h"

#define USE_SJIS	True
#ifdef SVR4
#define FIX_EUC32	True
#endif

#define GL		0x00
#define GR		0x80
#define MAX_CODESET	10
#define MAX_FONTSET	50
#define XIMP_MB_CUR_MAX(lcd)   (((Ximp_XLCd)(lcd))->ximp_lcpart->mb_cur_max) 
#ifndef MB_CUR_MAX
#define MB_CUR_MAX      XIMP_MB_CUR_MAX(_XlcCurrentLC())
#endif
#ifndef MB_LEN_MAX
#define MB_LEN_MAX      8
#endif

#ifndef MAXINT
#define MAXINT (~(1 << (8 * sizeof(int)) - 1))
#endif /* !MAXINT */

typedef struct _Ximp_XLCd	*Ximp_XLCd;
typedef struct _Ximp_XFontSet	*Ximp_XFontSet;

/*
 * XLCd dependent data
 */

enum {
    ISO8859_1, ISO8859_2, ISO8859_3, ISO8859_4, ISO8859_7,
    ISO8859_6, ISO8859_8, ISO8859_5, ISO8859_9, JISX0201_1976_0,
    GB2312_1980_0, GB2312_1980_1, JISX0208_1983_0, JISX0208_1983_1,
    KSC5601_1987_0, KSC5601_1987_1
} encoding_define;

typedef struct {
    int			lindex;
    unsigned char	msb_mask;
} EncodingIndexRec;

typedef struct {
    int			lindex;
    char	       *charset_name;
    int			char_length;
    char	       *GL_encoding;
    int			GL_gc_size;
    char	       *GR_encoding;
    int			GR_gc_size;
} EncodingRec;

typedef struct {
    int			char_length;
    unsigned char	msb_mask;
    int			index_num;
    EncodingIndexRec   *encoding_index;
} CodeSetRec;

typedef struct {
    int			cset_number;
    char	       *font_name;
    unsigned char       msb_mask;
    Bool		ext_flag;
} FontSetDataRec;

typedef struct {
    Bool		(*initialize)();
    void		(*cnv_start)();
    void		(*cnv_end)();
    char		(*mbchar)();
    int			(*mbstocs)();
    int			(*wcstocs)();
    int			(*cstombs)();
    int			(*cstowcs)();
} XLCdXimpMethods;

typedef struct {
    char	       *codeset_name;
    int			codeset_num;
    CodeSetRec	       *codeset;
    int			mb_cur_max; 
    Bool		state_dependent;
    int			fontset_data_num;
    FontSetDataRec     *fontset_data;
    XLCdXimpMethods    *methods;
    XPointer		extension;
} XLCdXimpRec;

typedef struct _Ximp_XLCd {
    XLCdMethods		methods;
    XLCdCoreRec		core;	
    XLCdXimpRec	       *ximp_lcpart;
} Ximp_XLCdRec;

/*
 * XFontSet dependent data
 */

typedef struct {
    XFontStruct	       *font;
    unsigned char	msb_mask;
    unsigned		min_char;
    unsigned		max_char;
} ExtFontRec;

typedef struct {
    XFontStruct	       *font;
    unsigned char	msb_mask;
    int			ext_font_num;
    ExtFontRec	       *ext_font_list;
} FontSetRec;

typedef struct {
    int			fontset_num;
    FontSetRec	       *fontset;
    XPointer		extension;
} XFontSetXimpRec;

typedef struct _Ximp_XFontSet {
    XFontSetMethods	methods;
    XFontSetCoreRec	core;	
    XFontSetXimpRec    *ximp_fspart;
} Ximp_XFontSetRec;

extern EncodingRec **encoding_table;

/*
 * Input Method data
 */

typedef struct _Ximp_XIM	*Ximp_XIM;
typedef struct _Ximp_XIC	*Ximp_XIC;

#define XIMP_NAME	256
#define	XIMP_TIME_OUT	120

#define XIMP_CREATE_IC	0
#define	XIMP_SET_IC	1
#define	XIMP_START_IC	2

#define XIMP_INPUT_STYLE	0x0001
#define XIMP_CLIENT_WIN		0x0002
#define XIMP_RES_NAME		0x0004
#define XIMP_RES_CLASS		0x0008
#define XIMP_GEOMETRY_CB        0x0010
#define XIMP_FILTER_EV          0x0020
#define XIMP_PRE_CALLBAK        0x0040
#define XIMP_STS_CALLBAK        0x0080

#define XIMP_PROP_FOCUS		( XIMP_FOCUS_WIN_MASK )
#define XIMP_PROP_PREEDIT	( XIMP_PRE_AREA_MASK \
				| XIMP_PRE_FG_MASK \
				| XIMP_PRE_BG_MASK \
				| XIMP_PRE_COLORMAP_MASK \
				| XIMP_PRE_BGPIXMAP_MASK \
				| XIMP_PRE_LINESP_MASK \
				| XIMP_PRE_CURSOR_MASK \
				| XIMP_PRE_AREANEED_MASK \
				| XIMP_PRE_SPOTL_MASK )
#define XIMP_PROP_STATUS	( XIMP_STS_AREA_MASK \
				| XIMP_STS_FG_MASK \
				| XIMP_STS_BG_MASK \
				| XIMP_STS_COLORMAP_MASK \
				| XIMP_STS_BGPIXMAP_MASK \
				| XIMP_STS_LINESP_MASK \
				| XIMP_STS_CURSOR_MASK \
				| XIMP_STS_AREANEED_MASK \
				| XIMP_STS_WINDOW_MASK )
#define XIMP_PROP_PREFONT       ( XIMP_PRE_FONT_MASK )
#define XIMP_PROP_STSFONT       ( XIMP_STS_FONT_MASK )

/*
 * XIM Extension data
 */
typedef struct {
	int		extension_back_front_exist;
	Atom		extension_back_front_id;
	Bool		extension_conversion_exist;
	Atom		extension_conversion_id;
	Bool		extension_conversion;
	int		extension_statuswindow_exist;
	Atom		extension_statuswindow_id;
	int		extension_lookup_exist;
	Atom		extension_lookup_id;
	Atom		extension_lookup_start;
	Atom		extension_lookup_start_rep;
	Atom		extension_lookup_draw;
	Atom		extension_lookup_proc;
	Atom		extension_lookup_proc_rep;
	/* Add Extension */
	} Ximp_ExtXIMRec;

/*
 * XIM dependent data
 */
typedef struct  {
	XIM		 im_next;
	int		 connectserver;
	int		 inputserver;
	Bool		 use_wchar;
	Ximp_KeyList	*process_start_keys;
	char		*locale_server;
	Window		 fe_window;
	Window		 owner;
	Atom		 improtocol_id;
	Atom		 version_id;
	Atom		 style_id;
	Atom		 keys_id;
	Atom		 servername_id;
	Atom		 serverversion_id;
	Atom		 vendorname_id;
	Atom		 extentions_id;
	Atom		 ctext_id;
	Atom		 focus_win_id;
	Atom		 preedit_atr_id;
	Atom		 status_atr_id;
	Atom		 preeditfont_id;
	Atom		 statusfont_id;
	Atom		 preeditmaxsize_id;
	char		*im_proto_vl;
	XIMStyles	*im_styles;
	Ximp_KeyList	*im_keyslist;
	char		*im_server_name;
	char		*im_server_vl;
	char		*im_vendor_name;
	Atom		*im_ext_list;
	Ximp_ExtXIMRec	*imtype;
	} XIMXimpRec;

/*
 * IM struct
 */
typedef struct _Ximp_XIM {
	XIMMethods	 methods;
	XIMCoreRec	 core;
	XIMXimpRec	*ximp_impart;
	} Ximp_XIMRec;

typedef struct {
	XIMCallback     start;
	XIMCallback     done;
	XIMCallback     draw;
	XIMCallback     proc;
	} ICExtLookupCallbacks;

 /*
  * data block describing the visual attributes associated with an input
  * context
  */
typedef struct {
	XRectangle      area;
	XRectangle      area_needed;
	XPoint          spot_location;
	Colormap        colormap;
	Atom            std_colormap;
	unsigned long   foreground;
	unsigned long   background;
	Pixmap          background_pixmap;
	XFontSet        fontset;
	int             line_space;
	Cursor          cursor;
	XPointer	draw_data;
	ICExtLookupCallbacks callbacks;
	} ICExtLookupAttributes, *ICExtLookupAttributesPtr;

/*
 * IC deprndent data
 */
typedef struct {
	long			 icid;
	int			 input_mode;
	int			 is_bep_mode;
	int			 filter_mode;
	unsigned long		 back_mask;
	long			 value_mask;
	Bool			 putback_key_event;
	Window			 back_focus_win;

	long			 proto_mask;
	Ximp_PreeditPropRec	 preedit_attr;
	char			*preedit_font;
	Ximp_StatusPropRec	 status_attr;
	char			*status_font;
	XIMCallback		 error;
 	/* Extended Callback attribute */
	Bool			 use_lookup_choices;
	ICExtLookupAttributes	 lookup_attr;
	XIMCallback		 restart;
	XIMCallback		 destroy;

	void			*ictype;
	} XICXimpRec;

/*
 * IC struct
 */
typedef struct _Ximp_XIC {
	XICMethods	 methods;
	XICCoreRec	 core;
	XICXimpRec	*ximp_icpart;
	} Ximp_XICRec;

/*
 * predicate argument
 */
typedef struct {
	Atom	type;
	Window	owner;
	int	protocol;
	ICID	icid;
} XimpCMPredicateArgRec, *XimpCMPredicateArg;

typedef struct {
	Atom	type;
	Window	owner;
	ICID	icid;
	Window	window;
	Atom	atom;
} XimpPNPredicateArgRec, *XimpPNPredicateArg;
