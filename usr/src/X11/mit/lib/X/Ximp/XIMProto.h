/* $XConsortium: XIMProto.h,v 1.6 92/07/29 10:15:02 rws Exp $ */
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
                               fujiwara@a80.tech.yk.fujitsu.co.jp

******************************************************************/

/* Ximp implementation revision */
#define XIMP_REVISION "Ximp Revision 3.3"

/* Ximp Protocol Version */
#define XIMP_PROTOCOL_VERSION "XIMP.3.5"

/* Input Context ID */
typedef unsigned long	ICID;

/* ClientMessage No. */

/* client <=> frontend  */
#define  XIMP_KEYPRESS		 1

/* client => frontend  */
/* Base Protocol       */
#define  XIMP_CREATE		 2
#define  XIMP_DESTROY		 3
#define  XIMP_BEGIN		 4
#define  XIMP_END		 5
#define  XIMP_SETFOCUS		 6
#define  XIMP_UNSETFOCUS	 7
#define  XIMP_CHANGE		 8
#define  XIMP_MOVE		 9
#define  XIMP_RESET		10
#define  XIMP_SETVALUE		11
#define  XIMP_GETVALUE		12

/* Callback  Protocol  */
#define  XIMP_PREEDITSTART_RETURN	20
#define  XIMP_PREEDITCARET_RETURN	21

/* frontend => client  */
/* Base Protocol       */
#define  XIMP_CREATE_RETURN	 2
#define  XIMP_CONVERSION_BEGIN	30
#define  XIMP_PROCESS_BEGIN	30
#define  XIMP_CONVERSION_END	31
#define  XIMP_PROCESS_END	31
#define  XIMP_READPROP		32
#define  XIMP_GETVALUE_RETURN	33
#define  XIMP_RESET_RETURN	34

/* Callback  Protocol  */
#define  XIMP_GEOMETRY		40
#define  XIMP_PREEDITSTART	41
#define  XIMP_PREEDITDONE	42
#define  XIMP_PREEDITDRAW	43
#define  XIMP_PREEDITDRAW_CM	44
#define  XIMP_PREEDITCARET	45
#define  XIMP_STATUSSTART	46
#define  XIMP_STATUSDONE	47
#define  XIMP_STATUSDRAW	48
#define  XIMP_STATUSDRAW_CM	49
#define  XIMP_PREEDITDRAW_TINY	50

/* Extension Protocol */
#define  XIMP_EXTENSION		90

/* frontend = ERROR => client */
#define  XIMP_ERROR		99

/* Error Notify from IM Server */
/*  Detail Error Number */
#define  XIMP_NoError			0	/* No Error */
#define  XIMP_BadAlloc			1	/* Memeory Alloc Fail */
#define  XIMP_BadStyle         		2	/* Unspported Input Style */
#define  XIMP_BadClientWindow         	3	/* Invalid Client Window */
#define  XIMP_BadFocusWindow		4	/* Invalid Focus Window */
#define  XIMP_BadArea			5	/* Invalid Area */
#define  XIMP_BadSpotLocation		6	/* SpotLocation Out Of Range */
#define  XIMP_BadColormap		7	/* Invalid Colormap ID */
#define  XIMP_BadAtom			8	/* Invalid Atom ID */
#define  XIMP_BadPixel			9	/* Invalid Pixel Value */
#define  XIMP_BadPixmap			10	/* Invalid Pixmap Value */
#define  XIMP_BadName			11	/* Invalid Font Name */
#define  XIMP_BadCursor			12	/* Invalid Cursor ID */
#define  XIMP_BadProtocol		13	/* Invalid Protocol ID */
#define  XIMP_BadProperty		14	/* Invalid Property Name */
#define  XIMP_BadPropertyType		15	/* Invalid Property Type */

/* Property Name */
#define  _XIMP_PROTOCOL		"_XIMP_PROTOCOL"
#define  _XIMP_BASE		"_XIMP_" /* Root Window _XIP_<locale_name> */

/* IMS Window Property Name */
#define  _XIMP_VERSION		"_XIMP_VERSION"
#define  _XIMP_STYLE		"_XIMP_STYLE"
#define  _XIMP_KEYS		"_XIMP_KEYS"
#define  _XIMP_SERVERNAME	"_XIMP_SERVERNAME"
#define  _XIMP_SERVERVERSION	"_XIMP_SERVERVERSION"
#define  _XIMP_EXTENSIONS	"_XIMP_EXTENSIONS"
#define  _XIMP_PREEDITMAXSIZE	"_XIMP_PREEDITMAXSIZE"
#define  _XIMP_VENDORNAME	"_XIMP_VENDORNAME"

/* Client Window Property Name */
#define  _XIMP_LIBVERSION	"_XIMP_VERSION"
#define  _XIMP_FOCUS		"_XIMP_FOCUS"
#define  _XIMP_PREEDIT		"_XIMP_PREEDIT"
#define  _XIMP_STATUS		"_XIMP_STATUS"
#define  _XIMP_PREEDITFONT	"_XIMP_PREEDITFONT"
#define  _XIMP_STATUSFONT	"_XIMP_STATUSFONT"

#define  _XIMP_CTEXT		"_XIMP_CTEXT"

/* CallBack Property Name */
#define  _XIMP_PREEDIT_DRAW_DATA		"_XIMP_PREEDIT_DRAW_DATA"
#define  _XIMP_FEEDBACKS			"_XIMP_FEEDBACKS"
#define  _XIMP_PREEDITDRAWLENGTH 		"_XIMP_PREEDITDRAWLENGTH"
#define  _XIMP_PREEDITDRAWSTRING 		"_XIMP_PREEDITDRAWSTRING"
#define  _XIMP_PREEDITDRAWFEEDBACK 		"_XIMP_PREEDITDRAWFEEDBACK"

#define  _XIMP_EXT_XIMP_CHOICE_START_REQ	"_XIMP_EXT_XIMP_CHOICE_START_REQ"
#define  _XIMP_EXT_XIMP_CHOICE_START_REP	"_XIMP_EXT_XIMP_CHOICE_START_REP"
#define  _XIMP_EXT_XIMP_CHOICE_DRAW_REQ		"_XIMP_EXT_XIMP_CHOICE_DRAW_REQ"
#define  _XIMP_EXT_XIMP_CHOICE_PROC_REQ		"_XIMP_EXT_XIMP_CHOICE_PROC_REQ"
#define  _XIMP_EXT_XIMP_CHOICE_PROC_REP		"_XIMP_EXT_XIMP_CHOICE_PROC_REP"
#define  _XIMP_EXT_XIMP_LOOKUPCHOICES		"_XIMP_EXT_XIMP_LOOKUPCHOICES"

/* Lookup choices REQ and REP */
#define  LOOKUP_CHOICES_BEGIN	        0
#define  LOOKUP_CHOICES_START_REQ	1
#define  LOOKUP_CHOICES_START_REP	2
#define  LOOKUP_CHOICES_PROCESS_REQ	3
#define  LOOKUP_CHOICES_PROCESS_REP	4
#define  LOOKUP_CHOICES_DRAW_REQ	5
#define  LOOKUP_CHOICES_DONE_REQ	6

/* mask (XIMP_CREATE, XIMP_SETVALUE, XIMP_GETVALUE) */
#define XIMP_FOCUS_WIN_MASK		(1L <<  0)
#define XIMP_PRE_AREA_MASK		(1L <<  1)
#define XIMP_PRE_FG_MASK		(1L <<  2)
#define XIMP_PRE_BG_MASK		(1L <<  3)
#define XIMP_PRE_COLORMAP_MASK		(1L <<  4)
#define XIMP_PRE_BGPIXMAP_MASK		(1L <<  5)
#define XIMP_PRE_LINESP_MASK		(1L <<  6)
#define XIMP_PRE_CURSOR_MASK		(1L <<  7)
#define XIMP_PRE_AREANEED_MASK		(1L <<  8)
#define XIMP_PRE_SPOTL_MASK		(1L <<  9)
#define XIMP_STS_AREA_MASK		(1L << 10)
#define XIMP_STS_FG_MASK		(1L << 11)
#define XIMP_STS_BG_MASK		(1L << 12)
#define XIMP_STS_COLORMAP_MASK		(1L << 13)
#define XIMP_STS_BGPIXMAP_MASK		(1L << 14)
#define XIMP_STS_LINESP_MASK		(1L << 15)
#define XIMP_STS_CURSOR_MASK		(1L << 16)
#define XIMP_STS_AREANEED_MASK		(1L << 17)
#define XIMP_STS_WINDOW_MASK		(1L << 18)
#define XIMP_PRE_FONT_MASK		(1L << 19)
#define XIMP_STS_FONT_MASK		(1L << 20)

/* FRONTEND or BACKEND MODE */
#define XIMP_FRONTEND	 0
#define XIMP_BACKEND	 1

/*  XIMP_PREEDITDRAW_CM status value
 * post Ximp 3.4 protocol maybe compliant. 
 * XIMP status flag will may contain the supplementary infomations to 
 * reassemble the XIMPreeditDrawCallbackStruct.
 *	  +-----------------------------------------+
 *	0 | XIMP_PREEDITDRAW_CM                     |
 *	  +-----------------------------------------+
 *	4 | ICID                                    |
 *	  +-------------------+---------------------+
 *	8 |PreeditDrawCBStatus|       caret         |
 *	  +-------------------+---------------------+
 *	12|      chg_first    |      chg_length     |
 *	  +-------------------+---------------------+
 *	16|               feedback                  |
 *	  +-----------------------------------------+
 * PreeditDrawCBStatus:
 *    0x0001 no_text:  if 1, string == NULL (no following client message.)
 *    0x0002 no_feedback: if 1 feedback == NULL
 *    0x0004 feedbacks_via_property: if 1 , feedback field is property atom#
 **/

#define XIMP_PDCBSTATUS_NOTEXT 			0x0001
#define XIMP_PDCBSTATUS_NOFEEDBACK 		0x0002
#define XIMP_PDCBSTATUS_FEEDBACKS_VIA_PROP	0x0004

/* _XIMP_KEYS   struct  */

typedef struct {
	unsigned long		modifier;
	unsigned long		modifier_mask;
	KeySym			keysym;
	} Ximp_Key;

typedef struct {
	unsigned short		 count_keys;
	Ximp_Key		*keys_list;
	} Ximp_KeyList;

typedef struct _Ximp_Area {
	long		x;
	long		y;
	long		width;
	long		height;
	} Ximp_AreaRec;

typedef struct _Ximp_Point {
	long		x;
	long		y;
	} Ximp_PointRec;

typedef struct _Ximp_Size {
	long		width;
	long		height;
	} Ximp_SizeRec;

/* kana-kanji conversion window attributes */

#define XIMP_PREEDIT_MAX_LONG 14
#define XIMP_PREEDIT_MAX_CHAR 56

typedef struct  _Ximp_Preedit {
	Ximp_AreaRec	Area;
	unsigned long   Foreground;
	unsigned long   Background;
	Colormap	Colormap;
	Pixmap		Bg_Pixmap;
	long		LineSpacing;
	Cursor		Cursor;
	Ximp_SizeRec	AreaNeeded;
	Ximp_PointRec    SpotLocation;
	} Ximp_PreeditPropRec;

#define XIMP_STATUS_MAX_LONG 13
#define XIMP_STATUS_MAX_CHAR 52

typedef struct  _Ximp_Status {
	Ximp_AreaRec	Area;
	unsigned long   Foreground;
	unsigned long   Background;
	Colormap	Colormap;
	Pixmap		Bg_Pixmap;
	long		LineSpacing;
	Cursor		Cursor;
	Ximp_SizeRec	AreaNeeded;
	Window		window;
	} Ximp_StatusPropRec;

/* for CallBack */
typedef struct _Ximp_PreeditDrawDataPropRec {
	long caret;
	long chg_first;
	long chg_length;
} Ximp_PreeditDrawDataPropRec, *Ximp_PreeditDrawDataProp;

/* for Commit, PreEditDraw, StatusDraw */
typedef struct _Ximp_CommitPropRec {
	unsigned long   icid;
	char            size;
	char            ctext[11];
} Ximp_CommitPropRec, Ximp_PreEditDrawCallbackPropRec,
  Ximp_StatusDrawCallbackPropRec;

/* for PreEditDraw */
typedef struct {
	short           chg_first;
	short           chg_length;
} Ximp_slong;

typedef union {
	Ximp_slong      slong;
	long            l;
} Ximp_uslong;
