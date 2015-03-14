/*
 * Copyright (c) 1991, 1992 Stanford University
 * Copyright (c) 1991, 1992 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include <IV-look/bevel.h>
#include <IV-look/ol_kit.h>
#include <InterViews/adjust.h>
#include <InterViews/brush.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/cursor.h>
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/font.h>
#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/observe.h>
#include <InterViews/patch.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/telltale.h>
#include <InterViews/window.h>
#include <OS/string.h>
#include <stdio.h>

/*
 * character definitions
 */
#define OLG_VSB_ELEVATOR                        1
#define OLG_VSB_ELEVATOR_LINE_BACKWARD          2
#define OLG_VSB_ELEVATOR_ABSOL_UTE              3
#define OLG_VSB_ELEVATOR_LINE_FORWARD           4
#define OLG_VSB_REDUCED_ELEVATOR                5
#define OLG_VSB_REDUCED_ELEVATOR_LINE_BACKWARD  6
#define OLG_VSB_REDUCED_ELEVATOR_LINE_FORWARD   7
#define OLG_VSB_ANCHOR                          8
#define OLG_VSB_ANCHOR_INVERTED                 9
#define OLG_HSB_ELEVATOR                        10
#define OLG_HSB_ELEVATOR_LINE_BACKWARD          11
#define OLG_HSB_ELEVATOR_ABSOLUTE               12
#define OLG_HSB_ELEVATOR_LINE_FORWARD           13
#define OLG_HSB_REDUCED_ELEVATOR                14
#define OLG_HSB_REDUCED_ELEVATOR_LINE_BACKWARD  15
#define OLG_HSB_REDUCED_ELEVATOR_LINE_FORWARD   16
#define OLG_HSB_ANCHOR                          17
#define OLG_HSB_ANCHOR_INVERTED                 18
#define OLG_MENU_PIN_OUT                        19
#define OLG_MENU_PIN_IN                         20
#define OLG_MENU_DEFAULT_PIN_OUT                21
#define OLG_ABBREV_MENU_BUTTON                  22
#define OLG_ABBREV_MENU_BUTTON_INVERTED         23
/* new extension */
#define BUTTON_UL				24
#define BUTTON_LL				25
#define BUTTON_LEFT_ENDCAP_FILL			26
#define BUTTON_LR				27
#define BUTTON_UR				28
#define BUTTON_RIGHT_ENDCAP_FILL		29
#define BUTTON_TOP_1				30
#define BUTTON_TOP_2				31
#define BUTTON_TOP_4				32
#define BUTTON_TOP_8				33
#define BUTTON_TOP_16				34
#define BUTTON_BOTTOM_1				35
#define BUTTON_BOTTOM_2				36
#define BUTTON_BOTTOM_4				37
#define BUTTON_BOTTOM_8				38
#define BUTTON_BOTTOM_16			39
#define BUTTON_FILL_1				40
#define BUTTON_FILL_2				41
#define BUTTON_FILL_4				42
#define BUTTON_FILL_8				43
#define BUTTON_FILL_16				44
#define VERT_MENU_MARK_UL			45
#define VERT_MENU_MARK_LR			46
#define VERT_MENU_MARK_FILL			47
#define HORIZ_MENU_MARK_UL			48
#define HORIZ_MENU_MARK_LR			49
#define HORIZ_MENU_MARK_FILL			50
#define ABBREV_MENU_UL				51
#define ABBREV_MENU_LR				52
#define ABBREV_MENU_FILL			53
#define VERT_SB_UL				54
#define VERT_SB_LR				55
#define VERT_SB_TOPBOX_FILL			56
#define HORIZ_SB_UL				57
#define HORIZ_SB_LR				58
#define VERT_SB_BOTBOX_FILL			59
#define HORIZ_SLIDER_CONTROL_UL			60
#define HORIZ_SLIDER_CONTROL_LR			61
#define HORIZ_SLIDER_CONTROL_FILL		62
#define HORIZ_SLIDER_UL				63
#define HORIZ_SLIDER_LL				64
#define HORIZ_SLIDER_UR				65
#define HORIZ_SLIDER_LR				66
#define HORIZ_SLIDER_BOTTOM_1			67
#define HORIZ_SLIDER_BOTTOM_2			68
#define HORIZ_SLIDER_BOTTOM_4			69
#define HORIZ_SLIDER_BOTTOM_8			70
#define HORIZ_SLIDER_BOTTOM_16			71
#define HORIZ_SLIDER_FILL_1			72
#define HORIZ_SLIDER_FILL_2			73
#define HORIZ_SLIDER_FILL_4			74
#define HORIZ_SLIDER_FILL_8			75
#define HORIZ_SLIDER_FILL_16			76
#define HORIZ_SLIDER_LEFT_ENDCAP_FILL		77
#define HORIZ_SLIDER_RIGHT_ENDCAP_FILL		78
#define VERT_SLIDER_UL				79
#define VERT_SLIDER_UR				80
#define VERT_SLIDER_TOP_ENDCAP_FILL		81
#define VERT_SLIDER_LL				82
#define VERT_SLIDER_LR				83
#define VERT_SLIDER_BOTTOM_ENDCAP_FILL		84
#define VERT_SLIDER_CONTROL_UL			85
#define VERT_SLIDER_CONTROL_LR			86
#define VERT_SLIDER_CONTROL_FILL		87
#define UL_RESIZE_UL				88
#define UL_RESIZE_LR				89
#define UL_RESIZE_FILL				90
#define UR_RESIZE_UL				91
#define UR_RESIZE_LR				92
#define UR_RESIZE_FILL				93
#define LR_RESIZE_UL				94
#define LR_RESIZE_LR				95
#define LR_RESIZE_FILL				96
#define LL_RESIZE_UL				97
#define LL_RESIZE_LR				98
#define LL_RESIZE_FILL				99
#define PUSHPIN_OUT_TOP				100
#define PUSHPIN_OUT_BOTTOM			101
#define PUSHPIN_OUT_MIDDLE			102
#define PUSHPIN_IN_TOP				103
#define PUSHPIN_IN_BOTTOM			104
#define PUSHPIN_IN_MIDDLE			105
#define DFLT_BUTTON_LEFT_ENDCAP			106
#define DFLT_BUTTON_RIGHT_ENDCAP		107
#define DFLT_BUTTON_MIDDLE_1			108
#define DFLT_BUTTON_MIDDLE_2			109
#define DFLT_BUTTON_MIDDLE_4			110
#define DFLT_BUTTON_MIDDLE_8			111
#define DFLT_BUTTON_MIDDLE_16			112
#define BASE_OFF_SPECIALCHAR			113 /*special char */
#define UNCHECKED_BOX_UL			114
#define UNCHECKED_BOX_LR			115
#define UNCHECKED_BOX_FILL			116
#define CHECK_MARK				117
#define CHECKED_BOX_FILL			118
#define UNCHECKED_BOX_OUTLINE			119
#define HORIZ_GAUGE_UL				120
#define HORIZ_GAUGE_LL				121
#define HORIZ_GAUGE_UR				122
#define HORIZ_GAUGE_LR				123
#define HORIZ_GAUGE_BOTTOM_1			124
#define HORIZ_GAUGE_BOTTOM_2			125
#define HORIZ_GAUGE_BOTTOM_4			126
#define HORIZ_GAUGE_BOTTOM_8			127
#define HORIZ_GAUGE_BOTTOM_16			128
#define VERT_GAUGE_UL				129
#define VERT_GAUGE_UR				130
#define VERT_GAUGE_LL				131
#define VERT_GAUGE_LR				132
#define VERT_ABBREV_SB_UL			133
#define VERT_ABBREV_SB_LR			134
#define HORIZ_SB_RIGHTBOX_FILL			135
#define HORIZ_ABBREV_SB_UL			136
#define HORIZ_ABBREV_SB_LR			137
#define HORIZ_SB_LEFTBOX_FILL			138
#define BUTTON_OUTLINE_LEFT_ENDCAP		139
#define BUTTON_OUTLINE_RIGHT_ENDCAP		140
#define BUTTON_OUTLINE_MIDDLE_1			141
#define BUTTON_OUTLINE_MIDDLE_2			142
#define BUTTON_OUTLINE_MIDDLE_4			143
#define BUTTON_OUTLINE_MIDDLE_8			144
#define BUTTON_OUTLINE_MIDDLE_16		145
#define BUTTON_FILL_2D_LEFTENDCAP		146
#define BUTTON_FILL_2D_RIGHTENDCAP      	147
#define BUTTON_FILL_2D_MIDDLE_1     		148
#define BUTTON_FILL_2D_MIDDLE_2 		149
#define BUTTON_FILL_2D_MIDDLE_4 		150
#define BUTTON_FILL_2D_MIDDLE_8 		151
#define BUTTON_FILL_2D_MIDDLE_16		152
#define MENU_DFLT_OUTLINE_LEFT_ENDCAP           153
#define MENU_DFLT_OUTLINE_RIGHT_ENDCAP          154
#define MENU_DFLT_OUTLINE_MIDDLE_1              155
#define MENU_DFLT_OUTLINE_MIDDLE_2              156
#define MENU_DFLT_OUTLINE_MIDDLE_4              157
#define MENU_DFLT_OUTLINE_MIDDLE_8              158
#define MENU_DFLT_OUTLINE_MIDDLE_16             159
#define PIXLABEL_BUTTON_UL			160 
#define PIXLABEL_BUTTON_LL			161
#define UL_RESIZE_OUTLINE			162
#define UR_RESIZE_OUTLINE			163
#define LR_RESIZE_OUTLINE			164
#define LL_RESIZE_OUTLINE			165
#define VERT_SB_NO_BACK_OUTLINE                 166
#define VERT_SB_NO_FWD_OUTLINE                  167
#define VERT_SB_INACTIVE_OUTLINE                168
#define HORIZ_SB_NO_BACK_OUTLINE                169
#define HORIZ_SB_NO_FWD_OUTLINE                 170
#define HORIZ_SB_INACTIVE_OUTLINE               171
#define HORIZ_SLIDER_CONTROL_OUTLINE		172
#define HORIZ_SLIDER_LEFT_ENDCAP_OUTLINE	173
#define	HORIZ_SLIDER_RIGHT_ENDCAP_OUTLINE	174
#define HORIZ_SLIDER_OUTLINE_1			175
#define HORIZ_SLIDER_OUTLINE_2			176
#define HORIZ_SLIDER_OUTLINE_4			177
#define HORIZ_SLIDER_OUTLINE_8			178
#define HORIZ_SLIDER_OUTLINE_16			179
#define VERT_SLIDER_TOP_ENDCAP_OUTLINE		180
#define VERT_SLIDER_BOTTOM_ENDCAP_OUTLINE	181
#define VERT_SLIDER_CONTROL_OUTLINE		182
#define PUSHPIN_OUT_DEFAULT_TOP 		183
#define PUSHPIN_OUT_DEFAULT_BOTTOM 		184
#define PUSHPIN_OUT_DEFAULT_MIDDLE 		185
#define HORIZ_GAUGE_LEFT_ENDCAP_OUTLINE		186
#define HORIZ_GAUGE_RIGHT_ENDCAP_OUTLINE	187
#define HORIZ_GAUGE_OUTLINE_MIDDLE_1		188
#define HORIZ_GAUGE_OUTLINE_MIDDLE_2		189
#define HORIZ_GAUGE_OUTLINE_MIDDLE_4		190
#define HORIZ_GAUGE_OUTLINE_MIDDLE_8		191
#define HORIZ_GAUGE_OUTLINE_MIDDLE_16		192
#define CHECK_BOX_CLEAR_FILL			193
#define VERT_SB_BOX_UL 				194
#define VERT_SB_BOX_LR 				195
#define DIMPLE_UL				196
#define DIMPLE_LR				197
#define DIMPLE_FILL				198
#define SLIDER_CHANNEL_OFFSET			199 /* special char */
#define HORIZ_SB_BOX_UL				200
#define HORIZ_SB_BOX_LR				201
#define VERT_BACK_MENU_MARK_UL			202
#define VERT_BACK_MENU_MARK_LR			203
#define VERT_BACK_MENU_MARK_FILL		204
#define HORIZ_BACK_MENU_MARK_UL			205
#define HORIZ_BACK_MENU_MARK_LR			206
#define HORIZ_BACK_MENU_MARK_FILL		207
#define	OLGX_ACTIVE_CARET			208
#define OLGX_INACTIVE_CARET			209
#define VERT_GAUGE_TOPENDCAP   			210
#define VERT_GAUGE_BOTENDCAP   			211
#define PIXLABEL_BUTTON_UR   			212
#define PIXLABEL_BUTTON_LR   			213
#define PIXLABEL_BUTTON_2D_LR 			214
#define PIXLABEL_DEF_BUTTON_UL 			215
#define PIXLABEL_DEF_BUTTON_LL 			216
#define PIXLABEL_DEF_BUTTON_UR 			217
#define PIXLABEL_DEF_BUTTON_LR 			218
#define HORIZ_GAUGE_LEFT_ENDFILL                219
#define HORIZ_GAUGE_MIDDLE_FILL_1               220
#define HORIZ_GAUGE_MIDDLE_FILL_2               221
#define HORIZ_GAUGE_MIDDLE_FILL_4               222
#define HORIZ_GAUGE_MIDDLE_FILL_8               223
#define HORIZ_GAUGE_MIDDLE_FILL_16              224
#define HORIZ_GAUGE_RIGHT_ENDFILL               225
#define VERT_GAUGE_TOP_FILL                     226
#define VERT_GAUGE_BOT_FILL                     227
#define TEXTSCROLLBUTTON_LEFT                   228
#define TEXTSCROLLBUTTON_RIGHT                  229
#define TEXTSCROLLBUTTON_LEFT_INV               230
#define TEXTSCROLLBUTTON_RIGHT_INV              231
#define NUMERIC_SCROLL_BUTTON_NORMAL            232
#define NUMERIC_SCROLL_BUTTON_LEFT_INV          233
#define NUMERIC_SCROLL_BUTTON_RIGHT_INV         234

static PropertyData kit_props[] = {
    { "*flat", "#aaaaaa" },
    { "*PaletteButton*minimumWidth", "72.0" },
    { "*PushButton*minimumWidth", "72.0" },
    nil
};

/* Table B-20 */
struct OL_ScrollbarSpecs {
    float a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, m_, n_, p_, r_;
};

static OL_ScrollbarSpecs sb_10 = {
    13.0, 5.0, 1.6, 40.6, 13.0, 13.0, 4.9, 1.6, 2.4, 0.8, 1.2, 0.8, 3.7, 5.3,
    0.8
};

static OL_ScrollbarSpecs sb_12 = {
    15.0, 6.0, 2.0, 47.0, 15.0, 15.0, 5.5, 2.0, 3.0, 1.0, 1.5, 1.0, 4.0, 6.0,
    1.0
};

static OL_ScrollbarSpecs sb_14 = {
    17.0, 7.0, 2.4, 53.4, 17.0, 17.0, 6.1, 2.4, 3.6, 1.2, 1.8, 1.2, 4.3, 6.7,
    1.2
};

static OL_ScrollbarSpecs sb_19 = {
    22.0, 9.0, 3.2, 69.2, 22.0, 22.0, 7.8, 3.6, 4.8, 1.6, 2.4, 1.6, 5.4, 8.6,
    1.6
};

struct OL_ButtonSpecs {
    float a_, b_, c_, d_, e_, f_;
};

static OL_ButtonSpecs bt_10 = { 18.0,  8.0, 0.8, 0.8, 0.8, 0.8 };
static OL_ButtonSpecs bt_12 = { 20.0, 10.0, 1.0, 1.0, 1.0, 1.0 };
static OL_ButtonSpecs bt_14 = { 22.0, 12.0, 1.2, 1.2, 1.2, 1.2 };
static OL_ButtonSpecs bt_19 = { 28.0, 16.0, 1.6, 1.6, 1.6, 1.6 };

struct OL_SettingSpecs {
    float a_, b_, c_, d_, e_;
};

static OL_SettingSpecs stg_10 = { 18.0,  8.0, 6.0,  5.0, 0.8 };
static OL_SettingSpecs stg_12 = { 20.0, 10.0, 6.0,  6.0, 1.0 };
static OL_SettingSpecs stg_14 = { 22.0, 12.0, 7.0,  8.0, 1.2 };
static OL_SettingSpecs stg_19 = { 27.0, 16.0, 8.0, 10.0, 1.6};

struct OL_CheckboxSpecs {
    float a_, b_, c_, d_, e_, f_;
};

static OL_CheckboxSpecs cbx_10 = { 11.0, 5.5, 7.0, 11.0, 3.0, 0.8 };
static OL_CheckboxSpecs cbx_12 = { 13.0, 6.5, 9.0, 13.0, 3.6, 1.0 };
static OL_CheckboxSpecs cbx_14 = { 15.0, 7.5, 11., 16.0, 4.2, 1.2 };
static OL_CheckboxSpecs cbx_19 = { 20.0, 10., 16., 21.0, 5.5, 1.6 };

/* Table B-28 */
struct OL_MenuButtonSpecs {
    float a_;
};

static OL_MenuButtonSpecs mb_10 = { 7.0 };
static OL_MenuButtonSpecs mb_12 = { 8.0 };
static OL_MenuButtonSpecs mb_14 = { 10.0 };
static OL_MenuButtonSpecs mb_19 = { 12.0 };

/* Table B-29 */
struct OL_MenuMarkSpecs {
    float a_, b_, c_;
};

static OL_MenuMarkSpecs mm_10 = { 0.8, 7.0, 0.8 };
static OL_MenuMarkSpecs mm_12 = { 1.0, 8.0, 1.0 };
static OL_MenuMarkSpecs mm_14 = { 1.2, 9.0, 1.2 };
static OL_MenuMarkSpecs mm_19 = { 1.6, 11., 1.6 };

/* Table B-38 */
struct OL_SliderSpecs {
    float a_, b_, c_, f_;
};

static OL_SliderSpecs sl_10 = { 13.0,  9.0, 0.8, 4.0 };
static OL_SliderSpecs sl_12 = { 15.0, 10.0, 1.0, 5.0 };
static OL_SliderSpecs sl_14 = { 17.0, 11.0, 1.2, 6.0 };
static OL_SliderSpecs sl_19 = { 21.0, 14.0, 1.6, 8.0 };

/* Table B-40 */

struct OL_TickSpecs {
    float a_;
};

static OL_TickSpecs tk_10 = { 5.0 };
static OL_TickSpecs tk_12 = { 6.0 };
static OL_TickSpecs tk_14 = { 7.0 };
static OL_TickSpecs tk_19 = { 9.0 };

/* Table B-44 */
struct OL_GaugeSpecs {
    float a_, b_, c_, d_, f_;
};

static OL_GaugeSpecs gg_10 = { 4, 10.0,  7.0, 0.8, 2.0 };
static OL_GaugeSpecs gg_12 = { 5, 11.0,  8.0, 1.0, 2.0 };
static OL_GaugeSpecs gg_14 = { 6, 13.2,  9.6, 1.2, 2.6 };
static OL_GaugeSpecs gg_19 = { 8, 17.6, 12.8, 1.6, 3.8 };

/* Table B-51 */
struct OL_ButtonMenuSpecs {
    float a_, b_, c_, i_;  // incomplete
};

static OL_ButtonMenuSpecs bm_10 = { 8.0, 14.0, 17.0, 25.0 };
static OL_ButtonMenuSpecs bm_12 = { 9.0, 16.0, 19.0, 27.0 };
static OL_ButtonMenuSpecs bm_14 = { 10.0, 17.0, 21.0, 32.0 };
static OL_ButtonMenuSpecs bm_19 = { 14.0, 22.0, 25.0, 37.0 };

/* Table B-52 */
struct OL_SubMenuSpecs {
    float a_, b_, e_, g_, h_;  // incomplete
};

static OL_SubMenuSpecs sm_10 = { 8.0, 17.0, 12.0, 5.0, 0.8 };
static OL_SubMenuSpecs sm_12 = { 9.0, 19.0, 14.0, 6.0, 1.0 };
static OL_SubMenuSpecs sm_14 = { 10., 21.0, 16.0, 7.0, 1.2 };
static OL_SubMenuSpecs sm_19 = { 14., 25.0, 20.0, 9.0, 1.6 };

static const CursorPattern question_pat = {
    0x0000, 0x0000, 0x0000, 0x7c00, 0xce00, 0x0600, 0x0600, 0x0c00,
    0x3000, 0x6000, 0x6000, 0x6000, 0x0000, 0x0000, 0x6000, 0x6000
};

static const CursorPattern question_mask = {
    0x0000, 0x0000, 0x7c00, 0xfe00, 0xff00, 0xcf00, 0x0f00, 0x3e00,
    0x7c00, 0xf000, 0xf000, 0xf000, 0xf000, 0xf000, 0xf000, 0xf000
};

static Cursor* question_mark_cursor;
static void init_ol_cursors() {
    question_mark_cursor = new Cursor(1, 15, question_pat, question_mask);
}

static const GlyphIndex normal = 0;
static const GlyphIndex backward_arrow_highlighted = 1;
static const GlyphIndex dragging = 2;
static const GlyphIndex forward_arrow_highlighted = 3;
static const GlyphIndex backward_arrow_dimmed = 4;
static const GlyphIndex forward_arrow_dimmed = 5;
static const GlyphIndex both_arrows_dimmed = 6;

typedef unsigned char OL_ButtonType;
typedef unsigned char OL_Direction;

class OL_Cable;
class OL_Channel;
class OL_Elevator;

class OL_Specs : public Resource {
public:
    OL_Specs(Style*);
    ~OL_Specs();

    const Font* font() const;
    
    Coord anchor_height() const;
    Coord anchor_rule() const;
    Coord anchor_to_side_gap() const;
    Coord anchor_width() const;

    Coord arrow_length() const;

    Coord button_default_ring_radius() const;
    Coord button_gap() const;
    Coord button_height() const;
    Coord button_radius() const;
    Coord button_rule_width() const;
    Coord button_vertical_margin() const;
    
    Coord cable_gap() const;
    Coord cable_width() const;

    Coord channel_cap_width() const;
    Coord channel_gap() const;
    Coord channel_highlight() const;
    Coord channel_length() const;
    Coord channel_rule() const;
    Coord channel_width() const;
    
    Coord checkbox_thickness() const;
    Coord checkbox_width() const;

    Coord checkmark_height() const;
    Coord checkmark_width() const;

    Coord dragbox_length() const;
    Coord dragbox_width() const;
    
    Coord elevator_length() const;
    Coord elevator_to_anchor_gap() const;
    Coord elevator_to_side_gap() const;
    Coord elevator_width() const;

    Coord gauge_cap_width() const;
    Coord gauge_end_width() const;
    Coord gauge_indent() const;
    Coord gauge_origin() const;
    Coord gauge_rule() const;
    Coord gauge_shimmer_gap() const;
    Coord gauge_shimmer_width() const;
    Coord gauge_width() const;
    Coord gauge_min_length() const;
    
    Coord menu_bmargin() const;
    Coord menu_button_height() const;
    Coord menu_hmargin() const;
    Coord menu_mark_gap() const;
    Coord menu_mark_height() const;
    Coord menu_mark_width() const;
    Coord menu_pushpin_gap() const;
    Coord menu_pushpin_height() const;
    Coord menu_tmargin() const;
    Coord menu_to_button_gap() const;

    Coord setting_default_ring_thickness() const;
    Coord setting_gap() const;
    Coord setting_height() const;
    Coord setting_horizontal_margin() const;
    Coord setting_thickness() const;
    Coord setting_vertical_margin() const;
    
    Coord shaft_length() const;
    Coord shaft_gap() const;

    Coord tick_length() const;

    Coord to_coord(float) const;
private:
    long points_;
    const Font* font_;
    Coord coords_per_point_;
    
    OL_ScrollbarSpecs* sb_;
    OL_ButtonSpecs* bt_;
    OL_SettingSpecs* stg_;
    OL_CheckboxSpecs* cbx_;
    OL_SliderSpecs* sl_;
    OL_TickSpecs* tk_;
    OL_GaugeSpecs* gg_;
    OL_MenuButtonSpecs* mb_;
    OL_ButtonMenuSpecs* bm_;
    OL_MenuMarkSpecs* mm_;
    OL_SubMenuSpecs* sm_;
};

class OL_AbbrevMenuButton : public Glyph {
public:
    OL_AbbrevMenuButton(
        const OLKit*, const OL_Specs*, DimensionName, TelltaleState*
    );

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
private:
    const OLKit* kit_;
    const OL_Specs* specs_;
    DimensionName dimension_;
    TelltaleState* state_;
    Coord width_, height_, lgap_, tgap_;
};

/*
 *  Cable anchor for Open Look scroll bars
 *
 *  Pane-splitting functionality not implemented
 */

class OL_Anchor : public Glyph {
public:
    OL_Anchor(
        const OLKit*, Coord width, Coord height, Coord thick, TelltaleState*);
    virtual ~OL_Anchor();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
private:
    const OLKit* kit_;
    Coord width_;
    Coord height_;
    Coord thickness_;
    TelltaleState* state_;
};

class OL_Button : public MonoGlyph, public Observer {
public:
    enum { PushButton, MenuButton, DefaultButton };
    
    OL_Button(
        const OLKit*, const OL_Specs*,
        Glyph*, TelltaleState*, OL_ButtonType, boolean extend = true);
    virtual ~OL_Button();

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int, Hit&);
protected:
    virtual void path (Canvas*, int, Coord, Coord, Coord, Coord) const;
    virtual void top_path(
	Canvas*, int inset, Coord l, Coord b, Coord r, Coord t
    ) const;
    virtual void bottom_path(
	Canvas*, int inset, Coord l, Coord b, Coord r, Coord t
    ) const;
    virtual void fill(Canvas*, const Allocation&, const Color*) const;
    virtual void draw_background(Canvas*, const Allocation&) const;
    virtual void draw_frame(Canvas*, const Allocation&) const;
private:
    const OLKit* kit_;
    const OL_Specs* specs_;
    TelltaleState* state_;
    OL_ButtonType type_;
    Brush* brush_;
};

class OL_CheckMark : public Glyph {
public:
    OL_CheckMark(const OLKit*, TelltaleState*, const OL_Specs*);
    virtual ~OL_CheckMark();
    
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
private:
    const OLKit* kit_;
    const TelltaleState* state_;
    const OL_Specs* specs_;
    const Font* font_;
    long code_;
    Coord width_, height_;
};

class OL_Dragbox : public Glyph {
public:
    OL_Dragbox(const OLKit*, const OL_Specs*, DimensionName);
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void undraw();
    
    boolean inside(const Event&) const;
    boolean less_than(const Event&) const;
    boolean greater_than(const Event&) const;

    void press(const Event&);
    void release(const Event&);
    boolean dragging() { return dragging_; }
private:
    const OLKit* kit_;	
    const OL_Specs* specs_;
    DimensionName dimension_;
    boolean dragging_;
    Canvas* canvas_;
    Extension extension_;
};

class OL_ElevatorGlyph : public Glyph {
public:
    OL_ElevatorGlyph(const OLKit*, const OL_Specs*, DimensionName);
    virtual ~OL_ElevatorGlyph();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void undraw();
    virtual void flip_to(GlyphIndex);
    
    virtual boolean inside(const Event&);
    virtual boolean backward_arrow_contains(Coord x, Coord y) const;
    virtual boolean forward_arrow_contains(Coord x, Coord y) const;

    virtual boolean less_than(const Event&) const;
    virtual boolean greater_than(const Event&) const;

    virtual float forward_arrow_center() const;
    virtual float backward_arrow_center() const;

    virtual GlyphIndex index() const;
protected:
    const OLKit* kit_;
    const OL_Specs* specs_;
    const DimensionName dimension_;
    const Font* font_;
    Canvas* canvas_;
    Extension extension_;
    GlyphIndex index_;
};

class OL_Indicator : public Glyph {
public:
    OL_Indicator(const OLKit*, const OL_Specs*, DimensionName);

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
protected:
    const OLKit* kit_;
    const OL_Specs* specs_;
    const DimensionName dimension_;
};

class OL_Frame : public BevelFrame {
public:
    OL_Frame(const OLKit*, Glyph*, TelltaleState*, Coord thickness);
    virtual ~OL_Frame();

    virtual void draw(Canvas*, const Allocation&) const;
    virtual void draw_background(Canvas*, const Allocation&) const;
    virtual void draw_frame(Canvas*, const Allocation&, Coord thickness) const;
protected:
    const OLKit* kit_;
    TelltaleState* state_;
    Coord thickness_;
};

class OL_Gauge : public Glyph, public Observer {
public:
    OL_Gauge(const OLKit*, const OL_Specs*, DimensionName, Adjustable*,Patch*);
    virtual ~OL_Gauge();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;

    void update(Observable*);
    void disconnect(Observable*);
private:
    const OLKit* kit_;
    const OL_Specs* specs_;
    const DimensionName dimension_;
    Adjustable* adjustable_;
    Patch* patch_;
};

class OL_MenuMark : public Glyph {
public:
    OL_MenuMark(const OLKit*, const OL_Specs*, boolean is_pulldown);
    virtual ~OL_MenuMark();
    
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
private:
    const OLKit* kit_;
    const OL_Specs* specs_;
    const Font* font_;
    long fill_code_, ul_code_, lr_code_;
    Coord fill_width_, fill_height_;
    Coord ul_width_, ul_height_;
    Coord lr_width_, lr_height_;
};

typedef unsigned int OL_MoverFlags;

class OL_Mover : public Glyph {
public:
    enum { up, down, left, right };
    
    OL_Mover(const OLKit*, const OL_Specs*, OL_MoverFlags, TelltaleState*);
    virtual ~OL_Mover();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
private:
    const OLKit* kit_;
    const OL_Specs* specs_;
    const TelltaleState* state_;
    const Font* font_;
    Coord height_, width_;
    Coord box_ul_, fill_, box_lr_;
};

class OL_Pushpin : public Action {
public:
    OL_Pushpin(TelltaleState*, const Window* unpinned, Window* pinned);
    virtual ~OL_Pushpin();

    virtual void execute();
private:
    TelltaleState* state_;
    const Window* unpinned_;
    Window* pinned_;
    boolean placed_;
};

class OL_PushpinLook : public Glyph {
public:
    OL_PushpinLook(const OLKit*, const OL_Specs*, TelltaleState*);
    virtual ~OL_PushpinLook();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
private:
    virtual void draw_pinned(Canvas*, const Allocation&) const;
    virtual void draw_unpinned(Canvas*, const Allocation&) const;
private:
    const OLKit* kit_;
    const OL_Specs* specs_;
    TelltaleState* state_;
    Coord width_;
    Coord height_;
};

class OL_Scrollbar : public ActiveHandler, public Observer {
public:
    OL_Scrollbar(
	const OL_Specs*, Adjustable*, Style*, DimensionName,
        OL_Cable*, OL_Elevator*
    );
    virtual ~OL_Scrollbar();

    virtual void allocation_changed(Canvas*, const Allocation&);
    
    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);

    virtual void update(Observable*);
    virtual void disconnect(Observable*);
protected:
    const OL_Specs* specs_;
    Adjustable* adjustable_;
    const DimensionName dimension_;
    OL_Cable* cable_;
    OL_Elevator* elevator_;
    PolyGlyph* overlay_;
};

// Both exclusive and non-exclusive settings use OL_Setting.  Non-exclusive
// settings are responsible to put margins between each other.

class OL_Setting : public OL_Frame, public Observer {
public:
    OL_Setting(
	const OLKit*, Glyph*, TelltaleState*,
	const OL_Specs*, boolean is_default = false
    );
    virtual ~OL_Setting();

    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int, Hit&);
    virtual void draw_background(Canvas*, const Allocation&) const;
    virtual void draw_frame(Canvas*, const Allocation&, Coord thickness) const;
protected:
    const OL_Specs* specs_;
    boolean is_default_;
    Brush* brush_;
};

class OL_Slider : public ActiveHandler, public Observer {
public:
    OL_Slider(
	const OL_Specs*, Adjustable*, Style*, DimensionName,
	OL_Channel*, OL_Dragbox*
    );
    virtual ~OL_Slider();

    virtual void allocation_changed(Canvas* c, const Allocation&);
    
    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);
    
    virtual void update(Observable*);
    virtual void disconnect(Observable*);
protected:
    const OL_Specs* specs_;
    Adjustable* adjustable_;
    const DimensionName dimension_;
    OL_Channel* channel_;
    OL_Dragbox* box_;
    PolyGlyph* overlay_;
};

class OL_Stepper : public MonoGlyph {
public:
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void redraw();

    virtual void press(const Event&);
    virtual void release(const Event&);
    virtual boolean grabbing() const;
    
    virtual void tick(long, long);
protected:
    OL_Stepper(const OL_Specs*, Adjustable*, DimensionName, Patch* thumb);
    virtual ~OL_Stepper();

    virtual Requirement requirement_x() const = 0;
    virtual Requirement requirement_y() const = 0;

    virtual void allocate_thumb(const Allocation&, Allocation&) = 0;
    virtual void allot_major_axis(
        const Allotment&, Coord length, Coord gap, Allotment&) const;
    virtual float percent_visible() const;
    virtual Coord
        thumb_position(const Allotment&, Coord thumb_length, Coord gap) const;
    virtual void
        allot_minor_axis(const Allotment&, Coord width, Allotment&) const;

    virtual void adjust_pointer(Coord x, Coord y) const = 0;
    virtual void move_pointer(Coord x, Coord y) const;
    virtual void save_pointer(const Event&);

    virtual void press_select();
    virtual void release_select();
    virtual void press_undefined();
    virtual void release_undefined();
    
    virtual boolean is_forward(Coord x, Coord y) const = 0;
    virtual boolean is_backward(Coord x, Coord y) const = 0;
    virtual void step_forward() = 0;
    virtual void step_backward() = 0;
    
    virtual boolean at_start() const;
    virtual boolean at_end() const;

    virtual void start_stepping();
    virtual void next_step();
    virtual void stop_stepping();
protected:
    const OL_Specs* specs_;
    Adjustable* adjustable_;
    const DimensionName dimension_;
    Patch* thumb_;
    Canvas* canvas_;
    Allocation allocation_;
    Allocation thumb_allocation_;
    Coord pointer_x_, pointer_y_;
    boolean forward_;
    boolean backward_;
    boolean grabbing_;
    float initial_delay_;
    float interval_;
    IOHandler* timer_;
    Cursor* saved_cursor_;
};

class OL_Tick : public Glyph {
public:
    OL_Tick(const OLKit*, const OL_Specs*, DimensionName);

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
private:
    const OLKit* kit_;
    const OL_Specs* specs_;
    DimensionName dimension_;
};

class OL_ToLimit : public Action {
public:
    enum { start, end };
    OL_ToLimit(Adjustable*, DimensionName, OL_Direction);

    virtual void execute();
private:
    Adjustable* adjustable_;
    const DimensionName dimension_;
    OL_Direction direction_;
};

class OL_Cable : public OL_Stepper {
public:
    OL_Cable(const OL_Specs*, Adjustable*, DimensionName, OL_Indicator*);
    virtual ~OL_Cable();

    virtual void draw(Canvas*, const Allocation&) const;
protected:
    virtual Requirement requirement_x() const;
    virtual Requirement requirement_y() const;
    virtual void allocate_thumb(const Allocation&, Allocation&);
    virtual void adjust_pointer(Coord x, Coord y) const;
    virtual boolean is_forward(Coord x, Coord y) const;
    virtual boolean is_backward(Coord x, Coord y) const;
    virtual void step_forward();
    virtual void step_backward();

    virtual Coord elevator_min() const;
    virtual Coord elevator_max() const;
private:
    OL_Indicator* indicator_;
    const Color* gray_;
};

class OL_CheckBox : public OL_Frame {
public:
    OL_CheckBox(const OLKit*, TelltaleState*, const OL_Specs*);

    virtual void pick(Canvas*, const Allocation&, int, Hit&);
    virtual void draw_background(Canvas*, const Allocation&) const;
};

class OL_Channel : public OL_Stepper {
public:
    OL_Channel(
        const OLKit*, const OL_Specs*, Adjustable*, DimensionName, OL_Dragbox*
    );
    virtual void draw(Canvas*, const Allocation&) const;
    void drag_to(const Event&);
protected:
    virtual Requirement requirement_x() const;
    virtual Requirement requirement_y() const;
    virtual void allocate_thumb(const Allocation&, Allocation&);
    virtual void adjust_pointer(Coord x, Coord y) const;
    virtual Coord thumb_min() const;
    virtual Coord thumb_max() const;
    virtual boolean is_forward(Coord x, Coord y) const;
    virtual boolean is_backward(Coord x, Coord y) const;
    virtual void step_forward();
    virtual void step_backward();
private:
    const OLKit* kit_;
};

class OL_Elevator : public OL_Stepper {
public:
    OL_Elevator(
        const OL_Specs*, Adjustable*, DimensionName, OL_ElevatorGlyph*
    );

    virtual void draw(Canvas*, const Allocation&) const;
    
    virtual boolean inside(const Event&) const;
    virtual boolean less_than(const Event&) const;
    virtual boolean greater_than(const Event&) const;

    virtual void press(const Event&);
    virtual void drag(const Event&);
protected:
    virtual Requirement requirement_x() const;
    virtual Requirement requirement_y() const;
    virtual void allocate_thumb(const Allocation&, Allocation&);
    virtual void adjust_pointer(Coord x, Coord y) const;
    virtual boolean is_forward(Coord x, Coord y) const;
    virtual boolean is_backward(Coord x, Coord y) const;
    virtual void step_forward();
    virtual void step_backward();
  
    virtual void drag_to(const Event&);
    virtual void release_select();
    
    virtual void adjust_for_dimming();
private:
    OL_ElevatorGlyph* glyph_;
    boolean dragging_;
};

class OLKitImpl {
private:
    friend OLKit;
    
    OLKitImpl(OLKit*);
    ~OLKitImpl();

    Glyph* cable_anchor(Adjustable*, OL_Direction, DimensionName) const;
    Glyph* scrollbar(Adjustable*, DimensionName) const;
    Coord frame_thickness() const;

    const Color* color(
        Display*, const char* name, const char* alias,
        ColorIntensity r, ColorIntensity g, ColorIntensity b, float alpha
    );
private:
    OLKit* kit_;
    LayoutKit* layout_;
    Style* style_;
    OL_Specs* specs_;
    Coord frame_thickness_;
    const Color* white_;
    const Color* black_;
    const Color* bg1_;
    const Color* bg2_;
    const Color* bg3_;
    const Color* inactive_;
    const Color* busy_;
};

OL_Specs::OL_Specs (Style* style) : Resource(), points_(12) {
    style->find_attribute("olglyph", points_);
    char fontname[20];
    sprintf(fontname, "olglyph-%d", points_);
    font_ = Font::lookup(fontname);
    if (font_ == nil) {
	fprintf(stderr, "font for OLKit not found: \'%s\'\n", fontname);
	fflush(stderr);
    }
    Resource::ref(font_);

    coords_per_point_ = Session::instance()->default_display()->to_coord(1);
    
    switch(points_) {
    case 10:
	sb_ = &sb_10;
	bt_ = &bt_10;
	stg_ = &stg_10;
	cbx_ = &cbx_10;
	sl_ = &sl_10;
	tk_ = &tk_10;
	gg_ = &gg_10;
	mb_ = &mb_10;
	bm_ = &bm_10;
	mm_ = &mm_10;
	sm_ = &sm_10;
	break;
    default:
    case 12:
	sb_ = &sb_12;
	bt_ = &bt_12;
	stg_ = &stg_12;
	cbx_ = &cbx_12;
	sl_ = &sl_12;
	tk_ = &tk_12;
	gg_ = &gg_12;
	mb_ = &mb_12;
	bm_ = &bm_12;
	mm_ = &mm_12;
	sm_ = &sm_12;
	break;
    case 14:
	sb_ = &sb_14;
	bt_ = &bt_14;
	stg_ = &stg_14;
	cbx_ = &cbx_14;
	sl_ = &sl_14;
	tk_ = &tk_14;
	gg_ = &gg_14;
	mb_ = &mb_14;
	bm_ = &bm_14;
	mm_ = &mm_14;
	sm_ = &sm_14;
	break;
    case 19:
	sb_ = &sb_19;
	bt_ = &bt_19;
	stg_ = &stg_19;
	cbx_ = &cbx_19;
	sl_ = &sl_19;
	tk_ = &tk_19;
	gg_ = &gg_19;
	mb_ = &mb_19;
	bm_ = &bm_19;
	mm_ = &mm_19;
	sm_ = &sm_19;
	break;
    }
}

OL_Specs::~OL_Specs() {
    Resource::unref(font_); 
}

inline Coord OL_Specs::to_coord(float point) const {
    return coords_per_point_ * point;
}

inline const Font* OL_Specs::font() const {
    return font_;
}

inline Coord OL_Specs::anchor_height() const {
    return to_coord(sb_->b_);
}

inline Coord OL_Specs::anchor_rule() const {
    return to_coord(sb_->r_);
}

inline Coord OL_Specs::anchor_to_side_gap() const {
    return to_coord(sb_->h_);
}

inline Coord OL_Specs::anchor_width() const {
    return to_coord(sb_->a_ - sb_->r_); // "- r" compensates for shadow
}

inline Coord OL_Specs::arrow_length() const {
    return to_coord(sb_->e_);
}

inline Coord OL_Specs::button_default_ring_radius() const {
    return to_coord(bt_->a_ - 2.0) * 0.5;
}

inline Coord OL_Specs::button_gap() const {
    return to_coord(bt_->c_);
}

inline Coord OL_Specs::button_height() const {
    return to_coord(bt_->a_);
}

inline Coord OL_Specs::button_radius() const {
    return to_coord(bt_->a_) * 0.5;
}

inline Coord OL_Specs::button_vertical_margin() const {
    return to_coord(bt_->a_) * 0.20;  // estimated 20%
}

inline Coord OL_Specs::button_rule_width() const {
    return to_coord(1.0);
}

inline Coord OL_Specs::cable_gap() const {
    return to_coord(sb_->j_);
}

inline Coord OL_Specs::cable_width() const {
    return to_coord(sb_->a_ - sb_->p_ - sb_->p_);
}

inline Coord OL_Specs::channel_cap_width() const {
    return to_coord(sl_->f_ * 0.5);
}

inline Coord OL_Specs::channel_gap() const {
    return to_coord(sl_->c_);
}

inline Coord OL_Specs::channel_highlight() const {
    return channel_gap();  // guessing
}

inline Coord OL_Specs::channel_length() const {
    return to_coord(sl_->c_ + sl_->b_ + sl_->c_);
}

inline Coord OL_Specs::channel_rule() const {
    return to_coord(sl_->c_);
}

inline Coord OL_Specs::channel_width() const {
    return to_coord(sl_->f_);
}

inline Coord OL_Specs::checkbox_thickness() const {
    return to_coord(cbx_->f_);
}
 
inline Coord OL_Specs::checkbox_width() const {
    return to_coord(cbx_->a_);
}
 
inline Coord OL_Specs::checkmark_height() const {
    return to_coord(cbx_->d_);
}
 
inline Coord OL_Specs::checkmark_width() const {
    return to_coord(cbx_->b_ + cbx_->c_ - cbx_->f_);
}

inline Coord OL_Specs::dragbox_length() const {
    return to_coord(sl_->b_);
}

inline Coord OL_Specs::dragbox_width() const {
    return to_coord(sl_->a_);
}

inline Coord OL_Specs::elevator_length() const {
    return to_coord(sb_->d_);
}

inline Coord OL_Specs::elevator_width() const {
    return to_coord(sb_->a_ - sb_->r_);  // "- r" compensates for shadow
}

inline Coord OL_Specs::elevator_to_anchor_gap() const {
    return to_coord(sb_->c_);
}

inline Coord OL_Specs::elevator_to_side_gap() const {
    return to_coord(sb_->h_);
}

inline Coord OL_Specs::gauge_cap_width() const {
    return to_coord(gg_->a_ / 2.0);
}

inline Coord OL_Specs::gauge_end_width() const {
    return to_coord(gg_->b_ / 2.0);
}

inline Coord OL_Specs::gauge_indent() const {
    return to_coord((gg_->b_ - gg_->a_) / 2.0);
}

inline Coord OL_Specs::gauge_origin() const {
    return to_coord(gg_->c_);
}

inline Coord OL_Specs::gauge_rule() const {
    return to_coord(gg_->d_);
}

inline Coord OL_Specs::gauge_shimmer_gap() const {
    return gauge_rule();  // guessing
}

inline Coord OL_Specs::gauge_shimmer_width() const {
    return gauge_rule();  // guessing
}

inline Coord OL_Specs::gauge_width() const {
    return to_coord(gg_->b_);
}

inline Coord OL_Specs::gauge_min_length() const {
    return gauge_origin() + gauge_origin();
}

inline Coord OL_Specs::menu_bmargin () const {
    return to_coord(0.7 * sm_->e_);  // 0.7 guessed
}

inline Coord OL_Specs::menu_button_height() const {
    return to_coord(bm_->c_);
}

inline Coord OL_Specs::menu_hmargin() const {
    return to_coord(bm_->a_);
}

inline Coord OL_Specs::menu_mark_gap() const {
    return to_coord(mb_->a_);
}

inline Coord OL_Specs::menu_mark_height() const {
    return to_coord(mm_->b_);
}

inline Coord OL_Specs::menu_mark_width() const {
    const float tangent = 0.57735;
    Coord side = to_coord(tangent * mm_->b_);
    return side + side;
}

inline Coord OL_Specs::menu_pushpin_gap() const {
    return to_coord(bm_->i_ - bm_->c_);
}

inline Coord OL_Specs::menu_pushpin_height() const {
    return to_coord(bm_->b_);
}

inline Coord OL_Specs::menu_tmargin () const {
    return to_coord(0.3 * sm_->b_);  // 0.3 guessed
}

inline Coord OL_Specs::menu_to_button_gap() const {
    return to_coord(sm_->h_);
}

inline Coord OL_Specs::setting_default_ring_thickness() const {
    return to_coord(stg_->e_);
}

inline Coord OL_Specs::setting_gap() const {
    return to_coord(stg_->e_);
}

inline Coord OL_Specs::setting_height() const {
    return to_coord(stg_->a_);
}

inline Coord OL_Specs::setting_thickness() const {
    return to_coord(stg_->e_);
}

inline Coord OL_Specs::setting_vertical_margin() const {
    return to_coord(stg_->a_ * 0.20);  // estimated 20%
}

inline Coord OL_Specs::setting_horizontal_margin() const {
    return to_coord(stg_->d_);
}

inline Coord OL_Specs::shaft_length() const {
    return to_coord(sb_->c_ + sb_->d_ + sb_->c_);
}

inline Coord OL_Specs::shaft_gap() const {
    return to_coord(sb_->h_);
}

inline Coord OL_Specs::tick_length() const {
    return to_coord(tk_->a_);
}

OL_AbbrevMenuButton::OL_AbbrevMenuButton(
    const OLKit* k, const OL_Specs* s, DimensionName d, TelltaleState* state
) : kit_(k), specs_(s), dimension_(d), state_(state)
{
    const Font* f = specs_->font();
    if (f != nil) {
	FontBoundingBox box;
	f->char_bbox(ABBREV_MENU_UL, box);
	width_ = box.left_bearing() + box.right_bearing();
	height_ = box.ascent() + box.descent();
	if (d == Dimension_X) {
	    f->char_bbox(VERT_MENU_MARK_UL, box);
	} else {
	    f->char_bbox(HORIZ_MENU_MARK_UL, box);
	}
	lgap_ = (width_ - (box.left_bearing() + box.right_bearing())) * 0.5;
	tgap_ = (height_ - (box.ascent() + box.descent())) * 0.5;
    } else {
	width_ = height_ = 16;  // filler
    }
}

void OL_AbbrevMenuButton::request(Requisition& r) const {
    Requirement height(height_);
    Requirement width(width_);
    r.require_x(width);
    r.require_y(height);
}

void OL_AbbrevMenuButton::allocate(
    Canvas* c, const Allocation& a, Extension& e
) {
    e.set(c, a);
}

void OL_AbbrevMenuButton::draw(Canvas* c, const Allocation& a) const {
    const Font* f = specs_->font();
    if (f != nil) {
	Coord l = a.left(), t = a.top();
	const Color* white = kit_->white(), *bg2 = kit_->bg2(),
	    *bg3 = kit_->bg3();
	const Color* ul, *lr, *fill;
	
	if (state_->test(TelltaleState::is_active)) {
	    ul = bg3;
	    lr = white;
	    fill = bg2;
	} else {
	    ul = white;
	    lr = bg3;
	    fill = kit_->bg1();
	}

	c->character(f, ABBREV_MENU_UL, width_, ul, l, t);
	c->character(f, ABBREV_MENU_LR, width_, lr, l, t);
	c->character(f, ABBREV_MENU_FILL, width_, fill, l, t);

	Coord ll = l + lgap_, tt = t - tgap_;
	if (dimension_ == Dimension_X) {
	    c->character(f, VERT_MENU_MARK_UL, width_, bg3, ll, tt);
	    c->character(f, VERT_MENU_MARK_LR, width_, white, ll, tt);
	    c->character(f, VERT_MENU_MARK_FILL, width_, bg2, ll, tt);
	} else {
	    c->character(f, HORIZ_MENU_MARK_UL, width_, bg3, ll, tt);
	    c->character(f, HORIZ_MENU_MARK_LR, width_, white, ll, tt);
	    c->character(f, HORIZ_MENU_MARK_FILL, width_, bg2, ll, tt);
	}

	if (state_->test(TelltaleState::is_running)) {
	    c->character(f, ABBREV_MENU_FILL, width_, kit_->busy(), l, t);
	} else if (!state_->test(TelltaleState::is_enabled)) {
	    c->character(f, ABBREV_MENU_FILL, width_, kit_->inactive(), l, t);
	}
    }
}

OL_Anchor::OL_Anchor(
    const OLKit* k, Coord w, Coord h, Coord t, TelltaleState* s
) : Glyph(), kit_(k), width_(w), height_(h), thickness_(t), state_(s) {
    Resource::ref(state_);
}

OL_Anchor::~OL_Anchor() {
    Resource::unref(state_); 
}

void OL_Anchor::allocate(Canvas* c, const Allocation& a, Extension& e) {
    e.set(c, a);
}

void OL_Anchor::request(Requisition& r) const {
    Requirement x(width_, 0, 0, 0.5);
    Requirement y(height_, 0, 0, 0.5);
    r.require_x(x);
    r.require_y(y);
}

void OL_Anchor::draw(Canvas* c, const Allocation& a) const {
    const Color* upper_left, *lower_right, *fill;
    if (state_->test(TelltaleState::is_active)) {
	upper_left = kit_->bg3();
	fill = kit_->bg2();
	lower_right = kit_->white();
    } else {
	upper_left = kit_->white();
	fill = kit_->bg1();
	lower_right = kit_->bg3();
    }
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    Bevel::rect(c, upper_left, fill, lower_right, thickness_, l, b, r, t);
}

OL_Button::OL_Button(
    const OLKit* k, const OL_Specs* s,
    Glyph* g, TelltaleState* t, OL_ButtonType type, boolean extend
) : MonoGlyph(nil), Observer(), kit_(k), specs_(s), state_(t), type_(type),
    brush_(new Brush(s->button_rule_width()))
{
    Resource::ref(brush_); 
    Resource::ref(state_);

    Requisition r;
    g->request(r);

    Coord min_width = 72.0;
    k->style()->find_attribute("minimumWidth", min_width);
    Requirement& rx = r.x_requirement();
    Coord radius = s->button_radius();
    Coord width = radius + rx.natural() + radius;
    Coord rm = 0;
    if (extend && width < min_width) {
	rm = min_width - width;
    }
    Coord vm = (s->button_height() - r.y_requirement().natural()) * 0.5;
    if (vm < s->button_vertical_margin()) {
	vm = s->button_vertical_margin();
    }

    body(LayoutKit::instance()->margin(g, radius, rm + radius, vm, vm));
}

OL_Button::~OL_Button() {
    Resource::unref(state_);    
    Resource::unref(brush_);    
}

void OL_Button::allocate(Canvas* c, const Allocation& a, Extension& e) {
    MonoGlyph::allocate(c, a, e); 
    e.set(c, a);
}

void OL_Button::draw(Canvas* c, const Allocation& a) const {
    draw_background(c, a);
    MonoGlyph::draw(c, a);

    if (!state_->test(TelltaleState::is_enabled)) {
	fill(c, a, kit_->inactive());
    }

    if (type_ != MenuButton || state_->test(TelltaleState::is_active)) {
	draw_frame(c, a);
    }

    if (state_->test(TelltaleState::is_running)) {
	fill(c, a, kit_->busy());
    }

    if (type_ == DefaultButton && !state_->test(TelltaleState::is_active)) {
	const OL_Specs& s = *specs_;
	const Color* color = kit_->bg3();
	Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
	
	path(c, 4, l, b, r, t);
	c->stroke(color, brush_);
    }
}

void OL_Button::pick(Canvas*, const Allocation& a, int depth, Hit& h) {
    Coord x = h.left();
    Coord y = h.bottom();
    if (a.left() <= x && x < a.right() && a.bottom() <= y && y < a.top()) {
	h.target(depth, this, 0);
    }
}

void OL_Button::draw_background(Canvas* c, const Allocation& a) const {
    const Color* background;
    if (state_->test(TelltaleState::is_active)) {
	background = kit_->bg2();
    } else {
	background = kit_->bg1();
    }
    fill(c, a, background);
}

void OL_Button::fill(
    Canvas* c, const Allocation& a, const Color* color
) const {
    Coord w = brush_->width();
    Coord l = a.left()+w, b = a.bottom()+w, r = a.right()-w, t = a.top()-w;
    
    path(c, 1, l, b, r, t);
    c->close_path();
    c->fill(color);
}

void OL_Button::draw_frame(Canvas* c, const Allocation& a) const {
    const Color* top, *bottom;
    if (state_->test(TelltaleState::is_active)) {
	top = kit_->bg3();
	bottom = kit_->white();
    } else {
	top = kit_->white();
	bottom = kit_->bg3();
    }
    Coord w = brush_->width();
    Coord l = a.left()+w, b = a.bottom()+w, r = a.right()-w, t = a.top()-w;

    top_path(c, 1, l, b, r, t);
    c->stroke(top, brush_);

    bottom_path(c, 1, l, b, r, t);
    c->stroke(bottom, brush_);
}

/*
 *  Draw button end caps using a polygon.  Numbers determined experimentally.
 *  Looks good for font sizes 10, 12, 14, bad for 19.
 */
static const int l1 = 2;
static const int l2 = 3;
static const int l3 = 6;

void OL_Button::path (
    Canvas* canvas, int inset, Coord l, Coord b, Coord r, Coord t
) const {
    Coord d0 = specs_->to_coord(inset);
    Coord d1 = specs_->to_coord(inset + l1);
    Coord d2 = specs_->to_coord(inset + l2);
    Coord d3 = specs_->to_coord(inset + l3);

    canvas->new_path();

    canvas->move_to(l + d1, b + d2);
    canvas->line_to(l + d0, b + d3);
    canvas->line_to(l + d0, t - d3);
    canvas->line_to(l + d1, t - d2);

    canvas->line_to(l + d2, t - d1);
    canvas->line_to(l + d3, t - d0);
    canvas->line_to(r - d3, t - d0);
    canvas->line_to(r - d2, t - d1);

    canvas->line_to(r - d1, t - d2);
    canvas->line_to(r - d0, t - d3);
    canvas->line_to(r - d0, b + d3);
    canvas->line_to(r - d1, b + d2);

    canvas->line_to(r - d2, b + d1);
    canvas->line_to(r - d3, b + d0);
    canvas->line_to(l + d3, b + d0);
    canvas->line_to(l + d2, b + d1);

    canvas->close_path();
}

void OL_Button::top_path(
    Canvas* c, int inset, Coord l, Coord b, Coord r, Coord t
) const {
    Coord d0 = specs_->to_coord(inset);
    Coord d1 = specs_->to_coord(inset + l1);
    Coord d2 = specs_->to_coord(inset + l2);
    Coord d3 = specs_->to_coord(inset + l3);

    c->move_to(l + d1, b + d2);
    c->line_to(l + d0, b + d3);
    c->line_to(l + d0, t - d3);
    c->line_to(l + d1, t - d2);

    c->line_to(l + d2, t - d1);
    c->line_to(l + d3, t - d0);
    c->line_to(r - d3, t - d0);
    c->line_to(r - d2, t - d1);
}

void OL_Button::bottom_path(
    Canvas* c, int inset, Coord l, Coord b, Coord r, Coord t
) const {
    Coord d0 = specs_->to_coord(inset);
    Coord d1 = specs_->to_coord(inset + l1);
    Coord d2 = specs_->to_coord(inset + l2);
    Coord d3 = specs_->to_coord(inset + l3);

    c->move_to(r - d1, t - d2);
    c->line_to(r - d0, t - d3);
    c->line_to(r - d0, b + d3);
    c->line_to(r - d1, b + d2);

    c->line_to(r - d2, b + d1);
    c->line_to(r - d3, b + d0);
    c->line_to(l + d3, b + d0);
    c->line_to(l + d2, b + d1);
}

OL_CheckMark::OL_CheckMark(
    const OLKit* k, TelltaleState* t, const OL_Specs* s
) : Glyph(), kit_(k), state_(t), specs_(s),
    font_(s->font()), code_(CHECK_MARK), width_(0), height_(0)
{
    Resource::ref(state_);
    Resource::ref(font_);
 
    if (font_ != nil) {
	FontBoundingBox box;
	font_->char_bbox(code_, box);
	width_ = box.width();
	height_ = box.ascent() + box.descent();
    }
}

OL_CheckMark::~OL_CheckMark() {
    Resource::unref(state_); 
    Resource::unref(font_); 
}

void OL_CheckMark::request(Requisition& r) const {
    Coord rule = specs_->checkbox_thickness();
    Coord side = specs_->checkbox_width() - (rule + rule);
    r.x_requirement().natural(side);
    r.y_requirement().natural(side);
}

void OL_CheckMark::allocate(Canvas* c, const Allocation& a, Extension& e) {
    Coord left = a.left(), bottom = a.bottom();
    e.set_xy(c, left, bottom, left + width_, bottom + height_);
}

void OL_CheckMark::draw(Canvas* c, const Allocation& a) const {
    if (state_->test(TelltaleState::is_chosen)) {
	if (font_ != nil) {
	    Coord l = a.left(), b = a.bottom();
            c->character(font_, code_, width_, kit_->black(), l, b + height_);
	}
    }
}

OL_ElevatorGlyph::OL_ElevatorGlyph(
    const OLKit* k, const OL_Specs* s, DimensionName d
) : Glyph(), kit_(k), specs_(s), dimension_(d),
    font_(s->font()),
    canvas_(nil),
    extension_(),
    index_(normal)
{
    Resource::ref(font_); 
}

OL_ElevatorGlyph::~OL_ElevatorGlyph() {
    Resource::unref(font_); 
}

void OL_ElevatorGlyph::request(Requisition& r) const {
    DimensionName major_d = dimension_;
    DimensionName minor_d = (major_d == Dimension_X)? Dimension_Y: Dimension_X;

    r.requirement(major_d).natural(specs_->elevator_length());
    r.requirement(minor_d).natural(specs_->elevator_width());
}

/*
 *  Return an extension that extends beyond the actual drawing by 1 point
 *  to eliminate roundoff errors in redraw which may leave doodoos.
 */
void OL_ElevatorGlyph::allocate(Canvas* c, const Allocation& a, Extension& e) {
    canvas_ = c;
    extension_.set(c, a);
    e.set_xy(c, a.left() - 1, a.bottom() - 1, a.right() + 1, a.top() + 1);
}

void OL_ElevatorGlyph::draw(Canvas* c, const Allocation& a) const {
    boolean h = dimension_ == Dimension_X;
    unsigned char ul        = h ? HORIZ_SB_UL            : VERT_SB_UL;
    unsigned char lr        = h ? HORIZ_SB_LR            : VERT_SB_LR;
    unsigned char box_ul    = h ? HORIZ_SB_BOX_UL        : VERT_SB_BOX_UL;
    unsigned char box1_fill = h ? HORIZ_SB_LEFTBOX_FILL  : VERT_SB_BOTBOX_FILL;
    unsigned char box_lr    = h ? HORIZ_SB_BOX_LR        : VERT_SB_BOX_LR;
    unsigned char box2_fill = h ? HORIZ_SB_RIGHTBOX_FILL : VERT_SB_TOPBOX_FILL;
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    Coord width = specs_->elevator_width();
    Coord arrow_length = specs_->arrow_length();
    const Font* f = font_;
    const Color* white = kit_->white();
    const Color* bg1 = kit_->bg1(), *bg2 = kit_->bg2(), *bg3 = kit_->bg3();
    const Color* dim = kit_->inactive();

    c->fill_rect(l, b, r, t, bg1);
    if (f != nil) {
	c->character(f, ul, width, white, l, t);
	c->character(f, lr, width, bg3, l, t);
    }
    
    switch (index_) {
    case backward_arrow_highlighted:
 	if (!h) {
 	    t -= arrow_length + arrow_length;
 	}
	if (f != nil) {
	    c->character(f, box_ul, arrow_length, bg3, l, t);
	    c->character(f, box1_fill, arrow_length, bg2, l, t);
	    c->character(f, box_lr, width, white, l, t);
	}
	break;
    case dragging:
	if (h) {
	    l += arrow_length;
	} else {
	    t -= arrow_length;
	}
	if (f != nil) {
	    c->character(f, DIMPLE_UL, arrow_length, bg3, l, t);
	    c->character(f, DIMPLE_FILL, arrow_length, bg2, l, t);
	    c->character(f, DIMPLE_LR, arrow_length, white, l, t);
	}
	break;
    case forward_arrow_highlighted:
	if (h) {
	    l += arrow_length + arrow_length;
	}
	if (f != nil) {
	    c->character(f, box_ul, arrow_length, bg3, l, t);
	    c->character(f, box2_fill, arrow_length, bg2, l, t);
	    c->character(f, box_lr, arrow_length, white, l, t);
	}
	break;
    case backward_arrow_dimmed:
	if (h) {
	    c->fill_rect(l, b, l + arrow_length, t, dim);
	} else {
	    c->fill_rect(l, b, r, t - arrow_length - arrow_length, dim);
	}
	break;
    case forward_arrow_dimmed:
	if (h) {
	    c->fill_rect(l + arrow_length + arrow_length, b, r, t, dim);
	} else {
	    c->fill_rect(l, t - arrow_length, r, t, dim);
	}
	break;
    case both_arrows_dimmed:
	c->fill_rect(l, b, r, t, dim);
	break;
    }
}

void OL_ElevatorGlyph::undraw() {
    canvas_ = nil;
}

void OL_ElevatorGlyph::flip_to(GlyphIndex i) {
    if (i != index_) {
	if (canvas_ != nil) {
	    canvas_->damage(extension_);
	}
	index_ = i;
    }
}

boolean OL_ElevatorGlyph::inside(const Event& p) {
    Coord x = p.pointer_x();
    Coord y = p.pointer_y();
    const Extension& e = extension_;
    return e.left() <= x && x < e.right() && e.bottom() <= y && y < e.top();
}

boolean OL_ElevatorGlyph::backward_arrow_contains(Coord x, Coord y) const {
    Coord arrow = specs_->arrow_length();
    const Extension& e = extension_;
    Coord l = e.left(), b = e.bottom(), r = e.right(), t = e.top();
    if (dimension_ == Dimension_X) {
	return l <= x && x < l + arrow && b <= y && y < t;
    } else {
	return l <= x && x < r && b <= y && y < t - arrow - arrow;
    }
}

boolean OL_ElevatorGlyph::forward_arrow_contains(Coord x, Coord y) const {
    Coord arrow = specs_->arrow_length();
    const Extension& e = extension_;
    Coord l = e.left(), b = e.bottom(), r = e.right(), t = e.top();
    if (dimension_ == Dimension_X) {
	return l + arrow + arrow <= x && x < r && b <= y && y < t;
    } else {
	return l <= x && x < r &&  t - arrow <= y && y < t;
    }
}

boolean OL_ElevatorGlyph::less_than(const Event& e) const {
    if (dimension_ == Dimension_X) {
	return extension_.right() < e.pointer_x();
    } else {
	return extension_.top() < e.pointer_y();
    }
}

boolean OL_ElevatorGlyph::greater_than(const Event& e) const {
    if (dimension_ == Dimension_X) {
	return extension_.left() > e.pointer_x();
    } else {
	return extension_.bottom() > e.pointer_y();
    }
}

float OL_ElevatorGlyph::forward_arrow_center() const {
    if (dimension_ == Dimension_X) {
	return extension_.right() - specs_->arrow_length() * 0.5;
    } else {
	return extension_.top() - specs_->arrow_length() * 0.5;
    }
}

float OL_ElevatorGlyph::backward_arrow_center() const {
    if (dimension_ == Dimension_X) {
	return extension_.left() + specs_->arrow_length() * 0.5;
    } else {
	return extension_.bottom() + specs_->arrow_length() * 0.5;
    }
}

GlyphIndex OL_ElevatorGlyph::index() const { return index_; }

OL_Indicator::OL_Indicator(
    const OLKit* k, const OL_Specs* s, DimensionName d
) : Glyph(), kit_(k), specs_(s), dimension_(d) {}

void OL_Indicator::allocate(Canvas* c, const Allocation& a, Extension& e) {
    Coord gap = specs_->cable_gap();
    if (dimension_ == Dimension_X) {
	e.set_xy(c, a.left() - gap, a.bottom(), a.right() + gap, a.top());
    } else {
	e.set_xy(c, a.left(), a.bottom() - gap, a.right(), a.top() + gap);
    }
    e.set_xy(c, e.left() - 1, e.bottom() - 1, e.right() + 1, e.top() + 1);
}

void OL_Indicator::draw(Canvas* c, const Allocation& a) const {
    Coord gap = specs_->cable_gap();
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    
    if (dimension_ == Dimension_X) {
	c->fill_rect(l - gap, b, r + gap, t, kit_->bg1());
    } else {
	c->fill_rect(l, b - gap, r, t + gap, kit_->bg1());
    }

    c->fill_rect(l, b, r, t, kit_->bg3());
}

OL_Frame::OL_Frame(
    const OLKit* k, Glyph* g, TelltaleState* t, Coord thickness
) : BevelFrame(g, thickness), kit_(k), state_(t), thickness_(thickness)
{
    Resource::ref(state_); 
}

OL_Frame::~OL_Frame() {
    Resource::unref(state_); 
}

void OL_Frame::draw(Canvas* c, const Allocation& a) const {
    draw_background(c, a);
    BevelFrame::draw(c, a);
    if (!state_->test(TelltaleState::is_enabled)) {
	Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
	Coord th = thickness_;
	c->fill_rect(l + th, b + th, r - th, t - th, kit_->inactive());
    }
}

void OL_Frame::draw_frame(
    Canvas* c, const Allocation& a, Coord thickness
) const {
    const Color* ul, *lr;
    if (!state_->test(TelltaleState::is_active)) {
	ul = kit_->white();
	lr = kit_->bg3();
    } else {
	ul = kit_->bg3();
	lr = kit_->white();
    }
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    Bevel::rect(c, ul, nil, lr, thickness, l, b, r, t);
}

void OL_Frame::draw_background(Canvas* c, const Allocation& a) const {
    c->fill_rect(a.left(), a.bottom(), a.right(), a.top(), kit_->bg1());
}

OL_Gauge::OL_Gauge(
    const OLKit* k, const OL_Specs* s, DimensionName d, Adjustable* a, Patch* p
) : kit_(k), specs_(s), dimension_(d), adjustable_(a), patch_(p) {
    adjustable_->attach(dimension_, this);
}

OL_Gauge::~OL_Gauge() {
    if (adjustable_ != nil) {
	adjustable_->detach(dimension_, this);
    }
}

void OL_Gauge::request(Requisition& r) const {
    Requirement width(specs_->gauge_width());
    Requirement length(specs_->gauge_min_length(), fil, 0.0, 0.0);

    if (dimension_ == Dimension_X) {
	r.require_x(length);
	r.require_y(width);
    } else {
	r.require_x(width);
	r.require_y(length);
    }
}

void OL_Gauge::allocate(Canvas* c, const Allocation& a, Extension& e) {
    e.set(c, a);
}

void OL_Gauge::draw(Canvas* c, const Allocation& a) const {
    boolean horizontal = dimension_ == Dimension_X;
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    const Color* white = kit_->white(), *black = kit_->black();
    const Color* bg1 = kit_->bg1(), *bg2 = kit_->bg2(), *bg3 = kit_->bg3();
    const Font* f = specs_->font();

    Coord cw = specs_->gauge_cap_width();
    Coord rule = specs_->gauge_rule();

    Coord ew, eh;
    if (f != nil) {
	FontBoundingBox box;
	if (horizontal) {
	    f->char_bbox(HORIZ_GAUGE_LEFT_ENDFILL, box);
	} else {
	    f->char_bbox(VERT_GAUGE_BOT_FILL, box);
	}
	ew = box.width();
	eh = box.ascent() + box.descent();
    } else {
	ew = eh = specs_->gauge_end_width();
    }

    /* background */
    if (horizontal) {
	if (f != nil) {
	    c->character(f, HORIZ_GAUGE_LEFT_ENDFILL, ew, bg2, l, t);
	    c->character(f, HORIZ_GAUGE_RIGHT_ENDFILL, ew, bg2, r - ew, t);
	}
	c->fill_rect(l + ew, b, r - ew, t, bg2);
    } else {
	if (f != nil) {
	    c->character(f, VERT_GAUGE_TOP_FILL, ew, bg2, l, t);
	    c->character(f, VERT_GAUGE_BOT_FILL, ew, bg2, l, b + eh);
	}
	c->fill_rect(l, b + eh, r, t - eh, bg2);
    }

    /* outline */
    if (horizontal) {
	if (f != nil) {
	    c->character(f, HORIZ_GAUGE_UL, ew, bg3, l, t);
	    c->character(f, HORIZ_GAUGE_UR, ew, bg3, r - ew, t);
	    c->character(f, HORIZ_GAUGE_LL, ew, white, l, t);
	    c->character(f, HORIZ_GAUGE_LR, ew, white, r - ew, t);
	}
	c->fill_rect(l + ew, t - rule, r - ew, t, bg3);
	c->fill_rect(l + ew, b, r - ew, b + rule, white);
    } else {
	if (f != nil) {
	    c->character(f, VERT_GAUGE_LL, ew, bg3, l, b + eh);
	    c->character(f, VERT_GAUGE_UL, ew, bg3, l, t);
	    c->character(f, VERT_GAUGE_UR, ew, white, l, t);
	    c->character(f, VERT_GAUGE_LR, ew, white, l, b + eh);
	}
	c->fill_rect(l, b + eh, l + rule, t - eh, bg3);
	c->fill_rect(r - rule, b + eh, r, t - eh, white);
    }

    /* interior */
    Coord indent = specs_->gauge_indent();
    Coord ll = l + indent, bb = b + indent, rr = r - indent, tt = t - indent;
    float percent =
        adjustable_->cur_upper(dimension_) / adjustable_->upper(dimension_);
    Coord origin = specs_->gauge_origin();
    Coord filled;
    if (horizontal) {
	if (f != nil) {
	    c->character(f, HORIZ_SLIDER_LEFT_ENDCAP_FILL, ew, black, ll, tt);
	}
	filled = l + origin + (r - l - origin - origin) * percent;
	c->fill_rect(ll + cw, bb, filled, tt, black);
	if (percent > 0) {
	    Coord ttt = tt - specs_->gauge_shimmer_gap();
	    c->fill_rect(
                l + origin, ttt - specs_->gauge_shimmer_width(), filled,ttt,bg3
	    );
	}
    } else {
	if (f != nil) {
	    c->character(
                f, VERT_SLIDER_BOTTOM_ENDCAP_FILL, ew, black, ll, bb + cw
            );
	}
	filled = b + origin + (t - b - origin - origin) * percent;
	c->fill_rect(ll, bb + cw, rr, filled, black);
	if (percent > 0) {
	    Coord lll = ll + specs_->gauge_shimmer_gap();
	    c->fill_rect(
                lll, b + origin, lll + specs_->gauge_shimmer_width(),filled,bg3
	    );
	}
    }
}

void OL_Gauge::update(Observable*) {
    patch_->redraw();
}

void OL_Gauge::disconnect(Observable*) {
    patch_->undraw();
    adjustable_ = nil;
}

OL_MenuMark::OL_MenuMark(
    const OLKit* k, const OL_Specs* s, boolean pulldown
) : Glyph(), kit_(k), specs_(s), font_(s->font()) {
    Resource::ref(font_); 
    if (pulldown) {
	fill_code_ = VERT_MENU_MARK_FILL;
	ul_code_ = VERT_MENU_MARK_UL;
	lr_code_ = VERT_MENU_MARK_LR;
    } else {
	fill_code_ = HORIZ_MENU_MARK_FILL; 
	ul_code_ = HORIZ_MENU_MARK_UL;
	lr_code_ = HORIZ_MENU_MARK_LR;
    }

    if (font_ != nil) {
	FontBoundingBox box;
	font_->char_bbox(fill_code_, box);
	fill_width_ = box.width();
	fill_height_ = box.ascent() + box.descent();
    
	font_->char_bbox(ul_code_, box);
	ul_width_ = box.width();
	ul_height_ = box.ascent() + box.descent();
	
	font_->char_bbox(lr_code_, box);
	lr_width_ = box.width();
	lr_height_ = box.ascent() + box.descent();
    }
}

OL_MenuMark::~OL_MenuMark () {
    Resource::unref(font_); 
}

void OL_MenuMark::request(Requisition& r) const {
    r.x_requirement().natural(specs_->menu_mark_width());
    r.y_requirement().natural(specs_->menu_mark_height());
}

void OL_MenuMark::allocate(Canvas* c, const Allocation& a, Extension& e) {
    e.set(c, a);
}

void OL_MenuMark::draw(Canvas* c, const Allocation& a) const {
    if (font_ != nil) {	
	const Font* f = font_;
	Coord x = a.x();
	Coord y = a.y();
    
	c->character(
            f, fill_code_, fill_width_, kit_->bg2(), x, y + fill_height_);
	c->character(f, ul_code_, ul_width_, kit_->bg3(), x, y + ul_height_);
	c->character(f, lr_code_, lr_width_, kit_->white(), x, y + lr_height_);
    }
}

OL_Mover::OL_Mover(
    const OLKit* k, const OL_Specs* s, OL_MoverFlags f, TelltaleState* t
) : kit_(k), specs_(s), state_(t), font_(s->font()) {
    Resource::ref(state_);
    if (f == up || f == down) {
	box_ul_ = VERT_SB_BOX_UL;
	box_lr_ = VERT_SB_BOX_LR;
	if (f == up) {
	    fill_ = VERT_SB_TOPBOX_FILL;
	} else {
	    fill_ = VERT_SB_BOTBOX_FILL;
	}
    } else {
	box_ul_ = HORIZ_SB_BOX_UL;
	box_lr_ = HORIZ_SB_BOX_LR;
	if (f == left) {
	    fill_ = HORIZ_SB_LEFTBOX_FILL;
	} else {
	    fill_ = HORIZ_SB_RIGHTBOX_FILL;
	}
    }
    if (font_ != nil) {
	FontBoundingBox box;
	font_->char_bbox(box_ul_, box);
	width_ = box.width() + 1;
	height_ = box.ascent() + box.descent() + 1;
    } else {
	width_ = height_ = specs_->arrow_length();
    }
}

OL_Mover::~OL_Mover() {
    Resource::unref(state_); 
}

void OL_Mover::request(Requisition& r) const {
    Requirement r_x(width_);
    Requirement r_y(height_);

    r.require_x(r_x);
    r.require_y(r_y);
}

void OL_Mover::allocate(Canvas* c, const Allocation& a, Extension& e) {
    e.set(c, a);
}

void OL_Mover::draw(Canvas* c, const Allocation& a) const {
    if (font_ != nil) {
	const Color* white = kit_->white();
	const Color* bg1 = kit_->bg1(), *bg2 = kit_->bg2(), *bg3 = kit_->bg3();
	Coord l = a.left(), b = a.bottom(),  r = a.right(), t = a.top();

	c->fill_rect(l + 1, b + 1, r - 1, t - 1, bg3);	// arrow

	if (state_->test(TelltaleState::is_active)) {
	    c->character(font_, fill_, width_, bg2, l, t);
	    c->character(font_, box_ul_, width_, bg3, l, t);
	    c->character(font_, box_lr_, width_, white, l, t);
	} else {
	    c->character(font_, fill_, width_, bg1, l, t);
	    c->character(font_, box_ul_, width_, white, l, t);
	    c->character(font_, box_lr_, width_, bg3, l, t);
	}

	if (state_->test(TelltaleState::is_running)) {
	    c->fill_rect(l, r, b, t, kit_->busy());
	} else if (!state_->test(TelltaleState::is_enabled)) {
	    c->fill_rect(l, r, b, t, kit_->inactive());
	}
    }
}

OL_Pushpin::OL_Pushpin(
    TelltaleState* s, const Window* u,  Window* p
) : Action(), state_(s), unpinned_(u), pinned_(p), placed_(false) {
    Resource::ref(state_); 
}

OL_Pushpin::~OL_Pushpin() {
    Resource::unref(state_); 
}

void OL_Pushpin::execute() {
    if (state_->test(TelltaleState::is_chosen)) {
	if (!placed_) {
	    pinned_->place(unpinned_->left(), unpinned_->bottom());
	    pinned_->align(0.0, 0.0);
	    placed_ = true;
	}
	pinned_->map();
    } else {
	pinned_->unmap();
    }
}

OL_PushpinLook::OL_PushpinLook(
    const OLKit* k, const OL_Specs* specs, TelltaleState* state
) : Glyph(), kit_(k), specs_(specs), state_(state), width_(0), height_(0) {
    Resource::ref(state_);
    const Font* font = specs->font();
    if (font != nil) {
	FontBoundingBox box;
	font->char_bbox(PUSHPIN_OUT_BOTTOM, box);
	width_ = box.width();
	height_ = box.ascent() + box.descent();
    }
}

OL_PushpinLook::~OL_PushpinLook() {
    Resource::unref(state_); 
}

void OL_PushpinLook::request(Requisition& r) const {
    Requirement x(width_);
    Requirement y(height_);
    r.require_x(x);
    r.require_y(y);
}

void OL_PushpinLook::allocate(Canvas* c, const Allocation& a, Extension& e) {
    e.set(c, a);
}

void OL_PushpinLook::draw(Canvas* c, const Allocation& a) const {
    if (state_->test(TelltaleState::is_chosen)) {
	if (state_->test(TelltaleState::is_active)) {
	    draw_unpinned(c, a);
	} else {
	    draw_pinned(c, a);
	}
    } else {
	if (state_->test(TelltaleState::is_active)) {
	    draw_pinned(c, a);
	} else {
	    draw_unpinned(c, a);
	}
    }
}

void OL_PushpinLook::draw_pinned(Canvas* c, const Allocation& a) const {
    Coord x = a.x(), y = a.y() + height_;
    const Font* font = specs_->font();
    
    c->character(font, PUSHPIN_IN_TOP, width_, kit_->white(), x, y);
    c->character(font, PUSHPIN_IN_BOTTOM, width_, kit_->bg3(), x, y);
    c->character(font, PUSHPIN_IN_MIDDLE, width_, kit_->bg2(), x, y);
}

void OL_PushpinLook::draw_unpinned(Canvas* c, const Allocation& a) const {
    Coord x = a.x(), y = a.y() + height_;
    const Font* font = specs_->font();
    
    c->character(font, PUSHPIN_OUT_TOP, width_, kit_->white(), x, y);
    c->character(font, PUSHPIN_OUT_BOTTOM, width_, kit_->bg3(), x, y);
    c->character(font, PUSHPIN_OUT_MIDDLE, width_, kit_->bg2(), x, y);
}

OL_Scrollbar::OL_Scrollbar(
    const OL_Specs* specs, Adjustable* a, Style* style, DimensionName d,
    OL_Cable* c, OL_Elevator* e
) : ActiveHandler(nil, style), Observer(), specs_(specs), adjustable_(a),
    dimension_(d), cable_(c), elevator_(e),
    overlay_(LayoutKit::instance()->overlay(c, e))
{
    body(overlay_);
    adjustable_->attach(dimension_, this);
}

OL_Scrollbar::~OL_Scrollbar() {
    if (adjustable_ != nil) {
	adjustable_->detach(dimension_, this);
    }
}

void OL_Scrollbar::allocation_changed(Canvas* c, const Allocation& a) {
    Extension e;
    e.clear();

    overlay_->modified(0);
    MonoGlyph::allocate(c, a, e); 
}

void OL_Scrollbar::press(const Event& e) {
    if (elevator_->inside(e) || elevator_->grabbing()) {
	elevator_->press(e);
    } else {
	cable_->press(e);
    }
    ActiveHandler::press(e); 
}

void OL_Scrollbar::drag(const Event& e) {
    if (elevator_->inside(e) || elevator_->grabbing()) {
	elevator_->drag(e);
    }
    ActiveHandler::drag(e); 
}

void OL_Scrollbar::release(const Event& e) {
    if (cable_->grabbing()) {
	cable_->release(e);
    } else if (elevator_->grabbing()) {
	elevator_->release(e);
    } else {
	if (elevator_->inside(e)) {
	    elevator_->release(e);
	} else if (elevator_->less_than(e) || elevator_->greater_than(e)) {
	    cable_->release(e);
	}
    }
    ActiveHandler::release(e); 
}

void OL_Scrollbar::update(Observable*) {
    Canvas* c = canvas();
    if (c != nil) {
	cable_->redraw();
	elevator_->redraw();
	
	Extension ext;
	ext.clear();
	allocate(c, allocation(), ext);
	cable_->redraw();
	elevator_->redraw();
    }
}

void OL_Scrollbar::disconnect(Observable*) {
    adjustable_ = nil;
}

OL_Slider::OL_Slider(
    const OL_Specs* specs, Adjustable* a, Style* style, DimensionName d,
    OL_Channel* c, OL_Dragbox* b
) : ActiveHandler(nil, style), Observer(), specs_(specs), adjustable_(a),
    dimension_(d), channel_(c), box_(b)
{
    body(channel_);
    adjustable_->attach(dimension_, this);
}

OL_Slider::~OL_Slider() {
    if (adjustable_ != nil) {
	adjustable_->detach(dimension_, this);
    }
}

void OL_Slider::allocation_changed(Canvas* c, const Allocation& a) {
    Extension e;
    e.clear();
    MonoGlyph::allocate(c, a, e); 
}

void OL_Slider::press(const Event& e) {
    if ((box_->inside(e) || box_->dragging())
	&& e.pointer_button() == Event::left)
    {
	box_->press(e);
	channel_->drag_to(e);
    } else {
	channel_->press(e);
    }
    ActiveHandler::press(e); 
}

void OL_Slider::drag(const Event& e) {
    if (box_->dragging()) {
	channel_->drag_to(e);
    }
    ActiveHandler::drag(e); 
}

void OL_Slider::release(const Event& e) {
    if (box_->dragging()) {
	box_->release(e);
    } else {
	channel_->release(e);
    }
    ActiveHandler::release(e); 
}

void OL_Slider::update(Observable*) {
    Canvas* c = canvas();
    if (c != nil) {
	channel_->redraw();
	
	Extension ext;
	ext.clear();
	allocate(c, allocation(), ext);
	channel_->redraw();
    }
}

void OL_Slider::disconnect(Observable*) {
    adjustable_ = nil;
}

OL_Setting::OL_Setting(
    const OLKit* k, Glyph* g, TelltaleState* t,
    const OL_Specs* s, boolean d
) : OL_Frame(k, g, t, s->setting_thickness()), Observer(),
    specs_(s), is_default_(d),
    brush_(new Brush(s->setting_default_ring_thickness()))
{
    Resource::ref(brush_);

    Requisition r;
    g->request(r);
    Coord vm = (s->setting_height() - r.y_requirement().natural()) * 0.5;
    if (vm < s->setting_vertical_margin()) {
	vm = s->setting_vertical_margin();
    }

    Coord min_width = 72.0;
    k->style()->find_attribute("minimumWidth", min_width);
    Coord hm = s->setting_horizontal_margin();
    Coord width = hm + r.x_requirement().natural() + hm;
    Coord rm = 0;
    if (width < min_width) {
	rm = min_width - width;
    }
    LayoutKit& l = *LayoutKit::instance();
 
    body(l.margin(g, hm, rm + hm, vm, vm));
}

OL_Setting::~OL_Setting() {
    Resource::unref(brush_); 
}

void OL_Setting::draw(Canvas* c, const Allocation& a) const {
    OL_Frame::draw(c, a);

    if (is_default_ && !state_->test(TelltaleState::is_active)) {
	const OL_Specs& s = *specs_;
	Coord i = s.setting_thickness() + s.setting_gap() +
	    s.setting_default_ring_thickness() * 0.5;
	Coord l = a.left()+i, b = a.bottom()+i, r = a.right()-i, t = a.top()-i;
	c->rect(l, b, r, t, kit_->bg3(), brush_);
    }
}

void OL_Setting::pick(Canvas*, const Allocation& a, int depth, Hit& h) {
    Coord x = h.left();
    Coord y = h.bottom();
    if (a.left() <= x && x < a.right() && a.bottom() <= y && y < a.top()) {
	h.target(depth, this, 0);
    }
}

void OL_Setting::draw_background(Canvas* c, const Allocation& a) const {
    const Color* background;
    if (state_->test(TelltaleState::is_active) ||
	state_->test(TelltaleState::is_chosen)
    ) {
	background = kit_->bg2();
    } else {
	background = kit_->bg1();
    }
    c->fill_rect(a.left(), a.bottom(), a.right(), a.top(), background);
}

void OL_Setting::draw_frame(
    Canvas* c, const Allocation& a, Coord thickness
) const {
    const Color* ul, *lr;
    if (state_->test(TelltaleState::is_active) ||
	state_->test(TelltaleState::is_chosen)
    ) {
	ul = kit_->bg3();
	lr = kit_->white();
    } else {
	ul = kit_->white();
	lr = kit_->bg3();
    }
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    Bevel::rect(c, ul, nil, lr, thickness,  l, b, r, t);
}

const long one_second = 1000000;

declareIOCallback(OL_Stepper)
implementIOCallback(OL_Stepper)

OL_Stepper::OL_Stepper(
    const OL_Specs* specs, Adjustable* a, DimensionName d, Patch* t
) : MonoGlyph(t), specs_(specs), adjustable_(a), dimension_(d), thumb_(t),
    canvas_(nil),
    allocation_(),
    thumb_allocation_(),
    pointer_x_(0.0), pointer_y_(0.0),
    forward_(false), backward_(false),
    grabbing_(false),
    initial_delay_(0.40 * one_second),
    interval_(0.10 * one_second),
    timer_(new IOCallback(OL_Stepper)(this, &OL_Stepper::tick)),
    saved_cursor_(nil) {}

OL_Stepper::~OL_Stepper() {
    delete timer_;
}

void OL_Stepper::request(Requisition& r) const {
    Requirement r_x = requirement_x();
    Requirement r_y = requirement_y();

    r.require(Dimension_X, r_x);
    r.require(Dimension_Y, r_y);
}

/*
 *  Position the thumb by calculating it's exact allocation.
 *
 *  However, we need to tell the enclosing ActiveHandler (the Slider)
 *  that we want events from the entire area.
 */
void OL_Stepper::allocate(Canvas* c, const Allocation& a, Extension& e) {
    allocation_ = a;
    canvas_ = c;

    allocate_thumb(a, thumb_allocation_);
    MonoGlyph::allocate(c, thumb_allocation_, e);

    e.set(c, a);
}

void OL_Stepper::draw(Canvas* c, const Allocation&) const {
    MonoGlyph::draw(c, thumb_allocation_);
    if (grabbing_) {
	adjust_pointer(pointer_x_, pointer_y_);
    }
}

void OL_Stepper::redraw() {
    thumb_->redraw();
}

boolean OL_Stepper::grabbing() const {
    return grabbing_;
}

void OL_Stepper::press(const Event& e) {	
    save_pointer(e);

    switch(e.pointer_button()) {
    case Event::left:
	press_select();
	break;
    case Event::middle:
	press_undefined();
	break;
    case Event::right:
	press_undefined();
	break;
    }
}

void OL_Stepper::release(const Event& e) {
    switch (e.pointer_button()) {
    case Event::left:
	release_select();
	break;
    case Event::middle: 
	release_undefined();
	break;
    case Event::right: 
	release_undefined();
	break;
    }

    // discard multiple clicks
    Event queued(e);
    while (queued.pending()) {
	queued.read();
    }
}

void OL_Stepper::tick(long, long) {
    if (forward_) {
	if (!at_end()) {
	    step_forward();
	}
	if (!at_end()) {
	    next_step();
	}
    } else if (backward_) {
	if (!at_start()) {
	    step_backward();
	}
	if (!at_start()) {
	    next_step();
	}
    }
}

void OL_Stepper::allot_major_axis(
    const Allotment& a, Coord length, Coord gap, Allotment& result
) const {
    Coord position = thumb_position(a, length, gap);

    result.span(length);
    result.origin(position);
    result.alignment(0.5);
}

float OL_Stepper::percent_visible() const {
    Adjustable& s = *adjustable_;
    DimensionName d = dimension_;

    float cur_upper = s.cur_upper(d);
    float cur_lower = s.cur_lower(d);
    return (cur_upper - cur_lower) / (s.upper(d) - s.lower(d));
}

Coord OL_Stepper::thumb_position(
    const Allotment& a, Coord thumb_length, Coord gap
) const {
    const Adjustable& s = *adjustable_;
    DimensionName d = dimension_;

    float normalized_position;
    float cur_upper = s.cur_upper(d);
    float upper = s.upper(d);
    float cur_lower = s.cur_lower(d);
    float lower = s.lower(d);

    if (cur_upper == upper) {
	normalized_position = 1.0;
    } else if (cur_lower == lower) {
	normalized_position = 0.0;
    } else {
	float visible_position = (cur_upper + cur_lower) * 0.5;
	float half_visible_length = s.cur_length(d) * 0.5;
	float visible_min = s.lower(d) + half_visible_length;
	float visible_max = s.upper(d) - half_visible_length;
	normalized_position =
	    (visible_position - visible_min) / (visible_max - visible_min);
    }

    Coord half_thumb = thumb_length * 0.5;
    Coord scrollable_min = a.begin() + gap + half_thumb;
    Coord scrollable_max = a.end() - gap - half_thumb;
    Coord scrollable = scrollable_max - scrollable_min;
    
    return scrollable_min + scrollable * normalized_position;
}

void OL_Stepper::allot_minor_axis(
    const Allotment& a, Coord width, Allotment& result
) const {
    result.origin((a.begin() + a.end()) * 0.5);
    result.span(width);
    result.alignment(0.5);
}

void OL_Stepper::move_pointer(Coord x, Coord y) const {
    Window* w = canvas_->window();
    Coord l = w->left();
    Coord b = w->bottom();
    
    Session::instance()->default_display()->move_pointer(l + x, b + y);

    OL_Stepper& self = *(OL_Stepper*)this;
    self.pointer_x_ = x;
    self.pointer_y_ = y;
}

void OL_Stepper::save_pointer(const Event& e) {
    pointer_x_ = e.pointer_x();
    pointer_y_ = e.pointer_y();

    forward_ = is_forward(pointer_x_, pointer_y_);
    backward_ = is_backward(pointer_x_, pointer_y_);
}

void OL_Stepper::press_select() {
    grabbing_ = true;
    if (forward_) {
	step_forward();
	start_stepping();
    } else if (backward_) {
	step_backward();
	start_stepping();
    }
}

void OL_Stepper::release_select() {
    grabbing_ = false;
    stop_stepping();
}

void OL_Stepper::press_undefined() {
    Window* window = canvas_->window();
    saved_cursor_ = window->cursor();
    window->cursor(question_mark_cursor);
}

void OL_Stepper::release_undefined() {
    canvas_->window()->cursor(saved_cursor_);
}

boolean OL_Stepper::at_start() const {
    Adjustable& a = *adjustable_;
    DimensionName d = dimension_;
    return a.cur_lower(d) == a.lower(d);
}

boolean OL_Stepper::at_end() const {
    Adjustable& a = *adjustable_;
    DimensionName d = dimension_;
    return a.cur_upper(d) == a.upper(d);
}

void OL_Stepper::start_stepping() {
    if (initial_delay_ > 10) {
        Dispatcher::instance().startTimer(0, initial_delay_, timer_);
    }
}

void OL_Stepper::next_step() {
    Dispatcher::instance().startTimer(0, interval_, timer_);
}

void OL_Stepper::stop_stepping() {
    Dispatcher::instance().stopTimer(timer_);
}

OL_Tick::OL_Tick(const OLKit* k, const OL_Specs* s, DimensionName d)
  : kit_(k), specs_(s), dimension_(d)
{}

void OL_Tick::request(Requisition& r) const {
    Requirement width(2.0);  // using Table B-40 may cause uneven widths
    Requirement length(specs_->tick_length());
    if (dimension_ == Dimension_X) {
	r.require_x(width);
	r.require_y(length);
    } else {
	r.require_x(length);
	r.require_y(width);
    }
}

void OL_Tick::allocate(Canvas* c, const Allocation& a, Extension& e) {
    e.set(c, a);
}

void OL_Tick::draw(Canvas* c, const Allocation& a) const {
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    Coord hcenter = (l + r) * 0.5, vcenter = (t + b) * 0.5;
    const Color* white = kit_->white(), *bg3 = kit_->bg3();
    
    if (dimension_ == Dimension_X) {
	c->fill_rect(l, t - 1.0, r, t, white);
	c->fill_rect(l, b, hcenter, t, white);
	c->fill_rect(hcenter, b, r, t - 1.0, bg3);
	c->fill_rect(l, b, r, b + 1.0, bg3);
    } else {
	c->fill_rect(l, vcenter, r, t, white);
	c->fill_rect(l, b, l + 1.0, t, white);
	c->fill_rect(l + 1.0, b, r, vcenter, bg3);
	c->fill_rect(r - 1.0, b, r, t, bg3);
    }
}

OL_ToLimit::OL_ToLimit(
    Adjustable* a, DimensionName dimension, OL_Direction direction
) : Action(), adjustable_(a), dimension_(dimension), direction_(direction) {
    if (dimension == Dimension_Y) { // Y coords are from bottom to up.
	if (direction == start) {
	    direction_ = end;
	} else {
	    direction_ = start;
	}
    }
}

void OL_ToLimit::execute() {
    Coord limit;
    if (direction_ == start) {
        limit = 0.0;
    } else {
        limit = adjustable_->length(dimension_);
    }
    adjustable_->scroll_to(dimension_, limit); 
}

OL_Cable::OL_Cable(
    const OL_Specs* specs, Adjustable* a, DimensionName d, OL_Indicator* i
) :
    OL_Stepper(specs, a, d, new Patch(i)),
    indicator_(i),
    gray_(new Color(0.0, 0.0, 0.0, 0.5))
{
    Resource::ref(gray_);
}

OL_Cable::~OL_Cable() {
    Resource::unref(gray_);
}

void OL_Cable::draw(Canvas* c, const Allocation& a) const {
    DimensionName d = (dimension_ == Dimension_X)? Dimension_Y: Dimension_X;
    const Allotment& allot = a.allotment(d);
    Coord center = (allot.begin() + allot.end()) * 0.5;
    Coord cable_width = specs_->cable_width();
    Coord g = specs_->cable_gap();

    if (dimension_ == Dimension_X) {
	Coord b =  center - cable_width * 0.5;
	c->fill_rect(a.left() + g, b, a.right() - g, b + cable_width, gray_);
    } else {
	Coord l = center - cable_width * 0.5;
        c->fill_rect(l, a.bottom() + g, l + cable_width, a.top() - g, gray_);
    }
    OL_Stepper::draw(c, a);
}

Requirement OL_Cable::requirement_x() const {
    Requirement result;
    if (dimension_ == Dimension_X) {
	result = Requirement(specs_->shaft_length(), fil, 0, 0.0);
    } else {
	result = Requirement (specs_->elevator_width(), fil, 0, 0.5);
    }
    return result;
}

Requirement OL_Cable::requirement_y() const {
    Requirement result;
    if (dimension_ == Dimension_X) {
	result = Requirement(specs_->elevator_width(), fil, 0, 0.5);	
    } else {
        result = Requirement(specs_->shaft_length(), fil, 0, 0.0);
    }
    return result;
}

void OL_Cable::allocate_thumb(const Allocation& a, Allocation& result) {
    const OL_Specs& s = *specs_;
    DimensionName d = dimension_;
    const Allotment& allotment = a.allotment(d);
    Coord gap = s.elevator_to_anchor_gap() + s.cable_gap();
    Coord length = percent_visible() * (allotment.span() - gap - gap);

    allot_major_axis(allotment, length, gap, result.allotment(d));

    DimensionName minor_d = (d == Dimension_X) ? Dimension_Y : Dimension_X;
    allot_minor_axis(
        a.allotment(minor_d), s.cable_width(), result.allotment(minor_d)
    );
}

void OL_Cable::adjust_pointer(Coord x, Coord y) const {
    Coord space = specs_->elevator_length() * 0.5;
    Coord elevator, p, begin, end;
    if (dimension_ == Dimension_X) {
	p = x;
	begin = allocation_.left();
	end = allocation_.right();
    } else {
	p = y;
	begin = allocation_.bottom();
	end = allocation_.top();
    }

    if (forward_) {
	elevator = elevator_max();
	if (p <= elevator + space) {
	    p = elevator + space;
	    if (p >= end) {
		p = end - 1;
	    }
	}
    } else if (backward_) {
	elevator = elevator_min();
	if (p >= elevator - space) {
	    p = elevator - space;
	    if (p <= begin) {
		p = begin + 1;
	    }
	}
    }

    if (dimension_ == Dimension_X) {
	move_pointer(p, y);
    } else {
	move_pointer(x, p);
    }
}

Coord OL_Cable::elevator_min() const {
    Coord l = specs_->elevator_length();
    Coord g = specs_->elevator_to_anchor_gap();
    const Allotment& a = allocation_.allotment(dimension_);

    return thumb_position(a, l, g) - l * 0.5;
}

Coord OL_Cable::elevator_max() const {
    Coord l = specs_->elevator_length();
    Coord g = specs_->elevator_to_anchor_gap();
    const Allotment& a = allocation_.allotment(dimension_);

    return thumb_position(a, l, g) + l * 0.5;
}

boolean OL_Cable::is_forward(Coord x, Coord y) const {
    if (dimension_ == Dimension_X) {
	return elevator_max() < x;
    } else {
	return elevator_max() < y;
    }
}

boolean OL_Cable::is_backward(Coord x, Coord y) const {
    if (dimension_ == Dimension_X) {
	return x < elevator_min();
    } else {
	return y < elevator_min();
    }
}

void OL_Cable::step_forward() { adjustable_->page_forward(dimension_); }
void OL_Cable::step_backward() { adjustable_->page_backward(dimension_); }

OL_Channel::OL_Channel(
     const OLKit* k, const OL_Specs* s, Adjustable* a, DimensionName d,
     OL_Dragbox* b
) : OL_Stepper(s, a, d, new Patch(b)), kit_(k) {}

void OL_Channel::draw(Canvas* c, const Allocation& a) const {
    boolean horizontal = dimension_ == Dimension_X;
    long ul, ll, ur, lr, lfill, rfill;
    if (horizontal) {
	ul = HORIZ_SLIDER_UL;
	ll = HORIZ_SLIDER_LL;
	ur = HORIZ_SLIDER_UR;
	lr = HORIZ_SLIDER_LR;
	lfill = HORIZ_SLIDER_LEFT_ENDCAP_FILL;
	rfill = HORIZ_SLIDER_RIGHT_ENDCAP_FILL;
    } else {
	ul = VERT_SLIDER_LL;
	ll = VERT_SLIDER_LR;
	ur = VERT_SLIDER_UL;
	lr = VERT_SLIDER_UR;
	lfill = VERT_SLIDER_BOTTOM_ENDCAP_FILL;
	rfill = VERT_SLIDER_TOP_ENDCAP_FILL;
    }
    const OL_Specs& s = *specs_;
    const Font* f = s.font();
    const OLKit& k = *kit_;
    const Color* black = k.black();
    const Color* white = k.white();
    const Color* bg2 = k.bg2();
    const Color* bg3 = k.bg3();
    Coord channel_width = s.channel_width();
    Coord cap_width = s.channel_cap_width();
    Coord gap = s.channel_gap();
    Coord highlight = s.channel_highlight();
    Coord rule = s.channel_rule();
    
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    if (horizontal) {
	b = (b + t - channel_width) * 0.5;
	t = b + channel_width;
    } else {
	l = (l + r - channel_width) * 0.5;
	r = l + channel_width;
    }
    const Allotment& al = allocation_.allotment(dimension_);
    Coord v = thumb_position(al, s.dragbox_length(), 0.0);

    /* bottom endcap */
    Coord tt = horizontal ? t : b + cap_width;
    if (f != nil) {
	c->character(f, ul, cap_width, black, l, tt);
	c->character(f, ll, cap_width, black, l, tt);
	c->character(f, lfill, cap_width, black, l, tt);
    }

    /* filled region */
    if (horizontal) {
	l += cap_width;
	c->fill_rect(l, b, v, t, black);
	c->fill_rect(l, t - gap, v, t - gap - highlight, bg3);
    } else {
	b += cap_width;
	c->fill_rect(l, b, r, v, black);
	c->fill_rect(l + gap, b, l + gap + highlight, v, bg3);
    }

    /* top section */
    Coord rr = horizontal ? r - cap_width : l;
    if (horizontal) {
	c->fill_rect(v, b, rr, t, bg2);
	c->fill_rect(v, t - rule, rr, t, bg3);
	c->fill_rect(v, b, rr, b + rule, white);
    } else {
	tt = t - cap_width;
	c->fill_rect(l, v, r, tt, bg2);
	c->fill_rect(l, v, l + rule, tt, bg3);
	c->fill_rect(r - rule, v, r, tt, white);
    }

    if (f != nil) {
	c->character(f, ur, cap_width, bg3, rr, t);
	c->character(f, lr, cap_width, white, rr, t);
	c->character(f, rfill, cap_width, bg2, rr, t);
    }

    OL_Stepper::draw(c, thumb_allocation_);
}

/*
 *  min and max refer to the minimim and maximum numbers of the midpoint of
 *  the dragbox, which corresponds to the current value of the adjustable.
 */
void OL_Channel::drag_to(const Event& e) {
    const OL_Specs& s = *specs_;
    Adjustable& a = *adjustable_;
    DimensionName d = dimension_;
    const Allotment& allotment = allocation_.allotment(d);
    
    Coord half_length = s.dragbox_length() * 0.5;
    Coord min = allotment.begin() + half_length;
    Coord max = allotment.end() - half_length;
    Coord scrollable = max - min;
    Coord mouse = (dimension_ == Dimension_X) ? e.pointer_x() : e.pointer_y();
    Coord adjusted_position = mouse - min;
    float fraction = adjusted_position / scrollable;

    a.scroll_to(d, a.lower(d) + a.length(d) * fraction);
}

Requirement OL_Channel::requirement_x() const {
    Requirement result;
    if (dimension_ == Dimension_X) {
	result = Requirement(specs_->channel_length(), fil, 0, 0.0);
    } else {
	result = Requirement (specs_->channel_width(), fil, 0, 0.0);
    }
    return result;
}

Requirement OL_Channel::requirement_y() const {
    Requirement result;
    if (dimension_ == Dimension_X) {
	result = Requirement(specs_->channel_width(), fil, 0, 0.0);	
    } else {
        result = Requirement(specs_->channel_length(), fil, 0, 0.0);
    }
    return result;
}

void OL_Channel::allocate_thumb(const Allocation& a, Allocation& result) {
    const OL_Specs& s = *specs_;
    DimensionName major_d = dimension_;
    const Allotment& major_allot = a.allotment(major_d);
    Coord l = s.dragbox_length();
    allot_major_axis(major_allot, l, 0.0, result.allotment(major_d));

    DimensionName minor_d = (major_d == Dimension_X)? Dimension_Y: Dimension_X;
    const Allotment& minor_allot = a.allotment(minor_d);
    Coord w = s.dragbox_width();
    allot_minor_axis(minor_allot, w, result.allotment(minor_d));
}

void OL_Channel::adjust_pointer(Coord x, Coord y) const {
    Coord space = specs_->dragbox_length() * 0.5;
    Coord thumb, p, begin, end;
    if (dimension_ == Dimension_X) {
	p = x;
	begin = allocation_.left();
	end = allocation_.right();
    } else {
	p = y;
	begin = allocation_.bottom();
	end = allocation_.top();
    }

    if (forward_) {
	thumb = thumb_max();
	if (p <= thumb + space) {
	    p = thumb + space;
	    if (p >= end) {
		p = end - 1;
	    }
	}
    } else if (backward_) {
	thumb = thumb_min();
	if (p >= thumb - space) {
	    p = thumb - space;
	    if (p <= begin) {
		p = begin + 1;
	    }
	}
    }

    if (dimension_ == Dimension_X) {
	move_pointer(p, y);
    } else {
	move_pointer(x, p);
    }
}

Coord OL_Channel::thumb_min() const {
    Coord l = specs_->dragbox_length();
    const Allotment& a = allocation_.allotment(dimension_);

    return thumb_position(a, l, 0.0) - l * 0.5;
}

Coord OL_Channel::thumb_max() const {
    Coord l = specs_->dragbox_length();
    const Allotment& a = allocation_.allotment(dimension_);

    return thumb_position(a, l, 0.0) + l * 0.5;
}

boolean OL_Channel::is_forward(Coord x, Coord y) const {
    if (dimension_ == Dimension_X) {
	return thumb_max() < x;
    } else {
	return thumb_max() < y;
    }
}

boolean OL_Channel::is_backward(Coord x, Coord y) const {
    if (dimension_ == Dimension_X) {
	return x < thumb_min();
    } else {
	return y < thumb_min();
    }
}

void OL_Channel::step_forward() { adjustable_->page_forward(dimension_); }
void OL_Channel::step_backward() { adjustable_->page_backward(dimension_); }

OL_CheckBox::OL_CheckBox(
    const OLKit* k, TelltaleState* t, const OL_Specs* s
) : OL_Frame(k, nil, t, s->checkbox_thickness()) {
    body(new OL_CheckMark(k, t, s));
}

void OL_CheckBox::pick(Canvas*, const Allocation& a, int depth, Hit& h) {
    Coord x = h.left(), y = h.bottom();
    if (a.left() <= x && x < a.right() && a.bottom() <= y && y < a.top()) {
	h.target(depth, this, 0);
    }
}

void OL_CheckBox::draw_background(Canvas* c, const Allocation& a) const {
    const Color* background;
    if (state_->test(TelltaleState::is_active)) {
	background = kit_->bg2();
    } else {
	background = kit_->bg1();
    }
    c->fill_rect(a.left(), a.bottom(), a.right(), a.top(), background);
}

OL_Dragbox::OL_Dragbox(const OLKit* k, const OL_Specs* s, DimensionName d)
  : Glyph(), kit_(k), specs_(s), dimension_(d), dragging_(false),
    canvas_(nil), extension_()
{}

void OL_Dragbox::request(Requisition& r) const {
    Requirement length(specs_->dragbox_length(), 0, 0, 0.0);
    Requirement width(specs_->dragbox_width(), 0, 0, 0.0);
    if (dimension_ == Dimension_X) {
	r.require_x(length);
	r.require_y(width);
    } else {
	r.require_x(width);
	r.require_y(length);
    }
}

/*
 *  Return an extension that extends beyond the actual drawing by 1 point
 *  to eliminate roundoff errors in redraw which may leave doodoos.
 */
void OL_Dragbox::allocate(Canvas* c, const Allocation& a, Extension& e) {
    canvas_ = c;
    extension_.set(c, a);
    e.set_xy(c, a.left() - 1, a.bottom() - 1, a.right() + 1, a.top() + 1);
}

void OL_Dragbox::draw(Canvas* c, const Allocation& a) const {
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    Coord ul, lr, box;
    if (dimension_ == Dimension_X) {
	ul = HORIZ_SLIDER_CONTROL_UL;
	lr = HORIZ_SLIDER_CONTROL_LR;
	box = HORIZ_SLIDER_CONTROL_FILL;
    } else {
	ul = VERT_SLIDER_CONTROL_UL;
	lr = VERT_SLIDER_CONTROL_LR;
	box = VERT_SLIDER_CONTROL_FILL;
    }
    const OLKit& k = *kit_;
    const OL_Specs& s = *specs_;
    Coord width = s.dragbox_width();
    const Color* upper_left, *fill, *lower_right;
    if (dragging_) {
	upper_left = k.bg3();
	fill = k.bg2();
	lower_right = k.white();
    } else {
	upper_left = k.white();
	fill = k.bg1();
	lower_right = k.bg3();
    }

    const Font* f = s.font();
    if (f != nil) {
	c->character(f, ul, width, upper_left, l, t);
	c->character(f, box, width, fill, l, t);
	c->character(f, lr, width, lower_right, l, t);
    }
}

void OL_Dragbox::undraw() {
    canvas_ = nil;
}

boolean OL_Dragbox::inside(const Event& p) const {
    Coord x = p.pointer_x();
    Coord y = p.pointer_y();
    const Extension& e = extension_;
    return e.left() <= x && x < e.right() && e.bottom() <= y && y < e.top();
}

boolean OL_Dragbox::less_than(const Event& e) const {
    if (dimension_ == Dimension_X) {
	return extension_.right() < e.pointer_x();
    } else {
	return extension_.top() < e.pointer_y();
    }
}

boolean OL_Dragbox::greater_than(const Event& e) const {
    if (dimension_ == Dimension_X) {
	return extension_.left() > e.pointer_x();
    } else {
	return extension_.bottom() > e.pointer_y();
    }
}

void OL_Dragbox::press(const Event&e) {
    if (e.pointer_button() == Event::left) {
	dragging_ = true;
    }
}

void OL_Dragbox::release(const Event& e) {
    if (e.pointer_button() == Event::left) {
	dragging_ = false;
	if (canvas_ != nil) {
	    canvas_->damage(extension_);
	}
    }
}

OL_Elevator::OL_Elevator(
    const OL_Specs* specs, Adjustable* a, DimensionName d, OL_ElevatorGlyph* g
) : OL_Stepper(specs, a, d, new Patch(g)), glyph_(g), dragging_(false) { }

/*
 *  Since draw might be initiated by Slider::update, we dim the
 *  OL_ElevatorGlyph if the position of the Adjustable has reached an
 *  end or undim it if otherwise.
 */
void OL_Elevator::draw(Canvas* c, const Allocation& a) const {
    OL_Elevator& self = *(OL_Elevator*)this;
    self.adjust_for_dimming();
    OL_Stepper::draw(c, a);
}

boolean OL_Elevator::inside(const Event& e) const {
    return glyph_->inside(e);
}

boolean OL_Elevator::less_than(const Event& e) const {
    return glyph_->less_than(e);
}

boolean OL_Elevator::greater_than(const Event& e) const {
    return glyph_->greater_than(e);
}

void OL_Elevator::press(const Event& e) {
    OL_Stepper::press(e);
    if (!forward_ && !backward_) {
	dragging_ = true;
	glyph_->flip_to(dragging);
    }
}

void OL_Elevator::drag(const Event& e) {
    if (dragging_) {
	if (!(at_start() && at_end())) {
	    drag_to(e);
	}
    }
}

Requirement OL_Elevator::requirement_x() const {
    Requirement result;
    if (dimension_ == Dimension_X) {
	result = Requirement(specs_->shaft_length(), fil, 0, 0.0);
    } else {
	result = Requirement(specs_->elevator_width(), fil, 0, 0.5);
    }
    return result;
}

Requirement OL_Elevator::requirement_y() const {
    Requirement result;
    if (dimension_ == Dimension_X) {
	result = Requirement(specs_->elevator_width(), fil, 0, 0.5);
    } else {
	result = Requirement(specs_->shaft_length(), fil, 0, 0.0);
    }
    return result;
}

void OL_Elevator::allocate_thumb(const Allocation& a, Allocation& result) {
    const OL_Specs& s = *specs_;
    DimensionName major_d = dimension_;
    const Allotment& major_allot = a.allotment(major_d);
    Coord l = s.elevator_length();
    Coord g = s.elevator_to_anchor_gap();
    allot_major_axis(major_allot, l, g, result.allotment(major_d));

    DimensionName minor_d = (major_d == Dimension_X)? Dimension_Y: Dimension_X;
    const Allotment& minor_allot = a.allotment(minor_d);
    Coord w = s.elevator_width();
    allot_minor_axis(minor_allot, w, result.allotment(minor_d));
}

void OL_Elevator::adjust_pointer(Coord x, Coord y) const {
    if (!dragging_) {
	if (dimension_ == Dimension_X) {
	    if (forward_) {
		x = glyph_->forward_arrow_center();
	    } else if (backward_) {
		x = glyph_->backward_arrow_center();
	    }
	} else {
	    if (forward_) {
		y = glyph_->forward_arrow_center();
	    } else if (backward_) {
		y = glyph_->backward_arrow_center();
	    }
	}
	move_pointer(x, y);
    }
}

boolean OL_Elevator::is_forward(Coord x, Coord y) const {
    return glyph_->forward_arrow_contains(x, y);
}

boolean OL_Elevator::is_backward(Coord x, Coord y) const {
    return glyph_->backward_arrow_contains(x, y);
}

void OL_Elevator::step_backward() {
    dragging_ = false;
    glyph_->flip_to(backward_arrow_highlighted);
    adjustable_->scroll_backward(dimension_);
}

void OL_Elevator::step_forward() {
    dragging_ = false;
    glyph_->flip_to(forward_arrow_highlighted);
    adjustable_->scroll_forward(dimension_);
}

/*
 *  min and max refer to the minimim and maximum numbers the midpoint of
 *  the elevator, which corresponds to the current value of the adjustable.
 */
void OL_Elevator::drag_to(const Event& e) {
    const OL_Specs& s = *specs_;
    Adjustable& a = *adjustable_;
    DimensionName d = dimension_;
    const Allotment& allotment = allocation_.allotment(d);
    
    Coord gap = s.elevator_to_anchor_gap();
    Coord elevator_half_length = s.elevator_length() * 0.5;
    Coord min = allotment.begin() + gap + elevator_half_length;
    Coord max = allotment.end() - gap - elevator_half_length;
    Coord scrollable = max - min;
    Coord mouse = (dimension_ == Dimension_X) ? e.pointer_x() : e.pointer_y();
    Coord adjusted_position = mouse - min;
    float fraction = adjusted_position / scrollable;

    a.scroll_to(d, a.lower(d) + a.length(d) * fraction);
}

void OL_Elevator::release_select() {
    OL_Stepper::release_select();
    glyph_->flip_to(normal);
    dragging_ = false;
}

void OL_Elevator::adjust_for_dimming() {
    if (!dragging_) {
	if (at_start() && at_end()) {
	    glyph_->flip_to(both_arrows_dimmed);
	} else if (at_start()) {
	    glyph_->flip_to(backward_arrow_dimmed);
	} else if (at_end()) {
	    glyph_->flip_to(forward_arrow_dimmed);
	} else {
	    GlyphIndex i = glyph_->index();
	    if (i == both_arrows_dimmed || i == backward_arrow_dimmed ||
		i == forward_arrow_dimmed
	    ) {
		glyph_->flip_to(normal);
	    }
	}
    }
}

OLKit::OLKit() {
    impl_ = new OLKitImpl(this);
    Style* s = Session::instance()->style();
    for (PropertyData* p = kit_props; p->path != nil; p++) {
	s->attribute(p->path, p->value, -10);
    }
}

OLKit::~OLKit() {
    delete impl_;
}

const char* OLKit::gui() const { return "OpenLook"; }

MonoGlyph* OLKit::outset_frame(Glyph* g) const {
    TelltaleState* state = new TelltaleState(TelltaleState::is_enabled);
    return new OL_Frame(this, g, state, impl_->frame_thickness());
}

MonoGlyph* OLKit::inset_frame(Glyph* g) const {
    TelltaleState* state = new TelltaleState(TelltaleState::is_enabled_active);
    return new OL_Frame(this, g, state,	impl_->frame_thickness());
}

MonoGlyph* OLKit::bright_inset_frame(Glyph* g) const {
    return inset_frame(g);	// for now
}

/*
 *  OL pulldown menus align in the center horizontally
 */
Menu* OLKit::menubar() const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("MenuBar", "Menu");
    Menu* m = new Menu(menubar_look(), style(), 0.5, -0.05, 0.5, 1.0);
    k->end_style();
    return m;
}

Glyph* OLKit::menubar_look() const {
    const LayoutKit& l = *impl_->layout_;
    return l.r_margin(l.hbox(), 0.0, fil, 0.0);
}

Glyph* OLKit::pulldown_look() const {
    const LayoutKit& l = *impl_->layout_;
    const OL_Specs& s = *impl_->specs_;
    Coord hmargin = s.menu_hmargin();
    Coord bottom = s.menu_bmargin();
    Coord top = s.menu_tmargin();
    return outset_frame(l.margin(l.vbox(), hmargin, hmargin, bottom, top));
}

/*
 *  Simple alignment first.
 *
 *  An OL pullright menu should be aligned so that its default item aligns
 *  vertically with the control that pops it up (not implemented).
 */
Glyph* OLKit::pullright_look() const {
    return pulldown_look();
}

Glyph* OLKit::menubar_item_look(Glyph* g, TelltaleState* state) const {
    const OLKitImpl& i = *impl_;
    const LayoutKit& l = *i.layout_;
    const OL_Specs* s = i.specs_;
    Glyph* hspace = l.hspace(s->menu_mark_gap());
    Glyph* menu_mark = new OL_MenuMark(this, s, true);
    Glyph* inner = l.hbox(l.vcenter(g), hspace, l.vcenter(menu_mark));
    Coord hm = s->menu_hmargin() * 0.5;
    return l.hmargin(
        new OL_Button(this, s, inner, state, OL_Button::PushButton, false), hm
    );
}    

Glyph* OLKit::menu_item_look(Glyph* g, TelltaleState* t) const {
    OLKitImpl& i = *impl_;
    const OL_Specs* s = i.specs_;
    const LayoutKit& l = *i.layout_;
    return new OL_Button(this, s, g, t, OL_Button::MenuButton);
}

Glyph* OLKit::check_menu_item_look(Glyph* g, TelltaleState* t) const {
    OLKitImpl& i = *impl_;
    Coord height = i.specs_->menu_button_height();
    const LayoutKit& l = *i.layout_;
    return l.vnatural(check_box_look(g, t), height);
}

Glyph* OLKit::radio_menu_item_look(Glyph* g, TelltaleState* t) const {
    return radio_button_look(g, t);
}

Glyph* OLKit::menu_item_separator_look() const {
    return impl_->layout_->vspace(4.0);  // for now
}

Glyph* OLKit::push_button_look(Glyph* g, TelltaleState* t) const {
    return new OL_Button(this, impl_->specs_, g, t, OL_Button::PushButton);
}

Glyph* OLKit::default_button_look(Glyph* g, TelltaleState* t) const {
    return new OL_Button(this, impl_->specs_, g, t, OL_Button::DefaultButton);
}

Glyph* OLKit::check_box_look(Glyph* g, TelltaleState* t) const {
    OLKitImpl& i = *impl_;
    const LayoutKit& l = *i.layout_;
    const OL_Specs* s = i.specs_;
    return l.hbox(
        l.vcenter(new OL_CheckBox(this, t, s)), l.hspace(6.0), l.vcenter(g)
    );
}

Glyph* OLKit::palette_button_look(Glyph* g, TelltaleState* t) const {
    return new OL_Setting(this, g, t, impl_->specs_);
}

Glyph* OLKit::radio_button_look(Glyph* g, TelltaleState* t) const {
    return new OL_Setting(this, g, t, impl_->specs_);
}

Glyph* OLKit::slider_look(DimensionName d, Adjustable* a) const {
    const OLKitImpl& i = *impl_;
    const OL_Specs* s = i.specs_;
    OL_Dragbox* b = new OL_Dragbox(this, s, d);
    OL_Channel* c = new OL_Channel(this, s, a, d, b);
    return new OL_Slider(s, a, i.style_, d, c, b);
}

Glyph* OLKit::scroll_bar_look(DimensionName d, Adjustable* a) const {
    OLKitImpl& i = *impl_;
    LayoutKit& l = *LayoutKit::instance();
    Coord shaft_gap = i.specs_->shaft_gap();
    
    Glyph* anchor1 = i.cable_anchor(a, OL_ToLimit::start, d);
    Glyph* anchor2 = i.cable_anchor(a, OL_ToLimit::end, d);
    Glyph* scrollbar = i.scrollbar(a, d);
    Glyph* result;
    
    if (d == Dimension_X) {
	Glyph* vspace = l.vspace(shaft_gap);
	result = l.vbox(vspace, l.hbox(anchor1, scrollbar, anchor2), vspace);
    } else {
	Glyph* hspace = l.hspace(shaft_gap);
	result = l.hbox(hspace, l.vbox(anchor1, scrollbar, anchor2), hspace);
    }
    return result;
}

/* unimplmented */
Glyph* OLKit::panner_look(Adjustable*, Adjustable*) const {  return nil; }
Glyph* OLKit::enlarger_look(TelltaleState*) const { return nil; }
Glyph* OLKit::reducer_look(TelltaleState*) const { return nil; }

Glyph* OLKit::up_mover_look(TelltaleState* s) const {
    return new OL_Mover(this, impl_->specs_, OL_Mover::up, s);
}

Glyph* OLKit::down_mover_look(TelltaleState* s) const {
    return new OL_Mover(this, impl_->specs_, OL_Mover::down, s);
}

Glyph* OLKit::left_mover_look(TelltaleState* s) const {
    return new OL_Mover(this, impl_->specs_, OL_Mover::left, s);
}

Glyph* OLKit::right_mover_look(TelltaleState* s) const {
    return new OL_Mover(this, impl_->specs_, OL_Mover::right, s);
}

const Color* OLKit::white() const { return impl_->white_; }
const Color* OLKit::bg1() const { return impl_->bg1_; }
const Color* OLKit::bg2() const { return impl_->bg2_; }
const Color* OLKit::bg3() const { return impl_->bg3_; }
const Color* OLKit::black() const { return impl_->black_; }
const Color* OLKit::inactive() const { return impl_->inactive_; }
const Color* OLKit::busy() const { return impl_->busy_; }

Button* OLKit::pushpin(const Window* unpinned, Window* pinned) const {
    const OLKitImpl& i = *impl_;
    TelltaleState* state = new TelltaleState(
	TelltaleState::is_enabled | TelltaleState::is_toggle |
	TelltaleState::is_choosable
    );
    Glyph* pushpin = new OL_PushpinLook(this, i.specs_, state);
    Action* action = new OL_Pushpin(state, unpinned, pinned);
    return new Button(pushpin, i.style_, state, action);
}

void OLKit::pinnable(Menu* menu, const Window* unpinned) const {
    const OLKitImpl& i = *impl_;
    const LayoutKit& l = *i.layout_;
    const OL_Specs& s = *i.specs_;
    Button* pin = pushpin(unpinned, new TransientWindow(menu));
    Coord h = s.menu_pushpin_height();

    Glyph* item = l.vbox(l.vnatural(pin, h), l.vspace(s.menu_pushpin_gap()));
    menu->insert_item(0, new MenuItem(item, pin->state(), pin->action()));
}

MenuItem* OLKit::habbrev_menu_button() const{
    TelltaleState* state = new TelltaleState(TelltaleState::is_enabled);
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("MenuItem");
    MenuItem* m = new MenuItem(
	new OL_AbbrevMenuButton(this, impl_->specs_, Dimension_X, state), state
    );
    k->end_style();
    return m;
}

MenuItem* OLKit::vabbrev_menu_button() const{
    TelltaleState* state = new TelltaleState(TelltaleState::is_enabled);
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("MenuItem");
    MenuItem* m = new MenuItem(
	new OL_AbbrevMenuButton(this, impl_->specs_, Dimension_Y, state), state
    );
    k->end_style();
    return m;
}

Glyph* OLKit::hgauge(Adjustable* a) const {
    Patch* patch = new Patch(nil);
    patch->body(new OL_Gauge(this, impl_->specs_, Dimension_X, a, patch));
    return patch;
}

Glyph* OLKit::vgauge(Adjustable* a) const {
    Patch* patch = new Patch(nil);
    patch->body(new OL_Gauge(this, impl_->specs_, Dimension_Y, a, patch));
    return patch;
}

Glyph* OLKit::htick() const {
    return new OL_Tick(this, impl_->specs_, Dimension_X);
}

Glyph* OLKit::vtick() const {
    return new OL_Tick(this, impl_->specs_, Dimension_Y);
}

Glyph* OLKit::top_end_box(Adjustable* a) const {
    return impl_->cable_anchor(a, OL_ToLimit::start, Dimension_Y);
}

Glyph* OLKit::bottom_end_box(Adjustable* a) const {
    return impl_->cable_anchor(a, OL_ToLimit::end, Dimension_Y);
}

Glyph* OLKit::left_end_box(Adjustable* a) const {
    return impl_->cable_anchor(a, OL_ToLimit::start, Dimension_X);
}

Glyph* OLKit::right_end_box(Adjustable* a) const {
    return impl_->cable_anchor(a, OL_ToLimit::end, Dimension_X);
}

OLKitImpl::OLKitImpl(OLKit* kit) : kit_(kit), layout_(LayoutKit::instance()),
    style_(kit->style()), specs_(new OL_Specs(style_)), 
    frame_thickness_(2.0)
{
    Display* d = Session::instance()->default_display();
    white_ = color(d, "white", "White", 1.0, 1.0, 1.0, 1.0);
    black_ = color(d, "black", "Black", 0.0, 0.0, 0.0, 1.0);

    bg1_ = color(d, "#aaaaaa", "#aaaaaa", 0.7, 0.7, 0.7, 1.0);
    bg2_ = bg1_->brightness(-0.125);
    bg3_ = bg1_->brightness(-0.5);
    inactive_ = new Color(*bg1_, 0.5);
    busy_ = new Color(*black_, 0.15);

    Resource::ref(white_);
    Resource::ref(black_);
    Resource::ref(bg1_);
    Resource::ref(bg2_);
    Resource::ref(bg3_);
    Resource::ref(inactive_);
    Resource::ref(busy_);

    init_ol_cursors();
}

OLKitImpl::~OLKitImpl() {
    Resource::unref(white_); 
    Resource::unref(black_); 
    Resource::unref(bg1_); 
    Resource::unref(bg2_); 
    Resource::unref(bg3_); 
    Resource::unref(inactive_); 
    Resource::unref(busy_); 
}

Glyph* OLKitImpl::cable_anchor(
    Adjustable* a, OL_Direction direction, DimensionName dimension
) const {
    TelltaleState* state = new TelltaleState(TelltaleState::is_enabled);
    const OL_Specs& s = *specs_;
    Coord w, h;
    if (dimension == Dimension_X) {
	w = s.anchor_height();
	h = s.anchor_width();
    } else {
	w = s.anchor_width();
	h = s.anchor_height();
    }
    OL_Anchor* anchor = new OL_Anchor(kit_, w, h, s.anchor_rule(), state);
    Action* action = new OL_ToLimit(a, dimension, direction);
    
    return new Button(anchor, style_, state, action);
}

Glyph* OLKitImpl::scrollbar(Adjustable* a, DimensionName d) const {
    const OL_Specs* s = specs_;
    const OLKit* k = kit_;
    OL_Cable* c = new OL_Cable(s, a, d, new OL_Indicator(k, s, d));
    OL_Elevator* e = new OL_Elevator(s, a, d, new OL_ElevatorGlyph(k, s, d));
    
    return new OL_Scrollbar(s, a, style_, d, c, e);
}

Coord OLKitImpl::frame_thickness() const {
    return frame_thickness_;
}

const Color* OLKitImpl::color(
    Display* d, const char* name, const char* alias,
    ColorIntensity r, ColorIntensity g, ColorIntensity b, float alpha
) {
    const Color* c = Color::lookup(d, name);
    if (c == nil) {
	c = Color::lookup(d, alias);
	if (c == nil) {
	    c = new Color(r, g, b, alpha);
	}
    }

    return c;
}
