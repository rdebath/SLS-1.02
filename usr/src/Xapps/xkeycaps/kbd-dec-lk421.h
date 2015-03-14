/* xkeycaps, Copyright (c) 1991 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the DEC LK421 keyboard.
 * By Tom Ivar Helbekkmo <tih@barsoom.nhh.no>
 *
 * The LK421 is really an LK401 that has been shrunk by removing the keypads,
 * and having them doubled on normal keys through the use of a new "Extend"
 * modifier that's local to the keyboard; it is not "visible" to the X clients.
 *
 * This file displays two images of the keyboard, the "normal" one on top, and
 * one containing only the "extended" keyboard at the bottom.  This is 
 * necessary because each key has two keycodes...
 *
 *  - The key near the top left with the symbols "~" and "`" on it actually
 *    has the word "ESC" printed in its top right corner
 *  - The ">", "<" key at the far right has "~" in the top right and "`" in
 *    the lower right corner
 *  - The keys for brackets and braces look like they're wrong, since the "["
 *    and "]" symbols aren't on them, but this is the way the keyboard is made.
 *  - The space bar is missing a "0" on the front (all the extended key codes
 *    are in blue print at the top front edge of the keys); this file reflects
 *    this by using a 0 instead of a " " for this key in the extended display.
 */

static struct key DEC_LK421_row00 [] = {
 {0,	0,		0,	1, 7},
 {86,	"F1",		0,	7, 7,	0,	XK_F1},
 {87,	"F2",		0,	7, 7,	0,	XK_F2},
 {88,	"F3",		0,	7, 7,	0,	XK_F3},
 {89,	"F4",		0,	7, 7,	0,	XK_F4},
 {90,	"F5",		0,	7, 7,	0,	XK_F5},
 {0,	0,		0,	7, 7},
 {100,	"F6",		0,	7, 7,	0,	XK_F6},
 {101,	"F7",		0,	7, 7,	0,	XK_F7},
 {102,	"F8",		0,	7, 7,	0,	XK_F8},
 {103,	"F9",		0,	7, 7,	0,	XK_F9},
 {104,	"F10",		0,	7, 7,	0,	XK_F10},
 {0,	0,		0,	7, 7},
 {167,	"LeftArrow",	0,	7, 7,	0,	XK_Left},
 {169,	"DownArrow",	0,	7, 7,	0,	XK_Down},
 {170,	"UpArrow",	0,	7, 7,	0,	XK_Up},
 {168,	"RightArrow",	0,	7, 7,	0,	XK_Right}
};

static struct key DEC_LK421_row01 [] = {
 {0,	0,	0,	1, 7},
 {191,	"~",	"`",   10, 7,	0,	XK_grave,	XK_asciitilde},
 {192,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {197,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {203,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {208,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {214,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {219,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {224,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {229,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {234,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {239,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {249,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {245,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {188,	"<X|",	0,     18, 7,	0,	XK_Delete}
};
 
static struct key DEC_LK421_row02 [] = {
 {0,	0,	0,		1, 7},
 {190,	"Tab",	0,	       14, 7,	0,	XK_Tab},
 {193,	"Q",	0,		7, 7,	0,	XK_Q},
 {198,	"W",	0,		7, 7,	0,	XK_W},
 {204,	"E",	0,		7, 7,	0,	XK_E},
 {209,	"R",	0,		7, 7,	0,	XK_R},
 {215,	"T",	0,		7, 7,	0,	XK_T},
 {220,	"Y",	0,		7, 7,	0,	XK_Y},
 {225,	"U",	0,		7, 7,	0,	XK_U},
 {230,	"I",	0,		7, 7,	0,	XK_I},
 {235,	"O",	0,		7, 7,	0,	XK_O},
 {240,	"P",	0,		7, 7,	0,	XK_P},
 {250,	"{",	0,		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {246,	"}",	0,		7, 7,	0,	XK_bracketright,XK_braceright},
 {247,	"|",	"\\",		7, 7,	0,	XK_backslash,	XK_bar},
 {201,	">",	"<",		7, 7,	0,	XK_less,	XK_greater}
};

static struct key DEC_LK421_row03 [] = {
 {175,	"Ctrl",	0,	       18, 7,	ControlMask,	XK_Control_L},
 {194,	"A",	0,		7, 7,	0,	XK_A},
 {199,	"S",	0,		7, 7,	0,	XK_S},
 {205,	"D",	0,		7, 7,	0,	XK_D},
 {210,	"F",	0,		7, 7,	0,	XK_F},
 {216,	"G",	0,		7, 7,	0,	XK_G},
 {221,	"H",	0,		7, 7,	0,	XK_H},
 {226,	"J",	0,		7, 7,	0,	XK_J},
 {231,	"K",	0,		7, 7,	0,	XK_K},
 {236,	"L",	0,		7, 7,	0,	XK_L},
 {242,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {251,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {189,	"Return",0,	       17, 7,	0,	XK_Return}
};

static struct key DEC_LK421_row04 [] = {
 {174,	"Shift",0,	       21, 7,	ShiftMask,	XK_Shift_L},
 {195,	"Z",	0,		7, 7,	0,	XK_Z},
 {200,	"X",	0,		7, 7,	0,	XK_X},
 {206,	"C",	0,		7, 7,	0,	XK_C},
 {211,	"V",	0,		7, 7,	0,	XK_V},
 {217,	"B",	0,		7, 7,	0,	XK_B},
 {222,	"N",	0,		7, 7,	0,	XK_N},
 {227,	"M",	0,		7, 7,	0,	XK_M},
 {232,	"<",	",",		7, 7,	0,	XK_comma,	XK_less},
 {237,	">",	".",		7, 7,	0,	XK_period,	XK_greater},
 {243,	"?",	"/",		7, 7,	0,	XK_slash,	XK_question},
 /* under WreckWindows, the right-shift key is identical to the left-shift */
 {171,	"Shift",0,	       21, 7,		ShiftMask,	XK_Shift_R}
};

static struct key DEC_LK421_row05 [] = {
 {0,	0,	0,		8, 7},
 {177, "Com-", "pose",		7, 7,	Mod1Mask, XK_Multi_key, XK_Meta_L},
 {172, "Alt",	"Function",    14, 7,	Mod2Mask, XK_Alt_L},
 {212,	" ",	0,	       49, 7,	0,	  XK_space},
 {178, "Alt",	"Function",    14, 7,	Mod2Mask, XK_Alt_R},
 {173, "Compose","Character",  14, 7,	Mod1Mask, XK_Multi_key, XK_Meta_R}
};

static struct key DEC_LK421_row10 [] = {
 {0,	0,	0,	1, 7},
 {113,	"F11",	0,	7, 7,	0,	XK_Escape, XK_F11},
 {114,	"F12",	0,	7, 7,	0,	XK_F12},
 {115,	"F13",	0,	7, 7,	0,	XK_F13},
 {116,	"F14",	0,	7, 7,	0,	XK_F14},
 {124,	"Help",	0,	7, 7,	0,	XK_Help},
 {0,	0,	0,	7, 7},
 {125,	"Do",	0,	7, 7,	0,	XK_Menu},
 {128,	"F17",	0,	7, 7,	0,	XK_F17},
 {129,	"F18",	0,	7, 7,	0,	XK_F18},
 {130,	"F19",	0,	7, 7,	0,	XK_F19},
 {131,	"F20",	0,	7, 7,	0,	XK_F20}
};

static struct key DEC_LK421_row11 [] = {
 {0,	0,	0,     46, 7},
 {161,	"PF1",	0,	7, 7,	0,	XK_KP_F1},
 {162,	"PF2",	0,	7, 7,	0,	XK_KP_F2},
 {163,	"PF3",	0,	7, 7,	0,	XK_KP_F3},
 {164,	"PF4",	0,	7, 7,	0,	XK_KP_F4},
 {138,	"Find",	0,	7, 7,	0,	XK_Find},
 {139,	"Insert",0,	7, 7,	0,	XK_Insert},
 {140,	"Remove",0,	7, 7,	0,	DXK_Remove}
};
 
static struct key DEC_LK421_row12 [] = {
 {0,	0,	0,     50, 7},
 {157,	"7",	0,	7, 7,	0,	XK_KP_7},
 {158,	"8",	0,	7, 7,	0,	XK_KP_8},
 {159,	"9",	0,	7, 7,	0,	XK_KP_9},
 {160,	"-",	0,	7, 7,	0,	XK_KP_Subtract},
 {141,	"Select",0,	7, 7,	0,	XK_Select},
 {142,	"Prev",	0,	7, 7,	0,	XK_Prior},
 {143,	"Next",	0,	7, 7,	0,	XK_Next}
};

static struct key DEC_LK421_row13 [] = {
 {0,	0,	0,     53, 7},
 {153,	"4",	0,	7, 7,	0,	XK_KP_4},
 {154,	"5",	0,	7, 7,	0,	XK_KP_5},
 {155,	"6",	0,	7, 7,	0,	XK_KP_6},
 {156,	",",	0,	7, 7,	0,	XK_KP_Separator},
 {0,	0,	0,     14, 7},
 {149,	"Enter",0,     17, 7,	0,	XK_KP_Enter}
};

static struct key DEC_LK421_row14 [] = {
 {0,	0,	0,     56, 7},
 {150,	"1",	0,	7, 7,	0,	XK_KP_1},
 {151,	"2",	0,	7, 7,	0,	XK_KP_2},
 {152,	"3",	0,	7, 7,	0,	XK_KP_3},
 {148,	".",	0,	7, 7,	0,	XK_KP_Decimal},
 {0,	0,	0,	7, 7},
 {176,	"Lock",	0,     21, 7,	LockMask,	XK_Caps_Lock}
};

static struct key DEC_LK421_row15 [] = {
 {0,	0,	0,		1, 7},
 {0,	"Ext-",	"end",		7, 7},
 {0,	0,	0,	       21, 7},
 {146,	0,	0,	       49, 7,	0,	XK_KP_0}
};

static struct row DEC_LK421_rows [] = {
  { sizeof (DEC_LK421_row00) / sizeof (struct key), 7, DEC_LK421_row00 },
  { sizeof (DEC_LK421_row01) / sizeof (struct key), 7, DEC_LK421_row01 },
  { sizeof (DEC_LK421_row02) / sizeof (struct key), 7, DEC_LK421_row02 },
  { sizeof (DEC_LK421_row03) / sizeof (struct key), 7, DEC_LK421_row03 },
  { sizeof (DEC_LK421_row04) / sizeof (struct key), 7, DEC_LK421_row04 },
  { sizeof (DEC_LK421_row05) / sizeof (struct key), 7, DEC_LK421_row05 },
  { 0, 7, 0 },
  { sizeof (DEC_LK421_row10) / sizeof (struct key), 7, DEC_LK421_row10 },
  { sizeof (DEC_LK421_row11) / sizeof (struct key), 7, DEC_LK421_row11 },
  { sizeof (DEC_LK421_row12) / sizeof (struct key), 7, DEC_LK421_row12 },
  { sizeof (DEC_LK421_row13) / sizeof (struct key), 7, DEC_LK421_row13 },
  { sizeof (DEC_LK421_row14) / sizeof (struct key), 7, DEC_LK421_row14 },
  { sizeof (DEC_LK421_row15) / sizeof (struct key), 7, DEC_LK421_row15 }
};

static struct keyboard DEC_LK421 = {
  "LK421", "Digital Equipment Corporation LK421",
  sizeof (DEC_LK421_rows) / sizeof (struct row),
  DEC_LK421_rows,
  6, 3, 3
};
