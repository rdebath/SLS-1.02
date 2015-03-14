/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the DEC LK201 keyboard.
 */

#ifndef NO_DEC_KEYSYMS
/* This file comes with the MIT distribution, but won't necessarily
   be present in all X implementations. */
# include <X11/DECkeysym.h>
#else
# define DXK_Remove 0x1000FF00
#endif

static struct key DEC_LK201_row0 [] = {
 {86,	"F1",	0,	7, 7,	0,	XK_F1},
 {87,	"F2",	0,	7, 7,	0,	XK_F2},
 {88,	"F3",	0,	7, 7,	0,	XK_F3},
 {89,	"F4",	0,	7, 7,	0,	XK_F4},
 {90,	"F5",	0,	7, 7,	0,	XK_F5},
 {0,	0,	0,	7, 7},
 {100,	"F6",	0,	7, 7,	0,	XK_F6},
 {101,	"F7",	0,	7, 7,	0,	XK_F7},
 {102,	"F8",	0,	7, 7,	0,	XK_F8},
 {103,	"F9",	0,	7, 7,	0,	XK_F9},
 {104,	"F10",	0,	7, 7,	0,	XK_F10},
 {0,	0,	0,	7, 7},
 {113,	"F11",	0,	7, 7,	0,	XK_Escape, XK_F11},
 {114,	"F12",	0,	7, 7,	0,	XK_F12},
 {115,	"F13",	0,	7, 7,	0,	XK_F13},
 {116,	"F14",	0,	7, 7,	0,	XK_F14},
 {0,	0,	0,	7, 7},
 {124,	"Help",	0,	8, 7,	0,	XK_Help},
 {125,	"Do",	0,	16, 7,	0,	XK_Menu},
 {0,	0,	0,	7, 7},
 {128,	"F17",	0,	7, 7,	0,	XK_F17},
 {129,	"F18",	0,	7, 7,	0,	XK_F18},
 {130,	"F19",	0,	7, 7,	0,	XK_F19},
 {131,	"F20",	0,	7, 7,	0,	XK_F20}
};

static struct key DEC_LK201_row2 [] = {
 {0,	0,	0,	5, 7},
 {191,	"~",	"`",	7, 7,	0,	XK_grave,	XK_asciitilde},
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
 {188,	"<X|",	0,	11, 7,	0,	XK_Delete},
 {0,	0,	0,	12, 7},
 {138,	"Find",	0,	8, 7,	0,	XK_Find},
 {139,	"Insert","Here",8, 7,	0,	XK_Insert},
 {140, "Re-","move",	8, 7,	0,	DXK_Remove},
 {0,	0,	0,	7, 7},
 {161,	"PF1",	0,	7, 7,	0,	XK_KP_F1},
 {162,	"PF2",	0,	7, 7,	0,	XK_KP_F2},
 {163,	"PF3",	0,	7, 7,	0,	XK_KP_F3},
 {164,	"PF4",	0,	7, 7,	0,	XK_KP_F4}
};
 
static struct key DEC_LK201_row3 [] = {
 {0,	0,	0,		5, 7},
 {190,"Tab",	0,		12, 7,	0,	XK_Tab},
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
 {250,	"{",	"[",		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {246,	"}",	"]",		7, 7,	0,	XK_bracketright,XK_braceright},
 {0,	0,	0,		2, 7},
 {189,	"Return",0,		8, 14,	0,	XK_Return},
 {0,	0,	0,		8, 7},
 {141,	"Select",0,		8, 7,	0,	XK_Select},
 {142,	"Prev",	"Screen",	8, 7,	0,	XK_Prior},
 {143,	"Next",	"Screen",	8, 7,	0,	XK_Next},
 {0,	0,	0,		7, 7},
 {157,	"7",	0,		7, 7,	0,	XK_KP_7},
 {158,	"8",	0,		7, 7,	0,	XK_KP_8},
 {159,	"9",	0,		7, 7,	0,	XK_KP_9},
 {160,	"-",	0,		7, 7,	0,	XK_KP_Subtract}
};

static struct key DEC_LK201_row4 [] = {
 {175,	"Ctrl",	0,		7, 7,	ControlMask,	XK_Control_L},
 {176,	"Lock",	0,		12, 7,	LockMask,	XK_Caps_Lock},
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
 {247,	"|",	"\\",		7, 7,	0,	XK_backslash,	XK_bar},
 {0,	0,	0,		24, 7},
 {170,	"UpArrow",0,		8, 7,	0,	XK_Up},
 {0,	0,	0,		15, 7},
 {153,	"4",	0,		7, 7,	0,	XK_KP_4},
 {154,	"5",	0,		7, 7,	0,	XK_KP_5},
 {155,	"6",	0,		7, 7,	0,	XK_KP_6},
 {156,	",",	0,		7, 7,	0,	XK_KP_Separator}
};

static struct key DEC_LK201_row5 [] = {
 {174,	"Shift",0,		16, 7,	ShiftMask,		XK_Shift_L},
 {201,	">",	"<",		7, 7,	0,	XK_less,	XK_greater},
 {195,	"Z",	0,		7, 7,	0,	XK_Z},
 {200,	"X",	0,		7, 7,	0,	XK_X},
 {206,	"C",	0,		7, 7,	0,	XK_C},
 {211,	"V",	0,		7, 7,	0,	XK_V},
 {217,	"B",	0,		7, 7,	0,	XK_B},
 {222,	"N",	0,		7, 7,	0,	XK_N},
 {227,	"M",	0,		7, 7,	0,	XK_M},
 /* these two are the only difference between the LK201 and LK401 */
 {232,	",",	",",		7, 7,	0,	XK_comma,	XK_comma},
 {237,	".",	".",		7, 7,	0,	XK_period,	XK_period},
 {243,	"?",	"/",		7, 7,	0,	XK_slash,	XK_question},
 /* under WreckWindows, the right-shift key is identical to the left-shift */
 {171,	"Shift",0,		16, 7,		ShiftMask,	XK_Shift_R},
 {0,	0,	0,		10, 7},
 {167,"LeftArrow", 0,		8, 7,	0,	XK_Left},
 {169,"DownArrow", 0,		8, 7,	0,	XK_Down},
 {168,"RightArrow",0,		8, 7,	0,	XK_Right},
 {0,	0,	0,		7, 7,	0,	XK_KP_0},
 {150,	"1",	0,		7, 7,	0,	XK_KP_1},
 {151,	"2",	0,		7, 7,	0,	XK_KP_2},
 {152,	"3",	0,		7, 7,	0,	XK_KP_3},
 {149,"Enter",	0,		7, 14,	0,	XK_KP_Enter}
};

static struct key DEC_LK201_row6 [] = {
 {0,	0,	0,		9, 7},
 {177, "Compose","Character",	16, 7,	Mod1Mask,	XK_Multi_key},
 {212,	" ",	0,		64, 7,	0,		XK_space},
 {0,	0,	0,		61, 7},
 {146,	"0",	0,		14, 7,	0,		XK_KP_0},
 {148,	".",	0,		7, 7,	0,		XK_KP_Decimal}
};

static struct row DEC_LK201_rows [] = {
  { sizeof (DEC_LK201_row0) / sizeof (struct key), 7, DEC_LK201_row0 },
  { 0, 7, 0 },
  { sizeof (DEC_LK201_row2) / sizeof (struct key), 7, DEC_LK201_row2 },
  { sizeof (DEC_LK201_row3) / sizeof (struct key), 7, DEC_LK201_row3 },
  { sizeof (DEC_LK201_row4) / sizeof (struct key), 7, DEC_LK201_row4 },
  { sizeof (DEC_LK201_row5) / sizeof (struct key), 7, DEC_LK201_row5 },
  { sizeof (DEC_LK201_row6) / sizeof (struct key), 7, DEC_LK201_row6 }
};

static struct keyboard DEC_LK201 = {
  "LK201", "Digital Equipment Corporation LK201",
  sizeof (DEC_LK201_rows) / sizeof (struct row),
  DEC_LK201_rows,
  6, 3, 3
};
