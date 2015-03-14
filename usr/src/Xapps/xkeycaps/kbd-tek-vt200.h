/* xkeycaps, Copyright (c) 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the Tektronix VT200 keyboard.
 * _TEK_KEYBOARD_TYPE(STRING) = "vt200"
 * _TEK_KEYBOARD_LAYOUT(STRING) = "ultrix"
 * Submitted by Juergen Stuber <juergen.stuber@mpi-sb.mpg.de>
 *
 * The keycodes are in decimal rather than hex because that's
 * how they're listed in the user's manual.
 *
 * BUGS:
 * Several other layout variants exist for this kind of keyboard,
 * but are not covered.
 * In order to keep the keycodes readable bottom strings are not
 * always set as they appear on the keys.
 *
 */

static struct key TEK_vt200_row0 [] = {
 {15,	"F1",	0,	7, 7,	0,	XK_F1},
 {23,	"F2",	0,	7, 7,	0,	XK_F2},
 {106,	"F3",	0,	7, 7,	0,	TekXK_Setup, XK_F3},
 {39,	"F4",	0,	7, 7,	0,	XK_F4},
 {47,	"F5",	0,	7, 7,	0,	XK_F5},
 {0,	0,	0,	5, 7},
 {55,	"F6",	0,	7, 7,	0,	XK_F6},
 {63,	"F7",	0,	7, 7,	0,	XK_F7},
 {71,	"F8",	0,	7, 7,	0,	XK_F8},
 {79,	"F9",	0,	7, 7,	0,	XK_F9},
 {87,	"F10",	0,	7, 7,	0,	XK_F10},
 {0,	0,	0,	5, 7},
 {18,	"F11",	0,	7, 7,	0,	XK_Escape},
 {19,	"F12",	0,	7, 7,	0,	XK_F12},
 {20,	"F13",	0,	7, 7,	0,	XK_F13},
 {88,	"F14",	0,	7, 7,	0,	XK_F14},
 {0,	0,	0,	5, 7},
 {89,   "Help", 0,      7, 7,   Mod5Mask, XK_Help},
 {135,  "Do",   0,     14, 7,   0,      XK_Menu},
 {0,	0,	0,	5, 7},
 {136,	"F17",	0,	7, 7,	0,	XK_F17},
 {137,	"F18",	0,	7, 7,	0,	XK_F18},
 {138,	"F19",	0,	7, 7,	0,	XK_F19},
 {139,	"F20",	0,	7, 7,	0,	XK_F20}
};

static struct key TEK_vt200_row1 [] = {
 {0,	0,	0,	6, 7},
 {22,	"~",	"`",	7, 7,	0,	XK_grave, XK_asciitilde},
 {30,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {38,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {46,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {45,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {54,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {62,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {69,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {70,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {78,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {77,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {86,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {93,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {110, "LeftArrow",0,  10, 7,	0,	XK_Delete},
 {0,	0,	0,	6, 7},
 {118,	"Find",	0,	7, 7,	0,	XK_Find},
 {111,	"Insert", 0,	7, 7,	0,	XK_Insert},
 {108,	"Remove", 0,	7, 7,	0,	TekXK_Remove},
 {0,	0,	0,	5, 7},
 {16,	"PF1",	0,	7, 7,	0,	XK_KP_F1},
 {126,	"PF2",	0,	7, 7,	0,	XK_KP_F2},
 {103,	"PF3",	0,	7, 7,	0,	XK_KP_F3},
 {95,	"PF4",	0,	7, 7,	0,	XK_KP_F4}
};

static struct key TEK_vt200_row2 [] = {
 {0,	0,	0,	6, 7},
 {21,	"Tab",	0,	10, 7,	0,	XK_Tab},
 {29,	"Q",	0,	7, 7,	0,	XK_Q},
 {37,	"W",	0,	7, 7,	0,	XK_W},
 {44,	"E",	0,	7, 7,	0,	XK_E},
 {53,	"R",	0,	7, 7,	0,	XK_R},
 {52,	"T",	0,	7, 7,	0,	XK_T},
 {61,	"Y",	0,	7, 7,	0,	XK_Y},
 {68,	"U",	0,	7, 7,	0,	XK_U},
 {75,	"I",	0,	7, 7,	0,	XK_I},
 {76,	"O",	0,	7, 7,	0,	XK_O},
 {85,	"P",	0,	7, 7,	0,	XK_P},
 {92,	"{",	"[",	7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {99,	"}",	"]",	7, 7,	0,	XK_bracketright,XK_braceright},
 {0,	0,	0,      1, 7},
 {98,	"Return", 0,    7, 14,	0,	XK_Return},
 {0,	0,	0,	5, 7},
 {109,	"Select", 0,	7, 7,	0,	XK_Select},
 {119,	"Prev", 0,	7, 7,	0,	XK_Prior},
 {117,	"Next", 0,	7, 7,	0,	XK_Next},
 {0,	0,	0,	5, 7},
 {116,	"7",	0,	7, 7,	0,	XK_KP_7},
 {125,	"8",    0,	7, 7,	0, 	XK_KP_8},
 {133,	"9",	0,	7, 7,	0,	XK_KP_9},
 {140,	"-",	0,	7, 7,	0,	XK_KP_Subtract,	XK_KP_Subtract}
};

static struct key TEK_vt200_row3 [] = {
 {25,	"Ctrl", 0,	7, 7,	ControlMask,	XK_Control_L},
 {28,   "Caps",	"Lock",	10, 7,	LockMask,	XK_Caps_Lock},
 {36,	"A",	0,	7, 7,	0,	XK_A},
 {35,	"S",	0,	7, 7,	0,	XK_S},
 {43,	"D",	0,	7, 7,	0,	XK_D},
 {51,	"F",	0,	7, 7,	0,	XK_F},
 {60,	"G",	0,	7, 7,	0,	XK_G},
 {59,	"H",	0,	7, 7,	0,	XK_H},
 {67,	"J",	0,	7, 7,	0,	XK_J},
 {74,	"K",	0,	7, 7,	0,	XK_K},
 {83,	"L",	0,	7, 7,	0,	XK_L},
 {84,	":",	";",	7, 7,	0,	XK_semicolon,	XK_colon},
 {90,	"\"",	"'",	7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {91,	"|",	"\\",	7, 7,	0,	XK_backslash,XK_bar},
 {0,	0,	0,     19, 7},
 {107,  "UpArrow", 0,	7, 7,	0,	XK_Up},
 {0,	0,	0,     12, 7},
 {115,  "4",	0,	7, 7,	0,	XK_KP_4},
 {123,  "5",	0,	7, 7,	0,	XK_KP_5},
 {124,  "6",	0,	7, 7,	0,	XK_KP_6},
 {132,	",",	0,	7, 7,	0,	XK_KP_Separator}
};

static struct key TEK_vt200_row4 [] = {
 {26,	"Shift",0,     15, 7,	ShiftMask, XK_Shift_L},
 {17,   ">",    "<",    7, 7,   0,      XK_less, XK_greater},
 {34,	"Z",	0,     	7, 7,	0,	XK_Z},
 {42,	"X",	0,     	7, 7,	0,	XK_X},
 {41,	"C",	0,     	7, 7,	0,	XK_C},
 {50,	"V",	0,     	7, 7,	0,	XK_V},
 {58,	"B",	0,     	7, 7,	0,      XK_B},
 {57,	"N",	0,     	7, 7,	0,	XK_N},
 {66,	"M",	0,     	7, 7,	0,	XK_M},
 {73,	",",	",",	7, 7,	0,	XK_comma, XK_comma},
 {81,	".",	".",	7, 7,	0,	XK_period,XK_period},
 {82,	"?",	"/",	7, 7,	0,	XK_slash, XK_question},
 {97,	"Shift",0,     15, 7,	ShiftMask, XK_Shift_L},
 {0,	0,	0,      6, 7},
 {105,	"LeftArrow",	0,	7, 7,	0,        XK_Left},
 {104,	"DownArrow",	0,	7, 7,	0,	  XK_Down},
 {114,	"RightArrow",	0,	7, 7,	0,	  XK_Right},
 {0,	0,	0,	5, 7},
 {113,	"1",	0,	7, 7,	0,	XK_KP_1},
 {122,	"2",	0,	7, 7,	0,	XK_KP_2},
 {130,  "3",	0,	7, 7,	0,	XK_KP_3},
 {129,"Enter",	0,	7, 14,	0,	XK_KP_Enter}
};

static struct key TEK_vt200_row5 [] = {
 {0,    0,	0,	6, 7},
 {33,	"Compose", 0,  10, 7,	Mod1Mask, XK_Alt_L},
 {49,	" ",	0,     69, 7,	0,	  XK_space},
 {0,    0,	0,     54, 7},
 {120,	"0",	0,     14, 7,	0,        XK_KP_0},
 {121,	".",	0,	7, 7,	0,        XK_KP_Decimal}
};

static struct row TEK_vt200_rows [] = {
  { sizeof (TEK_vt200_row0) / sizeof (struct key), 7, TEK_vt200_row0 },
  { 0, 4, 0 },
  { sizeof (TEK_vt200_row1) / sizeof (struct key), 7, TEK_vt200_row1 },
  { sizeof (TEK_vt200_row2) / sizeof (struct key), 7, TEK_vt200_row2 },
  { sizeof (TEK_vt200_row3) / sizeof (struct key), 7, TEK_vt200_row3 },
  { sizeof (TEK_vt200_row4) / sizeof (struct key), 7, TEK_vt200_row4 },
  { sizeof (TEK_vt200_row5) / sizeof (struct key), 7, TEK_vt200_row5 },
};

static struct keyboard TEK_vt200 = {
  "TEKvt200u", "Tektronix VT200 North American (Ultrix)",
  sizeof (TEK_vt200_rows) / sizeof (struct row),
  TEK_vt200_rows,
  6, 3, 3
};
