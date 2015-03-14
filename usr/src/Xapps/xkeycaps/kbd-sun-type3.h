/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the Sun type 3 keyboard.
 */

static struct key Sun_type3_row0 [] = {
 {8,	"L1",	0,	7, 7,	0,	XK_F11},
 {10,	"L2",	0,	7, 7,	0,	XK_F12},
 {0,	0,	0,	4, 7},
 {12,	"F1",	0,	7, 7,	0,	XK_F1},
 {13,	"F2",	0,	7, 7,	0,	XK_F2},
 {15,	"F3",	0,	14, 7,	0,	XK_F3},
 {17,	"F4",	0,	14, 7,	0,	XK_F4},
 {19,	"F5",	0,	14, 7,	0,	XK_F5},
 {21,	"F6",	0,	14, 7,	0,	XK_F6},
 {23,	"F7",	0,	14, 7,	0,	XK_F7},
 {24,	"F8",	0,	7, 7,	0,	XK_F8},
 {25,	"F9",	0,	7, 7,	0,	XK_F9},
 {50,	"Back ","Space",7, 7,	0,	XK_BackSpace},
 {0,	0,	0,	4, 7},
 {28,	"R1",	0,	7, 7,	0,	XK_F21},
 {29,	"R2",	0,	7, 7,	0,	XK_F22},
 {30,	"R3",	0,	7, 7,	0,	XK_F23}
};

static struct key Sun_type3_row1 [] = {
 {32,	"L3",	0,	7, 7,	0,	XK_F13},
 {33,	"L4",	0,	7, 7,	0,	XK_F14},
 {0,	0,	0,	4, 7},
 {36,	"Esc",	0,	7, 7,	0,	XK_Escape},
 {37,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {38,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {39,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {40,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {41,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {42,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {43,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {44,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {45,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {46,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {47,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {48,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {95,	"\\",	"|",	7, 7,	0,	XK_bar,		XK_backslash},
 {49,	"~",	"`",	7, 7,	0,	XK_grave,	XK_asciitilde},
 {0,	0,	0,	4, 7},
 {52,	"R4",	0,	7, 7,	0,	XK_F24},
 {53,	"R5",	0,	7, 7,	0,	XK_F25},
 {54,	"R6",	0,	7, 7,	0,	XK_F26}
};

static struct key Sun_type3_row2 [] = {
 {56,	"L5",	0,		7, 7,	0,	XK_F15},
 {58,	"L6",	0,		7, 7,	0,	XK_F16},
 {0,	0,	0,		4, 7},
 {60,	"Tab",	0,		10, 7,	0,	XK_Tab},
 {61,	"Q",	0,		7, 7,	0,	XK_Q},
 {62,	"W",	0,		7, 7,	0,	XK_W},
 {63,	"E",	0,		7, 7,	0,	XK_E},
 {64,	"R",	0,		7, 7,	0,	XK_R},
 {65,	"T",	0,		7, 7,	0,	XK_T},
 {66,	"Y",	0,		7, 7,	0,	XK_Y},
 {67,	"U",	0,		7, 7,	0,	XK_U},
 {68,	"I",	0,		7, 7,	0,	XK_I},
 {69,	"O",	0,		7, 7,	0,	XK_O},
 {70,	"P",	0,		7, 7,	0,	XK_P},
 {71,	"{",	"[",		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {72,	"}",	"]",		7, 7,	0,	XK_bracketright,XK_braceright},
 {73,	"Delete", 0,		11, 7,	0,	XK_Delete},
 {0,	0,	0,		4, 7},
 {75,	"R7",	0,		7, 7,	0,	XK_F27},
 {76,	"R8",	"UpArrow",	7, 7,	0,	XK_Up,		XK_F28},
 {77,	"R9",	0,		7, 7,	0,	XK_F29}
};

static struct key Sun_type3_row3 [] = {
 {79,	"L7",	0,		7, 7,	0,	XK_F17},
 {80,	"L8",	0,		7, 7,	0,	XK_F18},
 {0,	0,	0,		4, 7,	0,	0},
 {83,"Control",	0,		13, 7,	ControlMask,	XK_Control_L},
 {84,	"A",	0,		7, 7,	0,	XK_A},
 {85,	"S",	0,		7, 7,	0,	XK_S},
 {86,	"D",	0,		7, 7,	0,	XK_D},
 {87,	"F",	0,		7, 7,	0,	XK_F},
 {88,	"G",	0,		7, 7,	0,	XK_G},
 {89,	"H",	0,		7, 7,	0,	XK_H},
 {90,	"J",	0,		7, 7,	0,	XK_J},
 {91,	"K",	0,		7, 7,	0,	XK_K},
 {92,	"L",	0,		7, 7,	0,	XK_L},
 {93,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {94,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {96, "Return",	0,		15, 7,	0,	XK_Return},
 {0,	0,	0,		4, 7},
 {98,	"R10",	"LeftArrow",	7, 7,	0,	XK_Left,	XK_F30},
 {99,	"R11",	0,		7, 7,	0,	XK_F31},
 {100,	"R12",	"RightArrow",	7, 7,	0,	XK_Right,	XK_F32}
};

static struct key Sun_type3_row4 [] = {
 {102,	"L9",	0,		7, 7,	0,		XK_F19},
 {104,	"L10",	0,		7, 7,	0,		XK_F20},
 {0,	0,	0,		4, 7},
 {106,	"Shift",0,		16, 7,	ShiftMask,	XK_Shift_L},
 {107,	"Z",	0,		7, 7,	0,		XK_Z},
 {108,	"X",	0,		7, 7,	0,		XK_X},
 {109,	"C",	0,		7, 7,	0,		XK_C},
 {110,	"V",	0,		7, 7,	0,		XK_V},
 {111,	"B",	0,		7, 7,	0,		XK_B},
 {112,	"N",	0,		7, 7,	0,		XK_N},
 {113,	"M",	0,		7, 7,	0,		XK_M},
 {114,	"<",	",",		7, 7,	0,		XK_comma, XK_less},
 {115,	">",	".",		7, 7,	0,		XK_period,XK_greater},
 {116,	"?",	"/",		7, 7,	0,		XK_slash, XK_question},
 {117, "Shift",	0,		12, 7,	ShiftMask,	XK_Shift_R},
 {118, "Line ",	"Feed",		7, 7,	0,		XK_Linefeed},
 {0,	0,	0,		4, 7},
 {119, "R13",	0,		7, 7,	0,		XK_R13},
 {120, "R14",	"DownArrow",	7, 7,	0,		XK_Down,  XK_F34},
 {121, "R15",	0,		7, 7,	0,		XK_F35}
};

static struct key Sun_type3_row5 [] = {
 {0,	0,	0,	18, 7},
 {126, "Caps",	0,	9, 7,	LockMask,	XK_Caps_Lock},
 {127, "Left",	0,	10, 7,	Mod1Mask,	XK_Meta_L},
 {128,	" ",	0,	65, 7,	0,		XK_space},
 {129, "Right",	0,	10, 7,	Mod1Mask,	XK_Meta_R},
 {26,  "Alternate",0,	11, 7,	0,		XK_Alt_R}
};

static struct row Sun_type3_rows [] = {
  { sizeof (Sun_type3_row0) / sizeof (struct key), 7, Sun_type3_row0 },
  { sizeof (Sun_type3_row1) / sizeof (struct key), 7, Sun_type3_row1 },
  { sizeof (Sun_type3_row2) / sizeof (struct key), 7, Sun_type3_row2 },
  { sizeof (Sun_type3_row3) / sizeof (struct key), 7, Sun_type3_row3 },
  { sizeof (Sun_type3_row4) / sizeof (struct key), 7, Sun_type3_row4 },
  { sizeof (Sun_type3_row5) / sizeof (struct key), 7, Sun_type3_row5 },
};

static struct keyboard Sun_type3 = {
  "Sun3", "Sun type3 (MIT layout)",
  sizeof (Sun_type3_rows) / sizeof (struct row),
  Sun_type3_rows,
  6, 3, 3
};
