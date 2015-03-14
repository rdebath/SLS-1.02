/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the TI Explorer Lisp Machine keyboard.
 * Ain't it studly?
 */

static struct key Explorer_row0 [] = {
 {8,	"Help",		0,	14, 7,	0,		XK_Help},
 {0,	0,		0,	3, 7},
 {15,	"System",	0,	9, 7,	0,		XK_F24},
 {16,	"Network",	0,	9, 7,	0,		XK_F25},
 {17,	"Status",	0,	9, 7,	0,		XK_F26},
 {18,	"Term",		0,	9, 7,	0,		XK_F27},
 {0,	0,		0,	2, 7},
 {10,	"Caps",	"Lock",		8, 7,	LockMask,	XK_Caps_Lock},
 {11,	"Bold",	"Lock",		8, 7,	0,		XK_F21},
 {12,	"Italic","Lock",	8, 7,	0,		XK_F22},
 {13,	"Mode",	"Lock",		8, 7,	Mod5Mask,	XK_F23},
 {0,	0,		0,	2, 7},
 {20,	"Clear", "Screen",	9, 7,	0,		XK_F28},
 {21,	"Clear", "Input",	9, 7,	0,		XK_Clear},
 {22,	"Undo",		0,	9, 7,	0,		XK_Undo},
 {23,	"End",		0,	9, 7,	0,		XK_End},
 {0,	0,		0,	4, 7},
 {24,	"Left",		0,	7, 7,	0,		XK_F29},
 {25,	"Middle",	0,	7, 7,	0,		XK_F30},
 {26,	"Right",	0,	7, 7,	0,		XK_F31},
 {0,	0,		0,	4, 7},
 {27,	"F1",		0,	7, 7,	0,		XK_KP_F1},
 {28,	"F2",		0,	7, 7,	0,		XK_KP_F2},
 {29,	"F3",		0,	7, 7,	0,		XK_KP_F3},
 {30,	"F4",		0,	7, 7,	0,		XK_KP_F4}
};

static struct key Explorer_row2 [] = {
 {40,	"Resume", 0,		14, 7,	0,	XK_F32, 0},
 {0,	0,	0,		3, 7},
 {42,	"Escape", 0,		9, 7,	0,	XK_Escape},
 {43,	"!",	"1",		7, 7,	0,	XK_1,		XK_exclam},
 {44,	"@",	"2",		7, 7,	0,	XK_2,		XK_at},
 {45,	"#",	"3",		7, 7,	0,	XK_3,		XK_numbersign},
 {46,	"$",	"4",		7, 7,	0,	XK_4,		XK_dollar},
 {47,	"%",	"5",		7, 7,	0,	XK_5,		XK_percent},
 {48,	"^",	"6",		7, 7,	0,	XK_6,	       XK_asciicircum},
 {49,	"&",	"7",		7, 7,	0,	XK_7,		XK_ampersand},
 {50,	"*",	"8",		7, 7,	0,	XK_8,		XK_asterisk},
 {51,	"(",	"9",		7, 7,	0,	XK_9,		XK_parenleft},
 {52,	")",	"0",		7, 7,	0,	XK_0,		XK_parenright},
 {53,	"_",	"-",		8, 7,	0,	XK_minus,	XK_underscore},
 {54,	"+",	"=",		7, 7,	0,	XK_equal,	XK_plus},
 {55,	"{",	"`",		7, 7,	0,	XK_grave,	XK_braceleft},
 {56,	"}",	"~",		7, 7,	0,	XK_asciitilde,	XK_braceright},
 {0,	0,	0,		29, 7},
 {57,	"=",	0,		7, 7,	0,	XK_KP_Equal},
 {58,	"+",	0,		7, 7,	0,	XK_KP_Add},
 {59,	"Space",0,		7, 7,	0,	XK_KP_Space},
 {60,	"Tab",	0,		7, 7,	0,	XK_KP_Tab}
};

static struct key Explorer_row3 [] = {
 {61,	"Break", 0,		14, 7,	0,	XK_Break},
 {0,	0,	0,		3, 7},
 {63,	"Tab",	0,		13, 7,	0,	XK_Tab},
 {64,	"Q",	0,		7, 7,	0,	XK_Q},
 {65,	"W",	0,		7, 7,	0,	XK_W},
 {66,	"E",	0,		7, 7,	0,	XK_E},
 {67,	"R",	0,		7, 7,	0,	XK_R},
 {68,	"T",	0,		7, 7,	0,	XK_T},
 {69,	"Y",	0,		7, 7,	0,	XK_Y},
 {70,	"U",	0,		7, 7,	0,	XK_U},
 {71,	"I",	0,		7, 7,	0,	XK_I},
 {72,	"O",	0,		7, 7,	0,	XK_O},
 {73,	"P",	0,		7, 7,	0,	XK_P},
 {74,	"[",	"(",		7, 7,	0,	XK_parenleft, XK_bracketleft},
 {75,	"]",	")",		7, 7,	0,	XK_parenright,XK_bracketright},
 {0,	0,	0,		4, 7},
 {77,	"|",	"\\",		7, 7,	0,	XK_backslash, XK_bar},
 {0,	0,	0,		11, 7},
 {78,	"UpArrow",0,		7, 7,	0,	XK_Up},
 {0,	0,	0,		11, 7},
 {79,	"7",	0,		7, 7,	0,	XK_KP_7},
 {80,	"8",	0,		7, 7,	0,	XK_KP_8},
 {81,	"9",	0,		7, 7,	0,	XK_KP_9},
 {82,	"-",	0,		7, 7,	0,	XK_KP_Subtract}
};

static struct key Explorer_row4 [] = {
 {83,	"Abort",0,		14, 7,	0,	XK_Cancel},
 {0,	0,	0,		3, 7},
 {86,	"Rubout",0,		14, 7,	0,	XK_Delete},
 {87,	"A",	0,		7, 7,	0,	XK_A},
 {88,	"S",	0,		7, 7,	0,	XK_S},
 {89,	"D",	0,		7, 7,	0,	XK_D},
 {90,	"F",	0,		7, 7,	0,	XK_F},
 {91,	"G",	0,		7, 7,	0,	XK_G},
 {92,	"H",	0,		7, 7,	0,	XK_H},
 {93,	"J",	0,		7, 7,	0,	XK_J},
 {94,	"K",	0,		7, 7,	0,	XK_K},
 {95,	"L",	0,		7, 7,	0,	XK_L},
 {96,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {97,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {98,	"Return", 0,		10, 7,	0,	XK_Return},
 {99,	"Line ", "Feed",	7, 7,	0,	XK_Linefeed},
 {0,	0,	0,		4, 7},
 {100,	"LeftArrow",	0,	7, 7,	0,	XK_Left},
 {101,	"Purge",	"Disk",	7, 7,	0,	XK_Home},
 {102,	"RightArrow",	0,	7, 7,	0,	XK_Right},
 {0,	0,	0,		4, 7},
 {103,	"4",	0,		7, 7,	0,	XK_KP_4},
 {104,	"5",	0,		7, 7,	0,	XK_KP_5},
 {105,	"6",	0,		7, 7,	0,	XK_KP_6},
 {106,	",",	0,		7, 7,	0,	XK_KP_Separator}
};

static struct key Explorer_row5 [] = {
 {0,	0,	0,		17, 7},
 {109,	"Sym-",	"bol",		7, 7,	Mod4Mask,	XK_R13},
 {110,	"Shift",0,		10, 7,	ShiftMask,	XK_Shift_L},
 {111,	"Z",	0,		7, 7,	0,		XK_Z},
 {112,	"X",	0,		7, 7,	0,		XK_X},
 {113,	"C",	0,		7, 7,	0,		XK_C},
 {114,	"V",	0,		7, 7,	0,		XK_V},
 {115,	"B",	0,		7, 7,	0,		XK_B},
 {116,	"N",	0,		7, 7,	0,		XK_N},
 {117,	"M",	0,		7, 7,	0,		XK_M},
 {118,	"<",	",",		7, 7,	0,		XK_comma, XK_less},
 {119,	">",	".",		7, 7,	0,		XK_period,XK_greater},
 {120,	"?",	"/",		7, 7,	0,		XK_slash, XK_question},
 {121,	"Shift",0,		11, 7,	ShiftMask,	XK_Shift_R},
 {123,	"Symbol",0,		10, 7,	Mod4Mask,	XK_F34},
 {0,	0,	0,		11, 7},
 {124,	"DownArrow",0,		7, 7,	0,	XK_Down},
 {0,	0,	0,		11, 7},
 {125,	"1",	0,		7, 7,	0,	XK_KP_1},
 {126,	"2",	0,		7, 7,	0,	XK_KP_2},
 {127,	"3",	0,		7, 7,	0,	XK_KP_3},
 {134,	"Enter",0,		7, 14,	0,	XK_KP_Enter}
};

static struct key Explorer_row6 [] = {
 {14,	"Hyper",0,	7, 7,	Mod3Mask,	XK_Hyper_L},
 {33,	"Super",0,	7, 7,	Mod4Mask,	XK_Super_L},
 {0,	0,	0,	3, 7},
 {34,	"Meta",	0,	12, 7,	Mod1Mask,	XK_Meta_L},
 {35,	"Ctrl",	0,	12, 7,	ControlMask,	XK_Control_L},
 {130,	" ",	0,	56, 7,	0,		XK_space},
 {36,	"Ctrl",	0,	7, 7,	ControlMask,	XK_Control_R},
 {37,	"Meta",	0,	7, 7,	Mod1Mask,	XK_Meta_R},
 {38,	"Super",0,	7, 7,	Mod2Mask,	XK_Super_R},
 {39,	"Hyper",0,	7, 7,	Mod3Mask,	XK_Hyper_R},
 {0,	0,	0,	29, 7},
 {132,	"0",	0,	14, 7,	0,		XK_KP_0},
 {133,	".",	0,	7, 7,	0,		XK_KP_Decimal}
};

static struct row Explorer_rows [] = {
	{ sizeof (Explorer_row0) / sizeof (struct key), 7, Explorer_row0 },
	{ 0, 4, 0 },
	{ sizeof (Explorer_row2) / sizeof (struct key), 7, Explorer_row2 },
	{ sizeof (Explorer_row3) / sizeof (struct key), 7, Explorer_row3 },
	{ sizeof (Explorer_row4) / sizeof (struct key), 7, Explorer_row4 },
	{ sizeof (Explorer_row5) / sizeof (struct key), 7, Explorer_row5 },
	{ sizeof (Explorer_row6) / sizeof (struct key), 7, Explorer_row6 }
};

static struct keyboard Explorer = {
  "Explorer", "Texas Instruments Explorer",
  sizeof (Explorer_rows) / sizeof (struct row),
  Explorer_rows,
  5, 3, 3
};
