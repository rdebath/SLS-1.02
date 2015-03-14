/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the SCO ODT 110 keyboard.
 * By Steven W. Orr <steveo@world.std.com>.
 */

static struct key SCO_ODT_110_row1 [] = {
 {8,"Esc",	0,		8, 8,	0,	XK_Escape},
 {0,	0,	0,		8, 8},
 {66,	"F1",	0,		8, 8,	0,	XK_F1},
 {67,	"F2",	0,		8, 8,	0,	XK_F2},
 {68,	"F3",	0,		8, 8,	0,	XK_F3},
 {69,	"F4",	0,		8, 8,	0,	XK_F4},
 {0,	0,	0,		4, 8},
 {70,	"F5",	0,		8, 8,	0,	XK_F5},
 {71,	"F6",	0,		8, 8,	0,	XK_F6},
 {72,	"F7",	0,		8, 8,	0,	XK_F7},
 {73,	"F8",	0,		8, 8,	0,	XK_F8},
 {0,	0,	0,		4, 8},
 {74,	"F9",	0,		8, 8,	0,	XK_F9},
 {75,	"F10",	0,		8, 8,	0,	XK_F10},
 {94,	"F11",	0,		8, 8,	0,	XK_F11},
 {95,	"F12",	0,		8, 8,	0,	XK_F12},
 {0,	0,	0,		4, 8},
 {150,	"Print", "SysRq",	8, 8,	0,	XK_Print},
 {77,	"Scroll","Lock",	8, 8,	0,	XK_Cancel},
 {149,	"Pause", "Break",	8, 8,	0,	XK_Pause},
};

static struct key SCO_ODT_110_row2 [] = {
 {48,	"~",	"`",		8, 8,	0,	XK_grave,	XK_asciitilde},
 {9,	"!",	"1",		8, 8,	0,	XK_1,		XK_exclam},
 {10,	"@",	"2",		8, 8,	0,	XK_2,		XK_at},
 {11,	"#",	"3",		8, 8,	0,	XK_3,		XK_numbersign},
 {12,	"$",	"4",		8, 8,	0,	XK_4,		XK_dollar},
 {13,	"%",	"5",		8, 8,	0,	XK_5,		XK_percent},
 {14,	"^",	"6",		8, 8,	0,	XK_6,	       XK_asciicircum},
 {15,	"&",	"7",		8, 8,	0,	XK_7,		XK_ampersand},
 {16,	"*",	"8",		8, 8,	0,	XK_8,		XK_asterisk},
 {17,	"(",	"9",		8, 8,	0,	XK_9,		XK_parenleft},
 {18,	")",	"0",		8, 8,	0,	XK_0,		XK_parenright},
 {19,	"_",	"-",		8, 8,	0,	XK_minus,	XK_underscore},
 {20,	"+",	"=",		8, 8,	0,	XK_equal,	XK_plus},
 {21,	"Backspace", 0,		16, 8,	0,	XK_BackSpace},
 {0,	0,	0,		4, 8},
 {137,	"Insert", 0,		8, 8,	0,	XK_Insert},
 {139,	"Home",	0,		8, 8,	0,	XK_Home},
 {141,   "Page","up",		8, 8,	0,	XK_Prior},
 {0,	0,	0,		4, 8},
 {76,	"Num",	"Lock",		8, 8,	0,	XK_Num_Lock,	XK_Num_Lock},
 {147,	"/",	0,		8, 8,	0,	XK_KP_Divide},
 {62,	"*",	0,		8, 8,	0,	XK_KP_Multiply},
 {81,	"-",	0,		8, 8,	0,	XK_KP_Subtract}
};

static struct key SCO_ODT_110_row3 [] = {
 {22,	"Tab",	0,		12, 8,	0,	XK_Tab},
 {23,	"Q",	0,		8, 8,	0,	XK_Q},
 {24,	"W",	0,		8, 8,	0,	XK_W},
 {25,	"E",	0,		8, 8,	0,	XK_E},
 {26,	"R",	0,		8, 8,	0,	XK_R},
 {27,	"T",	0,		8, 8,	0,	XK_T},
 {28,	"Y",	0,		8, 8,	0,	XK_Y},
 {29,	"U",	0,		8, 8,	0,	XK_U},
 {30,	"I",	0,		8, 8,	0,	XK_I},
 {31,	"O",	0,		8, 8,	0,	XK_O},
 {32,	"P",	0,		8, 8,	0,	XK_P},
 {33,	"{",	"[",		8, 8,	0,	XK_bracketleft,	XK_braceleft},
 {34,	"}",	"]",		8, 8,	0,	XK_bracketright,XK_braceright},
 {50,	"|",	"\\",		12, 8,	0,	XK_backslash,	XK_bar},
 {0,	0,	0,		4, 8},
 {138,	"Delete", 0,		8, 8,	0,	XK_Delete},
 {140,	"End",	0,		8, 8,	0,	XK_End},
 {142, 	"Next",	0,		8, 8,	0,	XK_Next},
 {0,	0,	0,		4, 8},
 {78, 	"7",	"Home",		8, 8,	0,	XK_KP_7},
 {79, 	"8",	"UpArrow",	8, 8,	0,	XK_KP_8},
 {80, 	"9",	"PgUp",		8, 8,	0,	XK_KP_9},
 {85, 	"+",	0,		8, 16,	0,	XK_KP_Add}
};

static struct key SCO_ODT_110_row4 [] = {
 {65,   "Caps",	"Lock",		14, 8,	LockMask,	XK_Caps_Lock},
 {37,	"A",	0,		8, 8,	0,	XK_A},
 {38,	"S",	0,		8, 8,	0,	XK_S},
 {39,	"D",	0,		8, 8,	0,	XK_D},
 {40,	"F",	0,		8, 8,	0,	XK_F},
 {41,	"G",	0,		8, 8,	0,	XK_G},
 {42,	"H",	0,		8, 8,	0,	XK_H},
 {43,	"J",	0,		8, 8,	0,	XK_J},
 {44,	"K",	0,		8, 8,	0,	XK_K},
 {45,	"L",	0,		8, 8,	0,	XK_L},
 {46,	":",	";",		8, 8,	0,	XK_semicolon,	XK_colon},
 {47,	"\"",	"'",		8, 8,	0,	XK_apostrophe,	XK_quotedbl},
 {35,	"Return", 0,		18, 8,	0,	XK_Return},
 {0,	0,	0,		32, 8},
 {82,	"4",	"LeftArrow",	8, 8,	0,	XK_KP_4},
 {83,	"5",	0,		8, 8,	0,	XK_KP_5},
 {84,	"6",	"RightArrow",	8, 8,	0,	XK_KP_6}
};

static struct key SCO_ODT_110_row5 [] = {
 {49,	"Shift",0,		18, 8,	ShiftMask,	XK_Shift_L},
 {51,	"Z",	0,		8, 8,	0,		XK_Z},
 {52,	"X",	0,		8, 8,	0,		XK_X},
 {53,	"C",	0,		8, 8,	0,		XK_C},
 {54,	"V",	0,		8, 8,	0,		XK_V},
 {55,	"B",	0,		8, 8,	0,		XK_B},
 {56,	"N",	0,		8, 8,	0,		XK_N},
 {57,	"M",	0,		8, 8,	0,		XK_M},
 {58,	"<",	",",		8, 8,	0,		XK_comma, XK_less},
 {59,	">",	".",		8, 8,	0,		XK_period,XK_greater},
 {60,	"?",	"/",		8, 8,	0,		XK_slash, XK_question},
 {61,	"Shift",0,		22, 8,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,		12, 8},
 {145,	"UpArrow",0,		8, 8,	0,		XK_Up},
 {0,	0,	0,		12, 8},
 {86,   "1",	"End",		8, 8,	0,		XK_KP_1},
 {87,   "2",	"DownArrow",	8, 8,	0,		XK_KP_2},
 {88,   "3",	"PgDn",		8, 8,	0,		XK_KP_3},
 {148,	"Enter",0,		8, 16,	0,		XK_KP_Enter}
};

static struct key SCO_ODT_110_row6 [] = {
 {36,	"Ctrl",		0,	12, 8,	ControlMask,	XK_Control_L},
 {0,	0,		0,	8, 8},
 {63,	"Alt",		0,	12, 8,	Mod1Mask,	XK_Alt_L},
 {64,	" ",		0,	57, 8,	0,		XK_space},
 {136,	"Alt",		0,	12, 8,	Mod1Mask,	XK_Alt_R},
 {0,	0,		0,	7, 8},
 {135,	"Ctrl/Act",	0,	12, 8,	0,		XK_Execute},
 {0,	0,		0,	4, 8},
 {144,	"LeftArrow",	0,	8, 8,	0,		XK_Left},
 {146,	"DownArrow",	0,	8, 8,	0,		XK_Down},
 {143,	"RightArrow",	0,	8, 8,	0,		XK_Right},
 {0,	0,		0,	4, 8},
 {89,	"0",		"Ins",	16, 8,	0,		XK_KP_0},
 {90,	".",		"Del",	8, 8,	0,		XK_KP_Decimal}
};

static struct row SCO_ODT_110_rows [] = {
  { sizeof (SCO_ODT_110_row1) / sizeof (struct key), 8, SCO_ODT_110_row1 },
  { 0, 8, 0 },
  { sizeof (SCO_ODT_110_row2) / sizeof (struct key), 8, SCO_ODT_110_row2 },
  { sizeof (SCO_ODT_110_row3) / sizeof (struct key), 8, SCO_ODT_110_row3 },
  { sizeof (SCO_ODT_110_row4) / sizeof (struct key), 8, SCO_ODT_110_row4 },
  { sizeof (SCO_ODT_110_row5) / sizeof (struct key), 8, SCO_ODT_110_row5 },
  { sizeof (SCO_ODT_110_row6) / sizeof (struct key), 8, SCO_ODT_110_row6 },
};

static struct keyboard SCO_ODT_110 = {
  "SCO110", "Santa Cruz Operation 110",
  sizeof (SCO_ODT_110_rows) / sizeof (struct row),
  SCO_ODT_110_rows,
  6, 3, 3
};
