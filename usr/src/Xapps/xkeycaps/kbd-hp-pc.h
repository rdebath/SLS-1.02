/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the HP PC keyboard.
 * By Markus Stumpf <stumpf@informatik.tu-muenchen.de>
 */

static struct key HP_PC_row0 [] = {
 {39,	"Esc",	0,	8, 7,	0,	XK_Escape,	XK_Delete},
 {0,	0,	0,	7, 7},
 {84,	"F1",	0,	7, 7,	0,	XK_F1},
 {83,	"F2",	0,	7, 7,	0,	XK_F2},
 {82,	"F3",	0,	7, 7,	0,	XK_F3},
 {81,	"F4",	0,	7, 7,	0,	XK_F4},
 {0,	0,	0,	3, 7},
 {89,	"F5",	0,	7, 7,	0,	XK_F5},
 {90,	"F6",	0,	7, 7,	0,	XK_F6},
 {91,	"F7",	0,	7, 7,	0,	XK_F7},
 {92,	"F8",	0,	7, 7,	0,	XK_F8},
 {0,	0,	0,	3, 7},
 {45,	"F9",	0,	7, 7,	0,	XK_F9,		XK_KP_F1},
 {41,	"F10",	0,	7, 7,	0,	XK_F10,		XK_KP_F2},
 {43,	"F11",	0,	7, 7,	0,	XK_F11,		XK_KP_F3},
 {47,	"F12",	0,	7, 7,	0,	XK_F12,		XK_KP_F4},
 {0,	0,	0,	3, 7},
 {80,	"Print","Screen",7, 7,	0,	XK_Print},
 {86,	"Scroll","Lock",7, 7,	0,	XK_Scroll_Lock},
 {15,	"Pause", 0,	7, 7,	0,	XK_Pause,	XK_Break},
};

static struct key HP_PC_row2 [] = {
 {71,	"~",	"`",	9, 7,	0,	XK_grave,	XK_asciitilde},
 {70,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {69,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {68,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {67,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {66,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {65,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {64,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {96,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {97,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {98,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {99,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {100,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {101,"Backspace",0,	12, 7,	0,	XK_BackSpace},
 {0,	0,	0,	3, 7},
 {110,"Insert",	0,	7, 7,	0,	XK_Insert},
 {118,"Home",	0,	7, 7,	0,	XK_Home},
 {119,	"Page",	"Up",	7, 7,	0,	XK_Prior},
 {0,	0,	0,	3, 7},
 {88,	"Num",	"Lock",	7, 7,	 Mod5Mask,	XK_Num_Lock,	XK_Num_Lock},
 {25,	"/",	0,	7, 7,	0,	XK_KP_Divide},
 {29,	"*",	0,	7, 7,	0,	XK_KP_Multiply},
 {31,	"-",	0,	7, 7,	0,	XK_KP_Subtract}
};

static struct key HP_PC_row3 [] = {
 {63,"Tab",	0,		12, 7,	0,	XK_Tab,	XK_BackTab},
 {62,	"Q",	0,		7, 7,	0,	XK_Q},
 {61,	"W",	0,		7, 7,	0,	XK_W},
 {60,	"E",	0,		7, 7,	0,	XK_E},
 {59,	"R",	0,		7, 7,	0,	XK_R},
 {58,	"T",	0,		7, 7,	0,	XK_T},
 {57,	"Y",	0,		7, 7,	0,	XK_Y},
 {56,	"U",	0,		7, 7,	0,	XK_U},
 {104,	"I",	0,		7, 7,	0,	XK_I},
 {105,	"O",	0,		7, 7,	0,	XK_O},
 {106,	"P",	0,		7, 7,	0,	XK_P},
 {107,	"{",	"[",		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {108,	"}",	"]",		7, 7,	0,	XK_bracketright,XK_braceright},
 {109,	"|",	"\\",		9, 7,	0,	XK_backslash,	XK_bar},
 {0,	0,	0,		3, 7},
 {111,"Delete",	0,		7, 7,	0,	XK_Delete},
 {125,"End",	0,		7, 7,	0,	XK_End},
 {127, "Page",	"Down",		7, 7,	0,	XK_Next},
 {0,	0,	0,		3, 7},
 {21,	"7",	"Home",		7, 7,	0,	XK_KP_7,	XK_Home},
 {17,"8",	"UpArrow",	7, 7,	0,	XK_KP_8,	XK_Up},
 {19,	"9",	"Pg Up",	7, 7,	0,	XK_KP_9,	XK_Prior},
 {27,	"+",	0,		7, 14,	0,	XK_KP_Add},
};

static struct key HP_PC_row4 [] = {
 {55,"Caps Lock",0,		14, 7,		LockMask,	XK_Caps_Lock},
 {53,	"A",	0,		7, 7,		0,		XK_A},
 {52,	"S",	0,		7, 7,		0,		XK_S},
 {51,	"D",	0,		7, 7,		0,		XK_D},
 {50,	"F",	0,		7, 7,		0,		XK_F},
 {49,	"G",	0,		7, 7,		0,		XK_G},
 {48,	"H",	0,		7, 7,		0,		XK_H},
 {112,	"J",	0,		7, 7,	 	0,	XK_J},
 {113,	"K",	0,		7, 7,	 	0,	XK_K},
 {114,	"L",	0,		7, 7,	 	0,	XK_L},
 {115,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {116,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {117,"Enter",	0,		14, 7,		 0,	XK_Return},
 {0,	0,	0,		3, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		3, 7},
 {16,"4",	"LeftArrow",	7, 7,		0,	XK_KP_4, XK_Left},
 {18,	"5",	0,		7, 7,		0,	XK_KP_5},
 {20,"6",	"RightArrow",	7, 7,		0,	XK_KP_6, XK_Right},
};

static struct key HP_PC_row5 [] = {
 {13,"Shift",	0,		17, 7,	ShiftMask,	XK_Shift_L},
 {36,	"Z",	0,		7, 7,	0,		XK_Z},
 {35,	"X",	0,		7, 7,	0,		XK_X},
 {34,	"C",	0,		7, 7,	0,		XK_C},
 {33,	"V",	0,		7, 7,	0,		XK_V},
 {32,	"B",	0,		7, 7,	0,		XK_B},
 {128,	"N",	0,		7, 7,	0,		XK_N},
 {120,	"M",	0,		7, 7,	0,		XK_M},
 {121,	"<",	",",		7, 7,	0,		XK_comma, XK_less},
 {122,	">",	".",		7, 7,	0,		XK_period,XK_greater},
 {123,	"?",	"/",		7, 7,	0,		XK_slash, XK_question},
 {12,	"Shift",0,		18, 7,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,		3, 7},
 {0,	0,	0,		7, 7},
 {134,	"UpArrow",0,		7, 7,	0,		XK_Up},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		3, 7},
 {24,	"1",	"End",		7, 7,	0,		XK_KP_1, XK_End},
 {26,	"2",	"DownArrow",	7, 7,	0,		XK_KP_2, XK_Down},
 {28,	"3",	"Pg Dn",	7, 7,	0,		XK_KP_3, XK_Next},
 {23,	"Enter",0,		7, 14,	0,		XK_KP_Enter}
};
 
static struct key HP_PC_row6 [] = {
 {14,	"Ctrl",	0,	10, 7,	ControlMask,	XK_Control_L},
 {0,	0,	0,	7, 7},
 {11, 	"Alt",	0,	9, 7,	Mod1Mask,	XK_Alt_L},
 {129,	" ",	0,	53, 7,	0,		XK_space},
 {10,	"Alt",	0,	9, 7,	Mod1Mask,	XK_Alt_R},
 {0,	0,	0,	7, 7},
 {8,	"Ctrl",	0,	10, 7,	ControlMask,	XK_Control_R},
 {0,	0,	0,	3, 7},
 {132,	"LeftArrow",0,	7, 7,	0,		XK_Left},
 {133,	"DownArrow",0,	7, 7,	0,		XK_Down},
 {135,	"RightArrow",0,	7, 7,	0,		XK_Right},
 {0,	0,	0,	3, 7},
 {30,	"0",	"Ins",	14, 7,	0,		XK_KP_0,	XK_Insert},
 {44,	".",	"Del",	7, 7,	0,		XK_KP_Decimal,	XK_Delete}
};

static struct row HP_PC_rows [] = {
  { sizeof (HP_PC_row0) / sizeof (struct key), 7, HP_PC_row0 },
  { 0, 3, 0 },
  { sizeof (HP_PC_row2) / sizeof (struct key), 7, HP_PC_row2 },
  { sizeof (HP_PC_row3) / sizeof (struct key), 7, HP_PC_row3 },
  { sizeof (HP_PC_row4) / sizeof (struct key), 7, HP_PC_row4 },
  { sizeof (HP_PC_row5) / sizeof (struct key), 7, HP_PC_row5 },
  { sizeof (HP_PC_row6) / sizeof (struct key), 7, HP_PC_row6 }
};

static struct keyboard HP_PC = {
  "HPPC", "Hewlett-Packard PC",
  sizeof (HP_PC_rows) / sizeof (struct row),
  HP_PC_rows,
  6, 3, 3
};
