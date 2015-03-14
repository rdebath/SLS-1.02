/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the DELL PC keyboard.
 * By Todd Nix <todd@dellunix.dell.com>
 */

static struct key DELL_PC_row0 [] = {
 {9,	"Esc",	0,	8, 7,	0,	XK_Escape,	XK_Delete},
 {0,	0,	0,	7, 7},
 {67,	"F1",	0,	7, 7,	0,	XK_F1},
 {68,	"F2",	0,	7, 7,	0,	XK_F2},
 {69,	"F3",	0,	7, 7,	0,	XK_F3},
 {70,	"F4",	0,	7, 7,	0,	XK_F4},
 {0,	0,	0,	3, 7},
 {71,	"F5",	0,	7, 7,	0,	XK_F5},
 {72,	"F6",	0,	7, 7,	0,	XK_F6},
 {73,	"F7",	0,	7, 7,	0,	XK_F7},
 {74,	"F8",	0,	7, 7,	0,	XK_F8},
 {0,	0,	0,	3, 7},
 {75,	"F9",	0,	7, 7,	0,	XK_F9,		XK_KP_F1},
 {76,	"F10",	0,	7, 7,	0,	XK_F10,		XK_KP_F2},
 {95,	"F11",	0,	7, 7,	0,	XK_F11,		XK_KP_F3},
 {96,	"F12",	0,	7, 7,	0,	XK_F12,		XK_KP_F4},
 {0,	0,	0,	3, 7},
 {111,	"Print","Screen",7, 7,	0,	XK_Print},
 {78,	"Scroll","Lock",7, 7,	0,	XK_Scroll_Lock},
 {110,	"Pause", 0,	7, 7,	0,	XK_Pause,	XK_Break},
};

static struct key DELL_PC_row2 [] = {
 {49,	"~",	"`",	9, 7,	0,	XK_grave,	XK_asciitilde},
 {10,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {11,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {12,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {13,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {14,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {15,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {16,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {17,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {18,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {19,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {20,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {21,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {22,"Backspace",0,	12, 7,	0,	XK_BackSpace},
 {0,	0,	0,	3, 7},
 {106,"Insert",	0,	7, 7,	0,	XK_Insert},
 {97,"Home",	0,	7, 7,	0,	XK_Home},
 {99,	"Page",	"Up",	7, 7,	0,	XK_Prior},
 {0,	0,	0,	3, 7},
 {77,	"Num",	"Lock",	7, 7,	 Mod2Mask,	XK_Num_Lock,	XK_Num_Lock},
 {112,	"/",	0,	7, 7,	0,	XK_KP_Divide},
 {63,	"*",	0,	7, 7,	0,	XK_KP_Multiply},
 {82,	"-",	0,	7, 7,	0,	XK_KP_Subtract}
};

static struct key DELL_PC_row3 [] = {
 {23,"Tab",	0,		12, 7,	0,	XK_Tab,	XK_Tab},
 {24,	"Q",	0,		7, 7,	0,	XK_Q},
 {25,	"W",	0,		7, 7,	0,	XK_W},
 {26,	"E",	0,		7, 7,	0,	XK_E},
 {27,	"R",	0,		7, 7,	0,	XK_R},
 {28,	"T",	0,		7, 7,	0,	XK_T},
 {29,	"Y",	0,		7, 7,	0,	XK_Y},
 {30,	"U",	0,		7, 7,	0,	XK_U},
 {31,	"I",	0,		7, 7,	0,	XK_I},
 {32,	"O",	0,		7, 7,	0,	XK_O},
 {33,	"P",	0,		7, 7,	0,	XK_P},
 {34,	"{",	"[",		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {35,	"}",	"]",		7, 7,	0,	XK_bracketright,XK_braceright},
 {51,	"|",	"\\",		9, 7,	0,	XK_backslash,	XK_bar},
 {0,	0,	0,		3, 7},
 {107,"Delete",	0,		7, 7,	0,	XK_Delete},
 {103,"End",	0,		7, 7,	0,	XK_End},
 {105, "Page",	"Down",		7, 7,	0,	XK_Next},
 {0,	0,	0,		3, 7},
 {79,	"7",	"Home",		7, 7,	0,	XK_KP_7,	XK_Home},
 {80,"8",	"UpArrow",	7, 7,	0,	XK_KP_8,	XK_Up},
 {81,	"9",	"Pg Up",	7, 7,	0,	XK_KP_9,	XK_Prior},
 {86,	"+",	0,		7, 14,	0,	XK_KP_Add},
};

static struct key DELL_PC_row4 [] = {
 {66,"Caps Lock",0,		14, 7,		LockMask,	XK_Caps_Lock},
 {38,	"A",	0,		7, 7,		0,		XK_A},
 {39,	"S",	0,		7, 7,		0,		XK_S},
 {40,	"D",	0,		7, 7,		0,		XK_D},
 {41,	"F",	0,		7, 7,		0,		XK_F},
 {42,	"G",	0,		7, 7,		0,		XK_G},
 {43,	"H",	0,		7, 7,		0,		XK_H},
 {44,	"J",	0,		7, 7,	 	0,	XK_J},
 {45,	"K",	0,		7, 7,	 	0,	XK_K},
 {46,	"L",	0,		7, 7,	 	0,	XK_L},
 {47,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {48,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {36,"Enter",	0,		14, 7,		 0,	XK_Return},
 {0,	0,	0,		3, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		3, 7},
 {83,"4",	"LeftArrow",	7, 7,		0,	XK_KP_4, XK_Left},
 {84,	"5",	0,		7, 7,		0,	XK_KP_5},
 {85,"6",	"RightArrow",	7, 7,		0,	XK_KP_6, XK_Right},
};

static struct key DELL_PC_row5 [] = {
 {50,"Shift",	0,		17, 7,	ShiftMask,	XK_Shift_L},
 {52,	"Z",	0,		7, 7,	0,		XK_Z},
 {53,	"X",	0,		7, 7,	0,		XK_X},
 {54,	"C",	0,		7, 7,	0,		XK_C},
 {55,	"V",	0,		7, 7,	0,		XK_V},
 {56,	"B",	0,		7, 7,	0,		XK_B},
 {57,	"N",	0,		7, 7,	0,		XK_N},
 {58,	"M",	0,		7, 7,	0,		XK_M},
 {59,	"<",	",",		7, 7,	0,		XK_comma, XK_less},
 {60,	">",	".",		7, 7,	0,		XK_period,XK_greater},
 {61,	"?",	"/",		7, 7,	0,		XK_slash, XK_question},
 {62,	"Shift",0,		18, 7,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,		3, 7},
 {0,	0,	0,		7, 7},
 {98,	"UpArrow",0,		7, 7,	0,		XK_Up},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		3, 7},
 {87,	"1",	"End",		7, 7,	0,		XK_KP_1, XK_End},
 {88,	"2",	"DownArrow",	7, 7,	0,		XK_KP_2, XK_Down},
 {89,	"3",	"Pg Dn",	7, 7,	0,		XK_KP_3, XK_Next},
 {108,	"Enter",0,		7, 14,	0,		XK_KP_Enter}
};
 
static struct key DELL_PC_row6 [] = {
 {37,	"Ctrl",	0,	10, 7,	ControlMask,	XK_Control_L},
 {0,	0,	0,	7, 7},
 {64, 	"Alt",	0,	9, 7,	Mod1Mask,	XK_Alt_L},
 {65,	" ",	0,	53, 7,	0,		XK_space},
 {113,	"Alt",	0,	9, 7,	Mod1Mask,	XK_Alt_R},
 {0,	0,	0,	7, 7},
 {109,	"Ctrl",	0,	10, 7,	ControlMask,	XK_Control_R},
 {0,	0,	0,	3, 7},
 {100,	"LeftArrow",0,	7, 7,	0,		XK_Left},
 {104,	"DownArrow",0,	7, 7,	0,		XK_Down},
 {102,	"RightArrow",0,	7, 7,	0,		XK_Right},
 {0,	0,	0,	3, 7},
 {90,	"0",	"Ins",	14, 7,	0,		XK_KP_0,	XK_Insert},
 {91,	".",	"Del",	7, 7,	0,		XK_KP_Decimal,	XK_Delete}
};

static struct row DELL_PC_rows [] = {
  { sizeof (DELL_PC_row0) / sizeof (struct key), 7, DELL_PC_row0 },
  { 0, 3, 0 },
  { sizeof (DELL_PC_row2) / sizeof (struct key), 7, DELL_PC_row2 },
  { sizeof (DELL_PC_row3) / sizeof (struct key), 7, DELL_PC_row3 },
  { sizeof (DELL_PC_row4) / sizeof (struct key), 7, DELL_PC_row4 },
  { sizeof (DELL_PC_row5) / sizeof (struct key), 7, DELL_PC_row5 },
  { sizeof (DELL_PC_row6) / sizeof (struct key), 7, DELL_PC_row6 }
};

static struct keyboard DELL_PC = {
  "DELL", "DELL PC",
  sizeof (DELL_PC_rows) / sizeof (struct row),
  DELL_PC_rows,
  6, 3, 3
};
