/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the NCD X terminal N102 Norwegian keyboard.
 * By Bj|rn Wennberg <bjornw@edb.tih.no>
 */

static struct key NCD_N102n_row0 [] = {
 {8, "Esc",	0,	7, 7,	0,	XK_Escape},
 {0,	0,	0,	7, 7},
 {66,	"F1",	0,	7, 7,	0,	XK_F1},
 {67,	"F2",	0,	7, 7,	0,	XK_F2},
 {68,	"F3",	0,	7, 7,	0,	XK_F3},
 {69,	"F4",	0,	7, 7,	0,	XK_F4},
 {0,	0,	0,	4, 7},
 {70,	"F5",	0,	7, 7,	0,	XK_F5},
 {71,	"F6",	0,	7, 7,	0,	XK_F6},
 {72,	"F7",	0,	7, 7,	0,	XK_F7},
 {73,	"F8",	0,	7, 7,	0,	XK_F8},
 {0,	0,	0,	4, 7},
 {74,	"F9",	0,	7, 7,	0,	XK_F9},
 {75, "F10",	0,	7, 7,	0,	XK_F10},
 {94, "F11",	0,	7, 7,	0,	XK_F11},
 {95, "F12",	0,	7, 7,	0,	XK_F12},
 {0,	0,	0,	4, 7},
 {110,"Print",	0,	7, 7,	0,	XK_Print},
 {77,"Scroll","lock",	7, 7,	0,	XK_Scroll_Lock},
 {109,"Setup", 0,	7, 7,	0,	0}
};

static struct key NCD_N102n_row2 [] = {
 {48,	"|",	0,	7, 7,	0,	0,		XK_bar},
 {9,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {10,	"\"",	"2",	7, 7,	0,	XK_2,		XK_quotedbl},
 {11,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {12,	0,	"4",	7, 7,	0,	XK_4,		0},
 {13,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {14,	"&",	"6",	7, 7,	0,	XK_6,		XK_ampersand},
 {15,	"/",	"7",	7, 7,	0,	XK_7,		XK_slash},
 {16,	"(",	"8",	7, 7,	0,	XK_8,		XK_parenleft},
 {17,	")",	"9",	7, 7,	0,	XK_9,		XK_parenright},
 {18,	"=",	"0",	7, 7,	0,	XK_0,		XK_equal},
 {19,	"?",	"+",	7, 7,	0,	XK_plus,	XK_question},
 {20,	"`",	"\\",	7, 7,	0,	XK_backslash,	XK_grave},
 {21,	"Backspace",0,	15, 7,	0,	XK_BackSpace},
 {0,	0,	0,	4, 7},
 {105,	"Insert",0,	7, 7,	0,	XK_Insert},
 {96,	"Home",	0,	7, 7,	0,	XK_Home},
 {98,	"Page",	"Up",	7, 7,	0,	XK_Prior},
 {0,	0,	0,	4, 7},
 {76,	"Num ",	"Lock",	7, 7,	Mod5Mask,	XK_Num_Lock},
 {111,	"/",	0,	7, 7,	0,		XK_KP_Divide},
 {62,	"*",	0,	7, 7,	0,		XK_KP_Multiply},
 {81,	"-",	0,	7, 7,	0,		XK_KP_Subtract}
};

static struct key NCD_N102n_row3 [] = {
 {22, "Tab",	0,	11, 7,	0,	XK_Tab},
 {23,	"Q",	0,	7, 7,	0,	XK_Q},
 {24,	"W",	0,	7, 7,	0,	XK_W},
 {25,	"E",	0,	7, 7,	0,	XK_E},
 {26,	"R",	0,	7, 7,	0,	XK_R},
 {27,	"T",	0,	7, 7,	0,	XK_T},
 {28,	"Y",	0,	7, 7,	0,	XK_Y},
 {29,	"U",	0,	7, 7,	0,	XK_U},
 {30,	"I",	0,	7, 7,	0,	XK_I},
 {31,	"O",	0,	7, 7,	0,	XK_O},
 {32,	"P",	0,	7, 7,	0,	XK_P},
 {33, "\305",	0,	7, 7,	0,	XK_Adiaeresis},
 {34,	"^",	"\250",	7, 7,	0,	XK_diaeresis,	XK_asciicircum},
 {0,	0,	0,	2, 7},
 {35,"Return",0,	9, 14,	0,	XK_Return},
 {0,	0,	0,	4, 7},
 {106,"Delete",0,	7, 7,	0,	XK_Delete},
 {102,"End",	0,	7, 7,	0,	XK_End},
 {104,"Page",	"Down",	7, 7,	0,	XK_Next},
 {0,	0,	0,	4, 7},
 {78,	"7",	0,	7, 7,	0,	XK_KP_7},
 {79,	"8",	0,	7, 7,	0,	XK_KP_8},
 {80,	"9",	0,	7, 7,	0,	XK_KP_9},
 {85,	"+",	0,	7, 14,	0,	XK_KP_Add}
};

static struct key NCD_N102n_row4 [] = {
 {65,"Caps", "lock",	13, 7,	LockMask,	XK_Caps_Lock},
 {37,	"A",	0,	7, 7,	0,		XK_A},
 {38,	"S",	0,	7, 7,	0,		XK_S},
 {39,	"D",	0,	7, 7,	0,		XK_D},
 {40,	"F",	0,	7, 7,	0,		XK_F},
 {41,	"G",	0,	7, 7,	0,		XK_G},
 {42,	"H",	0,	7, 7,	0,		XK_H},
 {43,	"J",	0,	7, 7,	0,		XK_J},
 {44,	"K",	0,	7, 7,	0,		XK_K},
 {45,	"L",	0,	7, 7,	0,		XK_L},
 {46, "\245",	0,	7, 7,	0,		XK_Odiaeresis},
 {47, "\306",	0,	7, 7,	0,		XK_Adiaeresis},
 {50,	"*",	"'",	7, 7,	0,		XK_asterisk,	XK_apostrophe},
 {0,	0,	0,	38, 7},
 {82,	"4",	0,	7, 7,	0,		XK_KP_4},
 {83,	"5",	0,	7, 7,	0,		XK_KP_5},
 {84,	"6",	0,	7, 7,	0,		XK_KP_6}
};

static struct key NCD_N102n_row5 [] = {
 {49,"Shift",	0,	9, 7,	ShiftMask,	XK_Shift_L},
 {93,	">",	"<",	7, 7,	0,		XK_less,	XK_greater},
 {51,	"Z",	0,	7, 7,	0,		XK_Z},
 {52,	"X",	0,	7, 7,	0,		XK_X},
 {53,	"C",	0,	7, 7,	0,		XK_C},
 {54,	"V",	0,	7, 7,	0,		XK_V},
 {55,	"B",	0,	7, 7,	0,		XK_B},
 {56,	"N",	0,	7, 7,	0,		XK_N},
 {57,	"M",	0,	7, 7,	0,		XK_M},
 {58,	";",	",",	7, 7,	0,		XK_comma,	XK_semicolon},
 {59,	":",	".",	7, 7,	0,		XK_period,	XK_colon},
 {60,	"_",	"-",	7, 7,	0,		XK_underscore,	XK_hyphen},
 {61,	"Shift",	0,	20, 7,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,	11, 7},
 {97,	"UpArrow",0,	7, 7,	0,		XK_Up},
 {0,	0,	0,	11, 7},
 {86,	"1",	0,	7, 7,	0,		XK_KP_1},
 {87,	"2",	0,	7, 7,	0,		XK_KP_2},
 {88,	"3",	0,	7, 7,	0,		XK_KP_3},
 {107,"Enter",	0,	7, 14,	0,		XK_KP_Enter}
};

static struct key NCD_N102n_row6 [] = {
 {36,"Ctrl",	0,	11, 7,	ControlMask,	XK_Control_L},
 {0,	0,	0,	7, 7},
 {63,"Alt",	0,	11, 7,	Mod1Mask,	XK_Alt_L},
 {64,	" ",	0,	48, 7,	0,		XK_space},
 {112,"Alt",	0,	11, 7,	Mod1Mask,	XK_Alt_R},
 {0,	0,	0,	7, 7},
 {108,"Ctrl",	0,	11, 7,	ControlMask,	XK_Control_R},
 {0,	0,	0,	4, 7},
 {99,"LeftArrow",0,	7, 7,	0,		XK_Left},
 {103,"DownArrow",0,	7, 7,	0,		XK_Down},
 {101,"RightArrow",0,	7, 7,	0,		XK_Right},
 {0,	0,	0,	4, 7},
 {89,	"0",	0,	14, 7,	0,		XK_KP_0},
 {90,	".",	0,	7, 7,	0,		XK_KP_Decimal}
};

static struct row NCD_N102n_rows [] = {
  { sizeof (NCD_N102n_row0) / sizeof (struct key), 7, NCD_N102n_row0 },
  { 0,	5, 0 },
  { sizeof (NCD_N102n_row2) / sizeof (struct key), 7, NCD_N102n_row2 },
  { sizeof (NCD_N102n_row3) / sizeof (struct key), 7, NCD_N102n_row3 },
  { sizeof (NCD_N102n_row4) / sizeof (struct key), 7, NCD_N102n_row4 },
  { sizeof (NCD_N102n_row5) / sizeof (struct key), 7, NCD_N102n_row5 },
  { sizeof (NCD_N102n_row6) / sizeof (struct key), 7, NCD_N102n_row6 }
};

static struct keyboard NCD_N102n = {
  "N102N", "Network Computing Devices N102 (Norwegian layout)",
  sizeof (NCD_N102n_rows) / sizeof (struct row),
  NCD_N102n_rows,
  6, 3, 3
};
