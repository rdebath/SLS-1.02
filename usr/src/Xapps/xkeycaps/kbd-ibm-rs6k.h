/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the IBM RS/6000 keyboard.
 * By Tom McConnell <tmcconne@sedona.intel.com>
 */

static struct key IBM_rs6k_row0 [] = {
 {118,"Esc",	0,		8, 8,	0,	XK_Escape},
 {0,	0,	0,		8, 8},
 {120,	"F1",	0,		8, 8,	0,	XK_F1},
 {121,	"F2",	0,		8, 8,	0,	XK_F2},
 {122,	"F3",	0,		8, 8,	0,	XK_F3},
 {123,	"F4",	0,		8, 8,	0,	XK_F4},
 {0,	0,	0,		4, 8},
 {124,	"F5",	0,		8, 8,	0,	XK_F5},
 {125,	"F6",	0,		8, 8,	0,	XK_F6},
 {126,	"F7",	0,		8, 8,	0,	XK_F7},
 {127,	"F8",	0,		8, 8,	0,	XK_F8},
 {0,	0,	0,		4, 8},
 {128,	"F9",	0,		8, 8,	0,	XK_F9},
 {129,	"F10",	0,		8, 8,	0,	XK_F10},
 {130,	"F11",	0,		8, 8,	0,	XK_F11},
 {131,	"F12",	0,		8, 8,	0,	XK_F12},
 {0,	0,	0,		4, 8},
 {132,	"Print", "SysRq",	8, 8,	0,	XK_Print},
 {133,	"Scroll","Lock",	8, 8,	0,	XK_Cancel},
 {134,	"Pause", "Break",	8, 8,	0,	XK_Pause},
};

static struct key IBM_rs6k_row2 [] = {
 {9,	"~",	"`",		8, 8,	0,	XK_grave,	XK_asciitilde},
 {10,	"!",	"1",		8, 8,	0,	XK_1,		XK_exclam},
 {11,	"@",	"2",		8, 8,	0,	XK_2,		XK_at},
 {12,	"#",	"3",		8, 8,	0,	XK_3,		XK_numbersign},
 {13,	"$",	"4",		8, 8,	0,	XK_4,		XK_dollar},
 {14,	"%",	"5",		8, 8,	0,	XK_5,		XK_percent},
 {15,	"^",	"6",		8, 8,	0,	XK_6,	       XK_asciicircum},
 {16,	"&",	"7",		8, 8,	0,	XK_7,		XK_ampersand},
 {17,	"*",	"8",		8, 8,	0,	XK_8,		XK_asterisk},
 {18,	"(",	"9",		8, 8,	0,	XK_9,		XK_parenleft},
 {19,	")",	"0",		8, 8,	0,	XK_0,		XK_parenright},
 {20,	"_",	"-",		8, 8,	0,	XK_minus,	XK_underscore},
 {21,	"+",	"=",		8, 8,	0,	XK_equal,	XK_plus},
 {23,	"Backspace", 0,		16, 8,	0,	XK_BackSpace},
 {0,	0,	0,		4, 8},
 {83,	"Insert", 0,		8, 8,	0,	XK_Insert},
 {88,	"Home",	0,		8, 8,	0,	XK_Home},
 {93,	"Page", "up",		8, 8,	0,	XK_Prior},
 {0,	0,	0,		4, 8},
 {98,	"Num",	"Lock",		8, 8,	Mod5Mask,	XK_Num_Lock},
 {103,	"/",	0,		8, 8,	0,	XK_KP_Divide},
 {108,	"*",	0,		8, 8,	0,	XK_KP_Multiply},
 {113,	"-",	0,		8, 8,	0,	XK_KP_Subtract}
};

static struct key IBM_rs6k_row3 [] = {
 {24,	"Tab",	0,		12, 8,	0,	XK_Tab},
 {25,	"Q",	0,		8, 8,	0,	XK_q,	XK_Q},
 {26,	"W",	0,		8, 8,	0,	XK_w,	XK_W},
 {27,	"E",	0,		8, 8,	0,	XK_e,	XK_E},
 {28,	"R",	0,		8, 8,	0,	XK_r,	XK_R},
 {29,	"T",	0,		8, 8,	0,	XK_t,	XK_T},
 {30,	"Y",	0,		8, 8,	0,	XK_y,	XK_Y},
 {31,	"U",	0,		8, 8,	0,	XK_u,	XK_U},
 {32,	"I",	0,		8, 8,	0,	XK_i,	XK_I},
 {33,	"O",	0,		8, 8,	0,	XK_o,	XK_O},
 {34,	"P",	0,		8, 8,	0,	XK_p,	XK_P},
 {35,	"{",	"[",		8, 8,	0,	XK_bracketleft,	XK_braceleft},
 {36,	"}",	"]",		8, 8,	0,	XK_bracketright,XK_braceright},
 {37,	"|",	"\\",		12, 8,	0,	XK_backslash,	XK_bar},
 {0,	0,	0,		4, 8},
 {84,	"Delete", 0,		8, 8,	0,	XK_Delete},
 {89,	"End",	0,		8, 8,	0,	XK_End},
 {94,	"Next",	0,		8, 8,	0,	XK_Next},
 {0,	0,	0,		4, 8},
 {99,	"7",	"Home",		8, 8,	0,	XK_KP_7},
 {104,	"8",	"UpArrow",	8, 8,	0,	XK_KP_8},
 {109,	"9",	"PgUp",		8, 8,	0,	XK_KP_9},
 {114,	"+",	0,		8, 16,	0,	XK_KP_Add}
};

static struct key IBM_rs6k_row4 [] = {
 {38,	"Caps",	"Lock",		14, 8,	LockMask,	XK_Caps_Lock},
 {39,	"A",	0,		8, 8,	0,	XK_a,	XK_A},
 {40,	"S",	0,		8, 8,	0,	XK_s,	XK_S},
 {41,	"D",	0,		8, 8,	0,	XK_d,	XK_D},
 {42,	"F",	0,		8, 8,	0,	XK_f,	XK_F},
 {43,	"G",	0,		8, 8,	0,	XK_g,	XK_G},
 {44,	"H",	0,		8, 8,	0,	XK_h,	XK_H},
 {45,	"J",	0,		8, 8,	0,	XK_j,	XK_J},
 {46,	"K",	0,		8, 8,	0,	XK_k,	XK_K},
 {47,	"L",	0,		8, 8,	0,	XK_l,	XK_L},
 {48,	":",	";",		8, 8,	0,	XK_semicolon,	XK_colon},
 {49,	"\"",	"'",		8, 8,	0,	XK_apostrophe,	XK_quotedbl},
 {51,"Return",	0,		18, 8,	0,	XK_Return,	XK_Return},
 {0,	0,	0,		32, 8},
 {100,	"4",	"LeftArrow",	8, 8,	0,	XK_KP_4},
 {105,	"5",	0,		8, 8,	0,	XK_KP_5},
 {110,	"6",	"RightArrow",	8, 8,	0,	XK_KP_6}
};

static struct key IBM_rs6k_row5 [] = {
 {52,	"Shift",0,		18, 8,	ShiftMask,	XK_Shift_L},
 {54,	"Z",	0,		8, 8,	0,		XK_z,		XK_Z},
 {55,	"X",	0,		8, 8,	0,		XK_x,		XK_X},
 {56,	"C",	0,		8, 8,	0,		XK_c,		XK_C},
 {57,	"V",	0,		8, 8,	0,		XK_v,		XK_V},
 {58,	"B",	0,		8, 8,	0,		XK_b,		XK_B},
 {59,	"N",	0,		8, 8,	0,		XK_n,		XK_N},
 {60,	"M",	0,		8, 8,	0,		XK_m,		XK_M},
 {61,	"<",	",",		8, 8,	0,		XK_comma, XK_less},
 {62,	">",	".",		8, 8,	0,		XK_period,XK_greater},
 {63,	"?",	"/",		8, 8,	0,		XK_slash, XK_question},
 {65,	"Shift",0,		22, 8,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,		12, 8},
 {91,	"UpArrow",0,		8, 8,	0,		XK_Up},
 {0,	0,	0,		12, 8},
 {101,	 "1",	"End",		8, 8,	0,		XK_KP_1},
 {106,	 "2",	"DownArrow",	8, 8,	0,		XK_KP_2},
 {111,	 "3",	"PgDn",		8, 8,	0,		XK_KP_3},
 {116,	"Enter",0,		8, 16,	0,		XK_KP_Enter}
}; 

static struct key IBM_rs6k_row6 [] = {
 {66,	"Ctrl",		0,	12, 8,	ControlMask,	XK_Control_L},
 {0,	0,		0,	8, 8},
 {68,	"Alt",		0,	12, 8,	Mod1Mask,	XK_Alt_L},
 {69,	" ",		0,	57, 8,	0,		XK_space},
 {70,	"Alt",		0,	12, 8,	Mod1Mask,	XK_Alt_R},
 {0,	0,		0,	7, 8},
 /* On the IBM Xstation 130, this key generates ModControl, but on the
    IBM RS/6000 it is used for window manager stuff instead. */
 {72,	"Ctrl/Act",	0,	12, 8,	0,		XK_Execute},
 {0,	0,		0,	4, 8},
 {87,	"LeftArrow",	0,	8, 8,	0,		XK_Left},
 {92,	"DownArrow",	0,	8, 8,	0,		XK_Down},
 {97,	"RightArrow",	0,	8, 8,	0,		XK_Right},
 {0,	0,		0,	4, 8},
 {107,	"0",		"Ins",	16, 8,	0,		XK_KP_0},
 {112,	".",		"Del",	8, 8,	0,		XK_KP_Decimal}
};

static struct row IBM_rs6k_rows [] = {
  { sizeof (IBM_rs6k_row0) / sizeof (struct key), 8, IBM_rs6k_row0 },
  { 0, 8, 0 },
  { sizeof (IBM_rs6k_row2) / sizeof (struct key), 8, IBM_rs6k_row2 },
  { sizeof (IBM_rs6k_row3) / sizeof (struct key), 8, IBM_rs6k_row3 },
  { sizeof (IBM_rs6k_row4) / sizeof (struct key), 8, IBM_rs6k_row4 },
  { sizeof (IBM_rs6k_row5) / sizeof (struct key), 8, IBM_rs6k_row5 },
  { sizeof (IBM_rs6k_row6) / sizeof (struct key), 8, IBM_rs6k_row6 },
};

static struct keyboard IBM_rs6k = {
  "RS6k", "Inferior But Marketable RS/6000",
  sizeof (IBM_rs6k_rows) / sizeof (struct row),
  IBM_rs6k_rows,
  6, 3, 3
};
