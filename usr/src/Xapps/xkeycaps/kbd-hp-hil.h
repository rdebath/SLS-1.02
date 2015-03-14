/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the HP 300, 400, and 700 series keyboards.
 * By Dave Brooks <dbrooks@inel.gov>
 *
 * (HIL is apparently the name of the frob that HP hangs their
 * keyboards, mice, etc off of.)
 */

/* there are a number of keysyms defined for an HP that you won't
   (necessarily) find on other machines...  These are they.
 */
#ifdef hpux
# include <X11/HPkeysym.h>
#else
# define XK_Reset	0x1000FF6C
# define XK_System	0x1000FF6D
# define XK_User	0x1000FF6E
# define XK_ClearLine	0x1000FF6F
# define XK_InsertLine	0x1000FF70
# define XK_DeleteLine	0x1000FF71
# define XK_InsertChar	0x1000FF72
# define XK_DeleteChar	0x1000FF73
# define XK_BackTab	0x1000FF74

#endif

static struct key HP_HIL_row0 [] = {
 {15,	"Reset","Break",	8, 7,	0,	XK_Break,	XK_Reset},
 {86,	"Stop",	0,		8, 7,	0,	XK_Cancel},
 {0,	0,	0,		2, 7},
 {84,	"F1",	0,		9, 7,	0,	XK_F1},
 {83,	"F2",	0,		9, 7,	0,	XK_F2},
 {82,	"F3",	0,		9, 7,	0,	XK_F3},
 {81,	"F4",	0,		9, 7,	0,	XK_F4},
 {80,	"Menu",	0,		7, 7,	0,	XK_Menu},
 {88,	"User",	"System",	7, 7,	0,	XK_System,	XK_User},
 {89,	"F5",	0,		9, 7,	0,	XK_F5},
 {90,	"F6",	0,		9, 7,	0,	XK_F6},
 {91,	"F7",	0,		9, 7,	0,	XK_F7},
 {92,	"F8",	0,		9, 7,	0,	XK_F8},
 {0,	0,	0,		2, 7},
 {94,	"Clear","line",		8, 7,	0,	XK_ClearLine},
 {95,	"Clear","display",	7, 7,	0,	XK_Clear},
 {0,	0,	0,		7, 7},
 {45,	"F9",	0,		7, 7,	0,	XK_F9,	XK_KP_F1},
 {41,	"F10",	0,		7, 7,	0,	XK_F10,	XK_KP_F2},
 {43,	"F11",	0,		7, 7,	0,	XK_F11,	XK_KP_F3},
 {47,	"F12",	0,		7, 7,	0,	XK_F12,	XK_KP_F4},
};

static struct key HP_HIL_row2 [] = {
 {71,	"~",	"`",		9, 7,	0,	XK_grave,	XK_asciitilde},
 {70,	"!",	"1",		7, 7,	0,	XK_1,	XK_exclam},
 {69,	"@",	"2",		7, 7,	0,	XK_2,	XK_at},
 {68,	"#",	"3",		7, 7,	0,	XK_3,	XK_numbersign},
 {67,	"$",	"4",		7, 7,	0,	XK_4,	XK_dollar},
 {66,	"%",	"5",		7, 7,	0,	XK_5,	XK_percent},
 {65,	"^",	"6",		7, 7,	0,	XK_6,	XK_asciicircum},
 {64,	"&",	"7",		7, 7,	0,	XK_7,	XK_ampersand},
 {96,	"*",	"8",		7, 7,	0,	XK_8,	XK_asterisk},
 {97,	"(",	"9",		7, 7,	0,	XK_9,	XK_parenleft},
 {98,	")",	"0",		7, 7,	0,	XK_0,	XK_parenright},
 {99,	"_",	"-",		7, 7,	0,	XK_minus,	XK_underscore},
 {100,	"+",	"=",		7, 7,	0,	XK_equal,	XK_plus},
 {101,"Backspace",0,		12, 7,	0,	XK_BackSpace},
 {0,	0,	0,		2, 7},
 {102,	"Insert","line",	7, 7,	0,	XK_InsertLine},
 {103,	"Delete","line",	7, 7,	0,	XK_DeleteLine},
 {0,	0,	0,		7, 7},
 {29,	"*",	0,		7, 7,	0,	XK_KP_Multiply},
 {25,	"/",	0,		7, 7,	0,	XK_KP_Divide},
 {27,	"+",	0,		7, 7,	0,	XK_KP_Add},
 {31,	"-",	0,		7, 7,	0,	XK_KP_Subtract}
};

static struct key HP_HIL_row3 [] = {
 {63,	"Tab",	0,		14, 7,	0,	XK_Tab,		XK_BackTab},
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
 {109,	"|",	"\\",		7, 7,	0,	XK_backslash,	XK_bar},
 {0,	0,	0,		2, 7},
 {110,	"Insert","char",	7, 7,	0,	XK_InsertChar},
 {111,	"Delete","char",	7, 7,	0,	XK_DeleteChar},
 {0,	0,	0,		7, 7},
 {21,	"7",	0,		7, 7,	0,	XK_KP_7},
 {17,	"8",	0,		7, 7,	0,	XK_KP_8},
 {19,	"9",	0,		7, 7,	0,	XK_KP_9},
 {23,	"Enter",0,		7, 7,	0,	XK_KP_Enter}
};

static struct key HP_HIL_row4 [] = {
 {55,	"Caps",	0,		7, 7,	LockMask,		XK_Caps_Lock},
 {14,	"Ctrl",	0,		7, 7,	ControlMask,		XK_Control_L},
 {53,	"A",	0,		7, 7,	0,	XK_A},
 {52,	"S",	0,		7, 7,	0,	XK_S},
 {51,	"D",	0,		7, 7,	0,	XK_D},
 {50,	"F",	0,		7, 7,	0,	XK_F},
 {49,	"G",	0,		7, 7,	0,	XK_G},
 {48,	"H",	0,		7, 7,	0,	XK_H},
 {112,	"J",	0,		7, 7,	0,	XK_J},
 {113,	"K",	0,		7, 7,	0,	XK_K},
 {114,	"L",	0,		7, 7,	0,	XK_L},
 {115,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {116,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {117,	"Return",0,		14, 7,	0,	XK_Return},
 {0,	0,	0,		2, 7},
 {118,	"Home",	0,		7, 7,	0,	XK_Home},
 {119,	"Prev",	0,		7, 7,	0,	XK_Prior},
 {0,	0,	0,		7, 7},
 {16,	"4",	0,		7, 7,	0,	XK_KP_4},
 {18,	"5",	0,		7, 7,	0,	XK_KP_5},
 {20,	"6",	0,		7, 7,	0,	XK_KP_6},
 {22,	",",	0,		7, 7,	0,	XK_KP_Separator}
};

static struct key HP_HIL_row5 [] = {
 {39,	"Del",	"Esc",		7, 7,		0,	XK_Escape, XK_Delete},
 {13,	"Shift",0,		11, 7,		ShiftMask,	XK_Shift_L},
 {36,	"Z",	0,		7, 7,		0,		XK_Z},
 {35,	"X",	0,		7, 7,		0,		XK_X},
 {34,	"C",	0,		7, 7,		0,		XK_C},
 {33,	"V",	0,		7, 7,		0,		XK_V},
 {32,	"B",	0,		7, 7,		0,		XK_B},
 {128,	"N",	0,		7, 7,	 	0,	XK_N},
 {120,	"M",	0,		7, 7,	 	0,	XK_M},
 {121,	"<",	",",		7, 7,		0,	XK_comma, XK_less},
 {122,	">",	".",		7, 7,		0,	XK_period,XK_greater},
 {123,	"?",	"/",		7, 7,		0,	XK_slash, XK_question},
 {12,	"Shift",0,		11, 7,		ShiftMask,	XK_Shift_R},
 {0,	0,	0,		1, 7},
 {125,	"Select", 0,		7, 7,		0,	XK_Select},
 {134,	"UpArrow", 0,		7, 7,		0,	XK_Up},
 {127,	"Next",	0,		7, 7,		0,	XK_Next},
 {0,	0,	0,		7, 7},
 {24,	"1",	0,		7, 7,		0,	XK_KP_1},
 {26,	"2",	0,		7, 7,		0,	XK_KP_2},
 {28,	"3",	0,		7, 7,		0,	XK_KP_3},
 {46,	"Tab",	0,		7, 14,		0,	XK_KP_Tab}
};
 
static struct key HP_HIL_row6 [] = {
 {87,	"Print",  "Enter",	7, 7,	0,		XK_Execute, XK_Print},
 {0,	0,	0,		11, 7},
 {11,	"Extend", "char",	7, 7,	Mod1Mask,	XK_Meta_L},
 {129,	" ",	0,		56, 7,	0,		XK_space},
 {10,	"Extend", "char",	7, 7,	Mod1Mask,	XK_Meta_R},
 {0,	0,	0,		12, 7},
 {132,	"LeftArrow",  0,	7, 7,	0,		XK_Left},
 {133,	"DownArrow",  0,	7, 7,	0,		XK_Down},
 {135,	"RightArrow", 0,	7, 7,	0,		XK_Right},
 {0,	0,	0,		7, 7},
 {30,	"0",	0,		14, 7,	0,		XK_KP_0},
 {44,	".",	0,		7, 7,	0,		XK_KP_Decimal}
};

static struct row HP_HIL_rows [] = {
	{ sizeof (HP_HIL_row0) / sizeof (struct key), 7, HP_HIL_row0 },
	{ 0, 3, 0 },
	{ sizeof (HP_HIL_row2) / sizeof (struct key), 7, HP_HIL_row2 },
	{ sizeof (HP_HIL_row3) / sizeof (struct key), 7, HP_HIL_row3 },
	{ sizeof (HP_HIL_row4) / sizeof (struct key), 7, HP_HIL_row4 },
	{ sizeof (HP_HIL_row5) / sizeof (struct key), 7, HP_HIL_row5 },
	{ sizeof (HP_HIL_row6) / sizeof (struct key), 7, HP_HIL_row6 }
};

static struct keyboard HP_HIL = {
  "HPHIL", "Hewlett-Packard 300/400/700 series",
  sizeof (HP_HIL_rows) / sizeof (struct row),
  HP_HIL_rows,
  6, 3, 3
};
