/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the Tektronix XP20 keyboard,
 * _TEK_KEYBOARD_TYPE = "ibm101" (101-key North American Keyboard)
 *
 * Submitted by Joe English <joe@trystero.art.com>
 *
 * The keycodes are in decimal rather than hex because that's
 * how they're listed in the user's manual.
 *
 * Tektronix apparently supplies about seven different keyboards
 * with the XP20 series; this kbd file describes the 
 * "101 Key North American" flavor. The _TEK_KEYBOARD_TYPE property 
 * on the root window should be "ibm101".
 *
 * The default keybindings changed from release 4 to release 5 of
 * the XP terminal software.  This file describes both sets of
 * bindings; see also kbd-tek-101-4.h
 *
 * The newer bindings should work with an older
 * server, but the reverse does not hold -- as of release 5,
 * the TekSetup function is invoked by any key bound to the (new)
 * TekXK_Setup keysym, whereas before it was invoked by a specific
 * keycode.
 *
 */

/* Taken from TEKkeysym.h, in XP software distribution */
#ifndef TekXK_Setup 	/* %%%  assume none defined */

#define TekXK_Remove	0x1000FF00
#define TekXK_Hold      0x1000FF10   /* Hold Screen */
#define TekXK_Copy      0x1000FF11   /* Screen/Dialog HardCopy  */
#define TekXK_Setup     0x1000FF12   /* Setup */
#define TekXK_SErase    0x1000FF13   /* Screen Erase    */
#define TekXK_GErase    0x1000FF14   /* Graphics Erase  */
#define TekXK_DErase    0x1000FF15   /* Dialog Erase    */
#define TekXK_Dialog    0x1000FF16   /* Dialog */
#define TekXK_Tek       0x1000FFFF   /* Tek Key */

#endif


static struct key TEK_101_row0 [] = {
 {16,	"Esc",	0,	7, 7,	0,	XK_Escape},
 {0,	0,	0,	6, 7},
 {15,	"F1",	0,	7, 7,	0,	XK_F1},
 {23,	"F2",	0,	7, 7,	0,	XK_F2},
 {31,	"F3",	0,	7, 7,	0,	XK_F3},
 {39,	"F4",	0,	7, 7,	0,	XK_F4},
 {0,	0,	0,	4, 7},
 {47,	"F5",	0,	7, 7,	0,	XK_F5},
 {55,	"F6",	0,	7, 7,	0,	XK_F6},
 {63,	"F7",	0,	7, 7,	0,	XK_F7},
 {71,	"F8",	0,	7, 7,	0,	XK_F8},
 {0,	0,	0,	4, 7},
 {79,	"F9",	0,	7, 7,	0,	XK_F9},
 {87,	"F10",	0,	7, 7,	0,	XK_F10},
 {94,	"F11",	0,	7, 7,	0,	XK_F11},
 {102,	"F12",	0,	7, 7,	0,	XK_F12},
 {0,	0,	0,	3, 7},
 {95,	"Line",  "Feed", 	7, 7,	0,	XK_Linefeed },
 {103,	"Break",	0,	7, 7,	0,	XK_Break},
#ifdef TEK_VENDOR_RELEASE_4
 {106,	"Setup", 	0,	7, 7,	0, 	XK_Hyper_R,XK_Pause}
#else
 {106,	"Setup", 	0,	7, 7,	0, 	TekXK_Setup,XK_Pause}
#endif
};

static struct key TEK_101_row1 [] = {
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
 {100,	"|",	"\\",	7, 7,	0,	XK_backslash,	XK_bar},
 {110, "LeftArrow",0,	7, 7,	0,	XK_BackSpace},
 {0,	0,	0,	3, 7},
 {111,	"Insert", 0,	7, 7,	0,	XK_Insert},
 {118,	"Home",	0,	7, 7,	0,	XK_Home},
 {119,	"PgUp", 0,	7, 7,	0,	XK_Prior},
 {0,	0,	0,	3, 7},
 {126,	"Num",	"Lock",	7, 7,	0,	XK_Num_Lock},
 {127,	"/",	0,	7, 7,	0,	XK_KP_Divide},
 {134,	"*",	0,	7, 7,	0,	XK_KP_Multiply},
 {140,	"-",	0,	7, 7,	0,	XK_KP_Subtract}
};

static struct key TEK_101_row2 [] = {
 {21,	"Tab",	0,		10, 7,	0,	XK_Tab},
 {29,	"Q",	0,		7, 7,	0,	XK_Q},
 {37,	"W",	0,		7, 7,	0,	XK_W},
 {44,	"E",	0,		7, 7,	0,	XK_E},
 {53,	"R",	0,		7, 7,	0,	XK_R},
 {52,	"T",	0,		7, 7,	0,	XK_T},
 {61,	"Y",	0,		7, 7,	0,	XK_Y},
 {68,	"U",	0,		7, 7,	0,	XK_U},
 {75,	"I",	0,		7, 7,	0,	XK_I},
 {76,	"O",	0,		7, 7,	0,	XK_O},
 {85,	"P",	0,		7, 7,	0,	XK_P},
 {92,	"{",	"[",		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {99,	"}",	"]",		7, 7,	0,	XK_bracketright,XK_braceright},
 {98,	"Enter", 0,		11, 14,	0,	XK_Return},
 {0,	0,	0,	3, 7},
/* {0,	0,	0,	14, 7},*/
 {108,	"Delete", 0,	7, 7,	0,	XK_Delete},
 {109,	"End",	0,	7, 7,	0,	XK_End},
 {117,	"PgDn", 0,	7, 7,	0,	XK_Next},
 {0,	0,	0,	3, 7},
#ifdef TEK_VENDOR_RELEASE_4
 {116,	"7",	"Home",	7, 7,	0,	XK_KP_7},
 {125,	"8", "UpArrow",	7, 7,	0, 	XK_KP_8},
 {133,	"9",	"PgUp",	7, 7,	0,	XK_KP_9},
#else
 {116,	"7",	"Home",	7, 7,	0,	XK_KP_7, XK_Home},
 {125,	"8", "UpArrow",	7, 7,	0, 	XK_KP_8, XK_Up},
 {133,	"9",	"PgUp",	7, 7,	0,	XK_KP_9, XK_Prior},
#endif
 {132,	"+",	0,	7, 14,	0,	XK_KP_Add}
};

static struct key TEK_101_row3 [] = {
 {28,	"Ctrl", 0,		12, 7,	ControlMask,	XK_Control_L},
 {36,	"A",	0,		7, 7,	0,	XK_A},
 {35,	"S",	0,		7, 7,	0,	XK_S},
 {43,	"D",	0,		7, 7,	0,	XK_D},
 {51,	"F",	0,		7, 7,	0,	XK_F},
 {60,	"G",	0,		7, 7,	0,	XK_G},
 {59,	"H",	0,		7, 7,	0,	XK_H},
 {67,	"J",	0,		7, 7,	0,	XK_J},
 {74,	"K",	0,		7, 7,	0,	XK_K},
 {83,	"L",	0,		7, 7,	0,	XK_L},
 {84,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {90,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
/* {98,	"Enter", 0,		16, 7,	0,	XK_Return},
 {0,	0,	0,	27, 7},*/
 {0,	0,	0,	43, 7},
#ifdef TEK_VENDOR_RELEASE_4
 {115,  "4",	"LeftArrow",	7, 7,	0,	XK_KP_4},
 {123,  "5",	0,		7, 7,	0,	XK_KP_5},
 {124,  "6",	"RightArrow",	7, 7,	0,	XK_KP_6}
#else
 {115,  "4",	"LeftArrow",	7, 7,	0,	XK_KP_4, XK_Left},
 {123,  "5",	0,		7, 7,	0,	XK_KP_5},
 {124,  "6",	"RightArrow",	7, 7,	0,	XK_KP_6, XK_Right}
#endif
};

static struct key TEK_101_row4 [] = {
 {26,	"Shift",0,		15, 7,	ShiftMask,	XK_Shift_L},
 {34,	"Z",	0,		7, 7,	0,		XK_Z},
 {42,	"X",	0,		7, 7,	0,		XK_X},
 {41,	"C",	0,		7, 7,	0,		XK_C},
 {50,	"V",	0,		7, 7,	0,		XK_V},
 {58,	"B",	0,		7, 7,	0,		XK_B},
 {57,	"N",	0,		7, 7,	0,		XK_N},
 {66,	"M",	0,		7, 7,	0,		XK_M},
 {73,	"<",	",",		7, 7,	0,		XK_comma, XK_less},
 {81,	">",	".",		7, 7,	0,		XK_period,XK_greater},
 {82,	"?",	"/",		7, 7,	0,		XK_slash, XK_question},
 {97,	"Shift",0,		20, 7,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,		10, 7},
 {107,	"UpArrow",	0,	7, 7,	0,		XK_Up},
 {0,	0,	0,		10, 7},
#ifdef TEK_VENDOR_RELEASE_4
 {113,	"1",	"End",		7, 7,	0,		XK_KP_1},
 {122,	"2",	"DownArrow",	7, 7,	0,		XK_KP_2},
 {130,  "3",	"PgDn",		7, 7,	0,		XK_KP_3},
#else
 {113,	"1",	"End",		7, 7,	0,		XK_KP_1, XK_End},
 {122,	"2",	"DownArrow",	7, 7,	0,		XK_KP_2, XK_Down},
 {130, "3",	"PgDn",		7, 7,	0,		XK_KP_3, XK_Next},
#endif
 {129,"Enter",	0,		7, 14,	0,		XK_KP_Enter}
};

static struct key TEK_101_row5 [] = {
 {25,"Caps",	"Lock",		12, 7,	LockMask,	XK_Caps_Lock},
 {0,    0,	0,		7, 7},
 {33,	"Alt",	0,		11, 7,	Mod1Mask,      	XK_Alt_L},
 {49,	" ",	0,		47, 7,	0,		XK_space},
#ifdef TEK_VENDOR_RELEASE_4
 {65,	"Alt",	0,		11, 7,	Mod1Mask,      	XK_Alt_R},
#else
 {65,	"Alt",	0,		11, 7,	0,      	XK_Alt_R},
#endif
 {0,    0,	0,		7, 7},
 {96,	"Option", 0,		10, 7,	ControlMask,	XK_Control_R},
 {0,    0,	0,		3, 7},
 {105,	"LeftArrow",	0,	7, 7,	0,		XK_Left},
 {104,	"DownArrow",	0,	7, 7,	0,		XK_Down},
 {114,	"RightArrow",	0,	7, 7,	0,		XK_Right},
 {0,    0,	0,		3, 7}, 
#ifdef TEK_VENDOR_RELEASE_4
 {120,	"0",	"Ins",		14, 7,	0,	XK_KP_0},
 {121,	".",	"Del",		7, 7,	0,	XK_KP_Decimal}
#else
 {120,	"0",	"Ins",		14, 7,	0,	XK_KP_0, XK_Insert},
 {121,	".",	"Del",		7, 7,	0,	XK_KP_Decimal, XK_Delete}
#endif
};

static struct row TEK_101_rows [] = {
  { sizeof (TEK_101_row0) / sizeof (struct key), 7, TEK_101_row0 },
  { 0, 7, 0 },
  { sizeof (TEK_101_row1) / sizeof (struct key), 7, TEK_101_row1 },
  { sizeof (TEK_101_row2) / sizeof (struct key), 7, TEK_101_row2 },
  { sizeof (TEK_101_row3) / sizeof (struct key), 7, TEK_101_row3 },
  { sizeof (TEK_101_row4) / sizeof (struct key), 7, TEK_101_row4 },
  { sizeof (TEK_101_row5) / sizeof (struct key), 7, TEK_101_row5 },
};

static struct keyboard TEK_101 = {
#ifdef TEK_VENDOR_RELEASE_4
  "TEK101-4", "Tektronix XP20 101-Key North American (R4)",
#else
  "TEK101", "Tektronix XP20 101-Key North American (R5)",
#endif
  sizeof (TEK_101_rows) / sizeof (struct row),
  TEK_101_rows,
  6, 3, 3
};
