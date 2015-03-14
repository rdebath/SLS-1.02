/*
 * This file describes the HP 700/RX X terminal keyboard.
 * By Hide Horiuchi <hide@sierra.com>.
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

static struct key HP_700RX_row0 [] = {
 {17,	"Esc",	0,	8, 7,	0,	XK_Escape},
 {0,	0,	0,	7, 7},
 {16,	"F1",	0,	7, 7,	0,	XK_F1},
 {24,	"F2",	0,	7, 7,	0,	XK_F2},
 {32,	"F3",	0,	7, 7,	0,	XK_F3},
 {40,	"F4",	0,	7, 7,	0,	XK_F4},
 {0,	0,	0,	3, 7},
 {48,	"F5",	0,	7, 7,	0,	XK_F5},
 {56,	"F6",	0,	7, 7,	0,	XK_F6},
 {64,	"F7",	0,	7, 7,	0,	XK_F7},
 {72,	"F8",	0,	7, 7,	0,	XK_F8},
 {0,	0,	0,	3, 7},
 {80,	"F9",	0,	7, 7,	0,	XK_F9,		XK_KP_F1},
 {88,	"F10",	0,	7, 7,	0,	XK_F10,		XK_KP_F2},
 {95,	"F11",	0,	7, 7,	0,	XK_F11,		XK_KP_F3},
 {103,	"F12",	0,	7, 7,	0,	XK_F12,		XK_KP_F4},
 {0,	0,	0,	3, 7},
 {96,	"Print","Screen",7, 7,	0,	XK_Print},
 {104,	"Scroll","Lock",7, 7,	0,	XK_Scroll_Lock},
 {107,	"Pause", 0,	7, 7,	0,	XK_Pause,	XK_Break},
};

static struct key HP_700RX_row2 [] = {
 {23,	"~",	"`",	9, 7,	0,	XK_grave,	XK_asciitilde},
 {31,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {39,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {47,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {46,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {55,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {63,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {70,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {71,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {79,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {78,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {87,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {94,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {111,"Backspace",0,	12, 7,	0,	XK_BackSpace},
 {0,	0,	0,	3, 7},
 {112,"Insert",	0,	7, 7,	0,	XK_Insert},
 {119,"Home",	0,	7, 7,	0,	XK_Home},
 {120,	"Page",	"Up",	7, 7,	0,	XK_Prior},
 {0,	0,	0,	3, 7},
 {127,	"Num",	"Lock",	7, 7,	 Mod5Mask,	XK_Num_Lock,	XK_Num_Lock},
 {128,	"/",	0,	7, 7,	0,	XK_KP_Divide},
 {135,	"*",	0,	7, 7,	0,	XK_KP_Multiply},
 {129,"-",	0,	7, 7,	0,	XK_KP_Subtract}
};

static struct key HP_700RX_row3 [] = {
 {22,"Tab",	0,		12, 7,	0,	XK_Tab,	XK_BackTab},
 {30,	"Q",	0,		7, 7,	0,	XK_Q},
 {38,	"W",	0,		7, 7,	0,	XK_W},
 {45,	"E",	0,		7, 7,	0,	XK_E},
 {54,	"R",	0,		7, 7,	0,	XK_R},
 {53,	"T",	0,		7, 7,	0,	XK_T},
 {62,	"Y",	0,		7, 7,	0,	XK_Y},
 {69,	"U",	0,		7, 7,	0,	XK_U},
 {76,	"I",	0,		7, 7,	0,	XK_I},
 {77,	"O",	0,		7, 7,	0,	XK_O},
 {86,	"P",	0,		7, 7,	0,	XK_P},
 {93,	"{",	"[",		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {100,	"}",	"]",		7, 7,	0,	XK_bracketright,XK_braceright},
 {101,	"|",	"\\",		9, 7,	0,	XK_backslash,	XK_bar},
 {0,	0,	0,		3, 7},
 {109,"Delete",	0,		7, 7,	0,	XK_Delete},
 {110,"End",	0,		7, 7,	0,	XK_End},
 {118, "Page",	"Down",		7, 7,	0,	XK_Next},
 {0,	0,	0,		3, 7},
 {117,	"7",	"Home",		7, 7,	0,	XK_KP_7,	XK_Home},
 {126,"8",	"UpArrow",	7, 7,	0,	XK_KP_8,	XK_Up},
 {134,"9",	"Pg Up",	7, 7,	0,	XK_KP_9,	XK_Prior},
 {133,	"+",	0,		7, 14,	0,	XK_KP_Add},
};

static struct key HP_700RX_row4 [] = {
 {29,"Caps Lock",0,		14, 7,	LockMask,	XK_Caps_Lock},
 {37,	"A",	0,		7, 7,	0,	XK_A},
 {36,	"S",	0,		7, 7,	0,	XK_S},
 {44,	"D",	0,		7, 7,	0,	XK_D},
 {52,	"F",	0,		7, 7,	0,	XK_F},
 {61,	"G",	0,		7, 7,	0,	XK_G},
 {60,	"H",	0,		7, 7,	0,	XK_H},
 {68,	"J",	0,		7, 7,	0,	XK_J},
 {75,	"K",	0,		7, 7,	0,	XK_K},
 {84,	"L",	0,		7, 7,	0,	XK_L},
 {85,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {91,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {99,"Enter",	0,		14, 7,	0,	XK_Return},
 {0,	0,	0,		3, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		3, 7},
 {116,"4",	"LeftArrow",	7, 7,	0,	XK_KP_4,	XK_Left},
 {124,	"5",	0,		7, 7,	0,	XK_KP_5},
 {125,"6",	"RightArrow",	7, 7,	0,	XK_KP_6,	XK_Right},
};

static struct key HP_700RX_row5 [] = {
 {27,"Shift",	0,		17, 7,	ShiftMask,	XK_Shift_L},
 {35,	"Z",	0,		7, 7,	0,		XK_Z},
 {43,	"X",	0,		7, 7,	0,		XK_X},
 {42,	"C",	0,		7, 7,	0,		XK_C},
 {51,	"V",	0,		7, 7,	0,		XK_V},
 {59,	"B",	0,		7, 7,	0,		XK_B},
 {58,	"N",	0,		7, 7,	0,		XK_N},
 {67,	"M",	0,		7, 7,	0,		XK_M},
 {74,	"<",	",",		7, 7,	0,		XK_comma, XK_less},
 {82,	">",	".",		7, 7,	0,		XK_period,XK_greater},
 {83,	"?",	"/",		7, 7,	0,		XK_slash, XK_question},
 {98,	"Shift",0,		18, 7,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,		3, 7},
 {0,	0,	0,		7, 7},
 {108,	"UpArrow",0,		7, 7,	0,		XK_Up},
 {0,	0,	0,		7, 7},
 {0,	0,	0,		3, 7},
 {114,	"1",	"End",		7, 7,	0,		XK_KP_1, XK_End},
 {123,	"2",	"DownArrow",	7, 7,	0,		XK_KP_2, XK_Down},
 {131,	"3",	"Pg Dn",	7, 7,	0,		XK_KP_3, XK_Next},
 {130,	"Enter",0,		7, 14,	0,		XK_KP_Enter}
};

static struct key HP_700RX_row6 [] = {
 {26,	"Ctrl",	0,	10, 7,	ControlMask,	XK_Control_L},
 {0,	0,	0,	7, 7},
 {34, 	"Alt",	0,	9, 7,	Mod1Mask,	XK_Alt_L},
 {50,	" ",	0,	53, 7,	0,		XK_space},
 {66,	"Alt",	0,	9, 7,	Mod1Mask,	XK_Alt_R},
 {0,	0,	0,	7, 7},
 {97,	"Ctrl",	0,	10, 7,	ControlMask,	XK_Control_R},
 {0,	0,	0,	3, 7},
 {106,	"LeftArrow",0,	7, 7,	0,		XK_Left},
 {105,	"DownArrow",0,	7, 7,	0,		XK_Down},
 {115,	"RightArrow",0,	7, 7,	0,		XK_Right},
 {0,	0,	0,	3, 7},
 {121,	"0",	"Ins",	14, 7,	0,		XK_KP_0,	XK_Insert},
 {122,	".",	"Del",	7, 7,	0,		XK_KP_Decimal,	XK_Delete}
};

static struct row HP_700RX_rows [] = {
  { sizeof (HP_700RX_row0) / sizeof (struct key), 7, HP_700RX_row0 },
  { 0, 3, 0 },
  { sizeof (HP_700RX_row2) / sizeof (struct key), 7, HP_700RX_row2 },
  { sizeof (HP_700RX_row3) / sizeof (struct key), 7, HP_700RX_row3 },
  { sizeof (HP_700RX_row4) / sizeof (struct key), 7, HP_700RX_row4 },
  { sizeof (HP_700RX_row5) / sizeof (struct key), 7, HP_700RX_row5 },
  { sizeof (HP_700RX_row6) / sizeof (struct key), 7, HP_700RX_row6 }
};

static struct keyboard HP_700RX = {
  "HP700RX", "Hewlett Packard 700/RX X Terminal",
  sizeof (HP_700RX_rows) / sizeof (struct row),
  HP_700RX_rows,
  6, 3, 3
};
