/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the SGI Iris keyboard.
 * By Simon Leinen <simon@lia.di.epfl.ch>
 */

#ifndef XK_Sys_Req
#define XK_Sys_Req 0xFF15
#endif /* XK_Sys_Req */

static struct key SGI_iris_row0 [] = {
 {0x0E,	"Esc",	0,	7, 7,	0,	XK_Escape},
 {0,	0,	0,	7, 7},
 {0x5E,	"F1",	0,	7, 7,	0,	XK_F1},
 {0x5F,	"F2",	0,	7, 7,	0,	XK_F2},
 {0x60,	"F3",	0,	7, 7,	0,	XK_F3},
 {0x61,	"F4",	0,	7, 7,	0,	XK_F4},
 {0,	0,	0,	4, 7},
 {0x62,	"F5",	0,	7, 7,	0,	XK_F5},
 {0x63,	"F6",	0,	7, 7,	0,	XK_F6},
 {0x64,	"F7",	0,	7, 7,	0,	XK_F7},
 {0x65,	"F8",	0,	7, 7,	0,	XK_F8},
 {0,	0,	0,	4, 7},
 {0x66,	"F9",	0,	7, 7,	0,	XK_F9},
 {0x67,	"F10",	0,	7, 7,	0,	XK_F10},
 {0x68,	"F11",	0,	7, 7,	0,	XK_F11},
 {0x69,	"F12",	0,	7, 7,	0,	XK_F12},
 {0,	0,	0,	3, 7},
 {0x6A,	"Print", "Sys_req",	7, 7,	0,	XK_Print, XK_Sys_Req},
 {0x6B,	"Scroll",	0,	7, 7,	0,	XK_Scroll_Lock},
 {0x6C,	"Pause", "Break",	7, 7,	0,	XK_Pause, XK_Break},
};

static struct key SGI_iris_row1 [] = {
 {0x3E,	"~",	"`",	7, 7,	0,	XK_grave, XK_asciitilde},
 {0x0F,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {0x15,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {0x16,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {0x1D,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {0x1E,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {0x25,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {0x26,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {0x2D,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {0x2E,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {0x35,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {0x36,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {0x3D,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {0x44, "Backspace",0,	14, 7,	0,	XK_BackSpace},
 {0,	0,	0,	3, 7},
 {0x6D,	"Insert", 0,	7, 7,	0,	XK_Insert},
 {0x6E,	"Home",	0,	7, 7,	0,	XK_Home},
 {0x6F,	"PgUp", 0,	7, 7,	0,	XK_Prior},
 {0,	0,	0,	3, 7},
 {0x72,	"Num",	0,	7, 7,	0,	XK_Num_Lock},
 {0x73,	"/",	0,	7, 7,	0,	XK_KP_Divide, XK_KP_Divide},
 {0x74,	"*",	0,	7, 7,	0,	XK_KP_Multiply, XK_KP_Multiply},
 {0x53,	"-",	0,	7, 7,	0,	XK_KP_Subtract,	XK_KP_Subtract}
};

static struct key SGI_iris_row2 [] = {
 {0x10,	"Tab",	0,		11, 7,	0,	XK_Tab},
 {0x11,	"Q",	0,		7, 7,	0,	XK_Q},
 {0x17,	"W",	0,		7, 7,	0,	XK_W},
 {0x18,	"E",	0,		7, 7,	0,	XK_E},
 {0x1F,	"R",	0,		7, 7,	0,	XK_R},
 {0x20,	"T",	0,		7, 7,	0,	XK_T},
 {0x27,	"Y",	0,		7, 7,	0,	XK_Y},
 {0x28,	"U",	0,		7, 7,	0,	XK_U},
 {0x2F,	"I",	0,		7, 7,	0,	XK_I},
 {0x30,	"O",	0,		7, 7,	0,	XK_O},
 {0x37,	"P",	0,		7, 7,	0,	XK_P},
 {0x38,	"{",	"[",		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {0x3F,	"}",	"]",		7, 7,	0,	XK_bracketright,XK_braceright},
 {0x40,	"|",	"\\",		10, 7,	0,	XK_backslash,XK_bar},
 {0,	0,	0,	3, 7},
 {0x45,	"Delete", 0,	7, 7,	0,	XK_Delete},
 {0x70,	"End",	0,	7, 7,	0,	XK_End},
 {0x71,	"PgDn", 0,	7, 7,	0,	XK_Next},
 {0,	0,	0,	3, 7},
 {0x4A,	"7",	"Home",	7, 7,	0,	XK_Home, 	XK_KP_7},
 {0x4B,	"8",	"UpArrow",	7, 7,	XK_Begin,	XK_Up, XK_KP_8},
 {0x52,	"9",	"PgUp",	7, 7,	0,	XK_Prior, 	XK_KP_9},
 {0x75,	"+",	0,	7, 14,	0,	XK_KP_Add,	XK_KP_Add}
};

static struct key SGI_iris_row3 [] = {
 {0x0B,"Caps",	"Lock",		13, 7,	LockMask,	XK_Caps_Lock},
 {0x12,	"A",	0,		7, 7,	0,	XK_A},
 {0x13,	"S",	0,		7, 7,	0,	XK_S},
 {0x19,	"D",	0,		7, 7,	0,	XK_D},
 {0x1A,	"F",	0,		7, 7,	0,	XK_F},
 {0x21,	"G",	0,		7, 7,	0,	XK_G},
 {0x22,	"H",	0,		7, 7,	0,	XK_H},
 {0x29,	"J",	0,		7, 7,	0,	XK_J},
 {0x2A,	"K",	0,		7, 7,	0,	XK_K},
 {0x31,	"L",	0,		7, 7,	0,	XK_L},
 {0x32,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {0x39,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {0x3A,	"Enter", 0,		15, 7,	0,	XK_Return},
 {0,	0,	0,	27, 7},
 {0x46,  "4",	"LeftArrow",	7, 7,	0,	XK_Left,	XK_KP_4},
 {0x4C,  "5",	0,		7, 7,	0,	0,		XK_KP_5},
 {0x4D,  "6",	"RightArrow",	7, 7,	0,	XK_Right,	XK_KP_6}
};

static struct key SGI_iris_row4 [] = {
 {0x0D,	"Shift",0,		16, 7,	ShiftMask,	XK_Shift_L},
 {0x1B,	"Z",	0,		7, 7,	0,		XK_Z},
 {0x1C,	"X",	0,		7, 7,	0,		XK_X},
 {0x23,	"C",	0,		7, 7,	0,		XK_C},
 {0x24,	"V",	0,		7, 7,	0,		XK_V},
 {0x2B,	"B",	0,		7, 7,	0,		XK_B},
 {0x2C,	"N",	0,		7, 7,	0,		XK_N},
 {0x33,	"M",	0,		7, 7,	0,		XK_M},
 {0x34,	"<",	",",		7, 7,	0,		XK_comma, XK_less},
 {0x3B,	">",	".",		7, 7,	0,		XK_period,XK_greater},
 {0x3C,	"?",	"/",		7, 7,	0,		XK_slash, XK_question},
 {0x0C,	"Shift",0,		19, 7,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,		10, 7},
 {0x58,	"UpArrow",	0,	7, 7,	0,		XK_Up},
 {0,	0,	0,		10, 7},
 {0x41,	"1",	"End",		7, 7,	0,		XK_End,		XK_KP_1},
 {0x47,	"2",	"DownArrow",	7, 7,	0,		XK_Down,	XK_KP_2},
 {0x48, "3",	"PgDn",		7, 7,	0,		XK_Next,	XK_KP_3},
 {0x59,"Enter",	0,		7, 14,	0,		XK_KP_Enter,	XK_KP_Enter}
};

static struct key SGI_iris_row5 [] = {
 {0x0A,	"Ctrl", 0,		11, 7,	ControlMask,	XK_Control_L},
 {0,    0,	0,		7, 7},
 {0x5B,	"Alt",	0,		11, 7,	Mod1Mask,      	XK_Alt_L},
 {0x5A,	" ",	0,		47, 7,	0,		XK_space},
 {0x5C,	"Alt",	0,		11, 7,	Mod1Mask,      	XK_Alt_R},
 {0,    0,	0,		7, 7},
 {0x5D,	"Ctrl", 0,		11, 7,	ControlMask,	XK_Control_R},
 {0,    0,	0,		3, 7},
 {0x50,	"LeftArrow",	0,	7, 7,	0,		XK_Left},
 {0x51,	"DownArrow",	0,	7, 7,	0,		XK_Down},
 {0x57,	"RightArrow",	0,	7, 7,	0,		XK_Right},
 {0,    0,	0,		3, 7}, 
 {0x42,	"0",	"Ins",		14, 7,	0, XK_Insert,		 XK_KP_0},
 {0x49,	".",	"Del",		7, 7,	0, XK_Delete,		 XK_KP_Decimal}
};

static struct row SGI_iris_rows [] = {
  { sizeof (SGI_iris_row0) / sizeof (struct key), 7, SGI_iris_row0 },
  { 0, 7, 0 },
  { sizeof (SGI_iris_row1) / sizeof (struct key), 7, SGI_iris_row1 },
  { sizeof (SGI_iris_row2) / sizeof (struct key), 7, SGI_iris_row2 },
  { sizeof (SGI_iris_row3) / sizeof (struct key), 7, SGI_iris_row3 },
  { sizeof (SGI_iris_row4) / sizeof (struct key), 7, SGI_iris_row4 },
  { sizeof (SGI_iris_row5) / sizeof (struct key), 7, SGI_iris_row5 },
};

static struct keyboard SGI_iris = {
  "SGI", "Silicon Graphics",
  sizeof (SGI_iris_rows) / sizeof (struct row),
  SGI_iris_rows,
  6, 3, 3
};
