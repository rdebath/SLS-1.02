/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the NCD X terminal VT220 keyboard.
 *
 * I don't have access to an NCD-vt220 keyboard; let me know if the default
 * configuration encoded in this file is incorrect.
 */

static struct key NCD_VT220_row0 [] = {
 {0x10,	"F1",	0,		7, 7,	0,	XK_F1},
 {0x0F,	"F2",	0,		7, 7,	0,	XK_F2},
 {0x17, "F3",	"Compose Setup",7, 7,	0,	0},
 {0x1F,	"F4",	0,		7, 7,	0,	XK_F4},
 {0x27, "F5",	"Break",	7, 7,	0,	XK_F5},
 {0,	0,	0,		7, 7},
 {0x2F,	"F6",	0,		7, 7,	0,	XK_F6},
 {0x37,	"F7",	0,		7, 7,	0,	XK_F7},
 {0x3F,	"F8",	0,		7, 7,	0,	XK_F8},
 {0x47,	"F9",	0,		7, 7,	0,	XK_F9},
 {0x4F, "F10",	0,		7, 7,	0,	XK_F10},
 {0,	0,	0,		7, 7},
 {0x0A, "F11",	"Esc",		7, 7,	0,	XK_Escape},
 {0x0B, "F12",	"Backspace",	7, 7,	0,	XK_BackSpace},
 {0x0C, "F13",	"Line Feed",	7, 7,	0,	XK_Linefeed},
 {0x50, "F14",	0,		7, 7,	0,	XK_F14},
 {0,	0,	0,		7, 7},
 {0x51, "Help",	0,		8, 7,	0,	XK_Help},
 {0x7F,	"Do",	0,		16, 7,	0,	XK_Menu},
 {0,	0,	0,		7, 7},
 {0x80, "F17",	0,		7, 7,	0,	XK_F17},
 {0x81, "F18",	0,		7, 7,	0,	XK_F18},
 {0x82, "F19",	0,		7, 7,	0,	XK_F19},
 {0x83, "F20",	0,		7, 7,	0,	XK_F20}
};

static struct key NCD_VT220_row2 [] = {
 {0,	0,	0,	5, 7},
 {0x0E,	"~",	"`",	7, 7,	0,	XK_grave,	XK_asciitilde},
 {0x16,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {0x1E,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {0x26,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {0x25,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {0x2E,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {0x36,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {0x3D,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {0x3E,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {0x46,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {0x45,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {0x4E,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {0x55,	"+",	"=",	7, 7,	0,	XK_plus,	XK_equal},
 {0x66,"<X|",	0,	11, 7,	0,	XK_Delete},
 {0,	0,	0,	12, 7},
 {0x6E,"Find",	0,	8, 7,	0,	XK_Find},
 {0x67,"Insert","Here",	8, 7,	0,	XK_Insert},
 {0x64,"Re-","move",	8, 7,	0, 	0},
 {0,	0,	0,	7, 7},
 {0x08,"PF1",	0,	7, 7,	0,	XK_KP_F1},
 {0x76,"PF2",	0,	7, 7,	0,	XK_KP_F2},
 {0x5F,"PF3",	0,	7, 7,	0,	XK_KP_F3},
 {0x57,"PF4",	0,	7, 7,	0,	XK_KP_F4}
};

static struct key NCD_VT220_row3 [] = {
 {0,	0,	0,	5, 7},
 {0x0D,"Tab",	0,	12, 7,	0,	XK_Tab},
 {0x15,	"Q",	0,	7, 7,	0,	XK_Q},
 {0x1D,	"W",	0,	7, 7,	0,	XK_W},
 {0x24,	"E",	0,	7, 7,	0,	XK_E},
 {0x2D,	"R",	0,	7, 7,	0,	XK_R},
 {0x2C,	"T",	0,	7, 7,	0,	XK_T},
 {0x35,	"Y",	0,	7, 7,	0,	XK_Y},
 {0x3C,	"U",	0,	7, 7,	0,	XK_U},
 {0x43,	"I",	0,	7, 7,	0,	XK_I},
 {0x44,	"O",	0,	7, 7,	0,	XK_O},
 {0x4D,	"P",	0,	7, 7,	0,	XK_P},
 {0x54,	"{",	"[",	7, 7,	0,	XK_bracketleft,	 XK_braceleft},
 {0x5B,	"}",	"]",	7, 7,	0,	XK_bracketright, XK_braceright},
 {0,	0,	0,	2, 7},
 {0x5A,"Return",0,	8, 14,	0,	XK_Return},
 {0,	0,	0,	8, 7},
 {0x65,"Select", 0,	8, 7,	0,	XK_Select},
 {0x6F,"Prev","Screen",	8, 7,	0,	XK_Prior},
 {0x6D,"Next","Screen",	8, 7,	0,	XK_Next},
 {0,	0,	0,	7, 7},
 {0x6C,	"7",	0,	7, 7,	0,	XK_KP_7},
 {0x75,	"8",	0,	7, 7,	0,	XK_KP_8},
 {0x7D,	"9",	0,	7, 7,	0,	XK_KP_9},
 {0x84,	"-",	0,	7, 7,	0,	XK_KP_Subtract}
};

static struct key NCD_VT220_row4 [] = {
 {0x11,"Ctrl",	0,	7, 7,	ControlMask,	XK_Control_L},
 {0x14,"Lock",	0,	12, 7,	LockMask,	XK_Caps_Lock},
 {0x1C,	"A",	0,	7, 7,	0,		XK_A},
 {0x1B,	"S",	0,	7, 7,	0,		XK_S},
 {0x23,	"D",	0,	7, 7,	0,		XK_D},
 {0x2B,	"F",	0,	7, 7,	0,		XK_F},
 {0x34,	"G",	0,	7, 7,	0,		XK_G},
 {0x33,	"H",	0,	7, 7,	0,		XK_H},
 {0x3B,	"J",	0,	7, 7,	0,		XK_J},
 {0x42,	"K",	0,	7, 7,	0,		XK_K},
 {0x4B,	"L",	0,	7, 7,	0,		XK_L},
 {0x4C,	":",	";",	7, 7,	0,		XK_semicolon,	XK_colon},
 {0x52,	"\"",	"'",	7, 7,	0,		XK_apostrophe,	XK_quotedbl},
 {0x53,	"|",	"\\",	7, 7,	0,		XK_backslash,	XK_bar},
 {0,	0,	0,	24, 7},
 {0x63,"UpArrow",0,	8, 7,	0,		XK_Up},
 {0,	0,	0,	15, 7},
 {0x6B,	"4",	0,	7, 7,	0,		XK_KP_4},
 {0x73,	"5",	0,	7, 7,	0,		XK_KP_5},
 {0x74,	"6",	0,	7, 7,	0,		XK_KP_6},
 {0x7C,	",",	0,7, 7,	0,			XK_KP_Separator}
};

static struct key NCD_VT220_row5 [] = {
 {0x12,"Shift",	0,	16, 7,	ShiftMask,	XK_Shift_L},
 {0x09,	">",	"<",	7, 7,	0,		XK_less,	XK_greater},
 {0x1A,	"Z",	0,	7, 7,	0,		XK_Z},
 {0x22,	"X",	0,	7, 7,	0,		XK_X},
 {0x21,	"C",	0,	7, 7,	0,		XK_C},
 {0x2A,	"V",	0,	7, 7,	0,		XK_V},
 {0x32,	"B",	0,	7, 7,	0,		XK_B},
 {0x31,	"N",	0,	7, 7,	0,		XK_N},
 {0x3A,	"M",	0,	7, 7,	0,		XK_M},
 {0x41,	",",	",",	7, 7,	0,		XK_comma,	XK_comma},
 {0x49,	".",	".",	7, 7,	0,		XK_period,	XK_period},
 {0x4A,	"?",	"/",	7, 7,	0,		XK_slash,	XK_question},
 {0x59,"Shift",	0,	16, 7,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,	10, 7},
 {0x61,"LeftArrow",0,	8, 7,	0,		XK_Left},
 {0x60,"DownArrow",0,	8, 7,	0,		XK_Down},
 {0x6A,"RightArrow",0,	8, 7,	0,		XK_Right},
 {0,	0,	0,	7, 7,	0,		XK_KP_0},
 {0x69,	"1",	0,	7, 7,	0,		XK_KP_1},
 {0x72,	"2",	0,	7, 7,	0,		XK_KP_2},
 {0x7A,	"3",	0,	7, 7,	0,		XK_KP_3},
 {0x79,"Enter",	0,	7, 14,	0,		XK_KP_Enter}
};

static struct key NCD_VT220_row6 [] = {
 {0,	0,	0,		9, 7},
 {0x19,"Compose","Character",	16, 7,	Mod1Mask,	XK_Alt_L},
 {0x29,	" ",	0,		64, 7,	0,		XK_space},
 {0,	0,	0,		61, 7},
 {0x70,	"0",	0,		14, 7,	0,		XK_KP_0},
 {0x71,	".",	0,		7, 7,	0,		XK_KP_Decimal}
};

static struct row NCD_VT220_rows [] = {
  { sizeof (NCD_VT220_row0) / sizeof (struct key), 7, NCD_VT220_row0 },
  { 0, 7, 0 },
  { sizeof (NCD_VT220_row2) / sizeof (struct key), 7, NCD_VT220_row2 },
  { sizeof (NCD_VT220_row3) / sizeof (struct key), 7, NCD_VT220_row3 },
  { sizeof (NCD_VT220_row4) / sizeof (struct key), 7, NCD_VT220_row4 },
  { sizeof (NCD_VT220_row5) / sizeof (struct key), 7, NCD_VT220_row5 },
  { sizeof (NCD_VT220_row6) / sizeof (struct key), 7, NCD_VT220_row6 }
};

static struct keyboard NCD_VT220 = {
  "NCD220", "Network Computing Devices vt220",
  sizeof (NCD_VT220_rows) / sizeof (struct row),
  NCD_VT220_rows,
  6, 3, 3
};
