/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the NCD X terminal N102 Swedish/Finnish keyboard.
 * By Pekka Nikander <pnr@innopoli.ajk.tele.fi>
 */

static struct key NCD_N102sf_row0 [] = {
 {0x08, "Esc",	0,	7, 7,	0,	XK_Escape},
 {0,	0,	0,	7, 7},
 {0x10,	"F1",	0,	7, 7,	0,	XK_F1},
 {0x0F,	"F2",	0,	7, 7,	0,	XK_F2},
 {0x17,	"F3",	0,	7, 7,	0,	XK_F3},
 {0x1F,	"F4",	0,	7, 7,	0,	XK_F4},
 {0,	0,	0,	4, 7},
 {0x27,	"F5",	0,	7, 7,	0,	XK_F5},
 {0x2F,	"F6",	0,	7, 7,	0,	XK_F6},
 {0x37,	"F7",	0,	7, 7,	0,	XK_F7},
 {0x3F,	"F8",	0,	7, 7,	0,	XK_F8},
 {0,	0,	0,	4, 7},
 {0x47,	"F9",	0,	7, 7,	0,	XK_F9},
 {0x4F, "F10",	0,	7, 7,	0,	XK_F10},
 {0x56, "F11",	0,	7, 7,	0,	XK_F11},
 {0x5E, "F12",	0,	7, 7,	0,	XK_F12},
 {0,	0,	0,	4, 7},
 {0x57,"Print",	0,	7, 7,	0,	XK_Print},
 {0x5F,"Scroll","lock",	7, 7,	0,	XK_Scroll_Lock},
 {0x62,"Setup", 0,	7, 7,	0,	0}
};

static struct key NCD_N102sf_row2 [] = {
 {0x0E,"\275",	"\247",	7, 7,	0,	XK_section,	XK_onehalf},
 {0x16,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {0x1E,	"\"",	"2",	7, 7,	0,	XK_2,		XK_quotedbl},
 {0x26,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {0x25,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {0x2E,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {0x36,	"&",	"6",	7, 7,	0,	XK_6,		XK_ampersand},
 {0x3D,	"/",	"7",	7, 7,	0,	XK_7,		XK_slash},
 {0x3E,	"(",	"8",	7, 7,	0,	XK_8,		XK_parenleft},
 {0x46,	")",	"9",	7, 7,	0,	XK_9,		XK_parenright},
 {0x45,	"=",	"0",	7, 7,	0,	XK_0,		XK_equal},
 {0x4E,	"?",	"+",	7, 7,	0,	XK_plus,	XK_question},
 {0x55,	"`",	"\264",	7, 7,	0,	XK_acute,	XK_grave},
 {0x66,"Backspace",0,	15, 7,	0,	XK_BackSpace},
 {0,	0,	0,	4, 7},
 {0x67,"Insert",0,	7, 7,	0,	XK_Insert},
 {0x6E,"Home",	0,	7, 7,	0,	XK_Home},
 {0x6F,"Page",	"Up",	7, 7,	0,	XK_Prior},
 {0,	0,	0,	4, 7},
 {0x76,"Num ",	"Lock",	7, 7,	Mod5Mask,	XK_Num_Lock},
 {0x77,	"/",	0,	7, 7,	0,		XK_KP_Divide},
 {0x7E,	"*",	0,	7, 7,	0,		XK_KP_Multiply},
 {0x7B,	"-",	0,	7, 7,	0,		XK_KP_Subtract}
};

static struct key NCD_N102sf_row3 [] = {
 {0x0D, "Tab",	0,	11, 7,	0,	XK_Tab},
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
 {0x54, "\305",	0,	7, 7,	0,	XK_Aring},
 {0x5B,	"^",	"\250",	7, 7,	0,	XK_diaeresis,	XK_asciicircum},
 {0,	0,	0,	2, 7},
 {0x5A,"Return",0,	9, 14,	0,	XK_Return},
 {0,	0,	0,	4, 7},
 {0x64,"Delete",0,	7, 7,	0,	XK_Delete},
 {0x65,"End",	0,	7, 7,	0,	XK_End},
 {0x6D,"Page",	"Down",	7, 7,	0,	XK_Next},
 {0,	0,	0,	4, 7},
 {0x6C,	"7",	0,	7, 7,	0,	XK_KP_7},
 {0x75,	"8",	0,	7, 7,	0,	XK_KP_8},
 {0x7D,	"9",	0,	7, 7,	0,	XK_KP_9},
 {0x7C,	"+",	0,	7, 14,	0,	XK_KP_Add}
};

static struct key NCD_N102sf_row4 [] = {
 {0x14,"Caps", "lock",	13, 7,	LockMask,	XK_Caps_Lock},
 {0x1C,	"A",	0,	7, 7,	0,		XK_A},
 {0x1B,	"S",	0,	7, 7,	0,		XK_S},
 {0x23,	"D",	0,	7, 7,	0,		XK_D},
 {0x2B,	"F",	0,	7, 7,	0,		XK_F},
 {0x34,	"G",	0,	7, 7,	0,		XK_G},
 {0x33,	"H",	0,	7, 7,	0,		XK_H},
 {0x3B,	"J",	0,	7, 7,	0,		XK_J},
 {0x42,	"K",	0,	7, 7,	0,		XK_K},
 {0x4B,	"L",	0,	7, 7,	0,		XK_L},
 {0x4C, "\326",	0,	7, 7,	0,		XK_Odiaeresis},
 {0x52, "\304",	0,	7, 7,	0,		XK_Adiaeresis},
 {0x53,	"*",	"'",	7, 7,	0,		XK_asterisk,	XK_apostrophe},
 {0,	0,	0,	38, 7},
 {0x6B,	"4",	0,	7, 7,	0,		XK_KP_4},
 {0x73,	"5",	0,	7, 7,	0,		XK_KP_5},
 {0x74,	"6",	0,	7, 7,	0,		XK_KP_6}
};

static struct key NCD_N102sf_row5 [] = {
 {0x12,"Shift",	0,	9, 7,	ShiftMask,	XK_Shift_L},
 {0x13,	">",	"<",	7, 7,	0,		XK_less,	XK_greater},
 {0x1A,	"Z",	0,	7, 7,	0,		XK_Z},
 {0x22,	"X",	0,	7, 7,	0,		XK_X},
 {0x21,	"C",	0,	7, 7,	0,		XK_C},
 {0x2A,	"V",	0,	7, 7,	0,		XK_V},
 {0x32,	"B",	0,	7, 7,	0,		XK_B},
 {0x31,	"N",	0,	7, 7,	0,		XK_N},
 {0x3A,	"M",	0,	7, 7,	0,		XK_M},
 {0x41,	";",	",",	7, 7,	0,		XK_comma,	XK_semicolon},
 {0x49,	":",	".",	7, 7,	0,		XK_period,	XK_colon},
 {0x4A,	"_",	"-",	7, 7,	0,		XK_underscore,	XK_hyphen},
 {0x59,"Shift",	0,	20, 7,	ShiftMask,	XK_Shift_R},
 {0,	0,	0,	11, 7},
 {0x63,"UpArrow",0,	7, 7,	0,		XK_Up},
 {0,	0,	0,	11, 7},
 {0x69,	"1",	0,	7, 7,	0,		XK_KP_1},
 {0x72,	"2",	0,	7, 7,	0,		XK_KP_2},
 {0x7A,	"3",	0,	7, 7,	0,		XK_KP_3},
 {0x79,"Enter",	0,	7, 14,	0,		XK_KP_Enter}
};

static struct key NCD_N102sf_row6 [] = {
 {0x11,"Ctrl",	0,	11, 7,	ControlMask,	XK_Control_L},
 {0,	0,	0,	7, 7},
 {0x19,"Alt",	0,	11, 7,	Mod1Mask,	XK_Alt_L},
 {0x29,	" ",	0,	48, 7,	0,		XK_space},
 {0,  "Alt Gr",	0,	11, 7,	Mod1Mask},	/* Not 0x39, right?  Magic. */
 {0,	0,	0,	7, 7},
 {0x58,"Ctrl",	0,	11, 7,	ControlMask,	XK_Control_R},
 {0,	0,	0,	4, 7},
 {0x61,"LeftArrow",0,	7, 7,	0,		XK_Left},
 {0x60,"DownArrow",0,	7, 7,	0,		XK_Down},
 {0x6A,"RightArrow",0,	7, 7,	0,		XK_Right},
 {0,	0,	0,	4, 7},
 {0x70,	"0",	0,	14, 7,	0,		XK_KP_0},
 {0x71,	".",	0,	7, 7,	0,		XK_KP_Decimal}
};

static struct row NCD_N102sf_rows [] = {
  { sizeof (NCD_N102sf_row0) / sizeof (struct key), 7, NCD_N102sf_row0 },
  { 0,	5, 0 },
  { sizeof (NCD_N102sf_row2) / sizeof (struct key), 7, NCD_N102sf_row2 },
  { sizeof (NCD_N102sf_row3) / sizeof (struct key), 7, NCD_N102sf_row3 },
  { sizeof (NCD_N102sf_row4) / sizeof (struct key), 7, NCD_N102sf_row4 },
  { sizeof (NCD_N102sf_row5) / sizeof (struct key), 7, NCD_N102sf_row5 },
  { sizeof (NCD_N102sf_row6) / sizeof (struct key), 7, NCD_N102sf_row6 }
};

static struct keyboard NCD_N102sf = {
  "N102SF", "Network Computing Devices N102 (Swedish/Finnish layout)",
  sizeof (NCD_N102sf_rows) / sizeof (struct row),
  NCD_N102sf_rows,
  6, 3, 3
};
