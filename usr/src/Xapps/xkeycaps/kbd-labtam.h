/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes a Labtam MT200 keyboard (Austrialian layout), which
 * is really a Honeywell-101RXd keyboard, also used with some IBM PCs.
 * By Anthony Thyssen <anthony@cit.gu.edu.au>
 */

static struct key H101RXd_row0 [] = {
 {0x76, 0,	"Esc",	7, 7,	0,	XK_Escape},
 {0,	0,	0,	7, 7},
 {0x78,	0,	"F1",	7, 7,	0,	XK_F1},
 {0x79,	0,	"F2",	7, 7,	0,	XK_F2},
 {0x7a,	0,	"F3",	7, 7,	0,	XK_F3},
 {0x7b,	0,	"F4",	7, 7,	0,	XK_F4},
 {0,	0,	0,	4, 7},
 {0x7c,	0,	"F5",	7, 7,	0,	XK_F5},
 {0x7d,	0,	"F6",	7, 7,	0,	XK_F6},
 {0x7e,	0,	"F7",	7, 7,	0,	XK_F7},
 {0x7f,	0,	"F8",	7, 7,	0,	XK_F8},
 {0,	0,	0,	3, 7},
 {0x80,	0,	"F9",	7, 7,	0,	XK_F9},
 {0x81,	0,	"F10",	7, 7,	0,	XK_F10},
 {0x82,  0,	"F11",	7, 7,	0,	XK_F11},
 {0x83,	0,	"F12",	7, 7,	0,	XK_F12},
 {0,	0,	0,	4, 7},
 {0x84, "Print","Screen",7, 7,	0,	XK_Print},
 {0,   "Scroll","Lock",	7, 7,	0,	XK_Scroll_Lock},
 {0x86,   0,    "Pause",7, 7,	0,	XK_Pause}
};

static struct key H101RXd_row2 [] = {
 {0x09,	"~",	"`",	7, 7,	0,	XK_grave,	XK_asciitilde},
 {0x0a,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {0x0b,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {0x0c,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {0x0d,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {0x0e,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {0x0f,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {0x10,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {0x11,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {0x12,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {0x13,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {0x14,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {0x15,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {0x25,	"|",	"\\",	7, 7,	0,	XK_backslash,	XK_bar},
 {0x17,	0,"LeftArrow",	7, 7,	0,	XK_BackSpace},
 {0,	0,	0,	4, 7},
 {0x53,	0,    "Insert",	7, 7,	0,	XK_Insert},
 {0x58,	0,    "Home",	7, 7,	0,	XK_Home},
 {0x5d,	"Page","Up",	7, 7,	0,	XK_Prior},
 {0,	0,	0,	4, 7},
 {0x62,	"Num",	"Lock",	7, 7,	Mod2Mask,
                     XK_Num_Lock, XK_Mode_switch, XK_Num_Lock, XK_Mode_switch},
 {0x67,	0,	"/",	7, 7,	0,	XK_KP_Divide},
 {0x6c,	0,	"*",	7, 7,	0,	XK_KP_Multiply},
 {0x71,	0,	"-",	7, 7,	0,	XK_KP_Subtract}
};

static struct key H101RXd_row3 [] = {
 {0x18,	0,	"Tab",	10, 7,	0,	XK_Tab},
 {0x19,	"Q",	0,	7, 7,	0,	XK_Q},
 {0x1a,	"W",	0,	7, 7,	0,	XK_W},
 {0x1b,	"E",	0,	7, 7,	0,	XK_E},
 {0x1c,	"R",	0,	7, 7,	0,	XK_R},
 {0x1d,	"T",	0,	7, 7,	0,	XK_T},
 {0x1e,	"Y",	0,	7, 7,	0,	XK_Y},
 {0x1f,	"U",	0,	7, 7,	0,	XK_U},
 {0x20,	"I",	0,	7, 7,	0,	XK_I},
 {0x21,	"O",	0,	7, 7,	0,	XK_O},
 {0x22,	"P",	0,	7, 7,	0,	XK_P},
 {0x23,	"{",	"[",	7, 7,	0,	XK_bracketleft,	 XK_braceleft},
 {0x24,	"}",	"]",	7, 7,	0,	XK_bracketright, XK_braceright},
 {0,	"",	0,	11, 8,	0,	XK_Return}, /* top part of key */
 {0,	0,	0,	4, 7},
 {0x54,	0,	"Delete",7, 7,	0,	XK_Delete},
 {0x59,	0,	"End",	7, 7,	0,	XK_End},
 {0x5e,	"Page",	"Down",	7, 7,	0,	XK_Next},
 {0,	0,	0,	4, 7},
 {0x63,	"7",    "Home",	7, 7,	0,	XK_Home,  XK_KP_7, XK_KP_7, XK_Home},
 {0x68,	"8",    "UpArrow",7, 7,	0,	XK_Up,    XK_KP_8, XK_KP_8, XK_Up},
 {0x6d,	"9",    "PgUp",	7, 7,	0,	XK_Prior, XK_KP_9, XK_KP_9, XK_Prior},
 {0x72,	"",	"+",	7, 14,	0,	XK_KP_Add}
};

static struct key H101RXd_row4 [] = {
 {0x26,	"Caps", "Lock",	8, 7,	LockMask,	XK_Caps_Lock},
 {0,	0,	0,	3, 7},
 {0x27,	"A",	0,	7, 7,	0,	XK_A},
 {0x28,	"S",	0,	7, 7,	0,	XK_S},
 {0x29,	"D",	0,	7, 7,	0,	XK_D},
 {0x2a,	"F",	0,	7, 7,	0,	XK_F},
 {0x2b,	"G",	0,	7, 7,	0,	XK_G},
 {0x2c,	"H",	0,	7, 7,	0,	XK_H},
 {0x2d,	"J",	0,	7, 7,	0,	XK_J},
 {0x2e,	"K",	0,	7, 7,	0,	XK_K},
 {0x2f,	"L",	0,	7, 7,	0,	XK_L},
 {0x30,	":",	";",	7, 7,	0,	XK_semicolon,	XK_colon},
 {0x31,	"\"",	"'",	7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {0x33,	0," Enter  <-'",17, 7,	0,	XK_Return},/* complete the return key */
 {0,	0,	0,	29, 7}, 
 {0x64,	"4","LeftArrow",7, 7,	0,	XK_Left,  XK_KP_4, XK_KP_4, XK_Left},
 {0x69,	"5",	0,	7, 7,	0,	0,        XK_KP_5, XK_KP_5},
 {0x6e,	"6","RightArrow",7, 7,	0,	XK_Right, XK_KP_6, XK_KP_6, XK_Right}
};

static struct key H101RXd_row5 [] = {
 {0x34,	0,	"Shift",15, 7,	ShiftMask, XK_Shift_L},
 {0x36,	"Z",	0,	7, 7,	0,	XK_Z},
 {0x37,	"X",	0,	7, 7,	0,	XK_X},
 {0x38,	"C",	0,	7, 7,	0,	XK_C},
 {0x39,	"V",	0,	7, 7,	0,	XK_V},
 {0x3a,	"B",	0,	7, 7,	0,	XK_B},
 {0x3b,	"N",	0,	7, 7,	0,	XK_N},
 {0x3c,	"M",	0,	7, 7,	0,	XK_M},
 {0x3d,	"<",	",",	7, 7,	0,	XK_comma,	XK_less},
 {0x3e,	">",	".",	7, 7,	0,	XK_period,	XK_greater},
 {0x3f,	"?",	"/",	7, 7,	0,	XK_slash,	XK_question},
 {0x41,	0,	"Shift",20, 7,	ShiftMask, XK_Shift_R},
 {0,	0,	0,	11, 7},
 {0x5b,   0,  "UpArrow",7, 7,	0,	XK_Up},
 {0,	0,	0,	11, 7},
 {0x65,	"1",	"End",	7, 7,	0,	XK_End,  XK_KP_1, XK_KP_1, XK_End},
 {0x6a,	"2","DownArrow",7, 7,	0,	XK_Down, XK_KP_2, XK_KP_2, XK_Down},
 {0x6f,	"3",	"PgDn",	7, 7,	0,	XK_Next, XK_KP_3, XK_KP_3, XK_Next},
 {0x74,  0,	"Enter",7, 14,	0,	XK_KP_Enter}
}; 
 
static struct key H101RXd_row6 [] = {
 {0x42,	0,	"Ctrl",	10, 7,	ControlMask,	XK_Control_L},
 {0,	0,	0,	7, 7},
 {0x44,	0,	"Alt",	11, 7,	Mod1Mask,	XK_Alt_L},
 {0x45,	" ",	0,	48, 7,	0,		XK_space},
 {0x46,	0,	"Alt",	11, 7,	Mod1Mask,	XK_Alt_R},
 {0,	0,	0,	7, 7},
 {0x48,	0,	"Ctrl",	11, 7,	ControlMask,	XK_Control_R},
 {0,	0,	0,	4, 7},
 {0x57,	0,"LeftArrow",	7, 7,	0,	XK_Left},
 {0x5c,	0,"DownArrow",	7, 7,	0,	XK_Down},
 {0x61,	0,"RightArrow",	7, 7,	0,	XK_Right},
 {0,	0,	0,	4, 7},
 {0x6b,	"0",	"Ins",	14, 7,	0,	XK_Insert, XK_KP_0, XK_KP_0, XK_Insert},
 {0x70,	".",	"Del",	7, 7,	0,
                            XK_Delete, XK_KP_Decimal, XK_KP_Decimal, XK_Delete}
};

static struct row H101RXd_rows [] = {
  { sizeof (H101RXd_row0) / sizeof (struct key), 7, H101RXd_row0 },
  { 0,	5, 0 },
  { sizeof (H101RXd_row2) / sizeof (struct key), 7, H101RXd_row2 },
  { sizeof (H101RXd_row3) / sizeof (struct key), 7, H101RXd_row3 },
  { sizeof (H101RXd_row4) / sizeof (struct key), 7, H101RXd_row4 },
  { sizeof (H101RXd_row5) / sizeof (struct key), 7, H101RXd_row5 },
  { sizeof (H101RXd_row6) / sizeof (struct key), 7, H101RXd_row6 }
};

static struct keyboard H101RXd = {
  "Labtam", "Labtam Xterminal MT200 (Austrialian)",
  sizeof (H101RXd_rows) / sizeof (struct row),
  H101RXd_rows,
  6, 3, 3
};
