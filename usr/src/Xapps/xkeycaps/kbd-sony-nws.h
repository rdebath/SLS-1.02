/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the SONY laptop workstation NWS1250 keyboard.
 * By Pavel Rosendorf <prf@jprix.che.wisc.edu>
 */

static struct key NWS_1250_row0 [] = {
 {0,	"POWER", " ON ", 7, 8,	0, 0},
 {0,	0,	0,	7, 7},
 {0x08,	"F1",	0,	9, 8,	0,	XK_F1},
 {0x09,	"F2",	0,	9, 8,	0,	XK_F2},
 {0x0A,	"F3",	0,	9, 8,	0,	XK_F3},
 {0x0B,	"F4",	0,	9, 8,	0,	XK_F4},
 {0x0C,	"F5",	0,	9, 8,	0,	XK_F5},
 {0,	0,	0,	3, 7},
 {0x0D,	"F6",	0,	9, 8,	0,	XK_F6},
 {0x0E,	"F7",	0,	9, 8,	0,	XK_F7},
 {0x0F,	"F8",	0,	9, 8,	0,	XK_F8},
 {0x10,	"F9",	0,	9, 8,	0,	XK_F9},
 {0x11,	"F10",	0,	9, 8,	0,	XK_F10}
};

static struct key NWS_1250_row2 [] = {
 {0x12,"Esc",	0,	7, 7,	0,	XK_Escape},
 {0x13,	"!",	"1",	7, 7,	0,	XK_1, 	XK_exclam},
 {0x14,	"@",	"2",	7, 7,	0,	XK_2, 	XK_at},
 {0x15,	"#",	"3",	7, 7,	0,	XK_3, 	XK_numbersign},
 {0x16,	"$",	"4",	7, 7,	0,	XK_4, 	XK_dollar},
 {0x17,	"%",	"5",	7, 7,	0,	XK_5, 	XK_percent},
 {0x18,	"^",	"6",	7, 7,	0,	XK_6, 	XK_asciicircum},
 {0x19,	"&",	"7",	7, 7,	0,	XK_7, 	XK_ampersand},
 {0x1A,	"*",	"8",	7, 7,	0,	XK_8, 	XK_asterisk},
 {0x1B,	"(",	"9",	7, 7,	0,	XK_9, 	XK_parenleft},
 {0x1C,	")",	"0",	7, 7,	0,	XK_0, 	XK_parenright},
 {0x1D,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {0x1E,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {0x1F,	"|",	"\\",	7, 7,	0,	XK_backslash,	XK_bar},
 {0x20,	"Bs",    0,	11, 7,	0,	XK_BackSpace}
};

static struct key NWS_1250_row3 [] = {
 {0x21,	"Tab",	0,	10, 7,	0,	XK_Tab},
 {0x22,	"Q",	0,	7, 7,	0,	XK_Q},
 {0x23,	"W",	0,	7, 7,	0,	XK_W},
 {0x24,	"E",	0,	7, 7,	0,	XK_E},
 {0x25,	"R",	0,	7, 7,	0,	XK_R},
 {0x26,	"T",	0,	7, 7,	0,	XK_T},
 {0x27,	"Y",	0,	7, 7,	0,	XK_Y},
 {0x28,	"U",	0,	7, 7,	0,	XK_U},
 {0x29,	"I",	0,	7, 7,	0,	XK_I},
 {0x2A,	"O",	0,	7, 7,	0,	XK_O},
 {0x2B,	"P",	0,	7, 7,	0,	XK_P},
 {0x2C,	"{",	"[",	7, 7,	0,	XK_bracketleft,  XK_braceleft},
 {0x2D,	"}",	"]",	7, 7,	0,	XK_bracketright, XK_braceright},
 {0x2E,	"Ins",	"Del",	7, 7,	0,	XK_Delete},
 {0x3C,	"Ret",	0,	8, 7,   0,      XK_Return}
};

static struct key NWS_1250_row4 [] = {
 {0x2F,	"Ctrl",	0,	12, 7,	ControlMask,	XK_Control_L},
 {0x30,	"A",	0,	7, 7,	0,		XK_A},
 {0x31,	"S",	0,	7, 7,	0,		XK_S},
 {0x32,	"D",	0,	7, 7,	0,		XK_D},
 {0x33,	"F",	0,	7, 7,	0,		XK_F},
 {0x34,	"G",	0,	7, 7,	0,		XK_G},
 {0x35,	"H",	0,	7, 7,	0,		XK_H},
 {0x36,	"J",	0,	7, 7,	0,		XK_J},
 {0x37,	"K",	0,	7, 7,	0,		XK_K},
 {0x38,	"L",	0,	7, 7,	0,		XK_L},
 {0x39,	":",	";",	7, 7,	0,		XK_semicolon, XK_colon},
 {0x3A,	"\"",	"'",	7, 7,	0,		XK_apostrophe, XK_quotedbl},
 {0x3B,	"~",	"`",	7, 7,	0,		XK_grave, XK_asciitilde},
 {0x3C,	"Return", 0,	13, 7,	0,		XK_Return}
};

static struct key NWS_1250_row5 [] = {
 {0x3D,"Shift",	0,	16, 7,	ShiftMask,	XK_Shift_L},
 {0x3E,	"Z",	0,	7, 7,	0,		XK_Z},
 {0x3F,	"X",	0,	7, 7,	0,		XK_X},
 {0x40,	"C",	0,	7, 7,	0,		XK_C},
 {0x41,	"V",	0,	7, 7,	0,		XK_V},
 {0x42,	"B",	0,	7, 7,	0,		XK_B},
 {0x43,	"N",	0,	7, 7,	0,		XK_N},
 {0x44,	"M",	0,	7, 7,	0,		XK_M},
 {0x45,	"<",	",",	7, 7,	0,		XK_comma, XK_less},
 {0x46,	">",	".",	7, 7,	0,		XK_period, XK_greater},
 {0x47,	"?",	"/",	7, 7,	0,		XK_slash, XK_question},
 /* This key is physically disabled (you have to open up the keyboard and
    remove a piece of plastic to enable it) but this is the keycode it will
    generate once you do that.
  */
 {0x48, 0,	0,	7, 7,	0,	        XK_Menu},  
 {0x49,"Shift",	0,	16, 7,	ShiftMask,	XK_Shift_R}
};

static struct key NWS_1250_row6 [] = {
 {0x4A,"Alt",	0,	9, 7,	Mod1Mask,	XK_Alt_L},
 /* This key is also physically disabled, and is rather strange once you
    enable it: it generates the keycodes 0x4F and 0x50 alternately; that
    is, it will generate the sequence
          0x4F KeyPress  0x4F KeyRelease  0x50 KeyPress  0x50 KeyRelease
    if pressed and released twice.
  */
 {0x4F, 0,	0,	7, 7,	0,  0},
 {0x4B,"Caps",	0,	7, 7,	LockMask,	XK_Caps_Lock},
 {0x4C,"Nfer",	0,	9, 7,	0,      	XK_Multi_key},
 {0x4D,	" ",	0,	31, 7,	0,		XK_space},
 {0x4E,"Xfer",	0,	9, 7,	0,      	XK_Select},
 {0x62,"LeftArrow", 0,	7, 7,	0,		XK_Left},
 {0x63,"DownArrow", 0,	7, 7,	0,		XK_Down},
 {0x5F,"UpArrow", 0,	7, 7,	0,		XK_Up},
 {0x64,"RightArrow", 0,	7, 7,	0,		XK_Right},
 {0x51,"Enter", 0,	9, 7,	0,		XK_Execute}
};

static struct row NWS_1250_rows [] = {
  { sizeof (NWS_1250_row0) / sizeof (struct key), 7, NWS_1250_row0 },
  { 0, 3, 0 },
  { sizeof (NWS_1250_row2) / sizeof (struct key), 7, NWS_1250_row2 },
  { sizeof (NWS_1250_row3) / sizeof (struct key), 7, NWS_1250_row3 },
  { sizeof (NWS_1250_row4) / sizeof (struct key), 7, NWS_1250_row4 },
  { sizeof (NWS_1250_row5) / sizeof (struct key), 7, NWS_1250_row5 },
  { sizeof (NWS_1250_row6) / sizeof (struct key), 7, NWS_1250_row6 }
};

static struct keyboard NWS_1250 = {
  "NWS", "Sony NWS 1250",
  sizeof (NWS_1250_rows) / sizeof (struct row),
  NWS_1250_rows,
  6, 3, 3
};
