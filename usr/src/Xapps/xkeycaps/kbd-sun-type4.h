/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the Sun type 4 keyboard.
 *
 * Unfortunately, the default keymap of this keyboard is different depending
 * on whether you're using an X server from MIT, or one from Sun.  So this 
 * file contains both configurations, switched on the KBD_OPENWINDOWS flag.
 * The file "kbd-sun-type4ow.h" includes this file with that defined.
 *
 * The same sort of hack still needs to be done to the type2 and type3 kbds.
 */

#ifdef KBD_OPENWINDOWS
/* MIT X11r5 has this info in <X11/Sunkeysym.h>, but MIT X11r4 doesn't.
   The file kbd-sun-type5ow.h uses these too.
 */
# define SunXK_F36     0x1005FF10
# define SunXK_F37     0x1005FF11
# define SunXK_Props   0x1005FF70
# define SunXK_Front   0x1005FF71
# define SunXK_Copy    0x1005FF72
# define SunXK_Open    0x1005FF73
# define SunXK_Paste   0x1005FF74
# define SunXK_Cut     0x1005FF75
# define SunXK_Sys_Req 0x1005FF60
# define SunXK_Props   0x1005FF70
#endif

static struct key Sun_type4_row0 [] = {
#ifdef KBD_OPENWINDOWS
 {0x08,	"Stop",	"L1",	7, 7,	0,	XK_F11, XK_F11, XK_Cancel},
 {0x0A,	"Again","L2",	7, 7,	0,	XK_F12, XK_F12, XK_Redo},
#else
 {0x08,	"Stop",	"L1",	7, 7,	0,	XK_F11},
 {0x0A,	"Again","L2",	7, 7,	0,	XK_F12},
#endif
 {0,	0,	0,	4, 7},
 {0x0C,	"F1",	0,	7, 7,	0,	XK_F1},
 {0x0D,	"F2",	0,	7, 7,	0,	XK_F2},
 {0x0F,	"F3",	0,	7, 7,	0,	XK_F3},
 {0x11,	"F4",	0,	7, 7,	0,	XK_F4},
 {0x13,	"F5",	0,	7, 7,	0,	XK_F5},
 {0x15,	"F6",	0,	7, 7,	0,	XK_F6},
 {0x17,	"F7",	0,	7, 7,	0,	XK_F7},
 {0x18,	"F8",	0,	7, 7,	0,	XK_F8},
 {0x19,	"F9",	0,	7, 7,	0,	XK_F9},
 {0x0E,	"F10",	0,	7, 7,	0,	XK_F10},
#ifdef KBD_OPENWINDOWS
 {0x10,	"F11",	0,	7, 7,	0,	SunXK_F36},
 {0x12,	"F12",	0,	7, 7,	0,	SunXK_F37},
 {0x5F,	"|",	"\\",	7, 7,	0,	XK_backslash,	XK_bar, XK_brokenbar},
#else
 {0x10,	"F11",	0,	7, 7,	0,	XK_F11},
 {0x12,	"F12",	0,	7, 7,	0,	XK_F12},
 {0x5F,	"|",	"\\",	7, 7,	0,	XK_backslash,	XK_bar},
#endif
 {0x49,	"Delete",0,	14, 7,	0,	XK_Delete},
 {0,	0,	0,	4, 7},
#ifdef KBD_OPENWINDOWS
 {0x1C,	"Pause","R1",	7, 7,	0,	XK_F21,	XK_F21,	XK_Pause},
 {0x1D,	"PrSc",	"R2",	7, 7,	0,	XK_F22, XK_F22, XK_Print},
 {0x1E,	"Scroll","Lock",7, 7,	0,	XK_F23,XK_F23,XK_Scroll_Lock,XK_Break},
 {0x69,	"Num",	"Lock", 7, 7,	Mod3Mask,XK_Num_Lock}
#else
 {0x1C,	"Pause","R1",	7, 7,	0,	XK_F21,		XK_Pause},
 {0x1D,	"PrSc",	"R2",	7, 7,	0,	XK_F22},
 {0x1E,	"Scroll","Lock",7, 7,	0,	XK_F23},
 {0x69,	"Num",	"Lock", 7, 7,	0,	XK_Num_Lock,	XK_Num_Lock}
#endif
};

static struct key Sun_type4_row1 [] = {
#ifdef KBD_OPENWINDOWS
 {0x20, "Props","L3",	7, 7,	Mod5Mask, XK_F13, XK_F13, SunXK_Props},
 {0x21, "Undo",	"L4",	7, 7,	0,	  XK_F14, XK_F14, XK_Undo},
#else
 {0x20, "Props","L3",	7, 7,	0,	XK_F13},
 {0x21, "Undo",	"L4",	7, 7,	0,	XK_F14},
#endif
 {0,	0,	0,	4, 7},
 {0x24, "Esc",	0,	7, 7,	0,	XK_Escape},
 {0x25,	"!",	"1",	7, 7,	0,	XK_1,		XK_exclam},
 {0x26,	"@",	"2",	7, 7,	0,	XK_2,		XK_at},
 {0x27,	"#",	"3",	7, 7,	0,	XK_3,		XK_numbersign},
 {0x28,	"$",	"4",	7, 7,	0,	XK_4,		XK_dollar},
 {0x29,	"%",	"5",	7, 7,	0,	XK_5,		XK_percent},
 {0x2A,	"^",	"6",	7, 7,	0,	XK_6,		XK_asciicircum},
 {0x2B,	"&",	"7",	7, 7,	0,	XK_7,		XK_ampersand},
 {0x2C,	"*",	"8",	7, 7,	0,	XK_8,		XK_asterisk},
 {0x2D,	"(",	"9",	7, 7,	0,	XK_9,		XK_parenleft},
 {0x2E,	")",	"0",	7, 7,	0,	XK_0,		XK_parenright},
 {0x2F,	"_",	"-",	7, 7,	0,	XK_minus,	XK_underscore},
 {0x30,	"+",	"=",	7, 7,	0,	XK_equal,	XK_plus},
 {0x32, "Backspace",0,	14, 7,	0,	XK_BackSpace},
 {0,	0,	0,	4, 7},
#ifdef KBD_OPENWINDOWS
 {0x34,	"=",	"R4",	7, 7,	0,	XK_F24,	XK_F24,	XK_KP_Equal},
 {0x35,	"/",	"R5",	7, 7,	0,	XK_F25,	XK_F25,	XK_KP_Divide},
 {0x36,	"*",	"R6",	7, 7,	0,	XK_F26,	XK_F26,	XK_KP_Multiply},
#else
 {0x34,	"=",	"R4",	7, 7,	0,	XK_F24,		XK_KP_Equal},
 {0x35,	"/",	"R5",	7, 7,	0,	XK_F25,		XK_KP_Divide},
 {0x36,	"*",	"R6",	7, 7,	0,	XK_F26,		XK_KP_Multiply},
#endif
 {0x4E,	"-",	0,	7, 7,	0,	XK_KP_Subtract,	XK_KP_Subtract}
};

static struct key Sun_type4_row2 [] = {
#ifdef KBD_OPENWINDOWS
 {0x38,	"Front","L5",		7, 7,	0,	XK_F15, XK_F15, SunXK_Front},
 {0x3A,	"Copy",	"L6",		7, 7,	0,	XK_F16, XK_F16, SunXK_Copy},
#else
 {0x38,	"Front","L5",		7, 7,	0,	XK_F15},
 {0x3A,	"Copy",	"L6",		7, 7,	0,	XK_F16},
#endif
 {0,	0,	0,		4, 7},
 {0x3C,	"Tab",	0,		10, 7,	0,	XK_Tab},
 {0x3D,	"Q",	0,		7, 7,	0,	XK_Q},
 {0x3E,	"W",	0,		7, 7,	0,	XK_W},
 {0x3F,	"E",	0,		7, 7,	0,	XK_E},
 {0x40,	"R",	0,		7, 7,	0,	XK_R},
 {0x41,	"T",	0,		7, 7,	0,	XK_T},
 {0x42,	"Y",	0,		7, 7,	0,	XK_Y},
 {0x43,	"U",	0,		7, 7,	0,	XK_U},
 {0x44,	"I",	0,		7, 7,	0,	XK_I},
 {0x45,	"O",	0,		7, 7,	0,	XK_O},
 {0x46,	"P",	0,		7, 7,	0,	XK_P},
 {0x47,	"{",	"[",		7, 7,	0,	XK_bracketleft,	XK_braceleft},
 {0x48,	"}",	"]",		7, 7,	0,	XK_bracketright,XK_braceright},
 {0,	0,	0,		3, 7},
 {0x60,	"Return",0,		8, 14,	0,	XK_Return},
 {0,	0,	0,		4, 7},
#ifdef KBD_OPENWINDOWS
 {0x4B,	"7",	"Home",		7, 7,	0, XK_F27, XK_F27, XK_KP_7, XK_Home},
 {0x4C,	"8",	"UpArrow",	7, 7,	0,	XK_Up,	XK_F28,  XK_KP_8},
 {0x4D,	"9",	"PgUp",		7, 7,	0, XK_F29, XK_F29, XK_KP_9, XK_Prior},
#else
 {0x4B,	"7",	"Home",		7, 7,	0,	XK_F27,		XK_KP_7},
 {0x4C,	"8",	"UpArrow",	7, 7,	0,	XK_Up,		XK_KP_8},
 {0x4D,	"9",	"PgUp",		7, 7,	0,	XK_F29,		XK_KP_9},
#endif
 {0x84,	"+",	0,		7, 14,	0,	XK_KP_Add,	XK_KP_Add}
};

static struct key Sun_type4_row3 [] = {
#ifdef KBD_OPENWINDOWS
 {0x4F,"Open",	"L7",		7, 7,	0,	  XK_F17, XK_F17, SunXK_Open},
 {0x50,"Paste",	"L8",		7, 7,	Mod5Mask, XK_F18, XK_F18, SunXK_Paste},
#else
 {0x4F,"Open",	"L7",		7, 7,	0,	XK_F17},
 {0x50,"Paste",	"L8",		7, 7,	0,	XK_F18},
#endif
 {0,	0,	0,		4, 7},
 {0x53,"Control",	0,		13, 7,	ControlMask,	XK_Control_L},
 {0x54,	"A",	0,		7, 7,	0,	XK_A},
 {0x55,	"S",	0,		7, 7,	0,	XK_S},
 {0x56,	"D",	0,		7, 7,	0,	XK_D},
 {0x57,	"F",	0,		7, 7,	0,	XK_F},
 {0x58,	"G",	0,		7, 7,	0,	XK_G},
 {0x59,	"H",	0,		7, 7,	0,	XK_H},
 {0x5A,	"J",	0,		7, 7,	0,	XK_J},
 {0x5B,	"K",	0,		7, 7,	0,	XK_K},
 {0x5C,	"L",	0,		7, 7,	0,	XK_L},
 {0x5D,	":",	";",		7, 7,	0,	XK_semicolon,	XK_colon},
 {0x5E,	"\"",	"'",		7, 7,	0,	XK_apostrophe,	XK_quotedbl},
 {0x31,	"~",	"`",		7, 7,	0,	XK_grave,	XK_asciitilde},
 {0,	0,	0,		12, 7},
#ifdef KBD_OPENWINDOWS
 {0x62,  "4",	"LeftArrow",	7, 7,	0,	XK_Left,XK_F30,	XK_KP_4},
 {0x63,  "5",	"R11",		7, 7,	0,	XK_F31,	XK_F31,	XK_KP_5},
 {0x64,  "6",	"RightArrow",	7, 7,	0,	XK_Right,XK_F32,XK_KP_6}
#else
 {0x62,  "4",	"LeftArrow",	7, 7,	0,	XK_Left,	XK_KP_4},
 {0x63,  "5",	"R11",		7, 7,	0,	XK_F31,		XK_KP_5},
 {0x64,  "6",	"RightArrow",	7, 7,	0,	XK_Right,	XK_KP_6}
#endif
};

static struct key Sun_type4_row4 [] = {
#ifdef KBD_OPENWINDOWS
 {0x66,	"Find",	"L9",		7, 7,	0,	  XK_F19, XK_F19, XK_Find},
 {0x68,	"Cut",	"L10",		7, 7,	Mod5Mask, XK_F20, XK_F20, SunXK_Cut},
#else
 {0x66,	"Find",	"L9",		7, 7,	0,	XK_F19},
 {0x68,	"Cut",	"L10",		7, 7,	0,	XK_F20},
#endif
 {0,	0,	0,		4, 7},
 {0x6A,	"Shift",0,		16, 7,	ShiftMask,	XK_Shift_L},
 {0x6B,	"Z",	0,		7, 7,	0,		XK_Z},
 {0x6C,	"X",	0,		7, 7,	0,		XK_X},
 {0x6D,	"C",	0,		7, 7,	0,		XK_C},
 {0x6E,	"V",	0,		7, 7,	0,		XK_V},
 {0x6F,	"B",	0,		7, 7,	0,		XK_B},
 {0x70,	"N",	0,		7, 7,	0,		XK_N},
 {0x71,	"M",	0,		7, 7,	0,		XK_M},
 {0x72,	"<",	",",		7, 7,	0,		XK_comma, XK_less},
 {0x73,	">",	".",		7, 7,	0,		XK_period,XK_greater},
 {0x74,	"?",	"/",		7, 7,	0,		XK_slash, XK_question},
 {0x75,	"Shift",0,		12, 7,	ShiftMask,	XK_Shift_R},
 {0x76,	"Line ","Feed",		7, 7,	0,		XK_Linefeed},
 {0,	0,	0,		4, 7},
#ifdef KBD_OPENWINDOWS
 {0x77,	"1",	"End",		7, 7,	0,	XK_R13,	XK_R13,XK_KP_1,XK_End},
 {0x78,	"2",	"DownArrow",	7, 7,	0,	XK_Down,XK_F34,XK_KP_2},
 {0x79,  "3",	"PgDn",		7, 7,	0,	XK_F35,XK_F35,XK_KP_3,XK_Next},
#else
 {0x77,	"1",	"End",		7, 7,	0,	XK_R13,		XK_KP_1},
 {0x78,	"2",	"DownArrow",	7, 7,	0,	XK_Down,	XK_KP_2},
 {0x79,  "3",	"PgDn",		7, 7,	0,	XK_F35,		XK_KP_3},
#endif
 {0x61,"Enter",	0,		7, 14,	0,	XK_KP_Enter,	XK_KP_Enter}
};

static struct key Sun_type4_row5 [] = {
 {0x7D,	"Help",	0,		14, 7,	0,		XK_Help, XK_Help},
 {0,	0,	0,		4, 7},
 {0x7E,	"Caps","Lock",		7, 7,	LockMask,	XK_Caps_Lock},
#ifdef KBD_OPENWINDOWS
 {0x1A,	"Alt",	0,		7, 7,	Mod4Mask,	XK_Alt_L},
#else
 {0x1A,	"Alt",	0,		7, 7,	0,		XK_Alt_L},
#endif
 {0x7F,	"<>",	0,		7, 7,	Mod1Mask,	XK_Meta_L},
 {0x80,	" ",	0,		63, 7,	0,		XK_space},
 {0x81,	"<>",	0,		7, 7,	Mod1Mask,	XK_Meta_R},
 {0x4A, "Com ",	"pose",		7, 7,	0,		XK_Multi_key},
#ifdef KBD_OPENWINDOWS
 {0x14,	"Alt",	"Graph",	7, 7,	Mod2Mask,	XK_Mode_switch},
#else
 {0x14,	"Alt",	"Graph",	7, 7,	0,		0},
#endif
 {0,	0,	0,		4, 7},
#ifdef KBD_OPENWINDOWS
 {0x65,	"0",	"Ins",		14, 7,	0, XK_Insert, XK_Insert, XK_KP_0},
 {0x39,	".",	"Del",		7, 7,	0, XK_Delete, XK_Delete, XK_KP_Decimal}
#else
 {0x65,	"0",	"Ins",		14, 7,	0, XK_Insert,		 XK_KP_0},
 {0x39,	".",	"Del",		7, 7,	0, XK_Delete,		 XK_KP_Decimal}
#endif
};

static struct row Sun_type4_rows [] = {
  { sizeof (Sun_type4_row0) / sizeof (struct key), 7, Sun_type4_row0 },
  { sizeof (Sun_type4_row1) / sizeof (struct key), 7, Sun_type4_row1 },
  { sizeof (Sun_type4_row2) / sizeof (struct key), 7, Sun_type4_row2 },
  { sizeof (Sun_type4_row3) / sizeof (struct key), 7, Sun_type4_row3 },
  { sizeof (Sun_type4_row4) / sizeof (struct key), 7, Sun_type4_row4 },
  { sizeof (Sun_type4_row5) / sizeof (struct key), 7, Sun_type4_row5 },
};

static struct keyboard Sun_type4 = {
#ifdef KBD_OPENWINDOWS
  "Sun4ow", "Sun type4 (OpenWindows layout)",
#else
  "Sun4", "Sun type4 (MIT layout)",
#endif
  sizeof (Sun_type4_rows) / sizeof (struct row),
  Sun_type4_rows,
  6, 3, 3
};
