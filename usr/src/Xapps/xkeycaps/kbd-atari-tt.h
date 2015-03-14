/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the Atari TT (USA) keyboard
 * By Mats Wichmann <mats@alruna.com>
 */

static struct key Atari_TT_row0 [] = {
 {67,	"F1",	0,	31, 10,	0,  XK_F1, XK_F1},
 {68,	"F2",	0,	31, 10,	0,  XK_F2, XK_F2},
 {69,	"F3",	0,	31, 10,	0,  XK_F3, XK_F3},
 {70,	"F4",	0,	31, 10,	0,  XK_F4, XK_F4},
 {71,	"F5",	0,	31, 10,	0,  XK_F5, XK_F5},
 {72,	"F6",	0,	31, 10,	0,  XK_F6, XK_F6},
 {73,	"F7",	0,	31, 10,	0,  XK_F7, XK_F7},
 {74,	"F8",	0,	31, 10,	0,  XK_F8, XK_F8},
 {75,	"F9",	0,	31, 10,	0,  XK_F9, XK_F9},
 {76,	"F10",	0,	31, 10,	0,  XK_F10,XK_F10},
};

static struct key Atari_TT_row2 [] = {
 {9,	"Esc",	0,	20, 20,	0,	XK_Escape,	XK_Escape},
 {10,	"!",	"1",	20, 20,	0,	XK_1,		XK_exclam},
 {11,	"@",	"2",	20, 20,	0,	XK_2,		XK_at},
 {12,	"#",	"3",	20, 20,	0,	XK_3,		XK_numbersign},
 {13,	"$",	"4",	20, 20,	0,	XK_4,		XK_dollar},
 {14,	"%",	"5",	20, 20,	0,	XK_5,		XK_percent},
 {15,	"^",	"6",	20, 20,	0,	XK_6,		XK_asciicircum},
 {16,	"&",	"7",	20, 20,	0,	XK_7,		XK_ampersand},
 {17,	"*",	"8",	20, 20,	0,	XK_8,		XK_asterisk},
 {18,	"(",	"9",	20, 20,	0,	XK_9,		XK_parenleft},
 {19,	")",	"0",	20, 20,	0,	XK_0,		XK_parenright},
 {20,	"_",	"-",	20, 20,	0,	XK_minus,	XK_underscore},
 {21,	"+",	"=",	20, 20,	0,	XK_equal,	XK_plus},
 {49,	"~",	"`",	20, 20,	0,	XK_grave,	XK_asciitilde},
 {22,"Backspace",0,	30, 20,	0,	XK_BackSpace,	XK_BackSpace},
 {0,	0,	0,	7, 20},
 {106,"Help",	0,	30, 20,	0,	XK_Help,	XK_Help},
 {105,"Undo",	0,	30, 20,	0,	XK_Undo,	XK_Undo},
 {0,	0,	0,	7, 20},
 {107,	"(",	0,	20, 20,	0,	XK_parenleft,	XK_parenleft},
 {108,	")",	0,	20, 20,	0,	XK_parenright,	XK_parenright},
 {109,	"/",	0,	20, 20,	0,	XK_KP_Divide,	XK_KP_Divide},
 {110,	"*",	0,	20, 20,	0,	XK_KP_Multiply,	XK_KP_Multiply},
};

static struct key Atari_TT_row3 [] = {
 {23,	"Tab",	0,	30, 20,	0,	XK_Tab,		XK_Tab},
 {24,	"Q",	0,	20, 20,	0,	XK_q,		XK_Q},
 {25,	"W",	0,	20, 20,	0,	XK_w,		XK_W},
 {26,	"E",	0,	20, 20,	0,	XK_e,		XK_E},
 {27,	"R",	0,	20, 20,	0,	XK_r,		XK_R},
 {28,	"T",	0,	20, 20,	0,	XK_t,		XK_T},
 {29,	"Y",	0,	20, 20,	0,	XK_y,		XK_Y},
 {30,	"U",	0,	20, 20,	0,	XK_u,		XK_U},
 {31,	"I",	0,	20, 20,	0,	XK_i,		XK_I},
 {32,	"O",	0,	20, 20,	0,	XK_o,		XK_O},
 {33,	"P",	0,	20, 20,	0,	XK_p,		XK_P},
 {34,	"{",	"[",	20, 20,	0,	XK_bracketleft,	XK_braceleft},
 {35,	"}",	"]",	20, 20,	0,	XK_bracketright,XK_braceright},
 /* Return continues up here (from row 4), width 20 */
 {0,	0,	0,	20, 20},
 {91,	"Delete",0,	20, 20,	0,	XK_Delete,	XK_Delete},
 {0,	0,	0,	7, 20},
 {90,	"Insert",0,	20, 20,	0,	XK_Insert,	XK_Insert},
 {80,	"UpArrow",0,	20, 20,	0,	XK_Up,		XK_Up},
 {79,	"Clr","Home",	20, 20,	0,	XK_Home,	XK_Clear},
 {0,	0,	0,	7, 20},
 {111,	"7",	0,	20, 20,	0,	XK_KP_7,	XK_KP_7},
 {112,	"8",	0,	20, 20,	0,	XK_KP_8,	XK_KP_8},
 {113,	"9",	0,	20, 20,	0,	XK_KP_9,	XK_KP_9},
 {82,	"-",	0,	20, 20,	0,	XK_KP_Subtract,	XK_KP_Subtract},
};

static struct key Atari_TT_row4 [] = {
 {37,	"Control",0,	35,	20, ControlMask, XK_Control_L, XK_Control_L},
 {38,	"A",	0,	20, 20,	0,	XK_a,		XK_A},
 {39,	"S",	0,	20, 20,	0,	XK_s,		XK_S},
 {40,	"D",	0,	20, 20,	0,	XK_d,		XK_D},
 {41,	"F",	0,	20, 20,	0,	XK_f,		XK_F},
 {42,	"G",	0,	20, 20,	0,	XK_g,		XK_G},
 {43,	"H",	0,	20, 20,	0,	XK_h,		XK_H},
 {44,	"J",	0,	20, 20,	0,	XK_j,		XK_J},
 {45,	"K",	0,	20, 20,	0,	XK_k,		XK_K},
 {46,	"L",	0,	20, 20,	0,	XK_l,		XK_L},
 {47,	":",	";",	20, 20,	0,	XK_semicolon,	XK_colon},
 {48,	"\"",	"'",	20, 20,	0,	XK_apostrophe,	XK_quotedbl},
 {36,"Return",	0,	35, 20,	0,	XK_Return,	XK_Return},
 {51,	"|",	"\\",	20, 20,	0,	XK_backslash,	XK_bar},
 {0,	0,	0,	7, 20},
 {83,"LeftArrow",0,	20, 20,	0,	XK_Left,	XK_Left},
 {88,"DownArrow",0,	20, 20,	0,	XK_Down,	XK_Down},
 {85,"RightArrow",0,	20, 20,	0,	XK_Right,	XK_Right},
 {0,	0,	0,	7, 20},
 {114,	"4",	0,	20, 20,	0,	XK_KP_4,	XK_KP_4},
 {115,	"5",	0,	20, 20,	0,	XK_KP_5,	XK_KP_5},
 {116,	"6",	0,	20, 20,	0,	XK_KP_6,	XK_KP_6},
 {86,	"+",	0,	20, 20,	0,	XK_KP_Add,	XK_KP_Add},
};

static struct key Atari_TT_row5 [] = {
 {50,	"Shift",0,	45, 20,	ShiftMask,	XK_Shift_L,	XK_Shift_L},
#ifdef notdef
/* Non-US Atari keyboards have, instead of a wide left-shift key, */
/* a narrower left-shift and an additional "standard" key */
 {50,	"Shift",0,	25, 20,	ShiftMask,	XK_Shift_L,	XK_Shift_L},
 {104,	"XXX",	0,	20, 20,	0,		XK_xxx},
#endif
 {52,	"Z",	0,	20, 20,	0,		XK_z,		XK_Z},
 {53,	"X",	0,	20, 20,	0,		XK_x,		XK_X},
 {54,	"C",	0,	20, 20,	0,		XK_c,		XK_C},
 {55,	"V",	0,	20, 20,	0,		XK_v,		XK_V},
 {56,	"B",	0,	20, 20,	0,		XK_b,		XK_B},
 {57,	"N",	0,	20, 20,	0,		XK_n,		XK_N},
 {58,	"M",	0,	20, 20,	0,		XK_m,		XK_M},
 {59,	"<",	",",	20, 20,	0,		XK_comma,	XK_less},
 {60,	">",	".",	20, 20,	0,		XK_period,	XK_greater},
 {61,	"?",	"/",	20, 20,	0,		XK_slash,	XK_question},
 {62,"Shift",	0,	30, 20,	ShiftMask,	XK_Shift_R,	XK_Shift_R},
 {0,	0,	0,	109, 20},
 {117,	"1",	0,	20, 20,	0,		XK_KP_1,	XK_KP_1},
 {118,	"2",	0,	20, 20,	0,		XK_KP_2,	XK_KP_2},
 {119,	"3",	0,	20, 20,	0,		XK_KP_3,	XK_KP_3},
 {122,	"Enter",0,	20, 40,	0,		XK_KP_Enter,	XK_KP_Enter},
};
 
static struct key Atari_TT_row6 [] = {
 {0,	0,	0,	23, 20},
 {64,"Alternate",0,	30, 20,		Mod1Mask, XK_Mode_switch,XK_Meta_L},
 {65,	" ",	0,	180, 20,	0,	  XK_space,	 XK_space},
 {66,"CapsLock",0,	30, 20,		LockMask, XK_Caps_Lock,	 XK_Caps_Lock},
 {0,	0,	0,	121, 20},
 {120,	"0",	0,	40, 20,		0,	  XK_KP_0,	 XK_KP_0},
 {121,	".",	0,	20, 20,		0,	  XK_KP_Decimal, XK_KP_Decimal}
};

static struct row Atari_TT_rows [] = {
  { sizeof (Atari_TT_row0) / sizeof (struct key), 10, Atari_TT_row0 },
  { 0, 15, 0 },
  { sizeof (Atari_TT_row2) / sizeof (struct key), 20, Atari_TT_row2 },
  { sizeof (Atari_TT_row3) / sizeof (struct key), 20, Atari_TT_row3 },
  { sizeof (Atari_TT_row4) / sizeof (struct key), 20, Atari_TT_row4 },
  { sizeof (Atari_TT_row5) / sizeof (struct key), 20, Atari_TT_row5 },
  { sizeof (Atari_TT_row6) / sizeof (struct key), 20, Atari_TT_row6 }
};

static struct keyboard Atari_TT = {
  "TT", "Atari TT",
  sizeof (Atari_TT_rows) / sizeof (struct row),
  Atari_TT_rows,
  2, 3, 3
};
