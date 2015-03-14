/* xkeycaps, Copyright (c) 1991, 1992 Jamie Zawinski <jwz@lucid.com>
 *
 * This file describes the DEC LK401 keyboard.
 * By Tom Ivar Helbekkmo <tih@barsoom.nhh.no>
 * This file shares data with the DEC LK201 keyboard description.
 */

#define DEC_LK401_row0 DEC_LK201_row0
#define DEC_LK401_row1 DEC_LK201_row1
#define DEC_LK401_row2 DEC_LK201_row2
#define DEC_LK401_row3 DEC_LK201_row3
#define DEC_LK401_row4 DEC_LK201_row4

static struct key DEC_LK401_row5 [] = {
 {174,	"Shift",0,		16, 7,	ShiftMask,		XK_Shift_L},
 {201,	">",	"<",		7, 7,	0,	XK_less,	XK_greater},
 {195,	"Z",	0,		7, 7,	0,	XK_Z},
 {200,	"X",	0,		7, 7,	0,	XK_X},
 {206,	"C",	0,		7, 7,	0,	XK_C},
 {211,	"V",	0,		7, 7,	0,	XK_V},
 {217,	"B",	0,		7, 7,	0,	XK_B},
 {222,	"N",	0,		7, 7,	0,	XK_N},
 {227,	"M",	0,		7, 7,	0,	XK_M},
 /* these two are the only difference between the LK201 and LK401 */
 {232,	"<",	",",		7, 7,	0,	XK_comma,	XK_less},
 {237,	">",	".",		7, 7,	0,	XK_period,	XK_greater},
 {243,	"?",	"/",		7, 7,	0,	XK_slash,	XK_question},
 /* under WreckWindows, the right-shift key is identical to the left-shift */
 {171,	"Shift",0,		16, 7,		ShiftMask,	XK_Shift_R},
 {0,	0,	0,		10, 7},
 {167,"LeftArrow", 0,		8, 7,	0,	XK_Left},
 {169,"DownArrow", 0,		8, 7,	0,	XK_Down},
 {168,"RightArrow",0,		8, 7,	0,	XK_Right},
 {0,	0,	0,		7, 7,	0,	XK_KP_0},
 {150,	"1",	0,		7, 7,	0,	XK_KP_1},
 {151,	"2",	0,		7, 7,	0,	XK_KP_2},
 {152,	"3",	0,		7, 7,	0,	XK_KP_3},
 {149,"Enter",	0,		7, 14,	0,	XK_KP_Enter}
};

static struct key DEC_LK401_row6 [] = {
 {0,	0,	0,		9, 7},
 {177, "Compose", "Character",	12, 7,	Mod1Mask, XK_Multi_key, XK_Meta_L},
 {172, "Alt",	"Function",	12, 7,	Mod2Mask, XK_Alt_L},
 {212,	" ",	0,		48, 7,	0,	  XK_space},
 {178, "Alt",	"Function",	12, 7,	Mod2Mask, XK_Alt_R},
 {173, "Compose","Character",	12, 7,	Mod1Mask, XK_Multi_key, XK_Meta_R},
 {0,	0,	0,		45, 7},
 {146,	"0",	0,		14, 7,	0,	  XK_KP_0},
 {148,	".",	0,		7, 7,	0,	  XK_KP_Decimal}
};

static struct row DEC_LK401_rows [] = {
  { sizeof (DEC_LK401_row0) / sizeof (struct key), 7, DEC_LK401_row0 },
  { 0, 7, 0 },
  { sizeof (DEC_LK401_row2) / sizeof (struct key), 7, DEC_LK401_row2 },
  { sizeof (DEC_LK401_row3) / sizeof (struct key), 7, DEC_LK401_row3 },
  { sizeof (DEC_LK401_row4) / sizeof (struct key), 7, DEC_LK401_row4 },
  { sizeof (DEC_LK401_row5) / sizeof (struct key), 7, DEC_LK401_row5 },
  { sizeof (DEC_LK401_row6) / sizeof (struct key), 7, DEC_LK401_row6 }
};

static struct keyboard DEC_LK401 = {
  "LK401", "Digital Equipment Corporation LK401",
  sizeof (DEC_LK401_rows) / sizeof (struct row),
  DEC_LK401_rows,
  6, 3, 3
};
