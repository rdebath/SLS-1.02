/*
 * Copyright 1990 Jeff Sparkes.
 *
 *	All rights reserved.
 */
typedef struct {
	Pixel	foreground;
	Pixel	background;
	Pixel	normal;
	Pixel	select;
	Pixel	bold;
	Pixel	colorbg;
	Boolean mono;
	XFontStruct	*fontstruct;
	char	*model;
	char	*keymap;
} AppRes, *AppResptr;

extern Display		*display;
extern Widget		toplevel;
extern Font		ibmfont;
extern XFontStruct      *ibmfontinfo;
extern int		ROWS, COLS;
extern int		model_num;
extern XtActionsRec	actions[];
extern int		actioncount;
extern AppRes		appres;
