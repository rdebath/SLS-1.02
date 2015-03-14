/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#ifdef	GLOBAL
#undef	GLOBAL
#endif
#define	GLOBAL 

extern	int	optind;
extern	int	opterr;
extern	char	*optarg;

GLOBAL	int	maxfds;
GLOBAL	int	playfds;
GLOBAL	int	num_games;
GLOBAL	int	done_count;
GLOBAL	int	setup_flag;
GLOBAL	int	tourn_flag;
GLOBAL	int	color_type;
GLOBAL	int	num_players;
GLOBAL	int	iconic_start;
GLOBAL	int	keep_playing;
GLOBAL	int	reverse_video;
GLOBAL	int	tiles_remaining;
GLOBAL	int	tile_data[TILES];
GLOBAL	long	seed;
GLOBAL	long	buffer[256];
GLOBAL	char	*layout;
GLOBAL	char	*tile_font;
GLOBAL	char	*display_name;
GLOBAL	Tile	*flink;
GLOBAL	Tile	*blink;
GLOBAL	Tile	*tile1p;
GLOBAL	Tile	*tile2p;
GLOBAL	Tile	*order[TILES];
GLOBAL	Tile	tiles[ROWS][COLS][LEVS];
GLOBAL	Ulong	fore_colors[LEVS];
GLOBAL	Ulong	back_colors[LEVS];
GLOBAL	Player	player[MAX_PLAYERS];
GLOBAL	Player	*mypp;
GLOBAL	Player	*pp;
GLOBAL	Packet	packet;

GLOBAL	int	x_coor;
GLOBAL	int	y_coor;
GLOBAL	int	BorderColor;

GLOBAL	int		XGameFD;
GLOBAL	int		XGameDepth;
GLOBAL	int		XGameScreen;
GLOBAL	GC		XGameBorderGC;
GLOBAL	GC		XGameTextGC[2];
GLOBAL	GC		XGameOtherGC[2];
GLOBAL	GC		XGameTileGC[LEVS][2];
GLOBAL	Ulong		XGameGrey;
GLOBAL	Ulong		XGameBlack;
GLOBAL	Ulong		XGameWhite;
GLOBAL	Cursor		XGameCursor;
GLOBAL	Pixmap		XGameIcon;
GLOBAL	Pixmap		XGamePixmap;
GLOBAL	Window		XGameWindow;
GLOBAL	Window		XGameNewWindow;
GLOBAL	Window		XGameDoneWindow;
GLOBAL	Window		XGameQuitWindow;
GLOBAL	Window		XGameSameWindow;
GLOBAL	Window		XGamePlayWindow;
GLOBAL	Window		XGameCountWindow;
GLOBAL	XEvent		XGameEvent;
GLOBAL	XPoint		XGamePoint[12];
GLOBAL	Colormap 	XGameColormap;
GLOBAL	XSizeHints	XGameHints;
GLOBAL	Display		*XGameDisplay;
GLOBAL	Visual		*XGameVisual;
GLOBAL	XFontStruct 	*XGameTextFont;
GLOBAL	XFontStruct 	*XGameTileFont;
