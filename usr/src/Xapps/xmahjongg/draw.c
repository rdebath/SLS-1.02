/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <sys/time.h>
#include "xmahjongg.h"
#include "variables.h"

#define	DRAW_NAME(pp) {							\
	int xpos = X_NAMES;						\
									\
	sprintf((char *)buffer, "%8.8s", pp->name);			\
	draw_string((char *)buffer, xpos, pp->y, XGameTextGC[0]);	\
}

#define	DRAW_BOARD(pp, pos) {						\
	int xpos = X_BOARD+50*pos;					\
									\
	sprintf((char *)buffer, "%5.5d", pp->board[pos]);		\
	draw_string((char *)buffer, xpos, Y_BOARD, XGameTextGC[0]);	\
}

#define DRAW_SCORE(pp, pos) {						\
	int itmp;							\
	int xpos = pp->x+50*pos;					\
	int score = pp->tiles[pos];					\
									\
	if (score < 0) score *= -1;					\
	itmp = (pp->tiles[pos] > 0) ? 0 : 1;				\
	sprintf((char *)buffer, " %3.3d ", score);			\
	draw_string((char *)buffer, xpos, pp->y, XGameTextGC[itmp]);	\
}

#define DRAW_TOTAL(pp, pos) {						\
	int itmp;							\
	int xpos = X_BOARD+50*pos;					\
									\
	sprintf((char *)buffer, "TOTAL");				\
	draw_string((char *)buffer, xpos, Y_BOARD, XGameTextGC[0]);	\
	xpos = pp->x + 50*pos;						\
	itmp = (pp->quit == 0) ? 0 : 1;					\
	sprintf((char *)buffer, " %3.3d ", pp->total);			\
	draw_string((char *)buffer, xpos, pp->y, XGameTextGC[itmp]);	\
}

char	copy[]   = "(c) 1990 by Jeff Young. ";
char	rights[] = "All rights reserved.    ";

draw_user(pp, type)
Player *pp;
int type;
{
	int i;
	char buffer[20];
	GC userGC;

	if (type == GAME_START) {
		DRAW_NAME(pp);

		for (i = 0; i < MAX_BOARDS; i++) {
			if (pp->tiles[i] > TILES) break;
			DRAW_BOARD(pp, i);
			DRAW_SCORE(pp, i);
		};

		DRAW_TOTAL(pp, num_games);
	} else if (type == GAME_PLAY) {
		DRAW_BOARD(pp, pp->done);
		DRAW_SCORE(pp, pp->done);
		DRAW_TOTAL(pp, num_games);
	} else if (type == GAME_DONE) {
		DRAW_BOARD(pp, pp->done);
		DRAW_SCORE(pp, pp->done);
	} else if (type == GAME_QUIT) {
		DRAW_BOARD(pp, pp->done);
		DRAW_SCORE(pp, pp->done);
		DRAW_TOTAL(pp, num_games);
	} else {
		fprintf(stderr, "bad draw_user type (%d)\n", type);
		exit(1);
	};

	return(0);
}

draw_data()
{
	int i, j, x, y;
	char string[2];

/*
 *	Display the tiles on the screen before the start of the game.  Order:
 *	dragons, seasons, plants, directions, circles, bamboos, ideographs.
 *	The 'if' statement below is just a way to figure out the x and y 
 *	coordinates of the tiles.
 *
 */
	for (i = 10; i <= 51; i++) {
		if ((10 <= i) && (i <= 12)) {
			j = 6;
			x = 2*(i%9);
			y = 198;
		} else if ((13 <= i) && (i <= 16)) {
			j = 5;
			x = (2*i + 7)%8;
			y = 278 + 80*((i-13)/4);
		} else if ((17 <= i) && (i <= 20)) {
			j = 4;
			x = (2*i + 7)%8;
			y = 278 + 80*((i-13)/4);
		} else if ((21 <= i) && (i <= 24)) {
			j = 3;
			x = (2*i + 7)%8;
			y = 278 + 80*((i-13)/4);
		} else if ((25 <= i) && (i <= 33)) {
			j = 1;
			x = (i+2)%9;
			y = 278 + 80*((i+2)/9);
		} else if ((34 <= i) && (i <= 42)) {
			j = 0;
			x = (i+2)%9;
			y = 278 + 80*((i+2)/9);
		} else if ((43 <= i) && (i <= 51)) {
			j = 2;
			x = (i+2)%9;
			y = 278 + 80*((i+2)/9);
		};

		x = 42 + 106*x;
		string[0] = tile_data[i];
		XDrawImageString(XGameDisplay, XGameWindow,
			XGameTileGC[j][0], x, y, string, 1);
	};

	return(0);
}

draw_tiles()
{
	int i, j, k;
	char string[2];
	Tile *tp;

	XClearWindow(XGameDisplay, XGamePlayWindow);

	XCopyArea(XGameDisplay, XGamePlayWindow, XGamePixmap, XGameTextGC[0],
		0, 0, PLAY_WIDTH, PLAY_HEIGHT, 0, 0);

	for (i = 0, tp = order[0]; i < TILES; i++, tp = order[i]) {
		if (tp == NULL) break;
		if (tp->state != USED) continue;
		string[0] = tp->data;
		j = (tp != tile1p) ? 0 : 1;
		k = (color_type == 0) ? tp->lev : (tp->data/16) - 1;
		XDrawImageString(XGameDisplay, XGamePixmap,
			XGameTileGC[k][j], tp->x, tp->y, string, 1);

		draw_border(tp->x, tp->y);
	};

	XCopyArea(XGameDisplay, XGamePixmap, XGamePlayWindow, XGameTextGC[0],
		0, 0, PLAY_WIDTH, PLAY_HEIGHT, 0, 0);

	XFlush(XGameDisplay);

	return(0);
}

draw_count()
{
	char string[4];
	int count = (done_count != 0) ? done_count : tiles_remaining;

	string[0] = count/100;
	string[1] = (count/10)%10;
	string[2] = count%10;

	XDrawImageString(XGameDisplay, XGameCountWindow, XGameOtherGC[0],
		0, 0, string, 3);

	XFlush(XGameDisplay);

	return(0);
}

draw_border(x, y)
int	x;
int	y;
{
	int i;

	for (i = 0; i < 12; i++) {
		XGamePoint[i].x += x;
		XGamePoint[i].y += y;
	};

	XDrawLines(XGameDisplay, XGamePixmap, XGameBorderGC, XGamePoint,
		12, CoordModeOrigin);

	for (i = 0; i < 12; i++) {
		XGamePoint[i].x -= x;
		XGamePoint[i].y -= y;
	};

	return(0);
}

draw_letter(index, x, y)
int	index;
int	x;
int	y;
{
	char string[2];

	string[0] = index;
	string[1] = index+16;

	XDrawImageString(XGameDisplay, XGameWindow, XGameOtherGC[0],
		x, y+00, &string[0], 1);
	XDrawImageString(XGameDisplay, XGameWindow, XGameOtherGC[0],
		x, y+64, &string[1], 1);

	XFlush(XGameDisplay);

	return(0);
}

draw_window()
{
	int i, k, x, y;
	char string[64];

	k = XGameTextFont->ascent + XGameTextFont->descent + 1;
	x = 55;
	y = (Y_COUNT-4*k)/2;

/*
 *	Print out the strings in the upper window
 */
	XClearWindow(XGameDisplay, XGameWindow);

	sprintf(string, "Board number: %5d", seed);
	draw_string(copy,   x, y+0*k, XGameTextGC[1]);
	draw_string(rights, x, y+1*k, XGameTextGC[1]);
	draw_string(string, x, y+3*k, XGameTextGC[1]);

	draw_matches();

/*
 *	If in tournament mode, then reprint the current scores
 */
	if (tourn_flag != 0) {
		for (i = 0, pp = player; i < num_players; i++, pp++) {
			pp->y = Y_SCORE+i*k;
			draw_user(pp, GAME_START);
		};
	};

	XFlush(XGameDisplay);

	return(0);
}

draw_option(index, window)
int	index;
Window	window;
{
	int i;
	char string[2];


	string[0] = index;
	i = ((window == XGameDoneWindow) && (done_count != 0)) ? 1 : 0;
	XDrawImageString(XGameDisplay, window, XGameOtherGC[i],
		0, 0, string, 1);
	XFlush(XGameDisplay);

	return(0);
}

draw_string(data, x, y, imageGC)
char *data;
int x, y;
GC imageGC;
{

	XDrawImageString(XGameDisplay, XGameWindow, imageGC,
		x, y, data, strlen(data));

	XFlush(XGameDisplay);

	return(0);
}

draw_matches() {
	int i, j, k;
	int matches = 0;
	char string[80];
	Tile *t1p, *t2p;

	if (done_count == 0) return(0);

/*
 *	Count the number of matches remaining.
 */
	for(i = 0, t1p = &tiles[0][0][0]; i < ROWS*COLS*LEVS-1; i++, t1p++) {
		if (t1p->state != USED) continue;
		if (not_free(t1p->row, t1p->col, t1p->lev)) continue;

		for(j = i+1, t2p = t1p+1; j < ROWS*COLS*LEVS; j++, t2p++) {
			if (t2p->state != USED) continue;
			if (not_free(t2p->row, t2p->col, t2p->lev)) continue;
			if (t1p->type == t2p->type) matches++;
		};
	};

/*
 *	Display the number of matches.
 */
	i = XGameTextFont->ascent;
	j = XGameTextFont->descent;
	k = (184-Y_DONE-OPTION_HEIGHT-i-j)/2;
	sprintf(string, " Matches remaining = %3d ", matches);
	draw_string(string, 800, Y_DONE+OPTION_HEIGHT+k+i, XGameTextGC[1]);
	XFlush(XGameDisplay);

	return(0);
}
