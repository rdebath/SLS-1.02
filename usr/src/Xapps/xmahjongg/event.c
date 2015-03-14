/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "xmahjongg.h"
#include "variables.h"

event_process() {
	int readfds;

/*
 *	Wait for an event and then process it.
 */
	event_wait();

next_event:
	XNextEvent(XGameDisplay, &XGameEvent);
	switch (XGameEvent.type) {
		case Expose:
			if (XGameEvent.xexpose.count != 0) goto next_event;
			event_expose();
			break;
		case ButtonPress:
			event_button();
			break;
		default:
			break;
	};

	return(0);
}

event_packet(readfds)
int readfds;
{
	int i;

	for (i = 0, pp = player; i < num_players; i++, pp++) {
		if (pp->type == 'M') continue;
		if ((readfds & (1 << pp->fd)) != 0) {
			packet_recv(pp->fd);
		};
	};

	return(0);
}

event_expose() {

	if (XGameEvent.xexpose.window == XGameWindow) {
		draw_window();
	} else if (XGameEvent.xexpose.window == XGameNewWindow) {
		draw_option(INDEX_NEW, XGameNewWindow);
	} else if (XGameEvent.xexpose.window == XGameDoneWindow) {
		draw_option(INDEX_DONE, XGameDoneWindow);
	} else if (XGameEvent.xexpose.window == XGameQuitWindow) {
		draw_option(INDEX_QUIT, XGameQuitWindow);
	} else if (XGameEvent.xexpose.window == XGameSameWindow) {
		draw_option(INDEX_SAME, XGameSameWindow);
	} else if (XGameEvent.xexpose.window == XGamePlayWindow) {
		draw_tiles();
	} else if (XGameEvent.xexpose.window == XGameCountWindow) {
		draw_count();
	};

	return(0);
}

event_button() {
	int i, j, k;

	if (XGameEvent.xbutton.window == XGameNewWindow) {
		event_new();
	} else if (XGameEvent.xbutton.window == XGameDoneWindow) {
		event_done();
	} else if (XGameEvent.xbutton.window == XGameSameWindow) {
		event_same();
	} else if (XGameEvent.xbutton.window == XGameQuitWindow) {
		event_quit();
	} else if (XGameEvent.xbutton.window == XGamePlayWindow) {
		event_play();
	};

	return(0);
}

event_new() {
	int i, j, k;

/*
 *	If this is a tournament, the the user can't go to a new board
 *	unless this is not the last game in the series.
 */
	if ((tourn_flag != 0) && (mypp->done == num_games-1)) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
		return(0);
	};

/*
 *	If this is setup mode, then clear all of the tiles.
 */
	if (setup_flag != 0) {
		for (i = 0; i < TILES; i++) {
			order[i] = NULL;
		};

		for (i = 0; i < ROWS; i++) {
			for (j = 0; j < COLS; j++) {
				for (k = 0; k < LEVS; k++) {
					tiles[i][j][k].state = FREE;
				};
			};
		};
	};

/*
 *	Clear all the windows.
 */
	keep_playing = 0;
	packet_send(GAME_DONE);
	XClearArea(XGameDisplay, XGameWindow, 0, 0, 0, 0, True);
	XClearArea(XGameDisplay, XGameNewWindow, 0, 0, 0, 0, True);
	XClearArea(XGameDisplay, XGameDoneWindow, 0, 0, 0, 0, True);
	XClearArea(XGameDisplay, XGameSameWindow, 0, 0, 0, 0, True);
	XClearArea(XGameDisplay, XGameQuitWindow, 0, 0, 0, 0, True);
	XClearArea(XGameDisplay, XGamePlayWindow, 0, 0, 0, 0, True);
	XClearArea(XGameDisplay, XGameCountWindow, 0, 0, 0, 0, True);
	XFlush(XGameDisplay);

	return(0);
}

event_same() {

/*
 *	If this is a tournament, then the user can't play the same board
 *	twice.  If this is setup mode, then do nothing;
 */
	if ((num_games != 0) || (setup_flag != 0)) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
	} else {
		seed = -seed;
		keep_playing = 0;
		XClearArea(XGameDisplay, XGameWindow, 0, 0, 0, 0, True);
		XClearArea(XGameDisplay, XGameNewWindow, 0, 0, 0, 0, True);
		XClearArea(XGameDisplay, XGameDoneWindow, 0, 0, 0, 0, True);
		XClearArea(XGameDisplay, XGameSameWindow, 0, 0, 0, 0, True);
		XClearArea(XGameDisplay, XGameQuitWindow, 0, 0, 0, 0, True);
		XClearArea(XGameDisplay, XGamePlayWindow, 0, 0, 0, 0, True);
		XClearArea(XGameDisplay, XGameCountWindow, 0, 0, 0, 0, True);
		XFlush(XGameDisplay);
	};

	return(0);
}

event_done() {
	int i;
	FILE *stdconf;

/*
 *	If this is not setup mode, then mark the game as done and display
 *	the matches remaining.
 */
	if (setup_flag == 0) {
		if (done_count == 0) {
			done_count = tiles_remaining;
			packet_send(GAME_DONE);
		};

		draw_option(INDEX_DONE, XGameDoneWindow);
		draw_matches();
		return(0);
	};

/*
 *	For setup mode, we must check to be sure that all the tiles have
 *	been placed.  If they have, then write out the configuration file.
 */
	if (tiles_remaining != 0) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
		return(0);
	};

/*
 *	Open the configuration file.
 */
	if (layout != NULL) {
		if ((stdconf = fopen(layout, "w")) == NULL) {
			fprintf(stderr, "can't open layout file\n");
			exit(1);
		};
	} else {
		stdconf = stdout;
	};

/*
 *	Dump out the configuration.
 */
	for (i = 0; i < TILES; i++) {
		fprintf(stdconf, "%3d  %3d  %3d\n",
		order[i]->row, order[i]->col, order[i]->lev);
	};

	fclose(stdconf);
	event_quit();

	return(0);
}

event_quit() {
	int i;

	packet_send(GAME_QUIT);

/*
 *	Free all of the X resources which we grabbed.
 */
	XUnloadFont(XGameDisplay, XGameTextFont->fid);
	XUnloadFont(XGameDisplay, XGameTileFont->fid);

	XFreeGC(XGameDisplay, XGameBorderGC);
	XFreeGC(XGameDisplay, XGameTextGC[0]);
	XFreeGC(XGameDisplay, XGameTextGC[1]);
	XFreeGC(XGameDisplay, XGameOtherGC[0]);
	XFreeGC(XGameDisplay, XGameOtherGC[1]);

	for (i = 0; i < LEVS; i++) {
		XFreeGC(XGameDisplay, XGameTileGC[i][0]);
		XFreeGC(XGameDisplay, XGameTileGC[i][1]);
	};

	XFreeCursor(XGameDisplay, XGameCursor);
	XFreePixmap(XGameDisplay, XGamePixmap);

	XCloseDisplay(XGameDisplay);

	exit(0);
}

event_play() {
	int i, j, k;
	int x, y;

	x_coor = XGameEvent.xbutton.x;
	y_coor = XGameEvent.xbutton.y;

/*
 *	If we are in setup mode, then call the routine which will create a 
 *	new tile or delete one from the board.
 */
	if (setup_flag != 0) {
		if (XGameEvent.xbutton.button == Button1) {
			event_create();
		} else {
			event_delete();
		};

		return(0);
	};

/*
 *	We are in play mode.  Check the make sure the button was pushed on a 
 *	tile.
 */
	for (k = LEVS-1; k >= 0; k--) {
		x = (y_coor - Y_TILE + (4*k))/(TILE_SIDE/2);
		y = (x_coor - X_TILE - (4*k))/(TILE_SIDE/2);

		for (i = 0; i < 2; i++) {
			for (j = 0; j < 2; j++) {
				if (in_bounds(x-i, y-j)) {
					if (tiles[x-i][y-j][k].state == USED) {
						play_tile(&tiles[x-i][y-j][k]);
						return(0);
					};
				};
			};
		};
	};

/*
 *	The button was pushed in a bad area.  We beep the bell and continue.
 */
	XBell(XGameDisplay, 100);
	XFlush(XGameDisplay);

	return(0);
}

event_create() {
	int i, j, k;
	int x, y;

	if (tiles_remaining == 0) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
		return(0);
	} else if (x_coor < X_TILE) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
		return(0);
	} else if (y_coor < Y_TILE) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
		return(0);
	};

/*
 *	Check the make sure the button was pushed on a tile.
 */
	for (k = 0; k < LEVS; k++) {
		x = (y_coor - Y_TILE + (4*k))/(TILE_SIDE/2);
		y = (x_coor - X_TILE - (4*k))/(TILE_SIDE/2);

		for (i = 0; i < 2; i++) {
			for (j = 0; j < 2; j++) {
				if (in_bounds(x-i, y-j) == 0) continue;
				if (ok_below(x-i, y-j, k) != 0) continue;
				if (tiles[x-i][y-j][k].state == USED) continue;
				create_tile(&tiles[x-i][y-j][k]);
				return(0);
			};
		};
	};

/*
 *	The button was pushed in a bad area.  We beep the bell and continue.
 */
	XBell(XGameDisplay, 100);
	XFlush(XGameDisplay);

	return(0);
}

event_delete() {
	int i, j, k;
	int x, y;

	if (tiles_remaining == TILES) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
		return(0);
	} else if (x_coor < X_TILE) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
		return(0);
	} else if (y_coor < Y_TILE) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
		return(0);
	};

/*
 *	Check the make sure the button was pushed on a tile.
 */
	for (k = LEVS-1; k >= 0; k--) {
		x = (y_coor - Y_TILE + (4*k))/(TILE_SIDE/2);
		y = (x_coor - X_TILE - (4*k))/(TILE_SIDE/2);

		for (i = 0; i < 2; i++) {
			for (j = 0; j < 2; j++) {
				if (in_bounds(x-i, y-j) == 0) continue;
				if (ok_above(x-i, y-j, k) != 0) continue;
				if (tiles[x-i][y-j][k].state == FREE) continue;
				delete_tile(&tiles[x-i][y-j][k]);
				return(0);
			};
		};
	};

/*
 *	The button was pushed in a bad area.  We beep the bell and continue.
 */
	XBell(XGameDisplay, 100);
	XFlush(XGameDisplay);

	return(0);
}
