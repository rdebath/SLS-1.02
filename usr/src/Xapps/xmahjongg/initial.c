/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#include <netdb.h>
#include <stdio.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "xmahjongg.h"
#include "variables.h"
#include "icon.h"

int	initial_flag = 0;
char copyright[] = " Copyright (c) 1990 by Jeff Young (exceptions).  All rights reserved. ";

initialize() {

	initialize_conf();
	initialize_data();
	initialize_pool();
	initialize_tiles();
	initialize_socket();
	initialize_window();

	return(0);
}

initialize_conf() {
	int i, j, k;
	int cnt, row, col, lev;
	char line[128], file[128];
	FILE *stdconf;

	if (initial_flag != 0) return(0);
	if (setup_flag != 0) return(0);

/*
 *	Set all tiles to FREE before reading in the configuration.
 */
	for (i = 0; i < ROWS; i++) {
		for (j = 0; j < COLS; j++) {
			for (k = 0; k < LEVS; k++) {
				tiles[i][j][k].init = FREE;
				tiles[i][j][k].state = FREE;
			};
		};
	};

/*
 *	Open the file containing the specified layout.
 */
	cnt = 0;
	if (layout == NULL) {
		sprintf(file, "%s/default", LAYOUT);
	} else if ((layout[0] != '/') && (layout[0] != '.')) {
		sprintf(file, "%s/%s", LAYOUT, layout);
	} else {
		strcpy(file, layout);
	};

	if ((stdconf = fopen(file, "r")) == NULL) {
		fprintf(stderr, "can't open layout file \n");
		exit(1);
	};

/*
 *	Read in the row, col, lev lines
 */
	while (fgets(line, 127, stdconf) != NULL) {
		if (line[0] == '#') continue;

		sscanf(line, "%d %d %d", &row, &col, &lev);
		tiles[row][col][lev].init = USED;
		tiles[row][col][lev].state = USED;
		order[cnt++] = &tiles[row][col][lev];
	};

	fclose(stdconf);

/*
 *	Do a rudimentary check on the number of tiles read
 */
	if (cnt != TILES) {
		fprintf(stderr, "invalid layout file (%d)\n", cnt);
		exit(1);
	};

	return(0);
}

initialize_data() {
	int i, j, k, l;
	int dx, dy, dz;
	Tile *tp, *temp;

	done_count = 0;
	tiles_remaining = TILES;
	if (initial_flag != 0) return(0);

/*
 *	Initialize the bitmap pointers for shuffling into the pool.
 */
	tile_data[ 0] = INDEX_NUMBER + 0;
	tile_data[ 1] = INDEX_NUMBER + 1;
	tile_data[ 2] = INDEX_NUMBER + 2;
	tile_data[ 3] = INDEX_NUMBER + 3;
	tile_data[ 4] = INDEX_NUMBER + 4;
	tile_data[ 5] = INDEX_NUMBER + 5;
	tile_data[ 6] = INDEX_NUMBER + 6;
	tile_data[ 7] = INDEX_NUMBER + 7;
	tile_data[ 8] = INDEX_NUMBER + 8;
	tile_data[ 9] = INDEX_NUMBER + 9;
	tile_data[10] = INDEX_DRAGON + 0;
	tile_data[11] = INDEX_DRAGON + 1;
	tile_data[12] = INDEX_DRAGON + 2;
	tile_data[13] = INDEX_SEASON + 0;
	tile_data[14] = INDEX_SEASON + 1;
	tile_data[15] = INDEX_SEASON + 2;
	tile_data[16] = INDEX_SEASON + 3;
	tile_data[17] = INDEX_PLANT + 0;
	tile_data[18] = INDEX_PLANT + 1;
	tile_data[19] = INDEX_PLANT + 2;
	tile_data[20] = INDEX_PLANT + 3;
	tile_data[21] = INDEX_DIRECTION + 0;
	tile_data[22] = INDEX_DIRECTION + 1;
	tile_data[23] = INDEX_DIRECTION + 2;
	tile_data[24] = INDEX_DIRECTION + 3;
	tile_data[25] = INDEX_CIRCLE + 0;
	tile_data[26] = INDEX_CIRCLE + 1;
	tile_data[27] = INDEX_CIRCLE + 2;
	tile_data[28] = INDEX_CIRCLE + 3;
	tile_data[29] = INDEX_CIRCLE + 4;
	tile_data[30] = INDEX_CIRCLE + 5;
	tile_data[31] = INDEX_CIRCLE + 6;
	tile_data[32] = INDEX_CIRCLE + 7;
	tile_data[33] = INDEX_CIRCLE + 8;
	tile_data[34] = INDEX_BAMBOO + 0;
	tile_data[35] = INDEX_BAMBOO + 1;
	tile_data[36] = INDEX_BAMBOO + 2;
	tile_data[37] = INDEX_BAMBOO + 3;
	tile_data[38] = INDEX_BAMBOO + 4;
	tile_data[39] = INDEX_BAMBOO + 5;
	tile_data[40] = INDEX_BAMBOO + 6;
	tile_data[41] = INDEX_BAMBOO + 7;
	tile_data[42] = INDEX_BAMBOO + 8;
	tile_data[43] = INDEX_IDEOGRAPH + 0;
	tile_data[44] = INDEX_IDEOGRAPH + 1;
	tile_data[45] = INDEX_IDEOGRAPH + 2;
	tile_data[46] = INDEX_IDEOGRAPH + 3;
	tile_data[47] = INDEX_IDEOGRAPH + 4;
	tile_data[48] = INDEX_IDEOGRAPH + 5;
	tile_data[49] = INDEX_IDEOGRAPH + 6;
	tile_data[50] = INDEX_IDEOGRAPH + 7;
	tile_data[51] = INDEX_IDEOGRAPH + 8;

/*
 *	Initialize the coordinates for all the tiles.
 */
	l = TILE_SIDE/2;
	for (i = 0; i < ROWS; i++) {
		for (j = 0; j < COLS; j++) {
			for (k = 0; k < LEVS; k++) {
				tiles[i][j][k].row = i;
				tiles[i][j][k].col = j;
				tiles[i][j][k].lev = k;
				tiles[i][j][k].x = X_TILE+(j*l)+(4*k);
				tiles[i][j][k].y = Y_TILE+(i*l)-(4*k);
				if (setup_flag != 0) {
					tiles[i][j][k].state = FREE;
				};
			};
		};
	};

/*
 *	Put the randomized images into the tiles and sort them according
 *	to the drawing order.
 */
	if (setup_flag == 0) {
		for (i = 0; i < TILES-1; i++) {
			for (j = i+1; j < TILES; j++) {
				dx = order[i]->row - order[j]->row;
				dy = order[i]->col - order[j]->col;
				dz = order[i]->lev - order[j]->lev;

				if (dz < 0) continue;

				if (dz > 0) {
					temp = order[i];
					order[i] = order[j];
					order[j] = temp;
				} else if (dy < dx) {
					temp = order[i];
					order[i] = order[j];
					order[j] = temp;
				} else if ((dy == dx) && (dy > 0)) {
					temp = order[i];
					order[i] = order[j];
					order[j] = temp;
				};
			};

			order[i]->value = i;
		};

		order[TILES-1]->value = TILES-1;
	};

	return(0);
}

initialize_pool() {
	int i, j, k;
	int seed1, seed2;
	long save;

	if (setup_flag != 0) return(0);

/*
 *	Set up the randomness for the game
 */
	get_seed();
	random_init(seed);

/*
 *	Place the circle, bamboo, and ideograph tiles in the pool
 */
	for (j = 0, k = 0; j < 4; j++) {
		for (i = 21; i <= 51; i++) {
			buffer[k++] = i;
		};
	};

/*
 *	Place the dragon tiles in the pool
 */
	for (j = 0; j < 4; j++) {
		for (i = 10; i <= 12; i++) {
			buffer[k++] = i;
		};
	};

/*
 *	Place the season and plant tiles in the pool
 */
	for (i = 13; i <= 20; i++) {
		buffer[k++] = i;
	};

/* 
 *	Shuffle the pool of tiles
 */
	for (i = 0; i < 16384; i++) {
		seed1 = (random_next() & 0xfffffff)%TILES;
		seed2 = (random_next() & 0xfffffff)%TILES;
		save = buffer[seed1];
		buffer[seed1] = buffer[seed2];
		buffer[seed2] = save;
	};

	return(0);
}

initialize_tiles() {
	int i;
	int s1_tile = tile_data[13];
	int s2_tile = tile_data[16];
	int p1_tile = tile_data[17];
	int p2_tile = tile_data[20];
	Tile *tp;

	if (setup_flag != 0) return(0);

/*
 *	Put the randomized images into the tiles and sort them according
 *	to the drawing order.
 */
	for (i = 0, tp = order[0]; i < TILES; i++, tp = order[i]) {
		tp->state = tp->init;
		tp->data = tile_data[get_tile()];
		if ((s1_tile <= tp->data) && (tp->data <= s2_tile)) {
			tp->type = s1_tile;
		} else if ((p1_tile <= tp->data) && (tp->data <= p2_tile)) {
			tp->type = p1_tile;
		} else {
			tp->type = tp->data;
		};

	};

/*
 *	Set the tile pointers to NULL for starters.
 */
	tile1p = NULL;
	tile2p = NULL;

	return(0);
}

#ifdef NO_TCP
initialize_socket() 
{
  return;
}
#else
initialize_socket() {
	int i, s;
	int sizeon;
	int namelen;
	int on = 1;
	struct sockaddr_in name;
	struct hostent *hp, *gethostbyname();

	if (setup_flag != 0) return(0);
	if (tourn_flag != 1) return(0);
	printf("attempting connections\n");

/*
 *	Attempt connects
 */
	i = 0;
	for (pp = &player[num_players-1]; pp >= player; pp--, i++) {
		if (pp->type != 'C') continue;

		namelen = sizeof(name);
		name.sin_family = AF_INET;
		name.sin_port = htons(pp->port);
		hp = gethostbyname(pp->machine);
		bcopy(hp->h_addr, (char *)&name.sin_addr, hp->h_length);

		while (1) {
			if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
				sleep(1);
			} else if (connect(s, (char *)&name, namelen) < 0) {
				close(s);
				sleep(1);
			} else {
				pp->fd = s;
				break;
			};
		};
	};

/*
 *	Attempt accepts
 */
	name.sin_family = AF_INET;
	name.sin_port = htons(mypp->port);
	name.sin_addr.s_addr = 0;
	namelen = sizeof(name);
	sizeon = sizeof(on);

	if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		perror("can't open socket");
		exit(1);
	} else if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &on, sizeon) < 0) {
		perror("can't reset socket");
		exit(1);
	} else if (bind(s, (char *)&name, namelen) < 0) {
		perror("can't bind socket");
		exit(1);
	} else if (listen(s, 5) < 0) {
		perror("can't listen socket");
		exit(1);
	};

	for (i = 0, pp = player; i < num_players; i++, pp++) {
		if (pp->type != 'A') continue;
		namelen = sizeof(name);

		if ((pp->fd = accept(s, (char *)&name, &namelen)) < 0) {
			sleep(1);
		};
	};

	for (i = 0, pp = player; i < num_players; i++, pp++) {
		if (pp->type != 'M') playfds |= (1 << pp->fd);
	};

	tourn_flag = 2;
	printf("connections established\n");

	return(0);
}
#endif

initialize_window() {
	int color[9];
	int i, j, k, x, y;
	int init_x, init_y;
	int other_fore_color;
	int other_back_color;
	int window_fore_color;
	int window_back_color;
	int border_fore_color;
	int border_back_color;
	XWMHints XGameWMHints;

/*
 *	Only initialize the window if the initial_flag is zero
 */
	if (initial_flag != 0) return(0);

/*
 *	Open up the display.
 */
	if ((XGameDisplay = XOpenDisplay(display_name)) == NULL) {
		fprintf(stderr, "can't connect to display\n");
		exit(1);
	};

/*
 *	Set some default variables from the server.
 */
	XGameScreen = DefaultScreen(XGameDisplay);
	XGameColormap = DefaultColormap(XGameDisplay, XGameScreen);
	XGameVisual = DefaultVisual(XGameDisplay, XGameScreen);
	XGameDepth = DefaultDepth(XGameDisplay, XGameScreen);
	XGameFD = ConnectionNumber(XGameDisplay);

/*
 *	Get the colors that we need.
 */
	if (XGameDepth > 1) {
		XGameBlack = setup_color(0x0000, 0x0000, 0x0000);
		XGameWhite = setup_color(0xffff, 0xffff, 0xffff);
		XGameGrey  = setup_color(0x7fff, 0x7fff, 0x7fff);

		color[0] = setup_color(0x0000, 0x8fff, 0xffff);
		color[1] = setup_color(0x0000, 0xffff, 0xffff);
		color[2] = setup_color(0x0000, 0xffff, 0x0000);
		color[3] = setup_color(0xffff, 0xffff, 0x0000);
		color[4] = setup_color(0xffff, 0x7fff, 0x0000);
		color[5] = setup_color(0xffff, 0x0000, 0x0000);
		color[6] = setup_color(0xffff, 0x0000, 0xafff);

		for (i = 0; i < LEVS; i++) {
			fore_colors[i] = XGameBlack;
			back_colors[i] = color[i];
		};

		if (reverse_video == 0) {
			other_fore_color = XGameBlack;
			other_back_color = XGameWhite;
			border_fore_color = XGameGrey;
			border_back_color = XGameGrey;
			window_fore_color = XGameBlack;
			window_back_color = XGameWhite;
		} else {
			other_fore_color = XGameWhite;
			other_back_color = XGameBlack;
			border_fore_color = XGameGrey;
			border_back_color = XGameGrey;
			window_fore_color = XGameWhite;
			window_back_color = XGameBlack;
		};
	} else {
		XGameBlack  = BlackPixel(XGameDisplay, XGameScreen);
		XGameWhite  = WhitePixel(XGameDisplay, XGameScreen);

		if (reverse_video == 0) {
			for (i = 0; i < LEVS; i++) {
				fore_colors[i] = XGameBlack;
				back_colors[i] = XGameWhite;
			};

			other_fore_color = XGameBlack;
			other_back_color = XGameWhite;
			border_fore_color = XGameWhite;
			border_back_color = XGameBlack;
			window_fore_color = XGameBlack;
			window_back_color = XGameWhite;
		} else {
			for (i = 0; i < LEVS; i++) {
				fore_colors[i] = XGameWhite;
				back_colors[i] = XGameBlack;
			};

			other_fore_color = XGameWhite;
			other_back_color = XGameBlack;
			border_fore_color = XGameBlack;
			border_back_color = XGameWhite;
			window_fore_color = XGameWhite;
			window_back_color = XGameBlack;
		};

		color_type = 0;
	};

/*
 *	Get the font for text printing and tile printing.
 */
	if ((XGameTextFont = XLoadQueryFont(XGameDisplay, TEXT_FONT)) == NULL) {
		fprintf(stderr, "can't load text font %s\n", TEXT_FONT);
		exit(1);
	};

	if ((XGameTileFont = XLoadQueryFont(XGameDisplay, tile_font)) == NULL) {
		fprintf(stderr, "can't load tile font %s\n", tile_font);
		exit(1);
	};

/*
 *	Define an icon for the game
 */
	XGameIcon = XCreateBitmapFromData(
		XGameDisplay, DefaultRootWindow(XGameDisplay),
		icon_tiles_bits, ICON_WIDTH, ICON_HEIGHT);

/*
 *	Open the main window.
 */
	init_x = (DisplayWidth(XGameDisplay, XGameScreen)-WINDOW_WIDTH)/2;
	init_y = (DisplayHeight(XGameDisplay, XGameScreen)-WINDOW_HEIGHT)/2;

	XGameWindow = XCreateSimpleWindow(
		XGameDisplay, DefaultRootWindow(XGameDisplay),
		init_x, init_y, WINDOW_WIDTH, WINDOW_HEIGHT,
		BORDER_WIDTH, window_fore_color, window_back_color);

	if (XGameWindow == 0) {
		fprintf(stderr, "can't open main window");
		exit(1);
	};

/*
 *	Open the subwindows
 */
	XGameCountWindow = XCreateSimpleWindow(
		XGameDisplay, XGameWindow,
		X_COUNT, Y_COUNT, COUNT_WIDTH, COUNT_HEIGHT,
		BORDER_WIDTH, window_fore_color, window_back_color);

	if (XGameCountWindow == 0) {
		fprintf(stderr, "can't open COUNT window");
		exit(1);
	};

	XGamePlayWindow = XCreateSimpleWindow(
		XGameDisplay, XGameWindow,
		X_PLAY, Y_PLAY, PLAY_WIDTH, PLAY_HEIGHT,
		BORDER_WIDTH, window_fore_color, window_back_color);

	if (XGamePlayWindow == 0) {
		fprintf(stderr, "can't open PLAY window");
		exit(1);
	};

	XGameDoneWindow = XCreateSimpleWindow(
		XGameDisplay, XGameWindow,
		X_DONE, Y_DONE, OPTION_WIDTH, OPTION_HEIGHT,
		BORDER_WIDTH, window_fore_color, window_back_color);

	if (XGameDoneWindow == 0) {
		fprintf(stderr, "can't open DONE window");
		exit(1);
	};

	XGameQuitWindow = XCreateSimpleWindow(
		XGameDisplay, XGameWindow,
		X_QUIT, Y_QUIT, OPTION_WIDTH, OPTION_HEIGHT,
		BORDER_WIDTH, window_fore_color, window_back_color);

	if (XGameQuitWindow == 0) {
		fprintf(stderr, "can't open QUIT window");
		exit(1);
	};

	XGameSameWindow = XCreateSimpleWindow(
		XGameDisplay, XGameWindow,
		X_SAME, Y_SAME, OPTION_WIDTH, OPTION_HEIGHT,
		BORDER_WIDTH, window_fore_color, window_back_color);

	if (XGameSameWindow == 0) {
		fprintf(stderr, "can't open SAME window");
		exit(1);
	};

	XGameNewWindow = XCreateSimpleWindow(
		XGameDisplay, XGameWindow,
		X_NEW, Y_NEW, OPTION_WIDTH, OPTION_HEIGHT,
		BORDER_WIDTH, window_fore_color, window_back_color);

	if (XGameNewWindow == 0) {
		fprintf(stderr, "can't open NEW window");
		exit(1);
	};

/*
 *	Tell the window manager about us
 */
	XSetStandardProperties(
		XGameDisplay, XGameWindow,
		"xmahjongg", NULL,
		XGameIcon, NULL, 0,
		&XGameHints);

	XGameWMHints.flags = IconPixmapHint | StateHint;
	XGameWMHints.icon_pixmap = XGameIcon;
	XGameWMHints.initial_state = (iconic_start) ? IconicState : NormalState;
	XSetWMHints(XGameDisplay, XGameWindow, &XGameWMHints);

/*
 *	Create a pixmap for tile image copying.
 */
	XGamePixmap = XCreatePixmap(XGameDisplay, XGameWindow,
		PLAY_WIDTH, PLAY_HEIGHT, XGameDepth);

/*
 *	Setup the offsets for the border drawing.
 */
	XGamePoint[ 0].x = -1;	XGamePoint[ 0].y = 1;
	XGamePoint[ 1].x = -1;	XGamePoint[ 1].y = 64;
	XGamePoint[ 2].x = 62;	XGamePoint[ 2].y = 64;
	XGamePoint[ 3].x = 61;	XGamePoint[ 3].y = 65;
	XGamePoint[ 4].x = -2;	XGamePoint[ 4].y = 65;
	XGamePoint[ 5].x = -2;	XGamePoint[ 5].y = 2;
	XGamePoint[ 6].x = -3;	XGamePoint[ 6].y = 3;
	XGamePoint[ 7].x = -3;	XGamePoint[ 7].y = 66;
	XGamePoint[ 8].x = 60;	XGamePoint[ 8].y = 66;
	XGamePoint[ 9].x = 59;	XGamePoint[ 9].y = 67;
	XGamePoint[10].x = -4;	XGamePoint[10].y = 67;
	XGamePoint[11].x = -4;	XGamePoint[11].y = 4;

/*
 *	Setup the GCs for the text and tiles
 */
	XGameBorderGC = XCreateGC(XGameDisplay, XGameWindow, 0, 0);
	XSetForeground(XGameDisplay, XGameBorderGC, border_back_color);
	XSetBackground(XGameDisplay, XGameBorderGC, border_fore_color);
	XSetFont(XGameDisplay, XGameBorderGC, XGameTileFont->fid);

	XGameTextGC[0] = XCreateGC(XGameDisplay, XGameWindow, 0, 0);
	XSetForeground(XGameDisplay, XGameTextGC[0], other_fore_color);
	XSetBackground(XGameDisplay, XGameTextGC[0], other_back_color);
	XSetFont(XGameDisplay, XGameTextGC[0], XGameTextFont->fid);

	XGameTextGC[1] = XCreateGC(XGameDisplay, XGameWindow, 0, 0);
	XSetForeground(XGameDisplay, XGameTextGC[1], other_back_color);
	XSetBackground(XGameDisplay, XGameTextGC[1], other_fore_color);
	XSetFont(XGameDisplay, XGameTextGC[1], XGameTextFont->fid);

	XGameOtherGC[0] = XCreateGC(XGameDisplay, XGameWindow, 0, 0);
	XSetForeground(XGameDisplay, XGameOtherGC[0], other_fore_color);
	XSetBackground(XGameDisplay, XGameOtherGC[0], other_back_color);
	XSetFont(XGameDisplay, XGameOtherGC[0], XGameTileFont->fid);

	XGameOtherGC[1] = XCreateGC(XGameDisplay, XGameWindow, 0, 0);
	XSetForeground(XGameDisplay, XGameOtherGC[1], other_back_color);
	XSetBackground(XGameDisplay, XGameOtherGC[1], other_fore_color);
	XSetFont(XGameDisplay, XGameOtherGC[1], XGameTileFont->fid);

	for (i = 0; i < LEVS; i++) {
		XGameTileGC[i][0] = XCreateGC(XGameDisplay, XGameWindow, 0, 0);
		XSetForeground(XGameDisplay, XGameTileGC[i][0], fore_colors[i]);
		XSetBackground(XGameDisplay, XGameTileGC[i][0], back_colors[i]);
		XSetFont(XGameDisplay, XGameTileGC[i][0], XGameTileFont->fid);

		XGameTileGC[i][1] = XCreateGC(XGameDisplay, XGameWindow, 0, 0);
		XSetForeground(XGameDisplay, XGameTileGC[i][1], back_colors[i]);
		XSetBackground(XGameDisplay, XGameTileGC[i][1], fore_colors[i]);
		XSetFont(XGameDisplay, XGameTileGC[i][1], XGameTileFont->fid);
	};

/*
 *	Define the cursor for the game
 */
	XGameCursor = XCreateFontCursor(XGameDisplay, XC_left_ptr);
	XDefineCursor(XGameDisplay, XGameWindow, XGameCursor);

/*
 *	Select window exposure events to see if the contents of the window
 *	have been erased or altered.
 */
	XSelectInput(XGameDisplay, XGameWindow, XGameEvents);
	XSelectInput(XGameDisplay, XGameNewWindow, XGameEvents);
	XSelectInput(XGameDisplay, XGameDoneWindow, XGameEvents);
	XSelectInput(XGameDisplay, XGameQuitWindow, XGameEvents);
	XSelectInput(XGameDisplay, XGameSameWindow, XGameEvents);
	XSelectInput(XGameDisplay, XGamePlayWindow, XGameEvents);
	XSelectInput(XGameDisplay, XGameCountWindow, XGameEvents);

/*
 *	Map the game window to the screen.
 */
	XMapRaised(XGameDisplay, XGameWindow);
	while (1) {
		XNextEvent(XGameDisplay, &XGameEvent);
		if (XGameEvent.type == Expose) break;
	};

/*
 *	Display the game name.
 */
	XDrawLine(XGameDisplay, XGameWindow, XGameTextGC[0], 0, 184, 1000, 184);

	draw_letter(INDEX_X,  42, 50);
	draw_letter(INDEX_M, 148, 50);
	draw_letter(INDEX_A, 254, 50);
	draw_letter(INDEX_H, 360, 50);
	draw_letter(INDEX_J, 466, 50);
	draw_letter(INDEX_O, 572, 50);
	draw_letter(INDEX_N, 678, 50);
	draw_letter(INDEX_G, 784, 50);
	draw_letter(INDEX_G, 890, 50);

/*
 *	Display the copyright notice
 */
	i = strlen(copyright);
	j = XGameTextFont->max_bounds.width;
	k = XGameTextFont->ascent;

	j = (WINDOW_WIDTH - i*j)/2;
	k = (174 - k);
	draw_string(copyright, j, k, XGameTextGC[1]);

/*
 *	Display the tiles on the screen before the start of the game.  Order:
 *	dragons, seasons, plants, directions, circles, bamboos, ideographs
 */
	draw_data();

	XFlush(XGameDisplay);
	sleep(5);

/*
 *	Find the maximum file descriptor for the select system call and set
 *	the initialization flag.
 */
	initial_flag++;
	maxfds = 31;

/*
 *	Map the subwindows
 */
	XClearArea(XGameDisplay, XGameWindow, 0, 0, 0, 0, True);
	XMapSubwindows(XGameDisplay, XGameWindow);
	XFlush(XGameDisplay);
	return(0);
}

setup_color(r, g, b)
int	r;
int	g;
int	b;
{
	XColor XGameColor;

	XGameColor.red = r;
	XGameColor.green = g;
	XGameColor.blue = b;

	if (XAllocColor(XGameDisplay,XGameColormap, &XGameColor) == 0) {
		fprintf(stderr, "can't allocate color\n");
		exit(1);
	};

	return(XGameColor.pixel);
}

get_tile() {
	int i, j;

	i = random_next()%TILES;
	j = ((random_next()%2) == 0) ? 1 : -1;

/*
 *	Walk through the tile pool looking for an unused tile
 */
	while (buffer[i] == 0) {
		i = (i+j)%TILES;
		if (i < 0) i = TILES-1;
	};

	j = buffer[i];
	buffer[i] = 0;

	return(j);
}

