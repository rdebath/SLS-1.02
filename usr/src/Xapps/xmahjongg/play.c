/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#include <stdio.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "xmahjongg.h"
#include "variables.h"

extern int initial_flag;
int x_dist[] = {  0, -1,  1,  0, -1,  1, -1,  0,  1, -1,  0,  1, -1,  0,  1};
int y_dist[] = {  2,  2,  2, -2, -2, -2, -1, -1, -1,  0,  0,  0,  1,  1,  1};
int z_dist[] = {  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  1,  1};

play_tile(tp)
Tile *tp;
{

	if (not_free(tp->row, tp->col, tp->lev)) {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
	} else if (tile1p == NULL) {
		invert_tile(tp, 1);
		tile1p = tp;
	} else if (tp == tile1p) {
		invert_tile(tp, 0);
		tile1p = NULL;
	} else if (tp->type == tile1p->type) {
		tiles_remaining -= 2;
		remove_tile(tile1p);
		remove_tile(tp);
		tile1p = NULL;
		draw_count();
		draw_matches();
		if (done_count == 0) packet_send(GAME_PLAY);
	} else {
		XBell(XGameDisplay, 100);
		XFlush(XGameDisplay);
	};

	return(0);
}

invert_tile(tp, type)
Tile *tp;
int type;
{

	redraw_tile(tp, type);
	XFlush(XGameDisplay);

	return(0);
}

remove_tile(tp)
Tile *tp;
{

	tp->state = FREE;
	redraw_tile(tp, 0);
	XFlush(XGameDisplay);

	return(0);
}

create_tile(tp)
Tile *tp;
{
	int i, j;
	int dx, dy, dz;

	for (i = 0; i < TILES; i++) {
		if (order[i] == NULL) break;

		dx = order[i]->row - tp->row;
		dy = order[i]->col - tp->col;
		dz = order[i]->lev - tp->lev;

		if (dz < 0) continue;

		if (dz > 0) break;
		if (dy < dx) break;
		if ((dy == dx) && (dy > 0)) break;
	};

	for (j = TILES - tiles_remaining; j > i; j--) {
		order[j] = order[j-1];
	};

	order[i] = tp;
	tiles_remaining--;
	draw_count();

	tp->data = tp->lev;
	tp->state = USED;
	redraw_tile(tp, 0);
	XFlush(XGameDisplay);

	return(0);
}

delete_tile(tp)
Tile *tp;
{
	int i, j;

	for (i = 0; i < TILES; i++) {
		if (order[i] == NULL) break;
		if (order[i]->row != tp->row) continue;
		if (order[i]->col != tp->col) continue;
		if (order[i]->lev != tp->lev) continue;

		for (j = i; j < TILES-1; j++) {
			order[j] = order[j+1];
		};

		order[TILES-1] = 0;
		break;
	};

	tiles_remaining++;
	draw_count();

	tp->state = FREE;
	redraw_tile(tp, 0);
	XFlush(XGameDisplay);

	return(0);
}

redraw_tile(tp, type)
Tile	*tp;
int	type;
{
	int x_src, y_src;
	int x_dst, y_dst;
	int row, col, lev;
	int i, j, k, l, x, y, rv;
	char string[4];
	Tile *xp[144];
	GC gc;

/*
 *	Copy the tiles into the pixmap and then dump the pixmap into
 *	the window.
 */
	row = tp->row;
	col = tp->col;
	lev = tp->lev;

	string[0] = 127;
	string[1] = 127;
	string[2] = 127;
	XDrawImageString(XGameDisplay, XGamePixmap, XGameOtherGC[0],
		0, 0*TILE_SIDE, string, 3);
	XDrawImageString(XGameDisplay, XGamePixmap, XGameOtherGC[0],
		0, 1*TILE_SIDE, string, 3);
	XDrawImageString(XGameDisplay, XGamePixmap, XGameOtherGC[0],
		0, 2*TILE_SIDE, string, 3);

	l = load_tiles(xp, row, col);

	for (i = 0; i < l; i++) {
		rv = (xp[i] == tp) ? type : 0;
		k = (color_type == 0) ? xp[i]->lev : (xp[i]->data/16) - 1;

		string[0] = xp[i]->data;
		x = (xp[i]->col + 2-col)*(TILE_SIDE/2) + 4*xp[i]->lev;
		y = (xp[i]->row + 2-row)*(TILE_SIDE/2) - 4*xp[i]->lev;
		XDrawImageString(XGameDisplay, XGamePixmap,
			XGameTileGC[k][rv], x, y, string, 1);

		draw_border(x, y);
	};

	x_src = TILE_SIDE + 4*(lev-1);
	y_src = TILE_SIDE - 4*(lev-0);
	x_dst = tp->x - 4;
	y_dst = tp->y + 0;

	XCopyArea(XGameDisplay, XGamePixmap, XGamePlayWindow, XGameTextGC[0],
		x_src, y_src, TILE_SIDE+4, TILE_SIDE+4, x_dst, y_dst);

	return(0);
}

not_free(row, col, lev) 
int row, col, lev;
{
	int i, mask;
	int r1, c1, l1;

	for (i = 0, mask = 0; i < 15; i++) {
		r1 = row+x_dist[i];
		c1 = col+y_dist[i];
		l1 = lev+z_dist[i];
		if ((r1 < 0) || (r1 >=  ROWS)) continue;
		if ((c1 < 0) || (c1 >=  COLS)) continue;
		if ((l1 < 0) || (l1 >=  LEVS)) continue;

		if (tiles[r1][c1][l1].state == USED) {
			mask |= (1 << i);
		};
	};

	if ((mask & 077700) != 0) {
		return(1);
	} else if ((mask & 000070) == 0) {
		return(0);
	} else if ((mask & 000007) != 0) {
		return(1);
	} else {
		return(0);
	};
}

load_tiles(xp, row, col)
Tile	*xp[];
int	row;
int	col;
{
	register int i, m;
	register int dx, dy;
	register Tile *tp;

	m = 0;

	for (i = 0, tp = order[0]; i < TILES; i++, tp = order[i]) {
		if (tp == NULL) break;
		if (tp->state != USED) continue;

		dx = tp->row - row;
		dy = tp->col - col;

		if (dx < 0) dx = -dx;
		if (dy < 0) dy = -dy;

		if ((dx <= 2) && (dy <= 2)) xp[m++] = tp;
	};

	return(m);
}

ok_below(x, y, z)
int	x;
int	y;
int	z;
{
	int i, j, k;

/*
 *	Check this level to be sure that this tile will not overlap another
 *	tile.
 */
	for (i = x-1; i <= x+1; i++) {
		for (j = y-1; j <= y+1; j++) {
			if ((i < 0) || (i >= ROWS)) continue;
			if ((j < 0) || (j >= COLS)) continue;
			if ((i == x) && (j == y)) continue;
			if (tiles[i][j][z].state == USED) return(-1);
		};
	};

	if (z == 0) return(0);

/*
 *	Check the level below this tile to be sure that all four quadrants are
 *	covered.
 */
quadrant1:
	if (tiles[x-1][y-1][z-1].state == USED) goto quadrant2;
	if (tiles[x-1][y-0][z-1].state == USED) goto quadrant2;
	if (tiles[x-0][y-1][z-1].state == USED) goto quadrant2;
	if (tiles[x-0][y-0][z-1].state == USED) goto quadrant2;
	return(-1);

quadrant2:
	if (tiles[x-1][y-0][z-1].state == USED) goto quadrant3;
	if (tiles[x-1][y+1][z-1].state == USED) goto quadrant3;
	if (tiles[x-0][y-0][z-1].state == USED) goto quadrant3;
	if (tiles[x-0][y+1][z-1].state == USED) goto quadrant3;
	return(-1);

quadrant3:
	if (tiles[x-0][y-1][z-1].state == USED) goto quadrant4;
	if (tiles[x-0][y-0][z-1].state == USED) goto quadrant4;
	if (tiles[x+1][y-1][z-1].state == USED) goto quadrant4;
	if (tiles[x+1][y-0][z-1].state == USED) goto quadrant4;
	return(-1);

quadrant4:
	if (tiles[x-0][y-0][z-1].state == USED) goto quadrant5;
	if (tiles[x-0][y+1][z-1].state == USED) goto quadrant5;
	if (tiles[x+1][y-0][z-1].state == USED) goto quadrant5;
	if (tiles[x+1][y+1][z-1].state == USED) goto quadrant5;
	return(-1);

quadrant5:
	return(0);
}

ok_above(x, y, z)
int	x;
int	y;
int	z;
{
	int i, j, k;

	if (z == (LEVS-1)) return(0);

/*
 *	Check the level above this tile to be sure that all four quadrants are
 *	free.
 */
	for (i = -1; i <= 1; i++) {
		for (j = -1; j <= 1; j++) {
			if (((x+i) < 0) || ((x+i) >= ROWS)) continue;
			if (((y+j) < 0) || ((y+j) >= COLS)) continue;
			if (tiles[x+i][y+i][z+1].state == USED) return(-1);
		};
	};

/*
	if (tiles[x-1][y-1][z+1].state == USED) return(-1);
	if (tiles[x-1][y-0][z+1].state == USED) return(-1);
	if (tiles[x-1][y+1][z+1].state == USED) return(-1);

	if (tiles[x-0][y-1][z+1].state == USED) return(-1);
	if (tiles[x-0][y-0][z+1].state == USED) return(-1);
	if (tiles[x-0][y+1][z+1].state == USED) return(-1);

	if (tiles[x+1][y-1][z+1].state == USED) return(-1);
	if (tiles[x+1][y-0][z+1].state == USED) return(-1);
	if (tiles[x+1][y+1][z+1].state == USED) return(-1);
*/

	return(0);
}
