/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"

void PaintNewPanner(SUIT_object me)
    {
    if (SUIT_getBoolean (me, DRAW_AXIS_LINES))
	{
	SUIT_window win;
	GP_setColor (SUIT_getColor (me, AXIS_COLOR));
	win = SUIT_getWindow (me, WINDOW);
	GP_lineCoord (0.0, win.bottom_left.y, 0.0, win.top_right.y);
	GP_lineCoord (win.bottom_left.x, 0.0, win.top_right.x, 0.0);
	}

    GP_setColor (SUIT_getColor (me, TILE_COLOR));
    if (SUIT_getBoolean (me, FILL_TILE))
	{
	GP_fillRectangle (SUIT_getWindow (me, TILE_EXTENTS));
	}
    else
	{
	GP_drawRectangle (SUIT_getWindow (me, TILE_EXTENTS));
	}
    }

/* Scoot the tile back into the playing area. */
GP_rectangle KeepTileInWindow(SUIT_window win, SUIT_window tile)
    {
    GP_rectangle retval;
    double wid, hgt;
    double lowx, lowy, highx, highy;
    double minx, miny, maxx, maxy;

    wid = tile.top_right.x - tile.bottom_left.x;
    hgt = tile.top_right.y - tile.bottom_left.y;

    minx = win.bottom_left.x;
    miny = win.bottom_left.y;
    maxx = win.top_right.x;
    maxy = win.top_right.y;

    lowx = tile.bottom_left.x;
    lowy = tile.bottom_left.y;
    highx = tile.top_right.x;
    highy = tile.top_right.y;
    
    if (lowx < minx)	{lowx = minx;  highx = minx + wid;}
	
    if (lowy < miny)	{lowy = miny;  highy = miny + hgt;}
	
    if (highx > maxx)	{highx = maxx; lowx  = maxx - wid;}
	
    if (highy > maxy) 	{highy = maxy; lowy  = maxy - hgt;}
	
    retval = GP_defRectangle(lowx, lowy, highx, highy);
    return (retval);
    
    }


boolean EventInRectangle(SUIT_event e, GP_rectangle r)
    {
    return ((e.worldLocation.x > r.bottom_left.x) &&
	    (e.worldLocation.y > r.bottom_left.y) &&
	    (e.worldLocation.x < r.top_right.x)   &&
	    (e.worldLocation.y < r.top_right.y));
    }


void HitNewPanner(SUIT_object me, SUIT_event ev)
    {
    SUIT_window win;
    GP_rectangle tile;
    SUIT_viewport vp;

    tile = SUIT_getWindow (me, TILE_EXTENTS);
    if (EventInRectangle (ev, tile))
	{
	vp = SUIT_moveRectangle (GP_mapRectangle (tile), ev.locator, FALSE);
	tile = GP_unMapRectangle (vp);
	win = SUIT_getWindow (me, WINDOW);
	tile = KeepTileInWindow(win, tile);
	SUIT_setWindow (me, TILE_EXTENTS, tile);
	}
    }
    


SUIT_object SUIT_createPanner(char *name, 
				 GP_rectangle world, GP_rectangle tile)
    {
    SUIT_object retval;
    
    retval = SUIT_createObject (name, "2D panner");
    SUIT_addDisplayToObject (retval, "simple panner", HitNewPanner, PaintNewPanner); 

    SUIT_setWindow (retval, WINDOW, world);
    SUIT_setWindow (retval, TILE_EXTENTS, tile);
    
    return (retval);
    }


void CreateObjects(void)
    {
    GP_rectangle AreaWindow, TileWindow;

    AreaWindow = GP_defRectangle(0.0, 0.0, 1.0, 1.0);
    TileWindow = GP_defRectangle(0.0, 0.0, 0.5, 0.5);
    SUIT_createDoneButton();
    SUIT_createPanner("fred", AreaWindow, TileWindow);
    }


void main(int argc, char *argv[])
    {
    SUIT_init(&argc, argv);
    if ( argc != 1 )
	{
	SUIT_printCommandLineOptions();
	exit(-1);
	}

    CreateObjects();

    SUIT_beginDisplay();
    for ( ;;)
	SUIT_checkAndProcessInput(INDEFINITE);
    }

		
