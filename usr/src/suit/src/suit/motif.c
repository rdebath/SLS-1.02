/* (C) Copyright 1990, 1991, 1992 the University of Virginia */


#include "suit.h"


extern numberLegalColors;

GP_color GP_getShadowColor (GP_color color)
{
    unsigned short red;
    unsigned short green;
    unsigned short blue;
    unsigned int tred;
    unsigned int tgreen;
    unsigned int tblue;
    GP_color shadow;

    GP_describeColor (color, &red, &green, &blue);
    tred = red - red / 100 * 45;
    tgreen = green - green / 100 * 45;
    tblue = blue - blue / 100 * 45;
    shadow = GP_defColorRGB (tred, tgreen, tblue, BLACK_ON_MONO);
    return shadow;
}


GP_color GP_getDepthColor (GP_color color)
{
    if (BILEVEL_DISPLAY)
	return GP_defColor (color.colorName, !color.blackOnMonochrome);
    else {
	unsigned short red;
	unsigned short green;
	unsigned short blue;
	unsigned int tred;
	unsigned int tgreen;
	unsigned int tblue;
	GP_color shadow;
	
	GP_describeColor (color, &red, &green, &blue);
	tred = red - red / 100 * 10;
	tgreen = green - green / 100 * 10;
	tblue = blue - blue / 100 * 10;
	shadow = GP_defColorRGB (tred, tgreen, tblue, BLACK_ON_MONO);
	return shadow;
    }
}


#ifdef IBM_PC
#   define MAX_LIMIT  153
#   define MAX_INT    255
#else
#   define MAX_LIMIT   39321
#   define MAX_INT     65535
#endif

GP_color GP_getHighlightColor (GP_color color)
{
    unsigned short red;
    unsigned short green;
    unsigned short blue;
    unsigned short tred;
    unsigned short tgreen;
    unsigned short tblue;
    GP_color highlight;

    GP_describeColor (color, &red, &green, &blue);
    tred = (red >= MAX_LIMIT)? MAX_INT : red + (red * 4) / 10;
    tgreen = (green >= MAX_LIMIT)? MAX_INT : green + (green * 4) / 10;
    tblue = (blue >= MAX_LIMIT)? MAX_INT : blue + (blue * 4) / 10;
    highlight = GP_defColorRGB (tred, tgreen, tblue, BLACK_ON_MONO);
    return highlight;
}



PRIVATE void SetShadowOrHighlight (boolean raised, GP_color shadow, GP_color highlight)
{
    if (!BILEVEL_DISPLAY) {
	if (raised) 
	    GP_setColor (shadow);
	else
	    GP_setColor (highlight);
    } else {	
	SRGP_setColor (SRGP_WHITE);
	SRGP_setBackgroundColor (SRGP_BLACK);
	SRGP_setFillStyle (BITMAP_PATTERN_OPAQUE);
	SRGP_setFillBitmapPattern (raised ? 0 : 4);
    }
}


PRIVATE void SetColorOrDepthColor (boolean raised, GP_color c)
{
    if (raised) 
	GP_setColor (c);
    else
	GP_setColor (GP_getDepthColor(c));
}


void GP_unbeveledBorder (rectangle box, GP_color color, boolean raised, int width)
{
    int x1 = box.bottom_left.x;
    int y1 = box.bottom_left.y;
    int x2 = box.top_right.x;
    int y2 = box.top_right.y;
    GP_color highlight, shadow;
    
    highlight = GP_getHighlightColor (color);
    shadow = GP_getShadowColor (color);
    width--;
    
    GP_pushGraphicsState ();
    SetShadowOrHighlight (!raised, shadow, highlight);
    SRGP_fillRectangleCoord (x1, y2 - width, x2, y2);	/* Draw top rectangle */
    SRGP_fillRectangleCoord (x1, y1 + width, x1 + width, y2 - width);	/* draw left reactangle */
    
    SetShadowOrHighlight (raised, shadow, highlight);
    SRGP_fillRectangleCoord (x1, y1, x2, y1 + width);	/* Draw bottom */
    SRGP_fillRectangleCoord (x2 - width, y1 + width, x2, y2 - width);	/* draw right */
    GP_popGraphicsState ();
}


void GP_beveledBorder (rectangle box, GP_color color, boolean raised, int width)
{
	int x[6], y[6];

	int x1 = box.bottom_left.x;
#if defined(SGI_GL) || defined(IBM_PC) 
    int y1 = box.bottom_left.y;
#else
    int y1 = box.bottom_left.y - 1;
#endif

#if defined(IBM_PC)
    int x2 = box.top_right.x;
#else
    int x2 = box.top_right.x + 1;
#endif

#if defined(SGI_GL)
    int y2 = box.top_right.y + 1;
#else
    int y2 = box.top_right.y;
#endif

    GP_color highlight, shadow;
#ifdef IBM_PC
	width--;
#endif

    highlight = GP_getHighlightColor (color);
    shadow = GP_getShadowColor (color);
    
	GP_pushGraphicsState ();
    SetShadowOrHighlight (raised, shadow, highlight);
    x[0] = x1;
    y[0] = y1;
    x[1] = x2;
    y[1] = y1;
    x[2] = x2;
    y[2] = y2;
	x[3] = x2 - width;
    y[3] = y2 - width;
    x[4] = x2 - width;
    y[4] = y1 + width;
    x[5] = x1 + width;
    y[5] = y1 + width;
    SRGP_fillPolygonCoord (6, x, y);
    
    SetShadowOrHighlight (!raised, shadow, highlight);
    x[0] = x1;
    y[0] = y1;
    x[1] = x1;
    y[1] = y2;
    x[2] = x2;
    y[2] = y2;
    x[3] = x2 - width;
    y[3] = y2 - width;
    x[4] = x1 + width;
    y[4] = y2 - width;
    x[5] = x1 + width;
    y[5] = y1 + width;
    SRGP_fillPolygonCoord (6, x, y);
    GP_popGraphicsState ();
}



void GP_fancyBeveledBorder (rectangle box, GP_color color, int width)
{
    int x1 = box.bottom_left.x;
    int y1 = box.bottom_left.y;
    int x2 = box.top_right.x;
    int y2 = box.top_right.y;
    int w1, w2, w3;
    
    if (width >= 8) {
	w1 = width / 4;
	w3 = width / 8;
	w2 = width - w1 - 2 * w3;
    } else {
	if (width < 5) {
	    x1-=(5-width);
	    y1-=(5-width);
	    x2+=(5-width);
	    y2+=(5-width);
	    width = 5;
	}
        
	w1 = 2;
	w3 = 1;
	w2 = width - w1 - 2 * w3;
    }
    
    GP_pushGraphicsState ();
    GP_beveledBorder (box, color, TRUE, w1);
    
    GP_setColor (color);
    SRGP_fillRectangleCoord (x1 + w1, y1 + w1, x1 + w1 + w2, y2 - w1);	/* left */
    SRGP_fillRectangleCoord (x1 + w1 + w2, y1 + w1, x2 - w1 - w2, y1 + w1 + w2);	/* bottom */
    SRGP_fillRectangleCoord (x1 + w1 + w2, y2 - w1 - w2, x2 - w1 - w2, y2 - w1);	/* top */
    SRGP_fillRectangleCoord (x2 - w1 - w2, y1 + w1, x2 - w1, y2 - w1);	/* right */
    
    GP_unbeveledBorder (SRGP_defRectangle (x1 + w1 + w2, y1 + w1 + w2, x2 - w1 - w2, y2 - w1 - w2), color, FALSE, w3);
    GP_unbeveledBorder (SRGP_defRectangle (x1 + w1 + w2 + w3, y1 + w1 + w2 + w3, x2 - w1 - w2 - w3, y2 - w1 - w2 - w3), color, TRUE, w3);
    GP_popGraphicsState ();
}



void GP_beveledBox (rectangle box, GP_color color, boolean raised, int width)
{
    int margin = (!raised && BILEVEL_DISPLAY)? width+1 : width;
    SetColorOrDepthColor (raised, color);
    if (box.bottom_left.x + margin < box.top_right.x - margin &&
	box.bottom_left.y + margin < box.top_right.y - margin)
	SRGP_fillRectangleCoord (box.bottom_left.x + margin, box.bottom_left.y + margin,
				 box.top_right.x - margin, box.top_right.y - margin);
    if (box.top_right.x - box.bottom_left.x < 2*width ||
	box.top_right.y - box.bottom_left.y < 2*width)
	GP_unbeveledBorder (box, color, raised, 1);
    else
	GP_beveledBorder (box, color, raised, width);
}



void GP_beveledDiamond (rectangle box, GP_color color, boolean raised, int width)
{
    int x[6], y[6];
    int x1 = box.bottom_left.x;
    int y1 = box.bottom_left.y;
    int x2 = box.top_right.x;
    int y2 = box.top_right.y;
    int yhalf, xhalf;
    GP_color highlight, shadow;
    int margin = (!raised && BILEVEL_DISPLAY)? width+1 : width;

    highlight = GP_getHighlightColor (color);
    shadow = GP_getShadowColor (color);

    xhalf = (x2 - x1) / 2;
    yhalf = (y2 - y1) / 2;

    GP_pushGraphicsState ();
    SetColorOrDepthColor (raised, color);
    x[0] = x1 + xhalf;
    y[0] = y1 + margin;
    x[1] = x1 + margin;
    y[1] = y1 + yhalf;
    x[2] = x1 + xhalf;
    y[2] = y2 - margin;
    x[3] = x2 - margin;
    y[3] = y1 + yhalf;
    SRGP_fillPolygonCoord (4, x, y);

    SetShadowOrHighlight (!raised, shadow, highlight);
    x[0] = x1;
    y[0] = y1 + yhalf;
    x[1] = x1 + xhalf;
    y[1] = y2;
    x[2] = x2;
    y[2] = y1 + yhalf;
    x[3] = x2 - width;
    y[3] = y1 + yhalf;
    x[4] = x1 + xhalf;
    y[4] = y2 - width;
    x[5] = x1 + width;
    y[5] = y1 + yhalf;
    SRGP_fillPolygonCoord (6, x, y);

    SetShadowOrHighlight (raised, shadow, highlight);
    x[0] = x1;
    y[0] = y1 + yhalf;
    x[1] = x1 + xhalf;
    y[1] = y1;
    x[2] = x2;
    y[2] = y1 + yhalf;
    x[3] = x2 - width;
    y[3] = y1 + yhalf;
    x[4] = x1 + xhalf;
    y[4] = y1 + width;
    x[5] = x1 + width;
    y[5] = y1 + yhalf;
    SRGP_fillPolygonCoord (6, x, y);
    GP_popGraphicsState ();
}


void GP_beveledTriangleNorth (GP_point p3, GP_point p1, GP_point p2, GP_color color, boolean raised, int thickness)
{
    int x[4], y[4];
    point pt1, pt2, pt3;
    int x1, y1, x2, y2, x3, y3;
    GP_color highlight, shadow;

    pt1 = GP_mapPoint (p1);
    pt2 = GP_mapPoint (p2);
    pt3 = GP_mapPoint (p3);
    x1 = pt1.x; y1 = pt1.y;
    x2 = pt2.x; y2 = pt2.y;
    x3 = pt3.x; y3 = pt3.y;

    highlight = GP_getHighlightColor (color);
    shadow = GP_getShadowColor (color);

    GP_pushGraphicsState ();
    if (BILEVEL_DISPLAY)
	GP_setColor(color);
    else
	SetColorOrDepthColor (raised, color);
    x[0] = x1;
    y[0] = y1;
    x[1] = x2;
    y[1] = y2;
    x[2] = x3;
    y[2] = y3;
    SRGP_fillPolygonCoord (3, x, y);

    SetShadowOrHighlight (raised, shadow, highlight);
    x[0] = x1;
    y[0] = y1;
    x[1] = x1 + thickness;
    y[1] = y1 + thickness;
    x[2] = x2 - thickness;
    y[2] = y1 + thickness;
    x[3] = x2;
    y[3] = y2;
    SRGP_fillPolygonCoord (4, x, y);

    x[0] = x2;
    y[0] = y2;
    x[1] = x2 - thickness;
    y[1] = y2 + thickness;
    x[2] = x3;
    y[2] = y3 - thickness;
    x[3] = x3;
    y[3] = y3;
    SRGP_fillPolygonCoord (4, x, y);

    SetShadowOrHighlight (!raised, shadow, highlight);
    x[0] = x3;
    y[0] = y3;
    x[1] = x3;
    y[1] = y3 - thickness;
    x[2] = x1 + thickness;
    y[2] = y1 + thickness;
    x[3] = x1;
    y[3] = y1;
    SRGP_fillPolygonCoord (4, x, y);
    GP_popGraphicsState ();
}


void GP_beveledTriangleWest (GP_point p1, GP_point p2, GP_point p3, GP_color color, boolean raised, int thickness)
{
    int x[4], y[4];
    point pt1, pt2, pt3;
    int x1, y1, x2, y2, x3, y3;
    GP_color highlight, shadow;

    pt1 = GP_mapPoint (p1);
    pt2 = GP_mapPoint (p2);
    pt3 = GP_mapPoint (p3);
    x1 = pt1.x; y1 = pt1.y;
    x2 = pt2.x; y2 = pt2.y;
    x3 = pt3.x; y3 = pt3.y;
    highlight = GP_getHighlightColor (color);
    shadow = GP_getShadowColor (color);

    GP_pushGraphicsState ();
    if (BILEVEL_DISPLAY)
	GP_setColor(color);
    else
	SetColorOrDepthColor (raised, color);
    x[0] = x1;
    y[0] = y1;
    x[1] = x2;
    y[1] = y2;
    x[2] = x3;
    y[2] = y3;
    SRGP_fillPolygonCoord (3, x, y);

    SetShadowOrHighlight (raised, shadow, highlight);
    x[0] = x1;
    y[0] = y1;
    x[1] = x1 + thickness;
    y[1] = y1;
    x[2] = x2 - thickness;
    y[2] = y2 + thickness;
    x[3] = x2;
    y[3] = y2;
    SRGP_fillPolygonCoord (4, x, y);

    x[0] = x2;
    y[0] = y2;
    x[1] = x2 - thickness;
    y[1] = y2 + thickness;
    x[2] = x3 - thickness;
    y[2] = y3 - thickness;
    x[3] = x3;
    y[3] = y3;
    SRGP_fillPolygonCoord (4, x, y);

    SetShadowOrHighlight (!raised, shadow, highlight);
    x[0] = x3;
    y[0] = y3;
    x[1] = x3 - thickness;
    y[1] = y3 - thickness;
    x[2] = x1 + thickness;
    y[2] = y1;
    x[3] = x1;
    y[3] = y1;
    SRGP_fillPolygonCoord (4, x, y);
    GP_popGraphicsState ();
}



void GP_beveledTriangleSouth (GP_point p1, GP_point p2, GP_point p3, GP_color color, boolean raised, int thickness)
{
    int x[4], y[4];
    point pt1, pt2, pt3;
    int x1, y1, x2, y2, x3, y3;
    GP_color highlight, shadow;

    pt1 = GP_mapPoint (p1);
    pt2 = GP_mapPoint (p2);
    pt3 = GP_mapPoint (p3);
    x1 = pt1.x; y1 = pt1.y;
    x2 = pt2.x; y2 = pt2.y;
    x3 = pt3.x; y3 = pt3.y;
    highlight = GP_getHighlightColor (color);
    shadow = GP_getShadowColor (color);

    GP_pushGraphicsState ();
    if (BILEVEL_DISPLAY)
	GP_setColor(color);
    else
	SetColorOrDepthColor (raised, color);
    x[0] = x1;
    y[0] = y1;
    x[1] = x2;
    y[1] = y2;
    x[2] = x3;
    y[2] = y3;
    SRGP_fillPolygonCoord (3, x, y);

    SetShadowOrHighlight (raised, shadow, highlight);
    x[0] = x1;
    y[0] = y1;
    x[1] = x1;
    y[1] = y1 + thickness;
    x[2] = x2 - thickness;
    y[2] = y2 - thickness;
    x[3] = x2;
    y[3] = y2;
    SRGP_fillPolygonCoord (4, x, y);

    SetShadowOrHighlight (!raised, shadow, highlight);
    x[0] = x2;
    y[0] = y2;
    x[1] = x2 - thickness;
    y[1] = y2 - thickness;
    x[2] = x3 + thickness;
    y[2] = y3 - thickness;
    x[3] = x3;
    y[3] = y3;
    SRGP_fillPolygonCoord (4, x, y);

    x[0] = x3;
    y[0] = y3;
    x[1] = x3 + thickness;
    y[1] = y3 - thickness;
    x[2] = x1;
    y[2] = y1 + thickness;
    x[3] = x1;
    y[3] = y1;
    SRGP_fillPolygonCoord (4, x, y);
    GP_popGraphicsState ();
}



void GP_beveledTriangleEast (GP_point p2, GP_point p3, GP_point p1, GP_color color, boolean raised, int thickness)
{
    int x[4], y[4];
    point pt1, pt2, pt3;
    int x1, y1, x2, y2, x3, y3;
    GP_color highlight, shadow;

    pt1 = GP_mapPoint (p1);
    pt2 = GP_mapPoint (p2);
    pt3 = GP_mapPoint (p3);
    x1 = pt1.x; y1 = pt1.y;
    x2 = pt2.x; y2 = pt2.y;
    x3 = pt3.x; y3 = pt3.y;
    highlight = GP_getHighlightColor (color);
    shadow = GP_getShadowColor (color);

    GP_pushGraphicsState ();
    if (BILEVEL_DISPLAY)
	GP_setColor(color);
    else
	SetColorOrDepthColor (raised, color);
    x[0] = x1;
    y[0] = y1;
    x[1] = x2;
    y[1] = y2;
    x[2] = x3;
    y[2] = y3;
    SRGP_fillPolygonCoord (3, x, y);

    SetShadowOrHighlight (raised, shadow, highlight);
    x[0] = x1;
    y[0] = y1;
    x[1] = x1 + thickness;
    y[1] = y1 + thickness;
    x[2] = x2 - thickness;
    y[2] = y2;
    x[3] = x2;
    y[3] = y2;
    SRGP_fillPolygonCoord (4, x, y);

    SetShadowOrHighlight (!raised, shadow, highlight);
    x[0] = x2;
    y[0] = y2;
    x[1] = x2 - thickness;
    y[1] = y2;
    x[2] = x3 + thickness;
    y[2] = y3 - thickness;
    x[3] = x3;
    y[3] = y3;
    SRGP_fillPolygonCoord (4, x, y);

    x[0] = x3;
    y[0] = y3;
    x[1] = x3 + thickness;
    y[1] = y3 - thickness;
    x[2] = x1 + thickness;
    y[2] = y1 + thickness;
    x[3] = x1;
    y[3] = y1;
    SRGP_fillPolygonCoord (4, x, y);
    GP_popGraphicsState ();
}
