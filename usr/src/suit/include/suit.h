/* (C) 1990 Copyright Rector and Visitors of the University of Virginia */

#ifndef BASESUIT_INCLUDED
#define BASESUIT_INCLUDED 1


#define SUIT_VERSION  2
#define SUIT_REVISION 3



/* ------------------------------------------------------------------------------------------ */

#ifndef AVAILABLE_ARCHITECTURES
#    define AVAILABLE_ARCHITECTURES
#    ifdef THINK_C
#        define MACINTOSH
#        ifndef __STDC__
#            define __STDC__ /* the compiler is ANSI compatible but doesn't define this */
#        endif
#    elif __TURBOC__
#        define IBM_PC
#    elif _AIX
#        define RS6000
#    elif sgi
#        define SGI_X /* one day we'll there'll also be SGI_GL */
#    elif ultrix
#        define DEC
#    elif __unix__
#        define SUN /* either sparc or sun 3 */
#    endif
#endif


#if defined(RS6000) || defined(SGI_X) || defined(SUN) || defined(DEC)
#    define X_WINDOWS
#endif



#if defined(__STDC__) || defined(__cplusplus) || defined(IBM_PC)
#    define ANSI_COMPILER  1
#else
#    define ANSI_COMPILER  0
#endif

#if ANSI_COMPILER
#define CARGS(X)  X
#else
#define CARGS(X)  ()
#endif



#define INTERACTIVE_WIDGETS


#ifndef SUIT_PROTOTYPE
#    ifdef __cplusplus
#        define SUIT_PROTOTYPE extern "C"
#        define time_t long int
#    else
#        define SUIT_PROTOTYPE
#    endif
#endif


/* ------------------------------------------------------------------------------------------ */

#ifndef _Windows
     /* For the MS Windows version, these are global variables defined in
	initial.c rather than consant strings.  This is to consert static memory
	space. */
#define ACTIVE_DISPLAY 			"active display"
#define ALTERED 			"altered"
#define ANIMATED 			"animated"
#define ANY_KEYSTROKE_TRIGGERS 		"any keystroke triggers"
#define ARROWHEAD_ANGLE 		"arrowhead angle"
#define ARROWHEAD_LENGTH 		"arrowhead length"
#define AXIS_COLOR 			"axis color"
#define BACKGROUND_COLOR 		"background color"
#define BACKWARD_CHAR_KEY 		"backward char key"
#define BALL_SIZE 			"ball size"
#define BALL_X 				"ball x"
#define BALL_Y 				"ball y"
#define BEGINNING_OF_LINE_KEY 		"beginning of line key"
#define BEGINNING_OF_TEXT_KEY 		"beginning of text key"
#define BODY_COLOR 			"body color"
#define BORDER_COLOR 			"border color"
#define BORDER_RAISED 			"border raised"
#define BORDER_TYPE 			"border type"
#define BORDER_WIDTH 			"border width"
#define BUTTON_BACKGROUND_COLOR 	"button background color"
#define BUTTON_FOREGROUND_COLOR 	"button foreground color"
#define BUTTON_PRESSED 			"button pressed"
#define CAB_COLOR 			"cab color"
#define CACHE_USING_CANVAS 		"cache using canvas"
#define CALLBACK_FUNCTION 		"callback function"
#define CAN_BE_OPENED 			"can be opened"
#define CAN_BE_REDIRECTED 		"can be redirected"
#define CAN_COLOR 			"can color"
#define CHAINED_FROM_PROPERTY_TYPE 	"chained from property type"
#define CHAINED_TO_OBJECT 		"chained to object"
#define CHAINED_TO_PROPERTY_TYPE 	"chained to property type"
#define CHAINED_TO_PROPERTY 		"chained to property"
#define CHIP_BORDER 			"chip border"
#define CLIP_TO_VIEWPORT 		"clip to viewport"
#define CONNECTION_CALLBACK_FUNCTION 	"connection callback function"
#define CURRENT_DIRECTORY  		"current directory"
#define CURRENT_FILE                    "current file"
#define CURRENT_ROW 			"current row"
#define CURRENT_VALUE 			"current value"
#define CURSOR_COLOR 			"cursor color"
#define CURSOR_INDEX 			"cursor index"
#define CURSOR_STYLE 			"cursor style"
#define CUT_BUFFER 			"cut buffer"
#define DARKEN_BACKGROUND               "darken background"
#define DEFAULT_OBJECT_HEIGHT 		"default object height"
#define DEFAULT_OBJECT_WIDTH 		"default object width"
#define DELETE_CHAR_KEY 		"delete char key"
#define DELETE_ENTIRE_LINE_KEY 		"delete entire line key"
#define DIRECTION 			"direction"
#define DISABLED_COLOR 			"disabled color"
#define DISABLED 			"disabled"
#define DONE_CALLBACK_FUNCTION 		"done callback function"
#define DONE_EDITING_KEY 		"done editing key"
#define DRAW_AXIS_LINES 		"draw axis lines"
#define DRAW_BORDER_ON_INSIDE 		"draw border on inside"
#define DRAW_FILLED 			"draw filled"
#define DYNARRAY_DEBUG 			"dynarray debug"
#define END_OF_LINE_KEY 		"end of line key"
#define END_OF_TEXT_KEY 		"end of text key"
#define EXPAND_NAME 			"expand name"
#define FILE_FILTER 			"file filter"
#define FILL_TILE 			"fill tile"
#define FILLED 				"filled"
#define FINISHED     			"finished"
#define FONT 				"font"
#define FOREGROUND_COLOR 		"foreground color"
#define FORWARD_CHAR_KEY 		"forward char key"
#define FRAME_NAME 			"frame name"
#define FROM 				"from"
#define GOT_A_MOUSE_DOWN 		"got a mouse down"
#define GRAB_RETURN_KEY 		"grab return key"
#define GRANULARITY 			"granularity"
#define HAS_ARROW_HEAD 			"has arrow head"
#define HAS_ARROW 			"has arrow"
#define HAS_BACKGROUND 			"has background"
#define HAS_BORDER 			"has border"
#define HAS_RIM 			"has rim"
#define HAS_SECOND_HAND 		"has second hand"
#define HAS_TICK_MARKS 			"has tick marks"
#define HEAD_X 				"head x"
#define HEAD_Y 				"head y"
#define HELP_MESSAGE 			"help message"
#define HIGHLIGHT_BLOCK 		"highlight block"
#define HIGHLIGHT_COLOR 		"highlight color"
#define HOTKEY 				"hotkey"
#define INCREASE_CLOCKWISE 		"increase clockwise"
#define INPUT_SEQUENCE 			"input sequence"
#define INSIDE_COLOR 			"inside color"
#define INTERACTIVELY_CREATED 		"interactively created"
#define INTERMEDIATE_FEEDBACK 		"intermediate feedback"
#define JUSTIFICATION 			"justification"
#define KILL_LINE_KEY 			"kill line key"
#define LABEL_NAME 			"label name"
#define LABEL 				"label"
#define LAST_DRAWN_CURRENT_VALUE 	"last drawn current value"
#define LINE_SPACING 			"line spacing"
#define LINE_WIDTH 			"line width"
#define LIST 				"list"
#define MARGIN 				"margin"
#define MARK_COLOR 			"mark color"
#define MARK_END_INDEX 			"mark end index"
#define MARK_INDEX 			"mark index"
#define MAXIMIZED 			"maximized"
#define MAXIMUM_VALUE 			"maximum value"
#define MILITARY_TIME 			"military time"
#define MINIMIZED 			"minimized"
#define MINIMUM_VALUE 			"minimum value"
#define MINIMUM_VIEWPORT 		"minimum viewport"
#define MOVE_HEAD 			"move head"
#define NEEDLE_COLOR 			"needle color"
#define NEWLINE_KEY                     "newline key"
#define NEXT_LINE_KEY 			"next line key"
#define NUMBER_OF_CHILDREN 		"number of children"
#define NUMBER_OF_CONNECTIONS 		"number of connections"
#define NUMBER_OF_LINES 		"number of lines"
#define NUMBER_OF_SIDES 		"number of sides"
#define OPEN_LINE_KEY 			"open line key"
#define OUTLINE_COLOR 			"outline color"
#define PERCENT_FULL 			"percent full"
#define PIXELS_PER_SECOND 		"pixels per second"
#define PREVIOUS_TIME 			"previous time"
#define PREVIOUS_VALUE 			"previous value"
#define PREVIOUS_LINE_KEY 		"previous line key"
#define RAISED 				"raised"
#define READ_ONLY 			"read only"
#define REDUCE_NAME 			"reduce name"
#define REPAINT_KEY 			"repaint key"
#define RESTORED 			"restored"
#define ROUNDING_FACTOR 		"rounding factor"
#define SCREEN_DEPTH 			"screen depth"
#define SCREEN_HEIGHT 			"screen height"
#define SCREEN_WIDTH 			"screen width"
#define SCROLL_POSITION 		"scroll position"
#define SCROLL_DOWN_KEY 		"scroll down key"
#define SCROLL_UP_KEY 			"scroll up key"
#define SCROLLER 			"scroller"
#define SET_MARK_KEY 			"set mark key"
#define SHADOW_THICKNESS 		"shadow thickness"
#define SHOW_TEMPORARY_PROPERTIES 	"show temporary properties"
#define SHRINK_TO_FIT 			"shrink to fit"
#define SLIDING 			"sliding"
#define SPACING_GAP 			"spacing gap"
#define SPRINGINESS 			"springiness"
#define START_ANGLE 			"start angle"
#define STARTING_LINE			"starting line"
#define SUIT_PROPERTY_EDITOR 		"SUIT property editor"
#define SUIT_SYSTEM_FONT 		"SUIT system font"
#define TAB_KEY                         "tab key"
#define TAB_LENGTH 			"tab length"
#define TAIL_X 				"tail x"
#define TAIL_Y 				"tail y"
#define TEXT_COLOR 			"text color"
#define TEXT_SPACING 			"text spacing"
#define TILE_COLOR 			"tile color"
#define TILE_EXTENTS 			"tile extents"
#define TO 				"to"
#define VALID_COLORS 			"valid colors"
#define VALIDATION_FUNCTION 		"validation function"
#define VECTOR_X 			"vector X"
#define VECTOR_Y 			"vector Y"
#define VERTICAL_MENU_VIEWPORT 		"vertical menu viewport"
#define VIEWPORT 			"viewport"
#define VISIBLE_WITHIN_PROPERTY_EDITOR 	"visible within property editor"
#define VISIBLE 			"visible"
#define WALLPAPER 			"wallpaper"
#define WALLPAPER_FUNCTION 		"wallpaper function"
#define WHEEL_COLOR 			"wheel color"
#define WINDOW 				"window"
#define WIPE_BLOCK_KEY 			"wipe block key"
#define YANK_KEY 			"yank key"

#else
/* Windows Hack! */
/* These variable are declared in initial.c */
#ifndef WINDECLARED
extern char *ACTIVE_DISPLAY;
extern char *ALTERED;
extern char *ANIMATED;
extern char *ANY_KEYSTROKE_TRIGGERS;
extern char *ARROWHEAD_ANGLE;
extern char *ARROWHEAD_LENGTH;
extern char *AXIS_COLOR;
extern char *BACKGROUND_COLOR;
extern char *BACKWARD_CHAR_KEY;
extern char *BALL_SIZE;
extern char *BALL_X;
extern char *BALL_Y;
extern char *BEGINNING_OF_LINE_KEY;
extern char *BEGINNING_OF_TEXT_KEY;
extern char *BODY_COLOR;
extern char *BORDER_COLOR;
extern char *BORDER_RAISED;
extern char *BORDER_TYPE;
extern char *BORDER_WIDTH;
extern char *BUTTON_BACKGROUND_COLOR;
extern char *BUTTON_FOREGROUND_COLOR;
extern char *BUTTON_PRESSED;
extern char *CAB_COLOR;
extern char *CACHE_USING_CANVAS;
extern char *CALLBACK_FUNCTION;
extern char *CAN_BE_OPENED;
extern char *CAN_BE_REDIRECTED;
extern char *CAN_COLOR;
extern char *CHAINED_FROM_PROPERTY_TYPE;
extern char *CHAINED_TO_OBJECT;
extern char *CHAINED_TO_PROPERTY_TYPE;
extern char *CHAINED_TO_PROPERTY;
extern char *CHIP_BORDER;
extern char *CLIP_TO_VIEWPORT;
extern char *CONNECTION_CALLBACK_FUNCTION;
extern char *CURRENT_DIRECTORY;
extern char *CURRENT_FILE;
extern char *CURRENT_ROW;
extern char *CURRENT_VALUE;
extern char *CURSOR_COLOR;
extern char *CURSOR_INDEX;
extern char *CURSOR_STYLE;
extern char *CUT_BUFFER;
extern char *DARKEN_BACKGROUND;
extern char *DEFAULT_OBJECT_HEIGHT;
extern char *DEFAULT_OBJECT_WIDTH;
extern char *DELETE_CHAR_KEY;
extern char *DELETE_ENTIRE_LINE_KEY;
extern char *DIRECTION;
extern char *DISABLED_COLOR;
extern char *DISABLED;
extern char *DONE_CALLBACK_FUNCTION;
extern char *DONE_EDITING_KEY;
extern char *DRAW_AXIS_LINES;
extern char *DRAW_BORDER_ON_INSIDE;
extern char *DRAW_FILLED;
extern char *DYNARRAY_DEBUG;
extern char *END_OF_LINE_KEY;
extern char *END_OF_TEXT_KEY;
extern char *EXPAND_NAME;
extern char *FILE_FILTER;
extern char *FILL_TILE;
extern char *FILLED;
extern char *FINISHED;
extern char *FONT;
extern char *FOREGROUND_COLOR;
extern char *FORWARD_CHAR_KEY;
extern char *FRAME_NAME;
extern char *FROM;
extern char *GOT_A_MOUSE_DOWN;
extern char *GRAB_RETURN_KEY;
extern char *GRANULARITY;
extern char *HAS_ARROW_HEAD;
extern char *HAS_ARROW;
extern char *HAS_BACKGROUND;
extern char *HAS_BORDER;
extern char *HAS_RIM;
extern char *HAS_SECOND_HAND;
extern char *HAS_TICK_MARKS;
extern char *HEAD_X;
extern char *HEAD_Y;
extern char *HELP_MESSAGE;
extern char *HIGHLIGHT_BLOCK;
extern char *HIGHLIGHT_COLOR;
extern char *HOTKEY;
extern char *INCREASE_CLOCKWISE;
extern char *INPUT_SEQUENCE;
extern char *INSIDE_COLOR;
extern char *INTERACTIVELY_CREATED;
extern char *INTERMEDIATE_FEEDBACK;
extern char *JUSTIFICATION;
extern char *KILL_LINE_KEY;
extern char *LABEL_NAME;
extern char *LABEL;
extern char *LAST_DRAWN_CURRENT_VALUE;
extern char *LINE_SPACING;
extern char *LINE_WIDTH;
extern char *LIST;
extern char *MARGIN;
extern char *MARK_COLOR;
extern char *MARK_END_INDEX;
extern char *MARK_INDEX;
extern char *MAXIMIZED;
extern char *MAXIMUM_VALUE;
extern char *MILITARY_TIME;
extern char *MINIMIZED;
extern char *MINIMUM_VALUE;
extern char *MINIMUM_VIEWPORT;
extern char *MOVE_HEAD;
extern char *NEEDLE_COLOR;
extern char *NEWLINE_KEY;
extern char *NEXT_LINE_KEY;
extern char *NUMBER_OF_CHILDREN;
extern char *NUMBER_OF_CONNECTIONS;
extern char *NUMBER_OF_LINES;
extern char *NUMBER_OF_SIDES;
extern char *OPEN_LINE_KEY;
extern char *OUTLINE_COLOR;
extern char *PERCENT_FULL;
extern char *PIXELS_PER_SECOND;
extern char *PREVIOUS_TIME;
extern char *PREVIOUS_VALUE;
extern char *PREVIOUS_LINE_KEY;
extern char *RAISED;
extern char *READ_ONLY;
extern char *REDUCE_NAME;
extern char *REPAINT_KEY;
extern char *RESTORED;
extern char *ROUNDING_FACTOR;
extern char *SCREEN_DEPTH;
extern char *SCREEN_HEIGHT;
extern char *SCREEN_WIDTH;
extern char *SCROLL_POSITION;
extern char *SCROLL_DOWN_KEY;
extern char *SCROLL_UP_KEY;
extern char *SCROLLER;
extern char *SET_MARK_KEY;
extern char *SHADOW_THICKNESS;
extern char *SHOW_TEMPORARY_PROPERTIES;
extern char *SHRINK_TO_FIT;
extern char *SLIDING;
extern char *SPACING_GAP;
extern char *SPRINGINESS;
extern char *START_ANGLE;
extern char *STARTING_LINE;
extern char *SUIT_PROPERTY_EDITOR;
extern char *SUIT_SYSTEM_FONT;
extern char *TAB_KEY;
extern char *TAB_LENGTH;
extern char *TAIL_X;
extern char *TAIL_Y;
extern char *TEXT_COLOR;
extern char *TEXT_SPACING;
extern char *TILE_COLOR;
extern char *TILE_EXTENTS;
extern char *TO;
extern char *VALID_COLORS;
extern char *VALIDATION_FUNCTION;
extern char *VECTOR_X;
extern char *VECTOR_Y;
extern char *VERTICAL_MENU_VIEWPORT;
extern char *VIEWPORT;
extern char *VISIBLE_WITHIN_PROPERTY_EDITOR;
extern char *VISIBLE;
extern char *WALLPAPER;
extern char *WALLPAPER_FUNCTION;
extern char *WHEEL_COLOR;
extern char *WINDOW;
extern char *WIPE_BLOCK_KEY;
extern char *YANK_KEY;
#endif
#endif

/* ------------------------------------------------------------------------------------------ */



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
#include "dynarray.h"

#ifdef __unix__
#include <ctype.h>
#endif



#if defined(__cplusplus)
extern "C" {
#    include "srgp.h"
}
#else
#    include "srgp.h"
#endif



/* ---------------------------------------- G P ---------------------------------------- */

#include <time.h>
#ifdef __unix__
#    include <sys/time.h>
#    include <sys/types.h>
#    include <sys/times.h>
#    ifndef sgi
#        include <sys/timeb.h>
#    endif
#endif


#define STANDARD_CURSOR     0
#define PIRATE_CURSOR       1
#define WATCH_CURSOR        2
#define PROMPT_CURSOR       3
#define RIGHT_ARROW_CURSOR  4


typedef struct color_str {
    char *colorName;
    boolean blackOnMonochrome;
} GP_color;


#define BILEVEL_DISPLAY          (SRGP_inquireCanvasDepth()==1)
#define BLACK_ON_MONO            TRUE
#define WHITE_ON_MONO            FALSE


typedef struct {
    char *family;
    char *style;
    double pointSize;
} GP_font;


typedef struct {
    double x, y;
} GP_point;


typedef struct {
    GP_point bottom_left, top_right;
} GP_rectangle;



#if ANSI_COMPILER
    typedef enum {
	JUSTIFY_BOTTOM_LEFT, JUSTIFY_CENTER_LEFT, JUSTIFY_TOP_LEFT,
	JUSTIFY_BOTTOM_CENTER, JUSTIFY_CENTER, JUSTIFY_TOP_CENTER,
	JUSTIFY_BOTTOM_RIGHT, JUSTIFY_CENTER_RIGHT, JUSTIFY_TOP_RIGHT
    } GP_justification;
#else
    typedef int GP_justification;
#   define JUSTIFY_BOTTOM_LEFT     0
#   define JUSTIFY_CENTER_LEFT     1
#   define JUSTIFY_TOP_LEFT        2
#   define JUSTIFY_BOTTOM_CENTER   3
#   define JUSTIFY_CENTER          4
#   define JUSTIFY_TOP_CENTER      5
#   define JUSTIFY_BOTTOM_RIGHT    6
#   define JUSTIFY_CENTER_RIGHT    7
#   define JUSTIFY_TOP_RIGHT       8
#endif


typedef struct {
    time_t secs;
    time_t millisecs;
} GP_time;			/* time abstraction */



SUIT_PROTOTYPE GP_color	      GP_defColor     CARGS((char *name, boolean blackOnMonocrome));
SUIT_PROTOTYPE GP_font 	      GP_defFont      CARGS((char *family, char *style, double pointSize));
SUIT_PROTOTYPE GP_point       GP_defPoint     CARGS((double x, double y));
SUIT_PROTOTYPE GP_rectangle   GP_defRectangle CARGS((double x1, double y1, double x2, double y2));

SUIT_PROTOTYPE int 	      GP_mapHeight CARGS((double height));
SUIT_PROTOTYPE point 	      GP_mapPoint CARGS((GP_point pt));
SUIT_PROTOTYPE rectangle      GP_mapRectangle CARGS((GP_rectangle rect));
SUIT_PROTOTYPE int 	      GP_mapWidth CARGS((double width));
SUIT_PROTOTYPE int 	      GP_mapX CARGS((double x));
SUIT_PROTOTYPE int            GP_mapY CARGS((double y));
SUIT_PROTOTYPE point *        GP_mapPointArray CARGS((int vCount, GP_point * vertices));
SUIT_PROTOTYPE int *          GP_mapXArray CARGS((int vCount, double *vertices));
SUIT_PROTOTYPE int *          GP_mapYArray CARGS((int vCount, double *vertices));

SUIT_PROTOTYPE double         GP_unMapHeight CARGS((int height));
SUIT_PROTOTYPE GP_point       GP_unMapPoint CARGS((point pt));
SUIT_PROTOTYPE GP_rectangle   GP_unMapRectangle CARGS((rectangle rect));
SUIT_PROTOTYPE double         GP_unMapWidth CARGS((int width));
SUIT_PROTOTYPE double         GP_unMapX CARGS((int x));
SUIT_PROTOTYPE double         GP_unMapY CARGS((int y));

SUIT_PROTOTYPE void 	      GP_pushGraphicsState CARGS((void));
SUIT_PROTOTYPE void 	      GP_popGraphicsState CARGS((void));
SUIT_PROTOTYPE void 	      GP_setColor CARGS((GP_color color));
SUIT_PROTOTYPE void 	      GP_setCursor CARGS((int cursor));

#define       		      GP_setClipRectangle       SRGP_setClipRectangle
#define       		      GP_setFillBitmapPattern   SRGP_setFillBitmapPattern
#define       		      GP_setFillPixmapPattern   SRGP_setFillPixmapPattern
#define       		      GP_setFillStyle           SRGP_setFillStyle
#define       		      GP_setInputMode           SRGP_setInputMode
#define       		      GP_setLineStyle           SRGP_setLineStyle

#define       		      GP_setLocatorMeasure      SRGP_setLocatorMeasure
#define       		      GP_setMarkerStyle         SRGP_setMarkerStyle
#define       		      GP_setMarkerSize          SRGP_setMarkerSize
#define       		      GP_setPenBitmapPattern    SRGP_setPenBitmapPattern
#define       		      GP_setPenPixmapPattern    SRGP_setPenPixmapPattern
#define       		      GP_setPenStyle            SRGP_setPenStyle
#define       		      GP_setPlaneMask           SRGP_setPlaneMask
#define       		      GP_setWriteMode           SRGP_setWriteMode

SUIT_PROTOTYPE void 	      GP_setWindow CARGS((GP_rectangle newWorld));
SUIT_PROTOTYPE void 	      GP_setViewport CARGS((rectangle newView));
SUIT_PROTOTYPE void 	      GP_setLineWidth CARGS((int newValue));
SUIT_PROTOTYPE int 	      GP_getColorIndex CARGS((char *name));
SUIT_PROTOTYPE char *	      GP_getColorName CARGS((int index));
SUIT_PROTOTYPE void  	      GP_registerForColorAllocation CARGS((void (*func)(void)));

#define       		      GP_inquireActiveCanvas   SRGP_inquireActiveCanvas
#define       		      GP_inquireCanvasExtent   SRGP_inquireCanvasExtent
#define       		      GP_inquireCanvasSize     SRGP_inquireCanvasSize
#define       		      GP_inquireCanvasDepth    SRGP_inquireCanvasDepth
#define       		      GP_inquireColorTable     SRGP_inquireColorTable

SUIT_PROTOTYPE void 	      GP_inquireTextExtent CARGS((char *str, double *width,
							  double *height, double *descenderHeight));
SUIT_PROTOTYPE int 	      GP_numColorsAllocated CARGS((void));
SUIT_PROTOTYPE int 	      GP_setFont CARGS((GP_font newfont));

#define       		      GP_createCanvas          SRGP_createCanvas
#define       		      GP_useCanvas             SRGP_useCanvas
#define       		      GP_deleteCanvas          SRGP_deleteCanvas

SUIT_PROTOTYPE void 	      InitializeColors CARGS((int screenDepth));
SUIT_PROTOTYPE void 	      InitializeCursors CARGS((void));
SUIT_PROTOTYPE void 	      InitializeFonts CARGS((void));

#define       		      GP_loadBitmapPatterns    SRGP_loadBitmapPatterns
#define      		      GP_loadColorTable        SRGP_loadColorTable
#define       		      GP_loadCommonColor       SRGP_loadCommonColor
#define       		      GP_loadCursorTable       SRGP_loadCursorTable
#define       		      GP_loadFontTable         SRGP_loadFontTable
#define       		      GP_beep                  SRGP_beep
     
SUIT_PROTOTYPE void 	      GP_drawRectangle CARGS((GP_rectangle rect));
SUIT_PROTOTYPE void 	      GP_ellipse CARGS((GP_rectangle rect));
SUIT_PROTOTYPE void 	      GP_ellipseArc CARGS((GP_rectangle rect, double a1, double a2));
SUIT_PROTOTYPE void 	      GP_ellipseCoord CARGS((double x1, double y1, double x2, double y2));
SUIT_PROTOTYPE void 	      GP_fillEllipse CARGS((GP_rectangle rect));
SUIT_PROTOTYPE void 	      GP_fillEllipseArc CARGS((GP_rectangle rect, double a1, double a2));
SUIT_PROTOTYPE void 	      GP_fillEllipseCoord CARGS((double x1, double y1, double x2, double y2));
SUIT_PROTOTYPE void 	      GP_fillRectanglePt CARGS((GP_point bottomLeft, GP_point topRight));
SUIT_PROTOTYPE void 	      GP_fillRectangle CARGS((GP_rectangle rect));
SUIT_PROTOTYPE void 	      GP_fillPolygon CARGS((int vCount, GP_point *vertices));
SUIT_PROTOTYPE void 	      GP_fillPolygonCoord CARGS((int vCount, double *xList, double *yList));
SUIT_PROTOTYPE void 	      GP_line CARGS((GP_point pt1, GP_point pt2));

#define   		      GP_fillRectangleCoord(LX,BY,RX,TY) SRGP_fillRectangleCoord(GP_mapX(LX),\
											 GP_mapY(BY),\
											 GP_mapX(RX),\
											 GP_mapY(TY))
#define   		      GP_lineCoord(X1,Y1,X2,Y2)   SRGP_lineCoord(GP_mapX(X1), GP_mapY(Y1), \
									 GP_mapX(X2), GP_mapY(Y2))
#define   		      GP_marker(PNT)              SRGP_marker(GP_mapPoint(PNT))
#define   		      GP_markerCoord(X,Y)         SRGP_markerCoord(GP_mapX(X), GP_mapY(Y))
#define   		      GP_pointCoord(X,Y)          SRGP_pointCoord(GP_mapX(X), GP_mapY(Y))
#define   		      GP_rectangleCoord(LX,BY,RX,TY) SRGP_rectangleCoord(GP_mapX(LX), \
										 GP_mapY(BY), \
										 GP_mapX(RX), \
										 GP_mapY(TY))
#define   		      GP_refresh                  SRGP_refresh

SUIT_PROTOTYPE void 	      GP_polygon CARGS((int vCount, GP_point *vertices));
SUIT_PROTOTYPE void 	      GP_polygonCoord CARGS((int vCount, double *xList, double *yList));
SUIT_PROTOTYPE void 	      GP_polyLine CARGS((int vCount, GP_point *vertices));
SUIT_PROTOTYPE void 	      GP_polyLineCoord CARGS((int vxCount, double *xList, double *yList));
SUIT_PROTOTYPE void 	      GP_polyMarker CARGS((int vCount, GP_point *vertices));
SUIT_PROTOTYPE void 	      GP_polyMarkerCoord CARGS((int vCount, double *xs, double *ys));
SUIT_PROTOTYPE void 	      GP_polyPoint CARGS((int vCount, GP_point *vertices));
SUIT_PROTOTYPE void 	      GP_polyPointCoord CARGS((int vCount, double *xs, double *ys));
SUIT_PROTOTYPE void 	      GP_rectanglePt CARGS((GP_point bottomLeft, GP_point topRight));
SUIT_PROTOTYPE 		      GP_rectangle GP_text CARGS((GP_point pt, char *string));

SUIT_PROTOTYPE void 	      GP_unbeveledBorder CARGS((rectangle box, GP_color color,
							boolean raised, int width));
SUIT_PROTOTYPE void 	      GP_beveledBorder CARGS((rectangle box, GP_color color, boolean raised,
						      int width));
SUIT_PROTOTYPE void 	      GP_fancyBeveledBorder CARGS((rectangle box, GP_color color, int width));
SUIT_PROTOTYPE void 	      GP_beveledBox CARGS((rectangle box, GP_color color, boolean raised,
						   int width));
SUIT_PROTOTYPE void 	      GP_beveledDiamond CARGS((rectangle box, GP_color color, boolean raised,
						       int width));

SUIT_PROTOTYPE void 	      GP_beveledTriangleNorth CARGS((GP_point forward, GP_point left,
							     GP_point right, GP_color color,
							     boolean raised, int thickness));
SUIT_PROTOTYPE void 	      GP_beveledTriangleSouth CARGS((GP_point forward, GP_point left,
							     GP_point right, GP_color color,
							     boolean raised, int thickness));
SUIT_PROTOTYPE void 	      GP_beveledTriangleEast CARGS((GP_point forward, GP_point left,
							    GP_point right, GP_color col,
							    boolean raised, int thickness));
SUIT_PROTOTYPE void 	      GP_beveledTriangleWest CARGS((GP_point forward, GP_point left,
							    GP_point right, GP_color col,
							    boolean raised, int thickness));

SUIT_PROTOTYPE void 	      GP_justifyText CARGS((char *str, GP_justification just));
SUIT_PROTOTYPE void 	      GP_justifyTextInRectangle CARGS((char *text, GP_justification just,
							       GP_rectangle rect));

SUIT_PROTOTYPE GP_time 	      GP_getCurrentTime CARGS((void));
SUIT_PROTOTYPE time_t 	      GP_timeDifference CARGS((GP_time time1, GP_time time2));
SUIT_PROTOTYPE void 	      GP_convertTime CARGS((GP_time theTime, int *hour, int *minute,
						    int *second, int *millisecond));

SUIT_PROTOTYPE char **	      GP_possibleFonts CARGS((void));
SUIT_PROTOTYPE char **	      GP_possibleStyles CARGS((void));

SUIT_PROTOTYPE void 	      GP_registerSpecialCharacter CARGS((char *character, GP_rectangle (*funct) (GP_point, char *)));
SUIT_PROTOTYPE void 	      GP_registerStyle CARGS((char *character, GP_rectangle (*funct) (GP_point, char *, char *)));
SUIT_PROTOTYPE void 	      GP_inquireTextExtentWithoutMapping CARGS((char *str, int *width, int *ascent, int *descent));

SUIT_PROTOTYPE GP_color       GP_defColorRGB CARGS((unsigned short red, unsigned short blue, unsigned short green,
						    boolean blackOnMonocrome));
SUIT_PROTOTYPE void 	      GP_describeColor CARGS((GP_color color, unsigned short *red, unsigned short *green,
						      unsigned short *blue));
SUIT_PROTOTYPE void 	      GP_replaceColor CARGS((GP_color oldColor, GP_color newColor));

SUIT_PROTOTYPE GP_color       GP_getShadowColor CARGS((GP_color color));
SUIT_PROTOTYPE GP_color       GP_getDepthColor CARGS((GP_color color));
SUIT_PROTOTYPE GP_color       GP_getHighlightColor CARGS((GP_color color));




/* ---------------------------------------- SUIT ---------------------------------------- */

typedef void *Pointer;


typedef int SUIT_springiness;
#define VERTICAL_SPRINGINESS             1
#define HORIZONTAL_SPRINGINESS           2
#define ABOVE_SPRINGINESS                4
#define BELOW_SPRINGINESS                8
#define LEFT_SPRINGINESS                16
#define RIGHT_SPRINGINESS               32
#define NO_SPRINGINESS  0
#define ALL_SPRINGINESS VERTICAL_SPRINGINESS|HORIZONTAL_SPRINGINESS|ABOVE_SPRINGINESS|BELOW_SPRINGINESS|LEFT_SPRINGINESS|RIGHT_SPRINGINESS


typedef struct {
    DynArray choices;  /* of (char *) */
    int	currentChoice; 	
} SUIT_enum;


typedef DynArray SUIT_textList;	/* DynArray of (char *) */


#ifndef MAX
#    define MAX(A,B)	( (A) > (B) ? (A) : (B) )
#endif
#ifndef MIN
#    define MIN(A,B)	( (A) < (B) ? (A) : (B) )
#endif     
#ifndef ABS
#    define ABS(X)	( (X) > 0 ? (X) : -(X) )
#endif     
#ifndef AVG
#    define AVG(A,B)	( ( (A) + (B) ) / 2 )
#endif


#define PRIVATE		static
#define	VOIDNULL	( (void *) NULL )
#define	CHARNULL	( (char *) NULL )


/** SUIT TYPE DECLARATIONS **/
typedef struct SUIT_class_str {
    char *name;
    DynArray props;		/* of property */
    DynArray interestCallbacks;	/* of SUIT_objectInterestCallback */
} SUIT_class;


typedef struct object_str {
    char *name;
    SUIT_class *classInfo;
    unsigned int open               : 1;
    unsigned int selected           : 1;
    unsigned int has_been_painted   : 1;
    unsigned int redisplay_required : 1;
    unsigned int optimized_paint    : 1;
    unsigned int mark_redisplay     : 1;
    unsigned int permanent          : 1;
    point offset;
    DynArray displays;		/* dynamic array of type SUIT_display */
    DynArray props;		/* dynamic array of type property */
    DynArray children;		/* dynamic array of type SUIT_object */
    DynArray objectInterest;	/* dynamic array of type InterestCallback */
    struct object_str *parent;
} SUIT_objectRecord, *SUIT_object;



#if ANSI_COMPILER
    typedef enum { MOUSE_DOWN, MOUSE_UP, MOUSE_MOTION, CLICK, KEYSTROKE } SUIT_eventType;
#else
    typedef int SUIT_eventType;
#   define MOUSE_DOWN 0
#   define MOUSE_UP 1
#   define MOUSE_MOTION 2
#   define CLICK 3
#   define KEYSTROKE 4
#endif


typedef struct input_event_str {
    SUIT_eventType type;
    GP_point worldLocation;
    point relativePixelLocation;
    char keyboard;
    int button;
    deluxe_locator_measure locator;
} SUIT_event;


typedef void (*SUIT_functionPointer) CARGS((void));
typedef void (*SUIT_callbackFunctionPtr) CARGS((SUIT_object object));

typedef SUIT_object (*SUIT_trapperPtr) CARGS((SUIT_object object, SUIT_event *event));
typedef void (*SUIT_objectInterestCallback) CARGS((SUIT_object object, char *propertyName, char *propertyType, Pointer oldValue, Pointer newValue));

typedef rectangle SUIT_viewport;
typedef GP_rectangle SUIT_window;

typedef void (*SUIT_hitProcedure) CARGS((SUIT_object object, SUIT_event event));
typedef void (*SUIT_paintProcedure) CARGS((SUIT_object object));


#if ANSI_COMPILER
    typedef enum { GLOBAL, CLASS, OBJECT } SUIT_level;
#else
    typedef int SUIT_level;
#   define GLOBAL 0
#   define CLASS 1
#   define OBJECT 2
#endif


#if ANSI_COMPILER
    typedef enum { TEMPORARY, PERMANENT } SUIT_permanence;
#else
    typedef int  SUIT_permanence;
#   define TEMPORARY 0
#   define PERMANENT 1
#endif

    
#define NORMAL_EXIT TRUE	/* Write hints file. */
#define ABORT FALSE    	        /* Abort without writing a new hints file. */


#if ANSI_COMPILER
    typedef enum { DO_NOT_SAVE_SUI_FILE = 0, SAVE_SUI_FILE = 1 } SUIT_saveStatus;
    typedef enum { DO_NOT_EXIT_APPLICATION = 0, EXIT_APPLICATION = 1} SUIT_exitStatus;
#else
    typedef int SUIT_saveStatus;
    typedef int SUIT_exitStatus;
#   define DO_NOT_SAVE_SUI_FILE     0
#   define SAVE_SUI_FILE            1
#   define DO_NOT_EXIT_APPLICATION  0
#   define EXIT_APPLICATION         1
#endif


typedef void (*PropedFunction) CARGS((SUIT_object));
typedef SUIT_object (*SUIT_createProcedure) CARGS((char *name));


#if ANSI_COMPILER
    typedef enum { NEVER, WHILE_MOUSE_DOWN, UNTIL_MOUSE_UP } SUIT_mouseMotion;
#else
    typedef int SUIT_mouseMotion;
#   define NEVER 0
#   define WHILE_MOUSE_DOWN  1 
#   define UNTIL_MOUSE_UP 2
#endif


     
#if ANSI_COMPILER
    typedef enum { REPLY_NO=0, REPLY_YES=1, REPLY_CANCEL=2, REPLY_OK=3, REPLY_BUTTON1=1, REPLY_BUTTON2=0 } Reply;
#else
    typedef int Reply;
#   define REPLY_NO 0
#   define REPLY_YES 1
#   define REPLY_CANCEL 2
#   define REPLY_OK 3
#   define REPLY_BUTTON1 1
#   define REPLY_BUTTON2 0
#endif


typedef boolean (*SUIT_validationFunction) CARGS((SUIT_object object));


typedef struct {
    char *name;
    Pointer (*convertFromAscii) CARGS((char *string, boolean *error));
    char *(*convertToAscii) CARGS((Pointer value));
    int (*compare) CARGS((Pointer value1, Pointer value2));
    void (*destroy) CARGS((Pointer value));
    Pointer (*copy) CARGS((Pointer value));
    Pointer (*defaultValue) CARGS((void));
    char *widgetClass;
} SUIT_type;


typedef void (*SUIT_propertyCallback) CARGS((SUIT_level level, char *classOrObjectName, char *propertyName,
					     char *propertyType, Pointer value, boolean permanent));




extern void si_assertionHasFailed CARGS((char *message, int lineNumber, char *fileName));

#ifdef _Windows
     /* This is a hack to conserve static memory space that the assertion strings
	would otherwise use up. */
#define ASSERT(TEST,MESSAGE) \
	if ( !(TEST) )  si_assertionHasFailed("", 0, "");
#else

#define ASSERT(TEST,MESSAGE)	     				\
    if ( !(TEST) )  {						\
        char mes[1000];	             				\
	sprintf MESSAGE;					\
	si_assertionHasFailed(mes, __LINE__, __FILE__);		\
    }
#endif



#define 		      OBJECT_NAME(O)         	    (((O)==NULL)? "NULL" : (O)->name)
#define 		      OBJECT_CLASS(O)        	    (((O)==NULL)? "NULL" : (O)->classInfo->name)
#define 		      OBJECT_VIEWPORT(O)            (SUIT_getViewport((O),"viewport"))
#define 		      OBJECT_WINDOW(O)              (SUIT_getWindow((O),"window"))
#define 		      OBJECT_OPEN(O) 	            ((O)->open)
#define 		      OBJECT_SELECTED(O)            ((O)->selected)
#define 		      OBJECT_PERMANENT(O)           ((O)->permanent)
#define 		      OBJECT_OPTIMIZED(O)           ((O)->optimized_paint)



SUIT_PROTOTYPE char *	      SUIT_getEnumSelection CARGS((SUIT_enum e));
SUIT_PROTOTYPE void 	      SUIT_setEnumSelection CARGS((SUIT_enum *e, char *choice));
SUIT_PROTOTYPE SUIT_enum      SUIT_defEnum CARGS((char *currentChoice, int numChoices, char *choices[]));


SUIT_PROTOTYPE void 	      SUIT_appendToTextList CARGS((SUIT_textList list, char *str));
SUIT_PROTOTYPE void 	      SUIT_addToTextList CARGS((SUIT_textList list, int beforeIndex, char *str));
SUIT_PROTOTYPE void 	      SUIT_deleteFromTextList CARGS((SUIT_textList list, int index));
SUIT_PROTOTYPE int 	      SUIT_sizeOfTextList CARGS((SUIT_textList list));
SUIT_PROTOTYPE void 	      SUIT_sortTextList CARGS((SUIT_textList list));
SUIT_PROTOTYPE char *	      SUIT_itemInTextList CARGS((SUIT_textList list, int index));
SUIT_PROTOTYPE SUIT_textList  SUIT_defTextList CARGS((char *list[], int numItems));
SUIT_PROTOTYPE SUIT_textList  SUIT_copyTextList CARGS((SUIT_textList list));
SUIT_PROTOTYPE void 	      SUIT_destroyTextList CARGS((SUIT_textList list));
#define SUIT_freeTextList(LIST)    SUIT_destroyTextList(LIST)


SUIT_PROTOTYPE void 	      SUIT_init CARGS((char *programName));
SUIT_PROTOTYPE void 	      SUIT_initFromCode CARGS((char *programName));
SUIT_PROTOTYPE void 	      SUIT_deluxeInit CARGS((int *argc, char *argv[]));
SUIT_PROTOTYPE void 	      SUIT_printCommandLineOptions CARGS((void));
SUIT_PROTOTYPE void 	      SUIT_clearScreen CARGS((void));
SUIT_PROTOTYPE void 	      SUIT_writeSUIFile CARGS((char *fileName));
#define 		      SUIT_writeHintsFile(FILENAME)   SUIT_writeSUIFile(FILENAME)
SUIT_PROTOTYPE void 	      SUIT_done CARGS((SUIT_saveStatus saveStat, SUIT_exitStatus exitStat));
SUIT_PROTOTYPE SUIT_viewport  SUIT_getScreenViewport CARGS((void));
SUIT_PROTOTYPE void 	      SUIT_establishPropertyEditor CARGS((PropedFunction funct));
SUIT_PROTOTYPE void 	      SUIT_redisplayRequiredInRegion CARGS((SUIT_viewport rect));
#define 		      SUIT_redrawSection(VIEWPORT)  SUIT_redisplayRequiredInRegion(VIEWPORT)


SUIT_PROTOTYPE SUIT_object    SUIT_createObject CARGS((char *name, char *className));
SUIT_PROTOTYPE void 	      SUIT_destroyObject CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_eraseObject CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_addDisplayToObject CARGS((SUIT_object object, char *displayName,
							     SUIT_hitProcedure hitproc, SUIT_paintProcedure paintproc));
SUIT_PROTOTYPE void 	      SUIT_addEmployeeToDisplay CARGS((SUIT_object object, char *displayName, SUIT_object employee));
SUIT_PROTOTYPE void 	      SUIT_addChildToObject CARGS((SUIT_object object, SUIT_object child));
SUIT_PROTOTYPE SUIT_object    SUIT_getParent CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_selectObject CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_deselectObject CARGS((SUIT_object object)); 
SUIT_PROTOTYPE void 	      SUIT_bringToFront CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_sendToBack CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_cycleObject CARGS((SUIT_object object));
SUIT_PROTOTYPE SUIT_object    SUIT_mapPointToObject CARGS((point pt));
SUIT_PROTOTYPE boolean 	      SUIT_pointInObject CARGS((SUIT_object object, int x, int y));
SUIT_PROTOTYPE void 	      SUIT_borderObject CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_backgroundAndBorderObject CARGS((SUIT_object object));
SUIT_PROTOTYPE SUIT_object    SUIT_name CARGS((char *name));
#define                       SUIT_objectNamed(NAME)  SUIT_name(NAME)
SUIT_PROTOTYPE void 	      SUIT_allObjectsRequireRedisplay CARGS((char *className));
SUIT_PROTOTYPE SUIT_object    SUIT_dummyObjectInClass CARGS((char *className));
SUIT_PROTOTYPE void 	      SUIT_changeHeightPreservingRatio CARGS((SUIT_object object, int height));
SUIT_PROTOTYPE void 	      SUIT_changeWidthPreservingRatio CARGS((SUIT_object object, int width));
SUIT_PROTOTYPE void 	      SUIT_changeObjectSize CARGS((SUIT_object object, int width, int height));

    
SUIT_PROTOTYPE char *         SUIT_levelName CARGS((SUIT_level level));
SUIT_PROTOTYPE char *         SUIT_eventName CARGS((SUIT_event evt));

SUIT_PROTOTYPE void	      SUIT_setProperty CARGS((SUIT_object object, char *propertyName, char *propertyType,
						      Pointer propertyPtr, SUIT_level level));
SUIT_PROTOTYPE void           SUIT_setInteger CARGS((SUIT_object object, char *name, int value));
SUIT_PROTOTYPE void           SUIT_setFunctionPointer CARGS((SUIT_object object, char *name, SUIT_functionPointer value));
SUIT_PROTOTYPE void           SUIT_setDouble CARGS((SUIT_object object, char *name, double value));
SUIT_PROTOTYPE void           SUIT_setText CARGS((SUIT_object object, char *name, char *value));
SUIT_PROTOTYPE void           SUIT_setFont CARGS((SUIT_object object, char *name, GP_font value));
SUIT_PROTOTYPE void           SUIT_setColor CARGS((SUIT_object object, char *name, GP_color value));
SUIT_PROTOTYPE void           SUIT_setViewport CARGS((SUIT_object object, char *name, SUIT_viewport value));
SUIT_PROTOTYPE void           SUIT_setWindow CARGS((SUIT_object object, char *name, SUIT_window value));
SUIT_PROTOTYPE void 	      SUIT_setObject CARGS((SUIT_object object, char *name, SUIT_object value));
SUIT_PROTOTYPE void 	      SUIT_setDynArray CARGS((SUIT_object object, char *name, DynArray value));
SUIT_PROTOTYPE void 	      SUIT_setSpringiness CARGS((SUIT_object object, char *name, SUIT_springiness value));
SUIT_PROTOTYPE void 	      SUIT_setEnum CARGS((SUIT_object object, char *name, SUIT_enum value));
SUIT_PROTOTYPE void 	      SUIT_setBoolean CARGS((SUIT_object object, char *name, boolean value));
SUIT_PROTOTYPE void 	      SUIT_setTextList CARGS((SUIT_object object, char *name, SUIT_textList value));
SUIT_PROTOTYPE void 	      SUIT_setEnumString CARGS((SUIT_object object, char *propertyName, char *enumString));

    
SUIT_PROTOTYPE void 	      SUIT_deluxeSetInteger CARGS((SUIT_object object, char *name, int value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetFunctionPointer CARGS((SUIT_object object, char *name, SUIT_functionPointer value,
								   SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetBoolean CARGS((SUIT_object object, char *name, boolean value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetDouble CARGS((SUIT_object object, char *name, double value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetText CARGS((SUIT_object object, char *name, char *value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetFont CARGS((SUIT_object object, char *name, GP_font value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetColor CARGS((SUIT_object object, char *name, GP_color value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetViewport CARGS((SUIT_object object,char *name, SUIT_viewport value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetWindow CARGS((SUIT_object object, char *name, SUIT_window value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetObject CARGS((SUIT_object object, char *name, SUIT_object value,SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetDynArray CARGS((SUIT_object object, char *name, DynArray value,SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetSpringiness CARGS((SUIT_object object, char *name, SUIT_springiness value,
							       SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetEnum CARGS((SUIT_object object, char *name, SUIT_enum value, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetTextList CARGS((SUIT_object object, char *name, SUIT_textList value,
							    SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_deluxeSetEnumString CARGS((SUIT_object object, char *propertyName, char *enumString,
							      SUIT_level level));

    
SUIT_PROTOTYPE Pointer 	      SUIT_getProperty CARGS((SUIT_object object, char *propertyName, char *propertyType));
SUIT_PROTOTYPE int 	      SUIT_getInteger CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE SUIT_functionPointer SUIT_getFunctionPointer CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE boolean 	      SUIT_getBoolean CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE double 	      SUIT_getDouble CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE char *	      SUIT_getText CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE GP_font 	      SUIT_getFont CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE GP_color       SUIT_getColor CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE SUIT_viewport  SUIT_getViewport CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE SUIT_window    SUIT_getWindow CARGS((SUIT_object object,char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_getObject CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE DynArray       SUIT_getDynArray CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE SUIT_springiness SUIT_getSpringiness CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE SUIT_enum      SUIT_getEnum CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE SUIT_textList  SUIT_getTextList CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE char *	      SUIT_getEnumString CARGS((SUIT_object object, char *propertyName));


SUIT_PROTOTYPE Pointer   SUIT_deluxeGetProperty
                                    CARGS((SUIT_object object,
					   char *propertyName,
					   char *propertyType,
					   SUIT_level level));

SUIT_PROTOTYPE int 	      SUIT_deluxeGetInteger
                                    CARGS((SUIT_object object,
					   char *name,
					   SUIT_level level));

SUIT_PROTOTYPE SUIT_functionPointer SUIT_deluxeGetFunctionPointer CARGS((SUIT_object object,
									 char *name,
									 SUIT_level level));

SUIT_PROTOTYPE boolean 	      SUIT_deluxeGetBoolean CARGS((SUIT_object object,
							   char *name,
							   SUIT_level level));

SUIT_PROTOTYPE double 	      SUIT_deluxeGetDouble CARGS((SUIT_object object,
							  char *name,
							  SUIT_level level));
					  
SUIT_PROTOTYPE char *	      SUIT_deluxeGetText CARGS((SUIT_object object,
							char *name,
							SUIT_level level));

SUIT_PROTOTYPE GP_font 	      SUIT_deluxeGetFont CARGS((SUIT_object object,
							char *name,
							SUIT_level level));

SUIT_PROTOTYPE GP_color       SUIT_deluxeGetColor CARGS((SUIT_object object,
							 char *name,
							 SUIT_level level));

SUIT_PROTOTYPE SUIT_viewport  SUIT_deluxeGetViewport CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE SUIT_window    SUIT_deluxeGetWindow CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE SUIT_object    SUIT_deluxeGetObject CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE DynArray       SUIT_deluxeGetDynArray CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE SUIT_springiness SUIT_deluxeGetSpringiness CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE SUIT_enum      SUIT_deluxeGetEnum CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE SUIT_textList  SUIT_deluxeGetTextList CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE char *	      SUIT_deluxeGetEnumString CARGS((SUIT_object object, char *propertyName, SUIT_level level));
    
SUIT_PROTOTYPE boolean 	      SUIT_propertyExists CARGS((SUIT_object obj, char *propertyName, char *propertyType,
							 SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_makePropertyPermanent CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_makePropertyTemporary CARGS((SUIT_object object, char *name, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_makeObjectTemporary CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_makeObjectPermanent CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_eraseProperty CARGS((SUIT_object object, char *propertyName, SUIT_level level));
    
SUIT_PROTOTYPE void 	      SUIT_registerType CARGS((char *name,
						       Pointer (*readProcedure) (char *buffer, boolean *error),
						       char *(*writeProcedure) (Pointer value),
						       int (*compareProcedure) (Pointer value1, Pointer value2),
						       void (*destroyProcedure) (Pointer value),
						       Pointer (*copyProcedure) (Pointer value),
						       Pointer (*defaultProcedure) (void),
						       char *widgetClass));
    
SUIT_PROTOTYPE void 	      SUIT_beginDisplay CARGS((void));
SUIT_PROTOTYPE void 	      SUIT_checkAndProcessInput CARGS((int numberOfSecs));
#define SUIT_processInput(TIME)   SUIT_checkAndProcessInput(TIME)
SUIT_PROTOTYPE void 	      SUIT_redisplayRequired CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_redisplayNotRequired CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_performRedisplay CARGS((void));
    

SUIT_PROTOTYPE point 	      SUIT_drag CARGS((void (*graphicsCallback) (point pt)));
SUIT_PROTOTYPE point 	      SUIT_dragText CARGS((char *text));
SUIT_PROTOTYPE point 	      SUIT_dragTextWithOffset CARGS((char *text, int x, int y));
SUIT_PROTOTYPE void 	      SUIT_iterateOverObjects CARGS((void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE SUIT_viewport  SUIT_moveRectangle CARGS((SUIT_viewport original, point start, boolean allowOffScreen));
SUIT_PROTOTYPE SUIT_viewport  SUIT_resizeRectangle CARGS((SUIT_viewport original));
    

#if defined(MACINTOSH) || defined(RS6000) || defined(DEC)
SUIT_PROTOTYPE char *strdup CARGS((char *));
#elif defined(_Windows)
#define strdup winStrdup
 /* This function is in suitmem.c */
SUIT_PROTOTYPE char *strdup CARGS((char *));
#endif

SUIT_PROTOTYPE int 	      SUIT_stringsMatch CARGS((char *str1, char *str2));
SUIT_PROTOTYPE int 	      SUIT_caseInsensitiveCompare CARGS((char *str1, char *str2));
SUIT_PROTOTYPE boolean 	      SUIT_caseInsensitiveMatch CARGS((char *str1, char *str2));
SUIT_PROTOTYPE int 	      SUIT_stringContains CARGS((char *str1, char *str2));
#define 		      SUIT_createSafeString(STRING)  strdup(STRING)
#define 		      SUIT_copyString(STRING)        strdup(STRING)

SUIT_PROTOTYPE Pointer 	      SUIT_copyData CARGS((Pointer ptr, int length));
#define 		      SUIT_createSafeData(POINTER,LENGTH)  SUIT_copyData(POINTER,LENGTH)

    
SUIT_PROTOTYPE boolean 	      SUIT_viewportsEqual CARGS((SUIT_viewport first, SUIT_viewport second));
SUIT_PROTOTYPE void *	      SUIT_convertType CARGS((void *value, char *fromType, char *toType));
    

SUIT_PROTOTYPE void 	      SUIT_hitObject CARGS((SUIT_object object, SUIT_event event));
SUIT_PROTOTYPE void 	      SUIT_paintObject CARGS((SUIT_object object));
SUIT_PROTOTYPE SUIT_event     SUIT_adjustEventForObject CARGS((SUIT_event starter, SUIT_object parent, SUIT_object child));
SUIT_PROTOTYPE SUIT_viewport  SUIT_adjustForSpringiness CARGS((SUIT_viewport parentsOldViewport, SUIT_viewport parentsNewViewport,
							       SUIT_viewport childsOldViewport, SUIT_springiness springiness));
SUIT_PROTOTYPE boolean 	      SUIT_viewportsOverlap CARGS((SUIT_viewport rect, SUIT_viewport viewport));

SUIT_PROTOTYPE void 	      SUIT_registerClass CARGS((char *ClassName, SUIT_createProcedure createProc, char *helpText));
SUIT_PROTOTYPE int 	      SUIT_numberOfChildren CARGS((SUIT_object object));
SUIT_PROTOTYPE SUIT_object    SUIT_getChild CARGS((SUIT_object object, int whichChild));
SUIT_PROTOTYPE DynArray       SUIT_getChildren CARGS((SUIT_object object));
SUIT_PROTOTYPE SUIT_object    SUIT_getSibling CARGS((SUIT_object obj, char* siblingName));
SUIT_PROTOTYPE int 	      SUIT_numberOfEmployees CARGS((SUIT_object object, char *displayName));
SUIT_PROTOTYPE SUIT_object    SUIT_getEmployee CARGS((SUIT_object object, char *displayName, int whichEmployee));
SUIT_PROTOTYPE DynArray       SUIT_getEmployees CARGS((SUIT_object object, char *display));
SUIT_PROTOTYPE void 	      SUIT_registerTrapper CARGS((SUIT_trapperPtr trapper));
SUIT_PROTOTYPE void 	      SUIT_unregisterTrapper CARGS((void));
SUIT_PROTOTYPE void 	      SUIT_removeChild CARGS((SUIT_object child));
SUIT_PROTOTYPE void 	      SUIT_removeEmployee CARGS((char *display, SUIT_object employee));
    
SUIT_PROTOTYPE void 	      SUIT_reportMouseMotion CARGS((SUIT_object object, SUIT_mouseMotion type));
    
SUIT_PROTOTYPE void 	      SUIT_passEventDown CARGS((SUIT_object object, SUIT_event event));

SUIT_PROTOTYPE void 	      SUIT_limitedCheckAndProcessInput CARGS((int numberOfSecs, DynArray activeObjects));
SUIT_PROTOTYPE void 	      SUIT_beginStandardApplication CARGS((void));
    
SUIT_PROTOTYPE void 	      SUIT_suspendMarkingRedisplay CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_resumeMarkingRedisplay CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_redrawObjectsAbove CARGS((SUIT_object me));
SUIT_PROTOTYPE boolean 	      SUIT_isAnyoneOverMe CARGS((SUIT_object me));
    
SUIT_PROTOTYPE void 	      SUIT_paintChildren CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_paintEmployees CARGS((SUIT_object object));
    
SUIT_PROTOTYPE SUIT_object    SUIT_getOneObjectFromClass CARGS((char *className));
SUIT_PROTOTYPE void 	      SUIT_registerInterest CARGS((SUIT_object object, SUIT_objectInterestCallback callback));
SUIT_PROTOTYPE void 	      SUIT_registerInterestInClass CARGS((char *className, SUIT_objectInterestCallback callback));
SUIT_PROTOTYPE void 	      SUIT_registerInterestInGlobal CARGS((SUIT_objectInterestCallback callback));
    
SUIT_PROTOTYPE void 	      SUIT_unlockProperty CARGS((SUIT_object object, char *propertyName, SUIT_level level));
SUIT_PROTOTYPE void 	      SUIT_lockProperty CARGS((SUIT_object object, char *propertyName, SUIT_level level));
SUIT_PROTOTYPE boolean 	      SUIT_propertyIsLocked CARGS((SUIT_object object, char *propertyName, SUIT_level level));

#ifdef _Windows
#define 		      SUIT_malloc(SIZE)        SRGP_malloc(SIZE)
#define 		      SUIT_free(PTR)           SRGP_free(PTR)
#define 		      SUIT_realloc(PTR,SIZE)   SRGP_realloc(PTR,SIZE)
#else    
#define 		      SUIT_malloc(SIZE)        malloc(SIZE)
#define 		      SUIT_free(PTR)           free(PTR)
#define 		      SUIT_realloc(PTR,SIZE)   realloc(PTR,SIZE)
#endif
     
SUIT_PROTOTYPE char *	      SUIT_createRCString CARGS((char *s));

SUIT_PROTOTYPE void 	      SUIT_callPropertyEditor CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_closePropertyEditor CARGS((void));


SUIT_PROTOTYPE SUIT_object    SUIT_createBoundedArea CARGS((char *name, void (*callback) (void))); /* 2-d panner */
SUIT_PROTOTYPE SUIT_object    SUIT_createBoundedValue CARGS((char *name, void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createButton CARGS((char *name, void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createDoneButton CARGS((SUIT_callbackFunctionPtr callback));
SUIT_PROTOTYPE SUIT_object    SUIT_createAbortButton CARGS((SUIT_callbackFunctionPtr callback));
SUIT_PROTOTYPE SUIT_object    SUIT_createArrowButton CARGS((char *name, void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createColorChips CARGS((char *name, void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createLabel CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createOnOffSwitch CARGS((char *name, void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createPatternChips CARGS((char *name, void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createPlaceMat CARGS((char *name));  /* aesthetics only: just draws a rectangle */
SUIT_PROTOTYPE SUIT_object    SUIT_createTrashCan CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createInfoButton CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createExportButton CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createTypeInBox CARGS((char *name, void (*callback)(SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createTextEditor CARGS((char *name, void (*callback)(SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createUVALogo CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createElevator CARGS((char *name, void (*callback)(SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_createSpringPanel CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createFontPanel CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createCallbackFunctionPanel CARGS((char *name));

SUIT_PROTOTYPE SUIT_object    SUIT_createOKCancelDialogBox CARGS((char *name, SUIT_object innards, SUIT_validationFunction dataOK));

SUIT_PROTOTYPE Reply 	      SUIT_activateDialogBox CARGS((SUIT_object dbox));

SUIT_PROTOTYPE Reply 	      SUIT_ask CARGS((char *question, char *button1Name, char *button2Name));
SUIT_PROTOTYPE Reply 	      SUIT_askWithCancel CARGS((char *question, char *button1Name, char *button2Name));
SUIT_PROTOTYPE Reply 	      SUIT_inform CARGS((char *info));
SUIT_PROTOTYPE Reply 	      SUIT_getString CARGS((char *message, char *defaultString, char answer[], int answerLength));
#define 		      SUIT_askYesNo(QUESTION) SUIT_ask (QUESTION, "Yes", "No")
#define 		      SUIT_askOKCancel(QUESTION) ((SUIT_ask (QUESTION, "OK", "Cancel")==REPLY_YES)?REPLY_OK:REPLY_CANCEL)
#define 		      SUIT_askYesNoCancel(QUESTION) SUIT_askWithCancel (QUESTION, "Yes", "No")


SUIT_PROTOTYPE SUIT_object    SUIT_createRadioButtons CARGS((char *name, SUIT_callbackFunctionPtr function));
SUIT_PROTOTYPE void 	      SUIT_addButtonToRadioButtons CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE void 	      SUIT_pressThisRadioButton CARGS((SUIT_object object, char *buttonName));

SUIT_PROTOTYPE SUIT_object    SUIT_createScrollableList CARGS((char *name, void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE void 	      SUIT_resetScrollableListToTop CARGS((SUIT_object scroller));

SUIT_PROTOTYPE SUIT_object    SUIT_createPullDownMenu CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createVerticalMenu CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_addToMenu CARGS((SUIT_object object, char *name, void (*callback) (SUIT_object object)));
SUIT_PROTOTYPE SUIT_object    SUIT_addToMenuWithHotKey CARGS((SUIT_object object, char *name,
							      void (*callback) (SUIT_object), char *hotkey));
SUIT_PROTOTYPE SUIT_object    SUIT_createMenuBar CARGS((char *name));

SUIT_PROTOTYPE char *	      SUIT_textOfFile CARGS((char *fileName));

SUIT_PROTOTYPE void 	      SUIT_initPropertyEditor CARGS((void));


SUIT_PROTOTYPE SUIT_object    SUIT_createClock CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createPolygon CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createBouncingBall CARGS((char *name));

SUIT_PROTOTYPE SUIT_object    SUIT_createBulletinBoard CARGS((char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createBulletinBoardWithClass CARGS((char *name, char *className));

SUIT_PROTOTYPE void 	      SUIT_centerObjectOnScreen CARGS((SUIT_object object));

SUIT_PROTOTYPE point 	      SUIT_mapRelativeLocationToScreen CARGS((SUIT_object object, point pt));
SUIT_PROTOTYPE point 	      SUIT_mapScreenToRelativeLocation CARGS((SUIT_object object, point pt));

SUIT_PROTOTYPE rectangle      SUIT_mapViewportToScreen CARGS((SUIT_object object, SUIT_viewport viewport));
SUIT_PROTOTYPE SUIT_viewport  SUIT_mapScreenToViewport CARGS((SUIT_object object, rectangle screen));
SUIT_PROTOTYPE SUIT_viewport  SUIT_mapToParent CARGS((SUIT_object object, double x1, double y1, double x2, double y2));
SUIT_PROTOTYPE void 	      SUIT_centerInParent CARGS((SUIT_object object, double centerX, double centerY));

SUIT_PROTOTYPE char *	      SUIT_getHelp CARGS((char *className, char *propertyName));
SUIT_PROTOTYPE void 	      SUIT_registerHelp CARGS((char *className, char *propertyName, char *helpText));

SUIT_PROTOTYPE SUIT_object    SUIT_createTextBox CARGS((char *name, char *text));
SUIT_PROTOTYPE void 	      SUIT_optimizeSizeForText CARGS((SUIT_object object, int width));
SUIT_PROTOTYPE void 	      SUIT_redrawLocalSection CARGS((SUIT_object object, rectangle rect));
SUIT_PROTOTYPE SUIT_object    SUIT_createStacker CARGS((char *name));

SUIT_PROTOTYPE GP_rectangle   SUIT_defWindow CARGS((double x1, double y1, double x2, double y2));
SUIT_PROTOTYPE SUIT_viewport  SUIT_defViewport CARGS((int x1, int y1, int x2, int y2));
SUIT_PROTOTYPE void 	      SUIT_forceOnScreen CARGS((SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_getObjectSize CARGS((SUIT_object object, int *width, int *height));

SUIT_PROTOTYPE SUIT_object    SUIT_createTextEditorWithScrollBar CARGS((char *name, void (*callback)(SUIT_object object));
SUIT_PROTOTYPE void 	      SUIT_sendToEditor CARGS((SUIT_object object, char *command));
SUIT_PROTOTYPE void 	      SUIT_highlightBlockInTextEditor (SUIT_object object, int beginPosition, int endPosition));

SUIT_PROTOTYPE void 	      SUIT_iterateOverAllProperties CARGS((SUIT_propertyCallback callback));

SUIT_PROTOTYPE SUIT_object    SUIT_createObjectByClass CARGS((char *ObjectName, char *ClassName));
SUIT_PROTOTYPE boolean 	      SUIT_isAncestor CARGS((SUIT_object parent, SUIT_object child));

SUIT_PROTOTYPE char *	      SUIT_relativeName CARGS((SUIT_object object, char *name));
SUIT_PROTOTYPE SUIT_object    SUIT_createFileBrowser CARGS((char *name, char *startDirectory, char *label,
							    char *typeInBoxLabel, SUIT_callbackFunctionPtr callback));
SUIT_PROTOTYPE SUIT_object    SUIT_createFileBrowserDialogBox CARGS((char *name, char *startDir, char *label,
								     char *typeInBoxLabel));
SUIT_PROTOTYPE char *	      SUIT_askForFileName CARGS((char *startDirectory, char *label, char *typeInBoxLabel));
SUIT_PROTOTYPE SUIT_object    SUIT_copyObject CARGS((SUIT_object obj, char *newName));

SUIT_PROTOTYPE void           SUIT_makeAssertionsFatal CARGS((boolean fatal));


#endif /* BASESUIT_INCLUDED */
