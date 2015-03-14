#include <stdio.h>


#ifdef SRGP_BOSS
#define DECLARE
#else
#define DECLARE extern
#endif


/** the special screen canvas ID**/
#define SCREEN_CANVAS		0


/** WRITE MODE **/
typedef enum {WRITE_REPLACE=0, WRITE_XOR, WRITE_OR, WRITE_AND} writeMode;


/** FILL STYLE or PEN STYLE  **/
/* These *must* match the following X11 identifiers */
typedef enum {
   SOLID=0, 				/*FillSolid*/
   PIXMAP_PATTERN,			/*FillTiled*/
   BITMAP_PATTERN_TRANSPARENT,		/*FillStippled*/
   BITMAP_PATTERN_OPAQUE		/*FillOpaqueStippled*/
 } drawStyle;


/** SPECIAL DATA TYPES **/
typedef int 	      	(*funcptr)();
typedef int		canvasID;


/** GEOMETRIC DATA TYPES **/
typedef struct {int x, y;} 				srgp__point;
typedef struct {srgp__point bottom_left, top_right;}	srgp__rectangle;


/** ATTRIBUTE GROUP (application should never use internals) **/
typedef struct {
   int write_mode;
   srgp__rectangle clip_rectangle;
   int font;
   lineStyle line_style;
   int line_width;
   int marker_size;
   markerStyle marker_style;
   int color, background_color;
   int plane_mask;
   drawStyle fill_style;
   int fill_pixmap_pattern_id;
   int fill_bitmap_pattern_id;
   drawStyle pen_style;
   int pen_pixmap_pattern_id;
   int pen_bitmap_pattern_id;
} srgp__attribute_group;





/** THE TIMESTAMP FOR INPUT DEVICE MEASURES **/
typedef struct {
   int seconds;
   int ticks;
} srgp_timestamp;




/** LOCATOR MEASURE
It is of utmost importance that the order of the fields not be changed!
The "vanilla" and deluxe versions must "start off" identically.
**/

typedef struct {
   srgp__point position;
   buttonStatus button_chord[3];
   int button_of_last_transition;
} srgp__locator_measure;

typedef struct {
   srgp__point position;
   buttonStatus button_chord[3];
   int button_of_last_transition;
   buttonStatus modifier_chord[3];
   srgp_timestamp timestamp;
} srgp__deluxe_locator_measure;

#define buttonChord button_chord
#define buttonOfMostRecentTransition button_of_last_transition



/** KEYBOARD MEASURE **/

typedef char *srgp__keyboard_measure;

typedef struct {
   char *buffer;
   int buffer_length;
   buttonStatus modifier_chord[3];
   srgp__point position;
   srgp_timestamp timestamp;
} srgp__deluxe_keyboard_measure;
 


/** "CONSTANTS" GOVERNING THE SIZES OF INTERNAL TABLES
Since the tables are malloc'd at runtime, these are not really constants.
They are given default values, but these values may be changed by
function calls IF these function calls are made before the application
calls SRGP_begin().
**/

DECLARE short MAX_PATTERN_INDEX
#ifdef SRGP_BOSS
                      = DEFAULT_MAX_PATTERN_INDEX
#endif
      ;
DECLARE short MAX_CURSOR_INDEX
#ifdef SRGP_BOSS
                      = DEFAULT_MAX_CURSOR_INDEX
#endif
      ;
DECLARE short MAX_FONT_INDEX
#ifdef SRGP_BOSS
                      = DEFAULT_MAX_FONT_INDEX
#endif
      ;
DECLARE short MAX_CANVAS_INDEX
#ifdef SRGP_BOSS
                      = DEFAULT_MAX_CANVAS_INDEX
#endif
      ;
DECLARE short MAX_STRING_SIZE
#ifdef SRGP_BOSS
                      = DEFAULT_MAX_STRING_SIZE
#endif
      ;
DECLARE short MAX_POINTLIST_SIZE
#ifdef SRGP_BOSS
                      = DEFAULT_MAX_POINTLIST_SIZE
#endif
      ;


void SRGP_setMaxCanvasIndex (int i);
void SRGP_setMaxPatternIndex (int i);
void SRGP_setMaxCursorIndex (int i);
void SRGP_setMaxFontIndex (int i);
void SRGP_setMaxPointlistSize (int i);
void SRGP_setMaxStringSize (int i);





DECLARE int		SRGP_BLACK, SRGP_WHITE;
#define COLOR_BLACK SRGP_BLACK
#define COLOR_WHITE SRGP_WHITE

DECLARE FILE 		*SRGP_logStream;

#undef DECLARE


/* DEFINED ONLY IN THIS FILE SO I CAN 
   MAKE EACH PROTOTYPE FIT ON ONE LINE. */
#define ush     unsigned short
#define rect    srgp__rectangle
#define pt      srgp__point


/** ERROR HANDLING MODE **/
typedef enum {FATAL_ERRORS, NON_FATAL_ERRORS} 
   errorHandlingMode;
   
extern int SRGP_errorOccurred;


/** PROTOTYPES FOR ALL PUBLIC SRGP ROUTINES **/

/******************** attribute.c */
void 	    SRGP_setAttributes (srgp__attribute_group*);
void	    SRGP_setBackgroundColor (int value);
void	    SRGP_setClipRectangle (rect);
void 	    SRGP_setColor (int value);
void 	    SRGP_setFillBitmapPattern (int value);
void 	    SRGP_setFillPixmapPattern (int value);
void	    SRGP_setFillStyle (drawStyle);
void	    SRGP_setFont (int value);
void	    SRGP_setLineStyle (lineStyle);
void	    SRGP_setLineWidth (int value);
void	    SRGP_setMarkerStyle (markerStyle);
void	    SRGP_setMarkerSize (int value);
void	    SRGP_setPenBitmapPattern (int value);
void	    SRGP_setPenPixmapPattern (int value);
void 	    SRGP_setPenStyle (drawStyle);
void 	    SRGP_setPlaneMask (int value);
void 	    SRGP_setWriteMode (writeMode);

/******************** canvas.c */
canvasID    SRGP_createCanvas (int width, int height);
void	    SRGP_useCanvas (canvasID);
void	    SRGP_deleteCanvas (canvasID);

/******************** color.c */
void   	    SRGP_loadCommonColor (int entry, char *name);
void        SRGP_loadColorTable (int start, int cnt, ush *r, ush *g, ush *b);
void        SRGP_loadSingleColor (int start, ush r, ush g, ush b);

/******************** cursor.c, font.c, pattern.c */
void	    SRGP_loadCursor (int index, int CURSOR_SHAPE);
void	    SRGP_loadFont (int font_index, char* filename);
int	    SRGP_loadBitmapPatternsFromFile (FILE*);
int         SRGP_loadPixmapPatternsFromFile (FILE*);
void	    SRGP_loadBitmapPattern (int index, char *data);
void        SRGP_loadPixmapPattern (int index, int *data);

/******************** input.c */
void	    SRGP_setInputMode (inputDevice, inputMode);
inputDevice SRGP_waitEvent (int maximum_wait_time);
void	    SRGP_getLocator (srgp__locator_measure*);
void	    SRGP_getKeyboard (char *buf, int bufsize);
void	    SRGP_getDeluxeLocator (srgp__deluxe_locator_measure*);
void	    SRGP_getDeluxeKeyboard (srgp__deluxe_keyboard_measure*);
void	    SRGP_sampleLocator (srgp__locator_measure*);
void	    SRGP_sampleKeyboard (char *measure, int bufsize);
void	    SRGP_sampleDeluxeLocator (srgp__deluxe_locator_measure*);
void	    SRGP_sampleDeluxeKeyboard (srgp__deluxe_keyboard_measure*);
void	    SRGP_setLocatorMeasure (pt position);
void	    SRGP_setKeyboardMeasure (char*);
void	    SRGP_setLocatorEchoType (echoType);
void 	    SRGP_setLocatorEchoCursorShape (int id);
void	    SRGP_setLocatorEchoRubberAnchor (pt position);
void	    SRGP_setLocatorButtonMask (int BUTTON_MASK);
void	    SRGP_setKeyboardProcessingMode (keyboardMode);
void	    SRGP_setKeyboardEchoColor (int value);
void	    SRGP_setKeyboardEchoOrigin (pt position);
void	    SRGP_setKeyboardEchoFont (int fontindex);

/******************** inquire.c */
void	    SRGP_inquireAttributes (srgp__attribute_group *att_group);
rect	    SRGP_inquireClipRectangle (void);
canvasID    SRGP_inquireActiveCanvas (void);
rect 	    SRGP_inquireCanvasExtent (canvasID id);
void	    SRGP_inquireCanvasSize (canvasID id, int *w, int *h);
int	    SRGP_inquireCanvasDepth (void);
void	    SRGP_inquireTextExtent (char *s, int *w, int *h, int *d);
void	    SRGP_inquireColorTable (int start, int cnt, ush *r, ush *g, ush *b);
#ifdef X_PROTOCOL
Drawable    SRGP_inquireXDrawable (canvasID);
Display    *SRGP_inquireXDisplay (void);
Window      SRGP_inquireXWindow (void);
#endif

/******************** output.c */
pt	    SRGP_defPoint (int x, int y);
rect	    SRGP_defRectangle (int lx, int by, int rx, int t);
void	    SRGP_beep (void);
void	    SRGP_pointCoord (int x, int y);
void	    SRGP_point (pt);
void	    SRGP_polyPoint (int vCount, srgp__point *vertices);
void	    SRGP_polyPointCoord (int vCount, int *xs, int *ys);
void	    SRGP_markerCoord (int x, int y);
void	    SRGP_marker (pt);
void	    SRGP_polyMarker (int vCount, srgp__point *vertices);
void	    SRGP_polyMarkerCoord (int vCount, int *xlist, int *ylist);
void	    SRGP_lineCoord (int x1, int y1, int x2, int y2);
void	    SRGP_line (pt, pt);
void 	    SRGP_rectangleCoord (int lx, int by, int rx, int ty);
void	    SRGP_rectanglePt (srgp__point bottom_left, srgp__point top_right);
void	    SRGP_rectangle (rect);
void	    SRGP_polyLine (int vCount, pt *vertices);
void	    SRGP_polyLineCoord (int vxCount, int *xlist, int *ylist);
void	    SRGP_polygon (int vCount, pt *vertices);
void	    SRGP_polygonCoord (int vCount, int *xlist, int *ylist);
void 	    SRGP_fillRectangleCoord (int lx, int by, int rx, int ty);
void	    SRGP_fillRectanglePt (pt bl, pt tr);
void	    SRGP_fillRectangle (rect);
void	    SRGP_fillPolygon (int vCount, pt *vertices);
void	    SRGP_fillPolygonCoord (int vCount, int *xlist, int *ylist);
void	    SRGP_ellipse (rect);
void	    SRGP_ellipseArc (rect r, double a1, double a2);
void        SRGP_fillEllipse (rect);
void        SRGP_fillEllipseArc (rect r, double a1, double a2);
void	    SRGP_text (pt, char*);
void        SRGP_refresh (void);

/******************** raster.c */
void	    SRGP_copyPixel (canvasID src, rect s, pt d);

/******************** state.c */
void	    SRGP_begin (char *name, int w, int h, int planes, boolean trace);
void	    SRGP_beginWithDebug 
                (char *name, int w, int h, int planes, boolean trace);
void	    SRGP_disableDebugAids (void);
void	    SRGP_enableBlockedWait (void);
void	    SRGP_setErrorHandlingMode (errorHandlingMode);
void	    SRGP_enableSynchronous (void);
void	    SRGP_tracing (boolean);
void	    SRGP_allowResize (boolean);
void 	    SRGP_registerResizeCallback (funcptr);
void 	    SRGP_changeScreenCanvasSize (int newwidth, int newheight);
void	    SRGP_end (void);


#undef ush
#undef rect
#undef pt
