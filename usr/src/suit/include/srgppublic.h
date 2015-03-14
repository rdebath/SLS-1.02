#include <stdio.h>


#ifdef SRGP_BOSS
#define DECLARE
#else
#define DECLARE extern
#endif


/** the special screen canvas ID**/
#define SCREEN_CANVAS		0

#ifdef _Windows
void UserMain(int argc, char *argv[]);  /* needed to start up user's program */
#define main(ARGC,ARGV) UserMain(ARGC,ARGV)
#endif

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


#if defined(__STDC__) || defined(__cplusplus) || defined(IBM_PC)
#define ANSI_PROTO
#endif

#ifdef ANSI_PROTO
#define CARGS(X)  X
#else
#define CARGS(X)  ()
#endif



void SRGP_setMaxCanvasIndex CARGS((int i));
void SRGP_setMaxPatternIndex CARGS((int i));
void SRGP_setMaxCursorIndex CARGS((int i));
void SRGP_setMaxFontIndex CARGS((int i));
void SRGP_setMaxPointlistSize CARGS((int i));
void SRGP_setMaxStringSize CARGS((int i));





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
void 	    SRGP_setAttributes CARGS((srgp__attribute_group*));
void	    SRGP_setBackgroundColor CARGS((int value));
void	    SRGP_setClipRectangle CARGS((rect));
void 	    SRGP_setColor CARGS((int value));
void 	    SRGP_setFillBitmapPattern CARGS((int value));
void 	    SRGP_setFillPixmapPattern CARGS((int value));
void	    SRGP_setFillStyle CARGS((drawStyle));
void	    SRGP_setFont CARGS((int value));
void	    SRGP_setLineStyle CARGS((lineStyle));
void	    SRGP_setLineWidth CARGS((int value));
void	    SRGP_setMarkerStyle CARGS((markerStyle));
void	    SRGP_setMarkerSize CARGS((int value));
void	    SRGP_setPenBitmapPattern CARGS((int value));
void	    SRGP_setPenPixmapPattern CARGS((int value));
void 	    SRGP_setPenStyle CARGS((drawStyle));
void 	    SRGP_setPlaneMask CARGS((int value));
void 	    SRGP_setWriteMode CARGS((writeMode));

/******************** canvas.c */
canvasID    SRGP_createCanvas CARGS((int width, int height));
void	    SRGP_useCanvas CARGS((canvasID));
void	    SRGP_deleteCanvas CARGS((canvasID));

/******************** color.c */
void   	    SRGP_loadCommonColor CARGS((int entry, char *name));
void        SRGP_loadColorTable CARGS((int start, int cnt, ush *r, ush *g, ush *b));
void        SRGP_loadSingleColor CARGS((int start, ush r, ush g, ush b));

/******************** cursor.c, font.c, pattern.c */
void	    SRGP_loadCursor CARGS((int index, int CURSOR_SHAPE));
void	    SRGP_loadFont CARGS((int font_index, char* filename));
int	    SRGP_loadBitmapPatternsFromFile CARGS((FILE*));
int         SRGP_loadPixmapPatternsFromFile CARGS((FILE*));
void	    SRGP_loadBitmapPattern CARGS((int index, char *data));
void        SRGP_loadPixmapPattern CARGS((int index, int *data));

/******************** input.c */
void	    SRGP_setInputMode CARGS((inputDevice, inputMode));
inputDevice SRGP_waitEvent CARGS((int maximum_wait_time));
void	    SRGP_getLocator CARGS((srgp__locator_measure*));
void	    SRGP_getKeyboard CARGS((char *buf, int bufsize));
void	    SRGP_getDeluxeLocator CARGS((srgp__deluxe_locator_measure*));
void	    SRGP_getDeluxeKeyboard CARGS((srgp__deluxe_keyboard_measure*));
void	    SRGP_sampleLocator CARGS((srgp__locator_measure*));
void	    SRGP_sampleKeyboard CARGS((char *measure, int bufsize));
void	    SRGP_sampleDeluxeLocator CARGS((srgp__deluxe_locator_measure*));
void	    SRGP_sampleDeluxeKeyboard CARGS((srgp__deluxe_keyboard_measure*));
void	    SRGP_setLocatorMeasure CARGS((pt position));
void	    SRGP_setKeyboardMeasure CARGS((char*));
void	    SRGP_setLocatorEchoType CARGS((echoType));
void 	    SRGP_setLocatorEchoCursorShape CARGS((int id));
void	    SRGP_setLocatorEchoRubberAnchor CARGS((pt position));
void	    SRGP_setLocatorButtonMask CARGS((int BUTTON_MASK));
void	    SRGP_setKeyboardProcessingMode CARGS((keyboardMode));
void	    SRGP_setKeyboardEchoColor CARGS((int value));
void	    SRGP_setKeyboardEchoOrigin CARGS((pt position));
void	    SRGP_setKeyboardEchoFont CARGS((int fontindex));

/******************** inquire.c */
void	    SRGP_inquireAttributes CARGS((srgp__attribute_group *att_group));
rect	    SRGP_inquireClipRectangle CARGS((void));
canvasID    SRGP_inquireActiveCanvas CARGS((void));
rect 	    SRGP_inquireCanvasExtent CARGS((canvasID));
void	    SRGP_inquireCanvasSize CARGS((canvasID id, int *w, int *h));
int	    SRGP_inquireCanvasDepth CARGS((void));
void	    SRGP_inquireTextExtent CARGS((char *str, int *w, int *h, int *d));
void	    SRGP_inquireColorTable CARGS((int start, int cnt, ush *r, ush *g, ush *b));
#ifdef X_PROTOCOL
Drawable    SRGP_inquireXDrawable CARGS((canvasID));
#endif
#ifdef _Windows
/* in state_wi.c */
int	    SRGP_inquireWinInstance CARGS((void));
#endif

/******************** output.c */
pt	    SRGP_defPoint CARGS((int x, int y));
rect	    SRGP_defRectangle CARGS((int lx, int by, int rx, int t));
void	    SRGP_beep CARGS((void));
void	    SRGP_pointCoord CARGS((int x, int y));
void	    SRGP_point CARGS((pt));
void	    SRGP_polyPoint CARGS((int vCount, srgp__point *vertices));
void	    SRGP_polyPointCoord CARGS((int vCount, int *xs, int *ys));
void	    SRGP_markerCoord CARGS((int x, int y));
void	    SRGP_marker CARGS((pt));
void	    SRGP_polyMarker CARGS((int vCount, srgp__point *vertices));
void	    SRGP_polyMarkerCoord CARGS((int vCount, int *xlist, int *ylist));
void	    SRGP_lineCoord CARGS((int x1, int y1, int x2, int y2));
void	    SRGP_line CARGS((pt, pt));
void 	    SRGP_rectangleCoord CARGS((int lx, int by, int rx, int ty));
void	    SRGP_rectanglePt CARGS((srgp__point bottom_left, srgp__point top_right));
void	    SRGP_rectangle CARGS((rect));
void	    SRGP_polyLine CARGS((int vCount, pt *vertices));
void	    SRGP_polyLineCoord CARGS((int vxCount, int *xlist, int *ylist));
void	    SRGP_polygon CARGS((int vCount, pt *vertices));
void	    SRGP_polygonCoord CARGS((int vCount, int *xlist, int *ylist));
void 	    SRGP_fillRectangleCoord CARGS((int lx, int by, int rx, int ty));
void	    SRGP_fillRectanglePt CARGS((pt bl, pt tr));
void	    SRGP_fillRectangle CARGS((rect));
void	    SRGP_fillPolygon CARGS((int vCount, pt *vertices));
void	    SRGP_fillPolygonCoord CARGS((int vCount, int *xlist, int *ylist));
void	    SRGP_ellipse CARGS((rect));
void	    SRGP_ellipseArc CARGS((rect r, double a1, double a2));
void        SRGP_fillEllipse CARGS((rect));
void        SRGP_fillEllipseArc CARGS((rect r, double a1, double a2));
void	    SRGP_text CARGS((pt, char*));
void        SRGP_refresh CARGS((void));

/******************** raster.c */
void	    SRGP_copyPixel CARGS((canvasID src, rect s, pt d));

/******************** state.c */
void	    SRGP_begin CARGS((char *name, int w, int h, int planes, boolean trace));
void	    SRGP_beginWithDebug 
                CARGS((char *name, int w, int h, int planes, boolean trace));
void	    SRGP_disableDebugAids CARGS((void));
void	    SRGP_enableBlockedWait CARGS((void));
void	    SRGP_setErrorHandlingMode CARGS((errorHandlingMode));
void	    SRGP_enableSynchronous CARGS((void));
void	    SRGP_tracing CARGS((boolean));
void	    SRGP_allowResize CARGS((boolean));
void 	    SRGP_registerResizeCallback CARGS((funcptr));
void 	    SRGP_changeScreenCanvasSize CARGS((int newwidth, int newheight));
void	    SRGP_end CARGS((void));

#ifdef _Windows
/******************** mem.c */
void far   *SRGP_malloc CARGS((unsigned int size));
void far   *SRGP_realloc CARGS((void far *ptr, unsigned int size));
void        SRGP_free CARGS((void far *ptr));
#endif
    
#undef ush
#undef rect
#undef pt
