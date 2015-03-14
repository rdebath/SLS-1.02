/** PUBLIC CONSTANTS AND TYPES COMMON TO BOTH PACKAGES
**/

/* implementation-specific constants */

#ifdef THINK_C
#define DEFAULT_MAX_PATTERN_INDEX      120
#define DEFAULT_MAX_CURSOR_INDEX	 4
#define DEFAULT_MAX_FONT_INDEX          20
#define DEFAULT_MAX_CANVAS_INDEX        10
#define DEFAULT_MAX_STRING_SIZE		80
#define DEFAULT_MAX_POINTLIST_SIZE      50
#else
#define DEFAULT_MAX_PATTERN_INDEX      150
#define DEFAULT_MAX_CURSOR_INDEX	25
#define DEFAULT_MAX_FONT_INDEX          50
#define DEFAULT_MAX_CANVAS_INDEX        50
#define DEFAULT_MAX_STRING_SIZE		80
#define DEFAULT_MAX_POINTLIST_SIZE     500
#endif

/*!*/
/* EXTRA DATA TYPES */
typedef unsigned char boolean;

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif



/*!*/
/* ATTRIBUTES */


/* line style */
/*    If you change the order, see attribute.c!!! */
#define NUMBER_OF_LINE_STYLES		4
typedef enum {CONTINUOUS=0, DASHED, DOTTED, DOT_DASHED} lineStyle;


/* marker style */
#define NUMBER_OF_MARKER_STYLES		3
typedef enum {MARKER_CIRCLE=0, MARKER_SQUARE, MARKER_X} markerStyle;


/** INPUT RELATED STUFF **/

typedef enum {NO_DEVICE=0, KEYBOARD, LOCATOR} inputDevice;

typedef enum {INACTIVE=0, SAMPLE, EVENT} inputMode;

/* locator measure */
#define LEFT_BUTTON   0
#define MIDDLE_BUTTON 1
#define RIGHT_BUTTON  2
#define    LEFT_BUTTON_MASK   1
#define    MIDDLE_BUTTON_MASK 2
#define    RIGHT_BUTTON_MASK  4
#define SHIFT   0
#define CONTROL 1
#define META    2

typedef enum {EDIT=0, RAW} keyboardMode;

typedef enum {NO_ECHO=0, CURSOR, RUBBER_LINE, RUBBER_RECT} echoType;

typedef enum {UP=0, DOWN} buttonStatus;

/* time-out spec */
#define INDEFINITE              -1

/* explosions of geometric records */
#define ExplodeRect(R)   \
   R.bottom_left.x,R.bottom_left.y,R.top_right.x,R.top_right.y
#define ExplodePt(P)     P.x,P.y

