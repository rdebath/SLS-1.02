/* (C) Copyright 1990, 1991, 1992 the University of Virginia */

#include "privsuit.h"
#include "dynarray.h"
#include <string.h>
#include <ctype.h>


#ifdef RS6000
#include <time.h>
#include <sys/time.h>
#include <sys/times.h>
#endif

#ifdef SUN
#include <sys/time.h>
#endif

#ifdef _Windows
#include <time.h>
#endif

#ifdef IBM_PC
#include <dos.h>
#endif

#ifdef MACINTOSH
#include <time.h>
#endif

#ifdef SGI_X
#include <sys/time.h>
#endif

#ifndef MAX
#define MAX(A,B)   (((A)>(B))? (A):(B))
#endif

#define CreateSafeString(STRING) strdup(STRING)


PRIVATE rectangle view;
PRIVATE GP_rectangle world;
PRIVATE int screenDepth;
PRIVATE DynArray colorMenu;
PRIVATE DynArray cursorMenu;

#define PixelsPerWorldWidth	((view.top_right.x - view.bottom_left.x)/(world.top_right.x - world.bottom_left.x))

#define PixelsPerWorldHeight	((view.top_right.y - view.bottom_left.y)/(world.top_right.y - world.bottom_left.y))



void GP_setWindow (GP_rectangle newWorld)
{
    world = newWorld;
}


void GP_setViewport (rectangle newView)
{
    view = newView;
}


double GP_unMapX (int x)
{
    return ((double) (x - view.bottom_left.x) / PixelsPerWorldWidth + world.bottom_left.x);
}


double GP_unMapY (int y)
{
    return ((double) (y - view.bottom_left.y) / PixelsPerWorldHeight + world.bottom_left.y);
}


double GP_unMapWidth (int w)
{
    return (double) w / PixelsPerWorldWidth;
}



double GP_unMapHeight (int h)
{
    return (double) h / PixelsPerWorldHeight;
}


GP_point GP_unMapPoint (point pt)
{
    GP_point retval;

    retval.x = GP_unMapX (pt.x);
    retval.y = GP_unMapY (pt.y);
    return (retval);
}


GP_rectangle GP_unMapRectangle (rectangle r)
{
    GP_rectangle retval;

    retval.bottom_left = GP_unMapPoint (r.bottom_left);
    retval.top_right = GP_unMapPoint (r.top_right);
    return (retval);
}


GP_point GP_defPoint (double x, double y)
{
    GP_point retval;

    retval.x = x;
    retval.y = y;
    return (retval);
}


GP_rectangle GP_defRectangle (double x1, double y1, double x2, double y2)
{
    GP_rectangle retval;

    retval.bottom_left = GP_defPoint (x1, y1);
    retval.top_right = GP_defPoint (x2, y2);
    return (retval);
}


int GP_mapX (double x)
{
    return ((int) ((x - world.bottom_left.x) * PixelsPerWorldWidth) + view.bottom_left.x);
}


int GP_mapY (double y)
{
    return ((int) ((y - world.bottom_left.y) * PixelsPerWorldHeight) + view.bottom_left.y);
}


int GP_mapWidth (double w)
{
    return (int) (w * PixelsPerWorldWidth);
}


int GP_mapHeight (double h)
{
    return (int) (h * PixelsPerWorldHeight);
}


point GP_mapPoint (GP_point pt)
{
    point retval;

    retval.x = GP_mapX (pt.x);
    retval.y = GP_mapY (pt.y);
    return (retval);
}


rectangle GP_mapRectangle (GP_rectangle r)
{
    rectangle retval;

    retval.bottom_left = GP_mapPoint (r.bottom_left);
    retval.top_right = GP_mapPoint (r.top_right);
    return (retval);
}


void GP_setLineWidth (int newValue)
{
    ASSERT ((newValue >= 0), (mes, "GP_setLineWidth passed a bad value (%d)\n", newValue));
    SRGP_setLineWidth (MAX (newValue, 1));
}




PRIVATE char *CurrentFontFamily;
PRIVATE char *CurrentFontStyle;
PRIVATE double CurrentFontPointSize;
PRIVATE int CurrentCursor = -1;

typedef struct {
    attribute_group att;
    rectangle view;
    GP_rectangle world;
    char *family, *style;
    double pointsize;
    int cursor;
} GraphicsState;



PRIVATE DynArray attribute_stack;

void GP_pushGraphicsState (void)
{
    attribute_group newState;
    GraphicsState newGuy;
    PRIVATE int notCalledBefore = 1;
    
    if (notCalledBefore) {
	attribute_stack = DynCreate (sizeof (GraphicsState), 1);
	notCalledBefore = 0;
    }
    SRGP_inquireAttributes (&newState);
    newGuy.att = newState;
    newGuy.view = view;
    newGuy.world = world;
    newGuy.family = CurrentFontFamily;
    newGuy.style = CurrentFontStyle;
    newGuy.pointsize = CurrentFontPointSize;
    newGuy.cursor = CurrentCursor;
    DynAdd (attribute_stack, (char *) &newGuy);
}



void GP_popGraphicsState (void)
{
    GraphicsState oldGuy;

    if (DynSize (attribute_stack) < 1)
	return;
    oldGuy = *(GraphicsState *) DynGet (attribute_stack, DynHigh (attribute_stack));
    DynDelete (attribute_stack, DynHigh (attribute_stack));
    SRGP_setAttributes (&oldGuy.att);
    view = oldGuy.view;
    world = oldGuy.world;
    CurrentFontFamily = oldGuy.family;
    CurrentFontStyle = oldGuy.style;
    CurrentFontPointSize = oldGuy.pointsize;
    if (oldGuy.cursor >= 0 && oldGuy.cursor != CurrentCursor)
	GP_setCursor (oldGuy.cursor);
    CurrentCursor = oldGuy.cursor;
}



void GP_ellipse (GP_rectangle r)
{
    rectangle newR;

    newR = GP_mapRectangle (r);
    SRGP_ellipse (newR);
}



void GP_ellipseCoord (double x1, double y1, double x2, double y2)
{
    GP_ellipse (GP_defRectangle (x1, y1, x2, y2));
}



void GP_fillEllipseCoord (double x1, double y1, double x2, double y2)
{
    GP_fillEllipse (GP_defRectangle (x1, y1, x2, y2));
}



void GP_fillEllipse (GP_rectangle r)
{
    rectangle newR;

    newR = GP_mapRectangle (r);
    SRGP_fillEllipse (newR);
}



void GP_ellipseArc (GP_rectangle r, double a1, double a2)
{
    rectangle newR;

    newR = GP_mapRectangle (r);
    SRGP_ellipseArc (newR, a1, a2);
}



void GP_fillEllipseArc (GP_rectangle r, double a1, double a2)
{
    rectangle newR;

    newR = GP_mapRectangle (r);
    SRGP_fillEllipseArc (newR, a1, a2);
}



point *GP_mapPointArray (int vCount, GP_point * vertices)
{
    int i;
    point *retarray, *walker;

    retarray = (point *) SUIT_malloc (vCount * (sizeof (point)));
    walker = retarray;
    for (i = 0; i < vCount; i++)
	*walker++ = GP_mapPoint (vertices[i]);
    return (retarray);
}



int *GP_mapXArray (int vCount, double *vertices)
{
    int i;
    int *retarray, *walker;

    retarray = (int *) SUIT_malloc (vCount * (sizeof (int)));
    walker = retarray;
    for (i = 0; i < vCount; i++)
	*walker++ = GP_mapX (vertices[i]);
    return (retarray);
}



int *GP_mapYArray (int vCount, double *vertices)
{
    int i;
    int *retarray, *walker;

    retarray = (int *) SUIT_malloc (vCount * (sizeof (int)));
    walker = retarray;
    for (i = 0; i < vCount; i++)
	*walker++ = GP_mapY (vertices[i]);
    return (retarray);
}



void GP_rectanglePt (GP_point bottom_left, GP_point top_right)
{
    point low_left, up_right;

    low_left = GP_mapPoint (bottom_left);
    up_right = GP_mapPoint (top_right);
    SRGP_rectanglePt (low_left, up_right);
}



void GP_fillRectanglePt (GP_point bottom_left, GP_point top_right)
{
    point low_left, up_right;

    low_left = GP_mapPoint (bottom_left);
    up_right = GP_mapPoint (top_right);
    SRGP_fillRectanglePt (low_left, up_right);
}



void GP_drawRectangle (GP_rectangle rect)
{
    SRGP_rectangle (GP_mapRectangle (rect));
}



void GP_fillRectangle (GP_rectangle rect)
{
    SRGP_fillRectangle (GP_mapRectangle (rect));
}



void GP_polygon (int vCount, GP_point * vertices)
{
    point *temp = GP_mapPointArray (vCount, vertices);
    SRGP_polygon (vCount, temp);
    SUIT_free (temp);
}



void GP_polygonCoord (int vCount, double *xlist, double *ylist)
{
    int *mappedXlist, *mappedYlist;

    mappedXlist = GP_mapXArray (vCount, xlist);
    mappedYlist = GP_mapYArray (vCount, ylist);
    SRGP_polygonCoord (vCount, mappedXlist, mappedYlist);
    SUIT_free (mappedXlist);
    SUIT_free (mappedYlist);
}



void GP_polyLine (int vCount, GP_point * vertices)
{
    point *temp = GP_mapPointArray (vCount, vertices);
    SRGP_polyLine (vCount, temp);
    SUIT_free (temp);
}



void GP_polyLineCoord (int vCount, double *xlist, double *ylist)
{
    int *mappedXlist, *mappedYlist;

    mappedXlist = GP_mapXArray (vCount, xlist);
    mappedYlist = GP_mapYArray (vCount, ylist);
    SRGP_polyLineCoord (vCount, mappedXlist, mappedYlist);
    SUIT_free (mappedXlist);
    SUIT_free (mappedYlist);
}



void GP_fillPolygon (int vCount, GP_point * vertices)
{
    point *temp = GP_mapPointArray (vCount, vertices);
    SRGP_fillPolygon (vCount, temp);
    SUIT_free (temp);
}



void GP_fillPolygonCoord (int vCount, double *xlist, double *ylist)
{
    int *mappedXlist, *mappedYlist;

    mappedXlist = GP_mapXArray (vCount, xlist);
    mappedYlist = GP_mapYArray (vCount, ylist);
    SRGP_fillPolygonCoord (vCount, mappedXlist, mappedYlist);
    SUIT_free (mappedXlist);
    SUIT_free (mappedYlist);
}



void GP_polyPoint (int vCount, GP_point * vertices)
{
    point *temp = GP_mapPointArray (vCount, vertices);
    SRGP_polyPoint (vCount, temp);
    SUIT_free (temp);
}



void GP_polyPointCoord (int vCount, double *xs, double *ys)
{
    int *mappedXlist, *mappedYlist;

    mappedXlist = GP_mapXArray (vCount, xs);
    mappedYlist = GP_mapYArray (vCount, ys);
    SRGP_polyPointCoord (vCount, mappedXlist, mappedYlist);
    SUIT_free (mappedXlist);
    SUIT_free (mappedYlist);
}



void GP_polyMarker (int vCount, GP_point * vertices)
{
    point *temp = GP_mapPointArray (vCount, vertices);
    SRGP_polyMarker (vCount, temp);
    SUIT_free (temp);
}



void GP_polyMarkerCoord (int vCount, double *xs, double *ys)
{
    int *mappedXlist, *mappedYlist;

    mappedXlist = GP_mapXArray (vCount, xs);
    mappedYlist = GP_mapYArray (vCount, ys);
    SRGP_polyMarkerCoord (vCount, mappedXlist, mappedYlist);
    SUIT_free (mappedXlist);
    SUIT_free (mappedYlist);
}



void GP_line (GP_point pt1, GP_point pt2)
{
    SRGP_line (GP_mapPoint (pt1), GP_mapPoint (pt2));
}



void InitializeCursors (void)
{
    int i;
    static int initial_cursor_menu[] =
    {
#if defined(X_WINDOWS)
	88,			/* XC_pirate */
	150,		/* XC_watch */
	60,			/* XC_hand2 */
	2,			/* XC_arrow */
#elif defined(MACINTOSH)
	5,			/* skull and crossbones */
	7,			/* watch */
	8,			/* prompting hand */
	9,			/* right arrow */
#elif defined(_Windows)
	5,		    /* skull and crossbones */
	7,			/* watch */
	8,			/* prompting hand */
	9,			/* right arrow */
#endif
	-1
    };

    cursorMenu = DynCreate (sizeof (int), 5);

    for (i = 0; initial_cursor_menu[i] != -1; i++) {
	DynAdd (cursorMenu, (void *) &(initial_cursor_menu[i]));
	SRGP_loadCursor (i + 1, initial_cursor_menu[i]);
    }
    CurrentCursor = 0;
}



PRIVATE void LoadCommonColor (int slot, char *name)
{
    SRGP_setErrorHandlingMode (NON_FATAL_ERRORS);
    SRGP_loadCommonColor (slot, name);
    if (SRGP_errorOccurred != 0) {
	ASSERT (FALSE, (mes, "SUIT could not allocate color \"%s\", loading black instead\n",name));
	SRGP_loadCommonColor (slot, "black");
	SRGP_errorOccurred = 0;
    }
    SRGP_setErrorHandlingMode (FATAL_ERRORS);
}




PRIVATE int numberLegalColors;

void InitializeColors (int screenDep)
{
    int i;
    boolean isColor;

    char *black = "black";
    char *white = "white";
    static char *initial_color_menu[] =
    {
	"white",
	"black",
	"yellow",
	"red",
	"green",
	"blue",
	"grey",
	"brown",
#if !defined(IBM_PC) || defined(_Windows)
	"maroon",
	"orange",
	"violet",
	"pink",
	"light blue",
	"dark slate blue",
	"pale green",
	"indian red",
#endif
	NULL
    };

    screenDepth = screenDep;  /* save to the global variable */
    numberLegalColors = 1 << screenDepth;  /* this means 2 to the screenDepth power */
    colorMenu = DynCreate (sizeof (char *), numberLegalColors);

    /* special case black & white */
    DynAdd (colorMenu, (SRGP_BLACK == 0) ? (void *) &black : (void *) &white);
    DynAdd (colorMenu, (SRGP_BLACK == 1) ? (void *) &black : (void *) &white);

    isColor = (SRGP_inquireCanvasDepth () > 1);
    for (i = 2; (initial_color_menu[i] && (i < numberLegalColors)); i++) {
	DynAdd (colorMenu, (void *) &(initial_color_menu[i]));
	if (isColor)
	    LoadCommonColor (i, initial_color_menu[i]);
    }
}



/* NOTE: this routine gives back a pointer to a static buffer. It's only
 * valid until you call this routine again. */

char *GP_getColorName (int index)
{
    static char retval[100];

    ASSERT ((index >= 0), (mes, "GP_getColorName passed a bad value (%d)\n", index));

    if (index < DynSize (colorMenu))
	strcpy (retval, *((char **) DynGet (colorMenu, index)));
    else
	sprintf (retval, "%d", index);
    return (retval);
}



PRIVATE void (*RegisteredColorFunction)(void) = NULL;


void  GP_registerForColorAllocation (void (*func)())
{
    RegisteredColorFunction = func;
}


int CaseInsensitiveMatch (char *a, char *b)
{
    /*
    if (a == NULL || b == NULL)
	return 0;
    */

    if (a == b)
	return 0;

    for (; *a && *b; a++, b++) {
	char lowa = (*a >= 'A' && *a <= 'Z')? *a-'A'+'a' : *a;
	char lowb = (*b >= 'A' && *b <= 'Z')? *b-'A'+'a' : *b;
	if (lowa != lowb)
	    return lowa - lowb;
    }
    return (*a - *b);
}


int GP_getColorIndex (char *name)
{
    int i, num;
    int r, g, b;
    char *temp, *ptr;

    ASSERT (((name != NULL) && (strcmp (name, "") != 0)), (mes, "GP_getColorIndex was passed an invalid name\n"));

    if (((name) == NULL) || (strcmp (name, "") == 0))
	return SRGP_BLACK;
    if (CaseInsensitiveMatch (name, "black") == 0)
	return SRGP_BLACK;
    if (CaseInsensitiveMatch (name, "white") == 0)
	return SRGP_WHITE;

    for (i = 2; i < DynSize (colorMenu); i++) {
	ptr = *((char **) DynGet (colorMenu, i));
	if (CaseInsensitiveMatch (name, ptr) == 0) {
	    return i;
	}
    }

    if (GP_numColorsAllocated () < numberLegalColors) {
	temp = CreateSafeString (name);
	DynAdd (colorMenu, (void *) &temp);
	num = DynHigh (colorMenu);
	if (sscanf (name, "(%d %d %d)", &r, &g, &b) < 3)
	    LoadCommonColor (num, name);
	else
	    SRGP_loadSingleColor (num, (unsigned short) r, (unsigned short) g, (unsigned short) b);
	if (RegisteredColorFunction != NULL)
	    RegisteredColorFunction();
	return num;
    }
    return SRGP_BLACK;
}



int GP_numColorsAllocated (void)
{
    return (DynSize (colorMenu));
}



GP_color GP_defColor (char *name, boolean black)
{
    GP_color retval;

    retval.colorName = SUIT_createRCString (name);
    retval.blackOnMonochrome = black;
    return retval;
}



GP_color GP_defColorRGB (unsigned short red, unsigned short green, unsigned short blue, boolean black)
{
    char name[50];
    GP_color retval;

    sprintf (name, "(%d %d %d)", red, green, blue);
    retval.colorName = SUIT_createRCString (name);
    retval.blackOnMonochrome = black;
    return retval;
}



void GP_describeColor (GP_color c, unsigned short *red, unsigned short *green, unsigned short *blue)
{
    int r, g, b;
    if (sscanf (c.colorName, "(%d %d %d)", &r, &g, &b) < 3) {
	unsigned short rr, gg, bb;
	SRGP_inquireColorTable (GP_getColorIndex (c.colorName), 1, &rr, &gg, &bb);
	*red = rr;
	*green = gg;
	*blue = bb;
    } else {
	*red = (unsigned short) r;
	*green = (unsigned short) g;
	*blue = (unsigned short) b;
    }
}



void GP_setColor (GP_color c)
{
    if (((c.colorName) == NULL) || (strcmp (c.colorName, "") == 0)) {
	if (c.blackOnMonochrome)
	    c.colorName = CreateSafeString ("black");
	else
	    c.colorName = CreateSafeString ("white");
    }
    if (SRGP_inquireCanvasDepth () == 1)
	if (c.blackOnMonochrome)
	    SRGP_setColor (SRGP_BLACK);
	else
	    SRGP_setColor (SRGP_WHITE);
    else
	SRGP_setColor (GP_getColorIndex (c.colorName));
}


void GP_replaceColor (GP_color old, GP_color new)
{
    char *temp;
    unsigned int r, g, b;
    int num = GP_getColorIndex (old.colorName), i;
    DynArray newColorMenu = DynCreate (sizeof (char *), num);

    temp = CreateSafeString (new.colorName);

    for (i = DynLow (colorMenu); i < num; i++)
	DynAdd (newColorMenu, DynGet (colorMenu, i));
    DynAdd (colorMenu, (void *) &temp);
    for (i = num + 1; i <= DynHigh (colorMenu); i++)
	DynAdd (newColorMenu, DynGet (colorMenu, i));

    if (SRGP_inquireCanvasDepth () == 1)
	if (new.blackOnMonochrome)
	    SRGP_setColor (SRGP_BLACK);
	else
	    SRGP_setColor (SRGP_WHITE);
    else if (sscanf (new.colorName, "(%u %u %u)", &r, &g, &b) < 3)
	LoadCommonColor (num, new.colorName);
    else
	SRGP_loadSingleColor (num, r, g, b);
}



void GP_setCursor (int i)
{
#if defined(X_WINDOWS) || defined(MACINTOSH) || defined(_Windows)
    ASSERT ((i <= DynSize (cursorMenu) && (i >= 0)),
	    (mes, "GP_setCursor was passed a bad value (%d)\n", i));
    CurrentCursor = i;
    SRGP_setLocatorEchoCursorShape (i);
    SRGP_refresh();
#endif
}



GP_time GP_getCurrentTime (void)
{
    GP_time retval;

#ifdef MACINTOSH
	long tick = TickCount();
    retval.secs = (int)((double)tick / 60.0);
    retval.millisecs = (int)((tick % 60) * (double)1000.0 / 60.0);
#endif

#ifdef X_WINDOWS
    struct timeval tv;
    struct timezone tz;
#ifdef SUN
    int gettimeofday(struct timeval *tp, struct timezone *tzp);
#endif
    gettimeofday (&tv, &tz);
    retval.secs = tv.tv_sec;
    retval.millisecs = tv.tv_usec / 1000;
#endif

#ifdef _Windows
    time_t seconds=time(NULL);
    struct time t;
    gettime (&t);
    retval.secs = seconds;
    retval.millisecs = t.ti_hund * 10;
#elif defined (IBM_PC)
    struct time t;
    gettime (&t);
    retval.secs = t.ti_sec;
    retval.millisecs = t.ti_hund * 10;	                        /* convert from hundredths */
#endif

    return retval;
}



time_t GP_timeDifference (GP_time t1, GP_time t2)
{
    return 1000 * (t1.secs - t2.secs) + t1.millisecs - t2.millisecs;
}



void GP_convertTime (GP_time t, int *hour, int *min, int *sec, int *milli)
{
    struct tm tms;
    tms = *(localtime (&t.secs));
    *hour = tms.tm_hour;
    *min = tms.tm_min;
    *sec = tms.tm_sec;
    *milli = t.millisecs;
}



PRIVATE void MakeLowerCase (char *str)
{
    int i;
    for (i=0; i < strlen(str); i++)
	if (str[i] >= 'A' && str[i] <= 'Z')
	    str[i] -= 'A'-'a';
}


/* This routine returns whether s2 is a substring of s1 (case-INsensitive).
   Returns < 0 if no match.
*/

int SUIT_stringContains (char *s1, char *s2)
{
    char *lcase1 = SUIT_copyString(s1);
    char *lcase2 = SUIT_copyString(s2);
    int i, j, k, retval = -1;

    MakeLowerCase (lcase1);
    MakeLowerCase (lcase2);
    for (i = 0; i < strlen (lcase1); i++)
	if (lcase1[i] == lcase2[0]) {
	    for (j = 1, k = i + 1; j < strlen (lcase2) && lcase1[k] == lcase2[j]; j++, k++) ;
	    if (j == strlen (lcase2)) {
		retval = i;
		break;
	    }
	}
    SUIT_free(lcase1);
    SUIT_free(lcase2);
    return retval;
}


#if defined(X_WINDOWS)
PRIVATE char *XFontList[] =    { "new century schoolbook", "charter", "courier",
                                "helvetica", "lucida", "times", NULL };
PRIVATE char *XStyleList[] =   { "bold", "italic", NULL };

#elif defined(MACINTOSH)
PRIVATE char *MacFontList[] =  { "Chicago", "Courier", "Helvetica", "New York", "Times", NULL };
PRIVATE char *MacStyleList[] = { "bold", "condense", "extend", "italic",
                                "outline", "shadow", "underline", NULL };

#elif defined(SGI_GL)
PRIVATE char *SGIFontList[] =  { "Avant Garde", "Bookman", "Charter", "Courier", "Helvetica Narrow",
			        "Helvetica", "New Century Schoolbook", "Palatino", "Times", NULL };
PRIVATE char *SGIStyleList[] =   { "bold", "italic", NULL };

#elif defined(_Windows)
PRIVATE char *WinFontList[] = {"Courier", "Modern", "Helvetica", "Times", "Roman", "Script", NULL };
PRIVATE char *WinStyleList[] = {"bold", "italic", "underline", "strikeout", NULL};

#else
PRIVATE char *IBMFontList[] =  { "SYTEM08.FNT", NULL};
PRIVATE char *IBMStyleList[] = { NULL };
#endif



char **GP_possibleFonts (void)
{
#if defined(X_WINDOWS)
    return XFontList;
#elif defined(MACINTOSH)
    return MacFontList;
#elif defined(SGI_GL)
    return SGIFontList;
#elif defined(_Windows)
    return WinFontList;
#else
    return IBMFontList;
#endif
}



char **GP_possibleStyles (void)
{
#if defined(X_WINDOWS)
    return XStyleList;
#elif defined(MACINTOSH)
    return MacStyleList;
#elif defined(_Windows)
    return WinStyleList;
#elif defined(IBM_PC)
    return IBMStyleList;
#elif defined(SGI_GL)
    return SGIStyleList;
#endif
}

#ifdef X_WINDOWS
int ProduceXFontName (char *FontName, char *name, char *style, double points)
{
    char buf2[50];
    int size = (int) (points + 0.5);
    int retval = 1;

    strcpy (FontName, "-*-");
    if (strcmp (name, "courier") == 0 ||
	strcmp (name, "helvetica") == 0 ||
	strcmp (name, "new century schoolbook") == 0 ||
	strcmp (name, "times") == 0 ||
	strcmp (name, "lucida") == 0 ||
	strcmp (name, "charter") == 0)
	strcat (FontName, name);
    else {
	fprintf(stderr, "ProduceXFontName: bad font name `%s'\n", name);
	strcat (FontName, "times");
	retval = 0;
    }

    strcat (FontName, "-");
    strcat (FontName, (SUIT_stringContains (style, "bold") >= 0) ? "bold" : "medium");
    strcat (FontName, "-");

    if (strcmp (name, "courier") == 0 ||
	strcmp (name, "helvetica") == 0)
	strcat (FontName, (SUIT_stringContains (style, "italic") >= 0) ? "o" : "r");
    else
	strcat (FontName, (SUIT_stringContains (style, "italic") >= 0) ? "i" : "r");
    strcat (FontName, "-normal-*-*-");

    if (size <= 8)
	size = 8;
    else if (size <= 10)
	size = 10;
    else if (size <= 12)
	size = 12;
    else if (size <= 14)
	size = 14;
    else if (size <= 18)
	size = 18;
    else
	size = 24;
    sprintf (buf2, "%d-*-*-*-*-*-*", size * 10);
    strcat (FontName, buf2);
    return retval;
}
#endif

#ifdef MACINTOSH
PRIVATE int ProduceMacFontName (char *FontName, char *name, char *style, double points)
{
    int size = (int) (points + 0.5);
    char buf2[5];

    if (CaseInsensitiveMatch (name, "times") == 0 ||
	CaseInsensitiveMatch (name, "courier") == 0 ||
	CaseInsensitiveMatch (name, "helvetica") == 0 ||
	CaseInsensitiveMatch (name, "chicago") == 0 ||
	CaseInsensitiveMatch (name, "new york") == 0)
	strcpy (FontName, name);
    else {
	strcpy (FontName, "Times.12");
	return 0;
    }
    if (size <= 0)
	size = 1;
    sprintf (buf2, ".%d.", size);
    strcat (FontName, buf2);
    if (SUIT_stringContains (style, "bold") >= 0)
	strcat (FontName, "b");
    if (SUIT_stringContains (style, "italic") >= 0)
	strcat (FontName, "i");
    if (SUIT_stringContains (style, "underline") >= 0)
	strcat (FontName, "u");
    if (SUIT_stringContains (style, "outline") >= 0)
	strcat (FontName, "o");
    if (SUIT_stringContains (style, "shadow") >= 0)
	strcat (FontName, "s");
    if (SUIT_stringContains (style, "condense") >= 0)
	strcat (FontName, "c");
    if (SUIT_stringContains (style, "extend") >= 0)
	strcat (FontName, "e");
    return 1;
}
#endif

#ifdef _Windows
PRIVATE int ProduceWinFontName (char *FontName, char *name, char *style, double points)
{
    int size = (int) (points + 0.5);
    char buf2[5];

    if (CaseInsensitiveMatch (name, "Courier") == 0 ||
	CaseInsensitiveMatch (name, "Modern") == 0 ||
	CaseInsensitiveMatch (name, "Script") == 0 ||
	CaseInsensitiveMatch (name, "Roman") == 0)
	strcpy (FontName, name);
	else if (CaseInsensitiveMatch (name, "Helvetica") == 0) {
		strcpy (FontName, "Helv");
	} else if (CaseInsensitiveMatch (name, "Times") == 0) {
		strcpy (FontName, "Tms Rmn");
	} else {
	strcpy (FontName, "Tms Rmn.12");
	return 0;
    }
    if (size <= 0)
	size = 1;
    sprintf (buf2, ".%d.", size);
    strcat (FontName, buf2);
    if (SUIT_stringContains (style, "bold") >= 0)
	strcat (FontName, "b");
    if (SUIT_stringContains (style, "italic") >= 0)
	strcat (FontName, "i");
    if (SUIT_stringContains (style, "underline") >= 0)
	strcat (FontName, "u");
    if (SUIT_stringContains (style, "strikeout") >= 0)
	strcat (FontName, "s");
    return 1;
}
#endif


#if defined(IBM_PC) && !defined(_Windows)
PRIVATE int ProduceIBMFontName (char *FontName, char *name, char *style, double points)
{
    strcat (FontName, "SYSTEM08.FNT");
    return 0;
}
#endif


#ifdef SGI_GL
PRIVATE int ProduceSGIFontName (char *FontName, char *name, char *style, double points)
{
    char *Fname, *Fstyle;

    if (CaseInsensitiveMatch (name, "Bookman") == 0)
	Fname = "Bookman";
    else if (CaseInsensitiveMatch (name, "Charter") == 0)
	Fname = "Charter";
    else if (CaseInsensitiveMatch (name, "Courier") == 0)
	Fname = "Courier";
    else if (CaseInsensitiveMatch (name, "Helvetica") == 0)
	Fname = "Helvetica";
    else if (CaseInsensitiveMatch (name, "Palatino") == 0)
	Fname = "Palatino";
    else if (CaseInsensitiveMatch (name, "Times") == 0)
	Fname = "Times";
    else if (CaseInsensitiveMatch (name, "Avant Garde") == 0)
	Fname = "AvantGarde";
    else if (CaseInsensitiveMatch (name, "Helvetica Narrow") == 0)
	Fname = "Helvetica-Narrow";
    else if (CaseInsensitiveMatch (name, "New Century Schoolbook") == 0)
	Fname = "NewCenturySchlbk";
    else {
	fprintf (stderr, "ProduceSGIFontName: bad font name %s\n",name);
	Fname = "Times";
    }

    if (CaseInsensitiveMatch (name, "Courier") == 0 ||
	CaseInsensitiveMatch (name, "Helvetica") == 0 ||
	CaseInsensitiveMatch (name, "Helvetica Narrow") == 0) {
	if (SUIT_stringContains (style, "bold") >= 0)
	    if (SUIT_stringContains (style, "italic") >= 0)
		Fstyle = "BoldOblique";
	    else
		Fstyle = "Bold";
	else if (SUIT_stringContains (style, "italic") >= 0)
	    Fstyle = "Oblique";
	else
	    Fstyle = "";
    } else if (CaseInsensitiveMatch (name, "New Century Schoolbook") == 0 ||
	       CaseInsensitiveMatch (name, "Palatino") == 0 ||
	       CaseInsensitiveMatch (name, "Times") == 0) {
	if (SUIT_stringContains (style, "bold") >= 0)
	    if (SUIT_stringContains (style, "italic") >= 0)
		Fstyle = "BoldItalic";
	    else
		Fstyle = "Bold";
	else if (SUIT_stringContains (style, "italic") >= 0)
	    Fstyle = "Italic";
	else
	    Fstyle = "Roman";
    } else if (CaseInsensitiveMatch (name, "Avant Garde") == 0) {
	if (SUIT_stringContains (style, "bold") >= 0)
	    if (SUIT_stringContains (style, "italic") >= 0)
		Fstyle = "DemiOblique";
	    else
		Fstyle = "Demi";
	else if (SUIT_stringContains (style, "italic") >= 0)
	    Fstyle = "BookOblique";
	else
	    Fstyle = "Book";
    } else if (CaseInsensitiveMatch (name, "Bookman") == 0) {
	if (SUIT_stringContains (style, "bold") >= 0)
	    if (SUIT_stringContains (style, "italic") >= 0)
		Fstyle = "DemiItalic";
	    else
		Fstyle = "Demi";
	else if (SUIT_stringContains (style, "italic") >= 0)
	    Fstyle = "LightItalic";
	else
	    Fstyle = "Light";
    } else if (CaseInsensitiveMatch (name, "Charter") == 0) {
	if (SUIT_stringContains (style, "bold") >= 0)
	    if (SUIT_stringContains (style, "italic") >= 0)
		Fstyle = "BlackItalic";
	    else
		Fstyle = "Black";
	else if (SUIT_stringContains (style, "italic") >= 0)
	    Fstyle = "Italic";
	else
	    Fstyle = "Roman";
    }

    if (points <= 0) {
	fprintf (stderr, "ProduceSGIFontName: bad point size (%.2f)\n",points);
	points = 12.0;
    }
    if (SUIT_stringsMatch (Fstyle, ""))
	sprintf (FontName, "%s %.2f",Fname, points);
    else
	sprintf (FontName, "%s-%s %.2f",Fname, Fstyle, points);
    return 0;
}
#endif


typedef struct {
    char *name;
    int slot;
} FontType;
PRIVATE int FontsArrayExists = 0;
PRIVATE DynArray FontsAlreadyLoaded;


PRIVATE int CompareNames (void *first, void *second)
{
    FontType one, two;
    one = *(FontType *) first;
    two = *(FontType *) second;
    return strcmp (one.name, two.name);
}



PRIVATE FontType *DummyFontType (char *name)
{
    static FontType f;
    f.name = name;
    return &f;
}


int GP_setFont (GP_font newfont)
{
    char buf[80];
    FontType newguy;
    int status, where, i;

    if (!FontsArrayExists) {
	FontsAlreadyLoaded = DynCreate (sizeof (FontType), 1);
	FontsArrayExists = 1;
    }

#if defined(X_WINDOWS)
    status = ProduceXFontName (buf, newfont.family, newfont.style, newfont.pointSize);
#elif defined(MACINTOSH)
    status = ProduceMacFontName (buf, newfont.family, newfont.style, newfont.pointSize);
#elif defined(_Windows)
    status = ProduceWinFontName (buf, newfont.family, newfont.style, newfont.pointSize);
#elif defined(IBM_PC)
    status = ProduceIBMFontName (buf, newfont.family, newfont.style, newfont.pointSize);
#elif defined(SGI_GL)
    status = ProduceSGIFontName (buf, newfont.family, newfont.style, newfont.pointSize);
#endif

    if ((i = DynFindIndex (FontsAlreadyLoaded, DummyFontType (buf), CompareNames)) == DYN_NOT_FOUND) {
	if (DynSize (FontsAlreadyLoaded) == MAX_FONT_INDEX + 1)
	    DynDelete (FontsAlreadyLoaded, DynLow (FontsAlreadyLoaded));
	newguy.name = CreateSafeString (buf);
	newguy.slot = where = DynHigh (FontsAlreadyLoaded) + 1;
	SRGP_setErrorHandlingMode (NON_FATAL_ERRORS);
	SRGP_loadFont (where, buf);
	if (SRGP_errorOccurred != 0) {
	    where = 0;
	    SRGP_errorOccurred = 0;
	} else {
	    DynAdd (FontsAlreadyLoaded, (void *) &newguy);
	}
	SRGP_setErrorHandlingMode (FATAL_ERRORS);
    } else {
	where = ((FontType *) DynGet (FontsAlreadyLoaded, i))->slot;
    }
    SRGP_setFont (where);
    CurrentFontFamily = newfont.family;
    CurrentFontStyle = newfont.style;
    CurrentFontPointSize = newfont.pointSize;
    return status;
}


GP_font GP_defFont (char *family, char *style, double pointSize)
{
    GP_font retval;

    retval.family = CreateSafeString (family);
    retval.style = CreateSafeString (style);
    retval.pointSize = pointSize;
    return retval;
}


void GP_justifyText (char *str, GP_justification just)
{
    double x, y;
    double width, ascent, descent, height;

    GP_inquireTextExtent (str, &width, &ascent, &descent);
    height = ascent + descent;

    switch (just) {
      case JUSTIFY_BOTTOM_LEFT:
      case JUSTIFY_CENTER_LEFT:
      case JUSTIFY_TOP_LEFT:
	x = world.bottom_left.x;
	break;
      case JUSTIFY_BOTTOM_RIGHT:
      case JUSTIFY_CENTER_RIGHT:
      case JUSTIFY_TOP_RIGHT:
	x = world.top_right.x - width;
	break;
      case JUSTIFY_BOTTOM_CENTER:
      case JUSTIFY_CENTER:
      case JUSTIFY_TOP_CENTER:
	x = world.bottom_left.x + (world.top_right.x - world.bottom_left.x - width) / 2;
	break;
      default:
	x = 0;
	break;
    }
    switch (just) {
      case JUSTIFY_BOTTOM_LEFT:
      case JUSTIFY_BOTTOM_CENTER:
      case JUSTIFY_BOTTOM_RIGHT:
	y = world.bottom_left.y;
	break;
      case JUSTIFY_TOP_LEFT:
      case JUSTIFY_TOP_CENTER:
      case JUSTIFY_TOP_RIGHT:
	y = world.top_right.y - height;
	break;
      case JUSTIFY_CENTER_LEFT:
      case JUSTIFY_CENTER:
      case JUSTIFY_CENTER_RIGHT:
	y = world.bottom_left.y + (world.top_right.y - world.bottom_left.y - height) / 2 + descent;
	break;
      default:
	y = 0;
	break;
    }
    GP_text (GP_defPoint (x, y), str);
}


void GP_justifyTextInRectangle (char *text, GP_justification just, GP_rectangle r)
{
    GP_pushGraphicsState();
    GP_setViewport (GP_mapRectangle (r));
    GP_justifyText (text, just);
    GP_popGraphicsState();
}



typedef struct {
    char *abbrev;
    char ascii;
} char_str;


#ifdef X_WINDOWS
char_str ExtendedChars[] = {
    "ae", 230, "AE", 198, "cents", 162, "division", 247,
    "times", 215, "plusminus", 177, "paragraph", 182, "subsection", 167,
    "copyright", 169, "restricted", 174, "pounds", 163, "yen", 165,
    "<<", 171, ">>", 187, "?", 191, "!", 161, "ss", 223, NULL, -1
};
#endif


#ifdef MACINTOSH
char_str ExtendedChars[] = {
    "ae", 190, "AE", 174, "cents", 162, "division", 214,
    "times", 'x', "plusminus", 177, "paragraph", 166, "subsection", 164,
    "copyright", 169, "restricted", 168, "pounds", 163, "yen", 180,
    "<<", 199, ">>", 200, "?", 192, "!", 193, "ss", 167, NULL, -1
};
#endif


#ifdef _Windows
char_str ExtendedChars[] = {
	"ae", 230, "AE", 198, "cents", 162, "division", 247,
    "times", 215, "plusminus", 177, "paragraph", 182, "subsection", 167,
    "copyright", 169, "restricted", 174, "pounds", 163, "yen", 165,
    "<<", 171, ">>", 187, "?", 191, "!", 161, "ss", 223, NULL, -1
};
#elif defined (IBM_PC)
char_str ExtendedChars[] = {NULL, -1};
#endif


typedef struct {
    char unaccented, accented;
} accent_str;


#ifdef X_WINDOWS
accent_str acute[] = {'a', 225, 'e', 233, 'i', 237, 'o', 243, 'u', 250, 'y', 253,
'A', 193, 'E', 201, 'I', 205, 'O', 211, 'U', 218, 'Y', 221, -1, -1};
accent_str grave[] = {'a', 224, 'e', 232, 'i', 236, 'o', 242, 'u', 249,
'A', 192, 'E', 200, 'I', 204, 'O', 210, 'U', 217, -1, -1};
accent_str circumflex[] = {'a', 226, 'e', 234, 'i', 238, 'o', 244, 'u', 251,
'A', 194, 'E', 202, 'I', 206, 'O', 212, 'U', 219, -1, -1};
accent_str umlaut[] = {'a', 228, 'e', 235, 'i', 239, 'o', 246, 'u', 252, 'y', 255,
'A', 196, 'E', 203, 'I', 207, 'O', 214, 'U', 220, -1, -1};
accent_str tilde[] = {'a', 227, 'o', 245, 'n', 241,
'A', 195, 'O', 213, 'N', 209, -1, -1};
accent_str cedilla[] = {'c', 231, 'C', 199, -1, -1};

#define ACUTE      180
#define GRAVE      '`'
#define CEDILLA    184
#define UMLAUT     168
#define TILDE      '~'
#define CIRCUMFLEX '^'
#endif


#ifdef MACINTOSH
accent_str acute[] = { 'a', 135, 'e', 142, 'i', 146, 'o', 151, 'u', 156, 'A', 231,
'E', 131, 'I', 234, 'O', 238, 'U', 242, -1, -1};
accent_str grave[] = { 'a', 136, 'e', 143, 'i', 147, 'o', 152, 'u', 157, 'A', 203,
'E', 233, 'I', 237, 'O', 241, 'U', 244, -1, -1};
accent_str circumflex[] = { 'a', 137, 'e', 144, 'i', 148, 'o', 153, 'u', 158, 
'A', 229, 'E', 230, 'I', 235, 'O', 239, 'U', 243, -1, -1};
accent_str umlaut[] = { 'a', 138, 'e', 145, 'i', 149, 'o', 154, 'u', 159, 'y', 216,
'A', 128, 'E', 232, 'I', 236, 'O', 133, 'U', 134, 'Y', 217, -1, -1};
accent_str tilde[] = { 'a', 139, 'o', 155, 'n', 150, 'A', 204, 'O', 205, -1, -1};
accent_str cedilla[] = { 'c', 141, 'C', 130, -1, -1};

#define ACUTE      '\''
#define GRAVE      '`'
#define CEDILLA    ','
#define UMLAUT     '"'
#define TILDE      '~'
#define CIRCUMFLEX '^'
#endif


#ifdef _Windows
accent_str acute[] = {'a', 225, 'e', 233, 'i', 237, 'o', 243, 'u', 250, 'y', 253,
'A', 193, 'E', 201, 'I', 205, 'O', 211, 'U', 218, 'Y', 221, -1, -1};
accent_str grave[] = {'a', 224, 'e', 232, 'i', 236, 'o', 242, 'u', 249,
'A', 192, 'E', 200, 'I', 204, 'O', 210, 'U', 217, -1, -1};
accent_str circumflex[] = {'a', 226, 'e', 234, 'i', 238, 'o', 244, 'u', 251,
'A', 194, 'E', 202, 'I', 206, 'O', 212, 'U', 219, -1, -1};
accent_str umlaut[] = {'a', 228, 'e', 235, 'i', 239, 'o', 246, 'u', 252, 'y', 255,
'A', 196, 'E', 203, 'I', 207, 'O', 214, 'U', 220, -1, -1};
accent_str tilde[] = {'a', 227, 'o', 245, 'n', 241,
'A', 195, 'O', 213, 'N', 209, -1, -1};
accent_str cedilla[] = {'c', 231, 'C', 199, -1, -1};

#define ACUTE      180
#define GRAVE      '`'
#define CEDILLA    184
#define UMLAUT     168
#define TILDE      '~'
#define CIRCUMFLEX '^'

#elif defined(IBM_PC)
accent_str acute[] = {-1, -1};
accent_str grave[] = {-1, -1};
accent_str circumflex[] = {-1, -1};
accent_str umlaut[] = {-1, -1};
accent_str tilde[] = {-1, -1};
accent_str cedilla[] = {-1, -1};

#define ACUTE      '\''
#define GRAVE      '`'
#define CEDILLA    ','
#define UMLAUT     '"'
#define TILDE      '~'
#define CIRCUMFLEX '^'
#endif


PRIVATE void AccentLetter (point p, char letter, char accent)
{
    accent_str *table;
    int i, w, a, d, height;
    char buf[2];

    buf[1] = '\0';

    switch (accent) {
      case '\'':	table = acute;		break;
      case '`':    	table = grave;		break;
      case '^': 	table = circumflex;	break;
      case ':': 	table = umlaut; 	break;
      case '~': 	table = tilde;  	break;
      case ',': 	table = cedilla; 	break;
      default:  	table = tilde;
	fprintf (stderr, "Accent: bad accent (%c), using tilde instead\n", accent);
	break;
    }

    /* do I have an ASCII character for this accented letter? */
    for (i = 0; (table + i)->unaccented > 0; i++)
	if ((table + i)->unaccented == letter) {
	    buf[0] = (table + i)->accented;
	    SRGP_text (p, buf);
	    return;
	}

    /* no ASCII letter exists, so fake it */
    buf[0] = letter;
    SRGP_text (p, buf);
    SRGP_inquireTextExtent (buf, &w, &a, &d);
    height = a;

    switch (accent) {
      case '\'':	buf[0] = ACUTE; 	break;
      case '`': 	buf[0] = GRAVE; 	break;
      case '^': 	buf[0] = CIRCUMFLEX; 	break;
      case ':': 	buf[0] = UMLAUT; 	break;
      case '~': 	buf[0] = TILDE; 	break;
      case ',': 	buf[0] = CEDILLA; 	break;
      default:
	fprintf (stderr, "Accent: bad accent (%c)\n", accent);
	break;
    }

    if (accent != ',') {
	SRGP_inquireTextExtent (buf, &w, &a, &d);
	SRGP_text (SRGP_defPoint (p.x, p.y + height + 2 - a), buf);
    }
    else
	SRGP_text (p, buf);
}



typedef struct {
    char *command;
    GP_rectangle (*specialCharCallback) (GP_point, char *);
} RegisteredSpecialCharCallBack;


typedef struct {
    char *command;
    GP_rectangle (*styleCallback) (GP_point, char *, char *);
} RegisteredStyleCallBack;


PRIVATE DynArray RegisteredSpecialChars;
PRIVATE DynArray RegisteredStyles;


void InitializeFonts (void)
{
    GP_setFont (GP_defFont ("times", "", 12.0));
    RegisteredSpecialChars = DynCreate (sizeof (RegisteredSpecialCharCallBack), 1);
    RegisteredStyles = DynCreate (sizeof (RegisteredStyleCallBack), 1);
}



void GP_registerSpecialCharacter (char *character, GP_rectangle (*funct) (GP_point, char *))
{
    RegisteredSpecialCharCallBack f;
    f.command = (char *) SUIT_malloc (strlen (character) + 1);
    strcpy (f.command, character);
    f.specialCharCallback = funct;
    DynAdd (RegisteredSpecialChars, (void *) &f);
}


void GP_registerStyle (char *character, GP_rectangle (*funct) (GP_point, char *, char *))
{
    RegisteredStyleCallBack f;
    f.command = (char *) SUIT_malloc (strlen (character) + 1);
    strcpy (f.command, character);
    f.styleCallback = funct;
    DynAdd (RegisteredStyles, (void *) &f);
}



PRIVATE rectangle drawPlainText (point pt, char *str)
{
    int width, ascent, descent;
    char *copy = SUIT_copyString(str);
    SRGP_inquireTextExtent (str, &width, &ascent, &descent);
    SRGP_text (pt, copy);
    SUIT_free(copy);
    return SRGP_defRectangle (pt.x, pt.y - descent, pt.x + width, pt.y + ascent);
}



rectangle addBoundingBoxes(rectangle a, rectangle b)
{
    return SRGP_defRectangle(a.bottom_left.x, MIN(a.bottom_left.y, b.bottom_left.y),
			     a.top_right.x + (b.top_right.x - b.bottom_left.x),
			     MAX(a.top_right.y, b.top_right.y));
}



PRIVATE rectangle drawFancyText (point pt, char *str)
{
    char *beforeTheAt;		  /* the substring before the '@' */
    char *afterTheAt;             /* the substring after the '@' */
    char *command, *inside, *end;
    rectangle retval;
    point location;
    char buf[2];
    int n;
    
    retval = SRGP_defRectangle (pt.x, pt.y, pt.x, pt.y);
    location = pt;

    if (strchr (str, '@') == NULL){		/* if there's no '@' in the string, just draw it */
	retval = drawPlainText (pt, str);
	return retval;
    }

    beforeTheAt = (char *) SUIT_malloc (strlen (str) + 1);
    afterTheAt = (char *) SUIT_malloc (strlen (str) + 1);
    beforeTheAt[0] = afterTheAt[0] = '\0';
    
    /* first separate the string into two substrings: stuff before '@' and stuff after '@' */
    if (str[0] == '@') {			/* if the very first character is the '@' */
	beforeTheAt[0] = '\0';
	n = sscanf (str + 1, "%[^\n]", afterTheAt) + 1;
    } else
	n = sscanf (str, "%[^@]@%[^\n]", beforeTheAt, afterTheAt);

    if (n == 2)	{				/* so far, so good => paint what before the '@' */
	retval = addBoundingBoxes (retval, drawPlainText (location, beforeTheAt));
	location = SRGP_defPoint (retval.top_right.x, pt.y);
    }
    else {					/* there some kind of mistake, bomb out */
	SUIT_free (beforeTheAt);
	SUIT_free (afterTheAt);
	return retval;
    }

    command = (char *) SUIT_malloc (strlen (str) + 1); /* the special character or style */
    inside = (char *) SUIT_malloc (strlen (str) + 1);  /* what's inside the command */
    end = (char *) SUIT_malloc (strlen (str) + 1);     /* what's after the command */
    command[0] = inside[0] = end[0] = '\0';
    
    if (afterTheAt[0] == '@') {			/* this is a double '@', e.g. rad2r@@virginia.edu */
	/* draw the '@' */
	retval = addBoundingBoxes (retval, drawPlainText (location, "@"));
	location = SRGP_defPoint (retval.top_right.x, pt.y);

	/* now draw the rest of the text */
	retval = addBoundingBoxes (retval, drawFancyText (location, afterTheAt+1));
    }

    else if (afterTheAt[0] == '(') {		/* this is a special character, e.g. @(copyright) */
	boolean found = FALSE;
	int i;

	buf[1] = '\0';
	sscanf (afterTheAt, "(%[^)])%[^\n]", command, end);

	/* is this a user-registered special character? */
	for (i = 0; !found && i <= DynHigh (RegisteredSpecialChars); i++) {
	    RegisteredSpecialCharCallBack f;
	    f = * (RegisteredSpecialCharCallBack *) DynGet (RegisteredSpecialChars, i);
	    if (SUIT_stringsMatch (f.command, command)) {
		rectangle temp;
		temp = GP_mapRectangle(f.specialCharCallback (GP_unMapPoint(location), f.command));
		retval = addBoundingBoxes (retval, temp);
		location = SRGP_defPoint (retval.top_right.x, pt.y);
		found = TRUE;
	    }
	}
	for (i = 0; !found && ExtendedChars[i].abbrev != NULL; i++)
	    if (SUIT_stringsMatch (ExtendedChars[i].abbrev, command)) {
		buf[0] = ExtendedChars[i].ascii;
		retval = addBoundingBoxes (retval, drawPlainText (location, buf));
		location = SRGP_defPoint (retval.top_right.x, pt.y);
		found = TRUE;
	    }
	retval = addBoundingBoxes (retval, drawFancyText (location, end));
    }

    else {					/* this is a style, e.g. @bold(this is bold text) */
	char *endptr;
	char *newfont = (char *) SUIT_malloc (strlen (CurrentFontStyle) + 20);
	char firstChar;
	boolean found = FALSE;
	int i, paren = 0;
	
	/* parse the rest of the string into the command, its argument and the rest of the string */
	/* e.g. @bold(hello) world ==> command = "bold", inside = "hello", end = " world" */

	(void) strcpy (command, afterTheAt);
	sscanf (afterTheAt, "%[^(](%[^\v]", command, end);
	for (i = 0; i < strlen(end) && (end[i] != ')' || paren != 0); i++)
	    if (end[i] == '(')
		paren++;
	    else if (end[i] == ')')
		paren--;
	for (; end[i] == ')'; i++) ;
	endptr = &end[i];
	strncpy (inside, end, i - 1);
	inside[i - 1] = '\0';
	
	if (SUIT_stringsMatch (command, "superscript"))
	    firstChar = '+';
	else if (SUIT_stringsMatch (command, "subscript"))
	    firstChar = '-';
	else
	    firstChar = command[0];
	
	GP_pushGraphicsState ();
	strcpy (newfont, CurrentFontStyle);
	
	/* is this a user-registered style? */
	for (i = 0; !found && i <= DynHigh (RegisteredStyles); i++) {
	    RegisteredStyleCallBack f;
	    f = * (RegisteredStyleCallBack *) DynGet (RegisteredStyles, i);
	    if (SUIT_stringsMatch (f.command, command)) {
		rectangle temp;
		temp = GP_mapRectangle (f.styleCallback (GP_unMapPoint(location), f.command, inside));
		retval = addBoundingBoxes (retval, temp);
		location = SRGP_defPoint (retval.top_right.x, pt.y);
		found = TRUE;
	    }
	}
	
	if (!found)
	    switch (firstChar) {

	      case 'i': {			/* italic */
		  GP_setFont (GP_defFont (CurrentFontFamily,
					  strcat (newfont, "italic"),
					  CurrentFontPointSize));
		  retval = addBoundingBoxes (retval, drawFancyText (location, inside));
		  location = SRGP_defPoint (retval.top_right.x, pt.y);
		  break;
	      }

	      case 'b': {			/* bold */
		  GP_setFont (GP_defFont (CurrentFontFamily,
					  strcat (newfont, "bold"),
					  CurrentFontPointSize));
		  retval = addBoundingBoxes (retval, drawFancyText (location, inside));
		  location = SRGP_defPoint (retval.top_right.x, pt.y);
		  break;
	      }

	      case 'u': {			/* underline */
		  point pt1, pt2;
		  pt1 = location;
		  retval = addBoundingBoxes (retval, drawFancyText (location, inside));
		  pt2 = location = SRGP_defPoint (retval.top_right.x, pt.y);
		  pt1.y -= 2;  pt2.y -= 2;
		  SRGP_line (pt1, pt2);
		  break;
	      }

	      case 'c': {			/* centered */
		  int w, a, d;
		  point mid;
		  mid = location;
		  GP_inquireTextExtentWithoutMapping (inside, &w, &a, &d);
		  mid.x = view.bottom_left.x + (view.top_right.x - view.bottom_left.x - w)/2;
		  retval = addBoundingBoxes (retval, drawFancyText (mid, inside));
		  break;
	      }

	      case '+':
	      case '-': {			/* superscript/subscript */
		  int w, a, d;
		  point temp;

		  temp = location;
		  SRGP_inquireTextExtent ("D", &w, &a, &d);
		  GP_setFont (GP_defFont (CurrentFontFamily,
					  CurrentFontStyle,
					  CurrentFontPointSize - 2));
		  if (firstChar == '+')
		      temp.y += (a + d) / 2;
		  else
		      temp.y -= (a + d) / 2;
		  retval = addBoundingBoxes (retval, drawFancyText (temp, inside));
		  location = SRGP_defPoint (retval.top_right.x, pt.y);
		  break;
	      }

	      case 's': {			/* smallcaps ??? */
		  break;
	      }

	      case ',':				/* accents */
	      case '~':
	      case '^':
	      case ':':
	      case '`':
	      case '\'':{
		  for (i = 0; i < strlen(inside); i++) {
		      char buf2[2];
		      int w, a, d;
		      AccentLetter (location, inside[i], firstChar);
		      buf2[0] = inside[i];
		      buf2[1] = '\0';
		      SRGP_inquireTextExtent (buf2, &w, &a, &d);
		      location.x += w;
		  }
		  retval.top_right.x = location.x;
		  break;
	      }

	      default:{
		  fprintf (stderr, "GP_text:  unknown @ command `%s'\n", command);
		  retval = addBoundingBoxes (retval, drawFancyText (location, inside));
		  location = SRGP_defPoint (retval.top_right.x, pt.y);
		  break;
	      }

	    }

	GP_popGraphicsState ();
	if (n == 2)
	    retval = addBoundingBoxes (retval, drawFancyText (location, endptr));
	SUIT_free (newfont);
    }

    SUIT_free (beforeTheAt);
    SUIT_free (command);
    SUIT_free (inside);
    SUIT_free (afterTheAt);
    SUIT_free (end);

    return retval;
}



GP_rectangle GP_text (GP_point pt, char *str)
{
    return GP_unMapRectangle (drawFancyText (GP_mapPoint(pt), str));
}



void GP_inquireTextExtentWithoutMapping (char *str, int *width, int *ascent, int *descent)
{
    rectangle boundingBox;

    /* this is sneaky, but I kinda like it :^>   Rob */
    GP_pushGraphicsState();
    SRGP_setClipRectangle (SRGP_defRectangle(0,0,0,0));

    boundingBox = drawFancyText (SRGP_defPoint(0, 0), str);

    GP_popGraphicsState();

    *width = boundingBox.top_right.x - boundingBox.bottom_left.x;
    *ascent = boundingBox.top_right.y;
    *descent = -boundingBox.bottom_left.y;
}



void GP_inquireTextExtent (char *str, double *width, double *ascent, double *descent)
{
    int wid, asc, desc;

    GP_inquireTextExtentWithoutMapping (str, &wid, &asc, &desc);

    *width = GP_unMapWidth (wid);
    *ascent = GP_unMapHeight (asc);
    *descent = GP_unMapHeight (desc);
}
