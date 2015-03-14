/* (C) 1990 Copyright Rector and Visitors of the University of Virginia */


#ifndef PRIVSUIT_INCLUDED
#define PRIVSUIT_INCLUDED 1

#include "suit.h"


#ifdef SUITMAIN
boolean srgpHasBegun;
boolean hintsFileRead;
#else
extern boolean srgpHasBegun;
extern boolean hintsFileRead;
#endif



#if defined(SUN) && !defined(SGI_X) /* this gets rid of some dumb compiler warnings */
#if !defined(linux)
extern void printf ();
extern void fprintf ();
extern char *sprintf ();
extern void fclose ();
#endif
extern int sscanf ();
extern int fscanf ();
extern int fgetc ();
extern int remove ();
extern int rename ();
extern int tolower ();
extern void setlinebuf ();
#endif

#ifdef mips
extern int setlinebuf ();
#endif



#define TRACEFILENAME          "tracefile.sui"
#define VALIDSTRING(S)	       ( ((S) != NULL) && ( (*(S)) != '\0') )
#define DEBUG(message)         if (global.debug != 0) fprintf(stderr,message)



#if !defined(PRODUCTION_VERSION) && !defined(_Windows)
     /* We don't define this for MS Windows because we need the static */
     /* memory space that these strings take up. */

#    define TRACE(level,printparms)                                        \
        {                                                                  \
            char buf[500];                                                 \
            if ( trace.tracelevel >= level) {                              \
                int i;                                                     \
                for ( i = 0 ; i < trace.traceIndent ; i++ )                \
                    fprintf(trace.tracefile, "   ");                       \
                sprintf printparms ;                                       \
                fprintf(trace.tracefile, "%s", buf);                       \
            }                                                              \
        }

#    define ENTER(level,printparms)                                        \
        {                                                                  \
	    trace.traceIndent += 1;                                        \
	    if ( trace.tracelevel >= level)                                \
		fprintf(trace.tracefile, "%.2d -->",trace.traceIndent);    \
	    TRACE(level, printparms);                                      \
	}

#    define LEAVE(level,printparms)                                        \
        {                                                                  \
	    if ( trace.tracelevel >= level)                                \
		fprintf(trace.tracefile, "%.2d <--",trace.traceIndent);    \
	    TRACE(level, printparms);                                      \
	    trace.traceIndent -= 1;                                        \
	}

#else

#    define TRACE(level,printparms)
#    define ENTER(level,printparms)
#    define LEAVE(level,printparms)

#endif /* PRODUCTION_VERSION */


#define OBJECT_X1(O)           	((SUIT_getViewport(O, VIEWPORT)).bottom_left.x)
#define OBJECT_Y1(O)           	((SUIT_getViewport(O, VIEWPORT)).bottom_left.y)
#define OBJECT_X2(O)            ((SUIT_getViewport(O, VIEWPORT)).top_right.x)
#define OBJECT_Y2(O)            ((SUIT_getViewport(O, VIEWPORT)).top_right.y)
#define OBJECT_WIDTH(O)         (OBJECT_X2(O) - OBJECT_X1(O))
#define OBJECT_HEIGHT(O)        (OBJECT_Y2(O) - OBJECT_Y1(O))

/* defaults should fit on any unix bitmap screen, and are in golden ratio! */

#ifdef X_WINDOWS
#define DEFAULT_SCREEN_HEIGHT          600
#define DEFAULT_SCREEN_WIDTH           960
#endif

#ifdef _Windows
#define DEFAULT_SCREEN_HEIGHT          450
#define DEFAULT_SCREEN_WIDTH           630
#elif defined(IBM_PC)
#define DEFAULT_SCREEN_HEIGHT          480
#define DEFAULT_SCREEN_WIDTH           640
#endif

#ifdef MACINTOSH
#define DEFAULT_SCREEN_HEIGHT          450
#define DEFAULT_SCREEN_WIDTH           640
#endif

#define DEFAULT_SCREEN_DEPTH		7

typedef struct {
    char *name;        /* the class' name */
    DynArray objects;  /* the objects in that class */
} ClassObjectList;


typedef struct {
    char *name;			/* the name of the implimentation */
    SUIT_hitProcedure hit;	/* the output procedure */
    SUIT_paintProcedure paint;	/* the input procedure */
    DynArray employees;		/* of SUIT_object */
} SUIT_display;


typedef struct {
    char *name;
    SUIT_callbackFunctionPtr ptr;
} callbackPair;


typedef struct {
    char *name;
    SUIT_object ptr;
} nameTableEntry;

typedef struct {
    char *name;			/* the name of the property */
    char *type;			/* the property's type (human-readable string) */
    Pointer value;		/* a pointer to the prop's current value */
    boolean permanent;		/* non-permanents don't get saved between runs */
    boolean locked;		/* locked properties can't be changed */
} property;



typedef struct {
    SUIT_object root;
    char *hintsfile;
    char *programName;
    int debug;
    boolean srgp_trace;
    boolean srgp_debug;
    boolean malloc_debug;
    int sampleMode;
    DynArray types;		     /* of SUIT_type */
    DynArray classProperties;	     /* of SUIT_class */
    DynArray nameTable;              /* of nameTableEntry */
    DynArray callbackPtrs;           /* of callBackPair */
    PropedFunction propertyEditor;       /* takes SUIT_object parm */
    boolean NoCanvas;
    DynArray trapperFunctions;	     /* of SUIT_trapperPtr */
    boolean interestOn;		     /* way to turn all interest off at once */
    boolean propertyEditorIsActive;  /* whether the proped has control :( */
    boolean animated;
    boolean currentlyPainting;
    boolean currentlyInHitProcedure;
    boolean WidgetMenuOn;	/* don't add anyone to the widget menu before
				 * it's created */
    boolean CustomMenuOn;
    SUIT_object objectGetsMouse;
    boolean interactiveToolsAvailable;
} globalVariables;


typedef struct {
    int tracelevel;
    int traceIndent;
    FILE *tracefile;
} traceVariables;

#ifdef SUITMAIN
globalVariables global;
traceVariables trace;
DynArray OpenObjects;		/* of SUIT_object */
DynArray SelectedObjects;	/* of SUIT_object */
#else
extern globalVariables global;
extern traceVariables trace;
extern DynArray OpenObjects;	/* of SUIT_object */
extern DynArray SelectedObjects;/* of SUIT_object */
#endif

/* prototypes of all internal routines */

typedef void (*SUIT_animationFunction) (SUIT_object o, long currentTime, long totalTime);

void si_animateOverTime(SUIT_animationFunction func, SUIT_object o, long totalTime);

void si_animateScreenBorderToObject(SUIT_object o, long currentTime, long totalTime);

void si_registerInitialTypes (void);
char *si_makeFileName (char *str1, char *original);
void si_writeHints (char *filename);
void si_readAndProcessHints (char *filename);
SUIT_type *si_getType (char *typeName);
SUIT_display *si_getDisplay (SUIT_object o, char *displayName);

property *DummyProperty (char *name);
SUIT_class **DummyNamedPropertyList (char *name);
SUIT_type *DummyType (char *name);
SUIT_display *DummyDisplay (char *name);
nameTableEntry *DummyNameTableEntry (char *name);


int CompareProperty (void *first, void *second);
int CompareNamedPropertyList (void *first, void *second);
int CompareTypes (void *first, void *second);
int CompareDisplays (void *first, void *second);
int CompareNameTableEntry (void *first, void *second);
int CompareDispCallbacks (void *first, void *second);
int CompareCallbackFunctionPtr (void *first, void *second);

void si_registerinitialtypes (void);
SUIT_class *si_searchForClass (char *name);
property *si_searchForPropAtLevels (SUIT_object obj, char *propertyName, SUIT_level *level);
SUIT_class *FindClassAndAddItIfNecessary (char *name);
property *FindProperty (DynArray list, char *name);
boolean AddProperty (DynArray list, property * newGuy);
void si_inSample (void);
void si_outSample (void);
boolean si_buttonsEqual (deluxe_locator_measure first, deluxe_locator_measure second);
boolean si_atLeastOneButtonDown (deluxe_locator_measure measure);
boolean si_modifiersEqual (deluxe_locator_measure first, deluxe_locator_measure second);
boolean si_buttonsAndsi_modifiersEqual (deluxe_locator_measure first, deluxe_locator_measure second);
boolean si_eventIsButtonDown (SUIT_event event);

deluxe_locator_measure si_waitForAnyChange (deluxe_locator_measure original, boolean considerAnimation);
deluxe_locator_measure si_getLocatorMeasure (void);

void si_mouseDebounce (void);
void si_moveGroup (DynArray selectedObjects, deluxe_locator_measure original);
void si_moveObject (SUIT_object o, deluxe_locator_measure original);
boolean si_overTrashCan(int x, int y);
boolean si_overExportButton(int x, int y);
boolean si_overInfoButton(int x, int y);
void si_deselectAll (void);
void si_repaintGroup (DynArray selectedObjects);
void si_resizeObject (SUIT_object o, int direction);
void si_repaintScreen (void);
void si_resizeGroup (DynArray selectedObjects, int direction);
boolean si_nearEdge (SUIT_object, point);

int si_highlightSelected (DynArray, int, int, int, int);
void si_drawInHighlightStyle (void);
void si_eraseSelected (DynArray selectedObjects);
void si_highlightRectangle (rectangle, boolean);
rectangle si_getBoundingRectangle (DynArray);
void si_lessAndGreater (int, int, int *, int *);
void si_initializeProperties (void);
void si_createRoot (void);
void si_selectObjectAndDeselectParent (SUIT_object);
void si_selectObject (SUIT_object);
void si_closeObject (SUIT_object);
void si_systemMenu (int x, int y);
int si_overHandle (SUIT_viewport, point);	/* returns the direction you
						 * can move, one of the
						 * following */

#define NO_DIRECTION        0
#define TOP_DIRECTION       1
#define BOTTOM_DIRECTION    2
#define LEFT_DIRECTION      4
#define RIGHT_DIRECTION     8

void si_drawRectangleAsOpen (rectangle r);
void si_openObject (SUIT_object);

void si_showSUITMenu (point);
void InitTools (void);
void si_AdjustWidgetMenu (char *ClassName);
void si_AdjustCustomMenu (char *ClassName);
DynArray si_exportSelectedList (void);

boolean si_SUITInitialized(void);

DynArray si_createObjectList(void);

void AdjustWidgetMenu (char *ClassName);
void si_checkCallbacks (SUIT_object o, SUIT_level startLevel, property * newProp, Pointer oldvalue);
property *si_searchForProperty (SUIT_object obj, SUIT_level level, char *propertyName);
boolean si_viewportWithinViewport (SUIT_viewport vp1, SUIT_viewport vp2);
void si_cycleGroup (void);

void PositionOnScreen (SUIT_object o, double x1, double y1, double x2, double y2);

/* just a test to see if the profile will pick this file name up.  - Nat */
char *ReadName (FILE * infile);

SUIT_viewport si_resizeRectangleInDirection (SUIT_viewport original, int direction);


/* This routine returns the list of properties associated with the specified
 * object. */
DynArray si_getPropertyList (SUIT_object o, SUIT_level level);

void si_addChildHere (SUIT_object openobj, SUIT_object o, int which);

void SUIT_setViewportAfterSet (SUIT_object obj);
void SUIT_setVisibilityAfterSet (SUIT_object o);
char *SUIT_createRCString (char *s);
void SUIT_dumpRC (void);

void PeekAtHintsFile (int *width, int *height, int *depth);


extern  SUIT_object si_mapPointToObject (DynArray objectList, int x, int y);

/* the following routines are implemented in scrolbox.c */
extern	int sb_textHeight (SUIT_object o);
extern	int sb_mapListEvent (SUIT_object o, point p);
extern  SUIT_viewport sb_listSubViewport (SUIT_object o, int loc, SUIT_viewport drawingVP, double siblingScrollerCurrentValue, int textheight, int margin);
void listScrollerCallback (SUIT_object o, char *name, char *type, void *new, void *old);
extern  void sb_makeListConsistent (SUIT_object o);
extern  void sb_listInterestCallback (SUIT_object o, char *name, char *type, void *new, void *old);
extern	void sb_genericPaintScrollableBox (SUIT_object o, void (*DrawSingleItem)(SUIT_object o, int n, SUIT_viewport vp) );
    

extern void si_commandAlign (SUIT_object unused);
extern void si_commandDestroyObject(SUIT_object unused);
extern void si_commandCreateObject (SUIT_object unused);

SUIT_object ObjectBeingEdited(void);
SUIT_object EditorActiveWidget(void);
void StopEditingDestroyedObject(void);


#endif

void PrintClass (SUIT_object);
void DeleteObjectFromList (DynArray, SUIT_object);
void si_giveInfo (SUIT_object);
void si_editObject (SUIT_object);
void si_addChildToObject (SUIT_object, SUIT_object, boolean);
void si_setUpHelp (void);
void si_tickleAllViewports (SUIT_object root);
boolean si_buttonsAndModifiersEqual (deluxe_locator_measure first, deluxe_locator_measure second);
rectangle ViewportPlusBorder (SUIT_object o, int width);
void si_actuallyDestroyObjects (void);
void si_clearArea (rectangle r);
void si_checkChildrensCallbacks (SUIT_object o, property *newProp, Pointer oldValue);
Pointer si_readTextList (char *buffer, boolean *error);
Pointer si_readFunctionPtr (char *buffer, boolean *error);
char *si_writeTextList (Pointer val);
char *si_writeFunctionPtr (Pointer val);

SUIT_callbackFunctionPtr SUIT_getCallbackByName (char *functionName);
char *SUIT_getCallbackByPtr (SUIT_callbackFunctionPtr functionPtr);
void SUIT_registerCallbackPtr (char *functionName, SUIT_callbackFunctionPtr func);

void si_dumpInitializationCode(FILE * file);

int CaseInsensitiveMatch (char *a, char *b);

void WriteHintsToFile (FILE * file);

rectangle CalculateClipRectangle (SUIT_object obj);
rectangle CalculateVisiblePortion (SUIT_object obj);

#define si_isVisible(OBJ)       ((!global.propertyEditorIsActive && SUIT_getBoolean (OBJ, VISIBLE)) ||                    \
				 (global.propertyEditorIsActive && SUIT_getBoolean (OBJ, VISIBLE_WITHIN_PROPERTY_EDITOR)))

void si_initRCString (void);
void si_duplicateObject (SUIT_object obj);
char *si_generateCopyName (char *name);
void si_setUpHelp (void);
void ChainedPropertyCallback (SUIT_object o);
void si_quitLikeDoneButton (SUIT_object unused);
