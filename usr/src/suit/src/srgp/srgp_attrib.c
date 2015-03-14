#include "HEADERS.h"
#include "srgplocal.h"

#define __CURATT(xx)	srgp__curActiveCanvasSpec.attributes.xx


#ifdef THINK_C
#include "ChooseWhichQuickDraw.h"
#endif

#ifdef X11
static XGCValues gcvals;

static unsigned char writemode_table[4] = {
   /*REPLACE*/	0x3, /* GXcopy from /usr/include/X11/X.h */
   /*XOR*/ 	0x6, /* GXxor */
   /*OR*/ 	0x7, /* GXor */
   /*AND*/ 	0x1  /* GXand */
};
  
static unsigned char inv_writemode_table[4] = {
   /*REPLACE*/ 0x3, /* GXcopy */
   /*XOR*/     0x9, /* GXequiv */
   /*OR*/      0x1, /* GXand */
   /*AND*/     0x7  /* GXor */
};

#endif

#ifdef THINK_C
#ifdef COLOR_QUICKDRAW
static unsigned char transfermode_table[4][4] = {
	  /*REPLACE,SOLID*/ patCopy,
	  /* pixmap pattern entries in this table are unused */ 0,
	  /*REPLACE,BITMAP_PATTERN_TRANSPARENT*/ patOr,
	  /*REPLACE,BITMAP_PATTERN_OPAQUE*/ patCopy,
	  /*XOR,SOLID*/ patXor,
	  /* pixmap pattern entries in this table are unused */ 0,
	  /*XOR,BITMAP_PATTERN_TRANSPARENT*/ patXor,
	  /*XOR,BITMAP_PATTERN_OPAQUE*/ patXor,
	  /*OR,SOLID*/ patOr,
	  /* pixmap pattern entries in this table are unused */ 0,
	  /*OR,BITMAP_PATTERN_TRANSPARENT*/ patOr,
	  /*OR,BITMAP_PATTERN_OPAQUE*/ patOr,
	  /*AND,SOLID*/ notPatBic,
	  /* pixmap pattern entries in this table are unused */ 0,
	  /*AND,BITMAP_PATTERN_TRANSPARENT*/ notPatBic,
	  /*AND,BITMAP_PATTERN_OPAQUE*/ notPatBic
};


#define ComputeFrameTransfermode() \
   srgp__curActiveCanvasSpec.transfermode_frame = \
      transfermode_table \
         [srgp__curActiveCanvasSpec.attributes.write_mode] \
         [srgp__curActiveCanvasSpec.attributes.pen_style]

#define ComputeFillTransfermode() \
   srgp__curActiveCanvasSpec.transfermode_fill = \
      transfermode_table \
         [srgp__curActiveCanvasSpec.attributes.write_mode] \
         [srgp__curActiveCanvasSpec.attributes.fill_style]

#define SetTextTransfermode() \
   TextMode (transfermode_table \
   		[srgp__curActiveCanvasSpec.attributes.write_mode] \
   		[BITMAP_PATTERN_TRANSPARENT] \
   	     - 8)
   		
#else

#define DONT_DRAW   -1

static unsigned char transfermode_table[4][2][4] = {
  {
	/*REPLACE,WHITE,...*/ {notPatCopy, 0, patBic, notPatCopy},
	/*REPLACE,BLACK,...*/ {patCopy, 0, patOr, patCopy},
  },
  {
	/*XOR,WHITE,...*/ {notPatXor, 0, DONT_DRAW, notPatXor},
	/*XOR,BLACK,...*/ {patXor, 0, patXor, patXor} 
  },
  {
	/*OR,WHITE,...*/ {notPatOr, 0, DONT_DRAW, notPatOr},
	/*OR,BLACK,...*/ {patOr, 0, patOr, patOr} 
  },
  {
	/*AND,WHITE,...*/ {patBic, 0, DONT_DRAW, patBic},
	/*AND,BLACK,...*/ {notPatBic, 0, notPatBic, notPatBic} 
  }
};


static void ComputeFrameTransfermode (void)
{
   srgp__curActiveCanvasSpec.transfermode_frame = 
      transfermode_table[srgp__curActiveCanvasSpec.attributes.write_mode]
		   [srgp__curActiveCanvasSpec.attributes.color]
		   [srgp__curActiveCanvasSpec.attributes.pen_style];
}

static void ComputeFillTransfermode (void)
{
   srgp__curActiveCanvasSpec.transfermode_fill = 
      transfermode_table[srgp__curActiveCanvasSpec.attributes.write_mode]
		   [srgp__curActiveCanvasSpec.attributes.color]
		   [srgp__curActiveCanvasSpec.attributes.pen_style];
}

static void SetTextTransfermode (void) {
   TextMode (transfermode_table
   		[srgp__curActiveCanvasSpec.attributes.write_mode] 
		[srgp__curActiveCanvasSpec.attributes.color]
   		[BITMAP_PATTERN_TRANSPARENT]
   	     - 8);
}

#endif

#endif




/** WRITE_MODE
X11 implementation:
   Both GC's must be modified.
**/

void
SRGP_setWriteMode (writeMode value)
{
   if (value == __CURATT(write_mode)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setWriteMode  %d\n", value);
      srgp_check_system_state();
      srgp_check_write_mode(value);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.write_mode = value;

#ifdef X11
   if (XWHITE == 0)
       value = writemode_table[value];
   else
       value = inv_writemode_table[value];
   XSetFunction
      (srgpx__display,
       srgp__curActiveCanvasSpec.gc_frame,
       value);
   XSetFunction
      (srgpx__display,
       srgp__curActiveCanvasSpec.gc_fill,
       value);
#endif
#ifdef THINK_C
   ComputeFrameTransfermode();
   ComputeFillTransfermode();
   SetTextTransfermode();
#endif
}



/** CLIP_RECTANGLE
Both GC's must be modified.
**/
void
SRGP_setClipRectangle (rectangle rect)
{
#ifdef X11
   XRectangle xrect;
#endif
#ifdef THINK_C
   Rect r;
#endif

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setClipRect  (%d,%d)->(%d,%d)\n", ExplodeRect(rect));
      srgp_check_system_state();
      srgp_check_rectangle
	 (rect.bottom_left.x, rect.bottom_left.y,
	  rect.top_right.x, rect.top_right.y);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.clip_rectangle = rect;
   
#ifdef X11
   xrect.x = rect.bottom_left.x;
   xrect.y = FIXED(rect.top_right.y);
   xrect.width = (rect.top_right.x - rect.bottom_left.x + 1);
   xrect.height = (rect.top_right.y - rect.bottom_left.y + 1);
   XSetClipRectangles
      (srgpx__display,
       srgp__curActiveCanvasSpec.gc_frame,
       0, 0,
       &xrect, 1, YXSorted);
   XSetClipRectangles
      (srgpx__display,
       srgp__curActiveCanvasSpec.gc_fill,
       0, 0,
       &xrect, 1, YXSorted);
#endif

#ifdef THINK_C
   r = FIXED_RECT(ExplodeRect(rect));
   ClipRect(&r);
#endif
}




/** FILL STYLE
Only the fill GC is to be modified.
The SRGP fill-style maps directly to X's fill-style.
**/
void
SRGP_setFillStyle (drawStyle value)
{
   if (value == __CURATT(fill_style)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setFillStyle  %d\n", value);
      srgp_check_system_state();
      srgp_check_fill_style(value);
      LeaveIfNonFatalErr();
   }
   
   srgp__curActiveCanvasSpec.attributes.fill_style = value;
   
#ifdef X11
   XSetFillStyle (srgpx__display, srgp__curActiveCanvasSpec.gc_fill, value);
#endif

#ifdef THINK_C
   ComputeFillTransfermode();
   srgp__curActiveCanvasSpec.pat_fill = (Pattern*)
      ((value == SOLID) ?
          black :  /*Mac's "solid" pattern*/
          srgp__bitmapPatternTable
             [srgp__curActiveCanvasSpec.attributes.fill_bitmap_pattern_id]);
#endif      
}

/*!*/
void
SRGP_setFillBitmapPattern (int value)
{
   if (value == __CURATT(fill_bitmap_pattern_id)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setFillBitmapPattern  %d\n", value);
      srgp_check_system_state();
      srgp_check_pattern_index(value);
      srgp_check_pattern_table_entry(srgp__bitmapPatternTable[value]);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.fill_bitmap_pattern_id = value;

#ifdef X11
   XSetStipple
      (srgpx__display,
       srgp__curActiveCanvasSpec.gc_fill,
       srgp__bitmapPatternTable[value]);
#endif

#ifdef THINK_C
   if (srgp__curActiveCanvasSpec.attributes.fill_style != SOLID)
      srgp__curActiveCanvasSpec.pat_fill = (Pattern*)srgp__bitmapPatternTable[value];
#endif
}

/*!*/
void
SRGP_setFillPixmapPattern (int value)
{
   if (value == __CURATT(fill_pixmap_pattern_id)) return;
      
   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setFillPixmapPattern  %d\n", value);
      srgp_check_system_state();
      srgp_check_pattern_index(value);
      srgp_check_pattern_table_entry(srgp__pixmapPatternTable[value]);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.fill_pixmap_pattern_id = value;

#ifdef X11
   gcvals.tile = srgp__pixmapPatternTable[value];
   XChangeGC (srgpx__display, srgp__curActiveCanvasSpec.gc_fill,
	      GCTile, &gcvals);
#endif
} 








/** PEN STYLE
X11 implementation:
   Only the frame GC is to be modified.
   The SRGP pen-style maps directly to X's fill-style.
**/

void
SRGP_setPenStyle (drawStyle value)
{
   if (value == __CURATT(pen_style)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setPenStyle  %d\n", value);
      srgp_check_system_state();
      srgp_check_fill_style(value);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.pen_style = value;

#ifdef X11
   XSetFillStyle (srgpx__display, srgp__curActiveCanvasSpec.gc_frame, value);
#endif
#ifdef THINK_C
   ComputeFrameTransfermode();
   srgp__curActiveCanvasSpec.pat_frame = (Pattern*)
      ((value == SOLID) ?
         black :  /*Mac's "solid" pattern*/
         srgp__bitmapPatternTable
            [srgp__curActiveCanvasSpec.attributes.pen_bitmap_pattern_id]);
#endif      
}

/*!*/
void
SRGP_setPenBitmapPattern (int value)
{
   if (value == __CURATT(pen_bitmap_pattern_id)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setPenBitmapPattern  %d\n", value);
      srgp_check_system_state();
      srgp_check_pattern_index(value);
      srgp_check_pattern_table_entry(srgp__bitmapPatternTable[value]);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.pen_bitmap_pattern_id = value;

#ifdef X11
   XSetStipple
      (srgpx__display,
       srgp__curActiveCanvasSpec.gc_frame,
       srgp__bitmapPatternTable[value]);
#endif

#ifdef THINK_C
   if (srgp__curActiveCanvasSpec.attributes.pen_style != SOLID)
      srgp__curActiveCanvasSpec.pat_frame = 
         (Pattern*) srgp__bitmapPatternTable[value];
#endif
}



/*!*/
void
SRGP_setPenPixmapPattern (int value)
{
   if (value == __CURATT(pen_pixmap_pattern_id)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setPenPixmapPattern  %d\n", value);
      srgp_check_system_state();
      srgp_check_pattern_index(value);
      srgp_check_pattern_table_entry(srgp__pixmapPatternTable[value]);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.pen_pixmap_pattern_id = value;

#ifdef X11
   gcvals.tile = srgp__pixmapPatternTable[value];
   XChangeGC (srgpx__display, srgp__curActiveCanvasSpec.gc_frame,
	      GCTile, &gcvals);
#endif
}






/** MARKER ATTRIBUTES
**/

void 
SRGP_setMarkerSize (int value)
{
   if (value == __CURATT(marker_size)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setMarkerSize  %d\n", value);
      srgp_check_system_state();
      srgp_check_marker_size(value);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.marker_size = value;
}


void 
SRGP_setMarkerStyle (markerStyle value)
{
   if (value == __CURATT(marker_style)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setMarkerStyle  %d\n", value);
      srgp_check_system_state();
      srgp_check_marker_style(value);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.marker_style = value;
}



#ifdef X11
/** TRANSLATING SRGP DASHES TO X 
**/
#define LENGTH_OF_DASH_LIST   4
static char dot[] = {4,4,4,4};
static char dash[] = {10,5,10,5};
static char dotdash[] = {12,4,4,4};
static char *(dashmap[NUMBER_OF_LINE_STYLES]) =
   { /*CONTINUOUS: unimportant*/ 0,
     /*DASHED*/ dash,
     /*DOTTED*/ dot,
     /*DOT_DASHED*/ dotdash };


/** STATIC INTERNAL: SetXLineAttributes()
Notice that when the user desires a line-width of 1, we use 0.
This uses the hardware's fastest line-drawer, according to X.
**/

static void SetXLineAttributes()
{
   XSetLineAttributes
      (
       srgpx__display,
       srgp__curActiveCanvasSpec.gc_frame,
       ((srgp__curActiveCanvasSpec.attributes.line_width == 1) ?
	0 : srgp__curActiveCanvasSpec.attributes.line_width),
       ((srgp__curActiveCanvasSpec.attributes.line_style==CONTINUOUS) ?
	LineSolid : LineOnOffDash),
       CapButt,		/* CAPS and JOINS are no-ops when line width = 0 */
       JoinBevel);
}
#endif



/*!*/
void
SRGP_setLineStyle (lineStyle value)
{
   if (value == __CURATT(line_style)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setLineStyle  %d\n", value);
      srgp_check_system_state();
      srgp_check_line_style(value);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.line_style = value;

#ifdef X11
   if (value != CONTINUOUS)
      XSetDashes (srgpx__display, srgp__curActiveCanvasSpec.gc_frame,
		  0, dashmap[value], LENGTH_OF_DASH_LIST);
   SetXLineAttributes();
#endif
}



/*!*/
void
SRGP_setLineWidth (int value)
{
   if (value == __CURATT(line_width)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setLineWidth  %d\n", value);
      srgp_check_system_state();
      srgp_check_line_width(value);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.line_width = value;
   
#ifdef X11
   SetXLineAttributes ();
#endif
#ifdef THINK_C
   PenSize (value,value);
#endif
} 





/** ATTRIBUTES RELATED TO COLOR
**/

void
SRGP_setPlaneMask (int value)
{
   if (value == __CURATT(plane_mask)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setPlaneMask  %x\n", value);
      srgp_check_system_state();
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.plane_mask = value;

#ifdef X11
   XSetPlaneMask
      (srgpx__display, 
       srgp__curActiveCanvasSpec.gc_fill, (unsigned long) value);
   XSetPlaneMask
      (srgpx__display, 
       srgp__curActiveCanvasSpec.gc_frame, (unsigned long) value);
#endif
}


void
SRGP_setColor (int value)
{
   if (value == __CURATT(color)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setColor  %x\n", value);
      srgp_check_system_state();
      SRGP_correct_color(value);
      LeaveIfNonFatalErr();
   }

   if (value > srgp__max_pixel_value)
      value = srgp__max_pixel_value;
   srgp__curActiveCanvasSpec.attributes.color = value;

#ifdef X11
   XSetForeground(srgpx__display, srgp__curActiveCanvasSpec.gc_fill,
		  XCOLOR(COLORINDEX(value)));
   XSetForeground(srgpx__display, srgp__curActiveCanvasSpec.gc_frame,
		  XCOLOR(COLORINDEX(value)));
#endif

#ifdef THINK_C
#ifdef COLOR_QUICKDRAW
   srgp__curActiveCanvasSpec.drawable.win->fgColor = (COLORINDEX(value));
#else
   ComputeFrameTransfermode();
   ComputeFillTransfermode();
   SetTextTransfermode();
#endif
#endif
}



void
SRGP_setBackgroundColor (int value)
{
   if (value == __CURATT(background_color)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setBackgroundColor  %x\n", value);
      srgp_check_system_state();
      SRGP_correct_color(value);
      LeaveIfNonFatalErr();
   }

   if (value > srgp__max_pixel_value)
      value = srgp__max_pixel_value;
   srgp__curActiveCanvasSpec.attributes.background_color = value;

#ifdef X11
   XSetBackground(srgpx__display, srgp__curActiveCanvasSpec.gc_fill,
		  XCOLOR(COLORINDEX(value)));
   XSetBackground(srgpx__display, srgp__curActiveCanvasSpec.gc_frame,
		  XCOLOR(COLORINDEX(value)));
#endif

#ifdef THINK_C
#ifdef COLOR_QUICKDRAW
   srgp__curActiveCanvasSpec.drawable.win->bkColor = (COLORINDEX(value));
#endif
#endif
}



/** Load Single Color
A convenience subroutine for setting a single color table entry
**/
void SRGP_loadSingleColor
   (int startentry,
    unsigned short redi, 
    unsigned short greeni,
    unsigned short bluei)
{
   PUSH_TRACE;
   SRGP_loadColorTable (startentry, 1, &redi, &greeni, &bluei);
   POP_TRACE;
}




/** FONT
The frame gc is used to draw text.  This is a cheat, and violates SRGP's
spec.  This will be fixed someday.
**/
void
SRGP_setFont (int value)
{
   if (value == __CURATT(font)) return;

   DEBUG_AIDS{
      SRGP_trace (SRGP_logStream, "SRGP_setFont  %d\n", value);
      srgp_check_system_state();
      srgp_check_extant_font(value);
      LeaveIfNonFatalErr();
   }

   srgp__curActiveCanvasSpec.attributes.font = value;

#ifdef X11
   XSetFont
      (srgpx__display,
       srgp__curActiveCanvasSpec.gc_frame,
       srgp__fontTable[value]->fid);
#endif
#ifdef THINK_C
      TextFont(srgp__fontTable[value].txFont);
      TextSize(srgp__fontTable[value].txSize);
      TextFace(srgp__fontTable[value].txFace);
#endif
}

   

/** BATCH SET ATTRIBUTES
Woefully inefficient, but at least I don't reset attributes whose
value has not changed.
**/

void
SRGP_setAttributes (attribute_group *att_group)
{
   SRGP_trace (SRGP_logStream, "SRGP_setAttributes 0x%x\n", att_group);
   PUSH_TRACE;
   SRGP_setWriteMode (att_group->write_mode);
   SRGP_setClipRectangle (att_group->clip_rectangle);

   SRGP_setFont (att_group->font);
   SRGP_setLineStyle (att_group->line_style);
   SRGP_setLineWidth (att_group->line_width);
   SRGP_setColor (att_group->color);
   SRGP_setPlaneMask (att_group->plane_mask);
   SRGP_setBackgroundColor (att_group->background_color);
   SRGP_setFillStyle (att_group->fill_style);
   SRGP_setFillPixmapPattern (att_group->fill_pixmap_pattern_id);
   SRGP_setFillBitmapPattern (att_group->fill_bitmap_pattern_id);
   SRGP_setPenStyle (att_group->pen_style);
   SRGP_setPenPixmapPattern (att_group->pen_pixmap_pattern_id);
   SRGP_setPenBitmapPattern (att_group->pen_bitmap_pattern_id);
   SRGP_setMarkerSize (att_group->marker_size);
   SRGP_setMarkerStyle (att_group->marker_style);
   POP_TRACE;
}
