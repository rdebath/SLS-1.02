/*
 * $XConsortium: Bitmap.h,v 1.11 91/05/04 19:36:52 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Davor Matic, MIT X Consortium
 */


#ifndef _Bitmap_h
#define _Bitmap_h

/****************************************************************
 *
 * Bitmap widget
 *
 ****************************************************************/

#include <X11/Xaw/Simple.h>

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		------------- 
 background	     Background		Pixel		XtDefaultBackground
 foreground          Foredround         Pixel           XtDefaultForeground
 highlight           Highlight          Pixel           XtDefaultForeground
 frame               Frame              Pixel           XtDefaultForeground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 resize              Resize             Boolean         True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 height		     Height		Dimension	0
 size                Size               String          32x32
 squareWidht         SquareWidht        Dimension       16
 squareHeight        SquareHeight       Dimension       16
 x		     Position		Position	320
 y		     Position		Position	320
 xHot                XHot               Position        NotSet
 yHot                YHot               Position        NotSet
 margin              Margin             Dimension       16
 grid                Grid               Boolean         True
 gridTolerance       GridTolerance      Dimension       8
 dashed              Dashed             Boolean         True
 dashes              Dashes             Bitmap          XtUnspecifiedPixmap
 stippled            Stippled           Boolean         True
 stipple             Sripple            Bitmap          XtUnspecifiedPixmap
 proportional        Proportional       Boolean         True
 axes                Axes               Boolean         True
 button1Function     Button1Function    ButtonFunction  Set  
 button2Function     Button2Function    ButtonFunction  Invert
 button3Function     Button3Function    ButtonFunction  Clear
 button4Function     Button4Function    ButtonFunction  Invert
 button5Function     Button5Function    ButtonFunction  Invert
 filename            Filename           String          None
 basename            Basename           String          None
*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNbitmapResource "bitmapResource"
#define XtNstipple "stipple"
#define XtNstippled "stippled"
#define XtNdashes "dashes"
#define XtNdashed "dashed"
#define XtNgrid "grid"
#define XtNgridTolerance "gridTolerance"
#define XtNaxes "axes"
#define XtNbitmapSize "bitmapSize"
#define XtNsize "size"
#define XtNsquareWidth "squareWidth"
#define XtNsquareHeight "squareHeight"
#define XtNxHot "xHot"
#define XtNyHot "yHot"
#define XtNbutton1Function "button1Function"
#define XtNbutton2Function "button2Function"
#define XtNbutton3Function "button3Function"
#define XtNbutton4Function "button4Function"
#define XtNbutton5Function "button5Function"
#define XtNfilename "filename"
#define XtNbasename "basename"
#define XtNmouseForeground "mouseForeground"
#define XtNmouseBackground "mouseBackground"
#define XtNframe "frame"
#define XtNmargin "margin"
#define XtNproportional "proportional"

#define XtCBitmapResource "BitmapResource"
#define XtCHighlight "Highlight"
#define XtCStipple "Stipple"
#define XtCStippled "Stippled"
#define XtCDashes "Dashes"
#define XtCDashed "Dashed"
#define XtCGrid "Grid"
#define XtCGridTolerance "GridTolerance"
#define XtCAxes "Axes"
#define XtBitmapSize "BitmapSize"
#define XtCSize "Size"
#define XtCSquareWidth "SquareWidth"
#define XtCSquareHeight "SquareHeight"
#define XtCXHot "XHot"
#define XtCYHot "YHot"
#define XtCButton1Function "Button1Function"
#define XtCButton2Function "Button2Function"
#define XtCButton3Function "Button3Function"
#define XtCButton4Function "Button4Function"
#define XtCButton5Function "Button5Function"
#define XtCFilename "Filename"
#define XtCBasename "Basename"
#define XtCFrame "Frame"
#ifndef XtCMargin
#define XtCMargin "Margin"
#endif
#define XtCProportional "Proportional"

#define XtRButtonFunction "ButtonFunction"

/* bitmap defines */

#define NotSet   -1
#define Clear     0
#define Set       1
#define Invert    2
#define Highlight 3
#define On        True
#define Off       False

#define XtClear "clear"
#define XtSet "set"
#define XtInvert "invert"

#define MarkRequest "MarkRequest"
#define StoreRequest "StoreRequest"
#define RestoreRequest "RestoreRequest"
#define CopyRequest "CopyRequest"
#define MoveRequest "MoveRequest"
#define PointRequest "PointRequest"
#define LineRequest "LineRequest"
#define CurveRequest "CurveRequest"
#define RectangleRequest "RectangleRequest"
#define FilledRectangleRequest "FilledRectangleRequest"
#define CircleRequest "CircleRequest"
#define FilledCircleRequest "FilledCircleRequest"
#define FloodFillRequest "FloodFillRequest"
#define HotSpotRequest "HotSpotRequest"
#define ZoomInRequest "ZoomInRequest"
#define PasteRequest "PasteRequest"
#define ImmediateCopyRequest "ImmediateCopyRequest"
#define ImmediateMoveRequest "ImmediateMoveRequest"

/* bitmap exports */

extern Boolean BWEngageRequest();
extern Boolean BWTreminateRequest();

extern void BWClearAll();
extern void BWSetAll();
extern void BWInvertAll();
extern void BWUp();
extern void BWDown();
extern void BWLeft();
extern void BWRight();
extern void BWRotateRight();
extern void BWRotateLeft();
extern void BWSwitchGrid();
extern void BWGrid();
extern void BWSwitchDashed();
extern void BWDashed();
extern void BWSwitchAxes();
extern void BWAxes();
extern void BWDrawSquare();
extern void BWDrawLine();
extern void BWDrawRectangle();
extern void BWDrawFilledRectangle();
extern void BWDrawCircle();
extern void BWDrawFilledCircle();
extern void BWFloodFill();
extern void BWMark();
extern void BWMarkAll();
extern void BWUnmark();
extern void BWSelect();
extern void BWUnmark();
extern void BWStore();
extern void BWStoreToBuffer();
extern void BWUndo();
extern void BWResize();
extern void BWClip();
extern void BWUnclip();
extern void BWGrabSelection();
extern void BWRequestSelection();
extern void BWSetChanged();
extern Boolean BWQueryChanged();
extern int BWReadFile();
extern int BWWriteFile();
extern String BWUnparseStatus();
extern String BWGetFilename();
extern String BWGetBasename();
extern void BWChangeBasename();
extern void BWRemoveAllRequests();
extern void BWClearHotSpot();
extern Boolean BWQueryMarked();
extern void BWFold();
extern void BWClear();
extern void BWSet();
extern void BWInvert();
extern void BWFlipHoriz();
extern void BWFlipVert();
extern void BWClearMarked();
extern Boolean BWAddRequest();
extern void BWChangeNotify();
extern Pixmap BWGetUnzoomedPixmap();
extern void BWClearChanged();
extern Boolean BWQueryStored();
extern Boolean BWQueryStippled();
extern void BWSwitchStippled();
extern void BWRedrawMark();
extern Boolean BWQueryAxes();
extern void BWHighlightAxes();
extern void BWChangedFilename();
extern String BWGetFilepath();
extern void BWZoomOut();
extern void BWZoomMarked();
extern void BWRescale();
extern Boolean BWQueryZooming();
extern void BWRedrawGrid();
extern void BWRedrawSquares();
extern void BWRedrawHotSpot();
extern Boolean BWQueryGrid();
extern Boolean BWQueryDashed();
extern Boolean BWQueryProportional();
extern void BWSwitchProportional();
extern void BWDrawGrid();
extern void BWChangeFilename();
extern Boolean BWParseSize();

typedef struct _BWRequestRec BWRequestRec;
typedef char *BWRequest;

/* declare specific BitmapWidget class and instance datatypes */

typedef struct _BitmapClassRec *BitmapWidgetClass;
typedef struct _BitmapRec      *BitmapWidget;
/* declare the class constant */

extern WidgetClass bitmapWidgetClass;

#endif /* _Bitmap_h */


