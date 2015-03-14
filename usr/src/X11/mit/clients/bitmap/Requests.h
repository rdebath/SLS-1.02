/*
 * $XConsortium: Requests.h,v 1.5 90/12/08 17:29:57 dmatic Exp $
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

typedef struct {
    Boolean  success;
    Position at_x, at_y;
    Position from_x, from_y,
             to_x, to_y;
    void   (*draw)();
    int      value;
    Time     btime;
    int      state;
} BWStatus;

void OnePointEngage();
void OnePointTerminate();
void OnePointTerminateTransparent();
void DragOnePointEngage();
void DragOnePointTerminate();
void TwoPointsEngage();
void TwoPointsTerminate();
void TwoPointsTerminateTransparent();
void TwoPointsTerminateTimed();
void DragTwoPointsEngage();
void DragTwoPointsTerminate();
void Interface();
void Paste();

void BWMark();
void BWUnmark();
void BWStore();
void BWDragMarked();
void BWDragStored();
void BWRestore();
void BWCopy();
void BWMove();
void BWDrawPoint();
void BWDrawLine();
void BWBlindLine();
void BWDrawRectangle();
void BWDrawFilledRectangle();
void BWDrawCircle();
void BWDrawFilledCircle();
void BWFloodFill();
void BWDrawHotSpot();
void BWChangeNotify();
void BWZoomIn();

static BWRequestRec requests[] = /* SUPPRESS 592 */
{
{MarkRequest, sizeof(BWStatus),
     TwoPointsEngage, (caddr_t) BWDrawRectangle,
     TwoPointsTerminateTimed, (caddr_t) BWSelect,
     NULL, (caddr_t) NULL},
{RestoreRequest, sizeof(BWStatus),
     OnePointEngage, (caddr_t) BWDragStored,
     OnePointTerminate, (caddr_t) BWRestore,
     NULL, (caddr_t) NULL},
{ImmediateCopyRequest, sizeof(BWStatus),
     OnePointEngage, (caddr_t) BWDragMarked,
     OnePointTerminate, (caddr_t) BWCopy,
     NULL, (caddr_t) NULL},
{ImmediateMoveRequest, sizeof(BWStatus),
     OnePointEngage, (caddr_t) BWDragMarked,
     OnePointTerminate, (caddr_t) BWMove,
     NULL, (caddr_t) NULL},
{CopyRequest, sizeof(BWStatus),
     DragOnePointEngage, (caddr_t) Paste,
     DragOnePointTerminate, (caddr_t) ImmediateCopyRequest,
     Interface, (caddr_t) BWUnmark},
{MoveRequest, sizeof(BWStatus),
     DragOnePointEngage, (caddr_t) Paste,
     DragOnePointTerminate, (caddr_t) ImmediateMoveRequest,
     Interface, (caddr_t) BWUnmark},
{PointRequest, sizeof(BWStatus),
     DragOnePointEngage, (caddr_t) BWDrawPoint,
     DragOnePointTerminate, (caddr_t) BWDrawPoint,
     NULL, (caddr_t) NULL},
{CurveRequest, sizeof(BWStatus),
     DragTwoPointsEngage, (caddr_t) BWBlindLine,
     DragTwoPointsTerminate, (caddr_t) BWBlindLine,
     NULL, (caddr_t) NULL},
{LineRequest, sizeof(BWStatus), 
     TwoPointsEngage, (caddr_t) BWDrawLine, 
     TwoPointsTerminate, (caddr_t) BWDrawLine,
     NULL, (caddr_t) NULL},
{RectangleRequest, sizeof(BWStatus), 
     TwoPointsEngage, (caddr_t) BWDrawRectangle,
     TwoPointsTerminate, (caddr_t) BWDrawRectangle,
     NULL, (caddr_t) NULL},
{FilledRectangleRequest, sizeof(BWStatus), 
     TwoPointsEngage, (caddr_t) BWDrawRectangle,
     TwoPointsTerminate, (caddr_t) BWDrawFilledRectangle,
     NULL, (caddr_t) NULL},
{CircleRequest, sizeof(BWStatus), 
     TwoPointsEngage, (caddr_t) BWDrawCircle,
     TwoPointsTerminate, (caddr_t) BWDrawCircle,
     NULL, (caddr_t) NULL},
{FilledCircleRequest, sizeof(BWStatus), 
     TwoPointsEngage, (caddr_t) BWDrawCircle, 
     TwoPointsTerminate, (caddr_t) BWDrawFilledCircle,
     NULL, (caddr_t) NULL},
{FloodFillRequest, sizeof(BWStatus),
     OnePointEngage, (caddr_t) NULL,
     OnePointTerminate, (caddr_t) BWFloodFill,
     NULL, (caddr_t) NULL},
{HotSpotRequest, sizeof(BWStatus),
     OnePointEngage, (caddr_t) BWDrawHotSpot,
     OnePointTerminate, (caddr_t) BWDrawHotSpot,
     NULL, (caddr_t) NULL},
{ZoomInRequest, sizeof(BWStatus),
     TwoPointsEngage, (caddr_t) BWDrawRectangle,
     TwoPointsTerminate, (caddr_t) BWZoomIn,
     NULL, (caddr_t) NULL},
};

