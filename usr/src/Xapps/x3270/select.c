#include <X11/Xatom.h>
#include <stdio.h>
#include <ctype.h>

#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/StdSel.h>

#define KeyState(x) (((x) & (ShiftMask|ControlMask)) + (((x) & Mod1Mask) ? 2 : 0))
    /* adds together the bits:
        shift key -> 1
        meta key  -> 2
        control key -> 4 */
  
char *SaveText();
extern EditorButton();

extern Widget win;

/* Selection/extension variables */

/* Raw char position where the selection started */
static int rawRow, rawCol;

/* Selected area before CHAR, WORD, LINE selectUnit processing */
static int startRRow, startRCol, endRRow, endRCol = 0;

/* Selected area after CHAR, WORD, LINE selectUnit processing */
static int startSRow, startSCol, endSRow, endSCol = 0;

/* Valid rows for selection clipping */
static int firstValidRow, lastValidRow;

/* Start, end of extension */
static int startERow, startECol, endERow, endECol;

/* Saved values of raw selection for extend to restore to */
static int saveStartRRow, saveStartRCol, saveEndRRow, saveEndRCol;

/* Multi-click handling */
static int numberOfClicks = 0;
static long int lastButtonUpTime = 0;
typedef int SelectUnit;
#define SELECTCHAR 0
#define SELECTWORD 1
#define SELECTLINE 2
#define NSELECTUNITS 3
static SelectUnit selectUnit;

/*ARGSUSED*/
void HandleSelectExtend(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XMotionEvent */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
	register TScreen *screen = &((XtermWidget)w)->screen;
	int row, col;

	screen->selection_time = event->xmotion.time;
	switch (eventMode) {
		case LEFTEXTENSION :
		case RIGHTEXTENSION :
			PointToRowCol (event->xmotion.y, event->xmotion.x, 
				       &row, &col);
			ExtendExtend (row, col);
			break;
		case NORMAL :
			/* will get here if send_mouse_pos != 0 */
		        break;
	}
}


/*ARGSUSED*/
static void do_select_end (w, event, params, num_params, use_cursor_loc)
Widget w;
XEvent *event;			/* must be XButtonEvent */
String *params;			/* selections */
Cardinal *num_params;
Bool use_cursor_loc;
{
	((XtermWidget)w)->screen.selection_time = event->xbutton.time;
	switch (eventMode) {
		case NORMAL :
		        (void) SendMousePosition(w, event);
			break;
		case LEFTEXTENSION :
		case RIGHTEXTENSION :
			EndExtend(event, params, *num_params, use_cursor_loc);
			break;
	}
}


void HandleSelectEnd(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent */
String *params;			/* selections */
Cardinal *num_params;
{
	do_select_end (w, event, params, num_params, False);
}


void HandleKeyboardSelectEnd(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent */
String *params;			/* selections */
Cardinal *num_params;
{
	do_select_end (w, event, params, num_params, True);
}




struct _SelectionList {
    String *params;
    Cardinal count;
    Time time;
};


static void _GetSelection(w, time, params, num_params)
Widget w;
Time time;
String *params;			/* selections in precedence order */
Cardinal num_params;
{
    static void SelectionReceived();
    Atom selection;
    int buffer;

    XmuInternStrings(XtDisplay(w), params, (Cardinal)1, &selection);
    switch (selection) {
      case XA_CUT_BUFFER0: buffer = 0; break;
      case XA_CUT_BUFFER1: buffer = 1; break;
      case XA_CUT_BUFFER2: buffer = 2; break;
      case XA_CUT_BUFFER3: buffer = 3; break;
      case XA_CUT_BUFFER4: buffer = 4; break;
      case XA_CUT_BUFFER5: buffer = 5; break;
      case XA_CUT_BUFFER6: buffer = 6; break;
      case XA_CUT_BUFFER7: buffer = 7; break;
      default:	       buffer = -1;
    }
    if (buffer >= 0) {
	register TScreen *screen = &((XtermWidget)w)->screen;
	int inbytes;
	unsigned long nbytes;
	int fmt8 = 8;
	Atom type = XA_STRING;
	char *line = XFetchBuffer(screen->display, &inbytes, buffer);
	nbytes = (unsigned long) inbytes;
	if (nbytes > 0)
	    SelectionReceived(w, NULL, &selection, &type, (caddr_t)line,
			      &nbytes, &fmt8);
	else if (num_params > 1)
	    _GetSelection(w, time, params+1, num_params-1);
    } else {
	struct _SelectionList* list;
	if (--num_params) {
	    list = XtNew(struct _SelectionList);
	    list->params = params + 1;
	    list->count = num_params;
	    list->time = time;
	} else list = NULL;
	XtGetSelectionValue(w, selection, XA_STRING, SelectionReceived,
			    (caddr_t)list, time);
    }
}


/* ARGSUSED */
static void SelectionReceived(w, client_data, selection, type,
			      value, length, format)
Widget w;
caddr_t client_data;
Atom *selection, *type;
caddr_t value;
unsigned long *length;
int *format;
{
    int pty = ((XtermWidget)w)->screen.respond;	/* file descriptor of pty */
    register char *lag, *cp, *end;
    char *line = (char*)value;
				  
    if (*type == 0 /*XT_CONVERT_FAIL*/ || *length == 0) {
	struct _SelectionList* list = (struct _SelectionList*)client_data;
	if (list != NULL) {
	    _GetSelection(w, list->time, list->params, list->count);
	    XtFree(client_data);
	}
	return;
    }

    end = &line[*length];
    lag = line;
    for (cp = line; cp != end; cp++)
	{
	    if (*cp != '\n') continue;
	    *cp = '\r';
	    v_write(pty, lag, cp - lag + 1);
	    lag = cp + 1;
	}
    if (lag != end)
	v_write(pty, lag, end - lag);

    XtFree(client_data);
    XtFree(value);
}


HandleInsertSelection(w, event, params, num_params)
Widget w;
XEvent *event;			/* assumed to be XButtonEvent* */
String *params;			/* selections in precedence order */
Cardinal *num_params;
{
    if (SendMousePosition(w, event)) return;
    _GetSelection(w, event->xbutton.time, params, *num_params);
}


SetSelectUnit(buttonDownTime, defaultUnit)
unsigned long buttonDownTime;
SelectUnit defaultUnit;
{
/* Do arithmetic as integers, but compare as unsigned solves clock wraparound */
	if ((long unsigned)((long int)buttonDownTime - lastButtonUpTime)
	 > term->screen.multiClickTime) {
		numberOfClicks = 1;
		selectUnit = defaultUnit;
	} else {
		++numberOfClicks;
		selectUnit = ((selectUnit + 1) % NSELECTUNITS);
	}
}

static void do_select_start (w, event, startrow, startcol)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
int startrow, startcol;
{
	if (SendMousePosition(w, event)) return;
	SetSelectUnit(event->xbutton.time, SELECTCHAR);
	replyToEmacs = FALSE;
	StartSelect(startrow, startcol);
}

/* ARGSUSED */
HandleSelectStart(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
	register TScreen *screen = &((XtermWidget)w)->screen;
	int startrow, startcol;

	firstValidRow = 0;
	lastValidRow  = screen->max_row;
	PointToRowCol(event->xbutton.y, event->xbutton.x, &startrow, &startcol);
	do_select_start (w, event, startrow, startcol);
}


/* ARGSUSED */
HandleKeyboardSelectStart(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
	register TScreen *screen = &((XtermWidget)w)->screen;

	do_select_start (w, event, screen->cursor_row, screen->cursor_col);
}


static TrackDown(event)
register XButtonEvent *event;
{
	int startrow, startcol;

	SetSelectUnit(event->time, SELECTCHAR);
	if (numberOfClicks > 1 ) {
		PointToRowCol(event->y, event->x, &startrow, &startcol);
		replyToEmacs = TRUE;
		StartSelect(startrow, startcol);
	} else {
		waitingForTrackInfo = 1;
		EditorButton(event);
	}
}


#define boundsCheck(x)	if (x < 0) \
			    x = 0; \
			else if (x >= screen->max_row) \
			    x = screen->max_row;

TrackMouse(func, startrow, startcol, firstrow, lastrow)
int func, startrow, startcol, firstrow, lastrow;
{
	TScreen *screen = &term->screen;

	if (!waitingForTrackInfo) {	/* Timed out, so ignore */
		return;
	}
	waitingForTrackInfo = 0;
	if (func == 0) return;
	boundsCheck (startrow)
	boundsCheck (firstrow)
	boundsCheck (lastrow)
	firstValidRow = firstrow;
	lastValidRow  = lastrow;
	replyToEmacs = TRUE;
	StartSelect(startrow, startcol);
}

StartSelect(startrow, startcol)
int startrow, startcol;
{
	TScreen *screen = &term->screen;

	if (screen->cursor_state)
	    HideCursor ();
	if (numberOfClicks == 1) {
		/* set start of selection */
		rawRow = startrow;
		rawCol = startcol;
		
	} /* else use old values in rawRow, Col */

	saveStartRRow = startERow = rawRow;
	saveStartRCol = startECol = rawCol;
	saveEndRRow   = endERow   = rawRow;
	saveEndRCol   = endECol   = rawCol;
	if (Coordinate(startrow, startcol) < Coordinate(rawRow, rawCol)) {
		eventMode = LEFTEXTENSION;
		startERow = startrow;
		startECol = startcol;
	} else {
		eventMode = RIGHTEXTENSION;
		endERow = startrow;
		endECol = startcol;
	}
	ComputeSelect(startERow, startECol, endERow, endECol, False);

}

EndExtend(event, params, num_params, use_cursor_loc)
XEvent *event;			/* must be XButtonEvent */
String *params;			/* selections */
Cardinal num_params;
Bool use_cursor_loc;
{
	int	row, col;
	TScreen *screen = &term->screen;
	char line[9];

	if (use_cursor_loc) {
	    row = screen->cursor_row;
	    col = screen->cursor_col;
	} else {
	    PointToRowCol(event->xbutton.y, event->xbutton.x, &row, &col);
	}
	ExtendExtend (row, col);

	lastButtonUpTime = event->xbutton.time;
	/* Only do select stuff if non-null select */
	if (startSRow != endSRow || startSCol != endSCol) {
		if (replyToEmacs) {
			if (rawRow == startSRow && rawCol == startSCol 
			 && row == endSRow && col == endSCol) {
			 	/* Use short-form emacs select */
				strcpy(line, "\033[t");
				line[3] = ' ' + endSCol + 1;
				line[4] = ' ' + endSRow + 1;
				v_write(screen->respond, line, 5);
			} else {
				/* long-form, specify everything */
				strcpy(line, "\033[T");
				line[3] = ' ' + startSCol + 1;
				line[4] = ' ' + startSRow + 1;
				line[5] = ' ' + endSCol + 1;
				line[6] = ' ' + endSRow + 1;
				line[7] = ' ' + col + 1;
				line[8] = ' ' + row + 1;
				v_write(screen->respond, line, 9);
			}
			TrackText(0, 0, 0, 0);
		}
		SaltTextAway(startSRow, startSCol, endSRow, endSCol,
			     params, num_params);
	} else DisownSelection(term);

	/* TrackText(0, 0, 0, 0); */
	eventMode = NORMAL;
}

#define Abs(x)		((x) < 0 ? -(x) : (x))

/* ARGSUSED */
static void do_start_extend (w, event, params, num_params, use_cursor_loc)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
Bool use_cursor_loc;
{
	TScreen *screen = &((XtermWidget)w)->screen;
	int row, col, coord;

	if (SendMousePosition(w, event)) return;
	firstValidRow = 0;
	lastValidRow  = screen->max_row;
	SetSelectUnit(event->xbutton.time, selectUnit);
	replyToEmacs = FALSE;

	if (numberOfClicks == 1) {
		/* Save existing selection so we can reestablish it if the guy
		   extends past the other end of the selection */
		saveStartRRow = startERow = startRRow;
		saveStartRCol = startECol = startRCol;
		saveEndRRow   = endERow   = endRRow;
		saveEndRCol   = endECol   = endRCol;
	} else {
		/* He just needed the selection mode changed, use old values. */
		startERow = startRRow = saveStartRRow;
		startECol = startRCol = saveStartRCol;
		endERow   = endRRow   = saveEndRRow;
		endECol   = endRCol   = saveEndRCol;

	}
	if (use_cursor_loc) {
	    row = screen->cursor_row;
	    col = screen->cursor_col;
	} else {
	    PointToRowCol(event->xbutton.y, event->xbutton.x, &row, &col);
	}
	coord = Coordinate(row, col);

	if (Abs(coord - Coordinate(startSRow, startSCol))
	     < Abs(coord - Coordinate(endSRow, endSCol))
	    || coord < Coordinate(startSRow, startSCol)) {
	 	/* point is close to left side of selection */
		eventMode = LEFTEXTENSION;
		startERow = row;
		startECol = col;
	} else {
	 	/* point is close to left side of selection */
		eventMode = RIGHTEXTENSION;
		endERow = row;
		endECol = col;
	}
	ComputeSelect(startERow, startECol, endERow, endECol, True);
}

ExtendExtend (row, col)
int row, col;
{
	int coord = Coordinate(row, col);
	
	if (eventMode == LEFTEXTENSION 
	 && (coord + (selectUnit!=SELECTCHAR)) > Coordinate(endSRow, endSCol)) {
		/* Whoops, he's changed his mind.  Do RIGHTEXTENSION */
		eventMode = RIGHTEXTENSION;
		startERow = saveStartRRow;
		startECol = saveStartRCol;
	} else if (eventMode == RIGHTEXTENSION
	 && coord < Coordinate(startSRow, startSCol)) {
	 	/* Whoops, he's changed his mind.  Do LEFTEXTENSION */
		eventMode = LEFTEXTENSION;
		endERow   = saveEndRRow;
		endECol   = saveEndRCol;
	}
	if (eventMode == LEFTEXTENSION) {
		startERow = row;
		startECol = col;
	} else {
		endERow = row;
		endECol = col;
	}
	ComputeSelect(startERow, startECol, endERow, endECol, False);
}


void HandleStartExtend(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
    do_start_extend (w, event, params, num_params, False);
}

void HandleKeyboardStartExtend(w, event, params, num_params)
Widget w;
XEvent *event;			/* must be XButtonEvent* */
String *params;			/* unused */
Cardinal *num_params;		/* unused */
{
    do_start_extend (w, event, params, num_params, True);
}





ScrollSelection(screen, amount)
register TScreen* screen;
register int amount;
{
    register int minrow = -screen->savedlines;

    /* Sent by scrollbar stuff, so amount never takes selection out of
       saved text */

    /* XXX - the preceeding is false; cat /etc/termcap (or anything
       larger than the number of saved lines plus the screen height) and then
       hit extend select */

    startRRow += amount; endRRow += amount;
    startSRow += amount; endSRow += amount;
    rawRow += amount;
    screen->startHRow += amount;
    screen->endHRow += amount;

    if (startRRow < minrow) {
	startRRow = minrow;
	startRCol = 0;
    }
    if (endRRow < minrow) {
	endRRow = minrow;
        endRCol = 0;
    }
    if (startSRow < minrow) {
	startSRow = minrow;
	startSCol = 0;
    }
    if (endSRow < minrow) {
	endSRow = minrow;
	endSCol = 0;
    }
    if (rawRow < minrow) {
	rawRow = minrow;
	rawCol = 0;
    }
    if (screen->startHRow < minrow) {
	screen->startHRow = minrow;
	screen->startHCol = 0;
    }
    if (screen->endHRow < minrow) {
	screen->endHRow = minrow;
	screen->endHCol = 0;
    }
    screen->startHCoord = Coordinate (screen->startHRow, screen->startHCol);
    screen->endHCoord = Coordinate (screen->endHRow, screen->endHCol);
}


/*ARGSUSED*/
ResizeSelection (screen, rows, cols)
    TScreen *screen;
    int rows, cols;
{
    rows--;				/* decr to get 0-max */
    cols--;

    if (startRRow > rows) startRRow = rows;
    if (startSRow > rows) startSRow = rows;
    if (endRRow > rows) endRRow = rows;
    if (endSRow > rows) endSRow = rows;
    if (rawRow > rows) rawRow = rows;

    if (startRCol > cols) startRCol = cols;
    if (startSCol > cols) startSCol = cols;
    if (endRCol > cols) endRCol = cols;
    if (endSCol > cols) endSCol = cols;
    if (rawCol > cols) rawCol = cols;
}

static PointToRowCol(y, x, r, c)
register int y, x;
int *r, *c;
/* Convert pixel coordinates to character coordinates.
   Rows are clipped between firstValidRow and lastValidRow.
   Columns are clipped between to be 0 or greater, but are not clipped to some
       maximum value. */
{
	register TScreen *screen = &term->screen;
	register row, col;

	row = (y - screen->border) / FontHeight(screen);
	if(row < firstValidRow)
		row = firstValidRow;
	else if(row > lastValidRow)
		row = lastValidRow;
	col = (x - screen->border - screen->scrollbar) / FontWidth(screen);
	if(col < 0)
		col = 0;
	else if(col > screen->max_col+1) {
		col = screen->max_col+1;
	}
	*r = row;
	*c = col;
}

int LastTextCol(row)
register int row;
{
	register TScreen *screen =  &term->screen;
	register int i;
	register Char *ch;

	for(i = screen->max_col,
	 ch = screen->buf[2 * (row + screen->topline)] + i ;
	 i > 0 && (*ch == ' ' || *ch == 0); ch--, i--);
	return(i);
}	

static int charClass[128] = {
/* NUL  SOH  STX  ETX  EOT  ENQ  ACK  BEL */
    32,   1,   1,   1,   1,   1,   1,   1,
/*  BS   HT   NL   VT   NP   CR   SO   SI */
     1,  32,   1,   1,   1,   1,   1,   1,
/* DLE  DC1  DC2  DC3  DC4  NAK  SYN  ETB */
     1,   1,   1,   1,   1,   1,   1,   1,
/* CAN   EM  SUB  ESC   FS   GS   RS   US */
     1,   1,   1,   1,   1,   1,   1,   1,
/*  SP    !    "    #    $    %    &    ' */
    32,  33,  34,  35,  36,  37,  38,  39,
/*   (    )    *    +    ,    -    .    / */
    40,  41,  42,  43,  44,  45,  46,  47,
/*   0    1    2    3    4    5    6    7 */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   8    9    :    ;    <    =    >    ? */
    48,  48,  58,  59,  60,  61,  62,  63,
/*   @    A    B    C    D    E    F    G */
    64,  48,  48,  48,  48,  48,  48,  48,
/*   H    I    J    K    L    M    N    O */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   P    Q    R    S    T    U    V    W */ 
    48,  48,  48,  48,  48,  48,  48,  48,
/*   X    Y    Z    [    \    ]    ^    _ */
    48,  48,  48,  91,  92,  93,  94,  48,
/*   `    a    b    c    d    e    f    g */
    96,  48,  48,  48,  48,  48,  48,  48,
/*   h    i    j    k    l    m    n    o */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   p    q    r    s    t    u    v    w */
    48,  48,  48,  48,  48,  48,  48,  48,
/*   x    y    z    {    |    }    ~  DEL */
    48,  48,  48, 123, 124, 125, 126,   1};


int SetCharacterClassRange (low, high, value)
    register int low, high;		/* in range of [0..127] */
    register int value;			/* arbitrary */
{

    if (low < 0 || high > 127 || high < low) return (-1);

    for (; low <= high; low++) charClass[low] = value;

    return (0);
}


ComputeSelect(startRow, startCol, endRow, endCol, extend)
int startRow, startCol, endRow, endCol;
Bool extend;
{
	register TScreen *screen = &term->screen;
	register Char *ptr;
	register int length;
	register int class;
	int osc = startSCol;

	if (Coordinate(startRow, startCol) <= Coordinate(endRow, endCol)) {
		startSRow = startRRow = startRow;
		startSCol = startRCol = startCol;
		endSRow   = endRRow   = endRow;
		endSCol   = endRCol   = endCol;
	} else {	/* Swap them */
		startSRow = startRRow = endRow;
		startSCol = startRCol = endCol;
		endSRow   = endRRow   = startRow;
		endSCol   = endRCol   = startCol;
	}	

	switch (selectUnit) {
		case SELECTCHAR :
			if (startSCol > (LastTextCol(startSRow) + 1)) {
				startSCol = 0;
				startSRow++;
			}
			if (endSCol > (LastTextCol(endSRow) + 1)) {
				endSCol = 0;
				endSRow++;
			}
			break;
		case SELECTWORD :
			if (startSCol > (LastTextCol(startSRow) + 1)) {
				startSCol = 0;
				startSRow++;
			} else {
				ptr = screen->buf[2*(startSRow+screen->topline)]
				 + startSCol;
				class = charClass[*ptr];
				do {
					--startSCol;
					--ptr;
				} while (startSCol >= 0
				 && charClass[*ptr] == class);
				++startSCol;
			}
			if (endSCol > (LastTextCol(endSRow) + 1)) {
				endSCol = 0;
				endSRow++;
			} else {
				length = LastTextCol(endSRow);
				ptr = screen->buf[2*(endSRow+screen->topline)]
				 + endSCol;
				class = charClass[*ptr];
				do {
					++endSCol;
					++ptr;
				} while (endSCol <= length
				 && charClass[*ptr] == class);
				/* Word select selects if pointing to any char
				   in "word", especially in that it includes
				   the last character in a word.  So no --endSCol
				   and do special eol handling */
				if (endSCol > length+1) {
					endSCol = 0;
					++endSRow;
				}
			}
			break;
		case SELECTLINE :
			if (term->screen.cutToBeginningOfLine) {
			    startSCol = 0;
			} else if (!extend) {
			    startSCol = osc;
			}
			if (term->screen.cutNewline) {
			    endSCol = 0;
			    ++endSRow;
			} else {
			    endSCol = LastTextCol(endSRow) + 1;
			}
			break;
	}

	TrackText(startSRow, startSCol, endSRow, endSCol);
	return;
}


TrackText(frow, fcol, trow, tcol)
register int frow, fcol, trow, tcol;
/* Guaranteed (frow, fcol) <= (trow, tcol) */
{
	register int from, to;
	register TScreen *screen = &term->screen;
	int old_startrow, old_startcol, old_endrow, old_endcol;

	/* (frow, fcol) may have been scrolled off top of display */
	if (frow < 0)
		frow = fcol = 0;
	/* (trow, tcol) may have been scrolled off bottom of display */
	if (trow > screen->max_row+1) {
		trow = screen->max_row+1;
		tcol = 0;
	}
	old_startrow = screen->startHRow;
	old_startcol = screen->startHCol;
	old_endrow = screen->endHRow;
	old_endcol = screen->endHCol;
	if (frow == old_startrow && fcol == old_startcol &&
	    trow == old_endrow   && tcol == old_endcol) return;
	screen->startHRow = frow;
	screen->startHCol = fcol;
	screen->endHRow   = trow;
	screen->endHCol   = tcol;
	from = Coordinate(frow, fcol);
	to = Coordinate(trow, tcol);
	if (to <= screen->startHCoord || from > screen->endHCoord) {
	    /* No overlap whatsoever between old and new hilite */
	    ReHiliteText(old_startrow, old_startcol, old_endrow, old_endcol);
	    ReHiliteText(frow, fcol, trow, tcol);
	} else {
	    if (from < screen->startHCoord) {
		    /* Extend left end */
		    ReHiliteText(frow, fcol, old_startrow, old_startcol);
	    } else if (from > screen->startHCoord) {
		    /* Shorten left end */
		    ReHiliteText(old_startrow, old_startcol, frow, fcol);
	    }
	    if (to > screen->endHCoord) {
		    /* Extend right end */
		    ReHiliteText(old_endrow, old_endcol, trow, tcol);
	    } else if (to < screen->endHCoord) {
		    /* Shorten right end */
		    ReHiliteText(trow, tcol, old_endrow, old_endcol);
	    }
	}
	screen->startHCoord = from;
	screen->endHCoord = to;
}

ReHiliteText(frow, fcol, trow, tcol)
register int frow, fcol, trow, tcol;
/* Guaranteed that (frow, fcol) <= (trow, tcol) */
{
	register TScreen *screen = &term->screen;
	register int i;

	if (frow < 0)
	    frow = fcol = 0;
	else if (frow > screen->max_row)
	    return;		/* nothing to do, since trow >= frow */

	if (trow < 0)
	    return;		/* nothing to do, since frow <= trow */
	else if (trow > screen->max_row) {
	    trow = screen->max_row;
	    tcol = screen->max_col+1;
	}
	if (frow == trow && fcol == tcol)
		return;

	if(frow != trow) {	/* do multiple rows */
		if((i = screen->max_col - fcol + 1) > 0) {     /* first row */
		    ScrnRefresh(screen, frow, fcol, 1, i, True);
		}
		if((i = trow - frow - 1) > 0) {		       /* middle rows*/
		    ScrnRefresh(screen, frow+1, 0,i, screen->max_col+1, True);
		}
		if(tcol > 0 && trow <= screen->max_row) {      /* last row */
		    ScrnRefresh(screen, trow, 0, 1, tcol, True);
		}
	} else {		/* do single row */
		ScrnRefresh(screen, frow, fcol, 1, tcol - fcol, True);
	}
}

SaltTextAway(crow, ccol, row, col, params, num_params)
/*register*/ int crow, ccol, row, col;
String *params;			/* selections */
Cardinal num_params;
/* Guaranteed that (crow, ccol) <= (row, col), and that both points are valid
   (may have row = screen->max_row+1, col = 0) */
{
	register TScreen *screen = &term->screen;
	register int i, j = 0;
	char *line, *lp;
	static _OwnSelection();

	if (crow == row && ccol > col) {
	    int tmp = ccol;
	    ccol = col;
	    col = tmp;
	}

	--col;
	/* first we need to know how long the string is before we can save it*/

	if ( row == crow ) j = Length(screen, crow, ccol, col);
	else {	/* two cases, cut is on same line, cut spans multiple lines */
		j += Length(screen, crow, ccol, screen->max_col) + 1;
		for(i = crow + 1; i < row; i++) 
			j += Length(screen, i, 0, screen->max_col) + 1;
		if (col >= 0)
			j += Length(screen, row, 0, col);
	}
	
	/* now get some memory to save it in */

	if (screen->selection_size <= j) {
	    if((line = malloc((unsigned) j + 1)) == (char *)NULL)
		SysError(ERROR_BMALLOC2);
	    XtFree(screen->selection);
	    screen->selection = line;
	    screen->selection_size = j + 1;
	} else line = screen->selection;
	if (!line || j < 0) return;

	line[j] = '\0';		/* make sure it is null terminated */
	lp = line;		/* lp points to where to save the text */
	if ( row == crow ) lp = SaveText(screen, row, ccol, col, lp);
	else {
		lp = SaveText(screen, crow, ccol, screen->max_col, lp);
		*lp ++ = '\n';	/* put in newline at end of line */
		for(i = crow +1; i < row; i++) {
			lp = SaveText(screen, i, 0, screen->max_col, lp);
			*lp ++ = '\n';
			}
		if (col >= 0)
			lp = SaveText(screen, row, 0, col, lp);
	}
	*lp = '\0';		/* make sure we have end marked */
	
	screen->selection_length = j;
	_OwnSelection(term, params, num_params);
}

static Boolean ConvertSelection(w, selection, target,
				type, value, length, format)
Widget w;
Atom *selection, *target, *type;
caddr_t *value;
unsigned long *length;
int *format;
{
    Display* d = XtDisplay(w);
    XtermWidget xterm = (XtermWidget)w;

    if (xterm->screen.selection == NULL) return False; /* can this happen? */

    if (*target == XA_TARGETS(d)) {
	Atom* targetP;
	Atom* std_targets;
	unsigned long std_length;
	XmuConvertStandardSelection(
		    w, xterm->screen.selection_time, selection,
		    target, type, (caddr_t*)&std_targets, &std_length, format
		   );
	*length = std_length + 5;
	*value = (caddr_t)XtMalloc(sizeof(Atom)*(*length));
	targetP = *(Atom**)value;
	*targetP++ = XA_STRING;
	*targetP++ = XA_TEXT(d);
	*targetP++ = XA_COMPOUND_TEXT(d);
	*targetP++ = XA_LENGTH(d);
	*targetP++ = XA_LIST_LENGTH(d);
	bcopy((char*)std_targets, (char*)targetP, sizeof(Atom)*std_length);
	XtFree((char*)std_targets);
	*type = XA_ATOM;
	*format = 32;
	return True;
    }

    if (*target == XA_STRING ||
	*target == XA_TEXT(d) ||
	*target == XA_COMPOUND_TEXT(d)) {
	if (*target == XA_COMPOUND_TEXT(d))
	    *type = *target;
	else
	    *type = XA_STRING;
	*value = xterm->screen.selection;
	*length = xterm->screen.selection_length;
	*format = 8;
	return True;
    }
    if (*target == XA_LIST_LENGTH(d)) {
	*value = XtMalloc(4);
	if (sizeof(long) == 4)
	    *(long*)*value = 1;
	else {
	    long temp = 1;
	    bcopy( ((char*)&temp)+sizeof(long)-4, (char*)*value, 4);
	}
	*type = XA_INTEGER;
	*length = 1;
	*format = 32;
	return True;
    }
    if (*target == XA_LENGTH(d)) {
	*value = XtMalloc(4);
	if (sizeof(long) == 4)
	    *(long*)*value = xterm->screen.selection_length;
	else {
	    long temp = xterm->screen.selection_length;
	    bcopy( ((char*)&temp)+sizeof(long)-4, (char*)*value, 4);
	}
	*type = XA_INTEGER;
	*length = 1;
	*format = 32;
	return True;
    }
    if (XmuConvertStandardSelection(w, xterm->screen.selection_time, selection,
				    target, type, value, length, format))
	return True;

    /* else */
    return False;

}


static void LoseSelection(w, selection)
  Widget w;
  Atom *selection;
{
    register TScreen* screen = &((XtermWidget)w)->screen;
    register Atom* atomP;
    int i;
    for (i = 0, atomP = screen->selection_atoms;
	 i < screen->selection_count; i++, atomP++)
    {
	if (*selection == *atomP) *atomP = (Atom)0;
	switch (*atomP) {
	  case XA_CUT_BUFFER0:
	  case XA_CUT_BUFFER1:
	  case XA_CUT_BUFFER2:
	  case XA_CUT_BUFFER3:
	  case XA_CUT_BUFFER4:
	  case XA_CUT_BUFFER5:
	  case XA_CUT_BUFFER6:
	  case XA_CUT_BUFFER7:	*atomP = (Atom)0;
	}
    }

    for (i = screen->selection_count; i; i--) {
	if (screen->selection_atoms[i-1] != 0) break;
    }
    screen->selection_count = i;

    for (i = 0, atomP = screen->selection_atoms;
	 i < screen->selection_count; i++, atomP++)
    {
	if (*atomP == (Atom)0) {
	    *atomP = screen->selection_atoms[--screen->selection_count];
	}
    }

    if (screen->selection_count == 0)
	TrackText(0, 0, 0, 0);
}


/* ARGSUSED */
static void SelectionDone(w, selection, target)
Widget w;
Atom *selection, *target;
{
    /* empty proc so Intrinsics know we want to keep storage */
}


static /* void */ _OwnSelection(term, selections, count)
register XtermWidget term;
String *selections;
Cardinal count;
{
    Atom* atoms = term->screen.selection_atoms;
    int i;
    Boolean have_selection = False;

    if (term->screen.selection_length < 0) return;

    if (count > term->screen.sel_atoms_size) {
	XtFree((char*)atoms);
	atoms = (Atom*)XtMalloc(count*sizeof(Atom));
	term->screen.selection_atoms = atoms;
	term->screen.sel_atoms_size = count;
    }
    XmuInternStrings( XtDisplay((Widget)term), selections, count, atoms );
    for (i = 0; i < count; i++) {
	int buffer;
	switch (atoms[i]) {
	  case XA_CUT_BUFFER0: buffer = 0; break;
	  case XA_CUT_BUFFER1: buffer = 1; break;
	  case XA_CUT_BUFFER2: buffer = 2; break;
	  case XA_CUT_BUFFER3: buffer = 3; break;
	  case XA_CUT_BUFFER4: buffer = 4; break;
	  case XA_CUT_BUFFER5: buffer = 5; break;
	  case XA_CUT_BUFFER6: buffer = 6; break;
	  case XA_CUT_BUFFER7: buffer = 7; break;
	  default:	       buffer = -1;
	}
	if (buffer >= 0)
	    XStoreBuffer( XtDisplay((Widget)term), term->screen.selection,
			  term->screen.selection_length, buffer );
	else if (!replyToEmacs) {
	    have_selection |=
		XtOwnSelection( (Widget)term, atoms[i],
			    term->screen.selection_time,
			    ConvertSelection, LoseSelection, SelectionDone );
	}
    }
    if (!replyToEmacs)
	term->screen.selection_count = count;
    if (!have_selection)
	TrackText(0, 0, 0, 0);
}

/* void */ DisownSelection(term)
register XtermWidget term;
{
    Atom* atoms = term->screen.selection_atoms;
    Cardinal count = term->screen.selection_count;
    int i;

    for (i = 0; i < count; i++) {
	int buffer;
	switch (atoms[i]) {
	  case XA_CUT_BUFFER0: buffer = 0; break;
	  case XA_CUT_BUFFER1: buffer = 1; break;
	  case XA_CUT_BUFFER2: buffer = 2; break;
	  case XA_CUT_BUFFER3: buffer = 3; break;
	  case XA_CUT_BUFFER4: buffer = 4; break;
	  case XA_CUT_BUFFER5: buffer = 5; break;
	  case XA_CUT_BUFFER6: buffer = 6; break;
	  case XA_CUT_BUFFER7: buffer = 7; break;
	  default:	       buffer = -1;
	}
	if (buffer < 0)
	    XtDisownSelection( (Widget)term, atoms[i],
			       term->screen.selection_time );
    }
    term->screen.selection_count = 0;
    term->screen.startHRow = term->screen.startHCol = 0;
    term->screen.endHRow = term->screen.endHCol = 0;
}


/* returns number of chars in line from scol to ecol out */
int Length(screen, row, scol, ecol)
register int row, scol, ecol;
register TScreen *screen;
{
	register Char *ch;

	ch = screen->buf[2 * (row + screen->topline)];
	while (ecol >= scol && (ch[ecol] == ' ' || ch[ecol] == 0))
	    ecol--;
	return (ecol - scol + 1);
}

/* copies text into line, preallocated */
char *SaveText(screen, row, scol, ecol, lp)
int row;
int scol, ecol;
TScreen *screen;
register char *lp;		/* pointer to where to put the text */
{
	register int i = 0;
	register Char *ch = screen->buf[2 * (row + screen->topline)];
	register int c;

	if ((i = Length(screen, row, scol, ecol)) == 0) return(lp);
	ecol = scol + i;
	for (i = scol; i < ecol; i++) {
		if ((c = ch[i]) == 0)
			c = ' ';
		else if(c < ' ') {
			if(c == '\036')
				c = '#';
			else
				c += 0x5f;
		} else if(c == 0x7f)
			c = 0x5f;
		*lp++ = c;
	}
	return(lp);
}

EditorButton(event)
register XButtonEvent *event;
{
	register TScreen *screen = &term->screen;
	int pty = screen->respond;
	char line[6];
	register unsigned row, col;
	int button; 

	button = event->button - 1; 

	row = (event->y - screen->border) 
	 / FontHeight(screen);
	col = (event->x - screen->border - screen->scrollbar)
	 / FontWidth(screen);
	(void) strcpy(line, "\033[M");
	if (screen->send_mouse_pos == 1) {
		line[3] = ' ' + button;
	} else {
		line[3] = ' ' + (KeyState(event->state) << 2) + 
			((event->type == ButtonPress)? button:3);
	}
	line[4] = ' ' + col + 1;
	line[5] = ' ' + row + 1;
	v_write(pty, line, 6);
}


/*ARGSUSED*/
void HandleGINInput (w, event, param_list, nparamsp)
    Widget w;
    XEvent *event;
    String *param_list;
    Cardinal *nparamsp;
{
    if (term->screen.TekGIN && *nparamsp == 1) {
	int c = param_list[0][0];
	switch (c) {
	  case 'l': case 'm': case 'r':
	  case 'L': case 'M': case 'R':
	    break;
	  default:
	    Bell ();			/* let them know they goofed */
	    c = 'l';			/* provide a default */
	}
	TekEnqMouse (c | 0x80);
	TekGINoff();
    } else {
	Bell ();
    }
}


/* ARGSUSED */
void HandleSecure(w, event, params, param_count)
    Widget w;
    XEvent *event;		/* unused */
    String *params;		/* [0] = volume */
    Cardinal *param_count;	/* 0 or 1 */
{
    Time time = CurrentTime;

    if ((event->xany.type == KeyPress) ||
	(event->xany.type == KeyRelease))
	time = event->xkey.time;
    else if ((event->xany.type == ButtonPress) ||
	     (event->xany.type == ButtonRelease))
      time = event->xbutton.time;
    DoSecureKeyboard (time);
}
