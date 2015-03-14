
/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering 
 * teams as part of the course "Introduction to Software Engineering" 
 * under the supervision of Professor G. Davida.
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *     		soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations. Selling this software for profit without
 * the permission of the Board of Regents of the University
 * of Wisconsin is strictly forbidden. 
 *
 * Contact:	soft-eng@cs.uwm.edu
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *    		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 *
 *		HISTORY,CLAIMS and CONTRIBUTIONS
 */

/*	SC	A Spreadsheet Calculator
 *		Main driver
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *              
 *              A cursor style menu by SWLin, 8/90
 *
 *      Still more mods by Bruce Backman, Rich Sample  7/91
 *                   (see change list)
 *
 ***********************************************************************
 *                                                                     *
 *      More modifications by Mike Frey, Fang Wang and Jim Cornelius   *
 *                                     Fall,1991                       *
 *      Mouse implemented by Mike Frey, 11/91                          *
 *      All error correctness and using mouse moving on the screen     *
 *      are done by Jim Cornelius.      11/91                          *
 *      All the matrix operations are added by Fang Wang 12/91         *
 *                                                                     *
 ***********************************************************************
 */

#include <signal.h>
#ifdef Ultrix
/*#include "Ultrix_curses.h" */
#include "Ultrix_curses.h" 
#else /* Not Ultrix */
/*  #include <cursesX.h> */
/*#include <curses.h>*/
#include <curses.h>
#endif /* Ultrix */

#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <stdio.h>
#include <ctype.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include "sc.h"
#include "scXstuff.h"

char *getenv();
extern FILE *openout();

#ifdef SYSV3
void exit();
#endif

/*
 * CODE REVISION NUMBER:
 *
 * The part after the first colon, except the last char, appears on the screen.
 */


char *rev = "$Revision: 6.1 $";

#ifndef DFLT_PAGER
#define	DFLT_PAGER "more"	/* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD 160	/* for ! command below */

/* Globals defined in sc.h */

struct ent *tbl[MAXROWS][MAXCOLS];
int running;
int arg = 1;
int strow, stcol;         /* first cell shown on screen */
int currow, curcol;       /* row/col of current position */
int savedrow, savedcol;
int FullUpdate;          /** 1, if want to update the whole screen **/
int maxrow, maxcol;
int fwidth[MAXCOLS];
int precision[MAXCOLS];
char col_hidden[MAXCOLS];
char row_hidden[MAXROWS];
char line[1000];
int changed;
struct ent *to_fix;
int modflg;
int numeric;
char *mdir;
int showsc, showsr;	/* Starting cell for highlighted range */

char curfile[1024];
char    revmsg[80];

int  linelim = -1;

int  showtop   = 1;	/* Causes current cell value display in top line  */
int  showcell  = 1;	/* Causes current cell to be highlighted	  */
int  showrange = 0;	/* Causes ranges to be highlighted		  */
int  showneed  = 0;	/* Causes cells needing values to be highlighted  */
int  showexpr  = 0;	/* Causes cell exprs to be displayed, highlighted */

int  autocalc = 1 ;	/* 1 to calculate after each update */
int  calc_order = BYROWS;
int  tbl_style = 0;	/* headers for T command output */

int  lastmx, lastmy;	/* Screen address of the cursor */
int  lastcol;		/* Spreadsheet Column the cursor was in last */
char *under_cursor = " "; /* Data under the < cursor */
char laststring[1024];    /* string at position lastmx,lastmy */
int lstringstart = -1,      /* table column laststring starts in */
    lstringend = -1;        /*  ... and column it ends in */

char stringbuf[1024];     /* build misc. strings for display on screen*/

#ifdef VMS
int VMS_read_raw = 0;
#endif

int seenerr;

struct m_range_sd {             
       int ssr,ser,ssc,sec;
       int dsr,der,dsc,dec;
      };
struct m_range {
       int sr,er,sc,ec;
      };
struct m_range_sd *rge_sd;
struct m_range *rge_s1, *rge_s2, *rge_d;


/* The following function was modified 7-24-91 by B. Backman to remove curses
  functions.  You know as much as I do about who is setting line and
  linelim, but if it works in printw, it ought to work the same way for
  sprintf */
yyerror (err)
char *err; {
    if (seenerr) return;
    seenerr++;
   clearlines(1,1);
   sprintf(stringbuf,"%s: %.*s<=%s",err,linelim,line,line+linelim);
   XDrawImageString(dpy,mainwin,maingc,
	       textcol(0), textrow(1),
	       stringbuf, strlen(stringbuf));
   XFlush(dpy);
   seenerr = 1;
}

/*This fuction returns a pointer  to the "struct ent",cell of the*/
/*spreedsheet,specifed by row,col.   */

struct ent *
lookat(row,col){
    register struct ent **p;
    if (row < 0)
	row = 0;
    else if (row > MAXROWS-1) 
	row = MAXROWS-1;
    if (col < 0) 
	col = 0;
    else if (col > MAXCOLS-1)
	col = MAXCOLS-1;
    p = &tbl[row][col];
    if (*p==0) {
	*p = (struct ent *) xmalloc ((unsigned)sizeof (struct ent));
	if (row>maxrow) maxrow = row;
	if (col>maxcol) maxcol = col;
	(*p)->label = 0;
	(*p)->flags = 0;
	(*p)->row = row;
	(*p)->col = col;
	(*p)->expr = 0;
	(*p)->v = (double) 0.0;
    }
    return *p;
}

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */

free_ent(p)
register struct ent *p;
{
    p->next = to_fix;
    to_fix = p;
    p->flags |= is_deleted;
}

/* 
 * Clear the cell pointed by p if we don't need to fix, move to
 * next cell. clearent() is in interp.c 
 */

flush_saved()
{
    register struct ent *p;
    register struct ent *q;

    if (!(p = to_fix))
	return;
    while (p) {
	(void) clearent(p);
	q = p->next;
	xfree((char *)p);
	p = q;
    }
    to_fix = 0;
}

update ()
{
    register    row,
                col;
    register struct ent **p;
    int     mxcol;
    int     mxrow;
    int     rows;
    int     cols;
    int     minsr, minsc, maxsr, maxsc;
    register r;
    register i;
    char field[1024];        /* string for filling a field on screen */
    int     found_cursor=0;   /* true when we've accounted for contents of current cell*/
    while (row_hidden[currow])   /* You can't hide the last row or col */
	currow++;
    while (col_hidden[curcol])
	curcol++;
    /* First see if the last display still covers curcol */
    if (stcol <= curcol) { 
	for (i = stcol, cols = 0, col = RESCOL;
            (col + fwidth[i]) < maintextcols-1 && i < MAXCOLS; i++) {
	    cols++;
	    if (col_hidden[i])
		continue;
	    col += fwidth[i];
	}
    }
    while (stcol + cols - 1 < curcol || curcol < stcol) {
	FullUpdate++;
	if (stcol - 1 == curcol) {    /* How about back one? */
	    stcol--;
	} else if (stcol + cols == curcol) {   /* Forward one? */
	    stcol++;
	} else {
	    /* Try to put the cursor in the center of the screen */
        col = (maintextcols - RESCOL - fwidth[curcol]) / 2 + RESCOL;
	    stcol = curcol;
	    for (i=curcol-1; i >= 0 && col-fwidth[i] > RESCOL; i--) {
          stcol--;
          if (col_hidden[i])
             continue;
          col -= fwidth[i];
	    }
	}
	/* Now pick up the counts again */
	for (i = stcol, cols = 0, col = RESCOL;
            (col + fwidth[i]) < maintextcols-1 && i < MAXCOLS; i++) {
	    cols++;
	    if (col_hidden[i])
		continue;
	    col += fwidth[i];
	}
    }
    
    /* Now - same process on the rows */
    if (strow <= currow) { 
    for (i = strow, rows=0, row=RESROW; row<maintextrows && i<MAXROWS; i++){
	    rows++;
	    if (row_hidden[i])
		continue;
	    row++;
	}
    }
    while (strow + rows - 1 < currow || currow < strow) {
	FullUpdate++;
	if (strow - 1 == currow) {    /* How about up one? */
	    strow--;
	} else if (strow + rows == currow) {   /* Down one? */
	    strow++;
	} else {
	    /* Try to put the cursor in the center of the screen */
        row = (maintextrows - RESROW) / 2 + RESROW;
	    strow = currow;
	    for (i=currow-1; i >= 0 && row-1 > RESROW; i--) {
		strow--;
		if (row_hidden[i])
		    continue;
		row--;
	    }
	}
	/* Now pick up the counts again */
    for (i = strow, rows=0, row=RESROW; row<maintextrows && i<MAXROWS; i++) {
	    rows++;
	    if (row_hidden[i])
		continue;
	    row++;
	}
    }
    mxcol = stcol + cols - 1;
    mxrow = strow + rows - 1;

    /* relabel the rows and columns if a full update is performed */
    if (FullUpdate) {
        cleardownfrom(2);

	for (row=RESROW, i=strow; i <= mxrow; i++) {
	    if (row_hidden[i]) 
		continue;
#if MAXROW < 1000
        sprintf(stringbuf, "%-*d", RESCOL-1, i);
#else
        sprintf(stringbuf, "%-*d", RESCOL, i);
#endif
        XDrawImageString(dpy,mainwin,maingcreversed,
          textcol(0), textrow(row),
          stringbuf, strlen(stringbuf) );
	    row++;
	}  /* for row */

        /* the following will work as long as RESCOL<20.  It is 
	 * more efficient than the usual sprintf(stringbuf..) and
	 * XDrawString(...strlen(stringbuf)) combination
	 */
        XDrawImageString(dpy,mainwin,maingcreversed,
			 textcol(0),textrow(3),
			 "                    ",RESCOL);
	for (col=RESCOL, i = stcol; i <= mxcol; i++) {
	    register int k;
	    if (col_hidden[i])
		continue;
	    /*(void) move(3, col);*/
	    k = fwidth[i]/2;
	    if (k == 0)
              /*(void) printw("%1s", coltoa(i)); */
              sprintf(stringbuf,"%1s", coltoa(i));
	    else
              /* (void) printw("%*s%-*s", k, " ", fwidth[i]-k, coltoa(i));*/
              sprintf(stringbuf,"%*s%-*s", k, " ", fwidth[i]-k, coltoa(i));
            XDrawImageString(dpy,mainwin,maingcreversed,
                             textcol(col), textrow(3),
                             stringbuf, strlen(stringbuf) );
	    col += fwidth[i];
	} /* for col */

    /* (void) standend(); */
    }  /* if (FullUpdate) */

    /* Get rid of cursor standout on the cell at previous cursor position */
    /* don't bother if whole screen will be updated */
    /*(void) move(lastmx, lastmy);*/
    if (showcell && !FullUpdate)
        /* repaint(lastmx, lastmy, fwidth[lastcol]); */
	XDrawImageString(dpy,mainwin,maingc,
		         textcol(lastmx), textrow(lastmy),
		         laststring, strlen(laststring) );

    if (showrange) {
	minsr = showsr < currow ? showsr : currow;
	minsc = showsc < curcol ? showsc : curcol;
	maxsr = showsr > currow ? showsr : currow;
	maxsc = showsc > curcol ? showsc : curcol;

	if (showtop) {
	    /* (void) move(1,0);
	    (void) clrtoeol();
	    (void) printw("Default range:  %s",
			    r_name(minsr, minsc, maxsr, maxsc)); */
	    sprintf(stringbuf,"Default range:  %s",
			    r_name(minsr, minsc, maxsr, maxsc)); 
            XDrawImageString(dpy,mainwin,maingc,
                             textcol(0), textrow(1),
                             stringbuf, strlen(stringbuf) );
	}
    }

    /* if the current cell was previously painted over by laststring, we
     * don't need to look it up again, unless it has changed.  So, we can 
     * say we have the cursor accounted for */
    if ((row==currow)&&(lstringstart<=curcol) && (lstringend>=curcol)){
      found_cursor = 1;
    }

    /* Repaint the visible screen (where necessary) */
    for (row = strow, r = RESROW; row <= mxrow; row++) {
	register c = RESCOL;
	int do_stand = 0;   /* true if standout is set for cell */
	int get_value;         /* true if we need to determine a cell's contents*/
	int show_value;        /* true if we need to display a cell's contents */
	int fieldlen;
	int nextcol;
	if (row_hidden[row])
	    continue;

        /* if we are on the cursor row, and we have not accounted for the cursor,
	 * look at contents of all cells, in order to get correct laststring 
	 */
        if ((row==currow)&& !found_cursor){
	  get_value = 1;
        }

	for (p = &tbl[row][col = stcol]; col <= mxcol;
	         p += nextcol - col,  col = nextcol, c += fieldlen) {
	    nextcol = col+1;
	    if (col_hidden[col]) {
		fieldlen = 0;
		continue;
	    }

	    fieldlen = fwidth[col];

	    /*
	     * Set standout if:
	     *
	     * - showing ranges, and not showing cells which need to be filled
	     *   in, and not showing cell expressions, and in a range, OR
	     *
	     * - if showing cells which need to be filled in and this one is
	     *   of that type (has a value and doesn't have an expression, or
	     *   it is a string expression), OR
	     *
	     * - if showing cells which have expressions and this one does.
	     */
            if ((showrange && (! showneed) && (! showexpr)
			   && (row >= minsr) && (row <= maxsr)
			   && (col >= minsc) && (col <= maxsc))

	     || (showneed && (*p) && ((*p) -> flags & is_valid)
	      && (((*p) -> flags & is_strexpr) || ! ((*p) -> expr)))

	     || (showexpr && (*p) && ((*p) -> expr)))
	    {
		do_stand = 1;
	    }


            /* Display value of cell if:
	     * - (there is a valid cell here AND
	     *      # its value has changed, OR
	     *      # FullUpdate is set),  OR
	     * - do_stand is set for this cell
	     */
	    show_value = (*p && ((*p) -> flags & is_changed || FullUpdate) || do_stand );

	    if (get_value || show_value){ 
		/* (void) move (r, c); */
		if (!*p)
		    *p = lookat(row, col);
		if (do_stand) {
		    /* (void) standout(); */
		    (*p) -> flags |= is_changed; 
		} else {
		    (*p) -> flags &= ~is_changed;
		}
                /* B. Backman, 7-26-91: in the following, cells are no longer "shown"  
		 * when the comments say they are.  They are stored in the variable 
		 * 'field' which is put onto the screen at the bottom of the loop 
		 */
		/*
		 * Show expression; takes priority over other displays:
		 */

		if (showexpr && ((*p) -> expr)) {
		    linelim = 0;
		    editexp (row, col);		/* store expression string in line[] */
		    linelim = -1;
		    showstring (field, line, /* leftflush = */ 1, /* hasvalue = */ 0,
				row, col, & nextcol, mxcol, & fieldlen, r, c);
		}
		else {

		    /*
		     * Show cell's numeric value:
		     */

		    if ((*p) -> flags & is_valid) {
			(void)sprintf(field,"%*.*f", fwidth[col], precision[col], (*p)->v);
			if(strlen(field) > fwidth[col]) {
			    for(i = 0; i<fwidth[col]; i++)
				/* (void)addch('*'); */
				field[i]='*';
                            field[fwidth[col]]=0;
			} 
		    }

		    /*
		     * Show cell's label string:
		     */

		    if ((*p) -> label) {
			showstring (field,
				    (*p) -> label,
				    (*p) -> flags & is_leftflush,
				    (*p) -> flags & is_valid,
				    row, col, & nextcol, mxcol,
				    & fieldlen, r, c);
		    }

		    /*
		     * repaint a blank cell:
		     */

		    if (!((*p)->flags & is_valid) && !(*p)->label) {
			/* (void) printw ("%*s", fwidth[col], " "); */
                        sprintf (field,"%*s", fwidth[col], " "); 
		    }
		}

                if(show_value){
		   XDrawImageString(dpy,mainwin,
				    do_stand ? maingcreversed : maingc,
				    textcol(c),textrow(r),
				    field, strlen(field));
                }
                /* if we just painted over the current cell, save the string
		 * for later repaints 
		 */
                if((row==currow)&&(col<=curcol)&&(curcol<nextcol)){
		  lastmy = r;
		  lastmx = c;
		  strcpy (laststring, field);
		  lstringstart = col;
		  lstringend = nextcol-1;
		  found_cursor = 1;
		  get_value = 0;
                }

		/* if we just painted in the middle of the saved string,
		 * truncate the string accordingly 
		 */
                if ((currow==row)&&(lstringstart<col)&&(col<=lstringend)){
		  int i;
		  int skip=0;
		  for(i=lstringstart; i<col; i++)
		    skip += fwidth[i];
		  laststring[skip]=0;
		  lstringend = col;
		}

		/* if (do_stand) {
		    (void) standend();
		    do_stand = 0; 
		}*/
	    } /* end of repainting a cell */
	} /* for p (each entry in row) */
	r++;
    } /* for row */
	    
/*    (void) move(lastmy, lastmx+fwidth[lastcol]);
#ifndef INTERNATIONAL
    if((inch() & 0x7f) == '<')
#else
    if((inch() & 0xff) == '<')
#endif /* INTERNATIONAL */
/*        (void) addstr(under_cursor);
    lastmy =  RESROW;
    for (row = strow; row < currow; row++)
	if (!row_hidden[row])
		lastmy += 1;
    lastmx = RESCOL;
    for (col = stcol; col < curcol; col++)
	if (!col_hidden[col])
		lastmx += fwidth[col];
    lastcol = curcol;
    (void) move(lastmx, lastmy);
    if (showcell && (! showneed) && (! showexpr)) {
        (void) standout();
        repaint(lastmx, lastmy, fwidth[lastcol]);
        (void) standend();
    }
    (void) move(lastmy, lastmx+fwidth[lastcol]);
#ifndef INTERNATIONAL /*changed to make consistent with all other uses of INTERNATIONAL */
/*    *under_cursor = (inch() & 0x7f);
#else
    *under_cursor = (inch() & 0xff);
#endif /* INTERNATIONAL */
/*    (void) addstr("<");  */

    /* highlight the current cell, unless there is a reason not to */
    if (showcell && (!showneed) && (!showexpr))
	redraw_current(maingcreversed);

    /* mark current cell with a pointer */
    {
    int skip = 0;
    int i;
    for(i=lstringstart; i<=curcol; i++) 
      skip += fwidth[i];
    XDrawImageString(dpy,mainwin,maingcreversed,
		     textcol(lastmx+skip-1), textrow(lastmy),
		     "<", 1);
    } /* mark current */

    (void) show_top_line();
}

/*** B. Backman 7-27-91
 * redraw_current will redraw the cell at the cursor, using the graphics
 * context specified by the argument gc.  The current cursor position on 
 * the screen must be stored in lastmx and lastmy (the col. and row), the 
 * string containing the cursor is stored in laststring and covers table
 * columns lstringstart through lstringend.  Since the cursor is in there,
 * we must, therefore have:  lstrinstart <= lastcol <= lstringend. 
 ***/
redraw_current(gc)
  GC gc;
{
  int i;
  int skip=0;

  for (i=lstringstart; i<curcol; i++)
    skip += fwidth[i];
  XDrawImageString(dpy, mainwin, gc,
	       	   textcol(lastmx+skip), textrow(lastmy),
	       	   laststring+skip,
	           ((fwidth[curcol]<strlen(laststring+skip)) 
		        ? fwidth[curcol]
		        : strlen(laststring+skip)));
 } 


void show_top_line()
{   
    clearlines(0,0);
    stringbuf[0]=0;
    if (linelim >= 0) {
	sprintf(stringbuf,">> %s_", line);
    } else {
	if (showtop) {			/* show top line */
	    register struct ent *p1;
	    int printed = 0;		/* printed something? */

            /*(void) printw ("%s%d ", coltoa (curcol), currow);*/
            sprintf (stringbuf,"%s%d ", coltoa (curcol), currow);

	    if (p1 = tbl [currow] [curcol])
	    {
		if (p1 -> expr)		/* has expr of some type */
		{
		    linelim = 0;
		    editexp (currow, curcol);	/* set line to expr */
		    linelim = -1;
		}

		/*
		 * Display string part of cell:
		 */

		if ((p1 -> expr) && (p1 -> flags & is_strexpr))
		{
		    /*(void) addstr ((p1 -> flags & is_leftflush) ? "<{" : ">{");
		    (void) addstr (line);
		    (void) addstr ("} ");	/* and this '}' is for vi % */
		    strcat(stringbuf,((p1 -> flags & is_leftflush) ? "<{" : ">{"));
		    strcat(stringbuf, (line));
		    strcat(stringbuf, ("} "));	/* and this '}' is for vi % */
		    printed = 1;
		}
		else if (p1 -> label)		/* has constant label only */
		{
		    /*(void) addstr ((p1 -> flags & is_leftflush) ? "<\"" : ">\"");
		    (void) addstr (p1 -> label);
		    (void) addstr ("\" ");*/
		    strcat(stringbuf,((p1 -> flags & is_leftflush) ? "<\"" : ">\""));
		    strcat(stringbuf, (p1 -> label));
		    strcat(stringbuf, ("\" "));
		    printed = 1;
		}

		/*
		 * Display value part of cell:
		 */

		if (p1 -> flags & is_valid)	/* has value or num expr */
		{
		    if ((! (p1 -> expr)) || (p1 -> flags & is_strexpr))
			(void) sprintf (line, "%.15g", p1 -> v);

		    /*(void) addstr ("[");
		    (void) addstr (line);
		    (void) addstr ("]"); */
		    strcat(stringbuf,("["));
		    strcat(stringbuf, (line));
		    strcat(stringbuf, ("]"));
		    printed = 1;
		}
	    }

	    if (! printed)
		/*(void) addstr ("[]");*/
		strcat(stringbuf, "[]");
	}
	/*(void) move (lastmy, lastmx + fwidth[lastcol]);*/
    }
    XDrawImageString(dpy,mainwin,maingc,
		     textcol(0), textrow(0),
		     stringbuf, strlen(stringbuf));
    if (revmsg[0]) {
	/*(void) move(0, 0);
	(void) printw(revmsg); */
	clearlines(0,0);
	XDrawImageString(dpy,mainwin,maingc,
			 textcol(0), textrow(0),
			 revmsg, strlen(revmsg));
	revmsg[0] = 0;		/* don't show it again */
	/*(void) move (lastmy, lastmx + fwidth[lastcol]); */
    }
    FullUpdate = 0;
}

/* the following is no longer needed, is kept around for now for 
 * historical interest only (B. Backman 7-27-91)
 */
/*repaint(x, y, len)
* int x, y, len;
* {
*    char *buf;
*
*     buf = " ";
* 
*    while(len-- > 0) {
*	(void) move(y,x);
* #ifndef INTERNATIONAL
* 	*buf = inch() & 0x7f;
*#else
*	*buf = inch() & 0xff;
*#endif /* INTERNATIONAL */
/*	(void) addstr(buf);
*	x++;
*    }
*}
*/

char    *progname;

main (argc, argv)
int argc;
char  **argv;
{
    unsigned int lin;

    /* The following is the XSpread event structure and input buffer for key events */
    XEvent event;
    KeySym  key;
    char buffer[8];
    int count;
    int done;
    int complete;
    int pixel; 
    int     inloop = 1;
    register int   c;
    int     edistate = -1;
    int     narg;
    int     nedistate;
    char    *revi;
    int Mopt = 0;
    int Nopt = 1;  /* Start with the default of automatic data entry */
    int Copt = 0; 
    int Ropt = 0;
    
    sc_Xinit(argc,argv);

    /*
     * Keep command line options around until the file is read so the
     * command line overrides file options
     */
    
    
    progname = argv[0];
    while (argc > 1 && argv[1][0] == '-') {
      argv++;
      argc--;
      switch (argv[0][1]) {
      case 'x':
#ifdef VMS
	(void) fprintf(stderr, "Crypt not available for VMS\n");
exit(1);
#else 
		    Crypt = 1;
#endif
		    break;
	    case 'm':
		    Mopt = 1;
		    break;
	    case 'n':
		    Nopt = 0;
		    break;
	    case 'c':
		    Copt = 1;
		    break;
	    case 'r':
		    Ropt = 1;
		    break;
	    default:
		    (void) fprintf(stderr,"%s: unrecognized option: \"%c\"\n",
			progname,argv[0][1]);
		    exit(1);
	}
    }

    {
	register    i;
	for (i = 0; i < MAXCOLS; i++) {
	    fwidth[i] = DEFWIDTH;
	    precision[i] = DEFPREC;
	}
    }

    graphic_init();  /* initialize graphing parameters */

    curfile[0]=0;

    signals();


    /*
     * Build revision message for later use:
     */

    (void) strcpy (revmsg, progname);
    for (revi = rev; (*revi++) != ':'; );	/* copy after colon */
    (void) strcat (revmsg, revi);
    revmsg [strlen (revmsg) - 2] = 0;		/* erase last character */
    (void) strcat (revmsg, ":  Type '?' for help.");

    if (argc > 1) {
	(void) strcpy(curfile,argv[1]);
	readfile (argv[1], 0);
    }

    if (Mopt)
	autocalc = 0;
    if (Nopt)
	numeric = 1;
    if (Copt)
	calc_order = BYCOLS;
    if (Ropt)
	calc_order = BYROWS;

    modflg = 0;
#ifdef VENIX
    setbuf (stdin, NULL);
#endif
    FullUpdate++;
    while (inloop) { 
    running = 1;
    while (running) {
	nedistate = -1;
	narg = 1;
	if (edistate < 0 && linelim < 0 && autocalc && (changed || FullUpdate))
	    EvalAll (), changed = 0;
	update();

	/*(void) move (1, 0);
	(void) clrtoeol ();
	(void) fflush (stdout); */
	if (!seenerr) clearlines(1,1);
	seenerr = 0;
	showneed = 0;	/* reset after each update */
	showexpr = 0;

/* Initial Input */

        XNextEvent(dpy,&event);
	
	while (event.type == MotionNotify)
           XNextEvent(dpy,&event);

	if (event.xbutton.button == 3)
	   Main_Menu();
	else {
        if (event.type == ButtonPress)
        {
   	         if (linelim != -1)
 		 {
           	   showrange = 0;
		   if (linelim < 0)
		     line[linelim = 0] = 0;
		   else {
	   	     linelim = 0;
		     (void) yyparse ();
		     linelim = -1;
	           }
	         }
          pixel=31; 
          complete=0;
          curcol=stcol-1; 
          if ((stcol > 0) && (event.xbutton.x < 31)) 
          {
            curcol=stcol-1;
          }
          else
          { 
          while ((!complete) && (curcol <= MAXCOLS)) 
          {
            curcol++;
            if (!col_hidden[curcol])
            { 
              pixel = pixel + fwidth[curcol]*6;
              if (event.xbutton.x < pixel) complete++;
            }
          }
	  }
          pixel=55; 
          complete=0; 
          currow=strow-1; 
 	  if ((strow > 0) && (event.xbutton.y < 55))
          {
	    currow=strow-1;
 	  }
	  else
	  {
          while ((!complete) && (currow <= MAXROWS))
          {
            currow++; 
            if (!row_hidden[currow]) 
	    {
              pixel = pixel + 13;
              if (event.xbutton.y < pixel) complete++;
	    }
          }   
    	  } 
	  update();
        } } /* end of else */
        if (event.type == KeyPress)
	{  
          if((XLookupString ((XKeyEvent *)&event,buffer,8,&key,0)) || IsCursorKey(key))
	  {
	     if(!IsCursorKey(key))
	     {
	        c = buffer[0];
		if ((c < ' ') || ( c == DEL ))
		    switch (c) {
	/* #ifdef SIGTSTP
			case ctl ('z'):
			    deraw();
			    (void) kill(getpid(),SIGTSTP);

			    /* the pc stops here */
			    /*
			    goraw();
			    break;
	#endif */
			case ctl ('r'):
			case ctl ('l'):
			    FullUpdate++;
			    if (c == ctl ('r'))
				showneed = 1;
			    /*(void) clearok(stdscr,1); */
			    break;
			case ctl ('x'):
			    FullUpdate++;
			    showexpr = 1;
			    /*(void) clearok(stdscr,1);*/
			    break;
			default:
			    sprintf(stringbuf,"No such command (^%c)", c + 0100); 
			    error(stringbuf);
			    break;
			case ctl ('b'):
			    backcol(arg);
			    break;
			case ctl ('c'):
			    running = 0;
			    break;

			case ctl ('e'):{
			    int done  = 0;
			    while (!done)
			    {
				    XPeekEvent(dpy,&event);
				    if (event.type == KeyPress)
				    { if(XLookupString((XKeyEvent *)&event,buffer,8,0,0));
				       {
					       c = buffer[0];
					       switch (c) 
					       {
						 case ctl ('p'): case 'k':
						 doend (-1, 0);
						 break;
						 case ctl ('n'): case 'j':
						 doend ( 1, 0);
						 break;
						 case ctl ('b'): case 'h':
						 case ctl ('h'): doend ( 0,-1);
						 break;
						 case ctl ('f'): case 'l':
						 case ctl ('i'): case ' ':
						 doend ( 0, 1);
						 break;

						 case ESC:
						 case ctl ('g'):
						 break;

						 default:
						   error("Invalid ^E command"); 
						 break;
					       }
					       done = 1;
			               }
				    }
			    }
			    }
			    break;

			case ctl ('f'):
			    forwcol(arg);
			    break;
			case ctl ('g'):
			case ESC:	/* ctl ([) */
			    showrange = 0;
			    linelim = -1;
			    /*(void) move (1, 0);
			    (void) clrtoeol ();*/
			    clearlines(1,1);
			    break;
			case DEL:
			case ctl ('h'):
			    if (linelim <= 0) {	/* not editing line */
				backcol(arg);	/* treat like ^B    */
				break;
			    }
			    while (--arg>=0) if (linelim > 0)
				line[--linelim] = 0;
			    break;
			case ctl ('i'):		/* tab */
			    if (linelim <= 0) {	/* not editing line */
				forwcol(arg);
				break;
			    }

			    if (!showrange) {
				startshow();
			    } else {
				showdr();
				linelim = strlen(line);
				line[linelim++] = ' ';
				line[linelim] = 0;
				showrange = 0;
			    }
			    linelim = strlen (line);
			    break;
			case ctl ('m'):
			case ctl ('j'):
           		    showrange = 0;
			    if (linelim < 0)
				line[linelim = 0] = 0;
			    else {
				linelim = 0;
				(void) yyparse ();
				linelim = -1;
			    }
			    break;
			case ctl ('n'):
			    forwrow(arg);
			    break;
			case ctl ('p'):
			    backrow(arg);
			    break;
			case ctl ('q'):
			    break;	/* ignore flow control */
			case ctl ('s'):
			    break;	/* ignore flow control */
			case ctl ('u'):
			    narg = arg * 4;
			    nedistate = 1;
			    break;
			case ctl ('v'):	/* insert variable name */
			    if (linelim > 0) {
			    (void) sprintf (line+linelim,"%s", v_name(currow, curcol));
				linelim = strlen (line);
			    }
			    break;
			case ctl ('w'):	/* insert variable expression */
			    if (linelim > 0) editexp(currow,curcol);
			    break;
			case ctl ('a'):	/* insert variable value */
			    if (linelim > 0) {
				struct ent *p = tbl[currow][curcol];

				if (p && p -> flags & is_valid) {
				    (void) sprintf (line + linelim, "%.*f",
						precision[curcol],p -> v);
				    linelim = strlen (line);
				}
			    }
			    break;
			}
		else
		    if ('0' <= c && c <= '9' && ( (numeric && edistate >= 0) ||
				(!numeric && (linelim < 0 || edistate >= 0))))
		    {
			if (edistate != 0) {
			    if (c == '0')      /* just a '0' goes to left col */
				curcol = 0;
			    else {
				nedistate = 0;
				narg = c - '0';
			    }
			} else {
			    nedistate = 0;
			    narg = arg * 10 + (c - '0');
			}
		    }
		    else
			if (linelim >= 0) {     /* Editing line */
			    switch(c) {
				case ')':
				    if (showrange) {
					showdr();
					showrange = 0;
					linelim = strlen (line);
				    }
				    break;
			       default:
				    break;
			    }
			    line[linelim++] = c;
			    line[linelim] = 0;
			}
			else if (!numeric && ( c == '+' || c == '-' ) )	
					/* increment/decrement ops */
				{
				    register struct ent *p = tbl[currow][curcol];
				    if (!p)
					break;
				    FullUpdate++;
				    modflg++;
				    if( c == '+' ) p -> v += (double) arg;
				    else p -> v -= (double) arg;
				    }
			else
			    switch (c) {
				case ':':
				    break;	/* Be nice to vi users */

				case '@':
				    (void) sprintf(line,"let %s = @",
							v_name(currow, curcol));
				    linelim = strlen (line);
				    break;

				case '0': case '1': case '2': case '3': case '4':
				case '5': case '6': case '7': case '8': case '9':
				case '-': case '.': case '+':
				    (void) sprintf(line,"let %s = %c",
						v_name(currow, curcol), c);
				    linelim = strlen (line);
				    break;

				case '=':
				    (void) sprintf(line,"let %s = ",
							v_name(currow, curcol));
				    linelim = strlen (line);
				    break;

				 /*
				 * Range commands:
				 */

				case '/':
				   Main_Menu();
				   break;

				case '$':
				    {
				    register struct ent *p;

				    curcol = MAXCOLS - 1;
				    while (!VALID_CELL(p, currow, curcol) && curcol > 0)
					curcol--;
				    break;
				    }
				case '#':
				    {
				    register struct ent *p;

				    currow = MAXROWS - 1;
				    while (!VALID_CELL(p, currow, curcol) && currow > 0)
					currow--;
				    break;
				    }
				case 'w':
				    {
				    register struct ent *p;

				    while (--arg>=0) {
					do {
					    if (curcol < MAXCOLS - 1)
						curcol++;
					    else {
						if (currow < MAXROWS - 1) {
						    while(++currow < MAXROWS - 1 &&
							    row_hidden[currow]) /* */;
						    curcol = 0;
						} else {
						    error("At end of table");
						    break;
						}
					    }
					} while(col_hidden[curcol] ||
						!VALID_CELL(p, currow, curcol));
				    }
				    break;
				    }
				case 'b':
				    {
				    register struct ent *p;

				    while (--arg>=0) {
					do {
					    if (curcol) 
						curcol--;
					    else {
						if (currow) {
						    while(--currow &&
							row_hidden[currow]) /* */;
						    curcol = MAXCOLS - 1;
						} else {
						    error ("At start of table");
						    break;
						}
					    }
					} while(col_hidden[curcol] ||
						!VALID_CELL(p, currow, curcol));
				    }
				    break;
				    }
				case '^':
				    currow = 0;
				    break;
				case '?':
				    help ();
				    break;
				case '"':
				    (void) sprintf (line, "label %s = \"",
							v_name(currow, curcol));
				    linelim = strlen (line);
				    break;
				case '<':
				    (void) sprintf (line, "leftstring %s = \"",
					    v_name(currow, curcol));
				    linelim = strlen (line);
				    break;
				case '>':
				    (void) sprintf (line, "rightstring %s = \"",
					   v_name(currow, curcol));
				    linelim = strlen (line);
				    break;
				case 'e':
				    editv (currow, curcol);
				    break;
				case 'E':
				    edits (currow, curcol);
				    break;
				case 'f':
				    if (arg == 1)
					(void) sprintf (line, "format [for column] %s ",
						coltoa(curcol));
				    else {
					(void) sprintf(line, "format [for columns] %s:",
						coltoa(curcol));
					(void) sprintf(line+strlen(line), "%s ",
						coltoa(curcol+arg-1));
				    }
				    sprintf(stringbuf,"Current format is %d %d",
						fwidth[curcol],precision[curcol]);
				    error(stringbuf);
				    linelim = strlen (line);
				    break;
				case 'g':
				    (void) sprintf (line, "goto [v] ");
				    linelim = strlen (line);
				    break;
				case 'x':
				    {
				    register struct ent **p;
				    register int c1;

				    flush_saved();
				    if(calc_order == BYROWS) {
				    for (c1 = curcol; arg-- && c1 < MAXCOLS; c1++) {
					p = &tbl[currow][c1];
					if (*p) {
					    free_ent(*p);
					    *p = 0;
					}
				    }
				    }
				    else {
				    for (c1 = currow; arg-- && c1 < MAXROWS; c1++) {
					p = &tbl[c1][curcol];
					if (*p) {
					    free_ent(*p);
					    *p = 0;
					}
				    }
				    }
				    sync_refs();
				    modflg++;
				    FullUpdate++;
				    }
				    break;
				case 'h':
				    backcol(arg);
				    break;
				case 'j':
				    forwrow(arg);
				    break;
				case 'k':
				    backrow(arg);
				    break;
				case ' ':
				case 'l':
				    forwcol(arg);
				    break;
				case 'm':
				    savedrow = currow;
				    savedcol = curcol;
				    break;
				case 'c': {
				    register struct ent *p = tbl[savedrow][savedcol];
				    register c1;
				    register struct ent *n;
				    if (!p)
					break;
				    FullUpdate++;
				    modflg++;
				    for (c1 = curcol; arg-- && c1 < MAXCOLS; c1++) {
					n = lookat (currow, c1);
					(void) clearent(n);
					copyent( n, p, currow - savedrow, c1 - savedcol);
				    }
				    break;
				}
				default:
				    if ((c & 0177) != c){	/* doesn't this depend on INTERNATIONAL */
					/*error ("Weird character, decimal %d\n",
						(int) c); */
					sprintf(stringbuf,"Weird character, decimal %d\n",
						(int)c);
					error(stringbuf);
				    }else
					sprintf(stringbuf,"No such command (%c)", c);
					error(stringbuf);
				    break;
		    }
	  
	  }
	  else 
          {
   	         if (linelim != -1)
 		 {
           	   showrange = 0;
		   if (linelim < 0)
		     line[linelim = 0] = 0;
		   else {
	   	     linelim = 0;
		     (void) yyparse ();
		     linelim = -1;
	           }
	         }
            switch (key) {
		 case XK_Up :
		    backrow(arg);
		    break;
		 case XK_Down:
		    forwrow(arg);
		    break;
		 case XK_Left:
		    backcol(arg);
		    break;
		 case XK_Right:
		    forwcol(arg);
		    break;
             }
           }
	edistate = nedistate;
	arg = narg;
	update();  
        } /* if (XLookupString(....)) */
        } else
        switch(event.type)
        {
	       case Expose:
		 if (event.xexpose.count != 0) break;
		 FullUpdate++;
		 update();
		 break;

	       case MappingNotify:
		 XRefreshKeyboardMapping ((XMappingEvent *)&event);
		 break;

	       case ConfigureNotify:
		 sc_handleresize(&event);
		 break;
	}
    }	/* while (running) */
    inloop = modcheck(" before exiting");
    }	/*  while (inloop) */
    fflush(stdin);  /* clean up the buffer */
#ifdef VMS	/* Unit VMS "fixes" exit we should say 1 here */
    exit(1);
#else
    exit(0);
#endif
    /*NOTREACHED*/
}

startshow()
{
    showrange = 1;
    showsr = currow;
    showsc = curcol;
}

showdr()
{
    int     minsr, minsc, maxsr, maxsc;

    minsr = showsr < currow ? showsr : currow;
    minsc = showsc < curcol ? showsc : curcol;
    maxsr = showsr > currow ? showsr : currow;
    maxsc = showsc > curcol ? showsc : curcol;
    (void) sprintf (line+linelim,"%s", r_name(minsr, minsc, maxsr, maxsc));
}

setorder(i)
int i;
{
	if((i == BYROWS)||(i == BYCOLS))
	    calc_order = i;
	else
	    error("Not yet implemented");
}

setauto(i)
int i;
{
	autocalc = i;
}


signals()
{
#ifdef SIGVOID
    void quit();
    void time_out();
#else
    int quit();
    int time_out();
#endif

    (void) signal(SIGINT, SIG_IGN);
    (void) signal(SIGQUIT, quit);
    (void) signal(SIGPIPE, quit);
    (void) signal(SIGTERM, quit);
    (void) signal(SIGALRM, time_out);
    (void) signal(SIGFPE, quit);
    (void) signal(SIGBUS, quit);
}

#ifdef SIGVOID
void
#endif
quit()
{
    fflush(stdin);  /* clean up the buffer */
    exit(1);
}

/* modcheck prompts the user with a "Do you really want to exit?" message,
 * returning 0 if the user wants to quit (don't keep running), or 1 if 
 * the program should keep running 
 */
modcheck(endstr)
char *endstr;
{
    XEvent event;
    char ch, buffer[8];
    int done = 0;
    if (modflg && curfile[0]) {
	char lin[100];
/* I'd rather use line 1 than line 0, because that is where the user will 
 * be looking most often.  (B.Backman) 
 */
	clearlines(1,1);
	(void) sprintf (lin,"File \"%s\" is modified, save%s? ",curfile,endstr);
	XDrawImageString(dpy,mainwin,maingc,
			 textcol(0), textrow(1),
			 lin, strlen(lin));

	while (!done)
	{
		XPeekEvent(dpy,&event);

		while (event.type == MotionNotify)
		   XNextEvent(dpy,&event);
		if (event.type == KeyPress)
		{  
		   XNextEvent(dpy,&event);
		   if(XLookupString((XKeyEvent *)&event,buffer,8,0,0))
		   {
			   ch = buffer[0];
			   if (ch != 'y' && ch != 'Y' && ch != 'n' && ch != 'N')
			    {  
			      error("y or n response required");
			      return (1);
			    }
			   if (ch != 'n' && ch != 'N') 
			    {
			    if (writefile(curfile, 0, 0, maxrow, maxcol) < 0)
				return (1);
			    } 
			   else if (ch == ctl ('g') || ch == ESC) return(1);
			   done = 1;
                   }
		}
		else 
		{
		   error("Y or N keypress is required");
		   return(1);
		}
	}

    } else if (modflg) {
        char  lin[100];

	clearlines(1,1);
	(void) sprintf (lin,"Do you want a chance to save the data? ");
	XDrawImageString(dpy, mainwin, maingc,
			 textcol(0), textrow(1),
			 lin, strlen(lin));
	while (1)
	{
		XPeekEvent(dpy,&event);

		while (event.type == MotionNotify)
		   XNextEvent(dpy,&event);
		if (event.type == KeyPress)
		{
		   XNextEvent(dpy,&event);
		   if(XLookupString((XKeyEvent *)&event,buffer,8,0,0))
		   {
			   ch = buffer[0];
			   if (ch != 'y' && ch != 'Y' && ch != 'n' && ch != 'N')
			     {  
			       error("y or n response required");
			       return (1);
			     }
			   if (ch == 'n' || ch == 'N') return(0);
			   else 
/* 11-24-91 M. Frey
   Modified Quit so it saves file before it exits if user forgot to save file */
			     {
			     clearlines(1,1);
                             sprintf (line, "put [\"dest\" range] \"");
	                     XDrawImageString(dpy,mainwin,maingc,
			                      textcol(0), textrow(1),
			                      line, strlen(line));
                             if (*curfile){
                               sprintf(stringbuf,"Default path is \"%s\"",
				       curfile);
	                       error(stringbuf);
	                     }
                             linelim = strlen (line);
			     return(1);
			     }
		   }
		}
		else 
		{
		   error("Y or N keypress is required");
		   return(1);
		}
	}
      }
      return(0);
}  /*  end of modcheck */


/*========================================================================================
 *                                                                                       * 
 *        This following funtions were writen to implement the cursor style menu         * 
 *                                                                                       * 
 *                                 by SWLin, 8/90                                        *
 *                                                                                       *
 *========================================================================================
 */

unsigned int menu(item_no, item, help_prompt)
   unsigned int item_no;
   char         *item[], *help_prompt[]; 


{  int  i, col, choice, done, TempX_Loc;
   char c;
   int show_screen,cursor_temp,choice_temp;
   XEvent event;
   KeySym key;
   char buffer[8];

   choice = 1;
   done = 0;
   show_screen = 1;

   clearlines(1,1);

   while (!done) {
      TempX_Loc = event.xbutton.x;
      if (show_screen)
	{
	      for (i=0, col=0; i < item_no; i++) {
		 XDrawImageString(dpy, mainwin,
				  (i== choice-1) ? maingcreversed : maingc,
				  textcol(col), textrow(1),
				  item[i], strlen(item[i]));
		 col = col + strlen(item[i]) + 2;
	      }
	      clearlines(2,2);
	      sprintf(stringbuf,"%s",help_prompt[choice - 1]); 
	      XDrawImageString(dpy,mainwin,maingc,
			       textcol(0), textrow(2),
			       stringbuf, strlen(stringbuf));
	      show_screen = 0;
	}

      /*** HACK COURTESY OF MIKE ***/

      XNextEvent(dpy,&event);

      switch(event.type)
      {
      case MotionNotify:
	if (event.xbutton.y >= 10 && event.xbutton.y <= 28 )
	{
	      choice_temp = 1;
	      cursor_temp = ((strlen(item[0])+2)*6);
	      while (cursor_temp < event.xbutton.x) {
		    if (choice_temp < item_no) {
	                 cursor_temp += ((strlen(item[choice_temp++])+2)*6);
		    } else {
			break;
		    }
	      }
	      if (choice_temp <= item_no)
	      {
	      if (choice_temp != choice)
	         {choice = choice_temp;
	          show_screen = 1;
		 } /* end of if choice_temp */
	      } /* only if choice_temp is within bounds */
	}
	break;

       case ButtonPress:
	if ((event.xbutton.y > 28))
	   {
	   choice=0;
	   done=1;
	   }
	else /* Let the mouse select the exact choice */
	   show_screen = done = 1;
	break;
	 
       case Expose:
	 FullUpdate++;
	 update();
	 show_screen = 1;
	 break;

       case MappingNotify:
	 XRefreshKeyboardMapping ((XMappingEvent *)&event);
	 break;

       case ConfigureNotify:
	 sc_handleresize(&event);
	 break;

       /***** NEEDS TO BE FIXED *****/

       case KeyPress:
         if((XLookupString ((XKeyEvent *)&event,buffer,8,&key,0)) || (IsCursorKey(key)))
	 { 
	   show_screen = 1;
	   if(!IsCursorKey(key))
	     {
	      c = buffer[0];

      /* c = nmgetch(); */

	      switch (c) {
		 case ctl ('f'):
		 case 'l': 
		 case 'L': 
		    choice++;
		    if (choice == item_no + 1) choice = 1;
		    break; 
		 case ctl ('b'):
		 case ' ':
		 case 'h':
		 case 'H':
		    choice--;
		    if (choice == 0) choice = item_no;
		    break;
		 case ctl ('p'):
		 case 'k':
		 case 'K':
		    choice = 1;
		    break;
		 case ctl ('n'):
		 case 'j':
		 case 'J':
		    choice = item_no;
		    break;
		 case 27:
		 case ctl ('g'):
		    choice = 0;
		    done = 1;
		    break;
		 case 13:
		 case 10:
		    done = 1;
		    break;
		 default:
		    for (i=0; i < item_no; i++) {
		       if (c == *item[i] ||
			   c > 91 && c == *item[i] + 32) { 
			  choice = i+1;
			  done = 1;
			  break;
		       }
		    }
		    if (!done) { 
		       fprintf(stderr, "\7");
		    }     
		    break;
	      } /* switch(c) */          
           }  /* if !IsCursorKey(key) */
	   else switch(key) {
		 case XK_Up :
		    choice = 1;
		    break;
		 case XK_Down:
		    choice = item_no;
		    break;
		 case XK_Left:
		    choice--;
		    if (choice == 0) choice = item_no;
		    break;
		 case XK_Right:
		    choice++;
		    if (choice == item_no + 1) choice = 1;
		    break; 
		}
	      break;
	      } /* if XLookupString(..) */
      } /* switch (event.type) */
   } /* while (!done) */             

   /*(void) move(1,0);
   (void) clrtoeol();
   (void) move(2,0);
   (void) clrtoeol(); */
   clearlines(1,2);
   return choice;
}


/*
 *=========================================================================================
 */


void Range_Menu()
{
static char *item[] = { 
		               "Erase",
		               "Value", 
		               "Copy", 
		               "Fill",
		               "Define",
		               "Undefine",
		               "Show"
		      };

static char *help_prompt[] = {
                               "Clear a range", 
			       "Remove the expressions from a range, leaving just the values",
			       "Copy a source range to a destination range",
			       "Fill a range with constant values",
			       "Assign a name to a cell or a range of cells",
			       "Use this command to undefine a previously defined range name",
			       "Shows the currently defined range names"
                             };

   switch (menu(7, item, help_prompt)) {
      case 0:
	 Main_Menu();
         break;
      case 1:
         (void) sprintf(line,"erase [range] ");
         linelim = strlen(line);
         startshow();
         break;
      case 2:
         (void) sprintf(line, "value [range] ");
         linelim = strlen(line);
         startshow();
         break;
      case 3:
         (void) sprintf(line,"copy [dest_range src_range] ");
         linelim = strlen(line);
         startshow();
         break;
      case 4:
         (void) sprintf(line,"fill [range start inc] ");
         linelim = strlen(line);
         startshow();
         break;
      case 5:
         (void) sprintf(line,"define [string range] \"");
         linelim = strlen(line);
         startshow();
         modflg++;
         break;
      case 6:
         (void) sprintf(line,"undefine [range] ");
         linelim = strlen(line);
         modflg++;
         break;
      case 7:
         if(are_ranges()) {
         FILE *f;
         int pid;
         char px[MAXCMD] ;
         char *pager;
      
         (void) strcpy(px, "| sort | ");
         if(!(pager = getenv("PAGER")))
         pager = DFLT_PAGER;
         (void) strcat(px,pager);
         f = openout(px, &pid);
         if (!f) {
            error("Can't open pipe to sort");
            break;
         }
         list_range(f);
         closeout(f, pid);
         }
         else error("No ranges defined");
         break;
      default:
         error("Invalid region command");
         break;
    }
}

/*
 *=========================================================================================
 */

void Row_Col_Menu()
{
static char *item[] = { 
			    "Insert", 
			    "Append",
			    "Delete", 
			    "Pull", 
			    "Remove", 
			    "Hide", 
			    "Show",
			    "Format"
                      };

static  char *help_prompt[] = {
			    "Insert a new, empty row (column)",
                            "Append a new copy of the current row (column)",
			    "Delete the current row (column)",
			    "Pull deleted cells back into the spreadsheet",
			    "Remove expressions from the affected rows (columns)",
			    "Hide (``zap'') the current row (column)",
			    "Show hidden rows (columns)",
			    "Set the output format"
                           };

static char *item_1[] = {"Row", "Column"};

static char *help_promt_1[] = {"Make change to rows", "Make change to columns"};

switch (menu(8, item, help_prompt)) {

   case 0: 
      Main_Menu();
      break;

   case 1: /* Insert */
       switch(menu(2, item_1, help_promt_1) ) {
        case 0:  Row_Col_Menu();  break;
	case 1:  insertrow (arg); break;
	case 2:  insertcol (arg); break;
      }
      break;

   case 2: /* Append */
      switch(menu(2, item_1, help_promt_1)) {
        case 0:  Row_Col_Menu();  break;
        case 1: while (arg--) duprow(); break;
        case 2: while (arg--) dupcol(); break;
     }
     break;

   case 3:
       switch(menu(2, item_1, help_promt_1)) {
        case 0:  Row_Col_Menu();  break;
        case 1:  deleterow (arg); break;
        case 2:  deletecol (arg); break;
      }
      break;

   case 4:
      switch(menu(2, item_1, help_promt_1)) {
        case 0:  Row_Col_Menu();  break;
         case 1: while (arg--)	pullcells ("r"); break;
         case 2: while (arg--)	pullcells ("c"); break;
      }
      break;

   case 5:
      switch(menu(2, item_1, help_promt_1)){ 
        case 0:  Row_Col_Menu();  break;
         case 1: rowvalueize (arg); break;
         case 2: colvalueize (arg); break;
      }
      modflg = 1;
      break;

   case 6:
      switch (menu(2, item_1, help_promt_1)) {
         case 0:  Row_Col_Menu();  break;
         case 1: hiderow (arg); break;
         case 2: hidecol (arg); break;
      }
      modflg++;
      break;

   case 7:   /* special case; no repeat count */

       switch(menu(2, item_1, help_promt_1)) {
        case 0:  Row_Col_Menu();  break;
         case 1: rowshow_op(); break;
         case 2: colshow_op(); break;
      }
      break;
   case 8:   /* Format */
      if (arg == 1)
	(void) sprintf (line, "format [for column] %s ",
	coltoa(curcol));
      else {
	(void) sprintf(line, "format [for columns] %s:",
	coltoa(curcol));
	(void) sprintf(line+strlen(line), "%s ",
	coltoa(curcol+arg-1));
	    }
      sprintf(stringbuf,"Current format is %d %d",
		fwidth[curcol],precision[curcol]);
      clearlines(1,1);
      XDrawImageString(dpy,mainwin,maingc,
		       textcol(0), textrow(1),
   		       stringbuf, strlen(stringbuf));
      XFlush(dpy);
      seenerr = 1;
      linelim = strlen (line);
      break;
   }
}

		       

/*
 *=========================================================================================
 */

void Option_Menu()
{
static char *item[] = {
                             "Auto",
		             "Numeric",
		             "Top",
          	             "Cell",
		             "Encrypt",
		             "Pre-scale",
		             "Ext-funcs",
			     "Set"
	              };

static char *help_prompt[] = {
		             "Recalculate automatically or on ``@'' commands",
                             "Make a digit starts a numeric value",  
                             "Top line display enable/disable", 
                             "Current cell highlighting enable/disable",
                             "Encrypt/decrypt database and listing files",
			     "Numeric constants entered are multipled by 0.01",
                             "External function execution enable/disable",
			     "Set other options"
			    }; 

   switch (menu(8, item, help_prompt)) {
      case 0:
      Main_Menu();
      break; 
   case 1:
      autocalc ^= 1;
      /* error("Automatic recalculation %sabled.",
      autocalc ? "en":"dis"); */
      sprintf(stringbuf, "Automatic recalculation %sabled.",
      		autocalc ? "en":"dis"); 
      error(stringbuf);
      break;
   case 2:
      numeric = (! numeric);
      /* error ("Numeric input %sabled.",
      numeric ? "en" : "dis"); */
      sprintf(stringbuf,"Numeric input %sabled.",
      		numeric ? "en" : "dis");
      error(stringbuf);
      break;
   case 3:
      showtop = (! showtop);

      /*repaint(lastmx, lastmy, fwidth[lastcol]); */
      redraw_current(maingc);
      /*error ("Top line %sabled.", showtop ? "en" : "dis"); */
      sprintf(stringbuf,"Top line %sabled.", showtop ? "en" : "dis");
      error(stringbuf);
      break;
   case 4: 
      showcell = (! showcell);
      /*repaint(lastmx, lastmy, fwidth[lastcol]); */
      redraw_current(maingc);
      /*error ("Cell highlighting %sabled.",
      showcell ? "en" : "dis"); */
      sprintf(stringbuf,"Cell highlighting %sabled.",
      	      showcell ? "en" : "dis");
      error(stringbuf);
      break;
   case 5:
      Crypt = (! Crypt);
      /*error ("Encryption %sabled.", Crypt? "en" : "dis");*/
      sprintf(stringbuf,"Encryption %sabled.", Crypt? "en" : "dis");
      error(stringbuf);
      break;
   case 6:
      if (prescale == 1.0) {
      error ("Prescale enabled.");
      prescale = 0.01;
      } else {
      prescale = 1.0;
      error ("Prescale disabled.");
      }
      break;
   case 7: 
      extfunc = (! extfunc);
      /* error ("External functions %sabled.",
      extfunc? "en" : "dis"); */
      sprintf(stringbuf,"External functions %sabled.",
      	      extfunc? "en" : "dis");
      error(stringbuf);
      break;
   case 8:
      (void) sprintf (line, "set ");
      error("Options: byrows, bycols, iterations=n, tblstyle=(0|tbl|latex|tex)");
      linelim = strlen (line);
      break;
   }
   FullUpdate++;
   modflg++;
}


/*
 *=========================================================================================
 */

void File_Menu()

{
static char *item[] = {
		              "Get",
			      "Put",
			      "Write",
			      "Table",
			      "Merge",
			      "Combine",
			      "Directory"
                       };

static char *help_prompt[] = {
                              "Get a new database from a file",
			      "Put the current database into a file",
			      "Write the current database into a file in its screen format",
			      "Write the current database to a file in table format",
			      "Merge files",
			      "Combine macro files",
			      "Set directory"
			      };

   switch (menu(7, item, help_prompt)) {                   
      case 0:
	 Main_Menu();
         break;
      case 1:
         (void) sprintf (line, "get [\"source\"] \"");
         if (*curfile){
           sprintf(stringbuf,"Default file is \"%s\"",curfile);
	   error(stringbuf);
	 }
         linelim = strlen (line);
         break;
      case 2:
         (void) sprintf (line, "put [\"dest\" range] \"");
         if (*curfile){
           sprintf(stringbuf,"Default path is \"%s\"",curfile);
	   error(stringbuf);
	 }
         linelim = strlen (line);
         break;
      case 3:
         (void) sprintf (line, "write [\"dest\" range] \"");
         linelim = strlen (line);
         break;
      case 4:	/* tbl output */
         (void) sprintf (line, "tbl [\"dest\" range] \"");
         linelim = strlen (line);
         break;
      case 5:
         (void) sprintf (line, "merge [\"source\"] \"");
         linelim = strlen (line);
         break;
      case 6:
         (void) sprintf (line,"merge [\"macro_file\"] \"%s/", mdir);
         linelim = strlen (line);
         break;
      case 7:
         (void) sprintf (line, "mdir [\"macro_directory\"] \"");
         linelim = strlen (line);
         break;
      }
}


/*
 *=========================================================================================
 */
/****************************************************************************/
/*                                                                          */
/* The follwing function displays the Matrix_Menu                             */  
/*                                                                          */
/*                                           - Fang Wang     12/91          */
/****************************************************************************/

void Matrix_Menu()
{
static char *item[] = {
                               "Transpose",
                               "Addition",
                               "Subtraction",
                               "Multiplication",
                               "Invert"
                       };
static char *help_prompt[] = {
                                "Transpose the matrix",
                                "Add two matrices",
                                "Subtract the 2nd matrix from the 1st one",
                                "Multiply the 1st matrx by the 2nd one",
                                "Invert the matrix"
                              };

  switch (menu(5, item, help_prompt)) {
       case 0:
          Main_Menu();
          break;
       case 1:
          get_trans();
          break;
       case 2:
          get_add();
          break;
       case 3:
          get_sub();
          break;
       case 4:
          get_mult();
          break;
       case 5:
          get_invert();
          break;
       default:
         error("Invalid region command");
         break;
    }
}

/***************************************************************************/
/*                                                                         */
/* The following function calculate the transpose matrix of the input one  */
/*                                                                         */
/*                                            -Fang Wang   12/91           */
/***************************************************************************/

#define REG_LEN 28  /*length of input range string*/
get_trans()
{
  char s[100];
  struct m_range_sd *find_rge(),*rge_sd;
  struct ent *dv1,*dv2,*v1,*v2;
  int minsr, minsc;
  int maxsr, maxsc;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;

  sprintf(s, "transpose [dest_range src_range] ");
  get_str(s,REG_LEN);
  rge_sd=find_rge(s);
  /*check for input error*/
  if (rge_sd == NULL) {
      error ("Input syntax");
      return;
  }
  dv1=lookat(rge_sd->dsr,rge_sd->dsc);
  dv2=lookat(rge_sd->der,rge_sd->dec);
  v1=lookat(rge_sd->ssr,rge_sd->ssc);
  v2=lookat(rge_sd->ser,rge_sd->sec);
  mindr = dv1->row;
  mindc = dv1->col;
  maxdr = dv2->row;
  maxdc = dv2->col;
  if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
  if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
  maxsr = v2->row;
  maxsc = v2->col;
  minsr = v1->row;
  minsc = v1->col;
  if (minsr>maxsr) r = maxsr, maxsr = minsr, minsr = r;
  if (minsc>maxsc) c = maxsc, maxsc = minsc, minsc = c;
  if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     error ("The table can't be any bigger");
     return;
  }
  /*check for the matching of size of matrices*/
  if (((maxsr-minsr) != (maxdc-mindc)) || ((maxsc-minsc) != (maxdr-mindr)))
     {
       error("Destination matrix range, source matrix range don't match");
       return;
     }
  erase_area(mindr, mindc, maxdr, maxdc); /*clear the des. area*/
  if (minsr == maxsr && minsc == maxsc) {
        /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
               transpose(vr, vc, minsr, minsc, maxsr, maxsc);
    } else if (minsr == maxsr) {
        /* Source is a single row */
        for (vc = mindc; vc <= maxdc; vc++)
           transpose( mindr,vc, minsr, minsc, maxsr, maxsc);
    } else if (minsc == maxsc) {
        /* Source is a single column */
        for (vr = mindr; vr <= maxdr; vr++)
           transpose(vr, mindc, minsr, minsc, maxsr, maxsc);
    } else {
        /* Everything else */
        transpose(mindr, mindc, minsr, minsc, maxsr, maxsc);
    }
}

/***************************************************************************/
/*                                                                         */
/* The following function find the range upper left corner row,upper col   */
/* and  bottom right corner row and col.                                   */
/*                                                                         */
/*                                            -Fang Wang   12/91           */
/***************************************************************************/


#define ROWLIM 4 /*size of the array stores col label*/
#define COLIM 3 /*size of the array stores row label*/
struct m_range_sd *find_rge(s)
char *s;

{ char col[COLIM],row[ROWLIM];
  int i, j, mtemp[9];
  struct m_range_sd *rge_sd;

  for (i=0;i<=8;i++)
     mtemp[i] = 0;
  j = 0;
  while (*s != '\0') {
     i = 0;
     if ((*s == ':') || (*s == ' '))  s++;
     while (((*s <= 'Z') && (*s >= 'A')) || ((*s <= 'z') && (*s >= 'a')))
          col[i++] = *(s++);
     col[i] = '\0';
     if ((strlen(col) == 0 ) || (strlen(col) > (COLIM-1)))
         return(NULL);
     mtemp[j++] = convert(26,col,strlen(col));
     i = 0;
     while ((*s<='9') && (*s>='0'))
          row[i++] = *(s++);
     row[i] = '\0';
     if ((strlen(row) == 0 ) || (strlen(row) > (ROWLIM-1)))
         return(NULL);
     mtemp[j++] = convert(10,row,strlen(row));
  } /* end of while*/
  rge_sd = (struct m_range_sd *) xmalloc(sizeof(struct m_range_sd));
  rge_sd->dsc = mtemp[0];
  rge_sd->dsr = mtemp[1];
  rge_sd->dec = mtemp[2];
  rge_sd->der = mtemp[3];
  rge_sd->ssc = mtemp[4];
  rge_sd->ssr = mtemp[5];
  rge_sd->sec = mtemp[6];
  rge_sd->ser = mtemp[7];
  return(rge_sd);
}

/***************************************************************************
 *                                                                         *
 *    Convert the col, row into integer.                                   *
 *                                     -Fang Wang  12/91                   *
 **************************************************************************/

int convert( base, s, size)
int base,size;
char s[];

{ int val, i;
  int temp;

  temp = 0;
  val = 0;
  for (i=0; (i<size) && (s[i] != '\0'); i++) {
      if (((s[i]<='Z') && (s[i]>='A')) || ((s[i]<='z') && (s[i]>='a'))) {
          if (islower(s[i]))
               s[i] = toupper(s[i]);
      val = (temp*base) + ( s[i]-'A');
      temp = temp + 1;
      }
      else if ((s[i]<='9') && (s[i]>='0'))
          val = (val*base) + (s[i]-'0');
   }
   return (val);
}


/***************************************************************************
 *                                                                         *
 *    Calculates the transpose matrix.                                     *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 **************************************************************************/


transpose (vr, vc, minsr, minsc, maxsr, maxsc)
int vr, vc, minsr, minsc, maxsr, maxsc;
{
    register struct ent *p;
    register struct ent *n;
    register int sr, sc;
    register int dr, dc;

    for (dc=vc, sr=minsr; sr<=maxsr; sr++, dc++) 
       for (dr=vr, sc=minsc; sc<=maxsc; sc++, dr++) {
            n = lookat (dr, dc);
            (void) clearent(n);
            if (p = tbl[sr][sc])
                copyent( n, p, dr - sr, dc - sc);
        }
}


/***************************************************************************
 *                                                                         *
 *    Add two matrices.                                                    *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/


#define TSSIZE 15
get_add()
{
  char s[100];
  struct m_range *findrge();
  struct ent *dmin,*dmax,*s1min,*s1max,*s2min,*s2max;
  int mins1r, mins1c;
  int maxs1r, maxs1c;
  int mins2r, mins2c;
  int maxs2r, maxs2c;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;

  sprintf(s,"1st matrix range:");
  get_str(s,TSSIZE);
  rge_s1 = findrge(s);
  if (rge_s1 == NULL) {
     error("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"2nd matrix range:");
  get_str(s,TSSIZE);
  rge_s2 = findrge(s);
  if (rge_s1 == NULL) {
     error("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"result matrix range:");
  get_str(s,TSSIZE);
  rge_d = findrge(s);
  if (rge_s1 == NULL) {
     error("Input syntax error");
     return;
  }
  dmin=lookat(rge_d->sr,rge_d->sc);
  dmax=lookat(rge_d->er,rge_d->ec);
  s1min=lookat(rge_s1->sr,rge_s1->sc);
  s1max=lookat(rge_s1->er,rge_s1->ec);
  s2min=lookat(rge_s2->sr,rge_s2->sc);
  s2max=lookat(rge_s2->er,rge_s2->ec);
  mindr=dmin->row;
  mindc=dmin->col;
  maxdr=dmax->row;
  maxdc=dmax->col;
  if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
  if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
  maxs1r = s1max->row;
  maxs1c = s1max->col;
  mins1r = s1min->row;
  mins1c = s1min->col;
  maxs2r = s2max->row;
  maxs2c = s2max->col;
  mins2r = s2min->row;
  mins2c = s2min->col;
  if (mins1r>maxs1r) r = maxs1r, maxs1r = mins1r, mins1r = r;
  if (mins1c>maxs1c) c = maxs1c, maxs1c = mins1c, mins1c = c;
  if (mins2r>maxs2r) r = maxs2r, maxs2r = mins2r, mins2r = r;
  if (mins2c>maxs2c) c = maxs2c, maxs2c = mins2c, mins2c = c;
  if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     error ("The table can't be any bigger");
     return;
  }
  if (((maxs1r-mins1r) != (maxs2r-mins2r)) ||
                      ((maxs2c-mins2c) != (maxs1c-mins1c))) {
      error("Size of two matrices don't match,cann't be added");
      return;
     }
  if (((maxs1r-mins1r) != (maxdr-mindr)) ||
                      ((maxdc-mindc) != (maxs1c-mins1c))) {
      error("Size of source and destination matrices don't match");
      return;
     }
  erase_area(mindr, mindc, maxdr, maxdc);
  if (mins1r == maxs1r && mins1c == maxs1c && mins2r == maxs2r &&
        mins2c == maxs2c )  {
        /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
               addmatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else if (mins1r == maxs1r && mins2r == maxs2r) {
        /* Source is a single row */
        for (vr = mindr; vr <= maxdr; vr++)
               addmatrix(vr,mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else if (mins1c == maxs1c && mins2c == maxs2c ) {
        /* Source is a single column */
        for (vc = mindc; vc <= maxdc; vc++)
           addmatrix(mindr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else {
        /* Everything else */
        addmatrix(mindr, mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    }
}

/***************************************************************************
 *                                                                         *
 *    Get range for when input one by one.                                 *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/


#define ROWLIM 4
#define COLIM 3

struct m_range *findrge(s)
char *s;

{ char col[COLIM],row[ROWLIM];
  int i, j, mtemp[5];
  struct m_range *rge;

  for (i=0;i<=4;i++)
     mtemp[i] = 0;
  j = 0;
  while (*s != '\0') {
     i = 0;
     if (*s == ':')  s++;
     while (((*s <= 'Z') && (*s >= 'A')) || ((*s <= 'z') && (*s >= 'a')))
          col[i++] = *(s++);
     col[i] = '\0';
     if ((strlen(col) == 0 ) || (strlen(col) > (COLIM-1)))
          return(NULL);
     mtemp[j++] = convert(26,col,strlen(col));
     i = 0;
     while ((*s<='9') && (*s>='0'))
          row[i++] = *(s++);
     row[i] = '\0';
     if ((strlen(row) == 0 ) || (strlen(row) > (ROWLIM-1)))
         return(NULL);
     mtemp[j++] = convert(10,row,strlen(row)); 
  } /* end of while*/
  rge = (struct m_range *) xmalloc(sizeof(struct m_range));
  rge->sc = mtemp[0];
  rge->sr = mtemp[1];
  rge->ec = mtemp[2];
  rge->er = mtemp[3];
  return(rge);
} 


/***************************************************************************
 *                                                                         *
 *    Calculates addition of two matrices.                                 *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

#define MAXSS 2000
addmatrix (vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c)
int vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c;
{
    register struct ent *p1,*p2;
    register struct ent *n;
    register int s1r, s1c, s2r, s2c;
    register int dr, dc;
    int i,j;
    int list1[MAXSS],list2[MAXSS];


    for (i=0;i<=MAXSS;i++)
        {
            list1[i] = 0;
            list2[i] = 0;
        } 
     i = 0;
     for (s1r=mins1r;s1r<=maxs1r;s1r++)
        for (s1c=mins1c;s1c<=maxs1c;s1c++)
            if ((p1 = tbl[s1r][s1c]) && p1->flags&is_valid)
                   list1[i++] = p1->v;
     j = 0;
     for (s2r=mins2r;s2r<=maxs2r;s2r++)
        for (s2c=mins2c;s2c<=maxs2c;s2c++)
            if ((p2 = tbl[s2r][s2c]) && p2->flags&is_valid)
                  list2[j++] = p2->v;
     for (i=0;i<=MAXSS;i++)
          list1[i] = list1[i] + list2[i];
     i = 0;
     for (dr=vr, s1r=mins1r; s1r<=maxs1r; s1r++,dr++)
       for (dc=vc, s1c=mins1c; s1c<=maxs1c; s1c++, dc++) {
            n = lookat (dr, dc);
            (void) clearent(n);
            if ((p1 = tbl[s1r][s1c]) && (p1->flags&is_valid))
                {
                   copyent( n, p1, dr - s1r, dc - s1c);
                   n->v = list1[i++];
                 }
            }
}

/***************************************************************************
 *                                                                         *
 *    Subtraction of two matrices.                                         *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

#define TSSIZE 15
get_sub()
{
  char s[100];
  struct m_range *findrgt();
  struct ent *dmin,*dmax,*s1min,*s1max,*s2min,*s2max;
  int mins1r, mins1c;
  int maxs1r, maxs1c;
  int mins2r, mins2c;
  int maxs2r, maxs2c;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;

  sprintf(s,"1st matrix range:");
  get_str(s,TSSIZE);
  rge_s1 = findrge(s);
  if (rge_s1 == NULL ) {
     error ("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"2nd matrix range:");
  get_str(s,TSSIZE);
  rge_s2 = findrge(s);
  if (rge_s2 == NULL ) {
     error ("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"result matrix range:");
  get_str(s,TSSIZE);
  rge_d = findrge(s);
  if (rge_s2 == NULL ) {
     error ("Input syntax error");
     return;
  }
  dmin=lookat(rge_d->sr,rge_d->sc);
  dmax=lookat(rge_d->er,rge_d->ec);
  s1min=lookat(rge_s1->sr,rge_s1->sc);
  s1max=lookat(rge_s1->er,rge_s1->ec);
  s2min=lookat(rge_s2->sr,rge_s2->sc);
  s2max=lookat(rge_s2->er,rge_s2->ec);
  mindr=dmin->row;
  mindc=dmin->col;
  maxdr=dmax->row;
  maxdc=dmax->col;
  if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
  if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
  maxs1r = s1max->row;
  maxs1c = s1max->col;
  mins1r = s1min->row;
  mins1c = s1min->col;
  maxs2r = s2max->row;
  maxs2c = s2max->col;
  mins2r = s2min->row;
  mins2c = s2min->col;
  if (mins1r>maxs1r) r = maxs1r, maxs1r = mins1r, mins1r = r;
  if (mins1c>maxs1c) c = maxs1c, maxs1c = mins1c, mins1c = c;
  if (mins2r>maxs2r) r = maxs2r, maxs2r = mins2r, mins2r = r;
  if (mins2c>maxs2c) c = maxs2c, maxs2c = mins2c, mins2c = c;
  if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     error ("The table can't be any bigger");
     return;
  }
  if (((maxs1r-mins1r) != (maxs2r-mins2r)) ||
                      ((maxs2c-mins2c) != (maxs1c-mins1c))) {
      error("Size of two matrices don't match,cann't be subtructed");
      return;
     }
  if (((maxs1r-mins1r) != (maxdr-mindr)) ||
                      ((maxdc-mindc) != (maxs1c-mins1c))) {
      error("Size of source and destination matrices don't match");
      return;
     }
  erase_area(mindr, mindc, maxdr, maxdc);
    if (mins1r == maxs1r && mins1c == maxs1c && mins2r == maxs2r &&
        mins2c == maxs2c )  {
        /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
               submatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else if (mins1r == maxs1r && mins2r == maxs2r) {
        /* Source is a single row */
        for (vr = mindr; vr <= maxdr; vr++)
               submatrix(vr, mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else if (mins1c == maxs1c && mins2c == maxs2c ) {
        /* Source is a single column */
        for (vc = mindc; vc <= maxdc; vc++)
           submatrix(mindr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else {
        /* Everything else */
        submatrix(mindr, mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    }

}

/***************************************************************************
 *                                                                         *
 *    Calculates Subtraction of two matrices.                              *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/
#define MAXSS 2000
submatrix (vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c)
int vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c;
{
    register struct ent *p1,*p2;
    register struct ent *n;
    register int s1r, s1c, s2r, s2c;
    register int dr, dc;
    int i,j;
    int list1[MAXSS],list2[MAXSS];

    for (i=0;i<=MAXSS;i++)
        {
            list1[i] = 0;
            list2[i] = 0;
        }
    i = 0;
    for (s1r=mins1r;s1r<=maxs1r;s1r++)
        for (s1c=mins1c;s1c<=maxs1c;s1c++)
            if ((p1 = tbl[s1r][s1c]) && p1->flags&is_valid)
                   list1[i++] = p1->v;
    j = 0;
    for (s2r=mins2r;s2r<=maxs2r;s2r++)
        for (s2c=mins2c;s2c<=maxs2c;s2c++)
            if ((p2 = tbl[s2r][s2c]) && p2->flags&is_valid)
                  list2[j++] = p2->v;
    for (i=0;i<=MAXSS;i++)
          list1[i] = list1[i] - list2[i];
    i = 0;
    for (dr=vr, s1r=mins1r; s1r<=maxs1r; s1r++,dr++)
       for (dc=vc, s1c=mins1c; s1c<=maxs1c; s1c++, dc++) {
            n = lookat (dr, dc);
            (void) clearent(n);
            if ((p1 = tbl[s1r][s1c]) && (p1->flags&is_valid))
                {
                   copyent( n, p1, dr - s1r, dc - s1c);
                   n->v = list1[i++];
                 }
            }
}
/***************************************************************************
 *                                                                         *
 *    Multiplication of two matrices.                                      *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

#define TSSIZE 15
get_mult()
{
  char s[100];
  struct m_range *findrge();
  struct ent *dmin,*dmax,*s1min,*s1max,*s2min,*s2max;
  int mins1r, mins1c;
  int maxs1r, maxs1c;
  int mins2r, mins2c;
  int maxs2r, maxs2c;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;

  sprintf(s,"1st matrix range:");
  get_str(s,TSSIZE);
  rge_s1 = findrge(s);
  if (rge_s1 == NULL) {
     error("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"2nd matrix range:");
  get_str(s,TSSIZE);
  rge_s2 = findrge(s);
  if (rge_s2 == NULL) {
     error("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"result matrix range:");
  get_str(s,TSSIZE);
  rge_d = findrge(s);
  if (rge_d == NULL) {
     error("Input syntax error");
     return;
  }
  dmin=lookat(rge_d->sr,rge_d->sc);
  dmax=lookat(rge_d->er,rge_d->ec);
  s1min=lookat(rge_s1->sr,rge_s1->sc);
  s1max=lookat(rge_s1->er,rge_s1->ec);
  s2min=lookat(rge_s2->sr,rge_s2->sc);
  s2max=lookat(rge_s2->er,rge_s2->ec);
  mindr=dmin->row;
  mindc=dmin->col;
  maxdr=dmax->row;
  maxdc=dmax->col;
  if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
  if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
  maxs1r = s1max->row;
  maxs1c = s1max->col;
  mins1r = s1min->row;
  mins1c = s1min->col;
  maxs2r = s2max->row;
  maxs2c = s2max->col;
  mins2r = s2min->row;
  mins2c = s2min->col;
  if (mins1r>maxs1r) r = maxs1r, maxs1r = mins1r, mins1r = r;
  if (mins1c>maxs1c) c = maxs1c, maxs1c = mins1c, mins1c = c;
  if (mins2r>maxs2r) r = maxs2r, maxs2r = mins2r, mins2r = r;
  if (mins2c>maxs2c) c = maxs2c, maxs2c = mins2c, mins2c = c;
  if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     error ("The table can't be any bigger");
     return;
  }
  if ((maxs1c-mins1c) != (maxs2r-mins2r))
       {
           error ("The size of two matriies doesn't match,cann't multply");
           return;
       }
  if (((maxs1r-mins1r) != (maxdr-mindr)) ||
                      ((maxdc-mindc) != (maxs2c-mins2c))) {
      error("Size of source and destination matrices don't match");
      return;
     }
  erase_area(mindr, mindc, maxdr, maxdc);
    if (mins1r == maxs1r && mins1c == maxs1c && mins2r == maxs2r &&
        mins2c == maxs2c )  {
       /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
              multmatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    }
/* else if ((mins2c==maxs2c) && (mins1r != maxs1r)) {
         for (vr=mindr;vr<=maxdr;vc++)
              multmatrix1(vr,mindc,mins1r,mins1c,maxs1r,maxs1c, 
                                      mins2r,mins2c,maxs2r,maxs2c);
    }
*/
 else {
        /* Everything else */
        multmatrix(mindr, mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    }
}


/*
#define MINMAX 25
multmatrix1 (vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c)
int vr,vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c;
{
    register struct ent *p1,*p2;
    register struct ent *n;
    register int s1r, s1c, s2r, s2c;
    register int dr, dc;
    double sum;
    int i,j,k;
    double list1[MINMAX][MINMAX];
    int list2[MINMAX];
    int list3[MINMAX];

    for (i=0;i<MINMAX;i++)
      {
          list2[MINMAX] = 0;
          for (j=0;j<MINMAX;j++)
              list1[i][j] = 0;
      }
     i = 0;
     for (s1r=mins1r;s1r<=maxs1r;s1r++)
       {
        j = 0;
        for (s1c=mins1c;s1c<=maxs1c;s1c++)
            {
               if ((p1 = tbl[s1r][s1c]) && p1->flags&is_valid)
                    list1[i][j++] = p1->v;
            }
        i++;
       }
     j = 0;
     for (s2r=mins2r;s2r<=maxs2r;s2r++)
            if ((p2 = tbl[s2r][mins2c]) && p2->flags&is_valid)
                  list2[j++] = p2->v;
     for (i=0;i<MINMAX;i++)
        {
            sum = 0;
            for (k=0;k<MINMAX;k++)
               sum = sum + list1[i][k] * list2[k];
            list3[i++] = sum;
        }
     i = 0;
     for (dr=vr, s1r=mins1r; s1r<=maxs1r; s1r++,dr++)
         {
             n = lookat (dr, vc);
            (void) clearent(n);
            if ((p1 = tbl[s1r][mins1c]) && (p1->flags&is_valid))
                {
                   copyent( n, p1, dr - s1r, dc - s1c);
                   n->v = list3[i++];
                 }
       }
}

*/

/***************************************************************************
 *                                                                         *
 *    Calcultes Multiplication of two matrices.                            *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

#define MINMAX 25
multmatrix (vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c)
int vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c;
{
    register struct ent *p1,*p2;
    register struct ent *n;
    register int s1r, s1c, s2r, s2c;
    register int dr, dc;
    double sum;
    int i,j,k;
    double list1[MINMAX][MINMAX];
    double list2[MINMAX][MINMAX];
    double list3[MINMAX][MINMAX];

     for (i=0;i<MINMAX;i++)
       for (j=0;j<MINMAX;j++)
        {
            list1[i][j] = 0;
            list2[i][j] = 0;
            list3[i][j] = 0;
        }
     i = 0;
     for (s1r=mins1r;s1r<=maxs1r;s1r++)
       {
        j = 0;
        for (s1c=mins1c;s1c<=maxs1c;s1c++) 
               if ((p1 = tbl[s1r][s1c]) && p1->flags&is_valid)
                    list1[i][j++] = p1->v;
        i++;
       }
          
     i = 0;
     for (s2r=mins2r;s2r<=maxs2r;s2r++)
       {
         j = 0;
         for (s2c=mins2c;s2c<=maxs2c;s2c++)
            if ((p2 = tbl[s2r][s2c]) && p2->flags&is_valid)
                  list2[i][j++] = p2->v;
         i++;
       }
     for (i=0;i<MINMAX;i++)
           for (j=0;j<MINMAX;j++)
              {
                 sum = 0;
                 for (k=0;k<MINMAX;k++)
                     sum = sum + (list1[i][k]) * (list2[k][j]);
                 list3[i][j] = sum;
               }
     i = 0;
     for (dr=vr, s1r=mins1r; s1r<=maxs1r; s1r++,dr++)
       {
          j = 0;
          for (dc=vc, s1c=mins2c; s1c<=maxs2c; s1c++, dc++) {
            n = lookat (dr, dc);
            (void) clearent(n);
            if ((p1 = tbl[s1r][s1c]) && (p1->flags&is_valid))
                {
                   copyent( n, p1, dr - s1r, dc - s1c);
                   n->v = list3[i][j++];
                 }
            }
           i++;
       }
}


/***************************************************************************
 *                                                                         *
 *    Invet the matrix.                                                    *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

#define REG_LEN 28
get_invert()
{
  char s[100];
  struct m_range_sd *find_rge();
  struct ent *dv1,*dv2,*v1,*v2;
  int minsr, minsc;
  int maxsr, maxsc;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;

  sprintf(s, "invert [dest_range src_range] ");
  get_str(s,REG_LEN);
  rge_sd=find_rge(s);
  if (rge_sd == NULL) {
      error("Input syntax error");
      return;
  }
  dv1=lookat(rge_sd->dsr,rge_sd->dsc);
  dv2=lookat(rge_sd->der,rge_sd->dec);
  v1=lookat(rge_sd->ssr,rge_sd->ssc);
  v2=lookat(rge_sd->ser,rge_sd->sec);
  mindr = dv1->row;
  mindc = dv1->col;
  maxdr = dv2->row;
  maxdc = dv2->col;
  if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
  if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
  maxsr = v2->row;
  maxsc = v2->col;
  minsr = v1->row;
  minsc = v1->col;
  if (minsr>maxsr) r = maxsr, maxsr = minsr, minsr = r;
  if (minsc>maxsc) c = maxsc, maxsc = minsc, minsc = c;
  if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     error ("The table can't be any bigger");
     return;
  }
  if (((maxsr-minsr) != (maxdr-mindr)) ||
                      ((maxdc-mindc) != (maxsc-minsc))) {
      error("Size of source and destination matrices don't match");
      return;
     }
  erase_area(mindr, mindc, maxdr, maxdc);
  if (minsr == maxsr && minsc == maxsc) {
        /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
               invertmatrix(vr, vc, minsr, minsc, maxsr, maxsc);
   } else {
        /* Everything else */
        invertmatrix(mindr, mindc, minsr, minsc, maxsr, maxsc);
   }
}

/***************************************************************************
 *                                                                         *
 *    Calcultes the inversion of the matrix.                               *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

#define MAXROW 30
#define MAXCOL 30
invertmatrix (vr, vc, minsr, minsc, maxsr, maxsc)
int vr, vc, minsr, minsc, maxsr, maxsc;
{
    register struct ent *p;
    register struct ent *q;
    register int sr, sc;
    register int dr, dc;
    double list[MAXROWS-1][MAXCOLS-1];
    int i,j,k,m,n;
    int rowlim,collim;
    double temp;
   
    for (i=0; i<MAXROW; i++)
      for (j=0; j<MAXCOL; j++)
          list[i][j] = 0;
    rowlim = maxsr-minsr+1;
    collim = maxsc-minsc+1;
    for (i=0;i<rowlim;i++)
       for (j=0;j<collim;j++)
          if (i==j)  list[i][j] = 1;
    for (i=0,sr=minsr;(i<rowlim) && (sr<=maxsr); i++,sr++)
       for (j=collim,sc=minsc;(j<(2*collim)) && (sc<=maxsc); j++,sc++)
           if ((p = tbl[sr][sc]) && p->flags&is_valid)
                list[i][j] = p->v;
    for (j=collim; j<(2*collim); j++)  {
        k = j-collim;
        temp = list[k][j];
        for (i=0; i<(2*collim); i++)
            list[k][i] = list[k][i] / temp;
        for (m=0; m < rowlim; m++)
            for (n=0; n<(2*collim); n++)
               if (( m != k ) && ( n != j )) 
                  list[m][n] = list[m][n]-list[k][n]*list[m][j];
    }
    i = 0;
    for (dr=vr, sr=minsr; sr<=maxsr; sr++,dr++)
       {
          j = 0;
          for (dc=vc, sc=minsc; sc<=maxsc; sc++, dc++) {
            q = lookat (dr, dc);
            (void) clearent(q);
            if ((p = tbl[sr][sc]) && (p->flags&is_valid))
                {
                   copyent( q, p, dr - sr, dc - sc);
                   q->v = list[i][j++];
                 }
            }
           i++;
       }
}


/*=========================================================================================
*/

void Main_Menu()
{
static char *item[] = {
                             "Range",
			     "Column/Row",
			     "Option",
			     "File",
			     "Graph",
                             "Matrix",
			     "Quit"
                       };

static char *help_prompt[] = {
		             "Erase  Value  Copy  Fill  Define  Show  Undefine",
                             "Insert  Append  Delete  Pull  Remove  Hide  Show  Format",
                             "Auto  Numeric  Top  Cell  Encrypt  Pre-scale  Ext-funcs  Set",
		             "Get  Put  Write  Table  Merge  Combine  Directory",
			     "Type  A  B  C  D  E  F  Reset  View  Option",
                   "Transpose  Addition  Substraction  Multiplication  Invert ",
			     "Quit this spreadsheet"
			    };

      switch(menu(7, item, help_prompt)) {
      case 0:
	 show_top_line();
   	 break;
      case 1:
   	 Range_Menu();
	 break;
      case 2:
	 Row_Col_Menu();
   	 break;
      case 3:
   	 Option_Menu();
	 break;
      case 4:
	 File_Menu();
	 break;
      case 5:
	 Graph_Menu();
	 break;
      case 6:
         Matrix_Menu();
         break;
      case 7:
	 running = 0;
	 break;
       } 
    }

/* this is the old version, using curses.  The new version follows 
char *get_str(s, max_str_len)
char *s;
int max_str_len;
{
    (void) fflush(stdout);
    (void) move (0, 0);
    (void) clrtoeol ();
    (void) addstr (s);
    (void) refresh();

    (void) nl();
    (void) echo();
    fgets(s, max_str_len, stdin);
    (void) nonl();
    (void) move (0, 0);
    (void) clrtoeol ();
    (void) printw("revmsg");
    (void) move(1, 0);
    (void) show_top_line();
    (void) noecho();
    return s;
}  */


char *get_str(s, max_str_len)
  char *s;               /* prompt and returned string */
  int max_str_len;
{
  static char buf[1024]; /* hold the characters as they are typed */
  int count=0;           /* how many characters have been entered */
  int maxcount;          /* the max number of chars to be entered */
  int done=0;            /* true when input is finished */
  int slen;              /* length of prompt string */
  XEvent event;          /* input event structure */
  char keystr[3];        /* ASCII version of keypress */

  clearlines(0,0);
  slen = strlen(s);
  max_str_len--;      /* decrease this to save room for null byte */
  maxcount = maintextcols - slen;
  maxcount = ((maxcount < max_str_len) ? maxcount : max_str_len);
  buf[0]='_'; /* the "cursor" */
  if (slen)
    XDrawImageString(dpy,mainwin,maingc,
                     textcol(0), textrow(0),
                     s, slen);
  XDrawImageString(dpy,mainwin,maingc,
                   textcol(slen+1), textrow(0),
                   "_", 1 );
  while (!done){
    XNextEvent(dpy,&event);
    switch(event.type){
      case Expose:
        update();
        if (slen)
          XDrawImageString(dpy,mainwin,maingc,
                           textcol(0), textrow(0),
                           s, slen);
        if (count)
          XDrawImageString(dpy,mainwin,maingc,
                           textcol(slen), textrow(0),
                           buf, count);
        XDrawImageString(dpy,mainwin,maingc,
                         textcol(slen+count), textrow(0),
                         "_", 1 );
        break;

      case MappingNotify:
        XRefreshKeyboardMapping((XMappingEvent *)&event);
        break;
      case ConfigureNotify:
        sc_handleresize(&event);
        maxcount = maintextcols - slen;
        maxcount = ((maxcount < max_str_len) ? maxcount : max_str_len);
        break;
      case KeyPress:
        if (XLookupString((XKeyEvent *)&event, keystr, 3, 0, 0)){
          switch( keystr[0]){
            case 10: /* linefeed */
            case 13: /* carriage return */
              done =1;
              break;
            case ctl('h'):  /* backspace */
            case ctl('?'):  /* delete */
	    case DEL:
              if (count){
                buf[--count]='_';
                XDrawImageString(dpy,mainwin,maingc,
                                 textcol(slen+count+1),textrow(0),
                                 "_ ", 2);
              } else {
                fprintf(stderr,"\007"); /* bell */
              }
              break;
            default:
              if ((keystr[0]>=32) && (keystr[0]<127)){
                if (count<maxcount){
                  buf[count++]=keystr[0];
                  buf[count]='_';
                  XDrawImageString(dpy,mainwin,maingc,
                                   textcol(slen+count),textrow(0),
                                   buf+count-1, 2);
                } else
                    fprintf(stderr,"\007");
              } else
                  fprintf(stderr,"\007");
              break;
          } /* switch keystr[0] */
        } /* if XLookupString */
      } /* switch event.type */
    } /* while !done */
    buf[count]=0;
    strcpy(s,buf);
    clearlines(0,0);
    show_top_line();
    return s;
}
