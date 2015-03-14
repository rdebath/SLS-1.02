/*-------------------------------------------------------------------------
	panel.c - part of PANELS implementation for ncurses/Linux
	zmbenhal@netcom.com
	author:
	wht@n4hgf.Mt-Park.GA.US

  This module is not an efficient replacement for the SVR3.2 panel
  facility.  It is, however, fully featured and serves the needs of
  u386mon, assisting a port to SVR3.1.  It seems efficient enough to
  use in lieu of native (vendor-supplied) panels.

  Defined functions:
	Touchline(pan,start,count)
	Touchpan(pan)
	Wnoutrefresh(pan)
	__calculate_obscure()
	__free_obscure(pan)
	__override(pan,show)
	__panel_is_linked(pan)
	__panel_link_bottom(pan)
	__panel_link_top(pan)
	__panel_unlink(pan)
	__panels_overlapped(pan1,pan2)
	bottom_panel(pan)
	dPanel(text,pan)
	dStack(fmt,num,pan)
	del_panel(pan)
	hide_panel(pan)
	move_panel(pan,starty,startx)
	new_panel(win)
	open_dfp()
	panel_above(pan)
	panel_below(pan)
	panel_hidden(pan)
	panel_userptr(pan)
	panel_window(pan)
	replace_panel(pan,win)
	set_panel_userptr(pan,uptr)
	show_panel(pan)
	top_panel(pan)
	update_panels()

--------------------------------------------------------------------------*/

#ifdef TRACE         
#define PANEL_DEBUG
#endif

#include <ncurses.h>
#include "panel.h"

#define getbegx(w)	(w)->_begx
#define getbegy(w)	(w)->_begy
#define getmaxx(w)	(w)->_maxx
#define getmaxy(w)	(w)->_maxy

PANEL *__bottom_panel = (PANEL *)0;
PANEL *__top_panel = (PANEL *)0;
PANEL __stdscr_pseudo_panel = { (WINDOW *)0 };

#define STATIC static

#ifdef PANEL_DEBUG
#define dBug(fmt,p1,p2) _tracef((fmt),(p1),(p2))
#else
#define dBug(fmt,p1,p2)
#endif

/*+-------------------------------------------------------------------------
	dPanel(text,pan)
--------------------------------------------------------------------------*/
#ifdef PANEL_DEBUG
dPanel(text,pan)
char *text;
PANEL *pan;
{
	_tracef("%s id=%s b=%s a=%s y=%d x=%d",
		text,pan->user,
		(pan->below) ? pan->below->user : "--",
		(pan->above) ? pan->above->user : "--",
		pan->wstarty, pan->wstartx);
}	/* end of dPanel */
#else
#define dPanel(text,pan)
#endif

/*+-------------------------------------------------------------------------
	dStack(fmt,num,pan)
--------------------------------------------------------------------------*/
#ifdef PANEL_DEBUG
void
dStack(fmt,num,pan)
char *fmt;
int num;
PANEL *pan;
{
char s80[80];

	sprintf(s80,fmt,num,pan);
	_tracef("%s b=%s t=%s",s80,
		(__bottom_panel) ? __bottom_panel->user : "--",
		(__top_panel)    ? __top_panel->user    : "--");
	if(pan)
		_tracef("pan id=%s",pan->user);
	pan = __bottom_panel;
	while(pan)
	{
		dPanel("stk",pan);
		pan = pan->above;
	}
}	/* end of dStack */
#else
#define dStack(fmt,num,pan)
#endif

/*+-------------------------------------------------------------------------
	Wnoutrefresh(pan) - debugging hook for wnoutrefresh
--------------------------------------------------------------------------*/
#ifdef PANEL_DEBUG
STATIC int
Wnoutrefresh(pan)
PANEL *pan;
{
	dPanel("wnoutrefresh",pan);
	wnoutrefresh(pan->win);
}	/* end of Wnoutrefresh */
#else
#define Wnoutrefresh(pan) wnoutrefresh((pan)->win)
#endif

/*+-------------------------------------------------------------------------
	Touchpan(pan)
--------------------------------------------------------------------------*/
#ifdef PANEL_DEBUG
STATIC int
Touchpan(pan)
PANEL *pan;
{
	dPanel("Touchpan",pan);
	touchwin(pan->win);
}	/* end of Touchpan */
#else
#define Touchpan(pan) touchwin((pan)->win)
#endif

/*+-------------------------------------------------------------------------
	Touchline(pan,start,count)
--------------------------------------------------------------------------*/
#ifdef PANEL_DEBUG
STATIC int
Touchline(pan,start,count)
PANEL *pan;
int start;
int count;
{
char s80[80];
	sprintf(s80,"Touchline s=%d c=%d",start,count);
	dPanel(s80,pan);
	touchline(pan->win,start,count);
}	/* end of Touchline */
#else
#define Touchline(pan,start,count) touchline((pan)->win,start,count)
#endif

/*+-------------------------------------------------------------------------
	__panels_overlapped(pan1,pan2) - check panel overlapped
--------------------------------------------------------------------------*/
STATIC int
__panels_overlapped(pan1,pan2)
register PANEL *pan1;
register PANEL *pan2;
{
	if(!pan1 || !pan2)
		return(0);
	dBug("__panels_overlapped %s %s",pan1->user,pan2->user);
	if((pan1->wstarty >= pan2->wstarty) && (pan1->wstarty < pan2->wendy) &&
		(pan1->wstartx >= pan2->wstartx) && (pan1->wstartx < pan2->wendx))
		return(1);
	if((pan1->wstarty >= pan1->wstarty) && (pan2->wstarty < pan1->wendy) &&
		(pan1->wstartx >= pan1->wstartx) && (pan2->wstartx < pan1->wendx))
		return(1);
	dBug("  no",0,0);
	return(0);
}	/* end of __panels_overlapped */

/*+-------------------------------------------------------------------------
	__free_obscure(pan)
--------------------------------------------------------------------------*/
STATIC void
__free_obscure(pan)
PANEL *pan;
{
PANELOBS *tobs = pan->obscure;				/* "this" one */
PANELOBS *nobs;								/* "next" one */

	while(tobs)
	{
		nobs = tobs->above;
		free((char *)tobs);
		tobs = nobs;
	}
	pan->obscure = (PANELOBS *)0;
}	/* end of __free_obscure */

/*+-------------------------------------------------------------------------
	__override(pan,show)
--------------------------------------------------------------------------*/
STATIC void
__override(pan,show)
PANEL *pan;
int show;
{
register y;
register PANEL *pan2;
PANELOBS *tobs = pan->obscure;				/* "this" one */

	dBug("__override %s,%d",pan->user,show);

	if(show == 1)
		Touchpan(pan);
	else if(!show)
	{
		Touchpan(pan);
/*
		Touchline(&__stdscr_pseudo_panel,pan->wendy,getmaxy(pan->win));
*/
		Touchpan(&__stdscr_pseudo_panel);
	}
	else if(show == -1)
	{
		while(tobs && (tobs->pan != pan))
			tobs = tobs->above;
	}

	while(tobs)
	{
		if((pan2 = tobs->pan) != pan)
		{
			dBug("test obs pan=%s pan2=%s",pan->user,pan2->user);
			for(y = pan->wstarty; y < pan->wendy; y++)
			{
				if( (y >= pan2->wstarty) && (y < pan2->wendy) &&
					((is_linetouched(pan->win,y - pan->wstarty) == 1) ||
					(is_linetouched(stdscr,y) == 1)))
				{
					Touchline(pan2,y - pan2->wstarty,1);
				}
			}
		}
		tobs = tobs->above;
	}
}	/* end of __override */

/*+-------------------------------------------------------------------------
	__calculate_obscure()
--------------------------------------------------------------------------*/
STATIC void
__calculate_obscure()
{
PANEL *pan;
register PANEL *pan2;
register PANELOBS *tobs;			/* "this" one */
PANELOBS *lobs = (PANELOBS *)0;		/* last one */

	pan = __bottom_panel;
	while(pan)
	{
		if(pan->obscure)
			__free_obscure(pan);
		dBug("--> __calculate_obscure %s",pan->user,0);
		lobs = (PANELOBS *)0;		/* last one */
		pan2 = __bottom_panel;
		while(pan2)
		{
			if(__panels_overlapped(pan,pan2))
			{
				if(!(tobs = (PANELOBS *)malloc(sizeof(PANELOBS))))
					return;
				tobs->pan = pan2;
				dPanel("obscured",pan2);
				tobs->above = (PANELOBS *)0;
				if(lobs)
					lobs->above = tobs;
				else
					pan->obscure = tobs;
				lobs  = tobs;
			}
			pan2 = pan2->above;
		}
		__override(pan,1);
		pan = pan->above;
	}

}	/* end of __calculate_obscure */

/*+-------------------------------------------------------------------------
	__panel_is_linked(pan) - check to see if panel is in the stack
--------------------------------------------------------------------------*/
STATIC int
__panel_is_linked(pan)
PANEL *pan;
{
register PANEL *pan2 = __bottom_panel;

	while(pan2)
	{
		if(pan2 == pan)
			return(1);
		pan2 = pan2->above;
	}
	return(OK);
}	/* end of __panel_is_linked */

/*+-------------------------------------------------------------------------
	__panel_link_top(pan) - link panel into stack at top
--------------------------------------------------------------------------*/
STATIC void
__panel_link_top(pan)
PANEL *pan;
{

#ifdef PANEL_DEBUG
	dStack("<lt%d>",1,pan);
	if(__panel_is_linked(pan))
		return;
#endif

	pan->above = (PANEL *)0;
	pan->below = (PANEL *)0;
	if(__top_panel)
	{
		__top_panel->above = pan;
		pan->below = __top_panel;
	}
	__top_panel = pan;
	if(!__bottom_panel)
		__bottom_panel = pan;
	__calculate_obscure();
	dStack("<lt%d>",9,pan);

}	/* end of __panel_link_top */

/*+-------------------------------------------------------------------------
	__panel_link_bottom(pan) - link panel into stack at bottom
--------------------------------------------------------------------------*/
STATIC void
__panel_link_bottom(pan)
PANEL *pan;
{

#ifdef PANEL_DEBUG
	dStack("<lb%d>",1,pan);
	if(__panel_is_linked(pan))
		return;
#endif

	pan->above = (PANEL *)0;
	pan->below = (PANEL *)0;
	if(__bottom_panel)
	{
		__bottom_panel->below = pan;
		pan->above = __bottom_panel;
	}
	__bottom_panel = pan;
	if(!__top_panel)
		__top_panel = pan;
	__calculate_obscure();
	dStack("<lb%d>",9,pan);

}	/* end of __panel_link_bottom */

/*+-------------------------------------------------------------------------
	__panel_unlink(pan) - unlink panel from stack
--------------------------------------------------------------------------*/
STATIC void
__panel_unlink(pan)
PANEL *pan;
{
register PANEL *prev;
register PANEL *next;

#ifdef PANEL_DEBUG
	dStack("<u%d>",1,pan);
	if(!__panel_is_linked(pan))
		return;
#endif

	__override(pan,0);
	__free_obscure(pan);

	prev = pan->below;
	next = pan->above;

	if(prev)		/* if non-zero, we will not update the list head */
	{
		prev->above = next;
		if(next)
			next->below = prev;
	}
	else if(next)
		next->below = prev;
	if(pan == __bottom_panel)
		__bottom_panel = next;
	if(pan == __top_panel)
		__top_panel = prev;

	__calculate_obscure();

	pan->above = (PANEL *)0;
	pan->below = (PANEL *)0;
	dStack("<u%d>",9,pan);

}	/* end of __panel_unlink */

/*+-------------------------------------------------------------------------
	panel_window(pan) - get window associated with panel
--------------------------------------------------------------------------*/
WINDOW *
panel_window(pan)
PANEL *pan;
{
	return(pan->win);
}	/* end of panel_window */

/*+-------------------------------------------------------------------------
	update_panels() - wnoutrefresh windows in an orderly fashion
--------------------------------------------------------------------------*/
void
update_panels()
{
PANEL *pan;

	dBug("--> update_panels",0,0);
	pan = __bottom_panel;
	while(pan)
	{
		__override(pan,-1);
		pan = pan->above;
	}

	if(is_wintouched(stdscr))
		Wnoutrefresh(&__stdscr_pseudo_panel);
	
	if(pan = __bottom_panel)
	{
		while(pan)
		{
			if(is_wintouched(pan->win))
				Wnoutrefresh(pan);
			pan = pan->above;
		}
	}
}	/* end of update_panels */

/*+-------------------------------------------------------------------------
	hide_panel(pan) - remove a panel from stack
--------------------------------------------------------------------------*/
int
hide_panel(pan)
register PANEL *pan;
{

	if(!pan)
		return(ERR);

	dBug("--> hide_panel %s",pan->user,0);

	if(!__panel_is_linked(pan))
	{
		pan->above = (PANEL *)0;
		pan->below = (PANEL *)0;
		return(ERR);
	}

	__panel_unlink(pan);

	return(OK);
}	/* end of hide_panel */

/*+-------------------------------------------------------------------------
	show_panel(pan) - place a panel on top of stack
may already be in stack
--------------------------------------------------------------------------*/
int
show_panel(pan)
register PANEL *pan;
{

	if(!pan)
		return(ERR);
	if(pan == __top_panel)
		return(OK);
	dBug("--> show_panel %s",pan->user,0);
	if(__panel_is_linked(pan))
		(void)hide_panel(pan);
	__panel_link_top(pan);
	return(OK);
}	/* end of show_panel */

/*+-------------------------------------------------------------------------
	top_panel(pan) - place a panel on top of stack
--------------------------------------------------------------------------*/
int
top_panel(pan)
register PANEL *pan;
{
	return(show_panel(pan));
}	/* end of top_panel */

/*+-------------------------------------------------------------------------
	del_panel(pan) - remove a panel from stack, if in it, and free struct
--------------------------------------------------------------------------*/
int
del_panel(pan)
register PANEL *pan;
{
	if(pan)
	{
		dBug("--> del_panel %s",pan->user,0);
		if(__panel_is_linked(pan))
			(void)hide_panel(pan);
		free((char *)pan);
		return(OK);
	}
	return(ERR);
}	/* end of del_panel */

/*+-------------------------------------------------------------------------
	bottom_panel(pan) - place a panel on bottom of stack
may already be in stack
--------------------------------------------------------------------------*/
int
bottom_panel(pan)
register PANEL *pan;
{
	if(!pan)
		return(ERR);
	if(pan == __bottom_panel)
		return(OK);
	dBug("--> bottom_panel %s",pan->user,0);
	if(__panel_is_linked(pan))
		(void)hide_panel(pan);
	__panel_link_bottom(pan);
	return(OK);
}	/* end of bottom_panel */

/*+-------------------------------------------------------------------------
	new_panel(win) - create a panel and place on top of stack
--------------------------------------------------------------------------*/
PANEL *
new_panel(win)
WINDOW *win;
{
PANEL *pan = (PANEL *)malloc(sizeof(PANEL));

	if(!__stdscr_pseudo_panel.win)
	{
		__stdscr_pseudo_panel.win = stdscr;
		__stdscr_pseudo_panel.wstarty = 0;
		__stdscr_pseudo_panel.wstartx = 0;
		__stdscr_pseudo_panel.wendy = LINES;
		__stdscr_pseudo_panel.wendx = COLS;
		__stdscr_pseudo_panel.user = "stdscr";
		__stdscr_pseudo_panel.obscure = (PANELOBS *)0;
	}

	if(pan)
	{
		pan->win = win;
		pan->above = (PANEL *)0;
		pan->below = (PANEL *)0;
		pan->wstarty = getbegy(win);
		pan->wstartx = getbegx(win);
		pan->wendy = pan->wstarty + getmaxy(win);
		pan->wendx = pan->wstartx + getmaxx(win);
#ifdef PANEL_DEBUG
		pan->user = "new";
#else
		pan->user = (char *)0;
#endif
		pan->obscure = (PANELOBS *)0;
		(void)show_panel(pan);
	}

	return(pan);
}	/* end of new_panel */

/*+-------------------------------------------------------------------------
	panel_above(pan)
--------------------------------------------------------------------------*/
PANEL *
panel_above(pan)
PANEL *pan;
{
	if(!pan)
		return(__bottom_panel);
	else
		return(pan->above);
}	/* end of panel_above */

/*+-------------------------------------------------------------------------
	panel_below(pan)
--------------------------------------------------------------------------*/
PANEL *
panel_below(pan)
PANEL *pan;
{
	if(!pan)
		return(__top_panel);
	else
		return(pan->below);
}	/* end of panel_below */

/*+-------------------------------------------------------------------------
	set_panel_userptr(pan,uptr)
--------------------------------------------------------------------------*/
int
set_panel_userptr(pan,uptr)
PANEL *pan;
char *uptr;
{
	if(!pan)
		return(ERR);
	pan->user = uptr;
	return(OK);
}	/* end of set_panel_userptr */

/*+-------------------------------------------------------------------------
	panel_userptr(pan)
--------------------------------------------------------------------------*/
char *
panel_userptr(pan)
PANEL *pan;
{
	if(!pan)
		return((char *)0);
	return(pan->user);
}	/* end of panel_userptr */

/*+-------------------------------------------------------------------------
	move_panel(pan,starty,startx)
--------------------------------------------------------------------------*/
int
move_panel(pan,starty,startx)
PANEL *pan;
int starty;
int startx;
{
WINDOW *win;

	if(!pan)
		return(ERR);
	if(__panel_is_linked(pan))
		__override(pan,0);
	win = pan->win;
	if(mvwin(win,starty,startx))
		return(ERR);
	pan->wstarty = getbegy(win);
	pan->wstartx = getbegx(win);
	pan->wendy = pan->wstarty + getmaxy(win);
	pan->wendx = pan->wstartx + getmaxx(win);
	if(__panel_is_linked(pan))
		__calculate_obscure();
	return(OK);
}	/* end of move_panel */

/*+-------------------------------------------------------------------------
	replace_panel(pan,win)
--------------------------------------------------------------------------*/
int
replace_panel(pan,win)
PANEL *pan;
WINDOW *win;
{
	if(!pan)
		return(ERR);
	if(__panel_is_linked(pan))
		__override(pan,0);
	pan->win = win;
	if(__panel_is_linked(pan))
		__calculate_obscure();
	return(OK);
}	/* end of replace_panel */

/*+-------------------------------------------------------------------------
	panel_hidden(pan)
--------------------------------------------------------------------------*/
int
panel_hidden(pan)
PANEL *pan;
{
	if(!pan)
		return(ERR);
	return(__panel_is_linked(pan) ? ERR : OK);
}	/* end of panel_hidden */

/* end of libpanel.c */
