/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* do a button event */
/*}}}  */

/*{{{  #includes*/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "event.h"

#include "Write.h"
#include "border.h"
#include "do_button.h"
#include "font_subs.h"
#include "get_rect.h"
#include "get_text.h"
#include "intersect.h"
#include "move_box.h"
/*}}}  */
/*{{{  #defines*/
#define FSIZE(c)	((int) (W(font)->head.c))	/* from put_window.c */
#define SUB_SIZE	256				/* max temp str size */
#define START_SIZE   16          /* default starting size of sweep object */
/*}}}  */

/*{{{  event_args -- extract numeric argument from sweep events*/
static char *event_args(str,count,args)
char *str;        /*  beginning of args */
int *count;       /* # of args */
int *args;        /* where to put args */
	{
	register char c, *pntr = str;    /* 1st char of args */

	while (((c = *++pntr) >= '0' && c <= '9') || c==',' || c=='-')
		;
	*count = sscanf(str+1,"%d,%d,%d,%d",args,args+1,args+2,args+3);
	return(pntr);
	}
/*}}}  */
/*{{{  get_id -- compute a unique window id*/
static char *get_id(win) register WINDOW *win;
	{
	int sub = W(num);	/* subwindow number */
	int main = W(pid);	/* main window id */
	static char buff[6];

	sprintf(buff,"%d.%d",main,sub);
	return(buff);
	}
/*}}}  */
/*{{{  sub_event -- substitute %x into str, returns true if an area was swept.*/
static int sub_event(win,str,c,swept,count,args)
register WINDOW *win;
register char *str,c;
int swept;     /* if swept, don't do sweeps */
int count;     /* # of sweep args */
int *args;     /* the arg list */
	{
	int sweep = 0;
	static int x,y;
	char *get_id();
	int code;	/* for text sweeping */

 
#ifdef DEBUG
	if (debug) {
		register int i;
		dprintf(e)(stderr,"%s) event (%c) args:",W(tty),c);
		for(i=0;i<count;i++)
			dprintf(e)(stderr," %d",args[i]);
		dprintf(e)(stderr,"\r\n");
		}
#endif

	/* setup initial sweep conditions */

	if (swept == 0) {    /* no sweep - set up initial conditions */
		if (count >=2) {
			x = args[0];
			y = args[1];
			}
		else if (c == E_SWTEXT || c == E_SWTEXTT) {
			x = 0;
			y = 0;
			}
		else {
			x = START_SIZE;
			y = START_SIZE;
			}
#ifdef DEBUG
		dprintf(e)(stderr,"initial sweep (x,y) = (%d,%d)\r\n",x,y);
#endif
		count = 0;
		}

	switch(c) {
		case E_CPOS:	/* return mouse position (rows/cols) */
			sprintf(str,"%d %d",(mousex-(W(x0)+W(text).x))/FSIZE(wide),
			                    (mousey-(W(y0)+W(text).y))/FSIZE(high));
			break;
		case E_POS:		/* return mouse position */
			if (W(flags)&W_ABSCOORDS)
			   sprintf(str,"%d %d",mousex-W(x0),mousey-W(y0));
			else
			   sprintf(str,"%d %d", (mousex-W(x0))*GMAX/BIT_WIDE(W(window)),
			                      (mousey-W(y0))*GMAX/BIT_HIGH(W(window)));
			break;
		case E_SWLINE:		/* sweep out line */
			sweep++;
			if (!swept)
			   get_rect(screen,mouse,mousex,mousey,&x,&y,1);
			sprintf(str,"%d %d %d %d",mousex-W(x0),mousey-W(y0),
			                         mousex+x-W(x0),mousey+y-W(y0));
			break; 
		case E_SWRECT:		/* sweep out rectangle */
			sweep++;
			if (!swept)
			   get_rect(screen,mouse,mousex,mousey,&x,&y,0);
			sprintf(str,"%d %d %d %d",mousex-W(x0),mousey-W(y0),
			                         mousex+x-W(x0),mousey+y-W(y0));
			break; 
		case E_SWRECTA:		/* sweep out rectangle */
			sweep++;
			if (!swept)
			   get_rect(screen,mouse,mousex,mousey,&x,&y,0);
			sprintf(str,"%d %d %d %d",mousex,mousey,
			                         mousex+x,mousey+y);
			break; 
      case E_SWBOX:     /* sweep out box */
         sweep++;
         if (!swept)
            move_box(screen,mouse,&mousex,&mousey,x,y,1);
         sprintf(str,"%d %d",mousex-W(x0),mousey-W(y0));
         break;
      case E_SWBOXA:    /* sweep out box */
         sweep++;
         if (!swept)
            move_box(screen,mouse,&mousex,&mousey,x,y,1);
         sprintf(str,"%d %d",mousex,mousey);
         break;
		case E_SWTEXTT:		/* sweep out text */
		case E_SWTEXT:		/* sweep out text */
			sweep++;
			code = 0;
			if (!swept)
			    code = get_text(screen,mouse,mousex,mousey,&x,&y,win,c);
			sprintf(str,code ? "%d %d %d %d" : "",
			        (mousex-(W(x0)+W(text.x)))/FSIZE(wide),
			        (mousey-(W(y0)+W(text.y)))/FSIZE(high),
			        x,y);
			break; 
		case E_NOTIFY:		/* get other windows notify text */
			for(win=active;win != (WINDOW *) 0;win=W(next)) {
			   if (mousein(mousex,mousey,win,1))
			      break;
			   }
			if (win && IS_EVENT(win,EVENT_NOTIFY))
			   sprintf(str,"%.*s",SUB_SIZE-1,W(events[GET_EVENT(EVENT_NOTIFY)]));
			else
			   *str='\0';
			break; 
		case E_WHO:		/* send other windows id */
			for(win=active;win != (WINDOW *) 0;win=W(next)) {
			   if (mousein(mousex,mousey,win,1))
			      break;
			   }
			if (win)
			   sprintf(str,"%.*s",SUB_SIZE-1,get_id(win));
			else
			   *str='\0';
			break; 
		case E_WHOSIZE:		/* send other windows size */
			for(win=active;win != (WINDOW *) 0;win=W(next)) {
			   if (mousein(mousex,mousey,win,1))
			      break;
			   }
			if (win)
			   sprintf(str,"%d %d %d %d",
			           W(x0),W(y0),BIT_WIDE(W(border)),BIT_HIGH(W(border)));
			else
			   *str='\0';
			break; 
		case E_FROM:			/* see who message is from */
			sprintf(str,"%d",id_message);
			break; 
		case E_MESS:
			if (message)
			   sprintf(str,"%.*s",SUB_SIZE-1,message);
			else
			   *str = '\0';
			break; 
		case E_MSGSIZE:
			sprintf(str,"%d",message ? strlen(message) : 0);
			break; 
		case E_SNARFSIZE:				/* size of snarf buffer */
			sprintf(str,"%d",snarf ? strlen(snarf) : 0);
			break; 
		case E_SNARFBUF:				/* contents of snarf buffer */
			if (snarf)
			   sprintf(str,"%.*s",SUB_SIZE-1,snarf);
			else
			   *str = '\0';
			break; 
		case E_TIMESTAMP:		/* 100ths seconds since MGR startup */
			sprintf(str,"%d",timestamp());
			break; 
		case E_ESC:			/* the escape char */
			strcpy(str,"%");
			break; 
		}
	return(sweep);
	}
/*}}}  */

/*{{{  write_event -- write the event to a process*/
void write_event(win,str,list)
WINDOW *win;				/* window to get info about */
char *str;				/* event string */
char *list;				/* list of valid event chars */
	{
	char data[SUB_SIZE];
	int args[4];         /* arguments to sweep event */
	int count;           /* # of args */
	register char *start;
	char *end;
	int swept = 0;			/* already did a sweep */

	for(start=str;*start && (end=index(start,E_ESC));start=end+1) {
#ifdef DEBUG
		dprintf(e)(stderr,"  sending %d [%s]\r\n",strlen(str),str);
#endif
		if (end>start) 
			Write(W(to_fd),start,end-start);
		end = event_args(end,&count,args);
		if (index(list,*end)) {
			swept += sub_event(win,data,*end,swept,count,args);
			Write(W(to_fd),data,strlen(data));
			}
		}
	if (*start)
		Write(W(to_fd),start,strlen(start));
	if( swept )
		/* If we swept something, the button was down and is now up.  Notify
	 do_button(). */
		do_button(0);
	}
/*}}}  */
/*{{{  do_event -- do a button event*/
void do_event(event,win,flag)
int event;				/* event number */
register WINDOW *win;			/* window event applies to */
int flag;				/* type of window */
	{
	register char *buff;

	if (!win) return;

#ifdef DEBUG
	dprintf(e)(stderr,"%s: event %d (%s) %s\r\n",W(tty),GET_EVENT(event),
			     IS_EVENT(win,event)?"ON":"OFF",flag==E_MAIN ? "MAIN":"STACK");
#endif

		/* look for stacked events */

		if (IS_EVENT(win,EVENT_STFLAG)) {
			do_event(event,win->stack,E_STACK);
			}

		if (IS_EVENT(win,event) && (flag==E_MAIN || IS_EVENT(win,EVENT_STACK))) {

#ifdef DEBUG
			dprintf(e)(stderr,"\tSENT\r\n");
#endif

			/* do the event */

			switch(event) {
			case EVENT_B1_DOWN:
			case EVENT_B2_DOWN:
			     if (IS_EVENT(win,event) && (buff= W(events[GET_EVENT(event)])))
			        write_event(win,buff,E_LIST_BUTTON);

			     /* notify clicked window */

			     for(win=active;win != (WINDOW *) 0;win=W(next))
			        if(mousein(mousex,mousey,win,1))
			           break;
			     if (win && IS_EVENT(win,EVENT_TELLME)
			             && (buff= W(events[GET_EVENT(EVENT_TELLME)]))) {
			        if (message) { 
			           free(message);
			           message = (char *) 0;
			           }
			        id_message = ACTIVE(pid);
			        write_event(win,buff,E_LIST_ACCEPT);
			        }
			     break;
			case EVENT_PASTE:
			     if (IS_EVENT(win,event) && (buff= W(events[GET_EVENT(event)])))
			        write_event(win,buff,E_LIST_PASTE);
			     break;
			case EVENT_SNARFED:
			     if (IS_EVENT(win,event) && (buff= W(events[GET_EVENT(event)])))
			        write_event(win,buff,E_LIST_SNARF);
			     break;
			case EVENT_BSYS_DOWN:	/* No events for System Button, down or up. */
			case EVENT_BSYS_UP:	
				break;
			case EVENT_B1_UP:
			case EVENT_B2_UP:

			case EVENT_SHAPE:
			case EVENT_MOVE:
			case EVENT_DESTROY:
			case EVENT_REDRAW:
			case EVENT_COVERED:
			case EVENT_UNCOVERED:
			case EVENT_DEACTIVATED:
			case EVENT_ACTIVATED:
				if (IS_EVENT(win,event) && (buff= W(events[GET_EVENT(event)])))
					write_event(win,buff,E_LIST_UP);
				break;
			case EVENT_ACCEPT:
			     if (mode_ok(W(tty),MSG_MODEMASK) &&
			         message && (buff= W(events[GET_EVENT(event)]))) {
#ifdef DEBUG
			        dprintf(e)(stderr,"  accept: %d:  [%s]\r\n",strlen(buff),buff);
			        dprintf(c)(stderr,"  sent %d->%d: %s\r\n",
			                   id_message,W(pid),message);
#endif
			        write_event(win,buff,E_LIST_ACCEPT);
			        }
#ifdef DEBUG
			     else {
			        dprintf(c)(stderr,"%d: can't send [%s] to %s\r\n",
			                   id_message,message?message:"??",W(tty));
			        dprintf(e)(stderr,"  reject accept: %s %s %s\r\n",
			                   mode_ok(W(tty),MSG_MODEMASK) ? "OK" :
			                                  "BAD_MODE",
			                   message?message:"NO MESSAGE",
			                   buff?buff:"NO EVENT");
			        }
#endif
			     break;
#ifdef DEBUG
			default: 
			     dprintf(e)(stderr,"  oops! unknown event\r\n");
#endif
			} /* end switch */
		}
	return;
	}
/*}}}  */
