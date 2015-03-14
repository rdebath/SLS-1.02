/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* macros for writing to mgr terminal emulator */

#ifndef _MGR_TERM_H
#define _MGR_TERM_H

#include <setjmp.h>
#include <signal.h>
#include <stdio.h>

#include "window.h"

#include <termios.h>
#define RAW 0

#define MAXLINE		256	/* maximum line size from mgr */
#define TTYMAX		10	/* stack size for ttyset() */

#define M_FLUSH		0x1	/* autoflush output after each write */
#define M_DEBUG		0x2	/* use stderr instead of /dev/tty mgr i/o */
#define M_MODEOK	0x4	/* assume tty modes are always ok */

#ifndef M_DEVICEIN
#define M_DEVICEIN	"/dev/tty"
#endif

#ifndef M_DEVICEOUT
#define M_DEVICEOUT	"/dev/tty"
#endif

#ifndef M_DELIM
#define M_DELIM	'\005'		/* menu delimiter character */
#endif

#define m_flush()		fflush(m_termout)

#ifndef M_NOFLUSH
#define _m_flsh		,(m_flags&M_FLUSH?m_flush():m_flags)
#else
#define _m_flsh
#endif

/* vi-like stuff */

#define m_addline() (fprintf(m_termout,"%c%c",m_escchar,E_ADDLINE)_m_flsh)

#define m_addlines(n) (fprintf(m_termout,"%c%d%c",m_escchar,n,E_ADDLINE)_m_flsh)

#define m_addchar() (fprintf(m_termout,"%c%c",m_escchar,E_ADDCHAR)_m_flsh)

#define m_addchars(n) (fprintf(m_termout,"%c%d%c",m_escchar,n,E_ADDCHAR)_m_flsh)

#define m_deleteline() (fprintf(m_termout,"%c%c",m_escchar,E_DELETELINE)_m_flsh)

#define m_deletelines(n) (fprintf(m_termout,"%c%d%c",m_escchar,n,E_DELETELINE)_m_flsh)

#define m_deletechar() (fprintf(m_termout,"%c%c",m_escchar,E_DELETECHAR)_m_flsh)

#define m_deletechars(n) (fprintf(m_termout,"%c%d%c",m_escchar,n,E_DELETECHAR)_m_flsh)

#define m_standend() (fprintf(m_termout,"%c%c",m_escchar,E_STANDEND)_m_flsh)

#define m_standout() (fprintf(m_termout,"%c%c",m_escchar,E_STANDOUT)_m_flsh)

#define m_bell() (fprintf(m_termout,"\007")_m_flsh)

#define m_setcursor(n) (fprintf(m_termout,"%c%d%c",m_escchar,n,E_SETCURSOR)_m_flsh)

/* lines */

#define m_line(x0,y0,x1,y1) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c",m_escchar, \
	x0,y0,x1,y1,E_LINE)_m_flsh)

#define m_lineto(to,x0,y0,x1,y1) \
	(fprintf(m_termout,"%c%d,%d,%d,%d,%d%c",m_escchar, \
	x0,y0,x1,y1,to,E_LINE)_m_flsh)

#define m_draw(x,y) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar, \
	x,y,E_LINE)_m_flsh)

#define m_fastdraw(x,y,count,buff) \
	(fprintf(m_termout,"%c%d,%d,%d%c",m_escchar, \
	x,y,count,E_GRUNCH), \
	fwrite(buff,1,count,m_termout)_m_flsh)

#define m_rfastdraw(count,buff) \
	(fprintf(m_termout,"%c%d%c",m_escchar, \
	count,E_GRUNCH), \
	fwrite(buff,1,count,m_termout)_m_flsh)

#define m_aligntext() \
	(fprintf(m_termout,"%c%c",m_escchar, \
	E_LINE)_m_flsh)

#define m_gotext() \
	(fprintf(m_termout,"%c%c",m_escchar, \
	E_GO)_m_flsh)

#define m_go(x,y) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar, \
	x,y,E_GO)_m_flsh)

/* bitblits */

#define m_clear() \
	(fprintf(m_termout,"%c",C_FF)_m_flsh)

#define m_func(func)\
	(fprintf(m_termout,"%c%d%c",m_escchar,func,E_BITBLT)_m_flsh)

#define m_bitwrite(x,y,w,h) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c",m_escchar,x,y,w,h,E_BITBLT)_m_flsh)

#define m_bitwriteto(x,y,w,h,to) \
	(fprintf(m_termout,"%c%d,%d,%d,%d,%d%c", \
	m_escchar,x,y,w,h,to,E_BITBLT)_m_flsh)

#define m_bitcopy(xd,yd,w,h,xs,ys) \
	(fprintf(m_termout,"%c%d,%d,%d,%d,%d,%d%c", \
	m_escchar,xd,yd,w,h,xs,ys,E_BITBLT)_m_flsh)

#define m_bitcopyto(xd,yd,w,h,xs,ys,to,from) \
	(fprintf(m_termout,"%c%d,%d,%d,%d,%d,%d,%d,%d%c", \
	m_escchar,xd,yd,w,h,xs,ys,to,from,E_BITBLT)_m_flsh)

#define m_bitld(w,h,x,y,size) \
	(fprintf(m_termout,"%c%d,%d,%d,%d,%d%c",\
	m_escchar,w,h,x,y,size,E_BITLOAD),m_flush())

#define m_bitldto(w,h,x,y,to,size) \
	(fprintf(m_termout,"%c%d,%d,%d,%d,%d,%d%c",\
	m_escchar,w,h,x,y,to,size,E_BITLOAD),m_flush())

#define m_bitdestroy(n) \
	(fprintf(m_termout,"%c%d%c",\
	m_escchar,n,E_BITCRT),m_flush())

#define m_bitcreate(n,w,h) \
	(fprintf(m_termout,"%c%d,%d,%d%c",\
	m_escchar,n,w,h,E_BITCRT),m_flush())

#define m_bitget(from,size,offset) \
	(fprintf(m_termout,"%c%d,%d,%d%c",\
	m_escchar,from,size,offset,E_BITGET),m_flush())
 
#define m_othersave(id,sub,name) \
	(fprintf(m_termout,"%c%d,%d,%d%c%s",\
	m_escchar,id,sub,strlen(name),E_SMAP,name)_m_flsh)
 
#define m_windowsave(name) \
	(fprintf(m_termout,"%c%d%c%s",\
	m_escchar,strlen(name),E_SMAP,name)_m_flsh)
 
#define m_bitsave(from,name) \
	(fprintf(m_termout,"%c%d,%d%c%s",\
	m_escchar,from,strlen(name),E_SMAP,name)_m_flsh)
 
#define m_bitfromfile(to,name) \
	(fprintf(m_termout,"%c%d,%d%c%s",\
	m_escchar,to,strlen(name),E_GMAP,name)_m_flsh)
 
#define m_highlight(x,y,w,h) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c",\
	m_escchar,x,y,w,h,E_BLEEP),m_flush())
 
#define m_stringto(to,x,y,text) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c%s",\
	m_escchar,to,x,y,strlen(text),E_STRING,text)_m_flsh)

/* other graphic functions */

#define m_circle(x,y,r) \
	(fprintf(m_termout,"%c%d,%d,%d%c",m_escchar,x,y,r,E_CIRCLE)_m_flsh)

#define m_ellipse(x,y,r1,r2) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c",m_escchar,x,y,r1,r2,E_CIRCLE)_m_flsh)

#define m_arc(x,y,x1,y1,x2,y2) \
	(fprintf(m_termout,"%c%d,%d,%d,%d,%d,%d%c",m_escchar,x,y,x1,y1,x2,y2,E_CIRCLE)_m_flsh)

#define m_ellipseto(to,x,y,r1,r2) \
	(fprintf(m_termout,"%c%d,%d,%d,%d,%d%c",m_escchar,x,y,r1,r2,to,E_CIRCLE)_m_flsh)

#define m_rcircle(r) \
	(fprintf(m_termout,"%c%d%c",m_escchar,r,E_CIRCLE)_m_flsh)

#define m_rellipse(r1,r2) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,r1,r2,E_CIRCLE)_m_flsh)

#define m_movemouse(x,y) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,x,y,E_MOUSE)_m_flsh)

#define m_movecursor(x,y) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,x,y,E_MOVE)_m_flsh)

#define m_move(col,row) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,col,row,E_CUP)_m_flsh)

#define m_moveprint(x,y,str) \
	(fprintf(m_termout,"%c%d,%d%c%s",m_escchar,x,y,E_MOVE,str)_m_flsh)

#define m_incr(x) \
	(fprintf(m_termout,"%c%d%c",m_escchar,x,E_MOVE)_m_flsh)

#define m_cleareol() \
	(fprintf(m_termout,"%c%c",m_escchar,E_CLEAREOL)_m_flsh)

#define m_cleareos() \
	(fprintf(m_termout,"%c%c",m_escchar,E_CLEAREOS)_m_flsh)

/* window manipulation */

#define m_movewindow(x,y) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,x,y,E_SHAPE)_m_flsh)

#define m_shapewindow(x,y,dx,dy) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c",m_escchar,x,y,dx,dy,E_SHAPE)_m_flsh)

#define m_font(x) \
	(fprintf(m_termout,"%c%d%c",m_escchar,x,E_FONT)_m_flsh)

#define m_loadfont(n,name) \
	(fprintf(m_termout,"%c%d,%d%c%s", \
	m_escchar,n,strlen(name),E_FONT,name)_m_flsh)

#define m_size(cols,rows) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,cols,rows,E_SIZE)_m_flsh)

#define m_sizeall(x,y,cols,rows) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c", \
         m_escchar,x,y,cols,rows,E_SIZE)_m_flsh)

#define m_scrollregion(first,last) \
	(fprintf(m_termout,"%c%d,%d%c", \
	m_escchar,first,last,E_TEXTREGION)_m_flsh)

#define m_textregion(x,y,wide,high) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c", \
	m_escchar,x,y,wide,high,E_TEXTREGION)_m_flsh)

#define m_textreset() \
	(fprintf(m_termout,"%c%c",m_escchar,E_TEXTREGION)_m_flsh)

/* window creation/destruction */

#define m_newwin(x,y,w,h) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c",m_escchar,x,y,w,h,E_MAKEWIN)_m_flsh)

#define m_halfwin(x,y,w,h) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c",m_escchar,x,y,w,h,E_HALFWIN)_m_flsh)

#define m_destroywin(n) \
	(fprintf(m_termout,"%c%d,0%c",m_escchar,n,E_MAKEWIN)_m_flsh)

#define m_selectwin(n) \
	(fprintf(m_termout,"%c%d%c",m_escchar,n,E_MAKEWIN)_m_flsh)

/* events */

#define m_setevent(event,x) \
	(fprintf(m_termout,"%c%d,%d%c%s", \
	m_escchar,_mapevent(event),strlen(x),E_EVENT,x)_m_flsh)

#define m_clearevent(event) \
	(fprintf(m_termout,"%c%d%c", \
	m_escchar,_mapevent(event),E_EVENT)_m_flsh)

/* message passing */

#define m_sendme(str) \
	(fprintf(m_termout,"%c%d%c%s", \
	m_escchar,strlen(str),E_GIMME,str)_m_flsh)

#define m_sendto(pid,str) \
	(fprintf(m_termout,"%c%d,%d%c%s", \
	m_escchar,pid,strlen(str),E_SEND,str)_m_flsh)

#define m_broadcast(str) \
	(fprintf(m_termout,"%c%d%c%s", \
	m_escchar,strlen(str),E_SEND,str)_m_flsh)

#define m_snarf(str) \
	(fprintf(m_termout,"%c%d%c%s", \
	m_escchar,strlen(str),E_SNARF,str)_m_flsh)

#define m_put() \
	(fprintf(m_termout,"%c%c", \
	m_escchar,E_PUTSNARF)_m_flsh)

/* environment stacking */

#define m_push(mode) \
	(m_envcount++, \
         fprintf(m_termout,"%c%d%c",m_escchar,(mode)|P_CLEAR,E_PUSH) \
         _m_flsh)

#define m_pushsave(mode) \
	(m_envcount++, \
	fprintf(m_termout,"%c%d%c",m_escchar,(mode),E_PUSH)_m_flsh)

#define m_pop() \
	(m_envcount?m_envcount--:0, \
         fprintf(m_termout,"%c%c",m_escchar,E_POP)_m_flsh)

#define m_popall() \
	while(m_envcount--) \
         (fprintf(m_termout,"%c%c",m_escchar,E_POP)_m_flsh)

/* tty mode settings */

#define m_setraw() \
	m_setflags(RAW)

#define m_setnoraw() \
	m_resetflags(RAW)

#define m_setecho()	 \
        m_setflags(ECHO)

#define m_setnoecho() \
        m_resetflags(ECHO)

/* other stuff */

#define m_setmode(mode) \
	(fprintf(m_termout,"%c%d%c",m_escchar,mode,E_SETMODE)_m_flsh)

#define m_dupkey(key) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,M_DUPKEY,key,E_SETMODE)_m_flsh)

#define m_clearmode(mode) \
	(fprintf(m_termout,"%c%d%c",m_escchar,mode,E_CLEARMODE)_m_flsh)

#define m_getinfo(x) \
	(fprintf(m_termout,"%c%d%c",m_escchar,x,E_GETINFO),m_flush())

#define m_whatsat(x,y) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,x,y,E_GETINFO),m_flush())

#define m_get()\
	(m_flush(),fgets(m_linebuf,sizeof(m_linebuf),m_termin))

#define m_getchar()\
	(getc(m_termin))

#define m_gets(buff)\
	(m_flush(),fgets(buff,sizeof(buff),m_termin))

#define m_putchar(c) \
	(putc(c,m_termout)_m_flsh)

#define m_printstr(str) \
	(fprintf(m_termout,"%s",str)_m_flsh)

#define m_setesc(x) \
	(m_escchar = (x))

#define m_resetesc() \
	(m_escchar = ESC)

#define m_sleep() \
	(fprintf(m_termout,"%c%c",m_escchar,E_NULL)_m_flsh)

/* menu stuff */

#define m_selectmenu(n) \
	(fprintf(m_termout,"%c%d%c",m_escchar,n,E_MENU)_m_flsh)

#define m_selectmenu2(n) \
	(fprintf(m_termout,"%c-%d%c",m_escchar,n,E_MENU)_m_flsh)

#define m_nomenu() \
	(fprintf(m_termout,"%c%d%c",m_escchar,999,E_MENU)_m_flsh)

#define m_nomenu2() \
	(fprintf(m_termout,"%c-%d%c",m_escchar,999,E_MENU)_m_flsh)

#define m_loadmenu(n,str) \
	(fprintf(m_termout,"%c%d,%d%c%s",m_escchar,n,strlen(str),E_MENU,str)_m_flsh)

#define m_clearmenu(n) \
	(fprintf(m_termout,"%c%d,0%c",m_escchar,n,E_MENU)_m_flsh)

#define m_linkmenu(parent,item,child,flags) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c", \
	m_escchar,parent,item,child,flags,E_MENU)_m_flsh)

#define m_unlinkmenu(parent,item) \
	(fprintf(m_termout,"%c%d,%d,%d,%c", \
	m_escchar,parent,item,-1,E_MENU)_m_flsh)

#define m_pagemenu(parent,child) \
	(fprintf(m_termout,"%c%d,%d,%d,%c", \
	m_escchar,parent,-1,child,E_MENU)_m_flsh)

#define m_unpagemenu(parent) \
	(fprintf(m_termout,"%c%d,%d,%d,%c", \
	m_escchar,parent,-1,-1,E_MENU)_m_flsh)

/* temporary menu stuff */

#define m_menuitem(menu,item) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,menu,item,E_FOO)_m_flsh)

#define m_menuerase(menu) \
	(fprintf(m_termout,"%c%d%c",m_escchar,menu,E_FOO)_m_flsh)

#define m_menushow(x,y,menu) \
	(fprintf(m_termout,"%c%d,%d,%d%c",m_escchar,x,y,menu,E_FOO)_m_flsh)

#define m_menubar(x,y,menu,item) \
	(fprintf(m_termout,"%c%d,%d,%d,%d%c",m_escchar,x,y,menu,item,E_FOO)_m_flsh)

/* temporary relative character motion */

#define m_right(tenths) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,tenths,10,E_RIGHT)_m_flsh)

#define m_left(tenths) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,tenths,-10,E_RIGHT)_m_flsh)

#define m_up(tenths) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,tenths,10,E_UP)_m_flsh)

#define m_down(tenths) \
	(fprintf(m_termout,"%c%d,%d%c",m_escchar,tenths,10,E_DOWN)_m_flsh)

/* color stuff */

#define m_fcolor(color) \
        (fprintf(m_termout,"%c%d%c",m_escchar,color,E_FCOLOR)_m_flsh)
#define m_bcolor(color) \
        (fprintf(m_termout,"%c%d%c",m_escchar,color,E_BCOLOR)_m_flsh)
#define m_linecolor(op,color) \
        (fprintf(m_termout,"%c%d,%d%c",m_escchar,op,color,E_BITBLT)_m_flsh)

#define _mapevent(z)				((z)==3||(z)==4?2-(z):z)

/* events */

#define BUTTON_1		1		/* end button depressed */
#define BUTTON_2		2		/* middle button depressed */
#define BUTTON_1U		3		/* end button released */
#define BUTTON_2U		4		/* middle button released */
#define RESHAPE			5		/* window was reshaped */
#define RESHAPED		5		/* window was reshaped */
#define REDRAW			6		/* screen was redrawn */
#define REDRAWN			6		/* screen was redrawn */
#define ACTIVATE		7		/* window was activated */
#define ACTIVATED		7		/* window was activated */
#define DEACTIVATE		8		/* window was deactivated */
#define DEACTIVATED		8		/* window was deactivated */
#define COVERED			9		/* window was covered */
#define UNCOVERED		10		/* window was uncovered */
#define MOVE			11		/* window was moved */
#define MOVED			11		/* window was moved */
#define DESTROY			12		/* window was destroyed */
#define ACCEPT			13		/* accept messages */
#define NOTIFY			14		/* set notification */
#define SNARFED			16		/* text was just snarfed */
#define PASTE			17		/* text was just pasted */

/* stuff for setting terminal modes */

#define _m_ttyset()		(m_flags&M_MODEOK?m_flags:m_ttyset())
#define _m_ttyreset()		(m_flags&M_MODEOK?m_flags:m_ttyreset())

/* structure definitions */

#define MENU_SIZE(x)	(sizeof(x)/sizeof(struct menu_entry))

struct menu_entry {
   char *value;		/* this appears in the menu */
   char *action;	/* this gets sent by mgr upon selection */
   };

struct window_data {
   int x,y;		/* origin */
   int w,h;		/* width, height */
   char tty[3];		/* name of controlling tty */
   char status;		/* activation status */
   int num;		/* window number (>0 for client) */
   int setid;		/* window set ID */
   };

struct icon {
   char *name;		/* name of icon */
   int type;		/* always 1 */
   int w;		/* bitmap width */
   int h;		/* bitmap height */
   unsigned char *data;	/* data goes here */
   };

/* global variables */

extern FILE *m_termin;		/* output to window */
extern FILE *m_termout;		/* input from mgr */
extern int m_flags;		/* setup flags */
extern int m_envcount;		/* # of stacked environments */
extern int m_saveenvcount;	/* ??? */
extern char m_escchar;		/* \033, change only for debugging */
extern char m_menuchar;		/* menu field seperator char (\005) */
extern char m_linebuf[MAXLINE];

#ifdef __STDC__
extern void ckmgrterm(char *text);
extern int m_setup(int flags);
extern int get_info(int type, char **list);
extern int get_windata(struct window_data *windatap);
extern int get_eachwin(struct window_data *windatap);
extern int get_eachclientwin(struct window_data *windatap);
extern int get_all(struct window_data *list);
extern int get_client(struct window_data *list);
extern int get_size(int *x, int *y, int *wide, int *high);
extern int get_mouse(int *x, int *y);
extern int get_param(char *host, int *xmax, int *ymax, int *border);
extern int get_cursor(int *x, int *y);
extern int get_colrow(int *cols, int *rows);
extern char *get_termcap(void);
extern int get_font(int *wide, int *high);
extern int m_makewindow(int x, int y, int wide, int high);
extern int is_active(void);
extern char *m_lastline(void);
extern void menu_load(int n, int count, struct menu_entry *text);
extern void m_bitload(int x, int y, int w, int h, char *data);
extern int m_ttyset(void);
extern void m_ttyreset(void);
extern void m_resetflags(int flags);
extern void m_setflags(int flags);
extern int m_bitfile(int bitmapid, char *iconname, int *iconwidthp, int *iconheightp);
extern int parse(char *line, char **fields);
extern void _Catch(int sig);
extern void _Clean(int sig);
#else
extern void ckmgrterm();
extern int m_setup();
extern int get_info();
extern int get_windata();
extern int get_eachwin();
extern int get_eachclientwin();
extern int get_all();
extern int get_client();
extern int get_size();
extern int get_mouse();
extern int get_param();
extern int get_cursor();
extern int get_colrow();
extern char *get_termcap();
extern int get_font();
extern int m_makewindow();
extern int is_active();
extern char *m_lastline();
extern void menu_load();
extern void m_bitload();
extern int m_ttyset();
extern int m_ttyreset();
extern void m_resetflags();
extern void m_setflags();
extern int m_bitfile();
extern int parse();
extern void _Catch();
extern void _Clean();
#endif

/* stuff for restarting upon reshape/ redraw */

extern jmp_buf _env;

#define Ignore()	signal(SIGQUIT,SIG_IGN)

#define Restart()	signal(SIGINT,_Clean), \
			 signal(SIGTERM,_Clean), \
			 signal(SIGQUIT,_Catch), \
			 m_saveenvcount = m_envcount, \
			 m_pushsave(P_EVENT), \
			 m_setevent(RESHAPE,_quit), \
			 m_setevent(REDRAW,_quit), \
			 m_setevent(UNCOVERED,_quit), \
			 setjmp(_env)

#endif
