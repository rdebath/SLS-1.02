/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* routines for writing to mgr terminal emulator */
/*}}}  */

/*{{{  #includes*/
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "term.h"
/*}}}  */

/*{{{  variables*/
FILE	*m_termout;
FILE	*m_termin;
int	m_flags;
int	m_envcount = 0;
int	m_saveenvcount = 0;
char	m_escchar = ESC;
char	m_menuchar = M_DELIM;

jmp_buf _env;

static struct termios tty_save[TTYMAX];
static int tty_cnt = 0;
static char *m_fields[16];

char m_linebuf[MAXLINE];
/*}}}  */

/*{{{  m_setup*/
/******************************************************************************
 *
 *	setup
 */

int
m_setup(flags)
int flags;
   {
   m_flags = flags;

   if (!(m_flags&M_DEBUG)) {
      m_termout = fopen(M_DEVICEOUT,"w");
      m_termin = fopen(M_DEVICEIN,"r");
      }

   if (m_termin == NULL || m_termout == NULL)
      m_flags |= M_DEBUG;

   if (m_flags&M_DEBUG) {
      m_termin = stdin;
      m_termout = stdout;
      }
   return(m_flags);
   }
/*}}}  */
/*{{{  get_info -- get generic window parameters*/
int
get_info(type,list)
int type;
char **list;
   {
   if (type > G_MAX )
      return(-1);
   switch( type ) {
   case G_ALL:
   case G_ALLMINE:
	return(-1);
   }
   _m_ttyset();
   m_getinfo(type);
   m_gets(m_linebuf);
   _m_ttyreset();
   return  parse(m_linebuf,list);
   }
/*}}}  */
/*{{{  get_windata -- read window parameters off of standard input*/
int
get_windata(windatap)
struct window_data *windatap;
   {
   if( parse(m_gets(m_linebuf),m_fields) < 8 )
	return 0;
   windatap->x = atoi(m_fields[0]);
   windatap->y = atoi(m_fields[1]);
   windatap->w = atoi(m_fields[2]);
   windatap->h = atoi(m_fields[3]);
   strcpy(windatap->tty,m_fields[4]);
   windatap->num = atoi(m_fields[5]);
   windatap->status = *m_fields[6];
   windatap->setid = atoi(m_fields[7]);
   return 1;
}
/*}}}  */
/*{{{  get_eachwin -- get window parameters, one window at a time*/
/******************************************************************************
 *
 *	Get window parameters, one window at a time.
 *	Returns 1 if window_data structure has been filled, 0 otherwise.
 *	It is important to call get_eachwin() in a tight loop that doesn't
 *	ever exit, so that all the data is picked up.
 */

int
get_eachwin( windatap )
struct window_data *windatap;
   {
   static int i = 0;

   if( !i ) {
      _m_ttyset();
      m_getinfo(G_ALL);
   }
   i = get_windata( windatap );
   if( !i )
      _m_ttyreset();
   return(i);
   }
/*}}}  */
/*{{{  get_eachclientwin -- get window parameters for client windows, one at a time*/
/******************************************************************************
 *
 *	Get window parameters for the current window set, one window at a time.
 *	Returns 1 if window_data structure has been filled, 0 otherwise.
 *	It is important to call get_eachcleintwin() in a tight loop that
 *	doesn' tever exit, so that all the data is picked up.
 */

int
get_eachclientwin( windatap )
struct window_data *windatap;
   {
   static int i = 0;

   if( !i ) {
      _m_ttyset();
      m_getinfo(G_ALLMINE);
   }
   i = get_windata( windatap );
   if( !i )
      _m_ttyreset();
   return(i);
   }
/*}}}  */
/*{{{  get_all -- get all window parameters*/
/******************************************************************************
 *
 *	Get all window parameters.
 *	NOTE CAREFULLY: The array of window_data structures pointed to by
 *	list must be more than the total number of windows on the screen;
 *	not a robust technique.
 *	get_eachwin() is recommended above this.
 */

int
get_all(list)
struct window_data *list;
   { 
   register int i;

   for(i=0;  get_eachwin( list );  i++ )
      list++;
   return(i);
   }
/*}}}  */
/*{{{  get_client -- get window parameters for client windows*/
/******************************************************************************
 *
 *	Get window parameters for client windows.
 *	NOTE CAREFULLY: The array of window_data structures pointed to by
 *	list must be more than the total number of windows on the screen;
 *	not a robust technique.
 *	get_eachclientwin() is recommended above this.
 */

int
get_client(list)
struct window_data *list;
   { 
   register int i;

   _m_ttyset();
   m_getinfo(G_ALLMINE);
   for(i=0;  get_windata( list );  i++ )
      list++;
   _m_ttyreset();
   return(i);
   }
/*}}}  */
/*{{{  get_size -- get the window size*/
int get_size(x,y,wide,high) int *x, *y, *wide, *high;
   { 
   register int count;

   if ((count = get_info(G_COORDS,m_fields)) >= 4) {
      if (x)
         *x = atoi(m_fields[0]); 
      if (y)
         *y = atoi(m_fields[1]); 
      if (wide)
         *wide = atoi(m_fields[2]); 
      if (high)
         *high = atoi(m_fields[3]); 
      return(count);
      }
   else return(-count);
   }
/*}}}  */
/*{{{  get_mouse -- get the mouse coords*/
int get_mouse(x,y) int *x, *y;
   { 
   register int count;

   if ((count = get_info(G_MOUSE2,m_fields)) >= 3) {
      if (x)
         *x = atoi(m_fields[0]); 
      if (y)
         *y = atoi(m_fields[1]); 
      return(atoi(m_fields[2]));
      }
   else return(-count);
   }
/*}}}  */
/*{{{  get_param -- get system parameters*/
int get_param(host,xmax,ymax,border) char *host; int *xmax, *ymax, *border;
   { 
   register int count;

   if ((count = get_info(G_SYSTEM,m_fields)) >= 4) {
      if (host)
         strcpy(host,m_fields[0]);
      if (xmax)
         *xmax = atoi(m_fields[1]); 
      if (ymax)
         *ymax = atoi(m_fields[2]); 
      if (border)
         *border = atoi(m_fields[3]); 
      return(count);
      }
   else return(-count);
   }
/*}}}  */
/*{{{  get_cursor -- get the cursor position*/
int
get_cursor(x,y)
int *x, *y;

   { 
   register int count;

   if ((count = get_info(G_CURSOR,m_fields)) > 2) {
      if (x)
         *x = atoi(m_fields[0]); 
      if (y)
         *y = atoi(m_fields[1]); 
      return(2);
      }
   else return(-count);
   }
/*}}}  */
/*{{{  get_colrow -- get the window size in rows and columns*/
int
get_colrow(cols,rows)
int *cols, *rows;

   { 
   register int count;

   if ((count = get_info(G_WINSIZE,m_fields)) == 2) {
      if (cols)
         *cols = atoi(m_fields[0]); 
      if (rows)
         *rows = atoi(m_fields[1]); 
      return(2);
      }
   else return(-count);
   }
/*}}}  */
/*{{{  get_termcap -- get the termcap entry*/
char *
get_termcap()
   { 
   _m_ttyset();
   m_getinfo(G_TERMCAP);
   m_gets(m_linebuf);
   _m_ttyreset();
   return(m_linebuf);
   }
/*}}}  */
/*{{{  get_font -- get the font size in pixels*/
int
get_font(wide,high)
int  *wide, *high;

   { 
   register int count, result;

   if ((count = get_info(G_FONT,m_fields)) >= 3) {
      if (wide)
         *wide = atoi(m_fields[0]); 
      if (high)
         *high = atoi(m_fields[1]); 
      result = atoi(m_fields[2]); 
      return(result);
      }
   else return(-count);
   }
/*}}}  */
/*{{{  m_makewindow -- make a new window*/
int
m_makewindow(x,y,wide,high)
int  x,y,wide,high;
   {
   _m_ttyset();
   m_newwin(x,y,wide,high);
   m_gets(m_linebuf);
   _m_ttyreset();
   return(atoi(m_linebuf));
   }
/*}}}  */
/*{{{  is_active -- see if the window is active*/
int
is_active()
   { 
   *m_linebuf = '\0';
   get_info(G_STATUS,m_fields);
   return(*m_linebuf == 'a');
   }
/*}}}  */
/*{{{  m_lastline -- return last line read*/
char *
m_lastline()
   {
   return(m_linebuf);
   }   
/*}}}  */
/*{{{  menu_load -- down load a menu*/
void menu_load(n,count,text)
int n;				/* menu number */
int count;			/* number of menu items */
struct menu_entry *text;	/* menu choices */
   {
   register int i, len;

   /* calculate string lengths */

   len = 2 * count + 1;

   for (i=0;i<count;i++)
       len += strlen(text[i].value) + strlen(text[i].action);
   
   fprintf(m_termout,"%c%d,%d%c%c",m_escchar,n,len,E_MENU,m_menuchar);

   for (i=0;i<count;i++)
      fprintf(m_termout,"%s%c",text[i].value,m_menuchar);

   for (i=0;i<count;i++)
      fprintf(m_termout,"%s%c",text[i].action,m_menuchar);

   m_flush();
   }
/*}}}  */
/*{{{  m_bitload -- download a bitmap*/
void m_bitload(x,y,w,h,data)
int x,y;
int w,h;
register char *data;
   {
   register int size = h * ((w+15)&~15)/8;		/* round to 16 bit boundary */
   m_bitld(w,h,x,y,size);
   while(size-- > 0)
      fputc(*data++,m_termout);
   m_flush();
   }
/*}}}  */
/*{{{  m_ttyset -- set and save the terminal mode*/
int m_ttyset()
{
  int code;
  struct termios buff;
  
  m_flush();
  code=tcgetattr(fileno(m_termout),&tty_save[tty_cnt]);
  buff=tty_save[tty_cnt];
  buff.c_lflag=0;
  buff.c_oflag=0;
  buff.c_iflag=0;
  buff.c_cc[VTIME]=0;
  buff.c_cc[VMIN]=1;
  tcsetattr(fileno(m_termout),TCSANOW,&buff);
  if (tty_cnt < TTYMAX) tty_cnt++;
  return(code);
}
/*}}}  */
/*{{{  m_ttyreset -- restore the terminal mode*/
void m_ttyreset()
{
  if (tty_cnt) tty_cnt--;
  m_flush();
  tcsetattr(fileno(m_termout),TCSANOW,&tty_save[tty_cnt]);
}
/*}}}  */
/*{{{  m_resetflags -- change the terminal modes*/
void m_resetflags(flags) int flags;
{
  struct termios buff;
  tcgetattr(fileno(m_termin),&buff);
  if (buff.c_lflag & flags) {
    buff.c_lflag &= ~flags;
    m_flush();
    tcsetattr(fileno(m_termin),TCSANOW,&buff);
  }
}
/*}}}  */
/*{{{  m_setflags*/
void m_setflags(flags) int flags;
{
  struct termios buff;
  tcgetattr(fileno(m_termin),&buff);
  if (!( buff.c_lflag & flags)) {
    buff.c_lflag |= flags;
    m_flush();
    tcsetattr(fileno(m_termin),TCSANOW,&buff);
  }
}
/*}}}  */
/*{{{  m_bitfile -- cause server to load a bitmap from a file*/
/**
	Given a bitmap id and an icon name,
	have mgr load that icon into that bitmap, returning the icon width
	and height via the given integer pointers.
	Return a positive number if successful.
	If the icon is not loaded, set the width and height values to 0 and
	return 0.
*/
int
m_bitfile( bitmapid, iconname, iconwidthp, iconheightp )
int	bitmapid;
char	*iconname;
int	*iconwidthp,
	*iconheightp;
   {
	*iconwidthp = *iconheightp = 0;
	m_bitfromfile( bitmapid, iconname );
	m_flush();
	return( sscanf( m_get(), "%d %d", iconwidthp, iconheightp ) == 2 );
   }
/*}}}  */
/*{{{  parse -- parse a line into fields*/
#ifndef iswhite
#define iswhite(x)	((x)==' ' || (x)=='\t')
#endif

int
parse(line,fields)
register char *line;
register char **fields;
   {
   int inword = 0;
   int count = 0;
   char *start;
   register char c;

   for(start = line;(c = *line) && c != '\n';line++)
      if (inword && iswhite(c)) {
         inword = 0;
         *line = '\0';
         *fields++ = start;
         count++;
         }
      else if (!inword && !iswhite(c)) {
         start = line;
         inword = 1;
         }

   if (inword) {
      *fields++ = start;
      count++;
      if (c == '\n')
         *line = '\0';
      }
   *fields = (char *) 0;
   return(count);
   }
/*}}}  */
/*{{{  _Catch*/
/******************************************
 *	stuff for restarting
 */

void _Catch(sig) int sig;
   {
   ioctl(fileno(m_termin),TCIOFLUSH,0);
   longjmp(_env,1);
   }
/*}}}  */
/*{{{  _Clean*/
void _Clean(sig) int sig;
{
   while(m_saveenvcount < m_envcount) m_pop();
   exit(1);
}
/*}}}  */
