/* display bitmap files on window */

#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#include "term.h"
#include "bitmap.h"

#define GAP		3		/* space between icons */
#define MAXICONS	500		/* max number of icons */
#define SAVE		2		/* temp bitmap # */

#define dprintf		if(debug)fprintf

struct icon_pos {
   int x,y;				/* position on window */
   int w,h;				/* size of icon */
   char *name;				/* name of icon */
   };

struct icon_pos icons[MAXICONS];
char line[MAXLINE];		/* input buffer */
char cwd[MAXLINE];
int win_x, win_y;		/* window position */
int win_high, win_wide;		/* window size */
int f_wide, f_high;		/* font size */
int debug;
char **first;			/* pntr to first icon */
int invert = 0;

struct menu_entry menu1[] = {
   {"reread","r\n"}, {"next","n\n"}, {"prev","p\n"}, {"quit","q\n"}
   };

struct menu_entry menu2[] = {
   {"reread","r\n"}, {"next","n\n"},    {"quit","q\n"}
   };

struct menu_entry menu3[] = {
   {"reread","r\n"}, {"prev","p\n"},    {"quit","q\n"}
   };

struct menu_entry menu4[] = {
   {"reread","r\n"}, {"quit","q\n"}
   };

int
fill_page(names,icon)
char **names;
struct icon_pos *icon;
   {
   register int count = 0;
   int x=GAP, y=GAP;			/* current icon position */
   int w,h;				/* current icon size */
   int maxh = 0;			/* max y extent of icons */

   if (invert)
      m_func(BIT_NOT(BIT_SRC));
   else
      m_func(BIT_SRC);
   m_clear();
   for(;*names;names++) {
      if (**names == '/')
         m_bitfile(1,*names, &w, &h);
      else {
         sprintf(line,"%s/%s",cwd,*names);
         m_bitfile(1,line, &w, &h);
         }
      dprintf(stderr,"getting %s -> %s",*names,line);
      if (w==0 || h == 0)
         continue;

      if (w + x + 2*GAP > win_wide) {
         x = GAP;
         y += maxh + GAP;
         maxh = 0;
         }

      if (y + h + GAP > win_high) {
         dprintf(stderr,"%s won't fit\n",*names);
         break;
         }

      m_movecursor(x,y);
      m_bitcopyto(x,y,w,h,0,0,0,1);
      count++;
      icon->x = x;
      icon->y = y;
      icon->w = w;
      icon->h = h;
      icon->name = *names;

      icon++;
      x += w + GAP;
      maxh = h>maxh ? h : maxh;
      }
   icon->x = -1;
   m_movecursor(0,win_high-f_high-GAP);
   return(count);
   }

void clean(code)
int code;
   {
	m_bitdestroy(1);
   m_pop();
   m_ttyreset();
   exit(code);
   }

/* border an icon */

void border(icon,how)
struct icon_pos icon;
int how;
   {
   int x=icon.x, y=icon.y, w=icon.w, h=icon.h;
   dprintf(stderr,"border: %d,%d %dx%d\n",x,y,w,h);

   m_func(how);
   m_bitwrite(x-GAP,y-GAP,w+2*GAP,GAP);
   m_bitwrite(x-GAP,y+h,w+2*GAP,GAP);
   m_bitwrite(x-GAP,y,GAP,h);
   m_bitwrite(x+w,y,GAP,h);
   }
   
/* find an icon */

int
find_icon(icon,x,y)
register struct icon_pos *icon;
register int x,y;
   {
   register int i = 0;

   dprintf(stderr,"Looking for: %d %d\n",x,y);

   for(;icon->x != -1;i++,icon++)  {
      if (y>icon->y && x>icon->x &&
                       y < icon->y + icon->h &&
                       x < icon->x + icon->w) {
         dprintf(stderr,"found %d\n",i);
         return(i);
         }
      }
   return(-1);
   }
   
int pop_text(s,x,y)
char *s;		/* text to display */
int x,y;		/* center of window */
   {
   int wide = (strlen(s)<5?5:strlen(s)) * f_wide + 10;
   int high = f_high + 4*GAP;
   int x0 = x - wide/2;
   int y0 = y + f_high;

   if (x0<0)
      x0 = GAP;
   if (x0+wide > win_wide)
      x0 = win_wide-wide-GAP;
   if (y0+high > win_high - f_wide)
      y0 = GAP;

   if (x0 < 0 || y0 + high > win_high - f_wide) {	/* not enough room */
      m_gets(line);
      return(1);
      }

   /* save current window text */

   m_func(BIT_SRC);
   m_bitcopyto(0,0,wide,high,x0,y0,SAVE,0);

   /* draw border */

   m_func(BIT_SET);
   m_bitwrite(x0,y0,wide,high);
   m_func(BIT_CLR);
   m_bitwrite(x0+GAP,y0+GAP,wide-2*GAP,high-2*GAP);
   m_moveprint(x0+2*GAP,y0+(high-f_high)/2+GAP+f_high,s);
   m_movecursor(win_wide,y);

   m_gets(line);

   /* restore data */

   m_func(BIT_SRC);
   m_bitcopyto(x0,y0,wide,high,0,0,0,SAVE);
   m_bitdestroy(SAVE);
   return(0);
   }

int
set_menu(front,back)
int front;
int back;
   {
   register int i;
   if (front>0 && back>0)
      i = 1;
   else if (front > 0)
      i = 3;
   else if (back > 0)
      i = 2;
   else
      i = 4;

   m_selectmenu(i);
   dprintf(stderr,"set menu %d (%d,%d)\n",i,front,back);
   return(i);
   }

int main(argc,argv) int argc; char *argv[];
   {
   int count;				/* number of icons on page */
   int x,y;				/* mouse position */
   int n = -1;				/* current icon */

   ckmgrterm( *argv );

   debug = (int) getenv("DEBUG");

   if (getcwd(cwd,sizeof(cwd))==(char*)0)
   {
      fprintf(stderr,"%s: can't get current directory\n",*argv);
      exit(2);
      }
   
   if (argc <2 || argc > MAXICONS) {
      fprintf(stderr,"Usage: %s icons... (up to %d)\n",*argv,MAXICONS);
      exit(1);
      }

   if (strcmp(argv[1],"-r")==0) {
      invert++;
      argv++;
      argc--;
      }

   first = ++argv;
   argc--;

   m_setup(M_FLUSH);
   m_push(P_BITMAP|P_MENU|P_EVENT|P_FLAGS);
   m_ttyset();

   menu_load(1,4,menu1);
   menu_load(2,3,menu2);
   menu_load(3,3,menu3);
   menu_load(4,2,menu4);

   signal(SIGINT,clean);
   signal(SIGTERM,clean);

   m_setmode(M_ABS);
   m_setmode(M_NOWRAP);
   get_size(&win_x,&win_y,&win_wide, &win_high);
   get_font(&f_wide, &f_high);

   m_setevent(BUTTON_1,"^%p\n");
   m_setevent(BUTTON_1U,"$\n");
   m_setevent(MOVE,"M\n");
   m_setevent(RESHAPED,"R\n");
   m_setevent(REDRAW,"r\n");

   count = fill_page(first,icons);

   if (count == 0) {
      fprintf(stderr,"%s: no files in icon format\n",*--argv);
      clean(3);
      }

   set_menu(first-argv,argc-count-(first-argv)-1);

   dprintf(stderr,"Got %d,%d %dx%d [%dx%d]\n",
           win_x, win_y, win_wide, win_high, f_wide, f_high);

   while(m_gets(line) != NULL) {
      dprintf(stderr,"Got [%s]\n",line);
      switch(*line) {
	 case 'q':
	    clean(0);
	    break;
         case ':':		/* got a message - send current icon back */
            sscanf(line+1,"%d",&x);
            if (n >= 0) {
               m_sendto(x,icons[n].name);
               dprintf(stderr,"sent [%s] to %d\n",
                       icons[n].name,x);
               }
            break;
         case '^':		/* button down */
            sscanf(line+1,"%d %d",&x,&y);
            n = find_icon(icons,x,y);
            if (n >= 0) {
               border(icons[n],BIT_SET);
               sprintf(line,"%s/%s",cwd,icons[n].name);
               m_setevent(NOTIFY,line);
               sprintf(line,"%s (%d x %d)", icons[n].name,
			icons[n].w, icons[n].h);
               pop_text(line, icons[n].x+icons[n].w/2,
                                      icons[n].y+icons[n].h + GAP);
               border(icons[n],BIT_CLR);
               }
            break;
         case 'n':		/* next icons */
               if ((first-argv) + count >= argc)
                  break;
               first += 2*count/3 + 1;
               count = fill_page(first,icons);
               set_menu(first-argv,argc-count-(first-argv)-1);
               break;
         case 'p':		/* previous icons */
               if (first == argv)
                   break;
               first -= 2*count/3 + 1 ;
               if (first < argv)
                  first = argv;
               count = fill_page(first,icons);
               set_menu(first-argv,argc-count-(first-argv)-1);
               break;
         case 'M':		/* window moved */
               get_size(&win_x,&win_y,&win_wide, &win_high);
               break;
         case 'r':		/* window  redrawn  */
               count = fill_page(first,icons);
               set_menu(first-argv,argc-count-(first-argv)-1);
            break;
         case 'R':		/* window shaped*/
               dprintf(stderr,"Got %d,%d %dx%d [%dx%d]\n",
                       win_x, win_y, win_wide, win_high, f_wide, f_high);
               x = win_wide;
               y = win_high;
               get_size(&win_x,&win_y,&win_wide, &win_high);
               if (y != win_high || x>win_wide) {
                  count = fill_page(first,icons);
                  set_menu(first-argv,argc-count-(first-argv)-1);
                  }
            break;
         default:		/* button up (let go of button too fast) */
            break;
         }
      }
   clean(0);
   return (255);
   }
