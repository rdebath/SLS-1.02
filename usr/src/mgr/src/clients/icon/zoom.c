/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* icon editor -- version I (single window) */

#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stdarg.h>

#include "term.h"
#include "bitblit.h"

char *bsd_sprintf(char *s, char *fmt, ...)
{
va_list ap;

	va_start(ap, fmt);
	vsprintf(s, fmt, ap);
	return s;
}

#define sprintf		bsd_sprintf

/* general defines */

#define GAP	2		/* general purpose gap */
#define TITLE	20		/* cols needed for title */
#define MAXMARK	7		/* max # of text items on status line */
#define MIN	4		/* min number of pixels in a bitmap */
#define SLEEP	5		/* time it takes for a message to go away */
#define SETMSG	1		/* set the message */
#define MAX	100		/* max # of pixels in a bitmap */
#define FILES	100		/* max # of files to edit at once */

/* bitmap functions */

#define SET	0		/* bit set */
#define CLEAR	1		/* clear */
#define TOGGLE	2		/* toggle */
#define ON	4		/* bit is on */
#define OFF	0		/* bit is off */

/* zoom options */

#define YANK	1		/* yank a region */
#define PUT	2		/* put a previously yanked region */
#define SHRINK	3		/* make icon smaller */
#define GROW	4		/* make icon bigger */
#define SHIFT	5

/* title fields */

#define T_FUNC	0		/* raster op functions */
#define T_OP	1		/* sweep otions */
#define T_SIZE	2		/* bitmap size */
#define T_NAME	3		/* file name */

/* menu names */

#define M_MODES		1	/* set/clear/toggle */
#define M_OPTIONS	2	/* yank/put/shrink/grow/ */
#define M_SIZE		3	/* none */
#define M_EDIT		4	/* save/get */
#define M_PUT		5	/* put functions */
#define M_FILES		6	/* name of files to edit */

#define dprintf	if(debug)fprintf

#define SET_FUNC(n) \
	(n!=func ? m_func(func = n) : n);
#define SWAP(x,y) \
	(code=x,x=y,y=code)
#define MARK(i) \
	((i)>=0 ? marks[i] : 0)

#ifndef Min
#define Min(x,y)	((x)<(y)?(x):(y))
#endif
#ifndef Max
#define Max(x,y)	((x)>(y)?(x):(y))
#endif

#define INVERT(i) \
	m_bitwrite(x0+MARK(i-1),0,MARK(i)-MARK(i-1),font_high);

#define W(map)	BIT_WIDE(map)
#define H(map)	BIT_HIGH(map)

/* menus */

struct menu_entry modes[] = {
   {"set", "s0\n"},
   {"clear","s1\n"},
   {"toggle","s2\n"},
   {"grid","x\n"},
   };

struct menu_entry edit[] = {
   {"save","f\n"},
   {"get","g\n"},
   {"yank","y\n"},
   {"quit","Q\n"},
   };

struct menu_entry resize[] = {
   {"resize","+\n"},
   };

struct menu_entry options[] = {
   {"yank","F1\n"},
   {"put","F2\n"},
   {"shrink","F3\n"},
   {"grow","F4\n"},
   {"shift","F5\n"},
   {"fix window","w\n"},
   {"undo","u\n"},
   };

struct menu_entry functions[] = {
   {"copy","P0\n"},
   {"paint","P1\n"},
   {"mask","P2\n"},
   {"xor","P3\n"},
   {"grid","x\n"},
   };

struct menu_entry sizes[] = {
   {"normal cursor","16,16"},
   {"small icon","48,48"},
   {"normal icon","64,64"},
   };

struct menu_entry files[FILES];

char name[80];				/* name of icon */

/* function codings for put */

int func_put[] = {
   BIT_SRC, BIT_SRC|BIT_DST, BIT_SRC&BIT_DST, BIT_SRC^BIT_DST
   };

int func_zoom[] = {
   BIT_SRC^BIT_DST, BIT_SRC&BIT_NOT(BIT_DST), BIT_DST&BIT_NOT(BIT_SRC), BIT_SRC
   };


int marks[MAXMARK];		/* positions demarcating labels */
int win_wide, win_high;		/* window size */
int win_x, win_y;		/* window location */
int font_wide, font_high;	/* font size */
int x0,y0;			/* starting location of icon*/
int func;			/* current mgr raster function */
int debug=0;
char *title[MAXMARK];		/* title goes here */
int xmax,ymax,border;		/* screen parameters */
char *prog;

char *str_save();
void clean();
void message();
char *get_str();
BITMAP *read_icon(), *set_undo();
int get_scale(), Do_title(), write_icon();
void	zoom(), do_bit(), 
	draw_grid();

int main(argc,argv)
int argc;
char **argv;
{

   register int i, c;
   int w, h;			/* bitmap size */
   int x,y;			/* scale factors */
   int mx,my;			/* mouse position */
   int bitx, bity;		/* bit position in bitmap */
   int lastx, lasty;		/* prev. bit position in bitmap */
   int done=0;			/* exit flag */
   int code;			/* return codes */
 
   /* state flags */

   int menu = -1;		/* current menu selected */
   int grid = 0;		/* grid state */
   int mode = TOGGLE;		/* edit mode */
   int function = 0;		/* function flag */
   int put = 0;			/* put mode */

   /* misc */

   int file_count;		/* number of files in file menu */
   char *pntr;			/* temp char pntr */
   char dims[12];		/* string buffer for icon dims */
   char line[512];		/* input buffer */


   BITMAP *map;				/* what your editting */
   BITMAP *new, *temp;			/* temp bitmap */
   BITMAP *yanked=BIT_NULL;		/* yanked bitmap */
   BITMAP *last=BIT_NULL;		/* bitmap to undo */

   ckmgrterm( *argv );
   if (getenv("DEBUG")) debug = 1;

   if (argc <2) {
      fprintf(stderr,"Usage: %s files...\n",argv[0]);
      exit(1);
      }
   prog = *argv;

   /* setup mgr */

   m_setup(M_MODEOK);
   m_ttyset();
   m_push(P_FLAGS|P_MENU|P_EVENT);

   signal(SIGINT,clean);
   signal(SIGTERM,clean);
   signal(SIGALRM,message);

   m_setmode(M_ABS);
   m_setmode(M_NOWRAP);
   SET_FUNC(BIT_NOT(BIT_DST));
   get_font(&font_wide, &font_high);
   get_size(&win_x,&win_y,&win_wide,&win_high);
   get_param(0,&xmax,&ymax,&border);

   menu_load(1,MENU_SIZE(modes),modes);
   menu_load(2,MENU_SIZE(options),options);
   menu_load(3,MENU_SIZE(resize),resize);
   menu_load(4,MENU_SIZE(edit),edit);
   menu_load(5,MENU_SIZE(functions),functions);

   for(i=1;i<argc && i<= FILES-3;i++) {		/* save room for new names */
      pntr = strrchr(argv[i],'/');
      pntr = pntr ? pntr++ : argv[i];
      files[i-1].value = str_save(pntr,"");
      files[i-1].action = str_save(argv[i],"\n");
   }
   file_count = i-1;

   m_nomenu();

   /* get mgr events */

   m_setevent(BUTTON_1,"(%r)\n");
   m_setevent(BUTTON_2,"[%p]\n");
   m_setevent(BUTTON_2U,"$\n");
   m_setevent(RESHAPED,"S\n");
   m_setevent(REDRAW,"R\n");
   m_setevent(MOVE,"M\n");
   
   /* read in icon */ 

   strcpy(name,argv[1]);
   if ((map = read_icon(argv[1],0))  == BIT_NULL) {
      pntr=get_str("Enter icon size (xxx,yyy):\n",
            win_wide/2,win_high/2,sizes,MENU_SIZE(sizes));
      sscanf(pntr,"%d,%d",&w,&h);
      if (w>MAX || h>MAX || w<MIN || h<MIN) {
         fprintf(stderr,"%s: Wrong size, try again\n",prog);
         clean(1);
         }
      map = bit_alloc(w,h,BIT_NULL,1);
      bit_blit(map,0,0,w,h,BIT_CLR,0,0,0);
      }

   /* setup & display icon */
	
   x0 = 0;
   y0 = font_high + 2*GAP;
   get_scale(&x,&y,map);

   m_clear();
   title[T_FUNC] = modes[mode].value;
   title[T_OP] = options[function].value;
   title[T_SIZE] = sprintf(dims,"%d x %d",W(map),H(map));
   title[T_NAME] = name;
   title[4] = NULL;
   Do_title(title);

   zoom(map,x0,y0,x,y,SET);

   /* process menu and mouse hits */

   m_flush();
   while (!done && m_gets(line) != NULL) {
     dprintf(stderr,"main loop got: %s",line);
     menu = -1;
     m_nomenu();
     switch(c = *line) {
        case '[':				/* got button 1 hit */
           sscanf(line+1,"%d %d]",&mx,&my);
           dprintf(stderr,"Got %d,%d\n",mx,my);

           menu = -1;
           if (my < y0) {			/* button 1 hit on menu */
              for(i=0;i<4;i++)
                  if (mx<marks[i]) {
                     menu = i+1;
                     break;
                     }
              }

           else {
              lastx = -1;
              last = set_undo(last,map);
              while (*line != '$') {		/* button 1 hit on bitmap */
                 bitx = (mx-GAP)/x;
                 bity = (my-2*GAP-font_high)/y;
                 dprintf(stderr,"read (%d/%d,%d/%d) was (%d,%d) %s",
                         bitx,W(map),bity,H(map),lastx,lasty,line);

                 if (lastx == -1)  {
                    mode = bit_on(map,bitx,bity) ? CLEAR : SET;
                    title[T_FUNC]=modes[mode].value;
                    Do_title(title);
                    }
   
                 if (bitx==lastx && bity == lasty) {
                    dprintf(stderr,"same bit %d,%d\n",bitx,bity);
                    }
                 else if (bitx < W(map) && bity < H(map)) {
                    do_bit(map,x,y,bitx,bity,mode);
                    }
                 else {
                    dprintf(stderr,"? bit %d,%d\n",bitx,bity);
                    }

                 lastx=bitx; lasty=bity;
                 m_getinfo(G_MOUSE2);
                 m_flush();
                 m_gets(line);
                 sscanf(line,"%d %d",&mx,&my);
                 if (debug) sleep(1);
                 }
              }

           /* set menu (if any) */

           if (menu>0) {
              dprintf(stderr,"selecting menu %d ->",menu); fflush(stderr);
              if (menu==M_MODES && function==PUT)
                 menu=M_PUT;
              m_selectmenu(menu);
              dprintf(stderr," %d [%d]\n",menu,function);
              }
           break;
        case '(':			/* swept area  */
           {
           int rx1,ry1,rx2,ry2;		/* rect coords */

           sscanf(line+1,"%d %d %d %d)",&rx1,&ry1,&rx2,&ry2);

           if (rx1>rx2) SWAP(rx1,rx2);
           if (ry1>ry2) SWAP(ry1,ry2);

           rx1 = Max(rx1,x0);
           ry1 = Max(ry1,y0);

           rx1 = (rx1-GAP)/x;
           rx2 = (rx2-GAP)/x;
           ry1 = (ry1-2*GAP-font_high)/y;
           ry2 = (ry2-2*GAP-font_high)/y;

           rx2 = Min(rx2,W(map));
           ry2 = Min(ry2,H(map));

           w = rx2 - rx1;
           h = ry2 - ry1;
           new = bit_create(map,rx1,ry1,w,h);
           dprintf(stderr,"Extract %d,%d %dx%d code: %s\n",
                           rx1,ry1,w,h,new?"YES":"NO");

           last = set_undo(last,map);

           if (function) {
              INVERT(T_OP);
              dprintf(stderr,"Doing function %d\n",function);
              switch(function) {
                 case YANK:
                    if (yanked)
                       bit_destroy(yanked);
                    yanked = bit_alloc(W(new),H(new),BIT_NULL,1);
                    bit_blit(yanked,0,0,
                            W(new),H(new),BIT_SRC,new,0,0);
                    dprintf(stderr,"yanked: %s\n",yanked?"YES":"NO");
                    if (!yanked)
                       message(SETMSG,"Can't yank bitmap");
                    else
                       message(SETMSG,sprintf(line,"Yanked bitmap %d x %d",
                               W(new),H(new)));
                    break;
                 case PUT:
                    if (!yanked) {
                       message(SETMSG,"Nothing to PUT");
                       break;
                       }

                    w = Min(W(yanked),W(new));
                    h = Min(H(yanked),H(new));

                    if (w<1 || h < 1) {
                       message(SETMSG,"Put where??");
                       break;
                       }

                    /* setup and zoom bitmap */

                    temp = bit_alloc(w,h,BIT_NULL,1);
                    bit_blit(temp,0,0,w,h,BIT_SRC,new,0,0);
                    bit_blit(temp,0,0,w,h,func_zoom[put],yanked,0,0);
                    zoom(temp,x0+rx1*x,y0+ry1*y,x,y,SET);
                    bit_destroy(temp);

                    bit_blit(new,0,0,w,h,func_put[put],yanked,0,0);
                    title[T_FUNC] = modes[mode].value;
                    Do_title(title);
                    dprintf(stderr,"put:%dx%d at %d,%d [%d]\n",
                            w,h,rx1,ry1,put);
                    break;
                 case SHRINK:
                    w = W(new);
                    h = H(new);
                    if (w<MIN || h<MIN) {
                       message(SETMSG,"Icon would be too small");
                       break;
                       }
                    temp = bit_alloc(w,h,BIT_NULL,1);
                    bit_blit(temp,0,0,w,h,BIT_SRC,new,0,0);
                    bit_destroy(new);
                    bit_destroy(map);
                    new = BIT_NULL;
                    map = temp;
                    get_scale(&x,&y,map);
                    m_clear();
                    zoom(map,x0,y0,x,y,SET);
                    if (grid)
                       draw_grid(map,x0,y0,x,y);
                    title[T_SIZE] = sprintf(dims,"%d x %d",
                            W(map),H(map));
                    Do_title(title);
                    break;
                 case GROW:
                    w = W(map)*W(map)/W(new);
                    h = H(map)*H(map)/H(new);
                    if (w>MAX || h>MAX) {
                       message(SETMSG,"Icon would be too big");
                       break;
                       }
                    temp = bit_alloc(w,h,BIT_NULL,1);
                    fprintf(stderr,"growing to %d , %d\n",w,h);
                    bit_blit(temp,0,0, w,h,BIT_CLR,0,0,0);
                    bit_blit(temp,rx1,ry1,W(map),H(map),BIT_SRC,map,0,0);
                    bit_destroy(new);
                    bit_destroy(map);
                    new = BIT_NULL;
                    map = temp;
                    get_scale(&x,&y,map);
                    m_clear();
                    zoom(map,x0,y0,x,y,SET);
                    if (grid)
                       draw_grid(map,x0,y0,x,y);
                    title[T_SIZE] = sprintf(dims,"%d x %d",
                            W(map),H(map));
                    Do_title(title);
                    break;
                 case SHIFT:
                    bit_blit(map,0,0,W(map),H(map)-1,BIT_SRC,map,0,1);
                    zoom(map,x0,y0,x,y,SET);
                    if (grid)
                       draw_grid(map,x0,y0,x,y);
                    break;
                 }
              function = 0;
              }
           else {
              if (new && mode==TOGGLE) {
                 bit_blit(new,0,0,W(new),H(new),
                          BIT_NOT(BIT_DST),0,0,0);
                 m_bitwrite(x0+rx1*x,y0+ry1*y,x*W(new),
                            y*H(new));
                 }
              else if (new) {
                 zoom(new,x0+rx1*x,y0+ry1*y,x,y,mode==SET?CLEAR:SET);
                 bit_blit(new,0,0,W(new),H(new),
                          mode==SET?BIT_SET:BIT_CLR,0,0,0);
                 }
              if (new)
                 bit_destroy(new);
              }
           }
           break;
        case '$':			/* button up */
           dprintf(stderr,"done\n");
           menu = -1;
           m_nomenu();
           break;
        case 's':			/* set bit mode */
           c = *(line+1);
           if (c>='0' && c<='2')
              code = c - '0';
           if (mode != code){
              mode = code;
              title[T_FUNC]=modes[mode].value;
              Do_title(title);
              }
           break;
        case '+':			/* specify bitmap size */
           pntr=get_str("Enter new size (xxx,yyy):\n",
                     mx,my,sizes,MENU_SIZE(sizes));
           sscanf(pntr,"%d,%d",&w,&h);
           if (w>MAX || h>MAX || w<MIN || h<MIN) {
              message(SETMSG,"Sorry, invalid size");
              break;
              }
           temp = bit_alloc(w,h,BIT_NULL,1);
           fprintf(stderr,"resizing to %d , %d\n",w,h);
           mx = w>W(map) ? (w - W(map))/2 : 0;
           my = h>H(map) ? (h - H(map))/2 : 0;
           bit_blit(temp,0,0,w,h,BIT_CLR,0,0,0);
           bit_blit(temp,mx,my,W(map),H(map),BIT_SRC,map,0,0);
           bit_destroy(map);
           map = temp;
           get_scale(&x,&y,map);
           m_clear();
           zoom(map,x0,y0,x,y,SET);
           if (grid)
              draw_grid(map,x0,y0,x,y);
           title[T_SIZE] = sprintf(dims,"%d x %d",W(map),H(map));
           Do_title(title);
           if (function)
              INVERT(T_OP);
           break;
        case 'f':			/* save file */
           if ((pntr=get_str("Enter file name:\n",mx,my,files,file_count))
                                  && *pntr) {
              if (write_icon(pntr,map) && strcmp(name,pntr)!=0) {
                 title[T_NAME] = strcpy(name,pntr);
                 Do_title(title);
                 if (function) INVERT(T_OP);
              
                 /* add new name to menu */

                 for(i=0;i<file_count;i++)
                    if (strcmp(files[i].value,name)==0)
                       break;
                 if (i==file_count && file_count+1 < FILES) {
                    files[file_count].value = str_save(name,"");
                    files[file_count].action = str_save(name,"\n");
                    file_count++;
                    }
                 }
              }
           else {
              message(SETMSG,"No file saved");
              }
           break;
        case 'g':			/* get file */
           {
           pntr = get_str("Enter file name:\n",mx,my,files,file_count);
           if (pntr && *pntr && (new = read_icon(pntr,0))  != BIT_NULL) {
              bit_destroy(map);
              map=new;
              m_clear();
              code = get_scale(&x,&y,map);
              zoom(map,x0,y0,x,y,SET);
              title[T_SIZE] = sprintf(dims,"%d x %d",W(map),H(map));
              title[T_NAME] = strcpy(name,pntr);
              Do_title(title);
              if (grid)
                 draw_grid(map,x0,y0,x,y);
              }
           }
           break;
        case 'y':			/* yank file to buffer */
           pntr = get_str("Enter file name:\n",mx,my,files,file_count);
           if (pntr && *pntr && (new = read_icon(pntr,0))  != BIT_NULL) {
              if (yanked)
                 bit_destroy(yanked);
              yanked = new;
              new = BIT_NULL;
              message(SETMSG,sprintf(line,"Yanked %s (%d x %d)",
                      pntr,W(yanked),H(yanked)));
              if (function == YANK) {
                 INVERT(T_OP); 
                 function = 0;
                 }
              }
           else
              message(SETMSG,"Can'y yank file");
           break;
        case 'w':			/* reshape window to icon size  */
           code = Min(x,y);
           m_shapewindow(win_x,win_y,W(map)*code+GAP+2*border,
                        W(map)*code+2*font_high+GAP*3 + 2*border);
           m_sendme("S\n");
           break;
        case 'P':			/* set put mode  */
           code = *(line+1) - '0';
           if (code<0 || code > 9)
              code = 0;
           if (put != code) {
              put = code;
              title[T_FUNC]=functions[put].value;
              Do_title(title);
              INVERT(T_OP);
              }
           break;
        case 'F':			/* set option flags  */
           code = *(line+1) - '0';

           if (code<0 || code > 9)
              break;
           if (code == PUT && !yanked)
              break;

           if (code == function && function == PUT) {
              function = 0;
              title[T_FUNC] = modes[mode].value;
              }
           else if (code == function)
              function = 0;
           else {
              function = code;
              title[T_OP] = options[function-1].value;
              dprintf(stderr,"setting function =  %d\n",code);
              if (function == PUT)
                 title[T_FUNC]=functions[put].value;
              }
           Do_title(title);
           if (function)
              INVERT(T_OP);
           break;
        case 'u':			/* undo */
           if (!last)
              break;

           /* get ready to undo the undo */

           temp = map;
           map = last;
           last = temp;

           /* make changes in situ */

           if (W(last)==W(map) && H(last)==H(map)) {
              temp = bit_alloc(W(map),H(map),BIT_NULL,1);
              bit_blit(temp,0,0,W(map),H(map),BIT_SRC,map,0,0);
              bit_blit(temp,0,0,W(map),H(map),BIT_SRC^BIT_DST,last,0,0);
              zoom(temp,x0,y0,x,y,SET);
              bit_destroy(temp);
              }

           /* redoit all */

           else {
              get_scale(&x,&y,map);
              m_clear();
              zoom(map,x0,y0,x,y,SET);
              if (grid)
                 draw_grid(map,x0,y0,x,y);
              title[T_SIZE] = sprintf(dims,"%d x %d", W(map),H(map));
              Do_title(title);
              }
           break;
        case 'M':			/* move */
           get_size(&win_x,&win_y,0,0);
           break;
        case 'R':			/* redraw */
           m_clear();
           zoom(map,x0,y0,x,y,SET);
           Do_title(title);
           if (grid)
              draw_grid(map,x0,y0,x,y);
           if (function)
              INVERT(T_OP);
           break;
        case 'S':			/* reshape */
              {
              int lastx = x;
              int lasty = y;

              get_font(&font_wide, &font_high);
              code = get_scale(&x,&y,map);
              if (code>0 || lastx != x || lasty != y) {
                 m_clear();
                 zoom(map,x0,y0,x,y,SET);
                 if (grid)
                    draw_grid(map,x0,y0,x,y);
                 }
              Do_title(title);
              if (function)
                 INVERT(T_OP);
              }
              break;
        case 'Q':			/* quit */
           done++;
           m_gets(line);		/* eat the "$" */
           break;
        case 'x':			/* toggle grid */
           draw_grid(map,x0,y0,x,y);
           grid = 1-grid;
           break;
        default:
           break;
        }
     m_flush();
     }
   clean(0);
   return 0;
   }

/* grid a bitmap onto the window */

void
draw_grid(map,x,y,x_scale,y_scale)
register BITMAP *map;		/* bitmap to zoom */
int x,y;			/* screen location to start */
int x_scale, y_scale;		/* scale factors (>1) */
{
   register int sx,sy,dx,dy;	/* current src, dst coords */

   for(dy=y,sy=0;sy<H(map);sy++,dy+=y_scale)
            m_bitwrite(x,dy,W(map)*x_scale,1);
   for(dx=x,sx=0;sx<W(map);sx++,dx+=x_scale)
            m_bitwrite(dx,y,1,H(map)*y_scale);
} 

/* zoom a bitmap onto the window */

#define X_ON(x)	(x != -1)

void
zoom(map,x,y,x_scale,y_scale,how)
register BITMAP *map;		/* bitmap to zoom */
int x,y;			/* screen location to start */
int x_scale, y_scale;		/* scale factors (>1) */
int how;			/* 1==on bits  0==off bits */
{
   register int sx,sy,dx,dy;	/* current src, dst coords */
   int on, count=0, set_x;

   for(dy=y,sy=0;sy<H(map);sy++,dy+=y_scale) {
      for(count=0,dx=x,sx=0;sx<W(map);sx++,dx+=x_scale) {
         on = how==SET ? (bit_on(map,sx,sy)) : !(bit_on(map,sx,sy));
         if (on && count)
            count ++;
         else if (on) {
            count++;
            set_x = dx;
            }
         else if (count) {
            m_bitwrite(set_x,dy,count * x_scale,y_scale);
            count = 0;
            }
         }
      if (count)
         m_bitwrite(set_x,dy,count * x_scale,y_scale);
      }
} 


void
clean(n)
int n;
{
   m_moveprint(0,win_high,"done...");
   m_popall();
   m_ttyreset();
   exit(n);
}

void
message(how,s)
int how;
char *s;
   {
   alarm(0);
   if (how==SETMSG) {
      m_moveprint(0,win_high,s);
      m_flush();
      dprintf(stderr,"Setting message [%s]\n",s);
      alarm(SLEEP);
      }
   else {
      m_movecursor(0,win_high);
      dprintf(stderr,"clearing message\n");
      }
   m_cleareol();
   m_movecursor(win_wide,win_high);
   m_flush();
}

int Do_title(args)
char **args;
{
   register int i, count;
   int len[MAXMARK];		/* label sizes */
   int sum;			/* total label width */
   int x = 0;			/* starting label position */
   int y = font_high;		/* starting label position */
   register char **label=args;

   SET_FUNC(BIT_SET);
   m_movecursor(0,y);
   m_cleareol();

   for(sum=count=0;*label;count++,label++)
      sum += (len[count] = strlen(*label) * font_wide + GAP);
   for(i=0;i<count;i++) {
      marks[i] = MARK(i-1) + len[i] * win_wide / sum;
      if (i+1 == count)
         marks[i] = win_wide;		/* fix rounding error */
      dprintf(stderr,"%s (%d)at %d=>%d ",
              args[i],len[i],MARK(i-1),MARK(i));
      if (MARK(i) - MARK(i-1) > len[i])
         m_moveprint((MARK(i)+MARK(i-1)- len[i])/2,y,args[i]);
      m_bitwrite(marks[i],0,GAP,y);
      }
   dprintf(stderr,"(%d)\n",win_wide);

   m_bitwrite(0,y,win_wide,GAP);
   m_movecursor(win_wide+font_wide,y);
   SET_FUNC(BIT_NOT(BIT_DST));
   
   return(count);
}

/* get a user string */


char *
get_str(s,x,y,menu,count)
char *s;			/* text to display */
int x,y;			/* center of window */
struct menu_entry *menu;	/* menu to down load */
int count;			/* # of menu items */
   {
   static char input[128];
   int wide = Max(15,strlen(s)) * font_wide + 2*border;
   int high = 2*(font_high + border);
   int x0 = win_x + x - wide/2;
   int y0 = win_y + y + font_high;
   int win;
   char *pntr;

   if (x0<0)
      x0 = GAP;
   if (x0+wide > xmax)
      x0 = xmax-wide-GAP;
   if (y0+high > ymax)
      y0 = ymax-high-GAP;

   dprintf(stderr,"Making %d,%d %dx%d\n",x0,y0,wide,high);
   message(SIGALRM);		/* turn of any pending message */

   m_newwin(x0,y0,wide,high);
   m_flush();
   m_gets(input);
   if (*input == '$') {		/* button already let go */
      dprintf(stderr,"$ in makewin\n");
      m_gets(input);
      }

   dprintf(stderr,"makewin returns %s",input);
   win = atoi(input);
   dprintf(stderr,"Created %d\n",win);

   *input = '\0';
   if (win) {
      m_selectwin(win);
      m_setevent(DEACTIVATED,"\n");
      m_setevent(DESTROY,"\n");
      m_setevent(RESHAPE,"\n");
      m_setevent(BUTTON_1,"%n\n");
      m_setevent(ACCEPT,"%m\n");
      if (count > 0) {
         menu_load(1,count,menu);
         m_selectmenu(1);
         }
      m_printstr(s);
      dprintf(stderr,"prompt: %s ...",s);
      fflush(stderr);
      m_flush();
      m_ttyreset();
      m_gets(input);

      pntr = input;
      if (pntr=strchr(input,' '))
         *pntr='\0';
      if (pntr=strchr(input,'\n'))
         *pntr='\0';

      m_ttyset();
      dprintf(stderr,"Got: %s\n",input); fflush(stderr);
      m_selectwin(0);
      m_destroywin(win);
      m_flush();
      } 
   else
      message(SETMSG,"Sorry, Can't make prompt window");
   return(input);
   }

/* read in icon */ 

BITMAP *
read_icon(name)
char *name;				/* name of icon file */
   {
   FILE *fp;				/* fp to read bitmap from */
   BITMAP *map, *bitmapread();
   char tmp[100];

   if ((fp = fopen(name,"r")) == NULL ) {
      message(SETMSG,sprintf(tmp,"Can't find %s",name));
      return(BIT_NULL);
      }

   if( !( map = bitmapread(fp) ) ) {
      fclose(fp);
      message(SETMSG,sprintf(tmp,"%s is not an icon or is damaged",name));
      return(BIT_NULL);
      }

   fclose(fp);
   return(map);
   }

int
get_scale(x,y,map)
register int *x, *y;
BITMAP *map;
{
   char line[256];
   int w = W(map);
   int h = H(map);
   int count;

   for(count=0;;count++) {
      get_size(&win_x,&win_y,&win_wide,&win_high);
      *x = (win_wide - x0)/w;
      *y = (win_high - y0 - font_high)/h;
      if (*x>=1 && *y>=1) 
         break;
      m_clear();
      m_clearmode(M_NOWRAP);
      m_printstr("Window is too small\n");
      m_gets(line); 
      }
   if (count)
      m_setmode(M_NOWRAP);
   return(count);
}


int
write_icon(name,map)
char *name;
BITMAP *map;
{
   FILE *fp = fopen(name,"w");
   char tmp[100];

   if (fp == NULL  ||  !bitmapwrite(fp,map,NEW_BHDR)) {
      dprintf(stderr,"Can't write file %s\n",name);
      message(SETMSG,sprintf(tmp,"Can't write file %s",name));
      return(0);
      }
   fclose(fp);
   return(1);
}

/* do mode to bit, fix up display */

void
do_bit(map,x,y,bitx,bity,mode)
BITMAP *map;
int x,y;		/* scale factors */
int bitx,bity;		/* bit to do */
int mode;		/* SET, CLEAR, or TOGGLE */
{
   int state = bit_on(map,bitx,bity) ? ON : OFF;
   switch(mode | state) {
      case TOGGLE | OFF:
      case TOGGLE | ON:
      case SET | OFF:
      case CLEAR | ON:
         bit_point(map,bitx,bity,BIT_NOT(BIT_DST));
         m_bitwrite(x0+bitx*x,y0+bity*y,x,y);
         dprintf(stderr,"toggle (%d)\n",mode|state);
         break;
   }
}

/* save map for undo */

BITMAP *
set_undo(last,map)
BITMAP *last, *map;
{
   if (!last)
      last = bit_alloc(W(map),H(map),BIT_NULL,1);
   else if (W(last)!=W(map) || H(last)!=H(map)) {
      bit_destroy(last);
      last = bit_alloc(W(map),H(map),BIT_NULL,1);
   }
   bit_blit(last,0,0,W(last),H(last),BIT_SRC,map,0,0);
   return(last);
}

/* alloc and save space for the concatenation of s1 and s2 */

char *str_save(s1,s2)
char *s1, *s2;
{
   char *result;

   if ((result = malloc(strlen(s1) + strlen(s2) + 1)) == NULL) {
      fprintf(stderr,"Malloc failed\n");
      clean(1);  
   }

   strcpy(result,s1);
   strcat(result,s2);
   return(result);
}
