/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/* snap a piece of the screen -- only works locally */
/*}}}  */
/*{{{  #includes*/
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "term.h"
#include "bitblit.h"
/*}}}  */
/*{{{  #defines*/
#define ICON   "easel"
#define min(x,y)      ((x)>(y)?(y):(x))
#define dprintf   if(debug)fprintf
#define PRINTER   "lp"         /* default printer name */
#define CANCEL      10            /* time after which REVIEW is canceled */
/*}}}  */

/*{{{  variables*/
static char buff[100];         /* mgr input buffer */
static char cmd[100];            /* lpr command buffer */
static char *name;               /* file name */
static char my_host[32];            /* my-host */
static char mgr_host[32];            /* mgr host */

static int debug=0;
static int review=0;            /* review more set */
static int func = BIT_SRC;

#define MENU_COUNT      (sizeof(menu)/sizeof(struct menu_entry))

static struct menu_entry menu[] =
{
  {"print","Print\n"},
  {"file","File\n"},
  {"review =>","View\n"},
  {"quit","Quit\n"},
};
static struct menu_entry rop[] = 
{
  {"set","-Set\n"},
  {"paint","-Paint\n"},
  {"xor","-Xor\n"},
  {"mask","-Mask\n"},
};
/*}}}  */

/*{{{  cancel*/
void
cancel()
{
  review = 0;
  m_putchar('\007');
  m_clearmode(M_WOB);
  m_flush();
}
/*}}}  */
/*{{{  setup*/
void
setup(where,w,h)
int where;      /* bitmap # */
int w,h;         /* window size */
{
  int wx,wy;	/* window position */

  get_size(&wx,&wy,0,0);
  m_shapewindow(wx,wy,w+10,h+10);
  m_clear();
  m_bitcopyto(0,0,w,h,0,0,0,where);
  m_movecursor(w+20,h/2);
}
/*}}}  */
/*{{{  clean*/
void clean(n) int n;
{
  m_ttyreset();
  m_popall();
  exit(n);
}
/*}}}  */

/*{{{  main*/
int main(argc,argv) int argc; char **argv;
{
  /*{{{  variables*/
  BITMAP *screen , *tmp = (BITMAP *) 0;
  char *printer;
  FILE *fp;      /* file to write */
  int w,h;
  int wide,high;   /* picture size */
  int x,y;      /* window pos */
  int x1,y1;   /* sweep coords */
  int xmax, ymax;      /* display size */
  int n;
  int snapping = 0;      /* ready to snap */
  int format = OLD_BHDR;			/* new format */
  FILE *pf;
  /*}}}  */

  ckmgrterm(*argv);

  debug = (getenv("DEBUG")!=(char*)0);

  if (argc > 1 && strcmp("-n",argv[1])==0) 
  {
    format=NEW_BHDR;
    argc--;
    argv++;
  }

  if (argc != 2) 
  {
    fprintf(stderr,"Usage: snap [-n] <file>\n");
    exit(1);
  }

  name = argv[1];

  if ((screen = bit_open(SCREEN_DEV)) == (BITMAP *) 0) 
  {
    fprintf(stderr,"%s: Can't find %s\n",*argv,SCREEN_DEV);
    exit(1);
  }
  setuid(getuid()); setgid(getgid());
  if ((fp = fopen(name, "w")) == NULL) 
  {
    perror("fopen");
    fprintf(stderr,"%s: Can't fopen %s\n",*argv,name);
    exit(1);
  }

  if ((printer = getenv("PRINTER")) == (char *) 0)
  printer = PRINTER;
  sprintf(cmd,"lpr -P%s -J%s -v",printer,name);

  /* setup mgr library */

  m_setup(0);

  get_param(mgr_host,&xmax,&ymax,0);
  gethostname(my_host,sizeof(my_host));

  if (strcmp(my_host,mgr_host) != 0) 
  {
    fprintf(stderr,"%s only works on host: %s\n",
    argv[0],mgr_host);
    exit(1);
  }

  m_push(P_FONT|P_FLAGS|P_MENU|P_POSITION);
  m_ttyset();
  signal(SIGALRM,cancel);
  signal(SIGTERM,clean);
  signal(SIGINT,clean);
  signal(SIGHUP,clean);

  m_setmode(M_NOWRAP);
  m_setmode(M_ABS);
  m_func(BIT_SRC);
  m_bitfromfile(1,ICON);
  m_flush();
  m_gets(buff);
  n = sscanf(buff,"%d %d",&w,&h);
  if (n < 2) 
  {
    fprintf(stderr,"%s: Can't find %s\n",*argv,ICON);
    clean(1);
  }
  setup(1,w,h);
  m_setevent(BUTTON_1,"S%R\n");   /* get coords */
  m_setevent(REDRAW,"Redraw\n");	/* get coords */
  m_setevent(RESHAPE,"Reshape\n");	/* get coords */
  menu_load(1,MENU_COUNT,menu);
  menu_load(2,MENU_COUNT,rop);
  m_selectmenu(1);
  m_linkmenu(1,2,2,6);
  m_clearmode(M_ACTIVATE);

  m_flush();
  while(m_gets(buff)) 
  {
    dprintf(stderr,"got %s\n",buff);
    switch (*buff) 
    {
      case 'R':            /* redraw */
      setup(1,w,h);
      m_clearmode(M_ACTIVATE);
      break;
      case 'S':            /* set up to snap a picture */
      n = sscanf(buff+1,"%d %d %d %d",&x,&y,&x1,&y1);
      if (n < 4)
      break;;
      m_setmode(M_WOB);
      wide = abs(x1-x);
      high = abs(y1-y);
      x = min(x,x1);
      y = min(y,y1);
      m_push(P_MOUSE);
      if (x > 16 || y > 16)
      m_movemouse(0,0);         /* get mouse out of the picture */
      else
      m_movemouse(xmax-17,ymax-17);
      if (review) 
      {
        alarm(0);
        m_sendme("B review\n");                     /* synchronize review */
      }
      else 
      {
        m_sendme("E snap\n");                     /* synchronize snap */
        snapping = 1;
        if (tmp) 
        {
          bit_destroy(tmp);
          tmp = NULL;
        }
        tmp = bit_alloc(wide,high,BIT_NULL,1);
      }
      break;
      case 'B':               /* review the picture */
      if (review && tmp && !snapping) 
      {
        review = 0;
        if (wide < 20 && high < 20) 
        {
          wide = BIT_WIDE(tmp);
          high = BIT_HIGH(tmp);
        }
        bit_blit(screen,x,y,wide,high,func,tmp,0,0);
        dprintf(stderr,"review %d,%d by %d,%d\n",x,y,wide,high);
        m_pop();
      }
      m_clearmode(M_WOB);
      m_clearmode(M_ACTIVATE);
      break;
      case 'E':               /* get the picture */
      if (snapping && x!=16 && y!=16) 
      {
        bit_blit(tmp,0,0,wide,high,BIT_SRC,screen,x,y);
        m_pop();
        m_clearmode(M_WOB);
        m_clearmode(M_ACTIVATE);
      }
      snapping = 0;
      break;
      case 'F':                  /* file it */
      if (tmp && !snapping) 
      {
        fseek(fp,0L,0);
        ftruncate(fileno(fp),0);
        if (!bitmapwrite(fp,tmp,format) ) 
        {
          m_push(P_ALL);
          m_font(0);
          m_size(27,3);
          m_clear();
          m_printstr("unable to write file\n");
          m_printstr(name);
          m_flush();
          sleep(3);
          m_pop();
          m_clearmode(M_ACTIVATE);
          m_flush();
        }
        dprintf(stderr,"filing\n");
      }
      fflush(fp);
      m_clearmode(M_ACTIVATE);
      break;
      case 'Q':                  /* quit */
      clean(0);
      break;
      case 'P':                  /* print */
      if (tmp && !snapping && (pf = popen(cmd,"w"))) 
      {
        bitmapwrite(pf,tmp,format);
        dprintf(stderr,"printing [%s]\n",cmd);
        pclose(pf);
      }
      m_clearmode(M_ACTIVATE);
      break;
      case '-':                  /* set review mode*/
      if (review) 
      switch (*(buff+1)) 
      {
        case 'S':         /* set */
        func = BIT_SRC;
        break;
        case 'P':         /* paint */
        func = BIT_SRC | BIT_DST;
        break;
        case 'X':         /* XOR */
        func = BIT_SRC ^ BIT_DST;
        break;
        case 'M':         /* MASK */
        func = BIT_SRC & BIT_DST;
        break;
      }
      case 'V':                  /* review */
      if (tmp && !snapping) 
      {
        alarm(CANCEL);
        review++;
        m_setmode(M_WOB);
      }
      break;
    }
    m_flush();
  }
   
  clean(0);
}
/*}}}  */
