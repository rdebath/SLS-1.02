/*{{{}}}*/
/*{{{  #includes*/
#include <errno.h>
#include <string.h>
#include <limits.h>
#include <getopt.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "term.h"
/*}}}  */
/*{{{  #defines*/
#define WINDOW_BITMAP 0
#define IMAGE_BITMAP 1
/*}}}  */

/*{{{  clean*/
void clean(int n)
{
  m_setcursor(CS_BLOCK);
  m_pop();
  m_flush();
  m_ttyreset();
  exit(n);
}
/*}}}  */
/*{{{  display*/
void display(int x, int y, int my_width, int my_height, int image_width, int image_height)
{
  int image_x=0,image_y=0;

  m_func(BIT_CLR);
  m_bitwrite(0,0,my_width,my_height);
  m_func(BIT_SRC);
  if (x<0) { image_width+=x; image_x-=x; x=0; }
  if (y<0) { image_height+=y; image_y-=y; y=0; }
  m_bitcopyto(x,y,image_width,image_height,image_x,image_y,WINDOW_BITMAP,IMAGE_BITMAP);
}
/*}}}  */

/*{{{  main*/
int main(int argc, char *argv[])
{
  /*{{{  variables*/
  enum { NOTHING, LOCAL, REMOTE } in=NOTHING;
  int c;
  int x,y;
  int mouse_x,mouse_y;
  int image_width,image_height;
  int image_xoffset=0,image_yoffset=0;
  int my_width,my_height;
  int err=0,usage=0;
  BITMAP *bp;
  FILE *input;
  static struct menu_entry menu[] = { { "quit","q" } };
  char file[_POSIX_PATH_MAX];
  /*}}}  */

  /*{{{  parse arguments*/
  while ((c=getopt(argc,argv,"l:r:"))!=EOF)
  {
    switch (c)
    {
      /*{{{  l file*/
      case 'l':
      {
        if ((input=fopen(optarg,"r"))==(FILE*)0)
        {
          fprintf(stderr,"%s: Can't open %s\r\n",argv[0],optarg);
          err=1;
        }
        else in=LOCAL;
        break;
      }
      /*}}}  */
      /*{{{  r file*/
      case 'r':
      {
        char *cwd;

        in=REMOTE;
        if (*optarg!='/' && *optarg!='.')
        {
          if ((cwd=getcwd((char*)0,(size_t)0))!=(char*)0) { strcpy(file,cwd); strcat(file,"/"); strcat(file,optarg); }
          else { fprintf(stderr,"%s: Can't get current directory\r\n",argv[0]); err=1; }
        }
        else strcpy(file,optarg);
        break;
      }
      /*}}}  */
      /*{{{  default*/
      default:
      {
        usage=1;
        break;
      }
      /*}}}  */
    }
  }
  if (err) exit(err);
  if (usage || optind!=argc)
  {
    fprintf(stderr,"Usage: mgrview [-l file | -r file]\n");
    exit(1);
  }
  if (in==NOTHING) { in=LOCAL; input=stdin; }
  /*}}}  */
  /*{{{  setup*/
  ckmgrterm(argv[0]);
  m_setup(M_MODEOK);
  signal(SIGINT,clean);
  signal(SIGTERM,clean);
  signal(SIGPIPE,clean);
  m_ttyset();
  m_push(P_MENU|P_EVENT|P_FLAGS);
  m_setmode(M_ABS);
  m_setcursor(CS_INVIS);
  menu_load(1,1,menu);
  m_setevent(REDRAW, "R");
  m_setevent(RESHAPE, "R");
  m_setevent(BUTTON_1,"[%p]");
  m_setevent(BUTTON_2,"m");
  m_flush();
  /*}}}  */
  if (in==LOCAL)
  {
    /*{{{  load bitmap to client space*/
    if ((bp=bitmapread(input))==(BITMAP*)0)
    {
      fprintf(stderr,"%s: No MGR bitmap.\r\n",argv[0]);
      clean(1);
    }
    image_width=BIT_WIDE(bp);
    image_height=BIT_HIGH(bp);
    /*}}}  */
    /*{{{  transfer bitmap to server space*/
    m_func(BIT_SRC);
    m_bitcreate(IMAGE_BITMAP,image_width,image_height);
    m_bitldto(image_width,image_height,0,0,IMAGE_BITMAP,bit_size(image_width,image_height,BIT_DEPTH(bp)));
    fwrite(BIT_DATA(bp),bit_size(image_width,image_height,BIT_DEPTH(bp)),1,m_termout);
    m_flush();
    bit_destroy(bp);
    /*}}}  */
  }
  else if (in==REMOTE)
  {
    /*{{{  transfer bitmap from server fs to server space*/
    m_bitfromfile(IMAGE_BITMAP,file);
    m_get();
    if (sscanf(m_linebuf,"%d %d",&image_width,&image_height)<2)
    {
      fprintf(stderr,"%s: No MGR bitmap.\r\n",argv[0]);
      clean(1);
    }
    /*}}}  */
  }
  /*{{{  user interaction*/
  get_size((int*)0,(int*)0,&my_width,&my_height);
  display(image_xoffset,image_yoffset,my_width,my_height,image_width,image_height);
  m_flush();
  while ((c=getc(m_termin))!='q') switch (c)
  {
    /*{{{  m -- left button displays menu*/
    case 'm':
    {
      m_selectmenu(1);
      m_flush();
      break;
    }
    /*}}}  */
    /*{{{  [%d %d] -- right button*/
    case '[':
    {
      fscanf(m_termin,"%d %d]",&mouse_x,&mouse_y);
      /*{{{  compute new x start*/
      if (my_width>image_width) image_xoffset=0;
      else if (mouse_x<=0) image_xoffset=0;
      else if (mouse_x>=my_width) image_xoffset=my_width-image_width;
      else
      {
        /*{{{  move x start by difference from mouse and middle*/
        image_xoffset=image_xoffset-(mouse_x-my_width/2);
        /*}}}  */
        /*{{{  check and corrent range of x start*/
        if (image_xoffset<my_width-image_width) image_xoffset=my_width-image_width;
        else if (image_xoffset>0) image_xoffset=0;
        /*}}}  */
      }
      /*}}}  */
      /*{{{  compute new y start*/
      if (my_height>image_height) image_yoffset=0;
      else if (mouse_y<=0) image_yoffset=0;
      else if (mouse_y>=my_height) image_yoffset=my_height-image_height;
      else
      {
        /*{{{  move y start by difference from mouse and middle*/
        image_yoffset=image_yoffset-(mouse_y-my_height/2);
        /*}}}  */
        /*{{{  check and corrent range of y start*/
        if (image_yoffset<my_height-image_height) image_yoffset=my_height-image_height;
        else if (image_yoffset>0) image_yoffset=0;
        /*}}}  */
      }
      /*}}}  */
      display(image_xoffset,image_yoffset,my_width,my_height,image_width,image_height);
      m_flush();
      break;
    }
    /*}}}  */
    /*{{{  R -- redraw*/
    case 'R':
    {
      get_size((int*)0,(int*)0,&my_width,&my_height);
      /*{{{  compute new x offset*/
      if (my_width<image_width)
      {
        if (image_xoffset<my_width-image_width) image_xoffset=my_width-image_width;
      }
      else image_xoffset=0;
      /*}}}  */
      /*{{{  compute new y offset*/
      if (my_height<image_height)
      {
        if (image_yoffset<my_height-image_height) image_yoffset=my_height-image_height;
      }
      else image_yoffset=0;
      /*}}}  */
      m_func(BIT_CLR);
      m_bitwrite(0,0,my_width,my_height);
      m_func(BIT_SRC);
      m_bitcopyto(image_xoffset,image_yoffset,image_width,image_height,0,0,WINDOW_BITMAP,IMAGE_BITMAP);
      m_flush();
      break;
    }
    /*}}}  */
  }
  /*}}}  */
  /*{{{  exit*/
  m_bitdestroy(IMAGE_BITMAP);
  get_colrow(&x,&y);
  m_move(0,y-1);
  clean(0);
  /* NOTREACHED */
  return 255;
  /*}}}  */
}
/*}}}  */
