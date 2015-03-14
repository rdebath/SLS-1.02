/*{{{}}}*/
/*{{{  Notes*/
/*

The spirit of old mroff is still alive (and kicking:).  This driver and
the hfont.c file are essentially what the old mroff raster driver
contained in its first version.  Later there was support for LRU paging,
which was needed for a 64k I&D system, now the OS offers virtual memory.

Michael "likes roff since many years"

*/
/*}}}  */
/*{{{  #includes*/
#include <ctype.h>
#include <unistd.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>

#include "bit.h"
#include "hfont.h"
/*}}}  */
/*{{{  #defines*/
#define DEFAULT_XRES 72
#define DEFAULT_YRES 72
#define DEFAULT_XINCH 8
#define DEFAULT_YINCH 12

#define BASIC_XRES 288
#define BASIC_YRES 288

/* hfont uses fixpoint arithmetic */
#define PRECISION(x) ((x)<<7)
#define SINGLE(x) ((x)>>7)
/*}}}  */

/*{{{  variables*/
struct
{
  char *name;
  hfont_raw *raw;
  hfont_scaled *scaled;
  int size;
} mounted_font[] =
{
  /* Also change mkdevmgr when changing this */
  { "roman.s", (hfont_raw*)0, (hfont_scaled*)0, 0 },
  { "italic.t", (hfont_raw*)0, (hfont_scaled*)0, 0 },
  { "roman.t", (hfont_raw*)0, (hfont_scaled*)0, 0 },

  { "roman.s", (hfont_raw*)0, (hfont_scaled*)0, 0 },
  { "roman.d", (hfont_raw*)0, (hfont_scaled*)0, 0 },
  { "roman.c", (hfont_raw*)0, (hfont_scaled*)0, 0 },
  { "roman.t", (hfont_raw*)0, (hfont_scaled*)0, 0 },
  { "italic.d", (hfont_raw*)0, (hfont_scaled*)0, 0 },
  { "italic.t", (hfont_raw*)0, (hfont_scaled*)0, 0 },
  { "script.s", (hfont_raw*)0, (hfont_scaled*)0, 0 },
};
/*}}}  */

/*{{{  main*/
int main(int argc, char *argv[])
{
  /*{{{  variables*/
  int x,y,f,size,err=0,usage=0,verbose=0,page,newpage,firstpage=1,c;
#  ifdef MGR
  int mgr=0;
#  endif
  int xres=DEFAULT_XRES,yres=DEFAULT_YRES,xinch=DEFAULT_XINCH,yinch=DEFAULT_YINCH,wide,high;
  char ln[256],*s,*cmd=(char*)0;
  FILE *fp;
  /*}}}  */

  /*{{{  parse arguments*/
  /*{{{  parse arguments*/
  while ((c=getopt(argc,argv,
#  ifdef MGR
  "m"
#  endif
  "c:x:y:w:l:v"))!=EOF)
  {
    switch (c)
    {
#      ifdef MGR
      /*{{{  m*/
      case 'm':
      {
        mgr=1;
        break;
      }
      /*}}}  */
#      endif
      /*{{{  x*/
      case 'x':
      {
        xres=atoi(optarg);
        break;
      }
      /*}}}  */
      /*{{{  y*/
      case 'y':
      {
        yres=atoi(optarg);
        break;
      }
      /*}}}  */
      /*{{{  w*/
      case 'w':
      {
        xinch=atoi(optarg);
        break;
      }
      /*}}}  */
      /*{{{  l*/
      case 'l':
      {
        yinch=atoi(optarg);
        break;
      }
      /*}}}  */
      /*{{{  v*/
      case 'v': verbose=1; break;
      /*}}}  */
      /*{{{  c*/
      case 'c':
      {
        cmd=optarg;
        break;
      }
      /*}}}  */
      /*{{{  default*/
      default: usage=1; break;
      /*}}}  */
    }
  }
  if (optind!=argc) usage=1;
  wide=xres*xinch;
  high=yres*yinch;
  /*}}}  */
  /*{{{  exit if err or usage*/
  if (usage)
  {
    fprintf(stderr,"Usage: %s [-x dpi-x][-y dpi-y][-w inch-wide][-l inch-long][-c command][-v][-m]\n",argv[0]);
  }
  if (usage || err) exit(1);
  /*}}}  */
  /*}}}  */
  bitmalloc(wide,high);
  do
  {
    /*{{{  process one page*/
    bitclear();
    newpage=0;
    while (fgets(ln,sizeof(ln),stdin)!=(char*)0 && !newpage)
    {
      s=(ln[0]=='w' ? ln+1 : ln);
      *(s+strlen(s)-1)='\0';
      switch (*s)
      {
        /*{{{  hn*/
        case 'h': x+=PRECISION(atoi(s+1)*xres)/BASIC_XRES; break;
        /*}}}  */
        /*{{{  vn*/
        case 'v': y+=PRECISION(atoi(s+1)*yres)/BASIC_YRES; break;
        /*}}}  */
        /*{{{  fn*/
        case 'f':
        {
          f=atoi(s+1)-1;
          if (mounted_font[f].raw==(hfont_raw*)0) mounted_font[f].raw=hfont_open(mounted_font[f].name);
          break;
        }
        /*}}}  */
        /*{{{  sn*/
        case 's':
        {
          size=atoi(s+1);
          break;
        }
        /*}}}  */
        /*{{{  Hn*/
        case 'H': x=PRECISION(atoi(s+1)*xres)/BASIC_XRES; break;
        /*}}}  */
        /*{{{  Vn*/
        case 'V': y=PRECISION(atoi(s+1)*yres)/BASIC_YRES; break;
        /*}}}  */
        /*{{{  tstr*/
        case 't':
        {
          if (mounted_font[f].size!=size)
          {
            if (mounted_font[f].scaled!=(hfont_scaled*)0) free(mounted_font[f].scaled);
            mounted_font[f].scaled=(hfont_scaled*)0;
          }
          if (mounted_font[f].scaled==(hfont_scaled*)0)
          {
            mounted_font[f].scaled=hfont_scale(mounted_font[f].raw,xres,yres,size);
            mounted_font[f].size=size;
          }
          hfont_print(mounted_font[f].scaled,&x,&y,0,s+1);
          break;
        }
        /*}}}  */
        /*{{{  Cxy*/
        case 'C':
        {
          int fixed_x=x,fixed_y=y;

          if (mounted_font[f].size!=size)
          {
            if (mounted_font[f].scaled!=(hfont_scaled*)0) free(mounted_font[f].scaled);
            mounted_font[f].scaled=(hfont_scaled*)0;
          }
          if (mounted_font[f].scaled==(hfont_scaled*)0)
          {
            mounted_font[f].scaled=hfont_scale(mounted_font[f].raw,xres,yres,size);
            mounted_font[f].size=size;
          }
          if (*(s+1)=='\\' && *(s+2)=='-') hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,"\177");
          if (*(s+1)==':' && *(s+2)=='A') hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,"\200");
          if (*(s+1)==':' && *(s+2)=='a') hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,"\201");
          if (*(s+1)==':' && *(s+2)=='O') hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,"\202");
          if (*(s+1)==':' && *(s+2)=='o') hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,"\203");
          if (*(s+1)==':' && *(s+2)=='U') hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,"\204");
          if (*(s+1)==':' && *(s+2)=='u') hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,"\205");
          if (*(s+1)=='s' && *(s+2)=='s') hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,"\206");
          break;
        }
        /*}}}  */
        /*{{{  cx*/
        case 'c':
        {
          char str[2];
          int fixed_x=x,fixed_y=y;

          str[0]=*(s+1);
          str[1]='\0';
          hfont_print(mounted_font[f].scaled,&fixed_x,&fixed_y,0,str);
          if (*(s+2)=='t')
          {
            hfont_print(mounted_font[f].scaled,&x,&y,0,s+3);
          }
          break;
        }
        /*}}}  */
        /*{{{  pn*/
        case 'p':
        {
          y=0;
          page=atoi(s+1);
          if (!firstpage) newpage=1;
          else
          {
            firstpage=0;
            if (verbose) { fprintf(stderr,"[%d] ",page); fflush(stderr); }
          }
          break;
        }
        /*}}}  */
        /*{{{  D?*/
        case 'D':
        {
          switch (*(s+1))
          {
            /*{{{  e*/
            case 'e':
            {
              int dx,dy,rx,ry;

              sscanf(s+2,"%d %d",&dx,&dy);
              rx=PRECISION(dx*xres)/(2*BASIC_XRES);
              ry=PRECISION(dy*yres)/(2*BASIC_YRES);
              bitellipse(SINGLE(x+rx),SINGLE(y),SINGLE(rx),SINGLE(ry));
              break;
            }
            /*}}}  */
            /*{{{  c*/
            case 'c':
            {
              int d,rx,ry;

              sscanf(s+2,"%d",&d);
              rx=PRECISION(d*xres)/(2*BASIC_XRES);
              ry=PRECISION(d*yres)/(2*BASIC_YRES);
              bitellipse(SINGLE(x+rx),SINGLE(y),SINGLE(rx),SINGLE(ry));
              break;
            }
            /*}}}  */
            /*{{{  l*/
            case 'l':
            {
              int nx,ny;

              sscanf(s+2,"%d %d",&nx,&ny);
              nx=PRECISION(nx*xres)/BASIC_XRES;
              ny=PRECISION(ny*yres)/BASIC_YRES;
              bitline(SINGLE(x),SINGLE(y),SINGLE(x+nx),SINGLE(y+ny));
              x+=nx;
              y+=ny;
              break;
            }
            /*}}}  */
            /*{{{  p*/
            case 'p':
            {
              int nx,ny,ox=x,oy=y;

              s+=2; while (isblank(*s)) s++;
              do
              {
                nx=PRECISION(atoi(s)*xres)/BASIC_XRES; while (isdigit(*s) || *s=='-') s++; while (isblank(*s)) s++;
                ny=PRECISION(atoi(s)*yres)/BASIC_YRES; while (isdigit(*s) || *s=='-') s++; while (isblank(*s)) s++;
                bitline(SINGLE(ox),SINGLE(oy),SINGLE(ox+nx),SINGLE(oy+ny));
                ox+=nx;
                oy+=ny;
              } while (*s);
              bitline(SINGLE(ox),SINGLE(oy),SINGLE(x),SINGLE(y));
              x+=nx;
              y+=ny;
              break;
            }
            /*}}}  */
          }
          break;
        }
        /*}}}  */
      }
    }
    /*{{{  output page*/
    if (cmd!=(char*)0)
    {
      if ((fp=popen(cmd,"w"))==(FILE*)0)
      {
        fprintf(stderr,"%s: Can't execute %s: %s\n",argv[0],cmd,strerror(errno));
        exit(1);
      }
    }
    else fp=stdout;
#    ifdef MGR
    if (mgr) bitmgrwrite(fp); else
#    endif
    bitpbmwrite(fp);
    if (cmd!=(char*)0) pclose(fp);
    /*}}}  */
    if (newpage && !firstpage && verbose) { fprintf(stderr,"[%d] ",page); fflush(stderr); }
    /*}}}  */
  } while (newpage);
  bitfree();
  exit(0);
}
/*}}}  */
