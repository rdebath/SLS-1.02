/*{{{}}}*/
/*{{{  Notes*/
/* test vector drawn text routines */
/*}}}  */
/*{{{  #includes*/
#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "term.h"
#include "hfont.h"
/*}}}  */
/*{{{  #defines*/
#define FONTS	(sizeof(s)/sizeof(char*))
/*}}}  */

/*{{{  variables*/
char *s[]=
{
  "roman.s",
  "roman.d",
  "roman.c",
  "roman.t",
  "italic.d",
  "italic.t",
  "script.s"
};
/*}}}  */

/*{{{  main*/
int main(int argc, char *argv[])
{
  /*{{{  variables*/
  int font;
  int angle;
  hfont_raw *rfont;
  hfont_scaled *sfont;
  int x,y;

  char buff[256];
  /*}}}  */

  if (argc<2) 
  {
    fprintf(stderr,"Usage: %s <text>\n",*argv);
    exit(1);   
  }

  strcpy(buff,"   ");
  strcat(buff,argv[1]);

  m_setup(M_MODEOK);
  m_func(BIT_SET);
  for(font=0;font<FONTS;font++) 
  {
    m_clear();
    rfont=hfont_open(s[font]);
    sfont=hfont_scale(rfont,72,72,72);
    free(rfont);
    for(angle=0; angle<360; angle+=30)
    {
      x=500<<7;
      y=500<<7;
      hfont_print(sfont,&x,&y,angle,buff);
    }
    free(sfont);
    m_flush();
    sleep(3);
  }
  exit(0);
}
/*}}}  */
