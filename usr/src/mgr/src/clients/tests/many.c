/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* test many windows */

#include <stdio.h>
#include <unistd.h>
#define __USE_BSD
#include <stdlib.h>

#include "term.h"

#define GAP	10

void main(argc,argv)
int argc;
char **argv;
{
   int num;
   int count;
   int x,y,w,h;
   int lastx=0, lasty=0;
   int offset, foo;
   register int i;
   
   if (argc < 2 || (count=atoi(argv[1])) <=0)
      count = 5;

   srandom(getpid());
   m_setup(M_FLUSH);
   get_size(&x,&y,&w,&h);
   foo = (w + h)/2;

   for(i=0;i<=count;i++) {
      offset = random()%foo;
      x += random()&1 ? offset+GAP : - (offset+GAP);
      offset = random()%foo;
      y += random()&1 ? offset+GAP : - (offset+GAP);
      if (x<8) x=8;
      if (y<80) y=80;
      if (x+w > 1140) x = 1140-w;
      if (y+h > 890) y = 890-w;
      if (lastx == x || lasty == y) {
         i--; continue;
         }
      lastx = x, lasty = y;
      num = m_makewindow(x,y,w,h);
      m_selectwin(num);
      printf("Window %d\n",num);
      } 
   m_selectwin(0);
   m_setmode(M_ACTIVATE);
   printf("back to window zero\n");
}
