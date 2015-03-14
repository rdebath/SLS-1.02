/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* show a dithered image on a mgr window  (binary input) */
/* add color support */

#include <stdio.h>
#ifdef _POSIX_SOURCE
#include <termios.h>
#endif
#ifdef v7
#include <sgtty.h>
#endif
#include <signal.h>
#include "term.h"
#include "bitmap.h"

#define Min(x,y)	((x)>(y)?y:x)
#define GET_OPT(i)	\
	strlen(argv[i])>2 ? argv[i]+2 : argv[++i]

int mode;

main(argc,argv)
int argc;
char **argv;
   {
   register int i,j;
   int w=0,h=0,d=0,size;
   int x=0, y=0;
   int no_head = 0;
   int reverse=0;
   int shape=0;
   int margin=0;
   int wx,wy,border;
	int op = - 1;
   int clean();

   ckmgrterm( *argv );

   /* check arguments */

   for(i=1;i<argc;i++) {
      if (*argv[i] == '-')
         switch (argv[i][1]) {
            case 'x':				/* starting x position */
               x = atoi(GET_OPT(i));
            case 'y':				/* starting y position */
               y = atoi(GET_OPT(i));
               break;
            case 'f':				/* bitmap op code */
               op = 0xf&atoi(GET_OPT(i));
               break;
            case 'r':				/* flip bits */
               reverse++;
               break;
            case 's':				/* reshape window */
               shape++;
               break;
            case 'm':				/* margin for -s */
               margin = atoi(GET_OPT(i));
               break;
            default:
               fprintf(stderr,"%s: bad flag %c ignored\n",argv[0],argv[i][1]);
            }
      else if (!no_head){
         no_head++;
         w = atoi(argv[i]);
         }
      else if (h==0)
         w = atoi(argv[i]);
      else
         fprintf(stderr,"%s: invalid argument %s ignored\n",argv[0],argv[i]);
      }

   /* get header */

   if (no_head) {
      size = ((w+15)&~15)/8;  /* round to 16 bit boundary (rash assumption) */
      }
   else {
      if (!bitmaphead( stdin, &w, &h, &d, &size )) {
         fprintf(stderr,"%s: invalid bitmap format \n",*argv);
         exit(2);
	 }
      }

   m_setup(0);
#ifdef v7
   ioctl(fileno(m_termout),TIOCLGET,&mode);
   mode |= LLITOUT;
#endif
   signal(SIGINT,clean);
   signal(SIGTERM,clean);
   signal(SIGHUP,clean);
   get_size(&wx,&wy,0,0);
   get_param(0,0,0,&border);
#ifdef v7
   ioctl(fileno(m_termout),TIOCLSET,&mode);
#endif
	if (op != -1)
      m_func(op);
   else if (reverse)
      m_func(BIT_NOT(BIT_SRC));
   else
      m_func(BIT_SRC);

   if (shape) {
      border += margin;
      m_shapewindow(wx,wy,w+2*border,h+2*border);
      m_movecursor(wx+20,0);
      m_flush();
      }

   while(!feof(stdin) && h-- > 0) {
      m_bitld(w,1,margin+x,margin+y,size);
      i = size;
      while (i-- > 0 && !feof(stdin))
         fputc(getchar(),m_termout);
      while(i-- > 0)
            fputc('\0',m_termout);
      m_flush();
      y++;
      }

   clean(0);
   }

clean(n)
int n;
   {
#ifdef v7
   mode &= ~LLITOUT;
   ioctl(fileno(m_termout),TIOCLSET,&mode);
#endif
   exit(n);
   }
