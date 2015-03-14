/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/* Raster print filter for hp laser printer */

/***********************************************************************
 * $Header: hp_raster.c,v 1.3 88/07/15 14:34:13 sau Exp $
 * $Locker:  $
 * $Source: /tmp/mgrsrc/misc/RCS/hp_raster.c,v $
 * $Log:	hp_raster.c,v $
 * Revision 1.3  88/07/15  14:34:13  sau
 * Add new bitmap format (handles both formats now )
 * 
 * Revision 1.1  88/07/15  14:32:21  sau
 * Initial revision
 * 
 * Revision 1.2  88/07/08  08:25:43  sau
 * 
 * 
 * Revision 1.1  88/07/08  08:16:57  sau
 * Initial revision
 * 
 * Revision 1.1  88/07/08  08:14:22  sau
 * Initial revision
 * 
 * Revision 1.5  87/07/14  20:10:40  sau
 * Eliminate training white space on a line by line basis
 * 
 * Revision 1.4  87/06/25  11:03:29  sau
 * Auto scaling, lpr version
 * 
 ***********************************************************************/

/* avoid complaints from "lint" */

#ifndef lint
static char     RCS_Id[] = "$Header: hp_raster.c,v 1.3 88/07/15 14:34:13 sau Exp $";
#endif
#include <stdio.h>
#include "dump.h"

#define GET_OPT(i)	\
	strlen(argv[i])>2 ? argv[i]+2 : argv[++i]
#define WIDTH16(w)	((((w)+15)&~0xf)>>3)		/* old bitmap format */
#define WIDTH8(w)	((w)>>3)							/* new bitmap format */
#define Min(x,y)	((x)<(y)?(x):(y))

#define DOTS_WIDE	(8*300)		/* dots across page */
#define DOTS_HIGH	(10*300+150)	/* dots down page */
#define WIDE		1152		/* default # pixels/row */
#define HIGH		900		/* default # rows/screen */
#define HI_RES		300		/* dots per inch */
#define MED_RES		150		/* dots per inch */
#define LOW_RES		100		/* dots per inch */
#define MIN_RES		75		/* dots per inch */
#define MAX		4000		/* maximum row size */
#define BORDER		3		/* default size of border */
#define STRIP		3		/* white strip for viewgrqaphs */

/* printer macros */

#define set_pos(xp,yp) \
	printf("\033*p%dy%dX",yp,xp)	/* set cursor position (dots) */
#define set_res(res) \
	printf("\033*t%dR",res)		/* set dots/inch */
#define reset() \
	printf("\033E")			/* reset the printer */
#define manual_feed() \
	printf("\033&l2H")		/* select manual feed */
#define start_graphics() \
	printf("\033*r1A")		/* set raster mode */
#define set_row(wide) \
	(printf("\033*b%dW",wide), fflush(stdout))
#define end_graphics() \
	printf("\033*rB")		/* end graphics mode */
#define set_copies(n) \
	printf("\033&l%dX",Min(n,99))	/* set copy count */
#define set_rule(w,h) \
	printf("\033*c%da%dB",w,h)	/* set rule size */
#define print_rule(type) \
	printf("\033*c%dP",type)	/* print 'type' rule */

union {
	struct b_header new;
	struct old_b_header old;
	char type[2];
	} h_buff, *head = &h_buff;

main(argc,argv)
int argc;
char **argv;
   {
   register int k,i,j=0;
   int n;				/* last non-white space */
   unsigned char buff[MAX/8];		/* graphics line buffer */
   int wide = WIDE, high = HIGH;	/* raster size */
   int no_head=0;			/* no raster header found */
   int reverse=0;			/* reverse bits */
   int pause = 0;			/* for vgrafs */
   int manual = 0;			/* select manual feed */
   int copies = 0;			/* set copy count */
   int border = 0;			/* draw border around picture */
   int res = MED_RES;			/* resolution dots/in */
   int force_res=0;			/* force resolution */
   int x0,y0;				/* starting raster coord */
   int x=0,y=0;					/* user supplied coords */
   int bytes;				/* raster line size */
	int depth=1;			/* bitmap depth */
	char type[2];			/* bitmap type */

   /* check arguments */

   for(i=1;i<argc;i++) {
      if (*argv[i] == '-')
         switch (argv[i][1]) {
            case 'X':				/* x coord */
               x = atoi(GET_OPT(i));
               break;
            case 'Y':				/* y coord */
               y = atoi(GET_OPT(i));
               break;
            case 'r':				/* reverse bits */
               force_res = atoi(GET_OPT(i));
               break;
            case 'm':				/* set manual feed */
               manual++;
               break;
            case 'n':				/* login (from lpd )*/
               GET_OPT(i);
               break;
            case 'h':				/*  host (from lpd) */
               GET_OPT(i);
               break;
            case 'y':				/* specify border- for lpd */
               border = Min(32,atoi(GET_OPT(i)));
               break;
            case 'b':				/* set border size */
               if (argv[i][2])
                  border = atoi(&argv[i][2]);
               else
                  border = BORDER;
               break;
            case 'c':				/* set copies */
               copies = atoi(GET_OPT(i));
               break;
            case 'p':				/* set pause */
            case 'x':				/* for lpd */
               pause = atoi(GET_OPT(i));
               break;
            default:
               fprintf(stderr,"%s: bad flag %c ignored\n",argv[0],argv[i][1]);
            }
      }

   /* get header */

   if (!no_head) {
      read(0,head->type,2);				/* get magic number */
      if (B_ISHDR16(&head->old)) {		/* old style header */
         read(0,head->type+2,B_OHSIZE-2);
         B_GETOLDHDR(&head->old,wide,high);
   		bytes = WIDTH16(wide);
			}
		else if (B_ISHDR8(&head->new)) {		/* new style header */
         read(0,head->type+2,B_HSIZE-2);
         B_GETHDR8(&head->new,wide,high,depth);
   		bytes = WIDTH8(wide);
			}
      else {		/* assume 16 bit alignment , no header */
         wide=WIDE,high=HIGH-1;
         Read(0,buff,WIDTH16(wide)-2);		/* oops ! lose 1st line */
   		bytes = WIDTH16(wide);
         }
      }

	if (depth != 1) {
		fprintf(stderr,"Sorry, Can\'t handle 8-bit pixels\n");
		exit(0);
		}

   /* compute appropriate resolution */

   switch(Min(DOTS_WIDE/(border+wide),DOTS_HIGH/(border+high))) {
      case 0:		/* picture too big, use hi-res and go */
         /* no break - for now */
      case 1:
         res = HI_RES;
         break;
      case 2:
         res = MED_RES;
         break;
      case 3:
         res = LOW_RES;
         break;
      default:
         res = MIN_RES;
         break;
      }

   if (force_res)
      res = force_res;

   /* center picture */

   if (pause>1) 			/* skip white strip for viewgraphs */
      x0 = (DOTS_WIDE-HI_RES*wide/res);
   else
      x0 = (DOTS_WIDE-HI_RES*wide/res)/2;
   y0 = (DOTS_HIGH-HI_RES*high/res)/2;

   if (x>0)
      x0 = x;
   if (y>0)
      y0 = y;

   fprintf(stderr,"printing raster %dx%d at %d,%d - %d dots/in%s\n",
            wide,high,x0,y0,res,border ? " (bordered)":"");

   /* setup printer */

   reset();					/* reset the printer */
   if (pause || manual) 
		manual_feed();				/* select manual feed */
   if (copies)
		set_copies(copies);			/* set copy count */
   set_pos(x0,y0);				/* set starting position */
   set_res(res);				/* set resolution */
   start_graphics();				/* start graphics */

   for (i=0;i<high;i++) {
      Read(0,buff,bytes);
      for(j=bytes;j>0 && buff[j-1]==0;j--); 
      set_row(j);
      write(1,buff,j);
      }
   end_graphics();

   /* draw borders */

   if (border) {
      set_rule(border,2*border+high*HI_RES/res);
      set_pos(x0-border,y0-border);
      print_rule(0);				/* left edge */
      set_pos(x0+wide*HI_RES/res,y0-border);
      print_rule(0);				/* right edge */

      set_rule(wide*HI_RES/res,border);
      set_pos(x0,y0-border);
      print_rule(0);				/* top edge */
      set_pos(x0,y0+high*HI_RES/res);
      print_rule(0);				/* bottom edge */
      }

   printf("\n\f");
   exit(0);
   }

/* do multiple passes for read */

Read(fd,buff,count)
int fd,count;
register char *buff;
   {
   register int sum=0,current=0;

   while((current = read(0,buff+sum,count-sum))>0)
      sum += current;
   return(sum);
   }

/* return index of 1st non zero byte, or bytes if entirely blank */

int
first(buff,bytes)
register char *buff;
register int bytes;
   {
   char *start = buff+1;
   while(bytes-- > 0)
      if (*buff++) 
         break;
   return(buff-start);
   }
