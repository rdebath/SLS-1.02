/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/* graphics print filter for hp think jet printer (mgr format) */
/* version 2 - eliminate trailing white space */
/* version 3 - rotate fat pictures */

/*	$Header: tjfilter.c,v 1.3 88/08/24 10:36:59 bianchi Exp $
	$Source: /tmp/mgrsrc/misc/RCS/tjfilter.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/misc/RCS/tjfilter.c,v $$Revision: 1.3 $";

#include <stdio.h>
#include "dump.h"
#include <sgtty.h>

#define MAX	640		/* maximum dots per line */
#define START	"\033*rA"	/* start graphics */
#define COUNT	"\033*b%dW"	/* bytes on this line */
#define END	"\033*rB\n"	/* end graphics */
#define LOW	"\033*640S"	/* low density print mode */

char buff[MAX];

main(argc,argv)
int argc;
char **argv;
   {
   register int byteno;				/* byte number */
   register char *pntr;
   int mode;					/* tty local modes */
   int reverse = 0;				/* reverse bits */
   int rotate = 0;				/* rotate bitmap */
   int w,h,d;					/* bitmap size */
   int bytes;					/* bytes/line of bitmap */
   int lastbyte;				/* last significant byte */
   unsigned char mask;				/* to mask last byte */
   char mask1;					/* to mask last byte */
   int speed=0;					/* set speed to 9600 baud */
   int clean();
   char *sprintf();
   FILE *input, *popen();			/* for rotate filter */

   if (argc>1 && strcmp(argv[1],"-r")==0)
      reverse++;
   else if (argc>1 && strcmp(argv[1],"-s")==0)
      speed++;

   if (argc>2 && strcmp(argv[2],"-r")==0)
      reverse++;
   else if (argc>2 && strcmp(argv[2],"-s")==0)
      speed++;

   if (!bitmaphead( stdin, &w, &h, &d, &bytes )) {
      fprintf(stderr,"%s: invalid bitmap format \n",*argv);
      exit(2);
      }

   /* rotate bitmap, if short and fat */

   if (w>MAX && h<MAX && 
            (input = popen(sprintf(buff,"rotate -w %d -h %d",w,h),"r"))) {
      rotate++;
      if (!bitmaphead( stdin, &w, &h, &d, &bytes )) {
         fprintf(stderr,"%s: invalid bitmap format \n",*argv);
         exit(2);
         }
      }
   else
      input = stdin;
      

   mask = w%8 ? 0xff >> (w%8) : 0;
   mask1 = ~(mask & 0xff);
   lastbyte = w/8;

   /* set up the printer */

   ioctl(1,TIOCLGET,&mode);
   mode |= LLITOUT;
   ioctl(1,TIOCLSET,&mode);
   if (speed)
      set_speed(1,B9600);

   printf(LOW);
   printf(START);

   while (fread(buff,bytes,1,input) > 0)  {
      if (reverse)
         invert(buff,bytes);
      buff[lastbyte] &= mask1;
      for(byteno=lastbyte, pntr=buff+byteno; byteno>=0; byteno--)
         if (*pntr--)
            break;
      printf(COUNT,byteno+1);
      fwrite(buff,byteno+1,1,stdout);
      }

   printf(END);
   mode &= ~LLITOUT;
   /* ioctl(1,TIOCLSET,&mode); */
   if (rotate)
      pclose(input);
   exit(0);
   }

/* invert each bit */

invert(data,count)
register unsigned char *data;
int count;
   {
   register unsigned char *end = data + count;

   while(data < end)
      *data++ = ~*data;
   }


/*
 *	Set the terminal speed 
 */

set_speed(file,speed)
int file;		/* file pointer */
int speed;
{
	struct sgttyb buff;

	gtty(file,&buff);
	buff.sg_ospeed  = speed;
	buff.sg_ispeed  = speed;
	stty(file,&buff);
        return(0);
}
