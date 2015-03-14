/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/* convert a mgr window dump to lasergrafix format version 4 */

/*	$Header: lasergrafix.c,v 4.1 88/08/24 15:54:08 bianchi Exp $
	$Source: /tmp/mgrsrc/misc/RCS/lasergrafix.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/misc/RCS/lasergrafix.c,v $$Revision: 4.1 $";

#include <stdio.h>

#define WIDE	(30*85)				/* pixels/page */
#define HIGH	(30*115)			/* pixels/page */
#define HEX(x)	((x)<10?(x)+'0':(x)-10+'A')

main(argc,argv)
int argc;
char **argv;
   {
   register int c,count=0, byte;
   int w,h,d,bytesperline;
   int scale = 2;

   /* read in bitmap header */

   if (!bitmaphead( stdin, &w, &h, &d, &bytesperline )) {
      fprintf(stderr,"%s: invalid bitmap format \n",*argv);
      exit(1);
      }

   printf("\r^PY^-\r");				/* new page */
   printf("^PY^,");				/* new page */
   printf("^IJ%.5d^IT%.5d",
         (HIGH-h*scale)/2,(WIDE-w*scale)/2);	/* center picture */
   printf("^IP0%d0%d",scale,scale);		/* expand by 2x2 */
   printf("^P%.4d", bytesperline*8);		/* plot "w" dots/row */

   while((c=getchar()) != EOF) {
      if (count==0) {
          count=1;
          byte=c;
          }
      else if (c==byte && count<999) {
          count++;
          }
      else {
          switch(count) {
             case 3:  putchar(HEX(byte>>4)), putchar(HEX(byte&017));
             case 2:  putchar(HEX(byte>>4)), putchar(HEX(byte&017));
             case 1:  putchar(HEX(byte>>4)), putchar(HEX(byte&017));
                      break;
             default: printf("^C%03d%c%c",count,HEX(byte>>4),HEX(byte&017));
             }
          count=1;
          byte=c;
          }
      }
   /* flush the rest of the picture */

   switch(count) {
      case 3:  putchar(HEX(byte>>4)), putchar(HEX(byte&017));
      case 2:  putchar(HEX(byte>>4)), putchar(HEX(byte&017));
      case 1:  putchar(HEX(byte>>4)), putchar(HEX(byte&017));
               break;
      default: printf("^C%03d%c%c",count,HEX(byte>>4),HEX(byte&017));
      }

   printf("^G^,");			/* print the page */
   printf("\r^PY^-\r");			/* new page */
   exit(0);
   }
