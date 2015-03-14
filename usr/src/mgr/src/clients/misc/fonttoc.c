/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* font routines */

#include <string.h>
#include <time.h>
#include <stdio.h>

#include "font.h"

/**************************************************************************
 *
 *	compile a font file
 */

int main(argc,argv) int argc; char **argv;
   {
	long tm;
   struct font_header head;
   int sum=0;		/* # bytes of font data read so far */
   int size;		/* # bytes of font data */
   int c;			/* current font char */

   if (fread(&head,sizeof(head),1,stdin) != 1) {
		fprintf(stderr,"%s: sorry, Can't read font header\n",*argv);
      exit(1);
      }

   if (head.type == FONT_X) {
		fprintf(stderr,"%s: Sorry, Obsolete font format\n",*argv);
		exit(2);
		}

   else if (head.type != FONT_A) {
		fprintf(stderr,"%s: sorry, Input is not a font\n",*argv);
      exit(3);
      }
                               
   printf("/* static font file */\n\n");
   time(&tm);
	printf("/* created: %s */\n",ctime(&tm));

   printf("struct font_header %s_head = {\n",argv[1]);
   printf("\t(unsigned char) %d, (unsigned char) %d, (unsigned char) %d,\n",
          head.type, head.wide, head.high);
   printf("\t(unsigned char) %d, (unsigned char) %d, (char) %d\n",
          head.baseline, head.count, head.start);
   printf("\t};\n\n");

   printf("char %s_image[] = {\n\t",argv[1]);

	size = ((head.wide*head.count)+31)&~31; /* fonts always 32 bit padded */

   /* This is what I would call an ugly hack */
   while(sum++<(size*head.high)>>3) {
      c = getchar();
      printf("0x%02x,%s",(unsigned char)c,sum%12?"":"\n\t");
      }
   printf("\n\t};\n\n");
   printf("bit_static(%s,%d,%d,%s_image,1,0);\n",
           argv[1],size,head.high,argv[1]);

   exit(0);
   }
