/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* rotate a bitmap 90 deg clockwise (mgr format) */

#include <unistd.h>
#include <stdio.h>

#include "bitmap.h"

#define MAX	2400		/* max bitmap size */

#define GET_OPT(i)	\
	strlen(argv[i])>2 ? argv[i]+2 : argv[++i]

unsigned char buff[MAX];	/* output row buffer */

int main(argc,argv)
int argc;
char **argv;
   {
   char *malloc();
   struct b_header b_buff, *head = &b_buff;
   int w=0,h=0,d;
   int inbytes,outbytes,size;
   int reverse = 0;
   int x_flag=0;

   register unsigned char *data;
   register unsigned char *src;
   register int bit,word;
   register int col,row;
   int ok;
   int verbose = 0;
   int i;

   /* check arguments */

   for(i=1;i<argc;i++) {
      if (*argv[i] == '-')
         switch (argv[i][1]) {
            case 'w':				/* specify width */
               w = atoi(GET_OPT(i));
               break;
            case 'h':				/* specify height */
               h = atoi(GET_OPT(i));
               break; 
            case 'x':				/* don't output header */
               x_flag++;
               break; 
            case 'r':				/* reverse bits */
               reverse++;
               break; 
            case 'v':				/* verbose */
               verbose++;
               break; 
            default:
               fprintf(stderr,"%s: invalid flag %c ignored\n",argv[0],argv[i][1]);
            }
      else
         fprintf(stderr,"%s: invalid argument %s ignored\n",argv[0],argv[i]);
      }


   if (w==0 && h==0) {

      /* read in bitmap header */

      if (!bitmaphead( stdin, &w, &h, &d, &inbytes )) {
         fprintf(stderr,"%s: invalid bitmap format \n",*argv);
         exit(1);
         }
      }

   else if (w==0 || h==0) {
      fprintf(stderr,"%s:  Both -h and -w must be specified\n",*argv);
      exit(1);
      }
      
   if (w > MAX) {
      fprintf(stderr,"%s:  bitmap too big\n",*argv);
      exit(1);
      }

   size = inbytes * h;			/* total bytes in bitmap */
   outbytes = ((h+7)&~7)/8;		/* bytes/row (output) */
   
   if ((data= (unsigned char *) malloc(size)) == NULL) {
      fprintf(stderr,"%s: can't malloc space\n",*argv);
      exit(2);
      }
      
   for(ok=1,col=0;ok>0 && col<size;col += (ok=Read(data+col,size-col)))
      if (verbose) write(2,">",1);

   /* write new header */

   if (!x_flag) {
      B_PUTHDR8(head,h,w,d);
      fwrite(head,sizeof b_buff,1,stdout);
      }

   /* rotate and output new bitmap */

   data += size - inbytes;		/* start at end */
   for (col = 0;col< w; col++) {
      bit = 0x80 >> (col&0x7);
      word = col/8;
      if (reverse)
         for(src = &buff[outbytes]; src >= buff; src-- )
		*src = ~0;
      else
         bzero(buff,outbytes);
      for(src=data+word,row = 0; row< h; row++,src -= inbytes) 
         if (*src & bit)
            buff[row>>3] ^= 0x80 >> (row&0x7);
      fwrite(buff,outbytes,1,stdout);
      if (verbose) write(2,">",1);
      }
   exit(0);
   }

/* do multiple passes for read */

Read(buff,count)
register char *buff;
int count;
   {
   register int sum=0,current=0;

   while((current = fread(buff+sum,1,count-sum,stdin))>0)
      sum += current;
   return(sum);
   }
