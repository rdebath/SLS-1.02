/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
*/

/* convert mgr icon to "c" */
/*}}}  */
/*{{{  #includes*/
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
/*}}}  */

/*{{{  variables*/
static char *cmd;
/*}}}  */

/*{{{  usage*/
static void usage()
{
  fprintf(stderr,"Usage: %s <icon_file> ...  > <icon>.c\n", cmd);
  exit(1);
}
/*}}}  */
/*{{{  dofile*/
static void dofile(filename) char *filename;
{
  FILE *filep;
  register unsigned char *cp, *endp;
  register int i;
  BITMAP *map;
  char *name;

  if ((filep = fopen(filename,"r")) == (FILE*)0)
  {
    fprintf(stderr,"%s: Can\'t open %s\n",cmd,filename);
    exit(2);
  }

  if( !(map = bitmapread(filep)))
  {
    fprintf(stderr,"%s: %s is not a bitmap file.\n",cmd,filename);
    fclose(filep);
    exit(3);
  }
  fclose(filep);

  if (name = strrchr(filename,'/')) name++; else name = filename;

  /* print comment */

  printf("\n/* bitmap for \"%s\", %d wide, %d high, %d bit%s deep */\n",
  name, BIT_WIDE(map), BIT_HIGH(map),
  BIT_DEPTH(map), BIT_DEPTH(map) > 1 ? "s" : "");

  /* print out data */

  cp = (unsigned char *)BIT_DATA(map);
  endp = cp + bit_size(BIT_HIGH(map),BIT_WIDE(map),BIT_DEPTH(map));
  printf("char %s_data[%d] = {\n\t", name, endp - cp);
  for (i=1; cp<endp; cp++,i++) printf("0%03o%s", *cp, i%10 ? ", " : ",\n\t");
  printf("\n\t};\n");
  free((char*)map);

  /* print out header */

  printf("bit_static(%s, %d, %d, %s_data, 1, 0);\n", name,
  BIT_WIDE(map), BIT_HIGH(map), name);
}
/*}}}  */

/*{{{  main*/
int main(argc,argv) int argc; char **argv;
{
  cmd = *argv;
  argv++; argc--;
  if (argc < 1) usage();

  for(; argc>0; argv++, argc--) dofile(*argv);

  exit(0);
}
/*}}}  */
