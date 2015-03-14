#include <stdio.h>

#include "bitmap.h"		/* needed by font.h */
#include "font.h"

int main(argc,argv) int argc; char **argv;
{
  struct font_header head;
  int i;
  FILE *fp;
  
  for (i=1; i<argc;i++)
  {
    if ((fp=fopen(argv[i],"r"))!=(FILE*)0)
    {
      if (fread(&head,sizeof(head),1,fp)!=1)
      fprintf(stderr,"fontinfo: Can't read font header of %s\n",argv[i]);
      else
      {
        printf("%s: ",argv[i]);
        printf("type %d, width %d, height %d",head.type, head.wide, head.high);
        printf(", baseline %d, count %d, start %d\n", head.baseline, head.count, head.start);
      }
      fclose(fp);
    }
    else fprintf(stderr,"fontinfo: Can't open %s\n",argv[i]);
  }
  exit(0);
}
