#include <stdio.h>

int main(int argc, char *argv[])
{
  char ln[1024];
  int n=0, width;

  printf("name %s\n",argv[1]);
  while (fgets(ln,sizeof(ln),stdin)!=(char*)0)
  {
    width=ln[9]-ln[8];
    if (n==0) printf("spacewidth %d\ncharset\n",width);
    else if (n+' '<127) printf("%c	%d	0	%d\n",n+' ',width,n+' ');
    else printf("%c%c	%d	0	%d\n",ln[0],ln[1],width,n+' ');
    n++;
  }
  exit(0);
}
/*{{{}}}*/
