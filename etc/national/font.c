#include <stdio.h>

main ()
{
  unsigned char i, j;
  printf ("\n");
  for (i = 0; i < 16; i++)
    {
      for (j = 0; j < 16; j++)
	printf ("%c  ", (((j * 16 + i) & 127) < 32 || j * 16 + i == 127) ? 46 : j * 16 + i);
      printf ("\n");
    }
  printf ("\n");
}
