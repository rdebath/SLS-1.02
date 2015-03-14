#include <stdio.h>
FILE *fd;
static struct bookentry
{
  unsigned long bookkey;
  unsigned long bookbd;
  unsigned short bmove;
  unsigned short hint;
  unsigned short count;
  unsigned short flags;
} BEND;
int i;
int booksize, bookmv, bookpocket, bookcount;
int c1 = 0;
int in = 0;
int max = -9999;
int min = 9999999;
int n = 0;
int sum = 0;
int sumc = 0;
int even = 0;
int odd = 0;
main (argc, argv)
     int argc;
     char **argv;
{
  fd = fopen (argv[1], "r");
  if (fd != NULL)
    {
      fscanf (fd, "%d\n", &booksize);
      fscanf (fd, "%d\n", &bookcount);
      fscanf (fd, "%d\n", &bookpocket);
      printf ("entrysize %d\nbooksize %d\nbookpocket %d\nbookcount %d\n", sizeof (struct bookentry), booksize, bookpocket, bookcount);
      for (i = 0; i < booksize; i++)
	{
	  if (0 > fread (&BEND, sizeof (struct bookentry), 1, fd))
	    {
	      perror ("fread");
	      exit (1);
	    }
#ifdef notdef
	  printf ("%lx %lx %x %d \n", BEND.bookkey, BEND.bookbd, BEND.bmove, BEND.count);
#endif
	  if (in && BEND.count)
	    {
	      c1++;
	    }
	  else if (BEND.count)
	    {
	      in = 1;
	      c1 = 1;
	    }
	  else if (c1)
	    {
	      /*printf ("out %d\n", c1);*/
	      n++;
	      if (c1 < min)
		min = c1;
	      if (c1 > max)
		max = c1;
	      sum += c1;
	      c1 = 0;
	      in = 0;
	    }
	  sumc += BEND.count;
	  if(BEND.count)if(i&1)odd++; else even++;
	}
      fclose (fd);
      if (in)
	{
	  /*printf ("out %d\n", c1);*/
	  n++;
      if (c1 < min)
	min = c1;
      if (c1 > max)
	max = c1;
      sum += c1;
}
      printf ("max %d\nmin %d\navg %f\nsumc %d\n", max, min, (float) sum / (float) n, sumc);
	printf("odd %d\neven %d\n",even,odd);

    }
}
