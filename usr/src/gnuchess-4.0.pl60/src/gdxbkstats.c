#include <stdio.h>
#include <fcntl.h>
#include "gnuchess.h"
#undef rxx
#undef cxx
#undef scanz
#undef printz

#define rxx "12345678"
#define cxx "abcdefgh"
char xmvstr[12];
char *
cvt (m)
     unsigned int m;
{
    unsigned int f, t;
    f = m >> 8 & 0x3f;
    t = m & 0x3f;
/* algebraic notation */
    xmvstr[0] = cxx[column (f)];
    xmvstr[1] = rxx[row (f)];
    xmvstr[2] = cxx[column (t)];
    xmvstr[3] = rxx[row (t)];
    xmvstr[6] = '\0';
    if (m & DONTUSE)
	xmvstr[4] = '?';
    else
	xmvstr[4] = ' ';
    if (m & LASTMOVE)
	xmvstr[5] = '*';
    else
	xmvstr[5] = ' ';
    return xmvstr;
}

#define lts(x) (x>>16)		/* long to short to convert hashkey to short */
int gfd;
struct gdxadmin
{
    unsigned int bookcount;
    unsigned int booksize;
    unsigned long maxoffset;
} ADMIN;

#define N 2
struct gdxdata
{
    unsigned int hashbd;
    unsigned short hashkey;
    unsigned short bmove;
    unsigned short hint;
    unsigned short count;
} DATA;
void 
usage (char *x)
{
    printf ("usage %s binbookfile [ -h key bd]\n", x);
    exit ();
}

int i;
int c1 = 0;
int in = 0;
int max = -9999;
int min = 9999999;
int n = 0;
int sum = 0;
int sumc = 0;
unsigned long key, bd;
int hk = false;
main (argc, argv)
     int argc;
     char **argv;
{
    if (argc == 5)
      {
	  if (strcmp (argv[2], "-h") != 0)
	      usage (argv[0]);
	  key = strtol (argv[3], NULL, 16);
	  bd = strtol (argv[4], NULL, 16);
	  hk = true;
      }
    else if (argc != 2)
	usage (argv[0]);
    gfd = open (argv[1], O_RDONLY);
    if (gfd >= 0)
      {
	  read (gfd, &ADMIN, sizeof (struct gdxadmin));
	  printf ("entrysize %d\nbooksize %d\nbookcount %d\nmaxoffset %ld\n", sizeof (struct gdxdata), ADMIN.booksize, ADMIN.bookcount, ADMIN.maxoffset);
	  for (i = 0; i < ADMIN.booksize; i++)
	    {
		if (0 > read (gfd, &DATA, sizeof (struct gdxdata)))
		  {
		      perror ("fread");
		      exit (1);
		  }
		if (hk)
		  {
		      if (DATA.count && DATA.hashbd == bd && lts (key) == DATA.hashkey)
			{
			    printf ("%s ", cvt (DATA.bmove));
			    printf ("%s ", cvt (DATA.hint));
			    printf ("%d\n", DATA.count);
			}
		  }
		if (in && DATA.bmove)
		  {
		      c1++;
		  }
		else if (DATA.bmove)
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
	    }
	  close (gfd);
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

      }
}
