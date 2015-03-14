#include <fcntl.h>

#include "gdbmdefs.h"
#include "gdbmerrno.h"
#include "extern.h"
#include "gnuchess.h"
#undef rxx
#undef cxx
#undef scanz
#undef printz

#define rxx "12345678"
#define cxx "abcdefgh"
int nn = 0;

int listno = 0;

gdbm_file_info *gfd;

unsigned bookcount = 0;
unsigned bookup = 0;
unsigned bookmv = 0;

union U KEY;
union U DATA;
datum key =
{(char *) NULL, sizeof (union U)};
datum data =
{(char *) NULL, sizeof (union U)};
int temp;
datum return_data;
union U *OB;
struct keydata *OKK;
int i;
int n[N];
char mvstr[6];
void
cvt (f, t)
     unsigned int f, t;
{
/* algebraic notation */
  mvstr[0] = cxx[column (f)];
  mvstr[1] = rxx[row (f)];
  mvstr[2] = cxx[column (t)];
  mvstr[3] = rxx[row (t)];
  mvstr[4] = '\0';
}

main (argc,argv)
int argc;
char **argv;
{
  listno=atoi(argv[2]);
printf("%s %d\n",argv[1],listno);
  for (i = 0; i < N; i++) n[i] = 0;
  gfd = gdbm_open (argv[1], 512, GDBM_READER, 00444, NULL);
  nn = temp = 0;
  if (key.dptr != NULL) free (key.dptr);
  return_data = gdbm_firstkey (gfd);
  while (return_data.dptr != NULL)
    {
      OKK = (struct keydata *) return_data.dptr;
	  data = gdbm_fetch (gfd, return_data);
	  OB = (union U *) data.dptr;
	  temp++;
	  if(temp%1000 == 0)printf("%d\n",temp);
	  if (OB->D.number <= N)
	    n[OB->D.number - 1]++;
	if(OKK->bookbd == 0  && OKK->bookkey == 0) nn++;
	


if(listno){
	  printf ("%lx %lx %d ", OKK->bookkey, OKK->bookbd, OKK->side);
	  for (i = 0; i < OB->D.number; i++)
	    {
	      cvt (OB->D.bmove[i] >> 8, OB->D.bmove[i] & 0x3f);
	      printf ("%s ", mvstr);
	      cvt (OB->D.hint[i] >> 8, OB->D.hint[i] & 0x3f);
	      printf ("%s / ", mvstr);
	    }
	      listno--;
	  printf ("\n");
}
	  key = return_data;
	  free (OB);
      return_data = gdbm_nextkey (gfd, key);
      free (key.dptr);
    }
  printf ("There are %d items in the database.\n\n", temp);
  printf ("%d %d %d %d\n", n[0], n[1], n[2], n[3]);
  printf("nn %d\n",nn);
  key.dptr = NULL;
}
