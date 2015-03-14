#include <stdio.h>                                 /*      SORT.H */

#define SP ' '
#define LF '\n'

typedef struct memblk {
	struct memblk *nxtmem;
	char   *memend;
} MEMHDR;

typedef struct srtblk {
	struct srtblk *nxtblk;
	struct srtblk *prvblk;
	short   insno;
	short   pcnt;
	float   p1val;
	float   p2val;
	float   p3val;
	float   newp2;
	float   newp3;
	char    preced;
	char    text[9];
} SRTBLK;

extern  SRTBLK *frstbp;
extern  int    sectcnt;
extern  FILE   *SCOREIN;
extern  FILE   *SCOREOUT;
