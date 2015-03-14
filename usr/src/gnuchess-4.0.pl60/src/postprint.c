/*
 * postprint.c - C source for GNU CHESS
 *
 * Copyright (c) 1992 Free Software Foundation
 *
 * This file is part of GNU CHESS.
 *
 * GNU Chess is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * GNU Chess is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Chess; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#include <stdio.h>
#include "gnuchess.h"
#if !defined HASHFILE
#ifdef MSDOS
#define HASHFILE "gnuchess.has"
#else
#define HASHFILE "/usr/local/lib/gnuchess.hash"
#endif
#endif
#ifdef MSDOS
#include <stdlib.h>
#include <string.h>
#include <time.h>
#define RWA_ACC "r+b"
#define WA_ACC "w+b"
#else
#define RWA_ACC "r+"
#define WA_ACC "w+"
#include <sys/param.h>
#include <sys/types.h>
#endif /* MSDOS */
FILE *hashfile;

#define truescore 0x0001
#define lowerbound 0x0002
#define upperbound 0x0004
#define kingcastle 0x0008
#define queencastle 0x0010

long i, j;
int nr[MAXDEPTH];
struct fileentry n;
int r, c;
char line[128];
char *l;
short int t;
int cc1, cc2;
char mvstr[3][6];
short int board[64];
void
algbr (short int f, short int t, short int flag)

/*
 * Generate move strings in different formats.
 */

{
  int m3p;

  if (f != t)
    {
      /* algebraic notation */
      mvstr[0][0] = Cxx[column (f)];
      mvstr[0][1] = Rxx[row (f)];
      mvstr[0][2] = Cxx[column (t)];
      mvstr[0][3] = Rxx[row (t)];
      mvstr[0][4] = mvstr[3][0] = '\0';
      if (((mvstr[1][0] = Pxx[board[f]]) == 'P') || (flag & promote))
	{
	  if (mvstr[0][0] == mvstr[0][2])
	    {			/* pawn did not eat */
	      mvstr[2][0] = mvstr[1][0] = mvstr[0][2];	/* to column */
	      mvstr[2][1] = mvstr[1][1] = mvstr[0][3];	/* to row */
	      m3p = 2;
	    }
	  else
	    /* pawn ate */
	    {
	      mvstr[2][0] = mvstr[1][0] = mvstr[0][0];	/* from column */
	      mvstr[2][1] = mvstr[1][1] = mvstr[0][2];	/* to column */
	      mvstr[2][2] = mvstr[0][3];
	      m3p = 3;		/* to row */
	    }
	  mvstr[2][m3p] = mvstr[1][2] = '\0';
	  if (flag & promote)
	    {
	      mvstr[0][4] = mvstr[1][2] = mvstr[2][m3p] = Qxx[flag & pmask];
	      mvstr[1][3] = mvstr[2][m3p + 1] = mvstr[0][5] = '\0';
	    }
	}
      else
	/* not a pawn */
	{
	  mvstr[2][0] = mvstr[1][0];
	  mvstr[2][1] = mvstr[0][1];
	  mvstr[2][2] = mvstr[1][1] = mvstr[0][2];	/* to column */
	  mvstr[2][3] = mvstr[1][2] = mvstr[0][3];	/* to row */
	  mvstr[2][4] = mvstr[1][3] = '\0';
	  strcpy (mvstr[3], mvstr[2]);
	  mvstr[3][1] = mvstr[0][0];
	  if (flag & cstlmask)
	    {
	      if (t > f)
		{
		  strcpy (mvstr[1], "o-o");
		  strcpy (mvstr[2], "O-O");
		}
	      else
		{
		  strcpy (mvstr[1], "o-o-o");
		  strcpy (mvstr[2], "O-O-O");
		}
	    }
	}
    }
  else
    mvstr[0][0] = mvstr[1][0] = mvstr[2][0] = mvstr[3][0] = '\0';
}

void
main (int argc, char **argv)
{
  int f = 0;
  char flbuf[10];
  char *fl;

  if ((hashfile = fopen (HASHFILE, RWA_ACC)) == NULL)
    exit (1);
  for (i = 0; i < MAXDEPTH; i++)
    nr[i] = 0;
  fseek (hashfile, 0L, SEEK_END);
  i = ftell (hashfile) / sizeof (struct fileentry);
  fseek (hashfile, 0L, SEEK_SET);
  printf ("/V 11 72 mul def /L 60 def\n");
  for (j = 0; j < i; j++)
    {
      fread (&n, sizeof (struct fileentry), 1, hashfile);
      if (n.depth)
	{
	  nr[0]++;
	  if (nr[0] == 19)
	    {
	      nr[0] = 1;
	      printf ("showpage\n/V 11 72 mul def\n");
	      printf ("/L 60 def\n");
	      f = 0;
	    }
	  /* now process this entry */
	  strcpy (line, "C ('#[");
	  for (r = 0; r < 8; r++)
	    {
	      l = line + 6 + (7 - r) * 9;
	      for (c = 0; c < 4; c++)
		{
		  cc1 = (n.bd[r * 4 + c] >> 4) & 0xf;
		  cc2 = n.bd[r * 4 + c] & 0xf;
		  board[r * 8 + c * 2] = (int) cc1 & 0x7;
		  board[r * 8 + c * 2 + 1] = (int) cc2 & 0x7;
		  if (cc1 & 0x8)
		    *l++ = Qxx[cc1 & 0x7];
		  else
		    *l++ = Pxx[cc1 & 0x7];
		  if (cc2 & 0x8)
		    *l++ = Qxx[cc2 & 0x7];
		  else
		    *l++ = Pxx[cc2 & 0x7];
		}
	      *l++ = ';';
	    }
	  l--;
	  line[79] = '\0';
	  strcat (line, "]') show");
	  algbr (n.f, n.t, 0);
	  t = (n.sh << 8) + n.sl;
	  /* decode flags */
	  fl = flbuf;
	  if (n.flags & kingcastle)
	    *fl++ = 'k';
	  if (n.flags & queencastle)
	    *fl++ = 'q';
	  if (n.flags & truescore)
	    *fl++ = 't';
	  if (n.flags & lowerbound)
	    *fl++ = 'l';
	  if (n.flags & upperbound)
	    *fl++ = 'u';
	  *fl = '\0';
	  printf ("L V moveto\n");
	  printf ("R (%s flags %s depth %d score %d", mvstr[0], flbuf, n.depth, t);
	  printf (") show\n");
	  printf ("L  V 100 sub moveto\n");
	  printf ("%s\n", line);
	  f++;
	  if (f == 3)
	    {
	      printf ("/V V 120 sub def /L 60 def\n");
	      f = 0;
	    }
	  else
	    printf ("/L 160 L add def\n");
	}
    }
  if (nr[0])
    printf ("showpage\n");
}
