/*
 * postprint.c - C source for GNU CHESS
 *
 * Copyright (c) 1988,1989,1990 John Stanback
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
FILE *fd;

#define truescore 0x0001
#define lowerbound 0x0002
#define upperbound 0x0004
#define kingcastle 0x0008
#define queencastle 0x0010
const short otherside[3] =
{black, white, neutral};
const short Stboard[64] =
{rook, knight, bishop, queen, king, bishop, knight, rook,
 pawn, pawn, pawn, pawn, pawn, pawn, pawn, pawn,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
 pawn, pawn, pawn, pawn, pawn, pawn, pawn, pawn,
 rook, knight, bishop, queen, king, bishop, knight, rook};
const short Stcolor[64] =
{white, white, white, white, white, white, white, white,
 white, white, white, white, white, white, white, white,
 neutral, neutral, neutral, neutral, neutral, neutral, neutral, neutral,
 neutral, neutral, neutral, neutral, neutral, neutral, neutral, neutral,
 neutral, neutral, neutral, neutral, neutral, neutral, neutral, neutral,
 neutral, neutral, neutral, neutral, neutral, neutral, neutral, neutral,
 black, black, black, black, black, black, black, black,
 black, black, black, black, black, black, black, black};

struct GameRec GameList[512];
char mvstr[4][6];
long i, j;
int nr;
short int ep;
int r, c;
char line[128];
char *l;
short int board[64];
short int color[64];
short int GameCnt;
int from, to;
char *InPtr;

void
skip ()
{
  while (*InPtr != ' ')
    InPtr++;
  while (*InPtr == ' ')
    InPtr++;
}
void
skipb ()
{
  while (*InPtr == ' ')
    InPtr++;
}
int
parser (char *f, int side, unsigned short *flags)
{
  int c1, r1, c2, r2;

  *flags = 0;

  if (f[4] == 'o')
    if (side == black)
      return 0x3C3A;
    else
      return 0x0402;
  else if (f[0] == 'o')
    if (side == black)
      return 0x3C3E;
    else
      return 0x0406;
  else
    {
      c1 = f[0] - 'a';
      r1 = f[1] - '1';
      c2 = f[2] - 'a';
      r2 = f[3] - '1';
      if (f[4] != ' ')
	{
	  /* promotion */
	  for (i = 0; i < 7; i++)
	    if (f[4] == Qxx[i])
	      {
		*flags = (unsigned short)i | promote;
		break;
	      }
	}
      return (locn (r1, c1) << 8) | locn (r2, c2);
    }
  return (0);
}

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
	  if (mvstr[0][0] == mvstr[0][2])	/* pawn did not eat */
	    {
	      mvstr[2][0] = mvstr[1][0] = mvstr[0][2];	/* to column */
	      mvstr[2][1] = mvstr[1][1] = mvstr[0][3];	/* to row */
	      m3p = 2;
	    }
	  else
	    /* pawn ate */
	    {
	      mvstr[2][0] = mvstr[1][0] = mvstr[0][0];	/* column */
	      mvstr[2][1] = mvstr[1][1] = mvstr[0][2];	/* to column */
	      mvstr[2][2] = mvstr[0][3];
	      m3p = 3;		/* to row */
	    }
	  if (flag & promote)
	    {
	      mvstr[0][4] = mvstr[1][2] = mvstr[2][m3p] = Qxx[flag & pmask];
	      mvstr[1][3] = mvstr[2][m3p + 1] = mvstr[0][5] = '\0';
#ifdef CHESSTOOL
	      mvstr[3][0] = mvstr[0][0];	/* Allow e7e8 for chesstool */
	      mvstr[3][1] = mvstr[0][1];
	      mvstr[3][2] = mvstr[0][2];
	      mvstr[3][3] = mvstr[0][3];
	      mvstr[3][4] = '\0';
#endif
	    }
	  mvstr[2][m3p] = mvstr[1][2] = '\0';
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
GetGame ()
{
  char fb[256];
  unsigned short flags;

  fgets (fb, 256, fd);
  fgets (fb, 256, fd);
  while (fgets (fb, 256, fd))
    {
      struct GameRec *g;
      int side = white;

      side = otherside[side];
      if (fb[0] == '\n')
	return;
      ++GameCnt;
      InPtr = fb;
      skipb ();
      g = &GameList[GameCnt];
      g->gmove = parser (InPtr, side, &flags);
      skip ();
      g->score = atoi (InPtr);
      skip ();
      g->depth = atoi (InPtr);
      skip ();
      g->nodes = atol (InPtr);
      skip ();
      g->time = atol (InPtr);
      g->flags = flags;
      skip ();
      ++GameCnt;
      g = &GameList[GameCnt];
      g->gmove = parser (InPtr, side, &flags);
      skip ();
      g->score = atoi (InPtr);
      skip ();
      g->depth = atoi (InPtr);
      skip ();
      g->nodes = atol (InPtr);
      skip ();
      g->time = atol (InPtr);
      g->flags = flags;

    }
}
short int xside, side;
void
getboard (int mvno)

{
  register short int f, t;
  short int rf, rt;
  unsigned short mv;

  /* now update the board and hash values */

  /*
   * should really check the moves as we do this, but???
   */
  mv = GameList[mvno].gmove;
  f = mv >> 8 & 0x7F;
  t = mv & 0xFF;
  /* can only capture other side */
  if (board[t] != no_piece)
    {
      if (color[t] != xside)
	{
	  algbr (f, t, 0);
	  printf ("Illegal move - %d %s \n", mvno, mvstr[0]);
	}
    }
  /* there must be a piece to move */
  if (board[f] == no_piece || color[f] != side)
    {
      algbr (f, t, 0);
      printf ("Illegal move + %d %s \n", mvno, mvstr[0]);
    }
  /* is it EnPassant */
  if (board[f] == pawn && board[t] == no_piece)
    {
      if ((row (f) == 4 && row (t) == 3) || (row (f) == 5 && row (t) == 6))
	{
	  if ((column (t) == column (f) + 1)
	      || (column (t) == column (f) - 1))
	    {
	      ep = t + ((t > f) ? -8 : 8);
	      if (board[ep] == pawn && color[ep] == xside)
		{
		  board[ep] = no_piece;
		  color[ep] = neutral;
		}
	    }
	}
    }
  board[t] = board[f];
  color[t] = color[f];
  color[f] = neutral;
  board[f] = no_piece;
  /* castle moves */
  if ((board[t] == king) & ((mv == BLACKCASTLE) || (mv == WHITECASTLE) || (mv == LONGBLACKCASTLE) || (mv == LONGWHITECASTLE)))
    {

      if (t > f)
	{
	  rf = f + 3;
	  rt = t - 1;
	}
      else
	{
	  rf = f - 4;
	  rt = t + 1;
	}
      board[rt] = rook;
      color[rt] = side;
      board[rf] = no_piece;
      color[rf] = neutral;
    }
  else if (GameList[i].flags & promote)

    board[t] = GameList[i].flags & pmask;
  xside = side;
  side = otherside[side];
}

void
main (int argc, char **argv)
{
  int from, to;
  int f = 0;
  unsigned short int mv;
  int start, end;

  if (argc > 4 || argc < 2)
    {
      printf ("Usage: game file [start [end] ] \n");
      exit (1);
    }
  start = end = 0;
  if (argc > 2)
    start = (atoi (argv[2]) * 2) - 1;
  if (argc == 4)
    end = (atoi (argv[3]) * 2) - 1;
  side = white;
  xside = black;
  for (i = 0; i < 64; i++)
    {
      board[i] = Stboard[i];
      color[i] = Stcolor[i];
    }
  i = 1;
  if ((fd = fopen (argv[1], RWA_ACC)) == NULL)
    exit (1);
  printf ("/V 11 72 mul def /L 60 def\n");
  GetGame ();
  if (!start || start < 1 || start > GameCnt)
    start = 1;
  if (!end || end > GameCnt || end < 1)
    end = GameCnt;
  for (i = 1; i < end; i++)
    {
      getboard ((int)i);
      if (i < start)
	continue;
      nr++;
      if (nr == 19)
	{
	  nr = 1;
	  printf ("showpage\n/V 11 72 mul def\n");
	  printf ("/L 60 def\n");
	  f = 0;
	}
      /* now process this entry */
      strcpy (line, "C ('#[");
      for (r = 0; r < 8; r++)
	{
	  l = line + 6 + (7 - r) * 9;
	  for (c = 0; c < 8; c++)
	    {
	      if (color[r * 8 + c] == black)
		*l++ = Qxx[board[r * 8 + c]];
	      else
		*l++ = Pxx[board[r * 8 + c]];
	    }
	  *l++ = ';';
	}
      l--;
      line[79] = '\0';
      strcat (line, "]') show");
      /* decode flags */
      printf ("L V moveto\n");
      mv = GameList[i].gmove;
      from = mv >> 8 & 0x7F;
      to = mv & 0x7F;
      algbr (from, to, 0);
      if (i % 2)
	printf ("R (%d %s score %d time %d", (i + 1) / 2, mvstr[0], GameList[i].score, GameList[i].time);
      else
	printf ("R (%d  ... %s score %d time %d", (i + 1) / 2, mvstr[0], GameList[i].score, GameList[i].time);
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

  if (nr)
    printf ("showpage\n");
  exit (0);
}
