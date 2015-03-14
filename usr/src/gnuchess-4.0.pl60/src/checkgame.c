/*
 * checkgame.c - check a chess.lst file for illegal moves
 *
 * Usage: checkgame filename
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
#include <sys/times.h>
#endif /* MSDOS */
FILE *fd;

#define truescore 0x0001
#define lowerbound 0x0002
#define upperbound 0x0004
#define kingcastle 0x0008
#define queencastle 0x0010
const short otherside[3] =
{black, white, neutral};

struct GameRec GameList[512];
char mvstr[4][6];
long i, j;
short int ep;
int r, c;
char line[128];
char *l;
short int board[64];
short int color[64];
short int GameCnt;
int from, to;
char *InPtr;
int ckcastld[2];
short int epsquare = -1;
int ok;
int mvptr = 0;
struct leaf Tree[256];

/* .... MOVE GENERATION VARIABLES AND INITIALIZATIONS .... */

const short kingP[3] =
{4, 60, 0};
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
short board[64], color[64];

/*
 * nextpos[piece][from-square] , nextdir[piece][from-square] gives vector of
 * positions reachable from from-square in ppos with piece such that the
 * sequence	ppos = nextpos[piece][from-square]; pdir =
 * nextdir[piece][from-square]; u = ppos[sq]; do { u = ppos[u]; if(color[u]
 * != neutral) u = pdir[u]; } while (sq != u); will generate the sequence of
 * all squares reachable from sq.
 *
 * If the path is blocked u = pdir[sq] will generate the continuation of the
 * sequence in other directions.
 */

unsigned char nextpos[8][64][64];
unsigned char nextdir[8][64][64];

/*
 * ptype is used to separate white and black pawns, like this; ptyp =
 * ptype[side][piece] piece can be used directly in nextpos/nextdir when
 * generating moves for pieces that are not black pawns.
 */
const short ptype[2][8] =
{
  { no_piece, pawn, knight, bishop, rook, queen, king, no_piece },
  { no_piece, bpawn, knight, bishop, rook, queen, king, no_piece } };

/* data used to generate nextpos/nextdir */
static const short direc[8][8] =
{
  { 0, 0, 0, 0, 0, 0, 0, 0 },
  { 10, 9, 11, 0, 0, 0, 0, 0 },
  { 8, -8, 12, -12, 19, -19, 21, -21 },
  { 9, 11, -9, -11, 0, 0, 0, 0 },
  { 1, 10, -1, -10, 0, 0, 0, 0 },
  { 1, 10, -1, -10, 9, 11, -9, -11 },
  { 1, 10, -1, -10, 9, 11, -9, -11 },
  { -10, -9, -11, 0, 0, 0, 0, 0 } };
static const short max_steps[8] =
{0, 2, 1, 7, 7, 7, 1, 2};
static const short nunmap[120] =
{
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, 0, 1, 2, 3, 4, 5, 6, 7, -1,
  -1, 8, 9, 10, 11, 12, 13, 14, 15, -1,
  -1, 16, 17, 18, 19, 20, 21, 22, 23, -1,
  -1, 24, 25, 26, 27, 28, 29, 30, 31, -1,
  -1, 32, 33, 34, 35, 36, 37, 38, 39, -1,
  -1, 40, 41, 42, 43, 44, 45, 46, 47, -1,
  -1, 48, 49, 50, 51, 52, 53, 54, 55, -1,
  -1, 56, 57, 58, 59, 60, 61, 62, 63, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};

int InitFlag = false;


void
DISP (void)
{

  short r, c, l;

  if (true)
    {
      printf ("\n");
      for (r = 7; r >= 0; r--)
	{
	  for (c = 0; c <= 7; c++)
	    {
	      l = locn (r, c);
	      if (color[l] == neutral)
		printf (" -");
	      else if (color[l] == white)
		printf (" %c", Qxx[board[l]]);
	      else
		printf (" %c", Pxx[board[l]]);
	    }
	  printf ("\n");
	}
      printf ("\n");
    }
}

int
castle (short int side, short int kf, short int kt, short int iop)

/* Make or Unmake a castling move. */

{
  register short rf, rt, xside;

  xside = otherside[side];
  if (kt > kf)
    {
      rf = kf + 3;
      rt = kt - 1;
    }
  else
    {
      rf = kf - 4;
      rt = kt + 1;
    }
  if (kf != kingP[side] ||
      board[kf] != king ||
      board[rf] != rook ||
      color[kt] != neutral ||
      color[rt] != neutral ||
      color[kt - 1] != neutral)
    return (false);
  else
    return (true);
}

void
Initialize_moves (void)

/*
 * This procedure pre-calculates all moves for every piece from every square.
 * This data is stored in nextpos/nextdir and used later in the move
 * generation routines.
 */

{
  short ptyp, po, p0, d, di, s, delta;
  unsigned char *ppos, *pdir;
  short dest[8][8];
  short steps[8];
  short sorted[8];

  for (ptyp = 0; ptyp < 8; ptyp++)
    for (po = 0; po < 64; po++)
      for (p0 = 0; p0 < 64; p0++)
	{
	  nextpos[ptyp][po][p0] = (unsigned char) po;
	  nextdir[ptyp][po][p0] = (unsigned char) po;
	}
  for (ptyp = 1; ptyp < 8; ptyp++)
    for (po = 21; po < 99; po++)
      if (nunmap[po] >= 0)
	{
	  ppos = nextpos[ptyp][nunmap[po]];
	  pdir = nextdir[ptyp][nunmap[po]];
	  /* dest is a function of direction and steps */
	  for (d = 0; d < 8; d++)
	    {
	      dest[d][0] = nunmap[po];
	      delta = direc[ptyp][d];
	      if (delta != 0)
		{
		  p0 = po;
		  for (s = 0; s < max_steps[ptyp]; s++)
		    {
		      p0 = p0 + delta;

		      /*
		       * break if (off
		       * board) or (pawns
		       * only move two
		       * steps from home
		       * square)
		       */
		      if ((nunmap[p0] < 0) || (((ptyp == pawn) || (ptyp == bpawn))
					       && ((s > 0) && ((d > 0) || (Stboard[nunmap[po]] != pawn)))))
			break;
		      else
			dest[d][s] = nunmap[p0];
		    }
		}
	      else
		s = 0;

	      /*
	       * sort dest in number of steps order
	       * currently no sort is done due to
	       * compability with the move
	       * generation order in old gnu chess
	       */
	      steps[d] = s;
	      for (di = d; s > 0 && di > 0; di--)
		if (steps[sorted[di - 1]] == 0)	/* should be: < s */
		  sorted[di] = sorted[di - 1];
		else
		  break;
	      sorted[di] = d;
	    }

	  /*
	   * update nextpos/nextdir, pawns have two
	   * threads (capture and no capture)
	   */
	  p0 = nunmap[po];
	  if (ptyp == pawn || ptyp == bpawn)
	    {
	      for (s = 0; s < steps[0]; s++)
		{
		  ppos[p0] = (unsigned char) dest[0][s];
		  p0 = dest[0][s];
		}
	      p0 = nunmap[po];
	      for (d = 1; d < 3; d++)
		{
		  pdir[p0] = (unsigned char) dest[d][0];
		  p0 = dest[d][0];
		}
	    }
	  else
	    {
	      pdir[p0] = (unsigned char) dest[sorted[0]][0];
	      for (d = 0; d < 8; d++)
		for (s = 0; s < steps[sorted[d]]; s++)
		  {
		    ppos[p0] = (unsigned char) dest[sorted[d]][s];
		    p0 = dest[sorted[d]][s];
		    if (d < 7)
		      pdir[p0] = (unsigned char) dest[sorted[d + 1]][0];

		    /*
		     * else is already
		     * initialized
		     */
		  }
	    }
	}
}

#define Link(from,to,flag,s) \
{\
   node->f = from; node->t = to;\
     node->reply = 0;\
       node->flags = flag;\
	 node->score = s;\
	   ++node;\
	     ++mvptr;\
	     }

inline void
LinkMove (short int ply,
	  short int f,
	  short int t,
	  short int flag,
	  short int xside)

/*
 * Add a move to the tree.  Assign a bonus to order the moves as follows: 1.
 * Principle variation 2. Capture of last moved piece 3. Other captures
 * (major pieces first) 4. Killer moves 5.
 */

{
  register short s;
  register unsigned short mv;
  register struct leaf *node;

  s = 0;
  node = &Tree[mvptr];
  mv = (f << 8) | t;
  if (row (t) == 0 || row (t) == 7)
    {
      flag |= promote;
      Link (f, t, flag | queen, s - 20000);
      Link (f, t, flag | knight, s - 20000);
      Link (f, t, flag | rook, s - 20000);
      flag |= bishop;
    }
  else if (row (t) == 1 || row (t) == 6)
    {
      flag |= pwnthrt;
    }
  Link (f, t, flag, s - 20000);
}


void
GenMoves (register short int ply, register short int sq, short int side, short int xside)

/*
 * Generate moves for a piece. The moves are taken from the precalulated
 * array nextpos/nextdir. If the board is free, next move is choosen from
 * nextpos else from nextdir.
 */

{
  register short u, piece;
  register unsigned char *ppos, *pdir;

  mvptr = 0;
  piece = board[sq];
  ppos = nextpos[ptype[side][piece]][sq];
  pdir = nextdir[ptype[side][piece]][sq];
  if (piece == pawn)
    {
      u = ppos[sq];		/* follow no captures thread */
      if (color[u] == neutral)
	{
	  LinkMove (ply, sq, u, 0, xside);
	  u = ppos[u];
	  if (color[u] == neutral)
	    LinkMove (ply, sq, u, 0, xside);
	}
      u = pdir[sq];		/* follow captures thread */
      if (color[u] == xside)
	LinkMove (ply, sq, u, capture, xside);
      else if (u == epsquare)
	LinkMove (ply, sq, u, capture | epmask, xside);
      u = pdir[u];
      if (color[u] == xside)
	LinkMove (ply, sq, u, capture, xside);
      else if (u == epsquare)
	LinkMove (ply, sq, u, capture | epmask, xside);

    }
  else
    {
      u = ppos[sq];
      do
	{
	  if (color[u] == neutral)
	    {
	      LinkMove (ply, sq, u, 0, xside);
	      u = ppos[u];
	    }
	  else
	    {
	      if (color[u] == xside)
		LinkMove (ply, sq, u, capture, xside);
	      u = pdir[u];
	    }
      } while (u != sq);
    }
}
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
  /* return (0); */
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
	      mvstr[3][0] = mvstr[0][0];	/* Allow e7e8 for
						 * chesstool */
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
      if (fb[0] == '\n' || fb[0] == '\r') return;
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
int
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
      if ((row (f) == 3 &&
	   row (t) == 2) || (row (f) == 4 && row (t) == 5))
	{
	  if (column (t) != column (f))
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
  if ((mv == BLACKCASTLE) || (mv == WHITECASTLE) || (mv == LONGBLACKCASTLE) || (mv == LONGWHITECASTLE))
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
      if ((board[t] == king && color[t] == side) && (board[rf] == rook) && (color[rf] == side))
	{
	  board[rt] = rook;
	  color[rt] = side;
	  board[rf] = no_piece;
	  color[rf] = neutral;
	  ckcastld[side] = true;
	}
    }
  else if (GameList[i].flags & promote)

    board[t] = GameList[i].flags & pmask;
}

void
main (int argc, char **argv)
{
  int from, to;
  unsigned short int mv;
  int start, end;
  int ii, kf, jj;

  Initialize_moves ();
  ckcastld[0] = ckcastld[1] = false;

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
  GetGame ();
  if (!start || start < 1 || start > GameCnt)
    start = 1;
  if (!end || end > GameCnt || end < 1)
    end = GameCnt;
  side = white;
  xside = black;
  for (i = 1; i < end; i++)
    {
      mv = GameList[i].gmove;
      from = mv >> 8 & 0x7F;
      to = mv & 0x7F;
      algbr (from, to, 0);
      if (side)
	printf ("%s\n", mvstr[0]);
      else
	{
	  printf ("%d. ", 1 + ((i - 1) / 2));
	  printf ("%s  ", mvstr[0]);
	}

      GenMoves (0, from, side, xside);
      if (!ckcastld[side] && board[from] == king && color[from] == side)
	{
	  if (castle (side, from, from + 2, 0))
	    {
	      LinkMove (0, from, from + 2, cstlmask, xside);
	    }
	  if (castle (side, from, from - 2, 0))
	    {
	      LinkMove (0, from, from - 2, cstlmask, xside);
	    }
	}
      ok = false;
      for (ii = 0; ii < mvptr; ii++)
	{
	  if (from == Tree[ii].f && to == Tree[ii].t)
	    {
	      ok = true;
	      break;
	    }
	}
      if (!ok)
	{
	  algbr (from, to, 0);
	  printf ("Illegal move %s\n", mvstr[0]);
	  for (ii = 0; ii < mvptr; ii++)
	    {
	      algbr (Tree[ii].f, Tree[ii].t, 0);
	      printf (" %s\n", mvstr[0]);
	    }
	  DISP ();
	  exit (1);
	}
      getboard ((int)i);
      if (board[to] == pawn)
	if (to - from == 16)
	  epsquare = from + 8;
	else if (from - to == 16)
	  epsquare = from - 8;
	else
	  epsquare = -1;
      kf = -1;
      for (ii = 0; ii < 64; ii++)
	{
	  if ((board[ii] == king) && (color[ii] == side))
	    {
	      kf = ii;
	      break;
	    }
	}
      if (kf < 0)
	{
	  printf ("Badnews: you have no king\n");
	  DISP ();
	  exit (1);
	}
      for (ii = 0; ii < 64; ii++)
	{
	  if (color[ii] == xside)
	    {
	      mvptr = 0;
	      GenMoves (0, ii, xside, side);
	      for (jj = 0; jj < mvptr; jj++)
		{
		  if (Tree[jj].t == kf)
		    {

		      printf ("Badnews: you are in check\n");
		      printf ("king is at %d %d\n", row (kf), column (kf));
		      algbr (Tree[jj].f, Tree[jj].t, 0);
		      printf ("move is %s\n", mvstr[0]);
		      DISP ();
		      exit (1);
		    }
		}
	    }
	}
      xside = side;
      side = otherside[side];
    }
  printf ("\n\n");
  printf ("Final board:\n\n");
  DISP ();
  exit (0);
}
