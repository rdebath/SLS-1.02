/*
 * genmoves.c - C source for GNU CHESS
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
#include "gnuchess.h"
short *TrP;

#define Link(from,to,flag,s) \
{\
   node->f = from; node->t = to;\
     node->reply = 0;\
       node->flags = flag;\
	 node->score = s;\
	   ++node;\
	     (*TrP)++;\
	     }

inline void
LinkMove (short int ply, short int f,
	  register short int t,
	  short int flag,
	  short int xside)

/*
 * Add a move to the tree.  Assign a bonus to order the moves as follows: 1.
 * Principle variation 2. Capture of last moved piece 3. Other captures
 * (major pieces first) 4. Killer moves 5.
 */

{
  register short s = 0;
#if defined HISTORY
  register short z;
#endif
  register unsigned short mv;
  register struct leaf *node;

  node = &Tree[*TrP];
  mv = (f << 8) | t;
#ifdef KILLT
  s += killt[mv | sidebit];
#endif
#ifdef HISTORY
  z = mv;
  if (xside == white) z |= 0x4000;
  s += history[z];
#endif
  if (color[t] != neutral)
    {
      /* TOsquare is the square the last piece moved moved to */
      s +=  value[board[t]] - board[f] + ((t == TOsquare) ? 500 : 0);
    }
  if (board[f] == pawn)
    if (row (t) == 0 || row (t) == 7)
      {
	flag |= promote;
	s += 800;
#if !defined OLDXBOARD  && !defined GNU3 && !defined CHESSTOOL
	Link (f, t, flag | queen, s - 20000);
	s -= 200;
	Link (f, t, flag | knight, s - 20000);
	s -= 50;
	Link (f, t, flag | rook, s - 20000);
	flag |= bishop;
	s -= 50;
#else
	flag |= queen;
#endif
      }
    else if (row (t) == 1 || row (t) == 6)
      {
	flag |= pwnthrt;
	s += 600;
      }
    else if ((row(t) == ((color[f] == white)?5:2)) && (ply > MINDEPTH) && (ply < Sdepth+3))
      {
	if ((mtl[white] - pmtl[white] + mtl[black] - pmtl[black]) < PTVALUE)
	  {
	    flag |= pwnthrt;
	    s += 400;
	  }
      }
  Link (f, t, flag, s - 20000);
}

inline
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

  TrP = &TrPnt[ply + 1];
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
      u = pdir[u];
      if (color[u] == xside)
	LinkMove (ply, sq, u, capture, xside);
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
MoveList (short int side, register short int ply)

/*
 * Fill the array Tree[] with all available moves for side to play. Array
 * TrPnt[ply] contains the index into Tree[] of the first move at a ply.
 */

{
  register short i, xside, f;

  xside = side ^ 1;
  TrP = &TrPnt[ply + 1];
  *TrP = TrPnt[ply];
  if (!PV)
    Swag0 = killr0[ply];
   else Swag0 = PV;
  Swag1 = killr1[ply];
  Swag2 = killr2[ply];
  Swag3 = killr3[ply];
  if (ply > 2)
    Swag4 = killr1[ply - 2]; else Swag4 = 0;
#ifdef KILLT
  sidebit = ((side == white) ? 0 : 0x80);
  killt[SwagHt | sidebit] += 5000;
  killt[Swag0 | sidebit] += 2000;
  killt[Swag1 | sidebit] += 60;
  killt[Swag2 | sidebit] += 50;
  killt[Swag3 | sidebit] += 40;
  killt[Swag4 | sidebit] += 30;
#endif
#ifdef HISTORY
  i = (side == black)?0x4000:0;
  history[SwagHt | i] += 5000;
  history[Swag0 | i] += 2000;
  history[Swag1 | i] += 60;
  history[Swag2 | i] += 50;
  history[Swag3 | i] += 40;
  history[Swag4 | i] += 30;
#endif
  for (i = PieceCnt[side]; i >= 0; i--)
    GenMoves (ply, PieceList[side][i], side, xside);
  if (!castld[side])
    {
      f = PieceList[side][0];
      if (castle (side, f, f + 2, 0))
	{
	  LinkMove (ply, f, f + 2, cstlmask, xside);
	}
      if (castle (side, f, f - 2, 0))
	{
	  LinkMove (ply, f, f - 2, cstlmask, xside);
	}
    }
  if (epsquare > 0)
    {
      f = epmove1[epsquare];
      if (color[f] == side && board[f] == pawn)
	LinkMove (ply, f, epsquare, capture | epmask, xside);
      f = epmove2[epsquare];
      if (color[f] == side && board[f] == pawn)
	LinkMove (ply, f, epsquare, capture | epmask, xside);
    }
#ifdef KILLT
  killt[SwagHt | sidebit] -= 5000;
  killt[Swag0 | sidebit] -= 2000;
  killt[Swag1 | sidebit] -= 60;
  killt[Swag2 | sidebit] -= 50;
  killt[Swag3 | sidebit] -= 40;
  killt[Swag4 | sidebit] -= 30;
#endif
#ifdef HISTORY
 i = (side == black)?0x4000:0;
  history[SwagHt | i] -= 5000;
  history[Swag0 | i] -= 2000;
  history[Swag1 | i] -= 60;
  history[Swag2 | i] -= 50;
  history[Swag3 | i] -= 40;
  history[Swag4 | i] -= 30;
#endif
  SwagHt = 0;			/* SwagHt is only used once */
  GenCnt += (TrPnt[ply+1] - TrPnt[ply]);
}

void
CaptureList (register short int side, short int ply)

/*
 * Fill the array Tree[] with all available cature and promote moves for side
 * to play. Array TrPnt[ply] contains the index into Tree[] of the first move
 * at a ply.
 */

{
  register short u, sq, xside;
  register struct leaf *node;
  register unsigned char *ppos, *pdir;
  short i, piece, *PL, r7;

  xside = side ^ 1;
  TrP = &TrPnt[ply + 1];
  *TrP = TrPnt[ply];
  node = &Tree[*TrP];
  r7 = rank7[side];
  PL = PieceList[side];
#ifdef KILLT
  sidebit = ((side == white) ? 0 : 0x80);
  killt[SwagHt | sidebit] += 5000;
  killt[Swag0 | sidebit] += 2000;
  killt[Swag1 | sidebit] += 60;
  killt[Swag2 | sidebit] += 50;
  killt[Swag3 | sidebit] += 40;
  killt[Swag4 | sidebit] += 30;
#endif

  for (i = 0; i <= PieceCnt[side]; i++)
    {
      sq = PL[i];
      piece = board[sq];
      if (sweep[piece])
	{
	  ppos = nextpos[piece][sq];
	  pdir = nextdir[piece][sq];
	  u = ppos[sq];
	  do
	    {
	      if (color[u] == neutral)
		u = ppos[u];
	      else
		{
		  if (color[u] == xside)
		    Link (sq, u, capture, value[board[u]] + svalue[board[u]] - piece);
		  u = pdir[u];
		}
	  } while (u != sq);
	}
      else
	{
	  pdir = nextdir[ptype[side][piece]][sq];
	  if (piece == pawn && row (sq) == r7)
	    {
	      u = pdir[sq];
	      if (color[u] == xside)
		Link (sq, u, capture | promote | queen, valueQ);
	      u = pdir[u];
	      if (color[u] == xside)
		{
		  Link (sq, u, capture | promote | queen, valueQ);
#if !defined OLDXBOARD  && !defined GNU3 && !defined CHESSTOOL
		  Link (sq, u, capture | promote | knight, valueN);
		  Link (sq, u, capture | promote | rook, valueR);
		  Link (sq, u, capture | promote | bishop, valueB);
#endif
		}
	      ppos = nextpos[ptype[side][piece]][sq];
	      u = ppos[sq];	/* also generate non capture promote */
	      if (color[u] == neutral)
		{
		  Link (sq, u, promote | queen, valueQ);
#if !defined OLDXBOARD  && !defined GNU3 && !defined CHESSTOOL
		  Link (sq, u, promote | knight, valueN);
		  Link (sq, u, promote | rook, valueR);
		  Link (sq, u, promote | bishop, valueB);
#endif
		}
	    }
	  else
	    {
	      u = pdir[sq];
	      do
		{
		  if (color[u] == xside)
		    Link (sq, u, capture, value[board[u]] + svalue[board[u]] - piece);
		  u = pdir[u];
	      } while (u != sq);
	    }
	}
    }
#ifdef KILLT
  sidebit = ((side == white) ? 0 : 0x80);
  killt[SwagHt | sidebit] -= 5000;
  killt[Swag0 | sidebit] -= 2000;
  killt[Swag1 | sidebit] -= 60;
  killt[Swag2 | sidebit] -= 50;
  killt[Swag3 | sidebit] -= 40;
  killt[Swag4 | sidebit] -= 30;
#endif
  SwagHt = 0;			/* SwagHt is only used once */
}
