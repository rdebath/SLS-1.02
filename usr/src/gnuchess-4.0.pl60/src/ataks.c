/*
 * ataks.c - C source for GNU CHESS
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
void
ataks (short int side, short int *a)

/*
 * Fill array atak[][] with info about ataks to a square.  Bits 8-15 are set
 * if the piece (king..pawn) ataks the square.  Bits 0-7 contain a count of
 * total ataks to the square.
 */

{
  register short u, c, sq;
  register unsigned char *ppos, *pdir;
  short i, piece, *PL;

#ifdef NOMEMSET
  for (u = 64; u; a[--u] = 0) ;
#else
  memset ((char *) a, 0, 64 * sizeof (a[0]));
#endif /* NOMEMSET */
  PL = PieceList[side];
  for (i = PieceCnt[side]; i >= 0; i--)
    {
      sq = PL[i];
      piece = board[sq];
      c = control[piece];
      if (sweep[piece])
	{
	  ppos = nextpos[piece][sq];
	  pdir = nextdir[piece][sq];
	  u = ppos[sq];
	  do
	    {
	      a[u] = ((a[u]+1) | c);
	      u = ((color[u] == neutral) ? ppos[u] : pdir[u]);
	  } while (u != sq);
	}
      else
	{
	  pdir = nextdir[ptype[side][piece]][sq];
	  u = pdir[sq];		/* follow captures thread for pawns */
	  do
	    {
	      a[u] = ((a[u]+1) | c);
	      u = pdir[u];
	  } while (u != sq);
	}
    }
}
