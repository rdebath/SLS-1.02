/*
 * ataks.h - Header source for GNU CHESS
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
inline
static
int
SqAtakd (register short int sq, short int side)

/*
 * See if any piece with color 'side' ataks sq.  First check pawns then
 * Queen, Bishop, Rook and King and last Knight.
 */

{
  register short u;
  register unsigned char *ppos, *pdir;
  short xside;

  xside = side ^ 1;
  pdir = nextdir[ptype[xside][pawn]][sq];
  u = pdir[sq];			/* follow captures thread */
  if (u != sq)
    {
      if (board[u] == pawn && color[u] == side)
	return (true);
      u = pdir[u];
      if (u != sq && board[u] == pawn && color[u] == side)
	return (true);
    }
  /* king capture */
  if (distance (sq, PieceList[side][0]) == 1)
    return (true);
  /* try a queen bishop capture */
  ppos = nextpos[bishop][sq];
  pdir = nextdir[bishop][sq];
  u = ppos[sq];
  do
    {
      if (color[u] == neutral)
	u = ppos[u];
      else
	{
	  if (color[u] == side && (board[u] == queen || board[u] == bishop))
	    return (true);
	  u = pdir[u];
	}
  } while (u != sq);
  /* try a queen rook capture */
  ppos = nextpos[rook][sq];
  pdir = nextdir[rook][sq];
  u = ppos[sq];
  do
    {
      if (color[u] == neutral)
	u = ppos[u];
      else
	{
	  if (color[u] == side && (board[u] == queen || board[u] == rook))
	    return (true);
	  u = pdir[u];
	}
  } while (u != sq);
  /* try a knight capture */
  pdir = nextdir[knight][sq];
  u = pdir[sq];
  do
    {
      if (color[u] == side && board[u] == knight)
	return (true);
      u = pdir[u];
  } while (u != sq);
  return (false);
}
