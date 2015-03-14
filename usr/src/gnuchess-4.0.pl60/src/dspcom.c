/*
 * dspcom.c - C source for GNU CHESS
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
#include "ataks.h"
#ifdef DEBUG40
int whichway;
#endif
#ifdef HASGETTIMEOFDAY
#include <sys/time.h>
#endif
extern short Mwpawn[64], Mbpawn[64], Mknight[2][64], Mbishop[2][64];
extern char *version, *patchlevel;
extern unsigned int TTadd;
char mvstr[4][6];
char *InPtr;
int	InBackground = false;


#include <ctype.h>
#include <signal.h>
#ifdef MSDOS
#include <dos.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#else
#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#endif

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
      mvstr[0][0] = cxx[column (f)];
      mvstr[0][1] = rxx[row (f)];
      mvstr[0][2] = cxx[column (t)];
      mvstr[0][3] = rxx[row (t)];
      mvstr[0][4] = mvstr[3][0] = '\0';
      if (((mvstr[1][0] = pxx[board[f]]) == 'P') || (flag & promote))
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
#if !defined CLIENT
	      mvstr[0][4] = mvstr[1][2] = mvstr[2][m3p] = qxx[flag & pmask];
	      mvstr[0][5] = mvstr[1][3] = mvstr[2][m3p + 1] = mvstr[3][0] = '\0';
#ifdef CHESSTOOL 
	      mvstr[3][0] = mvstr[0][0];	/* Allow e7e8 for chesstool */
	      mvstr[3][1] = mvstr[0][1];
	      mvstr[3][2] = mvstr[0][2];
	      mvstr[3][3] = mvstr[0][3];
	      mvstr[3][4] = '\0';
#endif
#endif
	    } else mvstr[2][m3p] = mvstr[1][2] = '\0';
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
		  strcpy (mvstr[1], mvstr[0]);
		  strcpy (mvstr[0], CP[5]);
		  strcpy (mvstr[2], CP[7]);
		}
	      else
		{
		  strcpy (mvstr[1], mvstr[0]);
		  strcpy (mvstr[0], CP[6]);
		  strcpy (mvstr[2], CP[8]);
		}
	    }
	}
    }
  else
    mvstr[0][0] = mvstr[1][0] = mvstr[2][0] = mvstr[3][0] = '\0';
}


int
VerifyMove (char *s, short int iop, short unsigned int *mv)

/*
 * Compare the string 's' to the list of legal moves available for the
 * opponent. If a match is found, make the move on the board.
 */

{
  static short pnt, tempb, tempc, tempsf, tempst, cnt;
  static struct leaf xnode;
  struct leaf *node;

  *mv = 0;
  if (iop == 2)
    {
      UnmakeMove (opponent, &xnode, &tempb, &tempc, &tempsf, &tempst);
      return (false);
    }
  cnt = 0;
  MoveList (opponent, 2);
  pnt = TrPnt[2];
  while (pnt < TrPnt[3])
    {
      node = &Tree[pnt++];
      algbr (node->f, node->t, (short) node->flags);
      if (strcmp (s, mvstr[0]) == 0 || strcmp (s, mvstr[1]) == 0 ||
	  strcmp (s, mvstr[2]) == 0 || strcmp (s, mvstr[3]) == 0)
	{
	  cnt++;
	  xnode = *node;
	}
    }
  if (cnt == 1)
    {
      MakeMove (opponent, &xnode, &tempb, &tempc, &tempsf, &tempst, &INCscore);
      if (SqAtakd (PieceList[opponent][0], computer))
	{
	  UnmakeMove (opponent, &xnode, &tempb, &tempc, &tempsf, &tempst);
#if defined CHESSTOOL
	  printz (CP[15]);
#else
#ifdef NONDSP
/* Illegal move in check */
	  printz (CP[77]);
	  printz ("\n");
#else
/* Illegal move in check */
	  ShowMessage (CP[77]);
#endif
#endif /* CHESSTOOL */
	  return (false);
	}
      else
	{
	  if (iop == 1)
	    return (true);
	  UpdateDisplay (xnode.f, xnode.t, 0, (short) xnode.flags);
	  if ((board[xnode.t] == pawn)
	      || (xnode.flags & capture)
	      || (xnode.flags & cstlmask))
	    {
	      Game50 = GameCnt;
	      ZeroRPT ();
	    }
	  GameList[GameCnt].depth = GameList[GameCnt].score = 0;
	  GameList[GameCnt].nodes = 0;
	  GameList[GameCnt].epssq = epsquare;
	  ElapsedTime (1);
	  GameList[GameCnt].time = (short) (et+50)/100;
	  if (TCflag)
	    {
	      TimeControl.clock[opponent] -= et;
	      timeopp[oppptr] = et;
	      --TimeControl.moves[opponent];
	    }
	  *mv = (xnode.f << 8) | xnode.t;
	  algbr (xnode.f, xnode.t, false);
	  return (true);
	}
    }
#if defined CHESSTOOL
  printz (CP[78]);
#else
#ifdef NONDSP
/* Illegal move */
  printz (CP[75], s);
#ifdef DEBUG8
  if (1)
    {
      FILE *D;
      int r, c, l;
      extern unsigned short int PrVar[];
      D = fopen ("/tmp/DEBUG", "a+");
      pnt = TrPnt[2];
      fprintf (D, "resp = %d\n", ResponseTime);
      fprintf (D, "iop = %d\n", iop);
      fprintf (D, "matches = %d\n", cnt);
      algbr (hint >> 8, hint & 0xff, (short) 0);
      fprintf (D, "hint %s\n", mvstr[0]);
      fprintf (D, "inout move is %s\n", s);
      for (r = 1; PrVar[r]; r++)
	{
	  algbr (PrVar[r] >> 8, PrVar[r] & 0xff, (short) 0);
	  fprintf (D, " %s", mvstr[0]);
	}
      fprintf (D, "\n");
      fprintf (D, "legal move are \n");
      while (pnt < TrPnt[3])
	{
	  node = &Tree[pnt++];
	  algbr (node->f, node->t, (short) node->flags);
	  fprintf (D, "%s %s %s %s\n", mvstr[0], mvstr[1], mvstr[2], mvstr[3]);
	}
      fprintf (D, "\n current board is\n");
      for (r = 7; r >= 0; r--)
	{
	  for (c = 0; c <= 7; c++)
	    {
	      l = locn (r, c);
	      if (color[l] == neutral)
		fprintf (D, " -");
	      else if (color[l] == white)
		fprintf (D, " %c", qxx[board[l]]);
	      else
		fprintf (D, " %c", pxx[board[l]]);
	    }
	  fprintf (D, "\n");
	}
      fprintf (D, "\n");
      fclose (D);
      abort ();
    }
#endif
#else
/* Illegal move */
  ShowMessage (CP[76]);
#endif
#endif /* CHESSTOOL */
#if !defined CHESSTOOL && !defined XBOARD
  if (cnt > 1)
    ShowMessage (CP[32]);
#endif /* CHESSTOOL */
  return (false);
}

int
parser (char *f, int side)
{
  int c1, r1, c2, r2;

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
      return (locn (r1, c1) << 8) | locn (r2, c2);
    }
  /*NOTREACHED*/
}

void
GetGame (void)
{
  FILE *fd;
  char fname[256], *p;
  int c, i, j;
  short sq;
/* enter file name */
  ShowMessage (CP[63]);
  scanz ("%s", fname);
/* chess.000 */
  if (fname[0] == '\0')
    strcpy (fname, CP[137]);
  if ((fd = fopen (fname, "r")) != NULL)
    {
      NewGame ();
      fgets (fname, 256, fd);
      computer = opponent = white;
      InPtr = fname;
      skip ();
      if (*InPtr == 'c')
	computer = black;
      else
	opponent = black;
      skip ();
      skip ();
      skip ();
      Game50 = atoi (InPtr);
      fgets (fname, 256, fd);
      InPtr = &fname[14];
      castld[white] = ((*InPtr == CP[214][0]) ? true : false);
      skip ();
      skip ();
      castld[black] = ((*InPtr == CP[214][0]) ? true : false);
      fgets (fname, 256, fd);
      InPtr = &fname[11];
      skipb ();
      TCflag = atoi (InPtr);
      skip ();
      InPtr += 14;
      skipb ();
      OperatorTime = atoi (InPtr);
      fgets (fname, 256, fd);
      InPtr = &fname[11];
      skipb ();
      TimeControl.clock[white] = atol (InPtr);
      skip ();
      skip ();
      TimeControl.moves[white] = atol (InPtr);
      fgets (fname, 256, fd);
      InPtr = &fname[11];
      skipb ();
      TimeControl.clock[black] = atoi (InPtr);
      skip ();
      skip ();
      TimeControl.moves[black] = atoi (InPtr);
      fgets (fname, 256, fd);
      for (i = 7; i > -1; i--)
	{
	  fgets (fname, 256, fd);
	  p = &fname[2];
	  InPtr = &fname[11];
	  skipb ();
	  for (j = 0; j < 8; j++)
	    {
	      sq = i * 8 + j;
	      if (*p == '.')
		{
		  board[sq] = no_piece;
		  color[sq] = neutral;
		}
	      else
		{
		  for (c = 0; c < 8; c++)
		    {
		      if (*p == pxx[c])
			{
			  board[sq] = c;
			  color[sq] = black;
			}
		    }
		  for (c = 0; c < 8; c++)
		    {
		      if (*p == qxx[c])
			{
			  board[sq] = c;
			  color[sq] = white;
			}
		    }
		}
	      p++;
	      Mvboard[sq] = atoi (InPtr);
	      skip ();
	    }
	}
      GameCnt = 0;
      flag.regularstart = true;
      Book = BOOKFAIL;
      fgets (fname, 256, fd);
      fgets (fname, 256, fd);
      fgets (fname, 256, fd);
      while (fgets (fname, 256, fd))
	{
	  struct GameRec *g;
	  int side = computer;

	  side = side ^ 1;
	  ++GameCnt;
	  InPtr = fname;
	  skipb ();
	  g = &GameList[GameCnt];
	  g->gmove = parser (InPtr, side);
	  skip ();
	  g->score = atoi (InPtr);
	  skip ();
	  g->depth = atoi (InPtr);
	  skip ();
	  g->nodes = atol (InPtr);
	  skip ();
	  g->time = atol (InPtr);
	  skip ();
	  g->flags = c = atoi (InPtr);
	  skip ();
	  g->hashkey = strtol (InPtr, (char **) NULL, 16);
	  skip ();
	  g->hashbd = strtol (InPtr, (char **) NULL, 16);
	  g->piece = no_piece;
	  g->color = neutral;
	  if (c & (capture | cstlmask))
	    {
	      if (c & capture)
		{
		  skip ();
		  for (c = 0; c < 8; c++)
		    if (pxx[c] == *InPtr)
		      break;
		  g->piece = c;
		}
	      skip ();
	      g->color = ((*InPtr == CP[119][0]) ? black : white);
	    }
	}
      if (TimeControl.clock[white] > 0)
	TCflag = true;
      fclose (fd);
    }
  ZeroRPT ();
  InitializeStats ();
  UpdateDisplay (0, 0, 1, 0);
  Sdepth = 0;
  hint = 0;
}

void
GetXGame (void)
{
  FILE *fd;
  char fname[256], *p;
  int c, i, j;
  short sq;
/* Enter file name */
  ShowMessage (CP[63]);
  scanz ("%s", fname);
  if (fname[0] == '\0')
/* xboard.position.read*/
    strcpy (fname, CP[205]);
  if ((fd = fopen (fname, "r")) != NULL)
    {
      NewGame ();
      flag.regularstart = false;
      Book = false;
      fgets (fname, 256, fd);
#ifdef notdef
      fname[6] = '\0';
      if (strcmp (fname, CP[206]))
	return;
#endif
      fgets (fname, 256, fd);
      fgets (fname, 256, fd);
      for (i = 7; i > -1; i--)
	{
	  fgets (fname, 256, fd);
	  p = fname;
	  for (j = 0; j < 8; j++)
	    {
	      sq = i * 8 + j;
	      if (*p == '.')
		{
		  board[sq] = no_piece;
		  color[sq] = neutral;
		}
	      else
		{
		  for (c = 0; c < 8; c++)
		    {
		      if (*p == qxx[c])
			{
			  board[sq] = c;
			  color[sq] = black;
			}
		    }
		  for (c = 0; c < 8; c++)
		    {
		      if (*p == pxx[c])
			{
			  board[sq] = c;
			  color[sq] = white;
			}
		    }
		}
	      p += 2;
	    }
	}
        if (fgets(fname, 256, fd) != NULL && strncmp(fname, "black", 5) == 0)
        {
          computer = white;
          opponent = black;
          xwndw = BXWNDW;
	}
      fclose (fd);
    }
  ZeroRPT ();
  InitializeStats ();
  UpdateDisplay (0, 0, 1, 0);
  Sdepth = 0;
  hint = 0;
}

void
SaveGame (void)
{
  FILE *fd;
  char fname[256];
  short sq, i, c, f, t;
  char p;

  if (savefile[0])
    strcpy (fname, savefile);
  else
    {
/* Enter file name*/
      ShowMessage (CP[63]);
      scanz ("%s", fname);
    }

  if (fname[0] == '\0')
/* chess.000 */
    strcpy (fname, CP[137]);
  if ((fd = fopen (fname, "w")) != NULL)
    {
      char *b, *w;

      b = w = CP[74];
      if (computer == black)
	b = CP[141];
      if (computer == white)
	w = CP[141];
      fprintf (fd, CP[37], b, w, Game50);
      fprintf (fd, CP[42], castld[white] ? CP[214] : CP[215], castld[black] ? CP[214] : CP[215]);
      fprintf (fd, CP[111], TCflag, OperatorTime);
      fprintf (fd, CP[117],
	       TimeControl.clock[white], TimeControl.moves[white],
	       TimeControl.clock[black], TimeControl.moves[black]);
      for (i = 7; i > -1; i--)
	{
	  fprintf (fd, "%1d ", i + 1);
	  for (c = 0; c < 8; c++)
	    {
	      sq = i * 8 + c;
	      switch (color[sq])
		{
		case black:
		  p = pxx[board[sq]];
		  break;
		case white:
		  p = qxx[board[sq]];
		  break;
		default:
		  p = '.';
		}
	      fprintf (fd, "%c", p);
	    }
	  for (f = i * 8; f < i * 8 + 8; f++)
	    fprintf (fd, " %d", Mvboard[f]);
	  fprintf (fd, "\n");
	}
      fprintf (fd, "  %s\n", cxx);
      fprintf (fd, CP[126]);
      for (i = 1; i <= GameCnt; i++)
	{
	  struct GameRec *g = &GameList[i];

	  f = g->gmove >> 8;
	  t = (g->gmove & 0xFF);
	  algbr (f, t, g->flags);
	  fprintf (fd, "%s %5d %5d %7ld %6ld %5d  %#08lx %#08lx  %c   %s\n",
		   mvstr[0], g->score, g->depth,
		   g->nodes, g->time, g->flags, g->hashkey, g->hashbd,
	   pxx[g->piece], ((g->color == 2) ? "     " : ColorStr[g->color]));
	}
      fclose (fd);
/* Game saved */
      ShowMessage (CP[70]);
    }
  else
    /*ShowMessage ("Could not open file");*/
    ShowMessage (CP[48]);
}

void
ListGame (void)
{
  FILE *fd;
  short i, f, t;
#ifndef MSDOS
  time_t when;
  char fname[256], dbuf[256];
#else
  char fname[256];
#endif

  if (listfile[0])
    strcpy (fname, listfile);
  else
    {
#ifdef MSDOS
      sprintf (fname, "chess.lst");
#else
      time (&when);
      strncpy (dbuf, ctime (&when), 20);
      dbuf[7] = '\0';
      dbuf[10] = '\0';
      dbuf[13] = '\0';
      dbuf[16] = '\0';
      dbuf[19] = '\0';
/* use format "CLp16.Jan01-020304B" when patchlevel is 16,
   date is Jan 1
   time is 02:03:04
   program played black */
      sprintf (fname, "CLp%s.%s%s-%s%s%s%c", patchlevel, dbuf + 4, dbuf + 8, dbuf + 11, dbuf + 14, dbuf + 17, ColorStr[computer][0]);
      /* replace space padding with 0 */
      for (i = 0; fname[i] != '\0'; i++)
	if (fname[i] == ' ')
	  fname[i] = '0';
#endif /* MSDOS */
    }
  fd = fopen (fname, "w");
  if (!fd)
    {
      printf (CP[219], fname);
      exit (1);
    }
  /*fprintf (fd, "gnuchess game %d\n", u);*/
  fprintf (fd, CP[161], patchlevel);
  fprintf (fd, CP[10]);
  fprintf (fd, CP[11]);
  for (i = 1; i <= GameCnt; i++)
    {
      f = GameList[i].gmove >> 8;
      t = (GameList[i].gmove & 0xFF);
      algbr (f, t, GameList[i].flags);
      if(GameList[i].flags & book)
          fprintf (fd, "%5s  %5d    Book%7ld %5d", mvstr[0],
	       GameList[i].score, 
	       GameList[i].nodes, GameList[i].time);
      else
          fprintf (fd, "%5s  %5d     %2d %7ld %5d", mvstr[0],
	       GameList[i].score, GameList[i].depth,
	       GameList[i].nodes, GameList[i].time);
      if ((i % 2) == 0)
	{
	  fprintf (fd, "\n");
#ifdef DEBUG40
	  if (computer == black)
	    fprintf (fd, " %d %d %d %d %d %d %d\n",
		     GameList[i].d1,
		     GameList[i].d2,
		     GameList[i].d3,
		     GameList[i].d4,
		     GameList[i].d5,
		     GameList[i].d6,
		     GameList[i].d7);
	  else
	    fprintf (fd, " %d %d %d %d %d %d %d\n",
		     GameList[i - 1].d1,
		     GameList[i - 1].d2,
		     GameList[i - 1].d3,
		     GameList[i - 1].d4,
		     GameList[i - 1].d5,
		     GameList[i - 1].d6,
		     GameList[i - 1].d7);
#endif
	}
      else
	fprintf (fd, "         ");
    }
  fprintf (fd, "\n\n");
  if (GameList[GameCnt].flags & draw)
    {
      fprintf (fd, CP[54], DRAW);
    }
  else if (GameList[GameCnt].score == -9999)
    {
      fprintf (fd, "%s\n", ColorStr[player ]);
    }
  else if (GameList[GameCnt].score == 9998)
    {
      fprintf (fd, "%s\n", ColorStr[player ^ 1]);
    }
  fclose (fd);
}

void
Undo (void)

/*
 * Undo the most recent half-move.
 */

{
  short f, t;
  f = GameList[GameCnt].gmove >> 8;
  t = GameList[GameCnt].gmove & 0xFF;
  if (board[t] == king && distance (t, f) > 1)
    (void) castle (GameList[GameCnt].color, f, t, 2);
  else
    {
      /* Check for promotion: */
      if (GameList[GameCnt].flags & promote)
	{
	  board[t] = pawn;
	}
      board[f] = board[t];
      color[f] = color[t];
      board[t] = GameList[GameCnt].piece;
      color[t] = GameList[GameCnt].color;
      if (color[t] != neutral)
	Mvboard[t]--;
      Mvboard[f]--;
    }
  if (GameList[GameCnt].flags & epmask)
    EnPassant (otherside[color[f]], f, t, 2);
  else
    InitializeStats ();
epsquare = GameList[GameCnt].epssq;
  if (TCflag && (TCmoves>1))
    ++TimeControl.moves[color[f]];
  hashkey = GameList[GameCnt].hashkey;
  hashbd = GameList[GameCnt].hashbd;
  GameCnt--;
  computer = computer ^ 1;
  opponent = opponent ^ 1;
  flag.mate = false;
  Sdepth = 0;
  player = player ^ 1;
  ShowSidetoMove ();
  UpdateDisplay (0, 0, 1, 0);
  if (flag.regularstart)
    Book = BOOKFAIL;
}

void
 TestSpeed (void (*f) (short int side, short int ply), unsigned j)
{
#ifdef test
  unsigned jj;
#endif
  unsigned i;
  long cnt, rate, t1, t2;
#ifdef HASGETTIMEOFDAY
struct timeval tv;
#endif

#ifdef HASGETTIMEOFDAY
  gettimeofday(&tv,NULL);
  t1 = (tv.tv_sec*100+(tv.tv_usec/10000));
#else
  t1 = time (0);
#endif
  for (i = 0; i < j; i++)
    {
      f (opponent, 2);
#ifdef test
	for(jj=TrPnt[2];i<TrPnt[3];jj++)if(!pick(jj,TrPnt[3]-1))break;
#endif
    }
#ifdef HASGETTIMEOFDAY
  gettimeofday(&tv,NULL);
  t2 = (tv.tv_sec*100+(tv.tv_usec/10000));
#else
  t2 = time (0);
#endif
  cnt = j * (TrPnt[3] - TrPnt[2]);
  if (t2 - t1)
    et = (t2 - t1);
  else
    et = 1;
  rate = (((et) ? ((cnt*100) / et) : 0));
  /*printz ("Nodes= %ld Nodes/sec= %ld\n", cnt, rate);*/
#ifdef NONDSP
  printz (CP[91], cnt, rate);
#ifdef DEBUG9
  for (j = TrPnt[2]; j < TrPnt[3]; j++)
    {
      struct leaf *node = &Tree[j];
      algbr (node->f, node->t, node->flags);
      printf ("%s %s %s %s %d %x\n", mvstr[0], mvstr[1], mvstr[2], mvstr[3],node->score,node->flags);
    }
#endif
#else
  ShowNodeCnt (cnt);
#endif
}

void
 TestPSpeed (short int (*f) (short int side), unsigned j)
{
  short i;
  long cnt, rate, t1, t2;
#ifdef HASGETTIMEOFDAY
struct timeval tv;
#endif

#ifdef HASGETTIMEOFDAY
  gettimeofday(&tv,NULL);
  t1 = (tv.tv_sec*100+(tv.tv_usec/10000));
#else
  t1 = time (0);
#endif
  for (i = 0; i < j; i++)
    {
      (void) f (opponent);
    }
#ifdef HASGETTIMEOFDAY
  gettimeofday(&tv,NULL);
  t2 = (tv.tv_sec*100+(tv.tv_usec/10000));
#else
  t2 = time (0);
#endif
  cnt = j;
  if (t2 - t1)
    et = (t2 - t1);
  else
    et = 1;
  rate = (et) ? ((cnt*100) / et) : 0;
  /*printz ("Nodes= %ld Nodes/sec= %ld\n", cnt, rate);*/
#ifdef NONDSP
  printz (CP[91], cnt, rate);
#else
  ShowNodeCnt (cnt);
#endif
}


void
SetOppTime (char *s)
{
  char *time;
  register tmp = 0;
  int m, t,sec;
  sec = 0;
  time = &s[strlen (CP[197])];
  t = (int)strtol (time, &time, 10);
  if(*time == ':'){time++; sec=(int)strtol(time, &time,10);}
  m = (int)strtol (time, &time, 10);
#ifdef CLIENT
  if (t && !m){
     tmp = TimeControl.clock[opponent];
     TimeControl.clock[opponent] = t*6000+sec*100;
     timeopp[oppptr] += (tmp - TimeControl.clock[opponent]);
printf("otim %d %d %d %d\n", TimeControl.clock[opponent],TimeControl.moves[opponent],tmp - TimeControl.clock[opponent],TTadd);
	} else TimeControl.clock[opponent] = t*6000+sec*100;
#else
  if (t) TimeControl.clock[opponent] = t;
#endif
  if (m)
    TimeControl.moves[opponent] = m;
#if defined XBOARD && !defined CLIENT
  printz (CP[222], m, t);
#endif
}



void
SetMachineTime (char *s)
{
  char *time;
  long tmp = 0;
  int m, t,sec;
  sec = 0;
  time = &s[strlen (CP[197])];
  t = (int)strtol (time, &time, 10);
  if(*time == ':'){time++; sec=(int)strtol(time, &time,10);}
  m = (int)strtol (time, &time, 10);
#ifdef CLIENT
  if (!m){
     tmp = TimeControl.clock[computer];
     TimeControl.clock[computer] = t*6000+sec*100;
     timecomp[compptr] += (tmp - TimeControl.clock[computer]);
	} else TimeControl.clock[computer] = t*6000+sec*100;
printf("time %d %d netlag %d %ld %ld\n", TimeControl.clock[computer],TimeControl.moves[computer],tmp - TimeControl.clock[computer],tmp,TimeControl.clock[computer]);
#else
  if (t)
    TimeControl.clock[computer] = t;
#endif
  if (m)
    TimeControl.moves[computer] = m;
#if defined XBOARD && !defined CLIENT
  printz (CP[222], m, t);
#endif
}


void
InputCommand (void)

/*
 * Process the users command. If easy mode is OFF (the computer is thinking
 * on opponents time) and the program is out of book, then make the 'hint'
 * move on the board and call SelectMove() to find a response. The user
 * terminates the search by entering ^C (quit siqnal) before entering a
 * command. If the opponent does not make the hint move, then set Sdepth to
 * zero.
 */

{
  int eof = 0;
  short have_shown_prompt = false;
  short ok, tmp;
  unsigned short mv;
  char s[80], sx[80];

#if defined CHESSTOOL
  short normal = false;

#endif

  ok = flag.quit = false;
  player = opponent;
  if(TTadd > ttblsize)ZeroTTable();
  if (hint > 0 && !flag.easy && !flag.force)
    if ((board[hint >> 8] != pawn) || ((row (hint & 0x3f) != 0) && (row (hint & 0x3f) != 7)))
      {
	ft = time0;
	fflush (stdout);
	algbr ((short) hint >> 8, (short) hint & 0x3f, false);
	strcpy (s, mvstr[0]);
	tmp = epsquare;
#ifdef DEBUG12
#include "debug12.h"
#endif
#ifdef DEBUG40
whichway = 0;
#endif
#if !defined CHESSTOOL
	if (flag.post) GiveHint ();
#endif
	if (VerifyMove (s, 1, &mv))
	  {
	    Sdepth = 0;
#ifdef QUIETBACKGROUND
#ifdef NONDSP
	    PromptForMove ();
#else
	    ShowSidetoMove ();
	    ShowPrompt ();
#endif
	    have_shown_prompt = true;
#endif /* QUIETBACKGROUND */
#ifdef CLIENT
	InBackground = true;
#endif
	    SelectMove (computer, 2);
#ifdef CLIENT
	InBackground = false;
#endif
	    VerifyMove (s, 2, &mv);
	    Sdepth = 0;
	  }
#ifdef DEBUG40
	else whichway = 1;
#endif
	epsquare = tmp;
	time0 = ft;
      }
  while (!(ok || flag.quit))
    {
#if defined CHESSTOOL
      normal = false;
#endif
      player = opponent;
#ifdef QUIETBACKGROUND
      if (!have_shown_prompt)
	{
#endif /* QUIETBACKGROUND */
#ifdef NONDSP
	  PromptForMove ();
#else
	  ShowSidetoMove ();
	  ShowPrompt ();
#endif
#ifdef QUIETBACKGROUND
	}
      have_shown_prompt = false;
#endif /* QUIETBACKGROUND */
#ifdef NONDSP
      s[0] = sx[0] = '\0';
      while (!sx[0])
	(void) gets (sx);
#else
      fflush (stdout);
#ifdef MSDOS
      s[0] = '\0';
      eof = ( gets (sx) == NULL );
#else
      eof = ( getstr (sx) == ERR );
#endif
#endif
      sscanf (sx, "%s", s);
      if (eof)
	ExitChess ();
      if (s[0] == '\0')
	continue;
      if (strcmp (s, CP[131]) == 0)	/*bd*/
	{
#if defined CHESSTOOL || defined XBOARD
	  chesstool = 0;
#endif /* CHESSTOOL */
	  ClrScreen ();
	  UpdateDisplay (0, 0, 1, 0);
#if defined CHESSTOOL || defined XBOARD
	  chesstool = 1;
#endif /* CHESSTOOL */
	}
      else if (strcmp (s, CP[129]) == 0) /* noop */ ;	/*alg*/
      else if ((strcmp (s, CP[180]) == 0) || (strcmp (s, CP[216]) == 0))	/* quit exit*/
	flag.quit = true;
      else if (strcmp (s, CP[178]) == 0)	/*post*/
	{
	  flag.post = !flag.post;
	}
      else if ((strcmp (s, CP[191]) == 0) || (strcmp (s, CP[154]) == 0))	/*set edit*/
	EditBoard ();
#ifdef NONDSP
      else if (strcmp (s, CP[190]) == 0)	/*setup*/
	SetupBoard ();
#endif
      else if (strcmp (s, CP[156]) == 0)	/*first*/
	{
#if defined CHESSTOOL
	  computer = white;
	  opponent = black;
	  flag.force = false;
	  Sdepth = 0;
#endif /* CHESSTOOL */
	  ok = true;
	}
      else if (strcmp (s, CP[162]) == 0)	/*go*/
	{
	  ok = true;
	  flag.force = false;
	  if (computer == white)
	    {
	      computer = black;
	      opponent = white;
	    }
	  else
	    {
	      computer = white;
	      opponent = black;
	    }
	}
      else if (strcmp (s, CP[166]) == 0)	/*help*/
	help ();
      else if (strcmp (s, CP[221]) == 0)	/*material*/
	flag.material = !flag.material;
      else if (strcmp (s, CP[157]) == 0)	/*force*/
	{flag.force = !flag.force; flag.bothsides = false;}
      else if (strcmp (s, CP[134]) == 0)	/*book*/
	Book = Book ? 0 : BOOKFAIL;
      else if (strcmp (s, CP[172]) == 0)	/*new*/
	{
	  NewGame ();
	  UpdateDisplay (0, 0, 1, 0);
	}
      else if (strcmp (s, CP[171]) == 0)	/*list*/
	ListGame ();
      else if (strcmp (s, CP[169]) == 0 || strcmp (s, CP[217]) == 0)	/*level clock*/
	SelectLevel (sx);
      else if (strcmp (s, CP[165]) == 0)	/*hash*/
	flag.hash = !flag.hash;
      else if (strcmp (s, CP[227]) == 0)	/*gamein*/
	flag.gamein = !flag.gamein;
      else if (strcmp (s, CP[226]) == 0)	/*beep*/
	flag.beep = !flag.beep;
      else if (strcmp (s, CP[197]) == 0)	/*time*/
	{ SetMachineTime (sx); }
      else if (strcmp (s, CP[228]) == 0)	/*time*/
	{ SetOppTime (sx); }
      else if (strcmp (s, CP[33]) == 0)	/*Awindow*/
	ChangeAlphaWindow ();
      else if (strcmp (s, CP[39]) == 0)	/*Bwindow*/
	ChangeBetaWindow ();
      else if (strcmp (s, CP[183]) == 0)	/*rcptr*/
	flag.rcptr = !flag.rcptr;
      else if (strcmp (s, CP[168]) == 0)	/*hint*/
	GiveHint ();
      else if (strcmp (s, CP[135]) == 0)	/*both*/
	{
	  flag.bothsides = !flag.bothsides;
          flag.force = false;
	  Sdepth = 0;
	  ElapsedTime (1);
	  SelectMove (opponent, 1);
	  ok = true;
	}
      else if (strcmp (s, CP[185]) == 0)	/*reverse*/
	{
	  flag.reverse = !flag.reverse;
	  ClrScreen ();
	  UpdateDisplay (0, 0, 1, 0);
	}
      else if (strcmp (s, CP[195]) == 0)	/*switch*/
	{
	  computer = computer ^ 1;
	  opponent = opponent ^ 1;
	  xwndw = (computer == white) ? WXWNDW : BXWNDW;
	  flag.force = false;
	  Sdepth = 0;
	  ok = true;
	}
      else if (strcmp (s, CP[203]) == 0)	/*white*/
	{
	  computer = black;
	  opponent = white;
	  xwndw = WXWNDW;
	  flag.force = false;
	  Sdepth = 0;

	  /*
           * ok = true; don't automatically start with white command
           */
	}
      else if (strcmp (s, CP[133]) == 0)	/*black*/
	{
	  computer = white;
	  opponent = black;
	  xwndw = BXWNDW;
	  flag.force = false;
	  Sdepth = 0;

	  /*
           * ok = true; don't automatically start with black command
           */
	}
      else if (strcmp (s, CP[201]) == 0 && GameCnt > 0)	/*undo*/
	{
	  Undo ();
	}
      else if (strcmp (s, CP[184]) == 0 && GameCnt > 1)	/*remove*/
	{
	  Undo ();
	  Undo ();
	}
      else if (strcmp (s, CP[160]) == 0)	/*get*/
	GetGame ();
      else if (strcmp (s, CP[207]) == 0)	/*xget*/
	GetXGame ();
      else if (strcmp (s, CP[189]) == 0)	/*save*/
	SaveGame ();
      else if (strcmp (s, CP[151]) == 0)	/*depth*/
	ChangeSearchDepth ();
#ifdef DEBUG
      else if (strcmp (s, CP[147]) == 0)	/*debuglevel*/
	ChangeDbLev ();
#endif /* DEBUG */
      else if (strcmp (s, CP[164]) == 0)	/*hashdepth*/
	ChangeHashDepth ();
      else if (strcmp (s, CP[182]) == 0)	/*random*/
	dither = DITHER;
      else if (strcmp (s, CP[229]) == 0)	/*hard*/
	flag.easy = false;
      else if (strcmp (s, CP[152]) == 0)	/*easy*/
	flag.easy = !flag.easy;
      else if (strcmp (s, CP[143]) == 0)	/*contempt*/
	SetContempt ();
      else if (strcmp (s, CP[209]) == 0)	/*xwndw*/
	ChangeXwindow ();
      else if (strcmp (s, CP[186]) == 0)	/*rv*/
	{
	  flag.rv = !flag.rv;
	  UpdateDisplay (0, 0, 1, 0);
	}
      else if (strcmp (s, CP[145]) == 0)	/*coords*/
	{
	  flag.coords = !flag.coords;
	  UpdateDisplay (0, 0, 1, 0);
	}
      else if (strcmp (s, CP[193]) == 0)	/*stars*/
	{
	  flag.stars = !flag.stars;
	  UpdateDisplay (0, 0, 1, 0);
	}
      else if (strcmp (s, CP[196]) == 0)	/*test*/
	{
	  ShowMessage (CP[108]);/*test movelist*/
	  TestSpeed (MoveList, 20000);
	  ShowMessage (CP[107]);/*test capturelist*/
	  TestSpeed (CaptureList, 30000);
	  ShowMessage (CP[85]);/*test score position*/
	  TestPSpeed (ScorePosition, 15000);
	}
      else
      if (strcmp (s, CP[179]) == 0)	/*p*/
	ShowPostnValues ();
      else if (strcmp (s, CP[148]) == 0)	/*debug*/
	DoDebug ();
	else if (strcmp (s, "Mwpawn") == 0)        /*debug*/
        DoTable (Mwpawn);
	else if (strcmp (s, "Mbpawn") == 0)        /*debug*/
        DoTable (Mbpawn);
	else if (strcmp (s, "Mwknight") == 0)        /*debug*/
        DoTable (Mknight[white]);
	else if (strcmp (s, "Mbknight") == 0)        /*debug*/
        DoTable (Mknight[black]);
	else if (strcmp (s, "Mwbishop") == 0)        /*debug*/
        DoTable (Mbishop[white]);
	else if (strcmp (s, "Mbbishop") == 0)        /*debug*/
        DoTable (Mbishop[black]);
      else
	{
#if defined CHESSTOOL
	  normal = (ok = VerifyMove (s, 0, &mv));
#else
	  ok = VerifyMove (s, 0, &mv);
#endif
	  Sdepth = 0;
	}
    }

  ElapsedTime (1);
  if (flag.force)
    {
      computer = opponent;
      opponent = computer ^ 1;
    }
#if defined CHESSTOOL || defined XBOARD
#if defined CHESSTOOL
  if (normal)
    if (computer == white)
      printz ("%d. %s", ++mycnt2, s);
    else
      printz ("%d. ... %s", ++mycnt2, s);
#else
  printz ("%d. %s\n", ++mycnt2, s);
#endif
#ifdef notdef /* optional pass best line to frontend with move */
  if (flag.post)
    {
      register int i;

      printz (" %6d ", MSCORE);
      for (i = 1; MV[i] > 0; i++)
	{
	  algbr ((short) (MV[i] >> 8), (short) (MV[i] & 0xFF), false);
	  printz ("%5s ", mvstr[0]);
	}
    }
  printz ("\n");
#endif
#endif /* CHESSTOOL */
  signal (SIGINT, TerminateSearch);
#ifndef MSDOS
  signal (SIGQUIT, TerminateSearch);
#endif /* MSDOS */
}

#ifdef HASGETTIMEOFDAY
void
ElapsedTime (short int iop)


/*
 * Determine the time that has passed since the search was started. If the
 * elapsed time exceeds the target (ResponseTime+ExtraTime) then set timeout
 * to true which will terminate the search. iop = 0 calculate et bump ETnodes
 * iop = 1 calculate et set timeout if time exceeded, calculate et
 */

{
struct timeval tv;
#ifndef MSDOS
  int nchar;
  extern int errno;
  int i;
#ifdef FIONREAD
#ifdef CLIENT
if(InBackground){
#endif
  if (i = ioctl ((int) 0, FIONREAD, &nchar))
    {
      perror ("FIONREAD");
      fprintf (stderr,
        "You probably have a non-ANSI <ioctl.h>; see README. %d %d %x\n",
	i, errno, FIONREAD);
      exit (1);
    }

  if (nchar)
    {
      if (!flag.timeout)
	flag.back = true;
      flag.bothsides = false;
    }
#endif /*FIONREAD*/
#else
  if (kbhit ())
    {
      if (!flag.timeout)
	flag.back = true;
      flag.bothsides = false;
    }
#endif /* MSDOS */
#ifdef CLIENT
}
#endif
  gettimeofday(&tv,NULL);
  et = (tv.tv_sec*100+(tv.tv_usec/10000)) - time0;
  ETnodes = NodeCnt + ZNODES;
  if (et < 0)
    et = 0;
  if (iop == 1)
    {
      if (et > ResponseTime + ExtraTime && Sdepth > MINDEPTH)
	flag.timeout = true;
      ETnodes = NodeCnt + ZNODES;
      gettimeofday(&tv,NULL);
      time0 = tv.tv_sec*100+tv.tv_usec/10000;
    }
#if !defined NONDSP
#ifdef QUIETBACKGROUND
  if (!background)
#endif /* QUIETBACKGROUND */
    UpdateClocks ();
#endif
}
#else
void
ElapsedTime (short int iop)


/*
 * Determine the time that has passed since the search was started. If the
 * elapsed time exceeds the target (ResponseTime+ExtraTime) then set timeout
 * to true which will terminate the search. iop = 0 calculate et bump ETnodes
 * iop = 1 calculate et set timeout if time exceeded, calculate et
 */

{
#ifndef MSDOS
  int nchar;
  extern int errno;
  int i;
#ifdef FIONREAD
  if (i = ioctl ((int) 0, FIONREAD, &nchar))
    {
      perror ("FIONREAD");
      fprintf (stderr,
        "You probably have a non-ANSI <ioctl.h>; see README. %d %d %x\n",
	i, errno, FIONREAD);
      exit (1);
    }

  if (nchar)
    {
      if (!flag.timeout)
	flag.back = true;
      flag.bothsides = false;
    }
#endif /*FIONREAD*/
#else
  if (kbhit ())
    {
      if (!flag.timeout)
	flag.back = true;
      flag.bothsides = false;
    }
#endif /* MSDOS */
  et = (time ((long *) 0) - time0) * 100;
  ETnodes = NodeCnt + ZNODES;
  if (et < 0)
    et = 0;
  if (iop == 1)
    {
      if (et > ResponseTime + ExtraTime && Sdepth > MINDEPTH)
	flag.timeout = true;
      ETnodes = NodeCnt + ZNODES;
      time0 = time ((long *) 0);
    }
#if !defined NONDSP
#ifdef QUIETBACKGROUND
  if (!background)
#endif /* QUIETBACKGROUND */
    UpdateClocks ();
#endif
}
#endif
void
SetTimeControl (void)
{
  if (TCflag)
    {
      TimeControl.moves[white] = TimeControl.moves[black] = TCmoves;
      TimeControl.clock[white] += 6000L * TCminutes + TCseconds * 100;
      TimeControl.clock[black] += 6000L * TCminutes + TCseconds * 100;
    }
  else
    {
      TimeControl.moves[white] = TimeControl.moves[black] = 0;
      TimeControl.clock[white] = TimeControl.clock[black] = 0;
    }
  flag.onemove = (TCmoves == 1);
  et = 0;
  ElapsedTime (1);
}
