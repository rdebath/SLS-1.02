/*
 * uxdsp.c - ALPHA interface for CHESS
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
#include <ctype.h>
#include <signal.h>
#ifdef MSDOS
#include <dos.h>
#include <conio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define ESC 0x1B
#define refresh() fflush(stdout)

static void param (short n);

#else
#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
#include <curses.h>

#if defined(__STDC__)
/* <stdlib.h> */
     extern void *malloc (size_t);
     extern void exit (int);

/* <string.h> */
     extern char *strcat (char *, const char *);
     extern int strcmp (const char *, const char *);
     extern char *strcpy (char *, const char *);

/* <time.h> */
     extern long int time (long int *);
#endif

#endif /* MSDOS */

int mycnt1, mycnt2;
#include "gnuchess.h"

extern short int pscore[2];

#define TAB (46)
#define VIR_C(s)  ((flag.reverse) ? 7-column(s) : column(s))
#define VIR_R(s)  ((flag.reverse) ? 7-row(s) : row(s))

unsigned short int MV[MAXDEPTH];
int MSCORE;
char *DRAW;
extern char mvstr[4][6];

void TerminateSearch (int), Die (int);

void
Initialize (void)
{
  signal (SIGINT, Die);
#ifndef MSDOS
  signal (SIGQUIT, Die);
  initscr ();
  crmode ();
#else
  mycnt1 = mycnt2 = 0;
#endif /* MSDOS */
}

void
ExitChess (void)
{
  ListGame ();
  gotoXY (1, 24);
#ifndef MSDOS
  refresh();
  nocrmode ();
  endwin ();
#endif /* MSDOS */
  exit (0);
}

void
Die (int Sig)
{
  char s[80];

  signal (SIGINT, SIG_IGN);
#ifdef MSDOS
  Sig++;			/* shut up the compiler */
#else
  signal (SIGQUIT, SIG_IGN);
#endif /* MSDOS */
  ShowMessage (CP[31]);		/*Abort?*/
  scanz ("%s", s);
  if (strcmp (s, CP[210]) == 0)	/*yes*/
    ExitChess ();
  signal (SIGINT, Die);
#ifndef MSDOS
  signal (SIGQUIT, Die);
#endif /* MSDOS */
}

void
TerminateSearch (int Sig)
{
  signal (SIGINT, SIG_IGN);
#ifdef MSDOS
  Sig++;			/* shut up the compiler */
#else
  signal (SIGQUIT, SIG_IGN);
#endif /* MSDOS */
  if (!flag.timeout)
    flag.musttimeout = true;
  flag.bothsides = false;
  signal (SIGINT, Die);
#ifndef MSDOS
  signal (SIGQUIT, Die);
#endif /* MSDOS */
}
void
ShowLine (short unsigned int *bstline)
{
}

void
help (void)
{
  ClrScreen ();
  /*printz ("CHESS command summary\n");*/
  printz (CP[40]);
  printz ("----------------------------------------------------------------\n");
  /*printz ("g1f3      move from g1 to f3      quit      Exit Chess\n");*/
  printz (CP[158]);
  /*printz ("Nf3       move knight to f3       beep      turn %s\n", (flag.beep) ? "off" : "on");*/
  printz (CP[86], (flag.beep) ? CP[92] : CP[93]);
  /*printz ("a7a8q     promote pawn to queen\n");*/
  printz (CP[128], (flag.material) ? CP[92] : CP[93]);
  /*printz ("o-o       castle king side        easy      turn %s\n", (flag.easy) ? "off" : "on");*/
  printz (CP[173], (flag.easy) ? CP[92] : CP[93]);
  /*printz ("o-o-o     castle queen side       hash      turn %s\n", (flag.hash) ? "off" : "on");*/
  printz (CP[174], (flag.hash) ? CP[92] : CP[93]);
  /*printz ("bd        redraw board            reverse   board display\n");*/
  printz (CP[130]);
  /*printz ("list      game to chess.lst       book      turn %s used %d of %d\n", (Book) ? "off" : "on", book
count, booksize);*/
#ifdef GDBM
  printz (CP[170], (Book) ? CP[92] : CP[93], B.bookcount, 0);
#else
  printz (CP[170], (Book) ? CP[92] : CP[93], bookcount, BOOKSIZE);
#endif
  /*printz ("undo      undo last ply           remove    take back a move\n");*/
  printz (CP[200]);
  /*printz ("edit      edit board              force     enter game moves\n");*/
  printz (CP[153]);
  /*printz ("switch    sides with computer     both      computer match\n");*/
  printz (CP[194]);
  /*printz ("white     computer plays white    black     computer plays black\n");*/
  printz (CP[202]);
  /*printz ("depth     set search depth        clock     set time control\n");*/
  printz (CP[149]);
  /*printz ("hint      suggest a move         post      turn %s principle variation\n", (flag.post) ? "off" :
"on");*/
  printz (CP[177], (flag.post) ? CP[92] : CP[93]);
  /*printz ("save      game to file            get       game from file\n");*/
  printz (CP[188]);
  /*printz ("random    randomize play          new       start new game\n");*/
  printz (CP[181]);
  gotoXY (10, 20);
  printz (CP[47], ColorStr[computer]);
  gotoXY (10, 21);
  printz (CP[97], ColorStr[opponent]);
  gotoXY (10, 22);
  printz (CP[79], MaxResponseTime/100);
  gotoXY (10, 23);
  printz (CP[59], (flag.easy) ? CP[93] : CP[92]);
  gotoXY (40, 20);
  printz (CP[52], MaxSearchDepth);
  gotoXY (40, 21);
  printz (CP[100], (dither) ? CP[93] : CP[92]);
  gotoXY (40, 22);
  printz (CP[112], (flag.hash) ? CP[93] : CP[92]);
  gotoXY (40, 23);
  printz (CP[73]);
  gotoXY (10, 24);
  printz (CP[110], (TCflag) ? CP[93] : CP[92],
	  TimeControl.moves[white], TimeControl.clock[white] / 100, OperatorTime, MaxSearchDepth);
  refresh ();
#ifdef BOGUS
  fflush (stdin); /*what is this supposed to do??*/
#endif /*BOGUS*/
  getchar ();
  ClrScreen ();
  UpdateDisplay (0, 0, 1, 0);
}


void
EditBoard (void)

/*
 * Set up a board position. Pieces are entered by typing the piece followed
 * by the location. For example, Nf3 will place a knight on square f3.
 */

{
  short a, r, c, sq, i;
  char s[80];

  flag.regularstart = true;
  Book = BOOKFAIL;
  ClrScreen ();
  UpdateDisplay (0, 0, 1, 0);
  gotoXY (TAB, 3);
  printz (CP[29]);
  gotoXY (TAB, 4);
  printz (CP[28]);
  gotoXY (TAB, 5);
  printz (CP[136]);
  gotoXY (TAB, 7);
  printz (CP[64]);
  a = white;
  do
    {
      gotoXY (TAB, 6);
      printz (CP[60], ColorStr[a]);	/*Editing %s*/
      gotoXY (TAB + 24, 7);
      ClrEoln ();
      scanz ("%s", s);
      if (s[0] == CP[28][0])	/*#*/
	{
	  for (sq = 0; sq < 64; sq++)
	    {
	      board[sq] = no_piece;
	      color[sq] = neutral;
	      DrawPiece (sq);
	    }
	}
      if (s[0] == CP[136][0])	/*c*/
	a = otherside[a];
      c = s[1] - 'a';
      r = s[2] - '1';
      if ((c >= 0) && (c < 8) && (r >= 0) && (r < 8))
	{
	  sq = locn (r, c);
	  for (i = king; i > no_piece; i--)
	    if ((s[0] == pxx[i]) || (s[0] == qxx[i]))
	      break;
	  board[sq] = i;
	  color[sq] = ((board[sq] == no_piece) ? neutral : a);
	  DrawPiece (sq);
	}
  } while (s[0] != CP[29][0]);	/*.*/

  for (sq = 0; sq < 64; sq++)
    Mvboard[sq] = ((board[sq] != Stboard[sq]) ? 10 : 0);
  GameCnt = 0;
  Game50 = 1;
  ZeroRPT ();
  Sdepth = 0;
  InitializeStats ();
  ClrScreen ();
  UpdateDisplay (0, 0, 1, 0);
}

void
ShowPlayers (void)
{
  gotoXY (5, ((flag.reverse) ? 23 : 2));
  printz ("%s", (computer == black) ? CP[218] : CP[74]);
  gotoXY (5, ((flag.reverse) ? 2 : 23));
  printz ("%s", (computer == white) ? CP[218] : CP[74]);
}

void
ShowDepth (char ch)
{
  gotoXY (TAB, 4);
  printz (CP[53], Sdepth, ch);	/*Depth= %d%c*/
  ClrEoln ();
}

void
ShowScore (short score)
{
  gotoXY (TAB, 5);
  printz (CP[104], score);
  ClrEoln ();
}

void
ShowMessage (char *s)
{
  gotoXY (TAB, 6);
  printz ("%s", s);
  ClrEoln ();
}

void
ClearMessage (void)
{
  gotoXY (TAB, 6);
  ClrEoln ();
}

void
ShowCurrentMove (short int pnt, short int f, short int t)
{
  algbr (f, t, false);
  gotoXY (TAB, 7);
  printz ("(%2d) %4s", pnt, mvstr[0]);
}

void
ShowHeader (void)
{
  gotoXY (TAB, 2);
  printz (CP[69]);
}

void
ShowSidetoMove (void)
{
  gotoXY (TAB, 14);
  printz ("%2d:   %s", 1 + GameCnt / 2, ColorStr[player]);
  ClrEoln ();
}

void
ShowPrompt (void)
{
  gotoXY (TAB, 19);
  printz (CP[121]);		/*Your movwe is?*/
  ClrEoln ();
}

void
ShowNodeCnt (long int NodeCnt)
{
  gotoXY (TAB, 21);
  printz (CP[90], NodeCnt, (et > 100) ? NodeCnt / (et / 100) : 0);
  ClrEoln ();
}

void
ShowResults (short int score, short unsigned int *bstline, char ch)
{
  unsigned char d, ply;

  if (flag.post)
    {
      ShowDepth (ch);
      ShowScore (score);
      d = 7;
      for (ply = 1; bstline[ply] > 0; ply++)
	{
	  if (ply % 4 == 1)
	    {
	      gotoXY (TAB, ++d);
	      ClrEoln ();
	    }
	  algbr ((short) bstline[ply] >> 8, (short) bstline[ply] & 0xFF, false);
	  printz ("%5s ", mvstr[0]);
	}
      ClrEoln ();
      while (d < 13)
	{
	  gotoXY (TAB, ++d);
	  ClrEoln ();
	}
    }
}

void
SearchStartStuff (short int side)
{
  short i;

  signal (SIGINT, TerminateSearch);
#ifdef MSDOS
  side++;			/* shut up the compiler */
#else
  signal (SIGQUIT, TerminateSearch);
#endif /* MSDOS */
  for (i = 4; i < 14; i++)
    {
      gotoXY (TAB, i);
      ClrEoln ();
    }
}

void
OutputMove (void)
{

  UpdateDisplay (root->f, root->t, 0, (short) root->flags);
  gotoXY (TAB, 17);
  if(flag.illegal){printz(CP[225]);return;}
  printz (CP[84], mvstr[0]);	/*My move is %s*/
  if (flag.beep)
    putchar (7);
  ClrEoln ();

  gotoXY (TAB, 24);
  if (root->flags & draw)
    printz (CP[58]);
  else if (root->score == -9999)
    printz (CP[95]);
  else if (root->score == 9998)
    printz (CP[44]);
#ifdef VERYBUGGY
  else if (root->score < -9000)
    printz (CP[96]);
  else if (root->score > 9000)
    printz (CP[45]);
#endif /*VERYBUGGY*/
  ClrEoln ();
  if (flag.post)
    {
      register short h, l, t;

      h = TREE;
      l = 0;
      t = TREE >> 1;
      while (l != t)
	{
	  if (Tree[t].f || Tree[t].t)
	    l = t;
	  else
	    h = t;
	  t = (l + h) >> 1;
	}

      ShowNodeCnt (NodeCnt);
      gotoXY (TAB, 22);
      printz (CP[81], t);	/*Max Tree=*/
      ClrEoln ();
    }
  ShowSidetoMove ();
}

void
UpdateClocks (void)
{
  short m, s;

  m = (short) (et / 6000);
  s = (short) (et - 6000 * (long) m) / 100;
  if (TCflag)
    {
      m = (short) ((TimeControl.clock[player] - et) / 6000);
      s = (short) ((TimeControl.clock[player] - et - 6000 * (long) m) / 100);
    }
  if (m < 0)
    m = 0;
  if (s < 0)
    s = 0;
  if (player == white)
    gotoXY (20, (flag.reverse) ? 2 : 23);
  else
    gotoXY (20, (flag.reverse) ? 23 : 2);
  printz ("%d:%02d   ", m, s);
  if (flag.post)
    ShowNodeCnt (NodeCnt);
  refresh ();
}

void
gotoXY (short int x, short int y)
{
#ifdef MSDOS
  putchar (ESC);
  putchar ('[');
  param (y);
  putchar (';');
  param (x);
  putchar ('H');
#else
  move (y - 1, x - 1);
#endif /* MSDOS */
}

void
ClrScreen (void)
{
#ifdef MSDOS
  putchar (ESC);
  putchar ('[');
  putchar ('2');
  putchar ('J');
#else
  clear ();
#endif /* MSDOS */
  refresh ();
}

void
ClrEoln (void)
{
#ifdef MSDOS
  putchar (ESC);
  putchar ('[');
  putchar ('K');
#else
  clrtoeol ();
#endif /* MSDOS */
  refresh ();
}

#ifdef MSDOS
void
param (short n)
{
  if (n >= 10)
    {
      register short d, q;

      q = n / 10;
      d = n % 10;
      putchar (q + '0');
      putchar (d + '0');
    }
  else
    putchar (n + '0');
}

#endif /* MSDOS */

void
DrawPiece (short int sq)
{
  register char x;

  if (color[sq] == black)
#if defined(MSDOS) && !defined(SEVENBIT)
    x = '7';			/* print WHITE boldface, */
  else
    x = '1';			/* print BLACK inverted	 */
  gotoXY (6 + 5 * VIR_C (sq), 5 + 2 * (7 - VIR_R (sq)));
  printz ("\033[%cm%c\033[0m", x, pxx[board[sq]]);
#else
    x = '*';
  else
    x = ' ';
  gotoXY (5 + 5 * VIR_C (sq), 5 + 2 * (7 - VIR_R (sq)));
  printz ("%c%c ", x, pxx[board[sq]]);
#endif /* MSDOS && !SEVENBIT */
}

void
ShowPostnValue (short int sq)

/*
 * must have called ExaminePosition() first
 */

{
  short score;

  gotoXY (4 + 5 * VIR_C (sq), 5 + 2 * (7 - VIR_R (sq)));
  score = ScorePosition (color[sq]);
  if (color[sq] != neutral)
    printz ("%3d ", svalue[sq]);
  else
    printz ("   ");
}

void
ShowPostnValues (void)
{
  short sq, score;

  ExaminePosition ();
  for (sq = 0; sq < 64; sq++)
    ShowPostnValue (sq);
  score = ScorePosition (opponent);
  gotoXY (TAB, 5);
  printz (CP[103], score, mtl[computer], pscore[computer], mtl[opponent],pscore[opponent]);

  ClrEoln ();
}

void
UpdateDisplay (short int f, short int t, short int redraw, short int isspec)
{
  short i, sq, z;

  if (redraw)
    {
      ShowHeader ();
      ShowPlayers ();

      i = 3;
      gotoXY (3, ++i);
#if defined(MSDOS) && !defined(SEVENBIT)
      printz ("\332\304\304\304\304\302\304\304\304\304\302\304\304\304\304" \
	    "\302\304\304\304\304\302\304\304\304\304\302\304\304\304\304" \
	      "\302\304\304\304\304\302\304\304\304\304\277");
#else
      printz ("+----+----+----+----+----+----+----+----+");
#endif /* MSDOS && !SEVENBIT */
      while (i < 19)
	{
	  gotoXY (1, ++i);
	  if (flag.reverse)
	    z = (i / 2) - 1;
	  else
	    z = 10 - (i / 2);
#if defined(MSDOS) && !defined(SEVENBIT)
	  printz ("%d \263    \263    \263    \263    \263    \263    " \
		  "\263    \263    \263", z);
#else
	  printz ("%d |    |    |    |    |    |    |    |    |", z);
#endif /* MSDOS && !SEVENBIT */
	  gotoXY (3, ++i);
	  if (i < 19)
#if defined(MSDOS) && !defined(SEVENBIT)
	    printz ("\303\304\304\304\304\305\304\304\304\304\305\304\304" \
		    "\304\304\305\304\304\304\304\305\304\304\304\304\305" \
		    "\304\304\304\304\305\304\304\304\304\305\304\304\304" \
		    "\304\264");
#else
	    printz ("+----+----+----+----+----+----+----+----+");
#endif /* MSDOS && !SEVENBIT */
	}
#if defined(MSDOS) && !defined(SEVENBIT)
      printz ("\300\304\304\304\304\301\304\304\304\304\301\304\304\304\304" \
	    "\301\304\304\304\304\301\304\304\304\304\301\304\304\304\304" \
	      "\301\304\304\304\304\301\304\304\304\304\331");
#else
      printz ("+----+----+----+----+----+----+----+----+");
#endif /* MSDOS && !SEVENBIT */
      gotoXY (3, 21);
      if (flag.reverse)
	printz (CP[16]);
      else
	printz (CP[15]);
      for (sq = 0; sq < 64; sq++)
	DrawPiece (sq);
    }
  else
    {
      DrawPiece (f);
      DrawPiece (t);
      if (isspec & cstlmask)
	if (t > f)
	  {
	    DrawPiece (f + 3);
	    DrawPiece (t - 1);
	  }
	else
	  {
	    DrawPiece (f - 4);
	    DrawPiece (t + 1);
	  }
      else if (isspec & epmask)
	{
	  DrawPiece (t - 8);
	  DrawPiece (t + 8);
	}
    }
  refresh ();
}

extern char *InPtr;
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

void
ChangeAlphaWindow (void)
{
  ShowMessage (CP[114]);
  scanz ("%hd", &WAwindow);
  ShowMessage (CP[34]);
  scanz ("%hd", &BAwindow);
}

void
ChangeBetaWindow (void)
{
  ShowMessage (CP[115]);
  scanz ("%hd", &WBwindow);
  ShowMessage (CP[35]);
  scanz ("%hd", &BBwindow);
}

void
GiveHint (void)
{
  char s[40];
  if (hint)
    {
      algbr ((short) (hint >> 8), (short) (hint & 0xFF), false);
      strcpy (s, CP[198]);	/*try*/
      strcat (s, mvstr[0]);
      ShowMessage (s);
    }
  else
    ShowMessage (CP[223]);
}

void
ChangeHashDepth (void)
{
  ShowMessage (CP[163]);
  scanz ("%hd", &HashDepth);
  ShowMessage (CP[82]);
  scanz ("%hd", &HashMoveLimit);
}

void
ChangeSearchDepth (void)
{
  ShowMessage (CP[150]);
  scanz ("%hd", &MaxSearchDepth);
  TCflag = !(MaxSearchDepth > 0);
}

void
SetContempt (void)
{
  ShowMessage (CP[142]);
  scanz ("%hd", &contempt);
}

void
ChangeXwindow (void)
{
  ShowMessage (CP[208]);
  scanz ("%hd", &xwndw);
}

void
SelectLevel (char *sx)
{
  int item;

  ClrScreen ();
  gotoXY (32, 2);
  printz (CP[41]);
  gotoXY (20, 4);
  printz (CP[18]);
  gotoXY (20, 5);
  printz (CP[19]);
  gotoXY (20, 6);
  printz (CP[20]);
  gotoXY (20, 7);
  printz (CP[21]);
  gotoXY (20, 8);
  printz (CP[22]);
  gotoXY (20, 9);
  printz (CP[23]);
  gotoXY (20, 10);
  printz (CP[24]);
  gotoXY (20, 11);
  printz (CP[25]);
  gotoXY (20, 12);
  printz (CP[26]);
  gotoXY (20, 13);
  printz (CP[27]);

  OperatorTime = 0;
  TCmoves = 60;
  TCminutes = 5;
  TCseconds = 0;

  gotoXY (20, 17);
  printz (CP[62]);
  refresh ();
  scanz ("%d", &item);
  switch (item)
    {
    case 1:
      TCmoves = 60;
      TCminutes = 5;
      break;
    case 2:
      TCmoves = 60;
      TCminutes = 15;
      break;
    case 3:
      TCmoves = 60;
      TCminutes = 30;
      break;
    case 4:
      TCmoves = 40;
      TCminutes = 30;
      break;
    case 5:
      TCmoves = 40;
      TCminutes = 60;
      break;
    case 6:
      TCmoves = 40;
      TCminutes = 120;
      break;
    case 7:
      TCmoves = 40;
      TCminutes = 240;
      break;
    case 8:
      TCmoves = 1;
      TCminutes = 15;
      break;
    case 9:
      TCmoves = 1;
      TCminutes = 60;
      break;
    case 10:
      TCmoves = 1;
      TCminutes = 600;
      break;
    }

  TCflag = (TCmoves > 0);

  TimeControl.clock[white] = TimeControl.clock[black] = 0; 

  SetTimeControl ();
  ClrScreen ();
  UpdateDisplay (0, 0, 1, 0);
}

void
DoDebug (void)
{
  short c, p, sq, tp, tc, tsq, score;
  char s[40];

  ExaminePosition ();
  ShowMessage (CP[65]);
  scanz ("%s", s);
  c = neutral;
  if (s[0] == CP[9][0] || s[0] == CP[9][1])	/* w W*/
    c = white;
  if (s[0] == CP[9][2] || s[0] == CP[9][3])	/*b B*/
    c = black;
  for (p = king; p > no_piece; p--)
    if ((s[1] == pxx[p]) || (s[1] == qxx[p]))
      break;
  for (sq = 0; sq < 64; sq++)
    {
      tp = board[sq];
      tc = color[sq];
      board[sq] = p;
      color[sq] = c;
      tsq = PieceList[c][1];
      PieceList[c][1] = sq;
      ShowPostnValue (sq);
      PieceList[c][1] = tsq;
      board[sq] = tp;
      color[sq] = tc;
    }
  score = ScorePosition (opponent);
  gotoXY (TAB, 5);
  printz (CP[103], score, mtl[computer], pscore[computer], mtl[opponent],pscore[opponent]);

  ClrEoln ();
}
void
DoTable (short table[64])
{
  short  sq;
  ExaminePosition ();
  for (sq=0;sq<64;sq++){
  gotoXY (4 + 5 * VIR_C (sq), 5 + 2 * (7 - VIR_R (sq)));
  printz ("%3d ", table[sq]);

}
}

