/*
 * nondsp.c - UNIX & MSDOS NON-DISPLAY, AND CHESSTOOL interface for Chess
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
extern int EADD,EGET;
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
void TerminateSearch (int), Die (int);

#endif /* MSDOS */

#include "gnuchess.h"

#ifdef DEBUG
short int debuglevel = 0;

#endif /* DEBUG */
unsigned short int MV[MAXDEPTH];
int MSCORE;

#if defined CHESSTOOL || defined XBOARD
short int chesstool = 1;

#else
short int chesstool = 0;

#endif /* CHESSTOOL */
extern char mvstr[4][6];
int mycnt1, mycnt2;
char *DRAW;
extern char *InPtr;
extern short int pscore[];

void
Initialize (void)
{
  mycnt1 = mycnt2 = 0;
#if defined CHESSTOOL || defined XBOARD
#ifndef SYSV
  setlinebuf (stdout);
#else
  setvbuf (stdout, NULL, _IOLBF, BUFSIZ);
#endif
  printf (CP[43]);		/*Chess*/
  if (!TCflag && (MaxResponseTime == 0))
    MaxResponseTime = 15L*100L;
#endif /* CHESSTOOL */
}

void
ExitChess (void)
{
  signal (SIGTERM, SIG_IGN);
  ListGame ();
  exit (0);
}

#ifndef MSDOS			/* never called!!! */
void
Die (int sig)
{
  char s[80];

  ShowMessage (CP[31]);		/*Abort?*/
  scanz ("%s", s);
  if (strcmp (s, CP[210]) == 0)	/*yes*/
    ExitChess ();
}

#endif /* MSDOS */

void
TerminateSearch (int sig)
{
#ifdef MSDOS
  sig++;			/* shut up the compiler */
#endif /* MSDOS */
  if (!flag.timeout)
    flag.musttimeout = true;
  flag.bothsides = false;
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
  /*printz ("list      game to chess.lst       book      turn %s used %d of %d\n", (Book) ? "off" : "on", bookcount);*/
#ifdef GDBM
  printz (CP[170], (Book) ? CP[92] : CP[93], B.bookcount);
#else
  printz (CP[170], (Book) ? CP[92] : CP[93], bookcount,booksize);
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
  /*printz ("post      principle variation     hint      suggest a move\n");*/
  printz (CP[177]);
  /*printz ("save      game to file            get       game from file\n");*/
  printz (CP[188]);
  /*printz ("random    randomize play          new       start new game\n");*/
  printz (CP[181]);
  printz ("----------------------------------------------------------------\n");
  /*printz ("Computer: %-12s Opponent:            %s\n",*/
  printz (CP[46],
	  ColorStr[computer], ColorStr[opponent]);
  /*printz ("Depth:    %-12d Response time:       %d sec\n",*/
  printz (CP[51],
	  MaxSearchDepth, MaxResponseTime/100);
  /*printz ("Random:   %-12s Easy mode:           %s\n",*/
  printz (CP[99],
	  (dither) ? CP[93] : CP[92], (flag.easy) ? CP[93] : CP[92]);
  /*printz ("Beep:     %-12s Transposition file: %s\n",*/
  printz (CP[36],
	  (flag.beep) ? CP[93] : CP[92], (flag.hash) ? CP[93] : CP[92]);
  /*printz ("Time Control %s %d moves %d seconds %d opr %d depth\n", (TCflag) ? "ON" : "OFF",*/
  printz (CP[110], (TCflag) ? CP[93] : CP[92],
	  TimeControl.moves[white], TimeControl.clock[white] / 100, TCadd/100, MaxSearchDepth);
  signal (SIGINT, TerminateSearch);
#ifndef MSDOS
  signal (SIGQUIT, TerminateSearch);
#endif /* MSDOS */
}

void
EditBoard (void)

/*
 * Set up a board position. Pieces are entered by typing the piece followed
 * by the location. For example, Nf3 will place a knight on square f3.
 */

{
  short a, r, c, sq, i, found;
  char s[80];

  flag.regularstart = true;
  Book = BOOKFAIL;
  ClrScreen ();
  UpdateDisplay (0, 0, 1, 0);
  /*printz (".   exit to main\n");*/
  printz (CP[29]);
  /*printz ("#   clear board\n");*/
  printz (CP[28]);
  /*printz ("c   change sides\n");*/
  printz (CP[136]);
  /*printz ("enter piece & location: \n");*/
  printz (CP[155]);

  a = white;
  do
    {
      scanz ("%s", s);
      found=0;
      if (s[0] == CP[28][0])	/*#*/
	for (sq = 0; sq < 64; sq++)
	  {
	    board[sq] = no_piece;
	    color[sq] = neutral;
	  }
      if (s[0] == CP[136][0])	/*c*/
	a = otherside[a];
      c = s[1] - 'a';
      r = s[2] - '1';
      if ((c >= 0) && (c < 8) && (r >= 0) && (r < 8))
	{
	  sq = locn (r, c);
	  color[sq] = a;
	  board[sq] = no_piece;
	  for (i = no_piece; i <= king; i++)
	    if ((s[0] == pxx[i]) || (s[0] == qxx[i]))
	      {
		board[sq] = i;
		found=1;
		break;
	      }
	  if (found==0) color[sq] = neutral;	
	}
  } while (s[0] != CP[29][0]);
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
SetupBoard (void)

/*
 * Compatibility with Unix chess and the nchesstool. Set up a board position.
 * Eight lines of eight characters are used to setup the board. a8-h8 is the
 * first line. Black pieces are  represented  by  uppercase characters.
 */

{
  short r, c, sq, i;
  char ch;
  char s[80];

  NewGame ();

  gets (s);			/* skip "setup" command */
  for (r = 7; r >= 0; r--)
    {
      gets (s);
      for (c = 0; c <= 7; c++)
	{
	  ch = s[c];
	  sq = locn (r, c);
	  color[sq] = neutral;
	  board[sq] = no_piece;
	  for (i = no_piece; i <= king; i++)
	    if (ch == pxx[i])
	      {
		color[sq] = black;
		board[sq] = i;
		break;
	      }
	    else if (ch == qxx[i])
	      {
		color[sq] = white;
		board[sq] = i;
		break;
	      }
	}
    }
  for (sq = 0; sq < 64; sq++)
    Mvboard[sq] = ((board[sq] != Stboard[sq]) ? 10 : 0);
  InitializeStats ();
  ClrScreen ();
  UpdateDisplay (0, 0, 1, 0);
  /*printz ("Setup successful\n");*/
  printz (CP[106]);
}

void
ShowDepth (char ch)
{
#ifdef MSDOS
  ch++;				/* shut up the compiler */
#endif /* MSDOS */
}


void
ShowLine (short unsigned int *bstline)
{
  register int i;

  for (i = 1; bstline[i] > 0; i++)
    {
      if ((i > 1) && (i % 8 == 1))
	printf ("\n                          ");
      algbr ((short) (bstline[i] >> 8), (short) (bstline[i] & 0xFF), false);
      printf ("%5s ", mvstr[0]);
    }
  printf ("\n");
}

void
ShowResults (short int score, short unsigned int *bstline, char ch)
{
#if !defined CHESSTOOL
  if (flag.post)
    {
      ElapsedTime (2);
      printf ("%2d%c %6d %4ld %8ld  ", Sdepth, ch, score, et / 100, NodeCnt);
      ShowLine (bstline);
    }
#else
  register int i;

  MSCORE = score;
  MV[30] = ch;
  for (i = 1; bstline[i] > 0; i++)
    {
      MV[i] = bstline[i];
    } MV[i] = 0;
#endif /* CHESSTOOL */
}

void
SearchStartStuff (short int side)
{
  signal (SIGINT, TerminateSearch);
#ifndef MSDOS
  signal (SIGQUIT, TerminateSearch);
#endif /* MSDOS */
#if !defined CHESSTOOL
  if (flag.post)
    {
      printf (CP[123],
	       GameCnt/2+1,
	       ResponseTime, TimeControl.clock[side]);
    }
#endif /* CHESSTOOL */
}
void
OutputMove (void)
{
#ifdef DEBUG11
  if (1)
    {
      FILE *D;
      extern unsigned short int PrVar[];
      char d[80];
      int r, c, l, i;
      D = fopen ("/tmp/DEBUGA", "a+");
      fprintf (D, "inout move is %s\n", mvstr[0]);
      strcpy (d, mvstr[0]);
      for (i = 1; PrVar[i] > 0; i++)
	{
	  algbr ((short) (PrVar[i] >> 8), (short) (PrVar[i] & 0xFF), false);
	  fprintf (D, "%5s ", mvstr[0]);
	}
      fprintf (D, "\n");

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
      strcpy (mvstr[0], d);
    }
#endif
if(flag.illegal){printf("%s\n",CP[225]);return;}
if (mvstr[0][0] == '\0') goto nomove;
#if defined CHESSTOOL
  if (computer == black)
    printz ("%d. ... %s\n", ++mycnt1, mvstr[0]);
  else
    printz ("%d. %s\n", ++mycnt1, mvstr[0]);
#else
#ifdef XBOARD
  printz ("%d. ... %s\n", ++mycnt1, mvstr[0]);
#else
  printz ("%d. ... %s\n", ++mycnt1, mvstr[0]);
#endif
#endif /* CHESSTOOL */
#ifdef notdef /* optional pass best line to frontend with move */
  if (flag.post)
    {
      register int i;

      printz (" %6d%c ", MSCORE, MV[30]);
      for (i = 1; MV[i] > 0; i++)
	{
	  algbr ((short) (MV[i] >> 8), (short) (MV[i] & 0xFF), false);
	  printz ("%5s ", mvstr[0]);
	}
    }
  printz ("\n");
#endif
nomove:
  if ((root->flags & draw)||(root->score == -9999)||
      (root->score == 9998)) goto summary;
#if !defined CHESSTOOL && !defined XBOARD
  if (flag.post)
    {
      short h, l, t;

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
      /*printf ("Nodes %ld Tree %d Eval %ld Rate %ld RS high %ld low %ld\n",*/
      printf (CP[89],GenCnt,NodeCnt,t,EvalNodes,(et>100)?(NodeCnt/(et/100)):0,EADD,EGET,reminus,replus);
      /*printf ("Hin/Hout/Coll/Fin/Fout = %ld/%ld/%ld/%ld/%ld\n",*/
      printf (CP[71],
	       HashAdd, HashCnt, THashCol, HashCol,FHashCnt, FHashAdd);
    }
  UpdateDisplay (root->f, root->t, 0, root->flags);
  /*printf ("My move is: %s\n", mvstr[0]);*/
  printf (CP[83], mvstr[0]);
  if (flag.beep)
    printz ("%c", 7);
#endif /* CHESSTOOL */
 summary:
  if (root->flags & draw)
    /*	printf ("Drawn game!\n");*/
    printf (CP[57]);
  else if (root->score == -9999)
    printf("%s mates!\n",ColorStr[opponent]);
  else if (root->score == 9998)
    printf("%s mates!\n",ColorStr[computer]);
#if !defined CHESSTOOL && !defined XBOARD
#ifdef VERYBUGGY
  else if (root->score < -9000)
    printf("%s has a forced mate!\n",ColorStr[opponent]);
  else if (root->score > 9000)
    printf("%s has a forced mate!\n",ColorStr[computer]);
#endif /*VERYBUGGY*/
#endif /* CHESSTOOL */
}

void
ClrScreen (void)
{
#if !defined CHESSTOOL && !defined XBOARD
  printz ("\n");
#endif
}

void
UpdateDisplay (short int f, short int t, short int redraw, short int isspec)
{

  short r, c, l, m;

  if (redraw && !chesstool)
    {
      printz ("\n");
      r = (short)(TimeControl.clock[white] / 6000);
      c = (short)((TimeControl.clock[white] % 6000) / 100);
      l = (short)(TimeControl.clock[black] / 6000);
      m = (short)((TimeControl.clock[black] % 6000) / 100);
      /*printz ("White %d:%02d  Black %d:%02d\n", r, c, l, m);*/
      printz (CP[116], r, c, l, m);
      printz ("\n");
      for (r = 7; r >= 0; r--)
	{
	  for (c = 0; c <= 7; c++)
	    {
	      l = ((flag.reverse) ? locn (7 - r, 7 - c) : locn (r, c));
	      if (color[l] == neutral)
		printz (" -");
	      else if (color[l] == white)
		printz (" %c", qxx[board[l]]);
	      else
		printz (" %c", pxx[board[l]]);
	    }
	  printz ("\n");
	}
      printz ("\n");
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

void
ShowMessage (char *s)
{
  printf("%s\n", s);
}

void
ShowSidetoMove (void)
{
}

void
PromptForMove (void)
{
#if !defined CHESSTOOL && !defined XBOARD
  /*printz ("\nYour move is? ");*/
  printz (CP[124]);
#endif /* CHESSTOOL */
}


void
ShowCurrentMove (short int pnt, short int f, short int t)
{
#ifdef MSDOS
  f++;
  t++;
  pnt++;			/* shut up the compiler */
#endif /* MSDOS */
}

void
ChangeAlphaWindow (void)
{
  printz ("WAwindow: ");
  scanz ("%hd", &WAwindow);
  printz ("BAwindow: ");
  scanz ("%hd", &BAwindow);
}

void
ChangeBetaWindow (void)
{
  printz ("WBwindow: ");
  scanz ("%hd", &WBwindow);
  printz ("BBwindow: ");
  scanz ("%hd", &BBwindow);
}

void
GiveHint (void)
{
  if (hint)
    {
      algbr ((short) (hint >> 8), (short) (hint & 0xFF), false);
      printf(CP[72], mvstr[0]);	/*hint*/
    }
  else
    printz (CP[223]);
}

void
SelectLevel (char *sx)
{

  char T[64], *p, *q;
  p = (char *)strstr(sx,CP[169]) + 5;
  strcat(sx,"XX");
  q = T; *q = '\0';
  for(;*p != 'X';*q++ = *p++);
  *q = '\0';
/* line empty ask for input */
  if(!T[0]){ printz (CP[61]); gets(T); strcat(T,"XX"); }
/* skip whitespace */
  for (p = T; *p == ' '; p++) ;
/* could be moves or a fischer clock */
  if(*p == 'f') { /* its a fischer clock game */
	p++;
	TCminutes = (short)strtol(p,&q,10);
	TCadd = (short)strtol(q,NULL,10) *100;
	TCseconds = 0;
	TCmoves = 50;
  } else { /* regular game */
  TCadd = 0;
  TCmoves = (short)strtol (p, &q, 10);
  TCminutes = (short)strtol (q, &q, 10);
  if (*q == ':')
    TCseconds = (short)strtol (q + 1, (char **) NULL, 10);
  else
    TCseconds = 0;
#ifdef OPERATORTIME
  printz (CP[94]);
  scanz ("%hd", &OperatorTime);
#endif
  if (TCmoves == 0) {
    TCflag = false;
    MaxResponseTime = TCminutes*60L*100L + TCseconds*100L;
    TCminutes = TCseconds = 0;
  } else {
    TCflag = true;
    MaxResponseTime = 0;
  }
}
  TimeControl.clock[white] = TimeControl.clock[black] = 0;
  SetTimeControl ();
}

#ifdef DEBUG
void
ChangeDbLev (void)
{
  printz (CP[146]);
  scanz ("%hd", &debuglevel);
}

#endif /* DEBUG */

void
ChangeSearchDepth (void)
{
  printz ("depth= ");
  scanz ("%hd", &MaxSearchDepth);
  TCflag = !(MaxSearchDepth > 0);
}

void
ChangeHashDepth (void)
{
  printz ("hashdepth= ");
  scanz ("%hd", &HashDepth);
  printz ("MoveLimit= ");
  scanz ("%hd", &HashMoveLimit);
}

void
SetContempt (void)
{
  printz ("contempt= ");
  scanz ("%hd", &contempt);
}

void
ChangeXwindow (void)
{
  printz ("xwndw= ");
  scanz ("%hd", &xwndw);
}

void
ShowPostnValue (short int sq)

/*
 * must have called ExaminePosition() first
 */

{
  short score;
  score = ScorePosition (color[sq]);
  if (color[sq] != neutral){
    printz ("%3d%c ", svalue[sq],(color[sq] == black)?'b':'w');}
  else
    printz(" *   ");
}

void
DoDebug (void)
{
  short c, p, sq, tp, tc, tsq, score,j,k;
  char s[40];

  ExaminePosition ();
  ShowMessage (CP[65]);
  scanz ("%s", s);
  c = neutral;
  if (s[0] == CP[9][0] || s[0] == CP[9][1])     /* w W*/ c = white;
  if (s[0] == CP[9][2] || s[0] == CP[9][3])     /*b B*/ c = black;
  for (p = king; p > no_piece; p--)
    if ((s[1] == pxx[p]) || (s[1] == qxx[p])) break;
  if(p > no_piece)
  for(j=7;j>=0;j--){
  for(k=0;k<8;k++){
      sq=j*8+k;
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
      printz("\n");
    }
  score = ScorePosition (opponent);
  printz (CP[103], score, mtl[computer], pscore[computer], mtl[opponent],pscore[opponent]);
}

void
DoTable (short table[64])
{
  short  sq,j,k;
  ExaminePosition ();
  for(j=7;j>=0;j--){
  for(k=0;k<8;k++){
    sq=j*8+k;
    printz ("%3d ", table[sq]);
  }
printz("\n");
}
}

void
ShowPostnValues (void)
{
  short sq, score,j,k;
  ExaminePosition ();
  for(j=7;j>=0;j--){
  for(k=0;k<8;k++){
  sq=j*8+k;
    ShowPostnValue (sq);
  }
    printz("\n");
  }
  score = ScorePosition (opponent);
 printz (CP[103], score, mtl[computer], pscore[computer], mtl[opponent],pscore[opponent]);
 printz("\nhung white %d hung black %d\n",hung[white],hung[black]);
}

