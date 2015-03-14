/*
 * main.c - C source for GNU CHESS
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

#include "version.h"
#include "gnuchess.h"
#include <signal.h>
char *ColorStr[2];
char *CP[CPSIZE];

/*
 * In a networked enviroment gnuchess might be compiled on different hosts
 * with different random number generators, that is not acceptable if they
 * are going to share the same transposition table.
 */
unsigned long int next = 1;

unsigned int
urand (void)
{
  next *= 1103515245;
  next += 12345;
  return ((unsigned int) (next >> 16) & 0xFFFF);
}

void
gsrand (unsigned int seed)
{
  next = seed;
}

unsigned long hashkey, hashbd;
struct hashval hashcode[2][7][64];

#ifdef ttblsz
struct hashentry huge *ttable[2];
unsigned int ttblsize;
#endif
#ifdef BINBOOK
extern char *binbookfile;
#endif
extern char *bookfile;

char savefile[128] = "";
char listfile[128] = "";
#ifdef HISTORY
unsigned short history[32768];
#endif
short rpthash[2][256];
struct leaf Tree[TREE], *root;
short TrPnt[MAXDEPTH];
short PieceList[2][64], PawnCnt[2][8];
short castld[2], Mvboard[64];
short svalue[64];
struct flags flag;
short opponent, computer, WAwindow, WBwindow, BAwindow, BBwindow, dither,
  INCscore;
long ResponseTime, ExtraTime, MaxResponseTime, et, et0, time0, ft;
long GenCnt, NodeCnt, ETnodes, EvalNodes, HashCnt, HashAdd, FHashCnt, FHashAdd,
  HashCol, THashCol, filesz, hashmask, hashbase;
long replus, reminus;
short HashDepth = HASHDEPTH, HashMoveLimit = HASHMOVELIMIT;
short player, xwndw;
/*unsigned*/ short rehash; /* -1 is used as a flag --tpm */
struct GameRec GameList[MAXMOVES + MAXDEPTH];
short Sdepth, Game50, MaxSearchDepth;
short GameCnt = 0;
short epsquare, contempt;
int Book;
struct TimeControlRec TimeControl;
int TCadd = 0;
short TCflag, TCmoves, TCminutes, TCseconds, OperatorTime;
short XCmoves[3], XCminutes[3], XCseconds[3], XC, XCmore;
const short otherside[3] =
{black, white, neutral};
unsigned short hint;
short int TOflag;		/* force search re-init if we backup search */

short mtl[2], pmtl[2], hung[2];
short Pindex[64];
short PieceCnt[2];
short FROMsquare, TOsquare;
short HasKnight[2], HasBishop[2], HasRook[2], HasQueen[2];
short ChkFlag[MAXDEPTH], CptrFlag[MAXDEPTH], PawnThreat[MAXDEPTH];
short Pscore[MAXDEPTH], Tscore[MAXDEPTH];
const short qrook[3] =
{0, 56, 0};
const short krook[3] =
{7, 63, 0};
const short kingP[3] =
{4, 60, 0};
const short rank7[3] =
{6, 1, 0};
const short sweep[8] =
{false, false, false, true, true, true, false, false};
unsigned short killr0[MAXDEPTH], killr1[MAXDEPTH];
unsigned short killr2[MAXDEPTH], killr3[MAXDEPTH];
unsigned short PV, SwagHt, Swag0, Swag1, Swag2, Swag3, Swag4, sidebit;
#ifdef KILLT
short killt[0x4000];
#endif
const short value[7] =
{0, valueP, valueN, valueB, valueR, valueQ, valueK};
const short control[7] =
{0, ctlP, ctlN, ctlB, ctlR, ctlQ, ctlK};
short stage, stage2, Developed[2];
FILE *hashfile;
unsigned int starttime;
short int ahead = true, hash = true;

#if defined CHESSTOOL || defined XBOARD
void
TerminateChess (int sig)
{
  ExitChess ();
}

#endif
int timeopp[MINGAMEIN], timecomp[MINGAMEIN];
int compptr, oppptr;
inline void
TimeCalc ()
{
/* adjust number of moves remaining in gamein games */
  int increment = 0;
  int topsum = 0;
  int tcompsum = 0;
  int me,him;
  int i;
/* dont do anything til you have enough numbers */
  if (GameCnt < (MINGAMEIN * 2)) return;
/* calculate average time in sec for last MINGAMEIN moves */
  for (i = 0; i < MINGAMEIN; i++)
    {
      tcompsum += timecomp[i];
      topsum += timeopp[i];
    }
  topsum /= (100 * MINGAMEIN);
  tcompsum /= (100 * MINGAMEIN);
/* if I have less time than opponent add another move */
	me = TimeControl.clock[computer]/100; 
	him = TimeControl.clock[opponent]/100;
	if(me < him) increment += 2;
/* if I am losing more time with each move add another */
  /*if ( !((me - him) > 60) && tcompsum > topsum) increment++;*/
  if ( tcompsum > topsum) increment +=2;
/* but dont let moves go below MINMOVES */
  else if (TimeControl.moves[computer] < MINMOVES && !increment) increment++;
/* if I am doing really well use more time per move */
  else if (me > him && tcompsum < topsum) increment = -1;
  TimeControl.moves[computer] += increment;
}

/* hmm.... shouldn`t main be moved to the interface routines */
int
main (int argc, char **argv)
{
  char *xwin = 0;
  char *Lang = NULL;
  gsrand (starttime = ((unsigned int) time ((long *) 0)));	/* init urand */
#ifdef ttblsz
  ttblsize = ttblsz;
  rehash = -1;
#endif /* ttblsz */
  if (argc > 2)
    {
      if (argv[1][0] == '-' && argv[1][1] == 'L')
	{
	  Lang = argv[2];
	  argv += 2;
	  argc -= 2;
	}
    }
  InitConst (Lang);
  ColorStr[0] = CP[118];
  ColorStr[1] = CP[119];

  while (argc > 1 && ((argv[1][0] == '-') || (argv[1][0] == '+')))
    {
      switch (argv[1][1])
	{
	case 'a':
	  ahead = ((argv[1][0] == '-') ? false : true);
	  break;
	case 'b':
	  argv++;
	  argc--;
	  if (argc > 1)
	    {
	      bookfile = argv[1];
#ifdef BINBOOK
	      binbookfile = NULL;
#endif
	    }
	  break;
#ifdef BINBOOK
	case 'B':
	  argv++;
	  argc--;
	  if (argc > 1)
	    binbookfile = argv[1];
	  break;
#endif
	case 'h':
	  hash = ((argv[1][0] == '-') ? false : true);
	  break;
	case 's':
	  argc--;
	  argv++;
	  if (argc > 1)
	    strcpy (savefile, argv[1]);
	  break;
	case 'l':
	  argc--;
	  argv++;
	  if (argc > 1)
	    strcpy (listfile, argv[1]);
	  break;
#ifndef GDBM
	case 'S':
	  argc--;
	  argv++;
	  if(argc > 1)booksize = atoi(argv[1]);
	  break;
#endif
	case 'P':
	  argc--;
	  argv++;
	  if(argc > 1)bookmaxply = atoi(argv[1]);
	  break;

#if ttblsz
	case 'r':
	  if (argc > 2)
	    rehash = atoi (argv[2]);
	  argc--;
	  argv++;
	  if (rehash > MAXrehash)
	    rehash = MAXrehash;
	  break;
	case 'T':
	  if (argc > 2)
	    ttblsize = atoi (argv[2]);
	  argc--;
	  argv++;
	  if (ttblsize > 0 && ttblsize < 24)
	    ttblsize = (1 << ttblsize);
	  else
	    ttblsize = ttblsz;
	  break;
#ifdef HASHFILE
	case 't':		/* create or test persistent transposition
				 * table */
	  hashfile = fopen (HASHFILE, RWA_ACC);
	  if (hashfile)
	    {
	      fseek (hashfile, 0L, SEEK_END);
	      filesz = (ftell (hashfile) / sizeof (struct fileentry)) - 1;
	    }
	  if (hashfile != NULL)
	    {
	      long i, j;
	      int nr[MAXDEPTH];
	      struct fileentry n;

	      printf (CP[49]);
	      for (i = 0; i < MAXDEPTH; i++)
		nr[i] = 0;
	      fseek (hashfile, 0L, SEEK_END);
	      i = ftell (hashfile) / sizeof (struct fileentry);
	      fseek (hashfile, 0L, SEEK_SET);
	      for (j = 0; j < i + 1; j++)
		{
		  fread (&n, sizeof (struct fileentry), 1, hashfile);
if(n.depth >MAXDEPTH) {printf("ERROR\n");exit(1);}
		  if (n.depth)
		    {
		      nr[n.depth]++;
		      nr[0]++;
		    }
		}
	      printf (CP[109],
		      nr[0], i);
	      for (j = 1; j < MAXDEPTH; j++)
		printf ("%d ", nr[j]);
	      printf ("\n");
	    }
	  return 0;
	case 'c':		/* create or test persistent transposition
				 * table */
	  if (argc > 2)
	    filesz = atoi (argv[2]);
	  if (filesz > 0 && filesz < 24)
	    filesz = (1 << filesz) - 1 + MAXrehash;
	  else
	    filesz = Deffilesz + MAXrehash;
	  if ((hashfile = fopen (HASHFILE, RWA_ACC)) == NULL)
	    hashfile = fopen (HASHFILE, WA_ACC);
	  if (hashfile != NULL)
	    {
	      long j;
	      struct fileentry n;

	      printf (CP[66]);
	      n.f = n.t = 0;
	      n.flags = 0;
	      n.depth = 0;
	      n.sh = n.sl = 0;
	      for (j = 0; j < filesz + 1; j++)
		fwrite (&n, sizeof (struct fileentry), 1, hashfile);
	      fclose (hashfile);
	    }
	  else
	    printf (CP[50], HASHFILE);
	  return (0);
#endif /* HASHFILE */
#endif /* ttblsz */
	case 'x':
	  xwin = &argv[1][2];
	  break;
	case 'v':
	  fprintf (stderr, CP[102], version, patchlevel);
	  exit (1);
	default:
	  fprintf (stderr, CP[113]);
	  exit (1);
	}
      argv++;
      argc--;
    }
  XC = 0;
  MaxResponseTime = 0;
#if defined CHESSTOOL || defined XBOARD
  signal (SIGTERM, TerminateChess);
  TCflag = true;
  TCmoves = 40;
  TCminutes = 5;
  TCseconds = 0;
  TCadd = 0;
  OperatorTime = 0;
#else
  TCflag = false;
  OperatorTime = 0;
#endif
  if (argc == 2)
    {
      char *p;

      MaxResponseTime = 100L * strtol (argv[1], &p, 10);
      if (*p == ':')
	MaxResponseTime = 60L * MaxResponseTime +
	  100L * strtol (++p, (char **) NULL, 10);
      TCflag = false;
      TCmoves = 0;
      TCminutes = 0;
      TCseconds = 0;
    }
  if (argc >= 3)
    {
      char *p;
      if (argc > 9)
	{
	  printf ("%s\n", CP[220]);
	  exit (1);
	}
      TCmoves = atoi (argv[1]);
      TCminutes = (short)strtol (argv[2], &p, 10);
      if (*p == ':')
	TCseconds = (short)strtol (p + 1, (char **) NULL, 10);
      else
	TCseconds = 0;
      TCflag = true;
      argc -= 3;
      argv += 3;
      while (argc > 1)
	{
	  XCmoves[XC] = atoi (argv[0]);
	  XCminutes[XC] = (short)strtol (argv[1], &p, 10);
	  if (*p == ':')
	    XCseconds[XC] = (short)strtol (p + 1, (char **) NULL, 10);
	  else
	    XCseconds[XC] = 0;
	  if (XCmoves[XC] && (XCminutes[XC] || XCseconds[XC]))
	    XC++;
	  else
	    {
	      printf (CP[220]);
	      exit (1);
	    }
	  argc -= 2;
	  argv += 2;
	}
      if (argc)
	{
	  printf ("%s\n", CP[220]);
	  exit (1);
	}
    }
  Initialize ();
  Initialize_dist ();
  Initialize_moves ();
  NewGame ();

  flag.easy = ahead;
  flag.hash = hash;
  if (xwin)
    xwndw = atoi (xwin);

  hashfile = NULL;
#if ttblsz
#ifdef HASHFILE
  hashfile = fopen (HASHFILE, RWA_ACC);
  if (hashfile)
    {
      fseek (hashfile, 0L, SEEK_END);
      filesz = ftell (hashfile) / sizeof (struct fileentry) - 1 - MAXrehash;
              hashmask = filesz>>1;
	      hashbase = hashmask+1;
    }
#if !defined CHESSTOOL && !defined XBOARD
  else
    ShowMessage (CP[98]);
#endif
#endif /* HASHFILE */
#endif /* ttblsz */
  while (!(flag.quit))
    {
      oppptr = (oppptr + 1) % MINGAMEIN;
      if (flag.bothsides && !flag.mate)
	SelectMove (opponent, 1);
      else
	InputCommand ();
      if (opponent == black)
	if (flag.gamein || TCadd)
	  {
	    TimeCalc ();
	  }
	else if (TimeControl.moves[opponent] == 0)
	  {
	    if (XC)
	      if (XCmore < XC)
		{
		  TCmoves = XCmoves[XCmore];
		  TCminutes = XCminutes[XCmore];
		  TCseconds = XCseconds[XCmore];
		  XCmore++;
		}
	    SetTimeControl ();
	  }

      compptr = (compptr + 1) % MINGAMEIN;
      if (!(flag.quit || flag.mate || flag.force))
	{
	  SelectMove (computer, 1);
	  if (computer == black)
	    if (flag.gamein)
	      {
		TimeCalc ();
	      }
	    else if (TimeControl.moves[computer] == 0)
	      {
		if (XC)
		  if (XCmore < XC)
		    {
		      TCmoves = XCmoves[XCmore];
		      TCminutes = XCminutes[XCmore];
		      TCseconds = XCseconds[XCmore];
		      XCmore++;
		    }
		SetTimeControl ();
	      }
	}
    }
#if ttblsz
#ifdef HASHFILE
  if (hashfile)
    fclose (hashfile);
#endif /* HASHFILE */
#endif /* ttblsz */

  ExitChess ();
  return (0);
}
