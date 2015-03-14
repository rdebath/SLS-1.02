/*
 * gnuchess.h - Header file for GNU CHESS
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

#if !defined(__STDC__) && !defined(MSDOS)
#define const
#endif

#ifndef __GNUC__
#define inline
#endif

#include <stdio.h>

#define SEEK_SET 0
#define SEEK_END 2
#ifdef DEBUG
void
ShowDBLine (char *, short int, short int,
	    short int, short int, short int,
	    short unsigned int *);
     extern FILE *debugfd;
     extern short int debuglevel;

#endif /* DEBUG */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#ifdef MSDOS
#include <time.h>
#include <malloc.h>
#define RWA_ACC "r+b"
#define WA_ACC "w+b"
#define printz printf
#define scanz scanf
#else
#define RWA_ACC "r+"
#define WA_ACC "w+"
#include <sys/param.h>
#include <sys/types.h>
#include <sys/times.h>
#endif /* MSDOS */
#ifdef NONDSP
#define printz printf
#define scanz scanf
#else
#include <curses.h>
#define scanz fflush(stdout),scanw
#define printz printw
#endif

#ifdef notdef
#if defined(__STDC__) || defined(MSDOS)
/* <stdio.h> */
     extern int fclose (FILE *);
#ifndef __ultrix /* work around bug in c89 compiler --t.mann */
     extern int fscanf (FILE *, const char *, ...);
     extern int fprintf (FILE *, const char *, ...);
#endif /*__ultrix*/
     extern int fflush (FILE *);

/* <stdlib.h> */
     extern int abs (int);
     extern int atoi (const char *);

/* <time.h> */
     extern long int time (long int *);

/* <string.h> */
     extern void *memset (void *, int, size_t);
#endif
#endif

/* Piece values */
#define valueP 100
#define valueN 350
#define valueB 355
#define valueR 550
#define valueQ 1100
#define valueK 1200
/* masks into upper 8 bits of ataks array */
#define ctlP 0x4000
#define ctlN 0x2800
#define ctlB 0x1800
#define ctlR 0x0400
#define ctlQ 0x0200
#define ctlK 0x0100
#define ctlBQ 0x1200
#define ctlBN 0x0800
#define ctlRQ 0x0600
#define ctlNN 0x2000
/* attack functions */
#define Patak(c, u) (atak[c][u] > ctlP)
#define Anyatak(c, u) (atak[c][u] > 0)
/* distance function */
#define taxicab(a,b) taxidata[a][b]
/* hashtable flags */
#define truescore 0x0001
#define lowerbound 0x0002
#define upperbound 0x0004
#define kingcastle 0x0008
#define queencastle 0x0010
#define evalflag 0x0020
/* king positions */
#define wking PieceList[white][0]
#define bking PieceList[black][0]
#define EnemyKing PieceList[c2][0]
/* constants */
/* castle moves */
#define BLACKCASTLE	0x3C3E
#define WHITECASTLE	0x0406
#define LONGBLACKCASTLE	0x3C3A
#define LONGWHITECASTLE	0x0402
/* truth values */
#define false 0
#define true 1
/* colors */
#define white 0
#define black 1
#define neutral 2
/* piece code defines */
#define no_piece 0
#define pawn 1
#define knight 2
#define bishop 3
#define rook 4
#define queen 5
#define king 6
#define bpawn 7
/* node flags */
#define pmask 0x0007
#define promote 0x0008
#define cstlmask 0x0010
#define epmask 0x0020
#define exact 0x0040
#define pwnthrt 0x0080
#define check 0x0100
#define capture 0x0200
#define draw 0x0400
#define book 0x1000
/* move symbols */
#define pxx (CP[2])
#define qxx (CP[1])
#define rxx (CP[4])
#define cxx (CP[3])
/* for everything that can't use the above */
#define Qxx " pnbrqk"
#define Pxx " PNBRQK"
#define Cxx "abcdefgh"
#define Rxx "12345678"
/***************************************************************************/
/***************** Table limits ********************************************/
/*
 * ttblsz must be a power of 2. Setting ttblsz 0 removes the transposition
 * tables.
 */
#ifdef MSDOS
#define vttblsz (1 << 11)
#else
#define vttblsz (1 << 17)
#define huge
#endif /* MSDOS */

#define ttblsz vttblsz
#define TREE 1500		/* max number of tree entries */
#define MAXDEPTH 35		/* max depth a search can be carried */
#define MINDEPTH 2		/* min search depth =1 (no hint), >1 hint */
#define MAXMOVES 400		/* max number of half moves in a game */
#ifndef GDBM
#if defined MSDOS
#define BOOKSIZE 10000		/* Number of unique position/move combinations allowed */
#else
#define BOOKSIZE 250000		/* Number of unique position/move combinations allowed */
#endif
#endif
#define CPSIZE 235		/* size of lang file max */
#define ETABLE (2<<13)		/* static eval cache */
/***************** tuning paramaters **********************************************/
#define MINGAMEIN 4
#define MINMOVES 15
#define CHKDEPTH 1		/* always look forward CHKDEPTH half-moves if in check */
#define DEPTHBEYOND 11		/* Max to go beyond Sdepth */
#define HASHDEPTH 4		/* depth above which to use HashFile */
#define HASHMOVELIMIT 40	/* Use HashFile only for this many moves */
#define PTVALUE 0	        /* material value below which pawn threats at 5 & 3 are used */
#define ZDEPTH 3		/* depth beyond which to check ZDELTA for extra time */
#define ZDELTA 10		/* score delta per ply to cause extra time to be given */
#define ZNODES 1000		/* check the time every ZNODES positions */
#define MAXTCCOUNTX  10		/* max number of time clicks per search to complete ply*/
#define MAXTCCOUNTR 4		/* max number of time clicks per search extensions*/
#define SCORESPLIM 8		/* Score space doesn't apply after this stage */
#define HISTORYLIM 4096		/* Max value of history killer */
#define EWNDW 10		/* Eval window to force position scoring at depth greater than Sdepth + 2 */
#define WAWNDW 90		/* alpha window when computer white*/
#define WBWNDW 90		/* beta window when computer white*/
#define BAWNDW 90		/* alpha window when computer black*/
#define BBWNDW 90		/* beta window when computer black*/
#define BXWNDW 90		/* window to force position scoring at lower */
#define WXWNDW 90		/* window to force position scoring at lower */
#define DITHER 5		/* max amount random can alter a pos value */
#define BBONUS 2		/* points per stage value of B increases */
#define RBONUS 6		/* points per stage value of R increases */
#define KINGPOSLIMIT ( -1)	/* King positional scoring limit */
#define KINGSAFETY  32
#define MAXrehash (7)

#if defined AG0
#define WHITEAG0
#define BLACKAG0

#elif defined AG1
#define WHITEAG1
#define BLACKAG1

#elif defined AG2
#define WHITEAG2
#define BLACKAG2

#elif defined AG3
#define WHITEAG3
#define BLACKAG3

#elif defined AGB
#define WHITEAG2
#define BLACKAG2

#elif defined AG4
#define WHITEAG4
#define BLACKAG4
#endif
/************************* parameters for Opening Book *********************************/
#define BOOKFAIL 5		/* if no book move found for BOOKFAIL turns stop using book */
#define BOOKMAXPLY 28		/* Max plys to keep in book database */
#define BOOKPOCKET 64
#define BOOKRAND 1000		/* used to select an opening move from a list */
#define BOOKENDPCT 950		/* 5 % chance a BOOKEND will stop the book */
#define DONTUSE -32768		/* flag move as don't use */
/*************************** Book access defines ****************************************/
#define SIDEMASK 0x1
#define LASTMOVE 0x4000		/* means this is the last move of an opening */
#define BADMOVE 0x8000		/* means this is a bad move in this position */
/****************************************************************************************/
#ifdef GDBM
#define N 2
struct keydata
{
  unsigned long bookkey;
  unsigned long bookbd;
  unsigned char side;
};
union U {
struct datadata
{
  unsigned char number;
  unsigned short sum;
  unsigned short bmove[N];
  unsigned short hint[N];
  unsigned short count[N];
}D;

struct ADMIN
{
  unsigned bookcount;
  unsigned bookup;
  unsigned bookmv;
}A;
};
#endif
     struct hashval
     {
       unsigned long key, bd;
     };
     struct hashentry
     {
       unsigned long hashbd;
       unsigned short mv;
       unsigned char flags, depth;	/* char saves some space */
       unsigned short age;
       short score;
#ifdef HASHTEST
       unsigned char bd[32];
#endif /* HASHTEST */

     };

#ifdef HASHFILE
	struct etable
	{ unsigned long ehashbd;
		short int escore[2];
		short int sscore[64];
		short int score;
		short int hung[2];
	} ;

/*
 * persistent transposition table. The size must be a power of 2. If you
 * change the size, be sure to run gnuchess -c before anything else.
 */
#define frehash 6
#ifdef MSDOS
#define Deffilesz (1 << 11) -1
#else
#define Deffilesz (1 << 17) -1
#endif /* MSDOS */
     struct fileentry
     {
       unsigned char bd[32];
       unsigned char f, t, flags, depth, sh, sl;
     };

#endif /* HASHFILE */


     struct leaf
     {
       short int f, t, score, reply, width;
       unsigned short flags;
     };
     struct GameRec
     {
       unsigned short gmove;	/* this move */
       short score;		/* score after this move */
       short depth;		/* search depth this move */
       long time;               /* search time this move */
       short piece;		/* piece captured */
       short color;		/* color */
       short flags;		/* move flags capture, promote, castle */
       short Game50;		/* flag for repetition */
       long nodes;		/* nodes searched for this move */
       unsigned long hashkey, hashbd;	/* board key before this move */
       short epssq;		/* epssquare before this move */
#ifdef DEBUG40
       int d1;
       int d2;
       int d3;
       int d4;
       int d5;
       int d6;
       int d7;
#endif
     };
     struct TimeControlRec
     {
       short moves[2];
       long clock[2];
     };

     struct flags
     {
       short mate;		/* the game is over */
       short post;		/* show principle variation */
       short quit;		/* quit/exit */
       short regularstart;	/* did the game start from standard
				 * initial board ? */
       short reverse;		/* reverse board display */
       short bothsides;		/* computer plays both sides */
       short hash;		/* enable/disable transposition table */
       short force;		/* enter moves */
       short easy;		/* disable thinking on opponents time */
       short beep;		/* enable/disable beep */
       short timeout;		/* time to make a move */
       short musttimeout;	/* time to make a move */
       short back;		/* time to make a move */
       short rcptr;		/* enable/disable recapture heuristics */
       short rv;		/* reverse video */
       short stars;		/* add stars to uxdsp screen */
       short coords;		/* add coords to visual screen */
       short shade;
       short material;		/* draw on lack of material */
       short illegal;		/* illegal position */
       short onemove;		/* timing is onemove */
       short gamein;		/* timing is gamein */
     };

#ifdef DEBUG
     extern FILE *debugfile;

#endif /* DEBUG */
#ifdef HISTORY
     extern unsigned short history[32768];
#endif
     extern char *ColorStr[2];
     extern unsigned short int MV[MAXDEPTH];
     extern int MSCORE;
     extern int mycnt1, mycnt2;
     extern short int ahead;
     extern short int chesstool;
     extern struct leaf Tree[], *root,rootnode;
     extern char savefile[], listfile[];
     extern short TrPnt[];
     extern short board[], color[];
     extern short PieceList[2][64], PawnCnt[2][8];
     extern short castld[], Mvboard[];
     extern short svalue[64];
     extern struct flags flag;
     extern short opponent, computer, INCscore;
     extern short WAwindow, BAwindow, WBwindow, BBwindow;
     extern short dither, player;
     extern short xwndw, epsquare, contempt;
     extern long ResponseTime, ExtraTime, MaxResponseTime, et, et0, time0, ft;
     extern long reminus, replus;
     extern long GenCnt, NodeCnt, ETnodes, EvalNodes, HashAdd, HashCnt, HashCol, THashCol,
      FHashCnt, FHashAdd;
     extern short HashDepth, HashMoveLimit;
     extern struct GameRec GameList[];
     extern short GameCnt, Game50;
     extern short Sdepth, MaxSearchDepth;
     extern int Book;
     extern struct TimeControlRec TimeControl;
     extern int TCadd;
     extern short TCflag, TCmoves, TCminutes, TCseconds, OperatorTime;
     extern int timecomp[MINGAMEIN], timeopp[MINGAMEIN];
     extern int compptr,oppptr;
     extern short XCmore, XCmoves[], XCminutes[], XCseconds[], XC;
     extern const short otherside[];
     extern const short Stboard[];
     extern const short Stcolor[];
     extern unsigned short hint;
     extern short int TOflag;
     extern short stage, stage2, Developed[];
     extern short ChkFlag[], CptrFlag[], PawnThreat[];
     extern short Pscore[], Tscore[];
     extern /*unsigned*/ short rehash;  /* -1 is used as a flag --tpm */
     extern unsigned int ttblsize;
     extern short mtl[], pmtl[], hung[], emtl[];
     extern short Pindex[];
     extern short PieceCnt[];
     extern short FROMsquare, TOsquare;
     extern short HasKnight[], HasBishop[], HasRook[], HasQueen[];
     extern const short qrook[];
     extern const short krook[];
     extern const short kingP[];
     extern const short rank7[];
     extern const short sweep[];
     extern const short epmove1[], epmove2[];
     extern unsigned short killr0[], killr1[];
     extern unsigned short killr2[], killr3[];
     extern unsigned short PV, SwagHt, Swag0, Swag1, Swag2, Swag3, Swag4, sidebit;
     extern short killt[0x4000];
     extern short mtl[2], pmtl[2], hung[2];
     extern const short value[];
     extern const short control[];
     extern unsigned char nextpos[8][64][64];
     extern unsigned char nextdir[8][64][64];
     extern const short ptype[2][8];
     extern long filesz,hashmask,hashbase;
     extern FILE *hashfile;
     extern unsigned int starttime;
     extern short distdata[64][64], taxidata[64][64];
     extern union U admin;
     extern short unsigned bookmaxply;
#if !defined GDBM
     extern int unsigned bookcount;
     extern int unsigned booksize;
#endif
     extern unsigned long hashkey, hashbd;
     extern struct hashval hashcode[2][7][64];
     extern char *CP[];
#ifdef QUIETBACKGROUND
     extern short background;
#endif /* QUIETBACKGROUND */

#ifdef ttblsz
     extern struct hashentry huge *ttable[2];

#endif

/*
 * hashbd contains a 32 bit "signature" of the board position. hashkey
 * contains a 16 bit code used to address the hash table. When a move is
 * made, XOR'ing the hashcode of moved piece on the from and to squares with
 * the hashbd and hashkey values keeps things current.
 */
#define UpdateHashbd(side, piece, f, t) \
{\
  if ((f) >= 0)\
    {\
      hashbd ^= hashcode[side][piece][f].bd;\
      hashkey ^= hashcode[side][piece][f].key;\
    }\
  if ((t) >= 0)\
    {\
      hashbd ^= hashcode[side][piece][t].bd;\
      hashkey ^= hashcode[side][piece][t].key;\
    }\
}


     extern short rpthash[2][256];
     extern char *DRAW;

#define distance(a,b) distdata[a][b]
#define row(a) ((a) >> 3)
#define column(a) ((a) & 7)
#define locn(a,b) (((a) << 3) | (b))
     extern short distdata[64][64];

/* init external functions */
     extern void InitConst (char *lang);
     extern void Initialize_dist (void);
     extern void NewGame (void);
     extern int parse (FILE * fd, short unsigned int *mv, short int side, char *opening);
     extern void GetOpenings (void);
     extern int OpeningBook (unsigned short int *hint, short int side);
     extern void SelectMove (short int side, short int iop);
     extern int
      search (short int side,
	       short int ply,
	       short int depth,
	       short int alpha,
	       short int beta,
	       short unsigned int *bstline,
	       short int *rpt);
#if ttblsz
     extern int
      ProbeTTable (short int side,
		    short int depth,
		    short int ply,
		    short int *alpha,
		    short int *beta,
		    short int *score);
     extern int
      PutInTTable (short int side,
		    short int score,
		    short int depth,
		    short int ply,
		    short int alpha,
		    short int beta,
		    short unsigned int mv);
     extern void ZeroTTable (void);
     extern void ZeroRPT (void);
     extern void Initialize_ttable (void);
     extern unsigned int urand (void);
#ifdef HASHFILE
     extern void gsrand (unsigned int);
     extern int
      ProbeFTable (short int side,
		    short int depth,
		    short int ply,
		    short int *alpha,
		    short int *beta,
		    short int *score);
     extern void
      PutInFTable (short int side,
		    short int score,
		    short int depth,
		    short int ply,
		    short int alpha,
		    short int beta,
		    short unsigned int f,
		    short unsigned int t);

#endif /* HASHFILE */
#endif /* ttblsz */
     extern void Initialize_moves (void);
     extern void MoveList (short int side, short int ply);
     extern void CaptureList (short int side, short int ply);
     extern int castle (short int side, short int kf, short int kt, short int iop);
     extern void ataks (short int side, short int *a);
     extern void
      MakeMove (short int side,
		 struct leaf * node,
		 short int *tempb,
		 short int *tempc,
		 short int *tempsf,
		 short int *tempst,
		 short int *INCscore);
     extern void
      UnmakeMove (short int side,
		   struct leaf * node,
		   short int *tempb,
		   short int *tempc,
		   short int *tempsf,
		   short int *tempst);
     extern void InitializeStats (void);
     extern int
      evaluate (short int side,
		 short int ply,
		 short int alpha,
		 short int beta,
		 short int INCscore,
		 short int *InChk);
     extern short int ScorePosition (short int side);
     extern void ExaminePosition (void);
     extern void UpdateWeights (void);
     extern void Initialize (void);
     extern void InputCommand (void);
     extern void ExitChess (void);
     extern void ClrScreen (void);
     extern void SetTimeControl (void);
     extern void SelectLevel (char *sx);
     extern void
      UpdateDisplay (short int f,
		      short int t,
		      short int flag,
		      short int iscastle);
     extern void ElapsedTime (short int iop);
     extern void ShowSidetoMove (void);
     extern void SearchStartStuff (short int side);
     extern void ShowDepth (char ch);
     extern void TerminateSearch (int);
     extern void
      ShowResults (short int score,
		    short unsigned int *bstline,
		    char ch);
     extern void PromptForMove (void);
     extern void SetupBoard (void);
     extern void algbr (short int f, short int t, short int flag);
     extern void OutputMove (void);
     extern void ShowCurrentMove (short int pnt, short int f, short int t);
     extern void ListGame (void);
     extern void ShowMessage (char *s);
     extern void ClrScreen (void);
     extern void gotoXY (short int x, short int y);
     extern void ClrEoln (void);
     extern void DrawPiece (short int sq);
     extern void UpdateClocks (void);
     extern void DoDebug (void);
     extern void DoTable (short table[64]);
     extern void ShowPostnValues (void);
     extern void ChangeXwindow (void);
     extern void SetContempt (void);
     extern void ChangeHashDepth (void);
     extern void ChangeBetaWindow (void);
     extern void ChangeAlphaWindow (void);
     extern void GiveHint (void);
     extern void ShowPrompt (void);
     extern void EditBoard (void);
     extern void help (void);
     extern void ChangeSearchDepth (void);
     extern void skip (void);
     extern void skipb (void);
     extern void EnPassant (short int xside, short int f, short int t, short int iop);
     extern void ShowNodeCnt (long int NodeCnt);
     extern void ShowLine (short unsigned int *bstline);
     extern int pick (short int p1, short int p2);
     extern int VerifyMove (char *s, short int inp, unsigned short *mv);
     extern void AgeTT();
     extern unsigned short TTage;
#ifdef GDBM
     extern struct ADMIN B;
#endif
#ifdef GDX
     extern struct gdxadmin B;
#endif
