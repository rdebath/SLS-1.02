/*
 * XBoard -- an Xt/Athena user interface for GNU Chess
 *
 * Original authors:  Dan Sears and Chris Sears.
 * Enhancements (Version 2.0 and following):  Tim Mann.
 * Thanks to John Chanak for the initial implementation of ICS mode.
 *
 * XBoard borrows its colors, icon and piece bitmaps from XChess
 * which was written and is copyrighted by Wayne Christopher.
 *
 * Copyright 1991 by Digital Equipment Corporation, Maynard, Massachusetts.
 * Enhancements Copyright 1992 Free Software Foundation, Inc.
 *
 * The following terms apply to Digital Equipment Corporation's copyright
 * interest in XBoard:
 * ------------------------------------------------------------------------
 * All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * ------------------------------------------------------------------------
 *
 * This file is part of XBOARD.
 *
 * XBOARD is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY.  No author or distributor accepts responsibility to anyone for
 * the consequences of using it or for whether it serves any particular
 * purpose or works at all, unless he says so in writing.  Refer to the XBOARD
 * General Public License for full details.
 *
 * Everyone is granted permission to copy, modify and redistribute XBOARD, but
 * only under the conditions described in the XBOARD General Public License. A
 * copy of this license is supposed to have been given to you along with
 * XBOARD so you can know your rights and responsibilities.  It should be in a
 * file named COPYING.  Among other things, the copyright notice and this
 * notice must be preserved on all copies.
 * ------------------------------------------------------------------------
 *
 * See the file ChangeLog for a detailed revision history.
 */

#define VERSION "2.1"

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <time.h>
#ifdef HAS_GETTIMEOFDAY
#ifndef ESIX
#include <sys/time.h>
#endif
#endif
#ifdef	__STDC__
#ifndef ESIX
#include <stdlib.h>
#endif
#endif
#if defined(SYSTEM_FIVE) || defined(SYSV)
#include <sys/types.h>
#include <sys/stat.h>
#ifdef AIXV3
#include <fcntl.h>
#else
#include <sys/fcntl.h>
#endif /*AIXV3*/
#ifdef SVR4
#include <stropts.h>
#endif
#endif
#if defined(__STDC__) || defined(SYSTEM_FIVE) || defined(SYSV) || defined(sun)
#include <string.h>
#else
#include <strings.h>
#endif
#include <pwd.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/cursorfont.h>
#include "xboard.h"
#include "patchlevel.h"

#include "bitmaps/s_p.bm"
#include "bitmaps/s_r.bm"
#include "bitmaps/s_kt.bm"
#include "bitmaps/s_b.bm"
#include "bitmaps/s_q.bm"
#include "bitmaps/s_k.bm"

#include "bitmaps/ol_p.bm"
#include "bitmaps/ol_r.bm"
#include "bitmaps/ol_kt.bm"
#include "bitmaps/ol_b.bm"
#include "bitmaps/ol_q.bm"
#include "bitmaps/ol_k.bm"

#include "bitmaps/sm_s_p.bm"
#include "bitmaps/sm_s_r.bm"
#include "bitmaps/sm_s_kt.bm"
#include "bitmaps/sm_s_b.bm"
#include "bitmaps/sm_s_q.bm"
#include "bitmaps/sm_s_k.bm"

#include "bitmaps/sm_ol_p.bm"
#include "bitmaps/sm_ol_r.bm"
#include "bitmaps/sm_ol_kt.bm"
#include "bitmaps/sm_ol_b.bm"
#include "bitmaps/sm_ol_q.bm"
#include "bitmaps/sm_ol_k.bm"

#include "bitmaps/xs_s_p.bm"
#include "bitmaps/xs_s_r.bm"
#include "bitmaps/xs_s_kt.bm"
#include "bitmaps/xs_s_b.bm"
#include "bitmaps/xs_s_q.bm"
#include "bitmaps/xs_s_k.bm"

#include "bitmaps/xs_ol_p.bm"
#include "bitmaps/xs_ol_r.bm"
#include "bitmaps/xs_ol_kt.bm"
#include "bitmaps/xs_ol_b.bm"
#include "bitmaps/xs_ol_q.bm"
#include "bitmaps/xs_ol_k.bm"

#include "bitmaps/icon.bm"

int establish P((char *host, int port));
void read_from_player P((caddr_t client_data, int *file_num, XtInputId *id));
void read_from_ics P((caddr_t client_data, int *file_num, XtInputId *id));
void main P((int argc, char **argv));
void CreateGCs P((void));
void CreatePieces P((void));
void CreatePieceMenus P((void));
char *FindFont P((char *pattern, int targetPxlSize));
void PieceMenuPopup P((Widget w, XEvent *event,
		       String *params, Cardinal *num_params));
static void PieceMenuSelect P((Widget w, ChessSquare piece, caddr_t junk));
static void SetWhiteToPlay P((Widget w, XEvent *event,
			      String *prms, Cardinal *nprms));
static void SetBlackToPlay P((Widget w, XEvent *event,
			      String *prms, Cardinal *nprms));
void ReadBitmap P((String name, Pixmap *pm, char large_bits[],
		   char medium_bits[], char small_bits[]));
void CreateGrid P((void));
int EventToSquare P((int x));
ChessSquare CharToPiece P((int c));
char PieceToChar P((ChessSquare p));
ChessSquare PromoPiece P((ChessMove move_type));
ChessMove CoordsToAlgebraic P((int fromX, int fromY, int toX, int toY,
			       int promoChar, int currentBoardIndex,
			       char out[MOVE_LEN]));
void CastleCoords P((ChessMove move_type, int *fromX, int *fromY,
		     int *toX, int *toY));
void DrawSquare P((int row, int column, ChessSquare piece));
void EventProc P((Widget widget, caddr_t unused, XEvent *event));
void DrawPosition P((Widget w, XEvent *event,
		     String *prms, Cardinal *nprms));
void InitPosition P((void));
void CopyBoard P((Board to, Board from));
void SendCurrentBoard P((FILE *fp));
void SendBoard P((FILE *fp, Board board));
void HandleUserMove P((Widget w, XEvent *event,
		       String *prms, Cardinal *nprms));
void FinishUserMove P((ChessMove move_type, int to_x, int to_y));
void HandleMachineMove P((char *message, FILE *fp));
void LoadGameLoop P((void));
Boolean LoadGameOneMove P((void));
void ApplyMove P((ChessMove *move_type, int from_x, int from_y,
		  int to_x, int to_y, Board board));
void MakeMove P((ChessMove *move_type, int from_x, int from_y,
		 int to_x, int to_y));
void InitChessProgram P((char *host_name, char *program_name, int *pid,
			 FILE **to, FILE **from, XtIntervalId *xid,
			 int *sendTime));
void GameEnds P((char *message));
void ShutdownChessPrograms P((char *message));
void CommentPopUp P((char *label));
void CommentPopDown P((void));
void FileNamePopUp P((char *label, Boolean (*proc)(char *name)));
void FileNameCallback P((Widget w, XtPointer client_data,
			 XtPointer call_data));
void FileNameAction P((Widget w, XEvent *event,
		       String *prms, Cardinal *nprms));
void PromotionPopUp P((ChessSquare piece, int to_x, int to_y));
void PromotionCallback P((Widget w, XtPointer client_data,
			  XtPointer call_data));
void SelectCommand P((Widget w, XtPointer client_data, XtPointer call_data));
void ModeHighlight P((void));
void LoadGameProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void QuitProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void AcceptMatchProc P((Widget w, XEvent *event,
			String *prms, Cardinal *nprms));
void DrawProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void DeclineDrawProc P((Widget w, XEvent *event,
			String *prms, Cardinal *nprms));
void ResignProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
Boolean LoadGame P((char *name));
void MachineBlackProc P((Widget w, XEvent *event, String *prms,
			 Cardinal *nprms));
void ForwardProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void ResetProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
Boolean LoadPosition P((char *name));
void LoadPositionProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void MachineWhiteProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void BackwardProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void FlipViewProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
Boolean SaveGame P((char *name));
void SaveGameProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void SwitchProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void EditPositionProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void EditPositionDone P((void));
void ForceProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void HintProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void NothingProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
Boolean SavePosition P((char *name));
void SavePositionProc P((Widget w, XEvent *event,
			 String *prms, Cardinal *nprms));
void TwoMachinesProc P((Widget w, XEvent *event, String *prms,
			Cardinal *nprms));
void PauseProc P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void Iconify P((Widget w, XEvent *event, String *prms, Cardinal *nprms));
void PrintOpponents P((FILE *fp));
void PrintPosition P((FILE *fp, int move));
void SendToProgram P((char *message, FILE *fp));
void ReceiveFromProgram P((FILE *fp, int *source, XtInputId *id));
void SendSearchDepth P((FILE *fp));
void SendTimeRemaining P((FILE *fp));
void DisplayMessage P((char *message));
void DisplayMove P((int moveNumber));
void DisplayTitle P((char *title));
void Attention P((int pid));
void DisplayClocks P((int clock_mode));
void DisplayTimerLabel P((Widget w, char *color, long timer));
char *TimeString P((long tm));
void Usage P((void));
char *StrStr P((char *string, char *match));
int StrCaseCmp P((char *s1, char *s2));
int ToLower P((int c));
int ToUpper P((int c));
#if defined(SYSTEM_FIVE) || defined(SYSV)
char *PseudoTTY P((int *ptyv));
#else
void CatchPipeSignal P((int dummy));
#endif
extern int yylex P((void));
extern int yynewfile P((void));
extern ChessMove yylexstr P((int boardIndex, char *s, char **next));
void ParseGameHistory P((char *game));
void ParseBoard8 P((char *string));

/*
* XBoard depends on Xt R4 or higher
*/
int xtVersion = XtSpecificationRelease;

int xScreen;
Display *xDisplay;
Window xBoardWindow;
GC lightSquareGC, darkSquareGC, lineGC, wdPieceGC, wlPieceGC,
  bdPieceGC, blPieceGC, wbPieceGC, bwPieceGC, coordGC;
Pixmap solidPawnBitmap, solidRookBitmap, solidKnightBitmap,
  solidBishopBitmap, solidQueenBitmap, solidKingBitmap,
  outlinePawnBitmap, outlineRookBitmap, outlineKnightBitmap,
  outlineBishopBitmap, outlineQueenBitmap, outlineKingBitmap, iconPixmap;
Widget shellWidget, formWidget, boardWidget, commandsWidget, messageWidget,
  whiteTimerWidget, blackTimerWidget, titleWidget, widgetList[6], 
  commentShell, promotionShell, whitePieceMenu, blackPieceMenu;
XSegment gridSegments[(BOARD_SIZE + 1) * 2];
XtIntervalId firstProgramXID = 0, secondProgramXID = 0,
  readGameXID = 0, timerXID = 0;
Font mainFontID, coordFontID;
XFontStruct *mainFontStruct, *coordFontStruct;
XtAppContext appContext;
Boolean (*fileProc) P((char *name));
Position commentX = -1, commentY = -1;

FILE *fromFirstProgFP, *toFirstProgFP, *fromSecondProgFP,
  *toSecondProgFP, *gameFileFP, *lastMsgFP;
int currentMove = 0, forwardMostMove = 0, backwardMostMove = 0,
  firstProgramPID = 0, secondProgramPID = 0, telnetPID = 0,
  squareSize = LARGE_SQUARE_SIZE, fromX = -1,
  fromY = -1, firstMove = True, flipView = False,
  commentUp = False, filenameUp = False,
  blackPlaysFirst = False, startedFromSetupPosition = False,
  promotionUp = False, searchTime = 0, pmFromX = -1, pmFromY = -1,
  whiteFlag = False, blackFlag = False, maybeThinking = False, 
  ics_input = -1, ics_output = -1, ics_user_moved = 0, ics_gamenum = -1,
  ics_getting_history = 0;
int firstSendTime = 2, secondSendTime = 2;  /* 0=don't, 1=do, 2=test first*/
IcsMode ics_mode = IcsIdle;
Pixel timerForegroundPixel, timerBackgroundPixel;
MatchMode matchMode = MatchFalse;
GameMode gameMode = BeginningOfGame, lastGameMode = BeginningOfGame,
  pausePreviousMode = BeginningOfGame;
BoardSize boardSize = Large;
char moveList[MAX_MOVES][MOVE_LEN], parseList[MAX_MOVES][MOVE_LEN * 2],
  ptyname[24], *chessDir, *programName, ics_black[32], ics_white[32],
  endMessage[MOVE_LEN * 4], ics_match_offer[MSG_SIZ];

long whiteTimeRemaining, blackTimeRemaining, timeControl;
extern char currentMoveString[];
extern char yytext[];
extern int yyboardindex;
     
Board boards[MAX_MOVES], initialPosition = {
    { WhiteRook, WhiteKnight, WhiteBishop, WhiteQueen,
	WhiteKing, WhiteBishop, WhiteKnight, WhiteRook },
    { WhitePawn, WhitePawn, WhitePawn, WhitePawn,
	WhitePawn, WhitePawn, WhitePawn, WhitePawn },
    { EmptySquare, EmptySquare, EmptySquare, EmptySquare,
	EmptySquare, EmptySquare, EmptySquare, EmptySquare },
    { EmptySquare, EmptySquare, EmptySquare, EmptySquare,
	EmptySquare, EmptySquare, EmptySquare, EmptySquare },
    { EmptySquare, EmptySquare, EmptySquare, EmptySquare,
	EmptySquare, EmptySquare, EmptySquare, EmptySquare },
    { EmptySquare, EmptySquare, EmptySquare, EmptySquare,
	EmptySquare, EmptySquare, EmptySquare, EmptySquare },
    { BlackPawn, BlackPawn, BlackPawn, BlackPawn,
	BlackPawn, BlackPawn, BlackPawn, BlackPawn },
    { BlackRook, BlackKnight, BlackBishop, BlackQueen,
	BlackKing, BlackBishop, BlackKnight, BlackRook }
};
     
String gnuButtonStrings[] = {
    "Quit",          "Machine Black", "Load Game",     "Forward",
    "Reset",         "Machine White", "Save Game",     "Backward",
    "Flip View",     "Force Moves",   "Load Position", "Pause",
    "Edit Position", "Two Machines",  "Save Position", "Hint"
  };
/* must be in same order as buttonStrings! */
XtActionProc gnuButtonProcs[] = {
    QuitProc,         MachineBlackProc, LoadGameProc,     ForwardProc,
    ResetProc,        MachineWhiteProc, SaveGameProc,     BackwardProc,
    FlipViewProc,     ForceProc,        LoadPositionProc, PauseProc,
    EditPositionProc, TwoMachinesProc,  SavePositionProc, HintProc,  
    NULL
  };

String icsButtonStrings[] = {
    "Quit",          "Accept Match", "Load Game",     "Forward",       
    "Reset",         "Draw",         "Save Game",     "Backward",
    "Flip View",     "Decline Draw", "Load Position", "Pause",
    "Edit Position", "Resign",       "Save Position", ""
  };
/* must be in same order as icsButtonStrings! */
XtActionProc icsButtonProcs[] = {
    QuitProc,         AcceptMatchProc, LoadGameProc,     ForwardProc,
    ResetProc,        DrawProc,        SaveGameProc,     BackwardProc,
    FlipViewProc,     DeclineDrawProc, LoadPositionProc, PauseProc,
    EditPositionProc, ResignProc,      SavePositionProc, NothingProc,
    NULL
  };

String *buttonStrings;
XtActionProc *buttonProcs;
int buttonCount;

#define PIECE_MENU_SIZE 10
String pieceMenuStrings[PIECE_MENU_SIZE] = {
    "----", "Pawn", "Knight", "Bishop", "Rook", "Queen", "King",
    "----", "Empty square", "Clear board"
  };
/* must be in same order as PieceMenuStrings! */
ChessSquare pieceMenuTranslation[2][PIECE_MENU_SIZE] = {
    { (ChessSquare) 0, WhitePawn, WhiteKnight, WhiteBishop,
	WhiteRook, WhiteQueen, WhiteKing,
	(ChessSquare) 0, EmptySquare, ClearBoard },
    { (ChessSquare) 0, BlackPawn, BlackKnight, BlackBishop,
	BlackRook, BlackQueen, BlackKing,
	(ChessSquare) 0, EmptySquare, ClearBoard },
};

Arg shellArgs[] = {
    { XtNwidth, 0 },
    { XtNheight, 0 },
    { XtNminWidth, 0 },
    { XtNminHeight, 0 },
    { XtNmaxWidth, 0 },
    { XtNmaxHeight, 0 }
};

Arg boardArgs[] = {
    { XtNborderWidth, 0 },
    { XtNwidth, LINE_GAP + BOARD_SIZE * (LARGE_SQUARE_SIZE + LINE_GAP) },
    { XtNheight, LINE_GAP + BOARD_SIZE * (LARGE_SQUARE_SIZE + LINE_GAP) }
};

Arg messageArgs[] = {
    { XtNborderWidth, 0 },
    { XtNjustify, (XtArgVal) XtJustifyLeft },
    { XtNlabel, (XtArgVal) "starting..." }
};

Arg timerArgs[] = {
    { XtNborderWidth, 0 },
    { XtNjustify, (XtArgVal) XtJustifyLeft }
};

Arg titleArgs[] = {
    { XtNborderWidth, 0 },
    { XtNjustify, (XtArgVal) XtJustifyLeft }
};

typedef struct {
    Pixel whitePieceColor;
    Pixel blackPieceColor;
    Pixel lightSquareColor;
    Pixel darkSquareColor;
    int movesPerSession;
    String initString;
    String whiteString;
    String blackString;
    String firstChessProgram;
    String secondChessProgram;
    Boolean noChessProgram;
    String firstHost;
    String secondHost;
    String solidPawnBitmap;
    String solidRookBitmap;
    String solidBishopBitmap;
    String solidKnightBitmap;
    String solidQueenBitmap;
    String solidKingBitmap;
    String outlinePawnBitmap;
    String outlineRookBitmap;
    String outlineBishopBitmap;
    String outlineKnightBitmap;
    String outlineQueenBitmap;
    String outlineKingBitmap;
    String remoteShell;
    float timeDelay;
    String timeControl;
    Boolean icsActive;
    String icsHost;
    int icsPort;
    Boolean useTelnet;
    String gateway;
    String loadGameFile;
    String saveGameFile;
    Boolean autoSaveGames;
    String loadPositionFile;
    String savePositionFile;
    String matchMode;
    Boolean monoMode;
    Boolean debugMode;
    Boolean clockMode;
    String boardSize;
    Boolean Iconic;
    String searchTime;
    int searchDepth;
    Boolean showCoords;
    String mainFont;
    String coordFont;
    Boolean ringBellAfterMoves;
    int borderXoffset;
    int borderYoffset;
} AppData, *AppDataPtr;

AppData appData;

XtResource clientResources[] = {
    { "whitePieceColor", "WhitePieceColor", XtRPixel, sizeof(Pixel),
	XtOffset(AppDataPtr, whitePieceColor), XtRString,
	WHITE_PIECE_COLOR },
    { "blackPieceColor", "BlackPieceColor", XtRPixel, sizeof(Pixel),
	XtOffset(AppDataPtr, blackPieceColor), XtRString,
	BLACK_PIECE_COLOR },
    { "lightSquareColor", "LightSquareColor", XtRPixel,
	sizeof(Pixel), XtOffset(AppDataPtr, lightSquareColor),
	XtRString, LIGHT_SQUARE_COLOR }, 
    { "darkSquareColor", "DarkSquareColor", XtRPixel, sizeof(Pixel),
	XtOffset(AppDataPtr, darkSquareColor), XtRString,
	DARK_SQUARE_COLOR },
    { "movesPerSession", "movesPerSession", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, movesPerSession), XtRImmediate,
	(XtPointer) MOVES_PER_SESSION },
    { "initString", "initString", XtRString, sizeof(String),
	XtOffset(AppDataPtr, initString), XtRString, INIT_STRING },
    { "whiteString", "whiteString", XtRString, sizeof(String),
	XtOffset(AppDataPtr, whiteString), XtRString, WHITE_STRING },
    { "blackString", "blackString", XtRString, sizeof(String),
	XtOffset(AppDataPtr, blackString), XtRString, BLACK_STRING },
    { "firstChessProgram", "firstChessProgram", XtRString,
	sizeof(String), XtOffset(AppDataPtr, firstChessProgram),
	XtRString, FIRST_CHESS_PROGRAM },
    { "secondChessProgram", "secondChessProgram", XtRString,
	sizeof(String), XtOffset(AppDataPtr, secondChessProgram),
	XtRString, SECOND_CHESS_PROGRAM },
    { "noChessProgram", "noChessProgram", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, noChessProgram),
	XtRImmediate, (XtPointer) False },
    { "firstHost", "firstHost", XtRString, sizeof(String),
	XtOffset(AppDataPtr, firstHost), XtRString, FIRST_HOST },
    { "secondHost", "secondHost", XtRString, sizeof(String),
	XtOffset(AppDataPtr, secondHost), XtRString, SECOND_HOST },
    { "solidPawnBitmap", "solidPawnBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, solidPawnBitmap),
	XtRString, "" },
    { "solidRookBitmap", "solidRookBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, solidRookBitmap),
	XtRString, "" },
    { "solidKnightBitmap", "solidKnightBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, solidKnightBitmap),
	XtRString, "" },
    { "solidBishopBitmap", "solidBishopBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, solidBishopBitmap),
	XtRString, "" },
    { "solidQueenBitmap", "solidQueenBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, solidQueenBitmap),
	XtRString, "" },
    { "solidKingBitmap", "solidKingBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, solidKingBitmap),
	XtRString, "" },
    { "outlinePawnBitmap", "outlinePawnBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, outlinePawnBitmap),
	XtRString, "" },
    { "outlineRookBitmap", "outlineRookBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, outlineRookBitmap),
	XtRString, "" },
    { "outlineKnightBitmap", "outlineKnightBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, outlineKnightBitmap),
	XtRString, "" },
    { "outlineBishopBitmap", "outlineBishopBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, outlineBishopBitmap),
	XtRString, "" },
    { "outlineQueenBitmap", "outlineQueenBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, outlineQueenBitmap),
	XtRString, "" },
    { "outlineKingBitmap", "outlineKingBitmap", XtRString,
	sizeof(String), XtOffset(AppDataPtr, outlineKingBitmap),
	XtRString, "" },
    { "remoteShell", "remoteShell", XtRString, sizeof(String),
	XtOffset(AppDataPtr, remoteShell), XtRString, "rsh" },
    { "timeDelay", "timeDelay", XtRFloat, sizeof(float),
	XtOffset(AppDataPtr, timeDelay), XtRString,
	(XtPointer) TIME_DELAY },
    { "timeControl", "timeControl", XtRString, sizeof(String),
	XtOffset(AppDataPtr, timeControl), XtRString,
	(XtPointer) TIME_CONTROL },
    { "internetChessServerMode", "internetChessServerMode",
	XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, icsActive), XtRImmediate,
	(XtPointer) False },
    { "internetChessServerHost", "internetChessServerHost",
	XtRString, sizeof(String),
	XtOffset(AppDataPtr, icsHost),
	XtRString, (XtPointer) ICS_HOST },
    { "internetChessServerPort", "internetChessServerPort",
	XtRInt, sizeof(int),
	XtOffset(AppDataPtr, icsPort), XtRImmediate,
	(XtPointer) ICS_PORT },
    { "useTelnet", "useTelnet", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, useTelnet), XtRImmediate,
	(XtPointer) False },
    { "gateway", "gateway", XtRString, sizeof(String),
	XtOffset(AppDataPtr, gateway), XtRString, "" },
    { "loadGameFile", "loadGameFile", XtRString, sizeof(String),
	XtOffset(AppDataPtr, loadGameFile), XtRString, "" },
    { "saveGameFile", "saveGameFile", XtRString, sizeof(String),
	XtOffset(AppDataPtr, saveGameFile), XtRString, "" },
    { "autoSaveGames", "autoSaveGames", XtRBoolean,
	sizeof(Boolean), XtOffset(AppDataPtr, autoSaveGames),
	XtRImmediate, (XtPointer) False },
    { "loadPositionFile", "loadPositionFile", XtRString,
	sizeof(String), XtOffset(AppDataPtr, loadPositionFile),
	XtRString, "" },
    { "savePositionFile", "savePositionFile", XtRString,
	sizeof(String), XtOffset(AppDataPtr, savePositionFile),
	XtRString, "" },
    { "matchMode", "matchMode", XtRString, sizeof(String),
	XtOffset(AppDataPtr, matchMode), XtRString, MATCH_MODE },
    { "monoMode", "monoMode", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, monoMode), XtRImmediate,
	(XtPointer) False },
    { "debugMode", "debugMode", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, debugMode), XtRImmediate,
	(XtPointer) False },
    { "Iconic", "Iconic", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, Iconic), XtRImmediate,
	(XtPointer) False },
    { "clockMode", "clockMode", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, clockMode), XtRImmediate,
	(XtPointer) True },
    { "boardSize", "boardSize", XtRString, sizeof(String),
	XtOffset(AppDataPtr, boardSize), XtRString, DEFAULT_SIZE },
    { "searchTime", "searchTime", XtRString, sizeof(String),
	XtOffset(AppDataPtr, searchTime), XtRString,
	(XtPointer) "" },
    { "searchDepth", "searchDepth", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, searchDepth), XtRImmediate, 
	(XtPointer) 0 },
    { "showCoords", "showCoords", XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, showCoords), XtRImmediate,
	(XtPointer) False },
    { "mainFont", "mainFont", XtRString, sizeof(String),
	XtOffset(AppDataPtr, mainFont), XtRString, MAIN_FONT },
    { "coordFont", "coordFont", XtRString, sizeof(String),
	XtOffset(AppDataPtr, coordFont), XtRString, COORD_FONT },
    { "ringBellAfterMoves", "ringBellAfterMoves",
	XtRBoolean, sizeof(Boolean),
	XtOffset(AppDataPtr, ringBellAfterMoves),
	XtRImmediate, (XtPointer) False	},
    { "borderXoffset", "borderXoffset", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, borderXoffset), XtRImmediate,
	(XtPointer) BORDER_X_OFFSET },
    { "borderYoffset", "borderYOffset", XtRInt, sizeof(int),
	XtOffset(AppDataPtr, borderYoffset), XtRImmediate,
	(XtPointer) BORDER_Y_OFFSET },
};

Pixmap *pieceToSolid[] = {
    &solidPawnBitmap, &solidRookBitmap, &solidKnightBitmap,
    &solidBishopBitmap, &solidQueenBitmap, &solidKingBitmap,
    &solidPawnBitmap, &solidRookBitmap, &solidKnightBitmap,
    &solidBishopBitmap, &solidQueenBitmap, &solidKingBitmap
  };

Pixmap *pieceToOutline[] = {
    &outlinePawnBitmap, &outlineRookBitmap, &outlineKnightBitmap,
    &outlineBishopBitmap, &outlineQueenBitmap, &outlineKingBitmap,
    &outlinePawnBitmap, &outlineRookBitmap, &outlineKnightBitmap,
    &outlineBishopBitmap, &outlineQueenBitmap, &outlineKingBitmap
  };

char pieceToChar[] = {
    'P', 'R', 'N', 'B', 'Q', 'K',
    'p', 'r', 'n', 'b', 'q', 'k', '.'
  };

XrmOptionDescRec shellOptions[] = {
    { "-movesPerSession", "movesPerSession", XrmoptionSepArg, NULL },
    { "-mps", "movesPerSession", XrmoptionSepArg, NULL },
    { "-internetChessServerMode", "internetChessServerMode",
	XrmoptionSepArg, NULL },
    { "-ics", "internetChessServerMode", XrmoptionSepArg, NULL },
    { "-internetChessServerPort", "internetChessServerPort",
	XrmoptionSepArg, NULL },
    { "-icsport", "internetChessServerPort", XrmoptionSepArg, NULL },
    { "-internetChessServerHost", "internetChessServerHost",
	XrmoptionSepArg, NULL },
    { "-icshost", "internetChessServerHost", XrmoptionSepArg, NULL },
    { "-firstChessProgram", "firstChessProgram", XrmoptionSepArg, NULL },
    { "-fcp", "firstChessProgram", XrmoptionSepArg, NULL },
    { "-secondChessProgram", "secondChessProgram", XrmoptionSepArg, NULL },
    { "-scp", "secondChessProgram", XrmoptionSepArg, NULL },
    { "-noChessProgram", "noChessProgram", XrmoptionSepArg, NULL },
    { "-ncp", "noChessProgram", XrmoptionSepArg, NULL },
    { "-firstHost", "firstHost", XrmoptionSepArg, NULL },
    { "-fh", "firstHost", XrmoptionSepArg, NULL },
    { "-secondHost", "secondHost", XrmoptionSepArg, NULL },
    { "-sh", "secondHost", XrmoptionSepArg, NULL },
    { "-remoteShell", "remoteShell", XrmoptionSepArg, NULL },
    { "-rsh", "remoteShell", XrmoptionSepArg, NULL },
    { "-timeDelay", "timeDelay", XrmoptionSepArg, NULL },
    { "-td", "timeDelay", XrmoptionSepArg, NULL },
    { "-timeControl", "timeControl", XrmoptionSepArg, NULL },
    { "-tc", "timeControl", XrmoptionSepArg, NULL },
    { "-loadGameFile", "loadGameFile", XrmoptionSepArg, NULL },
    { "-lgf", "loadGameFile", XrmoptionSepArg, NULL },
    { "-saveGameFile", "saveGameFile", XrmoptionSepArg, NULL },
    { "-sgf", "saveGameFile", XrmoptionSepArg, NULL },
    { "-autoSaveGames", "autoSaveGames", XrmoptionSepArg, NULL },
    { "-autosave", "autoSaveGames", XrmoptionSepArg, NULL },
    { "-loadPositionFile", "loadPositionFile", XrmoptionSepArg, NULL },
    { "-lpf", "loadPositionFile", XrmoptionSepArg, NULL },
    { "-savePositionFile", "savePositionFile", XrmoptionSepArg, NULL },
    { "-spf", "savePositionFile", XrmoptionSepArg, NULL },
    { "-matchMode", "matchMode", XrmoptionSepArg, NULL },
    { "-mm", "matchMode", XrmoptionSepArg, NULL },
    { "-monoMode", "monoMode", XrmoptionSepArg, NULL },
    { "-mono", "monoMode", XrmoptionSepArg, NULL },
    { "-debugMode", "debugMode", XrmoptionSepArg, NULL },
    { "-debug", "debugMode", XrmoptionSepArg, NULL },
    { "-clockMode", "clockMode", XrmoptionSepArg, NULL },
    { "-clock", "clockMode", XrmoptionSepArg, NULL },
    { "-boardSize", "boardSize", XrmoptionSepArg, NULL },
    { "-size", "boardSize", XrmoptionSepArg, NULL },
    { "-searchTime", "searchTime", XrmoptionSepArg, NULL },
    { "-st", "searchTime", XrmoptionSepArg, NULL },
    { "-searchDepth", "searchDepth", XrmoptionSepArg, NULL },
    { "-sd", "searchDepth", XrmoptionSepArg, NULL },
    { "-showCoords", "showCoords", XrmoptionSepArg, NULL },
    { "-coords", "showCoords", XrmoptionSepArg, NULL },
    { "-bell", "ringBellAfterMoves", XrmoptionSepArg, NULL },
    { "-ringBellAfterMoves", "ringBellAfterMoves", XrmoptionSepArg, NULL },
    { "-iconic", "Iconic", XrmoptionNoArg, "True" }
};

XtActionsRec boardActions[] = {
    { "DrawPosition", DrawPosition },
    { "HandleUserMove", HandleUserMove },
    { "ResetProc", ResetProc },
    { "LoadGameProc", LoadGameProc },
    { "QuitProc", QuitProc },
    { "ForwardProc", ForwardProc },
    { "BackwardProc", BackwardProc },
    { "PauseProc", PauseProc },
    { "Iconify", Iconify },
    { "FileNameAction", FileNameAction },
    { "PieceMenuPopup", PieceMenuPopup },
    { "SetWhiteToPlay", SetWhiteToPlay },
    { "SetBlackToPlay", SetBlackToPlay }
};
     
char translationsTable[] =
  "<Expose>: DrawPosition() \n \
   <Btn1Down>: HandleUserMove() \n \
   <Btn1Up>: HandleUserMove() \n \
   <Btn2Down>: XawPositionSimpleMenu(menuW) PieceMenuPopup(menuW) \n \
   <Btn3Down>: XawPositionSimpleMenu(menuB) PieceMenuPopup(menuB) \n \
   <Key>r: ResetProc() \n \
   <Key>R: ResetProc() \n \
   <Key>g: LoadGameProc() \n \
   <Key>G: LoadGameProc() \n \
   <Key>q: QuitProc() \n \
   <Key>Q: QuitProc() \n \
   <Message>WM_PROTOCOLS: QuitProc() \n \
   <Key>f: ForwardProc() \n \
   <Key>F: ForwardProc() \n \
   <Key>b: BackwardProc() \n \
   <Key>B: BackwardProc() \n \
   <Key>p: PauseProc() \n \
   <Key>P: PauseProc() \n \
   <Key>i: Iconify() \n \
   <Key>I: Iconify() \n \
   <Key>c: Iconify() \n \
   <Key>C: Iconify() \n";
     
char whiteTranslations[] = "<BtnDown>: SetWhiteToPlay()\n";
char blackTranslations[] = "<BtnDown>: SetBlackToPlay()\n";
     
String xboardResources[] = {
    DEFAULT_FONT,
    "*Dialog*value.translations: #override \\n <Key>Return: FileNameAction()",
    NULL
  };
     
void main(argc, argv)
     int argc;
     char **argv;
{
    int ok, i, mainFontPxlSize, coordFontPxlSize;
    int min, sec, matched;
    XSetWindowAttributes window_attributes;
    char buf[MSG_SIZ];
    Arg args[10];
    Dimension timerWidth, boardWidth, commandsWidth, w, h;
    XFontStruct *labelFontStruct;
    
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);
    
    programName = strrchr(argv[0], '/');
    if (programName == NULL)
      programName = argv[0];
    else
      programName++;
    
    shellWidget =
      XtAppInitialize(&appContext, "XBoard", shellOptions,
		      XtNumber(shellOptions), &argc, argv,
		      xboardResources, NULL, 0);
    if (argc > 1)
      Usage();
    
    if ((chessDir = (char *) getenv("CHESSDIR")) == NULL) {
	chessDir = ".";
    } else {
	if (chdir(chessDir) != 0) {
	    fprintf(stderr, "%s: can't cd to CHESSDIR: ", programName);
	    perror(chessDir);
	    exit(1);
	}
    }
    
    XtGetApplicationResources(shellWidget, &appData, clientResources,
			      XtNumber(clientResources), NULL, 0);
    
    /*
     * Determine matchMode state -- poor man's resource converter
     */
    if (StrCaseCmp(appData.matchMode, "Init") == 0)
      matchMode = MatchInit;
    else if (StrCaseCmp(appData.matchMode, "Position") == 0)
      matchMode = MatchPosition;
    else if (StrCaseCmp(appData.matchMode, "Opening") == 0)
      matchMode = MatchOpening;
    else if (StrCaseCmp(appData.matchMode, "False") == 0)
      matchMode = MatchFalse;
    else {
	fprintf(stderr, "%s: bad matchMode option %s\n",
		programName, appData.matchMode);
	Usage();
    }
    
    /*
     * Parse internet chess server status
     */
    if (appData.icsActive) {
	ok = establish(appData.icsHost,
		       (unsigned short)appData.icsPort);
	if (ok == -1) {
	    fprintf(stderr,
		    "%s: could not connect to host %s, port %d: ",
		    programName, appData.icsHost, appData.icsPort);
	    perror("");
	    exit(1);
	}
	appData.noChessProgram = True;
	buttonStrings = icsButtonStrings;
	buttonProcs = icsButtonProcs;
	buttonCount = XtNumber(icsButtonStrings);
    } else {
	buttonStrings = gnuButtonStrings;
	buttonProcs = gnuButtonProcs;
	buttonCount = XtNumber(gnuButtonStrings);
    }
    
    /*
     * Parse timeControl resource
     */
    matched = sscanf(appData.timeControl, "%d:%d", &min, &sec);
    if (matched == 1) {
	timeControl = min * 60 * 1000;
    } else if (matched == 2) {
	timeControl = (min * 60 + sec) * 1000;
    } else {
	fprintf(stderr, "%s: bad timeControl option %s\n",
		programName, appData.timeControl);
	Usage();
    }
    if (appData.icsActive) timeControl = 0;
    
    /*
     * Parse searchTime resource
     */
    if (*appData.searchTime != NULLCHAR) {
	matched = sscanf(appData.searchTime, "%d:%d", &min, &sec);
	if (matched == 1) {
	    searchTime = min * 60;
	} else if (matched == 2) {
	    searchTime = min * 60 + sec;
	} else {
	    fprintf(stderr, "%s: bad searchTime option %s\n",
		    programName, appData.searchTime);
	    Usage();
	}
    }
    
    /*
     * Determine boardSize
     */
    if (StrCaseCmp(appData.boardSize, "Large") == 0)
      boardSize = Large;
    else if (StrCaseCmp(appData.boardSize, "Medium") == 0)
      boardSize = Medium;
    else if (StrCaseCmp(appData.boardSize, "Small") == 0)
      boardSize = Small;
    else {
	fprintf(stderr, "%s: bad boardSize option %s\n",
		programName, appData.boardSize);
	Usage();
    }
    xDisplay = XtDisplay(shellWidget);
    xScreen = DefaultScreen(xDisplay);
    if (((DisplayWidth(xDisplay, xScreen) < 800) ||
	 (DisplayHeight(xDisplay, xScreen) < 800))
	&& (boardSize == Large)) {
	boardSize = Medium;
    }
    switch (boardSize) {
      case Small:
	squareSize = SMALL_SQUARE_SIZE;
	mainFontPxlSize = 11;
	coordFontPxlSize = 10;
	break;
      case Medium:
	squareSize = MEDIUM_SQUARE_SIZE;
	mainFontPxlSize = 17;
	coordFontPxlSize = 12;
	break;
      case Large:
	squareSize = LARGE_SQUARE_SIZE;
	mainFontPxlSize = 17;
	coordFontPxlSize = 14;
	break;
    }
    boardWidth = LINE_GAP + BOARD_SIZE * (squareSize + LINE_GAP);
    XtSetArg(boardArgs[1], XtNwidth, boardWidth);
    XtSetArg(boardArgs[2], XtNheight,
	     LINE_GAP + BOARD_SIZE * (squareSize + LINE_GAP));
    
    /*
     * Determine what fonts to use.
     */
    appData.mainFont = FindFont(appData.mainFont, mainFontPxlSize);
    mainFontID = XLoadFont(xDisplay, appData.mainFont);
    mainFontStruct = XQueryFont(xDisplay, mainFontID);
    appData.coordFont = FindFont(appData.coordFont, coordFontPxlSize);
    coordFontID = XLoadFont(xDisplay, appData.coordFont);
    coordFontStruct = XQueryFont(xDisplay, coordFontID);

    if ((*appData.searchTime != NULLCHAR) || (appData.searchDepth > 0)
	|| appData.noChessProgram)
      appData.clockMode = False;
    if (appData.icsActive) appData.clockMode = True;
    
    /*
     * Detect if there are not enough colors are available and adapt.
     */
    if (DefaultDepth(xDisplay, xScreen) <= 2)
      appData.monoMode = True;
    
    /*
     * widget hierarchy
     */
    formWidget =
      XtCreateManagedWidget("form", formWidgetClass, shellWidget, NULL, 0);
    
    widgetList[0] = whiteTimerWidget =
      XtCreateWidget("white time:", labelWidgetClass,
		     formWidget, timerArgs, XtNumber(timerArgs));
    XtSetArg(args[0], XtNfont, mainFontStruct);
    XtSetValues(whiteTimerWidget, args, 1);
    
    widgetList[1] = blackTimerWidget =
      XtCreateWidget("black time:", labelWidgetClass,
		     formWidget, timerArgs, XtNumber(timerArgs));
    XtSetArg(args[0], XtNfont, mainFontStruct);
    XtSetValues(blackTimerWidget, args, 1);
    
    widgetList[2] = titleWidget =
      XtCreateWidget("", labelWidgetClass,
		     formWidget, titleArgs, XtNumber(titleArgs));
    XtSetArg(args[0], XtNfont, mainFontStruct);
    XtSetValues(titleWidget, args, 1);
    
    widgetList[3] = messageWidget =
      XtCreateWidget("message", labelWidgetClass, formWidget,
		     messageArgs, XtNumber(messageArgs));
    XtSetArg(args[0], XtNfont, mainFontStruct);
    XtSetValues(messageWidget, args, 1);
    
    i = 0;
    XtSetArg(args[i], XtNborderWidth, 0);                  i++;
    XtSetArg(args[i], XtNdefaultColumns, 4);               i++;
    XtSetArg(args[i], XtNforceColumns, True);              i++;
    XtSetArg(args[i], XtNcolumnSpacing, 12);               i++;
    XtSetArg(args[i], XtNlist, (XtArgVal) buttonStrings);  i++;
    XtSetArg(args[i], XtNnumberStrings, buttonCount);      i++;
    XtSetArg(args[i], XtNfont, mainFontStruct);            i++;
    widgetList[4] = commandsWidget =
      XtCreateWidget("commands", listWidgetClass, formWidget, args, i);
    
    widgetList[5] = boardWidget =
      XtCreateWidget("board", widgetClass, formWidget, boardArgs,
		     XtNumber(boardArgs));
    
    XtManageChildren(widgetList, XtNumber(widgetList));
    
    /*
     * Calculate the width of the timer labels.
     */
    XtSetArg(args[0], XtNfont, &labelFontStruct);
    XtGetValues(whiteTimerWidget, args, 1);
    if (appData.clockMode) {
	if (timeControl == 0) 
	  sprintf(buf, "White: %s ", TimeString(2 * 60 * 60 * 1000));
	else
	  sprintf(buf, "White: %s ", TimeString(timeControl));
	timerWidth = XTextWidth(labelFontStruct, buf, strlen(buf) - 1);
    } else {
	timerWidth = XTextWidth(labelFontStruct, "White  ", 7);
    }
    XtSetArg(args[0], XtNwidth, timerWidth);
    XtSetValues(whiteTimerWidget, args, 1);
    XtSetValues(blackTimerWidget, args, 1);
    
    XtSetArg(args[0], XtNbackground, &timerForegroundPixel);
    XtSetArg(args[1], XtNforeground, &timerBackgroundPixel);
    XtGetValues(whiteTimerWidget, args, 2);
    
    /*
     * Calculate the width of the title and message labels.
     */
    XtSetArg(args[0], XtNwidth, &commandsWidth);
    XtGetValues(commandsWidget, args, 1);
    w = (commandsWidth > boardWidth) ? commandsWidth : boardWidth;
    XtSetArg(args[0], XtNwidth, w - timerWidth*2 - 12);
    XtSetValues(titleWidget, args, 1);
    XtSetArg(args[0], XtNwidth, w - 8);
    XtSetValues(messageWidget, args, 1);
    
    /*
     * formWidget uses these constraints but they are stored
     * in the children.
     */
    XtSetArg(args[0], XtNfromHoriz, whiteTimerWidget);
    XtSetValues(blackTimerWidget, args, 1);
    XtSetArg(args[0], XtNfromHoriz, blackTimerWidget);
    XtSetValues(titleWidget, args, 1);
    XtSetArg(args[0], XtNfromVert, whiteTimerWidget);
    XtSetValues(messageWidget, args, 1);
    XtSetArg(args[0], XtNfromVert, messageWidget);
    XtSetValues(commandsWidget, args, 1);
    XtSetArg(args[0], XtNfromVert, commandsWidget);
    XtSetValues(boardWidget, args, 1);
    
    if (appData.icsActive) {
	XtAppAddInput(appContext, ics_input,
		      (XtPointer) (XtInputExceptMask|XtInputReadMask),
		      (XtInputCallbackProc) read_from_ics,
		      (XtPointer) NULL);
	XtAppAddInput(appContext, fileno(stdin),
		      (XtPointer) XtInputReadMask,
		      (XtInputCallbackProc) read_from_player,
		      (XtPointer) NULL);
    } 
    
    XtRealizeWidget(shellWidget);
    
    xBoardWindow = XtWindow(boardWidget);
    
    /*
     * Create an icon.
     */
    iconPixmap = XCreateBitmapFromData(xDisplay, XtWindow(shellWidget),
				       icon_bits, icon_width, icon_height);
    XtSetArg(args[0], XtNiconPixmap, iconPixmap);
    XtSetValues(shellWidget, args, 1);
    
    /*
     * Create a cursor for the board widget.
     */
    window_attributes.cursor = XCreateFontCursor(xDisplay, XC_hand2);
    XChangeWindowAttributes(xDisplay, xBoardWindow,
			    CWCursor, &window_attributes);
    
    /*
     * Inhibit shell resizing.
     */
    shellArgs[0].value = (XtArgVal) &w;
    shellArgs[1].value = (XtArgVal) &h;
    XtGetValues(shellWidget, shellArgs, 2);
    shellArgs[4].value = shellArgs[2].value = w;
    shellArgs[5].value = shellArgs[3].value = h;
    XtSetValues(shellWidget, &shellArgs[2], 4);
    
    CreateGCs();
    CreateGrid();
    CreatePieces();
    CreatePieceMenus();
    
    XtAddCallback(commandsWidget, XtNcallback, SelectCommand, NULL);
    XtAppAddActions(appContext, boardActions, XtNumber(boardActions));
    
    XtSetArg(args[0], XtNtranslations,
	     XtParseTranslationTable(translationsTable));
    XtSetValues(boardWidget, &args[0], 1);
    XtSetArg(args[0], XtNtranslations,
	     XtParseTranslationTable(whiteTranslations));
    XtSetValues(whiteTimerWidget, &args[0], 1);
    XtSetArg(args[0], XtNtranslations,
	     XtParseTranslationTable(blackTranslations));
    XtSetValues(blackTimerWidget, &args[0], 1);
    
    XtAddEventHandler(boardWidget, ExposureMask | ButtonPressMask
		      | ButtonReleaseMask | Button1MotionMask | KeyPressMask,
		      False, (XtEventHandler) EventProc, NULL);
    
    sprintf(buf, "xboard version %s, patchlevel %d", VERSION, PATCHLEVEL);
    
    /*
     * If there is to be a machine match, set it up.
     */
    if (matchMode != MatchFalse){
	if (appData.noChessProgram) {
	    fprintf(stderr,
		    "%s: can't have a match with no chess programs!\n",
		    programName);
	    exit(1);
	}
	DisplayMessage(buf);
	TwoMachinesProc(NULL, NULL, NULL, NULL);
    }
    else {
	ResetProc(NULL, NULL, NULL, NULL);
	DisplayMessage(buf);
	if (*appData.loadGameFile != NULLCHAR)
	  LoadGame(appData.loadGameFile);
	else if (*appData.loadPositionFile != NULLCHAR)
	  LoadPosition(appData.loadPositionFile);
    }
    
    XtAppMainLoop(appContext);
}

/*
 * Establish will establish a contact to a remote host.port.
 * Returns 0 if okay, -1 if not.
 */
int establish(host, port)
     char *host;
     int port;
{
    int s;
    char str[100];
    int to_prog[2], from_prog[2];
    struct sockaddr_in sa;
    struct hostent     *hp;
    unsigned short uport;
    
    if (appData.useTelnet || (*appData.gateway != NULLCHAR)) {
	pipe(to_prog);
	pipe(from_prog);
	if ((telnetPID = fork()) == 0) {
	    dup2(to_prog[0], 0);
	    dup2(from_prog[1], 1);
	    close(to_prog[0]);
	    close(to_prog[1]);
	    close(from_prog[0]);
	    close(from_prog[1]);
	    dup2(1, fileno(stderr)); /* force stderr to the pipe */
	    
	    uport = (unsigned short) port;
	    sprintf(str, "%d", uport);
	    if (*appData.gateway != NULLCHAR) {
		execlp(appData.remoteShell, appData.remoteShell,
		       appData.gateway, "telnet",
		       host, str, (char *) NULL);
	    } else {
		execlp("telnet", "telnet",
		       host, str, (char *) NULL);
	    }
	    perror("telnet");
	    exit(1);
	}
	close(to_prog[0]);
	close(from_prog[1]);
	ics_input = from_prog[0];
	ics_output = to_prog[1];
    } else {
	bzero((char *) &sa, sizeof(struct sockaddr_in));
	if (!(hp = gethostbyname(host))) {
	    fprintf(stderr, "%s: could not gethostbyname %s\n",
		    programName, host);
	    return(-1);
	}
	sa.sin_family = hp->h_addrtype;
	uport = (unsigned short) port;
	sa.sin_port = htons(uport);
	
	bcopy(hp->h_addr, (char *) &sa.sin_addr, hp->h_length);
	
	if ((s = socket(AF_INET, SOCK_STREAM, 6)) < 0) {
	    fprintf(stderr, "%s: could not get socket\n", programName);
	    return(-1);
	}
	if (connect(s, (struct sockaddr *) &sa, 
	            sizeof(struct sockaddr_in)) < 0) {
	    fprintf(stderr, "%s: could not bind socket\n", programName);
	    return(-1);
	}
	ics_input = ics_output = s;
    }
    return(0);
}

void read_from_player(client_data, file_num, id)
     caddr_t client_data;
     int *file_num;
     XtInputId *id;
{
#define BUF_SIZE 1024
    int s = *file_num;
    static char buf[BUF_SIZE];
    int buf_len, tmp_len;
    int out_len = 0;
    
    buf_len = read(s, buf, BUF_SIZE);
    
    if (buf_len > 0) {
	while (out_len < buf_len) {
	    tmp_len = write(ics_output, &buf[out_len], buf_len - out_len);
	    if (tmp_len == -1) {
		fprintf(stderr, "%s: error writing to ICS: ", programName);
		perror("");
		exit(1);
	    }
	    out_len += tmp_len;
	}
    } else {
	fprintf(stderr, "%s: got end of file from keyboard\n", programName);
	if (telnetPID != 0) {
	    if (kill(telnetPID, SIGTERM) == 0)
	      wait((union wait *) 0);
	}
	exit(0);
    }
}

void SendToICS(s)
     char *s;
{
    int i, j, tmp;
    
    i = strlen(s);
    j = 0;
    
    if (appData.debugMode)
      fprintf(stderr, "Sending to ICS: %s", s);

    while (j < i) {
	tmp = write(ics_output, &s[j], i - j);
	if (tmp == -1) {
	    fprintf(stderr, "%s: error writing to ICS: ", programName);
	    perror("");
	    exit(1);
	}
	j += tmp;
    }
}


static int leftover_start = 0, leftover_len = 0;
static char star_match[8][256];

/* Test whether pattern is present at &buf[*index]; if so, return True,
   advance *index beyond it, and set leftover_start to the new value of
   *index; else return False.  If pattern contains the character '*', it
   matches any sequence of characters not containing '\r', '\n', or the
   character following the '*', and the matched sequence(s) are copied
   into star_match.
*/
Boolean looking_at(buf, index, pattern)
     char *buf;
     int *index;
     char *pattern;
{
    char *bufp = &buf[*index], *patternp = pattern;
    int star_count = 0;
    char *matchp = star_match[0];
    
    for (;;) {
	if (*patternp == NULLCHAR) {
	    *index = leftover_start = bufp - buf;
	    *matchp = NULLCHAR;
	    return True;
	}
	if (*bufp == NULLCHAR) return False;
	if (*patternp == '*') {
	    if (*bufp == *(patternp + 1)) {
		*matchp = NULLCHAR;
		matchp = star_match[++star_count];
		patternp += 2;
		bufp++;
		continue;
	    } else if (*bufp == '\n' || *bufp == '\r') {
		return False;
	    } else {
		*matchp++ = *bufp++;
		continue;
	    }
	}
	if (*patternp != *bufp) return False;
	patternp++;
	bufp++;
    }
}


/*!! temporary; code to be stripped later*/
#define OLD_ICS 1
/*!!*/

void read_from_ics(client_data, file_num, id)
     caddr_t client_data;
     int *file_num;
     XtInputId *id;
{
#define BUF_SIZE 1024
#define BOARD 1
#define MOVES 2
    
    static int started = 0;
    static char parse[20000];
    static int  parse_pos;
    static char buf[BUF_SIZE + 1];
    
    char str[500];
    int i, oldi;
    int buf_len;
    int next_out;
    
    /* If last read ended with a partial line that we couldn't parse,
       prepend it to the new read and try again. */
    if (leftover_len > 0) {
	for (i=0; i<leftover_len; i++)
	  buf[i] = buf[leftover_start + i];
    }
    buf_len = read(ics_input, &buf[leftover_len], BUF_SIZE - leftover_len);
    next_out = leftover_len;
    
    if (buf_len > 0) {
	buf_len += leftover_len;
	leftover_start = 0;
	buf[buf_len] = NULLCHAR;
	
	i = 0;
	while (i < buf_len) {
	    
	    if ((looking_at(buf, &i,
			    "a   b   c   d   e   f   g   h") || 
		 looking_at(buf, &i,
			    "h   g   f   e   d   c   b   a"))) {
		/* End of board style 1 */
		SendToICS("style 8\n");
    	        SendToICS("refresh\n");
		continue;
	    }
	    
	    oldi = i;
	    if (looking_at(buf, &i, "#@#")) {
		started = BOARD;
		parse_pos = 0;
		fwrite(&buf[next_out], oldi - next_out, 1, stdout);
		continue;
	    }
	    
	    if (started == BOARD && looking_at(buf, &i, "@#@")) {
		/* Board read is done */
		started = 0;
		next_out = i;
		parse[parse_pos] = NULLCHAR;
		
		ParseBoard8(parse);
		
		/* Display the board */
		if (gameMode != PauseGame) {
		    DrawPosition(boardWidget, NULL, NULL, NULL);
		    DisplayMove(currentMove - 1);
		    if (appData.ringBellAfterMoves &&
			!ics_user_moved)
		      putc(BELLCHAR, stderr);
		}
		DisplayClocks(StartTimers);
		ics_user_moved = 0;
		continue;
	    }
	    
	    oldi = i;
	    if (ics_getting_history &&
		looking_at(buf, &i, "Move")) {
		/* Beginning of move list */
		/* !!maybe recognize something more specific later */
		started = MOVES;
		parse_pos = 0;
		fwrite(&buf[next_out], oldi - next_out, 1, stdout);
		continue;
	    }				
	    
	    if(looking_at(buf, &i, "% ")) {
		switch (started) {
		  case 0:
		    continue;
		  case BOARD:
		    /* Something went wrong; found a prompt while
		       accumulating a board */
		    started = 0;
		    fprintf(stderr, "%s: error gathering board\n",
			    programName);
		    continue;
		  case MOVES:
		    started = 0;
		    parse[parse_pos] = NULLCHAR;
		    if (ics_getting_history) {
			ParseGameHistory(parse);
			ics_getting_history = False;
		    }
		    next_out = i;
		    continue;
		}
	    }
	    
	    if (started && i >= leftover_len) {
		/* Accumulate characters in board
		   or move list*/
		if (buf[i] != '\r')
		  parse[parse_pos++] = buf[i];
	    }
	    
	    if (looking_at(buf, &i, "match * * requested with *.")) {
		/* Remember this offer for AcceptMatch button */
		sprintf(ics_match_offer, "match %s %s %s\n",
			star_match[2], star_match[0], star_match[1]);
		continue;
	    }

	    if (looking_at(buf, &i, "* has made an alternate *match * *.")) {
		/* Remember this offer for AcceptMatch button */
		sprintf(ics_match_offer, "match %s %s %s\n",
			star_match[0], star_match[2], star_match[3]);
		continue;
	    }

	    /* Start of game messages.  Mostly we detect start of game
	       when the first board image arrives, but we need to prime
	       the pump for games we're just observing. */

	    if (looking_at(buf, &i, "Adding game * to observation list")) {
		sprintf(str, "refresh %d\n", atoi(star_match[0]));
		SendToICS(str);
		continue;
	    }
	    
	    if (ics_user_moved) {
		if (looking_at(buf, &i, "No such command") ||
		    looking_at(buf, &i, "Your king is in check") ||
#ifdef OLD_ICS
		    looking_at(buf, &i, "Unknown") || 
#endif 
		    looking_at(buf, &i, "Illegal move") ||
		    looking_at(buf, &i, "It isn't your turn")) {
		    /**** Illegal move ****/
		    ics_user_moved = 0;
		    if (forwardMostMove > backwardMostMove) {
			currentMove = --forwardMostMove;
			DisplayMessage("Invalid move");
			DrawPosition(boardWidget, NULL, NULL, NULL);
			DisplayClocks(SwitchTimers);
		    }
		    continue;
		}
	    }
	    
	    /* Various end-of-game messages */
	    /* Hope they are all here and correct! */
	    
	    if (looking_at(buf, &i, "{Game * (* vs. *)* * *}")) {
		/* New style generic game ending messages */
		/* star_match[0] is the game number */
		/*           [1] is the white player's name */
		/*           [2] is the black player's name */
		/*           [3] is either ":" or empty (don't care) */
		/*           [4] is either the loser's name or a noise word */
		/*           [5] contains the reason for the game end */
		int gamenum = atoi(star_match[0]);
		char *white = star_match[1];
		char *loser = star_match[4];
		char *why = star_match[5];
		
		if (ics_gamenum != gamenum) continue;

		if (StrStr(why, "checkmate")) {
		    if (strcmp(loser, white) == 0)
		      GameEnds("Black mates");
		    else
		      GameEnds("White mates");
		} else if (StrStr(why, "resigns")) {
		    if (strcmp(loser, white) == 0)
		      GameEnds("White resigns");
		    else
		      GameEnds("Black resigns");
		} else if (StrStr(why, "forfeits on time")) {
		    if (strcmp(loser, white) == 0)
		      GameEnds("Black wins on time");
		    else
		      GameEnds("White wins on time");
		} else if (StrStr(why, "stalemate")) {
		    GameEnds("Stalemate");
		} else if (StrStr(why, "drawn by mutual agreement")) {
		    GameEnds("Draw agreed");
		} else if (StrStr(why, "drawn by repetition")) {
		    GameEnds("Draw by repetition");
		} else if (StrStr(why, "50")) {
		    GameEnds("Draw (50 move rule)");
		} else if (StrStr(why, "neither player has mating")) {
		    GameEnds("Draw (insufficient material)");
		} else if (StrStr(why, "no material")) {
		    GameEnds("Draw (insufficient material to win on time)");
		} else if (StrStr(why, "time")) {
		    GameEnds("Draw (both players ran out of time)");
		} else if (StrStr(why, "aborted")) {
		    DisplayClocks(StopTimers);
		    DisplayMessage("Game aborted");
		    ics_mode = IcsIdle;
		    ics_gamenum = -1;
		} else if (StrStr(why, "adjourn")) {
		    DisplayClocks(StopTimers);
		    DisplayMessage("Game adjourned");
		    ics_mode = IcsIdle;
		    ics_gamenum = -1;
		}   
		continue;
	    }

	    if (looking_at(buf, &i, "Removing game * from observation list")) {
		if (ics_mode == IcsObserving &&
		    atoi(star_match[0]) == ics_gamenum)
		  {
		      DisplayClocks(StopTimers);
		      ics_mode = IcsIdle;
		      ics_gamenum = -1;
		  }
		continue;
	    }
	    
#ifdef OLD_ICS
	    if (looking_at(buf, &i,
			   "You have been checkmated")) {
		switch (ics_mode) {
		  case IcsPlayingWhite:
		    GameEnds("Black mates");
		    break;
		  case IcsPlayingBlack:
		    GameEnds("White mates");
		    break;
		  default:
		    break;
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Your opponent (*) has been checkmated")) {
		switch (ics_mode) {
		  case IcsPlayingWhite:
		    GameEnds("White mates");
		    break;
		  case IcsPlayingBlack:
		    GameEnds("Black mates");
		    break;
		  default:
		    break;
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "White (*) checkmated")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_white, star_match[0]) == 0) {
		    GameEnds("Black mates");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Black (*) checkmated")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_black, star_match[0]) == 0) {
		    GameEnds("White mates");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "You resign the game")) {
		switch (ics_mode) {
		  case IcsPlayingWhite:
		    GameEnds("White resigns");
		    break;
		  case IcsPlayingBlack:
		    GameEnds("Black resigns");
		    break;
		  default:
		    break;
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Your opponent (*) has resigned")) {
		switch (ics_mode) {
		  case IcsPlayingWhite:
		    GameEnds("Black resigns");
		    break;
		  case IcsPlayingBlack:
		    GameEnds("White resigns");
		    break;
		  default:
		    break;
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "White (*) resigns")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_white, star_match[0]) == 0) {
		    GameEnds("White resigns");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Black (*) resigns")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_black, star_match[0]) == 0) {
		    GameEnds("Black resigns");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "You have run out of time")) {
		switch (ics_mode) {
		  case IcsPlayingWhite:
		    GameEnds("Black wins on time");
		    break;
		  case IcsPlayingBlack:
		    GameEnds("White wins on time");
		    break;
		  default:
		    break;
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Your opponent (*) has run out of time")) {
		switch (ics_mode) {
		  case IcsPlayingWhite:
		    GameEnds("White wins on time");
		    break;
		  case IcsPlayingBlack:
		    GameEnds("Black wins on time");
		    break;
		  default:
		    break;
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "White (*) forfeits on time")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_white, star_match[0]) == 0) {
		    GameEnds("Black wins on time");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Black (*) forfeits on time")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_black, star_match[0]) == 0) {
		    GameEnds("White wins on time");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Game : * vs. * : The game is drawn by repetition")
		) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_white, star_match[0]) == 0) {
		    GameEnds("Draw by repetition");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "The game is drawn by repetition")) {
		/* Your own game -- not prefixed by
		   "Game : * vs. * : "*/
		GameEnds("Draw by repetition");
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Game : * vs. * : Game is drawn by mutual ")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_white, star_match[0]) == 0) {
		    GameEnds("Draw agreed");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Game is drawn by mutual agreement")) {
		/* Your own game -- not prefixed by
		   "Game : * vs. * : "*/
		GameEnds("Draw agreed");
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Game : * vs. * : * has been stalemated")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_white, star_match[0]) == 0) {
		    GameEnds("Stalemate");
		}
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "has been stalemated")) {
		/* Your own game -- not prefixed by
		   "Game : * vs. * : "*/
		GameEnds("Stalemate");
		continue;
	    }
	    
	    if (looking_at(buf, &i,
			   "Game : * vs. * : Draw: neither player has")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_white, star_match[0]) == 0) {
		    GameEnds("Draw (insufficient material)");
		}
		continue;
	    }

	    if (looking_at(buf, &i,
			   "Draw: neither player has")) {
		/* Your own game -- not prefixed by
		   "Game : * vs. * : "*/
		GameEnds("Draw (insufficient material)");
		continue;
	    }

	    if (looking_at(buf, &i,
			   "Game : * vs. * : Draw: * has no material,")) {
		if (ics_mode == IcsObserving &&
		    strcmp(ics_white, star_match[0]) == 0) {
		    GameEnds("Draw (insufficient material for time forfeit)");
		}
		continue;
	    }

	    if (looking_at(buf, &i,
			   "Draw: * has no material,")) {
		/* Your own game -- not prefixed by
		   "Game : * vs. * : "*/
		GameEnds("Draw (insufficient material for time forfeit)");
		continue;
	    }
#endif

#ifdef OLD_ICS /*!! is this message changing?*/
	    if (looking_at(buf, &i, "Game has been adjourned") ||
	        looking_at(buf, &i, "Game has been saved and adjourned") ||
	        looking_at(buf, &i, "game has been saved for completion")) {
		DisplayClocks(StopTimers);
		DisplayMessage("Game adjourned");
		ics_mode = IcsIdle;
		ics_gamenum = -1;
		continue;
	    }
#endif
	    
	    /*!! new server only?  temporary format? */
	    if ((ics_mode == IcsPlayingWhite || ics_mode == IcsPlayingBlack) &&
		looking_at(buf, &i, "Your opponent has lost contact")) {
		DisplayClocks(StopTimers);
		DisplayMessage("Game aborted");
		ics_mode = IcsIdle;
		ics_gamenum = -1;
		continue;
	    }

#ifdef OLD_ICS /*!!?? needed only due to bug*/
	    if (ics_mode == IcsObserving &&
		looking_at(buf, &i,
			   "[* has disconnected]")) {
		if (strcmp(ics_white, star_match[0]) == 0 ||
		    strcmp(ics_black, star_match[0]) == 0) {
		    DisplayClocks(StopTimers);
		    DisplayMessage("Game adjourned");  /*!!or aborted?*/
		    ics_mode = IcsIdle;
		    ics_gamenum = -1;
		}
		continue;
	    }
#endif

	    /* Advance leftover_start past any newlines we find,
	       so only partial lines can get reparsed */
	    if (looking_at(buf, &i, "\n")) continue;
	    if (looking_at(buf, &i, "\r")) continue;
	    
	    i++;	/* skip unparsed character and loop back */
	}
	
	if (started == 0)
	  fwrite(&buf[next_out], i - next_out, 1, stdout);
	
	leftover_len = buf_len - leftover_start;
	/* if buffer ends with something we couldn't parse,
	   reparse it after appending the next read */
	
    } else if (buf_len == 0) {
	fprintf(stderr, "%s: connection closed by ICS\n", programName);
	if (telnetPID != 0) {
	    if (kill(telnetPID, SIGTERM) == 0)
	      wait((union wait *) 0);
	}
	exit(0);
    } else {
	fprintf(stderr, "%s: error reading from ICS: ", programName);
	perror("");
	exit(1);
    }
}

/*
  ICS board style 8 looks like this:
  
  #@#000observer        :aaa             :RNBQKBNRPPPPPPPP                                pppppppprnbqkbnr001W39390360003600@#@
  
  Information offsets, descriptions and lengths:
  +3   Game # (3)
  +6   White's name (16 + ':' = 17)
  +23  Black's name (16 + ':' = 17)
  +40  Board  (64)
  +104 Move # (3)
  +107 Whose move (1)
  +108 White Strength (2)
  +110 Black Strength (2)
  +112 White Time (5)
  +117 Black Time (5)
  +122 Move string (variable
  A "*" instead of a ":" after the name implies that the person using xboard
    is playing the game.
  The move string is either empty or consists of a move followed by
    elapsed time in parentheses.
  The pattern defined below doesn't include the #@# and @#@ brackets,
    and it assumes the board string is null-terminated.  ParseBoard8's
    caller takes care of this.
  */

#define PATTERN "%3d%16s %1c%16s %1c%64c%3d%1c%2d%2d%5d%5d%s %s"

void ParseBoard8(string)
     char *string;
{ 
    IcsMode new_ics_mode;
    int gamenum;
    int j, k, move_num, white_stren, black_stren, white_time, black_time;
    char playing_white, playing_black, to_play, board_chars[64];
    char move_str[500], str[500], elapsed_time[500];
    char black[32], white[32];
    Board board;
    
    if (appData.debugMode)
      fprintf(stderr, "Parsing board: %s\n", string);

    sscanf(string, PATTERN, &gamenum, white, &playing_white,
	   black, &playing_black, board_chars, &move_num, &to_play,
	   &white_stren, &black_stren, &white_time, &black_time,
	   move_str, elapsed_time);

    if (playing_white == '*')
      new_ics_mode = IcsPlayingWhite;
    else if (playing_black == '*')
      new_ics_mode = IcsPlayingBlack;
    else
      new_ics_mode = IcsObserving;
    
    /* Convert the move number to internal form */
    move_num = (move_num - 1) * 2;
    if (to_play == 'B') move_num++;

    /* Take action if this is the first board of a new game */
    if (gamenum != ics_gamenum) {
	if (ics_mode == IcsObserving) {
	    /* Error: xboard can't handle two games at once */
	    /* Stop observing the old game */
	    fprintf(stderr, "%s: Aren't you currently observing game %d",
		    programName, ics_gamenum);
	    fprintf(stderr, "?  Attempting to stop observing it.\n");
	    sprintf(str, "observe %d\n", ics_gamenum);
	    SendToICS(str);
	    /* continue as in normal case */
	} else if (ics_mode != IcsIdle) {
	    /* Error: xboard can't handle two games at once */
	    if (new_ics_mode == IcsObserving) {
		/* Stop observing the new game */
		fprintf(stderr, "%s: Aren't you playing a game now?  ",
			programName);
		fprintf(stderr, "Attempting to stop observing game %d.\n",
			gamenum);
		SendToICS("observe\n");
		/* ignore this board */
		return;
	    } else /*(new_ics_mode == IcsWhite || new_ics_mode == IcsBlack)*/ {
#ifdef OLD_ICS
		/* We can get here because the game number on the first
		   board can be wrong; minor ICS bug, to be fixed soon. */
		if (move_num == 1) goto all_is_well;
#endif
		/* Playing two games???  ICS supposedly can't do this. */
		fprintf(stderr, "%s: Yikes, are you playing two games now?  ",
			programName);
		fprintf(stderr, "If so, you've hit an ICS bug!\n");
		/* continue as in normal case, hoping old game is gone */
	    }
	}
	/* Normal case (ics_mode == IcsIdle), or error recovered */
	ResetProc(NULL, NULL, NULL, NULL);
	if (move_num > 0) {
	    /* Need to get game history */
	    ics_getting_history = True;
	    sprintf(str, "moves %d\n", gamenum);
	    SendToICS(str);
	}
    }

#ifdef OLD_ICS
  all_is_well:
#endif

    /* Initially flip the board to have black on the bottom iff playing
       black, but let the user change it with the Flip View button. */
    if (ics_mode == IcsIdle)
      flipView = (new_ics_mode == IcsPlayingBlack);

    /* Update known move number limits */
    if (ics_mode == IcsIdle)
      forwardMostMove = backwardMostMove = currentMove = move_num;
    if (move_num > forwardMostMove + 1 || move_num < forwardMostMove) {
	/* Different game or out of sequence positions (should not happen) */
	backwardMostMove = forwardMostMove = move_num;
    } else if (move_num > forwardMostMove) {
	forwardMostMove = move_num;
    }
    if (gameMode != PauseGame) 
      currentMove = move_num;

    /* Done with values from previous mode; copy in new ones */
    ics_mode = new_ics_mode;
    ics_gamenum = gamenum;
    strcpy(ics_white, white);
    strcpy(ics_black, black);

    /* Display opponents and material strengths */
    sprintf(str, "  %s (%d) vs. %s (%d)", 
	    ics_white, white_stren,
	    ics_black, black_stren);
    DisplayTitle(str);
    
    /* Parse the board */
    for (k = 0; k < 8; k++)
      for (j = 0; j < 8; j++)
	board[k][j] = CharToPiece(board_chars[k*8 + j]);
    CopyBoard(boards[move_num], board);
    
    /* Put the move on the move list, first converting
       to canonical algebraic form. */
    if (move_num > 0) {
	ChessMove move_type;
	int from_x, from_y, to_x, to_y;
	char promo_char;

	if (move_num - 1 < backwardMostMove) {
	    /* We don't know what the board looked like before
	       this move.  Punt. */
	    strcpy(parseList[move_num - 1], move_str);
	} else {
	    move_type = yylexstr(move_num - 1, move_str, NULL);
	    promo_char = NULLCHAR;
	    switch (move_type) {
	      case WhiteKingSideCastle:
	      case WhiteQueenSideCastle:
	      case BlackKingSideCastle:
	      case BlackQueenSideCastle:
		CastleCoords(move_type, &from_x, &from_y, &to_x, &to_y);
		break;
	      case WhitePromotionQueen:
	      case BlackPromotionQueen:
	      case WhitePromotionRook:
	      case BlackPromotionRook:
	      case WhitePromotionBishop:
	      case BlackPromotionBishop:
	      case WhitePromotionKnight:
	      case BlackPromotionKnight:
		promo_char = ToLower(PieceToChar(PromoPiece(move_type)));
		/* fall thru */
	      case NormalMove:
	      case WhiteCapturesEnPassant:
	      case BlackCapturesEnPassant:
		from_x = ToLower(currentMoveString[0]) - 'a';
		from_y = ToLower(currentMoveString[1]) - '1';
		to_x = ToLower(currentMoveString[2]) - 'a';
		to_y = ToLower(currentMoveString[3]) - '1';
		break;
	      case BadMove:
	      case AmbiguousMove:
	      case 0:		/* end of file */
	      case ElapsedTime:
	      case Comment:
	      case StartGame:
	      case WhiteWins:
	      case BlackWins:
	      case GameIsDrawn:
	      default:
		/* bug? */
		fprintf(stderr, "%s: bad move in ICS output: %s\n",
			programName, move_str);
		return;
	    }
	    /* Work around ICS bug: pawn promotion is not indicated,
	       even if underpromoted.  Unfortunately there is no
	       workaround for the same bug when it bites us in
	       ParseGameHistory().  */
	
	    if (move_str[0] == 'P' && (to_y == 0 || to_y == 7))
	      promo_char = ToLower(PieceToChar(board[to_y][to_x]));

	    (void) CoordsToAlgebraic(from_x, from_y, to_x, to_y, promo_char,
				     move_num - 1, parseList[move_num - 1]);
	    strcat(parseList[move_num - 1], " ");
	    strcat(parseList[move_num - 1], elapsed_time);
	}
    }
    
    /* Update the clocks */
    whiteTimeRemaining = white_time * 1000;
    blackTimeRemaining = black_time * 1000;
}


#define abs(n) ((n)<0 ? -(n) : (n))

/*
 * Find a font that matches "pattern" that is as close as
 * possible to the targetPxlSize.  Prefer fonts that are k
 * pixels smaller to fonts that are k pixels larger.  The
 * pattern must be in the X Consortium standard format, 
 * e.g. "-*-helvetica-bold-r-normal--*-*-*-*-*-*-*-*".
 * The return value should be freed with XtFree when no
 * longer needed.
 */
char *FindFont(pattern, targetPxlSize)
     char *pattern;
     int targetPxlSize;
{
    char **fonts, *p, *best;
    int i, j, nfonts, minerr, err, pxlSize;

    fonts = XListFonts(xDisplay, pattern, 999999, &nfonts);
    if (nfonts < 1) {
	fprintf(stderr, "%s: No fonts match pattern %s\n",
		programName, pattern);
	exit(1);
    }
    best = "";
    minerr = 999999;
    for (i=0; i<nfonts; i++) {
	j = 0;
	p = fonts[i];
	if (*p != '-') continue;
	while (j < 7) {
	    if (*p == NULLCHAR) break;
	    if (*p++ == '-') j++;
	}
	if (j < 7) continue;
	pxlSize = atoi(p);
	if (pxlSize == targetPxlSize) {
	    best = fonts[i];
	    break;
	}
	err = pxlSize - targetPxlSize;
	if (abs(err) < abs(minerr) ||
	    (minerr > 0 && err < 0 && -err == minerr)) {
	    best = fonts[i];
	    minerr = err;
	}
    }
    p = (char *) XtMalloc(strlen(best) + 1);
    strcpy(p, best);
    XFreeFontNames(fonts);
    return p;
}

void CreateGCs()
{
    XtGCMask value_mask = GCLineWidth | GCLineStyle | GCForeground
      | GCBackground | GCFunction | GCPlaneMask;
    XGCValues gc_values;
    
    gc_values.plane_mask = AllPlanes;
    gc_values.line_width = LINE_GAP;
    gc_values.line_style = LineSolid;
    gc_values.function = GXcopy;
    
    gc_values.foreground = XBlackPixel(xDisplay, xScreen);
    gc_values.background = XBlackPixel(xDisplay, xScreen);
    lineGC = XtGetGC(shellWidget, value_mask, &gc_values);
    
    gc_values.background = XWhitePixel(xDisplay, xScreen);
    coordGC = XtGetGC(shellWidget, value_mask, &gc_values);
    XSetFont(xDisplay, coordGC, coordFontID);
    
    if (appData.monoMode) {
	gc_values.foreground = XWhitePixel(xDisplay, xScreen);
	gc_values.background = XBlackPixel(xDisplay, xScreen);
	lightSquareGC = wbPieceGC
	  = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = XBlackPixel(xDisplay, xScreen);
	gc_values.background = XWhitePixel(xDisplay, xScreen);
	darkSquareGC = bwPieceGC
	  = XtGetGC(shellWidget, value_mask, &gc_values);
    } else {
	gc_values.foreground = appData.lightSquareColor;
	gc_values.background = appData.darkSquareColor;
	lightSquareGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = appData.darkSquareColor;
	gc_values.background = appData.lightSquareColor;
	darkSquareGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = appData.whitePieceColor;
	gc_values.background = appData.darkSquareColor;
	wdPieceGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = appData.whitePieceColor;
	gc_values.background = appData.lightSquareColor;
	wlPieceGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = appData.blackPieceColor;
	gc_values.background = appData.darkSquareColor;
	bdPieceGC = XtGetGC(shellWidget, value_mask, &gc_values);
	
	gc_values.foreground = appData.blackPieceColor;
	gc_values.background = appData.lightSquareColor;
	blPieceGC = XtGetGC(shellWidget, value_mask, &gc_values);
    }
}

void CreatePieces()
{
    XSynchronize(xDisplay, True); /* Work-around for xlib/xt
				     buffering bug */
    
    ReadBitmap(appData.solidPawnBitmap, &solidPawnBitmap,
	       solid_pawn_bits, pawn_small_bits, xs_s_p_bits);
    ReadBitmap(appData.solidRookBitmap, &solidRookBitmap,
	       solid_rook_bits, rook_small_bits, xs_s_r_bits);
    ReadBitmap(appData.solidKnightBitmap, &solidKnightBitmap,
	       solid_knight_bits, knight_small_bits, xs_s_kt_bits);
    ReadBitmap(appData.solidBishopBitmap, &solidBishopBitmap,
	       solid_bishop_bits, bishop_small_bits, xs_s_b_bits);
    ReadBitmap(appData.solidQueenBitmap, &solidQueenBitmap,
	       solid_queen_bits, queen_small_bits, xs_s_q_bits);
    ReadBitmap(appData.solidKingBitmap, &solidKingBitmap,
	       solid_king_bits, king_small_bits, xs_s_k_bits);
    
    if (appData.monoMode) {
	ReadBitmap(appData.outlinePawnBitmap, &outlinePawnBitmap,
		   outline_pawn_bits, pawn_small_outline_bits,
		   xs_ol_p_bits);
	ReadBitmap(appData.outlineRookBitmap, &outlineRookBitmap,
		   outline_rook_bits, rook_small_outline_bits,
		   xs_ol_r_bits);
	ReadBitmap(appData.outlineKnightBitmap, &outlineKnightBitmap,
		   outline_knight_bits, knight_small_outline_bits,
		   xs_ol_kt_bits);
	ReadBitmap(appData.outlineBishopBitmap, &outlineBishopBitmap,
		   outline_bishop_bits, bishop_small_outline_bits,
		   xs_ol_b_bits);
	ReadBitmap(appData.outlineQueenBitmap, &outlineQueenBitmap,
		   outline_queen_bits, queen_small_outline_bits,
		   xs_ol_q_bits);
	ReadBitmap(appData.outlineKingBitmap, &outlineKingBitmap,
		   outline_king_bits, king_small_outline_bits,
		   xs_ol_k_bits);
    }
    
    XSynchronize(xDisplay, False); /* Work-around for xlib/xt
				      buffering bug */
}

void ReadBitmap(name, pm, large_bits, medium_bits, small_bits)
     String name;
     Pixmap *pm;
     char large_bits[], medium_bits[], small_bits[];
{
    int x_hot, y_hot;
    u_int w, h;
    
    if (*name == NULLCHAR ||
	XReadBitmapFile(xDisplay, xBoardWindow, name,
			&w, &h, pm, &x_hot, &y_hot) != BitmapSuccess ||
	w != squareSize || h != squareSize) {
	switch (boardSize) {
	  case Large:
	    *pm = XCreateBitmapFromData(xDisplay, xBoardWindow,
					large_bits, squareSize, squareSize);
	    break;
	  case Medium:
	    *pm = XCreateBitmapFromData(xDisplay, xBoardWindow,
					medium_bits, squareSize, squareSize);
	    break;
	  case Small:
	    *pm = XCreateBitmapFromData(xDisplay, xBoardWindow,
					small_bits, squareSize, squareSize);
	    break;
	}
    }
}

void CreateGrid()
{
    int i;
    
    for (i = 0; i < BOARD_SIZE + 1; i++) {
	gridSegments[i].x1 = gridSegments[i + BOARD_SIZE + 1].y1 = 0;
	gridSegments[i].y1 = gridSegments[i].y2
	  = LINE_GAP / 2 + (i * (squareSize + LINE_GAP));
	gridSegments[i].x2 = LINE_GAP + BOARD_SIZE *
	  (squareSize + LINE_GAP);
	gridSegments[i + BOARD_SIZE + 1].x1 =
	  gridSegments[i + BOARD_SIZE + 1].x2 = LINE_GAP / 2
	    + (i * (squareSize + LINE_GAP));
	gridSegments[i + BOARD_SIZE + 1].y2 =
	  BOARD_SIZE * (squareSize + LINE_GAP);
    }
}

void CreatePieceMenus()
{
    int i;
    Widget entry;
    Arg args[1];
    ChessSquare selection;
    
    XtSetArg(args[0], XtNlabel, "White");
    whitePieceMenu = XtCreatePopupShell("menuW", simpleMenuWidgetClass,
					boardWidget, args, 1);
    for (i = 0; i < PIECE_MENU_SIZE; i++) {
	String item = pieceMenuStrings[i];
	
	if (strcmp(item, "----") == 0) {
	    entry = XtCreateManagedWidget(item, smeLineObjectClass,
					  whitePieceMenu, NULL, 0);
	} else {
	    entry = XtCreateManagedWidget(item, smeBSBObjectClass,
					  whitePieceMenu, NULL, 0);
	    selection = pieceMenuTranslation[0][i];
	    XtAddCallback(entry, XtNcallback,
			  (XtCallbackProc) PieceMenuSelect,
			  (caddr_t) selection);
	    if (selection == WhitePawn) {
		XtSetArg(args[0], XtNpopupOnEntry, entry);
		XtSetValues(whitePieceMenu, args, 1);
	    }
	}
    }
    
    XtSetArg(args[0], XtNlabel, "Black");
    blackPieceMenu = XtCreatePopupShell("menuB", simpleMenuWidgetClass,
					boardWidget, args, 1);
    for (i = 0; i < PIECE_MENU_SIZE; i++) {
	String item = pieceMenuStrings[i];
	
	if (strcmp(item, "----") == 0) {
	    entry = XtCreateManagedWidget(item, smeLineObjectClass,
					  blackPieceMenu, NULL, 0);
	} else {
	    entry = XtCreateManagedWidget(item, smeBSBObjectClass,
					  blackPieceMenu, NULL, 0);
	    selection = pieceMenuTranslation[1][i];
	    XtAddCallback(entry, XtNcallback,
			  (XtCallbackProc) PieceMenuSelect,
			  (caddr_t) selection);
	    if (selection == BlackPawn) {
		XtSetArg(args[0], XtNpopupOnEntry, entry);
		XtSetValues(blackPieceMenu, args, 1);
	    }
	}
    }
    
    XtRegisterGrabAction(PieceMenuPopup, True,
			 (unsigned)(ButtonPressMask|ButtonReleaseMask),
			 GrabModeAsync, GrabModeAsync);
}	

void PieceMenuPopup(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (event->type != ButtonPress) return;
    if (gameMode != EditPosition) return;
    
    if (((pmFromX = EventToSquare(event->xbutton.x)) < 0) ||
	((pmFromY = EventToSquare(event->xbutton.y)) < 0)) {
	pmFromX = pmFromY = -1;
	return;
    }
    if (flipView)
      pmFromX = BOARD_SIZE - 1 - pmFromX;
    else
      pmFromY = BOARD_SIZE - 1 - pmFromY;
    
    XtPopupSpringLoaded(XtNameToWidget(boardWidget, params[0]));
}

static void PieceMenuSelect(w, piece, junk)
     Widget w;
     ChessSquare piece;
     caddr_t junk;
{
    if (pmFromX < 0 || pmFromY < 0) return;
    switch (piece) {
      case ClearBoard:
	for (pmFromY = 0; pmFromY < BOARD_SIZE; pmFromY++)
	  for (pmFromX = 0; pmFromX < BOARD_SIZE; pmFromX++)
	      boards[0][pmFromY][pmFromX] = EmptySquare;
	DrawPosition(boardWidget, NULL, NULL, NULL);
	break;
	
      case WhitePlay:	/*not currently on menu*/
	SetWhiteToPlay(NULL, NULL, NULL, NULL);
	break;
	
      case BlackPlay:	/*not currently on menu*/
	SetBlackToPlay(NULL, NULL, NULL, NULL);
	break;
	
      default:
	boards[0][pmFromY][pmFromX] = piece;
	DrawPosition(boardWidget, NULL, NULL, NULL);
	break;
    }
    XSync(xDisplay, False);
}

static void SetWhiteToPlay(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode != EditPosition) return;
    blackPlaysFirst = False;
    DisplayClocks(ReDisplayTimers); /* kludge; currentMove is 0 */
}

static void SetBlackToPlay(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode != EditPosition) return;
    blackPlaysFirst = True;
    currentMove = 1;	/* kludge */
    DisplayClocks(ReDisplayTimers);
    currentMove = 0;
}

char PieceToChar(p)
     ChessSquare p;
{
    return pieceToChar[(int) p];
}

ChessSquare CharToPiece(c)
     int c;
{
    switch (c) {
      default:
      case '.':	return EmptySquare;
      case 'P':	return WhitePawn;
      case 'R':	return WhiteRook;
      case 'N':	return WhiteKnight;
      case 'B':	return WhiteBishop;
      case 'Q':	return WhiteQueen;
      case 'K':	return WhiteKing;
      case 'p':	return BlackPawn;
      case 'r':	return BlackRook;
      case 'n':	return BlackKnight;
      case 'b':	return BlackBishop;
      case 'q':	return BlackQueen;
      case 'k':	return BlackKing;
    }
}

ChessSquare PromoPiece(move_type)
     ChessMove move_type;
{
    switch (move_type) {
      default:
	return EmptySquare;
      case WhitePromotionQueen:
	return WhiteQueen;
      case BlackPromotionQueen:
	return BlackQueen;
      case WhitePromotionRook:
	return WhiteRook;
      case BlackPromotionRook:
	return BlackRook;
      case WhitePromotionBishop:
	return WhiteBishop;
      case BlackPromotionBishop:
	return BlackBishop;
      case WhitePromotionKnight:
	return WhiteKnight;
      case BlackPromotionKnight:
	return BlackKnight;
    }
}


void CastleCoords(move_type, from_x, from_y, to_x, to_y)
     ChessMove move_type;
     int *from_x, *from_y, *to_x, *to_y;
{
    switch (move_type) {
      default:
	*from_x = *from_y = *to_x = *to_y = 0;
	break;
      case WhiteKingSideCastle:
	*from_x = 4;
	*from_y = 0;
	*to_x = 6;
	*to_y = 0;
	break;
      case WhiteQueenSideCastle:
	*from_x = 4;
	*from_y = 0;
	*to_x = 2;
	*to_y = 0;
	break;
      case BlackKingSideCastle:
	*from_x = 4;
	*from_y = 7;
	*to_x = 6;
	*to_y = 7;
	break;
      case BlackQueenSideCastle:
	*from_x = 4;
	*from_y = 7;
	*to_x = 2;
	*to_y = 7;
	break;
    }
}


/* Convert coordinates to normal algebraic notation.
   promoChar must be NULLCHAR or '.' if not a promotion.
   */
ChessMove CoordsToAlgebraic(fromX, fromY, toX, toY, promoChar,
			    currentBoardIndex, out)
     int fromX, fromY, toX, toY;
     int promoChar;
     int currentBoardIndex;
     char out[MOVE_LEN];
{
    ChessSquare piece;
    ChessMove ret;
    char *outp = out;
    int i;
    
    if (promoChar == '.') promoChar = NULLCHAR;
    piece = boards[currentBoardIndex][fromY][fromX];
    switch (piece) {
      case WhitePawn:
      case BlackPawn:
	/* Pawn move */
	*outp++ = fromX + 'a';
	if (fromX == toX) {
	    /* Non-capture; use style "e5" or "e8Q" */
	    *outp++ = toY + '1';
	    *outp++ = ToUpper(promoChar);
	    *outp = NULLCHAR;
	} else {
	    /* Capture; use style "exd5" or "exd8Q" */
	    *outp++ = 'x';
	    *outp++ = toX + 'a';
	    *outp++ = toY + '1';
	    *outp++ = ToUpper(promoChar);
	    *outp = NULLCHAR;
	}
	
	/* Test if okay by parsing; this notation should parse 
	   unambiguously if the original move was legal.  More
	   code would be needed if we wanted the style "ed" for
	   captures, since that can be ambiguous.
	   */
	ret = yylexstr(currentBoardIndex, out, (char **) NULL);
	break;
	
      case WhiteKing:
      case BlackKing:
	/* Test for castling */
	/* Use style "0-0" (zero-zero) */
	if (fromY == toY &&
	    fromY == ((piece == WhiteKing) ? 0 : 7) &&
	    fromX == 4 && (toX == 2 || toX == 6)) {
	    strcpy(out, toX == 2 ? "0-0-0" : "0-0");
	    ret = yylexstr(currentBoardIndex, out, (char **) NULL);
	    break;
	}
	/* else fall through */
	
      default:
	/* Piece move */
	/* First try style "Nf3" or "Nxf7" */
	*outp++ = ToUpper(PieceToChar(piece));
	
	/* Capture? */
	if(boards[currentBoardIndex][toY][toX] != EmptySquare)
	  *outp++ = 'x';
	
	*outp++ = toX + 'a';
	*outp++ = toY + '1';
	*outp = NULLCHAR;
	
	/* Test if ambiguous */
	ret = yylexstr(currentBoardIndex, out, (char **) NULL);
	if (ret != AmbiguousMove) break;
	
	/* Try style "Ngf3" or "Nexf7" */
	for (i=4; i>=1; i--) out[i+1] = out[i];
	out[1] = fromX + 'a';
	
	/* Test if ambiguous */
	ret = yylexstr(currentBoardIndex, out, (char **) NULL);
	if (ret != AmbiguousMove) break;
	
	/* Try style "N1f3" */
	out[1] = fromY + '1';
	
	/* Test if ambiguous */
	ret = yylexstr(currentBoardIndex, out, (char **) NULL);
	if (ret != AmbiguousMove) break;
	
	/* Try style "Ng1f3" or "Ne5xf7" */
	/* Can be needed iff there are 3 or more pieces of the
	   type being moved on the board, due to promotion */
	for (i=5; i>=2; i--) out[i+1] = out[i];
	out[1] = fromX + 'a';
	out[2] = fromY + '1';
	
	/* Test if okay */
	ret = yylexstr(currentBoardIndex, out, (char **) NULL);
	break; 
	
      case EmptySquare:
	/* Illegal move; use coordinate notation */
	*outp++ = fromX + 'a';
	*outp++ = fromY + '1';
	*outp++ = toX + 'a';
	*outp++ = toY + '1';
	*outp++ = ToUpper(promoChar);
	*outp = NULLCHAR;
	return BadMove;
    }
    
    switch (ret) {
      case NormalMove:
      case WhitePromotionKnight:
      case WhitePromotionBishop:
      case WhitePromotionRook:
      case WhitePromotionQueen:
      case BlackPromotionKnight:
      case BlackPromotionBishop:
      case BlackPromotionRook:
      case BlackPromotionQueen:
      case WhiteCapturesEnPassant:
      case BlackCapturesEnPassant:
	if (currentMoveString[0] != fromX + 'a' ||
	    currentMoveString[1] != fromY + '1' ||
	    currentMoveString[2] != toX + 'a' ||
	    currentMoveString[3] != toY + '1' ||
	    (promoChar != NULLCHAR &&
	     currentMoveString[4] != ToLower(promoChar))) {
	    /* Illegal move; use coordinate notation */
	    outp = out;
	    *outp++ = fromX + 'a';
	    *outp++ = fromY + '1';
	    *outp++ = toX + 'a';
	    *outp++ = toY + '1';
	    *outp++ = ToUpper(promoChar);
	    *outp = NULLCHAR;
	    return BadMove;
	}
	else
	  return ret;
	
      case WhiteKingSideCastle:
      case WhiteQueenSideCastle:
      case BlackKingSideCastle:
      case BlackQueenSideCastle:
	return ret;
	
      default:
	/* Illegal move; use coordinate notation */
	outp = out;
	*outp++ = fromX + 'a';
	*outp++ = fromY + '1';
	*outp++ = toX + 'a';
	*outp++ = toY + '1';
	*outp++ = ToUpper(promoChar);
	*outp = NULLCHAR;
	return BadMove;
    }
}


/*
 * If the user selects on a border boundary or off the board, return failure.
 * Otherwise map the event coordinate to the square.
 */
int EventToSquare(x)
     int x;
{
    if (x < LINE_GAP)
      return -1;
    x -= LINE_GAP;
    if ((x % (squareSize + LINE_GAP)) >= squareSize)
      return -1;
    x /= (squareSize + LINE_GAP);
    if (x >= BOARD_SIZE)
      return -1;
    return x;
}

void DrawSquare(row, column, piece)
     int row, column;
     ChessSquare piece;
{
    int square_color, x, y, direction, font_ascent, font_descent;
    char string[2];
    XCharStruct overall;
    
    if (flipView) {
	x = LINE_GAP + ((BOARD_SIZE-1)-column) * 
	  (squareSize + LINE_GAP);
	y = LINE_GAP + row * (squareSize + LINE_GAP);
    } else {
	x = LINE_GAP + column * (squareSize + LINE_GAP);
	y = LINE_GAP + ((BOARD_SIZE-1)-row) * 
	  (squareSize + LINE_GAP);
    }
    
    square_color = ((column + row) % 2) == 1;
    
    if (piece == EmptySquare)
      XFillRectangle(xDisplay, xBoardWindow,
		     square_color ? lightSquareGC : darkSquareGC,
		     x, y, squareSize, squareSize);
    else if (appData.monoMode) {
	if (square_color)
	  XCopyPlane(xDisplay, (int) piece < (int) BlackPawn
		     ? *pieceToOutline[(int) piece]
		     : *pieceToSolid[(int) piece],
		     xBoardWindow, bwPieceGC, 0, 0,
		     squareSize, squareSize, x, y, 1);
	else
	  XCopyPlane(xDisplay, (int) piece < (int) BlackPawn
		     ? *pieceToSolid[(int) piece]
		     : *pieceToOutline[(int) piece],
		     xBoardWindow, wbPieceGC, 0, 0,
		     squareSize, squareSize, x, y, 1);
    } else {
	if (square_color)
	  XCopyPlane(xDisplay, *pieceToSolid[(int) piece],
		     xBoardWindow, (int) piece < (int) BlackPawn
		     ? wlPieceGC : blPieceGC, 0, 0,
		     squareSize, squareSize, x, y, 1);
	else
	  XCopyPlane(xDisplay, *pieceToSolid[(int) piece],
		     xBoardWindow, (int) piece < (int) BlackPawn
		     ? wdPieceGC : bdPieceGC, 0, 0,
		     squareSize, squareSize, x, y, 1);
    }
    string[1] = NULLCHAR;
    if (appData.showCoords && row == (flipView ? 7 : 0)) {
	string[0] = 'a' + column;
	XTextExtents(coordFontStruct, string, 1, &direction, 
		     &font_ascent, &font_descent, &overall);
	if (appData.monoMode) {
	    XDrawImageString(xDisplay, xBoardWindow, coordGC,
			     x + squareSize - overall.width - 2, 
			     y + squareSize - font_descent - 1, string, 1);
	} else {
	    XDrawString(xDisplay, xBoardWindow, coordGC,
			x + squareSize - overall.width - 2, 
			y + squareSize - font_descent - 1, string, 1);
	}
    }
    if (appData.showCoords && column == (flipView ? 7 : 0)) {
	string[0] = '1' + row;
	XTextExtents(coordFontStruct, string, 1, &direction, 
		     &font_ascent, &font_descent, &overall);
	if (appData.monoMode) {
	    XDrawImageString(xDisplay, xBoardWindow, coordGC,
			     x + 2, y + font_ascent + 1, string, 1);
	} else {
	    XDrawString(xDisplay, xBoardWindow, coordGC,
			x + 2, y + font_ascent + 1, string, 1);
	}	    
    }   
}

void EventProc(widget, unused, event)
     Widget widget;
     caddr_t unused;
     XEvent *event;
{
    if (event->type == MappingNotify) {
	XRefreshKeyboardMapping((XMappingEvent *) event);
	return;
    }
    
    if (!XtIsRealized(widget))
      return;
    
    if ((event->type == ButtonPress) || (event->type == ButtonRelease))
      if (event->xbutton.button != Button1)
	return;
    
    switch (event->type) {
      case Expose:
	DrawPosition(widget, event, NULL, NULL);
	break;
      default:
	return;
    }
}

/*
 * event handler for redrawing the board
 */
void DrawPosition(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[1];
    int i, j;
    static Board lastBoard;
    static int lastBoardValid = 0;
    static int lastFlipView = 0;
    
    if (!appData.Iconic){
	XtSetArg(args[0], XtNiconic, False);
	XtSetValues(shellWidget, args, 1);
    }
    
    /*
     * It would be simpler to clear the window with XClearWindow()
     * but this causes a very distracting flicker.
     */
    
    if (event == NULL && lastBoardValid && lastFlipView == flipView) {
	for (i = 0; i < BOARD_SIZE; i++)
	  for (j = 0; j < BOARD_SIZE; j++)
	    if (boards[currentMove][i][j] != lastBoard[i][j])
	      DrawSquare(i, j, boards[currentMove][i][j]);
    } else {
	XDrawSegments(xDisplay, xBoardWindow, lineGC,
		      gridSegments, (BOARD_SIZE + 1) * 2);
	
	for (i = 0; i < BOARD_SIZE; i++)
	  for (j = 0; j < BOARD_SIZE; j++)
	    DrawSquare(i, j, boards[currentMove][i][j]);
    }
    
    CopyBoard(lastBoard, boards[currentMove]);
    lastBoardValid = 1;
    lastFlipView = flipView;
    
    XSync(xDisplay, False);
}

void InitPosition()
{
    currentMove = forwardMostMove = backwardMostMove = 0;
    CopyBoard(boards[0], initialPosition);
    DrawPosition(boardWidget, NULL, NULL, NULL);
}

void CopyBoard(to, from)
     Board to, from;
{
    int i, j;
    
    for (i = 0; i < BOARD_SIZE; i++)
      for (j = 0; j < BOARD_SIZE; j++)
	to[i][j] = from[i][j];
}

void SendCurrentBoard(fp)
     FILE *fp;
{
    SendBoard(fp, boards[currentMove]);
}

void SendBoard(fp, board)
     FILE *fp;
     Board board;
{
    char message[MSG_SIZ];
    ChessSquare *bp;
    int i, j;
    
    SendToProgram("edit\n", fp);
    SendToProgram("#\n", fp);
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	bp = &board[i][0];
	for (j = 0; j < BOARD_SIZE; j++, bp++) {
	    if ((int) *bp < (int) BlackPawn) {
		sprintf(message, "%c%c%c\n", PieceToChar(*bp), 
		    'a' + j, '1' + i);
		SendToProgram(message, fp);
	    }
	}
    }
    
    SendToProgram("c\n", fp);
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	bp = &board[i][0];
	for (j = 0; j < BOARD_SIZE; j++, bp++) {
	    if (((int) *bp != (int) EmptySquare)
		&& ((int) *bp >= (int) BlackPawn)) {
		sprintf(message, "%c%c%c\n", ToUpper(PieceToChar(*bp)),
			'a' + j, '1' + i);
		SendToProgram(message, fp);
	    }
	}
    }
    
    SendToProgram(".\n", fp);
}

/*
 * event handler for parsing user moves
 */
void HandleUserMove(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    ChessMove move_type;
    ChessSquare from_piece;
    int to_x, to_y;
    
    if ((w != boardWidget) || (matchMode != MatchFalse))
      return;
    
    if (promotionUp) {
	XtPopdown(promotionShell);
	XtDestroyWidget(promotionShell);
	promotionUp = False;
	fromX = fromY = -1;
    }
    
    switch (gameMode) {
      case EndOfGame:
      case PlayFromGameFile:
      case TwoMachinesPlay:
	return;
      case MachinePlaysWhite:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayMessage("It is not your turn");
	    return;
	}
	break;
      case MachinePlaysBlack:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayMessage("It is not your turn");
	    return;
	}
	break;
      case ForceMoves:
	if (appData.icsActive) {
	    if (ics_user_moved ||
		ics_mode == IcsObserving ||
		ics_mode == IcsIdle)
	      return;
	} else {
	    forwardMostMove = currentMove;
	}
	break;
      default:
	break;
    }
    
    if (currentMove != forwardMostMove) {
	DisplayMessage("Displayed position is not current");
	return;
    }
    
    switch (event->type) {
      case ButtonPress:
	if ((fromX >= 0) || (fromY >= 0))
	  return;
	if (((fromX = EventToSquare(event->xbutton.x)) < 0) ||
	    ((fromY = EventToSquare(event->xbutton.y)) < 0)) {
	    fromX = fromY = -1;
	    return;
	}
	if (flipView)
	  fromX = BOARD_SIZE - 1 - fromX;
	else
	  fromY = BOARD_SIZE - 1 - fromY;
	break;
	
      case ButtonRelease:
	if ((fromX < 0) || (fromY < 0)) return;
	
	if (((to_x = EventToSquare(event->xbutton.x)) < 0)
	    || ((to_y = EventToSquare(event->xbutton.y)) < 0)) {
	    if (gameMode == EditPosition) {
		boards[0][fromY][fromX] = EmptySquare;
		DrawPosition(boardWidget, NULL, NULL, NULL);
		XSync(xDisplay, False);
	    }
	    fromX = fromY = -1;
	    return;
	}
	if (flipView)
	  to_x = BOARD_SIZE - 1 - to_x;
	else
	  to_y = BOARD_SIZE - 1 - to_y;
	
	if ((fromX == to_x) && (fromY == to_y)) {
	    fromX = fromY = -1;
	    return;
	}
	
	if (gameMode == EditPosition) {
	    boards[0][to_y][to_x] = boards[0][fromY][fromX];
	    boards[0][fromY][fromX] = EmptySquare;
	    fromX = fromY = -1;
	    DrawPosition(boardWidget, NULL, NULL, NULL);
	    XSync(xDisplay, False);
	    return;
	}
	
	from_piece = boards[currentMove][fromY][fromX];
	if (WhiteOnMove(currentMove)) {
	    if ((int)from_piece < (int)WhitePawn ||
		(int)from_piece > (int)WhiteKing) {
		fromX = fromY = -1;
		return;
	    }
	} else {
	    if ((int)from_piece < (int)BlackPawn ||
		(int)from_piece > (int)BlackKing) {
		fromX = fromY = -1;
		return;
	    }
	}
	if (((from_piece == WhitePawn && to_y == 7) ||
	     (from_piece == BlackPawn && to_y == 0))) {
	    PromotionPopUp(from_piece, to_x, to_y);
	    return;
	}
	move_type = NormalMove;
	MakeMove(&move_type, fromX, fromY, to_x, to_y);
	FinishUserMove(move_type, to_x, to_y);
	break;
    }
}

void FinishUserMove(move_type, to_x, to_y)
     ChessMove move_type;
     int to_x, to_y;
{
    char user_move[MSG_SIZ];
    char promoChar;
    
    if (appData.icsActive) {
	sprintf(user_move, "%c%c%c%c\n",
		'a' + fromX, '1' + fromY, 'a' + to_x, '1' + to_y);
	switch (move_type) {
	  default:
	    fprintf(stderr, "%s: internal error; bad move_type\n",
		    programName);
	    break;
	  case WhiteKingSideCastle:
	  case BlackKingSideCastle:
	    sprintf(user_move, "00\n");
	    break;
	  case WhiteQueenSideCastle:
	  case BlackQueenSideCastle:
	    sprintf(user_move, "000\n");
	    break;
	  case WhitePromotionQueen:
	  case BlackPromotionQueen:
	    SendToICS("promote queen\n");
	    break;
	  case WhitePromotionRook:
	  case BlackPromotionRook:
	    SendToICS("promote rook\n");
	    break;
	  case WhitePromotionBishop:
	  case BlackPromotionBishop:
	    SendToICS("promote bishop\n");
	    break;
	  case WhitePromotionKnight:
	  case BlackPromotionKnight:
	    SendToICS("promote knight\n");
	    break;
	  case NormalMove:
	  case WhiteCapturesEnPassant:
	  case BlackCapturesEnPassant:
	    break;
	}
	SendToICS(user_move);
	ics_user_moved = 1;
    } else {
	promoChar = ToLower(PieceToChar(PromoPiece(move_type)));
	if (promoChar == '.')
	  sprintf(user_move, "%c%c%c%c\n",
		  'a' + fromX, '1' + fromY, 'a' + to_x, '1' + to_y);
	else
	  sprintf(user_move, "%c%c%c%c%c\n",
		  'a' + fromX, '1' + fromY, 'a' + to_x, '1' + to_y,
		  promoChar);
	
	Attention(firstProgramPID);
	if (firstSendTime)
	  SendTimeRemaining(toFirstProgFP);
	SendToProgram(user_move, toFirstProgFP);
    }
    
    fromX = fromY = -1;
    
    strcpy(moveList[currentMove - 1], user_move);
    
    /* A user move restarts a paused game*/
    if (gameMode == PauseGame)
      PauseProc(NULL, NULL, NULL, NULL);
    
    switch (gameMode) {
      case ForceMoves:
	break;
      case BeginningOfGame:
	if (appData.noChessProgram)
	  lastGameMode = gameMode = ForceMoves;
	else
	  lastGameMode = gameMode = MachinePlaysBlack;
	ModeHighlight();
	break;
      case MachinePlaysBlack:
      case MachinePlaysWhite:
      default:
	break;
    }
}

/* Simple parser for moves from gnuchess.  Could replace with yylexstr() 
   and some extra glue. */
void ParseMachineMove(machine_move, move_type, from_x, from_y, to_x, to_y)
     char *machine_move;
     ChessMove *move_type;
     int *from_x, *from_y, *to_x, *to_y;
{       
    if (strcmp(machine_move, "o-o") == 0) {
	if (WhiteOnMove(forwardMostMove)) {
	    *move_type = WhiteKingSideCastle;
	    *from_x = 4;
	    *from_y = 0;
	    *to_x = 6;
	    *to_y = 0;
	} else {
	    *move_type = BlackKingSideCastle;
	    *from_x = 4;
	    *from_y = 7;
	    *to_x = 6;
	    *to_y = 7;
	}
    } else if (strcmp(machine_move, "o-o-o") == 0) {
	if (WhiteOnMove(forwardMostMove)) {
	    *move_type = WhiteQueenSideCastle;
	    *from_x = 4;
	    *from_y = 0;
	    *to_x = 2;
	    *to_y = 0;
	} else {
	    *move_type = BlackQueenSideCastle;
	    *from_x = 4;
	    *from_y = 7;
	    *to_x = 2;
	    *to_y = 7;
	}
    } else {
	*from_x = machine_move[0] - 'a';
	*from_y = machine_move[1] - '1';
	*to_x = machine_move[2] - 'a';
	*to_y = machine_move[3] - '1';
	
	switch (machine_move[4]) {
	  case 'q':
	    *move_type = WhiteOnMove(forwardMostMove) ?
	      WhitePromotionQueen : BlackPromotionQueen;
	    break;
	  case 'r':
	    *move_type = WhiteOnMove(forwardMostMove) ?
	      WhitePromotionRook : BlackPromotionRook;
	    break;
	  case 'b':
	    *move_type = WhiteOnMove(forwardMostMove) ?
	      WhitePromotionBishop : BlackPromotionBishop;
	    break;
	  case 'n':
	    *move_type = WhiteOnMove(forwardMostMove) ?
	      WhitePromotionKnight : BlackPromotionKnight;
	    break;
	  default:
	    *move_type = NormalMove;
	    break;
	}		  
    }
}

void HandleMachineMove(message, fp)
     char *message;
     FILE *fp;
{
    char machine_move[MSG_SIZ], buf1[MSG_SIZ], buf2[MSG_SIZ];
    int from_x, from_y, to_x, to_y;
    ChessMove move_type;
    
    maybeThinking = False;
    
    if (strncmp(message, "warning:", 8) == 0) {
	DisplayMessage(message);
	return;
    }
    
    /*
     * If chess program startup fails, exit with an error message.
     * Attempts to recover here are futile.
     */
    if ((StrStr(message, "unknown host") != NULL)
	|| (StrStr(message, "No remote directory") != NULL)
	|| (StrStr(message, "not found") != NULL)
	|| (StrStr(message, "No such file") != NULL)
	|| (StrStr(message, "Permission denied") != NULL)) {
	fprintf(stderr,
		"%s: failed to start chess program %s on %s: %s\n",
		programName,
		fp == fromFirstProgFP ? appData.firstChessProgram
		: appData.secondChessProgram,
		fp == fromFirstProgFP ? appData.firstHost
		: appData.secondHost,
		message);
	ShutdownChessPrograms(message);
	exit(1);
    }
    
    if (strncmp(message, "time", 4) == 0) {
	if (StrStr(message, "CHESS")) {
	    /* Program has a broken "time" command that
	       outputs a string not ending in newline.
	       Don't use it. */
	    if (fp == fromFirstProgFP) firstSendTime = 0;
	    if (fp == fromSecondProgFP) secondSendTime = 0;
	}
    }
    
    /*
     * If the move is illegal, cancel it and redraw the board.
     */
    if (strncmp(message, "Illegal move", 12) == 0) {
	
	if (fp == fromFirstProgFP && firstSendTime == 2) {
	    /* First program doesn't have the "time" command */
	    firstSendTime = 0;
	    return;
	} else if (fp == fromSecondProgFP && secondSendTime == 2) {
	    /* Second program doesn't have the "time" command */
	    secondSendTime = 0;
	    return;
	}
	if (forwardMostMove <= backwardMostMove) return;
	if (gameMode == PlayFromGameFile) {
	    /* Stop reading this game file */
	    gameMode = ForceMoves;
	    ModeHighlight();
	}
	currentMove = --forwardMostMove;
	if ((gameMode == PlayFromGameFile) || 
	    (gameMode == ForceMoves))
	  DisplayClocks(ReDisplayTimers);
	else
	  DisplayClocks(SwitchTimers);
	sprintf(buf1, "Illegal move: %s", parseList[currentMove]);
	DisplayMessage(buf1);
	
	DrawPosition(boardWidget, NULL, NULL, NULL);
	
	XSync(xDisplay, False);
	return;
    }
    
    if (strncmp(message, "Hint:", 5) == 0) {
	sscanf(message, "Hint: %s", machine_move);
	ParseMachineMove(machine_move, &move_type,
			 &from_x, &from_y, &to_x, &to_y);
	move_type = CoordsToAlgebraic(from_x, from_y, to_x, to_y, NULLCHAR,
				      forwardMostMove, buf1);
	sprintf(buf2, "Hint: %s", buf1);
	DisplayMessage(buf2);
	return;
    }
    
    /*
     * win, lose or draw
     */
    if (strncmp(message, "White", 5) == 0) {
	ShutdownChessPrograms("White mates");
	return;
    } else if (strncmp(message, "Black", 5) == 0) {
	ShutdownChessPrograms("Black mates");
	return;
    } else if (strncmp(message, "opponent mates!", 15) == 0) {
	switch (gameMode == PauseGame ? pausePreviousMode : gameMode) {
	  case MachinePlaysBlack:
	    ShutdownChessPrograms("White mates");
	    break;
	  case MachinePlaysWhite:
	    ShutdownChessPrograms("Black mates");
	    break;
	  case TwoMachinesPlay:
	    ShutdownChessPrograms(fp == fromFirstProgFP ?
				  "White mates" : "Black mates");
	    break;
	  default:
	    /* can't happen */
	    break;
	}
	return;
    } else if (strncmp(message, "computer mates!", 15) == 0) {
	switch (gameMode == PauseGame ? pausePreviousMode : gameMode) {
	  case MachinePlaysBlack:
	    ShutdownChessPrograms("Black mates");
	    break;
	  case MachinePlaysWhite:
	    ShutdownChessPrograms("White mates");
	    break;
	  case TwoMachinesPlay:
	    ShutdownChessPrograms(fp == fromFirstProgFP ?
				  "Black mates" : "White mates");
	    break;
	  default:
	    /* can't happen */
	    break;
	}
	return;
    } else if (strncmp(message, "Draw", 4) == 0) {
	ShutdownChessPrograms("Draw");
	return;
    }
    
    /*
     * normal machine reply move
     */
    maybeThinking = True;
    if (StrStr(message, "...") != NULL) {
	sscanf(message, "%s %s %s", buf1, buf2, machine_move);
	if (machine_move[0] == NULLCHAR)
	  return;
    } else
      return;		/* ignore noise */
    
    strcpy(moveList[forwardMostMove], machine_move);
    
    ParseMachineMove(machine_move, &move_type, &from_x, &from_y,
		     &to_x, &to_y);
    
    if (gameMode != PauseGame)
      currentMove = forwardMostMove;  /*display latest move*/
    
    MakeMove(&move_type, from_x, from_y, to_x, to_y);
    
    if (gameMode != PauseGame && appData.ringBellAfterMoves)
      putc(BELLCHAR, stderr);
    
    if (gameMode == TwoMachinesPlay ||
	(gameMode == PauseGame && pausePreviousMode == TwoMachinesPlay)) {
	strcat(machine_move, "\n");
	if (WhiteOnMove(forwardMostMove)) {
	    Attention(secondProgramPID);
	    if (secondSendTime) 
	      SendTimeRemaining(toSecondProgFP);
	    SendToProgram(machine_move, toSecondProgFP);
	    if (firstMove) {
		firstMove = False;
		SendToProgram(appData.whiteString,
			      toSecondProgFP);
	    }
	} else {
	    Attention(firstProgramPID);
	    if (firstSendTime)
	      SendTimeRemaining(toFirstProgFP);
	    SendToProgram(machine_move, toFirstProgFP);
	    if (firstMove) {
		firstMove = False;
		SendToProgram(appData.blackString,
			      toFirstProgFP);
	    }
	}
    }
}


/* Parse a game score from the character string "game", and
   record it as the history of the current game.  The game
   score is assumed to start from the standard position. 
   The display is not updated in any way.
   */
void ParseGameHistory(game)
     char *game;
{
    ChessMove move_type;
    int from_x, from_y, to_x, to_y, boardIndex;
    char promo_char;
    
    if (appData.debugMode)
      fprintf(stderr, "Parsing game history: %s\n", game);

    CopyBoard(boards[0], initialPosition);
    startedFromSetupPosition = False;
    boardIndex = 0;
    for (;;) {
	move_type = yylexstr(boardIndex, game, &game);
	promo_char = NULLCHAR;
	switch (move_type) {
	  case WhiteKingSideCastle:
	  case WhiteQueenSideCastle:
	  case BlackKingSideCastle:
	  case BlackQueenSideCastle:
	    CastleCoords(move_type, &from_x, &from_y, &to_x, &to_y);
	    break;
	  case WhitePromotionQueen:
	  case BlackPromotionQueen:
	  case WhitePromotionRook:
	  case BlackPromotionRook:
	  case WhitePromotionBishop:
	  case BlackPromotionBishop:
	  case WhitePromotionKnight:
	  case BlackPromotionKnight:
	    promo_char = ToLower(PieceToChar(PromoPiece(move_type)));
	    /* fall thru */
	  case NormalMove:
	  case WhiteCapturesEnPassant:
	  case BlackCapturesEnPassant:
	    from_x = ToLower(currentMoveString[0]) - 'a';
	    from_y = ToLower(currentMoveString[1]) - '1';
	    to_x = ToLower(currentMoveString[2]) - 'a';
	    to_y = ToLower(currentMoveString[3]) - '1';
	    break;
	  case BadMove:
	  case AmbiguousMove:
	    /* bug? */
	    fprintf(stderr, "?bad move in ICS output\n");
	    /* fall thru */
	  case 0:	/* end of file */
	    if (boardIndex < backwardMostMove) {
		/* Oops, gap.  How did that happen? */
		return;
	    }
	    backwardMostMove = 0;
	    if (boardIndex > forwardMostMove) {
		forwardMostMove = boardIndex;
	    }
	    return;
	  case ElapsedTime:
	    if (boardIndex > 0) {
		strcat(parseList[boardIndex-1], " ");
		strcat(parseList[boardIndex-1], yytext);
	    }
	    continue;
	  case Comment:
	  case StartGame:
	  case WhiteWins:
	  case BlackWins:
	  case GameIsDrawn:
	  default:
	    /* ignore */
	    continue;
	}
	CoordsToAlgebraic(from_x, from_y, to_x, to_y, promo_char,
			  boardIndex, parseList[boardIndex]);
	CopyBoard(boards[boardIndex + 1], boards[boardIndex]);
	boardIndex++;
	ApplyMove(&move_type, from_x, from_y, to_x, to_y,
		  boards[boardIndex]);
    }
}

void LoadGameLoop()
{
    readGameXID = 0;
    for (;;) {
	if (!LoadGameOneMove())
	  return;
	if (matchMode == MatchOpening || appData.timeDelay == 0)
	  continue;
	if (appData.timeDelay < 0)
	  return;
	readGameXID =
	  XtAppAddTimeOut(appContext, (int) (1000 * appData.timeDelay),
			  (XtTimerCallbackProc) LoadGameLoop, NULL);
	break;
    }
}

Boolean LoadGameOneMove()
{
    int from_x, from_y, to_x, to_y, done;
    ChessMove move_type;
    char move[MSG_SIZ];
    
    if (gameFileFP == NULL)
      return False;
    
    if (!(gameMode == PlayFromGameFile ||
	  (gameMode == PauseGame && pausePreviousMode == PlayFromGameFile))) {
	fclose(gameFileFP);
	gameFileFP = NULL;
	yynewfile();
	return False;
    }
    
    if (commentUp) CommentPopDown();
    
    yyboardindex = forwardMostMove;
    move_type = (ChessMove) yylex();
    
    if (appData.debugMode) {
	switch (move_type) {
	  case BadMove:
	    fprintf(stderr, "Parsed BadMove: %s\n", yytext);
	    break;
	  case AmbiguousMove:
	    fprintf(stderr, "Parsed AmbiguousMove: %s\n", yytext);
	    break;
	  case Comment:
	    fprintf(stderr, "Parsed Comment: %s\n", yytext);
	    break;
	  case WhiteWins:
	  case BlackWins:
	  case GameIsDrawn:
	  case 0:
	    fprintf(stderr, "Parsed game end: %s\n", yytext);
	    break;
	  case StartGame:
	    fprintf(stderr, "Parsed StartGame: %s\n", yytext);
	    break;
	  case ElapsedTime:
	    fprintf(stderr, "Parsed ElapsedTime: %s\n", yytext);
	    break;
	  default:
	    fprintf(stderr, "Parsed %s into %s", yytext, currentMoveString);
	    break;
	}
    }
    
    done = False;
    switch (move_type) {
      case Comment:
	CommentPopUp(yytext);
	return True;
      case WhiteKingSideCastle:
      case WhiteQueenSideCastle:
      case BlackKingSideCastle:
      case BlackQueenSideCastle:
	CastleCoords(move_type, &from_x, &from_y, &to_x, &to_y);
	break;
      case WhiteCapturesEnPassant:
      case BlackCapturesEnPassant:
      case WhitePromotionQueen:
      case BlackPromotionQueen:
      case WhitePromotionRook:
      case BlackPromotionRook:
      case WhitePromotionBishop:
      case BlackPromotionBishop:
      case WhitePromotionKnight:
      case BlackPromotionKnight:
      case NormalMove:
	from_x = ToLower(currentMoveString[0]) - 'a';
	from_y = ToLower(currentMoveString[1]) - '1';
	to_x = ToLower(currentMoveString[2]) - 'a';
	to_y = ToLower(currentMoveString[3]) - '1';
	break;
      case 0:  /* end of file */
	DisplayMessage("End of game file");
	done = True;
	break;
      case WhiteWins:
      case BlackWins:
      case GameIsDrawn:
	GameEnds(yytext);
	return False;
      case StartGame:
#ifdef notdef
	/* We see too many bogus StartGame markers with current
	   lex definition, especially in gnuchess listing files,
	   so this code is disabled. */
	/* Reached start of next game in file */
	DisplayMessage("End of game");
	done = True;
#else /*!notdef*/
	/* ignore */
	return LoadGameOneMove(); /* tail recursion */
#endif /*notdef*/
	break;
      case ElapsedTime:
	/* ignore */
	return LoadGameOneMove(); /* tail recursion */
      default:
      case BadMove:
	sprintf(move, "Bad move: %d. %s%s",
		(forwardMostMove / 2) + 1,
		WhiteOnMove(forwardMostMove) ? "" : "... ", yytext);
	DisplayMessage(move);
	done = True;
	break;
      case AmbiguousMove:
	sprintf(move, "Ambiguous move: %d. %s%s",
		(forwardMostMove / 2) + 1,
		WhiteOnMove(forwardMostMove) ? "" : "... ", yytext);
	DisplayMessage(move);
	done = True;
	break;
    }
    
    if (done) {
	lastGameMode = (gameMode == PauseGame ? pausePreviousMode : gameMode);
	gameMode = ForceMoves;
	ModeHighlight();
	if (readGameXID != 0) {
	    XtRemoveTimeOut(readGameXID);
	    readGameXID = 0;
	}
	fclose(gameFileFP);
	gameFileFP = NULL;
	yynewfile();
	return (int) False;
    } else {
	SendToProgram(currentMoveString, toFirstProgFP);
	strcpy(moveList[forwardMostMove], currentMoveString);
	
	MakeMove(&move_type, from_x, from_y, to_x, to_y);
	
	return (int) True;
    }
}

void ApplyMove(move_type, from_x, from_y, to_x, to_y, board)
     ChessMove *move_type;
     int from_x, from_y, to_x, to_y;
     Board board;
{
    if (from_y == 0 && from_x == 4
	&& board[from_y][from_x] == WhiteKing
	&& to_y == 0 && to_x == 6) {
	*move_type = WhiteKingSideCastle;
	board[0][7] = EmptySquare;
	board[0][6] = WhiteKing;
	board[0][5] = WhiteRook;
	board[0][4] = EmptySquare;
    } else if (from_y == 0 && from_x == 4
	       && board[from_y][from_x] == WhiteKing
	       && to_y == 0 && to_x == 2) {
	*move_type = WhiteQueenSideCastle;
	board[0][0] = EmptySquare;
	board[0][2] = WhiteKing;
	board[0][3] = WhiteRook;
	board[0][4] = EmptySquare;
    } else if (from_y == 6
	       && board[from_y][from_x] == WhitePawn
	       && to_y == 7) {
	/* white pawn promotion */
	board[7][to_x] = PromoPiece(*move_type);
	if (board[7][to_x] == EmptySquare) {
	    board[7][to_x] = WhiteQueen;
	    *move_type = WhitePromotionQueen;
	}
	board[6][from_x] = EmptySquare;
    } else if ((from_y == 4)
	       && (to_x != from_x)
	       && (board[from_y][from_x] == WhitePawn)
	       && (board[to_y][to_x] == EmptySquare)) {
	*move_type = WhiteCapturesEnPassant;
	board[from_y][from_x] = EmptySquare;
	board[to_y][to_x] = WhitePawn;
	board[to_y - 1][to_x] = EmptySquare;
    } else if (from_y == 7 && from_x == 4
	       && board[from_y][from_x] == BlackKing
	       && to_y == 7 && to_x == 6) {
	*move_type = BlackKingSideCastle;
	board[7][4] = EmptySquare;
	board[7][5] = BlackRook;
	board[7][6] = BlackKing;
	board[7][7] = EmptySquare;
    } else if (from_y == 7 && from_x == 4
	       && board[from_y][from_x] == BlackKing
	       && to_y == 7 && to_x == 2) {
	*move_type = BlackQueenSideCastle;
	board[7][0] = EmptySquare;
	board[7][2] = BlackKing;
	board[7][3] = BlackRook;
	board[7][4] = EmptySquare;
    } else if (from_y == 1
	       && board[from_y][from_x] == BlackPawn
	       && to_y == 0) {
	/* black pawn promotion */
	board[0][to_x] = PromoPiece(*move_type);
	if (board[0][to_x] == EmptySquare) {
	    board[0][to_x] = BlackQueen;
	    *move_type = BlackPromotionQueen;
	}
	board[1][from_x] = EmptySquare;
    } else if ((from_y == 3)
	       && (to_x != from_x)
	       && (board[from_y][from_x] == BlackPawn)
	       && (board[to_y][to_x] == EmptySquare)) {
	*move_type = BlackCapturesEnPassant;
	board[from_y][from_x] = EmptySquare;
	board[to_y][to_x] = BlackPawn;
	board[to_y + 1][to_x] = EmptySquare;
    } else {
	*move_type = NormalMove;
	board[to_y][to_x] = board[from_y][from_x];
	board[from_y][from_x] = EmptySquare;
    }
}

/*
 * MakeMove() displays moves.  If they are illegal, GNU chess will detect
 * this and send an Illegal move message.  XBoard will then retract the move.
 * The clockMode False case is tricky because it displays the player on move.
 */
void MakeMove(move_type, from_x, from_y, to_x, to_y)
     ChessMove *move_type;
     int from_x, from_y, to_x, to_y;
{
    forwardMostMove++;
    CopyBoard(boards[forwardMostMove], boards[forwardMostMove - 1]);
    ApplyMove(move_type, from_x, from_y, to_x, to_y,
	      boards[forwardMostMove]);
    
    endMessage[0] = NULLCHAR;
    
    CoordsToAlgebraic(from_x, from_y, to_x, to_y,
		      ToLower(PieceToChar(PromoPiece(*move_type))),
		      forwardMostMove - 1, parseList[forwardMostMove - 1]);
    
    if (gameMode == PauseGame && pausePreviousMode != PlayFromGameFile)
      return;
    
    currentMove = forwardMostMove;
    if ((gameMode == PlayFromGameFile) || (gameMode == ForceMoves) ||
	(gameMode == PauseGame && pausePreviousMode == PlayFromGameFile))
      DisplayClocks(ReDisplayTimers);
    else
      DisplayClocks(SwitchTimers);
    DisplayMove(currentMove - 1);
    DrawPosition(boardWidget, NULL, NULL, NULL);
    XSync(xDisplay, False);
}

void InitChessProgram(host_name, program_name, pid, to, from, xid, sendTime)
     char *host_name, *program_name;
     int *pid;
     FILE **to, **from;
     XtIntervalId *xid;
     int *sendTime;
{
    char arg_buf[10];
    char *arg1, *arg2;
    int to_prog[2], from_prog[2];
    FILE *from_fp, *to_fp;
    int dummy_source;
    XtInputId dummy_id;
#if defined(SYSTEM_FIVE) || defined(SYSV)
    char *pty_name;
#endif
    
    if (appData.noChessProgram) return;
    
#if defined(SYSTEM_FIVE) || defined(SYSV)
    if ((pty_name = PseudoTTY(&to_prog[1])) == NULL) {
	fprintf(stderr, "%s: can't open pseudo-tty: ", programName);
	perror("");
	exit(1);
    }
    
    from_prog[0] = to_prog[1];
    to_prog[0] = from_prog[1] = open(pty_name, O_RDWR, 0);
#ifdef SVR4
    if (ioctl (to_prog[0], I_PUSH, "ptem") == -1 ||
	ioctl (to_prog[0], I_PUSH, "ldterm") == -1 ||
	ioctl (to_prog[0], I_PUSH, "ttcompat") == -1) {
	fprintf(stderr, "%s: can't ioctl pseudo-tty: ", programName);
	perror("");
	exit(1);
    }
#endif
#else
    signal(SIGPIPE, CatchPipeSignal);
    pipe(to_prog);
    pipe(from_prog);
#endif
    
    if ((*pid = fork()) == 0) {
#if !defined(SYSTEM_FIVE) && !defined(SYSV)
	signal(SIGPIPE, CatchPipeSignal);
#endif
	dup2(to_prog[0], 0);
	dup2(from_prog[1], 1);
	close(to_prog[0]);
	close(to_prog[1]);
	close(from_prog[0]);
	close(from_prog[1]);
	dup2(1, fileno(stderr)); /* force stderr to the pipe */
	if (*appData.searchTime != NULLCHAR) {
	    sprintf(arg_buf, "%d", searchTime);
	    arg1 = arg_buf;
	    arg2 = (char *) NULL;
	} else if (appData.searchDepth > 0) {
	    sprintf(arg_buf, "%d", appData.searchDepth);
	    arg1 = "1";
	    arg2 = "9999";
	} else {
	    sprintf(arg_buf, "%d", appData.movesPerSession);
	    arg1 = arg_buf;
	    arg2 = appData.timeControl;
	}
	if (strcmp(host_name, "localhost") == 0) {
	    execlp(program_name, program_name, arg1, arg2,
		   (char *) NULL);
	} else {
	    execlp(appData.remoteShell, appData.remoteShell,
		   host_name, program_name, arg1, arg2,
		   (char *) NULL);
	}
	
	perror(program_name);
	exit(1);
    }
    
    close(to_prog[0]);
    close(from_prog[1]);
    
    *from = from_fp = fdopen(from_prog[0], "r");
    *to = to_fp = fdopen(to_prog[1], "w");
    setbuf(from_fp, NULL); setbuf(to_fp, NULL);
    
    ReceiveFromProgram(from_fp, &dummy_source, &dummy_id); /*"Chess"*/
    if (*pid == 0) return;
    
    *xid = XtAppAddInput(appContext, fileno(from_fp), 
			 (XtPointer)XtInputReadMask,
			 (XtInputCallbackProc)ReceiveFromProgram, 
			 (XtPointer)from_fp);
    
    SendToProgram(appData.initString, to_fp);
    SendSearchDepth(to_fp);
    
    if (*sendTime == 2) {
	/* Does program have "time" command? */
	char buf[MSG_SIZ];
	
	sprintf(buf, "time %d\nhelp\n", blackTimeRemaining/10);
	/* "help" is a kludge to work around a gnuchess bug;
	   some versions do not send a newline at the end of
	   their response to the time command */
	SendToProgram(buf, to_fp);
	ReceiveFromProgram(from_fp, &dummy_source, &dummy_id);
	if (*sendTime == 2) *sendTime = 1;  /* yes! */
    }
    
}

void GameEnds(why)
     char *why;
{
    ics_mode = IcsIdle;
    ics_gamenum = -1;
    if (readGameXID != 0) {
	XtRemoveTimeOut(readGameXID);
	readGameXID = 0;
    }
    if (gameFileFP != NULL) {
	fclose(gameFileFP);
	gameFileFP = NULL;
	yynewfile();
    }
    DisplayClocks(StopTimers);
    
    if (why == NULL) return;
    
    strncpy(endMessage, why, MOVE_LEN * 4);
    endMessage[MOVE_LEN * 4 - 1] = NULLCHAR;
    if (currentMove == forwardMostMove)
      DisplayMove(currentMove - 1);
    
    if (gameMode != PlayFromGameFile) {
	if (*appData.saveGameFile != NULLCHAR) {
	    SaveGame(appData.saveGameFile);
	} else if (appData.autoSaveGames) {
	    SaveGameProc(NULL, NULL, NULL, NULL);
	}
	if (appData.savePositionFile[0] != NULLCHAR) 
	  SavePosition(appData.savePositionFile);
    }
}

void ShutdownChessPrograms(why)
     char *why;
{
    GameEnds(why);
    
    lastGameMode = gameMode;
    if (gameMode == PauseGame) {
	pausePreviousMode = EndOfGame;
    } else {
	gameMode = EndOfGame;
	ModeHighlight();
    }
    
    if (firstProgramPID != 0) {
	if (kill(firstProgramPID, 0) == 0)
	  SendToProgram("quit\n", toFirstProgFP);
	fclose(fromFirstProgFP);
	fclose(toFirstProgFP);
	fromFirstProgFP = toFirstProgFP = NULL;
	if (kill(firstProgramPID, SIGTERM)==0) 
	  wait((union wait *)0);
	
    }
    firstProgramPID = 0;
    
    if (firstProgramXID != 0)
      XtRemoveInput(firstProgramXID);
    firstProgramXID = 0;
    
    if (secondProgramPID != 0) {
	if (kill(secondProgramPID, 0) == 0)
	  SendToProgram("quit\n", toSecondProgFP);
	fclose(fromSecondProgFP);
	fclose(toSecondProgFP);
	fromSecondProgFP = toSecondProgFP = NULL;
	if (kill(secondProgramPID, SIGTERM)==0) 
	  wait((union wait *)0);
    }
    secondProgramPID = 0;
    
    if (secondProgramXID != 0)
      XtRemoveInput(secondProgramXID);
    secondProgramXID = 0;
    
    if (matchMode != MatchFalse) exit(0);
}

void CommentPopDown()
{
    Arg args[2];
    
    XtSetArg(args[0], XtNx, &commentX);
    XtSetArg(args[1], XtNy, &commentY);
    XtGetValues(commentShell, args, 2);
    XtPopdown(commentShell);
    XtDestroyWidget(commentShell);
    commentUp = False;
}


void CommentPopUp(label)
     char *label;
{
    Arg args[2];
    Position x, y;
    Dimension bw_width, pw_width;
    
    if (commentUp) CommentPopDown();
    
    DisplayMessage("Comment");
    
    XtSetArg(args[0], XtNwidth, &bw_width);
    XtGetValues(formWidget, args, 1);
    
    XtSetArg(args[0], XtNresizable, True);
    XtSetArg(args[1], XtNwidth, bw_width - 8);
    
    commentShell =
      XtCreatePopupShell("Comment", transientShellWidgetClass,
			 commandsWidget, args, 2);
    
    XtSetArg(args[0], XtNlabel, label);
    
    (void) XtCreateManagedWidget("commentLabel", labelWidgetClass,
				 commentShell, args, 1);
    
    XtRealizeWidget(commentShell);
    
    XtSetArg(args[0], XtNwidth, &pw_width);
    XtGetValues(commentShell, args, 1);
    
    if (commentX == -1) {
	XtTranslateCoords(shellWidget, (bw_width - pw_width) / 2, -50, &x, &y);
    } else {
	x = commentX - appData.borderXoffset;
	y = commentY - appData.borderYoffset;
    }

    XtSetArg(args[0], XtNx, x);
    XtSetArg(args[1], XtNy, y);
    XtSetValues(commentShell, args, 2);
    
    XtPopup(commentShell, XtGrabNone);

    commentUp = True;
}

void FileNamePopUp(label, proc)
     char *label;
     Boolean (*proc) P((char *name));
{
    Arg args[2];
    Widget popup, dialog;
    Dimension bw_width, pw_width;
    Window root, child;
    int x, y;
    int win_x, win_y;
    unsigned int mask;
    
    fileProc = proc;
    
    XtSetArg(args[0], XtNwidth, &bw_width);
    XtGetValues(boardWidget, args, 1);
    
    XtSetArg(args[0], XtNresizable, True);
    XtSetArg(args[1], XtNwidth, DIALOG_SIZE);
    
    popup =
      XtCreatePopupShell("File Name Prompt", transientShellWidgetClass,
			 commandsWidget, args, 2);
    
    XtSetArg(args[0], XtNlabel, label);
    XtSetArg(args[1], XtNvalue, "");
    
    dialog = XtCreateManagedWidget("dialog", dialogWidgetClass,
				   popup, args, 2);
    
    XawDialogAddButton(dialog, "ok", FileNameCallback, (XtPointer) dialog);
    XawDialogAddButton(dialog, "cancel", FileNameCallback,
		       (XtPointer) dialog);
    
    XtRealizeWidget(popup);
    
    XtSetArg(args[0], XtNwidth, &pw_width);
    XtGetValues(popup, args, 1);
    
    XQueryPointer(xDisplay, xBoardWindow,
		  &root, &child, &x, &y,
		  &win_x, &win_y, &mask);
    
    XtSetArg(args[0], XtNx, x - 10);
    XtSetArg(args[1], XtNy, y - 10);
    XtSetValues(popup, args, 2);
    
    XtPopup(popup, XtGrabExclusive);
    filenameUp = True;
    
    XtSetKeyboardFocus(shellWidget, popup);
}

void FileNameCallback(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    String name;
    Arg args[1];
    
    XtSetArg(args[0], XtNlabel, &name);
    XtGetValues(w, args, 1);
    
    if (strcmp(name, "cancel") == 0) {
	XtPopdown(w = XtParent(XtParent(w)));
	XtDestroyWidget(w);
	filenameUp = False;
	ModeHighlight();
	return;
    }
    
    FileNameAction(w, NULL, NULL, NULL);
}

void FileNameAction(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    char buf[MSG_SIZ];
    String name;

    name = XawDialogGetValueString(w = XtParent(w));
    
    if ((name != NULL) && (*name != NULLCHAR)) {
	strcpy(buf, name);
	XtPopdown(w = XtParent(w));
	XtDestroyWidget(w);
	filenameUp = False;
	(void) (*fileProc)(buf); /* I can't see a way not
				    to use a global here */
	ModeHighlight();
	return;
    }
    
    XtPopdown(w = XtParent(w));
    XtDestroyWidget(w);
    filenameUp = False;
    ModeHighlight();
}

typedef struct {
    ChessSquare piece;
    int to_x, to_y;
} PromotionMoveInfo;

static PromotionMoveInfo pmi;  /*making this global is gross */

void PromotionPopUp(piece, to_x, to_y)
     ChessSquare piece;
     int to_x, to_y;
{
    Arg args[2];
    Widget dialog;
    Position x, y;
    Dimension bw_width, bw_height, pw_width, pw_height;
    
    pmi.piece = piece;
    pmi.to_x = to_x;
    pmi.to_y = to_y;
    
    XtSetArg(args[0], XtNwidth, &bw_width);
    XtSetArg(args[1], XtNheight, &bw_height);
    XtGetValues(boardWidget, args, 2);
    
    XtSetArg(args[0], XtNresizable, True);
    promotionShell =
      XtCreatePopupShell("Promotion", transientShellWidgetClass,
			 commandsWidget, args, 1);
    
    XtSetArg(args[0], XtNlabel, "Promote pawn to what?");
    dialog = XtCreateManagedWidget("promotion", dialogWidgetClass,
				   promotionShell, args, 1);
    
    XawDialogAddButton(dialog, "Queen", PromotionCallback, 
		       (XtPointer) dialog);
    XawDialogAddButton(dialog, "Rook", PromotionCallback, 
		       (XtPointer) dialog);
    XawDialogAddButton(dialog, "Bishop", PromotionCallback, 
		       (XtPointer) dialog);
    XawDialogAddButton(dialog, "Knight", PromotionCallback, 
		       (XtPointer) dialog);
    XawDialogAddButton(dialog, "cancel", PromotionCallback, 
		       (XtPointer) dialog);
    
    XtRealizeWidget(promotionShell);
    
    XtSetArg(args[0], XtNwidth, &pw_width);
    XtSetArg(args[1], XtNheight, &pw_height);
    XtGetValues(promotionShell, args, 2);
    
    XtTranslateCoords(boardWidget, (bw_width - pw_width) / 2,
		      LINE_GAP + squareSize/3 +
		      ((piece == WhitePawn) ^ (flipView) ?
		       0 : 6*(squareSize + LINE_GAP)),
		      &x, &y);
    
    XtSetArg(args[0], XtNx, x);
    XtSetArg(args[1], XtNy, y);
    XtSetValues(promotionShell, args, 2);
    
    XtPopup(promotionShell, XtGrabNone);
    
    promotionUp = True;
}

void PromotionCallback(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    String name;
    Arg args[1];
    ChessMove move_type;
    
    XtSetArg(args[0], XtNlabel, &name);
    XtGetValues(w, args, 1);
    
    XtPopdown(w = XtParent(XtParent(w)));
    XtDestroyWidget(w);
    promotionUp = False;
    
    if (fromX == -1) return;
    
    if (strcmp(name, "Queen") == 0) {
	if (pmi.piece == WhitePawn)
	  move_type = WhitePromotionQueen;
	else
	  move_type = BlackPromotionQueen;
    } else if (strcmp(name, "Rook") == 0) {
	if (pmi.piece == WhitePawn)
	  move_type = WhitePromotionRook;
	else
	  move_type = BlackPromotionRook;
    } else if (strcmp(name, "Bishop") == 0) {
	if (pmi.piece == WhitePawn)
	  move_type = WhitePromotionBishop;
	else
	  move_type = BlackPromotionBishop;
    } else if (strcmp(name, "Knight") == 0) {
	if (pmi.piece == WhitePawn)
	  move_type = WhitePromotionKnight;
	else
	  move_type = BlackPromotionKnight;
    } else /* strcmp(name, "cancel") == 0 */ {
	fromX = fromY = -1;
	return;
    }
    
    MakeMove(&move_type, fromX, fromY, pmi.to_x, pmi.to_y);
    FinishUserMove(move_type, pmi.to_x, pmi.to_y);
}

void SelectCommand(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    XawListReturnStruct *list_return = XawListShowCurrent(w);
    
    fromX = fromY = -1;
    
    if (promotionUp) {
	XtPopdown(promotionShell);
	XtDestroyWidget(promotionShell);
	promotionUp = False;
    }
    
    (*buttonProcs[list_return->list_index])(w, NULL, NULL, NULL);
    
    if (!filenameUp) ModeHighlight();
}

void HighlightProcButton(proc)
     XtActionProc proc;
{
    int i = 0;
    
    if (proc == NULL) {
	XawListUnhighlight(commandsWidget);
	return;
    }
    
    for (;;) {
	if (buttonProcs[i] == NULL) {
	    XawListUnhighlight(commandsWidget);
	    return;
	}
	if (buttonProcs[i] == proc) {
	    XawListHighlight(commandsWidget, i);
	    return;
	}
	i++;
    }
}

void ModeHighlight()
{
    switch (gameMode) {
      case BeginningOfGame:
	if (appData.icsActive)
	  HighlightProcButton(NULL);
	else if (appData.noChessProgram)
	  HighlightProcButton(ForceProc);
	else
	  HighlightProcButton(MachineBlackProc);
	break;
      case MachinePlaysBlack:
	HighlightProcButton(MachineBlackProc);
	break;
      case MachinePlaysWhite:
	HighlightProcButton(MachineWhiteProc);
	break;
      case TwoMachinesPlay:
	HighlightProcButton(TwoMachinesProc);
	break;
      case ForceMoves:
	if (appData.icsActive)
	  HighlightProcButton(NULL);
	else
	  HighlightProcButton(ForceProc);
	break;
      case PlayFromGameFile:
	HighlightProcButton(LoadGameProc);
	break;
      case PauseGame:
	HighlightProcButton(PauseProc);
	break;
      case EditPosition:
	HighlightProcButton(EditPositionProc);
	break;
      case EndOfGame:
      default:
	HighlightProcButton(NULL);
	break;
    }
}

/*
 * Button procedures
 */
void QuitProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    /* Save game if resource set and not already saved */
    if (*endMessage == NULLCHAR) {
	if (appData.saveGameFile[0] != NULLCHAR) 
	  SaveGame(appData.saveGameFile);
	if (appData.savePositionFile[0] != NULLCHAR) 
	  SavePosition(appData.savePositionFile);
    }
    ShutdownChessPrograms(NULL);
    if (telnetPID != 0) {
	if (kill(telnetPID, SIGTERM) == 0)
	  wait((union wait *) 0);
    }
    exit(0);
}

void AcceptMatchProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    /* Accept most recent ICS match offer */
    if (appData.icsActive && ics_match_offer[0] != NULLCHAR) {
	SendToICS(ics_match_offer);
	ics_match_offer[0] = NULLCHAR;
    }
}

void DrawProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    /* Offer draw or accept pending draw offer from opponent */
    
    if (appData.icsActive) {
	/* Note: tournament rules require draw offers to be
	   made after you make your move but before you punch
	   your clock.  Currently ICS doesn't let you do that;
	   instead, you always punch your clock after making a
	   move, but you can offer a draw at any time. */
	
	SendToICS("draw\n");
    } else {
	/* Currently GNU Chess doesn't offer or accept draws
	   at all, so there is no Draw button in GNU Chess
	   mode.  */
	
	fprintf(stderr, "Draw function not implemented\n");
    }
}


void DeclineDrawProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    /* Decline a pending draw offer from opponent */
    
    if (appData.icsActive) {
	/* Note: ICS also lets you withdraw your own draw
	   offer with this command.  I'm not sure how long
	   your draw offer remains pending if you don't
	   withdraw it. */
	
	SendToICS("decline draw\n");
    } else {
	/* Currently GNU Chess doesn't offer or accept draws
	   at all, so there is no Decline Draw button in
	   GNU Chess mode.  */
	
	fprintf(stderr, "Decline Draw function not implemented\n");
    }
}


void ResignProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    /* Resign.  You can do this even if it's not your turn. */
    
    if (appData.icsActive) {
	SendToICS("resign\n");
    } else {
	/* This button is not currently used in GNU Chess mode,
	   but it should work. */
	
	switch (gameMode) {
	  case MachinePlaysWhite:
	    ShutdownChessPrograms("Black resigns");
	    break;
	  case MachinePlaysBlack:
	    ShutdownChessPrograms("White resigns");
	    break;
	  default:
	    break;
	}
    }
}


Boolean LoadGame(name)
     char *name;
{
    char buf[MSG_SIZ], lname[MSG_SIZ];
    ChessMove cm;
    int gameNumber;
    char *p;
    
    if (gameMode != BeginningOfGame) {
	DisplayMessage("Press Reset first");
	return False;
    }
    
    /* Parse game number if given */
    strcpy(lname, name);
    p = strrchr(lname, ' ');
    if (p == NULL) {
	gameNumber = 1;
    } else {
	*p++ = NULLCHAR;
	gameNumber = atoi(p);
    }

    yynewfile();
    if ((gameFileFP = fopen(lname, "r")) == NULL) {
	sprintf(buf, "Can't open %s", lname);
	DisplayMessage(buf);
	return False;
    }
    
    lastGameMode = gameMode = PlayFromGameFile;
    ModeHighlight();
    InitPosition();
    DisplayClocks(StopTimers);
    if (firstProgramXID == 0) {
	InitChessProgram(appData.firstHost, appData.firstChessProgram,
			 &firstProgramPID, &toFirstProgFP,
			 &fromFirstProgFP, &firstProgramXID,
			 &firstSendTime);
    } else {
	SendToProgram(appData.initString, toFirstProgFP);
	SendSearchDepth(toFirstProgFP);
    }
    SendToProgram("force\n", toFirstProgFP);
    
    /*
     * Skip the first gameNumber-1 games in the file.
     * Also skip over anything that precedes an identifiable 
     * StartGame marker, to avoid being confused by 
     * garbage at the start of the file.  Currently 
     * recognized StartGame markers are the move number "1",
     * the pattern "gnuchess .* game", or a position diagram.
     */
    while (gameNumber > 0) {
	yyboardindex = forwardMostMove;
	cm = (ChessMove) yylex();
	if (cm == (ChessMove) 0) {
	    ResetProc(NULL, NULL, NULL, NULL);
	    DisplayMessage("Game not found in file");
	    return False;
	}
	if (cm == StartGame || cm == PositionDiagram)
	  gameNumber--;
    }
    
    /* If the file starts with a position diagram,
       set up the position */
    if (cm == PositionDiagram) {
	Board initial_position;
	int i, j;
	char *p = yytext;
	
	startedFromSetupPosition = True;
	
	for (i = BOARD_SIZE - 1; i >= 0; i--)
	  for (j = 0; j < BOARD_SIZE; p++)
	    switch (*p) {
	      case '[':
	      case '-':
	      case ' ':
	      case '\t':
	      case '\n':
		break;
	      default:
		initial_position[i][j++] = CharToPiece(*p);
		break;
	    }
	
	while (*p == ' ' || *p == '\t' || *p == '\n') p++;
	
	if (strncmp(p, "black", strlen("black"))==0)
	  blackPlaysFirst = True;
	else
	  blackPlaysFirst = False;
	
	if (blackPlaysFirst) {
	    CopyBoard(boards[0], initial_position);
	    strcpy(moveList[0], " ...");
	    strcpy(parseList[0], " ...");
	    currentMove = forwardMostMove = backwardMostMove = 1;
	    CopyBoard(boards[1], initial_position);
	    SendToProgram("a3\n", toFirstProgFP);
	    SendCurrentBoard(toFirstProgFP);
	} else {
	    currentMove = forwardMostMove = backwardMostMove = 0;
	    CopyBoard(boards[0], initial_position);
	    SendCurrentBoard(toFirstProgFP);
	}
	
	DrawPosition(boardWidget, NULL, NULL, NULL);
	DisplayClocks(ReDisplayTimers);
    }
    
    LoadGameLoop();
    
    return True;
}

void ResurrectChessProgram()
     /* Get out of EndOfGame mode.  This may require us to restart the
	chess program and feed it all the moves made so far. */
{
    char buf[MSG_SIZ];
    int i;
    
    /* assert(gameMode == EndOfGame) */
    
    gameMode = lastGameMode = ForceMoves;
    ModeHighlight();
    
    if (firstProgramPID != 0) return;
    
    InitChessProgram(appData.firstHost, appData.firstChessProgram,
		     &firstProgramPID, &toFirstProgFP, &fromFirstProgFP,
		     &firstProgramXID, &firstSendTime);
    SendToProgram("force\n", toFirstProgFP);
    
    if (startedFromSetupPosition) {
	if (backwardMostMove % 2 == 1)
	  SendToProgram("a3\n", toFirstProgFP);
	SendBoard(toFirstProgFP, boards[backwardMostMove]);
    }
    
    for (i = backwardMostMove; i < currentMove; i++) {
	strcpy(buf, moveList[i]);
	SendToProgram(buf, toFirstProgFP);
    }
    
    if (!firstSendTime) {
	/* can't tell gnuchess what its clock should read,
	   so we bow to its notion. */
	DisplayClocks(ResetTimers);
    }
}

void MachineBlackProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode == PauseGame) PauseProc(w, event, prms, nprms);
    if (gameMode == EditPosition) EditPositionDone();
    
    if ((gameMode == EndOfGame) || (gameMode == PlayFromGameFile)
	|| (gameMode == TwoMachinesPlay) || appData.noChessProgram
	|| (gameMode == MachinePlaysBlack))
      return;
    
    if (WhiteOnMove(gameMode == ForceMoves ?
		    currentMove : forwardMostMove)) {
	DisplayMessage("It is not Black's turn");
	return;
    }
    
    if (gameMode == ForceMoves) forwardMostMove = currentMove;
    
    lastGameMode = gameMode = MachinePlaysBlack;
    ModeHighlight();
    SendToProgram(appData.blackString, toFirstProgFP);
    DisplayClocks(StartTimers);
}

void ForwardProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    char buf[MSG_SIZ];
    int target;
    unsigned int state;
    
    if ((gameMode == EndOfGame) || (gameMode == EditPosition))
      return;
    
    if (gameMode == PlayFromGameFile)
      PauseProc(w, event, prms, nprms);
    
    if (currentMove >= forwardMostMove) {
	if (gameFileFP != NULL)
	  (void) LoadGameOneMove();
	return;
    }
    
    if (event == NULL) {
	/* Kludge */
	Window root, child;
	int root_x, root_y;
	int win_x, win_y;
	XQueryPointer(xDisplay, xBoardWindow,
		      &root, &child, &root_x, &root_y,
		      &win_x, &win_y, &state);
    } else {
	state = event->xkey.state;
    }
    
    if (state & ShiftMask)
      target = forwardMostMove;
    else
      target = currentMove + 1;
    
    if (gameMode == ForceMoves) {
	while (currentMove < target) {
	    strcpy(buf, moveList[currentMove++]);
	    SendToProgram(buf, toFirstProgFP);
	}
    } else {
	currentMove = target;
    }
    
    DisplayClocks(ReDisplayTimers);
    DisplayMove(currentMove - 1);
    DrawPosition(boardWidget, NULL, NULL, NULL);
}


void ResetProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    flipView = False;
    startedFromSetupPosition = blackPlaysFirst = False;
    matchMode = MatchFalse;
    firstMove = True;
    whiteFlag = blackFlag = False;
    maybeThinking = False;
    endMessage[0] = NULLCHAR;
    ics_white[0] = ics_black[0] = NULLCHAR;
    ics_user_moved = ics_getting_history = False;
    ics_mode = IcsIdle;
    ics_gamenum = -1;
    
    ShutdownChessPrograms(NULL);
    lastGameMode = gameMode = BeginningOfGame;
    ModeHighlight();
    InitPosition();
    DisplayClocks(ResetTimers);
    InitChessProgram(appData.firstHost, appData.firstChessProgram,
		     &firstProgramPID, &toFirstProgFP,
		     &fromFirstProgFP, &firstProgramXID, &firstSendTime);
    DisplayTitle("");
    DisplayMessage("");
    if (commentUp) CommentPopDown();
    if (promotionUp) {
	XtPopdown(promotionShell);
	XtDestroyWidget(promotionShell);
	promotionUp = False;
    }
}

void LoadPositionProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode != BeginningOfGame) {
	DisplayMessage("Press Reset first");
	return;
    }
    FileNamePopUp("Position file name?", LoadPosition);
}

Boolean LoadPosition(name)
     char *name;
{
    char *p, line[MSG_SIZ], buf[MSG_SIZ];
    Board initial_position;
    FILE *fp;
    int i, j, positionNumber;
    
    if (gameMode != BeginningOfGame) {
	DisplayMessage("Press Reset first");
	return False;
    }
    
    /* Parse position number if given */
    strcpy(line, name);
    p = strrchr(line, ' ');
    if (p == NULL) {
	positionNumber = 0;
    } else {
	*p++ = NULLCHAR;
	positionNumber = atoi(p);
    }

    if ((fp = fopen(line, "r")) == NULL) {
	sprintf(buf, "Can't open %s", line);
	DisplayMessage(buf);
	return False;
    }
    
    lastGameMode = gameMode = ForceMoves;
    ModeHighlight();
    startedFromSetupPosition = True;
    
    if (firstProgramXID == 0)
      InitChessProgram(appData.firstHost, appData.firstChessProgram,
		       &firstProgramPID, &toFirstProgFP,
		       &fromFirstProgFP, &firstProgramXID, &firstSendTime);
    
    if (positionNumber == 0) {
	/* Backward compatibility---don't look for '#' */
	(void) fgets(line, MSG_SIZ, fp);
    } else {
	while (positionNumber > 0) {
	    /* skip postions before number positionNumber */
	    if (fgets(line, MSG_SIZ, fp) == NULL) {
		ResetProc(NULL, NULL, NULL, NULL);
		DisplayMessage("Position not found in file");
		return False;
	    }
	    if (line[0] == '#') positionNumber--;
	}
    }

    (void) fgets(line, MSG_SIZ, fp);
    (void) fgets(line, MSG_SIZ, fp);
    
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	(void) fgets(line, MSG_SIZ, fp);
	for (p = line, j = 0; j < BOARD_SIZE; p++) {
	    if (*p == ' ')
	      continue;
	    initial_position[i][j++] = CharToPiece(*p);
	}
    }
    
    blackPlaysFirst = False;
    if (!feof(fp)) {
	(void) fgets(line, MSG_SIZ, fp);
	if (strncmp(line, "black", strlen("black"))==0)
	  blackPlaysFirst = True;
    }
    fclose(fp);
    
    if (blackPlaysFirst) {
	CopyBoard(boards[0], initial_position);
	strcpy(moveList[0], " ...");
	strcpy(parseList[0], " ...");
	currentMove = forwardMostMove = backwardMostMove = 1;
	CopyBoard(boards[1], initial_position);
	SendToProgram("force\na3\n", toFirstProgFP);
	SendCurrentBoard(toFirstProgFP);
	DisplayMessage("Black to play");
    } else {
	currentMove = forwardMostMove = backwardMostMove = 0;
	CopyBoard(boards[0], initial_position);
	SendCurrentBoard(toFirstProgFP);
	SendToProgram("force\n", toFirstProgFP);
	DisplayMessage("White to play");
    }
    
    DisplayClocks(ResetTimers);
    DrawPosition(boardWidget, NULL, NULL, NULL);
    
    return (int) True;
}

void EditPositionProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode == EditPosition) {
	ForceProc(w, event, prms, nprms);
	return;
    }
    
    ForceProc(w, event, prms, nprms);
    if (gameMode != ForceMoves) return;
    
    DisplayTitle("<-- Press to set side to play next");
    DisplayMessage("Mouse: 1=drag, 2=white, 3=black");
    
    lastGameMode = gameMode = EditPosition;
    ModeHighlight();
    if (currentMove > 0)
      CopyBoard(boards[0], boards[currentMove]);
    
    blackPlaysFirst = !WhiteOnMove(currentMove);
    currentMove = forwardMostMove = backwardMostMove = 0;
}

void EditPositionDone()
{
    startedFromSetupPosition = True;
    SendToProgram(appData.initString, toFirstProgFP);
    SendSearchDepth(toFirstProgFP);
    if (blackPlaysFirst) {
	strcpy(moveList[0], " ...");
	strcpy(parseList[0], " ...");
	currentMove = forwardMostMove = backwardMostMove = 1;
	CopyBoard(boards[1], boards[0]);
	SendToProgram("force\na3\n", toFirstProgFP);
	SendCurrentBoard(toFirstProgFP);
	DisplayTitle("");
	DisplayMessage("Black to play");
    } else {
	currentMove = forwardMostMove = backwardMostMove = 0;
	SendCurrentBoard(toFirstProgFP);
	SendToProgram("force\n", toFirstProgFP);
	DisplayTitle("");
	DisplayMessage("White to play");
    }
    lastGameMode = gameMode = ForceMoves;
}

void MachineWhiteProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode == PauseGame) PauseProc(w, event, prms, nprms);
    if (gameMode == EditPosition) EditPositionDone();
    
    if ((gameMode == EndOfGame) || (gameMode == PlayFromGameFile)
	|| (gameMode == TwoMachinesPlay) || appData.noChessProgram
	|| (gameMode == MachinePlaysWhite))
      return;
    
    if (!WhiteOnMove(gameMode == ForceMoves ?
		     currentMove : forwardMostMove)) {
	DisplayMessage("It is not White's turn");
	return;
    }
    
    if (gameMode == ForceMoves) forwardMostMove = currentMove;
    
    lastGameMode = gameMode = MachinePlaysWhite;
    ModeHighlight();
    SendToProgram(appData.whiteString, toFirstProgFP);
    DisplayClocks(StartTimers);
}

void BackwardProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    int target;
    unsigned int state;
    
    if ((currentMove <= backwardMostMove) || (gameMode == EditPosition))
      return;
    
    if (gameMode == EndOfGame)
      ResurrectChessProgram();
    
    if (gameMode == PlayFromGameFile)
      PauseProc(w, event, prms, nprms);
    
    if (event == NULL) {
	/* Kludge */
	Window root, child;
	int root_x, root_y;
	int win_x, win_y;
	XQueryPointer(xDisplay, xBoardWindow,
		      &root, &child, &root_x, &root_y,
		      &win_x, &win_y, &state);
    } else {
	state = event->xkey.state;
    }
    if (state & ShiftMask)
      target = backwardMostMove;
    else
      target = currentMove - 1;
    
    if (gameMode == ForceMoves) {
	Attention(firstProgramPID);
	while (currentMove > target) {
	    SendToProgram("undo\n", toFirstProgFP);
	    currentMove--;
	}
    } else {
	currentMove = target;
    }
    
    DisplayClocks(ReDisplayTimers);
    DisplayMove(currentMove - 1);
    DrawPosition(boardWidget, NULL, NULL, NULL);
}

void FlipViewProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    flipView = !flipView;
    DrawPosition(boardWidget, NULL, NULL, NULL);
}

void SaveGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    FileNamePopUp("Filename for saved game?", SaveGame);
}

Boolean SaveGame(name)
     char *name;
{
    char buf[MSG_SIZ];
    int i;
    time_t tm;
    
    if ((gameFileFP = fopen(name, "a")) == NULL) {
	sprintf(buf, "Can't open %s", name);
	DisplayMessage(buf);
	return False;
    }
    
    tm = time((time_t *) NULL);
    
    fprintf(gameFileFP, "# %s game file -- %s", programName, ctime(&tm));
    PrintOpponents(gameFileFP);
    fprintf(gameFileFP, "\talgebraic\n");
    
    if (backwardMostMove > 0 || startedFromSetupPosition) {
	fprintf(gameFileFP, "[--------------\n");
	PrintPosition(gameFileFP, backwardMostMove);
	fprintf(gameFileFP, "--------------]\n");
    }
    i = backwardMostMove;
    if ((i % 2) == 1) {
	i--;
	strcpy(parseList[i], "...");
    }
    while (i < forwardMostMove) {
	fprintf(gameFileFP, "%d. %s  ", i/2 + 1, parseList[i++]);
	if (i >= forwardMostMove) {
	    fprintf(gameFileFP, "\n");
	    break;
	}
	fprintf(gameFileFP, "%s\n", parseList[i++]);
    }
    
    if (endMessage[0] != NULLCHAR)
      fprintf(gameFileFP, "%s\n", endMessage);
    
    fclose(gameFileFP);
    gameFileFP = NULL;
    yynewfile();
    return True;
}

#ifdef notdef  
/* currently unused */
void SwitchProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (appData.noChessProgram) return;
    if (gameMode == PauseGame) PauseProc(w, event);
    switch (gameMode) {
      default:
	return;
      case MachinePlaysWhite:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayMessage("Wait until your turn");
	    return;
	}
	lastGameMode = gameMode = MachinePlaysBlack;
	ModeHighlight();
	break;
      case BeginningOfGame:
      case MachinePlaysBlack:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayMessage("Wait until your turn");
	    return;
	}
	if (forwardMostMove == 0) {
	    MachineWhiteProc(w, event);
	    return;
	}
	lastGameMode = gameMode = MachinePlaysWhite;
	ModeHighlight();
	break;
    }
    
    Attention(firstProgramPID);
    SendToProgram("switch\n", toFirstProgFP);
}
#endif /*notdef*/

void ForceProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    int gnuMove;
    
    if (gameMode == PauseGame) PauseProc(w, event, prms, nprms);
    switch (gameMode) {
      case MachinePlaysWhite:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayMessage("Wait until your turn");
	    return;
	}
	Attention(firstProgramPID);
	SendToProgram("force\n", toFirstProgFP);
	break;
      case MachinePlaysBlack:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayMessage("Wait until your turn");
	    return;
	}
	Attention(firstProgramPID);
	SendToProgram("force\n", toFirstProgFP);
	break;
      case BeginningOfGame:
	SendToProgram("force\n", toFirstProgFP);
	break;
      case PlayFromGameFile:
	if (readGameXID != 0) {
	    XtRemoveTimeOut(readGameXID);
	    readGameXID = 0;
	}
	if (gameFileFP != NULL) {
	    fclose(gameFileFP);
	    gameFileFP = NULL;
	    yynewfile();
	}
	break;
      case EndOfGame:
	ResurrectChessProgram();
	return;
      case EditPosition:
	EditPositionDone();
	break;
      case TwoMachinesPlay:
	ShutdownChessPrograms("");
	ResurrectChessProgram();
	return;
      default:
	return;
    }
    
    if (gameMode == MachinePlaysWhite ||
	gameMode == MachinePlaysBlack ||
	gameMode == TwoMachinesPlay ||
	gameMode == PlayFromGameFile) {
	gnuMove = forwardMostMove;
	while (gnuMove > currentMove) {
	    SendToProgram("undo\n", toFirstProgFP);
	    gnuMove--;
	}
    }		
    
    lastGameMode = gameMode = ForceMoves;
    ModeHighlight();
    
    DisplayClocks(StopTimers);
}

void NothingProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    return;
}

void HintProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (appData.noChessProgram) return;
    switch (gameMode) {
      case MachinePlaysWhite:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayMessage("Wait until your turn");
	    return;
	}
	break;
      case BeginningOfGame:
      case MachinePlaysBlack:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayMessage("Wait until your turn");
	    return;
	}
	break;
      default:
	DisplayMessage("No hint available");
	return;
    }
    Attention(firstProgramPID);
    SendToProgram("hint\n", toFirstProgFP);
}

void PrintPosition(fp, move)
     FILE *fp;
     int move;
{
    int i, j;
    
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	for (j = 0; j < BOARD_SIZE; j++) {
	    fprintf(fp, "%c", PieceToChar(boards[move][i][j]));
	    fputc(j == BOARD_SIZE - 1 ? '\n' : ' ', fp);
	}
    }
    if ((gameMode == EditPosition) ? !blackPlaysFirst : (move % 2 == 0))
      fprintf(fp, "white to play\n");
    else
      fprintf(fp, "black to play\n");
}

void PrintOpponents(fp)
     FILE *fp;
{
    char host_name[MSG_SIZ];
    
    gethostname(host_name, MSG_SIZ);
    switch (lastGameMode) {
      case MachinePlaysWhite:
	fprintf(fp, "\t%s@%s vs. %s@%s\n", appData.firstChessProgram,
		appData.firstHost, getpwuid(getuid())->pw_name,
		host_name);
	break;
      case MachinePlaysBlack:
	fprintf(fp, "\t%s@%s vs. %s@%s\n", getpwuid(getuid())->pw_name,
		host_name, appData.firstChessProgram,
		appData.firstHost);
	break;
      case TwoMachinesPlay:
	fprintf(fp, "\t%s@%s vs. %s@%s\n", appData.secondChessProgram,
		appData.secondHost, appData.firstChessProgram,
		appData.firstHost);
	break;
      default:
	if (appData.icsActive && ics_white[0] != NULLCHAR) {
	    fprintf(fp, "\t%s vs. %s\n",
		    ics_white, ics_black);
	} else {
	    fprintf(fp, "\n");
	}
	break;
    }
}

void SavePositionProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    FileNamePopUp("Filename for saved position?", SavePosition);
}

Boolean SavePosition(name)
     char *name;
{
    char buf[MSG_SIZ];
    FILE *fp;
    time_t tm;
    
    if ((fp = fopen(name, "a")) == NULL) {
	sprintf(buf, "Can't open %s", name);
	DisplayMessage(buf);
	return False;
    }
    
    tm = time((time_t *) NULL);
    
    fprintf(fp, "# %s position file -- %s", programName, ctime(&tm));
    PrintOpponents(fp);
    fprintf(fp, "\n");
    PrintPosition(fp, currentMove);
    fclose(fp);
    return True;
}

void TwoMachinesProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    int i;
    MatchMode matchKind;
    
    if (gameMode == PauseGame) PauseProc(w, event, prms, nprms);
    if ((gameMode == EndOfGame) || (gameMode == TwoMachinesPlay)
	|| appData.noChessProgram)
      return;
    
    if (matchMode == MatchFalse) {
	switch (gameMode) {
	  case PauseGame:
	  case PlayFromGameFile:
	    return;
	  case MachinePlaysWhite:
	  case MachinePlaysBlack:
	    ForceProc(w, event, prms, nprms);
	    if (gameMode != ForceMoves) return;
	    matchKind = MatchOpening;
	    break;
	  case ForceMoves:
	    matchKind = MatchOpening;
	    break;
	  case EditPosition:
	    EditPositionDone();
	    matchKind = MatchPosition;
	    break;
	  case BeginningOfGame:
	  default:
	    matchKind = MatchInit;
	    break;
	}
    } else {
	matchKind = matchMode;
    }
    
    forwardMostMove = currentMove;
    
    flipView = False;
    firstMove = False;
    
    switch (matchKind) {
      case MatchOpening:
	if (firstProgramXID == 0) {
	    if (*appData.loadGameFile == NULLCHAR) {
		DisplayMessage("You need to specify a game file");
		return;
	    }
	    InitChessProgram(appData.firstHost,
			     appData.firstChessProgram,
			     &firstProgramPID, &toFirstProgFP,
			     &fromFirstProgFP, &firstProgramXID,
			     &firstSendTime);
	    if (!LoadGame(appData.loadGameFile)) {
		ShutdownChessPrograms("Bad game file");
		return;
	    }
	    DrawPosition(boardWidget, NULL, NULL, NULL);
	}
	InitChessProgram(appData.secondHost,
			 appData.secondChessProgram,
			 &secondProgramPID, &toSecondProgFP,
			 &fromSecondProgFP, &secondProgramXID,
			 &secondSendTime);
	if (startedFromSetupPosition) {
	    if (blackPlaysFirst) {
		SendToProgram("force\na3\n", toSecondProgFP);
		SendBoard(toSecondProgFP,
			  boards[backwardMostMove]);
	    } else {
		SendBoard(toSecondProgFP,
			  boards[backwardMostMove]);
		SendToProgram("force\n", toSecondProgFP);
	    }
	} else {
	    SendToProgram("force\n", toSecondProgFP);
	}
	for (i = backwardMostMove; i < forwardMostMove; i++)
	  SendToProgram(moveList[i], toSecondProgFP);
	lastGameMode = gameMode = TwoMachinesPlay;
	ModeHighlight();
	firstMove = True;
	if (WhiteOnMove(forwardMostMove))
	  SendToProgram(appData.whiteString, toSecondProgFP);
	else
	  SendToProgram(appData.blackString, toFirstProgFP);
	break;
	
      case MatchPosition:
	if (firstProgramXID == 0) {
	    if (*appData.loadPositionFile == NULLCHAR) {
		DisplayMessage("You need to specify a position file");
		return;
	    }
	    InitChessProgram(appData.firstHost,
			     appData.firstChessProgram,
			     &firstProgramPID, &toFirstProgFP,
			     &fromFirstProgFP, &firstProgramXID,
			     &firstSendTime);
	    if (!LoadPosition(appData.loadPositionFile))
	      return;
	}
	InitChessProgram(appData.secondHost,
			 appData.secondChessProgram,
			 &secondProgramPID, &toSecondProgFP,
			 &fromSecondProgFP, &secondProgramXID,
			 &secondSendTime);
	if (blackPlaysFirst)
	  SendToProgram("force\na3\n", toSecondProgFP);
	SendCurrentBoard(toSecondProgFP);
	lastGameMode = gameMode = TwoMachinesPlay;
	ModeHighlight();
	firstMove = True;
	if (WhiteOnMove(forwardMostMove))
	  SendToProgram(appData.whiteString, toSecondProgFP);
	else
	  SendToProgram(appData.blackString, toFirstProgFP);
	break;
	
      case MatchInit:
	InitPosition();
	if (firstProgramXID == 0)
	  InitChessProgram(appData.firstHost,
			   appData.firstChessProgram,
			   &firstProgramPID, &toFirstProgFP,
			   &fromFirstProgFP, &firstProgramXID,
			   &firstSendTime);
	InitChessProgram(appData.secondHost,
			 appData.secondChessProgram,
			 &secondProgramPID, &toSecondProgFP,
			 &fromSecondProgFP, &secondProgramXID,
			 &secondSendTime);
	lastGameMode = gameMode = TwoMachinesPlay;
	ModeHighlight();
	SendToProgram(appData.whiteString, toSecondProgFP);
	
      default:
	break;
    }
    
    if (!firstSendTime || !secondSendTime)
      DisplayClocks(ResetTimers);
    DisplayClocks(StartTimers);
}

void PauseProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    switch (gameMode) {
      case EndOfGame:
      case EditPosition:
      default:
	return;
      case ForceMoves:
	if (appData.icsActive) {
	    pausePreviousMode = gameMode;
	    gameMode = PauseGame;
	    ModeHighlight();
	}
	return;
      case PauseGame:
	gameMode = pausePreviousMode;
	ModeHighlight();
	pausePreviousMode = PauseGame;
	if (gameMode == MachinePlaysWhite ||
	    gameMode == MachinePlaysBlack) {
	    DisplayClocks(StartTimers);
	} else {
	    DisplayClocks(ReDisplayTimers);
	}
	if (gameMode == PlayFromGameFile &&
	    readGameXID == 0 &&
	    appData.timeDelay >= 0) {
	    readGameXID =
	      XtAppAddTimeOut(appContext,
			      (int) (1000 * appData.timeDelay),
			      (XtTimerCallbackProc) LoadGameLoop, NULL);
	}
	break;
      case PlayFromGameFile:
	if (readGameXID != 0) {
	    XtRemoveTimeOut(readGameXID);
	    readGameXID = 0;
	}
	pausePreviousMode = gameMode;
	gameMode = PauseGame;
	ModeHighlight();
	break;
      case BeginningOfGame:
      case MachinePlaysWhite:
      case MachinePlaysBlack:
      case TwoMachinesPlay:
	if (forwardMostMove == 0)
	  return;	/* don't pause if no one has moved */
	if ((gameMode == MachinePlaysWhite &&
	     !WhiteOnMove(forwardMostMove)) ||
	    (gameMode == MachinePlaysBlack &&
	     WhiteOnMove(forwardMostMove))) {
	    DisplayClocks(StopTimers);
	}
	pausePreviousMode = gameMode;
	gameMode = PauseGame;
	ModeHighlight();
	break;
    }
}

void LoadGameProc(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    if (gameMode == PlayFromGameFile) {
	ForceProc(w, event, prms, nprms);
	return;
    }
    if (gameMode != BeginningOfGame) {
	DisplayMessage("Press Reset first");
	return;
    }
    FileNamePopUp("Game file name?", LoadGame);
}

void Iconify(w, event, prms, nprms)
     Widget w;
     XEvent *event;
     String *prms;
     Cardinal *nprms;
{
    Arg args[1];
    
    fromX = fromY = -1;
    
    XtSetArg(args[0], XtNiconic, True);
    XtSetValues(shellWidget, args, 1);
}

void SendToProgram(message, fp)
     char *message;
     FILE *fp;
{
    if (fp == NULL) return;
    lastMsgFP = fp;
    
    if (appData.debugMode)
      fprintf(stderr, "Sending to %s: %s\n",
	      fp == toFirstProgFP ? "first" : "second", message);
    
    if (message[strlen(message) - 1] != '\n')
      fprintf(fp, "\n%s\n", message);
    else
      fputs(message, fp);
    fflush(fp);
}

void ReceiveFromProgram(fp, source, id)
     FILE *fp;
     int *source;
     XtInputId *id;
{
    char message[MSG_SIZ], *end_str;
    
    if (fgets(message, MSG_SIZ, fp) == NULL) {
	if (ferror(fp) == 0) {
	    sprintf(message,
		    "%s chess program (%s) exited unexpectedly",
		    fp == fromFirstProgFP ? "first" : "second",
		    fp == fromFirstProgFP ?
		    appData.firstChessProgram :
		    appData.secondChessProgram);
	    fprintf(stderr, "%s: %s\n", programName, message);
	} else {
	    sprintf(message,
		    "error reading from %s chess program (%s)",
		    fp == fromFirstProgFP ? "first" : "second",
		    fp == fromFirstProgFP ?
		    appData.firstChessProgram :
		    appData.secondChessProgram);
	    fprintf(stderr, "%s: %s: ", programName, message);
	    perror("");
	}
	ShutdownChessPrograms(message);
	return;
    }
    
    if ((end_str = strchr(message, '\r')) != NULL)
      *end_str = NULLCHAR;
    if ((end_str = strchr(message, '\n')) != NULL)
      *end_str = NULLCHAR;
    
    if (appData.debugMode)
      fprintf(stderr, "Received from %s: %s\n",
	      fp == fromFirstProgFP ? "first" : "second", message);
    HandleMachineMove(message, fp);
}

void SendSearchDepth(fp)
     FILE *fp;
{
    char message[MSG_SIZ];
    
    if (appData.searchDepth <= 0) return;
    
    sprintf(message, "depth\n%d\nhelp\n", appData.searchDepth);
    /* note kludge: "help" command forces gnuchessx to print
       out something that ends with a newline. */
    SendToProgram(message, fp);
}

void SendTimeRemaining(fp)
     FILE *fp;
{
    char message[MSG_SIZ];
    
    sprintf(message, "time %d\n", WhiteOnMove(forwardMostMove) ?
	    whiteTimeRemaining/10 : blackTimeRemaining/10);
    SendToProgram(message, fp);
}


void DisplayMove(moveNumber)
     int moveNumber;
{
    char message[MSG_SIZ];
    
    if (moveNumber < backwardMostMove) {
	if (moveNumber == forwardMostMove - 1)
	  DisplayMessage(endMessage);
	else
	  DisplayMessage("");
    } else {
	sprintf(message, "%d. %s%s  %s", moveNumber / 2 + 1,
		WhiteOnMove(moveNumber) ? "" : "... ",
		parseList[moveNumber],
		moveNumber == forwardMostMove - 1 ? endMessage : "");
	DisplayMessage(message);
    }
}

void DisplayMessage(message)
     char *message;
{
    Arg arg;
    
    XtSetArg(arg, XtNlabel, message);
    XtSetValues(messageWidget, &arg, 1);
}

void DisplayTitle(title)
     char *title;
{
    Arg arg;
    
    XtSetArg(arg, XtNlabel, title);
    XtSetValues(titleWidget, &arg, 1);
}

/*
 * This routine sends a SIGINT (^C interrupt) to gnuchess to awaken it
 * if it might be busy thinking on our time.  This normally isn't needed,
 * but is useful on systems where the FIONREAD ioctl doesn't work (such 
 * as ESIX), since on those systems the gnuchess feature that lets you 
 * interrupt its thinking just by typing a command does not work.
 *
 * In the future, similar code could be used to stop gnuchess and make
 * it move immediately when it is thinking about its own move; this could
 * be useful if we want to make Backward or ForceMoves work while gnuchess
 * is thinking. --t.mann
 */
void Attention(pid)
     int pid;
{
#if defined(ATTENTION) || defined(ESIX) || !defined(FIONREAD)
    if (appData.noChessProgram || (pid == 0)) return;
    switch (gameMode) {
      case MachinePlaysWhite:
      case MachinePlaysBlack:
      case TwoMachinesPlay:
	if (forwardMostMove > backwardMostMove + 1 && maybeThinking) {
	    if (appData.debugMode)
	      fprintf(stderr, "Sending SIGINT to %s\n",
		      pid == firstProgramPID ? "first" : "second");
	    (void) kill(pid, SIGINT); /* stop it thinking */
	}
	break;
    }
#endif /*ATTENTION*/
}

void CheckFlags()
{
    if (whiteTimeRemaining <= 0) {
	whiteTimeRemaining = 0;
	if (appData.icsActive) return;
	if (!whiteFlag) {
	    whiteFlag = True;
	    if (blackFlag)
	      DisplayTitle("  Both flags have fallen");
	    else
	      DisplayTitle("  White's flag has fallen");
	}
    }
    if (blackTimeRemaining <= 0) {
	blackTimeRemaining = 0;
	if (appData.icsActive) return;
	if (!blackFlag) {
	    blackFlag = True;
	    if (whiteFlag)
	      DisplayTitle("  Both flags have fallen");
	    else
	      DisplayTitle("  Black's flag has fallen");
	}
    }
}

void CheckTimeControl()
{
    if (!appData.clockMode) return;
    if (appData.icsActive) return;
    if (forwardMostMove == 0) return;
    /*
     * add time to clocks when time control is achieved
     */
    if ((forwardMostMove % (appData.movesPerSession * 2)) == 0) {
	
	if (whiteTimeRemaining > 0)
	  whiteTimeRemaining += timeControl;
	
	if (blackTimeRemaining > 0)
	  blackTimeRemaining += timeControl;
    }
}

void DisplayLabels()
{
    DisplayTimerLabel(whiteTimerWidget, "White",
		      whiteTimeRemaining);
    DisplayTimerLabel(blackTimerWidget, "Black",
		      blackTimeRemaining);
}

#ifdef HAS_GETTIMEOFDAY
static struct timeval tickStartTV;
static int tickLength;

int PartialTickLength()
{
    struct timeval tv;
    struct timezone tz;
    int ptl;
    
    gettimeofday(&tv, &tz);
    ptl = ( (tv.tv_sec - tickStartTV.tv_sec)*1000000 +
	   (tv.tv_usec - tickStartTV.tv_usec) + 500 ) / 1000;
    if (ptl > tickLength) ptl = tickLength;
    return ptl;
}
#else /*!HAS_GETTIMEOFDAY*/
#define tickLength 1000
#endif /*HAS_GETTIMEOFDAY*/

/*
 * DisplayClocks manages the game clocks.
 *
 * In tournament play, black starts the clock and then white makes a move.
 * We give the human user a slight advantage if he is playing white---the
 * clocks don't run until he makes his first move, so it takes zero time.
 * Also, DisplayClocks doesn't account for network lag so it could get
 * out of sync with GNU Chess's clock -- but then, referees are always right.
 */
void DisplayClocks(clock_mode)
     int clock_mode;
{
#ifdef HAS_GETTIMEOFDAY
    struct timezone tz;
#endif /*HAS_GETTIMEOFDAY*/
    long timeRemaining;
    
    switch (clock_mode) {
      case ResetTimers:
	/* Stop clocks and reset to a fresh time control */
	if (timerXID != 0) {
	    XtRemoveTimeOut(timerXID);
	    timerXID = 0;
	}
	whiteTimeRemaining = timeControl;
	blackTimeRemaining = timeControl;
	if (whiteFlag || blackFlag) {
	    DisplayTitle("");
	    whiteFlag = blackFlag = False;
	}
	DisplayLabels();
	break;
	
      case DecrementTimers:
	/* Decrement running clock to next 1-second boundary */
	timerXID = 0;
	if (!appData.clockMode) return;
	
	if (WhiteOnMove(forwardMostMove)) {
	    timeRemaining = whiteTimeRemaining -= tickLength;
	    DisplayTimerLabel(whiteTimerWidget, "White",
			      whiteTimeRemaining);
	} else {
	    timeRemaining = blackTimeRemaining -= tickLength;
	    DisplayTimerLabel(blackTimerWidget, "Black",
			      blackTimeRemaining);
	}
	
	CheckFlags();
	if (timeRemaining <= 0) return;
	
#ifdef HAS_GETTIMEOFDAY
	tickLength = (timeRemaining <= 1000) ? 100 : 1000;
	gettimeofday(&tickStartTV, &tz);
#endif /*HAS_GETTIMEOFDAY*/
	timerXID =
	  XtAppAddTimeOut(appContext, tickLength,
			  (XtTimerCallbackProc) DisplayClocks,
			  (XtPointer) DecrementTimers);
	break;
	
      case SwitchTimers:
	/* A player has just moved, so stop the previously running
	   clock and start the other one. */
	if (timerXID != 0) {
	    XtRemoveTimeOut(timerXID);
	    timerXID = 0;
#ifdef HAS_GETTIMEOFDAY
	    if (appData.clockMode) {
		if (WhiteOnMove(forwardMostMove))
		  blackTimeRemaining -= PartialTickLength();
		else
		  whiteTimeRemaining -= PartialTickLength();
		CheckFlags();
	    }
#endif				/*HAS_GETTIMEOFDAY*/
	}
	CheckTimeControl();
	DisplayLabels();
	if (!appData.clockMode) return;
	if (gameMode == PauseGame &&
	    (pausePreviousMode == MachinePlaysBlack ||
	     pausePreviousMode == MachinePlaysWhite)) return;
	
	timeRemaining = WhiteOnMove(forwardMostMove) ?
	  whiteTimeRemaining : blackTimeRemaining;
	if (timeRemaining == 0) return;
#ifdef HAS_GETTIMEOFDAY
	tickLength = (timeRemaining <= 1000) ?
	  ((timeRemaining-1) % 100) + 1 :
	    ((timeRemaining-1) % 1000) + 1;
	gettimeofday(&tickStartTV, &tz);
#endif				/*HAS_GETTIMEOFDAY*/
	timerXID =
	  XtAppAddTimeOut(appContext, tickLength,
			  (XtTimerCallbackProc) DisplayClocks,
			  (XtPointer) DecrementTimers);
	break;
	
      case ReDisplayTimers:
	/* Display current clock values */
	DisplayLabels();
	break;
	
      case StopTimers:
	/* Stop both clocks */
	if (timerXID == 0) return;
	XtRemoveTimeOut(timerXID);
	timerXID = 0;
	if (!appData.clockMode) return;
#ifdef HAS_GETTIMEOFDAY
	if (WhiteOnMove(forwardMostMove))
	  whiteTimeRemaining -= PartialTickLength();
	else
	  blackTimeRemaining -= PartialTickLength();
	CheckFlags();
	DisplayLabels();
#endif /*HAS_GETTIMEOFDAY*/
	break;
	
      case StartTimers:
	/* Start clock of player on move, if not already running. */
	DisplayLabels();
	if (!appData.clockMode) return;
	if (timerXID != 0) return;
	
	timeRemaining = WhiteOnMove(forwardMostMove) ?
	  whiteTimeRemaining : blackTimeRemaining;
	if (timeRemaining == 0) return;
#ifdef HAS_GETTIMEOFDAY
	tickLength = (timeRemaining <= 1000) ?
	  ((timeRemaining-1) % 100) + 1 :
	    ((timeRemaining-1) % 1000) + 1;
	gettimeofday(&tickStartTV, &tz);
#endif /*HAS_GETTIMEOFDAY*/
	timerXID =
	  XtAppAddTimeOut(appContext, tickLength,
			  (XtTimerCallbackProc) DisplayClocks,
			  (XtPointer)DecrementTimers);
	break;
    }
}

void DisplayTimerLabel(w, color, timer)
     Widget w;
     char *color;
     long timer;
{
    char buf[MSG_SIZ];
    Arg args[3];
    
    if (appData.clockMode) {
	sprintf(buf, "%s: %s", color, TimeString(timer));
	XtSetArg(args[0], XtNlabel, buf);
    } else {
	XtSetArg(args[0], XtNlabel, color);
    }
    
    if (((color[0] == 'B') && WhiteOnMove(currentMove))
	|| ((color[0] == 'W') && !WhiteOnMove(currentMove))) {
	XtSetArg(args[1], XtNbackground, timerForegroundPixel);
	XtSetArg(args[2], XtNforeground, timerBackgroundPixel);
    } else {
	XtSetArg(args[1], XtNbackground, timerBackgroundPixel);
	XtSetArg(args[2], XtNforeground, timerForegroundPixel);
    }
    
    XtSetValues(w, args, 3);
}

char *TimeString(tm)
     long tm;
{
    int second, minute, hour, day;
    static char buf[32];
    
    if (tm > 0 && tm <= 900) {
	/* convert milliseconds to tenths, rounding up */
	sprintf(buf, " 0.%1d ", (tm+99)/100);
	return buf;
    }
    
    /* convert milliseconds to seconds, rounding up */
    tm = (tm + 999) / 1000; 
    
    if (tm >= (60 * 60 * 24)) {
	day = (int) (tm / (60 * 60 * 24));
	tm -= day * 60 * 60 * 24;
    } else {
	day = 0;
    }
    
    if (tm >= (60 * 60)) {
	hour = (int) (tm / (60 * 60));
	tm -= hour * 60 * 60;
    } else {
	hour = 0;
    }
    
    if (tm >= 60) {
	minute = (int) (tm / 60);
	tm -= minute * 60;
    } else {
	minute = 0;
    }
    
    second = tm % 60;
    
    if (day > 0)
      sprintf(buf, " %d:%02d:%02d:%02d ", day, hour, minute, second);
    else if (hour > 0)
      sprintf(buf, " %d:%02d:%02d ", hour, minute, second);
    else
      sprintf(buf, " %2d:%02d ", minute, second);
    
    return buf;
}

void Usage()
{
    fprintf(stderr, "Usage: %s\n", programName);
    fprintf(stderr, "\tstandard Xt options\n");
    fprintf(stderr, "\t-iconic\n");
    fprintf(stderr, "\t-tc or -timeControl minutes[:seconds]\n");
    fprintf(stderr, "\t-mps or -movesPerSession moves\n");
    fprintf(stderr, "\t-st or -searchTime minutes[:seconds]\n");
    fprintf(stderr, "\t-sd or -searchDepth number\n");
    fprintf(stderr, "\t-clock or -clockMode (True | False)\n");
    fprintf(stderr, "\t-td or -timeDelay seconds\n");
    fprintf(stderr, "\t-ics or -internetChessServerMode (True | False)\n");
    fprintf(stderr, "\t-icshost or -internetChessServerHost host_name\n");
    fprintf(stderr, "\t-icsport or -internetChessServerPort port_number\n");
    fprintf(stderr, "\t-ncp or -noChessProgram (True | False)\n");
    fprintf(stderr, "\t-fcp or -firstChessProgram program_name\n");
    fprintf(stderr, "\t-scp or -secondChessProgram program_name\n");
    fprintf(stderr, "\t-fh or -firstHost host_name\n");
    fprintf(stderr, "\t-sh or -secondHost host_name\n");
    fprintf(stderr, "\t-rsh or -remoteShell shell_name\n");
    fprintf(stderr,
	    "\t-mm or -matchMode (False | Init | Position | Opening)\n");
    fprintf(stderr, "\t-lgf or -loadGameFile file_name\n");
    fprintf(stderr, "\t-sgf or -saveGameFile file_name\n");
    fprintf(stderr, "\t-autosave or -autoSaveGames (True | False)\n");
    fprintf(stderr, "\t-lpf or -loadPositionFile file_name\n");
    fprintf(stderr, "\t-spf or -savePositionFile file_name\n");
    fprintf(stderr, "\t-size or -boardSize (Large | Medium | Small)\n");
    fprintf(stderr, "\t-coords or -showCoords (True | False)\n");
    fprintf(stderr, "\t-mono or -monoMode (True | False)\n");
    fprintf(stderr, "\t-debug or -debugMode (True | False)\n");
    exit(2);
}

/*
 * This is necessary because some C libraries aren't ANSI C compliant yet.
 */
char *StrStr(string, match)
     char *string, *match;
{
    int i, length;
    
    length = strlen(match);
    
    for (i = strlen(string) - length; i >= 0; i--, string++)
      if (!strncmp(match, string, (size_t) length))
	return string;
    
    return NULL;
}

int StrCaseCmp(s1, s2)
     char *s1, *s2;
{
    char c1, c2;
    
    for (;;) {
	c1 = ToLower(*s1++);
	c2 = ToLower(*s2++);
	if (c1 > c2) return 1;
	if (c1 < c2) return -1;
	if (c1 == NULLCHAR) return 0;
    }
}


int ToLower(c)
     int c;
{
    return isupper(c) ? tolower(c) : c;
}


int ToUpper(c)
     int c;
{
    return islower(c) ? toupper(c) : c;
}


#if defined(SYSTEM_FIVE) || defined(SYSV)
#ifdef	IRIX /*??*/
char *PseudoTTY(ptyv)
     int *ptyv;
{
    char *line;
    
    line = (char *)_getpty(ptyv, O_RDWR, 0600, 0);
    if (0 == line)
      return NULL;
    return line;
}
#else	/*!IRIX*/
char *PseudoTTY(ptyv)
     int *ptyv;
{
#ifdef SVR4
    char *ptsname(), *ptss;
    
    *ptyv = open("/dev/ptmx", O_RDWR);
    if (*ptyv > 0 ) {
	if (grantpt(*ptyv) == -1)
	  return NULL;
	if (unlockpt(*ptyv) == -1)
	  return NULL;
	if (!(ptss = ptsname(*ptyv)))
	  return NULL;
	strncpy(ptyname, ptss, sizeof(ptyname));
	ptyname[sizeof(ptyname) -1] = 0;
	return ptyname;
    }
    
#else /* !SVR4 */
    struct stat stb;
    int c, i;
    
    for (c = 'p'; c <= 'z'; c++)
#if defined(HPUX) || defined(hpux)
      for (i = 0; i < 15 /*??*/; i++) {
	  sprintf(ptyname, "/dev/ptym/pty%c%x", c, i);
#else /* !HPUX */
      for (i = 0; i < 16; i++) {
#ifdef RTU
	  sprintf(ptyname, "/dev/pty%x", i);
#else /* !RTU */
	  sprintf(ptyname, "/dev/pty%c%x", c, i);
#endif /* RTU */
#endif /* HPUX */
	      
#ifdef IRIS
	  *ptyv = open("/dev/ptc", O_RDWR, 0);
	  if (*ptyv < 0)
	    return NULL;
	  if (fstat(*ptyv, &stb) < 0)
	    return NULL;
#else /* !IRIS */
	  if (stat(ptyname, &stb) < 0)
	    return NULL;
	  *ptyv = open(ptyname, O_RDWR, 0);
#endif /* IRIS */
	      
	  if (*ptyv >= 0) {
#if defined(HPUX) || defined(hpux)
	      sprintf(ptyname, "/dev/pty/tty%c%x", c, i);
#else /* !HPUX */
#ifdef RTU
	      sprintf(ptyname, "/dev/ttyp%x", i);
#else /* !RTU */
#ifdef IRIS
	      sprintf(ptyname, "/dev/ttyq%d",
		      minor(stb.st_rdev));
#else /* !IRIS, !RTU, !HPUX */
	      sprintf(ptyname, "/dev/tty%c%x", c, i);
#endif /* IRIS */
#endif /* RTU */
#endif /* HPUX */
		  
#ifndef	UNIPLUS
	      if (access(ptyname, 6) != 0) {
		  close(*ptyv);
#ifdef IRIS
		  return NULL;
#else /* !IRIS */
		  continue;
#endif /* IRIS */
	      }
#endif /* !UNIPLUS */
		  
#ifdef IBMRTAIX
	      signal(SIGHUP, SIG_IGN);
#endif /* IBMRTAIX */
	      return ptyname;
	  }
      }
	  
#endif
    return NULL;
}
#endif /*IRIX*/
    
#else /* !SYSTEM_FIVE */
    
void CatchPipeSignal(dummy)
  int dummy;
{
    char message[MSG_SIZ];
	
    sprintf(message,
	    "%s chess program (%s) exited unexpectedly (pipe signal)",
	    lastMsgFP == toFirstProgFP ? "first" : "second",
	    lastMsgFP == toFirstProgFP ? appData.firstChessProgram
	    : appData.secondChessProgram);
    fprintf(stderr, "%s: %s\n", programName, message);
    ShutdownChessPrograms(message);
    return;
}
#endif
