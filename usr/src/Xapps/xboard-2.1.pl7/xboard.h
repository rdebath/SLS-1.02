/*
 * XBoard -- an Xt/Athena user interface for GNU Chess
 *
 * Original authors:  Dan Sears and Chris Sears
 * Enhancements (Version 2.0):  Tim Mann
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
 * The following terms apply to the enhanced version of XBoard distributed
 * by the Free Software Foundation:
 * ------------------------------------------------------------------------
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
 */
#define BOARD_SIZE		8
#define LARGE_SQUARE_SIZE	80
#define MEDIUM_SQUARE_SIZE	64
#define SMALL_SQUARE_SIZE       40
#define LINE_GAP		3
#define MAX_MOVES		512
#define MSG_SIZ			256
#define DIALOG_SIZE		256
#define MOVE_LEN		32
#define TIME_CONTROL		"5"	/* in minutes */
#define TIME_DELAY		"1.0"	/* seconds between moves */
#define MOVES_PER_SESSION	40	/* moves per TIME_CONTROL */
#define WhiteOnMove(move)	(((move) % 2) == 0)
#define ICS_HOST                "valkyries.andrew.cmu.edu"
#define ICS_PORT	        5000
#define FIRST_CHESS_PROGRAM	"gnuchessx"
#define SECOND_CHESS_PROGRAM	"gnuchessx"
#define FIRST_HOST		"localhost"
#define SECOND_HOST		"localhost"
#define MATCH_MODE		"False"
#define INIT_STRING		"new\nbeep\nrandom\neasy\n"
#define WHITE_STRING		"white\ngo\n"
#define BLACK_STRING		"black\ngo\n"
#define DEFAULT_SIZE            "Large"
#define WHITE_PIECE_COLOR	"#FFFFCC"
#define BLACK_PIECE_COLOR	"#202020"
#define LIGHT_SQUARE_COLOR	"#C8C365"
#define DARK_SQUARE_COLOR	"#77A26D"
#define MAIN_FONT    "-*-helvetica-medium-o-normal--*-*-*-*-*-*-*-*"
#define COORD_FONT   "-*-helvetica-bold-r-normal--*-*-*-*-*-*-*-*"
#define DEFAULT_FONT "*font: -*-helvetica-medium-r-normal--*-100-*-*-*-*-*-*"
#define BELLCHAR                '\007'
#define NULLCHAR                '\000'
#define BORDER_X_OFFSET         3
#define BORDER_Y_OFFSET         27

typedef enum {
    Large, Medium, Small
  } BoardSize;

typedef enum {
    BeginningOfGame, MachinePlaysWhite, MachinePlaysBlack, TwoMachinesPlay,
    ForceMoves, PlayFromGameFile, PauseGame, EndOfGame, EditPosition
  } GameMode;

typedef enum {
    MatchFalse, MatchInit, MatchPosition, MatchOpening
  } MatchMode;

typedef enum {
    IcsIdle, IcsPlayingWhite, IcsPlayingBlack, IcsObserving
  } IcsMode;

typedef enum {
    WhitePawn, WhiteRook, WhiteKnight, WhiteBishop, WhiteQueen, WhiteKing,
    BlackPawn, BlackRook, BlackKnight, BlackBishop, BlackQueen, BlackKing,
    EmptySquare,
    ClearBoard, WhitePlay, BlackPlay /*for use on EditPosition menus*/
  } ChessSquare;

typedef ChessSquare Board[BOARD_SIZE][BOARD_SIZE];

typedef enum {
    WhiteKingSideCastle = 1, WhiteQueenSideCastle,
    WhitePromotionKnight, WhitePromotionBishop,
    WhitePromotionRook, WhitePromotionQueen,
    BlackPromotionKnight, BlackPromotionBishop,
    BlackPromotionRook, BlackPromotionQueen,
    BlackKingSideCastle, BlackQueenSideCastle,
    WhiteCapturesEnPassant, BlackCapturesEnPassant, NormalMove,
    WhiteWins, BlackWins, GameIsDrawn, StartGame, BadMove, Comment,
    AmbiguousMove, PositionDiagram, ElapsedTime
  } ChessMove;

typedef enum {
    ResetTimers, DecrementTimers, SwitchTimers, 
    ReDisplayTimers, StopTimers, StartTimers
  } ClockMode;

/*
 * Various compatibility grunge
 */
#ifdef __STDC__
#define	P(args)		args
#else
#define P(args)		()
#endif
