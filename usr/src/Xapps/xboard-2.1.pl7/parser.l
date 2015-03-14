%a 5000
%o 5000
%e 1500
%k 2000
%p 3000
%{
/*
 * parser.l -- lex parser of algebraic chess moves for XBoard
 *
 * Original author:  Chris Sears
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
 *
 * This parser handles all forms of promotion.
 * The parser resolves ambiguous moves by searching and check-testing.
 * It also parses comments of the form [anything] or (anything).
 */
#include "xboard.h"
#include <ctype.h>
#include <string.h>

enum { False, True };

#define A_I(ch)			((isupper(ch) ? tolower(ch) : ch) - 'a')
#define NO_CONSTRAINT	-1
#undef YYLMAX
#define YYLMAX			2048
#define UNPUT_BUF_SIZE		YYLMAX
#undef input
#undef output
#undef unput

#undef WHITE_ON_MOVE
#define WHITE_ON_MOVE		((int) ((yyboardindex % 2) == 0))

#define KING_TEST(r, f, piece) { \
	if ((r >= 0) && (r < BOARD_SIZE) && (f >= 0) && (f < BOARD_SIZE)) { \
		if (boards[yyboardindex][r][f] == (ChessSquare) piece) { \
			currentMoveString[0] = f + 'a'; \
			currentMoveString[1] = r + '1'; \
			break; \
		} \
	} \
}

#define PIECE_TEST(r, f, piece) { \
	if ((r >= 0) && (r < BOARD_SIZE) && (f >= 0) && (f < BOARD_SIZE)) { \
		if (boards[yyboardindex][r][f] == (ChessSquare) piece) { \
			if (! CheckTest(rank, file, r, f)) { \
				currentMoveString[0] = f + 'a'; \
				currentMoveString[1] = r + '1'; \
				found++; \
			} \
		} \
	} \
}

#define PROMOTION_PIECE(p) { \
	currentMoveString[5] = '\n'; \
	currentMoveString[6] = '\0'; \
	switch (p) { \
	case 'n': case 'N': \
		currentMoveString[4] = 'n'; \
		result = WHITE_ON_MOVE \
			? (int) WhitePromotionKnight \
                        : (int) BlackPromotionKnight; \
		break; \
	case 'b': case 'B': \
		currentMoveString[4] = 'b'; \
		result = WHITE_ON_MOVE \
			? (int) WhitePromotionBishop \
                        : (int) BlackPromotionBishop; \
		break; \
	case 'r': case 'R': \
		currentMoveString[4] = 'r'; \
		result = WHITE_ON_MOVE \
			? (int) WhitePromotionRook \
                        : (int) BlackPromotionRook; \
		break; \
	case 'q': case 'Q': \
		currentMoveString[4] = 'q'; \
		result = WHITE_ON_MOVE \
			? (int) WhitePromotionQueen \
                        : (int) BlackPromotionQueen; \
		break; \
	default: \
		result = (int) BadMove; \
		break; \
	} \
}

extern Board	boards[MAX_MOVES];
int		yyboardindex;
extern int      xboardDebug;
extern FILE	*gameFileFP;
char		currentMoveString[YYLMAX];
char		unputBuffer[UNPUT_BUF_SIZE];
int		unputCount = 0;

static int RookSearch P((int *rank, int *file,
	ChessSquare piece, Board b, int recurse, int constraint));
static int BishopSearch P((int *rank, int *file,
	ChessSquare piece, Board b, int recurse, int constraint));
static int CheckTest P((int rt, int ft, int rf, int ff));
static int yywrap P((void));
static int input P((void));
static void output P((int ch));
static void unput P((int ch));
extern void CopyBoard P((Board to, Board from));

%}
%%

\([0-9]+:[0-9][0-9]\) {
	/* elapsed time indication, e.g. (0:12) */ 
        return (int) ElapsedTime;
}

"[--"[^\]]*"--]" {
	/* position diagram enclosed in [-- --] */
	return (int) PositionDiagram;
}

\[[^\]]*\]	{        			/* anything in [] */
	return (int) Comment; 
}

\([^()]*(\([^()]*\))+[^()]*\)  { 	  	/* nested () */
	return (int) Comment; 
}

\([^)][^)]+\)   { 				/* >=2 chars in () */
        return (int) Comment; 
}       

[RrBbNnQqKkPp][/]?[a-hA-H][1-8][xX:-]?[a-hA-H][1-8](=?\(?[RrBbNnQq]\)?)? {
	/*
	 * Fully-qualified algebraic move, possibly with promotion
	 */
	int result = (int) NormalMove, skip1 = 0, skip2 = 0;

	/*
	 * remove the / 
	 */
	if (yytext[1] == '/') skip1 = 1;

	/*
	 * remove the [xX:-] 
	 */
	if ((yytext[3+skip1] == 'x') || (yytext[3+skip1] == 'X') ||
	    (yytext[3+skip1] == '-') || (yytext[3+skip1] == ':')) skip2 = 1;

	currentMoveString[0] = yytext[1+skip1]; /*ignore piece name*/
	currentMoveString[1] = yytext[2+skip1];
	currentMoveString[2] = yytext[3+skip1+skip2];
	currentMoveString[3] = yytext[4+skip1+skip2];
	currentMoveString[4] = '\n';
	currentMoveString[5] = '\0';

	if (yyleng-skip1-skip2 == 5)
	  return (int) NormalMove;
	if (yytext[yyleng-1] == ')') {
		PROMOTION_PIECE(yytext[yyleng-2]);
	} else {
		PROMOTION_PIECE(yytext[yyleng-1]);
	}
	return result;
}

[a-hA-H][1-8][xX:-]?[a-hA-H][1-8](=?\(?[RrBbNnQq]\)?)?	{
	/*
	 * Simple algebraic move, possibly with promotion
	 */
	int result = (int) NormalMove, skip = 0;

	/*
	 * remove the [xX:-] 
	 */
	if ((yytext[2] == 'x') || (yytext[2] == 'X') ||
	    (yytext[2] == '-') || (yytext[2] == ':')) skip = 1;

	currentMoveString[0] = yytext[0];
	currentMoveString[1] = yytext[1];
	currentMoveString[2] = yytext[2+skip];
	currentMoveString[3] = yytext[3+skip];
	currentMoveString[4] = '\n';
	currentMoveString[5] = '\0';

	if (yyleng-skip == 4)
	  return (int) NormalMove;
	if (yytext[yyleng-1] == ')') {
		PROMOTION_PIECE(yytext[yyleng-2]);
	} else {
		PROMOTION_PIECE(yytext[yyleng-1]);
	}
	return result;
}

[a-hA-H][1-8](=?\(?[RrBbNnQq]\)?)?	{
	/*
	 * Pawn move, possibly with promotion
	 */
	int rank = yytext[1] - '1', file = A_I(yytext[0]);
        int result = (int) NormalMove;

	currentMoveString[0] = yytext[0];
	currentMoveString[1] = yytext[1];
	currentMoveString[2] = yytext[0];
	currentMoveString[3] = yytext[1];
	currentMoveString[4] = '\n';
	currentMoveString[5] = '\0';

	if (WHITE_ON_MOVE) {
		if (boards[yyboardindex][rank - 1][file] == WhitePawn)
		  currentMoveString[1]--;
		else
		  currentMoveString[1] -= 2;
	} else {
		if (boards[yyboardindex][rank + 1][file] == BlackPawn)
		  currentMoveString[1]++;
		else
		  currentMoveString[1] += 2;
	}

	if (yyleng == 2)
	  return (int) NormalMove;
	if (yytext[yyleng-1] == ')') {
		PROMOTION_PIECE(yytext[yyleng-2]);
	} else {
		PROMOTION_PIECE(yytext[yyleng-1]);
	}
	return result;
}

(ab|bc|cd|de|ef|fg|gh|hg|gf|fe|ed|dc|cb|ba|([a-h][xX:-][a-h]))(=?\(?[RrBbNnQq]\)?)? {
	/*
	 * Pawn capture, possibly with promotion, possibly ambiguous
	 */
	int i, p, file0, file1, result = 0, skip = 0;

	/*
	 * remove the [xX:-]
	 */
	if ((yytext[1] == 'x') || (yytext[1] == 'X')
	    || (yytext[1] == ':') || (yytext[1] == '-')) skip = 1;

	file0 = A_I(yytext[0]);
	file1 = A_I(yytext[1+skip]);

	currentMoveString[0] = yytext[0];
	currentMoveString[2] = yytext[1+skip];
	currentMoveString[4] = '\n';
	currentMoveString[5] = '\0';

	if (WHITE_ON_MOVE) {
		for (i = 1; i < BOARD_SIZE - 1; i++) {
			if (boards[yyboardindex][i][file0] == WhitePawn) {
				p = (int) boards[yyboardindex][i + 1][file1];
				if (((i == 4) && (p == (int) EmptySquare)
				         /* en passant */
				     && (boards[yyboardindex][i][file1] ==
					 BlackPawn)) ||
				    ((p >= (int) BlackPawn) &&
				     (p <= (int) BlackQueen))) {

					currentMoveString[1] = '1' + i;
					currentMoveString[3] = '1' + i + 1;
					if (p == (int) EmptySquare)
					  if (result != 0)
					    return (int) AmbiguousMove;
					  else
					    result = (int)
					      WhiteCapturesEnPassant;
					else if (yyleng-skip == 2)
					  if (result != 0)
					    return (int) AmbiguousMove;
					  else
					    result = (int) NormalMove;
					else
					  if (yytext[yyleng-1] == ')')
					    PROMOTION_PIECE(yytext[yyleng-2])
					  else
					    PROMOTION_PIECE(yytext[yyleng-1])
				}
			}
		}
	} else {
		for (i = 1; i < BOARD_SIZE - 1; i++) {
			if (boards[yyboardindex][i][file0] == BlackPawn) {
				p = (int) boards[yyboardindex][i - 1][file1];
				if (((i == 3) && (p == (int) EmptySquare)
				         /* en passant */
				     && (boards[yyboardindex][i][file1] ==
					 WhitePawn)) ||
				    ((p >= (int) WhitePawn) &&
				     (p <= (int) WhiteQueen))) {

					currentMoveString[1] = '1' + i;
					currentMoveString[3] = '1' + i - 1;
					if (p == (int) EmptySquare)
					  if (result != 0)
					    return (int) AmbiguousMove;
					  else
					    result = (int)
					      BlackCapturesEnPassant;
					else if (yyleng-skip == 2)
					  if (result != 0)
					    return (int) AmbiguousMove;
					  else
					    result = (int) NormalMove;
					else 
					  if (yytext[yyleng-1] == ')')
					    PROMOTION_PIECE(yytext[yyleng-2])
					  else 
					    PROMOTION_PIECE(yytext[yyleng-1])
				}
			}
		}
	}

	if (result == 0)
	  return (int) BadMove;
	else
	  return result;
}

[a-h][xX:]?[a-h][1-8](=?\(?[RrBbNnQq]\)?)?	{
	/*
	 * unambiguously abbreviated Pawn capture, possibly with promotion
	 */
	int result, skip = 0;

	/*
	 * remove the [xX:-]
	 */
	if ((yytext[1] == 'x') || (yytext[1] == 'X')
	    || (yytext[1] == ':') || (yytext[1] == '-')) skip = 1;

	currentMoveString[0] = yytext[0];
	currentMoveString[4] = '\n';
	currentMoveString[5] = '\0';

	if (WHITE_ON_MOVE) {
		if ((yytext[2+skip] == '6' ||
		     (yytext[2+skip] == '5' &&
		      boards[yyboardindex][3][A_I(yytext[0])] != WhitePawn)) &&
		    (boards[yyboardindex][4][A_I(yytext[0])] == WhitePawn) &&
		    (boards[yyboardindex][4][A_I(yytext[1+skip])] ==
					       BlackPawn) &&
		    (boards[yyboardindex][5][A_I(yytext[1+skip])] ==
					       EmptySquare)) {
			currentMoveString[1] = '5';
			currentMoveString[2] = yytext[1+skip];
			currentMoveString[3] = '6';
			return (int) WhiteCapturesEnPassant;
		}
	} else {
		if ((yytext[2+skip] == '3' ||
		     (yytext[2+skip] == '4' &&
		      boards[yyboardindex][4][A_I(yytext[0])] != BlackPawn)) &&
		    (boards[yyboardindex][3][A_I(yytext[0])] == BlackPawn) &&
		    (boards[yyboardindex][3][A_I(yytext[1+skip])] ==
					       WhitePawn) &&
		    (boards[yyboardindex][2][A_I(yytext[1+skip])] ==
					       EmptySquare)) {
			currentMoveString[1] = '4';
			currentMoveString[2] = yytext[1+skip];
			currentMoveString[3] = '3';
			return (int) BlackCapturesEnPassant;
		}
	}

	currentMoveString[2] = yytext[1+skip];
	currentMoveString[3] = yytext[2+skip];
	if (WHITE_ON_MOVE)
	  currentMoveString[1] = yytext[2+skip] - 1;
	else
	  currentMoveString[1] = yytext[2+skip] + 1;

	if (yyleng-skip == 3)
	  return (int) NormalMove;

	if (yytext[yyleng-1] == ')') {
		PROMOTION_PIECE(yytext[yyleng-2]);
	} else {
		PROMOTION_PIECE(yytext[yyleng-1]);
	}
	return result;
}

[RrBbNnQqKk][xX:-]?[a-hA-H][1-8]	{
	/*
	 * piece move, possibly ambiguous
	 */
	int p, rank, file, found;

	if ((yytext[1] == 'x') || (yytext[1] == 'X')
	    || (yytext[1] == ':') || (yytext[1] == '-')) {
		currentMoveString[2] = yytext[2];
		currentMoveString[3] = yytext[3];
		rank = yytext[3] - '1'; file = A_I(yytext[2]);
	} else {
		currentMoveString[2] = yytext[1];
		currentMoveString[3] = yytext[2];
		rank = yytext[2] - '1'; file = A_I(yytext[1]);
	}

	currentMoveString[4] = '\n';
	currentMoveString[5] = '\0';

	switch (yytext[0]) {
	      case 'R': case 'r':
		switch (RookSearch(&rank, &file, WHITE_ON_MOVE ? WhiteRook
				: BlackRook, boards[yyboardindex],
				(int) True, NO_CONSTRAINT))
		  {
			case 0: return (int) BadMove;
			case 1: break;
			default: return (int) AmbiguousMove;
		  }

		currentMoveString[0] = file + 'a';
		currentMoveString[1] = rank + '1';

		break;
	      case 'B': case 'b':
		switch (BishopSearch(&rank, &file, WHITE_ON_MOVE ? WhiteBishop
				  : BlackBishop, boards[yyboardindex],
				  (int) True, NO_CONSTRAINT))
		  {
			case 0: return (int) BadMove;
			case 1: break;
			default: return (int) AmbiguousMove;
		  }

		currentMoveString[0] = file + 'a';
		currentMoveString[1] = rank + '1';

		break;
	      case 'N': case 'n':
		p = (int) (WHITE_ON_MOVE ? WhiteKnight : BlackKnight);
		found = 0;
		PIECE_TEST(rank - 2, file - 1, p);
		PIECE_TEST(rank - 1, file - 2, p);
		PIECE_TEST(rank + 1, file - 2, p);
		PIECE_TEST(rank + 2, file - 1, p);
		PIECE_TEST(rank + 2, file + 1, p);
		PIECE_TEST(rank + 1, file + 2, p);
		PIECE_TEST(rank - 1, file + 2, p);
		PIECE_TEST(rank - 2, file + 1, p);
		if (found == 0)
		  return (int) BadMove;
		else if (found == 1)
		  break;
		else
		  return (int) AmbiguousMove;
		break;
	      case 'Q': case 'q':
		switch (RookSearch(&rank, &file, WHITE_ON_MOVE ? WhiteQueen
				: BlackQueen, boards[yyboardindex],
				(int) True, NO_CONSTRAINT) +
		      BishopSearch(&rank, &file, WHITE_ON_MOVE ? WhiteQueen
				    : BlackQueen, boards[yyboardindex],
				    (int) True, NO_CONSTRAINT))
		  {
			case 0: return (int) BadMove;
			case 1: break;
			default: return (int) AmbiguousMove;
		  }

		currentMoveString[0] = file + 'a';
		currentMoveString[1] = rank + '1';

		break;
	      case 'K': case 'k':
		p = (int) (WHITE_ON_MOVE ? WhiteKing : BlackKing);
		KING_TEST(rank - 1, file - 1, p);
		KING_TEST(rank, file - 1, p);
		KING_TEST(rank + 1, file - 1, p);
		KING_TEST(rank + 1, file, p);
		KING_TEST(rank + 1, file + 1, p);
		KING_TEST(rank, file + 1, p);
		KING_TEST(rank - 1, file + 1, p);
		KING_TEST(rank - 1, file, p);
		return (int) BadMove;
		break;
	}

	return (int) NormalMove;
}

[RrBbNnQqKk][a-hA-H1-8][xX:-]?[a-hA-H][1-8]	{
	/*
	 * piece move with rank or file disambiguator
	 */
	int p, c, rank, file, found;

	if ((yytext[2] == 'x') || (yytext[2] == 'X')
	    || (yytext[2] == ':') || (yytext[2] == '-')) {
		currentMoveString[2] = yytext[3];
		currentMoveString[3] = yytext[4];
		rank = yytext[4] - '1'; file = A_I(yytext[3]);
	} else {
		currentMoveString[2] = yytext[2];
		currentMoveString[3] = yytext[3];
		rank = yytext[3] - '1'; file = A_I(yytext[2]);
	}

	currentMoveString[4] = '\n';
	currentMoveString[5] = '\0';

	c = isupper(yytext[1]) ? tolower(yytext[1]) : yytext[1];

	switch (yytext[0]) {
	      case 'R': case 'r':
		switch (RookSearch(&rank, &file, WHITE_ON_MOVE ? WhiteRook
				: BlackRook, boards[yyboardindex],
				(int) True, yytext[1]))
		  {
			case 0: return (int) BadMove;
			case 1: break;
			default: return (int) AmbiguousMove;
		  }

		currentMoveString[0] = file + 'a';
		currentMoveString[1] = rank + '1';

		break;
	      case 'B': case 'b':
		switch (BishopSearch(&rank, &file, WHITE_ON_MOVE ? WhiteBishop
				  : BlackBishop, boards[yyboardindex],
				  (int) True, yytext[1]))
		  {
			case 0: return (int) BadMove;
			case 1: break;
			default: return (int) AmbiguousMove;
		  }

		currentMoveString[0] = file + 'a';
		currentMoveString[1] = rank + '1';

		break;
	      case 'N': case 'n':
		p = (int) (WHITE_ON_MOVE ? WhiteKnight : BlackKnight);
                found = 0;
		if (isalpha(c)) {
			c -= 'a';
			if ((file - 1) == c) {
				PIECE_TEST(rank - 2, file - 1, p);
				PIECE_TEST(rank + 2, file - 1, p);
			}
			if ((file - 2) == c) {
				PIECE_TEST(rank - 1, file - 2, p);
				PIECE_TEST(rank + 1, file - 2, p);
			}
			if ((file + 1) == c) {
				PIECE_TEST(rank + 2, file + 1, p);
				PIECE_TEST(rank - 2, file + 1, p);
			}
			if ((file + 2) == c) {
				PIECE_TEST(rank + 1, file + 2, p);
				PIECE_TEST(rank - 1, file + 2, p);
			}
		} else {
			c -= '1';
			if ((rank - 2) == c) {
				PIECE_TEST(rank - 2, file - 1, p);
				PIECE_TEST(rank - 2, file + 1, p);
			}
			if ((rank - 1) == c) {
				PIECE_TEST(rank - 1, file - 2, p);
				PIECE_TEST(rank - 1, file + 2, p);
			}
			if ((rank + 2) == c) {
				PIECE_TEST(rank + 2, file - 1, p);
				PIECE_TEST(rank + 2, file + 1, p);
			}
			if ((rank + 1) == c) {
				PIECE_TEST(rank + 1, file - 2, p);
				PIECE_TEST(rank + 1, file + 2, p);
			}
		}
		if (found == 0)
		  return (int) BadMove;
		else if (found == 1)
		  break;
		else
		  return (int) AmbiguousMove;
		break;
	      case 'Q': case 'q':
		switch (RookSearch(&rank, &file, WHITE_ON_MOVE ? WhiteQueen
				: BlackQueen, boards[yyboardindex],
				(int) True, yytext[1]) +
		      BishopSearch(&rank, &file,
				   WHITE_ON_MOVE ? WhiteQueen
				   : BlackQueen, boards[yyboardindex],
				   (int) True, yytext[1]))
		  {
			case 0: return (int) BadMove;
			case 1: break;
			default: return (int) AmbiguousMove;
		  }

		currentMoveString[0] = file + 'a';
		currentMoveString[1] = rank + '1';

		break;
	}

	return (int) NormalMove;
}

000|0-0-0|ooo|OOO|o-o-o|O-O-O	{
	strcpy(currentMoveString, "o-o-o\n");
	if (WHITE_ON_MOVE)
	  return (int) WhiteQueenSideCastle;
	else
	  return (int) BlackQueenSideCastle;
}

00|0-0|oo|OO|o-o|O-O	{
	strcpy(currentMoveString, "o-o\n");
	if (WHITE_ON_MOVE)
	  return (int) WhiteKingSideCastle;
	else
	  return (int) BlackKingSideCastle;
}

[Rr][Ee][Ss][Ii][Gg][Nn][Ss]?	{
	if (WHITE_ON_MOVE)
	  return (int) BlackWins;
	else
	  return (int) WhiteWins;
}

[Ww][Hh][Ii][Tt][Ee]" "[Rr][Ee][Ss][Ii][Gg][Nn][Ss]?	{
	return (int) BlackWins;
}

[Bb][Ll][Aa][Cc][Kk]" "[Rr][Ee][Ss][Ii][Gg][Nn][Ss]?	{
	return (int) WhiteWins;
}

[Ss][Tt][Aa][Ll][Ee][Mm][Aa][Tt][Ee]                    {
	return (int) GameIsDrawn;
}

([Cc][Hh][Ee][Cc][Kk])?[Mm][Aa][Tt][Ee]			{
	if (WHITE_ON_MOVE)
	  return (int) WhiteWins;
	else
	  return (int) BlackWins;
}

(([Bb][Ll][Aa][Cc][Kk]|[Ww][Hh][Ii][Tt][Ee])" "[Oo][Ff][Ff][Ee][Rr][Ss]" ")?[Dd][Rr][Aa][Ww][Nn]?|1\/2		{ 
	return (int) GameIsDrawn;
}
[Dd][Rr][Aa][Ww][Nn]?(" "[Bb][Yy])?(" "[Rr][Ee][Pp][Ee][Tt][Ii][Tt][Ii][Oo][Nn]|[Aa][Gg][Rr][Ee][Ee]([Dd]|[Mm][Ee][Nn][Tt])?) {
	return (int) GameIsDrawn;
}
[Dd][Rr][Aa][Ww][Nn]?" (".*")" {
        return (int) GameIsDrawn;
}
[Ww][Hh][Ii][Tt][Ee](" "[Ww][Ii][Nn][Ss](" "[Oo][Nn]" "[Tt][Ii][Mm][Ee])?|" "[Mm][Aa][Tt][Ee][Ss])? { 
	return (int) WhiteWins;
}
[Bb][Ll][Aa][Cc][Kk](" "[Ww][Ii][Nn][Ss](" "[Oo][Nn]" "[Tt][Ii][Mm][Ee])?|" "[Mm][Aa][Tt][Ee][Ss])? { 
	return (int) BlackWins;
}
1-0|"1 - 0"|"1/0"|"1 / 0"|"1:0"|"1 : 0"	{ 
	return (int) WhiteWins;
}
0-1|"0 - 1"|"0/1"|"0 / 1"|"0:1"|"0 : 1"	{ 
	return (int) BlackWins;
}

[1-9][0-9]*/[. \t\n]*[a-hNnPp]    {
	/* move numbers */
        /* note: above regular expression assumes game starts from
           conventional opening position, so that only pawn and
           knight moves are legal. */

	if ((yyleng == 1) && (yytext[0] == '1'))
		return (int) StartGame;
}

[Gg][Nn][Uu]" "?[Cc][Hh][Ee][Ss][Ss].*[Gg][Aa][Mm][Ee] {
	return (int) StartGame;
}

[a-zA-Z0-9'-]+			{
        /* Skip random words */
}

.|"\n"				{
        /* Skip everything else */
}

%%

static int
RookSearch(rank, file, piece, b, recurse, constraint)
	int *rank, *file, recurse, constraint;
	ChessSquare piece;
	Board b;
{
    int i, r = *rank, f = *file, found = 0;
    
    /*
     * Start from the target space and work outwards towards the piece.
     * This is necessary for check testing.
     */
    for (i = f + 1;; i++) {
	if (i >= BOARD_SIZE)
	  break;
	if (isalpha(constraint) && (i != A_I(constraint)))
	  continue;
	if (isdigit(constraint) && (r != (constraint - '1')))
	  break;
	if ((b[r][i] != EmptySquare) && (b[r][i] != piece))
	  break;
	if (recurse)
	  if (CheckTest(r, f, r, i))
	    continue;
	if (b[r][i] == piece) {
	    *file = i;
	    found++;
	    break;
	}
    }
    
    for (i = f - 1;; i--) {
	if (i < 0)
	  break;
	if (isalpha(constraint) && (i != A_I(constraint)))
	  continue;
	if (isdigit(constraint) && (r != (constraint - '1')))
	  break;
	if ((b[r][i] != EmptySquare) && (b[r][i] != piece))
	  break;
	if (recurse)
	  if (CheckTest(r, f, r, i))
	    continue;
	if (b[r][i] == piece) {
	    *file = i;
	    found++;
	    break;
	}
    }
    
    for (i = r + 1;; i++) {
	if (i >= BOARD_SIZE)
	  break;
	if (isalpha(constraint) && (f != A_I(constraint)))
	  break;
	if (isdigit(constraint) && (i != (constraint - '1')))
	  continue;
	if ((b[i][f] != EmptySquare) && (b[i][f] != piece))
	  break;
	if (recurse)
	  if (CheckTest(r, f, i, f))
	    continue;
	if (b[i][f] == piece) {
	    *rank = i;
	    found++;
	    break;
	}
    }
    
    for (i = r - 1;; i--) {
	if (i < 0)
	  break;
	if (isalpha(constraint) && (f != A_I(constraint)))
	  break;
	if (isdigit(constraint) && (i != (constraint - '1')))
	  continue;
	if ((b[i][f] != EmptySquare) && (b[i][f] != piece))
	  break;
	if (recurse)
	  if (CheckTest(r, f, i, f))
	    continue;
	if (b[i][f] == piece) {
	    *rank = i;
	    found++;
	    break;
	}
    }
    
    return found;
}

static int
  BishopSearch(rank, file, piece, b, recurse, constraint)
int *rank, *file, recurse, constraint;
ChessSquare piece;
Board b;
{
    int i, j, r = *rank, f = *file, found = 0;
    
    /*
     * Start from the target space and work outwards towards the piece.
     * This is necessary for check testing.
     */
    for (i = r + 1, j = f + 1;; i++, j++) {
	if ((i >= BOARD_SIZE) || (j >= BOARD_SIZE))
	  break;
	if (isalpha(constraint) && (j != A_I(constraint)))
	  continue;
	if (isdigit(constraint) && (i != (constraint - '1')))
	  continue;
	if ((b[i][j] != EmptySquare) && (b[i][j] != piece))
	  break;
	if (recurse)
	  if (CheckTest(r, f, i, j)) {
	      continue;
	  }
	if (b[i][j] == piece) {
	    *rank = i;
	    *file = j;
	    found++;
	    break;
	}
    }
    
    for (i = r + 1, j = f - 1;; i++, j--) {
	if ((i >= BOARD_SIZE) || (j < 0))
	  break;
	if (isalpha(constraint) && (j != A_I(constraint)))
	  continue;
	if (isdigit(constraint) && (i != (constraint - '1')))
	  continue;
	if ((b[i][j] != EmptySquare) && (b[i][j] != piece))
	  break;
	if (recurse)
	  if (CheckTest(r, f, i, j)) {
	      continue;
	  }
	
	if (b[i][j] == piece) {
	    *rank = i;
	    *file = j;
	    found++;
	    break;
	}
    }
    
    for (i = r - 1, j = f + 1;; i--, j++) {
	if ((i < 0) || (j >= BOARD_SIZE))
	  break;
	if (isalpha(constraint) && (j != A_I(constraint)))
	  continue;
	if (isdigit(constraint) && (i != (constraint - '1')))
	  continue;
	if ((b[i][j] != EmptySquare) && (b[i][j] != piece))
	  break;
	if (recurse)
	  if (CheckTest(r, f, i, j)) {
	      continue;
	  }
	if (b[i][j] == piece) {
	    *rank = i;
	    *file = j;
	    found++;
	    break;
	}
    }
    
    for (i = r - 1, j = f - 1;; i--, j--) {
	if ((i < 0) || (j < 0))
	  break;
	if (isalpha(constraint) && (j != A_I(constraint)))
	  continue;
	if (isdigit(constraint) && (i != (constraint - '1')))
	  continue;
	if ((b[i][j] != EmptySquare) && (b[i][j] != piece))
	  break;
	if (recurse)
	  if (CheckTest(r, f, i, j)) {
	      continue;
	  }
	if (b[i][j] == piece) {
	    *rank = i;
	    *file = j;
	    found++;
	    break;
	}
    }
    
    return found;
}

static int
  CheckTest(rt, ft, rf, ff)
int rt, ft, rf, ff;
{
    int rk, fk, rank, file;
    Board b;
    
    CopyBoard(b, boards[yyboardindex]);
    b[rt][ft] = b[rf][ff];
    b[rf][ff] = EmptySquare;
    
    /*
     * the bad move must have uncovered check by a rook, bishop or queen.
     */
    if (WHITE_ON_MOVE) {
	/* Is the white king in check? */
	for (rk = 0; rk < BOARD_SIZE; rk++) {
	    for (fk = 0; fk < BOARD_SIZE; fk++) {
		if (b[rk][fk] == WhiteKing) {
		    rank = rk; file = fk;
		    if (BishopSearch(&rank, &file,
				     BlackBishop,
				     b, (int) False, NO_CONSTRAINT))
		      return (int) True;
		    rank = rk; file = fk;
		    if (RookSearch(&rank, &file,
				   BlackRook,
				   b, (int) False, NO_CONSTRAINT))
		      return (int) True;
		    rank = rk; file = fk;
		    if (BishopSearch(&rank, &file,
				     BlackQueen,
				     b, (int) False, NO_CONSTRAINT))
		      return (int) True;
		    rank = rk; file = fk;
		    if (RookSearch(&rank, &file,
				   BlackQueen,
				   b, (int) False, NO_CONSTRAINT))
		      return (int) True;
		    return (int) False;
		}
	    }
	}
    } else {
	/* Is the black king in check? */
	for (rk = 0; rk < BOARD_SIZE; rk++) {
	    for (fk = 0; fk < BOARD_SIZE; fk++) {
		if (b[rk][fk] == BlackKing) {
		    rank = rk; file = fk;
		    if (BishopSearch(&rank, &file,
				     WhiteBishop,
				     b, (int) False, NO_CONSTRAINT))
		      return (int) True;
		    rank = rk; file = fk;
		    if (RookSearch(&rank, &file,
				   WhiteRook,
				   b, (int) False, NO_CONSTRAINT))
		      return (int) True;
		    rank = rk; file = fk;
		    if (BishopSearch(&rank, &file,
				     WhiteQueen,
				     b, (int) False, NO_CONSTRAINT))
		      return (int) True;
		    rank = rk; file = fk;
		    if (RookSearch(&rank, &file,
				   WhiteQueen,
				   b, (int) False, NO_CONSTRAINT))
		      return (int) True;
		    return (int) False;
		}
	    }
	}
    }
    
    return (int) False;
}

static int yywrap()
{
    return (int) True;
}

static char *StringToLex;

static int input()
{
    int ret;
    
    if (StringToLex != NULL) {
	ret = *StringToLex;
	if (ret == '\000')
	  ret = EOF;
	else
	  StringToLex++;
    } else if (unputCount > 0) {
	ret = unputBuffer[--unputCount];
    } else {
	ret = fgetc(gameFileFP);
    }    

    if (ret == EOF) 
      return 0;
    else
      return ret;
}

static void output(ch)
     int ch;
{
    fprintf(stderr, "PARSER BUG: unmatched character '%c' (0%o)\n",
	    ch, ch);
}

static void unput(ch)
     int ch;
{
    if (ch == 0) return;
    if (StringToLex != NULL) {
	StringToLex--;
    } else {
	if (unputCount >= UNPUT_BUF_SIZE)
	  fprintf(stderr, "PARSER BUG: unput buffer overflow '%c' (0%o)\n",
		  ch, ch);
	unputBuffer[unputCount++] = ch;
    }
}

void yynewfile()
{
    unputCount = 0;
}

/* Parse a move from the given string */
/* Return a pointer to the first unparsed character 
   in "next" if it is non-NULL */
ChessMove yylexstr(boardIndex, s, next)
     int boardIndex;
     char *s;
     char **next;
{
    ChessMove ret;
    
    StringToLex = s;
    yyboardindex = boardIndex;
    ret = (ChessMove) yylex();
    if (next != NULL) *next = StringToLex;
    StringToLex = NULL;
    return ret;
}
