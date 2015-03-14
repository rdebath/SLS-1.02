/***********************************************************************
 *
 *	Lexer Module.
 *
 ***********************************************************************/

/***********************************************************************
 *
 * Copyright (C) 1990, 1991 Free Software Foundation, Inc.
 * Written by Steve Byrne.
 *
 * This file is part of GNU Smalltalk.
 *
 * GNU Smalltalk is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 1, or (at your option) any later 
 * version.
 * 
 * GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
 * Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  
 *
 ***********************************************************************/

/*
 *    Change Log
 * ============================================================================
 * Author      Date       Change 
 * sbb	     13 Sep 91	  Fixed character pushback to properly deal with 2
 *			  characters of pushback.
 *
 * sbyrne    24 Apr 90	  Error checking for integers too large.
 *
 * sbyrne    20 Apr 90	  Added the closeIt argument to popStream so that the
 *			  closing behavior could be separated from the popping
 *			  behavior (in particular, for fileIn).
 *
 * sbyrne     7 Apr 90	  Character lexing routines (such as nextChar) now
 *			  return ints to get around problems on implementations
 *			  that don't sign extend characters by default.
 *
 * sbyrne    15 Feb 90	  Added support for := as alternative assignment
 *			  operator.
 *
 * sbyrne     3 Sep 89	  added getCurFileName
 *
 * sbyrne    30 Aug 89	  Fixed a bug in parseIdent which was parsing foo:2
 *			  (note no space) not as foo: and 2, but as a mixed up
 *			  token.
 *
 * sbyrne     8 Jul 89	  Added prompt when input is a terminal.  This should
 *			  help Emacs's shell mode determine what has been typed
 *			  as input to the system.
 *
 * sbyrne    24 Jan 89	  Added 2 chars of push back, because 3. needs to look
 *			  ahead one more character to see if its 3.DIGIT or 3.
 *			  next statement.
 *
 * sbyrne    27 Dec 88    Created.
 */


#include "mst.h"
#include "mstlex.h"
#include "mst.tab.h"
#include "mststr.h"
#include "mstid.h"
#include "mstdict.h"
#include "mstcomp.h"
#include "msttree.h"
#include <stdio.h>
#include <math.h>
#ifdef USE_READLINE
#include <readline/readline.h>
#endif /* USE_READLINE */

#define WHITE_SPACE		1
#define DIGIT			2
#define ID_CHAR			4
#define SPECIAL_CHAR		8

#define ERROR_ARGS		arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8

#define EMACS_PROCESS_MARKER	'\001' /* ^A as marker -- random choice */

typedef struct StringStreamStruct {
  char		*strBase;	/* base of asciz string */
  char		*str;		/* pointer into asciz string */
} StringStream;

struct StreamStruct {
  StreamType	type;
  int		pushedBackChars[2]; /* holds the 2 characters of buffering */
  int		pushedBackCount; /* number of chars pushed back */
  int		line;
  int		column;
  char		*fileName;
  Boolean	prompt;
  union {
    FILE	*u_st_file;
    StringStream u_st_str;
  } st;
  Stream	prevStream;
};

#define st_file		st.u_st_file
#define st_str		st.u_st_str

Stream	inStream = NULL;
int	lexDebug;
char	*processingFile = nil;

static int			byteAddr, methodStartPos = 0;
static Boolean			produceInternalToken;

static Boolean			isDigit(), isBaseDigit(), parsePrimitive(),
				parseDigits(), parseFraction();
static int			illegal(), comment(), charLiteral(),
				parseBinOP(), stringLiteral(), parseNumber(),
				parseIdent(), myGetC(), parseColon(),
  				nextChar();
static char			*scanStringoid();
static void 			unreadChar(), lineStamp(), myClose();
static Stream			pushNewStream();

typedef struct {
  int		(*lexFunc)();
  int		retToken;
  int		charClass;
} LexTabElt;

static LexTabElt charTab[128] = {
/*   0 */	illegal,	0,	0,
/*   1 */	illegal,	0,	0,
/*   2 */	illegal,	0,	0,
/*   3 */	illegal,	0,	0,
/*   4 */	illegal,	0,	0,
/*   5 */	illegal,	0,	0,
/*   6 */	illegal,	0,	0,
/*   7 */	illegal,	0,	0,
/*   8 */	illegal,	0,	0,
/*   9 */	0,		0,	WHITE_SPACE,
/*  10 */	0,		0,	WHITE_SPACE,
/*  11 */	illegal,	0,	0,
/*  12 */	0,		0,	WHITE_SPACE, 
/*  13 */	0,		0,	WHITE_SPACE,
/*  14 */	illegal,	0,	0,	
/*  15 */	illegal,	0,	0,
/*  16 */	illegal,	0,	0,
/*  17 */	illegal,	0,	0,
/*  18 */	illegal,	0,	0,
/*  19 */	illegal,	0,	0,
/*  20 */	illegal,	0,	0,
/*  21 */	illegal,	0,	0,
/*  22 */	illegal,	0,	0,
/*  23 */	illegal,	0,	0,
/*  24 */	illegal,	0,	0,
/*  25 */	illegal,	0,	0,
/*  26 */	illegal,	0,	0,
/*  27 */	illegal,	0,	0,
/*  28 */	illegal,	0,	0,
/*  29 */	illegal,	0,	0,
/*  30 */	illegal,	0,	0,
/*  31 */	illegal,	0,	0,
/*     */	0,		0,	WHITE_SPACE,
/*   ! */	parseBinOP,	0,	SPECIAL_CHAR, 
/*   " */	comment,	0,	0,
/*   # */	0,		SHARP,	0,
/*   $ */	charLiteral,	0,	0,
/*   % */	parseBinOP,	0,	SPECIAL_CHAR,
/*   & */	parseBinOP,	0,	SPECIAL_CHAR,
/*   ' */	stringLiteral,	0,	0,
/*   ( */	0,		OPEN_PAREN, 0,
/*   ) */	0,		CLOSE_PAREN, 0,
/*   * */	parseBinOP,	0,	SPECIAL_CHAR,
/*   + */	parseBinOP,	0,	SPECIAL_CHAR,
/*   , */	parseBinOP,	0,	SPECIAL_CHAR,
/*   - */	parseBinOP,	0,	SPECIAL_CHAR,
/*   . */	0,		DOT,	0,
/*   / */	parseBinOP,	0,	SPECIAL_CHAR,
/*   0 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   1 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   2 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   3 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   4 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   5 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   6 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   7 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   8 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   9 */	parseNumber,	0,	DIGIT | ID_CHAR,
/*   : */	parseColon,	0,	0,
/*   ; */	0,		SEMICOLON, 0,
/*   < */	parseBinOP,	0,	SPECIAL_CHAR,
/*   = */	parseBinOP,	0,	SPECIAL_CHAR,
/*   > */	parseBinOP,	0,	SPECIAL_CHAR,
/*   ? */	parseBinOP,	0,	SPECIAL_CHAR,
/*   @ */	parseBinOP,	0,	SPECIAL_CHAR,
/*   A */	parseIdent,	0,	ID_CHAR,
/*   B */	parseIdent,	0,	ID_CHAR,
/*   C */	parseIdent,	0,	ID_CHAR,
/*   D */	parseIdent,	0,	ID_CHAR,
/*   E */	parseIdent,	0,	ID_CHAR,
/*   F */	parseIdent,	0,	ID_CHAR,
/*   G */	parseIdent,	0,	ID_CHAR,
/*   H */	parseIdent,	0,	ID_CHAR,
/*   I */	parseIdent,	0,	ID_CHAR,
/*   J */	parseIdent,	0,	ID_CHAR,
/*   K */	parseIdent,	0,	ID_CHAR,
/*   L */	parseIdent,	0,	ID_CHAR,
/*   M */	parseIdent,	0,	ID_CHAR,
/*   N */	parseIdent,	0,	ID_CHAR,
/*   O */	parseIdent,	0,	ID_CHAR,
/*   P */	parseIdent,	0,	ID_CHAR,
/*   Q */	parseIdent,	0,	ID_CHAR,
/*   R */	parseIdent,	0,	ID_CHAR,
/*   S */	parseIdent,	0,	ID_CHAR,
/*   T */	parseIdent,	0,	ID_CHAR,
/*   U */	parseIdent,	0,	ID_CHAR,
/*   V */	parseIdent,	0,	ID_CHAR,
/*   W */	parseIdent,	0,	ID_CHAR,
/*   X */	parseIdent,	0,	ID_CHAR,
/*   Y */	parseIdent,	0,	ID_CHAR,
/*   Z */	parseIdent,	0,	ID_CHAR,
/*   [ */	0,		OPEN_BRACKET, 0,
/*   \ */	parseBinOP,	0,	SPECIAL_CHAR,
/*   ] */	0,		CLOSE_BRACKET, 0,
/*   ^ */	0,		UPARROW, 0,
/*   _ */	0,		ASSIGN,	0,
/*   ` */	illegal,	0,	0,
/*   a */	parseIdent,	0,	ID_CHAR,
/*   b */	parseIdent,	0,	ID_CHAR,
/*   c */	parseIdent,	0,	ID_CHAR,
/*   d */	parseIdent,	0,	ID_CHAR,
/*   e */	parseIdent,	0,	ID_CHAR,
/*   f */	parseIdent,	0,	ID_CHAR,
/*   g */	parseIdent,	0,	ID_CHAR,
/*   h */	parseIdent,	0,	ID_CHAR,
/*   i */	parseIdent,	0,	ID_CHAR,
/*   j */	parseIdent,	0,	ID_CHAR,
/*   k */	parseIdent,	0,	ID_CHAR,
/*   l */	parseIdent,	0,	ID_CHAR,
/*   m */	parseIdent,	0,	ID_CHAR,
/*   n */	parseIdent,	0,	ID_CHAR,
/*   o */	parseIdent,	0,	ID_CHAR,
/*   p */	parseIdent,	0,	ID_CHAR,
/*   q */	parseIdent,	0,	ID_CHAR,
/*   r */	parseIdent,	0,	ID_CHAR,
/*   s */	parseIdent,	0,	ID_CHAR,
/*   t */	parseIdent,	0,	ID_CHAR,
/*   u */	parseIdent,	0,	ID_CHAR,
/*   v */	parseIdent,	0,	ID_CHAR,
/*   w */	parseIdent,	0,	ID_CHAR,
/*   x */	parseIdent,	0,	ID_CHAR,
/*   y */	parseIdent,	0,	ID_CHAR,
/*   z */	parseIdent,	0,	ID_CHAR,
/*   { */	illegal,	0,	0,
/*   | */	parseBinOP,	0,	SPECIAL_CHAR,
/*   } */	illegal,	0,	0,
/*   ~ */	parseBinOP,	0,	SPECIAL_CHAR,
/*  ^? */	illegal,	0,	0
};


int yylex(lvalp, llocp)
YYSTYPE *lvalp;
voidPtr *llocp;		/* Bison is broken, doesn define type YYLTYPE!! */
{
  int		ic, result, tokenStartPos;

  if (produceInternalToken) {
    /* The internal token is a trick to make the grammar be "conditional".
     * Normally, the allowable syntax for internal compilation is that of
     * a single method (without the terminating BANG).  However, would make
     * for an ambiguous grammar since a method declaration could look like
     * an immediate expression.  So, when the compiler is called internally,
     * we force the first token returned by the lexer to be INTERNAL_TOKEN
     * and make the top most production in the grammar use INTERNAL_TOKEN as
     * the first token of an internal method. */
    produceInternalToken = false;
    return (INTERNAL_TOKEN);
  }

  while ((ic = nextChar()) != EOF) {
    if ((charTab[ic].charClass & WHITE_SPACE) == 0) {
      if (methodStartPos < 0) {
	tokenStartPos = getCurFilePos();
      }

      if (charTab[ic].lexFunc) {
	result = (*charTab[ic].lexFunc)(ic, lvalp);
	if (result) {
	  if (methodStartPos < 0) { /* only do this here to ignore comments */
	    methodStartPos = tokenStartPos - 1;
	  }
	  return (result);
	}
      } else if (charTab[ic].retToken) {
	return (charTab[ic].retToken);
      } else {
	errorf("Unknown character %d in input stream, ignoring", ic);
	hadError = true;
      }
    }
  }
  return (0);			/* keep fussy compilers happy */
}

initLexer(calledInternally)
Boolean calledInternally;
{
  byteAddr = 0;
  
  produceInternalToken = calledInternally;
}

static int illegal(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  char		charName[3], *cp;

  cp = charName;

  if (c < ' ' || c > '~') {
    *cp++ = '^';
    c &= 127;			/* strip high bit */
    c ^= 64;			/* uncontrolify */
  }

  *cp++ = c;
  *cp++ = '\0';
  
  errorf("Illegal character %s", charName);
  hadError = true;
}


/*
 *	static int comment(c, lvalp)
 *
 * Description
 *
 *	Scan a comment, but return 0 to indicate to the lexer that it's to be
 *	ignored (since it is a comment).
 *
 * Inputs
 *
 *	c     : first character of the comment (the delimiter).  Not terribly
 *		useful, but it's what the lexer calls us with.
 *	lvalp : ignored...passed because we're invoked indirectly and some of
 *		the functions that could be called require this parameter.
 *
 * Outputs
 *
 *	0, which tells the lexer to ignore this token.
 */
static int comment(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  scanStringoid(c, "comment");

  return (0);
}

/*
 *	static int charLiteral(c, lvalp)
 *
 * Description
 *
 *	parse a character literal.
 *
 * Inputs
 *
 *	c     : input character -- ignored
 *	lvalp : ignored -- passed because we're called indirectly.
 *
 * Outputs
 *
 *	CHAR_LITERAL token normally; 0 on EOF.
 */
static int charLiteral(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  int		ic;

  ic = nextChar();
  if (ic == EOF) {
    errorf("Unterminated character literal, attempting recovery");
    unreadChar(ic);
    hadError = true;
    return (0);
  } else {
    lvalp->cval = ic;
    return (CHAR_LITERAL);
  }
}

static int parseColon(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  int		ic;

  ic = nextChar();
  if (ic == '=') {		/* parse :=, compatibility mode assign */
    return (ASSIGN);
  } else {
    unreadChar(ic);
  }

  return (COLON);
}


static int parseBinOP(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  char		buf[3], *bp;
  int		ic;

  bp = buf;
  *bp++ = c;

  ic = nextChar();
  if (c == '<') {
    if (ic == 'p') {
      if (parsePrimitive(ic, lvalp)) {
	return (PRIMITIVE_START);
      }
    }
  }
  if (c == '-') {
    unreadChar(ic);
    if (isDigit(ic)) {
      return (parseNumber('-', lvalp));
    }
  } else {
    if (ic != EOF && (charTab[ic].charClass & SPECIAL_CHAR)) {
      *bp++ = ic;
    } else {
      unreadChar(ic);
    }
  }

  *bp = '\0';

  if (strcmp(buf, "!") == 0) {
    /* technically, token BANG has no string value, it's just a simple token,
       so we return from here before we set the token's value */
    return (BANG);
  } else if (strcmp(buf, "!!") == 0) {
    unreadChar('!');
    return (BANG);
  }
  
  lvalp->sval = copyStr(buf);
  
  if (strcmp(buf, "|") == 0) {
    return (VERTICAL_BAR);
  } else {
    return (BINOP);
  }
}

static int stringLiteral(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  lvalp->sval = copyStr(scanStringoid(c, "string"));
  return (STRING_LITERAL);
}

static Boolean parsePrimitive(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  Boolean 	result;

  parseIdent(c, lvalp);
  result = (strcmp(lvalp->sval, "primitive:") == 0);
  free(lvalp->sval);
  return (result);
}

static int parseIdent(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  int		ic, identType;
  
  initIdent(c);
  
  identType = IDENTIFIER;
  
  for (;;) {
    while (((ic = nextChar()) != EOF) && (charTab[ic].charClass & ID_CHAR)) {
      addIdentChar(ic);
    }
    
    if (ic == ':') {		/* we have a keyword if id ends with : */
      addIdentChar(ic);
      ic = nextChar();
      unreadChar(ic);
      if (ic == EOF || (charTab[ic].charClass & ID_CHAR) == 0
	  || (charTab[ic].charClass & DIGIT) != 0) {
	if (identType == IDENTIFIER) {
	  /* first time through */
	  identType = KEYWORD;
	}
	break;
      }
      identType = SYMBOL_KEYWORD;
    } else {
      unreadChar(ic);
      break;
    }
  }
  
  lvalp->sval = copyStr(doneIdent());
  
  return (identType);
}

static int parseNumber(c, lvalp)
char	c;
YYSTYPE *lvalp;
{
  int		base, exponent, ic;
  double	num, sign, floatExponent;
  Boolean	mantissaParsed = false, isNegative = false, dotSeen = false;
  double scale;
  
  base = 10;
  exponent = 0;
  scale = 1;
  ic = c;

  if (ic != '-') {
    parseDigits(ic, 10, &num);
    ic = nextChar();
    if (ic == 'r') {
      base = num;
      ic = nextChar();
    } else {
      mantissaParsed = true;
    }
  }

  /*
   * here we've either
   *  a) parsed base, an 'r' and are sitting on the following character
   *  b) parsed the integer part of the mantissa, and are sitting on the char
   *     following it, or
   *  c) parsed nothing and are sitting on a - sign.
   */
  if (!mantissaParsed) {
    if (ic == '-') {
      isNegative = true;
      ic = nextChar();
    }
    
    parseDigits(ic, base, &num);
    ic = nextChar();
  }

  if (ic == '.') {
    ic = nextChar();
    if (!isDigit(ic)) {
      /* OOPS...we gobbled the '.' by mistake...it was a statement boundary
	 delimiter.  We have an integer that we need to return, and need to
	 push back both the . and the character that we just read. */
      unreadChar(ic);
      ic = '.';
    } else {
      dotSeen = true;
      parseFraction(ic, base, &num, &scale);
      ic = nextChar();
    }
  }

  if (ic == 'e') {
    ic = nextChar();
    if (ic == '-') {
      parseDigits(nextChar(), 10, &floatExponent);
      exponent -= floatExponent;
    } else {
      parseDigits(ic, 10, &floatExponent);
      exponent += floatExponent;
    }
  } else {
    unreadChar(ic);
  }

  if (scale != 0.0) {
    num *= scale/*pow((double)base, (double)exponent)*/;
  }

  if (exponent != 0) {
    num *= pow((double)base, (double)exponent);
  }

  if (isNegative) {
    num = -num;
  }
    
  if (dotSeen) {
    lvalp->fval = num;
    return (FLOATING_LITERAL);
  } else {
    lvalp->ival = num;
    if (num < -(1<<30) || num >= (1 << 30)) {
      /* at least warn the guy... */
      errorf("Integer literal %d too large to be represented in Smalltalk",
	     num);
      hadError = true;
    }
      
    return (INTEGER_LITERAL);
  }
}

static Boolean parseDigits(c, base, numPtr)
char	c;
int	base;
double	*numPtr;
{
  double	result;
  Boolean	oneDigit = false;

  for (result = 0.0; isBaseDigit(c, base); c = nextChar()) {
    result *= base;
    oneDigit = true;
    result += digitToInt(c, base);
  }

  if (!oneDigit) {
    errorf("Unexpected EOF while scanning number");
    hadError = true;
  }

  unreadChar(c);

  *numPtr = result;

  return (true);
}

static Boolean parseFraction(c, base, numPtr, scalePtr)
char	c;
int	base;
double	*numPtr, *scalePtr;
{
  double	scale;
  double	num;

  scale = 1.0;

  for (num = *numPtr; isBaseDigit(c, base); c = nextChar()) {
    num *= base;
    num += digitToInt(c, base);
    scale /= base;
  }

  unreadChar(c);

  *numPtr = num;
  *scalePtr = scale;

  return (true);
}


int digitToInt(c, base)
char	c;
int	base;
{
  if (c < '0' || (c > '9' && c < 'A') || c > 'Z') {
    errorf("Illegal digit %c in number", c);
    hadError = true;
    return (0);
  }

  if (c >= 'A') {
    c = c - 'A' + 10;
  } else {
    c -= '0';
  }

  if (c >= base) {
    errorf("Digit '%c' too large for base %d", c, base);
    hadError = true;
    return (0);
  }

  return (c);
}

static Boolean isBaseDigit(c, base)
char	c;
int	base;
{
  if (c < '0' || (c > '9' && c < 'A') || c > 'Z') {
    return (false);
  }
  
  if (c >= 'A') {
    c = c - 'A' + 10;
  } else {
    c -= '0';
  }

  return (c < base);
}


static Boolean isDigit(c)
char	c;
{
  return ((charTab[c].charClass & DIGIT) != 0);
}

static char *scanStringoid(startChar, typeName)
char	startChar, *typeName;
{
  int		ic;

  initStrBuf();

  for (;;) {
    ic = nextChar();
    if (ic == EOF) {
      errorf("Unterminated %s, attempting recovery", typeName);
      hadError = true;
      return (curStrBuf());
    }
    if (ic == startChar) {
      /* check for doubled delimiters */
      ic = nextChar();
      if (ic != startChar) {
	unreadChar(ic);
	return (curStrBuf());
      }
    }
    addStrBufChar(ic);
  }
  
}

/*
 * Report an error to the user.  ### Will show position in the text of
 * the error at some point.
 */
errorf(str, ERROR_ARGS)
char	*str;
int	ERROR_ARGS;
{
  fflush(stdout);
  lineStamp();
  fprintf(stderr, str, ERROR_ARGS);
  fprintf(stderr, "\n");
  fflush(stderr);
}


yyerror(s)
char	*s;
{
  lineStamp();
  fprintf(stderr, "%s\n", s);
}

static void lineStamp()
{
  if (inStream) {
    if (inStream->fileName) {
      fprintf(stderr, "\"%s\", ", inStream->fileName);
    }
    fprintf(stderr, "line %d: ", inStream->line);
  } else {
    fprintf(stderr, "<unknown position> ");
  }
}

StreamType getCurStreamType()
{
  if (inStream) {
    return (inStream->type);
  } else {
    return (unknownStreamType);
  }
}

OOP getCurString()
{
  if (inStream && inStream->type == stringStreamType) {
    return (stringNew(inStream->st_str.strBase));
  } else {
    return (nilOOP);
  }
}

OOP getCurFileName()
{
  if (inStream && inStream->type == fileStreamType) {
    return (stringNew(inStream->fileName));
  } else {
    return (nilOOP);
  }
}

#ifdef USE_READLINE
OOP getCurReadline()
{
  if (inStream && inStream->type == readlineStreamType) {
    return (stringNew(inStream->st_str.strBase));
  } else {
    return (nilOOP);
  }
}
#endif /* USE_READLINE */

int getMethodStartPos()
{
  return (methodStartPos);
}

void clearMethodStartPos()
{
  methodStartPos = -1;
}

int getCurFilePos()
{
  if (inStream && inStream->type == fileStreamType) {
    return (ftell(inStream->st_file));
  } else {
    return (-1);
  }
}

static int nextChar()
{
  register int		ic;

  if (inStream->pushedBackCount > 0) {
    ic = inStream->pushedBackChars[--inStream->pushedBackCount];
    return (ic);
  } else {
    if (inStream->column == 0 && inStream->prompt) {
      if (emacsProcess) {
	printf("%c", EMACS_PROCESS_MARKER);
      }
      printf("st> ");
    }
    ic = myGetC(inStream);

    /* byteAddr++; */
    if (ic == '\n') {		/* a new line that was not pushed back */
      inStream->line++;
      inStream->column = 0;
    } else {
      inStream->column++;
    }
    return (ic);
  }
}

/*
 *	static void unreadChar(ic)
 *
 * Description
 *
 *	Push character 'ic' back into the input queue.  Allows for two
 *	character pushback currently.  This solves the problem of lexing 3. and
 *	then finding out that what we should have lexed was 3 followed by . as
 *	a statement terminator.
 *
 * Inputs
 *
 *	ic    : character to push back into the input stream.
 *
 */
static void unreadChar(ic)
int	ic;
{
  inStream->pushedBackChars[inStream->pushedBackCount++] = ic;
}



/***********************************************************************
 *
 *	Generic "stream" interface.  A stream is an abstraction for input and
 *	output.  It is most like common lisp streams.  Basically, these streams
 *	provide transparent reading from either a Smalltalk string, or a UNIX
 *	file.  They stack, and the previous stream can be restored by doing a
 *	"popStream" which optionally "closes" the current stream and goes back 
 *	to using the previous stream.
 *
 * 	The `readline()' interface:
 *
 * 		The behavior is like the Smalltalk-String interface.
 * 		The end-of-string or a NULL strBase-pointer decides
 * 		to read in a new line. The prompt is still shown by
 * 		the readline() call.
 * 		
 ***********************************************************************/

void popStream(closeIt)
Boolean closeIt;
{
  Stream	oldStream;

  oldStream = inStream;
  inStream = inStream->prevStream;

  if (closeIt && oldStream) {
    myClose(oldStream);
    free(oldStream);
  }
}

void pushUNIXFile(file, fileName)
FILE	*file;
char	*fileName;
{
  Stream	newStream;

  newStream = pushNewStream(fileStreamType);

  newStream->st_file = file;
  newStream->fileName = fileName;
  newStream->prompt = isatty(fileno(file));
}

void pushSmalltalkString(stringOOP)
OOP	stringOOP;
{
  Stream	newStream;

  newStream = pushNewStream(stringStreamType);

  newStream->st_str.strBase = (char *)toCString(stringOOP);
  newStream->st_str.str = newStream->st_str.strBase;
  newStream->fileName = "a Smalltalk string";
  newStream->prompt = false;
}

#ifdef USE_READLINE
void pushReadlineString()
{
  Stream	newStream;

  newStream = pushNewStream(readlineStreamType);

  newStream->st_str.strBase = 0;	/* force readline() but no free() */
  newStream->st_str.str = 0;
  newStream->fileName = "a Readline string";
  newStream->prompt = false;		/* prompt is shown by readline() */
}
#endif /* USE_READLINE */

static Stream pushNewStream(type)
StreamType type;
{
  Stream	newStream;

  newStream = (Stream)malloc(sizeof(struct StreamStruct));

  newStream->pushedBackCount = 0;
  newStream->line = 1;
  newStream->column = 0;
  newStream->type = type;
  newStream->prevStream = inStream;
  inStream = newStream;

  return (newStream);
}


static int myGetC(stream)
Stream	stream;
{
  int		ic;

  if (stream->type == stringStreamType) {
    ic = *stream->st_str.str++;
    return ((ic == '\0') ? EOF : ic);
  } else if (stream->type == fileStreamType) {
    return (getc(stream->st_file));
#ifdef USE_READLINE
  } else if (stream->type == readlineStreamType) {
    char *r_line;
    extern char *realloc();
    int r_len;

    if (stream->st_str.strBase) {
      ic = *stream->st_str.str++;
    } else {
      ic = '\0';
    }

    if (ic == '\0') {
      if(stream->st_str.strBase) {
	free(stream->st_str.strBase);
	stream->st_str.strBase = 0;
      }
      r_line = readline("st> ");
      if (!r_line) {
	/*
	 * return value of NULL indicates EOF:
	 */
	return EOF;
      }
      if (*r_line) {
	/*
	 * add only non-empty lines.
	 */
	add_history(r_line);
      }
      /*
       * tack on the newline, not returned by readline() :
       */
      r_len =  strlen(r_line);
      r_line = realloc(r_line, (unsigned) (r_len + 2));
      if (!r_line) {
	errorf("Out of memory, reallocating linebuffer space");
	stream->st_str.str = stream->st_str.strBase = 0;
	ic = '\n';			/* return a newline ... */
      } else {
	r_line[r_len] = '\n';
	r_line[r_len+1] = '\0';
	stream->st_str.str = stream->st_str.strBase = r_line;
	ic = *stream->st_str.str++;
      }
    }
    return (ic);
#endif /* USE_READLINE */
  } else {
    errorf("Bad stream type passed to myGetC");
    hadError = true;
  }
}

static void myClose(stream)
Stream	stream;
{
  if (stream->type == stringStreamType) {
    free(stream->st_str.strBase);
  } else if (stream->type == fileStreamType) {
    fclose(stream->st_file);
#ifdef USE_READLINE
  } else if (stream->type == readlineStreamType) {
    if (stream->st_str.strBase) {
      free(stream->st_str.strBase);
      stream->st_str.strBase = 0;
    }
#endif /* USE_READLINE */
  } else {
    errorf("Bad stream type passed to myClose");
    hadError = true;
  }
}
