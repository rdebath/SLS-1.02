#include "SYSTEM_.h"

#ifndef DEFINITION_Checks
#include "Checks.h"
#endif

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Source
#include "Source.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

INTEGER Scanner_TokenLength;
Scanner_tScanAttribute Scanner_Attribute;
struct Scanner_2 Scanner_ScanTabName;
PROC Scanner_Exit;

static CARDINAL NestingLevel;
#define TokIntConst	2
#define TokCharConst	4
#define TokRealConst	6
#define TokStringConst	7
#define TokNotEqual	8
#define TokLParent	9
#define TokRParent	10
#define TokTimes	11
#define TokPlus	12
#define TokComma	13
#define TokMinus	14
#define TokDot	15
#define TokRange	16
#define TokDivide	17
#define TokColon	18
#define TokAssign	19
#define TokSemiColon	20
#define TokLess	21
#define TokLessEqual	22
#define TokEqual	24
#define TokGreater	25
#define TokGreaterEqual	26
#define TokLBracket	27
#define TokRBracket	28
#define TokArrow	29
#define TokLBrace	30
#define TokBar	31
#define TokRBrace	32
#define TokAnd	34
#define TokArray	35
#define TokBegin	36
#define TokBy	37
#define TokCase	38
#define TokConst	39
#define TokDefinition	40
#define TokDiv	41
#define TokDo	42
#define TokElse	43
#define TokElsif	44
#define TokEnd	45
#define TokExit	46
#define TokExport	47
#define TokFor	48
#define TokFrom	49
#define TokIf	50
#define TokImplementation	51
#define TokImport	52
#define TokIn	53
#define TokLoop	54
#define TokMod	55
#define TokModule	56
#define TokNot	57
#define TokOf	58
#define TokOr	59
#define TokPointer	60
#define TokProcedure	61
#define TokQualified	62
#define TokRecord	63
#define TokRepeat	64
#define TokReturn	65
#define TokSet	66
#define TokThen	67
#define TokTo	68
#define TokType	69
#define TokUntil	70
#define TokVar	71
#define TokWhile	72
#define TokWith	73
#define TokForeign	74
#define yyTabSpace	8
#define yyDNoState	0
#define yyFileStackSize	16
#define yyInitBufferSize	(1024 * 8 + 256)
#define yyFirstCh	'\0'
#define yyLastCh	((CHAR)'\177')
#define yyEolCh	'\n'
#define yyEobCh	((CHAR)'\177')
#define yyDStateCount	233
#define yyTableSize	832
#define yyEobState	30
#define yyDefaultState	31
#define STD	1
#define Comment	3
#define Str1	5
#define Str2	7
typedef SHORTCARD yyTableElmt;
typedef yyTableElmt yyStateRange;
typedef yyTableElmt yyTableRange;
typedef struct S_1 {
    yyStateRange Check, Next;
} yyCombType;
typedef yyCombType *yyCombTypePtr;
typedef struct S_2 {
    CHAR A[1000000 + 1];
} *yytChBufferPtr;
typedef CHAR yyChRange;
static struct S_3 {
    LONGCARD A[yyDStateCount + 1];
} yyBasePtr;
static struct S_4 {
    yyStateRange A[yyDStateCount + 1];
} yyDefault;
static struct S_5 {
    yyCombType A[yyTableSize + 1];
} yyComb;
static struct S_6 {
    yyStateRange A[yyDStateCount + 1];
} yyEobTrans;
static struct S_7 {
    CHAR A[yyLastCh + 1];
} yyToLower, yyToUpper;
static struct S_8 {
    yyStateRange A[1000000 + 1];
} *yyStateStack;
static LONGINT yyStateStackSize;
static yyStateRange yyStartState;
static yyStateRange yyPreviousStart;
static CHAR yyCh;
static System_tFile yySourceFile;
static BOOLEAN yyEof;
static yytChBufferPtr yyChBufferPtr;
static INTEGER yyChBufferStart;
static LONGINT yyChBufferSize;
static INTEGER yyChBufferIndex;
static INTEGER yyBytesRead;
static CARDINAL yyLineCount;
static INTEGER yyLineStart;
static SHORTCARD yyFileStackPtr;
static struct S_9 {
    struct S_10 {
        System_tFile SourceFile;
        BOOLEAN Eof;
        yytChBufferPtr ChBufferPtr;
        INTEGER ChBufferStart;
        LONGINT ChBufferSize;
        INTEGER ChBufferIndex;
        INTEGER BytesRead;
        CARDINAL LineCount;
        INTEGER LineStart;
    } A[yyFileStackSize - 1 + 1];
} yyFileStack;
static void yyInitialize ARGS(());
static void yyStart ARGS((yyStateRange State));
static void yyPrevious ARGS(());
static void yyEcho ARGS(());
static void yyLess ARGS((INTEGER n));
static void yyTab ARGS(());
static void yyTab1 ARGS((INTEGER a));
static void yyTab2 ARGS((INTEGER a, INTEGER b));
static void yyEol ARGS((INTEGER Column));
static void output ARGS((CHAR c));
static void unput ARGS((CHAR c));
static CHAR input ARGS(());
static void yyGetTables ARGS(());
struct S_12 {
    yyTableRange A[yyDStateCount + 1];
};
static CARDINAL yyGetTable ARGS((System_tFile TableFile, ADDRESS Address));
static void yyErrorMessage ARGS((SHORTCARD ErrorCode));
static void yyExit ARGS(());


void Scanner_ErrorAttribute
# ifdef __STDC__
(CARDINAL Token, Scanner_tScanAttribute *Attribute)
# else
(Token, Attribute)
CARDINAL Token;
Scanner_tScanAttribute *Attribute;
# endif
{
  switch (Token) {
  case Scanner_TokIdent:;
    Attribute->U_1.V_1.Ident = Idents_NoIdent;
    break;
  default :
    break;
  }
}

INTEGER Scanner_GetToken
# ifdef __STDC__
()
# else
()
# endif
{
  yyStateRange yyState;
  yyCombTypePtr yyTablePtr;
  BOOLEAN yyRestartFlag;
  INTEGER yyi, yySource, yyTarget, yyChBufferFree;
  Strings_tString Word;

  for (;;) {
    yyState = yyStartState;
    Scanner_TokenLength = 0;
    for (;;) {
      for (;;) {
        yyTablePtr = (yyCombTypePtr)(yyBasePtr.A[yyState] + ORD(yyChBufferPtr->A[yyChBufferIndex]) * sizeof(yyCombType));
        if (yyTablePtr->Check != yyState) {
          yyState = yyDefault.A[yyState];
          if (yyState == yyDNoState) {
            goto EXIT_3;
          }
        } else {
          yyState = yyTablePtr->Next;
          INC(Scanner_TokenLength);
          yyStateStack->A[Scanner_TokenLength] = yyState;
          INC(yyChBufferIndex);
        }
      } EXIT_3:;
      for (;;) {
        switch (yyStateStack->A[Scanner_TokenLength]) {
        case 232:;
          INC(NestingLevel);
          yyStart(Comment);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 231:;
          DEC(NestingLevel);
          if (NestingLevel == 0) {
            yyStart(STD);
          }
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 11:;
        case 22:;
        case 35:;
        case 230:;
        case 233:;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 14:;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 12:;
        case 23:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokIntConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 21:;
          DEC1(yyChBufferIndex, 2);
          DEC1(Scanner_TokenLength, 2);
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokIntConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 20:;
          DEC1(yyChBufferIndex, 2);
          DEC1(Scanner_TokenLength, 2);
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokIntConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 17:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokIntConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 19:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokIntConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 18:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokCharConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 15:;
        case 24:;
        case 25:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokRealConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 229:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          yyStart(Str1);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 10:;
        case 36:;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 228:;
          yyStart(STD);
          return TokStringConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 227:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          yyStart(Str2);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 9:;
        case 37:;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 226:;
          yyStart(STD);
          return TokStringConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 225:;
          yyTab();
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 224:;
          Errors_ErrorMessage(Errors_UnclosedString, Errors_Error, Scanner_Attribute.Position);
          yyEol(0);
          yyStart(STD);
          return TokStringConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 223:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokNotEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 222:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokAnd;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 221:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokLParent;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 220:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokRParent;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 219:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokTimes;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 218:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokPlus;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 217:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokComma;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 216:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokMinus;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 214:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokDot;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 215:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokRange;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 213:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokDivide;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 211:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokColon;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 212:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokAssign;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 210:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokSemiColon;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 207:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokLess;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 209:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokLessEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 208:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokNotEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 206:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 204:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokGreater;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 205:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokGreaterEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 203:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokLBracket;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 202:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokRBracket;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 201:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokArrow;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 200:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokLBrace;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 199:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokBar;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 198:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokRBrace;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 197:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokNot;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 196:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokAnd;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 194:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokArray;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 189:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokBegin;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 185:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokBy;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 183:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokCase;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 180:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 175:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokDefinition;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 166:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokDiv;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 164:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokDo;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 162:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokElse;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 161:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokElsif;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 157:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokEnd;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 155:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokExit;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 153:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokExport;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 40:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokFor;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 147:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokFrom;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 144:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokIf;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 143:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokImplementation;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 132:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokImport;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 127:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokIn;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 125:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokLoop;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 118:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokMod;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 121:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokModule;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 115:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokNot;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 112:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokOf;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 111:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokOr;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 109:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokPointer;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 103:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokProcedure;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 94:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokQualified;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 85:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokRecord;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 81:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokRepeat;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 77:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokReturn;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 71:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokSet;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 68:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokThen;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 65:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokTo;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 64:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokType;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 60:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokUntil;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 55:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokVar;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 52:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokWhile;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 48:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokWith;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 44:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return TokForeign;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 13:;
        case 38:;
        case 39:;
        case 41:;
        case 42:;
        case 43:;
        case 45:;
        case 46:;
        case 47:;
        case 49:;
        case 50:;
        case 51:;
        case 53:;
        case 54:;
        case 56:;
        case 57:;
        case 58:;
        case 59:;
        case 61:;
        case 62:;
        case 63:;
        case 66:;
        case 67:;
        case 69:;
        case 70:;
        case 72:;
        case 73:;
        case 74:;
        case 75:;
        case 76:;
        case 78:;
        case 79:;
        case 80:;
        case 82:;
        case 83:;
        case 84:;
        case 86:;
        case 87:;
        case 88:;
        case 89:;
        case 90:;
        case 91:;
        case 92:;
        case 93:;
        case 95:;
        case 96:;
        case 97:;
        case 98:;
        case 99:;
        case 100:;
        case 101:;
        case 102:;
        case 104:;
        case 105:;
        case 106:;
        case 107:;
        case 108:;
        case 110:;
        case 113:;
        case 114:;
        case 116:;
        case 117:;
        case 119:;
        case 120:;
        case 122:;
        case 123:;
        case 124:;
        case 126:;
        case 128:;
        case 129:;
        case 130:;
        case 131:;
        case 133:;
        case 134:;
        case 135:;
        case 136:;
        case 137:;
        case 138:;
        case 139:;
        case 140:;
        case 141:;
        case 142:;
        case 145:;
        case 146:;
        case 148:;
        case 149:;
        case 150:;
        case 151:;
        case 152:;
        case 154:;
        case 156:;
        case 158:;
        case 159:;
        case 160:;
        case 163:;
        case 165:;
        case 167:;
        case 168:;
        case 169:;
        case 170:;
        case 171:;
        case 172:;
        case 173:;
        case 174:;
        case 176:;
        case 177:;
        case 178:;
        case 179:;
        case 181:;
        case 182:;
        case 184:;
        case 186:;
        case 187:;
        case 188:;
        case 190:;
        case 191:;
        case 192:;
        case 193:;
        case 195:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Scanner_Attribute.U_1.V_1.Ident = Idents_MakeIdent(&Word);
          return Scanner_TokIdent;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 34:;
          while (yyChBufferPtr->A[yyChBufferIndex] == ' ') {
            INC(yyChBufferIndex);
          }
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 33:;
          DEC1(yyLineStart, 7 - (yyChBufferIndex - yyLineStart - 2) % 8);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 32:;
          INC(yyLineCount);
          yyLineStart = yyChBufferIndex - 1;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 1:;
        case 2:;
        case 3:;
        case 4:;
        case 5:;
        case 6:;
        case 7:;
        case 8:;
        case 16:;
        case 26:;
        case 27:;
        case 28:;
        case 29:;
          DEC(yyChBufferIndex);
          DEC(Scanner_TokenLength);
          break;
        case 31:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart;
          INC(yyChBufferIndex);
          Scanner_TokenLength = 1;
          Errors_ErrorMessage(Errors_IllegalChar, Errors_Error, Scanner_Attribute.Position);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case yyDNoState:;
          yyGetTables();
          yyStateStack->A[0] = yyDefaultState;
          if (yyFileStackPtr == 0) {
            yyInitialize();
            yySourceFile = System_StdInput;
          }
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 30:;
          DEC(yyChBufferIndex);
          DEC(Scanner_TokenLength);
          if (Scanner_TokenLength == 0) {
            yyState = yyStartState;
          } else {
            yyState = yyStateStack->A[Scanner_TokenLength];
          }
          if (yyChBufferIndex != yyChBufferStart + yyBytesRead) {
            yyState = yyEobTrans.A[yyState];
            if (yyState != yyDNoState) {
              INC(yyChBufferIndex);
              INC(Scanner_TokenLength);
              yyStateStack->A[Scanner_TokenLength] = yyState;
              yyRestartFlag = TRUE;
              goto EXIT_4;
            }
          } else {
            yySource = yyChBufferIndex - Scanner_TokenLength - 1;
            yyTarget = General_MaxAlign - Scanner_TokenLength % General_MaxAlign - 1;
            if (yySource != yyTarget) {
              {
                LONGINT B_1 = 1, B_2 = Scanner_TokenLength;

                if (B_1 <= B_2)
                  for (yyi = B_1;; yyi += 1) {
                    yyChBufferPtr->A[yyTarget + yyi] = yyChBufferPtr->A[yySource + yyi];
                    if (yyi >= B_2) break;
                  }
              }
              DEC1(yyLineStart, yySource - yyTarget);
              yyChBufferStart = yyTarget + Scanner_TokenLength + 1;
            } else {
              yyChBufferStart = yyChBufferIndex;
            }
            if (!yyEof) {
              yyChBufferFree = General_Exp2(General_Log2(yyChBufferSize - 4 - General_MaxAlign - Scanner_TokenLength));
              if (yyChBufferFree < yyChBufferSize / 8) {
                DynArray_ExtendArray(&yyChBufferPtr, &yyChBufferSize, sizeof(CHAR));
                if (yyChBufferPtr == NIL) {
                  yyErrorMessage(1);
                }
                yyChBufferFree = General_Exp2(General_Log2(yyChBufferSize - 4 - General_MaxAlign - Scanner_TokenLength));
                if (yyStateStackSize < yyChBufferSize) {
                  DynArray_ExtendArray(&yyStateStack, &yyStateStackSize, sizeof(yyStateRange));
                  if (yyStateStack == NIL) {
                    yyErrorMessage(1);
                  }
                }
              }
              yyChBufferIndex = yyChBufferStart;
              yyBytesRead = Source_GetLine(yySourceFile, ADR(yyChBufferPtr->A[yyChBufferIndex]), yyChBufferFree);
              if (yyBytesRead <= 0) {
                yyBytesRead = 0;
                yyEof = TRUE;
              }
              yyChBufferPtr->A[yyChBufferStart + yyBytesRead] = yyEobCh;
              yyChBufferPtr->A[yyChBufferStart + yyBytesRead + 1] = '\0';
              yyRestartFlag = TRUE;
              goto EXIT_4;
            }
            if (Scanner_TokenLength == 0) {
              Scanner_Attribute.Position.Line = yyLineCount;
              Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart;
              Scanner_CloseFile();
              if (yyFileStackPtr == 0) {
                if (yyStartState == Comment) {
                  Errors_ErrorMessage(Errors_UnclosedComment, Errors_Error, Scanner_Attribute.Position);
                }
              }
              if (yyFileStackPtr == 0) {
                return Scanner_EofToken;
              }
              yyRestartFlag = FALSE;
              goto EXIT_4;
            }
          }
          break;
        default :
          yyErrorMessage(0);
          break;
        }
      } EXIT_4:;
      if (yyRestartFlag) {
      } else {
        goto EXIT_2;
      }
    } EXIT_2:;
  } EXIT_1:;
}

void Scanner_BeginFile
# ifdef __STDC__
(CHAR FileName[], LONGCARD O_1)
# else
(FileName, O_1)
CHAR FileName[];
LONGCARD O_1;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(FileName, O_1, CHAR)
  if (yyStateStack->A[0] == yyDNoState) {
    yyGetTables();
    yyStateStack->A[0] = yyDefaultState;
  }
  yyInitialize();
  yySourceFile = Source_BeginSource(FileName, O_1);
  if (yySourceFile < 0) {
    yyErrorMessage(5);
  }
  FREE_OPEN_ARRAYS
}

static void yyInitialize
# ifdef __STDC__
()
# else
()
# endif
{
  if (yyFileStackPtr >= yyFileStackSize) {
    yyErrorMessage(3);
  }
  INC(yyFileStackPtr);
  {
    register struct S_10 *W_1 = &yyFileStack.A[yyFileStackPtr - 1];

    W_1->SourceFile = yySourceFile;
    W_1->Eof = yyEof;
    W_1->ChBufferPtr = yyChBufferPtr;
    W_1->ChBufferStart = yyChBufferStart;
    W_1->ChBufferSize = yyChBufferSize;
    W_1->ChBufferIndex = yyChBufferIndex;
    W_1->BytesRead = yyBytesRead;
    W_1->LineCount = yyLineCount;
    W_1->LineStart = yyLineStart;
  }
  yyChBufferSize = yyInitBufferSize;
  DynArray_MakeArray(&yyChBufferPtr, &yyChBufferSize, sizeof(CHAR));
  if (yyChBufferPtr == NIL) {
    yyErrorMessage(1);
  }
  yyChBufferStart = General_MaxAlign;
  yyChBufferPtr->A[yyChBufferStart - 1] = yyEolCh;
  yyChBufferPtr->A[yyChBufferStart] = yyEobCh;
  yyChBufferPtr->A[yyChBufferStart + 1] = '\0';
  yyChBufferIndex = yyChBufferStart;
  yyEof = FALSE;
  yyBytesRead = 0;
  yyLineCount = 1;
  yyLineStart = yyChBufferStart - 1;
}

void Scanner_CloseFile
# ifdef __STDC__
()
# else
()
# endif
{
  if (yyFileStackPtr == 0) {
    yyErrorMessage(4);
  }
  Source_CloseSource(yySourceFile);
  DynArray_ReleaseArray(&yyChBufferPtr, &yyChBufferSize, sizeof(CHAR));
  {
    register struct S_10 *W_2 = &yyFileStack.A[yyFileStackPtr - 1];

    yySourceFile = W_2->SourceFile;
    yyEof = W_2->Eof;
    yyChBufferPtr = W_2->ChBufferPtr;
    yyChBufferStart = W_2->ChBufferStart;
    yyChBufferSize = W_2->ChBufferSize;
    yyChBufferIndex = W_2->ChBufferIndex;
    yyBytesRead = W_2->BytesRead;
    yyLineCount = W_2->LineCount;
    yyLineStart = W_2->LineStart;
  }
  DEC(yyFileStackPtr);
}

void Scanner_GetWord
# ifdef __STDC__
(Strings_tString *Word)
# else
(Word)
Strings_tString *Word;
# endif
{
  INTEGER i, WordStart;

  WordStart = yyChBufferIndex - Scanner_TokenLength - 1;
  {
    LONGINT B_3 = 1, B_4 = Scanner_TokenLength;

    if (B_3 <= B_4)
      for (i = B_3;; i += 1) {
        Word->Chars.A[i] = yyChBufferPtr->A[WordStart + i];
        if (i >= B_4) break;
      }
  }
  Word->Length = Scanner_TokenLength;
}

void Scanner_GetLower
# ifdef __STDC__
(Strings_tString *Word)
# else
(Word)
Strings_tString *Word;
# endif
{
  INTEGER i, WordStart;

  WordStart = yyChBufferIndex - Scanner_TokenLength - 1;
  {
    LONGINT B_5 = 1, B_6 = Scanner_TokenLength;

    if (B_5 <= B_6)
      for (i = B_5;; i += 1) {
        Word->Chars.A[i] = yyToLower.A[yyChBufferPtr->A[WordStart + i]];
        if (i >= B_6) break;
      }
  }
  Word->Length = Scanner_TokenLength;
}

void Scanner_GetUpper
# ifdef __STDC__
(Strings_tString *Word)
# else
(Word)
Strings_tString *Word;
# endif
{
  INTEGER i, WordStart;

  WordStart = yyChBufferIndex - Scanner_TokenLength - 1;
  {
    LONGINT B_7 = 1, B_8 = Scanner_TokenLength;

    if (B_7 <= B_8)
      for (i = B_7;; i += 1) {
        Word->Chars.A[i] = yyToUpper.A[yyChBufferPtr->A[WordStart + i]];
        if (i >= B_8) break;
      }
  }
  Word->Length = Scanner_TokenLength;
}

static void yyStart
# ifdef __STDC__
(yyStateRange State)
# else
(State)
yyStateRange State;
# endif
{
  yyPreviousStart = yyStartState;
  yyStartState = State;
}

static void yyPrevious
# ifdef __STDC__
()
# else
()
# endif
{
  yyStateRange s;

  s = yyStartState;
  yyStartState = yyPreviousStart;
  yyPreviousStart = s;
}

static void yyEcho
# ifdef __STDC__
()
# else
()
# endif
{
  INTEGER i;

  {
    LONGINT B_9 = yyChBufferIndex - Scanner_TokenLength, B_10 = yyChBufferIndex - 1;

    if (B_9 <= B_10)
      for (i = B_9;; i += 1) {
        IO_WriteC(IO_StdOutput, yyChBufferPtr->A[i]);
        if (i >= B_10) break;
      }
  }
}

static void yyLess
# ifdef __STDC__
(INTEGER n)
# else
(n)
INTEGER n;
# endif
{
  DEC1(yyChBufferIndex, Scanner_TokenLength - n);
  Scanner_TokenLength = n;
}

static void yyTab
# ifdef __STDC__
()
# else
()
# endif
{
  DEC1(yyLineStart, yyTabSpace - 1 - (yyChBufferIndex - yyLineStart - 2) % yyTabSpace);
}

static void yyTab1
# ifdef __STDC__
(INTEGER a)
# else
(a)
INTEGER a;
# endif
{
  DEC1(yyLineStart, yyTabSpace - 1 - (yyChBufferIndex - yyLineStart - Scanner_TokenLength + a - 1) % yyTabSpace);
}

static void yyTab2
# ifdef __STDC__
(INTEGER a, INTEGER b)
# else
(a, b)
INTEGER a, b;
# endif
{
  DEC1(yyLineStart, yyTabSpace - 1 - (yyChBufferIndex - yyLineStart - Scanner_TokenLength + a - 1) % yyTabSpace);
}

static void yyEol
# ifdef __STDC__
(INTEGER Column)
# else
(Column)
INTEGER Column;
# endif
{
  INC(yyLineCount);
  yyLineStart = yyChBufferIndex - 1 - Column;
}

static void output
# ifdef __STDC__
(CHAR c)
# else
(c)
CHAR c;
# endif
{
  IO_WriteC(IO_StdOutput, c);
}

static void unput
# ifdef __STDC__
(CHAR c)
# else
(c)
CHAR c;
# endif
{
  DEC(yyChBufferIndex);
  yyChBufferPtr->A[yyChBufferIndex] = c;
}

static CHAR input
# ifdef __STDC__
()
# else
()
# endif
{
  if (yyChBufferIndex == yyChBufferStart + yyBytesRead) {
    if (!yyEof) {
      DEC1(yyLineStart, yyBytesRead);
      yyChBufferIndex = 0;
      yyChBufferStart = 0;
      yyBytesRead = Source_GetLine(yySourceFile, yyChBufferPtr, General_Exp2(General_Log2(yyChBufferSize)));
      if (yyBytesRead <= 0) {
        yyBytesRead = 0;
        yyEof = TRUE;
      }
      yyChBufferPtr->A[yyBytesRead] = yyEobCh;
      yyChBufferPtr->A[yyBytesRead + 1] = '\0';
    }
  }
  if (yyChBufferIndex == yyChBufferStart + yyBytesRead) {
    return '\0';
  } else {
    INC(yyChBufferIndex);
    return yyChBufferPtr->A[yyChBufferIndex - 1];
  }
}

void Scanner_BeginScanner
# ifdef __STDC__
()
# else
()
# endif
{
  NestingLevel = 0;
}

void Scanner_CloseScanner
# ifdef __STDC__
()
# else
()
# endif
{
}

static void yyGetTables
# ifdef __STDC__
()
# else
()
# endif
{
  CARDINAL BlockSize, j, n;
  System_tFile TableFile;
  yyStateRange i;
  struct S_12 Base;

  BlockSize = 64000 / sizeof(yyCombType);
  TableFile = OpenInput(Scanner_ScanTabName.A, 128L);
  Checks_ErrorCheck("yyGetTables.OpenInput", 21L, TableFile);
  if (yyGetTable(TableFile, ADR(Base)) / sizeof(yyTableElmt) - 1 != yyDStateCount || yyGetTable(TableFile, ADR(yyDefault)) / sizeof(yyTableElmt) - 1 != yyDStateCount || yyGetTable(TableFile, ADR(yyEobTrans)) / sizeof(yyTableElmt) - 1 != yyDStateCount) {
    yyErrorMessage(2);
  }
  n = 0;
  j = 0;
  while (j <= yyTableSize) {
    INC1(n, yyGetTable(TableFile, ADR(yyComb.A[j])) / sizeof(yyCombType));
    INC1(j, BlockSize);
  }
  if (n != yyTableSize + 1) {
    yyErrorMessage(2);
  }
  Close(TableFile);
  for (i = 0; i <= yyDStateCount; i += 1) {
    yyBasePtr.A[i] = (LONGCARD)ADR(yyComb.A[Base.A[i]]);
  }
}

static CARDINAL yyGetTable
# ifdef __STDC__
(System_tFile TableFile, ADDRESS Address)
# else
(TableFile, Address)
System_tFile TableFile;
ADDRESS Address;
# endif
{
  INTEGER N;
  yyTableElmt Length;

  N = Read(TableFile, ADR(Length), sizeof(yyTableElmt));
  Checks_ErrorCheck("yyGetTable.Read1", 16L, N);
  N = Read(TableFile, Address, Length);
  Checks_ErrorCheck("yyGetTable.Read2", 16L, N);
  return Length;
}

static void yyErrorMessage
# ifdef __STDC__
(SHORTCARD ErrorCode)
# else
(ErrorCode)
SHORTCARD ErrorCode;
# endif
{
  Positions_WritePosition(IO_StdError, Scanner_Attribute.Position);
  switch (ErrorCode) {
  case 0:;
    IO_WriteS(IO_StdError, ": Scanner: internal error", 25L);
    break;
  case 1:;
    IO_WriteS(IO_StdError, ": Scanner: out of memory", 24L);
    break;
  case 2:;
    IO_WriteS(IO_StdError, ": Scanner: table mismatch", 25L);
    break;
  case 3:;
    IO_WriteS(IO_StdError, ": Scanner: too many nested include files", 40L);
    break;
  case 4:;
    IO_WriteS(IO_StdError, ": Scanner: file stack underflow (too many calls of CloseFile)", 61L);
    break;
  case 5:;
    IO_WriteS(IO_StdError, ": Scanner: cannot open input file", 33L);
    break;
  }
  IO_WriteNl(IO_StdError);
  (*Scanner_Exit)();
}

static void yyExit
# ifdef __STDC__
()
# else
()
# endif
{
  IO_CloseIO();
  Exit(1);
}

void BEGIN_Scanner()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Strings();
    BEGIN_Idents();
    BEGIN_Positions();
    BEGIN_Checks();
    BEGIN_System();
    BEGIN_General();
    BEGIN_Positions();
    BEGIN_IO();
    BEGIN_DynArray();
    BEGIN_Strings();
    BEGIN_Source();
    BEGIN_Strings();
    BEGIN_Idents();
    BEGIN_Errors();

    (void)strncpy(Scanner_ScanTabName.A, "Scanner.Tab", sizeof(Scanner_ScanTabName.A));
    Scanner_Exit = yyExit;
    yyFileStackPtr = 0;
    yyStartState = 1;
    yyPreviousStart = 1;
    yyBasePtr.A[yyStartState] = (LONGCARD)ADR(yyComb.A[0]);
    yyDefault.A[yyStartState] = yyDNoState;
    yyComb.A[0].Check = yyDNoState;
    yyChBufferPtr = (yytChBufferPtr)ADR(yyComb.A[0]);
    yyChBufferIndex = 1;
    yyStateStackSize = yyInitBufferSize;
    DynArray_MakeArray(&yyStateStack, &yyStateStackSize, sizeof(yyStateRange));
    yyStateStack->A[0] = yyDNoState;
    for (yyCh = yyFirstCh; yyCh <= yyLastCh; yyCh += 1) {
      yyToLower.A[yyCh] = yyCh;
    }
    yyToUpper = yyToLower;
    for (yyCh = 'A'; yyCh <= 'Z'; yyCh += 1) {
      yyToLower.A[yyCh] = CHR(ORD(yyCh) - ORD('A') + ORD('a'));
    }
    for (yyCh = 'a'; yyCh <= 'z'; yyCh += 1) {
      yyToUpper.A[yyCh] = CHR(ORD(yyCh) - ORD('a') + ORD('A'));
    }
  }
}
