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

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Tokens
#include "Tokens.h"
#endif

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

INTEGER Scanner_TokenLength;
Scanner_tScanAttribute Scanner_Attribute;
struct Scanner_2 Scanner_ScanTabName;
PROC Scanner_Exit;

static CARDINAL NestingLevel;
static StringMem_tStringRef DefaultString, DefaultReal;
static Strings_tString String;
static BOOLEAN UnderscoreUsed;
#define yyTabSpace	8
#define yyDNoState	0
#define yyFileStackSize	16
#define yyInitBufferSize	(1024 * 8 + 256)
#define yyFirstCh	'\0'
#define yyLastCh	((CHAR)'\177')
#define yyEolCh	'\n'
#define yyEobCh	((CHAR)'\177')
#define yyDStateCount	235
#define yyTableSize	897
#define yyEobState	31
#define yyDefaultState	32
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
  case Tokens_TokIdent:;
    Attribute->U_1.V_1.Ident = Idents_NoIdent;
    break;
  case Tokens_TokDecConst:;
  case Tokens_TokOctalConst:;
  case Tokens_TokHexConst:;
    Attribute->U_1.V_2.IntValue = 1;
    break;
  case Tokens_TokCharConst:;
    Attribute->U_1.V_3.CharValue = CHR(0);
    break;
  case Tokens_TokRealConst:;
    Attribute->U_1.V_4.RealValue = DefaultReal;
    break;
  case Tokens_TokStringConst:;
    Attribute->U_1.V_5.StringValue = DefaultString;
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
  Strings_tString String, S, Word;

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
        case 235:;
          NestingLevel = 1;
          yyStart(Comment);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 234:;
          INC(NestingLevel);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 232:;
          DEC(NestingLevel);
          if (NestingLevel == 0) {
            yyStart(STD);
          }
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 11:;
        case 23:;
        case 36:;
        case 231:;
        case 233:;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 14:;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 12:;
        case 24:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Scanner_Attribute.U_1.V_2.IntValue = Strings_StringToNumber(&Word, 10L);
          return Tokens_TokDecConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 22:;
          DEC1(yyChBufferIndex, 2);
          DEC1(Scanner_TokenLength, 2);
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Scanner_Attribute.U_1.V_2.IntValue = Strings_StringToNumber(&Word, 10L);
          return Tokens_TokDecConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 21:;
          DEC1(yyChBufferIndex, 2);
          DEC1(Scanner_TokenLength, 2);
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Scanner_Attribute.U_1.V_2.IntValue = Strings_StringToNumber(&Word, 10L);
          return Tokens_TokDecConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 18:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Strings_SubString(&Word, 1, (Strings_tStringIndex)(Strings_Length(&Word) - 1), &String);
          Scanner_Attribute.U_1.V_2.IntValue = Strings_StringToNumber(&String, 8L);
          return Tokens_TokOctalConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 19:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Strings_SubString(&Word, 1, (Strings_tStringIndex)(Strings_Length(&Word) - 1), &String);
          Scanner_Attribute.U_1.V_3.CharValue = CHR(Strings_StringToNumber(&String, 8L));
          return Tokens_TokCharConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 20:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Strings_SubString(&Word, 1, (Strings_tStringIndex)(Strings_Length(&Word) - 1), &String);
          Scanner_Attribute.U_1.V_2.IntValue = Strings_StringToNumber(&String, 16L);
          return Tokens_TokHexConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 16:;
        case 25:;
        case 26:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Scanner_Attribute.U_1.V_4.RealValue = StringMem_PutString(&Word);
          return Tokens_TokRealConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 230:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Strings_AssignEmpty(&String);
          yyStart(Str1);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 10:;
        case 37:;
          Scanner_GetWord(&S);
          Strings_Concatenate(&String, &S);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 229:;
          yyStart(STD);
          Scanner_Attribute.U_1.V_5.StringValue = StringMem_PutString(&String);
          return Tokens_TokStringConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 228:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Strings_AssignEmpty(&String);
          yyStart(Str2);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 9:;
        case 38:;
          Scanner_GetWord(&S);
          Strings_Concatenate(&String, &S);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 227:;
          yyStart(STD);
          Scanner_Attribute.U_1.V_5.StringValue = StringMem_PutString(&String);
          return Tokens_TokStringConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 226:;
          Strings_Append(&String, '\t');
          yyTab();
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 225:;
          yyEol(0L);
          yyStart(STD);
          Errors_ErrorMessageP((LONGCARD)Errors_UnclosedString, (LONGCARD)Errors_Error, Scanner_Attribute.Position);
          Scanner_Attribute.U_1.V_5.StringValue = StringMem_PutString(&String);
          return Tokens_TokStringConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 224:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokNotEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 223:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokAnd;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 222:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokLParent;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 221:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokRParent;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 220:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokTimes;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 219:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokPlus;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 218:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokComma;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 217:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokMinus;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 215:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokDot;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 216:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokRange;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 214:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokDivide;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 212:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokColon;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 213:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokAssign;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 211:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokSemiColon;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 208:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokLess;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 210:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokLessEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 209:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokNotEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 207:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 205:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokGreater;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 206:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokGreaterEqual;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 204:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokLBracket;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 203:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokRBracket;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 202:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokArrow;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 201:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokLBrace;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 200:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokBar;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 199:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokRBrace;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 198:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokNot;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 197:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokAnd;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 195:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokArray;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 190:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokBegin;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 186:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokBy;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 184:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokCase;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 181:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokConst;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 176:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokDefinition;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 167:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokDiv;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 165:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokDo;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 163:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokElse;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 162:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokElsif;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 158:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokEnd;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 156:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokExit;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 154:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokExport;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 41:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokFor;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 148:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokFrom;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 145:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokIf;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 144:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokImplementation;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 133:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokImport;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 128:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokIn;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 126:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokLoop;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 119:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokMod;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 122:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokModule;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 116:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokNot;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 113:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokOf;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 112:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokOr;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 110:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokPointer;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 104:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokProcedure;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 95:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokQualified;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 86:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokRecord;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 82:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokRepeat;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 78:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokReturn;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 72:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokSet;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 69:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokThen;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 66:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokTo;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 65:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokType;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 61:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokUntil;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 56:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokVar;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 53:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokWhile;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 49:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokWith;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 45:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          return Tokens_TokForeign;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 13:;
        case 39:;
        case 40:;
        case 42:;
        case 43:;
        case 44:;
        case 46:;
        case 47:;
        case 48:;
        case 50:;
        case 51:;
        case 52:;
        case 54:;
        case 55:;
        case 57:;
        case 58:;
        case 59:;
        case 60:;
        case 62:;
        case 63:;
        case 64:;
        case 67:;
        case 68:;
        case 70:;
        case 71:;
        case 73:;
        case 74:;
        case 75:;
        case 76:;
        case 77:;
        case 79:;
        case 80:;
        case 81:;
        case 83:;
        case 84:;
        case 85:;
        case 87:;
        case 88:;
        case 89:;
        case 90:;
        case 91:;
        case 92:;
        case 93:;
        case 94:;
        case 96:;
        case 97:;
        case 98:;
        case 99:;
        case 100:;
        case 101:;
        case 102:;
        case 103:;
        case 105:;
        case 106:;
        case 107:;
        case 108:;
        case 109:;
        case 111:;
        case 114:;
        case 115:;
        case 117:;
        case 118:;
        case 120:;
        case 121:;
        case 123:;
        case 124:;
        case 125:;
        case 127:;
        case 129:;
        case 130:;
        case 131:;
        case 132:;
        case 134:;
        case 135:;
        case 136:;
        case 137:;
        case 138:;
        case 139:;
        case 140:;
        case 141:;
        case 142:;
        case 143:;
        case 146:;
        case 147:;
        case 149:;
        case 150:;
        case 151:;
        case 152:;
        case 153:;
        case 155:;
        case 157:;
        case 159:;
        case 160:;
        case 161:;
        case 164:;
        case 166:;
        case 168:;
        case 169:;
        case 170:;
        case 171:;
        case 172:;
        case 173:;
        case 174:;
        case 175:;
        case 177:;
        case 178:;
        case 179:;
        case 180:;
        case 182:;
        case 183:;
        case 185:;
        case 187:;
        case 188:;
        case 189:;
        case 191:;
        case 192:;
        case 193:;
        case 194:;
        case 196:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          Scanner_GetWord(&Word);
          Scanner_Attribute.U_1.V_1.Ident = Idents_MakeIdent(&Word);
          return Tokens_TokIdent;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 15:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart - Scanner_TokenLength;
          UnderscoreUsed = TRUE;
          Scanner_GetWord(&Word);
          Scanner_Attribute.U_1.V_1.Ident = Idents_MakeIdent(&Word);
          return Tokens_TokIdent;
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 35:;
          while (yyChBufferPtr->A[yyChBufferIndex] == ' ') {
            INC(yyChBufferIndex);
          }
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 34:;
          DEC1(yyLineStart, 7 - (yyChBufferIndex - yyLineStart - 2) % 8);
          yyRestartFlag = FALSE;
          goto EXIT_4;
          break;
        case 33:;
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
        case 17:;
        case 27:;
        case 28:;
        case 29:;
        case 30:;
          DEC(yyChBufferIndex);
          DEC(Scanner_TokenLength);
          break;
        case 32:;
          Scanner_Attribute.Position.Line = yyLineCount;
          Scanner_Attribute.Position.Column = yyChBufferIndex - yyLineStart;
          INC(yyChBufferIndex);
          Scanner_TokenLength = 1;
          Errors_ErrorMessageP((LONGCARD)Errors_IllegalChar, (LONGCARD)Errors_Error, Scanner_Attribute.Position);
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
        case 31:;
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
                DynArray_ExtendArray((ADDRESS *)&yyChBufferPtr, &yyChBufferSize, (LONGINT)sizeof(CHAR));
                if (yyChBufferPtr == NIL) {
                  yyErrorMessage(1);
                }
                yyChBufferFree = General_Exp2(General_Log2(yyChBufferSize - 4 - General_MaxAlign - Scanner_TokenLength));
                if (yyStateStackSize < yyChBufferSize) {
                  DynArray_ExtendArray((ADDRESS *)&yyStateStack, &yyStateStackSize, (LONGINT)sizeof(yyStateRange));
                  if (yyStateStack == NIL) {
                    yyErrorMessage(1);
                  }
                }
              }
              yyChBufferIndex = yyChBufferStart;
              yyBytesRead = Source_GetLine(yySourceFile, ADR(yyChBufferPtr->A[yyChBufferIndex]), (LONGCARD)yyChBufferFree);
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
                  Errors_ErrorMessageP((LONGCARD)Errors_UnclosedComment, (LONGCARD)Errors_Error, Scanner_Attribute.Position);
                  NestingLevel = 0;
                }
                if (yyStartState != STD) {
                  yyStart(STD);
                }
                if (UnderscoreUsed) {
                  Errors_ErrorMessageP((LONGCARD)Errors_Underscores, (LONGCARD)Errors_Warning, Scanner_Attribute.Position);
                  UnderscoreUsed = FALSE;
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
  DynArray_MakeArray((ADDRESS *)&yyChBufferPtr, &yyChBufferSize, (LONGINT)sizeof(CHAR));
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
  DynArray_ReleaseArray((ADDRESS *)&yyChBufferPtr, &yyChBufferSize, (LONGINT)sizeof(CHAR));
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
        IO_WriteC((System_tFile)IO_StdOutput, yyChBufferPtr->A[i]);
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
  IO_WriteC((System_tFile)IO_StdOutput, c);
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
      yyBytesRead = Source_GetLine(yySourceFile, (ADDRESS)yyChBufferPtr, (LONGCARD)General_Exp2(General_Log2(yyChBufferSize)));
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
  UnderscoreUsed = FALSE;
  NestingLevel = 0;
  Strings_AssignEmpty(&String);
  DefaultString = StringMem_PutString(&String);
  Strings_ArrayToString((STRING)"1.0", 3L, &String);
  DefaultReal = StringMem_PutString(&String);
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
  Checks_ErrorCheck((STRING)"yyGetTables.OpenInput", 21L, TableFile);
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

  N = Read(TableFile, ADR(Length), (LONGINT)sizeof(yyTableElmt));
  Checks_ErrorCheck((STRING)"yyGetTable.Read1", 16L, N);
  N = Read(TableFile, Address, (LONGINT)Length);
  Checks_ErrorCheck((STRING)"yyGetTable.Read2", 16L, N);
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
  Positions_WritePosition((System_tFile)IO_StdError, Scanner_Attribute.Position);
  switch (ErrorCode) {
  case 0:;
    IO_WriteS((System_tFile)IO_StdError, (STRING)": Scanner: internal error", 25L);
    break;
  case 1:;
    IO_WriteS((System_tFile)IO_StdError, (STRING)": Scanner: out of memory", 24L);
    break;
  case 2:;
    IO_WriteS((System_tFile)IO_StdError, (STRING)": Scanner: table mismatch", 25L);
    break;
  case 3:;
    IO_WriteS((System_tFile)IO_StdError, (STRING)": Scanner: too many nested include files", 40L);
    break;
  case 4:;
    IO_WriteS((System_tFile)IO_StdError, (STRING)": Scanner: file stack underflow (too many calls of CloseFile)", 61L);
    break;
  case 5:;
    IO_WriteS((System_tFile)IO_StdError, (STRING)": Scanner: cannot open input file", 33L);
    break;
  }
  IO_WriteNl((System_tFile)IO_StdError);
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
  Exit(1L);
}

void BEGIN_Scanner()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Strings();
    BEGIN_StringMem();
    BEGIN_Idents();
    BEGIN_Tokens();
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
    BEGIN_StringMem();
    BEGIN_Idents();
    BEGIN_Errors();
    BEGIN_Tokens();

    (void)strncpy((char *)Scanner_ScanTabName.A, "Scanner.Tab", sizeof(Scanner_ScanTabName.A));
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
    DynArray_MakeArray((ADDRESS *)&yyStateStack, &yyStateStackSize, (LONGINT)sizeof(yyStateRange));
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
