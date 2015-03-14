#include "SYSTEM_.h"

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Tokens
#include "Tokens.h"
#endif


#define BitsPerBitset	32
static BOOLEAN IsElement ARGS((Tokens_tTokenSet *Set, Tokens_tToken Elmt));


static BOOLEAN IsElement
# ifdef __STDC__
(Tokens_tTokenSet *Set, Tokens_tToken Elmt)
# else
(Set, Elmt)
Tokens_tTokenSet *Set;
Tokens_tToken Elmt;
# endif
{
  return IN(Elmt % BitsPerBitset, Set->A[Elmt / BitsPerBitset]);
}

void Tokens_WriteTokenSet
# ifdef __STDC__
(IO_tFile f, Tokens_tTokenSet TokenSet)
# else
(f, TokenSet)
IO_tFile f;
Tokens_tTokenSet TokenSet;
# endif
{
  Tokens_tToken Token;

  for (Token = Tokens_TokEOF; Token <= Tokens_TokForeign; Token += 1) {
    if (IsElement(&TokenSet, Token)) {
      Tokens_WriteToken(f, Token);
      IO_WriteC(f, ' ');
    }
  }
}

void Tokens_WriteToken
# ifdef __STDC__
(IO_tFile f, Tokens_tToken Token)
# else
(f, Token)
IO_tFile f;
Tokens_tToken Token;
# endif
{
  switch (Token) {
  case Tokens_TokEOF:;
    IO_WriteS(f, (STRING)"End-of-Tokens", 13L);
    break;
  case Tokens_TokIdent:;
    IO_WriteS(f, (STRING)"Ident", 5L);
    break;
  case Tokens_TokDecConst:;
    IO_WriteS(f, (STRING)"Integer", 7L);
    break;
  case Tokens_TokOctalConst:;
    IO_WriteS(f, (STRING)"Integer", 7L);
    break;
  case Tokens_TokHexConst:;
    IO_WriteS(f, (STRING)"Integer", 7L);
    break;
  case Tokens_TokCharConst:;
    IO_WriteS(f, (STRING)"Char", 4L);
    break;
  case Tokens_TokRealConst:;
    IO_WriteS(f, (STRING)"Real", 4L);
    break;
  case Tokens_TokStringConst:;
    IO_WriteS(f, (STRING)"String", 6L);
    break;
  case Tokens_TokNotEqual:;
    IO_WriteS(f, (STRING)"'#'", 3L);
    break;
  case Tokens_TokLParent:;
    IO_WriteS(f, (STRING)"'('", 3L);
    break;
  case Tokens_TokRParent:;
    IO_WriteS(f, (STRING)"')'", 3L);
    break;
  case Tokens_TokTimes:;
    IO_WriteS(f, (STRING)"'*'", 3L);
    break;
  case Tokens_TokPlus:;
    IO_WriteS(f, (STRING)"'+'", 3L);
    break;
  case Tokens_TokComma:;
    IO_WriteS(f, (STRING)"','", 3L);
    break;
  case Tokens_TokMinus:;
    IO_WriteS(f, (STRING)"'-'", 3L);
    break;
  case Tokens_TokDot:;
    IO_WriteS(f, (STRING)"'.'", 3L);
    break;
  case Tokens_TokRange:;
    IO_WriteS(f, (STRING)"'..'", 4L);
    break;
  case Tokens_TokDivide:;
    IO_WriteS(f, (STRING)"'/'", 3L);
    break;
  case Tokens_TokColon:;
    IO_WriteS(f, (STRING)"':'", 3L);
    break;
  case Tokens_TokAssign:;
    IO_WriteS(f, (STRING)"':='", 4L);
    break;
  case Tokens_TokSemiColon:;
    IO_WriteS(f, (STRING)"';'", 3L);
    break;
  case Tokens_TokLess:;
    IO_WriteS(f, (STRING)"'<'", 3L);
    break;
  case Tokens_TokLessEqual:;
    IO_WriteS(f, (STRING)"'<='", 4L);
    break;
  case Tokens_TokEqual:;
    IO_WriteS(f, (STRING)"'='", 3L);
    break;
  case Tokens_TokGreater:;
    IO_WriteS(f, (STRING)"'>'", 3L);
    break;
  case Tokens_TokGreaterEqual:;
    IO_WriteS(f, (STRING)"'>='", 4L);
    break;
  case Tokens_TokLBracket:;
    IO_WriteS(f, (STRING)"'['", 3L);
    break;
  case Tokens_TokRBracket:;
    IO_WriteS(f, (STRING)"']'", 3L);
    break;
  case Tokens_TokArrow:;
    IO_WriteS(f, (STRING)"'^'", 3L);
    break;
  case Tokens_TokLBrace:;
    IO_WriteS(f, (STRING)"'{'", 3L);
    break;
  case Tokens_TokBar:;
    IO_WriteS(f, (STRING)"'|'", 3L);
    break;
  case Tokens_TokRBrace:;
    IO_WriteS(f, (STRING)"'}'", 3L);
    break;
  case Tokens_TokAnd:;
    IO_WriteS(f, (STRING)"AND", 3L);
    break;
  case Tokens_TokArray:;
    IO_WriteS(f, (STRING)"ARRAY", 5L);
    break;
  case Tokens_TokBegin:;
    IO_WriteS(f, (STRING)"BEGIN", 5L);
    break;
  case Tokens_TokBy:;
    IO_WriteS(f, (STRING)"BY", 2L);
    break;
  case Tokens_TokCase:;
    IO_WriteS(f, (STRING)"CASE", 4L);
    break;
  case Tokens_TokConst:;
    IO_WriteS(f, (STRING)"CONST", 5L);
    break;
  case Tokens_TokDefinition:;
    IO_WriteS(f, (STRING)"DEFINITION", 10L);
    break;
  case Tokens_TokDiv:;
    IO_WriteS(f, (STRING)"DIV", 3L);
    break;
  case Tokens_TokDo:;
    IO_WriteS(f, (STRING)"DO", 2L);
    break;
  case Tokens_TokElse:;
    IO_WriteS(f, (STRING)"ELSE", 4L);
    break;
  case Tokens_TokElsif:;
    IO_WriteS(f, (STRING)"ELSIF", 5L);
    break;
  case Tokens_TokEnd:;
    IO_WriteS(f, (STRING)"END", 3L);
    break;
  case Tokens_TokExit:;
    IO_WriteS(f, (STRING)"EXIT", 4L);
    break;
  case Tokens_TokExport:;
    IO_WriteS(f, (STRING)"EXPORT", 6L);
    break;
  case Tokens_TokFor:;
    IO_WriteS(f, (STRING)"FOR", 3L);
    break;
  case Tokens_TokFrom:;
    IO_WriteS(f, (STRING)"FROM", 4L);
    break;
  case Tokens_TokIf:;
    IO_WriteS(f, (STRING)"IF", 2L);
    break;
  case Tokens_TokImplementation:;
    IO_WriteS(f, (STRING)"IMPLEMENTATION", 14L);
    break;
  case Tokens_TokImport:;
    IO_WriteS(f, (STRING)"IMPORT", 6L);
    break;
  case Tokens_TokIn:;
    IO_WriteS(f, (STRING)"IN", 2L);
    break;
  case Tokens_TokLoop:;
    IO_WriteS(f, (STRING)"LOOP", 4L);
    break;
  case Tokens_TokMod:;
    IO_WriteS(f, (STRING)"MOD", 3L);
    break;
  case Tokens_TokModule:;
    IO_WriteS(f, (STRING)"MODULE", 6L);
    break;
  case Tokens_TokNot:;
    IO_WriteS(f, (STRING)"NOT", 3L);
    break;
  case Tokens_TokOf:;
    IO_WriteS(f, (STRING)"OF", 2L);
    break;
  case Tokens_TokOr:;
    IO_WriteS(f, (STRING)"OR", 2L);
    break;
  case Tokens_TokPointer:;
    IO_WriteS(f, (STRING)"POINTER", 7L);
    break;
  case Tokens_TokProcedure:;
    IO_WriteS(f, (STRING)"PROCEDURE", 9L);
    break;
  case Tokens_TokQualified:;
    IO_WriteS(f, (STRING)"QUALIFIED", 9L);
    break;
  case Tokens_TokRecord:;
    IO_WriteS(f, (STRING)"RECORD", 6L);
    break;
  case Tokens_TokRepeat:;
    IO_WriteS(f, (STRING)"REPEAT", 6L);
    break;
  case Tokens_TokReturn:;
    IO_WriteS(f, (STRING)"RETURN", 6L);
    break;
  case Tokens_TokSet:;
    IO_WriteS(f, (STRING)"SET", 3L);
    break;
  case Tokens_TokThen:;
    IO_WriteS(f, (STRING)"THEN", 4L);
    break;
  case Tokens_TokTo:;
    IO_WriteS(f, (STRING)"TO", 2L);
    break;
  case Tokens_TokType:;
    IO_WriteS(f, (STRING)"TYPE", 4L);
    break;
  case Tokens_TokUntil:;
    IO_WriteS(f, (STRING)"UNTIL", 5L);
    break;
  case Tokens_TokVar:;
    IO_WriteS(f, (STRING)"VAR", 3L);
    break;
  case Tokens_TokWhile:;
    IO_WriteS(f, (STRING)"WHILE", 5L);
    break;
  case Tokens_TokWith:;
    IO_WriteS(f, (STRING)"WITH", 4L);
    break;
  case Tokens_TokForeign:;
    IO_WriteS(f, (STRING)"FOREIGN", 7L);
    break;
  default :
    IO_WriteS(f, (STRING)"illegal token code", 18L);
    break;
  }
}

void BEGIN_Tokens()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_IO();

  }
}
