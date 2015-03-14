#include "SYSTEM_.h"

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Parser
#include "Parser.h"
#endif

Parser_tParsAttribute Parser_ParsAttribute;
struct Parser_1 Parser_ParsTabName;

static void Halt ARGS(());
#define xxEof	0
typedef struct S_1 *xxtUnionPtr;
typedef struct S_1 {
    xxtUnionPtr GlobalRecoverySet;
    SHORTCARD LocalRecoverySet;
} xxtUnion;
typedef struct S_2 {
    BITSET A[2 + 1];
} xxtSet;
static SHORTCARD xxToken;
static INTEGER xxErrorCount;
static BOOLEAN xxIsInitialized;
static BOOLEAN xxIsRepairMode;
static struct S_3 {
    xxtSet A[170 + 1];
} xxHorizontalSet;
static struct S_4 {
    BITSET A[74 + 1];
} xxVerticalSet0;
static struct S_5 {
    BITSET A[74 + 1];
} xxVerticalSet1;
static void Copy ARGS((CHAR Source[], LONGCARD , CHAR Target[], LONGCARD ));
static BOOLEAN xxIsElement ARGS((xxtSet *Set, SHORTCARD Element));
static void xxUnexpected ARGS((SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet));
static void xxExpected ARGS((SHORTCARD ExpectedSet, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet));
struct S_9 {
    CHAR A[127 + 1];
};
static void xxRecoveryLiteral ARGS((SHORTCARD Expected, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet));
struct S_10 {
    CHAR A[127 + 1];
};
static void xxRecoveryTerminal ARGS((SHORTCARD Expected, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet, Scanner_tScanAttribute *RepairAttribute));
struct S_11 {
    CHAR A[127 + 1];
};
static void xxSkipTokens ARGS((SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet));
static void BeginParser ARGS(());
static void yyCompilationUnit ARGS((Parser_tParsAttribute *CompilationUnit0, xxtUnionPtr xxGlobalRecoverySet));
static void yyNumber ARGS((Parser_tParsAttribute *Number0, xxtUnionPtr xxGlobalRecoverySet));
static void yyQualident ARGS((Parser_tParsAttribute *Qualident0, xxtUnionPtr xxGlobalRecoverySet));
static void yyConstantDeclaration ARGS((Parser_tParsAttribute *ConstantDeclaration0, xxtUnionPtr xxGlobalRecoverySet));
static void yyConstExpression ARGS((Parser_tParsAttribute *ConstExpression0, xxtUnionPtr xxGlobalRecoverySet));
static void yyTypeDeclaration ARGS((Parser_tParsAttribute *TypeDeclaration0, xxtUnionPtr xxGlobalRecoverySet));
static void yyType ARGS((Parser_tParsAttribute *Type0, xxtUnionPtr xxGlobalRecoverySet));
static void yySimpleType ARGS((Parser_tParsAttribute *SimpleType0, xxtUnionPtr xxGlobalRecoverySet));
static void yyIdentList ARGS((Parser_tParsAttribute *IdentList0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFieldListSequence ARGS((Parser_tParsAttribute *FieldListSequence0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFieldList ARGS((Parser_tParsAttribute *FieldList0, xxtUnionPtr xxGlobalRecoverySet));
static void yyVariant ARGS((Parser_tParsAttribute *Variant0, xxtUnionPtr xxGlobalRecoverySet));
static void yyCaseLabelList ARGS((Parser_tParsAttribute *CaseLabelList0, xxtUnionPtr xxGlobalRecoverySet));
static void yyCaseLabels ARGS((Parser_tParsAttribute *CaseLabels0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFormalTypeList ARGS((Parser_tParsAttribute *FormalTypeList0, xxtUnionPtr xxGlobalRecoverySet));
static void yyVariableDeclaration ARGS((Parser_tParsAttribute *VariableDeclaration0, xxtUnionPtr xxGlobalRecoverySet));
static void yyDesignator ARGS((Parser_tParsAttribute *Designator0, xxtUnionPtr xxGlobalRecoverySet));
static void yyExpList ARGS((Parser_tParsAttribute *ExpList0, xxtUnionPtr xxGlobalRecoverySet));
static void yyExpression ARGS((Parser_tParsAttribute *Expression0, xxtUnionPtr xxGlobalRecoverySet));
static void yyRelation ARGS((Parser_tParsAttribute *Relation0, xxtUnionPtr xxGlobalRecoverySet));
static void yySimpleExpression ARGS((Parser_tParsAttribute *SimpleExpression0, xxtUnionPtr xxGlobalRecoverySet));
static void yyAddOperator ARGS((Parser_tParsAttribute *AddOperator0, xxtUnionPtr xxGlobalRecoverySet));
static void yyTerm ARGS((Parser_tParsAttribute *Term0, xxtUnionPtr xxGlobalRecoverySet));
static void yyMulOperator ARGS((Parser_tParsAttribute *MulOperator0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFactor ARGS((Parser_tParsAttribute *Factor0, xxtUnionPtr xxGlobalRecoverySet));
static void yySet ARGS((Parser_tParsAttribute *Set0, xxtUnionPtr xxGlobalRecoverySet));
static void yyElement ARGS((Parser_tParsAttribute *Element0, xxtUnionPtr xxGlobalRecoverySet));
static void yyActualParameters ARGS((Parser_tParsAttribute *ActualParameters0, xxtUnionPtr xxGlobalRecoverySet));
static void yyStatement ARGS((Parser_tParsAttribute *Statement0, xxtUnionPtr xxGlobalRecoverySet));
static void yyStatementSequence ARGS((Parser_tParsAttribute *StatementSequence0, xxtUnionPtr xxGlobalRecoverySet));
static void yyCase ARGS((Parser_tParsAttribute *Case0, xxtUnionPtr xxGlobalRecoverySet));
static void yyProcedureDeclaration ARGS((Parser_tParsAttribute *ProcedureDeclaration0, xxtUnionPtr xxGlobalRecoverySet));
static void yyProcedureHeading ARGS((Parser_tParsAttribute *ProcedureHeading0, xxtUnionPtr xxGlobalRecoverySet));
static void yyBlock ARGS((Parser_tParsAttribute *Block0, xxtUnionPtr xxGlobalRecoverySet));
static void yyDeclaration ARGS((Parser_tParsAttribute *Declaration0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFormalParameters ARGS((Parser_tParsAttribute *FormalParameters0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFPSection ARGS((Parser_tParsAttribute *FPSection0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFormalType ARGS((Parser_tParsAttribute *FormalType0, xxtUnionPtr xxGlobalRecoverySet));
static void yyModuleDeclaration ARGS((Parser_tParsAttribute *ModuleDeclaration0, xxtUnionPtr xxGlobalRecoverySet));
static void yyPriority ARGS((Parser_tParsAttribute *Priority0, xxtUnionPtr xxGlobalRecoverySet));
static void yyExport ARGS((Parser_tParsAttribute *Export0, xxtUnionPtr xxGlobalRecoverySet));
static void yyImport ARGS((Parser_tParsAttribute *Import0, xxtUnionPtr xxGlobalRecoverySet));
static void yyDefinition ARGS((Parser_tParsAttribute *Definition0, xxtUnionPtr xxGlobalRecoverySet));


static void Halt
# ifdef __STDC__
()
# else
()
# endif
{
  IO_CloseIO();
  if (Errors_NumberOfErrors() == 0) {
    Exit(0);
  } else {
    Exit(1);
  }
}

static void Copy
# ifdef __STDC__
(CHAR Source[], LONGCARD O_2, CHAR Target[], LONGCARD O_1)
# else
(Source, O_2, Target, O_1)
CHAR Source[];
LONGCARD O_2;
CHAR Target[];
LONGCARD O_1;
# endif
{
  CARDINAL i, j;
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_2 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Source, O_2, CHAR)
  if ((O_2 - 1) < (O_1 - 1)) {
    j = (O_2 - 1);
  } else {
    j = (O_1 - 1);
  }
  {
    LONGCARD B_1 = 0, B_2 = j;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        Target[i] = Source[i];
        if (i >= B_2) break;
      }
  }
  if ((O_1 - 1) > j) {
    Target[j + 1] = CHR(0);
  }
  FREE_OPEN_ARRAYS
}

void Parser_xxTokenName
# ifdef __STDC__
(CARDINAL Token, CHAR Name[], LONGCARD O_3)
# else
(Token, Name, O_3)
CARDINAL Token;
CHAR Name[];
LONGCARD O_3;
# endif
{
  switch (Token) {
  case 0:;
    Copy("_EndOfFile", 10L, Name, O_3);
    break;
  case 1:;
    Copy("Ident", 5L, Name, O_3);
    break;
  case 2:;
    Copy("Integer", 7L, Name, O_3);
    break;
  case 4:;
    Copy("Char", 4L, Name, O_3);
    break;
  case 6:;
    Copy("Real", 4L, Name, O_3);
    break;
  case 7:;
    Copy("String", 6L, Name, O_3);
    break;
  case 8:;
    Copy("#", 1L, Name, O_3);
    break;
  case 9:;
    Copy("(", 1L, Name, O_3);
    break;
  case 10:;
    Copy(")", 1L, Name, O_3);
    break;
  case 11:;
    Copy("*", 1L, Name, O_3);
    break;
  case 12:;
    Copy("+", 1L, Name, O_3);
    break;
  case 13:;
    Copy(",", 1L, Name, O_3);
    break;
  case 14:;
    Copy("-", 1L, Name, O_3);
    break;
  case 15:;
    Copy(".", 1L, Name, O_3);
    break;
  case 16:;
    Copy("..", 2L, Name, O_3);
    break;
  case 17:;
    Copy("/", 1L, Name, O_3);
    break;
  case 18:;
    Copy(":", 1L, Name, O_3);
    break;
  case 19:;
    Copy(":=", 2L, Name, O_3);
    break;
  case 20:;
    Copy(";", 1L, Name, O_3);
    break;
  case 21:;
    Copy("<", 1L, Name, O_3);
    break;
  case 22:;
    Copy("<=", 2L, Name, O_3);
    break;
  case 24:;
    Copy("=", 1L, Name, O_3);
    break;
  case 25:;
    Copy(">", 1L, Name, O_3);
    break;
  case 26:;
    Copy(">=", 2L, Name, O_3);
    break;
  case 27:;
    Copy("[", 1L, Name, O_3);
    break;
  case 28:;
    Copy("]", 1L, Name, O_3);
    break;
  case 29:;
    Copy("^", 1L, Name, O_3);
    break;
  case 30:;
    Copy("{", 1L, Name, O_3);
    break;
  case 31:;
    Copy("|", 1L, Name, O_3);
    break;
  case 32:;
    Copy("}", 1L, Name, O_3);
    break;
  case 34:;
    Copy("AND", 3L, Name, O_3);
    break;
  case 35:;
    Copy("ARRAY", 5L, Name, O_3);
    break;
  case 36:;
    Copy("BEGIN", 5L, Name, O_3);
    break;
  case 37:;
    Copy("BY", 2L, Name, O_3);
    break;
  case 38:;
    Copy("CASE", 4L, Name, O_3);
    break;
  case 39:;
    Copy("CONST", 5L, Name, O_3);
    break;
  case 40:;
    Copy("DEFINITION", 10L, Name, O_3);
    break;
  case 41:;
    Copy("DIV", 3L, Name, O_3);
    break;
  case 42:;
    Copy("DO", 2L, Name, O_3);
    break;
  case 43:;
    Copy("ELSE", 4L, Name, O_3);
    break;
  case 44:;
    Copy("ELSIF", 5L, Name, O_3);
    break;
  case 45:;
    Copy("END", 3L, Name, O_3);
    break;
  case 46:;
    Copy("EXIT", 4L, Name, O_3);
    break;
  case 47:;
    Copy("EXPORT", 6L, Name, O_3);
    break;
  case 48:;
    Copy("FOR", 3L, Name, O_3);
    break;
  case 49:;
    Copy("FROM", 4L, Name, O_3);
    break;
  case 50:;
    Copy("IF", 2L, Name, O_3);
    break;
  case 51:;
    Copy("IMPLEMENTATION", 14L, Name, O_3);
    break;
  case 52:;
    Copy("IMPORT", 6L, Name, O_3);
    break;
  case 53:;
    Copy("IN", 2L, Name, O_3);
    break;
  case 54:;
    Copy("LOOP", 4L, Name, O_3);
    break;
  case 55:;
    Copy("MOD", 3L, Name, O_3);
    break;
  case 56:;
    Copy("MODULE", 6L, Name, O_3);
    break;
  case 57:;
    Copy("NOT", 3L, Name, O_3);
    break;
  case 58:;
    Copy("OF", 2L, Name, O_3);
    break;
  case 59:;
    Copy("OR", 2L, Name, O_3);
    break;
  case 60:;
    Copy("POINTER", 7L, Name, O_3);
    break;
  case 61:;
    Copy("PROCEDURE", 9L, Name, O_3);
    break;
  case 62:;
    Copy("QUALIFIED", 9L, Name, O_3);
    break;
  case 63:;
    Copy("RECORD", 6L, Name, O_3);
    break;
  case 64:;
    Copy("REPEAT", 6L, Name, O_3);
    break;
  case 65:;
    Copy("RETURN", 6L, Name, O_3);
    break;
  case 66:;
    Copy("SET", 3L, Name, O_3);
    break;
  case 67:;
    Copy("THEN", 4L, Name, O_3);
    break;
  case 68:;
    Copy("TO", 2L, Name, O_3);
    break;
  case 69:;
    Copy("TYPE", 4L, Name, O_3);
    break;
  case 70:;
    Copy("UNTIL", 5L, Name, O_3);
    break;
  case 71:;
    Copy("VAR", 3L, Name, O_3);
    break;
  case 72:;
    Copy("WHILE", 5L, Name, O_3);
    break;
  case 73:;
    Copy("WITH", 4L, Name, O_3);
    break;
  case 74:;
    Copy("FOREIGN", 7L, Name, O_3);
    break;
  }
}

INTEGER Parser_Parser
# ifdef __STDC__
()
# else
()
# endif
{
  xxErrorCount = 0;
  BeginParser();
  xxToken = Scanner_GetToken();
  xxIsRepairMode = FALSE;
  yyCompilationUnit(&Parser_ParsAttribute, NIL);
  if (xxToken != xxEof) {
    xxRecoveryLiteral(xxEof, 0, NIL);
  }
  return xxErrorCount;
}

static BOOLEAN xxIsElement
# ifdef __STDC__
(xxtSet *Set, SHORTCARD Element)
# else
(Set, Element)
xxtSet *Set;
SHORTCARD Element;
# endif
{
  return IN(Element % 32, Set->A[Element / 32]);
}

static void xxUnexpected
# ifdef __STDC__
(SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet)
# else
(LocalRecoverySet, GlobalRecoverySet)
SHORTCARD LocalRecoverySet;
xxtUnionPtr GlobalRecoverySet;
# endif
{
  if (!xxIsRepairMode) {
    INC(xxErrorCount);
    Errors_ErrorMessage(Errors_SyntaxError, Errors_Error, Scanner_Attribute.Position);
    xxSkipTokens(LocalRecoverySet, GlobalRecoverySet);
  }
}

static void xxExpected
# ifdef __STDC__
(SHORTCARD ExpectedSet, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet)
# else
(ExpectedSet, LocalRecoverySet, GlobalRecoverySet)
SHORTCARD ExpectedSet;
SHORTCARD LocalRecoverySet;
xxtUnionPtr GlobalRecoverySet;
# endif
{
  SHORTCARD Token;
  struct S_9 TokenArray;
  Strings_tString TokenString;
  Strings_tString ContinueString;

  if (!xxIsRepairMode) {
    INC(xxErrorCount);
    Errors_ErrorMessage(Errors_SyntaxError, Errors_Error, Scanner_Attribute.Position);
    Strings_AssignEmpty(&ContinueString);
    for (Token = 0; Token <= 74; Token += 1) {
      if (xxIsElement(&xxHorizontalSet.A[ExpectedSet], Token)) {
        Parser_xxTokenName(Token, TokenArray.A, 128L);
        Strings_ArrayToString(TokenArray.A, 128L, &TokenString);
        if (Strings_Length(&ContinueString) + Strings_Length(&TokenString) + 1 <= Strings_cMaxStrLength) {
          Strings_Concatenate(&ContinueString, &TokenString);
          Strings_Append(&ContinueString, ' ');
        }
      }
    }
    Errors_ErrorMessageI(Errors_ExpectedTokens, Errors_Information, Scanner_Attribute.Position, Errors_String, ADR(ContinueString));
    xxSkipTokens(LocalRecoverySet, GlobalRecoverySet);
  }
}

static void xxRecoveryLiteral
# ifdef __STDC__
(SHORTCARD Expected, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet)
# else
(Expected, LocalRecoverySet, GlobalRecoverySet)
SHORTCARD Expected;
SHORTCARD LocalRecoverySet;
xxtUnionPtr GlobalRecoverySet;
# endif
{
  struct S_10 TokenString;

  if (!xxIsRepairMode) {
    INC(xxErrorCount);
    Errors_ErrorMessage(Errors_SyntaxError, Errors_Error, Scanner_Attribute.Position);
    Parser_xxTokenName(Expected, TokenString.A, 128L);
    Errors_ErrorMessageI(Errors_ExpectedTokens, Errors_Information, Scanner_Attribute.Position, Errors_Array, ADR(TokenString));
    xxSkipTokens(LocalRecoverySet, GlobalRecoverySet);
  }
  if (xxToken != Expected) {
    Parser_xxTokenName(Expected, TokenString.A, 128L);
    Errors_ErrorMessageI(Errors_TokenInserted, Errors_Repair, Scanner_Attribute.Position, Errors_Array, ADR(TokenString));
  } else {
    if (xxToken != xxEof) {
      xxToken = Scanner_GetToken();
    }
    xxIsRepairMode = FALSE;
  }
}

static void xxRecoveryTerminal
# ifdef __STDC__
(SHORTCARD Expected, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet, Scanner_tScanAttribute *RepairAttribute)
# else
(Expected, LocalRecoverySet, GlobalRecoverySet, RepairAttribute)
SHORTCARD Expected;
SHORTCARD LocalRecoverySet;
xxtUnionPtr GlobalRecoverySet;
Scanner_tScanAttribute *RepairAttribute;
# endif
{
  struct S_11 TokenString;

  if (!xxIsRepairMode) {
    INC(xxErrorCount);
    Errors_ErrorMessage(Errors_SyntaxError, Errors_Error, Scanner_Attribute.Position);
    Parser_xxTokenName(Expected, TokenString.A, 128L);
    Errors_ErrorMessageI(Errors_ExpectedTokens, Errors_Information, Scanner_Attribute.Position, Errors_Array, ADR(TokenString));
    xxSkipTokens(LocalRecoverySet, GlobalRecoverySet);
  }
  if (xxToken != Expected) {
    Parser_xxTokenName(Expected, TokenString.A, 128L);
    Errors_ErrorMessageI(Errors_TokenInserted, Errors_Repair, Scanner_Attribute.Position, Errors_Array, ADR(TokenString));
    Scanner_ErrorAttribute(Expected, RepairAttribute);
  } else {
    *RepairAttribute = Scanner_Attribute;
    if (xxToken != xxEof) {
      xxToken = Scanner_GetToken();
    }
    xxIsRepairMode = FALSE;
  }
}

static void xxSkipTokens
# ifdef __STDC__
(SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet)
# else
(LocalRecoverySet, GlobalRecoverySet)
SHORTCARD LocalRecoverySet;
xxtUnionPtr GlobalRecoverySet;
# endif
{
  xxtSet RecoverySet;
  BOOLEAN TokensSkipped;

  RecoverySet = xxHorizontalSet.A[LocalRecoverySet];
  INCL(RecoverySet.A[0], xxEof);
  while (GlobalRecoverySet != NIL) {
    RecoverySet.A[0] = RecoverySet.A[0] | xxHorizontalSet.A[GlobalRecoverySet->LocalRecoverySet].A[0];
    RecoverySet.A[1] = RecoverySet.A[1] | xxHorizontalSet.A[GlobalRecoverySet->LocalRecoverySet].A[1];
    RecoverySet.A[2] = RecoverySet.A[2] | xxHorizontalSet.A[GlobalRecoverySet->LocalRecoverySet].A[2];
    GlobalRecoverySet = GlobalRecoverySet->GlobalRecoverySet;
  }
  TokensSkipped = FALSE;
  while (!xxIsElement(&RecoverySet, xxToken)) {
    xxToken = Scanner_GetToken();
    TokensSkipped = TRUE;
  }
  if (TokensSkipped) {
    Errors_ErrorMessage(Errors_RestartPoint, Errors_Information, Scanner_Attribute.Position);
  }
  xxIsRepairMode = TRUE;
}

static void BeginParser
# ifdef __STDC__
()
# else
()
# endif
{
  System_tFile xxTableFile;
  CARDINAL xxSize;

  if (xxIsInitialized) {
    return;
  }
  Scanner_BeginScanner();
  xxTableFile = OpenInput(Parser_ParsTabName.A, 129L);
  if (xxTableFile < 0) {
    Errors_ErrorMessage(Errors_ReadParseTable, Errors_Fatal, Positions_NoPosition);
  }
  xxSize = Read(xxTableFile, ADR(xxHorizontalSet), sizeof(xxHorizontalSet));
  if (xxSize != sizeof(xxHorizontalSet)) {
    Errors_ErrorMessage(Errors_ReadParseTable, Errors_Fatal, Positions_NoPosition);
  }
  xxSize = Read(xxTableFile, ADR(xxVerticalSet0), sizeof(xxVerticalSet0));
  if (xxSize != sizeof(xxVerticalSet0)) {
    Errors_ErrorMessage(Errors_ReadParseTable, Errors_Fatal, Positions_NoPosition);
  }
  xxSize = Read(xxTableFile, ADR(xxVerticalSet1), sizeof(xxVerticalSet1));
  if (xxSize != sizeof(xxVerticalSet1)) {
    Errors_ErrorMessage(Errors_ReadParseTable, Errors_Fatal, Positions_NoPosition);
  }
  Close(xxTableFile);
  xxIsInitialized = TRUE;
}

void Parser_CloseParser
# ifdef __STDC__
()
# else
()
# endif
{
  Scanner_CloseScanner();
}

static void yyCompilationUnit
# ifdef __STDC__
(Parser_tParsAttribute *CompilationUnit0, xxtUnionPtr xxGlobalRecoverySet)
# else
(CompilationUnit0, xxGlobalRecoverySet)
Parser_tParsAttribute *CompilationUnit0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1, Ident2, Ident3, Ident4;
  Parser_tParsAttribute Import1, Import2;
  Parser_tParsAttribute Definition1;
  Parser_tParsAttribute Priority1;
  Parser_tParsAttribute Block1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 40:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        switch (xxToken) {
        case 56:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          if (xxToken != 1) {
            xxRecoveryTerminal(1, 3, xxGlobalRecoverySet, &Ident1);
          } else {
            Ident1 = Scanner_Attribute;
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 3, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          IO_WriteS(IO_StdOutput, "DEFINITION MODULE ", 18L);
          Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
          IO_WriteS(IO_StdOutput, " ;", 2L);
          IO_WriteNl(IO_StdOutput);
          for (;;) {
            if (IN(0, xxVerticalSet0.A[xxToken])) {
              xxUnion.LocalRecoverySet = 6;
              yyImport(&Import1, ADR(xxUnion));
            } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
              goto EXIT_3;
            } else {
              xxExpected(4, 5, xxGlobalRecoverySet);
            }
          } EXIT_3:;
          IO_WriteS(IO_StdOutput, "END ", 4L);
          Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
          IO_WriteS(IO_StdOutput, " ;", 2L);
          IO_WriteNl(IO_StdOutput);
          Halt();
          goto EXIT_2;
          break;
        case 48:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          if (xxToken != 1) {
            xxRecoveryTerminal(1, 2, xxGlobalRecoverySet, &Ident2);
          } else {
            Ident2 = Scanner_Attribute;
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          if (xxToken != 56) {
            xxRecoveryLiteral(56, 2, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          if (xxToken != 1) {
            xxRecoveryTerminal(1, 3, xxGlobalRecoverySet, &Ident3);
          } else {
            Ident3 = Scanner_Attribute;
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 3, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          IO_WriteS(IO_StdOutput, "DEFINITION MODULE ", 18L);
          Idents_WriteIdent(IO_StdOutput, Ident3.U_1.V_1.Ident);
          IO_WriteS(IO_StdOutput, " ;", 2L);
          IO_WriteNl(IO_StdOutput);
          for (;;) {
            if (IN(0, xxVerticalSet0.A[xxToken])) {
              xxUnion.LocalRecoverySet = 6;
              yyImport(&Import2, ADR(xxUnion));
            } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
              goto EXIT_4;
            } else {
              xxExpected(4, 5, xxGlobalRecoverySet);
            }
          } EXIT_4:;
          IO_WriteS(IO_StdOutput, "END ", 4L);
          Idents_WriteIdent(IO_StdOutput, Ident3.U_1.V_1.Ident);
          IO_WriteS(IO_StdOutput, " ;", 2L);
          IO_WriteNl(IO_StdOutput);
          Halt();
          goto EXIT_2;
          break;
        default :
          if (xxIsRepairMode) {
            if (xxToken != 56) {
              xxRecoveryLiteral(56, 2, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            if (xxToken != 1) {
              xxRecoveryTerminal(1, 3, xxGlobalRecoverySet, &Ident1);
            } else {
              Ident1 = Scanner_Attribute;
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            if (xxToken != 20) {
              xxRecoveryLiteral(20, 3, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            IO_WriteS(IO_StdOutput, "DEFINITION MODULE ", 18L);
            Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
            IO_WriteS(IO_StdOutput, " ;", 2L);
            IO_WriteNl(IO_StdOutput);
            for (;;) {
              if (IN(0, xxVerticalSet0.A[xxToken])) {
                xxUnion.LocalRecoverySet = 6;
                yyImport(&Import1, ADR(xxUnion));
              } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
                goto EXIT_5;
              } else {
                xxExpected(4, 5, xxGlobalRecoverySet);
              }
            } EXIT_5:;
            IO_WriteS(IO_StdOutput, "END ", 4L);
            Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
            IO_WriteS(IO_StdOutput, " ;", 2L);
            IO_WriteNl(IO_StdOutput);
            Halt();
            goto EXIT_2;
          }
          xxExpected(8, 9, xxGlobalRecoverySet);
          break;
        }
      } EXIT_2:;
      for (;;) {
        if (IN(2, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 11;
          yyDefinition(&Definition1, ADR(xxUnion));
        } else if (xxToken == 45 || xxIsRepairMode) {
          goto EXIT_6;
        } else {
          xxExpected(10, 6, xxGlobalRecoverySet);
        }
      } EXIT_6:;
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 11, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 12, xxGlobalRecoverySet, &Ident4);
      } else {
        Ident4 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 15) {
        xxRecoveryLiteral(15, 13, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_1;
      break;
    case 74:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 56) {
        xxRecoveryLiteral(56, 2, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 3, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 3, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      IO_WriteS(IO_StdOutput, "FOREIGN MODULE ", 15L);
      Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
      IO_WriteS(IO_StdOutput, " ;", 2L);
      IO_WriteNl(IO_StdOutput);
      for (;;) {
        if (IN(0, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 6;
          yyImport(&Import1, ADR(xxUnion));
        } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_7;
        } else {
          xxExpected(4, 5, xxGlobalRecoverySet);
        }
      } EXIT_7:;
      IO_WriteS(IO_StdOutput, "END ", 4L);
      Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
      IO_WriteS(IO_StdOutput, " ;", 2L);
      IO_WriteNl(IO_StdOutput);
      Halt();
      for (;;) {
        if (IN(2, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 11;
          yyDefinition(&Definition1, ADR(xxUnion));
        } else if (xxToken == 45 || xxIsRepairMode) {
          goto EXIT_8;
        } else {
          xxExpected(10, 6, xxGlobalRecoverySet);
        }
      } EXIT_8:;
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 11, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 12, xxGlobalRecoverySet, &Ident2);
      } else {
        Ident2 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 15) {
        xxRecoveryLiteral(15, 13, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_1;
      break;
    case 51:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 56) {
        xxRecoveryLiteral(56, 16, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 16, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      for (;;) {
        if (xxToken == 27) {
          xxUnion.LocalRecoverySet = 18;
          yyPriority(&Priority1, ADR(xxUnion));
          goto EXIT_9;
        } else if (xxToken == 20 || xxIsRepairMode) {
          goto EXIT_9;
        }
        xxExpected(17, 16, xxGlobalRecoverySet);
      } EXIT_9:;
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 18, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      IO_WriteS(IO_StdOutput, "IMPLEMENTATION MODULE ", 22L);
      Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
      IO_WriteS(IO_StdOutput, " ;", 2L);
      IO_WriteNl(IO_StdOutput);
      for (;;) {
        if (IN(0, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 21;
          yyImport(&Import1, ADR(xxUnion));
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_10;
        } else {
          xxExpected(19, 20, xxGlobalRecoverySet);
        }
      } EXIT_10:;
      IO_WriteS(IO_StdOutput, "END ", 4L);
      Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
      IO_WriteS(IO_StdOutput, " ;", 2L);
      IO_WriteNl(IO_StdOutput);
      Halt();
      xxUnion.LocalRecoverySet = 12;
      yyBlock(&Block1, ADR(xxUnion));
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 12, xxGlobalRecoverySet, &Ident2);
      } else {
        Ident2 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 15) {
        xxRecoveryLiteral(15, 13, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_1;
      break;
    case 56:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 16, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      for (;;) {
        if (xxToken == 27) {
          xxUnion.LocalRecoverySet = 18;
          yyPriority(&Priority1, ADR(xxUnion));
          goto EXIT_11;
        } else if (xxToken == 20 || xxIsRepairMode) {
          goto EXIT_11;
        }
        xxExpected(17, 16, xxGlobalRecoverySet);
      } EXIT_11:;
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 18, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      IO_WriteS(IO_StdOutput, "PROGRAM MODULE ", 15L);
      Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
      IO_WriteS(IO_StdOutput, " ;", 2L);
      IO_WriteNl(IO_StdOutput);
      for (;;) {
        if (IN(0, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 21;
          yyImport(&Import1, ADR(xxUnion));
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_12;
        } else {
          xxExpected(19, 20, xxGlobalRecoverySet);
        }
      } EXIT_12:;
      IO_WriteS(IO_StdOutput, "END ", 4L);
      Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
      IO_WriteS(IO_StdOutput, " ;", 2L);
      IO_WriteNl(IO_StdOutput);
      Halt();
      xxUnion.LocalRecoverySet = 12;
      yyBlock(&Block1, ADR(xxUnion));
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 12, xxGlobalRecoverySet, &Ident2);
      } else {
        Ident2 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 15) {
        xxRecoveryLiteral(15, 13, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_1;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 56) {
          xxRecoveryLiteral(56, 16, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        if (xxToken != 1) {
          xxRecoveryTerminal(1, 16, xxGlobalRecoverySet, &Ident1);
        } else {
          Ident1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        for (;;) {
          if (xxToken == 27) {
            xxUnion.LocalRecoverySet = 18;
            yyPriority(&Priority1, ADR(xxUnion));
            goto EXIT_13;
          } else if (xxToken == 20 || xxIsRepairMode) {
            goto EXIT_13;
          }
          xxExpected(17, 16, xxGlobalRecoverySet);
        } EXIT_13:;
        if (xxToken != 20) {
          xxRecoveryLiteral(20, 18, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        IO_WriteS(IO_StdOutput, "PROGRAM MODULE ", 15L);
        Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
        IO_WriteS(IO_StdOutput, " ;", 2L);
        IO_WriteNl(IO_StdOutput);
        for (;;) {
          if (IN(0, xxVerticalSet0.A[xxToken])) {
            xxUnion.LocalRecoverySet = 21;
            yyImport(&Import1, ADR(xxUnion));
          } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_14;
          } else {
            xxExpected(19, 20, xxGlobalRecoverySet);
          }
        } EXIT_14:;
        IO_WriteS(IO_StdOutput, "END ", 4L);
        Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
        IO_WriteS(IO_StdOutput, " ;", 2L);
        IO_WriteNl(IO_StdOutput);
        Halt();
        xxUnion.LocalRecoverySet = 12;
        yyBlock(&Block1, ADR(xxUnion));
        if (xxToken != 1) {
          xxRecoveryTerminal(1, 12, xxGlobalRecoverySet, &Ident2);
        } else {
          Ident2 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        if (xxToken != 15) {
          xxRecoveryLiteral(15, 13, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        goto EXIT_1;
      }
      xxExpected(22, 22, xxGlobalRecoverySet);
      break;
    }
  } EXIT_1:;
}

static void yyNumber
# ifdef __STDC__
(Parser_tParsAttribute *Number0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Number0, xxGlobalRecoverySet)
Parser_tParsAttribute *Number0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Integer1;
  Scanner_tScanAttribute Char1;
  Scanner_tScanAttribute Real1;

  for (;;) {
    switch (xxToken) {
    case 2:;
      Integer1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_15;
      break;
    case 4:;
      Char1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_15;
      break;
    case 6:;
      Real1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_15;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 2) {
          xxRecoveryTerminal(2, 23, xxGlobalRecoverySet, &Integer1);
        } else {
          Integer1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        goto EXIT_15;
      }
      xxExpected(26, 26, xxGlobalRecoverySet);
      break;
    }
  } EXIT_15:;
}

static void yyQualident
# ifdef __STDC__
(Parser_tParsAttribute *Qualident0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Qualident0, xxGlobalRecoverySet)
Parser_tParsAttribute *Qualident0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;

  for (;;) {
    if (xxToken != 1) {
      xxRecoveryTerminal(1, 12, xxGlobalRecoverySet, &Ident1);
    } else {
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
    if (!(xxToken == 15)) {
      if (IN(4, xxVerticalSet0.A[xxToken])) {
        goto EXIT_16;
      }
      xxExpected(13, 12, xxGlobalRecoverySet);
      if (!(xxToken == 15 || xxToken == 1)) {
        goto EXIT_16;
      }
    }
    if (xxToken != 15) {
      xxRecoveryLiteral(15, 12, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_16:;
}

static void yyConstantDeclaration
# ifdef __STDC__
(Parser_tParsAttribute *ConstantDeclaration0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ConstantDeclaration0, xxGlobalRecoverySet)
Parser_tParsAttribute *ConstantDeclaration0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute ConstExpression1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 27, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  if (xxToken != 24) {
    xxRecoveryLiteral(24, 27, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 0;
  yyConstExpression(&ConstExpression1, ADR(xxUnion));
}

static void yyConstExpression
# ifdef __STDC__
(Parser_tParsAttribute *ConstExpression0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ConstExpression0, xxGlobalRecoverySet)
Parser_tParsAttribute *ConstExpression0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Expression1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 0;
  yyExpression(&Expression1, ADR(xxUnion));
}

static void yyTypeDeclaration
# ifdef __STDC__
(Parser_tParsAttribute *TypeDeclaration0, xxtUnionPtr xxGlobalRecoverySet)
# else
(TypeDeclaration0, xxGlobalRecoverySet)
Parser_tParsAttribute *TypeDeclaration0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute Type1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 28, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  if (xxToken != 24) {
    xxRecoveryLiteral(24, 28, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 0;
  yyType(&Type1, ADR(xxUnion));
}

static void yyType
# ifdef __STDC__
(Parser_tParsAttribute *Type0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Type0, xxGlobalRecoverySet)
Parser_tParsAttribute *Type0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Type1;
  Parser_tParsAttribute SimpleType1;
  Parser_tParsAttribute FieldListSequence1;
  Parser_tParsAttribute FormalTypeList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
    case 9:;
    case 27:;
      xxUnion.LocalRecoverySet = 0;
      yySimpleType(&SimpleType1, ADR(xxUnion));
      goto EXIT_17;
      break;
    case 35:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        xxUnion.LocalRecoverySet = 29;
        yySimpleType(&SimpleType1, ADR(xxUnion));
        if (!(xxToken == 13)) {
          if (xxToken == 58) {
            goto EXIT_18;
          }
          xxExpected(30, 29, xxGlobalRecoverySet);
          if (!(xxToken == 13 || IN(5, xxVerticalSet0.A[xxToken]))) {
            goto EXIT_18;
          }
        }
        if (xxToken != 13) {
          xxRecoveryLiteral(13, 29, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_18:;
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 31, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyType(&Type1, ADR(xxUnion));
      goto EXIT_17;
      break;
    case 63:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 33;
      yyFieldListSequence(&FieldListSequence1, ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_17;
      break;
    case 66:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 35, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yySimpleType(&SimpleType1, ADR(xxUnion));
      goto EXIT_17;
      break;
    case 60:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 68) {
        xxRecoveryLiteral(68, 36, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyType(&Type1, ADR(xxUnion));
      goto EXIT_17;
      break;
    case 61:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 9) {
          xxUnion.LocalRecoverySet = 0;
          yyFormalTypeList(&FormalTypeList1, ADR(xxUnion));
          goto EXIT_19;
        } else if (IN(6, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_19;
        }
        xxExpected(38, 38, xxGlobalRecoverySet);
      } EXIT_19:;
      goto EXIT_17;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 0;
        yySimpleType(&SimpleType1, ADR(xxUnion));
        goto EXIT_17;
      }
      xxExpected(39, 39, xxGlobalRecoverySet);
      break;
    }
  } EXIT_17:;
}

static void yySimpleType
# ifdef __STDC__
(Parser_tParsAttribute *SimpleType0, xxtUnionPtr xxGlobalRecoverySet)
# else
(SimpleType0, xxGlobalRecoverySet)
Parser_tParsAttribute *SimpleType0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Qualident1;
  Parser_tParsAttribute ConstExpression1, ConstExpression2;
  Parser_tParsAttribute IdentList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
      xxUnion.LocalRecoverySet = 40;
      yyQualident(&Qualident1, ADR(xxUnion));
      for (;;) {
        if (xxToken == 27) {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 42;
          yyConstExpression(&ConstExpression1, ADR(xxUnion));
          if (xxToken != 16) {
            xxRecoveryLiteral(16, 42, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          xxUnion.LocalRecoverySet = 43;
          yyConstExpression(&ConstExpression2, ADR(xxUnion));
          if (xxToken != 28) {
            xxRecoveryLiteral(28, 43, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          goto EXIT_21;
        } else if (IN(7, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_21;
        }
        xxExpected(40, 40, xxGlobalRecoverySet);
      } EXIT_21:;
      goto EXIT_20;
      break;
    case 9:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 45;
      yyIdentList(&IdentList1, ADR(xxUnion));
      if (xxToken != 10) {
        xxRecoveryLiteral(10, 45, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_20;
      break;
    case 27:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 42;
      yyConstExpression(&ConstExpression1, ADR(xxUnion));
      if (xxToken != 16) {
        xxRecoveryLiteral(16, 42, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 43;
      yyConstExpression(&ConstExpression2, ADR(xxUnion));
      if (xxToken != 28) {
        xxRecoveryLiteral(28, 43, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_20;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 40;
        yyQualident(&Qualident1, ADR(xxUnion));
        for (;;) {
          if (xxToken == 27) {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
            xxUnion.LocalRecoverySet = 42;
            yyConstExpression(&ConstExpression1, ADR(xxUnion));
            if (xxToken != 16) {
              xxRecoveryLiteral(16, 42, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            xxUnion.LocalRecoverySet = 43;
            yyConstExpression(&ConstExpression2, ADR(xxUnion));
            if (xxToken != 28) {
              xxRecoveryLiteral(28, 43, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            goto EXIT_22;
          } else if (IN(7, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_22;
          }
          xxExpected(40, 40, xxGlobalRecoverySet);
        } EXIT_22:;
        goto EXIT_20;
      }
      xxExpected(46, 46, xxGlobalRecoverySet);
      break;
    }
  } EXIT_20:;
}

static void yyIdentList
# ifdef __STDC__
(Parser_tParsAttribute *IdentList0, xxtUnionPtr xxGlobalRecoverySet)
# else
(IdentList0, xxGlobalRecoverySet)
Parser_tParsAttribute *IdentList0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;

  for (;;) {
    if (xxToken != 1) {
      xxRecoveryTerminal(1, 48, xxGlobalRecoverySet, &Ident1);
    } else {
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
    if (!(xxToken == 13)) {
      if (IN(8, xxVerticalSet0.A[xxToken])) {
        goto EXIT_23;
      }
      xxExpected(47, 48, xxGlobalRecoverySet);
      if (!(xxToken == 13 || xxToken == 1)) {
        goto EXIT_23;
      }
    }
    if (xxToken != 13) {
      xxRecoveryLiteral(13, 48, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_23:;
}

static void yyFieldListSequence
# ifdef __STDC__
(Parser_tParsAttribute *FieldListSequence0, xxtUnionPtr xxGlobalRecoverySet)
# else
(FieldListSequence0, xxGlobalRecoverySet)
Parser_tParsAttribute *FieldListSequence0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute FieldList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    xxUnion.LocalRecoverySet = 49;
    yyFieldList(&FieldList1, ADR(xxUnion));
    if (!(xxToken == 20)) {
      if (IN(10, xxVerticalSet0.A[xxToken])) {
        goto EXIT_24;
      }
      xxExpected(49, 50, xxGlobalRecoverySet);
      if (!(xxToken == 20 || IN(9, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_24;
      }
    }
    if (xxToken != 20) {
      xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_24:;
}

static void yyFieldList
# ifdef __STDC__
(Parser_tParsAttribute *FieldList0, xxtUnionPtr xxGlobalRecoverySet)
# else
(FieldList0, xxGlobalRecoverySet)
Parser_tParsAttribute *FieldList0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute Qualident1;
  Parser_tParsAttribute Type1;
  Parser_tParsAttribute FieldListSequence1;
  Parser_tParsAttribute IdentList1;
  Parser_tParsAttribute Variant1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
      xxUnion.LocalRecoverySet = 51;
      yyIdentList(&IdentList1, ADR(xxUnion));
      if (xxToken != 18) {
        xxRecoveryLiteral(18, 51, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyType(&Type1, ADR(xxUnion));
      goto EXIT_25;
      break;
    case 38:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          Ident1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          goto EXIT_26;
        } else if (xxToken == 18 || xxIsRepairMode) {
          goto EXIT_26;
        }
        xxExpected(53, 54, xxGlobalRecoverySet);
      } EXIT_26:;
      if (xxToken != 18) {
        xxRecoveryLiteral(18, 54, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 55;
      yyQualident(&Qualident1, ADR(xxUnion));
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 55, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      for (;;) {
        xxUnion.LocalRecoverySet = 56;
        yyVariant(&Variant1, ADR(xxUnion));
        if (!(xxToken == 31)) {
          if (IN(12, xxVerticalSet0.A[xxToken])) {
            goto EXIT_27;
          }
          xxExpected(56, 57, xxGlobalRecoverySet);
          if (!(xxToken == 31 || IN(11, xxVerticalSet0.A[xxToken]))) {
            goto EXIT_27;
          }
        }
        if (xxToken != 31) {
          xxRecoveryLiteral(31, 57, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_27:;
      for (;;) {
        if (xxToken == 43) {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 33;
          yyFieldListSequence(&FieldListSequence1, ADR(xxUnion));
          goto EXIT_28;
        } else if (xxToken == 45 || xxIsRepairMode) {
          goto EXIT_28;
        }
        xxExpected(58, 58, xxGlobalRecoverySet);
      } EXIT_28:;
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_25;
      break;
    case 20:;
    case 31:;
    case 43:;
    case 45:;
      goto EXIT_25;
      break;
    default :
      if (xxIsRepairMode) {
        goto EXIT_25;
      }
      xxUnexpected(60, xxGlobalRecoverySet);
      break;
    }
  } EXIT_25:;
}

static void yyVariant
# ifdef __STDC__
(Parser_tParsAttribute *Variant0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Variant0, xxGlobalRecoverySet)
Parser_tParsAttribute *Variant0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute FieldListSequence1;
  Parser_tParsAttribute CaseLabelList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    if (IN(13, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 62;
      yyCaseLabelList(&CaseLabelList1, ADR(xxUnion));
      if (xxToken != 18) {
        xxRecoveryLiteral(18, 62, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyFieldListSequence(&FieldListSequence1, ADR(xxUnion));
      goto EXIT_29;
    } else if (IN(10, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_29;
    }
    xxExpected(61, 61, xxGlobalRecoverySet);
  } EXIT_29:;
}

static void yyCaseLabelList
# ifdef __STDC__
(Parser_tParsAttribute *CaseLabelList0, xxtUnionPtr xxGlobalRecoverySet)
# else
(CaseLabelList0, xxGlobalRecoverySet)
Parser_tParsAttribute *CaseLabelList0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute CaseLabels1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    xxUnion.LocalRecoverySet = 47;
    yyCaseLabels(&CaseLabels1, ADR(xxUnion));
    if (!(xxToken == 13)) {
      if (xxToken == 18) {
        goto EXIT_30;
      }
      xxExpected(47, 63, xxGlobalRecoverySet);
      if (!(xxToken == 13 || IN(13, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_30;
      }
    }
    if (xxToken != 13) {
      xxRecoveryLiteral(13, 63, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_30:;
}

static void yyCaseLabels
# ifdef __STDC__
(Parser_tParsAttribute *CaseLabels0, xxtUnionPtr xxGlobalRecoverySet)
# else
(CaseLabels0, xxGlobalRecoverySet)
Parser_tParsAttribute *CaseLabels0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute ConstExpression1, ConstExpression2;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 64;
  yyConstExpression(&ConstExpression1, ADR(xxUnion));
  for (;;) {
    if (xxToken == 16) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyConstExpression(&ConstExpression2, ADR(xxUnion));
      goto EXIT_31;
    } else if (IN(14, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_31;
    }
    xxExpected(64, 64, xxGlobalRecoverySet);
  } EXIT_31:;
}

static void yyFormalTypeList
# ifdef __STDC__
(Parser_tParsAttribute *FormalTypeList0, xxtUnionPtr xxGlobalRecoverySet)
# else
(FormalTypeList0, xxGlobalRecoverySet)
Parser_tParsAttribute *FormalTypeList0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Qualident1;
  Parser_tParsAttribute FormalType1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 9) {
    xxRecoveryLiteral(9, 66, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (IN(15, xxVerticalSet0.A[xxToken])) {
      for (;;) {
        for (;;) {
          if (xxToken == 71) {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
            goto EXIT_34;
          } else if (IN(16, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_34;
          }
          xxExpected(71, 70, xxGlobalRecoverySet);
        } EXIT_34:;
        xxUnion.LocalRecoverySet = 72;
        yyFormalType(&FormalType1, ADR(xxUnion));
        if (!(xxToken == 13)) {
          if (xxToken == 10) {
            goto EXIT_33;
          }
          xxExpected(69, 70, xxGlobalRecoverySet);
          if (!(xxToken == 13 || IN(15, xxVerticalSet0.A[xxToken]))) {
            goto EXIT_33;
          }
        }
        if (xxToken != 13) {
          xxRecoveryLiteral(13, 70, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_33:;
      goto EXIT_32;
    } else if (xxToken == 10 || xxIsRepairMode) {
      goto EXIT_32;
    }
    xxExpected(67, 68, xxGlobalRecoverySet);
  } EXIT_32:;
  if (xxToken != 10) {
    xxRecoveryLiteral(10, 73, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (xxToken == 18) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyQualident(&Qualident1, ADR(xxUnion));
      goto EXIT_35;
    } else if (IN(6, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_35;
    }
    xxExpected(74, 74, xxGlobalRecoverySet);
  } EXIT_35:;
}

static void yyVariableDeclaration
# ifdef __STDC__
(Parser_tParsAttribute *VariableDeclaration0, xxtUnionPtr xxGlobalRecoverySet)
# else
(VariableDeclaration0, xxGlobalRecoverySet)
Parser_tParsAttribute *VariableDeclaration0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Type1;
  Parser_tParsAttribute IdentList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 51;
  yyIdentList(&IdentList1, ADR(xxUnion));
  if (xxToken != 18) {
    xxRecoveryLiteral(18, 51, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 0;
  yyType(&Type1, ADR(xxUnion));
}

static void yyDesignator
# ifdef __STDC__
(Parser_tParsAttribute *Designator0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Designator0, xxGlobalRecoverySet)
Parser_tParsAttribute *Designator0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1, Ident2;
  Parser_tParsAttribute ExpList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 75, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (IN(17, xxVerticalSet0.A[xxToken])) {
      for (;;) {
        switch (xxToken) {
        case 15:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          if (xxToken != 1) {
            xxRecoveryTerminal(1, 77, xxGlobalRecoverySet, &Ident2);
          } else {
            Ident2 = Scanner_Attribute;
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          goto EXIT_37;
          break;
        case 27:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 43;
          yyExpList(&ExpList1, ADR(xxUnion));
          if (xxToken != 28) {
            xxRecoveryLiteral(28, 43, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          goto EXIT_37;
          break;
        case 29:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          goto EXIT_37;
          break;
        default :
          if (xxIsRepairMode) {
            if (xxToken != 29) {
              xxRecoveryLiteral(29, 79, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            goto EXIT_37;
          }
          xxExpected(76, 76, xxGlobalRecoverySet);
          break;
        }
      } EXIT_37:;
    } else if (IN(18, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_36;
    } else {
      xxExpected(76, 76, xxGlobalRecoverySet);
    }
  } EXIT_36:;
}

static void yyExpList
# ifdef __STDC__
(Parser_tParsAttribute *ExpList0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ExpList0, xxGlobalRecoverySet)
Parser_tParsAttribute *ExpList0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Expression1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    xxUnion.LocalRecoverySet = 47;
    yyExpression(&Expression1, ADR(xxUnion));
    if (!(xxToken == 13)) {
      if (IN(19, xxVerticalSet0.A[xxToken])) {
        goto EXIT_38;
      }
      xxExpected(47, 63, xxGlobalRecoverySet);
      if (!(xxToken == 13 || IN(13, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_38;
      }
    }
    if (xxToken != 13) {
      xxRecoveryLiteral(13, 63, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_38:;
}

static void yyExpression
# ifdef __STDC__
(Parser_tParsAttribute *Expression0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Expression0, xxGlobalRecoverySet)
Parser_tParsAttribute *Expression0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute SimpleExpression1, SimpleExpression2;
  Parser_tParsAttribute Relation1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 80;
  yySimpleExpression(&SimpleExpression1, ADR(xxUnion));
  for (;;) {
    if (IN(20, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 61;
      yyRelation(&Relation1, ADR(xxUnion));
      xxUnion.LocalRecoverySet = 0;
      yySimpleExpression(&SimpleExpression2, ADR(xxUnion));
      goto EXIT_39;
    } else if (IN(21, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_39;
    }
    xxExpected(80, 80, xxGlobalRecoverySet);
  } EXIT_39:;
}

static void yyRelation
# ifdef __STDC__
(Parser_tParsAttribute *Relation0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Relation0, xxGlobalRecoverySet)
Parser_tParsAttribute *Relation0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  for (;;) {
    switch (xxToken) {
    case 24:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_40;
      break;
    case 8:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_40;
      break;
    case 21:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_40;
      break;
    case 22:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_40;
      break;
    case 25:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_40;
      break;
    case 26:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_40;
      break;
    case 53:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_40;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 24) {
          xxRecoveryLiteral(24, 81, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        goto EXIT_40;
      }
      xxExpected(80, 80, xxGlobalRecoverySet);
      break;
    }
  } EXIT_40:;
}

static void yySimpleExpression
# ifdef __STDC__
(Parser_tParsAttribute *SimpleExpression0, xxtUnionPtr xxGlobalRecoverySet)
# else
(SimpleExpression0, xxGlobalRecoverySet)
Parser_tParsAttribute *SimpleExpression0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Term1, Term2;
  Parser_tParsAttribute AddOperator1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    if (IN(22, xxVerticalSet0.A[xxToken])) {
      if (xxToken == 12) {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_41;
    } else if (IN(23, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_41;
    }
    xxExpected(61, 88, xxGlobalRecoverySet);
  } EXIT_41:;
  xxUnion.LocalRecoverySet = 89;
  yyTerm(&Term1, ADR(xxUnion));
  for (;;) {
    if (IN(24, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 90;
      yyAddOperator(&AddOperator1, ADR(xxUnion));
      xxUnion.LocalRecoverySet = 0;
      yyTerm(&Term2, ADR(xxUnion));
    } else if (IN(25, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_42;
    } else {
      xxExpected(89, 89, xxGlobalRecoverySet);
    }
  } EXIT_42:;
}

static void yyAddOperator
# ifdef __STDC__
(Parser_tParsAttribute *AddOperator0, xxtUnionPtr xxGlobalRecoverySet)
# else
(AddOperator0, xxGlobalRecoverySet)
Parser_tParsAttribute *AddOperator0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  for (;;) {
    switch (xxToken) {
    case 12:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_43;
      break;
    case 14:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_43;
      break;
    case 59:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_43;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 12) {
          xxRecoveryLiteral(12, 91, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        goto EXIT_43;
      }
      xxExpected(89, 89, xxGlobalRecoverySet);
      break;
    }
  } EXIT_43:;
}

static void yyTerm
# ifdef __STDC__
(Parser_tParsAttribute *Term0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Term0, xxGlobalRecoverySet)
Parser_tParsAttribute *Term0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Factor1, Factor2;
  Parser_tParsAttribute MulOperator1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 94;
  yyFactor(&Factor1, ADR(xxUnion));
  for (;;) {
    if (IN(26, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 90;
      yyMulOperator(&MulOperator1, ADR(xxUnion));
      xxUnion.LocalRecoverySet = 0;
      yyFactor(&Factor2, ADR(xxUnion));
    } else if (IN(27, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_44;
    } else {
      xxExpected(94, 94, xxGlobalRecoverySet);
    }
  } EXIT_44:;
}

static void yyMulOperator
# ifdef __STDC__
(Parser_tParsAttribute *MulOperator0, xxtUnionPtr xxGlobalRecoverySet)
# else
(MulOperator0, xxGlobalRecoverySet)
Parser_tParsAttribute *MulOperator0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  for (;;) {
    switch (xxToken) {
    case 11:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_45;
      break;
    case 17:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_45;
      break;
    case 41:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_45;
      break;
    case 55:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_45;
      break;
    case 34:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_45;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 11) {
          xxRecoveryLiteral(11, 95, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        goto EXIT_45;
      }
      xxExpected(94, 94, xxGlobalRecoverySet);
      break;
    }
  } EXIT_45:;
}

static void yyFactor
# ifdef __STDC__
(Parser_tParsAttribute *Factor0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Factor0, xxGlobalRecoverySet)
Parser_tParsAttribute *Factor0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute String1;
  Parser_tParsAttribute Number1;
  Parser_tParsAttribute Expression1;
  Parser_tParsAttribute Designator1;
  Parser_tParsAttribute Factor1;
  Parser_tParsAttribute Set1;
  Parser_tParsAttribute ActualParameters1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 2:;
    case 4:;
    case 6:;
      xxUnion.LocalRecoverySet = 0;
      yyNumber(&Number1, ADR(xxUnion));
      goto EXIT_46;
      break;
    case 7:;
      String1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_46;
      break;
    case 30:;
      xxUnion.LocalRecoverySet = 0;
      yySet(&Set1, ADR(xxUnion));
      goto EXIT_46;
      break;
    case 1:;
      xxUnion.LocalRecoverySet = 101;
      yyDesignator(&Designator1, ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 8:;
        case 9:;
        case 10:;
        case 11:;
        case 12:;
        case 13:;
        case 14:;
        case 16:;
        case 17:;
        case 18:;
        case 20:;
        case 21:;
        case 22:;
        case 24:;
        case 25:;
        case 26:;
        case 28:;
        case 31:;
        case 32:;
        case 34:;
        case 37:;
        case 41:;
        case 42:;
        case 43:;
        case 44:;
        case 45:;
        case 53:;
        case 55:;
        case 58:;
        case 59:;
        case 67:;
        case 68:;
        case 70:;
          for (;;) {
            if (xxToken == 9) {
              xxUnion.LocalRecoverySet = 0;
              yyActualParameters(&ActualParameters1, ADR(xxUnion));
              goto EXIT_48;
            } else if (IN(28, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
              goto EXIT_48;
            }
            xxExpected(38, 38, xxGlobalRecoverySet);
          } EXIT_48:;
          goto EXIT_47;
          break;
        case 30:;
          xxUnion.LocalRecoverySet = 0;
          yySet(&Set1, ADR(xxUnion));
          goto EXIT_47;
          break;
        default :
          if (xxIsRepairMode) {
            for (;;) {
              if (xxToken == 9) {
                xxUnion.LocalRecoverySet = 0;
                yyActualParameters(&ActualParameters1, ADR(xxUnion));
                goto EXIT_49;
              } else if (IN(28, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
                goto EXIT_49;
              }
              xxExpected(38, 38, xxGlobalRecoverySet);
            } EXIT_49:;
            goto EXIT_47;
          }
          xxUnexpected(101, xxGlobalRecoverySet);
          break;
        }
      } EXIT_47:;
      goto EXIT_46;
      break;
    case 9:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 45;
      yyExpression(&Expression1, ADR(xxUnion));
      if (xxToken != 10) {
        xxRecoveryLiteral(10, 45, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_46;
      break;
    case 57:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyFactor(&Factor1, ADR(xxUnion));
      goto EXIT_46;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 0;
        yyNumber(&Number1, ADR(xxUnion));
        goto EXIT_46;
      }
      xxExpected(90, 90, xxGlobalRecoverySet);
      break;
    }
  } EXIT_46:;
}

static void yySet
# ifdef __STDC__
(Parser_tParsAttribute *Set0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Set0, xxGlobalRecoverySet)
Parser_tParsAttribute *Set0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Element1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 30) {
    xxRecoveryLiteral(30, 103, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (IN(13, xxVerticalSet0.A[xxToken])) {
      for (;;) {
        xxUnion.LocalRecoverySet = 104;
        yyElement(&Element1, ADR(xxUnion));
        if (!(xxToken == 13)) {
          if (xxToken == 32) {
            goto EXIT_51;
          }
          xxExpected(104, 105, xxGlobalRecoverySet);
          if (!(xxToken == 13 || IN(13, xxVerticalSet0.A[xxToken]))) {
            goto EXIT_51;
          }
        }
        if (xxToken != 13) {
          xxRecoveryLiteral(13, 105, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_51:;
      goto EXIT_50;
    } else if (xxToken == 32 || xxIsRepairMode) {
      goto EXIT_50;
    }
    xxExpected(103, 103, xxGlobalRecoverySet);
  } EXIT_50:;
  if (xxToken != 32) {
    xxRecoveryLiteral(32, 106, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
}

static void yyElement
# ifdef __STDC__
(Parser_tParsAttribute *Element0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Element0, xxGlobalRecoverySet)
Parser_tParsAttribute *Element0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Expression1, Expression2;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 64;
  yyExpression(&Expression1, ADR(xxUnion));
  for (;;) {
    if (xxToken == 16) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyExpression(&Expression2, ADR(xxUnion));
      goto EXIT_52;
    } else if (IN(29, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_52;
    }
    xxExpected(64, 64, xxGlobalRecoverySet);
  } EXIT_52:;
}

static void yyActualParameters
# ifdef __STDC__
(Parser_tParsAttribute *ActualParameters0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ActualParameters0, xxGlobalRecoverySet)
Parser_tParsAttribute *ActualParameters0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute ExpList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 9) {
    xxRecoveryLiteral(9, 102, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (IN(13, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 45;
      yyExpList(&ExpList1, ADR(xxUnion));
      goto EXIT_53;
    } else if (xxToken == 10 || xxIsRepairMode) {
      goto EXIT_53;
    }
    xxExpected(102, 102, xxGlobalRecoverySet);
  } EXIT_53:;
  if (xxToken != 10) {
    xxRecoveryLiteral(10, 45, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
}

static void yyStatement
# ifdef __STDC__
(Parser_tParsAttribute *Statement0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Statement0, xxGlobalRecoverySet)
Parser_tParsAttribute *Statement0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute ConstExpression1;
  Parser_tParsAttribute Expression1, Expression2;
  Parser_tParsAttribute Designator1;
  Parser_tParsAttribute ActualParameters1;
  Parser_tParsAttribute StatementSequence1, StatementSequence2, StatementSequence3;
  Parser_tParsAttribute Case1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
      xxUnion.LocalRecoverySet = 107;
      yyDesignator(&Designator1, ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 19:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 0;
          yyExpression(&Expression1, ADR(xxUnion));
          goto EXIT_55;
          break;
        case 9:;
        case 20:;
        case 31:;
        case 43:;
        case 44:;
        case 45:;
        case 70:;
          for (;;) {
            if (xxToken == 9) {
              xxUnion.LocalRecoverySet = 0;
              yyActualParameters(&ActualParameters1, ADR(xxUnion));
              goto EXIT_56;
            } else if (IN(30, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
              goto EXIT_56;
            }
            xxExpected(38, 38, xxGlobalRecoverySet);
          } EXIT_56:;
          goto EXIT_55;
          break;
        default :
          if (xxIsRepairMode) {
            for (;;) {
              if (xxToken == 9) {
                xxUnion.LocalRecoverySet = 0;
                yyActualParameters(&ActualParameters1, ADR(xxUnion));
                goto EXIT_57;
              } else if (IN(30, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
                goto EXIT_57;
              }
              xxExpected(38, 38, xxGlobalRecoverySet);
            } EXIT_57:;
            goto EXIT_55;
          }
          xxUnexpected(107, xxGlobalRecoverySet);
          break;
        }
      } EXIT_55:;
      goto EXIT_54;
      break;
    case 50:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 110;
      yyExpression(&Expression1, ADR(xxUnion));
      if (xxToken != 67) {
        xxRecoveryLiteral(67, 110, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 111;
      yyStatementSequence(&StatementSequence1, ADR(xxUnion));
      for (;;) {
        if (xxToken == 44) {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 112;
          yyExpression(&Expression2, ADR(xxUnion));
          if (xxToken != 67) {
            xxRecoveryLiteral(67, 112, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          xxUnion.LocalRecoverySet = 58;
          yyStatementSequence(&StatementSequence2, ADR(xxUnion));
        } else if (IN(12, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_58;
        } else {
          xxExpected(111, 111, xxGlobalRecoverySet);
        }
      } EXIT_58:;
      for (;;) {
        if (xxToken == 43) {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 33;
          yyStatementSequence(&StatementSequence3, ADR(xxUnion));
          goto EXIT_59;
        } else if (xxToken == 45 || xxIsRepairMode) {
          goto EXIT_59;
        }
        xxExpected(58, 58, xxGlobalRecoverySet);
      } EXIT_59:;
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_54;
      break;
    case 38:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 55;
      yyExpression(&Expression1, ADR(xxUnion));
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 55, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      for (;;) {
        xxUnion.LocalRecoverySet = 56;
        yyCase(&Case1, ADR(xxUnion));
        if (!(xxToken == 31)) {
          if (IN(12, xxVerticalSet0.A[xxToken])) {
            goto EXIT_60;
          }
          xxExpected(56, 57, xxGlobalRecoverySet);
          if (!(xxToken == 31 || IN(11, xxVerticalSet0.A[xxToken]))) {
            goto EXIT_60;
          }
        }
        if (xxToken != 31) {
          xxRecoveryLiteral(31, 57, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_60:;
      for (;;) {
        if (xxToken == 43) {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 33;
          yyStatementSequence(&StatementSequence1, ADR(xxUnion));
          goto EXIT_61;
        } else if (xxToken == 45 || xxIsRepairMode) {
          goto EXIT_61;
        }
        xxExpected(58, 58, xxGlobalRecoverySet);
      } EXIT_61:;
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_54;
      break;
    case 72:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 116;
      yyExpression(&Expression1, ADR(xxUnion));
      if (xxToken != 42) {
        xxRecoveryLiteral(42, 116, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 33;
      yyStatementSequence(&StatementSequence1, ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_54;
      break;
    case 64:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 118;
      yyStatementSequence(&StatementSequence1, ADR(xxUnion));
      if (xxToken != 70) {
        xxRecoveryLiteral(70, 118, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyExpression(&Expression1, ADR(xxUnion));
      goto EXIT_54;
      break;
    case 54:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 33;
      yyStatementSequence(&StatementSequence1, ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_54;
      break;
    case 48:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 120, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 19) {
        xxRecoveryLiteral(19, 120, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 121;
      yyExpression(&Expression1, ADR(xxUnion));
      if (xxToken != 68) {
        xxRecoveryLiteral(68, 121, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 122;
      yyExpression(&Expression2, ADR(xxUnion));
      for (;;) {
        if (xxToken == 37) {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 116;
          yyConstExpression(&ConstExpression1, ADR(xxUnion));
          goto EXIT_62;
        } else if (xxToken == 42 || xxIsRepairMode) {
          goto EXIT_62;
        }
        xxExpected(123, 122, xxGlobalRecoverySet);
      } EXIT_62:;
      if (xxToken != 42) {
        xxRecoveryLiteral(42, 116, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 33;
      yyStatementSequence(&StatementSequence1, ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_54;
      break;
    case 73:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 116;
      yyDesignator(&Designator1, ADR(xxUnion));
      if (xxToken != 42) {
        xxRecoveryLiteral(42, 116, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 33;
      yyStatementSequence(&StatementSequence1, ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_54;
      break;
    case 46:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_54;
      break;
    case 65:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (IN(13, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 0;
          yyExpression(&Expression1, ADR(xxUnion));
          goto EXIT_63;
        } else if (IN(30, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_63;
        }
        xxExpected(61, 61, xxGlobalRecoverySet);
      } EXIT_63:;
      goto EXIT_54;
      break;
    case 20:;
    case 31:;
    case 43:;
    case 44:;
    case 45:;
    case 70:;
      goto EXIT_54;
      break;
    default :
      if (xxIsRepairMode) {
        goto EXIT_54;
      }
      xxUnexpected(127, xxGlobalRecoverySet);
      break;
    }
  } EXIT_54:;
}

static void yyStatementSequence
# ifdef __STDC__
(Parser_tParsAttribute *StatementSequence0, xxtUnionPtr xxGlobalRecoverySet)
# else
(StatementSequence0, xxGlobalRecoverySet)
Parser_tParsAttribute *StatementSequence0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Statement1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    xxUnion.LocalRecoverySet = 49;
    yyStatement(&Statement1, ADR(xxUnion));
    if (!(xxToken == 20)) {
      if (IN(0, xxVerticalSet1.A[xxToken])) {
        goto EXIT_64;
      }
      xxExpected(49, 128, xxGlobalRecoverySet);
      if (!(xxToken == 20 || IN(31, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_64;
      }
    }
    if (xxToken != 20) {
      xxRecoveryLiteral(20, 128, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_64:;
}

static void yyCase
# ifdef __STDC__
(Parser_tParsAttribute *Case0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Case0, xxGlobalRecoverySet)
Parser_tParsAttribute *Case0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute CaseLabelList1;
  Parser_tParsAttribute StatementSequence1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    if (IN(13, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 129;
      yyCaseLabelList(&CaseLabelList1, ADR(xxUnion));
      if (xxToken != 18) {
        xxRecoveryLiteral(18, 129, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyStatementSequence(&StatementSequence1, ADR(xxUnion));
      goto EXIT_65;
    } else if (IN(10, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_65;
    }
    xxExpected(61, 61, xxGlobalRecoverySet);
  } EXIT_65:;
}

static void yyProcedureDeclaration
# ifdef __STDC__
(Parser_tParsAttribute *ProcedureDeclaration0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ProcedureDeclaration0, xxGlobalRecoverySet)
Parser_tParsAttribute *ProcedureDeclaration0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute Block1;
  Parser_tParsAttribute ProcedureHeading1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 130;
  yyProcedureHeading(&ProcedureHeading1, ADR(xxUnion));
  if (xxToken != 20) {
    xxRecoveryLiteral(20, 130, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 77;
  yyBlock(&Block1, ADR(xxUnion));
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 77, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
}

static void yyProcedureHeading
# ifdef __STDC__
(Parser_tParsAttribute *ProcedureHeading0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ProcedureHeading0, xxGlobalRecoverySet)
Parser_tParsAttribute *ProcedureHeading0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute FormalParameters1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 61) {
    xxRecoveryLiteral(61, 131, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 132, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (xxToken == 9) {
      xxUnion.LocalRecoverySet = 0;
      yyFormalParameters(&FormalParameters1, ADR(xxUnion));
      goto EXIT_66;
    } else if (xxToken == 20 || xxIsRepairMode) {
      goto EXIT_66;
    }
    xxExpected(38, 38, xxGlobalRecoverySet);
  } EXIT_66:;
}

static void yyBlock
# ifdef __STDC__
(Parser_tParsAttribute *Block0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Block0, xxGlobalRecoverySet)
Parser_tParsAttribute *Block0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute StatementSequence1;
  Parser_tParsAttribute Declaration1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    if (IN(1, xxVerticalSet1.A[xxToken])) {
      xxUnion.LocalRecoverySet = 134;
      yyDeclaration(&Declaration1, ADR(xxUnion));
    } else if (IN(2, xxVerticalSet1.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_67;
    } else {
      xxExpected(133, 133, xxGlobalRecoverySet);
    }
  } EXIT_67:;
  for (;;) {
    if (xxToken == 36) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 33;
      yyStatementSequence(&StatementSequence1, ADR(xxUnion));
      goto EXIT_68;
    } else if (xxToken == 45 || xxIsRepairMode) {
      goto EXIT_68;
    }
    xxExpected(134, 134, xxGlobalRecoverySet);
  } EXIT_68:;
  if (xxToken != 45) {
    xxRecoveryLiteral(45, 33, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
}

static void yyDeclaration
# ifdef __STDC__
(Parser_tParsAttribute *Declaration0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Declaration0, xxGlobalRecoverySet)
Parser_tParsAttribute *Declaration0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute ConstantDeclaration1;
  Parser_tParsAttribute TypeDeclaration1;
  Parser_tParsAttribute VariableDeclaration1;
  Parser_tParsAttribute ProcedureDeclaration1;
  Parser_tParsAttribute ModuleDeclaration1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 39:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 49;
          yyConstantDeclaration(&ConstantDeclaration1, ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_70;
        } else {
          xxExpected(77, 77, xxGlobalRecoverySet);
        }
      } EXIT_70:;
      goto EXIT_69;
      break;
    case 69:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 49;
          yyTypeDeclaration(&TypeDeclaration1, ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_71;
        } else {
          xxExpected(77, 77, xxGlobalRecoverySet);
        }
      } EXIT_71:;
      goto EXIT_69;
      break;
    case 71:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 49;
          yyVariableDeclaration(&VariableDeclaration1, ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_72;
        } else {
          xxExpected(77, 77, xxGlobalRecoverySet);
        }
      } EXIT_72:;
      goto EXIT_69;
      break;
    case 61:;
      xxUnion.LocalRecoverySet = 49;
      yyProcedureDeclaration(&ProcedureDeclaration1, ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_69;
      break;
    case 56:;
      xxUnion.LocalRecoverySet = 49;
      yyModuleDeclaration(&ModuleDeclaration1, ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_69;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 39) {
          xxRecoveryLiteral(39, 136, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        for (;;) {
          if (xxToken == 1) {
            xxUnion.LocalRecoverySet = 49;
            yyConstantDeclaration(&ConstantDeclaration1, ADR(xxUnion));
            if (xxToken != 20) {
              xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
          } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_73;
          } else {
            xxExpected(77, 77, xxGlobalRecoverySet);
          }
        } EXIT_73:;
        goto EXIT_69;
      }
      xxExpected(139, 139, xxGlobalRecoverySet);
      break;
    }
  } EXIT_69:;
}

static void yyFormalParameters
# ifdef __STDC__
(Parser_tParsAttribute *FormalParameters0, xxtUnionPtr xxGlobalRecoverySet)
# else
(FormalParameters0, xxGlobalRecoverySet)
Parser_tParsAttribute *FormalParameters0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Qualident1;
  Parser_tParsAttribute FPSection1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 9) {
    xxRecoveryLiteral(9, 140, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (IN(3, xxVerticalSet1.A[xxToken])) {
      for (;;) {
        xxUnion.LocalRecoverySet = 145;
        yyFPSection(&FPSection1, ADR(xxUnion));
        if (!(xxToken == 20)) {
          if (xxToken == 10) {
            goto EXIT_75;
          }
          xxExpected(143, 144, xxGlobalRecoverySet);
          if (!(xxToken == 20 || IN(3, xxVerticalSet1.A[xxToken]))) {
            goto EXIT_75;
          }
        }
        if (xxToken != 20) {
          xxRecoveryLiteral(20, 144, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_75:;
      goto EXIT_74;
    } else if (xxToken == 10 || xxIsRepairMode) {
      goto EXIT_74;
    }
    xxExpected(141, 142, xxGlobalRecoverySet);
  } EXIT_74:;
  if (xxToken != 10) {
    xxRecoveryLiteral(10, 73, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (xxToken == 18) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyQualident(&Qualident1, ADR(xxUnion));
      goto EXIT_76;
    } else if (xxToken == 20 || xxIsRepairMode) {
      goto EXIT_76;
    }
    xxExpected(74, 74, xxGlobalRecoverySet);
  } EXIT_76:;
}

static void yyFPSection
# ifdef __STDC__
(Parser_tParsAttribute *FPSection0, xxtUnionPtr xxGlobalRecoverySet)
# else
(FPSection0, xxGlobalRecoverySet)
Parser_tParsAttribute *FPSection0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute IdentList1;
  Parser_tParsAttribute FormalType1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    if (xxToken == 71) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_77;
    } else if (xxToken == 1 || xxIsRepairMode) {
      goto EXIT_77;
    }
    xxExpected(138, 146, xxGlobalRecoverySet);
  } EXIT_77:;
  xxUnion.LocalRecoverySet = 147;
  yyIdentList(&IdentList1, ADR(xxUnion));
  if (xxToken != 18) {
    xxRecoveryLiteral(18, 147, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 0;
  yyFormalType(&FormalType1, ADR(xxUnion));
}

static void yyFormalType
# ifdef __STDC__
(Parser_tParsAttribute *FormalType0, xxtUnionPtr xxGlobalRecoverySet)
# else
(FormalType0, xxGlobalRecoverySet)
Parser_tParsAttribute *FormalType0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Qualident1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    if (xxToken == 35) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 150, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_78;
    } else if (xxToken == 1 || xxIsRepairMode) {
      goto EXIT_78;
    }
    xxExpected(148, 148, xxGlobalRecoverySet);
  } EXIT_78:;
  xxUnion.LocalRecoverySet = 0;
  yyQualident(&Qualident1, ADR(xxUnion));
}

static void yyModuleDeclaration
# ifdef __STDC__
(Parser_tParsAttribute *ModuleDeclaration0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ModuleDeclaration0, xxGlobalRecoverySet)
Parser_tParsAttribute *ModuleDeclaration0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1, Ident2;
  Parser_tParsAttribute Import1;
  Parser_tParsAttribute Priority1;
  Parser_tParsAttribute Block1;
  Parser_tParsAttribute Export1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 56) {
    xxRecoveryLiteral(56, 151, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 151, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (xxToken == 27) {
      xxUnion.LocalRecoverySet = 152;
      yyPriority(&Priority1, ADR(xxUnion));
      goto EXIT_79;
    } else if (xxToken == 20 || xxIsRepairMode) {
      goto EXIT_79;
    }
    xxExpected(17, 151, xxGlobalRecoverySet);
  } EXIT_79:;
  if (xxToken != 20) {
    xxRecoveryLiteral(20, 152, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (IN(0, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 155;
      yyImport(&Import1, ADR(xxUnion));
    } else if (IN(4, xxVerticalSet1.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_80;
    } else {
      xxExpected(153, 154, xxGlobalRecoverySet);
    }
  } EXIT_80:;
  for (;;) {
    if (xxToken == 47) {
      xxUnion.LocalRecoverySet = 157;
      yyExport(&Export1, ADR(xxUnion));
      goto EXIT_81;
    } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_81;
    }
    xxExpected(156, 155, xxGlobalRecoverySet);
  } EXIT_81:;
  xxUnion.LocalRecoverySet = 77;
  yyBlock(&Block1, ADR(xxUnion));
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 77, xxGlobalRecoverySet, &Ident2);
  } else {
    Ident2 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
}

static void yyPriority
# ifdef __STDC__
(Parser_tParsAttribute *Priority0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Priority0, xxGlobalRecoverySet)
Parser_tParsAttribute *Priority0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute ConstExpression1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 27) {
    xxRecoveryLiteral(27, 78, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 43;
  yyConstExpression(&ConstExpression1, ADR(xxUnion));
  if (xxToken != 28) {
    xxRecoveryLiteral(28, 43, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
}

static void yyExport
# ifdef __STDC__
(Parser_tParsAttribute *Export0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Export0, xxGlobalRecoverySet)
Parser_tParsAttribute *Export0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute IdentList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 47) {
    xxRecoveryLiteral(47, 158, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (xxToken == 62) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      goto EXIT_82;
    } else if (xxToken == 1 || xxIsRepairMode) {
      goto EXIT_82;
    }
    xxExpected(159, 160, xxGlobalRecoverySet);
  } EXIT_82:;
  xxUnion.LocalRecoverySet = 49;
  yyIdentList(&IdentList1, ADR(xxUnion));
  if (xxToken != 20) {
    xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
}

static void yyImport
# ifdef __STDC__
(Parser_tParsAttribute *Import0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Import0, xxGlobalRecoverySet)
Parser_tParsAttribute *Import0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute IdentList1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 49:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 162, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 52) {
        xxRecoveryLiteral(52, 162, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 49;
      yyIdentList(&IdentList1, ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      IO_WriteS(IO_StdOutput, "   IMPORT ", 10L);
      Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
      IO_WriteS(IO_StdOutput, " ;", 2L);
      IO_WriteNl(IO_StdOutput);
      goto EXIT_83;
      break;
    case 52:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken != 1) {
          xxRecoveryTerminal(1, 165, xxGlobalRecoverySet, &Ident1);
        } else {
          Ident1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        IO_WriteS(IO_StdOutput, "   IMPORT ", 10L);
        Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
        IO_WriteS(IO_StdOutput, " ;", 2L);
        IO_WriteNl(IO_StdOutput);
        if (!(xxToken == 13)) {
          if (xxToken == 20) {
            goto EXIT_84;
          }
          xxExpected(164, 165, xxGlobalRecoverySet);
          if (!(xxToken == 13 || xxToken == 1)) {
            goto EXIT_84;
          }
        }
        if (xxToken != 13) {
          xxRecoveryLiteral(13, 165, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_84:;
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_83;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 52) {
          xxRecoveryLiteral(52, 163, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        for (;;) {
          if (xxToken != 1) {
            xxRecoveryTerminal(1, 165, xxGlobalRecoverySet, &Ident1);
          } else {
            Ident1 = Scanner_Attribute;
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          IO_WriteS(IO_StdOutput, "   IMPORT ", 10L);
          Idents_WriteIdent(IO_StdOutput, Ident1.U_1.V_1.Ident);
          IO_WriteS(IO_StdOutput, " ;", 2L);
          IO_WriteNl(IO_StdOutput);
          if (!(xxToken == 13)) {
            if (xxToken == 20) {
              goto EXIT_85;
            }
            xxExpected(164, 165, xxGlobalRecoverySet);
            if (!(xxToken == 13 || xxToken == 1)) {
              goto EXIT_85;
            }
          }
          if (xxToken != 13) {
            xxRecoveryLiteral(13, 165, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } EXIT_85:;
        if (xxToken != 20) {
          xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        goto EXIT_83;
      }
      xxExpected(166, 166, xxGlobalRecoverySet);
      break;
    }
  } EXIT_83:;
}

static void yyDefinition
# ifdef __STDC__
(Parser_tParsAttribute *Definition0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Definition0, xxGlobalRecoverySet)
Parser_tParsAttribute *Definition0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute ConstantDeclaration1;
  Parser_tParsAttribute Type1;
  Parser_tParsAttribute VariableDeclaration1;
  Parser_tParsAttribute ProcedureHeading1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 39:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 49;
          yyConstantDeclaration(&ConstantDeclaration1, ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_87;
        } else {
          xxExpected(77, 77, xxGlobalRecoverySet);
        }
      } EXIT_87:;
      goto EXIT_86;
      break;
    case 69:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          Ident1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          for (;;) {
            if (xxToken == 24) {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
              xxUnion.LocalRecoverySet = 49;
              yyType(&Type1, ADR(xxUnion));
              goto EXIT_89;
            } else if (xxToken == 20 || xxIsRepairMode) {
              goto EXIT_89;
            }
            xxExpected(168, 168, xxGlobalRecoverySet);
          } EXIT_89:;
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_88;
        } else {
          xxExpected(77, 77, xxGlobalRecoverySet);
        }
      } EXIT_88:;
      goto EXIT_86;
      break;
    case 71:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 49;
          yyVariableDeclaration(&VariableDeclaration1, ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_90;
        } else {
          xxExpected(77, 77, xxGlobalRecoverySet);
        }
      } EXIT_90:;
      goto EXIT_86;
      break;
    case 61:;
      xxUnion.LocalRecoverySet = 49;
      yyProcedureHeading(&ProcedureHeading1, ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      goto EXIT_86;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 39) {
          xxRecoveryLiteral(39, 136, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        for (;;) {
          if (xxToken == 1) {
            xxUnion.LocalRecoverySet = 49;
            yyConstantDeclaration(&ConstantDeclaration1, ADR(xxUnion));
            if (xxToken != 20) {
              xxRecoveryLiteral(20, 49, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
          } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_91;
          } else {
            xxExpected(77, 77, xxGlobalRecoverySet);
          }
        } EXIT_91:;
        goto EXIT_86;
      }
      xxExpected(170, 170, xxGlobalRecoverySet);
      break;
    }
  } EXIT_86:;
}

void BEGIN_Parser()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Scanner();
    BEGIN_Positions();
    BEGIN_Errors();
    BEGIN_Scanner();
    BEGIN_Strings();
    BEGIN_System();
    BEGIN_System();
    BEGIN_IO();
    BEGIN_Idents();
    BEGIN_Scanner();
    BEGIN_Errors();

    xxIsInitialized = FALSE;
    (void)strncpy(Parser_ParsTabName.A, "Parser.Tab", sizeof(Parser_ParsTabName.A));
  }
}
