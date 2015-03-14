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

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Parser
#include "Parser.h"
#endif

Parser_tParsAttribute Parser_ParsAttribute;
struct Parser_2 Parser_ParsTabName;

static SHORTCARD Kind;
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
    xxtSet A[171 + 1];
} xxHorizontalSet;
static struct S_4 {
    BITSET A[74 + 1];
} xxVerticalSet0;
static void Copy ARGS((CHAR Source[], LONGCARD , CHAR Target[], LONGCARD ));
static BOOLEAN xxIsElement ARGS((xxtSet *Set, SHORTCARD Element));
static void xxUnexpected ARGS((SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet));
static void xxExpected ARGS((SHORTCARD ExpectedSet, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet));
struct S_8 {
    CHAR A[127 + 1];
};
static void xxRecoveryLiteral ARGS((SHORTCARD Expected, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet));
struct S_9 {
    CHAR A[127 + 1];
};
static void xxRecoveryTerminal ARGS((SHORTCARD Expected, SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet, Scanner_tScanAttribute *RepairAttribute));
struct S_10 {
    CHAR A[127 + 1];
};
static void xxSkipTokens ARGS((SHORTCARD LocalRecoverySet, xxtUnionPtr GlobalRecoverySet));
static void BeginParser ARGS(());
static void yyCompUnit ARGS((Parser_tParsAttribute *CompUnit0, xxtUnionPtr xxGlobalRecoverySet));
static void yyQualid ARGS((Parser_tParsAttribute *Qualid0, xxtUnionPtr xxGlobalRecoverySet));
static void yyConstDecl ARGS((Parser_tParsAttribute *ConstDecl0, xxtUnionPtr xxGlobalRecoverySet));
static void yyTypeDecl ARGS((Parser_tParsAttribute *TypeDecl0, xxtUnionPtr xxGlobalRecoverySet));
static void yyType ARGS((Parser_tParsAttribute *Type0, xxtUnionPtr xxGlobalRecoverySet));
static void yySimpleType ARGS((Parser_tParsAttribute *SimpleType0, xxtUnionPtr xxGlobalRecoverySet));
static void yyElemType ARGS((Parser_tParsAttribute *ElemType0, xxtUnionPtr xxGlobalRecoverySet));
static void yyTypeId ARGS((Parser_tParsAttribute *TypeId0, xxtUnionPtr xxGlobalRecoverySet));
static void yyEnumIds ARGS((Parser_tParsAttribute *EnumIds0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFields ARGS((Parser_tParsAttribute *Fields0, xxtUnionPtr xxGlobalRecoverySet));
static void yyField ARGS((Parser_tParsAttribute *Field0, xxtUnionPtr xxGlobalRecoverySet));
static void yyTagField ARGS((Parser_tParsAttribute *TagField0, xxtUnionPtr xxGlobalRecoverySet));
static void yyVariants ARGS((Parser_tParsAttribute *Variants0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFieldIds ARGS((Parser_tParsAttribute *FieldIds0, xxtUnionPtr xxGlobalRecoverySet));
static void yyLabels ARGS((Parser_tParsAttribute *Labels0, xxtUnionPtr xxGlobalRecoverySet));
static void yyLabel ARGS((Parser_tParsAttribute *Label0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFormalTypes ARGS((Parser_tParsAttribute *FormalTypes0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFormalType ARGS((Parser_tParsAttribute *FormalType0, xxtUnionPtr xxGlobalRecoverySet));
static void yyResultType ARGS((Parser_tParsAttribute *ResultType0, xxtUnionPtr xxGlobalRecoverySet));
static void yyVarDecl ARGS((Parser_tParsAttribute *VarDecl0, xxtUnionPtr xxGlobalRecoverySet));
static void yyVarIds ARGS((Parser_tParsAttribute *VarIds0, xxtUnionPtr xxGlobalRecoverySet));
static void yyDesignator ARGS((Parser_tParsAttribute *Designator0, xxtUnionPtr xxGlobalRecoverySet));
static void yyExpr ARGS((Parser_tParsAttribute *Expr0, xxtUnionPtr xxGlobalRecoverySet));
static void yyRelOpr ARGS((Parser_tParsAttribute *RelOpr0, xxtUnionPtr xxGlobalRecoverySet));
static void yySimpleExpr ARGS((Parser_tParsAttribute *SimpleExpr0, xxtUnionPtr xxGlobalRecoverySet));
static void yyAddOpr ARGS((Parser_tParsAttribute *AddOpr0, xxtUnionPtr xxGlobalRecoverySet));
static void yyTerm ARGS((Parser_tParsAttribute *Term0, xxtUnionPtr xxGlobalRecoverySet));
static void yyMulOpr ARGS((Parser_tParsAttribute *MulOpr0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFactor ARGS((Parser_tParsAttribute *Factor0, xxtUnionPtr xxGlobalRecoverySet));
static void yyElems ARGS((Parser_tParsAttribute *Elems0, xxtUnionPtr xxGlobalRecoverySet));
static void yyElem ARGS((Parser_tParsAttribute *Elem0, xxtUnionPtr xxGlobalRecoverySet));
static void yyActuals ARGS((Parser_tParsAttribute *Actuals0, xxtUnionPtr xxGlobalRecoverySet));
static void yyStmt ARGS((Parser_tParsAttribute *Stmt0, xxtUnionPtr xxGlobalRecoverySet));
static void yyElsifs ARGS((Parser_tParsAttribute *Elsifs0, xxtUnionPtr xxGlobalRecoverySet));
static void yyCases ARGS((Parser_tParsAttribute *Cases0, xxtUnionPtr xxGlobalRecoverySet));
static void yyStmts ARGS((Parser_tParsAttribute *Stmts0, xxtUnionPtr xxGlobalRecoverySet));
static void yyProcDecl ARGS((Parser_tParsAttribute *ProcDecl0, xxtUnionPtr xxGlobalRecoverySet));
static void yyBlock ARGS((Parser_tParsAttribute *Block0, xxtUnionPtr xxGlobalRecoverySet));
static void yyDecl ARGS((Parser_tParsAttribute *Decl0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFormals ARGS((Parser_tParsAttribute *Formals0, xxtUnionPtr xxGlobalRecoverySet));
static void yyFPSection ARGS((Parser_tParsAttribute *FPSection0, xxtUnionPtr xxGlobalRecoverySet));
static void yyParIds ARGS((Parser_tParsAttribute *ParIds0, xxtUnionPtr xxGlobalRecoverySet));
static void yyModDecl ARGS((Parser_tParsAttribute *ModDecl0, xxtUnionPtr xxGlobalRecoverySet));
static void yyPriority ARGS((Parser_tParsAttribute *Priority0, xxtUnionPtr xxGlobalRecoverySet));
static void yyExport ARGS((Parser_tParsAttribute *Export0, xxtUnionPtr xxGlobalRecoverySet));
static void yyExpIds ARGS((Parser_tParsAttribute *ExpIds0, xxtUnionPtr xxGlobalRecoverySet));
static void yyImport ARGS((Parser_tParsAttribute *Import0, xxtUnionPtr xxGlobalRecoverySet));
static void yyImpIds ARGS((Parser_tParsAttribute *ImpIds0, xxtUnionPtr xxGlobalRecoverySet));
static void yyDef ARGS((Parser_tParsAttribute *Def0, xxtUnionPtr xxGlobalRecoverySet));


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
    Copy((STRING)"_EndOfFile", 10L, Name, O_3);
    break;
  case 1:;
    Copy((STRING)"Ident", 5L, Name, O_3);
    break;
  case 2:;
    Copy((STRING)"DecConst", 8L, Name, O_3);
    break;
  case 3:;
    Copy((STRING)"OctalConst", 10L, Name, O_3);
    break;
  case 4:;
    Copy((STRING)"HexConst", 8L, Name, O_3);
    break;
  case 5:;
    Copy((STRING)"CharConst", 9L, Name, O_3);
    break;
  case 6:;
    Copy((STRING)"RealConst", 9L, Name, O_3);
    break;
  case 7:;
    Copy((STRING)"StringConst", 11L, Name, O_3);
    break;
  case 8:;
    Copy((STRING)"#", 1L, Name, O_3);
    break;
  case 9:;
    Copy((STRING)"(", 1L, Name, O_3);
    break;
  case 10:;
    Copy((STRING)")", 1L, Name, O_3);
    break;
  case 11:;
    Copy((STRING)"*", 1L, Name, O_3);
    break;
  case 12:;
    Copy((STRING)"+", 1L, Name, O_3);
    break;
  case 13:;
    Copy((STRING)"Comma", 5L, Name, O_3);
    break;
  case 14:;
    Copy((STRING)"-", 1L, Name, O_3);
    break;
  case 15:;
    Copy((STRING)".", 1L, Name, O_3);
    break;
  case 16:;
    Copy((STRING)"..", 2L, Name, O_3);
    break;
  case 17:;
    Copy((STRING)"/", 1L, Name, O_3);
    break;
  case 18:;
    Copy((STRING)":", 1L, Name, O_3);
    break;
  case 19:;
    Copy((STRING)":=", 2L, Name, O_3);
    break;
  case 20:;
    Copy((STRING)";", 1L, Name, O_3);
    break;
  case 21:;
    Copy((STRING)"<", 1L, Name, O_3);
    break;
  case 22:;
    Copy((STRING)"<=", 2L, Name, O_3);
    break;
  case 24:;
    Copy((STRING)"=", 1L, Name, O_3);
    break;
  case 25:;
    Copy((STRING)">", 1L, Name, O_3);
    break;
  case 26:;
    Copy((STRING)">=", 2L, Name, O_3);
    break;
  case 27:;
    Copy((STRING)"LBracket", 8L, Name, O_3);
    break;
  case 28:;
    Copy((STRING)"]", 1L, Name, O_3);
    break;
  case 29:;
    Copy((STRING)"Arrow", 5L, Name, O_3);
    break;
  case 30:;
    Copy((STRING)"{", 1L, Name, O_3);
    break;
  case 31:;
    Copy((STRING)"|", 1L, Name, O_3);
    break;
  case 32:;
    Copy((STRING)"}", 1L, Name, O_3);
    break;
  case 34:;
    Copy((STRING)"AND", 3L, Name, O_3);
    break;
  case 35:;
    Copy((STRING)"ARRAY", 5L, Name, O_3);
    break;
  case 36:;
    Copy((STRING)"BEGIN", 5L, Name, O_3);
    break;
  case 37:;
    Copy((STRING)"BY", 2L, Name, O_3);
    break;
  case 38:;
    Copy((STRING)"CASE", 4L, Name, O_3);
    break;
  case 39:;
    Copy((STRING)"CONST", 5L, Name, O_3);
    break;
  case 40:;
    Copy((STRING)"DEFINITION", 10L, Name, O_3);
    break;
  case 41:;
    Copy((STRING)"DIV", 3L, Name, O_3);
    break;
  case 42:;
    Copy((STRING)"DO", 2L, Name, O_3);
    break;
  case 43:;
    Copy((STRING)"ELSE", 4L, Name, O_3);
    break;
  case 44:;
    Copy((STRING)"ELSIF", 5L, Name, O_3);
    break;
  case 45:;
    Copy((STRING)"END", 3L, Name, O_3);
    break;
  case 46:;
    Copy((STRING)"EXIT", 4L, Name, O_3);
    break;
  case 47:;
    Copy((STRING)"EXPORT", 6L, Name, O_3);
    break;
  case 48:;
    Copy((STRING)"FOR", 3L, Name, O_3);
    break;
  case 49:;
    Copy((STRING)"FROM", 4L, Name, O_3);
    break;
  case 50:;
    Copy((STRING)"IF", 2L, Name, O_3);
    break;
  case 51:;
    Copy((STRING)"IMPLEMENTATION", 14L, Name, O_3);
    break;
  case 52:;
    Copy((STRING)"IMPORT", 6L, Name, O_3);
    break;
  case 53:;
    Copy((STRING)"IN", 2L, Name, O_3);
    break;
  case 54:;
    Copy((STRING)"LOOP", 4L, Name, O_3);
    break;
  case 55:;
    Copy((STRING)"MOD", 3L, Name, O_3);
    break;
  case 56:;
    Copy((STRING)"MODULE", 6L, Name, O_3);
    break;
  case 57:;
    Copy((STRING)"NOT", 3L, Name, O_3);
    break;
  case 58:;
    Copy((STRING)"OF", 2L, Name, O_3);
    break;
  case 59:;
    Copy((STRING)"OR", 2L, Name, O_3);
    break;
  case 60:;
    Copy((STRING)"POINTER", 7L, Name, O_3);
    break;
  case 61:;
    Copy((STRING)"PROCEDURE", 9L, Name, O_3);
    break;
  case 62:;
    Copy((STRING)"QUALIFIED", 9L, Name, O_3);
    break;
  case 63:;
    Copy((STRING)"RECORD", 6L, Name, O_3);
    break;
  case 64:;
    Copy((STRING)"REPEAT", 6L, Name, O_3);
    break;
  case 65:;
    Copy((STRING)"RETURN", 6L, Name, O_3);
    break;
  case 66:;
    Copy((STRING)"SET", 3L, Name, O_3);
    break;
  case 67:;
    Copy((STRING)"THEN", 4L, Name, O_3);
    break;
  case 68:;
    Copy((STRING)"TO", 2L, Name, O_3);
    break;
  case 69:;
    Copy((STRING)"TYPE", 4L, Name, O_3);
    break;
  case 70:;
    Copy((STRING)"UNTIL", 5L, Name, O_3);
    break;
  case 71:;
    Copy((STRING)"VAR", 3L, Name, O_3);
    break;
  case 72:;
    Copy((STRING)"WHILE", 5L, Name, O_3);
    break;
  case 73:;
    Copy((STRING)"WITH", 4L, Name, O_3);
    break;
  case 74:;
    Copy((STRING)"FOREIGN", 7L, Name, O_3);
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
  yyCompUnit(&Parser_ParsAttribute, (xxtUnionPtr)NIL);
  if (xxToken != xxEof) {
    xxRecoveryLiteral(xxEof, 0, (xxtUnionPtr)NIL);
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
    Errors_ErrorMessage((LONGCARD)Errors_SyntaxError, (LONGCARD)Errors_Error, Scanner_Attribute.Position);
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
  struct S_8 TokenArray;
  Strings_tString TokenString;
  Strings_tString ContinueString;

  if (!xxIsRepairMode) {
    INC(xxErrorCount);
    Errors_ErrorMessage((LONGCARD)Errors_SyntaxError, (LONGCARD)Errors_Error, Scanner_Attribute.Position);
    Strings_AssignEmpty(&ContinueString);
    for (Token = 0; Token <= 74; Token += 1) {
      if (xxIsElement(&xxHorizontalSet.A[ExpectedSet], Token)) {
        Parser_xxTokenName((LONGCARD)Token, TokenArray.A, 128L);
        Strings_ArrayToString(TokenArray.A, 128L, &TokenString);
        if (Strings_Length(&ContinueString) + Strings_Length(&TokenString) + 1 <= Strings_cMaxStrLength) {
          Strings_Concatenate(&ContinueString, &TokenString);
          Strings_Append(&ContinueString, ' ');
        }
      }
    }
    Errors_ErrorMessageI((LONGCARD)Errors_ExpectedTokens, (LONGCARD)Errors_Information, Scanner_Attribute.Position, (LONGCARD)Errors_String, ADR(ContinueString));
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
  struct S_9 TokenString;

  if (!xxIsRepairMode) {
    INC(xxErrorCount);
    Errors_ErrorMessage((LONGCARD)Errors_SyntaxError, (LONGCARD)Errors_Error, Scanner_Attribute.Position);
    Parser_xxTokenName((LONGCARD)Expected, TokenString.A, 128L);
    Errors_ErrorMessageI((LONGCARD)Errors_ExpectedTokens, (LONGCARD)Errors_Information, Scanner_Attribute.Position, (LONGCARD)Errors_Array, ADR(TokenString));
    xxSkipTokens(LocalRecoverySet, GlobalRecoverySet);
  }
  if (xxToken != Expected) {
    Parser_xxTokenName((LONGCARD)Expected, TokenString.A, 128L);
    Errors_ErrorMessageI((LONGCARD)Errors_TokenInserted, (LONGCARD)Errors_Repair, Scanner_Attribute.Position, (LONGCARD)Errors_Array, ADR(TokenString));
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
  struct S_10 TokenString;

  if (!xxIsRepairMode) {
    INC(xxErrorCount);
    Errors_ErrorMessage((LONGCARD)Errors_SyntaxError, (LONGCARD)Errors_Error, Scanner_Attribute.Position);
    Parser_xxTokenName((LONGCARD)Expected, TokenString.A, 128L);
    Errors_ErrorMessageI((LONGCARD)Errors_ExpectedTokens, (LONGCARD)Errors_Information, Scanner_Attribute.Position, (LONGCARD)Errors_Array, ADR(TokenString));
    xxSkipTokens(LocalRecoverySet, GlobalRecoverySet);
  }
  if (xxToken != Expected) {
    Parser_xxTokenName((LONGCARD)Expected, TokenString.A, 128L);
    Errors_ErrorMessageI((LONGCARD)Errors_TokenInserted, (LONGCARD)Errors_Repair, Scanner_Attribute.Position, (LONGCARD)Errors_Array, ADR(TokenString));
    Scanner_ErrorAttribute((LONGCARD)Expected, RepairAttribute);
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
    Errors_ErrorMessage((LONGCARD)Errors_RestartPoint, (LONGCARD)Errors_Information, Scanner_Attribute.Position);
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
    Errors_ErrorMessage((LONGCARD)Errors_ReadParseTable, (LONGCARD)Errors_Fatal, Positions_NoPosition);
  }
  xxSize = Read(xxTableFile, ADR(xxHorizontalSet), (LONGINT)sizeof(xxHorizontalSet));
  if (xxSize != sizeof(xxHorizontalSet)) {
    Errors_ErrorMessage((LONGCARD)Errors_ReadParseTable, (LONGCARD)Errors_Fatal, Positions_NoPosition);
  }
  xxSize = Read(xxTableFile, ADR(xxVerticalSet0), (LONGINT)sizeof(xxVerticalSet0));
  if (xxSize != sizeof(xxVerticalSet0)) {
    Errors_ErrorMessage((LONGCARD)Errors_ReadParseTable, (LONGCARD)Errors_Fatal, Positions_NoPosition);
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

static void yyCompUnit
# ifdef __STDC__
(Parser_tParsAttribute *CompUnit0, xxtUnionPtr xxGlobalRecoverySet)
# else
(CompUnit0, xxGlobalRecoverySet)
Parser_tParsAttribute *CompUnit0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1, Ident2, Ident3;
  Parser_tParsAttribute Import1;
  Parser_tParsAttribute Def1;
  Parser_tParsAttribute Priority1;
  Parser_tParsAttribute Block1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 40:;
    case 74:;
      if (xxToken == 40) {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
        Kind = Tree_Definition;
        for (;;) {
          if (xxToken == 48) {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
            if (xxToken != 1) {
              xxRecoveryTerminal(1, 4, xxGlobalRecoverySet, &Ident1);
            } else {
              Ident1 = Scanner_Attribute;
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            Kind = Tree_Foreign;
            goto EXIT_2;
          } else if (xxToken == 56 || xxIsRepairMode) {
            goto EXIT_2;
          }
          xxExpected(2, 3, xxGlobalRecoverySet);
        } EXIT_2:;
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
        Kind = Tree_Foreign;
      }
      if (xxToken != 56) {
        xxRecoveryLiteral(56, 4, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 8, xxGlobalRecoverySet, &Ident2);
      } else {
        Ident2 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 8, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Import1.U_1.V_0.Tree = Tree_mImport0();
      for (;;) {
        if (IN(0, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 11;
          yyImport(&Import1, (xxtUnionPtr)ADR(xxUnion));
        } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_3;
        } else {
          xxExpected(9, 10, xxGlobalRecoverySet);
        }
      } EXIT_3:;
      Import1.U_1.V_0.Tree = Tree_ReverseTree(Import1.U_1.V_0.Tree);
      Def1.U_1.V_0.Tree = Tree_mDecls0();
      for (;;) {
        if (IN(2, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 13;
          yyDef(&Def1, (xxtUnionPtr)ADR(xxUnion));
        } else if (xxToken == 45 || xxIsRepairMode) {
          goto EXIT_4;
        } else {
          xxExpected(12, 11, xxGlobalRecoverySet);
        }
      } EXIT_4:;
      Def1.U_1.V_0.Tree = Tree_ReverseTree(Def1.U_1.V_0.Tree);
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 13, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 14, xxGlobalRecoverySet, &Ident3);
      } else {
        Ident3 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 15) {
        xxRecoveryLiteral(15, 15, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      CompUnit0->U_1.V_0.Tree = Tree_mDefMod(Kind, Ident2.U_1.V_1.Ident, Ident2.Position, (Tree_tTree)Tree_NoTree, Import1.U_1.V_0.Tree, Def1.U_1.V_0.Tree);
      goto EXIT_1;
      break;
    case 51:;
    case 56:;
      if (xxToken == 51) {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
        Kind = Tree_Implementation;
      } else {
        Kind = Tree_Program;
      }
      if (xxToken != 56) {
        xxRecoveryLiteral(56, 18, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 18, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      for (;;) {
        if (xxToken == 27) {
          xxUnion.LocalRecoverySet = 20;
          yyPriority(&Priority1, (xxtUnionPtr)ADR(xxUnion));
          goto EXIT_5;
        } else if (xxToken == 20 || xxIsRepairMode) {
          goto EXIT_5;
        }
        xxExpected(19, 18, xxGlobalRecoverySet);
      } EXIT_5:;
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 20, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Import1.U_1.V_0.Tree = Tree_mImport0();
      for (;;) {
        if (IN(0, xxVerticalSet0.A[xxToken])) {
          xxUnion.LocalRecoverySet = 23;
          yyImport(&Import1, (xxtUnionPtr)ADR(xxUnion));
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_6;
        } else {
          xxExpected(21, 22, xxGlobalRecoverySet);
        }
      } EXIT_6:;
      Import1.U_1.V_0.Tree = Tree_ReverseTree(Import1.U_1.V_0.Tree);
      xxUnion.LocalRecoverySet = 14;
      yyBlock(&Block1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 14, xxGlobalRecoverySet, &Ident2);
      } else {
        Ident2 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 15) {
        xxRecoveryLiteral(15, 15, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      CompUnit0->U_1.V_0.Tree = Tree_mProgMod(Kind, Ident1.U_1.V_1.Ident, Ident1.Position, (Tree_tTree)Tree_NoTree, Import1.U_1.V_0.Tree, Block1.U_1.V_2.Decls, Block1.U_1.V_2.Stmts);
      goto EXIT_1;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken == 51) {
          if (xxToken != 51) {
            xxRecoveryLiteral(51, 16, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          Kind = Tree_Implementation;
        } else {
          Kind = Tree_Program;
        }
        if (xxToken != 56) {
          xxRecoveryLiteral(56, 18, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        if (xxToken != 1) {
          xxRecoveryTerminal(1, 18, xxGlobalRecoverySet, &Ident1);
        } else {
          Ident1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        for (;;) {
          if (xxToken == 27) {
            xxUnion.LocalRecoverySet = 20;
            yyPriority(&Priority1, (xxtUnionPtr)ADR(xxUnion));
            goto EXIT_7;
          } else if (xxToken == 20 || xxIsRepairMode) {
            goto EXIT_7;
          }
          xxExpected(19, 18, xxGlobalRecoverySet);
        } EXIT_7:;
        if (xxToken != 20) {
          xxRecoveryLiteral(20, 20, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        Import1.U_1.V_0.Tree = Tree_mImport0();
        for (;;) {
          if (IN(0, xxVerticalSet0.A[xxToken])) {
            xxUnion.LocalRecoverySet = 23;
            yyImport(&Import1, (xxtUnionPtr)ADR(xxUnion));
          } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_8;
          } else {
            xxExpected(21, 22, xxGlobalRecoverySet);
          }
        } EXIT_8:;
        Import1.U_1.V_0.Tree = Tree_ReverseTree(Import1.U_1.V_0.Tree);
        xxUnion.LocalRecoverySet = 14;
        yyBlock(&Block1, (xxtUnionPtr)ADR(xxUnion));
        if (xxToken != 1) {
          xxRecoveryTerminal(1, 14, xxGlobalRecoverySet, &Ident2);
        } else {
          Ident2 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        if (xxToken != 15) {
          xxRecoveryLiteral(15, 15, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        CompUnit0->U_1.V_0.Tree = Tree_mProgMod(Kind, Ident1.U_1.V_1.Ident, Ident1.Position, (Tree_tTree)Tree_NoTree, Import1.U_1.V_0.Tree, Block1.U_1.V_2.Decls, Block1.U_1.V_2.Stmts);
        goto EXIT_1;
      }
      xxExpected(24, 24, xxGlobalRecoverySet);
      break;
    }
  } EXIT_1:;
}

static void yyQualid
# ifdef __STDC__
(Parser_tParsAttribute *Qualid0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Qualid0, xxGlobalRecoverySet)
Parser_tParsAttribute *Qualid0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1, Ident2;

  if (xxToken != 1) {
    xxRecoveryTerminal(1, 14, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  Qualid0->U_1.V_0.Tree = Tree_mQualid0(Ident1.Position, Ident1.U_1.V_1.Ident);
  for (;;) {
    if (xxToken == 15) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 25, xxGlobalRecoverySet, &Ident2);
      } else {
        Ident2 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Qualid0->U_1.V_0.Tree = Tree_mQualid1(Ident2.Position, Ident2.U_1.V_1.Ident, Qualid0->U_1.V_0.Tree);
    } else if (IN(4, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_9;
    } else {
      xxExpected(15, 15, xxGlobalRecoverySet);
    }
  } EXIT_9:;
}

static void yyConstDecl
# ifdef __STDC__
(Parser_tParsAttribute *ConstDecl0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ConstDecl0, xxGlobalRecoverySet)
Parser_tParsAttribute *ConstDecl0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute Expr1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 26, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  if (xxToken != 24) {
    xxRecoveryLiteral(24, 26, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 0;
  yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
  ConstDecl0->U_1.V_0.Tree = Tree_mConst(ConstDecl0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident, Expr1.U_1.V_0.Tree);
}

static void yyTypeDecl
# ifdef __STDC__
(Parser_tParsAttribute *TypeDecl0, xxtUnionPtr xxGlobalRecoverySet)
# else
(TypeDecl0, xxGlobalRecoverySet)
Parser_tParsAttribute *TypeDecl0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute Type1;
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
  yyType(&Type1, (xxtUnionPtr)ADR(xxUnion));
  TypeDecl0->U_1.V_0.Tree = Tree_mTypeDecl(TypeDecl0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident, Type1.U_1.V_0.Tree, Ident1.Position);
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
  Parser_tParsAttribute ElemType1;
  Parser_tParsAttribute Fields1;
  Parser_tParsAttribute FormalTypes1;
  Parser_tParsAttribute ResultType1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
    case 9:;
    case 27:;
      xxUnion.LocalRecoverySet = 0;
      yySimpleType(&SimpleType1, (xxtUnionPtr)ADR(xxUnion));
      Type0->U_1.V_0.Tree = SimpleType1.U_1.V_0.Tree;
      goto EXIT_10;
      break;
    case 35:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 29;
      yySimpleType(&SimpleType1, (xxtUnionPtr)ADR(xxUnion));
      xxUnion.LocalRecoverySet = 0;
      yyElemType(&ElemType1, (xxtUnionPtr)ADR(xxUnion));
      Type0->U_1.V_0.Tree = Tree_mArray(FALSE, SimpleType1.U_1.V_0.Tree, ElemType1.U_1.V_0.Tree);
      goto EXIT_10;
      break;
    case 63:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 31;
      yyFields(&Fields1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Type0->U_1.V_0.Tree = Tree_mRecord(Fields1.U_1.V_0.Tree);
      goto EXIT_10;
      break;
    case 66:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 33, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yySimpleType(&SimpleType1, (xxtUnionPtr)ADR(xxUnion));
      Type0->U_1.V_0.Tree = Tree_mSetType(SimpleType1.U_1.V_0.Tree);
      goto EXIT_10;
      break;
    case 60:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 68) {
        xxRecoveryLiteral(68, 34, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyType(&Type1, (xxtUnionPtr)ADR(xxUnion));
      Type0->U_1.V_0.Tree = Tree_mPointer(Type1.U_1.V_0.Tree);
      goto EXIT_10;
      break;
    case 61:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        switch (xxToken) {
        case 9:;
          xxUnion.LocalRecoverySet = 36;
          yyFormalTypes(&FormalTypes1, (xxtUnionPtr)ADR(xxUnion));
          xxUnion.LocalRecoverySet = 0;
          yyResultType(&ResultType1, (xxtUnionPtr)ADR(xxUnion));
          goto EXIT_11;
          break;
        case 20:;
        case 31:;
        case 43:;
        case 45:;
          FormalTypes1.U_1.V_0.Tree = Tree_mFormalTypes0();
          ResultType1.U_1.V_0.Tree = Tree_mVoid();
          goto EXIT_11;
          break;
        default :
          if (xxIsRepairMode) {
            FormalTypes1.U_1.V_0.Tree = Tree_mFormalTypes0();
            ResultType1.U_1.V_0.Tree = Tree_mVoid();
            goto EXIT_11;
          }
          xxUnexpected(37, xxGlobalRecoverySet);
          break;
        }
      } EXIT_11:;
      Type0->U_1.V_0.Tree = Tree_mProcType(FormalTypes1.U_1.V_0.Tree, ResultType1.U_1.V_0.Tree);
      goto EXIT_10;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 0;
        yySimpleType(&SimpleType1, (xxtUnionPtr)ADR(xxUnion));
        Type0->U_1.V_0.Tree = SimpleType1.U_1.V_0.Tree;
        goto EXIT_10;
      }
      xxExpected(38, 38, xxGlobalRecoverySet);
      break;
    }
  } EXIT_10:;
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
  Scanner_tScanAttribute LBracket1;
  Parser_tParsAttribute Expr1, Expr2;
  Parser_tParsAttribute TypeId1;
  Parser_tParsAttribute EnumIds1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
      xxUnion.LocalRecoverySet = 39;
      yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 27:;
          LBracket1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 41;
          yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 16) {
            xxRecoveryLiteral(16, 41, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          xxUnion.LocalRecoverySet = 42;
          yyExpr(&Expr2, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 28) {
            xxRecoveryLiteral(28, 42, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          SimpleType0->U_1.V_0.Tree = Tree_mSubrange(TypeId1.U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Expr2.U_1.V_0.Tree);
          goto EXIT_13;
          break;
        case 13:;
        case 20:;
        case 31:;
        case 43:;
        case 45:;
        case 58:;
          SimpleType0->U_1.V_0.Tree = TypeId1.U_1.V_0.Tree;
          goto EXIT_13;
          break;
        default :
          if (xxIsRepairMode) {
            SimpleType0->U_1.V_0.Tree = TypeId1.U_1.V_0.Tree;
            goto EXIT_13;
          }
          xxUnexpected(39, xxGlobalRecoverySet);
          break;
        }
      } EXIT_13:;
      goto EXIT_12;
      break;
    case 9:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 44;
      yyEnumIds(&EnumIds1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 10) {
        xxRecoveryLiteral(10, 44, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      SimpleType0->U_1.V_0.Tree = Tree_mEnumeration(EnumIds1.U_1.V_0.Tree);
      goto EXIT_12;
      break;
    case 27:;
      LBracket1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 41;
      yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 16) {
        xxRecoveryLiteral(16, 41, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 42;
      yyExpr(&Expr2, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 28) {
        xxRecoveryLiteral(28, 42, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      SimpleType0->U_1.V_0.Tree = Tree_mSubrange(Tree_mVoid(), Expr1.U_1.V_0.Tree, Expr2.U_1.V_0.Tree);
      goto EXIT_12;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 39;
        yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
        for (;;) {
          switch (xxToken) {
          case 27:;
            if (xxToken != 27) {
              xxRecoveryTerminal(27, 40, xxGlobalRecoverySet, &LBracket1);
            } else {
              LBracket1 = Scanner_Attribute;
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            xxUnion.LocalRecoverySet = 41;
            yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
            if (xxToken != 16) {
              xxRecoveryLiteral(16, 41, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            xxUnion.LocalRecoverySet = 42;
            yyExpr(&Expr2, (xxtUnionPtr)ADR(xxUnion));
            if (xxToken != 28) {
              xxRecoveryLiteral(28, 42, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            SimpleType0->U_1.V_0.Tree = Tree_mSubrange(TypeId1.U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Expr2.U_1.V_0.Tree);
            goto EXIT_14;
            break;
          case 13:;
          case 20:;
          case 31:;
          case 43:;
          case 45:;
          case 58:;
            SimpleType0->U_1.V_0.Tree = TypeId1.U_1.V_0.Tree;
            goto EXIT_14;
            break;
          default :
            if (xxIsRepairMode) {
              SimpleType0->U_1.V_0.Tree = TypeId1.U_1.V_0.Tree;
              goto EXIT_14;
            }
            xxUnexpected(39, xxGlobalRecoverySet);
            break;
          }
        } EXIT_14:;
        goto EXIT_12;
      }
      xxExpected(45, 45, xxGlobalRecoverySet);
      break;
    }
  } EXIT_12:;
}

static void yyElemType
# ifdef __STDC__
(Parser_tParsAttribute *ElemType0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ElemType0, xxGlobalRecoverySet)
Parser_tParsAttribute *ElemType0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Comma1;
  Parser_tParsAttribute Type1;
  Parser_tParsAttribute SimpleType1;
  Parser_tParsAttribute ElemType1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 13:;
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 29;
      yySimpleType(&SimpleType1, (xxtUnionPtr)ADR(xxUnion));
      xxUnion.LocalRecoverySet = 0;
      yyElemType(&ElemType1, (xxtUnionPtr)ADR(xxUnion));
      ElemType0->U_1.V_0.Tree = Tree_mArray(FALSE, SimpleType1.U_1.V_0.Tree, ElemType1.U_1.V_0.Tree);
      goto EXIT_15;
      break;
    case 58:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyType(&Type1, (xxtUnionPtr)ADR(xxUnion));
      ElemType0->U_1.V_0.Tree = Type1.U_1.V_0.Tree;
      goto EXIT_15;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 58) {
          xxRecoveryLiteral(58, 47, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        xxUnion.LocalRecoverySet = 0;
        yyType(&Type1, (xxtUnionPtr)ADR(xxUnion));
        ElemType0->U_1.V_0.Tree = Type1.U_1.V_0.Tree;
        goto EXIT_15;
      }
      xxExpected(29, 29, xxGlobalRecoverySet);
      break;
    }
  } EXIT_15:;
}

static void yyTypeId
# ifdef __STDC__
(Parser_tParsAttribute *TypeId0, xxtUnionPtr xxGlobalRecoverySet)
# else
(TypeId0, xxGlobalRecoverySet)
Parser_tParsAttribute *TypeId0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1, Ident2;

  if (xxToken != 1) {
    xxRecoveryTerminal(1, 14, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  TypeId0->U_1.V_0.Tree = Tree_mTypeId0(Ident1.U_1.V_1.Ident, Ident1.Position);
  for (;;) {
    if (xxToken == 15) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 25, xxGlobalRecoverySet, &Ident2);
      } else {
        Ident2 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      TypeId0->U_1.V_0.Tree = Tree_mTypeId1(Ident2.U_1.V_1.Ident, Ident2.Position, TypeId0->U_1.V_0.Tree);
    } else if (IN(5, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_16;
    } else {
      xxExpected(15, 15, xxGlobalRecoverySet);
    }
  } EXIT_16:;
}

static void yyEnumIds
# ifdef __STDC__
(Parser_tParsAttribute *EnumIds0, xxtUnionPtr xxGlobalRecoverySet)
# else
(EnumIds0, xxGlobalRecoverySet)
Parser_tParsAttribute *EnumIds0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Scanner_tScanAttribute Comma1;

  EnumIds0->U_1.V_0.Tree = Tree_mEnumIds0();
  for (;;) {
    if (xxToken != 1) {
      xxRecoveryTerminal(1, 49, xxGlobalRecoverySet, &Ident1);
    } else {
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
    EnumIds0->U_1.V_0.Tree = Tree_mEnumIds1(Ident1.U_1.V_1.Ident, EnumIds0->U_1.V_0.Tree);
    if (!(xxToken == 13)) {
      if (xxToken == 10) {
        goto EXIT_17;
      }
      xxExpected(48, 49, xxGlobalRecoverySet);
      if (!(xxToken == 13 || xxToken == 1)) {
        goto EXIT_17;
      }
    }
    if (xxToken != 13) {
      xxRecoveryTerminal(13, 49, xxGlobalRecoverySet, &Comma1);
    } else {
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_17:;
  EnumIds0->U_1.V_0.Tree = Tree_ReverseTree(EnumIds0->U_1.V_0.Tree);
}

static void yyFields
# ifdef __STDC__
(Parser_tParsAttribute *Fields0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Fields0, xxGlobalRecoverySet)
Parser_tParsAttribute *Fields0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Field1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Field1.U_1.V_0.Tree = Tree_mFields0();
  for (;;) {
    xxUnion.LocalRecoverySet = 50;
    yyField(&Field1, (xxtUnionPtr)ADR(xxUnion));
    if (!(xxToken == 20)) {
      if (IN(7, xxVerticalSet0.A[xxToken])) {
        goto EXIT_18;
      }
      xxExpected(50, 51, xxGlobalRecoverySet);
      if (!(xxToken == 20 || IN(6, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_18;
      }
    }
    if (xxToken != 20) {
      xxRecoveryLiteral(20, 51, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_18:;
  Fields0->U_1.V_0.Tree = Tree_ReverseTree(Field1.U_1.V_0.Tree);
}

static void yyField
# ifdef __STDC__
(Parser_tParsAttribute *Field0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Field0, xxGlobalRecoverySet)
Parser_tParsAttribute *Field0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Type1;
  Parser_tParsAttribute Fields1;
  Parser_tParsAttribute FieldIds1;
  Parser_tParsAttribute TagField1;
  Parser_tParsAttribute Variants1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
      xxUnion.LocalRecoverySet = 52;
      yyFieldIds(&FieldIds1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 18) {
        xxRecoveryLiteral(18, 52, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyType(&Type1, (xxtUnionPtr)ADR(xxUnion));
      Field0->U_1.V_0.Tree = Tree_mRecordSect(Field0->U_1.V_0.Tree, FieldIds1.U_1.V_0.Tree, Type1.U_1.V_0.Tree);
      goto EXIT_19;
      break;
    case 38:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 54;
      yyTagField(&TagField1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 54, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 55;
      yyVariants(&Variants1, (xxtUnionPtr)ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 43:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 31;
          yyFields(&Fields1, (xxtUnionPtr)ADR(xxUnion));
          goto EXIT_20;
          break;
        case 45:;
          Fields1.U_1.V_0.Tree = Tree_mFields0();
          goto EXIT_20;
          break;
        default :
          if (xxIsRepairMode) {
            Fields1.U_1.V_0.Tree = Tree_mFields0();
            goto EXIT_20;
          }
          xxExpected(55, 55, xxGlobalRecoverySet);
          break;
        }
      } EXIT_20:;
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Field0->U_1.V_0.Tree = Tree_mVariantSect(Field0->U_1.V_0.Tree, TagField1.U_1.V_0.Tree, Variants1.U_1.V_0.Tree, Fields1.U_1.V_0.Tree);
      goto EXIT_19;
      break;
    case 20:;
    case 31:;
    case 43:;
    case 45:;
      goto EXIT_19;
      break;
    default :
      if (xxIsRepairMode) {
        goto EXIT_19;
      }
      xxUnexpected(57, xxGlobalRecoverySet);
      break;
    }
  } EXIT_19:;
}

static void yyTagField
# ifdef __STDC__
(Parser_tParsAttribute *TagField0, xxtUnionPtr xxGlobalRecoverySet)
# else
(TagField0, xxGlobalRecoverySet)
Parser_tParsAttribute *TagField0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute TypeId1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 18) {
        xxRecoveryLiteral(18, 58, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
      TagField0->U_1.V_0.Tree = Tree_mTagField1(TypeId1.U_1.V_0.Tree, Ident1.U_1.V_1.Ident);
      goto EXIT_21;
      break;
    case 18:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
      TagField0->U_1.V_0.Tree = Tree_mTagField0(TypeId1.U_1.V_0.Tree);
      goto EXIT_21;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 18) {
          xxRecoveryLiteral(18, 58, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        xxUnion.LocalRecoverySet = 0;
        yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
        TagField0->U_1.V_0.Tree = Tree_mTagField0(TypeId1.U_1.V_0.Tree);
        goto EXIT_21;
      }
      xxExpected(58, 58, xxGlobalRecoverySet);
      break;
    }
  } EXIT_21:;
}

static void yyVariants
# ifdef __STDC__
(Parser_tParsAttribute *Variants0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Variants0, xxGlobalRecoverySet)
Parser_tParsAttribute *Variants0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Fields1;
  Parser_tParsAttribute Labels1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Variants0->U_1.V_0.Tree = Tree_mVariants0();
  for (;;) {
    for (;;) {
      if (IN(10, xxVerticalSet0.A[xxToken])) {
        xxUnion.LocalRecoverySet = 61;
        yyLabels(&Labels1, (xxtUnionPtr)ADR(xxUnion));
        if (xxToken != 18) {
          xxRecoveryLiteral(18, 61, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        xxUnion.LocalRecoverySet = 59;
        yyFields(&Fields1, (xxtUnionPtr)ADR(xxUnion));
        Variants0->U_1.V_0.Tree = Tree_mVariant(Labels1.U_1.V_0.Tree, Fields1.U_1.V_0.Tree, Variants0->U_1.V_0.Tree);
        goto EXIT_23;
      } else if (IN(7, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
        goto EXIT_23;
      }
      xxExpected(60, 60, xxGlobalRecoverySet);
    } EXIT_23:;
    if (!(xxToken == 31)) {
      if (IN(9, xxVerticalSet0.A[xxToken])) {
        goto EXIT_22;
      }
      xxExpected(59, 60, xxGlobalRecoverySet);
      if (!(xxToken == 31 || IN(8, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_22;
      }
    }
    if (xxToken != 31) {
      xxRecoveryLiteral(31, 60, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_22:;
  Variants0->U_1.V_0.Tree = Tree_ReverseTree(Variants0->U_1.V_0.Tree);
}

static void yyFieldIds
# ifdef __STDC__
(Parser_tParsAttribute *FieldIds0, xxtUnionPtr xxGlobalRecoverySet)
# else
(FieldIds0, xxGlobalRecoverySet)
Parser_tParsAttribute *FieldIds0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Scanner_tScanAttribute Comma1;

  FieldIds0->U_1.V_0.Tree = Tree_mFieldIds0();
  for (;;) {
    if (xxToken != 1) {
      xxRecoveryTerminal(1, 49, xxGlobalRecoverySet, &Ident1);
    } else {
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
    FieldIds0->U_1.V_0.Tree = Tree_mFieldIds1(Ident1.U_1.V_1.Ident, FieldIds0->U_1.V_0.Tree);
    if (!(xxToken == 13)) {
      if (xxToken == 18) {
        goto EXIT_24;
      }
      xxExpected(48, 49, xxGlobalRecoverySet);
      if (!(xxToken == 13 || xxToken == 1)) {
        goto EXIT_24;
      }
    }
    if (xxToken != 13) {
      xxRecoveryTerminal(13, 49, xxGlobalRecoverySet, &Comma1);
    } else {
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_24:;
  FieldIds0->U_1.V_0.Tree = Tree_ReverseTree(FieldIds0->U_1.V_0.Tree);
}

static void yyLabels
# ifdef __STDC__
(Parser_tParsAttribute *Labels0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Labels0, xxGlobalRecoverySet)
Parser_tParsAttribute *Labels0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Comma1;
  Parser_tParsAttribute Label1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Label1.U_1.V_0.Tree = Tree_mLabels0();
  for (;;) {
    xxUnion.LocalRecoverySet = 48;
    yyLabel(&Label1, (xxtUnionPtr)ADR(xxUnion));
    if (!(xxToken == 13)) {
      if (xxToken == 18) {
        goto EXIT_25;
      }
      xxExpected(48, 62, xxGlobalRecoverySet);
      if (!(xxToken == 13 || IN(10, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_25;
      }
    }
    if (xxToken != 13) {
      xxRecoveryTerminal(13, 62, xxGlobalRecoverySet, &Comma1);
    } else {
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_25:;
  Labels0->U_1.V_0.Tree = Tree_ReverseTree(Label1.U_1.V_0.Tree);
}

static void yyLabel
# ifdef __STDC__
(Parser_tParsAttribute *Label0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Label0, xxGlobalRecoverySet)
Parser_tParsAttribute *Label0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Expr1, Expr2;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 63;
  yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
  for (;;) {
    switch (xxToken) {
    case 16:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyExpr(&Expr2, (xxtUnionPtr)ADR(xxUnion));
      Label0->U_1.V_0.Tree = Tree_mLabelRange(Label0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Expr2.U_1.V_0.Tree);
      goto EXIT_26;
      break;
    case 13:;
    case 18:;
      Label0->U_1.V_0.Tree = Tree_mLabel(Label0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
      goto EXIT_26;
      break;
    default :
      if (xxIsRepairMode) {
        Label0->U_1.V_0.Tree = Tree_mLabel(Label0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
        goto EXIT_26;
      }
      xxUnexpected(63, xxGlobalRecoverySet);
      break;
    }
  } EXIT_26:;
}

static void yyFormalTypes
# ifdef __STDC__
(Parser_tParsAttribute *FormalTypes0, xxtUnionPtr xxGlobalRecoverySet)
# else
(FormalTypes0, xxGlobalRecoverySet)
Parser_tParsAttribute *FormalTypes0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Comma1;
  Parser_tParsAttribute FormalType1, FormalType2;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  FormalTypes0->U_1.V_0.Tree = Tree_mFormalTypes0();
  if (xxToken != 9) {
    xxRecoveryLiteral(9, 65, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    for (;;) {
      if (IN(12, xxVerticalSet0.A[xxToken])) {
        if (xxToken == 71) {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 66;
          yyFormalType(&FormalType1, (xxtUnionPtr)ADR(xxUnion));
          FormalTypes0->U_1.V_0.Tree = Tree_mFormalType(TRUE, FormalType1.U_1.V_0.Tree, FormalTypes0->U_1.V_0.Tree);
        } else {
          xxUnion.LocalRecoverySet = 66;
          yyFormalType(&FormalType2, (xxtUnionPtr)ADR(xxUnion));
          FormalTypes0->U_1.V_0.Tree = Tree_mFormalType(FALSE, FormalType2.U_1.V_0.Tree, FormalTypes0->U_1.V_0.Tree);
        }
        goto EXIT_28;
      } else if (IN(13, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
        goto EXIT_28;
      }
      xxExpected(67, 67, xxGlobalRecoverySet);
    } EXIT_28:;
    if (!(xxToken == 13)) {
      if (xxToken == 10) {
        goto EXIT_27;
      }
      xxExpected(66, 67, xxGlobalRecoverySet);
      if (!(xxToken == 13 || IN(11, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_27;
      }
    }
    if (xxToken != 13) {
      xxRecoveryTerminal(13, 67, xxGlobalRecoverySet, &Comma1);
    } else {
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_27:;
  if (xxToken != 10) {
    xxRecoveryLiteral(10, 44, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  FormalTypes0->U_1.V_0.Tree = Tree_ReverseTree(FormalTypes0->U_1.V_0.Tree);
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
  Parser_tParsAttribute TypeId1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 35:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 69, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
      FormalType0->U_1.V_0.Tree = Tree_mArray(TRUE, Tree_mTypeId0(Defs_IdentLONGCARD, Positions_NoPosition), TypeId1.U_1.V_0.Tree);
      goto EXIT_29;
      break;
    case 1:;
      xxUnion.LocalRecoverySet = 0;
      yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
      FormalType0->U_1.V_0.Tree = TypeId1.U_1.V_0.Tree;
      goto EXIT_29;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 0;
        yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
        FormalType0->U_1.V_0.Tree = TypeId1.U_1.V_0.Tree;
        goto EXIT_29;
      }
      xxExpected(70, 70, xxGlobalRecoverySet);
      break;
    }
  } EXIT_29:;
}

static void yyResultType
# ifdef __STDC__
(Parser_tParsAttribute *ResultType0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ResultType0, xxGlobalRecoverySet)
Parser_tParsAttribute *ResultType0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute TypeId1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 18:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyTypeId(&TypeId1, (xxtUnionPtr)ADR(xxUnion));
      ResultType0->U_1.V_0.Tree = TypeId1.U_1.V_0.Tree;
      goto EXIT_30;
      break;
    case 20:;
    case 31:;
    case 43:;
    case 45:;
      ResultType0->U_1.V_0.Tree = Tree_mVoid();
      goto EXIT_30;
      break;
    default :
      if (xxIsRepairMode) {
        ResultType0->U_1.V_0.Tree = Tree_mVoid();
        goto EXIT_30;
      }
      xxUnexpected(36, xxGlobalRecoverySet);
      break;
    }
  } EXIT_30:;
}

static void yyVarDecl
# ifdef __STDC__
(Parser_tParsAttribute *VarDecl0, xxtUnionPtr xxGlobalRecoverySet)
# else
(VarDecl0, xxGlobalRecoverySet)
Parser_tParsAttribute *VarDecl0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Type1;
  Parser_tParsAttribute VarIds1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 52;
  yyVarIds(&VarIds1, (xxtUnionPtr)ADR(xxUnion));
  if (xxToken != 18) {
    xxRecoveryLiteral(18, 52, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 0;
  yyType(&Type1, (xxtUnionPtr)ADR(xxUnion));
  VarDecl0->U_1.V_0.Tree = Tree_mVar(VarDecl0->U_1.V_0.Tree, VarIds1.U_1.V_0.Tree, Type1.U_1.V_0.Tree);
}

static void yyVarIds
# ifdef __STDC__
(Parser_tParsAttribute *VarIds0, xxtUnionPtr xxGlobalRecoverySet)
# else
(VarIds0, xxGlobalRecoverySet)
Parser_tParsAttribute *VarIds0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Scanner_tScanAttribute Comma1;

  VarIds0->U_1.V_0.Tree = Tree_mVarIds0();
  for (;;) {
    if (xxToken != 1) {
      xxRecoveryTerminal(1, 49, xxGlobalRecoverySet, &Ident1);
    } else {
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
    VarIds0->U_1.V_0.Tree = Tree_mVarIds1(Ident1.U_1.V_1.Ident, VarIds0->U_1.V_0.Tree);
    if (!(xxToken == 13)) {
      if (xxToken == 18) {
        goto EXIT_31;
      }
      xxExpected(48, 49, xxGlobalRecoverySet);
      if (!(xxToken == 13 || xxToken == 1)) {
        goto EXIT_31;
      }
    }
    if (xxToken != 13) {
      xxRecoveryTerminal(13, 49, xxGlobalRecoverySet, &Comma1);
    } else {
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_31:;
  VarIds0->U_1.V_0.Tree = Tree_ReverseTree(VarIds0->U_1.V_0.Tree);
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
  Scanner_tScanAttribute Ident1;
  Scanner_tScanAttribute Comma1;
  Scanner_tScanAttribute LBracket1;
  Scanner_tScanAttribute Arrow1;
  Parser_tParsAttribute Qualid1;
  Parser_tParsAttribute Expr1, Expr2;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 71;
  yyQualid(&Qualid1, (xxtUnionPtr)ADR(xxUnion));
  Designator0->U_1.V_0.Tree = Qualid1.U_1.V_0.Tree;
  for (;;) {
    if (IN(15, xxVerticalSet0.A[xxToken])) {
      for (;;) {
        switch (xxToken) {
        case 15:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          if (xxToken != 1) {
            xxRecoveryTerminal(1, 25, xxGlobalRecoverySet, &Ident1);
          } else {
            Ident1 = Scanner_Attribute;
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          Designator0->U_1.V_0.Tree = Tree_mSelect(Ident1.Position, Designator0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident);
          goto EXIT_33;
          break;
        case 27:;
          LBracket1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 73;
          yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
          Designator0->U_1.V_0.Tree = Tree_mSubscript(LBracket1.Position, Designator0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
          for (;;) {
            if (xxToken == 13) {
              Comma1 = Scanner_Attribute;
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
              xxUnion.LocalRecoverySet = 42;
              yyExpr(&Expr2, (xxtUnionPtr)ADR(xxUnion));
              Designator0->U_1.V_0.Tree = Tree_mSubscript(Comma1.Position, Designator0->U_1.V_0.Tree, Expr2.U_1.V_0.Tree);
            } else if (xxToken == 28 || xxIsRepairMode) {
              goto EXIT_34;
            } else {
              xxExpected(73, 73, xxGlobalRecoverySet);
            }
          } EXIT_34:;
          if (xxToken != 28) {
            xxRecoveryLiteral(28, 42, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          goto EXIT_33;
          break;
        case 29:;
          Arrow1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          Designator0->U_1.V_0.Tree = Tree_mDeref(Arrow1.Position, Designator0->U_1.V_0.Tree);
          goto EXIT_33;
          break;
        default :
          if (xxIsRepairMode) {
            if (xxToken != 29) {
              xxRecoveryTerminal(29, 75, xxGlobalRecoverySet, &Arrow1);
            } else {
              Arrow1 = Scanner_Attribute;
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            Designator0->U_1.V_0.Tree = Tree_mDeref(Arrow1.Position, Designator0->U_1.V_0.Tree);
            goto EXIT_33;
          }
          xxExpected(71, 71, xxGlobalRecoverySet);
          break;
        }
      } EXIT_33:;
    } else if (IN(16, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_32;
    } else {
      xxExpected(71, 71, xxGlobalRecoverySet);
    }
  } EXIT_32:;
}

static void yyExpr
# ifdef __STDC__
(Parser_tParsAttribute *Expr0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Expr0, xxGlobalRecoverySet)
Parser_tParsAttribute *Expr0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute SimpleExpr1, SimpleExpr2;
  Parser_tParsAttribute RelOpr1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 76;
  yySimpleExpr(&SimpleExpr1, (xxtUnionPtr)ADR(xxUnion));
  Expr0->U_1.V_0.Tree = SimpleExpr1.U_1.V_0.Tree;
  for (;;) {
    if (IN(17, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 77;
      yyRelOpr(&RelOpr1, (xxtUnionPtr)ADR(xxUnion));
      xxUnion.LocalRecoverySet = 0;
      yySimpleExpr(&SimpleExpr2, (xxtUnionPtr)ADR(xxUnion));
      Expr0->U_1.V_0.Tree = Tree_mBinary(RelOpr1.U_1.V_1.Operator, Expr0->U_1.V_0.Tree, SimpleExpr2.U_1.V_0.Tree);
      goto EXIT_35;
    } else if (IN(18, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_35;
    }
    xxExpected(76, 76, xxGlobalRecoverySet);
  } EXIT_35:;
}

static void yyRelOpr
# ifdef __STDC__
(Parser_tParsAttribute *RelOpr0, xxtUnionPtr xxGlobalRecoverySet)
# else
(RelOpr0, xxGlobalRecoverySet)
Parser_tParsAttribute *RelOpr0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  for (;;) {
    switch (xxToken) {
    case 24:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      RelOpr0->U_1.V_1.Operator = Tree_Equal;
      goto EXIT_36;
      break;
    case 8:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      RelOpr0->U_1.V_1.Operator = Tree_NotEqual;
      goto EXIT_36;
      break;
    case 21:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      RelOpr0->U_1.V_1.Operator = Tree_Less;
      goto EXIT_36;
      break;
    case 22:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      RelOpr0->U_1.V_1.Operator = Tree_LessEqual;
      goto EXIT_36;
      break;
    case 25:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      RelOpr0->U_1.V_1.Operator = Tree_Greater;
      goto EXIT_36;
      break;
    case 26:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      RelOpr0->U_1.V_1.Operator = Tree_GreaterEqual;
      goto EXIT_36;
      break;
    case 53:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      RelOpr0->U_1.V_1.Operator = Tree_In;
      goto EXIT_36;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 24) {
          xxRecoveryLiteral(24, 78, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        RelOpr0->U_1.V_1.Operator = Tree_Equal;
        goto EXIT_36;
      }
      xxExpected(76, 76, xxGlobalRecoverySet);
      break;
    }
  } EXIT_36:;
}

static void yySimpleExpr
# ifdef __STDC__
(Parser_tParsAttribute *SimpleExpr0, xxtUnionPtr xxGlobalRecoverySet)
# else
(SimpleExpr0, xxGlobalRecoverySet)
Parser_tParsAttribute *SimpleExpr0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Term1, Term2, Term3;
  Parser_tParsAttribute AddOpr1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 14:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 86;
      yyTerm(&Term1, (xxtUnionPtr)ADR(xxUnion));
      SimpleExpr0->U_1.V_0.Tree = Tree_mUnary(Tree_Minus, Term1.U_1.V_0.Tree);
      goto EXIT_37;
      break;
    case 1:;
    case 2:;
    case 3:;
    case 4:;
    case 5:;
    case 6:;
    case 7:;
    case 9:;
    case 12:;
    case 30:;
    case 57:;
      for (;;) {
        if (xxToken == 12) {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          goto EXIT_38;
        } else if (IN(19, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_38;
        }
        xxExpected(87, 85, xxGlobalRecoverySet);
      } EXIT_38:;
      xxUnion.LocalRecoverySet = 86;
      yyTerm(&Term2, (xxtUnionPtr)ADR(xxUnion));
      SimpleExpr0->U_1.V_0.Tree = Term2.U_1.V_0.Tree;
      goto EXIT_37;
      break;
    default :
      if (xxIsRepairMode) {
        for (;;) {
          if (xxToken == 12) {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
            goto EXIT_39;
          } else if (IN(19, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_39;
          }
          xxExpected(87, 85, xxGlobalRecoverySet);
        } EXIT_39:;
        xxUnion.LocalRecoverySet = 86;
        yyTerm(&Term2, (xxtUnionPtr)ADR(xxUnion));
        SimpleExpr0->U_1.V_0.Tree = Term2.U_1.V_0.Tree;
        goto EXIT_37;
      }
      xxExpected(85, 85, xxGlobalRecoverySet);
      break;
    }
  } EXIT_37:;
  for (;;) {
    if (IN(20, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 88;
      yyAddOpr(&AddOpr1, (xxtUnionPtr)ADR(xxUnion));
      xxUnion.LocalRecoverySet = 0;
      yyTerm(&Term3, (xxtUnionPtr)ADR(xxUnion));
      SimpleExpr0->U_1.V_0.Tree = Tree_mBinary(AddOpr1.U_1.V_1.Operator, SimpleExpr0->U_1.V_0.Tree, Term3.U_1.V_0.Tree);
    } else if (IN(21, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_40;
    } else {
      xxExpected(86, 86, xxGlobalRecoverySet);
    }
  } EXIT_40:;
}

static void yyAddOpr
# ifdef __STDC__
(Parser_tParsAttribute *AddOpr0, xxtUnionPtr xxGlobalRecoverySet)
# else
(AddOpr0, xxGlobalRecoverySet)
Parser_tParsAttribute *AddOpr0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  for (;;) {
    switch (xxToken) {
    case 12:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      AddOpr0->U_1.V_1.Operator = Tree_Plus;
      goto EXIT_41;
      break;
    case 14:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      AddOpr0->U_1.V_1.Operator = Tree_Minus;
      goto EXIT_41;
      break;
    case 59:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      AddOpr0->U_1.V_1.Operator = Tree_Or;
      goto EXIT_41;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 12) {
          xxRecoveryLiteral(12, 89, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        AddOpr0->U_1.V_1.Operator = Tree_Plus;
        goto EXIT_41;
      }
      xxExpected(86, 86, xxGlobalRecoverySet);
      break;
    }
  } EXIT_41:;
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
  Parser_tParsAttribute MulOpr1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 92;
  yyFactor(&Factor1, (xxtUnionPtr)ADR(xxUnion));
  Term0->U_1.V_0.Tree = Factor1.U_1.V_0.Tree;
  for (;;) {
    if (IN(22, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 88;
      yyMulOpr(&MulOpr1, (xxtUnionPtr)ADR(xxUnion));
      xxUnion.LocalRecoverySet = 0;
      yyFactor(&Factor2, (xxtUnionPtr)ADR(xxUnion));
      Term0->U_1.V_0.Tree = Tree_mBinary(MulOpr1.U_1.V_1.Operator, Term0->U_1.V_0.Tree, Factor2.U_1.V_0.Tree);
    } else if (IN(23, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_42;
    } else {
      xxExpected(92, 92, xxGlobalRecoverySet);
    }
  } EXIT_42:;
}

static void yyMulOpr
# ifdef __STDC__
(Parser_tParsAttribute *MulOpr0, xxtUnionPtr xxGlobalRecoverySet)
# else
(MulOpr0, xxGlobalRecoverySet)
Parser_tParsAttribute *MulOpr0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  for (;;) {
    switch (xxToken) {
    case 11:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      MulOpr0->U_1.V_1.Operator = Tree_Times;
      goto EXIT_43;
      break;
    case 17:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      MulOpr0->U_1.V_1.Operator = Tree_Divide;
      goto EXIT_43;
      break;
    case 41:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      MulOpr0->U_1.V_1.Operator = Tree_Div;
      goto EXIT_43;
      break;
    case 55:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      MulOpr0->U_1.V_1.Operator = Tree_Mod;
      goto EXIT_43;
      break;
    case 34:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      MulOpr0->U_1.V_1.Operator = Tree_And;
      goto EXIT_43;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 11) {
          xxRecoveryLiteral(11, 93, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        MulOpr0->U_1.V_1.Operator = Tree_Times;
        goto EXIT_43;
      }
      xxExpected(92, 92, xxGlobalRecoverySet);
      break;
    }
  } EXIT_43:;
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
  Scanner_tScanAttribute DecConst1;
  Scanner_tScanAttribute OctalConst1;
  Scanner_tScanAttribute HexConst1;
  Scanner_tScanAttribute CharConst1;
  Scanner_tScanAttribute RealConst1;
  Scanner_tScanAttribute StringConst1;
  Parser_tParsAttribute Expr1;
  Parser_tParsAttribute Designator1;
  Parser_tParsAttribute Factor1;
  Parser_tParsAttribute Elems1;
  Parser_tParsAttribute Actuals1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 2:;
      DecConst1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      Factor0->U_1.V_0.Tree = Tree_mIntConst(Tree_Decimal, DecConst1.U_1.V_2.IntValue, DecConst1.Position);
      goto EXIT_44;
      break;
    case 3:;
      OctalConst1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      Factor0->U_1.V_0.Tree = Tree_mIntConst(Tree_Octal, OctalConst1.U_1.V_2.IntValue, OctalConst1.Position);
      goto EXIT_44;
      break;
    case 4:;
      HexConst1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      Factor0->U_1.V_0.Tree = Tree_mIntConst(Tree_Hexadecimal, HexConst1.U_1.V_2.IntValue, HexConst1.Position);
      goto EXIT_44;
      break;
    case 5:;
      CharConst1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      Factor0->U_1.V_0.Tree = Tree_mCharConst(CharConst1.U_1.V_3.CharValue);
      goto EXIT_44;
      break;
    case 6:;
      RealConst1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      Factor0->U_1.V_0.Tree = Tree_mRealConst(RealConst1.U_1.V_4.RealValue);
      goto EXIT_44;
      break;
    case 7:;
      StringConst1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      Factor0->U_1.V_0.Tree = Tree_mStringConst(StringConst1.U_1.V_5.StringValue);
      goto EXIT_44;
      break;
    case 30:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 105;
      yyElems(&Elems1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 32) {
        xxRecoveryLiteral(32, 105, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Factor0->U_1.V_0.Tree = Tree_mBitSet(Elems1.U_1.V_0.Tree);
      goto EXIT_44;
      break;
    case 1:;
      xxUnion.LocalRecoverySet = 106;
      yyDesignator(&Designator1, (xxtUnionPtr)ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 9:;
          xxUnion.LocalRecoverySet = 0;
          yyActuals(&Actuals1, (xxtUnionPtr)ADR(xxUnion));
          Factor0->U_1.V_0.Tree = Tree_mFuncCall(Designator1.U_1.V_0.Tree, Actuals1.U_1.V_0.Tree);
          goto EXIT_45;
          break;
        case 30:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 105;
          yyElems(&Elems1, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 32) {
            xxRecoveryLiteral(32, 105, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          if (Designator1.U_1.V_0.Tree->U_1.V_1.Kind != Tree_Qualid0 && Designator1.U_1.V_0.Tree->U_1.V_1.Kind != Tree_Qualid1) {
            Factor0->U_1.V_0.Tree = Tree_mBitSet(Elems1.U_1.V_0.Tree);
          } else {
            Factor0->U_1.V_0.Tree = Tree_mSet(Designator1.U_1.V_0.Tree, Elems1.U_1.V_0.Tree);
          }
          goto EXIT_45;
          break;
        case 8:;
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
          Factor0->U_1.V_0.Tree = Designator1.U_1.V_0.Tree;
          goto EXIT_45;
          break;
        default :
          if (xxIsRepairMode) {
            Factor0->U_1.V_0.Tree = Designator1.U_1.V_0.Tree;
            goto EXIT_45;
          }
          xxUnexpected(106, xxGlobalRecoverySet);
          break;
        }
      } EXIT_45:;
      goto EXIT_44;
      break;
    case 9:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 44;
      yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 10) {
        xxRecoveryLiteral(10, 44, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Factor0->U_1.V_0.Tree = Expr1.U_1.V_0.Tree;
      goto EXIT_44;
      break;
    case 57:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyFactor(&Factor1, (xxtUnionPtr)ADR(xxUnion));
      Factor0->U_1.V_0.Tree = Tree_mUnary(Tree_Not, Factor1.U_1.V_0.Tree);
      goto EXIT_44;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 2) {
          xxRecoveryTerminal(2, 98, xxGlobalRecoverySet, &DecConst1);
        } else {
          DecConst1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        Factor0->U_1.V_0.Tree = Tree_mIntConst(Tree_Decimal, DecConst1.U_1.V_2.IntValue, DecConst1.Position);
        goto EXIT_44;
      }
      xxExpected(88, 88, xxGlobalRecoverySet);
      break;
    }
  } EXIT_44:;
}

static void yyElems
# ifdef __STDC__
(Parser_tParsAttribute *Elems0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Elems0, xxGlobalRecoverySet)
Parser_tParsAttribute *Elems0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Comma1;
  Parser_tParsAttribute Elem1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Elem1.U_1.V_0.Tree = Tree_mElems0();
  for (;;) {
    if (IN(10, xxVerticalSet0.A[xxToken])) {
      for (;;) {
        xxUnion.LocalRecoverySet = 48;
        yyElem(&Elem1, (xxtUnionPtr)ADR(xxUnion));
        if (!(xxToken == 13)) {
          if (xxToken == 32) {
            goto EXIT_47;
          }
          xxExpected(48, 62, xxGlobalRecoverySet);
          if (!(xxToken == 13 || IN(10, xxVerticalSet0.A[xxToken]))) {
            goto EXIT_47;
          }
        }
        if (xxToken != 13) {
          xxRecoveryTerminal(13, 62, xxGlobalRecoverySet, &Comma1);
        } else {
          Comma1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_47:;
      goto EXIT_46;
    } else if (xxToken == 32 || xxIsRepairMode) {
      goto EXIT_46;
    }
    xxExpected(77, 77, xxGlobalRecoverySet);
  } EXIT_46:;
  Elems0->U_1.V_0.Tree = Tree_ReverseTree(Elem1.U_1.V_0.Tree);
}

static void yyElem
# ifdef __STDC__
(Parser_tParsAttribute *Elem0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Elem0, xxGlobalRecoverySet)
Parser_tParsAttribute *Elem0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Expr1, Expr2;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  xxUnion.LocalRecoverySet = 63;
  yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
  for (;;) {
    switch (xxToken) {
    case 16:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 0;
      yyExpr(&Expr2, (xxtUnionPtr)ADR(xxUnion));
      Elem0->U_1.V_0.Tree = Tree_mElemRange(Elem0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Expr2.U_1.V_0.Tree);
      goto EXIT_48;
      break;
    case 13:;
    case 32:;
      Elem0->U_1.V_0.Tree = Tree_mElem(Elem0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
      goto EXIT_48;
      break;
    default :
      if (xxIsRepairMode) {
        Elem0->U_1.V_0.Tree = Tree_mElem(Elem0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
        goto EXIT_48;
      }
      xxUnexpected(63, xxGlobalRecoverySet);
      break;
    }
  } EXIT_48:;
}

static void yyActuals
# ifdef __STDC__
(Parser_tParsAttribute *Actuals0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Actuals0, xxGlobalRecoverySet)
Parser_tParsAttribute *Actuals0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Comma1;
  Parser_tParsAttribute Expr1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Actuals0->U_1.V_0.Tree = Tree_mActuals0();
  if (xxToken != 9) {
    xxRecoveryLiteral(9, 107, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    if (IN(10, xxVerticalSet0.A[xxToken])) {
      for (;;) {
        xxUnion.LocalRecoverySet = 66;
        yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
        Actuals0->U_1.V_0.Tree = Tree_mActual(Expr1.U_1.V_0.Tree, Actuals0->U_1.V_0.Tree);
        if (!(xxToken == 13)) {
          if (xxToken == 10) {
            goto EXIT_50;
          }
          xxExpected(66, 108, xxGlobalRecoverySet);
          if (!(xxToken == 13 || IN(10, xxVerticalSet0.A[xxToken]))) {
            goto EXIT_50;
          }
        }
        if (xxToken != 13) {
          xxRecoveryTerminal(13, 108, xxGlobalRecoverySet, &Comma1);
        } else {
          Comma1 = Scanner_Attribute;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_50:;
      goto EXIT_49;
    } else if (xxToken == 10 || xxIsRepairMode) {
      goto EXIT_49;
    }
    xxExpected(107, 107, xxGlobalRecoverySet);
  } EXIT_49:;
  if (xxToken != 10) {
    xxRecoveryLiteral(10, 44, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  Actuals0->U_1.V_0.Tree = Tree_ReverseTree(Actuals0->U_1.V_0.Tree);
}

static void yyStmt
# ifdef __STDC__
(Parser_tParsAttribute *Stmt0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Stmt0, xxGlobalRecoverySet)
Parser_tParsAttribute *Stmt0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute Expr1, Expr2, Expr3;
  Parser_tParsAttribute Designator1;
  Parser_tParsAttribute Actuals1;
  Parser_tParsAttribute Stmts1, Stmts2;
  Parser_tParsAttribute Elsifs1;
  Parser_tParsAttribute Cases1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 1:;
      xxUnion.LocalRecoverySet = 109;
      yyDesignator(&Designator1, (xxtUnionPtr)ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 19:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 0;
          yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
          Stmt0->U_1.V_0.Tree = Tree_mAssign(Stmt0->U_1.V_0.Tree, Designator1.U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
          goto EXIT_52;
          break;
        case 9:;
        case 20:;
        case 31:;
        case 43:;
        case 44:;
        case 45:;
        case 70:;
          if (xxToken == 9) {
            xxUnion.LocalRecoverySet = 0;
            yyActuals(&Actuals1, (xxtUnionPtr)ADR(xxUnion));
          } else {
            Actuals1.U_1.V_0.Tree = Tree_mActuals0();
          }
          Stmt0->U_1.V_0.Tree = Tree_mCall(Stmt0->U_1.V_0.Tree, Designator1.U_1.V_0.Tree, Actuals1.U_1.V_0.Tree);
          goto EXIT_52;
          break;
        default :
          if (xxIsRepairMode) {
            if (xxToken == 9) {
              xxUnion.LocalRecoverySet = 0;
              yyActuals(&Actuals1, (xxtUnionPtr)ADR(xxUnion));
            } else {
              Actuals1.U_1.V_0.Tree = Tree_mActuals0();
            }
            Stmt0->U_1.V_0.Tree = Tree_mCall(Stmt0->U_1.V_0.Tree, Designator1.U_1.V_0.Tree, Actuals1.U_1.V_0.Tree);
            goto EXIT_52;
          }
          xxUnexpected(109, xxGlobalRecoverySet);
          break;
        }
      } EXIT_52:;
      goto EXIT_51;
      break;
    case 50:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 112;
      yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 67) {
        xxRecoveryLiteral(67, 112, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 113;
      yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
      xxUnion.LocalRecoverySet = 55;
      yyElsifs(&Elsifs1, (xxtUnionPtr)ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 43:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 31;
          yyStmts(&Stmts2, (xxtUnionPtr)ADR(xxUnion));
          goto EXIT_53;
          break;
        case 45:;
          Stmts2.U_1.V_0.Tree = Tree_mStmts0();
          goto EXIT_53;
          break;
        default :
          if (xxIsRepairMode) {
            Stmts2.U_1.V_0.Tree = Tree_mStmts0();
            goto EXIT_53;
          }
          xxExpected(55, 55, xxGlobalRecoverySet);
          break;
        }
      } EXIT_53:;
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Stmt0->U_1.V_0.Tree = Tree_mIf(Stmt0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Stmts1.U_1.V_0.Tree, Elsifs1.U_1.V_0.Tree, Stmts2.U_1.V_0.Tree);
      goto EXIT_51;
      break;
    case 38:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 54;
      yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 58) {
        xxRecoveryLiteral(58, 54, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 55;
      yyCases(&Cases1, (xxtUnionPtr)ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 43:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 31;
          yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 45) {
            xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
          Stmt0->U_1.V_0.Tree = Tree_mCase(Stmt0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Cases1.U_1.V_0.Tree, Stmts1.U_1.V_0.Tree, TRUE);
          goto EXIT_54;
          break;
        case 45:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          Stmt0->U_1.V_0.Tree = Tree_mCase(Stmt0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Cases1.U_1.V_0.Tree, Tree_mStmts0(), FALSE);
          goto EXIT_54;
          break;
        default :
          if (xxIsRepairMode) {
            if (xxToken != 45) {
              xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            Stmt0->U_1.V_0.Tree = Tree_mCase(Stmt0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Cases1.U_1.V_0.Tree, Tree_mStmts0(), FALSE);
            goto EXIT_54;
          }
          xxExpected(55, 55, xxGlobalRecoverySet);
          break;
        }
      } EXIT_54:;
      goto EXIT_51;
      break;
    case 72:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 117;
      yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 42) {
        xxRecoveryLiteral(42, 117, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 31;
      yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Stmt0->U_1.V_0.Tree = Tree_mWhile(Stmt0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree, Stmts1.U_1.V_0.Tree);
      goto EXIT_51;
      break;
    case 64:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 119;
      yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 70) {
        xxRecoveryLiteral(70, 119, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
      Stmt0->U_1.V_0.Tree = Tree_mRepeat(Stmt0->U_1.V_0.Tree, Stmts1.U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
      goto EXIT_51;
      break;
    case 54:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 31;
      yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Stmt0->U_1.V_0.Tree = Tree_mLoop(Stmt0->U_1.V_0.Tree, Stmts1.U_1.V_0.Tree);
      goto EXIT_51;
      break;
    case 48:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 121, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 19) {
        xxRecoveryLiteral(19, 121, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 122;
      yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 68) {
        xxRecoveryLiteral(68, 122, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 123;
      yyExpr(&Expr2, (xxtUnionPtr)ADR(xxUnion));
      for (;;) {
        switch (xxToken) {
        case 37:;
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
          xxUnion.LocalRecoverySet = 117;
          yyExpr(&Expr3, (xxtUnionPtr)ADR(xxUnion));
          goto EXIT_55;
          break;
        case 42:;
          Expr3.U_1.V_0.Tree = Tree_mIntConst(Tree_Decimal, 1L, Positions_NoPosition);
          goto EXIT_55;
          break;
        default :
          if (xxIsRepairMode) {
            Expr3.U_1.V_0.Tree = Tree_mIntConst(Tree_Decimal, 1L, Positions_NoPosition);
            goto EXIT_55;
          }
          xxExpected(125, 123, xxGlobalRecoverySet);
          break;
        }
      } EXIT_55:;
      if (xxToken != 42) {
        xxRecoveryLiteral(42, 117, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 31;
      yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Stmt0->U_1.V_0.Tree = Tree_mFor(Stmt0->U_1.V_0.Tree, Tree_mQualid0(Ident1.Position, Ident1.U_1.V_1.Ident), Expr1.U_1.V_0.Tree, Expr2.U_1.V_0.Tree, Expr3.U_1.V_0.Tree, Stmts1.U_1.V_0.Tree);
      goto EXIT_51;
      break;
    case 73:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 117;
      yyDesignator(&Designator1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 42) {
        xxRecoveryLiteral(42, 117, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 31;
      yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 45) {
        xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Stmt0->U_1.V_0.Tree = Tree_mWith(Stmt0->U_1.V_0.Tree, Designator1.U_1.V_0.Tree, Stmts1.U_1.V_0.Tree);
      goto EXIT_51;
      break;
    case 46:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      Stmt0->U_1.V_0.Tree = Tree_mExit(Stmt0->U_1.V_0.Tree);
      goto EXIT_51;
      break;
    case 65:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        switch (xxToken) {
        case 1:;
        case 2:;
        case 3:;
        case 4:;
        case 5:;
        case 6:;
        case 7:;
        case 9:;
        case 12:;
        case 14:;
        case 30:;
        case 57:;
          xxUnion.LocalRecoverySet = 0;
          yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
          Stmt0->U_1.V_0.Tree = Tree_mReturn2(Stmt0->U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
          goto EXIT_56;
          break;
        case 20:;
        case 31:;
        case 43:;
        case 44:;
        case 45:;
        case 70:;
          Stmt0->U_1.V_0.Tree = Tree_mReturn1(Stmt0->U_1.V_0.Tree);
          goto EXIT_56;
          break;
        default :
          if (xxIsRepairMode) {
            Stmt0->U_1.V_0.Tree = Tree_mReturn1(Stmt0->U_1.V_0.Tree);
            goto EXIT_56;
          }
          xxUnexpected(77, xxGlobalRecoverySet);
          break;
        }
      } EXIT_56:;
      goto EXIT_51;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 109;
        yyDesignator(&Designator1, (xxtUnionPtr)ADR(xxUnion));
        for (;;) {
          switch (xxToken) {
          case 19:;
            if (xxToken != 19) {
              xxRecoveryLiteral(19, 110, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
            xxUnion.LocalRecoverySet = 0;
            yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
            Stmt0->U_1.V_0.Tree = Tree_mAssign(Stmt0->U_1.V_0.Tree, Designator1.U_1.V_0.Tree, Expr1.U_1.V_0.Tree);
            goto EXIT_57;
            break;
          case 9:;
          case 20:;
          case 31:;
          case 43:;
          case 44:;
          case 45:;
          case 70:;
            if (xxToken == 9) {
              xxUnion.LocalRecoverySet = 0;
              yyActuals(&Actuals1, (xxtUnionPtr)ADR(xxUnion));
            } else {
              Actuals1.U_1.V_0.Tree = Tree_mActuals0();
            }
            Stmt0->U_1.V_0.Tree = Tree_mCall(Stmt0->U_1.V_0.Tree, Designator1.U_1.V_0.Tree, Actuals1.U_1.V_0.Tree);
            goto EXIT_57;
            break;
          default :
            if (xxIsRepairMode) {
              if (xxToken == 9) {
                xxUnion.LocalRecoverySet = 0;
                yyActuals(&Actuals1, (xxtUnionPtr)ADR(xxUnion));
              } else {
                Actuals1.U_1.V_0.Tree = Tree_mActuals0();
              }
              Stmt0->U_1.V_0.Tree = Tree_mCall(Stmt0->U_1.V_0.Tree, Designator1.U_1.V_0.Tree, Actuals1.U_1.V_0.Tree);
              goto EXIT_57;
            }
            xxUnexpected(109, xxGlobalRecoverySet);
            break;
          }
        } EXIT_57:;
        goto EXIT_51;
      }
      xxExpected(128, 128, xxGlobalRecoverySet);
      break;
    }
  } EXIT_51:;
}

static void yyElsifs
# ifdef __STDC__
(Parser_tParsAttribute *Elsifs0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Elsifs0, xxGlobalRecoverySet)
Parser_tParsAttribute *Elsifs0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Expr1;
  Parser_tParsAttribute Stmts1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Elsifs0->U_1.V_0.Tree = Tree_mElsifs0();
  for (;;) {
    if (xxToken == 44) {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 131;
      yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 67) {
        xxRecoveryLiteral(67, 131, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
      Elsifs0->U_1.V_0.Tree = Tree_mElsifs1(Expr1.U_1.V_0.Tree, Stmts1.U_1.V_0.Tree, Elsifs0->U_1.V_0.Tree);
    } else if (IN(9, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_58;
    } else {
      xxExpected(129, 129, xxGlobalRecoverySet);
    }
  } EXIT_58:;
  Elsifs0->U_1.V_0.Tree = Tree_ReverseTree(Elsifs0->U_1.V_0.Tree);
}

static void yyCases
# ifdef __STDC__
(Parser_tParsAttribute *Cases0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Cases0, xxGlobalRecoverySet)
Parser_tParsAttribute *Cases0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Labels1;
  Parser_tParsAttribute Stmts1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Cases0->U_1.V_0.Tree = Tree_mCases0();
  for (;;) {
    for (;;) {
      if (IN(10, xxVerticalSet0.A[xxToken])) {
        xxUnion.LocalRecoverySet = 132;
        yyLabels(&Labels1, (xxtUnionPtr)ADR(xxUnion));
        if (xxToken != 18) {
          xxRecoveryLiteral(18, 132, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        xxUnion.LocalRecoverySet = 59;
        yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
        Cases0->U_1.V_0.Tree = Tree_mCases1(Labels1.U_1.V_0.Tree, Stmts1.U_1.V_0.Tree, Cases0->U_1.V_0.Tree);
        goto EXIT_60;
      } else if (IN(7, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
        goto EXIT_60;
      }
      xxExpected(60, 60, xxGlobalRecoverySet);
    } EXIT_60:;
    if (!(xxToken == 31)) {
      if (IN(9, xxVerticalSet0.A[xxToken])) {
        goto EXIT_59;
      }
      xxExpected(59, 60, xxGlobalRecoverySet);
      if (!(xxToken == 31 || IN(8, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_59;
      }
    }
    if (xxToken != 31) {
      xxRecoveryLiteral(31, 60, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_59:;
  Cases0->U_1.V_0.Tree = Tree_ReverseTree(Cases0->U_1.V_0.Tree);
}

static void yyStmts
# ifdef __STDC__
(Parser_tParsAttribute *Stmts0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Stmts0, xxGlobalRecoverySet)
Parser_tParsAttribute *Stmts0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute Stmt1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Stmt1.U_1.V_0.Tree = Tree_mStmts0();
  for (;;) {
    for (;;) {
      if (IN(27, xxVerticalSet0.A[xxToken])) {
        xxUnion.LocalRecoverySet = 50;
        yyStmt(&Stmt1, (xxtUnionPtr)ADR(xxUnion));
        goto EXIT_62;
      } else if (IN(24, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
        goto EXIT_62;
      }
      xxExpected(133, 133, xxGlobalRecoverySet);
    } EXIT_62:;
    if (!(xxToken == 20)) {
      if (IN(26, xxVerticalSet0.A[xxToken])) {
        goto EXIT_61;
      }
      xxExpected(50, 133, xxGlobalRecoverySet);
      if (!(xxToken == 20 || IN(25, xxVerticalSet0.A[xxToken]))) {
        goto EXIT_61;
      }
    }
    if (xxToken != 20) {
      xxRecoveryLiteral(20, 133, xxGlobalRecoverySet);
    } else {
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_61:;
  Stmts0->U_1.V_0.Tree = Tree_ReverseTree(Stmt1.U_1.V_0.Tree);
}

static void yyProcDecl
# ifdef __STDC__
(Parser_tParsAttribute *ProcDecl0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ProcDecl0, xxGlobalRecoverySet)
Parser_tParsAttribute *ProcDecl0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1, Ident2;
  Parser_tParsAttribute Block1;
  Parser_tParsAttribute ResultType1;
  Parser_tParsAttribute Formals1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 61) {
    xxRecoveryLiteral(61, 134, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 134, xxGlobalRecoverySet, &Ident1);
  } else {
    Ident1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    switch (xxToken) {
    case 9:;
      xxUnion.LocalRecoverySet = 135;
      yyFormals(&Formals1, (xxtUnionPtr)ADR(xxUnion));
      xxUnion.LocalRecoverySet = 136;
      yyResultType(&ResultType1, (xxtUnionPtr)ADR(xxUnion));
      goto EXIT_63;
      break;
    case 20:;
      Formals1.U_1.V_0.Tree = Tree_mFormals0();
      ResultType1.U_1.V_0.Tree = Tree_mVoid();
      goto EXIT_63;
      break;
    default :
      if (xxIsRepairMode) {
        Formals1.U_1.V_0.Tree = Tree_mFormals0();
        ResultType1.U_1.V_0.Tree = Tree_mVoid();
        goto EXIT_63;
      }
      xxExpected(137, 134, xxGlobalRecoverySet);
      break;
    }
  } EXIT_63:;
  if (xxToken != 20) {
    xxRecoveryLiteral(20, 136, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 25;
  yyBlock(&Block1, (xxtUnionPtr)ADR(xxUnion));
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 25, xxGlobalRecoverySet, &Ident2);
  } else {
    Ident2 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  ProcDecl0->U_1.V_0.Tree = Tree_mProc(ProcDecl0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident, Formals1.U_1.V_0.Tree, ResultType1.U_1.V_0.Tree, Block1.U_1.V_2.Decls, Block1.U_1.V_2.Stmts);
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
  Parser_tParsAttribute Stmts1;
  Parser_tParsAttribute Decl1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  Decl1.U_1.V_0.Tree = Tree_mDecls0();
  for (;;) {
    if (IN(28, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 139;
      yyDecl(&Decl1, (xxtUnionPtr)ADR(xxUnion));
    } else if (IN(29, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_64;
    } else {
      xxExpected(138, 138, xxGlobalRecoverySet);
    }
  } EXIT_64:;
  Decl1.U_1.V_0.Tree = Tree_ReverseTree(Decl1.U_1.V_0.Tree);
  for (;;) {
    switch (xxToken) {
    case 36:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 31;
      yyStmts(&Stmts1, (xxtUnionPtr)ADR(xxUnion));
      goto EXIT_65;
      break;
    case 45:;
      Stmts1.U_1.V_0.Tree = Tree_mStmts0();
      goto EXIT_65;
      break;
    default :
      if (xxIsRepairMode) {
        Stmts1.U_1.V_0.Tree = Tree_mStmts0();
        goto EXIT_65;
      }
      xxExpected(139, 139, xxGlobalRecoverySet);
      break;
    }
  } EXIT_65:;
  if (xxToken != 45) {
    xxRecoveryLiteral(45, 31, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  Block0->U_1.V_2.Decls = Decl1.U_1.V_0.Tree;
  Block0->U_1.V_2.Stmts = Stmts1.U_1.V_0.Tree;
}

static void yyDecl
# ifdef __STDC__
(Parser_tParsAttribute *Decl0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Decl0, xxGlobalRecoverySet)
Parser_tParsAttribute *Decl0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute ConstDecl1;
  Parser_tParsAttribute TypeDecl1;
  Parser_tParsAttribute VarDecl1;
  Parser_tParsAttribute ProcDecl1;
  Parser_tParsAttribute ModDecl1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 39:;
      ConstDecl1.U_1.V_0.Tree = Decl0->U_1.V_0.Tree;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 50;
          yyConstDecl(&ConstDecl1, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_67;
        } else {
          xxExpected(25, 25, xxGlobalRecoverySet);
        }
      } EXIT_67:;
      Decl0->U_1.V_0.Tree = ConstDecl1.U_1.V_0.Tree;
      goto EXIT_66;
      break;
    case 69:;
      TypeDecl1.U_1.V_0.Tree = Decl0->U_1.V_0.Tree;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 50;
          yyTypeDecl(&TypeDecl1, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_68;
        } else {
          xxExpected(25, 25, xxGlobalRecoverySet);
        }
      } EXIT_68:;
      Decl0->U_1.V_0.Tree = TypeDecl1.U_1.V_0.Tree;
      goto EXIT_66;
      break;
    case 71:;
      VarDecl1.U_1.V_0.Tree = Decl0->U_1.V_0.Tree;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 50;
          yyVarDecl(&VarDecl1, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_69;
        } else {
          xxExpected(25, 25, xxGlobalRecoverySet);
        }
      } EXIT_69:;
      Decl0->U_1.V_0.Tree = VarDecl1.U_1.V_0.Tree;
      goto EXIT_66;
      break;
    case 61:;
      ProcDecl1.U_1.V_0.Tree = Decl0->U_1.V_0.Tree;
      xxUnion.LocalRecoverySet = 50;
      yyProcDecl(&ProcDecl1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Decl0->U_1.V_0.Tree = ProcDecl1.U_1.V_0.Tree;
      goto EXIT_66;
      break;
    case 56:;
      ModDecl1.U_1.V_0.Tree = Decl0->U_1.V_0.Tree;
      xxUnion.LocalRecoverySet = 50;
      yyModDecl(&ModDecl1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Decl0->U_1.V_0.Tree = ModDecl1.U_1.V_0.Tree;
      goto EXIT_66;
      break;
    default :
      if (xxIsRepairMode) {
        ConstDecl1.U_1.V_0.Tree = Decl0->U_1.V_0.Tree;
        if (xxToken != 39) {
          xxRecoveryLiteral(39, 141, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        for (;;) {
          if (xxToken == 1) {
            xxUnion.LocalRecoverySet = 50;
            yyConstDecl(&ConstDecl1, (xxtUnionPtr)ADR(xxUnion));
            if (xxToken != 20) {
              xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
          } else if (IN(3, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_70;
          } else {
            xxExpected(25, 25, xxGlobalRecoverySet);
          }
        } EXIT_70:;
        Decl0->U_1.V_0.Tree = ConstDecl1.U_1.V_0.Tree;
        goto EXIT_66;
      }
      xxExpected(144, 144, xxGlobalRecoverySet);
      break;
    }
  } EXIT_66:;
}

static void yyFormals
# ifdef __STDC__
(Parser_tParsAttribute *Formals0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Formals0, xxGlobalRecoverySet)
Parser_tParsAttribute *Formals0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Parser_tParsAttribute FPSection1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 9) {
    xxRecoveryLiteral(9, 145, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  FPSection1.U_1.V_0.Tree = Tree_mFormals0();
  for (;;) {
    if (IN(30, xxVerticalSet0.A[xxToken])) {
      for (;;) {
        xxUnion.LocalRecoverySet = 147;
        yyFPSection(&FPSection1, (xxtUnionPtr)ADR(xxUnion));
        if (!(xxToken == 20)) {
          if (xxToken == 10) {
            goto EXIT_72;
          }
          xxExpected(147, 148, xxGlobalRecoverySet);
          if (!(xxToken == 20 || IN(30, xxVerticalSet0.A[xxToken]))) {
            goto EXIT_72;
          }
        }
        if (xxToken != 20) {
          xxRecoveryLiteral(20, 148, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
      } EXIT_72:;
      goto EXIT_71;
    } else if (xxToken == 10 || xxIsRepairMode) {
      goto EXIT_71;
    }
    xxExpected(146, 146, xxGlobalRecoverySet);
  } EXIT_71:;
  if (xxToken != 10) {
    xxRecoveryLiteral(10, 44, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  Formals0->U_1.V_0.Tree = Tree_ReverseTree(FPSection1.U_1.V_0.Tree);
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
  Parser_tParsAttribute FormalType1;
  Parser_tParsAttribute ParIds1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 71:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 150;
      yyParIds(&ParIds1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 18) {
        xxRecoveryLiteral(18, 150, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyFormalType(&FormalType1, (xxtUnionPtr)ADR(xxUnion));
      FPSection0->U_1.V_0.Tree = Tree_mFormals1(TRUE, ParIds1.U_1.V_0.Tree, FormalType1.U_1.V_0.Tree, FPSection0->U_1.V_0.Tree);
      goto EXIT_73;
      break;
    case 1:;
      xxUnion.LocalRecoverySet = 150;
      yyParIds(&ParIds1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 18) {
        xxRecoveryLiteral(18, 150, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 0;
      yyFormalType(&FormalType1, (xxtUnionPtr)ADR(xxUnion));
      FPSection0->U_1.V_0.Tree = Tree_mFormals1(FALSE, ParIds1.U_1.V_0.Tree, FormalType1.U_1.V_0.Tree, FPSection0->U_1.V_0.Tree);
      goto EXIT_73;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 150;
        yyParIds(&ParIds1, (xxtUnionPtr)ADR(xxUnion));
        if (xxToken != 18) {
          xxRecoveryLiteral(18, 150, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        xxUnion.LocalRecoverySet = 0;
        yyFormalType(&FormalType1, (xxtUnionPtr)ADR(xxUnion));
        FPSection0->U_1.V_0.Tree = Tree_mFormals1(FALSE, ParIds1.U_1.V_0.Tree, FormalType1.U_1.V_0.Tree, FPSection0->U_1.V_0.Tree);
        goto EXIT_73;
      }
      xxExpected(143, 143, xxGlobalRecoverySet);
      break;
    }
  } EXIT_73:;
}

static void yyParIds
# ifdef __STDC__
(Parser_tParsAttribute *ParIds0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ParIds0, xxGlobalRecoverySet)
Parser_tParsAttribute *ParIds0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Scanner_tScanAttribute Comma1;

  ParIds0->U_1.V_0.Tree = Tree_mParIds0();
  for (;;) {
    if (xxToken != 1) {
      xxRecoveryTerminal(1, 49, xxGlobalRecoverySet, &Ident1);
    } else {
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
    ParIds0->U_1.V_0.Tree = Tree_mParIds1(Ident1.U_1.V_1.Ident, ParIds0->U_1.V_0.Tree);
    if (!(xxToken == 13)) {
      if (xxToken == 18) {
        goto EXIT_74;
      }
      xxExpected(48, 49, xxGlobalRecoverySet);
      if (!(xxToken == 13 || xxToken == 1)) {
        goto EXIT_74;
      }
    }
    if (xxToken != 13) {
      xxRecoveryTerminal(13, 49, xxGlobalRecoverySet, &Comma1);
    } else {
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_74:;
  ParIds0->U_1.V_0.Tree = Tree_ReverseTree(ParIds0->U_1.V_0.Tree);
}

static void yyModDecl
# ifdef __STDC__
(Parser_tParsAttribute *ModDecl0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ModDecl0, xxGlobalRecoverySet)
Parser_tParsAttribute *ModDecl0;
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
      yyPriority(&Priority1, (xxtUnionPtr)ADR(xxUnion));
      goto EXIT_75;
    } else if (xxToken == 20 || xxIsRepairMode) {
      goto EXIT_75;
    }
    xxExpected(19, 151, xxGlobalRecoverySet);
  } EXIT_75:;
  if (xxToken != 20) {
    xxRecoveryLiteral(20, 152, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  Import1.U_1.V_0.Tree = Tree_mImport0();
  for (;;) {
    if (IN(0, xxVerticalSet0.A[xxToken])) {
      xxUnion.LocalRecoverySet = 155;
      yyImport(&Import1, (xxtUnionPtr)ADR(xxUnion));
    } else if (IN(31, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
      goto EXIT_76;
    } else {
      xxExpected(153, 154, xxGlobalRecoverySet);
    }
  } EXIT_76:;
  Import1.U_1.V_0.Tree = Tree_ReverseTree(Import1.U_1.V_0.Tree);
  for (;;) {
    switch (xxToken) {
    case 47:;
      xxUnion.LocalRecoverySet = 156;
      yyExport(&Export1, (xxtUnionPtr)ADR(xxUnion));
      goto EXIT_77;
      break;
    case 36:;
    case 39:;
    case 45:;
    case 56:;
    case 61:;
    case 69:;
    case 71:;
      Export1.U_1.V_0.Tree = Tree_mExport0();
      goto EXIT_77;
      break;
    default :
      if (xxIsRepairMode) {
        Export1.U_1.V_0.Tree = Tree_mExport0();
        goto EXIT_77;
      }
      xxExpected(157, 155, xxGlobalRecoverySet);
      break;
    }
  } EXIT_77:;
  xxUnion.LocalRecoverySet = 25;
  yyBlock(&Block1, (xxtUnionPtr)ADR(xxUnion));
  if (xxToken != 1) {
    xxRecoveryTerminal(1, 25, xxGlobalRecoverySet, &Ident2);
  } else {
    Ident2 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  ModDecl0->U_1.V_0.Tree = Tree_mModule(ModDecl0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident, Import1.U_1.V_0.Tree, Export1.U_1.V_0.Tree, Block1.U_1.V_2.Decls, Block1.U_1.V_2.Stmts);
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
  Scanner_tScanAttribute LBracket1;
  Parser_tParsAttribute Expr1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 27) {
    xxRecoveryTerminal(27, 158, xxGlobalRecoverySet, &LBracket1);
  } else {
    LBracket1 = Scanner_Attribute;
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  xxUnion.LocalRecoverySet = 42;
  yyExpr(&Expr1, (xxtUnionPtr)ADR(xxUnion));
  if (xxToken != 28) {
    xxRecoveryLiteral(28, 42, xxGlobalRecoverySet);
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
  Parser_tParsAttribute ExpIds1, ExpIds2;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  if (xxToken != 47) {
    xxRecoveryLiteral(47, 159, xxGlobalRecoverySet);
  } else {
    xxToken = Scanner_GetToken();
    xxIsRepairMode = FALSE;
  }
  for (;;) {
    switch (xxToken) {
    case 62:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 50;
      yyExpIds(&ExpIds1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Export0->U_1.V_0.Tree = Tree_mExport1(TRUE, ExpIds1.U_1.V_0.Tree);
      goto EXIT_78;
      break;
    case 1:;
      xxUnion.LocalRecoverySet = 50;
      yyExpIds(&ExpIds2, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Export0->U_1.V_0.Tree = Tree_mExport1(FALSE, ExpIds2.U_1.V_0.Tree);
      goto EXIT_78;
      break;
    default :
      if (xxIsRepairMode) {
        xxUnion.LocalRecoverySet = 50;
        yyExpIds(&ExpIds2, (xxtUnionPtr)ADR(xxUnion));
        if (xxToken != 20) {
          xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        Export0->U_1.V_0.Tree = Tree_mExport1(FALSE, ExpIds2.U_1.V_0.Tree);
        goto EXIT_78;
      }
      xxExpected(161, 161, xxGlobalRecoverySet);
      break;
    }
  } EXIT_78:;
}

static void yyExpIds
# ifdef __STDC__
(Parser_tParsAttribute *ExpIds0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ExpIds0, xxGlobalRecoverySet)
Parser_tParsAttribute *ExpIds0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Scanner_tScanAttribute Comma1;

  ExpIds0->U_1.V_0.Tree = Tree_mExpIds0();
  for (;;) {
    if (xxToken != 1) {
      xxRecoveryTerminal(1, 49, xxGlobalRecoverySet, &Ident1);
    } else {
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
    ExpIds0->U_1.V_0.Tree = Tree_mExpIds1(Ident1.U_1.V_1.Ident, ExpIds0->U_1.V_0.Tree);
    if (!(xxToken == 13)) {
      if (xxToken == 20) {
        goto EXIT_79;
      }
      xxExpected(48, 49, xxGlobalRecoverySet);
      if (!(xxToken == 13 || xxToken == 1)) {
        goto EXIT_79;
      }
    }
    if (xxToken != 13) {
      xxRecoveryTerminal(13, 49, xxGlobalRecoverySet, &Comma1);
    } else {
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_79:;
  ExpIds0->U_1.V_0.Tree = Tree_ReverseTree(ExpIds0->U_1.V_0.Tree);
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
  Parser_tParsAttribute ImpIds1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 49:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 163, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      if (xxToken != 52) {
        xxRecoveryLiteral(52, 163, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      xxUnion.LocalRecoverySet = 50;
      yyImpIds(&ImpIds1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Import0->U_1.V_0.Tree = Tree_mFrom(Import0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident, Ident1.Position, ImpIds1.U_1.V_0.Tree);
      goto EXIT_80;
      break;
    case 52:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      xxUnion.LocalRecoverySet = 50;
      yyImpIds(&ImpIds1, (xxtUnionPtr)ADR(xxUnion));
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Import0->U_1.V_0.Tree = Tree_mObjects(Import0->U_1.V_0.Tree, ImpIds1.U_1.V_0.Tree);
      goto EXIT_80;
      break;
    default :
      if (xxIsRepairMode) {
        if (xxToken != 52) {
          xxRecoveryLiteral(52, 163, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        xxUnion.LocalRecoverySet = 50;
        yyImpIds(&ImpIds1, (xxtUnionPtr)ADR(xxUnion));
        if (xxToken != 20) {
          xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        Import0->U_1.V_0.Tree = Tree_mObjects(Import0->U_1.V_0.Tree, ImpIds1.U_1.V_0.Tree);
        goto EXIT_80;
      }
      xxExpected(164, 164, xxGlobalRecoverySet);
      break;
    }
  } EXIT_80:;
}

static void yyImpIds
# ifdef __STDC__
(Parser_tParsAttribute *ImpIds0, xxtUnionPtr xxGlobalRecoverySet)
# else
(ImpIds0, xxGlobalRecoverySet)
Parser_tParsAttribute *ImpIds0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Scanner_tScanAttribute Comma1;

  ImpIds0->U_1.V_0.Tree = Tree_mImpIds0();
  for (;;) {
    if (xxToken != 1) {
      xxRecoveryTerminal(1, 49, xxGlobalRecoverySet, &Ident1);
    } else {
      Ident1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
    ImpIds0->U_1.V_0.Tree = Tree_mImpIds1(Ident1.U_1.V_1.Ident, Ident1.Position, ImpIds0->U_1.V_0.Tree);
    if (!(xxToken == 13)) {
      if (xxToken == 20) {
        goto EXIT_81;
      }
      xxExpected(48, 49, xxGlobalRecoverySet);
      if (!(xxToken == 13 || xxToken == 1)) {
        goto EXIT_81;
      }
    }
    if (xxToken != 13) {
      xxRecoveryTerminal(13, 49, xxGlobalRecoverySet, &Comma1);
    } else {
      Comma1 = Scanner_Attribute;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
    }
  } EXIT_81:;
  ImpIds0->U_1.V_0.Tree = Tree_ReverseTree(ImpIds0->U_1.V_0.Tree);
}

static void yyDef
# ifdef __STDC__
(Parser_tParsAttribute *Def0, xxtUnionPtr xxGlobalRecoverySet)
# else
(Def0, xxGlobalRecoverySet)
Parser_tParsAttribute *Def0;
xxtUnionPtr xxGlobalRecoverySet;
# endif
{
  Scanner_tScanAttribute Ident1;
  Parser_tParsAttribute ConstDecl1;
  Parser_tParsAttribute Type1;
  Parser_tParsAttribute ResultType1;
  Parser_tParsAttribute VarDecl1;
  Parser_tParsAttribute Formals1;
  xxtUnion xxUnion;

  xxUnion.GlobalRecoverySet = xxGlobalRecoverySet;
  for (;;) {
    switch (xxToken) {
    case 39:;
      ConstDecl1.U_1.V_0.Tree = Def0->U_1.V_0.Tree;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 50;
          yyConstDecl(&ConstDecl1, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_83;
        } else {
          xxExpected(25, 25, xxGlobalRecoverySet);
        }
      } EXIT_83:;
      Def0->U_1.V_0.Tree = ConstDecl1.U_1.V_0.Tree;
      goto EXIT_82;
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
            switch (xxToken) {
            case 24:;
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
              xxUnion.LocalRecoverySet = 50;
              yyType(&Type1, (xxtUnionPtr)ADR(xxUnion));
              Def0->U_1.V_0.Tree = Tree_mTypeDecl(Def0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident, Type1.U_1.V_0.Tree, Ident1.Position);
              goto EXIT_85;
              break;
            case 20:;
              Def0->U_1.V_0.Tree = Tree_mOpaque(Def0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident);
              goto EXIT_85;
              break;
            default :
              if (xxIsRepairMode) {
                Def0->U_1.V_0.Tree = Tree_mOpaque(Def0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident);
                goto EXIT_85;
              }
              xxExpected(167, 167, xxGlobalRecoverySet);
              break;
            }
          } EXIT_85:;
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_84;
        } else {
          xxExpected(25, 25, xxGlobalRecoverySet);
        }
      } EXIT_84:;
      goto EXIT_82;
      break;
    case 71:;
      VarDecl1.U_1.V_0.Tree = Def0->U_1.V_0.Tree;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      for (;;) {
        if (xxToken == 1) {
          xxUnion.LocalRecoverySet = 50;
          yyVarDecl(&VarDecl1, (xxtUnionPtr)ADR(xxUnion));
          if (xxToken != 20) {
            xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
          } else {
            xxToken = Scanner_GetToken();
            xxIsRepairMode = FALSE;
          }
        } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
          goto EXIT_86;
        } else {
          xxExpected(25, 25, xxGlobalRecoverySet);
        }
      } EXIT_86:;
      Def0->U_1.V_0.Tree = VarDecl1.U_1.V_0.Tree;
      goto EXIT_82;
      break;
    case 61:;
      xxToken = Scanner_GetToken();
      xxIsRepairMode = FALSE;
      if (xxToken != 1) {
        xxRecoveryTerminal(1, 169, xxGlobalRecoverySet, &Ident1);
      } else {
        Ident1 = Scanner_Attribute;
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      for (;;) {
        switch (xxToken) {
        case 9:;
          xxUnion.LocalRecoverySet = 170;
          yyFormals(&Formals1, (xxtUnionPtr)ADR(xxUnion));
          xxUnion.LocalRecoverySet = 50;
          yyResultType(&ResultType1, (xxtUnionPtr)ADR(xxUnion));
          goto EXIT_87;
          break;
        case 20:;
          Formals1.U_1.V_0.Tree = Tree_mFormals0();
          ResultType1.U_1.V_0.Tree = Tree_mVoid();
          goto EXIT_87;
          break;
        default :
          if (xxIsRepairMode) {
            Formals1.U_1.V_0.Tree = Tree_mFormals0();
            ResultType1.U_1.V_0.Tree = Tree_mVoid();
            goto EXIT_87;
          }
          xxExpected(137, 137, xxGlobalRecoverySet);
          break;
        }
      } EXIT_87:;
      if (xxToken != 20) {
        xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
      } else {
        xxToken = Scanner_GetToken();
        xxIsRepairMode = FALSE;
      }
      Def0->U_1.V_0.Tree = Tree_mProcHead(Def0->U_1.V_0.Tree, Ident1.U_1.V_1.Ident, Formals1.U_1.V_0.Tree, ResultType1.U_1.V_0.Tree, Ident1.Position);
      goto EXIT_82;
      break;
    default :
      if (xxIsRepairMode) {
        ConstDecl1.U_1.V_0.Tree = Def0->U_1.V_0.Tree;
        if (xxToken != 39) {
          xxRecoveryLiteral(39, 141, xxGlobalRecoverySet);
        } else {
          xxToken = Scanner_GetToken();
          xxIsRepairMode = FALSE;
        }
        for (;;) {
          if (xxToken == 1) {
            xxUnion.LocalRecoverySet = 50;
            yyConstDecl(&ConstDecl1, (xxtUnionPtr)ADR(xxUnion));
            if (xxToken != 20) {
              xxRecoveryLiteral(20, 50, xxGlobalRecoverySet);
            } else {
              xxToken = Scanner_GetToken();
              xxIsRepairMode = FALSE;
            }
          } else if (IN(1, xxVerticalSet0.A[xxToken]) || xxIsRepairMode) {
            goto EXIT_88;
          } else {
            xxExpected(25, 25, xxGlobalRecoverySet);
          }
        } EXIT_88:;
        Def0->U_1.V_0.Tree = ConstDecl1.U_1.V_0.Tree;
        goto EXIT_82;
      }
      xxExpected(171, 171, xxGlobalRecoverySet);
      break;
    }
  } EXIT_82:;
}

void BEGIN_Parser()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Tree();
    BEGIN_Positions();
    BEGIN_Errors();
    BEGIN_Scanner();
    BEGIN_Strings();
    BEGIN_System();
    BEGIN_Scanner();
    BEGIN_Positions();
    BEGIN_Tree();
    BEGIN_Defs();

    xxIsInitialized = FALSE;
    (void)strncpy((char *)Parser_ParsTabName.A, "Parser.Tab", sizeof(Parser_ParsTabName.A));
  }
}
