#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Base
#include "Base.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Layout
#include "Layout.h"
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

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#ifndef DEFINITION_Types
#include "Types.h"
#endif

#ifndef DEFINITION_AssocTab
#include "AssocTab.h"
#endif

#ifndef DEFINITION_GenIdents
#include "GenIdents.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_GenIdents
#include "GenIdents.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#ifndef DEFINITION_Code
#include "Code.h"
#endif

IO_tFile Code_yyf;
PROC Code_Exit;

#define pMaxPrec	1
#define pCall	1
#define pSubscript	1
#define pSelect	1
#define pCast	2
#define pDeref	3
#define pAdr	3
#define pNot	3
#define pTimes	4
#define pDivide	4
#define pMod	4
#define pPlus	5
#define pMinus	5
#define pComparison	7
#define pLess	7
#define pLessEqual	7
#define pGreater	7
#define pGreaterEqual	7
#define pEqual	8
#define pNotEqual	8
#define pIntersection	9
#define pSymDiff	10
#define pUnion	11
#define pAnd	12
#define pOr	13
#define pIn	14
#define pDifference	14
#define pIsSubset1	14
#define pIsSubset2	14
#define pAssign	14
#define pPassValue	14
#define pPassAddress	14
#define pMinPrec	14
#define Auto	1
#define Static	2
#define Full	TRUE
#define NonFull	FALSE
#define C_1_Before	TRUE
#define C_2_After	FALSE
#define Strncpy	TRUE
static IO_tFile f;
static CARDINAL Indent;
static struct S_2 {
    SHORTCARD A[Code_cPassAddress + 1];
} Precedence;
#define C_7_Infinite	1048576
#define C_8_InitialSize	16
static LONGINT C_9_StackPtr, C_10_StackSize;
static struct S_3 {
    Idents_tIdent A[C_7_Infinite + 1];
} *C_11_LabelStack;
static void C_12_Push ARGS((Idents_tIdent Ident));
static void C_13_Pop ARGS(());
static Idents_tIdent Top ARGS(());
static void C_14_Begin ARGS(());
static void C_15_Close ARGS(());
#define Infinite	1048576
#define InitialSize	16
typedef struct S_1 {
    Idents_tIdent WithId;
    Defs_tObjects Fields;
} tStackElem;
static LONGINT StackPtr, StackSize;
static struct S_4 {
    tStackElem A[Infinite + 1];
} *C_5_WithStack;
static void Push ARGS((Idents_tIdent pWithId, Defs_tObjects pFields));
static void Pop ARGS(());
static Idents_tIdent GetWith ARGS((Idents_tIdent FieldId));
static void Begin ARGS(());
static void C_6_Close ARGS(());
static Defs_tCObjects VAROpens, ValueOpens;
static void GenParams ARGS((Defs_tCObjects OpenParams));
static Idents_tIdent GetParam ARGS((Defs_tObject M2Object));
static Defs_tCObjects GlobalPtrs, LocalPtrs;
static void GenGlobalPtrs ARGS((Defs_tCObjects GlobalPtrs));
static void GenLocalPtrs ARGS((Defs_tCObjects LocalPtrs));
static Idents_tIdent GetGlobalPtr ARGS((Defs_tObject M2Object));
static Idents_tIdent GetLocalPtr ARGS((Defs_tObject M2Object));
static BOOLEAN OverOrUnderflow ARGS((Defs_tType CtrlType, Values_tValue ToValue, Values_tValue ByValue));
static void CodeString1 ARGS((StringMem_tStringRef s));
static void CodeString3 ARGS((CHAR Ch));
static void CodeChar ARGS((CHAR Ch));
static void CodeValue ARGS((Values_tValue Value));
static void CodeTypeName ARGS((Defs_tType Type));
static void CodeDeclarator1 ARGS((Defs_tType Type));
static void CodeDeclarator2 ARGS((Defs_tType Type));
static void CodePointers ARGS((SHORTCARD StorageClass, Defs_tCObjects Pointers));
static void CodePush ARGS((Defs_tCObjects LocalPtrs));
static void CodePop ARGS((Defs_tCObjects LocalPtrs));
static void CodeLabelRange ARGS((Values_tValue Label1, Values_tValue Label2));
static void CodeAlloc ARGS((Defs_tCObjects ValueOpens));
static void CodeCopy ARGS((Defs_tCObjects ValueOpens));
static void CodeFree ARGS((Defs_tCObjects ValueOpens));
static void CodeStrings ARGS((Defs_tStrings Strings));
static void CodeStrncpy ARGS((Defs_tStrings Strings));
static void CodeSelectors ARGS((Defs_tSelectors Selectors));
static void CodeCast ARGS((Defs_tType Type));
static void CodeCastVAR ARGS((Defs_tType Type));
static void CodeMIN ARGS((Defs_tType Type));
static void CodeMAX ARGS((Defs_tType Type));
static void yyAbort ARGS((CHAR yyFunction[], LONGCARD ));
static BOOLEAN yyIsEqual ARGS((BYTE yya[], LONGCARD , BYTE yyb[], LONGCARD ));
struct S_7 {
    union {
        struct {
            struct S_8 {
                Tree_tTree DefMod;
            } yyR4;
        } V_1;
    } U_1;
};
static void CodeImports ARGS((Tree_tTree yyP2));
struct S_9 {
    union {
        char dummy;
    } U_1;
};
static void CodeInits ARGS((Tree_tTree t));
struct S_10 {
    union {
        char dummy;
    } U_1;
};
static void CodeDefs1 ARGS((Tree_tTree t));
struct S_11 {
    union {
        char dummy;
    } U_1;
};
static void CodeDefs2 ARGS((Tree_tTree t));
struct S_12 {
    union {
        char dummy;
    } U_1;
};
static void CodeDecls1 ARGS((Tree_tTree t));
struct S_13 {
    union {
        char dummy;
    } U_1;
};
static void CodeType ARGS((Tree_tTree t, BOOLEAN C_4_Full));
struct S_14 {
    union {
        struct {
            struct S_17 {
                Values_tValue Lwb;
                Tree_tTree LwbExpr;
                Tree_tTree UpbExpr;
            } yyR1;
        } V_1;
        struct {
            struct S_16 {
                Values_tValue Upb;
            } yyR2;
        } V_2;
        struct {
            struct S_15 {
                Values_tValue Lwb;
                Values_tValue Upb;
            } yyR3;
        } V_3;
    } U_1;
};
static void CodeDecla ARGS((Tree_tTree yyP3, BOOLEAN Before));
struct S_18 {
    union {
        char dummy;
    } U_1;
};
static void CodeStruct ARGS((Tree_tTree t));
struct S_19 {
    union {
        char dummy;
    } U_1;
};
static void CodeEnums ARGS((Tree_tTree t));
struct S_20 {
    union {
        char dummy;
    } U_1;
};
static void CodeIds ARGS((Tree_tTree t, Tree_tTree Type));
struct S_21 {
    union {
        char dummy;
    } U_1;
};
static void CodeProcs ARGS((Tree_tTree t));
struct S_22 {
    union {
        struct {
            struct S_23 {
                BOOLEAN NewLine;
            } yyR1;
        } V_1;
    } U_1;
};
static void CodeFormals1 ARGS((Tree_tTree t));
struct S_24 {
    union {
        char dummy;
    } U_1;
};
static void CodeFormals2 ARGS((Tree_tTree t, Idents_tIdent ElemTypeId));
struct S_25 {
    union {
        char dummy;
    } U_1;
};
static void CodeFormals3 ARGS((Tree_tTree t, Idents_tIdent TypeId));
struct S_26 {
    union {
        char dummy;
    } U_1;
};
static void CodeFormals4 ARGS((Tree_tTree yyP4));
struct S_27 {
    union {
        char dummy;
    } U_1;
};
static void CodeDecls2 ARGS((Tree_tTree yyP5, BOOLEAN *NewLine));
struct S_28 {
    union {
        char dummy;
    } U_1;
};
static void CodeExpr ARGS((Tree_tTree t, SHORTCARD Prec));
struct S_29 {
    union {
        struct {
            struct S_36 {
                Strings_tString String;
            } yyR10;
        } V_1;
        struct {
            struct S_35 {
                Values_tValue Lwb;
            } yyR30;
        } V_2;
        struct {
            struct S_34 {
                Values_tValue Lwb;
            } yyR31;
        } V_3;
        struct {
            struct S_33 {
                Values_tValue Lwb;
            } yyR32;
        } V_4;
        struct {
            struct S_32 {
                Values_tValue Lwb;
            } yyR33;
        } V_5;
        struct {
            struct S_31 {
                Defs_tObject Object;
            } yyR34;
        } V_6;
        struct {
            struct S_30 {
                Defs_tObject Object;
            } yyR37;
        } V_7;
    } U_1;
};
static void CodeAdr ARGS((Tree_tTree t));
struct S_37 {
    union {
        char dummy;
    } U_1;
};
static void CodeString2 ARGS((Tree_tTree t, BOOLEAN C_3_Strncpy));
struct S_38 {
    union {
        char dummy;
    } U_1;
};
static void CodeStmts ARGS((Tree_tTree t));
struct S_39 {
    union {
        struct {
            struct S_44 {
                SHORTCARD StdProc;
            } yyR4;
        } V_1;
        struct {
            struct S_43 {
                Idents_tIdent BoundId1;
                Idents_tIdent BoundId2;
                LONGINT ByValue;
            } yyR15;
        } V_2;
        struct {
            struct S_42 {
                Idents_tIdent WithId;
                Defs_tObject Object;
            } yyR16;
        } V_3;
        struct {
            struct S_41 {
                Idents_tIdent WithId;
            } yyR17;
        } V_4;
        struct {
            struct S_40 {
                Idents_tIdent ReturnId;
            } yyR20;
        } V_5;
    } U_1;
};
static void CodeStdProc ARGS((Tree_tTree yyP6, SHORTCARD StdProc, Defs_tObject AllocOrDealloc));
struct S_45 {
    union {
        struct {
            struct S_46 {
                Values_tValue Upb;
                Defs_tObject Object;
                Defs_tType Type;
            } yyR3;
        } V_1;
    } U_1;
};
static void CodeActuals ARGS((Tree_tTree t));
struct S_47 {
    union {
        struct {
            struct S_50 {
                CARDINAL length;
            } yyR3;
        } V_1;
        struct {
            struct S_49 {
                CARDINAL length;
            } yyR5;
        } V_2;
        struct {
            struct S_48 {
                Values_tValue Upb;
                Values_tValue Lwb;
                Defs_tType IndexType;
            } yyR9;
        } V_3;
    } U_1;
};
static void CodeSepa ARGS((Tree_tTree yyP7));
struct S_51 {
    union {
        char dummy;
    } U_1;
};
static void yyExit ARGS(());


static void C_12_Push
# ifdef __STDC__
(Idents_tIdent Ident)
# else
(Ident)
Idents_tIdent Ident;
# endif
{
  if (C_9_StackPtr == C_10_StackSize) {
    DynArray_ExtendArray((ADDRESS *)&C_11_LabelStack, &C_10_StackSize, (LONGINT)sizeof(Idents_tIdent));
  }
  C_11_LabelStack->A[C_9_StackPtr] = Ident;
  INC(C_9_StackPtr);
}

static void C_13_Pop
# ifdef __STDC__
()
# else
()
# endif
{
  DEC(C_9_StackPtr);
}

static Idents_tIdent Top
# ifdef __STDC__
()
# else
()
# endif
{
  if (C_9_StackPtr > 0) {
    return C_11_LabelStack->A[C_9_StackPtr - 1];
  } else {
    return Idents_NoIdent;
  }
}

static void C_14_Begin
# ifdef __STDC__
()
# else
()
# endif
{
  C_10_StackSize = C_8_InitialSize;
  C_9_StackPtr = 0;
  DynArray_MakeArray((ADDRESS *)&C_11_LabelStack, &C_10_StackSize, (LONGINT)sizeof(Idents_tIdent));
}

static void C_15_Close
# ifdef __STDC__
()
# else
()
# endif
{
  DynArray_ReleaseArray((ADDRESS *)&C_11_LabelStack, &C_10_StackSize, (LONGINT)sizeof(Idents_tIdent));
}

static void LabelStack()
{
}

static void Push
# ifdef __STDC__
(Idents_tIdent pWithId, Defs_tObjects pFields)
# else
(pWithId, pFields)
Idents_tIdent pWithId;
Defs_tObjects pFields;
# endif
{
  if (StackPtr == StackSize) {
    DynArray_ExtendArray((ADDRESS *)&C_5_WithStack, &StackSize, (LONGINT)sizeof(tStackElem));
  }
  {
    register tStackElem *W_1 = &C_5_WithStack->A[StackPtr];

    W_1->WithId = pWithId;
    W_1->Fields = pFields;
  }
  INC(StackPtr);
}

static void Pop
# ifdef __STDC__
()
# else
()
# endif
{
  DEC(StackPtr);
}

static Idents_tIdent GetWith
# ifdef __STDC__
(Idents_tIdent FieldId)
# else
(FieldId)
Idents_tIdent FieldId;
# endif
{
  LONGINT StackIndex;

  StackIndex = StackPtr - 1;
  for (;;) {
    {
      register tStackElem *W_2 = &C_5_WithStack->A[StackIndex];

      if (Defs_Identify2(FieldId, W_2->Fields) != Defs_NoObject) {
        return W_2->WithId;
      }
    }
    DEC(StackIndex);
  } EXIT_1:;
}

static void Begin
# ifdef __STDC__
()
# else
()
# endif
{
  StackSize = InitialSize;
  StackPtr = 0;
  DynArray_MakeArray((ADDRESS *)&C_5_WithStack, &StackSize, (LONGINT)sizeof(tStackElem));
}

static void C_6_Close
# ifdef __STDC__
()
# else
()
# endif
{
  DynArray_ReleaseArray((ADDRESS *)&C_5_WithStack, &StackSize, (LONGINT)sizeof(tStackElem));
}

static void WithStack()
{
}

static void GenParams
# ifdef __STDC__
(Defs_tCObjects OpenParams)
# else
(OpenParams)
Defs_tCObjects OpenParams;
# endif
{
  while (OpenParams != Defs_NoCObjects) {
    OpenParams->U_1.V_48.CObjects.CObject = GenIdents_GenParam();
    OpenParams = OpenParams->U_1.V_48.CObjects.Next;
  }
}

static Idents_tIdent GetParam
# ifdef __STDC__
(Defs_tObject M2Object)
# else
(M2Object)
Defs_tObject M2Object;
# endif
{
  if (Defs_IsVAR(M2Object)) {
    return Defs_LookUp(M2Object, VAROpens);
  } else {
    return Defs_LookUp(M2Object, ValueOpens);
  }
}

static void ParamTable()
{
  ValueOpens = Defs_NoCObjects;
  VAROpens = Defs_NoCObjects;
}

static void GenGlobalPtrs
# ifdef __STDC__
(Defs_tCObjects GlobalPtrs)
# else
(GlobalPtrs)
Defs_tCObjects GlobalPtrs;
# endif
{
  while (GlobalPtrs != Defs_NoCObjects) {
    {
      register Defs_yCObjects *W_3 = &GlobalPtrs->U_1.V_48.CObjects;

      W_3->CObject = GenIdents_GenGlobalPtr(W_3->M2Object->U_1.V_13.Var1.Ident);
    }
    GlobalPtrs = GlobalPtrs->U_1.V_48.CObjects.Next;
  }
}

static void GenLocalPtrs
# ifdef __STDC__
(Defs_tCObjects LocalPtrs)
# else
(LocalPtrs)
Defs_tCObjects LocalPtrs;
# endif
{
  while (LocalPtrs != Defs_NoCObjects) {
    LocalPtrs->U_1.V_48.CObjects.CObject = GenIdents_GenLocalPtr();
    LocalPtrs = LocalPtrs->U_1.V_48.CObjects.Next;
  }
}

static Idents_tIdent GetGlobalPtr
# ifdef __STDC__
(Defs_tObject M2Object)
# else
(M2Object)
Defs_tObject M2Object;
# endif
{
  return Defs_LookUp(M2Object, GlobalPtrs);
}

static Idents_tIdent GetLocalPtr
# ifdef __STDC__
(Defs_tObject M2Object)
# else
(M2Object)
Defs_tObject M2Object;
# endif
{
  return Defs_LookUp(M2Object, LocalPtrs);
}

static void PointerTable()
{
  GlobalPtrs = Defs_NoCObjects;
  LocalPtrs = Defs_NoCObjects;
}

static BOOLEAN OverOrUnderflow
# ifdef __STDC__
(Defs_tType CtrlType, Values_tValue ToValue, Values_tValue ByValue)
# else
(CtrlType, ToValue, ByValue)
Defs_tType CtrlType;
Values_tValue ToValue, ByValue;
# endif
{
  Values_tValue MinCValue, MaxCValue;

  if (ToValue.Kind == Values_NoValue) {
    return TRUE;
  } else {
    if (CtrlType->U_1.V_1.Kind == Defs_Subrange1) {
      CtrlType = CtrlType->U_1.V_44.Subrange1.Type;
    }
    switch (CtrlType->U_1.V_1.Kind) {
    case Defs_ShortCard:;
    case Defs_ShortInt:;
    case Defs_LongCard:;
    case Defs_LongInt:;
    case Defs_Char:;
      Types_GetLwb(CtrlType, &MinCValue);
      Types_GetUpb(CtrlType, &MaxCValue);
      break;
    case Defs_Bool:;
    case Defs_Enumeration1:;
      if (Types_TypeSize(CtrlType) == Types_SizeUnsignedChar) {
        MinCValue = Values_MinCharVal;
        MaxCValue = Values_MaxCharVal;
      } else {
        MinCValue = Values_ZeroValue;
        MaxCValue = Values_MaxShortCardVal;
      }
      break;
    default :
      return TRUE;
      break;
    }
    if (Values_ValueToInt(ByValue) >= 0) {
      if (CtrlType == Defs_TypeLONGCARD) {
        return FALSE;
      }
      return Values_ValueToInt(ToValue) > Values_ValueToInt(MaxCValue) - Values_ValueToInt(ByValue);
    } else {
      return Values_ValueToInt(ToValue) < Values_ValueToInt(MinCValue) - Values_ValueToInt(ByValue);
    }
  }
}

static void CodeString1
# ifdef __STDC__
(StringMem_tStringRef s)
# else
(s)
StringMem_tStringRef s;
# endif
{
  Strings_tStringIndex i;
  CHAR Ch;
  Strings_tString String;

  StringMem_GetString(s, &String);
  IO_WriteC(f, '"');
  {
    Strings_tStringIndex B_1 = 1, B_2 = StringMem_Length(s);

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        Ch = Strings_Char(&String, i);
        if (Ch == '"' || Ch == '\\') {
          IO_WriteC(f, '\\');
        }
        IO_WriteC(f, Ch);
        if (i >= B_2) break;
      }
  }
  IO_WriteC(f, '"');
}

static void CodeString3
# ifdef __STDC__
(CHAR Ch)
# else
(Ch)
CHAR Ch;
# endif
{
  IO_WriteC(f, '"');
  if (Ch < ' ' || Ch > '~') {
    IO_WriteC(f, '\\');
    IO_WriteN(f, ORD(Ch), 1L, 8L);
  } else if (Ch == '"' || Ch == '\\') {
    IO_WriteC(f, '\\');
    IO_WriteC(f, Ch);
  } else {
    IO_WriteC(f, Ch);
  }
  IO_WriteC(f, '"');
}

static void CodeChar
# ifdef __STDC__
(CHAR Ch)
# else
(Ch)
CHAR Ch;
# endif
{
  switch (Ch) {
  case '\'':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\'');
    break;
  case '\\':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteC(f, '\\');
    IO_WriteC(f, '\'');
    break;
  case ' ':;
  case '!':;
  case '"':;
  case '#':;
  case '$':;
  case '%':;
  case '&':;
  case '(':;
  case ')':;
  case '*':;
  case '+':;
  case ',':;
  case '-':;
  case '.':;
  case '/':;
  case '0':;
  case '1':;
  case '2':;
  case '3':;
  case '4':;
  case '5':;
  case '6':;
  case '7':;
  case '8':;
  case '9':;
  case ':':;
  case ';':;
  case '<':;
  case '=':;
  case '>':;
  case '?':;
  case '@':;
  case 'A':;
  case 'B':;
  case 'C':;
  case 'D':;
  case 'E':;
  case 'F':;
  case 'G':;
  case 'H':;
  case 'I':;
  case 'J':;
  case 'K':;
  case 'L':;
  case 'M':;
  case 'N':;
  case 'O':;
  case 'P':;
  case 'Q':;
  case 'R':;
  case 'S':;
  case 'T':;
  case 'U':;
  case 'V':;
  case 'W':;
  case 'X':;
  case 'Y':;
  case 'Z':;
  case '[':;
  case ']':;
  case '^':;
  case '_':;
  case '`':;
  case 'a':;
  case 'b':;
  case 'c':;
  case 'd':;
  case 'e':;
  case 'f':;
  case 'g':;
  case 'h':;
  case 'i':;
  case 'j':;
  case 'k':;
  case 'l':;
  case 'm':;
  case 'n':;
  case 'o':;
  case 'p':;
  case 'q':;
  case 'r':;
  case 's':;
  case 't':;
  case 'u':;
  case 'v':;
  case 'w':;
  case 'x':;
  case 'y':;
  case 'z':;
  case '{':;
  case '|':;
  case '}':;
  case '~':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, Ch);
    IO_WriteC(f, '\'');
    break;
  case '\0':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteC(f, '0');
    IO_WriteC(f, '\'');
    break;
  case '\b':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteC(f, 'b');
    IO_WriteC(f, '\'');
    break;
  case '\t':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteC(f, 't');
    IO_WriteC(f, '\'');
    break;
  case '\n':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteC(f, 'n');
    IO_WriteC(f, '\'');
    break;
  case '\f':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteC(f, 'f');
    IO_WriteC(f, '\'');
    break;
  case '\r':;
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteC(f, 'r');
    IO_WriteC(f, '\'');
    break;
  default :
    IO_WriteS(f, (STRING)"((CHAR)", 7L);
    IO_WriteC(f, '\'');
    IO_WriteC(f, '\\');
    IO_WriteN(f, ORD(Ch), 1L, 8L);
    IO_WriteC(f, '\'');
    IO_WriteC(f, ')');
    break;
  }
}

static void CodeValue
# ifdef __STDC__
(Values_tValue Value)
# else
(Value)
Values_tValue Value;
# endif
{
  Defs_tObject Object;

  switch (Value.Kind) {
  case Values_Integer:;
    IO_WriteI(f, Value.U_1.V_1.IntValue, 1L);
    break;
  case Values_Real:;
    IO_WriteR(f, (REAL)Value.U_1.V_2.RealValue, 1L, 1L, 1L);
    break;
  case Values_Boolean:;
    if (Value.U_1.V_3.BoolValue) {
      IO_WriteS(f, (STRING)"TRUE", 4L);
    } else {
      IO_WriteS(f, (STRING)"FALSE", 5L);
    }
    break;
  case Values_StringChar:;
  case Values_Char:;
    CodeChar(Value.U_1.V_4.CharValue);
    break;
  case Values_String:;
    CodeString1(Value.U_1.V_5.StringValue);
    break;
  case Values_Bitset:;
    IO_WriteS(f, (STRING)"0X", 2L);
    IO_WriteN(f, (LONGCARD)Value.U_1.V_6.BitsetValue, 1L, 16L);
    IO_WriteC(f, 'L');
    break;
  case Values_Enumeration:;
    Object = (Defs_tDefs)Value.U_1.V_7.EnumValue;
    Idents_WriteIdent(f, Object->U_1.V_6.EnumLiteral1.CIdent);
    break;
  case Values_NilType:;
    IO_WriteS(f, (STRING)"NIL", 3L);
    break;
  default :
    break;
  }
}

static void CodeTypeName
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  Defs_tObject Object;

  switch (Type->U_1.V_1.Kind) {
  case Defs_Array1:;
  case Defs_Enumeration1:;
  case Defs_Pointer1:;
  case Defs_ProcType1:;
  case Defs_Record1:;
  case Defs_Set1:;
  case Defs_Subrange1:;
  case Defs_OpaqueType1:;
    Object = Type->U_1.V_36.Constructor.TypeObj;
    if (Object != Defs_NoObject) {
      Idents_WriteIdent(f, Object->U_1.V_3.Object.CIdent);
      return;
    }
    break;
  default :
    break;
  }
  if (Type->U_1.V_1.Kind == Defs_Subrange1) {
    Type = Type->U_1.V_44.Subrange1.Type;
  }
  switch (Type->U_1.V_1.Kind) {
  case Defs_ShortInt:;
    IO_WriteS(f, (STRING)"SHORTINT", 8L);
    break;
  case Defs_LongInt:;
    IO_WriteS(f, (STRING)"LONGINT", 7L);
    break;
  case Defs_ShortCard:;
    IO_WriteS(f, (STRING)"SHORTCARD", 9L);
    break;
  case Defs_LongCard:;
    IO_WriteS(f, (STRING)"LONGCARD", 8L);
    break;
  case Defs_Real:;
    IO_WriteS(f, (STRING)"REAL", 4L);
    break;
  case Defs_LongReal:;
    IO_WriteS(f, (STRING)"LONGREAL", 8L);
    break;
  case Defs_Bool:;
    IO_WriteS(f, (STRING)"BOOLEAN", 7L);
    break;
  case Defs_Char:;
    IO_WriteS(f, (STRING)"CHAR", 4L);
    break;
  case Defs_Bitset:;
    IO_WriteS(f, (STRING)"BITSET", 6L);
    break;
  case Defs_Proc:;
    IO_WriteS(f, (STRING)"PROC", 4L);
    break;
  case Defs_Word:;
    IO_WriteS(f, (STRING)"WORD", 4L);
    break;
  case Defs_Address:;
    IO_WriteS(f, (STRING)"ADDRESS", 7L);
    break;
  case Defs_OpaqueType1:;
    IO_WriteS(f, (STRING)"OPAQUE", 6L);
    break;
  case Defs_Array1:;
    IO_WriteS(f, (STRING)"struct ", 7L);
    Idents_WriteIdent(f, Type->U_1.V_38.Array1.StructId);
    break;
  case Defs_Enumeration1:;
    if (Types_TypeSize(Type) == Types_SizeUnsignedChar) {
      IO_WriteS(f, (STRING)"unsigned char", 13L);
    } else {
      IO_WriteS(f, (STRING)"unsigned short", 14L);
    }
    break;
  case Defs_Pointer1:;
    CodeTypeName(Type->U_1.V_40.Pointer1.Type);
    break;
  case Defs_ProcType1:;
    CodeTypeName(Type->U_1.V_41.ProcType1.Type);
    break;
  case Defs_Record1:;
    IO_WriteS(f, (STRING)"struct ", 7L);
    Idents_WriteIdent(f, Type->U_1.V_42.Record1.StructId);
    break;
  case Defs_Set1:;
    IO_WriteS(f, (STRING)"unsigned long", 13L);
    break;
  default :
    break;
  }
}

static void CodeDeclarator1
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_Pointer1 && Type->U_1.V_40.Pointer1.TypeObj == Defs_NoObject) {
    CodeDeclarator1(Type->U_1.V_40.Pointer1.Type);
    IO_WriteS(f, (STRING)" *", 2L);
  } else if (Type->U_1.V_1.Kind == Defs_ProcType1 && Type->U_1.V_41.ProcType1.TypeObj == Defs_NoObject) {
    IO_WriteS(f, (STRING)" (*", 3L);
  }
}

static void CodeDeclarator2
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_Pointer1 && Type->U_1.V_40.Pointer1.TypeObj == Defs_NoObject) {
    CodeDeclarator2(Type->U_1.V_40.Pointer1.Type);
  } else if (Type->U_1.V_1.Kind == Defs_ProcType1 && Type->U_1.V_41.ProcType1.TypeObj == Defs_NoObject) {
    IO_WriteS(f, (STRING)")()", 3L);
  }
}

static void CodePointers
# ifdef __STDC__
(SHORTCARD StorageClass, Defs_tCObjects Pointers)
# else
(StorageClass, Pointers)
SHORTCARD StorageClass;
Defs_tCObjects Pointers;
# endif
{
  Tree_tTree Type;

  while (Pointers != Defs_NoCObjects) {
    {
      register Defs_yCObjects *W_4 = &Pointers->U_1.V_48.CObjects;

      Type = (Tree_tTree)W_4->M2Object->U_1.V_13.Var1.TypeTree;
      if (!Defs_IsOpenArray(W_4->M2Object)) {
        Layout_WriteSpaces(f, (LONGINT)Indent);
        if (StorageClass == Static) {
          IO_WriteS(f, (STRING)"static ", 7L);
        }
        CodeType(Type, NonFull);
        CodeDecla(Type, C_1_Before);
        IO_WriteS(f, (STRING)" *", 2L);
        Idents_WriteIdent(f, W_4->CObject);
        CodeDecla(Type, C_2_After);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
      } else {
        Layout_WriteSpaces(f, (LONGINT)Indent);
        if (StorageClass == Static) {
          IO_WriteS(f, (STRING)"static ", 7L);
        }
        CodeType(Type->U_1.V_44.Array.ElemType, NonFull);
        IO_WriteS(f, (STRING)" *", 2L);
        Idents_WriteIdent(f, W_4->CObject);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        if (StorageClass == Static) {
          IO_WriteS(f, (STRING)"static ", 7L);
        }
        IO_WriteS(f, (STRING)"LONGCARD ", 9L);
        Idents_WriteIdent(f, W_4->CObject);
        IO_WriteS(f, (STRING)"_O;", 3L);
        IO_WriteNl(f);
      }
    }
    Pointers = Pointers->U_1.V_48.CObjects.Next;
  }
}

static void CodePush
# ifdef __STDC__
(Defs_tCObjects LocalPtrs)
# else
(LocalPtrs)
Defs_tCObjects LocalPtrs;
# endif
{
  Idents_tIdent GlobalPtr;

  while (LocalPtrs != Defs_NoCObjects) {
    {
      register Defs_yCObjects *W_5 = &LocalPtrs->U_1.V_48.CObjects;

      GlobalPtr = GetGlobalPtr(W_5->M2Object);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      Idents_WriteIdent(f, W_5->CObject);
      IO_WriteS(f, (STRING)" = ", 3L);
      Idents_WriteIdent(f, GlobalPtr);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      if (Defs_IsOpenArray(W_5->M2Object)) {
        Layout_WriteSpaces(f, (LONGINT)Indent);
        Idents_WriteIdent(f, GlobalPtr);
        IO_WriteS(f, (STRING)" = ", 3L);
        Idents_WriteIdent(f, W_5->M2Object->U_1.V_13.Var1.CIdent);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        Idents_WriteIdent(f, W_5->CObject);
        IO_WriteS(f, (STRING)"_O = ", 5L);
        Idents_WriteIdent(f, GlobalPtr);
        IO_WriteS(f, (STRING)"_O;", 3L);
        IO_WriteNl(f);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        Idents_WriteIdent(f, GlobalPtr);
        IO_WriteS(f, (STRING)"_O = ", 5L);
        Idents_WriteIdent(f, GetParam(W_5->M2Object));
        IO_WriteC(f, ';');
        IO_WriteNl(f);
      } else if (Defs_IsVAR(W_5->M2Object)) {
        Layout_WriteSpaces(f, (LONGINT)Indent);
        Idents_WriteIdent(f, GlobalPtr);
        IO_WriteS(f, (STRING)" = ", 3L);
        Idents_WriteIdent(f, W_5->M2Object->U_1.V_13.Var1.CIdent);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
      } else {
        Layout_WriteSpaces(f, (LONGINT)Indent);
        Idents_WriteIdent(f, GlobalPtr);
        IO_WriteS(f, (STRING)" = &", 4L);
        Idents_WriteIdent(f, W_5->M2Object->U_1.V_13.Var1.CIdent);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
      }
    }
    LocalPtrs = LocalPtrs->U_1.V_48.CObjects.Next;
  }
}

static void CodePop
# ifdef __STDC__
(Defs_tCObjects LocalPtrs)
# else
(LocalPtrs)
Defs_tCObjects LocalPtrs;
# endif
{
  Idents_tIdent GlobalPtr;

  while (LocalPtrs != Defs_NoCObjects) {
    {
      register Defs_yCObjects *W_6 = &LocalPtrs->U_1.V_48.CObjects;

      GlobalPtr = GetGlobalPtr(W_6->M2Object);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      Idents_WriteIdent(f, GlobalPtr);
      IO_WriteS(f, (STRING)" = ", 3L);
      Idents_WriteIdent(f, W_6->CObject);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      if (Defs_IsOpenArray(W_6->M2Object)) {
        Layout_WriteSpaces(f, (LONGINT)Indent);
        Idents_WriteIdent(f, GlobalPtr);
        IO_WriteS(f, (STRING)"_O = ", 5L);
        Idents_WriteIdent(f, W_6->CObject);
        IO_WriteS(f, (STRING)"_O;", 3L);
        IO_WriteNl(f);
      }
    }
    LocalPtrs = LocalPtrs->U_1.V_48.CObjects.Next;
  }
}

static void CodeLabelRange
# ifdef __STDC__
(Values_tValue Label1, Values_tValue Label2)
# else
(Label1, Label2)
Values_tValue Label1, Label2;
# endif
{
  INTEGER IntLabel;
  BOOLEAN BoolLabel;
  CHAR CharLabel;
  Defs_tObjects EnumLiterals;
  SHORTCARD EnumIndex1, EnumIndex2;
  Defs_tType Type;

  switch (Label1.Kind) {
  case Values_Integer:;
    {
      LONGINT B_3 = Label1.U_1.V_1.IntValue, B_4 = Label2.U_1.V_1.IntValue;

      if (B_3 <= B_4)
        for (IntLabel = B_3;; IntLabel += 1) {
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteS(f, (STRING)"case ", 5L);
          IO_WriteI(f, IntLabel, 1L);
          IO_WriteS(f, (STRING)":;", 2L);
          IO_WriteNl(f);
          if (IntLabel >= B_4) break;
        }
    }
    break;
  case Values_Boolean:;
    {
      BOOLEAN B_5 = Label1.U_1.V_3.BoolValue, B_6 = Label2.U_1.V_3.BoolValue;

      if (B_5 <= B_6)
        for (BoolLabel = B_5;; BoolLabel += 1) {
          if (BoolLabel) {
            Layout_WriteSpaces(f, (LONGINT)Indent);
            IO_WriteS(f, (STRING)"case TRUE:", 10L);
            IO_WriteNl(f);
          } else {
            Layout_WriteSpaces(f, (LONGINT)Indent);
            IO_WriteS(f, (STRING)"case FALSE:", 11L);
            IO_WriteNl(f);
          }
          if (BoolLabel >= B_6) break;
        }
    }
    break;
  case Values_Char:;
  case Values_StringChar:;
    {
      CHAR B_7 = Label1.U_1.V_4.CharValue, B_8 = Label2.U_1.V_4.CharValue;

      if (B_7 <= B_8)
        for (CharLabel = B_7;; CharLabel += 1) {
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteS(f, (STRING)"case ", 5L);
          CodeChar(CharLabel);
          IO_WriteS(f, (STRING)":;", 2L);
          IO_WriteNl(f);
          if (CharLabel >= B_8) break;
        }
    }
    break;
  case Values_Enumeration:;
    Type = Defs_GetType((Defs_tDefs)Label1.U_1.V_7.EnumValue);
    EnumLiterals = Type->U_1.V_39.Enumeration1.Objects;
    EnumIndex1 = Values_ValueToInt(Label1);
    EnumIndex2 = Values_ValueToInt(Label2);
    while (EnumLiterals != Defs_NoObjects) {
      {
        register Defs_yEnumLiteral1 *W_7 = &EnumLiterals->U_1.V_46.Elmt.Object->U_1.V_6.EnumLiteral1;

        if (W_7->Index >= EnumIndex1 && W_7->Index <= EnumIndex2) {
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteS(f, (STRING)"case ", 5L);
          Idents_WriteIdent(f, W_7->CIdent);
          IO_WriteS(f, (STRING)":;", 2L);
          IO_WriteNl(f);
        }
      }
      EnumLiterals = EnumLiterals->U_1.V_46.Elmt.Next;
    }
    break;
  default :
    break;
  }
}

static void CodeAlloc
# ifdef __STDC__
(Defs_tCObjects ValueOpens)
# else
(ValueOpens)
Defs_tCObjects ValueOpens;
# endif
{
  CARDINAL ArrayCnt;

  if (ValueOpens != Defs_NoCObjects) {
    ArrayCnt = 0;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"ALLOC_OPEN_ARRAYS(", 18L);
    for (;;) {
      INC(ArrayCnt);
      {
        register Defs_yCObjects *W_8 = &ValueOpens->U_1.V_48.CObjects;

        Idents_WriteIdent(f, W_8->CObject);
        IO_WriteS(f, (STRING)" * ", 3L);
        IO_WriteS(f, (STRING)"sizeof(", 7L);
        CodeTypeName(Defs_GetElemType(Defs_GetType(W_8->M2Object)));
        IO_WriteC(f, ')');
      }
      ValueOpens = ValueOpens->U_1.V_48.CObjects.Next;
      if (ValueOpens == Defs_NoObjects) {
        goto EXIT_2;
      }
      IO_WriteS(f, (STRING)" + ", 3L);
    } EXIT_2:;
    IO_WriteS(f, (STRING)", ", 2L);
    IO_WriteI(f, (LONGINT)ArrayCnt, 1L);
    IO_WriteC(f, ')');
    IO_WriteNl(f);
  }
}

static void CodeCopy
# ifdef __STDC__
(Defs_tCObjects ValueOpens)
# else
(ValueOpens)
Defs_tCObjects ValueOpens;
# endif
{
  while (ValueOpens != Defs_NoCObjects) {
    {
      register Defs_yCObjects *W_9 = &ValueOpens->U_1.V_48.CObjects;

      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"COPY_OPEN_ARRAY(", 16L);
      Idents_WriteIdent(f, W_9->M2Object->U_1.V_13.Var1.CIdent);
      IO_WriteS(f, (STRING)", ", 2L);
      Idents_WriteIdent(f, W_9->CObject);
      IO_WriteS(f, (STRING)", ", 2L);
      CodeTypeName(Defs_GetElemType(Defs_GetType(W_9->M2Object)));
      IO_WriteC(f, ')');
      IO_WriteNl(f);
    }
    ValueOpens = ValueOpens->U_1.V_48.CObjects.Next;
  }
}

static void CodeFree
# ifdef __STDC__
(Defs_tCObjects ValueOpens)
# else
(ValueOpens)
Defs_tCObjects ValueOpens;
# endif
{
  if (ValueOpens != Defs_NoCObjects) {
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"FREE_OPEN_ARRAYS", 16L);
    IO_WriteNl(f);
  }
}

static void CodeStrings
# ifdef __STDC__
(Defs_tStrings Strings)
# else
(Strings)
Defs_tStrings Strings;
# endif
{
  while (Strings != Defs_NoStrings) {
    {
      register Defs_yStringPar *W_10 = &Strings->U_1.V_52.StringPar;

      Layout_WriteSpaces(f, (LONGINT)Indent);
      CodeTypeName(W_10->FormalType);
      IO_WriteC(f, ' ');
      Idents_WriteIdent(f, W_10->CString);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
    }
    Strings = Strings->U_1.V_52.StringPar.Next;
  }
}

static void CodeStrncpy
# ifdef __STDC__
(Defs_tStrings Strings)
# else
(Strings)
Defs_tStrings Strings;
# endif
{
  while (Strings != Defs_NoStrings) {
    {
      register Defs_yStringPar *W_11 = &Strings->U_1.V_52.StringPar;

      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"(void)strncpy(", 14L);
      if (Base_OptionIsSet('c')) {
        IO_WriteS(f, (STRING)"(char *)", 8L);
      }
      Idents_WriteIdent(f, W_11->CString);
      IO_WriteS(f, (STRING)".A, ", 4L);
      CodeString2((Tree_tTree)W_11->M2String, Strncpy);
      IO_WriteS(f, (STRING)", ", 2L);
      IO_WriteS(f, (STRING)"sizeof(", 7L);
      Idents_WriteIdent(f, W_11->CString);
      IO_WriteS(f, (STRING)".A)", 3L);
      IO_WriteS(f, (STRING)");", 2L);
      IO_WriteNl(f);
    }
    Strings = Strings->U_1.V_52.StringPar.Next;
  }
}

static void CodeSelectors
# ifdef __STDC__
(Defs_tSelectors Selectors)
# else
(Selectors)
Defs_tSelectors Selectors;
# endif
{
  if (Selectors != Defs_NoSelectors) {
    CodeSelectors(Selectors->U_1.V_51.Selectors.Next);
    Idents_WriteIdent(f, Selectors->U_1.V_51.Selectors.Selector);
    IO_WriteC(f, '.');
  }
}

static void CodeCast
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  IO_WriteC(f, '(');
  CodeTypeName(Type);
  CodeDeclarator1(Type);
  CodeDeclarator2(Type);
  IO_WriteC(f, ')');
}

static void CodeCastVAR
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  IO_WriteC(f, '(');
  CodeTypeName(Type);
  CodeDeclarator1(Type);
  IO_WriteS(f, (STRING)" *", 2L);
  CodeDeclarator2(Type);
  IO_WriteC(f, ')');
}

static void CodeMIN
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  Defs_tObject Object;

  switch (Type->U_1.V_1.Kind) {
  case Defs_ShortInt:;
    IO_WriteS(f, (STRING)"MIN_SHORTINT", 12L);
    break;
  case Defs_LongInt:;
    IO_WriteS(f, (STRING)"MIN_LONGINT", 11L);
    break;
  case Defs_ShortCard:;
    IO_WriteS(f, (STRING)"MIN_SHORTCARD", 13L);
    break;
  case Defs_LongCard:;
    IO_WriteS(f, (STRING)"MIN_LONGCARD", 12L);
    break;
  case Defs_Real:;
    IO_WriteS(f, (STRING)"MIN_REAL", 8L);
    break;
  case Defs_LongReal:;
    IO_WriteS(f, (STRING)"MIN_LONGREAL", 12L);
    break;
  case Defs_Bool:;
    IO_WriteS(f, (STRING)"MIN_BOOLEAN", 11L);
    break;
  case Defs_Char:;
    IO_WriteS(f, (STRING)"MIN_CHAR", 8L);
    break;
  case Defs_Enumeration1:;
    {
      register Defs_yEnumeration1 *W_12 = &Type->U_1.V_39.Enumeration1;

      Object = Defs_GetLiteral(W_12->Objects, 0);
    }
    Idents_WriteIdent(f, Object->U_1.V_6.EnumLiteral1.CIdent);
    break;
  case Defs_Subrange1:;
    CodeValue(Type->U_1.V_44.Subrange1.Lwb);
    if (Defs_IsIntType(Type) && Types_Cast(Code_cPassValue, Type, Defs_TypeIntCard)) {
      IO_WriteC(f, 'L');
    }
    break;
  default :
    break;
  }
}

static void CodeMAX
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  Defs_tObject Object;

  switch (Type->U_1.V_1.Kind) {
  case Defs_ShortInt:;
    IO_WriteS(f, (STRING)"MAX_SHORTINT", 12L);
    break;
  case Defs_LongInt:;
    IO_WriteS(f, (STRING)"MAX_LONGINT", 11L);
    break;
  case Defs_ShortCard:;
    IO_WriteS(f, (STRING)"MAX_SHORTCARD", 13L);
    break;
  case Defs_LongCard:;
    IO_WriteS(f, (STRING)"MAX_LONGCARD", 12L);
    break;
  case Defs_Real:;
    IO_WriteS(f, (STRING)"MAX_REAL", 8L);
    break;
  case Defs_LongReal:;
    IO_WriteS(f, (STRING)"MAX_LONGREAL", 12L);
    break;
  case Defs_Bool:;
    IO_WriteS(f, (STRING)"MAX_BOOLEAN", 11L);
    break;
  case Defs_Char:;
    IO_WriteS(f, (STRING)"MAX_CHAR", 8L);
    break;
  case Defs_Enumeration1:;
    {
      register Defs_yEnumeration1 *W_13 = &Type->U_1.V_39.Enumeration1;

      Object = Defs_GetLiteral(W_13->Objects, W_13->MaxValue);
    }
    Idents_WriteIdent(f, Object->U_1.V_6.EnumLiteral1.CIdent);
    break;
  case Defs_Subrange1:;
    CodeValue(Type->U_1.V_44.Subrange1.Upb);
    if (Defs_IsIntType(Type) && Types_Cast(Code_cPassValue, Type, Defs_TypeIntCard)) {
      IO_WriteC(f, 'L');
    }
    break;
  default :
    break;
  }
}

static void yyAbort
# ifdef __STDC__
(CHAR yyFunction[], LONGCARD O_1)
# else
(yyFunction, O_1)
CHAR yyFunction[];
LONGCARD O_1;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(yyFunction, O_1, CHAR)
  IO_WriteS((System_tFile)IO_StdError, (STRING)"Error: module Code, routine ", 28L);
  IO_WriteS((System_tFile)IO_StdError, yyFunction, O_1);
  IO_WriteS((System_tFile)IO_StdError, (STRING)" failed", 7L);
  IO_WriteNl((System_tFile)IO_StdError);
  (*Code_Exit)();
  FREE_OPEN_ARRAYS
}

static BOOLEAN yyIsEqual
# ifdef __STDC__
(BYTE yya[], LONGCARD O_3, BYTE yyb[], LONGCARD O_2)
# else
(yya, O_3, yyb, O_2)
BYTE yya[];
LONGCARD O_3;
BYTE yyb[];
LONGCARD O_2;
# endif
{
  INTEGER yyi;
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_2 * sizeof(WORD) + O_3 * sizeof(WORD), 2)
  COPY_OPEN_ARRAY(yyb, O_2, WORD)
  COPY_OPEN_ARRAY(yya, O_3, WORD)
  {
    LONGINT B_9 = 0, B_10 = (INTEGER)(O_3 - 1);

    if (B_9 <= B_10)
      for (yyi = B_9;; yyi += 1) {
        if (yya[yyi] != yyb[yyi]) {
          FREE_OPEN_ARRAYS
          return FALSE;
        }
        if (yyi >= B_10) break;
      }
  }
  FREE_OPEN_ARRAYS
  return TRUE;
}

void Code_CodeCompUnits
# ifdef __STDC__
(Tree_tTree yyP1)
# else
(yyP1)
Tree_tTree yyP1;
# endif
{
  struct S_7 yyTempo;

  if (yyP1->U_1.V_1.Kind == Tree_ROOT) {
    Code_CodeCompUnits(yyP1->U_1.V_3.ROOT.CompUnits);
    return;
  }
  if (yyP1->U_1.V_1.Kind == Tree_DefMod) {
    if (yyP1->U_1.V_7.DefMod.Next->U_1.V_1.Kind == Tree_CompUnits0) {
      f = Base_OpenHeader(yyP1->U_1.V_7.DefMod.Ident);
      IO_WriteS(f, (STRING)"#define DEFINITION_", 19L);
      Idents_WriteIdent(f, yyP1->U_1.V_7.DefMod.Ident);
      IO_WriteNl(f);
      IO_WriteNl(f);
      CodeImports(yyP1->U_1.V_7.DefMod.Import);
      CodeDefs1(yyP1->U_1.V_7.DefMod.Decls);
      IO_WriteS(f, (STRING)"extern void BEGIN_", 18L);
      Idents_WriteIdent(f, yyP1->U_1.V_7.DefMod.Ident);
      IO_WriteS(f, (STRING)"();", 3L);
      IO_WriteNl(f);
      IO_WriteClose(f);
      return;
    }
    if (Tree_IsType(yyP1->U_1.V_7.DefMod.Next, Tree_CompUnit)) {
      AssocTab_PutAssoc(yyP1->U_1.V_7.DefMod.Ident, (ADDRESS)yyP1);
      if (Base_OptionIsSet('i')) {
        f = Base_OpenHeader(yyP1->U_1.V_7.DefMod.Ident);
        IO_WriteS(f, (STRING)"#define DEFINITION_", 19L);
        Idents_WriteIdent(f, yyP1->U_1.V_7.DefMod.Ident);
        IO_WriteNl(f);
        IO_WriteNl(f);
        CodeImports(yyP1->U_1.V_7.DefMod.Import);
        CodeDefs1(yyP1->U_1.V_7.DefMod.Decls);
        IO_WriteS(f, (STRING)"extern void BEGIN_", 18L);
        Idents_WriteIdent(f, yyP1->U_1.V_7.DefMod.Ident);
        IO_WriteS(f, (STRING)"();", 3L);
        IO_WriteNl(f);
        IO_WriteClose(f);
      }
      Code_CodeCompUnits(yyP1->U_1.V_7.DefMod.Next);
      return;
    }
  }
  if (yyP1->U_1.V_1.Kind == Tree_ProgMod) {
    if (yyP1->U_1.V_8.ProgMod.Next->U_1.V_1.Kind == Tree_CompUnits0) {
      {
        register struct S_8 *W_14 = &yyTempo.U_1.V_1.yyR4;

        for (;;) {
          if (!(yyP1->U_1.V_8.ProgMod.Kind == Tree_Implementation)) {
            goto EXIT_3;
          }
          f = Base_OpenProgram(yyP1->U_1.V_8.ProgMod.Ident);
          AssocTab_GetAssoc(yyP1->U_1.V_8.ProgMod.Ident, (ADDRESS *)&W_14->DefMod);
          if (W_14->DefMod == Tree_NoTree) {
            Errors_CompilerError((STRING)"Code.CodeCompUnits", 18L);
          }
          IO_WriteS(f, (STRING)"#include \"SYSTEM_.h\"", 20L);
          IO_WriteNl(f);
          IO_WriteNl(f);
          CodeImports(yyP1->U_1.V_8.ProgMod.Import);
          IO_WriteS(f, (STRING)"#ifndef DEFINITION_", 19L);
          Idents_WriteIdent(f, yyP1->U_1.V_8.ProgMod.Ident);
          IO_WriteNl(f);
          IO_WriteS(f, (STRING)"#include \"", 10L);
          Idents_WriteIdent(f, yyP1->U_1.V_8.ProgMod.Ident);
          IO_WriteS(f, (STRING)".h\"", 3L);
          IO_WriteNl(f);
          IO_WriteS(f, (STRING)"#endif", 6L);
          IO_WriteNl(f);
          IO_WriteNl(f);
          CodeDefs2(W_14->DefMod->U_1.V_7.DefMod.Decls);
          IO_WriteNl(f);
          CodeDecls1(yyP1->U_1.V_8.ProgMod.Decls);
          IO_WriteNl(f);
          GenGlobalPtrs(yyP1->U_1.V_8.ProgMod.GlobalPtrs);
          GlobalPtrs = yyP1->U_1.V_8.ProgMod.GlobalPtrs;
          CodePointers(Static, yyP1->U_1.V_8.ProgMod.GlobalPtrs);
          IO_WriteNl(f);
          CodeProcs(yyP1->U_1.V_8.ProgMod.Decls);
          IO_WriteS(f, (STRING)"void BEGIN_", 11L);
          Idents_WriteIdent(f, yyP1->U_1.V_8.ProgMod.Ident);
          IO_WriteS(f, (STRING)"()", 2L);
          IO_WriteNl(f);
          IO_WriteC(f, '{');
          IO_WriteNl(f);
          IO_WriteS(f, (STRING)"  static BOOLEAN has_been_called = FALSE;", 41L);
          IO_WriteNl(f);
          IO_WriteNl(f);
          INC1(Indent, 2);
          CodeStrings(yyP1->U_1.V_8.ProgMod.Strings);
          if (yyP1->U_1.V_8.ProgMod.Strings != Defs_NoStrings) {
            IO_WriteNl(f);
          }
          IO_WriteS(f, (STRING)"  if (!has_been_called) {", 25L);
          IO_WriteNl(f);
          IO_WriteS(f, (STRING)"    has_been_called = TRUE;", 27L);
          IO_WriteNl(f);
          IO_WriteNl(f);
          INC1(Indent, 2);
          CodeInits(W_14->DefMod->U_1.V_7.DefMod.Import);
          CodeInits(yyP1->U_1.V_8.ProgMod.Import);
          CodeStrncpy(yyP1->U_1.V_8.ProgMod.Strings);
          CodeInits(yyP1->U_1.V_8.ProgMod.Decls);
          IO_WriteNl(f);
          CodeStmts(yyP1->U_1.V_8.ProgMod.Stmts);
          DEC1(Indent, 4);
          IO_WriteS(f, (STRING)"  }", 3L);
          IO_WriteNl(f);
          IO_WriteC(f, '}');
          IO_WriteNl(f);
          IO_WriteClose(f);
          return;
        } EXIT_3:;
      }
      for (;;) {
        if (!(yyP1->U_1.V_8.ProgMod.Kind == Tree_Program)) {
          goto EXIT_4;
        }
        f = Base_OpenProgram(yyP1->U_1.V_8.ProgMod.Ident);
        IO_WriteS(f, (STRING)"#include \"SYSTEM_.h\"", 20L);
        IO_WriteNl(f);
        IO_WriteNl(f);
        CodeImports(yyP1->U_1.V_8.ProgMod.Import);
        CodeDecls1(yyP1->U_1.V_8.ProgMod.Decls);
        IO_WriteNl(f);
        GenGlobalPtrs(yyP1->U_1.V_8.ProgMod.GlobalPtrs);
        GlobalPtrs = yyP1->U_1.V_8.ProgMod.GlobalPtrs;
        CodePointers(Static, yyP1->U_1.V_8.ProgMod.GlobalPtrs);
        IO_WriteNl(f);
        CodeProcs(yyP1->U_1.V_8.ProgMod.Decls);
        IO_WriteS(f, (STRING)"void BEGIN_MODULE()", 19L);
        IO_WriteNl(f);
        IO_WriteC(f, '{');
        IO_WriteNl(f);
        INC1(Indent, 2);
        CodeStrings(yyP1->U_1.V_8.ProgMod.Strings);
        if (yyP1->U_1.V_8.ProgMod.Strings != Defs_NoStrings) {
          IO_WriteNl(f);
        }
        CodeInits(yyP1->U_1.V_8.ProgMod.Import);
        CodeStrncpy(yyP1->U_1.V_8.ProgMod.Strings);
        CodeInits(yyP1->U_1.V_8.ProgMod.Decls);
        IO_WriteNl(f);
        CodeStmts(yyP1->U_1.V_8.ProgMod.Stmts);
        DEC1(Indent, 2);
        IO_WriteC(f, '}');
        IO_WriteNl(f);
        IO_WriteClose(f);
        return;
      } EXIT_4:;
    }
  }
}

static void CodeImports
# ifdef __STDC__
(Tree_tTree yyP2)
# else
(yyP2)
Tree_tTree yyP2;
# endif
{
  struct S_9 yyTempo;

  if (yyP2->U_1.V_1.Kind == Tree_From) {
    for (;;) {
      if (!(yyP2->U_1.V_12.From.Ident != Defs_IdentSYSTEM)) {
        goto EXIT_5;
      }
      IO_WriteS(f, (STRING)"#ifndef DEFINITION_", 19L);
      Idents_WriteIdent(f, yyP2->U_1.V_12.From.Ident);
      IO_WriteNl(f);
      IO_WriteS(f, (STRING)"#include \"", 10L);
      Idents_WriteIdent(f, yyP2->U_1.V_12.From.Ident);
      IO_WriteS(f, (STRING)".h\"", 3L);
      IO_WriteNl(f);
      IO_WriteS(f, (STRING)"#endif", 6L);
      IO_WriteNl(f);
      IO_WriteNl(f);
      CodeImports(yyP2->U_1.V_12.From.Next);
      return;
    } EXIT_5:;
    CodeImports(yyP2->U_1.V_12.From.Next);
    return;
  }
  if (yyP2->U_1.V_1.Kind == Tree_Objects) {
    CodeImports(yyP2->U_1.V_13.Objects.ImpIds);
    CodeImports(yyP2->U_1.V_13.Objects.Next);
    return;
  }
  if (yyP2->U_1.V_1.Kind == Tree_ImpIds1) {
    for (;;) {
      if (!(yyP2->U_1.V_16.ImpIds1.Ident != Defs_IdentSYSTEM)) {
        goto EXIT_6;
      }
      IO_WriteS(f, (STRING)"#ifndef DEFINITION_", 19L);
      Idents_WriteIdent(f, yyP2->U_1.V_16.ImpIds1.Ident);
      IO_WriteNl(f);
      IO_WriteS(f, (STRING)"#include \"", 10L);
      Idents_WriteIdent(f, yyP2->U_1.V_16.ImpIds1.Ident);
      IO_WriteS(f, (STRING)".h\"", 3L);
      IO_WriteNl(f);
      IO_WriteS(f, (STRING)"#endif", 6L);
      IO_WriteNl(f);
      IO_WriteNl(f);
      CodeImports(yyP2->U_1.V_16.ImpIds1.Next);
      return;
    } EXIT_6:;
    CodeImports(yyP2->U_1.V_16.ImpIds1.Next);
    return;
  }
}

static void CodeInits
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_10 yyTempo;

  if (t->U_1.V_1.Kind == Tree_From) {
    for (;;) {
      if (!(t->U_1.V_12.From.Ident != Defs_IdentSYSTEM)) {
        goto EXIT_7;
      }
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"BEGIN_", 6L);
      Idents_WriteIdent(f, t->U_1.V_12.From.Ident);
      IO_WriteS(f, (STRING)"();", 3L);
      IO_WriteNl(f);
      CodeInits(t->U_1.V_12.From.Next);
      return;
    } EXIT_7:;
    CodeInits(t->U_1.V_12.From.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Objects) {
    CodeInits(t->U_1.V_13.Objects.ImpIds);
    CodeInits(t->U_1.V_13.Objects.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_ImpIds1) {
    for (;;) {
      if (!(t->U_1.V_16.ImpIds1.Ident != Defs_IdentSYSTEM)) {
        goto EXIT_8;
      }
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"BEGIN_", 6L);
      Idents_WriteIdent(f, t->U_1.V_16.ImpIds1.Ident);
      IO_WriteS(f, (STRING)"();", 3L);
      IO_WriteNl(f);
      CodeInits(t->U_1.V_16.ImpIds1.Next);
      return;
    } EXIT_8:;
    CodeInits(t->U_1.V_16.ImpIds1.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Module) {
    Layout_WriteSpaces(f, (LONGINT)Indent);
    Idents_WriteIdent(f, t->U_1.V_32.Module.CIdent);
    IO_WriteS(f, (STRING)"();", 3L);
    IO_WriteNl(f);
    CodeInits(t->U_1.V_32.Module.Decls);
    CodeInits(t->U_1.V_32.Module.Next);
    return;
  }
  if (Tree_IsType(t, Tree_Decl)) {
    CodeInits(t->U_1.V_25.Decl.Next);
    return;
  }
}

static void CodeDefs1
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_11 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Var) {
    CodeEnums(t->U_1.V_26.Var.Type);
    IO_WriteS(f, (STRING)"extern ", 7L);
    CodeType(t->U_1.V_26.Var.Type, Full);
    IO_WriteC(f, ' ');
    CodeIds(t->U_1.V_26.Var.VarIds, t->U_1.V_26.Var.Type);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeDefs1(t->U_1.V_26.Var.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Const) {
    if (t->U_1.V_28.Const.Expr->U_1.V_1.Kind == Tree_StringConst) {
      for (;;) {
        if (!(StringMem_Length(t->U_1.V_28.Const.Expr->U_1.V_82.StringConst.StringVal) > 1)) {
          goto EXIT_9;
        }
        IO_WriteS(f, (STRING)"extern CHAR ", 12L);
        Idents_WriteIdent(f, t->U_1.V_28.Const.CIdent);
        IO_WriteS(f, (STRING)"[];", 3L);
        IO_WriteNl(f);
        CodeDefs1(t->U_1.V_28.Const.Next);
        return;
      } EXIT_9:;
    }
    for (;;) {
      if (!t->U_1.V_28.Const.Expr->U_1.V_77.Expr.IsCConst) {
        goto EXIT_10;
      }
      IO_WriteS(f, (STRING)"#define ", 8L);
      Idents_WriteIdent(f, t->U_1.V_28.Const.CIdent);
      IO_WriteC(f, '\t');
      CodeExpr(t->U_1.V_28.Const.Expr, pMaxPrec);
      IO_WriteNl(f);
      CodeDefs1(t->U_1.V_28.Const.Next);
      return;
    } EXIT_10:;
    IO_WriteS(f, (STRING)"#define ", 8L);
    Idents_WriteIdent(f, t->U_1.V_28.Const.CIdent);
    IO_WriteC(f, '\t');
    CodeValue(t->U_1.V_28.Const.Object->U_1.V_5.Const1.Value);
    IO_WriteNl(f);
    CodeDefs1(t->U_1.V_28.Const.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_TypeDecl) {
    CodeEnums(t->U_1.V_29.TypeDecl.Type);
    IO_WriteS(f, (STRING)"typedef ", 8L);
    CodeType(t->U_1.V_29.TypeDecl.Type, Full);
    IO_WriteC(f, ' ');
    CodeDecla(t->U_1.V_29.TypeDecl.Type, C_1_Before);
    Idents_WriteIdent(f, t->U_1.V_29.TypeDecl.CIdent);
    CodeDecla(t->U_1.V_29.TypeDecl.Type, C_2_After);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeDefs1(t->U_1.V_29.TypeDecl.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_ProcHead) {
    IO_WriteS(f, (STRING)"extern ", 7L);
    CodeType(t->U_1.V_31.ProcHead.ResultType, Full);
    IO_WriteC(f, ' ');
    Idents_WriteIdent(f, t->U_1.V_31.ProcHead.CIdent);
    IO_WriteS(f, (STRING)" ARGS((", 7L);
    CodeFormals3(t->U_1.V_31.ProcHead.Formals, Idents_NoIdent);
    IO_WriteS(f, (STRING)"));", 3L);
    IO_WriteNl(f);
    CodeDefs1(t->U_1.V_31.ProcHead.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Opaque) {
    IO_WriteS(f, (STRING)"typedef OPAQUE ", 15L);
    Idents_WriteIdent(f, t->U_1.V_33.Opaque.CIdent);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeDefs1(t->U_1.V_33.Opaque.Next);
    return;
  }
}

static void CodeDefs2
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_12 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Var) {
    CodeType(t->U_1.V_26.Var.Type, NonFull);
    IO_WriteC(f, ' ');
    CodeIds(t->U_1.V_26.Var.VarIds, t->U_1.V_26.Var.Type);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeDefs2(t->U_1.V_26.Var.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Const) {
    if (t->U_1.V_28.Const.Expr->U_1.V_1.Kind == Tree_StringConst) {
      for (;;) {
        if (!(StringMem_Length(t->U_1.V_28.Const.Expr->U_1.V_82.StringConst.StringVal) > 1)) {
          goto EXIT_11;
        }
        IO_WriteS(f, (STRING)"CHAR ", 5L);
        Idents_WriteIdent(f, t->U_1.V_28.Const.CIdent);
        IO_WriteS(f, (STRING)" [] = ", 6L);
        CodeString1(t->U_1.V_28.Const.Expr->U_1.V_82.StringConst.StringVal);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
        CodeDefs2(t->U_1.V_28.Const.Next);
        return;
      } EXIT_11:;
    }
  }
  if (Tree_IsType(t, Tree_Decl)) {
    CodeDefs2(t->U_1.V_25.Decl.Next);
    return;
  }
}

static void CodeDecls1
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_13 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Var) {
    for (;;) {
      if (!t->U_1.V_26.Var.IsGlobal) {
        goto EXIT_12;
      }
      CodeEnums(t->U_1.V_26.Var.Type);
      IO_WriteS(f, (STRING)"static ", 7L);
      CodeType(t->U_1.V_26.Var.Type, Full);
      IO_WriteC(f, ' ');
      CodeIds(t->U_1.V_26.Var.VarIds, t->U_1.V_26.Var.Type);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      CodeDecls1(t->U_1.V_26.Var.Next);
      return;
    } EXIT_12:;
    for (;;) {
      if (!!t->U_1.V_26.Var.IsGlobal) {
        goto EXIT_13;
      }
      CodeEnums(t->U_1.V_26.Var.Type);
      CodeStruct(t->U_1.V_26.Var.Type);
      CodeDecls1(t->U_1.V_26.Var.Next);
      return;
    } EXIT_13:;
  }
  if (t->U_1.V_1.Kind == Tree_Const) {
    if (t->U_1.V_28.Const.Expr->U_1.V_1.Kind == Tree_StringConst) {
      for (;;) {
        if (!(StringMem_Length(t->U_1.V_28.Const.Expr->U_1.V_82.StringConst.StringVal) > 1)) {
          goto EXIT_14;
        }
        IO_WriteS(f, (STRING)"static CHAR ", 12L);
        Idents_WriteIdent(f, t->U_1.V_28.Const.CIdent);
        IO_WriteS(f, (STRING)" [] = ", 6L);
        CodeString1(t->U_1.V_28.Const.Expr->U_1.V_82.StringConst.StringVal);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
        CodeDecls1(t->U_1.V_28.Const.Next);
        return;
      } EXIT_14:;
    }
    for (;;) {
      if (!t->U_1.V_28.Const.Expr->U_1.V_77.Expr.IsCConst) {
        goto EXIT_15;
      }
      IO_WriteS(f, (STRING)"#define ", 8L);
      Idents_WriteIdent(f, t->U_1.V_28.Const.CIdent);
      IO_WriteC(f, '\t');
      CodeExpr(t->U_1.V_28.Const.Expr, pMaxPrec);
      IO_WriteNl(f);
      CodeDecls1(t->U_1.V_28.Const.Next);
      return;
    } EXIT_15:;
    IO_WriteS(f, (STRING)"#define ", 8L);
    Idents_WriteIdent(f, t->U_1.V_28.Const.CIdent);
    IO_WriteC(f, '\t');
    CodeValue(t->U_1.V_28.Const.Object->U_1.V_5.Const1.Value);
    IO_WriteNl(f);
    CodeDecls1(t->U_1.V_28.Const.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_TypeDecl) {
    CodeEnums(t->U_1.V_29.TypeDecl.Type);
    IO_WriteS(f, (STRING)"typedef ", 8L);
    CodeType(t->U_1.V_29.TypeDecl.Type, Full);
    IO_WriteC(f, ' ');
    CodeDecla(t->U_1.V_29.TypeDecl.Type, C_1_Before);
    Idents_WriteIdent(f, t->U_1.V_29.TypeDecl.CIdent);
    CodeDecla(t->U_1.V_29.TypeDecl.Type, C_2_After);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeDecls1(t->U_1.V_29.TypeDecl.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Proc) {
    for (;;) {
      if (!!Defs_IsExported(t->U_1.V_30.Proc.Object)) {
        goto EXIT_16;
      }
      IO_WriteS(f, (STRING)"static ", 7L);
      CodeType(t->U_1.V_30.Proc.ResultType, Full);
      IO_WriteC(f, ' ');
      Idents_WriteIdent(f, t->U_1.V_30.Proc.CIdent);
      IO_WriteS(f, (STRING)" ARGS((", 7L);
      CodeFormals3(t->U_1.V_30.Proc.Formals, Idents_NoIdent);
      IO_WriteS(f, (STRING)"));", 3L);
      IO_WriteNl(f);
      CodeDecls1(t->U_1.V_30.Proc.Decls);
      CodeDecls1(t->U_1.V_30.Proc.Next);
      return;
    } EXIT_16:;
    CodeDecls1(t->U_1.V_30.Proc.Decls);
    CodeDecls1(t->U_1.V_30.Proc.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Module) {
    CodeDecls1(t->U_1.V_32.Module.Decls);
    CodeDecls1(t->U_1.V_32.Module.Next);
    return;
  }
}

static void CodeType
# ifdef __STDC__
(Tree_tTree t, BOOLEAN C_4_Full)
# else
(t, C_4_Full)
Tree_tTree t;
BOOLEAN C_4_Full;
# endif
{
  struct S_14 yyTempo;

  switch (t->U_1.V_1.Kind) {
  case Tree_Array:;
    {
      register struct S_17 *W_15 = &yyTempo.U_1.V_1.yyR1;

      for (;;) {
        if (!(t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2->U_1.V_1.Kind == Defs_Subrange1)) {
          goto EXIT_17;
        }
        if (C_4_Full) {
          Types_GetLwb(t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2, &W_15->Lwb);
          W_15->LwbExpr = (Tree_tTree)t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2->U_1.V_44.Subrange1.LwbExpr;
          W_15->UpbExpr = (Tree_tTree)t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2->U_1.V_44.Subrange1.UpbExpr;
          IO_WriteS(f, (STRING)"struct ", 7L);
          Idents_WriteIdent(f, t->U_1.V_44.Array.Type2->U_1.V_38.Array1.StructId);
          IO_WriteS(f, (STRING)" {", 2L);
          IO_WriteNl(f);
          INC1(Indent, 4);
          Layout_WriteSpaces(f, (LONGINT)Indent);
          CodeType(t->U_1.V_44.Array.ElemType, C_4_Full);
          IO_WriteC(f, ' ');
          CodeDecla(t->U_1.V_44.Array.ElemType, C_1_Before);
          IO_WriteS(f, (STRING)"A[", 2L);
          if (W_15->UpbExpr->U_1.V_77.Expr.IsCConst) {
            if (Values_ValueToInt(W_15->Lwb) != 0) {
              CodeExpr(W_15->UpbExpr, pMinus + 1);
            } else {
              CodeExpr(W_15->UpbExpr, pPlus + 1);
            }
          } else {
            CodeValue(t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2->U_1.V_44.Subrange1.Upb);
          }
          if (Values_ValueToInt(W_15->Lwb) != 0) {
            IO_WriteS(f, (STRING)" - ", 3L);
            if (W_15->LwbExpr->U_1.V_77.Expr.IsCConst) {
              CodeExpr(W_15->LwbExpr, pMinus);
            } else {
              CodeValue(W_15->Lwb);
            }
          }
          IO_WriteS(f, (STRING)" + 1", 4L);
          IO_WriteC(f, ']');
          CodeDecla(t->U_1.V_44.Array.ElemType, C_2_After);
          IO_WriteC(f, ';');
          IO_WriteNl(f);
          DEC1(Indent, 4);
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteC(f, '}');
        } else {
          IO_WriteS(f, (STRING)"struct ", 7L);
          Idents_WriteIdent(f, t->U_1.V_44.Array.Type2->U_1.V_38.Array1.StructId);
        }
        return;
      } EXIT_17:;
    }
    {
      register struct S_16 *W_16 = &yyTempo.U_1.V_2.yyR2;

      for (;;) {
        if (!(t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2->U_1.V_1.Kind == Defs_Enumeration1)) {
          goto EXIT_18;
        }
        if (C_4_Full) {
          Types_GetUpb(t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2, &W_16->Upb);
          IO_WriteS(f, (STRING)"struct ", 7L);
          Idents_WriteIdent(f, t->U_1.V_44.Array.Type2->U_1.V_38.Array1.StructId);
          IO_WriteS(f, (STRING)" {", 2L);
          IO_WriteNl(f);
          INC1(Indent, 4);
          Layout_WriteSpaces(f, (LONGINT)Indent);
          CodeType(t->U_1.V_44.Array.ElemType, C_4_Full);
          IO_WriteC(f, ' ');
          CodeDecla(t->U_1.V_44.Array.ElemType, C_1_Before);
          IO_WriteS(f, (STRING)"A[", 2L);
          CodeValue(W_16->Upb);
          IO_WriteS(f, (STRING)" + 1]", 5L);
          CodeDecla(t->U_1.V_44.Array.ElemType, C_2_After);
          IO_WriteC(f, ';');
          IO_WriteNl(f);
          DEC1(Indent, 4);
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteC(f, '}');
        } else {
          IO_WriteS(f, (STRING)"struct ", 7L);
          Idents_WriteIdent(f, t->U_1.V_44.Array.Type2->U_1.V_38.Array1.StructId);
        }
        return;
      } EXIT_18:;
    }
    {
      register struct S_15 *W_17 = &yyTempo.U_1.V_3.yyR3;

      if (C_4_Full) {
        Types_GetLwb(t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2, &W_17->Lwb);
        Types_GetUpb(t->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2, &W_17->Upb);
        IO_WriteS(f, (STRING)"struct ", 7L);
        Idents_WriteIdent(f, t->U_1.V_44.Array.Type2->U_1.V_38.Array1.StructId);
        IO_WriteS(f, (STRING)" {", 2L);
        IO_WriteNl(f);
        INC1(Indent, 4);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        CodeType(t->U_1.V_44.Array.ElemType, C_4_Full);
        IO_WriteC(f, ' ');
        CodeDecla(t->U_1.V_44.Array.ElemType, C_1_Before);
        IO_WriteS(f, (STRING)"A[", 2L);
        IO_WriteI(f, Values_ValueToInt(W_17->Upb) - Values_ValueToInt(W_17->Lwb) + 1, 1L);
        IO_WriteC(f, ']');
        CodeDecla(t->U_1.V_44.Array.ElemType, C_2_After);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
        DEC1(Indent, 4);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteC(f, '}');
      } else {
        IO_WriteS(f, (STRING)"struct ", 7L);
        Idents_WriteIdent(f, t->U_1.V_44.Array.Type2->U_1.V_38.Array1.StructId);
      }
      return;
    }
    break;
  case Tree_Record:;
    if (t->U_1.V_45.Record.Fields->U_1.V_1.Kind == Tree_Fields0) {
      if (C_4_Full) {
        IO_WriteS(f, (STRING)"struct ", 7L);
        Idents_WriteIdent(f, t->U_1.V_45.Record.Type2->U_1.V_42.Record1.StructId);
        IO_WriteS(f, (STRING)" {", 2L);
        IO_WriteNl(f);
        INC1(Indent, 4);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteS(f, (STRING)"char dummy;", 11L);
        IO_WriteNl(f);
        DEC1(Indent, 4);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteC(f, '}');
      } else {
        IO_WriteS(f, (STRING)"struct ", 7L);
        Idents_WriteIdent(f, t->U_1.V_45.Record.Type2->U_1.V_42.Record1.StructId);
      }
      return;
    }
    if (Tree_IsType(t->U_1.V_45.Record.Fields, Tree_Fields1)) {
      if (C_4_Full) {
        IO_WriteS(f, (STRING)"struct ", 7L);
        Idents_WriteIdent(f, t->U_1.V_45.Record.Type2->U_1.V_42.Record1.StructId);
        IO_WriteS(f, (STRING)" {", 2L);
        IO_WriteNl(f);
        INC1(Indent, 4);
        CodeType(t->U_1.V_45.Record.Fields, C_4_Full);
        DEC1(Indent, 4);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteC(f, '}');
      } else {
        IO_WriteS(f, (STRING)"struct ", 7L);
        Idents_WriteIdent(f, t->U_1.V_45.Record.Type2->U_1.V_42.Record1.StructId);
      }
      return;
    }
    break;
  case Tree_SetType:;
    IO_WriteS(f, (STRING)"unsigned long", 13L);
    return;
    break;
  case Tree_Pointer:;
    if (t->U_1.V_47.Pointer.TargetType->U_1.V_1.Kind == Tree_TypeId0) {
      for (;;) {
        if (!Defs_IsForward(t->U_1.V_47.Pointer.TypePos, t->U_1.V_47.Pointer.TargetType->U_1.V_55.TypeId0.Object)) {
          goto EXIT_19;
        }
        switch (t->U_1.V_47.Pointer.TargetType->U_1.V_55.TypeId0.Type2->U_1.V_1.Kind) {
        case Defs_Record1:;
          IO_WriteS(f, (STRING)"struct ", 7L);
          Idents_WriteIdent(f, t->U_1.V_47.Pointer.TargetType->U_1.V_55.TypeId0.Type2->U_1.V_42.Record1.StructId);
          break;
        case Defs_Array1:;
          IO_WriteS(f, (STRING)"struct ", 7L);
          Idents_WriteIdent(f, t->U_1.V_47.Pointer.TargetType->U_1.V_55.TypeId0.Type2->U_1.V_38.Array1.StructId);
          break;
        default :
          Idents_WriteIdent(f, t->U_1.V_47.Pointer.TargetType->U_1.V_55.TypeId0.Object->U_1.V_3.Object.CIdent);
          Errors_ErrorMessageP((LONGCARD)Errors_StructTypeReq, (LONGCARD)Errors_Restriction, t->U_1.V_47.Pointer.TargetType->U_1.V_55.TypeId0.Pos);
          break;
        }
        return;
      } EXIT_19:;
    }
    CodeType(t->U_1.V_47.Pointer.TargetType, C_4_Full);
    return;
    break;
  case Tree_ProcType:;
    CodeType(t->U_1.V_48.ProcType.ResultType, C_4_Full);
    return;
    break;
  case Tree_Enumeration:;
    if (Types_TypeSize(t->U_1.V_50.Enumeration.Type2) == Types_SizeUnsignedChar) {
      IO_WriteS(f, (STRING)"unsigned char", 13L);
    } else {
      IO_WriteS(f, (STRING)"unsigned short", 14L);
    }
    return;
    break;
  case Tree_Subrange:;
    if (t->U_1.V_51.Subrange.BaseType->U_1.V_1.Kind == Tree_Void) {
      CodeTypeName(t->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Type);
      return;
    }
    if (Tree_IsType(t->U_1.V_51.Subrange.BaseType, Tree_TypeId)) {
      Idents_WriteIdent(f, t->U_1.V_51.Subrange.BaseType->U_1.V_54.TypeId.Object->U_1.V_3.Object.CIdent);
      return;
    }
    break;
  case Tree_Void:;
    IO_WriteS(f, (STRING)"void", 4L);
    return;
    break;
  case Tree_TypeId:;
  case Tree_TypeId0:;
  case Tree_TypeId1:;
    Idents_WriteIdent(f, t->U_1.V_54.TypeId.Object->U_1.V_3.Object.CIdent);
    return;
    break;
  case Tree_RecordSect:;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    CodeType(t->U_1.V_60.RecordSect.Type, C_4_Full);
    IO_WriteC(f, ' ');
    CodeIds(t->U_1.V_60.RecordSect.FieldIds, t->U_1.V_60.RecordSect.Type);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeType(t->U_1.V_60.RecordSect.Next, C_4_Full);
    return;
    break;
  case Tree_VariantSect:;
    if (t->U_1.V_61.VariantSect.Variants->U_1.V_1.Kind == Tree_Variants0) {
      if (t->U_1.V_61.VariantSect.Else->U_1.V_1.Kind == Tree_Fields0) {
        CodeType(t->U_1.V_61.VariantSect.TagField, C_4_Full);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteS(f, (STRING)"union {", 7L);
        IO_WriteNl(f);
        INC1(Indent, 4);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteS(f, (STRING)"char dummy;", 11L);
        IO_WriteNl(f);
        DEC1(Indent, 4);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteS(f, (STRING)"} ", 2L);
        Idents_WriteIdent(f, t->U_1.V_61.VariantSect.UnionId);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
        CodeType(t->U_1.V_61.VariantSect.Next, C_4_Full);
        return;
      }
    }
    if (t->U_1.V_61.VariantSect.Else->U_1.V_1.Kind == Tree_Fields0) {
      CodeType(t->U_1.V_61.VariantSect.TagField, C_4_Full);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"union {", 7L);
      IO_WriteNl(f);
      INC1(Indent, 4);
      CodeType(t->U_1.V_61.VariantSect.Variants, C_4_Full);
      DEC1(Indent, 4);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"} ", 2L);
      Idents_WriteIdent(f, t->U_1.V_61.VariantSect.UnionId);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      CodeType(t->U_1.V_61.VariantSect.Next, C_4_Full);
      return;
    }
    CodeType(t->U_1.V_61.VariantSect.TagField, C_4_Full);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"union {", 7L);
    IO_WriteNl(f);
    INC1(Indent, 4);
    CodeType(t->U_1.V_61.VariantSect.Variants, C_4_Full);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"struct {", 8L);
    IO_WriteNl(f);
    INC1(Indent, 4);
    CodeType(t->U_1.V_61.VariantSect.Else, C_4_Full);
    DEC1(Indent, 4);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"} ", 2L);
    Idents_WriteIdent(f, t->U_1.V_61.VariantSect.ElseId);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    DEC1(Indent, 4);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"} ", 2L);
    Idents_WriteIdent(f, t->U_1.V_61.VariantSect.UnionId);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeType(t->U_1.V_61.VariantSect.Next, C_4_Full);
    return;
    break;
  case Tree_TagField1:;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    Idents_WriteIdent(f, t->U_1.V_67.TagField1.Type->U_1.V_54.TypeId.Object->U_1.V_3.Object.CIdent);
    IO_WriteC(f, ' ');
    Idents_WriteIdent(f, t->U_1.V_67.TagField1.CIdent);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    return;
    break;
  case Tree_Variant:;
    if (t->U_1.V_70.Variant.Variant->U_1.V_1.Kind == Tree_Fields0) {
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"struct {", 8L);
      IO_WriteNl(f);
      INC1(Indent, 4);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"char dummy;", 11L);
      IO_WriteNl(f);
      DEC1(Indent, 4);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"} ", 2L);
      Idents_WriteIdent(f, t->U_1.V_70.Variant.StructId);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      CodeType(t->U_1.V_70.Variant.Next, C_4_Full);
      return;
    }
    if (Tree_IsType(t->U_1.V_70.Variant.Variant, Tree_Fields1)) {
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"struct {", 8L);
      IO_WriteNl(f);
      INC1(Indent, 4);
      CodeType(t->U_1.V_70.Variant.Variant, C_4_Full);
      DEC1(Indent, 4);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"} ", 2L);
      Idents_WriteIdent(f, t->U_1.V_70.Variant.StructId);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      CodeType(t->U_1.V_70.Variant.Next, C_4_Full);
      return;
    }
    break;
  default :
    break;
  }
}

static void CodeDecla
# ifdef __STDC__
(Tree_tTree yyP3, BOOLEAN Before)
# else
(yyP3, Before)
Tree_tTree yyP3;
BOOLEAN Before;
# endif
{
  struct S_18 yyTempo;

  if (yyP3->U_1.V_1.Kind == Tree_Pointer) {
    CodeDecla(yyP3->U_1.V_47.Pointer.TargetType, Before);
    if (Before) {
      IO_WriteC(f, '*');
    }
    return;
  }
  if (yyP3->U_1.V_1.Kind == Tree_ProcType) {
    if (Before) {
      IO_WriteS(f, (STRING)"(*", 2L);
    } else {
      IO_WriteS(f, (STRING)") ARGS((", 8L);
      CodeFormals4(yyP3->U_1.V_48.ProcType.FormalTypes);
      IO_WriteS(f, (STRING)"))", 2L);
    }
    return;
  }
}

static void CodeStruct
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_19 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Array) {
    CodeType(t, Full);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Record) {
    CodeType(t, Full);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Pointer) {
    CodeStruct(t->U_1.V_47.Pointer.TargetType);
    return;
  }
}

static void CodeEnums
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_20 yyTempo;

  switch (t->U_1.V_1.Kind) {
  case Tree_Array:;
    CodeEnums(t->U_1.V_44.Array.IndexType);
    CodeEnums(t->U_1.V_44.Array.ElemType);
    return;
    break;
  case Tree_Record:;
    CodeEnums(t->U_1.V_45.Record.Fields);
    return;
    break;
  case Tree_SetType:;
    CodeEnums(t->U_1.V_46.SetType.BaseType);
    return;
    break;
  case Tree_Pointer:;
    CodeEnums(t->U_1.V_47.Pointer.TargetType);
    return;
    break;
  case Tree_Enumeration:;
    CodeEnums(t->U_1.V_50.Enumeration.EnumIds);
    return;
    break;
  case Tree_RecordSect:;
    CodeEnums(t->U_1.V_60.RecordSect.Type);
    return;
    break;
  case Tree_VariantSect:;
    CodeEnums(t->U_1.V_61.VariantSect.Variants);
    CodeEnums(t->U_1.V_61.VariantSect.Else);
    CodeEnums(t->U_1.V_61.VariantSect.Next);
    return;
    break;
  case Tree_Variant:;
    CodeEnums(t->U_1.V_70.Variant.Variant);
    CodeEnums(t->U_1.V_70.Variant.Next);
    return;
    break;
  case Tree_EnumIds1:;
    IO_WriteS(f, (STRING)"#define ", 8L);
    Idents_WriteIdent(f, t->U_1.V_76.EnumIds1.CIdent);
    IO_WriteC(f, '\t');
    IO_WriteI(f, (LONGINT)t->U_1.V_76.EnumIds1.Object->U_1.V_6.EnumLiteral1.Index, 1L);
    IO_WriteNl(f);
    CodeEnums(t->U_1.V_76.EnumIds1.Next);
    return;
    break;
  default :
    break;
  }
}

static void CodeIds
# ifdef __STDC__
(Tree_tTree t, Tree_tTree Type)
# else
(t, Type)
Tree_tTree t;
Tree_tTree Type;
# endif
{
  struct S_21 yyTempo;

  if (t->U_1.V_1.Kind == Tree_VarIds1) {
    CodeDecla(Type, C_1_Before);
    Idents_WriteIdent(f, t->U_1.V_36.VarIds1.CIdent);
    CodeDecla(Type, C_2_After);
    CodeSepa(t->U_1.V_36.VarIds1.Next);
    CodeIds(t->U_1.V_36.VarIds1.Next, Type);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_FieldIds1) {
    CodeDecla(Type, C_1_Before);
    Idents_WriteIdent(f, t->U_1.V_64.FieldIds1.CIdent);
    CodeDecla(Type, C_2_After);
    CodeSepa(t->U_1.V_64.FieldIds1.Next);
    CodeIds(t->U_1.V_64.FieldIds1.Next, Type);
    return;
  }
}

static void CodeProcs
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_22 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Proc) {
    {
      register struct S_23 *W_18 = &yyTempo.U_1.V_1.yyR1;

      CodeProcs(t->U_1.V_30.Proc.Decls);
      GenParams(t->U_1.V_30.Proc.VAROpens);
      GenParams(t->U_1.V_30.Proc.ValueOpens);
      GenLocalPtrs(t->U_1.V_30.Proc.LocalPtrs);
      VAROpens = t->U_1.V_30.Proc.VAROpens;
      ValueOpens = t->U_1.V_30.Proc.ValueOpens;
      LocalPtrs = t->U_1.V_30.Proc.LocalPtrs;
      if (!Defs_IsExported(t->U_1.V_30.Proc.Object)) {
        IO_WriteS(f, (STRING)"static ", 7L);
      }
      CodeType(t->U_1.V_30.Proc.ResultType, Full);
      IO_WriteC(f, ' ');
      Idents_WriteIdent(f, t->U_1.V_30.Proc.CIdent);
      IO_WriteNl(f);
      IO_WriteS(f, (STRING)"# ifdef __STDC__", 16L);
      IO_WriteNl(f);
      IO_WriteC(f, '(');
      CodeFormals3(t->U_1.V_30.Proc.Formals, Idents_NoIdent);
      IO_WriteC(f, ')');
      IO_WriteNl(f);
      IO_WriteS(f, (STRING)"# else", 6L);
      IO_WriteNl(f);
      IO_WriteC(f, '(');
      CodeFormals1(t->U_1.V_30.Proc.Formals);
      IO_WriteC(f, ')');
      IO_WriteNl(f);
      CodeFormals2(t->U_1.V_30.Proc.Formals, Idents_NoIdent);
      IO_WriteS(f, (STRING)"# endif", 7L);
      IO_WriteNl(f);
      IO_WriteC(f, '{');
      IO_WriteNl(f);
      W_18->NewLine = t->U_1.V_30.Proc.ValueOpens != Defs_NoCObjects || t->U_1.V_30.Proc.LocalPtrs != Defs_NoCObjects || t->U_1.V_30.Proc.Strings != Defs_NoStrings;
      INC1(Indent, 2);
      CodeDecls2(t->U_1.V_30.Proc.Decls, &W_18->NewLine);
      if (t->U_1.V_30.Proc.ValueOpens != Defs_NoCObjects) {
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteS(f, (STRING)"OPEN_ARRAY_LOCALS", 17L);
        IO_WriteNl(f);
      }
      CodePointers(Auto, t->U_1.V_30.Proc.LocalPtrs);
      CodeStrings(t->U_1.V_30.Proc.Strings);
      if (W_18->NewLine) {
        IO_WriteNl(f);
      }
      CodeAlloc(t->U_1.V_30.Proc.ValueOpens);
      CodeCopy(t->U_1.V_30.Proc.ValueOpens);
      CodePush(t->U_1.V_30.Proc.LocalPtrs);
      CodeStrncpy(t->U_1.V_30.Proc.Strings);
      CodeInits(t->U_1.V_30.Proc.Decls);
      CodeStmts(t->U_1.V_30.Proc.Stmts);
      if (Defs_GetResultType(Defs_GetType(t->U_1.V_30.Proc.Object)) == Defs_TypeVOID) {
        CodePop(t->U_1.V_30.Proc.LocalPtrs);
        CodeFree(t->U_1.V_30.Proc.ValueOpens);
      } else {
        if (Base_OptionIsSet('r')) {
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteS(f, (STRING)"ReturnError(__FILE__, __LINE__);", 32L);
          IO_WriteNl(f);
        }
      }
      DEC1(Indent, 2);
      IO_WriteC(f, '}');
      IO_WriteNl(f);
      IO_WriteNl(f);
      VAROpens = Defs_NoCObjects;
      ValueOpens = Defs_NoCObjects;
      LocalPtrs = Defs_NoCObjects;
      CodeProcs(t->U_1.V_30.Proc.Next);
      return;
    }
  }
  if (t->U_1.V_1.Kind == Tree_Module) {
    CodeProcs(t->U_1.V_32.Module.Decls);
    IO_WriteS(f, (STRING)"static void ", 12L);
    Idents_WriteIdent(f, t->U_1.V_32.Module.CIdent);
    IO_WriteS(f, (STRING)"()", 2L);
    IO_WriteNl(f);
    IO_WriteC(f, '{');
    IO_WriteNl(f);
    INC1(Indent, 2);
    CodeStrings(t->U_1.V_32.Module.Strings);
    if (t->U_1.V_32.Module.Strings != Defs_NoStrings) {
      IO_WriteNl(f);
    }
    CodeStrncpy(t->U_1.V_32.Module.Strings);
    CodeStmts(t->U_1.V_32.Module.Stmts);
    DEC1(Indent, 2);
    IO_WriteC(f, '}');
    IO_WriteNl(f);
    IO_WriteNl(f);
    CodeProcs(t->U_1.V_32.Module.Next);
    return;
  }
  if (Tree_IsType(t, Tree_Decl)) {
    CodeProcs(t->U_1.V_25.Decl.Next);
    return;
  }
}

static void CodeFormals1
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_24 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Formals1) {
    CodeFormals1(t->U_1.V_39.Formals1.ParIds);
    CodeSepa(t->U_1.V_39.Formals1.Next);
    CodeFormals1(t->U_1.V_39.Formals1.Next);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_ParIds1) {
    for (;;) {
      if (!Defs_IsOpenArray(t->U_1.V_42.ParIds1.Object)) {
        goto EXIT_20;
      }
      Idents_WriteIdent(f, t->U_1.V_42.ParIds1.CIdent);
      IO_WriteS(f, (STRING)", ", 2L);
      Idents_WriteIdent(f, GetParam(t->U_1.V_42.ParIds1.Object));
      CodeSepa(t->U_1.V_42.ParIds1.Next);
      CodeFormals1(t->U_1.V_42.ParIds1.Next);
      return;
    } EXIT_20:;
    Idents_WriteIdent(f, t->U_1.V_42.ParIds1.CIdent);
    CodeSepa(t->U_1.V_42.ParIds1.Next);
    CodeFormals1(t->U_1.V_42.ParIds1.Next);
    return;
  }
}

static void CodeFormals2
# ifdef __STDC__
(Tree_tTree t, Idents_tIdent ElemTypeId)
# else
(t, ElemTypeId)
Tree_tTree t;
Idents_tIdent ElemTypeId;
# endif
{
  struct S_25 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Formals1) {
    if (Tree_IsType(t->U_1.V_39.Formals1.Type, Tree_TypeId)) {
      Idents_WriteIdent(f, t->U_1.V_39.Formals1.Type->U_1.V_54.TypeId.Object->U_1.V_3.Object.CIdent);
      IO_WriteC(f, ' ');
      CodeFormals2(t->U_1.V_39.Formals1.ParIds, Idents_NoIdent);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      CodeFormals2(t->U_1.V_39.Formals1.Next, Idents_NoIdent);
      return;
    }
    if (t->U_1.V_39.Formals1.Type->U_1.V_1.Kind == Tree_Array) {
      if (t->U_1.V_39.Formals1.Type->U_1.V_44.Array.IsOpen == TRUE) {
        if (Tree_IsType(t->U_1.V_39.Formals1.Type->U_1.V_44.Array.ElemType, Tree_TypeId)) {
          CodeFormals2(t->U_1.V_39.Formals1.ParIds, t->U_1.V_39.Formals1.Type->U_1.V_44.Array.ElemType->U_1.V_54.TypeId.Object->U_1.V_3.Object.CIdent);
          CodeFormals2(t->U_1.V_39.Formals1.Next, Idents_NoIdent);
          return;
        }
      }
    }
  }
  if (t->U_1.V_1.Kind == Tree_ParIds1) {
    for (;;) {
      if (!Defs_IsOpenArray(t->U_1.V_42.ParIds1.Object)) {
        goto EXIT_21;
      }
      Idents_WriteIdent(f, ElemTypeId);
      IO_WriteC(f, ' ');
      Idents_WriteIdent(f, t->U_1.V_42.ParIds1.CIdent);
      IO_WriteS(f, (STRING)"[];", 3L);
      IO_WriteNl(f);
      IO_WriteS(f, (STRING)"LONGCARD ", 9L);
      Idents_WriteIdent(f, GetParam(t->U_1.V_42.ParIds1.Object));
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      CodeFormals2(t->U_1.V_42.ParIds1.Next, ElemTypeId);
      return;
    } EXIT_21:;
    if (Defs_IsVAR(t->U_1.V_42.ParIds1.Object)) {
      IO_WriteC(f, '*');
    }
    Idents_WriteIdent(f, t->U_1.V_42.ParIds1.CIdent);
    CodeSepa(t->U_1.V_42.ParIds1.Next);
    CodeFormals2(t->U_1.V_42.ParIds1.Next, Idents_NoIdent);
    return;
  }
}

static void CodeFormals3
# ifdef __STDC__
(Tree_tTree t, Idents_tIdent TypeId)
# else
(t, TypeId)
Tree_tTree t;
Idents_tIdent TypeId;
# endif
{
  struct S_26 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Formals1) {
    if (Tree_IsType(t->U_1.V_39.Formals1.Type, Tree_TypeId)) {
      CodeFormals3(t->U_1.V_39.Formals1.ParIds, t->U_1.V_39.Formals1.Type->U_1.V_54.TypeId.Object->U_1.V_3.Object.CIdent);
      CodeSepa(t->U_1.V_39.Formals1.Next);
      CodeFormals3(t->U_1.V_39.Formals1.Next, Idents_NoIdent);
      return;
    }
    if (t->U_1.V_39.Formals1.Type->U_1.V_1.Kind == Tree_Array) {
      if (t->U_1.V_39.Formals1.Type->U_1.V_44.Array.IsOpen == TRUE) {
        if (Tree_IsType(t->U_1.V_39.Formals1.Type->U_1.V_44.Array.ElemType, Tree_TypeId)) {
          CodeFormals3(t->U_1.V_39.Formals1.ParIds, t->U_1.V_39.Formals1.Type->U_1.V_44.Array.ElemType->U_1.V_54.TypeId.Object->U_1.V_3.Object.CIdent);
          CodeSepa(t->U_1.V_39.Formals1.Next);
          CodeFormals3(t->U_1.V_39.Formals1.Next, Idents_NoIdent);
          return;
        }
      }
    }
  }
  if (t->U_1.V_1.Kind == Tree_ParIds1) {
    for (;;) {
      if (!Defs_IsOpenArray(t->U_1.V_42.ParIds1.Object)) {
        goto EXIT_22;
      }
      Idents_WriteIdent(f, TypeId);
      IO_WriteC(f, ' ');
      Idents_WriteIdent(f, t->U_1.V_42.ParIds1.CIdent);
      IO_WriteS(f, (STRING)"[], ", 4L);
      IO_WriteS(f, (STRING)"LONGCARD ", 9L);
      Idents_WriteIdent(f, GetParam(t->U_1.V_42.ParIds1.Object));
      CodeSepa(t->U_1.V_42.ParIds1.Next);
      CodeFormals3(t->U_1.V_42.ParIds1.Next, TypeId);
      return;
    } EXIT_22:;
    Idents_WriteIdent(f, TypeId);
    if (Defs_IsVAR(t->U_1.V_42.ParIds1.Object)) {
      IO_WriteS(f, (STRING)" *", 2L);
    } else {
      IO_WriteC(f, ' ');
    }
    Idents_WriteIdent(f, t->U_1.V_42.ParIds1.CIdent);
    CodeSepa(t->U_1.V_42.ParIds1.Next);
    CodeFormals3(t->U_1.V_42.ParIds1.Next, TypeId);
    return;
  }
}

static void CodeFormals4
# ifdef __STDC__
(Tree_tTree yyP4)
# else
(yyP4)
Tree_tTree yyP4;
# endif
{
  struct S_27 yyTempo;

  if (yyP4->U_1.V_1.Kind == Tree_FormalType) {
    CodeType(yyP4->U_1.V_73.FormalType.Type, Full);
    if (yyP4->U_1.V_73.FormalType.IsVAR) {
      IO_WriteS(f, (STRING)" *", 2L);
    }
    CodeSepa(yyP4->U_1.V_73.FormalType.Next);
    CodeFormals4(yyP4->U_1.V_73.FormalType.Next);
    return;
  }
}

static void CodeDecls2
# ifdef __STDC__
(Tree_tTree yyP5, BOOLEAN *NewLine)
# else
(yyP5, NewLine)
Tree_tTree yyP5;
BOOLEAN *NewLine;
# endif
{
  struct S_28 yyTempo;

  if (yyP5->U_1.V_1.Kind == Tree_Var) {
    Layout_WriteSpaces(f, (LONGINT)Indent);
    CodeType(yyP5->U_1.V_26.Var.Type, NonFull);
    IO_WriteC(f, ' ');
    CodeIds(yyP5->U_1.V_26.Var.VarIds, yyP5->U_1.V_26.Var.Type);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    *NewLine = TRUE;
    CodeDecls2(yyP5->U_1.V_26.Var.Next, NewLine);
    return;
  }
  if (yyP5->U_1.V_1.Kind == Tree_Module) {
    CodeDecls2(yyP5->U_1.V_32.Module.Decls, NewLine);
    CodeDecls2(yyP5->U_1.V_32.Module.Next, NewLine);
    return;
  }
  if (Tree_IsType(yyP5, Tree_Decl)) {
    CodeDecls2(yyP5->U_1.V_25.Decl.Next, NewLine);
    return;
  }
}

static void CodeExpr
# ifdef __STDC__
(Tree_tTree t, SHORTCARD Prec)
# else
(t, Prec)
Tree_tTree t;
SHORTCARD Prec;
# endif
{
  struct S_29 yyTempo;

  switch (t->U_1.V_1.Kind) {
  case Tree_Binary:;
    for (;;) {
      if (!IN(t->U_1.V_78.Binary.COperator, SET_ELEM(Code_cIn) | SET_ELEM(Code_cDifference) | SET_ELEM(Code_cIsSubset1) | SET_ELEM(Code_cIsSubset2))) {
        goto EXIT_23;
      }
      switch (t->U_1.V_78.Binary.COperator) {
      case Code_cIn:;
        IO_WriteS(f, (STRING)"IN", 2L);
        break;
      case Code_cDifference:;
        IO_WriteS(f, (STRING)"SET_DIFF", 8L);
        break;
      case Code_cIsSubset1:;
        IO_WriteS(f, (STRING)"SET_IS_SUBSET1", 14L);
        break;
      case Code_cIsSubset2:;
        IO_WriteS(f, (STRING)"SET_IS_SUBSET2", 14L);
        break;
      }
      IO_WriteC(f, '(');
      CodeExpr(t->U_1.V_78.Binary.Lop, pPassValue);
      IO_WriteS(f, (STRING)", ", 2L);
      CodeExpr(t->U_1.V_78.Binary.Rop, pPassValue);
      IO_WriteC(f, ')');
      return;
    } EXIT_23:;
    for (;;) {
      if (!(IN(t->U_1.V_78.Binary.COperator, SET_ELEM(Code_cPlus) | SET_ELEM(Code_cMinus)) && t->U_1.V_78.Binary.Lop->U_1.V_77.Expr.Type == Defs_TypeADDRESS && t->U_1.V_78.Binary.Rop->U_1.V_77.Expr.Type == Defs_TypeADDRESS)) {
        goto EXIT_24;
      }
      if (Prec <= Precedence.A[t->U_1.V_78.Binary.COperator]) {
        IO_WriteC(f, '(');
      }
      CodeExpr(t->U_1.V_78.Binary.Lop, Precedence.A[t->U_1.V_78.Binary.COperator] + 1);
      if (t->U_1.V_78.Binary.COperator == Code_cPlus) {
        IO_WriteS(f, (STRING)" + ", 3L);
      } else {
        IO_WriteS(f, (STRING)" - ", 3L);
      }
      IO_WriteS(f, (STRING)"(LONGCARD)", 10L);
      CodeExpr(t->U_1.V_78.Binary.Rop, pCast);
      if (Prec <= Precedence.A[t->U_1.V_78.Binary.COperator]) {
        IO_WriteC(f, ')');
      }
      return;
    } EXIT_24:;
    for (;;) {
      if (!(t->U_1.V_78.Binary.COperator == Code_cMinus && !(t->U_1.V_78.Binary.Lop->U_1.V_77.Expr.Type == Defs_TypeADDRESS) && t->U_1.V_78.Binary.Rop->U_1.V_77.Expr.Type == Defs_TypeADDRESS)) {
        goto EXIT_25;
      }
      if (Prec < pCast) {
        IO_WriteC(f, '(');
      }
      IO_WriteS(f, (STRING)"(ADDRESS)(", 10L);
      CodeExpr(t->U_1.V_78.Binary.Lop, pMinus + 1);
      IO_WriteS(f, (STRING)" - (LONGCARD)", 13L);
      CodeExpr(t->U_1.V_78.Binary.Rop, pCast);
      IO_WriteC(f, ')');
      if (Prec < pCast) {
        IO_WriteC(f, ')');
      }
      return;
    } EXIT_25:;
    for (;;) {
      if (!(IN(t->U_1.V_78.Binary.COperator, SET_ELEM(Code_cTimes) | SET_ELEM(Code_cDivide)) && (t->U_1.V_78.Binary.Lop->U_1.V_77.Expr.Type == Defs_TypeADDRESS || t->U_1.V_78.Binary.Rop->U_1.V_77.Expr.Type == Defs_TypeADDRESS))) {
        goto EXIT_26;
      }
      if (Prec < pCast) {
        IO_WriteC(f, '(');
      }
      IO_WriteS(f, (STRING)"(ADDRESS)(", 10L);
      if (t->U_1.V_78.Binary.Lop->U_1.V_77.Expr.Type == Defs_TypeADDRESS) {
        IO_WriteS(f, (STRING)"(LONGCARD)", 10L);
        CodeExpr(t->U_1.V_78.Binary.Lop, pCast);
      } else {
        CodeExpr(t->U_1.V_78.Binary.Lop, Precedence.A[t->U_1.V_78.Binary.COperator] + 1);
      }
      if (t->U_1.V_78.Binary.COperator == Code_cTimes) {
        IO_WriteS(f, (STRING)" * ", 3L);
      } else {
        IO_WriteS(f, (STRING)" / ", 3L);
      }
      if (t->U_1.V_78.Binary.Rop->U_1.V_77.Expr.Type == Defs_TypeADDRESS) {
        IO_WriteS(f, (STRING)"(LONGCARD)", 10L);
        CodeExpr(t->U_1.V_78.Binary.Rop, pCast);
      } else {
        CodeExpr(t->U_1.V_78.Binary.Rop, Precedence.A[t->U_1.V_78.Binary.COperator]);
      }
      IO_WriteC(f, ')');
      if (Prec < pCast) {
        IO_WriteC(f, ')');
      }
      return;
    } EXIT_26:;
    if (Prec <= Precedence.A[t->U_1.V_78.Binary.COperator]) {
      IO_WriteC(f, '(');
    }
    CodeExpr(t->U_1.V_78.Binary.Lop, Precedence.A[t->U_1.V_78.Binary.COperator] + 1);
    switch (t->U_1.V_78.Binary.COperator) {
    case Code_cNotEqual:;
      IO_WriteS(f, (STRING)" != ", 4L);
      break;
    case Code_cTimes:;
      IO_WriteS(f, (STRING)" * ", 3L);
      break;
    case Code_cPlus:;
      IO_WriteS(f, (STRING)" + ", 3L);
      break;
    case Code_cMinus:;
      IO_WriteS(f, (STRING)" - ", 3L);
      break;
    case Code_cDivide:;
      IO_WriteS(f, (STRING)" / ", 3L);
      break;
    case Code_cLess:;
      IO_WriteS(f, (STRING)" < ", 3L);
      break;
    case Code_cLessEqual:;
      IO_WriteS(f, (STRING)" <= ", 4L);
      break;
    case Code_cEqual:;
      IO_WriteS(f, (STRING)" == ", 4L);
      break;
    case Code_cGreater:;
      IO_WriteS(f, (STRING)" > ", 3L);
      break;
    case Code_cGreaterEqual:;
      IO_WriteS(f, (STRING)" >= ", 4L);
      break;
    case Code_cAnd:;
      IO_WriteS(f, (STRING)" && ", 4L);
      break;
    case Code_cMod:;
      IO_WriteS(f, (STRING)" % ", 3L);
      break;
    case Code_cOr:;
      IO_WriteS(f, (STRING)" || ", 4L);
      break;
    case Code_cUnion:;
      IO_WriteS(f, (STRING)" | ", 3L);
      break;
    case Code_cIntersection:;
      IO_WriteS(f, (STRING)" & ", 3L);
      break;
    case Code_cSymDiff:;
      IO_WriteS(f, (STRING)" ^ ", 3L);
      break;
    default :
      break;
    }
    CodeExpr(t->U_1.V_78.Binary.Rop, Precedence.A[t->U_1.V_78.Binary.COperator]);
    if (Prec <= Precedence.A[t->U_1.V_78.Binary.COperator]) {
      IO_WriteC(f, ')');
    }
    return;
    break;
  case Tree_Unary:;
    if (t->U_1.V_79.Unary.Mop->U_1.V_1.Kind == Tree_IntConst) {
      for (;;) {
        if (!(t->U_1.V_79.Unary.COperator == Code_cMinus)) {
          goto EXIT_27;
        }
        switch (t->U_1.V_79.Unary.Mop->U_1.V_80.IntConst.Kind) {
        case Tree_Decimal:;
          IO_WriteC(f, '-');
          IO_WriteCard(f, t->U_1.V_79.Unary.Mop->U_1.V_80.IntConst.IntVal, 1L);
          break;
        case Tree_Octal:;
          IO_WriteS(f, (STRING)"-0", 2L);
          IO_WriteN(f, t->U_1.V_79.Unary.Mop->U_1.V_80.IntConst.IntVal, 1L, 8L);
          break;
        case Tree_Hexadecimal:;
          IO_WriteS(f, (STRING)"-0X", 3L);
          IO_WriteN(f, t->U_1.V_79.Unary.Mop->U_1.V_80.IntConst.IntVal, 1L, 16L);
          break;
        }
        return;
      } EXIT_27:;
    }
    if (Prec < Precedence.A[t->U_1.V_79.Unary.COperator]) {
      IO_WriteC(f, '(');
    }
    switch (t->U_1.V_79.Unary.COperator) {
    case Code_cMinus:;
      IO_WriteC(f, '-');
      break;
    case Code_cNot:;
      IO_WriteC(f, '!');
      break;
    default :
      break;
    }
    CodeExpr(t->U_1.V_79.Unary.Mop, Precedence.A[t->U_1.V_79.Unary.COperator]);
    if (Prec < Precedence.A[t->U_1.V_79.Unary.COperator]) {
      IO_WriteC(f, ')');
    }
    return;
    break;
  case Tree_IntConst:;
    if (t->U_1.V_80.IntConst.IntVal > Types_MaxLongInt) {
      IO_WriteS(f, (STRING)"(LONGCARD)", 10L);
      Errors_ErrorMessageP((LONGCARD)Errors_OutOfLongRange, (LONGCARD)Errors_Warning, t->U_1.V_80.IntConst.Pos);
    }
    switch (t->U_1.V_80.IntConst.Kind) {
    case Tree_Decimal:;
      IO_WriteCard(f, t->U_1.V_80.IntConst.IntVal, 1L);
      break;
    case Tree_Octal:;
      IO_WriteC(f, '0');
      IO_WriteN(f, t->U_1.V_80.IntConst.IntVal, 1L, 8L);
      break;
    case Tree_Hexadecimal:;
      IO_WriteS(f, (STRING)"0X", 2L);
      IO_WriteN(f, t->U_1.V_80.IntConst.IntVal, 1L, 16L);
      break;
    }
    return;
    break;
  case Tree_RealConst:;
    StringMem_WriteString(f, t->U_1.V_81.RealConst.RealVal);
    return;
    break;
  case Tree_StringConst:;
    {
      register struct S_36 *W_19 = &yyTempo.U_1.V_1.yyR10;

      for (;;) {
        if (!(StringMem_Length(t->U_1.V_82.StringConst.StringVal) == 1)) {
          goto EXIT_28;
        }
        StringMem_GetString(t->U_1.V_82.StringConst.StringVal, &W_19->String);
        CodeChar(Strings_Char(&W_19->String, 1));
        return;
      } EXIT_28:;
    }
    CodeString1(t->U_1.V_82.StringConst.StringVal);
    return;
    break;
  case Tree_CharConst:;
    CodeChar(t->U_1.V_83.CharConst.CharVal);
    return;
    break;
  case Tree_FuncCall:;
    for (;;) {
      if (!(t->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_StdProcType1)) {
        goto EXIT_29;
      }
      CodeStdProc(t->U_1.V_84.FuncCall.Actuals, t->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type->U_1.V_34.StdProcType1.StdProc, Defs_NoObject);
      return;
    } EXIT_29:;
    if (Tree_IsType(t->U_1.V_84.FuncCall.Designator, Tree_Qualid)) {
      for (;;) {
        if (!Defs_IsProcedure(t->U_1.V_84.FuncCall.Designator->U_1.V_88.Qualid.Object)) {
          goto EXIT_30;
        }
        Idents_WriteIdent(f, t->U_1.V_84.FuncCall.Designator->U_1.V_88.Qualid.Object->U_1.V_3.Object.CIdent);
        IO_WriteC(f, '(');
        CodeActuals(t->U_1.V_84.FuncCall.Actuals);
        IO_WriteC(f, ')');
        return;
      } EXIT_30:;
      if (t->U_1.V_84.FuncCall.Actuals->U_1.V_1.Kind == Tree_Actual) {
        if (t->U_1.V_84.FuncCall.Actuals->U_1.V_101.Actual.Next->U_1.V_1.Kind == Tree_Actuals0) {
          for (;;) {
            if (!Defs_IsOfType(t->U_1.V_84.FuncCall.Designator->U_1.V_88.Qualid.Object)) {
              goto EXIT_31;
            }
            if (Prec < pCast) {
              IO_WriteS(f, (STRING)"((", 2L);
              Idents_WriteIdent(f, t->U_1.V_84.FuncCall.Designator->U_1.V_88.Qualid.Object->U_1.V_3.Object.CIdent);
              IO_WriteC(f, ')');
              CodeExpr(t->U_1.V_84.FuncCall.Actuals->U_1.V_101.Actual.Expr, pCast);
              IO_WriteC(f, ')');
            } else {
              IO_WriteC(f, '(');
              Idents_WriteIdent(f, t->U_1.V_84.FuncCall.Designator->U_1.V_88.Qualid.Object->U_1.V_3.Object.CIdent);
              IO_WriteC(f, ')');
              CodeExpr(t->U_1.V_84.FuncCall.Actuals->U_1.V_101.Actual.Expr, pCast);
            }
            return;
          } EXIT_31:;
        }
      }
    }
    IO_WriteS(f, (STRING)"(*", 2L);
    CodeExpr(t->U_1.V_84.FuncCall.Designator, pDeref);
    IO_WriteC(f, ')');
    IO_WriteC(f, '(');
    CodeActuals(t->U_1.V_84.FuncCall.Actuals);
    IO_WriteC(f, ')');
    return;
    break;
  case Tree_Set:;
    if (t->U_1.V_85.Set.Elems->U_1.V_1.Kind == Tree_Elems0) {
      IO_WriteS(f, (STRING)"0X0L", 4L);
      return;
    }
    if (Tree_IsType(t->U_1.V_85.Set.Elems, Tree_Elems1)) {
      if (Prec <= pUnion) {
        IO_WriteC(f, '(');
        CodeExpr(t->U_1.V_85.Set.Elems, pMinPrec);
        IO_WriteC(f, ')');
      } else {
        CodeExpr(t->U_1.V_85.Set.Elems, Prec);
      }
      return;
    }
    break;
  case Tree_BitSet:;
    if (t->U_1.V_86.BitSet.Elems->U_1.V_1.Kind == Tree_Elems0) {
      IO_WriteS(f, (STRING)"0X0L", 4L);
      return;
    }
    if (Tree_IsType(t->U_1.V_86.BitSet.Elems, Tree_Elems1)) {
      if (Prec <= pUnion) {
        IO_WriteC(f, '(');
        CodeExpr(t->U_1.V_86.BitSet.Elems, pMinPrec);
        IO_WriteC(f, ')');
      } else {
        CodeExpr(t->U_1.V_86.BitSet.Elems, Prec);
      }
      return;
    }
    break;
  case Tree_Qualid0:;
    for (;;) {
      if (!(t->U_1.V_89.Qualid0.IsGlobalPtr && Defs_IsOpen(t->U_1.V_89.Qualid0.Type))) {
        goto EXIT_32;
      }
      Idents_WriteIdent(f, GetGlobalPtr(t->U_1.V_89.Qualid0.Object));
      return;
    } EXIT_32:;
    for (;;) {
      if (!(t->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_Field1)) {
        goto EXIT_33;
      }
      Idents_WriteIdent(f, GetWith(t->U_1.V_89.Qualid0.Ident));
      IO_WriteS(f, (STRING)"->", 2L);
      CodeSelectors(t->U_1.V_89.Qualid0.Object->U_1.V_7.Field1.Selectors);
      Idents_WriteIdent(f, t->U_1.V_89.Qualid0.Object->U_1.V_7.Field1.CIdent);
      return;
    } EXIT_33:;
    break;
  case Tree_Qualid1:;
    for (;;) {
      if (!t->U_1.V_90.Qualid1.Qualid->U_1.V_88.Qualid.IsGlobalPtr) {
        goto EXIT_34;
      }
      Idents_WriteIdent(f, GetGlobalPtr(t->U_1.V_90.Qualid1.Qualid->U_1.V_88.Qualid.Object));
      IO_WriteS(f, (STRING)"->", 2L);
      CodeSelectors(Defs_GetSelectors(t->U_1.V_90.Qualid1.Object));
      Idents_WriteIdent(f, t->U_1.V_90.Qualid1.Object->U_1.V_3.Object.CIdent);
      return;
    } EXIT_34:;
    for (;;) {
      if (!Defs_IsVAR(t->U_1.V_90.Qualid1.Qualid->U_1.V_88.Qualid.Object)) {
        goto EXIT_35;
      }
      Idents_WriteIdent(f, t->U_1.V_90.Qualid1.Qualid->U_1.V_88.Qualid.Object->U_1.V_13.Var1.CIdent);
      IO_WriteS(f, (STRING)"->", 2L);
      CodeSelectors(Defs_GetSelectors(t->U_1.V_90.Qualid1.Object));
      Idents_WriteIdent(f, t->U_1.V_90.Qualid1.Object->U_1.V_3.Object.CIdent);
      return;
    } EXIT_35:;
    for (;;) {
      if (!(t->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_Field1)) {
        goto EXIT_36;
      }
      CodeExpr(t->U_1.V_90.Qualid1.Qualid, pSelect);
      IO_WriteC(f, '.');
      CodeSelectors(t->U_1.V_90.Qualid1.Object->U_1.V_7.Field1.Selectors);
      Idents_WriteIdent(f, t->U_1.V_90.Qualid1.Object->U_1.V_7.Field1.CIdent);
      return;
    } EXIT_36:;
    break;
  default :
    break;
  }
  if (Tree_IsType(t, Tree_Qualid)) {
    for (;;) {
      if (!t->U_1.V_88.Qualid.IsGlobalPtr) {
        goto EXIT_37;
      }
      if (Prec < pDeref) {
        IO_WriteS(f, (STRING)"(*", 2L);
        Idents_WriteIdent(f, GetGlobalPtr(t->U_1.V_88.Qualid.Object));
        IO_WriteC(f, ')');
      } else {
        IO_WriteC(f, '*');
        Idents_WriteIdent(f, GetGlobalPtr(t->U_1.V_88.Qualid.Object));
      }
      return;
    } EXIT_37:;
    for (;;) {
      if (!(Defs_IsVAR(t->U_1.V_88.Qualid.Object) && !Defs_IsOpen(t->U_1.V_88.Qualid.Type))) {
        goto EXIT_38;
      }
      if (Prec < pDeref) {
        IO_WriteS(f, (STRING)"(*", 2L);
        Idents_WriteIdent(f, t->U_1.V_88.Qualid.Object->U_1.V_13.Var1.CIdent);
        IO_WriteC(f, ')');
      } else {
        IO_WriteC(f, '*');
        Idents_WriteIdent(f, t->U_1.V_88.Qualid.Object->U_1.V_13.Var1.CIdent);
      }
      return;
    } EXIT_38:;
    Idents_WriteIdent(f, t->U_1.V_88.Qualid.Object->U_1.V_3.Object.CIdent);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Subscript) {
    if (t->U_1.V_91.Subscript.Designator->U_1.V_1.Kind == Tree_Qualid0) {
      for (;;) {
        if (!Defs_IsOpenArray(t->U_1.V_91.Subscript.Designator->U_1.V_89.Qualid0.Object)) {
          goto EXIT_39;
        }
        CodeExpr(t->U_1.V_91.Subscript.Designator, pSubscript);
        IO_WriteC(f, '[');
        CodeExpr(t->U_1.V_91.Subscript.Index, pMinPrec);
        IO_WriteC(f, ']');
        return;
      } EXIT_39:;
    }
    if (t->U_1.V_91.Subscript.Designator->U_1.V_1.Kind == Tree_Deref) {
      {
        register struct S_35 *W_20 = &yyTempo.U_1.V_2.yyR30;

        for (;;) {
          if (!!(t->U_1.V_91.Subscript.Designator->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_OpaqueType1)) {
            goto EXIT_40;
          }
          Types_GetLwb(Defs_GetIndexType(t->U_1.V_91.Subscript.Designator->U_1.V_92.Deref.Type), &W_20->Lwb);
          CodeExpr(t->U_1.V_91.Subscript.Designator->U_1.V_92.Deref.Designator, pSelect);
          IO_WriteS(f, (STRING)"->A[", 4L);
          if (Values_ValueToInt(W_20->Lwb) != 0) {
            CodeExpr(t->U_1.V_91.Subscript.Index, pMinus + 1);
            IO_WriteS(f, (STRING)" - ", 3L);
            CodeValue(W_20->Lwb);
          } else {
            CodeExpr(t->U_1.V_91.Subscript.Index, pMinPrec);
          }
          IO_WriteC(f, ']');
          return;
        } EXIT_40:;
      }
    }
    if (Tree_IsType(t->U_1.V_91.Subscript.Designator, Tree_Qualid)) {
      {
        register struct S_34 *W_21 = &yyTempo.U_1.V_3.yyR31;

        for (;;) {
          if (!(t->U_1.V_91.Subscript.Designator->U_1.V_88.Qualid.IsGlobalPtr && !Defs_IsOpen(t->U_1.V_91.Subscript.Designator->U_1.V_88.Qualid.Type))) {
            goto EXIT_41;
          }
          Types_GetLwb(Defs_GetIndexType(t->U_1.V_91.Subscript.Designator->U_1.V_88.Qualid.Type), &W_21->Lwb);
          Idents_WriteIdent(f, GetGlobalPtr(t->U_1.V_91.Subscript.Designator->U_1.V_88.Qualid.Object));
          IO_WriteS(f, (STRING)"->A[", 4L);
          if (Values_ValueToInt(W_21->Lwb) != 0) {
            CodeExpr(t->U_1.V_91.Subscript.Index, pMinus + 1);
            IO_WriteS(f, (STRING)" - ", 3L);
            CodeValue(W_21->Lwb);
          } else {
            CodeExpr(t->U_1.V_91.Subscript.Index, pMinPrec);
          }
          IO_WriteC(f, ']');
          return;
        } EXIT_41:;
      }
      {
        register struct S_33 *W_22 = &yyTempo.U_1.V_4.yyR32;

        for (;;) {
          if (!(Defs_IsVAR(t->U_1.V_91.Subscript.Designator->U_1.V_88.Qualid.Object) && !Defs_IsOpen(t->U_1.V_91.Subscript.Designator->U_1.V_88.Qualid.Type))) {
            goto EXIT_42;
          }
          Types_GetLwb(Defs_GetIndexType(t->U_1.V_91.Subscript.Designator->U_1.V_88.Qualid.Type), &W_22->Lwb);
          Idents_WriteIdent(f, t->U_1.V_91.Subscript.Designator->U_1.V_88.Qualid.Object->U_1.V_13.Var1.CIdent);
          IO_WriteS(f, (STRING)"->A[", 4L);
          if (Values_ValueToInt(W_22->Lwb) != 0) {
            CodeExpr(t->U_1.V_91.Subscript.Index, pMinus + 1);
            IO_WriteS(f, (STRING)" - ", 3L);
            CodeValue(W_22->Lwb);
          } else {
            CodeExpr(t->U_1.V_91.Subscript.Index, pMinPrec);
          }
          IO_WriteC(f, ']');
          return;
        } EXIT_42:;
      }
    }
    {
      register struct S_32 *W_23 = &yyTempo.U_1.V_5.yyR33;

      Types_GetLwb(Defs_GetIndexType(t->U_1.V_91.Subscript.Designator->U_1.V_87.Designator.Type), &W_23->Lwb);
      CodeExpr(t->U_1.V_91.Subscript.Designator, pSelect);
      IO_WriteS(f, (STRING)".A[", 3L);
      if (Values_ValueToInt(W_23->Lwb) != 0) {
        CodeExpr(t->U_1.V_91.Subscript.Index, pMinus + 1);
        IO_WriteS(f, (STRING)" - ", 3L);
        CodeValue(W_23->Lwb);
      } else {
        CodeExpr(t->U_1.V_91.Subscript.Index, pMinPrec);
      }
      IO_WriteC(f, ']');
      return;
    }
  }
  if (t->U_1.V_1.Kind == Tree_Select) {
    if (t->U_1.V_93.Select.Designator->U_1.V_1.Kind == Tree_Deref) {
      {
        register struct S_31 *W_24 = &yyTempo.U_1.V_6.yyR34;

        for (;;) {
          if (!(t->U_1.V_93.Select.Designator->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_OpaqueType1)) {
            goto EXIT_43;
          }
          W_24->Object = t->U_1.V_93.Select.Designator->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type->U_1.V_36.Constructor.TypeObj;
          IO_WriteS(f, (STRING)"((", 2L);
          Idents_WriteIdent(f, GenIdents_GenOpaque(W_24->Object->U_1.V_3.Object.CIdent));
          IO_WriteC(f, ')');
          CodeExpr(t->U_1.V_93.Select.Designator->U_1.V_92.Deref.Designator, pCast);
          IO_WriteS(f, (STRING)")->", 3L);
          CodeSelectors(Defs_GetSelectors(t->U_1.V_93.Select.Object));
          Idents_WriteIdent(f, t->U_1.V_93.Select.Object->U_1.V_3.Object.CIdent);
          return;
        } EXIT_43:;
      }
      CodeExpr(t->U_1.V_93.Select.Designator->U_1.V_92.Deref.Designator, pSelect);
      IO_WriteS(f, (STRING)"->", 2L);
      CodeSelectors(Defs_GetSelectors(t->U_1.V_93.Select.Object));
      Idents_WriteIdent(f, t->U_1.V_93.Select.Object->U_1.V_3.Object.CIdent);
      return;
    }
    CodeExpr(t->U_1.V_93.Select.Designator, pSelect);
    IO_WriteC(f, '.');
    CodeSelectors(Defs_GetSelectors(t->U_1.V_93.Select.Object));
    Idents_WriteIdent(f, t->U_1.V_93.Select.Object->U_1.V_3.Object.CIdent);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Deref) {
    {
      register struct S_30 *W_25 = &yyTempo.U_1.V_7.yyR37;

      for (;;) {
        if (!(t->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_OpaqueType1)) {
          goto EXIT_44;
        }
        W_25->Object = t->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type->U_1.V_36.Constructor.TypeObj;
        if (Prec < pDeref) {
          IO_WriteS(f, (STRING)"(*(", 3L);
          Idents_WriteIdent(f, GenIdents_GenOpaque(W_25->Object->U_1.V_3.Object.CIdent));
          IO_WriteC(f, ')');
          CodeExpr(t->U_1.V_92.Deref.Designator, pCast);
          IO_WriteC(f, ')');
        } else {
          IO_WriteS(f, (STRING)"*(", 2L);
          Idents_WriteIdent(f, GenIdents_GenOpaque(W_25->Object->U_1.V_3.Object.CIdent));
          IO_WriteC(f, ')');
          CodeExpr(t->U_1.V_92.Deref.Designator, pCast);
        }
        return;
      } EXIT_44:;
    }
    if (Prec < pDeref) {
      IO_WriteS(f, (STRING)"(*", 2L);
      CodeExpr(t->U_1.V_92.Deref.Designator, pDeref);
      IO_WriteC(f, ')');
    } else {
      IO_WriteC(f, '*');
      CodeExpr(t->U_1.V_92.Deref.Designator, pDeref);
    }
    return;
  }
  if (t->U_1.V_1.Kind == Tree_Elem) {
    IO_WriteS(f, (STRING)"SET_ELEM(", 9L);
    CodeExpr(t->U_1.V_97.Elem.Elem, pPassValue);
    IO_WriteC(f, ')');
    CodeSepa(t->U_1.V_97.Elem.Next);
    CodeExpr(t->U_1.V_97.Elem.Next, pUnion);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_ElemRange) {
    for (;;) {
      if (!(t->U_1.V_98.ElemRange.Lwb->U_1.V_77.Expr.IsCConst && t->U_1.V_98.ElemRange.Upb->U_1.V_77.Expr.IsCConst)) {
        goto EXIT_45;
      }
      IO_WriteS(f, (STRING)"SET_cRNG(", 9L);
      CodeExpr(t->U_1.V_98.ElemRange.Lwb, pPassValue);
      IO_WriteS(f, (STRING)", ", 2L);
      CodeExpr(t->U_1.V_98.ElemRange.Upb, pPassValue);
      IO_WriteC(f, ')');
      CodeSepa(t->U_1.V_98.ElemRange.Next);
      CodeExpr(t->U_1.V_98.ElemRange.Next, pUnion);
      return;
    } EXIT_45:;
    IO_WriteS(f, (STRING)"SET_RANGE(", 10L);
    CodeExpr(t->U_1.V_98.ElemRange.Lwb, pPassValue);
    IO_WriteS(f, (STRING)", ", 2L);
    CodeExpr(t->U_1.V_98.ElemRange.Upb, pPassValue);
    IO_WriteC(f, ')');
    CodeSepa(t->U_1.V_98.ElemRange.Next);
    CodeExpr(t->U_1.V_98.ElemRange.Next, pUnion);
    return;
  }
}

static void CodeAdr
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_37 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Deref) {
    CodeExpr(t->U_1.V_92.Deref.Designator, pMinPrec);
    return;
  }
  if (Tree_IsType(t, Tree_Qualid)) {
    for (;;) {
      if (!t->U_1.V_88.Qualid.IsGlobalPtr) {
        goto EXIT_46;
      }
      Idents_WriteIdent(f, GetGlobalPtr(t->U_1.V_88.Qualid.Object));
      return;
    } EXIT_46:;
  }
  if (t->U_1.V_1.Kind == Tree_Qualid0) {
    for (;;) {
      if (!Defs_IsVAR(t->U_1.V_89.Qualid0.Object)) {
        goto EXIT_47;
      }
      Idents_WriteIdent(f, t->U_1.V_89.Qualid0.Object->U_1.V_13.Var1.CIdent);
      return;
    } EXIT_47:;
  }
  IO_WriteC(f, '&');
  CodeExpr(t, pAdr);
  return;
}

static void CodeString2
# ifdef __STDC__
(Tree_tTree t, BOOLEAN C_3_Strncpy)
# else
(t, C_3_Strncpy)
Tree_tTree t;
BOOLEAN C_3_Strncpy;
# endif
{
  struct S_38 yyTempo;

  if (t->U_1.V_1.Kind == Tree_StringConst) {
    if (Base_OptionIsSet('c') && !C_3_Strncpy) {
      IO_WriteS(f, (STRING)"(STRING)", 8L);
    }
    CodeString1(t->U_1.V_82.StringConst.StringVal);
    return;
  }
  if (t->U_1.V_1.Kind == Tree_CharConst) {
    if (Base_OptionIsSet('c') && !C_3_Strncpy) {
      IO_WriteS(f, (STRING)"(STRING)", 8L);
    }
    CodeString3(t->U_1.V_83.CharConst.CharVal);
    return;
  }
  if (Tree_IsType(t, Tree_Qualid)) {
    for (;;) {
      if (!(t->U_1.V_88.Qualid.Type == Defs_TypeSTRING)) {
        goto EXIT_48;
      }
      if (Base_OptionIsSet('c') && C_3_Strncpy) {
        IO_WriteS(f, (STRING)"(char *)", 8L);
      }
      Idents_WriteIdent(f, t->U_1.V_88.Qualid.Object->U_1.V_3.Object.CIdent);
      return;
    } EXIT_48:;
    for (;;) {
      if (!(t->U_1.V_88.Qualid.Type == Defs_TypeStringChar)) {
        goto EXIT_49;
      }
      if (Base_OptionIsSet('c') && !C_3_Strncpy) {
        IO_WriteS(f, (STRING)"(STRING)", 8L);
      }
      CodeString3(t->U_1.V_88.Qualid.Object->U_1.V_5.Const1.Value.U_1.V_4.CharValue);
      return;
    } EXIT_49:;
    for (;;) {
      if (!(t->U_1.V_88.Qualid.Type == Defs_TypeCHAR)) {
        goto EXIT_50;
      }
      if (Base_OptionIsSet('c') && !C_3_Strncpy) {
        IO_WriteS(f, (STRING)"(STRING)", 8L);
      }
      CodeString3(t->U_1.V_88.Qualid.Object->U_1.V_5.Const1.Value.U_1.V_4.CharValue);
      return;
    } EXIT_50:;
  }
}

static void CodeStmts
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_39 yyTempo;

  switch (t->U_1.V_1.Kind) {
  case Tree_Assign:;
    for (;;) {
      if (!(t->U_1.V_105.Assign.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_Array1 && (t->U_1.V_105.Assign.Expr->U_1.V_77.Expr.Type == Defs_TypeStringChar || t->U_1.V_105.Assign.Expr->U_1.V_77.Expr.Type == Defs_TypeSTRING))) {
        goto EXIT_51;
      }
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"(void)strncpy(", 14L);
      if (Base_OptionIsSet('c')) {
        IO_WriteS(f, (STRING)"(char *)", 8L);
      }
      CodeExpr(t->U_1.V_105.Assign.Designator, pSelect);
      IO_WriteS(f, (STRING)".A, ", 4L);
      CodeString2(t->U_1.V_105.Assign.Expr, Strncpy);
      IO_WriteS(f, (STRING)", ", 2L);
      IO_WriteS(f, (STRING)"sizeof(", 7L);
      CodeExpr(t->U_1.V_105.Assign.Designator, pSelect);
      IO_WriteS(f, (STRING)".A));", 5L);
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_105.Assign.Next);
      return;
    } EXIT_51:;
    for (;;) {
      if (!Types_Cast(Code_cAssign, t->U_1.V_105.Assign.Designator->U_1.V_87.Designator.Type, t->U_1.V_105.Assign.Expr->U_1.V_77.Expr.Type)) {
        goto EXIT_52;
      }
      Layout_WriteSpaces(f, (LONGINT)Indent);
      CodeExpr(t->U_1.V_105.Assign.Designator, pAssign);
      IO_WriteS(f, (STRING)" = ", 3L);
      CodeCast(t->U_1.V_105.Assign.Designator->U_1.V_87.Designator.Type);
      CodeExpr(t->U_1.V_105.Assign.Expr, pCast);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_105.Assign.Next);
      return;
    } EXIT_52:;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    CodeExpr(t->U_1.V_105.Assign.Designator, pAssign);
    IO_WriteS(f, (STRING)" = ", 3L);
    CodeExpr(t->U_1.V_105.Assign.Expr, pAssign);
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_105.Assign.Next);
    return;
    break;
  case Tree_Call:;
    {
      register struct S_44 *W_26 = &yyTempo.U_1.V_1.yyR4;

      for (;;) {
        if (!(t->U_1.V_106.Call.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_StdProcType1)) {
          goto EXIT_53;
        }
        W_26->StdProc = t->U_1.V_106.Call.Designator->U_1.V_87.Designator.Type->U_1.V_34.StdProcType1.StdProc;
        switch (W_26->StdProc) {
        case Defs_ProcTRANSFER:;
          Errors_ErrorMessageP((LONGCARD)Errors_NoTRANSFER, (LONGCARD)Errors_Restriction, t->U_1.V_106.Call.Designator->U_1.V_87.Designator.Pos);
          break;
        case Defs_ProcIOTRANSFER:;
          Errors_ErrorMessageP((LONGCARD)Errors_NoIOTRANSFER, (LONGCARD)Errors_Restriction, t->U_1.V_106.Call.Designator->U_1.V_87.Designator.Pos);
          break;
        case Defs_ProcNEWPROCESS:;
          Errors_ErrorMessageP((LONGCARD)Errors_NoNEWPROCESS, (LONGCARD)Errors_Restriction, t->U_1.V_106.Call.Designator->U_1.V_87.Designator.Pos);
          break;
        case Defs_ProcNEW:;
        case Defs_ProcDISPOSE:;
          Layout_WriteSpaces(f, (LONGINT)Indent);
          CodeStdProc(t->U_1.V_106.Call.Actuals, W_26->StdProc, t->U_1.V_106.Call.AllocOrDealloc);
          IO_WriteC(f, ';');
          IO_WriteNl(f);
          break;
        default :
          Layout_WriteSpaces(f, (LONGINT)Indent);
          CodeStdProc(t->U_1.V_106.Call.Actuals, W_26->StdProc, Defs_NoObject);
          IO_WriteC(f, ';');
          IO_WriteNl(f);
          break;
        }
        CodeStmts(t->U_1.V_106.Call.Next);
        return;
      } EXIT_53:;
    }
    if (Tree_IsType(t->U_1.V_106.Call.Designator, Tree_Qualid)) {
      for (;;) {
        if (!Defs_IsProcedure(t->U_1.V_106.Call.Designator->U_1.V_88.Qualid.Object)) {
          goto EXIT_54;
        }
        Layout_WriteSpaces(f, (LONGINT)Indent);
        Idents_WriteIdent(f, t->U_1.V_106.Call.Designator->U_1.V_88.Qualid.Object->U_1.V_3.Object.CIdent);
        IO_WriteC(f, '(');
        CodeActuals(t->U_1.V_106.Call.Actuals);
        IO_WriteS(f, (STRING)");", 2L);
        IO_WriteNl(f);
        CodeStmts(t->U_1.V_106.Call.Next);
        return;
      } EXIT_54:;
    }
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"(*", 2L);
    CodeExpr(t->U_1.V_106.Call.Designator, pDeref);
    IO_WriteC(f, ')');
    IO_WriteC(f, '(');
    CodeActuals(t->U_1.V_106.Call.Actuals);
    IO_WriteS(f, (STRING)");", 2L);
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_106.Call.Next);
    return;
    break;
  case Tree_If:;
    if (t->U_1.V_107.If.Else->U_1.V_1.Kind == Tree_Stmts0) {
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"if (", 4L);
      CodeExpr(t->U_1.V_107.If.Cond, pMinPrec);
      IO_WriteS(f, (STRING)") {", 3L);
      IO_WriteNl(f);
      INC1(Indent, 2);
      CodeStmts(t->U_1.V_107.If.Then);
      DEC1(Indent, 2);
      CodeStmts(t->U_1.V_107.If.Elsifs);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteC(f, '}');
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_107.If.Next);
      return;
    }
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"if (", 4L);
    CodeExpr(t->U_1.V_107.If.Cond, pMinPrec);
    IO_WriteS(f, (STRING)") {", 3L);
    IO_WriteNl(f);
    INC1(Indent, 2);
    CodeStmts(t->U_1.V_107.If.Then);
    DEC1(Indent, 2);
    CodeStmts(t->U_1.V_107.If.Elsifs);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"} else {", 8L);
    IO_WriteNl(f);
    INC1(Indent, 2);
    CodeStmts(t->U_1.V_107.If.Else);
    DEC1(Indent, 2);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteC(f, '}');
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_107.If.Next);
    return;
    break;
  case Tree_Case:;
    if (t->U_1.V_108.Case.Default == TRUE) {
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"switch (", 8L);
      CodeExpr(t->U_1.V_108.Case.Expr, pMinPrec);
      IO_WriteS(f, (STRING)") {", 3L);
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_108.Case.Cases);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"default :", 9L);
      IO_WriteNl(f);
      INC1(Indent, 2);
      CodeStmts(t->U_1.V_108.Case.Else);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"break;", 6L);
      IO_WriteNl(f);
      DEC1(Indent, 2);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteC(f, '}');
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_108.Case.Next);
      return;
    }
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"switch (", 8L);
    CodeExpr(t->U_1.V_108.Case.Expr, pMinPrec);
    IO_WriteS(f, (STRING)") {", 3L);
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_108.Case.Cases);
    if (Base_OptionIsSet('r')) {
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"default :", 9L);
      IO_WriteNl(f);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"  CaseError(__FILE__, __LINE__);", 32L);
      IO_WriteNl(f);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"  break;", 8L);
      IO_WriteNl(f);
    }
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteC(f, '}');
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_108.Case.Next);
    return;
    break;
  case Tree_While:;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"while (", 7L);
    CodeExpr(t->U_1.V_109.While.Cond, pMinPrec);
    IO_WriteS(f, (STRING)") {", 3L);
    IO_WriteNl(f);
    INC1(Indent, 2);
    CodeStmts(t->U_1.V_109.While.Stmts);
    DEC1(Indent, 2);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteC(f, '}');
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_109.While.Next);
    return;
    break;
  case Tree_Repeat:;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"do {", 4L);
    IO_WriteNl(f);
    INC1(Indent, 2);
    CodeStmts(t->U_1.V_110.Repeat.Stmts);
    DEC1(Indent, 2);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"} while (!", 10L);
    CodeExpr(t->U_1.V_110.Repeat.Cond, pNot);
    IO_WriteS(f, (STRING)");", 2L);
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_110.Repeat.Next);
    return;
    break;
  case Tree_Loop:;
    C_12_Push(GenIdents_GenLabel());
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"for (;;) {", 10L);
    IO_WriteNl(f);
    INC1(Indent, 2);
    CodeStmts(t->U_1.V_111.Loop.Stmts);
    DEC1(Indent, 2);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"} ", 2L);
    Idents_WriteIdent(f, Top());
    IO_WriteS(f, (STRING)":;", 2L);
    IO_WriteNl(f);
    C_13_Pop();
    CodeStmts(t->U_1.V_111.Loop.Next);
    return;
    break;
  case Tree_For:;
    for (;;) {
      if (!(t->U_1.V_112.For.To->U_1.V_77.Expr.IsCConst && !OverOrUnderflow(t->U_1.V_112.For.Qualid->U_1.V_88.Qualid.Type, t->U_1.V_112.For.ToVal, t->U_1.V_112.For.ByVal))) {
        goto EXIT_55;
      }
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"for (", 5L);
      CodeExpr(t->U_1.V_112.For.Qualid, pAssign);
      IO_WriteS(f, (STRING)" = ", 3L);
      CodeExpr(t->U_1.V_112.For.From, pAssign);
      IO_WriteS(f, (STRING)"; ", 2L);
      CodeExpr(t->U_1.V_112.For.Qualid, pComparison);
      if (Values_ValueToInt(t->U_1.V_112.For.ByVal) >= 0) {
        IO_WriteS(f, (STRING)" <= ", 4L);
      } else {
        IO_WriteS(f, (STRING)" >= ", 4L);
      }
      CodeExpr(t->U_1.V_112.For.To, pComparison);
      IO_WriteS(f, (STRING)"; ", 2L);
      CodeExpr(t->U_1.V_112.For.Qualid, pAssign);
      IO_WriteS(f, (STRING)" += ", 4L);
      CodeExpr(t->U_1.V_112.For.By, pAssign);
      IO_WriteS(f, (STRING)") {", 3L);
      IO_WriteNl(f);
      INC1(Indent, 2);
      CodeStmts(t->U_1.V_112.For.Stmts);
      DEC1(Indent, 2);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteC(f, '}');
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_112.For.Next);
      return;
    } EXIT_55:;
    {
      register struct S_43 *W_27 = &yyTempo.U_1.V_2.yyR15;

      W_27->BoundId1 = GenIdents_GenBound();
      W_27->BoundId2 = GenIdents_GenBound();
      W_27->ByValue = Values_ValueToInt(t->U_1.V_112.For.ByVal);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteC(f, '{');
      IO_WriteNl(f);
      INC1(Indent, 2);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      CodeTypeName(t->U_1.V_112.For.Qualid->U_1.V_88.Qualid.Type);
      IO_WriteC(f, ' ');
      Idents_WriteIdent(f, W_27->BoundId1);
      IO_WriteS(f, (STRING)" = ", 3L);
      CodeExpr(t->U_1.V_112.For.From, pAssign);
      IO_WriteS(f, (STRING)", ", 2L);
      Idents_WriteIdent(f, W_27->BoundId2);
      IO_WriteS(f, (STRING)" = ", 3L);
      CodeExpr(t->U_1.V_112.For.To, pAssign);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      IO_WriteNl(f);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"if (", 4L);
      Idents_WriteIdent(f, W_27->BoundId1);
      if (W_27->ByValue >= 0) {
        IO_WriteS(f, (STRING)" <= ", 4L);
      } else {
        IO_WriteS(f, (STRING)" >= ", 4L);
      }
      Idents_WriteIdent(f, W_27->BoundId2);
      IO_WriteC(f, ')');
      IO_WriteNl(f);
      INC1(Indent, 2);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"for (", 5L);
      CodeExpr(t->U_1.V_112.For.Qualid, pAssign);
      IO_WriteS(f, (STRING)" = ", 3L);
      Idents_WriteIdent(f, W_27->BoundId1);
      if (W_27->ByValue >= 2) {
        IO_WriteS(f, (STRING)", ", 2L);
        Idents_WriteIdent(f, W_27->BoundId2);
        IO_WriteS(f, (STRING)" = ", 3L);
        IO_WriteS(f, (STRING)"FOR_LIMIT_UP(", 13L);
        Idents_WriteIdent(f, W_27->BoundId2);
        IO_WriteS(f, (STRING)", ", 2L);
        CodeExpr(t->U_1.V_112.For.By, pPassValue);
        IO_WriteS(f, (STRING)", ", 2L);
        CodeMIN(t->U_1.V_112.For.Qualid->U_1.V_88.Qualid.Type);
        IO_WriteC(f, ')');
      } else if (W_27->ByValue <= -2) {
        IO_WriteS(f, (STRING)", ", 2L);
        Idents_WriteIdent(f, W_27->BoundId2);
        IO_WriteS(f, (STRING)" = ", 3L);
        IO_WriteS(f, (STRING)"FOR_LIMIT_DOWN(", 15L);
        Idents_WriteIdent(f, W_27->BoundId2);
        IO_WriteS(f, (STRING)", ", 2L);
        CodeExpr(t->U_1.V_112.For.By, pPassValue);
        IO_WriteS(f, (STRING)", ", 2L);
        CodeMAX(t->U_1.V_112.For.Qualid->U_1.V_88.Qualid.Type);
        IO_WriteC(f, ')');
      }
      IO_WriteS(f, (STRING)";; ", 3L);
      CodeExpr(t->U_1.V_112.For.Qualid, pAssign);
      IO_WriteS(f, (STRING)" += ", 4L);
      CodeExpr(t->U_1.V_112.For.By, pAssign);
      IO_WriteS(f, (STRING)") {", 3L);
      IO_WriteNl(f);
      INC1(Indent, 2);
      CodeStmts(t->U_1.V_112.For.Stmts);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"if (", 4L);
      CodeExpr(t->U_1.V_112.For.Qualid, pComparison);
      if (W_27->ByValue >= 0) {
        IO_WriteS(f, (STRING)" >= ", 4L);
      } else {
        IO_WriteS(f, (STRING)" <= ", 4L);
      }
      Idents_WriteIdent(f, W_27->BoundId2);
      IO_WriteS(f, (STRING)") break;", 8L);
      IO_WriteNl(f);
      DEC1(Indent, 2);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteC(f, '}');
      IO_WriteNl(f);
      DEC1(Indent, 4);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteC(f, '}');
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_112.For.Next);
      return;
    }
    break;
  case Tree_With:;
    if (t->U_1.V_113.With.Designator->U_1.V_1.Kind == Tree_Deref) {
      {
        register struct S_42 *W_28 = &yyTempo.U_1.V_3.yyR16;

        for (;;) {
          if (!(t->U_1.V_113.With.Designator->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_OpaqueType1)) {
            goto EXIT_56;
          }
          W_28->WithId = GenIdents_GenWith();
          W_28->Object = t->U_1.V_113.With.Designator->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type->U_1.V_36.Constructor.TypeObj;
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteC(f, '{');
          IO_WriteNl(f);
          INC1(Indent, 2);
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteS(f, (STRING)"register ", 9L);
          CodeTypeName(t->U_1.V_113.With.Designator->U_1.V_92.Deref.Type);
          IO_WriteS(f, (STRING)" *", 2L);
          Idents_WriteIdent(f, W_28->WithId);
          IO_WriteS(f, (STRING)" = ", 3L);
          IO_WriteC(f, '(');
          Idents_WriteIdent(f, GenIdents_GenOpaque(W_28->Object->U_1.V_3.Object.CIdent));
          IO_WriteC(f, ')');
          CodeExpr(t->U_1.V_113.With.Designator->U_1.V_92.Deref.Designator, pCast);
          IO_WriteC(f, ';');
          IO_WriteNl(f);
          IO_WriteNl(f);
          Push(W_28->WithId, Defs_GetFields(t->U_1.V_113.With.Designator->U_1.V_92.Deref.Type));
          CodeStmts(t->U_1.V_113.With.Stmts);
          Pop();
          DEC1(Indent, 2);
          Layout_WriteSpaces(f, (LONGINT)Indent);
          IO_WriteC(f, '}');
          IO_WriteNl(f);
          CodeStmts(t->U_1.V_113.With.Next);
          return;
        } EXIT_56:;
      }
    }
    {
      register struct S_41 *W_29 = &yyTempo.U_1.V_4.yyR17;

      W_29->WithId = GenIdents_GenWith();
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteC(f, '{');
      IO_WriteNl(f);
      INC1(Indent, 2);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"register ", 9L);
      CodeTypeName(t->U_1.V_113.With.Designator->U_1.V_87.Designator.Type);
      IO_WriteS(f, (STRING)" *", 2L);
      Idents_WriteIdent(f, W_29->WithId);
      IO_WriteS(f, (STRING)" = ", 3L);
      CodeAdr(t->U_1.V_113.With.Designator);
      IO_WriteC(f, ';');
      IO_WriteNl(f);
      IO_WriteNl(f);
      Push(W_29->WithId, Defs_GetFields(t->U_1.V_113.With.Designator->U_1.V_87.Designator.Type));
      CodeStmts(t->U_1.V_113.With.Stmts);
      Pop();
      DEC1(Indent, 2);
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteC(f, '}');
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_113.With.Next);
      return;
    }
    break;
  case Tree_Exit:;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"goto ", 5L);
    Idents_WriteIdent(f, Top());
    IO_WriteC(f, ';');
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_114.Exit.Next);
    return;
    break;
  case Tree_Return1:;
    CodePop(LocalPtrs);
    CodeFree(ValueOpens);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"return;", 7L);
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_115.Return1.Next);
    return;
    break;
  case Tree_Return2:;
    {
      register struct S_40 *W_30 = &yyTempo.U_1.V_5.yyR20;

      if ((ValueOpens != Defs_NoCObjects || LocalPtrs != Defs_NoCObjects) && t->U_1.V_116.Return2.OpenAccessOrCall) {
        W_30->ReturnId = GenIdents_GenReturn();
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteC(f, '{');
        IO_WriteNl(f);
        INC1(Indent, 2);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        CodeTypeName(t->U_1.V_116.Return2.ResultType);
        IO_WriteC(f, ' ');
        Idents_WriteIdent(f, W_30->ReturnId);
        IO_WriteS(f, (STRING)" = ", 3L);
        if (Types_Cast(Code_cAssign, t->U_1.V_116.Return2.ResultType, t->U_1.V_116.Return2.Result->U_1.V_77.Expr.Type)) {
          CodeCast(t->U_1.V_116.Return2.ResultType);
          CodeExpr(t->U_1.V_116.Return2.Result, pCast);
          IO_WriteC(f, ';');
          IO_WriteNl(f);
        } else {
          CodeExpr(t->U_1.V_116.Return2.Result, pMinPrec);
          IO_WriteC(f, ';');
          IO_WriteNl(f);
        }
        IO_WriteNl(f);
        CodePop(LocalPtrs);
        CodeFree(ValueOpens);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteS(f, (STRING)"return ", 7L);
        Idents_WriteIdent(f, W_30->ReturnId);
        IO_WriteC(f, ';');
        IO_WriteNl(f);
        DEC1(Indent, 2);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteC(f, '}');
        IO_WriteNl(f);
      } else {
        CodePop(LocalPtrs);
        CodeFree(ValueOpens);
        Layout_WriteSpaces(f, (LONGINT)Indent);
        IO_WriteS(f, (STRING)"return ", 7L);
        if (Types_Cast(Code_cAssign, t->U_1.V_116.Return2.ResultType, t->U_1.V_116.Return2.Result->U_1.V_77.Expr.Type)) {
          CodeCast(t->U_1.V_116.Return2.ResultType);
          CodeExpr(t->U_1.V_116.Return2.Result, pCast);
        } else {
          CodeExpr(t->U_1.V_116.Return2.Result, pMinPrec);
        }
        IO_WriteC(f, ';');
        IO_WriteNl(f);
      }
      CodeStmts(t->U_1.V_116.Return2.Next);
      return;
    }
    break;
  case Tree_Elsifs1:;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"} else if (", 11L);
    CodeExpr(t->U_1.V_119.Elsifs1.Cond, pMinPrec);
    IO_WriteS(f, (STRING)") {", 3L);
    IO_WriteNl(f);
    INC1(Indent, 2);
    CodeStmts(t->U_1.V_119.Elsifs1.Stmts);
    DEC1(Indent, 2);
    CodeStmts(t->U_1.V_119.Elsifs1.Next);
    return;
    break;
  case Tree_Cases1:;
    CodeStmts(t->U_1.V_122.Cases1.Labels);
    INC1(Indent, 2);
    CodeStmts(t->U_1.V_122.Cases1.Stmts);
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"break;", 6L);
    IO_WriteNl(f);
    DEC1(Indent, 2);
    CodeStmts(t->U_1.V_122.Cases1.Next);
    return;
    break;
  case Tree_Label:;
    for (;;) {
      if (!t->U_1.V_126.Label.Label->U_1.V_77.Expr.IsCConst) {
        goto EXIT_57;
      }
      Layout_WriteSpaces(f, (LONGINT)Indent);
      IO_WriteS(f, (STRING)"case ", 5L);
      CodeExpr(t->U_1.V_126.Label.Label, pMinPrec);
      IO_WriteS(f, (STRING)":;", 2L);
      IO_WriteNl(f);
      CodeStmts(t->U_1.V_126.Label.Next);
      return;
    } EXIT_57:;
    Layout_WriteSpaces(f, (LONGINT)Indent);
    IO_WriteS(f, (STRING)"case ", 5L);
    CodeValue(t->U_1.V_126.Label.LabelVal);
    IO_WriteS(f, (STRING)":;", 2L);
    IO_WriteNl(f);
    CodeStmts(t->U_1.V_126.Label.Next);
    return;
    break;
  case Tree_LabelRange:;
    CodeLabelRange(t->U_1.V_127.LabelRange.LwbVal, t->U_1.V_127.LabelRange.UpbVal);
    CodeStmts(t->U_1.V_127.LabelRange.Next);
    return;
    break;
  default :
    break;
  }
}

static void CodeStdProc
# ifdef __STDC__
(Tree_tTree yyP6, SHORTCARD StdProc, Defs_tObject AllocOrDealloc)
# else
(yyP6, StdProc, AllocOrDealloc)
Tree_tTree yyP6;
SHORTCARD StdProc;
Defs_tObject AllocOrDealloc;
# endif
{
  struct S_45 yyTempo;

  if (yyP6->U_1.V_1.Kind == Tree_Actuals0) {
    if (StdProc == Defs_ProcHALT) {
      IO_WriteS(f, (STRING)"exit(1)", 7L);
    }
    return;
  }
  if (yyP6->U_1.V_1.Kind == Tree_Actual) {
    if (yyP6->U_1.V_101.Actual.Expr->U_1.V_1.Kind == Tree_Qualid0) {
      if (yyP6->U_1.V_101.Actual.Next->U_1.V_1.Kind == Tree_Actuals0) {
        for (;;) {
          if (!Defs_IsOpenArray(yyP6->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Object)) {
            goto EXIT_58;
          }
          if (StdProc == Defs_ProcHIGH) {
            if (yyP6->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.IsGlobalPtr) {
              IO_WriteC(f, '(');
              Idents_WriteIdent(f, GetGlobalPtr(yyP6->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Object));
              IO_WriteS(f, (STRING)"_O - 1)", 7L);
            } else {
              IO_WriteC(f, '(');
              Idents_WriteIdent(f, GetParam(yyP6->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Object));
              IO_WriteS(f, (STRING)" - 1)", 5L);
            }
          } else if (StdProc == Defs_ProcADR) {
            IO_WriteS(f, (STRING)"ADR1(", 5L);
            CodeExpr(yyP6->U_1.V_101.Actual.Expr, pPassValue);
            IO_WriteC(f, ')');
          } else if (StdProc == Defs_ProcSIZE) {
            IO_WriteS(f, (STRING)"(sizeof(", 8L);
            CodeTypeName(yyP6->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Type->U_1.V_38.Array1.ElemType);
            IO_WriteS(f, (STRING)") * ", 4L);
            if (yyP6->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.IsGlobalPtr) {
              Idents_WriteIdent(f, GetGlobalPtr(yyP6->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Object));
              IO_WriteS(f, (STRING)"_O", 2L);
            } else {
              Idents_WriteIdent(f, GetParam(yyP6->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Object));
            }
            IO_WriteC(f, ')');
          }
          return;
        } EXIT_58:;
      }
    }
    if (yyP6->U_1.V_101.Actual.Next->U_1.V_1.Kind == Tree_Actuals0) {
      {
        register struct S_46 *W_31 = &yyTempo.U_1.V_1.yyR3;

        if (StdProc == Defs_ProcHIGH) {
          W_31->Type = Defs_GetIndexType(yyP6->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type);
          Types_GetUpb(W_31->Type, &W_31->Upb);
          CodeValue(W_31->Upb);
          if (Defs_IsIntType(W_31->Type) && Types_Cast(Code_cPassValue, W_31->Type, Defs_TypeIntCard)) {
            IO_WriteC(f, 'L');
          }
        } else if (StdProc == Defs_ProcMIN) {
          CodeMIN(yyP6->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type);
        } else if (StdProc == Defs_ProcMAX) {
          CodeMAX(yyP6->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type);
        } else if (StdProc == Defs_ProcADR) {
          IO_WriteS(f, (STRING)"ADR(", 4L);
          CodeExpr(yyP6->U_1.V_101.Actual.Expr, pPassValue);
          IO_WriteC(f, ')');
        } else if (StdProc == Defs_ProcSIZE || StdProc == Defs_ProcTSIZE) {
          IO_WriteS(f, (STRING)"sizeof(", 7L);
          CodeExpr(yyP6->U_1.V_101.Actual.Expr, pPassValue);
          IO_WriteC(f, ')');
        } else if (StdProc == Defs_ProcABS) {
          if (yyP6->U_1.V_101.Actual.Expr->U_1.V_77.Expr.IsCConst) {
            IO_WriteS(f, (STRING)"ABS", 3L);
          } else {
            W_31->Type = yyP6->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type;
            if (W_31->Type->U_1.V_1.Kind == Defs_Subrange1) {
              W_31->Type = W_31->Type->U_1.V_44.Subrange1.Type;
            }
            switch (W_31->Type->U_1.V_1.Kind) {
            case Defs_ShortInt:;
              IO_WriteS(f, (STRING)"ABSSI", 5L);
              break;
            case Defs_LongInt:;
              IO_WriteS(f, (STRING)"ABSLI", 5L);
              break;
            case Defs_IntCard:;
              IO_WriteS(f, (STRING)"ABS", 3L);
              break;
            case Defs_ShortCard:;
              IO_WriteS(f, (STRING)"ABSSC", 5L);
              break;
            case Defs_LongCard:;
              IO_WriteS(f, (STRING)"ABSLC", 5L);
              break;
            case Defs_Real:;
              IO_WriteS(f, (STRING)"ABSR", 4L);
              break;
            case Defs_LongReal:;
              IO_WriteS(f, (STRING)"ABSLR", 5L);
              break;
            default :
              break;
            }
          }
          IO_WriteC(f, '(');
          CodeExpr(yyP6->U_1.V_101.Actual.Expr, pPassValue);
          IO_WriteC(f, ')');
        } else if (StdProc == Defs_ProcNEW || StdProc == Defs_ProcDISPOSE) {
          Idents_WriteIdent(f, AllocOrDealloc->U_1.V_3.Object.CIdent);
          IO_WriteC(f, '(');
          if (Types_Cast(Code_cPassAddress, Defs_TypeADDRESS, yyP6->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type)) {
            IO_WriteS(f, (STRING)"(ADDRESS *)", 11L);
          }
          CodeAdr(yyP6->U_1.V_101.Actual.Expr);
          IO_WriteS(f, (STRING)", ", 2L);
          if (Base_OptionIsSet('c')) {
            IO_WriteS(f, (STRING)"(LONGCARD)", 10L);
          }
          IO_WriteS(f, (STRING)"sizeof(", 7L);
          CodeTypeName(Defs_GetTargetType(yyP6->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type));
          IO_WriteC(f, ')');
          IO_WriteC(f, ')');
        } else {
          switch (StdProc) {
          case Defs_ProcCAP:;
            IO_WriteS(f, (STRING)"CAP", 3L);
            break;
          case Defs_ProcCHR:;
            IO_WriteS(f, (STRING)"CHR", 3L);
            break;
          case Defs_ProcDEC:;
            IO_WriteS(f, (STRING)"DEC", 3L);
            break;
          case Defs_ProcFLOAT:;
            IO_WriteS(f, (STRING)"FLOAT", 5L);
            break;
          case Defs_ProcINC:;
            IO_WriteS(f, (STRING)"INC", 3L);
            break;
          case Defs_ProcODD:;
            IO_WriteS(f, (STRING)"ODD", 3L);
            break;
          case Defs_ProcORD:;
            IO_WriteS(f, (STRING)"ORD", 3L);
            break;
          case Defs_ProcTRUNC:;
            IO_WriteS(f, (STRING)"TRUNC", 5L);
            break;
          default :
            break;
          }
          IO_WriteC(f, '(');
          CodeExpr(yyP6->U_1.V_101.Actual.Expr, pPassValue);
          IO_WriteC(f, ')');
        }
        return;
      }
    }
    if (yyP6->U_1.V_101.Actual.Next->U_1.V_1.Kind == Tree_Actual) {
      if (yyP6->U_1.V_101.Actual.Next->U_1.V_101.Actual.Next->U_1.V_1.Kind == Tree_Actuals0) {
        if (StdProc == Defs_ProcDEC || StdProc == Defs_ProcINC) {
          switch (StdProc) {
          case Defs_ProcDEC:;
            IO_WriteS(f, (STRING)"DEC1", 4L);
            break;
          case Defs_ProcINC:;
            IO_WriteS(f, (STRING)"INC1", 4L);
            break;
          }
          IO_WriteC(f, '(');
          CodeExpr(yyP6->U_1.V_101.Actual.Expr, pPassValue);
          IO_WriteS(f, (STRING)", ", 2L);
          if (yyP6->U_1.V_101.Actual.Next->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type == Defs_TypeADDRESS) {
            IO_WriteS(f, (STRING)"(LONGCARD)", 10L);
            CodeExpr(yyP6->U_1.V_101.Actual.Next->U_1.V_101.Actual.Expr, pCast);
          } else {
            CodeExpr(yyP6->U_1.V_101.Actual.Next->U_1.V_101.Actual.Expr, pPassValue);
          }
          IO_WriteC(f, ')');
        } else {
          switch (StdProc) {
          case Defs_ProcEXCL:;
            IO_WriteS(f, (STRING)"EXCL", 4L);
            break;
          case Defs_ProcINCL:;
            IO_WriteS(f, (STRING)"INCL", 4L);
            break;
          case Defs_ProcVAL:;
            IO_WriteS(f, (STRING)"VAL", 3L);
            break;
          default :
            break;
          }
          IO_WriteC(f, '(');
          CodeExpr(yyP6->U_1.V_101.Actual.Expr, pPassValue);
          IO_WriteS(f, (STRING)", ", 2L);
          CodeExpr(yyP6->U_1.V_101.Actual.Next->U_1.V_101.Actual.Expr, pPassValue);
          IO_WriteC(f, ')');
        }
        return;
      }
    }
  }
}

static void CodeActuals
# ifdef __STDC__
(Tree_tTree t)
# else
(t)
Tree_tTree t;
# endif
{
  struct S_47 yyTempo;

  if (t->U_1.V_1.Kind == Tree_Actual) {
    for (;;) {
      if (!((t->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type == Defs_TypeSTRING || t->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type == Defs_TypeStringChar) && t->U_1.V_101.Actual.Formal->U_1.V_1.Kind == Defs_Array1 && !Defs_IsOpen(t->U_1.V_101.Actual.Formal))) {
        goto EXIT_59;
      }
      Idents_WriteIdent(f, t->U_1.V_101.Actual.String);
      CodeSepa(t->U_1.V_101.Actual.Next);
      CodeActuals(t->U_1.V_101.Actual.Next);
      return;
    } EXIT_59:;
    if (t->U_1.V_101.Actual.Expr->U_1.V_1.Kind == Tree_Qualid0) {
      for (;;) {
        if (!(Defs_IsOpen(t->U_1.V_101.Actual.Formal) && Defs_IsOpenArray(t->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Object))) {
          goto EXIT_60;
        }
        CodeExpr(t->U_1.V_101.Actual.Expr, pPassValue);
        IO_WriteS(f, (STRING)", ", 2L);
        if (t->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.IsGlobalPtr) {
          Idents_WriteIdent(f, GetGlobalPtr(t->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Object));
          IO_WriteS(f, (STRING)"_O", 2L);
        } else {
          Idents_WriteIdent(f, GetParam(t->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Object));
        }
        if (t->U_1.V_101.Actual.Formal->U_1.V_38.Array1.ElemType == Defs_TypeWORD) {
          IO_WriteS(f, (STRING)" * sizeof(", 10L);
          CodeTypeName(t->U_1.V_101.Actual.Expr->U_1.V_89.Qualid0.Type->U_1.V_38.Array1.ElemType);
          IO_WriteC(f, ')');
        }
        CodeSepa(t->U_1.V_101.Actual.Next);
        CodeActuals(t->U_1.V_101.Actual.Next);
        return;
      } EXIT_60:;
    }
    if (t->U_1.V_101.Actual.Expr->U_1.V_1.Kind == Tree_StringConst) {
      {
        register struct S_50 *W_32 = &yyTempo.U_1.V_1.yyR3;

        for (;;) {
          if (!Defs_IsOpen(t->U_1.V_101.Actual.Formal)) {
            goto EXIT_61;
          }
          CodeString2(t->U_1.V_101.Actual.Expr, (BOOLEAN)(!Strncpy));
          IO_WriteS(f, (STRING)", ", 2L);
          W_32->length = StringMem_Length(t->U_1.V_101.Actual.Expr->U_1.V_82.StringConst.StringVal);
          if (W_32->length == 0) {
            W_32->length = 1;
          }
          IO_WriteI(f, (LONGINT)W_32->length, 1L);
          IO_WriteC(f, 'L');
          CodeSepa(t->U_1.V_101.Actual.Next);
          CodeActuals(t->U_1.V_101.Actual.Next);
          return;
        } EXIT_61:;
      }
    }
    if (t->U_1.V_101.Actual.Expr->U_1.V_1.Kind == Tree_CharConst) {
      for (;;) {
        if (!Defs_IsOpen(t->U_1.V_101.Actual.Formal)) {
          goto EXIT_62;
        }
        CodeString2(t->U_1.V_101.Actual.Expr, (BOOLEAN)(!Strncpy));
        IO_WriteS(f, (STRING)", 1L", 4L);
        CodeSepa(t->U_1.V_101.Actual.Next);
        CodeActuals(t->U_1.V_101.Actual.Next);
        return;
      } EXIT_62:;
    }
    if (Tree_IsType(t->U_1.V_101.Actual.Expr, Tree_Qualid)) {
      {
        register struct S_49 *W_33 = &yyTempo.U_1.V_2.yyR5;

        for (;;) {
          if (!(Defs_IsOpen(t->U_1.V_101.Actual.Formal) && t->U_1.V_101.Actual.Expr->U_1.V_88.Qualid.Type == Defs_TypeSTRING)) {
            goto EXIT_63;
          }
          CodeString2(t->U_1.V_101.Actual.Expr, (BOOLEAN)(!Strncpy));
          IO_WriteS(f, (STRING)", ", 2L);
          W_33->length = StringMem_Length(t->U_1.V_101.Actual.Expr->U_1.V_88.Qualid.Object->U_1.V_5.Const1.Value.U_1.V_5.StringValue);
          if (W_33->length == 0) {
            W_33->length = 1;
          }
          IO_WriteI(f, (LONGINT)W_33->length, 1L);
          IO_WriteC(f, 'L');
          CodeSepa(t->U_1.V_101.Actual.Next);
          CodeActuals(t->U_1.V_101.Actual.Next);
          return;
        } EXIT_63:;
      }
      for (;;) {
        if (!(Defs_IsOpen(t->U_1.V_101.Actual.Formal) && t->U_1.V_101.Actual.Expr->U_1.V_88.Qualid.Type == Defs_TypeStringChar)) {
          goto EXIT_64;
        }
        CodeString2(t->U_1.V_101.Actual.Expr, (BOOLEAN)(!Strncpy));
        IO_WriteS(f, (STRING)", 1L", 4L);
        CodeSepa(t->U_1.V_101.Actual.Next);
        CodeActuals(t->U_1.V_101.Actual.Next);
        return;
      } EXIT_64:;
      for (;;) {
        if (!(Defs_IsOpen(t->U_1.V_101.Actual.Formal) && t->U_1.V_101.Actual.Expr->U_1.V_88.Qualid.Type == Defs_TypeCHAR)) {
          goto EXIT_65;
        }
        CodeString2(t->U_1.V_101.Actual.Expr, (BOOLEAN)(!Strncpy));
        IO_WriteS(f, (STRING)", 1L", 4L);
        CodeSepa(t->U_1.V_101.Actual.Next);
        CodeActuals(t->U_1.V_101.Actual.Next);
        return;
      } EXIT_65:;
    }
    for (;;) {
      if (!(Defs_IsOpen(t->U_1.V_101.Actual.Formal) && t->U_1.V_101.Actual.Formal->U_1.V_38.Array1.ElemType == Defs_TypeWORD)) {
        goto EXIT_66;
      }
      if (Base_OptionIsSet('c')) {
        IO_WriteS(f, (STRING)"(WORD *)", 8L);
      }
      CodeAdr(t->U_1.V_101.Actual.Expr);
      IO_WriteS(f, (STRING)", ", 2L);
      if (Base_OptionIsSet('c')) {
        IO_WriteS(f, (STRING)"(LONGCARD)", 10L);
      }
      IO_WriteS(f, (STRING)"sizeof(", 7L);
      CodeExpr(t->U_1.V_101.Actual.Expr, pPassValue);
      IO_WriteC(f, ')');
      CodeSepa(t->U_1.V_101.Actual.Next);
      CodeActuals(t->U_1.V_101.Actual.Next);
      return;
    } EXIT_66:;
    {
      register struct S_48 *W_34 = &yyTempo.U_1.V_3.yyR9;

      for (;;) {
        if (!(Defs_IsOpen(t->U_1.V_101.Actual.Formal) && t->U_1.V_101.Actual.Expr->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_Array1)) {
          goto EXIT_67;
        }
        W_34->IndexType = t->U_1.V_101.Actual.Expr->U_1.V_87.Designator.Type->U_1.V_38.Array1.IndexType;
        Types_GetLwb(W_34->IndexType, &W_34->Lwb);
        Types_GetUpb(W_34->IndexType, &W_34->Upb);
        CodeExpr(t->U_1.V_101.Actual.Expr, pSelect);
        IO_WriteS(f, (STRING)".A, ", 4L);
        IO_WriteI(f, Values_ValueToInt(W_34->Upb) - Values_ValueToInt(W_34->Lwb) + 1, 1L);
        IO_WriteC(f, 'L');
        CodeSepa(t->U_1.V_101.Actual.Next);
        CodeActuals(t->U_1.V_101.Actual.Next);
        return;
      } EXIT_67:;
    }
    for (;;) {
      if (!t->U_1.V_101.Actual.IsVAR) {
        goto EXIT_68;
      }
      if (Types_Cast(Code_cPassAddress, t->U_1.V_101.Actual.Formal, t->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type)) {
        CodeCastVAR(t->U_1.V_101.Actual.Formal);
        CodeAdr(t->U_1.V_101.Actual.Expr);
      } else {
        CodeAdr(t->U_1.V_101.Actual.Expr);
      }
      CodeSepa(t->U_1.V_101.Actual.Next);
      CodeActuals(t->U_1.V_101.Actual.Next);
      return;
    } EXIT_68:;
    if (t->U_1.V_101.Actual.Expr->U_1.V_1.Kind == Tree_IntConst) {
      CodeExpr(t->U_1.V_101.Actual.Expr, pPassValue);
      if (Types_Cast(Code_cPassValue, t->U_1.V_101.Actual.Formal, Defs_TypeIntCard)) {
        IO_WriteC(f, 'L');
      }
      CodeSepa(t->U_1.V_101.Actual.Next);
      CodeActuals(t->U_1.V_101.Actual.Next);
      return;
    }
    if (t->U_1.V_101.Actual.Expr->U_1.V_1.Kind == Tree_Unary) {
      if (t->U_1.V_101.Actual.Expr->U_1.V_79.Unary.Mop->U_1.V_1.Kind == Tree_IntConst) {
        for (;;) {
          if (!(t->U_1.V_101.Actual.Expr->U_1.V_79.Unary.COperator == Code_cMinus)) {
            goto EXIT_69;
          }
          CodeExpr(t->U_1.V_101.Actual.Expr, pPassValue);
          if (Types_Cast(Code_cPassValue, t->U_1.V_101.Actual.Formal, Defs_TypeIntCard)) {
            IO_WriteC(f, 'L');
          }
          CodeSepa(t->U_1.V_101.Actual.Next);
          CodeActuals(t->U_1.V_101.Actual.Next);
          return;
        } EXIT_69:;
      }
      for (;;) {
        if (!(t->U_1.V_101.Actual.Expr->U_1.V_79.Unary.Type == Defs_TypeBOOLEAN)) {
          goto EXIT_70;
        }
        if (Base_OptionIsSet('c')) {
          IO_WriteS(f, (STRING)"(BOOLEAN)", 9L);
          CodeExpr(t->U_1.V_101.Actual.Expr, pCast);
        } else {
          CodeExpr(t->U_1.V_101.Actual.Expr, pPassValue);
        }
        CodeSepa(t->U_1.V_101.Actual.Next);
        CodeActuals(t->U_1.V_101.Actual.Next);
        return;
      } EXIT_70:;
    }
    if (t->U_1.V_101.Actual.Expr->U_1.V_1.Kind == Tree_Binary) {
      for (;;) {
        if (!(t->U_1.V_101.Actual.Expr->U_1.V_78.Binary.Type == Defs_TypeBOOLEAN)) {
          goto EXIT_71;
        }
        if (Base_OptionIsSet('c')) {
          IO_WriteS(f, (STRING)"(BOOLEAN)", 9L);
          CodeExpr(t->U_1.V_101.Actual.Expr, pCast);
        } else {
          CodeExpr(t->U_1.V_101.Actual.Expr, pPassValue);
        }
        CodeSepa(t->U_1.V_101.Actual.Next);
        CodeActuals(t->U_1.V_101.Actual.Next);
        return;
      } EXIT_71:;
    }
    if (Types_Cast(Code_cPassValue, t->U_1.V_101.Actual.Formal, t->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type)) {
      CodeCast(t->U_1.V_101.Actual.Formal);
      CodeExpr(t->U_1.V_101.Actual.Expr, pCast);
    } else {
      CodeExpr(t->U_1.V_101.Actual.Expr, pPassValue);
    }
    CodeSepa(t->U_1.V_101.Actual.Next);
    CodeActuals(t->U_1.V_101.Actual.Next);
    return;
  }
}

static void CodeSepa
# ifdef __STDC__
(Tree_tTree yyP7)
# else
(yyP7)
Tree_tTree yyP7;
# endif
{
  struct S_51 yyTempo;

  switch (yyP7->U_1.V_1.Kind) {
  case Tree_VarIds1:;
    IO_WriteS(f, (STRING)", ", 2L);
    return;
    break;
  case Tree_ParIds1:;
    IO_WriteS(f, (STRING)", ", 2L);
    return;
    break;
  case Tree_FieldIds1:;
    IO_WriteS(f, (STRING)", ", 2L);
    return;
    break;
  case Tree_Formals1:;
    IO_WriteS(f, (STRING)", ", 2L);
    return;
    break;
  case Tree_FormalType:;
    IO_WriteS(f, (STRING)", ", 2L);
    return;
    break;
  case Tree_Actual:;
    IO_WriteS(f, (STRING)", ", 2L);
    return;
    break;
  case Tree_Elems1:;
  case Tree_Elem:;
  case Tree_ElemRange:;
    IO_WriteS(f, (STRING)" | ", 3L);
    return;
    break;
  default :
    break;
  }
}

void Code_BeginCode
# ifdef __STDC__
()
# else
()
# endif
{
  Indent = 0;
  Precedence.A[Code_cNoOp] = pMinPrec;
  Precedence.A[Code_cNotEqual] = pNotEqual;
  Precedence.A[Code_cTimes] = pTimes;
  Precedence.A[Code_cPlus] = pPlus;
  Precedence.A[Code_cMinus] = pMinus;
  Precedence.A[Code_cDivide] = pDivide;
  Precedence.A[Code_cLess] = pLess;
  Precedence.A[Code_cLessEqual] = pLessEqual;
  Precedence.A[Code_cEqual] = pEqual;
  Precedence.A[Code_cGreater] = pGreater;
  Precedence.A[Code_cGreaterEqual] = pGreaterEqual;
  Precedence.A[Code_cAnd] = pAnd;
  Precedence.A[Code_cIn] = pIn;
  Precedence.A[Code_cMod] = pMod;
  Precedence.A[Code_cNot] = pNot;
  Precedence.A[Code_cOr] = pOr;
  Precedence.A[Code_cUnion] = pUnion;
  Precedence.A[Code_cDifference] = pDifference;
  Precedence.A[Code_cIntersection] = pIntersection;
  Precedence.A[Code_cSymDiff] = pSymDiff;
  Precedence.A[Code_cIsSubset1] = pIsSubset1;
  Precedence.A[Code_cIsSubset2] = pIsSubset2;
  Precedence.A[Code_cAssign] = pAssign;
  Precedence.A[Code_cPassValue] = pPassValue;
  Precedence.A[Code_cPassAddress] = pPassAddress;
  AssocTab_BeginAssocTab();
  C_14_Begin();
  Begin();
}

void Code_CloseCode
# ifdef __STDC__
()
# else
()
# endif
{
  AssocTab_CloseAssocTab();
  C_15_Close();
  C_6_Close();
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

void BEGIN_Code()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_Tree();
    BEGIN_System();
    BEGIN_IO();
    BEGIN_Tree();
    BEGIN_Base();
    BEGIN_IO();
    BEGIN_Layout();
    BEGIN_Strings();
    BEGIN_StringMem();
    BEGIN_Idents();
    BEGIN_Tree();
    BEGIN_Defs();
    BEGIN_Values();
    BEGIN_Types();
    BEGIN_AssocTab();
    BEGIN_GenIdents();
    BEGIN_Errors();
    BEGIN_Strings();
    BEGIN_Idents();
    BEGIN_GenIdents();
    BEGIN_DynArray();
    BEGIN_Defs();
    BEGIN_Values();
    LabelStack();
    WithStack();
    ParamTable();
    PointerTable();

    Code_yyf = IO_StdOutput;
    Code_Exit = yyExit;
    Code_BeginCode();
  }
}
