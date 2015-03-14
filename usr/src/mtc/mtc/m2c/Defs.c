#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Layout
#include "Layout.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Texts
#include "Texts.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

Defs_tObject Defs_NoObject;
Defs_tType Defs_NoType;
Defs_tObjects Defs_Predefs;
Defs_tObject Defs_ModuleSYSTEM;
Idents_tIdent Defs_IdentALLOC, Defs_IdentDEALLOC, Defs_IdentSYSTEM, Defs_IdentLONGCARD;
Defs_tType Defs_TypeSHORTINT, Defs_TypeLONGINT, Defs_TypeSHORTCARD, Defs_TypeLONGCARD, Defs_TypeREAL, Defs_TypeLONGREAL, Defs_TypeBOOLEAN, Defs_TypeCHAR, Defs_TypeBITSET, Defs_TypePROC, Defs_TypeWORD, Defs_TypeADDRESS, Defs_TypeIntCard, Defs_TypeNIL, Defs_TypeSTRING, Defs_TypeStringChar, Defs_TypeVOID;
Defs_tDefs Defs_DefsRoot;
LONGCARD Defs_HeapUsed;
ADDRESS Defs_yyPoolFreePtr, Defs_yyPoolMaxPtr;
struct Defs_53 Defs_yyNodeSize;
PROC Defs_yyExit;

static Strings_tString Str;
static void CheckCycle1 ARGS((Defs_tType Type, BOOLEAN *Cycle));
static void CheckCycle2 ARGS((Defs_tObject Object, BOOLEAN *Cycle));
static Defs_tType GroundType1 ARGS((Defs_tType Type));
static Defs_tType GroundType2 ARGS((Defs_tObject Object));
static Defs_tObjects Result;
static void Filter2 ARGS((Defs_tObjects Objects));
static Defs_tCObjects Result1;
static void Pointers2 ARGS((BOOLEAN GlobalPtrs, Defs_tObjects Objects));
static Defs_tCObjects ValueOpens, VAROpens;
static void OpenArrays2 ARGS((Defs_tObjects Objects));
static Defs_tObjects Predef ARGS(());
static void BasicType ARGS((CHAR Name[], LONGCARD , Defs_tType Type));
static void StdConst ARGS((CHAR Name[], LONGCARD , Values_tValue Value));
static void StdProcedure ARGS((CHAR Name[], LONGCARD , SHORTCARD StdProc));
#define yyBlockSize	20480
typedef struct S_1 *yytBlockPtr;
typedef struct S_1 {
    struct S_2 {
        CHAR A[yyBlockSize - 1 + 1];
    } yyBlock;
    yytBlockPtr yySuccessor;
} yytBlock;
static yytBlockPtr yyBlockList;
static SHORTCARD yyMaxSize, yyi;
static struct S_3 {
    SHORTCARD A[50 + 1];
} yyTypeRange;
typedef Defs_tDefs *yyPtrtTree;
static IO_tFile yyf;
static SHORTCARD yyLabel;
static SHORTCARD yyKind;
static CHAR yyc;
static Strings_tString yys;
#define yyNil	((CHAR)'\374')
#define yyNoLabel	((CHAR)'\375')
#define yyLabelDef	((CHAR)'\376')
#define yyLabelUse	((CHAR)'\377')
static void xxExit ARGS(());

static Defs_tObjects *G_1_Objects;
static Strings_tString *G_2_String;
static Idents_tIdent *G_3_Ident;
static Defs_tObject *G_4_Object;

Defs_tObjects Defs_UNION
# ifdef __STDC__
(Defs_tObjects Objects1, Defs_tObjects Objects2)
# else
(Objects1, Objects2)
Defs_tObjects Objects1, Objects2;
# endif
{
  if (Objects1 == Defs_NoObjects) {
    return Objects2;
  } else if (Objects2 == Defs_NoObjects) {
    return Objects1;
  } else {
    return Defs_mUnion(Objects1, Objects2);
  }
}

Defs_tObject Defs_mConst2
# ifdef __STDC__
(Defs_tObject Object, Values_tValue Value)
# else
(Object, Value)
Defs_tObject Object;
Values_tValue Value;
# endif
{
  Object->U_1.V_5.Const1.Value = Value;
  return Object;
}

Defs_tObject Defs_mModule2
# ifdef __STDC__
(Defs_tObject Object, Defs_tObjects Objects)
# else
(Object, Objects)
Defs_tObject Object;
Defs_tObjects Objects;
# endif
{
  Object->U_1.V_8.Module1.Objects = Objects;
  return Object;
}

Defs_tObject Defs_mModule3
# ifdef __STDC__
(Defs_tObject Object, Defs_tObjects Locals)
# else
(Object, Locals)
Defs_tObject Object;
Defs_tObjects Locals;
# endif
{
  Object->U_1.V_8.Module1.Locals = Locals;
  return Object;
}

Defs_tObject Defs_mTypeDecl2
# ifdef __STDC__
(Defs_tObject Object, Defs_tType Type)
# else
(Object, Type)
Defs_tObject Object;
Defs_tType Type;
# endif
{
  Object->U_1.V_11.TypeDecl1.Type = Type;
  return Object;
}

Defs_tObject Defs_mTypeDecl3
# ifdef __STDC__
(Defs_tObject Object, Defs_tType Type)
# else
(Object, Type)
Defs_tObject Object;
Defs_tType Type;
# endif
{
  Object->U_1.V_11.TypeDecl1.Type = Type;
  return Object;
}

Defs_tObject Defs_mOpaque2
# ifdef __STDC__
(Defs_tObject Object, Defs_tType Type)
# else
(Object, Type)
Defs_tObject Object;
Defs_tType Type;
# endif
{
  Object->U_1.V_12.Opaque1.Type = Type;
  return Object;
}

Defs_tObject Defs_mOpaque3
# ifdef __STDC__
(Defs_tObject Object, Defs_tType Type)
# else
(Object, Type)
Defs_tObject Object;
Defs_tType Type;
# endif
{
  Object->U_1.V_12.Opaque1.Type = Type;
  return Object;
}

Defs_tObject Defs_mProc2
# ifdef __STDC__
(Defs_tObject Object, Defs_tObjects Locals, BOOLEAN IsExported)
# else
(Object, Locals, IsExported)
Defs_tObject Object;
Defs_tObjects Locals;
BOOLEAN IsExported;
# endif
{
  Object->U_1.V_9.Proc1.Locals = Locals;
  Object->U_1.V_9.Proc1.IsExported = IsExported;
  return Object;
}

Defs_tType Defs_mOpaqueType2
# ifdef __STDC__
(Defs_tType Type, Defs_tType FullType)
# else
(Type, FullType)
Defs_tType Type;
Defs_tType FullType;
# endif
{
  Type->U_1.V_37.OpaqueType1.Type = FullType;
  return Type;
}

Defs_tType Defs_mArray2
# ifdef __STDC__
(Defs_tType Type, BOOLEAN pIsOpen, Defs_tType pIndexType, Defs_tType pElemType)
# else
(Type, pIsOpen, pIndexType, pElemType)
Defs_tType Type;
BOOLEAN pIsOpen;
Defs_tType pIndexType, pElemType;
# endif
{
  {
    register Defs_yArray1 *W_1 = &Type->U_1.V_38.Array1;

    W_1->IsOpen = pIsOpen;
    W_1->IndexType = pIndexType;
    W_1->ElemType = pElemType;
  }
  return Type;
}

Defs_tType Defs_mEnumeration2
# ifdef __STDC__
(Defs_tType Type, Defs_tObjects EnumLiterals, SHORTCARD MaxValue)
# else
(Type, EnumLiterals, MaxValue)
Defs_tType Type;
Defs_tObjects EnumLiterals;
SHORTCARD MaxValue;
# endif
{
  Type->U_1.V_39.Enumeration1.Objects = EnumLiterals;
  Type->U_1.V_39.Enumeration1.MaxValue = MaxValue;
  return Type;
}

Defs_tType Defs_mPointer2
# ifdef __STDC__
(Defs_tType Type, Defs_tType TargetType)
# else
(Type, TargetType)
Defs_tType Type;
Defs_tType TargetType;
# endif
{
  Type->U_1.V_40.Pointer1.Type = TargetType;
  return Type;
}

Defs_tType Defs_mProcType2
# ifdef __STDC__
(Defs_tType Type, Defs_tTypes FormalTypes, Defs_tType ResultType)
# else
(Type, FormalTypes, ResultType)
Defs_tType Type;
Defs_tTypes FormalTypes;
Defs_tType ResultType;
# endif
{
  {
    register Defs_yProcType1 *W_2 = &Type->U_1.V_41.ProcType1;

    W_2->Types = FormalTypes;
    W_2->Type = ResultType;
  }
  return Type;
}

Defs_tType Defs_mRecord2
# ifdef __STDC__
(Defs_tType Type, Defs_tObjects Fields)
# else
(Type, Fields)
Defs_tType Type;
Defs_tObjects Fields;
# endif
{
  Type->U_1.V_42.Record1.Objects = Fields;
  return Type;
}

Defs_tType Defs_mSet2
# ifdef __STDC__
(Defs_tType Type, Defs_tType BaseType)
# else
(Type, BaseType)
Defs_tType Type;
Defs_tType BaseType;
# endif
{
  Type->U_1.V_43.Set1.Type = BaseType;
  return Type;
}

Defs_tType Defs_mSubrange2
# ifdef __STDC__
(Defs_tType Type, Defs_tType BaseType)
# else
(Type, BaseType)
Defs_tType Type;
Defs_tType BaseType;
# endif
{
  Type->U_1.V_44.Subrange1.Type = BaseType;
  return Type;
}

Defs_tType Defs_mSubrange3
# ifdef __STDC__
(Defs_tType Type, Values_tValue Lwb, Values_tValue Upb)
# else
(Type, Lwb, Upb)
Defs_tType Type;
Values_tValue Lwb, Upb;
# endif
{
  if (Type->U_1.V_44.Subrange1.Type == Defs_TypeVOID) {
    switch (Lwb.Kind) {
    case Values_Integer:;
      if (Values_ValueToInt(Lwb) < 0) {
        Type->U_1.V_44.Subrange1.Type = Defs_TypeLONGINT;
      } else {
        Type->U_1.V_44.Subrange1.Type = Defs_TypeLONGCARD;
      }
      break;
    case Values_Boolean:;
      Type->U_1.V_44.Subrange1.Type = Defs_TypeBOOLEAN;
      break;
    case Values_Char:;
    case Values_StringChar:;
      Type->U_1.V_44.Subrange1.Type = Defs_TypeCHAR;
      break;
    case Values_Enumeration:;
      Type->U_1.V_44.Subrange1.Type = Defs_GetType((Defs_tDefs)Lwb.U_1.V_7.EnumValue);
      break;
    default :
      break;
    }
  }
  return Type;
}

Idents_tIdent Defs_DefineCIdent
# ifdef __STDC__
(Defs_tObject Object, Idents_tIdent CIdent)
# else
(Object, CIdent)
Defs_tObject Object;
Idents_tIdent CIdent;
# endif
{
  Object->U_1.V_3.Object.CIdent = CIdent;
  return CIdent;
}

BOOLEAN Defs_NestedUse
# ifdef __STDC__
(Defs_tObject Object, SHORTCARD Level)
# else
(Object, Level)
Defs_tObject Object;
SHORTCARD Level;
# endif
{
  if (Object->U_1.V_1.Kind != Defs_Var1 || Object->U_1.V_13.Var1.Level == 0 || Object->U_1.V_13.Var1.Level == Level) {
    return FALSE;
  } else {
    Object->U_1.V_13.Var1.NestedUse = TRUE;
    return TRUE;
  }
}

Defs_tVoid Defs_mVoid1
# ifdef __STDC__
(Defs_tObject Object, Defs_tVoid Void)
# else
(Object, Void)
Defs_tObject Object;
Defs_tVoid Void;
# endif
{
  return Defs_cVoid;
}

Defs_tVoid Defs_mVoid2
# ifdef __STDC__
(Defs_tVoid Void1, Defs_tVoid Void2)
# else
(Void1, Void2)
Defs_tVoid Void1, Void2;
# endif
{
  return Defs_cVoid;
}

static void CheckCycle1
# ifdef __STDC__
(Defs_tType Type, BOOLEAN *Cycle)
# else
(Type, Cycle)
Defs_tType Type;
BOOLEAN *Cycle;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_Qualident1) {
    if (Type->U_1.V_2.yyHead.yyMark != 0) {
      *Cycle = TRUE;
    } else {
      Type->U_1.V_2.yyHead.yyMark = 1;
      CheckCycle2(Type->U_1.V_35.Qualident1.Object, Cycle);
      Type->U_1.V_2.yyHead.yyMark = 0;
    }
  } else {
    *Cycle = FALSE;
  }
}

static void CheckCycle2
# ifdef __STDC__
(Defs_tObject Object, BOOLEAN *Cycle)
# else
(Object, Cycle)
Defs_tObject Object;
BOOLEAN *Cycle;
# endif
{
  if (Object->U_1.V_1.Kind == Defs_TypeDecl1) {
    CheckCycle1(Object->U_1.V_11.TypeDecl1.Type, Cycle);
  } else {
    *Cycle = FALSE;
  }
}

Defs_tType Defs_GroundType
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  BOOLEAN Cycle;

  CheckCycle1(Type, &Cycle);
  if (Cycle) {
    return Defs_NoType;
  } else {
    return GroundType1(Type);
  }
}

static Defs_tType GroundType1
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_Qualident1) {
    return GroundType2(Type->U_1.V_35.Qualident1.Object);
  }
  return Type;
}

static Defs_tType GroundType2
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  if (Object->U_1.V_1.Kind == Defs_TypeDecl1) {
    return Defs_GroundType(Object->U_1.V_11.TypeDecl1.Type);
  } else if (Object->U_1.V_1.Kind == Defs_Opaque1) {
    return Object->U_1.V_12.Opaque1.Type;
  }
  return Defs_NoType;
}

Defs_tObject Defs_Identify
# ifdef __STDC__
(Idents_tIdent Ident, Defs_tEnv Env)
# else
(Ident, Env)
Idents_tIdent Ident;
Defs_tEnv Env;
# endif
{
  Defs_tDefs Object;

  while (Env != Defs_NoEnv) {
    Object = Defs_Identify2(Ident, Env->U_1.V_50.Env.Objects);
    if (Object != Defs_NoObject) {
      return Object;
    }
    Env = Env->U_1.V_50.Env.HiddenEnv;
  }
  return Defs_NoObject;
}

Defs_tObject Defs_Identify2
# ifdef __STDC__
(Idents_tIdent Ident, Defs_tObjects Objects)
# else
(Ident, Objects)
Idents_tIdent Ident;
Defs_tObjects Objects;
# endif
{
  Defs_tDefs Object;

  while (Objects != Defs_NoObjects) {
    if (Objects->U_1.V_1.Kind != Defs_Union) {
      if (Objects->U_1.V_46.Elmt.Ident == Ident) {
        return Objects->U_1.V_46.Elmt.Object;
      }
      Objects = Objects->U_1.V_46.Elmt.Next;
    } else {
      Object = Defs_Identify2(Ident, Objects->U_1.V_47.Union.Objects1);
      if (Object != Defs_NoObject) {
        return Object;
      }
      Objects = Objects->U_1.V_47.Union.Objects2;
    }
  }
  return Defs_NoObject;
}

BOOLEAN Defs_IsDeclared
# ifdef __STDC__
(Idents_tIdent Ident, Defs_tObjects Objects)
# else
(Ident, Objects)
Idents_tIdent Ident;
Defs_tObjects Objects;
# endif
{
  return Defs_Identify2(Ident, Objects) != Defs_NoObject;
}

Idents_tIdent Defs_LookUp
# ifdef __STDC__
(Defs_tObject M2Object, Defs_tCObjects Objects)
# else
(M2Object, Objects)
Defs_tObject M2Object;
Defs_tCObjects Objects;
# endif
{
  while (Objects != Defs_NoCObjects) {
    if (Objects->U_1.V_48.CObjects.M2Object == M2Object) {
      return Objects->U_1.V_48.CObjects.CObject;
    }
    Objects = Objects->U_1.V_48.CObjects.Next;
  }
  return Idents_NoIdent;
}

Defs_tObjects Defs_Filter
# ifdef __STDC__
(Defs_tObjects Objects)
# else
(Objects)
Defs_tObjects Objects;
# endif
{
  Result = Defs_NoObjects;
  Filter2(Objects);
  return Result;
}

static void Filter2
# ifdef __STDC__
(Defs_tObjects Objects)
# else
(Objects)
Defs_tObjects Objects;
# endif
{
  while (Objects != Defs_NoObjects) {
    if (Objects->U_1.V_1.Kind != Defs_Union) {
      {
        register Defs_yElmt *W_3 = &Objects->U_1.V_46.Elmt;

        switch (W_3->Object->U_1.V_1.Kind) {
        case Defs_Const1:;
        case Defs_EnumLiteral1:;
        case Defs_TypeDecl1:;
        case Defs_Var1:;
          Result = Defs_mElmt(W_3->Ident, FALSE, W_3->Object, Result);
          break;
        default :
          break;
        }
      }
      Objects = Objects->U_1.V_46.Elmt.Next;
    } else {
      Filter2(Objects->U_1.V_47.Union.Objects1);
      Objects = Objects->U_1.V_47.Union.Objects2;
    }
  }
}

Defs_tCObjects Defs_Pointers
# ifdef __STDC__
(BOOLEAN GlobalPtrs, Defs_tObjects Objects)
# else
(GlobalPtrs, Objects)
BOOLEAN GlobalPtrs;
Defs_tObjects Objects;
# endif
{
  Result1 = Defs_NoCObjects;
  Pointers2(GlobalPtrs, Objects);
  return Result1;
}

static void Pointers2
# ifdef __STDC__
(BOOLEAN GlobalPtrs, Defs_tObjects Objects)
# else
(GlobalPtrs, Objects)
BOOLEAN GlobalPtrs;
Defs_tObjects Objects;
# endif
{
  Defs_tObject Object;

  while (Objects != Defs_NoObjects) {
    if (Objects->U_1.V_1.Kind == Defs_Elmt) {
      if (!Objects->U_1.V_46.Elmt.IsPseudoObj) {
        Object = Objects->U_1.V_46.Elmt.Object;
        switch (Object->U_1.V_1.Kind) {
        case Defs_Var1:;
          if (Object->U_1.V_13.Var1.NestedUse) {
            Result1 = Defs_mCObjects(Object, Result1);
          }
          break;
        case Defs_Proc1:;
          if (GlobalPtrs) {
            Pointers2(GlobalPtrs, Object->U_1.V_9.Proc1.Locals);
          }
          break;
        case Defs_Module1:;
          Pointers2(GlobalPtrs, Object->U_1.V_8.Module1.Locals);
          break;
        default :
          break;
        }
      }
      Objects = Objects->U_1.V_46.Elmt.Next;
    } else {
      Pointers2(GlobalPtrs, Objects->U_1.V_47.Union.Objects1);
      Objects = Objects->U_1.V_47.Union.Objects2;
    }
  }
}

void Defs_OpenArrays
# ifdef __STDC__
(Defs_tObjects Objects, Defs_tCObjects *pValueOpens, Defs_tCObjects *pVAROpens)
# else
(Objects, pValueOpens, pVAROpens)
Defs_tObjects Objects;
Defs_tCObjects *pValueOpens, *pVAROpens;
# endif
{
  ValueOpens = Defs_NoCObjects;
  VAROpens = Defs_NoCObjects;
  OpenArrays2(Objects);
  *pValueOpens = ValueOpens;
  *pVAROpens = VAROpens;
}

static void OpenArrays2
# ifdef __STDC__
(Defs_tObjects Objects)
# else
(Objects)
Defs_tObjects Objects;
# endif
{
  while (Objects != Defs_NoObjects) {
    if (Objects->U_1.V_1.Kind != Defs_Union) {
      {
        register Defs_yElmt *W_4 = &Objects->U_1.V_46.Elmt;

        if (Defs_IsOpenArray(W_4->Object)) {
          if (Defs_IsVAR(W_4->Object)) {
            VAROpens = Defs_mCObjects(W_4->Object, VAROpens);
          } else {
            ValueOpens = Defs_mCObjects(W_4->Object, ValueOpens);
          }
        }
      }
      Objects = Objects->U_1.V_46.Elmt.Next;
    } else {
      OpenArrays2(Objects->U_1.V_47.Union.Objects1);
      Objects = Objects->U_1.V_47.Union.Objects2;
    }
  }
}

Defs_tSelectors Defs_GetSelectors
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  if (Object->U_1.V_1.Kind == Defs_Field1) {
    return Object->U_1.V_7.Field1.Selectors;
  } else {
    return Defs_NoSelectors;
  }
}

Defs_tObjects Defs_GetExport1
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  if (Object->U_1.V_1.Kind == Defs_Module1) {
    return Object->U_1.V_8.Module1.ExportList;
  } else {
    return Defs_NoObjects;
  }
}

Defs_tObjects Defs_GetExport2
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  if (Object->U_1.V_1.Kind == Defs_Module1) {
    return Object->U_1.V_8.Module1.Objects;
  } else {
    return Defs_NoObjects;
  }
}

Defs_tObjects Defs_GetObjects
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  if (Object->U_1.V_1.Kind == Defs_Var1 && Object->U_1.V_13.Var1.Type->U_1.V_1.Kind == Defs_Record1) {
    return Object->U_1.V_13.Var1.Type->U_1.V_42.Record1.Objects;
  } else if (Object->U_1.V_1.Kind == Defs_Field1 && Object->U_1.V_7.Field1.Type->U_1.V_1.Kind == Defs_Record1) {
    return Object->U_1.V_7.Field1.Type->U_1.V_42.Record1.Objects;
  } else if (Object->U_1.V_1.Kind == Defs_Module1) {
    return Object->U_1.V_8.Module1.Objects;
  } else {
    return Defs_NoObjects;
  }
}

Defs_tType Defs_GetType
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  switch (Object->U_1.V_1.Kind) {
  case Defs_Const1:;
    return (Defs_tDefs)Values_TypeOfValue(Object->U_1.V_5.Const1.Value);
    break;
  case Defs_EnumLiteral1:;
    return Object->U_1.V_6.EnumLiteral1.Type;
    break;
  case Defs_Field1:;
    return Object->U_1.V_7.Field1.Type;
    break;
  case Defs_Module1:;
    return Defs_TypeVOID;
    break;
  case Defs_Proc1:;
    return Object->U_1.V_9.Proc1.Type;
    break;
  case Defs_ProcHead1:;
    return Object->U_1.V_10.ProcHead1.Type;
    break;
  case Defs_TypeDecl1:;
    return Object->U_1.V_11.TypeDecl1.Type;
    break;
  case Defs_Opaque1:;
    return Object->U_1.V_12.Opaque1.Type;
    break;
  case Defs_Var1:;
    return Object->U_1.V_13.Var1.Type;
    break;
  case Defs_StdProc1:;
    return Object->U_1.V_14.StdProc1.Type;
    break;
  default :
    return Defs_NoType;
    break;
  }
}

Defs_tType Defs_GetIndexType
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_Array1) {
    return Type->U_1.V_38.Array1.IndexType;
  } else {
    return Defs_NoType;
  }
}

Defs_tType Defs_GetElemType
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_Array1) {
    return Type->U_1.V_38.Array1.ElemType;
  } else {
    return Defs_NoType;
  }
}

Defs_tType Defs_GetTargetType
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_Pointer1) {
    return Type->U_1.V_40.Pointer1.Type;
  } else if (Type->U_1.V_1.Kind == Defs_OpaqueType1) {
    return Defs_GetTargetType(Type->U_1.V_37.OpaqueType1.Type);
  } else {
    return Defs_NoType;
  }
}

Defs_tTypes Defs_GetFormals
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_ProcType1) {
    return Type->U_1.V_41.ProcType1.Types;
  } else if (Type->U_1.V_1.Kind == Defs_StdProcType1) {
    return Defs_NoTypes;
  } else {
    return Defs_NoTypes;
  }
}

Defs_tType Defs_GetResultType
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_ProcType1) {
    return Type->U_1.V_41.ProcType1.Type;
  } else {
    return Defs_NoType;
  }
}

Defs_tObjects Defs_GetFields
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  if (Type->U_1.V_1.Kind == Defs_Record1) {
    return Type->U_1.V_42.Record1.Objects;
  } else {
    return Defs_NoObjects;
  }
}

void Defs_Head
# ifdef __STDC__
(Defs_tTypes Types, BOOLEAN *IsVAR, Defs_tType *Type)
# else
(Types, IsVAR, Type)
Defs_tTypes Types;
BOOLEAN *IsVAR;
Defs_tType *Type;
# endif
{
  if (Types != Defs_NoTypes) {
    *IsVAR = Types->U_1.V_49.Types.IsVAR;
    *Type = Types->U_1.V_49.Types.Type;
  } else {
    *IsVAR = FALSE;
    *Type = Defs_NoType;
  }
}

Defs_tTypes Defs_Tail
# ifdef __STDC__
(Defs_tTypes Types)
# else
(Types)
Defs_tTypes Types;
# endif
{
  if (Types != Defs_NoTypes) {
    return Types->U_1.V_49.Types.Next;
  } else {
    return Defs_NoTypes;
  }
}

Defs_tObject Defs_GetLiteral
# ifdef __STDC__
(Defs_tObjects Objects, SHORTCARD Index)
# else
(Objects, Index)
Defs_tObjects Objects;
SHORTCARD Index;
# endif
{
  Defs_tObject Object;

  while (Objects != Defs_NoObjects) {
    Object = Objects->U_1.V_46.Elmt.Object;
    if (Object->U_1.V_6.EnumLiteral1.Index == Index) {
      return Object;
    }
    Objects = Objects->U_1.V_46.Elmt.Next;
  }
  return Defs_NoObject;
}

BOOLEAN Defs_IsExported
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  return Object->U_1.V_1.Kind == Defs_Proc1 && Object->U_1.V_9.Proc1.IsExported;
}

BOOLEAN Defs_IsOpenArray
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  return Object->U_1.V_1.Kind == Defs_Var1 && Defs_IsOpen(Object->U_1.V_13.Var1.Type);
}

BOOLEAN Defs_IsVAR
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  return Object->U_1.V_1.Kind == Defs_Var1 && Object->U_1.V_13.Var1.IsVAR;
}

BOOLEAN Defs_IsProcedure
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  return Object->U_1.V_1.Kind == Defs_Proc1 || Object->U_1.V_1.Kind == Defs_ProcHead1;
}

BOOLEAN Defs_IsOfType
# ifdef __STDC__
(Defs_tObject Object)
# else
(Object)
Defs_tObject Object;
# endif
{
  return Object->U_1.V_1.Kind == Defs_TypeDecl1 || Object->U_1.V_1.Kind == Defs_Opaque1;
}

BOOLEAN Defs_IsForward
# ifdef __STDC__
(SHORTCARD PosPointerTo, Defs_tObject TypeObj)
# else
(PosPointerTo, TypeObj)
SHORTCARD PosPointerTo;
Defs_tObject TypeObj;
# endif
{
  if (TypeObj->U_1.V_1.Kind == Defs_TypeDecl1) {
    return TypeObj->U_1.V_11.TypeDecl1.TypePos > PosPointerTo;
  } else if (TypeObj->U_1.V_1.Kind == Defs_Opaque1) {
    return TypeObj->U_1.V_12.Opaque1.TypePos > PosPointerTo;
  } else {
    return FALSE;
  }
}

BOOLEAN Defs_IsIntType
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  switch (Type->U_1.V_1.Kind) {
  case Defs_Subrange1:;
    return Defs_IsIntType(Type->U_1.V_44.Subrange1.Type);
    break;
  case Defs_ShortCard:;
  case Defs_LongCard:;
  case Defs_ShortInt:;
  case Defs_LongInt:;
    return TRUE;
    break;
  default :
    return FALSE;
    break;
  }
}

BOOLEAN Defs_IsOpen
# ifdef __STDC__
(Defs_tType Type)
# else
(Type)
Defs_tType Type;
# endif
{
  return Type->U_1.V_1.Kind == Defs_Array1 && Type->U_1.V_38.Array1.IsOpen;
}

void Defs_WriteEnv
# ifdef __STDC__
(IO_tFile f, Defs_tEnv Env)
# else
(f, Env)
IO_tFile f;
Defs_tEnv Env;
# endif
{
  while (Env != Defs_NoEnv) {
    Defs_WriteObjects(f, Env->U_1.V_50.Env.Objects);
    IO_WriteS(f, (STRING)"/", 1L);
    Env = Env->U_1.V_50.Env.HiddenEnv;
  }
  IO_WriteNl(f);
}

void Defs_WriteObjects
# ifdef __STDC__
(IO_tFile f, Defs_tObjects Objects)
# else
(f, Objects)
IO_tFile f;
Defs_tObjects Objects;
# endif
{
  while (Objects != Defs_NoObjects) {
    if (Objects->U_1.V_1.Kind != Defs_Union) {
      IO_WriteS(f, (STRING)" ", 1L);
      Idents_WriteIdent(f, Objects->U_1.V_46.Elmt.Ident);
      Objects = Objects->U_1.V_46.Elmt.Next;
    } else {
      Defs_WriteObjects(f, Objects->U_1.V_47.Union.Objects1);
      Objects = Objects->U_1.V_47.Union.Objects2;
    }
  }
}

static void BasicType
# ifdef __STDC__
(CHAR Name[], LONGCARD O_1, Defs_tType Type)
# else
(Name, O_1, Type)
CHAR Name[];
LONGCARD O_1;
Defs_tType Type;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Name, O_1, CHAR)
  Strings_ArrayToString(Name, O_1, G_2_String);
  *G_3_Ident = Idents_MakeIdent(G_2_String);
  *G_4_Object = Defs_mTypeDecl2(Defs_mTypeDecl1(*G_3_Ident, 0), Type);
  (*G_4_Object)->U_1.V_11.TypeDecl1.CIdent = *G_3_Ident;
  *G_1_Objects = Defs_mElmt(*G_3_Ident, FALSE, *G_4_Object, *G_1_Objects);
  FREE_OPEN_ARRAYS
}

static void StdConst
# ifdef __STDC__
(CHAR Name[], LONGCARD O_2, Values_tValue Value)
# else
(Name, O_2, Value)
CHAR Name[];
LONGCARD O_2;
Values_tValue Value;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_2 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Name, O_2, CHAR)
  Strings_ArrayToString(Name, O_2, G_2_String);
  *G_3_Ident = Idents_MakeIdent(G_2_String);
  *G_4_Object = Defs_mConst2(Defs_mConst1(*G_3_Ident), Value);
  (*G_4_Object)->U_1.V_5.Const1.CIdent = *G_3_Ident;
  *G_1_Objects = Defs_mElmt(*G_3_Ident, FALSE, *G_4_Object, *G_1_Objects);
  FREE_OPEN_ARRAYS
}

static void StdProcedure
# ifdef __STDC__
(CHAR Name[], LONGCARD O_3, SHORTCARD StdProc)
# else
(Name, O_3, StdProc)
CHAR Name[];
LONGCARD O_3;
SHORTCARD StdProc;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_3 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Name, O_3, CHAR)
  Strings_ArrayToString(Name, O_3, G_2_String);
  *G_3_Ident = Idents_MakeIdent(G_2_String);
  *G_4_Object = Defs_mStdProc1(*G_3_Ident, StdProc, Defs_mStdProcType1(StdProc));
  (*G_4_Object)->U_1.V_9.Proc1.CIdent = *G_3_Ident;
  *G_1_Objects = Defs_mElmt(*G_3_Ident, FALSE, *G_4_Object, *G_1_Objects);
  FREE_OPEN_ARRAYS
}

static Defs_tObjects Predef
# ifdef __STDC__
()
# else
()
# endif
{
  Defs_tObjects Objects;
  Strings_tString String;
  Idents_tIdent Ident;
  Defs_tObject Object;
  Defs_tObjects *L_1;
  Strings_tString *L_2;
  Idents_tIdent *L_3;
  Defs_tObject *L_4;

  L_1 = G_1_Objects;
  G_1_Objects = &Objects;
  L_2 = G_2_String;
  G_2_String = &String;
  L_3 = G_3_Ident;
  G_3_Ident = &Ident;
  L_4 = G_4_Object;
  G_4_Object = &Object;
  Objects = Defs_NoObjects;
  BasicType((STRING)"ADDRESS", 7L, Defs_TypeADDRESS);
  BasicType((STRING)"WORD", 4L, Defs_TypeWORD);
  BasicType((STRING)"BYTE", 4L, Defs_TypeWORD);
  StdProcedure((STRING)"ADR", 3L, Defs_ProcADR);
  StdProcedure((STRING)"TSIZE", 5L, Defs_ProcTSIZE);
  StdProcedure((STRING)"NEWPROCESS", 10L, Defs_ProcNEWPROCESS);
  StdProcedure((STRING)"TRANSFER", 8L, Defs_ProcTRANSFER);
  StdProcedure((STRING)"IOTRANSFER", 10L, Defs_ProcIOTRANSFER);
  Defs_ModuleSYSTEM = Defs_mModule2(Defs_mModule1(Defs_IdentSYSTEM, Objects), Objects);
  Defs_ModuleSYSTEM->U_1.V_8.Module1.CIdent = Defs_IdentSYSTEM;
  Objects = Defs_NoObjects;
  BasicType((STRING)"INTEGER", 7L, Defs_TypeLONGINT);
  BasicType((STRING)"SHORTINT", 8L, Defs_TypeSHORTINT);
  BasicType((STRING)"LONGINT", 7L, Defs_TypeLONGINT);
  BasicType((STRING)"CARDINAL", 8L, Defs_TypeLONGCARD);
  BasicType((STRING)"SHORTCARD", 9L, Defs_TypeSHORTCARD);
  BasicType((STRING)"LONGCARD", 8L, Defs_TypeLONGCARD);
  BasicType((STRING)"REAL", 4L, Defs_TypeREAL);
  BasicType((STRING)"LONGREAL", 8L, Defs_TypeLONGREAL);
  BasicType((STRING)"BOOLEAN", 7L, Defs_TypeBOOLEAN);
  BasicType((STRING)"CHAR", 4L, Defs_TypeCHAR);
  BasicType((STRING)"BITSET", 6L, Defs_TypeBITSET);
  BasicType((STRING)"PROC", 4L, Defs_TypePROC);
  Values_NilValue.Kind = Values_NilType;
  Values_TrueValue.Kind = Values_Boolean;
  Values_TrueValue.U_1.V_3.BoolValue = TRUE;
  Values_FalseValue.Kind = Values_Boolean;
  Values_FalseValue.U_1.V_3.BoolValue = FALSE;
  StdConst((STRING)"NIL", 3L, Values_NilValue);
  StdConst((STRING)"TRUE", 4L, Values_TrueValue);
  StdConst((STRING)"FALSE", 5L, Values_FalseValue);
  StdProcedure((STRING)"ABS", 3L, Defs_ProcABS);
  StdProcedure((STRING)"CAP", 3L, Defs_ProcCAP);
  StdProcedure((STRING)"CHR", 3L, Defs_ProcCHR);
  StdProcedure((STRING)"DEC", 3L, Defs_ProcDEC);
  StdProcedure((STRING)"EXCL", 4L, Defs_ProcEXCL);
  StdProcedure((STRING)"FLOAT", 5L, Defs_ProcFLOAT);
  StdProcedure((STRING)"HALT", 4L, Defs_ProcHALT);
  StdProcedure((STRING)"HIGH", 4L, Defs_ProcHIGH);
  StdProcedure((STRING)"INC", 3L, Defs_ProcINC);
  StdProcedure((STRING)"INCL", 4L, Defs_ProcINCL);
  StdProcedure((STRING)"MAX", 3L, Defs_ProcMAX);
  StdProcedure((STRING)"MIN", 3L, Defs_ProcMIN);
  StdProcedure((STRING)"ODD", 3L, Defs_ProcODD);
  StdProcedure((STRING)"ORD", 3L, Defs_ProcORD);
  StdProcedure((STRING)"SIZE", 4L, Defs_ProcSIZE);
  StdProcedure((STRING)"TRUNC", 5L, Defs_ProcTRUNC);
  StdProcedure((STRING)"VAL", 3L, Defs_ProcVAL);
  StdProcedure((STRING)"NEW", 3L, Defs_ProcNEW);
  StdProcedure((STRING)"DISPOSE", 7L, Defs_ProcDISPOSE);
  G_1_Objects = L_1;
  G_2_String = L_2;
  G_3_Ident = L_3;
  G_4_Object = L_4;
  return Objects;
}

Defs_tDefs Defs_yyAlloc
# ifdef __STDC__
()
# else
()
# endif
{
  yytBlockPtr yyBlockPtr;

  yyBlockPtr = yyBlockList;
  yyBlockList = (yytBlockPtr)Memory_Alloc((LONGINT)sizeof(yytBlock));
  yyBlockList->yySuccessor = yyBlockPtr;
  Defs_yyPoolFreePtr = ADR(yyBlockList->yyBlock);
  Defs_yyPoolMaxPtr = (ADDRESS)(Defs_yyPoolFreePtr + yyBlockSize - yyMaxSize + 1);
  INC1(Defs_HeapUsed, yyBlockSize);
  return (Defs_tDefs)Defs_yyPoolFreePtr;
}

Defs_tDefs Defs_MakeDefs
# ifdef __STDC__
(SHORTCARD yyKind)
# else
(yyKind)
SHORTCARD yyKind;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[yyKind]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = yyKind;
  return yyt;
}

BOOLEAN Defs_IsType
# ifdef __STDC__
(Defs_tDefs yyTree, SHORTCARD yyKind)
# else
(yyTree, yyKind)
Defs_tDefs yyTree;
SHORTCARD yyKind;
# endif
{
  return yyTree != Defs_NoDefs && yyKind <= yyTree->U_1.V_1.Kind && yyTree->U_1.V_1.Kind <= yyTypeRange.A[yyKind];
}

Defs_tDefs Defs_mObject
# ifdef __STDC__
(Idents_tIdent pIdent)
# else
(pIdent)
Idents_tIdent pIdent;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Object]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Object;
  {
    register Defs_yObject *W_5 = &yyt->U_1.V_3.Object;

    W_5->Ident = pIdent;
    W_5->CIdent = Idents_NoIdent;
  }
  return yyt;
}

Defs_tDefs Defs_mObject0
# ifdef __STDC__
(Idents_tIdent pIdent)
# else
(pIdent)
Idents_tIdent pIdent;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Object0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Object0;
  {
    register Defs_yObject0 *W_6 = &yyt->U_1.V_4.Object0;

    W_6->Ident = pIdent;
    W_6->CIdent = Idents_NoIdent;
  }
  return yyt;
}

Defs_tDefs Defs_mConst1
# ifdef __STDC__
(Idents_tIdent pIdent)
# else
(pIdent)
Idents_tIdent pIdent;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Const1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Const1;
  {
    register Defs_yConst1 *W_7 = &yyt->U_1.V_5.Const1;

    W_7->Ident = pIdent;
    W_7->CIdent = Idents_NoIdent;
    W_7->Value = Values_ErrorValue;
  }
  return yyt;
}

Defs_tDefs Defs_mEnumLiteral1
# ifdef __STDC__
(Idents_tIdent pIdent, Defs_tDefs pType, SHORTCARD pIndex)
# else
(pIdent, pType, pIndex)
Idents_tIdent pIdent;
Defs_tDefs pType;
SHORTCARD pIndex;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_EnumLiteral1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_EnumLiteral1;
  {
    register Defs_yEnumLiteral1 *W_8 = &yyt->U_1.V_6.EnumLiteral1;

    W_8->Ident = pIdent;
    W_8->CIdent = Idents_NoIdent;
    W_8->Type = pType;
    W_8->Index = pIndex;
  }
  return yyt;
}

Defs_tDefs Defs_mField1
# ifdef __STDC__
(Idents_tIdent pIdent, Defs_tDefs pType, Defs_tDefs pSelectors)
# else
(pIdent, pType, pSelectors)
Idents_tIdent pIdent;
Defs_tDefs pType;
Defs_tDefs pSelectors;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Field1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Field1;
  {
    register Defs_yField1 *W_9 = &yyt->U_1.V_7.Field1;

    W_9->Ident = pIdent;
    W_9->CIdent = Idents_NoIdent;
    W_9->Type = pType;
    W_9->Selectors = pSelectors;
  }
  return yyt;
}

Defs_tDefs Defs_mModule1
# ifdef __STDC__
(Idents_tIdent pIdent, Defs_tDefs pExportList)
# else
(pIdent, pExportList)
Idents_tIdent pIdent;
Defs_tDefs pExportList;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Module1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Module1;
  {
    register Defs_yModule1 *W_10 = &yyt->U_1.V_8.Module1;

    W_10->Ident = pIdent;
    W_10->CIdent = Idents_NoIdent;
    W_10->ExportList = pExportList;
    W_10->Objects = Defs_NoDefs;
    W_10->Locals = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mProc1
# ifdef __STDC__
(Idents_tIdent pIdent, Defs_tDefs pType)
# else
(pIdent, pType)
Idents_tIdent pIdent;
Defs_tDefs pType;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Proc1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Proc1;
  {
    register Defs_yProc1 *W_11 = &yyt->U_1.V_9.Proc1;

    W_11->Ident = pIdent;
    W_11->CIdent = Idents_NoIdent;
    W_11->Type = pType;
    W_11->IsExported = FALSE;
    W_11->Locals = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mProcHead1
# ifdef __STDC__
(Idents_tIdent pIdent, Defs_tDefs pType)
# else
(pIdent, pType)
Idents_tIdent pIdent;
Defs_tDefs pType;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_ProcHead1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_ProcHead1;
  {
    register Defs_yProcHead1 *W_12 = &yyt->U_1.V_10.ProcHead1;

    W_12->Ident = pIdent;
    W_12->CIdent = Idents_NoIdent;
    W_12->Type = pType;
  }
  return yyt;
}

Defs_tDefs Defs_mTypeDecl1
# ifdef __STDC__
(Idents_tIdent pIdent, SHORTCARD pTypePos)
# else
(pIdent, pTypePos)
Idents_tIdent pIdent;
SHORTCARD pTypePos;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_TypeDecl1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_TypeDecl1;
  {
    register Defs_yTypeDecl1 *W_13 = &yyt->U_1.V_11.TypeDecl1;

    W_13->Ident = pIdent;
    W_13->CIdent = Idents_NoIdent;
    W_13->TypePos = pTypePos;
    W_13->Type = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mOpaque1
# ifdef __STDC__
(Idents_tIdent pIdent, SHORTCARD pTypePos)
# else
(pIdent, pTypePos)
Idents_tIdent pIdent;
SHORTCARD pTypePos;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Opaque1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Opaque1;
  {
    register Defs_yOpaque1 *W_14 = &yyt->U_1.V_12.Opaque1;

    W_14->Ident = pIdent;
    W_14->CIdent = Idents_NoIdent;
    W_14->TypePos = pTypePos;
    W_14->Type = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mVar1
# ifdef __STDC__
(Idents_tIdent pIdent, Defs_tDefs pType, BOOLEAN pIsVAR, SHORTCARD pLevel, BOOLEAN pNestedUse, ADDRESS pTypeTree)
# else
(pIdent, pType, pIsVAR, pLevel, pNestedUse, pTypeTree)
Idents_tIdent pIdent;
Defs_tDefs pType;
BOOLEAN pIsVAR;
SHORTCARD pLevel;
BOOLEAN pNestedUse;
ADDRESS pTypeTree;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Var1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Var1;
  {
    register Defs_yVar1 *W_15 = &yyt->U_1.V_13.Var1;

    W_15->Ident = pIdent;
    W_15->CIdent = Idents_NoIdent;
    W_15->Type = pType;
    W_15->IsVAR = pIsVAR;
    W_15->Level = pLevel;
    W_15->NestedUse = pNestedUse;
    W_15->TypeTree = pTypeTree;
  }
  return yyt;
}

Defs_tDefs Defs_mStdProc1
# ifdef __STDC__
(Idents_tIdent pIdent, SHORTCARD pStdProc, Defs_tDefs pType)
# else
(pIdent, pStdProc, pType)
Idents_tIdent pIdent;
SHORTCARD pStdProc;
Defs_tDefs pType;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_StdProc1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_StdProc1;
  {
    register Defs_yStdProc1 *W_16 = &yyt->U_1.V_14.StdProc1;

    W_16->Ident = pIdent;
    W_16->CIdent = Idents_NoIdent;
    W_16->StdProc = pStdProc;
    W_16->Type = pType;
  }
  return yyt;
}

Defs_tDefs Defs_mType
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Type]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Type;
  return yyt;
}

Defs_tDefs Defs_mType0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Type0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Type0;
  return yyt;
}

Defs_tDefs Defs_mShortInt
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_ShortInt]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_ShortInt;
  return yyt;
}

Defs_tDefs Defs_mLongInt
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_LongInt]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_LongInt;
  return yyt;
}

Defs_tDefs Defs_mShortCard
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_ShortCard]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_ShortCard;
  return yyt;
}

Defs_tDefs Defs_mLongCard
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_LongCard]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_LongCard;
  return yyt;
}

Defs_tDefs Defs_mReal
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Real]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Real;
  return yyt;
}

Defs_tDefs Defs_mLongReal
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_LongReal]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_LongReal;
  return yyt;
}

Defs_tDefs Defs_mBool
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Bool]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Bool;
  return yyt;
}

Defs_tDefs Defs_mChar
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Char]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Char;
  return yyt;
}

Defs_tDefs Defs_mBitset
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Bitset]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Bitset;
  return yyt;
}

Defs_tDefs Defs_mProc
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Proc]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Proc;
  return yyt;
}

Defs_tDefs Defs_mWord
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Word]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Word;
  return yyt;
}

Defs_tDefs Defs_mAddress
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Address]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Address;
  return yyt;
}

Defs_tDefs Defs_mIntCard
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_IntCard]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_IntCard;
  return yyt;
}

Defs_tDefs Defs_mNil
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Nil]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Nil;
  return yyt;
}

Defs_tDefs Defs_mString
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_String]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_String;
  return yyt;
}

Defs_tDefs Defs_mStringChar
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_StringChar]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_StringChar;
  return yyt;
}

Defs_tDefs Defs_mVoid
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Void]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Void;
  return yyt;
}

Defs_tDefs Defs_mStdProcType1
# ifdef __STDC__
(SHORTCARD pStdProc)
# else
(pStdProc)
SHORTCARD pStdProc;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_StdProcType1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_StdProcType1;
  {
    register Defs_yStdProcType1 *W_17 = &yyt->U_1.V_34.StdProcType1;

    W_17->StdProc = pStdProc;
  }
  return yyt;
}

Defs_tDefs Defs_mQualident1
# ifdef __STDC__
(Defs_tDefs pObject)
# else
(pObject)
Defs_tDefs pObject;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Qualident1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Qualident1;
  {
    register Defs_yQualident1 *W_18 = &yyt->U_1.V_35.Qualident1;

    W_18->Object = pObject;
  }
  return yyt;
}

Defs_tDefs Defs_mConstructor
# ifdef __STDC__
(Defs_tDefs pTypeObj)
# else
(pTypeObj)
Defs_tDefs pTypeObj;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Constructor]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Constructor;
  {
    register Defs_yConstructor *W_19 = &yyt->U_1.V_36.Constructor;

    W_19->TypeObj = pTypeObj;
  }
  return yyt;
}

Defs_tDefs Defs_mOpaqueType1
# ifdef __STDC__
(Defs_tDefs pTypeObj)
# else
(pTypeObj)
Defs_tDefs pTypeObj;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_OpaqueType1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_OpaqueType1;
  {
    register Defs_yOpaqueType1 *W_20 = &yyt->U_1.V_37.OpaqueType1;

    W_20->TypeObj = pTypeObj;
    W_20->Type = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mArray1
# ifdef __STDC__
(Defs_tDefs pTypeObj, Idents_tIdent pStructId)
# else
(pTypeObj, pStructId)
Defs_tDefs pTypeObj;
Idents_tIdent pStructId;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Array1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Array1;
  {
    register Defs_yArray1 *W_21 = &yyt->U_1.V_38.Array1;

    W_21->TypeObj = pTypeObj;
    W_21->StructId = pStructId;
    W_21->IndexType = Defs_NoDefs;
    W_21->ElemType = Defs_NoDefs;
    W_21->IsOpen = FALSE;
  }
  return yyt;
}

Defs_tDefs Defs_mEnumeration1
# ifdef __STDC__
(Defs_tDefs pTypeObj)
# else
(pTypeObj)
Defs_tDefs pTypeObj;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Enumeration1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Enumeration1;
  {
    register Defs_yEnumeration1 *W_22 = &yyt->U_1.V_39.Enumeration1;

    W_22->TypeObj = pTypeObj;
    W_22->MaxValue = 0;
    W_22->Objects = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mPointer1
# ifdef __STDC__
(Defs_tDefs pTypeObj)
# else
(pTypeObj)
Defs_tDefs pTypeObj;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Pointer1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Pointer1;
  {
    register Defs_yPointer1 *W_23 = &yyt->U_1.V_40.Pointer1;

    W_23->TypeObj = pTypeObj;
    W_23->Type = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mProcType1
# ifdef __STDC__
(Defs_tDefs pTypeObj)
# else
(pTypeObj)
Defs_tDefs pTypeObj;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_ProcType1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_ProcType1;
  {
    register Defs_yProcType1 *W_24 = &yyt->U_1.V_41.ProcType1;

    W_24->TypeObj = pTypeObj;
    W_24->Types = Defs_NoDefs;
    W_24->Type = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mRecord1
# ifdef __STDC__
(Defs_tDefs pTypeObj, Idents_tIdent pStructId)
# else
(pTypeObj, pStructId)
Defs_tDefs pTypeObj;
Idents_tIdent pStructId;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Record1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Record1;
  {
    register Defs_yRecord1 *W_25 = &yyt->U_1.V_42.Record1;

    W_25->TypeObj = pTypeObj;
    W_25->StructId = pStructId;
    W_25->Objects = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mSet1
# ifdef __STDC__
(Defs_tDefs pTypeObj)
# else
(pTypeObj)
Defs_tDefs pTypeObj;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Set1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Set1;
  {
    register Defs_ySet1 *W_26 = &yyt->U_1.V_43.Set1;

    W_26->TypeObj = pTypeObj;
    W_26->Type = Defs_NoDefs;
  }
  return yyt;
}

Defs_tDefs Defs_mSubrange1
# ifdef __STDC__
(Defs_tDefs pTypeObj, ADDRESS pLwbExpr, ADDRESS pUpbExpr)
# else
(pTypeObj, pLwbExpr, pUpbExpr)
Defs_tDefs pTypeObj;
ADDRESS pLwbExpr;
ADDRESS pUpbExpr;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Subrange1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Subrange1;
  {
    register Defs_ySubrange1 *W_27 = &yyt->U_1.V_44.Subrange1;

    W_27->TypeObj = pTypeObj;
    W_27->LwbExpr = pLwbExpr;
    W_27->UpbExpr = pUpbExpr;
    W_27->Type = Defs_NoDefs;
    W_27->Lwb = Values_ErrorValue;
    W_27->Upb = Values_ErrorValue;
  }
  return yyt;
}

Defs_tDefs Defs_mObjects
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Objects]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Objects;
  return yyt;
}

Defs_tDefs Defs_mElmt
# ifdef __STDC__
(Idents_tIdent pIdent, BOOLEAN pIsPseudoObj, Defs_tDefs pObject, Defs_tDefs pNext)
# else
(pIdent, pIsPseudoObj, pObject, pNext)
Idents_tIdent pIdent;
BOOLEAN pIsPseudoObj;
Defs_tDefs pObject;
Defs_tDefs pNext;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Elmt]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Elmt;
  {
    register Defs_yElmt *W_28 = &yyt->U_1.V_46.Elmt;

    W_28->Ident = pIdent;
    W_28->IsPseudoObj = pIsPseudoObj;
    W_28->Object = pObject;
    W_28->Next = pNext;
  }
  return yyt;
}

Defs_tDefs Defs_mUnion
# ifdef __STDC__
(Defs_tDefs pObjects1, Defs_tDefs pObjects2)
# else
(pObjects1, pObjects2)
Defs_tDefs pObjects1;
Defs_tDefs pObjects2;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Union]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Union;
  {
    register Defs_yUnion *W_29 = &yyt->U_1.V_47.Union;

    W_29->Objects1 = pObjects1;
    W_29->Objects2 = pObjects2;
  }
  return yyt;
}

Defs_tDefs Defs_mCObjects
# ifdef __STDC__
(Defs_tObject pM2Object, Defs_tDefs pNext)
# else
(pM2Object, pNext)
Defs_tObject pM2Object;
Defs_tDefs pNext;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_CObjects]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_CObjects;
  {
    register Defs_yCObjects *W_30 = &yyt->U_1.V_48.CObjects;

    W_30->M2Object = pM2Object;
    W_30->Next = pNext;
    W_30->CObject = Idents_NoIdent;
  }
  return yyt;
}

Defs_tDefs Defs_mTypes
# ifdef __STDC__
(BOOLEAN pIsVAR, Defs_tDefs pType, Defs_tDefs pNext)
# else
(pIsVAR, pType, pNext)
BOOLEAN pIsVAR;
Defs_tDefs pType;
Defs_tDefs pNext;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Types]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Types;
  {
    register Defs_yTypes *W_31 = &yyt->U_1.V_49.Types;

    W_31->IsVAR = pIsVAR;
    W_31->Type = pType;
    W_31->Next = pNext;
  }
  return yyt;
}

Defs_tDefs Defs_mEnv
# ifdef __STDC__
(Defs_tDefs pObjects, Defs_tDefs pHiddenEnv)
# else
(pObjects, pHiddenEnv)
Defs_tDefs pObjects;
Defs_tDefs pHiddenEnv;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Env]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Env;
  {
    register Defs_yEnv *W_32 = &yyt->U_1.V_50.Env;

    W_32->Objects = pObjects;
    W_32->HiddenEnv = pHiddenEnv;
  }
  return yyt;
}

Defs_tDefs Defs_mSelectors
# ifdef __STDC__
(Idents_tIdent pSelector, Defs_tDefs pNext)
# else
(pSelector, pNext)
Idents_tIdent pSelector;
Defs_tDefs pNext;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_Selectors]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_Selectors;
  {
    register Defs_ySelectors *W_33 = &yyt->U_1.V_51.Selectors;

    W_33->Selector = pSelector;
    W_33->Next = pNext;
  }
  return yyt;
}

Defs_tDefs Defs_mStringPar
# ifdef __STDC__
(Idents_tIdent pCString, Defs_tDefs pFormalType, ADDRESS pM2String, Defs_tDefs pNext)
# else
(pCString, pFormalType, pM2String, pNext)
Idents_tIdent pCString;
Defs_tDefs pFormalType;
ADDRESS pM2String;
Defs_tDefs pNext;
# endif
{
  LONGINT yyByteCount;
  Defs_tDefs yyt;

  yyt = (Defs_tDefs)Defs_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Defs_yyPoolMaxPtr) {
    yyt = Defs_yyAlloc();
  }
  INC1(Defs_yyPoolFreePtr, Defs_yyNodeSize.A[Defs_StringPar]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Defs_StringPar;
  {
    register Defs_yStringPar *W_34 = &yyt->U_1.V_52.StringPar;

    W_34->CString = pCString;
    W_34->FormalType = pFormalType;
    W_34->M2String = pM2String;
    W_34->Next = pNext;
  }
  return yyt;
}

void Defs_BeginDefs
# ifdef __STDC__
()
# else
()
# endif
{
  Defs_NoObject = Defs_mObject0(Idents_NoIdent);
  Defs_NoObject->U_1.V_4.Object0.CIdent = Idents_NoIdent;
  Defs_NoType = Defs_mType0();
  Strings_ArrayToString((STRING)"SYSTEM", 6L, &Str);
  Defs_IdentSYSTEM = Idents_MakeIdent(&Str);
  Strings_ArrayToString((STRING)"LONGCARD", 8L, &Str);
  Defs_IdentLONGCARD = Idents_MakeIdent(&Str);
  Strings_ArrayToString((STRING)"ALLOCATE", 8L, &Str);
  Defs_IdentALLOC = Idents_MakeIdent(&Str);
  Strings_ArrayToString((STRING)"DEALLOCATE", 10L, &Str);
  Defs_IdentDEALLOC = Idents_MakeIdent(&Str);
  Defs_TypeSHORTINT = Defs_mShortInt();
  Defs_TypeLONGINT = Defs_mLongInt();
  Defs_TypeSHORTCARD = Defs_mShortCard();
  Defs_TypeLONGCARD = Defs_mLongCard();
  Defs_TypeREAL = Defs_mReal();
  Defs_TypeLONGREAL = Defs_mLongReal();
  Defs_TypeBOOLEAN = Defs_mBool();
  Defs_TypeCHAR = Defs_mChar();
  Defs_TypeBITSET = Defs_mBitset();
  Defs_TypePROC = Defs_mProc();
  Defs_TypeWORD = Defs_mWord();
  Defs_TypeADDRESS = Defs_mAddress();
  Defs_TypeIntCard = Defs_mIntCard();
  Defs_TypeNIL = Defs_mNil();
  Defs_TypeSTRING = Defs_mString();
  Defs_TypeStringChar = Defs_mStringChar();
  Defs_TypeVOID = Defs_mVoid();
  Defs_Predefs = Predef();
}

void Defs_CloseDefs
# ifdef __STDC__
()
# else
()
# endif
{
}

static void xxExit
# ifdef __STDC__
()
# else
()
# endif
{
  IO_CloseIO();
  Exit(1L);
}

void BEGIN_Defs()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_IO();
    BEGIN_Idents();
    BEGIN_Values();
    BEGIN_System();
    BEGIN_General();
    BEGIN_Memory();
    BEGIN_DynArray();
    BEGIN_IO();
    BEGIN_Layout();
    BEGIN_StringMem();
    BEGIN_Strings();
    BEGIN_Idents();
    BEGIN_Texts();
    BEGIN_Sets();
    BEGIN_Positions();
    BEGIN_IO();
    BEGIN_Strings();
    BEGIN_Idents();
    BEGIN_Values();
    BEGIN_Values();

    yyBlockList = NIL;
    Defs_yyPoolFreePtr = (ADDRESS)NIL;
    Defs_yyPoolMaxPtr = (ADDRESS)NIL;
    Defs_HeapUsed = 0;
    Defs_yyExit = xxExit;
    Defs_yyNodeSize.A[Defs_Object] = sizeof(Defs_yObject);
    Defs_yyNodeSize.A[Defs_Object0] = sizeof(Defs_yObject0);
    Defs_yyNodeSize.A[Defs_Const1] = sizeof(Defs_yConst1);
    Defs_yyNodeSize.A[Defs_EnumLiteral1] = sizeof(Defs_yEnumLiteral1);
    Defs_yyNodeSize.A[Defs_Field1] = sizeof(Defs_yField1);
    Defs_yyNodeSize.A[Defs_Module1] = sizeof(Defs_yModule1);
    Defs_yyNodeSize.A[Defs_Proc1] = sizeof(Defs_yProc1);
    Defs_yyNodeSize.A[Defs_ProcHead1] = sizeof(Defs_yProcHead1);
    Defs_yyNodeSize.A[Defs_TypeDecl1] = sizeof(Defs_yTypeDecl1);
    Defs_yyNodeSize.A[Defs_Opaque1] = sizeof(Defs_yOpaque1);
    Defs_yyNodeSize.A[Defs_Var1] = sizeof(Defs_yVar1);
    Defs_yyNodeSize.A[Defs_StdProc1] = sizeof(Defs_yStdProc1);
    Defs_yyNodeSize.A[Defs_Type] = sizeof(Defs_yType);
    Defs_yyNodeSize.A[Defs_Type0] = sizeof(Defs_yType0);
    Defs_yyNodeSize.A[Defs_ShortInt] = sizeof(Defs_yShortInt);
    Defs_yyNodeSize.A[Defs_LongInt] = sizeof(Defs_yLongInt);
    Defs_yyNodeSize.A[Defs_ShortCard] = sizeof(Defs_yShortCard);
    Defs_yyNodeSize.A[Defs_LongCard] = sizeof(Defs_yLongCard);
    Defs_yyNodeSize.A[Defs_Real] = sizeof(Defs_yReal);
    Defs_yyNodeSize.A[Defs_LongReal] = sizeof(Defs_yLongReal);
    Defs_yyNodeSize.A[Defs_Bool] = sizeof(Defs_yBool);
    Defs_yyNodeSize.A[Defs_Char] = sizeof(Defs_yChar);
    Defs_yyNodeSize.A[Defs_Bitset] = sizeof(Defs_yBitset);
    Defs_yyNodeSize.A[Defs_Proc] = sizeof(Defs_yProc);
    Defs_yyNodeSize.A[Defs_Word] = sizeof(Defs_yWord);
    Defs_yyNodeSize.A[Defs_Address] = sizeof(Defs_yAddress);
    Defs_yyNodeSize.A[Defs_IntCard] = sizeof(Defs_yIntCard);
    Defs_yyNodeSize.A[Defs_Nil] = sizeof(Defs_yNil);
    Defs_yyNodeSize.A[Defs_String] = sizeof(Defs_yString);
    Defs_yyNodeSize.A[Defs_StringChar] = sizeof(Defs_yStringChar);
    Defs_yyNodeSize.A[Defs_Void] = sizeof(Defs_yVoid);
    Defs_yyNodeSize.A[Defs_StdProcType1] = sizeof(Defs_yStdProcType1);
    Defs_yyNodeSize.A[Defs_Qualident1] = sizeof(Defs_yQualident1);
    Defs_yyNodeSize.A[Defs_Constructor] = sizeof(Defs_yConstructor);
    Defs_yyNodeSize.A[Defs_OpaqueType1] = sizeof(Defs_yOpaqueType1);
    Defs_yyNodeSize.A[Defs_Array1] = sizeof(Defs_yArray1);
    Defs_yyNodeSize.A[Defs_Enumeration1] = sizeof(Defs_yEnumeration1);
    Defs_yyNodeSize.A[Defs_Pointer1] = sizeof(Defs_yPointer1);
    Defs_yyNodeSize.A[Defs_ProcType1] = sizeof(Defs_yProcType1);
    Defs_yyNodeSize.A[Defs_Record1] = sizeof(Defs_yRecord1);
    Defs_yyNodeSize.A[Defs_Set1] = sizeof(Defs_ySet1);
    Defs_yyNodeSize.A[Defs_Subrange1] = sizeof(Defs_ySubrange1);
    Defs_yyNodeSize.A[Defs_Objects] = sizeof(Defs_yObjects);
    Defs_yyNodeSize.A[Defs_Elmt] = sizeof(Defs_yElmt);
    Defs_yyNodeSize.A[Defs_Union] = sizeof(Defs_yUnion);
    Defs_yyNodeSize.A[Defs_CObjects] = sizeof(Defs_yCObjects);
    Defs_yyNodeSize.A[Defs_Types] = sizeof(Defs_yTypes);
    Defs_yyNodeSize.A[Defs_Env] = sizeof(Defs_yEnv);
    Defs_yyNodeSize.A[Defs_Selectors] = sizeof(Defs_ySelectors);
    Defs_yyNodeSize.A[Defs_StringPar] = sizeof(Defs_yStringPar);
    yyMaxSize = 0;
    for (yyi = 1; yyi <= 50; yyi += 1) {
      Defs_yyNodeSize.A[yyi] = (LONGINT)((BITSET)(Defs_yyNodeSize.A[yyi] + (CARDINAL)General_MaxAlign - 1) & General_AlignMasks.A[General_MaxAlign]);
      yyMaxSize = General_Max((LONGINT)Defs_yyNodeSize.A[yyi], (LONGINT)yyMaxSize);
    }
    yyTypeRange.A[Defs_Object] = Defs_StdProc1;
    yyTypeRange.A[Defs_Object0] = Defs_Object0;
    yyTypeRange.A[Defs_Const1] = Defs_Const1;
    yyTypeRange.A[Defs_EnumLiteral1] = Defs_EnumLiteral1;
    yyTypeRange.A[Defs_Field1] = Defs_Field1;
    yyTypeRange.A[Defs_Module1] = Defs_Module1;
    yyTypeRange.A[Defs_Proc1] = Defs_Proc1;
    yyTypeRange.A[Defs_ProcHead1] = Defs_ProcHead1;
    yyTypeRange.A[Defs_TypeDecl1] = Defs_TypeDecl1;
    yyTypeRange.A[Defs_Opaque1] = Defs_Opaque1;
    yyTypeRange.A[Defs_Var1] = Defs_Var1;
    yyTypeRange.A[Defs_StdProc1] = Defs_StdProc1;
    yyTypeRange.A[Defs_Type] = Defs_Subrange1;
    yyTypeRange.A[Defs_Type0] = Defs_Type0;
    yyTypeRange.A[Defs_ShortInt] = Defs_ShortInt;
    yyTypeRange.A[Defs_LongInt] = Defs_LongInt;
    yyTypeRange.A[Defs_ShortCard] = Defs_ShortCard;
    yyTypeRange.A[Defs_LongCard] = Defs_LongCard;
    yyTypeRange.A[Defs_Real] = Defs_Real;
    yyTypeRange.A[Defs_LongReal] = Defs_LongReal;
    yyTypeRange.A[Defs_Bool] = Defs_Bool;
    yyTypeRange.A[Defs_Char] = Defs_Char;
    yyTypeRange.A[Defs_Bitset] = Defs_Bitset;
    yyTypeRange.A[Defs_Proc] = Defs_Proc;
    yyTypeRange.A[Defs_Word] = Defs_Word;
    yyTypeRange.A[Defs_Address] = Defs_Address;
    yyTypeRange.A[Defs_IntCard] = Defs_IntCard;
    yyTypeRange.A[Defs_Nil] = Defs_Nil;
    yyTypeRange.A[Defs_String] = Defs_String;
    yyTypeRange.A[Defs_StringChar] = Defs_StringChar;
    yyTypeRange.A[Defs_Void] = Defs_Void;
    yyTypeRange.A[Defs_StdProcType1] = Defs_StdProcType1;
    yyTypeRange.A[Defs_Qualident1] = Defs_Qualident1;
    yyTypeRange.A[Defs_Constructor] = Defs_Subrange1;
    yyTypeRange.A[Defs_OpaqueType1] = Defs_OpaqueType1;
    yyTypeRange.A[Defs_Array1] = Defs_Array1;
    yyTypeRange.A[Defs_Enumeration1] = Defs_Enumeration1;
    yyTypeRange.A[Defs_Pointer1] = Defs_Pointer1;
    yyTypeRange.A[Defs_ProcType1] = Defs_ProcType1;
    yyTypeRange.A[Defs_Record1] = Defs_Record1;
    yyTypeRange.A[Defs_Set1] = Defs_Set1;
    yyTypeRange.A[Defs_Subrange1] = Defs_Subrange1;
    yyTypeRange.A[Defs_Objects] = Defs_Union;
    yyTypeRange.A[Defs_Elmt] = Defs_Elmt;
    yyTypeRange.A[Defs_Union] = Defs_Union;
    yyTypeRange.A[Defs_CObjects] = Defs_CObjects;
    yyTypeRange.A[Defs_Types] = Defs_Types;
    yyTypeRange.A[Defs_Env] = Defs_Env;
    yyTypeRange.A[Defs_Selectors] = Defs_Selectors;
    yyTypeRange.A[Defs_StringPar] = Defs_StringPar;
    Defs_BeginDefs();
  }
}
