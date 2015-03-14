#include "SYSTEM_.h"

#ifndef DEFINITION_Base
#include "Base.h"
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

#ifndef DEFINITION_Code
#include "Code.h"
#endif

#ifndef DEFINITION_Types
#include "Types.h"
#endif

REAL Types_MinReal, Types_MaxReal;
LONGREAL Types_MinLongReal, Types_MaxLongReal;

static BOOLEAN Coercible ARGS((Defs_tType t1, Defs_tType t2));


Defs_tType Types_ResultType
# ifdef __STDC__
(SHORTCARD Operator, Defs_tType t1, Defs_tType t2)
# else
(Operator, t1, t2)
SHORTCARD Operator;
Defs_tType t1, t2;
# endif
{
  if (t1->U_1.V_1.Kind == Defs_Subrange1) {
    t1 = t1->U_1.V_44.Subrange1.Type;
  }
  if (t2->U_1.V_1.Kind == Defs_Subrange1) {
    t2 = t2->U_1.V_44.Subrange1.Type;
  }
  switch (Operator) {
  case Tree_NotEqual:;
  case Tree_Equal:;
  case Tree_Less:;
  case Tree_LessEqual:;
  case Tree_Greater:;
  case Tree_GreaterEqual:;
  case Tree_And:;
  case Tree_Or:;
  case Tree_In:;
  case Tree_Not:;
    return Defs_TypeBOOLEAN;
    break;
  case Tree_Times:;
  case Tree_Plus:;
  case Tree_Minus:;
  case Tree_Divide:;
  case Tree_Div:;
  case Tree_Mod:;
    if (Coercible(t2, t1)) {
      return t1;
    } else if (Coercible(t1, t2)) {
      return t2;
    } else {
      return Defs_NoType;
    }
    break;
  }
}

static BOOLEAN Coercible
# ifdef __STDC__
(Defs_tType t1, Defs_tType t2)
# else
(t1, t2)
Defs_tType t1, t2;
# endif
{
  if (t1 == t2) {
    return TRUE;
  }
  switch (t1->U_1.V_1.Kind) {
  case Defs_IntCard:;
    return t2 == Defs_TypeSHORTCARD || t2 == Defs_TypeLONGCARD || t2 == Defs_TypeSHORTINT || t2 == Defs_TypeLONGINT;
    break;
  case Defs_ShortInt:;
    return t2 == Defs_TypeLONGINT;
    break;
  case Defs_ShortCard:;
    return t2 == Defs_TypeLONGCARD || t2 == Defs_TypeADDRESS;
    break;
  case Defs_LongCard:;
    return t2 == Defs_TypeADDRESS;
    break;
  case Defs_Real:;
    return t2 == Defs_TypeLONGREAL;
    break;
  case Defs_Nil:;
    return t2 == Defs_TypeADDRESS;
    break;
  case Defs_Bitset:;
    return t2->U_1.V_1.Kind == Defs_Set1;
    break;
  default :
    return FALSE;
    break;
  }
}

Defs_tType Types_StdResultType
# ifdef __STDC__
(Defs_tType t, Defs_tTypes a)
# else
(t, a)
Defs_tType t;
Defs_tTypes a;
# endif
{
  Defs_tType ArgType;
  BOOLEAN IsVAR;

  switch (t->U_1.V_34.StdProcType1.StdProc) {
  case Defs_ProcMAX:;
  case Defs_ProcMIN:;
  case Defs_ProcVAL:;
    Defs_Head(a, &IsVAR, &ArgType);
    return ArgType;
    break;
  case Defs_ProcABS:;
    Defs_Head(a, &IsVAR, &ArgType);
    if (ArgType->U_1.V_1.Kind == Defs_Subrange1) {
      ArgType = ArgType->U_1.V_44.Subrange1.Type;
    }
    return ArgType;
    break;
  case Defs_ProcCAP:;
  case Defs_ProcCHR:;
    return Defs_TypeCHAR;
    break;
  case Defs_ProcORD:;
  case Defs_ProcTRUNC:;
    return Defs_TypeLONGCARD;
    break;
  case Defs_ProcSIZE:;
  case Defs_ProcTSIZE:;
    return Defs_TypeIntCard;
    break;
  case Defs_ProcFLOAT:;
    return Defs_TypeREAL;
    break;
  case Defs_ProcHIGH:;
    Defs_Head(a, &IsVAR, &ArgType);
    return Defs_GetIndexType(ArgType);
    break;
  case Defs_ProcODD:;
    return Defs_TypeBOOLEAN;
    break;
  case Defs_ProcADR:;
    return Defs_TypeADDRESS;
    break;
  default :
    return Defs_TypeVOID;
    break;
  }
}

CARDINAL Types_TypeSize
# ifdef __STDC__
(Defs_tType t)
# else
(t)
Defs_tType t;
# endif
{
  if (t->U_1.V_1.Kind == Defs_Subrange1) {
    t = t->U_1.V_44.Subrange1.Type;
  }
  switch (t->U_1.V_1.Kind) {
  case Defs_ShortInt:;
    return Types_SizeShort;
    break;
  case Defs_LongInt:;
    return Types_SizeLong;
    break;
  case Defs_ShortCard:;
    return Types_SizeUnsignedShort;
    break;
  case Defs_LongCard:;
    return Types_SizeUnsignedLong;
    break;
  case Defs_Real:;
    return Types_SizeFloat;
    break;
  case Defs_LongReal:;
    return Types_SizeDouble;
    break;
  case Defs_Bool:;
    return Types_SizeUnsignedChar;
    break;
  case Defs_Char:;
    return Types_SizeUnsignedChar;
    break;
  case Defs_Bitset:;
    return Types_SizeUnsignedLong;
    break;
  case Defs_Proc:;
    return Types_PointerSize;
    break;
  case Defs_Word:;
    return Types_SizeUnsignedChar;
    break;
  case Defs_Address:;
    return Types_PointerSize;
    break;
  case Defs_OpaqueType1:;
    return Types_PointerSize;
    break;
  case Defs_Array1:;
    return Types_NoSize;
    break;
  case Defs_Enumeration1:;
    if (t->U_1.V_39.Enumeration1.MaxValue <= ORD(Types_MaxChar)) {
      return Types_SizeUnsignedChar;
    } else {
      return Types_SizeUnsignedShort;
    }
    break;
  case Defs_Pointer1:;
    return Types_PointerSize;
    break;
  case Defs_ProcType1:;
    return Types_PointerSize;
    break;
  case Defs_Record1:;
    return Types_NoSize;
    break;
  case Defs_Set1:;
    return Types_SizeUnsignedLong;
    break;
  default :
    return Types_NoSize;
    break;
  }
}

void Types_GetLwb
# ifdef __STDC__
(Defs_tType t, Values_tValue *Lwb)
# else
(t, Lwb)
Defs_tType t;
Values_tValue *Lwb;
# endif
{
  switch (t->U_1.V_1.Kind) {
  case Defs_ShortInt:;
    *Lwb = Values_MinShortIntVal;
    break;
  case Defs_LongInt:;
    *Lwb = Values_MinLongIntVal;
    break;
  case Defs_ShortCard:;
    *Lwb = Values_ZeroValue;
    break;
  case Defs_LongCard:;
    *Lwb = Values_ZeroValue;
    break;
  case Defs_Real:;
    *Lwb = Values_MinRealVal;
    break;
  case Defs_LongReal:;
    *Lwb = Values_MinLongRealVal;
    break;
  case Defs_Bool:;
    *Lwb = Values_FalseValue;
    break;
  case Defs_Char:;
    *Lwb = Values_MinCharVal;
    break;
  case Defs_Enumeration1:;
    Lwb->Kind = Values_Enumeration;
    Lwb->U_1.V_7.EnumValue = (ADDRESS)Defs_GetLiteral(t->U_1.V_39.Enumeration1.Objects, 0);
    break;
  case Defs_Subrange1:;
    *Lwb = t->U_1.V_44.Subrange1.Lwb;
    break;
  default :
    *Lwb = Values_ErrorValue;
    break;
  }
}

void Types_GetUpb
# ifdef __STDC__
(Defs_tType t, Values_tValue *Upb)
# else
(t, Upb)
Defs_tType t;
Values_tValue *Upb;
# endif
{
  switch (t->U_1.V_1.Kind) {
  case Defs_ShortInt:;
    *Upb = Values_MaxShortIntVal;
    break;
  case Defs_LongInt:;
    *Upb = Values_MaxLongIntVal;
    break;
  case Defs_ShortCard:;
    *Upb = Values_MaxShortCardVal;
    break;
  case Defs_LongCard:;
    *Upb = Values_MaxLongCardVal;
    break;
  case Defs_Real:;
    *Upb = Values_MaxRealVal;
    break;
  case Defs_LongReal:;
    *Upb = Values_MaxLongRealVal;
    break;
  case Defs_Bool:;
    *Upb = Values_TrueValue;
    break;
  case Defs_Char:;
    *Upb = Values_MaxCharVal;
    break;
  case Defs_Enumeration1:;
    {
      register Defs_yEnumeration1 *W_1 = &t->U_1.V_39.Enumeration1;

      Upb->Kind = Values_Enumeration;
      Upb->U_1.V_7.EnumValue = (ADDRESS)Defs_GetLiteral(W_1->Objects, W_1->MaxValue);
    }
    break;
  case Defs_Subrange1:;
    *Upb = t->U_1.V_44.Subrange1.Upb;
    break;
  default :
    *Upb = Values_ErrorValue;
    break;
  }
}

BOOLEAN Types_Cast
# ifdef __STDC__
(SHORTCARD Operator, Defs_tType t1, Defs_tType t2)
# else
(Operator, t1, t2)
SHORTCARD Operator;
Defs_tType t1, t2;
# endif
{
  if (t1->U_1.V_1.Kind == Defs_Subrange1) {
    t1 = t1->U_1.V_44.Subrange1.Type;
  }
  if (t2->U_1.V_1.Kind == Defs_Subrange1) {
    t2 = t2->U_1.V_44.Subrange1.Type;
  }
  if (t1->U_1.V_1.Kind == Defs_OpaqueType1 && t1->U_1.V_37.OpaqueType1.Type != Defs_NoType && t2->U_1.V_1.Kind == Defs_OpaqueType1 && t2->U_1.V_37.OpaqueType1.Type != Defs_NoType) {
    t1 = t1->U_1.V_37.OpaqueType1.Type;
    t2 = t2->U_1.V_37.OpaqueType1.Type;
  }
  switch (Operator) {
  case Code_cAssign:;
    return t1 == Defs_TypeADDRESS && t2 != Defs_TypeADDRESS || t2 == Defs_TypeADDRESS && t1 != Defs_TypeADDRESS || t1->U_1.V_1.Kind == Defs_OpaqueType1 && t2->U_1.V_1.Kind != Defs_OpaqueType1 || t2->U_1.V_1.Kind == Defs_OpaqueType1 && t1->U_1.V_1.Kind != Defs_OpaqueType1;
    break;
  case Code_cPassAddress:;
    return t1 != t2 && Base_OptionIsSet('c');
    break;
  case Code_cPassValue:;
    if (t2 == Defs_TypeIntCard && (t1 == Defs_TypeSHORTCARD || t1 == Defs_TypeSHORTINT) || t2 == Defs_TypeStringChar && t1 == Defs_TypeCHAR || t2 == Defs_TypeREAL && t1 == Defs_TypeLONGREAL) {
      return FALSE;
    } else {
      return t1 != t2 && Base_OptionIsSet('c');
    }
    break;
  default :
    return FALSE;
    break;
  }
}

void BEGIN_Types()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Values();
    BEGIN_Defs();
    BEGIN_Base();
    BEGIN_Tree();
    BEGIN_Defs();
    BEGIN_Values();
    BEGIN_Code();

    Types_MinReal = 1.40129846432481707E-45;
    Types_MaxReal = 3.40282346638528860E+38;
    Types_MinLongReal = 4.94065645841246544E-324;
    Types_MaxLongReal = 1.79769313486231470E+308;
  }
}
