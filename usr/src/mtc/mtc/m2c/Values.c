#include "SYSTEM_.h"

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Types
#include "Types.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

Values_tValue Values_ErrorValue, Values_NilValue, Values_TrueValue, Values_FalseValue, Values_MinCharVal, Values_MaxCharVal, Values_ZeroValue, Values_MinShortIntVal, Values_MaxShortIntVal, Values_MinLongIntVal, Values_MaxLongIntVal, Values_MaxShortCardVal, Values_MaxLongCardVal, Values_MinRealVal, Values_MaxRealVal, Values_MinLongRealVal, Values_MaxLongRealVal;

#define CharKind	(SET_ELEM(Values_Char) | SET_ELEM(Values_StringChar))
#define FalseCode	0
#define TrueCode	1
static Defs_tEnv Env;
static void Evaluate ARGS((Tree_tTree t, Values_tValue *Value));


void Values_CompConst
# ifdef __STDC__
(ADDRESS Tree, ADDRESS pEnv, Values_tValue *Value)
# else
(Tree, pEnv, Value)
ADDRESS Tree, pEnv;
Values_tValue *Value;
# endif
{
  Env = (Defs_tDefs)pEnv;
  Evaluate((Tree_tTree)Tree, Value);
}

static void Evaluate
# ifdef __STDC__
(Tree_tTree t, Values_tValue *Value)
# else
(t, Value)
Tree_tTree t;
Values_tValue *Value;
# endif
{
  Values_tValue Value1, Value2;
  Defs_tObject Object1, Object2;
  Defs_tType Type1;
  Strings_tString string;
  CARDINAL i, size;
  REAL r;

  if (t == NIL) {
    return;
  }
  switch (t->U_1.V_1.Kind) {
  case Tree_Qualid0:;
    t->U_1.V_89.Qualid0.Object = Defs_Identify(t->U_1.V_89.Qualid0.Ident, Env);
    switch (t->U_1.V_89.Qualid0.Object->U_1.V_1.Kind) {
    case Defs_Const1:;
      *Value = t->U_1.V_89.Qualid0.Object->U_1.V_5.Const1.Value;
      return;
      break;
    case Defs_EnumLiteral1:;
      Value->Kind = Values_Enumeration;
      Value->U_1.V_7.EnumValue = (ADDRESS)t->U_1.V_89.Qualid0.Object;
      return;
      break;
    case Defs_TypeDecl1:;
    case Defs_Opaque1:;
      Value->Kind = Values_Type;
      Value->U_1.V_9.TypeValue = (ADDRESS)Defs_GetType(t->U_1.V_89.Qualid0.Object);
      return;
      break;
    case Defs_StdProc1:;
      Value->Kind = Values_StdProc;
      Value->U_1.V_8.StdProcValue = t->U_1.V_89.Qualid0.Object->U_1.V_14.StdProc1.StdProc;
      return;
      break;
    default :
      break;
    }
    break;
  case Tree_Qualid1:;
    Evaluate(t->U_1.V_90.Qualid1.Qualid, Value);
    t->U_1.V_90.Qualid1.Object = Defs_Identify2(t->U_1.V_90.Qualid1.Ident, Defs_GetExport2(t->U_1.V_90.Qualid1.Qualid->U_1.V_88.Qualid.Object));
    switch (t->U_1.V_90.Qualid1.Object->U_1.V_1.Kind) {
    case Defs_Const1:;
      *Value = t->U_1.V_90.Qualid1.Object->U_1.V_5.Const1.Value;
      return;
      break;
    case Defs_EnumLiteral1:;
      Value->Kind = Values_Enumeration;
      Value->U_1.V_7.EnumValue = (ADDRESS)t->U_1.V_90.Qualid1.Object;
      return;
      break;
    case Defs_TypeDecl1:;
    case Defs_Opaque1:;
      Value->Kind = Values_Type;
      Value->U_1.V_9.TypeValue = (ADDRESS)Defs_GetType(t->U_1.V_90.Qualid1.Object);
      return;
      break;
    case Defs_StdProc1:;
      Value->Kind = Values_StdProc;
      Value->U_1.V_8.StdProcValue = t->U_1.V_90.Qualid1.Object->U_1.V_14.StdProc1.StdProc;
      return;
      break;
    default :
      break;
    }
    break;
  case Tree_Binary:;
    Evaluate(t->U_1.V_78.Binary.Lop, &Value1);
    Evaluate(t->U_1.V_78.Binary.Rop, &Value2);
    switch (t->U_1.V_78.Binary.Operator) {
    case Tree_Plus:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Integer;
        Value->U_1.V_1.IntValue = Value1.U_1.V_1.IntValue + Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Real;
        Value->U_1.V_2.RealValue = Value1.U_1.V_2.RealValue + Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Bitset && Value2.Kind == Values_Bitset) {
        Value->Kind = Values_Bitset;
        Value->U_1.V_6.BitsetValue = Value1.U_1.V_6.BitsetValue | Value2.U_1.V_6.BitsetValue;
        return;
      }
      break;
    case Tree_Minus:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Integer;
        Value->U_1.V_1.IntValue = Value1.U_1.V_1.IntValue - Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Real;
        Value->U_1.V_2.RealValue = Value1.U_1.V_2.RealValue - Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Bitset && Value2.Kind == Values_Bitset) {
        Value->Kind = Values_Bitset;
        Value->U_1.V_6.BitsetValue = SET_DIFF(Value1.U_1.V_6.BitsetValue, Value2.U_1.V_6.BitsetValue);
        return;
      }
      break;
    case Tree_Times:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Integer;
        Value->U_1.V_1.IntValue = Value1.U_1.V_1.IntValue * Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Real;
        Value->U_1.V_2.RealValue = Value1.U_1.V_2.RealValue * Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Bitset && Value2.Kind == Values_Bitset) {
        Value->Kind = Values_Bitset;
        Value->U_1.V_6.BitsetValue = Value1.U_1.V_6.BitsetValue & Value2.U_1.V_6.BitsetValue;
        return;
      }
      break;
    case Tree_Divide:;
      if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Real;
        Value->U_1.V_2.RealValue = Value1.U_1.V_2.RealValue / Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Bitset && Value2.Kind == Values_Bitset) {
        Value->Kind = Values_Bitset;
        Value->U_1.V_6.BitsetValue = Value1.U_1.V_6.BitsetValue ^ Value2.U_1.V_6.BitsetValue;
        return;
      }
      break;
    case Tree_Div:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        if (Value2.U_1.V_1.IntValue != 0) {
          Value->Kind = Values_Integer;
          Value->U_1.V_1.IntValue = Value1.U_1.V_1.IntValue / Value2.U_1.V_1.IntValue;
          return;
        }
      }
      break;
    case Tree_Mod:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        if (Value2.U_1.V_1.IntValue != 0) {
          Value->Kind = Values_Integer;
          Value->U_1.V_1.IntValue = Value1.U_1.V_1.IntValue % Value2.U_1.V_1.IntValue;
          return;
        }
      }
      break;
    case Tree_Less:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_1.IntValue < Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_2.RealValue < Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Boolean && Value2.Kind == Values_Boolean) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_3.BoolValue < Value2.U_1.V_3.BoolValue;
        return;
      } else if (IN(Value1.Kind, CharKind) && IN(Value2.Kind, CharKind)) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_4.CharValue < Value2.U_1.V_4.CharValue;
        return;
      } else if (Value1.Kind == Values_Enumeration && Value2.Kind == Values_Enumeration) {
        Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
        Object2 = (Defs_tDefs)Value2.U_1.V_7.EnumValue;
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Object1->U_1.V_6.EnumLiteral1.Index < Object2->U_1.V_6.EnumLiteral1.Index;
        return;
      }
      break;
    case Tree_LessEqual:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_1.IntValue <= Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_2.RealValue <= Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Boolean && Value2.Kind == Values_Boolean) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_3.BoolValue <= Value2.U_1.V_3.BoolValue;
        return;
      } else if (IN(Value1.Kind, CharKind) && IN(Value2.Kind, CharKind)) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_4.CharValue <= Value2.U_1.V_4.CharValue;
        return;
      } else if (Value1.Kind == Values_Bitset && Value2.Kind == Values_Bitset) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = SET_IS_SUBSET1(Value1.U_1.V_6.BitsetValue, Value2.U_1.V_6.BitsetValue);
        return;
      } else if (Value1.Kind == Values_Enumeration && Value2.Kind == Values_Enumeration) {
        Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
        Object2 = (Defs_tDefs)Value2.U_1.V_7.EnumValue;
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Object1->U_1.V_6.EnumLiteral1.Index <= Object2->U_1.V_6.EnumLiteral1.Index;
        return;
      }
      break;
    case Tree_Equal:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_1.IntValue == Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_2.RealValue == Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Boolean && Value2.Kind == Values_Boolean) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_3.BoolValue == Value2.U_1.V_3.BoolValue;
        return;
      } else if (IN(Value1.Kind, CharKind) && IN(Value2.Kind, CharKind)) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_4.CharValue == Value2.U_1.V_4.CharValue;
        return;
      } else if (Value1.Kind == Values_Bitset && Value2.Kind == Values_Bitset) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_6.BitsetValue == Value2.U_1.V_6.BitsetValue;
        return;
      } else if (Value1.Kind == Values_NilType && Value2.Kind == Values_NilType) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = TRUE;
        return;
      } else if (Value1.Kind == Values_Enumeration && Value2.Kind == Values_Enumeration) {
        Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
        Object2 = (Defs_tDefs)Value2.U_1.V_7.EnumValue;
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Object1->U_1.V_6.EnumLiteral1.Index == Object2->U_1.V_6.EnumLiteral1.Index;
        return;
      }
      break;
    case Tree_NotEqual:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_1.IntValue != Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_2.RealValue != Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Boolean && Value2.Kind == Values_Boolean) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_3.BoolValue != Value2.U_1.V_3.BoolValue;
        return;
      } else if (IN(Value1.Kind, CharKind) && IN(Value2.Kind, CharKind)) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_4.CharValue != Value2.U_1.V_4.CharValue;
        return;
      } else if (Value1.Kind == Values_Bitset && Value2.Kind == Values_Bitset) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_6.BitsetValue != Value2.U_1.V_6.BitsetValue;
        return;
      } else if (Value1.Kind == Values_NilType && Value2.Kind == Values_NilType) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = FALSE;
        return;
      } else if (Value1.Kind == Values_Enumeration && Value2.Kind == Values_Enumeration) {
        Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
        Object2 = (Defs_tDefs)Value2.U_1.V_7.EnumValue;
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Object1->U_1.V_6.EnumLiteral1.Index != Object2->U_1.V_6.EnumLiteral1.Index;
        return;
      }
      break;
    case Tree_Greater:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_1.IntValue > Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_2.RealValue > Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Boolean && Value2.Kind == Values_Boolean) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_3.BoolValue > Value2.U_1.V_3.BoolValue;
        return;
      } else if (IN(Value1.Kind, CharKind) && IN(Value2.Kind, CharKind)) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_4.CharValue > Value2.U_1.V_4.CharValue;
        return;
      } else if (Value1.Kind == Values_Enumeration && Value2.Kind == Values_Enumeration) {
        Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
        Object2 = (Defs_tDefs)Value2.U_1.V_7.EnumValue;
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Object1->U_1.V_6.EnumLiteral1.Index > Object2->U_1.V_6.EnumLiteral1.Index;
        return;
      }
      break;
    case Tree_GreaterEqual:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_1.IntValue >= Value2.U_1.V_1.IntValue;
        return;
      } else if (Value1.Kind == Values_Real && Value2.Kind == Values_Real) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_2.RealValue >= Value2.U_1.V_2.RealValue;
        return;
      } else if (Value1.Kind == Values_Boolean && Value2.Kind == Values_Boolean) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_3.BoolValue >= Value2.U_1.V_3.BoolValue;
        return;
      } else if (IN(Value1.Kind, CharKind) && IN(Value2.Kind, CharKind)) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_4.CharValue >= Value2.U_1.V_4.CharValue;
        return;
      } else if (Value1.Kind == Values_Bitset && Value2.Kind == Values_Bitset) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = SET_IS_SUBSET2(Value1.U_1.V_6.BitsetValue, Value2.U_1.V_6.BitsetValue);
        return;
      } else if (Value1.Kind == Values_Enumeration && Value2.Kind == Values_Enumeration) {
        Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
        Object2 = (Defs_tDefs)Value2.U_1.V_7.EnumValue;
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Object1->U_1.V_6.EnumLiteral1.Index >= Object2->U_1.V_6.EnumLiteral1.Index;
        return;
      }
      break;
    case Tree_And:;
      if (Value1.Kind == Values_Boolean && Value2.Kind == Values_Boolean) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_3.BoolValue && Value2.U_1.V_3.BoolValue;
        return;
      }
      break;
    case Tree_Or:;
      if (Value1.Kind == Values_Boolean && Value2.Kind == Values_Boolean) {
        Value->Kind = Values_Boolean;
        Value->U_1.V_3.BoolValue = Value1.U_1.V_3.BoolValue || Value2.U_1.V_3.BoolValue;
        return;
      }
      break;
    case Tree_In:;
      if (Value1.Kind == Values_Integer && Value2.Kind == Values_Bitset) {
        if (Value1.U_1.V_1.IntValue >= 0 && Value1.U_1.V_1.IntValue <= Types_MaxBitset) {
          Value->Kind = Values_Boolean;
          Value->U_1.V_3.BoolValue = IN(Value1.U_1.V_1.IntValue, Value2.U_1.V_6.BitsetValue);
          return;
        }
      } else if (IN(Value1.Kind, CharKind) && Value2.Kind == Values_Bitset) {
        if (ORD(Value1.U_1.V_4.CharValue) <= Types_MaxBitset) {
          Value->Kind = Values_Boolean;
          Value->U_1.V_3.BoolValue = IN(ORD(Value1.U_1.V_4.CharValue), Value2.U_1.V_6.BitsetValue);
          return;
        }
      } else if (Value1.Kind == Values_Enumeration && Value2.Kind == Values_Bitset) {
        Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
        if (Object1->U_1.V_6.EnumLiteral1.Index <= Types_MaxBitset) {
          Value->Kind = Values_Boolean;
          Value->U_1.V_3.BoolValue = IN(Object1->U_1.V_6.EnumLiteral1.Index, Value2.U_1.V_6.BitsetValue);
          return;
        }
      }
      break;
    }
    break;
  case Tree_Unary:;
    Evaluate(t->U_1.V_79.Unary.Mop, Value);
    switch (t->U_1.V_79.Unary.Operator) {
    case Tree_Plus:;
      break;
    case Tree_Minus:;
      if (Value->Kind == Values_Integer) {
        Value->U_1.V_1.IntValue = -Value->U_1.V_1.IntValue;
        return;
      } else if (Value->Kind == Values_Real) {
        Value->U_1.V_2.RealValue = -Value->U_1.V_2.RealValue;
        return;
      }
      break;
    case Tree_Not:;
      if (Value->Kind == Values_Boolean) {
        Value->U_1.V_3.BoolValue = !Value->U_1.V_3.BoolValue;
        return;
      }
      break;
    }
    break;
  case Tree_IntConst:;
    if (t->U_1.V_80.IntConst.IntVal <= Types_MaxLongInt) {
      Value->Kind = Values_Integer;
      Value->U_1.V_1.IntValue = t->U_1.V_80.IntConst.IntVal;
    } else {
      *Value = Values_ErrorValue;
    }
    return;
    break;
  case Tree_RealConst:;
    Value->Kind = Values_Real;
    StringMem_GetString(t->U_1.V_81.RealConst.RealVal, &string);
    Value->U_1.V_2.RealValue = Strings_StringToReal(&string);
    return;
    break;
  case Tree_StringConst:;
    StringMem_GetString(t->U_1.V_82.StringConst.StringVal, &string);
    if (Strings_Length(&string) == 1) {
      Value->Kind = Values_StringChar;
      Value->U_1.V_4.CharValue = Strings_Char(&string, 1);
    } else {
      Value->Kind = Values_String;
      Value->U_1.V_5.StringValue = t->U_1.V_82.StringConst.StringVal;
    }
    return;
    break;
  case Tree_CharConst:;
    Value->Kind = Values_Char;
    Value->U_1.V_4.CharValue = t->U_1.V_83.CharConst.CharVal;
    return;
    break;
  case Tree_FuncCall:;
    Evaluate(t->U_1.V_84.FuncCall.Designator, Value);
    if (Value->Kind == Values_StdProc) {
      Evaluate(t->U_1.V_84.FuncCall.Actuals, &Value1);
      switch (Value->U_1.V_8.StdProcValue) {
      case Defs_ProcABS:;
        if (Value1.Kind == Values_Integer) {
          Value->Kind = Values_Integer;
          Value->U_1.V_1.IntValue = ABSLI(Value1.U_1.V_1.IntValue);
          return;
        } else if (Value1.Kind == Values_Real) {
          Value->Kind = Values_Real;
          Value->U_1.V_2.RealValue = ABSLR(Value1.U_1.V_2.RealValue);
          return;
        }
        break;
      case Defs_ProcCAP:;
        if (IN(Value1.Kind, CharKind)) {
          Value->Kind = Values_Char;
          Value->U_1.V_4.CharValue = CAP(Value1.U_1.V_4.CharValue);
          return;
        }
        break;
      case Defs_ProcCHR:;
        if (Value1.Kind == Values_Integer) {
          Value->Kind = Values_Char;
          Value->U_1.V_4.CharValue = CHR((CARDINAL)Value1.U_1.V_1.IntValue);
          return;
        }
        break;
      case Defs_ProcFLOAT:;
        if (Value1.Kind == Values_Integer) {
          Value->Kind = Values_Real;
          Value->U_1.V_2.RealValue = FLOAT((CARDINAL)Value1.U_1.V_1.IntValue);
          return;
        }
        break;
      case Defs_ProcMIN:;
        if (Value1.Kind == Values_Type) {
          Type1 = (Defs_tDefs)Value1.U_1.V_9.TypeValue;
          switch (Type1->U_1.V_1.Kind) {
          case Defs_ShortInt:;
            *Value = Values_MinShortIntVal;
            return;
            break;
          case Defs_LongInt:;
            *Value = Values_MinLongIntVal;
            return;
            break;
          case Defs_ShortCard:;
            *Value = Values_ZeroValue;
            return;
            break;
          case Defs_LongCard:;
            *Value = Values_ZeroValue;
            return;
            break;
          case Defs_Real:;
            *Value = Values_MinRealVal;
            return;
            break;
          case Defs_LongReal:;
            *Value = Values_MinLongRealVal;
            return;
            break;
          case Defs_Bool:;
            *Value = Values_FalseValue;
            return;
            break;
          case Defs_Char:;
            *Value = Values_MinCharVal;
            return;
            break;
          case Defs_Enumeration1:;
            Value->Kind = Values_Enumeration;
            Value->U_1.V_7.EnumValue = (ADDRESS)Defs_GetLiteral(Type1->U_1.V_39.Enumeration1.Objects, 0);
            return;
            break;
          case Defs_Subrange1:;
            *Value = Type1->U_1.V_44.Subrange1.Lwb;
            return;
            break;
          default :
            break;
          }
        }
        break;
      case Defs_ProcMAX:;
        if (Value1.Kind == Values_Type) {
          Type1 = (Defs_tDefs)Value1.U_1.V_9.TypeValue;
          switch (Type1->U_1.V_1.Kind) {
          case Defs_ShortInt:;
            *Value = Values_MaxShortIntVal;
            return;
            break;
          case Defs_LongInt:;
            *Value = Values_MaxLongIntVal;
            return;
            break;
          case Defs_ShortCard:;
            *Value = Values_MaxShortCardVal;
            return;
            break;
          case Defs_LongCard:;
            *Value = Values_MaxLongCardVal;
            return;
            break;
          case Defs_Real:;
            *Value = Values_MaxRealVal;
            return;
            break;
          case Defs_LongReal:;
            *Value = Values_MaxLongRealVal;
            return;
            break;
          case Defs_Bool:;
            *Value = Values_TrueValue;
            return;
            break;
          case Defs_Char:;
            *Value = Values_MaxCharVal;
            return;
            break;
          case Defs_Enumeration1:;
            Value->Kind = Values_Enumeration;
            {
              register Defs_yEnumeration1 *W_1 = &Type1->U_1.V_39.Enumeration1;

              Value->U_1.V_7.EnumValue = (ADDRESS)Defs_GetLiteral(W_1->Objects, W_1->MaxValue);
            }
            return;
            break;
          case Defs_Subrange1:;
            *Value = Type1->U_1.V_44.Subrange1.Upb;
            return;
            break;
          default :
            break;
          }
        }
        break;
      case Defs_ProcODD:;
        if (Value1.Kind == Values_Integer) {
          Value->Kind = Values_Boolean;
          Value->U_1.V_3.BoolValue = ODD(Value1.U_1.V_1.IntValue);
          return;
        }
        break;
      case Defs_ProcORD:;
        if (Value1.Kind == Values_Integer) {
          *Value = Value1;
          return;
        } else if (IN(Value1.Kind, CharKind)) {
          Value->Kind = Values_Integer;
          Value->U_1.V_1.IntValue = ORD(Value1.U_1.V_4.CharValue);
          return;
        } else if (Value1.Kind == Values_Enumeration) {
          Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
          Value->Kind = Values_Integer;
          Value->U_1.V_1.IntValue = Object1->U_1.V_6.EnumLiteral1.Index;
          return;
        }
        break;
      case Defs_ProcSIZE:;
      case Defs_ProcTSIZE:;
        if (Value1.Kind == Values_Type) {
          size = Types_TypeSize((Defs_tDefs)Value1.U_1.V_9.TypeValue);
          if (size != Types_NoSize) {
            Value->Kind = Values_Integer;
            Value->U_1.V_1.IntValue = size;
            return;
          }
        }
        break;
      case Defs_ProcTRUNC:;
        if (Value1.Kind == Values_Real) {
          r = Value1.U_1.V_2.RealValue;
          Value->Kind = Values_Integer;
          Value->U_1.V_1.IntValue = TRUNC(r);
          return;
        }
        break;
      case Defs_ProcVAL:;
        if (t->U_1.V_84.FuncCall.Actuals->U_1.V_1.Kind != Tree_Actuals0) {
          Evaluate(t->U_1.V_84.FuncCall.Actuals->U_1.V_101.Actual.Next, &Value2);
          if (Value1.Kind == Values_Type && Value2.Kind == Values_Integer) {
            Type1 = (Defs_tDefs)Value1.U_1.V_9.TypeValue;
            if (Type1->U_1.V_1.Kind == Defs_Subrange1) {
              Type1 = Type1->U_1.V_44.Subrange1.Type;
            }
            switch (Type1->U_1.V_1.Kind) {
            case Values_Char:;
              Value->Kind = Values_Char;
              Value->U_1.V_4.CharValue = VAL(CHAR, Value2.U_1.V_1.IntValue);
              return;
              break;
            case Defs_ShortInt:;
            case Defs_LongInt:;
            case Defs_ShortCard:;
            case Defs_LongCard:;
              *Value = Value2;
              return;
              break;
            case Defs_Enumeration1:;
              Object1 = Defs_GetLiteral(Type1->U_1.V_39.Enumeration1.Objects, (SHORTCARD)Value2.U_1.V_1.IntValue);
              if (Object1 != Defs_NoObject) {
                Value->U_1.V_7.EnumValue = (ADDRESS)Object1;
                Value->Kind = Values_Enumeration;
                return;
              }
              break;
            default :
              break;
            }
          }
        }
        break;
      default :
        break;
      }
    }
    break;
  case Tree_Set:;
    Evaluate(t->U_1.V_85.Set.Elems, Value);
    return;
    break;
  case Tree_BitSet:;
    Evaluate(t->U_1.V_86.BitSet.Elems, Value);
    return;
    break;
  case Tree_Elems0:;
    Value->Kind = Values_Bitset;
    Value->U_1.V_6.BitsetValue = 0X0L;
    return;
    break;
  case Tree_Elem:;
    Evaluate(t->U_1.V_97.Elem.Next, Value);
    Evaluate(t->U_1.V_97.Elem.Elem, &Value2);
    switch (Value2.Kind) {
    case Values_Integer:;
      if (Value2.U_1.V_1.IntValue >= 0 && Value2.U_1.V_1.IntValue <= Types_MaxBitset) {
        INCL(Value->U_1.V_6.BitsetValue, Value2.U_1.V_1.IntValue);
        return;
      }
      break;
    case Values_Char:;
    case Values_StringChar:;
      if (ORD(Value2.U_1.V_4.CharValue) <= Types_MaxBitset) {
        INCL(Value->U_1.V_6.BitsetValue, ORD(Value2.U_1.V_4.CharValue));
        return;
      }
      break;
    case Values_Enumeration:;
      Object2 = (Defs_tDefs)Value2.U_1.V_7.EnumValue;
      if (Object2->U_1.V_6.EnumLiteral1.Index <= Types_MaxBitset) {
        INCL(Value->U_1.V_6.BitsetValue, Object2->U_1.V_6.EnumLiteral1.Index);
        return;
      }
      break;
    default :
      break;
    }
    break;
  case Tree_ElemRange:;
    Evaluate(t->U_1.V_98.ElemRange.Next, Value);
    Evaluate(t->U_1.V_98.ElemRange.Lwb, &Value1);
    Evaluate(t->U_1.V_98.ElemRange.Upb, &Value2);
    if (Value1.Kind == Values_Integer && Value2.Kind == Values_Integer) {
      if (Value1.U_1.V_1.IntValue >= 0 && Value2.U_1.V_1.IntValue <= Types_MaxBitset) {
        {
          LONGCARD B_1 = (CARDINAL)Value1.U_1.V_1.IntValue, B_2 = (CARDINAL)Value2.U_1.V_1.IntValue;

          if (B_1 <= B_2)
            for (i = B_1;; i += 1) {
              INCL(Value->U_1.V_6.BitsetValue, i);
              if (i >= B_2) break;
            }
        }
        return;
      }
    } else if (IN(Value1.Kind, CharKind) && IN(Value2.Kind, CharKind)) {
      if (ORD(Value2.U_1.V_4.CharValue) <= Types_MaxBitset) {
        {
          LONGCARD B_3 = ORD(Value1.U_1.V_4.CharValue), B_4 = ORD(Value2.U_1.V_4.CharValue);

          if (B_3 <= B_4)
            for (i = B_3;; i += 1) {
              INCL(Value->U_1.V_6.BitsetValue, i);
              if (i >= B_4) break;
            }
        }
        return;
      }
    } else if (Value1.Kind == Values_Enumeration && Value2.Kind == Values_Enumeration) {
      Object1 = (Defs_tDefs)Value1.U_1.V_7.EnumValue;
      Object2 = (Defs_tDefs)Value2.U_1.V_7.EnumValue;
      if (Object2->U_1.V_6.EnumLiteral1.Index <= Types_MaxBitset) {
        {
          LONGCARD B_5 = Object1->U_1.V_6.EnumLiteral1.Index, B_6 = Object2->U_1.V_6.EnumLiteral1.Index;

          if (B_5 <= B_6)
            for (i = B_5;; i += 1) {
              INCL(Value->U_1.V_6.BitsetValue, i);
              if (i >= B_6) break;
            }
        }
        return;
      }
    }
    break;
  case Tree_Actuals0:;
    break;
  case Tree_Actual:;
    Evaluate(t->U_1.V_101.Actual.Expr, Value);
    return;
    break;
  default :
    break;
  }
  Value->Kind = Values_NoValue;
}

ADDRESS Values_TypeOfValue
# ifdef __STDC__
(Values_tValue Value)
# else
(Value)
Values_tValue Value;
# endif
{
  switch (Value.Kind) {
  case Values_Integer:;
    return (ADDRESS)Defs_TypeIntCard;
    break;
  case Values_Real:;
    return (ADDRESS)Defs_TypeREAL;
    break;
  case Values_Boolean:;
    return (ADDRESS)Defs_TypeBOOLEAN;
    break;
  case Values_Char:;
    return (ADDRESS)Defs_TypeCHAR;
    break;
  case Values_StringChar:;
    return (ADDRESS)Defs_TypeStringChar;
    break;
  case Values_String:;
    return (ADDRESS)Defs_TypeSTRING;
    break;
  case Values_Bitset:;
    return (ADDRESS)Defs_TypeBITSET;
    break;
  case Values_Enumeration:;
    return (ADDRESS)Defs_GetType((Defs_tDefs)Value.U_1.V_7.EnumValue);
    break;
  case Values_NilType:;
    return (ADDRESS)Defs_TypeNIL;
    break;
  case Values_Type:;
    return Value.U_1.V_9.TypeValue;
    break;
  default :
    return (ADDRESS)Defs_NoType;
    break;
  }
}

LONGINT Values_ValueToInt
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
    return Value.U_1.V_1.IntValue;
    break;
  case Values_Boolean:;
    if (Value.U_1.V_3.BoolValue) {
      return TrueCode;
    } else {
      return FalseCode;
    }
    break;
  case Values_Char:;
    return ORD(Value.U_1.V_4.CharValue);
    break;
  case Values_StringChar:;
    return ORD(Value.U_1.V_4.CharValue);
    break;
  case Values_Enumeration:;
    Object = (Defs_tDefs)Value.U_1.V_7.EnumValue;
    return Object->U_1.V_6.EnumLiteral1.Index;
    break;
  default :
    return 0;
    break;
  }
}

void Values_WriteValue
# ifdef __STDC__
(IO_tFile f, Values_tValue Value)
# else
(f, Value)
IO_tFile f;
Values_tValue Value;
# endif
{
  Defs_tObject Object;

  {
    register Values_tValue *W_2 = &Value;

    switch (W_2->Kind) {
    case Values_NoValue:;
      IO_WriteS(f, (STRING)"NoValue", 7L);
      break;
    case Values_Integer:;
      IO_WriteI(f, W_2->U_1.V_1.IntValue, 0L);
      break;
    case Values_Real:;
      IO_WriteR(f, (REAL)W_2->U_1.V_2.RealValue, 1L, 10L, 1L);
      break;
    case Values_Boolean:;
      IO_WriteB(f, W_2->U_1.V_3.BoolValue);
      break;
    case Values_Char:;
      IO_WriteC(f, W_2->U_1.V_4.CharValue);
      break;
    case Values_StringChar:;
      IO_WriteC(f, W_2->U_1.V_4.CharValue);
      break;
    case Values_String:;
      StringMem_WriteString(f, W_2->U_1.V_5.StringValue);
      break;
    case Values_Bitset:;
      IO_WriteN(f, (LONGCARD)(INTEGER)W_2->U_1.V_6.BitsetValue, 8L, 16L);
      break;
    case Values_Enumeration:;
      Object = (Defs_tDefs)W_2->U_1.V_7.EnumValue;
      IO_WriteI(f, (LONGINT)Object->U_1.V_6.EnumLiteral1.Index, 0L);
      break;
    case Values_NilType:;
      IO_WriteS(f, (STRING)"NIL", 3L);
      break;
    case Values_StdProc:;
      IO_WriteS(f, (STRING)"<StdProc>", 9L);
      break;
    case Values_Type:;
      IO_WriteS(f, (STRING)"<Type>", 6L);
      break;
    }
  }
}

void BEGIN_Values()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_StringMem();
    BEGIN_IO();
    BEGIN_StringMem();
    BEGIN_Strings();
    BEGIN_Tree();
    BEGIN_Defs();
    BEGIN_Types();
    BEGIN_Defs();
    BEGIN_Tree();
    BEGIN_Strings();

    {
      register Values_tValue *W_3 = &Values_ErrorValue;

      W_3->Kind = Values_NoValue;
    }
    {
      register Values_tValue *W_4 = &Values_NilValue;

      W_4->Kind = Values_NilType;
    }
    {
      register Values_tValue *W_5 = &Values_TrueValue;

      W_5->Kind = Values_Boolean;
      W_5->U_1.V_3.BoolValue = TRUE;
    }
    {
      register Values_tValue *W_6 = &Values_FalseValue;

      W_6->Kind = Values_Boolean;
      W_6->U_1.V_3.BoolValue = FALSE;
    }
    {
      register Values_tValue *W_7 = &Values_MinCharVal;

      W_7->Kind = Values_Char;
      W_7->U_1.V_4.CharValue = Types_MinChar;
    }
    {
      register Values_tValue *W_8 = &Values_MaxCharVal;

      W_8->Kind = Values_Char;
      W_8->U_1.V_4.CharValue = Types_MaxChar;
    }
    {
      register Values_tValue *W_9 = &Values_ZeroValue;

      W_9->Kind = Values_Integer;
      W_9->U_1.V_1.IntValue = 0;
    }
    {
      register Values_tValue *W_10 = &Values_MinShortIntVal;

      W_10->Kind = Values_Integer;
      W_10->U_1.V_1.IntValue = Types_MinShortInt;
    }
    {
      register Values_tValue *W_11 = &Values_MaxShortIntVal;

      W_11->Kind = Values_Integer;
      W_11->U_1.V_1.IntValue = Types_MaxShortInt;
    }
    {
      register Values_tValue *W_12 = &Values_MinLongIntVal;

      W_12->Kind = Values_Integer;
      W_12->U_1.V_1.IntValue = Types_MinLongInt;
    }
    {
      register Values_tValue *W_13 = &Values_MaxLongIntVal;

      W_13->Kind = Values_Integer;
      W_13->U_1.V_1.IntValue = Types_MaxLongInt;
    }
    {
      register Values_tValue *W_14 = &Values_MaxShortCardVal;

      W_14->Kind = Values_Integer;
      W_14->U_1.V_1.IntValue = Types_MaxShortCard;
    }
    Values_MaxLongCardVal = Values_ErrorValue;
    {
      register Values_tValue *W_15 = &Values_MinRealVal;

      W_15->Kind = Values_Real;
      W_15->U_1.V_2.RealValue = Types_MinReal;
    }
    {
      register Values_tValue *W_16 = &Values_MaxRealVal;

      W_16->Kind = Values_Real;
      W_16->U_1.V_2.RealValue = Types_MaxReal;
    }
    {
      register Values_tValue *W_17 = &Values_MinLongRealVal;

      W_17->Kind = Values_Real;
      W_17->U_1.V_2.RealValue = Types_MinLongReal;
    }
    {
      register Values_tValue *W_18 = &Values_MaxLongRealVal;

      W_18->Kind = Values_Real;
      W_18->U_1.V_2.RealValue = Types_MaxLongReal;
    }
  }
}
