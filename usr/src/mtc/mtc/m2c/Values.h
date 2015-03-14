#define DEFINITION_Values

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#define Values_NoValue	0
#define Values_Integer	1
#define Values_Real	2
#define Values_Boolean	3
#define Values_Char	4
#define Values_StringChar	5
#define Values_String	6
#define Values_Bitset	7
#define Values_Enumeration	8
#define Values_NilType	9
#define Values_StdProc	10
#define Values_Type	11
typedef struct Values_1 {
    SHORTINT Kind;
    union {
        struct {
            LONGINT IntValue;
        } V_1;
        struct {
            LONGREAL RealValue;
        } V_2;
        struct {
            BOOLEAN BoolValue;
        } V_3;
        struct {
            CHAR CharValue;
        } V_4;
        struct {
            StringMem_tStringRef StringValue;
        } V_5;
        struct {
            BITSET BitsetValue;
        } V_6;
        struct {
            ADDRESS EnumValue;
        } V_7;
        struct {
            SHORTCARD StdProcValue;
        } V_8;
        struct {
            ADDRESS TypeValue;
        } V_9;
    } U_1;
} Values_tValue;
extern Values_tValue Values_ErrorValue, Values_NilValue, Values_TrueValue, Values_FalseValue, Values_MinCharVal, Values_MaxCharVal, Values_ZeroValue, Values_MinShortIntVal, Values_MaxShortIntVal, Values_MinLongIntVal, Values_MaxLongIntVal, Values_MaxShortCardVal, Values_MaxLongCardVal, Values_MinRealVal, Values_MaxRealVal, Values_MinLongRealVal, Values_MaxLongRealVal;
extern void Values_CompConst ARGS((ADDRESS Tree, ADDRESS Env, Values_tValue *Value));
extern ADDRESS Values_TypeOfValue ARGS((Values_tValue Value));
extern LONGINT Values_ValueToInt ARGS((Values_tValue Value));
extern void Values_WriteValue ARGS((IO_tFile f, Values_tValue Value));
extern void BEGIN_Values();
