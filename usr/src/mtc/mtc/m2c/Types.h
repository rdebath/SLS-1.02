#define DEFINITION_Types

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#define Types_MinShortInt	-32768
#define Types_MaxShortInt	32767
#define Types_MinLongInt	-2147483648
#define Types_MaxLongInt	2147483647
#define Types_MaxShortCard	65535
#define Types_MaxLongCard	(LONGCARD)4294967295
#define Types_MinChar	'\0'
#define Types_MaxChar	((CHAR)'\377')
#define Types_MaxBitset	31
extern REAL Types_MinReal, Types_MaxReal;
extern LONGREAL Types_MinLongReal, Types_MaxLongReal;
#define Types_NoSize	0
#define Types_SizeUnsignedChar	1
#define Types_SizeShort	2
#define Types_SizeLong	4
#define Types_SizeUnsignedShort	2
#define Types_SizeUnsignedLong	4
#define Types_SizeFloat	4
#define Types_SizeDouble	8
#define Types_PointerSize	4
extern Defs_tType Types_ResultType ARGS((SHORTCARD Operator, Defs_tType t1, Defs_tType t2));
extern Defs_tType Types_StdResultType ARGS((Defs_tType t, Defs_tTypes a));
extern CARDINAL Types_TypeSize ARGS((Defs_tType t));
extern void Types_GetLwb ARGS((Defs_tType t, Values_tValue *Lwb));
extern void Types_GetUpb ARGS((Defs_tType t, Values_tValue *Upb));
extern BOOLEAN Types_Cast ARGS((SHORTCARD Operator, Defs_tType t1, Defs_tType t2));
extern void BEGIN_Types();
