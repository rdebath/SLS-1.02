#define DEFINITION_Code

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#define Code_cNoOp	0
#define Code_cNotEqual	1
#define Code_cTimes	2
#define Code_cPlus	3
#define Code_cMinus	4
#define Code_cDivide	5
#define Code_cLess	6
#define Code_cLessEqual	7
#define Code_cEqual	8
#define Code_cGreater	9
#define Code_cGreaterEqual	10
#define Code_cAnd	11
#define Code_cIn	13
#define Code_cMod	14
#define Code_cNot	15
#define Code_cOr	16
#define Code_cUnion	17
#define Code_cDifference	18
#define Code_cIntersection	19
#define Code_cSymDiff	20
#define Code_cIsSubset1	21
#define Code_cIsSubset2	22
#define Code_cAssign	23
#define Code_cPassValue	24
#define Code_cPassAddress	25
extern IO_tFile Code_yyf;
extern PROC Code_Exit;
extern void Code_CodeCompUnits ARGS((Tree_tTree yyP1));
extern void Code_BeginCode ARGS(());
extern void Code_CloseCode ARGS(());
extern void BEGIN_Code();
