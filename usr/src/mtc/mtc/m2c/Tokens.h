#define DEFINITION_Tokens

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#define Tokens_TokEOF	0
#define Tokens_TokIdent	1
#define Tokens_TokDecConst	2
#define Tokens_TokOctalConst	3
#define Tokens_TokHexConst	4
#define Tokens_TokCharConst	5
#define Tokens_TokRealConst	6
#define Tokens_TokStringConst	7
#define Tokens_TokNotEqual	8
#define Tokens_TokLParent	9
#define Tokens_TokRParent	10
#define Tokens_TokTimes	11
#define Tokens_TokPlus	12
#define Tokens_TokComma	13
#define Tokens_TokMinus	14
#define Tokens_TokDot	15
#define Tokens_TokRange	16
#define Tokens_TokDivide	17
#define Tokens_TokColon	18
#define Tokens_TokAssign	19
#define Tokens_TokSemiColon	20
#define Tokens_TokLess	21
#define Tokens_TokLessEqual	22
#define Tokens_TokEqual	24
#define Tokens_TokGreater	25
#define Tokens_TokGreaterEqual	26
#define Tokens_TokLBracket	27
#define Tokens_TokRBracket	28
#define Tokens_TokArrow	29
#define Tokens_TokLBrace	30
#define Tokens_TokBar	31
#define Tokens_TokRBrace	32
#define Tokens_TokAnd	34
#define Tokens_TokArray	35
#define Tokens_TokBegin	36
#define Tokens_TokBy	37
#define Tokens_TokCase	38
#define Tokens_TokConst	39
#define Tokens_TokDefinition	40
#define Tokens_TokDiv	41
#define Tokens_TokDo	42
#define Tokens_TokElse	43
#define Tokens_TokElsif	44
#define Tokens_TokEnd	45
#define Tokens_TokExit	46
#define Tokens_TokExport	47
#define Tokens_TokFor	48
#define Tokens_TokFrom	49
#define Tokens_TokIf	50
#define Tokens_TokImplementation	51
#define Tokens_TokImport	52
#define Tokens_TokIn	53
#define Tokens_TokLoop	54
#define Tokens_TokMod	55
#define Tokens_TokModule	56
#define Tokens_TokNot	57
#define Tokens_TokOf	58
#define Tokens_TokOr	59
#define Tokens_TokPointer	60
#define Tokens_TokProcedure	61
#define Tokens_TokQualified	62
#define Tokens_TokRecord	63
#define Tokens_TokRepeat	64
#define Tokens_TokReturn	65
#define Tokens_TokSet	66
#define Tokens_TokThen	67
#define Tokens_TokTo	68
#define Tokens_TokType	69
#define Tokens_TokUntil	70
#define Tokens_TokVar	71
#define Tokens_TokWhile	72
#define Tokens_TokWith	73
#define Tokens_TokForeign	74
typedef SHORTCARD Tokens_tToken;
typedef struct Tokens_1 {
    BITSET A[2 + 1];
} Tokens_tTokenSet;
extern void Tokens_WriteToken ARGS((IO_tFile f, Tokens_tToken Token));
extern void Tokens_WriteTokenSet ARGS((IO_tFile f, Tokens_tTokenSet TokenSet));
extern void BEGIN_Tokens();
