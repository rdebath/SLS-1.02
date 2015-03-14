#define DEFINITION_Errors

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#define Errors_TooManyErrors	0
#define Errors_SyntaxError	1
#define Errors_ExpectedTokens	2
#define Errors_RestartPoint	3
#define Errors_TokenInserted	4
#define Errors_ReadParseTable	5
#define Errors_IllegalChar	6
#define Errors_UnclosedComment	7
#define Errors_UnclosedString	8
#define Errors_CyclicDefMods	9
#define Errors_ModNotFound	10
#define Errors_NoNEWPROCESS	11
#define Errors_NoTRANSFER	12
#define Errors_NoIOTRANSFER	13
#define Errors_StructTypeReq	14
#define Errors_OpaqueConflict	15
#define Errors_ForeignConflict	16
#define Errors_Underscores	17
#define Errors_OutOfLongRange	18
#define Errors_Fatal	1
#define Errors_Restriction	2
#define Errors_Error	3
#define Errors_Warning	4
#define Errors_Repair	5
#define Errors_Note	6
#define Errors_Information	7
#define Errors_None	0
#define Errors_Integer	1
#define Errors_String	7
#define Errors_Array	8
#define Errors_Ident	10
extern void Errors_ErrorMessageP ARGS((CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Pos));
extern void Errors_ErrorMessagePI ARGS((CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Pos, CARDINAL InfoClass, ADDRESS Info));
extern void Errors_ErrorMessage ARGS((CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Pos));
extern void Errors_ErrorMessageI ARGS((CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Pos, CARDINAL InfoClass, ADDRESS Info));
extern void Errors_CompilerError ARGS((CHAR Proc[], LONGCARD ));
extern CARDINAL Errors_NumberOfErrors ARGS(());
extern void Errors_PrintMessages ARGS((BOOLEAN NoWarnings));
extern void BEGIN_Errors();
