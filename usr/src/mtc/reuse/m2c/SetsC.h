#define DEFINITION_SetsC

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

typedef Sets_ProcOfCard SetsC_ProcOfCard;
typedef Sets_ProcOfCardToBool SetsC_ProcOfCardToBool;
typedef Sets_tSet SetsC_tSet;
extern void SetsC_MakeSet ARGS((SetsC_tSet *Set, CARDINAL MaxSize));
extern void SetsC_ReleaseSet ARGS((SetsC_tSet *Set));
extern void SetsC_Union ARGS((SetsC_tSet *Set1, SetsC_tSet Set2));
extern void SetsC_Difference ARGS((SetsC_tSet *Set1, SetsC_tSet Set2));
extern void SetsC_Intersection ARGS((SetsC_tSet *Set1, SetsC_tSet Set2));
extern void SetsC_SymDiff ARGS((SetsC_tSet *Set1, SetsC_tSet Set2));
extern void SetsC_Complement ARGS((SetsC_tSet *Set));
extern void SetsC_Include ARGS((SetsC_tSet *Set, CARDINAL Elmt));
extern void SetsC_Exclude ARGS((SetsC_tSet *Set, CARDINAL Elmt));
extern CARDINAL SetsC_Card ARGS((SetsC_tSet *Set));
extern CARDINAL SetsC_Size ARGS((SetsC_tSet *Set));
extern CARDINAL SetsC_Minimum ARGS((SetsC_tSet *Set));
extern CARDINAL SetsC_Maximum ARGS((SetsC_tSet *Set));
extern CARDINAL SetsC_Select ARGS((SetsC_tSet *Set));
extern CARDINAL SetsC_Extract ARGS((SetsC_tSet *Set));
extern BOOLEAN SetsC_IsSubset ARGS((SetsC_tSet Set1, SetsC_tSet Set2));
extern BOOLEAN SetsC_IsStrictSubset ARGS((SetsC_tSet Set1, SetsC_tSet Set2));
extern BOOLEAN SetsC_IsEqual ARGS((SetsC_tSet *Set1, SetsC_tSet *Set2));
extern BOOLEAN SetsC_IsNotEqual ARGS((SetsC_tSet Set1, SetsC_tSet Set2));
extern BOOLEAN SetsC_IsElement ARGS((CARDINAL Elmt, SetsC_tSet *Set));
extern BOOLEAN SetsC_IsEmpty ARGS((SetsC_tSet Set));
extern BOOLEAN SetsC_Forall ARGS((SetsC_tSet Set, SetsC_ProcOfCardToBool Proc));
extern BOOLEAN SetsC_Exists ARGS((SetsC_tSet Set, SetsC_ProcOfCardToBool Proc));
extern BOOLEAN SetsC_Exists1 ARGS((SetsC_tSet Set, SetsC_ProcOfCardToBool Proc));
extern void SetsC_Assign ARGS((SetsC_tSet *Set1, SetsC_tSet Set2));
extern void SetsC_AssignElmt ARGS((SetsC_tSet *Set, CARDINAL Elmt));
extern void SetsC_AssignEmpty ARGS((SetsC_tSet *Set));
extern void SetsC_ForallDo ARGS((SetsC_tSet Set, SetsC_ProcOfCard Proc));
extern void SetsC_ReadSet ARGS((IO_tFile f, SetsC_tSet *Set));
extern void SetsC_WriteSet ARGS((IO_tFile f, SetsC_tSet Set));
extern void BEGIN_SetsC();
