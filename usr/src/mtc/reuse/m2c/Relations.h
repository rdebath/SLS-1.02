#define DEFINITION_Relations

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

typedef struct Relations_1 {
    Sets_tSet A[10000000 + 1];
} Relations_ArrayOfSet;
typedef void (*Relations_ProcOfIntInt) ARGS((INTEGER, INTEGER));
typedef BOOLEAN (*Relations_ProcOfIntIntToBool) ARGS((INTEGER, INTEGER));
typedef struct Relations_2 {
    Relations_ArrayOfSet *ArrayPtr;
    SHORTCARD Size1, Size2;
} Relations_tRelation;
extern void Relations_MakeRelation ARGS((Relations_tRelation *Rel, INTEGER Size1, INTEGER Size2));
extern void Relations_ReleaseRelation ARGS((Relations_tRelation *Rel));
extern void Relations_Include ARGS((Relations_tRelation *Rel, INTEGER e1, INTEGER e2));
extern void Relations_Exclude ARGS((Relations_tRelation *Rel, INTEGER e1, INTEGER e2));
extern BOOLEAN Relations_IsElement ARGS((INTEGER e1, INTEGER e2, Relations_tRelation Rel));
extern BOOLEAN Relations_IsRelated ARGS((INTEGER e1, INTEGER e2, Relations_tRelation Rel));
extern BOOLEAN Relations_IsReflexive1 ARGS((INTEGER e1, Relations_tRelation Rel));
extern BOOLEAN Relations_IsSymmetric1 ARGS((INTEGER e1, INTEGER e2, Relations_tRelation Rel));
extern BOOLEAN Relations_IsTransitive1 ARGS((INTEGER e1, INTEGER e2, INTEGER e3, Relations_tRelation Rel));
extern BOOLEAN Relations_IsReflexive ARGS((Relations_tRelation Rel));
extern BOOLEAN Relations_IsSymmetric ARGS((Relations_tRelation Rel));
extern BOOLEAN Relations_IsTransitive ARGS((Relations_tRelation Rel));
extern BOOLEAN Relations_IsEquivalence ARGS((Relations_tRelation Rel));
extern BOOLEAN Relations_HasReflexive ARGS((Relations_tRelation Rel));
extern BOOLEAN Relations_IsCyclic ARGS((Relations_tRelation Rel));
extern void Relations_GetCyclics ARGS((Relations_tRelation Rel, Sets_tSet *Set));
extern void Relations_Closure ARGS((Relations_tRelation *Rel));
extern void Relations_AssignEmpty ARGS((Relations_tRelation *Rel));
extern void Relations_AssignElmt ARGS((Relations_tRelation *Rel, INTEGER e1, INTEGER e2));
extern void Relations_Assign ARGS((Relations_tRelation *Rel1, Relations_tRelation Rel2));
extern void Relations_Union ARGS((Relations_tRelation *Rel1, Relations_tRelation Rel2));
extern void Relations_Difference ARGS((Relations_tRelation *Rel1, Relations_tRelation Rel2));
extern void Relations_Intersection ARGS((Relations_tRelation *Rel1, Relations_tRelation Rel2));
extern void Relations_SymDiff ARGS((Relations_tRelation *Rel1, Relations_tRelation Rel2));
extern void Relations_Complement ARGS((Relations_tRelation *Rel));
extern BOOLEAN Relations_IsSubset ARGS((Relations_tRelation Rel1, Relations_tRelation Rel2));
extern BOOLEAN Relations_IsStrictSubset ARGS((Relations_tRelation Rel1, Relations_tRelation Rel2));
extern BOOLEAN Relations_IsEqual ARGS((Relations_tRelation *Rel1, Relations_tRelation *Rel2));
extern BOOLEAN Relations_IsNotEqual ARGS((Relations_tRelation Rel1, Relations_tRelation Rel2));
extern BOOLEAN Relations_IsEmpty ARGS((Relations_tRelation Rel));
extern INTEGER Relations_Card ARGS((Relations_tRelation *Rel));
extern void Relations_Select ARGS((Relations_tRelation *Rel, INTEGER *e1, INTEGER *e2));
extern void Relations_Extract ARGS((Relations_tRelation *Rel, INTEGER *e1, INTEGER *e2));
extern BOOLEAN Relations_Forall ARGS((Relations_tRelation Rel, Relations_ProcOfIntIntToBool Proc));
extern BOOLEAN Relations_Exists ARGS((Relations_tRelation Rel, Relations_ProcOfIntIntToBool Proc));
extern BOOLEAN Relations_Exists1 ARGS((Relations_tRelation Rel, Relations_ProcOfIntIntToBool Proc));
extern void Relations_ForallDo ARGS((Relations_tRelation Rel, Relations_ProcOfIntInt Proc));
extern void Relations_ReadRelation ARGS((IO_tFile f, Relations_tRelation *Rel));
extern void Relations_WriteRelation ARGS((IO_tFile f, Relations_tRelation Rel));
extern void BEGIN_Relations();
