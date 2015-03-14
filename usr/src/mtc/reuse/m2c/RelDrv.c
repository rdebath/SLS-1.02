#include "SYSTEM_.h"

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_Relations
#include "Relations.h"
#endif

static Relations_tRelation r1, r2;
static INTEGER s;
static CHAR c;
static Sets_tSet s2;


void BEGIN_MODULE()
{
  BEGIN_IO();
  BEGIN_Sets();
  BEGIN_Relations();

  Relations_MakeRelation(&r2, 10L, 20L);
  Relations_Include(&r2, 0L, 9L);
  Relations_Include(&r2, 9L, 1L);
  Relations_Include(&r2, 1L, 8L);
  Relations_Include(&r2, 8L, 0L);
  Relations_WriteRelation((System_tFile)IO_StdOutput, r2);
  IO_WriteNl((System_tFile)IO_StdOutput);
  Relations_Closure(&r2);
  Relations_WriteRelation((System_tFile)IO_StdOutput, r2);
  IO_WriteNl((System_tFile)IO_StdOutput);
  Relations_ReleaseRelation(&r2);
  Relations_MakeRelation(&r1, 10L, 20L);
  Relations_Include(&r1, 2L, 3L);
  Relations_Include(&r1, 3L, 4L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"enter Size and Relation like below! (Size=0 terminates)", 55L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteI((System_tFile)IO_StdOutput, 4L, 3L);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)" ", 1L);
  Relations_WriteRelation((System_tFile)IO_StdOutput, r1);
  IO_WriteNl((System_tFile)IO_StdOutput);
  Relations_ReleaseRelation(&r1);
  for (;;) {
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteFlush((System_tFile)IO_StdOutput);
    s = IO_ReadI((System_tFile)IO_StdInput);
    if (s == 0) {
      goto EXIT_1;
    }
    Sets_MakeSet(&s2, (LONGCARD)s);
    Relations_MakeRelation(&r2, s, s);
    Relations_ReadRelation((System_tFile)IO_StdInput, &r2);
    Relations_WriteRelation((System_tFile)IO_StdOutput, r2);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Reflexive	= ", 12L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsReflexive(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Symmetric	= ", 12L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsSymmetric(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Transitive	= ", 13L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsTransitive(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Equivalence	= ", 14L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsEquivalence(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"HasReflexive	= ", 15L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_HasReflexive(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Cyclic		= ", 10L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsCyclic(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Card		= ", 8L);
    IO_WriteI((System_tFile)IO_StdOutput, Relations_Card(&r2), 1L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    Relations_GetCyclics(r2, &s2);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Cyclics		= ", 11L);
    Sets_WriteSet((System_tFile)IO_StdOutput, s2);
    IO_WriteNl((System_tFile)IO_StdOutput);
    Relations_Closure(&r2);
    Relations_WriteRelation((System_tFile)IO_StdOutput, r2);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Reflexive	= ", 12L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsReflexive(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Symmetric	= ", 12L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsSymmetric(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Transitive	= ", 13L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsTransitive(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Equivalence	= ", 14L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsEquivalence(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"HasReflexive	= ", 15L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_HasReflexive(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Cyclic		= ", 10L);
    IO_WriteB((System_tFile)IO_StdOutput, Relations_IsCyclic(r2));
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Card		= ", 8L);
    IO_WriteI((System_tFile)IO_StdOutput, Relations_Card(&r2), 1L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    Relations_GetCyclics(r2, &s2);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Cyclics		= ", 11L);
    Sets_WriteSet((System_tFile)IO_StdOutput, s2);
    IO_WriteNl((System_tFile)IO_StdOutput);
    Relations_ReleaseRelation(&r2);
    Sets_ReleaseSet(&s2);
  } EXIT_1:;
  IO_CloseIO();
}
