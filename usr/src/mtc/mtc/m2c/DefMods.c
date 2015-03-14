#include "SYSTEM_.h"

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Base
#include "Base.h"
#endif

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

#ifndef DEFINITION_Parser
#include "Parser.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_AssocTab
#include "AssocTab.h"
#endif

#ifndef DEFINITION_DefMods
#include "DefMods.h"
#endif


#define eNotAnalyzed	0
#define eAnalyzed	1
#define eError	2
typedef unsigned char tMarkClass;
typedef tMarkClass *tMarkPtr;
static Tree_tTree Tree;
static tMarkPtr MakeMark ARGS((tMarkClass Class));
static void TraverseImports ARGS((Tree_tTree Module));
static void GetDefinitionModule ARGS((Idents_tIdent ModuleName, Positions_tPosition Pos));
struct S_1 {
    CHAR A[255 + 1];
};


static tMarkPtr MakeMark
# ifdef __STDC__
(tMarkClass Class)
# else
(Class)
tMarkClass Class;
# endif
{
  tMarkPtr PtrToMark;

  PtrToMark = (tMarkPtr)Memory_Alloc((LONGINT)sizeof(tMarkClass));
  *PtrToMark = Class;
  return PtrToMark;
}

void DefMods_GetDefinitionModules
# ifdef __STDC__
(Tree_tTree CompUnit, Tree_tTree *Root)
# else
(CompUnit, Root)
Tree_tTree CompUnit;
Tree_tTree *Root;
# endif
{
  Tree = Tree_mCompUnits0();
  AssocTab_BeginAssocTab();
  AssocTab_PutAssoc(Defs_IdentSYSTEM, (ADDRESS)MakeMark(eAnalyzed));
  if (CompUnit->U_1.V_1.Kind == Tree_ProgMod) {
    if (CompUnit->U_1.V_8.ProgMod.Kind == Tree_Implementation) {
      GetDefinitionModule(CompUnit->U_1.V_8.ProgMod.Ident, CompUnit->U_1.V_8.ProgMod.Pos);
    }
    TraverseImports(CompUnit);
  } else {
    AssocTab_PutAssoc(CompUnit->U_1.V_7.DefMod.Ident, (ADDRESS)MakeMark(eNotAnalyzed));
    TraverseImports(CompUnit);
  }
  AssocTab_CloseAssocTab();
  *Root = Tree_mROOT(Tree_ReverseTree(Tree));
}

static void TraverseImports
# ifdef __STDC__
(Tree_tTree Module)
# else
(Module)
Tree_tTree Module;
# endif
{
  Tree_tTree Import, Ids;
  tMarkPtr PtrToMark;

  switch (Module->U_1.V_1.Kind) {
  case Tree_DefMod:;
    Import = Module->U_1.V_7.DefMod.Import;
    break;
  case Tree_ProgMod:;
    Import = Module->U_1.V_8.ProgMod.Import;
    break;
  }
  while (Import->U_1.V_1.Kind != Tree_Import0) {
    if (Import->U_1.V_1.Kind == Tree_From) {
      AssocTab_GetAssoc(Import->U_1.V_12.From.Ident, (ADDRESS *)&PtrToMark);
      if (PtrToMark == NIL) {
        GetDefinitionModule(Import->U_1.V_12.From.Ident, Import->U_1.V_12.From.Pos);
      } else if (*PtrToMark == eNotAnalyzed) {
        Errors_ErrorMessageP((LONGCARD)Errors_CyclicDefMods, (LONGCARD)Errors_Fatal, Import->U_1.V_12.From.Pos);
      }
    } else {
      Ids = Import->U_1.V_13.Objects.ImpIds;
      while (Ids->U_1.V_1.Kind != Tree_ImpIds0) {
        AssocTab_GetAssoc(Ids->U_1.V_16.ImpIds1.Ident, (ADDRESS *)&PtrToMark);
        if (PtrToMark == NIL) {
          GetDefinitionModule(Ids->U_1.V_16.ImpIds1.Ident, Ids->U_1.V_16.ImpIds1.Pos);
        } else if (*PtrToMark == eNotAnalyzed) {
          Errors_ErrorMessageP((LONGCARD)Errors_CyclicDefMods, (LONGCARD)Errors_Fatal, Ids->U_1.V_16.ImpIds1.Pos);
        }
        Ids = Ids->U_1.V_16.ImpIds1.Next;
      }
    }
    Import = Import->U_1.V_11.Import1.Next;
  }
  if (Module->U_1.V_1.Kind != Tree_ProgMod) {
    AssocTab_GetAssoc(Module->U_1.V_7.DefMod.Ident, (ADDRESS *)&PtrToMark);
    *PtrToMark = eAnalyzed;
    AssocTab_PutAssoc(Module->U_1.V_7.DefMod.Ident, (ADDRESS)PtrToMark);
  }
  Module->U_1.V_6.CompUnit.Next = Tree;
  Tree = Module;
}

static void GetDefinitionModule
# ifdef __STDC__
(Idents_tIdent ModuleName, Positions_tPosition Pos)
# else
(ModuleName, Pos)
Idents_tIdent ModuleName;
Positions_tPosition Pos;
# endif
{
  BOOLEAN Success;
  struct S_1 FileName;
  CARDINAL ParseErrors;

  Base_CheckDefFile(ModuleName, FileName.A, 256L, &Success);
  if (!Success) {
    AssocTab_PutAssoc(ModuleName, (ADDRESS)MakeMark(eError));
    Errors_ErrorMessagePI((LONGCARD)Errors_ModNotFound, (LONGCARD)Errors_Error, Pos, (LONGCARD)Errors_Ident, ADR(ModuleName));
  } else {
    Scanner_BeginFile(FileName.A, 256L);
    ParseErrors = Parser_Parser();
    if (ParseErrors == 0) {
      if (Parser_ParsAttribute.U_1.V_0.Tree->U_1.V_1.Kind == Tree_ProgMod) {
        AssocTab_PutAssoc(ModuleName, (ADDRESS)MakeMark(eError));
        Errors_ErrorMessagePI((LONGCARD)Errors_ModNotFound, (LONGCARD)Errors_Error, Pos, (LONGCARD)Errors_Ident, ADR(ModuleName));
      } else {
        AssocTab_PutAssoc(ModuleName, (ADDRESS)MakeMark(eNotAnalyzed));
        TraverseImports(Parser_ParsAttribute.U_1.V_0.Tree);
      }
    } else {
      AssocTab_PutAssoc(ModuleName, (ADDRESS)MakeMark(eError));
    }
  }
}

void BEGIN_DefMods()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Tree();
    BEGIN_Memory();
    BEGIN_Idents();
    BEGIN_Base();
    BEGIN_Scanner();
    BEGIN_Parser();
    BEGIN_Errors();
    BEGIN_Positions();
    BEGIN_Tree();
    BEGIN_Defs();
    BEGIN_AssocTab();

  }
}
