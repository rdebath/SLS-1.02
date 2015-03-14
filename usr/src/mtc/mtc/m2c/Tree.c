#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Layout
#include "Layout.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Texts
#include "Texts.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

Tree_tTree Tree_TreeRoot;
LONGCARD Tree_HeapUsed;
ADDRESS Tree_yyPoolFreePtr, Tree_yyPoolMaxPtr;
struct Tree_128 Tree_yyNodeSize;
PROC Tree_yyExit;

#define yyBlockSize	20480
typedef struct S_1 *yytBlockPtr;
typedef struct S_1 {
    struct S_2 {
        CHAR A[yyBlockSize - 1 + 1];
    } yyBlock;
    yytBlockPtr yySuccessor;
} yytBlock;
static yytBlockPtr yyBlockList;
static SHORTCARD yyMaxSize, yyi;
static struct S_3 {
    SHORTCARD A[125 + 1];
} yyTypeRange;
typedef Tree_tTree *yyPtrtTree;
static IO_tFile yyf;
static SHORTCARD yyLabel;
static SHORTCARD yyKind;
static CHAR yyc;
static Strings_tString yys;
#define yyNil	((CHAR)'\374')
#define yyNoLabel	((CHAR)'\375')
#define yyLabelDef	((CHAR)'\376')
#define yyLabelUse	((CHAR)'\377')
static void xxExit ARGS(());


Tree_tTree Tree_yyAlloc
# ifdef __STDC__
()
# else
()
# endif
{
  yytBlockPtr yyBlockPtr;

  yyBlockPtr = yyBlockList;
  yyBlockList = (yytBlockPtr)Memory_Alloc((LONGINT)sizeof(yytBlock));
  yyBlockList->yySuccessor = yyBlockPtr;
  Tree_yyPoolFreePtr = ADR(yyBlockList->yyBlock);
  Tree_yyPoolMaxPtr = (ADDRESS)(Tree_yyPoolFreePtr + yyBlockSize - yyMaxSize + 1);
  INC1(Tree_HeapUsed, yyBlockSize);
  return (Tree_tTree)Tree_yyPoolFreePtr;
}

Tree_tTree Tree_MakeTree
# ifdef __STDC__
(SHORTCARD yyKind)
# else
(yyKind)
SHORTCARD yyKind;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[yyKind]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = yyKind;
  return yyt;
}

BOOLEAN Tree_IsType
# ifdef __STDC__
(Tree_tTree yyTree, SHORTCARD yyKind)
# else
(yyTree, yyKind)
Tree_tTree yyTree;
SHORTCARD yyKind;
# endif
{
  return yyTree != Tree_NoTree && yyKind <= yyTree->U_1.V_1.Kind && yyTree->U_1.V_1.Kind <= yyTypeRange.A[yyKind];
}

Tree_tTree Tree_mROOT
# ifdef __STDC__
(Tree_tTree pCompUnits)
# else
(pCompUnits)
Tree_tTree pCompUnits;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ROOT]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ROOT;
  {
    register Tree_yROOT *W_1 = &yyt->U_1.V_3.ROOT;

    W_1->CompUnits = pCompUnits;
  }
  return yyt;
}

Tree_tTree Tree_mCompUnits
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_CompUnits]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_CompUnits;
  {
    register Tree_yCompUnits *W_2 = &yyt->U_1.V_4.CompUnits;

  }
  return yyt;
}

Tree_tTree Tree_mCompUnits0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_CompUnits0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_CompUnits0;
  {
    register Tree_yCompUnits0 *W_3 = &yyt->U_1.V_5.CompUnits0;

  }
  return yyt;
}

Tree_tTree Tree_mCompUnit
# ifdef __STDC__
(SHORTCARD pKind, Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pNext)
# else
(pKind, pIdent, pPos, pNext)
SHORTCARD pKind;
Idents_tIdent pIdent;
Positions_tPosition pPos;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_CompUnit]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_CompUnit;
  {
    register Tree_yCompUnit *W_4 = &yyt->U_1.V_6.CompUnit;

    W_4->Kind = pKind;
    W_4->Ident = pIdent;
    W_4->Pos = pPos;
    W_4->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mDefMod
# ifdef __STDC__
(SHORTCARD pKind, Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pNext, Tree_tTree pImport, Tree_tTree pDecls)
# else
(pKind, pIdent, pPos, pNext, pImport, pDecls)
SHORTCARD pKind;
Idents_tIdent pIdent;
Positions_tPosition pPos;
Tree_tTree pNext;
Tree_tTree pImport;
Tree_tTree pDecls;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_DefMod]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_DefMod;
  {
    register Tree_yDefMod *W_5 = &yyt->U_1.V_7.DefMod;

    W_5->Kind = pKind;
    W_5->Ident = pIdent;
    W_5->Pos = pPos;
    W_5->Next = pNext;
    W_5->Import = pImport;
    W_5->Decls = pDecls;
  }
  return yyt;
}

Tree_tTree Tree_mProgMod
# ifdef __STDC__
(SHORTCARD pKind, Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pNext, Tree_tTree pImport, Tree_tTree pDecls, Tree_tTree pStmts)
# else
(pKind, pIdent, pPos, pNext, pImport, pDecls, pStmts)
SHORTCARD pKind;
Idents_tIdent pIdent;
Positions_tPosition pPos;
Tree_tTree pNext;
Tree_tTree pImport;
Tree_tTree pDecls;
Tree_tTree pStmts;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ProgMod]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ProgMod;
  {
    register Tree_yProgMod *W_6 = &yyt->U_1.V_8.ProgMod;

    W_6->Kind = pKind;
    W_6->Ident = pIdent;
    W_6->Pos = pPos;
    W_6->Next = pNext;
    W_6->Import = pImport;
    W_6->Decls = pDecls;
    W_6->Stmts = pStmts;
  }
  return yyt;
}

Tree_tTree Tree_mImport
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Import]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Import;
  {
    register Tree_yImport *W_7 = &yyt->U_1.V_9.Import;

  }
  return yyt;
}

Tree_tTree Tree_mImport0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Import0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Import0;
  {
    register Tree_yImport0 *W_8 = &yyt->U_1.V_10.Import0;

  }
  return yyt;
}

Tree_tTree Tree_mImport1
# ifdef __STDC__
(Tree_tTree pNext)
# else
(pNext)
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Import1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Import1;
  {
    register Tree_yImport1 *W_9 = &yyt->U_1.V_11.Import1;

    W_9->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mFrom
# ifdef __STDC__
(Tree_tTree pNext, Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pImpIds)
# else
(pNext, pIdent, pPos, pImpIds)
Tree_tTree pNext;
Idents_tIdent pIdent;
Positions_tPosition pPos;
Tree_tTree pImpIds;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_From]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_From;
  {
    register Tree_yFrom *W_10 = &yyt->U_1.V_12.From;

    W_10->Next = pNext;
    W_10->Ident = pIdent;
    W_10->Pos = pPos;
    W_10->ImpIds = pImpIds;
  }
  return yyt;
}

Tree_tTree Tree_mObjects
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pImpIds)
# else
(pNext, pImpIds)
Tree_tTree pNext;
Tree_tTree pImpIds;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Objects]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Objects;
  {
    register Tree_yObjects *W_11 = &yyt->U_1.V_13.Objects;

    W_11->Next = pNext;
    W_11->ImpIds = pImpIds;
  }
  return yyt;
}

Tree_tTree Tree_mImpIds
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ImpIds]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ImpIds;
  {
    register Tree_yImpIds *W_12 = &yyt->U_1.V_14.ImpIds;

  }
  return yyt;
}

Tree_tTree Tree_mImpIds0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ImpIds0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ImpIds0;
  {
    register Tree_yImpIds0 *W_13 = &yyt->U_1.V_15.ImpIds0;

  }
  return yyt;
}

Tree_tTree Tree_mImpIds1
# ifdef __STDC__
(Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pNext)
# else
(pIdent, pPos, pNext)
Idents_tIdent pIdent;
Positions_tPosition pPos;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ImpIds1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ImpIds1;
  {
    register Tree_yImpIds1 *W_14 = &yyt->U_1.V_16.ImpIds1;

    W_14->Ident = pIdent;
    W_14->Pos = pPos;
    W_14->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mExport
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Export]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Export;
  {
    register Tree_yExport *W_15 = &yyt->U_1.V_17.Export;

  }
  return yyt;
}

Tree_tTree Tree_mExport0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Export0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Export0;
  {
    register Tree_yExport0 *W_16 = &yyt->U_1.V_18.Export0;

  }
  return yyt;
}

Tree_tTree Tree_mExport1
# ifdef __STDC__
(BOOLEAN pQualified, Tree_tTree pExpIds)
# else
(pQualified, pExpIds)
BOOLEAN pQualified;
Tree_tTree pExpIds;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Export1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Export1;
  {
    register Tree_yExport1 *W_17 = &yyt->U_1.V_19.Export1;

    W_17->Qualified = pQualified;
    W_17->ExpIds = pExpIds;
  }
  return yyt;
}

Tree_tTree Tree_mExpIds
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ExpIds]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ExpIds;
  {
    register Tree_yExpIds *W_18 = &yyt->U_1.V_20.ExpIds;

  }
  return yyt;
}

Tree_tTree Tree_mExpIds0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ExpIds0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ExpIds0;
  {
    register Tree_yExpIds0 *W_19 = &yyt->U_1.V_21.ExpIds0;

  }
  return yyt;
}

Tree_tTree Tree_mExpIds1
# ifdef __STDC__
(Idents_tIdent pIdent, Tree_tTree pNext)
# else
(pIdent, pNext)
Idents_tIdent pIdent;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ExpIds1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ExpIds1;
  {
    register Tree_yExpIds1 *W_20 = &yyt->U_1.V_22.ExpIds1;

    W_20->Ident = pIdent;
    W_20->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mDecls
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Decls]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Decls;
  {
    register Tree_yDecls *W_21 = &yyt->U_1.V_23.Decls;

  }
  return yyt;
}

Tree_tTree Tree_mDecls0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Decls0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Decls0;
  {
    register Tree_yDecls0 *W_22 = &yyt->U_1.V_24.Decls0;

  }
  return yyt;
}

Tree_tTree Tree_mDecl
# ifdef __STDC__
(Tree_tTree pNext)
# else
(pNext)
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Decl]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Decl;
  {
    register Tree_yDecl *W_23 = &yyt->U_1.V_25.Decl;

    W_23->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mVar
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pVarIds, Tree_tTree pType)
# else
(pNext, pVarIds, pType)
Tree_tTree pNext;
Tree_tTree pVarIds;
Tree_tTree pType;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Var]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Var;
  {
    register Tree_yVar *W_24 = &yyt->U_1.V_26.Var;

    W_24->Next = pNext;
    W_24->VarIds = pVarIds;
    W_24->Type = pType;
  }
  return yyt;
}

Tree_tTree Tree_mObject
# ifdef __STDC__
(Tree_tTree pNext, Idents_tIdent pIdent)
# else
(pNext, pIdent)
Tree_tTree pNext;
Idents_tIdent pIdent;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Object]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Object;
  {
    register Tree_yObject *W_25 = &yyt->U_1.V_27.Object;

    W_25->Next = pNext;
    W_25->Ident = pIdent;
  }
  return yyt;
}

Tree_tTree Tree_mConst
# ifdef __STDC__
(Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pExpr)
# else
(pNext, pIdent, pExpr)
Tree_tTree pNext;
Idents_tIdent pIdent;
Tree_tTree pExpr;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Const]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Const;
  {
    register Tree_yConst *W_26 = &yyt->U_1.V_28.Const;

    W_26->Next = pNext;
    W_26->Ident = pIdent;
    W_26->Expr = pExpr;
  }
  return yyt;
}

Tree_tTree Tree_mTypeDecl
# ifdef __STDC__
(Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pType, Positions_tPosition pPos)
# else
(pNext, pIdent, pType, pPos)
Tree_tTree pNext;
Idents_tIdent pIdent;
Tree_tTree pType;
Positions_tPosition pPos;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_TypeDecl]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_TypeDecl;
  {
    register Tree_yTypeDecl *W_27 = &yyt->U_1.V_29.TypeDecl;

    W_27->Next = pNext;
    W_27->Ident = pIdent;
    W_27->Type = pType;
    W_27->Pos = pPos;
  }
  return yyt;
}

Tree_tTree Tree_mProc
# ifdef __STDC__
(Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pFormals, Tree_tTree pResultType, Tree_tTree pDecls, Tree_tTree pStmts)
# else
(pNext, pIdent, pFormals, pResultType, pDecls, pStmts)
Tree_tTree pNext;
Idents_tIdent pIdent;
Tree_tTree pFormals;
Tree_tTree pResultType;
Tree_tTree pDecls;
Tree_tTree pStmts;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Proc]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Proc;
  {
    register Tree_yProc *W_28 = &yyt->U_1.V_30.Proc;

    W_28->Next = pNext;
    W_28->Ident = pIdent;
    W_28->Formals = pFormals;
    W_28->ResultType = pResultType;
    W_28->Decls = pDecls;
    W_28->Stmts = pStmts;
  }
  return yyt;
}

Tree_tTree Tree_mProcHead
# ifdef __STDC__
(Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pFormals, Tree_tTree pResultType, Positions_tPosition pPos)
# else
(pNext, pIdent, pFormals, pResultType, pPos)
Tree_tTree pNext;
Idents_tIdent pIdent;
Tree_tTree pFormals;
Tree_tTree pResultType;
Positions_tPosition pPos;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ProcHead]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ProcHead;
  {
    register Tree_yProcHead *W_29 = &yyt->U_1.V_31.ProcHead;

    W_29->Next = pNext;
    W_29->Ident = pIdent;
    W_29->Formals = pFormals;
    W_29->ResultType = pResultType;
    W_29->Pos = pPos;
  }
  return yyt;
}

Tree_tTree Tree_mModule
# ifdef __STDC__
(Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pImport, Tree_tTree pExport, Tree_tTree pDecls, Tree_tTree pStmts)
# else
(pNext, pIdent, pImport, pExport, pDecls, pStmts)
Tree_tTree pNext;
Idents_tIdent pIdent;
Tree_tTree pImport;
Tree_tTree pExport;
Tree_tTree pDecls;
Tree_tTree pStmts;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Module]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Module;
  {
    register Tree_yModule *W_30 = &yyt->U_1.V_32.Module;

    W_30->Next = pNext;
    W_30->Ident = pIdent;
    W_30->Import = pImport;
    W_30->Export = pExport;
    W_30->Decls = pDecls;
    W_30->Stmts = pStmts;
  }
  return yyt;
}

Tree_tTree Tree_mOpaque
# ifdef __STDC__
(Tree_tTree pNext, Idents_tIdent pIdent)
# else
(pNext, pIdent)
Tree_tTree pNext;
Idents_tIdent pIdent;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Opaque]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Opaque;
  {
    register Tree_yOpaque *W_31 = &yyt->U_1.V_33.Opaque;

    W_31->Next = pNext;
    W_31->Ident = pIdent;
  }
  return yyt;
}

Tree_tTree Tree_mVarIds
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_VarIds]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_VarIds;
  {
    register Tree_yVarIds *W_32 = &yyt->U_1.V_34.VarIds;

  }
  return yyt;
}

Tree_tTree Tree_mVarIds0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_VarIds0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_VarIds0;
  {
    register Tree_yVarIds0 *W_33 = &yyt->U_1.V_35.VarIds0;

  }
  return yyt;
}

Tree_tTree Tree_mVarIds1
# ifdef __STDC__
(Idents_tIdent pIdent, Tree_tTree pNext)
# else
(pIdent, pNext)
Idents_tIdent pIdent;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_VarIds1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_VarIds1;
  {
    register Tree_yVarIds1 *W_34 = &yyt->U_1.V_36.VarIds1;

    W_34->Ident = pIdent;
    W_34->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mFormals
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Formals]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Formals;
  {
    register Tree_yFormals *W_35 = &yyt->U_1.V_37.Formals;

  }
  return yyt;
}

Tree_tTree Tree_mFormals0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Formals0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Formals0;
  {
    register Tree_yFormals0 *W_36 = &yyt->U_1.V_38.Formals0;

  }
  return yyt;
}

Tree_tTree Tree_mFormals1
# ifdef __STDC__
(BOOLEAN pIsVAR, Tree_tTree pParIds, Tree_tTree pType, Tree_tTree pNext)
# else
(pIsVAR, pParIds, pType, pNext)
BOOLEAN pIsVAR;
Tree_tTree pParIds;
Tree_tTree pType;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Formals1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Formals1;
  {
    register Tree_yFormals1 *W_37 = &yyt->U_1.V_39.Formals1;

    W_37->IsVAR = pIsVAR;
    W_37->ParIds = pParIds;
    W_37->Type = pType;
    W_37->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mParIds
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ParIds]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ParIds;
  {
    register Tree_yParIds *W_38 = &yyt->U_1.V_40.ParIds;

  }
  return yyt;
}

Tree_tTree Tree_mParIds0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ParIds0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ParIds0;
  {
    register Tree_yParIds0 *W_39 = &yyt->U_1.V_41.ParIds0;

  }
  return yyt;
}

Tree_tTree Tree_mParIds1
# ifdef __STDC__
(Idents_tIdent pIdent, Tree_tTree pNext)
# else
(pIdent, pNext)
Idents_tIdent pIdent;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ParIds1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ParIds1;
  {
    register Tree_yParIds1 *W_40 = &yyt->U_1.V_42.ParIds1;

    W_40->Ident = pIdent;
    W_40->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mType
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Type]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Type;
  {
    register Tree_yType *W_41 = &yyt->U_1.V_43.Type;

  }
  return yyt;
}

Tree_tTree Tree_mArray
# ifdef __STDC__
(BOOLEAN pIsOpen, Tree_tTree pIndexType, Tree_tTree pElemType)
# else
(pIsOpen, pIndexType, pElemType)
BOOLEAN pIsOpen;
Tree_tTree pIndexType;
Tree_tTree pElemType;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Array]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Array;
  {
    register Tree_yArray *W_42 = &yyt->U_1.V_44.Array;

    W_42->IsOpen = pIsOpen;
    W_42->IndexType = pIndexType;
    W_42->ElemType = pElemType;
  }
  return yyt;
}

Tree_tTree Tree_mRecord
# ifdef __STDC__
(Tree_tTree pFields)
# else
(pFields)
Tree_tTree pFields;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Record]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Record;
  {
    register Tree_yRecord *W_43 = &yyt->U_1.V_45.Record;

    W_43->Fields = pFields;
  }
  return yyt;
}

Tree_tTree Tree_mSetType
# ifdef __STDC__
(Tree_tTree pBaseType)
# else
(pBaseType)
Tree_tTree pBaseType;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_SetType]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_SetType;
  {
    register Tree_ySetType *W_44 = &yyt->U_1.V_46.SetType;

    W_44->BaseType = pBaseType;
  }
  return yyt;
}

Tree_tTree Tree_mPointer
# ifdef __STDC__
(Tree_tTree pTargetType)
# else
(pTargetType)
Tree_tTree pTargetType;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Pointer]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Pointer;
  {
    register Tree_yPointer *W_45 = &yyt->U_1.V_47.Pointer;

    W_45->TargetType = pTargetType;
  }
  return yyt;
}

Tree_tTree Tree_mProcType
# ifdef __STDC__
(Tree_tTree pFormalTypes, Tree_tTree pResultType)
# else
(pFormalTypes, pResultType)
Tree_tTree pFormalTypes;
Tree_tTree pResultType;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ProcType]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ProcType;
  {
    register Tree_yProcType *W_46 = &yyt->U_1.V_48.ProcType;

    W_46->FormalTypes = pFormalTypes;
    W_46->ResultType = pResultType;
  }
  return yyt;
}

Tree_tTree Tree_mSimpleType
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_SimpleType]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_SimpleType;
  {
    register Tree_ySimpleType *W_47 = &yyt->U_1.V_49.SimpleType;

  }
  return yyt;
}

Tree_tTree Tree_mEnumeration
# ifdef __STDC__
(Tree_tTree pEnumIds)
# else
(pEnumIds)
Tree_tTree pEnumIds;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Enumeration]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Enumeration;
  {
    register Tree_yEnumeration *W_48 = &yyt->U_1.V_50.Enumeration;

    W_48->EnumIds = pEnumIds;
  }
  return yyt;
}

Tree_tTree Tree_mSubrange
# ifdef __STDC__
(Tree_tTree pBaseType, Tree_tTree pLwb, Tree_tTree pUpb)
# else
(pBaseType, pLwb, pUpb)
Tree_tTree pBaseType;
Tree_tTree pLwb;
Tree_tTree pUpb;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Subrange]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Subrange;
  {
    register Tree_ySubrange *W_49 = &yyt->U_1.V_51.Subrange;

    W_49->BaseType = pBaseType;
    W_49->Lwb = pLwb;
    W_49->Upb = pUpb;
  }
  return yyt;
}

Tree_tTree Tree_mPrimaryType
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_PrimaryType]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_PrimaryType;
  {
    register Tree_yPrimaryType *W_50 = &yyt->U_1.V_52.PrimaryType;

  }
  return yyt;
}

Tree_tTree Tree_mVoid
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Void]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Void;
  {
    register Tree_yVoid *W_51 = &yyt->U_1.V_53.Void;

  }
  return yyt;
}

Tree_tTree Tree_mTypeId
# ifdef __STDC__
(Idents_tIdent pIdent, Positions_tPosition pPos)
# else
(pIdent, pPos)
Idents_tIdent pIdent;
Positions_tPosition pPos;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_TypeId]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_TypeId;
  {
    register Tree_yTypeId *W_52 = &yyt->U_1.V_54.TypeId;

    W_52->Ident = pIdent;
    W_52->Pos = pPos;
  }
  return yyt;
}

Tree_tTree Tree_mTypeId0
# ifdef __STDC__
(Idents_tIdent pIdent, Positions_tPosition pPos)
# else
(pIdent, pPos)
Idents_tIdent pIdent;
Positions_tPosition pPos;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_TypeId0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_TypeId0;
  {
    register Tree_yTypeId0 *W_53 = &yyt->U_1.V_55.TypeId0;

    W_53->Ident = pIdent;
    W_53->Pos = pPos;
  }
  return yyt;
}

Tree_tTree Tree_mTypeId1
# ifdef __STDC__
(Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pTypeId)
# else
(pIdent, pPos, pTypeId)
Idents_tIdent pIdent;
Positions_tPosition pPos;
Tree_tTree pTypeId;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_TypeId1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_TypeId1;
  {
    register Tree_yTypeId1 *W_54 = &yyt->U_1.V_56.TypeId1;

    W_54->Ident = pIdent;
    W_54->Pos = pPos;
    W_54->TypeId = pTypeId;
  }
  return yyt;
}

Tree_tTree Tree_mFields
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Fields]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Fields;
  {
    register Tree_yFields *W_55 = &yyt->U_1.V_57.Fields;

  }
  return yyt;
}

Tree_tTree Tree_mFields0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Fields0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Fields0;
  {
    register Tree_yFields0 *W_56 = &yyt->U_1.V_58.Fields0;

  }
  return yyt;
}

Tree_tTree Tree_mFields1
# ifdef __STDC__
(Tree_tTree pNext)
# else
(pNext)
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Fields1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Fields1;
  {
    register Tree_yFields1 *W_57 = &yyt->U_1.V_59.Fields1;

    W_57->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mRecordSect
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pFieldIds, Tree_tTree pType)
# else
(pNext, pFieldIds, pType)
Tree_tTree pNext;
Tree_tTree pFieldIds;
Tree_tTree pType;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_RecordSect]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_RecordSect;
  {
    register Tree_yRecordSect *W_58 = &yyt->U_1.V_60.RecordSect;

    W_58->Next = pNext;
    W_58->FieldIds = pFieldIds;
    W_58->Type = pType;
  }
  return yyt;
}

Tree_tTree Tree_mVariantSect
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pTagField, Tree_tTree pVariants, Tree_tTree pElse)
# else
(pNext, pTagField, pVariants, pElse)
Tree_tTree pNext;
Tree_tTree pTagField;
Tree_tTree pVariants;
Tree_tTree pElse;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_VariantSect]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_VariantSect;
  {
    register Tree_yVariantSect *W_59 = &yyt->U_1.V_61.VariantSect;

    W_59->Next = pNext;
    W_59->TagField = pTagField;
    W_59->Variants = pVariants;
    W_59->Else = pElse;
  }
  return yyt;
}

Tree_tTree Tree_mFieldIds
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_FieldIds]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_FieldIds;
  {
    register Tree_yFieldIds *W_60 = &yyt->U_1.V_62.FieldIds;

  }
  return yyt;
}

Tree_tTree Tree_mFieldIds0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_FieldIds0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_FieldIds0;
  {
    register Tree_yFieldIds0 *W_61 = &yyt->U_1.V_63.FieldIds0;

  }
  return yyt;
}

Tree_tTree Tree_mFieldIds1
# ifdef __STDC__
(Idents_tIdent pIdent, Tree_tTree pNext)
# else
(pIdent, pNext)
Idents_tIdent pIdent;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_FieldIds1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_FieldIds1;
  {
    register Tree_yFieldIds1 *W_62 = &yyt->U_1.V_64.FieldIds1;

    W_62->Ident = pIdent;
    W_62->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mTagField
# ifdef __STDC__
(Tree_tTree pType)
# else
(pType)
Tree_tTree pType;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_TagField]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_TagField;
  {
    register Tree_yTagField *W_63 = &yyt->U_1.V_65.TagField;

    W_63->Type = pType;
  }
  return yyt;
}

Tree_tTree Tree_mTagField0
# ifdef __STDC__
(Tree_tTree pType)
# else
(pType)
Tree_tTree pType;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_TagField0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_TagField0;
  {
    register Tree_yTagField0 *W_64 = &yyt->U_1.V_66.TagField0;

    W_64->Type = pType;
  }
  return yyt;
}

Tree_tTree Tree_mTagField1
# ifdef __STDC__
(Tree_tTree pType, Idents_tIdent pIdent)
# else
(pType, pIdent)
Tree_tTree pType;
Idents_tIdent pIdent;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_TagField1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_TagField1;
  {
    register Tree_yTagField1 *W_65 = &yyt->U_1.V_67.TagField1;

    W_65->Type = pType;
    W_65->Ident = pIdent;
  }
  return yyt;
}

Tree_tTree Tree_mVariants
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Variants]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Variants;
  {
    register Tree_yVariants *W_66 = &yyt->U_1.V_68.Variants;

  }
  return yyt;
}

Tree_tTree Tree_mVariants0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Variants0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Variants0;
  {
    register Tree_yVariants0 *W_67 = &yyt->U_1.V_69.Variants0;

  }
  return yyt;
}

Tree_tTree Tree_mVariant
# ifdef __STDC__
(Tree_tTree pLabels, Tree_tTree pVariant, Tree_tTree pNext)
# else
(pLabels, pVariant, pNext)
Tree_tTree pLabels;
Tree_tTree pVariant;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Variant]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Variant;
  {
    register Tree_yVariant *W_68 = &yyt->U_1.V_70.Variant;

    W_68->Labels = pLabels;
    W_68->Variant = pVariant;
    W_68->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mFormalTypes
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_FormalTypes]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_FormalTypes;
  {
    register Tree_yFormalTypes *W_69 = &yyt->U_1.V_71.FormalTypes;

  }
  return yyt;
}

Tree_tTree Tree_mFormalTypes0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_FormalTypes0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_FormalTypes0;
  {
    register Tree_yFormalTypes0 *W_70 = &yyt->U_1.V_72.FormalTypes0;

  }
  return yyt;
}

Tree_tTree Tree_mFormalType
# ifdef __STDC__
(BOOLEAN pIsVAR, Tree_tTree pType, Tree_tTree pNext)
# else
(pIsVAR, pType, pNext)
BOOLEAN pIsVAR;
Tree_tTree pType;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_FormalType]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_FormalType;
  {
    register Tree_yFormalType *W_71 = &yyt->U_1.V_73.FormalType;

    W_71->IsVAR = pIsVAR;
    W_71->Type = pType;
    W_71->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mEnumIds
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_EnumIds]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_EnumIds;
  {
    register Tree_yEnumIds *W_72 = &yyt->U_1.V_74.EnumIds;

  }
  return yyt;
}

Tree_tTree Tree_mEnumIds0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_EnumIds0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_EnumIds0;
  {
    register Tree_yEnumIds0 *W_73 = &yyt->U_1.V_75.EnumIds0;

  }
  return yyt;
}

Tree_tTree Tree_mEnumIds1
# ifdef __STDC__
(Idents_tIdent pIdent, Tree_tTree pNext)
# else
(pIdent, pNext)
Idents_tIdent pIdent;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_EnumIds1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_EnumIds1;
  {
    register Tree_yEnumIds1 *W_74 = &yyt->U_1.V_76.EnumIds1;

    W_74->Ident = pIdent;
    W_74->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mExpr
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Expr]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Expr;
  {
    register Tree_yExpr *W_75 = &yyt->U_1.V_77.Expr;

  }
  return yyt;
}

Tree_tTree Tree_mBinary
# ifdef __STDC__
(SHORTCARD pOperator, Tree_tTree pLop, Tree_tTree pRop)
# else
(pOperator, pLop, pRop)
SHORTCARD pOperator;
Tree_tTree pLop;
Tree_tTree pRop;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Binary]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Binary;
  {
    register Tree_yBinary *W_76 = &yyt->U_1.V_78.Binary;

    W_76->Operator = pOperator;
    W_76->Lop = pLop;
    W_76->Rop = pRop;
  }
  return yyt;
}

Tree_tTree Tree_mUnary
# ifdef __STDC__
(SHORTCARD pOperator, Tree_tTree pMop)
# else
(pOperator, pMop)
SHORTCARD pOperator;
Tree_tTree pMop;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Unary]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Unary;
  {
    register Tree_yUnary *W_77 = &yyt->U_1.V_79.Unary;

    W_77->Operator = pOperator;
    W_77->Mop = pMop;
  }
  return yyt;
}

Tree_tTree Tree_mIntConst
# ifdef __STDC__
(SHORTCARD pKind, CARDINAL pIntVal, Positions_tPosition pPos)
# else
(pKind, pIntVal, pPos)
SHORTCARD pKind;
CARDINAL pIntVal;
Positions_tPosition pPos;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_IntConst]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_IntConst;
  {
    register Tree_yIntConst *W_78 = &yyt->U_1.V_80.IntConst;

    W_78->Kind = pKind;
    W_78->IntVal = pIntVal;
    W_78->Pos = pPos;
  }
  return yyt;
}

Tree_tTree Tree_mRealConst
# ifdef __STDC__
(StringMem_tStringRef pRealVal)
# else
(pRealVal)
StringMem_tStringRef pRealVal;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_RealConst]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_RealConst;
  {
    register Tree_yRealConst *W_79 = &yyt->U_1.V_81.RealConst;

    W_79->RealVal = pRealVal;
  }
  return yyt;
}

Tree_tTree Tree_mStringConst
# ifdef __STDC__
(StringMem_tStringRef pStringVal)
# else
(pStringVal)
StringMem_tStringRef pStringVal;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_StringConst]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_StringConst;
  {
    register Tree_yStringConst *W_80 = &yyt->U_1.V_82.StringConst;

    W_80->StringVal = pStringVal;
  }
  return yyt;
}

Tree_tTree Tree_mCharConst
# ifdef __STDC__
(CHAR pCharVal)
# else
(pCharVal)
CHAR pCharVal;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_CharConst]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_CharConst;
  {
    register Tree_yCharConst *W_81 = &yyt->U_1.V_83.CharConst;

    W_81->CharVal = pCharVal;
  }
  return yyt;
}

Tree_tTree Tree_mFuncCall
# ifdef __STDC__
(Tree_tTree pDesignator, Tree_tTree pActuals)
# else
(pDesignator, pActuals)
Tree_tTree pDesignator;
Tree_tTree pActuals;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_FuncCall]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_FuncCall;
  {
    register Tree_yFuncCall *W_82 = &yyt->U_1.V_84.FuncCall;

    W_82->Designator = pDesignator;
    W_82->Actuals = pActuals;
  }
  return yyt;
}

Tree_tTree Tree_mSet
# ifdef __STDC__
(Tree_tTree pBaseType, Tree_tTree pElems)
# else
(pBaseType, pElems)
Tree_tTree pBaseType;
Tree_tTree pElems;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Set]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Set;
  {
    register Tree_ySet *W_83 = &yyt->U_1.V_85.Set;

    W_83->BaseType = pBaseType;
    W_83->Elems = pElems;
  }
  return yyt;
}

Tree_tTree Tree_mBitSet
# ifdef __STDC__
(Tree_tTree pElems)
# else
(pElems)
Tree_tTree pElems;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_BitSet]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_BitSet;
  {
    register Tree_yBitSet *W_84 = &yyt->U_1.V_86.BitSet;

    W_84->Elems = pElems;
  }
  return yyt;
}

Tree_tTree Tree_mDesignator
# ifdef __STDC__
(Positions_tPosition pPos)
# else
(pPos)
Positions_tPosition pPos;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Designator]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Designator;
  {
    register Tree_yDesignator *W_85 = &yyt->U_1.V_87.Designator;

    W_85->Pos = pPos;
  }
  return yyt;
}

Tree_tTree Tree_mQualid
# ifdef __STDC__
(Positions_tPosition pPos, Idents_tIdent pIdent)
# else
(pPos, pIdent)
Positions_tPosition pPos;
Idents_tIdent pIdent;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Qualid]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Qualid;
  {
    register Tree_yQualid *W_86 = &yyt->U_1.V_88.Qualid;

    W_86->Pos = pPos;
    W_86->Ident = pIdent;
  }
  return yyt;
}

Tree_tTree Tree_mQualid0
# ifdef __STDC__
(Positions_tPosition pPos, Idents_tIdent pIdent)
# else
(pPos, pIdent)
Positions_tPosition pPos;
Idents_tIdent pIdent;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Qualid0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Qualid0;
  {
    register Tree_yQualid0 *W_87 = &yyt->U_1.V_89.Qualid0;

    W_87->Pos = pPos;
    W_87->Ident = pIdent;
  }
  return yyt;
}

Tree_tTree Tree_mQualid1
# ifdef __STDC__
(Positions_tPosition pPos, Idents_tIdent pIdent, Tree_tTree pQualid)
# else
(pPos, pIdent, pQualid)
Positions_tPosition pPos;
Idents_tIdent pIdent;
Tree_tTree pQualid;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Qualid1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Qualid1;
  {
    register Tree_yQualid1 *W_88 = &yyt->U_1.V_90.Qualid1;

    W_88->Pos = pPos;
    W_88->Ident = pIdent;
    W_88->Qualid = pQualid;
  }
  return yyt;
}

Tree_tTree Tree_mSubscript
# ifdef __STDC__
(Positions_tPosition pPos, Tree_tTree pDesignator, Tree_tTree pIndex)
# else
(pPos, pDesignator, pIndex)
Positions_tPosition pPos;
Tree_tTree pDesignator;
Tree_tTree pIndex;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Subscript]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Subscript;
  {
    register Tree_ySubscript *W_89 = &yyt->U_1.V_91.Subscript;

    W_89->Pos = pPos;
    W_89->Designator = pDesignator;
    W_89->Index = pIndex;
  }
  return yyt;
}

Tree_tTree Tree_mDeref
# ifdef __STDC__
(Positions_tPosition pPos, Tree_tTree pDesignator)
# else
(pPos, pDesignator)
Positions_tPosition pPos;
Tree_tTree pDesignator;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Deref]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Deref;
  {
    register Tree_yDeref *W_90 = &yyt->U_1.V_92.Deref;

    W_90->Pos = pPos;
    W_90->Designator = pDesignator;
  }
  return yyt;
}

Tree_tTree Tree_mSelect
# ifdef __STDC__
(Positions_tPosition pPos, Tree_tTree pDesignator, Idents_tIdent pField)
# else
(pPos, pDesignator, pField)
Positions_tPosition pPos;
Tree_tTree pDesignator;
Idents_tIdent pField;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Select]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Select;
  {
    register Tree_ySelect *W_91 = &yyt->U_1.V_93.Select;

    W_91->Pos = pPos;
    W_91->Designator = pDesignator;
    W_91->Field = pField;
  }
  return yyt;
}

Tree_tTree Tree_mElems
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Elems]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Elems;
  {
    register Tree_yElems *W_92 = &yyt->U_1.V_94.Elems;

  }
  return yyt;
}

Tree_tTree Tree_mElems0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Elems0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Elems0;
  {
    register Tree_yElems0 *W_93 = &yyt->U_1.V_95.Elems0;

  }
  return yyt;
}

Tree_tTree Tree_mElems1
# ifdef __STDC__
(Tree_tTree pNext)
# else
(pNext)
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Elems1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Elems1;
  {
    register Tree_yElems1 *W_94 = &yyt->U_1.V_96.Elems1;

    W_94->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mElem
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pElem)
# else
(pNext, pElem)
Tree_tTree pNext;
Tree_tTree pElem;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Elem]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Elem;
  {
    register Tree_yElem *W_95 = &yyt->U_1.V_97.Elem;

    W_95->Next = pNext;
    W_95->Elem = pElem;
  }
  return yyt;
}

Tree_tTree Tree_mElemRange
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pLwb, Tree_tTree pUpb)
# else
(pNext, pLwb, pUpb)
Tree_tTree pNext;
Tree_tTree pLwb;
Tree_tTree pUpb;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_ElemRange]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_ElemRange;
  {
    register Tree_yElemRange *W_96 = &yyt->U_1.V_98.ElemRange;

    W_96->Next = pNext;
    W_96->Lwb = pLwb;
    W_96->Upb = pUpb;
  }
  return yyt;
}

Tree_tTree Tree_mActuals
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Actuals]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Actuals;
  {
    register Tree_yActuals *W_97 = &yyt->U_1.V_99.Actuals;

  }
  return yyt;
}

Tree_tTree Tree_mActuals0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Actuals0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Actuals0;
  {
    register Tree_yActuals0 *W_98 = &yyt->U_1.V_100.Actuals0;

  }
  return yyt;
}

Tree_tTree Tree_mActual
# ifdef __STDC__
(Tree_tTree pExpr, Tree_tTree pNext)
# else
(pExpr, pNext)
Tree_tTree pExpr;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Actual]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Actual;
  {
    register Tree_yActual *W_99 = &yyt->U_1.V_101.Actual;

    W_99->Expr = pExpr;
    W_99->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mStmts
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Stmts]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Stmts;
  {
    register Tree_yStmts *W_100 = &yyt->U_1.V_102.Stmts;

  }
  return yyt;
}

Tree_tTree Tree_mStmts0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Stmts0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Stmts0;
  {
    register Tree_yStmts0 *W_101 = &yyt->U_1.V_103.Stmts0;

  }
  return yyt;
}

Tree_tTree Tree_mStmt
# ifdef __STDC__
(Tree_tTree pNext)
# else
(pNext)
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Stmt]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Stmt;
  {
    register Tree_yStmt *W_102 = &yyt->U_1.V_104.Stmt;

    W_102->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mAssign
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pDesignator, Tree_tTree pExpr)
# else
(pNext, pDesignator, pExpr)
Tree_tTree pNext;
Tree_tTree pDesignator;
Tree_tTree pExpr;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Assign]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Assign;
  {
    register Tree_yAssign *W_103 = &yyt->U_1.V_105.Assign;

    W_103->Next = pNext;
    W_103->Designator = pDesignator;
    W_103->Expr = pExpr;
  }
  return yyt;
}

Tree_tTree Tree_mCall
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pDesignator, Tree_tTree pActuals)
# else
(pNext, pDesignator, pActuals)
Tree_tTree pNext;
Tree_tTree pDesignator;
Tree_tTree pActuals;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Call]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Call;
  {
    register Tree_yCall *W_104 = &yyt->U_1.V_106.Call;

    W_104->Next = pNext;
    W_104->Designator = pDesignator;
    W_104->Actuals = pActuals;
  }
  return yyt;
}

Tree_tTree Tree_mIf
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pCond, Tree_tTree pThen, Tree_tTree pElsifs, Tree_tTree pElse)
# else
(pNext, pCond, pThen, pElsifs, pElse)
Tree_tTree pNext;
Tree_tTree pCond;
Tree_tTree pThen;
Tree_tTree pElsifs;
Tree_tTree pElse;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_If]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_If;
  {
    register Tree_yIf *W_105 = &yyt->U_1.V_107.If;

    W_105->Next = pNext;
    W_105->Cond = pCond;
    W_105->Then = pThen;
    W_105->Elsifs = pElsifs;
    W_105->Else = pElse;
  }
  return yyt;
}

Tree_tTree Tree_mCase
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pExpr, Tree_tTree pCases, Tree_tTree pElse, BOOLEAN pDefault)
# else
(pNext, pExpr, pCases, pElse, pDefault)
Tree_tTree pNext;
Tree_tTree pExpr;
Tree_tTree pCases;
Tree_tTree pElse;
BOOLEAN pDefault;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Case]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Case;
  {
    register Tree_yCase *W_106 = &yyt->U_1.V_108.Case;

    W_106->Next = pNext;
    W_106->Expr = pExpr;
    W_106->Cases = pCases;
    W_106->Else = pElse;
    W_106->Default = pDefault;
  }
  return yyt;
}

Tree_tTree Tree_mWhile
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pCond, Tree_tTree pStmts)
# else
(pNext, pCond, pStmts)
Tree_tTree pNext;
Tree_tTree pCond;
Tree_tTree pStmts;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_While]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_While;
  {
    register Tree_yWhile *W_107 = &yyt->U_1.V_109.While;

    W_107->Next = pNext;
    W_107->Cond = pCond;
    W_107->Stmts = pStmts;
  }
  return yyt;
}

Tree_tTree Tree_mRepeat
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pStmts, Tree_tTree pCond)
# else
(pNext, pStmts, pCond)
Tree_tTree pNext;
Tree_tTree pStmts;
Tree_tTree pCond;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Repeat]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Repeat;
  {
    register Tree_yRepeat *W_108 = &yyt->U_1.V_110.Repeat;

    W_108->Next = pNext;
    W_108->Stmts = pStmts;
    W_108->Cond = pCond;
  }
  return yyt;
}

Tree_tTree Tree_mLoop
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pStmts)
# else
(pNext, pStmts)
Tree_tTree pNext;
Tree_tTree pStmts;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Loop]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Loop;
  {
    register Tree_yLoop *W_109 = &yyt->U_1.V_111.Loop;

    W_109->Next = pNext;
    W_109->Stmts = pStmts;
  }
  return yyt;
}

Tree_tTree Tree_mFor
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pQualid, Tree_tTree pFrom, Tree_tTree pTo, Tree_tTree pBy, Tree_tTree pStmts)
# else
(pNext, pQualid, pFrom, pTo, pBy, pStmts)
Tree_tTree pNext;
Tree_tTree pQualid;
Tree_tTree pFrom;
Tree_tTree pTo;
Tree_tTree pBy;
Tree_tTree pStmts;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_For]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_For;
  {
    register Tree_yFor *W_110 = &yyt->U_1.V_112.For;

    W_110->Next = pNext;
    W_110->Qualid = pQualid;
    W_110->From = pFrom;
    W_110->To = pTo;
    W_110->By = pBy;
    W_110->Stmts = pStmts;
  }
  return yyt;
}

Tree_tTree Tree_mWith
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pDesignator, Tree_tTree pStmts)
# else
(pNext, pDesignator, pStmts)
Tree_tTree pNext;
Tree_tTree pDesignator;
Tree_tTree pStmts;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_With]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_With;
  {
    register Tree_yWith *W_111 = &yyt->U_1.V_113.With;

    W_111->Next = pNext;
    W_111->Designator = pDesignator;
    W_111->Stmts = pStmts;
  }
  return yyt;
}

Tree_tTree Tree_mExit
# ifdef __STDC__
(Tree_tTree pNext)
# else
(pNext)
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Exit]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Exit;
  {
    register Tree_yExit *W_112 = &yyt->U_1.V_114.Exit;

    W_112->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mReturn1
# ifdef __STDC__
(Tree_tTree pNext)
# else
(pNext)
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Return1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Return1;
  {
    register Tree_yReturn1 *W_113 = &yyt->U_1.V_115.Return1;

    W_113->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mReturn2
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pResult)
# else
(pNext, pResult)
Tree_tTree pNext;
Tree_tTree pResult;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Return2]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Return2;
  {
    register Tree_yReturn2 *W_114 = &yyt->U_1.V_116.Return2;

    W_114->Next = pNext;
    W_114->Result = pResult;
  }
  return yyt;
}

Tree_tTree Tree_mElsifs
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Elsifs]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Elsifs;
  {
    register Tree_yElsifs *W_115 = &yyt->U_1.V_117.Elsifs;

  }
  return yyt;
}

Tree_tTree Tree_mElsifs0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Elsifs0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Elsifs0;
  {
    register Tree_yElsifs0 *W_116 = &yyt->U_1.V_118.Elsifs0;

  }
  return yyt;
}

Tree_tTree Tree_mElsifs1
# ifdef __STDC__
(Tree_tTree pCond, Tree_tTree pStmts, Tree_tTree pNext)
# else
(pCond, pStmts, pNext)
Tree_tTree pCond;
Tree_tTree pStmts;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Elsifs1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Elsifs1;
  {
    register Tree_yElsifs1 *W_117 = &yyt->U_1.V_119.Elsifs1;

    W_117->Cond = pCond;
    W_117->Stmts = pStmts;
    W_117->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mCases
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Cases]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Cases;
  {
    register Tree_yCases *W_118 = &yyt->U_1.V_120.Cases;

  }
  return yyt;
}

Tree_tTree Tree_mCases0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Cases0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Cases0;
  {
    register Tree_yCases0 *W_119 = &yyt->U_1.V_121.Cases0;

  }
  return yyt;
}

Tree_tTree Tree_mCases1
# ifdef __STDC__
(Tree_tTree pLabels, Tree_tTree pStmts, Tree_tTree pNext)
# else
(pLabels, pStmts, pNext)
Tree_tTree pLabels;
Tree_tTree pStmts;
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Cases1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Cases1;
  {
    register Tree_yCases1 *W_120 = &yyt->U_1.V_122.Cases1;

    W_120->Labels = pLabels;
    W_120->Stmts = pStmts;
    W_120->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mLabels
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Labels]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Labels;
  {
    register Tree_yLabels *W_121 = &yyt->U_1.V_123.Labels;

  }
  return yyt;
}

Tree_tTree Tree_mLabels0
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Labels0]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Labels0;
  {
    register Tree_yLabels0 *W_122 = &yyt->U_1.V_124.Labels0;

  }
  return yyt;
}

Tree_tTree Tree_mLabels1
# ifdef __STDC__
(Tree_tTree pNext)
# else
(pNext)
Tree_tTree pNext;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Labels1]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Labels1;
  {
    register Tree_yLabels1 *W_123 = &yyt->U_1.V_125.Labels1;

    W_123->Next = pNext;
  }
  return yyt;
}

Tree_tTree Tree_mLabel
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pLabel)
# else
(pNext, pLabel)
Tree_tTree pNext;
Tree_tTree pLabel;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_Label]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_Label;
  {
    register Tree_yLabel *W_124 = &yyt->U_1.V_126.Label;

    W_124->Next = pNext;
    W_124->Label = pLabel;
  }
  return yyt;
}

Tree_tTree Tree_mLabelRange
# ifdef __STDC__
(Tree_tTree pNext, Tree_tTree pLwb, Tree_tTree pUpb)
# else
(pNext, pLwb, pUpb)
Tree_tTree pNext;
Tree_tTree pLwb;
Tree_tTree pUpb;
# endif
{
  LONGINT yyByteCount;
  Tree_tTree yyt;

  yyt = (Tree_tTree)Tree_yyPoolFreePtr;
  if ((ADDRESS)yyt >= Tree_yyPoolMaxPtr) {
    yyt = Tree_yyAlloc();
  }
  INC1(Tree_yyPoolFreePtr, Tree_yyNodeSize.A[Tree_LabelRange]);
  yyt->U_1.V_2.yyHead.yyMark = 0;
  yyt->U_1.V_1.Kind = Tree_LabelRange;
  {
    register Tree_yLabelRange *W_125 = &yyt->U_1.V_127.LabelRange;

    W_125->Next = pNext;
    W_125->Lwb = pLwb;
    W_125->Upb = pUpb;
  }
  return yyt;
}

Tree_tTree Tree_ReverseTree
# ifdef __STDC__
(Tree_tTree yyOld)
# else
(yyOld)
Tree_tTree yyOld;
# endif
{
  Tree_tTree yyNew, yyNext, yyTail;

  yyNew = yyOld;
  yyTail = yyOld;
  for (;;) {
    switch (yyOld->U_1.V_1.Kind) {
    case Tree_CompUnit:;
      yyNext = yyOld->U_1.V_6.CompUnit.Next;
      yyOld->U_1.V_6.CompUnit.Next = yyNew;
      break;
    case Tree_DefMod:;
      yyNext = yyOld->U_1.V_7.DefMod.Next;
      yyOld->U_1.V_7.DefMod.Next = yyNew;
      break;
    case Tree_ProgMod:;
      yyNext = yyOld->U_1.V_8.ProgMod.Next;
      yyOld->U_1.V_8.ProgMod.Next = yyNew;
      break;
    case Tree_Import1:;
      yyNext = yyOld->U_1.V_11.Import1.Next;
      yyOld->U_1.V_11.Import1.Next = yyNew;
      break;
    case Tree_From:;
      yyNext = yyOld->U_1.V_12.From.Next;
      yyOld->U_1.V_12.From.Next = yyNew;
      break;
    case Tree_Objects:;
      yyNext = yyOld->U_1.V_13.Objects.Next;
      yyOld->U_1.V_13.Objects.Next = yyNew;
      break;
    case Tree_ImpIds1:;
      yyNext = yyOld->U_1.V_16.ImpIds1.Next;
      yyOld->U_1.V_16.ImpIds1.Next = yyNew;
      break;
    case Tree_ExpIds1:;
      yyNext = yyOld->U_1.V_22.ExpIds1.Next;
      yyOld->U_1.V_22.ExpIds1.Next = yyNew;
      break;
    case Tree_Decl:;
      yyNext = yyOld->U_1.V_25.Decl.Next;
      yyOld->U_1.V_25.Decl.Next = yyNew;
      break;
    case Tree_Var:;
      yyNext = yyOld->U_1.V_26.Var.Next;
      yyOld->U_1.V_26.Var.Next = yyNew;
      break;
    case Tree_Object:;
      yyNext = yyOld->U_1.V_27.Object.Next;
      yyOld->U_1.V_27.Object.Next = yyNew;
      break;
    case Tree_Const:;
      yyNext = yyOld->U_1.V_28.Const.Next;
      yyOld->U_1.V_28.Const.Next = yyNew;
      break;
    case Tree_TypeDecl:;
      yyNext = yyOld->U_1.V_29.TypeDecl.Next;
      yyOld->U_1.V_29.TypeDecl.Next = yyNew;
      break;
    case Tree_Proc:;
      yyNext = yyOld->U_1.V_30.Proc.Next;
      yyOld->U_1.V_30.Proc.Next = yyNew;
      break;
    case Tree_ProcHead:;
      yyNext = yyOld->U_1.V_31.ProcHead.Next;
      yyOld->U_1.V_31.ProcHead.Next = yyNew;
      break;
    case Tree_Module:;
      yyNext = yyOld->U_1.V_32.Module.Next;
      yyOld->U_1.V_32.Module.Next = yyNew;
      break;
    case Tree_Opaque:;
      yyNext = yyOld->U_1.V_33.Opaque.Next;
      yyOld->U_1.V_33.Opaque.Next = yyNew;
      break;
    case Tree_VarIds1:;
      yyNext = yyOld->U_1.V_36.VarIds1.Next;
      yyOld->U_1.V_36.VarIds1.Next = yyNew;
      break;
    case Tree_Formals1:;
      yyNext = yyOld->U_1.V_39.Formals1.Next;
      yyOld->U_1.V_39.Formals1.Next = yyNew;
      break;
    case Tree_ParIds1:;
      yyNext = yyOld->U_1.V_42.ParIds1.Next;
      yyOld->U_1.V_42.ParIds1.Next = yyNew;
      break;
    case Tree_Fields1:;
      yyNext = yyOld->U_1.V_59.Fields1.Next;
      yyOld->U_1.V_59.Fields1.Next = yyNew;
      break;
    case Tree_RecordSect:;
      yyNext = yyOld->U_1.V_60.RecordSect.Next;
      yyOld->U_1.V_60.RecordSect.Next = yyNew;
      break;
    case Tree_VariantSect:;
      yyNext = yyOld->U_1.V_61.VariantSect.Next;
      yyOld->U_1.V_61.VariantSect.Next = yyNew;
      break;
    case Tree_FieldIds1:;
      yyNext = yyOld->U_1.V_64.FieldIds1.Next;
      yyOld->U_1.V_64.FieldIds1.Next = yyNew;
      break;
    case Tree_Variant:;
      yyNext = yyOld->U_1.V_70.Variant.Next;
      yyOld->U_1.V_70.Variant.Next = yyNew;
      break;
    case Tree_FormalType:;
      yyNext = yyOld->U_1.V_73.FormalType.Next;
      yyOld->U_1.V_73.FormalType.Next = yyNew;
      break;
    case Tree_EnumIds1:;
      yyNext = yyOld->U_1.V_76.EnumIds1.Next;
      yyOld->U_1.V_76.EnumIds1.Next = yyNew;
      break;
    case Tree_Elems1:;
      yyNext = yyOld->U_1.V_96.Elems1.Next;
      yyOld->U_1.V_96.Elems1.Next = yyNew;
      break;
    case Tree_Elem:;
      yyNext = yyOld->U_1.V_97.Elem.Next;
      yyOld->U_1.V_97.Elem.Next = yyNew;
      break;
    case Tree_ElemRange:;
      yyNext = yyOld->U_1.V_98.ElemRange.Next;
      yyOld->U_1.V_98.ElemRange.Next = yyNew;
      break;
    case Tree_Actual:;
      yyNext = yyOld->U_1.V_101.Actual.Next;
      yyOld->U_1.V_101.Actual.Next = yyNew;
      break;
    case Tree_Stmt:;
      yyNext = yyOld->U_1.V_104.Stmt.Next;
      yyOld->U_1.V_104.Stmt.Next = yyNew;
      break;
    case Tree_Assign:;
      yyNext = yyOld->U_1.V_105.Assign.Next;
      yyOld->U_1.V_105.Assign.Next = yyNew;
      break;
    case Tree_Call:;
      yyNext = yyOld->U_1.V_106.Call.Next;
      yyOld->U_1.V_106.Call.Next = yyNew;
      break;
    case Tree_If:;
      yyNext = yyOld->U_1.V_107.If.Next;
      yyOld->U_1.V_107.If.Next = yyNew;
      break;
    case Tree_Case:;
      yyNext = yyOld->U_1.V_108.Case.Next;
      yyOld->U_1.V_108.Case.Next = yyNew;
      break;
    case Tree_While:;
      yyNext = yyOld->U_1.V_109.While.Next;
      yyOld->U_1.V_109.While.Next = yyNew;
      break;
    case Tree_Repeat:;
      yyNext = yyOld->U_1.V_110.Repeat.Next;
      yyOld->U_1.V_110.Repeat.Next = yyNew;
      break;
    case Tree_Loop:;
      yyNext = yyOld->U_1.V_111.Loop.Next;
      yyOld->U_1.V_111.Loop.Next = yyNew;
      break;
    case Tree_For:;
      yyNext = yyOld->U_1.V_112.For.Next;
      yyOld->U_1.V_112.For.Next = yyNew;
      break;
    case Tree_With:;
      yyNext = yyOld->U_1.V_113.With.Next;
      yyOld->U_1.V_113.With.Next = yyNew;
      break;
    case Tree_Exit:;
      yyNext = yyOld->U_1.V_114.Exit.Next;
      yyOld->U_1.V_114.Exit.Next = yyNew;
      break;
    case Tree_Return1:;
      yyNext = yyOld->U_1.V_115.Return1.Next;
      yyOld->U_1.V_115.Return1.Next = yyNew;
      break;
    case Tree_Return2:;
      yyNext = yyOld->U_1.V_116.Return2.Next;
      yyOld->U_1.V_116.Return2.Next = yyNew;
      break;
    case Tree_Elsifs1:;
      yyNext = yyOld->U_1.V_119.Elsifs1.Next;
      yyOld->U_1.V_119.Elsifs1.Next = yyNew;
      break;
    case Tree_Cases1:;
      yyNext = yyOld->U_1.V_122.Cases1.Next;
      yyOld->U_1.V_122.Cases1.Next = yyNew;
      break;
    case Tree_Labels1:;
      yyNext = yyOld->U_1.V_125.Labels1.Next;
      yyOld->U_1.V_125.Labels1.Next = yyNew;
      break;
    case Tree_Label:;
      yyNext = yyOld->U_1.V_126.Label.Next;
      yyOld->U_1.V_126.Label.Next = yyNew;
      break;
    case Tree_LabelRange:;
      yyNext = yyOld->U_1.V_127.LabelRange.Next;
      yyOld->U_1.V_127.LabelRange.Next = yyNew;
      break;
    default :
      goto EXIT_1;
      break;
    }
    yyNew = yyOld;
    yyOld = yyNext;
  } EXIT_1:;
  switch (yyTail->U_1.V_1.Kind) {
  case Tree_CompUnit:;
    yyTail->U_1.V_6.CompUnit.Next = yyOld;
    break;
  case Tree_DefMod:;
    yyTail->U_1.V_7.DefMod.Next = yyOld;
    break;
  case Tree_ProgMod:;
    yyTail->U_1.V_8.ProgMod.Next = yyOld;
    break;
  case Tree_Import1:;
    yyTail->U_1.V_11.Import1.Next = yyOld;
    break;
  case Tree_From:;
    yyTail->U_1.V_12.From.Next = yyOld;
    break;
  case Tree_Objects:;
    yyTail->U_1.V_13.Objects.Next = yyOld;
    break;
  case Tree_ImpIds1:;
    yyTail->U_1.V_16.ImpIds1.Next = yyOld;
    break;
  case Tree_ExpIds1:;
    yyTail->U_1.V_22.ExpIds1.Next = yyOld;
    break;
  case Tree_Decl:;
    yyTail->U_1.V_25.Decl.Next = yyOld;
    break;
  case Tree_Var:;
    yyTail->U_1.V_26.Var.Next = yyOld;
    break;
  case Tree_Object:;
    yyTail->U_1.V_27.Object.Next = yyOld;
    break;
  case Tree_Const:;
    yyTail->U_1.V_28.Const.Next = yyOld;
    break;
  case Tree_TypeDecl:;
    yyTail->U_1.V_29.TypeDecl.Next = yyOld;
    break;
  case Tree_Proc:;
    yyTail->U_1.V_30.Proc.Next = yyOld;
    break;
  case Tree_ProcHead:;
    yyTail->U_1.V_31.ProcHead.Next = yyOld;
    break;
  case Tree_Module:;
    yyTail->U_1.V_32.Module.Next = yyOld;
    break;
  case Tree_Opaque:;
    yyTail->U_1.V_33.Opaque.Next = yyOld;
    break;
  case Tree_VarIds1:;
    yyTail->U_1.V_36.VarIds1.Next = yyOld;
    break;
  case Tree_Formals1:;
    yyTail->U_1.V_39.Formals1.Next = yyOld;
    break;
  case Tree_ParIds1:;
    yyTail->U_1.V_42.ParIds1.Next = yyOld;
    break;
  case Tree_Fields1:;
    yyTail->U_1.V_59.Fields1.Next = yyOld;
    break;
  case Tree_RecordSect:;
    yyTail->U_1.V_60.RecordSect.Next = yyOld;
    break;
  case Tree_VariantSect:;
    yyTail->U_1.V_61.VariantSect.Next = yyOld;
    break;
  case Tree_FieldIds1:;
    yyTail->U_1.V_64.FieldIds1.Next = yyOld;
    break;
  case Tree_Variant:;
    yyTail->U_1.V_70.Variant.Next = yyOld;
    break;
  case Tree_FormalType:;
    yyTail->U_1.V_73.FormalType.Next = yyOld;
    break;
  case Tree_EnumIds1:;
    yyTail->U_1.V_76.EnumIds1.Next = yyOld;
    break;
  case Tree_Elems1:;
    yyTail->U_1.V_96.Elems1.Next = yyOld;
    break;
  case Tree_Elem:;
    yyTail->U_1.V_97.Elem.Next = yyOld;
    break;
  case Tree_ElemRange:;
    yyTail->U_1.V_98.ElemRange.Next = yyOld;
    break;
  case Tree_Actual:;
    yyTail->U_1.V_101.Actual.Next = yyOld;
    break;
  case Tree_Stmt:;
    yyTail->U_1.V_104.Stmt.Next = yyOld;
    break;
  case Tree_Assign:;
    yyTail->U_1.V_105.Assign.Next = yyOld;
    break;
  case Tree_Call:;
    yyTail->U_1.V_106.Call.Next = yyOld;
    break;
  case Tree_If:;
    yyTail->U_1.V_107.If.Next = yyOld;
    break;
  case Tree_Case:;
    yyTail->U_1.V_108.Case.Next = yyOld;
    break;
  case Tree_While:;
    yyTail->U_1.V_109.While.Next = yyOld;
    break;
  case Tree_Repeat:;
    yyTail->U_1.V_110.Repeat.Next = yyOld;
    break;
  case Tree_Loop:;
    yyTail->U_1.V_111.Loop.Next = yyOld;
    break;
  case Tree_For:;
    yyTail->U_1.V_112.For.Next = yyOld;
    break;
  case Tree_With:;
    yyTail->U_1.V_113.With.Next = yyOld;
    break;
  case Tree_Exit:;
    yyTail->U_1.V_114.Exit.Next = yyOld;
    break;
  case Tree_Return1:;
    yyTail->U_1.V_115.Return1.Next = yyOld;
    break;
  case Tree_Return2:;
    yyTail->U_1.V_116.Return2.Next = yyOld;
    break;
  case Tree_Elsifs1:;
    yyTail->U_1.V_119.Elsifs1.Next = yyOld;
    break;
  case Tree_Cases1:;
    yyTail->U_1.V_122.Cases1.Next = yyOld;
    break;
  case Tree_Labels1:;
    yyTail->U_1.V_125.Labels1.Next = yyOld;
    break;
  case Tree_Label:;
    yyTail->U_1.V_126.Label.Next = yyOld;
    break;
  case Tree_LabelRange:;
    yyTail->U_1.V_127.LabelRange.Next = yyOld;
    break;
  default :
    break;
  }
  return yyNew;
}

void Tree_BeginTree
# ifdef __STDC__
()
# else
()
# endif
{
}

void Tree_CloseTree
# ifdef __STDC__
()
# else
()
# endif
{
}

static void xxExit
# ifdef __STDC__
()
# else
()
# endif
{
  IO_CloseIO();
  Exit(1L);
}

void BEGIN_Tree()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_StringMem();
    BEGIN_Idents();
    BEGIN_Positions();
    BEGIN_Defs();
    BEGIN_Values();
    BEGIN_UniqueIds();
    BEGIN_System();
    BEGIN_General();
    BEGIN_Memory();
    BEGIN_DynArray();
    BEGIN_IO();
    BEGIN_Layout();
    BEGIN_StringMem();
    BEGIN_Strings();
    BEGIN_Idents();
    BEGIN_Texts();
    BEGIN_Sets();
    BEGIN_Positions();
    BEGIN_StringMem();
    BEGIN_Idents();
    BEGIN_Positions();

    yyBlockList = NIL;
    Tree_yyPoolFreePtr = (ADDRESS)NIL;
    Tree_yyPoolMaxPtr = (ADDRESS)NIL;
    Tree_HeapUsed = 0;
    Tree_yyExit = xxExit;
    Tree_yyNodeSize.A[Tree_ROOT] = sizeof(Tree_yROOT);
    Tree_yyNodeSize.A[Tree_CompUnits] = sizeof(Tree_yCompUnits);
    Tree_yyNodeSize.A[Tree_CompUnits0] = sizeof(Tree_yCompUnits0);
    Tree_yyNodeSize.A[Tree_CompUnit] = sizeof(Tree_yCompUnit);
    Tree_yyNodeSize.A[Tree_DefMod] = sizeof(Tree_yDefMod);
    Tree_yyNodeSize.A[Tree_ProgMod] = sizeof(Tree_yProgMod);
    Tree_yyNodeSize.A[Tree_Import] = sizeof(Tree_yImport);
    Tree_yyNodeSize.A[Tree_Import0] = sizeof(Tree_yImport0);
    Tree_yyNodeSize.A[Tree_Import1] = sizeof(Tree_yImport1);
    Tree_yyNodeSize.A[Tree_From] = sizeof(Tree_yFrom);
    Tree_yyNodeSize.A[Tree_Objects] = sizeof(Tree_yObjects);
    Tree_yyNodeSize.A[Tree_ImpIds] = sizeof(Tree_yImpIds);
    Tree_yyNodeSize.A[Tree_ImpIds0] = sizeof(Tree_yImpIds0);
    Tree_yyNodeSize.A[Tree_ImpIds1] = sizeof(Tree_yImpIds1);
    Tree_yyNodeSize.A[Tree_Export] = sizeof(Tree_yExport);
    Tree_yyNodeSize.A[Tree_Export0] = sizeof(Tree_yExport0);
    Tree_yyNodeSize.A[Tree_Export1] = sizeof(Tree_yExport1);
    Tree_yyNodeSize.A[Tree_ExpIds] = sizeof(Tree_yExpIds);
    Tree_yyNodeSize.A[Tree_ExpIds0] = sizeof(Tree_yExpIds0);
    Tree_yyNodeSize.A[Tree_ExpIds1] = sizeof(Tree_yExpIds1);
    Tree_yyNodeSize.A[Tree_Decls] = sizeof(Tree_yDecls);
    Tree_yyNodeSize.A[Tree_Decls0] = sizeof(Tree_yDecls0);
    Tree_yyNodeSize.A[Tree_Decl] = sizeof(Tree_yDecl);
    Tree_yyNodeSize.A[Tree_Var] = sizeof(Tree_yVar);
    Tree_yyNodeSize.A[Tree_Object] = sizeof(Tree_yObject);
    Tree_yyNodeSize.A[Tree_Const] = sizeof(Tree_yConst);
    Tree_yyNodeSize.A[Tree_TypeDecl] = sizeof(Tree_yTypeDecl);
    Tree_yyNodeSize.A[Tree_Proc] = sizeof(Tree_yProc);
    Tree_yyNodeSize.A[Tree_ProcHead] = sizeof(Tree_yProcHead);
    Tree_yyNodeSize.A[Tree_Module] = sizeof(Tree_yModule);
    Tree_yyNodeSize.A[Tree_Opaque] = sizeof(Tree_yOpaque);
    Tree_yyNodeSize.A[Tree_VarIds] = sizeof(Tree_yVarIds);
    Tree_yyNodeSize.A[Tree_VarIds0] = sizeof(Tree_yVarIds0);
    Tree_yyNodeSize.A[Tree_VarIds1] = sizeof(Tree_yVarIds1);
    Tree_yyNodeSize.A[Tree_Formals] = sizeof(Tree_yFormals);
    Tree_yyNodeSize.A[Tree_Formals0] = sizeof(Tree_yFormals0);
    Tree_yyNodeSize.A[Tree_Formals1] = sizeof(Tree_yFormals1);
    Tree_yyNodeSize.A[Tree_ParIds] = sizeof(Tree_yParIds);
    Tree_yyNodeSize.A[Tree_ParIds0] = sizeof(Tree_yParIds0);
    Tree_yyNodeSize.A[Tree_ParIds1] = sizeof(Tree_yParIds1);
    Tree_yyNodeSize.A[Tree_Type] = sizeof(Tree_yType);
    Tree_yyNodeSize.A[Tree_Array] = sizeof(Tree_yArray);
    Tree_yyNodeSize.A[Tree_Record] = sizeof(Tree_yRecord);
    Tree_yyNodeSize.A[Tree_SetType] = sizeof(Tree_ySetType);
    Tree_yyNodeSize.A[Tree_Pointer] = sizeof(Tree_yPointer);
    Tree_yyNodeSize.A[Tree_ProcType] = sizeof(Tree_yProcType);
    Tree_yyNodeSize.A[Tree_SimpleType] = sizeof(Tree_ySimpleType);
    Tree_yyNodeSize.A[Tree_Enumeration] = sizeof(Tree_yEnumeration);
    Tree_yyNodeSize.A[Tree_Subrange] = sizeof(Tree_ySubrange);
    Tree_yyNodeSize.A[Tree_PrimaryType] = sizeof(Tree_yPrimaryType);
    Tree_yyNodeSize.A[Tree_Void] = sizeof(Tree_yVoid);
    Tree_yyNodeSize.A[Tree_TypeId] = sizeof(Tree_yTypeId);
    Tree_yyNodeSize.A[Tree_TypeId0] = sizeof(Tree_yTypeId0);
    Tree_yyNodeSize.A[Tree_TypeId1] = sizeof(Tree_yTypeId1);
    Tree_yyNodeSize.A[Tree_Fields] = sizeof(Tree_yFields);
    Tree_yyNodeSize.A[Tree_Fields0] = sizeof(Tree_yFields0);
    Tree_yyNodeSize.A[Tree_Fields1] = sizeof(Tree_yFields1);
    Tree_yyNodeSize.A[Tree_RecordSect] = sizeof(Tree_yRecordSect);
    Tree_yyNodeSize.A[Tree_VariantSect] = sizeof(Tree_yVariantSect);
    Tree_yyNodeSize.A[Tree_FieldIds] = sizeof(Tree_yFieldIds);
    Tree_yyNodeSize.A[Tree_FieldIds0] = sizeof(Tree_yFieldIds0);
    Tree_yyNodeSize.A[Tree_FieldIds1] = sizeof(Tree_yFieldIds1);
    Tree_yyNodeSize.A[Tree_TagField] = sizeof(Tree_yTagField);
    Tree_yyNodeSize.A[Tree_TagField0] = sizeof(Tree_yTagField0);
    Tree_yyNodeSize.A[Tree_TagField1] = sizeof(Tree_yTagField1);
    Tree_yyNodeSize.A[Tree_Variants] = sizeof(Tree_yVariants);
    Tree_yyNodeSize.A[Tree_Variants0] = sizeof(Tree_yVariants0);
    Tree_yyNodeSize.A[Tree_Variant] = sizeof(Tree_yVariant);
    Tree_yyNodeSize.A[Tree_FormalTypes] = sizeof(Tree_yFormalTypes);
    Tree_yyNodeSize.A[Tree_FormalTypes0] = sizeof(Tree_yFormalTypes0);
    Tree_yyNodeSize.A[Tree_FormalType] = sizeof(Tree_yFormalType);
    Tree_yyNodeSize.A[Tree_EnumIds] = sizeof(Tree_yEnumIds);
    Tree_yyNodeSize.A[Tree_EnumIds0] = sizeof(Tree_yEnumIds0);
    Tree_yyNodeSize.A[Tree_EnumIds1] = sizeof(Tree_yEnumIds1);
    Tree_yyNodeSize.A[Tree_Expr] = sizeof(Tree_yExpr);
    Tree_yyNodeSize.A[Tree_Binary] = sizeof(Tree_yBinary);
    Tree_yyNodeSize.A[Tree_Unary] = sizeof(Tree_yUnary);
    Tree_yyNodeSize.A[Tree_IntConst] = sizeof(Tree_yIntConst);
    Tree_yyNodeSize.A[Tree_RealConst] = sizeof(Tree_yRealConst);
    Tree_yyNodeSize.A[Tree_StringConst] = sizeof(Tree_yStringConst);
    Tree_yyNodeSize.A[Tree_CharConst] = sizeof(Tree_yCharConst);
    Tree_yyNodeSize.A[Tree_FuncCall] = sizeof(Tree_yFuncCall);
    Tree_yyNodeSize.A[Tree_Set] = sizeof(Tree_ySet);
    Tree_yyNodeSize.A[Tree_BitSet] = sizeof(Tree_yBitSet);
    Tree_yyNodeSize.A[Tree_Designator] = sizeof(Tree_yDesignator);
    Tree_yyNodeSize.A[Tree_Qualid] = sizeof(Tree_yQualid);
    Tree_yyNodeSize.A[Tree_Qualid0] = sizeof(Tree_yQualid0);
    Tree_yyNodeSize.A[Tree_Qualid1] = sizeof(Tree_yQualid1);
    Tree_yyNodeSize.A[Tree_Subscript] = sizeof(Tree_ySubscript);
    Tree_yyNodeSize.A[Tree_Deref] = sizeof(Tree_yDeref);
    Tree_yyNodeSize.A[Tree_Select] = sizeof(Tree_ySelect);
    Tree_yyNodeSize.A[Tree_Elems] = sizeof(Tree_yElems);
    Tree_yyNodeSize.A[Tree_Elems0] = sizeof(Tree_yElems0);
    Tree_yyNodeSize.A[Tree_Elems1] = sizeof(Tree_yElems1);
    Tree_yyNodeSize.A[Tree_Elem] = sizeof(Tree_yElem);
    Tree_yyNodeSize.A[Tree_ElemRange] = sizeof(Tree_yElemRange);
    Tree_yyNodeSize.A[Tree_Actuals] = sizeof(Tree_yActuals);
    Tree_yyNodeSize.A[Tree_Actuals0] = sizeof(Tree_yActuals0);
    Tree_yyNodeSize.A[Tree_Actual] = sizeof(Tree_yActual);
    Tree_yyNodeSize.A[Tree_Stmts] = sizeof(Tree_yStmts);
    Tree_yyNodeSize.A[Tree_Stmts0] = sizeof(Tree_yStmts0);
    Tree_yyNodeSize.A[Tree_Stmt] = sizeof(Tree_yStmt);
    Tree_yyNodeSize.A[Tree_Assign] = sizeof(Tree_yAssign);
    Tree_yyNodeSize.A[Tree_Call] = sizeof(Tree_yCall);
    Tree_yyNodeSize.A[Tree_If] = sizeof(Tree_yIf);
    Tree_yyNodeSize.A[Tree_Case] = sizeof(Tree_yCase);
    Tree_yyNodeSize.A[Tree_While] = sizeof(Tree_yWhile);
    Tree_yyNodeSize.A[Tree_Repeat] = sizeof(Tree_yRepeat);
    Tree_yyNodeSize.A[Tree_Loop] = sizeof(Tree_yLoop);
    Tree_yyNodeSize.A[Tree_For] = sizeof(Tree_yFor);
    Tree_yyNodeSize.A[Tree_With] = sizeof(Tree_yWith);
    Tree_yyNodeSize.A[Tree_Exit] = sizeof(Tree_yExit);
    Tree_yyNodeSize.A[Tree_Return1] = sizeof(Tree_yReturn1);
    Tree_yyNodeSize.A[Tree_Return2] = sizeof(Tree_yReturn2);
    Tree_yyNodeSize.A[Tree_Elsifs] = sizeof(Tree_yElsifs);
    Tree_yyNodeSize.A[Tree_Elsifs0] = sizeof(Tree_yElsifs0);
    Tree_yyNodeSize.A[Tree_Elsifs1] = sizeof(Tree_yElsifs1);
    Tree_yyNodeSize.A[Tree_Cases] = sizeof(Tree_yCases);
    Tree_yyNodeSize.A[Tree_Cases0] = sizeof(Tree_yCases0);
    Tree_yyNodeSize.A[Tree_Cases1] = sizeof(Tree_yCases1);
    Tree_yyNodeSize.A[Tree_Labels] = sizeof(Tree_yLabels);
    Tree_yyNodeSize.A[Tree_Labels0] = sizeof(Tree_yLabels0);
    Tree_yyNodeSize.A[Tree_Labels1] = sizeof(Tree_yLabels1);
    Tree_yyNodeSize.A[Tree_Label] = sizeof(Tree_yLabel);
    Tree_yyNodeSize.A[Tree_LabelRange] = sizeof(Tree_yLabelRange);
    yyMaxSize = 0;
    for (yyi = 1; yyi <= 125; yyi += 1) {
      Tree_yyNodeSize.A[yyi] = (LONGINT)((BITSET)(Tree_yyNodeSize.A[yyi] + (CARDINAL)General_MaxAlign - 1) & General_AlignMasks.A[General_MaxAlign]);
      yyMaxSize = General_Max((LONGINT)Tree_yyNodeSize.A[yyi], (LONGINT)yyMaxSize);
    }
    yyTypeRange.A[Tree_ROOT] = Tree_ROOT;
    yyTypeRange.A[Tree_CompUnits] = Tree_ProgMod;
    yyTypeRange.A[Tree_CompUnits0] = Tree_CompUnits0;
    yyTypeRange.A[Tree_CompUnit] = Tree_ProgMod;
    yyTypeRange.A[Tree_DefMod] = Tree_DefMod;
    yyTypeRange.A[Tree_ProgMod] = Tree_ProgMod;
    yyTypeRange.A[Tree_Import] = Tree_Objects;
    yyTypeRange.A[Tree_Import0] = Tree_Import0;
    yyTypeRange.A[Tree_Import1] = Tree_Objects;
    yyTypeRange.A[Tree_From] = Tree_From;
    yyTypeRange.A[Tree_Objects] = Tree_Objects;
    yyTypeRange.A[Tree_ImpIds] = Tree_ImpIds1;
    yyTypeRange.A[Tree_ImpIds0] = Tree_ImpIds0;
    yyTypeRange.A[Tree_ImpIds1] = Tree_ImpIds1;
    yyTypeRange.A[Tree_Export] = Tree_Export1;
    yyTypeRange.A[Tree_Export0] = Tree_Export0;
    yyTypeRange.A[Tree_Export1] = Tree_Export1;
    yyTypeRange.A[Tree_ExpIds] = Tree_ExpIds1;
    yyTypeRange.A[Tree_ExpIds0] = Tree_ExpIds0;
    yyTypeRange.A[Tree_ExpIds1] = Tree_ExpIds1;
    yyTypeRange.A[Tree_Decls] = Tree_Opaque;
    yyTypeRange.A[Tree_Decls0] = Tree_Decls0;
    yyTypeRange.A[Tree_Decl] = Tree_Opaque;
    yyTypeRange.A[Tree_Var] = Tree_Var;
    yyTypeRange.A[Tree_Object] = Tree_Opaque;
    yyTypeRange.A[Tree_Const] = Tree_Const;
    yyTypeRange.A[Tree_TypeDecl] = Tree_TypeDecl;
    yyTypeRange.A[Tree_Proc] = Tree_Proc;
    yyTypeRange.A[Tree_ProcHead] = Tree_ProcHead;
    yyTypeRange.A[Tree_Module] = Tree_Module;
    yyTypeRange.A[Tree_Opaque] = Tree_Opaque;
    yyTypeRange.A[Tree_VarIds] = Tree_VarIds1;
    yyTypeRange.A[Tree_VarIds0] = Tree_VarIds0;
    yyTypeRange.A[Tree_VarIds1] = Tree_VarIds1;
    yyTypeRange.A[Tree_Formals] = Tree_Formals1;
    yyTypeRange.A[Tree_Formals0] = Tree_Formals0;
    yyTypeRange.A[Tree_Formals1] = Tree_Formals1;
    yyTypeRange.A[Tree_ParIds] = Tree_ParIds1;
    yyTypeRange.A[Tree_ParIds0] = Tree_ParIds0;
    yyTypeRange.A[Tree_ParIds1] = Tree_ParIds1;
    yyTypeRange.A[Tree_Type] = Tree_TypeId1;
    yyTypeRange.A[Tree_Array] = Tree_Array;
    yyTypeRange.A[Tree_Record] = Tree_Record;
    yyTypeRange.A[Tree_SetType] = Tree_SetType;
    yyTypeRange.A[Tree_Pointer] = Tree_Pointer;
    yyTypeRange.A[Tree_ProcType] = Tree_ProcType;
    yyTypeRange.A[Tree_SimpleType] = Tree_TypeId1;
    yyTypeRange.A[Tree_Enumeration] = Tree_Enumeration;
    yyTypeRange.A[Tree_Subrange] = Tree_Subrange;
    yyTypeRange.A[Tree_PrimaryType] = Tree_TypeId1;
    yyTypeRange.A[Tree_Void] = Tree_Void;
    yyTypeRange.A[Tree_TypeId] = Tree_TypeId1;
    yyTypeRange.A[Tree_TypeId0] = Tree_TypeId0;
    yyTypeRange.A[Tree_TypeId1] = Tree_TypeId1;
    yyTypeRange.A[Tree_Fields] = Tree_VariantSect;
    yyTypeRange.A[Tree_Fields0] = Tree_Fields0;
    yyTypeRange.A[Tree_Fields1] = Tree_VariantSect;
    yyTypeRange.A[Tree_RecordSect] = Tree_RecordSect;
    yyTypeRange.A[Tree_VariantSect] = Tree_VariantSect;
    yyTypeRange.A[Tree_FieldIds] = Tree_FieldIds1;
    yyTypeRange.A[Tree_FieldIds0] = Tree_FieldIds0;
    yyTypeRange.A[Tree_FieldIds1] = Tree_FieldIds1;
    yyTypeRange.A[Tree_TagField] = Tree_TagField1;
    yyTypeRange.A[Tree_TagField0] = Tree_TagField0;
    yyTypeRange.A[Tree_TagField1] = Tree_TagField1;
    yyTypeRange.A[Tree_Variants] = Tree_Variant;
    yyTypeRange.A[Tree_Variants0] = Tree_Variants0;
    yyTypeRange.A[Tree_Variant] = Tree_Variant;
    yyTypeRange.A[Tree_FormalTypes] = Tree_FormalType;
    yyTypeRange.A[Tree_FormalTypes0] = Tree_FormalTypes0;
    yyTypeRange.A[Tree_FormalType] = Tree_FormalType;
    yyTypeRange.A[Tree_EnumIds] = Tree_EnumIds1;
    yyTypeRange.A[Tree_EnumIds0] = Tree_EnumIds0;
    yyTypeRange.A[Tree_EnumIds1] = Tree_EnumIds1;
    yyTypeRange.A[Tree_Expr] = Tree_Select;
    yyTypeRange.A[Tree_Binary] = Tree_Binary;
    yyTypeRange.A[Tree_Unary] = Tree_Unary;
    yyTypeRange.A[Tree_IntConst] = Tree_IntConst;
    yyTypeRange.A[Tree_RealConst] = Tree_RealConst;
    yyTypeRange.A[Tree_StringConst] = Tree_StringConst;
    yyTypeRange.A[Tree_CharConst] = Tree_CharConst;
    yyTypeRange.A[Tree_FuncCall] = Tree_FuncCall;
    yyTypeRange.A[Tree_Set] = Tree_Set;
    yyTypeRange.A[Tree_BitSet] = Tree_BitSet;
    yyTypeRange.A[Tree_Designator] = Tree_Select;
    yyTypeRange.A[Tree_Qualid] = Tree_Qualid1;
    yyTypeRange.A[Tree_Qualid0] = Tree_Qualid0;
    yyTypeRange.A[Tree_Qualid1] = Tree_Qualid1;
    yyTypeRange.A[Tree_Subscript] = Tree_Subscript;
    yyTypeRange.A[Tree_Deref] = Tree_Deref;
    yyTypeRange.A[Tree_Select] = Tree_Select;
    yyTypeRange.A[Tree_Elems] = Tree_ElemRange;
    yyTypeRange.A[Tree_Elems0] = Tree_Elems0;
    yyTypeRange.A[Tree_Elems1] = Tree_ElemRange;
    yyTypeRange.A[Tree_Elem] = Tree_Elem;
    yyTypeRange.A[Tree_ElemRange] = Tree_ElemRange;
    yyTypeRange.A[Tree_Actuals] = Tree_Actual;
    yyTypeRange.A[Tree_Actuals0] = Tree_Actuals0;
    yyTypeRange.A[Tree_Actual] = Tree_Actual;
    yyTypeRange.A[Tree_Stmts] = Tree_Return2;
    yyTypeRange.A[Tree_Stmts0] = Tree_Stmts0;
    yyTypeRange.A[Tree_Stmt] = Tree_Return2;
    yyTypeRange.A[Tree_Assign] = Tree_Assign;
    yyTypeRange.A[Tree_Call] = Tree_Call;
    yyTypeRange.A[Tree_If] = Tree_If;
    yyTypeRange.A[Tree_Case] = Tree_Case;
    yyTypeRange.A[Tree_While] = Tree_While;
    yyTypeRange.A[Tree_Repeat] = Tree_Repeat;
    yyTypeRange.A[Tree_Loop] = Tree_Loop;
    yyTypeRange.A[Tree_For] = Tree_For;
    yyTypeRange.A[Tree_With] = Tree_With;
    yyTypeRange.A[Tree_Exit] = Tree_Exit;
    yyTypeRange.A[Tree_Return1] = Tree_Return1;
    yyTypeRange.A[Tree_Return2] = Tree_Return2;
    yyTypeRange.A[Tree_Elsifs] = Tree_Elsifs1;
    yyTypeRange.A[Tree_Elsifs0] = Tree_Elsifs0;
    yyTypeRange.A[Tree_Elsifs1] = Tree_Elsifs1;
    yyTypeRange.A[Tree_Cases] = Tree_Cases1;
    yyTypeRange.A[Tree_Cases0] = Tree_Cases0;
    yyTypeRange.A[Tree_Cases1] = Tree_Cases1;
    yyTypeRange.A[Tree_Labels] = Tree_LabelRange;
    yyTypeRange.A[Tree_Labels0] = Tree_Labels0;
    yyTypeRange.A[Tree_Labels1] = Tree_LabelRange;
    yyTypeRange.A[Tree_Label] = Tree_Label;
    yyTypeRange.A[Tree_LabelRange] = Tree_LabelRange;
    Tree_BeginTree();
  }
}
