#define DEFINITION_GenIdents

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

extern Idents_tIdent GenIdents_MakeQualified ARGS((Idents_tIdent Module, Idents_tIdent Ident));
extern Idents_tIdent GenIdents_RenameField ARGS((Idents_tIdent Ident));
extern Idents_tIdent GenIdents_Rename ARGS((Idents_tIdent Ident));
extern Idents_tIdent GenIdents_GenLabel ARGS(());
extern Idents_tIdent GenIdents_GenSelector ARGS((CHAR StructOrUnion, CARDINAL Nr));
extern Idents_tIdent GenIdents_GenParam ARGS(());
extern Idents_tIdent GenIdents_GenWith ARGS(());
extern Idents_tIdent GenIdents_GenStruct1 ARGS((Idents_tIdent Module, CARDINAL Nr));
extern Idents_tIdent GenIdents_GenStruct2 ARGS(());
extern Idents_tIdent GenIdents_GenGlobalPtr ARGS((Idents_tIdent Ident));
extern Idents_tIdent GenIdents_GenLocalPtr ARGS(());
extern Idents_tIdent GenIdents_GenBound ARGS(());
extern Idents_tIdent GenIdents_GenReturn ARGS(());
extern Idents_tIdent GenIdents_GenString ARGS(());
extern Idents_tIdent GenIdents_GenOpaque ARGS((Idents_tIdent TypeName));
extern void BEGIN_GenIdents();
