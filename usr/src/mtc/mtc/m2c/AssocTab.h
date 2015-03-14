#define DEFINITION_AssocTab

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

extern void AssocTab_BeginAssocTab ARGS(());
extern void AssocTab_PutAssoc ARGS((Idents_tIdent Ident, ADDRESS Object));
extern void AssocTab_GetAssoc ARGS((Idents_tIdent Ident, ADDRESS *Object));
extern void AssocTab_CloseAssocTab ARGS(());
extern void BEGIN_AssocTab();
