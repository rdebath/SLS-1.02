#define DEFINITION_UniqueIds

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#define UniqueIds_cIdents	0
typedef unsigned char UniqueIds_tIdents;
#define UniqueIds_eKeyword	0
#define UniqueIds_eConst	1
#define UniqueIds_eType	2
#define UniqueIds_eVar	3
#define UniqueIds_eModuleVar	4
#define UniqueIds_eProc	5
#define UniqueIds_eField	6
typedef unsigned char UniqueIds_tIdentClass;
extern void UniqueIds_BeginUniqueIds ARGS(());
extern UniqueIds_tIdents UniqueIds_EnterProc ARGS((UniqueIds_tIdents Idents));
extern UniqueIds_tIdents UniqueIds_LeaveProc ARGS((UniqueIds_tIdents Idents));
extern UniqueIds_tIdents UniqueIds_DeclareIdent ARGS((UniqueIds_tIdents Idents, UniqueIds_tIdentClass IdentClass, Idents_tIdent Ident));
extern BOOLEAN UniqueIds_NameConflict ARGS((UniqueIds_tIdents Idents, UniqueIds_tIdentClass IdentClass, Idents_tIdent Ident));
extern void UniqueIds_CloseUniqueIds ARGS(());
extern void BEGIN_UniqueIds();
