#define DEFINITION_Idents

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

typedef SHORTCARD Idents_tIdent;
extern Idents_tIdent Idents_NoIdent;
extern Idents_tIdent Idents_MakeIdent ARGS((Strings_tString *s));
extern void Idents_GetString ARGS((Idents_tIdent i, Strings_tString *s));
extern StringMem_tStringRef Idents_GetStringRef ARGS((Idents_tIdent i));
extern Idents_tIdent Idents_MaxIdent ARGS(());
extern void Idents_WriteIdent ARGS((IO_tFile f, Idents_tIdent i));
extern void Idents_WriteIdents ARGS(());
extern void Idents_InitIdents ARGS(());
extern void Idents_WriteHashTable ARGS(());
extern void BEGIN_Idents();
