#define DEFINITION_Base

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

extern struct Base_1 {
    CHAR A[127 + 1];
} Base_SourceFile;
extern Strings_tString Base_MtcLibrary;
extern void Base_CheckArguments ARGS(());
extern BOOLEAN Base_OptionIsSet ARGS((CHAR Option));
extern void Base_CheckDefFile ARGS((Idents_tIdent ModuleName, CHAR FileName[], LONGCARD , BOOLEAN *Success));
extern IO_tFile Base_OpenHeader ARGS((Idents_tIdent Ident));
extern IO_tFile Base_OpenProgram ARGS((Idents_tIdent Ident));
extern void BEGIN_Base();
