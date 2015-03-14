#define DEFINITION_StringMem

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

typedef LONGINT StringMem_tStringRef;
extern StringMem_tStringRef StringMem_PutString ARGS((Strings_tString *s));
extern void StringMem_GetString ARGS((StringMem_tStringRef r, Strings_tString *s));
extern CARDINAL StringMem_Length ARGS((StringMem_tStringRef r));
extern BOOLEAN StringMem_IsEqual ARGS((StringMem_tStringRef r, Strings_tString *s));
extern void StringMem_WriteString ARGS((IO_tFile f, StringMem_tStringRef r));
extern void StringMem_WriteStringMemory ARGS(());
extern void StringMem_InitStringMemory ARGS(());
extern void BEGIN_StringMem();
