#define DEFINITION_Texts

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Lists
#include "Lists.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

typedef Lists_tList Texts_tText;
extern void Texts_MakeText ARGS((Texts_tText *Text));
extern void Texts_Append ARGS((Texts_tText *Text, Strings_tString *String));
extern void Texts_Insert ARGS((Texts_tText *Text, Strings_tString *String));
extern BOOLEAN Texts_IsEmpty ARGS((Texts_tText *Text));
extern void Texts_WriteText ARGS((IO_tFile f, Texts_tText Text));
extern void BEGIN_Texts();
