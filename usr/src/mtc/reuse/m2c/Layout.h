#define DEFINITION_Layout

#ifndef DEFINITION_IO
#include "IO.h"
#endif

extern void Layout_WriteChar ARGS((IO_tFile f, CHAR Ch));
extern void Layout_WriteSpace ARGS((IO_tFile f));
extern void Layout_WriteSpaces ARGS((IO_tFile f, INTEGER Count));
extern void Layout_ReadSpace ARGS((IO_tFile f));
extern void Layout_ReadSpaces ARGS((IO_tFile f, INTEGER Count));
extern void Layout_SkipSpaces ARGS((IO_tFile f));
extern void BEGIN_Layout();
