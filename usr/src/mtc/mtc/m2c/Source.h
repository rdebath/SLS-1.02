#define DEFINITION_Source

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

extern System_tFile Source_BeginSource ARGS((CHAR FileName[], LONGCARD ));
extern INTEGER Source_GetLine ARGS((System_tFile File, ADDRESS Buffer, CARDINAL Size));
extern void Source_CloseSource ARGS((System_tFile File));
extern void BEGIN_Source();
