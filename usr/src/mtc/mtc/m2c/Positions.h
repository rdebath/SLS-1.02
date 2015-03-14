#define DEFINITION_Positions

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

typedef struct Positions_1 {
    Idents_tIdent File;
    SHORTCARD Line, Column;
} Positions_tPosition;
extern Positions_tPosition Positions_NoPosition;
extern void Positions_WritePosition ARGS((IO_tFile File, Positions_tPosition Position));
extern void BEGIN_Positions();
