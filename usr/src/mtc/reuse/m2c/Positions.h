#define DEFINITION_Positions

#ifndef DEFINITION_IO
#include "IO.h"
#endif

typedef struct Positions_1 {
    SHORTCARD Line, Column;
} Positions_tPosition;
extern Positions_tPosition Positions_NoPosition;
extern INTEGER Positions_Compare ARGS((Positions_tPosition Position1, Positions_tPosition Position2));
extern void Positions_WritePosition ARGS((IO_tFile File, Positions_tPosition Position));
extern void BEGIN_Positions();
