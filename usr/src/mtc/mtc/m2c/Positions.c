#include "SYSTEM_.h"

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

Positions_tPosition Positions_NoPosition;



void Positions_WritePosition
# ifdef __STDC__
(IO_tFile File, Positions_tPosition Position)
# else
(File, Position)
IO_tFile File;
Positions_tPosition Position;
# endif
{
  IO_WriteI(File, (LONGINT)Position.Line, 3L);
  IO_WriteS(File, (STRING)", ", 2L);
  IO_WriteI(File, (LONGINT)Position.Column, 2L);
}

void BEGIN_Positions()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_Idents();
    BEGIN_IO();
    BEGIN_Idents();

    Positions_NoPosition.File = Idents_NoIdent;
    Positions_NoPosition.Line = 0;
    Positions_NoPosition.Column = 0;
  }
}
