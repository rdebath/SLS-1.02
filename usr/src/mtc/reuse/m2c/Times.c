#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Times
#include "Times.h"
#endif


static LONGINT PrevTime;


LONGINT Times_CpuTime
# ifdef __STDC__
()
# else
()
# endif
{
  return Time();
}

LONGINT Times_StepTime
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT ActTime;
  LONGINT DeltaTime;

  ActTime = Times_CpuTime();
  DeltaTime = ActTime - PrevTime;
  PrevTime = ActTime;
  return DeltaTime;
}

void Times_WriteStepTime
# ifdef __STDC__
(CHAR Text[], LONGCARD O_1)
# else
(Text, O_1)
CHAR Text[];
LONGCARD O_1;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Text, O_1, CHAR)
  IO_WriteS((System_tFile)IO_StdOutput, Text, O_1);
  IO_WriteI((System_tFile)IO_StdOutput, Times_StepTime(), 5L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  FREE_OPEN_ARRAYS
}

void BEGIN_Times()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_System();
    BEGIN_IO();

    PrevTime = 0;
  }
}
