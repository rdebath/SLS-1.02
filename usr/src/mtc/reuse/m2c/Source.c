#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_Source
#include "Source.h"
#endif


#define IgnoreChar	' '
struct S_2 {
    CHAR A[30000 + 1];
};


System_tFile Source_BeginSource
# ifdef __STDC__
(CHAR FileName[], LONGCARD O_1)
# else
(FileName, O_1)
CHAR FileName[];
LONGCARD O_1;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(FileName, O_1, CHAR)
  {
    System_tFile R_1 = OpenInput(FileName, O_1);

    FREE_OPEN_ARRAYS
    return R_1;
  }
}

INTEGER Source_GetLine
# ifdef __STDC__
(System_tFile File, ADDRESS Buffer, CARDINAL Size)
# else
(File, Buffer, Size)
System_tFile File;
ADDRESS Buffer;
CARDINAL Size;
# endif
{
  INTEGER n;
  struct S_2 *BufferPtr;

  return Read(File, Buffer, (LONGINT)Size);
}

void Source_CloseSource
# ifdef __STDC__
(System_tFile File)
# else
(File)
System_tFile File;
# endif
{
  Close(File);
}

void BEGIN_Source()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_System();
    BEGIN_System();

  }
}
