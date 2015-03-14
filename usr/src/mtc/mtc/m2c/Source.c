#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

#ifndef DEFINITION_Source
#include "Source.h"
#endif


static Strings_tString String;


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
  Strings_ArrayToString(FileName, O_1, &String);
  Scanner_Attribute.Position.File = Idents_MakeIdent(&String);
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
    BEGIN_Idents();
    BEGIN_System();
    BEGIN_Idents();
    BEGIN_Strings();
    BEGIN_Scanner();

    Strings_ArrayToString((STRING)"StdInput", 8L, &String);
    Scanner_Attribute.Position.File = Idents_MakeIdent(&String);
  }
}
