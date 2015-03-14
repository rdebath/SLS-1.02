#include "SYSTEM_.h"

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Layout
#include "Layout.h"
#endif




void Layout_WriteChar
# ifdef __STDC__
(IO_tFile f, CHAR Ch)
# else
(f, Ch)
IO_tFile f;
CHAR Ch;
# endif
{
  if ('!' <= Ch && Ch <= '~') {
    IO_WriteC(f, '\'');
    IO_WriteC(f, Ch);
    IO_WriteC(f, '\'');
  } else if (Ch == '\0') {
    IO_WriteS(f, (STRING)"eps", 3L);
  } else {
    IO_WriteI(f, (LONGINT)ORD(Ch), 2L);
    IO_WriteC(f, 'C');
  }
}

void Layout_WriteSpace
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  IO_WriteC(f, ' ');
}

void Layout_WriteSpaces
# ifdef __STDC__
(IO_tFile f, INTEGER Count)
# else
(f, Count)
IO_tFile f;
INTEGER Count;
# endif
{
  INTEGER i;

  {
    LONGINT B_1 = 1, B_2 = Count;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        IO_WriteC(f, ' ');
        if (i >= B_2) break;
      }
  }
}

void Layout_ReadSpace
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  CHAR Ch;

  Ch = IO_ReadC(f);
}

void Layout_ReadSpaces
# ifdef __STDC__
(IO_tFile f, INTEGER Count)
# else
(f, Count)
IO_tFile f;
INTEGER Count;
# endif
{
  INTEGER i;
  CHAR Ch;

  {
    LONGINT B_3 = 1, B_4 = Count;

    if (B_3 <= B_4)
      for (i = B_3;; i += 1) {
        Ch = IO_ReadC(f);
        if (i >= B_4) break;
      }
  }
}

void Layout_SkipSpaces
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  do {
  } while (!(IO_ReadC(f) != ' '));
  IO_UnRead(f);
}

void BEGIN_Layout()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_IO();

  }
}
