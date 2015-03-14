#include "SYSTEM_.h"

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Lists
#include "Lists.h"
#endif




void Lists_MakeList
# ifdef __STDC__
(Lists_tList *List)
# else
(List)
Lists_tList *List;
# endif
{
  List->FirstElmt = NIL;
  List->LastElmt = NIL;
}

void Lists_Insert
# ifdef __STDC__
(Lists_tList *List, Lists_tElmt Elmt)
# else
(List, Elmt)
Lists_tList *List;
Lists_tElmt Elmt;
# endif
{
  Lists_tListElmtPtr ActElmt;

  ActElmt = (Lists_tListElmtPtr)Memory_Alloc((LONGINT)sizeof(Lists_tListElmt));
  ActElmt->Succ = NIL;
  ActElmt->Elmt = Elmt;
  if (List->FirstElmt == NIL) {
    List->LastElmt = ActElmt;
  } else {
    ActElmt->Succ = List->FirstElmt;
  }
  List->FirstElmt = ActElmt;
}

void Lists_Append
# ifdef __STDC__
(Lists_tList *List, Lists_tElmt Elmt)
# else
(List, Elmt)
Lists_tList *List;
Lists_tElmt Elmt;
# endif
{
  Lists_tListElmtPtr ActElmt;

  ActElmt = (Lists_tListElmtPtr)Memory_Alloc((LONGINT)sizeof(Lists_tListElmt));
  ActElmt->Succ = NIL;
  ActElmt->Elmt = Elmt;
  if (List->FirstElmt == NIL) {
    List->FirstElmt = ActElmt;
  } else {
    List->LastElmt->Succ = ActElmt;
  }
  List->LastElmt = ActElmt;
}

Lists_tElmt Lists_Head
# ifdef __STDC__
(Lists_tList List)
# else
(List)
Lists_tList List;
# endif
{
  return List.FirstElmt->Elmt;
}

void Lists_Tail
# ifdef __STDC__
(Lists_tList *List)
# else
(List)
Lists_tList *List;
# endif
{
  List->FirstElmt = List->FirstElmt->Succ;
}

Lists_tElmt Lists_Last
# ifdef __STDC__
(Lists_tList List)
# else
(List)
Lists_tList List;
# endif
{
  return List.LastElmt->Elmt;
}

void Lists_Front
# ifdef __STDC__
(Lists_tList *List)
# else
(List)
Lists_tList *List;
# endif
{
}

BOOLEAN Lists_IsEmpty
# ifdef __STDC__
(Lists_tList List)
# else
(List)
Lists_tList List;
# endif
{
  return List.FirstElmt == NIL;
}

CARDINAL Lists_Length
# ifdef __STDC__
(Lists_tList List)
# else
(List)
Lists_tList List;
# endif
{
  CARDINAL n;

  n = 0;
  while (List.FirstElmt != NIL) {
    INC(n);
    List.FirstElmt = List.FirstElmt->Succ;
  }
  return n;
}

void Lists_WriteList
# ifdef __STDC__
(IO_tFile f, Lists_tList List, Lists_tProcOfFileAddress Proc)
# else
(f, List, Proc)
IO_tFile f;
Lists_tList List;
Lists_tProcOfFileAddress Proc;
# endif
{
  while (!Lists_IsEmpty(List)) {
    (*Proc)(f, Lists_Head(List));
    Lists_Tail(&List);
  }
}

void BEGIN_Lists()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_Memory();
    BEGIN_IO();

  }
}
