#include "SYSTEM_.h"

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

Idents_tIdent Idents_NoIdent;

#define cNoIdent	0
#define InitialTableSize	512
#define HashTableSize	256
typedef struct S_1 {
    StringMem_tStringRef String;
    Strings_tStringIndex Length;
    Idents_tIdent Collision;
} IdentTableEntry;
typedef LONGCARD HashIndex;
static struct S_2 {
    IdentTableEntry A[1000000 + 1];
} *TablePtr;
static LONGINT IdentTableSize;
static Idents_tIdent IdentCount;
static struct S_3 {
    Idents_tIdent A[HashTableSize + 1];
} HashTable;
static HashIndex i;


Idents_tIdent Idents_MakeIdent
# ifdef __STDC__
(Strings_tString *s)
# else
(s)
Strings_tString *s;
# endif
{
  HashIndex HashTableIndex;
  Idents_tIdent CurIdent;
  LONGINT lIdentCount;

  {
    register Strings_tString *W_1 = s;

    if (W_1->Length == 0) {
      HashTableIndex = 0;
    } else {
      HashTableIndex = (ORD(W_1->Chars.A[1]) + ORD(W_1->Chars.A[W_1->Length]) * 11 + W_1->Length * 26) % HashTableSize;
    }
  }
  CurIdent = HashTable.A[HashTableIndex];
  for (;;) {
    if (CurIdent == cNoIdent) {
      goto EXIT_1;
    }
    {
      register IdentTableEntry *W_2 = &TablePtr->A[CurIdent];

      if (W_2->Length == s->Length && StringMem_IsEqual(W_2->String, s)) {
        return CurIdent;
      }
      CurIdent = W_2->Collision;
    }
  } EXIT_1:;
  INC(IdentCount);
  lIdentCount = IdentCount;
  if (lIdentCount == IdentTableSize) {
    DynArray_ExtendArray((ADDRESS *)&TablePtr, &IdentTableSize, (LONGINT)sizeof(IdentTableEntry));
  }
  {
    register IdentTableEntry *W_3 = &TablePtr->A[IdentCount];

    W_3->String = StringMem_PutString(s);
    W_3->Length = s->Length;
    W_3->Collision = HashTable.A[HashTableIndex];
  }
  HashTable.A[HashTableIndex] = IdentCount;
  return IdentCount;
}

void Idents_GetString
# ifdef __STDC__
(Idents_tIdent i, Strings_tString *s)
# else
(i, s)
Idents_tIdent i;
Strings_tString *s;
# endif
{
  {
    register IdentTableEntry *W_4 = &TablePtr->A[i];

    StringMem_GetString(W_4->String, s);
  }
}

StringMem_tStringRef Idents_GetStringRef
# ifdef __STDC__
(Idents_tIdent i)
# else
(i)
Idents_tIdent i;
# endif
{
  return TablePtr->A[i].String;
}

Idents_tIdent Idents_MaxIdent
# ifdef __STDC__
()
# else
()
# endif
{
  return IdentCount;
}

void Idents_WriteIdent
# ifdef __STDC__
(IO_tFile f, Idents_tIdent i)
# else
(f, i)
IO_tFile f;
Idents_tIdent i;
# endif
{
  Strings_tString s;

  Idents_GetString(i, &s);
  Strings_WriteS(f, &s);
}

void Idents_WriteIdents
# ifdef __STDC__
()
# else
()
# endif
{
  CARDINAL i;

  {
    LONGCARD B_1 = 1, B_2 = IdentCount;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)i, 5L);
        IO_WriteC((System_tFile)IO_StdOutput, ' ');
        Idents_WriteIdent((System_tFile)IO_StdOutput, (SHORTCARD)i);
        IO_WriteNl((System_tFile)IO_StdOutput);
        if (i >= B_2) break;
      }
  }
}

void Idents_WriteHashTable
# ifdef __STDC__
()
# else
()
# endif
{
  Idents_tIdent CurIdent;
  HashIndex i;
  CARDINAL Count;

  for (i = 0; i <= HashTableSize; i += 1) {
    IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)i, 5L);
    Count = 0;
    CurIdent = HashTable.A[i];
    while (CurIdent != cNoIdent) {
      INC(Count);
      CurIdent = TablePtr->A[CurIdent].Collision;
    }
    IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Count, 5L);
    CurIdent = HashTable.A[i];
    while (CurIdent != cNoIdent) {
      IO_WriteC((System_tFile)IO_StdOutput, ' ');
      Idents_WriteIdent((System_tFile)IO_StdOutput, CurIdent);
      CurIdent = TablePtr->A[CurIdent].Collision;
    }
    IO_WriteNl((System_tFile)IO_StdOutput);
  }
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Idents =", 8L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)IdentCount, 5L);
  IO_WriteNl((System_tFile)IO_StdOutput);
}

void Idents_InitIdents
# ifdef __STDC__
()
# else
()
# endif
{
  Strings_tString String;

  for (i = 0; i <= HashTableSize; i += 1) {
    HashTable.A[i] = cNoIdent;
  }
  IdentCount = 0;
  Strings_AssignEmpty(&String);
  Idents_NoIdent = Idents_MakeIdent(&String);
}

void BEGIN_Idents()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_Strings();
    BEGIN_StringMem();
    BEGIN_DynArray();
    BEGIN_Strings();
    BEGIN_StringMem();
    BEGIN_IO();
    BEGIN_Strings();
    BEGIN_StringMem();

    IdentTableSize = InitialTableSize;
    DynArray_MakeArray((ADDRESS *)&TablePtr, &IdentTableSize, (LONGINT)sizeof(IdentTableEntry));
    Idents_InitIdents();
  }
}
