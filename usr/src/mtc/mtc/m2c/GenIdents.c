#include "SYSTEM_.h"

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_GenIdents
#include "GenIdents.h"
#endif


static CARDINAL RenameCnt, LabelCnt, ParamCnt, WithCnt, StructCnt, GlobalPtrCnt, LocalPtrCnt, BoundCnt, ReturnCnt, StringCnt;
static Idents_tIdent IdentEXIT;
static Strings_tString String;
static Idents_tIdent GenIdent1 ARGS((CHAR Letter, CARDINAL Nr, Idents_tIdent Ident));
static Idents_tIdent GenIdent2 ARGS((CHAR Letter, CARDINAL Nr));
static Idents_tIdent GenIdent3 ARGS((Idents_tIdent Ident, CARDINAL Nr));


Idents_tIdent GenIdents_MakeQualified
# ifdef __STDC__
(Idents_tIdent Module, Idents_tIdent Ident)
# else
(Module, Ident)
Idents_tIdent Module, Ident;
# endif
{
  Strings_tString QualId, String;

  Idents_GetString(Module, &QualId);
  Strings_Append(&QualId, '_');
  Idents_GetString(Ident, &String);
  Strings_Concatenate(&QualId, &String);
  return Idents_MakeIdent(&QualId);
}

Idents_tIdent GenIdents_RenameField
# ifdef __STDC__
(Idents_tIdent Ident)
# else
(Ident)
Idents_tIdent Ident;
# endif
{
  return GenIdent1('C', 0L, Ident);
}

Idents_tIdent GenIdents_Rename
# ifdef __STDC__
(Idents_tIdent Ident)
# else
(Ident)
Idents_tIdent Ident;
# endif
{
  INC(RenameCnt);
  return GenIdent1('C', RenameCnt, Ident);
}

Idents_tIdent GenIdents_GenLabel
# ifdef __STDC__
()
# else
()
# endif
{
  INC(LabelCnt);
  return GenIdent3(IdentEXIT, LabelCnt);
}

Idents_tIdent GenIdents_GenSelector
# ifdef __STDC__
(CHAR StructOrUnion, CARDINAL Nr)
# else
(StructOrUnion, Nr)
CHAR StructOrUnion;
CARDINAL Nr;
# endif
{
  return GenIdent2(StructOrUnion, Nr);
}

Idents_tIdent GenIdents_GenParam
# ifdef __STDC__
()
# else
()
# endif
{
  INC(ParamCnt);
  return GenIdent2('O', ParamCnt);
}

Idents_tIdent GenIdents_GenWith
# ifdef __STDC__
()
# else
()
# endif
{
  INC(WithCnt);
  return GenIdent2('W', WithCnt);
}

Idents_tIdent GenIdents_GenStruct1
# ifdef __STDC__
(Idents_tIdent Module, CARDINAL Nr)
# else
(Module, Nr)
Idents_tIdent Module;
CARDINAL Nr;
# endif
{
  Strings_tString ModuleId;

  Idents_GetString(Module, &ModuleId);
  if (Strings_Length(&ModuleId) > 1) {
    return GenIdent3(Module, Nr);
  } else {
    Strings_Append(&ModuleId, '_');
    return GenIdent3(Idents_MakeIdent(&ModuleId), Nr);
  }
}

Idents_tIdent GenIdents_GenStruct2
# ifdef __STDC__
()
# else
()
# endif
{
  INC(StructCnt);
  return GenIdent2('S', StructCnt);
}

Idents_tIdent GenIdents_GenGlobalPtr
# ifdef __STDC__
(Idents_tIdent Ident)
# else
(Ident)
Idents_tIdent Ident;
# endif
{
  INC(GlobalPtrCnt);
  return GenIdent1('G', GlobalPtrCnt, Ident);
}

Idents_tIdent GenIdents_GenLocalPtr
# ifdef __STDC__
()
# else
()
# endif
{
  INC(LocalPtrCnt);
  return GenIdent2('L', LocalPtrCnt);
}

Idents_tIdent GenIdents_GenBound
# ifdef __STDC__
()
# else
()
# endif
{
  INC(BoundCnt);
  return GenIdent2('B', BoundCnt);
}

Idents_tIdent GenIdents_GenReturn
# ifdef __STDC__
()
# else
()
# endif
{
  INC(ReturnCnt);
  return GenIdent2('R', ReturnCnt);
}

Idents_tIdent GenIdents_GenString
# ifdef __STDC__
()
# else
()
# endif
{
  INC(StringCnt);
  return GenIdent2('X', StringCnt);
}

Idents_tIdent GenIdents_GenOpaque
# ifdef __STDC__
(Idents_tIdent TypeName)
# else
(TypeName)
Idents_tIdent TypeName;
# endif
{
  Strings_tString NewId, QualId;
  Strings_tStringIndex Index;

  Idents_GetString(TypeName, &QualId);
  Index = Strings_Length(&QualId) - 1;
  for (;;) {
    if (Strings_Char(&QualId, Index) == '_') {
      goto EXIT_1;
    } else {
      DEC(Index);
    }
  } EXIT_1:;
  Strings_SubString(&QualId, Index + 1, (Strings_tStringIndex)Strings_Length(&QualId), &NewId);
  return Idents_MakeIdent(&NewId);
}

static Idents_tIdent GenIdent1
# ifdef __STDC__
(CHAR Letter, CARDINAL Nr, Idents_tIdent Ident)
# else
(Letter, Nr, Ident)
CHAR Letter;
CARDINAL Nr;
Idents_tIdent Ident;
# endif
{
  Strings_tString NewId, String;

  Strings_AssignEmpty(&NewId);
  Strings_Append(&NewId, Letter);
  Strings_Append(&NewId, '_');
  Strings_IntToString((LONGINT)Nr, &String);
  Strings_Concatenate(&NewId, &String);
  Strings_Append(&NewId, '_');
  Idents_GetString(Ident, &String);
  Strings_Concatenate(&NewId, &String);
  return Idents_MakeIdent(&NewId);
}

static Idents_tIdent GenIdent2
# ifdef __STDC__
(CHAR Letter, CARDINAL Nr)
# else
(Letter, Nr)
CHAR Letter;
CARDINAL Nr;
# endif
{
  Strings_tString NewId, String;

  Strings_AssignEmpty(&NewId);
  Strings_Append(&NewId, Letter);
  Strings_Append(&NewId, '_');
  Strings_IntToString((LONGINT)Nr, &String);
  Strings_Concatenate(&NewId, &String);
  return Idents_MakeIdent(&NewId);
}

static Idents_tIdent GenIdent3
# ifdef __STDC__
(Idents_tIdent Ident, CARDINAL Nr)
# else
(Ident, Nr)
Idents_tIdent Ident;
CARDINAL Nr;
# endif
{
  Strings_tString NewId, String;

  Strings_AssignEmpty(&NewId);
  Idents_GetString(Ident, &String);
  Strings_Concatenate(&NewId, &String);
  Strings_Append(&NewId, '_');
  Strings_IntToString((LONGINT)Nr, &String);
  Strings_Concatenate(&NewId, &String);
  return Idents_MakeIdent(&NewId);
}

void BEGIN_GenIdents()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Idents();
    BEGIN_Idents();
    BEGIN_Strings();

    RenameCnt = 0;
    LabelCnt = 0;
    ParamCnt = 0;
    WithCnt = 0;
    StructCnt = 0;
    GlobalPtrCnt = 0;
    LocalPtrCnt = 0;
    BoundCnt = 0;
    ReturnCnt = 0;
    StringCnt = 0;
    Strings_ArrayToString((STRING)"EXIT", 4L, &String);
    IdentEXIT = Idents_MakeIdent(&String);
  }
}
