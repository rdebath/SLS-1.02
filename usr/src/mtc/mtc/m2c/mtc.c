#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Base
#include "Base.h"
#endif

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

#ifndef DEFINITION_Parser
#include "Parser.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_DefMods
#include "DefMods.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Semantics
#include "Semantics.h"
#endif

#ifndef DEFINITION_Code
#include "Code.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Times
#include "Times.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Code
#include "Code.h"
#endif

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

static Strings_tString String, FileNameS;
static CARDINAL ParseErrors;
static BOOLEAN t, m;
static LONGINT StartTime, StopTime;
static void WritePhase ARGS((CHAR Phase[], LONGCARD ));
static void WriteMemory ARGS(());
static void WriteCompleteTime ARGS((CHAR Text[], LONGCARD ));
static void Halt ARGS((CARDINAL Status));


static void WritePhase
# ifdef __STDC__
(CHAR Phase[], LONGCARD O_1)
# else
(Phase, O_1)
CHAR Phase[];
LONGCARD O_1;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Phase, O_1, CHAR)
  IO_WriteS((System_tFile)IO_StdOutput, Phase, O_1);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteFlush((System_tFile)IO_StdOutput);
  FREE_OPEN_ARRAYS
}

static void WriteMemory
# ifdef __STDC__
()
# else
()
# endif
{
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Memory ", 7L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Memory_MemoryUsed, 7L);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"  Heap (Tree) ", 14L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Tree_HeapUsed, 7L);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"  Heap (Defs) ", 14L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Defs_HeapUsed, 7L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteFlush((System_tFile)IO_StdOutput);
}

static void WriteCompleteTime
# ifdef __STDC__
(CHAR Text[], LONGCARD O_2)
# else
(Text, O_2)
CHAR Text[];
LONGCARD O_2;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_2 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Text, O_2, CHAR)
  StopTime = Times_CpuTime();
  IO_WriteS((System_tFile)IO_StdOutput, Text, O_2);
  IO_WriteI((System_tFile)IO_StdOutput, StopTime - StartTime, 5L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  FREE_OPEN_ARRAYS
}

static void TestOutput()
{
  StartTime = Times_CpuTime();
}

static void Halt
# ifdef __STDC__
(CARDINAL Status)
# else
(Status)
CARDINAL Status;
# endif
{
  Errors_PrintMessages(Base_OptionIsSet('w'));
  IO_CloseIO();
  Exit((LONGINT)Status);
}

void BEGIN_MODULE()
{
  BEGIN_System();
  BEGIN_IO();
  BEGIN_Strings();
  BEGIN_Base();
  BEGIN_Scanner();
  BEGIN_Parser();
  BEGIN_Tree();
  BEGIN_DefMods();
  BEGIN_Defs();
  BEGIN_Semantics();
  BEGIN_Code();
  BEGIN_Errors();
  BEGIN_Times();
  BEGIN_Tree();
  BEGIN_Defs();
  BEGIN_Code();
  BEGIN_Memory();
  TestOutput();

  Base_SourceFile.A[0] = '\0';
  Strings_AssignEmpty(&Base_MtcLibrary);
  Base_CheckArguments();
  if (Base_OptionIsSet('h')) {
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"usage: mtc [-options] [file]			", 31L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-w suppress warning diagnostics			", 34L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-i generate header files for imported modules	", 46L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-c generate type casts to make programs lint free", 49L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-r generate runtime checks			", 29L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-h print help information			", 28L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-t print test output (time)			", 30L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-m print test output (memory)			", 32L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-ddir allow import from modules in library dir	", 47L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"-ldir specify directory where mtc finds its tables", 50L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    Halt(0L);
  }
  t = Base_OptionIsSet('t');
  m = Base_OptionIsSet('m');
  Strings_ArrayToString(Scanner_ScanTabName.A, 128L, &FileNameS);
  Strings_Assign(&String, &Base_MtcLibrary);
  Strings_Concatenate(&String, &FileNameS);
  Strings_Append(&String, '\0');
  Strings_StringToArray(&String, Scanner_ScanTabName.A, 128L);
  Strings_ArrayToString(Parser_ParsTabName.A, 129L, &FileNameS);
  Strings_Assign(&String, &Base_MtcLibrary);
  Strings_Concatenate(&String, &FileNameS);
  Strings_Append(&String, '\0');
  Strings_StringToArray(&String, Parser_ParsTabName.A, 129L);
  if (Base_SourceFile.A[0] != '\0') {
    Scanner_BeginFile(Base_SourceFile.A, 128L);
  }
  if (m) {
    WriteMemory();
  }
  if (t || m) {
    WritePhase((STRING)"Parser", 6L);
  }
  ParseErrors = Parser_Parser();
  if (t) {
    Times_WriteStepTime((STRING)"Parser ", 7L);
  }
  if (Errors_NumberOfErrors() == 0) {
    if (m) {
      WriteMemory();
    }
    if (t || m) {
      WritePhase((STRING)"GetDefinitionModules", 20L);
    }
    DefMods_GetDefinitionModules(Parser_ParsAttribute.U_1.V_0.Tree, &Tree_TreeRoot);
    if (t) {
      Times_WriteStepTime((STRING)"GetDefinitionModules  ", 22L);
    }
  }
  Parser_CloseParser();
  if (Errors_NumberOfErrors() == 0) {
    if (m) {
      WriteMemory();
    }
    if (t || m) {
      WritePhase((STRING)"Eval", 4L);
    }
    Semantics_BeginSemantics();
    Semantics_Eval(Tree_TreeRoot);
    Semantics_CloseSemantics();
    if (t) {
      Times_WriteStepTime((STRING)"Eval  ", 6L);
    }
    if (m) {
      WriteMemory();
    }
    if (t || m) {
      WritePhase((STRING)"Code", 4L);
    }
    Code_BeginCode();
    Code_CodeCompUnits(Tree_TreeRoot);
    Code_CloseCode();
    if (t) {
      Times_WriteStepTime((STRING)"Code  ", 6L);
    }
  }
  Tree_CloseTree();
  Defs_CloseDefs();
  if (t) {
    WriteCompleteTime((STRING)"mtc  ", 5L);
  }
  if (m) {
    WriteMemory();
  }
  if (Errors_NumberOfErrors() != 0) {
    Halt(1L);
  } else {
    Halt(0L);
  }
}
