#include "SYSTEM_.h"

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_Heap
#include "Heap.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_Lists
#include "Lists.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Texts
#include "Texts.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_SetsC
#include "SetsC.h"
#endif

#ifndef DEFINITION_Relations
#include "Relations.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_StdIO
#include "StdIO.h"
#endif

#ifndef DEFINITION_Layout
#include "Layout.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Sort
#include "Sort.h"
#endif

#ifndef DEFINITION_Source
#include "Source.h"
#endif

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_Checks
#include "Checks.h"
#endif

#ifndef DEFINITION_Times
#include "Times.h"
#endif



void BEGIN_MODULE()
{
  BEGIN_General();
  BEGIN_Memory();
  BEGIN_Heap();
  BEGIN_DynArray();
  BEGIN_Lists();
  BEGIN_Strings();
  BEGIN_Texts();
  BEGIN_StringMem();
  BEGIN_Idents();
  BEGIN_Sets();
  BEGIN_SetsC();
  BEGIN_Relations();
  BEGIN_IO();
  BEGIN_StdIO();
  BEGIN_Layout();
  BEGIN_Positions();
  BEGIN_Errors();
  BEGIN_Sort();
  BEGIN_Source();
  BEGIN_System();
  BEGIN_Checks();
  BEGIN_Times();

}
