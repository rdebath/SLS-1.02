#define DEFINITION_Lists

#ifndef DEFINITION_IO
#include "IO.h"
#endif

typedef ADDRESS Lists_tElmt;
typedef struct Lists_1 *Lists_tListElmtPtr;
typedef struct Lists_1 {
    Lists_tListElmtPtr Succ;
    Lists_tElmt Elmt;
} Lists_tListElmt;
typedef struct Lists_2 {
    Lists_tListElmtPtr FirstElmt, LastElmt;
} Lists_tList;
typedef void (*Lists_tProcOfFileAddress) ARGS((IO_tFile, Lists_tElmt));
extern void Lists_MakeList ARGS((Lists_tList *List));
extern void Lists_Insert ARGS((Lists_tList *List, Lists_tElmt Elmt));
extern void Lists_Append ARGS((Lists_tList *List, Lists_tElmt Elmt));
extern Lists_tElmt Lists_Head ARGS((Lists_tList List));
extern void Lists_Tail ARGS((Lists_tList *List));
extern Lists_tElmt Lists_Last ARGS((Lists_tList List));
extern void Lists_Front ARGS((Lists_tList *List));
extern BOOLEAN Lists_IsEmpty ARGS((Lists_tList List));
extern CARDINAL Lists_Length ARGS((Lists_tList List));
extern void Lists_WriteList ARGS((IO_tFile f, Lists_tList List, Lists_tProcOfFileAddress Proc));
extern void BEGIN_Lists();
