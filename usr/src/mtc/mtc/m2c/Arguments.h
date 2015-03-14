/* $Id: Arguments.h,v 1.2 1991/11/21 16:57:59 grosch rel grosch $ */

#define DEFINITION_Arguments

typedef struct Arguments_1 {
    struct Arguments_2 {
        CHAR A[999 + 1];
    } *A[999 + 1];
} *Arguments_ArgTable;

extern void GetArgs ARGS((SHORTCARD *argc, Arguments_ArgTable *argv));
extern void GetEnv ARGS((Arguments_ArgTable *env));
extern void BEGIN_Arguments();
