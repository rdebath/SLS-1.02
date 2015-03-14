/* $Id: Arguments.h,v 1.3 1991/12/05 18:09:30 grosch rel $ */

#define DEFINITION_Arguments

typedef struct Arguments_1 {
    struct Arguments_2 {
        CHAR A[999 + 1];
    } *A[999 + 1];
} *Arguments_ArgTable;

extern void GetArgs ARGS((SHORTCARD *argc, Arguments_ArgTable *argv));
extern void GetEnv ARGS((Arguments_ArgTable *env));
extern void BEGIN_Arguments();
