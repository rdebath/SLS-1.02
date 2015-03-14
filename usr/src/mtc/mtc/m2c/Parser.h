#define DEFINITION_Parser

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#define Parser_RelOpr	1
#define Parser_AddOpr	2
#define Parser_MulOpr	3
#define Parser_Block	4
typedef struct Parser_1 {
    union {
        struct {
            SHORTCARD Operator;
        } V_1;
        struct {
            Tree_tTree Decls, Stmts;
        } V_2;
        struct {
            Tree_tTree Tree;
        } V_0;
    } U_1;
} Parser_tParsAttribute;
extern Parser_tParsAttribute Parser_ParsAttribute;
extern struct Parser_2 {
    CHAR A[128 + 1];
} Parser_ParsTabName;
extern INTEGER Parser_Parser ARGS(());
extern void Parser_CloseParser ARGS(());
extern void Parser_xxTokenName ARGS((CARDINAL Token, CHAR Name[], LONGCARD ));
extern void BEGIN_Parser();
