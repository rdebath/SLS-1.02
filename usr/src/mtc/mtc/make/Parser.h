#define DEFINITION_Parser

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

typedef Scanner_tScanAttribute Parser_tParsAttribute;
extern Parser_tParsAttribute Parser_ParsAttribute;
extern struct Parser_1 {
    CHAR A[128 + 1];
} Parser_ParsTabName;
extern INTEGER Parser_Parser ARGS(());
extern void Parser_CloseParser ARGS(());
extern void Parser_xxTokenName ARGS((CARDINAL Token, CHAR Name[], LONGCARD ));
extern void BEGIN_Parser();
