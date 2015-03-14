#define DEFINITION_Scanner

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Tokens
#include "Tokens.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

typedef struct Scanner_1 {
    Positions_tPosition Position;
    union {
        struct {
            Idents_tIdent Ident;
        } V_1;
        struct {
            CARDINAL IntValue;
        } V_2;
        struct {
            CHAR CharValue;
        } V_3;
        struct {
            StringMem_tStringRef RealValue;
        } V_4;
        struct {
            StringMem_tStringRef StringValue;
        } V_5;
    } U_1;
} Scanner_tScanAttribute;
extern void Scanner_ErrorAttribute ARGS((CARDINAL Token, Scanner_tScanAttribute *Attribute));
#define Scanner_EofToken	0
extern INTEGER Scanner_TokenLength;
extern Scanner_tScanAttribute Scanner_Attribute;
extern struct Scanner_2 {
    CHAR A[127 + 1];
} Scanner_ScanTabName;
extern PROC Scanner_Exit;
extern void Scanner_BeginScanner ARGS(());
extern void Scanner_BeginFile ARGS((CHAR FileName[], LONGCARD ));
extern INTEGER Scanner_GetToken ARGS(());
extern void Scanner_GetWord ARGS((Strings_tString *Word));
extern void Scanner_GetLower ARGS((Strings_tString *Word));
extern void Scanner_GetUpper ARGS((Strings_tString *Word));
extern void Scanner_CloseFile ARGS(());
extern void Scanner_CloseScanner ARGS(());
extern void BEGIN_Scanner();
