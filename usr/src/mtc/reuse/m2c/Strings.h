#define DEFINITION_Strings

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#define Strings_cMaxStrLength	255
typedef SHORTCARD Strings_tStringIndex;
typedef struct Strings_1 {
    struct Strings_2 {
        CHAR A[Strings_cMaxStrLength + 1];
    } Chars;
    Strings_tStringIndex Length;
} Strings_tString;
extern void Strings_Assign ARGS((Strings_tString *s1, Strings_tString *s2));
extern void Strings_AssignEmpty ARGS((Strings_tString *s));
extern void Strings_Concatenate ARGS((Strings_tString *s1, Strings_tString *s2));
extern void Strings_Append ARGS((Strings_tString *s, CHAR c));
extern CARDINAL Strings_Length ARGS((Strings_tString *s));
extern BOOLEAN Strings_IsEqual ARGS((Strings_tString *s1, Strings_tString *s2));
extern BOOLEAN Strings_IsInOrder ARGS((Strings_tString *s1, Strings_tString *s2));
extern void Strings_Exchange ARGS((Strings_tString *s1, Strings_tString *s2));
extern void Strings_SubString ARGS((Strings_tString *s1, Strings_tStringIndex from, Strings_tStringIndex to, Strings_tString *s2));
extern CHAR Strings_Char ARGS((Strings_tString *s, Strings_tStringIndex i));
extern void Strings_ArrayToString ARGS((CHAR a[], LONGCARD , Strings_tString *s));
extern void Strings_StringToArray ARGS((Strings_tString *s, CHAR a[], LONGCARD ));
extern INTEGER Strings_StringToInt ARGS((Strings_tString *s));
extern CARDINAL Strings_StringToNumber ARGS((Strings_tString *s, CARDINAL Base));
extern REAL Strings_StringToReal ARGS((Strings_tString *s));
extern void Strings_IntToString ARGS((INTEGER n, Strings_tString *s));
extern void Strings_ReadS ARGS((IO_tFile f, Strings_tString *s, Strings_tStringIndex FieldWidth));
extern void Strings_ReadL ARGS((IO_tFile f, Strings_tString *s));
extern void Strings_WriteS ARGS((IO_tFile f, Strings_tString *s));
extern void Strings_WriteL ARGS((IO_tFile f, Strings_tString *s));
extern void BEGIN_Strings();
