#define DEFINITION_IO

#ifndef DEFINITION_System
#include "System.h"
#endif

#define IO_StdInput	System_StdInput
#define IO_StdOutput	System_StdOutput
#define IO_StdError	System_StdError
typedef System_tFile IO_tFile;
extern IO_tFile IO_ReadOpen ARGS((CHAR FileName[], LONGCARD ));
extern void IO_ReadClose ARGS((IO_tFile f));
extern INTEGER IO_Read ARGS((IO_tFile f, ADDRESS Buffer, CARDINAL Size));
extern CHAR IO_ReadC ARGS((IO_tFile f));
extern INTEGER IO_ReadI ARGS((IO_tFile f));
extern REAL IO_ReadR ARGS((IO_tFile f));
extern BOOLEAN IO_ReadB ARGS((IO_tFile f));
extern INTEGER IO_ReadN ARGS((IO_tFile f, INTEGER Base));
extern void IO_ReadS ARGS((IO_tFile f, CHAR s[], LONGCARD ));
extern SHORTINT IO_ReadShort ARGS((IO_tFile f));
extern LONGINT IO_ReadLong ARGS((IO_tFile f));
extern CARDINAL IO_ReadCard ARGS((IO_tFile f));
extern void IO_ReadNl ARGS((IO_tFile f));
extern void IO_UnRead ARGS((IO_tFile f));
extern BOOLEAN IO_EndOfLine ARGS((IO_tFile f));
extern BOOLEAN IO_EndOfFile ARGS((IO_tFile f));
extern IO_tFile IO_WriteOpen ARGS((CHAR FileName[], LONGCARD ));
extern void IO_WriteClose ARGS((IO_tFile f));
extern void IO_WriteFlush ARGS((IO_tFile f));
extern INTEGER IO_Write ARGS((IO_tFile f, ADDRESS Buffer, INTEGER Size));
extern void IO_WriteC ARGS((IO_tFile f, CHAR c));
extern void IO_WriteI ARGS((IO_tFile f, INTEGER n, CARDINAL FieldWidth));
extern void IO_WriteR ARGS((IO_tFile f, REAL n, CARDINAL Before, CARDINAL After, CARDINAL Exp));
extern void IO_WriteB ARGS((IO_tFile f, BOOLEAN b));
extern void IO_WriteN ARGS((IO_tFile f, LONGCARD n, CARDINAL FieldWidth, CARDINAL Base));
extern void IO_WriteS ARGS((IO_tFile f, CHAR s[], LONGCARD ));
extern void IO_WriteShort ARGS((IO_tFile f, SHORTINT n, CARDINAL FieldWidth));
extern void IO_WriteLong ARGS((IO_tFile f, LONGINT n, CARDINAL FieldWidth));
extern void IO_WriteCard ARGS((IO_tFile f, CARDINAL n, CARDINAL FieldWidth));
extern void IO_WriteNl ARGS((IO_tFile f));
extern void IO_CloseIO ARGS(());
extern void BEGIN_IO();
