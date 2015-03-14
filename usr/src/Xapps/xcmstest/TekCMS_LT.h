/* $XConsortum: TekCMS_LT.h,v 1.1 91/02/11 19:40:54 dave Exp $ */

#ifndef TEKCMS_LT
#define TEKCMS_LT

#define LIBTEST_CMDTBL	LibTstCmdTbl
#define LIBTEST_COMMENT_CHAR    '#'
#define LIBTEST_PROMPT	"TekCMS > "


extern FuncTableEntry LibTstCmdTbl[] ;






extern int
Check_init();
extern int
Cmd_List();
extern int
Cmd_SetInputDir();
extern int
Cmd_SetVerificationDir();
extern int
Cmd_SetResultDir();
extern int
Cmd_GetInputDir();
extern int
Cmd_GetVerificationDir();
extern int
Cmd_GetResultDir();
extern int
Cmd_CreateColormap();
extern int
Cmd_FreeColormap();
extern int
Cmd_quit();
extern int
Cmd_XSynchronize();
extern int
Cmd_AllocColor();
extern int
Cmd_StoreColor();
extern int
Cmd_QueryColor();
extern int
Cmd_QueryColors();
extern int
Cmd_ConvertColor();
extern int
Cmd_ParseColor();
extern int
Cmd_LookupColor();
extern int
Cmd_AllocNamedColor();
extern int
Cmd_StoreNamedColor();
extern int
Cmd_MaxChroma();
extern int
Cmd_MaxValue();
extern int
Cmd_MaxValueSamples();
extern int
Cmd_MaxValueChroma();
extern int
Cmd_MinValue();
extern int
Cmd_AdjustValue();
extern int
Cmd_ReduceChroma();
extern int
Cmd_ShortestValueChroma();
extern int
Cmd_PrefixOfId();
extern int
Cmd_FormatOfPrefix();
extern int
Cmd_AddDIColorSpace();
extern int
Cmd_XAllocNamedColor();
extern int
Cmd_XLookupColor();
extern int
Cmd_XParseColor();
extern int
Cmd_XStoreNamedColor();
extern int
Cmd_StoreColors();
#endif
