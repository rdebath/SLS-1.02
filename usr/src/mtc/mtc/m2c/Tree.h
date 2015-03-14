#define DEFINITION_Tree

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#ifndef DEFINITION_UniqueIds
#include "UniqueIds.h"
#endif

typedef ADDRESS Tree_yyEstra;
#define Tree_NoTree	NIL
#define Tree_ROOT	1
#define Tree_CompUnits	2
#define Tree_CompUnits0	3
#define Tree_CompUnit	4
#define Tree_DefMod	5
#define Tree_ProgMod	6
#define Tree_Import	7
#define Tree_Import0	8
#define Tree_Import1	9
#define Tree_From	10
#define Tree_Objects	11
#define Tree_ImpIds	12
#define Tree_ImpIds0	13
#define Tree_ImpIds1	14
#define Tree_Export	15
#define Tree_Export0	16
#define Tree_Export1	17
#define Tree_ExpIds	18
#define Tree_ExpIds0	19
#define Tree_ExpIds1	20
#define Tree_Decls	21
#define Tree_Decls0	22
#define Tree_Decl	23
#define Tree_Var	24
#define Tree_Object	25
#define Tree_Const	26
#define Tree_TypeDecl	27
#define Tree_Proc	28
#define Tree_ProcHead	29
#define Tree_Module	30
#define Tree_Opaque	31
#define Tree_VarIds	32
#define Tree_VarIds0	33
#define Tree_VarIds1	34
#define Tree_Formals	35
#define Tree_Formals0	36
#define Tree_Formals1	37
#define Tree_ParIds	38
#define Tree_ParIds0	39
#define Tree_ParIds1	40
#define Tree_Type	41
#define Tree_Array	42
#define Tree_Record	43
#define Tree_SetType	44
#define Tree_Pointer	45
#define Tree_ProcType	46
#define Tree_SimpleType	47
#define Tree_Enumeration	48
#define Tree_Subrange	49
#define Tree_PrimaryType	50
#define Tree_Void	51
#define Tree_TypeId	52
#define Tree_TypeId0	53
#define Tree_TypeId1	54
#define Tree_Fields	55
#define Tree_Fields0	56
#define Tree_Fields1	57
#define Tree_RecordSect	58
#define Tree_VariantSect	59
#define Tree_FieldIds	60
#define Tree_FieldIds0	61
#define Tree_FieldIds1	62
#define Tree_TagField	63
#define Tree_TagField0	64
#define Tree_TagField1	65
#define Tree_Variants	66
#define Tree_Variants0	67
#define Tree_Variant	68
#define Tree_FormalTypes	69
#define Tree_FormalTypes0	70
#define Tree_FormalType	71
#define Tree_EnumIds	72
#define Tree_EnumIds0	73
#define Tree_EnumIds1	74
#define Tree_Expr	75
#define Tree_Binary	76
#define Tree_Unary	77
#define Tree_IntConst	78
#define Tree_RealConst	79
#define Tree_StringConst	80
#define Tree_CharConst	81
#define Tree_FuncCall	82
#define Tree_Set	83
#define Tree_BitSet	84
#define Tree_Designator	85
#define Tree_Qualid	86
#define Tree_Qualid0	87
#define Tree_Qualid1	88
#define Tree_Subscript	89
#define Tree_Deref	90
#define Tree_Select	91
#define Tree_Elems	92
#define Tree_Elems0	93
#define Tree_Elems1	94
#define Tree_Elem	95
#define Tree_ElemRange	96
#define Tree_Actuals	97
#define Tree_Actuals0	98
#define Tree_Actual	99
#define Tree_Stmts	100
#define Tree_Stmts0	101
#define Tree_Stmt	102
#define Tree_Assign	103
#define Tree_Call	104
#define Tree_If	105
#define Tree_Case	106
#define Tree_While	107
#define Tree_Repeat	108
#define Tree_Loop	109
#define Tree_For	110
#define Tree_With	111
#define Tree_Exit	112
#define Tree_Return1	113
#define Tree_Return2	114
#define Tree_Elsifs	115
#define Tree_Elsifs0	116
#define Tree_Elsifs1	117
#define Tree_Cases	118
#define Tree_Cases0	119
#define Tree_Cases1	120
#define Tree_Labels	121
#define Tree_Labels0	122
#define Tree_Labels1	123
#define Tree_Label	124
#define Tree_LabelRange	125
typedef struct Tree_127 *Tree_tTree;
typedef void (*Tree_tProcTree) ARGS((Tree_tTree));
#define Tree_Definition	1
#define Tree_Foreign	2
#define Tree_Implementation	3
#define Tree_Program	4
#define Tree_NotEqual	1
#define Tree_Times	2
#define Tree_Plus	3
#define Tree_Minus	4
#define Tree_Divide	5
#define Tree_Less	6
#define Tree_LessEqual	7
#define Tree_Equal	8
#define Tree_Greater	9
#define Tree_GreaterEqual	10
#define Tree_And	11
#define Tree_Div	12
#define Tree_In	13
#define Tree_Mod	14
#define Tree_Not	15
#define Tree_Or	16
#define Tree_Decimal	1
#define Tree_Octal	2
#define Tree_Hexadecimal	3
typedef struct Tree_1 {
    SHORTCARD yyKind, yyMark;
} Tree_yytNodeHead;
typedef struct Tree_2 {
    Tree_yytNodeHead yyHead;
    Tree_tTree CompUnits;
} Tree_yROOT;
typedef struct Tree_3 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    SHORTCARD PosIn;
} Tree_yCompUnits;
typedef struct Tree_4 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    SHORTCARD PosIn;
} Tree_yCompUnits0;
typedef struct Tree_5 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    SHORTCARD PosIn;
    SHORTCARD Kind;
    Idents_tIdent Ident;
    Positions_tPosition Pos;
    Tree_tTree Next;
    Defs_tObject Object;
} Tree_yCompUnit;
typedef struct Tree_6 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    SHORTCARD PosIn;
    SHORTCARD Kind;
    Idents_tIdent Ident;
    Positions_tPosition Pos;
    Tree_tTree Next;
    Defs_tObject Object;
    Tree_tTree Import;
    Tree_tTree Decls;
} Tree_yDefMod;
typedef struct Tree_7 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    SHORTCARD PosIn;
    SHORTCARD Kind;
    Idents_tIdent Ident;
    Positions_tPosition Pos;
    Tree_tTree Next;
    Defs_tObject Object;
    Tree_tTree Import;
    Tree_tTree Decls;
    Tree_tTree Stmts;
    Defs_tCObjects GlobalPtrs;
    Defs_tStrings Strings;
} Tree_yProgMod;
typedef struct Tree_8 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects2;
    Defs_tEnv Env1;
} Tree_yImport;
typedef struct Tree_9 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects2;
    Defs_tEnv Env1;
} Tree_yImport0;
typedef struct Tree_10 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects2;
    Defs_tEnv Env1;
    Tree_tTree Next;
} Tree_yImport1;
typedef struct Tree_11 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects2;
    Defs_tEnv Env1;
    Tree_tTree Next;
    Idents_tIdent Ident;
    Positions_tPosition Pos;
    Tree_tTree ImpIds;
} Tree_yFrom;
typedef struct Tree_12 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects2;
    Defs_tEnv Env1;
    Tree_tTree Next;
    Tree_tTree ImpIds;
} Tree_yObjects;
typedef struct Tree_13 {
    Tree_yytNodeHead yyHead;
} Tree_yImpIds;
typedef struct Tree_14 {
    Tree_yytNodeHead yyHead;
} Tree_yImpIds0;
typedef struct Tree_15 {
    Tree_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Positions_tPosition Pos;
    Tree_tTree Next;
} Tree_yImpIds1;
typedef struct Tree_16 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects2;
    BOOLEAN IsQualified;
} Tree_yExport;
typedef struct Tree_17 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects2;
    BOOLEAN IsQualified;
} Tree_yExport0;
typedef struct Tree_18 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects2;
    BOOLEAN IsQualified;
    BOOLEAN Qualified;
    Tree_tTree ExpIds;
} Tree_yExport1;
typedef struct Tree_19 {
    Tree_yytNodeHead yyHead;
} Tree_yExpIds;
typedef struct Tree_20 {
    Tree_yytNodeHead yyHead;
} Tree_yExpIds0;
typedef struct Tree_21 {
    Tree_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Tree_tTree Next;
    Defs_tObject Object1;
    Defs_tObject Object2;
} Tree_yExpIds1;
typedef struct Tree_22 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
} Tree_yDecls;
typedef struct Tree_23 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
} Tree_yDecls0;
typedef struct Tree_24 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
} Tree_yDecl;
typedef struct Tree_25 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
    Tree_tTree VarIds;
    Tree_tTree Type;
    BOOLEAN IsGlobal;
} Tree_yVar;
typedef struct Tree_26 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
} Tree_yObject;
typedef struct Tree_27 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Tree_tTree Expr;
    Defs_tObject Object;
} Tree_yConst;
typedef struct Tree_28 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Tree_tTree Type;
    Positions_tPosition Pos;
    Defs_tObject Object;
    Defs_tType Type1;
} Tree_yTypeDecl;
typedef struct Tree_29 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Tree_tTree Formals;
    Tree_tTree ResultType;
    Tree_tTree Decls;
    Tree_tTree Stmts;
    Defs_tObject Object;
    Defs_tType Type;
    Defs_tCObjects ValueOpens;
    Defs_tCObjects VAROpens;
    Defs_tCObjects LocalPtrs;
    Defs_tStrings Strings;
} Tree_yProc;
typedef struct Tree_30 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Tree_tTree Formals;
    Tree_tTree ResultType;
    Positions_tPosition Pos;
    Defs_tObject Object;
} Tree_yProcHead;
typedef struct Tree_31 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Tree_tTree Import;
    Tree_tTree Export;
    Tree_tTree Decls;
    Tree_tTree Stmts;
    Defs_tObject Object;
    Defs_tStrings Strings;
} Tree_yModule;
typedef struct Tree_32 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects1;
    Defs_tObjects Objects3;
    Defs_tObjects Objects4In;
    Defs_tObjects Objects4Out;
    Defs_tObjects DefTypes;
    Defs_tObjects DefObjects;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Defs_tEnv Env3;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    SHORTCARD Level;
    Tree_tTree Next;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Defs_tObject Object;
    Defs_tType Type1;
} Tree_yOpaque;
typedef struct Tree_33 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
} Tree_yVarIds;
typedef struct Tree_34 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
} Tree_yVarIds0;
typedef struct Tree_35 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Idents_tIdent Ident;
    Tree_tTree Next;
    Defs_tObject Object;
    Idents_tIdent CIdent;
} Tree_yVarIds1;
typedef struct Tree_36 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
} Tree_yFormals;
typedef struct Tree_37 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
} Tree_yFormals0;
typedef struct Tree_38 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    BOOLEAN IsVAR;
    Tree_tTree ParIds;
    Tree_tTree Type;
    Tree_tTree Next;
} Tree_yFormals1;
typedef struct Tree_39 {
    Tree_yytNodeHead yyHead;
} Tree_yParIds;
typedef struct Tree_40 {
    Tree_yytNodeHead yyHead;
} Tree_yParIds0;
typedef struct Tree_41 {
    Tree_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Tree_tTree Next;
    Defs_tObject Object;
    Idents_tIdent CIdent;
} Tree_yParIds1;
typedef struct Tree_42 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
} Tree_yType;
typedef struct Tree_43 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    BOOLEAN IsOpen;
    Tree_tTree IndexType;
    Tree_tTree ElemType;
} Tree_yArray;
typedef struct Tree_44 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Tree_tTree Fields;
} Tree_yRecord;
typedef struct Tree_45 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Tree_tTree BaseType;
} Tree_ySetType;
typedef struct Tree_46 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Tree_tTree TargetType;
    SHORTCARD TypePos;
} Tree_yPointer;
typedef struct Tree_47 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Tree_tTree FormalTypes;
    Tree_tTree ResultType;
} Tree_yProcType;
typedef struct Tree_48 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
} Tree_ySimpleType;
typedef struct Tree_49 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Tree_tTree EnumIds;
} Tree_yEnumeration;
typedef struct Tree_50 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Tree_tTree BaseType;
    Tree_tTree Lwb;
    Tree_tTree Upb;
} Tree_ySubrange;
typedef struct Tree_51 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
} Tree_yPrimaryType;
typedef struct Tree_52 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
} Tree_yVoid;
typedef struct Tree_53 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Idents_tIdent Ident;
    Positions_tPosition Pos;
    Defs_tObject Object;
} Tree_yTypeId;
typedef struct Tree_54 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Idents_tIdent Ident;
    Positions_tPosition Pos;
    Defs_tObject Object;
} Tree_yTypeId0;
typedef struct Tree_55 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects3;
    Defs_tType Type1;
    Defs_tType Type2;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    SHORTCARD Kind;
    Idents_tIdent Module;
    SHORTCARD CntIn;
    Idents_tIdent Ident;
    Positions_tPosition Pos;
    Defs_tObject Object;
    Tree_tTree TypeId;
} Tree_yTypeId1;
typedef struct Tree_56 {
    Tree_yytNodeHead yyHead;
} Tree_yFields;
typedef struct Tree_57 {
    Tree_yytNodeHead yyHead;
} Tree_yFields0;
typedef struct Tree_58 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
} Tree_yFields1;
typedef struct Tree_59 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree FieldIds;
    Tree_tTree Type;
} Tree_yRecordSect;
typedef struct Tree_60 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree TagField;
    Tree_tTree Variants;
    Tree_tTree Else;
    Idents_tIdent UnionId;
    Idents_tIdent ElseId;
} Tree_yVariantSect;
typedef struct Tree_61 {
    Tree_yytNodeHead yyHead;
} Tree_yFieldIds;
typedef struct Tree_62 {
    Tree_yytNodeHead yyHead;
} Tree_yFieldIds0;
typedef struct Tree_63 {
    Tree_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Tree_tTree Next;
    Defs_tObject Object;
    Idents_tIdent CIdent;
} Tree_yFieldIds1;
typedef struct Tree_64 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Type;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
} Tree_yTagField;
typedef struct Tree_65 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Type;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
} Tree_yTagField0;
typedef struct Tree_66 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Type;
    Defs_tEnv Env1;
    Defs_tVoid Env2;
    Idents_tIdent Ident;
    Defs_tObject Object;
    Idents_tIdent CIdent;
} Tree_yTagField1;
typedef struct Tree_67 {
    Tree_yytNodeHead yyHead;
} Tree_yVariants;
typedef struct Tree_68 {
    Tree_yytNodeHead yyHead;
} Tree_yVariants0;
typedef struct Tree_69 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Labels;
    Tree_tTree Variant;
    Tree_tTree Next;
    Idents_tIdent StructId;
} Tree_yVariant;
typedef struct Tree_70 {
    Tree_yytNodeHead yyHead;
} Tree_yFormalTypes;
typedef struct Tree_71 {
    Tree_yytNodeHead yyHead;
} Tree_yFormalTypes0;
typedef struct Tree_72 {
    Tree_yytNodeHead yyHead;
    BOOLEAN IsVAR;
    Tree_tTree Type;
    Tree_tTree Next;
} Tree_yFormalType;
typedef struct Tree_73 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects;
} Tree_yEnumIds;
typedef struct Tree_74 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects;
} Tree_yEnumIds0;
typedef struct Tree_75 {
    Tree_yytNodeHead yyHead;
    Defs_tObjects Objects;
    Idents_tIdent Ident;
    Tree_tTree Next;
    Defs_tObject Object;
    Idents_tIdent CIdent;
} Tree_yEnumIds1;
typedef struct Tree_76 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
} Tree_yExpr;
typedef struct Tree_77 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    SHORTCARD Operator;
    Tree_tTree Lop;
    Tree_tTree Rop;
    SHORTCARD COperator;
} Tree_yBinary;
typedef struct Tree_78 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    SHORTCARD Operator;
    Tree_tTree Mop;
    SHORTCARD COperator;
} Tree_yUnary;
typedef struct Tree_79 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    SHORTCARD Kind;
    CARDINAL IntVal;
    Positions_tPosition Pos;
} Tree_yIntConst;
typedef struct Tree_80 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    StringMem_tStringRef RealVal;
} Tree_yRealConst;
typedef struct Tree_81 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    StringMem_tStringRef StringVal;
} Tree_yStringConst;
typedef struct Tree_82 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    CHAR CharVal;
} Tree_yCharConst;
typedef struct Tree_83 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Tree_tTree Designator;
    Tree_tTree Actuals;
} Tree_yFuncCall;
typedef struct Tree_84 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Tree_tTree BaseType;
    Tree_tTree Elems;
} Tree_ySet;
typedef struct Tree_85 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Tree_tTree Elems;
} Tree_yBitSet;
typedef struct Tree_86 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Positions_tPosition Pos;
} Tree_yDesignator;
typedef struct Tree_87 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Positions_tPosition Pos;
    Idents_tIdent Ident;
    Defs_tObject Object;
    BOOLEAN IsGlobalPtr;
} Tree_yQualid;
typedef struct Tree_88 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Positions_tPosition Pos;
    Idents_tIdent Ident;
    Defs_tObject Object;
    BOOLEAN IsGlobalPtr;
} Tree_yQualid0;
typedef struct Tree_89 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Positions_tPosition Pos;
    Idents_tIdent Ident;
    Defs_tObject Object;
    BOOLEAN IsGlobalPtr;
    Tree_tTree Qualid;
} Tree_yQualid1;
typedef struct Tree_90 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Positions_tPosition Pos;
    Tree_tTree Designator;
    Tree_tTree Index;
} Tree_ySubscript;
typedef struct Tree_91 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Positions_tPosition Pos;
    Tree_tTree Designator;
} Tree_yDeref;
typedef struct Tree_92 {
    Tree_yytNodeHead yyHead;
    Defs_tType Type;
    BOOLEAN IsCConst;
    Positions_tPosition Pos;
    Tree_tTree Designator;
    Idents_tIdent Field;
    Defs_tObject Object;
} Tree_ySelect;
typedef struct Tree_93 {
    Tree_yytNodeHead yyHead;
} Tree_yElems;
typedef struct Tree_94 {
    Tree_yytNodeHead yyHead;
} Tree_yElems0;
typedef struct Tree_95 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
} Tree_yElems1;
typedef struct Tree_96 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Elem;
} Tree_yElem;
typedef struct Tree_97 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Lwb;
    Tree_tTree Upb;
} Tree_yElemRange;
typedef struct Tree_98 {
    Tree_yytNodeHead yyHead;
} Tree_yActuals;
typedef struct Tree_99 {
    Tree_yytNodeHead yyHead;
} Tree_yActuals0;
typedef struct Tree_100 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Expr;
    Tree_tTree Next;
    BOOLEAN IsVAR;
    Defs_tType Formal;
    Idents_tIdent String;
} Tree_yActual;
typedef struct Tree_101 {
    Tree_yytNodeHead yyHead;
} Tree_yStmts;
typedef struct Tree_102 {
    Tree_yytNodeHead yyHead;
} Tree_yStmts0;
typedef struct Tree_103 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
} Tree_yStmt;
typedef struct Tree_104 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Designator;
    Tree_tTree Expr;
} Tree_yAssign;
typedef struct Tree_105 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Designator;
    Tree_tTree Actuals;
    Defs_tObject AllocOrDealloc;
} Tree_yCall;
typedef struct Tree_106 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Cond;
    Tree_tTree Then;
    Tree_tTree Elsifs;
    Tree_tTree Else;
} Tree_yIf;
typedef struct Tree_107 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Expr;
    Tree_tTree Cases;
    Tree_tTree Else;
    BOOLEAN Default;
} Tree_yCase;
typedef struct Tree_108 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Cond;
    Tree_tTree Stmts;
} Tree_yWhile;
typedef struct Tree_109 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Stmts;
    Tree_tTree Cond;
} Tree_yRepeat;
typedef struct Tree_110 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Stmts;
} Tree_yLoop;
typedef struct Tree_111 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Qualid;
    Tree_tTree From;
    Tree_tTree To;
    Tree_tTree By;
    Tree_tTree Stmts;
    Values_tValue ToVal;
    Values_tValue ByVal;
} Tree_yFor;
typedef struct Tree_112 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Designator;
    Tree_tTree Stmts;
} Tree_yWith;
typedef struct Tree_113 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
} Tree_yExit;
typedef struct Tree_114 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
} Tree_yReturn1;
typedef struct Tree_115 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Result;
    BOOLEAN OpenAccessOrCall;
    Defs_tType ResultType;
} Tree_yReturn2;
typedef struct Tree_116 {
    Tree_yytNodeHead yyHead;
} Tree_yElsifs;
typedef struct Tree_117 {
    Tree_yytNodeHead yyHead;
} Tree_yElsifs0;
typedef struct Tree_118 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Cond;
    Tree_tTree Stmts;
    Tree_tTree Next;
} Tree_yElsifs1;
typedef struct Tree_119 {
    Tree_yytNodeHead yyHead;
} Tree_yCases;
typedef struct Tree_120 {
    Tree_yytNodeHead yyHead;
} Tree_yCases0;
typedef struct Tree_121 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Labels;
    Tree_tTree Stmts;
    Tree_tTree Next;
} Tree_yCases1;
typedef struct Tree_122 {
    Tree_yytNodeHead yyHead;
} Tree_yLabels;
typedef struct Tree_123 {
    Tree_yytNodeHead yyHead;
} Tree_yLabels0;
typedef struct Tree_124 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
} Tree_yLabels1;
typedef struct Tree_125 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Label;
    Values_tValue LabelVal;
} Tree_yLabel;
typedef struct Tree_126 {
    Tree_yytNodeHead yyHead;
    Tree_tTree Next;
    Tree_tTree Lwb;
    Tree_tTree Upb;
    Values_tValue LwbVal;
    Values_tValue UpbVal;
} Tree_yLabelRange;
typedef struct Tree_127 {
    union {
        struct {
            SHORTCARD Kind;
        } V_1;
        struct {
            Tree_yytNodeHead yyHead;
        } V_2;
        struct {
            Tree_yROOT ROOT;
        } V_3;
        struct {
            Tree_yCompUnits CompUnits;
        } V_4;
        struct {
            Tree_yCompUnits0 CompUnits0;
        } V_5;
        struct {
            Tree_yCompUnit CompUnit;
        } V_6;
        struct {
            Tree_yDefMod DefMod;
        } V_7;
        struct {
            Tree_yProgMod ProgMod;
        } V_8;
        struct {
            Tree_yImport Import;
        } V_9;
        struct {
            Tree_yImport0 Import0;
        } V_10;
        struct {
            Tree_yImport1 Import1;
        } V_11;
        struct {
            Tree_yFrom From;
        } V_12;
        struct {
            Tree_yObjects Objects;
        } V_13;
        struct {
            Tree_yImpIds ImpIds;
        } V_14;
        struct {
            Tree_yImpIds0 ImpIds0;
        } V_15;
        struct {
            Tree_yImpIds1 ImpIds1;
        } V_16;
        struct {
            Tree_yExport Export;
        } V_17;
        struct {
            Tree_yExport0 Export0;
        } V_18;
        struct {
            Tree_yExport1 Export1;
        } V_19;
        struct {
            Tree_yExpIds ExpIds;
        } V_20;
        struct {
            Tree_yExpIds0 ExpIds0;
        } V_21;
        struct {
            Tree_yExpIds1 ExpIds1;
        } V_22;
        struct {
            Tree_yDecls Decls;
        } V_23;
        struct {
            Tree_yDecls0 Decls0;
        } V_24;
        struct {
            Tree_yDecl Decl;
        } V_25;
        struct {
            Tree_yVar Var;
        } V_26;
        struct {
            Tree_yObject Object;
        } V_27;
        struct {
            Tree_yConst Const;
        } V_28;
        struct {
            Tree_yTypeDecl TypeDecl;
        } V_29;
        struct {
            Tree_yProc Proc;
        } V_30;
        struct {
            Tree_yProcHead ProcHead;
        } V_31;
        struct {
            Tree_yModule Module;
        } V_32;
        struct {
            Tree_yOpaque Opaque;
        } V_33;
        struct {
            Tree_yVarIds VarIds;
        } V_34;
        struct {
            Tree_yVarIds0 VarIds0;
        } V_35;
        struct {
            Tree_yVarIds1 VarIds1;
        } V_36;
        struct {
            Tree_yFormals Formals;
        } V_37;
        struct {
            Tree_yFormals0 Formals0;
        } V_38;
        struct {
            Tree_yFormals1 Formals1;
        } V_39;
        struct {
            Tree_yParIds ParIds;
        } V_40;
        struct {
            Tree_yParIds0 ParIds0;
        } V_41;
        struct {
            Tree_yParIds1 ParIds1;
        } V_42;
        struct {
            Tree_yType Type;
        } V_43;
        struct {
            Tree_yArray Array;
        } V_44;
        struct {
            Tree_yRecord Record;
        } V_45;
        struct {
            Tree_ySetType SetType;
        } V_46;
        struct {
            Tree_yPointer Pointer;
        } V_47;
        struct {
            Tree_yProcType ProcType;
        } V_48;
        struct {
            Tree_ySimpleType SimpleType;
        } V_49;
        struct {
            Tree_yEnumeration Enumeration;
        } V_50;
        struct {
            Tree_ySubrange Subrange;
        } V_51;
        struct {
            Tree_yPrimaryType PrimaryType;
        } V_52;
        struct {
            Tree_yVoid Void;
        } V_53;
        struct {
            Tree_yTypeId TypeId;
        } V_54;
        struct {
            Tree_yTypeId0 TypeId0;
        } V_55;
        struct {
            Tree_yTypeId1 TypeId1;
        } V_56;
        struct {
            Tree_yFields Fields;
        } V_57;
        struct {
            Tree_yFields0 Fields0;
        } V_58;
        struct {
            Tree_yFields1 Fields1;
        } V_59;
        struct {
            Tree_yRecordSect RecordSect;
        } V_60;
        struct {
            Tree_yVariantSect VariantSect;
        } V_61;
        struct {
            Tree_yFieldIds FieldIds;
        } V_62;
        struct {
            Tree_yFieldIds0 FieldIds0;
        } V_63;
        struct {
            Tree_yFieldIds1 FieldIds1;
        } V_64;
        struct {
            Tree_yTagField TagField;
        } V_65;
        struct {
            Tree_yTagField0 TagField0;
        } V_66;
        struct {
            Tree_yTagField1 TagField1;
        } V_67;
        struct {
            Tree_yVariants Variants;
        } V_68;
        struct {
            Tree_yVariants0 Variants0;
        } V_69;
        struct {
            Tree_yVariant Variant;
        } V_70;
        struct {
            Tree_yFormalTypes FormalTypes;
        } V_71;
        struct {
            Tree_yFormalTypes0 FormalTypes0;
        } V_72;
        struct {
            Tree_yFormalType FormalType;
        } V_73;
        struct {
            Tree_yEnumIds EnumIds;
        } V_74;
        struct {
            Tree_yEnumIds0 EnumIds0;
        } V_75;
        struct {
            Tree_yEnumIds1 EnumIds1;
        } V_76;
        struct {
            Tree_yExpr Expr;
        } V_77;
        struct {
            Tree_yBinary Binary;
        } V_78;
        struct {
            Tree_yUnary Unary;
        } V_79;
        struct {
            Tree_yIntConst IntConst;
        } V_80;
        struct {
            Tree_yRealConst RealConst;
        } V_81;
        struct {
            Tree_yStringConst StringConst;
        } V_82;
        struct {
            Tree_yCharConst CharConst;
        } V_83;
        struct {
            Tree_yFuncCall FuncCall;
        } V_84;
        struct {
            Tree_ySet Set;
        } V_85;
        struct {
            Tree_yBitSet BitSet;
        } V_86;
        struct {
            Tree_yDesignator Designator;
        } V_87;
        struct {
            Tree_yQualid Qualid;
        } V_88;
        struct {
            Tree_yQualid0 Qualid0;
        } V_89;
        struct {
            Tree_yQualid1 Qualid1;
        } V_90;
        struct {
            Tree_ySubscript Subscript;
        } V_91;
        struct {
            Tree_yDeref Deref;
        } V_92;
        struct {
            Tree_ySelect Select;
        } V_93;
        struct {
            Tree_yElems Elems;
        } V_94;
        struct {
            Tree_yElems0 Elems0;
        } V_95;
        struct {
            Tree_yElems1 Elems1;
        } V_96;
        struct {
            Tree_yElem Elem;
        } V_97;
        struct {
            Tree_yElemRange ElemRange;
        } V_98;
        struct {
            Tree_yActuals Actuals;
        } V_99;
        struct {
            Tree_yActuals0 Actuals0;
        } V_100;
        struct {
            Tree_yActual Actual;
        } V_101;
        struct {
            Tree_yStmts Stmts;
        } V_102;
        struct {
            Tree_yStmts0 Stmts0;
        } V_103;
        struct {
            Tree_yStmt Stmt;
        } V_104;
        struct {
            Tree_yAssign Assign;
        } V_105;
        struct {
            Tree_yCall Call;
        } V_106;
        struct {
            Tree_yIf If;
        } V_107;
        struct {
            Tree_yCase Case;
        } V_108;
        struct {
            Tree_yWhile While;
        } V_109;
        struct {
            Tree_yRepeat Repeat;
        } V_110;
        struct {
            Tree_yLoop Loop;
        } V_111;
        struct {
            Tree_yFor For;
        } V_112;
        struct {
            Tree_yWith With;
        } V_113;
        struct {
            Tree_yExit Exit;
        } V_114;
        struct {
            Tree_yReturn1 Return1;
        } V_115;
        struct {
            Tree_yReturn2 Return2;
        } V_116;
        struct {
            Tree_yElsifs Elsifs;
        } V_117;
        struct {
            Tree_yElsifs0 Elsifs0;
        } V_118;
        struct {
            Tree_yElsifs1 Elsifs1;
        } V_119;
        struct {
            Tree_yCases Cases;
        } V_120;
        struct {
            Tree_yCases0 Cases0;
        } V_121;
        struct {
            Tree_yCases1 Cases1;
        } V_122;
        struct {
            Tree_yLabels Labels;
        } V_123;
        struct {
            Tree_yLabels0 Labels0;
        } V_124;
        struct {
            Tree_yLabels1 Labels1;
        } V_125;
        struct {
            Tree_yLabel Label;
        } V_126;
        struct {
            Tree_yLabelRange LabelRange;
        } V_127;
    } U_1;
} Tree_yyNode;
extern Tree_tTree Tree_TreeRoot;
extern LONGCARD Tree_HeapUsed;
extern ADDRESS Tree_yyPoolFreePtr, Tree_yyPoolMaxPtr;
extern struct Tree_128 {
    SHORTCARD A[125 + 1];
} Tree_yyNodeSize;
extern PROC Tree_yyExit;
extern Tree_tTree Tree_yyAlloc ARGS(());
extern Tree_tTree Tree_MakeTree ARGS((SHORTCARD Kind));
extern BOOLEAN Tree_IsType ARGS((Tree_tTree Tree, SHORTCARD Kind));
extern Tree_tTree Tree_mROOT ARGS((Tree_tTree pCompUnits));
extern Tree_tTree Tree_mCompUnits ARGS(());
extern Tree_tTree Tree_mCompUnits0 ARGS(());
extern Tree_tTree Tree_mCompUnit ARGS((SHORTCARD pKind, Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pNext));
extern Tree_tTree Tree_mDefMod ARGS((SHORTCARD pKind, Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pNext, Tree_tTree pImport, Tree_tTree pDecls));
extern Tree_tTree Tree_mProgMod ARGS((SHORTCARD pKind, Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pNext, Tree_tTree pImport, Tree_tTree pDecls, Tree_tTree pStmts));
extern Tree_tTree Tree_mImport ARGS(());
extern Tree_tTree Tree_mImport0 ARGS(());
extern Tree_tTree Tree_mImport1 ARGS((Tree_tTree pNext));
extern Tree_tTree Tree_mFrom ARGS((Tree_tTree pNext, Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pImpIds));
extern Tree_tTree Tree_mObjects ARGS((Tree_tTree pNext, Tree_tTree pImpIds));
extern Tree_tTree Tree_mImpIds ARGS(());
extern Tree_tTree Tree_mImpIds0 ARGS(());
extern Tree_tTree Tree_mImpIds1 ARGS((Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pNext));
extern Tree_tTree Tree_mExport ARGS(());
extern Tree_tTree Tree_mExport0 ARGS(());
extern Tree_tTree Tree_mExport1 ARGS((BOOLEAN pQualified, Tree_tTree pExpIds));
extern Tree_tTree Tree_mExpIds ARGS(());
extern Tree_tTree Tree_mExpIds0 ARGS(());
extern Tree_tTree Tree_mExpIds1 ARGS((Idents_tIdent pIdent, Tree_tTree pNext));
extern Tree_tTree Tree_mDecls ARGS(());
extern Tree_tTree Tree_mDecls0 ARGS(());
extern Tree_tTree Tree_mDecl ARGS((Tree_tTree pNext));
extern Tree_tTree Tree_mVar ARGS((Tree_tTree pNext, Tree_tTree pVarIds, Tree_tTree pType));
extern Tree_tTree Tree_mObject ARGS((Tree_tTree pNext, Idents_tIdent pIdent));
extern Tree_tTree Tree_mConst ARGS((Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pExpr));
extern Tree_tTree Tree_mTypeDecl ARGS((Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pType, Positions_tPosition pPos));
extern Tree_tTree Tree_mProc ARGS((Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pFormals, Tree_tTree pResultType, Tree_tTree pDecls, Tree_tTree pStmts));
extern Tree_tTree Tree_mProcHead ARGS((Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pFormals, Tree_tTree pResultType, Positions_tPosition pPos));
extern Tree_tTree Tree_mModule ARGS((Tree_tTree pNext, Idents_tIdent pIdent, Tree_tTree pImport, Tree_tTree pExport, Tree_tTree pDecls, Tree_tTree pStmts));
extern Tree_tTree Tree_mOpaque ARGS((Tree_tTree pNext, Idents_tIdent pIdent));
extern Tree_tTree Tree_mVarIds ARGS(());
extern Tree_tTree Tree_mVarIds0 ARGS(());
extern Tree_tTree Tree_mVarIds1 ARGS((Idents_tIdent pIdent, Tree_tTree pNext));
extern Tree_tTree Tree_mFormals ARGS(());
extern Tree_tTree Tree_mFormals0 ARGS(());
extern Tree_tTree Tree_mFormals1 ARGS((BOOLEAN pIsVAR, Tree_tTree pParIds, Tree_tTree pType, Tree_tTree pNext));
extern Tree_tTree Tree_mParIds ARGS(());
extern Tree_tTree Tree_mParIds0 ARGS(());
extern Tree_tTree Tree_mParIds1 ARGS((Idents_tIdent pIdent, Tree_tTree pNext));
extern Tree_tTree Tree_mType ARGS(());
extern Tree_tTree Tree_mArray ARGS((BOOLEAN pIsOpen, Tree_tTree pIndexType, Tree_tTree pElemType));
extern Tree_tTree Tree_mRecord ARGS((Tree_tTree pFields));
extern Tree_tTree Tree_mSetType ARGS((Tree_tTree pBaseType));
extern Tree_tTree Tree_mPointer ARGS((Tree_tTree pTargetType));
extern Tree_tTree Tree_mProcType ARGS((Tree_tTree pFormalTypes, Tree_tTree pResultType));
extern Tree_tTree Tree_mSimpleType ARGS(());
extern Tree_tTree Tree_mEnumeration ARGS((Tree_tTree pEnumIds));
extern Tree_tTree Tree_mSubrange ARGS((Tree_tTree pBaseType, Tree_tTree pLwb, Tree_tTree pUpb));
extern Tree_tTree Tree_mPrimaryType ARGS(());
extern Tree_tTree Tree_mVoid ARGS(());
extern Tree_tTree Tree_mTypeId ARGS((Idents_tIdent pIdent, Positions_tPosition pPos));
extern Tree_tTree Tree_mTypeId0 ARGS((Idents_tIdent pIdent, Positions_tPosition pPos));
extern Tree_tTree Tree_mTypeId1 ARGS((Idents_tIdent pIdent, Positions_tPosition pPos, Tree_tTree pTypeId));
extern Tree_tTree Tree_mFields ARGS(());
extern Tree_tTree Tree_mFields0 ARGS(());
extern Tree_tTree Tree_mFields1 ARGS((Tree_tTree pNext));
extern Tree_tTree Tree_mRecordSect ARGS((Tree_tTree pNext, Tree_tTree pFieldIds, Tree_tTree pType));
extern Tree_tTree Tree_mVariantSect ARGS((Tree_tTree pNext, Tree_tTree pTagField, Tree_tTree pVariants, Tree_tTree pElse));
extern Tree_tTree Tree_mFieldIds ARGS(());
extern Tree_tTree Tree_mFieldIds0 ARGS(());
extern Tree_tTree Tree_mFieldIds1 ARGS((Idents_tIdent pIdent, Tree_tTree pNext));
extern Tree_tTree Tree_mTagField ARGS((Tree_tTree pType));
extern Tree_tTree Tree_mTagField0 ARGS((Tree_tTree pType));
extern Tree_tTree Tree_mTagField1 ARGS((Tree_tTree pType, Idents_tIdent pIdent));
extern Tree_tTree Tree_mVariants ARGS(());
extern Tree_tTree Tree_mVariants0 ARGS(());
extern Tree_tTree Tree_mVariant ARGS((Tree_tTree pLabels, Tree_tTree pVariant, Tree_tTree pNext));
extern Tree_tTree Tree_mFormalTypes ARGS(());
extern Tree_tTree Tree_mFormalTypes0 ARGS(());
extern Tree_tTree Tree_mFormalType ARGS((BOOLEAN pIsVAR, Tree_tTree pType, Tree_tTree pNext));
extern Tree_tTree Tree_mEnumIds ARGS(());
extern Tree_tTree Tree_mEnumIds0 ARGS(());
extern Tree_tTree Tree_mEnumIds1 ARGS((Idents_tIdent pIdent, Tree_tTree pNext));
extern Tree_tTree Tree_mExpr ARGS(());
extern Tree_tTree Tree_mBinary ARGS((SHORTCARD pOperator, Tree_tTree pLop, Tree_tTree pRop));
extern Tree_tTree Tree_mUnary ARGS((SHORTCARD pOperator, Tree_tTree pMop));
extern Tree_tTree Tree_mIntConst ARGS((SHORTCARD pKind, CARDINAL pIntVal, Positions_tPosition pPos));
extern Tree_tTree Tree_mRealConst ARGS((StringMem_tStringRef pRealVal));
extern Tree_tTree Tree_mStringConst ARGS((StringMem_tStringRef pStringVal));
extern Tree_tTree Tree_mCharConst ARGS((CHAR pCharVal));
extern Tree_tTree Tree_mFuncCall ARGS((Tree_tTree pDesignator, Tree_tTree pActuals));
extern Tree_tTree Tree_mSet ARGS((Tree_tTree pBaseType, Tree_tTree pElems));
extern Tree_tTree Tree_mBitSet ARGS((Tree_tTree pElems));
extern Tree_tTree Tree_mDesignator ARGS((Positions_tPosition pPos));
extern Tree_tTree Tree_mQualid ARGS((Positions_tPosition pPos, Idents_tIdent pIdent));
extern Tree_tTree Tree_mQualid0 ARGS((Positions_tPosition pPos, Idents_tIdent pIdent));
extern Tree_tTree Tree_mQualid1 ARGS((Positions_tPosition pPos, Idents_tIdent pIdent, Tree_tTree pQualid));
extern Tree_tTree Tree_mSubscript ARGS((Positions_tPosition pPos, Tree_tTree pDesignator, Tree_tTree pIndex));
extern Tree_tTree Tree_mDeref ARGS((Positions_tPosition pPos, Tree_tTree pDesignator));
extern Tree_tTree Tree_mSelect ARGS((Positions_tPosition pPos, Tree_tTree pDesignator, Idents_tIdent pField));
extern Tree_tTree Tree_mElems ARGS(());
extern Tree_tTree Tree_mElems0 ARGS(());
extern Tree_tTree Tree_mElems1 ARGS((Tree_tTree pNext));
extern Tree_tTree Tree_mElem ARGS((Tree_tTree pNext, Tree_tTree pElem));
extern Tree_tTree Tree_mElemRange ARGS((Tree_tTree pNext, Tree_tTree pLwb, Tree_tTree pUpb));
extern Tree_tTree Tree_mActuals ARGS(());
extern Tree_tTree Tree_mActuals0 ARGS(());
extern Tree_tTree Tree_mActual ARGS((Tree_tTree pExpr, Tree_tTree pNext));
extern Tree_tTree Tree_mStmts ARGS(());
extern Tree_tTree Tree_mStmts0 ARGS(());
extern Tree_tTree Tree_mStmt ARGS((Tree_tTree pNext));
extern Tree_tTree Tree_mAssign ARGS((Tree_tTree pNext, Tree_tTree pDesignator, Tree_tTree pExpr));
extern Tree_tTree Tree_mCall ARGS((Tree_tTree pNext, Tree_tTree pDesignator, Tree_tTree pActuals));
extern Tree_tTree Tree_mIf ARGS((Tree_tTree pNext, Tree_tTree pCond, Tree_tTree pThen, Tree_tTree pElsifs, Tree_tTree pElse));
extern Tree_tTree Tree_mCase ARGS((Tree_tTree pNext, Tree_tTree pExpr, Tree_tTree pCases, Tree_tTree pElse, BOOLEAN pDefault));
extern Tree_tTree Tree_mWhile ARGS((Tree_tTree pNext, Tree_tTree pCond, Tree_tTree pStmts));
extern Tree_tTree Tree_mRepeat ARGS((Tree_tTree pNext, Tree_tTree pStmts, Tree_tTree pCond));
extern Tree_tTree Tree_mLoop ARGS((Tree_tTree pNext, Tree_tTree pStmts));
extern Tree_tTree Tree_mFor ARGS((Tree_tTree pNext, Tree_tTree pQualid, Tree_tTree pFrom, Tree_tTree pTo, Tree_tTree pBy, Tree_tTree pStmts));
extern Tree_tTree Tree_mWith ARGS((Tree_tTree pNext, Tree_tTree pDesignator, Tree_tTree pStmts));
extern Tree_tTree Tree_mExit ARGS((Tree_tTree pNext));
extern Tree_tTree Tree_mReturn1 ARGS((Tree_tTree pNext));
extern Tree_tTree Tree_mReturn2 ARGS((Tree_tTree pNext, Tree_tTree pResult));
extern Tree_tTree Tree_mElsifs ARGS(());
extern Tree_tTree Tree_mElsifs0 ARGS(());
extern Tree_tTree Tree_mElsifs1 ARGS((Tree_tTree pCond, Tree_tTree pStmts, Tree_tTree pNext));
extern Tree_tTree Tree_mCases ARGS(());
extern Tree_tTree Tree_mCases0 ARGS(());
extern Tree_tTree Tree_mCases1 ARGS((Tree_tTree pLabels, Tree_tTree pStmts, Tree_tTree pNext));
extern Tree_tTree Tree_mLabels ARGS(());
extern Tree_tTree Tree_mLabels0 ARGS(());
extern Tree_tTree Tree_mLabels1 ARGS((Tree_tTree pNext));
extern Tree_tTree Tree_mLabel ARGS((Tree_tTree pNext, Tree_tTree pLabel));
extern Tree_tTree Tree_mLabelRange ARGS((Tree_tTree pNext, Tree_tTree pLwb, Tree_tTree pUpb));
extern Tree_tTree Tree_ReverseTree ARGS((Tree_tTree Tree));
extern void Tree_BeginTree ARGS(());
extern void Tree_CloseTree ARGS(());
extern void BEGIN_Tree();
