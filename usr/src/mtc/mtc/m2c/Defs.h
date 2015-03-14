#define DEFINITION_Defs

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#define Defs_NoDefs	NIL
#define Defs_Object	1
#define Defs_Object0	2
#define Defs_Const1	3
#define Defs_EnumLiteral1	4
#define Defs_Field1	5
#define Defs_Module1	6
#define Defs_Proc1	7
#define Defs_ProcHead1	8
#define Defs_TypeDecl1	9
#define Defs_Opaque1	10
#define Defs_Var1	11
#define Defs_StdProc1	12
#define Defs_Type	13
#define Defs_Type0	14
#define Defs_ShortInt	15
#define Defs_LongInt	16
#define Defs_ShortCard	17
#define Defs_LongCard	18
#define Defs_Real	19
#define Defs_LongReal	20
#define Defs_Bool	21
#define Defs_Char	22
#define Defs_Bitset	23
#define Defs_Proc	24
#define Defs_Word	25
#define Defs_Address	26
#define Defs_IntCard	27
#define Defs_Nil	28
#define Defs_String	29
#define Defs_StringChar	30
#define Defs_Void	31
#define Defs_StdProcType1	32
#define Defs_Qualident1	33
#define Defs_Constructor	34
#define Defs_OpaqueType1	35
#define Defs_Array1	36
#define Defs_Enumeration1	37
#define Defs_Pointer1	38
#define Defs_ProcType1	39
#define Defs_Record1	40
#define Defs_Set1	41
#define Defs_Subrange1	42
#define Defs_Objects	43
#define Defs_Elmt	44
#define Defs_Union	45
#define Defs_CObjects	46
#define Defs_Types	47
#define Defs_Env	48
#define Defs_Selectors	49
#define Defs_StringPar	50
typedef struct Defs_52 *Defs_tDefs;
typedef void (*Defs_tProcTree) ARGS((Defs_tDefs));
typedef Defs_tDefs Defs_tEnv;
typedef Defs_tDefs Defs_tObject;
typedef Defs_tDefs Defs_tObjects;
typedef Defs_tDefs Defs_tCObjects;
typedef Defs_tDefs Defs_tType;
typedef Defs_tDefs Defs_tTypes;
typedef Defs_tDefs Defs_tSelectors;
typedef Defs_tDefs Defs_tStrings;
#define Defs_cVoid	0
typedef unsigned char Defs_tVoid;
#define Defs_NoEnv	Defs_NoDefs
#define Defs_NoObjects	Defs_NoDefs
#define Defs_NoCObjects	Defs_NoDefs
#define Defs_NoTypes	Defs_NoDefs
#define Defs_NoSelectors	Defs_NoDefs
#define Defs_NoStrings	Defs_NoDefs
#define Defs_ProcABS	1
#define Defs_ProcCAP	2
#define Defs_ProcCHR	3
#define Defs_ProcDEC	4
#define Defs_ProcEXCL	5
#define Defs_ProcFLOAT	6
#define Defs_ProcHALT	7
#define Defs_ProcHIGH	8
#define Defs_ProcINC	9
#define Defs_ProcINCL	10
#define Defs_ProcMAX	11
#define Defs_ProcMIN	12
#define Defs_ProcODD	13
#define Defs_ProcORD	14
#define Defs_ProcSIZE	15
#define Defs_ProcTRUNC	16
#define Defs_ProcVAL	17
#define Defs_ProcADR	18
#define Defs_ProcTSIZE	19
#define Defs_ProcNEWPROCESS	20
#define Defs_ProcTRANSFER	21
#define Defs_ProcIOTRANSFER	22
#define Defs_ProcNEW	23
#define Defs_ProcDISPOSE	24
extern Defs_tObject Defs_NoObject;
extern Defs_tType Defs_NoType;
extern Defs_tObjects Defs_Predefs;
extern Defs_tObject Defs_ModuleSYSTEM;
extern Idents_tIdent Defs_IdentALLOC, Defs_IdentDEALLOC, Defs_IdentSYSTEM, Defs_IdentLONGCARD;
extern Defs_tType Defs_TypeSHORTINT, Defs_TypeLONGINT, Defs_TypeSHORTCARD, Defs_TypeLONGCARD, Defs_TypeREAL, Defs_TypeLONGREAL, Defs_TypeBOOLEAN, Defs_TypeCHAR, Defs_TypeBITSET, Defs_TypePROC, Defs_TypeWORD, Defs_TypeADDRESS, Defs_TypeIntCard, Defs_TypeNIL, Defs_TypeSTRING, Defs_TypeStringChar, Defs_TypeVOID;
extern Defs_tObjects Defs_UNION ARGS((Defs_tObjects Objects1, Defs_tObjects Objects2));
extern Defs_tObject Defs_mConst2 ARGS((Defs_tObject Object, Values_tValue Value));
extern Defs_tObject Defs_mModule2 ARGS((Defs_tObject Object, Defs_tObjects Objects));
extern Defs_tObject Defs_mModule3 ARGS((Defs_tObject Object, Defs_tObjects Locals));
extern Defs_tObject Defs_mTypeDecl2 ARGS((Defs_tObject Object, Defs_tType Type));
extern Defs_tObject Defs_mTypeDecl3 ARGS((Defs_tObject Object, Defs_tType Type));
extern Defs_tObject Defs_mOpaque2 ARGS((Defs_tObject Object, Defs_tType Type));
extern Defs_tObject Defs_mOpaque3 ARGS((Defs_tObject Object, Defs_tType Type));
extern Defs_tObject Defs_mProc2 ARGS((Defs_tObject Object, Defs_tObjects Locals, BOOLEAN IsExported));
extern Defs_tType Defs_mOpaqueType2 ARGS((Defs_tType Type, Defs_tType FullType));
extern Defs_tType Defs_mArray2 ARGS((Defs_tType Type, BOOLEAN IsOpen, Defs_tType IndexType, Defs_tType ElemType));
extern Defs_tType Defs_mEnumeration2 ARGS((Defs_tType Type, Defs_tObjects EnumLiterals, SHORTCARD MaxValue));
extern Defs_tType Defs_mPointer2 ARGS((Defs_tType Type, Defs_tType TargetType));
extern Defs_tType Defs_mProcType2 ARGS((Defs_tType Type, Defs_tTypes FormalTypes, Defs_tType ResultType));
extern Defs_tType Defs_mRecord2 ARGS((Defs_tType Type, Defs_tObjects Fields));
extern Defs_tType Defs_mSet2 ARGS((Defs_tType Type, Defs_tType BaseType));
extern Defs_tType Defs_mSubrange2 ARGS((Defs_tType Type, Defs_tType BaseType));
extern Defs_tType Defs_mSubrange3 ARGS((Defs_tType Type, Values_tValue Lwb, Values_tValue Upb));
extern Idents_tIdent Defs_DefineCIdent ARGS((Defs_tObject Object, Idents_tIdent CIdent));
extern BOOLEAN Defs_NestedUse ARGS((Defs_tObject Object, SHORTCARD Level));
extern Defs_tVoid Defs_mVoid1 ARGS((Defs_tObject Object, Defs_tVoid Void));
extern Defs_tVoid Defs_mVoid2 ARGS((Defs_tVoid Void1, Defs_tVoid Void2));
extern Defs_tType Defs_GroundType ARGS((Defs_tType Type));
extern Defs_tObject Defs_Identify ARGS((Idents_tIdent Ident, Defs_tEnv Env));
extern Defs_tObject Defs_Identify2 ARGS((Idents_tIdent Ident, Defs_tObjects Objects));
extern BOOLEAN Defs_IsDeclared ARGS((Idents_tIdent Ident, Defs_tObjects Objects));
extern Idents_tIdent Defs_LookUp ARGS((Defs_tObject M2Object, Defs_tCObjects Objects));
extern Defs_tObjects Defs_Filter ARGS((Defs_tObjects Objects));
extern Defs_tCObjects Defs_Pointers ARGS((BOOLEAN GlobalPtrs, Defs_tObjects Objects));
extern void Defs_OpenArrays ARGS((Defs_tObjects Objects, Defs_tCObjects *pValueOpens, Defs_tCObjects *pVAROpens));
extern Defs_tSelectors Defs_GetSelectors ARGS((Defs_tObject Object));
extern Defs_tObjects Defs_GetExport1 ARGS((Defs_tObject Object));
extern Defs_tObjects Defs_GetExport2 ARGS((Defs_tObject Object));
extern Defs_tObjects Defs_GetObjects ARGS((Defs_tObject Object));
extern Defs_tType Defs_GetType ARGS((Defs_tObject Object));
extern Defs_tType Defs_GetIndexType ARGS((Defs_tType Type));
extern Defs_tType Defs_GetElemType ARGS((Defs_tType Type));
extern Defs_tType Defs_GetTargetType ARGS((Defs_tType Type));
extern Defs_tTypes Defs_GetFormals ARGS((Defs_tType Type));
extern Defs_tType Defs_GetResultType ARGS((Defs_tType Type));
extern Defs_tObjects Defs_GetFields ARGS((Defs_tType Type));
extern void Defs_Head ARGS((Defs_tTypes Types, BOOLEAN *IsVAR, Defs_tType *Type));
extern Defs_tTypes Defs_Tail ARGS((Defs_tTypes Types));
extern Defs_tObject Defs_GetLiteral ARGS((Defs_tObjects Objects, SHORTCARD Index));
extern BOOLEAN Defs_IsExported ARGS((Defs_tObject Object));
extern BOOLEAN Defs_IsOpenArray ARGS((Defs_tObject Object));
extern BOOLEAN Defs_IsVAR ARGS((Defs_tObject Object));
extern BOOLEAN Defs_IsProcedure ARGS((Defs_tObject Object));
extern BOOLEAN Defs_IsOfType ARGS((Defs_tObject Object));
extern BOOLEAN Defs_IsForward ARGS((SHORTCARD PosPointerTo, Defs_tObject TypeObj));
extern BOOLEAN Defs_IsIntType ARGS((Defs_tType Type));
extern BOOLEAN Defs_IsOpen ARGS((Defs_tType Type));
extern void Defs_WriteEnv ARGS((IO_tFile f, Defs_tEnv Env));
extern void Defs_WriteObjects ARGS((IO_tFile f, Defs_tObjects Objects));
typedef struct Defs_1 {
    SHORTCARD yyKind, yyMark;
} Defs_yytNodeHead;
typedef struct Defs_2 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
} Defs_yObject;
typedef struct Defs_3 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
} Defs_yObject0;
typedef struct Defs_4 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Values_tValue Value;
} Defs_yConst1;
typedef struct Defs_5 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Defs_tDefs Type;
    SHORTCARD Index;
} Defs_yEnumLiteral1;
typedef struct Defs_6 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Defs_tDefs Type;
    Defs_tDefs Selectors;
} Defs_yField1;
typedef struct Defs_7 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Defs_tDefs ExportList;
    Defs_tDefs Objects;
    Defs_tDefs Locals;
} Defs_yModule1;
typedef struct Defs_8 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Defs_tDefs Type;
    BOOLEAN IsExported;
    Defs_tDefs Locals;
} Defs_yProc1;
typedef struct Defs_9 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Defs_tDefs Type;
} Defs_yProcHead1;
typedef struct Defs_10 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    SHORTCARD TypePos;
    Defs_tDefs Type;
} Defs_yTypeDecl1;
typedef struct Defs_11 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    SHORTCARD TypePos;
    Defs_tDefs Type;
} Defs_yOpaque1;
typedef struct Defs_12 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    Defs_tDefs Type;
    BOOLEAN IsVAR;
    SHORTCARD Level;
    BOOLEAN NestedUse;
    ADDRESS TypeTree;
} Defs_yVar1;
typedef struct Defs_13 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    Idents_tIdent CIdent;
    SHORTCARD StdProc;
    Defs_tDefs Type;
} Defs_yStdProc1;
typedef struct Defs_14 {
    Defs_yytNodeHead yyHead;
} Defs_yType;
typedef struct Defs_15 {
    Defs_yytNodeHead yyHead;
} Defs_yType0;
typedef struct Defs_16 {
    Defs_yytNodeHead yyHead;
} Defs_yShortInt;
typedef struct Defs_17 {
    Defs_yytNodeHead yyHead;
} Defs_yLongInt;
typedef struct Defs_18 {
    Defs_yytNodeHead yyHead;
} Defs_yShortCard;
typedef struct Defs_19 {
    Defs_yytNodeHead yyHead;
} Defs_yLongCard;
typedef struct Defs_20 {
    Defs_yytNodeHead yyHead;
} Defs_yReal;
typedef struct Defs_21 {
    Defs_yytNodeHead yyHead;
} Defs_yLongReal;
typedef struct Defs_22 {
    Defs_yytNodeHead yyHead;
} Defs_yBool;
typedef struct Defs_23 {
    Defs_yytNodeHead yyHead;
} Defs_yChar;
typedef struct Defs_24 {
    Defs_yytNodeHead yyHead;
} Defs_yBitset;
typedef struct Defs_25 {
    Defs_yytNodeHead yyHead;
} Defs_yProc;
typedef struct Defs_26 {
    Defs_yytNodeHead yyHead;
} Defs_yWord;
typedef struct Defs_27 {
    Defs_yytNodeHead yyHead;
} Defs_yAddress;
typedef struct Defs_28 {
    Defs_yytNodeHead yyHead;
} Defs_yIntCard;
typedef struct Defs_29 {
    Defs_yytNodeHead yyHead;
} Defs_yNil;
typedef struct Defs_30 {
    Defs_yytNodeHead yyHead;
} Defs_yString;
typedef struct Defs_31 {
    Defs_yytNodeHead yyHead;
} Defs_yStringChar;
typedef struct Defs_32 {
    Defs_yytNodeHead yyHead;
} Defs_yVoid;
typedef struct Defs_33 {
    Defs_yytNodeHead yyHead;
    SHORTCARD StdProc;
} Defs_yStdProcType1;
typedef struct Defs_34 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs Object;
} Defs_yQualident1;
typedef struct Defs_35 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
} Defs_yConstructor;
typedef struct Defs_36 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
    Defs_tDefs Type;
} Defs_yOpaqueType1;
typedef struct Defs_37 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
    Idents_tIdent StructId;
    Defs_tDefs IndexType;
    Defs_tDefs ElemType;
    BOOLEAN IsOpen;
} Defs_yArray1;
typedef struct Defs_38 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
    SHORTCARD MaxValue;
    Defs_tDefs Objects;
} Defs_yEnumeration1;
typedef struct Defs_39 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
    Defs_tDefs Type;
} Defs_yPointer1;
typedef struct Defs_40 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
    Defs_tDefs Types;
    Defs_tDefs Type;
} Defs_yProcType1;
typedef struct Defs_41 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
    Idents_tIdent StructId;
    Defs_tDefs Objects;
} Defs_yRecord1;
typedef struct Defs_42 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
    Defs_tDefs Type;
} Defs_ySet1;
typedef struct Defs_43 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs TypeObj;
    ADDRESS LwbExpr;
    ADDRESS UpbExpr;
    Defs_tDefs Type;
    Values_tValue Lwb;
    Values_tValue Upb;
} Defs_ySubrange1;
typedef struct Defs_44 {
    Defs_yytNodeHead yyHead;
} Defs_yObjects;
typedef struct Defs_45 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Ident;
    BOOLEAN IsPseudoObj;
    Defs_tDefs Object;
    Defs_tDefs Next;
} Defs_yElmt;
typedef struct Defs_46 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs Objects1;
    Defs_tDefs Objects2;
} Defs_yUnion;
typedef struct Defs_47 {
    Defs_yytNodeHead yyHead;
    Defs_tObject M2Object;
    Defs_tDefs Next;
    Idents_tIdent CObject;
} Defs_yCObjects;
typedef struct Defs_48 {
    Defs_yytNodeHead yyHead;
    BOOLEAN IsVAR;
    Defs_tDefs Type;
    Defs_tDefs Next;
} Defs_yTypes;
typedef struct Defs_49 {
    Defs_yytNodeHead yyHead;
    Defs_tDefs Objects;
    Defs_tDefs HiddenEnv;
} Defs_yEnv;
typedef struct Defs_50 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent Selector;
    Defs_tDefs Next;
} Defs_ySelectors;
typedef struct Defs_51 {
    Defs_yytNodeHead yyHead;
    Idents_tIdent CString;
    Defs_tDefs FormalType;
    ADDRESS M2String;
    Defs_tDefs Next;
} Defs_yStringPar;
typedef struct Defs_52 {
    union {
        struct {
            SHORTCARD Kind;
        } V_1;
        struct {
            Defs_yytNodeHead yyHead;
        } V_2;
        struct {
            Defs_yObject Object;
        } V_3;
        struct {
            Defs_yObject0 Object0;
        } V_4;
        struct {
            Defs_yConst1 Const1;
        } V_5;
        struct {
            Defs_yEnumLiteral1 EnumLiteral1;
        } V_6;
        struct {
            Defs_yField1 Field1;
        } V_7;
        struct {
            Defs_yModule1 Module1;
        } V_8;
        struct {
            Defs_yProc1 Proc1;
        } V_9;
        struct {
            Defs_yProcHead1 ProcHead1;
        } V_10;
        struct {
            Defs_yTypeDecl1 TypeDecl1;
        } V_11;
        struct {
            Defs_yOpaque1 Opaque1;
        } V_12;
        struct {
            Defs_yVar1 Var1;
        } V_13;
        struct {
            Defs_yStdProc1 StdProc1;
        } V_14;
        struct {
            Defs_yType Type;
        } V_15;
        struct {
            Defs_yType0 Type0;
        } V_16;
        struct {
            Defs_yShortInt ShortInt;
        } V_17;
        struct {
            Defs_yLongInt LongInt;
        } V_18;
        struct {
            Defs_yShortCard ShortCard;
        } V_19;
        struct {
            Defs_yLongCard LongCard;
        } V_20;
        struct {
            Defs_yReal Real;
        } V_21;
        struct {
            Defs_yLongReal LongReal;
        } V_22;
        struct {
            Defs_yBool Bool;
        } V_23;
        struct {
            Defs_yChar Char;
        } V_24;
        struct {
            Defs_yBitset Bitset;
        } V_25;
        struct {
            Defs_yProc Proc;
        } V_26;
        struct {
            Defs_yWord Word;
        } V_27;
        struct {
            Defs_yAddress Address;
        } V_28;
        struct {
            Defs_yIntCard IntCard;
        } V_29;
        struct {
            Defs_yNil Nil;
        } V_30;
        struct {
            Defs_yString String;
        } V_31;
        struct {
            Defs_yStringChar StringChar;
        } V_32;
        struct {
            Defs_yVoid Void;
        } V_33;
        struct {
            Defs_yStdProcType1 StdProcType1;
        } V_34;
        struct {
            Defs_yQualident1 Qualident1;
        } V_35;
        struct {
            Defs_yConstructor Constructor;
        } V_36;
        struct {
            Defs_yOpaqueType1 OpaqueType1;
        } V_37;
        struct {
            Defs_yArray1 Array1;
        } V_38;
        struct {
            Defs_yEnumeration1 Enumeration1;
        } V_39;
        struct {
            Defs_yPointer1 Pointer1;
        } V_40;
        struct {
            Defs_yProcType1 ProcType1;
        } V_41;
        struct {
            Defs_yRecord1 Record1;
        } V_42;
        struct {
            Defs_ySet1 Set1;
        } V_43;
        struct {
            Defs_ySubrange1 Subrange1;
        } V_44;
        struct {
            Defs_yObjects Objects;
        } V_45;
        struct {
            Defs_yElmt Elmt;
        } V_46;
        struct {
            Defs_yUnion Union;
        } V_47;
        struct {
            Defs_yCObjects CObjects;
        } V_48;
        struct {
            Defs_yTypes Types;
        } V_49;
        struct {
            Defs_yEnv Env;
        } V_50;
        struct {
            Defs_ySelectors Selectors;
        } V_51;
        struct {
            Defs_yStringPar StringPar;
        } V_52;
    } U_1;
} Defs_yyNode;
extern Defs_tDefs Defs_DefsRoot;
extern LONGCARD Defs_HeapUsed;
extern ADDRESS Defs_yyPoolFreePtr, Defs_yyPoolMaxPtr;
extern struct Defs_53 {
    SHORTCARD A[50 + 1];
} Defs_yyNodeSize;
extern PROC Defs_yyExit;
extern Defs_tDefs Defs_yyAlloc ARGS(());
extern Defs_tDefs Defs_MakeDefs ARGS((SHORTCARD Kind));
extern BOOLEAN Defs_IsType ARGS((Defs_tDefs Tree, SHORTCARD Kind));
extern Defs_tDefs Defs_mObject ARGS((Idents_tIdent pIdent));
extern Defs_tDefs Defs_mObject0 ARGS((Idents_tIdent pIdent));
extern Defs_tDefs Defs_mConst1 ARGS((Idents_tIdent pIdent));
extern Defs_tDefs Defs_mEnumLiteral1 ARGS((Idents_tIdent pIdent, Defs_tDefs pType, SHORTCARD pIndex));
extern Defs_tDefs Defs_mField1 ARGS((Idents_tIdent pIdent, Defs_tDefs pType, Defs_tDefs pSelectors));
extern Defs_tDefs Defs_mModule1 ARGS((Idents_tIdent pIdent, Defs_tDefs pExportList));
extern Defs_tDefs Defs_mProc1 ARGS((Idents_tIdent pIdent, Defs_tDefs pType));
extern Defs_tDefs Defs_mProcHead1 ARGS((Idents_tIdent pIdent, Defs_tDefs pType));
extern Defs_tDefs Defs_mTypeDecl1 ARGS((Idents_tIdent pIdent, SHORTCARD pTypePos));
extern Defs_tDefs Defs_mOpaque1 ARGS((Idents_tIdent pIdent, SHORTCARD pTypePos));
extern Defs_tDefs Defs_mVar1 ARGS((Idents_tIdent pIdent, Defs_tDefs pType, BOOLEAN pIsVAR, SHORTCARD pLevel, BOOLEAN pNestedUse, ADDRESS pTypeTree));
extern Defs_tDefs Defs_mStdProc1 ARGS((Idents_tIdent pIdent, SHORTCARD pStdProc, Defs_tDefs pType));
extern Defs_tDefs Defs_mType ARGS(());
extern Defs_tDefs Defs_mType0 ARGS(());
extern Defs_tDefs Defs_mShortInt ARGS(());
extern Defs_tDefs Defs_mLongInt ARGS(());
extern Defs_tDefs Defs_mShortCard ARGS(());
extern Defs_tDefs Defs_mLongCard ARGS(());
extern Defs_tDefs Defs_mReal ARGS(());
extern Defs_tDefs Defs_mLongReal ARGS(());
extern Defs_tDefs Defs_mBool ARGS(());
extern Defs_tDefs Defs_mChar ARGS(());
extern Defs_tDefs Defs_mBitset ARGS(());
extern Defs_tDefs Defs_mProc ARGS(());
extern Defs_tDefs Defs_mWord ARGS(());
extern Defs_tDefs Defs_mAddress ARGS(());
extern Defs_tDefs Defs_mIntCard ARGS(());
extern Defs_tDefs Defs_mNil ARGS(());
extern Defs_tDefs Defs_mString ARGS(());
extern Defs_tDefs Defs_mStringChar ARGS(());
extern Defs_tDefs Defs_mVoid ARGS(());
extern Defs_tDefs Defs_mStdProcType1 ARGS((SHORTCARD pStdProc));
extern Defs_tDefs Defs_mQualident1 ARGS((Defs_tDefs pObject));
extern Defs_tDefs Defs_mConstructor ARGS((Defs_tDefs pTypeObj));
extern Defs_tDefs Defs_mOpaqueType1 ARGS((Defs_tDefs pTypeObj));
extern Defs_tDefs Defs_mArray1 ARGS((Defs_tDefs pTypeObj, Idents_tIdent pStructId));
extern Defs_tDefs Defs_mEnumeration1 ARGS((Defs_tDefs pTypeObj));
extern Defs_tDefs Defs_mPointer1 ARGS((Defs_tDefs pTypeObj));
extern Defs_tDefs Defs_mProcType1 ARGS((Defs_tDefs pTypeObj));
extern Defs_tDefs Defs_mRecord1 ARGS((Defs_tDefs pTypeObj, Idents_tIdent pStructId));
extern Defs_tDefs Defs_mSet1 ARGS((Defs_tDefs pTypeObj));
extern Defs_tDefs Defs_mSubrange1 ARGS((Defs_tDefs pTypeObj, ADDRESS pLwbExpr, ADDRESS pUpbExpr));
extern Defs_tDefs Defs_mObjects ARGS(());
extern Defs_tDefs Defs_mElmt ARGS((Idents_tIdent pIdent, BOOLEAN pIsPseudoObj, Defs_tDefs pObject, Defs_tDefs pNext));
extern Defs_tDefs Defs_mUnion ARGS((Defs_tDefs pObjects1, Defs_tDefs pObjects2));
extern Defs_tDefs Defs_mCObjects ARGS((Defs_tObject pM2Object, Defs_tDefs pNext));
extern Defs_tDefs Defs_mTypes ARGS((BOOLEAN pIsVAR, Defs_tDefs pType, Defs_tDefs pNext));
extern Defs_tDefs Defs_mEnv ARGS((Defs_tDefs pObjects, Defs_tDefs pHiddenEnv));
extern Defs_tDefs Defs_mSelectors ARGS((Idents_tIdent pSelector, Defs_tDefs pNext));
extern Defs_tDefs Defs_mStringPar ARGS((Idents_tIdent pCString, Defs_tDefs pFormalType, ADDRESS pM2String, Defs_tDefs pNext));
extern void Defs_BeginDefs ARGS(());
extern void Defs_CloseDefs ARGS(());
extern void BEGIN_Defs();
