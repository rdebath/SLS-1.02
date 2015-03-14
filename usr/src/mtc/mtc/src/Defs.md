DEFINITION MODULE Defs;

IMPORT SYSTEM, IO;
(* line 15 "defs.cg" *)

FROM SYSTEM	IMPORT ADDRESS;
FROM IO		IMPORT tFile;
FROM Idents	IMPORT tIdent;
FROM Values	IMPORT tValue;


CONST
NoDefs = NIL;

Object = 1;
Object0 = 2;
Const1 = 3;
EnumLiteral1 = 4;
Field1 = 5;
Module1 = 6;
Proc1 = 7;
ProcHead1 = 8;
TypeDecl1 = 9;
Opaque1 = 10;
Var1 = 11;
StdProc1 = 12;
Type = 13;
Type0 = 14;
ShortInt = 15;
LongInt = 16;
ShortCard = 17;
LongCard = 18;
Real = 19;
LongReal = 20;
Bool = 21;
Char = 22;
Bitset = 23;
Proc = 24;
Word = 25;
Address = 26;
IntCard = 27;
Nil = 28;
String = 29;
StringChar = 30;
Void = 31;
StdProcType1 = 32;
Qualident1 = 33;
Constructor = 34;
OpaqueType1 = 35;
Array1 = 36;
Enumeration1 = 37;
Pointer1 = 38;
ProcType1 = 39;
Record1 = 40;
Set1 = 41;
Subrange1 = 42;
Objects = 43;
Elmt = 44;
Union = 45;
CObjects = 46;
Types = 47;
Env = 48;
Selectors = 49;
StringPar = 50;

TYPE tDefs = POINTER TO yyNode;
tProcTree = PROCEDURE (tDefs);
(* line 22 "defs.cg" *)

TYPE tEnv	= tDefs;	(* type to represent environments	*)
TYPE tObject	= tDefs;	(* type for object descriptions		*)
TYPE tObjects	= tDefs;	(* type to repr. lists of object descr.	*)
TYPE tCObjects	= tDefs;	(* type to repr. lists of object descr.	*)
TYPE tType	= tDefs;	(* representation of data types		*)
TYPE tTypes	= tDefs;	(* type to repr. formal parameter lists	*)
TYPE tSelectors	= tDefs;	(* type to repr. record field selectors	*)
TYPE tStrings	= tDefs;	(* type to repr. lists of string params.*)

TYPE tVoid	= (cVoid);

CONST NoEnv		= NoDefs;	(* empty environment		*)
CONST NoObjects		= NoDefs;	(* empty list of object descr.	*)
CONST NoCObjects	= NoDefs;	(* empty list of object descr.	*)
CONST NoTypes		= NoDefs;	(* empty formal parameter list	*)
CONST NoSelectors	= NoDefs;	(* empty list of record field	*)
CONST NoStrings		= NoDefs;	(* empty list of string params.	*)

CONST
   ProcABS		=  1;		(* standard procedures		*)
   ProcCAP		=  2;
   ProcCHR		=  3;
   ProcDEC		=  4;
   ProcEXCL		=  5;
   ProcFLOAT		=  6;
   ProcHALT		=  7;
   ProcHIGH		=  8;
   ProcINC		=  9;
   ProcINCL		= 10;
   ProcMAX		= 11;
   ProcMIN		= 12;
   ProcODD		= 13;
   ProcORD		= 14;
   ProcSIZE		= 15;
   ProcTRUNC		= 16;
   ProcVAL		= 17;
   ProcADR		= 18;
   ProcTSIZE		= 19;
   ProcNEWPROCESS	= 20;
   ProcTRANSFER		= 21;
   ProcIOTRANSFER	= 22;
   ProcNEW		= 23;
   ProcDISPOSE		= 24;

VAR NoObject	: tObject;		(* default, error, or no object	*)
VAR NoType	: tType;		(* default, error, or no type	*)

VAR Predefs	: tObjects;		(* predefined objects		*)

VAR
   ModuleSYSTEM	: tObject;		(* module SYSTEM		*)

VAR
   IdentALLOC	,			(* identifier ALLOCATE		*)
   IdentDEALLOC	,			(* identifier DEALLOCATE	*)
   IdentSYSTEM	,			(* identifier of module SYSTEM	*)
   IdentLONGCARD: tIdent;		(* identifier of type LONGCARD	*)

VAR
  TypeSHORTINT	,			(* predefined types	*)
  TypeLONGINT	,
  TypeSHORTCARD	,
  TypeLONGCARD	,
  TypeREAL	,
  TypeLONGREAL	,
  TypeBOOLEAN	,
  TypeCHAR	,
  TypeBITSET	,
  TypePROC	,
  TypeWORD	,
  TypeADDRESS	,
  TypeIntCard	,
  TypeNIL	,
  TypeSTRING	,
  TypeStringChar,
  TypeVOID	: tType;


PROCEDURE UNION		(Objects1, Objects2: tObjects): tObjects;
			(* performs a union of two object lists		*)
			(* (mUnion) with an optimization of some very	*)
			(* frequent special cases to speed up later	*)
			(* identification				*)

(*
 *	procedures to extend object and type descriptions
 *)

PROCEDURE mConst2	(Object: tObject; Value: tValue): tObject;
PROCEDURE mModule2	(Object: tObject; Objects: tObjects): tObject;
PROCEDURE mModule3	(Object: tObject; Locals: tObjects): tObject;
PROCEDURE mTypeDecl2	(Object: tObject; Type: tType): tObject;
PROCEDURE mTypeDecl3	(Object: tObject; Type: tType): tObject;
PROCEDURE mOpaque2	(Object: tObject; Type: tType): tObject;
PROCEDURE mOpaque3	(Object: tObject; Type: tType): tObject;
PROCEDURE mProc2	(Object: tObject; Locals: tObjects; IsExported: BOOLEAN): tObject;

PROCEDURE mOpaqueType2	(Type: tType; FullType: tType): tType;
PROCEDURE mArray2	(Type: tType; IsOpen: BOOLEAN; IndexType, ElemType: tType): tType;
PROCEDURE mEnumeration2	(Type: tType; EnumLiterals: tObjects; MaxValue: SHORTCARD): tType;
PROCEDURE mPointer2	(Type: tType; TargetType: tType): tType;
PROCEDURE mProcType2	(Type: tType; FormalTypes: tTypes; ResultType: tType): tType;
PROCEDURE mRecord2	(Type: tType; Fields: tObjects): tType;
PROCEDURE mSet2		(Type: tType; BaseType: tType): tType;
PROCEDURE mSubrange2	(Type: tType; BaseType: tType): tType;
PROCEDURE mSubrange3	(Type: tType; Lwb, Upb: tValue): tType;

PROCEDURE DefineCIdent	(Object: tObject; CIdent: tIdent): tIdent;
PROCEDURE NestedUse	(Object: tObject; Level: SHORTCARD): BOOLEAN;

PROCEDURE mVoid1	(Object: tObject; Void: tVoid): tVoid;
PROCEDURE mVoid2	(Void1, Void2: tVoid): tVoid;

(*
 *	procedures for identification
 *)

PROCEDURE GroundType	(Type: tType): tType;
PROCEDURE Identify	(Ident: tIdent; Env: tEnv): tObject;
PROCEDURE Identify2	(Ident: tIdent; Objects: tObjects): tObject;
PROCEDURE IsDeclared	(Ident: tIdent; Objects: tObjects): BOOLEAN;
PROCEDURE LookUp	(M2Object: tObject; Objects: tCObjects): tIdent;

(*
 *	procedures to collect object descriptions
 *)

PROCEDURE Filter	(Objects: tObjects): tObjects;
PROCEDURE Pointers	(GlobalPtrs: BOOLEAN; Objects: tObjects): tCObjects;
PROCEDURE OpenArrays	(Objects: tObjects; VAR pValueOpens, pVAROpens: tCObjects);

(*
 *	procedures for 'safe' access to object and type descriptions
 *)

PROCEDURE GetSelectors	(Object: tObject): tSelectors;
PROCEDURE GetExport1	(Object: tObject): tObjects;
PROCEDURE GetExport2	(Object: tObject): tObjects;
PROCEDURE GetObjects	(Object: tObject): tObjects;
PROCEDURE GetType	(Object: tObject): tType;

PROCEDURE GetIndexType	(Type: tType): tType;
PROCEDURE GetElemType	(Type: tType): tType;
PROCEDURE GetTargetType	(Type: tType): tType;
PROCEDURE GetFormals	(Type: tType): tTypes;
PROCEDURE GetResultType	(Type: tType): tType;
PROCEDURE GetFields	(Type: tType): tObjects;

PROCEDURE Head		(Types: tTypes; VAR IsVAR: BOOLEAN; VAR Type: tType);
PROCEDURE Tail		(Types: tTypes): tTypes;

PROCEDURE GetLiteral	(Objects: tObjects; Index: SHORTCARD): tObject;

(*
 *	predicates on objects and types
 *)

PROCEDURE IsExported	(Object: tObject): BOOLEAN;
PROCEDURE IsOpenArray	(Object: tObject): BOOLEAN;
PROCEDURE IsVAR		(Object: tObject): BOOLEAN;
PROCEDURE IsProcedure	(Object: tObject): BOOLEAN;
PROCEDURE IsOfType	(Object: tObject): BOOLEAN;

PROCEDURE IsForward	(PosPointerTo: SHORTCARD; TypeObj: tObject): BOOLEAN;

PROCEDURE IsIntType	(Type: tType): BOOLEAN;
PROCEDURE IsOpen	(Type: tType): BOOLEAN;

(*
 *	procedures for test output
 *)

PROCEDURE WriteEnv	(f: tFile; Env: tEnv);
PROCEDURE WriteObjects	(f: tFile; Objects: tObjects);





TYPE
yytNodeHead = RECORD yyKind, yyMark: SHORTCARD;  END;
yObject = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; END;
yObject0 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; END;
yConst1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; Value: tValue; END;
yEnumLiteral1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; Type: tDefs; Index: SHORTCARD; END;
yField1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; Type: tDefs; Selectors: tDefs; END;
yModule1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; ExportList: tDefs; Objects: tDefs; Locals: tDefs; END;
yProc1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; Type: tDefs; IsExported: BOOLEAN; Locals: tDefs; END;
yProcHead1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; Type: tDefs; END;
yTypeDecl1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; TypePos: SHORTCARD; Type: tDefs; END;
yOpaque1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; TypePos: SHORTCARD; Type: tDefs; END;
yVar1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; Type: tDefs; IsVAR: BOOLEAN; Level: SHORTCARD; NestedUse: BOOLEAN; TypeTree: ADDRESS; END;
yStdProc1 = RECORD yyHead: yytNodeHead; Ident: tIdent; CIdent: tIdent; StdProc: SHORTCARD; Type: tDefs; END;
yType = RECORD yyHead: yytNodeHead; END;
yType0 = RECORD yyHead: yytNodeHead; END;
yShortInt = RECORD yyHead: yytNodeHead; END;
yLongInt = RECORD yyHead: yytNodeHead; END;
yShortCard = RECORD yyHead: yytNodeHead; END;
yLongCard = RECORD yyHead: yytNodeHead; END;
yReal = RECORD yyHead: yytNodeHead; END;
yLongReal = RECORD yyHead: yytNodeHead; END;
yBool = RECORD yyHead: yytNodeHead; END;
yChar = RECORD yyHead: yytNodeHead; END;
yBitset = RECORD yyHead: yytNodeHead; END;
yProc = RECORD yyHead: yytNodeHead; END;
yWord = RECORD yyHead: yytNodeHead; END;
yAddress = RECORD yyHead: yytNodeHead; END;
yIntCard = RECORD yyHead: yytNodeHead; END;
yNil = RECORD yyHead: yytNodeHead; END;
yString = RECORD yyHead: yytNodeHead; END;
yStringChar = RECORD yyHead: yytNodeHead; END;
yVoid = RECORD yyHead: yytNodeHead; END;
yStdProcType1 = RECORD yyHead: yytNodeHead; StdProc: SHORTCARD; END;
yQualident1 = RECORD yyHead: yytNodeHead; Object: tDefs; END;
yConstructor = RECORD yyHead: yytNodeHead; TypeObj: tDefs; END;
yOpaqueType1 = RECORD yyHead: yytNodeHead; TypeObj: tDefs; Type: tDefs; END;
yArray1 = RECORD yyHead: yytNodeHead; TypeObj: tDefs; StructId: tIdent; IndexType: tDefs; ElemType: tDefs; IsOpen: BOOLEAN; END;
yEnumeration1 = RECORD yyHead: yytNodeHead; TypeObj: tDefs; MaxValue: SHORTCARD; Objects: tDefs; END;
yPointer1 = RECORD yyHead: yytNodeHead; TypeObj: tDefs; Type: tDefs; END;
yProcType1 = RECORD yyHead: yytNodeHead; TypeObj: tDefs; Types: tDefs; Type: tDefs; END;
yRecord1 = RECORD yyHead: yytNodeHead; TypeObj: tDefs; StructId: tIdent; Objects: tDefs; END;
ySet1 = RECORD yyHead: yytNodeHead; TypeObj: tDefs; Type: tDefs; END;
ySubrange1 = RECORD yyHead: yytNodeHead; TypeObj: tDefs; LwbExpr: ADDRESS; UpbExpr: ADDRESS; Type: tDefs; Lwb: tValue; Upb: tValue; END;
yObjects = RECORD yyHead: yytNodeHead; END;
yElmt = RECORD yyHead: yytNodeHead; Ident: tIdent; IsPseudoObj: BOOLEAN; Object: tDefs; Next: tDefs; END;
yUnion = RECORD yyHead: yytNodeHead; Objects1: tDefs; Objects2: tDefs; END;
yCObjects = RECORD yyHead: yytNodeHead; M2Object: tObject; Next: tDefs; CObject: tIdent; END;
yTypes = RECORD yyHead: yytNodeHead; IsVAR: BOOLEAN; Type: tDefs; Next: tDefs; END;
yEnv = RECORD yyHead: yytNodeHead; Objects: tDefs; HiddenEnv: tDefs; END;
ySelectors = RECORD yyHead: yytNodeHead; Selector: tIdent; Next: tDefs; END;
yStringPar = RECORD yyHead: yytNodeHead; CString: tIdent; FormalType: tDefs; M2String: ADDRESS; Next: tDefs; END;

yyNode = RECORD
CASE : SHORTCARD OF
| 0: Kind: SHORTCARD;
| 51: yyHead: yytNodeHead;
| Object: Object: yObject;
| Object0: Object0: yObject0;
| Const1: Const1: yConst1;
| EnumLiteral1: EnumLiteral1: yEnumLiteral1;
| Field1: Field1: yField1;
| Module1: Module1: yModule1;
| Proc1: Proc1: yProc1;
| ProcHead1: ProcHead1: yProcHead1;
| TypeDecl1: TypeDecl1: yTypeDecl1;
| Opaque1: Opaque1: yOpaque1;
| Var1: Var1: yVar1;
| StdProc1: StdProc1: yStdProc1;
| Type: Type: yType;
| Type0: Type0: yType0;
| ShortInt: ShortInt: yShortInt;
| LongInt: LongInt: yLongInt;
| ShortCard: ShortCard: yShortCard;
| LongCard: LongCard: yLongCard;
| Real: Real: yReal;
| LongReal: LongReal: yLongReal;
| Bool: Bool: yBool;
| Char: Char: yChar;
| Bitset: Bitset: yBitset;
| Proc: Proc: yProc;
| Word: Word: yWord;
| Address: Address: yAddress;
| IntCard: IntCard: yIntCard;
| Nil: Nil: yNil;
| String: String: yString;
| StringChar: StringChar: yStringChar;
| Void: Void: yVoid;
| StdProcType1: StdProcType1: yStdProcType1;
| Qualident1: Qualident1: yQualident1;
| Constructor: Constructor: yConstructor;
| OpaqueType1: OpaqueType1: yOpaqueType1;
| Array1: Array1: yArray1;
| Enumeration1: Enumeration1: yEnumeration1;
| Pointer1: Pointer1: yPointer1;
| ProcType1: ProcType1: yProcType1;
| Record1: Record1: yRecord1;
| Set1: Set1: ySet1;
| Subrange1: Subrange1: ySubrange1;
| Objects: Objects: yObjects;
| Elmt: Elmt: yElmt;
| Union: Union: yUnion;
| CObjects: CObjects: yCObjects;
| Types: Types: yTypes;
| Env: Env: yEnv;
| Selectors: Selectors: ySelectors;
| StringPar: StringPar: yStringPar;
END;
END;

VAR DefsRoot	: tDefs;
VAR HeapUsed	: LONGCARD;
VAR yyPoolFreePtr, yyPoolMaxPtr	: SYSTEM.ADDRESS;
VAR yyNodeSize	: ARRAY [0..50] OF SHORTCARD;
VAR yyExit	: PROC;

PROCEDURE yyAlloc	(): tDefs;
PROCEDURE MakeDefs	(Kind: SHORTCARD): tDefs;
PROCEDURE IsType	(Tree: tDefs; Kind: SHORTCARD): BOOLEAN;

PROCEDURE mObject (pIdent: tIdent): tDefs;
PROCEDURE mObject0 (pIdent: tIdent): tDefs;
PROCEDURE mConst1 (pIdent: tIdent): tDefs;
PROCEDURE mEnumLiteral1 (pIdent: tIdent; pType: tDefs; pIndex: SHORTCARD): tDefs;
PROCEDURE mField1 (pIdent: tIdent; pType: tDefs; pSelectors: tDefs): tDefs;
PROCEDURE mModule1 (pIdent: tIdent; pExportList: tDefs): tDefs;
PROCEDURE mProc1 (pIdent: tIdent; pType: tDefs): tDefs;
PROCEDURE mProcHead1 (pIdent: tIdent; pType: tDefs): tDefs;
PROCEDURE mTypeDecl1 (pIdent: tIdent; pTypePos: SHORTCARD): tDefs;
PROCEDURE mOpaque1 (pIdent: tIdent; pTypePos: SHORTCARD): tDefs;
PROCEDURE mVar1 (pIdent: tIdent; pType: tDefs; pIsVAR: BOOLEAN; pLevel: SHORTCARD; pNestedUse: BOOLEAN; pTypeTree: ADDRESS): tDefs;
PROCEDURE mStdProc1 (pIdent: tIdent; pStdProc: SHORTCARD; pType: tDefs): tDefs;
PROCEDURE mType (): tDefs;
PROCEDURE mType0 (): tDefs;
PROCEDURE mShortInt (): tDefs;
PROCEDURE mLongInt (): tDefs;
PROCEDURE mShortCard (): tDefs;
PROCEDURE mLongCard (): tDefs;
PROCEDURE mReal (): tDefs;
PROCEDURE mLongReal (): tDefs;
PROCEDURE mBool (): tDefs;
PROCEDURE mChar (): tDefs;
PROCEDURE mBitset (): tDefs;
PROCEDURE mProc (): tDefs;
PROCEDURE mWord (): tDefs;
PROCEDURE mAddress (): tDefs;
PROCEDURE mIntCard (): tDefs;
PROCEDURE mNil (): tDefs;
PROCEDURE mString (): tDefs;
PROCEDURE mStringChar (): tDefs;
PROCEDURE mVoid (): tDefs;
PROCEDURE mStdProcType1 (pStdProc: SHORTCARD): tDefs;
PROCEDURE mQualident1 (pObject: tDefs): tDefs;
PROCEDURE mConstructor (pTypeObj: tDefs): tDefs;
PROCEDURE mOpaqueType1 (pTypeObj: tDefs): tDefs;
PROCEDURE mArray1 (pTypeObj: tDefs; pStructId: tIdent): tDefs;
PROCEDURE mEnumeration1 (pTypeObj: tDefs): tDefs;
PROCEDURE mPointer1 (pTypeObj: tDefs): tDefs;
PROCEDURE mProcType1 (pTypeObj: tDefs): tDefs;
PROCEDURE mRecord1 (pTypeObj: tDefs; pStructId: tIdent): tDefs;
PROCEDURE mSet1 (pTypeObj: tDefs): tDefs;
PROCEDURE mSubrange1 (pTypeObj: tDefs; pLwbExpr: ADDRESS; pUpbExpr: ADDRESS): tDefs;
PROCEDURE mObjects (): tDefs;
PROCEDURE mElmt (pIdent: tIdent; pIsPseudoObj: BOOLEAN; pObject: tDefs; pNext: tDefs): tDefs;
PROCEDURE mUnion (pObjects1: tDefs; pObjects2: tDefs): tDefs;
PROCEDURE mCObjects (pM2Object: tObject; pNext: tDefs): tDefs;
PROCEDURE mTypes (pIsVAR: BOOLEAN; pType: tDefs; pNext: tDefs): tDefs;
PROCEDURE mEnv (pObjects: tDefs; pHiddenEnv: tDefs): tDefs;
PROCEDURE mSelectors (pSelector: tIdent; pNext: tDefs): tDefs;
PROCEDURE mStringPar (pCString: tIdent; pFormalType: tDefs; pM2String: ADDRESS; pNext: tDefs): tDefs;

PROCEDURE BeginDefs;
PROCEDURE CloseDefs;

END Defs.
