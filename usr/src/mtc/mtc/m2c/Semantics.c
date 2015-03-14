#include "SYSTEM_.h"

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_UniqueIds
#include "UniqueIds.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_GenIdents
#include "GenIdents.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Types
#include "Types.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Values
#include "Values.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_UniqueIds
#include "UniqueIds.h"
#endif

#ifndef DEFINITION_GenIdents
#include "GenIdents.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_GenIdents
#include "GenIdents.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Tree
#include "Tree.h"
#endif

#ifndef DEFINITION_Code
#include "Code.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_GenIdents
#include "GenIdents.h"
#endif

#ifndef DEFINITION_Defs
#include "Defs.h"
#endif

#ifndef DEFINITION_Semantics
#include "Semantics.h"
#endif


static BOOLEAN yyb;
static void yyVisit1ROOT ARGS((Tree_tTree yyt));
struct S_1 {
    union {
        struct {
            Defs_tVoid ROOTyCompUnitsyObjects2;
            Defs_tObjects ROOTyCompUnitsyObjects3;
            Defs_tVoid ROOTyCompUnitsyObjects4In;
            Defs_tVoid ROOTyCompUnitsyObjects4Out;
            Idents_tIdent ROOTyCompUnitsyImplIdent;
            Defs_tObjects ROOTyCompUnitsyImplTypes;
            Defs_tEnv ROOTyCompUnitsyEnv1;
            Defs_tEnv ROOTyCompUnitsyEnv3;
            UniqueIds_tIdents ROOTyCompUnitsyIdsIn;
            UniqueIds_tIdents ROOTyCompUnitsyIdsOut;
            SHORTCARD ROOTyCompUnitsyPosOut;
        } V_1;
    } U_1;
};
static void yyVisit1CompUnits ARGS((Tree_tTree yyt));
struct S_2 {
    union {
        struct {
            SHORTCARD DefModyDeclsyPosIn;
            SHORTCARD DefModyDeclsyPosOut;
        } V_1;
        struct {
            SHORTCARD ProgModyDeclsyPosIn;
            SHORTCARD ProgModyDeclsyPosOut;
        } V_2;
    } U_1;
};
static void yyVisit2CompUnits ARGS((Tree_tTree yyt, Defs_tVoid *yyObjects2, Defs_tEnv *yyEnv1));
struct S_3 {
    union {
        struct {
            Defs_tVoid DefModyNextyObjects2;
            Defs_tObjects DefModyImportyObjects1;
            Defs_tVoid DefModyDeclsyObjects2;
        } V_1;
        struct {
            Defs_tVoid ProgModyNextyObjects2;
            Defs_tObjects ProgModyImportyObjects1;
            Defs_tVoid ProgModyDeclsyObjects2;
            Defs_tObjects ProgModyyDefObjects1;
        } V_2;
    } U_1;
};
static void yyVisit3CompUnits ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects3, Idents_tIdent *yyImplIdent, Defs_tObjects *yyImplTypes, Defs_tVoid *yyEnv2));
struct S_4 {
    union {
        struct {
            Idents_tIdent CompUnityNextyImplIdent;
            Defs_tObjects CompUnityNextyImplTypes;
        } V_1;
        struct {
            Defs_tObjects DefModyNextyObjects3;
            Defs_tObjects DefModyDeclsyImplTypes;
        } V_2;
        struct {
            Defs_tObjects ProgModyNextyObjects3;
            Idents_tIdent ProgModyNextyImplIdent;
            Defs_tObjects ProgModyNextyImplTypes;
            Defs_tObjects ProgModyDeclsyImplTypes;
        } V_3;
    } U_1;
};
static void yyVisit4CompUnits ARGS((Tree_tTree yyt, Defs_tVoid *yyObjects4In, Defs_tVoid *yyObjects4Out, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut, SHORTCARD *yyPosOut));
struct S_5 {
    union {
        struct {
            UniqueIds_tIdents DefModyNextyIdsIn;
            Defs_tEnv DefModyDeclsyEnv4;
            BOOLEAN DefModyDeclsyInLocal;
            SHORTCARD DefModyDeclsyCntOut;
            BOOLEAN DefModyDeclsyGlobalPtrs;
        } V_1;
        struct {
            UniqueIds_tIdents ProgModyNextyIdsIn;
            Defs_tEnv ProgModyDeclsyEnv4;
            BOOLEAN ProgModyDeclsyInLocal;
            SHORTCARD ProgModyDeclsyCntOut;
            BOOLEAN ProgModyDeclsyGlobalPtrs;
            SHORTCARD ProgModyStmtsyLevel;
            BOOLEAN ProgModyStmtsyGlobalPtrs;
            Defs_tStrings ProgModyStmtsyStrsIn;
            Defs_tStrings ProgModyStmtsyStrsOut;
            Defs_tType ProgModyStmtsyType;
            Defs_tObjects ProgModyyDefObjects3;
        } V_2;
    } U_1;
};
static void yyVisit1Import ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects1));
struct S_6 {
    union {
        struct {
            Defs_tObjects FromyNextyObjects1;
            Defs_tObjects FromyImpIdsyObjects1;
            Defs_tEnv FromyImpIdsyEnv1;
            Defs_tObject FromyyObject1;
        } V_1;
        struct {
            Defs_tObjects ObjectsyNextyObjects1;
            Defs_tObjects ObjectsyImpIdsyObjects1;
            Defs_tEnv ObjectsyImpIdsyEnv1;
        } V_2;
    } U_1;
};
static void yyVisit2Import ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv2));
struct S_7 {
    union {
        struct {
            Defs_tObjects Import1yNextyObjects1;
        } V_1;
        struct {
            Defs_tObjects FromyImpIdsyObjects2;
            Defs_tEnv FromyImpIdsyEnv2;
            Defs_tObject FromyyObject2;
        } V_2;
        struct {
            Defs_tObjects ObjectsyImpIdsyObjects2;
        } V_3;
    } U_1;
};
static void yyVisit1ImpIds ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects1, Defs_tEnv *yyEnv1));
struct S_8 {
    union {
        struct {
            Defs_tObjects ImpIds1yNextyObjects1;
            Defs_tObject ImpIds1yyObject1;
        } V_1;
    } U_1;
};
static void yyVisit2ImpIds ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects2, Defs_tEnv *yyEnv2));
struct S_9 {
    union {
        struct {
            Defs_tObjects ImpIds1yNextyObjects2;
            Defs_tType ImpIds1yyType;
            Defs_tObject ImpIds1yyObject2;
        } V_1;
    } U_1;
};
static void yyVisit1Export ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects1, Defs_tEnv *yyEnv1));
struct S_10 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit2Export ARGS((Tree_tTree yyt, Defs_tObjects *yyDefTypesIn, Defs_tObjects *yyDefTypesOut));
struct S_11 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit3Export ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv2));
struct S_12 {
    union {
        struct {
            Defs_tObjects Export1yExpIdsyObjects2;
        } V_1;
    } U_1;
};
static void yyVisit4Export ARGS((Tree_tTree yyt, Defs_tObjects *yyDefObjsIn, Defs_tObjects *yyDefObjsOut));
struct S_13 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit1ExpIds ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects1, Defs_tEnv *yyEnv1));
struct S_14 {
    union {
        struct {
            Defs_tObjects ExpIds1yNextyObjects1;
        } V_1;
    } U_1;
};
static void yyVisit2ExpIds ARGS((Tree_tTree yyt, Defs_tObjects *yyDefTypesIn, Defs_tObjects *yyDefTypesOut));
struct S_15 {
    union {
        struct {
            Defs_tObjects ExpIds1yNextyDefTypesOut;
        } V_1;
    } U_1;
};
static void yyVisit3ExpIds ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects2, Defs_tEnv *yyEnv2));
struct S_16 {
    union {
        struct {
            Defs_tObjects ExpIds1yNextyObjects2;
            Defs_tType ExpIds1yyType;
        } V_1;
    } U_1;
};
static void yyVisit4ExpIds ARGS((Tree_tTree yyt, Defs_tObjects *yyDefObjsIn, Defs_tObjects *yyDefObjsOut));
struct S_17 {
    union {
        struct {
            Defs_tObjects ExpIds1yNextyDefObjsOut;
        } V_1;
    } U_1;
};
static void yyVisit1Decls ARGS((Tree_tTree yyt, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_18 {
    union {
        struct {
            SHORTCARD VaryNextyPosIn;
            SHORTCARD VaryTypeyCntOut;
        } V_1;
        struct {
            SHORTCARD TypeDeclyNextyPosIn;
            SHORTCARD TypeDeclyTypeyCntOut;
        } V_2;
        struct {
            SHORTCARD ProcyNextyPosIn;
            SHORTCARD ProcyFormalsyKind;
            Idents_tIdent ProcyFormalsyModule;
            SHORTCARD ProcyFormalsyPosOut;
            SHORTCARD ProcyResultTypeyCntOut;
            SHORTCARD ProcyResultTypeyPosOut;
            SHORTCARD ProcyDeclsyPosOut;
        } V_3;
        struct {
            SHORTCARD ProcHeadyNextyPosIn;
            SHORTCARD ProcHeadyFormalsyKind;
            Idents_tIdent ProcHeadyFormalsyModule;
            SHORTCARD ProcHeadyFormalsyPosOut;
            SHORTCARD ProcHeadyResultTypeyCntOut;
            SHORTCARD ProcHeadyResultTypeyPosOut;
        } V_4;
        struct {
            SHORTCARD ModuleyNextyPosIn;
            Defs_tObjects ModuleyExportyObjects1;
            Defs_tEnv ModuleyExportyEnv1;
        } V_5;
        struct {
            SHORTCARD OpaqueyNextyPosIn;
        } V_6;
    } U_1;
};
static void yyVisit2Decls ARGS((Tree_tTree yyt, Defs_tVoid *yyObjects2));
struct S_19 {
    union {
        struct {
            Defs_tVoid TypeDeclyNextyObjects2;
            Defs_tObject TypeDeclyTypeyTypeObj;
        } V_1;
        struct {
            Defs_tVoid ProcyNextyObjects2;
            Defs_tVoid ProcyDeclsyObjects2;
        } V_2;
        struct {
            Defs_tVoid ModuleyNextyObjects2;
            Defs_tObjects ModuleyImportyObjects1;
            Defs_tObjects ModuleyExportyDefTypesIn;
            Defs_tObjects ModuleyExportyDefTypesOut;
            Defs_tVoid ModuleyDeclsyObjects2;
        } V_3;
        struct {
            Defs_tVoid OpaqueyNextyObjects2;
        } V_4;
    } U_1;
};
static void yyVisit3Decls ARGS((Tree_tTree yyt, Defs_tObjects *yyImplTypes));
struct S_20 {
    union {
        struct {
            Defs_tType VaryVarIdsyType;
            ADDRESS VaryVarIdsyTypeTree;
            SHORTCARD VaryVarIdsyLevel;
            Defs_tObject VaryTypeyTypeObj;
        } V_1;
        struct {
            Defs_tType TypeDeclyyType2;
        } V_2;
        struct {
            Defs_tTypes ProcyFormalsyTypes;
            Defs_tEnv ProcyFormalsyEnv1;
            Defs_tVoid ProcyFormalsyEnv2;
            SHORTCARD ProcyFormalsyLevel;
            Defs_tObject ProcyResultTypeyTypeObj;
        } V_3;
        struct {
            Defs_tTypes ProcHeadyFormalsyTypes;
            Defs_tEnv ProcHeadyFormalsyEnv1;
            Defs_tVoid ProcHeadyFormalsyEnv2;
            SHORTCARD ProcHeadyFormalsyLevel;
            Defs_tObject ProcHeadyResultTypeyTypeObj;
            Defs_tType ProcHeadyyType;
        } V_4;
        struct {
            Defs_tEnv ModuleyExportyEnv2;
            Defs_tObjects ModuleyDeclsyImplTypes;
        } V_5;
        struct {
            Defs_tType OpaqueyyFullType;
            Defs_tType OpaqueyyType2;
        } V_6;
    } U_1;
};
static void yyVisit4Decls ARGS((Tree_tTree yyt));
struct S_21 {
    union {
        struct {
            Defs_tObjects ProcyDeclsyImplTypes;
        } V_1;
        struct {
            Defs_tEnv ModuleyImportyEnv2;
            Defs_tObjects ModuleyExportyDefObjsIn;
            Defs_tObjects ModuleyExportyDefObjsOut;
        } V_2;
    } U_1;
};
static void yyVisit5Decls ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv4, BOOLEAN *yyInLocal, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut, SHORTCARD *yyCntOut, BOOLEAN *yyGlobalPtrs));
struct S_22 {
    union {
        struct {
            UniqueIds_tIdents VaryNextyIdsIn;
            SHORTCARD VaryVarIdsyKind;
            Idents_tIdent VaryVarIdsyModule;
            UniqueIds_tIdents VaryVarIdsyIdsOut;
            Defs_tEnv VaryTypeyEnv3;
        } V_1;
        struct {
            UniqueIds_tIdents ConstyNextyIdsIn;
            Defs_tEnv ConstyExpryEnv;
            SHORTCARD ConstyExpryLevel;
            BOOLEAN ConstyExpryOpenAccessOrCall;
            BOOLEAN ConstyExpryGlobalPtrs;
            Defs_tStrings ConstyExpryStrsIn;
            Defs_tStrings ConstyExpryStrsOut;
        } V_2;
        struct {
            UniqueIds_tIdents TypeDeclyNextyIdsIn;
            Defs_tEnv TypeDeclyTypeyEnv3;
            UniqueIds_tIdents TypeDeclyTypeyIdsOut;
        } V_3;
        struct {
            UniqueIds_tIdents ProcyNextyIdsIn;
            UniqueIds_tIdents ProcyNextyIdsOut;
            BOOLEAN ProcyNextyGlobalPtrs;
            Defs_tEnv ProcyFormalsyEnv3;
            UniqueIds_tIdents ProcyFormalsyIdsIn;
            UniqueIds_tIdents ProcyFormalsyIdsOut;
            Defs_tEnv ProcyResultTypeyEnv3;
            UniqueIds_tIdents ProcyResultTypeyIdsOut;
            Defs_tEnv ProcyDeclsyEnv4;
            BOOLEAN ProcyDeclsyInLocal;
            UniqueIds_tIdents ProcyDeclsyIdsOut;
            SHORTCARD ProcyDeclsyCntOut;
            BOOLEAN ProcyDeclsyGlobalPtrs;
            SHORTCARD ProcyStmtsyLevel;
            BOOLEAN ProcyStmtsyGlobalPtrs;
            Defs_tStrings ProcyStmtsyStrsIn;
            Defs_tStrings ProcyStmtsyStrsOut;
            Defs_tType ProcyStmtsyType;
        } V_4;
        struct {
            UniqueIds_tIdents ProcHeadyNextyIdsIn;
            Defs_tEnv ProcHeadyFormalsyEnv3;
            UniqueIds_tIdents ProcHeadyFormalsyIdsIn;
            UniqueIds_tIdents ProcHeadyFormalsyIdsOut;
            Defs_tEnv ProcHeadyResultTypeyEnv3;
            UniqueIds_tIdents ProcHeadyResultTypeyIdsIn;
            UniqueIds_tIdents ProcHeadyResultTypeyIdsOut;
        } V_5;
        struct {
            UniqueIds_tIdents ModuleyNextyIdsIn;
            UniqueIds_tIdents ModuleyNextyIdsOut;
            BOOLEAN ModuleyNextyGlobalPtrs;
            Defs_tEnv ModuleyDeclsyEnv4;
            BOOLEAN ModuleyDeclsyInLocal;
            SHORTCARD ModuleyDeclsyCntOut;
            BOOLEAN ModuleyDeclsyGlobalPtrs;
            SHORTCARD ModuleyStmtsyLevel;
            BOOLEAN ModuleyStmtsyGlobalPtrs;
            Defs_tStrings ModuleyStmtsyStrsIn;
            Defs_tStrings ModuleyStmtsyStrsOut;
            Defs_tType ModuleyStmtsyType;
        } V_6;
    } U_1;
};
static void yyVisit1VarIds ARGS((Tree_tTree yyt, Defs_tType *yyType, ADDRESS *yyTypeTree, SHORTCARD *yyLevel));
struct S_23 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit2VarIds ARGS((Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, BOOLEAN *yyInLocal, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_24 {
    union {
        struct {
            UniqueIds_tIdents VarIds1yNextyIdsIn;
        } V_1;
    } U_1;
};
static void yyVisit1Formals ARGS((Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_25 {
    union {
        struct {
            SHORTCARD Formals1yTypeyCntOut;
            SHORTCARD Formals1yTypeyPosOut;
        } V_1;
    } U_1;
};
static void yyVisit2Formals ARGS((Tree_tTree yyt, Defs_tTypes *yyTypes, Defs_tEnv *yyEnv1, Defs_tVoid *yyEnv2, SHORTCARD *yyLevel));
struct S_26 {
    union {
        struct {
            Defs_tObjects Formals1yParIdsyObjects3;
            Defs_tType Formals1yParIdsyType;
            ADDRESS Formals1yParIdsyTypeTree;
            BOOLEAN Formals1yParIdsyIsVAR;
            Defs_tType Formals1yParIdsyTypesIn;
            Defs_tObject Formals1yTypeyTypeObj;
        } V_1;
    } U_1;
};
static void yyVisit3Formals ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_27 {
    union {
        struct {
            UniqueIds_tIdents Formals1yParIdsyIdsOut;
            UniqueIds_tIdents Formals1yTypeyIdsOut;
        } V_1;
    } U_1;
};
static void yyVisit1ParIds ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects3, Defs_tType *yyType, ADDRESS *yyTypeTree, BOOLEAN *yyIsVAR, Defs_tType *yyTypesIn, Defs_tTypes *yyTypesOut, SHORTCARD *yyLevel));
struct S_28 {
    union {
        struct {
            Defs_tObjects ParIds1yNextyObjects3;
            Defs_tTypes ParIds1yNextyTypesOut;
        } V_1;
    } U_1;
};
static void yyVisit2ParIds ARGS((Tree_tTree yyt, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_29 {
    union {
        struct {
            UniqueIds_tIdents ParIds1yNextyIdsIn;
        } V_1;
    } U_1;
};
static void yyVisit1Type ARGS((Tree_tTree yyt, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_30 {
    union {
        struct {
            SHORTCARD ArrayyIndexTypeyCntOut;
            SHORTCARD ArrayyIndexTypeyPosOut;
            SHORTCARD ArrayyElemTypeyPosOut;
        } V_1;
        struct {
            SHORTCARD RecordyFieldsyKind;
            Idents_tIdent RecordyFieldsyModule;
            SHORTCARD RecordyFieldsyCntIn;
            SHORTCARD RecordyFieldsyPosOut;
        } V_2;
        struct {
            SHORTCARD SetTypeyBaseTypeyCntOut;
            SHORTCARD SetTypeyBaseTypeyPosOut;
        } V_3;
        struct {
            SHORTCARD PointeryTargetTypeyPosOut;
        } V_4;
        struct {
            SHORTCARD ProcTypeyFormalTypesyKind;
            Idents_tIdent ProcTypeyFormalTypesyModule;
            SHORTCARD ProcTypeyFormalTypesyPosOut;
            SHORTCARD ProcTypeyResultTypeyCntOut;
            SHORTCARD ProcTypeyResultTypeyPosOut;
        } V_5;
        struct {
            SHORTCARD SubrangeyBaseTypeyCntOut;
            SHORTCARD SubrangeyBaseTypeyPosOut;
        } V_6;
        struct {
            SHORTCARD TypeId1yTypeIdyCntOut;
            SHORTCARD TypeId1yTypeIdyPosOut;
        } V_7;
    } U_1;
};
static void yyVisit2Type ARGS((Tree_tTree yyt, Defs_tObject *yyTypeObj));
struct S_31 {
    union {
        struct {
            SHORTCARD EnumerationyEnumIdsyIndexIn;
            SHORTCARD EnumerationyEnumIdsyIndexOut;
            Defs_tType EnumerationyyType0;
        } V_1;
    } U_1;
};
static void yyVisit3Type ARGS((Tree_tTree yyt));
struct S_32 {
    union {
        struct {
            Defs_tObject ArrayyIndexTypeyTypeObj;
            Defs_tObject ArrayyElemTypeyTypeObj;
        } V_1;
        struct {
            Defs_tObjects RecordyFieldsyObjects3;
            Defs_tObjects RecordyFieldsyFieldsIn;
            Defs_tObjects RecordyFieldsyFieldsOut;
            Defs_tEnv RecordyFieldsyEnv1;
            Defs_tVoid RecordyFieldsyEnv2;
            SHORTCARD RecordyFieldsynUnion;
            Defs_tSelectors RecordyFieldsySelect;
        } V_2;
        struct {
            Defs_tObject SetTypeyBaseTypeyTypeObj;
        } V_3;
        struct {
            Defs_tObject PointeryTargetTypeyTypeObj;
        } V_4;
        struct {
            Defs_tTypes ProcTypeyFormalTypesyTypes;
            Defs_tEnv ProcTypeyFormalTypesyEnv1;
            Defs_tVoid ProcTypeyFormalTypesyEnv2;
            Defs_tObject ProcTypeyResultTypeyTypeObj;
        } V_5;
        struct {
            Defs_tObject SubrangeyBaseTypeyTypeObj;
        } V_6;
    } U_1;
};
static void yyVisit4Type ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_33 {
    union {
        struct {
            UniqueIds_tIdents ArrayyIndexTypeyIdsOut;
        } V_1;
        struct {
            UniqueIds_tIdents ProcTypeyFormalTypesyIdsOut;
        } V_2;
        struct {
            SHORTCARD EnumerationyEnumIdsyKind;
            Idents_tIdent EnumerationyEnumIdsyModule;
        } V_3;
        struct {
            SHORTCARD SubrangeyLwbyLevel;
            BOOLEAN SubrangeyLwbyOpenAccessOrCall;
            BOOLEAN SubrangeyLwbyGlobalPtrs;
            Defs_tStrings SubrangeyLwbyStrsIn;
            Defs_tStrings SubrangeyLwbyStrsOut;
            SHORTCARD SubrangeyUpbyLevel;
            BOOLEAN SubrangeyUpbyOpenAccessOrCall;
            BOOLEAN SubrangeyUpbyGlobalPtrs;
            Defs_tStrings SubrangeyUpbyStrsIn;
            Defs_tStrings SubrangeyUpbyStrsOut;
            Defs_tType SubrangeyyType3;
        } V_4;
    } U_1;
};
static void yyVisit1SimpleType ARGS((Tree_tTree yyt, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_34 {
    union {
        struct {
            SHORTCARD SubrangeyBaseTypeyCntOut;
            SHORTCARD SubrangeyBaseTypeyPosOut;
        } V_1;
        struct {
            SHORTCARD TypeId1yTypeIdyCntOut;
            SHORTCARD TypeId1yTypeIdyPosOut;
        } V_2;
    } U_1;
};
static void yyVisit2SimpleType ARGS((Tree_tTree yyt, Defs_tObject *yyTypeObj));
struct S_35 {
    union {
        struct {
            SHORTCARD EnumerationyEnumIdsyIndexIn;
            SHORTCARD EnumerationyEnumIdsyIndexOut;
            Defs_tType EnumerationyyType0;
        } V_1;
    } U_1;
};
static void yyVisit3SimpleType ARGS((Tree_tTree yyt));
struct S_36 {
    union {
        struct {
            Defs_tObject SubrangeyBaseTypeyTypeObj;
        } V_1;
    } U_1;
};
static void yyVisit4SimpleType ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_37 {
    union {
        struct {
            SHORTCARD EnumerationyEnumIdsyKind;
            Idents_tIdent EnumerationyEnumIdsyModule;
        } V_1;
        struct {
            SHORTCARD SubrangeyLwbyLevel;
            BOOLEAN SubrangeyLwbyOpenAccessOrCall;
            BOOLEAN SubrangeyLwbyGlobalPtrs;
            Defs_tStrings SubrangeyLwbyStrsIn;
            Defs_tStrings SubrangeyLwbyStrsOut;
            SHORTCARD SubrangeyUpbyLevel;
            BOOLEAN SubrangeyUpbyOpenAccessOrCall;
            BOOLEAN SubrangeyUpbyGlobalPtrs;
            Defs_tStrings SubrangeyUpbyStrsIn;
            Defs_tStrings SubrangeyUpbyStrsOut;
            Defs_tType SubrangeyyType3;
        } V_2;
    } U_1;
};
static void yyVisit1PrimaryType ARGS((Tree_tTree yyt, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_38 {
    union {
        struct {
            SHORTCARD TypeId1yTypeIdyCntOut;
            SHORTCARD TypeId1yTypeIdyPosOut;
        } V_1;
    } U_1;
};
static void yyVisit2PrimaryType ARGS((Tree_tTree yyt, Defs_tObject *yyTypeObj));
struct S_39 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit3PrimaryType ARGS((Tree_tTree yyt));
struct S_40 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit4PrimaryType ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_41 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit1TypeId ARGS((Tree_tTree yyt, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_42 {
    union {
        struct {
            SHORTCARD TypeId1yTypeIdyCntOut;
            SHORTCARD TypeId1yTypeIdyPosOut;
        } V_1;
    } U_1;
};
static void yyVisit2TypeId ARGS((Tree_tTree yyt, Defs_tObject *yyTypeObj));
struct S_43 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit3TypeId ARGS((Tree_tTree yyt));
struct S_44 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit4TypeId ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_45 {
    union {
        char dummy;
    } U_1;
};
static void yyVisit1Fields ARGS((Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyCntIn, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_46 {
    union {
        struct {
            SHORTCARD RecordSectyNextyCntIn;
            SHORTCARD RecordSectyNextyPosIn;
        } V_1;
        struct {
            SHORTCARD VariantSectyNextyCntIn;
            SHORTCARD VariantSectyNextyPosIn;
            SHORTCARD VariantSectyTagFieldyPosOut;
            SHORTCARD VariantSectyVariantsyCntOut;
            SHORTCARD VariantSectyVariantsyPosOut;
        } V_2;
    } U_1;
};
static void yyVisit2Fields ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects3, Defs_tObjects *yyFieldsIn, Defs_tObjects *yyFieldsOut, Defs_tEnv *yyEnv1, Defs_tVoid *yyEnv2, SHORTCARD *yynUnion, Defs_tSelectors *yySelect));
struct S_47 {
    union {
        struct {
            Defs_tObjects RecordSectyNextyObjects3;
            Defs_tObjects RecordSectyNextyFieldsOut;
            Defs_tType RecordSectyFieldIdsyType;
            Defs_tObject RecordSectyTypeyTypeObj;
        } V_1;
        struct {
            Defs_tObjects VariantSectyNextyObjects3;
            Defs_tObjects VariantSectyNextyFieldsOut;
            SHORTCARD VariantSectyNextynUnion;
            Defs_tObjects VariantSectyTagFieldyFieldsIn;
            Defs_tObjects VariantSectyVariantsyObjects3;
            Defs_tObjects VariantSectyVariantsyFieldsIn;
            SHORTCARD VariantSectyVariantsynStruct;
            Defs_tSelectors VariantSectyVariantsySelect;
            Defs_tObjects VariantSectyElseyObjects3;
            SHORTCARD VariantSectyElseynUnion;
            Defs_tSelectors VariantSectyElseySelect;
        } V_2;
    } U_1;
};
static void yyVisit3Fields ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_48 {
    union {
        struct {
            UniqueIds_tIdents RecordSectyNextyIdsIn;
            UniqueIds_tIdents RecordSectyFieldIdsyIdsOut;
        } V_1;
        struct {
            UniqueIds_tIdents VariantSectyNextyIdsIn;
            UniqueIds_tIdents VariantSectyTagFieldyIdsOut;
            UniqueIds_tIdents VariantSectyVariantsyIdsOut;
        } V_2;
    } U_1;
};
static void yyVisit1FieldIds ARGS((Tree_tTree yyt, Defs_tType *yyType, Defs_tObjects *yyFieldsIn, Defs_tObjects *yyFieldsOut, Defs_tSelectors *yySelect));
struct S_49 {
    union {
        struct {
            Defs_tObjects FieldIds1yNextyFieldsOut;
        } V_1;
    } U_1;
};
static void yyVisit2FieldIds ARGS((Tree_tTree yyt, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_50 {
    union {
        struct {
            UniqueIds_tIdents FieldIds1yNextyIdsIn;
        } V_1;
    } U_1;
};
static void yyVisit1TagField ARGS((Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_51 {
    union {
        struct {
            SHORTCARD TagFieldyTypeyCntOut;
        } V_1;
        struct {
            SHORTCARD TagField0yTypeyCntOut;
        } V_2;
        struct {
            SHORTCARD TagField1yTypeyCntOut;
        } V_3;
    } U_1;
};
static void yyVisit2TagField ARGS((Tree_tTree yyt, Defs_tObjects *yyFieldsIn, Defs_tObjects *yyFieldsOut, Defs_tSelectors *yySelect));
struct S_52 {
    union {
        struct {
            Defs_tObject TagField1yTypeyTypeObj;
        } V_1;
    } U_1;
};
static void yyVisit3TagField ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_53 {
    union {
        struct {
            Defs_tObject TagFieldyTypeyTypeObj;
        } V_1;
        struct {
            Defs_tObject TagField0yTypeyTypeObj;
        } V_2;
        struct {
            UniqueIds_tIdents TagField1yTypeyIdsOut;
        } V_3;
    } U_1;
};
static void yyVisit1Variants ARGS((Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyCntIn, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_54 {
    union {
        struct {
            SHORTCARD VariantyVariantyCntOut;
            SHORTCARD VariantyVariantyPosOut;
        } V_1;
    } U_1;
};
static void yyVisit2Variants ARGS((Tree_tTree yyt, Defs_tObjects *yyObjects3, Defs_tObjects *yyFieldsIn, Defs_tObjects *yyFieldsOut, Defs_tEnv *yyEnv1, Defs_tVoid *yyEnv2, SHORTCARD *yynStruct, Defs_tSelectors *yySelect));
struct S_55 {
    union {
        struct {
            Defs_tObjects VariantyVariantyObjects3;
            Defs_tObjects VariantyVariantyFieldsIn;
            SHORTCARD VariantyVariantynUnion;
            Defs_tSelectors VariantyVariantySelect;
            Defs_tObjects VariantyNextyObjects3;
            SHORTCARD VariantyNextynStruct;
        } V_1;
    } U_1;
};
static void yyVisit3Variants ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_56 {
    union {
        struct {
            UniqueIds_tIdents VariantyVariantyIdsOut;
        } V_1;
    } U_1;
};
static void yyVisit1FormalTypes ARGS((Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut));
struct S_57 {
    union {
        struct {
            SHORTCARD FormalTypeyTypeyCntOut;
            SHORTCARD FormalTypeyTypeyPosOut;
        } V_1;
    } U_1;
};
static void yyVisit2FormalTypes ARGS((Tree_tTree yyt, Defs_tTypes *yyTypes, Defs_tEnv *yyEnv1, Defs_tVoid *yyEnv2));
struct S_58 {
    union {
        struct {
            Defs_tObject FormalTypeyTypeyTypeObj;
            Defs_tTypes FormalTypeyNextyTypes;
        } V_1;
    } U_1;
};
static void yyVisit3FormalTypes ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_59 {
    union {
        struct {
            UniqueIds_tIdents FormalTypeyTypeyIdsOut;
        } V_1;
    } U_1;
};
static void yyVisit1EnumIds ARGS((Tree_tTree yyt, Defs_tType *yyType, SHORTCARD *yyIndexIn, SHORTCARD *yyIndexOut));
struct S_60 {
    union {
        struct {
            SHORTCARD EnumIds1yNextyIndexIn;
        } V_1;
    } U_1;
};
static void yyVisit2EnumIds ARGS((Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut));
struct S_61 {
    union {
        struct {
            UniqueIds_tIdents EnumIds1yNextyIdsIn;
        } V_1;
    } U_1;
};
static void yyVisit1Expr ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyOpenAccessOrCall, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut));
struct S_62 {
    union {
        struct {
            BOOLEAN BinaryyLopyOpenAccessOrCall;
            BOOLEAN BinaryyLopyGlobalPtrs;
            Defs_tStrings BinaryyLopyStrsOut;
            BOOLEAN BinaryyRopyOpenAccessOrCall;
            BOOLEAN BinaryyRopyGlobalPtrs;
        } V_1;
        struct {
            BOOLEAN FuncCallyDesignatoryOpenAccessOrCall;
            BOOLEAN FuncCallyDesignatoryGlobalPtrs;
            Defs_tStrings FuncCallyDesignatoryStrsOut;
            Defs_tTypes FuncCallyActualsyTypes;
            Defs_tTypes FuncCallyActualsyFormals;
            BOOLEAN FuncCallyActualsyIsCConst;
            BOOLEAN FuncCallyActualsyGlobalPtrs;
        } V_2;
        struct {
            BOOLEAN SetyBaseTypeyOpenAccessOrCall;
            BOOLEAN SetyBaseTypeyGlobalPtrs;
            Defs_tStrings SetyBaseTypeyStrsOut;
            BOOLEAN SetyElemsyIsCConst;
        } V_3;
        struct {
            BOOLEAN BitSetyElemsyIsCConst;
        } V_4;
        struct {
            BOOLEAN Qualid1yQualidyOpenAccessOrCall;
            BOOLEAN Qualid1yQualidyGlobalPtrs;
        } V_5;
        struct {
            BOOLEAN SubscriptyDesignatoryGlobalPtrs;
            Defs_tStrings SubscriptyDesignatoryStrsOut;
            BOOLEAN SubscriptyIndexyOpenAccessOrCall;
            BOOLEAN SubscriptyIndexyGlobalPtrs;
        } V_6;
        struct {
            BOOLEAN DerefyDesignatoryOpenAccessOrCall;
        } V_7;
    } U_1;
};
static void yyVisit1Designator ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyOpenAccessOrCall, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut));
struct S_63 {
    union {
        struct {
            BOOLEAN Qualid1yQualidyOpenAccessOrCall;
            BOOLEAN Qualid1yQualidyGlobalPtrs;
        } V_1;
        struct {
            BOOLEAN SubscriptyDesignatoryGlobalPtrs;
            Defs_tStrings SubscriptyDesignatoryStrsOut;
            BOOLEAN SubscriptyIndexyOpenAccessOrCall;
            BOOLEAN SubscriptyIndexyGlobalPtrs;
        } V_2;
        struct {
            BOOLEAN DerefyDesignatoryOpenAccessOrCall;
        } V_3;
    } U_1;
};
static void yyVisit1Qualid ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyOpenAccessOrCall, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut));
struct S_64 {
    union {
        struct {
            BOOLEAN Qualid1yQualidyOpenAccessOrCall;
            BOOLEAN Qualid1yQualidyGlobalPtrs;
        } V_1;
    } U_1;
};
static void yyVisit1Elems ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyIsCConst, BOOLEAN *yyOpenAccessOrCall, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut));
struct S_65 {
    union {
        struct {
            BOOLEAN Elems1yNextyIsCConst;
            BOOLEAN Elems1yNextyOpenAccessOrCall;
        } V_1;
        struct {
            BOOLEAN ElemyNextyIsCConst;
            BOOLEAN ElemyNextyOpenAccessOrCall;
            BOOLEAN ElemyNextyGlobalPtrs;
            Defs_tStrings ElemyNextyStrsIn;
            BOOLEAN ElemyElemyOpenAccessOrCall;
            BOOLEAN ElemyElemyGlobalPtrs;
        } V_2;
        struct {
            BOOLEAN ElemRangeyNextyIsCConst;
            BOOLEAN ElemRangeyNextyOpenAccessOrCall;
            BOOLEAN ElemRangeyNextyGlobalPtrs;
            Defs_tStrings ElemRangeyNextyStrsIn;
            BOOLEAN ElemRangeyLwbyOpenAccessOrCall;
            BOOLEAN ElemRangeyLwbyGlobalPtrs;
            Defs_tStrings ElemRangeyLwbyStrsOut;
            BOOLEAN ElemRangeyUpbyOpenAccessOrCall;
            BOOLEAN ElemRangeyUpbyGlobalPtrs;
        } V_3;
    } U_1;
};
static void yyVisit1Actuals ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv, Defs_tTypes *yyTypes, Defs_tTypes *yyFormals, SHORTCARD *yyLevel, BOOLEAN *yyIsCConst, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut));
struct S_66 {
    union {
        struct {
            BOOLEAN ActualyExpryOpenAccessOrCall;
            BOOLEAN ActualyExpryGlobalPtrs;
            Defs_tStrings ActualyExpryStrsOut;
            Defs_tTypes ActualyNextyTypes;
            Defs_tTypes ActualyNextyFormals;
            BOOLEAN ActualyNextyIsCConst;
            BOOLEAN ActualyNextyGlobalPtrs;
            Defs_tStrings ActualyNextyStrsOut;
        } V_1;
    } U_1;
};
static void yyVisit1Stmts ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut, Defs_tType *yyType));
struct S_67 {
    union {
        struct {
            BOOLEAN AssignyNextyGlobalPtrs;
            Defs_tStrings AssignyNextyStrsIn;
            BOOLEAN AssignyDesignatoryOpenAccessOrCall;
            BOOLEAN AssignyDesignatoryGlobalPtrs;
            Defs_tStrings AssignyDesignatoryStrsOut;
            BOOLEAN AssignyExpryOpenAccessOrCall;
            BOOLEAN AssignyExpryGlobalPtrs;
        } V_1;
        struct {
            BOOLEAN CallyNextyGlobalPtrs;
            Defs_tStrings CallyNextyStrsIn;
            BOOLEAN CallyDesignatoryOpenAccessOrCall;
            BOOLEAN CallyDesignatoryGlobalPtrs;
            Defs_tStrings CallyDesignatoryStrsOut;
            Defs_tTypes CallyActualsyTypes;
            Defs_tTypes CallyActualsyFormals;
            BOOLEAN CallyActualsyIsCConst;
            BOOLEAN CallyActualsyGlobalPtrs;
        } V_2;
        struct {
            BOOLEAN IfyNextyGlobalPtrs;
            Defs_tStrings IfyNextyStrsIn;
            BOOLEAN IfyCondyOpenAccessOrCall;
            BOOLEAN IfyCondyGlobalPtrs;
            Defs_tStrings IfyCondyStrsOut;
            BOOLEAN IfyThenyGlobalPtrs;
            Defs_tStrings IfyThenyStrsOut;
            BOOLEAN IfyElsifsyGlobalPtrs;
            Defs_tStrings IfyElsifsyStrsOut;
            BOOLEAN IfyElseyGlobalPtrs;
        } V_3;
        struct {
            BOOLEAN CaseyNextyGlobalPtrs;
            Defs_tStrings CaseyNextyStrsIn;
            BOOLEAN CaseyExpryOpenAccessOrCall;
            BOOLEAN CaseyExpryGlobalPtrs;
            Defs_tStrings CaseyExpryStrsOut;
            BOOLEAN CaseyCasesyGlobalPtrs;
            Defs_tStrings CaseyCasesyStrsOut;
            BOOLEAN CaseyElseyGlobalPtrs;
        } V_4;
        struct {
            BOOLEAN WhileyNextyGlobalPtrs;
            Defs_tStrings WhileyNextyStrsIn;
            BOOLEAN WhileyCondyOpenAccessOrCall;
            BOOLEAN WhileyCondyGlobalPtrs;
            Defs_tStrings WhileyCondyStrsOut;
            BOOLEAN WhileyStmtsyGlobalPtrs;
        } V_5;
        struct {
            BOOLEAN RepeatyNextyGlobalPtrs;
            Defs_tStrings RepeatyNextyStrsIn;
            BOOLEAN RepeatyStmtsyGlobalPtrs;
            Defs_tStrings RepeatyStmtsyStrsOut;
            BOOLEAN RepeatyCondyOpenAccessOrCall;
            BOOLEAN RepeatyCondyGlobalPtrs;
        } V_6;
        struct {
            BOOLEAN LoopyNextyGlobalPtrs;
            Defs_tStrings LoopyNextyStrsIn;
            BOOLEAN LoopyStmtsyGlobalPtrs;
        } V_7;
        struct {
            BOOLEAN ForyNextyGlobalPtrs;
            Defs_tStrings ForyNextyStrsIn;
            BOOLEAN ForyQualidyOpenAccessOrCall;
            BOOLEAN ForyQualidyGlobalPtrs;
            Defs_tStrings ForyQualidyStrsOut;
            BOOLEAN ForyFromyOpenAccessOrCall;
            BOOLEAN ForyFromyGlobalPtrs;
            Defs_tStrings ForyFromyStrsOut;
            BOOLEAN ForyToyOpenAccessOrCall;
            BOOLEAN ForyToyGlobalPtrs;
            Defs_tStrings ForyToyStrsOut;
            BOOLEAN ForyByyOpenAccessOrCall;
            BOOLEAN ForyByyGlobalPtrs;
            Defs_tStrings ForyByyStrsOut;
            BOOLEAN ForyStmtsyGlobalPtrs;
        } V_8;
        struct {
            BOOLEAN WithyNextyGlobalPtrs;
            Defs_tStrings WithyNextyStrsIn;
            BOOLEAN WithyDesignatoryOpenAccessOrCall;
            BOOLEAN WithyDesignatoryGlobalPtrs;
            Defs_tStrings WithyDesignatoryStrsOut;
            Defs_tEnv WithyStmtsyEnv;
            BOOLEAN WithyStmtsyGlobalPtrs;
        } V_9;
        struct {
            BOOLEAN Return2yNextyGlobalPtrs;
            Defs_tStrings Return2yNextyStrsIn;
            BOOLEAN Return2yResultyOpenAccessOrCall;
            BOOLEAN Return2yResultyGlobalPtrs;
        } V_10;
    } U_1;
};
static void yyVisit1Elsifs ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut, Defs_tType *yyType));
struct S_68 {
    union {
        struct {
            BOOLEAN Elsifs1yCondyOpenAccessOrCall;
            BOOLEAN Elsifs1yCondyGlobalPtrs;
            Defs_tStrings Elsifs1yCondyStrsOut;
            BOOLEAN Elsifs1yStmtsyGlobalPtrs;
            Defs_tStrings Elsifs1yStmtsyStrsOut;
            BOOLEAN Elsifs1yNextyGlobalPtrs;
        } V_1;
    } U_1;
};
static void yyVisit1Cases ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut, Defs_tType *yyType));
struct S_69 {
    union {
        struct {
            BOOLEAN Cases1yStmtsyGlobalPtrs;
            Defs_tStrings Cases1yStmtsyStrsOut;
            BOOLEAN Cases1yNextyGlobalPtrs;
        } V_1;
    } U_1;
};
static void yyVisit1Labels ARGS((Tree_tTree yyt, Defs_tEnv *yyEnv));
struct S_70 {
    union {
        struct {
            SHORTCARD LabelyLabelyLevel;
            BOOLEAN LabelyLabelyOpenAccessOrCall;
            BOOLEAN LabelyLabelyGlobalPtrs;
            Defs_tStrings LabelyLabelyStrsIn;
            Defs_tStrings LabelyLabelyStrsOut;
        } V_1;
        struct {
            SHORTCARD LabelRangeyLwbyLevel;
            BOOLEAN LabelRangeyLwbyOpenAccessOrCall;
            BOOLEAN LabelRangeyLwbyGlobalPtrs;
            Defs_tStrings LabelRangeyLwbyStrsIn;
            Defs_tStrings LabelRangeyLwbyStrsOut;
            SHORTCARD LabelRangeyUpbyLevel;
            BOOLEAN LabelRangeyUpbyOpenAccessOrCall;
            BOOLEAN LabelRangeyUpbyGlobalPtrs;
            Defs_tStrings LabelRangeyUpbyStrsIn;
            Defs_tStrings LabelRangeyUpbyStrsOut;
        } V_2;
    } U_1;
};


void Semantics_Eval
# ifdef __STDC__
(Tree_tTree yyt)
# else
(yyt)
Tree_tTree yyt;
# endif
{
  yyVisit1ROOT(yyt);
}

static void yyVisit1ROOT
# ifdef __STDC__
(Tree_tTree yyt)
# else
(yyt)
Tree_tTree yyt;
# endif
{
  struct S_1 yyTempo;

  {
    register struct S_1 *W_1 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ROOT:;
      yyt->U_1.V_3.ROOT.CompUnits->U_1.V_4.CompUnits.PosIn = 0;
      yyVisit1CompUnits(yyt->U_1.V_3.ROOT.CompUnits);
      W_1->U_1.V_1.ROOTyCompUnitsyEnv1 = Defs_mEnv(Defs_mElmt(Defs_IdentSYSTEM, FALSE, Defs_ModuleSYSTEM, yyt->U_1.V_3.ROOT.CompUnits->U_1.V_4.CompUnits.Objects1), (Defs_tDefs)Defs_NoEnv);
      yyVisit2CompUnits(yyt->U_1.V_3.ROOT.CompUnits, &W_1->U_1.V_1.ROOTyCompUnitsyObjects2, &W_1->U_1.V_1.ROOTyCompUnitsyEnv1);
      yyVisit3CompUnits(yyt->U_1.V_3.ROOT.CompUnits, &W_1->U_1.V_1.ROOTyCompUnitsyObjects3, &W_1->U_1.V_1.ROOTyCompUnitsyImplIdent, &W_1->U_1.V_1.ROOTyCompUnitsyImplTypes, &W_1->U_1.V_1.ROOTyCompUnitsyObjects2);
      W_1->U_1.V_1.ROOTyCompUnitsyIdsIn = UniqueIds_cIdents;
      W_1->U_1.V_1.ROOTyCompUnitsyEnv3 = Defs_mEnv(Defs_mElmt(Defs_IdentSYSTEM, FALSE, Defs_ModuleSYSTEM, W_1->U_1.V_1.ROOTyCompUnitsyObjects3), (Defs_tDefs)Defs_NoEnv);
      W_1->U_1.V_1.ROOTyCompUnitsyObjects4In = Defs_cVoid;
      yyVisit4CompUnits(yyt->U_1.V_3.ROOT.CompUnits, &W_1->U_1.V_1.ROOTyCompUnitsyObjects4In, &W_1->U_1.V_1.ROOTyCompUnitsyObjects4Out, &W_1->U_1.V_1.ROOTyCompUnitsyEnv3, &W_1->U_1.V_1.ROOTyCompUnitsyIdsIn, &W_1->U_1.V_1.ROOTyCompUnitsyIdsOut, &W_1->U_1.V_1.ROOTyCompUnitsyPosOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1CompUnits
# ifdef __STDC__
(Tree_tTree yyt)
# else
(yyt)
Tree_tTree yyt;
# endif
{
  struct S_2 yyTempo;

  {
    register struct S_2 *W_2 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_CompUnits:;
      yyt->U_1.V_4.CompUnits.Objects1 = Defs_NoObjects;
      break;
    case Tree_CompUnits0:;
      yyt->U_1.V_5.CompUnits0.Objects1 = Defs_NoObjects;
      break;
    case Tree_CompUnit:;
      yyt->U_1.V_6.CompUnit.Object = Defs_NoObject;
      yyt->U_1.V_6.CompUnit.Next->U_1.V_4.CompUnits.PosIn = yyt->U_1.V_6.CompUnit.PosIn;
      yyVisit1CompUnits(yyt->U_1.V_6.CompUnit.Next);
      yyt->U_1.V_6.CompUnit.Objects1 = yyt->U_1.V_6.CompUnit.Next->U_1.V_4.CompUnits.Objects1;
      break;
    case Tree_DefMod:;
      W_2->U_1.V_1.DefModyDeclsyPosIn = yyt->U_1.V_7.DefMod.PosIn;
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.CntIn = 0;
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Module = yyt->U_1.V_7.DefMod.Ident;
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Kind = yyt->U_1.V_7.DefMod.Kind;
      yyVisit1Decls(yyt->U_1.V_7.DefMod.Decls, &W_2->U_1.V_1.DefModyDeclsyPosIn, &W_2->U_1.V_1.DefModyDeclsyPosOut);
      yyt->U_1.V_7.DefMod.Object = Defs_mModule1(yyt->U_1.V_7.DefMod.Ident, yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Objects1);
      yyt->U_1.V_7.DefMod.Next->U_1.V_4.CompUnits.PosIn = W_2->U_1.V_1.DefModyDeclsyPosOut;
      yyVisit1CompUnits(yyt->U_1.V_7.DefMod.Next);
      yyt->U_1.V_7.DefMod.Objects1 = Defs_mElmt(yyt->U_1.V_7.DefMod.Ident, FALSE, yyt->U_1.V_7.DefMod.Object, yyt->U_1.V_7.DefMod.Next->U_1.V_4.CompUnits.Objects1);
      break;
    case Tree_ProgMod:;
      if (yyt->U_1.V_8.ProgMod.Kind == Tree_Program) {
        yyt->U_1.V_8.ProgMod.Object = Defs_mModule2(Defs_mModule1(yyt->U_1.V_8.ProgMod.Ident, (Defs_tDefs)Defs_NoObjects), (Defs_tDefs)Defs_NoObjects);
      } else {
        yyt->U_1.V_8.ProgMod.Object = Defs_NoObject;
      }
      W_2->U_1.V_2.ProgModyDeclsyPosIn = yyt->U_1.V_8.ProgMod.PosIn;
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.CntIn = 0;
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Module = yyt->U_1.V_8.ProgMod.Ident;
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Kind = yyt->U_1.V_8.ProgMod.Kind;
      yyVisit1Decls(yyt->U_1.V_8.ProgMod.Decls, &W_2->U_1.V_2.ProgModyDeclsyPosIn, &W_2->U_1.V_2.ProgModyDeclsyPosOut);
      yyt->U_1.V_8.ProgMod.Next->U_1.V_4.CompUnits.PosIn = W_2->U_1.V_2.ProgModyDeclsyPosOut;
      yyVisit1CompUnits(yyt->U_1.V_8.ProgMod.Next);
      if (yyt->U_1.V_8.ProgMod.Kind == Tree_Program) {
        yyt->U_1.V_8.ProgMod.Objects1 = Defs_mElmt(yyt->U_1.V_8.ProgMod.Ident, FALSE, yyt->U_1.V_8.ProgMod.Object, yyt->U_1.V_8.ProgMod.Next->U_1.V_4.CompUnits.Objects1);
      } else {
        yyt->U_1.V_8.ProgMod.Objects1 = yyt->U_1.V_8.ProgMod.Next->U_1.V_4.CompUnits.Objects1;
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit2CompUnits
# ifdef __STDC__
(Tree_tTree yyt, Defs_tVoid *yyObjects2, Defs_tEnv *yyEnv1)
# else
(yyt, yyObjects2, yyEnv1)
Tree_tTree yyt;
Defs_tVoid *yyObjects2;
Defs_tEnv *yyEnv1;
# endif
{
  struct S_3 yyTempo;

  {
    register struct S_3 *W_3 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_CompUnits:;
      *yyObjects2 = Defs_cVoid;
      break;
    case Tree_CompUnits0:;
      *yyObjects2 = Defs_cVoid;
      break;
    case Tree_CompUnit:;
      yyVisit2CompUnits(yyt->U_1.V_6.CompUnit.Next, yyObjects2, yyEnv1);
      break;
    case Tree_DefMod:;
      yyt->U_1.V_7.DefMod.Import->U_1.V_9.Import.Env1 = *yyEnv1;
      yyVisit1Import(yyt->U_1.V_7.DefMod.Import, &W_3->U_1.V_1.DefModyImportyObjects1);
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Env1 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_Predefs, W_3->U_1.V_1.DefModyImportyObjects1), yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Objects1), (Defs_tDefs)Defs_NoEnv);
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.DefTypes = Defs_NoObjects;
      yyVisit2Decls(yyt->U_1.V_7.DefMod.Decls, &W_3->U_1.V_1.DefModyDeclsyObjects2);
      yyVisit2CompUnits(yyt->U_1.V_7.DefMod.Next, &W_3->U_1.V_1.DefModyNextyObjects2, yyEnv1);
      *yyObjects2 = Defs_mVoid2(W_3->U_1.V_1.DefModyDeclsyObjects2, W_3->U_1.V_1.DefModyNextyObjects2);
      break;
    case Tree_ProgMod:;
      yyt->U_1.V_8.ProgMod.Import->U_1.V_9.Import.Env1 = *yyEnv1;
      yyVisit1Import(yyt->U_1.V_8.ProgMod.Import, &W_3->U_1.V_2.ProgModyImportyObjects1);
      W_3->U_1.V_2.ProgModyyDefObjects1 = Defs_Filter(Defs_GetExport1(Defs_Identify(yyt->U_1.V_8.ProgMod.Ident, *yyEnv1)));
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Env1 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_UNION(Defs_Predefs, W_3->U_1.V_2.ProgModyImportyObjects1), yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Objects1), W_3->U_1.V_2.ProgModyyDefObjects1), (Defs_tDefs)Defs_NoEnv);
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.DefTypes = Defs_GetExport1(Defs_Identify(yyt->U_1.V_8.ProgMod.Ident, *yyEnv1));
      yyVisit2Decls(yyt->U_1.V_8.ProgMod.Decls, &W_3->U_1.V_2.ProgModyDeclsyObjects2);
      yyVisit2CompUnits(yyt->U_1.V_8.ProgMod.Next, &W_3->U_1.V_2.ProgModyNextyObjects2, yyEnv1);
      *yyObjects2 = Defs_mVoid2(W_3->U_1.V_2.ProgModyDeclsyObjects2, W_3->U_1.V_2.ProgModyNextyObjects2);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3CompUnits
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects3, Idents_tIdent *yyImplIdent, Defs_tObjects *yyImplTypes, Defs_tVoid *yyEnv2)
# else
(yyt, yyObjects3, yyImplIdent, yyImplTypes, yyEnv2)
Tree_tTree yyt;
Defs_tObjects *yyObjects3;
Idents_tIdent *yyImplIdent;
Defs_tObjects *yyImplTypes;
Defs_tVoid *yyEnv2;
# endif
{
  struct S_4 yyTempo;

  {
    register struct S_4 *W_4 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_CompUnits:;
      *yyImplTypes = Defs_NoObjects;
      *yyImplIdent = Idents_NoIdent;
      *yyObjects3 = yyt->U_1.V_4.CompUnits.Objects1;
      break;
    case Tree_CompUnits0:;
      *yyImplTypes = Defs_NoObjects;
      *yyImplIdent = Idents_NoIdent;
      *yyObjects3 = yyt->U_1.V_5.CompUnits0.Objects1;
      break;
    case Tree_CompUnit:;
      yyVisit3CompUnits(yyt->U_1.V_6.CompUnit.Next, yyObjects3, &W_4->U_1.V_1.CompUnityNextyImplIdent, &W_4->U_1.V_1.CompUnityNextyImplTypes, yyEnv2);
      *yyImplTypes = Defs_NoObjects;
      *yyImplIdent = Idents_NoIdent;
      break;
    case Tree_DefMod:;
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Level = 0;
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Env2 = *yyEnv2;
      yyVisit3CompUnits(yyt->U_1.V_7.DefMod.Next, &W_4->U_1.V_2.DefModyNextyObjects3, yyImplIdent, yyImplTypes, yyEnv2);
      if (yyt->U_1.V_7.DefMod.Ident == *yyImplIdent) {
        W_4->U_1.V_2.DefModyDeclsyImplTypes = *yyImplTypes;
      } else {
        W_4->U_1.V_2.DefModyDeclsyImplTypes = Defs_NoObjects;
      }
      yyVisit3Decls(yyt->U_1.V_7.DefMod.Decls, &W_4->U_1.V_2.DefModyDeclsyImplTypes);
      *yyObjects3 = Defs_mElmt(yyt->U_1.V_7.DefMod.Ident, FALSE, Defs_mModule2(yyt->U_1.V_7.DefMod.Object, yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Objects3), W_4->U_1.V_2.DefModyNextyObjects3);
      break;
    case Tree_ProgMod:;
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Level = 0;
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Env2 = *yyEnv2;
      W_4->U_1.V_3.ProgModyDeclsyImplTypes = Defs_NoObjects;
      yyVisit3Decls(yyt->U_1.V_8.ProgMod.Decls, &W_4->U_1.V_3.ProgModyDeclsyImplTypes);
      yyVisit3CompUnits(yyt->U_1.V_8.ProgMod.Next, &W_4->U_1.V_3.ProgModyNextyObjects3, &W_4->U_1.V_3.ProgModyNextyImplIdent, &W_4->U_1.V_3.ProgModyNextyImplTypes, yyEnv2);
      *yyImplTypes = yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Objects3;
      *yyImplIdent = yyt->U_1.V_8.ProgMod.Ident;
      if (yyt->U_1.V_8.ProgMod.Kind == Tree_Program) {
        *yyObjects3 = Defs_mElmt(yyt->U_1.V_8.ProgMod.Ident, FALSE, yyt->U_1.V_8.ProgMod.Object, yyt->U_1.V_8.ProgMod.Next->U_1.V_4.CompUnits.Objects1);
      } else {
        *yyObjects3 = W_4->U_1.V_3.ProgModyNextyObjects3;
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit4CompUnits
# ifdef __STDC__
(Tree_tTree yyt, Defs_tVoid *yyObjects4In, Defs_tVoid *yyObjects4Out, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut, SHORTCARD *yyPosOut)
# else
(yyt, yyObjects4In, yyObjects4Out, yyEnv3, yyIdsIn, yyIdsOut, yyPosOut)
Tree_tTree yyt;
Defs_tVoid *yyObjects4In;
Defs_tVoid *yyObjects4Out;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
SHORTCARD *yyPosOut;
# endif
{
  struct S_5 yyTempo;

  {
    register struct S_5 *W_5 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_CompUnits:;
      *yyPosOut = yyt->U_1.V_4.CompUnits.PosIn;
      *yyIdsOut = *yyIdsIn;
      *yyObjects4Out = *yyObjects4In;
      break;
    case Tree_CompUnits0:;
      *yyPosOut = yyt->U_1.V_5.CompUnits0.PosIn;
      *yyIdsOut = *yyIdsIn;
      *yyObjects4Out = *yyObjects4In;
      break;
    case Tree_CompUnit:;
      yyVisit4CompUnits(yyt->U_1.V_6.CompUnit.Next, yyObjects4In, yyObjects4Out, yyEnv3, yyIdsIn, yyIdsOut, yyPosOut);
      break;
    case Tree_DefMod:;
      yyVisit2Import(yyt->U_1.V_7.DefMod.Import, yyEnv3);
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Env3 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_Predefs, yyt->U_1.V_7.DefMod.Import->U_1.V_9.Import.Objects2), yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Objects3), (Defs_tDefs)Defs_NoEnv);
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.DefObjects = Defs_NoObjects;
      yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Objects4In = Defs_NoObjects;
      yyVisit4Decls(yyt->U_1.V_7.DefMod.Decls);
      W_5->U_1.V_1.DefModyDeclsyInLocal = FALSE;
      W_5->U_1.V_1.DefModyDeclsyEnv4 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_Predefs, yyt->U_1.V_7.DefMod.Import->U_1.V_9.Import.Objects2), yyt->U_1.V_7.DefMod.Decls->U_1.V_23.Decls.Objects4Out), (Defs_tDefs)Defs_NoEnv);
      yyVisit5Decls(yyt->U_1.V_7.DefMod.Decls, &W_5->U_1.V_1.DefModyDeclsyEnv4, &W_5->U_1.V_1.DefModyDeclsyInLocal, yyIdsIn, &W_5->U_1.V_1.DefModyNextyIdsIn, &W_5->U_1.V_1.DefModyDeclsyCntOut, &W_5->U_1.V_1.DefModyDeclsyGlobalPtrs);
      yyVisit4CompUnits(yyt->U_1.V_7.DefMod.Next, yyObjects4In, yyObjects4Out, yyEnv3, &W_5->U_1.V_1.DefModyNextyIdsIn, yyIdsOut, yyPosOut);
      break;
    case Tree_ProgMod:;
      yyVisit2Import(yyt->U_1.V_8.ProgMod.Import, yyEnv3);
      W_5->U_1.V_2.ProgModyyDefObjects3 = Defs_Filter(Defs_GetExport2(Defs_Identify(yyt->U_1.V_8.ProgMod.Ident, *yyEnv3)));
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Env3 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_UNION(Defs_Predefs, yyt->U_1.V_8.ProgMod.Import->U_1.V_9.Import.Objects2), yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Objects3), W_5->U_1.V_2.ProgModyyDefObjects3), (Defs_tDefs)Defs_NoEnv);
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.DefObjects = Defs_GetExport2(Defs_Identify(yyt->U_1.V_8.ProgMod.Ident, *yyEnv3));
      yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Objects4In = Defs_NoObjects;
      yyVisit4Decls(yyt->U_1.V_8.ProgMod.Decls);
      W_5->U_1.V_2.ProgModyDeclsyInLocal = FALSE;
      W_5->U_1.V_2.ProgModyDeclsyEnv4 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_UNION(Defs_Predefs, yyt->U_1.V_8.ProgMod.Import->U_1.V_9.Import.Objects2), yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Objects4Out), W_5->U_1.V_2.ProgModyyDefObjects3), (Defs_tDefs)Defs_NoEnv);
      yyVisit5Decls(yyt->U_1.V_8.ProgMod.Decls, &W_5->U_1.V_2.ProgModyDeclsyEnv4, &W_5->U_1.V_2.ProgModyDeclsyInLocal, yyIdsIn, &W_5->U_1.V_2.ProgModyNextyIdsIn, &W_5->U_1.V_2.ProgModyDeclsyCntOut, &W_5->U_1.V_2.ProgModyDeclsyGlobalPtrs);
      if (W_5->U_1.V_2.ProgModyDeclsyGlobalPtrs) {
        yyt->U_1.V_8.ProgMod.GlobalPtrs = Defs_Pointers(TRUE, yyt->U_1.V_8.ProgMod.Decls->U_1.V_23.Decls.Objects4Out);
      } else {
        yyt->U_1.V_8.ProgMod.GlobalPtrs = Defs_NoCObjects;
      }
      W_5->U_1.V_2.ProgModyStmtsyType = Defs_TypeVOID;
      W_5->U_1.V_2.ProgModyStmtsyStrsIn = Defs_NoStrings;
      W_5->U_1.V_2.ProgModyStmtsyLevel = 0;
      yyVisit1Stmts(yyt->U_1.V_8.ProgMod.Stmts, &W_5->U_1.V_2.ProgModyDeclsyEnv4, &W_5->U_1.V_2.ProgModyStmtsyLevel, &W_5->U_1.V_2.ProgModyStmtsyGlobalPtrs, &W_5->U_1.V_2.ProgModyStmtsyStrsIn, &W_5->U_1.V_2.ProgModyStmtsyStrsOut, &W_5->U_1.V_2.ProgModyStmtsyType);
      yyVisit4CompUnits(yyt->U_1.V_8.ProgMod.Next, yyObjects4In, yyObjects4Out, yyEnv3, &W_5->U_1.V_2.ProgModyNextyIdsIn, yyIdsOut, yyPosOut);
      yyt->U_1.V_8.ProgMod.Strings = W_5->U_1.V_2.ProgModyStmtsyStrsOut;
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Import
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects1)
# else
(yyt, yyObjects1)
Tree_tTree yyt;
Defs_tObjects *yyObjects1;
# endif
{
  struct S_6 yyTempo;

  {
    register struct S_6 *W_6 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Import:;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_Import0:;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_Import1:;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_From:;
      W_6->U_1.V_1.FromyyObject1 = Defs_Identify(yyt->U_1.V_12.From.Ident, yyt->U_1.V_12.From.Env1);
      W_6->U_1.V_1.FromyImpIdsyEnv1 = Defs_mEnv(Defs_GetExport1(W_6->U_1.V_1.FromyyObject1), (Defs_tDefs)Defs_NoEnv);
      yyVisit1ImpIds(yyt->U_1.V_12.From.ImpIds, &W_6->U_1.V_1.FromyImpIdsyObjects1, &W_6->U_1.V_1.FromyImpIdsyEnv1);
      yyt->U_1.V_12.From.Next->U_1.V_9.Import.Env1 = yyt->U_1.V_12.From.Env1;
      yyVisit1Import(yyt->U_1.V_12.From.Next, &W_6->U_1.V_1.FromyNextyObjects1);
      *yyObjects1 = Defs_UNION(W_6->U_1.V_1.FromyImpIdsyObjects1, W_6->U_1.V_1.FromyNextyObjects1);
      break;
    case Tree_Objects:;
      W_6->U_1.V_2.ObjectsyImpIdsyEnv1 = yyt->U_1.V_13.Objects.Env1;
      yyVisit1ImpIds(yyt->U_1.V_13.Objects.ImpIds, &W_6->U_1.V_2.ObjectsyImpIdsyObjects1, &W_6->U_1.V_2.ObjectsyImpIdsyEnv1);
      yyt->U_1.V_13.Objects.Next->U_1.V_9.Import.Env1 = yyt->U_1.V_13.Objects.Env1;
      yyVisit1Import(yyt->U_1.V_13.Objects.Next, &W_6->U_1.V_2.ObjectsyNextyObjects1);
      *yyObjects1 = Defs_UNION(W_6->U_1.V_2.ObjectsyImpIdsyObjects1, W_6->U_1.V_2.ObjectsyNextyObjects1);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2Import
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv2)
# else
(yyt, yyEnv2)
Tree_tTree yyt;
Defs_tEnv *yyEnv2;
# endif
{
  struct S_7 yyTempo;

  {
    register struct S_7 *W_7 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Import:;
      yyt->U_1.V_9.Import.Objects2 = Defs_NoObjects;
      break;
    case Tree_Import0:;
      yyt->U_1.V_10.Import0.Objects2 = Defs_NoObjects;
      break;
    case Tree_Import1:;
      yyt->U_1.V_11.Import1.Next->U_1.V_9.Import.Env1 = yyt->U_1.V_11.Import1.Env1;
      yyVisit1Import(yyt->U_1.V_11.Import1.Next, &W_7->U_1.V_1.Import1yNextyObjects1);
      yyVisit2Import(yyt->U_1.V_11.Import1.Next, yyEnv2);
      yyt->U_1.V_11.Import1.Objects2 = Defs_NoObjects;
      break;
    case Tree_From:;
      W_7->U_1.V_2.FromyyObject2 = Defs_Identify(yyt->U_1.V_12.From.Ident, *yyEnv2);
      W_7->U_1.V_2.FromyImpIdsyEnv2 = Defs_mEnv(Defs_GetExport2(W_7->U_1.V_2.FromyyObject2), (Defs_tDefs)Defs_NoEnv);
      yyVisit2ImpIds(yyt->U_1.V_12.From.ImpIds, &W_7->U_1.V_2.FromyImpIdsyObjects2, &W_7->U_1.V_2.FromyImpIdsyEnv2);
      yyVisit2Import(yyt->U_1.V_12.From.Next, yyEnv2);
      yyt->U_1.V_12.From.Objects2 = Defs_UNION(W_7->U_1.V_2.FromyImpIdsyObjects2, yyt->U_1.V_12.From.Next->U_1.V_9.Import.Objects2);
      break;
    case Tree_Objects:;
      yyVisit2ImpIds(yyt->U_1.V_13.Objects.ImpIds, &W_7->U_1.V_3.ObjectsyImpIdsyObjects2, yyEnv2);
      yyVisit2Import(yyt->U_1.V_13.Objects.Next, yyEnv2);
      yyt->U_1.V_13.Objects.Objects2 = Defs_UNION(W_7->U_1.V_3.ObjectsyImpIdsyObjects2, yyt->U_1.V_13.Objects.Next->U_1.V_9.Import.Objects2);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1ImpIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects1, Defs_tEnv *yyEnv1)
# else
(yyt, yyObjects1, yyEnv1)
Tree_tTree yyt;
Defs_tObjects *yyObjects1;
Defs_tEnv *yyEnv1;
# endif
{
  struct S_8 yyTempo;

  {
    register struct S_8 *W_8 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ImpIds:;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_ImpIds0:;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_ImpIds1:;
      yyVisit1ImpIds(yyt->U_1.V_16.ImpIds1.Next, &W_8->U_1.V_1.ImpIds1yNextyObjects1, yyEnv1);
      W_8->U_1.V_1.ImpIds1yyObject1 = Defs_Identify(yyt->U_1.V_16.ImpIds1.Ident, *yyEnv1);
      if (W_8->U_1.V_1.ImpIds1yyObject1 == Defs_NoObject) {
        *yyObjects1 = W_8->U_1.V_1.ImpIds1yNextyObjects1;
      } else {
        *yyObjects1 = Defs_mElmt(yyt->U_1.V_16.ImpIds1.Ident, TRUE, W_8->U_1.V_1.ImpIds1yyObject1, W_8->U_1.V_1.ImpIds1yNextyObjects1);
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit2ImpIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects2, Defs_tEnv *yyEnv2)
# else
(yyt, yyObjects2, yyEnv2)
Tree_tTree yyt;
Defs_tObjects *yyObjects2;
Defs_tEnv *yyEnv2;
# endif
{
  struct S_9 yyTempo;

  {
    register struct S_9 *W_9 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ImpIds:;
      *yyObjects2 = Defs_NoObjects;
      break;
    case Tree_ImpIds0:;
      *yyObjects2 = Defs_NoObjects;
      break;
    case Tree_ImpIds1:;
      yyVisit2ImpIds(yyt->U_1.V_16.ImpIds1.Next, &W_9->U_1.V_1.ImpIds1yNextyObjects2, yyEnv2);
      W_9->U_1.V_1.ImpIds1yyObject2 = Defs_Identify(yyt->U_1.V_16.ImpIds1.Ident, *yyEnv2);
      W_9->U_1.V_1.ImpIds1yyType = Defs_GetType(W_9->U_1.V_1.ImpIds1yyObject2);
      if (W_9->U_1.V_1.ImpIds1yyObject2->U_1.V_1.Kind == Defs_TypeDecl1 && W_9->U_1.V_1.ImpIds1yyType->U_1.V_1.Kind == Defs_Enumeration1) {
        *yyObjects2 = Defs_mElmt(yyt->U_1.V_16.ImpIds1.Ident, TRUE, W_9->U_1.V_1.ImpIds1yyObject2, Defs_UNION(W_9->U_1.V_1.ImpIds1yyType->U_1.V_39.Enumeration1.Objects, W_9->U_1.V_1.ImpIds1yNextyObjects2));
      } else {
        *yyObjects2 = Defs_mElmt(yyt->U_1.V_16.ImpIds1.Ident, TRUE, W_9->U_1.V_1.ImpIds1yyObject2, W_9->U_1.V_1.ImpIds1yNextyObjects2);
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Export
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects1, Defs_tEnv *yyEnv1)
# else
(yyt, yyObjects1, yyEnv1)
Tree_tTree yyt;
Defs_tObjects *yyObjects1;
Defs_tEnv *yyEnv1;
# endif
{
  struct S_10 yyTempo;

  {
    register struct S_10 *W_10 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Export:;
      yyt->U_1.V_17.Export.IsQualified = TRUE;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_Export0:;
      yyt->U_1.V_18.Export0.IsQualified = TRUE;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_Export1:;
      yyVisit1ExpIds(yyt->U_1.V_19.Export1.ExpIds, yyObjects1, yyEnv1);
      yyt->U_1.V_19.Export1.IsQualified = yyt->U_1.V_19.Export1.Qualified;
      break;
    default :
      break;
    }
  }
}

static void yyVisit2Export
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyDefTypesIn, Defs_tObjects *yyDefTypesOut)
# else
(yyt, yyDefTypesIn, yyDefTypesOut)
Tree_tTree yyt;
Defs_tObjects *yyDefTypesIn;
Defs_tObjects *yyDefTypesOut;
# endif
{
  struct S_11 yyTempo;

  {
    register struct S_11 *W_11 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Export:;
      *yyDefTypesOut = Defs_NoObjects;
      break;
    case Tree_Export0:;
      *yyDefTypesOut = Defs_NoObjects;
      break;
    case Tree_Export1:;
      yyVisit2ExpIds(yyt->U_1.V_19.Export1.ExpIds, yyDefTypesIn, yyDefTypesOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3Export
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv2)
# else
(yyt, yyEnv2)
Tree_tTree yyt;
Defs_tEnv *yyEnv2;
# endif
{
  struct S_12 yyTempo;

  {
    register struct S_12 *W_12 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Export:;
      yyt->U_1.V_17.Export.Objects2 = Defs_NoObjects;
      break;
    case Tree_Export0:;
      yyt->U_1.V_18.Export0.Objects2 = Defs_NoObjects;
      break;
    case Tree_Export1:;
      yyVisit3ExpIds(yyt->U_1.V_19.Export1.ExpIds, &W_12->U_1.V_1.Export1yExpIdsyObjects2, yyEnv2);
      yyt->U_1.V_19.Export1.Objects2 = W_12->U_1.V_1.Export1yExpIdsyObjects2;
      break;
    default :
      break;
    }
  }
}

static void yyVisit4Export
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyDefObjsIn, Defs_tObjects *yyDefObjsOut)
# else
(yyt, yyDefObjsIn, yyDefObjsOut)
Tree_tTree yyt;
Defs_tObjects *yyDefObjsIn;
Defs_tObjects *yyDefObjsOut;
# endif
{
  struct S_13 yyTempo;

  {
    register struct S_13 *W_13 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Export:;
      *yyDefObjsOut = Defs_NoObjects;
      break;
    case Tree_Export0:;
      *yyDefObjsOut = Defs_NoObjects;
      break;
    case Tree_Export1:;
      yyVisit4ExpIds(yyt->U_1.V_19.Export1.ExpIds, yyDefObjsIn, yyDefObjsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1ExpIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects1, Defs_tEnv *yyEnv1)
# else
(yyt, yyObjects1, yyEnv1)
Tree_tTree yyt;
Defs_tObjects *yyObjects1;
Defs_tEnv *yyEnv1;
# endif
{
  struct S_14 yyTempo;

  {
    register struct S_14 *W_14 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ExpIds:;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_ExpIds0:;
      *yyObjects1 = Defs_NoObjects;
      break;
    case Tree_ExpIds1:;
      yyVisit1ExpIds(yyt->U_1.V_22.ExpIds1.Next, &W_14->U_1.V_1.ExpIds1yNextyObjects1, yyEnv1);
      yyt->U_1.V_22.ExpIds1.Object1 = Defs_Identify(yyt->U_1.V_22.ExpIds1.Ident, *yyEnv1);
      if (yyt->U_1.V_22.ExpIds1.Object1 == Defs_NoObject) {
        *yyObjects1 = W_14->U_1.V_1.ExpIds1yNextyObjects1;
      } else {
        *yyObjects1 = Defs_mElmt(yyt->U_1.V_22.ExpIds1.Ident, TRUE, yyt->U_1.V_22.ExpIds1.Object1, W_14->U_1.V_1.ExpIds1yNextyObjects1);
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit2ExpIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyDefTypesIn, Defs_tObjects *yyDefTypesOut)
# else
(yyt, yyDefTypesIn, yyDefTypesOut)
Tree_tTree yyt;
Defs_tObjects *yyDefTypesIn;
Defs_tObjects *yyDefTypesOut;
# endif
{
  struct S_15 yyTempo;

  {
    register struct S_15 *W_15 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ExpIds:;
      *yyDefTypesOut = Defs_NoObjects;
      break;
    case Tree_ExpIds0:;
      *yyDefTypesOut = Defs_NoObjects;
      break;
    case Tree_ExpIds1:;
      yyVisit2ExpIds(yyt->U_1.V_22.ExpIds1.Next, yyDefTypesIn, &W_15->U_1.V_1.ExpIds1yNextyDefTypesOut);
      if (Defs_IsDeclared(yyt->U_1.V_22.ExpIds1.Ident, *yyDefTypesIn)) {
        *yyDefTypesOut = Defs_mElmt(yyt->U_1.V_22.ExpIds1.Ident, FALSE, yyt->U_1.V_22.ExpIds1.Object1, W_15->U_1.V_1.ExpIds1yNextyDefTypesOut);
      } else {
        *yyDefTypesOut = W_15->U_1.V_1.ExpIds1yNextyDefTypesOut;
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit3ExpIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects2, Defs_tEnv *yyEnv2)
# else
(yyt, yyObjects2, yyEnv2)
Tree_tTree yyt;
Defs_tObjects *yyObjects2;
Defs_tEnv *yyEnv2;
# endif
{
  struct S_16 yyTempo;

  {
    register struct S_16 *W_16 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ExpIds:;
      *yyObjects2 = Defs_NoObjects;
      break;
    case Tree_ExpIds0:;
      *yyObjects2 = Defs_NoObjects;
      break;
    case Tree_ExpIds1:;
      yyVisit3ExpIds(yyt->U_1.V_22.ExpIds1.Next, &W_16->U_1.V_1.ExpIds1yNextyObjects2, yyEnv2);
      yyt->U_1.V_22.ExpIds1.Object2 = Defs_Identify(yyt->U_1.V_22.ExpIds1.Ident, *yyEnv2);
      W_16->U_1.V_1.ExpIds1yyType = Defs_GetType(yyt->U_1.V_22.ExpIds1.Object2);
      if (yyt->U_1.V_22.ExpIds1.Object2->U_1.V_1.Kind == Defs_TypeDecl1 && W_16->U_1.V_1.ExpIds1yyType->U_1.V_1.Kind == Defs_Enumeration1) {
        *yyObjects2 = Defs_mElmt(yyt->U_1.V_22.ExpIds1.Ident, TRUE, yyt->U_1.V_22.ExpIds1.Object2, Defs_UNION(W_16->U_1.V_1.ExpIds1yyType->U_1.V_39.Enumeration1.Objects, W_16->U_1.V_1.ExpIds1yNextyObjects2));
      } else {
        *yyObjects2 = Defs_mElmt(yyt->U_1.V_22.ExpIds1.Ident, TRUE, yyt->U_1.V_22.ExpIds1.Object2, W_16->U_1.V_1.ExpIds1yNextyObjects2);
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit4ExpIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyDefObjsIn, Defs_tObjects *yyDefObjsOut)
# else
(yyt, yyDefObjsIn, yyDefObjsOut)
Tree_tTree yyt;
Defs_tObjects *yyDefObjsIn;
Defs_tObjects *yyDefObjsOut;
# endif
{
  struct S_17 yyTempo;

  {
    register struct S_17 *W_17 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ExpIds:;
      *yyDefObjsOut = Defs_NoObjects;
      break;
    case Tree_ExpIds0:;
      *yyDefObjsOut = Defs_NoObjects;
      break;
    case Tree_ExpIds1:;
      yyVisit4ExpIds(yyt->U_1.V_22.ExpIds1.Next, yyDefObjsIn, &W_17->U_1.V_1.ExpIds1yNextyDefObjsOut);
      if (Defs_IsDeclared(yyt->U_1.V_22.ExpIds1.Ident, *yyDefObjsIn)) {
        *yyDefObjsOut = Defs_mElmt(yyt->U_1.V_22.ExpIds1.Ident, FALSE, yyt->U_1.V_22.ExpIds1.Object2, W_17->U_1.V_1.ExpIds1yNextyDefObjsOut);
      } else {
        *yyDefObjsOut = W_17->U_1.V_1.ExpIds1yNextyDefObjsOut;
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Decls
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_18 yyTempo;

  {
    register struct S_18 *W_18 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Decls:;
      *yyPosOut = *yyPosIn;
      yyt->U_1.V_23.Decls.Objects1 = Defs_NoObjects;
      break;
    case Tree_Decls0:;
      *yyPosOut = *yyPosIn;
      yyt->U_1.V_24.Decls0.Objects1 = Defs_NoObjects;
      break;
    case Tree_Decl:;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.CntIn = yyt->U_1.V_25.Decl.CntIn;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Module = yyt->U_1.V_25.Decl.Module;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_25.Decl.Kind;
      yyVisit1Decls(yyt->U_1.V_25.Decl.Next, yyPosIn, yyPosOut);
      yyt->U_1.V_25.Decl.Objects1 = yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Objects1;
      break;
    case Tree_Var:;
      yyt->U_1.V_26.Var.Type->U_1.V_43.Type.CntIn = yyt->U_1.V_26.Var.CntIn;
      yyt->U_1.V_26.Var.Type->U_1.V_43.Type.Module = yyt->U_1.V_26.Var.Module;
      yyt->U_1.V_26.Var.Type->U_1.V_43.Type.Kind = yyt->U_1.V_26.Var.Kind;
      yyVisit1Type(yyt->U_1.V_26.Var.Type, &W_18->U_1.V_1.VaryTypeyCntOut, yyPosIn, &W_18->U_1.V_1.VaryNextyPosIn);
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.CntIn = W_18->U_1.V_1.VaryTypeyCntOut;
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Module = yyt->U_1.V_26.Var.Module;
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_26.Var.Kind;
      yyVisit1Decls(yyt->U_1.V_26.Var.Next, &W_18->U_1.V_1.VaryNextyPosIn, yyPosOut);
      yyt->U_1.V_26.Var.Objects1 = yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Objects1;
      break;
    case Tree_Object:;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.CntIn = yyt->U_1.V_27.Object.CntIn;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Module = yyt->U_1.V_27.Object.Module;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_27.Object.Kind;
      yyVisit1Decls(yyt->U_1.V_27.Object.Next, yyPosIn, yyPosOut);
      yyt->U_1.V_27.Object.Objects1 = yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Objects1;
      break;
    case Tree_Const:;
      yyt->U_1.V_28.Const.Object = Defs_mConst1(yyt->U_1.V_28.Const.Ident);
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.CntIn = yyt->U_1.V_28.Const.CntIn;
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Module = yyt->U_1.V_28.Const.Module;
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_28.Const.Kind;
      yyVisit1Decls(yyt->U_1.V_28.Const.Next, yyPosIn, yyPosOut);
      yyt->U_1.V_28.Const.Objects1 = yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Objects1;
      break;
    case Tree_TypeDecl:;
      yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.CntIn = yyt->U_1.V_29.TypeDecl.CntIn;
      yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Module = yyt->U_1.V_29.TypeDecl.Module;
      yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Kind = yyt->U_1.V_29.TypeDecl.Kind;
      yyVisit1Type(yyt->U_1.V_29.TypeDecl.Type, &W_18->U_1.V_2.TypeDeclyTypeyCntOut, yyPosIn, &W_18->U_1.V_2.TypeDeclyNextyPosIn);
      yyt->U_1.V_29.TypeDecl.Object = Defs_mTypeDecl1(yyt->U_1.V_29.TypeDecl.Ident, W_18->U_1.V_2.TypeDeclyNextyPosIn);
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.CntIn = W_18->U_1.V_2.TypeDeclyTypeyCntOut;
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Module = yyt->U_1.V_29.TypeDecl.Module;
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_29.TypeDecl.Kind;
      yyVisit1Decls(yyt->U_1.V_29.TypeDecl.Next, &W_18->U_1.V_2.TypeDeclyNextyPosIn, yyPosOut);
      yyt->U_1.V_29.TypeDecl.Objects1 = Defs_mElmt(yyt->U_1.V_29.TypeDecl.Ident, FALSE, yyt->U_1.V_29.TypeDecl.Object, yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Objects1);
      break;
    case Tree_Proc:;
      W_18->U_1.V_3.ProcyFormalsyModule = yyt->U_1.V_30.Proc.Module;
      W_18->U_1.V_3.ProcyFormalsyKind = yyt->U_1.V_30.Proc.Kind;
      yyVisit1Formals(yyt->U_1.V_30.Proc.Formals, &W_18->U_1.V_3.ProcyFormalsyKind, &W_18->U_1.V_3.ProcyFormalsyModule, yyPosIn, &W_18->U_1.V_3.ProcyFormalsyPosOut);
      yyt->U_1.V_30.Proc.ResultType->U_1.V_52.PrimaryType.CntIn = 0;
      yyt->U_1.V_30.Proc.ResultType->U_1.V_52.PrimaryType.Module = yyt->U_1.V_30.Proc.Module;
      yyt->U_1.V_30.Proc.ResultType->U_1.V_52.PrimaryType.Kind = yyt->U_1.V_30.Proc.Kind;
      yyVisit1PrimaryType(yyt->U_1.V_30.Proc.ResultType, &W_18->U_1.V_3.ProcyResultTypeyCntOut, &W_18->U_1.V_3.ProcyFormalsyPosOut, &W_18->U_1.V_3.ProcyResultTypeyPosOut);
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.CntIn = 0;
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Module = yyt->U_1.V_30.Proc.Module;
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Kind = yyt->U_1.V_30.Proc.Kind;
      yyVisit1Decls(yyt->U_1.V_30.Proc.Decls, &W_18->U_1.V_3.ProcyResultTypeyPosOut, &W_18->U_1.V_3.ProcyDeclsyPosOut);
      W_18->U_1.V_3.ProcyNextyPosIn = W_18->U_1.V_3.ProcyDeclsyPosOut + 1;
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.CntIn = yyt->U_1.V_30.Proc.CntIn;
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Module = yyt->U_1.V_30.Proc.Module;
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_30.Proc.Kind;
      yyVisit1Decls(yyt->U_1.V_30.Proc.Next, &W_18->U_1.V_3.ProcyNextyPosIn, yyPosOut);
      yyt->U_1.V_30.Proc.Objects1 = yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Objects1;
      break;
    case Tree_ProcHead:;
      W_18->U_1.V_4.ProcHeadyFormalsyModule = yyt->U_1.V_31.ProcHead.Module;
      W_18->U_1.V_4.ProcHeadyFormalsyKind = yyt->U_1.V_31.ProcHead.Kind;
      yyVisit1Formals(yyt->U_1.V_31.ProcHead.Formals, &W_18->U_1.V_4.ProcHeadyFormalsyKind, &W_18->U_1.V_4.ProcHeadyFormalsyModule, yyPosIn, &W_18->U_1.V_4.ProcHeadyFormalsyPosOut);
      yyt->U_1.V_31.ProcHead.ResultType->U_1.V_52.PrimaryType.CntIn = 0;
      yyt->U_1.V_31.ProcHead.ResultType->U_1.V_52.PrimaryType.Module = yyt->U_1.V_31.ProcHead.Module;
      yyt->U_1.V_31.ProcHead.ResultType->U_1.V_52.PrimaryType.Kind = yyt->U_1.V_31.ProcHead.Kind;
      yyVisit1PrimaryType(yyt->U_1.V_31.ProcHead.ResultType, &W_18->U_1.V_4.ProcHeadyResultTypeyCntOut, &W_18->U_1.V_4.ProcHeadyFormalsyPosOut, &W_18->U_1.V_4.ProcHeadyResultTypeyPosOut);
      W_18->U_1.V_4.ProcHeadyNextyPosIn = W_18->U_1.V_4.ProcHeadyResultTypeyPosOut + 1;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.CntIn = yyt->U_1.V_31.ProcHead.CntIn;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Module = yyt->U_1.V_31.ProcHead.Module;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_31.ProcHead.Kind;
      yyVisit1Decls(yyt->U_1.V_31.ProcHead.Next, &W_18->U_1.V_4.ProcHeadyNextyPosIn, yyPosOut);
      yyt->U_1.V_31.ProcHead.Objects1 = yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Objects1;
      break;
    case Tree_Module:;
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.CntIn = 0;
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Module = yyt->U_1.V_32.Module.Module;
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Kind = yyt->U_1.V_32.Module.Kind;
      yyVisit1Decls(yyt->U_1.V_32.Module.Decls, yyPosIn, &W_18->U_1.V_5.ModuleyNextyPosIn);
      W_18->U_1.V_5.ModuleyExportyEnv1 = Defs_mEnv(yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Objects1, (Defs_tDefs)Defs_NoEnv);
      yyVisit1Export(yyt->U_1.V_32.Module.Export, &W_18->U_1.V_5.ModuleyExportyObjects1, &W_18->U_1.V_5.ModuleyExportyEnv1);
      yyt->U_1.V_32.Module.Object = Defs_mModule1(yyt->U_1.V_32.Module.Ident, W_18->U_1.V_5.ModuleyExportyObjects1);
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.CntIn = yyt->U_1.V_32.Module.CntIn;
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Module = yyt->U_1.V_32.Module.Module;
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_32.Module.Kind;
      yyVisit1Decls(yyt->U_1.V_32.Module.Next, &W_18->U_1.V_5.ModuleyNextyPosIn, yyPosOut);
      if (yyt->U_1.V_32.Module.Export->U_1.V_17.Export.IsQualified) {
        yyt->U_1.V_32.Module.Objects1 = Defs_mElmt(yyt->U_1.V_32.Module.Ident, FALSE, yyt->U_1.V_32.Module.Object, yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Objects1);
      } else {
        yyt->U_1.V_32.Module.Objects1 = Defs_mElmt(yyt->U_1.V_32.Module.Ident, FALSE, yyt->U_1.V_32.Module.Object, Defs_UNION(W_18->U_1.V_5.ModuleyExportyObjects1, yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Objects1));
      }
      break;
    case Tree_Opaque:;
      W_18->U_1.V_6.OpaqueyNextyPosIn = *yyPosIn + 1;
      yyt->U_1.V_33.Opaque.Object = Defs_mOpaque1(yyt->U_1.V_33.Opaque.Ident, W_18->U_1.V_6.OpaqueyNextyPosIn);
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.CntIn = yyt->U_1.V_33.Opaque.CntIn;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Module = yyt->U_1.V_33.Opaque.Module;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Kind = yyt->U_1.V_33.Opaque.Kind;
      yyVisit1Decls(yyt->U_1.V_33.Opaque.Next, &W_18->U_1.V_6.OpaqueyNextyPosIn, yyPosOut);
      yyt->U_1.V_33.Opaque.Objects1 = Defs_mElmt(yyt->U_1.V_33.Opaque.Ident, FALSE, yyt->U_1.V_33.Opaque.Object, yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Objects1);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2Decls
# ifdef __STDC__
(Tree_tTree yyt, Defs_tVoid *yyObjects2)
# else
(yyt, yyObjects2)
Tree_tTree yyt;
Defs_tVoid *yyObjects2;
# endif
{
  struct S_19 yyTempo;

  {
    register struct S_19 *W_19 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Decls:;
      *yyObjects2 = Defs_cVoid;
      break;
    case Tree_Decls0:;
      *yyObjects2 = Defs_cVoid;
      break;
    case Tree_Decl:;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_25.Decl.Env1;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_25.Decl.DefTypes;
      yyVisit2Decls(yyt->U_1.V_25.Decl.Next, yyObjects2);
      break;
    case Tree_Var:;
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_26.Var.Env1;
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_26.Var.DefTypes;
      yyVisit2Decls(yyt->U_1.V_26.Var.Next, yyObjects2);
      break;
    case Tree_Object:;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_27.Object.Env1;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_27.Object.DefTypes;
      yyVisit2Decls(yyt->U_1.V_27.Object.Next, yyObjects2);
      break;
    case Tree_Const:;
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_28.Const.Env1;
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_28.Const.DefTypes;
      yyVisit2Decls(yyt->U_1.V_28.Const.Next, yyObjects2);
      break;
    case Tree_TypeDecl:;
      yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Env1 = yyt->U_1.V_29.TypeDecl.Env1;
      W_19->U_1.V_1.TypeDeclyTypeyTypeObj = yyt->U_1.V_29.TypeDecl.Object;
      yyVisit2Type(yyt->U_1.V_29.TypeDecl.Type, &W_19->U_1.V_1.TypeDeclyTypeyTypeObj);
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_29.TypeDecl.Env1;
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_29.TypeDecl.DefTypes;
      yyVisit2Decls(yyt->U_1.V_29.TypeDecl.Next, &W_19->U_1.V_1.TypeDeclyNextyObjects2);
      if (Defs_IsDeclared(yyt->U_1.V_29.TypeDecl.Ident, yyt->U_1.V_29.TypeDecl.DefTypes)) {
        yyt->U_1.V_29.TypeDecl.Type1 = Defs_mOpaqueType1(yyt->U_1.V_29.TypeDecl.Object);
      } else {
        yyt->U_1.V_29.TypeDecl.Type1 = yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Type1;
      }
      *yyObjects2 = Defs_mVoid1(Defs_mTypeDecl2(yyt->U_1.V_29.TypeDecl.Object, yyt->U_1.V_29.TypeDecl.Type1), W_19->U_1.V_1.TypeDeclyNextyObjects2);
      break;
    case Tree_Proc:;
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Env1 = Defs_mEnv(yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Objects1, yyt->U_1.V_30.Proc.Env1);
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.DefTypes = Defs_NoObjects;
      yyVisit2Decls(yyt->U_1.V_30.Proc.Decls, &W_19->U_1.V_2.ProcyDeclsyObjects2);
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_30.Proc.Env1;
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_30.Proc.DefTypes;
      yyVisit2Decls(yyt->U_1.V_30.Proc.Next, &W_19->U_1.V_2.ProcyNextyObjects2);
      *yyObjects2 = Defs_mVoid2(W_19->U_1.V_2.ProcyDeclsyObjects2, W_19->U_1.V_2.ProcyNextyObjects2);
      break;
    case Tree_ProcHead:;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_31.ProcHead.Env1;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_31.ProcHead.DefTypes;
      yyVisit2Decls(yyt->U_1.V_31.ProcHead.Next, yyObjects2);
      break;
    case Tree_Module:;
      yyt->U_1.V_32.Module.Import->U_1.V_9.Import.Env1 = yyt->U_1.V_32.Module.Env1;
      yyVisit1Import(yyt->U_1.V_32.Module.Import, &W_19->U_1.V_3.ModuleyImportyObjects1);
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Env1 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_Predefs, W_19->U_1.V_3.ModuleyImportyObjects1), yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Objects1), (Defs_tDefs)Defs_NoEnv);
      if (yyt->U_1.V_32.Module.Export->U_1.V_17.Export.IsQualified) {
        W_19->U_1.V_3.ModuleyExportyDefTypesIn = Defs_NoObjects;
      } else {
        W_19->U_1.V_3.ModuleyExportyDefTypesIn = yyt->U_1.V_32.Module.DefTypes;
      }
      yyVisit2Export(yyt->U_1.V_32.Module.Export, &W_19->U_1.V_3.ModuleyExportyDefTypesIn, &W_19->U_1.V_3.ModuleyExportyDefTypesOut);
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.DefTypes = W_19->U_1.V_3.ModuleyExportyDefTypesOut;
      yyVisit2Decls(yyt->U_1.V_32.Module.Decls, &W_19->U_1.V_3.ModuleyDeclsyObjects2);
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_32.Module.Env1;
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_32.Module.DefTypes;
      yyVisit2Decls(yyt->U_1.V_32.Module.Next, &W_19->U_1.V_3.ModuleyNextyObjects2);
      *yyObjects2 = Defs_mVoid2(W_19->U_1.V_3.ModuleyDeclsyObjects2, W_19->U_1.V_3.ModuleyNextyObjects2);
      break;
    case Tree_Opaque:;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Env1 = yyt->U_1.V_33.Opaque.Env1;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.DefTypes = yyt->U_1.V_33.Opaque.DefTypes;
      yyVisit2Decls(yyt->U_1.V_33.Opaque.Next, &W_19->U_1.V_4.OpaqueyNextyObjects2);
      yyt->U_1.V_33.Opaque.Type1 = Defs_mOpaqueType1(yyt->U_1.V_33.Opaque.Object);
      *yyObjects2 = Defs_mVoid1(Defs_mOpaque2(yyt->U_1.V_33.Opaque.Object, yyt->U_1.V_33.Opaque.Type1), W_19->U_1.V_4.OpaqueyNextyObjects2);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3Decls
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyImplTypes)
# else
(yyt, yyImplTypes)
Tree_tTree yyt;
Defs_tObjects *yyImplTypes;
# endif
{
  struct S_20 yyTempo;

  {
    register struct S_20 *W_20 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Decls:;
      yyt->U_1.V_23.Decls.Objects3 = yyt->U_1.V_23.Decls.Objects1;
      break;
    case Tree_Decls0:;
      yyt->U_1.V_24.Decls0.Objects3 = yyt->U_1.V_24.Decls0.Objects1;
      break;
    case Tree_Decl:;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_25.Decl.Env2;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Level = yyt->U_1.V_25.Decl.Level;
      yyVisit3Decls(yyt->U_1.V_25.Decl.Next, yyImplTypes);
      yyt->U_1.V_25.Decl.Objects3 = yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Objects3;
      break;
    case Tree_Var:;
      yyt->U_1.V_26.Var.IsGlobal = yyt->U_1.V_26.Var.Level == 0;
      yyt->U_1.V_26.Var.Type->U_1.V_43.Type.Env1 = yyt->U_1.V_26.Var.Env1;
      W_20->U_1.V_1.VaryTypeyTypeObj = Defs_NoObject;
      yyVisit2Type(yyt->U_1.V_26.Var.Type, &W_20->U_1.V_1.VaryTypeyTypeObj);
      yyt->U_1.V_26.Var.Type->U_1.V_43.Type.Env2 = yyt->U_1.V_26.Var.Env2;
      yyVisit3Type(yyt->U_1.V_26.Var.Type);
      W_20->U_1.V_1.VaryVarIdsyLevel = yyt->U_1.V_26.Var.Level;
      W_20->U_1.V_1.VaryVarIdsyTypeTree = (ADDRESS)yyt->U_1.V_26.Var.Type;
      W_20->U_1.V_1.VaryVarIdsyType = yyt->U_1.V_26.Var.Type->U_1.V_43.Type.Type2;
      yyVisit1VarIds(yyt->U_1.V_26.Var.VarIds, &W_20->U_1.V_1.VaryVarIdsyType, &W_20->U_1.V_1.VaryVarIdsyTypeTree, &W_20->U_1.V_1.VaryVarIdsyLevel);
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Level = yyt->U_1.V_26.Var.Level;
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_26.Var.Env2;
      yyVisit3Decls(yyt->U_1.V_26.Var.Next, yyImplTypes);
      yyt->U_1.V_26.Var.Objects3 = Defs_UNION(yyt->U_1.V_26.Var.VarIds->U_1.V_34.VarIds.Objects3, Defs_UNION(yyt->U_1.V_26.Var.Type->U_1.V_43.Type.Objects3, yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Objects3));
      break;
    case Tree_Object:;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_27.Object.Env2;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Level = yyt->U_1.V_27.Object.Level;
      yyVisit3Decls(yyt->U_1.V_27.Object.Next, yyImplTypes);
      yyt->U_1.V_27.Object.Objects3 = yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Objects3;
      break;
    case Tree_Const:;
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_28.Const.Env2;
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Level = yyt->U_1.V_28.Const.Level;
      yyVisit3Decls(yyt->U_1.V_28.Const.Next, yyImplTypes);
      yyt->U_1.V_28.Const.Objects3 = Defs_mElmt(yyt->U_1.V_28.Const.Ident, FALSE, yyt->U_1.V_28.Const.Object, yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Objects3);
      break;
    case Tree_TypeDecl:;
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_29.TypeDecl.Env2;
      yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Env2 = yyt->U_1.V_29.TypeDecl.Env2;
      yyVisit3Type(yyt->U_1.V_29.TypeDecl.Type);
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Level = yyt->U_1.V_29.TypeDecl.Level;
      yyVisit3Decls(yyt->U_1.V_29.TypeDecl.Next, yyImplTypes);
      if (Defs_IsDeclared(yyt->U_1.V_29.TypeDecl.Ident, yyt->U_1.V_29.TypeDecl.DefTypes)) {
        W_20->U_1.V_2.TypeDeclyyType2 = Defs_mOpaqueType2(yyt->U_1.V_29.TypeDecl.Type1, yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Type2);
      } else {
        W_20->U_1.V_2.TypeDeclyyType2 = yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Type2;
      }
      yyt->U_1.V_29.TypeDecl.Objects3 = Defs_mElmt(yyt->U_1.V_29.TypeDecl.Ident, FALSE, Defs_mTypeDecl3(yyt->U_1.V_29.TypeDecl.Object, W_20->U_1.V_2.TypeDeclyyType2), Defs_UNION(yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Objects3, yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Objects3));
      break;
    case Tree_Proc:;
      yyt->U_1.V_30.Proc.ResultType->U_1.V_52.PrimaryType.Env1 = yyt->U_1.V_30.Proc.Env1;
      W_20->U_1.V_3.ProcyResultTypeyTypeObj = Defs_NoObject;
      yyVisit2PrimaryType(yyt->U_1.V_30.Proc.ResultType, &W_20->U_1.V_3.ProcyResultTypeyTypeObj);
      W_20->U_1.V_3.ProcyFormalsyEnv1 = yyt->U_1.V_30.Proc.Env1;
      yyt->U_1.V_30.Proc.ResultType->U_1.V_52.PrimaryType.Env2 = yyt->U_1.V_30.Proc.Env2;
      yyVisit3PrimaryType(yyt->U_1.V_30.Proc.ResultType);
      W_20->U_1.V_3.ProcyFormalsyLevel = yyt->U_1.V_30.Proc.Level + 1;
      W_20->U_1.V_3.ProcyFormalsyEnv2 = yyt->U_1.V_30.Proc.Env2;
      yyVisit2Formals(yyt->U_1.V_30.Proc.Formals, &W_20->U_1.V_3.ProcyFormalsyTypes, &W_20->U_1.V_3.ProcyFormalsyEnv1, &W_20->U_1.V_3.ProcyFormalsyEnv2, &W_20->U_1.V_3.ProcyFormalsyLevel);
      yyt->U_1.V_30.Proc.Type = Defs_mProcType2(Defs_mProcType1(Defs_NoObject), W_20->U_1.V_3.ProcyFormalsyTypes, yyt->U_1.V_30.Proc.ResultType->U_1.V_52.PrimaryType.Type2);
      yyt->U_1.V_30.Proc.Object = Defs_mProc1(yyt->U_1.V_30.Proc.Ident, yyt->U_1.V_30.Proc.Type);
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Level = yyt->U_1.V_30.Proc.Level;
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_30.Proc.Env2;
      yyVisit3Decls(yyt->U_1.V_30.Proc.Next, yyImplTypes);
      Defs_OpenArrays(yyt->U_1.V_30.Proc.Formals->U_1.V_37.Formals.Objects3, &yyt->U_1.V_30.Proc.ValueOpens, &yyt->U_1.V_30.Proc.VAROpens);
      yyt->U_1.V_30.Proc.Objects3 = Defs_mElmt(yyt->U_1.V_30.Proc.Ident, FALSE, yyt->U_1.V_30.Proc.Object, yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Objects3);
      break;
    case Tree_ProcHead:;
      yyt->U_1.V_31.ProcHead.ResultType->U_1.V_52.PrimaryType.Env1 = yyt->U_1.V_31.ProcHead.Env1;
      W_20->U_1.V_4.ProcHeadyResultTypeyTypeObj = Defs_NoObject;
      yyVisit2PrimaryType(yyt->U_1.V_31.ProcHead.ResultType, &W_20->U_1.V_4.ProcHeadyResultTypeyTypeObj);
      W_20->U_1.V_4.ProcHeadyFormalsyEnv1 = yyt->U_1.V_31.ProcHead.Env1;
      yyt->U_1.V_31.ProcHead.ResultType->U_1.V_52.PrimaryType.Env2 = yyt->U_1.V_31.ProcHead.Env2;
      yyVisit3PrimaryType(yyt->U_1.V_31.ProcHead.ResultType);
      W_20->U_1.V_4.ProcHeadyFormalsyLevel = yyt->U_1.V_31.ProcHead.Level + 1;
      W_20->U_1.V_4.ProcHeadyFormalsyEnv2 = yyt->U_1.V_31.ProcHead.Env2;
      yyVisit2Formals(yyt->U_1.V_31.ProcHead.Formals, &W_20->U_1.V_4.ProcHeadyFormalsyTypes, &W_20->U_1.V_4.ProcHeadyFormalsyEnv1, &W_20->U_1.V_4.ProcHeadyFormalsyEnv2, &W_20->U_1.V_4.ProcHeadyFormalsyLevel);
      W_20->U_1.V_4.ProcHeadyyType = Defs_mProcType2(Defs_mProcType1(Defs_NoObject), W_20->U_1.V_4.ProcHeadyFormalsyTypes, yyt->U_1.V_31.ProcHead.ResultType->U_1.V_52.PrimaryType.Type2);
      yyt->U_1.V_31.ProcHead.Object = Defs_mProcHead1(yyt->U_1.V_31.ProcHead.Ident, W_20->U_1.V_4.ProcHeadyyType);
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Level = yyt->U_1.V_31.ProcHead.Level;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_31.ProcHead.Env2;
      yyVisit3Decls(yyt->U_1.V_31.ProcHead.Next, yyImplTypes);
      yyt->U_1.V_31.ProcHead.Objects3 = Defs_mElmt(yyt->U_1.V_31.ProcHead.Ident, FALSE, yyt->U_1.V_31.ProcHead.Object, yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Objects3);
      break;
    case Tree_Module:;
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_32.Module.Env2;
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Level = yyt->U_1.V_32.Module.Level;
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Env2 = yyt->U_1.V_32.Module.Env2;
      W_20->U_1.V_5.ModuleyDeclsyImplTypes = Defs_NoObjects;
      yyVisit3Decls(yyt->U_1.V_32.Module.Decls, &W_20->U_1.V_5.ModuleyDeclsyImplTypes);
      W_20->U_1.V_5.ModuleyExportyEnv2 = Defs_mEnv(yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Objects3, (Defs_tDefs)Defs_NoEnv);
      yyVisit3Export(yyt->U_1.V_32.Module.Export, &W_20->U_1.V_5.ModuleyExportyEnv2);
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Level = yyt->U_1.V_32.Module.Level;
      yyVisit3Decls(yyt->U_1.V_32.Module.Next, yyImplTypes);
      if (yyt->U_1.V_32.Module.Export->U_1.V_17.Export.IsQualified) {
        yyt->U_1.V_32.Module.Objects3 = Defs_mElmt(yyt->U_1.V_32.Module.Ident, FALSE, Defs_mModule2(yyt->U_1.V_32.Module.Object, yyt->U_1.V_32.Module.Export->U_1.V_17.Export.Objects2), yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Objects3);
      } else {
        yyt->U_1.V_32.Module.Objects3 = Defs_mElmt(yyt->U_1.V_32.Module.Ident, FALSE, Defs_mModule2(yyt->U_1.V_32.Module.Object, yyt->U_1.V_32.Module.Export->U_1.V_17.Export.Objects2), Defs_UNION(yyt->U_1.V_32.Module.Export->U_1.V_17.Export.Objects2, yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Objects3));
      }
      break;
    case Tree_Opaque:;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Env2 = yyt->U_1.V_33.Opaque.Env2;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Level = yyt->U_1.V_33.Opaque.Level;
      yyVisit3Decls(yyt->U_1.V_33.Opaque.Next, yyImplTypes);
      W_20->U_1.V_6.OpaqueyyFullType = Defs_GetType(Defs_Identify2(yyt->U_1.V_33.Opaque.Ident, *yyImplTypes));
      if (W_20->U_1.V_6.OpaqueyyFullType->U_1.V_1.Kind == Defs_OpaqueType1) {
        W_20->U_1.V_6.OpaqueyyType2 = Defs_mOpaqueType2(yyt->U_1.V_33.Opaque.Type1, W_20->U_1.V_6.OpaqueyyFullType->U_1.V_37.OpaqueType1.Type);
      } else {
        W_20->U_1.V_6.OpaqueyyType2 = Defs_mOpaqueType2(yyt->U_1.V_33.Opaque.Type1, Defs_NoType);
      }
      yyt->U_1.V_33.Opaque.Objects3 = Defs_mElmt(yyt->U_1.V_33.Opaque.Ident, FALSE, Defs_mOpaque3(yyt->U_1.V_33.Opaque.Object, W_20->U_1.V_6.OpaqueyyType2), yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Objects3);
      break;
    default :
      break;
    }
  }
}

static void yyVisit4Decls
# ifdef __STDC__
(Tree_tTree yyt)
# else
(yyt)
Tree_tTree yyt;
# endif
{
  struct S_21 yyTempo;

  {
    register struct S_21 *W_21 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Decls:;
      yyt->U_1.V_23.Decls.Objects4Out = yyt->U_1.V_23.Decls.Objects3;
      break;
    case Tree_Decls0:;
      yyt->U_1.V_24.Decls0.Objects4Out = yyt->U_1.V_24.Decls0.Objects4In;
      break;
    case Tree_Decl:;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_25.Decl.Env3;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_25.Decl.DefObjects;
      yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Objects4In = yyt->U_1.V_25.Decl.Objects4In;
      yyVisit4Decls(yyt->U_1.V_25.Decl.Next);
      yyt->U_1.V_25.Decl.Objects4Out = yyt->U_1.V_25.Decl.Next->U_1.V_23.Decls.Objects4Out;
      break;
    case Tree_Var:;
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_26.Var.Env3;
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_26.Var.DefObjects;
      yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Objects4In = Defs_UNION(yyt->U_1.V_26.Var.VarIds->U_1.V_34.VarIds.Objects3, Defs_UNION(yyt->U_1.V_26.Var.Type->U_1.V_43.Type.Objects3, yyt->U_1.V_26.Var.Objects4In));
      yyVisit4Decls(yyt->U_1.V_26.Var.Next);
      yyt->U_1.V_26.Var.Objects4Out = yyt->U_1.V_26.Var.Next->U_1.V_23.Decls.Objects4Out;
      break;
    case Tree_Object:;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_27.Object.Env3;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_27.Object.DefObjects;
      yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Objects4In = yyt->U_1.V_27.Object.Objects4In;
      yyVisit4Decls(yyt->U_1.V_27.Object.Next);
      yyt->U_1.V_27.Object.Objects4Out = yyt->U_1.V_27.Object.Next->U_1.V_23.Decls.Objects4Out;
      break;
    case Tree_Const:;
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_28.Const.Env3;
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_28.Const.DefObjects;
      Values_CompConst((ADDRESS)yyt->U_1.V_28.Const.Expr, (ADDRESS)yyt->U_1.V_28.Const.Env3, &yyt->U_1.V_28.Const.Object->U_1.V_5.Const1.Value);
      yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Objects4In = Defs_mElmt(yyt->U_1.V_28.Const.Ident, FALSE, yyt->U_1.V_28.Const.Object, yyt->U_1.V_28.Const.Objects4In);
      yyVisit4Decls(yyt->U_1.V_28.Const.Next);
      yyt->U_1.V_28.Const.Objects4Out = yyt->U_1.V_28.Const.Next->U_1.V_23.Decls.Objects4Out;
      break;
    case Tree_TypeDecl:;
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_29.TypeDecl.Env3;
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_29.TypeDecl.DefObjects;
      yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Objects4In = Defs_mElmt(yyt->U_1.V_29.TypeDecl.Ident, FALSE, yyt->U_1.V_29.TypeDecl.Object, Defs_UNION(yyt->U_1.V_29.TypeDecl.Type->U_1.V_43.Type.Objects3, yyt->U_1.V_29.TypeDecl.Objects4In));
      yyVisit4Decls(yyt->U_1.V_29.TypeDecl.Next);
      yyt->U_1.V_29.TypeDecl.Objects4Out = yyt->U_1.V_29.TypeDecl.Next->U_1.V_23.Decls.Objects4Out;
      break;
    case Tree_Proc:;
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Level = yyt->U_1.V_30.Proc.Level + 1;
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Env2 = yyt->U_1.V_30.Proc.Env2;
      W_21->U_1.V_1.ProcyDeclsyImplTypes = Defs_NoObjects;
      yyVisit3Decls(yyt->U_1.V_30.Proc.Decls, &W_21->U_1.V_1.ProcyDeclsyImplTypes);
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Env3 = Defs_mEnv(Defs_UNION(yyt->U_1.V_30.Proc.Formals->U_1.V_37.Formals.Objects3, yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Objects3), yyt->U_1.V_30.Proc.Env3);
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.DefObjects = Defs_NoObjects;
      yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Objects4In = Defs_NoObjects;
      yyVisit4Decls(yyt->U_1.V_30.Proc.Decls);
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_30.Proc.Env3;
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_30.Proc.DefObjects;
      yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Objects4In = Defs_mElmt(yyt->U_1.V_30.Proc.Ident, FALSE, Defs_mProc2(yyt->U_1.V_30.Proc.Object, Defs_UNION(yyt->U_1.V_30.Proc.Formals->U_1.V_37.Formals.Objects3, yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Objects4Out), Defs_IsDeclared(yyt->U_1.V_30.Proc.Ident, yyt->U_1.V_30.Proc.DefObjects)), yyt->U_1.V_30.Proc.Objects4In);
      yyVisit4Decls(yyt->U_1.V_30.Proc.Next);
      yyt->U_1.V_30.Proc.Objects4Out = yyt->U_1.V_30.Proc.Next->U_1.V_23.Decls.Objects4Out;
      break;
    case Tree_ProcHead:;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_31.ProcHead.Env3;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_31.ProcHead.DefObjects;
      yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Objects4In = Defs_mElmt(yyt->U_1.V_31.ProcHead.Ident, FALSE, yyt->U_1.V_31.ProcHead.Object, yyt->U_1.V_31.ProcHead.Objects4In);
      yyVisit4Decls(yyt->U_1.V_31.ProcHead.Next);
      yyt->U_1.V_31.ProcHead.Objects4Out = yyt->U_1.V_31.ProcHead.Next->U_1.V_23.Decls.Objects4Out;
      break;
    case Tree_Module:;
      W_21->U_1.V_2.ModuleyImportyEnv2 = yyt->U_1.V_32.Module.Env3;
      yyVisit2Import(yyt->U_1.V_32.Module.Import, &W_21->U_1.V_2.ModuleyImportyEnv2);
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Env3 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_Predefs, yyt->U_1.V_32.Module.Import->U_1.V_9.Import.Objects2), yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Objects3), (Defs_tDefs)Defs_NoEnv);
      if (yyt->U_1.V_32.Module.Export->U_1.V_17.Export.IsQualified) {
        W_21->U_1.V_2.ModuleyExportyDefObjsIn = Defs_NoObjects;
      } else {
        W_21->U_1.V_2.ModuleyExportyDefObjsIn = yyt->U_1.V_32.Module.DefObjects;
      }
      yyVisit4Export(yyt->U_1.V_32.Module.Export, &W_21->U_1.V_2.ModuleyExportyDefObjsIn, &W_21->U_1.V_2.ModuleyExportyDefObjsOut);
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.DefObjects = W_21->U_1.V_2.ModuleyExportyDefObjsOut;
      yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Objects4In = Defs_NoObjects;
      yyVisit4Decls(yyt->U_1.V_32.Module.Decls);
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_32.Module.Env3;
      yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_32.Module.DefObjects;
      if (yyt->U_1.V_32.Module.Export->U_1.V_17.Export.IsQualified) {
        yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Objects4In = Defs_mElmt(yyt->U_1.V_32.Module.Ident, FALSE, Defs_mModule3(yyt->U_1.V_32.Module.Object, yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Objects4Out), yyt->U_1.V_32.Module.Objects4In);
      } else {
        yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Objects4In = Defs_mElmt(yyt->U_1.V_32.Module.Ident, FALSE, Defs_mModule3(yyt->U_1.V_32.Module.Object, yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Objects4Out), Defs_UNION(yyt->U_1.V_32.Module.Export->U_1.V_17.Export.Objects2, yyt->U_1.V_32.Module.Objects4In));
      }
      yyVisit4Decls(yyt->U_1.V_32.Module.Next);
      yyt->U_1.V_32.Module.Objects4Out = yyt->U_1.V_32.Module.Next->U_1.V_23.Decls.Objects4Out;
      break;
    case Tree_Opaque:;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Env3 = yyt->U_1.V_33.Opaque.Env3;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.DefObjects = yyt->U_1.V_33.Opaque.DefObjects;
      yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Objects4In = Defs_mElmt(yyt->U_1.V_33.Opaque.Ident, FALSE, yyt->U_1.V_33.Opaque.Object, yyt->U_1.V_33.Opaque.Objects4In);
      yyVisit4Decls(yyt->U_1.V_33.Opaque.Next);
      yyt->U_1.V_33.Opaque.Objects4Out = yyt->U_1.V_33.Opaque.Next->U_1.V_23.Decls.Objects4Out;
      break;
    default :
      break;
    }
  }
}

static void yyVisit5Decls
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv4, BOOLEAN *yyInLocal, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut, SHORTCARD *yyCntOut, BOOLEAN *yyGlobalPtrs)
# else
(yyt, yyEnv4, yyInLocal, yyIdsIn, yyIdsOut, yyCntOut, yyGlobalPtrs)
Tree_tTree yyt;
Defs_tEnv *yyEnv4;
BOOLEAN *yyInLocal;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
SHORTCARD *yyCntOut;
BOOLEAN *yyGlobalPtrs;
# endif
{
  struct S_22 yyTempo;

  {
    register struct S_22 *W_22 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Decls:;
      *yyGlobalPtrs = FALSE;
      *yyCntOut = yyt->U_1.V_23.Decls.CntIn;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Decls0:;
      *yyGlobalPtrs = FALSE;
      *yyCntOut = yyt->U_1.V_24.Decls0.CntIn;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Decl:;
      yyVisit5Decls(yyt->U_1.V_25.Decl.Next, yyEnv4, yyInLocal, yyIdsIn, yyIdsOut, yyCntOut, yyGlobalPtrs);
      break;
    case Tree_Var:;
      W_22->U_1.V_1.VaryVarIdsyModule = yyt->U_1.V_26.Var.Module;
      W_22->U_1.V_1.VaryVarIdsyKind = yyt->U_1.V_26.Var.Kind;
      yyVisit2VarIds(yyt->U_1.V_26.Var.VarIds, &W_22->U_1.V_1.VaryVarIdsyKind, &W_22->U_1.V_1.VaryVarIdsyModule, yyInLocal, yyIdsIn, &W_22->U_1.V_1.VaryVarIdsyIdsOut);
      W_22->U_1.V_1.VaryTypeyEnv3 = yyt->U_1.V_26.Var.Env3;
      yyVisit4Type(yyt->U_1.V_26.Var.Type, &W_22->U_1.V_1.VaryTypeyEnv3, &W_22->U_1.V_1.VaryVarIdsyIdsOut, &W_22->U_1.V_1.VaryNextyIdsIn);
      yyVisit5Decls(yyt->U_1.V_26.Var.Next, yyEnv4, yyInLocal, &W_22->U_1.V_1.VaryNextyIdsIn, yyIdsOut, yyCntOut, yyGlobalPtrs);
      break;
    case Tree_Object:;
      yyt->U_1.V_27.Object.CIdent = Idents_NoIdent;
      yyVisit5Decls(yyt->U_1.V_27.Object.Next, yyEnv4, yyInLocal, yyIdsIn, yyIdsOut, yyCntOut, yyGlobalPtrs);
      break;
    case Tree_Const:;
      W_22->U_1.V_2.ConstyExpryStrsIn = Defs_NoStrings;
      W_22->U_1.V_2.ConstyExpryLevel = yyt->U_1.V_28.Const.Level;
      W_22->U_1.V_2.ConstyExpryEnv = yyt->U_1.V_28.Const.Env3;
      yyVisit1Expr(yyt->U_1.V_28.Const.Expr, &W_22->U_1.V_2.ConstyExpryEnv, &W_22->U_1.V_2.ConstyExpryLevel, &W_22->U_1.V_2.ConstyExpryOpenAccessOrCall, &W_22->U_1.V_2.ConstyExpryGlobalPtrs, &W_22->U_1.V_2.ConstyExpryStrsIn, &W_22->U_1.V_2.ConstyExpryStrsOut);
      if (IN(yyt->U_1.V_28.Const.Kind, SET_ELEM(Tree_Definition) | SET_ELEM(Tree_Foreign))) {
        yyt->U_1.V_28.Const.CIdent = Defs_DefineCIdent(yyt->U_1.V_28.Const.Object, GenIdents_MakeQualified(yyt->U_1.V_28.Const.Module, yyt->U_1.V_28.Const.Ident));
        W_22->U_1.V_2.ConstyNextyIdsIn = *yyIdsIn;
      } else if (UniqueIds_NameConflict(*yyIdsIn, UniqueIds_eConst, yyt->U_1.V_28.Const.Ident)) {
        yyt->U_1.V_28.Const.CIdent = Defs_DefineCIdent(yyt->U_1.V_28.Const.Object, GenIdents_Rename(yyt->U_1.V_28.Const.Ident));
        W_22->U_1.V_2.ConstyNextyIdsIn = *yyIdsIn;
      } else {
        yyt->U_1.V_28.Const.CIdent = Defs_DefineCIdent(yyt->U_1.V_28.Const.Object, yyt->U_1.V_28.Const.Ident);
        W_22->U_1.V_2.ConstyNextyIdsIn = UniqueIds_DeclareIdent(*yyIdsIn, UniqueIds_eConst, yyt->U_1.V_28.Const.Ident);
      }
      yyVisit5Decls(yyt->U_1.V_28.Const.Next, yyEnv4, yyInLocal, &W_22->U_1.V_2.ConstyNextyIdsIn, yyIdsOut, yyCntOut, yyGlobalPtrs);
      break;
    case Tree_TypeDecl:;
      W_22->U_1.V_3.TypeDeclyTypeyEnv3 = yyt->U_1.V_29.TypeDecl.Env3;
      yyVisit4Type(yyt->U_1.V_29.TypeDecl.Type, &W_22->U_1.V_3.TypeDeclyTypeyEnv3, yyIdsIn, &W_22->U_1.V_3.TypeDeclyTypeyIdsOut);
      if (IN(yyt->U_1.V_29.TypeDecl.Kind, SET_ELEM(Tree_Definition) | SET_ELEM(Tree_Foreign))) {
        yyt->U_1.V_29.TypeDecl.CIdent = Defs_DefineCIdent(yyt->U_1.V_29.TypeDecl.Object, GenIdents_MakeQualified(yyt->U_1.V_29.TypeDecl.Module, yyt->U_1.V_29.TypeDecl.Ident));
        W_22->U_1.V_3.TypeDeclyNextyIdsIn = W_22->U_1.V_3.TypeDeclyTypeyIdsOut;
      } else if (Defs_IsDeclared(yyt->U_1.V_29.TypeDecl.Ident, yyt->U_1.V_29.TypeDecl.DefObjects)) {
        yyt->U_1.V_29.TypeDecl.CIdent = GenIdents_GenOpaque(Defs_DefineCIdent(yyt->U_1.V_29.TypeDecl.Object, GenIdents_MakeQualified(yyt->U_1.V_29.TypeDecl.Module, yyt->U_1.V_29.TypeDecl.Ident)));
        if (UniqueIds_NameConflict(W_22->U_1.V_3.TypeDeclyTypeyIdsOut, UniqueIds_eType, yyt->U_1.V_29.TypeDecl.Ident)) {
          Errors_ErrorMessagePI((LONGCARD)Errors_OpaqueConflict, (LONGCARD)Errors_Restriction, yyt->U_1.V_29.TypeDecl.Pos, (LONGCARD)Errors_Ident, ADR(yyt->U_1.V_29.TypeDecl.Ident));
          W_22->U_1.V_3.TypeDeclyNextyIdsIn = W_22->U_1.V_3.TypeDeclyTypeyIdsOut;
        } else {
          W_22->U_1.V_3.TypeDeclyNextyIdsIn = UniqueIds_DeclareIdent(W_22->U_1.V_3.TypeDeclyTypeyIdsOut, UniqueIds_eType, yyt->U_1.V_29.TypeDecl.Ident);
        }
      } else if (UniqueIds_NameConflict(W_22->U_1.V_3.TypeDeclyTypeyIdsOut, UniqueIds_eType, yyt->U_1.V_29.TypeDecl.Ident)) {
        yyt->U_1.V_29.TypeDecl.CIdent = Defs_DefineCIdent(yyt->U_1.V_29.TypeDecl.Object, GenIdents_Rename(yyt->U_1.V_29.TypeDecl.Ident));
        W_22->U_1.V_3.TypeDeclyNextyIdsIn = W_22->U_1.V_3.TypeDeclyTypeyIdsOut;
      } else {
        yyt->U_1.V_29.TypeDecl.CIdent = Defs_DefineCIdent(yyt->U_1.V_29.TypeDecl.Object, yyt->U_1.V_29.TypeDecl.Ident);
        W_22->U_1.V_3.TypeDeclyNextyIdsIn = UniqueIds_DeclareIdent(W_22->U_1.V_3.TypeDeclyTypeyIdsOut, UniqueIds_eType, yyt->U_1.V_29.TypeDecl.Ident);
      }
      yyVisit5Decls(yyt->U_1.V_29.TypeDecl.Next, yyEnv4, yyInLocal, &W_22->U_1.V_3.TypeDeclyNextyIdsIn, yyIdsOut, yyCntOut, yyGlobalPtrs);
      break;
    case Tree_Proc:;
      W_22->U_1.V_4.ProcyFormalsyEnv3 = yyt->U_1.V_30.Proc.Env3;
      W_22->U_1.V_4.ProcyResultTypeyEnv3 = yyt->U_1.V_30.Proc.Env3;
      yyVisit4PrimaryType(yyt->U_1.V_30.Proc.ResultType, &W_22->U_1.V_4.ProcyResultTypeyEnv3, yyIdsIn, &W_22->U_1.V_4.ProcyResultTypeyIdsOut);
      if (Defs_IsDeclared(yyt->U_1.V_30.Proc.Ident, yyt->U_1.V_30.Proc.DefObjects)) {
        yyt->U_1.V_30.Proc.CIdent = Defs_DefineCIdent(yyt->U_1.V_30.Proc.Object, GenIdents_MakeQualified(yyt->U_1.V_30.Proc.Module, yyt->U_1.V_30.Proc.Ident));
        W_22->U_1.V_4.ProcyNextyIdsIn = W_22->U_1.V_4.ProcyResultTypeyIdsOut;
      } else if (UniqueIds_NameConflict(W_22->U_1.V_4.ProcyResultTypeyIdsOut, UniqueIds_eProc, yyt->U_1.V_30.Proc.Ident)) {
        yyt->U_1.V_30.Proc.CIdent = Defs_DefineCIdent(yyt->U_1.V_30.Proc.Object, GenIdents_Rename(yyt->U_1.V_30.Proc.Ident));
        W_22->U_1.V_4.ProcyNextyIdsIn = W_22->U_1.V_4.ProcyResultTypeyIdsOut;
      } else {
        yyt->U_1.V_30.Proc.CIdent = Defs_DefineCIdent(yyt->U_1.V_30.Proc.Object, yyt->U_1.V_30.Proc.Ident);
        W_22->U_1.V_4.ProcyNextyIdsIn = UniqueIds_DeclareIdent(W_22->U_1.V_4.ProcyResultTypeyIdsOut, UniqueIds_eProc, yyt->U_1.V_30.Proc.Ident);
      }
      yyVisit5Decls(yyt->U_1.V_30.Proc.Next, yyEnv4, yyInLocal, &W_22->U_1.V_4.ProcyNextyIdsIn, &W_22->U_1.V_4.ProcyNextyIdsOut, yyCntOut, &W_22->U_1.V_4.ProcyNextyGlobalPtrs);
      W_22->U_1.V_4.ProcyFormalsyIdsIn = UniqueIds_EnterProc(W_22->U_1.V_4.ProcyNextyIdsOut);
      yyVisit3Formals(yyt->U_1.V_30.Proc.Formals, &W_22->U_1.V_4.ProcyFormalsyEnv3, &W_22->U_1.V_4.ProcyFormalsyIdsIn, &W_22->U_1.V_4.ProcyFormalsyIdsOut);
      W_22->U_1.V_4.ProcyDeclsyInLocal = FALSE;
      W_22->U_1.V_4.ProcyDeclsyEnv4 = Defs_mEnv(Defs_UNION(yyt->U_1.V_30.Proc.Formals->U_1.V_37.Formals.Objects3, yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Objects4Out), *yyEnv4);
      yyVisit5Decls(yyt->U_1.V_30.Proc.Decls, &W_22->U_1.V_4.ProcyDeclsyEnv4, &W_22->U_1.V_4.ProcyDeclsyInLocal, &W_22->U_1.V_4.ProcyFormalsyIdsOut, &W_22->U_1.V_4.ProcyDeclsyIdsOut, &W_22->U_1.V_4.ProcyDeclsyCntOut, &W_22->U_1.V_4.ProcyDeclsyGlobalPtrs);
      if (W_22->U_1.V_4.ProcyDeclsyGlobalPtrs) {
        yyt->U_1.V_30.Proc.LocalPtrs = Defs_Pointers(FALSE, Defs_UNION(yyt->U_1.V_30.Proc.Formals->U_1.V_37.Formals.Objects3, yyt->U_1.V_30.Proc.Decls->U_1.V_23.Decls.Objects4Out));
      } else {
        yyt->U_1.V_30.Proc.LocalPtrs = Defs_NoCObjects;
      }
      W_22->U_1.V_4.ProcyStmtsyType = Defs_GetResultType(yyt->U_1.V_30.Proc.Type);
      W_22->U_1.V_4.ProcyStmtsyStrsIn = Defs_NoStrings;
      W_22->U_1.V_4.ProcyStmtsyLevel = yyt->U_1.V_30.Proc.Level + 1;
      yyVisit1Stmts(yyt->U_1.V_30.Proc.Stmts, &W_22->U_1.V_4.ProcyDeclsyEnv4, &W_22->U_1.V_4.ProcyStmtsyLevel, &W_22->U_1.V_4.ProcyStmtsyGlobalPtrs, &W_22->U_1.V_4.ProcyStmtsyStrsIn, &W_22->U_1.V_4.ProcyStmtsyStrsOut, &W_22->U_1.V_4.ProcyStmtsyType);
      yyt->U_1.V_30.Proc.Strings = W_22->U_1.V_4.ProcyStmtsyStrsOut;
      *yyGlobalPtrs = W_22->U_1.V_4.ProcyDeclsyGlobalPtrs || W_22->U_1.V_4.ProcyStmtsyGlobalPtrs || W_22->U_1.V_4.ProcyNextyGlobalPtrs;
      *yyIdsOut = UniqueIds_LeaveProc(W_22->U_1.V_4.ProcyDeclsyIdsOut);
      break;
    case Tree_ProcHead:;
      W_22->U_1.V_5.ProcHeadyFormalsyEnv3 = yyt->U_1.V_31.ProcHead.Env3;
      W_22->U_1.V_5.ProcHeadyFormalsyIdsIn = UniqueIds_EnterProc(*yyIdsIn);
      yyVisit3Formals(yyt->U_1.V_31.ProcHead.Formals, &W_22->U_1.V_5.ProcHeadyFormalsyEnv3, &W_22->U_1.V_5.ProcHeadyFormalsyIdsIn, &W_22->U_1.V_5.ProcHeadyFormalsyIdsOut);
      W_22->U_1.V_5.ProcHeadyResultTypeyIdsIn = UniqueIds_LeaveProc(W_22->U_1.V_5.ProcHeadyFormalsyIdsOut);
      W_22->U_1.V_5.ProcHeadyResultTypeyEnv3 = yyt->U_1.V_31.ProcHead.Env3;
      yyVisit4PrimaryType(yyt->U_1.V_31.ProcHead.ResultType, &W_22->U_1.V_5.ProcHeadyResultTypeyEnv3, &W_22->U_1.V_5.ProcHeadyResultTypeyIdsIn, &W_22->U_1.V_5.ProcHeadyResultTypeyIdsOut);
      if (yyt->U_1.V_31.ProcHead.Kind == Tree_Foreign) {
        yyt->U_1.V_31.ProcHead.CIdent = Defs_DefineCIdent(yyt->U_1.V_31.ProcHead.Object, yyt->U_1.V_31.ProcHead.Ident);
        if (UniqueIds_NameConflict(W_22->U_1.V_5.ProcHeadyResultTypeyIdsOut, UniqueIds_eProc, yyt->U_1.V_31.ProcHead.Ident)) {
          Errors_ErrorMessagePI((LONGCARD)Errors_ForeignConflict, (LONGCARD)Errors_Warning, yyt->U_1.V_31.ProcHead.Pos, (LONGCARD)Errors_Ident, ADR(yyt->U_1.V_31.ProcHead.Ident));
          W_22->U_1.V_5.ProcHeadyNextyIdsIn = W_22->U_1.V_5.ProcHeadyResultTypeyIdsOut;
        } else {
          W_22->U_1.V_5.ProcHeadyNextyIdsIn = UniqueIds_DeclareIdent(W_22->U_1.V_5.ProcHeadyResultTypeyIdsOut, UniqueIds_eProc, yyt->U_1.V_31.ProcHead.Ident);
        }
      } else {
        yyt->U_1.V_31.ProcHead.CIdent = Defs_DefineCIdent(yyt->U_1.V_31.ProcHead.Object, GenIdents_MakeQualified(yyt->U_1.V_31.ProcHead.Module, yyt->U_1.V_31.ProcHead.Ident));
        W_22->U_1.V_5.ProcHeadyNextyIdsIn = W_22->U_1.V_5.ProcHeadyResultTypeyIdsOut;
      }
      yyVisit5Decls(yyt->U_1.V_31.ProcHead.Next, yyEnv4, yyInLocal, &W_22->U_1.V_5.ProcHeadyNextyIdsIn, yyIdsOut, yyCntOut, yyGlobalPtrs);
      break;
    case Tree_Module:;
      W_22->U_1.V_6.ModuleyStmtsyType = Defs_TypeVOID;
      W_22->U_1.V_6.ModuleyStmtsyStrsIn = Defs_NoStrings;
      W_22->U_1.V_6.ModuleyStmtsyLevel = yyt->U_1.V_32.Module.Level + 1;
      W_22->U_1.V_6.ModuleyDeclsyEnv4 = Defs_mEnv(Defs_UNION(Defs_UNION(Defs_Predefs, yyt->U_1.V_32.Module.Import->U_1.V_9.Import.Objects2), yyt->U_1.V_32.Module.Decls->U_1.V_23.Decls.Objects4Out), (Defs_tDefs)Defs_NoEnv);
      yyVisit1Stmts(yyt->U_1.V_32.Module.Stmts, &W_22->U_1.V_6.ModuleyDeclsyEnv4, &W_22->U_1.V_6.ModuleyStmtsyLevel, &W_22->U_1.V_6.ModuleyStmtsyGlobalPtrs, &W_22->U_1.V_6.ModuleyStmtsyStrsIn, &W_22->U_1.V_6.ModuleyStmtsyStrsOut, &W_22->U_1.V_6.ModuleyStmtsyType);
      if (UniqueIds_NameConflict(*yyIdsIn, UniqueIds_eProc, yyt->U_1.V_32.Module.Ident)) {
        yyt->U_1.V_32.Module.CIdent = Defs_DefineCIdent(yyt->U_1.V_32.Module.Object, GenIdents_Rename(yyt->U_1.V_32.Module.Ident));
        W_22->U_1.V_6.ModuleyNextyIdsIn = *yyIdsIn;
      } else {
        yyt->U_1.V_32.Module.CIdent = Defs_DefineCIdent(yyt->U_1.V_32.Module.Object, yyt->U_1.V_32.Module.Ident);
        W_22->U_1.V_6.ModuleyNextyIdsIn = UniqueIds_DeclareIdent(*yyIdsIn, UniqueIds_eProc, yyt->U_1.V_32.Module.Ident);
      }
      yyt->U_1.V_32.Module.Strings = W_22->U_1.V_6.ModuleyStmtsyStrsOut;
      yyVisit5Decls(yyt->U_1.V_32.Module.Next, yyEnv4, yyInLocal, &W_22->U_1.V_6.ModuleyNextyIdsIn, &W_22->U_1.V_6.ModuleyNextyIdsOut, yyCntOut, &W_22->U_1.V_6.ModuleyNextyGlobalPtrs);
      W_22->U_1.V_6.ModuleyDeclsyInLocal = TRUE;
      yyVisit5Decls(yyt->U_1.V_32.Module.Decls, &W_22->U_1.V_6.ModuleyDeclsyEnv4, &W_22->U_1.V_6.ModuleyDeclsyInLocal, &W_22->U_1.V_6.ModuleyNextyIdsOut, yyIdsOut, &W_22->U_1.V_6.ModuleyDeclsyCntOut, &W_22->U_1.V_6.ModuleyDeclsyGlobalPtrs);
      *yyGlobalPtrs = W_22->U_1.V_6.ModuleyDeclsyGlobalPtrs || W_22->U_1.V_6.ModuleyStmtsyGlobalPtrs || W_22->U_1.V_6.ModuleyNextyGlobalPtrs;
      break;
    case Tree_Opaque:;
      yyt->U_1.V_33.Opaque.CIdent = Defs_DefineCIdent(yyt->U_1.V_33.Opaque.Object, GenIdents_MakeQualified(yyt->U_1.V_33.Opaque.Module, yyt->U_1.V_33.Opaque.Ident));
      yyVisit5Decls(yyt->U_1.V_33.Opaque.Next, yyEnv4, yyInLocal, yyIdsIn, yyIdsOut, yyCntOut, yyGlobalPtrs);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1VarIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tType *yyType, ADDRESS *yyTypeTree, SHORTCARD *yyLevel)
# else
(yyt, yyType, yyTypeTree, yyLevel)
Tree_tTree yyt;
Defs_tType *yyType;
ADDRESS *yyTypeTree;
SHORTCARD *yyLevel;
# endif
{
  struct S_23 yyTempo;

  {
    register struct S_23 *W_23 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_VarIds:;
      yyt->U_1.V_34.VarIds.Objects3 = Defs_NoObjects;
      break;
    case Tree_VarIds0:;
      yyt->U_1.V_35.VarIds0.Objects3 = Defs_NoObjects;
      break;
    case Tree_VarIds1:;
      yyt->U_1.V_36.VarIds1.Object = Defs_mVar1(yyt->U_1.V_36.VarIds1.Ident, *yyType, FALSE, *yyLevel, FALSE, *yyTypeTree);
      yyVisit1VarIds(yyt->U_1.V_36.VarIds1.Next, yyType, yyTypeTree, yyLevel);
      yyt->U_1.V_36.VarIds1.Objects3 = Defs_mElmt(yyt->U_1.V_36.VarIds1.Ident, FALSE, yyt->U_1.V_36.VarIds1.Object, yyt->U_1.V_36.VarIds1.Next->U_1.V_34.VarIds.Objects3);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2VarIds
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, BOOLEAN *yyInLocal, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyKind, yyModule, yyInLocal, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
SHORTCARD *yyKind;
Idents_tIdent *yyModule;
BOOLEAN *yyInLocal;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_24 yyTempo;

  {
    register struct S_24 *W_24 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_VarIds:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_VarIds0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_VarIds1:;
      if (IN(*yyKind, SET_ELEM(Tree_Definition) | SET_ELEM(Tree_Foreign))) {
        yyt->U_1.V_36.VarIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_36.VarIds1.Object, GenIdents_MakeQualified(*yyModule, yyt->U_1.V_36.VarIds1.Ident));
        W_24->U_1.V_1.VarIds1yNextyIdsIn = *yyIdsIn;
      } else if (*yyInLocal) {
        if (UniqueIds_NameConflict(*yyIdsIn, UniqueIds_eModuleVar, yyt->U_1.V_36.VarIds1.Ident)) {
          yyt->U_1.V_36.VarIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_36.VarIds1.Object, GenIdents_Rename(yyt->U_1.V_36.VarIds1.Ident));
          W_24->U_1.V_1.VarIds1yNextyIdsIn = *yyIdsIn;
        } else {
          yyt->U_1.V_36.VarIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_36.VarIds1.Object, yyt->U_1.V_36.VarIds1.Ident);
          W_24->U_1.V_1.VarIds1yNextyIdsIn = UniqueIds_DeclareIdent(*yyIdsIn, UniqueIds_eModuleVar, yyt->U_1.V_36.VarIds1.Ident);
        }
      } else {
        if (UniqueIds_NameConflict(*yyIdsIn, UniqueIds_eVar, yyt->U_1.V_36.VarIds1.Ident)) {
          yyt->U_1.V_36.VarIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_36.VarIds1.Object, GenIdents_Rename(yyt->U_1.V_36.VarIds1.Ident));
          W_24->U_1.V_1.VarIds1yNextyIdsIn = *yyIdsIn;
        } else {
          yyt->U_1.V_36.VarIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_36.VarIds1.Object, yyt->U_1.V_36.VarIds1.Ident);
          W_24->U_1.V_1.VarIds1yNextyIdsIn = UniqueIds_DeclareIdent(*yyIdsIn, UniqueIds_eVar, yyt->U_1.V_36.VarIds1.Ident);
        }
      }
      yyVisit2VarIds(yyt->U_1.V_36.VarIds1.Next, yyKind, yyModule, yyInLocal, &W_24->U_1.V_1.VarIds1yNextyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Formals
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyKind, yyModule, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyKind;
Idents_tIdent *yyModule;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_25 yyTempo;

  {
    register struct S_25 *W_25 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Formals:;
      *yyPosOut = *yyPosIn;
      break;
    case Tree_Formals0:;
      *yyPosOut = *yyPosIn;
      break;
    case Tree_Formals1:;
      yyt->U_1.V_39.Formals1.Type->U_1.V_43.Type.CntIn = 0;
      yyt->U_1.V_39.Formals1.Type->U_1.V_43.Type.Module = *yyModule;
      yyt->U_1.V_39.Formals1.Type->U_1.V_43.Type.Kind = *yyKind;
      yyVisit1Type(yyt->U_1.V_39.Formals1.Type, &W_25->U_1.V_1.Formals1yTypeyCntOut, yyPosIn, &W_25->U_1.V_1.Formals1yTypeyPosOut);
      yyVisit1Formals(yyt->U_1.V_39.Formals1.Next, yyKind, yyModule, &W_25->U_1.V_1.Formals1yTypeyPosOut, yyPosOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2Formals
# ifdef __STDC__
(Tree_tTree yyt, Defs_tTypes *yyTypes, Defs_tEnv *yyEnv1, Defs_tVoid *yyEnv2, SHORTCARD *yyLevel)
# else
(yyt, yyTypes, yyEnv1, yyEnv2, yyLevel)
Tree_tTree yyt;
Defs_tTypes *yyTypes;
Defs_tEnv *yyEnv1;
Defs_tVoid *yyEnv2;
SHORTCARD *yyLevel;
# endif
{
  struct S_26 yyTempo;

  {
    register struct S_26 *W_26 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Formals:;
      *yyTypes = Defs_NoTypes;
      yyt->U_1.V_37.Formals.Objects3 = Defs_NoObjects;
      break;
    case Tree_Formals0:;
      *yyTypes = Defs_NoTypes;
      yyt->U_1.V_38.Formals0.Objects3 = Defs_NoObjects;
      break;
    case Tree_Formals1:;
      yyt->U_1.V_39.Formals1.Type->U_1.V_43.Type.Env1 = *yyEnv1;
      W_26->U_1.V_1.Formals1yTypeyTypeObj = Defs_NoObject;
      yyVisit2Type(yyt->U_1.V_39.Formals1.Type, &W_26->U_1.V_1.Formals1yTypeyTypeObj);
      yyt->U_1.V_39.Formals1.Type->U_1.V_43.Type.Env2 = *yyEnv2;
      yyVisit3Type(yyt->U_1.V_39.Formals1.Type);
      yyVisit2Formals(yyt->U_1.V_39.Formals1.Next, &W_26->U_1.V_1.Formals1yParIdsyTypesIn, yyEnv1, yyEnv2, yyLevel);
      W_26->U_1.V_1.Formals1yParIdsyIsVAR = yyt->U_1.V_39.Formals1.IsVAR;
      W_26->U_1.V_1.Formals1yParIdsyTypeTree = (ADDRESS)yyt->U_1.V_39.Formals1.Type;
      W_26->U_1.V_1.Formals1yParIdsyType = yyt->U_1.V_39.Formals1.Type->U_1.V_43.Type.Type2;
      yyVisit1ParIds(yyt->U_1.V_39.Formals1.ParIds, &W_26->U_1.V_1.Formals1yParIdsyObjects3, &W_26->U_1.V_1.Formals1yParIdsyType, &W_26->U_1.V_1.Formals1yParIdsyTypeTree, &W_26->U_1.V_1.Formals1yParIdsyIsVAR, &W_26->U_1.V_1.Formals1yParIdsyTypesIn, yyTypes, yyLevel);
      yyt->U_1.V_39.Formals1.Objects3 = Defs_UNION(W_26->U_1.V_1.Formals1yParIdsyObjects3, yyt->U_1.V_39.Formals1.Next->U_1.V_37.Formals.Objects3);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3Formals
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_27 yyTempo;

  {
    register struct S_27 *W_27 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Formals:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Formals0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Formals1:;
      yyVisit2ParIds(yyt->U_1.V_39.Formals1.ParIds, yyIdsIn, &W_27->U_1.V_1.Formals1yParIdsyIdsOut);
      yyVisit4Type(yyt->U_1.V_39.Formals1.Type, yyEnv3, &W_27->U_1.V_1.Formals1yParIdsyIdsOut, &W_27->U_1.V_1.Formals1yTypeyIdsOut);
      yyVisit3Formals(yyt->U_1.V_39.Formals1.Next, yyEnv3, &W_27->U_1.V_1.Formals1yTypeyIdsOut, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1ParIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects3, Defs_tType *yyType, ADDRESS *yyTypeTree, BOOLEAN *yyIsVAR, Defs_tType *yyTypesIn, Defs_tTypes *yyTypesOut, SHORTCARD *yyLevel)
# else
(yyt, yyObjects3, yyType, yyTypeTree, yyIsVAR, yyTypesIn, yyTypesOut, yyLevel)
Tree_tTree yyt;
Defs_tObjects *yyObjects3;
Defs_tType *yyType;
ADDRESS *yyTypeTree;
BOOLEAN *yyIsVAR;
Defs_tType *yyTypesIn;
Defs_tTypes *yyTypesOut;
SHORTCARD *yyLevel;
# endif
{
  struct S_28 yyTempo;

  {
    register struct S_28 *W_28 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ParIds:;
      *yyTypesOut = *yyTypesIn;
      *yyObjects3 = Defs_NoObjects;
      break;
    case Tree_ParIds0:;
      *yyTypesOut = *yyTypesIn;
      *yyObjects3 = Defs_NoObjects;
      break;
    case Tree_ParIds1:;
      yyt->U_1.V_42.ParIds1.Object = Defs_mVar1(yyt->U_1.V_42.ParIds1.Ident, *yyType, *yyIsVAR, *yyLevel, FALSE, *yyTypeTree);
      yyVisit1ParIds(yyt->U_1.V_42.ParIds1.Next, &W_28->U_1.V_1.ParIds1yNextyObjects3, yyType, yyTypeTree, yyIsVAR, yyTypesIn, &W_28->U_1.V_1.ParIds1yNextyTypesOut, yyLevel);
      *yyTypesOut = Defs_mTypes(*yyIsVAR, *yyType, W_28->U_1.V_1.ParIds1yNextyTypesOut);
      *yyObjects3 = Defs_mElmt(yyt->U_1.V_42.ParIds1.Ident, FALSE, yyt->U_1.V_42.ParIds1.Object, W_28->U_1.V_1.ParIds1yNextyObjects3);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2ParIds
# ifdef __STDC__
(Tree_tTree yyt, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_29 yyTempo;

  {
    register struct S_29 *W_29 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_ParIds:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_ParIds0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_ParIds1:;
      if (UniqueIds_NameConflict(*yyIdsIn, UniqueIds_eVar, yyt->U_1.V_42.ParIds1.Ident)) {
        yyt->U_1.V_42.ParIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_42.ParIds1.Object, GenIdents_Rename(yyt->U_1.V_42.ParIds1.Ident));
        W_29->U_1.V_1.ParIds1yNextyIdsIn = *yyIdsIn;
      } else {
        yyt->U_1.V_42.ParIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_42.ParIds1.Object, yyt->U_1.V_42.ParIds1.Ident);
        W_29->U_1.V_1.ParIds1yNextyIdsIn = UniqueIds_DeclareIdent(*yyIdsIn, UniqueIds_eVar, yyt->U_1.V_42.ParIds1.Ident);
      }
      yyVisit2ParIds(yyt->U_1.V_42.ParIds1.Next, &W_29->U_1.V_1.ParIds1yNextyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Type
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyCntOut, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyCntOut;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_30 yyTempo;

  {
    register struct S_30 *W_30 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Type:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_43.Type.CntIn;
      break;
    case Tree_Array:;
      yyt->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.CntIn = yyt->U_1.V_44.Array.CntIn + 1;
      yyt->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Module = yyt->U_1.V_44.Array.Module;
      yyt->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Kind = yyt->U_1.V_44.Array.Kind;
      yyVisit1SimpleType(yyt->U_1.V_44.Array.IndexType, &W_30->U_1.V_1.ArrayyIndexTypeyCntOut, yyPosIn, &W_30->U_1.V_1.ArrayyIndexTypeyPosOut);
      yyt->U_1.V_44.Array.ElemType->U_1.V_43.Type.CntIn = W_30->U_1.V_1.ArrayyIndexTypeyCntOut;
      yyt->U_1.V_44.Array.ElemType->U_1.V_43.Type.Module = yyt->U_1.V_44.Array.Module;
      yyt->U_1.V_44.Array.ElemType->U_1.V_43.Type.Kind = yyt->U_1.V_44.Array.Kind;
      yyVisit1Type(yyt->U_1.V_44.Array.ElemType, yyCntOut, &W_30->U_1.V_1.ArrayyIndexTypeyPosOut, &W_30->U_1.V_1.ArrayyElemTypeyPosOut);
      *yyPosOut = W_30->U_1.V_1.ArrayyElemTypeyPosOut + 1;
      break;
    case Tree_Record:;
      W_30->U_1.V_2.RecordyFieldsyCntIn = yyt->U_1.V_45.Record.CntIn + 1;
      W_30->U_1.V_2.RecordyFieldsyModule = yyt->U_1.V_45.Record.Module;
      W_30->U_1.V_2.RecordyFieldsyKind = yyt->U_1.V_45.Record.Kind;
      yyVisit1Fields(yyt->U_1.V_45.Record.Fields, &W_30->U_1.V_2.RecordyFieldsyKind, &W_30->U_1.V_2.RecordyFieldsyModule, &W_30->U_1.V_2.RecordyFieldsyCntIn, yyCntOut, yyPosIn, &W_30->U_1.V_2.RecordyFieldsyPosOut);
      *yyPosOut = W_30->U_1.V_2.RecordyFieldsyPosOut + 1;
      break;
    case Tree_SetType:;
      yyt->U_1.V_46.SetType.BaseType->U_1.V_49.SimpleType.CntIn = 0;
      yyt->U_1.V_46.SetType.BaseType->U_1.V_49.SimpleType.Module = yyt->U_1.V_46.SetType.Module;
      yyt->U_1.V_46.SetType.BaseType->U_1.V_49.SimpleType.Kind = yyt->U_1.V_46.SetType.Kind;
      yyVisit1SimpleType(yyt->U_1.V_46.SetType.BaseType, &W_30->U_1.V_3.SetTypeyBaseTypeyCntOut, yyPosIn, &W_30->U_1.V_3.SetTypeyBaseTypeyPosOut);
      *yyPosOut = W_30->U_1.V_3.SetTypeyBaseTypeyPosOut + 1;
      *yyCntOut = yyt->U_1.V_46.SetType.CntIn;
      break;
    case Tree_Pointer:;
      yyt->U_1.V_47.Pointer.TargetType->U_1.V_43.Type.CntIn = yyt->U_1.V_47.Pointer.CntIn;
      yyt->U_1.V_47.Pointer.TargetType->U_1.V_43.Type.Module = yyt->U_1.V_47.Pointer.Module;
      yyt->U_1.V_47.Pointer.TargetType->U_1.V_43.Type.Kind = yyt->U_1.V_47.Pointer.Kind;
      yyVisit1Type(yyt->U_1.V_47.Pointer.TargetType, yyCntOut, yyPosIn, &W_30->U_1.V_4.PointeryTargetTypeyPosOut);
      *yyPosOut = W_30->U_1.V_4.PointeryTargetTypeyPosOut + 1;
      yyt->U_1.V_47.Pointer.TypePos = *yyPosOut;
      break;
    case Tree_ProcType:;
      W_30->U_1.V_5.ProcTypeyFormalTypesyModule = yyt->U_1.V_48.ProcType.Module;
      W_30->U_1.V_5.ProcTypeyFormalTypesyKind = yyt->U_1.V_48.ProcType.Kind;
      yyVisit1FormalTypes(yyt->U_1.V_48.ProcType.FormalTypes, &W_30->U_1.V_5.ProcTypeyFormalTypesyKind, &W_30->U_1.V_5.ProcTypeyFormalTypesyModule, yyPosIn, &W_30->U_1.V_5.ProcTypeyFormalTypesyPosOut);
      yyt->U_1.V_48.ProcType.ResultType->U_1.V_52.PrimaryType.CntIn = 0;
      yyt->U_1.V_48.ProcType.ResultType->U_1.V_52.PrimaryType.Module = yyt->U_1.V_48.ProcType.Module;
      yyt->U_1.V_48.ProcType.ResultType->U_1.V_52.PrimaryType.Kind = yyt->U_1.V_48.ProcType.Kind;
      yyVisit1PrimaryType(yyt->U_1.V_48.ProcType.ResultType, &W_30->U_1.V_5.ProcTypeyResultTypeyCntOut, &W_30->U_1.V_5.ProcTypeyFormalTypesyPosOut, &W_30->U_1.V_5.ProcTypeyResultTypeyPosOut);
      *yyPosOut = W_30->U_1.V_5.ProcTypeyResultTypeyPosOut + 1;
      *yyCntOut = yyt->U_1.V_48.ProcType.CntIn;
      break;
    case Tree_SimpleType:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_49.SimpleType.CntIn;
      break;
    case Tree_Enumeration:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_50.Enumeration.CntIn;
      break;
    case Tree_Subrange:;
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.CntIn = 0;
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Module = yyt->U_1.V_51.Subrange.Module;
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Kind = yyt->U_1.V_51.Subrange.Kind;
      yyVisit1PrimaryType(yyt->U_1.V_51.Subrange.BaseType, &W_30->U_1.V_6.SubrangeyBaseTypeyCntOut, yyPosIn, &W_30->U_1.V_6.SubrangeyBaseTypeyPosOut);
      *yyPosOut = W_30->U_1.V_6.SubrangeyBaseTypeyPosOut + 1;
      *yyCntOut = yyt->U_1.V_51.Subrange.CntIn;
      break;
    case Tree_PrimaryType:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_52.PrimaryType.CntIn;
      break;
    case Tree_Void:;
      *yyPosOut = *yyPosIn;
      *yyCntOut = yyt->U_1.V_53.Void.CntIn;
      break;
    case Tree_TypeId:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_54.TypeId.CntIn;
      break;
    case Tree_TypeId0:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_55.TypeId0.CntIn;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.CntIn = 0;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Module = yyt->U_1.V_56.TypeId1.Module;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Kind = yyt->U_1.V_56.TypeId1.Kind;
      yyVisit1TypeId(yyt->U_1.V_56.TypeId1.TypeId, &W_30->U_1.V_7.TypeId1yTypeIdyCntOut, yyPosIn, &W_30->U_1.V_7.TypeId1yTypeIdyPosOut);
      *yyPosOut = W_30->U_1.V_7.TypeId1yTypeIdyPosOut + 1;
      *yyCntOut = yyt->U_1.V_56.TypeId1.CntIn;
      break;
    default :
      break;
    }
  }
}

static void yyVisit2Type
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObject *yyTypeObj)
# else
(yyt, yyTypeObj)
Tree_tTree yyt;
Defs_tObject *yyTypeObj;
# endif
{
  struct S_31 yyTempo;

  {
    register struct S_31 *W_31 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Type:;
      yyt->U_1.V_43.Type.Type1 = Defs_NoType;
      break;
    case Tree_Array:;
      if (IN(yyt->U_1.V_44.Array.Kind, SET_ELEM(Tree_Definition) | SET_ELEM(Tree_Foreign))) {
        yyt->U_1.V_44.Array.Type1 = Defs_mArray1(*yyTypeObj, GenIdents_GenStruct1(yyt->U_1.V_44.Array.Module, (LONGCARD)(yyt->U_1.V_44.Array.CntIn + 1)));
      } else {
        yyt->U_1.V_44.Array.Type1 = Defs_mArray1(*yyTypeObj, GenIdents_GenStruct2());
      }
      break;
    case Tree_Record:;
      if (IN(yyt->U_1.V_45.Record.Kind, SET_ELEM(Tree_Definition) | SET_ELEM(Tree_Foreign))) {
        yyt->U_1.V_45.Record.Type1 = Defs_mRecord1(*yyTypeObj, GenIdents_GenStruct1(yyt->U_1.V_45.Record.Module, (LONGCARD)(yyt->U_1.V_45.Record.CntIn + 1)));
      } else {
        yyt->U_1.V_45.Record.Type1 = Defs_mRecord1(*yyTypeObj, GenIdents_GenStruct2());
      }
      break;
    case Tree_SetType:;
      yyt->U_1.V_46.SetType.Type1 = Defs_mSet1(*yyTypeObj);
      break;
    case Tree_Pointer:;
      yyt->U_1.V_47.Pointer.Type1 = Defs_mPointer1(*yyTypeObj);
      break;
    case Tree_ProcType:;
      yyt->U_1.V_48.ProcType.Type1 = Defs_mProcType1(*yyTypeObj);
      break;
    case Tree_SimpleType:;
      yyt->U_1.V_49.SimpleType.Type1 = Defs_NoType;
      break;
    case Tree_Enumeration:;
      W_31->U_1.V_1.EnumerationyEnumIdsyIndexIn = 0;
      W_31->U_1.V_1.EnumerationyyType0 = Defs_mEnumeration1(*yyTypeObj);
      yyVisit1EnumIds(yyt->U_1.V_50.Enumeration.EnumIds, &W_31->U_1.V_1.EnumerationyyType0, &W_31->U_1.V_1.EnumerationyEnumIdsyIndexIn, &W_31->U_1.V_1.EnumerationyEnumIdsyIndexOut);
      yyt->U_1.V_50.Enumeration.Type1 = Defs_mEnumeration2(W_31->U_1.V_1.EnumerationyyType0, yyt->U_1.V_50.Enumeration.EnumIds->U_1.V_74.EnumIds.Objects, W_31->U_1.V_1.EnumerationyEnumIdsyIndexOut - 1);
      break;
    case Tree_Subrange:;
      yyt->U_1.V_51.Subrange.Type1 = Defs_mSubrange1(*yyTypeObj, (ADDRESS)yyt->U_1.V_51.Subrange.Lwb, (ADDRESS)yyt->U_1.V_51.Subrange.Upb);
      break;
    case Tree_PrimaryType:;
      yyt->U_1.V_52.PrimaryType.Type1 = Defs_NoType;
      break;
    case Tree_Void:;
      yyt->U_1.V_53.Void.Type1 = Defs_TypeVOID;
      break;
    case Tree_TypeId:;
      yyt->U_1.V_54.TypeId.Object = Defs_NoObject;
      yyt->U_1.V_54.TypeId.Type1 = Defs_NoType;
      break;
    case Tree_TypeId0:;
      yyt->U_1.V_55.TypeId0.Object = Defs_Identify(yyt->U_1.V_55.TypeId0.Ident, yyt->U_1.V_55.TypeId0.Env1);
      yyt->U_1.V_55.TypeId0.Type1 = Defs_mQualident1(yyt->U_1.V_55.TypeId0.Object);
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Env1 = yyt->U_1.V_56.TypeId1.Env1;
      yyVisit2TypeId(yyt->U_1.V_56.TypeId1.TypeId, yyTypeObj);
      yyt->U_1.V_56.TypeId1.Object = Defs_Identify2(yyt->U_1.V_56.TypeId1.Ident, Defs_GetExport1(yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Object));
      yyt->U_1.V_56.TypeId1.Type1 = Defs_mQualident1(yyt->U_1.V_56.TypeId1.Object);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3Type
# ifdef __STDC__
(Tree_tTree yyt)
# else
(yyt)
Tree_tTree yyt;
# endif
{
  struct S_32 yyTempo;

  {
    register struct S_32 *W_32 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Type:;
      yyt->U_1.V_43.Type.Type2 = yyt->U_1.V_43.Type.Type1;
      yyt->U_1.V_43.Type.Objects3 = Defs_NoObjects;
      break;
    case Tree_Array:;
      yyt->U_1.V_44.Array.ElemType->U_1.V_43.Type.Env1 = yyt->U_1.V_44.Array.Env1;
      W_32->U_1.V_1.ArrayyElemTypeyTypeObj = Defs_NoObject;
      yyVisit2Type(yyt->U_1.V_44.Array.ElemType, &W_32->U_1.V_1.ArrayyElemTypeyTypeObj);
      yyt->U_1.V_44.Array.ElemType->U_1.V_43.Type.Env2 = yyt->U_1.V_44.Array.Env2;
      yyVisit3Type(yyt->U_1.V_44.Array.ElemType);
      yyt->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Env1 = yyt->U_1.V_44.Array.Env1;
      W_32->U_1.V_1.ArrayyIndexTypeyTypeObj = Defs_NoObject;
      yyVisit2SimpleType(yyt->U_1.V_44.Array.IndexType, &W_32->U_1.V_1.ArrayyIndexTypeyTypeObj);
      yyt->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Env2 = yyt->U_1.V_44.Array.Env2;
      yyVisit3SimpleType(yyt->U_1.V_44.Array.IndexType);
      yyt->U_1.V_44.Array.Type2 = Defs_mArray2(yyt->U_1.V_44.Array.Type1, yyt->U_1.V_44.Array.IsOpen, yyt->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Type2, yyt->U_1.V_44.Array.ElemType->U_1.V_43.Type.Type2);
      yyt->U_1.V_44.Array.Objects3 = Defs_UNION(yyt->U_1.V_44.Array.IndexType->U_1.V_49.SimpleType.Objects3, yyt->U_1.V_44.Array.ElemType->U_1.V_43.Type.Objects3);
      break;
    case Tree_Record:;
      W_32->U_1.V_2.RecordyFieldsySelect = Defs_NoSelectors;
      W_32->U_1.V_2.RecordyFieldsynUnion = 0;
      W_32->U_1.V_2.RecordyFieldsyEnv1 = yyt->U_1.V_45.Record.Env1;
      W_32->U_1.V_2.RecordyFieldsyEnv2 = yyt->U_1.V_45.Record.Env2;
      W_32->U_1.V_2.RecordyFieldsyFieldsIn = Defs_NoObjects;
      yyVisit2Fields(yyt->U_1.V_45.Record.Fields, &W_32->U_1.V_2.RecordyFieldsyObjects3, &W_32->U_1.V_2.RecordyFieldsyFieldsIn, &W_32->U_1.V_2.RecordyFieldsyFieldsOut, &W_32->U_1.V_2.RecordyFieldsyEnv1, &W_32->U_1.V_2.RecordyFieldsyEnv2, &W_32->U_1.V_2.RecordyFieldsynUnion, &W_32->U_1.V_2.RecordyFieldsySelect);
      yyt->U_1.V_45.Record.Type2 = Defs_mRecord2(yyt->U_1.V_45.Record.Type1, W_32->U_1.V_2.RecordyFieldsyFieldsOut);
      yyt->U_1.V_45.Record.Objects3 = W_32->U_1.V_2.RecordyFieldsyObjects3;
      break;
    case Tree_SetType:;
      yyt->U_1.V_46.SetType.BaseType->U_1.V_49.SimpleType.Env1 = yyt->U_1.V_46.SetType.Env1;
      W_32->U_1.V_3.SetTypeyBaseTypeyTypeObj = Defs_NoObject;
      yyVisit2SimpleType(yyt->U_1.V_46.SetType.BaseType, &W_32->U_1.V_3.SetTypeyBaseTypeyTypeObj);
      yyt->U_1.V_46.SetType.BaseType->U_1.V_49.SimpleType.Env2 = yyt->U_1.V_46.SetType.Env2;
      yyVisit3SimpleType(yyt->U_1.V_46.SetType.BaseType);
      yyt->U_1.V_46.SetType.Type2 = Defs_mSet2(yyt->U_1.V_46.SetType.Type1, yyt->U_1.V_46.SetType.BaseType->U_1.V_49.SimpleType.Type2);
      yyt->U_1.V_46.SetType.Objects3 = yyt->U_1.V_46.SetType.BaseType->U_1.V_49.SimpleType.Objects3;
      break;
    case Tree_Pointer:;
      yyt->U_1.V_47.Pointer.TargetType->U_1.V_43.Type.Env1 = yyt->U_1.V_47.Pointer.Env1;
      W_32->U_1.V_4.PointeryTargetTypeyTypeObj = Defs_NoObject;
      yyVisit2Type(yyt->U_1.V_47.Pointer.TargetType, &W_32->U_1.V_4.PointeryTargetTypeyTypeObj);
      yyt->U_1.V_47.Pointer.TargetType->U_1.V_43.Type.Env2 = yyt->U_1.V_47.Pointer.Env2;
      yyVisit3Type(yyt->U_1.V_47.Pointer.TargetType);
      yyt->U_1.V_47.Pointer.Type2 = Defs_mPointer2(yyt->U_1.V_47.Pointer.Type1, yyt->U_1.V_47.Pointer.TargetType->U_1.V_43.Type.Type2);
      yyt->U_1.V_47.Pointer.Objects3 = yyt->U_1.V_47.Pointer.TargetType->U_1.V_43.Type.Objects3;
      break;
    case Tree_ProcType:;
      yyt->U_1.V_48.ProcType.ResultType->U_1.V_52.PrimaryType.Env1 = yyt->U_1.V_48.ProcType.Env1;
      W_32->U_1.V_5.ProcTypeyResultTypeyTypeObj = Defs_NoObject;
      yyVisit2PrimaryType(yyt->U_1.V_48.ProcType.ResultType, &W_32->U_1.V_5.ProcTypeyResultTypeyTypeObj);
      W_32->U_1.V_5.ProcTypeyFormalTypesyEnv1 = yyt->U_1.V_48.ProcType.Env1;
      yyt->U_1.V_48.ProcType.ResultType->U_1.V_52.PrimaryType.Env2 = yyt->U_1.V_48.ProcType.Env2;
      yyVisit3PrimaryType(yyt->U_1.V_48.ProcType.ResultType);
      W_32->U_1.V_5.ProcTypeyFormalTypesyEnv2 = yyt->U_1.V_48.ProcType.Env2;
      yyVisit2FormalTypes(yyt->U_1.V_48.ProcType.FormalTypes, &W_32->U_1.V_5.ProcTypeyFormalTypesyTypes, &W_32->U_1.V_5.ProcTypeyFormalTypesyEnv1, &W_32->U_1.V_5.ProcTypeyFormalTypesyEnv2);
      yyt->U_1.V_48.ProcType.Type2 = Defs_mProcType2(yyt->U_1.V_48.ProcType.Type1, W_32->U_1.V_5.ProcTypeyFormalTypesyTypes, yyt->U_1.V_48.ProcType.ResultType->U_1.V_52.PrimaryType.Type2);
      yyt->U_1.V_48.ProcType.Objects3 = Defs_NoObjects;
      break;
    case Tree_SimpleType:;
      yyt->U_1.V_49.SimpleType.Type2 = yyt->U_1.V_49.SimpleType.Type1;
      yyt->U_1.V_49.SimpleType.Objects3 = Defs_NoObjects;
      break;
    case Tree_Enumeration:;
      yyt->U_1.V_50.Enumeration.Type2 = yyt->U_1.V_50.Enumeration.Type1;
      yyt->U_1.V_50.Enumeration.Objects3 = yyt->U_1.V_50.Enumeration.EnumIds->U_1.V_74.EnumIds.Objects;
      break;
    case Tree_Subrange:;
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Env1 = yyt->U_1.V_51.Subrange.Env1;
      W_32->U_1.V_6.SubrangeyBaseTypeyTypeObj = Defs_NoObject;
      yyVisit2PrimaryType(yyt->U_1.V_51.Subrange.BaseType, &W_32->U_1.V_6.SubrangeyBaseTypeyTypeObj);
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Env2 = yyt->U_1.V_51.Subrange.Env2;
      yyVisit3PrimaryType(yyt->U_1.V_51.Subrange.BaseType);
      yyt->U_1.V_51.Subrange.Type2 = Defs_mSubrange2(yyt->U_1.V_51.Subrange.Type1, yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Type2);
      yyt->U_1.V_51.Subrange.Objects3 = Defs_NoObjects;
      break;
    case Tree_PrimaryType:;
      yyt->U_1.V_52.PrimaryType.Type2 = yyt->U_1.V_52.PrimaryType.Type1;
      yyt->U_1.V_52.PrimaryType.Objects3 = Defs_NoObjects;
      break;
    case Tree_Void:;
      yyt->U_1.V_53.Void.Type2 = yyt->U_1.V_53.Void.Type1;
      yyt->U_1.V_53.Void.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId:;
      yyt->U_1.V_54.TypeId.Type2 = yyt->U_1.V_54.TypeId.Type1;
      yyt->U_1.V_54.TypeId.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId0:;
      yyt->U_1.V_55.TypeId0.Type2 = Defs_GroundType(yyt->U_1.V_55.TypeId0.Type1);
      yyt->U_1.V_55.TypeId0.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.Type2 = Defs_GroundType(yyt->U_1.V_56.TypeId1.Type1);
      yyt->U_1.V_56.TypeId1.Objects3 = Defs_NoObjects;
      break;
    default :
      break;
    }
  }
}

static void yyVisit4Type
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_33 yyTempo;

  {
    register struct S_33 *W_33 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Type:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Array:;
      yyVisit4SimpleType(yyt->U_1.V_44.Array.IndexType, yyEnv3, yyIdsIn, &W_33->U_1.V_1.ArrayyIndexTypeyIdsOut);
      yyVisit4Type(yyt->U_1.V_44.Array.ElemType, yyEnv3, &W_33->U_1.V_1.ArrayyIndexTypeyIdsOut, yyIdsOut);
      break;
    case Tree_Record:;
      yyVisit3Fields(yyt->U_1.V_45.Record.Fields, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    case Tree_SetType:;
      yyVisit4SimpleType(yyt->U_1.V_46.SetType.BaseType, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    case Tree_Pointer:;
      yyVisit4Type(yyt->U_1.V_47.Pointer.TargetType, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    case Tree_ProcType:;
      yyVisit3FormalTypes(yyt->U_1.V_48.ProcType.FormalTypes, yyEnv3, yyIdsIn, &W_33->U_1.V_2.ProcTypeyFormalTypesyIdsOut);
      yyVisit4PrimaryType(yyt->U_1.V_48.ProcType.ResultType, yyEnv3, &W_33->U_1.V_2.ProcTypeyFormalTypesyIdsOut, yyIdsOut);
      break;
    case Tree_SimpleType:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Enumeration:;
      W_33->U_1.V_3.EnumerationyEnumIdsyModule = yyt->U_1.V_50.Enumeration.Module;
      W_33->U_1.V_3.EnumerationyEnumIdsyKind = yyt->U_1.V_50.Enumeration.Kind;
      yyVisit2EnumIds(yyt->U_1.V_50.Enumeration.EnumIds, &W_33->U_1.V_3.EnumerationyEnumIdsyKind, &W_33->U_1.V_3.EnumerationyEnumIdsyModule, yyIdsIn, yyIdsOut);
      break;
    case Tree_Subrange:;
      W_33->U_1.V_4.SubrangeyUpbyStrsIn = Defs_NoStrings;
      W_33->U_1.V_4.SubrangeyUpbyLevel = 0;
      yyVisit1Expr(yyt->U_1.V_51.Subrange.Upb, yyEnv3, &W_33->U_1.V_4.SubrangeyUpbyLevel, &W_33->U_1.V_4.SubrangeyUpbyOpenAccessOrCall, &W_33->U_1.V_4.SubrangeyUpbyGlobalPtrs, &W_33->U_1.V_4.SubrangeyUpbyStrsIn, &W_33->U_1.V_4.SubrangeyUpbyStrsOut);
      W_33->U_1.V_4.SubrangeyLwbyStrsIn = Defs_NoStrings;
      W_33->U_1.V_4.SubrangeyLwbyLevel = 0;
      yyVisit1Expr(yyt->U_1.V_51.Subrange.Lwb, yyEnv3, &W_33->U_1.V_4.SubrangeyLwbyLevel, &W_33->U_1.V_4.SubrangeyLwbyOpenAccessOrCall, &W_33->U_1.V_4.SubrangeyLwbyGlobalPtrs, &W_33->U_1.V_4.SubrangeyLwbyStrsIn, &W_33->U_1.V_4.SubrangeyLwbyStrsOut);
      yyVisit4PrimaryType(yyt->U_1.V_51.Subrange.BaseType, yyEnv3, yyIdsIn, yyIdsOut);
      Values_CompConst((ADDRESS)yyt->U_1.V_51.Subrange.Lwb, (ADDRESS)(*yyEnv3), &yyt->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Lwb);
      Values_CompConst((ADDRESS)yyt->U_1.V_51.Subrange.Upb, (ADDRESS)(*yyEnv3), &yyt->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Upb);
      W_33->U_1.V_4.SubrangeyyType3 = Defs_mSubrange3(yyt->U_1.V_51.Subrange.Type2, yyt->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Lwb, yyt->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Upb);
      break;
    case Tree_PrimaryType:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Void:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Env2 = yyt->U_1.V_56.TypeId1.Env2;
      yyVisit3TypeId(yyt->U_1.V_56.TypeId1.TypeId);
      yyVisit4TypeId(yyt->U_1.V_56.TypeId1.TypeId, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1SimpleType
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyCntOut, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyCntOut;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_34 yyTempo;

  {
    register struct S_34 *W_34 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_SimpleType:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_49.SimpleType.CntIn;
      break;
    case Tree_Enumeration:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_50.Enumeration.CntIn;
      break;
    case Tree_Subrange:;
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.CntIn = 0;
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Module = yyt->U_1.V_51.Subrange.Module;
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Kind = yyt->U_1.V_51.Subrange.Kind;
      yyVisit1PrimaryType(yyt->U_1.V_51.Subrange.BaseType, &W_34->U_1.V_1.SubrangeyBaseTypeyCntOut, yyPosIn, &W_34->U_1.V_1.SubrangeyBaseTypeyPosOut);
      *yyPosOut = W_34->U_1.V_1.SubrangeyBaseTypeyPosOut + 1;
      *yyCntOut = yyt->U_1.V_51.Subrange.CntIn;
      break;
    case Tree_PrimaryType:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_52.PrimaryType.CntIn;
      break;
    case Tree_Void:;
      *yyPosOut = *yyPosIn;
      *yyCntOut = yyt->U_1.V_53.Void.CntIn;
      break;
    case Tree_TypeId:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_54.TypeId.CntIn;
      break;
    case Tree_TypeId0:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_55.TypeId0.CntIn;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.CntIn = 0;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Module = yyt->U_1.V_56.TypeId1.Module;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Kind = yyt->U_1.V_56.TypeId1.Kind;
      yyVisit1TypeId(yyt->U_1.V_56.TypeId1.TypeId, &W_34->U_1.V_2.TypeId1yTypeIdyCntOut, yyPosIn, &W_34->U_1.V_2.TypeId1yTypeIdyPosOut);
      *yyPosOut = W_34->U_1.V_2.TypeId1yTypeIdyPosOut + 1;
      *yyCntOut = yyt->U_1.V_56.TypeId1.CntIn;
      break;
    default :
      break;
    }
  }
}

static void yyVisit2SimpleType
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObject *yyTypeObj)
# else
(yyt, yyTypeObj)
Tree_tTree yyt;
Defs_tObject *yyTypeObj;
# endif
{
  struct S_35 yyTempo;

  {
    register struct S_35 *W_35 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_SimpleType:;
      yyt->U_1.V_49.SimpleType.Type1 = Defs_NoType;
      break;
    case Tree_Enumeration:;
      W_35->U_1.V_1.EnumerationyEnumIdsyIndexIn = 0;
      W_35->U_1.V_1.EnumerationyyType0 = Defs_mEnumeration1(*yyTypeObj);
      yyVisit1EnumIds(yyt->U_1.V_50.Enumeration.EnumIds, &W_35->U_1.V_1.EnumerationyyType0, &W_35->U_1.V_1.EnumerationyEnumIdsyIndexIn, &W_35->U_1.V_1.EnumerationyEnumIdsyIndexOut);
      yyt->U_1.V_50.Enumeration.Type1 = Defs_mEnumeration2(W_35->U_1.V_1.EnumerationyyType0, yyt->U_1.V_50.Enumeration.EnumIds->U_1.V_74.EnumIds.Objects, W_35->U_1.V_1.EnumerationyEnumIdsyIndexOut - 1);
      break;
    case Tree_Subrange:;
      yyt->U_1.V_51.Subrange.Type1 = Defs_mSubrange1(*yyTypeObj, (ADDRESS)yyt->U_1.V_51.Subrange.Lwb, (ADDRESS)yyt->U_1.V_51.Subrange.Upb);
      break;
    case Tree_PrimaryType:;
      yyt->U_1.V_52.PrimaryType.Type1 = Defs_NoType;
      break;
    case Tree_Void:;
      yyt->U_1.V_53.Void.Type1 = Defs_TypeVOID;
      break;
    case Tree_TypeId:;
      yyt->U_1.V_54.TypeId.Object = Defs_NoObject;
      yyt->U_1.V_54.TypeId.Type1 = Defs_NoType;
      break;
    case Tree_TypeId0:;
      yyt->U_1.V_55.TypeId0.Object = Defs_Identify(yyt->U_1.V_55.TypeId0.Ident, yyt->U_1.V_55.TypeId0.Env1);
      yyt->U_1.V_55.TypeId0.Type1 = Defs_mQualident1(yyt->U_1.V_55.TypeId0.Object);
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Env1 = yyt->U_1.V_56.TypeId1.Env1;
      yyVisit2TypeId(yyt->U_1.V_56.TypeId1.TypeId, yyTypeObj);
      yyt->U_1.V_56.TypeId1.Object = Defs_Identify2(yyt->U_1.V_56.TypeId1.Ident, Defs_GetExport1(yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Object));
      yyt->U_1.V_56.TypeId1.Type1 = Defs_mQualident1(yyt->U_1.V_56.TypeId1.Object);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3SimpleType
# ifdef __STDC__
(Tree_tTree yyt)
# else
(yyt)
Tree_tTree yyt;
# endif
{
  struct S_36 yyTempo;

  {
    register struct S_36 *W_36 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_SimpleType:;
      yyt->U_1.V_49.SimpleType.Type2 = yyt->U_1.V_49.SimpleType.Type1;
      yyt->U_1.V_49.SimpleType.Objects3 = Defs_NoObjects;
      break;
    case Tree_Enumeration:;
      yyt->U_1.V_50.Enumeration.Type2 = yyt->U_1.V_50.Enumeration.Type1;
      yyt->U_1.V_50.Enumeration.Objects3 = yyt->U_1.V_50.Enumeration.EnumIds->U_1.V_74.EnumIds.Objects;
      break;
    case Tree_Subrange:;
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Env1 = yyt->U_1.V_51.Subrange.Env1;
      W_36->U_1.V_1.SubrangeyBaseTypeyTypeObj = Defs_NoObject;
      yyVisit2PrimaryType(yyt->U_1.V_51.Subrange.BaseType, &W_36->U_1.V_1.SubrangeyBaseTypeyTypeObj);
      yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Env2 = yyt->U_1.V_51.Subrange.Env2;
      yyVisit3PrimaryType(yyt->U_1.V_51.Subrange.BaseType);
      yyt->U_1.V_51.Subrange.Type2 = Defs_mSubrange2(yyt->U_1.V_51.Subrange.Type1, yyt->U_1.V_51.Subrange.BaseType->U_1.V_52.PrimaryType.Type2);
      yyt->U_1.V_51.Subrange.Objects3 = Defs_NoObjects;
      break;
    case Tree_PrimaryType:;
      yyt->U_1.V_52.PrimaryType.Type2 = yyt->U_1.V_52.PrimaryType.Type1;
      yyt->U_1.V_52.PrimaryType.Objects3 = Defs_NoObjects;
      break;
    case Tree_Void:;
      yyt->U_1.V_53.Void.Type2 = yyt->U_1.V_53.Void.Type1;
      yyt->U_1.V_53.Void.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId:;
      yyt->U_1.V_54.TypeId.Type2 = yyt->U_1.V_54.TypeId.Type1;
      yyt->U_1.V_54.TypeId.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId0:;
      yyt->U_1.V_55.TypeId0.Type2 = Defs_GroundType(yyt->U_1.V_55.TypeId0.Type1);
      yyt->U_1.V_55.TypeId0.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.Type2 = Defs_GroundType(yyt->U_1.V_56.TypeId1.Type1);
      yyt->U_1.V_56.TypeId1.Objects3 = Defs_NoObjects;
      break;
    default :
      break;
    }
  }
}

static void yyVisit4SimpleType
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_37 yyTempo;

  {
    register struct S_37 *W_37 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_SimpleType:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Enumeration:;
      W_37->U_1.V_1.EnumerationyEnumIdsyModule = yyt->U_1.V_50.Enumeration.Module;
      W_37->U_1.V_1.EnumerationyEnumIdsyKind = yyt->U_1.V_50.Enumeration.Kind;
      yyVisit2EnumIds(yyt->U_1.V_50.Enumeration.EnumIds, &W_37->U_1.V_1.EnumerationyEnumIdsyKind, &W_37->U_1.V_1.EnumerationyEnumIdsyModule, yyIdsIn, yyIdsOut);
      break;
    case Tree_Subrange:;
      W_37->U_1.V_2.SubrangeyUpbyStrsIn = Defs_NoStrings;
      W_37->U_1.V_2.SubrangeyUpbyLevel = 0;
      yyVisit1Expr(yyt->U_1.V_51.Subrange.Upb, yyEnv3, &W_37->U_1.V_2.SubrangeyUpbyLevel, &W_37->U_1.V_2.SubrangeyUpbyOpenAccessOrCall, &W_37->U_1.V_2.SubrangeyUpbyGlobalPtrs, &W_37->U_1.V_2.SubrangeyUpbyStrsIn, &W_37->U_1.V_2.SubrangeyUpbyStrsOut);
      W_37->U_1.V_2.SubrangeyLwbyStrsIn = Defs_NoStrings;
      W_37->U_1.V_2.SubrangeyLwbyLevel = 0;
      yyVisit1Expr(yyt->U_1.V_51.Subrange.Lwb, yyEnv3, &W_37->U_1.V_2.SubrangeyLwbyLevel, &W_37->U_1.V_2.SubrangeyLwbyOpenAccessOrCall, &W_37->U_1.V_2.SubrangeyLwbyGlobalPtrs, &W_37->U_1.V_2.SubrangeyLwbyStrsIn, &W_37->U_1.V_2.SubrangeyLwbyStrsOut);
      yyVisit4PrimaryType(yyt->U_1.V_51.Subrange.BaseType, yyEnv3, yyIdsIn, yyIdsOut);
      Values_CompConst((ADDRESS)yyt->U_1.V_51.Subrange.Lwb, (ADDRESS)(*yyEnv3), &yyt->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Lwb);
      Values_CompConst((ADDRESS)yyt->U_1.V_51.Subrange.Upb, (ADDRESS)(*yyEnv3), &yyt->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Upb);
      W_37->U_1.V_2.SubrangeyyType3 = Defs_mSubrange3(yyt->U_1.V_51.Subrange.Type2, yyt->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Lwb, yyt->U_1.V_51.Subrange.Type2->U_1.V_44.Subrange1.Upb);
      break;
    case Tree_PrimaryType:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Void:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Env2 = yyt->U_1.V_56.TypeId1.Env2;
      yyVisit3TypeId(yyt->U_1.V_56.TypeId1.TypeId);
      yyVisit4TypeId(yyt->U_1.V_56.TypeId1.TypeId, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1PrimaryType
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyCntOut, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyCntOut;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_38 yyTempo;

  {
    register struct S_38 *W_38 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_PrimaryType:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_52.PrimaryType.CntIn;
      break;
    case Tree_Void:;
      *yyPosOut = *yyPosIn;
      *yyCntOut = yyt->U_1.V_53.Void.CntIn;
      break;
    case Tree_TypeId:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_54.TypeId.CntIn;
      break;
    case Tree_TypeId0:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_55.TypeId0.CntIn;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.CntIn = 0;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Module = yyt->U_1.V_56.TypeId1.Module;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Kind = yyt->U_1.V_56.TypeId1.Kind;
      yyVisit1TypeId(yyt->U_1.V_56.TypeId1.TypeId, &W_38->U_1.V_1.TypeId1yTypeIdyCntOut, yyPosIn, &W_38->U_1.V_1.TypeId1yTypeIdyPosOut);
      *yyPosOut = W_38->U_1.V_1.TypeId1yTypeIdyPosOut + 1;
      *yyCntOut = yyt->U_1.V_56.TypeId1.CntIn;
      break;
    default :
      break;
    }
  }
}

static void yyVisit2PrimaryType
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObject *yyTypeObj)
# else
(yyt, yyTypeObj)
Tree_tTree yyt;
Defs_tObject *yyTypeObj;
# endif
{
  struct S_39 yyTempo;

  {
    register struct S_39 *W_39 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_PrimaryType:;
      yyt->U_1.V_52.PrimaryType.Type1 = Defs_NoType;
      break;
    case Tree_Void:;
      yyt->U_1.V_53.Void.Type1 = Defs_TypeVOID;
      break;
    case Tree_TypeId:;
      yyt->U_1.V_54.TypeId.Object = Defs_NoObject;
      yyt->U_1.V_54.TypeId.Type1 = Defs_NoType;
      break;
    case Tree_TypeId0:;
      yyt->U_1.V_55.TypeId0.Object = Defs_Identify(yyt->U_1.V_55.TypeId0.Ident, yyt->U_1.V_55.TypeId0.Env1);
      yyt->U_1.V_55.TypeId0.Type1 = Defs_mQualident1(yyt->U_1.V_55.TypeId0.Object);
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Env1 = yyt->U_1.V_56.TypeId1.Env1;
      yyVisit2TypeId(yyt->U_1.V_56.TypeId1.TypeId, yyTypeObj);
      yyt->U_1.V_56.TypeId1.Object = Defs_Identify2(yyt->U_1.V_56.TypeId1.Ident, Defs_GetExport1(yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Object));
      yyt->U_1.V_56.TypeId1.Type1 = Defs_mQualident1(yyt->U_1.V_56.TypeId1.Object);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3PrimaryType
# ifdef __STDC__
(Tree_tTree yyt)
# else
(yyt)
Tree_tTree yyt;
# endif
{
  struct S_40 yyTempo;

  {
    register struct S_40 *W_40 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_PrimaryType:;
      yyt->U_1.V_52.PrimaryType.Type2 = yyt->U_1.V_52.PrimaryType.Type1;
      yyt->U_1.V_52.PrimaryType.Objects3 = Defs_NoObjects;
      break;
    case Tree_Void:;
      yyt->U_1.V_53.Void.Type2 = yyt->U_1.V_53.Void.Type1;
      yyt->U_1.V_53.Void.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId:;
      yyt->U_1.V_54.TypeId.Type2 = yyt->U_1.V_54.TypeId.Type1;
      yyt->U_1.V_54.TypeId.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId0:;
      yyt->U_1.V_55.TypeId0.Type2 = Defs_GroundType(yyt->U_1.V_55.TypeId0.Type1);
      yyt->U_1.V_55.TypeId0.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.Type2 = Defs_GroundType(yyt->U_1.V_56.TypeId1.Type1);
      yyt->U_1.V_56.TypeId1.Objects3 = Defs_NoObjects;
      break;
    default :
      break;
    }
  }
}

static void yyVisit4PrimaryType
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_41 yyTempo;

  {
    register struct S_41 *W_41 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_PrimaryType:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Void:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Env2 = yyt->U_1.V_56.TypeId1.Env2;
      yyVisit3TypeId(yyt->U_1.V_56.TypeId1.TypeId);
      yyVisit4TypeId(yyt->U_1.V_56.TypeId1.TypeId, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1TypeId
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyCntOut, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyCntOut;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_42 yyTempo;

  {
    register struct S_42 *W_42 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_TypeId:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_54.TypeId.CntIn;
      break;
    case Tree_TypeId0:;
      *yyPosOut = *yyPosIn + 1;
      *yyCntOut = yyt->U_1.V_55.TypeId0.CntIn;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.CntIn = 0;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Module = yyt->U_1.V_56.TypeId1.Module;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Kind = yyt->U_1.V_56.TypeId1.Kind;
      yyVisit1TypeId(yyt->U_1.V_56.TypeId1.TypeId, &W_42->U_1.V_1.TypeId1yTypeIdyCntOut, yyPosIn, &W_42->U_1.V_1.TypeId1yTypeIdyPosOut);
      *yyPosOut = W_42->U_1.V_1.TypeId1yTypeIdyPosOut + 1;
      *yyCntOut = yyt->U_1.V_56.TypeId1.CntIn;
      break;
    default :
      break;
    }
  }
}

static void yyVisit2TypeId
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObject *yyTypeObj)
# else
(yyt, yyTypeObj)
Tree_tTree yyt;
Defs_tObject *yyTypeObj;
# endif
{
  struct S_43 yyTempo;

  {
    register struct S_43 *W_43 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_TypeId:;
      yyt->U_1.V_54.TypeId.Object = Defs_NoObject;
      yyt->U_1.V_54.TypeId.Type1 = Defs_NoType;
      break;
    case Tree_TypeId0:;
      yyt->U_1.V_55.TypeId0.Object = Defs_Identify(yyt->U_1.V_55.TypeId0.Ident, yyt->U_1.V_55.TypeId0.Env1);
      yyt->U_1.V_55.TypeId0.Type1 = Defs_mQualident1(yyt->U_1.V_55.TypeId0.Object);
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Env1 = yyt->U_1.V_56.TypeId1.Env1;
      yyVisit2TypeId(yyt->U_1.V_56.TypeId1.TypeId, yyTypeObj);
      yyt->U_1.V_56.TypeId1.Object = Defs_Identify2(yyt->U_1.V_56.TypeId1.Ident, Defs_GetExport1(yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Object));
      yyt->U_1.V_56.TypeId1.Type1 = Defs_mQualident1(yyt->U_1.V_56.TypeId1.Object);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3TypeId
# ifdef __STDC__
(Tree_tTree yyt)
# else
(yyt)
Tree_tTree yyt;
# endif
{
  struct S_44 yyTempo;

  {
    register struct S_44 *W_44 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_TypeId:;
      yyt->U_1.V_54.TypeId.Type2 = yyt->U_1.V_54.TypeId.Type1;
      yyt->U_1.V_54.TypeId.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId0:;
      yyt->U_1.V_55.TypeId0.Type2 = Defs_GroundType(yyt->U_1.V_55.TypeId0.Type1);
      yyt->U_1.V_55.TypeId0.Objects3 = Defs_NoObjects;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.Type2 = Defs_GroundType(yyt->U_1.V_56.TypeId1.Type1);
      yyt->U_1.V_56.TypeId1.Objects3 = Defs_NoObjects;
      break;
    default :
      break;
    }
  }
}

static void yyVisit4TypeId
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_45 yyTempo;

  {
    register struct S_45 *W_45 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_TypeId:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_TypeId1:;
      yyt->U_1.V_56.TypeId1.TypeId->U_1.V_54.TypeId.Env2 = yyt->U_1.V_56.TypeId1.Env2;
      yyVisit3TypeId(yyt->U_1.V_56.TypeId1.TypeId);
      yyVisit4TypeId(yyt->U_1.V_56.TypeId1.TypeId, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Fields
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyCntIn, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyKind, yyModule, yyCntIn, yyCntOut, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyKind;
Idents_tIdent *yyModule;
SHORTCARD *yyCntIn;
SHORTCARD *yyCntOut;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_46 yyTempo;

  {
    register struct S_46 *W_46 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Fields:;
      *yyPosOut = *yyPosIn;
      *yyCntOut = *yyCntIn;
      break;
    case Tree_Fields0:;
      *yyPosOut = *yyPosIn;
      *yyCntOut = *yyCntIn;
      break;
    case Tree_Fields1:;
      yyVisit1Fields(yyt->U_1.V_59.Fields1.Next, yyKind, yyModule, yyCntIn, yyCntOut, yyPosIn, yyPosOut);
      break;
    case Tree_RecordSect:;
      yyt->U_1.V_60.RecordSect.Type->U_1.V_43.Type.CntIn = *yyCntIn;
      yyt->U_1.V_60.RecordSect.Type->U_1.V_43.Type.Module = *yyModule;
      yyt->U_1.V_60.RecordSect.Type->U_1.V_43.Type.Kind = *yyKind;
      yyVisit1Type(yyt->U_1.V_60.RecordSect.Type, &W_46->U_1.V_1.RecordSectyNextyCntIn, yyPosIn, &W_46->U_1.V_1.RecordSectyNextyPosIn);
      yyVisit1Fields(yyt->U_1.V_60.RecordSect.Next, yyKind, yyModule, &W_46->U_1.V_1.RecordSectyNextyCntIn, yyCntOut, &W_46->U_1.V_1.RecordSectyNextyPosIn, yyPosOut);
      break;
    case Tree_VariantSect:;
      yyt->U_1.V_61.VariantSect.ElseId = GenIdents_GenSelector('V', 0L);
      yyVisit1TagField(yyt->U_1.V_61.VariantSect.TagField, yyKind, yyModule, yyPosIn, &W_46->U_1.V_2.VariantSectyTagFieldyPosOut);
      yyVisit1Variants(yyt->U_1.V_61.VariantSect.Variants, yyKind, yyModule, yyCntIn, &W_46->U_1.V_2.VariantSectyVariantsyCntOut, &W_46->U_1.V_2.VariantSectyTagFieldyPosOut, &W_46->U_1.V_2.VariantSectyVariantsyPosOut);
      yyVisit1Fields(yyt->U_1.V_61.VariantSect.Else, yyKind, yyModule, &W_46->U_1.V_2.VariantSectyVariantsyCntOut, &W_46->U_1.V_2.VariantSectyNextyCntIn, &W_46->U_1.V_2.VariantSectyVariantsyPosOut, &W_46->U_1.V_2.VariantSectyNextyPosIn);
      yyVisit1Fields(yyt->U_1.V_61.VariantSect.Next, yyKind, yyModule, &W_46->U_1.V_2.VariantSectyNextyCntIn, yyCntOut, &W_46->U_1.V_2.VariantSectyNextyPosIn, yyPosOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2Fields
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects3, Defs_tObjects *yyFieldsIn, Defs_tObjects *yyFieldsOut, Defs_tEnv *yyEnv1, Defs_tVoid *yyEnv2, SHORTCARD *yynUnion, Defs_tSelectors *yySelect)
# else
(yyt, yyObjects3, yyFieldsIn, yyFieldsOut, yyEnv1, yyEnv2, yynUnion, yySelect)
Tree_tTree yyt;
Defs_tObjects *yyObjects3;
Defs_tObjects *yyFieldsIn;
Defs_tObjects *yyFieldsOut;
Defs_tEnv *yyEnv1;
Defs_tVoid *yyEnv2;
SHORTCARD *yynUnion;
Defs_tSelectors *yySelect;
# endif
{
  struct S_47 yyTempo;

  {
    register struct S_47 *W_47 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Fields:;
      *yyFieldsOut = *yyFieldsIn;
      *yyObjects3 = Defs_NoObjects;
      break;
    case Tree_Fields0:;
      *yyFieldsOut = *yyFieldsIn;
      *yyObjects3 = Defs_NoObjects;
      break;
    case Tree_Fields1:;
      yyVisit2Fields(yyt->U_1.V_59.Fields1.Next, yyObjects3, yyFieldsIn, yyFieldsOut, yyEnv1, yyEnv2, yynUnion, yySelect);
      break;
    case Tree_RecordSect:;
      yyt->U_1.V_60.RecordSect.Type->U_1.V_43.Type.Env1 = *yyEnv1;
      W_47->U_1.V_1.RecordSectyTypeyTypeObj = Defs_NoObject;
      yyVisit2Type(yyt->U_1.V_60.RecordSect.Type, &W_47->U_1.V_1.RecordSectyTypeyTypeObj);
      yyt->U_1.V_60.RecordSect.Type->U_1.V_43.Type.Env2 = *yyEnv2;
      yyVisit3Type(yyt->U_1.V_60.RecordSect.Type);
      yyVisit2Fields(yyt->U_1.V_60.RecordSect.Next, &W_47->U_1.V_1.RecordSectyNextyObjects3, yyFieldsIn, &W_47->U_1.V_1.RecordSectyNextyFieldsOut, yyEnv1, yyEnv2, yynUnion, yySelect);
      W_47->U_1.V_1.RecordSectyFieldIdsyType = yyt->U_1.V_60.RecordSect.Type->U_1.V_43.Type.Type2;
      yyVisit1FieldIds(yyt->U_1.V_60.RecordSect.FieldIds, &W_47->U_1.V_1.RecordSectyFieldIdsyType, &W_47->U_1.V_1.RecordSectyNextyFieldsOut, yyFieldsOut, yySelect);
      *yyObjects3 = Defs_UNION(yyt->U_1.V_60.RecordSect.Type->U_1.V_43.Type.Objects3, W_47->U_1.V_1.RecordSectyNextyObjects3);
      break;
    case Tree_VariantSect:;
      yyt->U_1.V_61.VariantSect.UnionId = GenIdents_GenSelector('U', (LONGCARD)(*yynUnion + 1));
      W_47->U_1.V_2.VariantSectyVariantsySelect = Defs_mSelectors(yyt->U_1.V_61.VariantSect.UnionId, *yySelect);
      W_47->U_1.V_2.VariantSectyElseySelect = Defs_mSelectors(yyt->U_1.V_61.VariantSect.ElseId, W_47->U_1.V_2.VariantSectyVariantsySelect);
      W_47->U_1.V_2.VariantSectyElseynUnion = 0;
      yyt->U_1.V_61.VariantSect.TagField->U_1.V_65.TagField.Env1 = *yyEnv1;
      W_47->U_1.V_2.VariantSectyNextynUnion = *yynUnion + 1;
      yyVisit2Fields(yyt->U_1.V_61.VariantSect.Next, &W_47->U_1.V_2.VariantSectyNextyObjects3, yyFieldsIn, &W_47->U_1.V_2.VariantSectyNextyFieldsOut, yyEnv1, yyEnv2, &W_47->U_1.V_2.VariantSectyNextynUnion, yySelect);
      yyVisit2Fields(yyt->U_1.V_61.VariantSect.Else, &W_47->U_1.V_2.VariantSectyElseyObjects3, &W_47->U_1.V_2.VariantSectyNextyFieldsOut, &W_47->U_1.V_2.VariantSectyVariantsyFieldsIn, yyEnv1, yyEnv2, &W_47->U_1.V_2.VariantSectyElseynUnion, &W_47->U_1.V_2.VariantSectyElseySelect);
      W_47->U_1.V_2.VariantSectyVariantsynStruct = 0;
      yyVisit2Variants(yyt->U_1.V_61.VariantSect.Variants, &W_47->U_1.V_2.VariantSectyVariantsyObjects3, &W_47->U_1.V_2.VariantSectyVariantsyFieldsIn, &W_47->U_1.V_2.VariantSectyTagFieldyFieldsIn, yyEnv1, yyEnv2, &W_47->U_1.V_2.VariantSectyVariantsynStruct, &W_47->U_1.V_2.VariantSectyVariantsySelect);
      yyt->U_1.V_61.VariantSect.TagField->U_1.V_65.TagField.Env2 = *yyEnv2;
      yyVisit2TagField(yyt->U_1.V_61.VariantSect.TagField, &W_47->U_1.V_2.VariantSectyTagFieldyFieldsIn, yyFieldsOut, yySelect);
      *yyObjects3 = Defs_UNION(W_47->U_1.V_2.VariantSectyVariantsyObjects3, Defs_UNION(W_47->U_1.V_2.VariantSectyElseyObjects3, W_47->U_1.V_2.VariantSectyNextyObjects3));
      break;
    default :
      break;
    }
  }
}

static void yyVisit3Fields
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_48 yyTempo;

  {
    register struct S_48 *W_48 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Fields:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Fields0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Fields1:;
      yyVisit3Fields(yyt->U_1.V_59.Fields1.Next, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    case Tree_RecordSect:;
      yyVisit2FieldIds(yyt->U_1.V_60.RecordSect.FieldIds, yyIdsIn, &W_48->U_1.V_1.RecordSectyFieldIdsyIdsOut);
      yyVisit4Type(yyt->U_1.V_60.RecordSect.Type, yyEnv3, &W_48->U_1.V_1.RecordSectyFieldIdsyIdsOut, &W_48->U_1.V_1.RecordSectyNextyIdsIn);
      yyVisit3Fields(yyt->U_1.V_60.RecordSect.Next, yyEnv3, &W_48->U_1.V_1.RecordSectyNextyIdsIn, yyIdsOut);
      break;
    case Tree_VariantSect:;
      yyVisit3TagField(yyt->U_1.V_61.VariantSect.TagField, yyEnv3, yyIdsIn, &W_48->U_1.V_2.VariantSectyTagFieldyIdsOut);
      yyVisit3Variants(yyt->U_1.V_61.VariantSect.Variants, yyEnv3, &W_48->U_1.V_2.VariantSectyTagFieldyIdsOut, &W_48->U_1.V_2.VariantSectyVariantsyIdsOut);
      yyVisit3Fields(yyt->U_1.V_61.VariantSect.Else, yyEnv3, &W_48->U_1.V_2.VariantSectyVariantsyIdsOut, &W_48->U_1.V_2.VariantSectyNextyIdsIn);
      yyVisit3Fields(yyt->U_1.V_61.VariantSect.Next, yyEnv3, &W_48->U_1.V_2.VariantSectyNextyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1FieldIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tType *yyType, Defs_tObjects *yyFieldsIn, Defs_tObjects *yyFieldsOut, Defs_tSelectors *yySelect)
# else
(yyt, yyType, yyFieldsIn, yyFieldsOut, yySelect)
Tree_tTree yyt;
Defs_tType *yyType;
Defs_tObjects *yyFieldsIn;
Defs_tObjects *yyFieldsOut;
Defs_tSelectors *yySelect;
# endif
{
  struct S_49 yyTempo;

  {
    register struct S_49 *W_49 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_FieldIds:;
      *yyFieldsOut = *yyFieldsIn;
      break;
    case Tree_FieldIds0:;
      *yyFieldsOut = *yyFieldsIn;
      break;
    case Tree_FieldIds1:;
      yyt->U_1.V_64.FieldIds1.Object = Defs_mField1(yyt->U_1.V_64.FieldIds1.Ident, *yyType, *yySelect);
      yyVisit1FieldIds(yyt->U_1.V_64.FieldIds1.Next, yyType, yyFieldsIn, &W_49->U_1.V_1.FieldIds1yNextyFieldsOut, yySelect);
      *yyFieldsOut = Defs_mElmt(yyt->U_1.V_64.FieldIds1.Ident, FALSE, yyt->U_1.V_64.FieldIds1.Object, W_49->U_1.V_1.FieldIds1yNextyFieldsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2FieldIds
# ifdef __STDC__
(Tree_tTree yyt, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_50 yyTempo;

  {
    register struct S_50 *W_50 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_FieldIds:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_FieldIds0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_FieldIds1:;
      if (UniqueIds_NameConflict(*yyIdsIn, UniqueIds_eField, yyt->U_1.V_64.FieldIds1.Ident)) {
        yyt->U_1.V_64.FieldIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_64.FieldIds1.Object, GenIdents_RenameField(yyt->U_1.V_64.FieldIds1.Ident));
        W_50->U_1.V_1.FieldIds1yNextyIdsIn = *yyIdsIn;
      } else {
        yyt->U_1.V_64.FieldIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_64.FieldIds1.Object, yyt->U_1.V_64.FieldIds1.Ident);
        W_50->U_1.V_1.FieldIds1yNextyIdsIn = UniqueIds_DeclareIdent(*yyIdsIn, UniqueIds_eField, yyt->U_1.V_64.FieldIds1.Ident);
      }
      yyVisit2FieldIds(yyt->U_1.V_64.FieldIds1.Next, &W_50->U_1.V_1.FieldIds1yNextyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1TagField
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyKind, yyModule, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyKind;
Idents_tIdent *yyModule;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_51 yyTempo;

  {
    register struct S_51 *W_51 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_TagField:;
      yyt->U_1.V_65.TagField.Type->U_1.V_54.TypeId.CntIn = 0;
      yyt->U_1.V_65.TagField.Type->U_1.V_54.TypeId.Module = *yyModule;
      yyt->U_1.V_65.TagField.Type->U_1.V_54.TypeId.Kind = *yyKind;
      yyVisit1TypeId(yyt->U_1.V_65.TagField.Type, &W_51->U_1.V_1.TagFieldyTypeyCntOut, yyPosIn, yyPosOut);
      break;
    case Tree_TagField0:;
      yyt->U_1.V_66.TagField0.Type->U_1.V_54.TypeId.CntIn = 0;
      yyt->U_1.V_66.TagField0.Type->U_1.V_54.TypeId.Module = *yyModule;
      yyt->U_1.V_66.TagField0.Type->U_1.V_54.TypeId.Kind = *yyKind;
      yyVisit1TypeId(yyt->U_1.V_66.TagField0.Type, &W_51->U_1.V_2.TagField0yTypeyCntOut, yyPosIn, yyPosOut);
      break;
    case Tree_TagField1:;
      yyt->U_1.V_67.TagField1.Type->U_1.V_54.TypeId.CntIn = 0;
      yyt->U_1.V_67.TagField1.Type->U_1.V_54.TypeId.Module = *yyModule;
      yyt->U_1.V_67.TagField1.Type->U_1.V_54.TypeId.Kind = *yyKind;
      yyVisit1TypeId(yyt->U_1.V_67.TagField1.Type, &W_51->U_1.V_3.TagField1yTypeyCntOut, yyPosIn, yyPosOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2TagField
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyFieldsIn, Defs_tObjects *yyFieldsOut, Defs_tSelectors *yySelect)
# else
(yyt, yyFieldsIn, yyFieldsOut, yySelect)
Tree_tTree yyt;
Defs_tObjects *yyFieldsIn;
Defs_tObjects *yyFieldsOut;
Defs_tSelectors *yySelect;
# endif
{
  struct S_52 yyTempo;

  {
    register struct S_52 *W_52 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_TagField:;
      *yyFieldsOut = *yyFieldsIn;
      break;
    case Tree_TagField0:;
      *yyFieldsOut = *yyFieldsIn;
      break;
    case Tree_TagField1:;
      yyt->U_1.V_67.TagField1.Type->U_1.V_54.TypeId.Env1 = yyt->U_1.V_67.TagField1.Env1;
      W_52->U_1.V_1.TagField1yTypeyTypeObj = Defs_NoObject;
      yyVisit2TypeId(yyt->U_1.V_67.TagField1.Type, &W_52->U_1.V_1.TagField1yTypeyTypeObj);
      yyt->U_1.V_67.TagField1.Type->U_1.V_54.TypeId.Env2 = yyt->U_1.V_67.TagField1.Env2;
      yyVisit3TypeId(yyt->U_1.V_67.TagField1.Type);
      yyt->U_1.V_67.TagField1.Object = Defs_mField1(yyt->U_1.V_67.TagField1.Ident, yyt->U_1.V_67.TagField1.Type->U_1.V_54.TypeId.Type2, *yySelect);
      *yyFieldsOut = Defs_mElmt(yyt->U_1.V_67.TagField1.Ident, FALSE, yyt->U_1.V_67.TagField1.Object, *yyFieldsIn);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3TagField
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_53 yyTempo;

  {
    register struct S_53 *W_53 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_TagField:;
      yyt->U_1.V_65.TagField.Type->U_1.V_54.TypeId.Env1 = yyt->U_1.V_65.TagField.Env1;
      W_53->U_1.V_1.TagFieldyTypeyTypeObj = Defs_NoObject;
      yyVisit2TypeId(yyt->U_1.V_65.TagField.Type, &W_53->U_1.V_1.TagFieldyTypeyTypeObj);
      yyt->U_1.V_65.TagField.Type->U_1.V_54.TypeId.Env2 = yyt->U_1.V_65.TagField.Env2;
      yyVisit3TypeId(yyt->U_1.V_65.TagField.Type);
      yyVisit4TypeId(yyt->U_1.V_65.TagField.Type, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    case Tree_TagField0:;
      yyt->U_1.V_66.TagField0.Type->U_1.V_54.TypeId.Env1 = yyt->U_1.V_66.TagField0.Env1;
      W_53->U_1.V_2.TagField0yTypeyTypeObj = Defs_NoObject;
      yyVisit2TypeId(yyt->U_1.V_66.TagField0.Type, &W_53->U_1.V_2.TagField0yTypeyTypeObj);
      yyt->U_1.V_66.TagField0.Type->U_1.V_54.TypeId.Env2 = yyt->U_1.V_66.TagField0.Env2;
      yyVisit3TypeId(yyt->U_1.V_66.TagField0.Type);
      yyVisit4TypeId(yyt->U_1.V_66.TagField0.Type, yyEnv3, yyIdsIn, yyIdsOut);
      break;
    case Tree_TagField1:;
      yyVisit4TypeId(yyt->U_1.V_67.TagField1.Type, yyEnv3, yyIdsIn, &W_53->U_1.V_3.TagField1yTypeyIdsOut);
      if (UniqueIds_NameConflict(W_53->U_1.V_3.TagField1yTypeyIdsOut, UniqueIds_eField, yyt->U_1.V_67.TagField1.Ident)) {
        yyt->U_1.V_67.TagField1.CIdent = Defs_DefineCIdent(yyt->U_1.V_67.TagField1.Object, GenIdents_RenameField(yyt->U_1.V_67.TagField1.Ident));
      } else {
        yyt->U_1.V_67.TagField1.CIdent = Defs_DefineCIdent(yyt->U_1.V_67.TagField1.Object, yyt->U_1.V_67.TagField1.Ident);
      }
      if (UniqueIds_NameConflict(W_53->U_1.V_3.TagField1yTypeyIdsOut, UniqueIds_eField, yyt->U_1.V_67.TagField1.Ident)) {
        *yyIdsOut = W_53->U_1.V_3.TagField1yTypeyIdsOut;
      } else {
        *yyIdsOut = UniqueIds_DeclareIdent(W_53->U_1.V_3.TagField1yTypeyIdsOut, UniqueIds_eField, yyt->U_1.V_67.TagField1.Ident);
      }
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Variants
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyCntIn, SHORTCARD *yyCntOut, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyKind, yyModule, yyCntIn, yyCntOut, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyKind;
Idents_tIdent *yyModule;
SHORTCARD *yyCntIn;
SHORTCARD *yyCntOut;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_54 yyTempo;

  {
    register struct S_54 *W_54 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Variants:;
      *yyPosOut = *yyPosIn;
      *yyCntOut = *yyCntIn;
      break;
    case Tree_Variants0:;
      *yyPosOut = *yyPosIn;
      *yyCntOut = *yyCntIn;
      break;
    case Tree_Variant:;
      yyVisit1Fields(yyt->U_1.V_70.Variant.Variant, yyKind, yyModule, yyCntIn, &W_54->U_1.V_1.VariantyVariantyCntOut, yyPosIn, &W_54->U_1.V_1.VariantyVariantyPosOut);
      yyVisit1Variants(yyt->U_1.V_70.Variant.Next, yyKind, yyModule, &W_54->U_1.V_1.VariantyVariantyCntOut, yyCntOut, &W_54->U_1.V_1.VariantyVariantyPosOut, yyPosOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2Variants
# ifdef __STDC__
(Tree_tTree yyt, Defs_tObjects *yyObjects3, Defs_tObjects *yyFieldsIn, Defs_tObjects *yyFieldsOut, Defs_tEnv *yyEnv1, Defs_tVoid *yyEnv2, SHORTCARD *yynStruct, Defs_tSelectors *yySelect)
# else
(yyt, yyObjects3, yyFieldsIn, yyFieldsOut, yyEnv1, yyEnv2, yynStruct, yySelect)
Tree_tTree yyt;
Defs_tObjects *yyObjects3;
Defs_tObjects *yyFieldsIn;
Defs_tObjects *yyFieldsOut;
Defs_tEnv *yyEnv1;
Defs_tVoid *yyEnv2;
SHORTCARD *yynStruct;
Defs_tSelectors *yySelect;
# endif
{
  struct S_55 yyTempo;

  {
    register struct S_55 *W_55 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Variants:;
      *yyFieldsOut = *yyFieldsIn;
      *yyObjects3 = Defs_NoObjects;
      break;
    case Tree_Variants0:;
      *yyFieldsOut = *yyFieldsIn;
      *yyObjects3 = Defs_NoObjects;
      break;
    case Tree_Variant:;
      yyt->U_1.V_70.Variant.StructId = GenIdents_GenSelector('V', (LONGCARD)(*yynStruct + 1));
      W_55->U_1.V_1.VariantyNextynStruct = *yynStruct + 1;
      yyVisit2Variants(yyt->U_1.V_70.Variant.Next, &W_55->U_1.V_1.VariantyNextyObjects3, yyFieldsIn, &W_55->U_1.V_1.VariantyVariantyFieldsIn, yyEnv1, yyEnv2, &W_55->U_1.V_1.VariantyNextynStruct, yySelect);
      W_55->U_1.V_1.VariantyVariantySelect = Defs_mSelectors(yyt->U_1.V_70.Variant.StructId, *yySelect);
      W_55->U_1.V_1.VariantyVariantynUnion = 0;
      yyVisit2Fields(yyt->U_1.V_70.Variant.Variant, &W_55->U_1.V_1.VariantyVariantyObjects3, &W_55->U_1.V_1.VariantyVariantyFieldsIn, yyFieldsOut, yyEnv1, yyEnv2, &W_55->U_1.V_1.VariantyVariantynUnion, &W_55->U_1.V_1.VariantyVariantySelect);
      *yyObjects3 = Defs_UNION(W_55->U_1.V_1.VariantyVariantyObjects3, W_55->U_1.V_1.VariantyNextyObjects3);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3Variants
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_56 yyTempo;

  {
    register struct S_56 *W_56 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Variants:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Variants0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_Variant:;
      yyVisit1Labels(yyt->U_1.V_70.Variant.Labels, yyEnv3);
      yyVisit3Fields(yyt->U_1.V_70.Variant.Variant, yyEnv3, yyIdsIn, &W_56->U_1.V_1.VariantyVariantyIdsOut);
      yyVisit3Variants(yyt->U_1.V_70.Variant.Next, yyEnv3, &W_56->U_1.V_1.VariantyVariantyIdsOut, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1FormalTypes
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, SHORTCARD *yyPosIn, SHORTCARD *yyPosOut)
# else
(yyt, yyKind, yyModule, yyPosIn, yyPosOut)
Tree_tTree yyt;
SHORTCARD *yyKind;
Idents_tIdent *yyModule;
SHORTCARD *yyPosIn;
SHORTCARD *yyPosOut;
# endif
{
  struct S_57 yyTempo;

  {
    register struct S_57 *W_57 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_FormalTypes:;
      *yyPosOut = *yyPosIn;
      break;
    case Tree_FormalTypes0:;
      *yyPosOut = *yyPosIn;
      break;
    case Tree_FormalType:;
      yyt->U_1.V_73.FormalType.Type->U_1.V_43.Type.CntIn = 0;
      yyt->U_1.V_73.FormalType.Type->U_1.V_43.Type.Module = *yyModule;
      yyt->U_1.V_73.FormalType.Type->U_1.V_43.Type.Kind = *yyKind;
      yyVisit1Type(yyt->U_1.V_73.FormalType.Type, &W_57->U_1.V_1.FormalTypeyTypeyCntOut, yyPosIn, &W_57->U_1.V_1.FormalTypeyTypeyPosOut);
      yyVisit1FormalTypes(yyt->U_1.V_73.FormalType.Next, yyKind, yyModule, &W_57->U_1.V_1.FormalTypeyTypeyPosOut, yyPosOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2FormalTypes
# ifdef __STDC__
(Tree_tTree yyt, Defs_tTypes *yyTypes, Defs_tEnv *yyEnv1, Defs_tVoid *yyEnv2)
# else
(yyt, yyTypes, yyEnv1, yyEnv2)
Tree_tTree yyt;
Defs_tTypes *yyTypes;
Defs_tEnv *yyEnv1;
Defs_tVoid *yyEnv2;
# endif
{
  struct S_58 yyTempo;

  {
    register struct S_58 *W_58 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_FormalTypes:;
      *yyTypes = Defs_NoTypes;
      break;
    case Tree_FormalTypes0:;
      *yyTypes = Defs_NoTypes;
      break;
    case Tree_FormalType:;
      yyt->U_1.V_73.FormalType.Type->U_1.V_43.Type.Env1 = *yyEnv1;
      W_58->U_1.V_1.FormalTypeyTypeyTypeObj = Defs_NoObject;
      yyVisit2Type(yyt->U_1.V_73.FormalType.Type, &W_58->U_1.V_1.FormalTypeyTypeyTypeObj);
      yyt->U_1.V_73.FormalType.Type->U_1.V_43.Type.Env2 = *yyEnv2;
      yyVisit3Type(yyt->U_1.V_73.FormalType.Type);
      yyVisit2FormalTypes(yyt->U_1.V_73.FormalType.Next, &W_58->U_1.V_1.FormalTypeyNextyTypes, yyEnv1, yyEnv2);
      *yyTypes = Defs_mTypes(yyt->U_1.V_73.FormalType.IsVAR, yyt->U_1.V_73.FormalType.Type->U_1.V_43.Type.Type2, W_58->U_1.V_1.FormalTypeyNextyTypes);
      break;
    default :
      break;
    }
  }
}

static void yyVisit3FormalTypes
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv3, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyEnv3, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv3;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_59 yyTempo;

  {
    register struct S_59 *W_59 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_FormalTypes:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_FormalTypes0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_FormalType:;
      yyVisit4Type(yyt->U_1.V_73.FormalType.Type, yyEnv3, yyIdsIn, &W_59->U_1.V_1.FormalTypeyTypeyIdsOut);
      yyVisit3FormalTypes(yyt->U_1.V_73.FormalType.Next, yyEnv3, &W_59->U_1.V_1.FormalTypeyTypeyIdsOut, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1EnumIds
# ifdef __STDC__
(Tree_tTree yyt, Defs_tType *yyType, SHORTCARD *yyIndexIn, SHORTCARD *yyIndexOut)
# else
(yyt, yyType, yyIndexIn, yyIndexOut)
Tree_tTree yyt;
Defs_tType *yyType;
SHORTCARD *yyIndexIn;
SHORTCARD *yyIndexOut;
# endif
{
  struct S_60 yyTempo;

  {
    register struct S_60 *W_60 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_EnumIds:;
      *yyIndexOut = *yyIndexIn;
      yyt->U_1.V_74.EnumIds.Objects = Defs_NoObjects;
      break;
    case Tree_EnumIds0:;
      *yyIndexOut = *yyIndexIn;
      yyt->U_1.V_75.EnumIds0.Objects = Defs_NoObjects;
      break;
    case Tree_EnumIds1:;
      yyt->U_1.V_76.EnumIds1.Object = Defs_mEnumLiteral1(yyt->U_1.V_76.EnumIds1.Ident, *yyType, *yyIndexIn);
      W_60->U_1.V_1.EnumIds1yNextyIndexIn = *yyIndexIn + 1;
      yyVisit1EnumIds(yyt->U_1.V_76.EnumIds1.Next, yyType, &W_60->U_1.V_1.EnumIds1yNextyIndexIn, yyIndexOut);
      yyt->U_1.V_76.EnumIds1.Objects = Defs_mElmt(yyt->U_1.V_76.EnumIds1.Ident, FALSE, yyt->U_1.V_76.EnumIds1.Object, yyt->U_1.V_76.EnumIds1.Next->U_1.V_74.EnumIds.Objects);
      break;
    default :
      break;
    }
  }
}

static void yyVisit2EnumIds
# ifdef __STDC__
(Tree_tTree yyt, SHORTCARD *yyKind, Idents_tIdent *yyModule, UniqueIds_tIdents *yyIdsIn, UniqueIds_tIdents *yyIdsOut)
# else
(yyt, yyKind, yyModule, yyIdsIn, yyIdsOut)
Tree_tTree yyt;
SHORTCARD *yyKind;
Idents_tIdent *yyModule;
UniqueIds_tIdents *yyIdsIn;
UniqueIds_tIdents *yyIdsOut;
# endif
{
  struct S_61 yyTempo;

  {
    register struct S_61 *W_61 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_EnumIds:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_EnumIds0:;
      *yyIdsOut = *yyIdsIn;
      break;
    case Tree_EnumIds1:;
      if (IN(*yyKind, SET_ELEM(Tree_Definition) | SET_ELEM(Tree_Foreign))) {
        yyt->U_1.V_76.EnumIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_76.EnumIds1.Object, GenIdents_MakeQualified(*yyModule, yyt->U_1.V_76.EnumIds1.Ident));
        W_61->U_1.V_1.EnumIds1yNextyIdsIn = *yyIdsIn;
      } else if (UniqueIds_NameConflict(*yyIdsIn, UniqueIds_eConst, yyt->U_1.V_76.EnumIds1.Ident)) {
        yyt->U_1.V_76.EnumIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_76.EnumIds1.Object, GenIdents_Rename(yyt->U_1.V_76.EnumIds1.Ident));
        W_61->U_1.V_1.EnumIds1yNextyIdsIn = *yyIdsIn;
      } else {
        yyt->U_1.V_76.EnumIds1.CIdent = Defs_DefineCIdent(yyt->U_1.V_76.EnumIds1.Object, yyt->U_1.V_76.EnumIds1.Ident);
        W_61->U_1.V_1.EnumIds1yNextyIdsIn = UniqueIds_DeclareIdent(*yyIdsIn, UniqueIds_eConst, yyt->U_1.V_76.EnumIds1.Ident);
      }
      yyVisit2EnumIds(yyt->U_1.V_76.EnumIds1.Next, yyKind, yyModule, &W_61->U_1.V_1.EnumIds1yNextyIdsIn, yyIdsOut);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Expr
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyOpenAccessOrCall, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut)
# else
(yyt, yyEnv, yyLevel, yyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
SHORTCARD *yyLevel;
BOOLEAN *yyOpenAccessOrCall;
BOOLEAN *yyGlobalPtrs;
Defs_tStrings *yyStrsIn;
Defs_tStrings *yyStrsOut;
# endif
{
  struct S_62 yyTempo;

  {
    register struct S_62 *W_62 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Expr:;
      yyt->U_1.V_77.Expr.Type = Defs_NoType;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      yyt->U_1.V_77.Expr.IsCConst = TRUE;
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_Binary:;
      yyVisit1Expr(yyt->U_1.V_78.Binary.Lop, yyEnv, yyLevel, &W_62->U_1.V_1.BinaryyLopyOpenAccessOrCall, &W_62->U_1.V_1.BinaryyLopyGlobalPtrs, yyStrsIn, &W_62->U_1.V_1.BinaryyLopyStrsOut);
      yyVisit1Expr(yyt->U_1.V_78.Binary.Rop, yyEnv, yyLevel, &W_62->U_1.V_1.BinaryyRopyOpenAccessOrCall, &W_62->U_1.V_1.BinaryyRopyGlobalPtrs, &W_62->U_1.V_1.BinaryyLopyStrsOut, yyStrsOut);
      yyt->U_1.V_78.Binary.Type = Types_ResultType(yyt->U_1.V_78.Binary.Operator, yyt->U_1.V_78.Binary.Lop->U_1.V_77.Expr.Type, yyt->U_1.V_78.Binary.Rop->U_1.V_77.Expr.Type);
      if ((yyt->U_1.V_78.Binary.Lop->U_1.V_77.Expr.Type == Defs_TypeBITSET || yyt->U_1.V_78.Binary.Lop->U_1.V_77.Expr.Type->U_1.V_1.Kind == Defs_Set1) && (yyt->U_1.V_78.Binary.Rop->U_1.V_77.Expr.Type == Defs_TypeBITSET || yyt->U_1.V_78.Binary.Rop->U_1.V_77.Expr.Type->U_1.V_1.Kind == Defs_Set1)) {
        switch (yyt->U_1.V_78.Binary.Operator) {
        case Tree_Times:;
          yyt->U_1.V_78.Binary.COperator = Code_cIntersection;
          break;
        case Tree_Plus:;
          yyt->U_1.V_78.Binary.COperator = Code_cUnion;
          break;
        case Tree_Minus:;
          yyt->U_1.V_78.Binary.COperator = Code_cDifference;
          break;
        case Tree_Divide:;
          yyt->U_1.V_78.Binary.COperator = Code_cSymDiff;
          break;
        case Tree_LessEqual:;
          yyt->U_1.V_78.Binary.COperator = Code_cIsSubset1;
          break;
        case Tree_GreaterEqual:;
          yyt->U_1.V_78.Binary.COperator = Code_cIsSubset2;
          break;
        case Tree_NotEqual:;
          yyt->U_1.V_78.Binary.COperator = Code_cNotEqual;
          break;
        case Tree_Equal:;
          yyt->U_1.V_78.Binary.COperator = Code_cEqual;
          break;
        default :
          yyt->U_1.V_78.Binary.COperator = Code_cNoOp;
          break;
        }
      } else {
        switch (yyt->U_1.V_78.Binary.Operator) {
        case Tree_NotEqual:;
          yyt->U_1.V_78.Binary.COperator = Code_cNotEqual;
          break;
        case Tree_Less:;
          yyt->U_1.V_78.Binary.COperator = Code_cLess;
          break;
        case Tree_Equal:;
          yyt->U_1.V_78.Binary.COperator = Code_cEqual;
          break;
        case Tree_Greater:;
          yyt->U_1.V_78.Binary.COperator = Code_cGreater;
          break;
        case Tree_And:;
          yyt->U_1.V_78.Binary.COperator = Code_cAnd;
          break;
        case Tree_Div:;
          yyt->U_1.V_78.Binary.COperator = Code_cDivide;
          break;
        case Tree_In:;
          yyt->U_1.V_78.Binary.COperator = Code_cIn;
          break;
        case Tree_Mod:;
          yyt->U_1.V_78.Binary.COperator = Code_cMod;
          break;
        case Tree_Or:;
          yyt->U_1.V_78.Binary.COperator = Code_cOr;
          break;
        case Tree_Times:;
          yyt->U_1.V_78.Binary.COperator = Code_cTimes;
          break;
        case Tree_Plus:;
          yyt->U_1.V_78.Binary.COperator = Code_cPlus;
          break;
        case Tree_Minus:;
          yyt->U_1.V_78.Binary.COperator = Code_cMinus;
          break;
        case Tree_Divide:;
          yyt->U_1.V_78.Binary.COperator = Code_cDivide;
          break;
        case Tree_LessEqual:;
          yyt->U_1.V_78.Binary.COperator = Code_cLessEqual;
          break;
        case Tree_GreaterEqual:;
          yyt->U_1.V_78.Binary.COperator = Code_cGreaterEqual;
          break;
        }
      }
      *yyGlobalPtrs = W_62->U_1.V_1.BinaryyLopyGlobalPtrs || W_62->U_1.V_1.BinaryyRopyGlobalPtrs;
      if (IN(yyt->U_1.V_78.Binary.COperator, SET_ELEM(Code_cAnd) | SET_ELEM(Code_cOr) | SET_ELEM(Code_cIsSubset1) | SET_ELEM(Code_cIsSubset2))) {
        yyt->U_1.V_78.Binary.IsCConst = FALSE;
      } else {
        yyt->U_1.V_78.Binary.IsCConst = yyt->U_1.V_78.Binary.Lop->U_1.V_77.Expr.IsCConst && yyt->U_1.V_78.Binary.Rop->U_1.V_77.Expr.IsCConst;
      }
      *yyOpenAccessOrCall = W_62->U_1.V_1.BinaryyLopyOpenAccessOrCall || W_62->U_1.V_1.BinaryyRopyOpenAccessOrCall;
      break;
    case Tree_Unary:;
      yyVisit1Expr(yyt->U_1.V_79.Unary.Mop, yyEnv, yyLevel, yyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_79.Unary.Type = Types_ResultType(yyt->U_1.V_79.Unary.Operator, yyt->U_1.V_79.Unary.Mop->U_1.V_77.Expr.Type, yyt->U_1.V_79.Unary.Mop->U_1.V_77.Expr.Type);
      switch (yyt->U_1.V_79.Unary.Operator) {
      case Tree_Minus:;
        yyt->U_1.V_79.Unary.COperator = Code_cMinus;
        break;
      case Tree_Not:;
        yyt->U_1.V_79.Unary.COperator = Code_cNot;
        break;
      }
      if (yyt->U_1.V_79.Unary.COperator == Code_cNot) {
        yyt->U_1.V_79.Unary.IsCConst = FALSE;
      } else {
        yyt->U_1.V_79.Unary.IsCConst = yyt->U_1.V_79.Unary.Mop->U_1.V_77.Expr.IsCConst;
      }
      break;
    case Tree_IntConst:;
      yyt->U_1.V_80.IntConst.Type = Defs_TypeIntCard;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      yyt->U_1.V_80.IntConst.IsCConst = TRUE;
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_RealConst:;
      yyt->U_1.V_81.RealConst.Type = Defs_TypeREAL;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      yyt->U_1.V_81.RealConst.IsCConst = TRUE;
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_StringConst:;
      if (StringMem_Length(yyt->U_1.V_82.StringConst.StringVal) == 1) {
        yyt->U_1.V_82.StringConst.Type = Defs_TypeStringChar;
      } else {
        yyt->U_1.V_82.StringConst.Type = Defs_TypeSTRING;
      }
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      yyt->U_1.V_82.StringConst.IsCConst = TRUE;
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_CharConst:;
      yyt->U_1.V_83.CharConst.Type = Defs_TypeCHAR;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      yyt->U_1.V_83.CharConst.IsCConst = TRUE;
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_FuncCall:;
      yyVisit1Designator(yyt->U_1.V_84.FuncCall.Designator, yyEnv, yyLevel, &W_62->U_1.V_2.FuncCallyDesignatoryOpenAccessOrCall, &W_62->U_1.V_2.FuncCallyDesignatoryGlobalPtrs, yyStrsIn, &W_62->U_1.V_2.FuncCallyDesignatoryStrsOut);
      W_62->U_1.V_2.FuncCallyActualsyFormals = Defs_GetFormals(yyt->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type);
      yyVisit1Actuals(yyt->U_1.V_84.FuncCall.Actuals, yyEnv, &W_62->U_1.V_2.FuncCallyActualsyTypes, &W_62->U_1.V_2.FuncCallyActualsyFormals, yyLevel, &W_62->U_1.V_2.FuncCallyActualsyIsCConst, &W_62->U_1.V_2.FuncCallyActualsyGlobalPtrs, &W_62->U_1.V_2.FuncCallyDesignatoryStrsOut, yyStrsOut);
      if (yyt->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_StdProcType1) {
        yyt->U_1.V_84.FuncCall.Type = Types_StdResultType(yyt->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type, W_62->U_1.V_2.FuncCallyActualsyTypes);
      } else if (yyt->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_ProcType1) {
        yyt->U_1.V_84.FuncCall.Type = Defs_GetResultType(yyt->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type);
      } else {
        yyt->U_1.V_84.FuncCall.Type = yyt->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type;
      }
      *yyGlobalPtrs = W_62->U_1.V_2.FuncCallyDesignatoryGlobalPtrs || W_62->U_1.V_2.FuncCallyActualsyGlobalPtrs;
      if (yyt->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_StdProcType1 && IN(yyt->U_1.V_84.FuncCall.Designator->U_1.V_87.Designator.Type->U_1.V_34.StdProcType1.StdProc, SET_ELEM(Defs_ProcCHR) | SET_ELEM(Defs_ProcFLOAT) | SET_ELEM(Defs_ProcMAX) | SET_ELEM(Defs_ProcMIN) | SET_ELEM(Defs_ProcODD) | SET_ELEM(Defs_ProcORD) | SET_ELEM(Defs_ProcSIZE) | SET_ELEM(Defs_ProcTRUNC) | SET_ELEM(Defs_ProcVAL) | SET_ELEM(Defs_ProcTSIZE))) {
        yyt->U_1.V_84.FuncCall.IsCConst = W_62->U_1.V_2.FuncCallyActualsyIsCConst;
      } else {
        yyt->U_1.V_84.FuncCall.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = !yyt->U_1.V_84.FuncCall.IsCConst;
      break;
    case Tree_Set:;
      yyVisit1Qualid(yyt->U_1.V_85.Set.BaseType, yyEnv, yyLevel, &W_62->U_1.V_3.SetyBaseTypeyOpenAccessOrCall, &W_62->U_1.V_3.SetyBaseTypeyGlobalPtrs, yyStrsIn, &W_62->U_1.V_3.SetyBaseTypeyStrsOut);
      yyt->U_1.V_85.Set.Type = yyt->U_1.V_85.Set.BaseType->U_1.V_88.Qualid.Type;
      yyVisit1Elems(yyt->U_1.V_85.Set.Elems, yyEnv, yyLevel, &W_62->U_1.V_3.SetyElemsyIsCConst, yyOpenAccessOrCall, yyGlobalPtrs, &W_62->U_1.V_3.SetyBaseTypeyStrsOut, yyStrsOut);
      yyt->U_1.V_85.Set.IsCConst = W_62->U_1.V_3.SetyElemsyIsCConst;
      break;
    case Tree_BitSet:;
      yyt->U_1.V_86.BitSet.Type = Defs_TypeBITSET;
      yyVisit1Elems(yyt->U_1.V_86.BitSet.Elems, yyEnv, yyLevel, &W_62->U_1.V_4.BitSetyElemsyIsCConst, yyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_86.BitSet.IsCConst = W_62->U_1.V_4.BitSetyElemsyIsCConst;
      break;
    case Tree_Designator:;
      yyt->U_1.V_87.Designator.Type = Defs_NoType;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      yyt->U_1.V_87.Designator.IsCConst = FALSE;
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_Qualid:;
      yyt->U_1.V_88.Qualid.Object = Defs_NoObject;
      yyt->U_1.V_88.Qualid.Type = Defs_NoType;
      yyt->U_1.V_88.Qualid.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_88.Qualid.Object, *yyLevel);
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      if (yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_88.Qualid.IsCConst = TRUE;
      } else {
        yyt->U_1.V_88.Qualid.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_Qualid0:;
      yyt->U_1.V_89.Qualid0.Object = Defs_Identify(yyt->U_1.V_89.Qualid0.Ident, *yyEnv);
      yyt->U_1.V_89.Qualid0.Type = Defs_GetType(yyt->U_1.V_89.Qualid0.Object);
      yyt->U_1.V_89.Qualid0.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_89.Qualid0.Object, *yyLevel);
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = yyt->U_1.V_89.Qualid0.IsGlobalPtr;
      if (yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_89.Qualid0.IsCConst = TRUE;
      } else {
        yyt->U_1.V_89.Qualid0.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = Defs_IsOpenArray(yyt->U_1.V_89.Qualid0.Object);
      break;
    case Tree_Qualid1:;
      yyVisit1Qualid(yyt->U_1.V_90.Qualid1.Qualid, yyEnv, yyLevel, &W_62->U_1.V_5.Qualid1yQualidyOpenAccessOrCall, &W_62->U_1.V_5.Qualid1yQualidyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_90.Qualid1.Object = Defs_Identify2(yyt->U_1.V_90.Qualid1.Ident, Defs_GetObjects(yyt->U_1.V_90.Qualid1.Qualid->U_1.V_88.Qualid.Object));
      yyt->U_1.V_90.Qualid1.Type = Defs_GetType(yyt->U_1.V_90.Qualid1.Object);
      yyt->U_1.V_90.Qualid1.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_90.Qualid1.Object, *yyLevel);
      *yyGlobalPtrs = W_62->U_1.V_5.Qualid1yQualidyGlobalPtrs || yyt->U_1.V_90.Qualid1.IsGlobalPtr;
      if (yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_90.Qualid1.IsCConst = TRUE;
      } else {
        yyt->U_1.V_90.Qualid1.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_Subscript:;
      yyVisit1Designator(yyt->U_1.V_91.Subscript.Designator, yyEnv, yyLevel, yyOpenAccessOrCall, &W_62->U_1.V_6.SubscriptyDesignatoryGlobalPtrs, yyStrsIn, &W_62->U_1.V_6.SubscriptyDesignatoryStrsOut);
      yyt->U_1.V_91.Subscript.Type = Defs_GetElemType(yyt->U_1.V_91.Subscript.Designator->U_1.V_87.Designator.Type);
      yyVisit1Expr(yyt->U_1.V_91.Subscript.Index, yyEnv, yyLevel, &W_62->U_1.V_6.SubscriptyIndexyOpenAccessOrCall, &W_62->U_1.V_6.SubscriptyIndexyGlobalPtrs, &W_62->U_1.V_6.SubscriptyDesignatoryStrsOut, yyStrsOut);
      *yyGlobalPtrs = W_62->U_1.V_6.SubscriptyDesignatoryGlobalPtrs || W_62->U_1.V_6.SubscriptyIndexyGlobalPtrs;
      yyt->U_1.V_91.Subscript.IsCConst = FALSE;
      break;
    case Tree_Deref:;
      yyVisit1Designator(yyt->U_1.V_92.Deref.Designator, yyEnv, yyLevel, &W_62->U_1.V_7.DerefyDesignatoryOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_92.Deref.Type = Defs_GetTargetType(yyt->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type);
      yyt->U_1.V_92.Deref.IsCConst = FALSE;
      *yyOpenAccessOrCall = TRUE;
      break;
    case Tree_Select:;
      yyVisit1Designator(yyt->U_1.V_93.Select.Designator, yyEnv, yyLevel, yyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_93.Select.Object = Defs_Identify2(yyt->U_1.V_93.Select.Field, Defs_GetFields(yyt->U_1.V_93.Select.Designator->U_1.V_87.Designator.Type));
      yyt->U_1.V_93.Select.Type = Defs_GetType(yyt->U_1.V_93.Select.Object);
      yyt->U_1.V_93.Select.IsCConst = FALSE;
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Designator
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyOpenAccessOrCall, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut)
# else
(yyt, yyEnv, yyLevel, yyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
SHORTCARD *yyLevel;
BOOLEAN *yyOpenAccessOrCall;
BOOLEAN *yyGlobalPtrs;
Defs_tStrings *yyStrsIn;
Defs_tStrings *yyStrsOut;
# endif
{
  struct S_63 yyTempo;

  {
    register struct S_63 *W_63 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Designator:;
      yyt->U_1.V_87.Designator.Type = Defs_NoType;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      yyt->U_1.V_87.Designator.IsCConst = FALSE;
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_Qualid:;
      yyt->U_1.V_88.Qualid.Object = Defs_NoObject;
      yyt->U_1.V_88.Qualid.Type = Defs_NoType;
      yyt->U_1.V_88.Qualid.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_88.Qualid.Object, *yyLevel);
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      if (yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_88.Qualid.IsCConst = TRUE;
      } else {
        yyt->U_1.V_88.Qualid.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_Qualid0:;
      yyt->U_1.V_89.Qualid0.Object = Defs_Identify(yyt->U_1.V_89.Qualid0.Ident, *yyEnv);
      yyt->U_1.V_89.Qualid0.Type = Defs_GetType(yyt->U_1.V_89.Qualid0.Object);
      yyt->U_1.V_89.Qualid0.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_89.Qualid0.Object, *yyLevel);
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = yyt->U_1.V_89.Qualid0.IsGlobalPtr;
      if (yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_89.Qualid0.IsCConst = TRUE;
      } else {
        yyt->U_1.V_89.Qualid0.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = Defs_IsOpenArray(yyt->U_1.V_89.Qualid0.Object);
      break;
    case Tree_Qualid1:;
      yyVisit1Qualid(yyt->U_1.V_90.Qualid1.Qualid, yyEnv, yyLevel, &W_63->U_1.V_1.Qualid1yQualidyOpenAccessOrCall, &W_63->U_1.V_1.Qualid1yQualidyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_90.Qualid1.Object = Defs_Identify2(yyt->U_1.V_90.Qualid1.Ident, Defs_GetObjects(yyt->U_1.V_90.Qualid1.Qualid->U_1.V_88.Qualid.Object));
      yyt->U_1.V_90.Qualid1.Type = Defs_GetType(yyt->U_1.V_90.Qualid1.Object);
      yyt->U_1.V_90.Qualid1.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_90.Qualid1.Object, *yyLevel);
      *yyGlobalPtrs = W_63->U_1.V_1.Qualid1yQualidyGlobalPtrs || yyt->U_1.V_90.Qualid1.IsGlobalPtr;
      if (yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_90.Qualid1.IsCConst = TRUE;
      } else {
        yyt->U_1.V_90.Qualid1.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_Subscript:;
      yyVisit1Designator(yyt->U_1.V_91.Subscript.Designator, yyEnv, yyLevel, yyOpenAccessOrCall, &W_63->U_1.V_2.SubscriptyDesignatoryGlobalPtrs, yyStrsIn, &W_63->U_1.V_2.SubscriptyDesignatoryStrsOut);
      yyt->U_1.V_91.Subscript.Type = Defs_GetElemType(yyt->U_1.V_91.Subscript.Designator->U_1.V_87.Designator.Type);
      yyVisit1Expr(yyt->U_1.V_91.Subscript.Index, yyEnv, yyLevel, &W_63->U_1.V_2.SubscriptyIndexyOpenAccessOrCall, &W_63->U_1.V_2.SubscriptyIndexyGlobalPtrs, &W_63->U_1.V_2.SubscriptyDesignatoryStrsOut, yyStrsOut);
      *yyGlobalPtrs = W_63->U_1.V_2.SubscriptyDesignatoryGlobalPtrs || W_63->U_1.V_2.SubscriptyIndexyGlobalPtrs;
      yyt->U_1.V_91.Subscript.IsCConst = FALSE;
      break;
    case Tree_Deref:;
      yyVisit1Designator(yyt->U_1.V_92.Deref.Designator, yyEnv, yyLevel, &W_63->U_1.V_3.DerefyDesignatoryOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_92.Deref.Type = Defs_GetTargetType(yyt->U_1.V_92.Deref.Designator->U_1.V_87.Designator.Type);
      yyt->U_1.V_92.Deref.IsCConst = FALSE;
      *yyOpenAccessOrCall = TRUE;
      break;
    case Tree_Select:;
      yyVisit1Designator(yyt->U_1.V_93.Select.Designator, yyEnv, yyLevel, yyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_93.Select.Object = Defs_Identify2(yyt->U_1.V_93.Select.Field, Defs_GetFields(yyt->U_1.V_93.Select.Designator->U_1.V_87.Designator.Type));
      yyt->U_1.V_93.Select.Type = Defs_GetType(yyt->U_1.V_93.Select.Object);
      yyt->U_1.V_93.Select.IsCConst = FALSE;
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Qualid
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyOpenAccessOrCall, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut)
# else
(yyt, yyEnv, yyLevel, yyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
SHORTCARD *yyLevel;
BOOLEAN *yyOpenAccessOrCall;
BOOLEAN *yyGlobalPtrs;
Defs_tStrings *yyStrsIn;
Defs_tStrings *yyStrsOut;
# endif
{
  struct S_64 yyTempo;

  {
    register struct S_64 *W_64 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Qualid:;
      yyt->U_1.V_88.Qualid.Object = Defs_NoObject;
      yyt->U_1.V_88.Qualid.Type = Defs_NoType;
      yyt->U_1.V_88.Qualid.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_88.Qualid.Object, *yyLevel);
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      if (yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_88.Qualid.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_88.Qualid.IsCConst = TRUE;
      } else {
        yyt->U_1.V_88.Qualid.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = FALSE;
      break;
    case Tree_Qualid0:;
      yyt->U_1.V_89.Qualid0.Object = Defs_Identify(yyt->U_1.V_89.Qualid0.Ident, *yyEnv);
      yyt->U_1.V_89.Qualid0.Type = Defs_GetType(yyt->U_1.V_89.Qualid0.Object);
      yyt->U_1.V_89.Qualid0.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_89.Qualid0.Object, *yyLevel);
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = yyt->U_1.V_89.Qualid0.IsGlobalPtr;
      if (yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_89.Qualid0.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_89.Qualid0.IsCConst = TRUE;
      } else {
        yyt->U_1.V_89.Qualid0.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = Defs_IsOpenArray(yyt->U_1.V_89.Qualid0.Object);
      break;
    case Tree_Qualid1:;
      yyVisit1Qualid(yyt->U_1.V_90.Qualid1.Qualid, yyEnv, yyLevel, &W_64->U_1.V_1.Qualid1yQualidyOpenAccessOrCall, &W_64->U_1.V_1.Qualid1yQualidyGlobalPtrs, yyStrsIn, yyStrsOut);
      yyt->U_1.V_90.Qualid1.Object = Defs_Identify2(yyt->U_1.V_90.Qualid1.Ident, Defs_GetObjects(yyt->U_1.V_90.Qualid1.Qualid->U_1.V_88.Qualid.Object));
      yyt->U_1.V_90.Qualid1.Type = Defs_GetType(yyt->U_1.V_90.Qualid1.Object);
      yyt->U_1.V_90.Qualid1.IsGlobalPtr = Defs_NestedUse(yyt->U_1.V_90.Qualid1.Object, *yyLevel);
      *yyGlobalPtrs = W_64->U_1.V_1.Qualid1yQualidyGlobalPtrs || yyt->U_1.V_90.Qualid1.IsGlobalPtr;
      if (yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_Const1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_EnumLiteral1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_TypeDecl1 || yyt->U_1.V_90.Qualid1.Object->U_1.V_1.Kind == Defs_Opaque1) {
        yyt->U_1.V_90.Qualid1.IsCConst = TRUE;
      } else {
        yyt->U_1.V_90.Qualid1.IsCConst = FALSE;
      }
      *yyOpenAccessOrCall = FALSE;
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Elems
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyIsCConst, BOOLEAN *yyOpenAccessOrCall, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut)
# else
(yyt, yyEnv, yyLevel, yyIsCConst, yyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
SHORTCARD *yyLevel;
BOOLEAN *yyIsCConst;
BOOLEAN *yyOpenAccessOrCall;
BOOLEAN *yyGlobalPtrs;
Defs_tStrings *yyStrsIn;
Defs_tStrings *yyStrsOut;
# endif
{
  struct S_65 yyTempo;

  {
    register struct S_65 *W_65 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Elems:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      *yyOpenAccessOrCall = FALSE;
      *yyIsCConst = TRUE;
      break;
    case Tree_Elems0:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      *yyOpenAccessOrCall = FALSE;
      *yyIsCConst = TRUE;
      break;
    case Tree_Elems1:;
      yyVisit1Elems(yyt->U_1.V_96.Elems1.Next, yyEnv, yyLevel, &W_65->U_1.V_1.Elems1yNextyIsCConst, &W_65->U_1.V_1.Elems1yNextyOpenAccessOrCall, yyGlobalPtrs, yyStrsIn, yyStrsOut);
      *yyOpenAccessOrCall = FALSE;
      *yyIsCConst = TRUE;
      break;
    case Tree_Elem:;
      yyVisit1Expr(yyt->U_1.V_97.Elem.Elem, yyEnv, yyLevel, &W_65->U_1.V_2.ElemyElemyOpenAccessOrCall, &W_65->U_1.V_2.ElemyElemyGlobalPtrs, yyStrsIn, &W_65->U_1.V_2.ElemyNextyStrsIn);
      yyVisit1Elems(yyt->U_1.V_97.Elem.Next, yyEnv, yyLevel, &W_65->U_1.V_2.ElemyNextyIsCConst, &W_65->U_1.V_2.ElemyNextyOpenAccessOrCall, &W_65->U_1.V_2.ElemyNextyGlobalPtrs, &W_65->U_1.V_2.ElemyNextyStrsIn, yyStrsOut);
      *yyGlobalPtrs = W_65->U_1.V_2.ElemyElemyGlobalPtrs || W_65->U_1.V_2.ElemyNextyGlobalPtrs;
      *yyOpenAccessOrCall = W_65->U_1.V_2.ElemyElemyOpenAccessOrCall || W_65->U_1.V_2.ElemyNextyOpenAccessOrCall;
      *yyIsCConst = yyt->U_1.V_97.Elem.Elem->U_1.V_77.Expr.IsCConst && W_65->U_1.V_2.ElemyNextyIsCConst;
      break;
    case Tree_ElemRange:;
      yyVisit1Expr(yyt->U_1.V_98.ElemRange.Lwb, yyEnv, yyLevel, &W_65->U_1.V_3.ElemRangeyLwbyOpenAccessOrCall, &W_65->U_1.V_3.ElemRangeyLwbyGlobalPtrs, yyStrsIn, &W_65->U_1.V_3.ElemRangeyLwbyStrsOut);
      yyVisit1Expr(yyt->U_1.V_98.ElemRange.Upb, yyEnv, yyLevel, &W_65->U_1.V_3.ElemRangeyUpbyOpenAccessOrCall, &W_65->U_1.V_3.ElemRangeyUpbyGlobalPtrs, &W_65->U_1.V_3.ElemRangeyLwbyStrsOut, &W_65->U_1.V_3.ElemRangeyNextyStrsIn);
      yyVisit1Elems(yyt->U_1.V_98.ElemRange.Next, yyEnv, yyLevel, &W_65->U_1.V_3.ElemRangeyNextyIsCConst, &W_65->U_1.V_3.ElemRangeyNextyOpenAccessOrCall, &W_65->U_1.V_3.ElemRangeyNextyGlobalPtrs, &W_65->U_1.V_3.ElemRangeyNextyStrsIn, yyStrsOut);
      *yyGlobalPtrs = W_65->U_1.V_3.ElemRangeyLwbyGlobalPtrs || W_65->U_1.V_3.ElemRangeyUpbyGlobalPtrs || W_65->U_1.V_3.ElemRangeyNextyGlobalPtrs;
      *yyOpenAccessOrCall = W_65->U_1.V_3.ElemRangeyLwbyOpenAccessOrCall || W_65->U_1.V_3.ElemRangeyUpbyOpenAccessOrCall || W_65->U_1.V_3.ElemRangeyNextyOpenAccessOrCall;
      *yyIsCConst = yyt->U_1.V_98.ElemRange.Lwb->U_1.V_77.Expr.IsCConst && yyt->U_1.V_98.ElemRange.Upb->U_1.V_77.Expr.IsCConst && W_65->U_1.V_3.ElemRangeyNextyIsCConst;
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Actuals
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv, Defs_tTypes *yyTypes, Defs_tTypes *yyFormals, SHORTCARD *yyLevel, BOOLEAN *yyIsCConst, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut)
# else
(yyt, yyEnv, yyTypes, yyFormals, yyLevel, yyIsCConst, yyGlobalPtrs, yyStrsIn, yyStrsOut)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
Defs_tTypes *yyTypes;
Defs_tTypes *yyFormals;
SHORTCARD *yyLevel;
BOOLEAN *yyIsCConst;
BOOLEAN *yyGlobalPtrs;
Defs_tStrings *yyStrsIn;
Defs_tStrings *yyStrsOut;
# endif
{
  struct S_66 yyTempo;

  {
    register struct S_66 *W_66 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Actuals:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      *yyIsCConst = TRUE;
      *yyTypes = Defs_NoTypes;
      break;
    case Tree_Actuals0:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      *yyIsCConst = TRUE;
      *yyTypes = Defs_NoTypes;
      break;
    case Tree_Actual:;
      Defs_Head(*yyFormals, &yyt->U_1.V_101.Actual.IsVAR, &yyt->U_1.V_101.Actual.Formal);
      yyVisit1Expr(yyt->U_1.V_101.Actual.Expr, yyEnv, yyLevel, &W_66->U_1.V_1.ActualyExpryOpenAccessOrCall, &W_66->U_1.V_1.ActualyExpryGlobalPtrs, yyStrsIn, &W_66->U_1.V_1.ActualyExpryStrsOut);
      W_66->U_1.V_1.ActualyNextyFormals = Defs_Tail(*yyFormals);
      yyVisit1Actuals(yyt->U_1.V_101.Actual.Next, yyEnv, &W_66->U_1.V_1.ActualyNextyTypes, &W_66->U_1.V_1.ActualyNextyFormals, yyLevel, &W_66->U_1.V_1.ActualyNextyIsCConst, &W_66->U_1.V_1.ActualyNextyGlobalPtrs, &W_66->U_1.V_1.ActualyExpryStrsOut, &W_66->U_1.V_1.ActualyNextyStrsOut);
      if ((yyt->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type == Defs_TypeSTRING || yyt->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type == Defs_TypeStringChar) && yyt->U_1.V_101.Actual.Formal->U_1.V_1.Kind == Defs_Array1 && !Defs_IsOpen(yyt->U_1.V_101.Actual.Formal)) {
        yyt->U_1.V_101.Actual.String = GenIdents_GenString();
        *yyStrsOut = Defs_mStringPar(yyt->U_1.V_101.Actual.String, yyt->U_1.V_101.Actual.Formal, (ADDRESS)yyt->U_1.V_101.Actual.Expr, W_66->U_1.V_1.ActualyNextyStrsOut);
      } else {
        yyt->U_1.V_101.Actual.String = Idents_NoIdent;
        *yyStrsOut = W_66->U_1.V_1.ActualyNextyStrsOut;
      }
      *yyGlobalPtrs = W_66->U_1.V_1.ActualyExpryGlobalPtrs || W_66->U_1.V_1.ActualyNextyGlobalPtrs;
      *yyIsCConst = yyt->U_1.V_101.Actual.Expr->U_1.V_77.Expr.IsCConst && W_66->U_1.V_1.ActualyNextyIsCConst;
      *yyTypes = Defs_mTypes(FALSE, yyt->U_1.V_101.Actual.Expr->U_1.V_77.Expr.Type, W_66->U_1.V_1.ActualyNextyTypes);
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Stmts
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut, Defs_tType *yyType)
# else
(yyt, yyEnv, yyLevel, yyGlobalPtrs, yyStrsIn, yyStrsOut, yyType)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
SHORTCARD *yyLevel;
BOOLEAN *yyGlobalPtrs;
Defs_tStrings *yyStrsIn;
Defs_tStrings *yyStrsOut;
Defs_tType *yyType;
# endif
{
  struct S_67 yyTempo;

  {
    register struct S_67 *W_67 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Stmts:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      break;
    case Tree_Stmts0:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      break;
    case Tree_Stmt:;
      yyVisit1Stmts(yyt->U_1.V_104.Stmt.Next, yyEnv, yyLevel, yyGlobalPtrs, yyStrsIn, yyStrsOut, yyType);
      break;
    case Tree_Assign:;
      yyVisit1Designator(yyt->U_1.V_105.Assign.Designator, yyEnv, yyLevel, &W_67->U_1.V_1.AssignyDesignatoryOpenAccessOrCall, &W_67->U_1.V_1.AssignyDesignatoryGlobalPtrs, yyStrsIn, &W_67->U_1.V_1.AssignyDesignatoryStrsOut);
      yyVisit1Expr(yyt->U_1.V_105.Assign.Expr, yyEnv, yyLevel, &W_67->U_1.V_1.AssignyExpryOpenAccessOrCall, &W_67->U_1.V_1.AssignyExpryGlobalPtrs, &W_67->U_1.V_1.AssignyDesignatoryStrsOut, &W_67->U_1.V_1.AssignyNextyStrsIn);
      yyVisit1Stmts(yyt->U_1.V_105.Assign.Next, yyEnv, yyLevel, &W_67->U_1.V_1.AssignyNextyGlobalPtrs, &W_67->U_1.V_1.AssignyNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_1.AssignyDesignatoryGlobalPtrs || W_67->U_1.V_1.AssignyExpryGlobalPtrs || W_67->U_1.V_1.AssignyNextyGlobalPtrs;
      break;
    case Tree_Call:;
      yyVisit1Designator(yyt->U_1.V_106.Call.Designator, yyEnv, yyLevel, &W_67->U_1.V_2.CallyDesignatoryOpenAccessOrCall, &W_67->U_1.V_2.CallyDesignatoryGlobalPtrs, yyStrsIn, &W_67->U_1.V_2.CallyDesignatoryStrsOut);
      if (yyt->U_1.V_106.Call.Designator->U_1.V_87.Designator.Type->U_1.V_1.Kind == Defs_StdProcType1) {
        if (yyt->U_1.V_106.Call.Designator->U_1.V_87.Designator.Type->U_1.V_34.StdProcType1.StdProc == Defs_ProcNEW) {
          yyt->U_1.V_106.Call.AllocOrDealloc = Defs_Identify(Defs_IdentALLOC, *yyEnv);
        } else if (yyt->U_1.V_106.Call.Designator->U_1.V_87.Designator.Type->U_1.V_34.StdProcType1.StdProc == Defs_ProcDISPOSE) {
          yyt->U_1.V_106.Call.AllocOrDealloc = Defs_Identify(Defs_IdentDEALLOC, *yyEnv);
        } else {
          yyt->U_1.V_106.Call.AllocOrDealloc = Defs_NoObject;
        }
      } else {
        yyt->U_1.V_106.Call.AllocOrDealloc = Defs_NoObject;
      }
      W_67->U_1.V_2.CallyActualsyFormals = Defs_GetFormals(yyt->U_1.V_106.Call.Designator->U_1.V_87.Designator.Type);
      yyVisit1Actuals(yyt->U_1.V_106.Call.Actuals, yyEnv, &W_67->U_1.V_2.CallyActualsyTypes, &W_67->U_1.V_2.CallyActualsyFormals, yyLevel, &W_67->U_1.V_2.CallyActualsyIsCConst, &W_67->U_1.V_2.CallyActualsyGlobalPtrs, &W_67->U_1.V_2.CallyDesignatoryStrsOut, &W_67->U_1.V_2.CallyNextyStrsIn);
      yyVisit1Stmts(yyt->U_1.V_106.Call.Next, yyEnv, yyLevel, &W_67->U_1.V_2.CallyNextyGlobalPtrs, &W_67->U_1.V_2.CallyNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_2.CallyDesignatoryGlobalPtrs || W_67->U_1.V_2.CallyActualsyGlobalPtrs || W_67->U_1.V_2.CallyNextyGlobalPtrs;
      break;
    case Tree_If:;
      yyVisit1Expr(yyt->U_1.V_107.If.Cond, yyEnv, yyLevel, &W_67->U_1.V_3.IfyCondyOpenAccessOrCall, &W_67->U_1.V_3.IfyCondyGlobalPtrs, yyStrsIn, &W_67->U_1.V_3.IfyCondyStrsOut);
      yyVisit1Stmts(yyt->U_1.V_107.If.Then, yyEnv, yyLevel, &W_67->U_1.V_3.IfyThenyGlobalPtrs, &W_67->U_1.V_3.IfyCondyStrsOut, &W_67->U_1.V_3.IfyThenyStrsOut, yyType);
      yyVisit1Elsifs(yyt->U_1.V_107.If.Elsifs, yyEnv, yyLevel, &W_67->U_1.V_3.IfyElsifsyGlobalPtrs, &W_67->U_1.V_3.IfyThenyStrsOut, &W_67->U_1.V_3.IfyElsifsyStrsOut, yyType);
      yyVisit1Stmts(yyt->U_1.V_107.If.Else, yyEnv, yyLevel, &W_67->U_1.V_3.IfyElseyGlobalPtrs, &W_67->U_1.V_3.IfyElsifsyStrsOut, &W_67->U_1.V_3.IfyNextyStrsIn, yyType);
      yyVisit1Stmts(yyt->U_1.V_107.If.Next, yyEnv, yyLevel, &W_67->U_1.V_3.IfyNextyGlobalPtrs, &W_67->U_1.V_3.IfyNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_3.IfyCondyGlobalPtrs || W_67->U_1.V_3.IfyThenyGlobalPtrs || W_67->U_1.V_3.IfyElsifsyGlobalPtrs || W_67->U_1.V_3.IfyElseyGlobalPtrs || W_67->U_1.V_3.IfyNextyGlobalPtrs;
      break;
    case Tree_Case:;
      yyVisit1Expr(yyt->U_1.V_108.Case.Expr, yyEnv, yyLevel, &W_67->U_1.V_4.CaseyExpryOpenAccessOrCall, &W_67->U_1.V_4.CaseyExpryGlobalPtrs, yyStrsIn, &W_67->U_1.V_4.CaseyExpryStrsOut);
      yyVisit1Cases(yyt->U_1.V_108.Case.Cases, yyEnv, yyLevel, &W_67->U_1.V_4.CaseyCasesyGlobalPtrs, &W_67->U_1.V_4.CaseyExpryStrsOut, &W_67->U_1.V_4.CaseyCasesyStrsOut, yyType);
      yyVisit1Stmts(yyt->U_1.V_108.Case.Else, yyEnv, yyLevel, &W_67->U_1.V_4.CaseyElseyGlobalPtrs, &W_67->U_1.V_4.CaseyCasesyStrsOut, &W_67->U_1.V_4.CaseyNextyStrsIn, yyType);
      yyVisit1Stmts(yyt->U_1.V_108.Case.Next, yyEnv, yyLevel, &W_67->U_1.V_4.CaseyNextyGlobalPtrs, &W_67->U_1.V_4.CaseyNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_4.CaseyExpryGlobalPtrs || W_67->U_1.V_4.CaseyCasesyGlobalPtrs || W_67->U_1.V_4.CaseyElseyGlobalPtrs || W_67->U_1.V_4.CaseyNextyGlobalPtrs;
      break;
    case Tree_While:;
      yyVisit1Expr(yyt->U_1.V_109.While.Cond, yyEnv, yyLevel, &W_67->U_1.V_5.WhileyCondyOpenAccessOrCall, &W_67->U_1.V_5.WhileyCondyGlobalPtrs, yyStrsIn, &W_67->U_1.V_5.WhileyCondyStrsOut);
      yyVisit1Stmts(yyt->U_1.V_109.While.Stmts, yyEnv, yyLevel, &W_67->U_1.V_5.WhileyStmtsyGlobalPtrs, &W_67->U_1.V_5.WhileyCondyStrsOut, &W_67->U_1.V_5.WhileyNextyStrsIn, yyType);
      yyVisit1Stmts(yyt->U_1.V_109.While.Next, yyEnv, yyLevel, &W_67->U_1.V_5.WhileyNextyGlobalPtrs, &W_67->U_1.V_5.WhileyNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_5.WhileyCondyGlobalPtrs || W_67->U_1.V_5.WhileyStmtsyGlobalPtrs || W_67->U_1.V_5.WhileyNextyGlobalPtrs;
      break;
    case Tree_Repeat:;
      yyVisit1Stmts(yyt->U_1.V_110.Repeat.Stmts, yyEnv, yyLevel, &W_67->U_1.V_6.RepeatyStmtsyGlobalPtrs, yyStrsIn, &W_67->U_1.V_6.RepeatyStmtsyStrsOut, yyType);
      yyVisit1Expr(yyt->U_1.V_110.Repeat.Cond, yyEnv, yyLevel, &W_67->U_1.V_6.RepeatyCondyOpenAccessOrCall, &W_67->U_1.V_6.RepeatyCondyGlobalPtrs, &W_67->U_1.V_6.RepeatyStmtsyStrsOut, &W_67->U_1.V_6.RepeatyNextyStrsIn);
      yyVisit1Stmts(yyt->U_1.V_110.Repeat.Next, yyEnv, yyLevel, &W_67->U_1.V_6.RepeatyNextyGlobalPtrs, &W_67->U_1.V_6.RepeatyNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_6.RepeatyStmtsyGlobalPtrs || W_67->U_1.V_6.RepeatyCondyGlobalPtrs || W_67->U_1.V_6.RepeatyNextyGlobalPtrs;
      break;
    case Tree_Loop:;
      yyVisit1Stmts(yyt->U_1.V_111.Loop.Stmts, yyEnv, yyLevel, &W_67->U_1.V_7.LoopyStmtsyGlobalPtrs, yyStrsIn, &W_67->U_1.V_7.LoopyNextyStrsIn, yyType);
      yyVisit1Stmts(yyt->U_1.V_111.Loop.Next, yyEnv, yyLevel, &W_67->U_1.V_7.LoopyNextyGlobalPtrs, &W_67->U_1.V_7.LoopyNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_7.LoopyStmtsyGlobalPtrs || W_67->U_1.V_7.LoopyNextyGlobalPtrs;
      break;
    case Tree_For:;
      yyVisit1Qualid(yyt->U_1.V_112.For.Qualid, yyEnv, yyLevel, &W_67->U_1.V_8.ForyQualidyOpenAccessOrCall, &W_67->U_1.V_8.ForyQualidyGlobalPtrs, yyStrsIn, &W_67->U_1.V_8.ForyQualidyStrsOut);
      yyVisit1Expr(yyt->U_1.V_112.For.From, yyEnv, yyLevel, &W_67->U_1.V_8.ForyFromyOpenAccessOrCall, &W_67->U_1.V_8.ForyFromyGlobalPtrs, &W_67->U_1.V_8.ForyQualidyStrsOut, &W_67->U_1.V_8.ForyFromyStrsOut);
      yyVisit1Expr(yyt->U_1.V_112.For.To, yyEnv, yyLevel, &W_67->U_1.V_8.ForyToyOpenAccessOrCall, &W_67->U_1.V_8.ForyToyGlobalPtrs, &W_67->U_1.V_8.ForyFromyStrsOut, &W_67->U_1.V_8.ForyToyStrsOut);
      if (yyt->U_1.V_112.For.To->U_1.V_77.Expr.IsCConst) {
        Values_CompConst((ADDRESS)yyt->U_1.V_112.For.To, (ADDRESS)(*yyEnv), &yyt->U_1.V_112.For.ToVal);
      } else {
        yyt->U_1.V_112.For.ToVal = Values_ErrorValue;
      }
      yyVisit1Expr(yyt->U_1.V_112.For.By, yyEnv, yyLevel, &W_67->U_1.V_8.ForyByyOpenAccessOrCall, &W_67->U_1.V_8.ForyByyGlobalPtrs, &W_67->U_1.V_8.ForyToyStrsOut, &W_67->U_1.V_8.ForyByyStrsOut);
      yyVisit1Stmts(yyt->U_1.V_112.For.Stmts, yyEnv, yyLevel, &W_67->U_1.V_8.ForyStmtsyGlobalPtrs, &W_67->U_1.V_8.ForyByyStrsOut, &W_67->U_1.V_8.ForyNextyStrsIn, yyType);
      yyVisit1Stmts(yyt->U_1.V_112.For.Next, yyEnv, yyLevel, &W_67->U_1.V_8.ForyNextyGlobalPtrs, &W_67->U_1.V_8.ForyNextyStrsIn, yyStrsOut, yyType);
      Values_CompConst((ADDRESS)yyt->U_1.V_112.For.By, (ADDRESS)(*yyEnv), &yyt->U_1.V_112.For.ByVal);
      *yyGlobalPtrs = W_67->U_1.V_8.ForyQualidyGlobalPtrs || W_67->U_1.V_8.ForyFromyGlobalPtrs || W_67->U_1.V_8.ForyToyGlobalPtrs || W_67->U_1.V_8.ForyStmtsyGlobalPtrs || W_67->U_1.V_8.ForyNextyGlobalPtrs;
      break;
    case Tree_With:;
      yyVisit1Designator(yyt->U_1.V_113.With.Designator, yyEnv, yyLevel, &W_67->U_1.V_9.WithyDesignatoryOpenAccessOrCall, &W_67->U_1.V_9.WithyDesignatoryGlobalPtrs, yyStrsIn, &W_67->U_1.V_9.WithyDesignatoryStrsOut);
      W_67->U_1.V_9.WithyStmtsyEnv = Defs_mEnv(Defs_GetFields(yyt->U_1.V_113.With.Designator->U_1.V_87.Designator.Type), *yyEnv);
      yyVisit1Stmts(yyt->U_1.V_113.With.Stmts, &W_67->U_1.V_9.WithyStmtsyEnv, yyLevel, &W_67->U_1.V_9.WithyStmtsyGlobalPtrs, &W_67->U_1.V_9.WithyDesignatoryStrsOut, &W_67->U_1.V_9.WithyNextyStrsIn, yyType);
      yyVisit1Stmts(yyt->U_1.V_113.With.Next, yyEnv, yyLevel, &W_67->U_1.V_9.WithyNextyGlobalPtrs, &W_67->U_1.V_9.WithyNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_9.WithyDesignatoryGlobalPtrs || W_67->U_1.V_9.WithyStmtsyGlobalPtrs || W_67->U_1.V_9.WithyNextyGlobalPtrs;
      break;
    case Tree_Exit:;
      yyVisit1Stmts(yyt->U_1.V_114.Exit.Next, yyEnv, yyLevel, yyGlobalPtrs, yyStrsIn, yyStrsOut, yyType);
      break;
    case Tree_Return1:;
      yyVisit1Stmts(yyt->U_1.V_115.Return1.Next, yyEnv, yyLevel, yyGlobalPtrs, yyStrsIn, yyStrsOut, yyType);
      break;
    case Tree_Return2:;
      yyVisit1Expr(yyt->U_1.V_116.Return2.Result, yyEnv, yyLevel, &W_67->U_1.V_10.Return2yResultyOpenAccessOrCall, &W_67->U_1.V_10.Return2yResultyGlobalPtrs, yyStrsIn, &W_67->U_1.V_10.Return2yNextyStrsIn);
      yyt->U_1.V_116.Return2.OpenAccessOrCall = W_67->U_1.V_10.Return2yResultyOpenAccessOrCall;
      yyt->U_1.V_116.Return2.ResultType = *yyType;
      yyVisit1Stmts(yyt->U_1.V_116.Return2.Next, yyEnv, yyLevel, &W_67->U_1.V_10.Return2yNextyGlobalPtrs, &W_67->U_1.V_10.Return2yNextyStrsIn, yyStrsOut, yyType);
      *yyGlobalPtrs = W_67->U_1.V_10.Return2yResultyGlobalPtrs || W_67->U_1.V_10.Return2yNextyGlobalPtrs;
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Elsifs
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut, Defs_tType *yyType)
# else
(yyt, yyEnv, yyLevel, yyGlobalPtrs, yyStrsIn, yyStrsOut, yyType)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
SHORTCARD *yyLevel;
BOOLEAN *yyGlobalPtrs;
Defs_tStrings *yyStrsIn;
Defs_tStrings *yyStrsOut;
Defs_tType *yyType;
# endif
{
  struct S_68 yyTempo;

  {
    register struct S_68 *W_68 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Elsifs:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      break;
    case Tree_Elsifs0:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      break;
    case Tree_Elsifs1:;
      yyVisit1Expr(yyt->U_1.V_119.Elsifs1.Cond, yyEnv, yyLevel, &W_68->U_1.V_1.Elsifs1yCondyOpenAccessOrCall, &W_68->U_1.V_1.Elsifs1yCondyGlobalPtrs, yyStrsIn, &W_68->U_1.V_1.Elsifs1yCondyStrsOut);
      yyVisit1Stmts(yyt->U_1.V_119.Elsifs1.Stmts, yyEnv, yyLevel, &W_68->U_1.V_1.Elsifs1yStmtsyGlobalPtrs, &W_68->U_1.V_1.Elsifs1yCondyStrsOut, &W_68->U_1.V_1.Elsifs1yStmtsyStrsOut, yyType);
      yyVisit1Elsifs(yyt->U_1.V_119.Elsifs1.Next, yyEnv, yyLevel, &W_68->U_1.V_1.Elsifs1yNextyGlobalPtrs, &W_68->U_1.V_1.Elsifs1yStmtsyStrsOut, yyStrsOut, yyType);
      *yyGlobalPtrs = W_68->U_1.V_1.Elsifs1yCondyGlobalPtrs || W_68->U_1.V_1.Elsifs1yStmtsyGlobalPtrs || W_68->U_1.V_1.Elsifs1yNextyGlobalPtrs;
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Cases
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv, SHORTCARD *yyLevel, BOOLEAN *yyGlobalPtrs, Defs_tStrings *yyStrsIn, Defs_tStrings *yyStrsOut, Defs_tType *yyType)
# else
(yyt, yyEnv, yyLevel, yyGlobalPtrs, yyStrsIn, yyStrsOut, yyType)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
SHORTCARD *yyLevel;
BOOLEAN *yyGlobalPtrs;
Defs_tStrings *yyStrsIn;
Defs_tStrings *yyStrsOut;
Defs_tType *yyType;
# endif
{
  struct S_69 yyTempo;

  {
    register struct S_69 *W_69 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Cases:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      break;
    case Tree_Cases0:;
      *yyStrsOut = *yyStrsIn;
      *yyGlobalPtrs = FALSE;
      break;
    case Tree_Cases1:;
      yyVisit1Labels(yyt->U_1.V_122.Cases1.Labels, yyEnv);
      yyVisit1Stmts(yyt->U_1.V_122.Cases1.Stmts, yyEnv, yyLevel, &W_69->U_1.V_1.Cases1yStmtsyGlobalPtrs, yyStrsIn, &W_69->U_1.V_1.Cases1yStmtsyStrsOut, yyType);
      yyVisit1Cases(yyt->U_1.V_122.Cases1.Next, yyEnv, yyLevel, &W_69->U_1.V_1.Cases1yNextyGlobalPtrs, &W_69->U_1.V_1.Cases1yStmtsyStrsOut, yyStrsOut, yyType);
      *yyGlobalPtrs = W_69->U_1.V_1.Cases1yStmtsyGlobalPtrs || W_69->U_1.V_1.Cases1yNextyGlobalPtrs;
      break;
    default :
      break;
    }
  }
}

static void yyVisit1Labels
# ifdef __STDC__
(Tree_tTree yyt, Defs_tEnv *yyEnv)
# else
(yyt, yyEnv)
Tree_tTree yyt;
Defs_tEnv *yyEnv;
# endif
{
  struct S_70 yyTempo;

  {
    register struct S_70 *W_70 = &yyTempo;

    switch (yyt->U_1.V_1.Kind) {
    case Tree_Labels:;
      break;
    case Tree_Labels0:;
      break;
    case Tree_Labels1:;
      yyVisit1Labels(yyt->U_1.V_125.Labels1.Next, yyEnv);
      break;
    case Tree_Label:;
      Values_CompConst((ADDRESS)yyt->U_1.V_126.Label.Label, (ADDRESS)(*yyEnv), &yyt->U_1.V_126.Label.LabelVal);
      W_70->U_1.V_1.LabelyLabelyStrsIn = Defs_NoStrings;
      W_70->U_1.V_1.LabelyLabelyLevel = 0;
      yyVisit1Expr(yyt->U_1.V_126.Label.Label, yyEnv, &W_70->U_1.V_1.LabelyLabelyLevel, &W_70->U_1.V_1.LabelyLabelyOpenAccessOrCall, &W_70->U_1.V_1.LabelyLabelyGlobalPtrs, &W_70->U_1.V_1.LabelyLabelyStrsIn, &W_70->U_1.V_1.LabelyLabelyStrsOut);
      yyVisit1Labels(yyt->U_1.V_126.Label.Next, yyEnv);
      break;
    case Tree_LabelRange:;
      Values_CompConst((ADDRESS)yyt->U_1.V_127.LabelRange.Lwb, (ADDRESS)(*yyEnv), &yyt->U_1.V_127.LabelRange.LwbVal);
      W_70->U_1.V_2.LabelRangeyUpbyStrsIn = Defs_NoStrings;
      W_70->U_1.V_2.LabelRangeyUpbyLevel = 0;
      yyVisit1Expr(yyt->U_1.V_127.LabelRange.Upb, yyEnv, &W_70->U_1.V_2.LabelRangeyUpbyLevel, &W_70->U_1.V_2.LabelRangeyUpbyOpenAccessOrCall, &W_70->U_1.V_2.LabelRangeyUpbyGlobalPtrs, &W_70->U_1.V_2.LabelRangeyUpbyStrsIn, &W_70->U_1.V_2.LabelRangeyUpbyStrsOut);
      W_70->U_1.V_2.LabelRangeyLwbyStrsIn = Defs_NoStrings;
      W_70->U_1.V_2.LabelRangeyLwbyLevel = 0;
      yyVisit1Expr(yyt->U_1.V_127.LabelRange.Lwb, yyEnv, &W_70->U_1.V_2.LabelRangeyLwbyLevel, &W_70->U_1.V_2.LabelRangeyLwbyOpenAccessOrCall, &W_70->U_1.V_2.LabelRangeyLwbyGlobalPtrs, &W_70->U_1.V_2.LabelRangeyLwbyStrsIn, &W_70->U_1.V_2.LabelRangeyLwbyStrsOut);
      yyVisit1Labels(yyt->U_1.V_127.LabelRange.Next, yyEnv);
      Values_CompConst((ADDRESS)yyt->U_1.V_127.LabelRange.Upb, (ADDRESS)(*yyEnv), &yyt->U_1.V_127.LabelRange.UpbVal);
      break;
    default :
      break;
    }
  }
}

void Semantics_BeginSemantics
# ifdef __STDC__
()
# else
()
# endif
{
  UniqueIds_BeginUniqueIds();
}

void Semantics_CloseSemantics
# ifdef __STDC__
()
# else
()
# endif
{
  UniqueIds_CloseUniqueIds();
}

void BEGIN_Semantics()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Tree();
    BEGIN_Tree();
    BEGIN_Idents();
    BEGIN_Tree();
    BEGIN_UniqueIds();
    BEGIN_Values();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_GenIdents();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_StringMem();
    BEGIN_Types();
    BEGIN_Defs();
    BEGIN_Values();
    BEGIN_Idents();
    BEGIN_Defs();
    BEGIN_UniqueIds();
    BEGIN_GenIdents();
    BEGIN_Errors();
    BEGIN_Errors();
    BEGIN_Defs();
    BEGIN_GenIdents();
    BEGIN_Defs();
    BEGIN_Tree();
    BEGIN_Code();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_Defs();
    BEGIN_GenIdents();
    BEGIN_Defs();

  }
}
