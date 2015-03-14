/* $XConsortium: TranslateI.h,v 1.43 92/02/24 17:42:20 converse Exp $ */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* 
 * TranslateI.h - Header file private to translation management
 * 
 * Author:	Gabe Beged-Dov, HP
 *
 * Former Author:	Charles Haynes
 * 		Digital Equipment Corporation
 * 		Western Research Laboratory
 * Date:	Sat Aug 29 1987
 */

/*#define REFCNT_TRANSLATIONS*/
#define CACHE_TRANSLATIONS

#define TM_NO_MATCH (-2)

#define _XtRStateTablePair "_XtStateTablePair"

typedef unsigned char TMByteCard;
typedef unsigned short TMShortCard;
typedef unsigned long TMLongCard;
typedef short TMShortInt;

typedef Boolean (*MatchProc)();
    /* Event *parsed, TMEventPtr incoming */

typedef struct _ModToKeysymTable {
    Modifiers mask;
    int count;
    int idx;
} ModToKeysymTable;

typedef struct _LateBindings {
    unsigned int knot:1;
    unsigned int pair:1;
    unsigned short ref_count;	/* garbage collection */
    KeySym keysym;
} LateBindings, *LateBindingsPtr;

typedef short ModifierMask;

typedef struct _ActionsRec *ActionPtr;
typedef struct _ActionsRec {
    int idx;			/* index into quarkTable to find proc */
    String *params;		/* pointer to array of params */
    Cardinal num_params;	/* number of params */
    ActionPtr next;		/* next action to perform */
} ActionRec;

typedef struct _XtStateRec *StatePtr;
typedef struct _XtStateRec {
    unsigned int	isCycleStart:1;
    unsigned int	isCycleEnd:1;
    TMShortCard		typeIndex;
    TMShortCard		modIndex;
    ActionPtr		actions;	/* rhs list of actions to perform */
    StatePtr 		nextLevel;
}StateRec;


#define XtTableReplace	0
#define XtTableAugment	1
#define XtTableOverride	2
#define XtTableUnmerge  3

typedef unsigned int _XtTranslateOp;

/*
 * New Definitions
 */
typedef struct _TMModifierMatchRec{
    TMLongCard	 modifiers;
    TMLongCard	 modifierMask;
    LateBindingsPtr lateModifiers;
    Boolean	 standard;
}TMModifierMatchRec, *TMModifierMatch;

typedef struct _TMTypeMatchRec{
    TMLongCard	 eventType;
    TMLongCard	 eventCode;
    TMLongCard	 eventCodeMask;
    MatchProc	 matchEvent;
}TMTypeMatchRec, *TMTypeMatch;

typedef struct _TMBranchHeadRec {
    unsigned int	isSimple:1;
    unsigned int	hasActions:1;
    unsigned int	hasCycles:1;
    int			more:13;
    TMShortCard		typeIndex;
    TMShortCard		modIndex;
}TMBranchHeadRec, *TMBranchHead;

/* NOTE: elements of this structure must match those of
 * TMComplexStateTreeRec and TMParseStateTreeRec.
 */
typedef struct _TMSimpleStateTreeRec{
    unsigned int	isSimple:1;
    unsigned int	isAccelerator:1;
    unsigned int	mappingNotifyInterest:1;
    unsigned int	refCount:13;
    TMShortCard		numBranchHeads;
    TMShortCard		numQuarks;   /* # of entries in quarkTbl */
    TMShortCard		unused;	     /* to ensure same alignment */
    TMBranchHeadRec	*branchHeadTbl;
    XrmQuark		*quarkTbl;  /* table of quarkified rhs*/
}TMSimpleStateTreeRec, *TMSimpleStateTree;    

/* NOTE: elements of this structure must match those of
 * TMSimpleStateTreeRec and TMParseStateTreeRec.
 */
typedef struct _TMComplexStateTreeRec{
    unsigned int	isSimple:1;
    unsigned int	isAccelerator:1;
    unsigned int	mappingNotifyInterest:1;
    unsigned int	refCount:13;
    TMShortCard		numBranchHeads;
    TMShortCard		numQuarks;   /* # of entries in quarkTbl */
    TMShortCard		numComplexBranchHeads;
    TMBranchHeadRec	*branchHeadTbl;
    XrmQuark		*quarkTbl;  /* table of quarkified rhs*/
    StatePtr		*complexBranchHeadTbl;
}TMComplexStateTreeRec, *TMComplexStateTree;    

/* NOTE: elements of this structure must match those of
 * TMSimpleStateTreeRec and TMComplexStateTreeRec.
 */
typedef struct _TMParseStateTreeRec{
    unsigned int	isSimple:1;
    unsigned int	isAccelerator:1;
    unsigned int	mappingNotifyInterest:1;
    unsigned int	isStackQuarks:1;
    unsigned int	isStackBranchHeads:1;
    unsigned int	isStackComplexBranchHeads:1;
    unsigned int	unused:10; /* to ensure correct alignment */
    TMShortCard		numBranchHeads;
    TMShortCard		numQuarks;   /* # of entries in quarkTbl */
    TMShortCard		numComplexBranchHeads;
    TMBranchHeadRec	*branchHeadTbl;
    XrmQuark		*quarkTbl;  /* table of quarkified rhs*/
    StatePtr		*complexBranchHeadTbl;
    TMShortCard		branchHeadTblSize;
    TMShortCard		quarkTblSize; /*total size of quarkTbl */
    TMShortCard		complexBranchHeadTblSize;
    StatePtr		head;
}TMParseStateTreeRec, *TMParseStateTree;    

typedef union _TMStateTreeRec{
    TMSimpleStateTreeRec	simple;
    TMParseStateTreeRec		parse;
    TMComplexStateTreeRec	complex;
}*TMStateTree, **TMStateTreePtr, **TMStateTreeList;

typedef struct _TMSimpleBindProcsRec {
    XtActionProc	*procs;
}TMSimpleBindProcsRec, *TMSimpleBindProcs;

typedef struct _TMComplexBindProcsRec {
    Widget	 	widget;		/*widgetID to pass to action Proc*/
    XtTranslations	aXlations;
    XtActionProc	*procs;
}TMComplexBindProcsRec, *TMComplexBindProcs;

typedef struct _TMSimpleBindDataRec {
    unsigned int		isComplex:1;	/* must be first */
    TMSimpleBindProcsRec	bindTbl[1];	/* variable length */
}TMSimpleBindDataRec, *TMSimpleBindData;

typedef struct _TMComplexBindDataRec {
    unsigned int		isComplex:1;	/* must be first */
    struct _ATranslationData	*accel_context;	/* for GetValues */
    TMComplexBindProcsRec	bindTbl[1]; 	/* variable length */
}TMComplexBindDataRec, *TMComplexBindData;

typedef union _TMBindDataRec{
    TMSimpleBindDataRec		simple;
    TMComplexBindDataRec	complex;
}*TMBindData;

typedef struct _TranslationData{
    unsigned char		hasBindings;	/* must be first */
    unsigned char		operation; /*replace,augment,override*/
    TMShortCard			numStateTrees;
    struct _TranslationData    	*composers[2];
    EventMask			eventMask;
    TMStateTree			stateTreeTbl[1]; /* variable length */
}TranslationData;

/*
 * ATranslations is returned by GetValues for translations that contain 
 * accelerators.  The TM can differentiate between this and TranslationData
 * (that don't have a bindTbl) by looking at the first field (hasBindings)
 * of either structure.  All ATranslationData structures associated with a 
 * widget are chained off the BindData record of the widget. 
 */
typedef struct _ATranslationData{ 
    unsigned char		hasBindings;	/* must be first */
    unsigned char		operation;
    struct _TranslationData	*xlations;  /* actual translations */
    struct _ATranslationData	*next;      /* chain the contexts together */
    TMComplexBindProcsRec	bindTbl[1]; /* accelerator bindings */
}ATranslationData, *ATranslations;

typedef struct _TMConvertRec {
    XtTranslations	old; /* table to merge into */
    XtTranslations	new; /* table to merge from */
} TMConvertRec;

#define _XtEventTimerEventType ((TMLongCard)~0L)
#define KeysymModMask		(1L<<27) /* private to TM */
#define AnyButtonMask		(1L<<28) /* private to TM */

typedef struct _EventRec {
    TMLongCard modifiers;
    TMLongCard modifierMask;
    LateBindingsPtr lateModifiers;
    TMLongCard eventType;
    TMLongCard eventCode;
    TMLongCard eventCodeMask;
    MatchProc matchEvent;
    Boolean standard;
} Event;

typedef struct _EventSeqRec *EventSeqPtr;
typedef struct _EventSeqRec {
    Event event;	/* X event description */
    StatePtr state;	/* private to state table builder */
    EventSeqPtr next;	/* next event on line */
    ActionPtr actions;	/* r.h.s.   list of actions to perform */
} EventSeqRec;

typedef EventSeqRec EventRec;
typedef EventSeqPtr EventPtr;

typedef struct _TMEventRec {
    XEvent *xev;
    Event event;
}TMEventRec,*TMEventPtr;

typedef struct _ActionHookRec {
    struct _ActionHookRec* next; /* must remain first */
    XtAppContext app;
    XtActionHookProc proc;
    XtPointer closure;
} ActionHookRec, *ActionHook;

/* choose a number between 2 and 8 */
#define TMKEYCACHELOG2 6
#define TMKEYCACHESIZE (1<<TMKEYCACHELOG2)

typedef struct _KeyCacheRec {
    Modifiers modifiers_return; /* constant for a give XtKeyProc */
    KeyCode keycode[TMKEYCACHESIZE];
    unsigned short modifiers[TMKEYCACHESIZE];
    KeySym keysym[TMKEYCACHESIZE];
} TMKeyCache;

typedef struct _TMKeyContextRec {
    XEvent *event;
    unsigned long serial;
    KeySym keysym;
    Modifiers modifiers;
    TMKeyCache keycache;  /* keep this last, to keep offsets to others small */
} TMKeyContextRec, *TMKeyContext;

typedef struct _TMGlobalRec{
    TMTypeMatchRec 		**typeMatchSegmentTbl;
    TMShortCard			numTypeMatches;
    TMShortCard			numTypeMatchSegments;
    TMShortCard			typeMatchSegmentTblSize;
    TMModifierMatchRec 		**modMatchSegmentTbl;
    TMShortCard			numModMatches;
    TMShortCard			numModMatchSegments;
    TMShortCard			modMatchSegmentTblSize;
    Boolean			newMatchSemantics;
#ifdef TRACE_TM
    XtTranslations		*tmTbl;
    TMShortCard			numTms;
    TMShortCard			tmTblSize;
    struct _TMBindCacheRec	**bindCacheTbl;
    TMShortCard			numBindCache;
    TMShortCard			bindCacheTblSize;
    TMShortCard			numLateBindings;
    TMShortCard			numBranchHeads;
    TMShortCard			numComplexStates;
    TMShortCard			numComplexActions;
#endif /* TRACE_TM */
}TMGlobalRec;

extern TMGlobalRec _XtGlobalTM;

#define TM_MOD_SEGMENT_SIZE 	16
#define TM_TYPE_SEGMENT_SIZE 	16

#define TMGetTypeMatch(idx) \
  ((TMTypeMatch) \
   &((_XtGlobalTM.typeMatchSegmentTbl[((idx) >> 4)])[(idx) & 15]))
#define TMGetModifierMatch(idx) \
  ((TMModifierMatch) \
   &((_XtGlobalTM.modMatchSegmentTbl[(idx) >> 4])[(idx) & 15]))

/* Useful Access Macros */
#define TMNewMatchSemantics() (_XtGlobalTM.newMatchSemantics)
#define TMBranchMore(branch) (branch->more)
#define TMComplexBranchHead(tree, br) \
  (((TMComplexStateTree)tree)->complexBranchHeadTbl[TMBranchMore(br)])

#define TMGetComplexBindEntry(bindData, idx) \
  ((TMComplexBindProcs)&(((TMComplexBindData)bindData)->bindTbl[idx]))

#define TMGetSimpleBindEntry(bindData, idx) \
  ((TMSimpleBindProcs)&(((TMSimpleBindData)bindData)->bindTbl[idx]))


#define _InitializeKeysymTables(dpy, pd) \
    if (pd->keysyms == NULL) \
        _XtBuildKeysymTables(dpy, pd)

/* 
 * Internal Functions
 */

extern void _XtPopup(
#if NeedFunctionPrototypes
    Widget      /* widget */,
    XtGrabKind  /* grab_kind */,
    _XtBoolean	/* spring_loaded */
#endif
);

extern String _XtPrintXlations(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XtTranslations 	/* xlations */,
    Widget		/* accelWidget */,
    _XtBoolean		/* includeRHS */
#endif
);

extern void _XtRegisterGrabs(
#if NeedFunctionPrototypes
    Widget	/* widget */
#endif
);

extern XtPointer _XtInitializeActionData(
#if NeedFunctionPrototypes
    struct _XtActionsRec *	/* actions */,
    Cardinal 			/* count */,
    _XtBoolean			/* inPlace */
#endif
);

extern void _XtAddEventSeqToStateTree(
#if NeedFunctionPrototypes
    EventSeqPtr		/* eventSeq */,
    TMParseStateTree	/* stateTree */
#endif 
);

extern Boolean _XtMatchUsingStandardMods(
#if NeedFunctionPrototypes
    TMTypeMatch		/* typeMatch */,
    TMModifierMatch	/* modMatch */,
    TMEventPtr		/* eventSeq */
#endif
);

extern Boolean _XtMatchUsingDontCareMods(
#if NeedFunctionPrototypes
    TMTypeMatch		/* typeMatch */,
    TMModifierMatch	/* modMatch */,
    TMEventPtr		/* eventSeq */
#endif
);

extern Boolean _XtRegularMatch(
#if NeedFunctionPrototypes
    TMTypeMatch		/* typeMatch */,
    TMModifierMatch	/* modMatch */,
    TMEventPtr		/* eventSeq */
#endif
);

extern Boolean _XtMatchAtom(
#if NeedFunctionPrototypes
    TMTypeMatch		/* typeMatch */,
    TMModifierMatch	/* modMatch */,
    TMEventPtr		/* eventSeq */
#endif
);

extern void _XtTranslateEvent(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XEvent*		/* event */
#endif
);

extern void _XtBuildKeysymTables();

#ifndef NO_MIT_HACKS
extern void  _XtDisplayTranslations(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XEvent*		/* event */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif 
);

extern void  _XtDisplayAccelerators(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XEvent*		/* event */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);

extern void _XtDisplayInstalledAccelerators(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XEvent*		/* event */,
    String*		/* params */,
    Cardinal*		/* num_params */
#endif
);
#endif /* ifndef NO_MIT_HACKS */

extern void _XtPopupInitialize(
#if NeedFunctionPrototypes
    XtAppContext	/* app_context */
#endif
);

extern void _XtBindActions(
#if NeedFunctionPrototypes
    Widget	/* widget */,
    XtTM 	/* tm_rec */
#endif
);

extern Boolean _XtComputeLateBindings(
#if NeedFunctionPrototypes
    LateBindingsPtr	/* lateModifiers */,
    TMEventPtr		/* eventSeq */,
    Modifiers*		/* computed */,
    Modifiers*		/* computedMask */
#endif
);

extern XtTranslations _XtCreateXlations(
#if NeedFunctionPrototypes
    TMStateTree *	/* stateTrees */,
    TMShortCard		/* numStateTrees */,
    XtTranslations 	/* first */,
    XtTranslations	/* second */
#endif
);

extern Boolean _XtCvtMergeTranslations(
#if NeedFunctionPrototypes
    Display*	/* dpy */,
    XrmValuePtr	/* args */,
    Cardinal*	/* num_args */,
    XrmValuePtr	/* from */,
    XrmValuePtr	/* to */,
    XtPointer*	/* closure_ret */
#endif
);

void _XtFreeTranslations(
#if NeedFunctionPrototypes
    XtAppContext	/* app */,
    XrmValuePtr		/* toVal */,
    XtPointer		/* closure */,
    XrmValuePtr		/* args */,
    Cardinal*		/* num_args */
#endif
);

extern TMShortCard _XtGetModifierIndex(
#if NeedFunctionPrototypes
    Event*	/* event */
#endif
);
   
extern TMShortCard _XtGetQuarkIndex(
#if NeedFunctionPrototypes
    TMParseStateTree	/* stateTreePtr */,
    XrmQuark		/* quark */
#endif
);

extern XtTranslations _XtGetTranslationValue(
#if NeedFunctionPrototypes
    Widget		/* widget */
#endif
);

extern TMShortCard _XtGetTypeIndex(
#if NeedFunctionPrototypes
    Event*	/* event */
#endif
);

extern void _XtGrabInitialize(
#if NeedFunctionPrototypes
    XtAppContext	/* app */
#endif
);

extern void _XtInstallTranslations(
#if NeedFunctionPrototypes
    Widget		/* widget */
#endif
);

extern void _XtRemoveTranslations(
#if NeedFunctionPrototypes
    Widget		/* widget */
#endif
);

extern void _XtDestroyTMData(
#if NeedFunctionPrototypes
    Widget		/* widget */
#endif
);

extern void _XtMergeTranslations(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XtTranslations	/* newXlations */,
    _XtTranslateOp	/* operation */
#endif
);

extern void _XtActionInitialize(
#if NeedFunctionPrototypes
    XtAppContext	/* app */
#endif
);

extern TMStateTree _XtParseTreeToStateTree(
#if NeedFunctionPrototypes
    TMParseStateTree 	/* parseTree */
#endif
);

extern String _XtPrintActions(
#if NeedFunctionPrototypes
    ActionRec*	/* actions */,
    XrmQuark*	/* quarkTbl */
#endif
);

extern String _XtPrintEventSeq(
#if NeedFunctionPrototypes
    EventSeqPtr	/* eventSeq */,
    Display*	/* dpy */
#endif
);

typedef Boolean (*_XtTraversalProc)(
#if NeedFunctionPrototypes
    StatePtr	/* state */,
    XtPointer	/* data */
#endif
);
				    
extern void _XtTraverseStateTree(
#if NeedFunctionPrototypes
    TMStateTree		/* tree */,
    _XtTraversalProc	/* func */,				 
    XtPointer		/* data */
#endif
);

extern void _XtTranslateInitialize(
#if NeedFunctionPrototypes
    void
#endif
);

extern void _XtUnbindActions(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XtTranslations	/* xlations */,
    TMBindData		/* bindData */
#endif
);

extern void _XtUnmergeTranslations(
#if NeedFunctionPrototypes
    Widget		/* widget */,
    XtTranslations 	/* xlations */
#endif
);
