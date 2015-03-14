/* $XConsortium: TMaction.c,v 1.16 91/07/31 13:12:13 swick Exp $ */
/*LINTLIBRARY*/

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

/* TMaction.c -- maintains the state table of actions for the translation 
 *              manager.
 */

#include "IntrinsicI.h"
#include "StringDefs.h"

#if __STDC__ && !defined(VMS)
#define RConst const
#else
#define RConst /**/
#endif

static String XtNtranslationError = "translationError";

typedef struct _CompiledAction{
    XrmQuark		signature;
    XtActionProc	proc;
}CompiledAction, *CompiledActionTable;


#define GetClassActions(wc) \
  ((wc->core_class.actions) \
? (((TMClassCache)wc->core_class.actions)->actions) \
: NULL)

static CompiledActionTable CompileActionTable(actions, count, stat, perm)
    register struct _XtActionsRec *actions;
    register Cardinal count;	/* may be 0 */
    Boolean stat;	/* if False, copy before compiling in place */
    Boolean perm;	/* if False, use XrmStringToQuark */
{
    register CompiledActionTable cActions;
    register int i;
    CompiledAction hold;
    CompiledActionTable cTableHold;
    XrmQuark (*func)();

    if (!count)
	return (CompiledActionTable) NULL;
    func = (perm ? XrmPermStringToQuark : XrmStringToQuark);

    if (! stat) {
	cTableHold = cActions = (CompiledActionTable)
	    XtMalloc(count * sizeof(CompiledAction));

	for (i=count; --i >= 0; cActions++, actions++) {
	    cActions->proc = actions->proc;
	    cActions->signature = (*func)(actions->string);
	}
    } else {
	cTableHold = (CompiledActionTable) actions;

	for (i=count; --i >= 0; actions++)
	    ((CompiledActionTable) actions)->signature = 
		(*func)(actions->string);
    }
    cActions = cTableHold;

    /* Insertion sort.  Whatever sort is used, it must be stable. */
    for (i=1; i <= count - 1; i++) {
	register int j;
	hold = cActions[i];
	j = i;
	while (j && cActions[j-1].signature > hold.signature) {
	    cActions[j] = cActions[j-1];
	    j--;
	}
	cActions[j] = hold;
    }

    return cActions;
}


typedef struct _ActionListRec *ActionList;
typedef struct _ActionListRec {
    ActionList		next;
    CompiledActionTable table;
    TMShortCard		count;
} ActionListRec;

static void ReportUnboundActions(xlations, bindData)
    XtTranslations	xlations;
    TMBindData		bindData;
{
    TMSimpleStateTree	stateTree = (TMSimpleStateTree)xlations->stateTreeTbl[0];
    Cardinal num_unbound;
    char     message[10000];
    register Cardinal num_chars;
    register Cardinal i, j;
    XtActionProc *procs;
    num_unbound = 0;
    (void) strcpy(message, "Actions not found: ");
    num_chars = strlen(message);

    for (i=0; 
	 i < xlations->numStateTrees; 
	 i++) {
	if (bindData->simple.isComplex)
	  procs = TMGetComplexBindEntry(bindData, i)->procs;
	else
	  procs = TMGetSimpleBindEntry(bindData, i)->procs;
	stateTree = (TMSimpleStateTree)xlations->stateTreeTbl[i];
	for (j=0; j < stateTree->numQuarks; j++) {
	    if (procs[j] == NULL) {
		String s = XrmQuarkToString(stateTree->quarkTbl[j]);
		if (num_unbound != 0) {
		    (void) strcpy(&message[num_chars], ", ");
		    num_chars = num_chars + 2;
		}
		(void) strcpy(&message[num_chars], s);
		num_chars += strlen(s);
		num_unbound++;
	    }
	}
    }
    message[num_chars] = '\0';
    if (num_unbound != 0)
      XtWarningMsg(XtNtranslationError,"unboundActions",XtCXtToolkitError,
		   message, (String *)NULL, (Cardinal *)NULL);
}


static CompiledAction *SearchActionTable(signature, actionTable, numActions)
    XrmQuark		signature;
    CompiledActionTable	actionTable;
    Cardinal		numActions;
{
    register int i, left, right;

    left = 0;
    right = numActions - 1;
    while (left <= right) {
	i = (left + right) >> 1;
	if (signature < actionTable[i].signature)
	    right = i - 1;
	else if (signature > actionTable[i].signature)
	    left = i + 1;
	else {
	    while (i && actionTable[i - 1].signature == signature)
		i--;
	    return &actionTable[i];
	}
    }
    return (CompiledAction *) NULL;
}

static int BindActions(stateTree, procs, compiledActionTable, numActions, ndxP)
    TMSimpleStateTree	stateTree;
    XtActionProc	*procs;
    CompiledActionTable compiledActionTable;
    TMShortCard		numActions;
    Cardinal 		*ndxP;
{
    register int unbound = stateTree->numQuarks - *ndxP;
    CompiledAction* action;
    register Cardinal ndx;
    register Boolean savedNdx = False;
    
    for (ndx = *ndxP; ndx < stateTree->numQuarks; ndx++) {
	if (procs[ndx] == NULL) {
	    /* attempt to bind it */
	    XrmQuark q = stateTree->quarkTbl[ndx];

	    action = SearchActionTable(q, compiledActionTable, numActions);
	    if (action) {
		procs[ndx] = action->proc;
		unbound--;
	    } else if (!savedNdx) {
		*ndxP= ndx;
		savedNdx = True;
	    }
	} else {
	    /* already bound, leave it alone */
	    unbound--;
	}
    }
    return unbound;
}

typedef struct _TMBindCacheStatusRec{
    unsigned int	boundInClass:1;
    unsigned int	boundInHierarchy:1;
    unsigned int	boundInContext:1;
    unsigned int	notFullyBound:1;
    unsigned int	refCount:12;
}TMBindCacheStatusRec, *TMBindCacheStatus;

typedef struct _TMBindCacheRec{
    struct _TMBindCacheRec *next;
    TMBindCacheStatusRec status;
    TMStateTree		stateTree;
#ifdef TRACE_TM
    WidgetClass		widgetClass;
#endif /* TRACE_TM */
    XtActionProc	procs[1];	/* variable length */
}TMBindCacheRec, *TMBindCache;

typedef struct _TMClassCacheRec {
    CompiledActionTable	actions;
    TMBindCacheRec	*bindCache;
}TMClassCacheRec, *TMClassCache;

#define IsPureClassBind(bc) \
  (bc->status.boundInClass && \
   !(bc->status.boundInHierarchy || \
     bc->status.boundInContext || \
     bc->status.notFullyBound))

#define GetClassCache(w) \
  ((TMClassCache)w->core.widget_class->core_class.actions)


static int  BindProcs(widget, stateTree, procs, bindStatus)
    Widget 		widget;
    TMSimpleStateTree	stateTree;
    XtActionProc	*procs;
    TMBindCacheStatus 	bindStatus;
{
    register WidgetClass    	class;
    register ActionList     	actionList;
    int 			unbound = -1, newUnbound = -1;
    Cardinal 			ndx = 0;
    Widget			w = widget; 
   
    do {
        class = w->core.widget_class;
        do {
            if (class->core_class.actions != NULL)
	      unbound =
		BindActions(stateTree,
			    procs,
			    GetClassActions(class),
			    class->core_class.num_actions,
			    &ndx);
	    class = class->core_class.superclass;
        } while (unbound != 0 && class != NULL);
	if (unbound < (int)stateTree->numQuarks)
	  bindStatus->boundInClass = True;
	else
	  bindStatus->boundInClass = False;
	if (newUnbound == -1)
	  newUnbound = unbound;
	w = XtParent(w);
    } while (unbound != 0 && w != NULL);
    
    if (newUnbound > unbound)
      bindStatus->boundInHierarchy = True;
    else
      bindStatus->boundInHierarchy = False;

    if (unbound) {
	XtAppContext app = XtWidgetToApplicationContext(widget);
	newUnbound = unbound;
	for (actionList = app->action_table;
	     unbound != 0 && actionList != NULL;
	     actionList = actionList->next) {
	    unbound = BindActions(stateTree,
				  procs,
				  actionList->table,
				  actionList->count,
				  &ndx);
	}
	if (newUnbound > unbound)
	  bindStatus->boundInContext = True;
	else
	  bindStatus->boundInContext = False;

    }

    return unbound;
}

static XtActionProc  *TryBindCache(widget, stateTree)
    Widget	widget;
    TMStateTree	stateTree;
{
    TMClassCache	classCache = GetClassCache(widget);

    if (classCache == NULL)
      {
	  WidgetClass	wc = XtClass(widget);

	  wc->core_class.actions = (XtActionList)
	    _XtInitializeActionData(NULL, 0, True);
      }
    else 
      {
	  TMBindCache bindCache =
	    (TMBindCache)(classCache->bindCache);
	  for (; bindCache; bindCache = bindCache->next)
	    {
		if (IsPureClassBind(bindCache) && 
		    (stateTree == bindCache->stateTree))
		  {
		      bindCache->status.refCount++;
		      return &bindCache->procs[0];
		  }
	    }
      }
    return NULL;
}



/*
 * The class record actions field will point to the bind cache header
 * after this call is made out of coreClassPartInit.
 */
#if NeedFunctionPrototypes
XtPointer _XtInitializeActionData(
    register struct _XtActionsRec	*actions,
    register Cardinal			count,
    _XtBoolean				inPlace
    )
#else
XtPointer _XtInitializeActionData(actions, count, inPlace)
    register struct _XtActionsRec	*actions;
    register Cardinal			count;
    Boolean				inPlace;
#endif
{
    TMClassCache	classCache;

    classCache = XtNew(TMClassCacheRec);
    classCache->actions = CompileActionTable(actions, count, inPlace, True);
    classCache->bindCache = NULL;
    return (XtPointer)classCache;
}


#define TM_BIND_CACHE_REALLOC	2

static XtActionProc *EnterBindCache(w, stateTree, procs, bindStatus)
    Widget		w;
    TMSimpleStateTree	stateTree;
    XtActionProc 	*procs;
    TMBindCacheStatus 	bindStatus;
{
    TMClassCache	classCache = GetClassCache(w);
    TMBindCache		*bindCachePtr = &classCache->bindCache;
    TMShortCard		procsSize = stateTree->numQuarks * sizeof(XtActionProc);
    TMBindCache		bindCache;

    for (bindCache = *bindCachePtr;
	 (*bindCachePtr); 
	bindCachePtr = &(*bindCachePtr)->next, bindCache = *bindCachePtr)
      {
	  TMBindCacheStatus	cacheStatus = &bindCache->status;

	  if ((bindStatus->boundInClass == cacheStatus->boundInClass) &&
	      (bindStatus->boundInHierarchy == cacheStatus->boundInHierarchy) &&
	      (bindStatus->boundInContext == cacheStatus->boundInContext) &&
	      (bindCache->stateTree == (TMStateTree)stateTree) &&
	      !XtBCmp(&bindCache->procs[0], procs, procsSize))
	    {
		bindCache->status.refCount++;
		break;
	    }
      }
    if (*bindCachePtr == NULL)
      {
	  *bindCachePtr = 
	    bindCache = (TMBindCache)
	      XtMalloc(sizeof(TMBindCacheRec) + 
		       (procsSize - sizeof(XtActionProc)));
	  bindCache->next = NULL;
	  bindCache->status = *bindStatus;
	  bindCache->status.refCount = 1;
	  bindCache->stateTree = (TMStateTree)stateTree;
#ifdef TRACE_TM	
	  bindCache->widgetClass = XtClass(w);
	  if (_XtGlobalTM.numBindCache == _XtGlobalTM.bindCacheTblSize)
	    {
		_XtGlobalTM.bindCacheTblSize += 16;
		_XtGlobalTM.bindCacheTbl = (TMBindCache *)
		  XtRealloc((char *)_XtGlobalTM.bindCacheTbl, 
			    ((_XtGlobalTM.bindCacheTblSize) *
			     sizeof(TMBindCache)));
	    }
	  _XtGlobalTM.bindCacheTbl[_XtGlobalTM.numBindCache++] = bindCache;
#endif /* TRACE_TM */
	  XtBCopy((XtPointer)procs, 
		  (XtPointer)&bindCache->procs[0], 
		  procsSize);
      }
    return &bindCache->procs[0];
}

static void RemoveFromBindCache(w,procs)
    Widget		w;
    XtActionProc 	*procs;
{
    TMClassCache	classCache = GetClassCache(w);
    TMBindCache		*bindCachePtr = (TMBindCache *)&classCache->bindCache;
    TMBindCache		bindCache;

    for (bindCache = *bindCachePtr;
	 *bindCachePtr;
	 bindCachePtr = &(*bindCachePtr)->next, bindCache = *bindCachePtr)
      {
	  if (&bindCache->procs[0] == procs)
	    {
		if (--bindCache->status.refCount == 0)
		  {
#ifdef TRACE_TM	
		      TMShortCard	j;
		      Boolean		found = False;
		      TMBindCache	*tbl = _XtGlobalTM.bindCacheTbl;

		      for (j = 0; j < _XtGlobalTM.numBindCache; j++) {
			  if (found)
			    tbl[j-1] = tbl[j];
			  if (tbl[j] == bindCache)
			    found = True;
		      }
		      if (!found)
			XtWarning("where's the action ??? ");
		      else
			_XtGlobalTM.numBindCache--;
#endif /* TRACE_TM */
		      *bindCachePtr = bindCache->next;
		      XtFree((XtPointer)bindCache);
		  }
		break;
	    }
      }
}

/* ARGSUSED */
static void RemoveAccelerators(widget,closure,data)
    Widget widget;
    XtPointer closure, data;
{
    Widget 		destination = (Widget)closure;
    TMComplexBindProcs	bindProcs;
    XtTranslations	stackXlations[16];
    XtTranslations	*xlationsList, destXlations;
    TMShortCard		i, numXlations = 0;

    if ((destXlations = destination->core.tm.translations) == NULL) {
        XtAppWarningMsg(XtWidgetToApplicationContext(widget),
            XtNtranslationError,"nullTable",XtCXtToolkitError,
            "Can't remove accelerators from NULL table",
            (String *)NULL, (Cardinal *)NULL);
        return;
    }

    xlationsList = (XtTranslations *) 
      XtStackAlloc((destXlations->numStateTrees * sizeof(XtTranslations)),
		   stackXlations);

    for (i = 0, bindProcs = TMGetComplexBindEntry(destination->core.tm.proc_table, i);
	 i < destXlations->numStateTrees;
	 i++, bindProcs++) {
	if (bindProcs->widget == widget) {
	    /*
	     * if it's being destroyed don't do all the work
	     */
	    if (destination->core.being_destroyed) {
		bindProcs->procs = NULL;
	    }
	    else
	      xlationsList[numXlations] = bindProcs->aXlations;
	    numXlations++;
	}
    }

    if (numXlations == 0)
      XtAppWarningMsg(XtWidgetToApplicationContext(widget),
		      XtNtranslationError,"nullTable",XtCXtToolkitError,
		      "Tried to remove nonexistent accelerators",
		      (String *)NULL, (Cardinal *)NULL);
    else {
	if (!destination->core.being_destroyed)
	  for (i = 0; i < numXlations; i++)
	    _XtUnmergeTranslations(destination, xlationsList[i]);
    }
    XtStackFree((char *)xlationsList, stackXlations);
}

void _XtBindActions(widget, tm)
    Widget widget;
    XtTM tm;
{
    XtTranslations  		xlations = tm->translations;
    TMSimpleStateTree		stateTree;
    int				globalUnbound = 0;
    Cardinal 			i;
    TMBindData			bindData = (TMBindData)tm->proc_table;
    TMSimpleBindProcs		simpleBindProcs;
    TMComplexBindProcs 		complexBindProcs;
    XtActionProc		*newProcs;
    Widget			bindWidget;

    if ((xlations == NULL) || widget->core.being_destroyed) 
      return;

    stateTree = (TMSimpleStateTree)xlations->stateTreeTbl[0];
    
    for (i = 0; i < xlations->numStateTrees; i++)
      {
	  stateTree = (TMSimpleStateTree)xlations->stateTreeTbl[i];
	  if (bindData->simple.isComplex) {
	      complexBindProcs = TMGetComplexBindEntry(bindData, i);
	      if (complexBindProcs->widget) {
		  bindWidget = complexBindProcs->widget;
		  
		  if (bindWidget->core.destroy_callbacks != NULL)
		      _XtAddCallbackOnce((InternalCallbackList *)
					 &bindWidget->core.destroy_callbacks,
					 RemoveAccelerators,
					 (XtPointer)widget);
		  else
		      _XtAddCallback((InternalCallbackList *)
				     &bindWidget->core.destroy_callbacks,
				     RemoveAccelerators,
				     (XtPointer)widget);
	      }
	      else
		bindWidget = widget;
	  }
	  else {
	      simpleBindProcs = TMGetSimpleBindEntry(bindData, i);
	      bindWidget = widget;
	  }
	  if ((newProcs = 
	       TryBindCache(bindWidget,(TMStateTree)stateTree)) == NULL)
	    {
		XtActionProc		*procs, stackProcs[256];
		int			localUnbound;
		TMBindCacheStatusRec	bcStatusRec;

		procs = (XtActionProc *)
		  XtStackAlloc(stateTree->numQuarks * sizeof(XtActionProc),
			       stackProcs);
		XtBZero((XtPointer)procs, 
		      stateTree->numQuarks * sizeof(XtActionProc));

		localUnbound = BindProcs(bindWidget, 
					 stateTree, 
					 procs,
					 &bcStatusRec);

		if (localUnbound)
		  bcStatusRec.notFullyBound = True;
		else
		  bcStatusRec.notFullyBound = False;

		newProcs =
		  EnterBindCache(bindWidget, 
				 stateTree, 
				 procs,
				 &bcStatusRec);
		XtStackFree((XtPointer)procs, (XtPointer)stackProcs);
		globalUnbound += localUnbound;
	    }
	  if (bindData->simple.isComplex)
	    complexBindProcs->procs = newProcs;
	  else
	    simpleBindProcs->procs = newProcs;
      }
    if (globalUnbound) 
      ReportUnboundActions(xlations,
			   (TMBindData)tm->proc_table);
}


void _XtUnbindActions(widget, xlations, bindData)
    Widget 	widget;
    XtTranslations xlations;
    TMBindData	bindData;
{
    Cardinal			i;
    Widget			bindWidget;
    XtActionProc		*procs;

    if ((xlations == NULL) || !XtIsRealized(widget)) return;

    for (i = 0; i < xlations->numStateTrees; i++) {
	if (bindData->simple.isComplex) {
	    TMComplexBindProcs	complexBindProcs;

	    complexBindProcs = TMGetComplexBindEntry(bindData, i);

	    if (complexBindProcs->widget) {
		/* 
		 * check for this being an accelerator binding whose
		 * source is gone ( set by RemoveAccelerators) 
		 */
		if (complexBindProcs->procs == NULL)
		  continue;

		XtRemoveCallback(complexBindProcs->widget,
				 XtNdestroyCallback,
				 RemoveAccelerators,
				 (XtPointer)widget);
		bindWidget = complexBindProcs->widget;
	    }
	    else
	      bindWidget = widget;
	    procs = complexBindProcs->procs;
	    complexBindProcs->procs = NULL;
	} 
	else {
	    TMSimpleBindProcs simpleBindProcs;
	    simpleBindProcs = TMGetSimpleBindEntry(bindData,i);
	    procs = simpleBindProcs->procs;
	    simpleBindProcs->procs = NULL;
	    bindWidget = widget;
	}
	RemoveFromBindCache(bindWidget, procs);
      }
}

#ifdef notdef
void _XtRemoveBindProcsByIndex(w, bindData, ndx)
    Widget	w;
    TMBindData	bindData;
    TMShortCard	ndx;
{
    TMShortCard	i = ndx;
    TMBindProcs	bindProcs = (TMBindProcs)&bindData->bindTbl[0];

    RemoveFromBindCache(bindProcs->widget ? bindProcs->widget : w,
			bindProcs[i].procs);

    for (; i < bindData->bindTblSize; i++)
      bindProcs[i] = bindProcs[i+1];
}
#endif /* notdef */

/*
 * used to free all copied action tables, called from DestroyAppContext
 */
void _XtFreeActions(actions)
    ActionList	actions;
{
    ActionList	curr, next;

    for (curr = actions; curr;) {
	next = curr->next;
	XtFree((char *)curr->table);
	XtFree((char *)curr);
	curr = next;
    }
}

void XtAddActions(actions, num_actions)
    XtActionList actions;
    Cardinal num_actions;
{
    XtAppAddActions(_XtDefaultAppContext(), actions, num_actions);
}

void XtAppAddActions(app, actions, num_actions)
    XtAppContext app;
    XtActionList actions;
    Cardinal num_actions;
{
    register ActionList rec;

    rec = XtNew(ActionListRec);
    rec->next = app->action_table;
    app->action_table = rec;
    rec->table = CompileActionTable(actions, num_actions, False, False);
    rec->count = num_actions;
}

void XtGetActionList(widget_class, actions_return, num_actions_return)
    WidgetClass widget_class;
    XtActionList* actions_return;
    Cardinal* num_actions_return;
{
    XtActionList list;
    CompiledActionTable table;
    int i;

    *actions_return = NULL;
    *num_actions_return = 0;

    if (! widget_class->core_class.class_inited)
	return;
    if (! (widget_class->core_class.class_inited & WidgetClassFlag))
	return;
    *num_actions_return = widget_class->core_class.num_actions;
    if (*num_actions_return) {
	list = *actions_return = (XtActionList) 
	    XtMalloc(*num_actions_return * sizeof(XtActionsRec));
	table = GetClassActions(widget_class);
	for (i= (*num_actions_return); --i >= 0; list++, table++) {
	    list->string = XrmQuarkToString(table->signature);
	    list->proc = table->proc;
	}
    }
}

/***********************************************************************
 *
 * Pop-up and Grab stuff
 *
 ***********************************************************************/

static Widget _XtFindPopup(widget, name)
    Widget widget;
    String name;
{
    register Cardinal i;
    register XrmQuark q;
    register Widget w;

    q = XrmStringToQuark(name);

    for (w=widget; w != NULL; w=w->core.parent)
	for (i=0; i<w->core.num_popups; i++)
	    if (w->core.popup_list[i]->core.xrm_name == q)
		return w->core.popup_list[i];

    return NULL;
}

void XtMenuPopupAction(widget, event, params, num_params)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Boolean spring_loaded;
    register Widget popup_shell;

    if (*num_params != 1) {
	XtAppWarningMsg(XtWidgetToApplicationContext(widget),
		      "invalidParameters","xtMenuPopupAction",XtCXtToolkitError,
			"MenuPopup wants exactly one argument",
			(String *)NULL, (Cardinal *)NULL);
	return;
    }

    if (event->type == ButtonPress)
	spring_loaded = True;
    else if (event->type == KeyPress || event->type == EnterNotify)
	spring_loaded = False;
    else {
	XtAppWarningMsg(XtWidgetToApplicationContext(widget),
		"invalidPopup","unsupportedOperation",XtCXtToolkitError,
"Pop-up menu creation is only supported on ButtonPress, KeyPress or EnterNotify events.",
                  (String *)NULL, (Cardinal *)NULL);
	spring_loaded = False;
    }

    popup_shell = _XtFindPopup(widget, params[0]);
    if (popup_shell == NULL) {
	XtAppWarningMsg(XtWidgetToApplicationContext(widget),
			"invalidPopup","xtMenuPopup",XtCXtToolkitError,
			"Can't find popup widget \"%s\" in XtMenuPopup",
			params, num_params);
	return;
    }

    if (spring_loaded) _XtPopup(popup_shell, XtGrabExclusive, TRUE);
    else _XtPopup(popup_shell, XtGrabNonexclusive, FALSE);
}


/*ARGSUSED*/
static void _XtMenuPopdownAction(widget, event, params, num_params)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Widget popup_shell;

    if (*num_params == 0) {
	XtPopdown(widget);
    } else if (*num_params == 1) {
	popup_shell = _XtFindPopup(widget, params[0]);
	if (popup_shell == NULL) {
            XtAppWarningMsg(XtWidgetToApplicationContext(widget),
			    "invalidPopup","xtMenuPopdown",XtCXtToolkitError,
			    "Can't find popup widget \"%s\" in XtMenuPopdown",
			    params, num_params);
	    return;
	}
	XtPopdown(popup_shell);
    } else {
	XtAppWarningMsg(XtWidgetToApplicationContext(widget),
			"invalidParameters","xtMenuPopdown",XtCXtToolkitError,
			"XtMenuPopdown called with num_params != 0 or 1",
			(String *)NULL, (Cardinal *)NULL);
    }
}

static XtActionsRec RConst tmActions[] = {
    {"XtMenuPopup", XtMenuPopupAction},
    {"XtMenuPopdown", _XtMenuPopdownAction},
    {"MenuPopup", XtMenuPopupAction}, /* old & obsolete */
    {"MenuPopdown", _XtMenuPopdownAction}, /* ditto */
#ifndef NO_MIT_HACKS
    {"XtDisplayTranslations", _XtDisplayTranslations},
    {"XtDisplayAccelerators", _XtDisplayAccelerators},
    {"XtDisplayInstalledAccelerators", _XtDisplayInstalledAccelerators},
#endif
};


void _XtActionInitialize(app)
    XtAppContext app;
{
    register ActionList rec;

    rec = XtNew(ActionListRec);
    rec->next = app->action_table;
    app->action_table = rec;
    rec->table = CompileActionTable(tmActions, XtNumber(tmActions), False,
				    True);
    rec->count = XtNumber(tmActions);
}

#if NeedFunctionPrototypes
void XtCallActionProc(
    Widget widget,
    _Xconst char* action,
    XEvent *event,
    String *params,
    Cardinal num_params
    )
#else
void XtCallActionProc(widget, action, event, params, num_params)
    Widget widget;
    String action;
    XEvent *event;
    String *params;
    Cardinal num_params;
#endif
{
    CompiledAction* actionP;
    XrmQuark q = XrmStringToQuark(action);
    Widget w = widget;
    XtAppContext app = XtWidgetToApplicationContext(widget);
    ActionList actionList;
    Cardinal i;
    
    XtCheckSubclass(widget, coreWidgetClass,
		    "XtCallActionProc first argument is not a subclass of Core");
    
    do {
	WidgetClass class = XtClass(w);
	do {
	    if ((actionP = GetClassActions(class)) != NULL)
	      for (i = 0; 
		   i < class->core_class.num_actions; 
		   i++, actionP++) {
		  
		  if (actionP->signature == q) {
		      ActionHook hook = app->action_hook_list;
		      while (hook != NULL) {
			  (*hook->proc)( widget,
					hook->closure,
					(String)action,
					event,
					params,
					&num_params
					);
			  hook= hook->next;
		      }
		      (*(actionP->proc))
			(widget, event, params, &num_params);
		      return;
		  }
	      }
	    class = class->core_class.superclass;
	} while (class != NULL);
	w = XtParent(w);
    } while (w != NULL);
    
    for (actionList = app->action_table;
	 actionList != NULL;
	 actionList = actionList->next) {
	
	for (i = 0, actionP = actionList->table; 
	     i < actionList->count;
	     i++, actionP++) {
	    if (actionP->signature == q) {
		ActionHook hook = app->action_hook_list;
		while (hook != NULL) {
		    (*hook->proc)( widget,
				  hook->closure,
				  (String)action,
				  event,
				  params,
				  &num_params
				  );
		    hook= hook->next;
		}
		(*(actionP->proc))
		  (widget, event, params, &num_params);
		return;
	    }
	}
	
    }
    
    {
	String params[2];
	Cardinal num_params = 2;
	params[0] = (String)action;
	params[1] = XtName(widget);
	XtAppWarningMsg(app,
			"noActionProc", "xtCallActionProc", XtCXtToolkitError,
			"No action proc named \"%s\" is registered for widget \"%s\"",
			params, &num_params
			);
    }
}


