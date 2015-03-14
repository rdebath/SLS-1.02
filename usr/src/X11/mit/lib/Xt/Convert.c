/* $Header: /home/x_cvs/mit/lib/Xt/Convert.c,v 1.3 1992/09/16 14:51:15 dawes Exp $ */
/* $XConsortium: Convert.c,v 1.65 92/02/27 17:08:12 converse Exp $ */

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

#include	"IntrinsicI.h"
#include	"StringDefs.h"

/* Conversion procedure hash table */

#define CONVERTHASHSIZE	((unsigned)256)
#define CONVERTHASHMASK	255
#define ProcHash(from_type, to_type) (2 * (from_type) + to_type)

typedef struct _ConverterRec *ConverterPtr;
typedef struct _ConverterRec {
    ConverterPtr	next;
    XrmRepresentation	from, to;
    XtTypeConverter	converter;
    XtDestructor	destructor;
    unsigned short	num_args;
    unsigned int	do_ref_count:1;
    unsigned int	new_style:1;
    char		cache_type;
} ConverterRec;

#define ConvertArgs(p) ((XtConvertArgList)((p)+1))

/* used for old-style type converter cache only */
static Heap globalHeap = {NULL, NULL, 0};

void _XtSetDefaultConverterTable(table)
	ConverterTable *table;
{
    register ConverterTable globalConverterTable =
	_XtGetProcessContext()->globalConverterTable;

    *table = (ConverterTable)
	XtCalloc(CONVERTHASHSIZE, (unsigned)sizeof(ConverterPtr));
    _XtAddDefaultConverters(*table);

    if (globalConverterTable) {
	ConverterPtr rec;
	int i;
	XtCacheType cache_type;
	for (i = CONVERTHASHSIZE; --i >= 0; ) {
	    for (rec = *globalConverterTable++; rec; rec = rec->next) {
		cache_type = rec->cache_type;
		if (rec->do_ref_count)
		    cache_type |= XtCacheRefCount;
	       _XtTableAddConverter(*table, rec->from, rec->to, rec->converter,
				    ConvertArgs(rec), rec->num_args,
				    rec->new_style, cache_type,
				    rec->destructor);
	    }
  	}
    }
}

void _XtFreeConverterTable(table)
	ConverterTable table;
{
	register int i;
	register ConverterPtr p;

	for (i = 0; i < CONVERTHASHSIZE; i++) {
	    for (p = table[i]; p; ) {
		register ConverterPtr next = p->next;
		XtFree((char*)p);
		p = next;
	    }
	}
	XtFree((char*)table);
}	

/* Data cache hash table */

typedef struct _CacheRec *CachePtr;

typedef struct _CacheRec {
    CachePtr	next;
    XtPointer	tag;
    int		hash;
    XtTypeConverter converter;
    unsigned short num_args;
    unsigned int conversion_succeeded:1;
    unsigned int has_ext:1;
    unsigned int is_refcounted:1;
    unsigned int must_be_freed:1;
    unsigned int from_is_value:1;
    unsigned int to_is_value:1;
    XrmValue	from;
    XrmValue	to;
} CacheRec;

typedef struct _CacheRecExt {
    CachePtr	*prev;
    XtDestructor destructor;
    XtPointer	 closure;
    long	 ref_count;
} CacheRecExt;

#define CEXT(p) ((CacheRecExt *)((p)+1))
#define CARGS(p) ((p)->has_ext ? (XrmValue *)(CEXT(p)+1) : (XrmValue *)((p)+1))

#define CACHEHASHSIZE	256
#define CACHEHASHMASK	255
typedef CachePtr CacheHashTable[CACHEHASHSIZE];

static CacheHashTable	cacheHashTable;

#if NeedFunctionPrototypes
void _XtTableAddConverter(
    ConverterTable	table,
    XrmRepresentation   from_type,
    XrmRepresentation   to_type,
    XtTypeConverter	converter,
    XtConvertArgList    convert_args,
    Cardinal		num_args,
    _XtBoolean		new_style,
    XtCacheType		cache_type,
    XtDestructor	destructor
    )
#else    			  
void _XtTableAddConverter(table, from_type, to_type, converter, convert_args, 
			  num_args, new_style, cache_type, destructor)
    ConverterTable	table;
    XrmRepresentation   from_type, to_type;
    XtTypeConverter	converter;
    XtConvertArgList    convert_args;
    Cardinal		num_args;
    Boolean		new_style;
    XtCacheType		cache_type;
    XtDestructor	destructor;
#endif
{
    register ConverterPtr	*pp;
    register ConverterPtr	p;
    XtConvertArgList args;

    pp= &table[ProcHash(from_type, to_type) & CONVERTHASHMASK];
    while ((p = *pp) && (p->from != from_type || p->to != to_type))
	pp = &p->next;

    if (p) {
	*pp = p->next;
	XtFree((char *)p);
    }

    p = (ConverterPtr) XtMalloc(sizeof(ConverterRec) +
				sizeof(XtConvertArgRec) * num_args);
    p->next	    = *pp;
    *pp = p;
    p->from	    = from_type;
    p->to	    = to_type;
    p->converter    = converter;
    p->destructor   = destructor;
    p->num_args     = num_args;	
    args = ConvertArgs(p);
    while (num_args--)
	*args++ = *convert_args++;
    p->new_style    = new_style;
    p->do_ref_count = False;
    if (destructor || (cache_type & 0xff)) {
	p->cache_type = cache_type & 0xff;
	if (cache_type & XtCacheRefCount)
	    p->do_ref_count = True;
    } else {
	p->cache_type = XtCacheNone;
    }
}

#if NeedFunctionPrototypes
void XtSetTypeConverter(
    register _Xconst char* from_type,
    register _Xconst char* to_type,
    XtTypeConverter	converter,
    XtConvertArgList    convert_args,
    Cardinal		num_args,
    XtCacheType		cache_type,
    XtDestructor	destructor
    )
#else
void XtSetTypeConverter(from_type, to_type, converter, convert_args, num_args, cache_type, destructor)
    register String	from_type, to_type;
    XtTypeConverter	converter;
    XtConvertArgList    convert_args;
    Cardinal		num_args;
    XtCacheType		cache_type;
    XtDestructor	destructor;
#endif
{
    ProcessContext process = _XtGetProcessContext();
    XtAppContext app = process->appContextList;
    XrmRepresentation from = XrmStringToRepresentation(from_type);
    XrmRepresentation to = XrmStringToRepresentation(to_type);

    if (!process->globalConverterTable) {
	process->globalConverterTable = (ConverterTable)
	    XtCalloc(CONVERTHASHSIZE, (unsigned)sizeof(ConverterPtr));
    }
    _XtTableAddConverter(process->globalConverterTable, from, to,
			 converter, convert_args,
			 num_args, True, cache_type, destructor);
    while (app) {
	_XtTableAddConverter(app->converterTable, from, to,
			     converter, convert_args,
			     num_args, True, cache_type, destructor);
	app = app->next;
    }
}

#if NeedFunctionPrototypes
void XtAppSetTypeConverter(
    XtAppContext	app,
    register _Xconst char* from_type,
    register _Xconst char* to_type,
    XtTypeConverter	converter,
    XtConvertArgList    convert_args,
    Cardinal		num_args,
    XtCacheType		cache_type,
    XtDestructor	destructor
    )
#else
void XtAppSetTypeConverter(app, from_type, to_type, converter, convert_args, num_args, cache_type, destructor)
    XtAppContext	app;
    register String	from_type, to_type;
    XtTypeConverter	converter;
    XtConvertArgList    convert_args;
    Cardinal		num_args;
    XtCacheType		cache_type;
    XtDestructor	destructor;
#endif
{
    _XtTableAddConverter(app->converterTable,
	XrmStringToRepresentation(from_type),
        XrmStringToRepresentation(to_type),
	converter, convert_args, num_args,
	True, cache_type, destructor);
}

/* old interface */
#if NeedFunctionPrototypes
void XtAddConverter(
    register _Xconst char* from_type,
    register _Xconst char* to_type,
    XtConverter		converter,
    XtConvertArgList    convert_args,
    Cardinal		num_args
    )
#else
void XtAddConverter(from_type, to_type, converter, convert_args, num_args)
    register String	from_type, to_type;
    XtConverter		converter;
    XtConvertArgList    convert_args;
    Cardinal		num_args;
#endif
{
    ProcessContext process = _XtGetProcessContext();
    XtAppContext app = process->appContextList;
    XrmRepresentation from = XrmStringToRepresentation(from_type);
    XrmRepresentation to = XrmStringToRepresentation(to_type);

    if (!process->globalConverterTable) {
	process->globalConverterTable = (ConverterTable)
	    XtCalloc(CONVERTHASHSIZE, (unsigned)sizeof(ConverterPtr));
    }
    _XtTableAddConverter(process->globalConverterTable, from, to,
			 (XtTypeConverter)converter, convert_args, num_args,
			 False, XtCacheAll, (XtDestructor)NULL);
    while (app) {
	_XtTableAddConverter(app->converterTable, from, to,
			     (XtTypeConverter)converter, convert_args,
			     num_args, False, XtCacheAll, (XtDestructor)NULL);
	app = app->next;
    }
}

/* old interface */
#if NeedFunctionPrototypes
void XtAppAddConverter(
    XtAppContext	app,
    register _Xconst char* from_type,
    register _Xconst char* to_type,
    XtConverter		converter,
    XtConvertArgList    convert_args,
    Cardinal		num_args
    )
#else
void XtAppAddConverter(app, from_type, to_type, converter, convert_args, num_args)
    XtAppContext	app;
    register String	from_type, to_type;
    XtConverter		converter;
    XtConvertArgList    convert_args;
    Cardinal		num_args;
#endif
{
    _XtTableAddConverter(app->converterTable,
	XrmStringToRepresentation(from_type),
        XrmStringToRepresentation(to_type),
	(XtTypeConverter)converter, convert_args, num_args,
	False, XtCacheAll, (XtDestructor)NULL);
}

static CachePtr
CacheEnter(heap, converter, args, num_args, from, to, succeeded, hash,
	   do_ref, do_free, destructor, closure)
    Heap*		    heap;
    register XtTypeConverter converter;
    register XrmValuePtr    args;
    Cardinal		    num_args;
    XrmValuePtr		    from;
    XrmValuePtr		    to;
    Boolean		    succeeded;
    register int	    hash;
    Boolean		    do_ref;
    Boolean		    do_free;
    XtDestructor	    destructor;
    XtPointer		    closure;
{
    register	CachePtr *pHashEntry;
    register	CachePtr p;
    register    Cardinal i;

    pHashEntry = &cacheHashTable[hash & CACHEHASHMASK];

    if ((succeeded && destructor) || do_ref) {
	p = (CachePtr) _XtHeapAlloc(heap, (sizeof(CacheRec) +
					   sizeof(CacheRecExt) +
					   num_args * sizeof(XrmValue)));
	CEXT(p)->prev = pHashEntry;
	CEXT(p)->destructor = succeeded ? destructor : NULL;
	CEXT(p)->closure = closure;
	CEXT(p)->ref_count = 1;
	p->has_ext = True;
    }
    else {
	p = (CachePtr)_XtHeapAlloc(heap, (sizeof(CacheRec) +
					  num_args * sizeof(XrmValue)));
	p->has_ext = False;
    }
    if (!to->addr)
	succeeded = False;
    p->conversion_succeeded = succeeded;
    p->is_refcounted = do_ref;
    p->must_be_freed = do_free;
    p->next	    = *pHashEntry;
    if (p->next && p->next->has_ext)
	CEXT(p->next)->prev = &p->next;

    *pHashEntry     = p;
    p->tag	    = (XtPointer)heap;
    p->hash	    = hash;
    p->converter    = converter;
    p->from.size    = from->size;
    if (from->size <= sizeof(p->from.addr)) {
	p->from_is_value = True;
	XtBCopy(from->addr, &p->from.addr, from->size);
    } else {
	p->from_is_value = False;
	p->from.addr = (XPointer)_XtHeapAlloc(heap, from->size);
	bcopy((char *)from->addr, (char *)p->from.addr, from->size);
    }
    p->num_args = num_args;
    if (num_args) {
	XrmValue *pargs = CARGS(p);
	for (i = 0; i < num_args; i++) {
	    pargs[i].size = args[i].size;
	    pargs[i].addr = (XPointer)_XtHeapAlloc(heap, args[i].size);
	    XtBCopy(args[i].addr, pargs[i].addr, args[i].size);
	}
    }
    p->to.size = to->size;
    if (!succeeded) {
	p->to_is_value = False;
	p->to.addr = NULL;
    } else if (to->size <= sizeof(p->to.addr)) {
	p->to_is_value = True;
	XtBCopy(to->addr, &p->to.addr, to->size);
    } else {
	p->to_is_value = False;
	p->to.addr = (XPointer)_XtHeapAlloc(heap, to->size);
	bcopy((char *)to->addr, (char *)p->to.addr, to->size);
    }
    return p;
}

static void _XtFreeCacheRec();

void _XtCacheFlushTag(app, tag)
    XtAppContext app;
    XtPointer	tag;
{
    int i;
    register CachePtr *prev;
    register CachePtr rec;

    for (i = CACHEHASHSIZE; --i >= 0;) {
	prev = &cacheHashTable[i];
	while (rec = *prev) {
	    if (rec->tag == tag)
		_XtFreeCacheRec(app, rec, prev);
	    else
		prev = &rec->next;
	}
    }
}

#ifdef DEBUG
#include	<stdio.h>

void _XtConverterCacheStats()
{
    register Cardinal i;
    register CachePtr p;
    register Cardinal entries;

    for (i = 0; i < CACHEHASHSIZE; i++) {
	p = cacheHashTable[i];
	if (p) {
	    for (entries = 0; p; p = p->next) {
		entries++;
	    }
	    (void) fprintf(stdout, "Index: %4d  Entries: %d\n", i, entries);
	    for (p = cacheHashTable[i]; p; p = p->next) {
		(void) fprintf(stdout, "    Size: %3d  Refs: %3d  '",
			       p->from.size,
			       p->has_ext ? CEXT(p)->ref_count : 0);
		(void) fprintf(stdout, "'\n");
	    }
	    (void) fprintf(stdout, "\n");
	}
    }
}
#endif /*DEBUG*/

static Boolean ResourceQuarkToOffset(widget_class, name, offset)
    WidgetClass widget_class;
    XrmName     name;
    Cardinal    *offset;
{
    register WidgetClass     wc;
    register Cardinal        i;
    register XrmResourceList res, *resources;

    for (wc = widget_class; wc; wc = wc->core_class.superclass) {
	resources = (XrmResourceList*) wc->core_class.resources;
	for (i = 0; i < wc->core_class.num_resources; i++, resources++) {
	    res = *resources;
	    if (res->xrm_name == name) {
		*offset = -res->xrm_offset - 1;
		return True;
	    }
	} /* for i in resources */
    } /* for wc in widget classes */
    (*offset) = 0;
    return False;
}


static void ComputeArgs(widget, convert_args, num_args, args)
    Widget		widget;
    XtConvertArgList    convert_args;
    Cardinal		num_args;
    XrmValuePtr		args;
{
    register Cardinal   i;
    Cardinal		offset;
    String              params[1];
    Cardinal		num_params = 1;
    Widget		ancestor = NULL;

    for (i = 0; i < num_args; i++) {
	args[i].size = convert_args[i].size;
	switch (convert_args[i].address_mode) {
	case XtAddress: 
	    args[i].addr = convert_args[i].address_id;
	    break;

	case XtBaseOffset:
#if defined(CRAY1) && !defined(__STDC__)
	    args[i].addr =
		(XPointer)((int)widget + (int)convert_args[i].address_id);
#else
	    args[i].addr = (XPointer)((char *)widget + (int)convert_args[i].address_id);
#endif
	    break;

	case XtWidgetBaseOffset:
	    if (!ancestor) {
		if (XtIsWidget(widget))
		    ancestor = widget;
		else
		    ancestor = _XtWindowedAncestor(widget);
	    }

#if defined(CRAY1) && !defined(__STDC__)
	    args[i].addr =
		(XPointer)((int)ancestor + (int)convert_args[i].address_id);
#else
	    args[i].addr =
		(XPointer)((char *)ancestor + (int)convert_args[i].address_id);
#endif
	    break;

	case XtImmediate:
	    args[i].addr = (XPointer) &(convert_args[i].address_id);
	    break;

	case XtProcedureArg:
	    (*(XtConvertArgProc)convert_args[i].address_id)
		(widget, &convert_args[i].size, &args[i]);
	    break;

	case XtResourceString:
	    /* Convert in place for next usage */
	    convert_args[i].address_mode = XtResourceQuark;
	    convert_args[i].address_id =
	       (XtPointer)XrmStringToQuark((String)convert_args[i].address_id);
	    /* Fall through */

	case XtResourceQuark:
	    if (! ResourceQuarkToOffset(widget->core.widget_class,
		    (XrmQuark) convert_args[i].address_id, &offset)) {
		params[0]=
                  XrmQuarkToString((XrmQuark) convert_args[i].address_id);
               XtAppWarningMsg(XtWidgetToApplicationContext(widget),
		    "invalidResourceName","computeArgs",XtCXtToolkitError,
		    "Cannot find resource name %s as argument to conversion",
                     params,&num_params);
		offset = 0;
	    }
#if defined(CRAY1) && !defined(__STDC__)
	    args[i].addr = (XPointer)((int)widget + offset);
#else
	    args[i].addr = (XPointer)((char *)widget + offset);
#endif
	    break;
	default:
	    params[0] = XtName(widget);
	    XtAppWarningMsg(XtWidgetToApplicationContext(widget),
		"invalidAddressMode", "computeArgs", XtCXtToolkitError,
		"Conversion arguments for widget '%s' contain an unsupported address mode",
			params,&num_params);
	    args[i].addr = NULL;
	    args[i].size = 0;
	} /* switch */
    } /* for */
} /* ComputeArgs */

void XtDirectConvert(converter, args, num_args, from, to)
    XtConverter     converter;
    XrmValuePtr     args;
    Cardinal	    num_args;
    register XrmValuePtr from;
    XrmValuePtr     to;
{
    register CachePtr   p;
    register int	hash;
    register Cardinal   i;

    /* Try to find cache entry for conversion */
    hash = ((int)(converter) >> 2) + from->size + *((char *) from->addr);
    if (from->size > 1) hash += ((char *) from->addr)[1];
    
    for (p = cacheHashTable[hash & CACHEHASHMASK]; p; p = p->next) {
	if ((p->hash == hash)
	 && (p->converter == (XtTypeConverter)converter)
	 && (p->from.size == from->size)
	 && !(p->from_is_value ?
	      XtBCmp(&p->from.addr, from->addr, from->size) :
	      bcmp((char *)p->from.addr, (char *)from->addr, from->size))
         && (p->num_args == num_args)) {
	    if (i = num_args) {
		XrmValue *pargs = CARGS(p);
		/* Are all args the same data ? */
		while (i) {
		    i--; /* do not move to while test, broken compilers */
		    if (pargs[i].size != args[i].size ||
			XtBCmp(pargs[i].addr, args[i].addr, args[i].size)) {
			i++;
			break;
		    }
		}
	    }
	    if (!i) {
		/* Perfect match */
		to->size = p->to.size;
		if (p->to_is_value)
		    to->addr = (XPointer)&p->to.addr;
		else
		    to->addr = p->to.addr;
		return;
	    }
	}
    }

    /* Didn't find it, call converter procedure and entry result in cache */
    (*to).size = 0;
    (*to).addr = NULL;
    (*converter)(args, &num_args, from, to);
    /* This memory can never be freed since we don't know the Display
     * or app context from which to compute the persistance */
    {
	CacheEnter(&globalHeap, (XtTypeConverter)converter, args, num_args,
		   from, to, (to->addr != NULL), hash, False, False,
		   (XtDestructor)NULL, NULL);
    }
}


static ConverterPtr GetConverterEntry( app, converter )
    XtAppContext app;
    XtTypeConverter converter;
{
    int entry;
    register ConverterPtr cP;
    ConverterTable converterTable = app->converterTable;
    cP = NULL;
    for (entry = 0; (entry < CONVERTHASHSIZE) && !cP; entry++) {
	cP = converterTable[entry];
	while (cP && (cP->converter != converter)) cP = cP->next;
    }
    return cP;
}


static Boolean
_XtCallConverter(dpy, converter,
		 args, num_args, from, to, cache_ref_return, cP)
    Display*	    dpy;
    XtTypeConverter converter;
    XrmValuePtr     args;
    Cardinal	    num_args;
    register XrmValuePtr from;
    XrmValuePtr     to;
    XtCacheRef	    *cache_ref_return;
    register ConverterPtr cP;
{
    register CachePtr   p;
    register int	hash;
    register Cardinal   i;

    if (!cP || ((cP->cache_type == XtCacheNone) && !cP->destructor)) {
	XtPointer closure;
	if (cache_ref_return) *cache_ref_return = NULL;
	return (*(XtTypeConverter)converter)
	    (dpy, args, &num_args, from, to, &closure);
    }

    /* Try to find cache entry for conversion */
    hash = ((int)(converter) >> 2) + from->size + *((char *) from->addr);
    if (from->size > 1) hash += ((char *) from->addr)[1];
    
    if (cP->cache_type != XtCacheNone) {
	for (p = cacheHashTable[hash & CACHEHASHMASK]; p; p = p->next){
	    if ((p->hash == hash)
	     && (p->converter == converter)
	     && (p->from.size == from->size)
	     && !(p->from_is_value ?
		  XtBCmp(&p->from.addr, from->addr, from->size) :
		  bcmp((char *)p->from.addr, (char *)from->addr, from->size))
	     && (p->num_args == num_args)) {
		if (i = num_args) {
		    XrmValue *pargs = CARGS(p);
		    /* Are all args the same data ? */
		    while (i) {
			i--; /* do not move to while test, broken compilers */
			if (pargs[i].size != args[i].size ||
			    XtBCmp(pargs[i].addr, args[i].addr, args[i].size)){
			    i++;
			    break;
			}
		    }
		}
		if (!i) {
		    /* Perfect match */
		    if (p->conversion_succeeded) {
			if (to->addr) {	/* new-style call */
			    if (to->size < p->to.size) {
				to->size = p->to.size;
				return False;
			    }
			    to->size = p->to.size;
			    if (p->to_is_value) {
				XtBCopy(&p->to.addr, to->addr, to->size);
			    } else {
				bcopy((char *)p->to.addr, (char *)to->addr,
				      to->size);
			    }
			} else {	/* old-style call */
			    to->size = p->to.size;
			    if (p->to_is_value)
				to->addr = (XPointer)&p->to.addr;
			    else
				to->addr = p->to.addr;
			}
		    }
		    if (p->is_refcounted) {
			CEXT(p)->ref_count++;
			if (cache_ref_return)
			    *cache_ref_return = (XtCacheRef)p;
			else
			    p->is_refcounted = False;
		    }
		    else {
			if (cache_ref_return)
			    *cache_ref_return = NULL;
		    }
		    return (p->conversion_succeeded);
		}
	    }
	}
    }

    /* No cache entry, call converter procedure and enter result in cache */
    {
	Heap *heap;
	XtPointer closure = NULL;
	unsigned int supplied_size = to->size;
	Boolean do_ref = cP->do_ref_count && cache_ref_return;
	Boolean do_free = False;
	Boolean retval =
	    (*(XtTypeConverter)converter)(dpy, args, &num_args, from, to, &closure);

	if (retval == False && supplied_size < to->size) {
	    /* programmer error: caller must allocate sufficient storage */
	    *cache_ref_return = NULL;
	    return False;
	}

	if ((cP->cache_type == XtCacheNone) || do_ref) {
	    heap = NULL;
	    do_free = True;
	}
	else if (cP->cache_type == XtCacheByDisplay)
	    heap = &_XtGetPerDisplay(dpy)->heap;
	else
	    heap = &XtDisplayToApplicationContext(dpy)->heap;

	p = CacheEnter(heap, converter, args, num_args, from, to, retval,
		       hash, do_ref, do_free, cP->destructor, closure);
	if (do_ref)
	    *cache_ref_return = (XtCacheRef)p;
	else if (cache_ref_return)
	    *cache_ref_return = NULL;

	return retval;
    }
}

Boolean
XtCallConverter(dpy, converter, args, num_args, from, to, cache_ref_return)
    Display*	    dpy;
    XtTypeConverter converter;
    XrmValuePtr     args;
    Cardinal	    num_args;
    register XrmValuePtr from;
    XrmValuePtr     to;
    XtCacheRef	    *cache_ref_return;
{
    ConverterPtr cP;

    cP = GetConverterEntry( XtDisplayToApplicationContext(dpy), converter );
    return _XtCallConverter(dpy, converter, args, num_args, from, to, 
			    cache_ref_return, cP);
}

Boolean _XtConvert(widget, from_type, from, to_type, to, cache_ref_return)
             Widget		widget;
    register XrmRepresentation	from_type;
	     XrmValuePtr	from;
    register XrmRepresentation	to_type;
    register XrmValuePtr	to;
    XtCacheRef			*cache_ref_return;
{
    XtAppContext	app = XtWidgetToApplicationContext(widget);
    register ConverterPtr	p;
    Cardinal		num_args;
    XrmValue		*args;

    /* Look for type converter */
    p = app->converterTable[ProcHash(from_type, to_type) & CONVERTHASHMASK];
    for (; p; p = p->next) {
	if (from_type == p->from && to_type == p->to) {
	    Boolean retval = False;
	    /* Compute actual arguments from widget and arg descriptor */
	    num_args = p->num_args;
	    if (num_args != 0) {
		args = (XrmValue*)
		    ALLOCATE_LOCAL( num_args * sizeof (XrmValue) );
		if (!args) _XtAllocError("alloca");
		ComputeArgs(widget, ConvertArgs(p), num_args, args);
	    } else args = NULL;
	    if (p->new_style) {
		retval =
		    _XtCallConverter(XtDisplayOfObject(widget),
				     p->converter, args, num_args,
				     from, to, cache_ref_return, p);
	    }
	    else { /* is old-style (non-display) converter */
		XrmValue tempTo;
		XtDirectConvert((XtConverter)p->converter, args, num_args,
				from, &tempTo);
		if (cache_ref_return)
		    *cache_ref_return = NULL;
		if (tempTo.addr) {
		    if (to->addr) {	/* new-style caller */
			if (to->size >= tempTo.size) {
			    if (to_type == _XtQString)
				*(String*)(to->addr) = tempTo.addr;
			    else {
				XtBCopy(tempTo.addr, to->addr, tempTo.size);
			    }
			    retval = True;
			}
			to->size = tempTo.size;
		    } else {		/* old-style caller */
			*to = tempTo;
			retval = True;
		    } 
		}
	    }
	    if (args) DEALLOCATE_LOCAL( (XtPointer)args );
	    return retval;
	}
    }

    {
	String params[2];
	Cardinal num_params = 2;
	params[0] = XrmRepresentationToString(from_type);
	params[1] = XrmRepresentationToString(to_type);
	XtAppWarningMsg(app, "typeConversionError", "noConverter", XtCXtToolkitError,
	     "No type converter registered for '%s' to '%s' conversion.",
             params, &num_params);
    }
    return False;
}

#if NeedFunctionPrototypes
void XtConvert(
    Widget	widget,
    _Xconst char* from_type_str,
    XrmValuePtr	from,
    _Xconst char* to_type_str,
    XrmValuePtr	to
    )
#else
void XtConvert(widget, from_type_str, from, to_type_str, to)
    Widget	widget;
    String	from_type_str;
    XrmValuePtr	from;
    String	to_type_str;
    XrmValuePtr	to;
#endif
{
    XrmQuark    from_type, to_type;

    from_type = XrmStringToRepresentation(from_type_str);
    to_type = XrmStringToRepresentation(to_type_str);
    if (from_type != to_type) {
	/*  It's not safe to ref count these resources, 'cause we
	    don't know what older clients may have assumed about
	    the resource lifetimes.
	XtCacheRef ref;
	*/
	to->addr = NULL;
	to->size = 0;
	_XtConvert(widget, from_type, from, to_type, to, /*&ref*/ NULL);
	/*
	if (ref) {
	    XtAddCallback( widget, XtNdestroyCallback,
			   XtCallbackReleaseCacheRef, (XtPointer)ref );
	}
	*/
    }
    else
	(*to) = *from;
}

#if NeedFunctionPrototypes
Boolean XtConvertAndStore(
    Widget	object,
    _Xconst char* from_type_str,
    XrmValuePtr	from,
    _Xconst char* to_type_str,
    XrmValuePtr	to
    )
#else
Boolean XtConvertAndStore(object, from_type_str, from, to_type_str, to)
    Widget	object;
    String	from_type_str;
    XrmValuePtr	from;
    String	to_type_str;
    XrmValuePtr	to;
#endif
{
    XrmQuark    from_type, to_type;

    from_type = XrmStringToRepresentation(from_type_str);
    to_type = XrmStringToRepresentation(to_type_str);
    if (from_type != to_type) {
	static XtPointer local_valueP = NULL;
	static Cardinal local_valueS = 128;
	XtCacheRef ref;
	Boolean local = False;
	do {
	    if (!to->addr) {
		if (!local_valueP)
		    local_valueP = _XtHeapAlloc(&globalHeap, local_valueS);
		to->addr = local_valueP;
		to->size = local_valueS;
		local = True;
	    }
	    if (!_XtConvert(object, from_type, from, to_type, to, &ref)) {
		if (local && (to->size > local_valueS)) {
		    to->addr =
			local_valueP = _XtHeapAlloc(&globalHeap, to->size);
		    local_valueS = to->size;
		    continue;
		} else {
		    if (local) {
			to->addr = NULL;
			to->size = 0;
		    }
		    return False;
		}
	    }
	    if (ref) {
		XtAddCallback( object, XtNdestroyCallback,
			       XtCallbackReleaseCacheRef, (XtPointer)ref );
	    }
	    return True;
	} while (local /* && local_valueS < to->size */);
    }
    if (to->addr) {
	if (to->size < from->size) {
	    to->size = from->size;
	    return False;
	}
	bcopy( from->addr, to->addr, from->size );
	to->size = from->size;
    } else			/* from_type == to_type */
	*to = *from;

    return True;
}

static void _XtFreeCacheRec(app, p, prev)
    XtAppContext app;
    CachePtr p;
    CachePtr *prev;
{
    if (p->has_ext) {
	if (CEXT(p)->destructor) {
	    Cardinal num_args = p->num_args;
	    XrmValue *args = NULL;
	    XrmValue toc;
	    if (num_args)
		args = CARGS(p);
	    toc.size = p->to.size;
	    if (p->to_is_value)
		toc.addr = (XPointer)&p->to.addr;
	    else
		toc.addr = p->to.addr;
	    (*CEXT(p)->destructor) (app, &toc, CEXT(p)->closure, args,
				    &num_args);
	}
	*(CEXT(p)->prev) = p->next;
	if (p->next && p->next->has_ext)
	    CEXT(p->next)->prev = CEXT(p)->prev;
    } else {
	*prev = p->next;
    }
    if (p->must_be_freed) {
	register int i;
	if (!p->from_is_value)
	    XtFree(p->from.addr);
	if (i = p->num_args) {
	    XrmValue *pargs = CARGS(p);
	    while (i--)
		XtFree(pargs[i].addr);
	}
	if (!p->to_is_value)
	    XtFree(p->to.addr);
	XtFree((char*)p);
    }
    /* else on private heap; will free entire heap later */
}

void XtAppReleaseCacheRefs(app, refs)
    XtAppContext app;
    XtCacheRef *refs;
{
    register CachePtr *r;
    register CachePtr p;

    for (r = (CachePtr*)refs; p = *r; r++) {
	if (p->is_refcounted && --(CEXT(p)->ref_count) == 0) {
	    _XtFreeCacheRec(app, p, NULL);
	}
    }
}


/* ARGSUSED */
void XtCallbackReleaseCacheRefList(widget, closure, call_data)
    Widget widget;		/* unused */
    XtPointer closure;
    XtPointer call_data;	/* unused */
{
    XtAppReleaseCacheRefs( XtWidgetToApplicationContext(widget),
			   (XtCacheRef*)closure );
    XtFree(closure);
}


/* ARGSUSED */
void XtCallbackReleaseCacheRef(widget, closure, call_data)
    Widget widget;		/* unused */
    XtPointer closure;
    XtPointer call_data;	/* unused */
{
    XtCacheRef cache_refs[2];
    cache_refs[0] = (XtCacheRef)closure;
    cache_refs[1] = NULL;
    XtAppReleaseCacheRefs( XtWidgetToApplicationContext(widget), cache_refs );
}
