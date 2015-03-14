/* $XConsortium: Varargs.c,v 1.24 91/06/13 18:00:58 converse Exp $ */

/*

Copyright 1985, 1986, 1987, 1988, 1989 by the
Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/

#include "IntrinsicI.h"
#include "VarargsI.h"
#include "StringDefs.h"

static String XtNxtConvertVarToArgList = "xtConvertVarToArgList";

/*
 *    Given a nested list, _XtCountNestedList() returns counts of the
 *    total number of attribute-value pairs and the count of those
 *    attributes that are typed. The list is counted recursively.
 */
static  void
_XtCountNestedList(avlist, total_count, typed_count)
    XtTypedArgList  avlist;
    int             *total_count;
    int             *typed_count;
{
    for (; avlist->name != NULL; avlist++) {
        if (strcmp(avlist->name, XtVaNestedList) == 0) {
            _XtCountNestedList((XtTypedArgList)avlist->value, total_count,
                	       typed_count);
        } else {
            if (avlist->type != NULL) {
                ++(*typed_count);
            }
            ++(*total_count);
        }
    }    
}


/*
 *    Given a variable length attribute-value list, _XtCountVaList()
 *    returns counts of the total number of attribute-value pairs,
 *    and the count of the number of those attributes that are typed.
 *    The list is counted recursively.
 */
#if NeedFunctionPrototypes
void
_XtCountVaList(va_list var, int* total_count, int* typed_count)
#else
void
_XtCountVaList(var, total_count, typed_count)
    va_list     var;
    int         *total_count;
    int         *typed_count;
#endif
{
    String          attr;
    
    *total_count = 0;
    *typed_count = 0;
 
    for(attr = va_arg(var, String) ; attr != NULL;
                        attr = va_arg(var, String)) {
        if (strcmp(attr, XtVaTypedArg) == 0) {
            va_arg(var, String);
            va_arg(var, String);
            va_arg(var, XtArgVal);
            va_arg(var, int);
            ++(*total_count);
            ++(*typed_count);
        } else if (strcmp(attr, XtVaNestedList) == 0) {
            _XtCountNestedList(va_arg(var, XtTypedArgList), total_count,
                typed_count);
        } else {
            va_arg(var, XtArgVal);
            ++(*total_count);
	}
    }
}


/* 
 *   Given a variable length attribute-value list, XtVaCreateArgsList()
 *   constructs an attribute-value list of type XtTypedArgList and 
 *   returns the list.
 */
#if NeedVarargsPrototypes
XtVarArgsList
XtVaCreateArgsList(XtPointer unused, ...)
#else
/*ARGSUSED*/
/*VARARGS1*/
XtVarArgsList XtVaCreateArgsList(unused, va_alist)
    XtPointer unused;
    va_dcl
#endif
{
    va_list var;
    XtTypedArgList  avlist;
    int		    count = 0;
    String	    attr;

    /*
     * Count the number of attribute-value pairs in the list.
     * Note: The count is required only to allocate enough space to store
     * the list. Therefore nested lists are not counted recursively.
     */
    Va_start(var,unused);
    for(attr = va_arg(var, String) ; attr != NULL;
                        attr = va_arg(var, String)) {
        ++count;
        if (strcmp(attr, XtVaTypedArg) == 0) {
            va_arg(var, String);
            va_arg(var, String);
            va_arg(var, XtArgVal);
            va_arg(var, int);
        } else {
            va_arg(var, XtArgVal);
        }
    }
    va_end(var);

    Va_start(var,unused);
    avlist = _XtVaCreateTypedArgList(var, count);
    va_end(var);
    return (XtVarArgsList)avlist;
}


#if NeedFunctionPrototypes
XtTypedArgList _XtVaCreateTypedArgList(va_list var, register int count)
#else
XtTypedArgList _XtVaCreateTypedArgList(var, count)
    va_list    	    var;     
    register int    count;
#endif
{
    String	    attr;
    XtTypedArgList  avlist;

    avlist = (XtTypedArgList)
		XtCalloc((int)count + 1, (unsigned)sizeof(XtTypedArg));

    for(attr = va_arg(var, String), count = 0; attr != NULL; 
		attr = va_arg(var, String)) {
	if (strcmp(attr, XtVaTypedArg) == 0) {
	    avlist[count].name = va_arg(var, String);
	    avlist[count].type = va_arg(var, String);
	    avlist[count].value = va_arg(var, XtArgVal);
	    avlist[count].size = va_arg(var, int);
	} else {
	    avlist[count].name = attr;
	    avlist[count].type = NULL;
	    avlist[count].value = va_arg(var, XtArgVal);
	}
	++count;
    }
    avlist[count].name = NULL;

    return avlist;
}


/*
 *    _XtTypedArgToArg() invokes a resource converter to convert the
 *    passed typed arg into a name/value pair and stores the name/value
 *    pair in the passed Arg structure. It returns 1 if the conversion
 *    succeeded and 0 if the conversion failed.
 */
static int
_XtTypedArgToArg(widget, typed_arg, arg_return, resources, num_resources)
    Widget              widget;
    XtTypedArgList      typed_arg;
    ArgList             arg_return;
    XtResourceList      resources;
    Cardinal            num_resources;
{     
    String              to_type = NULL;
    XrmValue            from_val, to_val;
    register int        i;
      

    if (widget == NULL) {
        XtAppWarningMsg(XtWidgetToApplicationContext(widget),
            "nullWidget", XtNxtConvertVarToArgList, XtCXtToolkitError,
	    "XtVaTypedArg conversion needs non-NULL widget handle",
            (String *)NULL, (Cardinal *)NULL);
        return(0);
    }
       
    /* again we assume that the XtResourceList is un-compiled */

    for (i = 0; i < num_resources; i++) {
        if (StringToName(typed_arg->name) ==
            StringToName(resources[i].resource_name)) {
            to_type = resources[i].resource_type;
            break;
        }
    }

    if (to_type == NULL) {
        XtAppWarningMsg(XtWidgetToApplicationContext(widget),
            "unknownType", XtNxtConvertVarToArgList, XtCXtToolkitError,
            "Unable to find type of resource for conversion",
            (String *)NULL, (Cardinal *)NULL);
        return(0);
    }
       
    to_val.addr = NULL;
    from_val.size = typed_arg->size;
    if ((strcmp(typed_arg->type, XtRString) == 0) ||
            (typed_arg->size > sizeof(XtArgVal))) {
        from_val.addr = (XPointer)typed_arg->value;
    } else {
            from_val.addr = (XPointer)&typed_arg->value;
    }
       
    XtConvert(widget, typed_arg->type, &from_val, to_type, &to_val);
 
    if (to_val.addr == NULL) {
        XtAppWarningMsg(XtWidgetToApplicationContext(widget),
            "conversionFailed", XtNxtConvertVarToArgList, XtCXtToolkitError,
            "Type conversion failed", (String *)NULL, (Cardinal *)NULL);
        return(0);
    }

    arg_return->name = typed_arg->name;

    if (strcmp(to_type, XtRString) == 0) {
	arg_return->value = (XtArgVal) to_val.addr;
    }
    else {
	if (to_val.size == sizeof(long))
	    arg_return->value = (XtArgVal) *(long *)to_val.addr;
	else if (to_val.size == sizeof(short))
	    arg_return->value = (XtArgVal) *(short *)to_val.addr;
	else if (to_val.size == sizeof(char))
	    arg_return->value = (XtArgVal) *(char *)to_val.addr;
	else if (to_val.size == sizeof(XtArgVal))
	    arg_return->value = *(XtArgVal *)to_val.addr;
    }
       
    return(1);
}


/*
 *    _XtNestedArgtoArg() converts the passed nested list into
 *    an ArgList/count.
 */
static int
_XtNestedArgtoArg(widget, avlist, args, resources, num_resources)
    Widget              widget;
    XtTypedArgList      avlist;
    ArgList             args;
    XtResourceList      resources;
    Cardinal            num_resources;
{
    int         count = 0;
 
    for (; avlist->name != NULL; avlist++) {
        if (avlist->type != NULL) {
            /* If widget is NULL, the typed arg is ignored */
            if (widget != NULL) {
                /* this is a typed arg */
                count += _XtTypedArgToArg(widget, avlist, (args+count),
                             resources, num_resources);
            }
        } else if (strcmp(avlist->name, XtVaNestedList) == 0) {
            count += _XtNestedArgtoArg(widget, (XtTypedArgList)avlist->value,
                        (args+count), resources, num_resources);
        } else {
            (args+count)->name = avlist->name;
            (args+count)->value = avlist->value;
            ++count;
        }
    }

    return(count);
}

static void		GetResources();

 
/* 
 *    Given a variable argument list, _XtVaToArgList() returns the 
 *    equivalent ArgList and count. _XtVaToArgList() handles nested 
 *    lists and typed arguments. 
 */
#if NeedFunctionPrototypes
void
_XtVaToArgList(
    Widget		widget,
    va_list     	var,
    int			max_count,
    ArgList		*args_return,
    Cardinal		*num_args_return)
#else
void
_XtVaToArgList(widget, var, max_count, args_return, num_args_return)
    Widget		widget;
    va_list     	var;
    int			max_count;
    ArgList		*args_return;
    Cardinal		*num_args_return;
#endif
{
    String		attr;
    int			count = 0;
    ArgList		args = (ArgList)NULL;
    XtTypedArg		typed_arg;
    XtResourceList	resources = (XtResourceList)NULL;
    Cardinal		num_resources;
    Boolean		fetched_resource_list = False;

    if (max_count  == 0) {
	*num_args_return = 0;
	*args_return = (ArgList)NULL;
	return;
    }


    args = (ArgList)XtMalloc((unsigned)(max_count * sizeof(Arg)));

    for(attr = va_arg(var, String) ; attr != NULL;
			attr = va_arg(var, String)) {
	if (strcmp(attr, XtVaTypedArg) == 0) {
	    typed_arg.name = va_arg(var, String);
	    typed_arg.type = va_arg(var, String);
	    typed_arg.value = va_arg(var, XtArgVal);
	    typed_arg.size = va_arg(var, int);

	    /* if widget is NULL, typed args are ignored */
	    if (widget != NULL) {
		if (!fetched_resource_list) {
		    GetResources(widget, &resources, &num_resources);
		    fetched_resource_list = True;
		}
		count += _XtTypedArgToArg(widget, &typed_arg, &args[count],
			     resources, num_resources);
	    }
	} else if (strcmp(attr, XtVaNestedList) == 0) {
	    if (widget != NULL || !fetched_resource_list) {
		GetResources(widget, &resources, &num_resources);
		fetched_resource_list = True;
	    }

	    count += _XtNestedArgtoArg(widget, va_arg(var, XtTypedArgList),
			&args[count], resources, num_resources);
	} else {
	    args[count].name = attr;
	    args[count].value = va_arg(var, XtArgVal);
	    count ++;
	}
    }

    XtFree((XtPointer)resources);

    *num_args_return = (Cardinal)count;
    *args_return = (ArgList)args;
}

/*	Function Name: GetResources
 *	Description: Retreives the normal and constraint resources
 *                   for this widget.
 *	Arguments: widget - the widget.
 * RETURNED        res_list - the list of resource for this widget
 * RETURNED        number - the number of resources in the above list.
 *	Returns: none
 */

static void
GetResources(widget, res_list, number)
Widget widget;
XtResourceList * res_list;
Cardinal * number;
{
    Widget parent = XtParent(widget);

    XtInitializeWidgetClass(XtClass(widget));
    XtGetResourceList(XtClass(widget), res_list, number);
    
    /* assert: !XtIsShell(w) => (XtParent(w) != NULL) */
    if (!XtIsShell(widget) && XtIsConstraint(parent)) {
	XtResourceList res, constraint, cons_top;
	Cardinal num_constraint, temp;

	XtGetConstraintResourceList(XtClass(parent), &constraint, 
				    &num_constraint);

	cons_top = constraint;
	*res_list = (XtResourceList) XtRealloc((char*)*res_list, 
					       ((*number + num_constraint) * 
						sizeof(XtResource)));

	for (temp= num_constraint, res= *res_list + *number; temp != 0; temp--)
	    *res++ = *constraint++;

	*number += num_constraint;
	XtFree( (XtPointer) cons_top);
    }
}

static int _XtNestedArgtoTypedArg(args, avlist) 
    XtTypedArgList      args;
    XtTypedArgList      avlist;
{    
    int         count = 0;
     
    for (; avlist->name != NULL; avlist++) { 
        if (avlist->type != NULL) { 
            (args+count)->name = avlist->name; 
            (args+count)->type = avlist->type; 
            (args+count)->size = avlist->size;
            (args+count)->value = avlist->value;
            ++count; 
        } else if(strcmp(avlist->name, XtVaNestedList) == 0) {             
            count += _XtNestedArgtoTypedArg((args+count),  
                            (XtTypedArgList)avlist->value); 
        } else {                             
            (args+count)->name = avlist->name; 
	    (args+count)->type = NULL;
            (args+count)->value = avlist->value; 
            ++count;
        }                                     
    }         
    return(count);
}


/*
 *    Given a variable argument list, _XtVaToTypedArgList() returns 
 *    the equivalent TypedArgList. _XtVaToTypedArgList() handles nested
 *    lists.
 *    Note: _XtVaToTypedArgList() does not do type conversions.
 */
#if NeedFunctionPrototypes
void
_XtVaToTypedArgList(
    va_list             var,
    int			max_count,
    XtTypedArgList   	*args_return,
    Cardinal            *num_args_return)
#else
void
_XtVaToTypedArgList(var, max_count, args_return, num_args_return)
    va_list             var;
    int			max_count;
    XtTypedArgList   	*args_return;
    Cardinal            *num_args_return;
#endif
{
    XtTypedArgList	args = NULL;
    String              attr;
    int			count;

    args = (XtTypedArgList)
	XtMalloc((unsigned)(max_count * sizeof(XtTypedArg))); 

    for(attr = va_arg(var, String), count = 0 ; attr != NULL;
		    attr = va_arg(var, String)) {
        if (strcmp(attr, XtVaTypedArg) == 0) {
	    args[count].name = va_arg(var, String);
	    args[count].type = va_arg(var, String);
	    args[count].value = va_arg(var, XtArgVal);
	    args[count].size = va_arg(var, int);
	    ++count;
	} else if (strcmp(attr, XtVaNestedList) == 0) {
   	    count += _XtNestedArgtoTypedArg(&args[count], 
			va_arg(var, XtTypedArgList));
	} else {
	    args[count].name = attr;
	    args[count].type = NULL;
	    args[count].value = va_arg(var, XtArgVal);
	    ++count;
	}
    }

    *args_return = args;
    *num_args_return = count;
}
