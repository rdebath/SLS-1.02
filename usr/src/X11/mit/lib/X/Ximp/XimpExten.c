/* $XConsortium: XimpExten.c,v 1.6 92/07/29 10:15:43 rws Exp $ */
/******************************************************************

    Copyright 1991, 1992 by FUJITSU LIMITED.
    Copyright 1991, 1992 by Sun Microsystems, Inc.
    Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of FUJITSU LIMITED, Sun
Microsystems, Inc. and Sony Corporation not be used in advertising
or publicity pertaining to distribution of the software without
specific, written prior permission. FUJITSU LIMITED, Sun Microsystems,
Inc. and Sony Corporation make no representations about the suitability
of this software for any purpose.  It is provided "as is" without
express or implied warranty.

FUJITSU LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION DISCLAIMS
ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL FUJITSU
LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION BE LIABLE FOR
ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Author: Takashi Fujiwara     FUJITSU LIMITED
        Hideki Hiura         Sun Microsystems, Inc.
        Makoto Wakamatsu     Sony Corporation
******************************************************************/

#define NEED_EVENTS
#include <X11/keysym.h>
#include <X11/Xlib.h>
#include "Xlibint.h"
#include "Xlcint.h"
#include "Ximplc.h"

#define		XIM_UNKNOWN_KEYSYM	0x77777777
#define		XIM_UNDETERMINED	0x77777776

typedef int     XIMTextVisiblePosType;

/*
 * LookupChoice Region
 */

typedef enum {
    XIMCB_Success,
    XIMCB_FatalError
}               XIMCBResult;


typedef enum {
    DrawUpHorizontally = 0,
    DrawUpVertically = 1
}               DrawUpDirection;

typedef enum {
    XIMOwnsLabel = 0,
    CBOwnsLabel = 1
}               WhoOwnsLabel;

typedef struct {
    int             choice_per_window;	/* Number of choices can be display
					 * in the region */
    int             nrows;
    int             ncolumns;
    DrawUpDirection DrawUpDirection;
    WhoOwnsLabel    WhoOwnsLabel;	/* For callback to tell XIM whether
					 * it wants to control what label
					 * should be for the choices. */
}               LayoutInfo;

typedef enum {
    HasNotBeenNegotiated = 0,
    XIMIsMaster = 1,
    CBIsMaster = 2
} WhoIsMaster;

typedef struct _XIMLookupStartCallbackStruct {
    XKeyEvent      *event;
    WhoIsMaster     WhoIsMaster;/* For callback to tell whether is going to
				 * be a master */
    LayoutInfo     *XIMPreference;
    LayoutInfo     *CBPreference;
}               XIMLookupStartCallbackStruct;

typedef struct _XIMChoiceObject {
    XIMText        *label;
    XIMText        *value;
}               XIMChoiceObject;

typedef struct _XIMLookupDrawCallbackStruct {
    XIMChoiceObject *choices;	/* the lookup choices */
    int             n_choices;	/* Total number of lookup choices */
    int             max_len;	/* Max number of characters per choice item */
    int             index_of_first_candidate;
    int             index_of_last_candidate;
}               XIMLookupDrawCallbackStruct;

typedef struct _XIMLookupProcessCallbackStruct {
    XKeyEvent      *event;
    int             index_of_choice_selected;	/* Set by callback for the
						 * selected choice.
						 * XIM_UNKNOW_KEYSYM and
						 * XIM_UNDETERMINED are also
						 * possible value. */
}               XIMLookupProcessCallbackStruct;

extern Ximp_XIC _Ximp_LookupXIC();
extern void     _Ximp_IM_SendMessage();
extern void 	_Ximp_ProcError();

#define Private static
/*
 * Ximp extentions to XIC attributes
 */
#define XNExtXimp_Backfront             "XNExtXimp_Backfront"
#define XNExtXimp_Statuswindow          "XNExtXimp_Statuswindow"
#define XNExtXimp_Conversion	        "XNExtXimp_Conversion"
#define XNExtXimp_Error		        "XNExtXimp_Error"
#define XNExtXimp_LookupAttributes	"XNExtXimp_LookupAttributes"
#define XNExtXimp_LookupStartCallback   "XNExtXimp_LookupStartCallback"
#define XNExtXimp_LookupDrawCallback    "XNExtXimp_LookupDrawCallback"
#define XNExtXimp_LookupDoneCallback    "XNExtXimp_LookupDoneCallback"
#define XNExtXimp_LookupProcessCallback "XNExtXimp_LookupProcessCallback"
#define XNExtXimp_AuxAttributes		"XNExtXimp_AuxAttributes"
#define XNExtXimp_AuxStartCallback      "XNExtXimp_AuxStartCallback"
#define XNExtXimp_AuxDrawCallback       "XNExtXimp_AuxDrawCallback"
#define XNExtXimp_AuxProcessCallback    "XNExtXimp_AuxProcessCallback"
#define XNExtXimp_AuxDoneCallback       "XNExtXimp_AuxDoneCallback"
#define XNExtXimp_LookupBegin	        "XNExtXimp_LookupBegin"
#define XNExtXimp_RestartCallback	"XNExtXimp_RestartCallback"
#define XNExtXimp_DestroyCallback	"XNExtXimp_DestroyCallback"

/*
 * Ximp properties for extented XIC attribute
 */
#define XIMP_EXT_XIMP_CONVERSION           "_XIMP_EXT_XIMP_CONVERSION"
#define XIMP_EXT_XIMP_BACK_FRONT           "_XIMP_EXT_XIMP_BACK_FRONT"
#define XIMP_EXT_XIMP_STATUSWINDOW         "_XIMP_EXT_XIMP_STATUSWINDOW"
#define XIMP_EXT_XIMP_ERROR		   "_XIMP_EXT_XIMP_ERROR"
#define XIMP_EXT_XIMP_AUXSTARTCALLBACK     "_XIMP_EXT_XIMP_AUXSTARTCALLBACK"
#define XIMP_EXT_XIMP_AUXDRAWCALLBACK      "_XIMP_EXT_XIMP_AUXDRAWCALLBACK"
#define XIMP_EXT_XIMP_AUXPROCESSCALLBACK   "_XIMP_EXT_XIMP_AUXPROCESSCALLBACK"
#define XIMP_EXT_XIMP_AUXDONECALLBACK      "_XIMP_EXT_XIMP_AUXDONECALLBACK"
#define XIMP_EXT_XIMP_LOOKUPSTARTCALLBACK  "_XIMP_EXT_XIMP_LOOKUPSTARTCALLBACK"
#define XIMP_EXT_XIMP_LOOKUPDRAWCALLBACK   "_XIMP_EXT_XIMP_LOOKUPDRAWCALLBACK"
#define XIMP_EXT_XIMP_LOOKUPDONECALLBACK   "_XIMP_EXT_XIMP_LOOKUPDONECALLBACK"
#define XIMP_EXT_XIMP_LOOKUPPROCESSCALLBACK "_XIMP_EXT_XIMP_LOOKUPPROCESSCALLBACK"

#define XIMP_EXT_XIMP_LOOKUPCHOICES        "_XIMP_EXT_XIMP_LOOKUPCHOICES"
/*
 * Ximp extentions to IM attributes
 */
#define XIMLookupCallbacks	0x4000000L
#define XIMAuxCallbacks		0x8000000L

/*
 * Possible values of XSetICValues(XNExtXimp_Conversion, XXX );
 */
#define	XIMEnable	1
#define	XIMDisable	0

/*
 * Possible values of XSetICValues(XNExtXimp_Backfront, XXX );
 */
#define	IMServBackend	1
#define	IMServFrontend  0

typedef enum {
    XICOpCreate = 1,
    XICOpSet = 2,
    XICOpGet = 3
}               XICOp_t;

/*
 * Declaration of the entry functions for each extensions.
 */
Private int     ximp_ext_backfront();
Private int     ximp_ext_conversion();
Private int     ximp_ext_statuswindow();
Private int     ximp_ext_error();
Private int     ximp_ext_lookup_begin();
Private int     ximp_ext_lookup_start_callback();
Private int     ximp_ext_lookup_draw_callback();
Private int     ximp_ext_lookup_done_callback();
Private int     ximp_ext_lookup_process_callback();
Private int     ximp_ext_aux_start_callback();
Private int     ximp_ext_aux_draw_callback();
Private int     ximp_ext_aux_process_callback();
Private int     ximp_ext_aux_done_callback();
Private int     ximp_ext_restart_callback();
Private int     ximp_ext_destroy_callback();
Private int	nested_list();

/* If you need to extend IC attributes, please add function here */

typedef struct {
    char           *name;
    int             (*func) ();
}               icop_t;

Private
icop_t icoptbl[] = {
    XNExtXimp_Backfront, ximp_ext_backfront,
	XNExtXimp_Conversion, ximp_ext_conversion,
	XNExtXimp_Statuswindow, ximp_ext_statuswindow,
	XNExtXimp_Error, ximp_ext_error,
    	XNExtXimp_LookupAttributes, nested_list,
	XNExtXimp_LookupStartCallback, ximp_ext_lookup_start_callback,
	XNExtXimp_LookupDrawCallback, ximp_ext_lookup_draw_callback,
	XNExtXimp_LookupDoneCallback, ximp_ext_lookup_done_callback,
	XNExtXimp_LookupProcessCallback, ximp_ext_lookup_process_callback,
    	XNExtXimp_LookupAttributes, nested_list,
	XNExtXimp_AuxStartCallback, ximp_ext_aux_start_callback,
	XNExtXimp_AuxDrawCallback, ximp_ext_aux_draw_callback,
	XNExtXimp_AuxProcessCallback, ximp_ext_aux_process_callback,
	XNExtXimp_AuxDoneCallback, ximp_ext_aux_done_callback,
	XNExtXimp_LookupBegin, ximp_ext_lookup_begin,
	XNExtXimp_RestartCallback, ximp_ext_restart_callback,
	XNExtXimp_DestroyCallback, ximp_ext_destroy_callback,
    /*
     * If you need to extend IC attributes, please add attribute/function
     * here
     */
	0
};

#define ICOPTBLSIZE ((sizeof(icoptbl)/sizeof(icop_t)) - 1)

/*
 * Ximp extentions
 *      XIMP_EXT_XIMP_BACK_FRONT
 */

Private int
ximp_ext_backfront(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    Ximp_ExtXIMRec *ext_im = (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if (!(ext_im->extension_back_front_exist))
	return (False);

    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->is_bep_mode = (int) value;
	return (True);
	break;
    case XICOpGet:
	*((long *) value) = (long)(ic->ximp_icpart->is_bep_mode);
	return (True);
	break;
    }
    return (False);
}

/*
 * Ximp extentions
 *      XIMP_EXT_XIMP_CONVERSION
 */
/**
 * Extended protocol for XNExtXimp_Conversion.
 * From Ximp lib to IM Server (To query or to set)
 *	  +-----------------------------------------+
 *	0 | XIMP_EXTENSION                          |
 *	  +-----------------------------------------+
 *	4 | ICID                                    |
 *	  +-----------------------------------------+
 *	8 | Atom ID of XNExtXimp_Conversion         |
 *	  +-----------------------------------------+
 *	12| Operation: Boolean Set(True)/Get(False) |
 *	  +-----------------------------------------+
 *	16| Conversion: Boolean On(True)/Off(False) |
 *	  +-----------------------------------------+
 *
 * From IM Server to Ximp library (reply of querying)
 *	  +-----------------------------------------+
 *	0 | XIMP_EXTENSION                          |
 *	  +-----------------------------------------+
 *	4 | ICID                                    |
 *	  +-----------------------------------------+
 *	8 | Atom ID of XNExtXimp_Conversion         |
 *	  +-----------------------------------------+
 *	12| VOID                                    |
 *	  +-----------------------------------------+
 *	16| Conversion: Boolean On(True)/Off(False) |
 *	  +-----------------------------------------+
 **/


typedef struct {
	Atom	message_type;
	Atom	ext_type;
	Window	owner;
	ICID	icid;
} XimpConversionPredArgRec, *XimpConversionPredArg;

Private Bool
#if NeedFunctionPrototypes
ximp_ext_conversionPredicate(
    Display *d,
    XEvent *ev,
    XPointer arg0
    )
#else
ximp_ext_conversionPredicate(d, ev, arg0)
Display *d;
XEvent *ev;
XPointer arg0;
#endif
{
    XimpConversionPredArg arg = (XimpConversionPredArg) arg0;

    if( ev->type == ClientMessage ) {
	if( ev->xclient.message_type == arg->message_type  &&
	    ev->xclient.format == 32  &&
	    ev->xclient.data.l[1] == arg->icid ) {
	    if( ev->xclient.data.l[0] == XIMP_ERROR )
		return( True );
	    else if( ev->xclient.data.l[0] == XIMP_EXTENSION  &&
		     ev->xclient.data.l[2] == arg->ext_type )
		return( True );
	}
    } else if( ev->type == DestroyNotify ) {
	if( ev->xdestroywindow.window == arg->owner ) {
	    return( True );
	}
    }
    return( False );
}

Private int
ximp_ext_conversion(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    Ximp_ExtXIMRec *ext_im = (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;
    XEvent	    event;
    XimpConversionPredArgRec Arg;

    if (!(ext_im->extension_conversion_exist))
	return (False);

    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	if(ic->ximp_icpart->icid == NULL) {
		if(((Ximp_XIM)ic->core.im)->ximp_impart->inputserver)
			return (False);
		return (True);
		}
	/*
	 * Set Conversion mode on/off
	 */
	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION,
			     ext_im->extension_conversion_id,
			     True,	/* SetICVelues */
			     value);
	/*
	 * This call expect IM Server to report new conversion state to Ximp
	 * lib right after setting.
	 */
	return True;
	break;
    case XICOpGet:
	if( ic->ximp_icpart->icid == NULL ) {
	    if(((Ximp_XIM)ic->core.im)->ximp_impart->inputserver)
		return( False );
	    *((long *) value) = (long)(ext_im->extension_conversion);
	    return (True);
	}
	_Ximp_IM_SendMessage( ic, XIMP_EXTENSION,
		     ext_im->extension_conversion_id,
		     False,	/* GetICVelues */
		     value );
	Arg.message_type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	Arg.ext_type = ext_im->extension_conversion_id;
	Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	Arg.icid = ic->ximp_icpart->icid;
	if( !_XimpIfEvent( ic, &event, ximp_ext_conversionPredicate, (XPointer)&Arg ) )
	    return( False );
	ext_im->extension_conversion = (Bool)event.xclient.data.l[4];
	*((long *) value) = (long)(ext_im->extension_conversion);
	return( True );
    }
    return (False);
}

/*
 * Ximp extentions
 *      XIMP_EXT_XIMP_STATUSWINDOW
 */

Private int
ximp_ext_statuswindow(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    Ximp_ExtXIMRec *ext_im = (Ximp_ExtXIMRec *)(((Ximp_XIM)ic->core.im)->ximp_impart)->imtype;

    if (!(ext_im->extension_statuswindow_exist))
	return (False);

    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

/*
 * Ximp extentions
 *      XIMP_EXT_XIMP_ERROR
 */

Private int
ximp_ext_error(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->error.client_data = ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->error.callback = ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

/*
 * Ximp extentions
 *      XIMP_EXT_XIMP_LOOKUPCHOICES
 */

Private int
ximp_ext_lookup_start_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->lookup_attr.callbacks.start.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->lookup_attr.callbacks.start.callback =
	    ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_lookup_draw_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->lookup_attr.callbacks.draw.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->lookup_attr.callbacks.draw.callback =
	    ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_lookup_done_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->lookup_attr.callbacks.done.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->lookup_attr.callbacks.done.callback =
	    ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_lookup_process_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	ic->ximp_icpart->lookup_attr.callbacks.proc.client_data =
	    ((XIMCallback *) value)->client_data;
	ic->ximp_icpart->lookup_attr.callbacks.proc.callback =
	    ((XIMCallback *) value)->callback;
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_aux_start_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_aux_draw_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_aux_process_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_aux_done_callback(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    switch (op) {
    case XICOpCreate:
    case XICOpSet:
	/*
	 * NOT Implemented Yet
	 */
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_lookup_begin(ic, op, value)
    Ximp_XIC        ic;
    XICOp_t         op;
    long            value;
{
    XIMXimpRec     *im_impart = ((Ximp_XIM)ic->core.im)->ximp_impart;
    Ximp_ExtXIMRec *ext_im;

    ext_im = (Ximp_ExtXIMRec *) im_impart->imtype;
    if (!(ext_im->extension_lookup_exist))
	return (False);

    switch (op) {
    case XICOpCreate:
	ic->ximp_icpart->use_lookup_choices = True ;
	break ;
    case XICOpSet:
	if(ic->ximp_icpart->icid == NULL) {
		if(((Ximp_XIM)ic->core.im)->ximp_impart->inputserver)
			return (False);
		return (True);
		}
	ic->ximp_icpart->use_lookup_choices = True ;
	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION,
			     ext_im->extension_lookup_id,
			     LOOKUP_CHOICES_BEGIN,
			     NULL);
	return (True);
	break;
    case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return (False);
}

Private int
ximp_ext_restart_callback( ic, op, value )
Ximp_XIC        ic;
XICOp_t         op;
long            value;
{
    switch( op ) {
      case XICOpCreate:
      case XICOpSet:
	ic->ximp_icpart->restart.client_data = ((XIMCallback *)value)->client_data;
	ic->ximp_icpart->restart.callback = ((XIMCallback *)value)->callback;
	return( True );
	break;

      case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return( False );
}

Private int
ximp_ext_destroy_callback( ic, op, value )
Ximp_XIC        ic;
XICOp_t         op;
long            value;
{
    switch( op ) {
      case XICOpCreate:
      case XICOpSet:
	ic->ximp_icpart->destroy.client_data = ((XIMCallback *)value)->client_data;
	ic->ximp_icpart->destroy.callback = ((XIMCallback *)value)->callback;
	return( True );
	break;

      case XICOpGet:
	/*
	 * NOT surely implemented Yet. Need attention.
	 */
	break;
    }
    return( False );
}

static void _Ximp_Extlookupstart();
static void _Ximp_Extlookupdraw();
static void _Ximp_Extlookupprocess();
static void _Ximp_Extlookupdone();

void
_Ximp_ExtLookup(d, w, ev, ic)
    Display        *d;
    Window          w;
    XClientMessageEvent *ev;
    Ximp_XIC        ic;
{
    switch (ev->data.l[3]) {
    case LOOKUP_CHOICES_START_REQ:
	_Ximp_Extlookupstart(ic, ev);
	break;
    case LOOKUP_CHOICES_DRAW_REQ:
	_Ximp_Extlookupdraw(ic, ev);
	break;
    case LOOKUP_CHOICES_PROCESS_REQ:
	_Ximp_Extlookupprocess(ic, ev, d, w);
	break;
    case LOOKUP_CHOICES_DONE_REQ:
	_Ximp_Extlookupdone(ic, ev);
	break;
    }
    return;
}

static void
FreeXIMLookupDrawCallbackStruct(p)
XIMLookupDrawCallbackStruct *p;
{
    register i ;
    for(i = 0 ; i < p->n_choices ; i++){
	if(p->choices[i].label){
	    if(p->choices[i].label->string.multi_byte)
	      Xfree(p->choices[i].label->string.multi_byte);
	    if(p->choices[i].label->feedback)
	      Xfree(p->choices[i].label->feedback);
	    Xfree(p->choices[i].label);
	}
	if(p->choices[i].value){
	    if(p->choices[i].value->string.multi_byte)
	      Xfree(p->choices[i].value->string.multi_byte);
	    if(p->choices[i].value->feedback)
	      Xfree(p->choices[i].value->feedback);
	    Xfree(p->choices[i].value);
	}
    }
    Xfree(p->choices);
    Xfree(p);
}

static void
_Ximp_Extlookupstart(ic, event)
    Ximp_XIC        ic;
    XClientMessageEvent *event;
{
    register XIMCallback *cb;
    static XClientMessageEvent clmsg;
    XIMLookupStartCallbackStruct xim_start;
    Ximp_ExtXIMRec *ext_im;
    long           *prop;
    Atom            type;
    int             format;
    unsigned long   nitems, after;

    cb = &ic->ximp_icpart->lookup_attr.callbacks.start;
    if(ic->ximp_icpart->lookup_attr.draw_data){
	FreeXIMLookupDrawCallbackStruct(ic->ximp_icpart->lookup_attr.draw_data);
	ic->ximp_icpart->lookup_attr.draw_data = NULL ;
    }
    if (cb->callback) {
	if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			       event->data.l[4], 0, 8, True, AnyPropertyType,
			       &type, &format, &nitems, &after,
			       (unsigned char **) &prop) == Success) && prop){

	    xim_start.event = (XKeyEvent *) malloc(sizeof(XEvent));
	    bzero(xim_start.event , sizeof(XEvent));
	    xim_start.event->keycode = *prop;
	    xim_start.event->state = *(prop + 1);
	    xim_start.event->window = ic->core.focus_window ;
	    xim_start.WhoIsMaster = *(prop + 2);
	    xim_start.XIMPreference = (LayoutInfo *) malloc(sizeof(LayoutInfo));
	    xim_start.XIMPreference->choice_per_window = *(prop + 3);
	    xim_start.XIMPreference->nrows = *(prop + 4);
	    xim_start.XIMPreference->ncolumns = *(prop + 5);
	    xim_start.XIMPreference->DrawUpDirection = *(prop + 6);
	    xim_start.XIMPreference->WhoOwnsLabel = *(prop + 7);
	    xim_start.CBPreference = (LayoutInfo *) malloc(sizeof(LayoutInfo));
	    XFree((char *) prop);

	    (*cb->callback) (ic, cb->client_data, &xim_start);

	    ext_im = (Ximp_ExtXIMRec *) (((Ximp_XIMRec *) ic->core.im)->ximp_impart->imtype);

	    prop = (long *) malloc(sizeof(long) * 6);
	    *(prop + 0) = xim_start.WhoIsMaster;
	    *(prop + 1) = xim_start.CBPreference->choice_per_window;
	    *(prop + 2) = xim_start.CBPreference->nrows;
	    *(prop + 3) = xim_start.CBPreference->ncolumns;
	    *(prop + 4) = xim_start.CBPreference->DrawUpDirection;
	    *(prop + 5) = xim_start.CBPreference->WhoOwnsLabel;

	    XChangeProperty(ic->core.im->core.display,
			    ic->core.focus_window,
			    ext_im->extension_lookup_start_rep,
			    ext_im->extension_lookup_start_rep,
			    32,
			    PropModeReplace,
			    (unsigned char *)prop,
			    6);

	    XFlush(ic->core.im->core.display);

	    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION,
				 ext_im->extension_lookup_id,
				 LOOKUP_CHOICES_START_REP,
				 ext_im->extension_lookup_start_rep,
				 NULL);

	    XFlush(ic->core.im->core.display);

	    free((char *) prop);
	    free((char *) xim_start.event);
	    free((char *) xim_start.CBPreference);
	    free((char *) xim_start.XIMPreference);
	}
    }
}

static void
_Ximp_Extlookupdraw(ic, event)
    Ximp_XIC        ic;
    XClientMessageEvent *event;
{
    register XIMCallback *cb;
    static XClientMessageEvent clmsg;
    XIMLookupDrawCallbackStruct *luc_draw;
    Ximp_ExtXIMRec *ext_im;
    long           *prop;
    char           *text, *textaddr;
    long           *feedback;
    XIMText        *textbuf;
    XIMChoiceObject *choicebuf, *chptr;
    Atom            type;
    int             format;
    int             i, l, j;
    int             strnum;
    unsigned long   nitems, after;

    cb = &ic->ximp_icpart->lookup_attr.callbacks.draw;
    if(ic->ximp_icpart->lookup_attr.draw_data){
	FreeXIMLookupDrawCallbackStruct(ic->ximp_icpart->lookup_attr.draw_data);
	ic->ximp_icpart->lookup_attr.draw_data =  NULL ;
	
    }
    if (!cb->callback)
	return;

    ic->ximp_icpart->lookup_attr.draw_data =  (XPointer)(luc_draw
      = (XIMLookupDrawCallbackStruct *)Xmalloc(sizeof(XIMLookupDrawCallbackStruct)));
    bzero(luc_draw, sizeof(XIMLookupDrawCallbackStruct));

    if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			   event->data.l[4], 0, 5, True, AnyPropertyType,
			   &type, &format, &nitems, &after,
			   (unsigned char **) &prop) != Success) || !prop)
	return;

    luc_draw->max_len = *prop;
    luc_draw->index_of_first_candidate = *(prop + 1);
    luc_draw->index_of_last_candidate = *(prop + 2);

    if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			   *(prop + 3), 0, 4096, True, AnyPropertyType,
			   &type, &format, &nitems, &after,
			   (unsigned char **) &text) == Success) && text) {

	for (strnum = 0, textaddr = text, i = 0; i < nitems; i++) {
	    j = strlen(textaddr);
	    textaddr += (j + 1);
	    strnum++;
	    i += j;
	}

	choicebuf = (XIMChoiceObject *) Xmalloc(sizeof(XIMChoiceObject)
					       * (strnum / 2));
	bzero(choicebuf, sizeof(XIMChoiceObject) * (strnum / 2));

	for (textaddr = text, chptr = choicebuf, i = 0; i < strnum; i += 2, chptr++) {
	    if (*textaddr) {
		chptr->label = (XIMText *) Xmalloc(sizeof(XIMText));
		bzero(chptr->label, sizeof(XIMText));
		l = j = strlen(textaddr);
		chptr->label->string.multi_byte = (char *) Xmalloc(j);
		bzero(chptr->label->string.multi_byte,j);
		_Ximp_cttombs(ic->core.im->core.lcd,
			      textaddr, j,
			      chptr->label->string.multi_byte, &l, NULL);
		chptr->label->length = l;
		textaddr += j;
	    }
	    textaddr++;

	    if (*textaddr) {
		chptr->value = (XIMText *) Xmalloc(sizeof(XIMText));
		bzero(chptr->value, sizeof(XIMText));
		l = j = strlen(textaddr);
		chptr->value->string.multi_byte = (char *) Xmalloc(j);
		bzero(chptr->value->string.multi_byte,j);
		_Ximp_cttombs(ic->core.im->core.lcd,
			      textaddr, j,
			      chptr->value->string.multi_byte, &l, NULL);
		chptr->value->length = l;
		textaddr += j;
	    }
	    textaddr++;
	}

	luc_draw->choices = choicebuf;
	luc_draw->n_choices = (strnum / 2);
    }
    if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			   *(prop + 4), 0, 4096, True, AnyPropertyType,
			   &type, &format, &nitems, &after,
			   (unsigned char **) &feedback) == Success)&&feedback){

	for (i = l = 0; i < luc_draw->n_choices; i++, l += 2) {
	    if (luc_draw->choices[i].value){
		register len = luc_draw->choices[i].value->length ;
		luc_draw->choices[i].value->feedback =
		  (XIMFeedback *)Xmalloc(sizeof(XIMFeedback) * len );
		if(len == 1){
		    luc_draw->choices[i].value->feedback[0] = feedback[l];
		} else {
		    register j ;
		    for (j = 0 ; j < len ; j++){
			luc_draw->choices[i].value->feedback[j] = feedback[l+1];
		    }
		}
		if (luc_draw->choices[i].label){
		    luc_draw->choices[i].label->feedback = 
		      (XIMFeedback *)Xmalloc(sizeof(XIMFeedback) * len );
		    if(len == 1){
			luc_draw->choices[i].label->feedback[0] = feedback[l];
		    } else {
			register j ;
			for (j = 0 ; j < len ; j++){
			    luc_draw->choices[i].label->feedback[j] = feedback[l];
			}
		    }
		}
	    }
	}
	(*cb->callback) (ic, cb->client_data, luc_draw);
	XFree(text);
	XFree(feedback);
    }
}

static void
_Ximp_Extlookupprocess(ic, event, d, w)
    Ximp_XIC        ic;
    XClientMessageEvent *event;
    Display        *d;
    Window          w;
{
    register XIMCallback *cb;
    static XClientMessageEvent clmsg;
    XIMLookupProcessCallbackStruct xim_proc;
    Ximp_ExtXIMRec *ext_im;
    long           *prop;
    Atom            type;
    int             format;
    unsigned long   nitems, after;

    cb = &ic->ximp_icpart->lookup_attr.callbacks.proc;

    if (cb->callback) {
	if ((XGetWindowProperty(ic->core.im->core.display,
			   ((Ximp_XIM) ic->core.im)->ximp_impart->fe_window,
			       event->data.l[4], 0, 2, True, AnyPropertyType,
			       &type, &format, &nitems, &after,
			       (unsigned char **) &prop) == Success)&&prop) {

	    xim_proc.event = (XKeyEvent *)event;
	    xim_proc.event->keycode = ((long *)prop)[0];
	    xim_proc.event->state = ((long *)prop)[1];
	    xim_proc.event->type = KeyPress ;

	    (*cb->callback) (ic, cb->client_data, &xim_proc);

	    ext_im = (Ximp_ExtXIMRec *) (((Ximp_XIMRec *) ic->core.im)->ximp_impart->imtype);

	    _Ximp_IM_SendMessage(ic, XIMP_EXTENSION,
				 ext_im->extension_lookup_id,
				 LOOKUP_CHOICES_PROCESS_REP,
				 xim_proc.index_of_choice_selected,
				 NULL);

	    XFlush(ic->core.im->core.display);
	}
    }
}

static void
_Ximp_Extlookupdone(ic, event)
    Ximp_XIC        ic;
    XClientMessageEvent *event;
{
    register XIMCallback *cb;

    cb = &ic->ximp_icpart->lookup_attr.callbacks.done;

    if (cb->callback) {
	(*cb->callback) (ic, cb->client_data, NULL);
    }
    if(ic->ximp_icpart->lookup_attr.draw_data){
	FreeXIMLookupDrawCallbackStruct(ic->ximp_icpart->lookup_attr.draw_data);
	ic->ximp_icpart->lookup_attr.draw_data = NULL ;	
    }
}

/*
 * Following functions are called by Core of Ximp.
 * These are used as fook for extension to Ximp.
 */

Private int
_Ximp_ext_icop(ic, name, op, value)
    Ximp_XIC        ic;
    char           *name;
    XICOp_t         op;
    long            value;
{
    register        i;

    for (i = 0; i < ICOPTBLSIZE; i++) {
	if (name[0] == icoptbl[i].name[0]) {	/* For faster comparison */
	    if (!strcmp(name + 1, icoptbl[i].name + 1)) {
		return ((*(icoptbl[i].func)) (ic, op, value));

	    }
	}
    }
    return False ;
}

Private int
nested_list(ic, op, args)
Ximp_XIC ic ;
XICOp_t op;
XIMArg *args ;
{
    register i ;
    int status ;
    register XIMArg *arg;

    for (arg = args; arg->name && *(arg->name); arg++) {
	if(_Ximp_ext_icop(ic, arg->name, op, arg->value ) == False){
	    return False ;
	}
    }
    return True ;
}  

/*
 * _Ximp_OpenIMResourceExtension() and _Ximp_SetupExtention()
 * is called from XOpenIM().
 */

void
_Ximp_OpenIMResourceExtension(im)
    Ximp_XIM        im;
{
    /* Add extension here */
    return;
}

void
_Ximp_SetupExtension(im)
    Ximp_XIM        im;
{
    Ximp_ExtXIMRec *ext_im;
    Atom           *atom;
    Atom            back_front;	/* Ximp registered extention */
    Atom            conversion;	/* Ximp registered extention */
    Atom            status_win;	/* Ximp registered extention */
    Atom            lookup;	/* Ximp registered extention */

    if ((ext_im = (Ximp_ExtXIMRec *) Xmalloc(sizeof(Ximp_ExtXIMRec))) == (Ximp_ExtXIMRec *) NULL)
	return;
    bzero((char *) ext_im, sizeof(Ximp_ExtXIMRec));

    /*
     * Backend / Frontend switching
     */
    back_front = XInternAtom(im->core.display,
			     XIMP_EXT_XIMP_BACK_FRONT, True);

    /*
     * Conversion on/off setting/querying
     */
    conversion = XInternAtom(im->core.display,
			     XIMP_EXT_XIMP_CONVERSION, True);

    /*
     * Status Window
     */
    status_win = XInternAtom(im->core.display,
			     XIMP_EXT_XIMP_STATUSWINDOW, True);
    /*
     * Lookup Choice  switching
     */
    lookup = XInternAtom(im->core.display,
			 XIMP_EXT_XIMP_LOOKUPCHOICES, True);

    for (atom = im->ximp_impart->im_ext_list; *atom; atom++) {
	if (back_front == *atom) {	/* Backend / Frontend */
	    ext_im->extension_back_front_exist = True;
	    ext_im->extension_back_front_id = back_front;
	} else if (conversion == *atom) {
	    ext_im->extension_conversion_exist = True;
	    ext_im->extension_conversion_id = conversion;
	} else if (status_win == *atom) {
	    ext_im->extension_statuswindow_exist = True;
	    ext_im->extension_statuswindow_id = status_win;
	} else if (lookup == *atom) {
	    ext_im->extension_lookup_exist = True;
	    ext_im->extension_lookup_id = lookup;

	    ext_im->extension_lookup_start = XInternAtom(
				 im->core.display,
				   XIMP_EXT_XIMP_LOOKUPSTARTCALLBACK, True);
	    ext_im->extension_lookup_start_rep = XInternAtom(im->core.display,
				   "_XIMP_EXT_XIMP_CHOICE_START_REP", True);

	    ext_im->extension_lookup_proc_rep = XInternAtom(im->core.display,
				 "_XIMP_EXT_XIMP_CHOICE_PROCESS_REP", True);
	} else;
	    	/* Add extension here */
    }
    im->ximp_impart->imtype = ext_im;
    return;
}

/*
 * _Ximp_SetupFreeExtension() is called by XCloseIM().
 */

void
_Ximp_SetupFreeExtension(im)
    Ximp_XIM        im;
{
    if (im->ximp_impart->imtype)
	Xfree(im->ximp_impart->imtype);
    /* Add extension here */
    return;
}

/*
 * _Ximp_GetIMFreeExtension() is called by XGetIMValues().
 */

Bool
_Ximp_GetIMExtension(ic, name, value)
    Ximp_XIC        ic;
    char           *name;
    long            value;
{
    /* Add extension here */
    return (False);
}

/*
 * _Ximp_SetICExtension() is called by XCreateIC() or XSetICValues().
 */

Bool
_Ximp_SetICExtension(ic, name, value, mode)
    Ximp_XIC        ic;
    char           *name;
    long            value;
    int             mode;
{
    if(mode == XIMP_CREATE_IC)
        return _Ximp_ext_icop(ic, name, XICOpCreate, value);
    else
        return _Ximp_ext_icop(ic, name, XICOpSet, value);
}

/*
 * _Ximp_GetICExtension() is called by XGetICValues().
 */

Bool
_Ximp_GetICExtension(ic, name, value)
    Ximp_XIC        ic;
    char           *name;
    long            value;
{
    return _Ximp_ext_icop(ic, name, XICOpGet, value);
}

/*
 * _Ximp_A_CreateExtension() is called by XCretaeIC().
 * But this is called after protocol of XIMP_CRETAE.
 */

void
_Ximp_A_CreateExtension(ic)
    Ximp_XIC        ic;
{
    Ximp_ExtXIMRec *ext_im;

    ext_im = (Ximp_ExtXIMRec *) (((Ximp_XIMRec *) ic->core.im)->ximp_impart->imtype);

    /* Backend / Frontend */
    if (ic->ximp_icpart->is_bep_mode == XIMP_BACKEND) {
	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION,
			     ext_im->extension_back_front_id,
			     ic->ximp_icpart->is_bep_mode,
			     NULL);
    }

    /* Lookup Choice using check */
    if((ext_im->extension_lookup_exist)) {
	if ( ic->ximp_icpart->use_lookup_choices ) {
	    	_Ximp_IM_SendMessage(ic, XIMP_EXTENSION,
				     ext_im->extension_lookup_id,
				     LOOKUP_CHOICES_BEGIN,
				     NULL);
	}
    }
    /* Add extension here */
    return;
}

/*
 * _Ximp_ProcExtension() is called by XFilterEvent().
 */

void
_Ximp_ProcExtension(d, w, ev)
    Display        *d;
    Window          w;
    XClientMessageEvent *ev;
{
    ICID            icid;
    Ximp_XIC        ic;
    Atom            ext_id;
    XIMXimpRec     *im_impart;
    Ximp_ExtXIMRec *ext_im;

    icid = ev->data.l[1];
    ext_id = ev->data.l[2];
    if( (ic = _Ximp_LookupXIC(icid)) == NULL )
	return;

    im_impart = ((Ximp_XIM)ic->core.im)->ximp_impart;
    ext_im = (Ximp_ExtXIMRec *) im_impart->imtype;

    if (ext_id == ext_im->extension_lookup_id) {
	if (ext_im->extension_lookup_exist)
	    _Ximp_ExtLookup(d, w, ev, ic);
	return;
	/* anything to do it? */
    } else;
    /* Add extension here */
    return;
}


void
_Ximp_CallRestartCallbackExtension( xic )
Ximp_XIC		xic;
{
    register	XIMCallback	*cb;

    cb = &xic->ximp_icpart->restart;
    if( cb->callback ) {
	(*cb->callback)( xic, cb->client_data, NULL );
    }
}

void
_Ximp_CallDestroyCallbackExtension( xic )
Ximp_XIC		xic;
{
    register	XIMCallback	*cb;

    cb = &xic->ximp_icpart->destroy;
    if( cb->callback ) {
	(*cb->callback)( xic, cb->client_data, NULL );
    }
}
