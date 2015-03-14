/* $XConsortium: XimpLkup.c,v 1.8 92/07/29 10:16:17 rws Exp $ */
/******************************************************************

              Copyright 1991, 1992 by Sony Corporation
              Copyright 1991, 1992 by FUJITSU LIMITED
              Copyright 1991, 1992 by Fuji Xerox Co.,Ltd.

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Sony Corporation,
FUJITSU LIMITED and Fuji Xerox Co.,Ltd. not be used in advertising
or publicity pertaining to distribution of the software without
specific, written prior permission.
Sony Corporation, FUJITSU LIMITED and Fuji Xerox Co.,Ltd. make no
representations about the suitability of this software for any purpose.
It is provided "as is" without express or implied warranty.

SONY CORPORATION, FUJITSU LIMITED AND FUJI XEROX CO.,LTD. DISCLAIM
ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
SONY CORPORATION, FUJITSU LIMITED, FUJI XEROX CO.,LTD. BE LIABLE FOR
ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

  Author: Masaki Takeuchi      Sony Corporation
          Takashi Fujiwara     FUJITSU LIMITED 
          Kazunori Nishihara   Fuji Xerox Co.,Ltd.
          Makoto Wakamatsu     Sony Corporation

******************************************************************/

#define NEED_EVENTS
#include <X11/keysym.h>
#include "Xlibint.h"
#include "Xutil.h"
#include "Xlcint.h"
#include "Xlibnet.h"
#include <X11/Xatom.h>

#include "Ximplc.h"

extern Ximp_XIC		_Ximp_LookupXIC();
extern Atom		_Ximp_Protocol_id();
extern Bool 		_Ximp_XimFilter_Client();
extern Bool 		_Ximp_XimFilter_Destroy();
static Bool 		_Ximp_StartXIMP();
extern Bool 		_Ximp_SetOpenXIMP();
extern void 		_Ximp_IM_SendMessage();
extern int		_Ximp_SetupFree();

extern void		_Ximp_A_CreateExtension();
extern void		_Ximp_SetupFreeExtension();
extern void		_Ximp_ProcExtension();

static void		_Ximp_CallCallback();
extern void		_Ximp_ProcError();

Bool
_XimpIfEvent( ic, event, predicate, arg )
Ximp_XIC	ic;
XEvent		*event;
Bool		(*predicate)(
#if NeedNestedPrototypes
	Display*,
	XEvent*,
	char*
#endif
);
char		*arg;
{
    XIfEvent( ic->core.im->core.display, event, predicate, arg );
    if( event->type == DestroyNotify ) {
	XPutBackEvent( ic->core.im->core.display, event );
	return( False );
    }
    else if( event->type == ClientMessage  &&  event->xclient.format == 32  &&
	     event->xclient.data.l[0] == XIMP_ERROR ) {
	_Ximp_ProcError( ic, ic->core.im->core.display, NULL, event );
	return( False );
    }
    return( True );
}


Bool
_Ximp_CMPredicate(d, ev, arg0)
Display *d;
XEvent *ev;
XPointer arg0;
{
    XimpCMPredicateArg arg = (XimpCMPredicateArg)arg0;

    if( ev->type == ClientMessage ) {
	if( ev->xclient.message_type == arg->type  &&
	    ev->xclient.format == 32 ) {
	    if( arg->icid == 0  ||  (ev->xclient.data.l[1] == arg->icid) )
		if( ev->xclient.data.l[0] == arg->protocol  ||
		    ev->xclient.data.l[0] == XIMP_ERROR )
		    return(True);
	}
    } else if( ev->type == DestroyNotify) {
	if( ev->xdestroywindow.window == arg->owner ) {
	    return( True );
	}
    }
    return( False );
}

static unsigned char *
_Ximp_Reset(ic)
	Ximp_XIC	 ic;
{
    XEvent			Message;
    XEvent			event;
    XimpCMPredicateArgRec	Arg;
    int				rval;
    Atom			actual_type_return;
    int				actual_format_return;
    unsigned long		nitems_return, bytes_after_return;
    unsigned char		*p = NULL;

    if(ic->ximp_icpart->icid) {
	/* ClientMessage Send */
	_Ximp_IM_SendMessage(ic, XIMP_RESET, NULL, NULL, NULL);

	Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
	Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
	Arg.protocol = XIMP_RESET_RETURN;
	Arg.icid = ic->ximp_icpart->icid;
	if( !_XimpIfEvent( ic, &event, _Ximp_CMPredicate, (XPointer)&Arg ) )
	    return( NULL );

	ic->ximp_icpart->icid =(ICID)event.xclient.data.l[1];
	rval = XGetWindowProperty( ic->core.im->core.display,
		    ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window,
		    (Atom)event.xclient.data.l[2], 0, 1024, True,
		    AnyPropertyType, &actual_type_return,
		    &actual_format_return, &nitems_return,
		    &bytes_after_return, &p );
	return( p );
    }
    return( (unsigned char *)NULL );
}

#define XIMP_MAXBUF 1024

char *
_Ximp_MbReset(ic)
	Ximp_XIC	 ic;
{
	char *mb;
	int length = XIMP_MAXBUF +1;
	unsigned char *ct = _Ximp_Reset(ic);

	if (!ct) return(NULL);
	mb = Xmalloc(length+1);
	_Ximp_cttombs(ic->core.im->core.lcd, ct, strlen((char *)ct), mb, &length, NULL);
	mb[length] = '\0';
	return(mb);
}

wchar_t *
_Ximp_WcReset(ic)
	Ximp_XIC	 ic;
{
	wchar_t *wc;
	int length = XIMP_MAXBUF +1;
	unsigned char *ct = _Ximp_Reset(ic);

	if (!ct) return(NULL);
	wc = (wchar_t *)Xmalloc((length + 1) * sizeof(wchar_t));
	_Ximp_cttowcs(ic->core.im->core.lcd, ct, strlen((char *)ct), wc, &length, NULL);
	wc[length] = (wchar_t)0;
	return(wc);
}

#define	LookupKeypress	1
#define LookupProperty	2
#define LookupMessage	3

static int		_xim_lookup_sign;
static unsigned char	*_xim_prop_return = (unsigned char *) NULL;
static unsigned long	_xim_string_length;
static int		_xim_message_len;
static unsigned char	_xim_message_buf[24];

int
_Ximp_MbLookupString(ic, ev, buffer, bytes, keysym, status)
	Ximp_XIC	 ic;
	XKeyEvent	*ev;
	char		*buffer;
	int		 bytes;
	KeySym		*keysym;
	Status		*status;
{
	XComposeStatus	 comp_status;
	int		 ret = 0, len;
	Ximp_XLCd	 lcd;
	unsigned char    *s;
	int		 str_len;

	if(ev->type == KeyPress && ev->keycode == 0) { /* Filter function */
		if ((_xim_lookup_sign == LookupProperty) ||
		    (_xim_lookup_sign == LookupMessage)) {
			if (_xim_lookup_sign == LookupMessage) {
				s = _xim_message_buf;
				str_len = _xim_message_len;
			} else {
				s = _xim_prop_return;
				str_len = _xim_string_length;
			}
			lcd = (Ximp_XLCd)ic->core.im->core.lcd;
			len = _Ximp_ct_mbslen(lcd, s, str_len, NULL);
			if (len > bytes) {
				ret = len;
				if(status) *status = XBufferOverflow;
			} else if (_Ximp_cttombs(lcd, s, str_len,
					  	 buffer, &bytes, NULL) <=  0) {
				ret = 0;
				if(status) *status = XLookupNone;
			} else {
				ret = bytes;
				if(status) *status = XLookupChars;
			}
			return(ret);
		} else {
			if(status) *status = XLookupNone;
			return(0);
		}
	}
	else if(ev->type == KeyPress) {
		if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
			if(status) *status = XLookupNone;
			return(0);
		}
		ic->ximp_icpart->putback_key_event = False;
		ret = _Ximp_LookupMBText(ic, ev, buffer, bytes, keysym, &comp_status);
		if(ret > 0) {
			if(keysym && *keysym != NoSymbol) {
				if(status) *status = XLookupBoth;
			}
			else {
				if(status) *status = XLookupChars;
			}
		}
		else {
			if(keysym && *keysym != NoSymbol) {
				if(status) *status = XLookupKeySym;
			}
			else {
				if(status) *status = XLookupNone;
			}
		}
 	}
	else {
		if (status) *status = XLookupNone;
    	}
	return(ret);
}

int
_Ximp_WcLookupString(ic, ev, buffer, wlen, keysym, status)
	Ximp_XIC		 ic;
	XKeyEvent	*ev;
	wchar_t		*buffer;
	int		 wlen;
	KeySym		*keysym;
	Status		*status;
{
	XComposeStatus	 comp_status;
	int		 ret, len;
	char		 look[128];
	Ximp_XLCd	 lcd;
	unsigned char    *s;
	int		 str_len;

	if(ev->type == KeyPress && ev->keycode == 0) { /* Filter function */
		if ((_xim_lookup_sign == LookupProperty) ||
		    (_xim_lookup_sign == LookupMessage)) {
			if (_xim_lookup_sign == LookupMessage) {
				s = _xim_message_buf;
				str_len = _xim_message_len;
			} else {
				s = _xim_prop_return;
				str_len = _xim_string_length;
			}
			lcd = (Ximp_XLCd)ic->core.im->core.lcd;
			len = _Ximp_ct_wcslen(lcd, s, str_len, NULL);
			if (len > wlen) {
				ret = len;
				if(status) *status = XBufferOverflow;
			} else if (_Ximp_cttowcs(lcd, s, str_len,
						 buffer, &wlen, NULL) <=  0) {
				ret = 0;
				if(status) *status = XLookupNone;
			} else {
				ret = wlen;
				if(status) *status = XLookupChars;
			}
			return(ret);
		} else {
			if(status) *status = XLookupNone;
			return(0);
		}
	}
	else if(ev->type == KeyPress) {
		if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
			if(status) *status = XLookupNone;
			return(0);
			}
		ic->ximp_icpart->putback_key_event = False;
		ret = _Ximp_LookupWCText(ic, ev, buffer, wlen, keysym, &comp_status);
		if(ret > 0) {
			if(keysym && *keysym != NoSymbol) {
				if(status) *status = XLookupBoth;
			}
			 else {
				if(status) *status = XLookupChars;
			}
		}
		else {
			if(keysym && *keysym != NoSymbol) {
				if(status) *status = XLookupKeySym;
			}
			 else {
				if(status) *status = XLookupNone;
			}
		}
 	}
	else {
		if (status) *status = XLookupNone;
    	}
	return(ret);
}

static Bool
_Ximp_FocusInput (window, mask)
	Window		window;
	unsigned long	*mask;
{
	int		i;
	Ximp_XIM	pim;
	Ximp_XIC	pic;
	extern int	Ximp_Xim_count;
	extern Ximp_XIM	*Ximp_Xim_List;

	for(i = 0; i < Ximp_Xim_count; i++) {
		if( (pim = Ximp_Xim_List[i]) == NULL )
			continue;
		for (pic = (Ximp_XIC)pim->core.ic_chain;
			pic; pic = (Ximp_XIC)pic->core.next) {
			if(pic->core.focus_window == window &&
				pic->ximp_icpart->input_mode) {
				*mask = pic->ximp_icpart->back_mask;
				return(True);
			}
		}
	}
	return (False);
}

static Bool
_Ximp_StartXIMP(ic, ev, keysym)
	Ximp_XIC		 ic;
	XKeyEvent		*ev;
	KeySym			 keysym;
{
	Ximp_KeyList		*list;
	int			 i, isEventPassedToIMS;
	XWindowAttributes	 ret_attributes;
	unsigned long		 dummy_mask;
	XEvent			 Message;
	extern Bool		_Ximp_Setup ();

	if (ic->ximp_icpart->input_mode) { /* ON : input_mode */
		if (!ic->ximp_icpart->putback_key_event && ev->keycode != 0) {
			_Ximp_IM_SendMessage( ic, XIMP_KEYPRESS,
					     (long)ev->keycode,
					     (long)ev->state, NULL );
			return (True);
		}
		return (False);
	}

	if(!(((Ximp_XIM)ic->core.im)->ximp_impart->connectserver)) {
		if( (list = ((Ximp_XIM)ic->core.im)->ximp_impart->process_start_keys) == NULL )
			return(False);
		for(i = 0, isEventPassedToIMS = 1; i < (int)list->count_keys; i++) {
			if( (keysym && keysym == list->keys_list[i].keysym)
			   && ((ev->state & list->keys_list[i].modifier_mask)
			       == list->keys_list[i].modifier ) ) {
				isEventPassedToIMS = (_Ximp_Setup( ic->core.im ) == False);
				break;
				}
			}
		}
	else {
		list = ((Ximp_XIM)ic->core.im)->ximp_impart->im_keyslist;
		for(i = 0, isEventPassedToIMS = 1; i < (int)list->count_keys; i++) {
			if( (keysym && keysym == list->keys_list[i].keysym)
			   && ((ev->state & list->keys_list[i].modifier_mask)
			       == list->keys_list[i].modifier ) ) {
				isEventPassedToIMS = 0;
				break;
				}
			}
		}
	if(isEventPassedToIMS) return(False);

	if(ic->ximp_icpart->icid == NULL)
		if(!(_Ximp_SetOpenXIMP(ic, XIMP_START_IC))) return(False);

	if (_Ximp_FocusInput (ic->core.focus_window, &dummy_mask))
		ic->ximp_icpart->back_mask = dummy_mask;
	else {
		Display *d = ic->core.im->core.display;

		XGetWindowAttributes(d, ic->core.focus_window, &ret_attributes);
		dummy_mask = ret_attributes.your_event_mask;
		ic->ximp_icpart->back_mask = dummy_mask;
		if(ic->ximp_icpart->is_bep_mode == XIMP_FRONTEND)
			dummy_mask &= ~(KeyPressMask | KeyReleaseMask);
		else
			dummy_mask &= ~(KeyReleaseMask);
		XSelectInput(d, ic->core.focus_window, dummy_mask);
	}
	ic->ximp_icpart->input_mode = 1;
	_Ximp_IM_SendMessage(ic, XIMP_MOVE,
		ic->ximp_icpart->preedit_attr.SpotLocation.x,
		ic->ximp_icpart->preedit_attr.SpotLocation.y,
		NULL);
	_Ximp_IM_SendMessage(ic, XIMP_BEGIN, NULL, NULL, NULL);
	XFlush(ic->core.im->core.display);
	_Ximp_CallRestartCallbackExtension( ic );
	return(True);
}

Bool
_Ximp_SetOpenXIMP(ic, mode)
	Ximp_XIC	ic;
	int		mode;
{
    unsigned long	mask;
    XEvent		event;
    XimpCMPredicateArgRec	Arg;

    if(mode == XIMP_CREATE_IC) {  /* XCretaeIC() */
	if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
	    ic->core.client_window = XCreateSimpleWindow(
	    ic->core.im->core.display,
	    DefaultRootWindow(ic->core.im->core.display),
	    0, 0, 1, 1, 1, 0, 0);
	}
    }

    if(!(ic->ximp_icpart->proto_mask & XIMP_FOCUS_WIN_MASK)) {
	ic->core.focus_window = ic->core.client_window;
    }
	
    /* Property Data Set */
    XChangeProperty(ic->core.im->core.display, ic->core.client_window,
		    ((Ximp_XIM)ic->core.im)->ximp_impart->version_id,
		    XA_STRING, 8, PropModeReplace,
		    (unsigned char *)XIMP_PROTOCOL_VERSION, strlen(XIMP_PROTOCOL_VERSION));
    XFlush(ic->core.im->core.display);

    mask = ic->ximp_icpart->proto_mask;
    _Ximp_SetFocusWindow(ic);
    mask |= XIMP_FOCUS_WIN_MASK;
    if(!(   (ic->core.input_style & XIMPreeditCallbacks)
	 || (ic->core.input_style & XIMPreeditNone) ) ) { 
	    if(mask & XIMP_PROP_PREEDIT)
		    _Ximp_SetPreeditAtr(ic);
	    if(mask & XIMP_PROP_PREFONT)
		    _Ximp_SetPreeditFont(ic);
    }
    else {
	mask &= ~(XIMP_PROP_PREEDIT | XIMP_PROP_PREFONT);
    }
    if(!(   (ic->core.input_style & XIMStatusCallbacks)
	 || (ic->core.input_style & XIMStatusNone) ) ) { 
	if(mask & XIMP_PROP_STATUS)
	    _Ximp_SetStatusAtr(ic);
	if(mask & XIMP_PROP_STSFONT)
	    _Ximp_SetStatusFont(ic);
    }
    else {
	mask &= ~(XIMP_PROP_STATUS | XIMP_PROP_STSFONT);
    }

    /* ClientMessage Send */
    _Ximp_IM_SendMessage(ic, XIMP_CREATE, ic->core.input_style, mask, NULL);

    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.protocol = XIMP_CREATE_RETURN;
    Arg.icid = 0;
    if( _XimpIfEvent( ic, &event, _Ximp_CMPredicate, (XPointer)&Arg ) ) {
	ic->ximp_icpart->icid = (ICID)event.xclient.data.l[1];
	_Ximp_A_CreateExtension(ic);
	return(True);
    }
    else {
	XDeleteProperty( ic->core.im->core.display,
			ic->core.client_window,
			((Ximp_XIM)ic->core.im)->ximp_impart->focus_win_id );
	if( !((ic->core.input_style & XIMPreeditCallbacks)  ||
	    (ic->core.input_style & XIMPreeditNone)) ) { 
	    if( mask & XIMP_PROP_PREEDIT )
		XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			((Ximp_XIM)ic->core.im)->ximp_impart->preedit_atr_id );
	    if( mask & XIMP_PROP_PREFONT )
		XDeleteProperty( ic->core.im->core.display,
			ic->core.client_window,
			((Ximp_XIM)ic->core.im)->ximp_impart->preeditfont_id );
	}
	if( !((ic->core.input_style & XIMStatusCallbacks)  ||
	    (ic->core.input_style & XIMStatusNone)) ) { 
	    if( mask & XIMP_PROP_STATUS )
		XDeleteProperty( ic->core.im->core.display,
			ic->core.client_window,
			((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id );
	    if( mask & XIMP_PROP_STSFONT )
		XDeleteProperty( ic->core.im->core.display,
			ic->core.client_window,
			((Ximp_XIM)ic->core.im)->ximp_impart->statusfont_id );
	}
	return( False );
    }
}

void
_Ximp_MakeKeypress (d, w, ev)
	Display			*d;
	Window			w;
	XKeyEvent		*ev;
{
	ev->type = KeyPress;
	ev->keycode = 0;
}

Bool
_Ximp_ProcKeypress (d, w, ev, kev)
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
	XKeyEvent		*kev;
{
	ICID			icid;
	Ximp_XIC 		ic;

	icid = ev->data.l[1];
	if( (ic = _Ximp_LookupXIC(icid)) == NULL )
	    return( False );
	kev->type = KeyPress;
	kev->serial = ev->serial;
	kev->send_event = False;
	kev->display = ev->display;
	kev->window = ev->window;
	kev->root = DefaultRootWindow(ev->display);
	kev->subwindow = (Window)NULL;
	kev->time = 0L;
	kev->x = 0;
	kev->y = 0;
	kev->x_root = 0;
	kev->y_root = 0;
	kev->keycode = ev->data.l[2];
	kev->state = ev->data.l[3];
	kev->same_screen = True;
	if(ic->ximp_icpart->input_mode) { /* ON : input_mode */
		ic->ximp_icpart->putback_key_event = True;
	}
	return( True );
}

static void
_Ximp_ProcCreateReturn (d, w, ev)
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
{
	ICID	icid;

	icid = ev->data.l[1];
}

static void
_Ximp_ProcConversionBegin (d, w, ev)
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
{
	ICID			icid;
	Ximp_XIC		ic;
	XWindowAttributes	ret_attributes;
	unsigned long		dummy_mask;

	icid = ev->data.l[1];
	if( (ic = _Ximp_LookupXIC(icid)) == NULL )
	    return;

	if(ic->ximp_icpart->input_mode) /* ON : input_mode */
		return;

	if (_Ximp_FocusInput (ic->core.focus_window, &dummy_mask))
		ic->ximp_icpart->back_mask = dummy_mask;
	else {
		XGetWindowAttributes(d, ic->core.focus_window, &ret_attributes);
		dummy_mask = ret_attributes.your_event_mask;
		ic->ximp_icpart->back_mask = dummy_mask;
		if(ic->ximp_icpart->is_bep_mode == XIMP_FRONTEND)
			dummy_mask &= ~(KeyPressMask | KeyReleaseMask);
		else
			dummy_mask &= ~(KeyReleaseMask);
		XSelectInput(d, ic->core.focus_window, dummy_mask);
		XFlush(d);
	}
	ic->ximp_icpart->input_mode = 1;
	return;
}

static void
_Ximp_ProcConversionEnd (d, w, ev)
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
{
	ICID			icid;
	Ximp_XIC		ic;

	icid = ev->data.l[1];
	if( (ic = _Ximp_LookupXIC(icid)) == NULL )
	    return;
	XSelectInput(d, ic->core.focus_window, ic->ximp_icpart->back_mask );
	XFlush(d);
	ic->ximp_icpart->input_mode = 0;
	return;
}

static void
_Ximp_ProcReadProperty (d, w, ev)
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
{
	Ximp_XIC		ic;
	ICID			icid;
	Atom			read_prop;
	int			rval;
	Atom			actual_type_return;
	int			actual_format_return;
	unsigned long		nitems_return;

	if (_xim_prop_return) {
		XFree((XPointer)(_xim_prop_return));
	}
	icid      = ev->data.l[1];
	read_prop = ev->data.l[2];
	if( (ic = _Ximp_LookupXIC(icid)) == NULL )
	    return;
	rval = XGetWindowProperty( d,
				((Ximp_XIM)ic->core.im)->ximp_impart->fe_window,
				read_prop, 0, 1024, True,
				AnyPropertyType, &actual_type_return,
				&actual_format_return, &_xim_string_length,
				&nitems_return, &_xim_prop_return );
	/*
	 * Note:
	 *	After getting the result from _xim_prop_return,
	 *	do not forget to do XFree it.
	 */
	_xim_lookup_sign = LookupProperty;
}

void
_Ximp_ProcError (ic0, d, w, ev)
	Ximp_XIC		ic0;
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
{
	ICID			icid;
	Ximp_XIC		ic;
	unsigned long		data[3];

	/*
	 * ToDo:
	 *	If you want to process the error from IM server,
	 *	you should modify this routine.
	 */

	if( (icid = ev->data.l[1]) != 0 ) {
		if( (ic = _Ximp_LookupXIC(icid)) == NULL )
			return;
	}
	else if( (ic = ic0) == NULL )
		return;

	if (ic->ximp_icpart->error.callback) {
		data[0] = ev->data.l[2];
		data[2] = ev->data.l[4];
		if(ev->data.l[0] != XIMP_ERROR)
			data[1] = XIMP_BadProtocol;
		else
			data[1]  = ev->data.l[3];

		(*ic->ximp_icpart->error.callback)(ic,
					ic->ximp_icpart->error.client_data,
					data);
		}
}

static void
_Ximp_ProcReadMessage (d, w, ev)
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
{
	ICID	icid;
	long	nicid;

	nicid = *(unsigned long *)(&(ev->data.b[0]));
	icid = (ICID)ntohl(nicid);
	_xim_message_len = ev->data.b[4];
	strncpy ((char *)_xim_message_buf, &(ev->data.b[5]), _xim_message_len);
	_xim_message_buf[_xim_message_len] = 0;
	_xim_message_buf[_xim_message_len + 1] = 0;
	_xim_message_buf[_xim_message_len + 2] = 0;
	_xim_message_buf[_xim_message_len + 3] = 0;
	_xim_lookup_sign = LookupMessage;
}

static void
_Ximp_CallCallback (d, w, ev)
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
{
	ICID		icid;
	Ximp_XIC	ic;

	icid = ev->data.l[1];
	if( (ic = _Ximp_LookupXIC(icid)) == NULL )
	    return;
	switch (ev->data.l[0]) {
		case XIMP_GEOMETRY:
			_Ximp_CallGeometryCallback (ic, ev);
			break;
		case XIMP_PREEDITSTART:
			_Ximp_CallPreeditStartCallback (ic, ev);
			break;
		case XIMP_PREEDITDONE:
			_Ximp_CallPreeditDoneCallback (ic, ev);
			break;
		case XIMP_PREEDITDRAW:
			_Ximp_CallPreeditDrawCallback (ic, ev);
			break;
		case XIMP_PREEDITDRAW_CM:
			_Ximp_CallPreeditDrawCallback2 (ic, ev);
			break;
		case XIMP_PREEDITDRAW_TINY:
			_Ximp_CallPreeditDrawCallback3 (ic, ev);
			break;
		case XIMP_PREEDITCARET:
			_Ximp_CallPreeditCaretCallback (ic, ev);
			break;
		case XIMP_STATUSSTART:
			_Ximp_CallStatusStartCallback (ic, ev);
			break;
		case XIMP_STATUSDONE:
			_Ximp_CallStatusDoneCallback (ic, ev);
			break;
		case XIMP_STATUSDRAW:
			_Ximp_CallStatusDrawCallback (ic, ev);
			break;
		case XIMP_STATUSDRAW_CM:
			_Ximp_CallStatusDrawCallback2 (ic, ev);
			break;
		default:
			break;
		}
}

static Bool
_Ximp_ProtoReceive (d, w, ev, client_data)
	Display			*d;
	Window			w;
	XClientMessageEvent	*ev;
	XPointer		*client_data;
{
	XKeyEvent kev;

	if (ev->message_type != _Ximp_Protocol_id ())
		return (False);
	if (ev->format == 32) {
		switch (ev->data.l[0]) {
		case XIMP_KEYPRESS:
			if( _Ximp_ProcKeypress (d, w, ev, &kev) )
			    XPutBackEvent(d, (XEvent *)&kev);
			else
			    return( False );
			break ;
		case XIMP_CREATE_RETURN:
			_Ximp_ProcCreateReturn (d, w, ev);
			break;
		case XIMP_CONVERSION_BEGIN:
			_Ximp_ProcConversionBegin (d, w, ev);
			break;
		case XIMP_CONVERSION_END:
			_Ximp_ProcConversionEnd (d, w, ev);
			break;
		case XIMP_READPROP:
			_Ximp_ProcReadProperty (d, w, ev);
			_Ximp_MakeKeypress (d, w, ev);
			ev->send_event = False ;
			XPutBackEvent(d, (XEvent *)ev);
			break ;
		case XIMP_ERROR:
			_Ximp_ProcError (NULL, d, w, ev);
			break;
		case XIMP_GEOMETRY:
		case XIMP_PREEDITSTART:
		case XIMP_PREEDITDONE:
		case XIMP_PREEDITDRAW:
		case XIMP_PREEDITDRAW_CM:
		case XIMP_PREEDITCARET:
		case XIMP_STATUSSTART:
		case XIMP_STATUSDONE:
		case XIMP_STATUSDRAW:
		case XIMP_STATUSDRAW_CM:
			_Ximp_CallCallback (d, w, ev);
			break;
		case XIMP_EXTENSION:
			_Ximp_ProcExtension(d, w, ev);
			break;
		default:
			break;
		}
        } else if (ev->format == 8) {
		_Ximp_ProcReadMessage (d, w, ev);
		_Ximp_MakeKeypress (d, w, ev);
		ev->send_event = False ;
		XPutBackEvent(d, (XEvent *)ev);
        }
	return (True);
}

static Bool
_Ximp_ServerDestroy (d, w, ev, client_data)
	Display			*d;
	Window			w;
	XEvent			*ev;
	XPointer		*client_data;
{
	extern Ximp_XIM		*Ximp_Xim_List;
	extern int		Ximp_Xim_count;
	register int		i;
	register XIMXimpRec	*ximp_impart;
	register XIC		 ic;
	long			dummy_mask;

	for(i=0; i < Ximp_Xim_count; i++) {
		if(Ximp_Xim_List[i] != NULL  &&
		   Ximp_Xim_List[i]->ximp_impart->fe_window == w)
			ximp_impart = Ximp_Xim_List[i]->ximp_impart;
		else
			continue;
		_XUnregisterFilter(d, w, _Ximp_XimFilter_Destroy, (XPointer)NULL);
		_Ximp_SetupFreeExtension(Ximp_Xim_List[i]);
		_Ximp_SetupFree(ximp_impart->im_proto_vl,
			ximp_impart->im_styles,
			ximp_impart->im_keyslist,
			ximp_impart->im_server_name,
			ximp_impart->im_server_vl,
			ximp_impart->im_vendor_name,
			ximp_impart->im_ext_list);
		ximp_impart->connectserver = 0;
		ximp_impart->fe_window = (Window)NULL;
		for(ic = Ximp_Xim_List[i]->core.ic_chain; ic; ic = ic->core.next) {
			((Ximp_XIC)ic)->ximp_icpart->icid = NULL;
			_Ximp_CallDestroyCallbackExtension( ic );
			_XUnregisterFilter(d,
					   ic->core.focus_window,
					   _Ximp_XimFilter_Client,
					   NULL);
			if(((Ximp_XIC)ic)->ximp_icpart->input_mode) {/* ON : input_mode */
				dummy_mask = ((Ximp_XIC)ic)->ximp_icpart->back_mask;
				XSelectInput(ic->core.im->core.display,
				     ic->core.focus_window, dummy_mask);
				((Ximp_XIC)ic)->ximp_icpart->input_mode = 0;
			}
		}
	}
	XFlush (d);
	return (False);
}

Bool
_Ximp_XimFilter_Keypress (d, w, ev, client_data)
	Display		*d;
	Window		w;
	XEvent		*ev;
	XPointer	client_data;
{
#define	BUFFLIM		32
    KeySym		ks;
    char		buff[BUFFLIM];

    XLookupString( (XKeyEvent *)ev, buff, BUFFLIM, &ks, NULL );
    return( _Ximp_StartXIMP( (Ximp_XIC)client_data, ev, ks ) );
}

/*
 *  _Ximp_XimFilter
 *	Regist _Ximp_XimFilter_Client filter using XRegisterFilterByType
 *	with start_type == end_type  == ClientMessage
 *	Regist _Ximp_XimFilter_Destroy filter using XRegisterFilterByType
 *	with start_type == end_type == DestroyNotify
 */

Bool
_Ximp_XimFilter_Client (d, w, ev, client_data)
	Display		*d;
	Window		w;
	XEvent		*ev;
	XPointer	*client_data;
{
	return(_Ximp_ProtoReceive (d, w, ev, client_data));
}

Bool
_Ximp_XimFilter_Destroy (d, w, ev, client_data)
	Display		*d;
	Window		w;
	XEvent		*ev;
	XPointer	*client_data;
{
	return(_Ximp_ServerDestroy (d, w, ev, client_data));
}
