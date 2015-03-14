/* $XConsortium: XimpICS.c,v 1.4 92/07/29 10:16:04 rws Exp $ */
/******************************************************************

              Copyright 1991, 1992 by FUJITSU LIMITED
              Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED
and Sony Corporaion not be used in advertising or publicity
pertaining to distribution of the software without specific,
written prior permission.
FUJITSU LIMITED and Sony Corporaion make no representations about
the suitability of this software for any purpose.  It is provided
"as is" without express or implied warranty.

FUJITSU LIMITED AND SONY CORPORATION DISCLAIM ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL FUJITSU LIMITED AND
SONY CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE
OR PERFORMANCE OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
          Makoto Wakamatsu     Sony Corporation

******************************************************************/

#define	NEED_EVENTS
#include "Xlibint.h"
#include "Xlcint.h"

#include "Ximplc.h"

extern char 		*_Ximp_SetICValues();
extern char 		*_Ximp_SetICValueData();
extern void		_Ximp_SetValue_Resource();
extern Bool		_Ximp_SetICExtension();

extern void		_Ximp_SetFocusWindow();
extern void		_Ximp_SetPreeditAtr();
extern void		_Ximp_SetPreeditFont();
extern void		_Ximp_SetStatusAtr();
extern void		_Ximp_SetStatusFont();
extern void		_Ximp_IM_SendMessage();
extern Bool		_Ximp_XimFilter_Keypress();

static Bool		_Ximp_PreSetAttributes();
static Bool		_Ximp_StatusSetAttributes();


static Bool
_Ximp_PNPredicate( d, ev, arg0 )
Display		*d;
XEvent		*ev;
XPointer	arg0;
{
    XimpPNPredicateArg arg = (XimpPNPredicateArg)arg0;

    if( ev->type == PropertyNotify ) {
	if( ev->xproperty.window == arg->window  &&
	    ev->xproperty.atom == arg->atom  &&
	    ev->xproperty.state == PropertyDelete ) {
	    return(True);
	}
    }
    else if( ev->type == ClientMessage ) {
	if( ev->xclient.message_type == arg->type  &&
	    ev->xclient.format == 32  &&
	    ev->xclient.data.l[1] == arg->icid  &&
	    ev->xclient.data.l[0] == XIMP_ERROR )
	    return( True );
    }
    else if( ev->type == DestroyNotify ) {
	if( ev->xdestroywindow.window == arg->owner ) {
	    return( True );
	}
    }
    return( False );
}

Bool
_XimpPNIfEvent( ic, atom )
Ximp_XIC	ic;
Atom		atom;
{
    XEvent			event;
    XimpPNPredicateArgRec	Arg;	

    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.icid = ic->ximp_icpart->icid;
    Arg.window = ic->core.client_window;
    Arg.atom = atom;
    XIfEvent( ic->core.im->core.display, &event, _Ximp_PNPredicate, (XPointer)&Arg );
    if( event.type != PropertyNotify ) {
	XPutBackEvent( ic->core.im->core.display, &event );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->focus_win_id );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->preedit_atr_id );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->preeditfont_id );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id );
	XDeleteProperty( ic->core.im->core.display,
			 ic->core.client_window,
			 ((Ximp_XIM)ic->core.im)->ximp_impart->statusfont_id );
	if( event.type == DestroyNotify )
	    XPutBackEvent( ic->core.im->core.display, &event );
	else
	    _Ximp_ProcError( ic, ic->core.im->core.display, NULL, &event );
	return( False );
    }
    return( True );
}

char *
_Ximp_SetICValues(ic, values)
	Ximp_XIC	 ic;
	XIMArg		*values;
	{
	XIM		 im;
	char		*ret;
	int		 change_mask = 0;

	if((ret = _Ximp_SetICValueData(ic, values, XIMP_SET_IC, &change_mask)))
		return(ret);

	if(   (ic->ximp_icpart->value_mask & XIMP_RES_NAME)
	   || (ic->ximp_icpart->value_mask & XIMP_RES_CLASS) )
		_Ximp_SetValue_Resource(ic, &change_mask);

	if(ic->ximp_icpart->icid == NULL) {
		if(change_mask & XIMP_PROP_FOCUS) {
			if(ic->ximp_icpart->filter_mode & 0x1) {
				_XUnregisterFilter (ic->core.im->core.display,
						ic->ximp_icpart->back_focus_win,
						_Ximp_XimFilter_Keypress,
						(XPointer)ic);
				}
			_XRegisterFilterByType (ic->core.im->core.display,
						ic->core.focus_window,
						KeyPress, KeyPress,
						_Ximp_XimFilter_Keypress,
						(XPointer)ic);
			ic->ximp_icpart->filter_mode |= 0x1;
			}
		return(ret);
		}

	if(ic->core.input_style & XIMPreeditPosition) {
		if(change_mask == XIMP_PRE_SPOTL_MASK) {
			if(ic->ximp_icpart->input_mode)
			    _Ximp_IM_SendMessage(ic, XIMP_MOVE,
				ic->ximp_icpart->preedit_attr.SpotLocation.x,
				ic->ximp_icpart->preedit_attr.SpotLocation.y,
				NULL);
			return(ret);
		}
	}
	if(change_mask & XIMP_PROP_FOCUS)
		_Ximp_SetFocusWindow(ic);
	if(!(   (ic->core.input_style & XIMPreeditCallbacks)
	     || (ic->core.input_style & XIMPreeditNone) ) ) { 
		if(change_mask & XIMP_PROP_PREEDIT)
			_Ximp_SetPreeditAtr(ic);
		if(change_mask & XIMP_PROP_PREFONT)
			_Ximp_SetPreeditFont(ic);
		}
	else {
		change_mask &= ~(XIMP_PROP_PREEDIT | XIMP_PROP_PREFONT);
		}
	if(!(   (ic->core.input_style & XIMStatusCallbacks)
	     || (ic->core.input_style & XIMStatusNone) ) ) { 
		if(change_mask & XIMP_PROP_STATUS)
			_Ximp_SetStatusAtr(ic);
		if(change_mask & XIMP_PROP_STSFONT)
			_Ximp_SetStatusFont(ic);
		}
	else {
		change_mask &= ~(XIMP_PROP_STATUS | XIMP_PROP_STSFONT);
		}
	if(change_mask) {
	    XWindowAttributes		war;
	    long			mask;

	    XGetWindowAttributes( ic->core.im->core.display,
				  ic->core.client_window, &war );
	    XSelectInput( ic->core.im->core.display, ic->core.client_window,
			  war.your_event_mask | PropertyChangeMask );
	    _Ximp_IM_SendMessage(ic, XIMP_SETVALUE, change_mask, NULL, NULL);
	    if( change_mask & XIMP_PROP_FOCUS )
		if( !_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->focus_win_id ) )
		    change_mask = 0;
	    if( change_mask & XIMP_PROP_PREEDIT )
		if( !_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->preedit_atr_id ) )
		    change_mask = 0;
	    if( change_mask & XIMP_PROP_PREFONT )
		if( !_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->preeditfont_id ) )
		    change_mask = 0;
	    if( change_mask & XIMP_PROP_STATUS )
		if( !_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id ) )
		    change_mask = 0;
	    if( change_mask & XIMP_PROP_STSFONT )
		!_XimpPNIfEvent( ic, ((Ximp_XIM)ic->core.im)->ximp_impart->statusfont_id );
	    XSelectInput( ic->core.im->core.display, ic->core.client_window,
			  war.your_event_mask );
	}
	return(ret);
}

char *
_Ximp_SetICValueData(ic, values, mode, change_mask)
	Ximp_XIC	 ic;
	XIMArg		*values;
	int		 mode;
	int		*change_mask;
	{
	XIMArg		*p;
	char		*return_name = NULL;

	for(p = values; p->name != NULL; p++) {
		if(strcmp(p->name, XNInputStyle) == 0) {
			if(mode == XIMP_CREATE_IC) {
				ic->core.input_style = (XIMStyle)p->value;
				ic->ximp_icpart->value_mask |= XIMP_INPUT_STYLE;
				}
			else
				; /* Currently Fixed value */
			}
		else if(strcmp(p->name, XNClientWindow)==0) {
			if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN)) {
				if(mode == XIMP_SET_IC && (ic->ximp_icpart->icid)) {
					long	icid_old, icid_new;
					Window	client_window;

					icid_old = ic->ximp_icpart->icid;
					client_window = ic->core.client_window;
					ic->ximp_icpart->value_mask |= XIMP_CLIENT_WIN;
					ic->core.client_window = (Window)p->value;
					if(_Ximp_SetOpenXIMP(ic, mode) == False) {
						return_name = p->name;
						ic->ximp_icpart->value_mask &= ~XIMP_CLIENT_WIN;
						ic->core.client_window = client_window;
						break;
						}
					icid_new = ic->ximp_icpart->icid;
					ic->ximp_icpart->icid = icid_old;
					_Ximp_IM_SendMessage(ic, XIMP_DESTROY, NULL, NULL, NULL);
					ic->ximp_icpart->icid = icid_new;
					XDestroyWindow(ic->core.im->core.display,
						       client_window);
					*change_mask = NULL;
					}
				else { /* XIMP_CREATE_IC | (XIMP_SET_IC && XIMP_START_IC) */
					ic->core.client_window = (Window)p->value;
					ic->ximp_icpart->value_mask |= XIMP_CLIENT_WIN;
					if(!(ic->ximp_icpart->proto_mask & XIMP_FOCUS_WIN_MASK)) {
						ic->core.focus_window = ic->core.client_window;
						ic->ximp_icpart->proto_mask |= XIMP_FOCUS_WIN_MASK;
						}
					}
				}
			else {
				return_name = p->name;
				break; /* Can't change this value */
				}
			}
		else if(strcmp(p->name, XNFocusWindow)==0) {
			if(mode == XIMP_SET_IC && ic->ximp_icpart->input_mode) {
				Window	new_focus_window = (Window)p->value;
				unsigned long	dummy_mask;
				XWindowAttributes	wattr;

				XSelectInput(ic->core.im->core.display,
					     ic->core.focus_window,
					     ic->ximp_icpart->back_mask);
				XGetWindowAttributes(ic->core.im->core.display,
						     new_focus_window,
						     &wattr);
				dummy_mask = wattr.your_event_mask;
				ic->ximp_icpart->back_mask = dummy_mask;
				if(ic->ximp_icpart->is_bep_mode == XIMP_FRONTEND) {
					dummy_mask &= ~(KeyPressMask | KeyReleaseMask);
					}
				else {
					dummy_mask &= ~(KeyReleaseMask);
					}
				XSelectInput(ic->core.im->core.display,
					     new_focus_window,
					     dummy_mask);
				}
			ic->ximp_icpart->back_focus_win = ic->core.focus_window;
			ic->core.focus_window = (Window)p->value;
			ic->ximp_icpart->proto_mask |= XIMP_FOCUS_WIN_MASK;
			*change_mask                |= XIMP_FOCUS_WIN_MASK;
			}
		else if(strcmp(p->name, XNResourceName)==0) {
			ic->core.im->core.res_name = (char *)p->value;
			ic->ximp_icpart->value_mask |= XIMP_RES_NAME;
			}
		else if(strcmp(p->name, XNResourceClass)==0) {
			ic->core.im->core.res_class = (char *)p->value;
			ic->ximp_icpart->value_mask |= XIMP_RES_CLASS;
			}
		else if(strcmp(p->name, XNGeometryCallback)==0) {
			ic->core.geometry_callback.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.geometry_callback.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_GEOMETRY_CB;
			}
		else if(strcmp(p->name, XNPreeditAttributes)==0) {
			if( _Ximp_PreSetAttributes(ic,
				&(ic->ximp_icpart->preedit_attr),
				p->value, mode, change_mask,
				return_name) == False )
				break;
			}
		else if(strcmp(p->name, XNStatusAttributes)==0) {
			if( _Ximp_StatusSetAttributes(ic,
				&(ic->ximp_icpart->status_attr),
				p->value, mode, change_mask,
				return_name) == False )
				break;
			}
		else {
			if( _Ximp_SetICExtension(ic, p->name, p->value, mode) == False ) {
				return_name = p->name;
				break;
				}
			}
		}
	return(return_name);
	}
		
static Bool
_Ximp_PreSetAttributes(ic, attr, vl, mode, change_mask, return_name)
	Ximp_XIC		 ic;
	Ximp_PreeditPropRec	*attr;
	XIMArg			*vl;
	int			 mode;
	int			*change_mask;
	char			*return_name;
	{
	XIMArg			*p;
	Colormap		 colormap_ret;
	int			 list_ret;
	XFontStruct		**struct_list;
	char			**name_list;
	int 			 i, len;
	char 			*tmp;


	for(p = vl; p->name != NULL; p++) {
		if(strcmp(p->name, XNArea)==0) {
			ic->core.preedit_attr.area.x = ((XRectangle *)p->value)->x;
			ic->core.preedit_attr.area.y = ((XRectangle *)p->value)->y;
			ic->core.preedit_attr.area.width = ((XRectangle *)p->value)->width;
			ic->core.preedit_attr.area.height = ((XRectangle *)p->value)->height;
			attr->Area.x      = ic->core.preedit_attr.area.x;
			attr->Area.y      = ic->core.preedit_attr.area.y;
			attr->Area.width  = ic->core.preedit_attr.area.width;
			attr->Area.height = ic->core.preedit_attr.area.height;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_AREA_MASK;
			*change_mask                |= XIMP_PRE_AREA_MASK;
			}
		else if(strcmp(p->name, XNAreaNeeded)==0) {
			ic->core.preedit_attr.area_needed.width  = ((XRectangle *)p->value)->width;
			ic->core.preedit_attr.area_needed.height = ((XRectangle *)p->value)->height;
			attr->AreaNeeded.width  = ic->core.preedit_attr.area_needed.width;
			attr->AreaNeeded.height = ic->core.preedit_attr.area_needed.height;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_AREANEED_MASK;
			*change_mask                |= XIMP_PRE_AREANEED_MASK;
			}
		else if(strcmp(p->name, XNSpotLocation)==0) {
			ic->core.preedit_attr.spot_location.x = ((XPoint *)p->value)->x;
			ic->core.preedit_attr.spot_location.y = ((XPoint *)p->value)->y;
			attr->SpotLocation.x = ic->core.preedit_attr.spot_location.x;
			attr->SpotLocation.y = ic->core.preedit_attr.spot_location.y;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_SPOTL_MASK;
			*change_mask                |= XIMP_PRE_SPOTL_MASK;
			}
		else if(strcmp(p->name, XNColormap)==0) {
			ic->core.preedit_attr.colormap = (Colormap)p->value;
			attr->Colormap = ic->core.preedit_attr.colormap;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_COLORMAP_MASK;
			*change_mask                |= XIMP_PRE_COLORMAP_MASK;
			}
		else if(strcmp(p->name, XNStdColormap)==0) {
			if( XGetStandardColormap(ic->core.im->core.display,
					ic->core.focus_window,
					&colormap_ret, (Atom)p->value) != 0) {
				ic->core.preedit_attr.colormap = colormap_ret;
				attr->Colormap = ic->core.preedit_attr.colormap;
				ic->ximp_icpart->proto_mask |= XIMP_PRE_COLORMAP_MASK;
				*change_mask                |= XIMP_PRE_COLORMAP_MASK;
				}
			else {
				return_name = p->name;
				return(False);
				}
			}
		else if(strcmp(p->name, XNBackground)==0) {
			ic->core.preedit_attr.background = (unsigned long)p->value;
			attr->Background = ic->core.preedit_attr.background;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_BG_MASK;
			*change_mask                |= XIMP_PRE_BG_MASK;
			}
		else if(strcmp(p->name, XNForeground)==0) {
			ic->core.preedit_attr.foreground = (unsigned long)p->value;
			attr->Foreground = ic->core.preedit_attr.foreground;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_FG_MASK;
			*change_mask                |= XIMP_PRE_FG_MASK;
			}
		else if(strcmp(p->name, XNBackgroundPixmap)==0) {
			ic->core.preedit_attr.background_pixmap = (Pixmap)p->value;
			attr->Bg_Pixmap = ic->core.preedit_attr.background_pixmap;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_BGPIXMAP_MASK;
			*change_mask                |= XIMP_PRE_BGPIXMAP_MASK;
			}
		else if(strcmp(p->name, XNFontSet)==0) {
			ic->core.preedit_attr.fontset = (XFontSet)p->value;
			if(p->value != NULL) {
				if(ic->ximp_icpart->preedit_font)
	    				Xfree(ic->ximp_icpart->preedit_font);
				list_ret = XFontsOfFontSet(
					ic->core.preedit_attr.fontset,
					&struct_list, &name_list);
				for(i = 0, len = 0; i < list_ret; i++) {
					len += strlen(name_list[i]);
					}
				if( (tmp = Xmalloc(len + i + 1)) == NULL ) {
				    return_name = p->name;
				    return( False );
				}
				tmp[0] = NULL;
				for(i = 0; i < list_ret; i++) {
					strcat(tmp, name_list[i]);
					strcat(tmp, ",");
					}
				tmp[len + i - 1] = NULL;
				ic->ximp_icpart->preedit_font = tmp;
				ic->ximp_icpart->proto_mask |= XIMP_PRE_FONT_MASK;
				*change_mask                |= XIMP_PRE_FONT_MASK;
				}
			else {
				return_name = p->name;
				return(False);
				}
			}
		else if(strcmp(p->name, XNLineSpace)==0) {
			ic->core.preedit_attr.line_space = (long)p->value;
			attr->LineSpacing = ic->core.preedit_attr.line_space;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_LINESP_MASK;
			*change_mask                |= XIMP_PRE_LINESP_MASK;
			}
		else if(strcmp(p->name, XNCursor)==0) {
			ic->core.preedit_attr.cursor = (Cursor)p->value;
			attr->Cursor = ic->core.preedit_attr.cursor;
			ic->ximp_icpart->proto_mask |= XIMP_PRE_CURSOR_MASK;
			*change_mask                |= XIMP_PRE_CURSOR_MASK;
			}
		else if(strcmp(p->name, XNPreeditStartCallback)==0) {
			ic->core.preedit_attr.callbacks.start.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.preedit_attr.callbacks.start.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_PRE_CALLBAK;
			}
		else if(strcmp(p->name, XNPreeditDoneCallback)==0) {
			ic->core.preedit_attr.callbacks.done.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.preedit_attr.callbacks.done.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_PRE_CALLBAK;
			}
		else if(strcmp(p->name, XNPreeditDrawCallback)==0) {
			ic->core.preedit_attr.callbacks.draw.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.preedit_attr.callbacks.draw.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_PRE_CALLBAK;
			}
		else if(strcmp(p->name, XNPreeditCaretCallback)==0) {
			ic->core.preedit_attr.callbacks.caret.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.preedit_attr.callbacks.caret.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_PRE_CALLBAK;
			}
		}
	return(True);
	}

static Bool
_Ximp_StatusSetAttributes(ic, attr, vl, mode, change_mask, return_name)
	Ximp_XIC		 ic;
	Ximp_StatusPropRec	*attr;
	XIMArg			*vl;
	int			 mode;
	int			*change_mask;
	char			*return_name;
	{
	XIMArg			*p;
	Colormap	 	colormap_ret;
	int			 list_ret;
	XFontStruct		**struct_list;
	char			**name_list;
	int 			 i, len;
	char 			*tmp;

	for(p = vl; p->name != NULL; p++) {
		if(strcmp(p->name, XNArea)==0) {
			ic->core.status_attr.area.x = ((XRectangle *)p->value)->x;
			ic->core.status_attr.area.y = ((XRectangle *)p->value)->y;
			ic->core.status_attr.area.width = ((XRectangle *)p->value)->width;
			ic->core.status_attr.area.height = ((XRectangle *)p->value)->height;
			attr->Area.x      = ic->core.status_attr.area.x;
			attr->Area.y      = ic->core.status_attr.area.y;
			attr->Area.width  = ic->core.status_attr.area.width;
			attr->Area.height = ic->core.status_attr.area.height;
			ic->ximp_icpart->proto_mask |= XIMP_STS_AREA_MASK;
			*change_mask                |= XIMP_STS_AREA_MASK;
			}
		else if(strcmp(p->name, XNAreaNeeded)==0) {
			ic->core.status_attr.area_needed.width  = ((XRectangle *)p->value)->width;
			ic->core.status_attr.area_needed.height = ((XRectangle *)p->value)->height;
			attr->AreaNeeded.width  = ic->core.status_attr.area_needed.width;
			attr->AreaNeeded.height = ic->core.status_attr.area_needed.height;
			ic->ximp_icpart->proto_mask |= XIMP_STS_AREANEED_MASK;
			*change_mask                |= XIMP_STS_AREANEED_MASK;
			}
		else if(strcmp(p->name, XNColormap)==0) {
			ic->core.status_attr.colormap = (Colormap)p->value;
			attr->Colormap = ic->core.status_attr.colormap;
			ic->ximp_icpart->proto_mask |= XIMP_STS_COLORMAP_MASK;
			*change_mask                |= XIMP_STS_COLORMAP_MASK;
			}
		else if(strcmp(p->name, XNStdColormap)==0) {
			if(XGetStandardColormap(ic->core.im->core.display,
					ic->core.focus_window,
					&colormap_ret, (Atom)p->value) !=0) {
				ic->core.status_attr.colormap = colormap_ret;
				attr->Colormap = ic->core.status_attr.colormap;
				ic->ximp_icpart->proto_mask |= XIMP_STS_COLORMAP_MASK;
				*change_mask                |= XIMP_STS_COLORMAP_MASK;
				}
			else {
				return_name = p->name;
				return(False);
				}
			}
		else if(strcmp(p->name, XNBackground)==0) {
			ic->core.status_attr.background = (unsigned long)p->value;
			attr->Background = ic->core.status_attr.background;
			ic->ximp_icpart->proto_mask |= XIMP_STS_BG_MASK;
			*change_mask                |= XIMP_STS_BG_MASK;
			}
		else if(strcmp(p->name, XNForeground)==0) {
			ic->core.status_attr.foreground = (unsigned long)p->value;
			attr->Foreground = ic->core.status_attr.foreground;
			ic->ximp_icpart->proto_mask |= XIMP_STS_FG_MASK;
			*change_mask                |= XIMP_STS_FG_MASK;
			}
		else if(strcmp(p->name, XNBackgroundPixmap)==0) {
			ic->core.status_attr.background_pixmap = (Pixmap)p->value;
			attr->Bg_Pixmap = ic->core.status_attr.background_pixmap;
			ic->ximp_icpart->proto_mask |= XIMP_STS_BGPIXMAP_MASK;
			*change_mask                |= XIMP_STS_BGPIXMAP_MASK;
			}
		else if(strcmp(p->name, XNFontSet)==0) {
			ic->core.status_attr.fontset = (XFontSet)p->value;
			if (p->value != NULL) {
				if(ic->ximp_icpart->status_font)
	    				Xfree(ic->ximp_icpart->status_font);
				list_ret = XFontsOfFontSet(
					ic->core.status_attr.fontset,
					&struct_list, &name_list);
				for(i = 0, len = 0; i < list_ret; i++) {
					len += strlen(name_list[i]);
					}
				if( (tmp = Xmalloc(len + i + 1)) == NULL ) {
				    return_name = p->name;
				    return( False );
				}
				tmp[0] = NULL;
				for(i = 0; i < list_ret; i++) {
					strcat(tmp, name_list[i]);
					strcat(tmp, ",");
					}
				tmp[len + i - 1] = NULL;
				ic->ximp_icpart->status_font = tmp;
				ic->ximp_icpart->proto_mask |= XIMP_STS_FONT_MASK;
				*change_mask                |= XIMP_STS_FONT_MASK;
				}
			else {
				return_name = p->name;
				return(False);
				}
			}
		else if(strcmp(p->name, XNLineSpace)==0) {
			ic->core.status_attr.line_space = (long)p->value;
			attr->LineSpacing = ic->core.status_attr.line_space;
			ic->ximp_icpart->proto_mask |= XIMP_STS_LINESP_MASK;
			*change_mask                |= XIMP_STS_LINESP_MASK;
			}
		else if(strcmp(p->name, XNCursor)==0) {
			ic->core.status_attr.cursor = (Cursor)p->value;
			attr->Cursor = ic->core.status_attr.cursor;
			ic->ximp_icpart->proto_mask |= XIMP_STS_CURSOR_MASK;
			*change_mask                |= XIMP_STS_CURSOR_MASK;
			}
		else if(strcmp(p->name, XNStatusStartCallback)==0) {
			ic->core.status_attr.callbacks.start.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.status_attr.callbacks.start.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_STS_CALLBAK;
			}
		else if(strcmp(p->name, XNStatusDoneCallback)==0) {
			ic->core.status_attr.callbacks.done.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.status_attr.callbacks.done.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_STS_CALLBAK;
			}
		else if(strcmp(p->name, XNStatusDrawCallback)==0) {
			ic->core.status_attr.callbacks.draw.client_data =
				((XIMCallback *)p->value)->client_data;
			ic->core.status_attr.callbacks.draw.callback =
				((XIMCallback *)p->value)->callback;
			ic->ximp_icpart->value_mask |= XIMP_STS_CALLBAK;
			}
		}
	return(True);
	}
