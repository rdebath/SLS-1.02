/* $XConsortium: XimpRm.c,v 1.5 92/07/29 10:16:22 rws Exp $ */
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
FUJITSU LIMITED and Sony Corporation make no representations about
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

#define NEED_EVENTS
#include <X11/keysym.h>
#include "Xlibint.h"
#include "Xlcint.h"
#include "Ximplc.h"
#include "Xresource.h"

extern void		_Ximp_OpenIMResourceExtension();

void
_Ximp_Get_resource_name(im, res_name, res_class)
	Ximp_XIM	 im;
	char		*res_name;
	char		*res_class;
	{
	if(im->core.res_name == NULL)
		strcpy(res_name, "*");
	else	{
		strcpy(res_name, im->core.res_name);
		strcat(res_name, ".");
		}
	if(im->core.res_class == NULL)
		strcpy(res_class, "*");
	else	{
		strcpy(res_class, im->core.res_class);
		strcat(res_class, ".");
		}
	strcat(res_name, "ximp.");
	strcat(res_class, "Ximp.");
	}


#ifndef	isalnum
#define	isalnum(c)	\
    (('0' <= (c) && (c) <= '9')  || \
     ('A' <= (c) && (c) <= 'Z')  || \
     ('a' <= (c) && (c) <= 'z'))
#endif

static void	_Ximp_parse( im, event )
Ximp_XIM	 im;
char		*event;
{
    static Ximp_Key	key;
    char		*modifier, *detail;
    char		*ss;
    int			ii;
    Bool		exclamation, tilde;
    Ximp_KeyList	*keylist;
    static struct {
	char	*name;
	int	len;
	long	mask;
    } mod[] = {
	{ "Ctrl",	4,	ControlMask	},
	{ "Lock",	4,	LockMask	},
	{ "Caps",	4,	LockMask	},
	{ "Shift",	5,	ShiftMask	},
	{ "Alt",	3,	Mod1Mask	},
	{ "Meta",	4,	Mod1Mask	},
	{ NULL,		0,	0		}};
    extern char		*_Ximp_Strstr();
#define	AllMask	(ControlMask | LockMask | ShiftMask | Mod1Mask)

    if( (ss = _Ximp_Strstr( event, "<Key>" )) == NULL )
	return;
    detail = ss + 5;
    *ss = NULL;

    modifier = event;
    key.modifier = 0;
    key.modifier_mask = 0;
    key.keysym = XK_VoidSymbol;
    exclamation = False;
    do {
	while( *modifier == ' '  ||  *modifier == '\t' )
	    modifier++;
	if( *modifier == NULL )
	    break;
	if( strncmp( modifier, "None", 4 ) == 0 ) {
	    if( key.modifier_mask != 0  ||  exclamation )
		return;
	    key.modifier_mask = AllMask;
	    modifier += 4;
	}
	else {
	    if( *modifier == '!' ) {
		if( key.modifier_mask != 0  ||  exclamation )
		    return;
		key.modifier_mask = AllMask;
		exclamation = True;
		modifier++;
		continue;
	    }
	    if( (tilde = (*modifier == '~')) ) {
		modifier++;
		while( *modifier == ' '  ||  *modifier == '\t' )
		    modifier++;
	    }
	    for( ii = 0; mod[ii].name != NULL; ii++ ) {
		if( strncmp( modifier, mod[ii].name, mod[ii].len ) == 0 ) {
		    key.modifier_mask |= mod[ii].mask;
		    if( !tilde )
			key.modifier |= mod[ii].mask;
		    modifier += mod[ii].len;
		    break;
		}
	    }
	}
	if( mod[ii].name == NULL )
	    return;
    } while( *modifier != NULL );

    while( *detail == ' '  ||  *detail == '\t' )
	detail++;
    for( ss = detail; isalnum(*ss)  ||  *ss == '_'; ss++ );
    *ss = NULL;
    if( (key.keysym = XStringToKeysym( detail )) != NoSymbol ) {
	if( !(keylist = im->ximp_impart->process_start_keys) ) {
	    if( (keylist = (Ximp_KeyList *)Xcalloc(1, sizeof(Ximp_KeyList))) == NULL )
		return;
	    if((keylist->keys_list = (Ximp_Key *)Xmalloc(sizeof(Ximp_Key))) == NULL) {
		Xfree( keylist );
		return;
	    }
	}
	else {
	    Ximp_Key	*keys_list;
	    if( (keys_list = (Ximp_Key *)Xrealloc(keylist->keys_list, sizeof(Ximp_Key) * (keylist->count_keys + 1))) == NULL )
		return;
	    keylist->keys_list = keys_list;
	}
	keylist->keys_list[keylist->count_keys] = key;
	keylist->count_keys++;
	im->ximp_impart->process_start_keys = keylist;
    }
}


Bool
_Ximp_OpenIM_Resource(im)
	Ximp_XIM	 im;
	{
	char		 res_name[256];
	char		 res_class[256];
	char		*str_type;
	XrmValue	 value;
	Bool		 ret = False;
	KeySym		 keysym = NoSymbol;
	Ximp_KeyList	*keylist;

	if(im->core.rdb == NULL)
		return(ret);

	/* Inputserver */
	_Ximp_Get_resource_name(im, res_name, res_class);
	strcat(res_name, "inputserver");
	strcat(res_class, "Inputserver");
	if(XrmGetResource(im->core.rdb, res_name, res_class,
				&str_type, &value) == True) { 
		if(strcmp(value.addr, "off") == 0) {
			/* Keysym */
			_Ximp_Get_resource_name(im, res_name, res_class);
			strcat(res_name, "startkeysym");
			strcat(res_class, "Startkeysym");
			if(XrmGetResource(im->core.rdb, res_name, res_class,
				&str_type, &value) == True) { 
				keysym = XStringToKeysym(value.addr);
				}
			ret = False;
			if(keysym != NoSymbol) {
				if((keylist = (Ximp_KeyList *)Xmalloc(sizeof(Ximp_KeyList))) != NULL ) {
					if((keylist->keys_list = (Ximp_Key *)Xmalloc(sizeof(Ximp_Key)))!= NULL) {
						keylist->count_keys = 1;
						keylist->keys_list[0].modifier = 0;
						keylist->keys_list[0].modifier_mask = 0;
						keylist->keys_list[0].keysym = keysym;
						im->ximp_impart->process_start_keys = keylist;
						ret = True;
						}
					else
						Xfree(keylist);
					}
				}

			/* ProcessStartKeys */
			_Ximp_Get_resource_name(im, res_name, res_class);
			strcat(res_name, "processStartKeys");
			strcat(res_class, "ProcessStartKeys");
			if(XrmGetResource(im->core.rdb, res_name, res_class,
						&str_type, &value)  == True) {
				char	*string, *ss, c;
				char	*line;

				if( (line = Xmalloc(value.size)) != NULL ) {
					string = value.addr;
					do {
						ss = line;
						while( (c = *string) != NULL ) {
							string++;
							if( c == '\n' )
								break;
							*ss++ = c;
							}
						*ss = NULL;
						_Ximp_parse( im, line );
					} while( *string != NULL );
					Xfree( line );
					if( im->ximp_impart->process_start_keys )
						ret = True;
					}
				}
			}
		}
	/* Call Back */
	_Ximp_Get_resource_name(im, res_name, res_class);
	strcat(res_name, "callbackEncoding");
	strcat(res_class, "CallbackEncoding");
	if(XrmGetResource(im->core.rdb, res_name, res_class,
				&str_type, &value) == True) { 
		if(strcmp(value.addr, "wchar") == 0) {
			im->ximp_impart->use_wchar = True;
			}
		}
	/* Extension : XOpenIM(, rdb, res_name, res_class) */
	_Ximp_OpenIMResourceExtension(im);
	return(ret);
	}

void
_Ximp_SetValue_Resource(ic, mask)
	Ximp_XIC	 ic;
	long		*mask;
	{
	Ximp_XIM	 im;
	char		 res_name[256];
	char		 res_class[256];
	char		*str_type;
	XrmValue	 value;
	Colormap	 default_colormap;
	XColor		 screen_def, exact_def;
	int		 num;

	im = (Ximp_XIM)XIMOfIC((XIC)ic);
	if(im->core.rdb == NULL)
		return;

	if(!(   (ic->core.input_style & XIMPreeditCallbacks)
	     || (ic->core.input_style & XIMPreeditNone) ) ) {
		if(!(ic->ximp_icpart->proto_mask & XIMP_PRE_BG_MASK)) {
			_Ximp_Get_resource_name(im, res_name, res_class);
			strcat(res_name, "preedit.background");
			strcat(res_class, "Preedit.Background");
			if(XrmGetResource(im->core.rdb, res_name, res_class,
					&str_type, &value) == True) { 
				default_colormap = DefaultColormap(
						im->core.display,
						DefaultScreen(im->core.display) );
				if( XAllocNamedColor(im->core.display, default_colormap,
					     value.addr,
					     &screen_def, &exact_def) ) {
					ic->core.preedit_attr.background = screen_def.pixel;
					ic->ximp_icpart->preedit_attr.Background = 
						ic->core.preedit_attr.background;
					ic->ximp_icpart->proto_mask |= XIMP_PRE_BG_MASK;
					*mask                       |= XIMP_PRE_BG_MASK;
					}
				}
			}
		if(!(ic->ximp_icpart->proto_mask & XIMP_PRE_FG_MASK)) {
			_Ximp_Get_resource_name(im, res_name, res_class);
			strcat(res_name, "preedit.foreground");
			strcat(res_class, "Preedit.Foreground");
			if(XrmGetResource(im->core.rdb, res_name, res_class,
					&str_type, &value) == True) { 
				default_colormap = DefaultColormap(
						im->core.display,
						DefaultScreen(im->core.display) );
				if( XAllocNamedColor(im->core.display, default_colormap,
					     value.addr,
					     &screen_def, &exact_def) ) {
					ic->core.preedit_attr.foreground = screen_def.pixel;
					ic->ximp_icpart->preedit_attr.Foreground = 
						ic->core.preedit_attr.foreground;
					ic->ximp_icpart->proto_mask |= XIMP_PRE_FG_MASK;
					*mask                       |= XIMP_PRE_FG_MASK;
					}
				}
			}
		if(!(ic->ximp_icpart->proto_mask & XIMP_PRE_LINESP_MASK)) {
			_Ximp_Get_resource_name(im, res_name, res_class);
			strcat(res_name, "preedit.linespacing");
			strcat(res_class, "Preedit.Linespacing");
			if(XrmGetResource(im->core.rdb, res_name, res_class,
					&str_type, &value) == True) { 
				num = atoi(value.addr);
				ic->core.preedit_attr.line_space = num;
				ic->ximp_icpart->preedit_attr.LineSpacing = 
					ic->core.preedit_attr.line_space;
				ic->ximp_icpart->proto_mask |= XIMP_PRE_LINESP_MASK;
				*mask                       |= XIMP_PRE_LINESP_MASK;
				}
			}
		}
	if(!(   (ic->core.input_style & XIMStatusCallbacks)
	     || (ic->core.input_style & XIMStatusNone) ) ) {
		if(!(ic->ximp_icpart->proto_mask & XIMP_STS_BG_MASK)) {
			_Ximp_Get_resource_name(im, res_name, res_class);
			strcat(res_name, "status.background");
			strcat(res_class, "Status.Background");
			if(XrmGetResource(im->core.rdb, res_name, res_class,
					&str_type, &value) == True) { 
				default_colormap = DefaultColormap(
						im->core.display,
						DefaultScreen(im->core.display) );
				if( XAllocNamedColor(im->core.display, default_colormap,
					     value.addr,
					     &screen_def, &exact_def) ) {
					ic->core.status_attr.background = screen_def.pixel;
					ic->ximp_icpart->status_attr.Background = 
						ic->core.status_attr.background;
					ic->ximp_icpart->proto_mask |= XIMP_STS_BG_MASK;
					*mask                       |= XIMP_STS_BG_MASK;
					}
				}

			}
		if(!(ic->ximp_icpart->proto_mask & XIMP_STS_FG_MASK)) {
			_Ximp_Get_resource_name(im, res_name, res_class);
			strcat(res_name, "status.foreground");
			strcat(res_class, "Status.Foreground");
			if(XrmGetResource(im->core.rdb, res_name, res_class,
					&str_type, &value) == True) { 
				default_colormap = DefaultColormap(
						im->core.display,
						DefaultScreen(im->core.display) );
				if( XAllocNamedColor(im->core.display, default_colormap,
					     value.addr,
					     &screen_def, &exact_def) ) {
					ic->core.status_attr.foreground = screen_def.pixel;
					ic->ximp_icpart->status_attr.Foreground = 
						ic->core.status_attr.foreground;
					ic->ximp_icpart->proto_mask |= XIMP_STS_FG_MASK;
					*mask                       |= XIMP_STS_FG_MASK;
					}
				}
			}
		if(!(ic->ximp_icpart->proto_mask & XIMP_STS_LINESP_MASK)) {
			_Ximp_Get_resource_name(im, res_name, res_class);
			strcat(res_name, "status.linespacing");
			strcat(res_class, "Status.Linespacing");
			if(XrmGetResource(im->core.rdb, res_name, res_class,
					&str_type, &value) == True) { 
				num = atoi(value.addr);
				ic->core.status_attr.line_space = num;
				ic->ximp_icpart->status_attr.LineSpacing = 
					ic->core.status_attr.line_space;
				ic->ximp_icpart->proto_mask |= XIMP_STS_LINESP_MASK;
				*mask                       |= XIMP_STS_LINESP_MASK;
				}
			}
		}
	if(   (ic->ximp_icpart->value_mask & XIMP_RES_NAME)
           || (ic->ximp_icpart->value_mask & XIMP_RES_CLASS) )
		ic->ximp_icpart->value_mask &= ~(XIMP_RES_NAME | XIMP_RES_CLASS);
	return;
	}
