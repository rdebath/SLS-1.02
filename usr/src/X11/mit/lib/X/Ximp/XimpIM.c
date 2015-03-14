/* $XConsortium: XimpIM.c,v 1.8 92/07/29 10:16:10 rws Exp $ */
/******************************************************************

              Copyright 1991, 1992 by FUJITSU LIMITED
              Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED
and Sony Corporation not be used in advertising or publicity
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
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
	  Makoto Wakamatsu     Sony Corporation

******************************************************************/

#include <X11/Xatom.h>
#include <X11/Xos.h>
#include "Xlibint.h"
#include "Xlcint.h"

#include "Ximplc.h"

static Status		 _Ximp_CloseIM();
static char		*_Ximp_GetIMValues();
extern XIC		 _Ximp_CreateIC();

extern Bool		 _Ximp_OpenIM_Resource();
extern Ximp_XIC		 _Ximp_LookupXIC();
extern int		 _Ximp_SetupFree();
extern Bool		 _Ximp_Setup();

extern void		 _Ximp_SetupExtension();
extern Bool		 _Ximp_GetIMExtension();
static Bool 		 _Ximp_GetStyle();

extern Bool		 _Ximp_XimFilter_Destroy();

Ximp_XIM 		*Ximp_Xim_List = (Ximp_XIM *)NULL;
int			 Ximp_Xim_count = 0;
static	Atom	 	 Protocol_ID;
static XIMMethodsRec	 Ximp_im_methods = {
				_Ximp_CloseIM, 		/* close */
				_Ximp_GetIMValues, 	/* get_values */
				_Ximp_CreateIC, 	/* create_ic */
				};

char	*_Ximp_Strstr( src, dest )
register char	*src, *dest;
{
    register	len;

    if( src == NULL  ||  dest == NULL )
	return( NULL );
    len = strlen(dest);
    while( src = index( src, *dest ) ) {
	if( strncmp( src, dest, len ) == 0 )
	    return( src );
	src++;
    }
    return( NULL );
}

XIM
_Ximp_OpenIM(lcd, dpy, rdb, res_name, res_class)
	XLCd		 lcd;
	Display		*dpy;
	XrmDatabase	 rdb;
	char		*res_name, *res_class;
{
	Ximp_XIM		 im;
	XIMXimpRec		*ximp_impart;
	Bool			 ret;

	if((im = (Ximp_XIM)Xmalloc(sizeof(Ximp_XIMRec))) == (Ximp_XIM)NULL) {
		return((XIM)NULL);
	}

	im->methods = &Ximp_im_methods;

	im->core.lcd      = lcd;
	im->core.ic_chain = (XIC)NULL;
	im->core.display   = dpy;
	im->core.rdb       = rdb;
	im->core.res_name  = res_name;
	im->core.res_class = res_class;

	if((ximp_impart = (XIMXimpRec *)Xmalloc(sizeof(XIMXimpRec))) == (XIMXimpRec *)NULL) {
		Xfree(im);
		return((XIM)NULL);
	}
	ximp_impart->connectserver  = 0;
	ximp_impart->inputserver    = 1;
	ximp_impart->use_wchar      = False;
	ximp_impart->process_start_keys = NULL;
	im->ximp_impart = ximp_impart;

        if(_Ximp_OpenIM_Resource(im) == True)
		im->ximp_impart->inputserver = 0;
	if(_Ximp_Setup(im) == False && im->ximp_impart->inputserver) {
		Xfree(ximp_impart);
		Xfree(im);
		return((XIM)NULL);
	}
	return((XIM)im);
}

static char *
_Ximp_GetIMValues(im, values)
	Ximp_XIM	 im;
	XIMArg		*values;
{
	XIMArg		*p;

	for(p = values; p->name != NULL; p++) {
		if(strcmp(p->name, XNQueryInputStyle) == 0) {
			if( _Ximp_GetStyle(im, p->value) == False )
				break;
		}
		else {
			if( _Ximp_GetIMExtension(im, p->name, p->value) == False )
				break;
		}
	}
	return(p->name);
}

static Status
_Ximp_CloseIM(im)
	Ximp_XIM	im;
{
	XIC		ic;
	int		i;
	XIMXimpRec	*ximp_impart;

	for(ic = im->core.ic_chain; ic; ic = ic->core.next)
		XDestroyIC(ic);
	ximp_impart = (XIMXimpRec *)im->ximp_impart;
	if( ximp_impart->process_start_keys ) {
		XFree( ximp_impart->process_start_keys->keys_list );
		XFree( ximp_impart->process_start_keys );
	}
	for(i=0; i < Ximp_Xim_count; i++) {
		if(Ximp_Xim_List[i] == im) {
			Ximp_Xim_List[i] = NULL;
			break;
		}
	}
	if(ximp_impart->connectserver) {
		_Ximp_SetupFreeExtension(im);
		_Ximp_SetupFree(ximp_impart->im_proto_vl,
				ximp_impart->im_styles,
				ximp_impart->im_keyslist,
				ximp_impart->im_server_name,
				ximp_impart->im_server_vl,
				ximp_impart->im_vendor_name,
				ximp_impart->im_ext_list);
	}
	Xfree(ximp_impart);
	return(True);
}

static	char	 IMname[XIMP_NAME];
static  char     LCname[XIMP_NAME];

Bool
_Ximp_Setup(im)
	Ximp_XIM	im;
{
	XLCd		 lcd;
	Atom		 atom_server;
	Window	 	 fe_window_id;
	Display		*dpy;
	Atom		improtocol_id,
			version_id, style_id, keys_id, servername_id,
			serverversion_id, vendorname_id, extentions_id,
			ctext_id, focus_win_id, preedit_atr_id, status_atr_id,
			preeditfont_id, statusfont_id, preeditmaxsize_id;
	char		*version;
	XIMStyles	*imstyle;
	Ximp_KeyList	*keylist;
	Atom		*ext_list;
	char		*server_name;
	char		*server_vl;
	char		*vendor_name;
	Atom		actual_type;
	int		actual_format;
	unsigned long	nitems, bytes_after;
	char		*prop;
	long		*prop_long;
	int		*prop_int;
	int		 i, n, count;

	lcd = im->core.lcd;
	dpy = im->core.display;

	strcpy(LCname, lcd->core.name);
	prop = index(LCname, '.');
#ifdef sun	/* XXX */
	if (!strcmp(LCname, "japanese"))
	    strcpy(LCname, "ja_JP");
#endif
	if (prop)
	    *prop = 0;

	/* IMserver Name  ex. _XIMP_ja_JP  */
	if(lcd->core.modifiers == (char *)NULL || *lcd->core.modifiers == '\0')
		sprintf(IMname, "%s%s", _XIMP_BASE, LCname); /* Default */
	else {
#define MODIFIER "@im="
 		char *mod, *s, buf[20];
 
 		(void)strcpy(IMname, _XIMP_BASE);
 		(void)strcat(IMname, LCname);
 		mod = _Ximp_Strstr(lcd->core.modifiers, MODIFIER);
 		if (mod) {
 			(void)strcat(IMname, "@");
 			mod += strlen(MODIFIER);
 			s = IMname + strlen(IMname);
 			while (*mod && *mod != '@') {
 				*s++ = *mod++;
			}
			*s = '\0';
		}
		(void)sprintf(buf, ".%d", dpy->default_screen);
		(void)strcat(IMname, buf);
	}
#undef MODIFIER
	/* Get IMS Window WID */
	if((atom_server = XInternAtom(dpy, IMname, False)) == (Atom)NULL){
		return(False);
	}
	fe_window_id = XGetSelectionOwner(dpy, atom_server);
	if(fe_window_id == NULL) return(False);

	/* Get Property : _XIMP_VERSION */
	version_id = XInternAtom(dpy, _XIMP_VERSION, False);
	if( XGetWindowProperty(dpy, fe_window_id, version_id, 0L, 1000000L, False,
			XA_STRING, &actual_type, &actual_format, &nitems,
			&bytes_after, (unsigned char **)(&prop)) != Success)
		return(False);
	if((version = Xmalloc((sizeof(char) * nitems + 1))) == NULL) {
		XFree(prop);
		return(False);
	}
	strncpy(version, prop, nitems);
	version[nitems] = '\0';
	XFree(prop);

	/* Get Property : _XIMP_STYLE */
	style_id = XInternAtom(dpy, _XIMP_STYLE, False);
	if( XGetWindowProperty(dpy, fe_window_id, style_id, 0L, 1000000L, False,
			style_id, &actual_type, &actual_format, &nitems,
			&bytes_after, (unsigned char **)(&prop_long)) != Success) {
		_Ximp_SetupFree(version, NULL, NULL, NULL, NULL, NULL, NULL);
		return(False);
	}
	if((imstyle = (XIMStyles *)Xmalloc(sizeof(XIMStyles))) == NULL) {
		_Ximp_SetupFree(version, NULL, NULL, NULL, NULL, NULL, NULL);
		XFree((XPointer)prop_long);
		return(False);
	}
	if((imstyle->supported_styles =
		(XIMStyle *)Xmalloc(sizeof(XIMStyle) * nitems)) == NULL) {
		Xfree(imstyle);
		_Ximp_SetupFree(version, NULL, NULL, NULL, NULL, NULL, NULL);
		XFree((XPointer)prop_long);
		return(False);
	}
	for(i=0; i < nitems; i++) {
		imstyle->supported_styles[i] = prop_long[i];
	}
	imstyle->count_styles = nitems;
	XFree((XPointer)prop_long);

	/* Get Property : _XIMP_KEYS */
	keys_id = XInternAtom(dpy, _XIMP_KEYS, False);
	if( XGetWindowProperty(dpy, fe_window_id, keys_id, 0L, 1000000L, False,
			keys_id, &actual_type, &actual_format, &nitems,
			&bytes_after, (unsigned char **)(&prop_int)) != Success) {
		_Ximp_SetupFree(version, imstyle, NULL, NULL, NULL, NULL, NULL);
		return(False);
	}
	if((keylist = (Ximp_KeyList *)Xmalloc(sizeof(Ximp_KeyList))) == NULL) {
		_Ximp_SetupFree(version, imstyle, NULL, NULL, NULL, NULL, NULL);
		XFree((XPointer)prop_int);
		return(False);
	}
	count = nitems / 3;
	if((keylist->keys_list = (Ximp_Key *)Xmalloc(sizeof(Ximp_Key) * count)) == NULL) {
		Xfree(keylist);
		_Ximp_SetupFree(version, imstyle, NULL, NULL, NULL, NULL, NULL);
		XFree((XPointer)prop_int);
		return(False);
	}
	for(i=0,n=0; n < count; n++) {
		keylist->keys_list[n].modifier        = prop_int[i++];
		keylist->keys_list[n].modifier_mask   = prop_int[i++];
		keylist->keys_list[n].keysym          = prop_int[i++];
	}
	keylist->count_keys = count;
	XFree((XPointer)prop_int);

	/* Get Property : _XIMP_SERVERNAME */
	servername_id = XInternAtom(dpy, _XIMP_SERVERNAME, False);
	if( XGetWindowProperty(dpy, fe_window_id, servername_id, 0L, 1000000L, False,
			XA_STRING, &actual_type, &actual_format, &nitems,
			&bytes_after, (unsigned char **)(&prop)) != Success) {
		_Ximp_SetupFree(version, imstyle, keylist, NULL, NULL, NULL, NULL);
		return(False);
	}
	if((server_name = (char *)Xmalloc((sizeof(char) * nitems + 1))) == NULL) {
		_Ximp_SetupFree(version, imstyle, keylist, NULL, NULL, NULL, NULL);
		XFree(prop);
		return(False);
	}
	strncpy(server_name, prop, nitems);
	server_name[nitems] = '\0';
	XFree(prop);

	/* Get Property : _XIMP_SERVERVERSION */
	serverversion_id  = XInternAtom(dpy, _XIMP_SERVERVERSION, False);
	if( XGetWindowProperty(dpy, fe_window_id, serverversion_id, 0L, 1000000L, False,
			XA_STRING, &actual_type, &actual_format, &nitems,
			&bytes_after, (unsigned char **)(&prop)) != Success) {
		_Ximp_SetupFree(version, imstyle, keylist, server_name, NULL, NULL, NULL);
		return(False);
	}
	if((server_vl = (char *)Xmalloc((sizeof(char) * nitems + 1))) == NULL) {
		_Ximp_SetupFree(version, imstyle, keylist, server_name, NULL, NULL, NULL);
		XFree(prop);
		return(False);
	}
	strncpy(server_vl, prop, nitems);
	server_vl[nitems] = '\0';
	XFree(prop);

	/* Get Property : _XIMP_VENDORNAME */
	vendorname_id  = XInternAtom(dpy, _XIMP_VENDORNAME, False);
	if( XGetWindowProperty(dpy, fe_window_id, vendorname_id, 0L, 1000000L, False,
			XA_STRING, &actual_type, &actual_format, &nitems,
			&bytes_after, (unsigned char **)(&prop)) != Success) {
		_Ximp_SetupFree(version, imstyle, keylist, server_name, NULL, NULL, NULL);
		return(False);
	}
	if((vendor_name = (char *)Xmalloc((sizeof(char) * nitems + 1))) == NULL) {
		_Ximp_SetupFree(version, imstyle, keylist, server_name, NULL, NULL, NULL);
		XFree(prop);
		return(False);
	}
	strncpy(vendor_name, prop, nitems);
	vendor_name[nitems] = '\0';
	XFree(prop);

	/* Get Property : _XIMP_EXTENSIONS */
	extentions_id = XInternAtom(dpy, _XIMP_EXTENSIONS, False);
	if( XGetWindowProperty(dpy, fe_window_id, extentions_id, 0L, 1000000L, False,
			extentions_id, &actual_type, &actual_format, &nitems,
			&bytes_after, (unsigned char **)(&prop_int)) != Success) {
		_Ximp_SetupFree(version, imstyle, keylist, server_name, server_vl, vendor_name, NULL);
		return(False);
	}
	if((ext_list = (Atom *)Xmalloc((sizeof(Atom) * (nitems + 1)))) == NULL) {
		_Ximp_SetupFree(version, imstyle, keylist, server_name, server_vl, vendor_name, NULL);
		if( prop_int )
		    XFree((XPointer)prop_int);
		return(False);
	}
	for(i=0; i < nitems; i++)
		ext_list[i] = prop_int[i];
	ext_list[nitems] = NULL;
	if( prop_int )
	    XFree((XPointer)prop_int);

	im->ximp_impart->fe_window	    = fe_window_id;
	Protocol_ID                 = XInternAtom(dpy, _XIMP_PROTOCOL, False);
	im->ximp_impart->improtocol_id  = Protocol_ID;
	im->ximp_impart->version_id     = version_id;
	im->ximp_impart->style_id       = style_id;
	im->ximp_impart->keys_id        = keys_id;
	im->ximp_impart->servername_id  = servername_id;
	im->ximp_impart->serverversion_id = serverversion_id;
	im->ximp_impart->vendorname_id  = vendorname_id;
	im->ximp_impart->extentions_id  = extentions_id;
	im->ximp_impart->ctext_id       = XInternAtom(dpy, _XIMP_CTEXT, False);
	im->ximp_impart->focus_win_id   = XInternAtom(dpy, _XIMP_FOCUS, False);
	im->ximp_impart->preedit_atr_id = XInternAtom(dpy, _XIMP_PREEDIT, False);
	im->ximp_impart->status_atr_id  = XInternAtom(dpy, _XIMP_STATUS, False);
	im->ximp_impart->preeditfont_id = XInternAtom(dpy, _XIMP_PREEDITFONT, False);
	im->ximp_impart->statusfont_id  = XInternAtom(dpy, _XIMP_STATUSFONT, False);
	im->ximp_impart->preeditmaxsize_id = XInternAtom(dpy, _XIMP_PREEDITMAXSIZE, False);
	im->ximp_impart->im_proto_vl = version;
	im->ximp_impart->im_styles   = imstyle;;
	im->ximp_impart->im_keyslist = keylist;
	im->ximp_impart->im_server_name = server_name;
	im->ximp_impart->im_server_vl = server_vl;
	im->ximp_impart->im_vendor_name = vendor_name;
	im->ximp_impart->im_ext_list  = ext_list;

	if(Ximp_Xim_List == (Ximp_XIM *)NULL){
		Ximp_Xim_List = (Ximp_XIM *)Xmalloc(sizeof(Ximp_XIM));
		if( Ximp_Xim_List == NULL ) {
		    _Ximp_SetupFree(version, imstyle, keylist, server_name, server_vl, vendor_name, NULL);
		    return( False );
		}
		Ximp_Xim_List[0] = im;
		Ximp_Xim_count = 1;
	}
	else {
		Ximp_XIM	*ximp_xim;

		n = 0;
		for(i=0; i < Ximp_Xim_count; i++) {
			if( Ximp_Xim_List[i] == im ) {
				n = 1;
				break;
			}
			if(Ximp_Xim_List[i] == NULL) {
				Ximp_Xim_List[i] = im;
				n = 1;
				break;
			}
		}
		if(n == 0) {
			ximp_xim = (Ximp_XIM *)Xrealloc(Ximp_Xim_List, ((i + 1) * sizeof(Ximp_XIM)));
			if( ximp_xim == NULL ) {
			    _Ximp_SetupFree(version, imstyle, keylist, server_name, server_vl, vendor_name, NULL);
			    return( False );
			}
			Ximp_Xim_List = ximp_xim;
			Ximp_Xim_List[Ximp_Xim_count] = im;
			Ximp_Xim_count++;
		}
	}

	_Ximp_SetupExtension(im);

	_XRegisterFilterByType(im->core.display, fe_window_id,
			       DestroyNotify, DestroyNotify,
			       _Ximp_XimFilter_Destroy, NULL);
	XSelectInput(im->core.display, fe_window_id, StructureNotifyMask);

	im->ximp_impart->connectserver  = 1;
	return(True);
}

int
_Ximp_SetupFree(proto_vl, style_list, keys_list, server_name, server_vl, 
		vendor_name, ext_list)
	char		*proto_vl;
	XIMStyles	*style_list;
	Ximp_KeyList	*keys_list;
	char		*server_name;
	char		*server_vl;
	char		*vendor_name;
	Atom		*ext_list;
{
	if(proto_vl)
		Xfree(proto_vl);
	if(style_list) {
		Xfree(style_list->supported_styles);
		Xfree(style_list);
	}
	if(keys_list) {
		Xfree(keys_list->keys_list);
		Xfree(keys_list);
	}
	if(server_name)
		Xfree(server_name);
	if(server_vl)
		Xfree(server_vl);
	if(vendor_name)
		Xfree(vendor_name);
	if(ext_list)
		Xfree(ext_list);
}

Ximp_XIC
_Ximp_LookupXIC(icid)
	int	icid;
{
	int		i;
	Ximp_XIM	pim;
	Ximp_XIC	pic;

	for(i = 0; i < Ximp_Xim_count; i++) {
		if( (pim = Ximp_Xim_List[i]) == NULL )
			continue;
		for(pic = (Ximp_XIC)pim->core.ic_chain; pic; pic = (Ximp_XIC)pic->core.next) {
			if(pic->ximp_icpart->icid == icid)
				return(pic);
		}
	}
	return(NULL);
}

static Bool
_Ximp_GetStyle(im, p_style)
	Ximp_XIM	im;
	XIMStyles	**p_style;
{
	XIMStyles	*p;
	int		i;

	if(!(im->ximp_impart->connectserver)) {
		*p_style = (XIMStyles *)NULL;
		return(False);
	}
	p = im->ximp_impart->im_styles;
	if((*p_style = (XIMStyles *)Xmalloc(sizeof(XIMStyles)
			    + p->count_styles * sizeof(XIMStyle))) == NULL)
		return(False);
	(*p_style)->count_styles = p->count_styles;
	(*p_style)->supported_styles = (XIMStyle *)((char *)*p_style + sizeof(XIMStyles));
	for(i=0; i < (int)p->count_styles; i++) {
		(*p_style)->supported_styles[i] = p->supported_styles[i];
	}
	return(True);
}

Atom
_Ximp_Protocol_id()
{
	return(Protocol_ID);
}
