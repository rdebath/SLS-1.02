/* $XConsortium: TMkey.c,v 1.16 92/05/19 11:16:48 converse Exp $ */
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

#define XK_MISCELLANY
#define XK_LATIN1
#define XK_LATIN2
#define XK_LATIN3
#define XK_LATIN4

#include "IntrinsicI.h"
#include <X11/keysymdef.h>

#ifdef __STDC__
#define Const const
#else
#define Const /**/
#endif

#define FLUSHKEYCACHE(ctx) bzero((char *)&ctx->keycache, sizeof(TMKeyCache))

/*
 * The following array reorders the modifier bits so that the most common ones
 * (used by a translator) are in the top-most bits with respect to the size of
 * the keycache.  The array currently just reverses the bits as a good guess.
 * This might be more trouble than it is worth, but it seems to help.
 */

#define FM(i) i >> (8 - TMKEYCACHELOG2)
static Const unsigned char modmix[256] = {
FM(0x00), FM(0x80), FM(0x40), FM(0xc0), FM(0x20), FM(0xa0), FM(0x60), FM(0xe0),
FM(0x10), FM(0x90), FM(0x50), FM(0xd0), FM(0x30), FM(0xb0), FM(0x70), FM(0xf0),
FM(0x08), FM(0x88), FM(0x48), FM(0xc8), FM(0x28), FM(0xa8), FM(0x68), FM(0xe8),
FM(0x18), FM(0x98), FM(0x58), FM(0xd8), FM(0x38), FM(0xb8), FM(0x78), FM(0xf8),
FM(0x04), FM(0x84), FM(0x44), FM(0xc4), FM(0x24), FM(0xa4), FM(0x64), FM(0xe4),
FM(0x14), FM(0x94), FM(0x54), FM(0xd4), FM(0x34), FM(0xb4), FM(0x74), FM(0xf4),
FM(0x0c), FM(0x8c), FM(0x4c), FM(0xcc), FM(0x2c), FM(0xac), FM(0x6c), FM(0xec),
FM(0x1c), FM(0x9c), FM(0x5c), FM(0xdc), FM(0x3c), FM(0xbc), FM(0x7c), FM(0xfc),
FM(0x02), FM(0x82), FM(0x42), FM(0xc2), FM(0x22), FM(0xa2), FM(0x62), FM(0xe2),
FM(0x12), FM(0x92), FM(0x52), FM(0xd2), FM(0x32), FM(0xb2), FM(0x72), FM(0xf2),
FM(0x0a), FM(0x8a), FM(0x4a), FM(0xca), FM(0x2a), FM(0xaa), FM(0x6a), FM(0xea),
FM(0x1a), FM(0x9a), FM(0x5a), FM(0xda), FM(0x3a), FM(0xba), FM(0x7a), FM(0xfa),
FM(0x06), FM(0x86), FM(0x46), FM(0xc6), FM(0x26), FM(0xa6), FM(0x66), FM(0xe6),
FM(0x16), FM(0x96), FM(0x56), FM(0xd6), FM(0x36), FM(0xb6), FM(0x76), FM(0xf6),
FM(0x0e), FM(0x8e), FM(0x4e), FM(0xce), FM(0x2e), FM(0xae), FM(0x6e), FM(0xee),
FM(0x1e), FM(0x9e), FM(0x5e), FM(0xde), FM(0x3e), FM(0xbe), FM(0x7e), FM(0xfe),
FM(0x01), FM(0x81), FM(0x41), FM(0xc1), FM(0x21), FM(0xa1), FM(0x61), FM(0xe1),
FM(0x11), FM(0x91), FM(0x51), FM(0xd1), FM(0x31), FM(0xb1), FM(0x71), FM(0xf1),
FM(0x09), FM(0x89), FM(0x49), FM(0xc9), FM(0x29), FM(0xa9), FM(0x69), FM(0xe9),
FM(0x19), FM(0x99), FM(0x59), FM(0xd9), FM(0x39), FM(0xb9), FM(0x79), FM(0xf9),
FM(0x05), FM(0x85), FM(0x45), FM(0xc5), FM(0x25), FM(0xa5), FM(0x65), FM(0xe5),
FM(0x15), FM(0x95), FM(0x55), FM(0xd5), FM(0x35), FM(0xb5), FM(0x75), FM(0xf5),
FM(0x0d), FM(0x8d), FM(0x4d), FM(0xcd), FM(0x2d), FM(0xad), FM(0x6d), FM(0xed),
FM(0x1d), FM(0x9d), FM(0x5d), FM(0xdd), FM(0x3d), FM(0xbd), FM(0x7d), FM(0xfd),
FM(0x03), FM(0x83), FM(0x43), FM(0xc3), FM(0x23), FM(0xa3), FM(0x63), FM(0xe3),
FM(0x13), FM(0x93), FM(0x53), FM(0xd3), FM(0x33), FM(0xb3), FM(0x73), FM(0xf3),
FM(0x0b), FM(0x8b), FM(0x4b), FM(0xcb), FM(0x2b), FM(0xab), FM(0x6b), FM(0xeb),
FM(0x1b), FM(0x9b), FM(0x5b), FM(0xdb), FM(0x3b), FM(0xbb), FM(0x7b), FM(0xfb),
FM(0x07), FM(0x87), FM(0x47), FM(0xc7), FM(0x27), FM(0xa7), FM(0x67), FM(0xe7),
FM(0x17), FM(0x97), FM(0x57), FM(0xd7), FM(0x37), FM(0xb7), FM(0x77), FM(0xf7),
FM(0x0f), FM(0x8f), FM(0x4f), FM(0xcf), FM(0x2f), FM(0xaf), FM(0x6f), FM(0xef),
FM(0x1f), FM(0x9f), FM(0x5f), FM(0xdf), FM(0x3f), FM(0xbf), FM(0x7f), FM(0xff)
};
#undef FM

#define TRANSLATE(ctx,pd,dpy,key,mod,mod_ret,sym_ret) \
{ \
    int _i_ = (((key) - (pd)->min_keycode + modmix[(mod) & 0xff]) & \
	       (TMKEYCACHESIZE-1)); \
    if ((key) != 0 && /* Xlib XIM composed input */ \
	(ctx)->keycache.keycode[_i_] == (key) && \
	(ctx)->keycache.modifiers[_i_] == (mod)) { \
	mod_ret = (ctx)->keycache.modifiers_return; \
	sym_ret = (ctx)->keycache.keysym[_i_]; \
    } else { \
	XtTranslateKeycode(dpy, key, mod, &mod_ret, &sym_ret); \
	(ctx)->keycache.keycode[_i_] = key; \
	(ctx)->keycache.modifiers[_i_] = (unsigned short)(mod); \
	(ctx)->keycache.keysym[_i_] = sym_ret; \
	(ctx)->keycache.modifiers_return = mod_ret; \
    } \
}

/* usual number of expected keycodes in XtKeysymToKeycodeList */
#define KEYCODE_ARRAY_SIZE 10

static void _XtConvertCase();

Boolean _XtComputeLateBindings(lateModifiers,eventSeq,computed,computedMask)
    LateBindingsPtr lateModifiers;
    TMEventPtr eventSeq;
    Modifiers *computed,*computedMask;
{
    int i,j,ref;
    ModToKeysymTable* temp;
    XtPerDisplay perDisplay;
    Display *dpy;
    Boolean found;
    KeySym tempKeysym = NoSymbol;
    dpy = eventSeq->xev->xany.display;
    perDisplay = _XtGetPerDisplay(dpy);
    if (perDisplay == NULL) {
        XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
		"displayError","invalidDisplay",XtCXtToolkitError,
            "Can't find display structure",
            (String *)NULL, (Cardinal *)NULL);
         return FALSE;
    }
    _InitializeKeysymTables(dpy, perDisplay);
    for (ref=0; lateModifiers[ref].keysym; ref++) {
        found = FALSE;
        for (i=0;i<8;i++) {
            temp = &(perDisplay->modsToKeysyms[i]);
            for (j=0;j<temp->count;j++){
                if (perDisplay->modKeysyms[temp->idx+j] ==
		    lateModifiers[ref].keysym) {
                    *computedMask = *computedMask | temp->mask;
                    if (!lateModifiers[ref].knot)
		      *computed |= temp->mask;
                    tempKeysym = lateModifiers[ref].keysym;
                    found = TRUE; break;
                }
            }
            if (found) break;
        }
        if (!found  && !lateModifiers[ref].knot)
            if (!lateModifiers[ref].pair && (tempKeysym == NoSymbol))
                return FALSE;
        /* if you didn't find the modifier and the modifier must be
           asserted then return FALSE. If you didn't find the modifier
           and the modifier must be off, then it is OK . Don't
           return FALSE if this is the first member of a pair or if
           it is the second member of a pair when the first member
           was bound to a modifier */
    if (!lateModifiers[ref].pair) tempKeysym = NoSymbol;
    }
    return TRUE;
}

void _XtAllocTMContext(pd)
    XtPerDisplay pd;
{
    TMKeyContext ctx;
    ctx = (TMKeyContext)_XtHeapAlloc(&pd->heap,
				     sizeof(TMKeyContextRec));
    ctx->event = NULL;
    ctx->serial = 0;
    ctx->keysym = NoSymbol;
    ctx->modifiers = 0;
    FLUSHKEYCACHE(ctx);
    pd->tm_context = ctx;
}

Boolean _XtMatchUsingDontCareMods(typeMatch, modMatch, eventSeq)
    TMTypeMatch 	typeMatch;
    TMModifierMatch 	modMatch;
    TMEventPtr 		eventSeq;
{
    Modifiers modifiers_return;
    KeySym keysym_return;
    Modifiers useful_mods;
    int i;
    Modifiers computed = 0;
    Modifiers computedMask = 0;
    Boolean resolved = TRUE;
    Display *dpy = eventSeq->xev->xany.display;
    XtPerDisplay pd;
    TMKeyContext tm_context;
    
    if (modMatch->lateModifiers != NULL)
      resolved = _XtComputeLateBindings(modMatch->lateModifiers,
					eventSeq,&computed,&computedMask);
    if (!resolved) return FALSE;
    computed |= modMatch->modifiers;
    computedMask |= modMatch->modifierMask; /* gives do-care mask */
    
    if ( (computed & computedMask) ==
        (eventSeq->event.modifiers & computedMask) ) {
	Modifiers least_mod;
	
	pd = _XtGetPerDisplay(dpy);
	tm_context = pd->tm_context;
	TRANSLATE(tm_context, pd, dpy, (KeyCode)eventSeq->event.eventCode,
			    (unsigned)0, modifiers_return, keysym_return);
	
        if ((keysym_return & typeMatch->eventCodeMask)  == typeMatch->eventCode ) {
	    tm_context->event = eventSeq->xev;
	    tm_context->serial = eventSeq->xev->xany.serial;
	    tm_context->keysym = keysym_return;
	    tm_context->modifiers = (Modifiers)0;
	    return TRUE;
	}
        useful_mods = ~computedMask & modifiers_return;
        if (useful_mods == 0) return FALSE;
	for (least_mod = 1; (least_mod & useful_mods)==0; least_mod <<= 1){/*EMPTY*/};
        for (i = modifiers_return; i >= least_mod; i--)
	  /* all useful combinations of 8 modifier bits */
	  if (useful_mods & i) {
	      TRANSLATE(tm_context, pd, dpy, eventSeq->event.eventCode,
			(Modifiers)i, modifiers_return, keysym_return);
	      if (keysym_return  ==
		  (typeMatch->eventCode & typeMatch->eventCodeMask)) {
		  tm_context->event = eventSeq->xev;
		  tm_context->serial = eventSeq->xev->xany.serial;
		  tm_context->keysym = keysym_return;
		  tm_context->modifiers = (Modifiers)i;
		  return TRUE;
	      }
	  }
    }
    return FALSE;
}

void XtConvertCase(dpy,keysym,lower_return,upper_return)
    Display *dpy;
    KeySym keysym;
    KeySym *lower_return, *upper_return;
{
    XtPerDisplay pd = _XtGetPerDisplay(dpy);
    register CaseConverterPtr ptr;

    *lower_return = *upper_return = keysym;
    for (ptr=pd->case_cvt;  ptr; ptr = ptr->next)
	if (ptr->start <= keysym && keysym <= ptr->stop) {
	    (*ptr->proc)(dpy, keysym, lower_return, upper_return);
	    return;
	}
    if (keysym <= 0x3ff)	/* Latin-1 start = 0, Latin-4 stop = 0x3ff */
	_XtConvertCase(dpy, keysym, lower_return, upper_return);
}
    
Boolean _XtMatchUsingStandardMods (typeMatch, modMatch, eventSeq)
    TMTypeMatch typeMatch;
    TMModifierMatch modMatch;
    TMEventPtr eventSeq;
{
    Modifiers modifiers_return;
    KeySym keysym_return;
    Modifiers computed= 0;
    Modifiers computedMask = 0;
    Boolean resolved = TRUE;
    Display *dpy = eventSeq->xev->xany.display;
    XtPerDisplay pd = _XtGetPerDisplay(dpy);
    TMKeyContext tm_context = pd->tm_context;
    Modifiers translateModifiers;

    translateModifiers =(Modifiers)
      (eventSeq->event.modifiers & ((ShiftMask|LockMask) | pd->mode_switch));

    TRANSLATE(tm_context, pd, dpy, (KeyCode)eventSeq->event.eventCode,
			translateModifiers, modifiers_return, keysym_return);

    if ((typeMatch->eventCode & typeMatch->eventCodeMask) ==
             (keysym_return & typeMatch->eventCodeMask)) {
        if (modMatch->lateModifiers != NULL) 
            resolved = _XtComputeLateBindings(modMatch->lateModifiers,
					   eventSeq,&computed,&computedMask);
        if (!resolved) return FALSE;
        computed |= modMatch->modifiers;
        computedMask |= modMatch->modifierMask;

        if ((computed & computedMask) ==
	    (eventSeq->event.modifiers & ~modifiers_return & computedMask)) {
	    tm_context->event = eventSeq->xev;
	    tm_context->serial = eventSeq->xev->xany.serial;
	    tm_context->keysym = keysym_return;
	    tm_context->modifiers = translateModifiers;
	    return TRUE;
	}
    }
    return FALSE;
}


void _XtBuildKeysymTables(dpy,pd)
    Display *dpy;
    register XtPerDisplay pd;
{
    ModToKeysymTable *table;
    int maxCount,i,j,k,tempCount,idx;
    KeySym keysym,tempKeysym;
    XModifierKeymap* modKeymap;
    KeyCode keycode;
#define KeysymTableSize 16

    FLUSHKEYCACHE(pd->tm_context);
    if (pd->keysyms)
	XFree( (char *)pd->keysyms );
    XDisplayKeycodes(dpy, &pd->min_keycode, &pd->max_keycode);
    pd->keysyms_serial = NextRequest(dpy);
    pd->keysyms = XGetKeyboardMapping(dpy, pd->min_keycode,
				      pd->max_keycode-pd->min_keycode+1,
				      &pd->keysyms_per_keycode);
    if (pd->modKeysyms)
	XtFree((char *)pd->modKeysyms);
    if (pd->modsToKeysyms)
	XtFree((char *)pd->modsToKeysyms);
    pd->modKeysyms = (KeySym*)XtMalloc((Cardinal)KeysymTableSize*sizeof(KeySym));
    maxCount = KeysymTableSize;
    tempCount = 0;

    table = (ModToKeysymTable*)XtMalloc((Cardinal)8*sizeof(ModToKeysymTable));
    pd->modsToKeysyms = table;

    table[0].mask = ShiftMask;
    table[1].mask = LockMask;
    table[2].mask = ControlMask;
    table[3].mask = Mod1Mask;
    table[4].mask = Mod2Mask;
    table[5].mask = Mod3Mask;
    table[6].mask = Mod4Mask;
    table[7].mask = Mod5Mask;
    tempKeysym = 0;

    modKeymap = XGetModifierMapping(dpy);
    for (i=0;i<32;i++)
	pd->isModifier[i] = 0;
    pd->mode_switch = 0;
    for (i=0;i<8;i++) {
        table[i].idx = tempCount;
        table[i].count = 0;
        for (j=0;j<modKeymap->max_keypermod;j++) {
            keycode = modKeymap->modifiermap[i*modKeymap->max_keypermod+j];
            if (keycode != 0) {
		pd->isModifier[keycode>>3] |= 1 << (keycode & 7);
                for (k=0; k<pd->keysyms_per_keycode;k++) {
                    idx = ((keycode-pd->min_keycode)*
                             pd->keysyms_per_keycode)+k;
                    keysym = pd->keysyms[idx];
		    if ((keysym == XK_Mode_switch) && (i > 2))
			pd->mode_switch |= 1 << i;
                    if (keysym != 0 && keysym != tempKeysym ){
                        if (tempCount==maxCount) {
                            maxCount += KeysymTableSize;
                            pd->modKeysyms = (KeySym*)XtRealloc(
                                (char*)pd->modKeysyms,
                                (unsigned) (maxCount*sizeof(KeySym)) );
                        }
                        pd->modKeysyms[tempCount++] = keysym;
                        table[i].count++;
                        tempKeysym = keysym;
                    }
                }
            }
        }
    }
    pd->lock_meaning = NoSymbol;
    for (i = 0; i < table[1].count; i++) {
	keysym = pd->modKeysyms[table[1].idx + i];
	if (keysym == XK_Caps_Lock) {
	    pd->lock_meaning = XK_Caps_Lock;
	    break;
	} else if (keysym == XK_Shift_Lock) {
	    pd->lock_meaning = XK_Shift_Lock;
	}
    }
    XFreeModifiermap(modKeymap);
}

#if NeedFunctionPrototypes
void XtTranslateKeycode (
    Display *dpy, 
    _XtKeyCode keycode,
    Modifiers modifiers,
    Modifiers *modifiers_return,
    KeySym *keysym_return
    )
#else
void XtTranslateKeycode (dpy, keycode, modifiers,
                            modifiers_return, keysym_return)

    Display *dpy;
    KeyCode keycode;
    Modifiers modifiers;
    Modifiers *modifiers_return;
    KeySym *keysym_return;
#endif
{
    XtPerDisplay pd = _XtGetPerDisplay(dpy);
    _InitializeKeysymTables(dpy, pd);
    (*pd->defaultKeycodeTranslator)(
            dpy,keycode,modifiers,modifiers_return,keysym_return);
}

/* This code should match XTranslateKey (internal, sigh) in Xlib */
#if NeedFunctionPrototypes
void XtTranslateKey(
    register Display *dpy,
    _XtKeyCode keycode,
    Modifiers modifiers,
    Modifiers *modifiers_return,
    KeySym *keysym_return
    )
#else
void XtTranslateKey(dpy, keycode, modifiers,
                            modifiers_return, keysym_return)
    register Display *dpy;
    KeyCode keycode;
    Modifiers modifiers;
    Modifiers *modifiers_return;
    KeySym *keysym_return;
#endif
{
    register XtPerDisplay pd = _XtGetPerDisplay(dpy);
    int per;
    register KeySym *syms;
    KeySym sym, lsym, usym;

    *modifiers_return = (ShiftMask|LockMask) | pd->mode_switch;
    if (((int)keycode < pd->min_keycode) || ((int)keycode > pd->max_keycode)) {
	*keysym_return = NoSymbol;
	return;
    }
    per = pd->keysyms_per_keycode;
    syms = &pd->keysyms[(keycode - pd->min_keycode) * per];
    while ((per > 2) && (syms[per - 1] == NoSymbol))
	per--;
    if ((per > 2) && (modifiers & pd->mode_switch)) {
	syms += 2;
	per -= 2;
    }
    if (!(modifiers & ShiftMask) &&
	(!(modifiers & LockMask) || (pd->lock_meaning == NoSymbol))) {
	if ((per == 1) || (syms[1] == NoSymbol))
	    XtConvertCase(dpy, syms[0], keysym_return, &usym);
	else
	    *keysym_return = syms[0];
    } else if (!(modifiers & LockMask) ||
	       (pd->lock_meaning != XK_Caps_Lock)) {
	if ((per == 1) || ((usym = syms[1]) == NoSymbol))
	    XtConvertCase(dpy, syms[0], &lsym, &usym);
	*keysym_return = usym;
    } else {
	if ((per == 1) || ((sym = syms[1]) == NoSymbol))
	    sym = syms[0];
	XtConvertCase(dpy, sym, &lsym, &usym);
	if (!(modifiers & ShiftMask) && (sym != syms[0]) &&
	    ((sym != usym) || (lsym == usym)))
	    XtConvertCase(dpy, syms[0], &lsym, &usym);
	*keysym_return = usym;
    }

    if (*keysym_return == XK_VoidSymbol)
	*keysym_return = NoSymbol;
}

void XtSetKeyTranslator(dpy, translator)

    Display *dpy;
    XtKeyProc translator;

{
    XtPerDisplay pd = _XtGetPerDisplay(dpy);

    pd->defaultKeycodeTranslator = translator;
    FLUSHKEYCACHE(pd->tm_context);
    /* XXX should now redo grabs */
}

void XtRegisterCaseConverter(dpy, proc, start, stop)
    Display *dpy;
    XtCaseProc proc;
    KeySym start;
    KeySym stop;
{
    XtPerDisplay pd = _XtGetPerDisplay(dpy);
    CaseConverterPtr ptr, prev;

    ptr = (CaseConverterPtr) XtMalloc(sizeof(CaseConverterRec));
    ptr->start = start;
    ptr->stop = stop;
    ptr->proc = proc;
    ptr->next = pd->case_cvt;
    pd->case_cvt = ptr;

    /* Remove obsolete case converters from the list */
    prev = ptr;
    for (ptr=ptr->next; ptr; ptr=prev->next) {
	if (start <= ptr->start && stop >= ptr->stop) {
	    prev->next = ptr->next;
	    XtFree((char *)ptr);
	} 
	else prev = ptr;
    }
    FLUSHKEYCACHE(pd->tm_context);
    /* XXX should now redo grabs */
}

/* This code should match XConvertCase (internal, sigh) in Xlib */
/* ARGSUSED */
static void _XtConvertCase(dpy, sym, lower, upper)
    Display *dpy;
    KeySym sym;
    KeySym *lower;
    KeySym *upper;
{
    *lower = sym;
    *upper = sym;
    switch(sym >> 8) {
    case 0:
	if ((sym >= XK_A) && (sym <= XK_Z))
	    *lower += (XK_a - XK_A);
	else if ((sym >= XK_a) && (sym <= XK_z))
	    *upper -= (XK_a - XK_A);
	else if ((sym >= XK_Agrave) && (sym <= XK_Odiaeresis))
	    *lower += (XK_agrave - XK_Agrave);
	else if ((sym >= XK_agrave) && (sym <= XK_odiaeresis))
	    *upper -= (XK_agrave - XK_Agrave);
	else if ((sym >= XK_Ooblique) && (sym <= XK_Thorn))
	    *lower += (XK_oslash - XK_Ooblique);
	else if ((sym >= XK_oslash) && (sym <= XK_thorn))
	    *upper -= (XK_oslash - XK_Ooblique);
	break;
#ifdef XK_LATIN2
      case 1:
	/* Assume the KeySym is a legal value (ignore discontinuities) */
	if (sym == XK_Aogonek)
	    *lower = XK_aogonek;
	else if (sym >= XK_Lstroke && sym <= XK_Sacute)
	    *lower += (XK_lstroke - XK_Lstroke);
	else if (sym >= XK_Scaron && sym <= XK_Zacute)
	    *lower += (XK_scaron - XK_Scaron);
	else if (sym >= XK_Zcaron && sym <= XK_Zabovedot)
	    *lower += (XK_zcaron - XK_Zcaron);
	else if (sym == XK_aogonek)
	    *upper = XK_Aogonek;
	else if (sym >= XK_lstroke && sym <= XK_sacute)
	    *upper -= (XK_lstroke - XK_Lstroke);
	else if (sym >= XK_scaron && sym <= XK_zacute)
	    *upper -= (XK_scaron - XK_Scaron);
	else if (sym >= XK_zcaron && sym <= XK_zabovedot)
	    *upper -= (XK_zcaron - XK_Zcaron);
	else if (sym >= XK_Racute && sym <= XK_Tcedilla)
	    *lower += (XK_racute - XK_Racute);
	else if (sym >= XK_racute && sym <= XK_tcedilla)
	    *upper -= (XK_racute - XK_Racute);
	break;
#endif
#ifdef XK_LATIN3
      case 2:
	/* Assume the KeySym is a legal value (ignore discontinuities) */
	if (sym >= XK_Hstroke && sym <= XK_Hcircumflex)
	    *lower += (XK_hstroke - XK_Hstroke);
	else if (sym >= XK_Gbreve && sym <= XK_Jcircumflex)
	    *lower += (XK_gbreve - XK_Gbreve);
	else if (sym >= XK_hstroke && sym <= XK_hcircumflex)
	    *upper -= (XK_hstroke - XK_Hstroke);
	else if (sym >= XK_gbreve && sym <= XK_jcircumflex)
	    *upper -= (XK_gbreve - XK_Gbreve);
	else if (sym >= XK_Cabovedot && sym <= XK_Scircumflex)
	    *lower += (XK_cabovedot - XK_Cabovedot);
	else if (sym >= XK_cabovedot && sym <= XK_scircumflex)
	    *upper -= (XK_cabovedot - XK_Cabovedot);
	break;
#endif
#ifdef XK_LATIN4
      case 3:
	/* Assume the KeySym is a legal value (ignore discontinuities) */
	if (sym >= XK_Rcedilla && sym <= XK_Tslash)
	    *lower += (XK_rcedilla - XK_Rcedilla);
	else if (sym >= XK_rcedilla && sym <= XK_tslash)
	    *upper -= (XK_rcedilla - XK_Rcedilla);
	else if (sym == XK_ENG)
	    *lower = XK_eng;
	else if (sym == XK_eng)
	    *upper = XK_ENG;
	else if (sym >= XK_Amacron && sym <= XK_Umacron)
	    *lower += (XK_amacron - XK_Amacron);
	else if (sym >= XK_amacron && sym <= XK_umacron)
	    *upper -= (XK_amacron - XK_Amacron);
	break;
#endif
    }
}


KeySym *XtGetKeysymTable(dpy, min_keycode_return, keysyms_per_keycode_return)
    Display *dpy;
    KeyCode *min_keycode_return;
    int *keysyms_per_keycode_return;
{
    XtPerDisplay pd = _XtGetPerDisplay(dpy);
    _InitializeKeysymTables(dpy, pd);
    *min_keycode_return = pd->min_keycode; /* %%% */
    *keysyms_per_keycode_return = pd->keysyms_per_keycode;
    return pd->keysyms;
}

void XtKeysymToKeycodeList(dpy, keysym, keycodes_return, keycount_return)
    Display *dpy;
    KeySym keysym;
    KeyCode **keycodes_return;
    Cardinal *keycount_return;
{
    register XtPerDisplay pd = _XtGetPerDisplay(dpy);
    unsigned keycode;
    int per, match;
    register KeySym *syms;
    register int i, j;
    KeySym lsym, usym;
    unsigned maxcodes = 0;
    unsigned ncodes = 0;
    KeyCode *keycodes, *codeP;

    _InitializeKeysymTables(dpy, pd);
    keycodes = NULL;
    per = pd->keysyms_per_keycode;
    for (syms = pd->keysyms, keycode = (unsigned) pd->min_keycode;
	 (int)keycode <= pd->max_keycode;
	 syms += per, keycode++) {
	match = 0;
	for (j = 0; j < per; j++) {
	    if (syms[j] == keysym) {
		match = 1;
		break;
	    }		
	}
	if (!match)
	    for (i = 1; i < 5; i += 2) {
		if ((per == i) || ((per > i) && (syms[i] == NoSymbol))) {
		    XtConvertCase(dpy, syms[i-1], &lsym, &usym);
		    if ((lsym == keysym) || (usym == keysym)) {
			match = 1;
			break;
		    }
		}
	    }
	if (match) {
	    if (ncodes == maxcodes) {
		KeyCode *old = keycodes;
		maxcodes += KEYCODE_ARRAY_SIZE;
		keycodes = (KeyCode*)XtMalloc(maxcodes*sizeof(KeyCode));
		if (ncodes) {
		    bcopy( (char *)old, (char *)keycodes, ncodes*sizeof(KeyCode) );
		    XtFree((char *)old);
		}
		codeP = &keycodes[ncodes];
	    }
	    *codeP++ = (KeyCode) keycode;
	    ncodes++;
	}
    }
    *keycodes_return = keycodes;
    *keycount_return = ncodes;
}


