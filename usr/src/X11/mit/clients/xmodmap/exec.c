/*
 * xmodmap - program for loading keymap definitions into server
 *
 * $XConsortium: exec.c,v 1.12 91/07/18 10:25:57 rws Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 * Copyright 1987 by Sun Microsystems, Inc. Mountain View, CA.
 *
 *                     All Rights Reserved
 * 
 * Permission  to  use,  copy,  modify,  and  distribute   this
 * software  and  its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright no-
 * tice  appear  in all copies and that both that copyright no-
 * tice and this permission notice appear in  supporting  docu-
 * mentation,  and  that the names of Sun or MIT not be used in
 * advertising or publicity pertaining to distribution  of  the
 * software  without specific prior written permission. Sun and
 * M.I.T. make no representations about the suitability of this
 * software for any purpose. It is provided "as is" without any
 * express or implied warranty.
 * 
 * SUN DISCLAIMS ALL WARRANTIES WITH REGARD TO  THIS  SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-
 * NESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SUN BE  LI-
 * ABLE  FOR  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,  DATA  OR
 * PROFITS,  WHETHER  IN  AN  ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
 * THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Jim Fulton, MIT X Consortium; derived from parts of the
 * original xmodmap, written by David Rosenthal, of Sun Microsystems.
 */

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include "xmodmap.h"
#include "wq.h"

static mapping_busy_key (timeout)
    int timeout;
{
    int i;
    unsigned char keymap[32];
    static unsigned int masktable[8] = {
	0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80 };

    XQueryKeymap (dpy, (char *) keymap);

    fprintf (stderr,
	     "%s:  please release the following keys within %d seconds:\n",
	     ProgramName, timeout);
    for (i = 0; i < 256; i++) {
	if (keymap[i >> 3] & masktable[i & 7]) {
	    KeySym ks = XKeycodeToKeysym (dpy, (KeyCode) i, 0);
	    char *cp = XKeysymToString (ks);
	    fprintf (stderr, "    %s (keysym 0x%x, keycode %d)\n",
		     cp ? cp : "UNNAMED", ks, i);
	}
    }
    sleep (timeout);
    return;
}

static mapping_busy_pointer (timeout)
    int timeout;
{
    int i;
    Window root, child;			/* dummy variables */
    int rx, ry, wx, wy;			/* dummy variables */
    unsigned int mask;
    static unsigned int masks[5] = {
	Button1Mask, Button2Mask, Button3Mask, Button4Mask, Button5Mask };

    if (!XQueryPointer (dpy, RootWindow(dpy,DefaultScreen(dpy)),
			&root, &child, &rx, &ry, &wx, &wy, &mask))
      mask = 0;

    fprintf (stderr,
	     "%s:  please release the following buttons within %d seconds:\n",
	     ProgramName, timeout);
    for (i = 0; i < 5; i++) {
	if (mask & masks[i]) 
	  fprintf (stderr, "    Button%d\n", i+1);
    }
    sleep (timeout);
    return;
}


/*
 * UpdateModifierMapping - this sends the modifier map to the server
 * and deals with retransmissions due to the keyboard being busy.
 */

int UpdateModifierMapping (map)
    XModifierKeymap *map;
{
    int retries, timeout;

    for (retries = 5, timeout = 2; retries > 0; retries--, timeout *= 2) {
	int result;

	result = XSetModifierMapping (dpy, map);
	switch (result) {
	  case MappingSuccess:	/* Success */
	    return (0);
	  case MappingBusy:		/* Busy */
	    mapping_busy_key (timeout);
	    continue;
	  case MappingFailed:
	    fprintf (stderr, "%s: bad set modifier mapping.\n",
		     ProgramName);
	    return (-1);
	  default:
	    fprintf (stderr, "%s:  bad return %d from XSetModifierMapping\n",
		     ProgramName, result);
	    return (-1);
	}
    }
    fprintf (stderr,
	     "%s:  unable to set modifier mapping, keyboard problem\n",
	     ProgramName);
    return (-1);
}


/*
 * AddModifier - this adds a keycode to the modifier list
 */

int AddModifier (mapp, keycode, modifier)
    XModifierKeymap **mapp;
    KeyCode keycode;
    int modifier;
{
    if (keycode) {
	*mapp = XInsertModifiermapEntry (*mapp, keycode, modifier);
	return (0);
    } else {
	return (-1);
    }
    /*NOTREACHED*/
}


/*
 * DeleteModifier - this removes a keycode from the modifier list
 */

int RemoveModifier (mapp, keycode, modifier)
    XModifierKeymap **mapp;
    KeyCode keycode;
    int modifier;
{
    if (keycode) {
	*mapp = XDeleteModifiermapEntry (*mapp, keycode, modifier);
	return (0);
    } else {
	return (-1);
    }
    /*NOTREACHED*/
}


/*
 * ClearModifier - this removes all entries from the modifier list
 */

int ClearModifier (mapp, modifier)
    XModifierKeymap **mapp;
    int modifier;
{
    int i;
    XModifierKeymap *map = *mapp;
    KeyCode *kcp;

    kcp = &map->modifiermap[modifier * map->max_keypermod];
    for (i = 0; i < map->max_keypermod; i++) {
	*kcp++ = (KeyCode) 0;
    }
    return (0);
}


/*
 * print the contents of the map
 */

PrintModifierMapping (map, fp)
    XModifierKeymap *map;
    FILE *fp;
{
    int i, k = 0;

    fprintf (fp, 
    	     "%s:  up to %d keys per modifier, (keycodes in parentheses):\n\n", 
    	     ProgramName, map->max_keypermod);
    for (i = 0; i < 8; i++) {
	int j;

	fprintf(fp, "%-10s", modifier_table[i].name);
	for (j = 0; j < map->max_keypermod; j++) {
	    if (map->modifiermap[k]) {
		KeySym ks = XKeycodeToKeysym(dpy, map->modifiermap[k], 0);
		char *nm = XKeysymToString(ks);

		fprintf (fp, "%s  %s (0x%0x)", (j > 0 ? "," : ""), 
			 (nm ? nm : "BadKey"), map->modifiermap[k]);
	    }
	    k++;
	}
	fprintf(fp, "\n");
    }
    fprintf (fp, "\n");
    return;
}


PrintKeyTable (exprs, fp)
    Bool exprs;
    FILE *fp;
{
    int         i;
    int min_keycode, max_keycode, keysyms_per_keycode;
    KeySym *keymap, *origkeymap;

    XDisplayKeycodes (dpy, &min_keycode, &max_keycode);
    origkeymap = XGetKeyboardMapping (dpy, min_keycode,
				      (max_keycode - min_keycode + 1),
				      &keysyms_per_keycode);

    if (!origkeymap) {
	fprintf (stderr, "%s:  unable to get keyboard mapping table.\n",
		 ProgramName);
	return;
    }
    if (!exprs) {
	fprintf (fp, 
		 "There are %d KeySyms per KeyCode; KeyCodes range from %d to %d.\n\n", 
		 keysyms_per_keycode, min_keycode, max_keycode);
	fprintf (fp, "    KeyCode\tKeysym (Keysym)\t...\n");
	fprintf (fp, "    Value  \tValue   (Name) \t...\n\n");
    }
    keymap = origkeymap;
    for (i = min_keycode; i <= max_keycode; i++) {
	int  j, max;

	if (exprs)
	    fprintf(fp, "keycode %3d =", i);
	else
	    fprintf(fp, "    %3d    \t", i);
	max = keysyms_per_keycode - 1;
	while ((max >= 0) && (keymap[max] == NoSymbol))
	    max--;
	for (j = 0; j <= max; j++) {
	    register KeySym ks = keymap[j];
	    char *s;
	    if (ks != NoSymbol)
		s = XKeysymToString (ks);
	    else
		s = "NoSymbol";
	    if (!exprs)
		fprintf (fp, "0x%04x (%s)\t", ks, s ? s : "no name");
	    else if (s)
		fprintf (fp, " %s", s);
	    else
		fprintf (fp, " 0x%04x", ks);
	}
	keymap += keysyms_per_keycode;
	fprintf (fp, "\n");
    }

    XFree ((char *) origkeymap);
    return;
}


PrintPointerMap (fp)
    FILE *fp;
{
    unsigned char pmap[256];		/* there are 8 bits of buttons */
    int count, i;

    count = XGetPointerMapping (dpy, pmap, 256);

    fprintf (fp, "There are %d pointer buttons defined.\n\n", count);
    fprintf (fp, "    Physical        Button\n");
    fprintf (fp, "     Button          Code\n");
/*               "      ###            ###\n"               */
    for (i = 0; i < count; i++) {
	fprintf (fp, "      %3u            %3u\n", 
		 i+1, (unsigned int) pmap[i]);
    }
    fprintf (fp, "\n");
    return;
}


/*
 * SetPointerMap - set the pointer map
 */

int SetPointerMap (map, n)
    unsigned char *map;
    int n;
{
    unsigned char defmap[MAXBUTTONCODES];
    int j;
    int retries, timeout;

    if (n == 0) {				/* reset to default */
	n = XGetPointerMapping (dpy, defmap, MAXBUTTONCODES);
	for (j = 0; j < n; j++) defmap[j] = (unsigned char) (j + 1);
	map = defmap;
    }

    for (retries = 5, timeout = 2; retries > 0; retries--, timeout *= 2) {
	int result;

	switch (result = XSetPointerMapping (dpy, map, n)) {
	  case MappingSuccess:
	    return 0;
	  case MappingBusy:
	    mapping_busy_pointer (timeout);
	    continue;
	  case MappingFailed:
	    fprintf (stderr, "%s:  bad pointer mapping\n", ProgramName);
	    return -1;
	  default:
	    fprintf (stderr, "%s:  bad return %d from XSetPointerMapping\n",
		     ProgramName, result);
	    return -1;
	}
    }
    fprintf (stderr, "%s:  unable to set pointer mapping\n", ProgramName);
    return -1;
}
