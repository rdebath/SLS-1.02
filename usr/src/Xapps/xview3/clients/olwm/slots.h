/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)slots.h	26.7	91/09/14 SMI"

#ifndef _OLWM_SLOT_H
#define _OLWM_SLOT_H

typedef enum {SOTopToBottom, SOBottomToTop, SOLeftToRight, SORightToLeft} SlotOrder;

typedef struct _iconSlot {
	int 	ma, mi, maw, miw;
	Bool 	positioned;
} IconSlot;

typedef struct _iconGrid {
	SlotOrder	SOmajor,SOminor;
	int		*occupancy;
	int		minoccupancy;
	List		*iconList;
	int		maslots,mislots;
	int		slotshoriz,slotsvert;
	int		pixhoriz,pixvert;
} IconGrid;

extern IconGrid *SlotInit();	 /* Display *dpy, int screeno */
	/* initialises the Slots package; should be called for
	 * each screen
	 */

extern struct _iconSlot *SlotAlloc();	/* WinIcon *, Bool, Bool */
	/* given a sized and possibly positioned icon window, allocate 
	 * the appropriate slots for it.  If the window is positioned, 
	 * True should be passed for the second parameter, and the x,y
	 * position will be honoured.  If the window is not positioned, it
	 * will be positioned by this function to the appropriate slots(s).
	 * If the icon is being manually positioned and should be positioned
	 * according to the icon grid, True should be passed for the third
	 * parameter; False should be passed otherwise.
	 */

extern Bool SlotFree();		/* WinIcon * */
 	/* An icon is going away, so its references to slots should also go
	 * away. 
	 */

extern Bool SlotSetLocations();	/* Display *dpy */
	/* sets the order in which slots are allocated for icons which are
	 * not explicitly positioned.  The new order is obtained from the
	 * global resource vector.
	 * For example, the AlongBottom order is expressed as
	 * major BottomToTop, minor LeftToRight.  Any icons which were 
	 * automatically positioned are repositioned to equivalent positions
	 * in the new order.
	 */

#endif /* _OLWM_SLOT_H */
