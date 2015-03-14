#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)wizzy.h 1.2 91/09/14";
#endif
#endif

#include <xview/panel.h>

typedef Xv_panel_extension_item Xv_panel_wizzy;

extern Xv_pkg	    xv_panel_wizzy_pkg;

#define WIZZY	    &xv_panel_wizzy_pkg

/* Pick an Attribute ID between ATTR_PKG_UNUSED_FIRST and
 * ATTR_PKG_UNUSED_LAST.  The Attribute ID need only be
 * unique within the heirarchy for this object.
 */
#define ATTR_WIZZY ATTR_PKG_UNUSED_FIRST

#define WIZZY_ATTR(type, ordinal)	ATTR(ATTR_WIZZY, type, ordinal)

typedef enum {
    WIZZY_OFFSET	= WIZZY_ATTR(ATTR_INT,		1)
} Wizzy_attr;

