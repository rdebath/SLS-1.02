#ident	"@(#)pscanvas.h	1.22 3/1/91 Copyright 1990 Sun Microsystems, Inc."

#ifndef	pscanvas_defined
#define	pscanvas_defined

#include <xview/openwin.h>
#include <xview/win_input.h>
#include <xview/xv_xrect.h>

#if defined(__cplusplus)
extern  "C" {    
#include "pscan_ps.h"
}
#else
#include "pscan_ps.h"
#endif   /*__cplusplus */


#define PSCANVAS_NOSYNC	-1
#define NeWStoken int
int ps_token_from_xid(), ps_token_from_code(), ps_end_stopped();
void pscanvas_show_error();

#define PSCANVAS	&xv_pscanvas_pkg

#define ATTR_PKG_PSCANVAS	(ATTR_PKG_UNUSED_LAST - 1)
#define PSCANVAS_ATTR(type, ordinal)	\
			ATTR (ATTR_PKG_PSCANVAS, type, ordinal)
#define PSCANVAS_TYPE	ATTR_PKG_PSCANVAS

/*
 * Types
 */
typedef Xv_opaque	PScanvas;
typedef Xv_opaque	PSview;

/*
 * Enumerations
 */
typedef enum {
	/*
	 * Public Attributes
	 */
	PSCANVAS_SYNC		= PSCANVAS_ATTR(ATTR_INT,		 3),
	PSCANVAS_INPUT_PROC	= PSCANVAS_ATTR(ATTR_FUNCTION_PTR,	 7),
	PSCANVAS_REPAINT_PROC	= PSCANVAS_ATTR(ATTR_FUNCTION_PTR,	10),
	PSCANVAS_RESIZE_PROC	= PSCANVAS_ATTR(ATTR_FUNCTION_PTR,	15),
	PSCANVAS_SCROLL_PROC	= PSCANVAS_ATTR(ATTR_FUNCTION_PTR,	20),
	PSCANVAS_CLIPRECTS	= PSCANVAS_ATTR(ATTR_OPAQUE,		30),
	PSCANVAS_NEWSTOKEN	= PSCANVAS_ATTR(ATTR_INT,		50),
} PScanvas_attr;

typedef enum {
	/*
	 * Public Attributes
	 */
	PSVIEW_PSCANVAS		= PSCANVAS_ATTR(ATTR_OPAQUE,		30),
} PSview_attr;

/*
 * Structures
 */
typedef struct {
    Xv_openwin		parent_data;
    Xv_opaque		private_data;
} Xv_pscanvas;

typedef struct {
    Xv_window_struct	parent_data;
    Xv_opaque		private_data;
} Xv_psview;

/*
 * Globals
 */

/*
 * Variables
 */
extern Xv_pkg		xv_pscanvas_pkg;
extern Xv_pkg		xv_psview_pkg;

/*
 * Functions
 */
EXTERN_FUNCTION(Event *pscanvas_xlate_event_xy, (PScanvas can, Event *event));
#endif	pscanvas_defined
