/*
 * a useful library routine for getting the type of an XView object.
 * Typical use:
 *    printf("object is: %s\n", type_to_string(xv_get(obj, XV_TYPE)));
 */
#include <xview/xview.h>
#include <xview/textsw.h>
#include <xview/panel.h>
#include <xview/server.h>
#include <xview/font.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/ttysw.h>
#include <xview/tty.h>
#include <xview/text.h>

char *
type_to_string(type)
long type;
{
    static char *sprintf(), buf[32];
    switch (type) {
	case CANVAS_TYPE : return "canvas";
	case FONT_TYPE : return "font";
	case FRAME_TYPE : return "frame";
	case ICON_TYPE : return "icon";
	case MENU_TYPE : return "menu";
	case PANEL_TYPE : return "panel";
	case SCREEN_TYPE : return "screen";
	case SERVER_IMAGE_TYPE : return "server image";
	case TERMSW_TYPE : return "term";
	case TERMSW_VIEW_TYPE : return "termsw view";
	case TERMSW_MODE_TYPE : return "termsw mode";
	case TTYSW_MODE_TYPE : return "ttysw mode";
	case TEXTSW_TYPE : return "text subwindow";
	case TTY_VIEW_TYPE : return "tty view";
	case TTY_TYPE : return "tty";
	case WINDOW_TYPE : return "window";
	case WIN_MESSAGE_TYPE : return "win message";
	default :
	    return sprintf(buf, "%x: Unknown type", type);
    }
}
