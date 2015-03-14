/*
 * Logo.c -- a XView object class that paints an X logo in a window.
 * This object is subclassed from the window object to take advantage
 * of the window it creates.  This object has no attributes, so the
 * set and get functions are virtually empty.  The only internal
 * fields used by this object are a GC and a Pixmap.  The GC is used
 * to paint the Pixmap into the window.  The window object has no GC
 * associated with it or we would have inherited it.  This will
 * probably go away in the next version of XView.
 */
#include "logo_impl.h"
#include <xview/notify.h>
#include <xview/cms.h>
#include <X11/bitmaps/xlogo32>

/* declare the "methods" used by the logo class. */
static int logo_init(), logo_destroy();
static Xv_opaque logo_set(), logo_get();
static void logo_repaint();

Xv_pkg logo_pkg = {
    "Logo",                     /* package name */
    ATTR_PKG_UNUSED_FIRST,      /* package ID */
    sizeof(Logo_public),        /* size of the public struct */
    WINDOW,                     /* subclassed from the window package */
    logo_init,
    logo_set,
    logo_get,
    logo_destroy,
    NULL                        /* disable the use of xv_find() */
};

/* the only thing this object does is paint an X into its own window.
 * This is the event handling routine that is used to check for
 * Expose or Configure event requests.  the configure event clears
 * the window and the "expose" event causes a repaint of the X image.
 * The GC has it's foreground and background colros set from the
 * CMS of the window from which this logo object is subclassed.
 */
static void
logo_redraw(logo_public, event)
Logo_public     *logo_public;
Event           *event;
{
    Logo_private *logo_private = LOGO_PRIVATE(logo_public);
    XEvent *xevent = event_xevent(event);

    if (xevent->xany.type == Expose && xevent->xexpose.count == 0) {
        Display *dpy = (Display *)xv_get(logo_public, XV_DISPLAY);
        Window window = (Window)xv_get(logo_public, XV_XID);
        int width = (int)xv_get(logo_public, XV_WIDTH);
        int height = (int)xv_get(logo_public, XV_HEIGHT);
        int x = (width - xlogo32_width)/2;
        int y = (height - xlogo32_height)/2;

        XCopyPlane(dpy, logo_private->bitmap, window, logo_private->gc,
            0, 0, xlogo32_width, xlogo32_height, x, y, 1L);
    } else if (xevent->xany.type == ConfigureNotify)
        XClearArea(xv_get(logo_public, XV_DISPLAY),
            xv_get(logo_public, XV_XID), 0, 0,
            xevent->xconfigure.width, xevent->xconfigure.height, True);
}

/* initialize the logo object -- create (alloc) an instance of it.
 * There are two parts to an object class: a public part and a private
 * part.  Each contains a pointer to the other, so link the two
 * together and initialize the remaining fields of the logo data
 * structure.  This includes creating the Xlogo pixmap.  However,
 * we do no initialize the logo's GC because it is dependent on its
 * window's cms and that isn't assigned to the window till the "set"
 * method.  See logo_set() below.
 */
static int
logo_init(owner, logo_public, avlist)
Xv_opaque       owner;
Logo_public     *logo_public;
Attr_avlist     avlist; /* ignored here */
{
    Logo_private *logo_private = xv_alloc(Logo_private);
    Display *dpy;
    Window win;

    if (!logo_private)
        return XV_ERROR;

    dpy = (Display *)xv_get(owner, XV_DISPLAY);
    win = (Window)xv_get(logo_public, XV_XID);

    /* link the public to the private and vice-versa */
    logo_public->private_data = (Xv_opaque)logo_private;
    logo_private->public_self = (Xv_opaque)logo_public;

    /* create the 1-bit deep pixmap of the X logo */
    if ((logo_private->bitmap = XCreatePixmapFromBitmapData(dpy, win,
        xlogo32_bits, xlogo32_width, xlogo32_height,
        1, 0, 1)) == NULL) {
        free(logo_private);
        return XV_ERROR;
    }
    /* set up event handlers to get resize and repaint events */
    xv_set(logo_public,
        WIN_NOTIFY_SAFE_EVENT_PROC,      logo_redraw,
        WIN_NOTIFY_IMMEDIATE_EVENT_PROC, logo_redraw,
        NULL);

    return XV_OK;
}

/* logo_set() -- the function called to set attributes in a logo
 * object.  This function is called when a logo is created after
 * the init routine as well as when the programmer calls xv_set.
 */
static Xv_opaque
logo_set(logo_public, avlist)
Logo_public *logo_public;
Attr_avlist avlist;
{
    Logo_private *logo_private = LOGO_PRIVATE(logo_public);
    Attr_attribute *attrs;

    for (attrs = avlist; *attrs; attrs = attr_next(attrs))
        switch ((int) attrs[0]) {
            case XV_END_CREATE : {
                /* this stuff *must* be here rather than in the "init"
                 * routine because the CMS is not loaded into the
                 * window object until the "set" routines are called.
                 */
                Cms cms = xv_get(logo_public, WIN_CMS);
                XGCValues gcvalues;
                Display *dpy =
                    (Display *)xv_get(logo_public, XV_DISPLAY);
                gcvalues.foreground =
                    (unsigned long)xv_get(cms, CMS_FOREGROUND_PIXEL);
                gcvalues.background =
                    (unsigned long)xv_get(cms, CMS_BACKGROUND_PIXEL);
                gcvalues.graphics_exposures = False;
                logo_private->gc = XCreateGC(dpy,
                    xv_get(logo_public, XV_XID),
                    GCForeground|GCBackground|GCGraphicsExposures,
                    &gcvalues);
            }
            default :
                xv_check_bad_attr(LOGO, attrs[0]);
                break;
        }

    return XV_OK;
}

/* logo_get() -- There are no logo attributes to get, so just return */
static Xv_opaque
logo_get(logo_public, status, attr, args)
Logo_public     *logo_public;
int             *status;
Attr_attribute  attr;
Attr_avlist     args;
{
    *status = xv_check_bad_attr(LOGO, attr);
    return (Xv_opaque)XV_OK;
}

/* destroy method: free the pixmap and the GC before freeing the object */
static int
logo_destroy(logo_public, status)
Logo_public     *logo_public;
Destroy_status   status;
{
    Logo_private *logo_private = LOGO_PRIVATE(logo_public);

    if (status == DESTROY_CLEANUP) {
        XFreePixmap(xv_get(logo_public, XV_DISPLAY),
            logo_private->bitmap);
        XFreeGC(xv_get(logo_public, XV_DISPLAY), logo_private->gc);
        free(logo_private);
    }

    return XV_OK;
}
