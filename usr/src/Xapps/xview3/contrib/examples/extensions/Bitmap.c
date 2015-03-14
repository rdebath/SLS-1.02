/*
 * Bitmap.c -- an XView object class that displays an arbitrary
 * pixmap.  This is similar to the Logo object, but the programmer
 * may specify the bitmap to use via the BITMAP_FILE attribute.
 */
#include "bitmap_impl.h"
#include <xview/notify.h>
#include <xview/cms.h>
#include <X11/Xutil.h>

/* declare the "methods" used by the bitmap class. */
static int bitmap_init(), bitmap_destroy();
static Xv_opaque bitmap_set(), bitmap_get();
static void bitmap_repaint();

Xv_pkg bitmap_pkg = {
    "Bitmap2",               /* package name */
    ATTR_PKG_BITMAP,         /* package ID */
    sizeof(Bitmap_public),   /* size of the public struct */
    WINDOW,                  /* subclassed from the window package */
    bitmap_init,
    bitmap_set,
    bitmap_get,
    bitmap_destroy,
    NULL                     /* disable the use of xv_find() */
};

static void
bitmap_redraw(bitmap_public, event)
Bitmap_public     *bitmap_public;
Event           *event;
{
    Bitmap_private *bitmap_private = BITMAP_PRIVATE(bitmap_public);
    XEvent *xevent = event_xevent(event);

    if (bitmap_private->bitmap &&
        xevent->xany.type == Expose && xevent->xexpose.count == 0) {
        Display *dpy = (Display *)xv_get(bitmap_public, XV_DISPLAY);
        Window window = (Window)xv_get(bitmap_public, XV_XID);
        int width = (int)xv_get(bitmap_public, XV_WIDTH);
        int height = (int)xv_get(bitmap_public, XV_HEIGHT);
        int x = (width - bitmap_private->width)/2;
        int y = (height - bitmap_private->height)/2;

        XCopyPlane(dpy, bitmap_private->bitmap, window,
            bitmap_private->gc, 0, 0, bitmap_private->width,
            bitmap_private->height, x, y, 1L);
    } else if (xevent->xany.type == ConfigureNotify)
        XClearArea(xv_get(bitmap_public, XV_DISPLAY),
            xv_get(bitmap_public, XV_XID), 0, 0,
            xevent->xconfigure.width, xevent->xconfigure.height,
            True);
}

/* initialize the bitmap object by creating (alloc) an instance
 * of it.  There are two parts to an object class: a public part
 * and a private part.  Each contains a pointer to the other, so
 * link the two together and initialize the remaining fields of
 * the bitmap data structure.  Do no initialize the bitmap's GC
 * because it is dependent on its window's cms and that isn't
 * assigned to the window till the "set" method.  Also, wait till
 * till the "set" method to initialize the bitmap file specified.
 */
static int
bitmap_init(owner, bitmap_public, avlist)
Xv_opaque       owner;
Bitmap_public     *bitmap_public;
Attr_avlist     avlist; /* ignored here */
{
    Bitmap_private *bitmap_private = xv_alloc(Bitmap_private);

    if (!bitmap_private)
        return XV_ERROR;

    /* link the public to the private and vice-versa */
    bitmap_public->private_data = (Xv_opaque)bitmap_private;
    bitmap_private->public_self = (Xv_opaque)bitmap_public;

    /* set up event handlers to get resize and repaint events */
    xv_set(bitmap_public,
        WIN_NOTIFY_SAFE_EVENT_PROC,      bitmap_redraw,
        WIN_NOTIFY_IMMEDIATE_EVENT_PROC, bitmap_redraw,
        NULL);

    return XV_OK;
}

/* bitmap_set() -- the function called to set attributes in a bitmap
 * object.  This function is called when a bitmap is created after
 * the init routine as well as when the programmer calls xv_set.
 */
static Xv_opaque
bitmap_set(bitmap_public, avlist)
Bitmap_public *bitmap_public;
Attr_avlist avlist;
{
    Bitmap_private *bitmap_private = BITMAP_PRIVATE(bitmap_public);
    Attr_attribute *attrs;

    for (attrs = avlist; *attrs; attrs = attr_next(attrs))
        switch ((int) attrs[0]) {
            case BITMAP_FILE : {
                int val, x, y;
                Display *dpy =
                    (Display *)xv_get(bitmap_public, XV_DISPLAY);
                Window window =
                    (Window)xv_get(bitmap_public, XV_XID);
                Pixmap old = bitmap_private->bitmap;
                if (XReadBitmapFile(dpy, window, attrs[1],
                    &bitmap_private->width, &bitmap_private->height,
                    &bitmap_private->bitmap, &x, &y) != BitmapSuccess)
                {
                    xv_error(bitmap_public,
                        ERROR_STRING, "Unable to load bitmap file",
                        ERROR_PKG,    BITMAP,
                        NULL);
                    bitmap_private->bitmap = old;
                }
                break;
            }
            case BITMAP_PIXMAP :
                xv_error(bitmap_public,
                    ERROR_CANNOT_SET, attrs[0],
                    ERROR_PKG,        BITMAP,
                    NULL);
                break;
            case XV_END_CREATE : {
                /* this stuff *must* be here rather than in the "init"
                 * routine because the CMS is not loaded into the
                 * window object until the "set" routines are called.
                 */
                Cms cms = xv_get(bitmap_public, WIN_CMS);
                XGCValues gcvalues;
                Display *dpy =
                    (Display *)xv_get(bitmap_public, XV_DISPLAY);
                gcvalues.foreground =
                    (unsigned long)xv_get(cms, CMS_FOREGROUND_PIXEL);
                gcvalues.background =
                    (unsigned long)xv_get(cms, CMS_BACKGROUND_PIXEL);
                gcvalues.graphics_exposures = False;
                bitmap_private->gc =
                    XCreateGC(dpy, xv_get(bitmap_public, XV_XID),
                        GCForeground|GCBackground|GCGraphicsExposures,
                        &gcvalues);
            }
            default :
                xv_check_bad_attr(BITMAP, attrs[0]);
                break;
        }
    return XV_OK;
}

static Xv_opaque
bitmap_get(bitmap_public, status, attr, args)
Bitmap_public     *bitmap_public;
int             *status;
Attr_attribute  attr;
Attr_avlist     args;
{
    Bitmap_private *bitmap_private = BITMAP_PRIVATE(bitmap_public);

    switch ((int) attr) {
        case BITMAP_PIXMAP :
            return (Xv_opaque)bitmap_private->bitmap;
        case BITMAP_FILE : /* can't get this attribute */
        default :
            *status = xv_check_bad_attr(BITMAP, attr);
            return (Xv_opaque)XV_OK;
    }
}

/* destroy method: free the pixmap and the GC before freeing object */
static int
bitmap_destroy(bitmap_public, status)
Bitmap_public     *bitmap_public;
Destroy_status   status;
{
    Bitmap_private *bitmap_private = BITMAP_PRIVATE(bitmap_public);

    if (status == DESTROY_CLEANUP) {
        if (bitmap_private->bitmap)
            XFreePixmap(xv_get(bitmap_public, XV_DISPLAY),
                bitmap_private->bitmap);
        XFreeGC(xv_get(bitmap_public, XV_DISPLAY),
            bitmap_private->gc);
        free(bitmap_private);
    }
    return XV_OK;
}
