/*
 * Image.c -- An extension to the Server_image object to support
 * xv_find().  This is done by creating a list of all the image
 * objects of this type and attaching it to the owner (screen).
 * The "screen" is chosen because when xv_find() is called, you
 * need to find the object associated with the same "screen" as
 * the owner's screen, otherwise what we return may not render
 * correctly.
 */
#include "image_impl.h"

static int image_init(), image_destroy();
static Xv_opaque image_get(), image_set(), image_find();
static void image_repaint();

Xv_pkg image_pkg = {
    "Image",                    /* package name */
    ATTR_PKG_IMAGE,             /* package ID */
    sizeof(Image_public),       /* size of the public struct */
    SERVER_IMAGE,               /* subclassed from the server image */
    image_init,
    image_set,
    image_get,
    image_destroy,
    image_find
};

/* initialize the image object -- create (alloc) an instance of it and
 * connect the public and private parts.  Since this package supports
 * xv_find(), createa linked list and/or append the newly created
 * instance of this object to the end of the list.
 */
static int
image_init(owner, image_public, avlist)
Xv_Screen       owner;
Image_public   *image_public;
Attr_avlist     avlist;            /* ignored here */
{
    Attr_attribute *attrs;
    Image_private *image_private = xv_alloc(Image_private);
    Image_private *list; /* linked list of image instances */
    Xv_Screen screen = owner? owner : xv_default_screen;

    if (!image_private || !screen)
        return XV_ERROR;

    /* link the public to the private and vice-versa */
    image_public->private_data = (Xv_opaque)image_private;
    image_private->public_self = (Xv_opaque)image_public;

    for (attrs = avlist; *attrs; attrs = attr_next(attrs))
        if (attrs[0] == SERVER_IMAGE_BITMAP_FILE)
            image_private->filename =
                strcpy(malloc(strlen(attrs[1])+1), attrs[1]);

    image_private->next = (Image_private *)NULL;
    image_private->screen = screen;

    /* get the list of existing images from the screen */
    if (list = (Image_private *)xv_get(screen,
                    XV_KEY_DATA, ATTR_PKG_IMAGE)) {
        /* follow list till the end */
        while (list->next)
            list = list->next;
        /* assign new image object to end of list */
        list->next = image_private;
    } else {
        /* no image objects on this screen -- create a new list */
        xv_set(screen,
            XV_KEY_DATA, ATTR_PKG_IMAGE, image_private,
            NULL);
    }
    return XV_OK;
}

/* image_set() -- needed to track whether "filename" changes */
static Xv_opaque
image_set(image_public, avlist)
Image_public   *image_public;
Attr_avlist     avlist;
{
    Attr_attribute *attrs;
    Image_private *image_private = IMAGE_PRIVATE(image_public);

    /* loop thru attrs looking for anything that would invalidate
     * the fact that the filename is set to a valid file.  If the
     * programmer is assigning a new pixmap or data to this server
     * image, the filename that was originally associated with the
     * object is no longer valid.  Disable for later get/find calls.
     */
    if (image_private->filename)
        for (attrs = avlist; *attrs; attrs = attr_next(attrs))
            if (attrs[0] == SERVER_IMAGE_PIXMAP ||
                attrs[0] == SERVER_IMAGE_BITS   ||
                attrs[0] == SERVER_IMAGE_X_BITS) {
                    free(image_private->filename);
                    image_private->filename = NULL;
            }

    return (Xv_opaque)XV_OK;
}

/* image_get() -- Support xv_get() for SERVER_IMAGE_BITMAP_FILE
 * and XV_SCREEN.  Warning: because this package is subclassed
 * from the Server_image package, we must not attempt to return
 * values for attrs that the Server_image package supports.
 */
static Xv_opaque
image_get(image_public, status, attr, args)
Image_public   *image_public;
int            *status;
Attr_attribute  attr;
Attr_avlist     args;
{
    Image_private *image_private = IMAGE_PRIVATE(image_public);

    switch ((int) attr) {
        case SERVER_IMAGE_BITMAP_FILE :
            return (Xv_opaque)image_private->filename;
        case XV_SCREEN :
            return (Xv_opaque)image_private->screen;
        default :
            *status = xv_check_bad_attr(IMAGE, attr);
            return (Xv_opaque)XV_OK;
    }
}

/* destroy method: free the object and remove from linked list */
static int
image_destroy(image_public, status)
Image_public    *image_public;
Destroy_status   status;
{
    Image_private *image_private = IMAGE_PRIVATE(image_public);
    Image_private *list; /* linked list of image instances */
    Xv_Screen screen = image_private->screen;

    if (status == DESTROY_CLEANUP) {
        /* get the list of existing images from the screen */
        list = (Image_private *)xv_get(screen,
                        XV_KEY_DATA, ATTR_PKG_IMAGE);
        if ((Image)XV_PUBLIC(list) == (Image)image_public)
            xv_set(screen,
                XV_KEY_DATA, ATTR_PKG_IMAGE, list->next,
                NULL);
        for ( ; list->next; list = list->next)
            if ((Image)XV_PUBLIC(list->next) == (Image)image_public) {
                list->next = list->next->next;
                break;
            }
        if (list->filename)
            free(list->filename);
        free(list);
    }

    return XV_OK;
}

/* image_find() -- find an existing image object that matches the
 * attribute-values specified here.  If none found, return NULL.
 * XView internals handle the XV_AUTO_CREATE case.  This function
 * supports finding images according to the following attributes:
 * XV_WIDTH, XV_HEIGHT, SERVER_IMAGE_DEPTH, SERVER_IMAGE_PIXMAP
 * and SERVER_IMAGE_BITMAP_FILE.  All others result in a NULL return.
 */
static Xv_opaque
image_find(owner, pkg, avlist)
Xv_Screen       owner;
Xv_pkg         *pkg;
Attr_avlist     avlist;            /* ignored here */
{
    Image_private *list; /* linked list of image instances */
    /* this is what the server image package does */
    Xv_Screen screen = owner? owner : xv_default_screen;
    Attr_attribute *attrs;
    /* consider all the attrs we allow "find" to match on */
    int     width = -1, height = -1, depth = -1;
    Pixmap  pixmap = (Pixmap)NULL;
    char   *filename = NULL;

    /* get the list of existing images from the screen */
    list = (Image_private *)xv_get(screen,
                    XV_KEY_DATA, ATTR_PKG_IMAGE);

    if (!list)
        return NULL;

    /* loop thru each attribute requested and save the value
     * associated with it.  Later, we'll loop thru the existing
     * objects looking for the object that has the same values.
     */
    for (attrs = avlist; *attrs; attrs = attr_next(attrs))
        switch ((int)attrs[0]) {
            case XV_WIDTH :
                width = (int)attrs[1];
                break;
            case XV_HEIGHT :
                height = (int)attrs[1];
                break;
            case SERVER_IMAGE_DEPTH :
                depth = (int)attrs[1];
                break;
            case SERVER_IMAGE_PIXMAP :
                pixmap = (Pixmap)attrs[1];
                break;
            case SERVER_IMAGE_BITMAP_FILE :
                filename = (char *)attrs[1];
                break;
            case SERVER_IMAGE_BITS :
            case SERVER_IMAGE_X_BITS :
            case SERVER_IMAGE_COLORMAP :
            case SERVER_IMAGE_CMS :
            case SERVER_IMAGE_SAVE_PIXMAP :
            default :
                return NULL; /* you can't "find" for these attrs */
        }
    /* Now loop thru each object looking for those whose
     * value that match those specified above.
     */
    for ( ; list; list = list->next) {
        /* If it doesn't match, continue to the next object in
         * the list.  Repeat for each requested attribute.
         */
        if (width > -1 &&
            (width != (int)xv_get(XV_PUBLIC(list), XV_WIDTH)))
            continue;
        if (height > -1 &&
            (height != (int)xv_get(XV_PUBLIC(list), XV_HEIGHT)))
            continue;
        if (depth > -1 && (depth != (int)xv_get(XV_PUBLIC(list),
                                    SERVER_IMAGE_DEPTH)))
            continue;
        if (pixmap && (pixmap != (Pixmap)xv_get(XV_PUBLIC(list),
                                    SERVER_IMAGE_PIXMAP)))
            continue;
        if (filename &&
            (!list->filename || strcmp(filename, list->filename)))
            continue;
        /* all matches seemed to be successful, return this object */
        return XV_PUBLIC(list);
    }
    /* nothing found */
    return NULL;
}
