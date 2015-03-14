#include "bitmap.h"

typedef struct {
    Xv_object   public_self;    /* pointer back to self */
    GC          gc;             /* GC to render logo */
    Pixmap      bitmap;
    int         width, height;  /* ...of pixmap */
} Bitmap_private;

#define BITMAP_PUBLIC(item)  XV_PUBLIC(item)
#define BITMAP_PRIVATE(item) \
    XV_PRIVATE(Bitmap_private, Bitmap_public, item)
