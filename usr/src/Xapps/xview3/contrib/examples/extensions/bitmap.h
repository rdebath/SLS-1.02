#include <xview/xview.h>
#include <xview/window.h>

extern Xv_pkg bitmap_pkg;

#define BITMAP &bitmap_pkg

typedef Xv_opaque Bitmap;

#define ATTR_PKG_BITMAP            ATTR_PKG_UNUSED_FIRST
#define BITMAP_ATTR(type, ordinal) ATTR(ATTR_PKG_BITMAP, type, ordinal)

typedef enum {
    BITMAP_FILE     = BITMAP_ATTR(ATTR_STRING, 1),
    BITMAP_PIXMAP   = BITMAP_ATTR(ATTR_OPAQUE, 2), /* get-only */
};

typedef struct {
    Xv_window_struct   parent_data;
    Xv_opaque          private_data;
} Bitmap_public;
