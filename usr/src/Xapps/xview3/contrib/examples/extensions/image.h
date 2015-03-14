#include <xview/xview.h>
#include <xview/svrimage.h>

extern Xv_pkg image_pkg;

#define IMAGE &image_pkg

typedef Xv_opaque Image;

#define ATTR_PKG_IMAGE        ATTR_PKG_UNUSED_FIRST+1

typedef struct {
    Xv_server_image   parent_data;
    Xv_opaque         private_data;
} Image_public;
