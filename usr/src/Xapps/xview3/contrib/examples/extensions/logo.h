/* logo.h -- public header file for the logo XView class. */
#include <xview/xview.h>
#include <xview/window.h>

extern Xv_pkg logo_pkg;

#define LOGO &logo_pkg

typedef Xv_opaque Logo;

typedef struct {
    Xv_window_struct    parent_data;
    Xv_opaque           private_data;
} Logo_public;
