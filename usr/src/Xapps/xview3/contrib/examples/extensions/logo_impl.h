/* logo_impl.h -- implementation-dependent header file for the
 * logo XView class.
 */
#include "logo.h"

typedef struct {
    Xv_object	public_self;	/* pointer back to self */
    GC		gc;		/* GC to render logo */
    Pixmap	bitmap;		/* xlogo bitmap */
} Logo_private;

#define LOGO_PUBLIC(item)	XV_PUBLIC(item)
#define LOGO_PRIVATE(item)	XV_PRIVATE(Logo_private, Logo_public, item)
