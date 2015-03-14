#include <stdio.h> /* for BUFSIZ */
#include "image.h"

typedef struct _image {
    Xv_object	   public_self;	        /* pointer back to self */
    char          *filename;            /* for get/find */
    Xv_Screen      screen;              /* need to retain for list */
    struct _image *next;	        /* linked list for find */
} Image_private;

#define IMAGE_PUBLIC(item)	XV_PUBLIC(item)
#define IMAGE_PRIVATE(item)	XV_PRIVATE(Image_private, Image_public, item)
