/*							rtctl.h		*/

typedef struct
    {
    OPDS h;
    float   *kxrslt, *kyrslt;
    float   *iprd, *ixmin, *ixmax, *iymin, *iymax, *ixinit, *iyinit;
    int     countdown, timcount;
    XYINDAT w;		/* window specific data structure */
    } XYIN;

