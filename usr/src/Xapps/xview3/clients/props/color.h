/*
 * @(#)color.h 1.4 91/09/14
 */

/*
 * Copyright (c) 1990 by Sun Microsystems, Inc.
 */

#define	MAXRGB	0xff
#define	MAXH	360
#define	MAXSV	MAXRGB

#define VMUL		12		/* brighten by 20% (12 = 1.2*10) */
#define SDIV		2		/* unsaturate by 50% (divide by 2) */
#define VMIN		((4*MAXSV)/10)	/* highlight brightness 40% minimum */

typedef struct {
    int         r,
                g,
                b;
}           RGB;

typedef struct {
    int         h,
                s,
                v;
}           HSV;

void hsv_to_rgb();
void rgb_to_hsv();
void rgb_to_xcolor();
void hsv_to_xcolor();
void xcolor_to_hsv();
void olgx_hsv_to_3D();
void olgx_calculate_3Dcolors();
