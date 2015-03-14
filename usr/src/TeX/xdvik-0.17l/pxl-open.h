/* pxl-open.h: open a bitmap font at a given resolution.  */

#ifndef PXL_OPEN_H
#define PXL_OPEN_H

#include <stdio.h>
#include "types.h"

/* Name of the last-resort font to try, if the real font can't be opened
   at any size.  */
#ifndef	ALTFONT
#define	ALTFONT	"cmr10"
#endif
extern string alt_font;

/* Initialize all paths, etc.  Must be called before `pxl_open'.  */
extern void init_pxl_open P1H(void);

/* Defines how far away a pixel file can be found from its stated size
   before we issue a warning.  The DVI standard says any resolution
   within 0.2% of the stated size is ok, but we are more forgiving.  */
#define RES_TOLERANCE(r) ((r) / 500 + 1)

/* Open a PK/GF/PXL file named FONT, at resolution DPI.  If FONT cannot
   be found, try the name in `alt_font'.  Return the name of the font
   found in FONT_RET and the resolution found in DPI_RET, and the filename
   as the function value.  If not found, return NULL.  */
extern string pxl_open P4H(string font, string *font_ret,
                           int dpi, int *dpi_ret);

#endif /* PXL_OPEN_H */
