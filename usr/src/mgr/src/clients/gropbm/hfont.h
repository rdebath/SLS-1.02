#ifndef _HFONT_H
#define _HFONT_H
/*{{{}}}*/
/*{{{  #defines*/
#define HFONT_MAXVECTORS 5500   /* max number of vectors per font */
#define HFONT_MAXCHARS 122      /* max number of characters in each font */
/*}}}  */

/*{{{  hfont_raw*/
typedef struct
{
  int scale,maxchars;
  /* The structure of one character is its width, Number of following pairs,
   * x,y to start with, and the pairs byself.  These data are stored in ASCII
   * into the font files and binary in memory.
   */
  struct hfont_vector { int x,y; } vectors[HFONT_MAXVECTORS];
  int offset[HFONT_MAXCHARS];
} hfont_raw;
/*}}}  */
/*{{{  hfont_scaled*/
typedef struct
{
  int x_overlay, y_overlay, height;
  struct hfont_vector vectors[HFONT_MAXVECTORS];
  int offset[HFONT_MAXCHARS];
} hfont_scaled;
/*}}}  */

hfont_raw *hfont_open(char *name);
hfont_scaled *hfont_scale(hfont_raw *f, int xres_u, int yres_u, int point_p);
void hfont_print(hfont_scaled *f, int *x, int *y, int angle, unsigned char *s);
#endif
