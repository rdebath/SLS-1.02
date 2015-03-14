/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */
#include <math.h>
#include "video.h"
#include "dither.h"

/* Array that remaps color numbers to actual pixel values used by X server. */

unsigned char pixel[256];

/* Arrays holding quantized value ranged for lum, cr, and cb. */

int lum_values[LUM_RANGE];
int cr_values[CR_RANGE];
int cb_values[CB_RANGE];

/* Declaration of global variable containing dither type. */

extern int ditherType;

/* Structures used by the X server. */

Display *display;

static XImage *ximage = NULL;
static Colormap cmap;
static Window window;
static GC gc;



/*
 *--------------------------------------------------------------
 *
 * InitColor --
 *
 *	Initialized lum, cr, and cb quantized range value arrays.
 *
 * Results: 
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
InitColor()
{
  int i;

  for (i=0; i<LUM_RANGE; i++) {
    lum_values[i] = ((i * 256) / (LUM_RANGE)) + (256/(LUM_RANGE*2));
  }

  for (i=0; i<CR_RANGE; i++) {
    cr_values[i] = ((i * 256) / (CR_RANGE)) + (256/(CR_RANGE*2));
  }

  for (i=0; i<CB_RANGE; i++) {
    cb_values[i] = ((i * 256) / (CB_RANGE)) + (256/(CB_RANGE*2));
  }

}


/*
 *--------------------------------------------------------------
 *
 * ConvertColor --
 *
 *	Given a l, cr, cb tuple, converts it to r,g,b.
 *
 * Results:
 *	r,g,b values returned in pointers passed as parameters.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void
ConvertColor(l, cr, cb, r, g, b)
     unsigned char l, cr, cb;
     unsigned char *r, *g, *b;
{
  double fl, fcr, fcb, fr, fg, fb;

  fl = (double) l;
  fcr =  ((double) cr) - 128.0;
  fcb =  ((double) cb) - 128.0;


  fr = fl + (1.40200 * fcb);
  fg = fl - (0.71414 * fcb) - (0.34414 * fcr);
  fb = fl + (1.77200 * fcr);

  if (fr < 0.0) fr = 0.0;
  else if (fr > 255.0) fr = 255.0;

  if (fg < 0.0) fg = 0.0;
  else if (fg > 255.0) fg = 255.0;

  if (fb < 0.0) fb = 0.0;
  else if (fb > 255.0) fb = 255.0;

  *r = (unsigned char) fr;
  *g = (unsigned char) fg;
  *b = (unsigned char) fb;

}

/* Array back mapping pixel value to color number. Used for debugging and dumping
   purposes. 
*/

static char backpixel[256];

#ifdef SH_MEM

int gXErrorFlag = 0;

int HandleXError(dpy, event)
     Display *dpy;
     XErrorEvent *event;
{
  gXErrorFlag = 1;

  return 0;
}

void InstallXErrorHandler()
{
  int HandleXError();

  XSetErrorHandler(HandleXError);
  XFlush(display);
}

void DeInstallXErrorHandler()
{
  XSetErrorHandler(NULL);
  XFlush(display);
}
#endif


/*
 *--------------------------------------------------------------
 *
 * ResizeDisplay --
 *
 *	Resizes display window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void ResizeDisplay(w, h)
     int w, h;
{

  if (ditherType == NO_DITHER) return;

  XResizeWindow(display, window, w, h);
  XFlush(display);
}


/*
 *--------------------------------------------------------------
 *
 * MakeWindow --
 *
 *	Create X Window
 *
 * Results:
 *	Read the code.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void 
MakeWindow() 
{
  
  XSizeHints hint;
  unsigned int fg, bg;
  char *hello = "MPEG Play";
  int screen;
  Window CreateFullColorWindow();

  if (ditherType == NO_DITHER) return;

  display = XOpenDisplay("");
  if (display == NULL) {
    fprintf(stderr, "Can not open display\n");
    exit(-2);
  }

  screen = DefaultScreen (display);
  
  /* Fill in hint structure */

  hint.x = 200;
  hint.y = 300;
  hint.width = 150;
  hint.height = 150;
  hint.flags = PPosition | PSize;
  
  /* Get some colors */
  
  bg = WhitePixel (display, screen);
  fg = BlackPixel (display, screen);
  
  /* Make the window */
  
  if (ditherType == FULL_COLOR_DITHER) {
    window = CreateFullColorWindow (display, hint.x, hint.y, hint.width, hint.height);
    if (window == 0) {
      fprintf (stderr, "-color option only valid on full color display\n");
      exit (-1);
    }
  } else if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD) {
    window = XCreateSimpleWindow (display,
				  DefaultRootWindow (display),
				  hint.x, hint.y,
				  hint.width, hint.height,
				  4, fg, bg);
  } else {
    XVisualInfo vinfo;
    
    if (!XMatchVisualInfo (display, screen, 8, PseudoColor, 
			   &vinfo)) {

      if (!XMatchVisualInfo(display, screen, 8, GrayScale, 
			    &vinfo)) {

	fprintf(stderr, "-requires 8 bit display\n");
	exit(-1);
      }
    }

    window = XCreateSimpleWindow (display,
				 DefaultRootWindow (display),
				 hint.x, hint.y,
				 hint.width, hint.height,
				 4, fg, bg);
  }
  
  /* Tell other applications about this window */
  
  XSetStandardProperties (display, window, hello, hello, None, NULL, 0, &hint);
  
  /* Map window. */

  XMapWindow(display, window);
}
  

/*
 *--------------------------------------------------------------
 *
 * InitDisplay --
 *
 *	Initialized display, sets up colormap, etc.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitDisplay()
{

  int ncolors = LUM_RANGE*CB_RANGE*CR_RANGE;
  XColor xcolor;
  int i, lum_num, cr_num, cb_num;
  unsigned char r, g, b;

  if (ditherType == NO_DITHER) return;

  MakeWindow();

  gc = XCreateGC(display, window, 0, 0);

  cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  for (i=0; i<ncolors; i++) {

    lum_num = (i / (CR_RANGE*CB_RANGE))%LUM_RANGE;
    cr_num = (i / CB_RANGE)%CR_RANGE;
    cb_num = i % CB_RANGE;

    ConvertColor(lum_values[lum_num], cr_values[cr_num], cb_values[cb_num], &r, &g, &b);

    xcolor.red = r * 256;
    xcolor.green = g * 256;
    xcolor.blue = b * 256;

    XAllocColor(display, cmap, &xcolor);
    pixel[i] = xcolor.pixel;
    backpixel[xcolor.pixel] = i;
  }

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitGrayDisplay --
 *
 *	Initialized display for gray scale dither.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

#define NUM_COLORS 128

void InitGrayDisplay()
{
  int ncolors = NUM_COLORS;
  XColor xcolor;
  int i;

  MakeWindow();

  gc = XCreateGC(display, window, 0, 0);

  cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  for (i=0; i<ncolors; i++) {

    xcolor.red = (i*2) * 256;
    xcolor.green = (i*2) * 256;
    xcolor.blue = (i*2) * 256;

    XAllocColor(display, cmap, &xcolor);
    pixel[(i*2)] = xcolor.pixel;
    pixel[(i*2)+1] = xcolor.pixel;
    backpixel[xcolor.pixel] = i;
  }

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitMonoDisplay --
 *
 *	Initialized display for monochrome dither.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitMonoDisplay()
{
  XGCValues xgcv;

  MakeWindow();

  xgcv.background = BlackPixel(display, DefaultScreen(display));
  xgcv.foreground = WhitePixel(display, DefaultScreen(display));

  gc = XCreateGC(display, window, GCForeground | GCBackground, &xgcv);

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitColorDisplay --
 *
 *	Initialized display for full color output.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitColorDisplay()
{

  MakeWindow();

  gc = XCreateGC(display, window, 0, 0);
  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * ExecuteDisplay --
 *
 *	Actually displays display plane in previously created window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
ExecuteDisplay(vid_stream)
     VidStream *vid_stream;
{
  char dummy;
  Visual *FindFullColorVisual();
  Visual *fc_visual;
  int depth;

  totNumFrames++;
  fprintf (stderr, "%d\r", totNumFrames);

  if (ditherType == NO_DITHER) return;

  if (ximage == NULL) {

    if (ditherType == Twox2_DITHER) {
      ximage = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			    vid_stream->mb_width * 32,
			    vid_stream->mb_height * 32, 8, 0);
    } else if (ditherType == FULL_COLOR_DITHER) {
      fc_visual = FindFullColorVisual(display, &depth);
      ximage = XCreateImage (display, fc_visual, depth, ZPixmap,
			     0, &dummy, vid_stream->mb_width * 16,
			     vid_stream->mb_height * 16, 32, 0);
    } else if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD) {
      ximage = XCreateImage (display, None, 1, XYBitmap, 0, &dummy,
			     vid_stream->mb_width * 16,
			     vid_stream->mb_height * 16, 8, 0);
    } else {
      ximage = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			    vid_stream->mb_width * 16,
			    vid_stream->mb_height * 16, 8, 0);
    }
  }

#ifdef SH_MEM
  if (shmemFlag) {
    XShmPutImage(display, window, gc, vid_stream->current->ximage, 
		 0, 0, 0, 0,
		 vid_stream->current->ximage->width, 
		 vid_stream->current->ximage->height, False);
    XFlush(display);

  }
  else 
#endif

    {
      ximage->data = (char *) vid_stream->current->display; 

      XPutImage(display, window, gc, ximage, 0, 0, 0, 0, ximage->width, ximage->height);
    }
}


