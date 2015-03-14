/*
 * 	xv_x_color.c
 *    This program demonstrates the combined use of the XView color
 *    model/API and Xlib graphics calls. The program uses XView to 
 *    create and manage its colormap segment while doing its actual 
 *    drawing using Xlib. The program draws the X logo in red, green
 *    and blue.
 */
#include <X11/Xlib.h>
#include <X11/bitmaps/xlogo64>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/cms.h>
#include <xview/xv_xrect.h>

#define WIDTH                           448
#define HEIGHT                          192

/* Color definitions */
#define WHITE                           0
#define RED                             1
#define GREEN                           2
#define BLUE                            3
#define NUM_COLORS                              4

/* graphics context used for rendering logos */
GC      gc;

/* conversion table for pixel values from XView to X11 */
unsigned long     pixel_table[NUM_COLORS];

/*
 *              main()
 *    Create a frame and a canvas.
 *    Allocate read-only colors (called a static colormap segment in
 *    XView parlance) and associate colors with the canvas.
 *    The indices into an XView colormap segment always range from 0 to
 *    size-1, where size is the number of colors allocated in the
 *    colormap segment. These logical index values translate into actual 
 *    indices into the colormap map as allocated by the X server. The
 *    CMS_INDEX_TABLE attribute returns the actual colormap indices. 
 *    The indices are returned as an array of unsigned long.
 */
main(argc,argv)
    int         argc;
    char                *argv[];
{
	Frame   	frame;
	Canvas         	canvas;
	Cms		cms;
	Xv_Singlecolor  cms_colors[NUM_COLORS];
	Display        	*display;
	XGCValues      	gc_val;
	XID            	xid;
	Pixmap         	xlogo;
	XGCValues      	gcvalues;
	int            	gcvaluemask;
	unsigned long	*xpixels;
	void           	canvas_repaint_proc();
	register int   	i;

	xv_init(XV_INIT_ARGS, argc, argv, 0);

	frame = xv_create(XV_NULL, FRAME,
			  FRAME_LABEL,                  "xv_color",
			  XV_WIDTH,                     WIDTH,
			  XV_HEIGHT,                    HEIGHT,
			  0);

	/* Initialize required colors */
	initialize_cms_colors(cms_colors);

	cms = xv_create(XV_NULL, CMS,
			CMS_SIZE, NUM_COLORS,
			CMS_COLORS, cms_colors,
			0);

	/* Get the actual indices into the colormap and store */
	xpixels = (unsigned long *)xv_get(cms, CMS_INDEX_TABLE);
	for (i = 0; i <= NUM_COLORS; i++) {
	    pixel_table[i] = xpixels[i];
	}

	canvas = xv_create(frame, CANVAS,
			   CANVAS_X_PAINT_WINDOW,   TRUE,
			   CANVAS_REPAINT_PROC,
			   canvas_repaint_proc,
			   WIN_CMS, cms,
			   0);

	/* Get display and the XID of the canvas */
	display = (Display *)xv_get(canvas, XV_DISPLAY);
	xid = (XID)xv_get(canvas, XV_XID);

	/* create the stipple xlogo */
	xlogo = XCreateBitmapFromData(display, xid, xlogo64_bits,
				      xlogo64_width, xlogo64_height);
	if( xlogo == NULL ) {
		printf( "Error allocating Pixmap for logo!\n");
		exit(1);
	}

	/* setup gc for rendering logos to screen */
	gcvalues.function = GXcopy;
	gcvalues.stipple = xlogo;
	gcvalues.fill_style = FillStippled;
	gcvalues.graphics_exposures = False;
	gcvaluemask = GCFunction | GCStipple | GCFillStyle |
	GCGraphicsExposures;

	/* create normal render gc for logo rendering */
	gc = XCreateGC(display, xid, gcvaluemask, &gcvalues);
	if(gc == NULL) {
		printf("Error allocating render graphics context!\n");
		exit(1);
	}

	/* Start event loop */
	xv_main_loop(frame);
	return(0);
}


/*
 *              canvas_repaint_proc()
 *      Draws onto the canvas using Xlib drawing functions.
 *      Use the current clipping rectangle to
 *      1. Restrict graphics output by setting the clip_mask
 *         in the graphics context.
 *      2. Do "smart repainting" by only painting the objects
 *         that lie within the damaged region (not being done in
 *         this example).
 */
void
canvas_repaint_proc(canvas, pw, display, xid, xrects)
    Canvas      canvas;
    Xv_Window   pw;
    Display     *display;
    Window      xid;
    Xv_xrectlist *xrects;
{
	XGCValues                               gc_val;
	unsigned long                   pixel_value;

	/* Set clip rects, if any */
	if (xrects) {
		XSetClipRectangles(display, gc, 0, 0,
		xrects->rect_array,
				   xrects->count, Unsorted);
	} else {
		gc_val.clip_mask = None;
		XChangeGC(display, gc, GCClipMask, &gc_val);
	}

	/* draw the logos in red, green and blue */
	pixel_value = (long)pixel_table[RED];
	XSetForeground(display, gc, pixel_value);
	XFillRectangle(display, xid, gc, 64, 64, xlogo64_width,
	xlogo64_height);

	pixel_value = (long)pixel_table[GREEN];
	XSetForeground(display, gc, pixel_value);
	XFillRectangle(display, xid, gc, 192, 64, xlogo64_width,
	xlogo64_height);

	pixel_value = (long)pixel_table[BLUE];
	XSetForeground(display, gc, pixel_value);
	XFillRectangle(display, xid, gc, 320, 64, xlogo64_width,
	xlogo64_height);
}

initialize_cms_colors(colors)
    Xv_Singlecolor      *colors;
{
    colors[WHITE].red = 255;
    colors[WHITE].green = 255;
    colors[WHITE].blue = 255;
 
    colors[RED].red = 255;
    colors[RED].green = 0;
    colors[RED].blue = 0;
 
    colors[GREEN].red = 0;
    colors[GREEN].green = 255;
    colors[GREEN].blue = 0;
 
    colors[BLUE].red = 0;
    colors[BLUE].green = 0;
    colors[BLUE].blue = 255;
}
