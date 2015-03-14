/*
 *	xv_simple_color.c
 *    This simple application demonstrates the use of color in XView.
 *    A colormap segment is created using named colors. The default view 
 *    of a text subwindow, and a canvas share the colormap segment. The
 *    colors in the colormap segment are displayed in the canvas.
 */
#include  <stdio.h>
#include  <xview/xview.h>
#include  <xview/textsw.h>
#include  <xview/canvas.h>
#include  <xview/cms.h>

/* color definitions */
#define WHITE                                   0
#define RED                                     1
#define GREEN                                   2
#define BLUE                                    3
#define NUM_COLORS                              4

main(argc,argv)
    int         argc;
    char        *argv[];
{
    Frame                       frame;
    Textsw                      text;
    Canvas                      canvas;
    Cms				cms;
    void                	canvas_repaint_proc();

    xv_init(XV_INIT_ARGS, argc,argv, 0);

    frame = xv_create(0, FRAME,
		WIN_HEIGHT, 500,
		WIN_WIDTH, 500,
		0);

    /* Create the colormap segment */
    cms = xv_create(0, CMS,
		CMS_SIZE, NUM_COLORS,
		CMS_NAMED_COLORS,
		    "white",
		    "red",
		    "green",
		    "blue",
		    NULL,
		0);

     /* Create the text subwindow and the canvas */
    text = xv_create(frame, TEXTSW,
		     WIN_HEIGHT, 300,
		     WIN_WIDTH, 500,
		     OPENWIN_VIEW_ATTRS,
		         WIN_CMS, cms,
			 0,
		     0);

    /*
     * Simply name the colormap segment to be used since it has
     * already been created.
     */
    canvas = xv_create(frame, CANVAS,
			WIN_HEIGHT, 200,
			WIN_WIDTH,  500,
			XV_X, 0,
			XV_Y, 300,
			WIN_CMS, cms,
			CANVAS_REPAINT_PROC, canvas_repaint_proc,
			0,
		    0);

    xv_main_loop(frame);
}

/*
 *              canvas_repaint_proc()
 *      Called to repaint the canvas window.  Display the colors in the
 *      colormap segment associated with the canvas.
 */
void
canvas_repaint_proc(canvas, paint_window, repaint_area)
    Canvas      canvas;
    Xv_window   paint_window;
    Rectlist    *repaint_area;
{
    xv_rop(paint_window, 10, 10, 50, 50, PIX_SRC|PIX_COLOR(WHITE),
    NULL, 0, 0);
    xv_rop(paint_window, 70, 10, 50, 50, PIX_SRC|PIX_COLOR(RED), NULL,
    0, 0);
    xv_rop(paint_window, 130, 10, 50, 50, PIX_SRC|PIX_COLOR(GREEN),
    NULL, 0, 0);
    xv_rop(paint_window, 190, 10, 50, 50, PIX_SRC|PIX_COLOR(BLUE),
    NULL, 0, 0);

}

