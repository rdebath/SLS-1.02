/*
 * Name:	pw_draw.c
 *
 * Description:
 *	Demonstrates some of the pixwin drawing functions on
 *	a canvas window. The drawing is done inside a repaint proc.
 *	XView (and X11) applications must always be prepared to
 *	repaint themselves on demand.	
 *
 * Note: XView doesn't really support pw_* graphics calls.
 * These are leftover functions from Sunview and are shown here for
 * compatibility reference only.
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/svrimage.h>

static short chess_bits[] = {
#include <images/chesstool.icon>
};

static short gray_bits[] = {
#include "gray_stencil.pr"
};

Server_image	chess_image;
Server_image	stencil_image;

/*
 *              main()
 *      Create frame and canvas windows.
 *      Set a repaint proc for the canvas window.
 *	Create Server Images for fill and stencil operations 
 *	in canvas_repaint_proc().
 *      Start the event loop.
 */
main(argc,argv)
    int	    argc;
    char    *argv[];
{
	Frame	frame;
	Canvas  canvas;
	void	canvas_repaint_proc();

	xv_init(XV_INIT_ARGS, argc, argv, 0);

	/* Create a frame and a canvas */
	frame = xv_create(NULL, FRAME,
			  FRAME_LABEL, 	"xv_canvas_pw_draw",
			  XV_WIDTH, 	734,
			  XV_HEIGHT, 	448,
			  0);

	canvas = xv_create(frame, CANVAS,
			   CANVAS_REPAINT_PROC,	canvas_repaint_proc,
			   0);

	/* By default, server images are created with depth 1 */
	chess_image = xv_create(NULL,SERVER_IMAGE,
				XV_WIDTH,		64,
				XV_HEIGHT,		64,
				SERVER_IMAGE_BITS,	chess_bits,
				0);

	stencil_image = xv_create(NULL,SERVER_IMAGE,
			      	  XV_WIDTH,		64,
			      	  XV_HEIGHT,		64,
			      	  SERVER_IMAGE_BITS,	gray_bits,
			      	  0);

	xv_main_loop(frame);
	return(0);
}

/*
 *              canvas_repaint_proc()
 *      Called to repaint the canvas window.  Draws into the window
 *      using various pixwin drawing functions.
 */
void
canvas_repaint_proc(canvas, canvas_pw, repaint_area)
    Canvas      canvas;
    Pixwin   	*canvas_pw;
    Rectlist    *repaint_area;
{
	static	Pr_brush 	brush = {5};
	static	short 		dashed[] = {5, 3, 0};
	static	Pr_texture 	tex = {dashed, 0, 0, 0, 0, 0, 0, 0 };
	static	struct pr_pos 	lines[] = {{0,0}, {64,0}, {0,64}, {64,64}};
	static  struct pr_pos  	points[] = {{0, 128}, {128, 128}, {64, 0}};
	static	int		num_points[] = {3};

	/* Rop the chess_image icon onto the canvas */
	pw_text(canvas_pw, 64, 58, PIX_SRC, NULL, "pw_rop");
	pw_rop(canvas_pw, 64, 64, 64, 64, PIX_SET, chess_image, 0, 0);

	/* Draw the chess_image icon using the stencil_image as a mask */
	pw_text(canvas_pw, 192, 58, PIX_SRC, NULL, "pw_stencil");
	pw_stencil(canvas_pw, 192, 64, 64, 64, PIX_SRC, stencil_image, 0, 0,
		   chess_image, 0, 0);

	/* Replicate the chess_image icon within the specified rectangle */
	pw_text(canvas_pw, 352, 58, PIX_SRC, NULL,"pw_replrop");
	pw_replrop(canvas_pw, 320, 64, 128, 128, PIX_SRC, chess_image, 0, 0);

	/* Fill the specified polygon with the chess_image icon */
	pw_text(canvas_pw, 542, 58, PIX_SRC, NULL,"pw_polygon_2");
	pw_polygon_2(canvas_pw, 512, 64, 1, num_points, points, PIX_SRC, 
		     chess_image, 0, 0);

	pw_text(canvas_pw, 64, 312, PIX_SRC, NULL, "pw_vector");
	pw_vector(canvas_pw, 64, 320, 128, 384, PIX_SRC, 1);
	pw_vector(canvas_pw, 64, 384, 128, 320, PIX_SRC, 1);
	
	/* Draw a dashed line with the specifed line pattern & thickness */
	pw_text(canvas_pw, 192, 312, PIX_SRC, NULL, "pw_line");
	pw_line(canvas_pw, 192, 320, 256, 384, &brush, &tex, PIX_SET);
	pw_line(canvas_pw, 192, 384, 256, 320, &brush, &tex, PIX_SET);
	
	/* Draw a set of lines */
	pw_text(canvas_pw, 320, 312, PIX_SRC, NULL, "pw_polyline");
	pw_polyline(canvas_pw, 320, 320, 4, lines, POLY_CLOSE, NULL, NULL, PIX_SET);

	/* Copy from one region of the canvas to another */
	pw_text(canvas_pw, 512, 312, PIX_SRC, NULL, "pw_copy"); 
	pw_copy(canvas_pw, 448, 320, 192, 64, PIX_SRC, canvas_pw, 400, 96);
}

