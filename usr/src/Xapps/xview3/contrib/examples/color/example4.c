/* 
 * 	xv_colors.c
 *	This simple application demonstates the use of the XView color 
 *	API. The colormap segment can be created in a combination of the 
 *	following ways:
 *	- static or dynamic
 *	- control or non-control
 *	- using named colors or RGB values
 */	
#include  <stdio.h>
#include  <xview/xview.h>
#include  <xview/canvas.h>
#include  <xview/cms.h>
#include  <X11/Xlib.h>
#include  <xview/xv_xrect.h>

#define CHIP_WIDTH	50
#define CHIP_HEIGHT	50

/* color definitions */
#define GRAY            0
#define YELLOW          1
#define PINK            2
#define VIOLET          3
#define RED             4
#define GREEN           5
#define BLACK           6
#define BLUE            7
#define NUM_COLORS      8

unsigned long 		*index_table;
unsigned long		cms_size;

main(argc, argv)
int argc;
char **argv;
{
    Frame  		frame;
    Canvas		canvas;
    Cms			cms;
    short		dynamic_colors = FALSE;
    short		control_cms = FALSE;
    short		named_colors = FALSE;
    short		print_colors = FALSE;
    int			i;
    Xv_Singlecolor      *cms_colors;
    XColor              *cms_x_colors;
    Xv_Singlecolor      colors[NUM_COLORS];
    void		canvas_repaint_proc();


    while (--argc) {
	switch (**(++argv)) {
	    case 'c':
	    case 'C':
	        control_cms = TRUE;
		printf("Control cms set \n");
		break;
	
	    case 'd':
	    case 'D':
		dynamic_colors = TRUE;
		printf("Dynamic color set \n");
		break;

	    case 'n':
	    case 'N':
		named_colors = TRUE;
		printf("Named colors set \n");
		break;

	    case 'p':
	    case 'P':
		print_colors = TRUE;
		printf("Print colors set \n");
		break;
	    
	    default:
		break;
	}
    }

    printf("\n");

    xv_init(XV_INIT_ARGS, argc, argv, 0);

    frame = xv_create(0, FRAME, 
		XV_LABEL, "XView Colors",
		0);

    /* create the colormap segment based on flags */
    cms  = xv_create(0, CMS,
		CMS_TYPE, 
		    (dynamic_colors == TRUE) ? XV_DYNAMIC_CMS : XV_STATIC_CMS,
		CMS_CONTROL_CMS, control_cms,
		CMS_SIZE, 
		    control_cms ? NUM_COLORS + CMS_CONTROL_COLORS : NUM_COLORS,
		NULL);

    if (named_colors == TRUE) {
	xv_set(cms,
	    CMS_NAMED_COLORS,
		"gray",
		"yellow",
		"pink",
		"violet",
		"red",
		"green",
		"black",
		"blue",
		NULL,
	    NULL);
    } else {
	initialize_colors(colors);
	xv_set(cms, CMS_COLORS, colors, NULL);
    }

    /*
     * the frame height is adjusted based on the number of colors 
     * to be displayed 
     */
    cms_size = (unsigned long)xv_get(cms, CMS_SIZE);
    printf("Cms size = %d\n", (int)cms_size);
    xv_set(frame, 
	WIN_WIDTH, CHIP_WIDTH * 3,
	WIN_HEIGHT, (cms_size + 2) * CHIP_HEIGHT,
	0);

    /* conversion table for pixel values from XView to X11 */
    index_table = (unsigned long *)xv_get(cms, CMS_INDEX_TABLE);

    if (print_colors == TRUE) {
	/* 
	 * Colormap segment RGB values can be set or retreived either as 
	 * an array of Xv_Singlecolor or XColor.
	 */
        cms_colors = (Xv_Singlecolor *)malloc(cms_size * sizeof(Xv_Singlecolor));
        cms_colors = (Xv_Singlecolor *)xv_get(cms, CMS_COLORS, cms_colors);
        printf("Cms colors: \n");
        for(i = 0; i <= cms_size - 1; i++) 
	    printf("%d  %d  %d \n", (int)(cms_colors + i)->red, 
	        (int)(cms_colors + i)->green,
	        (int)(cms_colors + i)->blue);
        free(cms_colors);
        printf("\n");
    
        cms_x_colors = (XColor *)malloc(cms_size * sizeof(XColor));
        cms_x_colors = (XColor *)xv_get(cms, CMS_X_COLORS, cms_x_colors);
        printf("Cms XColors: \n");
        for(i = 0; i <= cms_size - 1; i++) 
	    printf("%d  %d  %d \n", (int)(cms_x_colors + i)->red, 
		    (int)(cms_x_colors + i)->green,
	    	    (int)(cms_x_colors + i)->blue);
        free(cms_x_colors);
        printf("\n");
    }
    
    canvas = xv_create(frame, CANVAS,
/*		WIN_DYNAMIC_VISUAL, dynamic_colors,   */
		XV_VISUAL_CLASS, PseudoColor,
		CANVAS_REPAINT_PROC, canvas_repaint_proc,
		CANVAS_X_PAINT_WINDOW, TRUE,
		WIN_CMS, cms,
		0);

    window_fit(frame);
    xv_main_loop(frame);
    return(0);
}

/*
 *              canvas_repaint_proc()
 *      Draws onto the canvas using Xlib drawing functions.
 */
void
canvas_repaint_proc(canvas, pw, display, xid, xrects)
    Canvas      canvas;
    Xv_Window   pw;
    Display     *display;
    Window      xid;
    Xv_xrectlist *xrects;
{
	static GC	gc = NULL;
        XGCValues       gcvalues;
    	int             gcvaluemask;
	register int	i;

	if (gc == NULL) {
    	    /* setup gc for rendering colors */
    	    gcvalues.function = GXcopy;
    	    gcvalues.fill_style = FillSolid;
    	    gcvaluemask = GCFunction | GCFillStyle; 

    	    gc = XCreateGC(display, xid, gcvaluemask, &gcvalues);
    	    if(gc == NULL) {
                fprintf(stderr, "XCreateGC failed ... exiting... \n");
        	    exit(1);
    	    }
	}

	for (i = 0; i <= cms_size - 1; i++) {
	    XSetForeground(display, gc, index_table[i]);
	    XFillRectangle(display, xid, gc, CHIP_WIDTH, 
		CHIP_HEIGHT * (i + 1), CHIP_WIDTH, CHIP_HEIGHT);
	}
}


initialize_colors(colors)
    Xv_Singlecolor	*colors;
{
    colors[GRAY].red = 220;   
    colors[GRAY].green = 220;   
    colors[GRAY].blue = 220;

    colors[YELLOW].red = 255; 
    colors[YELLOW].green = 255; 
    colors[YELLOW].blue = 0;

    colors[PINK].red = 188;   
    colors[PINK].green = 143;   
    colors[PINK].blue = 143;

    colors[VIOLET].red = 159; 
    colors[VIOLET].green = 95;  
    colors[VIOLET].blue = 159;

    colors[RED].red = 255;    
    colors[RED].green = 0;      
    colors[RED].blue = 0;

    colors[GREEN].red = 0;    
    colors[GREEN].green = 255;  
    colors[GREEN].blue = 0;

    colors[BLACK].red = 0;    
    colors[BLACK].green = 0;    
    colors[BLACK].blue = 0;

    colors[BLUE].red = 0;     
    colors[BLUE].green = 0;     
    colors[BLUE].blue = 255;
}
