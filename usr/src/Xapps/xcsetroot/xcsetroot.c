/* Copyright 1987, Massachusetts Institute of Technology */

#include <X11/Xatom.h>
#include "X11/bitmaps/gray"
#include "common.h"

/*
 * xcsetroot.c 	MIT Project Athena, X Window system root window 
 *		parameter setting utility.  This program will set 
 *		various parameters of the X root window.
 *
 *  Authors:	Thomas Wu, Davor Matic (MIT Project Athena)
 *		Feb-90
 */

/*
 * Global Variable Declarations
 */
Bool optimize = False, reverse = False, portable = False;
Bool save_colors = False, unsave_past = False;
String fore_color = NULL, back_color = NULL;
String program_name;
Display *dpy;
Window root;
int screen;

usage()
{
    fprintf(stderr, "usage: %s [options]\n", program_name);
    fprintf(stderr, "  where options are:\n");
    fprintf(stderr, "  -display <display>   or   -d <display>\n");
    fprintf(stderr, "  -fg <color>   or   -foreground <color>\n");
    fprintf(stderr, "  -bg <color>   or   -background <color>\n");
    fprintf(stderr, "  -rv   or   -reverse\n");
    fprintf(stderr, "  -a    or   -autoscale\n");
    fprintf(stderr, "  -O    or   -optimize\n");
    fprintf(stderr, "  -help\n");
    fprintf(stderr, "  -def   or   -default\n");
    fprintf(stderr, "  -portable\n");
    fprintf(stderr, "  -limit <n>\n");
    fprintf(stderr, "  -name <string>\n");
    fprintf(stderr, "  -cursor <cursor file> <mask file>\n");
#ifdef CURSOR_NAMES
    fprintf(stderr, "  -cursor_name <cursor-font name>\n");
#endif
    fprintf(stderr, "  -solid <color>\n");
    fprintf(stderr, "  -gray   or   -grey\n");
    fprintf(stderr, "  -bitmap <filename>\n");
    fprintf(stderr, "  -xwd <filename>\n");
    fprintf(stderr, "  -gif <filename>\n");
    fprintf(stderr, "  -size <x> <y>\n");
    fprintf(stderr, "  -mod <x> <y>\n");
    exit(1);
    /*NOTREACHED*/
}

#ifdef CURSOR_NAMES
Cursor	CreateCursorFromName();
#endif
Pixel NameToPixel();
Pixmap MakeModulaBitmap(), ReadBitmapFile();
XColor NameToXColor();

main(argc, argv) 
    int argc;
    char **argv;
{
    Bool gray = False;
    Bool restore_defaults = False;
    Cursor cursor;
    Pixmap bitmap;
    String ReadXwdFile();
    String display_name = NULL, name = NULL;
    String cursor_file = NULL, cursor_mask = NULL;
#ifdef CURSOR_NAMES
    String cursor_name = NULL;
#endif
    String solid_color = NULL;
    String bitmap_file = NULL, xwd_file = NULL, gif_file = NULL;
    XColor *file_colors;
    XImage *file_image, *real_image, *ProcessImage();
    int mod_x = 0, mod_y = 0;
    int new_x = 0, new_y = 0;
    int excl = 0, nonexcl = 0;
    int i, ncolors, max_colors = 0;
    unsigned int ww, hh;

    fprintf(stderr, "XCsetroot, written by Thomas Wu and Davor Matic (c) 1990, MIT Fishbowl\n");
    program_name=argv[0];
    for (i = 1; i < argc; i++) {
	if (!strcmp ("-display", argv[i]) || !strcmp ("-d", argv[i])) {
	    if (++i>=argc) usage ();
	    display_name = argv[i];
	    continue;
	}
	if (!strcmp("-help", argv[i])) {
	    usage();
	}
	if (!strcmp("-def", argv[i]) || !strcmp("-default", argv[i])) {
	    restore_defaults = True;
	    continue;
	}
	if (!strcmp("-name", argv[i])) {
	    if (++i>=argc) usage();
	    name = argv[i];
	    nonexcl++;
	    continue;
	}
	if (!strcmp("-cursor", argv[i])) {
	    if (++i>=argc) usage();
	    cursor_file = argv[i];
	    if (++i>=argc) usage();
	    cursor_mask = argv[i];
	    nonexcl++;
	    continue;
	}
#ifdef CURSOR_NAMES
	if (!strcmp("-cursor_name", argv[i])) {
	    if (++i>=argc) usage();
	    cursor_name = argv[i];
	    nonexcl++;
	    continue;
	}
#endif
	if (!strcmp("-fg",argv[i]) || !strcmp("-foreground",argv[i])) {
	    if (++i>=argc) usage();
	    fore_color = argv[i];
	    continue;
	}
	if (!strcmp("-bg",argv[i]) || !strcmp("-background",argv[i])) {
	    if (++i>=argc) usage();
	    back_color = argv[i];
	    continue;
	}
	if (!strcmp("-solid", argv[i])) {
	    if (++i>=argc) usage();
	    solid_color = argv[i];
	    excl++;
	    continue;
	}
	if (!strcmp("-gray", argv[i]) || !strcmp("-grey", argv[i])) {
	    gray = True;
	    excl++;
	    continue;
	}
	if (!strcmp("-limit", argv[i])) {
	  if(++i >= argc) usage();
	  max_colors = atoi(argv[i]);
	  continue;
	}
	if (!strcmp("-O", argv[i]) || !strcmp("-optimize", argv[i])) {
	  optimize = True;
	  continue;
	}
	if (!strcmp("-bitmap", argv[i])) {
	    if (++i>=argc) usage();
	    bitmap_file = argv[i];
	    excl++;
	    continue;
	}
	if (!strcmp("-xwd", argv[i])) {
	    if (++i>=argc) usage();
	    xwd_file = argv[i];
	    excl++;
	    continue;
	}
	if (!strcmp("-gif", argv[i])) {
	    if (++i>=argc) usage();
	    gif_file = argv[i];
	    excl++;
	    continue;
	}
	if (!strcmp("-mod", argv[i])) {
	    if (++i>=argc) usage();
	    mod_x = atoi(argv[i]);
	    if (mod_x <= 0) mod_x = 1;
	    if (++i>=argc) usage();
	    mod_y = atoi(argv[i]);
	    if (mod_y <= 0) mod_y = 1;
	    excl++;
	    continue;
	}
	if (!strcmp("-size", argv[i])) {
	  if(++i >= argc) usage();
	  new_x = atoi(argv[i]);
	  if(++i >= argc) usage();
	  new_y = atoi(argv[i]);
	  if(new_x <= 0 || new_y <= 0) usage();
	  continue;
	}
	if (!strcmp("-portable", argv[i])) {
	  portable = True;
	  continue;
	}
	if (!strcmp("-a", argv[i]) || !strcmp("-autoscale", argv[i])) {
	  new_x = new_y = (-1);
	  continue;
	}
	if (!strcmp("-rv",argv[i]) || !strcmp("-reverse",argv[i])) {
	    reverse = True;
	    continue;
	}
	usage();
    } 

    /* Check for multiple use of exclusive options */
    if (excl > 1) {
	fprintf(stderr, "%s: choose only one of {solid, gray, bitmap, mod}\n",
		program_name);
	usage();
    }

    dpy = XOpenDisplay(display_name);
    if (!dpy) {
	fprintf(stderr, "%s:  unable to open display '%s'\n",
		program_name, XDisplayName (display_name));
	usage ();
    }
    screen = DefaultScreen(dpy);
    root = RootWindow(dpy, screen);

    if(new_x < 0 && new_y < 0) {
      new_x = DisplayWidth(dpy, screen);
      new_y = DisplayHeight(dpy, screen);
    }

    /* If there are no arguments then restore defaults. */
    if (!excl && !nonexcl)
	restore_defaults = True;
  
    /* Handle a cursor file */
    if (cursor_file) {
	cursor = CreateCursorFromFiles(cursor_file, cursor_mask);
	XDefineCursor(dpy, root, cursor);
	XFreeCursor(dpy, cursor);
    }
  
#ifdef CURSOR_NAMES
    if (cursor_name) {
	cursor = CreateCursorFromName (cursor_name);
	if (cursor)
	{
	    XDefineCursor (dpy, root, cursor);
	    XFreeCursor (dpy, cursor);
	}
    }
#endif
    /* Handle -gray and -grey options */
    if (gray) {
	bitmap = XCreateBitmapFromData(dpy, root, gray_bits,
				       gray_width, gray_height);
	SetBackgroundToBitmap(bitmap, gray_width, gray_height);
    }
  
    /* Handle -solid option */
    if (solid_color) {
	XSetWindowBackground(dpy, root, NameToPixel(solid_color,
						    BlackPixel(dpy, screen)));
	XClearWindow(dpy, root);
	unsave_past = True;
    }
  
    /* Handle -bitmap option */
    if (bitmap_file) {
	bitmap = ReadBitmapFile(bitmap_file, &ww, &hh, 
				(int *)NULL, (int *)NULL);
	SetBackgroundToBitmap(bitmap, ww, hh);
    }
  
    /* Handle -xwd option */
    if (xwd_file) {
	free(ReadXwdFile(xwd_file, &file_image, &file_colors, &ncolors));
	real_image = ProcessImage(file_image, file_colors, ncolors, max_colors,
				  new_x, new_y);
	SetBackgroundToImage(real_image);
	XDestroyImage(real_image);
	free((char *)file_colors);
    }

    /* Handle -gif option */
    if (gif_file) {
	ReadGifFile(gif_file, &file_image, &file_colors, &ncolors);
	real_image = ProcessImage(file_image, file_colors, ncolors, max_colors,
				  new_x, new_y);
	SetBackgroundToImage(real_image);
	XDestroyImage(real_image);
	free((char *)file_colors);
    }

    /* Handle set background to a modula pattern */
    if (mod_x) {
	bitmap = MakeModulaBitmap(mod_x, mod_y);
	SetBackgroundToBitmap(bitmap, 16, 16);
    }

    /* Handle set name */
    if (name)
	XStoreName(dpy, root, name);
  
    /* Handle restore defaults */
    if (restore_defaults) {
	if (!cursor_file)
	    XUndefineCursor(dpy, root);
	if (!excl) {
	    XSetWindowBackgroundPixmap(dpy, root, (Pixmap) None);
	    XClearWindow(dpy, root);
	    unsave_past = True;
	}
    }

    FixupState();
    XCloseDisplay(dpy);
    exit (0);
}

/* Free past incarnation if needed, and retain state if needed. */
FixupState()
{
    Atom prop, type;
    Pixmap save_pixmap;
    int format;
    unsigned long length, after;
    unsigned char *data;

    if (!(DefaultVisual(dpy, screen)->class & Dynamic))
      unsave_past = False;
    if (!unsave_past && !save_colors)
      return;
    prop = XInternAtom(dpy, "_XSETROOT_ID", False);
    if (unsave_past) {
      (void)XGetWindowProperty(dpy, root, prop, 0L, 1L, True, AnyPropertyType,
                               &type, &format, &length, &after, &data);
      if ((type == XA_PIXMAP) && (format == 32) &&
          (length == 1) && (after == 0))
          XKillClient(dpy, *((Pixmap *)data));
      else if (type != None)
          fprintf(stderr, "%s: warning: _XSETROOT_ID property is garbage\n",
                  program_name);
    }
    if (save_colors) {
      save_pixmap = XCreatePixmap(dpy, root, 1, 1, 1);
      XChangeProperty(dpy, root, prop, XA_PIXMAP, 32, PropModeReplace,
                      (unsigned char *) &save_pixmap, 1);
      XSetCloseDownMode(dpy, RetainPermanent);
    }
}

/*
 * SetBackgroundToBitmap: Set the root window background to a caller supplied 
 *                        bitmap.
 */
SetBackgroundToBitmap(bitmap, width, height)
    Pixmap bitmap;
    unsigned int width, height;
{
    Pixmap pix;
    GC gc;
    XGCValues gc_init;

    gc_init.foreground = NameToPixel(fore_color, BlackPixel(dpy, screen));
    gc_init.background = NameToPixel(back_color, WhitePixel(dpy, screen));
    if (reverse) {
	Pixel temp = gc_init.foreground;
	gc_init.foreground=gc_init.background;
	gc_init.background=temp;
    }
    gc = XCreateGC(dpy, root, GCForeground|GCBackground, &gc_init);
    pix = XCreatePixmap(dpy, root, width, height,
			(unsigned int)DefaultDepth(dpy, screen));
    XCopyPlane(dpy, bitmap, pix, gc, 0, 0, width, height, 0, 0, (unsigned long)1);
    XSetWindowBackgroundPixmap(dpy, root, pix);
    XFreePixmap(dpy, bitmap);
    XClearWindow(dpy, root);
    XFreePixmap(dpy, pix);
    XFreeGC(dpy, gc);
    unsave_past = True;
}

/*
 * SetBackgroundToImage: Set the root window background to a caller supplied 
 *                        image.
 */
SetBackgroundToImage(image)
    XImage *image;
{
    Pixmap pix;
    GC gc;
    XGCValues gc_init;

    gc_init.foreground = NameToPixel(fore_color, BlackPixel(dpy, screen));
    gc_init.background = NameToPixel(back_color, WhitePixel(dpy, screen));
    if (reverse) {
	Pixel temp = gc_init.foreground;
	gc_init.foreground=gc_init.background;
	gc_init.background=temp;
    }
    pix = XCreatePixmap(dpy, root, image->width, image->height,
			DefaultDepth(dpy, screen));
    gc = XCreateGC(dpy, pix, GCForeground|GCBackground, &gc_init);
    XPutImage(dpy, pix, gc, image, 0, 0, 0, 0, image->width, image->height);
    XSetWindowBackgroundPixmap(dpy, root, pix);
    XClearWindow(dpy, root);
    XFreePixmap(dpy, pix);
    XFreeGC(dpy, gc);
}

/*
 * CreateCursorFromFiles: make a cursor of the right colors from two bitmap
 *                        files.
 */
#define BITMAP_HOT_DEFAULT 8

CreateCursorFromFiles(cursor_file, mask_file)
    String cursor_file, mask_file;
{
    Cursor cursor;
    Pixmap cursor_bitmap, mask_bitmap;
    XColor fg, bg, temp;
    int x_hot, y_hot;
    unsigned int width, height, ww, hh;

    fg = NameToXColor(fore_color, BlackPixel(dpy, screen));
    bg = NameToXColor(back_color, WhitePixel(dpy, screen));
    if (reverse) {
	temp = fg; fg = bg; bg = temp;
    }

    cursor_bitmap = ReadBitmapFile(cursor_file, &width, &height, &x_hot, &y_hot);
    mask_bitmap = ReadBitmapFile(mask_file, &ww, &hh, (int *)NULL, (int *)NULL);

    if (width != ww || height != hh) {
	fprintf(stderr, 
"%s: dimensions of cursor bitmap and cursor mask bitmap are different\n", 
		program_name);
	exit(1);
	/*NOTREACHED*/
    }

    if ((x_hot == -1) && (y_hot == -1)) {
	x_hot = BITMAP_HOT_DEFAULT;
	y_hot = BITMAP_HOT_DEFAULT;
    }
    if ((x_hot < 0) || (x_hot >= width) ||
	(y_hot < 0) || (y_hot >= height)) {
	fprintf(stderr, "%s: hotspot is outside cursor bounds\n", program_name);
	exit(1);
	/*NOTREACHED*/
    }

    cursor = XCreatePixmapCursor(dpy, cursor_bitmap, mask_bitmap, &fg, &bg,
				 (unsigned int)x_hot, (unsigned int)y_hot);
    XFreePixmap(dpy, cursor_bitmap);
    XFreePixmap(dpy, mask_bitmap);

    return(cursor);
}

#ifdef CURSOR_NAMES

Cursor
CreateCursorFromName (name)
    String name;
{
    Font    fid;
    XColor fg, bg, temp;
    int	    i;

    fg = NameToXColor(fore_color, BlackPixel(dpy, screen));
    bg = NameToXColor(back_color, WhitePixel(dpy, screen));
    if (reverse) {
	temp = fg; fg = bg; bg = temp;
    }
    i = XmuCursorNameToIndex (name);
    if (i == -1)
	return (Cursor) 0;
    fid = XLoadFont (dpy, "cursor");
    if (!fid)
	return (Cursor) 0;
    return XCreateGlyphCursor (dpy, fid, fid,
			       i, i+1, &fg, &bg);
}

#endif

/*
 * MakeModulaBitmap: Returns a modula bitmap based on an x & y mod.
 */
Pixmap MakeModulaBitmap(mod_x, mod_y)
    int mod_x, mod_y;
{
    char modula_data[16*16/8];
    int i;
    long pattern_line = 0;
    
    for (i=0; i<16; i++) {
	pattern_line <<=1;
	if ((i % mod_x) == 0) pattern_line |= 0x0001;
    }
    for (i=0; i<16; i++) {
	if ((i % mod_y) == 0) {
	    modula_data[i*2] = 0xff;
	    modula_data[i*2+1] = 0xff;
	} else {
	    modula_data[i*2] = pattern_line & 0xff;
	    modula_data[i*2+1] = (pattern_line>>8) & 0xff;
	}
    }

    return(XCreateBitmapFromData(dpy, root, modula_data, 16, 16));
}


/*
 * NameToXColor: Convert the name of a color to its Xcolor value.
 */
XColor NameToXColor(name, pixel)
    String name;
    Pixel pixel;
{
    XColor c;
    
    if (!name || !*name) {
	c.pixel = pixel;
	XQueryColor(dpy, DefaultColormap(dpy, screen), &c);
    } else if (!XParseColor(dpy, DefaultColormap(dpy, screen), name, &c)) {
	fprintf(stderr, "%s: unknown color or bad color format: %s\n",
			program_name, name);
	exit(1);
	/*NOTREACHED*/
    }
    return(c);
}

Pixel NameToPixel(name, pixel)
    String name;
    Pixel pixel;
{
    XColor ecolor;

    if (!name || !*name)
	return pixel;
    if (!XParseColor(dpy,DefaultColormap(dpy,screen),name,&ecolor)) {
	fprintf(stderr,"%s:  unknown color \"%s\"\n",program_name,name);
	exit(1);
	/*NOTREACHED*/
    }
    if (!XAllocColor(dpy, DefaultColormap(dpy, screen),&ecolor)) {
	fprintf(stderr, "%s:  unable to allocate color for \"%s\"\n",
		program_name, name);
	exit(1);
	/*NOTREACHED*/
    }
    if ((ecolor.pixel != BlackPixel(dpy, screen)) &&
	(ecolor.pixel != WhitePixel(dpy, screen)) &&
	(DefaultVisual(dpy, screen)->class & Dynamic))
	save_colors = 1;
    return(ecolor.pixel);
}

Pixmap ReadBitmapFile(filename, width, height, x_hot, y_hot)
    String filename;
    unsigned int *width, *height;
    int *x_hot, *y_hot;
{
    Pixmap bitmap;
    int status;

    status = XReadBitmapFile(dpy, root, filename, width,
			     height, &bitmap, x_hot, y_hot);
    if (status == BitmapSuccess)
      return(bitmap);
    else if (status == BitmapOpenFailed)
	fprintf(stderr, "%s: can't open file: %s\n", program_name, filename);
    else if (status == BitmapFileInvalid)
	fprintf(stderr, "%s: bad bitmap format file: %s\n",
			program_name, filename);
    else
	fprintf(stderr, "%s: insufficient memory for bitmap: %s",
			program_name, filename);
    exit(1);
    /*NOTREACHED*/
}
