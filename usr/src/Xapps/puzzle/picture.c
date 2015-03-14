/* $XConsortium: picture.c,v 1.7 91/08/21 11:48:53 rws Exp $ */
/* Puzzle - (C) Copyright 1987, 1988 Don Bennett.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation.
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <errno.h>

extern Display	*dpy;
extern int	screen;
extern int	errno;
extern int	CreateNewColormap;
extern Colormap PuzzleColormap;

/*
 * This function reads a file that contains a color-mapped pixmap, in the
 * following format:
 * (a) width of the pixmap, first 4 bytes;
 * (b) height of the pixmap next 4 bytes;
 * (c) number of colormap entries, 1 byte;
 * (d) colormap values, each is one byte each of (r,g,b),
 *        3xnum_colormap_entries;
 * (e) pixmap data, one byte per pixel;
 * 
 *******************************************************
 * Thanks to Jack Palevich @apple, whose code I used
 * for generating the color-mapped version of the mandrill
 * from an original image of 8-bits per component.
 *******************************************************
 */

Pixmap PictureSetup(fname,width,height)
char *fname;
long *width, *height; /* RETURNS */
{
int readcount;

    int fd, i, cmapSize;
    unsigned char cmapSizeByte;
    unsigned char colormapData[256][3];
    XColor colorMap[256];
    Pixmap PicturePixmap;
    unsigned char *data;
    unsigned long swaptest = 1;
    
    fd = open(fname, O_RDONLY);
    if (fd == -1) {
	fprintf(stderr,"could not open picture file '%s', errno = %d\n",
		fname, errno);
	exit(1);
    }

    read(fd, (char *)width, sizeof(*width));
    read(fd, (char *)height, sizeof(*height));
    if (*(char *) &swaptest) {
	swap_long((char *) width, sizeof(*width));
	swap_long((char *) height, sizeof(*height));
    }

    read(fd, (char *)&cmapSizeByte, sizeof(cmapSizeByte));

    cmapSize = cmapSizeByte;
    if (cmapSize == 0)
	cmapSize = 256;

    read(fd, (char *)colormapData, 3*cmapSize);
    data = (unsigned char *) malloc((*width)*(*height));
    if (!data) {
	fprintf(stderr,
		"could not allocate memory for picture '%s', errno = %d\n",
		fname, errno);
	exit(1);
    }
    readcount = read(fd, (char *)data, (*width)*(*height));

    /***********************************/
    /** allocate the colormap entries **/
    /***********************************/

    for (i=0; i<cmapSize; i++) {
	colorMap[i].red   = colormapData[i][0];
	colorMap[i].green = colormapData[i][1];
	colorMap[i].blue  = colormapData[i][2];
	colorMap[i].red   |= colorMap[i].red   << 8;
	colorMap[i].green |= colorMap[i].green << 8;
	colorMap[i].blue  |= colorMap[i].blue  << 8;
    }

    getColormapEntries(colorMap,cmapSize);

    /******************************************************/
    /** convert the data to the appropriate pixel value, **/
    /** cache the data as a pixmap on the server         **/
    /******************************************************/
      
    for (i=0; i<(*width)*(*height); i++)
	data[i] = colorMap[data[i]].pixel;

    {
	GC gc;
	XImage *image;

	gc = DefaultGC(dpy, screen);
	image = XCreateImage(dpy, DefaultVisual(dpy,screen),
			     8, ZPixmap, 0, (char *)data, *width, *height,
			     8, *width);

	PicturePixmap = XCreatePixmap(dpy,RootWindow(dpy,screen),
				      image->width,image->height,8);
	XPutImage(dpy,PicturePixmap,gc,image,0,0,0,0,
		  image->width,image->height);
    }
    return(PicturePixmap);
}

getColormapEntries(colorMap, numColors)
XColor *colorMap;
int numColors;
{
    int i;

    if (CreateNewColormap)
	PuzzleColormap = XCreateColormap(dpy,RootWindow(dpy,screen),
					 DefaultVisual(dpy,screen),AllocNone);
    else
	PuzzleColormap = DefaultColormap(dpy, screen);
    
    for (i = 0; i < numColors; i++) {
	/* Need a read-only color for this value */
	if (!XAllocColor(dpy,PuzzleColormap,&colorMap[i])) {
	    fprintf(stderr, "not enough colors (asked for %d, got %d).\n",
		    numColors, i);
	    fprintf(stderr, "try the -cm option.\n");
	    return (-1);
	}
    }
    return(0);
}

swap_long (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
}
