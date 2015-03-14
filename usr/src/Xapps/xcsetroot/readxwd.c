#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/XWDFile.h>

#define MAXCMD 128

/*
 * Read in the header, window name, colormap, and pixmap from the input file.
 */
String ReadXwdFile(file_name, image, colors, ncolors)
    String file_name;
    XImage **image;
    XColor **colors;
    int *ncolors;
{
    Bool pipeline = False;
    FILE *file;
    XWDFileHeader header;
    String win_name;
    char shell_cmd[MAXCMD];
    int i;
    register char *buffer;
    unsigned buffer_size, win_name_size;
    unsigned long swaptest = 1;

    if (strlen(file_name) > 1 &&
	!strcmp(file_name + strlen(file_name) - 2, ".Z")) {
      (void) sprintf(shell_cmd, "zcat %s", file_name);
      if((file = popen(shell_cmd, "r")) == NULL) {
	  XtWarning("Cannot uncompress the file");
	  return;
      }
      pipeline = True;
    }
    else if (strcmp(file_name, "-")) {
	/*
	 * Open the output file.
	 */
	file = fopen(file_name, "r");
	if (!file) {
	    XtWarning("Cannot open the file");
	    return;
	}
    }
    else {
      file_name = "<standard input>";
      file = stdin;
    }
    
    /*
     * Read in header information.
     */
    if(fread((char *)&header, sizeof(header), 1, file) != 1)
	XtWarning("Unable to read dump file header.");

    if (*(char *) &swaptest)
	_swaplong((char *) &header, sizeof(header));

    /*
     * Check if the dump file is in the proper format.
     */
    if (header.file_version != XWD_FILE_VERSION)
	XtWarning("XWD file format version mismatch.");

    if (header.header_size < sizeof(header))
	XtWarning("XWD header size is too small.");

    /*
     * Malloc window name.
     */
    win_name_size = (header.header_size - sizeof(header));
    win_name = (String) XtMalloc((unsigned) win_name_size * sizeof(char));

    /*
     * Read in window name.
     */
    if(fread(win_name, sizeof(char), (int)win_name_size, file)!= win_name_size)
	XtWarning("Unable to read window name from dump file.");

    /*
     * Malloc the colormap buffer.
     */
    if(*ncolors = header.ncolors) {
	*colors = (XColor *) XtMalloc((unsigned) *ncolors * sizeof(XColor));
	if(fread((char *) *colors, sizeof(XColor), *ncolors, file) != *ncolors)
	    XtWarning("Unable to read color map from dump file.");
    }

    if (*(char *) &swaptest) {
	for (i = 0; i < *ncolors; i++) {
	    _swaplong((char *) &(*colors)[i].pixel, sizeof(long));
	    _swapshort((char *) &(*colors)[i].red, 3 * sizeof(short));
	}
    }
    
    /*
     * Malloc the pixel buffer.
     */
    buffer_size = DataSize((int)header.bytes_per_line,
			   (int)header.pixmap_height,
			   (int)header.pixmap_depth, 
			   (int)header.pixmap_format);
    buffer = XtMalloc(buffer_size);

    /*
     * Malloc the image.
     */
    *image = (XImage *) XtMalloc(sizeof(XImage));
    
    /*
     * Initialize image.
     */
    (*image)->data = (char *) buffer;
    (*image)->width = (int) header.pixmap_width;
    (*image)->height = (int) header.pixmap_height;
    (*image)->xoffset = (int) header.xoffset;
    (*image)->format = (int) header.pixmap_format;
    (*image)->byte_order = (int) header.byte_order;
    (*image)->bitmap_unit = (int) header.bitmap_unit;
    (*image)->bitmap_bit_order = (int) header.bitmap_bit_order;
    (*image)->bitmap_pad = (int) header.bitmap_pad;
    (*image)->depth = (int) header.pixmap_depth;
    (*image)->bits_per_pixel = (int) header.bits_per_pixel;
    (*image)->bytes_per_line = (int) header.bytes_per_line;
    (*image)->red_mask = header.red_mask;
    (*image)->green_mask = header.green_mask;
    (*image)->blue_mask = header.blue_mask;
    (*image)->obdata = NULL;
    _XInitImageFuncPtrs(*image);

    /****************************&&&&&&&&&&%%%%%%%%%%%%%%%%%*/
    if((*image)->depth == 1)
      (*image)->format = XYBitmap;

    fprintf(stderr, "%s is %dx%d, depth %d (%s), %d colors.\n", file_name,
	    (*image)->width, (*image)->height, (*image)->depth,
	    (*image)->format == ZPixmap? "Z-Pixmap" :
	    (*image)->format == XYPixmap? "XY-Pixmap" : "XY-Bitmap", *ncolors);

    /*
     * Read in the pixmap buffer.
     */
    if(fread((char *) buffer, sizeof(char), (int)buffer_size, file)
       != buffer_size)
	XtWarning("Unable to read pixmap from dump file.");

    /*
     * Close the input file.
     */
    if(pipeline)
      (void) pclose(file);
    else
      (void) fclose(file);

    return win_name;
}

_swapshort (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
}

_swaplong (bp, n)
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
