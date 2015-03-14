#include "common.h"
#include "extern.h"

typedef struct _Pair {
  Pixel pixel;
  long freq;
} Pair;

XImage *
ProcessImage(image, colors, ncolors, color_limit, x, y)
     XImage *image;
     XColor *colors;
     int ncolors, color_limit, x, y;     
{
  Pixel *GetColorUsage(), *new_ranking;
  XImage *new_image;
  double ColorDistance();

  unsave_past = True;
  if (image->depth > 1) {
    new_ranking = optimize ? GetColorUsage(image, ncolors) : NULL;
    AllocateColors(colors, ncolors, new_ranking, color_limit);
    if(new_ranking)
      free((char *) new_ranking);
    if(x == 0 || y == 0) {
      x = image->width;
      y = image->height;
    }
    MapAndAutoscaleImage(image, colors, &new_image, x, y);
    XDestroyImage(image);
    save_colors = True;
    return new_image;
  }
  else {
    if(x > 0 && y > 0)
      AutoscaleImage(image, x, y);
    return image;
  }
}

PCompare(p1, p2)
     Pair *p1, *p2;
{
  if(p1->freq < p2->freq)
    return 1;
  else if(p1->freq > p2->freq)
    return -1;
  else
    return 0;
}

Pixel *GetColorUsage(image, ncolors)
     XImage *image;
     int ncolors;
{
  Pair *usage_info;
  Pixel *rank_list;
  int i, j, unused;

  if((usage_info = (Pair *)calloc((unsigned)ncolors, sizeof(Pair))) == NULL ||
     (rank_list = (Pixel *)calloc((unsigned)ncolors, sizeof(Pixel))) == NULL) {
    fprintf(stderr, "%s: can't calloc color mapping storage.\n", program_name);
    exit(1);
  }
  for(i = 0; i < ncolors; ++i) {
    usage_info[i].pixel = i;
    usage_info[i].freq = 0;
  }
  for(i = 0; i < image->width; ++i)
    for(j = 0; j < image->height; ++j)
      ++usage_info[image->data[image->bytes_per_line * j + i]].freq;
  qsort((char *) usage_info, ncolors, sizeof(Pair), PCompare);
  unused = 0;
  for(i = 0; i < ncolors; ++i) {
    if(usage_info[i].freq == 0)
      ++unused;
    rank_list[i] = usage_info[i].pixel;
  }
  if(unused > 0)
    fprintf(stderr, "(%d colors unused.)\n", unused);
  return rank_list;
}

/*
 * Strategy for compromise:  If a color cannot be allocated (because the
 * colormap is full), find the closest existing color currently allocated.
 */
TryBestColorMatch(cl, test_color, num_colors)
XColor cl[], *test_color;
int num_colors;
{
  int i;
  long low_pixel_diff, pixel_diff, l_abs();

  low_pixel_diff = (-1);
  for(i = 0; i < num_colors; ++i) {
    pixel_diff = l_abs((long)(test_color->red - cl[i].red)) +
                 l_abs((long)(test_color->green - cl[i].green)) +
		 l_abs((long)(test_color->blue - cl[i].blue));
    if(low_pixel_diff < 0 || pixel_diff < low_pixel_diff) {
      test_color->pixel = cl[i].pixel;
      low_pixel_diff = pixel_diff;
    }
  }
}

long l_abs(x)
long x;
{
  return (x < 0) ? -x : x;
}

/*
 * Allocate all necessary colors in the default colormap, if possible.
 */
AllocateColors(colors, ncolors, ranked_colors, limit)
     XColor *colors;
     int ncolors;
     Pixel *ranked_colors;
     int limit;
{
  Bool color_table_full = False;
  XColor cmaplist[MAXCOLORS];
  int i, j, index;

  if(limit <= 0 || limit > ncolors)
    limit = ncolors;
  for(i = 0; i < ncolors; ++i) {
    index = ranked_colors ? ranked_colors[i] : i;
    if(i >= limit ||
       XAllocColor(dpy, DefaultColormap(dpy, screen), &colors[index]) == 0) {
      if(!color_table_full && i < limit) {
	if(unsave_past) {
	  FixupState();
	  unsave_past = False;
	  if(XAllocColor(dpy, DefaultColormap(dpy, screen), &colors[index]) != 0)
	    continue;
	}
	fprintf(stderr,
		"%s: warning: cannot allocate all the desired colors.\n",
		program_name);
	for(j = 0; j < DisplayCells(dpy, screen); ++j)
	  cmaplist[j].pixel = j;
	XQueryColors(dpy, DefaultColormap(dpy, screen), cmaplist, 
		     DisplayCells(dpy, screen));
	color_table_full = True;
      }
      TryBestColorMatch(cmaplist, &colors[index], DisplayCells(dpy, screen));
    }
  }
}

/*
 * This routine combines the operations of autoscaling and pixel mapping in
 * order to boost efficiency when handling autoscaled color images.
 */
MapAndAutoscaleImage(image, colors, new_image, expand_width, expand_height)
     XImage *image;
     XColor *colors;
     XImage **new_image;
     int expand_width, expand_height;
{
    Bool indirect_scaling;
    Byte *buffer, *old_buffer, *templine;
    Pixel pixel;
    int *table_x, *table_y;
    int expand_bpl, marker, nmarker;
    int r, s, tempm;
    register m, n, acc_x, acc_y;
    unsigned buffer_size;
    
    expand_bpl = image->bytes_per_line * expand_width / image->width;

    table_x = (int *) calloc((unsigned)expand_width, sizeof(int));
    table_y = (int *) calloc((unsigned)expand_height, sizeof(int));
    if (!table_x || !table_y) {
	fprintf(stderr, "%s: cannot calloc autoscaling tables.\n",
		program_name);
	exit(1);
    }

    old_buffer = (Byte *) image->data;

    buffer_size = DataSize(expand_bpl, expand_height,
			   image->depth, image->format);

    if(!(buffer = (Byte *) malloc(buffer_size))) {
	fprintf(stderr, "%s: cannot malloc new data buffer.\n",	program_name);
	exit(1);
    }

    /*
     * Create image that matches default visual.
     */
    *new_image = XCreateImage(dpy, 
			      DefaultVisual(dpy, screen),
			      DefaultDepth(dpy, screen),
			      ZPixmap, image->xoffset, (char *) buffer, 
			      expand_width, expand_height,
			      image->bitmap_pad, expand_bpl);

    indirect_scaling = portable;

    templine = (Byte *) malloc((unsigned)expand_width);
    if(templine == NULL) {
	fprintf(stderr, "%s: cannot malloc temporary line.\n", program_name);
	exit(1);
    }

    marker = nmarker = 0;

    for(r = 0; r <= image->width; ++r)
	table_x[r] = (int) ((r + 1) * expand_width / image->width) -
	    (int) (r * expand_width / image->width);
    for(r = 0; r <= image->height; ++r)
	table_y[r] = (int) ((r + 1) * expand_height / image->height) -
	    (int) (r * expand_height / image->height);
    if(indirect_scaling) {
	acc_y = 0;
	for(r = 0; r < image->height; acc_y += table_y[r++]) {
	    acc_x = 0;
	    for(s = 0; s < image->width; acc_x += table_x[s++]) {
		pixel = colors[XGetPixel(image, s, r)].pixel;
		for(m = 0; m < table_x[s]; ++m) {
		    for(n = 0; n < table_y[r]; ++n)
			XPutPixel(*new_image, acc_x + m, acc_y + n, pixel);
		}
	    }
	}
    }
    else
	for(r = 0; r < image->height; ++r) {
	    tempm = 0;
	    for(s = 0; s < image->width; ++s) {
		for(m = 0; m < table_x[s]; ++m) {
		    templine[tempm++] = colors[old_buffer[marker]].pixel;
		}
		++marker;
	    }
	    for(m = 0; m < table_y[r]; ++m) {
		bcopy((char *) templine, (char *) buffer + nmarker,
		      expand_width);
		nmarker += expand_width;
	    }
	}
    
    free((char *)templine);
    free((char *)table_x);
    free((char *)table_y);
}

/*
 * Autoscale the image.
 * This routine uses floating-point arithmetic to scale the image
 * to the EXACT size of the screen.
 */    
AutoscaleImage(image, expand_width, expand_height)
     XImage *image;
     int expand_width, expand_height;
{
    Bool indirect_scaling;
    Pixel pixel;
    XImage *old_image;
    Byte *buffer, *old_buffer, *templine;
    int *table_x, *table_y;
    int expand_bpl, marker, nmarker;
    int r, s, tempm;
    int bit, newbit, pow2, newpow2, bitval;
    register m, n, acc_x, acc_y;
    unsigned buffer_size;

    old_image = XCreateImage(dpy, DefaultVisual(dpy, screen), 
			       image->depth, image->format, 
			       image->xoffset, image->data, 
			       image->width, image->height, 
			       image->bitmap_pad, image->bytes_per_line);
    old_image->byte_order = image->byte_order;
    old_image->bitmap_bit_order = image->bitmap_bit_order;

    expand_bpl = image->bytes_per_line * expand_width / image->width;

    table_x = (int *) calloc((unsigned)expand_width, sizeof(int));
    table_y = (int *) calloc((unsigned)expand_height, sizeof(int));
    if (!table_x || !table_y) {
	fprintf(stderr, "%s: cannot calloc autoscaling tables.\n",
		program_name);
	exit(1);
    }

    old_buffer = (Byte *) old_image->data;

    image->bytes_per_line = expand_bpl;
    image->width = expand_width;
    image->height = expand_height;
    
    buffer_size = DataSize(image->bytes_per_line, image->height,
			   image->depth, image->format);

    if(!(buffer = (Byte *) malloc(buffer_size))) {
	fprintf(stderr, "%s: cannot malloc new data buffer.\n", program_name);
	exit(1);
    }
    image->data = (char *) buffer;

    indirect_scaling = portable;

    templine = (Byte *) malloc((unsigned)expand_width);
    if(templine == NULL) {
	fprintf(stderr, "%s: cannot malloc temporary line.\n", program_name);
	exit(1);
    }

    marker = nmarker = 0;

    for(r = 0; r <= old_image->width; ++r)
	table_x[r] = (int) ((r + 1) * image->width / old_image->width) -
	    (int) (r * image->width / old_image->width);
    for(r = 0; r <= old_image->height; ++r)
	table_y[r] = (int) ((r + 1) * image->height / old_image->height) -
	    (int) (r * image->height / old_image->height);
    if(indirect_scaling)
	for(acc_y = 0, r = 0; r < old_image->height; acc_y += table_y[r++]) {
	    for(acc_x = 0, s = 0; s < old_image->width; acc_x += table_x[s++]){
		pixel = XGetPixel(old_image, s, r);
		for(m = 0; m < table_x[s]; ++m) {
		    for(n = 0; n < table_y[r]; ++n)
			XPutPixel(image, acc_x + m, acc_y + n, pixel);
		}
	    }
	}
    else if(old_image->format == XYBitmap)
      for(r = 0; r < old_image->height; ++r) {
	tempm = 0;
	newbit = 0;
	newpow2 = 1;
	for(s = 0; s < old_image->bytes_per_line; ++s, ++marker) {
	  for(bit = 0, pow2 = 1; bit < 8; ++bit, pow2 <<= 1) {
	    bitval = (old_buffer[marker] & pow2);
	    m = table_x[8 * s + bit];
	    while(m--) {
	      if(bitval)
		templine[tempm] |= newpow2;
	      else
		templine[tempm] &= ~newpow2;
	      if(++newbit >= 8) {
		newbit = 0;
		newpow2 = 1;
		++tempm;
	      }
	      else
		newpow2 <<= 1;
	    }
	  }
	}
	for(m = 0; m < table_y[r]; ++m) {
	  bcopy((char *) templine, (char *) buffer + nmarker,
		image->bytes_per_line);
	  nmarker += image->bytes_per_line;
	}
      }
    else
	for(r = 0; r < old_image->height; ++r) {
	    tempm = 0;
	    for(s = 0; s < old_image->bytes_per_line; ++s) {
		for(m = 0; m < table_x[s]; ++m) {
		    templine[tempm++] = old_buffer[marker];
		}
		++marker;
	    }
	    for(m = 0; m < table_y[r]; ++m) {
		bcopy((char *) templine, (char *) buffer + nmarker,
		      image->width);
		nmarker += image->width;
	    }
	}
    
    XDestroyImage(old_image);
    
    free((char *)templine);
    free((char *)old_buffer);
    free((char *)table_x);
    free((char *)table_y);
}

unsigned long DataSize(bpl, height, depth, format)
     int bpl, height, depth, format;
{
    if (format != ZPixmap)
	return (bpl * height * depth);
    else
	return(bpl * height);
}
