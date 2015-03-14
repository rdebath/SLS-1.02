#include "common.h"
#include "extern.h"
#include "gif.h"

ReadGifFile(file_name, image, colors, ncolors)
     String file_name;
     XImage **image;
     XColor **colors;
     int *ncolors;
{
  Bool pipeline = False, has_colormap, interlaced;
  Byte *image_buffer;
  ByteStream stream;
  GifHeader header;
  ImageSpec image_spec;
  RawColor raw_color;
  RawGifHeader raw_header;
  RawImageSpec raw_image_spec;
  FILE *file;
  char shell_cmd[MAXCMD];
  int bits_per_pixel, colormap_size, code_size, initial_code_size;
  int code;
  int eof_code, clear_code, free_code, max_code, cur_code, old_code, in_code;
  int fin_char, first_free;
  int out_count = 0;
  int bit_mask, read_mask;
  int bit_offset = 0;
  int i, x = 0, y = 0;
  int pass = 0;
  int prefix[4096], suffix[4096], out_code[1025];

  if (strlen(file_name) > 1 &&
      !strcmp(file_name + strlen(file_name) - 2, ".Z")) {
    (void) sprintf(shell_cmd, "zcat %s", file_name);
    if((file = popen(shell_cmd, "r")) == NULL) {
      fprintf(stderr, "%s: can't uncompress file\n", program_name);
      exit(1);
    }
    pipeline = True;
  }
  else if (strcmp(file_name, "-")) {
    /*
     * Open the output file.
     */
    file = fopen(file_name, "r");
    if (!file) {
      fprintf(stderr, "%s: cannot open %s\n", program_name, file_name);
      exit(1);
    }
  }
  else {
    file_name = "<standard input>";
    file = stdin;
  }

  if(fread((char *) &raw_header, sizeof(RawGifHeader), 1, file) != 1) {
    fprintf(stderr, "%s: cannot open %s\n", program_name, file_name);
    exit(1);
  }

  if(strncmp(raw_header.id, GIF_ID, strlen(GIF_ID))) {
    fprintf(stderr, "%s: not a GIF file\n", program_name);
    exit(1);
  }

  if(raw_header.null_byte) {
    fprintf(stderr, "%s: corrupt GIF file\n", program_name);
    exit(2);
  }

  ConvertHeader(&raw_header, &header);

  has_colormap = ((header.colormap_info & COLORMAPMASK) ? True : False);

  bits_per_pixel = (header.colormap_info & 7) + 1;
  *ncolors = colormap_size = 1 << bits_per_pixel;
  bit_mask = colormap_size - 1;

  if(has_colormap) {
    if((*colors = (XColor *)calloc((unsigned) *ncolors, sizeof(XColor)))
       == NULL) {
      fprintf(stderr, "%s:  cannot calloc colormap storage\n");
      exit(1);
    }
    for(i = 0; i < *ncolors; ++i) {
      if(fread((char *) &raw_color, sizeof(RawColor), 1, file) != 1) {
	fprintf(stderr, "%s: error reading colortable.\n", program_name);
	exit(1);
      }
      (*colors)[i].red = raw_color.Red << 8;
      (*colors)[i].green = raw_color.Green << 8;
      (*colors)[i].blue = raw_color.Blue << 8;
      (*colors)[i].pixel = i;
    }
  }
  else {
    fprintf(stderr, "%s: warning: no colortable (winging it).\n",
	    program_name);
    if(*ncolors == 0)
      *ncolors = 256;
    if((*colors = (XColor *)calloc((unsigned) *ncolors, sizeof(XColor)))
       == NULL) {
      fprintf(stderr, "%s:  cannot calloc colormap storage\n");
      exit(1);
    }
    for(i = 0; i < *ncolors; ++i) {
      (*colors)[i].red = (*colors)[i].green = (*colors)[i].blue = i << 8;
      (*colors)[i].pixel = i;
    }
  }

  if(fread((char *) &raw_image_spec, sizeof(RawImageSpec), 1, file) != 1) {
    fprintf(stderr, "%s: unable to read image information.\n", program_name);
    exit(1);
  } 

  if(raw_image_spec.image_separator != IMAGESEP) {
    fprintf(stderr, "%s: corrupt GIF file (no image separator\n", program_name);
    exit(1);
  }

  ConvertSpec(&raw_image_spec, &image_spec);

  interlaced = ((image_spec.interlace & INTERLACEMASK) ? True : False);

  fprintf(stderr, "%s is %dx%d, %d bits per pixel, %sinterlaced, %d colors.\n",
	  file_name, image_spec.width, image_spec.height, bits_per_pixel,
	  interlaced? "" : "non-", colormap_size);

  code_size = image_spec.data_size;
  clear_code = (1 << code_size);
  eof_code = clear_code + 1;
  free_code = first_free = clear_code + 2;

  ++code_size;
  initial_code_size = code_size;
  max_code = (1 << code_size);
  read_mask = max_code - 1;

  InitializeByteStream(file, &stream);

  if((image_buffer =
      (Byte *)malloc((unsigned)image_spec.width*image_spec.height)) == NULL) {
    fprintf(stderr, "%s: cannot malloc storage for image.\n", program_name);
    exit(1);
  }

  *image = XCreateImage(dpy, DefaultVisual(dpy, screen), 8, ZPixmap, 0,
			(char *) image_buffer, image_spec.width,
			image_spec.height, 8, image_spec.width);

  if(*image == NULL) {
    fprintf(stderr, "%s: unable to create image.\n", program_name);
    exit(1);
  }

  code = ReadCode(&stream, &bit_offset, code_size, read_mask);
  while(code != eof_code) {
    if(code == clear_code) {
      code_size = initial_code_size;
      max_code = (1 << code_size);
      read_mask = max_code - 1;
      free_code = first_free;
      cur_code = old_code = code = ReadCode(&stream, &bit_offset, code_size,
					    read_mask);
      if(Corrupt(stream)) {
	fprintf(stderr, "%s: corrupt GIF file, continuing...\n", program_name);
	break;
      }
      fin_char = cur_code & bit_mask;
      add_to_pixel((Byte) fin_char, &pass, &x, &y, *image, interlaced);
    }
    else {
      cur_code = in_code = code;
      if(cur_code >= free_code) {
	cur_code = old_code;
	out_code[out_count++] = fin_char;
      }
      while(cur_code > bit_mask) {
	if(out_count > 1024) {
	  Corrupt(stream) = True;
	  break;
	}
	out_code[out_count++] = suffix[cur_code];
	cur_code = prefix[cur_code];
      }
      if(Corrupt(stream)) {
	fprintf(stderr, "%s:  corrupt GIF file, continuing...\n",
		program_name);
	break;
      }
      fin_char = cur_code & bit_mask;
      out_code[out_count++] = fin_char;
      for(i = out_count - 1; i >= 0; --i)
	add_to_pixel((Byte) out_code[i], &pass, &x, &y, *image, interlaced);
      out_count = 0;
      prefix[free_code] = old_code;
      suffix[free_code] = fin_char;
      old_code = in_code;
      ++free_code;
      if(free_code >= max_code) {
	if(code_size < 12) {
	  ++code_size;
	  max_code <<= 1;
	  read_mask = (1 << code_size) - 1;
	}
      }
    }
    code = ReadCode(&stream, &bit_offset, code_size, read_mask);
  }
  if(pipeline)
    (void) pclose(file);
  else if(file != stdin)
    (void) fclose(file);
}

ConvertHeader(raw, cooked)
     RawGifHeader *raw;
     GifHeader *cooked;
{
  String strncpy();

  (void) strncpy(raw->id, cooked->id, 6);
  cooked->width = ConvertToInt(raw->width);
  cooked->height = ConvertToInt(raw->height);
  cooked->colormap_info = raw->colormap_info;
  cooked->background_color = raw->background_color;
}

ConvertSpec(raw, cooked)
     RawImageSpec *raw;
     ImageSpec *cooked;
{
  cooked->left_offset = ConvertToInt(raw->left_offset);
  cooked->top_offset = ConvertToInt(raw->top_offset);
  cooked->width = ConvertToInt(raw->width);
  cooked->height = ConvertToInt(raw->height);
  cooked->interlace = raw->interlace;
  cooked->data_size = raw->data_size;
}

InitializeByteStream(fp, f)
     FILE *fp;
     ByteStream *f;
{
  f->file_ptr = fp;
  f->corrupt = False;
  f->current_byte_offset = -2;
  f->bytes_left = getc(fp) - 1;
  NextNextByte(f) = getc(fp);
}

ReadCode(f, bit_offset, size, mask)
     ByteStream *f;
     int *bit_offset, size, mask;
{
  int raw_code, byte_offset;

  byte_offset = *bit_offset / 8;
  FixFilePosition(f, byte_offset);
  raw_code = CurrentByte(f) + (NextByte(f) << 8);
  if(size >= 8)
    raw_code += NextNextByte(f) << 16;
  raw_code >>= (*bit_offset % 8);
  *bit_offset += size;
  return (raw_code & mask);
}

add_to_pixel(index, pass, xc, yc, image, interlaced)
     Byte index;
     int *pass;
     int *xc, *yc;
     XImage *image;
     Bool interlaced;
{
  if(*yc < image->height)
    *(image->data + *yc * image->bytes_per_line + *xc) = index;

  if(++*xc == image->width) {
    *xc = 0;
    if(!interlaced)
      ++*yc;
    else {
      switch(*pass) {
      case 0:
	*yc += 8;
	if(*yc >= image->height) {
	  ++*pass;
	  *yc = 4;
	}
	break;
      case 1:
	*yc += 8;
	if(*yc >= image->height) {
	  ++*pass;
	  *yc = 2;
	}
	break;
      case 2:
	*yc += 4;
	if(*yc >= image->height) {
	  ++*pass;
	  *yc = 1;
	}
	break;
      case 3:
	*yc += 2;
	break;
      }
    }
  }
}
