/* dvi_draw.c: display a DVI file.  */

#include "config.h"
#include <ctype.h>

#include "dvi.h"

/*
 *	Explanation of the following constant:
 *	offset_[xy]   << 16:	margin (defaults to one inch)
 *	shrink_factor << 16:	one pixel page border
 *	shrink_factor << 15:	rounding for pixel_conv
 */
#define OFFSET_X	(offset_x << 16) + (shrink_factor * 3 << 15)
#define OFFSET_Y	(offset_y << 16) + (shrink_factor * 3 << 15)

struct frame *stack;
struct frame *stackp;

#ifndef	BMLONG
#ifndef	BMSHORT
unsigned char bit_masks[9] =
{
  0x0, 0x1, 0x3, 0x7,
  0xf, 0x1f, 0x3f, 0x7f,
  0xff
};
#else /* BMSHORT */
unsigned short bit_masks[17] =
{
  0x0, 0x1, 0x3, 0x7,
  0xf, 0x1f, 0x3f, 0x7f,
  0xff, 0x1ff, 0x3ff, 0x7ff,
  0xfff, 0x1fff, 0x3fff, 0x7fff,
  0xffff
};
#endif /* BMSHORT */
#else /* BMLONG */
unsigned long bit_masks[33] =
{
  0x0, 0x1, 0x3, 0x7,
  0xf, 0x1f, 0x3f, 0x7f,
  0xff, 0x1ff, 0x3ff, 0x7ff,
  0xfff, 0x1fff, 0x3fff, 0x7fff,
  0xffff, 0x1ffff, 0x3ffff, 0x7ffff,
  0xfffff, 0x1fffff, 0x3fffff, 0x7fffff,
  0xffffff, 0x1ffffff, 0x3ffffff, 0x7ffffff,
  0xfffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
  0xffffffff
};
#endif /* BMLONG */

boolean check_dvi_file ();
void applicationDoSpecial ();

static
print_bitmap (bitmap)
     register struct bitmap *bitmap;
{
  register BMUNIT *ptr = (BMUNIT *) bitmap->bits;
  register int x, y, i;

  if (ptr == NULL)
    oops ("print_bitmap called with null pointer.");
  Printf ("w = %d, h = %d, bytes wide = %d\n",
	  bitmap->w, bitmap->h, bitmap->bytes_wide);
  for (y = 0; y < bitmap->h; ++y)
    {
      for (x = bitmap->bytes_wide; x > 0; x -= BYTES_PER_BMUNIT)
	{
#ifndef	MSBITFIRST
	  for (i = 0; i < BITS_PER_BMUNIT; ++i)
#else
	  for (i = BITS_PER_BMUNIT - 1; i >= 0; --i)
#endif
	    Putchar ((*ptr & (1 << i)) ? '@' : ' ');
	  ++ptr;
	}
      Putchar ('\n');
    }
}

static void
print_char (ch, g)
     ubyte ch;
     struct glyph *g;
{
  Printf ("char %d", ch);
  if (isprint (ch))
    Printf (" (%c)", ch);
  Putchar ('\n');
  Printf ("x = %d, y = %d, dvi = %d\n",
	  g->x, g->y, g->dvi_adv);
  print_bitmap (&g->bitmap);
}

static char *dvi_table1[]=
{
  "SET1", NULL, NULL, NULL, "SETRULE", "PUT1", NULL, NULL,
  NULL, "PUTRULE", "NOP", "BOP", "EOP", "PUSH", "POP", "RIGHT1",
  "RIGHT2", "RIGHT3", "RIGHT4", "W0", "W1", "W2", "W3", "W4",
  "X0", "X1", "X2", "X3", "X4", "DOWN1", "DOWN2", "DOWN3",
  "DOWN4", "Y0", "Y1", "Y2", "Y3", "Y4", "Z0", "Z1",
  "Z2", "Z3", "Z4"};

static char *dvi_table2[]=
{
  "FNT1", "FNT2", "FNT3", "FNT4", "XXX1", "XXX2", "XXX3", "XXX4",
  "FNTDEF1", "FNTDEF2", "FNTDEF3", "FNTDEF4", "PRE", "POST", "POSTPOST",
  NULL, NULL, NULL, NULL, NULL, NULL};

static void
print_dvi (ch)
     ubyte ch;
{
  char *s;

  if (stackp != NULL)
    Printf ("%4d %4d ", PXL_H, PXL_V);
  else
    Fputs ("          ", stdout);

  if (ch <= SETCHAR0 + 127)
    {
      Printf ("SETCHAR%-3d", ch - SETCHAR0);
      if (isprint (ch))
	Printf (" (%c)", ch);
      Putchar ('\n');
      return;
    }
  else if (ch < FNTNUM0)
    s = dvi_table1[ch - 128];
  else if (ch <= FNTNUM0 + 63)
    {
      Printf ("FNTNUM%d\n", ch - FNTNUM0);
      return;
    }
  else
    s = dvi_table2[ch - (FNTNUM0 + 64)];
  if (s)
    puts (s);
  else
    oops ("Unknown opcode %d, offset %d", ch, ftell (dvi_file) - 1);
}

/**
 **	Allocate bitmap for given font and character
 **/

void
alloc_bitmap (bitmap)
     register struct bitmap *bitmap;
{
  register unsigned int size;

  /* width must be multiple of 16 bits for raster_op */
  bitmap->bytes_wide = ROUNDUP (bitmap->w, BITS_PER_BMUNIT) * BYTES_PER_BMUNIT;
  size = bitmap->bytes_wide * bitmap->h;
  bitmap->bits = (char *) xmalloc (size != 0 ? size : 1);
}

char sample_count[]=
{0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};

static int
sample (bits, bytes_wide, bit_skip, w, h)
     BMUNIT *bits;
     int bytes_wide, bit_skip, w, h;
{
  BMUNIT *ptr, *endp;
  register BMUNIT *cp;
  int bits_left;
  register int n, bit_shift, wid;

  ptr = bits + bit_skip / BITS_PER_BMUNIT;
  endp = ADD (bits, h * bytes_wide);
  bits_left = w;
#ifndef	MSBITFIRST
  bit_shift = bit_skip % BITS_PER_BMUNIT;
#else
  bit_shift = BITS_PER_BMUNIT - bit_skip % BITS_PER_BMUNIT;
#endif
  n = 0;
  while (bits_left)
    {
#ifndef	MSBITFIRST
      wid = BITS_PER_BMUNIT - bit_shift;
#else
      wid = bit_shift;
#endif
      if (wid > bits_left)
	wid = bits_left;
      if (wid > 4)
	wid = 4;
#ifdef	MSBITFIRST
      bit_shift -= wid;
#endif
      for (cp = ptr; cp < endp; cp = ADD (cp, bytes_wide))
	n += sample_count[(*cp >> bit_shift) & bit_masks[wid]];
#ifndef	MSBITFIRST
      bit_shift += wid;
      if (bit_shift == BITS_PER_BMUNIT)
	{
	  bit_shift = 0;
	  ++ptr;
	}
#else
      if (bit_shift == 0)
	{
	  bit_shift = BITS_PER_BMUNIT;
	  ++ptr;
	}
#endif
      bits_left -= wid;
    }
  return n;
}

static void
shrink_glyph (g)
     register struct glyph *g;
{
  int shrunk_bytes_wide, shrunk_height;
  int rows_left, rows, init_cols, cols_left;
  register int cols;
  BMUNIT *old_ptr, *new_ptr;
  register BMUNIT m, *cp;
  int min_sample = shrink_factor * shrink_factor * density / 100;

  /* These machinations ensure that the character is shrunk according to
     its hot point, rather than its upper left-hand corner. */
  g->x2 = g->x / shrink_factor;
  init_cols = g->x - g->x2 * shrink_factor;
  if (init_cols <= 0)
    init_cols += shrink_factor;
  else
    ++g->x2;
  g->bitmap2.w = g->x2 + ROUNDUP (g->bitmap.w - g->x, shrink_factor);
  /* include row zero with the positively numbered rows */
  cols = g->y + 1;		/* spare register variable */
  g->y2 = cols / shrink_factor;
  rows = cols - g->y2 * shrink_factor;
  if (rows <= 0)
    {
      rows += shrink_factor;
      --g->y2;
    }
  g->bitmap2.h = shrunk_height = g->y2 +
    ROUNDUP (g->bitmap.h - cols, shrink_factor) + 1;
  if (g->bitmap2.bits)
    free (g->bitmap2.bits);
  alloc_bitmap (&g->bitmap2);
  old_ptr = (BMUNIT *) g->bitmap.bits;
  new_ptr = (BMUNIT *) g->bitmap2.bits;
  shrunk_bytes_wide = g->bitmap2.bytes_wide;
  rows_left = g->bitmap.h;
  bzero ((char *) new_ptr, shrunk_bytes_wide * shrunk_height);
  while (rows_left)
    {
      if (rows > rows_left)
	rows = rows_left;
      cols_left = g->bitmap.w;
#ifndef	MSBITFIRST
      m = (1 << 0);
#else
      m = (1 << (BITS_PER_BMUNIT - 1));
#endif
      cp = new_ptr;
      cols = init_cols;
      while (cols_left)
	{
	  if (cols > cols_left)
	    cols = cols_left;
	  if (sample (old_ptr, g->bitmap.bytes_wide,
		      g->bitmap.w - cols_left, cols, rows) >= min_sample)
	    *cp |= m;
#ifndef	MSBITFIRST
	  if (m == (BMUNIT) (1 << (BITS_PER_BMUNIT - 1)))
	    {
	      m = (1 << 0);
	      ++cp;
	    }
	  else
	    m <<= 1;
#else
	  if (m == (1 << 0))
	    {
	      m = (1 << (BITS_PER_BMUNIT - 1));
	      ++cp;
	    }
	  else
	    m >>= 1;
#endif
	  cols_left -= cols;
	  cols = shrink_factor;
	}
      *((char **) &new_ptr) += shrunk_bytes_wide;
      *((char **) &old_ptr) += rows * g->bitmap.bytes_wide;
      rows_left -= rows;
      rows = shrink_factor;
    }
  g->y2 = g->y / shrink_factor;
  if (debug & DBG_BITMAP)
    print_bitmap (&g->bitmap2);
}


/* Set the character CH at the current position.  */

static void
set_char (ch)
     ubyte ch;
{
  register struct glyph *g;

  /* If the font didn't have this many characters, forget it.  */
  if (ch > maxchar)
    {
      if (!hush_chars)
	Fprintf (stderr, "%s: Character %d past end of %s.\n",
		 prog, ch, current_font->fontname);
      return;
    }

  /* The character might exist.  Get the character info.  */
  g = &current_font->glyph[ch];
  if (g->bitmap.bits == NULL)
    {				/* We do not have an unpacked bitmap.  Do we have a packed one?  */
      if (g->packed_data == NULL)
	{			/* No unpacked one.  Give up.  */
	  if (!hush_chars)
	    Fprintf (stderr, "%s: Character %d undefined in %s.\n",
		     prog, ch, current_font->fontname);
	  return;
	}

      /* Unpack the bitmap.  */
      (*current_font->unpack_bitmap) (ch);
    }

  /* Now we have an unpacked bitmap.  Do we need to shrink it?  */
  if (shrink_factor == 1)
    put_bitmap (&g->bitmap, PXL_H - g->x, PXL_V - g->y);
  else
    {
      if (g->bitmap2.bits == NULL)
	shrink_glyph (g);
      put_bitmap (&g->bitmap2, PXL_H - g->x2, PXL_V - g->y2);
    }
}

static void
set_rule (h, w)
     int h, w;
{
  /* (w,h) specifies lower left corner of rule box */
  put_rectangle (PXL_H, PXL_V - h + 1, w, h, false);
}

/* Find font #n and move it to the head of the list. */

static void
change_font (n)
     unsigned long n;
{
  register struct font *fontp, **prev;

  prev = &current_font;
  for (;;)
    {
      fontp = *prev;
      if (fontp == NULL)
	oops ("Non-existent font #%d", n);
      if (fontp->TeXnumber == n)
	break;
      prev = &(fontp->next);
    }
  *prev = fontp->next;
  fontp->next = current_font;
  current_font = fontp;
  maxchar = current_font->maxchar;
}

static void
special (nbytes)
     long nbytes;
{
  static char *cmd = NULL;
  static long cmdlen = -1;

  if (cmdlen < nbytes)
    {
      if (cmd)
	free (cmd);
      cmd = (char *) xmalloc ((unsigned) nbytes + 1);
      cmdlen = nbytes;
    }
  Fread (cmd, sizeof (char), (int) nbytes, dvi_file);
  cmd[nbytes] = '\0';
  applicationDoSpecial (cmd);
}

draw_page ()
{
  ubyte ch;

  /* Check for changes in dvi file. */
  if (!check_dvi_file ())
    return;

  put_border (ROUNDUP (unshrunk_paper_w, shrink_factor) + 1,
	      ROUNDUP (unshrunk_paper_h, shrink_factor) + 1, 1);

  Fseek (dvi_file, page_offset[current_page], SEEK_SET);
  for (;;)
    {
      ch = one (dvi_file);
      if (debug & DBG_DVI)
	print_dvi (ch);
      if (ch <= SETCHAR0 + 127)
	{
	  set_char (ch);
	  DVI_H += current_font->glyph[ch].dvi_adv;
	}
      else if (FNTNUM0 <= ch && ch <= FNTNUM0 + 63)
	{
	  change_font ((unsigned long) (ch - FNTNUM0));
	}
      else
	{
	  long a, b;
	  ubyte ch1;

	  switch (ch)
	    {
	    case SET1:
	    case PUT1:
	      ch1 = one (dvi_file);
	      set_char (ch1);
	      if (ch == SET1)
		DVI_H += current_font->glyph[ch1].dvi_adv;
	      break;

	    case SETRULE:
	      /* Be careful, dvicopy outputs rules with
	         height = 0x80000000.  We don't want any
	         SIGFPE here. */
	      a = sfour (dvi_file);
	      b = spellfour (dvi_file);
	      if (a > 0 && b > 0)
		set_rule (pixel_round ((long) a * fraction),
			  pixel_round (b));
	      DVI_H += b;
	      break;

	    case PUTRULE:
	      a = spellfour (dvi_file);
	      b = spellfour (dvi_file);
	      if (a > 0 && b > 0)
		set_rule (pixel_round (a), pixel_round (b));
	      break;

	    case NOP:
	      break;

	    case BOP:
	      Fseek (dvi_file, (long) 11 * 4, SEEK_CUR);
	      stackp = stack;
	      DVI_H = OFFSET_X;
	      DVI_V = OFFSET_Y;
	      PXL_V = pixel_conv (DVI_V);
	      WW = XX = YY = ZZ = 0;
	      break;

	    case EOP:
	      if (stackp > stack)
		oops ("Stack not empty at EOP (%d)",
		      stackp - stack);
	      return;

	    case PUSH:
	      stackp++;
	      if (stackp > stack + maxstack)
		oops ("More PUSHes than were promised");
	      *stackp = stackp[-1];
	      break;

	    case POP:
	      if (stackp <= stack)
		oops ("More POPs than PUSHes");
	      stackp--;
	      break;

	    case RIGHT1:
	    case RIGHT2:
	    case RIGHT3:
	    case RIGHT4:
	      DVI_H += spellnum (dvi_file, ch - RIGHT1 + 1);
	      break;

	    case X1:
	    case X2:
	    case X3:
	    case X4:
	      XX = spellnum (dvi_file, ch - X0);
	    case X0:
	      DVI_H += XX;
	      break;

	    case W1:
	    case W2:
	    case W3:
	    case W4:
	      WW = spellnum (dvi_file, ch - W0);
	    case W0:
	      DVI_H += WW;
	      break;

	    case Y1:
	    case Y2:
	    case Y3:
	    case Y4:
	      YY = spellnum (dvi_file, ch - Y0);
	    case Y0:
	      DVI_V += YY;
	      PXL_V = pixel_conv (DVI_V);
	      break;

	    case Z1:
	    case Z2:
	    case Z3:
	    case Z4:
	      ZZ = spellnum (dvi_file, ch - Z0);
	    case Z0:
	      DVI_V += ZZ;
	      PXL_V = pixel_conv (DVI_V);
	      break;

	    case DOWN1:
	    case DOWN2:
	    case DOWN3:
	    case DOWN4:
	      DVI_V += spellnum (dvi_file, ch - DOWN1 + 1);
	      PXL_V = pixel_conv (DVI_V);
	      break;

	    case FNT1:
	    case FNT2:
	    case FNT3:
	    case FNT4:
	      change_font (num (dvi_file, ch - FNT1 + 1));
	      break;

	    case XXX1:
	    case XXX2:
	    case XXX3:
	    case XXX4:
	      a = num (dvi_file, ch - XXX1 + 1);
	      if (a > 0)
		special (a);
	      break;

	    case FNTDEF1:
	    case FNTDEF2:
	    case FNTDEF3:
	    case FNTDEF4:
	      Fseek (dvi_file, (long) (12 + ch - FNTDEF1 + 1),
		     SEEK_CUR);
	      a = one (dvi_file) + one (dvi_file);
	      Fseek (dvi_file, (long) a, SEEK_CUR);
	      break;

	    case PRE:
	      oops ("Shouldn't happen: PRE encountered.");
	      break;

	    case POST:
	      oops ("Shouldn't happen: POST encountered.");
	      break;

	    case POSTPOST:
	      oops ("Unexpected POSTPOST encountered.");
	      break;

	    default:
	      oops ("Unknown op-code %d, offset %d",
		    ch, ftell (dvi_file));
	    }			/* end switch*/
	}			/* end else (ch not a SETCHAR or FNTNUM) */
    }				/* end for */
}

/*
**
**      Read size bytes from the FILE fp, constructing them into a
**      signed/unsigned integer.
**
*/
unsigned long
num (fp, size)
     register FILE *fp;
     register ubyte size;
{
  register int i;
  register long x;

  x = 0;
  for (i = 0; i < size; i += 1)
    x = (x << 8) + (unsigned) (getc (fp) & 0xff);
  return (x);
}

long
snum (fp, size)
     register FILE *fp;
     register ubyte size;
{
  register int i;
  register long x;

  x = getc (fp) & 0xff;
  if (x & 0x80)
    x -= 0x100;
  for (i = 1; i < size; i += 1)
    x = (x << 8) + (unsigned) (getc (fp) & 0xff);
  return (x);
}

/* VARARGS1 */
oops (message, a, b, c, d, e, f)
     char *message;
{
  Fprintf (stderr, "%s: ", prog);
  Fprintf (stderr, message, a, b, c, d, e, f);
  Putc ('\n', stderr);
  exit (1);
}
