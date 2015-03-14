/* pk.c: PK font reading routines.  */

#include "config.h"

static void read_packed_bitmap P2H(FILE *, struct glyph *);

#define PK_ID      89
#define PK_CMD_START 240
#define PK_X1     240
#define PK_X2     241
#define PK_X3     242
#define PK_X4     243
#define PK_Y      244
#define PK_POST   245
#define PK_NOOP   246
#define PK_PRE    247

/* Utility routines to read the PK character data.  */
static int PK_flag_byte;
static unsigned PK_input_byte;
static int PK_bitpos;
static int PK_dyn_f;
static int PK_repeat_count;
static char *data_string;

#define data_one() ((unsigned long) (*data_string++ & 0xff))
#define data_four() data_num (4)
#define data_sfour() data_snum (4)

static long
data_snum P1C(ubyte, size)
{
  register int i;
  register long x = *data_string++ & 0xff;

  if (x & 0x80)
    x -= 0x100;

  for (i = 1; i < size; i += 1)
    x = (x<<8) + (unsigned) (*data_string++ & 0xff);

  return x;
}


static unsigned long
data_num P1C(ubyte, size)
{
  register int i;
  register long x = 0;

  for (i = 0; i < size; i += 1)
    x = (x<<8) + (unsigned) (*data_string++ & 0xff);
  return x;
}


static int
PK_get_nyb ()
{
  unsigned temp;
  if (PK_bitpos < 0)
    {
      PK_input_byte = data_one ();
      PK_bitpos = 4;
    }
  temp = PK_input_byte >> PK_bitpos;
  PK_bitpos -= 4;
  return temp & 0xf;
}


static int
PK_packed_num ()
{
  int i, j;
  if ((i = PK_get_nyb ()) == 0)
    {
      do
	{
	  j = PK_get_nyb ();
	  i++;
	}
      while (j == 0);

      while (i > 0)
	{
	  j = (j << 4) + PK_get_nyb ();
	  i--;
	}
      return (j - 15 + ((13 - PK_dyn_f) << 4) + PK_dyn_f);
    }
  else
    {
      if (i <= PK_dyn_f)
	return (i);
      if (i < 14)
	return ((i - PK_dyn_f - 1) << 4) + PK_get_nyb () + PK_dyn_f + 1;
      if (i == 14)
	PK_repeat_count = PK_packed_num ();
      else
	PK_repeat_count = 1;
      return PK_packed_num ();
    }
}

static void
PK_skip_specials P2C(FILE *, f,  struct font *, fontp)
{
  int i, j;
  do
    {
      PK_flag_byte = one (f);
      if (PK_flag_byte >= PK_CMD_START)
	{
	  switch (PK_flag_byte)
	    {
	    case PK_X1:
	    case PK_X2:
	    case PK_X3:
	    case PK_X4:
	      {
		i = 0;
		for (j = PK_CMD_START; j <= PK_flag_byte; j++)
		  i = (i * 256) + one (f);
		while (i--)
		  (void) one (f);
		break;
	      }
	    case PK_Y:
	      (void) four (f);
	    case PK_POST:
	    case PK_NOOP:
	      break;
	    default:
	      oops ("Unexpected %d in PK file %s", PK_flag_byte,
		    fontp->fontname);
	      break;
	    }
	}
    }
  while (PK_flag_byte != PK_POST && PK_flag_byte >= PK_CMD_START);
}

/* Read a PK font from the file F into the structure FONTP, but don't
   unpack the bitmaps.  */

static void
pk_glyphs_fn P2C(FILE *, f,  struct font *, fontp)
{
  register struct glyph *g;
  int hppp, vppp;

  if (debug & DBG_PK)
    Printf ("Reading PK pixel file %s\n", fontp->filename);

  /* skip comment */
  Fseek (f, (long) one (f), SEEK_CUR);

  (void) four (f);	/* skip design size */
  (void) four (f);	/* skip checksum */
  hppp = sfour (f);
  vppp = sfour (f);
  if (hppp != vppp && (debug & DBG_PK))
    Printf ("Font has non-square aspect ratio %d:%d\n", vppp, hppp);

  /* Initialize glyph array.  */
  for (g = fontp->glyph; g < fontp->glyph + 256; ++g)
    {
      g->packed_data = NULL;
      g->bitmap.bits = NULL;
      g->bitmap2.bits = NULL;
    }

  /* Read glyph directory (really a pass over the whole file).  */
  for (;;)
    {
      int bytes_left, flag_low_bits;
      unsigned int cc;

      PK_skip_specials (f, fontp);
      if (PK_flag_byte == PK_POST)
	break;
      flag_low_bits = PK_flag_byte & 0x7;
      if (flag_low_bits == 7)
	{
	  bytes_left = four (f);
	  cc = four (f);
	}
      else if (flag_low_bits > 3)
	{
	  bytes_left = ((flag_low_bits - 4) << 16) + two (f);
	  cc = one (f);
	}
      else
	{
	  bytes_left = (flag_low_bits << 8) + one (f);
	  cc = one (f);
	}
      /* Use the (randomly chosen) `x2' field for the flag byte.  */
      fontp->glyph[cc].x2 = PK_flag_byte;
      
      /* Read the bitmap data, but don't unpack it.  */
      fontp->glyph[cc].packed_data = xmalloc (bytes_left);
      Fread (fontp->glyph[cc].packed_data, bytes_left, 1, f);
    }
}

/* Unpack the given bitmap.  */

static void
pk_unpack_fn P1C(ubyte, ch)
{
  int i, j;
  ubyte n;
  int row_bit_pos;
  boolean paint_switch;
  BMUNIT *cp;
  long fpwidth;
  BMUNIT word;
  int word_weight, bytes_wide;
  int rows_left, h_bit, count;
  register struct glyph *g = &current_font->glyph[ch];

  /* We randomly chose `x2' to store this above.  */
  PK_flag_byte = g->x2;
  PK_dyn_f = PK_flag_byte >> 4;
  paint_switch = !!(PK_flag_byte & 8);
  PK_flag_byte &= 0x7;
  if (PK_flag_byte == 7)
    n = 4;
  else if (PK_flag_byte > 3)
    n = 2;
  else
    n = 1;

  if (debug & DBG_PK)
    Printf ("loading pk char %d, char type %d ", ch, n);

  /* Set the static variable for other routines to get the data from.  */
  data_string = g->packed_data;
  
  /* Now read rest of character preamble.  */
  if (n != 4)
    fpwidth = data_snum (3);
  else
    {
      fpwidth = data_sfour ();
      (void) data_four ();	/* horizontal escapement */
    }
  (void) data_num (n);		/* vertical escapement */
  {
    unsigned long w, h;

    w = data_num (n);
    h = data_num (n);
    if (w > 0x7fff || h > 0x7fff)
      oops ("Too large character in file %s", current_font->fontname);
    g->bitmap.w = w;
    g->bitmap.h = h;
  }
  g->x = data_snum (n);
  g->y = data_snum (n);

  g->dvi_adv = ((double) current_font->scale * fpwidth) / (1 << 20);

  if (debug & DBG_PK)
    {
      if (g->bitmap.w != 0)
	Printf (", size=%dx%d, dvi_adv=%d", g->bitmap.w, g->bitmap.h,
		g->dvi_adv);
      Putchar ('\n');
    }

  alloc_bitmap (&g->bitmap);
  cp = (BMUNIT *) g->bitmap.bits;

  /* read character data into *cp.  */
  bytes_wide = ROUNDUP (g->bitmap.w, BITS_PER_BMUNIT) * BYTES_PER_BMUNIT;
  PK_bitpos = -1;
  if (PK_dyn_f == 14)		/* get raster by bits */
    {
      bzero (g->bitmap.bits, g->bitmap.h * bytes_wide);
      for (i = 0; i < g->bitmap.h; i++)	/* get all rows */
	{
	  cp = ADD (g->bitmap.bits, i * bytes_wide);
#ifndef	MSBITFIRST
	  row_bit_pos = -1;
#else
	  row_bit_pos = BITS_PER_BMUNIT;
#endif
	  for (j = 0; j < g->bitmap.w; j++)	/* get one row */
	    {
	      if (--PK_bitpos < 0)
		{
		  word = data_one ();
		  PK_bitpos = 7;
		}
#ifndef	MSBITFIRST
	      if (++row_bit_pos >= BITS_PER_BMUNIT)
#else
	      if (--row_bit_pos < 0)
#endif
		{
		  cp++;
#ifndef	MSBITFIRST
		  row_bit_pos = 0;
#else
		  row_bit_pos = BITS_PER_BMUNIT - 1;
#endif
		}
	      if (word & (1 << PK_bitpos))
		*cp |= 1 << row_bit_pos;
	    }
	}
    }
  else
    {
      /* get packed raster */
      rows_left = g->bitmap.h;
      h_bit = g->bitmap.w;
      PK_repeat_count = 0;
      word_weight = BITS_PER_BMUNIT;
      word = 0;
      while (rows_left > 0)
	{
	  count = PK_packed_num ();
	  while (count > 0)
	    {
	      if (count < word_weight && count < h_bit)
		{
#ifndef	MSBITFIRST
		  if (paint_switch)
		    word |= bit_masks[count] <<
		      (BITS_PER_BMUNIT - word_weight);
#endif
		  h_bit -= count;
		  word_weight -= count;
#ifdef	MSBITFIRST
		  if (paint_switch)
		    word |= bit_masks[count] << word_weight;
#endif
		  count = 0;
		}
	      else if (count >= h_bit && h_bit <= word_weight)
		{
		  if (paint_switch)
		    word |= bit_masks[h_bit] <<
#ifndef	MSBITFIRST
		      (BITS_PER_BMUNIT - word_weight);
#else
		      (word_weight - h_bit);
#endif
		  *cp++ = word;
		  /* "output" row(s) */
		  for (i = PK_repeat_count * bytes_wide /
		       BYTES_PER_BMUNIT; i > 0; --i)
		    {
		      *cp = *SUB (cp, bytes_wide);
		      ++cp;
		    }
		  rows_left -= PK_repeat_count + 1;
		  PK_repeat_count = 0;
		  word = 0;
		  word_weight = BITS_PER_BMUNIT;
		  count -= h_bit;
		  h_bit = g->bitmap.w;
		}
	      else
		{
		  if (paint_switch)
#ifndef	MSBITFIRST
		    word |= bit_masks[word_weight] <<
		      (BITS_PER_BMUNIT - word_weight);
#else
		    word |= bit_masks[word_weight];
#endif
		  *cp++ = word;
		  word = 0;
		  count -= word_weight;
		  h_bit -= word_weight;
		  word_weight = BITS_PER_BMUNIT;
		}
	    }
	  paint_switch = 1 - paint_switch;
	}
      if (cp != ((BMUNIT *) (g->bitmap.bits + bytes_wide * g->bitmap.h)))
	oops ("Wrong number of bits stored:  char. %d, font %s", ch,
	      current_font->fontname);
      if (rows_left != 0 || h_bit != g->bitmap.w)
	oops ("Bad pk file (%s), too many bits", current_font->fontname);
    }
  
  /* Now that we've read the packed data, we can release the memory.  */
  free (g->packed_data);
  g->packed_data = NULL;
}

/* Define the global variables for outside callers.  */

read_glyphs_proc_ptr pk_glyphs = pk_glyphs_fn;
unpack_bitmap_proc_ptr pk_unpack = pk_unpack_fn;
