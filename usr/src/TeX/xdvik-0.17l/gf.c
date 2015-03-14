/* gf.c: GF font reading routines.  */

#include "config.h"

static void read_packed_bitmap P2H(FILE *, struct glyph *);

#define	PAINT_0		0
#define	PAINT1		64
#define	PAINT2		65
#define	PAINT3		66
#define	BOC		67
#define	BOC1		68
#define	EOC		69
#define	SKIP0		70
#define	SKIP1		71
#define	SKIP2		72
#define	SKIP3		73
#define	NEW_ROW_0	74
#define	NEW_ROW_MAX	238
#define	XXX1		239
#define	XXX2		240
#define	XXX3		241
#define	XXX4		242
#define	YYY		243
#define	NO_OP		244
#define	CHAR_LOC	245
#define	CHAR_LOC0	246
#define	PRE		247
#define	POST		248
#define	POST_POST	249
#define	GF_ID_BYTE	131
#define	TRAILER		223	/* Trailing bytes at end of file */

/* A couple small utilities.  */

static void
expect P2C(FILE *, f, ubyte, ch)
{
  ubyte ch1 = one (f);

  if (ch1 != ch)
    oops ("Bad GF file:  %d expected, %d received.", ch, ch1);
}

static void
too_many_bits (ch)
     ubyte ch;
{
  oops ("Too many bits found when loading character %d", ch);
}

/* Read a GF font from the file F into the structure FONTP.  */

static void
gf_glyphs_fn P2C(FILE *, f,  struct font *, fontp)
{
  int hppp, vppp;
  ubyte ch, cmnd;
  register struct glyph *g;
  long trailer_word = TRAILER << 24 | TRAILER << 16 | TRAILER << 8 | TRAILER;

  if (debug & DBG_PK)
    Printf ("Reading GF pixel file %s\n", fontp->filename);

  /* Find postamble.  */
  Fseek (f, (long) -4, SEEK_END);
  while (four (f) != trailer_word)
    {
      int x = fseek (f, (long) -5, SEEK_CUR);
      if (x < 0)
        {
          fprintf (stderr, "%s: Bad GF file (no trailer).\n", fontp->filename);
          perror (fontp->filename);
          exit (errno);
	}
    }
  Fseek (f, (long) -5, SEEK_CUR);
  for (;;)
    {
      ch = one (f);
      if (ch != TRAILER)
	break;
      Fseek (f, (long) -2, SEEK_CUR);
    }
  if (ch != GF_ID_BYTE)
    oops ("Bad end of font file %s", fontp->fontname);
  Fseek (f, (long) -6, SEEK_CUR);
  expect (f, POST_POST);
  Fseek (f, sfour (f), SEEK_SET);	/* move to postamble */

  /* Read postamble.  */
  expect (f, POST);
  (void) four (f);	/* pointer to last eoc + 1 */
  (void) four (f);	/* skip design size */
  (void) four (f);	/* skip checksum */
  hppp = sfour (f);
  vppp = sfour (f);
  if (hppp != vppp && (debug & DBG_PK))
    Printf ("Font has non-square aspect ratio %d v by %d h.\n", vppp, hppp);
  (void) four (f);	/* skip min_m */
  (void) four (f);	/* skip max_m */
  (void) four (f);	/* skip min_n */
  (void) four (f);	/* skip max_n */

  /* Prepare glyph array. */
  for (g = fontp->glyph; g < fontp->glyph + 256; g++)
    {
      g->packed_data = NULL;
      g->bitmap.bits = NULL;
      g->bitmap2.bits = NULL;
    }

  /* Read glyph directory.  */
  while ((cmnd = one (f)) != POST_POST)
    {
      int addr;
      ubyte ch = one (f);	/* character code */

      g = &fontp->glyph[ch];
      switch (cmnd)
	{
	case CHAR_LOC:
	  (void) four (f); /* skip dx */
	  (void) four (f); /* skip dy */
	  break;
	case CHAR_LOC0:
	  (void) one (f); /* skip dx */
	  break;
	default:
	  oops ("Non-char_loc command (%d) found in GF postamble", cmnd);
	}
      /* Read TFM width.  */
      g->dvi_adv = ((double) fontp->scale * sfour (f)) / (1 << 20);
      
      /* Get pointer to bitmap data and read it.  */
      addr = four (f);
      if (addr != -1)
        {
          long curpos = ftell (f);
          Fseek (f, addr, SEEK_SET);

          if (debug & DBG_PK)
            Printf ("Loading gf char %d", ch);

          read_packed_bitmap (f, g);
          
          Fseek (f, curpos, SEEK_SET);
        }

      if (debug & DBG_PK)
	Printf ("Read GF glyph for character %d; dy = %d, addr = %d\n",
		ch, g->dvi_adv, addr);
    }
}

/* We'd like to read but not unpack the bitmap for the glyph G in the
   file F.  But, unfortunately, we have to unpack it, because GF format
   provides no way to determine the length of a character description
   without interpreting it.  This is acceptable (to me (karl), anyway)
   because essentially all fonts should be stored in PK form.  */

#define	WHITE false
#define	BLACK true

static void
read_packed_bitmap P2C(FILE *, f,  struct glyph *, g)
{
  ubyte cmnd;
  int min_m, max_m, min_n, max_n;
  BMUNIT *cp, *basep, *maxp;
  int bytes_wide;
  boolean paint_switch = WHITE;
  boolean new_row;
  int count;
  int word_weight;

  for (;;)
    {
      switch (cmnd = one (f))
        {
        case XXX1:
        case XXX2:
        case XXX3:
        case XXX4:
          Fseek (f, (long) num (f, cmnd - XXX1 + 1), SEEK_CUR);
          continue;
        case YYY:
          (void) four (f);
          continue;
        case BOC:
          (void) four (f);	/* skip character code */
          (void) four (f);	/* skip pointer to prev char */
          min_m = sfour (f);
          max_m = sfour (f);
          g->x = -min_m;
          min_n = sfour (f);
          g->y = max_n = sfour (f);
          g->bitmap.w = max_m - min_m + 1;
          g->bitmap.h = max_n - min_n + 1;
          break;
        case BOC1:
          (void) one (f);	/* skip character code */
          g->bitmap.w = one (f);	/* max_m - min_m */
          g->x = g->bitmap.w - one (f);	/* ditto - max_m */
          ++g->bitmap.w;
          g->bitmap.h = one (f) + 1;
          g->y = one (f);
          break;
        default:
          oops ("Bad BOC code:  %d", cmnd);
        }
      break;
    }

  if (debug & DBG_PK)
    Printf (", size=%dx%d, dvi_adv=%d\n", g->bitmap.w, g->bitmap.h,
            g->dvi_adv);

  alloc_bitmap (&g->bitmap);
  cp = basep = (BMUNIT *) g->bitmap.bits;

  /* Read character data into *basep.  */
  bytes_wide = ROUNDUP (g->bitmap.w, BITS_PER_BMUNIT) * BYTES_PER_BMUNIT;
  maxp = ADD (basep, g->bitmap.h * bytes_wide);
  bzero (g->bitmap.bits, g->bitmap.h * bytes_wide);
  new_row = false;
  word_weight = BITS_PER_BMUNIT;
  for (;;)
    {
      count = -1;
      cmnd = one (f);
      if (cmnd < 64)
        count = cmnd;
      else if (cmnd >= NEW_ROW_0 && cmnd <= NEW_ROW_MAX)
        {
          count = cmnd - NEW_ROW_0;
          paint_switch = WHITE;	/* it'll be complemented later */
          new_row = true;
        }
      else
        switch (cmnd)
          {
          case PAINT1:
          case PAINT2:
          case PAINT3:
            count = num (f, cmnd - PAINT1 + 1);
            break;
          case EOC:
            if (cp >= ADD (basep, bytes_wide))
              too_many_bits ();
            return;
          case SKIP1:
          case SKIP2:
          case SKIP3:
            *((char **) &basep) +=
              num (f, cmnd - SKIP0) * bytes_wide;
          case SKIP0:
            new_row = true;
            paint_switch = WHITE;
            break;
          case XXX1:
          case XXX2:
          case XXX3:
          case XXX4:
            Fseek (f, (long) num (f, cmnd - XXX1 + 1), SEEK_CUR);
            break;
          case YYY:
            (void) four (f);
            break;
          case NO_OP:
            break;
          default:
            oops ("Bad command in GF file:  %d", cmnd);
          } /* end switch */

      if (new_row)
        {
          *((char **) &basep) += bytes_wide;
          if (basep >= maxp || cp >= basep)
            too_many_bits ();
          cp = basep;
          word_weight = BITS_PER_BMUNIT;
          new_row = false;
        }

      if (count >= 0)
        {
          while (count)
            if (count <= word_weight)
              {
#ifndef	MSBITFIRST
                if (paint_switch)
                  *cp |= bit_masks[count] << (BITS_PER_BMUNIT - word_weight);
#endif
                word_weight -= count;
#ifdef	MSBITFIRST
                if (paint_switch)
                  *cp |= bit_masks[count] << word_weight;
#endif
                break;
              }
            else
              {
                if (paint_switch)
#ifndef	MSBITFIRST
                  *cp |= (bit_masks[word_weight]
                          << (BITS_PER_BMUNIT - word_weight));
#else
                  *cp |= bit_masks[word_weight];
#endif
                cp++;
                count -= word_weight;
                word_weight = BITS_PER_BMUNIT;
              }
          paint_switch = !paint_switch;
        }
    } /* end for */
}


/* Unpack the bitmap for the character CH in `current_font'.  This
   should never be called, because we read and unpack the entire font
   when it is loaded.  See comments above.  */

static void
gf_unpack_fn P1C(ubyte, ch)
{
  oops ("Tried to unpack GF character %d", ch);
}

/* Define the global variables for outside callers.  */

read_glyphs_proc_ptr gf_glyphs = gf_glyphs_fn;
unpack_bitmap_proc_ptr gf_unpack = gf_unpack_fn;
