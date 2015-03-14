/* dvi_init.c: start reading the DVI file. */

#define	GF_PRE		247
#define	GF_ID_BYTE	131
#define	GF_MAGIC	(GF_PRE << 8) + GF_ID_BYTE
#define	PK_PRE		247
#define	PK_ID		89
#define	PK_MAGIC	(PK_PRE << 8) + PK_ID

#include "config.h"

#include <sys/stat.h>
#include "dvi.h"
#include "pxl-open.h"

#if 0
#define	dvi_oops(str) longjmp (dvi_env, (int) str);
#else
#define dvi_oops(str) FATAL (str)
#endif

#define XtOffset(type, field) ((unsigned) &((type) NULL)->field)

static struct stat fstatbuf;	/* mechanism to see if file was */
time_t dvi_time;		/* modified since last usage */

struct font *current_font = NULL;	/* ptr into linked list of fonts */
ubyte maxchar;


/* DVI preamble and postamble information.  */
int current_page;
boolean hush_spec_now;
int total_pages;
double fraction;
int maxstack;
static char job_id[300];
static long numerator, denominator, magnification;
static float document_mag_factor;

/* Table of page offsets in DVI file, indexed by page number - 1.
   Initialized in prepare_pages. */
long *page_offset;

/* Offset in DVI file of last page, set in read_postamble().  */
static long last_page_offset;

static void free_fonts P1H(void);

/* define_font reads the rest of the fntdef command and then reads in
   the specified pixel file, prepending it to the global linked-list
   holding all of the fonts used in the job.  We return true if we could
   find a font, and false if not.  */

static boolean
define_font P1C(ubyte, cmnd)
{
  float fsize, font_mag_factor;
  unsigned len, design;
  int size, magic;
  int dpi, dpi_found;
  string font_found;
  FILE *font_file;
  read_glyphs_proc_ptr read_glyphs;
  register struct font *fontp
    = (struct font *) xmalloc ((unsigned) sizeof (struct font));

  /* Read the rest of the fntdef command.  */
  fontp->TeXnumber = num (dvi_file, (ubyte) cmnd - FNTDEF1 + 1);
  (void) four (dvi_file);	/* ignore checksum */
  fontp->scale = four (dvi_file);
  design = four (dvi_file);
  len = one (dvi_file) + one (dvi_file);
  fontp->fontname = (string) xmalloc (len + 1);
  Fread (fontp->fontname, sizeof (char), len, dvi_file);
  fontp->fontname[len] = '\0';
  if (debug & DBG_PK)
    Printf ("Define font `%s'\t(scale=%d design=%u)\n",
	    fontp->fontname, fontp->scale, design);

  /* `document_mag_factor' was set in `process_preamble'.  */
  font_mag_factor = (float) fontp->scale / design;
  fsize = font_mag_factor * document_mag_factor * pixels_per_inch;
  size = fsize + 0.5;
  fontp->scale = fontp->scale * fraction;

  /* Try to find some pixel file for this font.  */
  fontp->filename = pxl_open (fontp->fontname, &font_found, size, &dpi_found);

  if (fontp->filename == NULL)
    { /* This should never happen, since we should always be able to
	 find `altfont' at some size or another.  */
      Fprintf (stderr, "Can't find font %s.\n", fontp->fontname);
      return false;
    }
  font_file = fopen (fontp->filename, FOPEN_RBIN_MODE);
  if (font_file == NULL)
    { /* This should never happen, either, since it means `pxl_open'
	 as able to find a readable font file, but fopen failed.  */
      perror (fontp->filename);
      return false;
    }

  /* Did we find the wrong font?  */
  if (font_found != fontp->fontname)
    {
      Fprintf (stderr, "Substituting %s.%d dpi for %s.%d.\n",
	       font_found, dpi_found, fontp->fontname, size);
      free (fontp->fontname);
      fontp->fontname = font_found;
    }

  /* Did we find the wrong size?  */
  else if (!(dpi_found > fsize - RES_TOLERANCE (fsize)
	     && dpi_found < fsize + RES_TOLERANCE (fsize)))
    Fprintf (stderr, "Substituting %s.%d dpi for %d dpi.\n",
	     fontp->fontname, dpi_found, size);

  /* We found some file or other.  Let's see what format we've got.  */
  magic = two (font_file);
  if (magic == GF_MAGIC)
    {
      read_glyphs = gf_glyphs;
      fontp->unpack_bitmap = gf_unpack;
    }
  else if (magic == PK_MAGIC)
    {
      read_glyphs = pk_glyphs;
      fontp->unpack_bitmap = pk_unpack;
    }
  else
    oops ("%s: Cannot recognize font format.", fontp->filename);

  /* Read the font into memory, and close the file.  */
  (*read_glyphs) (font_file, fontp);
  fclose (font_file);

  /* Reallocate the font structure to eliminate unused glyphs at the
     end, if possible.  */
  maxchar = 255;
  while (maxchar > 0
         && fontp->glyph[maxchar].packed_data == NULL
         && fontp->glyph[maxchar].bitmap.bits == NULL)
    --maxchar;
  if (maxchar < 255)
    fontp = xrealloc (fontp, XtOffset (struct font *, glyph[maxchar + 1]));
  fontp->maxchar = maxchar;

  /* Put this font at the head of the list.  */
  fontp->next = current_font;
  current_font = fontp;
  if (list_fonts)
    puts (fontp->fontname);

  return true;
}

/* process_preamble reads the information in the preamble and stores
   it into global variables for later use.  */

static void
process_preamble ()
{
  ubyte k;

  if (one (dvi_file) != PRE)
    dvi_oops ("DVI file doesn't start with preamble");
  if (one (dvi_file) != 2)
    dvi_oops ("Wrong version of DVI output for this program");
  numerator = four (dvi_file);
  denominator = four (dvi_file);
  magnification = four (dvi_file);
  document_mag_factor = (float) magnification / 1000;
  fraction = (((double) numerator * magnification)
	      / ((double) denominator * 1000.));
  fraction = fraction * (((long) pixels_per_inch) << 16) / 254000;
  k = one (dvi_file);
  Fread (job_id, sizeof (char), (int) k, dvi_file);
  job_id[k] = '\0';
}

/* find_postamble locates the beginning of the postamble
   leaves the file ready to start reading at that location.  */
#define	TMPSIZ	516		/* 4 trailer bytes + 512 junk bytes allowed */

static void
find_postamble ()
{
  long pos;
  ubyte temp[TMPSIZ];
  ubyte *p;
  ubyte *p1;
  ubyte byte;

  Fseek (dvi_file, (long) 0, SEEK_END);
  pos = ftell (dvi_file) - TMPSIZ;
  if (pos < 0)
    pos = 0;
  Fseek (dvi_file, pos, SEEK_SET);
  p = temp + fread ((char *) temp, sizeof (char), TMPSIZ, dvi_file);
  for (;;)
    {
      p1 = p;
      while (p1 > temp && *(--p1) != TRAILER);
      p = p1;
      while (p > temp && *(--p) == TRAILER);
      if (p <= p1 - 4)
	break;			/* found 4 TRAILER bytes */
      if (p <= temp)
	dvi_oops ("DVI file corrupted");
    }
  pos += p - temp;
  byte = *p;
  while (byte == TRAILER)
    {
      Fseek (dvi_file, --pos, SEEK_SET);
      byte = one (dvi_file);
    }
  if (byte != 2)
    dvi_oops ("Wrong version of DVI output for this program");
  Fseek (dvi_file, pos - 4, SEEK_SET);
  Fseek (dvi_file, sfour (dvi_file), SEEK_SET);
}

/* read_postamble reads the information in the postamble, storing it
   into global variables.  It also takes care of reading in all of the
   pixel files for the fonts used in the job.  */

static void
read_postamble ()
{
  ubyte cmnd;
  boolean font_not_found = false;

  if (one (dvi_file) != POST)
    dvi_oops ("Postamble doesn't begin with POST");
  last_page_offset = four (dvi_file);
  if (numerator != four (dvi_file)
      || denominator != four (dvi_file)
      || magnification != four (dvi_file))
    dvi_oops ("Postamble doesn't match preamble");

  /* read largest box height and width */
  unshrunk_page_h = (spellfour (dvi_file) >> 16) + offset_y;
  if (unshrunk_page_h < unshrunk_paper_h)
    unshrunk_page_h = unshrunk_paper_h;
  unshrunk_page_w = (spellfour (dvi_file) >> 16) + offset_x;
  if (unshrunk_page_w < unshrunk_paper_w)
    unshrunk_page_w = unshrunk_paper_w;

  maxstack = two (dvi_file);
  total_pages = two (dvi_file);

  /* Release the storage for the fonts.  I deliberately do not try to
     reuse fonts, in case some font has changed.  I suppose it would be
     best to keep file mtimes around and check, but that seems like a
     lot of work.  It doesn't take too long to reread the fonts.  */
  free_fonts ();
  
  do
    {
      switch (cmnd = one (dvi_file))
	{
	case FNTDEF1:
	case FNTDEF2:
	case FNTDEF3:
	case FNTDEF4:
	  font_not_found = font_not_found | !define_font (cmnd);
	  break;
	case POSTPOST:
	  break;
	default:
	  dvi_oops ("Non-fntdef command found in postamble");
	}
    }
  while (cmnd != POSTPOST);

  /* We hope this will never happen.  */
  if (font_not_found)
    dvi_oops ("Not all pixel files were found");
}

static void
prepare_pages ()
{
  int i;

  stack = (struct frame *)
    xmalloc ((unsigned) sizeof (struct frame) * (maxstack + 1));
  page_offset = (long *) xmalloc ((unsigned) total_pages * sizeof (long));
  i = total_pages;
  page_offset[--i] = last_page_offset;
  Fseek (dvi_file, last_page_offset, SEEK_SET);
  /*
   * Follow back pointers through pages in the DVI file,
   * storing the offsets in the page_offset table.
   */
  while (i > 0)
    {
      Fseek (dvi_file, (long) (1 + 4 + (9 * 4)), SEEK_CUR);
      Fseek (dvi_file, page_offset[--i] = four (dvi_file), SEEK_SET);
    }
}


void
init_page ()
{
  page_w = ROUNDUP (unshrunk_page_w, mane.shrinkfactor) + 2;
  page_h = ROUNDUP (unshrunk_page_h, mane.shrinkfactor) + 2;
}

/* init_dvi_file is the main subroutine for reading the startup information
   from the dvi file.  */

static void
init_dvi_file ()
{
  (void) fstat (fileno (dvi_file), &fstatbuf);
  dvi_time = fstatbuf.st_mtime;
  process_preamble ();
  find_postamble ();
  read_postamble ();
  prepare_pages ();
  init_page ();
  if (current_page >= total_pages)
    current_page = total_pages - 1;
  hush_spec_now = hush_spec;
}


/* open_dvi_file opens the dvi file in `dvi_name' as `dvi_file' and
   calls init_dvi_file() to initialize it.  If the extension is not
   already `.dvi' try adding `.dvi'.  If that fails, then try `dvi_name'
   as it stands.  */

void
open_dvi_file ()
{
  string extension = strrchr (dvi_name, '.');

  /* Should we add .dvi?  */
  if (extension == NULL || !STREQ (extension, ".dvi"))
    {				/* Yes.  */
      string name = concat (dvi_name, ".dvi");
      dvi_file = fopen (name, FOPEN_RBIN_MODE);
      if (dvi_file != NULL)
	dvi_name = name;
      else
	free (name);
    }

  /* If we didn't open it with .dvi added, try without.  */
  if (dvi_file == NULL)
    {
      dvi_file = fopen (dvi_name, FOPEN_RBIN_MODE);
      if (dvi_file == NULL)
	{
	  perror (dvi_name);
	  exit (1);
	}
    }

  /* We've opened something, so let's start things off.  */
  init_dvi_file ();
}


/* Check for changes in dvi file. */

boolean
check_dvi_file ()
{
  if (dvi_file == NULL || fstat (fileno (dvi_file), &fstatbuf) != 0
      || fstatbuf.st_mtime != dvi_time)
    {
      if (dvi_file)
	Fclose (dvi_file);
      free ((char *) stack);
      free ((char *) page_offset);
      dvi_file = fopen (dvi_name, FOPEN_RBIN_MODE);
      if (dvi_file == NULL)
	dvi_oops ("Cannot reopen dvi file.");
      if (list_fonts)
	Putchar ('\n');
      init_dvi_file ();
      redraw_page ();
      return false;
    }
  return true;
}

/* Remove all storage allocated for the fonts.  */

static void
free_fonts ()
{
  struct font *f;
  
  free_shrunken_fonts ();
  
  for (f = current_font; f != NULL; f = f->next)
    {
      struct glyph *g;
      
      for (g = f->glyph; g <= f->glyph + f->maxchar; g++)
        {
          if (g->bitmap.bits)
            free (g->bitmap.bits);
          
          if (g->packed_data)
            free (g->packed_data);
        }
      
      free (f);
    }
  
  current_font = NULL;
}


/* Release all shrunken bitmaps for all fonts.  We need this separately
   when the shrink factor changes.  */

void
free_shrunken_fonts ()
{
  register struct font *f;
  register struct glyph *g;

  for (f = current_font; f != NULL; f = f->next)
    for (g = f->glyph; g <= f->glyph + f->maxchar; ++g)
      if (g->bitmap2.bits)
	{
	  free (g->bitmap2.bits);
	  g->bitmap2.bits = NULL;
	}
}
