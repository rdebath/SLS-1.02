/* pxl-open.c: find font filenames.  */

#include "config.h"

#include "pathsrch.h"
#include "pxl-open.h"

/* Fontname to try if we can't open the real fontname at any size.  */
string alt_font = ALTFONT;

/* Directory lists to search through.  */
string *gf_path_list, *pk_path_list;

/* List of sizes to search for (if the desired size is unavailable).  */
int *last_resort_sizes = NULL;


static string try_name (), try_resolution (), try_size (), try_format ();

/* Initialize path lists, etc.  */

void
init_pxl_open ()
{
  string gf_var, pk_var;
  string size_str;
  unsigned size_count = 0;
  string size_list = expand_default (getenv ("XDVISIZES"), DEFAULT_XDVISIZES);
  
  /* Be sure `size_list' is in writable memory, since we use strtok.  */
  size_list = xstrdup (size_list);

  /* Initialize the list of last-resort sizes.  */
  for (size_str = strtok (size_list, PATH_DELIMITER_STRING); size_str != NULL;
       size_str = strtok (NULL, PATH_DELIMITER_STRING))
    {
      size_count++;
      XRETALLOC (last_resort_sizes, size_count, int);
      last_resort_sizes[size_count - 1] = atoi (size_str);
    }

  /* Add a zero to mark the end of the list.  */
  size_count++;
  XRETALLOC (last_resort_sizes, size_count, int);
  last_resort_sizes[size_count - 1] = 0;

  /* Initialize the directory lists for the font formats.  */
  gf_var = getenv ("GFFONTS") ? "GFFONTS" : "TEXFONTS";
  gf_path_list = initialize_path_list (gf_var, DEFAULT_GF_PATH);
  
  /* Do not reparse the directory path if the list will be the same.  */
  pk_var = getenv ("PKFONTS") ? "PKFONTS" 
           : getenv ("TEXPKS") ? "TEXPKS" : "TEXFONTS";
  pk_path_list = pk_var == gf_var && DEFAULT_GF_PATH == DEFAULT_PK_PATH
                 ? gf_path_list
                 : initialize_path_list (pk_var, DEFAULT_PK_PATH);
}

/* Try to open the font FONT at resolution DPI; if that fails, use
   `alt_font'.  Return the complete filename.  */

string
pxl_open (font, font_ret, dpi, dpi_ret)
    string font;
    string *font_ret;
    int dpi;
    int *dpi_ret;
{
  string name = try_name (font, font_ret, dpi, dpi_ret);
  if (name == NULL)
    name = try_name (alt_font, font_ret, dpi, dpi_ret);
  
  return name;
}


/* Try to open the font name FONT.  We first try the resolution DPI,
   then the sizes in `last_resort_sizes'.  */

static string
try_name (font, font_ret, dpi, dpi_ret)
    string font;
    string *font_ret;
    int dpi;
    int *dpi_ret;
{
  unsigned s;
  string name;
  
  *font_ret = font;
  name = try_resolution (font, dpi, dpi_ret);
  
  /* It would be better to try the last resort sizes that are closest to
     the stated size first.  */
  for (s = 0; name == NULL && last_resort_sizes[s] != 0; s++)
    if (last_resort_sizes[s] != dpi)
      name = try_resolution (font, last_resort_sizes[s], dpi_ret);
  
  return name;
}


/* Try to open the font name FONT at the resolution DPI.  If that fails,
   try the resolutions within RES_TOLERANCE (DPI).  */

static string
try_resolution (font, dpi, dpi_ret)
    string font;
    int dpi;
    int *dpi_ret;
{
  string name;
  int r = dpi;
  int lower_bound = dpi - RES_TOLERANCE (dpi);
  int upper_bound = dpi + RES_TOLERANCE (dpi);
  
  name = try_size (font, r);
  if (name == NULL)
    {
      for (r = lower_bound; name == NULL && r <= upper_bound; r++)
        if (r != dpi)
          name = try_size (font, r);
    }
  
  *dpi_ret = r;
  return name;
}


/* Try to open the font name FONT at the resolution DPI.  Try PK format
   first, then GF.  */

static string
try_size (font, dpi)
    string font;
    int dpi;
{
  string name = try_format (font, dpi, "pk", pk_path_list);
  if (name == NULL)
    name = try_format (font, dpi, "gf", gf_path_list);
  
  return name;
}


/* Try to open the font name FONT at DPI with suffix
   SUFFIX, using the given PATH_LIST.  */

static string
try_format (font, dpi, suffix, path_list)
  string font;
  int dpi;
  string suffix;
  string *path_list;
{
  string name, name_ret;
  /* Assume suffix is always two characters long, and leave room for the
     null that sprintf writes.  */ 
  char extension[1 + MAX_INT_LENGTH + 3];
  sprintf (extension, ".%d%s", dpi, suffix);
  
  name = concat (font, extension);
  name_ret = find_path_filename (name, path_list);
  
  if (name != name_ret)
    free (name);
  
  return name_ret;
}
