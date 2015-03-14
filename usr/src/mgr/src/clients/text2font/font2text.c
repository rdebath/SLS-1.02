/* File font2text.c
 * Aronsson was here
 * 13 February 1991
 *
 * This file contains no Bellcore-copyrighted code,
 * but is sensitive to changes in the font file format.
 *
 * Convert an MGR font file to text
 */

#include <stdlib.h>			/* malloc */
#include <stdio.h>

#define ONE '*'
#define ZERO '.'			/* Must *not* be space */

/* The fullnames must *not* contain any ONEs or ZEROs! */
static char *fullname[] =		/* ISO 8859-1 names */
{
  "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT",
  "LF", "VT", "FF", "CR", "SO", "SI", "DLE", "DC1", "DC2", "DC3",
  "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC, florin",
  "FS, dagger", "GS, daggerdbl", "RS, perthousand", "US, trademark",

  "space", "exclam", "quotedbl", "numbersign", "dollar", "percent",
  "ampersand", "quoteright", "parenleft", "parenright", "asterisk",
  "plus", "comma", "hyphen", "period", "slash", "zero", "one", "two",
  "three", "four", "five", "six", "seven", "eight", "nine", "colon",
  "semicolon", "less", "equal", "greater", "question",

  "at", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
  "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y",
  "Z", "bracketleft", "backslash", "bracketright", "asciicircum",
  "underscore",

  "quoteleft", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
  "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
  "y", "z", "braceleft", "bar", "braceright", "asciitilde", "",

  "grave", "circumflex", "caron", "breve", "hungarumlaut", "cedilla",
  "ogonek", "tilde", "dotaccent", "dotlessi", "Lslash", "Scaron",
  "Zcaron", "OE", "fl", "fraction", "quotesingle", "quotedblleft",
  "quotedblbase", "quotedblright", "guilsinglleft", "guilsinglright",
  "endash", "emdash", "periodcentered", "ellipsis", "lslash",
  "scaron", "zcaron", "oe", "fi", "Ydieresis",

  "", "exclamdown", "cent", "sterling", "currency", "yen",
  "brokenbar", "section", "dieresis", "copyright", "ordfeminine",
  "guillemotleft", "logicalnot", "", "registered", "macron", "ring",
  "plusminus", "twosuperior", "threesuperior", "acute", "mu",
  "paragraph", "bullet", "quotesinglbase", "onesuperior",
  "ordmasculine", "guillemotright", "onequarter", "onehalf",
  "threequarters", "questiondown",

  "Agrave", "Aacute", "Acircumflex", "Atilde", "Adieresis", "Aring",
  "AE", "Ccedilla", "Egrave", "Eacute", "Ecircumflex", "Edieresis",
  "Igrave", "Iacute", "Icircumflex", "Idieresis", "Eth", "Ntilde",
  "Ograve", "Oacute", "Ocircumflex", "Otilde", "Odieresis",
  "multiply", "Oslash", "Ugrave", "Uacute", "Ucircumflex",
  "Udieresis", "Yacute", "Thorn", "germandbls",

  "agrave", "aacute", "acircumflex", "atilde", "adieresis", "aring",
  "ae", "ccedilla", "egrave", "eacute", "ecircumflex", "edieresis",
  "igrave", "iacute", "icircumflex", "idieresis", "eth", "ntilde",
  "ograve", "oacute", "ocircumflex", "otilde", "odieresis", "minus",
  "oslash", "ugrave", "uacute", "ucircumflex", "udieresis", "yacute",
  "thorn", "ydieresis"
};


int main(int argc, char *argv[])
{
  int f_type, f_wide, f_high, f_base, f_count, f_start;
  unsigned int nbits;			/* # of bits total */
  int c;
  int bpl;				/* # of bits per scan line */
  char *bigbuf;
  char *cp;
  int bit;
  int i;
  int line;
  
  /* Input head */

  f_type = getc (stdin);
  f_wide = getc (stdin);
  f_high = getc (stdin);
  f_base = getc (stdin);
  f_count = getc (stdin);
  f_start = getc (stdin);

  /* Any complaints? */

  if (f_type != 24 || f_start == EOF || f_count + f_start - 1 > 255)
    {
      fprintf (stderr,"%s: sorry, Input is not a font\n", argv[0]);
      return 3;
    }
  
  bpl = ((f_wide*f_count)+31)&~31; /* fonts always 32 bit padded */
  nbits = bpl * f_high;
  
  cp = bigbuf = malloc (nbits);
  if (! cp)
    {
      fprintf (stderr, "Sorry, cannot malloc\n");
      return 19;
    }
  
  /* Output head */

  printf ("%d %d %d\n", f_type, f_wide, f_high);
  printf ("%d %d %d\n\n", f_base, f_count, f_start);
  
  printf ("Magic number %3d\n", f_type);
  printf ("Width        %3d pixels\n", f_wide);
  printf ("Height       %3d pixels\n", f_high);
  printf ("Baseline at  %3d\n", f_base);
  printf ("Total of     %3d characters\n", f_count);
  printf ("Starting at  %3d %3.3o 0x%2.2x %s\n\n",
	  f_start, f_start, f_start, fullname[f_start]);
  
  /* Input body */

  for (i = 0; i < nbits/8; i++)
    {
      c = getchar();
      for (bit=7; bit>=0; bit--)
	*(cp++) = c & (1<<bit) ? ONE : ZERO;
    }

  /* Output body */

  for (i = 0; i < f_count; i++)		/* chars */
    {
      printf ("\t\t%3d %3.3o 0x%2.2x %s\n",
	      i+f_start, i+f_start, i+f_start, fullname[i+f_start]);
      for (line = 0; line < f_high; line++) /* scan lines */
	{
	  for (bit = 0; bit < f_wide; bit++) /* pixels */
	    {
	      putchar (*(bigbuf+i*f_wide+line*bpl+bit));
	    }
	  putchar ('\n');
	}
    }
  return 0;
}

