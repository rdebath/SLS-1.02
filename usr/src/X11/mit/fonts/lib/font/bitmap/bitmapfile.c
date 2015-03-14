/* $Header: /home/x_cvs/mit/fonts/lib/font/bitmap/bitmapfile.c,v 1.2 1992/05/23 12:42:20 dawes Exp $ */

#include    "fontfilest.h"
#include    "bitmap.h"

#ifdef TEST
#undef xalloc
#undef xfree
#define xalloc(s)   malloc(s)
#define xfree(s)    free(s)
#endif

FontFilePtr
FontFileOpen (path, mode)
    char *path;
    char *mode;
{
    FontFilePtr file;
    int		l;
    char	*p;

    file = (FontFilePtr) xalloc(sizeof(struct _FontFile));
    if (!file)
	return NULL;
    if (path && path[0]) {
	l = strlen(path);
	if (l >= 2 && path[l - 2] == '.' && path[l - 1] == 'Z') {
	    for (p = mode; *p; p++)
		if (*p == 'w' || *p == 'a' || *p == '+') {
		    xfree(file);
		    return NULL;
		}
	    file->compressed = TRUE;
	    file->file = (FILE *) CompressedFileOpen(path);
	}
	else {
	    file->compressed = FALSE;
	    file->file = fopen(path, mode);
	}
	if (!file->file) {
	    xfree(file);
	    return NULL;
	}
    }
    else {
	file->compressed = FALSE;
	file->file = stdin;
	for (p = mode; *p; p++)
	    if (*p == 'w' || *p == 'a' || *p == '+') {
		file->file = stdout;
		break;
	    }
    }
    return file;
}

int
FontFileClose (file)
    FontFilePtr file;
{
    int ret;

    if (file->compressed)
	ret = CompressedFileClose((CompressedFile *)(file->file));
    else
	ret = fclose(file->file);
    xfree(file);
    return ret;
}

/* 
 * decompress - cat a compressed file
 */

static unsigned char magic_header[] = { "\037\235" };	/* 1F 9D */

/* Defines for third byte of header */
#define BIT_MASK	0x1f
#define BLOCK_MASK	0x80
/* Masks 0x40 and 0x20 are free.  I think 0x20 should mean that there is
   a fourth header byte (for expansion).
*/

#define INIT_BITS 9			/* initial number of bits/code */

#define MAXCODE(n_bits)	((1 << (n_bits)) - 1)

static long getcode();

/*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 */ 
#define FIRST	257	/* first free entry */
#define	CLEAR	256	/* table clear output code */

static int hsize_table[] = {
    5003,	/* 12 bits - 80% occupancy */
    9001,	/* 13 bits - 91% occupancy */
    18013,	/* 14 bits - 91% occupancy */
    35023,	/* 15 bits - 94% occupancy */
    69001	/* 16 bits - 95% occupancy */
};

CompressedFile *
CompressedFileOpen (path)
    char	    *path;
{
    int		    code;
    int		    maxbits;
    int		    hsize;
    FILE	    *f;
    CompressedFile  *file;
    int		    extra;
    char	    *p;

    f = fopen(path, "r");
    if (!f)
	return NULL;
    if ((getc(f) != (magic_header[0] & 0xFF)) ||
	(getc(f) != (magic_header[1] & 0xFF)))
    {
	fclose(f);
	return NULL;
    }
    code = getc (f);
    maxbits = code & BIT_MASK;
    if (maxbits > BITS || maxbits < 12)
    {
	fclose(f);
	return NULL;
    }
    hsize = hsize_table[maxbits - 12];
    extra = (1 << maxbits) * sizeof (unsigned char) +
	    hsize * sizeof (unsigned short);
    file = (CompressedFile *) xalloc (sizeof (CompressedFile) + extra);
    if (!file)
    {
	fclose(f);
	return NULL;
    }
    file->file = f;
    file->maxbits = maxbits;
    file->block_compress = code & BLOCK_MASK;
    file->maxmaxcode = 1 << file->maxbits;
    file->tab_suffix = (unsigned char *) &file[1];
    file->tab_prefix = (unsigned short *) (file->tab_suffix + file->maxmaxcode);
    /*
     * As above, initialize the first 256 entries in the table.
     */
    file->maxcode = MAXCODE(file->n_bits = INIT_BITS);
    for ( code = 255; code >= 0; code-- ) {
	file->tab_prefix[code] = 0;
	file->tab_suffix[code] = (unsigned char) code;
    }
    file->free_ent = ((file->block_compress) ? FIRST : 256 );
    file->clear_flg = 0;
    file->offset = 0;
    file->size = 0;
    file->stackp = file->de_stack;
    file->finchar = file->oldcode = getcode (file);
    if (file->oldcode != -1)
	*file->stackp++ = file->finchar;
    return file;
}

int
CompressedFileClose (file)
    CompressedFile  *file;
{
    FILE    *f;

    f = file->file;
    xfree (file);
    return fclose(f);
}

int _filldcbuf (file)
    CompressedFile  *file;
{
    register unsigned char *stackp;
    register long code, incode;

    if (file->stackp > file->de_stack)
	return *--file->stackp;

    if (file->oldcode == -1)
	return EOF;

    stackp = file->stackp;
    code = getcode (file);
    if (code == -1)
	return EOF;

    if ( (code == CLEAR) && file->block_compress ) {
	for ( code = 255; code >= 0; code-- )
	    file->tab_prefix[code] = 0;
	file->clear_flg = 1;
	file->free_ent = FIRST - 1;
	if ( (code = getcode (file)) == -1 )	/* O, untimely death! */
	    return EOF;
    }
    incode = code;
    /*
     * Special case for KwKwK string.
     */
    if ( code >= file->free_ent ) {
	*stackp++ = file->finchar;
	code = file->oldcode;
    }

    /*
     * Generate output characters in reverse order
     */
    while ( code >= 256 )
    {
	*stackp++ = file->tab_suffix[code];
	code = file->tab_prefix[code];
    }
    file->finchar = file->tab_suffix[code];

    /*
     * Generate the new entry.
     */
    if ( (code=file->free_ent) < file->maxmaxcode ) {
	file->tab_prefix[code] = (unsigned short)file->oldcode;
	file->tab_suffix[code] = file->finchar;
	file->free_ent = code+1;
    } 
    /*
     * Remember previous code.
     */
    file->oldcode = incode;
    file->stackp = stackp;
    return file->finchar;
}

/*****************************************************************
 * TAG( getcode )
 *
 * Read one code from the standard input.  If EOF, return -1.
 * Inputs:
 * 	stdin
 * Outputs:
 * 	code or -1 is returned.
 */

static unsigned char rmask[9] =
    {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};

static long
getcode(file)
    CompressedFile  *file;
{
    register long code;
    register int r_off, bits;
    register unsigned char *bp = file->buf;
    register FILE   *fp;

    if ( file->clear_flg > 0 || file->offset >= file->size ||
	file->free_ent > file->maxcode )
    {
	/*
	 * If the next entry will be too big for the current code
	 * size, then we must increase the size.  This implies reading
	 * a new buffer full, too.
	 */
	if ( file->free_ent > file->maxcode ) {
	    file->n_bits++;
	    if ( file->n_bits == file->maxbits )
		file->maxcode = file->maxmaxcode;	/* won't get any bigger now */
	    else
		file->maxcode = MAXCODE(file->n_bits);
	}
	if ( file->clear_flg > 0) {
    	    file->maxcode = MAXCODE (file->n_bits = INIT_BITS);
	    file->clear_flg = 0;
	}
	bits = file->n_bits;
	fp = file->file;
	while (bits > 0 && (code = getc (fp)) != EOF)
	{
	    *bp++ = code;
	    --bits;
	}
	bp = file->buf;
	if (bits == file->n_bits)
	    return -1;			/* end of file */
	file->size = file->n_bits - bits;
	file->offset = 0;
	/* Round size down to integral number of codes */
	file->size = (file->size << 3) - (file->n_bits - 1);
    }
    r_off = file->offset;
    bits = file->n_bits;
    /*
     * Get to the first byte.
     */
    bp += (r_off >> 3);
    r_off &= 7;
    /* Get first part (low order bits) */
    code = (*bp++ >> r_off);
    bits -= (8 - r_off);
    r_off = 8 - r_off;		/* now, offset into code word */
    /* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
    if ( bits >= 8 ) {
	code |= *bp++ << r_off;
	r_off += 8;
	bits -= 8;
    }
    /* high order bits. */
    code |= (*bp & rmask[bits]) << r_off;
    file->offset += file->n_bits;

    return code;
}

int
CompressedFileRead (buf, nbytes, file)
    char	    *buf;
    unsigned	    nbytes;
    CompressedFile  *file;
{
    int		    c;
    unsigned	    n;

    n = nbytes;
    while (n)
    {
	if ((c = getdcchar (file)) == EOF)
	    break;
	*buf++ = c;
	--n;
    }
    return nbytes - n;
}

int
CompressedFileSkip (file, bytes)
    CompressedFile  *file;
    unsigned	    bytes;
{
    int	    c = 0;

    while (bytes-- && ((c = getdcchar(file)) != EOF))
	    ;
    return c != EOF;
}

#ifdef TEST
main (argc, argv)
    int		    argc;
    char	    **argv;
{
    CompressedFile  *file;
    int		    c;

    file = CompressedFileOpen (argv[1]);
    if (!file) {
	fprintf(stderr, "can't open file\n");
	exit(1);
    }
    while ((c = getdcchar (file)) != -1)
	putchar (c);
    CompressedFileClose (file);
    exit(0);
}
#endif
