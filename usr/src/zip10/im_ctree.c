/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  im_ctree.c by Richard B. Wales.
 *
 *  Includes modifications by Jean-Loup Gailly.
 *
 *  PURPOSE
 *
 *      Encode various sets of source values using variable-length
 *      binary code trees.
 *
 *  DISCUSSION
 *
 *      The PKZIP "implosion" process uses several variable-depth binary
 *      code trees, similar to Huffman trees.  The more common source
 *      values are represented by shorter bit sequences.
 *
 *      Each code tree is stored in the ZIP file in a compressed form
 *      that is essentially a run-length-encoded list of the lengths of
 *      all the code strings (in ascending order by source values).
 *      The actual code strings are reconstructed from the lengths in
 *      the UNZIP process, as described in the "application note"
 *      (APPNOTE.TXT) distributed as part of PKWARE's PKZIP program.
 *
 *      Because of the way a code tree is stored in the ZIP file, the
 *      codes must conform to the following restrictions:
 *
 *      (1) No code string may ever exceed 16 bits in length.
 *
 *      (2) If all code strings are extended to 16 bits by padding on
 *          the right (low-order end) with zeros, and then treated as
 *          unsigned 16-bit integers, then:
 *
 *          (a) The arithmetically higher 16-bit values must correspond
 *              to the shorter code strings.
 *
 *          (b) If two source values map into code strings of the same
 *              length, the higher code string must correspond to the
 *              lower source value (where source values are treated as
 *              unsigned).
 *
 *      Further, shortcuts taken by PKUNZIP 1.10 in the way it decodes
 *      the compressed data via the trees impose the following extra
 *      limitations:
 *
 *      (3) No code string in the "distance" tree can be longer than 8
 *          bits.
 *
 *      (4) The maximum length of any code string is limited by the
 *          number of initial zero bits, as follows:
 *
 *          (a) 0-3 leading zeros:  maximum length is 8.
 *
 *          (b) 4-5 leading zeros:  maximum length is 12.
 *
 *          (c) 6-7 leading zeros:  maximum length is 14.
 *
 *      (5) In the "literal" tree, the code string corresponding to the
 *          source value 255 must be at least 10 bits in length, regard-
 *          less of the frequency of the value 255 in the file.
 *
 *      PKWARE calls the code trees "Shannon-Fano trees".  (Shannon-Fano
 *      coding was a predecessor to the better-known Huffman coding
 *      technique; see the references below.)  Although it appears that
 *      the Shannon-Fano (top-down partitioning) algorithm is in fact
 *      used by PKZIP in the process of creating the code trees, the
 *      resulting trees are not in fact "pure" Shannon-Fano, because of
 *      the extra processing required in order to meet the restrictions
 *      described above.
 *
 *  REFERENCES
 *
 *      Lynch, Thomas J.
 *          Data Compression:  Techniques and Applications, pp. 53-55.
 *          Lifetime Learning Publications, 1985.  ISBN 0-534-03418-7.
 *
 *      Storer, James A.
 *          Data Compression:  Methods and Theory, pp. 49-50.
 *          Computer Science Press, 1988.  ISBN 0-7167-8156-5.
 *
 *  INTERFACE
 *
 *      ImpErr ct_init (void)
 *          Initialize the code tree routines.  This routine must be
 *          called before any other code tree routines may be called.
 *
 *      ImpErr ct_tally (MATCH *ma)
 *          Tally a "string match" data set.  The tally results will be
 *          used to determine how large the imploded result will be.
 *
 *      ImpErr ct_mktrees (FDATA *fd)
 *          Construct all code trees, and then determine how big the
 *          imploded file will be under both "literal tree" and "no
 *          literal tree" conditions.  Choose the best option.
 *
 *      ImpErr ct_wrtrees (FILE *outfp)
 *          Output the code trees to the ZIP file.
 *
 *      ImpErr ct_wrdata (FILE *outfp)
 *          Output the file data to the ZIP file.
 *
 *      ImpErr ct_windup (void)
 *          Deallocate all code trees.
 */


#ifdef DEBUG
#   define VALIDATE
#endif /* DEBUG */

#include "implode.h"


/***********************************************************************
 *
 * Local data used by the "code tree" routines.
 */


/* Data structure describing a single value and its code string. */
typedef
struct  ct_data
    {   UL_INT  ct_freq;                /* frequency count */
        US_INT  ct_code;                /* bit string */
        U_CHAR  ct_len;                 /* length of bit string */
        U_CHAR  ct_val;                 /* source value */
    }
    TRDATA;


/*
 * Data structure for re-sorting source values by bit string length;
 * used during tree generation.
 */
typedef
struct  ct_resort
    {   U_CHAR  ct_rlen;                /* length of bit string */
        U_CHAR  ct_rval;                /* source value */
#ifdef VMS
        US_INT  dummy;                  /* because of bug in qsort() */
#endif /* VMS */
    }
    RESORT;


/* Header for a code tree. */
typedef
struct  ct_desc
    {   TRDATA *ct_array;               /* array of TRDATA */
        int     ct_size;                /* # of entries in tree */
    }
    TRDESC;


/*
 * Currently active code trees.
 * We allow for five trees (one literal tree, two length trees, and
 * two distance trees) so that we can evaluate the total compression
 * with or without the use of the literal tree.  Remember that the
 * minimum matching distance depends on whether or not a literal tree
 * is used; hence, the "length" and "distance" trees will differ.
 */
#define MAXTREES        5               /* max # of trees at once */
local   TRDESC  ct_table[MAXTREES];


/* Macro to validate a code tree handle. */
#define VALID_HANDLE(x) \
        ((x) >= 0 && (x) < MAXTREES && ct_table[x].ct_array != NULL)


/*
 * Assorted frequency counts.
 * The "Minimum Match Length" (MML) is 2 if a "literal character" code
 * tree is not used, or 3 if a "literal character" tree is used.  Thus,
 * we need to keep two sets of "length" and "distance" frequency counts.
 * Also, 2-character matches need to be counted separately; depending
 * on whether a "literal character" tree is used or not, these will be
 * processed either as literal characters or as distance/length pairs.
 */

/* Total number of source values from Pass One. */
local   long    ct_litc_num;            /* total # literal chars */
local   long    ct_lit2_num;            /* total # of 2-char matches */
local   long    ct_strg_num;            /* total # of string matches */

/* Source value frequencies for the five trees. */
local   long    ct_litc_freq[256];      /* literal character freqs */
local   long    ct_len2_freq[64];       /* length freqs (MML=2) */
local   long    ct_len3_freq[64];       /* length freqs (MML=3) */
local   long    ct_dst2_freq[64];       /* distance freqs (MML=2) */
local   long    ct_dst3_freq[64];       /* distance freqs (MML=3) */

/* Number of bits saved by using each of the trees. */
local   long    ct_litc_saved;          /* literal tree */
local   long    ct_len2_saved;          /* length tree (MML=2) */
local   long    ct_len3_saved;          /* length tree (MML=3) */
local   long    ct_dst2_saved;          /* distance tree (MML=2) */
local   long    ct_dst3_saved;          /* distance tree (MML=3) */

/* Handles for the code trees to be used. */
local   int     ct_litc_tree;           /* temp literal tree */
local   int     ct_len2_tree;           /* temp length tree (MML=2) */
local   int     ct_len3_tree;           /* temp length tree (MML=3) */
local   int     ct_dst2_tree;           /* temp distance tree (MML=2) */
local   int     ct_dst3_tree;           /* temp distance tree (MML=3) */
local   int     lit_tree;               /* literal tree (-1 if none) */
local   int     len_tree;               /* length tree */
local   int     dst_tree;               /* distance tree */


/***********************************************************************
 *
 * Local (static) routines in this file.
 */

local ImpErr ct_alloc
        OF ((int size, int *handle));

local ImpErr ct_free
        OF ((int handle));

local ImpErr ct_loadf
        OF ((int handle, long *freq));

local ImpErr ct_ziprep
        OF ((int handle, U_CHAR **result));

local ImpErr ct_gencodes
        OF ((int handle, int minbits, int maxbits, long *saved));

local ImpErr ct_split
        OF ((TRDATA *part, int size, long freq,
             int prefix, int preflen, int minbits, int maxbits));

local int ct_fsort
        OF ((TRDATA *tr1, TRDATA *tr2));

local int ct_rsort
        OF ((RESORT *cr1, RESORT *cr2));


/***********************************************************************
 *
 * Allocate a code tree.
 */

local
ImpErr
ct_alloc (size, handle)
    int size;
    int *handle;
{   register TRDATA *ct;
    int n;

#ifdef VALIDATE
    /* Validate the arguments. */
    if (size < 2 || size > 256) goto badarg;
    if (handle == NULL)         goto badarg;
#endif /* VALIDATE */

    /* Allocate an available code tree handle. */
    for (n = 0;
         n < MAXTREES && ct_table[n].ct_array != NULL;
         n++) ;
    if (n >= MAXTREES) return IM_NOCTBLS;
    *handle = n;
    ct_table[n].ct_size  = size;

    /* Allocate space for the code tree. */
    ct = (TRDATA *) malloc ((unsigned) (size * sizeof (TRDATA)));
    if (ct == NULL) return IM_NOMEM;
    ct_table[n].ct_array = ct;

    /* Initialize the code tree. */
    for (n = 0; n < size; n++, ct++)
    {   ct->ct_freq = 0;
        ct->ct_code = 0;
        ct->ct_val  = (U_CHAR)n;
        ct->ct_len  = 0;
    }

    /* That's all. */
    return IM_OK;

#ifdef VALIDATE
badarg:
    fprintf (stderr, "\nError in ct_alloc: bad argument(s)");
    return IM_BADARG;
#endif /* VALIDATE */
}


/***********************************************************************
 *
 * Free a code tree.
 */

local
ImpErr
ct_free (handle)
    int handle;
{
#ifdef VALIDATE
    /* Validate the argument. */
    if (!VALID_HANDLE (handle)) goto badarg;
#endif /* VALIDATE */

    /* Free the code tree. */
    free ((char *) ct_table[handle].ct_array);
    ct_table[handle].ct_array = NULL;
    ct_table[handle].ct_size  = 0;

    /* That's all. */
    return IM_OK;

#ifdef VALIDATE
badarg:
    fprintf (stderr, "\nError in ct_free: bad argument(s)");
    return IM_BADARG;
#endif /* VALIDATE */
}


/***********************************************************************
 *
 * Update a code tree with frequency data.
 *
 * In order to allow for the later addition of "adaptive" coding,
 * the new frequencies are added to the existing data.
 */

local
ImpErr
ct_loadf (handle, freq)
    int   handle;
    long *freq;
{   register long *f;
    register TRDATA *ct;
    int n;

#ifdef VALIDATE
    /* Validate the arguments. */
    if (!VALID_HANDLE (handle)) goto badarg;
#endif /* VALIDATE */

    /* Add in the frequencies. */
    for (f = freq,
             ct = ct_table[handle].ct_array,
             n = ct_table[handle].ct_size;
         n > 0;
         f++, ct++, n--)
        ct->ct_freq += *f;

    /* That's all. */
    return IM_OK;

#ifdef VALIDATE
badarg:
    fprintf (stderr, "\nError in ct_loadf: bad argument(s)");
    return IM_BADARG;
#endif /* VALIDATE */
}


/***********************************************************************
 *
 * Generate the ZIP-file representation for a code tree.
 *
 * The returned "result" value points to static data which will be
 * overwritten on the next call to "ct_ziprep".
 *
 * The length of the result string is implicit in the first byte of
 * the value, as specified in the PKZIP applications note.
 */

#ifdef IMPDEBUG
char *treename;
#endif /* IMPDEBUG */

local
ImpErr
ct_ziprep (handle, result)
    int      handle;
    U_CHAR **result;
{   static U_CHAR buffer[257];          /* result info */
    register U_CHAR *c;
    register TRDATA *ct;
    int s, n, l;

#ifdef VALIDATE
    /* Validate the arguments. */
    if (!VALID_HANDLE (handle)) goto badarg;
    if (result == NULL)         goto badarg;
#endif /* VALIDATE */

#ifdef  IMPDEBUG
    if (treename != NULL && treename[0] != 0)
    {   /* Print the code tree info. */
        fprintf (stderr, "\n%s tree:\n  value      len   string\n",
                 treename);
        for (ct = ct_table[handle].ct_array,
                  s = ct_table[handle].ct_size,
                  n = 0;
             s > 0;
             ct++, n++, s--)
            fprintf (stderr, "  %3d (%02x)    %2d    %04x (rev %04x)\n",
                     n, n, ct->ct_len,
                     bi_reverse(ct->ct_code << (16 - ct->ct_len),
                                ct->ct_len) << (16 - ct->ct_len),
                     ct->ct_code);
    }
#endif  /* IMPDEBUG */

    /* Generate the returned value. */
    for (c = buffer+1,
             ct = ct_table[handle].ct_array,
             s = ct_table[handle].ct_size,
             n = 0,
             l = ct->ct_len;
         s > 0;
         ct++, s--)
    {   if (l < 1 || l > 16)
        {   fprintf (stderr, "\nError in ct_ziprep: bad code length");
            return IM_LOGICERR;
        }
        if (n >= 16 || (int)ct->ct_len != l)
        {   *c++ = (U_CHAR)((((n-1) << 4) & 0xf0) | ((l-1) & 0x0f));
            n = 1; l = ct->ct_len;
        }
        else n++;
    }
    if (n > 0)
        *c++ = (U_CHAR)((((n-1) << 4) & 0xf0) | ((l-1) & 0x0f));
    buffer[0] = (U_CHAR)((c - buffer) - 2);

    /* That's all. */
    *result = buffer;
    return IM_OK;

#ifdef VALIDATE
badarg:
    fprintf (stderr, "\nError in ct_ziprep: bad argument(s)");
    return IM_BADARG;
#endif /* VALIDATE */
}


/***********************************************************************
 *
 * Look up the code string for a given value.
 */

#define ct_lookup(handle, value, string, length) \
{   register TRDATA *ct; \
    ct = ct_table[handle].ct_array + (value); \
    string = ct->ct_code; \
    length = ct->ct_len; \
}


/***********************************************************************
 *
 * Generate the codes for a code tree.  If old codes already exist for
 * the tree, they are discarded, and a new set of codes is generated
 * from scratch.
 * IN assertion: ma_buf is already allocated and can be overwritten.
 */

local
ImpErr
ct_gencodes (handle, minbits, maxbits, saved)
    int   handle;                       /* which tree */
    int   minbits;                      /* min code string bit length */
    int   maxbits;                      /* max code string bit length */
    long *saved;                        /* how many bits saved */
{   register TRDATA *ct;
    TRDATA *ct2;
    register int n;
    UL_INT f;
    register RESORT *cr;
    int code, srclen;
    long totalfreq, totalbits;
    ImpErr retcode;
    RESORT rbuf[256];
    int size; /* alias for ct_table[handle].ct_size */
    int z;    /* index of zero frequency element */
    int nz;   /* index of non zero frequency element */

#ifdef VALIDATE
    /* Validate the arguments. */
    if (!VALID_HANDLE (handle)) goto badarg;
    if (minbits < 1)            goto badarg;
    if (maxbits > 16)           goto badarg;
    if (maxbits < minbits)      goto badarg;
    if (saved == NULL)          goto badarg;
#endif /* VALIDATE */
    size = ct_table[handle].ct_size;

    /*
     * Start by sorting the data by frequency.  The source values with
     * higher frequency need to get the shorter Shannon-Fano codes.
     * First exclude the elements with zero frequency, to speed up the sort.
     * This optimization is very important for small files, otherwise the sort
     * takes most of the implode time.
     *    Also determine the total of all frequencies.  This will be needed in
     * order to partition the source values in the Shannon-Fano bit
     * string computation. Finally clear the "code" (bit string) and "len"
     * (bit string length) fields in the code tree array.
     */
    totalfreq = 0;
    ct = ct_table[handle].ct_array;
    /* Copy ct into a tempo */
    ct2 = (TRDATA*) ma_buf;
    memcpy((char*)ct2, (char*)ct, size * sizeof(TRDATA));
    for (nz = 0, z = n = size-1; n >= 0; n--) {
        int m;
        if (ct2[n].ct_freq != 0L) {
            m = nz++;
            totalfreq += (ct[m].ct_freq = ct2[n].ct_freq);
        } else {
            m = z--;
            ct[m].ct_freq = 0L;
        }
        ct[m].ct_code = 0;
        ct[m].ct_len = 0;
        ct[m].ct_val = (U_CHAR)n; /* ct2[n].ct_val */
    }
    qsort ((char *) (ct_table[handle].ct_array), nz,
           sizeof (TRDATA), (int (*)())ct_fsort);

    /*
     * Generate the bit strings via a Shannon-Fano (top-down) algorithm.
     */
    retcode =
        ct_split (ct_table[handle].ct_array,    /* partition start */
                  size,                         /* partition size */
                  totalfreq,                    /* total frequency */
                  0,                            /* code string prefix */
                  0,                            /* # bits in prefix */
                  minbits,                      /* minimum tree depth */
                  maxbits);                     /* maximum tree depth */
    if (retcode != IM_OK) return retcode;

    /*
     * The source value 255 needs to be assigned a bit string with a
     * length of at least 10, in order to accommodate shortcuts in
     * PKUNZIP's decoding algorithm.  If no bit string in the tree is
     * of length 10, we assign 255 to the longest string and hope for
     * the best.
     */
    n = size;
    if (n == 256)
    {   for (ct = ct_table[handle].ct_array;
             n > 0 && ct->ct_val != 255;
             n--, ct++) ;
        if (n == 0)
        {   fprintf (stderr, "\nError in ct_gencodes: no value 255");
            return IM_LOGICERR;
        }
        if (ct->ct_len < 10)
        {   ct2 = ct;
            while (n > 0 && ct->ct_len < 10) n--, ct++;
            if (n == 0) ct--;   /* no len>=10 in tree; use longest */
            n = ct->ct_val;
                ct->ct_val = ct2->ct_val;
                ct2->ct_val = (U_CHAR)n;
            f = ct->ct_freq;
                ct->ct_freq = ct2->ct_freq;
                ct2->ct_freq = f;
    }   }

    /*
     * The source values need to be re-sorted so that all source values
     * with code strings of the same length will be in ascending order.
     * This is because of the compression scheme used to represent the
     * tree in the ZIP file.
     */
    for (n = size,
             ct = ct_table[handle].ct_array,
             cr = rbuf;
         n > 0;
         n--, ct++, cr++)
    {   cr->ct_rlen = ct->ct_len;
        cr->ct_rval = ct->ct_val;
    }
    n = size;
    qsort ((char *) rbuf, n, sizeof (RESORT), (int (*)())ct_rsort);
    for (ct = ct_table[handle].ct_array,
             cr = rbuf;
         n > 0;
         n--, ct++, cr++)
        ct->ct_val = cr->ct_rval;

#ifdef DUMP_TREE
    printf ("Finished tree:\n");
    for (n = size,
             ct = ct_table[handle].ct_array;
         n > 0;
         n--, ct++)
        printf ("  %3d (0x%02x)  l %2d  c 0x%04x f %ld\n",
                ct->ct_val, ct->ct_val, ct->ct_len, ct->ct_code, ct->ct_freq);
    putchar ('\n');
#endif /* DUMP_TREE */

    /*
     * Finally, sort the tree back in ascending order by source value
     * -- which is the order expected by other portions of the program.
     */
    ct = ct_table[handle].ct_array;
    /* Copy ct into a tempo */
    ct2 = (TRDATA*)ma_buf;
    memcpy((char*)ct2, (char*)ct, size * sizeof(TRDATA));
    for (n = size-1; n >= 0; n--) {
        U_CHAR v = ct2[n].ct_val;
        ct[v].ct_freq = ct2[n].ct_freq;
        ct[v].ct_code = ct2[n].ct_code;
        ct[v].ct_len = ct2[n].ct_len;
        ct[v].ct_val = v;
    }

    /*
     * Determine how many bits will be saved if all the source data is
     * encoded using this new set of code strings, as opposed to being
     * represented directly in unencoded form.
     */
    /* # of bits needed for unencoded source values */
    n = ct_table[handle].ct_size;
    for (code = 1, srclen = 0; code < n; code <<= 1, srclen++) ;
    /* # of bits used if all source values encoded via this tree */
    for (ct = ct_table[handle].ct_array,
             totalbits = 0;
         n > 0;
         n--, ct++)
        totalbits += ct->ct_freq * ct->ct_len;
    /* # bits saved by using the tree */
    *saved = (totalfreq * srclen) - totalbits;

    /* That's all. */
    return IM_OK;

#ifdef VALIDATE
badarg:
    fprintf (stderr, "\nError in ct_gencodes: bad argument(s)");
    return IM_BADARG;
#endif /* VALIDATE */
}


/***********************************************************************
 *
 * Split a portion of a code tree into two pieces of approximately equal
 * total frequency.  This routine calls itself recursively in order to
 * generate the bit strings for the entire code tree.
 */

local
ImpErr
ct_split (part, size, freq, prefix, preflen, minbits, maxbits)
    TRDATA *part;               /* start of partition */
    int     size;               /* # elements in partition */
    long    freq;               /* sum of frequencies in partition */
    int     prefix;             /* initial code bits for partition */
    int     preflen;            /* # bits in prefix */
    int     minbits;            /* minimum permissible bit length */
    int     maxbits;            /* maximum permissible bit length */
{   register TRDATA *ct;
    int topmaxbits, botmaxbits, localminbits;
    U_INT topmaxvals, botmaxvals;
    int topsize, botsize;
    long topfreq, botfreq, halffreq, onefreq;
    int n, m, leadzeros;
    int maxshort, minlong;
    ImpErr retcode;
    static maxarray[17] =
        { 8,8,8,8,12,12,14,14,16,16,16,16,16,16,16,16,16 };

#ifdef VALIDATE
    if (part == NULL)           goto badarg;
    if (size < 1)               goto badarg;
    if (freq < 0)               goto badarg;
    if (preflen < 0)            goto badarg;
    if (preflen > maxbits)      goto badarg;
    if (minbits < preflen)      goto badarg;
    if (maxbits > 16)           goto badarg;
    if (maxbits < minbits)      goto badarg;
    /*
    putc ('\n', stderr);
    for (n = preflen; n > 0; n--) fprintf (stderr, "   ");
    fprintf (stderr, "ct_split (sz=%d, pr=%04x[%d], min=%d, max=%d)",
             size, prefix, preflen, minbits, maxbits);
    */
#endif /* VALIDATE */

    /*
     * If there's only one element in this partition, we simply take
     * the "prefix" value as the code string for the single element.
     * We reverse the bits of the prefix for more efficient output.
     */
    if (size == 1)
    {   part->ct_code = bi_reverse(prefix, preflen);
        part->ct_len  = (U_CHAR)preflen;
        return IM_OK;
    }

    /*
     * This partition will be divided into two parts.  The "top" part
     * will have a "1" bit appended to its "prefix" bit string; the
     * "bottom" part will have a "0" bit appended to its "prefix".
     *
     * We need to determine the maximum number of source values which
     * may be assigned to the two partitions.  The first issue to con-
     * sider is that PKUNZIP 1.10's tree-decoding shortcuts require a
     * certain number of leading "0" bits in each code string, depending
     * on its length.  Code strings of 9-12 bits must have at least 4
     * leading zeros; strings of 13 or 14 bits, at least 6 leading
     * zeros; and strings of 15 or 16 bits, at least 8 leading zeros.
     *
     * If the "prefix" is zero, the above limitation is used to restrict
     * the maximum size of the top half of the partition.  The bottom
     * half does not need to be restricted in this way, since it can be
     * extended as far as needed along the path where the "prefix" grows
     * in length but remains all zero.
     */
    botmaxbits = maxbits;
    if (prefix != 0) topmaxbits = maxbits;
    else
    {   for (n = 0, leadzeros = 0x8000;
             n < preflen && (prefix & leadzeros) == 0;
             n++, leadzeros >>= 1) ;
        topmaxbits = maxarray[n];
        if (topmaxbits > maxbits) topmaxbits = maxbits;
    }
    if (topmaxbits < minbits)
    {   fprintf (stderr, "\nError in ct_split: ");
        fprintf (stderr, "topmaxbits(%d) < minbits(%d)",
                 topmaxbits, minbits);
        goto oops;
    }
    if (botmaxbits < minbits)
    {   fprintf (stderr, "\nError in ct_split: ");
        fprintf (stderr, "botmaxbits(%d) < minbits(%d)",
                 botmaxbits, minbits);
        goto oops;
    }
    topmaxvals = 1 << (topmaxbits - preflen - 1);
    n = size >> 1; if (topmaxvals > n) topmaxvals = n;
    botmaxvals = 1 << (botmaxbits - preflen - 1);
    n = size - 1;  if (botmaxvals > n) botmaxvals = n;
    if (topmaxvals + botmaxvals < size)
    {   fprintf (stderr, "\nError in ct_split: ");
        fprintf (stderr, "topmaxvals(%d) + botmaxvals(%d) ",
                 topmaxvals, botmaxvals);
        fprintf (stderr, "< size(%d)", size);
        goto oops;
    }

    /*
     * We now split the current partition into two halves of as close
     * to equal frequency as possible.  If the total of all frequencies
     * in the partition is zero, split into two halves of equal size.
     */
    if (freq == 0)
    {   topsize = size >> 1;
        ct = part + topsize;
        topfreq = 0;
    }
    else
    {   halffreq = freq >> 1;           /* half the total frequency */
        m = size >> 1;                  /* half the total elements, */
                                        /*    rounded down          */
        for (topsize = 0, topfreq = 0, ct = part;
             topsize < m && topfreq <= halffreq
                 && (onefreq = ct->ct_freq) > 0;
             topsize++, ct++)
            topfreq += onefreq;
        if (topsize >= 2)
        {   /*
             * If moving one element from the top to the bottom parti-
             * tion would make the two more closely equal in frequency,
             * do it.
             */
            onefreq = (ct-1)->ct_freq;
            if ((topfreq - halffreq) > (halffreq - (topfreq - onefreq)))
                ct--, topsize--, topfreq -= onefreq;
    }   }
    botsize = size - topsize;
    botfreq = freq - topfreq;
    /* "ct" points to first element in bottom half */

    /*
     * The above first-cut attempt to split the partition may not work
     * for one of two reasons.  First, one or the other half may contain
     * too many values (more than "topmaxvals" or "botmaxvals").
     */
    while (topsize > topmaxvals)
    {   onefreq = (--ct)->ct_freq;
        topsize--; topfreq -= onefreq;
        botsize++; botfreq += onefreq;
    }
    while (botsize > botmaxvals)
    {   onefreq = (ct++)->ct_freq;
        topsize++; topfreq += onefreq;
        botsize--; botfreq -= onefreq;
    }

    /*
     * Second, the number of bits required to represent the values in
     * each half may violate PKZIP's requirement (implicit in the way
     * trees are compressed in an imploded file) that no code string in
     * the top half may be longer than any code string in the bottom
     * half.
     */
    localminbits = preflen + 1;
    if (localminbits < minbits) localminbits = minbits;
    for (;;)
    {   for (maxshort = preflen + 1, n = 1;
             n < botsize;
             maxshort++, n <<= 1) ;
        if (n > botsize) maxshort--;
        if (maxshort < localminbits) maxshort = localminbits;
        if (maxshort > topmaxbits) maxshort = topmaxbits;
        for (minlong = preflen + 1, n = 1;
             n < topsize;
             minlong++, n <<= 1) ;
        if (minlong <= maxshort) break;
        onefreq = (--ct)->ct_freq;
        topsize--; topfreq -= onefreq;
        botsize++; botfreq += onefreq;
    }

    /*
     * Third, the number of elements in the top half must be enough to
     * result in each string having at least "minbits" bits in all.
     */
    n = 1 << (minbits - preflen - 1);
    while (topsize < n)
    {   onefreq = (ct++)->ct_freq;
        topsize++; topfreq += onefreq;
        botsize--; botfreq -= onefreq;
    }

    /*
     * Now that the sizes of the two halves of the partition have been
     * finalized, process the top and bottom halves via recursion.
     */
    retcode = ct_split (part, topsize, topfreq,
                        prefix | (1 << (15-preflen)),
                        preflen + 1, localminbits, maxshort);
    if (retcode != IM_OK) return retcode;
    ct = part + topsize;
    retcode = ct_split (ct, botsize, botfreq,
                        prefix, preflen + 1, (int)ct[-1].ct_len, maxbits);
    if (retcode != IM_OK) return retcode;

    /* That's all. */
    return IM_OK;

#ifdef VALIDATE
badarg:
    fprintf (stderr, "\nError in ct_split: bad argument(s)");
    putchar ('\n'); fflush (stdout); fflush (stderr);
    /* abort (); */
    return IM_BADARG;
#endif /* VALIDATE */

oops:
#ifdef VALIDATE
    putchar ('\n'); fflush (stdout); fflush (stderr);
    /* abort (); */
#endif /* VALIDATE */
    return IM_LOGICERR;
}


/***********************************************************************
 *
 * Sorting function -- descending order by source value frequency.
 *
 * This sorting function is used at the start of the code tree con-
 * struction process, before the bit string values are assigned.
 * To ensure consistent behaviour on all machines, we use the source
 * values as secondary sort key, but this is not mandatory.
 */

local
int
ct_fsort (tr1, tr2)
    TRDATA *tr1, *tr2;
{   long d;
    int v;

    d = (long) tr1->ct_freq - (long) tr2->ct_freq;
    if (d < 0) return 1;
    if (d > 0) return -1;
    v = (int) tr1->ct_val - (int) tr2->ct_val;
    if (v < 0) return 1;
    if (v > 0) return -1;
    return 0;
}


/***********************************************************************
 *
 * Sorting function -- ascending order by bit string length; if lengths
 * are the same, ascending order by source value.
 *
 * This sorting function is used after the bit string values have been
 * assigned.  
 */

local
int
ct_rsort (cr1, cr2)
    RESORT *cr1, *cr2;
{   int d;

    d = (int) cr1->ct_rlen - (int) cr2->ct_rlen;
    if (d > 0) return 1;
    if (d < 0) return -1;
    d = (int) cr1->ct_rval - (int) cr2->ct_rval;
    if (d > 0) return 1;
    if (d < 0) return -1;
    return 0;
}


/***********************************************************************
 *
 * Allocate the code trees.
 */

ImpErr
ct_init ()
{   ImpErr retcode;
    int i;

#ifdef DEBUG
    if (256*sizeof(TRDATA) > MA_BUFSIZE*sizeof(MATCH)) return IM_LOGICERR;
#endif /* DEBUG */

    retcode = ct_windup ();
        if (retcode != IM_OK) return retcode;

    ct_litc_num = 0;
    ct_lit2_num = 0;
    ct_strg_num = 0;

    for (i = 255; i >= 0;  i--)
        ct_litc_freq[i] = 0;
    for (i = 63; i >= 0; i--)
        ct_len2_freq[i] = 0, ct_len3_freq[i] = 0,
        ct_dst2_freq[i] = 0, ct_dst3_freq[i] = 0;

    retcode = ct_alloc (256, &ct_litc_tree);
    if (retcode != IM_OK) return retcode;
    retcode = ct_alloc  (64, &ct_len2_tree);
    if (retcode != IM_OK) return retcode;
    retcode = ct_alloc  (64, &ct_len3_tree);
    if (retcode != IM_OK) return retcode;
    retcode = ct_alloc  (64, &ct_dst2_tree);
    if (retcode != IM_OK) return retcode;
    retcode = ct_alloc  (64, &ct_dst3_tree);
    if (retcode != IM_OK) return retcode;

    return IM_OK;
}


/***********************************************************************
 *
 * Tally a "string match" data set.  The tally results will be used to
 * determine how large the imploded result will be.
 */

ImpErr
ct_tally (ma)
         MATCH *ma;             /* match data to write out */
{   register int ch;
    int dist = ma->ma_dist;

    /* Tally up the latest data. */
    if (dist == 0) {                 /* literal character */
            ct_litc_num++;
            ch = ma->l.ma_litc[0];
                ct_litc_freq[ch]++;

    } else if (dist < 0) {           /* 2-character match */
            ct_lit2_num++;
            ch = ma->l.ma_litc[0];
                ct_litc_freq[ch]++;
            ch = ma->l.ma_litc[1];
                ct_litc_freq[ch]++;
            ch = ((-dist-1) >> fd.fd_nbits) & 0x3f;
                ct_dst2_freq[ch]++;
            ct_len2_freq[0]++;

     } else {                        /* 3-char or longer match */
            ct_strg_num++;
            ch = ((dist-1) >> fd.fd_nbits) & 0x3f;
                ct_dst3_freq[ch]++;
         /* We defer the update of ct_dst2_freq and ct_len2_freq until
          * ct_mktrees:
          *     ct_dst2_freq[ch]++;
          * ch = ma->l.ma_length - 2;
          *     if (ch > 63) ch = 63;
          *     ct_len2_freq[ch]++;
          */
            ch = ma->l.ma_length - 3;
                if (ch > 63) ch = 63;
                ct_len3_freq[ch]++;
    }

    /* That's all. */
    return IM_OK;
}


/***********************************************************************
 *
 * Construct all code trees, and then determine how big the imploded
 * file will be under both "literal tree" and "no literal tree" con-
 * ditions.  Choose the best option.
 */

ImpErr
ct_mktrees ()
{   U_CHAR *c;
    ImpErr retcode;
    register long sum;
    long len2, len3;
    int n;

    /* ct_tally did not update ct_dst2_freq and ct_len2_freq for matches of
     * length > 2, so correct this now.
     */
    for (n = 62; n >= 0; n--) {
        ct_dst2_freq[n] += ct_dst3_freq[n];
        ct_len2_freq[n+1] += ct_len3_freq[n];
    }
    ct_dst2_freq[63] += ct_dst3_freq[63];
    ct_len2_freq[63] += ct_len3_freq[63];

    /*
     * Construct the code trees and see how much space each will save.
     *
     * It is conceivable that a tree could result in a negative savings
     * if its compressed form is sufficiently long.
     *
     * We need to construct the ZIP-file compressed representation of
     * each tree in order to figure out how much space it will take.
     * However, we don't save these tree representations now; rather,
     * we'll wait until later and reconstruct the representations for
     * whichever two (or three) trees we really need for the output.
     */
#ifdef IMPDEBUG
    treename = (char *)NULL;
#endif /* IMPDEBUG */

    /* literal code tree */
    retcode = ct_loadf    (ct_litc_tree,  ct_litc_freq);
        if (retcode != IM_OK) return retcode;
    retcode = ct_gencodes (ct_litc_tree, 1, 16, &ct_litc_saved);
        if (retcode != IM_OK) return retcode;
    retcode = ct_ziprep   (ct_litc_tree, &c);
        if (retcode != IM_OK) return retcode;
    ct_litc_saved -= (int) (c[0]+2) * 8;

    /* length code tree (2) */
    retcode = ct_loadf    (ct_len2_tree,  ct_len2_freq);
        if (retcode != IM_OK) return retcode;
    retcode = ct_gencodes (ct_len2_tree, 1, 16, &ct_len2_saved);
        if (retcode != IM_OK) return retcode;
    retcode = ct_ziprep   (ct_len2_tree, &c);
        if (retcode != IM_OK) return retcode;
    ct_len2_saved -= (int) (c[0]+2) * 8;

    /* length code tree (3) */
    retcode = ct_loadf    (ct_len3_tree,  ct_len3_freq);
        if (retcode != IM_OK) return retcode;
    retcode = ct_gencodes (ct_len3_tree, 1, 16, &ct_len3_saved);
        if (retcode != IM_OK) return retcode;
    retcode = ct_ziprep   (ct_len3_tree, &c);
        if (retcode != IM_OK) return retcode;
    ct_len3_saved -= (int) (c[0]+2) * 8;

    /* distance code tree (2) */
    retcode = ct_loadf    (ct_dst2_tree,  ct_dst2_freq);
        if (retcode != IM_OK) return retcode;
    retcode = ct_gencodes (ct_dst2_tree, 1,  8, &ct_dst2_saved);
        if (retcode != IM_OK) return retcode;
    retcode = ct_ziprep   (ct_dst2_tree, &c);
        if (retcode != IM_OK) return retcode;
    ct_dst2_saved -= (int) (c[0]+2) * 8;

    /* distance code tree (3) */
    retcode = ct_loadf    (ct_dst3_tree,  ct_dst3_freq);
        if (retcode != IM_OK) return retcode;
    retcode = ct_gencodes (ct_dst3_tree, 1,  8, &ct_dst3_saved);
        if (retcode != IM_OK) return retcode;
    retcode = ct_ziprep   (ct_dst3_tree, &c);
        if (retcode != IM_OK) return retcode;
    ct_dst3_saved -= (int) (c[0]+2) * 8;

    /*
     * Determine how big the compressed file will be
     * with, and without, a literal character tree.
     */

    /* compressed length (no literal tree) */
    sum  = ct_litc_num + ct_lit2_num + ct_strg_num;    /* initial bit */
    sum += ct_litc_num * 8;                          /* literal bytes */
    sum += (ct_lit2_num+ct_strg_num) * 6 - ct_len2_saved;  /* lengths */
    sum += 8 * ct_len2_freq[63];                  /* oversize lengths */
    sum += (ct_lit2_num+ct_strg_num) * (fd.fd_nbits+6)
            - ct_dst2_saved;                             /* distances */
    len2 = (sum+7) / 8;                           /* convert to bytes */

    /* compressed length (with literal tree) */
    sum  = ct_litc_num + 2*ct_lit2_num + ct_strg_num;  /* initial bit */
    sum += (ct_litc_num+2*ct_lit2_num)*8 - ct_litc_saved;/* lit bytes */
    sum += ct_strg_num * 6 - ct_len3_saved;                /* lengths */
    sum += 8 * ct_len3_freq[63];                  /* oversize lengths */
    sum += ct_strg_num * (fd.fd_nbits+6) - ct_dst3_saved;   /* dist's */
    len3 = (sum+7) / 8;                           /* convert to bytes */

    /*
     * PKUNZIP 1.10 requires that the source value 255 in a "literal"
     * tree must be represented by a bit string of length >= 10.  The
     * literal tree was already adjusted to ensure that the value 255
     * was given a bit string of length 10 or greater if possible.  If
     * this did not succeed -- which would only happen if the longest
     * bit string in the literal tree were of length 8 or 9 -- then the
     * literal tree cannot be used.  In such a case, not much would be
     * gained by using it anyway, so there's little reason to be upset.
     */
    if (ct_table[ct_litc_tree].ct_array[255].ct_len < 10)
        len3 = len2;

    /*
     * Choose the method of compression which will use the least space
     * for this particular file.  The possibilities are:  use a literal
     * character tree; or, don't use a literal character tree.
     */
    if (len2 <= len3)
    {   fd.fd_method = NO_LITERAL_TREE;
        fd.fd_clen   = len2;
        lit_tree     = -1;
        len_tree     = ct_len2_tree;
        dst_tree     = ct_dst2_tree;
        retcode = ct_free (ct_litc_tree);
            if (retcode != IM_OK) return retcode;
        retcode = ct_free (ct_dst3_tree);
            if (retcode != IM_OK) return retcode;
        retcode = ct_free (ct_len3_tree);
            if (retcode != IM_OK) return retcode;
    }
    else
    {   fd.fd_method = LITERAL_TREE;
        fd.fd_clen   = len3;
        lit_tree     = ct_litc_tree;
        len_tree     = ct_len3_tree;
        dst_tree     = ct_dst3_tree;
        retcode = ct_free (ct_dst2_tree);
            if (retcode != IM_OK) return retcode;
        retcode = ct_free (ct_len2_tree);
            if (retcode != IM_OK) return retcode;
    }

    /* That's all. */
    return IM_OK;
}


/***********************************************************************
 *
 * Output the code trees.
 */

ImpErr
ct_wrtrees (outfp)
    FILE *outfp;                        /* output file */
{   ImpErr retcode;
    U_CHAR *c;

    /* Output the literal tree, if any. */
#ifdef IMPDEBUG
    treename = "Literal";
#endif /* IMPDEBUG */
    if (lit_tree >= 0)
    {   retcode = ct_ziprep (lit_tree, &c);
            if (retcode != IM_OK) return retcode;
        if (zfwrite ((char *) c, (int) (c[0]+2), 1, outfp) != 1)
            return IM_IOERR;
    }

    /* Output the length tree. */
#ifdef IMPDEBUG
    treename = "Length";
#endif /* IMPDEBUG */
    retcode = ct_ziprep (len_tree, &c);
        if (retcode != IM_OK) return retcode;
    if (zfwrite ((char *) c, (int) (c[0]+2), 1, outfp) != 1)
        return IM_IOERR;

    /* Output the distance tree. */
#ifdef IMPDEBUG
    treename = "Distance";
#endif /* IMPDEBUG */
    retcode = ct_ziprep (dst_tree, &c);
        if (retcode != IM_OK) return retcode;
    if (zfwrite ((char *) c, (int) (c[0]+2), 1, outfp) != 1)
        return IM_IOERR;

    return IM_OK;
}

/* Macros for outputting bit string. */

#define OUTBITS(value,length) \
        {   retcode = bi_rlout ((int) (value), (int) (length)); \
                if (retcode != IM_OK) return retcode; \
        }
#define OUTCODE(value,tree) \
        {   ct_lookup (tree, value, bitstring, bitlength); \
            retcode = bi_rlout (bitstring, bitlength); \
                if (retcode != IM_OK) return retcode; \
        }

/***********************************************************************
 *
 * Output the body of the file in imploded form.
 */
ImpErr
ct_wrdata (outfp)
         FILE *outfp;                   /* output (ZIP) file */
{   MATCH *ma;
    ImpErr retcode;
    register int minmatch;
    int bitstring, bitlength;
    int bitmask = (1 << (fd.fd_nbits+1))-1;
    /* Used to select the bottom 6 or 7 bits of a distance, which are
     * output literally, plus 1 bit marking a distance
     */
    int matches;
#ifdef  IMPDEBUG
    long srcpos;
#endif  /* IMPDEBUG */

    /* Determine the minimum match length. */
    minmatch = (lit_tree >= 0) ? 3 : 2;

    /* Prepare the I/O. */
    if (tflush (fd.fd_temp) != 0) return IM_IOERR;
    trewind (fd.fd_temp);
    retcode = bi_init (outfp);
        if (retcode != IM_OK) return retcode;

#ifdef  IMPDEBUG
        srcpos = 0;
        fprintf (stderr, "\nImploded output:\n");
#endif  /* IMPDEBUG */

    /* Read and process data from the temporary file. */
    while ((matches =
       tread ((char *) ma_buf, sizeof(MATCH), MA_BUFSIZE, fd.fd_temp)) > 0)
       for (ma = ma_buf; matches > 0; ma++, matches--)
    {
        int dist = ma->ma_dist;
        int len = 0;

#ifdef  IMPDEBUG
        fprintf (stderr, "%8ld: ", srcpos);
#endif  /* IMPDEBUG */

        if (dist < 0) {
            dist = -dist, len = 2;
        } else if (dist > 0) {
            len = ma->l.ma_length;
        }

        /* Output distance and length if enough characters match. */
        if (len >= minmatch)
        {   /* "matched string" header bit (0) */
#ifdef  IMPDEBUG
            fprintf (stderr, "str (dst=%d,len=%d)  ",
                     dist, len);
            srcpos += len;
#endif  /* IMPDEBUG */

            /* ouput one zero bit then the distance */
            dist--;
            OUTBITS ((dist << 1) & bitmask, fd.fd_nbits + 1);
            OUTCODE (dist >> fd.fd_nbits, dst_tree);

            /* length -- depends on how it compares to maximum */
            len -= minmatch;
            if (len >= 63)
            {   /* big length -- output code for 63, then surplus */
                OUTCODE (63, len_tree);
                OUTBITS ((len - 63), 8);
            }
            else
            {   /* small length -- output code */
                OUTCODE (len, len_tree);
        }   }
        else if (lit_tree >= 0)
        {   /* first or single literal -- header bit (1) plus char */
#ifdef  IMPDEBUG
            fprintf (stderr, "lit (val=%02x)  ",
                     ma->l.ma_litc[0] & 0xff);
            srcpos++;
#endif  /* IMPDEBUG */
            OUTBITS (1, 1);
            OUTCODE (ma->l.ma_litc[0], lit_tree);
            if (len == 2)
            {   /* second literal -- header bit (1) plus char */
#ifdef  IMPDEBUG
                fprintf (stderr, "\n%8ld: lit (val=%02x)  ",
                         srcpos, ma->l.ma_litc[1] & 0xff);
                srcpos++;
#endif  /* IMPDEBUG */
                OUTBITS (1, 1);
                OUTCODE (ma->l.ma_litc[1], lit_tree);
        }   }
        else
        {   /* single literal -- header bit (1) plus char */
#ifdef  IMPDEBUG
            fprintf (stderr, "lit (val=%02x)  ",
                     ma->l.ma_litc[0] & 0xff);
            srcpos++;
#endif  /* IMPDEBUG */
            OUTBITS ((ma->l.ma_litc[0] << 1) + 1, 9);
        }
#ifdef  IMPDEBUG
        putc ('\n', stderr);
#endif  /* IMPDEBUG */
    }

    /* Make sure we hit EOF on input without an error. */
    if (terror (fd.fd_temp)
#ifndef MINIX
#ifndef __TURBOC__      /* TurboC 2.0 does not set the EOF flag (?) */
        || !teof (fd.fd_temp)
#endif /* !__TURBOC__ */
#endif /* !MINIX */
       )
      return IM_IOERR;
    retcode = bi_windup ();
        if (retcode != IM_OK) return retcode;

    /* That's all. */
    return IM_OK;
}

#undef  OUTBITS
#undef  OUTCODE


/***********************************************************************
 *
 * Deallocate all code trees.
 */

ImpErr
ct_windup ()
{   int n;
    static windup_already_called = 0;
    ImpErr retcode;

    if (windup_already_called)
    {   /* Discard any old code trees. */
        for (n = 0; n < MAXTREES; n++)
        {   if (ct_table[n].ct_array != NULL)
            {   retcode = ct_free (n);
                if (retcode != IM_OK) return retcode;
    }   }   }
    else
    {   /* Initialize the list of active code trees. */
        for (n = 0; n < MAXTREES; n++)
        {   ct_table[n].ct_array = NULL;
            ct_table[n].ct_size  = 0;
        }
        windup_already_called = 1;
    }

    /* That's all. */
    return IM_OK;
}


/**********************************************************************/
