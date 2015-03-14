/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  implode.c by Richard B. Wales.
 *
 *  PURPOSE
 *
 *      Compress an input file using the ZIP "implosion" method.
 *
 *  DISCUSSION
 *
 *      The "implosion" algorithm is a composite of (1) OPM/L compres-
 *      sion within a sliding window, and (2) variable-length binary
 *      encoding of various parts of the OPM/L output.
 *
 *      For a detailed treatment of OPM/L compression, see the source
 *      file "im_lmat.c".
 *
 *      For a detailed treatment of variable-length binary encoding,
 *      see the source file "im_ctree.c".
 *
 *      Since Pass Two (binary encoding) depends on a statistical anal-
 *      ysis of the entire output of Pass One (OPM/L compression), the
 *      Pass One output is saved in a temporary file and re-read for
 *      Pass Two.  Implosion is thus a two-pass algorithm.
 *
 *      An outline of the algorithm follows:
 *
 *      (1) The entire input file is read and compressed via the OPM/L
 *          technique.  The OPM/L output is saved in a temporary file.
 *
 *      (2) The compressed info from (1) is analyzed, and various fre-
 *          quency counts are tallied.
 *
 *      (3) Based on the frequency counts, a decision is made as to
 *          whether or not the "literal" characters (those not matched
 *          in earlier parts of the input) should be represented via a
 *          binary code tree or left "as is".  If there are not very
 *          many literal characters, or if their frequency distribution
 *          is fairly even, the number of bits saved through coding may
 *          be exceeded by the amount of info which must be included to
 *          describe the tree.
 *
 *      (4) The temporary file from (1) is re-read.  The information is
 *          further compressed via the binary code trees, and the result
 *          is sent to the ZIP output file.
 *
 *  REFERENCES
 *
 *      APPNOTE.TXT documentation file in PKZIP 1.10 distribution.
 *
 *      See also references in the "im_lmat.c" and "im_ctree.c" source
 *      files.
 *
 *  INTERFACE
 *
 *      int imp_setup (long filesize, int pack_level)
 *          Initialize the "implode" routines for a new file.
 *
 *      int imp_p1 (char *buf, int count)
 *          Process "count" input characters pointed to by "buf".  This
 *          routine is called as many times as needed to process the
 *          entire input file.  There is no upper limit on "count".
 *
 *      int imp_size (long *size, char *opts)
 *          Terminate processing of the input file, and return the com-
 *          pressed size ("size") and implosion option flags ("opts").
 *
 *      int imp_p2 (FILE *outfp)
 *          Output the entire compressed file -- including S-F trees and
 *          data -- to the ZIP output file "outfp".
 *
 *      int imp_clear (void)
 *          Clean up after processing and outputting a file.  This rou-
 *          tine may be called at any time to abort further processing.
 *
 *      All the above routines return zero if successful, or a non-zero
 *      value if there was an error.
 */

#include "implode.h"
#include "ziperr.h"


/***********************************************************************
 *
 * Error return codes for the main ZIP program.
 * Most of these are in "ziperr.h".
 */

#define ZE_MAP(err) \
        ( ((err) == IM_OK)      ? ZE_OK \
        : ((err) == IM_NOMEM)   ? ZE_MEM \
        : ((err) == IM_IOERR)   ? ZE_TEMP \
                                : (fprintf(stderr,"\nZE_MAP(%d)",(err)), \
                                    ZE_LOGIC))


/***********************************************************************
 *
 * State variable for the implosion routines.
 */

#define IMP_SETUP       0       /* need to do "imp_setup" */
#define IMP_P1          1       /* setup done, ready for Pass 1 */
#define IMP_P2          2       /* Pass 1 done, ready for Pass 2 */
#define IMP_CLEAR       3       /* Pass 2 done, ready for "imp_clear" */

local int imp_state = IMP_SETUP;


/***********************************************************************
 *
 * Global variables.
 */

FDATA fd;


/***********************************************************************
 *
 * Set up for performing implosion on a new file.
 */

int
imp_setup (filesize, pack_level)
    long filesize;  /* input file size */
    int pack_level; /* 0 = best speed, 9 = best compression, other = default */
{
    ImpErr retcode;
    extern char *tempname();

    if (imp_state != IMP_SETUP)
    {   fprintf (stderr, "\nimp_setup called with wrong state %d",
                 imp_state);
        return ZE_LOGIC;
    }
    imp_state = IMP_P1;

    /* Set up the initial parameters. Use an 8K window if the input
     * file is greater than or equal to 5.5K, a 4K window otherwise.
     */
    fd.fd_bufsize = 8192;
    if (filesize < 5632) fd.fd_bufsize = 4096;

    fd.fd_strsize = 320;
    fd.fd_nbits   = (fd.fd_bufsize == 4096) ? 6 : 7;

    /* Create a temporary file for the Pass One output. */
    fd.fd_temp = topen ('I');
    if (fd.fd_temp == NULL)
        return ZE_MEM;

    /*
     * Initialize the "longest match" routines.
     * "ct_init" is called at this point because the "lm_input" routine
     * (called in "imp_p1") calls the "ct_tally" routine.
     */
    retcode = lm_init (pack_level);
    if (retcode != IM_OK) return ZE_MAP (retcode);
    retcode = ct_init ();
    return ZE_MAP (retcode);
}


/***********************************************************************
 *
 * Feed characters to the implosion computation.
 * This routine is called as many times as needed, until the entire
 * input file has been processed.
 */

int
imp_p1 (buf, count)
    char *buf;                  /* input characters */
    int   count;                /* character count */
{   ImpErr retcode;

    if (imp_state != IMP_P1)
    {   fprintf (stderr, "\nimp_p1 called with wrong state %d",
                 imp_state);
        return ZE_LOGIC;
    }

    if (buf == NULL || count < 0)
    {   fprintf (stderr, "\nimp_p1 called with bad arguments");
        return ZE_LOGIC;
    }
    retcode = lm_input ((U_CHAR *) buf, (U_INT) count);
    return ZE_MAP (retcode);
}


/***********************************************************************
 *
 * Terminate processing of input for this file, and determine the size
 * this file will have if it is imploded.  Also, find out whether two
 * or three S-F trees will be used (needed for directory entries).
 */

int
imp_size (size, opts)
    long *size;                 /* imploded size */
    char *opts;                 /* implosion option info */
{   ImpErr retcode;

    if (imp_state != IMP_P1)
    {   fprintf (stderr, "\nimp_size called with wrong state %d",
                 imp_state);
        return ZE_LOGIC;
    }
    imp_state = IMP_P2;

    if (size == NULL || opts == NULL)
    {   fprintf (stderr, "\nimp_size called with bad arguments");
        return ZE_LOGIC;
    }
    retcode = lm_windup ();
    if (retcode != IM_OK) return ZE_MAP (retcode);
    retcode = ct_mktrees ();
    if (retcode != IM_OK) return ZE_MAP (retcode);
    *size = fd.fd_clen;
    *opts = (char)(((fd.fd_bufsize == 8192) ? 0x02 : 0)
          | ((fd.fd_method == LITERAL_TREE) ? 0x04 : 0));
    return ZE_OK;
}


/***********************************************************************
 *
 * Output the imploded version of the file (but not the file directory
 * info) to the ZIP file.
 */

int
imp_p2 (outfp)
    FILE *outfp;                        /* output (ZIP) file */
{   ImpErr retcode;

    if (imp_state != IMP_P2)
    {   fprintf (stderr, "\nimp_p2 called with wrong state %d",
                 imp_state);
        return ZE_LOGIC;
    }
    imp_state = IMP_CLEAR;

    if (outfp == NULL)
    {   fprintf (stderr, "\nimp_p2 called with bad arguments");
        return ZE_LOGIC;
    }
    retcode = ct_wrtrees (outfp);
    if (retcode != IM_OK) return ZE_MAP (retcode);
    retcode = ct_wrdata (outfp);
    if (retcode != IM_OK) return ZE_MAP (retcode);
    fflush (outfp);
    if (ferror (outfp)) return ZE_TEMP;
    return ZE_OK;
}


/***********************************************************************
 *
 * Clean up after doing an implosion.
 * This routine may also be called at any time to abort an implosion.
 */

int
imp_clear ()
{   (void) lm_windup ();
    if (fd.fd_temp != NULL)
        (void) tclose (fd.fd_temp);
    (void) ct_windup ();
    imp_state = IMP_SETUP;
    return ZE_OK;
}


/**********************************************************************/
