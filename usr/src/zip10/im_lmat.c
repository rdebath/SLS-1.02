/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  im_lmat.c by Jean-loup Gailly.
 *
 *  PURPOSE
 *
 *      Identify new text as repetitions of old text within a fixed-
 *      length sliding window trailing behind the new text.
 *
 *  DISCUSSION
 *
 *      The "implosion" process depends on being able to identify portions
 *      of the input text which are identical to earlier input (within a
 *      sliding window trailing behind the input currently being processed).
 *
 *      The most straightforward technique turns out to be the fastest for
 *      most input files: try all possible matches and select the longest.
 *      The key feature is of this algorithm is that insertion and deletions
 *      from the string dictionary are very simple and thus fast. Insertions
 *      and deletions are performed at each input character, whereas string
 *      matches are performed only when the previous match ends. So it is
 *      preferable to spend more time in matches to allow very fast string
 *      insertions and deletions. The matching algorithm for small strings
 *      is inspired from that of Rabin & Karp. A brute force approach is
 *      used to find longer strings when a small match has been found.
 *      A similar algorithm is used in freeze (by Leonid Broukhis) but the
 *      algorithm used here is faster.
 *         A previous version of this file used a more sophisticated algorithm
 *      (by Fiala and Greene) which is guaranteed to run in linear amortized
 *      time, but has a larger average cost and uses more memory. However
 *      the F&G algorithm may be faster for some highly redundant files if
 *      the parameter max_chain_length (described below) is too large.
 *
 *  ACKNOWLEDGEMENTS
 *
 *      Rich Wales defined the interface, provided the necessary information
 *      to ensure compatibility with pkunzip 1.0 (not an easy job) and
 *      suggested the solution (n == 1 + n-1) adopted here.
 *      The idea of lazy evaluation of matches is due to Jan Mark Wams, and
 *      I found it in 'freeze' written by Leonid Broukhis.
 *      Special thanks to Kai-Uwe Rommel for the OS/2 port, to Glenn J.
 *      Andrews for the VMS port, and to many other info-zippers for testing.
 *
 *  REFERENCES
 *
 *      A description of the Rabin and Karp algorithm is given in the book
 *          "Algorithms" by R. Sedgewick, Addison-Wesley, p252.
 *
 *      Fiala,E.R., and Greene,D.H.
 *          Data Compression with Finite Windows, CACM, 32,4 (1989) 490-595.
 *
 *  INTERFACE
 *
 *      ImpErr lm_init (int pack_level)
 *          Initialize the "longest match" routines for a new file.
 *          The global variable fd is an implicit parameter.
 *
 *      ImpErr lm_input (U_CHAR *block, U_INT count)
 *          Process a block of input characters.
 *
 *      ImpErr lm_windup (void)
 *          Flush out the remaining unprocessed input.
 */

#include "implode.h"

/***********************************************************************
 *
 * Configuration parameters
 */

#define MAX_MATCH_LENGTH  320
/* The maximum match length. 320 = 64 + 256. (If the length is greater than
 * 63, pkzip uses an extra byte.)
 */

#define MAX_WBITS  13
#define WSIZE (1 << MAX_WBITS)
/* Maximum window size = 8K */

/* Constants used to dimension the hash table: */
#define HASH_BITS  14
/* HASH_BITS must be >= 13, see longest_match() */

#define HASH_SIZE (1<<HASH_BITS)
#define HASH_MASK (HASH_SIZE-1)

#if defined(MSDOS) || defined(i386) || defined(mc68020) || defined(vax)
#   define UNALIGNED_OK
    /* Define this symbol if your target allows access to unaligned data.
     * This is not mandatory, just a speed optimization. The compressed
     * output is strictly identical.
     */
#endif
#ifdef __TURBOC__
#   define DYN_ALLOC
    /* Turbo C 2.0 does not accept far static allocations in small model */
#endif

/***********************************************************************
 *
 * Local data used by the "longest match" routines.
 */

#if HASH_BITS <= 14
   typedef unsigned short Hash;
#else
   /* Defined just for safety, since values > 14 do not speed up implosion */
   typedef unsigned long Hash;
#endif

typedef unsigned short Pos;
typedef unsigned int  IPos;
/* A Pos is an index in the character window. We use short instead of int to
 * save space in the various tables. IPos is used only for parameter passing.
 */

int near min_match_length;
/* Minimum match length, 2 for binary files, 3 for ascii files.
 * (bad luck for ebcdic users; not because they may not get optimal
 * compression, but because they have to use ebcdic machines :-)
 * A zero value means that the min_match_length is not yet determined.
 */

U_CHAR near window[MAX_MATCH_LENGTH + WSIZE + BSZ];
/* MAX_MATCH_LENGTH bytes are duplicated at both ends of the window,
 * to speed up string comparisons. The BSZ extra bytes allow a block copy
 * of the input buffer into the window instead of a copy one byte at a time.
 */

#define MAX_DIST (WSIZE + BSZ)
/* Maximum theoretical distance between two distinct bytes in the window.
 * Actual distances are limited to bufsize.
 */

#define NIL  MAX_DIST
/* Tail of hash chains */

#ifdef DYN_ALLOC
   Hash far *next = NULL;
   Pos  far *prev = NULL;
#else
   Hash far next[MAX_DIST+1];
   Pos  far prev[MAX_DIST+HASH_SIZE+1];
#endif
/* next is a link to a more recent string with same hash index, or to the head
 * of a hash table chain if there is no such string. next[NIL] is used to
 * avoid extra checks. next[s] is NIL if string s is not yet in the dictionary
 *
 * prev is a link to an older string with same hash index (first MAX_DIST
 * values) or head of hash chain (last HASH_SIZE values). prev[NIL] is used
 * to avoid extra checks.
 */
#define match_head (prev+(MAX_DIST+1))

Hash near ins_h;  /* hash index of string to be inserted. */

int near h_shift;
/* Number of bits by which ins_h must be shifted at each
 * input step. It must be such that after min_match_length steps, the oldest
 * byte no longer takes part in the hash key, that is:
 *   h_shift * min_match_length >= HASH_BITS
 */

MATCH *ma_buf = NULL;
/* Buffer used to speed up reading/writing to/from temp file */
#define MA_BUFEND (ma_buf+MA_BUFSIZE)

MATCH *ma;
/* Pointer to the most recent match. */

int near start_length;
/* Matches not greater than this are discarded. This is used in the lazy match
 * evaluation. If start_length > 1, ma is a valid guess of length start_length
 * and ct_tally has not yet been called.
 */

        int near strstart;      /* start of string to insert */
        int near strsize;       /* length of string to insert */
        int near match_length;  /* length of current best match */
        int near bufsize;       /* # of slots in window */
        int near checkpoint;    /* look for new match at this point */
static  int      insert_point;  /* position of next input buffer */

static  int      max_lazy_match;
/* We try lazy evaluation only for matches of length 2..max_lazy_match, to
 * speed up the implosion. We use 0 for maximum speed, 0.9*MAX_MATCH_LENGTH
 * for maximum compression.
 */

        int near max_chain_length;
/* To speed up implosion, hash chains are truncated to this length.
 * A higher limit improves compression ratio but degrades the speed.
 * We use 40 for maximum speed, 960 for maximum compression. Values
 * below 20 are not recommended.
 */

/* Values for max_lazy_match and max_chain_length, depending on the desired
 * pack level (0..9). The values given below have been tuned to exclude
 * worst case performance for pathological files. Better values may be
 * found for specific files. Note that the current algorithm requires 
 * max_lazy >= 2.
 */
typedef struct config {
   int max_lazy;
   int max_chain;
} config;

static config configuration_table[10] = {
/* 0 */ {2,                     MAX_MATCH_LENGTH/8}, /* maximum speed */
/* 1 */ {4,                     MAX_MATCH_LENGTH/4},
/* 2 */ {5,                     MAX_MATCH_LENGTH/2},
/* 3 */ {MAX_MATCH_LENGTH/16,   MAX_MATCH_LENGTH/2},
/* 4 */ {MAX_MATCH_LENGTH/16,   3*MAX_MATCH_LENGTH/4},
/* 5 */ {MAX_MATCH_LENGTH/16,   MAX_MATCH_LENGTH},
/* 6 */ {MAX_MATCH_LENGTH/16,   3*MAX_MATCH_LENGTH/2},
/* 7 */ {MAX_MATCH_LENGTH/16,   2*MAX_MATCH_LENGTH},
/* 8 */ {9*MAX_MATCH_LENGTH/10, 2*MAX_MATCH_LENGTH},
/* 9 */ {9*MAX_MATCH_LENGTH/10, 3*MAX_MATCH_LENGTH}}; /* maximum compression */


#define MIN(a,b) ((a) <= (b) ? (a) : (b))
/* The arguments must not have side effects. */

#define EQUAL 0
/* result of strncmp for equal strings */

/*  Prototypes for local functions */

static void   set_min_match_length OF ((U_CHAR *block, U_INT count));
       ImpErr write_match OF ((IPos ma_start, int ma_length));
       IPos   longest_match OF ((IPos cur_match));
       ImpErr lm_process OF ((U_INT count));

/***********************************************************************
 *
 * Initialize the "longest match" routines for a new file.
 * The global variable fd is an implicit parameter.
 */
ImpErr lm_init (pack_level)
    int pack_level; /* 0: best speed, 9: best compression, other: default */
{
    register int i;

    /* Validate the arguments */
    bufsize = fd.fd_bufsize;
    strsize = MIN (fd.fd_strsize, MAX_MATCH_LENGTH);
    if (bufsize > WSIZE)          return IM_BADARG;
    if (bufsize < 2 * strsize)    return IM_BADARG;
    if (pack_level < 0 || pack_level > 9) return IM_BADARG;

    /* Make sure "bufsize" is a power of 2 */
    if ((bufsize & (bufsize - 1)) != 0) return IM_BADARG;

    /* Use dynamic allocation if compiler does not like big static arrays: */
#ifdef DYN_ALLOC
    if (prev == NULL) {
       next = (Hash far*)farmalloc((U_INT)(MAX_DIST+9)*sizeof(Hash));
       prev = (Pos far*) farmalloc((U_INT)(MAX_DIST+HASH_SIZE+9)*sizeof(Pos));
       /* We allocate 16 extra bytes for the normalization under MSDOS */
       if (prev == NULL || next == NULL) return IM_NOMEM;

#   if defined(MSDOS) && !defined(OS2)
       /* Normalize to pointers with offset 0 (required by asm version).
        * For OS/2, we can't of course play such nasty games.
        */
#define NORMALIZE(ptr) { \
   *((int*)&ptr+1) += ((unsigned)(ptr-0) + 15) >> 4; \
   *(int*)&ptr = 0; \
}
       NORMALIZE(prev); NORMALIZE(next);
#   endif
    }
#endif /* DYN_ALLOC */

    /* Initialize the hash tables. */
    for (i = 0;  i < HASH_SIZE; i++) match_head[i] = NIL;
    for (i = 0;  i <= MAX_DIST; i++) next[i] = NIL;
    /* prev[0..MAX_DIST] will be initialized on the fly */
    ins_h = 0;

    /* Assume strsize zeros before the input (bytes beyond strsize
     * can be garbage):
     */
    memset((char*)window, 0, MAX_MATCH_LENGTH);
    /* It is not necessary to duplicate this at the end of the window.
     * Duplication will start only after the first wrap around.
     */
    insert_point = MAX_MATCH_LENGTH;

    /* Force a check for the file type (ascii/binary) and set the default
     * configuration parameters:
     */
    min_match_length = 0;
    max_lazy_match   = configuration_table[pack_level].max_lazy;
    max_chain_length = configuration_table[pack_level].max_chain;

    /* Do not report matches before the first strsize strings have been
     * inserted in the suffix tree:
     */
    strstart = 0;
    checkpoint = strsize;
    if (ma_buf == NULL) {
        ma_buf = (MATCH *) malloc ((unsigned) (MA_BUFSIZE * sizeof (MATCH)));
        if (ma_buf == NULL) return IM_NOMEM;
    }
    ma = ma_buf - 1;
    start_length = 1;

    /* All done. */
    return IM_OK;
}

/***********************************************************************
 *
 * Output the match info.
 * IN assertions: The matching strings start at strstart and ma_start
 *    and have a length of ma_length bytes.
 *    If ma_length is not greater than start_length, ma_start is garbage.
 *    strstat == checkpoint. If start_length > 1, ma is the
 *    previous match which has not yet been output.
 * OUT assertion: checkpoint is reset according to the match length
 *    actually chosen.
 *    ma is set to the current match, with start_length set appropriately.
 */
ImpErr write_match(ma_start, ma_length)
    IPos ma_start;           /* start of matched string */
    int ma_length;           /* length of complete match */
{
    int ma_dist = 0;         /* distance of current match */

    /* ma_length can be too large towards the end of the input: */
    if (ma_length > strsize) ma_length = strsize;

#ifdef DEBUG
    /* check that the match is indeed a match */
    if (ma_length > start_length &&
        strncmp(window + ma_start, window + strstart, ma_length) != EQUAL) {
        fprintf(stderr,
            "write_match: ma_start %d, strstart %d, ma_length %d\n",
            ma_start, strstart, ma_length);
        exit(1);
    }
#endif
    /* PKUNZIP accepts most overlapping matches.  However, when the
     * distance has the value 1, versions of PKUNZIP prior to 1.10 don't
     * handle the overlap properly -- and version 1.10 handles the
     * overlap correctly only if the length is limited to 62 plus the
     * minimum match length; i.e., only if there is no supplementary
     * length byte.  (From phone conversation with Phil Katz, 23 January
     * 1991.) The compression ratio is generally better when we do not
     * limit the match length to 64, so we remove distance-one matches
     * completely. (But PKUNZIP 1.01 also rejects some distance-two matches.
     * This could be fixed but would degrade compression.)
     */
    if (ma_length > 1) {
        ma_dist = strstart - ma_start;
        if (ma_dist < 0) ma_dist += MAX_DIST;
        if (ma_dist == 1) {
            /* keep the previous match if it was delayed */
            if (start_length > 1) {
                ma_length = 1;
            } else {
                /* Truncate the match to 1 */
                ImpErr retcode = write_match(ma_start, 1);
                if (retcode != IM_OK) return retcode;

                /* Emit a match with a distance of two and a length reduced by
                 * one. This reduced match may be delayed.
                 */
                checkpoint = ++strstart;
                retcode = write_match(ma_start, ma_length-1);
                strstart--;
                return retcode; /* Leave checkpoint unchanged */
            } /* start_length > 1 */
        } /* ma_dist == 1 */
    } /* ma_length > 1 */

    /* If the previous match has been delayed, keep it or prefer the
     * current match:
     */
    if (start_length > 1) {
        /* Keep the previous match if it is not shorter than the current one.
         * Otherwise, emit only the first byte of the previous match,
         * followed by the current match. If we have a delayed match for
         * the last bytes of the input file, the next match will necessarily
         * be smaller, so ct_tally will correctly be called for the delayed
         * match.
         */
        if (start_length >= ma_length) {
            /* Keep the previous match */
            if (start_length == 2) {
                ma->ma_dist = - ma->ma_dist;
                ma->l.ma_litc[1] = window[strstart]; /* litc[0] already set */
            } else {
                ma->l.ma_length = start_length; /* overwrite ma->l.ma_litc */
            }
            checkpoint = strstart + start_length - 1;
            start_length = 1;
            return ct_tally (ma);
        }
        /* Shorten the previous match to zero */
        ma->ma_dist = 0; /* keep ma->l.ma_litc */
        start_length = 1;
        (void) ct_tally (ma); /* ignore result, ct_tally cannot fail */
    }

    if (++ma == MA_BUFEND) {
        ma = ma_buf;
        if (twrite ((char *) ma, sizeof(MATCH), MA_BUFSIZE, fd.fd_temp)
            != MA_BUFSIZE) return IM_IOERR;
    }

    /* Keep the current match as guess only if its length is small,
     * trying to find a better match at the next step. If speed is not
     * critical, we use this lazy mechanism for all lengths.
     */
    if (ma_length > 1) {
        ma->ma_dist = ma_dist;
        if (ma_length <= max_lazy_match) {
           /* Set ma_litc[0]: this is the only way to identify the unmatched
            * data if the delayed match will be truncated to 1. It is also
            * useful if ma_length == 2: it may be more efficient in this case
            * to encode the individual characters rather than the match info.
            */
            ma->l.ma_litc[0] = window[strstart];
            start_length = ma_length;
            checkpoint = strstart + 1;
            return IM_OK;
        }
        /* At this point, ma_length >= 3, no need for ma_litc */
        ma->l.ma_length = ma_length;
        checkpoint = strstart + ma_length;
    } else {
        ma->ma_dist = 0;
        ma->l.ma_litc[0] = window[strstart]; /* ma_litc[1] is not required */
        checkpoint = strstart + 1;
    }
    return ct_tally (ma);
    /* Keep start_length == 1 */
}

/***********************************************************************
 *
 * Determine the minimum match length, based on the type of data
 * in the given input buffer: 2 for binary data, 3 otherwise. Set also
 * h_shift according to the chosen min_match_length, and reduce
 * max_chain_length for binary files.
 *    If the guess about data type is wrong, this only affects the
 * compression ratio and speed but not the correctness of the algorithms.
 * If there are more than 20% bytes which seem non ascii in the first
 * 500 bytes, we assume that the data is binary. (We accept data
 * with a few high bits set as ascii to take into account special
 * word processor formats.)
 */
static void set_min_match_length (block, count)
    U_CHAR *block;          /* input data */
    U_INT  count;           /* # of input char's */
{
    int non_ascii = 0;
    min_match_length = 3;  /* Default ascii */
    if (count >= 500) {
        count = 500;
        while (--count != 0) {
            if (*block <= 6 || *block >= 0x80) non_ascii++;
            block++;
        }
        if (non_ascii > 100) {
            min_match_length = 2;
            max_chain_length >>= 2;
        }
    }
    h_shift = (HASH_BITS+min_match_length-1)/min_match_length;
#ifdef DEBUG
    fprintf(stderr," (min_match_length %d) ", min_match_length);
#endif
}

/***********************************************************************
 *
 * Insert string s in the dictionary and set last_match to the previous head
 * of the hash chain (the most recent string with same hash key).
 * IN  assertion: all calls to to INSERT_STRING are made with consecutive
 *    input characters, so that a running hash key can be computed from the
 *    previous key instead of complete recalculation each time.
 */
#define INSERT_STRING(s, last_match) \
{ \
    ins_h = ((ins_h<<h_shift) ^ window[s + min_match_length-1]) & HASH_MASK; \
    prev[s] = last_match = match_head[ins_h]; \
    next[last_match] = prev[next[s] = ins_h + MAX_DIST+1] = s; \
}
    /* next[NIL] is garbage, we can overwrite it if s is a tail */

/***********************************************************************
 *
 * Remove string s from the dictionary, or do nothing if s is not yet
 * in the dictionary.
 * IN assertion: s is the tail of its hash chain (the oldest string).
 */
#define DELETE_STRING(s)  {prev[next[s]] = NIL;}
/* No effect if next[s] == NIL (s not in dictionary) */

/***********************************************************************
 *
 * Find the longest match starting at the given string. Return its position
 * and set its length in match_length. Matches shorter or equal to
 * start_length are discarded, in which case match_length is unchanged
 * and the result position is NIL.
 * IN assertions: cur_match is the head of the hash chain for the current
 *   string (strstart) and is not NIL, and start_length >= 1
 */
#if !defined(MSDOS) || defined(NO_ASM)
/* For MSDOS, a version of this routine written in assembler is in im_lm.asm.
 * The algorithms are strictly equivalent, so the C version can be used
 * instead if you do not have masm or tasm. (Update the makefile in this case.)
 */
IPos longest_match(cur_match)
    IPos cur_match;
{
    register U_CHAR *match;                   /* pointer in matched string */
    register U_CHAR *scan = window + strstart;/* pointer in current string */
    register int len;                         /* length of current match */
    IPos cur_best = NIL;                      /* best match so far */
    register int ma_length = start_length;    /* best match length so far */
    int chain_count = max_chain_length;       /* used to limit hash chains */
    typedef unsigned short US;
    typedef unsigned long  UL;
#ifdef UNALIGNED_OK
    register US scan_start = *(US*)scan;
    register US scan_end   = *(US*)(scan+ma_length-1);
#else
    register U_CHAR scan_start = *scan;
    register U_CHAR scan_end1  = scan[ma_length-1];
    register U_CHAR scan_end   = scan[ma_length];
#endif
    do {
        match = window + cur_match;
        /* Skip to next match if the match length cannot increase
         * or if the match length is less than 2:
         */
#ifdef UNALIGNED_OK
        /* This code assumes sizeof(unsigned short) == 2 and
         * sizeof(unsigned long) == 4. Do not use UNALIGNED_OK if your
         * compiler uses different sizes.
         */
        if (*(US*)(match+ma_length-1) != scan_end ||
            *(US*)match != scan_start) continue;

        len = min_match_length - 4;
        /* If min_match_length == 3, it is not necessary to compare
         * scan[2] and match[2] since they are always equal when the other
         * bytes match, given that the hash keys are equal and that
         * HASH_BITS >= 8.
         */
#       define ML MAX_MATCH_LENGTH
        do {} while ((len+=4) < ML && *(UL*)(scan+len) == *(UL*)(match+len));

        if (*(US*)(scan+len) == *(US*)(match+len)) len += 2;
        if (scan[len] == match[len]) len++;

#else /* UNALIGNED_OK */
        if (match[ma_length] != scan_end ||
            match[ma_length-1] != scan_end1 || *match != scan_start)
           continue;
        /* It is not necessary to compare scan[1] and match[1] since they
         * are always equal when the other bytes match, given that
         * the hash keys are equal and that h_shift+8 <= HASH_BITS,
         * that is, when the last byte is entirely included in the hash key.
         * The condition is equivalent to
         *       (HASH_BITS+2)/3 + 8 <= HASH_BITS
         * or: HASH_BITS >= 13 (see set_min_match_length()).
         * Also, we check for a match at ma_length-1 to get rid quickly of
         * the match with the suffix of the match made at the previous step,
         * which is known to fail.
         */
        len = 1;
        do {} while (++len < MAX_MATCH_LENGTH && scan[len] == match[len]);

#endif /* UNALIGNED_OK */

        if (len > ma_length) {
            cur_best = cur_match, ma_length = len;
            if (len >= strsize) break;
#ifdef UNALIGNED_OK
            scan_end = *(US*)(scan+ma_length-1);
#else
            scan_end1  = scan[ma_length-1];
            scan_end   = scan[ma_length];
#endif
        }
    } while (--chain_count != 0 && (cur_match = prev[cur_match]) != NIL);

    if (ma_length > start_length) match_length = ma_length;
    return cur_best;
}
#endif /* MSDOS */

/***********************************************************************
 *
 * Process a block of input characters, generating zero or more match
 * info records as appropriate.
 * IN assertion: count <= BSZ
 */
ImpErr lm_input (block, count)
    U_CHAR *block;          /* input data */
    U_INT  count;           /* # of input char's */
{
    if (count == 0) return IM_OK;

    /* Determine the input file type if this is the first call */
    if (min_match_length == 0) set_min_match_length (block, count);

    if (insert_point + count <= sizeof(window)) {
        memcpy((char*)window + insert_point, (char*)block, count);

    } else {
        int remain = sizeof(window)-insert_point;
        memcpy((char*)window + insert_point, (char*)block, remain);

        memcpy((char*)window + MAX_MATCH_LENGTH,
               (char*)block + remain, count - remain);
    }
    insert_point += count;
    if (insert_point > MAX_DIST) {
        /* Duplicate the end of the window */
        memcpy((char*)window,
               (char*)window + MAX_DIST,
               MIN (insert_point - MAX_DIST, MAX_MATCH_LENGTH));
    }
    if (insert_point >= sizeof(window)) insert_point -= MAX_DIST;

    return lm_process(count);
}

/***********************************************************************
 *
 * Process a block of characters already inserted in the window
 * IN assertion: count > 0
 */
#if !defined(MSDOS) || defined(NO_ASM)
ImpErr lm_process (count)
    U_INT  count;           /* number of bytes to process */
{
    ImpErr retcode;         /* as usual */
    IPos cur_match;         /* starting point for longest match search */
    IPos best_match = NIL;  /* longest match found */
    int delete_point;       /* position of next string to remove */
    
    delete_point = strstart - bufsize + MAX_MATCH_LENGTH - 1;
    if (delete_point < 0) delete_point += MAX_DIST;

    /* Process the input block. */
    do {
        /* Insert the string window[strstart .. strstart+strsize-1] in the
         * dictionary, and set cur_match to the head of the hash chain:
         */
        INSERT_STRING(strstart, cur_match);

        if (strstart == checkpoint) {
            /* Find the longest match, discarding those <= start_length */
            match_length = 0;
            if (cur_match != NIL) {
                best_match = longest_match (cur_match);
                /* longest_match updates match_length if longer match found */
            }
            retcode = write_match (best_match, match_length);
            if (retcode != IM_OK) return retcode;
        }

        /* Remove the oldest string from the dictionary, except if we have not
         * yet created bufsize dictionary entries. We could avoid this
         * deletion and check instead for obsolete pointers in
         * longest_match(), but this would be slower.
         */
#if (MAX_DIST & (MAX_DIST-1)) != 0
        if (++delete_point == MAX_DIST) delete_point = 0;
#else
        delete_point = (delete_point + 1) & (MAX_DIST-1);
#endif
        DELETE_STRING (delete_point);

        if (++strstart == MAX_DIST) {
            strstart = 0, checkpoint -= MAX_DIST;
        }
    } while (--count != 0);
    return IM_OK;
}
#endif /* MSDOS */

/***********************************************************************
 *
 * Wind up processing by flushing unprocessed input. For normal processing,
 * this routine is called twice (by imp_size then imp_clear) and the
 * second call does nothing. In case of error, this routine is called only
 * by imp_clear().
 */
ImpErr lm_windup()
{
    ImpErr retcode;
    int matches;

    /* Process the remaining input. */
    while (strsize > 0) {
       retcode = lm_process (1);
       if (retcode != IM_OK) return retcode;
       --strsize;
    }
    /* Flush the match buffer. */
    if ((matches = ma-ma_buf+1) != 0 && matches != 
        twrite ((char *) ma_buf, sizeof(MATCH), matches, fd.fd_temp)) {
        return IM_IOERR;
    }
    ma = ma_buf - 1;
    return IM_OK;
}
