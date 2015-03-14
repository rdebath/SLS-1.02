/* -*- Mode: Text -*- */

struct dent {
	struct dent *next;
	char *word;

	unsigned short used : 1;

/* bit fields for all of the flags */
	unsigned short v_flag : 1;
		/*
			"V" flag:
		        ...E --> ...IVE  as in CREATE --> CREATIVE
		        if # .ne. E, ...# --> ...#IVE  as in PREVENT --> PREVENTIVE
		*/
	unsigned short n_flag : 1;
		/*
			"N" flag:
			        ...E --> ...ION  as in CREATE --> CREATION
			        ...Y --> ...ICATION  as in MULTIPLY --> MULTIPLICATION
			        if # .ne. E or Y, ...# --> ...#EN  as in FALL --> FALLEN
		*/
	unsigned short x_flag : 1;
		/*
			"X" flag:
			        ...E --> ...IONS  as in CREATE --> CREATIONS
			        ...Y --> ...ICATIONS  as in MULTIPLY --> MULTIPLICATIONS
			        if # .ne. E or Y, ...# --> ...#ENS  as in WEAK --> WEAKENS
		*/
	unsigned short h_flag : 1;
		/*
			"H" flag:
			        ...Y --> ...IETH  as in TWENTY --> TWENTIETH
			        if # .ne. Y, ...# --> ...#TH  as in HUNDRED --> HUNDREDTH
		*/
	unsigned short y_flag : 1;
		/*
			"Y" FLAG:
			        ... --> ...LY  as in QUICK --> QUICKLY
		*/
	unsigned short g_flag : 1;
		/*
			"G" FLAG:
			        ...E --> ...ING  as in FILE --> FILING
			        if # .ne. E, ...# --> ...#ING  as in CROSS --> CROSSING
		*/
	unsigned short j_flag : 1;
		/*
			"J" FLAG"
			        ...E --> ...INGS  as in FILE --> FILINGS
			        if # .ne. E, ...# --> ...#INGS  as in CROSS --> CROSSINGS
		*/
	unsigned short d_flag : 1;
		/*
			"D" FLAG:
			        ...E --> ...ED  as in CREATE --> CREATED
			        if @ .ne. A, E, I, O, or U,
			                ...@Y --> ...@IED  as in IMPLY --> IMPLIED
			        if # .ne. E or Y, or (# = Y and @ = A, E, I, O, or U)
			                ...@# --> ...@#ED  as in CROSS --> CROSSED
			                                or CONVEY --> CONVEYED
		*/
	unsigned short t_flag : 1;
		/*
			"T" FLAG:
			        ...E --> ...EST  as in LATE --> LATEST
			        if @ .ne. A, E, I, O, or U,
			                ...@Y --> ...@IEST  as in DIRTY --> DIRTIEST
			        if # .ne. E or Y, or (# = Y and @ = A, E, I, O, or U)
			                ...@# --> ...@#EST  as in SMALL --> SMALLEST
			                                or GRAY --> GRAYEST
		*/
	unsigned short r_flag : 1;
		/*
			"R" FLAG:
			        ...E --> ...ER  as in SKATE --> SKATER
			        if @ .ne. A, E, I, O, or U,
			                ...@Y --> ...@IER  as in MULTIPLY --> MULTIPLIER
			        if # .ne. E or Y, or (# = Y and @ = A, E, I, O, or U)
			                ...@# --> ...@#ER  as in BUILD --> BUILDER
			                                or CONVEY --> CONVEYER
		*/
	unsigned short z_flag : 1;
		/*
			"Z FLAG:
			        ...E --> ...ERS  as in SKATE --> SKATERS
			        if @ .ne. A, E, I, O, or U,
			                ...@Y --> ...@IERS  as in MULTIPLY --> MULTIPLIERS
			        if # .ne. E or Y, or (# = Y and @ = A, E, I, O, or U)
			                ...@# --> ...@#ERS  as in BUILD --> BUILDERS
			                                or SLAY --> SLAYERS
		*/
	unsigned short s_flag : 1;
		/*
			"S" FLAG:
			        if @ .ne. A, E, I, O, or U,
			                ...@Y --> ...@IES  as in IMPLY --> IMPLIES
			        if # .eq. S, X, Z, or H,
			                ...# --> ...#ES  as in FIX --> FIXES
			        if # .ne. S,X,Z,H, or Y, or (# = Y and @ = A, E, I, O, or U)
			                ...# --> ...#S  as in BAT --> BATS
			                                or CONVEY --> CONVEYS
		*/
	unsigned short p_flag : 1;
		/*
			"P" FLAG:
			        if @ .ne. A, E, I, O, or U,
			                ...@Y --> ...@INESS  as in CLOUDY --> CLOUDINESS
			        if # .ne. Y, or @ = A, E, I, O, or U,
			                ...@# --> ...@#NESS  as in LATE --> LATENESS
			                                or GRAY --> GRAYNESS
		*/
	unsigned short m_flag : 1;
		/*
			"M" FLAG:
			        ... --> ...'S  as in DOG --> DOG'S
		*/

	unsigned short keep : 1;

#ifdef CAPITALIZE
	/*
	** if followcase is set, the actual word entry (dent->word)
	** is followed by one or more further strings giving exact
	** capitalizations.   The first byte after the uppercase word
	** gives the number of capitalizations.  Each capitalization
	** is preceded by the character "+" if it is to be kept, or
	** "-" if it is to be discarded from the personal dictionary.
	** For example, the entry "ITCORP\0\3+ITcorp\0+ITCorp\0+ItCorp\0"
	** gives various ways of writing my e-mail address.  If all-lowercase
	** is acceptable, an all-lower entry must appear.  Simple
	** capitalization, on the other hand, is signified by the "capitalize"
	** flag.
	**
	** Suffixes always match the case of the final character of a word.
	**
	** If "allcaps" is set, the other two flags must be clear.
	*/
	unsigned short allcaps : 1;	/* Word must be all capitals */
	unsigned short capitalize : 1;	/* Capitalize the word */
	unsigned short followcase : 1;	/* Follow capitalization exactly */
	/*
	** The following entries denote the flag values that are actually
	** to be kept for this dictionary entry.  They may differ if the
	** "a" command is used for a word that differs only in capitalization.
	*/
	unsigned short k_allcaps : 1;
	unsigned short k_capitalize : 1;
	unsigned short k_followcase : 1;
#endif

};

#define WORDLEN 30

struct hashheader {
	int magic;
	int stringsize;
	int tblsize;
};

/* hash table magic number */
#define MAGIC 2

	
/*
 * termcap variables
 */
#ifdef MAIN
# define EXTERN /* nothing */
#else
# define EXTERN extern
#endif

EXTERN char *tgetstr();
EXTERN char PC;	/* padding character */
EXTERN char *BC;	/* backspace if not ^H */
EXTERN char *UP;	/* Upline (cursor up) */
EXTERN char *cd;	/* clear to end of display */
EXTERN char *ce;	/* clear to end of line */
EXTERN char *cl;	/* clear display */
EXTERN char *cm;	/* cursor movement */
EXTERN char *dc;	/* delete character */
EXTERN char *dl;	/* delete line */
EXTERN char *dm;	/* delete mode */
EXTERN char *ed;	/* exit delete mode */
EXTERN char *ei;	/* exit insert mode */
EXTERN char *ho;	/* home */
EXTERN char *ic;	/* insert character */
EXTERN char *il;	/* insert line */
EXTERN char *im;	/* insert mode */
EXTERN char *ip;	/* insert padding */
EXTERN char *nd;	/* non-destructive space */
EXTERN char *vb;	/* visible bell */
EXTERN char *so;	/* standout */
EXTERN char *se;	/* standout end */
EXTERN int bs;
EXTERN int li, co;	/* lines, columns */

EXTERN char termcap[1024];
EXTERN char termstr[1024];	/* for string values */
EXTERN char *termptr;

EXTERN char rootword[BUFSIZ];
EXTERN struct dent *lastdent;

EXTERN char *hashstrings;
EXTERN struct hashheader hashheader;

extern int aflag;
extern int lflag;

EXTERN int erasechar;
EXTERN int killchar;

EXTERN char tempfile[200];

