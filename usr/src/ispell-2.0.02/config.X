/*
 * This is the configuration file for ispell.  Thanks to Bob McQueer
 * for creating it and making the necessary changes elsewhere to
 * support it.
 * Look through this file from top to bottom, and edit anything that
 * needs editing.  There are also five or six variables in the
 * Makefile that you must edit.  Note that the Makefile edits this
 * file (config.X) to produce config.h.  If you are looking at
 * config.h, you're in the wrong file.
 *
 * Don't change the funny-looking lines with !!'s in them;  see the
 * Makefile!
 */

/*
** library directory for hash table(s) / default hash table name
** If you intend to use multiple dictionary files, I would suggest
** LIBDIR be a directory which will contain nothing else, so sensible
** names can be constructed for the -d option without conflict.
*/
#ifndef LIBDIR
#define LIBDIR "!!LIBDIR!!"
#endif
#ifndef DEFHASH
#define DEFHASH "!!DEFHASH!!"
#endif

#ifdef USG
#define index strchr
#define rindex strchr
#endif

/* environment variable for user's word list */
#ifndef PDICTVAR
#define PDICTVAR "WORDLIST"
#endif

/* default word list */
#ifndef DEFPDICT
#define DEFPDICT ".ispell_words"
#endif

/* environment variable for include file string */
#ifndef INCSTRVAR
#define INCSTRVAR "INCLUDE_STRING"
#endif

/* default include string */
#ifndef DEFINCSTR
#define DEFINCSTR "&Include_File&"
#endif

/* mktemp template for temporary file - MUST contain 6 consecutive X's */
#ifndef TEMPNAME
#define TEMPNAME "/tmp/ispellXXXXXX"
#endif

/* default dictionary file */
#ifndef DEFDICT
#define DEFDICT "!!DEFDICT!!"
#endif

/* path to LOOK (if look(1) command is available) */
#ifndef LOOK
#define LOOK "/usr/bin/look"
#endif

/* path to egrep (use speeded up version if available) */
#ifndef EGREPCMD
#define EGREPCMD "/usr/bin/egrep"
#endif

/* path to wordlist for Lookup command (typically /usr/dict/{words|web2} */
#ifndef WORDS
#define WORDS	"/usr/dict/words"   /* "/usr/dict/{words,web2}" */
#endif

/* buffer size to use for file names if not in sys/param.h */
#ifndef MAXPATHLEN
#define MAXPATHLEN 240
#endif

/* word length allowed in dictionary by buildhash */
#define WORDLEN 30

/* suppress the 8-bit character feature */
#ifndef NO8BIT
#define NO8BIT
#endif

/* maximum number of include files supported by xgets;  set to 0 to disable */
#ifndef MAXINCLUDEFILES
#define MAXINCLUDEFILES	5
#endif

/* Approximate number of words in the full dictionary, after munching.
** Err on the high side unless you are very short on memory, in which
** case you might want to change the tables in tree.c and also increase
** MAXPCT.
**
** (Note:  dict.191 is a bit over 15000 words.  dict.191 munched with
** /usr/dict/words is a little over 28000).
*/
#ifndef BIG_DICT
#define BIG_DICT	29000
#endif

/*
** Maximum hash table fullness percentage.  Larger numbers trade space
** for time.
**/
#ifndef MAXPCT
#define MAXPCT	70		/* Expand table when 70% full */
#endif

/*
** the isXXXX macros normally only check ASCII range.  These are used
** instead for text characters, which we assume may be 8 bit.  The
** NO8BIT ifdef shuts off significance of 8 bit characters.  If you are
** using this, and your ctype.h already masks, you can simplify.
*/
#ifdef NO8BIT
#define myupper(X) isupper((X)&0x7f)
#define mylower(X) islower((X)&0x7f)
#define myspace(X) isspace((X)&0x7f)
#define myalpha(X) isalpha((X)&0x7f)
#else
#define myupper(X) (!((X)&0x80) && isupper(X))
#define mylower(X) (!((X)&0x80) && islower(X))
#define myspace(X) (!((X)&0x80) && isspace(X))
#define myalpha(X) (!((X)&0x80) && isalpha(X))
#endif

/*
** the NOPARITY mask is applied to user input characters from the terminal
** in order to mask out the parity bit.
*/
#ifdef NO8BIT
#define NOPARITY 0x7f
#else
#define NOPARITY 0xff
#endif


/*
** the terminal mode for ispell, set to CBREAK or RAW
**
*/
#ifndef TERM_MODE
#define TERM_MODE	CBREAK
#endif

/*
** Define this if you want your columns of words to be of equal length.
** This will spread short word lists across the screen instead of down it.
*/
#ifndef EQUAL_COLUMNS
#undef EQUAL_COLUMNS
#endif

/*
** This is the extension that will be added to backup files
*/
#ifndef	BAKEXT
#define	BAKEXT	".bak"
#endif

/*
** Define this if you want the capitalization feature.  This will increase
** the size of the hashed dictionary on most 16-bit and some 32-bit machines.
*/
#ifndef CAPITALIZE
#define CAPITALIZE
#endif

/*
** Define this if you want your personal dictionary sorted.  This may take
** a long time for very large dictionaries.  Dictionaries larger than
** SORTPERSONAL words will not be sorted.
*/
/* Commented out because 'I' doesn't work the second time if you have
   this - clobbers dictionary leaving only latest word there (why?)
#define SORTPERSONAL	1000
*/
#ifndef SORTPERSONAL
#undef SORTPERSONAL
#endif
