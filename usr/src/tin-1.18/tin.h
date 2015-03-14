/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : tin.h
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 05-12-92
 *  Notes     : #include files, #defines & struct's
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

/*
 * OS specific doda's
 */
#include	"config.h"

#include	<stdio.h>
#include	<signal.h>
#include	<errno.h>

#ifdef AMIGA
#	include	<stat.h>
#	include	<stddef.h>
#	include	<time.h>
#else
#	ifdef apollo
#		include </bsd4.3/usr/include/sys/types.h>
#		include </bsd4.3/usr/include/sys/stat.h>
#		include </bsd4.3/usr/include/time.h>
#	else
#		include <sys/types.h>
#		include	<sys/stat.h>
#		include	<sys/time.h>
#		include	<time.h>
#		ifdef HAVE_UNISTD_H
#			include	<unistd.h>
#		endif
#	endif
#	include	<pwd.h>
#	include <sys/param.h>
#endif

#include	<ctype.h>

#ifdef HAVE_STDLIB_H
#	include	<stdlib.h>
#endif

#ifdef HAVE_STRINGS_H
#	include	<strings.h>
#else
#	include	<string.h>
#endif

#ifdef HAVE_FCNTL_H
#	include	<fcntl.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#	include <sys/ioctl.h>
#endif

#ifdef HAVE_PROTOTYPES_H
#	include	<prototypes.h>
#endif

#ifdef HAVE_LOCALE_H
#	include <locale.h>
#endif

#ifdef HAVE_SYS_UTSNAME_H
#	include	<sys/utsname.h>
#endif

/*
 * Needed for catching child processes
 */

#ifdef HAVE_SYS_WAIT_H
#	include	<sys/wait.h>
#endif

/*
 * Needed for timeout in user abort of indexing a group
 */

#ifdef HAVE_SYS_WAIT_H
#	include	<sys/wait.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#	include	<sys/select.h>
#endif

/*
 * Needed for resizing under an xterm
 */

#if defined(SIGWINCH) && !defined(DONT_HAVE_SIGWINCH)
#	ifdef HAVE_TERMIOS_H
#		include <termios.h>
#	endif
#	if !defined(TIOCGWINSZ) && !defined(TIOCGSIZE)
#		ifdef HAVE_SYS_STREAM_H
#			include <sys/stream.h>
#		endif
#		ifdef HAVE_TERMIO_H
#			include <termio.h>
#		else
#			ifdef HAVE_SYS_PTEM_H
#				include <sys/ptem.h>
#				include <sys/tty.h>
#			endif
#			ifdef HAVE_SYS_PTY_H
#				if !defined(_h_BSDTYPES) && !defined(DONT_HAVE_SYS_BSDTYPES_H)
#					include <sys/bsdtypes.h>
#				endif
#				include <sys/pty.h>
#			endif
#		endif
#	endif
#endif

/*
 * Directory handling code - Hopefully one of these is right for you. 
 */

#ifdef BSD
#	ifdef sinix
#		include <dir.h>
#	else
#		ifdef __arm
#			include <dirent.h>
#			define	DIR_BUF	struct dirent
#		else
#			include <sys/dir.h>
#		endif
#	endif
#	ifndef DIR_BUF
#		define	DIR_BUF		struct direct
#	endif
#	define		D_LENGTH	d_namlen
#endif
#ifdef M_XENIX
#	include <sys/ndir.h>
#	define		DIR_BUF		struct direct
#	define		D_LENGTH	d_namlen
#endif
#ifdef AMIGA
#	include	"amiga.h"
#	define		DIR_BUF		struct dirent
#	define		D_LENGTH	d_reclen
#endif
#ifndef DIR_BUF
#	include	<dirent.h>
#	define		DIR_BUF		struct dirent
#	define		D_LENGTH	d_reclen
#endif

/*
 * Setup support for reading from CD-ROM
 */

#ifdef CDROM_ONLY
#	define CDROM_ABLE
#endif
 
#ifdef CDROM_ABLE
#	define XSPOOLDIR
#	define NNTP_ABLE
#	undef NNTP_ONLY
#	undef NNTP_INEWS
#endif

/*
 * Setup support for reading from NNTP
 */
 
#if defined(NNTP_ABLE) || defined(NNTP_ONLY)
#	ifndef NNTP_ABLE
#		define	NNTP_ABLE
#	endif
#	ifndef NNTP_INEWS
#		define	NNTP_INEWS
#	endif
#endif

#ifdef AMIGA
#	define	TMPDIR "T:"
#else
#	define	TMPDIR "/tmp/"
#endif

/*
 * Index file daemon version of tin. Will create/update index files from cron
 * on NNTP server machine so clients can retreive index file with NNTP XINDEX
 * command from server. Also can be used on machines that just want one copy
 * of all the index files in one place. In this case the normal tin must have
 * access to the index directory (-I dir option) or be setuid news.
 */
 
#ifdef INDEX_DAEMON
#	define	DONT_HAVE_SELECT
#	define	LOCK_FILE "tind.LCK"
#	undef	NNTP_ABLE
#	undef	NNTP_ONLY
#	undef	NNTP_INEWS
#endif

/*
 * Specify News spool & control directories
 */
 
#ifndef SPOOLDIR
#	define		SPOOLDIR	"/usr/spool/news"
#endif
#ifndef LIBDIR
#	define		LIBDIR		"/usr/lib/news"
#endif
#ifndef INEWSDIR
#	define		INEWSDIR	LIBDIR
#endif

/*
 * Determine machine configuration for external programs & directories
 */

#ifdef BSD
#	ifdef DONT_HAVE_MEMCMP
#		define		memcmp(s1, s2, n)	bcmp(s2, s1, n)
#	endif
#	define		strchr(str, ch)		index(str, ch)
#	define		strrchr(str, ch)	rindex(str, ch)
#	define		DEFAULT_SHELL		"/bin/csh"
#	ifdef BSDI
#		define	DEFAULT_EDITOR  "/usr/bin/vi"
#		define	DEFAULT_PRINTER "/usr/bin/lpr"
#		define	DEFAULT_MAILER  "/usr/sbin/sendmail"
#		define	DEFAULT_MAILBOX "/var/mail"
#	else
#		define	DEFAULT_EDITOR  "/usr/ucb/vi"
#		define	DEFAULT_PRINTER "/usr/ucb/lpr"
#	endif
#	define		DEFAULT_SUM		"sum"
#	ifdef DGUX
#		define	DEFAULT_MAILBOX	"/usr/mail"
#		define	USE_INVERSE_HACK
#	endif
#	ifdef pyr
#		define	DEFAULT_MAILER	"/usr/.ucbbin/mail"
#	endif
#	ifndef DEFAULT_MAILER
#		define	DEFAULT_MAILER	"/usr/lib/sendmail"
#	endif
#	ifndef DEFAULT_MAILBOX
#		define	DEFAULT_MAILBOX	"/usr/spool/mail"
#	endif
#else
#	if defined(NCR) || defined(atthcx) || defined(__hpux) || defined(PTX) || \
	   defined(sinix)
#		define	DEFAULT_MAILER	"/usr/bin/mailx"
#	endif
#	ifdef AMIGA
#		define	DEFAULT_EDITOR	"c:ed"
#		define	DEFAULT_MAILER	"uucp:c/sendmail"
#		define	DEFAULT_MAILBOX	"uumail:"
#		define	DEFAULT_PRINTER	"c:type"	/* Not Yet Implemented */
#		define	DEFAULT_SHELL	"c:newshell"
#	endif
#	ifdef M_XENIX
#		define	DEFAULT_EDITOR	"/bin/vi"
#		define	DEFAULT_MAILER	"/usr/bin/mail"
#		define	DEFAULT_MAILBOX	"/usr/spool/mail"
#	endif
#	ifdef RS6000
#		define	DEFAULT_MAILER	"/usr/lib/sendmail"
#		define	DEFAULT_PRINTER	"/bin/lp"
#	endif
#	ifdef SCO_UNIX
#		define	HAVE_MMDF_MAILER
#	endif
#	ifdef sinix
#		define	DEFAULT_PRINTER	"/bin/lpr"
#	endif
#	ifdef sysV68
#		define	DEFAULT_MAILER	"/bin/rmail"
#	endif
#	ifdef UNIXPC
#		define	DEFAULT_MAILER	"/bin/rmail"
#	endif
#	ifndef DEFAULT_SHELL
#		define	DEFAULT_SHELL	"/bin/sh"
#	endif
#	ifndef DEFAULT_EDITOR
#		define	DEFAULT_EDITOR	"/usr/bin/vi"
#	endif
#	ifndef DEFAULT_MAILBOX
#		define	DEFAULT_MAILBOX	"/usr/mail"
#	endif
#	ifndef DEFAULT_MAILER
#		define	DEFAULT_MAILER	"/bin/mail"
#	endif
#	ifndef DEFAULT_PRINTER
#		define	DEFAULT_PRINTER	"/usr/bin/lp"
#	endif
#	define		DEFAULT_SUM		"sum -r"
#endif

#ifdef HAVE_LONG_FILENAMES
#	define		LONG_PATH_PART	"part"
#	define		LONG_PATH_PATCH	"patch"
#else
#	define		LONG_PATH_PART	""
#	define		LONG_PATH_PATCH	"p"
#endif

/*
 * Useful for logging user usage
 */
 
#define		LOG_USER_FILE	".tin_log" 

/*
 * Should active file be reread for new news & if so how often
 */
 
#ifdef DONT_REREAD_ACTIVE_FILE
#	define	REREAD_ACTIVE_FILE_SECS	0
#else
#	ifndef REREAD_ACTIVE_FILE_SECS
#		define	REREAD_ACTIVE_FILE_SECS	300	/* seconds */
#	endif
#endif

/*
 * Initial sizes of internal arrays for small (<4MB) & large memory machines
 */
 
#ifdef SMALL_MEMORY_MACHINE
#	define		DEFAULT_ACTIVE_NUM	1800	
#	define		DEFAULT_ARTICLE_NUM	400	
#	define		DEFAULT_KILL_NUM	10	
#	define		DEFAULT_SAVE_NUM	10	
#	define		DEFAULT_SPOOLDIR_NUM	5	
#	define		DEFAULT_ACTIVE_SIZE_NUM	5	
#else
#	define		DEFAULT_ACTIVE_NUM	1800	
#	define		DEFAULT_ARTICLE_NUM	1200	
#	define		DEFAULT_KILL_NUM	30	
#	define		DEFAULT_SAVE_NUM	30	
#	define		DEFAULT_SPOOLDIR_NUM	10	
#	define		DEFAULT_ACTIVE_SIZE_NUM	10	
#endif

#define		RCDIR			".tin"
#define		RCFILE			"tinrc"
#define		INDEX_MAILDIR		".mailidx"
#define		INDEX_NEWSDIR		".index"
#define		ACTIVE			"active"
#define		ACTIVE_MAIL		"active.mail"
#define		ACTIVE_SAVE		"active.save"
#define		ACTIVE_TIMES		"active.times"
#define		KILLFILE		"kill"
#define		POSTFILE		"posted"
#define		DEFAULT_MAILDIR		"Mail"
#define		DEFAULT_SAVEDIR		"News"
#define		MAILGROUPS_FILE		"mailgroups"

#ifdef TRUE
#	undef TRUE
#endif
#define		TRUE		1

#ifdef FALSE
#	undef FALSE
#endif
#define		FALSE		0

#ifdef AMIGA
#define		LEN			512
#define		PATH_LEN		128
#else
#ifndef MAXPATHLEN
#   define MAXPATHLEN 256
#endif
#define		PATH_LEN		MAXPATHLEN
#define		LEN			1024
#endif
#define		NEWSRC_LINE		8192

#ifdef HAVE_MAIL_HANDLER
#	define	HEADER_LEN		8192
#else
#	define	HEADER_LEN		1024
#endif
#define		MODULO_COUNT_NUM	5
#define		TABLE_SIZE		1409
#define		MAX_PAGES		1000
#define		ctrl(c)			((c) & 0x1F)

#ifndef DEFAULT_COMMENT
#	define		DEFAULT_COMMENT	": "	/* used when by follow-ups & replys */
#endif
#ifndef UNREAD_ART_MARK
#	define		UNREAD_ART_MARK	'+'	/* used to show that an art is unread */
#endif
#ifndef RETURN_ART_MARK
#	define		RETURN_ART_MARK	'-'	/* used to show that an art will return */
#endif
#ifndef HOT_ART_MARK
#	define		HOT_ART_MARK	'*'	/* used to show that an art was auto selected */
#endif
#ifndef READ_ART_MARK
#	define		READ_ART_MARK	' '	/* used to show that an art was not read or seen  */
#endif

#ifdef USE_INVERSE_HACK
#	define		SELECT_MISC_COLS	21
#	define		BLANK_GROUP_COLS	2
#	define		BLANK_PAGE_COLS		2
#else
#	define		SELECT_MISC_COLS	21
#	define		BLANK_GROUP_COLS	0
#	define		BLANK_PAGE_COLS		0
#endif

#define		SCREEN_READ_UNREAD		6		/* position for "  +" / "   " */
#define		INDEX_TOP			2

#ifdef NO_REGEX 
#	define STR_MATCH(s1,s2)	(str_str (s1, s2, strlen (s2)) != 0)
#else		
#	define STR_MATCH(s1,pat)	(wildmat (s1, pat))
#endif

#define IGNORE_ART(i)	((arts[i].thread == ART_EXPIRED) || \
			 (arts[i].killed && kill_level > 0))

/*
 *  News/Mail group types 
 */
 
#define		GROUP_TYPE_MAIL			0
#define		GROUP_TYPE_NEWS			1
#define		GROUP_TYPE_SAVE			2

/*
 *  used by get_arrow_key()
 */

#define		KEYMAP_UNKNOWN			0
#define		KEYMAP_UP			1
#define		KEYMAP_DOWN			2
#define		KEYMAP_PAGE_UP			3
#define		KEYMAP_PAGE_DOWN		4
#define		KEYMAP_HOME			5
#define		KEYMAP_END			6

/*
 *  used by feed_articles() & show_mini_help()
 */
 
#define		SELECT_LEVEL			1
#define		SPOOLDIR_LEVEL			2
#define		GROUP_LEVEL			3
#define		THREAD_LEVEL			4
#define		PAGE_LEVEL			5

#define		MINI_HELP_LINES			5

#define		FEED_MAIL			1
#define		FEED_PIPE			2
#define		FEED_PRINT			3
#define		FEED_SAVE			4
#define		FEED_XPOST			5

/*
 *  used in art.c & rcfile.c
 */
 
#define		SORT_BY_NOTHING			0		/* sort types on arts[] array */
#define		SORT_BY_SUBJ_DESCEND		1
#define		SORT_BY_SUBJ_ASCEND		2
#define		SORT_BY_FROM_DESCEND		3
#define		SORT_BY_FROM_ASCEND		4
#define		SORT_BY_DATE_DESCEND		5
#define		SORT_BY_DATE_ASCEND		6

#define		SHOW_FROM_NONE			0
#define		SHOW_FROM_ADDR			1
#define		SHOW_FROM_NAME			2
#define		SHOW_FROM_BOTH			3

/*
 *  used in help.c
 */

#define		HELP_INFO			0
#define		POST_INFO			1

/*
 *  used in save.c
 */

#define		CHECK_ANY_NEWS			0
#define		START_ANY_NEWS			1
#define		MAIL_ANY_NEWS			2
#define		SAVE_ANY_NEWS			3

/*
 *  used in help.c
 */

#define		HEADER_TO			0
#define		HEADER_SUBJECT			1
#define		HEADER_NEWSGROUPS		2

/*
 *  used in page.c & post.c
 */

#define		POSTED_NONE			0
#define		POSTED_REDRAW			1
#define		POSTED_OK			2

/*
 *  Assertion verifier
 */


#ifdef HAVE_ANSI_ASSERT
#	define	assert(p)	if(! (p)) asfail(__FILE__, __LINE__, #p); else
#else
#	define	assert(p)	if(! (p)) asfail(__FILE__, __LINE__, "p"); else
#endif

#define	ESC	27

#ifdef HAVE_CR_AS_CHAR
#	define		CR		'\r'
#else
#	define		CR		10
#endif

/*
 * return codes for change_rcfile ()
 */

#define		NO_KILLING		0
#define		KILLING			1

/*
 *  art.thread
 */

#define		ART_NORMAL		-1
#define		ART_EXPIRED		-2

/*
 *  art.unread
 */

#define		ART_READ		0
#define		ART_UNREAD		1
#define		ART_WILL_RETURN		2

#define		ART_UNAVAILABLE		-1

/*
 * used by group_t & my_group[]
 */
 
#define		UNSUBSCRIBED	0x01	/* haven't put in my_group[] yet */
#define		SUBSCRIBED	0x02	/* subscribed to */

/*
 * kill_type used in struct kill_t
 */
 
#define KILL_SUBJ	1
#define KILL_FROM	2
#define KILL_BOTH	3

/*
 * used in feed.c & save.c
 */
 
#define POST_PROC_NONE			0
#define POST_PROC_SHAR			1
#define POST_PROC_UUDECODE		2
#define POST_PROC_UUD_LST_ZOO		3
#define POST_PROC_UUD_EXT_ZOO		4
#define POST_PROC_UUD_LST_ZIP		5
#define POST_PROC_UUD_EXT_ZIP		6

/*
 *  struct article_t - article header
 *
 *  article.artnum:
 *	article number in spool directory for group
 *
 *  article.thread:
 *	-1 initial default
 *	-2 means article has expired (wasn't found in file search
 *	of spool directory for the group)
 *	>=0 points to another arts[] (struct article_t)
 *
 *  article.inthread:
 *	FALSE for the first article in a thread, TRUE for all
 *	following articles in thread
 *
 *  article.unread:
 *	boolean, has this article been read or not
 *
 *  article.killed:
 *	boolean, has this article been killed
 *
 *  article.hot:
 *	boolean, has this article been auto-selected
 *
 *  article.tagged:
 *	count, has this article been tagged for saving (>0) or not (=0)
 *
 *  article.date
 *	date: line used for sorting articles by date order
 *
 *  article.archive:
 *	archive name used in *source* groups
 *
 *  article.part:
 *	part no. of archive
 *
 *  article.patch:
 *	patch no. of archive
 *
 */

struct article_t {
	long artnum;
	char *subject;		/* Subject: line from mail header */
	char *from;		/* From: line from mail header (address) */
	char *name;		/* From: line from mail header (full name) */
	int thread;
	unsigned int inthread:1;/* 0 = thread head, 1 = thread follower */
	unsigned int unread:2;	/* 0 = read, 1 = unread, 2 = will return */
	unsigned int killed:1;	/* 0 = not killed, 1 = killed */
	unsigned int hot:1;	/* 0 = not hot, 1 = hot */
	unsigned int zombie:1;	/* 1 = was alive (unread) before 'X' command */
	unsigned int o_unread:2;/* original value of unread - used in xref */
	int tagged;		/* 0 = not tagged, >0 = tagged */
	long date;		/* Date: line from header in seconds */
	char *archive;		/* Archive-name: line from mail header */
	char *part;		/* part  no. of archive */
	char *patch;		/* patch no. of archive */
	char *xref;		/* Xref: cross posted article reference line */
};

/*
 *  struct attribute_t - configurable attributes on a per group basis
 */

struct attribute_t {
	char *maildir;				/* mail dir if other than ~/Mail */
	char *savedir;				/* save dir if other than ~/News */
	char *sigfile;				/* sig file if other than ~/.Sig */
	char *organization;			/* organization name */
	char *followup_to;			/* where posts should be redirected */
	char *printer;				/* printer command & parameters */
	unsigned int read_during_session:1;	/* marked TRUE if group entered during session */
	unsigned int auto_save:1;		/* 0=none, 1=save */
	unsigned int batch_save:1;		/* 0=none, 1=save -S/mail -M  */
	unsigned int delete_tmp_files:1;	/* 0=leave, 1=delete */
	unsigned int show_only_unread:1;	/* 0=all, 1=only unread */
	unsigned int thread_arts:1;		/* 0=unthread, 1=thread */
	unsigned int show_author:4;		/* 0=none, 1=name, 2=addr, 3=both */
	unsigned int sort_art_type:4;		/* 0=none, 1=subj descend, 2=subj ascend, 
						   3=from descend, 4=from ascend,
						   5=date descend, 6=date ascend */
	unsigned int post_proc_type:4;		/* 0=none, 1=shar, 2=uudecode, 
				  		   3=uud & list zoo, 4=uud & ext zoo*/
};

/*
 *  struct group_t - newsgroup info from active file
 */

struct group_t {
	char *name;			/* newsgroup / mailbox name */
	char *description;		/* text from LIBDIR/newsgroups file */
	char *spooldir;			/* groups spool directory */
	char moderated;			/* state of group moderation */
	long max;			/* max. article number */
	long min;			/* min. article number */
	int type;			/* grouptype - newsgroup / mailbox */
	int next;			/* next active entry in hash chain */
	int my_group;			/* subscribed/unsubscribed to group */
	int unread;			/* unread articles in group */
	struct attribute_t attribute;	/* group specific attributes */ 
#ifdef INDEX_DAEMON
	long last_updated_time;		/* last time group dir was changed */
#endif
};

/*
 *  used in hashstr.c
 */
 
struct hashnode {
	char *s;				/* the string we're saving */
	struct hashnode *next;			/* chain for spillover */
};

/*
 *  used in filter.c (new name for kill.c)
 *
 *  Create 2 filter arrays - global & local. Local will be part of group_t
 *  structure and will have priority over global filter. Should help to
 *  speed kill/selecting within a group. The long value number that is in
 *  ~/.tin/kill will be replaced by group name so that it is more human
 *  readable and that if hash routine is changed it will still work.
 *
 *  Add time period to filter_t struct to allow timed kills & auto-selection
 *  Default kill & select time 30 days. Store as a long and compare when 
 *  loading against present time. If time secs is passed set flag to save
 *  filter file and don't load expired entry. Renamed to filter because of
 *  future directions in adding other retrieval methods to present kill &
 *  hot selection.
 *
 *  Also seperate kill/select screen to allow ^K=kill ^A=auto-select
 */
 
struct kill_t {
	unsigned int kill_type:8;
	unsigned int kill_how:8;	/* kill/auto select */
	long kill_group;
	char *kill_subj;
	char *kill_from;
};

struct save_t {
	char *subject;
	char *dir;
	char *file;
	char *archive;
	char *part;
	char *patch;
	int index;	
	int saved;	
	int is_mailbox;	
};

struct screen_t {
	char *col;
};

struct posted_t {
	char date[10];
	char group[80];
	char action;
	char subj[120];
};

struct art_stat_t {
	int total;	/* total article count */
	int unread;	/* number of unread articles (does not include seen) arts */
	int seen;	/* number of seen articles (ART_WILL_RETURN) */
	int hot_total;	/* total hot count */
	int hot_unread; /* hot and unread */
	int hot_seen;	/* hot and seen */
	char art_mark;	/* mark to use for this thread - not used for groups */
};

/*
 * Used by spooldir command
 */
 
struct spooldir_t {
	int state;
	char *name;
	char *comment;
};

/*
 * Used for auto-detecting changes in active file size on different news servers
 */
 
struct active_size_t {
	char *server;
	char *attribute;
};

/* 
 * Time functions. 
 */

typedef struct _TIMEINFO {
    time_t	time;
    long	usec;
    long	tzone;
} TIMEINFO;

/*
 * Determine signal return type
 */
 
#ifdef HAVE_SIGTYPE_VOID
typedef void sigtype_t;
#endif

#ifdef HAVE_SIGTYPE_INT
typedef int sigtype_t;
#endif

#define MOTD_FILE		"motd"
#define SUBSCRIPTIONS_FILE	"subscriptions"

#ifdef AMIGA
#	define NEWSGROUPS_FILE		"newsdescrip"
#	define BUG_REPORT_ADDRESS	"mark@garden.equinox.gen.nz"
#	define REDIRECT_OUTPUT		"> NIL:"
extern joinpath (char *result, char *dir, char *file);
#else
#	define NEWSGROUPS_FILE		"newsgroups"
#	define BUG_REPORT_ADDRESS	"Iain.Lea%anl433.uucp@Germany.EU.net"
#	define REDIRECT_OUTPUT		"> /dev/null 2>&1"
#	define joinpath(result,dir,file)	sprintf (result,"%s/%s", dir, file)
#endif

/*
 *  function prototypes	& extern definitions	
 */ 

#include	"patchlev.h"
#include	"extern.h"
#include	"nntplib.h"
#include	"proto.h"
