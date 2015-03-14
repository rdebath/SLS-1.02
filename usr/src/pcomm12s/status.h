/*
 * The status flags, and other various changeable things.  Obviously
 * the "config.h" file must appear before this file.
 */

#define MAX_ROW		64
#define MAX_COL		133
#define PATH		128

struct STATUS {
	int	fd;			/* file descriptor for TTY */
	int	dup_fd;			/* for duplicating input */
	int	add_lf;			/* add <CR> to <LF>? */
	int	log;			/* status of log option */
	int	print;			/* status of print option */
	char	log_path[PATH];		/* data logging file */
#ifdef SHAREDMEM
	int	clr;			/* flag to clear the screen */
	int	row;			/* cursor row position */
	int	col;			/* cursor column position */
	char	vs[MAX_ROW][MAX_COL];	/* the virtual screen */
#else /* SHAREDMEM */
	char	vs_path[PATH];		/* virtual screen file */
#endif /* SHAREDMEM */
};

#ifndef MAIN
extern struct STATUS *status;
#endif /* MAIN */
