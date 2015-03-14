/*
 * The dialing directory structure.  The first eight elements are
 * contained in the pcomm.dial_dir file.
 */

#define NUM_DIR		100
#define NUM_QUEUE	10
#define FAST		0
#define SLOW		1
#define QUIET		0
#define VERBOSE		1

struct DIAL_DIR {
	char	*name[NUM_DIR+1];	/* name of system being called */
	char	*number[NUM_DIR+1];	/* phone number */
	int	baud[NUM_DIR+1];	/* baud rate */
	char	parity[NUM_DIR+1];	/* parity */
	int	dbits[NUM_DIR+1];	/* data bits */
	int	sbits[NUM_DIR+1];	/* stop bits */
	char	duplex[NUM_DIR+1];	/* duplex (F = full, H = half) */
	char	*script[NUM_DIR+1];	/* script name (or TTY) */

	int	q_num[NUM_QUEUE];	/* entry numbers in the queue */
	char	q_ld[NUM_QUEUE];	/* LD codes in the queue */

	int	d_entries;		/* number of entries in the file */
	int	d_cur;			/* the current entry */

	char	*d_path;		/* path to the pcomm.dial_dir file */
};

#ifndef MAIN
extern struct DIAL_DIR *dir;
#endif /* MAIN */
