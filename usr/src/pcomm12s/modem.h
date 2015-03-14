/*
 * The modem and TTY databases.  The first 3 elements make up the TTY
 * database, the next 16 make up the modem database.  A "tname" in common
 * with a "mname" link the two together.
 */

#define NUM_TTY		10
#define NUM_MODEM	10

struct MODEM {
	char	*tty[NUM_TTY];		/* TTY names */
	char	*tname[NUM_TTY];	/* modem name */
	int	init_sp[NUM_TTY];	/* initialization baud rate */

	char	*mname[NUM_MODEM];	/* modem name (matches tname above) */
	char	*init[NUM_MODEM];	/* initialization */
	char	*dial[NUM_MODEM];	/* dial command */
	char	*suffix[NUM_MODEM];	/* dialing command suffix */
	char	*hang_up[NUM_MODEM];	/* hang up the modem */
	char	auto_baud[NUM_MODEM];	/* should we sync baud rates? */
	char	*con_3[NUM_MODEM];	/* 300 baud connect message */
	char	*con_12[NUM_MODEM];	/* 1200 baud connect message */
	char	*con_24[NUM_MODEM];	/* 2400 baud connect message */
	char	*con_48[NUM_MODEM];	/* 4800 baud connect message */
	char	*con_96[NUM_MODEM];	/* 9600 baud connect message */
	char	*con_192[NUM_MODEM];	/* 19200 baud connect message */
	char	*no_con1[NUM_MODEM];	/* no connect #1 */
	char	*no_con2[NUM_MODEM];	/* no connect #2 */
	char	*no_con3[NUM_MODEM];	/* no connect #3 */
	char	*no_con4[NUM_MODEM];	/* no connect #4 */

	int	t_entries;		/* number of TTY entries */
	int	m_entries;		/* number of modem entries */
	int	t_cur;			/* current TTY entry number */
	int	m_cur;			/* current modem entry number */

	char	*m_path;		/* path to the pcomm.modem file */
};

#ifndef MAIN
extern struct MODEM *modem;
#endif /* MAIN */
