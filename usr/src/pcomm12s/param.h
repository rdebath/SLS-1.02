/*
 * The standard Pcomm parameters.  Everything can be altered by using one
 * of Pcomm's menus.  Although editing by hand is not encouraged, the
 * pcomm.param file is just an ASCII file.
 */

#define MAX_CDELAY	120
#define MIN_CDELAY	10
#define MAX_PAUSE	120
#define MIN_PAUSE	1
#define MAX_TIMER	20
#define MIN_TIMER	2

#define NUM_PARAM	44
#define LINE_SET	0
#define TERM_SETUP	4
#define GEN_SETUP	10
#define DELAY_TIMES	19
#define ASCII_SETUP	21
#define LD_CODES	30
#define MACROS		34

struct PARAM {
				/* 0-3 used in ls_menu() */
	int	d_baud;			/* default baud rate */
	char	d_parity;		/* default parity */
	int	d_dbits;		/* default data bits */
	int	d_sbits;		/* default stop bits */

				/* 4-9 used in term_setup() */
	int	hot;			/* the decimal code for the hot key */
	char	*ascii_hot;		/* ascii representation of hot key */
	char	*d_duplex;		/* default duplex */
	char	*flow;			/* flow control */
	char	*cr_in;			/* send as carriage return */
	char	*cr_out;		/* receive carriage return as */

				/* 10-18 used in gen_setup() */
	char	*logfile;		/* default log file */
	char	*dumpfile;		/* default screen dump file */
	char	*strip;			/* strip high bit (translate table) */
	char	pause_char;		/* pause char synonym */
	char	cr_char;		/* carriage return char synonym */
	char	ctrl_char;		/* ctrl char synonym */
	char	esc_char;		/* escape char synonym */
	char	brk_char;		/* modem break synonym */
	char	*abort;			/* destination of aborted downloads */

				/* 19-20 used in gen_setup() & delay_times() */
	int	c_delay;		/* connect delay time */
	int	r_delay;		/* redial delay time */

				/* 21-29 used in axfer_setup() */
	char	*lecho;			/* echo locally? */
	char	*expand;		/* expand blank lines? */
	int	cr_delay;		/* carriage return delay (ms) */
	char	*pace;			/* pace the output? */
	char	*cr_up;			/* send carriage return as */
	char	*lf_up;			/* send line feed as */
	int	timer;			/* transfer timeout */
	char	*cr_dn;			/* receive carriage return as */
	char	*lf_dn;			/* receive line feed as */

				/* 30-33 used in d_revise() */
	char	*ld_plus;		/* + long distance code */
	char	*ld_minus;		/* - long distance code */
	char	*ld_at;			/* @ long distance code */
	char	*ld_pound;		/* # long distance code */

				/* 34-43 used in macro() */
	char	*mac_1;			/* shifted 1 macro */
	char	*mac_2;			/* shifted 2 macro */
	char	*mac_3;			/* shifted 3 macro */
	char	*mac_4;			/* shifted 4 macro */
	char	*mac_5;			/* shifted 5 macro */
	char	*mac_6;			/* shifted 6 macro */
	char	*mac_7;			/* shifted 7 macro */
	char	*mac_8;			/* shifted 8 macro */
	char	*mac_9;			/* shifted 9 macro */
	char	*mac_0;			/* shifted 0 macro */

	char	*p_path;		/* path to the pcomm.param file */
};

#ifndef MAIN
extern struct PARAM *param;
#endif /* MAIN */
