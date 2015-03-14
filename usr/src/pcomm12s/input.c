/*
 * The input routines.  This program runs as a child process to the
 * Pcomm program.
 */

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#define MAIN
#include "config.h"
#include "misc.h"
#include "status.h"
#include "vcs.h"

#ifdef SHAREDMEM
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#endif /* SHAREDMEM */

jmp_buf i_jmp;
int vcs_param[NUM_VCS][5];		/* positional parameters */
int vcs_opt[NUM_VCS][10];		/* options unique to each VCS */
int vcs_codes[NUM_VCS][VCS_SIZE];	/* the VCS codes */
int vcs_leadin[NUM_VCS];		/* unique list of lead-in characters */
int num_leadin;				/* length of lead-in list */
int hold, max_row, max_col, skip_row;
FILE *logfp, *lprfp;
struct STATUS *status;

#ifdef SHAREDMEM
#define VROW	status->row
#define VCOL	status->col
#define VS	status->vs
#else /* SHAREDMEM */
int VROW, VCOL;
char VS[MAX_ROW][MAX_COL];
struct STATUS s;
#endif /* SHAREDMEM */

/*
 * Read the serial port and write the characters to the screen.  Watch
 * for signals from the parent process to toggle the fancy options.
 * Writes the characters received to a virtual screen buffer.
 */

main(argc, argv)
int argc;
char *argv[];
{
	FILE *popen();
	register int in_cnt, out_cnt;
	int got_sig();
	char c, *strcpy(), *bufp, in_buf[INPUT_BUF], out_buf[INPUT_BUF*2];
	void _exit(), exit(), vcs_table(), setbuf(), vs_putchar(), vs_clear();
#ifdef SHAREDMEM
	int shm_id;
	char *shmat();
	void perror();
#endif /* SHAREDMEM */
					/* set the trap for the signals */
	signal(SIGALRM, SIG_IGN);
	signal(SIGHUP,  SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGUSR1, got_sig);
	signal(SIGUSR2, got_sig);
	signal(SIGINT,  got_sig);
	signal(SIGTERM, got_sig);

	setbuf(stdout, (char *) NULL);
					/* for the curious... */
	if (argc == 1) {
		fprintf(stderr, "This is the input routine for the Pcomm program\n");
		fprintf(stderr, "It is not designed to be run as a separate program\n");
		exit(1);
	}

#ifdef SHAREDMEM
	shm_id = atoi(argv[1]);
	status = (struct STATUS *) shmat(shm_id, (char *) 0, 0);
	if ((int) status == -1) {
		perror("shmat");
		_exit(1);
	}
#else /* SHAREDMEM */
	status = &s;
#endif /* SHAREDMEM */
					/* load the VCS table */
	vcs_table();
	if (max_row > MAX_ROW)
		max_row = MAX_ROW;
	if (max_col > MAX_COL-1)
		max_col = MAX_COL-1;
					/* parse the command line */
#ifndef SHAREDMEM
	status->fd = atoi(argv[1]);
	status->dup_fd = atoi(argv[2]);
	status->add_lf = atoi(argv[3]);
	status->log = atoi(argv[4]);
	status->print = atoi(argv[5]);
	strcpy(status->log_path, argv[6]);
	strcpy(status->vs_path, argv[7]);
#endif /* SHAREDMEM */

	skip_row = 0;

#ifdef SHAREDMEM
	if (status->clr)
		skip_row = 1;
#else /* SHAREDMEM */
					/* read previous screen */
	if (!access(status->vs_path, 0))
		read_vs();
	else
		skip_row = 1;
#endif /* SHAREDMEM */

	hold = 0;
					/* start up file pointers */
	lprfp = (FILE *) NULL;
	logfp = (FILE *) NULL;

	switch (setjmp(i_jmp)) {
		case 0:			/* no signal */
			break;
		case 1:			/* toggle the data logging */
			status->log = status->log ? 0 : 1;
			break;
		case 2:			/* toggle the printer */
			status->print = status->print ? 0 : 1;
			break;
		case 3:			/* suspend the input */
			hold = hold ? 0 : 1;
#ifndef SHAREDMEM
			if (hold)
				write_vs();
#endif /* SHAREDMEM */
			break;
		case 4:			/* clean up and go home */
			if (status->log)
				fclose(logfp);
			if (status->print) {
				putc('\f', lprfp);
				pclose(lprfp);
			}
#ifdef SHAREDMEM
					/* detach shared memory */
			shmdt((char *) status);
#endif /* SHAREDMEM */
			_exit(0);
			break;
	}
					/* any signal will awaken pause() */
	if (hold)
#ifdef BSD
		sigpause(0);
#else /* BSD */
		pause();
#endif /* BSD */
					/* open or close the printer */
	if (status->print && lprfp == NULL)
		lprfp = popen(LPR, "w");

	if (!status->print && lprfp != NULL) {
		putc('\f', lprfp);
		pclose(lprfp);
		lprfp = (FILE *) NULL;
	}
					/* open or close the log file */
	if (status->log && logfp == NULL) {
		if (strcmp(status->log_path, "NOT_DEFINED")) {
			if (!(logfp = fopen(status->log_path, "a")))
				status->log = 0;
		}
		else
			status->log = 0;
	}
	if (!status->log && logfp != NULL) {
		fclose(logfp);
		logfp = (FILE *) NULL;
	}

#ifdef SHAREDMEM
	if (status->clr) {
		status->clr = 0;
		vs_clear();
	}
#else /* SHAREDMEM */
					/* clear if vs_path doesn't exist */
	if (access(status->vs_path, 0))
		vs_clear();
#endif /* SHAREDMEM */

	/*
	 * The very first screen we see after dialing has the "Connected to..."
	 * message at row 0, therefore we start our virtual screen at row 1.
	 */
	if (skip_row) {
		skip_row = 0;
		VROW = 1;
	}
					/* here we go... */
	while (1) {
		if ((in_cnt = read(status->fd, in_buf, INPUT_BUF)) <= 0)
			continue;
					/* send a duplicate to Pcomm */
		if (status->dup_fd != -1)
			write(status->dup_fd, in_buf, in_cnt);

					/* "peel" the buffer one at a time */
		out_cnt = 0;
		bufp = in_buf;
		while (--in_cnt >= 0) {
			c = *bufp++ & 0xff;
					/* send to logfile */
			if (status->log) {
				if (c == '\r' && status->add_lf)
					putc('\n', logfp);
					/* no carriage returns in logfile */
				if (c != '\r')
					putc(c, logfp);
			}
					/* send to printer too? */
			if (status->print)
				putc(c, lprfp);

					/* put a char in virtual screen */
			vs_putchar(c);

					/* build the output buffer */
			out_buf[out_cnt++] = c;
			if (c == '\r' && status->add_lf)
				out_buf[out_cnt++] = '\n';

					/* output in smaller chunks */
			if (out_cnt >= OUTPUT_BUF) {
				fwrite(out_buf, sizeof(char), out_cnt, stdout);
				out_cnt = 0;
			}
		}
		if (out_cnt)
			fwrite(out_buf, sizeof(char), out_cnt, stdout);
	}
}

/*
 * Figure out which signal we just received, and fix the return code of
 * the setjmp function above to the proper value.
 */

int
got_sig(sig)
int sig;
{
	switch (sig) {
		case SIGUSR1:
			signal(SIGUSR1, got_sig);
			longjmp(i_jmp, 1);
		case SIGUSR2:
			signal(SIGUSR2, got_sig);
			longjmp(i_jmp, 2);
		case SIGINT:
			signal(SIGINT, got_sig);
			longjmp(i_jmp, 3);
		case SIGTERM:
			signal(SIGTERM, got_sig);
			longjmp(i_jmp, 4);
	}
}

/*
 * Put a character in the virtual screen.  This routine saves incoming
 * characters in a two dimensional buffer designed to mimic the real
 * screen.
 */

void
vs_putchar(c)
char c;
{
	register int i;
	char *memset();
	int tab_stop;
	void vs_scroll();

	switch (vcs_filter(c)) {
		case MAYBE:		/* wait and see... */
			break;
		case 256+HOME:		/* home virtual screen "cursor" */
			VROW = 0;
			VCOL = 0;
			break;
		case 256+CLR_EOL:	/* clear to end of line */
			memset(&VS[VROW][VCOL], ' ', max_col - VCOL);
			VCOL = max_col -1;
			break;
		case 256+CLR_EOS:	/* clear to end of screen */
			memset(&VS[VROW][VCOL], ' ', max_col - VCOL);
			for (i=VROW+1; i<max_row; i++)
				memset(VS[i], ' ', max_col);
			VROW = max_row -1;
			VCOL = max_col -1;
			break;
		case 256+CLEAR:		/* clear all and home "cursor" */
			for (i=0; i<max_row; i++)
				memset(VS[i], ' ', max_col);
			VROW = 0;
			VCOL = 0;
			break;
		case 256+MV_UP:		/* move "cursor" up */
			VROW--;
			if (VROW < 0)
				VROW = 0;
			break;
		case 256+MV_DOWN:	/* move "cursor" down */
			VROW++;
			if (VROW >= max_row)
				VROW = max_row -1;
			break;
		case 256+MV_RIGHT:	/* move "cursor" right */
			VCOL++;
			if (VCOL >= max_col)
				VCOL = max_col -1;
			break;
		case 256+MV_LEFT:	/* move "cursor" left */
		case BS:		/* non destructive back space */
			VCOL--;
			if (VCOL < 0)
				VCOL = 0;
			break;
		case 256+MV_DIRECT:	/* direct cursor movement */
			VROW = vcs_param[MV_DIRECT][0];
			VCOL = vcs_param[MV_DIRECT][1];

					/* if "add one" and "decimal" */
			if (vcs_opt[MV_DIRECT][0] && vcs_opt[MV_DIRECT][1]) {
				VROW--;
				VCOL--;
			}
					/* if "character" */
			if (vcs_opt[MV_DIRECT][2]) {
					/* if "add offset" */
				if (vcs_opt[MV_DIRECT][3]) {
					VROW -= vcs_opt[MV_DIRECT][5];
					VCOL -= vcs_opt[MV_DIRECT][5];
				}
					/* if "subtract offset" */
				if (vcs_opt[MV_DIRECT][4]) {
					VROW += vcs_opt[MV_DIRECT][5];
					VCOL += vcs_opt[MV_DIRECT][5];
				}
				VROW--;
				VCOL--;
			}
			break;
		case 0:
		case 7:			/* skip NULL and "bell" character */
			break;
		case '\t':		/* tab character */
			tab_stop = VCOL + 8 - (VCOL % 8);
					/* if wrap around */
			if (tab_stop >= max_col) {
					/* spaces up to eol */
				memset(&VS[VROW][VCOL], ' ', max_col - VCOL);
				VROW++;
				if (VROW >= max_row)
					vs_scroll();

					/* the remainder of the tab */
				VCOL = tab_stop - max_col;
			}
			else {
				memset(&VS[VROW][VCOL], ' ', tab_stop - VCOL);
				VCOL = tab_stop;
			}
			break;
		case '\r':		/* carriage return */
			VCOL = 0;
			if (!status->add_lf)
				break;
			/* fall thru...*/
		case '\n':		/* line feed */
			VROW++;
			if (VROW >= max_row)
				vs_scroll();
			break;
		default:		/* a normal character */
			VS[VROW][VCOL] = c;
			VCOL++;
					/* wrap around */
			if (VCOL >= max_col) {
				VCOL = 0;
				VROW++;
				if (VROW >= max_row)
					vs_scroll();
			}
			break;
	}
	return;
}

#ifndef SHAREDMEM
/*
 * Save the virtual screen to a file.
 */

int
write_vs()
{
	FILE *fp;
	register int i;

	if (!(fp = fopen(status->vs_path, "w")))
		return(1);
					/* current x y coordinates */
	fprintf(fp, "%d,%d\n", VROW, VCOL);

	for (i=0; i<max_row; i++) {
		VS[i][max_col] = '\0';
		fprintf(fp, "%s\n", VS[i]);
	}
	fclose(fp);
	return(0);
}

/*
 * Get the virtual screen image from the file.  Since input() gets
 * killed from time to time, the vs_path file is the only way to retain
 * the screen image.
 */

int
read_vs()
{
	FILE *fp;
	register int i;
	char buf[10];
					/* in case the fopen fails... */
	VROW = 0;
	VCOL = 0;
					/* not guaranteed to exist yet */
	if (!(fp = fopen(status->vs_path, "r")))
		return(1);
					/* get the x, y coordinates */
	fgets(buf, 10, fp);
	sscanf(buf, "%d,%d\n", &VROW, &VCOL);

					/* read the file into the vs array */
	for (i=0; i<max_row; i++) {
		fgets(VS[i], MAX_COL, fp);
		VS[i][max_col] = '\0';
	}
	fclose(fp);
	return(0);
}
#endif /* SHAREDMEM */

/*
 * If the user clears the screen with the ^A-C command, the input
 * has to be in sync.
 */

void
vs_clear()
{
	register int i;
	char *memset();

	for (i=0; i<max_row; i++)
		memset(VS[i], ' ', max_col);
					/* home the "cursor" */
	VROW = 0;
	VCOL = 0;
	return;
}

/*
 * Do a software scroll on the virtual screen.  Does not alter the
 * "col" variable.
 */

void
vs_scroll()
{
	char *strcpy(), *memset();
					/* move 'em up 1 line */
#ifdef MEMMOVE
	char *MEMMOVE();

	MEMMOVE(VS[0], VS[1], (max_row -1) * MAX_COL);
#else /* MEMMOVE */
	register int i;

	for (i=0; i<max_row-1; i++)
		strcpy(VS[i], VS[i+1]);
#endif /* MEMMOVE */
					/* clear the bottom line */
	memset(VS[max_row-1], ' ', max_col);

	VROW = max_row -1;
	return;
}

#ifdef BSD
/*
 * Copies the character c, n times to string str
 */

char *
memset(str, c, n)
char *str, c;
int n;
{
	char *s1 = str;

	while (n > 0) {
		--n;
		*s1++ = c;
	}
	return(str);
}
#endif /* BSD */
