/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include <sys/time.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdio.h>

#include "term.h"
#include "bitmap.h"

/*
 * cycle -- a program to do simple flip-book animation
 * Steve Hawley
 */

#define fsleep(x) \
   { \
   struct timeval time; \
   time.tv_sec = 0; \
   time.tv_usec = x; \
   select(0,0,0,0,&time); \
   }

#define DEF_SPEED 20000
#define MAXBUF 512

static char	*cmd;
static int	offset;	/* where icons end (offset from argc) */
static int	bitcount;	/* number of bitmaps created */
static char	cwd[MAXBUF];
static int	w, h;

static void usage();
static void loadmap();

static void
cleanup()
{
	/* be a nice program and clean up */
	int i;

	m_ttyreset();			/* reset echo */
	for (i = 1; i <= bitcount; i++)	/* free up bitmaps */
		m_bitdestroy(i);
	m_pop();			/* pop environment */
	exit(0);
}

int main(argc,argv) int argc; char *argv[];
{
	int	speed, i;
	int	reverse = 0;

	ckmgrterm(*argv);
	cmd = *argv;
        getcwd(cwd,sizeof(cwd));

	argc--; argv++;
        if (argc<1) usage();

	m_setup(M_FLUSH);	/* flush output stream */
	m_push(P_BITMAP|P_EVENT|P_FLAGS);
	m_setmode(M_ABS);

	signal(SIGINT,cleanup);		/* program loops forever */
	signal(SIGTERM,cleanup);	/* this gives a mechanism */
	signal(SIGQUIT,cleanup);	/* for cleaning up */

	m_func(BIT_SRC);	/* bit copy, so we don't have to erase */
	m_clear();	/* clear the screen */
	m_ttyset()	;/* no keybaord echo */

	speed = DEF_SPEED;
	offset = 0;

	while( argv[0][0] == '-' ) {
		switch( argv[0][1] ) {
		case 's':
			speed = atoi(&(argv[0][2]));
			break;
		case 'r':
			reverse = 1;
			break;
		default:
			usage();
		}
		argv++; argc--;
	}

	if (argc < 1)
		usage();

	for ( ; argc;  argv++, argc-- ) {
		bitcount++;
		loadmap( bitcount, *argv );
	}
	while(1) {
		for (i = 1; i <= bitcount; i++) {
			m_bitcopyto(0, 0, w, h, 0, 0, 0, i);
			fsleep(speed);
			/* delay a bit, so we can see animation */
		}
		if( !reverse )
			continue;
		for (i--;  i > 1;  i--) {
			m_bitcopyto(0, 0, w, h, 0, 0, 0, i);
			fsleep(speed);
		}
	}
}


static
void
loadmap( i, file )
int	i;
char	*file;
{
	char	buf[MAXBUF];

	if (*file == '/')
		m_bitfromfile(i, file);
	else if (strncmp(file, "../", 3) == 0) {
		sprintf(buf, "%s/%s", cwd, file);
		m_bitfromfile(i, buf);
	}
	else if (strncmp(file, "./", 2) == 0) {
		sprintf(buf, "%s%s", cwd, file+1);
		m_bitfromfile(i, buf);
	}
	else {
		m_bitfromfile(i, file);
	}
	m_gets(buf);
	sscanf(buf, "%d %d", &w, &h); /* load in icons. */
	if (w == 0 || h == 0) {
		fprintf(stderr, "%s: %s is not a bitmap.\n", cmd, file);
		cleanup();
	}
}


static
void
usage()
{
	fprintf(stderr, "Usage: %s [-sspeed] [-r] icon1 [icon2 ...iconn]\n",
		cmd);
	fputs("\
-sspeed	delay `speed' microseconds between frames\n\
-r	after running forward through the frames, reverse and run backwards\n",
		stderr);
	exit(1);
}
