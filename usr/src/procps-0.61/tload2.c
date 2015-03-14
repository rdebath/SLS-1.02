/*
 * tload.c	- terminal version of xload
 *
 * Options:
 *	-s initial scaling exponent (default = 6)
 *	-d delay
 *	-b 	background mode: don't update if window in background
 *		Uses less cpu time and the screen saver still works if
 *		the tload console is not the current console.
 *
 *
 * Copyright (c) 1992 Branko Lankester
 */

#include <stdio.h>
#include <termios.h>
#include <signal.h>
#include <sys/stat.h>
#include <setjmp.h>

#define	NR_CONSOLE	8

char *screen;

int nrows = 25;
int ncols = 80;
int scr_size;
int interv = 1;
int fd=1;
jmp_buf jb;

extern int optind;
extern char *optarg;

void
readavg(av)
     long av[3];
{
  FILE *fp;

  fp = fopen ("/proc/loadavg", "r");
  if (!fp)
    av[0] = av[1] = av[2] = 0;
  else
    {
      float a[3];

      fscanf (fp, "%g %g %g", a, a + 1, a + 2);
      fclose (fp);
      av[0] = a[0] * 2048;
      av[1] = a[1] * 2048;
      av[2] = a[2] * 2048;
    }
}

void
alrm(n)
{
    signal(SIGALRM, alrm);
    alarm(1);
}

void
setsize(i)
{
    struct winsize win;

    signal(SIGWINCH, setsize);
    if (ioctl(fd, TIOCGWINSZ, &win) != -1) {
	if (win.ws_col > 0)
	    ncols = win.ws_col;
	if (win.ws_row > 0)
	    nrows = win.ws_row;
    }
    scr_size = nrows * ncols;
    if (screen == NULL)
    	screen = (char *) malloc(scr_size);
    else
    	screen = (char *) realloc(screen, scr_size);

    if (screen == NULL) {
	perror("");
	exit(1);
    }
    memset(screen, ' ', scr_size-1);
    *(screen + scr_size - 2) = '\0';
    if (i)
	longjmp(jb, 0);
}


main(argc, argv)
char **argv;
{
    int row, col=0, i, opt, bgnd_mode = 0;
    int lines, min_scale = 6, scale_shift;
    int delay = 5;
    long av[3];

    while ((opt = getopt(argc, argv, "bs:d:")) != -1)
	switch (opt) {
	    case 's': min_scale = atoi(optarg); break;
	    case 'd': delay = atoi(optarg); break;
	    case 'b': bgnd_mode = 1; break;
	    default:
		printf("usage: tload [-b] [-d delay] [-s scale] [tty]\n");
		exit(1);
	}

    if (argc > optind) {
	if ((fd = open(argv[optind], 1)) == -1) {
	    perror(argv[optind]);
	    exit(1);
	}
    }

    setsize(0);

    scale_shift = min_scale;

    setjmp(jb);
    col = 0;
    alrm(0);
    while (1) {
	int dly = delay;

 	if ( scale_shift > min_scale )
	    scale_shift--; /* help it drift back down.. */

	readavg (av);
    repeat:
	lines = av[0] >> scale_shift;
	row = nrows-1;

	while (--lines >= 0) {
	    *(screen + row * ncols + col) = '*';
	    if (--row < 0) {
		++scale_shift;
		goto repeat;
	    }
	}
	while (row >= 0)
	    *(screen + row-- * ncols + col) = ' ';

	for (i = 1; ; ++i) {
	    char *p;
	    row = nrows - (i*2048 >> scale_shift);
	    if (row < 0)
		break;
	    if (*(p = screen + row * ncols + col) == ' ')
		*p = '-';
	    else
		*p = '+';
	}

	if (++col == ncols) {
	    --col;
	    memmove(screen, screen + 1, scr_size-1);

	    for(row = nrows-2; row >= 0; --row)
		*(screen + row * ncols + col) = ' ';
	}
	for (i=0; i<3; ++i) {
	    av[i] *= 100;
	    av[i] >>= 11;
	}
	i = sprintf(screen, " %d.%02d, %d.%02d, %d.%02d",
		av[0] / 100, av[0] % 100,
		av[1] / 100, av[1] % 100,
		av[2] / 100, av[2] % 100);
	if (i>0)
	    screen[i] = ' ';

	write(fd, "\033[H", 3);
	write(fd, screen, scr_size - 1);
	while (--dly >= 0)
	    pause();
    }
}
