/*
 * tload.c	- terminal version of xload
 *
 * Options:
 *	-s initial scaling exponent (default = 6)
 *	-d delay
 *
 * Copyright (c) 1992 Branko Lankester
 * /proc changes by David Engel (david@ods.com)
 * Made a little more efficient by Michael K. Johnson (johnsonm@stolaf.edu)
 */

#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <setjmp.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include "sysinfo.h"

char *screen;

int nrows = 25;
int ncols = 80;
int scr_size;
int interv = 1;
int fd=1;
int dly=5;
jmp_buf jb;

extern int optind;
extern char *optarg;

void alrm(int n)
{
    signal(SIGALRM, alrm);
    alarm(dly);
}

void setsize(int i)
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

int main(int argc, char **argv)
{
    int lines, row, col=0;
    int i, opt;
    double av[3], max_scale, scale_fact;
    char *scale_arg = NULL;

    while ((opt = getopt(argc, argv, "s:d:")) != -1)
	switch (opt) {
	    case 's': scale_arg = optarg; break;
	    case 'd': dly = atoi(optarg); break;
	    default:
		printf("usage: tload [-d delay] [-s scale] [tty]\n");
		exit(1);
	}

    if (argc > optind) {
	if ((fd = open(argv[optind], 1)) == -1) {
	    perror(argv[optind]);
	    exit(1);
	}
    }

    setsize(0);

    if (scale_arg)
      max_scale = atof(scale_arg);
    else
      max_scale = nrows;

    scale_fact = max_scale;

    setjmp(jb);
    col = 0;
    alrm(0);

    while (1) {

 	if (scale_fact < max_scale)
	    scale_fact *= 2.0; /* help it drift back up. */

	loadavg(&av[0], &av[1], &av[2]);

    repeat:
	lines = av[0] * scale_fact;
	row = nrows-1;

	while (--lines >= 0) {
	    *(screen + row * ncols + col) = '*';
	    if (--row < 0) {
		scale_fact /= 2.0;
		goto repeat;
	    }
	}
	while (row >= 0)
	    *(screen + row-- * ncols + col) = ' ';

	for (i = 1; ; ++i) {
	    char *p;
	    row = nrows - (i * scale_fact);
	    if (row < 0)
		break;
	    if (*(p = screen + row * ncols + col) == ' ')
		*p = '-';
	    else
		*p = '=';
	}

	if (++col == ncols) {
	    --col;
	    memmove(screen, screen + 1, scr_size-1);

	    for(row = nrows-2; row >= 0; --row)
		*(screen + row * ncols + col) = ' ';
	}
	i = sprintf(screen, " %.2f, %.2f, %.2f",
		av[0], av[1], av[2]);
	if (i>0)
	    screen[i] = ' ';

	write(fd, "\033[H", 3);
	write(fd, screen, scr_size - 1);
	pause();
    }
}
