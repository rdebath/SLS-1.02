/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include <time.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h> 
#include <stdio.h>

#include "term.h"
#include "bitmap.h"

#define SCREEN 0
#define MINS 1
#define MASKMINS 13
#define HRS 25
#define MASKHRS 37
#define FACE 49
#define SCRATCH 50
#define ICONPATH	"mclock"

static char *_quit = "\034";

int x, y, w, firstw, h, firsth, i, j, k;
int hsize, vsize;

void cleanup()
{
	m_setcursor(0);
	m_clear();
	m_pop();
	exit(0);
}

void do_time()
{
	struct tm *tme, *localtime();
	long tmp, time();
	int hr, mn, mn1;

	tmp = time(0);
	tme = localtime(&tmp); /* get the time */

	mn = tme->tm_min;
	hr = (tme->tm_hour > 11 ? tme->tm_hour - 12 : tme->tm_hour);
	/* set hours to be 0-11 */
	mn1 = ( (mn/5) + (mn % 5 < 3 ? 0 : 1));
	/* adjust minutes so that 10:03 will read as 10:05 to help accuracy */
	if (mn > 33) hr++;
	/* move hour hand when minute hand is on 7 so that 7:45 will look ok */
	mn1 %= 12; /* fix minutes to be 0-11 */
	hr %= 12; /* fix hours to be 0-11 */

	m_func(BIT_SRC);
	m_bitcopyto(0, 0, w, h, 0, 0, SCRATCH, FACE); /* copy face */
	m_func(BIT_AND);
	m_bitcopyto(0, 0, w, h, 0, 0, SCRATCH, MASKHRS + hr); /* mask of hour */
	m_func(BIT_OR);
	m_bitcopyto(0, 0, w, h, 0, 0, SCRATCH, HRS + hr); /* OR hour in */
	m_func(BIT_AND);
	m_bitcopyto(0, 0, w, h, 0, 0, SCRATCH, MASKMINS + mn1); /* mask minutes */
	m_func(BIT_OR);
	m_bitcopyto(0, 0, w, h, 0, 0, SCRATCH, MINS + mn1); /* OR in minute hand */
	m_func(BIT_SRC);
	m_bitcopyto(0, 0, w, h, 0, 0, SCREEN, SCRATCH); /* put on screen */
}

void clearit()
{
	m_clear();
	do_time();
}

void shapeit()
{
	int	border;

	get_size(&x, &y, &hsize, &vsize);
	get_param( (char *)0, (int *)0, (int *)0, &border );
	m_shapewindow(x, y, w + 2*border, h + 2*border);
}

void panic()
{
	if( !firstw )
		firstw = w;
	if( !firsth )
		firsth = h;
	if( !w  ||  !h  ||  w != firstw  ||  h != firsth ) {
		m_ttyreset();
		fprintf(stderr, "%d %d\n", w, h );
		fprintf(stderr, "bitmap size mismatch: bitmaps may be bad.\n");
		exit(0);
	}
}

int main(argc,argv) 
int argc; char **argv;
{
	char buf[101];

	ckmgrterm( *argv );

	m_setup(M_FLUSH);
	m_push(P_BITMAP | P_EVENT | P_FLAGS | P_POSITION);
	m_setmode(M_ABS);

	signal(SIGINT,cleanup);
	signal(SIGTERM,cleanup);
	signal(SIGQUIT,clearit);

	m_setevent(RESHAPE,_quit);
	m_setevent(REDRAW,_quit);
	
	m_ttyset(); /* no echo */
        m_setcursor(9);
	fprintf(stderr,"Please wait...");
	fflush(stderr);
	for (i = 0; i < 12; i++) {
	/* load all the hands and their masks */
		sprintf(buf, "%s/mhand%d",ICONPATH, i);
		m_bitfile(i+MINS, buf, &w, &h);
		panic(); /* if the dimensions are wrong, panic and exit */
		sprintf(buf, "%s/mmhand%d",ICONPATH, i);
		m_bitfile(i+MASKMINS, buf, &w, &h);
		panic();
		sprintf(buf, "%s/hhand%d",ICONPATH, i);
		m_bitfile(i+HRS, buf, &w, &h);
		panic();
		sprintf(buf, "%s/mhhand%d",ICONPATH, i);
		m_bitfile(i+MASKHRS, buf, &w, &h);
		panic();
	}
	sprintf(buf, "%s/mickface",ICONPATH); /* get the face */
	m_bitfile(FACE, buf, &w, &h);
	panic();
	m_ttyreset();/* reset echo */
	shapeit();
	clearit();
	while(1) {
		m_flush();
		do_time();
		sleep(15); /* update only every 15 seconds */
	}
}
