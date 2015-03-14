/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include <signal.h>
#include "term.h"
#include "bitmap.h"

/* program to draw hilbert space filling curves (very quick).
 * Steve Hawley			11/87 (Macintosh C implementation)
 * translated from a pascal version from the Oberlin Computer Science
 * Lab manual, Fall 1984, Author unknown.
 * --------- ported to mgr by SAU: FAST version uses fast vector mode
 */

int dir;

/* global direction variable.  The original program used turtle graphics
 * with turn functions taking angles.  I cheat since all turns in this
 * program are 90 degrees, and make the directions be:
 *		0: down
 *		1: right
 *		2: up
 *		3: left
 */

start()
{
	/* put the graphics cursor somewhere nice, and initialize
	 * the direction.
	 */

	m_go(10,10);

	dir = 0;
}

left()
{
	/* a turn to the left is actually the direction + 3
	 * modulo 3.
	 */
	dir = (dir + 3) & 0x3;
}

right()
{
	/* a right turn is the direction + 1 modulo 3 */
	dir = (dir + 1) & 0x3;
}

forward(size)
register int size;
{
	/* move the graphics cursor and draw a line segment.
	 * The Macintosh function Line(dh, dv) draws a line from the
	 * current graphics cursor to the graphics cursor displaced
	 * by dh and dv (horizontal and vertical deltas).
	 */
	switch(dir) {
	case 0:
		Line(0, size);
		break;
	case 1:
		Line(size, 0);
		break;
	case 2:
		Line(0, -size);
		break;
	case 3:
		Line(-size, 0);
		break;
	}
}

/* mutually recursive hilbert functions: */
lhilbert(size, level)
register int size, level;
{
	if (level > 0) {
		left();
		rhilbert(size, level-1);
		forward(size);
		right();
		lhilbert(size, level-1);
		forward(size);
		lhilbert(size, level-1);
		right();
		forward(size);
		rhilbert(size, level-1);
		left();
	}
}

rhilbert(size, level)
register int size, level;
{
	if (level > 0) {
		right();
		lhilbert(size, level-1);
		forward(size);
		left();
		rhilbert(size, level-1);
		forward(size);
		rhilbert(size, level-1);
		left();
		forward(size);
		lhilbert(size, level-1);
		right();
	}
}

main (argc,argv)
int	argc;
char	**argv;
{
	int clean();

	ckmgrterm( *argv );
 	m_setup(0);	
	signal(SIGTERM,clean);
	signal(SIGINT,clean);
	signal(SIGHUP,clean);
	system("stty litout");
	m_func(BIT_SET);
	/* initialize */
	start();
	m_clear();
	/* draw the hilbert (this is *very* fast) */
	rhilbert(8, 7);
	clean();
}

/* FAST drawing stuff */

#define SIZE	75			/* maximum # of points in a shot */
#define MAX	7				/* maximum distance */
static int count = 0;
char buff[1024];	/* grunge buffer */

/* add delta to grunge list */

int
Line(dx,dy)
register int dx,dy;
	{
	register int mx,my;

	if (dx > MAX || dy > MAX) {
		mx = (dx>>1);
		my = (dy>>1);
	   buff[count++] = (mx+8)<<4 | (my+8);
		dx = dx-mx;
		dy = dy-my;
		}

	buff[count++] = (dx+8)<<4 | (dy+8);
	if (count >= SIZE)
		flush();

	}

/* flush pending grunge data */

int
flush()
	{
	if (count > 0) {
		m_rfastdraw(count,buff);
		count = 0;
		}
	}

clean()
	{
	flush();
	m_flush();
	system("stty -litout");
	exit(0);
	}
