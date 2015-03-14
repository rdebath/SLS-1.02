/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/*	grid.c  - draw a grid of lines */

#include <signal.h>
#include "term.h"
#include "bitmap.h"

static
clean( i )
int	i;
{
	m_pop();
	exit(i);
}


main(argc,argv)
	int argc;
	char *argv[];
{
	int x,y,i,j;
	int xmax,ymax,dummy;

	ckmgrterm( *argv );

	if (argc >= 2) {
		x = atoi(argv[1]);
		y = atoi(argv[2]);
		}
	else {
		x = 10;
		y = 10;
		}

	if (x<2)
		x = 10;

	if (y<2)
		y = 10;

	m_setup(0);
	get_size(&dummy,&dummy,&xmax,&ymax);
	signal(SIGTERM,clean);
	signal(SIGINT,clean);
	signal(SIGHUP,clean);
	m_clear();
	m_push(P_FLAGS);
	m_setmode(M_ABS);

	m_func(BIT_SET);
	for(i=0;i<xmax;i+=x)
		m_line(i,0,i,ymax);
	for(i=0;i<ymax;i+=y)
		m_line(0,i,xmax,i);
	clean(0);
}
