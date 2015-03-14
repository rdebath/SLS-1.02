/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: stringart.c,v 4.3 88/06/30 11:41:47 sau Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/stringart.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/stringart.c,v $$Revision: 4.3 $";

/*	stringart.c	13	84/04/22	*/

#include <stdio.h>
#include <sys/time.h>
#include "term.h"

#define NUMLINES	343		/* number of vectors in a design */
#define NUMFUNCTIONS	13		/* number of functions */
#define RAWMIN		(-10000)	/* smallest raw data value */
#define RAWMAX		(10000)		/* largest raw data value */

extern int function[NUMFUNCTIONS][NUMLINES];

#define fsleep(x) \
   { \
   struct timeval time; \
   time.tv_sec = 0; \
   time.tv_usec = x; \
   select(0,0,0,0,&time); \
   }

main(argc,argv)
	int argc;
	char *argv[];
{
	register int m,i,j,k,l;
	int xoffset;
	int yoffset;
	int xscale, yscale, rscale;
	int xmin,xmax,ymin,ymax;
	short lines[4][NUMLINES];
	int lcolor,bcolor;	/* line colors */
        int slp=0;

	ckmgrterm( *argv );

	if (argc>1 && strcmp(argv[1],"-s")==0) {
           argc--; argv++;
           slp++;
           }
	rscale = (RAWMAX-RAWMIN);
	if (argc >= 5) {
		xmin = atoi(argv[1]);
		ymin = atoi(argv[2]);
		xmax = atoi(argv[3]);
		ymax = atoi(argv[4]);
		}
	else {
		xmin = 0;
		ymin = 0;
		xmax = 999;
		ymax = 999;
		}

	xscale = xmax-xmin;
	yscale = ymax-ymin;
	xoffset = xmin;
	yoffset = ymin;

	srand(getpid());
	m_setup(0);
	m_func(BIT_SET);

	/*
        Restart();
        */
	m_clear(); m_flush();
	while(1) {
		i=(rand()>>5)%NUMFUNCTIONS;
		while((j=(rand()>>5)%NUMFUNCTIONS)==i);
		k=(rand()>>5)%NUMFUNCTIONS;
		while((l=(rand()>>5)%NUMFUNCTIONS)==k);
		bcolor = rand()%24;
		m_bcolor(bcolor);
		for(m=0;m<NUMLINES;m++) {
			lines[0][m] = (function[i][m]-RAWMIN)*xscale/rscale+xoffset;
			lines[1][m] = (function[k][m]-RAWMIN)*yscale/rscale+yoffset;
			lines[2][m] = (function[j][m]-RAWMIN)*xscale/rscale+xoffset;
			lines[3][m] = (function[l][m]-RAWMIN)*yscale/rscale+yoffset;
			}
		m_clear();
		for(m=0;m<NUMLINES;m++) {
			while((lcolor = rand()%24) == bcolor);
			m_linecolor(BIT_SRC,lcolor);
			m_line(lines[0][m],lines[1][m],
					  lines[2][m],lines[3][m]);
                   if (slp) {
                      m_flush();
                      fsleep(60000);
                      }
                   }
	m_flush();
	sleep(argc>5?atoi(argv[5]):3);
	}
}
