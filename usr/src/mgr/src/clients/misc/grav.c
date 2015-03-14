/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#include <sys/time.h>
#include <stdio.h>
#include "term.h"
#include "bitmap.h"

#define MAXX	999
#define MAXY	999
#define MAXV	60
#define MINV	20
#define LCT	10
#define SLOW	60000		/* usec to sleep between lines */
#define ACC     2
#define fsleep(x) \
   { \
   struct timeval time; \
   time.tv_sec = 0; \
   time.tv_usec = x; \
   select(0,0,0,0,&time); \
   }
#define abs(x)	((x) < 0 ? -(x) : (x))

int vx, vy; /* x and y velocities */
int x, y, x1, y1;
int rad, hsize, vsize;
int lcolor,bcolor;
long random();

main(argc,argv)
char **argv;
{
	char *getenv();
        register int s = 0;
	int sleep = 0;

	ckmgrterm( *argv );

	if (argc>1 && strcmp(argv[1],"-s")==0)
		sleep++;

	m_setup(0);
	m_push(P_EVENT|P_FLAGS);
	get_size(&x, &y, &hsize, &vsize);
	rad = (vsize+hsize)>>6;/* size is the avg. of dims. / 32 */

	vx = 5; /* constant horizontal velocity */
	vy = 0;  /* initial vertical velocity of zero */
	x = rad + 1;
	y = rad + 1;

	bcolor = random()%24;
	m_bcolor(bcolor);
	while((lcolor = random()%24) == bcolor);
	m_linecolor(BIT_SRC,lcolor);
	m_fcolor(lcolor);
	m_clear();
	m_circle(x,y,rad);
	for(;;)
	{
		x1 = x +vx; /* add velocity to x */
		if (x1 > MAXX-rad || x1 < rad) {
		/* fix coords if over border */
			vx *= -1;
			x1 += vx;
		}
		vy += ACC; /* accelerate vertical velocity */
		y1 = y + vy; /* add velocity to y */
		if (y1 > MAXY-rad || y1 < rad) {
	                m_circle(x1,MAXY-rad,rad);
			vy = -vy * 95 /100;
                        if (abs(vy) < ACC) {
                           fsleep(900000);
                           }
			y1 += vy;
		}
		m_circle(x1,y1,rad); /* draw new position */
		x = x1; /* reset x and y */
		y = y1;
		m_flush();
		if (sleep)
		   fsleep(90000);
	}
}

