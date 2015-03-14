/* setchan.c	1.1	(IRCAM)	7/25/85	13:26:17 */
#include <stdio.h>
#include <carl/sndio.h>

/* array chans will be !=0 for each channel that is to be output */
short	*Chans,nchans;

setchan(cs, nc)
	char *cs; int nc;
{
	extern char *calloc();
	char *chans;
	char *c, *d, *index();
	int i, j, hit, inc = 0;

	chans = (char *) calloc((unsigned) nc, sizeof(char));

	/* no channels specified? default to all channels */
	if (cs == NULL) {
		nchans=nc;
		return(0);
	}

	/* pick up specified channel numbers */
	for (c = cs; *c != '\0'; cs = ++c) {
		c = index(cs, ',');
		if (c != NULL) 
			*c = '\0';
		if ((d = index(cs, 'x')) != NULL) {	/* pickup chan inc */
			inc = sfexpr(d+1, 1.0);
			*d = '\0';
		}
		i = sfexpr(cs, 1.0);
		if (inc != 0) {
		    for (j = i; j < nc; j += inc)
			    chans[j-1]++;
		    inc = 0;
		} else {
		    if (0 < i && i <= nc) 
			    chans[i-1]++;
		}
		if (c == NULL) 
			break;
	}

	for (hit = i = 0; i < nc; i++) if (chans[i]) {
		hit++;
#ifdef debug
		fprintf(stderr,"hit %d\n", i);
#endif debug
	}

	nchans = hit;
	if(!hit) return(-1);
	Chans = (short *) calloc((unsigned) hit+1, sizeof(short));
	for (hit = i = j = 0; i < nc; i++,hit++) if (chans[i]) {
		Chans[j++]=hit;
		hit=0;
	}
	Chans[j]=hit;
#ifdef debug
	for (i=0; i<nchans+1; i++)
		fprintf(stderr,"Chans[%d] = %d\n", i, Chans[i]);
#endif debug
	return(0);
}
