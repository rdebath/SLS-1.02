/* whead.c	1.1	(IRCAM)	7/25/85	13:26:23 */
#include <stdio.h>
#include <carl/sndio.h>
#include <carl/carl.h>
#include <carl/defaults.h>
#include <carl/procom.h>
#include <local/sfheader.h>

whead(sfh, chanspec, opak, name)
	SFHEADER *sfh;
	char *chanspec, opak, *name;
{
	char srate[16], nchans[16], *format;
	int cnt;

	(void) sprintf(srate, "%f", sfh->sfinfo.sf_srate);
	for (cnt = 0; chanspec != NULL && chanspec[cnt] != '\0'; cnt++)
		/* empty */ ;
	if (cnt == 0)
		cnt = sfh->sfinfo.sf_chans;
	(void) sprintf(nchans, "%d", cnt);
	if (opak == PM16BIT)
		format = H_SHORTSAM;
	else
		format = H_FLOATSAM;

	if (stdheader(stdout, name, srate, nchans, format)) {
		fprintf(stderr, "stdheader failed\n");
		return(-1);
	}
	if (putheader(stdout)) {
		fprintf(stderr, "putheader failed\n");
		return(-1);
	}
	if (flushfloat() < 0) {
		fprintf(stderr, "flushfloat failed\n");
		return(-1);
	}
	return(0);
}
