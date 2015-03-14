/* rhead.c	1.1	(IRCAM)	7/25/85	14:04:21 */
#include <stdio.h>
#include <carl/libsf.h>
#include <carl/defaults.h>
#include <carl/carl.h>
#include <carl/procom.h>
#include <local/sfheader.h>

rhead(sfh, ipak, iop)
	SFHEADER *sfh;
	char *ipak;
	FILE *iop;
{
	extern char *index();
	extern float atof();
	char *pval;


	if (getheader(iop) == NULL)
		return(0);

	if ((pval = getprop(iop, H_FORMAT)) != NULL){
		if (!strcmp(pval, H_FLOATSAM))
			*ipak = PMFLOAT;
		else
			*ipak = PM16BIT;
	}

	if ((pval = getprop(iop, H_NCHANS)) != NULL)
		sfchans(sfh) = atoi(pval);

	if ((pval = getprop(iop, H_SRATE)) != NULL)
		sfsrate(sfh) = atof(pval);

	if ((pval = getprop(iop, H_SNDOUT_FORMAT)) != NULL)
		if (!strcmp(pval, H_FLOATSAM))
			sfclass(sfh) = SF_FLOAT;

	return(0);
}
