/* ftos.c	1.1	(IRCAM)	7/25/85	13:26:15 */
#include <stdio.h>
#include <carl/sndio.h>
#include <local/sfheader.h>

extern int otty;
extern long begin_b, end_b;
extern int sfd;
extern SFHEADER sfh;
extern struct stat sfstat;
extern int	chans;
extern char	bufin[],bufout[];

ftos()
{
	register long i,j,k;
	register float *fl;
	register short *sh, *l;

	if(sflseek(sfd,begin_b,0)<0){
		fprintf(stderr,"sndin: error seeking to begin\n");
		exit(-1);
	}
	if (!otty){
		while(begin_b+SF_BUFSIZE < end_b){
			i = read(sfd,bufin,SF_BUFSIZE);
			fl = (float *) bufin;
			sh = (short *) bufout;
			l = (short *) (bufout + i);
			while(sh < l)
				*sh++ = *fl++ * 32767.0;
			j = write(1,bufout,i/2);
			if (i!=SF_BUFSIZE){
				fprintf(stderr,
				"sndin: error reading samples\n");
				exit(-1);
			}
			else if (j!=(i/2)) {
				fprintf(stderr,
				"sndin: error writing samples\n");
				exit(-1);
			}
			begin_b += SF_BUFSIZE;
		}
		fl = (float *) bufin;
		sh = (short *) bufout;
		i = read(sfd,bufin,end_b-begin_b);
		l = (short *) (bufout + i);
		while(sh < l)
			*sh++ = *fl++ * 32767.0;
		j = write(1,bufout,i/2);
		if (i != end_b - begin_b){
			fprintf(stderr, "sndin: error reading samples\n");
			exit(-1);
		}
		else if (j!=(i/2)) {
			fprintf(stderr, "sndin: error writing samples\n");
			exit(-1);
		}
		return(0);
	}
	fl = (float *) bufin;
	while(begin_b+SF_BUFSIZE < end_b){
		i = read(sfd,bufin,SF_BUFSIZE);
		for(j = 0; j < (i/sizeof(float)); j++) 
			printf("%d\n",(int)(fl[j]*32767.0));
		if (i!=SF_BUFSIZE){
			fprintf(stderr,
			"sndin: error reading samples\n");
			exit(-1);
		}
		else if (j != (i/sizeof(float))) {
			fprintf(stderr,
			"sndin: error writing samples\n");
			exit(-1);
		}
		begin_b += SF_BUFSIZE;
	}
	i = read(sfd,bufin,end_b-begin_b);
	for(j = 0; j < (i/sizeof(float)); j++) 
		printf("%d\n",(int)(fl[j]*32767.0));
	if (i != end_b - begin_b){
		fprintf(stderr, "sndin: error reading samples\n");
		exit(-1);
	}
	else if (j != (i/sizeof(float))) {
		fprintf(stderr, "sndin: error writing samples\n");
		exit(-1);
	}
	return(0);
}
