/* stof.c	1.1	(IRCAM)	7/25/85	13:26:20 */
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

stof()
{
	register long i,j,k;
	register short *sh,*l;
	register float *fl;

	if(sflseek(sfd,begin_b,0)<0){
		fprintf(stderr,"sndin: error seeking to begin\n");
		exit(-1);
	}
	if (!otty){
		while(begin_b+SF_BUFSIZE < end_b){
			sh = (short *) bufin;
			fl = (float *) bufout;
			i = read(sfd,bufin,SF_BUFSIZE);
			l = (short *) (bufin + i); 
			while (sh < l)
				*fl++ = *sh++ / 32767.0;
			j = write(1,bufout,i*2);
			if (i!=SF_BUFSIZE){
				fprintf(stderr,
				"sndin: error reading samples\n");
				exit(-1);
			}
			else if (j!=(i*2)) {
				fprintf(stderr,
				"sndin: error writing samples\n");
				exit(-1);
			}
			begin_b += SF_BUFSIZE;
		}
		sh = (short *) bufin;
		fl = (float *) bufout;
		i = read(sfd,bufin,end_b-begin_b);
		l = (short *) (bufin + i); 
		while (sh < l)
			*fl++ = *sh++ / 32767.0;
		j = write(1,bufout,i*2);
		if (i != end_b - begin_b){
			fprintf(stderr, "sndin: error reading samples\n");
			exit(-1);
		}
		else if (j!=(i*2)) {
			fprintf(stderr, "sndin: error writing samples\n");
			exit(-1);
		}
		return(0);
	}
	sh = (short *) bufin;
	while(begin_b+SF_BUFSIZE < end_b){
		i = read(sfd,bufin,SF_BUFSIZE);
		for(j = 0; j < (i/sizeof(short)); j++) 
			printf("%f\n",(float) sh[j]/32767.0);
		if (i!=SF_BUFSIZE){
			fprintf(stderr,
			"sndin: error reading samples\n");
			exit(-1);
		}
		else if (j != (i/sizeof(short))) {
			fprintf(stderr,
			"sndin: error writing samples\n");
			exit(-1);
		}
		begin_b += SF_BUFSIZE;
	}
	i = read(sfd,bufin,end_b-begin_b);
	for(j = 0; j < (i/sizeof(short)); j++)
		printf("%f\n",(float) sh[j]/32767.0);
	if (i != end_b - begin_b){
		fprintf(stderr, "sndin: error reading samples\n");
		exit(-1);
	}
	else if (j != (i/sizeof(short))) {
		fprintf(stderr, "sndin: error writing samples\n");
		exit(-1);
	}
	return(0);
}
