/* ftof.c	1.1	(IRCAM)	7/25/85	13:26:14 */
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

ftof()
{
	register long i,j;
	register float *fl;

	if(sflseek(sfd,begin_b,0)<0){
		fprintf(stderr,"sndin: error seeking to begin\n");
		exit(-1);
	}
	if (!otty){
		while(begin_b+SF_BUFSIZE < end_b){
			i = read(sfd,bufin,SF_BUFSIZE);
			j = write(1,bufin,i);
			if (i!=SF_BUFSIZE){
				fprintf(stderr,
				"sndin: error reading samples\n");
				exit(-1);
			}
			else if (j!=i) {
				fprintf(stderr,
				"sndin: error writing samples\n");
				exit(-1);
			}
			begin_b += SF_BUFSIZE;
		}
		i = read(sfd,bufin,end_b-begin_b);
		j = write(1,bufin,i);
		if (i != end_b - begin_b){
			fprintf(stderr, "sndin: error reading samples\n");
			exit(-1);
		}
		else if (j!=i) {
			fprintf(stderr, "sndin: error writing samples\n");
			exit(-1);
		}
		return(0);
	}
	fl = (float *) bufin;
	while(begin_b+SF_BUFSIZE < end_b){
		i = read(sfd,bufin,SF_BUFSIZE);
		for(j = 0; j < (i/sizeof(float)); j++) printf("%f\n",fl[j]);
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
	for(j = 0; j < (i/sizeof(float)); j++) printf("%f\n",fl[j]);
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
