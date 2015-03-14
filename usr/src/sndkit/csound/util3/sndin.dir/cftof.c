/* cftof.c	1.1	(IRCAM)	7/25/85	13:26:08 */
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
extern	short	*Chans,nchans;

cftof()
{
	register long i,j,k;
	register float *flin, *flout;

	if(sflseek(sfd,begin_b,0)<0){
		fprintf(stderr,"sndin: error seeking to begin\n");
		exit(-1);
	}
	flin=(float *)bufin;
	flout=(float *)bufout;
	i=j=k=0;
	i+=Chans[j++];
	if (!otty){
		while(begin_b+SF_BUFSIZE < end_b){
			if (read(sfd,bufin,SF_BUFSIZE)!=SF_BUFSIZE){
				fprintf(stderr,
				"sndin: error reading samples\n");
				exit(-1);
			}
			while(i<SF_BUFSIZE/sizeof(float)){
			    flout[k++]=flin[i];
			    if(k==SF_BUFSIZE/sizeof(short)){
				if(write(1,bufout,SF_BUFSIZE*2)!=SF_BUFSIZE*2){
				    fprintf(stderr,
				    "sndin: error writing samples\n");
				    exit(-1);
				}
				k=0;
			    }
			    i+=Chans[j++];
			    	if(j==nchans+1){j=0;i+=Chans[j++];}
			}
			i -= (SF_BUFSIZE/sizeof(float));
			begin_b += SF_BUFSIZE;
		}
		if (read(sfd,bufin,end_b-begin_b)!=end_b-begin_b){
			fprintf(stderr, "sndin: error reading samples\n");
			exit(-1);
		}
		while(i<(end_b-begin_b)/sizeof(float)){
		    flout[k++]=flin[i];
		    if(k==SF_BUFSIZE/sizeof(short)){
			if (write(1,bufout,SF_BUFSIZE*2)!=SF_BUFSIZE*2){
			    fprintf(stderr,
			    "sndin: error writing samples\n");
			    exit(-1);
			}
			k=0;
		    }
		    i+=Chans[j++];
		    	if(j==nchans+1){j=0;i+=Chans[j++];}
		}
		j = write(1,bufout,k*sizeof(float));
		if (j!=k*sizeof(float)) {
			fprintf(stderr, "sndin: error writing samples\n");
			exit(-1);
		}
		return(0);
	}
	while(begin_b+SF_BUFSIZE < end_b){
		if (read(sfd,bufin,SF_BUFSIZE)!=SF_BUFSIZE){
			fprintf(stderr,
			"sndin: error reading samples\n");
			exit(-1);
		}
		while(i<SF_BUFSIZE/sizeof(float)){
		    printf("%f\n",flin[i]);
		    i+=Chans[j++];
		    	if(j==nchans+1){j=0;i+=Chans[j++];}
		}
		i -= (SF_BUFSIZE/sizeof(float));
		begin_b += SF_BUFSIZE;
	}
	if (read(sfd,bufin,end_b-begin_b)!=end_b-begin_b){
		fprintf(stderr, "sndin: error reading samples\n");
		exit(-1);
	}
	while(i<(end_b-begin_b)/sizeof(float)){
	    printf("%f\n",flin[i]);
	    i+=Chans[j++];
	    	if(j==nchans+1){j=0;i+=Chans[j++];}
	}
	return(0);
}
