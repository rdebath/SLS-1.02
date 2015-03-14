/* ftof.c	1.1	(IRCAM)	7/25/85	14:04:18 */
#include <stdio.h>
#include <local/sfheader.h>

extern int itty;
extern long begin_b, end_b;
extern int sfd;
extern int	chans;
extern char	bufin[],bufout[];

ftof()
{
	register long i,j;
	register float *fl;

	if(sflseek(sfd,begin_b,0)<0){
		fprintf(stderr,"sndout: error seeking to begin\n");
		exit(-1);
	}
	if (!itty){
	    if(end_b<0){
		while((i = fgetfbuf(bufin,SF_BUFSIZE/sizeof(float),stdin) *
					sizeof(float)) == SF_BUFSIZE){
			j = write(sfd,bufin,i);
			if (j!=SF_BUFSIZE) {
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
		}
		j = write(sfd,bufin,i);
		if (j!=i) {
			fprintf(stderr,
			"sndout: error writing samples\n");
			exit(-1);
		}
	    }
	    else {
		while(begin_b+SF_BUFSIZE < end_b){
			i = fgetfbuf(bufin,SF_BUFSIZE/sizeof(float),stdin) *
					sizeof(float);
			j = write(sfd,bufin,i);
			if (i!=SF_BUFSIZE){
				fprintf(stderr,
				"sndout: not enought samples\n");
				exit(-1);
			}
			else if (j!=i) {
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
			begin_b += SF_BUFSIZE;
		}
		i = fgetfbuf(bufin,(end_b-begin_b)/sizeof(float),stdin) *
				sizeof(float);
		j = write(sfd,bufin,i);
		if (i != end_b - begin_b){
			fprintf(stderr, "sndout: not enought samples\n");
			exit(-1);
		}
		else if (j!=i) {
			fprintf(stderr, "sndout: error writing samples\n");
			exit(-1);
		}
	    }
	    return(0);
	}

	fl = (float *) bufin;
	if (end_b < 0) {
	    while(scanf("%f",fl++) != EOF){
		if(fl >= (float *) (bufin + SF_BUFSIZE)) {
			i = write(sfd,bufin,SF_BUFSIZE);
			fl = (float *) bufin;
			if (i!=SF_BUFSIZE){
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
		}
	    }
	    fl--;
	    i = write(sfd,bufin,(char *)fl-bufin);
	    if (i!=(char *)fl-bufin){
		    fprintf(stderr,
		    "sndout: error writing samples\n");
		    exit(-1);
	    }
	}
	else {
	    while(begin_b+SF_BUFSIZE < end_b){
		for (fl= (float *) bufin;fl<(float *) (bufin+SF_BUFSIZE);)
			if(scanf("%f",fl++)==EOF){
				fl--;
				i = write(sfd,bufin,(char *)fl-bufin);
	    			if (i!=(char *)fl-bufin){
		    			fprintf(stderr,
		    			"sndout: error writing samples\n");
		    			exit(-1);
	    			}
				fprintf(stderr,
				"sndout: not enought samples\n");
				exit(-1);
			}
		i = write(sfd,bufin,SF_BUFSIZE);
	    	if (i!=SF_BUFSIZE){
			fprintf(stderr,
		    	"sndout: error writing samples\n");
		    	exit(-1);
	    	}
		begin_b += SF_BUFSIZE;
	    }

	    for (fl= (float *) bufin;fl<(float *) (bufin+end_b-begin_b);)
		if(scanf("%f",fl++)==EOF){
			fl--;
			i = write(sfd,bufin,(char *)fl-bufin);
	    		if (i!=(char *)fl-bufin){
	    			fprintf(stderr,
	    			"sndout: error writing samples\n");
	    			exit(-1);
			}
			fprintf(stderr,
			"sndout: not enought samples\n");
			exit(-1);
		}
	    i = write(sfd,bufin,end_b-begin_b);
	    if (i!=end_b-begin_b){
		    fprintf(stderr,
	    	    "sndout: error writing samples\n");
	    	    exit(-1);
	    }
	}
	return(0);
}
