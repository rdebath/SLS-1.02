/* ftos.c	1.1	(IRCAM)	7/25/85	14:04:19 */
#include <stdio.h>
#include <local/sfheader.h>

extern int itty;
extern long begin_b, end_b;
extern int sfd;
extern char	bufin[],bufout[];

ftos()
{
	register long i,j;
	register float *fl, *l;
	register short *sh;
	register float flo;

	if(sflseek(sfd,begin_b,0)<0){
		fprintf(stderr,"sndout: error seeking to begin\n");
		exit(-1);
	}
	if (!itty){
	    if(end_b<0){
		while((i = fgetfbuf(bufin,SF_BUFSIZE*2/sizeof(float),stdin) *
					sizeof(float)) == SF_BUFSIZE*2){
			fl = (float *) bufin;
			sh = (short *) bufout;
			l = (float *) (bufin + i);
			while(fl < l)
				*sh++ = (short) (*fl++ * 32767.0);
			j = write(sfd,bufout,i/2);
			if (j!=SF_BUFSIZE) {
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
		}
		fl = (float *) bufin;
		sh = (short *) bufout;
		l = (float *) (bufin + i);
		while(fl < l)
			*sh++ = (short) (*fl++ * 32767.0);
		j = write(sfd,bufout,i/2);
		if (j!=i/2) {
			fprintf(stderr,
			"sndout: error writing samples\n");
			exit(-1);
		}
	    }
	    else{
		while(begin_b+SF_BUFSIZE < end_b){
			i = fgetfbuf(bufin,SF_BUFSIZE*2/sizeof(float),stdin) *
					sizeof(float);
			fl = (float *) bufin;
			sh = (short *) bufout;
			l = (float *) (bufin + i);
			while(fl < l)
				*sh++ = (short) (*fl++ * 32767.0);
			j = write(sfd,bufout,i/2);
			if (i!=SF_BUFSIZE*2) {
				fprintf(stderr,
				"sndout: not enought samples\n");
				exit(-1);
			}
			else if (j!=i/2) {
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
			begin_b += SF_BUFSIZE;
		}
		fl = (float *) bufin;
		sh = (short *) bufout;
		i = fgetfbuf(bufin,(end_b-begin_b)*2/sizeof(float),stdin) *
				sizeof(float);
		l = (float *) (bufin + i);
		while(fl < l)
			*sh++ = (short) (*fl++ * 32767.0);
		j = write(sfd,bufout,i/2);
		if (i != (end_b - begin_b)*2){
			fprintf(stderr, "sndout: not enought samples\n");
			exit(-1);
		}
		else if (j!=i/2) {
			fprintf(stderr, "sndout: error writing samples\n");
			exit(-1);
		}
	    }
	    return(0);
	}

	sh = (short *) bufout;
	if (end_b < 0) {
	    while(scanf("%f",&flo) != EOF){
		*sh++ = (short) (flo * 32767.0);
		if(sh >= (short *) (bufout + SF_BUFSIZE)) {
			i = write(sfd,bufout,SF_BUFSIZE);
			sh = (short *) bufout;
			if (i!=SF_BUFSIZE){
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
		}
	    }
	    i = write(sfd,bufout,(char *)sh-bufout);
	    if (i!=(char *)sh-bufout){
		    fprintf(stderr,
		    "sndout: error writing samples\n");
		    exit(-1);
	    }
	}
	else {
	    while(begin_b+SF_BUFSIZE < end_b){
		for (sh= (short *) bufout;sh<(short *) (bufout+SF_BUFSIZE);
				*sh++=(short) (flo*32767.0))
			if(scanf("%f",&flo)==EOF){
				i = write(sfd,bufout,(char *)sh-bufout);
	    			if (i!=(char *)sh-bufout){
		    			fprintf(stderr,
		    			"sndout: error writing samples\n");
		    			exit(-1);
	    			}
				fprintf(stderr,
				"sndout: not enought samples\n");
				exit(-1);
			}
		i = write(sfd,bufout,SF_BUFSIZE/2);
	    	if (i!=SF_BUFSIZE/2){
			fprintf(stderr,
		    	"sndout: error writing samples\n");
		    	exit(-1);
	    	}
		begin_b += SF_BUFSIZE;
	    }

	    for (sh= (short *) bufout;sh<(short *) (bufout+end_b-begin_b);
				*sh++ = (short) (flo*32767.0))
		if(scanf("%f",&flo)==EOF){
			i = write(sfd,bufout,(char *)sh-bufout);
	    		if (i!=(char *)sh-bufout){
	    			fprintf(stderr,
	    			"sndout: error writing samples\n");
	    			exit(-1);
			}
			fprintf(stderr,
			"sndout: not enought samples\n");
			exit(-1);
		}
	    i = write(sfd,bufout,end_b-begin_b);
	    if (i!=end_b-begin_b){
		    fprintf(stderr,
	    	    "sndout: error writing samples\n");
	    	    exit(-1);
	    }
	}
	return(0);
}
