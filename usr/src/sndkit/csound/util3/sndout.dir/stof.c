/* stof.c	1.1	(IRCAM)	7/25/85	14:04:23 */
#include <stdio.h>
#include <local/sfheader.h>

extern int itty;
extern long begin_b, end_b;
extern int sfd;
extern SFHEADER sfh;
extern struct stat sfstat;
extern int	chans;
extern char	bufin[],bufout[];

stof()
{
	register long i,j;
	register float *fl;
	register short *sh, *l;
	register short sho;

	if(sflseek(sfd,begin_b,0)<0){
		fprintf(stderr,"sndout: error seeking to begin\n");
		exit(-1);
	}
	if (!itty){
	    if(end_b<0){
		while((i = fgetsbuf(bufin,SF_BUFSIZE/2/sizeof(short),stdin) *
					sizeof(short)) == SF_BUFSIZE/2){
			sh = (short *) bufin;
			fl = (float *) bufout;
			l = (short *) (bufin + i);
			while(sh < l)
				*fl++ = (float) (*sh++ / 32767.0);
			j = write(sfd,bufout,i*2);
			if (j!=SF_BUFSIZE) {
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
		}
		sh = (short *) bufin;
		fl = (float *) bufout;
		l = (short *) (bufin + i);
		while(sh < l)
			*fl++ = (float) (*sh++ / 32767.0);
		j = write(sfd,bufout,i*2);
		if (j!=i*2) {
			fprintf(stderr,
			"sndout: error writing samples\n");
			exit(-1);
		}
	    }
	    else{
		while(begin_b+SF_BUFSIZE < end_b){
			i = fgetsbuf(bufin,SF_BUFSIZE/2/sizeof(short),stdin) *
					sizeof(short);
			sh = (short *) bufin;
			fl = (float *) bufout;
			l = (short *) (bufin + i);
			while(sh < l)
				*fl++ = (float) (*sh++ / 32767.0);
			j = write(sfd,bufout,i*2);
			if (i!=SF_BUFSIZE/2) {
				fprintf(stderr,
				"sndout: not enought samples\n");
				exit(-1);
			}
			else if (j!=i*2) {
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
			begin_b += SF_BUFSIZE;
		}
		sh = (short *) bufin;
		fl = (float *) bufout;
		i = fgetsbuf(bufin,(end_b-begin_b)/2/sizeof(short),stdin) *
				sizeof(short);
		l = (short *) (bufin + i);
		while(sh < l)
			*fl++ = (float) (*sh++ / 32767.0);
		j = write(sfd,bufout,i*2);
		if (i != (end_b - begin_b)/2){
			fprintf(stderr, "sndout: not enought samples\n");
			exit(-1);
		}
		else if (j!=i*2) {
			fprintf(stderr, "sndout: error writing samples\n");
			exit(-1);
		}
	    }
	    return(0);
	}

	fl = (float *) bufout;
	if (end_b < 0) {
	    while(scanf("%d",&sho) != EOF){
		*fl++ = (float) (sho / 32767.0);
		if(fl >= (float *) (bufout + SF_BUFSIZE)) {
			i = write(sfd,bufout,SF_BUFSIZE);
			fl = (float *) bufout;
			if (i!=SF_BUFSIZE){
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
		}
	    }
	    i = write(sfd,bufout,(char *)fl-bufout);
	    if (i!=(char *)fl-bufout){
		    fprintf(stderr,
		    "sndout: error writing samples\n");
		    exit(-1);
	    }
	}
	else {
	    while(begin_b+SF_BUFSIZE < end_b){
		for (fl= (float *) bufout;fl<(float *) (bufout+SF_BUFSIZE);
				*fl++=(float) (sho/32767.0))
			if(scanf("%d",&sho)==EOF){
				i = write(sfd,bufout,(char *)fl-bufout);
	    			if (i!=(char *)fl-bufout){
		    			fprintf(stderr,
		    			"sndout: error writing samples\n");
		    			exit(-1);
	    			}
				fprintf(stderr,
				"sndout: not enought samples\n");
				exit(-1);
			}
		i = write(sfd,bufout,SF_BUFSIZE);
	    	if (i!=SF_BUFSIZE){
			fprintf(stderr,
		    	"sndout: error writing samples\n");
		    	exit(-1);
	    	}
		begin_b += SF_BUFSIZE*2;
	    }

	    for (fl= (float *) bufout;fl<(float *) (bufout+end_b-begin_b);
				*fl++ = (float) (sho/32767.0))
		if(scanf("%d",&sho)==EOF){
			i = write(sfd,bufout,(char *)fl-bufout);
	    		if (i!=(char *)fl-bufout){
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
