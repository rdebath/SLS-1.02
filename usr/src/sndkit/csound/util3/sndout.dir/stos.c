/* stos.c       1.2     (IRCAM) 8/1/85  16:29:58 */
#include <stdio.h>
#include <local/sfheader.h>

extern int itty;
extern long begin_b, end_b;
extern int sfd;
extern SFHEADER sfh;
extern struct stat sfstat;
extern int	chans;
extern char     bufin[],bufout[];

stos()
{
	register long i,j;
	register short *sh;

	if(sflseek(sfd,begin_b,0)<0){
		fprintf(stderr,"sndout: error seeking to begin\n");
		exit(-1);
	}
	if (!itty){
	    if(end_b<0){
		while ((i = fgetsbuf(bufin,SF_BUFSIZE/sizeof(short),stdin) *
		    sizeof(short)) == SF_BUFSIZE) {
	/*      while((i = fread(bufin,1,SF_BUFSIZE,stdin)) == SF_BUFSIZE){*/
	/* (bug fix 9/85 msp) */
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
	    else{
		while(begin_b+SF_BUFSIZE < end_b){
			i = fgetsbuf(bufin,SF_BUFSIZE/sizeof(short),stdin) *
						sizeof(short);
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
		i = fgetsbuf(bufin,(end_b-begin_b)/sizeof(short),stdin) *
						sizeof(short);
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

	sh = (short *) bufin;
	if (end_b < 0) {
	    while(scanf("%d",sh++) != EOF){
		if(sh >= (short *) (bufin + SF_BUFSIZE)) {
			i = write(sfd,bufin,SF_BUFSIZE);
			sh = (short *) bufin;
			if (i!=SF_BUFSIZE){
				fprintf(stderr,
				"sndout: error writing samples\n");
				exit(-1);
			}
		}
	    }
	    sh--;
	    i = write(sfd,bufin,(char *)sh-bufin);
	    if (i!=(char *)sh-bufin){
		    fprintf(stderr,
		    "sndout: error writing samples\n");
		    exit(-1);
	    }
	}
	else {
	    while(begin_b+SF_BUFSIZE < end_b){
		for (sh= (short *) bufin;sh<(short *) (bufin+SF_BUFSIZE);)
			if(scanf("%d",sh++)==EOF){
				sh--;
				i = write(sfd,bufin,(char *)sh-bufin);
	    			if (i!=(char *)sh-bufin){
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

	    for (sh= (short *) bufin;sh<(short *) (bufin+end_b-begin_b);)
		if(scanf("%d",sh++)==EOF){
			sh--;
			i = write(sfd,bufin,(char *)sh-bufin);
	    		if (i!=(char *)sh-bufin){
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
