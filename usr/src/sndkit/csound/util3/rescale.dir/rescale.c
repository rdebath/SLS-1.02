/* fast program to rescale from floating point file to integer file.
   It will create an integer file called  filename.short if you don't
   specify an output file. */

#include <local/sfheader.h>
#include "rescale.h"
#include <stdio.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#define  BUFSIZE 32768

static SFCODE	ampcode = {
	SF_MAXAMP,
	sizeof(SFMAXAMP) + sizeof(SFCODE)
}; 

/*
static SFCODE	commentcode = {
	SF_COMMENT,
	MINCOMM + sizeof(SFCODE)
	};
*/

extern void sfbcopy();
extern SFCODE *getsfcode();

main(argc,argv)
  int argc;
  char *argv[];
{
	SFMAXAMP sfm,sfmnew;
/*	SFCOMMENT sfcm;       */
	SFHEADER sfh1,sfh2;
	SFCODE *sizer;
	struct stat sfst1,sfst2;

	char *cp,*sfin,*sfout;
	double atof();
	short outbuffer[BUFSIZE];
	float inbuffer[BUFSIZE];
	int i,n,bytes,words,inbytes,outbytes,sf1,sf2,result,readbytes,durbytes;
	int empty,skipbytes,outskipbytes,newfile;
	float opeak,factor,skpin,dur,newpeak,outskip;
	char command[256];
	char newname[128];
	char *point,*point2,*strcat();
	short *bufp;

	if(argc == 1) {
usage:		printf("usage: -s inskip -d dur -o outskip -p peakamp -e emptybuffers_at_end -z [create new file] inputfile [outputfile]\n");
		printf("defaults: s=0, d=to_eof, o=0, p=input_peak, e=1, write over old file, outputfile=inputfile.short\n");
		exit(0);
		}
	newfile=skpin=dur=newpeak=outskip=0;

	empty = 1;  /* defaults to 1 empty buffers at end*/

	while((*++argv)[0] == '-') {
		argc -= 2;
		for(cp = argv[0]+1; *cp; cp++) {
			switch(*cp) {
			case 's': 
				skpin = atof(*++argv);
				printf("input skip = %f\n",skpin);
				break;
			case 'd': 
				dur = atof(*++argv);
				printf("rescale duration = %f\n",dur);
				empty = 0; /* no empty buffers for splice */
				break;
			case 'p': 
				newpeak = atof(*++argv);
				printf("specified peak = %f\n",newpeak);
				break;
			case 'o': 
				outskip = atof(*++argv);
				printf("output skip =  %f\n",outskip);
				break;
			case 'e':
				empty = atof(*++argv);
				printf("write %d empty buffers at end\n",empty);
				break;
			case 'z':
				newfile = 1;
				break;
			default:
				printf("uh-oh, unkown option\n");
				goto usage;
			}
		}
	}
	sfin = argv[0];
	sfout = argv[1];
	readopensf(sfin,sf1,sfh1,sfst1,"rescale",result);
	if(result < 0) {
		close(sf1);
		exit(1);
	}
	/* do input skip on input file*/
	if(skipbytes = 
		skpin * sfclass(&sfh1) * sfsrate(&sfh1) * sfchans(&sfh1)) {
		skipbytes -= skipbytes % (sfclass(&sfh1) * sfchans(&sfh1));
			/* make sure it lands on sample block */
		if(sflseek(sf1,skipbytes,0) == -1) {
			printf("Bad skip on input file\n");
			exit(1);
		}
	}

/*	printsf(&sfh1);
*/
	if(sfclass(&sfh1) == SF_SHORT) {
		printf("Note: Input file has short integers.\n");
	}

	if ((cp = (char *)getsfcode(&sfh1,SF_MAXAMP)) != NULL) {
	    sfbcopy(cp + sizeof(SFCODE), (char *) &sfm, sizeof(SFMAXAMP));
printf("did bcopy\n");
	    for(i=0,opeak=0; i<sfchans(&sfh1); i++) 
		if(sfmaxamp(&sfm,i) > opeak) opeak = sfmaxamp(&sfm,i);
printf("did opeak\n");
	    opeak = newpeak ? newpeak : opeak;
	    printf("Peak amplitude of input file is %e\n",opeak);
	    factor = 32767./opeak;
	    printf("factor = %f\n",factor);
	}
	else die("input file has no max-ampcode values");

/*	if((cp=getsfcode(&sfh1,SF_COMMENT)) != NULL)  {
	    sizer = (SFCODE *) cp;
	    sfbcopy(cp + sizeof(SFCODE) , (char *) &sfcm, sizer->bsize);
	}
*/
	newrwopensf(sfout,sf2,sfh2,sfst2,"rescale",result,2);
	if(result < 0) {
		if(sfout == NULL) {
			point = sfin;
			if(sfclass(&sfh1) == SF_FLOAT) 
				point = strcat(sfin,".short");
			else
				point = strcat(sfin,".xshort");
		}
		else
			point = sfout;

		sfmagic(&sfh2) = SF_MAGIC;
		sfclass(&sfh2) = SF_SHORT;
		sfchans(&sfh2) = sfchans(&sfh1);
		sfsrate(&sfh2) = sfsrate(&sfh1);
		if((sf2 = open(point,O_CREAT|O_RDWR,0644)) < 0 ) {
			printf("Can't open file %s\n",point);
			exit(-2);
		}
		if(newfile) ftruncate(sf2,SIZEOF_HEADER);

		for(i=0; i<sfchans(&sfh2); i++) {
			sfmaxamp(&sfmnew,i)=sfmaxamp(&sfm,i)*factor;
			sfmaxamploc(&sfmnew,i)=sfmaxamploc(&sfm,i);
		}
		sfmaxamptime(&sfmnew) = sfmaxamptime(&sfm);
		if (putsfcode(&sfh2, &ampcode, &sfmnew) < 0)
		        die("error in writing new ampcode");
/*		if (putsfcode(&sfh2, &commentcode, &sfcm) < 0) {
			printf("comment didn't get written, sorry!\n");
			exit(-1);
		}
*/		printf ("\nCreating output file: %s\n",point);
		if(wheader(sf2,(char *)&sfh2)) {
	       		printf("Can't seem to write header on file %s\n",point);
			perror("main");
			exit(-1);
		}
	}
/*	else printsf(&sfh2);
*/
	if(sfclass(&sfh2) != SF_SHORT) {
		printf("Output file must have short integers.\n");
		exit(-1);
	}
	/* do output skip*/
	if(outskipbytes = 
		  outskip * sfclass(&sfh2) * sfsrate(&sfh2) * sfchans(&sfh2)) {
		outskipbytes -= 
			      outskipbytes % (sfclass(&sfh2) * sfchans(&sfh2));
			/* make sure it lands on sample block */
		if(sflseek(sf2,outskipbytes,0) == -1) {
			printf("Bad skip on output file\n");
			exit(1);
		}
	}

	readbytes = inbytes = BUFSIZE * sfclass(&sfh1);

	durbytes = dur * sfclass(&sfh1) * sfchans(&sfh1) * sfsrate(&sfh1);
	
	fflush (stdout);
	fprintf(stderr,"Rescaling....\t");

	bufp = (short *)inbuffer;

	while(1) {
		if(dur) {
			inbytes = (durbytes > readbytes) ? inbytes : durbytes;
			durbytes -= inbytes;
		}
		if((bytes = read(sf1,(char *)inbuffer,inbytes)) <= 0) {
			printf("reached eof on input\n");
			close(sf1);
			break;
		}
		words = bytes/sfclass(&sfh1);
		outbytes = words * SF_SHORT;

		if(sfclass(&sfh1) == SF_SHORT)
			for(i=0; i<words; i++) 
				outbuffer[i] = (short)((float)bufp[i] * factor);
		else
			for(i=0; i<words; i++)
				outbuffer[i] = (short) (inbuffer[i] * factor);
		if(write(sf2,(char *)outbuffer,outbytes) != outbytes) {
			printf("Bad write on output file\n");
			close(sf1);
			close(sf2);
			exit(0);
		}
	}
	/* write  empty buffers */
	for(i=0; i<words; i++) outbuffer[i] = 0;
	for(i=0; i<empty; i++)
		if(n=write(sf2,(char *)outbuffer,outbytes) != outbytes) {
			printf("Bad write on output file\n");
			close(sf2);
			exit(0);
		}
	close(sf1); close(sf2);

	printf("\ndone.\n");
}

die(s)
 char *s;
{
       printf("%s\n",s);
       exit(1);
}
