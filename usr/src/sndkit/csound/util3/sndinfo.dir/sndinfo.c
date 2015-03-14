/* sndinfo.c	1.1	(IRCAM)	8/1/85	17:50:41 */
#include <sys/types.h>
#include <sys/stat.h>
#include <local/sfheader.h>
#include <stdio.h>
#include <carl/sndio.h>

char	*getsfname();
int sfd;
SFHEADER sfh;
struct stat sfstat;

main(argc,argv)
	char **argv;
{
	char opak = PMFLOAT;
	char ch;
	int result;
	char *name;
	int i = 1;
	int	samp=0,dur=0,chan=0,rate=0,pack=0,flag=0;

	while ((ch = crack(argc, argv, "SdcRP", 0)) != NULL) {
		switch (ch) {
			case 'S' :
				samp=1;flag++;break;
			case 'd' :
				dur=1;flag++;break;
			case 'c' :
				chan=1;flag++;break;
			case 'R' :
				rate=1;flag++;break;
			case 'P' :
				pack=1;flag++;break;
			default: usage(1);
		}
	}

	if (argc == arg_index) 
		name = getsfname(DEFNM);
	else 
	while(arg_index<argc){
	    name = getsfname(argv[arg_index++]);

	    result=0;
	    readopensf(name,sfd,sfh,sfstat,"infobsd",result)
	    /* do all boundary calculations in sample frames */
	    if (result==0) {
	    if(!flag)
	    printf( 
    "\t%s:\n%s:\t\t%f\n%s:\t%d\n%s:\t\t%s\n%s\t\t\t%d samples\t%f seconds\n",
		name,
		"Sampling rate",sfsrate(&sfh),
		"Number of channels",sfchans(&sfh),
		"Packing mode",sfclass(&sfh)==SF_SHORT?"short":"float",
		"Length", sfbsize(&sfstat)/sfclass(&sfh),
		sfbsize(&sfstat)/sfchans(&sfh)/sfsrate(&sfh)/sfclass(&sfh));
	    else{
	        if(samp)
		    printf("%d\n", sfbsize(&sfstat)/sfclass(&sfh));
	        if(dur)
		    printf("%f\n",
			    sfbsize(&sfstat)/sfchans(&sfh)/
				    sfsrate(&sfh)/sfclass(&sfh));
	        if(chan)
		    printf("%d\n", sfchans(&sfh));
	        if(rate)
		    printf("%f\n", sfsrate(&sfh));
	        if(pack)
		    printf("%d\n", sfclass(&sfh));
	    }
        }
	close(sfd);
    }
}
usage(i)
{
	fprintf(stderr,
		"usage: infobsd [-R] [-c] [-P] [-S] [-d] name .. ..\n");
}
