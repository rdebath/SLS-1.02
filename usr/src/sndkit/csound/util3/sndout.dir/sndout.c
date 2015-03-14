/* sndout.c	1.2	(IRCAM)	8/1/85	16:32:06 */
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <local/sfheader.h>
#include <stdio.h>
#include <carl/sndio.h>

int itty, header = 1;
char endfmt;
long beg = 0, tend;
char *cmode = NULL, *cbeg = NULL, *cend = NULL, *cchans = NULL, *csrate = NULL, *name;
int sfd;
SFHEADER sfh;
struct stat sfstat;
char	bufin[SF_BUFSIZE*2],bufout[SF_BUFSIZE];
long begin_b,end_b,size_b;
char	*getsfname();

main(argc, argv)
	char **argv;
{
	char ipak = PMFLOAT;
	char opak = 'x';
	char ch;
	int force=0, append=0, headexists=0;
	unsigned short mode=0644;

	itty = isatty(0);

	while ((ch = crack(argc, argv, "b|e|d|o|i|c|R|p|hnt", 0)) != NULL) {
		switch (ch) {
			case 'b': cbeg = arg_option; break;
			case 'e': endfmt = ch; cend = arg_option; break;
			case 'd': endfmt = ch; cend = arg_option; break;
			case 'o': opak = *arg_option; break;
			case 'i': ipak = *arg_option; break;
			case 'c': cchans = arg_option; break;
			case 'R': csrate = arg_option; break;
			case 'p': cmode = arg_option;
			case 't': itty = 1; break;
			case 'n': force++; break;
			case 'h': usage(0);
			default: usage(1);
		}
	}

	if (argc == arg_index) 
		name = getsfname(DEFNM);
	else 
		name = getsfname(argv[arg_index]);

	if (!access(name,F_OK)){
		if(!stat(name,&sfstat)){
			mode = sfstat.st_mode & 0777;
			size_b = sfbsize(&sfstat);
		}
		if((sfd=open(name,0)) >= 0){
			if(read(sfd,&sfh,sizeof(SFHEADER))==sizeof(SFHEADER))
				if (ismagic(&sfh))
					headexists++;
			close(sfd);
		}
	}
	if (!headexists){
		sfsrate(&sfh) = 16000.0;
		sfchans(&sfh) = 1;
		sfclass(&sfh) = SF_SHORT;
		sfh.sfinfo.sf_magic = SF_MAGIC;
	}

	if(ipak==PM16BIT)
		set_sample_size(sizeof(short));
	if (!itty)
		if (rhead(&sfh, &ipak, stdin) != 0) {
			fprintf(stderr, "sndout: error reding header\n");
			exit(1);
		}

	if (cchans != NULL)
		sfchans(&sfh) = (int) sfexpr(cchans, 1.0); 
	if (csrate != NULL)
		sfsrate(&sfh) = sfexpr(csrate, 1.0); 
	if (cmode != NULL)
		sscanf(cmode,"%o",&mode);
	if (cbeg != NULL)
		beg = sfexpr(cbeg, sfsrate(&sfh));
	if (cend != NULL) {
		if (endfmt == 'e')
			tend = sfexpr(cend, sfsrate(&sfh));
		else
			tend = beg + sfexpr(cend, sfsrate(&sfh));
	} else 
		tend = -1;

	if (opak == PMFLOAT)
		sfclass(&sfh) = SF_FLOAT;
	else if (opak == PM16BIT)
		sfclass(&sfh) = SF_SHORT;

	begin_b = beg<0? size_b : beg*sfchans(&sfh)*sfclass(&sfh);
	end_b = tend*sfchans(&sfh)*sfclass(&sfh);
	if((end_b>0)&&(begin_b>=end_b)){
		fprintf(stderr,"sndout: begin >= end\n");
		exit(-1);
	}
	
	if ((cbeg != NULL) || (cend != NULL)) append++;
	
	if (!access(name,F_OK)) {
		if (access(name,W_OK)){
			fprintf(stderr,
				"sndout: no write permission on %s\n",name);
			exit(-1);
		}
		if(force) {
			fprintf(stderr, 
			    "sndout: %s file exists\n",
				name);
			exit(-1);
		}
	}

	if((sfd=open(name,append?O_RDWR:O_WRONLY|O_CREAT|O_TRUNC,mode)) < 0){
		fprintf(stderr,"sndout: cannot open %s\n",name);
		exit(-1);
	}
	if(write(sfd,&sfh,sizeof(SFHEADER))!=sizeof(SFHEADER)){
		fprintf(stderr,"sndout: cannot write header on %s\n",name);
		close(sfd);
		exit(-1);
	}

	switch (ipak) {
		case PM16BIT:	
			switch (sfclass(&sfh)) {
				case SF_SHORT:	
					if (stos() != 0)
						oerr();
					break;
				case SF_FLOAT:
					if (stof() != 0)
						oerr();
					break;
			}
		break;
		case PMFLOAT:
			switch (sfclass(&sfh)) {
				case SF_SHORT:	
					if (ftos() != 0)
						oerr(); 
					break;
				case SF_FLOAT:
					if (ftof() != 0)
						oerr();
					break;
			}
		break;
		default: 
			usage(2);
	}
	
	close(sfd);
	exit();
}

oerr()
{
	fprintf(stderr, "sndout: error writing samples\n");
}

usage(x)
{
fprintf (
stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
"usage: sndout [flags] [filename] < input\n",
"input format:\n",
"\tif stdin is a tty, reads arabic numbers,\n",
"\telse if stdin is a file or pipe, reads floatsams\n",
"flags:\n",
"-iX\tX overrides default input format\n",
"-oX\tX overrides default output format, X can be\n",
"\t\tf pipe: floatsams, tty: floating point\n",
"\t\ts pipe: shortsams, tty: integer\n",
"\tNOW ONLY flot to flot AND short to short IMPLEMENTED\n",
"-t force arabic input (even if input is file or pipe)\n",
"-bN\t set begin time to N\n",
"-eN\t set end time to N\n",
"-dN\t set duration to N\n",
"-RN\t set sampling rate to N\n",
"-pN\t set protection to N\n",
"-n\t don't overwrite file if already exists\n",
"default usage: sndout -if -os test\n"
);
exit(x);
}
