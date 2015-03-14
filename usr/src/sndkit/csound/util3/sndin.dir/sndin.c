/* sndin.c	1.1	(IRCAM)	7/25/85	13:26:19 */
#include <sys/types.h>
#include <sys/stat.h>
#include <local/sfheader.h>
#include <stdio.h>
#include <carl/sndio.h>

int otty, header = 1;
char *chanspec, endfmt;
long beg = 0, tend;
char *cbeg, *cend, *name;
int sfd;
SFHEADER sfh;
struct stat sfstat;
int	chans;
char	bufin[SF_BUFSIZE],bufout[SF_BUFSIZE*2];
long begin_b,end_b;
char	*getsfname();
extern	int	nchans;

main(argc, argv)
	char **argv;
{
	char opak = PMFLOAT;
	char ch;
	int result;

	otty = isatty(1);

	while ((ch = crack(argc, argv, "b|e|d|o|c|Hht", 0)) != NULL) {
		switch (ch) {
			case 'b': cbeg = arg_option; break;
			case 'e': endfmt = ch; cend = arg_option; break;
			case 'd': endfmt = ch; cend = arg_option; break;
			case 'o': opak = *arg_option; break;
			case 'c': chanspec = arg_option; break;
			case 't': otty = 1; break;	/* force tty output */
			case 'H': header = 0; break;	/* no output header */
			case 'h': usage(0);
			default: usage(1);
		}
	}
	if (otty) 
		header = 0;	/* no header if output is a tty */
	/* open sound file */
	if (argc == arg_index) 
		name = getsfname(DEFNM);
	else 
		name = getsfname(argv[arg_index]);

	readopensf(name,sfd,sfh,sfstat,"sndin",result)
	if (result != 0) exit(result);
	/* do all boundary calculations in sample frames */
	if (cbeg != NULL) {
		beg = sfexpr(cbeg, sfsrate(&sfh));
		if (beg < 0) 
			beg = 0;
	}
	chans = sfchans(&sfh);
	if (cend != NULL) {
		if (endfmt == 'e')
			tend = sfexpr(cend, sfsrate(&sfh));
		else
			tend = beg + sfexpr(cend, sfsrate(&sfh));
	} else 
		tend = sfbsize(&sfstat) / sfclass(&sfh) / chans;

	begin_b = beg*chans*sfclass(&sfh);
	end_b = tend*chans*sfclass(&sfh);

	if (setchan(chanspec, chans)) 
		usage(2);
	if (header)
		if (whead(&sfh, chanspec, opak, name) != 0) {
			fprintf(stderr, "sndin: error writing header\n");
			exit(1);
		}

	if(chans==nchans)		/* output all channels */
	switch (opak) {
		case PM16BIT:	
			switch (sfclass(&sfh)) {
				case SF_SHORT:	
					if (stos() != 0)
						oerr();
					break;
				case SF_FLOAT:
					if (ftos() != 0)
						oerr();
					break;
			}
		break;
		case PMFLOAT:
			switch (sfclass(&sfh)) {
				case SF_SHORT:	
					if (stof() != 0)
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
	else
	switch (opak) {
		case PM16BIT:	
			switch (sfclass(&sfh)) {
				case SF_SHORT:	
					if (cstos() != 0)
						oerr();
					break;
				case SF_FLOAT:
					if (cftos() != 0)
						oerr();
					break;
			}
		break;
		case PMFLOAT:
			switch (sfclass(&sfh)) {
				case SF_SHORT:	
					if (cstof() != 0)
						oerr(); 
					break;
				case SF_FLOAT:
					if (cftof() != 0)
						oerr();
					break;
			}
		break;
		default: 
			usage(2);
	}
		
	exit();
}

oerr()
{
	fprintf(stderr, "sndin: error writing samples\n");
}

usage(x)
{
fprintf (
stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
"usage: sndin [flags] [filename] > output\n",
"output format:\n",
"\tif stdout is a tty, writes arabic numbers,\n",
"\telse if stdout is a file or pipe, writes floatsams\n",
"flags:\n",
"-oX\tX overrides default output format, X can be\n",
"\t\tf pipe: floatsams, tty: floating point\n",
"\t\ts pipe: shortsams, tty: integer\n",
"-t force arabic output (even if output is file or pipe)\n",
"-bN\t set begin time to N\n",
"-eN\t set end time to N\n",
"-dN\t set duration to N\n",
"-cS\tS is a comma-separated list of selected channel numbers\n",
"-H\t  suppress generating header.\n",
"default usage: sndin -of test\n"
);
exit(x);
}
