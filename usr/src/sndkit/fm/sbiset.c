/* Heavily modified by Rob Hooft (hooft@chem.ruu.nl) */

#include <unistd.h>
#include <sys/ioctl.h>
#include <linux/soundcard.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <getopt.h>

struct sbi_instrument instr;
		
#define USAGE { fprintf(stderr,"usage: %s [-v] sbi_file...\n",argv[0]);\
		exit(2); }

int main(int argc, char *argv[])
{
	char buf[100];
	int opt,offset;
	extern int optind;
	int verbose=0;

	int l,i, fm, op, instrf;

	if ((fm=open("/dev/sequencer", O_WRONLY, 0))==-1) 
	{
		perror("/dev/sequencer");
		exit(-1);
	}
	while ((opt = getopt(argc,argv,"v")) != -1) {
		switch (opt) {
		case 'v': verbose = 1; break;
		default : USAGE;
		}
	}
	if (argc == optind) USAGE;
	offset=optind;
	if (argc-offset>128) {
		argc=128+offset;
		fprintf(stderr,"Warning: Excess arguments ignored\n");
	}
	for (op=offset;op < argc;op++) {
		if ((instrf=open(argv[op], O_RDONLY, 0))==-1) {
			perror(argv[op]);
			offset++;
		} else if ((l=read(instrf, buf, 100))==-1) {
			perror(argv[op]);
			offset++;
		} else if (buf[0]!='S' || buf[1]!='B' || buf[2]!='I') {
			fprintf(stderr,"%s: Not SBI file\n",argv[op]);
			offset++;
		} else if (l<51) {
			fprintf(stderr,"%s: Short file\n",argv[op]);
			offset++;
		} else {	
			instr.channel = op-offset;
		
			for (i=0;i<16;i++) {
				instr.operators[i]=buf[i+0x24];
			}
	
			if (ioctl(fm, SNDCTL_FM_LOAD_INSTR, &instr)==-1) perror("/dev/sequencer");

			if (verbose) fprintf(stderr,"Loaded %d with %s\n",op-offset,argv[op]);
		}
		close(instrf);
	}
	if (verbose) 
		fprintf(stderr,"Initialised %d FM-instruments.\n",argc-offset);

	for (i=0;i<10;i++)
	{
		buf[0] = 3;
		buf[1] = i;
		buf[2] = i;
		write(fm, buf, 4);
	}

	if (offset == optind) exit(0); else exit(1);
}
