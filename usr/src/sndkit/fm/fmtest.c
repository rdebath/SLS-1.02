/* FMTEST by Rob Hooft (hooft@chem.ruu.nl) */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <getopt.h>
#include <linux/soundcard.h>

struct sbi_instrument instr;
static int sb;
static unsigned char sbbuf[404];
static int sbptr = 0;
static int instrf;

void sbflush()
{
	if (!sbptr) return;

	if (write(sb, sbbuf, sbptr) == -1)
	{
		perror("write sb");
		exit(-1);
	}

	sbptr=0;
}

void sbwrite(char *msg)
{
	if (sbptr>400) sbflush();

	memcpy(&sbbuf[sbptr], msg, 4);
	sbptr +=4;
}

void midich(char c)
{
	char buf[4];

	buf[0] = 5;
	buf[1] = c;
	sbwrite(buf);
}

void noteon(int chan,int pitch,int vol)
{
	char buf[4];
#ifdef DEBUG
	printf("Note on, chan=%d pitch=%d vol=%d\n",chan+1,pitch,vol);
#endif

	if (chan >= 0)
	{
		buf[0] = SEQ_FMNOTEON;
		buf[1] = chan;
		buf[2] = pitch;
		buf[3] = vol;
		sbwrite(buf);	
	} else {
		midich(0x90+chan);
		midich(pitch);
		midich(vol);
	}
}

void noteoff(int chan,int pitch,int vol)
{
	char buf[4];
#ifdef DEBUG
	printf("Note off, chan=%d pitch=%d vol=%d\n",chan+1,pitch,vol);	
#endif

	if (chan >= 0)
	{
		buf[0] = SEQ_FMNOTEOFF;
		buf[1] = chan;
		buf[2] = pitch;
		buf[3] = vol;
		sbwrite(buf);	
	} else {
		midich(0x80+chan);
		midich(pitch);
		midich(vol);
	}
}

void wait(int delay)
{
	int jiffies;
	jiffies = (delay << 8) | SEQ_WAIT;
	sbwrite((char*)&jiffies);
}

volatile void usage(char *s) 
{
	fprintf(stderr,
		"usage: %s [-h] [-o octave] [-t time] [-i instr]\n",s);
	fprintf(stderr,
		"       [-v volume] [-c channel] [sbifile]\n");
	fprintf(stderr,
		" Default values are: -o 3 -t 50 -i 0 -v 64 -c 0\n");
	exit(9);
}

int main(int argc,char **argv)
{
	char buf[100];
	int l,i,opt;
	int nr_instr = -1, octave = 3, time = 50, volume = 64, channel = 0;

	extern int optind;
	extern char *optarg;
	
	while ((opt=getopt(argc,argv,"c:o:i:t:v:h"))!=-1) {
		switch(opt) {
			case 'o': if (sscanf(optarg,"%d",&octave)<1) {
					fprintf(stderr,"-o: option error\n"); 
					exit(2);
				  }
				  break;
			case 'i': if (sscanf(optarg,"%d",&nr_instr)<1) {
					fprintf(stderr,"-i: option error\n"); 
					exit(2);
				  }
				  break;
			case 't': if (sscanf(optarg,"%d",&time)<1) {
					fprintf(stderr,"-t: option error\n"); 
					exit(2);
				  }
				  break;
			case 'v': if (sscanf(optarg,"%d",&volume)<1) {
					fprintf(stderr,"-v: option error\n"); 
					exit(2);
				  }
				  break;
			case 'c': if (sscanf(optarg,"%d",&channel)<1) {
					fprintf(stderr,"-c: option error\n"); 
					exit(2);
				  }
				  break;
			default : usage(argv[0]);
		}
	}
	if ((sb=open("/dev/sequencer", O_WRONLY, 0))==-1)
	{
		perror("/dev/sequencer");
		exit(-1);
	}
	if (argc == optind+1) {
		/* load instrument file into # 129 */

		if ((instrf=open(argv[optind], O_RDONLY, 0))==-1) {
			perror(argv[optind]);
			exit(1);
		} else if ((l=read(instrf, buf, 100))==-1) {
			perror(argv[optind]);
			exit(1);
		} else if (buf[0]!='S' || buf[1]!='B' || buf[2]!='I') {
			fprintf(stderr,"%s: Not SBI file\n",argv[optind]);
			exit(1);
		} else if (l<51) {
			fprintf(stderr,"%s: Short file\n",argv[optind]);
			exit(1);
		} else {
			/* do not overwrite first 128 instruments by default */
			if (nr_instr < 0) nr_instr = 128;
			instr.channel = nr_instr;
		
			for (i=0;i<16;i++) {
				instr.operators[i]=buf[i+0x24];
			}
	
			if (ioctl(sb, SNDCTL_FM_LOAD_INSTR, &instr)==-1) 
				perror("/dev/sequencer");
		}
		close(instrf);
	} else if (argc > optind) {
		usage(argv[0]);
	}
	buf[0] = SEQ_FMPGMCHANGE;
	buf[1] = channel;		/* program voice #channel to */
	if (nr_instr<0) {
		buf[2] = 0; 		/* either play default instrument 0 */
	} else {
		buf[2] = nr_instr; 	/* or play requested instrument */
	}
	sbwrite(buf);
	noteon(channel,0+12*octave,volume); 
        wait(1*time);
	noteoff(channel,0+12*octave,volume);
	noteon(channel,2+12*octave,volume);
        wait(2*time);
	noteoff(channel,2+12*octave,volume);
	noteon(channel,4+12*octave,volume);
        wait(3*time);
	noteoff(channel,4+12*octave,volume);
	noteon(channel,5+12*octave,volume);
        wait(4*time);
	noteoff(channel,5+12*octave,volume);
	noteon(channel,7+12*octave,volume);
        wait(5*time);
	noteoff(channel,7+12*octave,volume);
	noteon(channel,9+12*octave,volume);
        wait(6*time);
	noteoff(channel,9+12*octave,volume);
	noteon(channel,11+12*octave,volume);
        wait(7*time);
	noteoff(channel,11+12*octave,volume);
	noteon(channel,12+12*octave,volume);
        wait(8*time);
	noteoff(channel,12+12*octave,volume);
	sbflush();
	exit(0);
}
