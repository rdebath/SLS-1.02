/*
 * 
 * fmplay by Hannu Savolainen (hsavolai@cs.helsinki.fi)
 * Modifications, bugfixes, and ANSIfied by Rob Hooft (hooft@chem.ruu.nl)
 *
 *
 * Plays a 1 track MIDI-file using AdLib -compatible FM -synthesizer card.
 * This program is based on midifile/mftext made by Michael Czeiszperger.
 *
 */

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <getopt.h>   /* Why use 'crack' if we have GNU getopt? */
#include <sys/stat.h>
#include "midifile/midifile.h"
#include <linux/soundcard.h>

/* some prototypes */

void initfuncs(void);
void midifile(void);
void midisync(void);
void fm_noteoff(int,int,int);

/* #undef DEBUG */
#ifdef DEBUG
void timestamp(void);
#endif

int division;        /* from the file header */
static int mf;
long tempo = 500000; /* the default tempo is 120 beats/minute */

static int sb;
static unsigned char sbbuf[404];
static int sbptr = 0;

#define MFBUF_SIZE  (32768)
static unsigned char mfbuf[MFBUF_SIZE];
static int mbuflen=0, mbufptr = MFBUF_SIZE, mbufcount = 0;
static int volume_factor = 1000;

static int chn_pgm[16] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};

static int quiet;

/* temporary scratchfile */
char scrfil[50] = "/tmp/cmfXXXXXX";
static int input_is_temp = 0;

struct CMF_Header {
		char	fid[4];		/* File ID ("CTMF")		*/
		short	vers;		/* Version number (1.0 = 0x0100)*/
		short	instr_offs;	/* Offset to instrument block	*/
		short	midi_offs;	/* Offset to MIDI data		*/
		short	ticks_per_beat;	/* # of ticks per quarter note	*/
		short	ticks_per_sec;	/* # of ticks per second	*/
		short	tittle_offs;	/* Pointer to tittle 		*/
		short 	composer_offs;	/* Pointer to composer name 	*/
		short	remarks_offs;	/* Pointer to remarks 		*/
		char	chn_in_use[16];	/* 1 if channel is in use 	*/
		short	num_of_instr;	/* Number of instruments in 
						  the instrument block	*/
		short	basic_tempo;	/* Main tempo used in the music */
	};
/* 
 * Hi! Now my most important change to the first version of this program:
 * While the older one used one voice for each channel, and so could only
 * play one note for each channel at a time, this new version does some
 * kind of a dynamic allocation of voices for the notes. It generally tries
 * to use voices that are not yet in use, and otherwise stops the oldest 
 * voice playing to make space. This algorithm is very easy to adapt to
 * different number of supported simultaneous voices, but I have put
 * in a fixed number of 9 for the moment. Maybe once this number can be 
 * queried from the kernel driver in a future version (hint, hint).
 */
#define N_CELL		(9)
#define MAX_CELL	(N_CELL-1)
/*
 * keep track of the state of all cells, and keep a list of cells in
 * the order of preference for usage.
 */
static int cell_next[N_CELL]={1,2,3,4,5,6,7,8,0}; /* "circular linked list?" */
static int cell_chan[N_CELL]={-1,-1,-1,-1,-1,-1,-1,-1,-1};
static int cell_pitch[N_CELL]={-1,-1,-1,-1,-1,-1,-1,-1,-1};
/*
 * The next array is a lot worse. I needed it because in some
 * of my midi-files a note which is repeated is first started, and
 * at the same midi-time-step (but later in sequence) the previous
 * note is stopped. The 'restarted' array is used to help us remember 
 * not to stop it again, which would lead to 'clicking' and missing notes.
 *
 * This only goes wrong if too many simultaneous voices are needed,
 * and in that case the result will be unbearable anyway (just kidding).
 */
static int cell_restarted[N_CELL]={0,0,0,0,0,0,0,0,0};
static int cell_pointer=0;
/* 
 * Well, it is clear that 'lseek' will break playing from a pipe on
 * 'stdin'. We can try to salvage this by using a large buffer, and
 * do seeking in this buffer instead.
 */
void fileseek(long pos)
{
	if (mbufcount==1 && pos < mbuflen) {
		mbufptr=pos;
	} else {
		/* let's hope it is not a PIPE-IN */
		mbuflen=0;
		mbufptr=MFBUF_SIZE;
		if (lseek(mf, pos, 0)<0) {
			perror("lseek on input");
			exit(3);
		}
	}
}

int filegetc(void)
{
	if (mbufptr>=mbuflen)
	{
		if ((mbuflen=read(mf, mfbuf, MFBUF_SIZE))<1) return EOF;
		mbufptr=0;
		mbufcount++;
	}
	return mfbuf[mbufptr++];
}

void sbflush(void)
{
	if (!sbptr) return;

	if (write(sb, sbbuf, sbptr) == -1) {
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

void wr_len(int fd, unsigned long value)
{
  unsigned long buffer;
  unsigned char bb[2];

  buffer = value & 0x7f;
  while((value >>= 7) > 0)
  {
	buffer <<= 8;
	buffer |= 0x80;
	buffer += (value & 0x7f);
  }
  while(1) {
       bb[0]=(unsigned)(buffer & 0xff);
       write(fd, bb, 1);
       
	if(buffer & 0x80)
		buffer >>= 8;
	else
		return;
   }
}

void wr_int(int fd, unsigned long data)
{
	unsigned char buf[4];

	buf[0]=(unsigned)((data >> 24) & 0xff);
	buf[1]=(unsigned)((data >> 16) & 0xff);
	buf[2]=(unsigned)((data >> 8 ) & 0xff);
	buf[3]=(unsigned)(data & 0xff);
	write(fd, buf, 4);
}

void prepare_for_CMF(void)
{
	int newfile;

	unsigned short division = 40;

	struct sbi_instrument idata;

	struct stat st;

	unsigned char def_hd[14] = {'M', 'T', 'h', 'd', 0, 0, 0, 6, 0, 0, 0, 1};

	char *trakid = "MTrk";

	char buf[1024];
	int l, i, j;

	struct CMF_Header hdr;

	lseek(mf, 0, 0);

	if (read(mf, (char*)&hdr, sizeof(hdr)) != sizeof(hdr))
	{
		perror("CMF -header");
		exit(-1);
	}
	if (!quiet) {
		printf(" Ver 0x%4x Offset to MIDI data 0x%x\n", hdr. vers, hdr.midi_offs);

		printf("#instr %d, tempo %d, Ticks: %d,%d\n", hdr.num_of_instr, hdr.basic_tempo,
		hdr.ticks_per_beat, hdr.ticks_per_sec);
	}
	fileseek(hdr.instr_offs);

	for (i=0;i<hdr.num_of_instr;i++)
	{
		idata.channel=i;
		for (j=0;j<16;j++) idata.operators[j]=filegetc();

		if (ioctl(sb, SNDCTL_FM_LOAD_INSTR, &idata)==-1) perror("ioctl");
	}

	fileseek((long)hdr.midi_offs);
        mktemp(scrfil);
	if ((newfile = open(scrfil, O_RDWR|O_CREAT, 0644))<0) {
		perror(scrfil);
		exit(9);
	}

	def_hd[12] = division >> 8;
	def_hd[13] = division  & 0xff;

	write(newfile, def_hd, sizeof(def_hd));
	write(newfile, trakid, 4);

	fstat(mf, &st);

	wr_int(newfile, st.st_size-hdr.midi_offs);

	/* write the buffered part */
	write(newfile,&mfbuf[mbufptr],mbuflen-mbufptr);

	/* and the rest of the data */
	while ((l=read(mf,buf,1024))>0) write(newfile, buf, l);

	close(mf);
	mf=newfile;
	/* invalidate input buffer: NEW FILE! */
	mbuflen=0; 
	/* And look for the start of the file */
	fileseek(0);

	/* Mark inputfile for deletion */
	input_is_temp = 1;
}

void detect_filetype(void)
{
	switch (filegetc())
	{
	case 'M':
		if (filegetc() == 'T')
		if (filegetc() == 'h')
		if (filegetc() == 'd')
		{
			printf("MIDI -file\n");
			fileseek(0);
			return;
		}
		break;

	case 'C':
		if (filegetc() == 'T')
		if (filegetc() == 'M')
		if (filegetc() == 'F')
		{
			printf("CMF -file\n");
			prepare_for_CMF();
			return;
		}
		break;
	default: ;
	}
	fprintf(stderr, "File type not recognized.\n");
	exit(-1);
}

int main(int argc,char **argv)
{
	FILE *efopen();
	extern int optind;
	extern char * optarg;
	int opt;
        float tmp;

	while((opt = getopt(argc,argv,"qv:")) != -1) {
	    switch(opt) {
		case 'q': quiet = 1; break;
		case 'v': sscanf(optarg,"%f",&tmp);
			  if (tmp>5) tmp=5;
			  if (tmp<0.1) tmp=0.1;
			  volume_factor=(int) 1000.0*tmp;
			  break;
		default : fprintf(stderr,"usage: %s [-q] [-v volume] [midifile|cmffile]\n",argv[0]);
			  exit(-1);
	    }
	}

	if ( argc == optind )
	    mf = 0;
	else
	    if ((mf = open(argv[optind],O_RDONLY, 0))==-1) {
	    	perror(argv[optind]);
	    	exit(-1);
	    }

	if ((sb=open("/dev/sequencer", O_WRONLY, 0))==-1) {
		perror("/dev/sequencer");
		exit(-1);
	}

	detect_filetype();

	initfuncs();
	Mf_getc = filegetc;
	midifile();
	close(mf);
	sbflush();
	if (input_is_temp) unlink(scrfil);
	exit(0);
}

FILE * efopen(char *name,char *mode)
{
	FILE *f;
	extern int errno;
	extern int sys_nerr;
	const char *errmess;

	if ( (f=fopen(name,mode)) == NULL ) {
		(void) fprintf(stderr,"%s: cannot open\n",name);
		if ( errno <= sys_nerr )
			errmess = sys_errlist[errno];
		else
			errmess = "Unknown error";
		(void) fprintf(stderr,"Reason: %s\n",errmess);
		exit(1);
	}
	return(f);
}

void error(char *s)
{
	fprintf(stderr,"Error: %s\n",s);
	sbflush();
	if (input_is_temp) unlink(scrfil);
	exit(9);
}

void fm_header(int format,int ntrks,int ldivision)
{
        division = ldivision; 
	if (!quiet)
	printf("Header format=%d ntrks=%d division=%d\n",format,ntrks,division);
	if (ntrks != 1) {
		fprintf(stderr,"This program supports only 1 tracks/midifile\n");
		fprintf(stderr,"Try converting this file by mf1to0\n");
		exit(1);
	}
}

void fm_trackstart(void)
{
	char buf[4];
#ifdef DEBUG
	printf("Track start\n");
#endif
	buf[0] = SEQ_SYNCTIMER;
	sbwrite(buf);
}

void fm_noteon(int chan,int pitch,int vol)
{
	char buf[4];
	int cell;
	midisync();
	
/* CMF files call note_on with vol=0 to switch a note off. */
	if (!vol) {
		fm_noteoff(chan,pitch,vol);
		return;
	}

	if (chan >= 0) {
		/* first determine: is this note already running ? */
		cell=0; 
		while (cell<N_CELL && (cell_chan[cell]!=chn_pgm[chan] || cell_pitch[cell]!=pitch)) {
			++cell;
		}
		if (cell<N_CELL) {
#ifdef DEBUG
			printf("Hey! This note is already running!\n");
#endif
			/* terminate identical running note first */
			fm_noteoff(chan,pitch,vol);
			/* And AFTER putting it to silence, mark it as 
			   restarted, such that it will not be stopped 
			   immediately.  */
			cell_restarted[cell_pointer]=1;
		}
		/* now allocate the cell, and move the pointer */
		cell=cell_pointer;
		cell_pointer=cell_next[cell_pointer];		
		cell_pitch[cell]=pitch;
#ifdef DEBUG
		printf("Note on, chan=%d pitch=%d",chan,pitch);
		printf(" -- selected cell %d\n",cell);
#endif
		if (1) /*if (cell_chan[cell] != chn_pgm[chan])*/ {
#ifdef DEBUG
			printf("Loading instrument %d into cell.\n",chn_pgm[chan]);
#endif
			buf[0] = SEQ_FMPGMCHANGE;
			buf[1] = cell;
			buf[2] = chn_pgm[chan];
			sbwrite(buf);
			cell_chan[cell]=chn_pgm[chan];
		}
		buf[0] = SEQ_FMNOTEON;
		buf[1] = cell;
		buf[2] = pitch;
		buf[3] = (volume_factor*vol)/1000;
		sbwrite(buf);	
#ifdef DEBUG_STACK
		{ int this,num;
		printf("CellStack:");
		this=cell_pointer;
		for (num=0;num<N_CELL;num++) {
			printf("%4d",this); this=cell_next[this];
		}
		printf("\n"); }
#endif
	} else {
		midich(0x90+chan);
		midich(pitch);
		midich(vol);
	}
}

void fm_noteoff(int chan,int pitch,int vol)
{
	char buf[4];
	int cell,before,last;
	midisync();
#ifdef DEBUG
	printf("Note off, chan=%d pitch=%d ",chan,pitch);
#endif
	if (chan >= 0)
	{
        	cell=0;
		while (cell_chan[cell]!=chn_pgm[chan] || cell_pitch[cell]!=pitch) {
			if (++cell>8) {
#ifdef DEBUG
				printf("Has already been used again\n");
#endif
				return;
			}
		}
		if (cell_restarted[cell]) {
#ifdef DEBUG
			printf("Has only just been restarted. Ignored.\n");
#endif
			return;
		}
#ifdef DEBUG
		printf("-- Found at cell %d\n",cell);
#endif
		buf[0] = SEQ_FMNOTEOFF;
		buf[1] = cell;
		buf[2] = pitch;
		buf[3] = (volume_factor*vol)/1000;
		sbwrite(buf);	
		if (cell!=cell_pointer) {
			before=0;
			while (cell_next[before]!=cell) { 
				if (++before>MAX_CELL) {
					fprintf(stderr,"Can not happen 1\n");
					return;
				}
			}
			last=0;
			while (cell_next[last]!=cell_pointer) { 
				if (++last>MAX_CELL) {
					fprintf(stderr,"Can not happen 2\n");
					return;
				}
			}
			if (last != cell) {
				cell_next[before]=cell_next[cell];
				cell_next[last]=cell;
       				cell_next[cell]=cell_pointer;
			}
			cell_pointer=cell;
			cell_pitch[cell]=-1;
#ifdef DEBUG_STACK
			{ int this,num;
			printf("CellStack:");
			this=cell_pointer;
			for (num=0;num<N_CELL;num++) {
				printf("%4d",this); this=cell_next[this];
			}
			printf("\n"); }
#endif
		}
	} else {
		midich(0x80+chan);
		midich(pitch);
		midich(vol);
	}
}

void fm_program(int chan,int program)
{
#ifdef DEBUG
	timestamp();
	printf("Program, chan=%d program=%d\n",chan,program);
#endif
	chn_pgm[chan] = program;
}

#ifdef DEBUG
void fm_pressure(int chan,int pitch,int press)
{
	timestamp();
	printf("IGNORE Pressure, chan=%d pitch=%d press=%d\n",chan+1,pitch,press);
}

void fm_parameter(int chan,int control,int value)
{
	timestamp();
	printf("IGNORE Parameter, chan=%d c1=%d c2=%d\n",chan+1,control,value);
}

void fm_pitchbend(int chan,int msb,int lsb)
{
	timestamp();
	printf("IGNORE Pitchbend, chan=%d msb=%d lsb=%d\n",chan+1,msb,lsb);
}

void fm_chanpressure(int chan,int press)
{
	timestamp();
	printf("Channel pressure, chan=%d pressure=%d\n",chan+1,press);
}

void fm_sysex(int leng,char *mess)
{
	timestamp();
	printf("Sysex, leng=%d\n",leng);
}

void fm_metamisc(int type,int leng,char *mess)
{
	timestamp();
	printf("Meta event, unrecognized, type=0x%02x leng=%d\n",type,leng);
}

void fm_metaspecial(int type,int leng,char *mess)
{
	timestamp();
	printf("Meta event, sequencer-specific, type=0x%02x leng=%d\n",type,leng);
}

void fm_metatext(int type,int leng,char *mess)
{
	static char *ttype[] = {
		NULL,
		"Text Event",		/* type=0x01 */
		"Copyright Notice",	/* type=0x02 */
		"Sequence/Track Name",
		"Instrument Name",	/* ...       */
		"Lyric",
		"Marker",
		"Cue Point",		/* type=0x07 */
		"Unrecognized"
	};
	int unrecognized = (sizeof(ttype)/sizeof(char *)) - 1;
	register int n, c;
	register char *p = mess;

	if ( type < 1 || type > unrecognized )
		type = unrecognized;
	timestamp();
	printf("Meta Text, type=0x%02x (%s)  leng=%d\n",type,ttype[type],leng);
	printf("     Text = <");
	for ( n=0; n<leng; n++ ) {
		c = *p++;
		printf( (isprint(c)||isspace(c)) ? "%c" : "\\0x%02x" , c);
	}
	printf(">\n");
}

void fm_metaseq(int num)
{
	timestamp();
	printf("Meta event, sequence number = %d\n",num);
}

void fm_metaeot(void)
{
	timestamp();
	printf("Meta event, end of track\n");
}

void fm_keysig(int sf,int mi)
{
	timestamp();
	printf("Key signature, sharp/flats=%d  minor=%d\n",sf,mi);
}

void fm_tempo(long ltempo)
{
	tempo = ltempo;
	timestamp();
	printf("Tempo, microseconds-per-MIDI-quarter-note=%d\n",tempo);
}

void fm_timesig(int nn,int dd,int cc,int bb)
{
	int denom = 1;
	while ( dd-- > 0 )
		denom *= 2;
	timestamp();
	printf("Time signature=%d/%d  MIDI-clocks/click=%d  32nd-notes/24-MIDI-clocks=%d\n",
		nn,denom,cc,bb);
}

void fm_smpte(int hr,int mn,int se,int fr,int ff)
{
	timestamp();
	printf("SMPTE, hour=%d minute=%d second=%d frame=%d fract-frame=%d\n",
		hr,mn,se,fr,ff);
}

void fm_arbitrary(int leng,char *mess)
{
	timestamp();
	printf("Arbitrary bytes, leng=%d\n",leng);
}

void fm_trackend(void)
{
	printf("Track end\n");
}

void timestamp(void)
{
	return;
}
#endif

void midisync(void)
{
	float time;
	unsigned jiffies;

	static int prevtime = 0;
	int i;

	time = mf_ticks2sec(Mf_currtime,division,tempo);
	jiffies = (unsigned) (time * 100);
#ifdef DEBUG_EVEN_MORE
	printf("Time=%4d=%f (%4d)   ", Mf_currtime, time, jiffies);
#endif
	if (jiffies > prevtime)
	{
#ifdef DEBUG
		printf("Jiffies now is %d (was %d)\n",jiffies,prevtime);
		if ((jiffies-prevtime) > 100) printf(" Long wait %d (%d) ", jiffies-prevtime, jiffies);
#endif
		/* this command was too late: jiffies would 
                   sometimes be messed up */
		prevtime = jiffies; 
		jiffies = (jiffies << 8) | SEQ_WAIT;
		sbwrite((char*)&jiffies);
/* Now, since time has changed, clear the cell_restarted array */
		for (i=0;i<N_CELL;i++) cell_restarted[i]=0;
	}

}
/*
 * How do I get rid of the Warning messages in the following routine
 * when compiled with gcc -Wall? Only by modifying midifile.h?
 */
void initfuncs(void)
{
	Mf_error = error;
	Mf_header =  fm_header;
	Mf_noteon =  fm_noteon;
	Mf_noteoff =  fm_noteoff;
	Mf_program =  fm_program;
	Mf_trackstart =  fm_trackstart;
#ifdef DEBUG
	Mf_trackend =  fm_trackend;
	Mf_pressure =  fm_pressure;
	Mf_parameter =  fm_parameter;
	Mf_pitchbend =  fm_pitchbend;
	Mf_chanpressure =  fm_chanpressure;
	Mf_sysex =  fm_sysex;
	Mf_metamisc =  fm_metamisc;
	Mf_seqnum =  fm_metaseq;
	Mf_eot =  fm_metaeot;
	Mf_timesig =  fm_timesig;
	Mf_smpte =  fm_smpte;
	Mf_tempo =  fm_tempo;
	Mf_keysig =  fm_keysig;
	Mf_seqspecific =  fm_metaspecial;
	Mf_text =  fm_metatext;
	Mf_arbitrary =  fm_arbitrary;
#endif
}
