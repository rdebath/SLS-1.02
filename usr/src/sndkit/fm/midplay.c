/*
 * 
 * midplay by Hannu Savolainen (hsavolai@cs.helsinki.fi)
 *
 * Copies a 1 track MIDI -file to the MIDI port.
 * This program is based on midifile/mftext made by Michael Czeiszperger.
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "midifile/midifile.h"
#include <linux/soundcard.h>

int SECONDS;      /* global that tells whether to display seconds or ticks */
int division;        /* from the file header */
static int mf;
long tempo = 500000; /* the default tempo is 120 beats/minute */

static int sb;
static unsigned char sbbuf[404];
static int sbptr = 0;

static unsigned char mfbuf[1024];
static int mbuflen=0, mbufptr = 1024;

static int chn_pgm[16] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};

fileseek(long pos)
{
	mbufptr=1024;
	mbuflen=0;

	lseek(mf, pos, 0);
}

filegetc()
{
	if (mbufptr>=mbuflen)
	{
		if ((mbuflen=read(mf, mfbuf, 1024))<1) return EOF;	/* EOF */
		mbufptr=0;
	}

	return mfbuf[mbufptr++];
}

/* for crack */
extern int arg_index;

void sbflush()
{
	int i;
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
  while(1){
       bb[0]=(unsigned)(buffer & 0xff);
       write(fd, bb, 1);
       
	if(buffer & 0x80)
		buffer >>= 8;
	else
		return;
	}
}/* end of WriteVarLen */

void wr_int(int fd, unsigned long data)
{
	unsigned char buf[4];
	int i;

    buf[0]=(unsigned)((data >> 24) & 0xff);
    buf[1]=(unsigned)((data >> 16) & 0xff);
    buf[2]=(unsigned)((data >> 8 ) & 0xff);
    buf[3]=(unsigned)(data & 0xff);
	write(fd, buf, 4);
}

void prepare_for_CMF()
{
	int newfile, oldfile;

	unsigned short division = 40;

	struct sbi_instrument idata;

	struct stat st;

	unsigned char def_hd[14] = {'M', 'T', 'h', 'd', 0, 0, 0, 6, 0, 0, 0, 1};

	char *trakid = "MTrk";

	char buf[1024];
	int l, i, j;

	struct CMF_Header
		{
			char	fid[4];		/* File ID ("CTMF")	*/
			short	vers;		/* Version number (1.0 = 0x0100)	*/
			short	instr_offs;	/* Offset to the instrument block	*/
			short	midi_offs;	/* Offset to the MIDI data	*/
			short	ticks_per_beat;	/* Num. of ticks per quarter note	*/
			short	ticks_per_sec;	/* Num. of ticks per second	*/
			short	tittle_offs;	/* Pointer to tittle */
			short 	composer_offs;	/* Pointer to composer name */
			short	remarks_offs;	/* Pointer to remarks */
			char	chn_in_use[16];	/* 1 if channel is in use */
			short	num_of_instr;	/* Number of instruments in the instrument block */
			short	basic_tempo;	/* Main tempo used in the music */
		} hdr;

	lseek(mf, 0, 0);

	if (read(mf, (char*)&hdr, sizeof(hdr)) != sizeof(hdr))
	{
		perror("CMF -header");
		exit(-1);
	}

	printf(" Ver 0x%4x Offset to MIDI data 0x%x\n", hdr. vers, hdr.midi_offs);

	printf("#instr %d, tempo %d, Ticks: %d,%d\n", hdr.num_of_instr, hdr.basic_tempo,
		hdr.ticks_per_beat, hdr.ticks_per_sec);

	if (hdr.num_of_instr > 16) hdr.num_of_instr=16;

	fileseek(hdr.instr_offs);

	for (i=0;i<hdr.num_of_instr;i++)
	{
		idata.channel=i;
		for (j=0;j<16;j++) idata.operators[j]=filegetc();

		if (ioctl(sb, SNDCTL_FM_LOAD_INSTR, &idata)==-1) perror("ioctl");
	}

	for (i=0;i<9;i++)
	{
		buf[0] = SEQ_FMPGMCHANGE;
		buf[1] = i;
		buf[2] = i;
		sbwrite(buf);
	}

	fileseek((long)hdr.midi_offs);

	newfile = open("/tmp/cmf_file.mid", O_RDWR|O_CREAT, 0644);

	def_hd[12] = division >> 8;
	def_hd[13] = division  & 0xff;

	write(newfile, def_hd, sizeof(def_hd));
	write(newfile, trakid, 4);

	fstat(mf, &st);

	wr_int(newfile, st.st_size-hdr.midi_offs);

	while ((l=read(mf,buf,1024))>0) write(newfile, buf, l);

	close(mf);
	mf=newfile;
	fileseek(0);
	
}

void detect_filetype()
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

main(argc,argv)
char **argv;
{
	FILE *efopen();
	char ch;

	SECONDS = 0;

	while((ch = crack(argc,argv,"s",0)) != 0){
	    switch(ch){
		case 's' : SECONDS = 1; break;
		}
	    }

	if ( argc < 2 && !SECONDS || argc < 3 && SECONDS )
	    mf = 0;
	else
	    if ((mf = open(argv[arg_index],O_RDONLY, 0))==-1)
	    {
	    	perror(argv[arg_index]);
	    	exit(-1);
	    }

	if ((sb=open("/dev/sequencer", O_WRONLY, 0))==-1)
	{
		perror("/dev/sequencer");
		exit(-1);
	}

	detect_filetype();

	initfuncs();
	Mf_getc = filegetc;
	midifile();
	close(mf);
	sbflush();
	exit(0);
}

FILE *
efopen(name,mode)
char *name;
char *mode;
{
	FILE *f;
	extern int errno;
	extern int sys_nerr;
	const char *errmess;

	if ( (f=fopen(name,mode)) == NULL ) {
		(void) fprintf(stderr,"*** ERROR *** Cannot open '%s'!\n",name);
		if ( errno <= sys_nerr )
			errmess = sys_errlist[errno];
		else
			errmess = "Unknown error!";
		(void) fprintf(stderr,"************* Reason: %s\n",errmess);
		exit(1);
	}
	return(f);
}

error(s)
char *s;
{
	fprintf(stderr,"Error: %s\n",s);
}

txt_header(format,ntrks,ldivision)
{
        division = ldivision; 
	printf("Header format=%d ntrks=%d division=%d\n",format,ntrks,division);
	if (ntrks != 1) 
	{
		printf("This program supports only 1 tracks/midifile\n");
		exit(1);
	}
}

txt_trackstart()
{
	char buf[4];
	printf("Track start\n");

/*	buf[0] = SEQ_SYNCTIMER;
	sbwrite(buf);	*/
}

txt_trackend()
{
	printf("Track end\n");
}

txt_noteon(chan,pitch,vol)
{
	char buf[4];
	midisync();
/*	printf("Note on, chan=%d pitch=%d vol=%d\n",chan+1,pitch,vol);	*/

	midich(0x90+chan);
	midich(pitch);
	midich(vol);
}

txt_noteoff(chan,pitch,vol)
{
	char buf[4];
	midisync();
/*	printf("Note off, chan=%d pitch=%d vol=%d\n",chan+1,pitch,vol);	*/

	midich(0x80+chan);
	midich(pitch);
	midich(vol);
}

txt_pressure(chan,pitch,press)
{
	timestamp();
	printf("Pressure, chan=%d pitch=%d press=%d\n",chan+1,pitch,press);
}

txt_parameter(chan,control,value)
{
	timestamp();
	printf("Parameter, chan=%d c1=%d c2=%d\n",chan+1,control,value);
}

txt_pitchbend(chan,msb,lsb)
{
	timestamp();
/*	printf("Pitchbend, chan=%d msb=%d lsb=%d\n",chan+1,msb,lsb);	*/
}

txt_program(chan,program)
{
	midisync();
/*	printf("Program, chan=%d program=%d\n",chan+1,program);	*/

	chn_pgm[chan] = program;
	midich(0xa0 + chan);
	midich(program);
}

txt_chanpressure(chan,press)
{
	timestamp();
/*	printf("Channel pressure, chan=%d pressure=%d\n",chan+1,press);	*/
}

txt_sysex(leng,mess)
char *mess;
{
	timestamp();
	printf("Sysex, leng=%d\n",leng);
}

txt_metamisc(type,leng,mess)
char *mess;
{
	timestamp();
	printf("Meta event, unrecognized, type=0x%02x leng=%d\n",type,leng);
}

txt_metaspecial(type,leng,mess)
char *mess;
{
	timestamp();
	printf("Meta event, sequencer-specific, type=0x%02x leng=%d\n",type,leng);
}

txt_metatext(type,leng,mess)
char *mess;
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

txt_metaseq(num)
{
	timestamp();
	printf("Meta event, sequence number = %d\n",num);
}

txt_metaeot()
{
	timestamp();
	printf("Meta event, end of track\n");
}

txt_keysig(sf,mi)
{
	timestamp();
	printf("Key signature, sharp/flats=%d  minor=%d\n",sf,mi);
}

txt_tempo(ltempo)
long ltempo;
{
	tempo = ltempo;
	timestamp();
	printf("Tempo, microseconds-per-MIDI-quarter-note=%d\n",tempo);
}

txt_timesig(nn,dd,cc,bb)
{
	int denom = 1;
	while ( dd-- > 0 )
		denom *= 2;
	timestamp();
	printf("Time signature=%d/%d  MIDI-clocks/click=%d  32nd-notes/24-MIDI-clocks=%d\n",
		nn,denom,cc,bb);
}

txt_smpte(hr,mn,se,fr,ff)
{
	timestamp();
	printf("SMPTE, hour=%d minute=%d second=%d frame=%d fract-frame=%d\n",
		hr,mn,se,fr,ff);
}

txt_arbitrary(leng,mess)
char *mess;
{
	timestamp();
	printf("Arbitrary bytes, leng=%d\n",leng);
}

timestamp()
{
	return 0;
}

midisync()
{
	float time;
	unsigned jiffies, diff;

	static int prevtime = 0;

	time = mf_ticks2sec(Mf_currtime,division,tempo);
	jiffies = (unsigned) (time * 100);

/*	printf("Time=%4d=%f (%4d)   ", Mf_currtime, time, jiffies);	*/

	if (jiffies > prevtime)
	{
	/*	if ((jiffies-prevtime) > 100) printf(" Long wait %d (%d) ", jiffies-prevtime, jiffies);	*/
		jiffies = (jiffies << 8) | SEQ_WAIT;
		sbwrite((char*)&jiffies);
	}

	prevtime = jiffies;
}

initfuncs()
{
	Mf_error = error;
	Mf_header =  txt_header;
	Mf_trackstart =  txt_trackstart;
	Mf_trackend =  txt_trackend;
	Mf_noteon =  txt_noteon;
	Mf_noteoff =  txt_noteoff;
	Mf_pressure =  txt_pressure;
	Mf_parameter =  txt_parameter;
	Mf_pitchbend =  txt_pitchbend;
	Mf_program =  txt_program;
	Mf_chanpressure =  txt_chanpressure;
	Mf_sysex =  txt_sysex;
	Mf_metamisc =  txt_metamisc;
	Mf_seqnum =  txt_metaseq;
	Mf_eot =  txt_metaeot;
	Mf_timesig =  txt_timesig;
	Mf_smpte =  txt_smpte;
	Mf_tempo =  txt_tempo;
	Mf_keysig =  txt_keysig;
	Mf_seqspecific =  txt_metaspecial;
	Mf_text =  txt_metatext;
	Mf_arbitrary =  txt_arbitrary;
}
