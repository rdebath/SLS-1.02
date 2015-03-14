/*
 * mftext
 * 
 * Convert a MIDI file to verbose text.
 */

#include <stdio.h>
#include <ctype.h>
#include "midifile.h"

static FILE *F;
int SECONDS;      /* global that tells whether to display seconds or ticks */
int division;        /* from the file header */
long tempo = 500000; /* the default tempo is 120 beats/minute */

filegetc()
{
	return(getc(F));
}

/* for crack */
extern int arg_index;

main(argc,argv)
char **argv;
{
	FILE *efopen();
	char ch;

	SECONDS = 0;

	while((ch = crack(argc,argv,"s",0)) != NULL){
	    switch(ch){
		case 's' : SECONDS = 1; break;
		}
	    }

	if ( argc < 2 && !SECONDS || argc < 3 && SECONDS )
	    F = stdin;
	else
	    F = efopen(argv[arg_index],"r");

	initfuncs();
	Mf_getc = filegetc;
	midifile();
	fclose(F);
	exit(0);
}

FILE *
efopen(name,mode)
char *name;
char *mode;
{
	FILE *f;
	extern int errno;
	extern char *sys_errlist[];
	extern int sys_nerr;
	char *errmess;

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
}

txt_trackstart()
{
	printf("Track start\n");
}

txt_trackend()
{
	printf("Track end\n");
}

txt_noteon(chan,pitch,vol)
{
	prtime();
	printf("Note on, chan=%d pitch=%d vol=%d\n",chan+1,pitch,vol);
}

txt_noteoff(chan,pitch,vol)
{
	prtime();
	printf("Note off, chan=%d pitch=%d vol=%d\n",chan+1,pitch,vol);
}

txt_pressure(chan,pitch,press)
{
	prtime();
	printf("Pressure, chan=%d pitch=%d press=%d\n",chan+1,pitch,press);
}

txt_parameter(chan,control,value)
{
	prtime();
	printf("Parameter, chan=%d c1=%d c2=%d\n",chan+1,control,value);
}

txt_pitchbend(chan,msb,lsb)
{
	prtime();
	printf("Pitchbend, chan=%d msb=%d lsb=%d\n",chan+1,msb,lsb);
}

txt_program(chan,program)
{
	prtime();
	printf("Program, chan=%d program=%d\n",chan+1,program);
}

txt_chanpressure(chan,press)
{
	prtime();
	printf("Channel pressure, chan=%d pressure=%d\n",chan+1,press);
}

txt_sysex(leng,mess)
char *mess;
{
	prtime();
	printf("Sysex, leng=%d\n",leng);
}

txt_metamisc(type,leng,mess)
char *mess;
{
	prtime();
	printf("Meta event, unrecognized, type=0x%02x leng=%d\n",type,leng);
}

txt_metaspecial(type,leng,mess)
char *mess;
{
	prtime();
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
	prtime();
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
	prtime();
	printf("Meta event, sequence number = %d\n",num);
}

txt_metaeot()
{
	prtime();
	printf("Meta event, end of track\n");
}

txt_keysig(sf,mi)
{
	prtime();
	printf("Key signature, sharp/flats=%d  minor=%d\n",sf,mi);
}

txt_tempo(ltempo)
long ltempo;
{
	tempo = ltempo;
	prtime();
	printf("Tempo, microseconds-per-MIDI-quarter-note=%d\n",tempo);
}

txt_timesig(nn,dd,cc,bb)
{
	int denom = 1;
	while ( dd-- > 0 )
		denom *= 2;
	prtime();
	printf("Time signature=%d/%d  MIDI-clocks/click=%d  32nd-notes/24-MIDI-clocks=%d\n",
		nn,denom,cc,bb);
}

txt_smpte(hr,mn,se,fr,ff)
{
	prtime();
	printf("SMPTE, hour=%d minute=%d second=%d frame=%d fract-frame=%d\n",
		hr,mn,se,fr,ff);
}

txt_arbitrary(leng,mess)
char *mess;
{
	prtime();
	printf("Arbitrary bytes, leng=%d\n",leng);
}

prtime()
{
    if(SECONDS)
	printf("Time=%f   ",mf_ticks2sec(Mf_currtime,division,tempo));
    else
	printf("Time=%ld  ",Mf_currtime);
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
