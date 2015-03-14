/*
 * f1to0.c 4/30/89
 *
 * Author:  Michael S. Czeiszperger
 * Purpose: The program converts format 1 MIDI files to format 0
 *          MIDI files.  It is meant to demonstrate the use of
 *          the midifile library.  The internal data format is
 *          a linked list of MIDI and SMF meta events.
 *
 *          The spec is available from:
 *               International MIDI Association
 *               5316 West 57th Street
 *               Los Angeles, CA 90056
 *
 *          An in-depth description of the spec can also be found
 *          in the article "Introducing Standard MIDI Files", published
 *          in Electronic Musician magazine, April, 1988.
 * 
 * usage: f1to0 infile outfile
 * 
 */
 
#include <stdio.h>
#include <ctype.h>

#ifdef THINK
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#include "midifile.h"

enum PACKET_TYPE { MIDI_EVENT, META_EVENT };

struct MIDIpacket	    /* Stucture for MIDI events */
{ 							
    enum PACKET_TYPE p_type; /* What type of packet is it?               */
    unsigned long time;      /* absolute time of event, in ms increments */
    unsigned int m_type;     /* What kind of MIDI message?               */
    unsigned int chan;       /* The MIDI channel                         */
    unsigned long len;       /* length of the midi message               */
    unsigned char *data;     /* pointer to the data                      */
    struct MIDIpacket *next,*before;           /* point to next packet */
} *sequence_start,*sequence_end;

/* 
   This example structure is not very portable. It does, however,
   mirror the logical operation of the header, and on some machines can
   be written and read directly.  It assumes that shorts are at least
   two bytes and chars are at least one byte.
*/
struct header_type {
	short format;
	short ntrks;
	union {
		short quarter_note;
		struct{
			char format;
			char resolution;
    		} smpte;
    	} division;
} header;

/* These lines are needed to use the library */
FILE *fpIn,*fpOut;
extern long Mf_currtime;
mygetc() { return(getc(fpIn)); }
myputc(c) { return(putc(c,fpOut));}

main(argc,argv)
int argc;
char **argv;
{
/* This is included of an example of what to do if you use THINK C on
   the mac. */
#ifdef THINK
    #include <console.h>
	    argc = ccommand(&argv);
#endif

    if (argc<2 || !strcmp(argv[1],"-") ) {
	fpIn=stdin;
    } else {
	if((fpIn = fopen(argv[1],"r")) == 0L)
	    printf("f1to0: unable to open file %s for reading.\n",argv[1]);
    }

    if (argc<3 || !strcmp(argv[2],"-") ) {
	fpOut=stdout;
    } else {
	if((fpOut = fopen(argv[2],"w")) == 0L)
	    printf("f1to0: unable to open file %s for writing.\n",argv[2]);
    }
    init_funcs();

    /* read the midi file */
    mfread();

    /* write a single track */
    mfwrite((int)header.format,1,(int)header.division.quarter_note,fpOut);
}

myheader(format,ntrks,division)
int format,ntrks,division;
{
    header.format = format;
    header.ntrks = ntrks;
    header.division.quarter_note = division;
}

myerror(s)
char *s;
{
	fprintf(stderr,"%10ld: %s\n",Mf_currtime,s);
	exit(9);
}

/*
 * mywritetrack()
 *
 * Sample showing how to use the library routines to write out a track.
 * Returns 1 if successful, and -1 if not.
 *
 */
int mywritetrack(track)
int track;
{
    struct MIDIpacket *current;
    unsigned long delta_time;

    current = sequence_start; /* init to point at the beginning of the list */

    /* shuffle through each element in the linked list, writing out
       the data in the standard MIDI format */
    while(current != 0L)
    {
        if(current == sequence_start)
           delta_time = current->time;
        else
           delta_time = current->time - (current->before)->time;
#ifdef DEBUG
printf("ticks = %lu event = %d packet = %d chan = %d len = %lu\n",
current->time,current->m_type,current->p_type,current->chan,current->len);
#endif

	if(current->p_type == MIDI_EVENT)
	{
	    if(mf_write_midi_event(delta_time,current->m_type,current->chan,current->data,current->len) < 0)
	        return(-1);
	}
	else if(current->p_type == META_EVENT)
	{
	    if(mf_write_meta_event(delta_time,current->m_type,current->data,current->len) < 0)
	        return(-1);
	}
	else
	    fprintf(stderr,"Unknown MIDI packet encounted.\n");

        current = current->next;
    }
    return(1);
} /* end of write_track() */


/*
 * This routine adds a MIDI or META event into the linked list.  
 */
void add_packet(ticks,event_type,packet_type,chan,data,len)
unsigned long ticks;                 /* absolute ticks of event */
int event_type;                      /* note_on, note_off, etc  */
enum PACKET_TYPE packet_type;        /* What kind of data?      */
unsigned char *data;                 /* already allocated data  */
unsigned long len;                   /* length of data          */
{
    struct MIDIpacket *scratch,*insert_pt,*get_insert_point();

#ifdef DEBUG
printf("ticks = %d event = %d packet = %d chan = %d len = %d\n",ticks,event_type,packet_type,chan,len);
#endif
    scratch = (struct MIDIpacket *) malloc(sizeof(struct MIDIpacket));

    if(scratch == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }

    scratch->data = data;
    scratch->m_type = event_type;
    scratch->p_type = packet_type;
    scratch->chan = chan;
    scratch->len  = len;
    scratch->time = ticks;
    
    /* Where in the linked list should this packet go? */
    insert_pt = get_insert_point(sequence_start,sequence_end,ticks);
   
    if(sequence_start == 0L)       /* the list is empty */
    {  
    	sequence_start = sequence_end = scratch;
        scratch->before = 0L;
        scratch->next = 0L;
    }
    else if(insert_pt == 0L) /* insert the new bottom record */
    { 
        sequence_end->next = scratch;
        scratch->before = sequence_end;
        scratch->next = 0L;
        sequence_end = scratch;
    }
    else if(insert_pt == sequence_start) /* insert the new top record */
    {      
    	sequence_start->before = scratch;
        scratch->before = 0L;
        scratch->next = sequence_start;
        sequence_start = scratch;
    }
    else  /* insert before the insert pointer */
    {      
    	scratch->next = insert_pt;
        scratch->before = insert_pt->before;
        insert_pt->before = scratch;
        (scratch->before)->next = scratch;
    }
} /* end of add_packet() */

/*
 * get_insert_point()
 *    A routine to help insertion into a linked list,  
 *    by returning a pointer to the position in the list
 *    where the MIDI packet should be inserted based on
 *    the time the MIDI event occurs. This could be a lot
 *    faster, but was left simple for easy debugging.
 *    If you'd like to speed this up, please go ahead!
 */
struct MIDIpacket *get_insert_point(top,bottom,time)
struct MIDIpacket *top,*bottom;
unsigned long time;
{
    struct MIDIpacket *index;
    /* this keeps track of the last inset point, making it easier */
    /* to find the next one assuming that most times will be     */
    /* consecutive.                                               */
    static struct MIDIpacket *insert_pointer;
	
    if(top == 0L || bottom == 0L) return(0L);
    if(time > bottom->time) return(0L); /* tack onto end of list */
    if(time < top->time) return(top); /* insert as the new first record */	

    /* check around where the last packet was inserted */
    if(insert_pointer != 0L)
        index = insert_pointer;
    else
    {
        index = sequence_start;
        insert_pointer = index;
    }
    if(time > index->time)
    {
        while(time > index->time && index != 0L)
	    index = index->next;
        return(index);
    }
    else if(time < index->time)
    {
        while(time < index->time && index != 0L)
	    index = index->before;
        return(index->next);
    }
    else 
	return(insert_pointer);
} /* end of get_insert_point() */

/* Routines called from midifile lib */
mynoteon(chan,c1,c2)
int chan,c1,c2;
{
    unsigned char *data;

    data = (unsigned char *) malloc(sizeof(char) * 2);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = (unsigned char) c1;
    data[1] = (unsigned char) c2;
    add_packet((unsigned long)Mf_currtime,note_on,MIDI_EVENT,chan,data,(unsigned long)2);
}

mynoteoff(chan,c1,c2)
int chan,c1,c2;
{
    unsigned char *data;

    data = (unsigned char *) malloc(sizeof(char) * 2);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = (unsigned char) c1;
    data[1] = (unsigned char) c2;
    add_packet((unsigned long)Mf_currtime,note_off,MIDI_EVENT,chan,data,(unsigned long)2);
}

mypressure(chan,pitch,pressure)
int chan,pitch,pressure;
{
    unsigned char *data;

    data = (unsigned char *) malloc(sizeof(char) * 2);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = (unsigned char) pitch;
    data[1] = (unsigned char) pressure;
    add_packet((unsigned long)Mf_currtime,poly_aftertouch,MIDI_EVENT,chan,data,(unsigned long)2);
}

mykeypressure(chan,pitch,pressure)
int chan,pitch,pressure;
{
    unsigned char *data;

    data = (unsigned char *) malloc(sizeof(char) * 2);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = (unsigned char) pitch;
    data[1] = (unsigned char) pressure;
    add_packet((unsigned long)Mf_currtime,channel_aftertouch,MIDI_EVENT,chan,data,(unsigned long)2);
}

myparameter(chan,control,value)
int chan,control,value;
{
    unsigned char *data;

    data = (unsigned char *) malloc(sizeof(char) * 2);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = (unsigned char) control;
    data[1] = (unsigned char) value;
    add_packet((unsigned long)Mf_currtime,control_change,MIDI_EVENT,chan,data,(unsigned long)2);
}

mytempo(microsecs)
unsigned long microsecs;
{
    unsigned char *data;
    data = (unsigned char *) malloc(sizeof(char) * 3);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[2] = (unsigned)(microsecs & 0xff);
    data[1] = (unsigned)((microsecs >> 8) & 0xff);
    data[0] = (unsigned)((microsecs >> 16) & 0xff);
    add_packet((unsigned long)Mf_currtime,set_tempo,META_EVENT,0,data,(unsigned long)3);
}

myvarlen(type,len,msg)
int type,len;
char *msg;
{
    unsigned char *data;
    int i;

    data = (unsigned char *) malloc((unsigned)(sizeof(char) * len));
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    for(i = 0; i < len; i++)
        data[i] = msg[i];
    add_packet((unsigned long)Mf_currtime,type,META_EVENT,0,data,(unsigned long)len);
}

mysmpte(hour,min,sec,frame,fract)
char hour,min,sec,frame,fract;
{
    unsigned char *data;

    data = (unsigned char *) malloc(sizeof(char) * 5);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = hour;
    data[1] = min;
    data[2] = sec;
    data[3] = frame;
    data[4] = fract;
    add_packet((unsigned long)Mf_currtime,smpte_offset,META_EVENT,0,data,(unsigned long)5);
}

myprogram(chan,program)
int chan, program;
{
    unsigned char *data;
    data = (unsigned char *) malloc(sizeof(char) * 1);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = (unsigned char) program;
    add_packet((unsigned long)Mf_currtime,program_chng,MIDI_EVENT,chan,data,(unsigned long)1);
}

mypitchbend(chan,msb,lsb)
int chan, msb,lsb;
{
    unsigned char *data;
    data = (unsigned char *) malloc(sizeof(char) * 2);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = (unsigned char) msb;
    data[1] = (unsigned char) lsb;
    add_packet((unsigned long)Mf_currtime,pitch_wheel,MIDI_EVENT,chan,data,(unsigned long)2);
}

mysysex(len,msg)
int len;
char *msg;
{
    unsigned char *data;
    int i;

    data = (unsigned char *) malloc((unsigned)(sizeof(char) * len));
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    for(i = 0; i < len; i++)
        data[i] = msg[i];
    add_packet((unsigned long)Mf_currtime,system_exclusive,MIDI_EVENT,0,data,(unsigned long)len);
}

myseqnum(num)
int num;
{
    unsigned char *data;
    data = (unsigned char *) malloc(sizeof(char) * 1);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = (unsigned char) num;
    add_packet((unsigned long)Mf_currtime,sequence_number,META_EVENT,0,data,(unsigned long)1);
}

mytimesig(numer,denom,clocks,qnotes)
char numer,denom,clocks,qnotes;
{
    unsigned char *data;

    data = (unsigned char *) malloc(sizeof(char) * 4);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = numer;
    data[1] = denom;
    data[2] = clocks;
    data[3] = qnotes;
    add_packet((unsigned long)Mf_currtime,time_signature,META_EVENT,0,data,(unsigned long)4);
}

mykeysig(sharpflat, minor)
int sharpflat, minor;
{
    unsigned char *data;

    data = (unsigned char *) malloc(sizeof(char) * 2);
    if(data == NULL)
    {
          fprintf(stderr,"Sorry, out of memory!\n");
          exit(1);
    }
    data[0] = sharpflat;
    data[1] = minor;
    add_packet((unsigned long)Mf_currtime,key_signature,META_EVENT,0,data,(unsigned long)2);
}

init_funcs()
{
    sequence_start = 0L;
    sequence_end = 0L;

    /* needed for reading */
    Mf_getc = mygetc;
    Mf_error = myerror;
    Mf_header = myheader;
    Mf_noteon = mynoteon;
    Mf_noteoff = mynoteoff;
    Mf_pressure = mypressure;
    Mf_parameter = myparameter;
    Mf_pitchbend = mypitchbend;
    Mf_program = myprogram;
    Mf_chanpressure = mykeypressure;
    Mf_sysex = mysysex;
    Mf_metamisc = myvarlen;    
    Mf_seqspecific = myvarlen;    
    Mf_seqnum =  myseqnum;
    Mf_text = myvarlen;
    Mf_timesig = mytimesig;
    Mf_smpte = mysmpte;
    Mf_tempo = mytempo;
    Mf_keysig = mykeysig;

   /* needed for writing */
    Mf_putc = myputc;
    Mf_writetrack = mywritetrack;
}
