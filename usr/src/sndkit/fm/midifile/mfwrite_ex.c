/*
 * writing_example.c 4/30/89
 *
 */
#include <stdio.h>
#include <ctype.h>
#include "midifile.h"

/* These lines are needed to use the library */
FILE *fp;
myputc(c) { return(putc(c,fp));}


/*
 * mywritetrack()
 *
 * Sample showing how to use the library routines to write out a track.
 * Returns 1 if successful, and -1 if not.  The track consists of
 * a series of quarter notes from lowest to highest in pitch at
 * constant velocity, each separted by a quarter-note rest.
 *
 */
int mywritetrack(track)
int track;
{
    int i;
    char data[2];

    mf_write_tempo((long)500000); /* 120 beats/per/second */

    for(i = 1 ; i < 128; i++){
       data[0] = i; /* note number */ 
       data[1] = 64; /* velocity */ 
       if(!mf_write_midi_event(480,note_on,1,data,2)) return(-1);
       if(!mf_write_midi_event(480,note_off,1,data,2)) return(-1);
    }
    return(1);
} /* end of write_track() */

main(argc,argv)
char **argv;
{
    if((fp = fopen(argv[1],"w")) == 0L)
	printf("f1to0: unable to open file %s for writing.\n",argv[1]);

    Mf_putc = myputc;
    Mf_writetrack = mywritetrack;

    /* write a single track */
    mfwrite(0,1,480,fp);
}
