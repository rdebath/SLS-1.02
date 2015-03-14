/* channel.h */

/* $Id: channel.h,v 3.5 1992/11/27 10:29:00 espie Exp espie $
 * $Log: channel.h,v $
 * Revision 3.5  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.4  1992/11/23  10:12:23  espie
 * *** empty log message ***
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Simplified delay_pattern.
 *
 * Revision 3.2  1992/11/20  14:53:32  espie
 * Added finetune.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.7  1992/11/13  13:24:24  espie
 * Added parameters for extended Retriger command.
 * Added transpose feature.
 * Structured part of the code, especially replay ``automaton''
 * and setting up of effects.
 *
 * Revision 1.5  1991/11/16  16:54:19  espie
 * Bug correction: when doing arpeggio, there might not
 * be a new note, so we have to save the old note value
 * and do the arppeggio on that note.
 * Added fields for arpeggio.
 */

     
#ifndef NUMBER_PATTERNS
#define NUMBER_PATTERNS 128
#endif

/* DO_NOTHING is also used for the automaton */
#define DO_NOTHING 0
#define PLAY 1
#define REPLAY 2
     
#define MAX_ARP 3
     
/* there is no note in each channel initially.
 * This is defensive programming, because some
 * commands rely on the previous note. Checking
 * that there was no previous note is a way to
 * detect faulty modules.
 */
#define NO_NOTE 255

struct channel
    {
    struct sample_info *samp;
    int mode;               /* automaton state for the sound generatio */
    unsigned long pointer;   /* current sample position (fixed pos) */
    unsigned int step;      /* sample position increment (gives pitch) */
	int finetune;
    int volume;             /* current volume of the sample (0-64) */
    int pitch;              /* current pitch of the sample */
	int cpitch;
    int note;               /* we have to save the note cause */
                            /* we can do an arpeggio without a new note */
    
    int arp[MAX_ARP];       /* the three pitch values for an arpeggio */
    int arpindex;           /* an index to know which note the arpeggio is doing
                             */

    int viboffset;          /* current offset for vibrato (if any) */
    int vibdepth;           /* depth of vibrato (if any) */

    int slide;              /* step size of pitch slide */

    int pitchgoal;          /* pitch to slide to */
    int pitchrate;          /* step rate for portamento */

    int volumerate;         /* step rate for volume slide */

    int vibrate;            /* step rate for vibrato */

    int retrig;             /* delay for extended retrig command */
    int current;

    void (*adjust)();       /* current command to adjust parameters */
    };

/* DO_NOTHING was already declared for the channel
   #define DO_NOTHING 0 */
#define SET_SPEED 1
#define SET_SKIP 2
#define SET_FASTSKIP 4

#define JUMP_PATTERN 8
#define DELAY_PATTERN 16

#define NORMAL_SPEED 6
#define NORMAL_FINESPEED 125

struct automaton
    {
    int pattern_num;            /* the pattern in the song */
    int note_num;               /* the note in the pattern */
    struct block *pattern;      /* the physical pattern */
    struct song_info *info;     /* we need the song_info */

    char gonethrough[NUMBER_PATTERNS + 1];  /* to check for repeats */

    int counter;                /* the fine position inside the effect */
    int speed;                  /* the `speed', number of effect repeats */
    int finespeed;              /* the finespeed, base is 100 */

    int do_stuff;               /* keeping some stuff to do */
                                /* ... and parameters for it: */
    int new_speed, new_note, new_pattern, new_fastspeed;

    int pitch, note, para;      /* some extra parameters effects need */

	int loop_pattern_num, loop_note_num, loop_counter;
								/* for command E6 */
    };
