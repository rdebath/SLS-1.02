/* player.c */

/* $Id: player.c,v 3.8 1993/01/15 14:00:28 espie Exp espie $
 * $Log: player.c,v $
 * Revision 3.8  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.7  1993/01/06  17:58:39  espie
 * *** empty log message ***
 *
 * Revision 3.6  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.5  1992/11/24  10:51:19  espie
 * un#ifdef'ed showseq code.
 *
 * Revision 3.4  1992/11/23  10:12:23  espie
 * *** empty log message ***
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Added <> operators.
 * Added update frequency on the fly.
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
 * Revision 2.19  1992/11/17  17:15:37  espie
 * Added interface using may_getchar(). Still primitive, though.
 * imask, start.
 * Added transpose feature.
 * Added possibility to get back to MONO for the sgi.
 * Added stereo capabilities to the indigo version.
 * Added two level of fault tolerancy.
 * Added some control on the number of replays,
 * and better error recovery.
 */
     
#include <stdio.h>
     
#include "defs.h"
#include "extern.h"
#include "song.h"
#include "channel.h"
#include "pref.h"
     
LOCAL BOOL show;

LOCAL char *num[] = {
" 0", " 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9",
"10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
"20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
"30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
"40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
"50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
"60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
"70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
"80", "81", "82", "83", "84", "85", "86", "87", "88", "89"};

LOCAL char *cmdname[] = {
"arp", "dwn", "up ", "prt", "vib", "pts", "vbs", "7  ", "8  ", "off", "svl", 
"ff ", "vol", "skp", "ext", "spd"};

LOCAL char *id = "$Id: player.c,v 3.8 1993/01/15 14:00:28 espie Exp espie $";
     

/* init_channel(ch, dummy):
 * setup channel, with initially
 * a dummy sample ready to play,
 * and no note.
 */
LOCAL void init_channel(ch, dummy)
struct channel *ch;
struct sample_info *dummy;
    {
    ch->samp = dummy;
	ch->finetune = ch->samp->finetune;
    ch->mode = DO_NOTHING; 
    ch->pointer = 0; 
    ch->step = 0; 
    ch->volume = 0; 
    ch->pitch = 0; 
    ch->note = NO_NOTE;

        /* we don't setup arpeggio values. */
    ch->viboffset = 0; 
    ch->vibdepth = 0;

    ch->slide = 0; 

    ch->pitchgoal = 0; 
    ch->pitchrate = 0;

    ch->volumerate = 0;

    ch->vibrate = 0;
    ch->adjust = do_nothing;
    }



LOCAL int VSYNC;          /* base number of sample to output */
void (*eval[NUMBER_EFFECTS])();
                    /* the effect table */
LOCAL int oversample;     /* oversample value */
LOCAL int frequency;      /* output frequency */
LOCAL int channel;        /* channel loop counter */

LOCAL struct channel chan[NUMBER_TRACKS];
                    /* every channel */
LOCAL int countdown;      /* keep playing the tune or not */

LOCAL struct song_info *info;
LOCAL struct sample_info *voices;

LOCAL struct automaton a;


void init_player(o, f)
int o, f;
    {
    oversample = o;
    frequency = f;
    init_tables(o, f, NULL);
    init_effects(eval);
    }

LOCAL void dump_event(ch, e, imask)
struct channel *ch;
struct event *e;
unsigned long imask;
	{
	int samp;
	int cmd;

	if (!run_in_fg())
		return;
	samp = e->sample_number;
	cmd = e->effect;
	if (samp == 0 && e->pitch == 0 && cmd == 0)
		printf("                 ");
	else
		{
		if (samp)
			printf("%s ", num[samp]);
		else
			printf("   ");
		if (((samp != 0) && ((1L<<samp) & imask)) 
			|| ((samp == 0) && (ch->samp == voices)))
			printf("             ");
		else
			{
			if (e->note != NO_NOTE)
				printf("%s ", note_name[e->note]);
			else
				printf("    ");
			if (e->effect == 0 && e->parameters == 0)
				printf("          ");
			else
				switch(cmd)
					{
				case 10:
					printf("%s %3d   ", cmdname[cmd], 
						HI(e->parameters) - LOW(e->parameters));
					break;
				case 14:
					printf("%s %2d/%2d ", cmdname[cmd], 
						HI(e->parameters), LOW(e->parameters));
					break;
				default:
					printf("%s %3d   ", cmdname[cmd], e->parameters);
					}
			}
		}
	fflush(stdout);
	}

LOCAL void setup_effect(ch, a, e, imask, bcdvol)
struct channel *ch;
struct automaton *a;
struct event *e;
unsigned long imask;
int bcdvol;
    {
    int samp, cmd;

        /* retrieves all the parameters */
    samp = e->sample_number;
    a->note = e->note;
    if (a->note != NO_NOTE)
        a->pitch = pitch_table[a->note][ch->finetune];
    else
        a->pitch = e->pitch;
    cmd = e->effect;
    a->para = e->parameters;

    if (a->pitch >= MAX_PITCH)
        {
        fprintf(stderr, "Pitch out of bounds %d\n", a->pitch);
        a->pitch = 0;
        error = FAULT;
        }
    if (cmd == 12 && bcdvol)
        a->para = HI(a->para)*10+LOW(a->para);

	if (show)
		dump_event(ch, e, imask);

        /* load new instrument */
    if (samp)  
        {
            /* note that we can change sample in the middle
             * of a note. This is a *feature*, not a bug (see
             * made). Precisely: the sampel change will be taken
             * into account for the next note, BUT the volume change
             * takes effect immediately.
             */
        ch->samp = voices + samp;
		ch->finetune = voices[samp].finetune;
        if ((1L<<samp) & imask)
            ch->samp = voices;
        set_current_volume(ch, voices[samp].volume);
        }
        /* check for a new note: cmd 3/5 (portamento)
         * is the special case where we do not restart
         * the note.
         */
    if (a->pitch && cmd != 3 && cmd != 5)
        reset_note(ch, a->note, a->pitch);
    ch->adjust = do_nothing;
        /* do effects */
    (eval[cmd])(a, ch);
    }


LOCAL void adjust_sync(ofreq, tempo)
int ofreq, tempo;
	{
	VSYNC = ofreq * NORMAL_FINESPEED / tempo;
	}

LOCAL void play_once(a, pref)
struct automaton *a;
struct pref *pref;
	{
	int channel;

	if (a->do_stuff & DELAY_PATTERN)
		for (channel = 0; channel < NUMBER_TRACKS; channel++)
			/* do the effects */
			(chan[channel].adjust)(chan + channel);
	else
		{	
		for (channel = 0; channel < NUMBER_TRACKS; channel++)
			if (a->counter == 0)
				/* setup effects */
				setup_effect(chan + channel, a, 
					&(a->pattern->e[channel][a->note_num]), 
					pref->imask, pref->bcdvol);
			else
				/* do the effects */
				(chan[channel].adjust)(chan + channel);
		}

		/* advance player for the next tick */
	next_tick(a);
		/* actually output samples */
	resample(chan, oversample, VSYNC / a->finespeed);
	}



void play_song(song, pref, start)
struct song *song;
struct pref *pref;
int start;
    {
	int tempo;

	tempo = pref->speed;
	show = pref->show;

	adjust_sync(frequency, tempo);
    /* a repeats of 0 is infinite replays */
    if (pref->repeats)
        countdown = pref->repeats;
    else
        countdown = 1;

    info = &song->info;
    voices = song->samples; 

    init_automaton(&a, song, start, pref->show);

    for (channel = 0; channel < NUMBER_TRACKS; channel++) 
        init_channel(chan + channel, voices);


    while(countdown)
        {
		play_once(&a, pref);
		switch(may_getchar())
			{
		case 'n':
			discard_buffer();
			error = NEXT_SONG;
			return;
		case 'p':
			discard_buffer();
			error = PREVIOUS_SONG;
			return;
		case 'x':
		case 'e':
		case 'q':
			discard_buffer();
			end_all();
		case 's':
			tempo = 50;
			adjust_sync(frequency, tempo);
			break;
		case 'S':
			tempo = 60;
			adjust_sync(frequency, tempo);
			break;
		case 'r':
			discard_buffer();
			init_automaton(&a, song, start, pref->show);
			for (channel = 0; channel < NUMBER_TRACKS; channel++) 
				init_channel(chan + channel, voices);
			break;
		case '>':
			discard_buffer();
			init_automaton(&a, song, a.pattern_num+1, pref->show);
			break;
		case '<':
			discard_buffer();
			if (a.note_num < 4)
				init_automaton(&a, song, a.pattern_num -1, pref->show);
			else
				init_automaton(&a, song, a.pattern_num, pref->show);
			break;
		case '?':
			dump_song(song);
			break;
		case ' ':
			while (may_getchar() == EOF)
				;
			break;
		default:
			break;
			}
		{
		int new_freq;
		if (new_freq = update_frequency())
			{
			frequency = new_freq;
			adjust_sync(frequency, tempo);
			init_tables(oversample, frequency, chan);
			}
		}

        switch(error)
            {
		case NONE:
			break;
		case ENDED:
			if (pref->repeats)
				countdown--;
			break;
		case SAMPLE_FAULT:
			if (!pref->tolerate)
				countdown = 0;
			break;
		case FAULT:
			if (pref->tolerate < 2)
				countdown = 0;
			break;
		case PREVIOUS_SONG:
		case NEXT_SONG:
		case UNRECOVERABLE:
			countdown = 0;
			break;
		default:
			break;
			}
        error = NONE;
        }
         
    }

