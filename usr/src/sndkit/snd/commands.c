/* commands.c */

/* $Id: commands.c,v 3.7 1993/01/15 14:00:28 espie Exp espie $
 * $Log: commands.c,v $
 * Revision 3.7  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.6  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.5  1992/11/24  10:51:19  espie
 * More precise vibrato table.
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
 * Revision 2.12  1992/11/13  13:24:24  espie
 * Added some extended commands: E12AB, and some.
 * now use set_volume in audio.c. All the device-dependent operation
 * is there.
 * Defensive programming: check the range of each note
 * for arpeggio setup.
 * Structured part of the code, especially replay ``automaton''
 * and setting up of effects.
 *
 * Revision 1.9  1991/11/17  17:09:53  espie
 * Added missing prototypes.
 * Dynamic oversample and frequency.
 * Added arpeggio.
 * Fixed up vibrato depth.
 * Added vibslide and portaslide.
 * Added command 9.
 */

#include <stdio.h>

#include "defs.h"
#include "extern.h"
#include "channel.h"
#include "song.h"
     
LOCAL char *id = "$Id: commands.c,v 3.7 1993/01/15 14:00:28 espie Exp espie $";

/* sine table for the vibrato effect */

int vibrato_table[64] = 
	{
	0,50,100,149,196,241,284,325,362,396,426,452,473,490,502,510,512,
	510,502,490,473,452,426,396,362,325,284,241,196,149,100,50,0,-49,
	-99,-148,-195,-240,-283,-324,-361,-395,-425,-451,-472,-489,-501,
	-509,-511,-509,-501,-489,-472,-451,-425,-395,-361,-324,-283,-240,
	-195,-148,-99,-49
	};

/***
 *
 *
 *  setting up effects/doing effects.
 *  The set_xxx gets called while parsing the effect,
 *  the do_xxx gets called each tick, and update the
 *  sound parameters while playing it.
 *
 *
 ***/


void do_nothing(ch)
struct channel *ch;
    {
    }

void set_nothing(a, ch)
struct automaton *a;
struct channel *ch;
    {
    }

/* slide pitch (up or down) */
void do_slide(ch)
struct channel *ch;
    {
    ch->pitch += ch->slide;
    ch->pitch = MIN(ch->pitch, MAX_PITCH);
    ch->pitch = MAX(ch->pitch, MIN_PITCH);
    set_current_pitch(ch, ch->pitch);
    }

void set_upslide(a, ch)
struct automaton *a;
struct channel *ch;
    {
    ch->adjust = do_slide;
    if (a->para)
        ch->slide = a->para;
    }

void set_downslide(a, ch)
struct automaton *a;
struct channel *ch;
    {
    ch->adjust = do_slide;
    if (a->para)
        ch->slide = -a->para;
    }

/* modulating the pitch with vibrato */
void do_vibrato(ch)
struct channel *ch;
    {
    int offset;

        /* this is no longer a literal transcription of the pt
         * code. I have rescaled the vibrato table.
         */
    ch->viboffset += ch->vibrate;
    ch->viboffset &= 63;
		/* please don't use logical shift on signed values */
    offset = (vibrato_table[ch->viboffset] * ch->vibdepth)/256;
        /* temporary update of only the step value,
         * note that we do not change the saved pitch.
         */
    set_current_pitch(ch, ch->pitch + offset);
    }

void set_vibrato(a, ch)
struct automaton *a;
struct channel *ch;
    {
    ch->adjust = do_vibrato;
    if (a->para)
        {
        ch->vibrate = HI(a->para);
        ch->vibdepth = LOW(a->para);
        }
    }

/* arpeggio looks a bit like chords: we alternate between two
 * or three notes very fast.
 * Issue: we are able to re-generate real chords. Would that be
 * better ? To try.
 */
void do_arpeggio(ch)
struct channel *ch;
    {
    if (++ch->arpindex >= MAX_ARP)
        ch->arpindex =0;
    set_current_pitch(ch, ch->arp[ch->arpindex]);
    }

void set_arpeggio(a, ch)
struct automaton *a;
struct channel *ch;
    {
        /* normal play is arpeggio with 0/0 */
    if (!a->para)
        return;
        /* arpeggio can be installed relative to the
         * previous note, so we have to check that there
         * actually is a current(previous) note
         */
    if (ch->note == NO_NOTE)
        {
        fprintf(stderr,
            "No note present for arpeggio");
        error = FAULT;
        }
    else
        {
        int note;

        ch->arp[0] = pitch_table[ch->note][ch->finetune];
        note = ch->note + HI(a->para);
        if (note < NUMBER_NOTES)
            ch->arp[1] = pitch_table[note][ch->finetune];
        else
            {
            fprintf(stderr,
                "Arpeggio note out of range");
            error = FAULT;
            }
        note = ch->note + LOW(a->para);
        if (note < NUMBER_NOTES)
            ch->arp[2] = pitch_table[note][ch->finetune];
        else
            {
            fprintf(stderr,
                "Arpeggio note out of range");
            error = FAULT;
            }
        ch->arpindex = 0;
        ch->adjust = do_arpeggio;
        }
    }

/* volume slide. Mostly used to simulate waveform control.
 * (attack/decay/sustain).
 */
void do_slidevol(ch)
struct channel *ch;
    {
    ch->volume += ch->volumerate;
    set_current_volume(ch, ch->volume);
    }

/* note that volumeslide does not have a ``take default''
 * behavior. If para is 0, this is truly a 0 volumeslide.
 * Issue: is the test really necessary ? Can't we do
 * a HI(para) - LOW(para). Answer: protracker does not.
 */
void parse_slidevol(ch, para)
struct channel *ch;
int para;
    {
    if (LOW(para))
        ch->volumerate = -LOW(para);
    else
        ch->volumerate = HI(para);
    }

void set_slidevol(a, ch)
struct automaton *a;
struct channel *ch;
    {
    ch->adjust = do_slidevol;
    parse_slidevol(ch, a->para);
    }

/* extended command: retrig note at a fast pace
 */
void do_retrig(ch)
struct channel *ch;
    {
    if (--ch->current <= 0)
        {
        reset_note(ch, ch->note, ch->pitch);
        ch->current = ch->retrig;
        }
    }

/* extended command: start note after a small
 * delay
 */
void do_latestart(ch)
struct channel *ch;
	{
	if (--ch->current <= 0)
		{
		reset_note(ch, ch->note, ch->pitch);
		ch->adjust = do_nothing;
		}
	}

/* extended command: cut note after some time.
 * Note we only kill the volume.
 */
void do_cut(ch)
struct channel *ch;
    {
    if (ch->retrig)
        {
        if (--ch->retrig == 0)
            set_current_volume(ch, 0);
        }
    }

void set_extended(a, ch)
struct automaton *a;
struct channel *ch;
    {
    switch(HI(a->para))
        {
    case 0:
        break;
    case 1:
        ch->pitch += LOW(a->para);
        ch->pitch = MIN(ch->pitch, MAX_PITCH);
        ch->pitch = MAX(ch->pitch, MIN_PITCH);
        set_current_pitch(ch, ch->pitch);
        break;
    case 2:
        ch->pitch -= LOW(a->para);
        ch->pitch = MIN(ch->pitch, MAX_PITCH);
        ch->pitch = MAX(ch->pitch, MIN_PITCH);
        set_current_pitch(ch, ch->pitch);
        break;
	case 5:
		ch->finetune = LOW(a->para);
		break;
	case 6:
		/* Note: the current implementation of protracker
		 * does not allow for a jump from pattern to pattern,
		 * but it looks like a logical extension to the current 
		 * format.
		 */
		if (LOW(a->para) == 0) 
			{
			a->loop_pattern_num = a->pattern_num;
			a->loop_note_num = a->note_num;
			}
		else
			{
			if (a->loop_counter == 0)
				a->loop_counter = LOW(a->para);
			else
				a->loop_counter--;
			if (a->loop_counter > 0)
				a->do_stuff |= JUMP_PATTERN;
			}
		break;
    case 9:
        ch->retrig = LOW(a->para);
        ch->current = ch->retrig;
        ch->adjust = do_retrig;
        break;
    case 10:
        ch->volume += LOW(a->para);
        break;
    case 11:
        ch->volume -= LOW(a->para);
        break;
    case 12:
        ch->retrig = LOW(a->para);
        ch->adjust = do_cut;
        break;
	case 13:
		ch->mode = DO_NOTHING;
		ch->current = LOW(a->para);
		ch->adjust = do_latestart;
		break;
	case 14:
		a->counter -= (LOW(a->para) + 1) * a->speed;
		a->do_stuff |= DELAY_PATTERN;
		break;
    default:
        fprintf(stderr, "Not supported %d-%d\n", HI(a->para), LOW(a->para));
		break;
        }
    }
        
/* portamento: gets from a given pitch to another.
 * We can simplify the routine by cutting it in
 * a pitch up and pitch down part while setting up
 * the effect.
 */
void do_portamento(ch)
struct channel *ch;
    {
    if (ch->pitch < ch->pitchgoal)
        {
        ch->pitch += ch->pitchrate;
        ch->pitch = MIN(ch->pitch, ch->pitchgoal);
        }
    else if (ch->pitch > ch->pitchgoal)
        {
        ch->pitch -= ch->pitchrate;
        ch->pitch = MAX(ch->pitch, ch->pitchgoal);
        }
		/* if we want to implement funk glissando, we need a change right
		 * there
		 */
    set_current_pitch(ch, ch->pitch);
    }

/* if para and pitch are 0, this is obviously a continuation
 * of the previous portamento.
 */
void set_portamento(a, ch)
struct automaton *a;
struct channel *ch;
    {
    ch->adjust = do_portamento;
    if (a->para)
        ch->pitchrate = a->para;
    if (a->pitch)
        ch->pitchgoal = a->pitch;
    }

/*
 * combined commands.
 */
void do_portaslide(ch)
struct channel *ch;
    {
    do_portamento(ch);
    do_slidevol(ch);
    }

void set_portaslide(a, ch)
struct automaton *a;
struct channel *ch;
    {
    ch->adjust = do_portaslide;
	if (a->pitch)
		ch->pitchgoal = a->pitch;
    parse_slidevol(ch, a->para);
    }

void do_vibratoslide(ch)
struct channel *ch;
    {
    do_vibrato(ch);
    do_slidevol(ch);
    }

void set_vibratoslide(a, ch)
struct automaton *a;
struct channel *ch;
    {
    ch->adjust = do_vibratoslide;
    parse_slidevol(ch, a->para);
    }

/***
 *
 *  effects that just need a setup part
 *
 ***/

/* IMPORTANT: because of the special nature of
 * the player, we can't process each effect independently,
 * we have to merge effects from the four channel before
 * doing anything about it. For instance, there can be 
 * several speed change in the same note,
 * only the last one takes effect.
 */
void set_speed(a, ch)
struct automaton *a;
struct channel *ch;
    {
    a->new_speed = a->para;
    a->do_stuff |= SET_SPEED;
    }

void set_skip(a, ch)
struct automaton *a;
struct channel *ch;
    {
        /* yep, this is BCD. */
    a->new_note = HI(a->para) * 10 + LOW(a->para);
    a->do_stuff |= SET_SKIP;
    }

void set_fastskip(a, ch)
struct automaton *a;
struct channel *ch;
    {
    a->new_pattern = a->para;
    a->do_stuff |= SET_FASTSKIP;
    }

/* immediate effect: starts the sample somewhere
 * off the start.
 */
void set_offset(a, ch)
struct automaton *a;
struct channel *ch;
    {
    ch->pointer = int_to_fix(a->para * 256);
    }

/* change the volume of the current channel.
 * Is effective until there is a new set_volume,
 * slide_volume, or an instrument is reloaded 
 * explicitly by giving its number. Obviously, if
 * you load an instrument and do a set_volume in the
 * same note, the set_volume will take precedence.
 */
void set_volume(a, ch)
struct automaton *a;
struct channel *ch;
    {
	set_current_volume(ch, a->para);
    }





/* Initialize the whole effect table */

void init_effects(table)
void (*table[])();
    {
    table[0] = set_arpeggio;
    table[15] = set_speed;
    table[13] = set_skip;
    table[11] = set_fastskip;
    table[12] = set_volume;
    table[10] = set_slidevol;
    table[9] = set_offset;
    table[3] = set_portamento;
    table[5] = set_portaslide;
    table[2] = set_upslide;
    table[1] = set_downslide;
    table[4] = set_vibrato;
    table[6] = set_vibratoslide;
    table[14] = set_extended;
    table[7] = set_nothing;
    table[8] = set_nothing;
    }

