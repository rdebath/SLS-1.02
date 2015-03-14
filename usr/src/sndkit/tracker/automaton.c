/* automaton.c */

/* $Id: automaton.c,v 3.8 1993/01/16 17:00:27 espie Exp espie $
 * $Log: automaton.c,v $
 * Revision 3.8  1993/01/16  17:00:27  espie
 * Corrected stupid bug (run_in_fg)
 *
 * Revision 3.7  1993/01/15  14:00:28  espie
 * Added bg/fg test.
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
 * Revision 2.16  1992/11/17  17:15:37  espie
 * New output for new interface
 * Modified repeat logic: now works irregardless of repeat points.
 * start
 *
 * Revision 2.8  1992/07/14  14:23:41  espie
 * Changed fine speed command and comments.
 * Added two level of fault tolerancy.
 */
     

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
     
#include "defs.h"
#include "extern.h"
#include "song.h"
#include "channel.h"
     
LOCAL char *id = "$Id: automaton.c,v 3.8 1993/01/16 17:00:27 espie Exp espie $";
LOCAL BOOL show;
     
/* updates the pattern to play in the automaton.
 * Checks that the pattern actually exists.
 */
LOCAL void set_pattern(a)
struct automaton *a;
    {
    int p;
    if (a->pattern_num >= a->info->length)
        {
        error = UNRECOVERABLE;
        return;
        }

	if (run_in_fg())
		{
		if (show)
			printf("\n%3d\n", a->pattern_num);
		else
			printf("%3d/%3d\b\b\b\b\b\b\b", a->pattern_num, a->info->length);
    	fflush(stdout); 
		}
        /* there is a level of indirection in the format,
         * i.e., patterns can be repeated.
         */
    p = a->info->patnumber[a->pattern_num];
    a->gonethrough[a->pattern_num] = TRUE;
    if (p >= a->info->maxpat)
        {
        error = UNRECOVERABLE;
        return;
        }
    a->pattern = a->info->pblocks + p;
    }

LOCAL void clear_repeats(a, from, upto)
struct automaton *a;
int from, upto;
	{
	int i;

	for (i = from; i <= upto; i++)
		a->gonethrough[i] = FALSE;
	}

LOCAL void reset_repeats(a)
struct automaton *a;
    {
	clear_repeats(a, 0, a->info->length);
    a->gonethrough[a->info->length] = TRUE;
    }

/* initialize all the fields of the automaton necessary
 * to play a given song.
 */
void init_automaton(a, song, start, s)
struct automaton *a;
struct song *song;
int start;
BOOL s;
    {
    a->info = &song->info;
    a->pattern_num = start;     /* first pattern */
	show = s;

	a->loop_pattern_num = 0;
	a->loop_note_num = 0;
	a->loop_counter = 0;

    reset_repeats(a);

    a->note_num = 0;        /* first note in pattern */
    a->counter = 0;         /* counter for the effect tempo */
    a->speed = NORMAL_SPEED;/* this is the default effect tempo */
    a->finespeed = NORMAL_FINESPEED;    
                            /* this is the fine speed (100%=NORMAL_FINESPEED) */
    a->do_stuff = DO_NOTHING;   
                            /* some effects affect the automaton,
                             * we keep them here.
                             */

    error = NONE;           /* Maybe we should not reset errors at
                             * this point.
                             */
    set_pattern(a);
    }

/* Gets to the next pattern, and displays stuff */
LOCAL void advance_pattern(a)
struct automaton *a;
    {
	if (run_in_fg() && show)
		printf("\n\n");
    if (++a->pattern_num >= a->info->length)
        a->pattern_num = 0;
    if (a->gonethrough[a->pattern_num])
        {
        error = ENDED;
        reset_repeats(a);
        }
    set_pattern(a);
    a->note_num = 0;
    }

        

/* process all the stuff which we need to advance in the song,
 * including set_speed, set_skip and set_fastskip.
 */
void next_tick(a)
struct automaton *a;
    {
    if (a->do_stuff & SET_SPEED && a->new_speed)
        {
            /* there are three classes of speed changes:
             * 0 does nothing. (should stop)
             * <32 is the effect speed (resets the fine speed).
             * >=32 changes the finespeed, default 125
             */
        if (a->new_speed >= 32)
            a->finespeed = a->new_speed;
        else if (a->new_speed)
            {
            a->speed = a->new_speed;
            a->finespeed = NORMAL_FINESPEED;
            }
        }
    if (++a->counter >= a->speed)
        {
		a->counter = 0;
		if (run_in_fg() && show)
			printf("\n");
		if (a->do_stuff & JUMP_PATTERN)
			{
			clear_repeats(a, a->loop_pattern_num, a->pattern_num);
			a->pattern_num = a->loop_pattern_num;
			set_pattern(a);
			a->note_num = a->loop_note_num;
			}
		else if (a->do_stuff & SET_FASTSKIP)
            {
            a->pattern_num = a->new_pattern;
            set_pattern(a);
            a->note_num = 0;
            }
        else if (a->do_stuff & SET_SKIP)
            {
            advance_pattern(a);
            a->note_num = a->new_note;
            }
        else
            {
            if (++a->note_num >= BLOCK_LENGTH)
                advance_pattern(a);
            }
        a->do_stuff = DO_NOTHING;
        }
    }


