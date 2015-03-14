/* main.c */

/* plays sound/noisetracker files on Sparc, silicon graphics.
 * Authors  : Liam Corner - zenith@dcs.warwick.ac.uk
 *            Marc Espie - espie@dmi.ens.fr
 *            Steve Haehnichen - shaehnic@ucsd.edu
 *
 * Usage    : tracker <filename> 
 *  this version plays compressed files as well.
 */

/* $Id: main.c,v 3.9 1993/01/15 14:00:28 espie Exp espie $
 * $Log: main.c,v $
 * Revision 3.9  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.8  1993/01/06  17:58:39  espie
 * *** empty log message ***
 *
 * Revision 3.7  1992/12/03  15:00:50  espie
 * restore stty.
 *
 * Revision 3.6  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.5  1992/11/24  10:51:19  espie
 * Added loads of new options.
 *
 * Revision 3.4  1992/11/23  17:18:59  espie
 * *** empty log message ***
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Augmented usage.
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
 * Revision 2.20  1992/11/17  17:06:25  espie
 * Added PREVIOUS_SONG handling ???
 * Use streamio for new interface (obsolescent signal handlers), and
 * related changes.
 * Cleaned up path reader, and better signal handling.
 * Support for open_file.
 * Added imask.
 * Use transparent decompression/path lookup through open_file/close_file.
 * Added setup_audio().
 * Added some frequency/oversample/stereo change on the fly.
 * Necessitates rightful closing/reopening of audio.
 * Added compression methods. Changed getopt.
 * Separated mix/stereo stuff.
 * Added transpose feature.
 * Added possibility to get back to MONO for the sgi.
 * Added stereo capabilities to the indigo version.
 * Added recovery and reread for automatic recognition
 * of old/new tracker files.
 * Added two level of fault tolerancy.
 * Added more rational options.
 * Moved almost everything to audio and automaton.
 * Structured part of the code, especially replay ``automaton''
 * and setting up of effects.
 *
 * Revision 1.26  1991/11/17  17:09:53  espie
 * Added missing prototypes.
 * Some more info while loading files.
 * Added FAULT env variable, FAULT resistant playing,
 * for playing modules which are not quite correct.
 * Serious bug: dochangespeed was not reset all the time.
 * Check all these parameters, they MUST be reset for
 * each new song.
 * Fixed a stupid bug: when env variable LOOPING was
 * undefined, we got a segv on strcmp.
 * Now we just test for its existence, since this is
 * about all we want...
 * Bug correction: when doing arpeggio, there might not
 * be a new note, so we have to save the old note value
 * and do the arppeggio on that note.
 * Completely added control with OVERSAMPLE and FREQUENCY.
 * Added control flow.
 * Added pipe decompression, so that now you can do
 * str32 file.Z directly.
 * stdin may go away.
 * Added arpeggio.
 * Added vibslide and portaslide.
 * Added speed command.
 * Added signal control.
 * Error checking: there shouldn't be that many
 * segv signals any more.
 * Moved every command to commands.c.
 * Added some debug code for showing the full
 * sequence for a file.
 * Corrected the bug in volume slide: there is
 * no default value, i.e., if it is 0, it is 0,
 * as stupid as it may seem.
 * Added vibrato.
 * Added fastskip/corrected skip.
 * Modified control flow of the player till
 * it looks like something reasonable (i.e.,
 * the structure is more natural and reflects
 * the way stuff is played actually...)
 * Do not restart the sound when we change instruments
 * on the fly. A bit strange, but it works that way.
 * Modified main to use new data structures.
 * The sound player is MUCH cleaner, it uses now
 * a 3-state automaton for each voice.
 * Corrected ruckus with data type of sample.
 */
     

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
     
#include "defs.h"
#include "extern.h"
#include "song.h"
#include "pref.h"
     
#include "getopt.h"
     
LOCAL char *id = "$Id: main.c,v 3.9 1993/01/15 14:00:28 espie Exp espie $";

#define USAGE \
"[options] filename [...]\n\
-help               Display usage information\n\
-quiet              Print no output other than errors\n\
-picky              Do not tolerate any faults (default is to ignore all)\n\
-tolerant           Ignore all faults\n\
-mono               Select single audio channel output\n\
-stereo             Select dual audio channel output\n\
-verbose            Show text representation of song\n\
-repeats <count>    Number of repeats (0 is forever) (default 1)\n\
-speed <speed>      Song speed.  Some songs want 60 (default 50)\n\
-mix <percent>      Percent of channel mixing. (0 = spatial, 100 = mono)\n\
-new -old -both     Select default reading type (default is -both)\n\
-frequency <freq>   Set playback frequency in Hz\n\
-oversample <times> Set oversampling factor\n\
-transpose <n>      Transpose all notes up\n\
-show               Show what's going on\n\
-sync               Try to synch audio output with display\n\
\n\
RunTime:\n\
r       restart current song\n\
e,x     exit program\n\
n       next song\n\
p       previous song (experimental)\n\
>       fast forward\n\
<       rewind\n\
S       NTSC tempo\t s\tPAL tempo\n"

/* Command-line options. */
LOCAL struct long_option long_options[] =
{
  {"help",              0, 'H'},
  {"quiet",             0, 'Q'}, 
  {"picky",             0, 'P'},
  {"tolerant",          0, 'L'},
  {"new",               0, 'N'},
  {"old",               0, 'O'},
  {"both",              0, 'B'},
  {"mono",              0, 'M'},
  {"stereo",            0, 'S'},
  {"verbose",           0, 'V'},
  {"frequency",         1, 'f'},
  {"oversample",        1, 'o'},
  {"transpose",         1, 't'},
  {"repeats",           1, 'r'},
  {"speed",             1, 's'},
  {"mix",               1, 'm'},
  {"start",             1, 'X'},
  {"cut",               1, '-'},
  {"add",               1, '+'},
  {"show",              0, 'v'},
  {"sync",              0, '='},
  {0,                   0,  0 }
};


/* global variable to catch various types of errors
 * and achieve the desired flow of control
 */
int error;

LOCAL int optvalue(def)
int def;
    {
    int d;
        if (sscanf(optarg, "%d", &d) == 1)
            return d;
        else
            {
            optind--;
            return def;
            }
    }

void end_all()
    {
	if (run_in_fg())
		printf("\n");
    do_close_audio();
	sane_tty();
    exit(0);
    }

LOCAL struct song *do_read_song(name, type)
char *name;
int type;
    {
    struct song *song;
    FILE *file;

	if (run_in_fg())
		fflush(stdout); 

    file = open_file(name, "r", getenv("MODPATH"));
    if (!file)
        return NULL;
    song = read_song(file, type); 
    close_file(file);
    return song;
    }

int main(argc, argv)
int argc;
char **argv;
    {
    int ask_freq;
    int oversample;
    int stereo;
    int start;
    int transpose;


    struct pref pref;
    struct song *song;
	BOOL *is_song;
    int c;
	int i;
    int default_type;


	is_song = (BOOL *)malloc(sizeof(BOOL) * argc);
	if (!is_song)
		end_all();
	for (i = 0; i < argc; i++)
		is_song[i] = FALSE;

    start = 0;
    pref.imask = 0;
    pref.bcdvol = 0;
    pref.dump_song = FALSE;
    pref.show = FALSE;
    pref.sync = FALSE;

	nonblocking_io();

    if (argc == 1)
        {
        fprintf(stderr, "Usage: %s %s", argv[0], USAGE);
		end_all();
        }

    ask_freq = read_env("FREQUENCY", 0);
    oversample = read_env("OVERSAMPLE", 1);
    transpose = read_env("TRANSPOSE", 0);
    stereo = !getenv("MONO");

    create_notes_table();

        /* check the command name for default reading type */

    default_type = BOTH;

    pref.type = default_type;
    pref.repeats = 1;
    pref.speed = 50;
    pref.tolerate = 1;

    for (optind = 1; optind < argc; optind++)
        {
        while ((c = getopt(argc, argv, long_options))
                != EOF)
            switch(c)
                {
            case '-':
                if (strcmp(optarg, "all") == 0)
                    pref.imask = ~0;
                else
                    pref.imask |= 1L << optvalue(0);
                break;
            case '+':
                if (strcmp(optarg, "all") == 0)
                    pref.imask = 0;
                else
                    pref.imask &= ~ (1L << optvalue(0));
                break;
            case 'O':   /* old tracker type */
                pref.type = OLD;
                break;
            case 'N':   /* new tracker type */
                pref.type = NEW;
                break;
            case 'v':
                pref.show = TRUE;
                break;
            case '=':
                pref.sync = TRUE;
                break;
            case 'B':   /* both tracker types */
                pref.type = BOTH;
                break;
            case 'r':   /* number of repeats */
                pref.repeats = optvalue(0);
                break;
            case 's':   /* speed */
                pref.speed = optvalue(50);
                break;
            case 'M':   /* MONO */
                stereo = 0;
                break;
            case 'S':   /* STEREO */
                stereo = 1;
                break;
            case 'o':   /* oversample */
                oversample = optvalue(1);
                break;
            case 'f':   /* frequency */
                ask_freq = optvalue(0);
                break;
            case 't':   /* transpose */
                transpose = optvalue(0);
                break;
            case 'P':
                pref.tolerate = 0;
                break;
            case 'L':
                pref.tolerate = 2;
                break;
            case 'm':   /* % of channel mix. 
                         * 0->full stereo, 100->mono */
                set_mix(optvalue(30));
                break;
            case 'X':
                start = optvalue(0);
                break;
            case 'H':   /* template */
                fprintf(stderr, "Usage: %s %s", argv[0], USAGE);
				end_all();
            case 'V':
                pref.dump_song = TRUE;
                }
		if (error == PREVIOUS_SONG)
			for (optind -= 2; optind > 1 && !is_song[optind]; optind--)
				;
        if (optind < argc)
            {
			if (run_in_fg())
				printf("%s:", argv[optind]);
            switch(pref.type)
                {
            case BOTH:
                song = do_read_song(argv[optind], NEW);
                if (!song && error != NEXT_SONG)
                    song = do_read_song(argv[optind], OLD);
                break;
            case OLD:
                song = do_read_song(argv[optind], pref.type);
                break;
                /* this is explicitly flagged as a new module,
                 * so we don't need to look for a signature.
                 */
            case NEW:
                song = do_read_song(argv[optind], NEW_NO_CHECK);
                break;
                }
            if (song)
				is_song[optind] = TRUE;
			else
                {
                puts("not a song");
                continue;
                }

            if (pref.dump_song)
                dump_song(song); 
            transpose_song(song, transpose);
            setup_audio(ask_freq, stereo, oversample, pref.sync);
            play_song(song, &pref, start);
            release_song(song);
			if (run_in_fg())
				printf("\n");
            }
        }

    end_all();
    /* NOT REACHED */
    }


