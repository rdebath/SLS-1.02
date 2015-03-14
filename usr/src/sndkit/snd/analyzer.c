/* analyzer.c */

/* read module files and output statistics on them */

/* $Id: analyzer.c,v 3.1 1993/01/16 17:00:27 espie Exp espie $
 * $Log: analyzer.c,v $
 * Revision 3.1  1993/01/16  17:00:27  espie
 * Added patch for non termio.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "extern.h"
#include "song.h"
#include "pref.h"

LOCAL char *id = "$Id: analyzer.c,v 3.1 1993/01/16 17:00:27 espie Exp espie $";

int error;

/* dummy function for linking wo termio */
BOOL run_in_fg()
	{
	return TRUE;
	}

struct song *do_read_song(name, type)
char *name;
int type;
    {
    struct song *song;
    FILE *file;

    file = open_file(name, "r", getenv("MODPATH"));
    if (!file)
		return NULL;
    song = read_song(file, type); 
    close_file(file);
	if (song)
		puts(name);
    return song;
    }


BOOL use_command[16];
BOOL use_extended[16];

void analyze_block(b)
struct block *b;
	{
	int i, j;
	struct event *e;

	for (i = 0; i < BLOCK_LENGTH; i++)
		for (j = 0; j < NUMBER_TRACKS; j++)
			{
			e = &b->e[j][i];
			switch(e->effect)
				{
#if 0
			case 13:	/* skip */
				return;
			case 11:	/* fastskip */
				return;
#endif
			case 14:
				use_extended[HI(e->parameters)] = TRUE;
				break;
			default:
				use_command[e->effect] = TRUE;
				}
			}
	}


void analyze_song(song)
struct song *song;
	{
	int i;

	for (i = 0; i < NUMBER_SAMPLES; i++)
		{
		if (song->samples[i].start)
			{
			if (song->samples[i].finetune)
				printf("Sample %d: finetune is %d\n", 
					i, song->samples[i].finetune);
			}
		}
	for (i = 0; i < 16; i++)
		{
		use_command[i] = FALSE;
		use_extended[i] = FALSE;
		}
	for (i = 0; i < song->info.maxpat; i++)
		analyze_block(song->info.pblocks+i);
	for (i = 0; i < 16; i++)
		if (use_command[i])
			printf("%3d", i);
	for (i = 0; i < 16; i++)
		if (use_extended[i])
			printf("%3dE", i);
	printf("\n");
	}

int main(argc, argv)
int argc;
char **argv;
	{
	int i;

	struct pref pref;
	struct song *song;
	int default_type;

	default_type = BOTH;
	pref.tolerate = 2;

	create_notes_table();

	for (i = 1; i < argc; i++)
		{
		song = do_read_song(argv[i], NEW);
		if (!song && error != NEXT_SONG)
			song = do_read_song(argv[i], OLD);
		if (song)
			{
			analyze_song(song);
			release_song(song);
			}
		}
	}

		


