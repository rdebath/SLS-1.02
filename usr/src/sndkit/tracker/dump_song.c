/* dump_song.c */

/* $Id: dump_song.c,v 3.3 1993/01/15 14:00:28 espie Exp espie $
 * $Log: dump_song.c,v $
 * Revision 3.3  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.2  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 */

#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "defs.h"
#include "extern.h"
#include "song.h"
#include "channel.h"

LOCAL char *id = "$Id: dump_song.c,v 3.3 1993/01/15 14:00:28 espie Exp espie $";

/***
 *
 *  dump_block/dump_song:
 *  shows most of the readable info
 *  concerning a module on the screen.
 *
 ***/

LOCAL void dump_block(b)
struct block *b;
    {
    int i, j;

    for (i = 0; i < BLOCK_LENGTH; i++)
        {
        for (j = 0; j < NUMBER_TRACKS; j++)
            {
            printf("%8d%5d%2d%4d", b->e[j][i].sample_number,
                b->e[j][i].pitch, b->e[j][i].effect,
                b->e[j][i].parameters);
            }
        printf("\n");
        }
    }

/* make_readable(s):
 * transform s into a really readable string */

LOCAL void make_readable(s)
char *s;
	{
	char *t, *orig;

	if (!s)
		return;

	orig = s;
	t = s;

		/* get rid of the st-xx: junk */
	if (strncmp(s, "st-", 3) == 0 || strncmp(s, "ST-", 3) == 0)
		{
		if (isdigit(s[3]) && isdigit(s[4]) && s[5] == ':')
			s += 6;
		}
	while (*s)
		{
		if (isprint(*s))
			*t++ = *s;
		s++;
		}
	*t = '\0';
	while (t != orig && isspace(t[-1]))
		*--t = '\0';
	}

void dump_song(song)
struct song *song;
    {
    int i, j;
	int maxlen;
	static char dummy[1];

	if (!run_in_fg())
		return;
	dummy[0] = '\0';
    printf("%s\n", song->title);

	maxlen = 0;
	for (i = 1; i < NUMBER_SAMPLES; i++)
		{
		if (!song->samples[i].name)
			song->samples[i].name = dummy;
		make_readable(song->samples[i].name);
		if (maxlen < strlen(song->samples[i].name))
			maxlen = strlen(song->samples[i].name);
		}
    for (i = 1; i < NUMBER_SAMPLES; i++)
        {
        if (song->samples[i].start || strlen(song->samples[i].name) > 2)
            {
#ifdef SHOW_SEQ
            printf("%2d", i);
#endif
            printf("   %s", song->samples[i].name);
			for (j = strlen(song->samples[i].name); j < maxlen + 2; j++)
				putchar(' ');
			if (song->samples[i].start)
				{
				printf("%5d", song->samples[i].length);
				if (song->samples[i].rp_length > 2)
					{
					printf("(%5d %5d)", song->samples[i].rp_offset, 
						song->samples[i].rp_length);
					}
				else
					printf("             ");
				if (song->samples[i].volume != MAX_VOLUME)
					printf("%3d", song->samples[i].volume);
				}
			putchar('\n');
            }
        }
	fflush(stdout);
    }
