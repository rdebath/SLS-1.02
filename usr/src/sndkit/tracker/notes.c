/* notes.c */

/* $Id: notes.c,v 3.3 1992/11/27 10:29:00 espie Exp espie $
 * $Log: notes.c,v $
 * Revision 3.3  1992/11/27  10:29:00  espie
 * General cleanup
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
 */

#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <math.h>

#include "defs.h"
#include "extern.h"
#include "song.h"
#include "channel.h"

LOCAL char *id = "$Id: notes.c,v 3.3 1992/11/27 10:29:00 espie Exp espie $";


/* the musical notes correspond to some specific pitch.
 * It's useful to be able to find them back, at least for
 * arpeggii.
 */
int pitch_table[NUMBER_NOTES][NUMBER_FINETUNES];
char note_name[NUMBER_NOTES][4];

char *note_template[12] = {"C-", "C#", "D-", "D#", "E-", "F-", "F#", "G-",
"G#", "A-", "A#", "B-"};

/* find_note(pitch): find note corresponding to the stated pitch */
int find_note(pitch)
int pitch;
    {
    int a, b, i;
    if (pitch == 0)
        return -1;
    a = 0;
    b = NUMBER_NOTES-1;
    while(b-a > 1)
        {
        i = (a+b)/2;
        if (pitch_table[i][0] == pitch)
            return i;
        if (pitch_table[i][0] > pitch)
            a = i;
        else
            b = i;
        }
    if (pitch_table[a][0] - FUZZ <= pitch)
        return a;
    if (pitch_table[b][0] + FUZZ >= pitch)
        return b;
    return NO_NOTE;
    }

void create_notes_table()
    {
    double base, pitch;
    int i, j, k;

	for (j = -8; j < 8; j++)
		{
		k = j < 0 ? j + 16 : j;
		base = AMIGA_CLOCKFREQ/440.0/4.0 / pow(2.0, j/96.0);

		for (i = 0; i < NUMBER_NOTES; i++)
			{
			pitch = base / pow(2.0, i/12.0);
			pitch_table[i][k] = floor(pitch + 0.5);
			if (j == 0)
				{
				note_name[i][0] = note_template[(i+9)%12][0];
				note_name[i][1] = note_template[(i+9)%12][1];
				note_name[i][2] = '0'+ (i-3)/12;
				note_name[i][3] = '\0';
				}
			}
		}
    }

int transpose_song(s, transpose)
struct song *s;
int transpose;
	{
	int oldt;
	int i, j, n;

	if (!s)
		return 0;
	oldt = s->info.transpose;
	for (n = 0; n < s->info.maxpat; n++)
		for (i = 0; i < BLOCK_LENGTH; i++)
			for (j = 0; j < NUMBER_TRACKS; j++)
				if (s->info.pblocks[n].e[j][i].note != NO_NOTE)
					s->info.pblocks[n].e[j][i].note += transpose - oldt;
	s->info.transpose = transpose;
	return oldt;
	}
