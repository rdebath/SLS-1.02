/* read.c */

/* $Id: read.c,v 3.5 1992/11/27 10:29:00 espie Exp espie $
 * $Log: read.c,v $
 * Revision 3.5  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.4  1992/11/23  17:18:59  espie
 * *** empty log message ***
 *
 * Revision 3.3  1992/11/23  10:12:23  espie
 * *** empty log message ***
 *
 * Revision 3.2  1992/11/22  17:20:01  espie
 * Checks for finetune ?
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.16  1992/11/17  17:06:25  espie
 * fix_xxx for better speed.
 * Added some sample information in the dump.
 * Added transpose feature.
 * Feature fix: length 1 sample should be empty.
 * Corrected repeat length problems concerning badly formed files,
 * added signature checking for new tracker files.
 * Corrected small problem with repeat being too short.
 * Coded error types. More amiga specific stuff.
 *
 * Revision 1.17  1991/11/17  16:30:48  espie
 * Rationnalized error recovery.
 * There was a bug: you could try to deallocate
 * stuff in no-noland. Also, strings never got
 * to be freed.
 * Centralized error control to error_song.
 * Added a new test on length, aborts most modules now.
 * Maybe should say it as well.
 * Added checkpoints for early return if file too short.
 * Added memory recovery and error control.
 * Suppressed ! warning for bad note.
 * Added note support.
 * Corrected length and rep_length/rep_offset
 * which are given in words and should be converted to
 * bytes.
 */

#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "defs.h"
#include "extern.h"
#include "song.h"
#include "channel.h"

LOCAL char *id = "$Id: read.c,v 3.5 1992/11/27 10:29:00 espie Exp espie $";

/***
 *
 *	Low level st-file access routines 
 *
 ***/

/* c = checkgetc(f):
 * gets a character from file f.
 * Aborts program if file f is finished
 */
LOCAL int checkgetc(f)
FILE *f;
    {
    int c;

    if ((c = fgetc(f)) == EOF)
        error = FILE_TOO_SHORT;
    return c;
    }

#define MAX_LEN 50
/* s = getstring(f, len):
 * gets a soundtracker string from file f.
 * I.e, it is a fixed length string terminated
 * by a 0 if too short. Length should be
 * smaller than MAX_LEN.
 */
LOCAL char *getstring(f, len)
FILE *f;
int len;
    {
    static char s[MAX_LEN];
    char *new;
    int i;
        
	assert(len < MAX_LEN);
    for (i = 0; i < len; i++)
        s[MIN(i, MAX_LEN - 1)] = checkgetc(f);
    s[MIN(len, MAX_LEN - 1)] = '\0';
    new = malloc(strlen(s)+1);

    return strcpy(new, s);
    }

/* byteskip(f, len)
 * same as fseek, xcpt it works on stdin
 */
LOCAL void byteskip(f, len)
FILE *f;
int len;
    {
    int i;

    for (i = 0; i < len; i++)
        checkgetc(f);
    }

/* v = getushort(f)
 * reads an unsigned short from f
 */
LOCAL int getushort(f)
FILE *f;
    {
    int i;

    i = checkgetc(f) << 8;
    return i | checkgetc(f);
    }


/* fill_sample_info(info, f):
 	fill sample info with the information at current position of
 	file f. Allocate memory for storing the sample, also.
	fill_sample_info is guaranteed to give you an accurate snapshot
	of what sample should be like. In particular, length, rp_length,
	start, rp_start, fix_length, fix_rp_length will have the values
	you can expect if part of the sample is missing.
 */
LOCAL void fill_sample_info(info, f)
struct sample_info *info;
FILE *f;
    {
    info->name = getstring(f, SAMPLENAME_MAXLENGTH);
    info->length = getushort(f);
    info->finetune = checkgetc(f);
	if (info->finetune > 15)
		info->finetune = 0;
    info->volume = checkgetc(f);
    info->volume = MIN(info->volume, MAX_VOLUME);
    info->rp_offset = getushort(f);
    info->rp_length = getushort(f);

    /* the next check is for old modules for which
     * the sample data types are a bit confused, so
     * that what we were expecting to be #words is #bytes.
     */
        /* not sure I understand the -1 myself, though it's
         * necessary to play kawai-k1 correctly 
         */
    if (info->rp_length + info->rp_offset - 1 > info->length)
        info->rp_offset /= 2;
    
    if (info->rp_length + info->rp_offset > info->length)
        info->rp_length = info->length - info->rp_offset;

    info->length *= 2;
    info->rp_offset *= 2;
    info->rp_length *= 2;
        /* in all logic, a 2-sized sample could exist,
         * but this is not the case, and even so, some
         * trackers output empty instruments as being 2-sized.
         */
    if (info->length <= 2)
		{
		info->start = NULL;
		info->length = 0;
		}
	else
		{
		info->start = (SAMPLE *)calloc(info->length, 1);

		if (info->rp_length > 2)
			info->rp_start = info->start + info->rp_offset;
		else
			{
			info->rp_start = NULL;
			info->rp_length = 0;
			}
		}

    if (info->length > MAX_SAMPLE_LENGTH)
        error = CORRUPT_FILE;
	info->fix_length = int_to_fix(info->length);
	info->fix_rp_length = int_to_fix(info->rp_length);
    }

LOCAL void fill_song_info(info, f)
struct song_info *info;
FILE *f;
    {
    int i;
    int p;

    info->length = checkgetc(f);
    checkgetc(f);
    info->maxpat = -1;
    for (i = 0; i < NUMBER_PATTERNS; i++)
        {
        p = checkgetc(f);
        if (p >= NUMBER_PATTERNS)
            p = 0;
        if (p > info->maxpat)
            info->maxpat = p;
        info->patnumber[i] = p;
        }
    info->maxpat++;
    if (info->maxpat == 0 || info->length == 0)
        error = CORRUPT_FILE;
    }

LOCAL void fill_event(e, f)
struct event *e;
FILE *f;
    {
    int a, b, c, d;

    a = checkgetc(f);
    b = checkgetc(f);
    c = checkgetc(f);
    d = checkgetc(f);
    e->sample_number = (a & 0x10) | (c >> 4);
    e->effect = c & 0xf;
    e->parameters = d;
    e->pitch = ( (a & 15) << 8 ) | b;
    e->note = find_note(e->pitch);
    }

LOCAL void fill_pattern(pattern, f)
struct block *pattern;
FILE *f;
    {
    int i, j;

    for (i = 0; i < BLOCK_LENGTH; i++)
        for (j = 0; j < NUMBER_TRACKS; j++)
            fill_event(&(pattern->e[j][i]), f);
    }


LOCAL void read_sample(info, f)
struct sample_info *info;
FILE *f;
    {

    if (info->start)
        {
        fread(info->start, 1, info->length, f);
        }
    }




/***
 *
 *  new_song: allocates a new structure for a song.
 *  clears each and every field as appropriate.
 *
 ***/
LOCAL struct song *new_song()
    {
    struct song *new;
    int i;

    new = (struct song *)malloc(sizeof(struct song));
    new->title = NULL;
    new->info.length = 0;
    new->info.maxpat = -1;
	new->info.transpose = 0;
    new->info.pblocks = NULL;
    for (i = 0; i < NUMBER_SAMPLES; i++)
		{
		new->samples[i].finetune = 0;
		new->samples[i].name = NULL;
		new->samples[i].length = NULL;
		new->samples[i].start = NULL;
		new->samples[i].rp_start = NULL;
		}
    return new;
    }

/* release_song(song): gives back all memory 
 * occupied by song. Assume that each structure
 * has been correctly allocated by a call to the
 * corresponding new_XXX function.
 */
void release_song(song)
struct song *song;
    {
    int i;

    for (i = 0; i < NUMBER_SAMPLES; i++)
        {
		if (song->samples[i].start)
			free(song->samples[i].start);
		if (song->samples[i].name)
			free(song->samples[i].name);
        }
	if (song->info.pblocks)
		free(song->info.pblocks);
    if (song->title)
        free(song->title);
    free(song);
    }

/* error_song(song): what we should return
 * if there was an error. Actually, is mostly
 * useful for its side effects.
 */
LOCAL struct song *error_song(song)
struct song *song;
    {
    release_song(song);
    return NULL;
    }

/* bad_sig(f): read the signature on file f
 * and returns TRUE if it is not a known sig.
 */
LOCAL BOOL bad_sig(f)
FILE *f;
    {
    char a, b, c, d;

    a = checkgetc(f);
    b = checkgetc(f);
    c = checkgetc(f);
    d = checkgetc(f);
    if (a == 'M' && b == '.' && c == 'K' && d == '.')
        return FALSE;
    if (a == 'M' && b == '&' && c == 'K' && d == '!')
        return FALSE;
    if (a == 'F' && b == 'L' && c == 'T' && d == '4')
        return FALSE;
    return TRUE;
    }

/* s = read_song(f, type): tries to read a song s
 * of type NEW/OLD in file f. Might fail, i.e.,
 * returns NULL if file is not a mod file of the
 * correct type.
 */
struct song *read_song(f, type)
FILE *f;
int type;
    {
    struct song *song;
    int i;
    int ninstr;

    error = NONE;
    if (type == NEW || type == NEW_NO_CHECK)
        ninstr = 31;
    else
        ninstr = 15;

    song = new_song();
    song->title = getstring(f, TITLE_MAXLENGTH);
    if (error != NONE)
        return error_song(song);

    for (i = 1; i <= ninstr; i++)
        {
        fill_sample_info(&song->samples[i], f);
        if (error != NONE)
            return error_song(song);
        }

    fill_song_info(&song->info, f);

    if (error != NONE)
        return error_song(song);

    if (type == NEW && bad_sig(f))
        return error_song(song);

    if (type == NEW_NO_CHECK)
        byteskip(f, 4);
        

    song->info.pblocks = (struct block *)
        malloc(sizeof(struct block) * song->info.maxpat);
    for (i = 0; i < song->info.maxpat; i++)
        {
        fill_pattern(song->info.pblocks + i, f);
        if (error != NONE)
            return error_song(song);
        }

    for (i = 1; i <= ninstr; i++)
        read_sample(&song->samples[i], f);
    
    if (error != NONE)
        return error_song(song);
    return song;
    }

