/*
 * hextris Copyright 1990 David Markley, dm3e@+andrew.cmu.edu, dam@cs.cmu.edu
 *
 * Permission to use, copy, modify, and distribute, this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders be used in
 * advertising or publicity pertaining to distribution of the software with
 * specific, written prior permission, and that no fee is charged for further
 * distribution of this software, or any modifications thereof.  The copyright
 * holder make no representations about the suitability of this software for
 * any purpose.  It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDER DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA, PROFITS, QPA OR GPA, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

/* This file contains routines used by hextris that are generally standard
 * on most system and display types. It contains no I/O, other than to files.
 */

#include <stdio.h>
#include "header.h"

/* The function reverse is not copyrighted.
 *
 * This is a standard string reverse routine.
 */
reverse(s)
char *s;
{
    int c,i,j;

    for (i = 0, j = strlen(s)-1; i < j; i++,j--) {
	c = s[i];
	s[i] = s[j];
	s[j] = c;
    }
}

/* The function itoa is not copyrighted.
 *
 * This is a standard integer to string converter.
 */
itoa(n,s)
int n;
char *s;
{
    int i, sign;

    if ((sign = n) < 0)
      n = -n;
    i = 0;
    do {
	s[i++] = n % 10 + '0';
    } while ((n /= 10) > 0);
    if (sign < 0)
      s[i++] = '-';
    s[i] = '\0';
    reverse(s);
}

/* This randomly selects the next piece. It also selects the current
 * piece, if it has not yet been set (rotation = -1).
 */
new_piece(npiece,piece)
piece_t *npiece,*piece;
{
    if (npiece->rotation == -1) {
	piece->type = (int)(random() % NUMBEROFPIECES);
	piece->rotation = 0;
	piece->row = 1;
	piece->column = MAXCOLUMN / 2;
    } else {
	piece->type = npiece->type;
	piece->rotation = npiece->rotation;
	piece->row = npiece->row;
	piece->column = npiece->column;
    }
    npiece->type = (int)(random() % NUMBEROFPIECES);
    npiece->rotation = 0;
    npiece->row = 1;
    npiece->column = MAXCOLUMN / 2;
}

/* This reads in the high score file.
 */
read_high_scores(high_scores)
high_score_t high_scores[MAXHIGHSCORES];
{
    int i, j;
    FILE *high_score_file;
    char high_score_file_name[512];
    char buffer[40];

    strcpy(high_score_file_name,HIGHSCOREFILE);
    
    if ((high_score_file = fopen(high_score_file_name , "r")) == NULL) {
	fprintf(stderr,"xhextris: Can't open high score file.\n");
	return 0;
    }

    for (i = 0; i < MAXHIGHSCORES; i++) {
	fread(high_scores[i].name,sizeof(char),MAXNAMELENGTH,
	      high_score_file);
	fread(high_scores[i].userid,sizeof(char),MAXUSERIDLENGTH,
	      high_score_file);
	fread(buffer,sizeof(char),40,high_score_file);
	high_scores[i].score = atoi(buffer);
	fread(buffer,sizeof(char),40,high_score_file);
	high_scores[i].rows = atoi(buffer);
	if (feof(high_score_file))
	  break;
    }
    for (j = i; j < MAXHIGHSCORES; j++) {
	strcpy(high_scores[j].name,"nobody");
	strcpy(high_scores[j].userid,"NON");
	high_scores[j].score = 0;
	high_scores[j].rows = 0;
    }
    fclose(high_score_file);
    return 1;
}

/* This writes the high score file.
 */
write_high_scores(high_scores,uniqueid)
high_score_t high_scores[MAXHIGHSCORES];
char *uniqueid;
{
    int i;
    FILE *high_score_file;
    char high_score_file_name[512];
    char buffer[40];
    
    strcpy(high_score_file_name,HIGHSCOREFILE);
    
#ifdef AFS
    beGames();
#endif
    if ((high_score_file = fopen(high_score_file_name, "w")) == NULL) {
	fprintf(stderr,"xhextris: Can't open high score file.\n");
	return 0;
    }
    for (i = 0; i < MAXHIGHSCORES; i++) {
	fwrite(high_scores[i].name,sizeof(char),MAXNAMELENGTH,
	       high_score_file);
	fwrite(high_scores[i].userid,sizeof(char),MAXUSERIDLENGTH,
	       high_score_file);
	itoa(high_scores[i].score,buffer);
	fwrite(buffer,sizeof(char),40,high_score_file);
	itoa(high_scores[i].rows,buffer);
	fwrite(buffer,sizeof(char),40,high_score_file);
    }
    fflush(high_score_file);
    fclose(high_score_file);
/*    rename(tmp_high_score_file_name,high_score_file_name);*/
#ifdef AFS
    bePlayer();
#endif
    return 1;
}



