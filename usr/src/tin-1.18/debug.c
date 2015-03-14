/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : debug.c
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 02-10-92
 *  Notes     : debug routines
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include "tin.h"

int debug;

/*
 *  nntp specific debug routines
 */
 
void debug_nntp (func, line)
	char *func;	
	char *line;	
{
#ifdef DEBUG
	char file[PATH_LEN];
	FILE *fp;

	if (debug != 1)
		return;

	sprintf (file, "%sNNTP", TMPDIR);

	if ((fp = fopen (file, "a+")) != NULL) {
		fprintf (fp,"%s: %s\n", func, line);
		fclose (fp);
		chmod (file, 0666);
	}
#endif
}


void debug_nntp_respcode (respcode)
	int respcode;
{
#ifdef DEBUG
	debug_nntp ("get_respcode", nntp_respcode (respcode));
#endif
}	

/*
 *  tin specific debug routines
 */
 
void debug_print_arts ()
{
#ifdef DEBUG
	int i;

	if (debug != 2)
		return;

	for (i = 0; i < top; i++) {	/* for each group */
		debug_print_header (&arts[i]);
	}
#endif
}


void debug_print_header (s)
	struct article_t *s;
{
#ifdef DEBUG
	char file[PATH_LEN];
	FILE *fp;

	if (debug != 2)
		return;

	sprintf (file, "%sARTS", TMPDIR);

	if ((fp = fopen (file, "a+")) != NULL) {
		fprintf (fp,"art=[%5ld] tag=[%s] kill=[%s] hot=[%s]\n", s->artnum,
			(s->tagged ? "TRUE" : "FALSE"),
			(s->killed ? "TRUE" : "FALSE"),
			(s->hot ? "TRUE" : "FALSE"));
		fprintf (fp,"subj=[%-38s]\n", s->subject);
		fprintf (fp,"date=[%ld]  from=[%s]  name=[%s]\n", s->date, s->from, s->name);
 		if (s->archive) {
 		    fprintf (fp, "arch=[%-38s]  ", s->archive);
 		} else {
 		    fprintf (fp, "arch=[]  ");
 		}
 		if (s->part) {
 		    fprintf (fp, "part=[%s]  ", s->part);
 		} else {
 		    fprintf (fp, "part=[]  ");
 		}
 		if (s->patch) {
 		    fprintf (fp, "patch=[%s]\n", s->patch);
		} else {
 		    fprintf (fp, "patch=[]\n");
 		}
		fprintf (fp,"thread=[%d]  inthread=[%d]  unread=[%d]\n\n",
			s->thread, s->inthread, s->unread);
/*		fprintf (fp,"thread=[%s]  inthread=[%s]  unread=[%s]\n",
			(s->thread == ART_NORMAL ? "ART_NORMAL" : "ART_EXPIRED"),
			(s->inthread ? "TRUE" : "FALSE"),
			(s->unread ? "TRUE" : "FALSE"));
*/
		fflush (fp);
		fclose (fp);
		chmod (file, 0666);
	}
#endif
}


void debug_save_comp ()
{
#ifdef DEBUG
	char file[PATH_LEN];
	FILE *fp;
	int i;
	
	if (debug != 2)
		return;

	sprintf (file, "%sSAVE_COMP", TMPDIR);

	if ((fp = fopen (file, "a+")) != NULL) {
		for (i = 0 ; i < num_save ; i++) {

			fprintf (fp,"subj=[%-38s]\n", save[i].subject);
			fprintf (fp,"dir=[%s]  file=[%s]\n", save[i].dir, save[i].file);
 			if (save[i].archive) {
 			    fprintf (fp, "arch=[%-38s]  ", save[i].archive);
 			} else {
 			    fprintf (fp, "arch=[]  ");
 			}
 			if (save[i].part) {
 			    fprintf (fp, "part=[%s]  ", save[i].part);
 			} else {
 			    fprintf (fp, "part=[]  ");
 			}
 			if (save[i].patch) {
 			    fprintf (fp, "patch=[%s]\n", save[i].patch);
			} else {
 			    fprintf (fp, "patch=[]\n");
 			}
			fprintf (fp,"index=[%d]  saved=[%d]  mailbox=[%d]\n\n",
				save[i].index, save[i].saved, save[i].is_mailbox);
		}	
		fflush (fp);
		fclose (fp);
		chmod (file, 0666);
	}
#endif
}


void debug_print_comment (comment)
	char *comment;
{
#ifdef DEBUG
	char file[PATH_LEN];
	FILE *fp;

	if (debug != 2)
		return;

	sprintf (file, "%sBASE", TMPDIR);

	if ((fp = fopen (file, "a+")) != NULL) {
		fprintf (fp,"\n%s\n\n", comment);
		fflush (fp);
		fclose (fp);
		chmod (file, 0666);
	}
#endif
}


void debug_print_base ()
{
#ifdef DEBUG
	char file[PATH_LEN];
	FILE *fp;
	int i;

	if (debug != 2)
		return;

	sprintf (file, "%sBASE", TMPDIR);

	if ((fp = fopen (file, "a+")) != NULL) {
		for (i = 0; i < top_base; i++) {
			fprintf (fp, "base[%3d]=[%5ld]\n",i,base[i]);
		}
		fflush (fp);
		fclose (fp);
		chmod (file, 0666);
	}
#endif
}


void debug_print_active ()
{
#ifdef DEBUG
	char file[PATH_LEN];
	FILE *fp;
	int i;

	if (debug != 2)
		return;

	sprintf (file, "%sACTIVE", TMPDIR);

	if ((fp = fopen (file, "w")) != NULL) {
		for (i = 0; i < num_active; i++) {	/* for each group */
			fprintf (fp, "[%4d]=[%-28s] type=[%d] spooldir=[%s]\n", 
				i, active[i].name, active[i].type, active[i].spooldir);
			fprintf (fp, "max=[%4ld] min=[%4ld] mod=[%c] nxt=[%4d] my_group=[%d]\n",
				active[i].max, active[i].min,
				active[i].moderated, active[i].next, active[i].my_group);
			fprintf (fp, "hash=[%ld]  description=[%s]\n", hash_groupname (active[i].name), 
				(active[i].description ? active[i].description : ""));
			fprintf (fp, "read=[%d] show=[%d] thread=[%d] sort=[%d] author=[%d] auto=[%d] process=[%d]\n",
				active[i].attribute.read_during_session,
				active[i].attribute.show_only_unread,
				active[i].attribute.thread_arts,
				active[i].attribute.sort_art_type,
				active[i].attribute.show_author,  
				active[i].attribute.auto_save,
				active[i].attribute.post_proc_type);
			fprintf (fp, "maildir=[%s] savedir=[%s]\n",
				(active[i].attribute.maildir == (char *) 0 ? "" : active[i].attribute.maildir),
				(active[i].attribute.savedir == (char *) 0 ? "" : active[i].attribute.savedir));
			fprintf (fp, "sigfile=[%s] followup_to=[%s]\n\n",
				(active[i].attribute.sigfile == (char *) 0 ? "" : active[i].attribute.sigfile),
				(active[i].attribute.followup_to  == (char *) 0 ? "" : active[i].attribute.followup_to));
		}
		fflush (fp);
		fclose (fp);
		chmod (file, 0666);
	}
#endif
}

/*
 * Prints out hash distribution of active[]
 */
 
void	debug_print_active_hash ()
{
#ifdef DEBUG
	int empty = 0, number = 0;
	int collisions[32];
	register i, j;
			
	for (i = 0; i < 32; i++) {
		collisions[i] = 0;
	}
	
	for (i = 0; i < TABLE_SIZE; i++) {
/*		printf ("HASH[%4d]  ", i);
*/

		if (group_hash[i] == -1) {
/*		
			printf ("EMPTY\n");
*/
			empty++;
		} else {
			number = 1;
			for (j=group_hash[i]; active[j].next >= 0; j=active[j].next) {
				number++;
			}
			if (number > 31) {
				printf ("MEGA HASH COLLISION > 31 HASH[%d]=[%d]!!!\n", i, number);
			} else {
				collisions[number]++;
			}
		}
	}
	
	printf ("HashTable Active=[%d] Size=[%d] Filled=[%d] Empty=[%d]\n",
		num_active, TABLE_SIZE, TABLE_SIZE-empty, empty);
	printf ("01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32\n");
	printf ("-----------------------------------------------------------------------------------------------\n");
	for (i = 0; i < 32; i++) {
		if (i) {
			printf ("%2d ", collisions[i]);
		}
	}
	printf ("\n");
#endif
}
	