/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : memory.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 25-10-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"


/*
 * Dynamic arrays maximum & current sizes
 * num_* values are one past top of used part of array
 */
int max_active = 0;
int num_active = -1;
int max_active_size = 0;
int num_active_size = 0;
int max_art = 0;
int max_kill = 0;
int num_kill = 0;
int max_save = 0;
int num_save = 0;
int max_spooldir = 0;
int num_spooldir = 0;

/*
 * Dynamic arrays
 */
int *my_group;				/* .newsrc --> active[] */
long *base;				/* base articles for each thread */
struct group_t *active;			/* active newsgroups */
struct active_size_t *active_size;	/* active file sizes on differnet servers */
struct article_t *arts;			/* articles headers in current group */
struct save_t *save;			/* sorts articles before saving them */
struct spooldir_t *spooldirs;		/* spooldirs on NNTP server (cdrom) */


/*
 *  Dynamic table management
 *  These settings are memory conservative:  small initial allocations
 *  and a 50% expansion on table overflow.  A fast vm system with
 *  much memory might want to start with higher initial allocations
 *  and a 100% expansion on overflow, especially for the arts[] array.
 */

void init_alloc ()
{
	/*
	 * active file arrays
	 */
	max_active = DEFAULT_ACTIVE_NUM;
	max_active_size = DEFAULT_ACTIVE_SIZE_NUM;

	active = (struct group_t *) my_malloc ((unsigned) sizeof(*active) * max_active);
	active_size = (struct active_size_t *) my_malloc ((unsigned) sizeof(*active_size) * max_active_size);
	my_group = (int *) my_malloc ((unsigned) sizeof(int) * max_active);

	/*
	 * article headers array
	 */
	max_art = DEFAULT_ARTICLE_NUM;

	arts = (struct article_t *) my_malloc ((unsigned) sizeof(*arts) * max_art);
	base = (long *) my_malloc ((unsigned) sizeof(long) * max_art);

	/*
	 * kill file array
	 */
	max_kill = DEFAULT_KILL_NUM;
	
	killf = (struct kill_t *) my_malloc ((unsigned) sizeof(*killf) * max_kill);

	/*
	 * save file array
	 */
	max_save = DEFAULT_SAVE_NUM;
	
	save = (struct save_t *) my_malloc ((unsigned) sizeof(*save) * max_save);

	/*
	 * spooldirs array
	 */
	max_spooldir = DEFAULT_SPOOLDIR_NUM;

	spooldirs = (struct spooldir_t *) my_malloc ((unsigned) sizeof(*spooldirs) * max_spooldir);

	screen = (struct screen_t *) 0;
}


void expand_art ()
{
	max_art += max_art / 2;		/* increase by 50% */

	arts = (struct article_t *) my_realloc ((char *) arts, (unsigned) sizeof(*arts) * max_art);
	base = (long *) my_realloc ((char *) base, (unsigned) sizeof(long) * max_art);
}


void expand_active ()
{
	max_active += max_active / 2;		/* increase by 50% */

	if (active == (struct group_t *) 0) {
		active = (struct group_t *) my_malloc ((unsigned) sizeof (*active) * max_active);
		my_group = (int *) my_malloc ((unsigned) sizeof (int) * max_active);
	} else {
		active = (struct group_t *) my_realloc((char *) active,
				 (unsigned) sizeof (*active) * max_active);
		my_group = (int *) my_realloc((char *) my_group, (unsigned) sizeof (int) * max_active);
	}
}


void expand_kill ()
{
	max_kill += max_kill / 2;		/* increase by 50% */

	killf = (struct kill_t *) my_realloc((char *) killf, 
		(unsigned) sizeof (struct kill_t) * max_kill);
}


void expand_save ()
{
	max_save += max_save / 2;		/* increase by 50% */

	save = (struct save_t *) my_realloc((char *) save, 
		(unsigned) sizeof (struct save_t) * max_save);
}


void expand_spooldirs ()
{
	max_spooldir += max_spooldir / 2;	/* increase by 50% */

	spooldirs = (struct spooldir_t *) my_realloc((char *) spooldirs, 
		(unsigned) sizeof (struct spooldir_t) * max_spooldir);
}


void expand_active_size ()
{
	max_active_size += max_active_size / 2;		/* increase by 50% */

	active_size = (struct active_size_t *) my_realloc((char *) active_size, 
		(unsigned) sizeof(struct active_size_t) * max_active_size);
}


void init_screen_array (allocate)
	int allocate;
{
	int i;

	if (allocate) {
		screen = (struct screen_t *) my_malloc(
			(unsigned) sizeof(struct screen_t) * LINES+1);

		for (i=0 ; i < LINES ; i++) {
			screen[i].col = (char *) my_malloc ((unsigned) COLS+2);
		}
	} else {
		if (screen != (struct screen_t *) 0) {
			for (i=0 ; i < LINES ; i++) {
				if (screen[i].col != (char *) 0) {
					free ((char *) screen[i].col);
					screen[i].col = (char *) 0;
				}
			}	

			free ((char *) screen);
			screen = (struct screen_t *) 0;
		}
	}
}


void free_all_arrays ()
{
	hash_reclaim ();
	
	init_screen_array (FALSE);

	free_art_array ();

	if (arts != (struct article_t *) 0) {
		free ((char *) arts);
		arts = (struct article_t *) 0;
	}

	free_active_arrays ();

	if (base != (long *) 0) {
		free ((char *) base);
		base = (long *) 0;
	}

	if (killf != (struct kill_t *) 0) {
		free_kill_array ();
		if (killf != (struct kill_t *) 0) {
			free ((char *) killf);
			killf = (struct kill_t *) 0;
		}
	}

	if (save != (struct save_t *) 0) {
		free_save_array ();
		if (save != (struct save_t *) 0) {
			free ((char *) save);
			save = (struct save_t *) 0;
		}
	}

	if (spooldirs != (struct spooldir_t *) 0) {
		free_spooldirs_array ();
		if (spooldirs != (struct spooldir_t *) 0) {
			free ((char *) spooldirs);
			spooldirs = (struct spooldir_t *) 0;
		}
	}

	if (active_size != (struct active_size_t *) 0) {
		free_active_size_array ();
		if (active_size != (struct active_size_t *) 0) {
			free ((char *) active_size);
			active_size = (struct active_size_t *) 0;
		}
	}
}


void free_art_array ()
{
	register int i;

	for (i=0 ; i < top ; i++) {
		arts[i].artnum = 0L;
		arts[i].thread = ART_EXPIRED;
		arts[i].inthread = FALSE;
		arts[i].unread = ART_UNREAD;
		arts[i].killed = FALSE;
		arts[i].tagged = FALSE;
		arts[i].hot = FALSE;
		arts[i].date = 0L;
		if (arts[i].part != (char *) 0) {
			free ((char *) arts[i].part);
			arts[i].part = (char *) 0;
		}
		if (arts[i].patch != (char *) 0) {
			free ((char *) arts[i].patch);
			arts[i].patch = (char *) 0;
		}
		if (arts[i].xref != NULL) {
			free ((char *) arts[i].xref);
			arts[i].xref = NULL;
		}
	}
}


void free_attributes_array ()
{
	register int i;
	
	for (i = 0 ; i < num_active ; i++) {
		if (active[i].attribute.maildir != (char *) 0 &&
		    active[i].attribute.maildir != default_maildir) {
			free ((char *) active[i].attribute.maildir);
			active[i].attribute.maildir = (char *) 0;
		}
		if (active[i].attribute.savedir != (char *) 0 &&
		    active[i].attribute.savedir != default_savedir) {
			free ((char *) active[i].attribute.savedir);
			active[i].attribute.savedir = (char *) 0;
		}
		if (active[i].attribute.organization != (char *) 0 &&
		    active[i].attribute.organization != default_organization) {
			free ((char *) active[i].attribute.organization);
			active[i].attribute.organization = (char *) 0;
		}
		if (active[i].attribute.sigfile != (char *) 0 &&
		    active[i].attribute.sigfile != default_sigfile) {
			free ((char *) active[i].attribute.sigfile);
			active[i].attribute.sigfile = (char *) 0;
		}
		if (active[i].attribute.printer != (char *) 0 &&
		    active[i].attribute.printer != default_printer) {
			free ((char *) active[i].attribute.printer);
			active[i].attribute.printer = (char *) 0;
		}
		if (active[i].attribute.followup_to != (char *) 0) {
			free ((char *) active[i].attribute.followup_to);
			active[i].attribute.followup_to = (char *) 0;
		}
	}
}


void free_active_arrays ()
{
	register int i;
	
	if (my_group != (int *) 0) {			/* my_group[] */
		free ((char *) my_group);
		my_group = (int *) 0;
	}

	if (active != (struct group_t *) 0) {		/* active[] */
		for (i=0 ; i < num_active ; i++) {
			if (active[i].name != (char *) 0) {
				free ((char *) active[i].name);
				active[i].name = (char *) 0;
			}
			if (active[i].description != (char *) 0) {
				free ((char *) active[i].description);
				active[i].description = (char *) 0;
			}
			if (active[i].type == GROUP_TYPE_MAIL &&
			    active[i].spooldir != (char *) 0) {
				free ((char *) active[i].spooldir);
				active[i].spooldir = (char *) 0;
			}
		}
		
		free_attributes_array ();
	
		if (active != (struct group_t *) 0) {
			free ((char *) active);
			active = (struct group_t *) 0;
		}
	}
	num_active = -1;
}


void free_kill_array ()
{
	int i;
	
	for (i=0 ; i < num_kill ; i++) {
		if (killf[i].kill_subj != (char *) 0) {
			free ((char *) killf[i].kill_subj);
			killf[i].kill_subj = (char *) 0;
		}
		if (killf[i].kill_from != (char *) 0) {
			free ((char *) killf[i].kill_from);
			killf[i].kill_from = (char *) 0;
		}
	}

	num_kill = 0;
}

 
void free_save_array ()
{
	int i;
	
	for (i=0 ; i < num_save ; i++) {
		if (save[i].subject != (char *) 0) {
			free ((char *) save[i].subject);
			save[i].subject = (char *) 0;
		}
		if (save[i].archive != (char *) 0) {
			free ((char *) save[i].archive);
			save[i].archive = (char *) 0;
		}
		if (save[i].dir != (char *) 0) {
			free ((char *) save[i].dir);
			save[i].dir = (char *) 0;
		}
		if (save[i].file != (char *) 0) {
			free ((char *) save[i].file);
			save[i].file = (char *) 0;
		}
		if (save[i].part != (char *) 0) {
			free ((char *) save[i].part);
			save[i].part = (char *) 0;
		}
		if (save[i].patch != (char *) 0) {
			free ((char *) save[i].patch);
			save[i].patch = (char *) 0;
		}
		save[i].index   = -1;
		save[i].saved   = FALSE;
		save[i].is_mailbox = FALSE;
	}
	
	num_save = 0;
}


void free_spooldirs_array ()
{
	int i;
	
	for (i=0 ; i < num_spooldir ; i++) {
		if (spooldirs[i].name != (char *) 0) {
			free ((char *) spooldirs[i].name);
			spooldirs[i].name = (char *) 0;
		}
		if (spooldirs[i].comment != (char *) 0) {
			free ((char *) spooldirs[i].comment);
			spooldirs[i].comment = (char *) 0;
		}
		spooldirs[i].state = 0;
	}
	
	num_spooldir = 0;
}


void free_active_size_array ()
{
	int i;
	
	for (i=0 ; i < num_active_size ; i++) {
		if (active_size[i].server != (char *) 0) {
			free ((char *) active_size[i].server);
			active_size[i].server = (char *) 0;
		}
		if (active_size[i].attribute != (char *) 0) {
			free ((char *) active_size[i].attribute);
			active_size[i].attribute = (char *) 0;
		}
	}
	
	num_active_size = 0;
}


char *my_malloc (size)
	unsigned size;
{
	char *p;

	if ((p = (char *) calloc (1, (int) size)) == NULL) {
		error_message (txt_out_of_memory, progname);
		tin_done (1);
	}
	return p;
}


char *my_realloc (p, size)
	char *p;
	unsigned size;
{
	if (! p) {
		p = (char *) calloc (1, (int) size);
	} else {
		p = (char *) realloc (p, (int) size);
	}

	if (! p) {
		error_message (txt_out_of_memory, progname);
		tin_done (1);
	}
	return p;
}
