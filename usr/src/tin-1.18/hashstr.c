/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : hashstr.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 21-03-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

/*
 *  Maintain a table of all strings we have seen.
 *  If a new string comes in, add it to the table and return a pointer
 *  to it.  If we've seen it before, just return the pointer to it.
 *
 *  Usage:  hash_str("some string") returns char *
 *
 *  Spillovers are chained on the end
 */

/*
 *  Arbitrary table size, but make sure it's prime!
 */

#define		HASHNODE_TABLE_SIZE	2411

struct hashnode *table[HASHNODE_TABLE_SIZE];


char *hash_str (s)
	char *s;
{
	long h;				/* result of hash:  index into hash table */
	struct hashnode *p;	/* used to descend the spillover structs */

	if (s == (char *) 0) {
		return ((char *) 0);
	}

	{
		unsigned char *t = (unsigned char *) s;

		h = *t++;
		while (*t)
			h = ((h << 1) ^ *t++) % (long) HASHNODE_TABLE_SIZE;
	}

	p = table[h];

	if (p == (struct hashnode *) 0) {
		table[h] = add_string (s);
		return table[h]->s;
	}

	while (1) {
		if (strcmp (s, p->s) == 0) {
			return (p->s);
		}

		if (p->next == (struct hashnode *) 0) {
			p->next = add_string (s);
			return p->next->s;
		} else {
			p = p->next;
		}
	}
	/* NOTREACHED */
}


struct hashnode *add_string (s)
	char *s;
{
	int *iptr;
	struct hashnode *p;

	p = (struct hashnode *) my_malloc ((unsigned) sizeof (struct hashnode));

	p->next = (struct hashnode *) 0;
	iptr = (int *) my_malloc ((unsigned) strlen (s) + sizeof (int) + 1);
	*iptr++ = -1;
	p->s = (char *) iptr;
	strcpy (p->s, s);
	return (p);
}


void hash_init ()
{
	int i;

	for (i = 0; i < HASHNODE_TABLE_SIZE; i++) {
		table[i] = (struct hashnode *) 0;
	}
}


void hash_reclaim ()
{
	int i;
	int *iptr;
	struct hashnode *p, *next;

	for (i = 0; i < HASHNODE_TABLE_SIZE; i++)
		if (table[i] != (struct hashnode *) 0) {
			p = table[i];
			while (p != (struct hashnode *) 0) {
				next = p->next;
				if (p->s != (char *) 0) {
					iptr = (int *) p->s;
					iptr--;
					free ((char *) iptr);
				}
				free ((char *) p);
				p = next;
			}
			table[i] = (struct hashnode *) 0;
		}
}
