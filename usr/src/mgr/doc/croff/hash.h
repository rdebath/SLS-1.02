/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: hash.h,v 1.1 88/07/07 10:12:10 sau Exp $
	$Source: /tmp/mgrsrc/doc/usrman/croff/RCS/hash.h,v $
*/
static char	h_hash_[] = "$Source: /tmp/mgrsrc/doc/usrman/croff/RCS/hash.h,v $$Revision: 1.1 $";

/* hash table item */

struct table_entry {
	char *name;		/* item name */
	char *value;		/* current value */
	int  count;		/* # of references to this entry */
	int  flags;		/* entry flags */
	struct table_entry
	     *next;		/* address of next table entry */
	};

struct table_data {
	char *name;		/* item name */
	char *value;		/* current value */
	int  count;		/* # of references to this entry */
	};

typedef struct table_entry  TABLE;

#define HASH_STATIC		0x001	/* static entry */

#ifndef Same
#   define Same(x,y)		(strcmp(x,y) ? 0 : 1)
#else
    extern int Same();
#endif
#ifndef HASH
#   define HASH	hash			/* name of hash routine */
#endif
