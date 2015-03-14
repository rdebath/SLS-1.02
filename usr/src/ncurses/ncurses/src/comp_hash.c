
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	comp_hash.c --- Routines to deal with the hashtable of capability
 *			names.
 *
 */

#include "compiler.h"
#include "terminfo.h"

/*
 *	make_hash_table()
 *
 *	Takes the entries in cap_table[] and hashes them into cap_hash_table[]
 *	by name.  There are Captabsize entries in cap_table[] and Hashtabsize
 *	slots in cap_hash_table[].
 *
 */

void make_hash_table()
{
	int	i;
	int	hashvalue;
	int	collisions = 0;
	static  int hash_function();

	for (i=0; i < Captabsize; i++)
	{
	    hashvalue = hash_function(cap_table[i].nte_name);       
	    DEBUG(9, "%d\n", hashvalue);

	    if (cap_hash_table[hashvalue] != (struct name_table_entry *) 0)
		collisions++;

	    cap_table[i].nte_link = cap_hash_table[hashvalue];
	    cap_hash_table[hashvalue] = &cap_table[i];
	}

	DEBUG(3, "Hash table complete\n%d collisions ", collisions);
	DEBUG(3, "out of %d entries\n", Captabsize);
}


/*
 *	int hash_function(string)
 *
 *	Computes the hashing function on the given string.
 *
 *	The current hash function is the sum of each consectutive pair
 *	of characters, taken as two-byte integers, mod Hashtabsize.
 *
 */

static
int
hash_function(string)
char	*string;
{
	long	sum = 0;

	while (*string)
	{
	    sum += *string + (*(string + 1) << 8);
	    string++;
	}

	return (sum % Hashtabsize);
}


/*
 *	struct name_table_entry *
 *	find_entry(string)
 *
 *	Finds the entry for the given string in the hash table if present.
 *	Returns a pointer to the entry in the table or 0 if not found.
 *
 */

struct name_table_entry *
find_entry(string)
char	*string;
{
	int	hashvalue;
	struct name_table_entry	*ptr;

	hashvalue = hash_function(string);

	ptr = cap_hash_table[hashvalue];

	while (ptr != (struct name_table_entry *) 0  &&
       	           strcmp(ptr->nte_name, string) != 0)
	    ptr = ptr->nte_link;

	return (ptr);
}
