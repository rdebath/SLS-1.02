/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: hash.c,v 1.1 88/07/07 10:12:06 sau Exp $
	$Source: /tmp/mgrsrc/doc/usrman/croff/RCS/hash.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/doc/usrman/croff/RCS/hash.c,v $$Revision: 1.1 $";

/************************************************************************
 *
 *	Simple Forms Package		Version 1.0	10/83
 */

/* table lookup routines */

#include "hash.h"
#include <stdio.h>

/*******************************************************************************
 * Generate a hash table index from an arbitrary length string
 * This is probably not a good algorithm
 */

int
Hash(key,max)
char *key;		/* key to hash on */
int max;		/* max should be a prime number */
   {
   register int i = 0;
   register long sum;
   for(sum = 0; *key != '\0';i++, sum += (*key++)<<(i&7));
   return((int) (sum % max));
   }

/*******************************************************************************
 * Generate a hash table index from an arbitrary length string
 * This is probably not a good algorithm
 */

int
hash(key,max)
register char *key;		/* key to hash on */
int max;			/* max should be a prime number */
   {
   register int sum;
        
   for (sum = *key; *key; sum += (*key) * (*(++key)));
   return( (sum += *(key-2)) % max);
   }

/*******************************************************************************
 *	add an entry to the hash table, return count
 */

int
add_entry(table,size,name)
struct table_entry *table[];	/* name of hash table */
int size;			/* number of table entries */
char *name;			/* name to be put in table */
	{
	int HASH(), index;
	char *alloc(), *save_line();
	register TABLE *list;

	index=HASH(name,size);
	for(list=table[index]; list != (TABLE *) 0; list = list -> next)
	   if (Same(list->name,name)) {
	      list -> count += 1;
	      return(list -> count);
	      }
	list = (TABLE *) alloc(sizeof (TABLE));
	list -> name = save_line(name);
	list -> next = table[index];
	list -> value = (char *) 0;
	list -> count = 1;
	table[index] = list;
	return(1);
	}	    

/*******************************************************************************
 *	remove an entry to the hash table, return count
 */

int
dlt_entry(table,size,name)
struct table_entry *table[];	/* pntr to hash table */
int size;			/* size of hash table */
char *name;			/* name to be put in table */
	{
	int HASH(), index;
	void free();
	register struct table_entry *list, *temp= (struct table_entry *) 0;
	index=HASH(name,size);
	for(list=table[index]; list != (TABLE *) 0; temp=list,list = list->next)
	   if (Same(list->name,name)) {
	      if (list -> count > 0)
	         list -> count -= 1;
	      if (list -> count == 0 && !(list->flags&HASH_STATIC)) {
	         free(list -> name);
	         if (list -> value)
 	            free(list -> value);
	         if (list == table[index]) {
	            table[index] = list->next;
	            free(list);
	            }
	         else {
	            temp->next = list->next;
                    free(list);
	            }
		 return(0);
	         }    
	      else return(list -> count);
	      }
	return(-1);
	}	    

/*******************************************************************************
 *	get an entry to the hash table, return value
 */

char *
get_entry(table,size,name)
struct table_entry *table[];
int size;
char *name;			/* name to be put in table */
	{
	int HASH(), index;
	register struct table_entry *list;

	index=HASH(name,size);
	for(list=table[index]; list != (TABLE *) 0; list = list -> next)
	   if (Same(list->name,name)) {
	      return(list->count > 0 ? list -> value : (char *) 0);
	      }
	return((char *) 0);
	}	    

/*******************************************************************************
 *	see if item is in table 
 */

int
is_entry(table,size,name)
struct table_entry *table[];
int size;
char *name;			/* name to be put in table */
	{
	int HASH(), index;
	register struct table_entry *list;

	index=HASH(name,size);
	for(list=table[index]; list != (TABLE *) 0; list = list -> next)
	   if (Same(list->name,name)) {
	      return(list->count);
	      }
	return(0);
	}	    

/*******************************************************************************
 *	put an entry to the hash table, return 1 if ok
 */

int
put_entry(table,size,name,value)
struct table_entry *table[];	/* name of hash table */
int size;			/* number of table entries */
char *name;			/* name to be put in table */
char *value;			/* value to be put into table */
	{
	int HASH(), index;
	register struct table_entry *list;
        char *save_line();
	void free();

	index=HASH(name,size);
	for(list=table[index]; list != (TABLE *) 0; list = list -> next)
	   if (Same(list->name,name) && !(list->flags&HASH_STATIC)) {
	      if (list -> value != (char *) 0) free(list -> value);
	      if (value != (char *) 0) list -> value = save_line(value);
              else list->value == (char *) 0;
	      return(1);
	      }
	return(0);
	}	    

/*******************************************************************************
 *
 *	allocate space for, and save a string; return its address
 */

char *
save_line(string)
char *string;
   {
   char *where,*alloc(),*strcpy();

   if (string == (char *) 0) string = "";
   where=alloc(strlen(string)+1);
   strcpy(where,string);
   return(where);
   }

/*******************************************************************************
 *
 *	allocate some space
 */

char *
alloc(bytes)
int bytes;
  {
  char *malloc(),*where;
  void exit();

  if ((where=malloc((unsigned)bytes)) == NULL) {
     fprintf(stderr,"no room for %d bytes\n",bytes);
     exit(1);
     }
  return(where);
  }	
