/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <strings.h>
#include <malloc.h>

#include "alloc.h"
#include "hash.h"

/*
  The main initialization function. Use a size that is appropriate for 
  your application.
*/
HTTABLE *
htinit(size, keysize)
int size;
int keysize;
{
    HTTABLE *ht;
    HTENTRY **hte;

    if (((ht = salloc(1, HTTABLE))) == NULL ||
	(hte = salloc(size, HTENTRY *)) == NULL) {
	printf("htinit: out of memory\n");
	return (NULL);
    }

    ht->ht_entries = hte;
    ht->ht_size = size;
    ht->ht_keysize = keysize;

    return (ht);
}

/* 
  Deletes all the elements and deallocates a table - use with care.
*/  
void htfree(table)
HTTABLE *table;
{
    register int i;
    HTENTRY *hte, *temp;

    for (i=0; i<table->ht_size; i++) {
	hte = table->ht_entries[i];
	while (hte != NULL) {
	    temp = hte;
	    hte = hte->ht_next;
	    free(temp->ht_key);
	    free((char *)temp);
	}
    }

    free((char *)table->ht_entries);
    free((char *)table);
}

/*
  Takes a hash table and a pointer to a function, and calls function 
  successively with elements found in the hash table. Notice that if you 
  are using the data field to be a pointer to your own data structure 
  you should do a htmap(table, free); before you do a htfree(table);
*/
/*VARARGS*/
int
htmap(table, function, data)
HTTABLE *table;
int (*function)();
char *data;
{
    register int i;
    HTENTRY *hte;

    for (i=0;i<table->ht_size;i++) {
	hte = (table->ht_entries)[i];
	while (hte != NULL) {
	    (*function)(hte->ht_data, hte->ht_key, data);
	    hte = hte->ht_next;
	}
    }

    return (0);
}

/*
  Prints out statistics about the table if it contains any entries.
*/
void
htstat(fp, table)
FILE *fp;
HTTABLE *table;
{
    if (table->ht_noe != 0)
      fprintf(fp,
	      "size=%d, entries=%d, colls=%d, %%full=%f, %%coll=%f\n",
	      table->ht_size, table->ht_noe, table->ht_noc,
	      (float)((table->ht_noe * 100)/(table->ht_size)),
	      (float)((table->ht_noc * 100)/table->ht_noe));
    else
      fprintf(fp,
	      "size=%d, entries=%d, colls=%d, %%full=%f, %%coll=%f\n",
	      table->ht_size, table->ht_noe, table->ht_noc, 0.0, 0.0);
}

#define rotl(x) (x && (1 << 8*sizeof(x)-1) ? (x << 1) & 1 : x << 1)

/*
  Takes a string key value, and a table and returns an offset number into 
  the table of entries.
*/
int
hthash(s, table)
char *s;
HTTABLE *table;
{
    register int total = 0;
    register int c;
    register int i;

    if (table->ht_keysize == 0) {
	while (*s != '\0') {
	    c = *s++;
	    total = (total<<3) + (total>>28) + c;
	}
	if (total < 0)
	  total = -total;
    } else {
	for (total = i = 0; i < table->ht_keysize; i++, s++) {
	    total ^= *s * 23;
	    total = rotl(total);
	}
    }

    return (total % table->ht_size);
}

HTENTRY *
htlookup(str, table)
char *str;
HTTABLE *table;
{
    HTENTRY *hte;
    int offset;

    if ((offset = hthash(str, table)) < 0)
      return (NULL);

    hte = table->ht_entries[offset];

    if (hte == NULL) 
      return (NULL);

    while (hte != NULL) {
	if (table->ht_keysize == 0) {
	    if (strcmp(hte->ht_key, str) == 0) 
	      return (hte);
	} else {
	    if (bcmp(hte->ht_key, str, table->ht_keysize) == 0)
	      return (hte);
	}
	hte = hte->ht_next;
    }

    return (NULL);
}

char *
htgetdata(str, table)
char *str;
HTTABLE *table;
{
    HTENTRY *hte;

    if ((hte = htlookup(str, table)) == NULL) 
      return (NULL);
    else 
      return (hte->ht_data);
}

HTENTRY *
htadd_hte(str, table, hte, data)
char *str;
HTTABLE *table;
HTENTRY *hte;
char *data;
{
    int value = hthash(str, table);

    /* add hte to table */
    hte->ht_key = str;
    hte->ht_data = data;
    hte->ht_next = table->ht_entries[value];
    table->ht_entries[value] = hte;

    /* update stats */
    table->ht_noe++;
    if (hte->ht_next != NULL)
      table->ht_noc++;

    return (hte);
}

HTENTRY *
htadd(str, table, data)
char *str;
HTTABLE *table;
char *data;
{
    HTENTRY *hte = htlookup(str, table);
    char *str_copy;

    if (hte == NULL) {
	/* not found in the table */
	if ((hte = salloc(1, HTENTRY)) == NULL)
	  return (NULL);

	if (table->ht_keysize == 0) {
	    if ((str_copy = salloc(strlen(str)+1, char)) == NULL) {
		free(hte);
		return (NULL);
	    } else
	      strcpy(str_copy, str);
	} else {
	    if ((str_copy = salloc(table->ht_keysize, char)) == NULL) {
		free(hte);
		return (NULL);
	    } else
	      bcopy(str, str_copy, table->ht_keysize);
	}

	return (htadd_hte(str_copy, table, hte, data));
    } else {
	/* update the previous data item by the current one */
	hte->ht_data = data;
	return (hte);
    }
}

HTENTRY *
htdelete_hte(str, table)
char *str;
HTTABLE *table;
{
    int value = hthash(str, table);
    HTENTRY *hte = table->ht_entries[value];
    HTENTRY *prev = hte;
    int found = 0;

    while (hte != NULL) {
	if (table->ht_keysize == 0) {
	    if (strcmp(hte->ht_key, str) == 0) {
		found++; 
		break;
	    }
	} else {
	    if (bcmp(hte->ht_key, str, table->ht_keysize) == 0) {
		found++; 
		break;
	    }
	}
	prev = hte;
	hte = hte->ht_next;
    }

    if (found == 0) 
      return (NULL);

    if (prev == hte) 
      table->ht_entries[value] = hte->ht_next;
    else 
      prev->ht_next = hte->ht_next;

    /* update stats */
    table->ht_noe--;
    if (table->ht_entries[value] != NULL)
      table->ht_noc--;

    return (hte);
}

int
htdelete(str, table)
char *str;
HTTABLE *table;
{
    HTENTRY *hte = htdelete_hte(str, table);

    if (hte == NULL)
      return (0);

    free(hte->ht_key);
    free((char *)hte);

    return (1);
}

#ifdef DEBUG
main()
{
    HTTABLE *ht;
    char cmd[20];
    char data[20];
    int val;

    ht = htinit(257);

    for (;;) {
	htstat(stdout, ht);
	printf("? ");
	gets(cmd);
	switch (cmd[0]) {
	  case 'i':
	    printf("data string: ");
	    gets(data);
	    printf("value: ");
	    scanf("%d", &val);
	    htadd(data, ht, (char *)val);
	    break;
	  case 'l':
	    printf("data string: ");
	    gets(data);
	    val = (int) htgetdata(data, ht);
	    if (val == 0) printf("not found!\r\n");
	    else printf("value: %d\r\n", val);
	    break;
	  case 'd':
	    printf("data string: ");
	    gets(data);
	    if (htdelete(data, ht)< 0)
	      printf("not found!\r\n");
	    else printf("deleted!\r\n");
	    break;
	  default:
	    printf("i-insert, l-lookup, d-delete\r\n");
	    break;
	}
    }
}
#endif
