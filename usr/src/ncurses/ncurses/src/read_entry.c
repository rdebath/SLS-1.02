
/* This work is copyrighted. See COPYRIGHT.OLD & COPYRIGHT.NEW for   *
*  details. If they are missing then this copy is in violation of    *
*  the copyright conditions.                                        */

/*
 *	read_entry.c -- Routine for reading in a compiled terminfo file
 *
 */

#include <malloc.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "terminfo.h"
#include "object.h"

#define OFFSET_BUFSIZE	100

#define min(a, b)	((a) > (b)  ?  (b)  :  (a))

/*
 *	int
 *	read_entry(filename, ptr)
 *
 *	Read the compiled terminfo entry in the given file into the
 *	structure pointed to by ptr, allocating space for the string
 *	table and placing its address in ptr->str_table.
 *
 */

#define swap(x)		(((x >> 8) & 0377) + 256 * (x & 0377))

static char	TermNames[128];	/* Buffer for terminal names for first term */
static char	StringTable[1024];	/* String table for first terminal  */

int must_swap();

int read_entry(char *filename, struct term *ptr)
{
int		fd;
int		numread;
int		num_strings;
int		cur_string;
int		i;
struct header	header;
unsigned char	bytebuf[2];
char		ch;
union
{
    unsigned char    byte[2];
    short            number;
}		offset_buf[OFFSET_BUFSIZE];

	fd = open(filename, 0);

	if (fd < 0) {
		fputs("couldn't open file.\n", stderr);
		exit(1);
	}

	read(fd, &header, sizeof(header));

	if (must_swap())
	{
	    header.magic = swap(header.magic);
	    header.name_size = swap(header.name_size);
	    header.bool_count = swap(header.bool_count);
	    header.num_count = swap(header.num_count);
	    header.str_count = swap(header.str_count);
	    header.str_size = swap(header.str_size);
	}

	if (header.magic != MAGIC)
	{
	    close(fd);
	    return(-1);
	}

	read(fd, TermNames, min(127, header.name_size));
	TermNames[127] = '\0';
	ptr->term_names = TermNames;
	if (header.name_size > 127)
	    lseek(fd, (long) (header.name_size - 127), 1);

	read(fd, ptr->Booleans, min(BOOLCOUNT, header.bool_count));
	if (header.bool_count > BOOLCOUNT)
	    lseek(fd, (long) (header.bool_count - BOOLCOUNT), 1);
	else
	    for (i=header.bool_count; i < BOOLCOUNT; i++)
		ptr->Booleans[i] = 0;

	if ((header.name_size + header.bool_count) % 2 != 0)
	    read(fd, &ch, 1);

	if (must_swap())
	    read(fd, ptr->Numbers, min(NUMCOUNT, header.num_count * 2));
	else
	{
	    for (i=0; i < min(header.num_count, NUMCOUNT); i++)
	    {
		read(fd, bytebuf, 2);
		if (bytebuf[0] == 0377  &&  bytebuf[1] == 0377)
		    ptr->Numbers[i] = -1;
		else
		    ptr->Numbers[i] = bytebuf[0] + 256 * bytebuf[1];
	    }
	}

	if (header.num_count > NUMCOUNT)
	    lseek(fd, (long) (2 * (header.num_count - NUMCOUNT)), 1);
	else
	    for (i=header.num_count; i < NUMCOUNT; i++)
		ptr->Numbers[i] = -1;

	if (cur_term)	/* cur_term is non-zero only if we've been called */
	{
	    ptr->str_table = malloc(header.str_size);
	    if (ptr->str_table == NULL)
	    {
		close(fd);
		return (-1);
	    }
	}
	else
	    ptr->str_table = StringTable;

	num_strings = min(STRCOUNT, header.str_count);
	cur_string = 0;

	while (num_strings > 0) {
	    numread = read(fd, offset_buf, 2*min(num_strings, OFFSET_BUFSIZE));
	    if (numread <= 0) {
		close(fd);
		return(-1);
	    }

	    if (must_swap()) {
		for (i = 0; i < numread / 2; i++) {
		    ptr->Strings[i + cur_string] =
			(offset_buf[i].byte[0] == 0377
			    &&  offset_buf[i].byte[1] == 0377) ? 0
			: ((offset_buf[i].byte[0] + 256*offset_buf[i].byte[1])
							      + ptr->str_table);
		}
	    } else {
		for (i = 0; i < numread / 2; i++) {
		    ptr->Strings[i + cur_string] =
			(offset_buf[i].number == -1) ?  0
			: offset_buf[i].number + ptr->str_table;
		}
	    }

	    cur_string += numread / 2;
	    num_strings -= numread / 2;
	}

	if (header.str_count > STRCOUNT)
	    lseek(fd, (long) (2 * (header.str_count - STRCOUNT)), 1);
	else
	    for (i=header.str_count; i < STRCOUNT; i++)
		ptr->Strings[i] = 0;

	numread = read(fd, ptr->str_table, header.str_size);
	close(fd);
	if (numread != header.str_size)
	    return(-1);

	return(0);
}


/*
 *	int
 *	must_swap()
 *
 *	Test whether this machine will need byte-swapping
 *
 */

int
must_swap()
{
union
{
    short num;
    char  byte[2];
}test;

	test.num = 1;
	return(test.byte[1]);
}
