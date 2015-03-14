/*
 * Routines to manipulate the dialing directory file pcomm.dial_dir
 */

#include <stdio.h>
#include "dial_dir.h"
#include "param.h"

/*
 * Read the dialing directory.  Returns a pointer to a static area
 * containing the DIAL_DIR structure.  All of the entries are created
 * regardless of the number of physical entries in the file.  Element
 * number zero is reserved for the "manual" entry.  All errors are fatal.
 */

struct DIAL_DIR *
read_dir(extra)
char *extra;
{
	extern char *null_ptr;
	FILE *fp, *my_fopen();
	int i, line, oops;
	char *str_dup(), buf[200], *temp_token, *str, *str_tok(), token[20];
	char message[80], *sep, *findfile();
	static struct DIAL_DIR d;
	void error_win();

	if ((d.d_path = findfile(extra, "pcomm.dial_dir")) == NULL)
		error_win(1, "Support file \"pcomm.dial_dir\" is missing", "or no read permission");

	if (!(fp = my_fopen(d.d_path, "r"))) {
		sprintf(buf, "\"%s\" for read", d.d_path);
		error_win(1, "Can't open dialing directory file", buf);
	}

	sep = ";;---;;\n";
	line = 0;
	oops = 0;
	while (fgets(buf, 200, fp) != NULL) {
		line++;
		if (line > NUM_DIR)
			break;
					/* get the token */
		if (!(temp_token = str_tok(buf, '='))) {
			sprintf(message, "is missing a token at line %d", line);
			oops++;
			break;
		}
		/*
		 * Parse the rest of the line.  This is similar to using
		 * the "real" strtok() function, but this version returns
		 * a pointer to NULL if the token is missing.  Note the use
		 * of the array of field separators.
		 */
		for (i=0; i<8; i++) {
			if (!(str = str_tok((char *) NULL, sep[i]))) {
				sprintf(message, "is missing a parameter at line %d", line);
				oops++;
				break;
			}
			switch (i) {
				case 0:
					d.name[line] = str_dup(str);
					break;
				case 1:
					d.number[line] = str_dup(str);
					break;
				case 2:
					d.baud[line] = atoi(str);
					break;
				case 3:
					d.parity[line] = *str;
					break;
				case 4:
					d.dbits[line] = atoi(str);
					break;
				case 5:
					d.sbits[line] = atoi(str);
					break;
				case 6:
					d.duplex[line] = *str;
					break;
				case 7:
					d.script[line] = str_dup(str);
					break;
			}
		}
		if (oops)
			break;
					/* sanity checking */
		sprintf(token, "DIR_%d", line);
		if (strcmp(temp_token, token)) {
			sprintf(message, "is corrupted at line %d", line);
			oops++;
			break;
		}
	}
	fclose(fp);

	if (oops) {
		sprintf(buf, "Dialing directory file \"%s\"", d.d_path);
		error_win(1, buf, message);
	}
	d.d_entries = line;
					/* if empty database */
	if (!line) {
		sprintf(buf, "Dialing directory file \"%s\"", d.d_path);
		error_win(0, buf, "has no data");
	}
					/* fill in the rest with defaults */
	for (i=line+1; i<=NUM_DIR; i++) {
		d.name[i] = null_ptr;
		d.number[i] = null_ptr;
		d.baud[i] = param->d_baud;
		d.parity[i] = param->d_parity;
		d.dbits[i] = param->d_dbits;
		d.sbits[i] = param->d_sbits;
		d.duplex[i] = *param->d_duplex;
		d.script[i] = null_ptr;
	}
					/* create an empty "manual" entry */
	d.name[0] = null_ptr;
	d.number[0] = null_ptr;
	d.baud[0] = param->d_baud;
	d.parity[0] = param->d_parity;
	d.dbits[0] = param->d_dbits;
	d.sbits[0] = param->d_sbits;
	d.duplex[0] = *param->d_duplex;
	d.script[0] = null_ptr;
					/* create an empty queue */
	for (i=0; i<NUM_QUEUE; i++) {
		d.q_ld[i] = '\0';
		d.q_num[i] = -1;
	}
					/* the start up d_cur is 0 */
	d.d_cur = 0;
	return(&d);
}

/*
 * Update a dialing directory entry.  Update only the one entry asked for,
 * not the entire image in memory.  If the new entry is beyond the end of
 * the physical file, then fill in the holes, and update "dir->d_entries".
 * A non-zero return code means a non-fatal error.
 */

int
up_dir(entry)
int entry;
{
	FILE *fp_in, *fp_out, *my_fopen();
	int i;
	char *temp[NUM_DIR+1], buf[200], *str_dup(), *str_rep();
	void error_win(), free_ptr();

					/* open for read */
	if (!(fp_in = my_fopen(dir->d_path, "r"))) {
		sprintf(buf, "\"%s\" for read", dir->d_path);
		error_win(1, "Can't open dialing directory file", buf);
	}
					/* read in a temporary version */
	i = 0;
	while (fgets(buf, 200, fp_in) != NULL)
		temp[++i] = str_dup(buf);

	fclose(fp_in);
					/* alter only 1 entry */
	sprintf(buf, "DIR_%d=%s;%s;%d-%c-%d-%d;%c;%s\n", entry,
	 dir->name[entry], dir->number[entry], dir->baud[entry],
	 dir->parity[entry], dir->dbits[entry], dir->sbits[entry],
	 dir->duplex[entry], dir->script[entry]);

	if (entry <= dir->d_entries)
		temp[entry] = str_rep(temp[entry], buf);
	else
		temp[entry] = str_dup(buf);

					/* fill in holes if beyond end */
	if (entry > dir->d_entries+1) {
		for (i=dir->d_entries+1; i<entry; i++) {
			sprintf(buf, "DIR_%d=;;%d-%c-%d-%d;%c;\n", i,
			 param->d_baud, param->d_parity, param->d_dbits,
			 param->d_sbits, *param->d_duplex);
			temp[i] = str_dup(buf);
		}
	}
					/* update "dir->d_entries" */
	if (entry > dir->d_entries)
		dir->d_entries = entry;

					/* open for write */
	if (!(fp_out = my_fopen(dir->d_path, "w"))) {
		for (i=1; i<=dir->d_entries; i++)
			free_ptr(temp[i]);
		sprintf(buf, "\"%s\"", dir->d_path);
		error_win(0, "No write permission on dialing directory file", buf);
		return(1);
	}
					/* put it back */
	for (i=1; i<=dir->d_entries; i++) {
		fputs(temp[i], fp_out);
		free_ptr(temp[i]);
	}

	fclose(fp_out);
	return(0);
}

/*
 * Delete a range of dialing directory entries.  Actually, just copies
 * default (empty) entries in place of deleted entries.  However, it will
 * shrink the file if deletions occur at the physical EOF.  A non-zero
 * return code means a non-fatal error.
 */

int
del_dir(first, last)
int first, last;
{
	FILE *fp_in, *fp_out, *my_fopen();
	int i;
	char *temp[NUM_DIR+1], buf[200], *str_dup(), *str_rep();
	void error_win(), free_ptr();
					/* sanity checking */
	if (first > dir->d_entries)
		return(0);
	if (last > dir->d_entries)
		last = dir->d_entries;

					/* open for read */
	if (!(fp_in = my_fopen(dir->d_path, "r"))) {
		sprintf(buf, "\"%s\" for read", dir->d_path);
		error_win(1, "Can't open dialing directory file", buf);
	}
					/* read in a temporary version */
	i = 0;
	while (fgets(buf, 200, fp_in) != NULL)
		temp[++i] = str_dup(buf);

	fclose(fp_in);
					/* delete the range of values */
	for (i=first; i<=last; i++) {
		sprintf(buf, "DIR_%d=;;%d-%c-%d-%d;%c;\n", i, param->d_baud,
		 param->d_parity, param->d_dbits, param->d_sbits,
		 *param->d_duplex);
		temp[i] = str_rep(temp[i], buf);
	}
					/* shrink the file? */
	if (last >= dir->d_entries) {
		for (i=first; i<=last; i++)
			free_ptr(temp[i]);
		dir->d_entries = first-1;
	}
					/* open for write */
	if (!(fp_out = my_fopen(dir->d_path, "w"))) {
		for (i=1; i<=dir->d_entries; i++)
			free_ptr(temp[i]);
		sprintf(buf, "\"%s\"", dir->d_path);
		error_win(0, "No write permission on dialing directory file", buf);
		return(1);
	}
					/* put it all back */
	for (i=1; i<=dir->d_entries; i++) {
		fputs(temp[i], fp_out);
		free_ptr(temp[i]);
	}

	fclose(fp_out);
	return(0);
}
