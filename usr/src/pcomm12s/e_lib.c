/*
 * Routines to manipulate the pcomm.extrnl file
 */

#include <stdio.h>
#include "extrnl.h"

/*
 * Read the external file transfer program database.  Returns a pointer
 * to a static area containing the EXTRNL structure.  This support file is 
 * optional.
 */

struct EXTRNL *
read_extrnl(extra)
char *extra;
{
	extern char *null_ptr;
	FILE *fp, *my_fopen();
	int i, line, up, entry, oops;
	char *str_dup(), buf[200], message[80], token[40], *str_tok(), *str;
	char *sep, *temp_token, *findfile();
	static struct EXTRNL e;
	void error_win();

	if ((e.e_path = findfile(extra, "pcomm.extrnl")) == NULL) {
					/* not required to exist */
		for (i=0; i<3; i++) {
			e.name[0][i] = null_ptr;
			e.command[0][i] = null_ptr;
			e.prompt[0][i] = 'N';
			e.name[1][i] = null_ptr;
			e.command[1][i] = null_ptr;
			e.prompt[1][i] = 'N';
		}
		e.up_entries = 0;
		e.dn_entries = 0;

		return(&e);
	}

	if (!(fp = my_fopen(e.e_path, "r"))) {
		sprintf(buf, "\"%s\" for read", e.e_path);
		error_win(1, "Can't open external program file", buf);
	}

	sep = ";;\n";
	line = 0;
	up = 1;
	oops = 0;
	while (fgets(buf, 200, fp) != NULL) {
		line++;
		if (line <= 3)
			entry = line-1;
		else {
			up = 0;
			entry = line-4;
		}
					/* get the token */
		if (!(temp_token = str_tok(buf, '='))) {
			sprintf(message, "is missing a token at line %d", line);
			oops++;
			break;
		}
		/*
		 * Parse the rest of the line.  This is similar to using
		 * the "real" strtok() function, but this version returns
		 * a pointer to NULL if the token is missing.  Note the
		 * use of the array of separators.
		 */
		for (i=0; i<3; i++) {
			if (!(str = str_tok((char *) NULL, sep[i]))) {
				sprintf(message, "is missing a parameter at line %d", line);
				oops++;
				break;
			}
			switch(i) {
				case 0:
					e.name[up][entry] = str_dup(str);
					break;
				case 1:
					e.command[up][entry] = str_dup(str);
					break;
				case 2:
					e.prompt[up][entry] = *str;
					break;
			}
		}
		if (oops)
			break;

					/* sanity checking */
		if (up)
			sprintf(token, "SEND_%d", entry+1);
		else
			sprintf(token, "RCV_%d", entry+1);

		if (strcmp(temp_token, token)) {
			sprintf(message, "is corrupted at line %d", line);
			oops++;
			break;
		}
	}
	fclose(fp);

	if (oops) {
		sprintf(buf, "External program file \"%s\"", e.e_path);
		error_win(1, buf, message);
	}
					/* find number of upload entries */
	for (i=0; i<3; i++) {
		if (e.name[1][i] == null_ptr)
			break;
	}
	e.up_entries = i;
					/* find number of download entries */
	for (i=0; i<3; i++) {
		if (e.name[0][i] == null_ptr)
			break;
	}
	e.dn_entries = i;
					/* if empty database */
	if (!e.up_entries || !e.dn_entries) {
		sprintf(buf, "External program file \"%s\"", e.e_path);
		error_win(0, buf, "has no data");
	}

	return(&e);
}

/*
 * Update the external file transfer program database.  A non-zero return
 * code means a non-fatal error.
 */

int
up_extrnl()
{
	FILE *fp, *my_fopen();
	int i, up, entry;
	char buf[200];
	void error_win();
					/* open for write */
	if (!(fp = my_fopen(extrnl->e_path, "w"))) {
		sprintf(buf, "\"%s\"", extrnl->e_path);
		error_win(0, "No write permission on externl program file", buf);
		return(1);
	}
					/* put 'em back */
	up = 1;
	for (i=0; i<6; i++) {
		if (i < 3)
			entry = i;
		else {
			up = 0;
			entry = i-3;
		}
		if (up)
			fprintf(fp, "SEND_%d=%s;%s;%c\n", entry+1, extrnl->name[up][entry], extrnl->command[up][entry], extrnl->prompt[up][entry]);
		else
			fprintf(fp, "RCV_%d=%s;%s;%c\n", entry+1, extrnl->name[up][entry], extrnl->command[up][entry], extrnl->prompt[up][entry]);
	}

	fclose(fp);
	return(0);
}
