/*
 * Routines to manipulate the pcomm.modem file
 */

#include <stdio.h>
#include "modem.h"

/*
 * Read the modem/TTY database file.  Returns a pointer to a static area
 * containing the MODEM structure.  All modem entries and all TTY entries
 * are created regardless of the number of physical entries in the file.
 */

struct MODEM *
read_modem(extra)
char *extra;
{
	extern char *null_ptr;
	FILE *fp, *my_fopen();
	int i, tty, mod, line, oops, m_line, start, stop;
	char *str_dup(), buf[200], message[80], token[40], *str_tok(), *str;
	char *temp_token, *t_sep, *m_sep, *m_letter, *findfile();
	static struct MODEM m;
	void error_win();

	if ((m.m_path = findfile(extra, "pcomm.modem")) == NULL)
		error_win(1, "Support file \"pcomm.modem\" is missing", "or no read permission");

	if (!(fp = my_fopen(m.m_path, "r"))) {
		sprintf(buf, "\"%s\" for read", m.m_path);
		error_win(1, "Can't open modem/TTY file", buf);
	}

	t_sep = ";;\n";
	m_sep = ";;;;\n;;;;;;\n;;;\n";
	m_letter = "abc";
	oops = 0;
	tty = 0;
	mod = 0;
	line = 0;
	m_line = 0;
	while (fgets(buf, 200, fp) != NULL) {
		line++;
		if (tty >= NUM_TTY || mod >= NUM_MODEM)
			break;
					/* get the token */
		if (!(temp_token = str_tok(buf, '='))) {
			sprintf(message, "is missing a token at line %d", line);
			oops++;
			break;
		}
		if (*temp_token != 'T' && *temp_token != 'M') {
			sprintf(message, "is corrupted at line %d", line);
			oops++;
			break;
		}
					/* the TTY database */
		if (*temp_token == 'T') {
			/*
			 * This is similar to the "real" strtok() command
			 * but this one returns a pointer to NULL on a missing
			 * token.  Note the use of the field separator
			 * array.
			 */
			for (i=0; i<3; i++) {
				if (!(str = str_tok((char *) NULL, t_sep[i]))) {
					sprintf(message, "is missing a parameter at line %d", line);
					oops++;
					break;
				}
				switch (i) {
					case 0:
						m.tty[tty] = str_dup(str);
						break;
					case 1:
						m.tname[tty] = str_dup(str);
						break;
					case 2:
						m.init_sp[tty] = atoi(str);
						break;
				}
			}
			if (oops)
				break;
					/* sanity checking */
			sprintf(token, "TTY_%d", tty+1);
			if (strcmp(token, temp_token)) {
				sprintf(message, "is corrupted at line %d", line);
				oops++;
				break;
			}
			tty++;
			continue;
		}
					/* the modem database */
		else {
			sprintf(token, "MODEM_%d%c", mod+1, m_letter[m_line]);
			if (strcmp(token, temp_token)) {
				sprintf(message, "is corrupted at line %d", line);
				oops++;
				break;
			}
			/*
			 * There are three lines to the modem database.  They
			 * are distinguished by the letters a, b, and, c
			 * appended to the entry number.
			 */
			switch (m_line) {
				case 0:
					start = 0;
					stop = 5;
					break;
				case 1:
					start = 5;
					stop = 12;
					break;
				case 2:
					start = 12;
					stop = 16;
					break;
			}
			for (i=start; i<stop; i++) {
				if (!(str = str_tok((char *) NULL, m_sep[i]))) {
					sprintf(message, "is missing a parameter at line %d", line);
					oops++;
					break;
				}
				switch (i) {
					case 0:
						m.mname[mod] = str_dup(str);
						break;
					case 1:
						m.init[mod] = str_dup(str);
						break;
					case 2:
						m.dial[mod] = str_dup(str);
						break;
					case 3:
						m.suffix[mod] = str_dup(str);
						break;
					case 4:
						m.hang_up[mod] = str_dup(str);
						break;
					case 5:
						m.auto_baud[mod] = *str;
						break;
					case 6:
						m.con_3[mod] = str_dup(str);
						break;
					case 7:
						m.con_12[mod] = str_dup(str);
						break;
					case 8:
						m.con_24[mod] = str_dup(str);
						break;
					case 9:
						m.con_48[mod] = str_dup(str);
						break;
					case 10:
						m.con_96[mod] = str_dup(str);
						break;
					case 11:
						m.con_192[mod] = str_dup(str);
						break;
					case 12:
						m.no_con1[mod] = str_dup(str);
						break;
					case 13:
						m.no_con2[mod] = str_dup(str);
						break;
					case 14:
						m.no_con3[mod] = str_dup(str);
						break;
					case 15:
						m.no_con4[mod] = str_dup(str);
						break;
				}
			}
			if (oops)
				break;
			m_line++;
			if (m_line >= 3) {
				m_line = 0;
				mod++;
			}
		}
	}
	fclose(fp);

	if (oops) {
		sprintf(buf, "Modem/TTY database file \"%s\"", m.m_path);
		error_win(1, buf, message);
	}
	m.t_entries = tty;
	m.m_entries = mod;
	m.t_cur = -1;
	m.m_cur = -1;
					/* if empty database */
	if (!tty) {
		sprintf(buf, "Modem/TTY database file \"%s\"", m.m_path);
		error_win(0, buf, "has no TTY data");
	}
	if (!mod) {
		sprintf(buf, "Modem/TTY database file \"%s\"", m.m_path);
		error_win(0, buf, "has no modem data");
	}
					/* fill in the rest */
	for (; tty<NUM_TTY; tty++) {
		m.tty[tty] = null_ptr;
		m.tname[tty] = null_ptr;
		m.init_sp[tty] = 0;
	}
	for (; mod<NUM_MODEM; mod++) {
		m.mname[mod] = null_ptr;
		m.init[mod] = null_ptr;
		m.dial[mod] = null_ptr;
		m.suffix[mod] = null_ptr;
		m.hang_up[mod] = null_ptr;

		m.auto_baud[mod] = 'Y';
		m.con_3[mod] = null_ptr;
		m.con_12[mod] = null_ptr;
		m.con_24[mod] = null_ptr;
		m.con_48[mod] = null_ptr;
		m.con_96[mod] = null_ptr;
		m.con_192[mod] = null_ptr;

		m.no_con1[mod] = null_ptr;
		m.no_con2[mod] = null_ptr;
		m.no_con3[mod] = null_ptr;
		m.no_con4[mod] = null_ptr;
	}
	return(&m);
}

/*
 * Update the modem database.  Other routines actually do the changes
 * or deletions in memory.  A non-zero return code means non-fatal error.
 */

int
up_modem()
{
	FILE *fp, *my_fopen();
	char buf[80];
	int i;
	void error_win();

					/* open for write */
	if (!(fp = my_fopen(modem->m_path, "w"))) {
		sprintf(buf, "\"%s\"", modem->m_path);
		error_win(0, "No write permission on modem/TTY database file", buf);
		return(1);
	}
					/* put back the TTY entries */
	for (i=0; i<modem->t_entries; i++)
		fprintf(fp, "TTY_%d=%s;%s;%d\n", i+1, modem->tty[i],
		 modem->tname[i], modem->init_sp[i]);

					/* put back the modem entries */
	for (i=0; i<modem->m_entries; i++) {
		fprintf(fp, "MODEM_%da=%s;%s;%s;%s;%s\n", i+1, modem->mname[i],
		 modem->init[i], modem->dial[i], modem->suffix[i],
		 modem->hang_up[i]);

		fprintf(fp, "MODEM_%db=%c;%s;%s;%s;%s;%s;%s\n", i+1,
		 modem->auto_baud[i], modem->con_3[i], modem->con_12[i],
		 modem->con_24[i], modem->con_48[i], modem->con_96[i],
		 modem->con_192[i]);

		fprintf(fp, "MODEM_%dc=%s;%s;%s;%s\n", i+1, modem->no_con1[i],
		 modem->no_con2[i], modem->no_con3[i], modem->no_con4[i]);
	}

	fclose(fp);
	return(0);
}

/*
 * See if the new modem is already in the database.  If it's not, create
 * a slot for it and update the modem->m_cur variable.
 */

void
create_modem(str)
char *str;
{
	int i;
	char *str_rep(), buf[80];
	void error_win();
					/* modem entry already exists? */
	for (i=0; i<modem->m_entries; i++) {
		if (!strcmp(str, modem->mname[i]))
			return;
	}
					/* empty slot available? */
	if (modem->m_entries == NUM_MODEM) {
		sprintf(buf, "\"%s\"", modem->m_path);
		error_win(0, "No empty modem slots in", buf);
		return;
	}
					/* create a new entry */
	i = modem->m_entries;
	modem->mname[i] = str_rep(modem->mname[i], str);

					/* update number of entries */
	modem->m_entries++;
	return;
}

/*
 * See if the modem names in the list still need to be in the database.
 * If you find a "lost" entry, delete it and collapse the list.
 */

void
del_modem()
{
	extern char *null_ptr;
	int i, j, match;
	char *str_rep();
	void free_ptr();

	for (i=0; i<modem->m_entries; i++) {
		match = 0;
		for (j=0; j<modem->t_entries; j++) {
			if (!strcmp(modem->mname[i], modem->tname[j])) {
				match++;
				break;
			}
		}
					/* found a "lost" modem name */
		if (!match) {
			for (j=i; j<modem->m_entries-1; j++) {
					/* copy the info */
				modem->mname[j] = str_rep(modem->mname[j], modem->mname[j+1]);
				modem->init[j] = str_rep(modem->init[j], modem->init[j+1]);
				modem->dial[j] = str_rep(modem->dial[j], modem->dial[j+1]);
				modem->suffix[j] = str_rep(modem->suffix[j], modem->suffix[j+1]);
				modem->hang_up[j] = str_rep(modem->hang_up[j], modem->hang_up[j+1]);

				modem->auto_baud[j] = modem->auto_baud[j+1];
				modem->con_3[j] = str_rep(modem->con_3[j], modem->con_3[j+1]);
				modem->con_12[j] = str_rep(modem->con_12[j], modem->con_12[j+1]);
				modem->con_24[j] = str_rep(modem->con_24[j], modem->con_24[j+1]);
				modem->con_48[j] = str_rep(modem->con_48[j], modem->con_48[j+1]);
				modem->con_96[j] = str_rep(modem->con_96[j], modem->con_96[j+1]);
				modem->con_192[j] = str_rep(modem->con_192[j], modem->con_192[j+1]);

				modem->no_con1[j] = str_rep(modem->no_con1[j], modem->no_con1[j+1]);
				modem->no_con2[j] = str_rep(modem->no_con2[j], modem->no_con2[j+1]);
				modem->no_con3[j] = str_rep(modem->no_con3[j], modem->no_con3[j+1]);
				modem->no_con4[j] = str_rep(modem->no_con4[j], modem->no_con4[j+1]);
			}
			j = modem->m_entries -1;

			free_ptr(modem->mname[j]);
			free_ptr(modem->init[j]);
			free_ptr(modem->dial[j]);
			free_ptr(modem->suffix[j]);
			free_ptr(modem->hang_up[j]);

			free_ptr(modem->con_3[j]);
			free_ptr(modem->con_12[j]);
			free_ptr(modem->con_24[j]);
			free_ptr(modem->con_48[j]);
			free_ptr(modem->con_96[j]);
			free_ptr(modem->con_192[j]);

			free_ptr(modem->no_con1[j]);
			free_ptr(modem->no_con2[j]);
			free_ptr(modem->no_con3[j]);
			free_ptr(modem->no_con4[j]);

					/* create an empty entry */
			modem->mname[j] = null_ptr;
			modem->init[j] = null_ptr;
			modem->dial[j] = null_ptr;
			modem->suffix[j] = null_ptr;
			modem->hang_up[j] = null_ptr;

			modem->auto_baud[j] = 'Y';
			modem->con_3[j] = null_ptr;
			modem->con_12[j] = null_ptr;
			modem->con_24[j] = null_ptr;
			modem->con_48[j] = null_ptr;
			modem->con_96[j] = null_ptr;
			modem->con_192[j] = null_ptr;

			modem->no_con1[j] = null_ptr;
			modem->no_con2[j] = null_ptr;
			modem->no_con3[j] = null_ptr;
			modem->no_con4[j] = null_ptr;

					/* update the counts */
			modem->m_entries--;
			if (modem->m_cur >= modem->m_entries)
				modem->m_cur = -1;
			return;
		}
	}
	return;
}
