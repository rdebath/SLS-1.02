/*	xcdial.c -- dialing directory module for XC
	This file uses 4-character tabstops
	Author: Steve Manes 8/26/88
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <fcntl.h>
#include "xc.h"

#define MAXNAME		27
#define MAXNUMBER	20
#define MAXPROTOCOL	 9
#define MAXSCRIPT	14
#define NOWAITT   "*70,"
#define NOWAITP   "1170,"

static FILE *dirf;
static long pages[57];		/* offsets into phonefile */
static short dirnum, thispage, lastpage;
static char *last_nbr = NULLS;
extern short s_flag, tone, nowait;
extern void 
cl_end(), newbmask();
extern void 
pauz();

/*	showentry(dirnum, entry)
	show a single, formatted dialdir entry.
	check its format integrity as we go
*/
static void 
showentry(choice, entry)
	short choice;
	char *entry;
{
	char name[MAXNAME + 1], num[MAXNUMBER + 1], f[30], protocol[MAXPROTOCOL + 1], script[MAXSCRIPT + 1];
	char *s;
	short i, j = 0;

	s = entry;

	/* get phone number */
	while (isspace(*s))
		s++;

	for (i = 0; i < MAXNUMBER && !isspace(*s); i++)
		num[j++] = *s++;
	num[j] = '\0';

	/* get name */
	while (!isspace(*s))
		s++;
	while (isspace(*s))
		s++;
	j = 0;
	for (i = 0; i < MAXNAME && *s != '\t'; i++)
		name[j++] = *s++;
	name[j] = '\0';
	s = strchr(name, '\n');
	if (s)
		*s = '\0';

	/* get protocol */
	if (s = strstr(entry, "BPS=")) {
		s += 4;
		j = 0;
		for (i = 0; i < 6 && isdigit(*s); i++)
			protocol[j++] = *s++;
		protocol[j] = '\0';
		strcat(protocol, "/");
	}
	else
		sprintf(protocol, "????%c", '/');

	if (s = strstr(entry, "BITS=")) {
		s += 5;
		switch (*s) {
		case '7':
			strcat(protocol, "7/");
			break;
		case '8':
			strcat(protocol, "8/");
			break;
		default:
			beep();
			sprintf(Msg, "Invalid BITS= for '%s'", name);
			S;
			strcat(protocol, "8/N");
			return;
		}
	}
	else
		strcat(protocol, "?/");

	if (*s == '7')
		strcat(protocol, "E");
	else
		strcat(protocol, "N");

	if (s = strstr(entry, "SCRIPT=")) {
		s += 7;
		j = 0;
		for (i = 0; i < MAXSCRIPT && !isspace(*s); i++)
			script[j++] = *s++;
		script[j] = '\0';

	}
	else
		script[0] = '\0';

	sprintf(f, "%%3d - %%-%ds %%%ds %%%ds %%-%ds\n",
		MAXNAME, MAXNUMBER, MAXPROTOCOL, MAXSCRIPT);
	fprintf(tfp, f, choice, name, num, protocol, script);
}

/*	scroll_dir()
	scroll directory at current filepos
*/
static void 
scroll_dir()
{
	short i;
	char buff[121];

	ttgoto(4, 0);
	cur_off();
	cl_end();

	fseek(dirf, pages[thispage], 0),
		dirnum = thispage * (LI - 6);
	for (i = 0; i < LI - 6; i++) {
		if (fgets(buff, 120, dirf) == NULLS) {
			lastpage = thispage;
			break;
		}
		showentry(++dirnum, buff);
	}

	pages[thispage + 1] = ftell(dirf);
	if (fgets(buff, 120, dirf) == NULLS)
		lastpage = thispage;
	cur_on();
}

/* Dial a phone number, using proper format and delay. */
void 
xcdial(s)
	char *s;
{
	char buffer[SM_BUFF], *p;

	p = buffer;

	if (last_nbr)
		free(last_nbr);

	last_nbr = strdup(s);
	if (tone) {
		p += sprintf(p, DIALSTRT);
		if (nowait)
			p += sprintf(p, NOWAITT);
	}
	else {
		p += sprintf(p, DIALSTRP);
		if (nowait)
			p += sprintf(p, NOWAITP);
	}
	sprintf(p, "%s\r", s);

	send_slowly(buffer);
}

static void 
parse_entry(buf)
	char *buf;
{
	char *s, *t, *nbr;

	if (s = strstr(buf, "BPS=")) {
		s += 4;
		if (mrate(s) < 0) {
			show(0, "Invalid BPS=");
			return;
		}
	}
	if (s = strstr(buf, "BITS=")) {
		s += 5;
		switch (*s) {
		case '7':
			if (bitmask == 0xff)
				newbmask();
			break;
		case '8':
			if (bitmask == 0x7f)
				newbmask();
			break;
		default:
			show(0, "Invalid BITS=");
			return;
		}
	}

	cls();
	if ((s = strstr(buf, "PREFIX=")) != NULLS) {
		s += 7;
		send_slowly("\r");
		send_slowly(s);
		send_slowly("\r");
		s -= 7;
		*s = '\0';
		sleep(2);
	}
	sprintf(Msg, "Calling %s", buf);
	S;

	while (isspace(*buf) && *buf)
		buf++;

	if (!(*buf))
		return;

	for (nbr = buf; !isspace(*buf) && *buf; buf++);

	*buf = '\0';
	intdel(TRUE);
	xcdial(nbr);

	if (s = strstr(++buf, "SCRIPT=")) {
		s += 7;
		t = s;
		while (!isspace(*t) && *t != '\0')
			t++;
		*t = '\0';
		sprintf(ddsname, "%s", s);
		s_flag = linkflag = TRUE;
	}
	intdel(FALSE);
}

dial_dir()
{
	int i, c;
	char buff[5], f[SM_BUFF];

	if ((dirf = openfile(phonefile)) == NULLF) {
		sprintf(Msg, "Phonelist '%s' not found", phonefile);
		S;
		pauz();
		return FAILURE;
	}

	dirnum = thispage = 0;
	lastpage = -1;
	cls();
	sleep(1);
	cls();
	drawline(0, 0, CO);
	ttgoto(1, 23);
	show(-1, " D I A L I N G   D I R E C T O R Y ");
	drawline(2, 0, CO);
	ttgoto(3, 0);
	sprintf(f, "     %%-%ds %%%ds %%%ds %%-%ds%*s\n",
		MAXNAME, MAXNUMBER, MAXPROTOCOL, MAXSCRIPT,
		CO - MAXNAME - MAXNUMBER - MAXPROTOCOL - MAXSCRIPT - 8, "");
	sprintf(Msg, f, "NAME", "NUMBER", "PROTOCOL", "SCRIPT");
	show(-1, Msg);
	scroll_dir();
	while (1) {
		ttgoto(LI - 1, 0);
		fprintf(tfp,
			"==>     [#] Dial Entry   [M]anual Dial   [X]it   [N]ext   [P]revious");
		ttgoto(LI - 1, 4);
		while (1) {
			c = toupper(dgetch());
			if (c == '\b')
				continue;
			if (c == 'N' || c == '\n' || c == ' ') {
				if (thispage > (int) ((1000 / (LI - 6)) - 1) || thispage == lastpage)
					show(0, "Last page");
				else
					thispage++,
						scroll_dir();
				break;
			}
			else if (c == 'P' && dirnum > 1) {
				if (thispage == 0)
					show(0, "First page");
				else
					thispage--,
						scroll_dir();
				break;
			}
			else if (c == 'X') {
				cls();
				fclose(dirf);
				return FAILURE;
			}
			else if (c == 'M') {
				if (man_dial()) {
					fclose(dirf);
					reterm = TRUE;
					return SUCCESS;
				}
				reterm = FALSE;
				break;
			}
			else if (isdigit(c)) {
				buff[0] = c;
				fputc(c, tfp);
				for (i = 1; i < 4; ++i) {
					buff[i] = getchar();
					if (buff[i] == '\b') {
						if (i > 0)
							fprintf(tfp, "\b \b"),
								i -= 2;
						else
							i = -1;
						continue;
					}
					fputc(buff[i], tfp);
					if (buff[i] == '\n' || buff[i] == '\r')
						break;
				}
				if (i == 0) {
					reterm = FALSE;
					break;
				}
				buff[++i] = '\0';
				if (dial_entry(atoi(buff))) {
					fclose(dirf);
					reterm = TRUE;
					return SUCCESS;
				}
				reterm = FALSE;
				break;
			}
		}
	}
}
static 
dial_entry(choice)
	short choice;
{
	char buff[121];

	if (choice == 0)
		return FAILURE;
	rewind(dirf);
	while (choice--) {
		if (fgets(buff, 120, dirf) == NULLS) {
			show(0, "Nonexistent entry");
			return FAILURE;
		}
	}
	parse_entry(buff);
	return SUCCESS;
}

static 
man_dial()
{
	ttgoto(LI - 1, 0);
	cl_end();
	fprintf(tfp, "Number to dial: ");
	getline();
	if (!line[0])
		return FAILURE;
	parse_entry(line);
	return SUCCESS;
}

redial()
{
	char *s;

	if (last_nbr == NULLS) {
		show(1, "REDIAL FAILURE");
		return -1;
	}

	s = strdup(last_nbr);
	xcdial(s);
	free(s);
	return 0;
}
