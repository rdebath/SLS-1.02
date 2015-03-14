/*
 * Perform administrative functions.  Check to see if the user has
 * permission to make long distance calls, and record all phone calls
 * made by Pcomm.
 */

#include <stdio.h>
#include <grp.h>
#include "config.h"
#include "dial_dir.h"
#include "param.h"

/*
 * Make a log of all calls made by Pcomm.  The argument is the index
 * into the queue.
 */

/* ARGSUSED */
void
log_calls(i)
int i;
{
#ifdef LOG_CALLS
	FILE *fp;
	char *number, *build_num(), *date, *ctime(), *getlogin(), buf[80];
	long now, time();
	void error_win();
					/* build the complete phone number */
	number = build_num(i);
					/* build date and time */
	time(&now);
	date = ctime(&now);
	date[10] = '\0';
	date[16] = '\0';

	if (!(fp = fopen(LOGFILE, "a+"))) {
					/* fatal! (to prevent hanky panky) */
		sprintf(buf, "Can't open log file \"%s\"", LOGFILE);
		error_win(1, buf, "Contact your system administrator");
	}

	fprintf(fp, "pcomm: %s called %s at %s on %s\n", getlogin(), number, &date[11], date);
	fclose(fp);
#endif /* LOG_CALLS */
	return;
}

/*
 * Check to see if long distance (toll) call is authorized.  The argument
 * is the index into the queue.
 */

/* ARGSUSED */
int
limit_ld(i)
int i;
{
#ifdef LIMIT_LD
	char *number, *build_num(), *name, *getlogin();
	struct group *getgrnam(), *grpbuf;

					/* if no group, don't bother */
	grpbuf = getgrnam(GROUP_NAME);
	if (grpbuf == NULL || *grpbuf->gr_mem == '\0')
		return(0);
					/* are you in the group? */
	name = getlogin();
	for (; *grpbuf->gr_mem!='\0'; grpbuf->gr_mem++) {
		if (!strcmp(*grpbuf->gr_mem, name))
			return(0);
	}
					/* numbers only... */
	number = build_num(i);

	/*
	 * VERY SITE SPECIFIC!!!  We use a "9" to get an outside line,
	 * so any 9 followed by a 1 is a toll call (except for 1-800
	 * numbers).
	 */
	if (!strncmp(number, "91", 2) && strncmp(number, "91800", 5)) {
		error_win(0, "You are not authorized to place long distance (toll) calls", "");
		return(1);
	}

	if (*number == '\0') {
		error_win(0, "You are not authorized direct access to the line", "Use the automatic dialing feature");
		return(1);
	}
#endif /* LIMIT_LD */
	return(0);
}

#if defined(LOG_CALLS) || defined(LIMIT_LD)
/*
 * Put together the complete phone number but strip out the extraneous
 * characters.
 */

static char *
build_num(i)
int i;
{
	int j;
	char *t, temp[80], *strcpy(), *strcat();
	static char ans[80];

	temp[0] = '\0';
					/* add LD codes? */
	switch (dir->q_ld[i]) {
		case 0:
			break;
		case '+':
			strcpy(temp, param->ld_plus);
			break;
		case '-':
			strcpy(temp, param->ld_minus);
			break;
		case '@':
			strcpy(temp, param->ld_at);
			break;
		case '#':
			strcpy(temp, param->ld_pound);
			break;
	}
					/* add the number */
	strcat(temp, dir->number[dir->q_num[i]]);

					/* copy only digits */
	j = 0;
	t = temp;
	while (*t) {
		if (*t >= '0' && *t <= '9')
			ans[j++] = *t;
		t++;
	}
	ans[j] = '\0';

	return(ans);
}
#endif /* LOG_CALLS || LIMIT_LD */
