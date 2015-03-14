/*
 * Can you believe it???  Masscomps don't have a function to return the
 * current working directory while in the AT&T universe!
 */

#include <stdio.h>

char *
getcwd(buf, size)
char *buf;
int size;
{
	FILE *pfp, *popen();

	if (!(pfp = popen("pwd", "r")))
		return(".");

	fgets(buf, size, pfp);
	pclose(pfp);
					/* zap the new line */
	buf[strlen(buf)-1] = '\0';
	return(buf);
}
