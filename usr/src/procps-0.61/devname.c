/*
 * devname.c
 *
 * modified by Michael K. Johnson, johnsonm@stolaf.edu for YAPPS
 * Needed to not return "a1", "a2", etc. for virtual terminals so that
 * it would jive with the utmp entries...
 */

#include <sys/stat.h>
#include <string.h>

/*
 * ttynames:
 * 	ttya0 1  2 ...	virtual consoles
 *	ttys0 S1 S2...	serial lines
 *	ttyp0 p1 p2...	pty's
 */

static char *ttgrp = "    StuvPQRWpqrs";
static char *ttsub = "0123456789abcdef";

void dev_to_tty(char *tty, int dev)
{

  if (dev == -1)
    strcpy(tty,"?");
  else if (dev == 0)
    strcpy(tty,"co");
  else if (dev < 64) {
    tty[0] = (dev % 10) + '0';
    tty[1] = (dev / 10) + '0'; if (tty[1] == '0') tty[1] = 0;
  } else {
    tty[0] = ttgrp[(dev >> 4) & 017];
    tty[1] = ttsub[dev & 017];
  }
  tty[2] = 0;
}

int tty_to_dev(char *tty)
{
    char *p, *q;

    if (*tty == '\0') {		/* empty string: controlling tty */
	struct stat buf;
	if (fstat(0, &buf) != -1)
	    return(buf.st_rdev & 0xff);
	else
	    return -1;
    }
    if (tty[1] == '\0' && *tty >= '0' && *tty <= '9')
	return(*tty - '0');
    if (strcmp(tty, "co") == 0)
	return 0;
    if ((p = strchr(ttgrp, *tty)) != NULL &&
	(q = strchr(ttsub, tty[1])) != NULL)
	return(((p - ttgrp) << 4) | (q - ttsub));
    else
	return -1;
}
