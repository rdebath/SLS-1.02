/*
 * The routines that dial the modem and listen for the return codes.
 */

#define HZ	60

#include <stdio.h>
#include <sys/types.h>
#include <sys/times.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "modem.h"
#include "param.h"

#ifdef UNIXPC
#include <sys/phone.h>
#endif /* UNIXPC */

/*
 * Get the dial string ready, send it to the modem.  The parameter is not
 * the actual entry number, it is an index into the queue.
 */

void
dial_it(num)
int num;
{
	extern int fd;
	int i, skip;
	char s[100], number[40], *strcpy(), *strcat(), *n, *strchr();
	void send_str();
#ifdef UNIXPC
	struct updata pbuf;
	unsigned int sleep();
#endif /* UNIXPC */

	/*
	 * Create the string to be sent to the modem.  The long distance
	 * codes are added if they are requested.
	 */
	s[0] = '\0';
	strcpy(s, modem->dial[modem->m_cur]);

	switch (dir->q_ld[num]) {
		case 0:			/* no ld code requested */
			break;
		case '+':
			strcat(s, param->ld_plus);
			break;
		case '-':
			strcat(s, param->ld_minus);
			break;
		case '@':
			strcat(s, param->ld_at);
			break;
		case '#':
			strcat(s, param->ld_pound);
			break;
	}
	/*
	 * Purify the phone number by removing all the pretty characters
	 * that don't need to be sent to the modem.  Typically the "-",
	 * "(", ")", and space characters are just for looks.  To prevent
	 * this action, prepend a "\" to the character.
	 */
	i = 0;
	skip = 0;
	n = dir->number[dir->q_num[num]];
	while (*n) {
		if (*n == '\\' && !skip) {
			skip++;
			n++;
			continue;
		}
		if (!strchr("-() ", *n) || skip)
			number[i++] = *n;
		n++;
		skip = 0;
	}
	number[i] = '\0';
					/* add it to the string */
	strcat(s, number);
	strcat(s, modem->suffix[modem->m_cur]);
#ifdef DEBUG
	fprintf(stderr, "raw dial string: \"%s\"\n", s);
#endif /* DEBUG */

#ifdef UNIXPC
					/* special case for OBM */
	if (!strcmp(modem->mname[modem->m_cur], "OBM")) {
					/* prepare the modem */
		pbuf.c_lineparam = DATA|DTMF;
		pbuf.c_waitdialtone = 5;
		pbuf.c_linestatus = 0;
		pbuf.c_feedback = SPEAKERON|NORMSPK;
		pbuf.c_waitflash = 500;
		ioctl(fd, PIOCSETP, &pbuf);
		sleep(1);
					/* connect the dialer */
		ioctl(fd, PIOCRECONN);
		sleep(1);
					/* dial each digit */
		n = s;
		while (*n) {
					/* switch tone/pulse dialing? */
			switch (*n) {
				case '^':
					pbuf.c_lineparam = DATA|PULSE;
					ioctl(fd, PIOCSETP, &pbuf);
					break;
				case '%':
					pbuf.c_lineparam = DATA|DTMF;
					ioctl(fd, PIOCSETP, &pbuf);
					break;
				default:
					ioctl(fd, PIOCDIAL, n);
					break;
			}
			n++;
		}
		return;
	}
#endif /* UNIXPC */

	send_str(s, SLOW);
	return;
}

/*
 * Send a string to the modem.  Performs all the character synonym
 * translations.
 */

void
send_str(s, slow)
char *s;
int slow;
{
	extern int fd;
	int skip, has_pause;
	char *strchr();
	unsigned int sleep();
	void do_pause();
					/* empty string? */
	if (s == NULL || *s == '\0')
		return;

					/* contains a pause? */
	has_pause = 0;
	if (strchr(s, '~'))
		has_pause++;

	tty_flush(fd, 1);
	/*
	 * Change the character synonyms to their real values.  Writes
	 * the characters to the modem.  To remove the special meaning
	 * of one of the characters, prepend a "\" to it.
	 */
	skip = 0;
	while (*s) {
					/* send the literal character */
		if (skip) {
			skip = 0;
			write(fd, s, 1);
			if (has_pause || slow)
				tty_drain(fd);
			if (slow)
				do_pause();
#ifdef DEBUG
			fprintf(stderr, "send_str: \"%c\", %02x, %03o, %d\n", *s, *s, *s, *s);
#endif /* DEBUG */
			s++;
			continue;
		}
					/* turn off the special meaning */
		if (*s == '\\') {
			skip++;
			s++;
			continue;
		}
					/* pause synonym */
		if (*s == param->pause_char) {
			sleep(1);
			s++;
			continue;
		}
					/* carriage return synonym */
		if (*s == param->cr_char)
			*s = '\r';
					/* 2 character control sequence */
		if (*s == param->ctrl_char) {
			s++;
					/* premature EOF? */
			if (*s == '\0')
				break;
					/* upper and lower case */
			if (*s > '_')
				*s -= 96;
			else
				*s -= 64;
		}
					/* escape synonym */
		if (*s == param->esc_char)
			*s = ESC;
					/* modem break synonym */
		if (*s == param->brk_char) {
			tty_break(fd);
			sleep(1);
			s++;
			continue;
		}

		write(fd, s, 1);
#ifdef DEBUG
		fprintf(stderr, "send_str: \"%c\", %02x, %03o, %d\n", *s, *s, *s, *s);
#endif /* DEBUG */
		/*
		 * Because the pause char makes the timing critical, we
		 * wait until the buffer is clear before we continue.
		 */
		if (has_pause || slow)
			tty_drain(fd);
		if (slow)
			do_pause();
		s++;
	}
	return;
}

/*
 * Read the result codes coming back from the modem.  Test for the 6
 * "connect" strings and the 4 "no connect" strings.  Return the connected
 * baud rate (as a string) or the error message.
 */

char rc_buf[512];
int rc_index;

char *
read_codes()
{
	extern int fd;
	int i;
	char c;
#ifdef UNIXPC
	unsigned int sleep();
	struct updata pbuf;
					/* special case for OBM */
	if (!strcmp(modem->mname[modem->m_cur], "OBM")) {
		ioctl(fd, PIOCGETP, &pbuf);

		/*
		 * The OBM doesn't use a return message to announce the
		 * connection to a remote, so we fake one.  The 1200
		 * is quite arbitrary... it is not an indicator of the
		 * connected baud rate.
		 */
		if (pbuf.c_linestatus & MODEMCONNECTED)
			return("1200");

		sleep(1);
		return(NULL);
	}
#endif /* UNIXPC */
					/* search for key words */
	for (; rc_index<511; rc_index++) {
		if ((i = getc_line(1)) <= 0)
			return(NULL);

		c = i & 0x7f;
#ifdef DEBUG
		fprintf(stderr, "read_codes: \"%c\", %02x, %03o, %d\n", c, c, c, c);
#endif /* DEBUG */
					/* no NULLs please */
		if (c == '\0') {
			if (rc_index)
				rc_index--;
			continue;
		}
		rc_buf[rc_index] = c;
		rc_buf[rc_index+1] = '\0';
					/* the connect strings */
		if (match(rc_buf, modem->con_3[modem->m_cur]))
			return("300");

		if (match(rc_buf, modem->con_12[modem->m_cur]))
			return("1200");

		if (match(rc_buf, modem->con_24[modem->m_cur]))
			return("2400");

		if (match(rc_buf, modem->con_48[modem->m_cur]))
			return("4800");

		if (match(rc_buf, modem->con_96[modem->m_cur]))
			return("9600");

		if (match(rc_buf, modem->con_192[modem->m_cur]))
			return("19200");

					/* the no connect strings */
		if (match(rc_buf, modem->no_con1[modem->m_cur]))
			return(modem->no_con1[modem->m_cur]);

		if (match(rc_buf, modem->no_con2[modem->m_cur]))
			return(modem->no_con2[modem->m_cur]);

		if (match(rc_buf, modem->no_con3[modem->m_cur]))
			return(modem->no_con3[modem->m_cur]);

		if (match(rc_buf, modem->no_con4[modem->m_cur]))
			return(modem->no_con4[modem->m_cur]);
	}
					/* ran out of buffer? */
	return("ERROR");
}

/*
 * Test for a match between two character strings.  A non-zero return code
 * means that s2 was found at the end of s1.
 */

static int
match(s1, s2)
char *s1, *s2;
{
	register int i;
	int skip, diff;
	char new[40];
					/* if no string to match */
	if (*s2 == '\0')
		return(0);
					/* translate synonyms */
	i = 0;
	skip = 0;
	while (*s2) {
					/* literal character */
		if (skip) {
			skip = 0;
			new[i++] = *s2;
			s2++;
			continue;
		}
					/* turn off the special meaning */
		if (*s2 == '\\') {
			skip++;
			s2++;
			continue;
		}
					/* carriage return synonym */
		if (*s2 == param->cr_char)
			*s2 = '\r';

					/* 2 character control sequence */
		if (*s2 == param->ctrl_char) {
			s2++;
			if (*s2 == '\0')
				break;
			if (*s2 > '_')
				*s2 -= 96;
			else
				*s2 -= 64;
		}
					/* escape synonym */
		if (*s2 == param->esc_char)
			*s2 = ESC;

		new[i++] = *s2;
		s2++;
	}
	new[i] = '\0';

	diff = strlen(s1) - strlen(new);
					/* is it possible? */
	if (diff < 0)
		return(0);
					/* test it out */
	if (!strcmp(&s1[diff], new))
		return(1);
	return(0);
}

/*
 * Apparently some modems can't take input at the rated speed while
 * in the command mode.  Therefore, a 0.10 sec pause a required between
 * characters.
 */

void
do_pause()
{
	struct tms t;
	long t1;

	t1 = times(&t);
	while ((times(&t) - t1) < HZ/10)
		;
	return;
}
