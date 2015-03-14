/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: dmgr.c,v 4.1 88/06/30 10:06:21 bianchi Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/dmgr.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/dmgr.c,v $$Revision: 4.1 $";

/* Ditroff to MGR conversion filter  (sample version) */

#include "term.h"
#include <signal.h>
#include	<ctype.h>

/* ditroff macros */

#define t_push()	
#define t_pop()
#define hmot(N)		(hpos+=(N))
#define hgoto(N)	(hpos = (N))
#define vmot(N)		(vpos+=(N))
#define vgoto(N)	(vpos=(N))
#define put1s(c)		put1('?')
#define put1a(N)	printf("\\%3.3o",N);
#define t_text(S)	printf("{%s}",S)
#define draw(S)		printf("\nDRAW %s\n",S);
#define setfont(S)	(font=(S))
#define setsize(N)	(size=(N))
#define t_page()	(page++>0?(getpass("\007"),m_clear()):0)
#define t_newline()	m_flush()
#define t_init()
#define t_trailer()
#define t_reset(C)
#define	loadfont(N, S1, S2)
#define error		printf
#define done()		getpass("\007")

/* mgr defines */

#define GMAX	1000		/* maximum coordinate */

/* these should be calculated from the ditroff input file */

#define PAGE_WIDE	2300	/* page width (pixels) */
#define PAGE_HIGH	3200	/* page height (pixels) */

int hpos,vpos;
int font;
int size;
int page=0;

main()
   {

	int clean();

	/* setup MGR */

	m_setup(M_DEBUG);
   m_push(P_FLAGS|P_FONT);
   signal(SIGTERM,clean);
   signal(SIGINT,clean);
   m_setmode(M_OVERSTRIKE);
	m_func(14);
   /* m_font(4); */
	m_clear();

	/* display output */

   conv(stdin);

	/* wait for ack. */

	getpass("\007");
   clean(0);
   }

/* print a character */

int
put1(c)
register char c;		/* the character to print */
	{
	register int x = hpos * GMAX / PAGE_WIDE;
   register int y = vpos * GMAX / PAGE_HIGH;

	m_movecursor(x,y);
	putc(c,m_termout);
	switch(font) {
		case 2:	/* italic */
			m_printstr("\010_");
			break;
		case 3:	/* bold */
			m_incr(2);
			break;
		}
	}

/* do ditroff conversion (standard template) */

conv(fp)
register FILE *fp;
{
	register int c, k, sign;
	int m, n, i, n1, m1;
	char str[100], buf[300];

	while ((c = getc(fp)) != EOF) {
		switch (c) {
		case '\n':	/* when input is text */
		case ' ':
		case 0:		/* occasional noise creeps in */
			break;
		case '{':	/* push down current environment */
			t_push();
			break;
		case '}':
			t_pop();
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/* two motion digits plus a character */
			hmot((c-'0')*10 + getc(fp)-'0');
			put1(getc(fp));
			break;
		case 'c':	/* single ascii character */
			put1(getc(fp));
			break;
		case 'C':
			fscanf(fp, "%s", str);
			put1s(str);
			break;
		case 'N':	/* absolute character number */
			fscanf(fp, "%d", &n);
			put1a(n);
			break;
		case 't':	/* straight text */
			fgets(buf, sizeof(buf), fp);
			t_text(buf);
			break;
		case 'D':	/* draw function */
			fgets(buf, sizeof(buf), fp);
                        draw(buf);
			break;
		case 's':
			fscanf(fp, "%d", &n);	/* ignore fractional sizes */
			setsize(n);
			break;
		case 'f':
			fscanf(fp, "%d", &n);
			setfont(n);
			if (n==3)	/* bold */
				m_func(1);
			else if (n==1 || n==2)
				m_func(14);
			break;
		case 'H':	/* absolute horizontal motion */
			/* fscanf(fp, "%d", &n); */
			while ((c = getc(fp)) == ' ')
				;
			k = 0;
			do {
				k = 10 * k + c - '0';
			} while (isdigit(c = getc(fp)));
			ungetc(c, fp);
			hgoto(k);
			break;
		case 'h':	/* relative horizontal motion */
			/* fscanf(fp, "%d", &n); */
			while ((c = getc(fp)) == ' ')
				;
			k = 0;
			sign = 1;
			if (c == '-') {
				sign = -1;
				c = getc(fp);
			}
			do {
				k = 10 * k + c - '0';
			} while (isdigit(c = getc(fp)));
			ungetc(c, fp);
			hmot(sign * k);
			break;
		case 'w':	/* word space */
			break;
		case 'V':
			fscanf(fp, "%d", &n);
			vgoto(n);
			break;
		case 'v':
			fscanf(fp, "%d", &n);
			vmot(n);
			break;
		case 'p':	/* new page */
			fscanf(fp, "%d", &n);
			t_page();
			break;
		case 'n':	/* end of line */
			while (getc(fp) != '\n')
				;
			t_newline();
			break;
		case '#':	/* comment */
			while (getc(fp) != '\n')
				;
			break;
		case 'x':	/* device control */
			devcntrl(fp);
			break;
		default:
			error("unknown input character %o %c\n", c, c);
			fprintf(stderr, "input context is:\n%c", c);
			for (i = 0; i < 10; i++) {
				if (fgets(buf, sizeof(buf), fp) == NULL)
					break;
				fprintf(stderr, "%s", buf);
			}
			done();
		}
	}
   }

devcntrl(fp)	/* interpret device control functions */
FILE *fp;
{
        char str[20], str1[50], buf[50];
	int c, n;

	fscanf(fp, "%s", str);
	switch (str[0]) {	/* crude for now */
	case 'i':	/* initialize */
		t_init();
		break;
	case 'T':	/* device name */
		fscanf(fp, "%s", buf);
		break;
	case 't':	/* trailer */
		t_trailer();
		break;
	case 'p':	/* pause -- can restart */
		t_reset('p');
		break;
	case 's':	/* stop */
		t_reset('s');
		break;
	case 'r':	/* resolution assumed when prepared */
		fscanf(fp, "%d", &n);
		break;
	case 'f':	/* font used */
		fscanf(fp, "%d %s", &n, str);
		fgets(buf, sizeof buf, fp);	/* in case there's a filename */
		ungetc('\n', fp);	/* fgets goes too far */
		str1[0] = 0;	/* in case there's nothing to come in */
		sscanf(buf, "%s", str1);
		loadfont(n, str, str1);
		break;
	}
	while ((c = getc(fp)) != '\n')	/* skip rest of input line */
		if (c == EOF)
			break;
}

int
clean(n)
int n;
	{
	m_pop();
	m_clear();
   }
