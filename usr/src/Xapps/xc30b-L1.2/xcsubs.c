/*	xcsubs.c -- subroutines for XC
	This file uses 4-character tabstops
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <ctype.h>
#include <signal.h>
#include <termio.h>
#ifdef T6000
#include <sys/ioctl.h>
#endif
#include <setjmp.h>
#include "xc.h"

extern jmp_buf erret;

char line[SM_BUFF],	/* Input line */
	 word[SM_BUFF],	/* Parsed word */
	 *wptr, *lptr,	/* Word and line pointers */
	 *tgetstr(),
	 *tgoto();

int	LI,	/* One less than screen length in termcap entry */
	CO;	/* Screen width */
short ospeed;

static char tc[LG_BUFF],	/* termcap buffer */
			tbuf[LG_BUFF], PC, *CD, *CE, *CF, *CL, *CM, *CN, *SO, *SE;

void show();

#define Tgetstr(code) ((s = tgetstr(code,&p)) ? s : "")

#if	!STRSTR		/* For those that do not have strstr() */
/*	Find first occurence of str2 in str1 and return a pointer to it */
char *strstr(str1, str2)
char *str1, *str2;
{
	register char *Sptr, *Tptr;
	int len = strlen(str1) -strlen(str2) + 1;

	if (*str2)
		for (; len > 0; len--, str1++) {
			if (*str1 != *str2)
				continue;

			for (Sptr = str1, Tptr = str2; *Tptr != '\0'; Sptr++, Tptr++)
				if (*Sptr != *Tptr)
					break;

			if (*Tptr == '\0')
				return str1;
		}

	return NULLS;
}
#endif

#if !DUP2		/* For those that do not have dup2() */
#include <fcntl.h>
dup2(oldfd, newfd)
int oldfd, newfd;
{
if (fcntl(oldfd, F_GETFL, 0) == -1)			/* Valid file descriptor? */
		return (-1);						/* No, return an error */
	close(newfd);							/* Ensure newfd is closed */
	return (fcntl(oldfd, F_DUPFD, newfd));	/* Dup oldfd into newfd */
}

#endif /* !DUP2	Thanks to Bill Allie CIS: 76703,2061 */

#if !STRDUP	/* For those that do not have strdup() */
char *strdup(s) 
char *s;
{
	return strcpy((char *)malloc(strlen(s)+1), s);
}
#endif

#if !MEMSET		/* For those that do not have memset() */
char *memset(dst, chr, len)
char *dst;
register chr;
register len;
{
	char *d;
	for (d = dst; --len >= 0; *d++ = chr)
		;
	return dst;
}
#endif

void msecs(t)
long t;
{
	long start;
	struct tms Tbuf;

	start = times(&Tbuf);
	while ((times(&Tbuf)-start) < (t*HZ)/1000)
		;
}

/*	Do the fork call, packaging the error return so that the caller
	need not have code for it.
*/
forkem()
{
	int i;

	if ((i = fork()) < 0) {
		show(1,"XC: Fork failed");
		longjmp(erret,1);
	}
	return(i);
}

/*	Throw away all input characters until no more are sent. 
void purge()
{
	while (readbyte(1) != -1)
		;
}
*/

/*	Line input routine to be used when the raw terminal mode is in effect. */
void getline()
{
	int c, i = 0;

	lptr = line;
	memset(line, 0, SM_BUFF);

	while ((c = trminp()) != '\r' && c != '\n') {
        if (c == '\177')
             c = '\b' ; 
		if (c == '\b') {
			if (i > 0) {
				line[--i] = '\0';
				fprintf(tfp,"\b \b");
			} else
				beep();
			continue;
		}
        if(i < SM_BUFF )  {  /* protect against buffer over run */
    	    line[i++] = c;
    		fputc(c, tfp); }
        else
            i-- ;
	}
}

/*	Parse the "line" array for a word */
void getword()
{
	char quote, *ptr = word;
	short carat = FALSE, backslash = FALSE;

	wptr = lptr;
	memset(word, 0, SM_BUFF);

	while (isspace(*lptr) )
		lptr++;


	if (*lptr == '\0')
		return;

	if (*lptr == '\'' || *lptr == '\"')
		quote = *lptr++;
	else
		quote = '\0';

	for (; *lptr != '\0'; lptr++) {
		if (quote) {
			if (*lptr == '\0') {
				word[0] = '\0';
				sprintf(Msg,"Unmatched quote: %s", line);
				S;
				s_exit();
			}
			if (*lptr == quote)
				break;
		} else if (!backslash && isspace(*lptr))
       			break;

		if (carat)
			*ptr++ = *lptr & 0x1f,
			carat = FALSE;
		else if (backslash)
			*ptr++ = *lptr,
			backslash = FALSE;
		else if (*lptr == '^')
			carat = TRUE;
		else if (*lptr == '\\')
			backslash = TRUE;
		else
			*ptr++ = *lptr;
	}

	lptr++;
}

/*	Make the specified word all lower case */
void lc_word(ptr)
char *ptr ;
{

	while (*ptr) {
    	*ptr = tolower(*ptr);
		ptr++;
	}

}

/*	Get a single character from the local terminal */
trminp()
{
	static int count = 0;
	static char *p, kbbuf[SM_BUFF];
     if(*p == '\177' )
         *p = 0x8 ; 
     if (count > 0) {
		count--;
		return(*p++ & 0xff);
	}

	while ((count = read(0, p = kbbuf, SM_BUFF)) < 1)
		;

	count--;
	return(*p++ & 0xff);
}

void intdel(flag)
{
	if (flag)
		ioctl(0, TCSETAW, &sigmode);
	else
		ioctl(0, TCSETAW, &newmode);
}

beep()
{
	putc(7,tfp);
}

/*	initialize termcap stuff */
void get_ttype()
{
	char *ttytype;
	char *p = tbuf;
	char *s;

	if ((ttytype = getenv("TERM")) == NULLS) {
		show(1,"TERM not set");
		exit(6);
	}
	if (tgetent(tc, ttytype) != 1) {
		sprintf(Msg,"Can't load %s", ttytype);
		S;
		exit(7);
	}
	ospeed = newmode.c_cflag & CBAUD;
	LI = tgetnum("li") - 1;
	CO = tgetnum("co");
	if ((s=Tgetstr("pc")) == NULLS)
		PC = '\0';
	else
		PC = *s;
	
	CD = Tgetstr("cd");
	CE = Tgetstr("ce");
	CL = Tgetstr("cl");
	CM = Tgetstr("cm");
	SE = Tgetstr("se");
	SO = Tgetstr("so");
	CF = Tgetstr("CF");
	CN = Tgetstr("CN");
}

/*	putchr() is a routine to pass to tputs() */
void putchr(c)
int c;
{
	putc(c,tfp);
}

void cls()		{ tputs(CL,LI,putchr); }

void cur_on()	{ tputs(CN,1,putchr); }

void cur_off()	{ tputs(CF,1,putchr); }


void cl_line()	{ tputs(CE,1,putchr); }

void cl_end()	{ tputs(CD,LI,putchr); }

void ttgoto(row, col)
int row, col;
{
	tputs(tgoto(CM, col, row),1,putchr);
}

void drawline(row, col, len)
int row, col, len;
{

	ttgoto(row, col);
	while (len--)
		fputc('-', tfp);
}

void show(flag, str)
short flag;
char *str;
{
	if (!flag) {
		beep();
		ttgoto(LI,0);
		cl_line();
		ttgoto(LI,(CO-(int)strlen(str))/2 -1);
	}
	if (flag == 2)
		putc('\n', tfp);
	tputs(SO,1,putchr);
	putc(' ',tfp);
	fprintf(tfp, str);
	putc(' ',tfp);
	tputs(SE,1,putchr);
	if (flag > 0)
		putc('\n',tfp);
}

void show_abort()
{
	show(2,"USER ABORT");
}

/*	fetch a "hot key"
	returns: (int)
*/
dgetch()
{
	char c;
	struct	termio oldt, t;

	ioctl(0, TCGETA, &oldt);
	t = oldt;
	t.c_lflag &= ~ICANON;	/* turn off canonicalization */
	t.c_lflag &= ~ECHO;		/* turn off echo */
	t.c_lflag &= ~ISIG;		/* turn off signal processing */
	t.c_cc[VMIN] = 1;		/* single-character I/O */
	ioctl(0, TCSETAW, &t);

	read(0, &c, 1);

	ioctl(0, TCSETAW, &oldt);
	return((int)c);
}

FILE *isregfile(pathname)
char *pathname;
{
	struct stat statbuf;

	if (stat(pathname,&statbuf) || statbuf.st_mode & S_IFMT != S_IFREG)
		return NULLF;
	return fopen(pathname, "r");
}


FILE *openfile(name)
char *name;
{
	FILE *fp = NULLF;
	char *home, fullname[SM_BUFF], *path, *pathend;
	int pathlen;

	if ((path = getenv("XC_PATH")) != NULLS) {
		while (fp == NULLF) {
			if ((pathend = strchr(path, ':')) == NULLS)
				pathlen = strlen(path);
			else
				pathlen = pathend - path;

			sprintf(fullname, "%.*s/%s", pathlen, path, name);
			fp = isregfile(fullname);

			path += pathlen;
			if (*path == '\0')
				break;
			path++;
		}
	}

	if (fp == NULLF)
		fp = isregfile(name);
	
	if (fp == NULLF) {
		if ((home = getenv("HOME")) != NULLS)
			sprintf(fullname, "%s/%s", home, name);
		fp = isregfile(fullname);
	}

	if (fp == NULLF) {
		sprintf(fullname, "%s/%s", LIBDIR, name);
		fp = isregfile(fullname);
	}

	return fp;
}
