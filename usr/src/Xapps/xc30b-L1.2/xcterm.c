/*	xcterm.c -- terminal modemodule for XC
	This file uses 4-character tabstops
*/
#include "xctolu.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>
#include <termio.h>
#include "xc.h"

#define ENDCHAR		(('X' & 0x1f) + 128)	/* Exit terminal mode */
#define TOGCHAR		(('T' & 0x1f) + 128)	/* Toggle capture buffer */
#define DIVCHAR		(('F' & 0x1f) + 128)	/* Send file through modem */
#define DIALCHR		(('D' & 0x1f) + 128)	/* Dial from phonelist */
#define HUPCHAR		(('H' & 0x1f) + 128)	/* Hang up modem */
#define SCRPCHR		(('S' & 0x1f) + 128)	/* Execute script file */
#define BRKCHAR		(('B' & 0x1f) + 128)	/* modem BREAK character */
#define QUITCHR		(('Q' & 0x1f) + 128)	/* close and leave XC */

char captfile[SM_BUFF] = CAPTFILE,	/* capture file's name */
	 phonefile[SM_BUFF] = PHFILE;	/* phone number file's name */

static FILE	*cfp,			/* capture file pointer */
			*fp;			/* file to transmit */

static	int	child_pid;		/* ID of child process */
static	void (*oldvec)();
static	short doneyet_dd;
static	jmp_buf rtm, stop;
char	ddsname[SM_BUFF];
short	s_flag, capture = FALSE;

extern char *strstr();
extern short autoflag, hdplxflag, nlmode;

/* toggle capture status */
static void toggle()
{
	if (capture) {
		fclose(cfp);
		capture = FALSE;
		sprintf(Msg,"\"%s\" closed for capturing",captfile);
		S2;
	} else {
		if ((cfp = fopen(captfile, "a")) == NULLF) {
			sprintf(Msg,"Can't open \"%s\" for capturing",captfile);
			S2;
		} else {
			capture = TRUE;
			sprintf(Msg,"capturing to \"%s\"",captfile);
			setbuf(cfp, NULLS);
			S2;
		}
	}
	sleep(1);

	signal(SIGUSR2, toggle);	/* set signal for next toggle */
}

/*	cleanup, flush and exit */
static void cleanup()
{
	if (capture) {
		fclose(cfp);
		sprintf(Msg,"\"%s\" closed for capturing",captfile);
		S2;
	}

	exit(0);
}

void newbmask()
{
	if (bitmask == 0xff)
		bitmask = 0x7f;
	else
		bitmask = 0xff;


	if (child_pid)
		kill(child_pid, SIGUSR1);
}

static void cisbmode()
{
	cismode = 2;
	signal(SIGCLD, SIG_IGN);

	longjmp(rtm,1);
}

static void end_divert()
{
	show_abort();
	fclose(fp);
	signal(SIGINT, oldvec);
	longjmp(stop,1);
}

/*	Divert file into input stream, with delay after each newline. */
void divert(script)
short script;
{
	int c, i = 0;

	if (!script) {
		fputc('\n',tfp);
		show(-1,"File?");
		getline();
		getword();
	}
	if (word[0] == '\0')
		return;
	if ((fp = fopen(word, "r")) == NULLF) {
		sprintf(Msg,"Can't access '%s'",word);
		S2;
		return;
	}

	oldvec = signal(SIGINT,end_divert);
	if (setjmp(stop))
		return;

	while ((c = getc(fp)) != EOF) {
		if (c != '\n') {
			send_mbyte(c);
			i++;
		}
		else {
			i = (CBAUD-cbaud)*100 + 5*i + 50;
			send_mbyte(nlmode ? '\r' : '\n');
			if (script)
				k_waitfor(-i, "");
			else
				msecs(i);
			i = 0;
		}
	}
	fclose(fp);
	signal(SIGINT,oldvec);
}

void terminal(todir)
short todir;
{
        extern short dosmode ;
	register c;
     
	doneyet_dd = FALSE;
	intdel(FALSE);
Reterm:
	setjmp(rtm);

	if (cismode > 1 || doneyet_dd) {
		if (doneyet_dd)
			doneyet_dd = FALSE;
		return;
	}

	s_flag = FALSE;		/* reset scripting flag */

	if (!todir)
		show(2,"Entering Terminal Mode");
	/* split into read and write processes */
	if ((child_pid = forkem()) == 0) {
		/* child, read proc: read from port and write to tty */

		cfp = NULLF;
		if (autoflag && !todir)
			toggle();
		signal(SIGUSR2, toggle);
		signal(SIGTERM, cleanup);
		signal(SIGUSR1, newbmask);
		while (1) {
			while ((c = read_mbyte(0)) == -1)
				;
			if (cismode && c == ENQ) {
				cismode = 2;
				cleanup();
			}
                        if ( dosmode )  
                          c = codelu[ (unsigned) c ] ;
			fputc(c,tfp);
			if (capture && c != '\r')
				fputc(c,cfp);
		}
		/*NOTREACHED*/
	}
	/* parent, write proc: read from tty and write to port */
	signal(SIGCLD, cisbmode);

	if (todir) goto dialdir;
	do {
		switch (c = getconchr()) {
		case TOGCHAR:		/* signal child to toggle buffer */
			kill(child_pid, SIGUSR2);
			break;

		case DIVCHAR:		/* divert a file through modem port */
			intdel(TRUE);
			divert(FALSE);
			intdel(FALSE);
			break;

		case BRKCHAR:
			xmitbrk();
			break;

		case SCRPCHR:		/* execute a script file */
			if (get_script()==FAILURE)
				break;
			s_flag = TRUE;
			goto filicide;

		case DIALCHR:		/* select and dial a phone number */
dialdir:
			doneyet_dd = TRUE;
			if ((dial_dir()==FAILURE && todir) || s_flag)
				goto filicide;
			if (autoflag)
				toggle();
			break;

		case ENDCHAR:		/* signal child to cleanup and exit */
filicide:
			c = ENDCHAR;
			signal(SIGCLD, SIG_IGN);
			kill(child_pid, SIGTERM);
			break;
		
		case QUITCHR:
			signal(SIGCLD, SIG_IGN);
			kill(child_pid, SIGTERM);
			s_exit();
			break;

		case HUPCHAR:		/* Hangup */
			hangup();
			break;

		case '\n':		/* See if NL translation in effect */
			if (nlmode)
				c = '\r';

        case '\177':     /* Echo rubout as control H */ 

            if (c!='\r' &&  c != '\n')
               c = '\b' ; 

		default:	/* just send the character to the port */
                        if (hdplxflag)
                                 fputc(c,tfp) ;    
                        if( dosmode )
                            c = codepc[ (unsigned) c ] ; 
			send_mbyte(c);
			break;
		}
		todir = FALSE;
	} while (c != ENDCHAR);

	while (wait((int *) 0) >= 0)	/* wait for the read process to die */
		;

	if (s_flag) {
		do_script(ddsname);
		goto Reterm;
	}

	reterm = FALSE;
}

/*	The next three functions are only run by the port read process (child).
	They handle the capture file.
*/
/*	Select a script file. If the file exists, execute it, otherwise
	exit with a non-zero return.
*/
static get_script()
{
	fputc('\n',tfp);
	show(-1,"Enter script file:");
	getline();
	if (line[0] == '\0') {
		fputc('\n',tfp);
		show(1,"Script file not specified");
		return FAILURE;
	}
	linkflag = FALSE;
	getword();
	sprintf(ddsname,"%s",word);
	return SUCCESS;
}

static getconchr()
{
	int c, tc;

	if ((c = trminp()) == MY_ESC) {
		switch (c = ((tc = trminp()) & 0x1f) + 128) {
		case ENDCHAR:
		case TOGCHAR:
		case BRKCHAR:
		case DIVCHAR:
		case DIALCHR:
		case HUPCHAR:
		case SCRPCHR:
		case QUITCHR:
			break;

		default:
			c = tc;
		}
	}
	return c;
}
