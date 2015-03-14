/* TTY input driver */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <termios.h>
#include <fcntl.h>
#include "config.h"
#include "global.h"
#include "session.h"

/*
 * This is a tty input package.  A struct ttystate is used to keep
 * track of the state of the connection.  One of these structs
 * must be passed to ttydriv.  By putting all state in this struct,
 * this package can be used to handle multiple connections.
 * 
 * A struct is allocated by open_remote_tty, which opens a tty, sets
 * it to raw mode etc, and returns a pointer to the struct.  That
 * pointer should be treated as opaque by other code.
 *
 * Close_remote_tty takes such a struct, closes the tty and deallocates
 * memory.
 *
 * Input from stdin should be done using NULL instead of a struct.  
 * ttydriv turns this into a pointer to a permanently allocated struct
 * that describes stdin.
 *
 * remote_tty_fd and remote_tty_iflag get a couple of fields from the struct.
 */

extern unsigned char escape;	/* default escape character is ^] */

#define	TTY_LIT	0		/* Send next char literally */
#define	TTY_RAW	1
#define TTY_COOKED	2
/* TTY_ESC is escape char seen */
#define TTY_ESC 3

#define	TTY_NOECHO	0
#define	TTY_ECHO	1

struct ttystate {
  int fd;
  FILE *file;
  int ttymode;
  int ttyecho;
  int confirm;  /* waiting for confirmation of exit */
  unsigned char *linebuf;
  unsigned char *cp;
  struct termios savecon;
  int savettyfl;
};

#define	LINESIZE	256

static unsigned char std_linebuf[LINESIZE];

static struct ttystate std_ttystate = 
   { 0, stdout, TTY_COOKED, TTY_ECHO, 0, std_linebuf, std_linebuf };

#ifdef	FLOW
int ttyflow=1;
#endif

#define CTLR	18
#define	CTLU	21
#define	CTLV	22
#define	CTLW	23
#define	CTLZ	26
#define	RUBOUT	127

/*
 * First, some general explanation.
 * raw and cooked are used with the usual meaning, but they don't
 * involve any change in TTY modes.  Rather, the tty is set into
 * a raw mode, and cooked mode is implemented by doing our own
 * editing here.  The only TTY mode we normally toggle is flow control.
 *
 * This code also handles telnet special characters.  It could be
 * done in telnet, but it turns out to be easier to code it here.
 *
 * Note that raw mode is either TTY_RAW or TTY_ESC, and cooked
 * is either TTY_COOKED or TTY_LIT.  TTY_ESC and TTY_LIT are special
 * substates used when a special character has been seen.
 */

raw(tts)
	struct ttystate *tts;
{
	if (!tts)
		tts = &std_ttystate;

	/* 
	 * TTY_ESC is part of cooked mode, so if it's set, leave it.
	 */
	if (tts->ttymode != TTY_ESC && tts->ttymode != TTY_RAW)
		set_stdout(tts->fd, TTY_RAW); /* CR/LF vs LF madness..-- hyc */
	if (tts->ttymode != TTY_ESC)
		tts->ttymode = TTY_RAW;
}

cooked(tts)
	struct ttystate *tts;
{
	if (!tts)
		tts = &std_ttystate;

	/* 
	 * TTY_LIT is part of cooked mode, so if it's set, leave it.
	 */
	if (tts->ttymode != TTY_LIT && tts->ttymode != TTY_COOKED)
		set_stdout(tts->fd, TTY_COOKED); /* CR/LF vs LF madness. hyc */
	if (tts->ttymode != TTY_LIT)
		tts->ttymode = TTY_COOKED;
	set_stdout(tts->fd, TTY_COOKED);
}

void
echo(tts)
	struct ttystate *tts;
{
	if (!tts)
		tts = &std_ttystate;
	tts->ttyecho = TTY_ECHO;
}

void
noecho(tts)
	struct ttystate *tts;
{
	if (!tts)
		tts = &std_ttystate;
	tts->ttyecho = TTY_NOECHO;
}
 
/*
 * Accept characters from the incoming tty buffer and process them
 * (if in cooked mode) or just pass them directly (if in raw mode).
 * Returns the number of characters available for use; if non-zero,
 * also stashes a pointer to the character(s) in the "buf" argument.
 *
 * This routine is called whenever a character is input.
 * When in cooked mode, returns zero until we've got a whole line
 * or there's some other reason to want to activate.
 *
 * This routine performs the mode switch to conversational mode.
 * If this becomes a problem, perhaps we could return a negative
 * number or some other special code.
 */
 /*Control-R added by df for retype of lines - useful in Telnet */
 /*Then df got impatient and added Control-W for erasing words  */
 /* Control-V for the literal-next function, slightly improved
  * flow control, local echo stuff -- hyc */
int
ttydriv(tts,c,buf)
struct ttystate *tts;
unsigned char c;
unsigned char **buf;
{
	unsigned char *rp;
	int cnt;
	int seenprint;

	if (!tts)
		tts = &std_ttystate;

#define cp      tts->cp
#define ttymode tts->ttymode
#define ttyecho tts->ttyecho
#define linebuf tts->linebuf
#define f	tts->file

	if(buf == (char **)NULL)
		return 0;	/* paranoia check */

	cnt = 0;
	if (tts->confirm) {
		if (c == tts->confirm)
			return -1;
		tts->confirm = 0;
		return 0;
	}
	switch(ttymode){
	
	/* TTY_LIT means we've seen a ^V in cooked mode. */
	case TTY_LIT:
		ttymode = TTY_COOKED;	/* Reset to cooked mode */
		*cp++ = c;
		if(cp >= linebuf+LINESIZE){
		      cnt = cp - linebuf;
		      cp = linebuf;
		}
		break;

	/* TTY_ESC means we've seen the escape character in raw mode. */
	/* Raw mode is only used by telnet, so we can safely generate IAC's */
	case TTY_ESC:
		ttymode = TTY_RAW;
		if (c == escape)
		    *cp++ = c;
		else switch(c & 0x1f) {
/*BRK*/		    case '\002':  *cp++ = 255; *cp++ = 243; break; 
/*IP*/		    case '\003':  *cp++ = 255; *cp++ = 244; break;
/*AO*/		    case '\017':  *cp++ = 255; *cp++ = 245; break;
/*AYT*/		    case '\024':  *cp++ = 255; *cp++ = 246; break;
/*EC*/		    case '\010':  *cp++ = 255; *cp++ = 247; break;
/*EL*/		    case '\025':  *cp++ = 255; *cp++ = 248; break;
		    case '\030':
			if (tts == &std_ttystate) {
			  printf("\r\n"); 
			  cmdmode();
			} else {
			  fprintf(f,"\r\nClose connection? [y or n]\r\n");
			  tts->confirm = 'y';
			}
			break;
		    case '\037':
	       fprintf(f,"\r\n");
	       fprintf(f,"Type the escape character followed by\r\n");
	       fprintf(f,"  escape character - send a real escape character\r\n");
	       fprintf(f,"  x or ^x - return to command mode\r\n");
	       fprintf(f,"  b or ^b - send break\r\n");
	       fprintf(f,"  c or ^c - send interrupt process\r\n");
	       fprintf(f,"  o or ^o - abort output\r\n");
	       fprintf(f,"  t or ^t - are you there?\r\n");
	       fprintf(f,"  h or ^h - send telnet erase character\r\n");
	       fprintf(f,"  u or ^u - send telnet erase line\r\n");
	       fprintf(f,"  ?       - print this help message\r\n");
		             break;
		}
		cnt = cp - linebuf;
		cp = linebuf;
		break;
	case TTY_RAW:
		if (c == escape) {
		    ttymode = TTY_ESC;
		    break;
		}
		/* 
		 * More telnet-specific stuff.  There's some debate
		 * what to do with CR.  In theory a telnet end of
		 * line is CR LF, so Cisco turns CR into CR LF.  But
		 * experience shows that with full duplex systems
		 * CR 0 (which means a real CR character) is safer.
		 * Some Unix systems turn CR LF into LF.  Presumably
		 * on systems where CR doesn't make sense we'll be
		 * in half-duplex.  Our half-duplex code does use CR LF
		 */
		switch(c) {
		case '\r':  /* CR must be sent as CR 0 or CR LF */
		    *cp++ = c;
		    c = '\0';
		    break;
		case 0xff: /* IAC must be doubled */
		    *cp++ = c;
		    break;
		}
		*cp++ = c;
		cnt = cp - linebuf;
		cp = linebuf;
		break;
	case TTY_COOKED:
		/* Perform cooked-mode line editing */
		/* should really check for telnet */
		if (mode == CONV_MODE || tts != &std_ttystate) {
		  if (c == escape) {
		    if (tts == &std_ttystate) {
		      fprintf(f,"\r\n"); cmdmode(); cp = linebuf; goto endline;
		    } else {
		      fprintf(f,"\r\nClose connection? [y or n]\r\n");
		      tts->confirm = 'y'; goto endline;
		    }
		  } else switch(c & 0x7f) {
		    case CTLV: ttymode = TTY_LIT; goto nochar;
/*BRK*/		    case '\002':  *cp++ = 255; *cp++ = 243; goto endline;
/*IP*/		    case '\003':  *cp++ = 255; *cp++ = 244; goto endline;
/*AO*/		    case '\017':  *cp++ = 255; *cp++ = 245; goto endline;
/*AYT*/		    case '\024':  *cp++ = 255; *cp++ = 246; goto endline;
		  }
		}
#ifdef PC9801
		switch(c){
#else
		switch(c & 0x7f){
#endif
		case '\r':	/* CR and LF are equivalent */
		case '\n':
			*cp++ = '\r';
			*cp++ = '\n';
			fprintf(f,"\n");
endline:
			cnt = cp - linebuf;
			cp = linebuf;
nochar:
			break;
		case RUBOUT:
		case '\b':		/* Backspace */
			if(cp != linebuf){
				cp--;
				if (ttyecho)
					fprintf(f,"\b \b");
			}
			break;
		case CTLR:	/* print line buffer */
			if(ttyecho)
				fprintf(f,"^R");
			fprintf(f,"\n");
			if(ttyecho) {
				rp = linebuf ;
				while (rp < cp)
					putc(*rp++,f) ;
			}
			break ;
		case CTLU:	/* Line kill */
			if(ttyecho) {
				while(cp != linebuf){
					cp--;
					fprintf(f,"\b \b");
				}
			} else
				cp = linebuf;
			break;
		case CTLV:
			ttymode = TTY_LIT;
			break;
		case CTLW:	/* erase word */
			seenprint = 0 ;	/* we haven't seen a printable char yet */
			while (cp != linebuf) {
				char lc;
				cp--;
				if(ttyecho)
					fprintf(f,"\b \b") ;
				lc = *cp;
				if (isspace(lc)) {
					if (seenprint)
						break ;
				}
				else
					seenprint = 1 ;
			}
			break ;
		default:	/* Ordinary character */
			*cp++ = c;
#ifndef	AMIGA
			/* ^Z apparently hangs the terminal emulators under
			 * DoubleDos and Desqview. I REALLY HATE having to patch
			 * around other people's bugslike this!!!
			 */
			if (ttyecho && (c != CTLZ))
				putc(c, f);
#endif
			if(cp >= linebuf+LINESIZE){
				cnt = cp - linebuf;
				cp = linebuf;
			}
			break;
		}
	}
	if(cnt > 0)
		*buf = linebuf;
	else
		*buf = NULLCHAR;
	/*
	 * This isn't flow control in XON/XOFF sense.  Rather, it's
	 * a feature that holds output while the user is typing a
	 * line.  So if we're in the middle of a line, disable output
	 */
#ifdef	FLOW
	if (tts == &std_ttystate) {
		if(cp > linebuf)
			ttyflow = 0;
		else
			ttyflow = 1;
	}
#endif
	fflush(f);
	return cnt;
}

#undef cp
#undef ttymode
#undef ttyecho
#undef linebuf
#undef f

struct ttystate *
open_remote_tty(name)
  char *name;
{
  struct ttystate *tts;
  struct termios ttybuf;
  FILE *fp;
  int fd;
  unsigned char *buf;

  tts = (struct ttystate *)malloc(sizeof(struct ttystate));
  if (! tts)
    return NULL;

  buf = (unsigned char *)malloc(LINESIZE);
  if (! buf) {
    free (tts);
    return NULL;
  }

  fp = fopen(name, "r+");
  if (! fp) {
    free(tts);
    free(buf);
    return NULL;
  }

  fd = fileno(fp);

  tts->fd = fd;
  tts->file = fp;
  tts->confirm = 0;
  tts->ttymode = TTY_COOKED;
  tts->ttyecho = TTY_ECHO;
  tts->linebuf = buf;
  tts->cp = buf;

  ioctl(fd, TCGETS, &ttybuf);
  tts->savecon = ttybuf;

  ttybuf.c_iflag &= ~(ICRNL);
  ttybuf.c_lflag &= ~(ICANON|ISIG|ECHO);
  ttybuf.c_cc[VTIME] = '\01';
  ttybuf.c_cc[VMIN] = '\0';
  if ((tts->savettyfl = fcntl(fd, F_GETFL, 0)) == -1) {
    perror("Could not read console flags");
    return NULL;
  }

/* #ifdef  undef */
  fcntl(fd, F_SETFL, tts->savettyfl | O_NDELAY);
/* #endif */

  ioctl(fd, TCSETSW, &ttybuf);
  return tts;
}

close_remote_tty(tts)
 struct ttystate *tts;
{
  int fd;

  fd = tts->fd;

  ioctl(fd, TCSETSW, &tts->savecon);
  fcntl(fd, F_SETFL, tts->savettyfl);

  fclose(tts->file);

  free(tts->linebuf);
  free(tts);
}

remote_tty_fd(tts)
 struct ttystate *tts;
{
  return(tts->fd);
}

remote_tty_iflag(tts)
 struct ttystate *tts;
{
  return(tts->savecon.c_iflag);
}
