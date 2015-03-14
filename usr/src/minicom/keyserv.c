/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * Keyserv.c	- a process that translates keypresses to
 *		  ANSI, VT100 or MINIX escape sequences.
 *
 * 		  Communications with this process from minicom
 *		  goes through pipes, file descriptors 3 & 4.
 */

#include <sys/types.h>
#include <signal.h>
#include <string.h>
#ifdef _POSIX_SOURCE
#  include <unistd.h>
#endif
#include <setjmp.h>
#undef NULL
#include <stdio.h>

#include "window.h"
#include "keyserv.h"
#include "minicom.h"

/* Emulation modes */
#define VT100	1
#define MINIX	2
#define ANSI	3

/* Pipe file descriptors */
#define from_minicom 3
#define to_minicom   4

/* Keypad modes */
#define NORMAL 1
#define APPL 2

/* Set modes to normal */
int keypadmode = NORMAL;
int cursormode = NORMAL;

char *_tptr = CNULL;		/* Pointer to termcap information */
int mode = VT100;		/* Emulation mode selector */
int parent;			/* Process ID of minicom */
jmp_buf mainloop;		/* To jump to after a 'HELLO' signal */
char **escseq;			/* Translation table to use */
static int argument;		/* Argument to 'HELLO' command */
static int esc_char = 1;	/* Escape character (initially ^A) */
static int bs_code = 8;		/* Code that backspace key sends */
extern int pendingkeys;		/* From wkeys.c */

char *vtesc[] = {
  "", "\033OP", "\033OQ", "\033OR", "\033OS", "", "", "", "", "", "",
  "\033[H", "", "\033[A", "\033[D", "\033[C", "\033[B", "\033[K", "",
  "", "\177" };

char *minixesc[] = {
  "", "\033OP", "\033OQ", "\033OR", "\033OS", "\033OT", "\033OU", "\033OV",
  "\033OW", "\033OX", "\033OY", "\033[H", "\033[V", "\033[A", "\033[D",
  "\033[C", "\033[B", "\033[Y", "\033[U", "0", "\177" };

char *ansiesc[] = {
  "", "\033OP", "\033OQ", "\033OR", "\033OS", "\033OT", "\033OU", "\033OV",
  "\033OW", "\033OX", "\033OY", "\033[H", "\033[V", "\033[A", "\033[D",
  "\033[C", "\033[B", "\033[Y", "\033[U", "0", "\177" };

/*
 * We got a signal. This means that there is some information for us.
 * Read it, and jump to the main loop.
 */
/*ARGSUSED*/
void handler(dummy)
int dummy;
{
  unsigned char buf[8];
  int n;

  signal(HELLO, handler);
  n = read(from_minicom, buf, 8);
  if (n <= 0) {
  	n = 2;
  	buf[0] = 0;
  }
  if (n % 2 == 1) {
	n++;
	read(from_minicom, buf + n, 1);
  }
  argument = buf[n-1];
  longjmp(mainloop, (int)buf[n - 2]);
}

/*
 * Send a string to the modem
 */
void sendstr(s)
char *s;
{
  write(1, s, strlen(s));
}

/*
 * Main program of keyserv.
 */
int main(argc, argv)
int argc;
char **argv;
{
  int c;
  char ch;
  int f, fun;
  int stopped = 0;

#ifdef _COHERENT
  parent = atoi(argv[1]);
#else
  parent = getppid();
#endif
  
  /* Initialize signal handlers */
  signal(SIGHUP, SIG_IGN);
#ifdef SIGTSTP
  signal(SIGTSTP, SIG_IGN);
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN);
#endif
  signal(HELLO, handler);

  /* Set up escape sequence table */
  escseq = vtesc;

  /* Cbreak, no echo (minicom itself sets to raw if needed) */
  (void) setcbreak(1);

  if ((fun = setjmp(mainloop)) != 0) {

  	switch(fun) {

  	/* We come here after minicom has told us something */

  	case KVT100: /* VT100 keyboard */
		mode = VT100;
		escseq = vtesc;
		break;
	case KANSI:  /* ANSI keyboard */
		mode = ANSI;
		escseq = ansiesc;
		break;
	case KMINIX: /* MINIX keyboard */
		mode = MINIX;
		escseq = minixesc;
		break;
	case KKPST: /* Keypad in standard mode, not used */
		keypadmode = NORMAL; 
		break;
	case KKPAPP: /* Keypad in applications mode, not used */
		keypadmode = APPL;
		break;
	case KCURST: /* Standard cursor keys */
		cursormode = NORMAL;
		if (mode == VT100)
			for(f = K_UP; f <= K_DN; f++)
				vtesc[f-256][1] = '[';
		break;
	case KCURAPP: /* cursor keys in applications mode */
		cursormode = APPL;
		if (mode == VT100)
			for(f = K_UP; f <= K_DN; f++)
				vtesc[f-256][1] = 'O';
		break;
	case KSTOP:  /* Sleep until further notice */
		stopped = 1;
		break;
	case KSIGIO: /* Wait for keypress and tell parent */
		kill(parent, ACK);
		f = read(0, &ch, 1);
		if (f == 1) {
			write(to_minicom, &ch, 1);
			(void) kill(parent, HELLO);
		}
		break;
	case KSTART: /* Restart when stopped */
		stopped  = 0;
		break;
	case KSETBS: /* Set code that BS key sends */
		bs_code = argument;
		break;
	case KSETESC: /* Set escape character */
		esc_char = argument;
		break;
	default:
		break;
    }
    if (fun != KSIGIO) {
	kill(parent, ACK);
    }
  }
  /* Wait if stopped */
  if (stopped) pause();

  /* Main loop: read keyboard, send to modem */
  while(1) {
  	c = getch();
  	if (c > 256 && c < 256 + NUM_KEYS) {
  		sendstr(escseq[c - 256]);
  	}
  	if (c < 256) {
  		if (c == K_ERA) c = bs_code;
  		ch = c;
  		/* Test for escape characters */
  		if (c == esc_char || (esc_char == 128 && c > 128)) { 
			/* If we typed too fast, and the escape sequence
			 * was not that of a function key, the next key
			 * is already in the buffer.
			 */
			if (c == esc_char && pendingkeys > 0) {
				ch = getch();
			}
  			write(to_minicom, &ch, 1);
  			(void) kill(parent, HELLO);
  		} else {
  			write(1, &ch, 1);
  		}
  	}
  }
  /*NOTREACHED*/
  return(0);
}
