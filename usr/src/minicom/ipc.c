
/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * ipc.c - talk to the keyserv process.
 * 
 * Entry points:
 *
 * keyserv(command, arg)
 *	command can be KINSTALL, KUNINSTALL, or a command for keyserv.
 *	arg is a 1 byte argument.
 *
 * use 'setjmp(ksigbuf)' and 'keyserv(KSIGIO)' to get keypresses
 * asynchronously.
 *
 * setbskey()  - tell keyserv about new BS key
 * setesckey() - tell keyserv about new ESC key.
 */
#include <sys/types.h>
#ifdef _POSIX_SOURCE
#  include <unistd.h>
#  include <stdlib.h>
#endif
#undef NULL
#include <signal.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <setjmp.h>
#include <errno.h>

#include "window.h"
#include "minicom.h"
#include "keyserv.h"
#include "configsym.h"

static int tokeyserv, fromkeyserv;	/* File desciptors of ipc pipes */
static int keypid;			/* pid of keyserv  		*/
static jmp_buf ackjmp;			/* To jump to after ACK signal  */
static int waiting = 0;			/* Do we expect an ACK signal?	*/

/*
 * We got the "ksigio" signal. This means that CTRL-A was pressed in
 * the main terminal loop, or any key if requested by keyserv(KSIGIO).
 */
/*ARGSUSED*/
static void ksigio(dummy)
int dummy;
{
  unsigned char c;

  signal(HELLO, ksigio);
  while (read(fromkeyserv, &c, 1) < 0 && errno == EINTR)
  	;
  longjmp(ksigbuf, c);
}

/*
 * After we have sent a signal to "keyserv", we wait for it to signal
 * us back. Otherwise "keyserv" would be swamped with signals and
 * die ungracefully....
 */
static void sigack(dummy)
int dummy;
{
  signal(ACK, sigack);
  if (waiting) longjmp(ackjmp, 1);
  printf("sigack: unexpected ACK signal &^%%$!! (pardon my French)\r\n");
}

/*
 * Install the keyserv process. This involves setting up the communications
 * channels (pipes) and forking the child process.
 */
static void kinstall()
{
#ifdef _COHERENT
  char mpid[8];
#endif
  int pipe1[2], pipe2[2];
  char buf[2];

  if (pipe(pipe1) < 0 || pipe(pipe2) < 0)
  	leave("minicom: out of file descriptors\n");
  tokeyserv = pipe1[1];
  fromkeyserv = pipe2[0];

  /* Set signal handler */
  signal(HELLO, ksigio);
  signal(ACK, sigack);

#ifdef _COHERENT
  sprintf(mpid, "%d", getpid());
#endif

  switch(keypid = fork()) {
  	case -1:
  		leave("minicom: could not fork.\n");
  		break;
  	case 0: /* Child */

  		/* Set up fd #1 : stdout */
  		dup2(portfd, 1);
  		close(portfd);

		/* Set up fd #3 : minicom ---> keyserv */
		dup2(pipe1[0], 3);

		/* Set up fd #4 : minicom <--- keyserv */
		dup2(pipe2[1], 4);

		/* Close unused file descriptors */
		close(pipe1[1]);
		close(pipe2[0]);
#ifdef _COHERENT
  		execl(KEYSERV, "keyserv", mpid, (char *)NULL);
#else
  		execl(KEYSERV, "keyserv", (char *)NULL);
#endif
  		exit(0);
  	default: /* Parent */
  		sleep(2); /* Wait for keyserv to initialize */
		if (setjmp(ackjmp) == 0) {
			waiting = 1;
  			buf[0] = KSTOP;
  			write(tokeyserv, buf, 2);
  			if (kill(keypid, HELLO) < 0) {
  				leave("minicom: could not exec keyserv\n");
  			}
			/* Do nothing 'till ACK signal */
			while(1) pause();
		}
		waiting = 0;
		/* close unused pipes */
		close(pipe1[0]);
		close(pipe2[1]);
		break;
  }
}


/*
 * Install / tell /de-install "keyserv" program.
 */
void keyserv(cmd, arg)
int cmd, arg;
{
  char ch[2];
  int pid, stt;
  static int lastcmd = -1;

  if (lastcmd == KSTOP) m_flush(0);
  lastcmd = cmd;

  if (cmd == KINSTALL) {
  	kinstall();
  	return;
  }

  if (cmd == KUNINSTALL) {
	(void) kill(keypid, SIGKILL);
	pid = m_wait(&stt);
	return;
  }

  if (setjmp(ackjmp) == 0) {
	waiting = 1;
	ch[0] = cmd;
	ch[1] = arg;
	write(tokeyserv, ch, 2);
	(void) kill(keypid, HELLO);
	/* Do nothing 'till ACK signal */
	while(1) pause();
  }
  waiting = 0;
}

/* Tell keyserv about the backspace key */
void setbskey()
{
  int k = 8;

  if (P_BACKSPACE[0] == 'D') k = 0177;
  keyserv(KSETBS, k);
}

/* Tell keyserv about the escape / command key */
void setesckey()
{
  int k = 128;

  if (P_ESCAPE[0] == '^') k = P_ESCAPE[1] & 0x1f;
  keyserv(KSETESC, k);
}

