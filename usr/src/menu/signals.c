# include <signal.h>
# include "menu.h"

/*	PSC MENU COPYRIGHT NOTICE

	Part of PSCMenu

	This software is to be considered to be public domain, it
may be copied, modified and parts of it may be used in other programs
as long as this copyright notice remains intact.

	Copyright()   PSC - Plymouth State College
	Written by:   Ted Wisniewski 12-9-1990
 
*/

/*
 *	void setup_sigs():
 *
 *	Parameters:	None.
 *
 *	Description:	Set up signal handling.
 *
 *	Returns:	None.
 *
 *	Last Modify:	05-8-91 (TW)
 *
 */

void setup_sigs()
{
	/* (void) signal(SIGHUP,catch); */
	/* (void) signal(SIGSEGV,catch); */
	/* (void) signal(SIGBUS,catch); */
	(void) signal(SIGINT,SIG_IGN);
	(void) signal(SIGTSTP,SIG_IGN);
	(void) signal(SIGQUIT,SIG_IGN);
}

/*
 *	catch():
 *
 *	Parameters:	signo - The signal ID #.
 *
 *	Description:	Routine to catch specific signals.
 *
 *	Returns:	None.
 *
 *	Last Modify:	05-8-91 (TW)
 *
 */

int catch(signo)
int signo;
{
	switch (signo){
	   case SIGHUP:
		exit(1);
	   break;
	   case SIGSEGV:
	   /* case SIGBUS: */
		clr_scr();  
	        reset_tty();
	        end_rev();
		perror(" ");
		exit(1);
	   break;
	}
}

/*
 * 	log_out():
 *
 *	Parameters:	None.
 *
 *	Description:	Kill The process group of the parent to the menu,
 *			hopefully this will be the users login shell.
 *
 *	Returns:	None.
 *
 *	Last Modify:	09-04-91 (TW)
 *
 */

void log_out()
{
# ifndef HPUX
	killpg(getppid(),SIGHUP);	/* Kill Parent Process		*/
	killpg(getpid(),SIGHUP);	/* Suicide: Kill self.		*/
# else
	kill(getppid(),SIGHUP);		/* Kill Parent Process		*/
	kill(getpid(),SIGHUP);		/* Suicide: Kill self.		*/
# endif HPUX
}
