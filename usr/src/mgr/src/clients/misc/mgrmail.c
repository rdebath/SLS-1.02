/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: mgrmail.c,v 4.2 88/06/22 14:37:50 bianchi Exp $
	$Source: /tmp/mgrsrc/demo/misc/RCS/mgrmail.c,v $
*/
static char	RCSid_[] = "$Source: /tmp/mgrsrc/demo/misc/RCS/mgrmail.c,v $$Revision: 4.2 $";

/* check for new mail  (experimental version) */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <stdio.h>
#include "term.h"

#define MSG_1	"\fLooking for new mail"
#define MSG_2	"\f\007You have new mail"
#define MSG_3	"\freading mail ...\r"
#define MSG_4	"\rChecking for new mail..."
#define MSG_5	"\fMail window is active"
#define MSG_6	"\rYou don't have mail   "

#define MAILF		"/usr/spool/mail"	/* spool file */
#define MAIL		"mail"			/* name of mail command */
#define POLL		60			/* polling interval */
#define XPOS		240			/* x start of mail window */
#define YPOS		190			/* y start of mail window */
#define W_WIDE		650			/* width of mail window */
#define W_HIGH		394			/* height of mail window */

#define PROCESSED	2			/* new mail already processed */

#define S(x)			statb.x
#define Isflag(arg,flag)	(!strncmp(arg,flag,strlen(flag)))
#define Max(x,y)		((x)>(y)?(x):(y))
#define dprintf			if(debug) fprintf

#define MENU_COUNT		(sizeof(menu)/sizeof(struct menu_entry))

struct menu_entry menu[] = {
	"print","t\n",
	"delete","dt\n",
	"next","n\n",
	"quit","q\n",
	"help","?\n",
	"headers","h *\n",
	"abort","x\n",
};

struct	stat statb;			/* spool file status */
char	mail[255];			/* spool file path name */
long omtime=0l;				/* previous file mod. time */
int state = 0;				/* mail & window state */
int poll = POLL;			/* poll interval */
int debug=0;				/* for mgrmail -d >& /dev/tty?? */

main(argc,argv)
	char **argv;
{
	register int i;
	int xpos = XPOS;		/* screen position of mail subwindow */
	int ypos = YPOS;
	int font = -1;			/* font to use for mail subwindow */
	int shape = 1;			/* initially reshape window */
	char *command = MAIL;		/* name of readmail command */

	char *getenv();
	char *user = getenv("USER");
	char line[80];			/* event input buffer */

	int clean(), update();

	ckmgrterm( *argv );

	/* make sure environment is ok */
	if (user==NULL || *user=='\0') {
		fprintf(stderr,"%s: Who are you?\n",argv[0]);
		exit(2);
	}

	/* process arguments */

	for(i=1;i<argc;i++) {
		if (Isflag(argv[i],"-s"))
			shape = 0;
		else if (Isflag(argv[i],"-d"))
			debug = 1;
		else if (Isflag(argv[i],"-x"))
			xpos = atoi(argv[i]+2);
		else if (Isflag(argv[i],"-y"))
			ypos = atoi(argv[i]+2);
		else if (Isflag(argv[i],"-f"))
			font = atoi(argv[i]+2);
		else if (Isflag(argv[i],"-p"))
			poll  = Max(atoi(argv[i]+2),10);
		else if (Isflag(argv[i],"-M"))
			command  = argv[i]+2;
		else
			usage(argv[0],argv[i]);
	}
	sprintf(mail,"%s/%s",MAILF,user);

	/* set up window environment */

	m_setup(M_FLUSH);
	m_ttyset();
	m_push(P_MENU|P_EVENT|P_FLAGS);
	dprintf(stderr,"pushing environment\n"); fflush(stderr);
	m_setmode(M_NOWRAP);

	signal(SIGTERM,clean);
	signal(SIGINT,clean);
	signal(SIGALRM,update);

	m_setmode(M_ACTIVATE);
	if (shape) {
		m_size(strlen(MSG_1),1);
        }

	m_setevent(ACTIVATE,"A\n");
	m_setevent(REDRAW,"R\n");

	m_clearmode(M_ACTIVATE);
	m_clear();
	m_printstr(MSG_1);

	dprintf(stderr,"Starting state 0x%x\n",state); fflush(stderr);

	update();

	/* wait for an event */

	while(1) {
		m_gets(line);
		dprintf(stderr,"state 0x%x line : %c\n",state,*line); fflush(stderr);
		switch(*line) {
			case 'A':	/* window is activated */
				if (!stat(mail,&statb) && S(st_size))
					do_mail(command,font,xpos,ypos);
                                else {
	                                m_printstr(MSG_6);
                                        sleep(2);
                                        m_clearmode(M_ACTIVATE);
                                        }
				state &= ~PROCESSED;
				update();
				break;
			case 'R':	/* screen is redrawn */
				state &= ~PROCESSED;
				update();
				break;
		}
	}
}

/* run readmail in a subwindow */

do_mail(command,font,xpos,ypos)
char *command;
int font,xpos,ypos;
	{
	int code;
	int n;

	alarm(0);
	dprintf(stderr,"doing mail\n"); fflush(stderr);
	n=m_makewindow(xpos,ypos,W_WIDE,W_HIGH);
	if (n==0) {	/* can't make window */
		m_printstr("\007\fCan't open mail window, sorry");
		return(0);
		}
	m_clearevent(ACTIVATE);
	m_printstr(MSG_5);
	m_selectwin(n);
	menu_load(1,MENU_COUNT,menu);
	m_selectmenu(1);
	m_printstr(MSG_3);
	m_ttyreset();
	code = system(command);
	m_printstr(MSG_4);
	sleep(1);	/* for "New mail arrived" message */
	dprintf(stderr,"Readmail completed code %d\n",code); fflush(stderr);
	m_ttyset();
	m_destroywin(n);
	m_setevent(ACTIVATE,"A\n");
	m_clearmode(M_ACTIVATE);
	dprintf(stderr,"window deactivated\n"); fflush(stderr);
	}

/* check the spool file for new mail and update message */

int
update()
{
	alarm(0);
	dprintf(stderr,"checking mail state 0x%x\n",state); fflush(stderr);
	if (!stat(mail,&statb) && S(st_mtime)>S(st_atime) && S(st_size)) {
		state &= ~PROCESSED;
		if (S(st_mtime) != omtime) {
		dprintf(stderr,"	First time New mail\n"); fflush(stderr);
			m_printstr(MSG_2);
			m_setmode(M_WOB);
			omtime = S(st_mtime);
		}
	}
	else if (!(state&PROCESSED)) {
		dprintf(stderr,"	Clearing new mail\n"); fflush(stderr);
		m_clearmode(M_WOB);
		m_printstr(MSG_1);
		state |= PROCESSED;
	}
	alarm(poll);
}

/*	Clean up and exit */

clean()
{
	m_popall();
	m_ttyreset();
	exit(1);
}

usage(name,error)
char *name, *error;
{
	fprintf(stderr,"Invalid flag: %s\n",error);
	fprintf(stderr,
		"usage: %s -[s|x<pos>|y<pos>|f<font>|p<poll>|M<mail_program>]\n"
		,name);
	exit(1);
}
