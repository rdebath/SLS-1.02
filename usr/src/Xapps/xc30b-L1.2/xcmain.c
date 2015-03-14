/*	xcmain.c -- main module for XC
	This file uses 4-character tabstops
*/

#include <sys/types.h>
#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include <termio.h>
#ifdef T6000
#include <sys/ioctl.h>
#endif
#include <setjmp.h>
#include "xc.h"


static char version[]="@(#)JPR XC 3.0bL |     KI4N Lpatch 1.2 -- 22 Apr 92 ";

#define Resume_Not_Allowed	1
#define PAUZ  1

void pauz(void);        /* used to help clean up screen after error messages */

jmp_buf erret;			/* non-local error return */

short
	autoflag = FALSE,	/* Flag for automatic capturing */
	badline = FALSE,	/* Bad telephone line? */
	cismode = FALSE,	/* Respond to CIS "ENQ" */
	cr_add = TRUE,		/* Add carriage returns for B+ uploads */
	hdplxflag = FALSE,	/* Flag for half-duplex mode */
        dosmode = TRUE,         /* Flag for DOS character translation */
	nlmode = TRUE,		/* Map newlines to carriage returns */
	reterm = FALSE,		/* No jumping into terminal mode */
	stat_flag = FALSE,	/* Flag for status display */
	eof_flag = FALSE,	/* Flag to quit a script */
	first_sete = TRUE,  /* Flag for pauz() so as not to pause during startup */
    nowait = FALSE,     /* When TRUE this will disable call waiting */
    tone = TRUE;        /* Flag for pulse or touch tone dialing */
 
int s_cis(), s_set(), s_exit(), s_shell();

char Msg[SM_BUFF];

FILE *tfp;

struct termio newmode, oldmode, sigmode;

static char	*statfmt = "\t\t%-8s %25s %s\n",
			oldshell[SM_BUFF];

static s_script(), s_xmodem(), s_term(), s_help(), s_dial(), puttake(),
		SET_7bit(), SET_cr(), SET_cis(), SET_nl(), SET_purge(), SET_xoff(),
                SET_dos(),         
		SET_halfdplx(), SET_bps(), SET_autocapt(), SET_cfile(), SET_pfile();

extern short scriptflag;
extern void B_Transfer(), dbglog(), mattach(), terminal(), xreceive(), xsend(),
			get_ttype();

static char	*babble[] = {
	"Usage: xc [-l device] [-t] [-g file]",
	"",
	"-l device  Use 'device' as the modem port (eg, -l /dev/tty64)",
	"-t         Enter terminal mode immediately",
	"-s file    Execute script file 'file' immediately",
    "-p         Use pulse dialing instead of tone dialing",
    "-w         Disable call waiting",
	"",
	NULLS };

static void usage()
{
	char **ptr;

	for (ptr = babble; *ptr != NULLS; ptr++)
		fprintf(tfp, "%s\n", *ptr);
}

struct kw {					/* Used by command parsing routines */
	char *keyword;
	int (*rtn)();
};

static struct kw cmds[] = { /* Command table */
	{"c",		s_cis},
	{"s",		s_script},
	{"h",		hangup},
	{"ro",      s_xmodem},
	{"rx",		s_xmodem},
	{"rt",		s_xmodem},
	{"so",      s_xmodem},
	{"sx",		s_xmodem},
	{"st",		s_xmodem},
	{"set",		s_set},
	{"t",		s_term},
	{"d",		s_dial},
	{"q",		s_exit},
	{"x",		s_exit},
	{"exit",	s_exit},
	{"!",		s_shell},
	{"!!",		s_shell},
	{"$",		s_shell},
	{"%p",		puttake},
	{"%t",		puttake},
	{"help",	s_help},
	{"?",		s_help},
	{NULLS,		0}
};

/* Catch a signal and jump to main. Reset signal and do a longjmp */
static void catch()
{
	show(2,"XC: Interrupt");

	signal(SIGINT,catch);
	signal(SIGQUIT,catch);
	longjmp(erret,1);
}

static struct kw setlist[] = {
	{"7bit",	SET_7bit},
	{"auto",	SET_autocapt},
	{"bps",		SET_bps},
	{"cfile",	SET_cfile},
	{"cis",		SET_cis},
	{"cr",		SET_cr},
        {"dos_mode",    SET_dos},
	{"halfdplx",    SET_halfdplx},
	{"nl",		SET_nl},
	{"pfile",	SET_pfile},
	{"purge",	SET_purge},
	{"xoff",	SET_xoff},
	{NULLS,		0}
};

/* Print the status of the program */
static void status()
{
	struct kw *ptr;
	char p[30];
	int (*fct)() = 0;

	stat_flag = TRUE;

	cls();
	cur_off();
	sprintf(p,"Modem Port: %s",mport(NULLS));
	drawline(0, 0, CO);
	ttgoto(1, 1);
	sprintf(Msg,"%-45s%25s",&version[4], p);
	S;
	drawline(2, 0, CO);
	ttgoto(3, 0);
	fprintf(tfp, statfmt, "Keyword", "Description", "Status");
	fprintf(tfp, statfmt, "--------", "-------------------------", "-----------");

	for (ptr = setlist; ptr->keyword != NULLS; ptr++)
		if (ptr->rtn != fct) {
			fct = ptr->rtn;
			(*fct)();
		}

	ttgoto(18, 25);
	show(1,"Type \"help\" or ? for help");
	stat_flag = FALSE;
	cur_on();
}

main(argc, argv)
int argc;
char *argv[];
{
	char *script = NULLS;
	extern char *optarg;
	int c;
	extern int optind;

	struct kw *ptr;
	tfp = stderr;
	if (isatty(0))
		get_ttype();

	ioctl(0, TCGETA, &oldmode);	/* get current tty mode	*/

	/* trap for SIGHUP and SIGTERM, make sure LCKfile gets killed */
	signal(SIGHUP,s_exit);
	signal(SIGTERM,s_exit);

	newmode = oldmode;

	newmode.c_oflag &= (OPOST | ONLCR);
	newmode.c_iflag &= ~(IXON | IXOFF);
	newmode.c_lflag &= ~(ICANON | ISIG | ECHO);
	newmode.c_cc[VMIN] = 1;
	newmode.c_cc[VTIME] = 1;

	sigmode = newmode;
	sigmode.c_lflag |= ISIG;

	oldshell[0] = '\0';	/* set last command to blank */
	if (setjmp(erret))	/* set error handler to exit */
		exit(0);		/* while parsing command line */
	signal(SIGINT,catch);	/* catch break & quit signals/keys */
	signal(SIGQUIT,catch);

	while ((c = getopt(argc, argv, "s:l:pwt")) != -1)
		switch (c) {
		case 'l':	/* set modem port name */
			mport(optarg);
			break;
		case 's':	/* Execute SCRIPT file */
			script = optarg;
			break;
		case 't':	/* jump into terminal mode */
			reterm = TRUE;
			break;
        case 'p':   /* use pulse dialing  */
            tone = FALSE;
            break;       
        case 'w':  /* disable call waiting  */
            nowait = TRUE ; 
            break; 
		default:	/* Bad command .. print help */
			usage();
			exit(1);
		}

	setuid((int)geteuid());
	setgid((int)getegid());

	mopen();	/* opens and configures modem port, or exits */

	setuid((int)getuid());
	setgid((int)getgid());

	intdel(FALSE);
	linkflag=2;
	do_script(STARTUP);
    first_sete = FALSE ;

#ifdef DEBUG
	dbglog();
#endif

	if (!script)
		status();

	for ( ; ; ) {
		setjmp(erret);
		signal(SIGQUIT,s_exit);
		intdel(FALSE);

		if (script)
			do_script(script),
			script = NULLS,
			reterm = TRUE;

		if (reterm) {
			s_term();
			continue;
		}

		intdel(TRUE);
		fputc('\n',tfp);
		show(-1,"<XC>");
		fputc(' ',tfp);

		lptr = line;
		getline();
		intdel(FALSE);
		fputc('\n',tfp);

		getword();
		lc_word(word);
		if (word[0] == '\0'){		/* If blank line... reprompt */
			status();	
  			continue;
			}

		for (ptr = cmds; ptr->keyword != NULLS; ptr++)
			if (strcmp(word, ptr->keyword) == 0)
				break;

		if (ptr->keyword)
			(*ptr->rtn)();
		else {
			sprintf(Msg,"Unrecognized command: %s",word),
			S;
			pauz();
			}	
	}
}

static s_script()
{
	getword();

	if (word[0] == '\0') {
		show(1,"Script file not specified");
		return;
	}

	sprintf(ddsname,"%s",word);
	do_script(ddsname);
	reterm = TRUE;
}

static s_xmodem()
{
	char d = word[0];
	char c = word[1];

	getword();
	if (word[0] == '\0')
		show(1,"Transfer file not specified");
	else if (d == 's')
		xsend(c);
	else
		xreceive(c);

	reterm = TRUE;
}

static s_term()
{
	for ( ; ; ) {
		terminal(FALSE);
		if (cismode != 2)
			return;
		cismode = 1;
		s_cis();
	}
}

static s_dial()
{
	for ( ; ; ) {
		terminal(TRUE);
		if (cismode != 2)
			return;
		cismode = 1;
		s_cis();
	}
}

s_cis()
{
	B_Transfer();
	reterm = TRUE;
}

s_shell()
{
	void (*oldvec)();
	int stat_loc = 0;
	char c = word[0];
	static char *shell = NULLS;

#ifdef NOSHELL
	return(0);
#endif
	if (word[0] == word[1])
		strcpy(wptr = word, oldshell);
	else {
		getword();
		if (*wptr)
			strcpy(oldshell, wptr);
	}

	if (!shell) {
		shell = getenv("SHELL");
		if (!shell)
			shell = "/bin/sh";
	}

	fputc('\n',tfp);
	ioctl(0, TCSETAW, &oldmode);

	if ((forkem()) == 0) {
		if (c == '$')	/* Attach modem to stdin, stdout */
			mattach();
		signal(SIGCLD,SIG_DFL);
		signal(SIGINT,SIG_DFL);
		signal(SIGQUIT,SIG_DFL);
		if (word[0] == '\0')
			execl(shell, shell, "-i", NULLS);
		else
			execl(shell, shell, "-c", wptr, NULLS);
		show(1,"Exec failed!");
		exit(2);
	}

	oldvec = signal(SIGINT,SIG_IGN);
	wait(&stat_loc);
	signal(SIGINT,oldvec);

	intdel(FALSE);
	strcpy(oldshell, wptr);
	return(!!stat_loc);
}

static char	*cmdlist[] = {
	"\tXC Command Summary\n\n",
	"\tc\t\tInitiate CIS B+ File Transfer (Upload and Download)\n",
        "\tro file\t\tXMODEM-1K receive (old YMODEM)\n",
	"\trx file\t\tXMODEM receive (128 byte packets)\n",
	"\trt file\t\tXMODEM receive (CP/M Ascii mode)\n",
        "\tso file ...\tXMODEM-1k send (old YMODEM)\n", 
	"\tsx file ...\tXMODEM send (128 byte packets)\n",
	"\tst file ...\tXMODEM send (CP/M Ascii mode)\n",
	"\tset ...\t\tXC Parameter set\n\n",
	"\ts file\t\tExecute XC script file\n",
	"\tt\t\tEnter terminal mode\n",
	"\td\t\tDialing directory\n",
	"\th\t\tHang up the modem\n",
	"\tq, x or exit\tExit XC\n\n",
#ifndef NOSHELL
	"\t!\t\tExecute a local interactive shell\n",
	"\t! cmd\t\tExecute shell command string on the local system\n",
	"\t!!\t\tRe-execute the last shell command string\n",
	"\t$ cmd\t\tShell command with stdin and stdout redirected to modem\n",
#endif	
	"\t%p loc [rem]\tPut local file to a UNIX system\n",
	"\t%t rem [loc]\tTake remote file from a UNIX system\n",
	"\t? or help\tPrint this help screen\n",
	"",
	"\tSET Keywords:\n\n",
	"\tset\t\t\tDisplay current XC status\n",
	"\tset 7bit on|off\t\tSet/Reset 7-bit data mask (ignore high bits)\n",
	"\tset auto on|off\t\tSet/Reset automatic capturing\n",
	"\tset bps <value>\t\tSet Bits/Second to <value>\n",
	"\tset cfile name\t\tChange name of capture file\n",
	"\tset cis on|off\t\tSet/Reset CIS <ENQ> mode (Auto up/download)\n",
	"\tset cr on|off\t\tSet/Reset Carriage Return Injection mode\n",
        "\tset dos_mode on|off\tSet/Reset DOS Character Translation\n",  
	"\tset halfdplx on|off\tSet/reset half-duplex terminal mode\n",
	"\tset nl on|off\t\tSet/Reset newline translation\n",
	"\tset pfile name\t\tChange name of phonelist file\n",
	"\tset purge on|off\tSet/Reset bad phone line mode\n",
	"\tset xoff on|off\t\tSet/Reset XON/XOFF flow control\n",
	"",
	"\tESCAPE sequences in TERMINAL mode:\n\n",
	"\t",ESC_STR," b\tSend modem break\n",
	"\t",ESC_STR," d\tDialing directory\n",
	"\t",ESC_STR," f\tSend a file through the modem (ASCII transfer)\n",
	"\t",ESC_STR," s\tExecute XC script file\n",
	"\t",ESC_STR," h\tHang up the modem\n",
	"\t",ESC_STR," t\tToggle capture file on and off\n",
	"\t",ESC_STR," x\tExit terminal mode back to XC command mode\n",
	"\t",ESC_STR," q\tQuit XC entirely\n",
	"",
	NULLS };

static s_help()
{
	char **ptr = cmdlist;

	ioctl(0, TCSETAW, &oldmode);
	cls();
	cur_off();
	for ( ; *ptr != NULLS; ptr++)
		if (**ptr)
			fprintf(tfp, "%s", *ptr);
		else
			show(0,"PRESS ENTER"),
			getline(),
			cls();
	status();
}

s_set()
{
	struct kw *ptr;

	getword();

	if (word[0] == '\0' && !scriptflag) {
		status();
		return;
	} else if (word[0] == '\0') {
		show(1,"SET keyword requires an argument");
		eof_flag++;
		return;
	}

	lc_word(word);

	for (ptr = setlist; ptr->keyword != NULLS; ptr++)
		if (strcmp(ptr->keyword, word) == 0) {
			(*ptr->rtn)();
			return;
		}

	sprintf(Msg,"Invalid SET keyword: %s", word);
	S;
    pauz();
	eof_flag++;
}

set_onoff(flag)
short *flag;
{
	char *ptr = strdup(word);

	getword();
	lc_word(word);

	if (strcmp(word, "on") == 0)
		*flag = TRUE;
	else if (strcmp(word, "off") == 0)
		*flag = FALSE;
	else{
		sprintf(Msg,"Set '%s' value must be 'on' or 'off'",ptr),
		S,
    	eof_flag++;}

	free(ptr);
}

static SET_7bit()
{
	short val ;

	if (stat_flag) {
		fprintf(tfp, statfmt, "7bit", "Seven-bit Mask",
			bitmask == 0x7f ? "ON" : "OFF");
		return;
	}

	set_onoff(&val);
	bitmask = val ? 0x7f : 0xff;
        if (bitmask == 0x7f ) 
            dosmode = FALSE ;

	if (!scriptflag){
		sprintf(Msg,"%s-bit mask enabled", val ? "Seven" : "Eight"),
		S;
		pauz();
		}
}

static SET_cr()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "cr", "Carriage Return Injection",
			cr_add ? "ON" : "OFF");
		return;
	}

	set_onoff(&cr_add);

	if (!scriptflag){
		sprintf(Msg,"Carriage Returns %s injected in B+ ASCII uploads",
			cr_add ? "ARE" : "are NOT"),
		S;
		pauz(); 
		}
}

static SET_nl()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "nl", "Newline Translation",
			nlmode ? "ON" : "OFF");
		return;
	}

	set_onoff(&nlmode);

	if (!scriptflag){
		sprintf(Msg,"Newlines %s changed to Carriage Returns",
			nlmode ? "ARE" : "are NOT"),
		S;
		pauz();
		}
}

static SET_cis()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "cis", "CIS <ENQ> Auto Download",
			cismode ? "ON" : "OFF");
		return;
	}

	set_onoff(&cismode);

	if (!scriptflag){
		sprintf(Msg,"CIS <ENQ> Auto Download is %s", cismode ? "ON" : "OFF"),
		S;
		pauz();
		}
}

static SET_dos()
{
        extern int bitmask ;

        if (stat_flag) {
                fprintf(tfp, statfmt, "dos_mode", "Character Translation",
                        dosmode ? "ON" : "OFF" );
                return ;    
                }                    

        bitmask = 0xff ;
        set_onoff(&dosmode);
        if (!scriptflag) {
                sprintf(Msg,"DOS translation is %s", dosmode ? "ON" : "OFF" ),
                S;
                pauz();
                }
        
}

static SET_purge()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "purge", "Bad Telephone line",
			badline ? "ON" : "OFF");
		return;
	}

	set_onoff(&badline);

	if (!scriptflag){
		sprintf(Msg,"Bad telephone line purging is %s", badline ? "ON" : "OFF"),
		S;
		pauz();
        }       
}

static SET_xoff()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "xoff", "Terminal mode XON/XOFF",
			flowflag ? "ON" : "OFF");
		return;
	}

	set_onoff(&flowflag);
	xc_setflow(flowflag);

	if (!scriptflag){
		sprintf(Msg,"XOFF/XON Flow control is %s", flowflag ? "ON" : "OFF"),
		S;
		pauz();
		}
}

static SET_bps()
{
	if (stat_flag) {
		char br[10];
		sprintf(br, "%d", 10*mrate(NULLS));
		fprintf(tfp, statfmt, "bps", "Bits per Second", br);
		return;
	}

	getword();
	if (word[0] == '\0')
		show(1,"Set Bps must have a rate");
	else if (mrate(word) < 0)
		sprintf(Msg,"Unsupported bps rate %s",word),
		S;
	eof_flag++;
	if (!scriptflag){
		sprintf(Msg,"Bits/Second set to %d",10*mrate(NULLS)),
		S;
		pauz();
		}
}

static SET_halfdplx()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "halfdplx", "Half-duplex mode",
			hdplxflag ? "ON" : "OFF");
		return;
	}

	set_onoff(&hdplxflag);

	if (!scriptflag){
		sprintf(Msg,"Half-duplex Mode is %s", hdplxflag ? "ON" : "OFF"),
		S;
		pauz();
		}
}

static SET_autocapt()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "auto", "Auto capture toggle",
			autoflag ? "ON" : "OFF");
		return;
	}

	set_onoff(&autoflag);

	if (!scriptflag){
		sprintf(Msg,"Auto Capture Toggling is %s", autoflag ? "ON" : "OFF"),
		S;
		pauz();
		}
}

static SET_cfile()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "cfile", "Capture file", captfile);
		return;
	}

	getword();
	if (word[0] == '\0') {
		show(1,"Set CFILE must have file name");
		eof_flag++;
		return;
	}

	strcpy(captfile, word);

	if (!scriptflag){
		sprintf(Msg,"Capture file set to '%s'",captfile),
		S;
		pauz();
		}
}

static SET_pfile()
{
	if (stat_flag) {
		fprintf(tfp, statfmt, "pfile", "Phone number file", phonefile);
		return;
	}

	getword();
	if (word[0] == '\0') {
		show(1,"Set PFILE must have file name");
		eof_flag++;
		return;
	}

	strcpy(phonefile, word);

	if (!scriptflag){
		sprintf(Msg,"Phone number file set to '%s'",phonefile),
		S;
		pauz();
		}
}

/*	Put and Take a file to/from a UNIX-type "cu" system. Unfortunately,
	the stty command is one of those commands that always gets changed
	with different UNIX systems, so you will get (at least) a file full of
	^M on the take command for systems later than V7 or work-alikes.

	Additionally, the Take command takes a bit too much!

	Fixed a lot of this: JPRadley 89/07/27
*/

static puttake()
{
	FILE * fp;
	int i, Ch;
	char c = word[1], fname[SM_BUFF], tname[SM_BUFF], wrkbuff[SM_BUFF];

	getword();

	intdel(TRUE);
	signal(SIGINT,catch);
	signal(SIGQUIT,catch);
	xc_setflow(TRUE);
	if (word[0] == '\0') {
		sprintf(Msg,"Must give a filename with the '%%%c' option",c);
		S;
		return;
	}

	strcpy(fname, word);
	getword();
	if (word[0] == '\0')
		strcpy(tname, fname);
	else
		strcpy(tname, word);
	switch (c) {
	case 'p':
		if ((fp = fopen(fname, "r")) == NULLF) {
			sprintf(Msg,"Can't access '%s'",fname);
			S;
			break;
		}

		fprintf(tfp, "\nPutting file '%s' to '%s' on remote UNIX\n",
			fname, tname);
		sprintf(wrkbuff, "stty -echo;cat >%s;sleep 1;stty echo\n", tname);
		send_slowly(wrkbuff);		/* send command string to remote shell */
		i = 64;
		while ((Ch = getc(fp)) != EOF) {
			if (++i > 64) { /* this prevents an overload on the */
				i = 0;	 /* receiver's input buffer (64=kludge) */
				msecs((CBAUD-cbaud) * 100);
			}
			send_mbyte(Ch);		/* send characters to cat command */
		}
		fclose(fp);
		send_mbyte('D'-'@');	/* send a ^D to cat */
		purge();				/* get rid of whatever was sent back */
		send_mbyte('\n');
		break;

	case 't':
		strcpy(Name, tname);
		if ((fp=QueryCreate(Resume_Not_Allowed)) == NULLF)
			break;

		fprintf(tfp, "\nTaking file '%s' from remote UNIX to '%s'\n",
			fname, tname);
									/* if 'fname' has a ^E, we'll die */
		sprintf(wrkbuff, "stty nl;cat %s;echo %c;stty -nl;sleep 1\n",
			fname, ENQ);
		send_slowly(wrkbuff);		/* send command string to remote shell */
		while (read_mbyte(3) != '\n'); /* discard up to the \n in wrkbuff */
			;
		while ((Ch=read_mbyte(3)) != -1 && /* while more chars are being sent */
					Ch != ENQ)			   /* and we haven't seen our ENQ */
			fputc(Ch,fp);
		fclose(fp);
		break;
	}
	intdel(FALSE);
	xc_setflow(flowflag);
	reterm = TRUE;
}

s_exit()
{
	signal(SIGHUP,SIG_IGN);
	signal(SIGINT,SIG_IGN);
	signal(SIGQUIT,SIG_IGN);
	signal(SIGTERM,SIG_IGN);

	putc('\n',tfp);

	unlock_tty();

	ioctl(0, TCSETAF, &oldmode);

	exit(0);
}

void pauz(void) 
{
	if(first_sete)
		return;
	sleep(1);
    status();
}





