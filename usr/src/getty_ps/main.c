/*
**	main.c
**
**	$Id: main.c, v 2.1 93/04/01 17:00:00 kris Rel $
**
**	main body of program
*/

#define MAIN
#include "main.h"

#ifdef UUGETTY
#include "uufuncs.h"
#endif /* UUGETTY */

#if defined(RCSID) && !defined(lint)
#include "release.h"
static char *RcsId = "@$Id: main.c, v 2.1 93/04/01 17:00:00 kris Rel $";
static char *Release = RELEASE;
static char *RelDate = DATE;
#endif /* RCSID && !lint */

/* forward function declarations
*/

sig_t	timeout();
sig_t	schedalarm();
sig_t	rbalarm();
sig_t	shangup();
int	tputc();
void	exit_usage();
void	initvalues();
void	defs();
void	initline();
void	opentty();
void	dologin();

#ifdef DEBUG
void	debugstart();
#endif /* DEBUG */


/* trivial globals
*/

char	buf[MAXLINE+1];
FILE	*fp;
int	fd;
char	tbuf[64];



/*
** main
*/

void main(argc, argv)
int 	argc;
char 	**argv;
{
	initvalues();		/* initialize all runtime variables */
	defs(argc, argv);	/* parse command line and defaults file */

#ifdef UUGETTY
	waitlocks();		/* hold on 'til the lockfiles are gone */
#endif /* UUGETTY */

	initline();		/* initialize the line */

#ifdef UUGETTY
	watchlocks();		/* monitor the lockfiles */
#endif /* UUGETTY */

	opentty();		/* open & initialize the tty */
	dologin();		/* get username and start login */
}


/* 
** initvalues
**
** initialize all runtime stuff to default values
*/

void initvalues()
{
	(void) close(0);	/* get rid of any old stdin, out, err */
	(void) close(1);
	(void) close(2);

	(void) signal(SIGHUP, SIG_IGN);
	(void) vhangup();
	(void) signal(SIGHUP, shangup);

	(void) setsid();

	(void) signal(SIGINT,  SIG_IGN);	/* ignore ^C */
	(void) signal(SIGQUIT, SIG_DFL);
	(void) signal(SIGTERM, SIG_DFL);

	strcpy(term, 	"unknown");		/* tty type */
	Device =	"unknown";		/* tty device */
	InitDevice =	Device;			/* device to init */
	LineD =		(char *) NULL;		/* lined */
	AutoBaud = 	FALSE;			/* no autobauding */
	AutoRate[0] = 	'\0';
	Check = 	FALSE;			/* ! check files and exit */
	CheckFile =	(char *) NULL;
	GtabId =	(char *) NULL;
	NoHangUp =	FALSE;			/* hangup the line first */
	TimeOut =	0;			/* no timeout */
	delay = 	0;			/* delay before prompt */
	speed =		(char *) NULL;		
	clear =		TRUE;			/* clear the screen */
	login =		LOGIN;			/* login program */
	waitchar =	FALSE;			/* don't wait for a char */
	waitfor =	(char *) NULL;		/* no waitfor string */
	connect =	(char *) NULL;		/* no connect string */
	defname =	(char *) NULL;		/* no defaults file */

#ifdef ISSUE
	issue =		ISSUE;			/* login banner */
#endif /* ISSUE */

#ifdef FIDO
	fido =		(char *) NULL;		/* fido program */
#endif /* FIDO */

#ifdef SCHED
	allow =		TRUE;			/* no scheduling */
#endif /* SCHED */

#ifdef DEBUG
	Debug =		0;			/* no debugging */
	Dfp =		NULL;			/* no debugging file */
#endif /* DEBUG */

#ifdef WARNCASE
	WarnCase =	TRUE;			/* moan about all caps */
#endif /* WARNCASE */

#ifdef RBGETTY
	minrbtime = 	6;			/* min time to call back */
	maxrbtime =	60;			/* max time to call back */
	interring =	4;			/* time between rings */
	minrings = 	1;			/* min rings to set off rb */
	maxrings =	3;			/* max rings to set off rb */
	rbmode = 	FALSE;			/* off by default */
#endif /* RBGETTY */

#ifdef UUGETTY
	MyName =	"uugetty";		/* hello... my name is */
#else
	MyName =	"getty";
#endif /* UUGETTY */
}


#ifdef SCHED
/*
** SetSched
**
** parse the SCHED information
*/

void setsched(p)
char	*p;
{
	time_t		t_cur, t_base, t_begin, t_end;
	int		s_dow, s_hr, s_min, e_dow, e_hr, e_min, count;

/* set up the time base
*/
	(void) time(&t_cur);
	t_base = t_cur - ((t_cur + 4 * 86400) % 604800);

/* parse the sched line
*/
	count = 0;
	allow = FALSE;
	while(sscanf(nextword(p, &count), "%d:%d:%d-%d:%d:%d",
	  &s_dow, &s_hr, &s_min, &e_dow, &e_hr, &e_min) == 6) {
		p += count;
		t_begin = t_base + (s_dow % 7) * 86400 + s_hr * 3600
		  + s_min * 60;
		t_end = t_base + (e_dow % 7) * 86400 + e_hr * 3600
		  + e_min * 60;

/* hadle week overlaps
*/

		if(t_end < t_begin) {
			if(t_cur < t_end)
				t_begin -= 604800;
			else
				t_end += 604800;
		}

/* set alarm based on times
*/
		if((t_begin > t_cur) && 
		  ((t_begin < alrm) || (alrm == 0))) alrm = t_begin ;
		if((t_end > t_cur) && 
		  ((t_end < alrm) || (alrm == 0))) alrm = t_end ;

/* set allow based on this
*/
		if ((t_begin <= t_cur) && (t_cur < t_end)) allow = TRUE;
	}
	alrm -= t_cur;

	debug3(D_SCH, "Alarm set to: %d\n", alrm);
	debug3(D_SCH, "Allow: %s\n", ((allow) ? "TRUE" : "FALSE"));
}
#endif /* SCHED */


/*
** defs
**
** parse the command line and the defaults file
*/

void defs(count, args)
int	count;
char	**args;
{
	register int	c;
	DEF		**def;
	char		*p;
	char		termcap[1024];

/* first, the command line
*/

	while((c = getopt(count, args, "RC:D:ac:d:hr:t:w:")) != EOF) {
		switch(c) {
#ifdef RBGETTY
			case 'R':
				rbmode = TRUE;
				break;
#endif /* RBGETTY */
			case 'C':
				connect = optarg;
				break;
			case 'D':
#ifdef DEBUG
				(void) sscanf(optarg, "%o", &Debug);
#else
				logerr("DEBUG not compiled in");
#endif /* DEBUG */
				break;
			case 'a':
				InitDevice = optarg;
				break;
			case 'c':
				Check = TRUE;
				CheckFile = optarg;
				break;
			case 'd':
				defname = optarg;
				break;
			case 'h':
				NoHangUp = TRUE;
				break;
			case 'r':
				waitchar = TRUE;
				delay = (unsigned) atoi(optarg);
				break;
			case 'w':
				waitchar = TRUE;
				waitfor = optarg;
				break;
			case '?':
				exit_usage(2);
		}
	}

	
/* if we're just checking, exit here
*/

	if(Check) {
		(void) signal(SIGINT, SIG_DFL);
		(void) gtabvalue((char *) NULL, G_CHECK);
		exit(0);
	}


/* get line, speed, tty type, lined
*/

	if(optind < count)
		Device = args[optind++];
	else {
		logerr("no line given");
		exit_usage(2);
	}
	if(optind < count) GtabId = args[optind++]; 
	if(optind < count) strncpy(term, args[optind++], sizeof(term));
	if(optind < count) LineD = args[optind++];


/* now, get all that info in the defaults file
*/

	def = defbuild(defname);
#ifdef DEBUG
	if((p = defvalue(def, "DEBUG"))) (void) sscanf(p, "%o", &Debug);
	if(Debug) debugstart();
#endif /* DEBUG */
	if(! (SysName = defvalue(def, "SYSTEM"))) SysName = getuname();
	if((Version = defvalue(def, "VERSION")) && (*Version == '/')) {
		if((fp = fopen(Version, "r"))) {
			(void) fgets(buf, sizeof(buf), fp);
			(void) fclose(fp);
			buf[strlen(buf)-1] = '\0';
			Version = strdup(buf);
		}
	}
	if((p = defvalue(def, "LOGIN"))) login = p;
	if((p = defvalue(def, "ISSUE"))) issue = p;
	if((p = defvalue(def, "CLEAR")) && (strequal(p, "NO"))) 
		clear = FALSE;
	if((p = defvalue(def, "HANGUP")) && (strequal(p, "NO"))) 
		NoHangUp = TRUE;
	if((p = defvalue(def, "WAITCHAR")) && (strequal(p, "YES")))
		waitchar = TRUE;
	if((p = defvalue(def, "DELAY"))) delay = (unsigned) atoi(p);
	if((p = defvalue(def, "TIMEOUT"))) TimeOut = atoi(p);
	if((p = defvalue(def, "CONNECT"))) connect = p;
	if((p = defvalue(def, "WAITFOR"))) {
		waitchar = TRUE;
		waitfor = p;
	}
	if((p = defvalue(def, "INIT"))) init = p;  
	if((p = defvalue(def, "INITLINE"))) InitDevice = p;
#ifdef FIDO
	fido = defvalue(def, "FIDO");
#endif /* FIDO */

#ifdef SCHED
	if((p = defvalue(def, "SCHED"))) {
		setsched(p);

		if((! allow)) init = defvalue(def, "OFF");
	}
#endif /* SCHED */

#ifdef RBGETTY
	if((p = defvalue(def, "MINRBTIME"))) minrbtime = atoi(p);
	if((p = defvalue(def, "MAXRBTIME"))) maxrbtime = atoi(p);
	if((p = defvalue(def, "INTERRING"))) interring = atoi(p);
	if((p = defvalue(def, "MINRINGS"))) minrings = atoi(p);
	if((p = defvalue(def, "MAXRINGS"))) maxrings = atoi(p);
	if((p = defvalue(def, "RINGBACK")) && (strequal(p, "YES"))) rbmode = TRUE;
#endif /* RBGETTY */

/* find out how on earth to clear the screen
*/

	if(! strequal(term, "unknown")) {
		p = tbuf;
		if((tgetent(termcap, term) == 1)
		  && (! (clrscr = tgetstr("cl", &p)))) clrscr = "";
	}

/* construct /dev/ names for the lines
*/
	(void) sprintf(devname, "/dev/%s", Device);
	if(strequal(InitDevice, "unknown")) InitDevice = Device;
	(void) sprintf(initdevname, "/dev/%s", InitDevice);

#ifdef UUGETTY
	(void) sprintf(buf, LOCK, Device);
	lock = strdup(buf);
	if((p = defvalue(def, "ALTLOCK"))) {
		(void) sprintf(buf, LOCK, p);
		altlock = strdup(buf);
	} else if(! strequal(Device, InitDevice)) {
		(void) sprintf(buf, LOCK, InitDevice);
		altlock = strdup(buf);
	}

	debug3(D_LOCK, "lock = (%s)\n", lock);
	debug3(D_LOCK, "altlock = (%s)\n", altlock);
#endif /* UUGETTY */
}


#ifdef DEBUG
/*
** debugstart
**
** open up the debug file
*/

void debugstart()
{
	time_t	clock;

	(void) sprintf(buf, "/tmp/%s:%s", MyName, Device);
	if(! (Dfp = fopen(buf, "a+"))) {
		logerr("cannot open debug file");
		exit(FAIL);
	} 
	if (fileno(Dfp) < 3) {
		if((fd = fcntl(fileno(Dfp), F_DUPFD, 3)) > 2) {
			(void) fclose(Dfp);
			Dfp = fdopen(fd, "a+");
		}
	}
	(void) time(&clock);
	debug4(0777, "--------\n%s Started: %s", MyName, ctime(&clock));
}
#endif /* DEBUG */



#ifdef LOGUTMP
/*
** doutmp
**
** update the utmp and wtmp files
*/

void doutmp()
{
	int 		pid;
	time_t 		clock;
	struct utmp	*utmp;

	debug2(D_RUN, "update utmp/wtmp files\n");

	pid = getpid();

#ifdef SIMPLEINIT
	if((utmp = malloc(sizeof(struct utmp))))
#else
	while((utmp = getutent()))
		if((utmp->ut_type == INIT_PROCESS) && (utmp->ut_pid == pid))
#endif /* SIMPLEINIT */
		{
			strncopy(utmp->ut_line, Device);
#ifdef SIMPLEINIT
			strncopy(utmp->ut_id, Device+3);
#endif /* SIMPLEINIT */
			utmp->ut_host[0] = '\0';
			utmp->ut_addr = 0;
			strncopy(utmp->ut_user, "LOGIN");
			utmp->ut_pid = pid;
			utmp->ut_type = LOGIN_PROCESS;
			(void) time(&clock);
			utmp->ut_time = clock;
			pututline(utmp);

			if((fp = fopen(WTMP_FILE, "a"))) {
				(void) fseek(fp, 0L, 2);
				(void) fwrite((char *)utmp, sizeof(*utmp), 
				  1, fp);
				(void) fclose(fp);
		}
	}
	endutent();
}
#endif /* LOGUTMP */


/*
** initline
**
** initialize the line, do waitchar, waitfor
*/

void initline()
{
	struct stat	st;
	TERMIO		termio;
	char		ch;
	int		flags;
	char *		wait = waitfor;

/* set the line owned by root
*/

	(void) chmod(devname, 0666);
	if (! stat(devname, &st)) (void) chown(devname, 0, st.st_gid);


/* update utmp/wtmp
*/

#ifdef LOGUTMP
	doutmp();
#endif /* LOGUTMP */



/* now, the init device is opened ONLY if INIT or WAITCHAR is requested
*/

	if((init) || (waitchar)) {
		debug3(D_RUN, "opening init line: %s\n", initdevname);
		while(((fd = open(initdevname, O_RDWR | O_NDELAY)) < 0) &&
		  (errno == EBUSY)) sleep(30);
		if(fd < 0) {
			logerr("cannot open init line");
			exit(FAIL);
		}
		if(fd != 0) {
			logerr("cannot open init stdin");
			exit(FAIL);
		}
		if(dup(0) != 1) {
			logerr("cannot open init stdout");
			exit(FAIL);
		}
		if(dup(0) != 2) {
			logerr("cannot open init stderr");
			exit(FAIL);
		}

		setbuf(stdin, (char *) NULL);
		setbuf(stdout, (char *) NULL);
		setbuf(stderr, (char *) NULL);

		gtab = gtabvalue(GtabId, G_FORCE);
		if(! NoHangUp) {
			(void) ioctl(STDIN, TCGETS, &termio);
			termio.c_cflag &= ~CBAUD;
			termio.c_cflag |= B0;
			(void) ioctl(STDIN, TCSETSF, &termio);
			sleep(2);
		}
		settermio(&(gtab->itermio), INITIAL);

		flags = fcntl(STDIN, F_GETFL, 0);
		(void) fcntl(STDIN, F_SETFL, flags & ~O_NDELAY);

/* init the line
*/

		if(init) {
			debug2(D_RUN, "initializing line\n");

/* set CLOCAL for init, so ATZ works right
*/
			(void) ioctl(STDIN, TCGETS, &termio);
			termio.c_cflag |= CLOCAL;
			(void) ioctl(STDIN, TCSETSF, &termio);

			if(chat(init) == FAIL) {
				debug2(D_RUN, "init failed... aborting\n");
				logerr("warning: INIT sequence failed");
				exit(FAIL);
			}

			settermio(&(gtab->itermio), INITIAL);
		}
	}

/* set the alarm if requested
*/

#ifdef SCHED
	if(alrm) {
		debug2(D_SCH, "SCHED alarm set\n");
		(void) signal(SIGALRM, schedalarm);
		(void) alarm(alrm);
	}
#endif /* SCHED */

/* wait for a char
*/
#ifdef SCHED
	if(allow)
#endif /* SCHED */
	  if(waitchar) {
		debug2(D_RUN, "waiting for a character...\n");

		(void) ioctl(STDIN, TCFLSH, 0);
		(void) read(STDIN, &ch, 1);		/* blocks */
		debug2(D_RUN, "got it\n");

#ifdef UUGETTY
/* check the lockfiles
*/
		if(checklock(lock)) exit(0);
		if((altlock) && checklock(altlock)) exit(0);
#endif /* UUGETTY */

		if(wait) {
			if(ch == *wait) wait++;
			if((*wait) && (expect(wait) == FAIL)) {
				debug2(D_RUN, "WAITFOR match failed\n");
				exit(0);
			}
		}
	}

	if((init) || (waitchar)) {
		(void) close(0);
		(void) close(1);
		(void) close(2);
	}
}


#ifdef RBGETTY
/*
** dorb
**
** watch the pattern of incoming rings (defined by the WAITFOR
** string) to do a ringback connect
**
** contributed by: Shane Alderton (shanea@extra.ucc.su.oz.au)
*/

void dorb()
{
	time_t	lasttime = 0;
	time_t	currenttime = 0;
	time_t	elapsed;
	int	ringcount = 1;
	boolean success = FALSE;
	char *	wait;
	char	ch;

	while(! success) {
		(void) time(&lasttime);
		(void) signal(SIGALRM, rbalarm);
		(void) alarm(maxrbtime);

		debug2(D_RUN, "waiting for another ring... \n");
		(void) ioctl(STDIN, TCFLSH, 0);
		(void) read(STDIN, &ch, 1);		/* this blocks */
		debug2(D_RUN, "got one\n");

		(void) alarm(0);
		(void) signal(SIGALRM, SIG_DFL);
		wait = waitfor;
		if(wait) {
			if(ch == *wait) wait++;
			if((*wait) && (expect(wait) == FAIL)) {
				debug2(D_RUN, "RINGBACK: WAITFOR match failed\n");
				exit(0);
			}
		}
		
		(void) time(&currenttime);
		elapsed = currenttime - lasttime;

		if(elapsed <= interring) {
			ringcount++;
			debug3(D_RUN, "got ring number %d.\n", ringcount);
		} else if (elapsed < minrbtime) {
			ringcount = 1;
			debug2(D_RUN, "ring is out of bounds... treating as new call\n");
		} else if ((ringcount < minrings) || (ringcount > maxrings)) {
			debug3(D_RUN, "%d rings... Out of bounds.  Treating as new call\n", 
				ringcount);
			ringcount = 1;
		} else { 
			debug2(D_RUN, "got ringback...");
			success = TRUE;
		}
	}
}
#endif /* RBGETTY */
		


/*
** opentty
**
** open the tty device.
** the tty is opened in blocking mode, unless WAITFOR is specified
*/

void opentty()
{
	int	flags, i, cbaud, nspeed;
	char	ch;
	GTAB	*gt;
#ifdef UUGETTY	
	long	status;
#endif /* UUGETTY */

	
#ifdef SCHED
/* handle allow
*/

	debug2(D_SCH, (allow ? "not sleeping\n" : "sleeping\n"));
	while(! allow) pause();
#endif /* SCHED */

	debug3(D_RUN, "opening line: %s\n", devname);

	if((fd = open(devname, (waitfor) ? 
	  (O_RDWR | O_NDELAY) : (O_RDWR))) < 0) {
		logerr("cannot open connect line");
		exit(FAIL);
	}
	if(fd != 0) {
		logerr("cannot open connect stdin");
		exit(FAIL);
	}
	if(dup(0) != 1) {
		logerr("cannot open connect stdout");
		exit(FAIL);
	}
	if(dup(0) != 2) {
		logerr("cannot open connect stderr");
		exit(FAIL);
	}

#ifdef UUGETTY
/* stop looking for lockfiles now
*/

	if(! (waitfor)) {
		kill(chpid, SIGHUP);
		wait(&status);
	}
	lockline();
#endif /* UUGETTY */

	setpgrp();

	setbuf(stdin, (char *) NULL);
	setbuf(stdout, (char *) NULL);
	setbuf(stderr, (char *) NULL);

	flags = fcntl(STDIN, F_GETFL, 0);
	(void) fcntl(STDIN, F_SETFL, flags & ~O_NDELAY);

	gtab = gtabvalue(GtabId, G_FORCE);
	settermio(&(gtab->itermio), INITIAL);

	(void) signal(SIGALRM, SIG_DFL);
	(void) alarm(0);

	if(delay) {
		debug3(D_RUN, "delay(%d)\n", delay);

		(void) sleep(delay);
		(void) fcntl(STDIN, F_SETFL, flags | O_NDELAY);
		while (read(STDIN, &ch, 1) == 1);
		(void) fcntl(STDIN, F_SETFL, flags & ~O_NDELAY);
	}

#ifdef RBGETTY
	if(rbmode) dorb();
#endif /* RBGETTY */

	if(connect) {
		debug2(D_RUN, "performing connect sequence\n");

		cbaud = 0;
		if(strequal(connect, "DEFAULT")) connect = DEF_CONNECT;
		if((chat(connect)) == FAIL) {
			logerr("warning: CONNECT sequence failed");
			debug2(D_RUN, "connect sequence failed... aborting\n");
			exit(FAIL);
		}
		if(AutoBaud) {
			debug3(D_RUN, "AutoRate = (%s)\n", AutoRate);
#ifdef TELEBIT
			if(strequal(AutoRate, "FAST")) 
				(void) strcpy(AutoRate, TB_FAST);
#endif /* TELEBIT */
			if((nspeed = atoi(AutoRate)) > 0)
				for (i=0; speedtab[i].nspeed; i++)
					if(nspeed == speedtab[i].nspeed) {
						cbaud = speedtab[i].cbaud;
						speed = speedtab[i].speed;
						break;
					}
		}
		if(cbaud) {
			debug3(D_RUN, "setting speed to %s\n", speed);
			if((gt = gtabvalue(speed, G_FIND)) && 
			  strequal(gt->cur_id, speed)) 
				gtab = gt;
			else {
				gtab->itermio.c_cflag = 
				  (gtab->itermio.c_cflag & ~CBAUD) | cbaud;
				gtab->ftermio.c_cflag = 
				  (gtab->ftermio.c_cflag & ~CBAUD) | cbaud;
			}
			settermio(&(gtab->itermio), INITIAL);
		}
	}
}
				

/*
** dologin
**
** print the login banner, get the user's name, and exec login
*/

void dologin()
{
	struct utmp	*utmp;
	int		cbaud, i;
	TERMIO		termio;
	

	for(;;) {
		Nusers = 0;
		setutent();
		while((utmp = getutent())) 
			if(utmp->ut_type == USER_PROCESS) Nusers++;
		endutent();

		cbaud = gtab->itermio.c_cflag & CBAUD;
		for(i=0; speedtab[i].cbaud != cbaud; i++);
		Speed = speedtab[i].speed;

#ifdef ISSUE
		if (clear && *clrscr) {
			(void) tputs(clrscr, 1, tputc);
		}
		fputc('\r', stdout);

		if(*issue != '/') {
			(void) Fputs(issue, stdout);
			(void) fputs("\r\n", stdout);
		} else if((fp = fopen(issue, "r"))) {
			while(fgets(buf, sizeof(buf), fp))
			  (void) Fputs(buf, stdout);
			(void) fclose(fp);
		}
#endif /* ISSUE */

login_prompt:
		(void) Fputs(gtab->login, stdout);
		(void) ioctl(STDIN, TCFLSH, 0);
		if(TimeOut > 0) {
			(void) signal(SIGALRM, timeout);
			(void) alarm((unsigned) TimeOut);
		}

		switch(getlogname(&termio, buf, MAXLINE)) {
#ifdef FIDO
			case FIDOCALL:
				login = fido;
				logerr("Fido Call Detected");
#endif /* FIDO */
			case SUCCESS:
				if(TimeOut > 0) {
					(void) alarm((unsigned) 0);
					(void) signal(SIGALRM, SIG_DFL);
				}

				termio.c_iflag |= gtab->ftermio.c_iflag;
				termio.c_oflag |= gtab->ftermio.c_oflag;
				termio.c_cflag |= gtab->ftermio.c_cflag;
				termio.c_lflag |= gtab->ftermio.c_lflag;
				termio.c_line |= gtab->ftermio.c_line;
				settermio(&termio, FINAL);
#ifdef SETTERM
				setenv("TERM", term, TRUE);
#endif /* SETTERM */
				(void) execl(login, 
				  "login", buf, (char *)NULL);
				(void) execl("/bin/sh", "sh", "-c", login,
				  buf, (char *) NULL);
				(void) sprintf(MsgBuf, "cannot execute %s", 
				  login);
				logerr(MsgBuf);
				exit(FAIL);

			case BADSPEED:
				GtabId = gtab->next_id;
				gtab = gtabvalue(GtabId, G_FORCE);
				settermio(&(gtab->itermio), INITIAL);
				break;

#ifdef WARNCASE
			case BADCASE:
				for(i=0; bad_case[i] != (char *) NULL; i++)
					(void) fputs(bad_case[i], stdout);
				goto login_prompt;
#endif /* WARNCASE */

			case NONAME:
				break;
		}
	}
}


/*
** exit_usage
**
** moan about bad command line options
*/

void exit_usage(code)
int code;
{
	char	msg[512] = "";
	
	(void) sprintf(msg, USAGE, MyName);
	logerr(msg);
	exit(code);
}


/*
** timeout
**
** the user types way too damn slow
*/

sig_t timeout()
{
	TERMIO termio;

	(void) sprintf(MsgBuf, "\nTimed out after %d seconds.\n", TimeOut);
	(void) Fputs(MsgBuf, stdout);

	(void) ioctl(STDIN, TCGETS, &termio);
	termio.c_cflag &= ~CBAUD;
	termio.c_cflag |= B0;
	(void) ioctl(STDIN, TCSETAF, &termio);
	sleep(5);

	exit(1);
}


/*
** tputc
**
** just because
*/

int tputc(c)
char c;
{
	return(fputc(c, stdout));
}


#ifdef SCHED
/*
** schedalarm
**
** signal handler for SCHED
*/

sig_t schedalarm()
{
	debug2(D_SCH, "SCHED alarm caught\n");
	exit(0);
}
#endif /* SCHED */


#ifdef RBGETTY
/*
** rbalarm
**
** signal handler for RINGBACK
*/

sig_t rbalarm()
{
	debug2(D_RUN, "RINGBACK alarm caught... waited too long for ringback\n");
	exit(0);
}
#endif /* RBGETTY */



/* 
** shangup
**
** signal handler for a hangup
*/

sig_t shangup()
{
	debug2(D_RUN, "Caught HANGUP signal\n");
	exit(0);
}
