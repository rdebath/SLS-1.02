/* exp_main_sub.c - miscellaneous subroutines for Expect or Tk main() */

#include <stdio.h>
#include <sys/ioctl.h>
#include "tcl.h"
#include "tclInt.h"
#include "exp_rename.h"
#include "exp_main.h"
#include "exp_global.h"
#include "exp_command.h"
#include "exp_tty.h"
#include "exp_log.h"
#include "exp_event.h"
#include "exp_main.h"

#ifdef __SABER__
#undef	VERSION
#define	VERSION		"Saber"
#undef	SCRIPTDIR
#define SCRIPTDIR	"test/"
#endif
static char exp_version[] = VERSION;
#define VERSION_VARNAME   "expect_version"
#define NEED_TCL_MAJOR		6
#define NEED_TCL_MINOR		1
#define SCRIPTDIR_VARNAME "expect_library"

char *exp_argv0 = "this program";	/* default program name */
void (*exp_app_exit)() = 0;
FILE *exp_cmdfile = 0;
int exp_cmdlinecmds = FALSE;
int exp_interactive =  FALSE;

static Tcl_Interp *exp_interp;	/* for emergency use only */

static void
usage(interp)
Tcl_Interp *interp;
{
	errorlog(stderr,"usage: expect [-di] [-c cmds] [[-f] cmdfile] [args]\r\n");
	exp_exit(interp,-1);
}

void
exp_exit(interp,status)
Tcl_Interp *interp;
int status;
{
	extern int forked;
	static int exp_exit_in_progress = FALSE;

	/* prevent recursion */
	if (exp_exit_in_progress) {
		errorlog("exp_exit recursed\r\n");
		exit(status);
	}
	exp_exit_in_progress = TRUE;

	/* called user-defined exit routine if one exists */
	exp_generic_sighandler(0);

	if (!interp) {
		/* if no interp handy (i.e., called from interrupt handler) */
		/* use last one created - it's a hack but we're exiting */
		/* ungracefully to begin with */
		interp = exp_interp;
	}
	if (exp_app_exit) (*exp_app_exit)(interp);
	if (exp_event_exit) (*exp_event_exit)(interp);

	Tcl_DeleteInterp(interp);

	if (!exp_disconnected && !forked
	    && (dev_tty != -1) && isatty(dev_tty) && ioctled_devtty) {
		tty_set(interp,&exp_tty_original,dev_tty,0);
	}
	/* all other files either don't need to be flushed or will be
	   implicitly closed at exit.  Spawned processes are free to continue
	   running, however most will shutdown after seeing EOF on stdin.
	   Some systems also deliver SIGHUP and other sigs to idle processes
	   which will blow them away if not prepared.
	*/
#ifdef CRAY
	ttyp_reset();
#endif
	exit(status);
}


/* this stupidity because Tcl needs commands in writable space */
static char prompt1[] = "prompt1";
static char prompt2[] = "prompt2";

static char *prompt2_default = "+> ";
static char prompt1_default[] = "expect%d.%d> ";

/*ARGSUSED*/
int
cmdPrompt1(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	Interp *iPtr = (Interp *)interp;

	sprintf(interp->result,prompt1_default,
		iPtr->numLevels,iPtr->curEventNum+1);			
	return(TCL_OK);
}

/*ARGSUSED*/
int
cmdPrompt2(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	strcpy(interp->result,prompt2_default);
	return(TCL_OK);
}

/* user has pressed escape char from interact or somehow requested expect.
If a user-supplied command returns:

TCL_ERROR,	assume user is experimenting and reprompt
TCL_OK,		ditto
TCL_RETURN,	return TCL_OK (assume user just wants to escape() to return)
TCL_RETURN_TCL,	return TCL_RETURN
anything else	return it
*/
int
exp_interpreter(interp)
Tcl_Interp *interp;
{
	int rc;
	char *ccmd;		/* pointer to complete command */
	char line[BUFSIZ];	/* space for partial command */
	int newcmd = TRUE;
	Tcl_CmdBuf buffer;
	Interp *iPtr = (Interp *)interp;
	int tty_changed = FALSE;

	exp_tty tty_old;
	int was_raw, was_echo;

	int dummy;
	int fd = fileno(stdin);

	int key = expect_key++;

	if (!(buffer = Tcl_CreateCmdBuf())) {
		/* why are we doing this, this way rather than, say, */
		/* AppendResult? */
		strcpy(interp->result,"no more space for cmd buffer\r\n");
		return(TCL_ERROR);
	}
	newcmd = TRUE;
	while (TRUE) {
		/* force terminal state */
		tty_changed = tty_cooked_echo(interp,&tty_old,&was_raw,&was_echo);

		if (newcmd) {
			rc = Tcl_Eval(interp,prompt1,0,(char **)0);
			if (rc == TCL_OK) Log(1,"%s",interp->result);
			else Log(1,prompt1_default,iPtr->numLevels,
						   iPtr->curEventNum+1);
		} else {
			rc = Tcl_Eval(interp,prompt2,0,(char **)0);
			if (rc == TCL_OK) Log(1,"%s",interp->result);
			else Log(1,prompt2_default,1);
		}

/* exp_get_next_event isn't necessary for non-Tk apps, but doesn't hurt */
		rc = exp_get_next_event(interp,&fd,1,&dummy,EXP_TIME_INFINITY,key);
		/*  check for rc == EXP_TCLERROR? */
		
		if (rc != EXP_EOF) {
			if (fgets(line,BUFSIZ,stdin) == NULL) {
				if (!newcmd) line[0] = 0;
				else rc = EXP_EOF;
			}
		}

		if (rc == EXP_EOF) exp_exit(interp,0);

		if (debugfile) fwrite(line,1,strlen(line),debugfile);
		/* intentionally always write to logfile */
		if (logfile) fwrite(line,1,strlen(line),logfile);
		/* no need to write to stdout, since they will see it */
		/* just from it having been echoed as they are typing it */

		if (NULL == (ccmd = Tcl_AssembleCmd(buffer,line))) {
			newcmd = FALSE;
			continue;	/* continue collecting command */
		}
		newcmd = TRUE;

		if (tty_changed) tty_set(interp,&tty_old,was_raw,was_echo);
		switch (rc = Tcl_RecordAndEval(interp,ccmd,0)) {
		case TCL_OK:
			if (*interp->result != 0)
				Log(0,"%s\r\n",exp_cook(interp->result,(int *)0));
			continue;
		case TCL_ERROR:
			errorlog("%s\r\n",exp_cook(get_var("errorInfo"),(int *)0));
			/* since user is typing by hand, we expect lots
			   of errors, and want to give another chance */
			continue;
#define finish(x)	{rc = x; goto done;}
		case TCL_BREAK:
		case TCL_CONTINUE:
			finish(rc);
		case TCL_RETURN_TCL:
			finish(TCL_RETURN);
		case TCL_RETURN:
			finish(TCL_OK);
		default:
			/* note that ccmd has trailing newline */
			errorlog("error %d: %s\r\n",rc,ccmd);
			continue;
		}
	}
	/* cannot fall thru here, must jump to label */
 done:
	if (tty_changed) tty_set(interp,&tty_old,was_raw,was_echo);

	/* currently, code guarantees buffer is valid */
	Tcl_DeleteCmdBuf(buffer);

	return(rc);
}

/*ARGSUSED*/
int
cmdExpectVersion(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int emajor, umajor;
	char *user_version;	/* user-supplied version string */

	if (argc == 1) {
		Tcl_SetResult(interp,exp_version,TCL_STATIC);
		return(TCL_OK);
	}
	if (argc > 3) {
		exp_error(interp,"usage: expect_version [[-exit] version]");
		return(TCL_ERROR);
	}

	user_version = argv[argc==2?1:2];
	emajor = atoi(exp_version);
	umajor = atoi(user_version);

	/* first check major numbers */
	if (emajor > umajor) return(TCL_OK);
	else if (emajor == umajor) {
		int u, e;

		/* now check minor numbers */
		char *dot = strchr(user_version,'.');
		/* if user did not supply minor number, let it go */
		if (dot) {
			u = atoi(dot+1);
			dot = strchr(exp_version,'.');
			e = atoi(dot+1);
			if (e >= u) return(TCL_OK);
		}
	}

	if (argc == 2) {
		exp_error(interp,"%s requires Expect version %s (but using %s)",
			exp_argv0,user_version,exp_version);
		return(TCL_ERROR);
	}
	errorlog("%s: requires Expect version %s (but using %s)\r\n",
		exp_argv0,user_version,exp_version);
	exp_exit(interp,-1);
	/*NOTREACHED*/
}

void
exp_init(interp)
Tcl_Interp *interp;
{
	static int first_time = TRUE;

	if (first_time) {
		int tcl_major = atoi(TCL_VERSION);
		char *dot = strchr(TCL_VERSION,'.');
		int tcl_minor = atoi(dot+1);

		if (tcl_major < NEED_TCL_MAJOR || tcl_minor < NEED_TCL_MINOR) {
			fprintf(stderr,
			   "%s compiled with Tcl %d.%d but needs at least Tcl %d.%d\n",
				exp_argv0,tcl_major,tcl_minor,
				NEED_TCL_MAJOR,NEED_TCL_MINOR);
		       exit(-1);
		}

		exp_init_pty();
		exp_init_tty(); /* do this only now that we have looked at */
				/* original tty state */
		exp_init_sig();
		exp_init_event();
		exp_init_trap();
		exp_init_unit_random();
		exp_init_spawn_ids();

		first_time = FALSE;
	}

	/* save last known interp for emergencies */
	exp_interp = interp;

	/* call explicitly, so user can put history info in init prompt */
	Tcl_InitHistory(interp);

	/* initialize commands */
	exp_create_commands(interp);/* add non-expect cmds to interpreter */
	exp_init_expect(interp);	/* add expect cmds to interpreter */
	exp_init_spawn_id_vars(interp);

	Tcl_SetVar(interp,SCRIPTDIR_VARNAME,SCRIPTDIR,0);
	Tcl_SetVar(interp,VERSION_VARNAME,VERSION,0);
}

void
exp_parse_argv(interp,argc,argv)
Tcl_Interp *interp;
int argc;
char **argv;
{
	static char *cmdfilename = 0;

	int sys_rc = TRUE;	/* read system rc file */
	int my_rc = TRUE;	/* read personal rc file */

	int c;
	int rc;

	extern int optind;
	extern char *optarg;
	char *args;		/* ptr to string-rep of all args */

	exp_argv0 = argv[0];

	while ((c = getopt(argc, argv, "c:df:inN")) != EOF) {
		switch(c) {
		case 'c': /* command */
			exp_cmdlinecmds = TRUE;
			rc = Tcl_Eval(interp,optarg,0,(char **)0);

			if (rc != TCL_OK) {
			    errorlog("%s\r\n",exp_cook(get_var("errorInfo"),(int *)0));
			}
			break;
		case 'd': exp_is_debugging = TRUE;
			debuglog("expect version %s\r\n",exp_version);
			break;
		case 'f': /* name of cmd file */
			cmdfilename = optarg;
			break;
		case 'i': /* interactive */
			exp_interactive = TRUE;
			break;
		case 'n': /* don't read personal rc file */
			my_rc = FALSE;
			break;
		case 'N': /* don't read system-wide rc file */
			sys_rc = FALSE;
			break;
		default: usage(interp);
		}
	}

	for (c = 0;c<argc;c++) {
		debuglog("argv[%d] = %s  ",c,argv[c]);
	}
	debuglog("\r\n");

	/* get cmd file name, if we haven't got it already */
	if (!cmdfilename && (optind < argc)) {
		cmdfilename = argv[optind];
		optind++;
	}
	if (cmdfilename) {
		if (streq(cmdfilename,"-")) {
			exp_cmdfile = stdin;
		} else if (NULL == (exp_cmdfile = fopen(cmdfilename,"r"))) {
			errorlog("%s: %s\r\n",cmdfilename,sys_errlist[errno]);
			exp_exit(interp,-1);
		}
	}

	if (exp_cmdfile && exp_interactive) {
		errorlog("cannot read commands from both file and keyboard\r\n");
		exp_exit(interp,-1);
	}

	/* collect remaining args and make into an argv */
	/* Tcl expects argv[0] to be the command name, but it doesn't do */
	/* anything useful with it anyway, so just back up the pointer */
	/* actually, back it up twice, so we can make the resultant [0] */
	/* be the program name */
	/* Oh, and we add to argc for the same reason */
	/* If no cmdfilename, there certainly can't be any args! */
	if (cmdfilename) {
		argv[optind-1] = cmdfilename;
		Tcl_ListCmd((ClientData)0,interp,2+argc-optind,argv+optind-2);
		args = interp->result;
	} else args = "";

	debuglog("set argv {%s}\r\n",args);
	Tcl_SetVar(interp,"argv",args,0);

	exp_interpret_rcfiles(interp,my_rc,sys_rc);
}

/* read rc files */
void
exp_interpret_rcfiles(interp,my_rc,sys_rc)
Tcl_Interp *interp;
int my_rc;
int sys_rc;
{
	int rc;

	if (sys_rc) {
	    char file[200];
	    int fd;

	    sprintf(file,"%sexpect.rc",SCRIPTDIR);
	    if (-1 != (fd = open(file,0))) {
		if (TCL_ERROR == (rc = Tcl_EvalFile(interp,file))) {
		    errorlog("error executing system initialization file: %s\r\n",file);
		    if (rc != TCL_ERROR)
				errorlog("Tcl_Eval = %d\r\n",rc);
		    if (*interp->result != 0)
				errorlog("%s\r\n",interp->result);
		    exp_exit(interp,-1);
		}
		close(fd);
	    }
	}
	if (my_rc) {
	    char file[200];
	    char *home;
	    int fd;
	    char *getenv();

	    if (NULL != (home = getenv("HOME"))) {
		sprintf(file,"%s/.expect.rc",home);
		if (-1 != (fd = open(file,0))) {
		    if (TCL_ERROR == (rc = Tcl_EvalFile(interp,file))) {
			errorlog("error executing file: %s\r\n",file);
			if (rc != TCL_ERROR)
				errorlog("Tcl_Eval = %d\r\n",rc);
			if (*interp->result != 0)
				errorlog("%s\r\n",interp->result);
			exp_exit(interp,-1);
		    }
		    close(fd);
	        }
	    }
	}
}

void
exp_interpret_cmdfile(interp,fp)
Tcl_Interp *interp;
FILE *fp;
{
	int rc;

	Tcl_CmdBuf buffer;
	int newcmd;

	debuglog("executing commands from command file\r\n");

	if (!(buffer = Tcl_CreateCmdBuf())) {
		errorlog("no more space for cmd buffer\r\n");
		exp_exit(interp,0);
	}
	newcmd = TRUE;
	while (1) {
		char line[BUFSIZ];/* buffer for partial Tcl command */
		char *ccmd;	/* pointer to complete Tcl command */

		if (fgets(line,BUFSIZ,fp) == NULL) {
			if (newcmd) break;
			else line[0] = 0;
		}
		if (NULL == (ccmd = Tcl_AssembleCmd(buffer,line))) {
			newcmd = FALSE;
			continue;	/* continue collecting command */
		}
		newcmd = TRUE;

		rc = Tcl_Eval(interp,ccmd,0,(char **)0);

		if (rc != TCL_OK) {
		    /* no \n at end, since ccmd will already have one. */
		    /* Actually, this is not true if command is last in */
		    /* file and has no newline after it, oh well */
		    errorlog("%s\r\n",exp_cook(get_var("errorInfo"),(int *)0));
		}
	}
	/*NOTREACHED*/
}

