#define VERSION	"1.0.7"
#define I_IOCTL
#define I_SOCKET
#define I_ERRNO
#define I_SYS
#include "includes.h"

#include "debug.h"
#include <signal.h>
#include <sys/stat.h>
#include <pwd.h>

#ifdef USE_TIOCNOTTY
#include <sys/ioctl.h>
#endif

#ifdef SYSV
#include <stropts.h>
#ifdef SVR4
int grantpt(int);
char *ptsname(int);
int unlockpt(int);
#endif
#endif

int pty_pid;

/* Moved these strings here for compilers which can't handle 
   aggregate auto initialization.  Makes open_pty() non-reentrant.
   HSW 93-01-29
   */
#ifdef __hpux
static  char masterline[]="/dev/ptym/ptyXY";
static  char slaveline[]="/dev/pty/ttyXY";
static  char *first ="pqrstuv";
#else				/*  not hpux, and not sgi. */
static  char masterline[]="/dev/ptyXY";
static  char *first ="pqrstuvwzyz";
static  char slaveline[]="/dev/ttyXY";
#endif

int open_pty(char *prog) 
{
  int masterfd = -1,
  slavefd,
  i,
  pid;
  int pip[2];
#if !defined(sgi) && !defined(SYSV)
#define	SFIRST	(sizeof (slaveline) - 3)
#define	SSECOND	(sizeof (slaveline) - 2)

				/* Ok. Stuff for not sgi. */
#define	MFIRST	(sizeof (masterline) - 3)
#define	MSECOND	(sizeof (masterline) - 2)
  char *ptr = first;
  int c;
#else /* sgi || SYSV */
#ifdef SYSV
				/* Stuff for SYSV */
  void (*savesig)(int);
  char *slaveline;
#else /* SYSV */
				/* And then finally, sgi stuff. */
  char *linetmp;
  char slaveline[20];
#endif /* SYSV */
#endif /* sgi || SYSV */
  
#if !defined(sgi) && !defined(SYSV)
  /* get a pseudo tty */
  ptr = first;
  for (c = *ptr; (c = *ptr); ++ptr) {
    struct stat statbuff;
    
    masterline[MFIRST] = c;
    masterline[MSECOND] = '0';
    
    if (stat (masterline, &statbuff) < 0)
      continue; /* no pty's on this bank availiable */

    for (i = 0; i < 16; ++i) {
      masterline[MSECOND] = "0123456789abcdef"[i];
      if ((masterfd = open (masterline, O_RDWR, 0)) > 0)
	break;
    }
    if (i != 16) {
				/* Ok. now check to make sure we can */
				/* open the slave as well. */
      slaveline[SFIRST] = masterline[MFIRST];
      slaveline[SSECOND] = masterline[MSECOND];
      
      if ((slavefd = open (slaveline, O_RDWR, 0)) < 0) {
	close(masterfd);
	continue;			/* unable to open slave pty */
      }
      break;
    }
    
  }

  if (!c)
    return -1;
  
#ifdef SUIDROOT
  if (fchmod (slavefd, 0622))
    return -2;
#endif
  
  close(slavefd);

#else /* sgi || SYSV */

#ifdef SYSV
  if ((masterfd = open("/dev/ptmx", O_RDWR, 0)) < 0)
    return -1;
  savesig = signal(SIGCHLD, SIG_DFL);
  if (grantpt(masterfd) < 0) {
    close(masterfd);
    sigset(SIGCHLD, savesig);
    return -1;
  }
  sigset(SIGCHLD, savesig);
  if (unlockpt(masterfd) < 0) {
    close(masterfd);
    return -1;
  }
  slaveline = ptsname(masterfd);
  if (slaveline == NULL) {
    close(masterfd);
    return -1;
  }
#else /* SYSV */
  
  linetmp = _getpty(&masterfd, O_RDWR, 0600, 0);
  
  if (linetmp == NULL)
    return -1; /* no pty's available */
  
  strcpy(slaveline, linetmp);
  
  /* Don't check slave - have to assume it's going to work */
  
#endif /* SYSV */
#endif /* sgi */

  fflush (stdout);
  pipe(pip);
  if ((pid = fork ()) < 0)
    return -3; /* unable to fork */
  
  if (pid == 0) { /* child. */
/*    char  ** a, *b[2] = {0, }; Ultrix MIPS compiler chokes */
    char  ** a, *b[2];
    b[0]=b[1]=NULL;

    close(0);
    close(1);
    close(2);

    lose_ctty();
    
    /* make it control tty */
    if ((slavefd = open (slaveline, O_RDWR, 0)) < 0) {
      exit(1);
    }
#ifdef USE_VHANGUP
    signal (SIGHUP, SIG_IGN);
    vhangup ();
    signal (SIGHUP, SIG_DFL);
    
    if ((slavefd = open (slaveline, O_RDWR, 0)) < 0) {
      exit(1);
    }
#endif

#ifdef SYSV
    if (ioctl(slavefd, I_PUSH, "ptem") < 0) {
      exit(1);
    }
    if (ioctl(slavefd, I_PUSH, "ldterm") < 0) {
      exit(1);
    }
#ifdef SVR4
    if (ioctl(slavefd, I_PUSH, "ttcompat") < 0) {
      exit(1);
    }
#endif /* SVR4 */
#endif /* SYSV */

    /*
     **	do a term setup here.
     */
    dup2 (slavefd, 0);
    dup2 (slavefd, 1);
    dup2 (slavefd, 2);  
    for (slavefd = 3; slavefd < 64; ++slavefd)
    {
      if (slavefd != pip[1])  close(slavefd);
    }

#if !defined(SVR4)
    terminal_restore(0); 
#endif

    /* find out what shell to run. */
    /* note: technically we should free a, but as we are 
     * in the child who will either exec or exit, we can 
     * neglect this.  croutons.
     */
    a = rebuild_arg(prog);
    if ( !a )  a = b; 
    if ( !a[0] )  a[0] = getenv("SHELL");
    if ( !a[0] )  a[0] = "/bin/sh";

    /* these printfs are dangerous on NeXT,
       if the strings are too long, the printfs block! */
    printf ("Remote: term %s\r\n", VERSION);
    printf ("tty %s. Exec %s\r\n", slaveline, a[0]);

    /* pipe will close upon successful exec */
    if (fcntl (pip[1], F_SETFD, 1) == -1)
    {
      /* close it now since fcntl failed */
      close (pip[1]);
    }

    execvp(a[0], a);
    
   /* write something so parent notes exec failure */
    write(pip[1],"",1);
    close(pip[1]);

    /* for what it's worth */
    printf ("Exec failed %s\r\n", a[0]);
    exit(1);
  }

  /* parent */
  close(pip[1]);

  /* Wait for exec, read blocks until pipe is closed */
  if (read(pip[0], (char *) &i, 1) == 0)
    DEBUG_FP (stderr, "%s: exec apparently succeeded\n", term_server);
  else
    DEBUG_FP (stderr, "%s: exec apparently failed\n", term_server);
  close(pip[0]);

  set_nonblock(masterfd);
  /*	ioctl(masterfd, TIOCPKY, &one); BSD pty packet mode. */
  pty_pid = pid;
  
  return masterfd;
}

static char *argv[4] = {"/bin/sh", "-c", NULL, NULL};

/* non-pty exec */
/* usage is more like rsh, shell can handle compound command */
int open_socket(char *prog) 
{
  int i;
  int soc[2];
  int pid;
  int pip[2];
  
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, soc) < 0)  
    return -4;  /* no socket */

  /* soc[0] is term's end */
  set_nonblock(soc[0]);

  pipe(pip);
  if ((pid = fork ()) < 0)
    return -3; /* unable to fork */
  
  if (pid == 0) /* child. */
  { 
    close (0);
    close (1);
    close (2);

    lose_ctty();

    /* soc[1] is child's end */
    dup2 (soc[1], 0);
    dup2 (soc[1], 1);
    dup2 (soc[1], 2);
    for (i = 3; i < 64;++i)
      if (i != pip[1])
	close(i);
    
    setbuf(stdout, NULL);
    
    /* unpack args */
    for (i=0; prog[i] != '\0'; ++i)
      if (prog[i] == '\377')
	prog[i] = ' ';

    argv[2] = prog;

    /* pipe will close upon successful exec */
    if (fcntl (pip[1], F_SETFD, 1) == -1)
      close (pip[1]);

    execvp(argv[0], argv);

    write(pip[1],"",1);
    close(pip[1]);
    
    printf ("Exec failed %s\r\n", argv[0]);
    exit(1);
  }

  /* parent */
  close(pip[1]);

  /* Wait for exec, read blocks until pipe is closed */
  if (read(pip[0], (char *) &i, 1) == 0)
    DEBUG_FP (stderr, "%s: exec apparently succeeded\n", term_server);
  else
    DEBUG_FP (stderr, "%s: exec apparently failed\n", term_server);
  close(pip[0]);

  pty_pid = pid;
  return soc[0];
}
