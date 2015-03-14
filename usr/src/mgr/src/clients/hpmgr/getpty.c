/*{{{}}}*/
/*{{{  #includes*/
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
/* hack !!! */
#undef _POSIX_SOURCE
#include <signal.h>
#define _POSIX_SOURCE
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef _POSIX_SOURCE
#include <termios.h>
#endif
#include <sys/types.h>
#include <sys/wait.h>

#include <sys/file.h>
#include <sys/signal.h>
#include <termios.h>
#include <stdio.h>

#include <errno.h>
#include <pwd.h>
/*}}}  */
/*{{{  #defines*/
/*
**	size of input and output buffers
*/
#define BFRSIZE		1024
#define SHELL		"/bin/sh"
/*}}}  */

/*{{{  variables*/
static char line[] = {"/dev/ptypX"};
extern char **environ;
extern	int errno;
int shellid;
int	rem;
int verboseflag = 0;
int	defflags, tabflag;
char	deferase, defkill;

/*
** flags to signal that massage routines have more data ready to go
**	these flags are necessary so that massage routines can buffer
**	data if necessary
*/
int more_out = 0;
int more_in = 0;
/*}}}  */

/*{{{  mode*/
mode(f)
{
#if 0
	struct tchars *tc;
	struct ltchars *ltc;
	struct sgttyb sb;

	ioctl(0, TIOCGETP, (char *)&sb);
	switch (f) {

	case 0:
		sb.sg_flags &= ~(CBREAK|RAW|TBDELAY);
		sb.sg_flags |= defflags|tabflag;
		tc = &deftc;
		ltc = &defltc;
		sb.sg_kill = defkill;
		sb.sg_erase = deferase;
		break;

	case 1:
		sb.sg_flags |=  RAW;
/*
		sb.sg_flags |= (eight ? RAW : CBREAK);
*/
		sb.sg_flags &= ~defflags;
		/* preserve tab delays, but turn off XTABS */
		if ((sb.sg_flags & TBDELAY) == XTABS)
			sb.sg_flags &= ~TBDELAY;
		tc = &notc;
		ltc = &noltc;
		sb.sg_kill = sb.sg_erase = -1;
		break;

	default:
		return;
	}
	ioctl(0, TIOCSLTC, (char *)ltc);
	ioctl(0, TIOCSETC, (char *)tc);
	ioctl(0, TIOCSETN, (char *)&sb);
#endif
}
/*}}}  */
/*{{{  done*/
done()
{

	mode(0);
	if (shellid > 0 && kill(shellid, SIGKILL) >= 0)
		wait((int *)0);
	cleanup();
}
/*}}}  */
/*{{{  lostpeer*/
lostpeer()
{
	signal(SIGPIPE, SIG_IGN);
	fprintf(stderr,"\007Connection closed.\r\n");
	done();
}
/*}}}  */
/*{{{  getpty*/
getpty(cmd)
	char **cmd;
{
	char pibuf[BFRSIZE], fibuf[BFRSIZE], *pbp, *fbp;
	int pcc = 0, fcc = 0;
	int cc;

	signal(SIGPIPE, lostpeer);
        shellid = get_command(cmd,&rem);
        if (rem < 0)
                return(-1);

	signal(SIGINT, exit);
	signal(SIGHUP, exit);
	signal(SIGQUIT, exit);
	mode(1);
	signal(SIGINT, SIG_IGN);
	signal(SIGCHLD, done);
	for (;;)
	{
		int ibits = 0, obits = 0;

		if (fcc)
			obits |= (1<<rem);
		else
			ibits |= (1<<0);
		if (pcc >= 0)
			if (pcc)
				obits |= (1<<1);
			else
				ibits |= (1<<rem);
		if (fcc < 0 && pcc < 0)
			break;
		select(16, &ibits, &obits, 0, 0);
		if (ibits == 0 && obits == 0) {
			sleep(5);
			continue;
		}
		if ((fcc == 0) && more_in)
		{
			fbp = fibuf;
			fcc = inmassage(fibuf,-2);
		}
		else
		{
			if (ibits & (1<<0)) {
				fcc = read(0, fibuf, sizeof (fibuf));
				if (fcc < 0 && errno == EWOULDBLOCK)
					fcc = 0;
				else {
					if (fcc <= 0)
						break;
					fbp = fibuf;
					fcc = inmassage(fibuf,fcc);
				}
			}
		}
		if ((pcc == 0) && more_out)
		{
			pbp = pibuf;
			pcc = outmassage(pibuf,-2);
		}
		else
		{
			if (ibits & (1<<rem)) {
				pcc = read(rem, pibuf, sizeof (pibuf));
				pbp = pibuf;
				if (pcc < 0 && errno == EWOULDBLOCK)
					pcc = 0;
				else if (pcc <= 0)
					pcc = -1;
				pcc = outmassage(pibuf,pcc);
			}
		}
		if ((obits & (1<<1)) && pcc > 0) {
			cc = write(1, pbp, pcc);
			if (cc > 0) {
				pcc -= cc;
				pbp += cc;
			}
		}
		if ((obits & (1<<rem)) && fcc > 0) {
			cc = write(rem, fbp, fcc);
			if (cc > 0) {
				fcc -= cc;
				fbp += cc;
			}
		}
	}
	fprintf(stderr,"Closed connection.\r\n");
	done();
	/* this point should never be reached !!! */
	return(-2);
}
/*}}}  */
/*{{{  writer*/
/*
 * writer: write to remote: 0 -> line.
 */
writer()
{
	int c;
	while (read(0,&c,1) != 0)
	{
		if (write(rem, &c, 1) == 0) {
			fprintf(stderr,"line gone\r\n");
			return(0);
		}
	}
}
/*}}}  */
/*{{{  reader*/
/*
 * reader: read from remote: line -> 1
 */
reader()
{
	char rb[BUFSIZ];
	register int cnt;

	for (;;) {
		cnt = read(rem, rb, sizeof (rb));
		if (cnt == 0)
			break;
		if (cnt < 0) {
			if (errno == EINTR)
				continue;
			break;
		}
		write(1, rb, cnt);
	}
}
/*}}}  */
/*{{{  getapty -- get a pty line*/
static int getapty()
{
  int i;
  int pty_fd, tty_fd;

  for(line[8]='p';line[8]<='t';line[8]++) for (i=0;i<=16;i++)
  {
    line[9]="0123456789abcdef"[i%16];
    line[5] = 'p';
    if ((pty_fd=open(line,O_RDWR))>=0)
    {
      line[5] = 't';
      if ((tty_fd=open(line,O_RDWR))>=0) { close(tty_fd); return(pty_fd); }
      else close(pty_fd);
    }
  }
  return(-1);
}
/*}}}  */
/*{{{  last_tty*/
char *
last_tty()
   {
   return(line);
   }
/*}}}  */
/*{{{  doenv -- change an environment variable*/
void do_env(name,value) char *name,*value;
   {
   register int i;
   int n = strlen(name);

   for(i=0;environ[i] != (char *) 0;i++)
      if (strncmp(environ[i],name,n) == 0) {
         strcpy(environ[i]+n,value);
         break;
         }
   }
/*}}}  */
/*{{{  get_path -- get a complete path name from command*/
static char path[512];
static char start[512];

char *
get_path(name)
char *name;
   {
   register char *next, *list;

   if (index("/.",*name))
      if (access(name,X_OK)==0)
         return(name);
      else
         return((char *)0);

   strcpy(start,getenv("PATH"));
   for(list=start;next=index(list,':');list=next+1) {
      *next = '\0';
      sprintf(path,"%s/%s",list,name);
      if (access(path,X_OK) == 0)
         return(path);
      }

   sprintf(path,"%s/%s",list,name);
   if (list && access(path,X_OK) == 0) {
      return(path);
      }
   else {
      return((char *) 0);
      }
   }
/*}}}  */
/*{{{  get_command -- start a command*/
int get_command(argv,file) char **argv; int *file;
{
   /*{{{  variables*/
   register int i;				/* counter */
   int fd;					/* file desc */
   pid_t pid;					/* pid of shell */
   int group;					/* process group id */
   int tty_slots;				/* # of tty slots */
   int tty;
   char *name;
   char *shell = getenv("SHELL");
   char *arg[2];
   /*}}}  */

   if (argv == (char **) 0 )
   {
      argv = arg;
      *argv = shell?shell:SHELL;
      *(argv+1) = (char *) 0;
   }
   name = get_path(argv[0]);

   if (name == (char *) 0 || *name == '\0') return(-2);

   if ((*file=getapty())<0) return(-1);
   if ((pid=fork())>0)
   /*{{{  parent side of fork*/
   return(pid);
   /*}}}  */
   else if (pid<0)
   /*{{{  error side of fork*/
   return(pid);
   /*}}}  */

   /*{{{  child side of fork*/
   /*{{{  set signal handlers to default*/
   for(i=0;i<NSIG;i++) signal(i,SIG_DFL);
   /*}}}  */
   /*{{{  void association with controlling terminal*/
#      ifdef SYSV
   tty_slots = 20;
#      else
   tty_slots = getdtablesize();
#      endif

   for(i=0;i<tty_slots;i++) close(i);

#   ifdef TIOCNOTTY
   tty=open("/dev/tty",O_RDONLY);
   ioctl(tty,TIOCNOTTY,0);
   close(tty);
#   endif
   /*}}}  */
   /*{{{  set process group and session ID*/
#      ifdef _POSIX_SOURCE
   setpgrp();
   setsid();
#      endif
   /*}}}  */
   /*{{{  make it controlling tty*/
   /* set the uid-0 stuff up */

   fd=open(line,O_RDONLY);
   if (fd!=0) perror("mgr: internal error with opening fd 0");
   if (geteuid() < 1) {
      int uid = getuid();
      fchmod(fd,0622);
      fchown(fd,uid,-1);
      setreuid(uid,uid);

      uid = getgid();
      fchown(fd,-1,uid);
      setregid(uid,uid);
      }
   if (open(line,O_WRONLY)!=1) { perror("mgr: internal error with opening fd 1"); exit(1); }
   if (open(line,O_WRONLY)!=2) { perror("mgr: internal error with opening fd 2"); exit(1); }
   /*}}}  */
   /*{{{  set up tty mode*/
#   if 0
   adjust_mode(0,ECHO|ICANON|ISIG);
   restore_modes(0);
#   endif
   /*}}}  */
   /*{{{  add a utmp entry*/
#      ifdef WHO
   add_utmp(line);
#      endif
   /*}}}  */
   /*{{{  start the command*/
   do_env("TERM=","hp2");
   execve(name,argv,environ);
   _exit(1);
   /*}}}  */
   /*}}}  */
}
/*}}}  */
