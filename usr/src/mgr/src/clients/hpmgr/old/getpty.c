/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
#ifndef lint
static char rcsid[] = "$Header: getpty.c,v 4.2 88/06/22 14:37:33 bianchi Exp $";
#endif
#include <sys/types.h>
#include <sys/wait.h>

#include <sys/file.h>
#include <sys/signal.h>
#include <sgtty.h>
#include <stdio.h>

#include <errno.h>
#include <pwd.h>

/*
**	size of input and output buffers
*/
#define BFRSIZE		1024

extern	int errno;
extern  int done();
int	lostpeer();
int shellid;
int	rem;
int verboseflag = 0;
int	defflags, tabflag;
char	deferase, defkill;
struct	tchars deftc;
struct	ltchars defltc;
struct	tchars notc =	{ -1, -1, -1, -1, -1, -1 };
struct	ltchars noltc =	{ -1, -1, -1, -1, -1, -1 };

/*
** flags to signal that massage routines have more data ready to go
**	these flags are necessary so that massage routines can buffer
**	data if necessary
*/
int more_out = 0;
int more_in = 0;
getpty(cmd)
	char **cmd;
{
	int exit();
	struct sgttyb sb;
	char pibuf[BFRSIZE], fibuf[BFRSIZE], *pbp, *fbp;
	int pcc = 0, fcc = 0;
	int cc;

	signal(SIGPIPE, lostpeer);
        shellid = get_command(cmd,&rem);
        if (rem < 0)
                return(-1);

	ioctl(0, TIOCGETP, (char *)&sb);
	defflags = sb.sg_flags;
	tabflag = defflags & TBDELAY;
	defflags &= ECHO | CRMOD;
	deferase = sb.sg_erase;
	defkill = sb.sg_kill;
	ioctl(0, TIOCGETC, (char *)&deftc);
	notc.t_startc = deftc.t_startc;
	notc.t_stopc = deftc.t_stopc;
	ioctl(0, TIOCGLTC, (char *)&defltc);
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
		select(16, &ibits, &obits, 0, 0, 0);
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

done()
{

	mode(0);
	if (shellid > 0 && kill(shellid, SIGKILL) >= 0)
		wait((int *)0);
	cleanup();
}


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

mode(f)
{
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
}


lostpeer()
{
	signal(SIGPIPE, SIG_IGN);
	fprintf(stderr,"\007Connection closed.\r\n");
	done();
}


#define SHELL		"/bin/sh"

#ifdef SYSV
#define index		strchr
#endif

static char *line = "/dev/ptypX";
static int  pty_index=5;	/* better hit rate than 0 */
extern char **environ;

/* place to save tty modes */

int t_ldisc;
struct sgttyb t_sgttyb;
struct tchars t_tchars;
struct ltchars t_ltchars;
int t_lflags;
/*	get a pty line */

int
getapty()
   {
   register int i;
   int fd;
   char list[20];

   strcpy(list,"0123456789abcdef");
   line[5] = 'p';
/*
   for(line[8]='p';line[8]!='r';line[8]='q')
*/
   for(line[8]='p';line[8]!='r';line[8]++)
      for (i=1;i<=16;i++) {
         line[9]=list[(pty_index+i)%16];
	 if (verboseflag)
	 {
		printf("trying %s\n",line);
	 }
         if ((fd = open(line,2)) >= 0) {
            /* pty_index = (pty_index+i)%16; */
	    if (verboseflag)
	    {
		printf("   GOT %s\n",line);
	    }
            line[5] = 't';
            return(fd);
            }
         }
   return(-1);
   }
      
int getatty()
   {
   int fd;
   line[5]='t';
   fd=open(line,2);
   if (fd<0) {
      sleep(3);
      return (open(line,2));
      }
   return(fd);
   }

char *
last_tty()
   {
   return(line);
   }

/******************************************************************************/
/* start a command */

get_command(argv,file)
char **argv;
int *file;
   {
   register int i;				/* counter */
   int fd;					/* file desc */
   int tty;					/* fd of /dev/tty */
   int pid;					/* pid of shell */
   int group;					/* process group id */
   int tty_slots;				/* # of tty slots */
   char *name, *get_path();
   char *getenv();
   char *shell = getenv("SHELL");
   char *arg[2];
#define MAXNAME 256
   char who[MAXNAME];

   if (argv == (char **) 0 ) {
      argv = arg;
      *argv = shell?shell:SHELL;
      *(argv+1) = (char *) 0;
      }
   name = get_path(argv[0]);

   if (name == (char *) 0 || *name == '\0')
      return(-2);

#ifdef DEBUG
   dprintf(stderr,"EXECING: ");
   for(i=0;argv[i]!='\0';i++)
      dprintf(stderr,"%s ",argv[i]);
   dprintf("\n");
#endif

   if ((*file=getapty()) < 0)
      return(-1);
   ioctl(*file,TIOCREMOTE,0);	/* I dunno */

   ioctl(0,TIOCGETD,&t_ldisc);
   ioctl(0,TIOCGETP,&t_sgttyb);
   ioctl(0,TIOCGETC,&t_tchars);
   ioctl(0,TIOCGLTC,&t_ltchars);
   ioctl(0,TIOCLGET,&t_lflags);

   if ((pid=fork()) != 0) {
      return(pid);
      }

   /* void association with controlling terminal */

#ifdef TIOCNOTTY
   tty = open("/dev/tty",0);
   ioctl(tty,TIOCNOTTY,0);
   close(tty);
#endif

   if ((fd=getatty())<0) {
	char permsg[256];
      sprintf(permsg,"Slave side of p-tty %s won't open",line);
      perror(permsg);
      sleep(5);
      exit(1);
      }


   group=getpid();

#ifndef SYSV
   tty_slots = getdtablesize();
#else
   tty_slots = 20;
#endif

   for(i=0;i<tty_slots;i++) if (i != fd) close(i);

   /* set the uid stuff up */

   if (geteuid() < 2) {
      int uid = getuid();
      fchmod(fd,0622);
      fchown(fd,uid,-1);
      setreuid(uid,uid);

      uid = getgid();
      fchown(fd,-1,uid);
      setregid(uid,uid);
      }

   dup(fd), dup(fd), dup(fd);
   close(fd);

   setpgrp(group,group);
   ioctl(0,TIOCSPGRP,&group);

   t_ldisc=NTTYDISC;
   t_sgttyb.sg_flags = ECHO|CRMOD|EVENP|ODDP;

   ioctl(0,TIOCSETD,&t_ldisc);
   ioctl(0,TIOCSETP,&t_sgttyb);
   ioctl(0,TIOCSETC,&t_tchars);
   ioctl(0,TIOCSLTC,&t_ltchars);
   ioctl(0,TIOCLSET,&t_lflags);

   /* add a utmp entry */

/*
#ifdef WHO
   sprintf(who,"%s%c",HOST,line[9]);
   add_utmp(0,who);
#endif
*/

   /* start the command */

#ifdef DEBUG
   dprintf(stderr,"execing %s (%s ...)\r\n",name,*argv);
   fflush(stderr);
#endif

/*
   do_env("TERM=",TERMNAME);
   do_env("TERMCAP=","");
*/

   execve(name,argv,environ);
   _exit(1);
   }

/* get a complete path name from command */

static char path[512];
static char start[512];

char *
get_path(name)
char *name;
   {
   char *getenv(), *index();
   register char c, *next, *list;

#ifdef DEBUG
   dprintf(stderr,"looking for command: %s\n",name);
#endif

   if (index("/.",*name))
      if (access(name,X_OK)==0)
         return(name);
      else
         return((char *)0);

   strcpy(start,getenv("PATH"));
   for(list=start;next=index(list,':');list=next+1) {
      *next = '\0';
      sprintf(path,"%s/%s",list,name);
#ifdef DEBUG
      dprintf(stderr," X? %s\n",path);
#endif
      if (access(path,X_OK) == 0)
         return(path);
      }

   sprintf(path,"%s/%s",list,name);
#ifdef DEBUG
   dprintf(stderr,"X? %s\n",path);
#endif
   if (list && access(path,X_OK) == 0) {
      return(path);
      }
   else {
      return((char *) 0);
      }
   }

/* change an environment variable */

do_env(name,value)
char *name, *value;
   {
   register int i;
   int n = strlen(name);
   
   for(i=0;environ[i] != (char *) 0;i++)
      if (strncmp(environ[i],name,n) == 0) {
         strcpy(environ[i]+n,value);
         break;
         }
   }

