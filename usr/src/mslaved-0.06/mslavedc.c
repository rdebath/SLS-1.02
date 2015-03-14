/* mslavedc.c - MandelSpawn computation server control program */
/* (named in accordance with timedc, etc.). */

/*  This file is part of MandelSpawn, a parallel Mandelbrot program for
    the X window system.

    Copyright (C) 1990 Andreas Gustafsson

    MandelSpawn is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License, version 1,
    as published by the Free Software Foundation.

    MandelSpawn is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License,
    version 1, along with this program; if not, write to the Free 
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <signal.h>

/* The only system I know needs these is SCO Unix, which needs them to */
/* define FNDELAY.  If they cause trouble with you system, remove them */
#include <sys/types.h>
#include <sys/file.h>

#include "ms_ipc.h"

#define RETRIES 5 /* how many times to retry getting a PID in ping() */

extern int errno; /* 4.3BSD needs this even when errno.h is included */

char *me; 		/* name of program */
int port=DEFAULT_PORT;	/* UDP port */
char *command=NULL;
char *timeout_arg="-t900"; /* by default time out in 15 minutes */
char *nice_arg=NULL;
int kill_mode=0;


/* report a fatal error and exit */

error(s)
     char *s;
{ fprintf(stderr, "%s: ", me);
  perror(s);
  exit(1);
}


/* Repeatedly send datagrams to "host" trying to find out the pid of */
/* the existing server, if any.  Returns the pid or 0 if no response. */

int ping(host)
     char *host;
{ int i;
  int status;
  WhoAreYouMessage out;
  Message in;
  int osock, isock;
  struct sockaddr_in oname, iname, mname;
  int mnamelen;
  struct hostent *hp, *gethostbyname();
  /* set up the output socket; use an arbitrary port */
  osock=socket(AF_INET, SOCK_DGRAM, 0);
  if(osock<0)
  { error("opening output socket");
  }
  hp=gethostbyname(host);
  if(hp==0)
  { error("unknown host");
  }
  /* set up the destination address */
  oname.sin_family=AF_INET;
  oname.sin_port=htons(port);
  bcopy(hp->h_addr, (char *)&oname.sin_addr, hp->h_length);
  isock=socket(AF_INET, SOCK_DGRAM, 0);
  if(isock<0)
  { error("opening input socket");
  }
  iname.sin_family=AF_INET;
  iname.sin_addr.s_addr=INADDR_ANY;
  iname.sin_port=0;
  if(bind(isock, &iname, (int) sizeof(iname)))
  { error("binding input socket");
  }
  if(fcntl(isock, F_SETFL, FNDELAY) == -1)
  { error("unblocking socket");
  }
  mnamelen=sizeof(mname);
  if(getsockname(isock, &mname, &mnamelen)== -1)
    error("getsockname");
  out.header.magic=htons(MAGIC);
  out.header.version=htons(VERSION);
  out.header.type=htons(WHO_R_U_MESSAGE);
  out.port=mname.sin_port; /* in network byte order already */
  for(i=0; i<RETRIES; i++)
  { int nbytes;
    int fromlen;
    if(sendto(osock, (char *)&out, sizeof(WhoAreYouMessage), 0,
	      (struct sockaddr *)&oname, sizeof(oname)) < 0)
      error("sending pid inquiry message");
    /* This previously used read(), but some non-BSD TCP/IP implementations */
    /* don't allow it to be used with connectionless sockets.  Also, */
    /* while the Sun implementation does allow for a null pointer */
    /* for the "from" argument in recvfrom(), the "fromlen" argument */
    /* may not be a null pointer. */
    nbytes = recvfrom(isock, (char *) &in, sizeof(in),
		      0, (struct sockaddr *) 0, &fromlen);
      if(nbytes == 0
#ifdef EWOULDBLOCK
	 || (nbytes == -1 && errno == EWOULDBLOCK)
#endif
	)
      {	/* no reply yet */
	sleep(1);
      }
      else if(nbytes == -1)
      {	error("receiving pid info");
      }
      else /* got a reply */
      {	status=ntohs(in.iam.pid);
	goto done;
      }
  }
  status=0; /* unsuccessful */
 done:
  (void) close(osock);
  (void) close(isock);
  return(status);
}


/* Kill any server that is already active in this machine */
/* return 0 if successful kill, 1 if no response */
int murder()
{ int pid;
  char my_name[256];
  if(gethostname(my_name, sizeof(my_name)) == 1)
    error("gethostname");
  pid=ping(my_name); /* get pid of existing server, if any */
  if(pid==0)
    return(1); /* return failed exit status */
  if(kill(pid, SIGTERM)== -1) 
    error("kill");
  return(0);
}


/* Set up the file handles and exec an mslaved */

void birth()
{ int isock;
  struct sockaddr_in iname;
  int tty;
  int i;

  /* argument list for mslaved */
  char *arglist[10]; /* more than enough */
  char **a; /* pointer to the above */

  /* set up the socket */
  isock=socket(AF_INET, SOCK_DGRAM, 0);
  if(isock<0)
  { error("opening socket");
  }
  iname.sin_family=AF_INET;
  iname.sin_addr.s_addr=INADDR_ANY;
  iname.sin_port=htons(port);
  if(bind(isock, &iname, (int) sizeof(iname)) == -1) 
  { /* probable cause of error is that the port is in use */
    error("binding socket");
  }
  
  /* dup the newly-opened socket to stdin */
  close(0);
  dup(isock);

#ifdef TIOCNOTTY /* presumably BSD-like */
  if((tty=open("/dev/tty", 0, 0)) != -1)
  { ioctl(tty, TIOCNOTTY, (struct sgttyb *) 0);
    (void) close(tty);
  }
  else
  { if(setpgrp(0, 0)) 
      error("setpgrp");
  }
#else /* presumably SYSV-like */
  setpgrp();
#endif

  for(i=1; i<10; i++)
    (void) close(i);
  
  a = arglist;
  *a++ = command;
  if(timeout_arg)
    *a++ = timeout_arg;
  if(nice_arg)
    *a++ = nice_arg;
  *a++ = (char *) 0;
  execvp(command, arglist);
  exit(1); /* there's no good way to report errors here */
}


main(argc, argv)
     int argc; char **argv;
{ char *s;

  me=argv[0];
  for(;s = *++argv, --argc;)
  { if(*s == '-')
      switch(*++s)
      {	
      case 'k': /* kill */
	kill_mode++;
	break;
      case 'q': /* query */
      {	int rpid=ping(++s);
	if(rpid) 
	{ printf("%d\n", rpid);
	  exit(0);
	}
	else exit(1);
      }
      case 'n': /* nice */
	nice_arg= s-1; /* include the "-n" */
	break;
      case 't':
	timeout_arg= s-1; /* include the "-t" */
	break;
      case 'p': /* port */
	port=atoi(++s);
	break;
      default: 
	goto usage;
      }
    else
    { command= s;
    }
  }

  if(kill_mode) 
    exit(murder());

  if(!command)
    error("missing command");

  switch(fork())
  { case -1:
      error("fork");
    case 0: /* child */
      birth();
    default: /* parent */
      exit(0);
  }
 usage:
  fprintf(stderr, "usage: %s [-k] [-q] [-nnice] [-ttimeout] [-pport]\n", me);
  exit(1);
}
