#include <stdio.h>
#define __USE_BSD_SIGNAL
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <utmp.h>
#include "global.h"
#include "config.h"
#include "mbuf.h"
#include "timer.h"
#include "icmp.h"
#include "netuser.h"
#include "tcp.h"
#include "telnet.h"
#include "session.h"

#define TUMAXSCAN 32	/* max number of telunix clients active */
#define TURQSIZ	512	/* max data we will request from pty at once */

extern int debug_options;
extern FILE *trfp;            /* file pointer used for tracing */
extern char *t_options[];

struct tcb *tnix_tcb = NULLTCB;
struct tcb *tnixtcb[TUMAXSCAN];	/* savebuf for tcb ptrs for scan routines */
int sessionpid[TUMAXSCAN]; /* pid associated with telunix session */
struct tcb *tnix_fd2tcb[32];  /* fd to tcb */
extern int errno;
#ifdef	SYS5
extern unsigned long selmask;
extern unsigned long tnixmask;
#endif
void tnix_scan();

/* Start telnet-unix server */
tnix1(argc,argv)
char *argv[];
{
	struct socket lsocket;
	extern int32 ip_addr;
	void tnix_state();
	void tnix_rcv();
	void tnix_xmt();
	struct sigaction action;

	/* Incoming Telnet */
	lsocket.address = ip_addr;

	if(argc < 2)
		lsocket.port = TELNET_PORT;
	else
		lsocket.port = atoi(argv[1]);

	tnix_tcb = open_tcp(&lsocket,NULLSOCK,TCP_SERVER,0,
			tnix_rcv,tnix_xmt,tnix_state,0,(char *)NULL);

	if(tnix_tcb == NULLTCB)
		fprintf(stderr,"start telunix fails rsn %d.\n",net_error);
	else
		log(tnix_tcb,"STARTED Telunix - (%d %x)",
			lsocket.port,tnix_tcb);

	(void) signal(SIGCHLD, tnix_scan);
}

/* Shut down Telnet server */
tnix0()
{
	if(tnix_tcb != NULLTCB) {
		log(tnix_tcb,"STOPPED Telunix - (%x)",tnix_tcb);
		close_tcp(tnix_tcb);
	} else
		fprintf(stderr,"stop telunix fails -- no server active.\n");
}

/* Handle incoming Telnet-Unix connect requests */
static void
tnix_state(tcb,old,new)
struct tcb *tcb;
char old,new;
{
	register struct telnet *tn;
	int tnix_addscan();
	extern void tnix_rmvscan();
	extern int send_tcp();
	extern struct mbuf *free_p();
	extern int del_tcp();
	extern int close_tcp();
#ifdef	SYS5
	extern void free();
#endif
	char *ttyname;
	int scanindex;

	tn = (struct telnet *)tcb->user;

	switch(new){
	case ESTABLISHED:
		/* Create and initialize a Telnet protocol descriptor */
		if((tn = (struct telnet *)calloc(1,sizeof(struct telnet))) == NULLTN){
			log(tcb,"reject Telunix - no space");
			sndmsg(tcb,"Rejected; no space on remote\n");
			close_tcp(tcb);
			return;
		}
		tn->session = NULLSESSION;
		tn->state = TS_DATA;
		tcb->user = (char *)tn;	/* Upward pointer */
		tn->tcb = tcb;		/* Downward pointer */
		tn->inbuf = NULLBUF;
		tn->outbuf = NULLBUF;
		ttyname = (char *)0;
		if((scanindex = tnix_addscan(tcb)) < 0 ||
		   (tn->fd = OpenPty(&ttyname)) < 3) {	/* barf if <= stderr */
			tnix_rmvscan(tcb);
			log(tcb,"reject Telunix - no Unix ports");
			sndmsg(tcb,
			    "Rejected; no ports available on remote\n");
			close_tcp(tcb);
 			free (ttyname);
			return;
		}
#ifdef	SYS5
		tnix_fd2tcb[tn->fd] = tcb;
		selmask |= (1 << tn->fd);
		tnixmask |= (1 << tn->fd);
#endif
  		log(tcb,"open Telunix - (%d %x %d %d)",tn->fd,tcb,old,new);

		{ /* negotiate connection */
			char data[] = { IAC, WILL, TN_ECHO, IAC, WILL,
				TN_SUPPRESS_GA /*, IAC, WILL, TN_TRANSMIT_BINARY,
				IAC, WILL, TN_STATUS, IAC, DO, TN_LFLOW, IAC,
				DONT, ECHO */};
			send_tcp (tcb, qdata (data, 6 /* 18 */));
		}

		/* spawn login process */
		switch (sessionpid[scanindex] = fork ()) {
		case -1:
			sndmsg (tcb, "fork failed!\n");
			close_tcp (tcb);
			free (ttyname);
			return;
		case 0:
			{
				int i;
				for (i = 0; i < getdtablesize (); i++) close (i);
				setsid ();
				execl ("/etc/getty","getty","9600",ttyname+5,NULL);
				execl ("/etc/agetty","getty","9600",ttyname+5,NULL);
				execl ("/bin/getty","getty","9600",ttyname+5,NULL);
				execl ("/bin/agetty","getty","9600",ttyname+5,NULL);
				exit (1);
			}
		}
		sndmsg(tcb, "\r\nLinux ka9q telnet server\r\n");

		free (ttyname);
		break;

	case FINWAIT1:
	case FINWAIT2:
	case CLOSING:
	case LAST_ACK:
	case TIME_WAIT:
		if(tn != NULLTN &&
		   tn->fd > 2) {
			log(tcb,"close Telunix - (%d %x %d %d)",
				tn->fd,tcb,old,new);
			close(tn->fd);
#ifdef	SYS5
			selmask &=  ~(1 << tn->fd);
			tnixmask &=  ~(1 << tn->fd);
			tnix_fd2tcb[tn->fd] = NULL;
#endif
			tn->fd = 0;
		}
		tnix_rmvscan(tcb);
		break;

	case CLOSE_WAIT:
		/* flush that last buffer */
		if(tn != NULLTN &&
		   tn->outbuf != NULLBUF &&
		   tn->outbuf->cnt != 0) {
			send_tcp(tcb,tn->outbuf);
			tn->outbuf = NULLBUF;
		}
		close_tcp(tcb);
		break;
	
	case CLOSED:
		if(tn != NULLTN) {
			if(tn->fd > 2) {
				log(tcb,"close Telunix - (%d %x %d %d)",
					tn->fd,tcb,old,new);
				close(tn->fd);
#ifdef	SYS5
				selmask &=  ~(1 << tn->fd);
				tnixmask &=  ~(1 << tn->fd);
				tnix_fd2tcb[tn->fd] = NULL;
#endif
				tn->fd = 0;
			}
			if(tn->inbuf != NULLBUF)
				free_p(tn->inbuf);
			if(tn->outbuf != NULLBUF)
				free_p(tn->outbuf);
			free((char *)tn);
		}
		tnix_rmvscan(tcb);
		del_tcp(tcb);
		if(tcb == tnix_tcb)
			tnix_tcb = NULLTCB;
		break;
	}
}

/* Telnet receiver upcall routine */
void
tnix_rcv(tcb,cnt)
register struct tcb *tcb;
int16 cnt;
{
	register struct telnet *tn;
	extern void tnix_rmvscan();
	extern int recv_tcp();
	extern void tnix_input();

	if((tn = (struct telnet *)tcb->user) == NULLTN || tn->fd < 3) {
		/* Unknown connection - remove it from queue */
		log(tcb,"error Telnet - tnix_try (%d)", tn);
		tnix_rmvscan(tcb);
		return;
	}
	/*
	 * Check if there is any pending io for the pty:
	 */
	if(tn->inbuf != NULLBUF) {
		tnix_input(tn);
	}
	if(tn->inbuf == NULLBUF) {
		if(tcb->rcvcnt > 0 &&
		   recv_tcp(tcb,&(tn->inbuf),0) > 0)
			tnix_input(tn);
	}
}

/* call when select has input from the pty */

tnix_ready(mask)
     unsigned long mask;
{

  int fd;

  for (fd = 0; mask; fd++, mask >>= 1)
    if (mask & 1) {
	if (tnix_fd2tcb[fd])
	    tnix_read(tnix_fd2tcb[fd]);
    }
}

tnix_read(tcb)
register struct tcb *tcb;
{
	extern void tnix_rmvscan();
	extern int send_tcp();

	register struct telnet *tn;
	register int i;

	if((tn = (struct telnet *)tcb->user) == NULLTN || tn->fd < 3) {
		/* Unknown connection - remove it from queue */
		log(tcb,"error Telnet - tnix_try (%d)", tn);
		tnix_rmvscan(tcb);
		return;
	}
	/*
	 * Next, check if there is any io for tcp:
	 */
	do {
		unsigned char c;

		if(tn->outbuf == NULLBUF &&
		   (tn->outbuf = alloc_mbuf(TURQSIZ)) == NULLBUF)
			return;		/* can't do much without a buffer */
	
#ifdef undef
		if(tn->outbuf->cnt < TURQSIZ) {
			if((i = read(tn->fd, tn->outbuf->data + tn->outbuf->cnt,
				(int)(TURQSIZ - tn->outbuf->cnt))) == -1) {
				if (errno == EAGAIN) return;
				log(tcb,"error Telunix - read (%d %d %d)",
					errno, tn->fd,
					TURQSIZ - tn->outbuf->cnt);
				close_tcp(tcb);
				return;
			}
#endif
		if(tn->outbuf->cnt < TURQSIZ - 1) {
		    i = 1;
		    while (i && tn->outbuf->cnt < TURQSIZ - 1) {
			if((i = read(tn->fd, &c, 1)) == -1) {
				if (errno == EAGAIN) break;
				log(tcb,"error Telunix - read (%d %d %d)",
					errno, tn->fd, 1);
				close_tcp(tcb);
				return;
			}
			if (i > 0) {
				if (c == IAC) {
				  tn->outbuf->data[tn->outbuf->cnt] = IAC;
				  tn->outbuf->data[tn->outbuf->cnt+1] = IAC;
				  i = 2;
				} else if (c == '\r') {
				  tn->outbuf->data[tn->outbuf->cnt] = '\r';
				  tn->outbuf->data[tn->outbuf->cnt+1] = 0;
				  i = 2;
				} else
				  tn->outbuf->data[tn->outbuf->cnt] = c;
			}
			tn->outbuf->cnt += i;
		    }
		    if(tn->outbuf->cnt < TURQSIZ - 1)
		        i = 0;	/* didn't fill buffer so don't retry */

		} else {
			i = -1;		/* any nonzero value will do */
		}
		if(tn->outbuf->cnt == 0)
			return;
		if(send_tcp(tcb,tn->outbuf) < 0) {
			log(tcb,"error Telunix - send_tcp (%d %d %d)",
				net_error, tn->fd, tn->outbuf->cnt);
			close_tcp(tcb);
			tn->outbuf = NULLBUF;
			return;
		}
		tn->outbuf = NULLBUF;
	} while(i);
/*
 * If we've already queued enough data, stop reading from pty, to exert
 * backpressure on application.  But we allow plenty of rope -- 2 MSS
 * worth -- in order to keep the data flow smooth.
 */
	if (tcb->sndcnt >= (tcb->snd.wnd + 2 * tcb->mss)) {
	  selmask &=  ~(1 << tn->fd);
	  tnixmask &=  ~(1 << tn->fd);
	}
}

/*
 * transmit done upcall.  The primary purpose of this routine is to
 * start reading from the pty again once enough data has been sent
 * over the network.
 */

tnix_xmt(tcb,cnt)
struct tcb *tcb;
int16 cnt;
{
	register struct telnet *tn;
	extern void tnix_rmvscan();
	extern int recv_tcp();
	extern void tnix_input();

	if((tn = (struct telnet *)tcb->user) == NULLTN || tn->fd < 3) {
		/* Unknown connection - remove it from queue */
		log(tcb,"error Telnet - tnix_try (%d)", tn);
		tnix_rmvscan(tcb);
		return;
	}

	/* if we had disabled reading, reconsider */

	if (! (tnixmask & (1 << tn->fd)))
	  if (tcb->sndcnt < (tcb->snd.wnd + 2 * tcb->mss)) {
	    selmask |= (1 << tn->fd);
	    tnixmask |= (1 << tn->fd);
	}
 }

/* Called by SIGCHLD handler to see if the process has died */

void
tnix_try(tcb,sesspid)
register struct tcb *tcb;
int sesspid;
{
	extern void tnix_rmvscan();
	extern int recv_tcp();
	extern int send_tcp();
	extern void tnix_input();

	register struct telnet *tn;
	register int i;

	if((tn = (struct telnet *)tcb->user) == NULLTN || tn->fd < 3) {
		/* Unknown connection - remove it from queue */
		log(tcb,"error Telnet - tnix_try (%d)", tn);
		tnix_rmvscan(tcb);
		return;
	}

	/* check if session process has died */
	{
		int sesspstat;
		if (wait4 (sesspid, &sesspstat, WNOHANG, NULL)) {
			struct utmp *ut, uu;
			int wtmp;

			selmask &=  ~(1 << tn->fd);
			tnixmask &=  ~(1 << tn->fd);
			tnix_fd2tcb[tn->fd] = NULL;

			utmpname (UTMP_FILE);
			setutent ();
			while (ut = getutent ()) {
				if (ut->ut_pid == sesspid) {
					memcpy (&uu, ut, sizeof (struct utmp));
					uu.ut_type = DEAD_PROCESS;
					time(&uu.ut_time);
/*					strcpy (uu.ut_user, "NONE"); */
					pututline (&uu);
					break;
				}
			}
			endutent ();
			if((wtmp = open(WTMP_FILE, O_APPEND|O_WRONLY)) >= 0) {
			  write(wtmp, (char *)&uu, sizeof(uu));
			  close(wtmp);
			}

			close_tcp (tcb);
			return;
		}
	}
}

/* Process incoming TELNET characters */
void
tnix_input(tn)
register struct telnet *tn;
{
	void dooptx(),dontoptx(),willoptx(),wontoptx(),answer();
	char *memchr();
	register int i;
	register struct mbuf *bp;
	char c;

	bp = tn->inbuf;

	/* Optimization for very common special case -- no special chars */
	if(tn->state == TS_DATA){
		while(bp != NULLBUF &&
 			memchr(bp->data,'\r',(int)bp->cnt) == NULLCHAR &&
			memchr(bp->data,IAC,(int)bp->cnt) == NULLCHAR) {
			if((i = write(tn->fd, bp->data, (int)bp->cnt)) == bp->cnt) {
				tn->inbuf = bp = free_mbuf(bp);
			} else if(i == -1) {
				log(tn->tcb,"error Telunix - write (%d %d %d)",
					errno, tn->fd, bp->cnt);
				close_tcp(tn->tcb);
				return;
			} else {
				bp->cnt -= i;
				bp->data += i;
				return;
			}
		}
		if(bp == NULLBUF)
			return;
	}
	while(pullup(&(tn->inbuf),&c,1) == 1){
		bp = tn->inbuf;
		switch(tn->state){
		case TS_CR:
			tn->state = TS_DATA;
			if (c == 0 || c == '\n') break; /* cr-nul or cr-nl */
		case TS_DATA:
			if(uchar(c) == IAC){
				tn->state = TS_IAC;
			} else if (uchar (c) == '\r') {
				tn->state = TS_CR;
				if(write(tn->fd, &c, 1) != 1) {
					/* we drop a character here */
					return;
				}
			} else {
#ifdef undef
/* yes, the standard says this, but nobody does it and it breaks things */
				if(!tn->remote[TN_TRANSMIT_BINARY])
					c &= 0x7f;
#endif
				if(write(tn->fd, &c, 1) != 1) {
					/* we drop a character here */
					return;
				}
			}
			break;
		case TS_IAC:
			switch(uchar(c)){
			case WILL:
				tn->state = TS_WILL;
				break;
			case WONT:
				tn->state = TS_WONT;
				break;
			case DO:
				tn->state = TS_DO;
				break;
			case DONT:
				tn->state = TS_DONT;
				break;
			case AYT:
				tn->state = TS_DATA;
				sndmsg (tn->tcb,"\r\n[ka9q here]\r\n");
				break;
			case IAC:
				if(write(tn->fd, &c, 1) != 1) {
					/* we drop a character here */
					return;
				}
				tn->state = TS_DATA;
				break;
			default:
				tn->state = TS_DATA;
				break;
			}
			break;
		case TS_WILL:
			willoptx(tn,c);
			tn->state = TS_DATA;
			break;
		case TS_WONT:
			wontoptx(tn,c);
			tn->state = TS_DATA;
			break;
		case TS_DO:
			dooptx(tn,c);
			tn->state = TS_DATA;
			break;
		case TS_DONT:
			dontoptx(tn,c);
			tn->state = TS_DATA;
			break;
		}
	}
}

/* Use our own copy.  It's too confusing to mix this with the client side */
void
willoptx(tn,opt)
struct telnet *tn;
char opt;
{
	int ack;
	void answer();

	if (debug_options) {
		fprintf(trfp, "[Recv: will ");
		if(uchar(opt) <= NOPTIONS)
			fprintf(trfp, "%s]\n",t_options[opt]);
		else
			fprintf(trfp, "%u]\n",opt);
	}
	
	switch(uchar(opt)){
	default:
		ack = DONT;	/* We don't know what he's offering; refuse */
	}
	answer(tn,ack,opt);
}
void
wontoptx(tn,opt)
struct telnet *tn;
char opt;
{
	void answer();

	if (debug_options) {
		fprintf(trfp, "[Recv: wont ");
		if(uchar(opt) <= NOPTIONS)
			fprintf(trfp, "%s]\n",t_options[opt]);
		else
			fprintf(trfp, "%u]\n",opt);
	}

	if(uchar(opt) <= NOPTIONS){
		if(tn->remote[uchar(opt)] == 0)
			return;		/* Already clear, ignore to prevent loop */
		tn->remote[uchar(opt)] = 0;
	}
	answer(tn,DONT,opt);	/* Must always accept */
}
void
dooptx(tn,opt)
struct telnet *tn;
char opt;
{
	void answer();
	int ack;

	if (debug_options) {
		fprintf(trfp, "[Recv: do ");
		if(uchar(opt) <= NOPTIONS)
			fprintf(trfp, "%s]\n",t_options[opt]);
		else
			fprintf(trfp, "%u]\n",opt);
	}

	switch(uchar(opt)){
/* in fact at the moment we always echo -- better fix this */
	case TN_ECHO:
		if(tn->local[uchar(opt)] == 1)
			return;		/* Already set, ignore to prevent loop */
		tn->local[uchar(opt)] = 1;
		ack = WILL;
		break;
	case TN_SUPPRESS_GA:
		if(tn->local[uchar(opt)] == 1)
			return;		/* Already set, ignore to prevent loop */
		tn->local[uchar(opt)] = 1;
		ack = WILL;
		break;
	default:
		ack = WONT;	/* Don't know what it is */
	}
	answer(tn,ack,opt);
}
void
dontoptx(tn,opt)
struct telnet *tn;
char opt;
{
	void answer();

	if (debug_options) {
		fprintf(trfp, "[Recv: dont ");
		if(uchar(opt) <= NOPTIONS)
			fprintf(trfp, "%s]\n",t_options[opt]);
		else
			fprintf(trfp, "%u]\n",opt);
	}

	if(uchar(opt) <= NOPTIONS){
		if(tn->local[uchar(opt)] == 0){
			/* Already clear, ignore to prevent loop */
			return;
		}
		tn->local[uchar(opt)] = 0;
	}
	answer(tn,WONT,opt);
}


/* This is the SIGCHLD trap handler */

void
tnix_scan()
{
	void tnix_try();
	register int i;

	for(i = 0; i < TUMAXSCAN; i += 1)
		if(tnixtcb[i] != NULLTCB)
			tnix_try(tnixtcb[i], sessionpid[i]);
}

int
tnix_addscan(tcb)
struct tcb *tcb;
{
	register int i;
	for(i = 0; i < TUMAXSCAN; i += 1)
		if(tnixtcb[i] == NULLTCB) {
			tnixtcb[i] = tcb;
			sessionpid[i] =  -1;
			return i;
		}
	return -1;
}

void
tnix_rmvscan(tcb)
struct tcb *tcb;
{
	register int i;

	for(i = 0; i < TUMAXSCAN; i += 1)
		if(tnixtcb[i] == tcb) {
			tnixtcb[i] = NULLTCB;
			if (sessionpid[i] > 0) kill (sessionpid[i], SIGHUP);
			sessionpid[i] = -1;
		}
}
