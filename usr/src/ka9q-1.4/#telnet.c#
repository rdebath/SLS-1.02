#include <stdio.h>
#include "config.h"
#include "global.h"
#include "mbuf.h"
#include "timer.h"
#include "icmp.h"
#include "netuser.h"
#include "tcp.h"
#include "telnet.h"
#include "session.h"
#include "ftp.h"
#include "iface.h"
#include "ax25.h"
#include "lapb.h"
#include "finger.h"
#include "nr4.h"
#ifdef	UNIX
#include <string.h>
#endif

#define	CTLZ	26

#define debug printf

extern char nospace[];
extern char badhost[];
int refuse_echo = 0;
int unix_line_mode = 0;    /* if true turn <cr> to <nl> when in line mode */
int debug_options = 0;
extern FILE *trfp;            /* file pointer used for tracing */

/*
 * Two separate I/O packages are being used here.
 * Input is done by ttydriv, which is called from the main loop
 *   (main for connections from the main program, or telserv for
 *   connections from a telnet client).  ttydriv has its own
 *   data structures, which are allocated by open_remote_tty
 *   for connections from a telnet client.  tn->inbuf is set
 *   to a pointer to the ttydriv data structure.  For input in the
 *   main program, this is NULL.  ttydriv transforms references to
 *   NULL into a static structure that describes primary input.
 * Output is done by a mini-stdio package included in this module.
 *   This is used instead of real stdio, because real stdio has no
 *   way to clear the output buffer.  Also, this is almost certainly
 *   a lot faster.  The data structure for this is allocated by
 *   tnalloc.  tn->outbuf is set to a pointer to that data structure.
 *   &std_tnbuf is used for the main program's stdout.
 * tn->client is set for connections from a telnet client.  It's
 *   actually the fd of a pipe to the client, but that is private
 *   data used only by telserv.  If tn->client
 *   is set telserv_close must be called to clean up telserv's data.
 * Either tn->client or tn->inbuf can be used to see whether the
 *   connection is in the main program or a telnet client.
 */

/* 
 * stdio has no way to flush.  So do our own.
 */

/* might as well make the buffer big enough for an Ethernet packet */
#define TBSIZE 2048
struct tnbuf {
  int fd;
  char *termbuf;
  char *termbufp;
  char *termbufe;
};

char std_termbuf[TBSIZE];

struct tnbuf std_tnbuf = {1, std_termbuf, std_termbuf, std_termbuf+TBSIZE};

struct mbuf *
tnalloc(fd)
  int fd;
{
  struct tnbuf *tb;

  tb = (struct tnbuf *)malloc(sizeof(struct tnbuf));
  if (! tb)
    return NULL;
  tb->termbuf = malloc(TBSIZE);
  if (!tb->termbuf) {
    free(tb);
    return NULL;
  }

  tb->fd = fd;
  tb->termbufp = tb->termbuf;
  tb->termbufe = tb->termbuf + TBSIZE;
  return (struct mbuf *)tb;
}

tnfree(tbi)
  struct mbuf *tbi;
{
  struct tnbuf *tb = (struct tnbuf *)tbi;
  if (!tb)
    return;

  free(tb->termbuf);
  free(tb);
}

int
tnputchar(c, tbi)
int c;
struct mbuf *tbi;
{
  struct tnbuf *tb = (struct tnbuf *)tbi;

  if (tb->termbufp >= tb->termbufe) {
    write(tb->fd, tb->termbuf, TBSIZE);
    tb->termbufp = tb->termbuf;
  }
  *(tb->termbufp)++ = c;
}

/* send out buffered data */
int
tnflush(tbi)
struct mbuf *tbi;
{
  struct tnbuf *tb = (struct tnbuf *)tbi;

  if (tb->termbufp > tb->termbuf)
    write(tb->fd, tb->termbuf, tb->termbufp - tb->termbuf);
  tb->termbufp = tb->termbuf;
}

/* clear output -- should also do clearout to do Unix clear */
int
tnclear(tbi)
struct mbuf *tbi;
{
  struct tnbuf *tb = (struct tnbuf *)tbi;

  tb->termbufp = tb->termbuf;
}

char *t_options[] = {
	"BINARY", "ECHO", "RCP", "SGA",	"NAMS",	"STATUS", "TM",
	"RCTE", "NAOL", "NAOP", "NAOCRD", "NAOHTS", "NAOHTD", "NAOFFD", 
	"NAOVTS", "NAOVTD", "NAOLFD", "XASCII", "LOGOUT", "BM", "DET", 
	"SUPDUP", "SUPDUPOUOT", "SNDLOC", "TTYPE", "EOR", "TACACS", 
	"OUTMARK", "LOCNUM", "TN3270", "X3PAD", "NAWS", "TSPEED",
	"LFLOW", "LINEMODE", "XDISPLOC"
};

/* Execute user telnet command */
int
dotelnet(argc,argv)
int argc;
char *argv[];
{
  struct telnet *tn;
  
  tn = dotelnet_subr(argc, argv, 0);
  if (tn)
    return 0;
  else
    return 1;
}

struct telnet *
dotelnet_subr(argc,argv,tty)
int argc;
char *argv[];
char *tty;
{
	void t_state(),rcv_char(),tn_tx();
	char *inet_ntoa();
	int32 resolve();
	int send_tel();
        int unix_send_tel();
	int send_rem_tel();
        int unix_send_rem_tel();
	struct session *s = NULL;
	struct telnet *tn;
	struct tcb *tcb;
	struct socket lsocket,fsocket;

	lsocket.address = ip_addr;
	lsocket.port = lport++;
	if((fsocket.address = resolve(argv[1])) == 0){
		printf(badhost,argv[1]);
		return NULL;
	}
	if(argc < 3)
		fsocket.port = TELNET_PORT;
	else
		fsocket.port = atoi(argv[2]);

	if (! tty) {
	  /* Allocate a session descriptor */
	  if((s = newsession()) == NULLSESSION){
		printf("Too many sessions\n");
		return NULL;
	  }
	  if((s->name = malloc((unsigned)strlen(argv[1])+1)) != NULLCHAR)
	    strcpy(s->name,argv[1]);
	  s->type = TELNET;
	  if ((refuse_echo == 0) && (unix_line_mode != 0)) {
	    s->parse = unix_send_tel;
	  } else {
	    s->parse = send_tel;
	  }
	  current = s;
	}

	/* Create and initialize a Telnet protocol descriptor */
	if((tn = (struct telnet *)calloc(1,sizeof(struct telnet))) == NULLTN){
		printf(nospace);
		if (s)
		  s->type = FREE;
		return NULL;
	}

	if (tty) {
	  tn->inbuf = (struct mbuf *)open_remote_tty(tty);
	  if (!tn->inbuf) {
	    printf(nospace);
	    free(tn);
	    return NULL;
	  }
	  tn->fd = remote_tty_fd(tn->inbuf);
	  tn->outbuf = tnalloc(tn->fd);
	  if (!tn->outbuf) {
	    close_remote_tty(tn->inbuf);
	    free(tn);
	    printf(nospace);
	    return NULL;
	  }
	  if ((refuse_echo == 0) && (unix_line_mode != 0)) {
	    tn->parse = unix_send_rem_tel;
	  } else {
	    tn->parse = send_rem_tel;
	  }
	} else {
	  tn->outbuf = (struct mbuf *)&std_tnbuf;
	  tn->fd = 0;
	}

	tn->session = s;	/* Upward pointer */
	tn->state = TS_DATA;
	if (s)
	  s->cb.telnet = tn;	/* Downward pointer */

	tcb = open_tcp(&lsocket,&fsocket,TCP_ACTIVE,0,
	 rcv_char,tn_tx,t_state,0,(char *)tn);

	tn->tcb = tcb;	/* Downward pointer */
	if (! tty)
	  go_mode();
	return tn;
}

/* Process typed characters */
int
unix_send_tel(buf,n)
char *buf;
int16 n;
{
	int i;

	for (i=0; (i<n) && (buf[i] != '\r'); i++)
		;
	if (buf[i] == '\r') {
		buf[i] = '\n';
		n = i+1;
	}
	send_tel(buf,n);
}
int
send_tel(buf,n)
char *buf;
int16 n;
{
	struct mbuf *bp,*qdata();
	if(current == NULLSESSION || current->cb.telnet == NULLTN
	 || current->cb.telnet->tcb == NULLTCB)
		return;
	/* If we're doing our own echoing and recording is enabled, record it */
	if(!current->cb.telnet->remote[TN_ECHO] && current->record != NULLFILE)
		fwrite(buf,1,(int)n,current->record);
	bp = qdata(buf,n);
	send_tcp(current->cb.telnet->tcb,bp);
}

/* Process typed characters */
int
unix_send_rem_tel(buf,n,tn)
char *buf;
int16 n;
struct telnet *tn;
{
	int i;

	for (i=0; (i<n) && (buf[i] != '\r'); i++)
		;
	if (buf[i] == '\r') {
		buf[i] = '\n';
		n = i+1;
	}
	send_rem_tel(buf,n,tn);
}
int
send_rem_tel(buf,n,tn)
char *buf;
int16 n;
struct telnet *tn;
{
	struct mbuf *bp,*qdata();

	bp = qdata(buf,n);
	send_tcp(tn->tcb,bp);
}

/* set up correct tty modes */
int
tel_setterm(tn)
register struct telnet *tn;
{
	if(tn->remote[TN_ECHO]) {
		raw(tn->inbuf);	/* other end is echoing, we're raw */
		if (tn->lflow)
			flowon(tn->fd);
		else
			flowoff(tn->fd);
	} else {
		cooked(tn->inbuf);
		flowdefault(tn->fd, tn->inbuf);
	}
}

/* Process incoming TELNET characters */
int
tel_input(tn,bp)
register struct telnet *tn;
struct mbuf *bp;
{
	char c;
	void doopt(),dontopt(),willopt(),wontopt(),answer();
	FILE *record;
	char *memchr();

	/* Optimization for very common special case -- no special chars */
	if(tn->state == TS_DATA){
		while(bp != NULLBUF && memchr(bp->data,IAC,(int)bp->cnt) == NULLCHAR){
			if (! tn->outsup) {
				if(tn->session && 
				   (record = tn->session->record) != NULLFILE)
					fwrite(bp->data,1,(int)bp->cnt,record);
				while(bp->cnt-- != 0)
					tnputchar(*bp->data++, tn->outbuf);
			}
			bp = free_mbuf(bp);
		}
	}
	while(pullup(&bp,&c,1) == 1){
		switch(tn->state){
		case TS_DATA:
			if(uchar(c) == IAC){
				tn->state = TS_IAC;
			} else {
#ifdef undef
				if(!tn->remote[TN_TRANSMIT_BINARY])
					c &= 0x7f;
#endif
				if (! tn->outsup) {
					tnputchar(c, tn->outbuf);
					if(tn->session && 
					   (record = tn->session->record) 
					   != NULLFILE)
						putc(c,record);
				}
			}
			break;
		case TS_IAC:
process_iac:
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
			case TN_DM:
				/*
				 * if outsup > 1, we are still in urgent
				 * data, so DM is ignored
				 */
				if (tn->outsup == 1) {
					tn->outsup = 0;
					if (debug_options)
						fprintf(trfp, "[End of urgent data]\n");
				}
				tn->state = TS_DATA;
				break;
			case SB:
				SB_CLEAR();
				tn->state = TS_SB;
				break;
			case IAC:
				tnputchar(c, tn->outbuf);
				tn->state = TS_DATA;
				break;
			default:
				tn->state = TS_DATA;
				break;
			}
			break;
		case TS_WILL:
			willopt(tn,c);
			tn->state = TS_DATA;
			break;
		case TS_WONT:
			wontopt(tn,c);
			tn->state = TS_DATA;
			break;
		case TS_DO:
			doopt(tn,c);
			tn->state = TS_DATA;
			break;
		case TS_DONT:
			dontopt(tn,c);
			tn->state = TS_DATA;
			break;
	        case TS_SB:
			if (uchar(c) == IAC) {
				tn->state = TS_SE;
				break;
			} else {
				SB_ACCUM(c);
			}
			break;
		case TS_SE:
			if (uchar(c) != SE) {
			  if (uchar(c) != IAC) {
			    /*
			     * This is an error.  We only expect to get
			     * "IAC IAC" or "IAC SE".  Several things may
			     * have happend.  An IAC was not doubled, the
			     * IAC SE was left off, or another option got
			     * inserted into the suboption are all possibilities.
			     * If we assume that the IAC was not doubled,
			     * and really the IAC SE was left off, we could
			     * get into an infinate loop here.  So, instead,
			     * we terminate the suboption, and process the
			     * partial suboption if we can.
			     */
			    SB_ACCUM(IAC);
			    SB_ACCUM(c);
			    tn->subpointer -= 2;
			    SB_TERM();
			    suboption(tn);	/* handle sub-option */
			    tn->state = TS_IAC;
			    goto process_iac;
			  }
			  SB_ACCUM(c);
			  tn->state = TS_SB;
			} else {
			  SB_ACCUM(IAC);
			  SB_ACCUM(SE);
			  tn->subpointer -= 2;
			  SB_TERM();
			  suboption(tn);	/* handle sub-option */
			  tn->state = TS_DATA;
			}
			break;
		}
	}
}

/* Telnet receiver upcall routine */
void
rcv_char(tcb,cnt)
register struct tcb *tcb;
int16 cnt;
{
	struct mbuf *bp;
	struct telnet *tn;
	FILE *record;
#ifdef	FLOW
	extern int ttyflow;
#endif
	if((tn = (struct telnet *)tcb->user) == NULLTN){
		/* Unknown connection; ignore it */
		return;
	}
	/* Hold output if we're not the current session */
	if(!tn->inbuf &&
	   (mode != CONV_MODE || current == NULLSESSION
#ifdef	FLOW
	    || !ttyflow	/* Or if blocked by keyboard input -- hyc */
#endif
	    || current->type != TELNET || current->cb.telnet != tn)) {
	 	if (! tn->warned &&
		    (mode != CONV_MODE || current == NULLSESSION
		     || current->type != TELNET || current->cb.telnet != tn)) {
			char buffer[50];
			sprintf(buffer, "\r\n[Telnet: Input from session %d]\r\n",
				(int)(tn->session - sessions));
			write(1, buffer, strlen(buffer));
			tn->warned = 1;
		      }
		return;
	 }

/*
 * Urgent data is done by combination of the TCP level and this code.
 * The TCP level sets URGCUR and tcb->up (urgent pointer), but we
 * clear it at this level.  We want to clear it when we've taken
 * data beyond the urgent pointer, and only here are we in a position
 * to control that.  The code is better here anyway, because other
 * applications may treat the urgent pointer differently.  Rlogin
 * for example, would have rather different code.  This implementation
 * is a layering violation, but lets us get very good results.
 */
	if (tcb->flags & URGCUR) {
		/* we are in urgent data */
		int32 start = tcb->rcv.nxt - tcb->rcvcnt;
		int32 count;

/*
 * we use rcv.up - 2  as the end of urgent data so that the DM (which
 * is 2 characters isn't included.  Actually one could argue that it should
 * be - 1 because of the modification to the definition of urgent 
 * introduced in the host requirements RFC and other official modifications,
 * but - 2 is safe.  Note that outsup == 2 indicates that we're still in
 * urgent data, so we should ignore any DM.
 */
		if (seq_gt (tcb->rcv.up - 2, start)) {
			/* 
			 * at least some data in this segment is urgent.
			 * clear output and set outsup to suppress output.
			 */
			tnclear(tn->outbuf);
			clearout(tn->fd);
			tn->outsup = 2;
			/* see how much data is urgent */
			count = (tcb->rcv.up - 2) - start;
			if (count > cnt)
				count = cnt;
			if(recv_tcp(tcb,&bp,count) > 0) { /* get from TCP */
				tel_input(tn,bp);  /* process through telnet */
				/* any data beyond urgent? */
				cnt -= count;
				if (cnt > 0) {
					/*
					 * say we're no longer in urgent
					 * data.  outsup 1 means to suppress
					 * output until next DM.
					 */
					tcb->flags &= ~URGCUR;
					tn->outsup = 1;
					if(recv_tcp(tcb,&bp,cnt) > 0)
						tel_input(tn,bp);
				}
			}
		} else {
			/* 
			 * no more urgent data.  Clear urgent flag and
			 * set so suppress output stops at next DM
			 */
			tcb->flags &= ~URGCUR;
			tn->outsup = 1;
			if(recv_tcp(tcb,&bp,cnt) > 0)
				tel_input(tn,bp);
	        }
	} else {
	/* normal (no urgent data) case */
	if(recv_tcp(tcb,&bp,cnt) > 0)
		tel_input(tn,bp);
        }

	tnflush(tn->outbuf);  /* flush tty output buffer */
	if(tn->session && (record = tn->session->record) != NULLFILE)
		fflush(record);
}
/* Handle transmit upcalls. Used only for file uploading */
void
tn_tx(tcb,cnt)
struct tcb *tcb;
int16 cnt;
{
	struct telnet *tn;
	struct session *s;
	struct mbuf *bp;
	int size;

	if((tn = (struct telnet *)tcb->user) == NULLTN
	 || (s = tn->session) == NULLSESSION
	 || s->upload == NULLFILE)
		return;
	if((bp = alloc_mbuf(cnt)) == NULLBUF)
		return;
	if((size = fread(bp->data,1,(int)cnt,s->upload)) > 0){
		bp->cnt = (int16)size;
		send_tcp(tcb,bp);
	} else {
		free_p(bp);
	}
	if(size != cnt){
		/* Error or end-of-file */
		fclose(s->upload);
		s->upload = NULLFILE;
		free(s->ufile);
		s->ufile = NULLCHAR;
	}
}

/* State change upcall routine */
/*ARGSUSED*/
void
t_state(tcb,old,new)
register struct tcb *tcb;
char old,new;
{
	struct telnet *tn;
	char notify = 0;
	extern char *tcpstates[];
	extern char *reasons[];
	extern char *unreach[];
	extern char *exceed[];

	/* Can't add a check for unknown connection here, it would loop
	 * on a close upcall! We're just careful later on.
	 */
	tn = (struct telnet *)tcb->user;

	if(current != NULLSESSION && current->type == TELNET && current->cb.telnet == tn)
	{
		notify = 1;
		cooked(tn->inbuf);	/* prettify things... -- hyc */
	}

	switch(new){
	case CLOSE_WAIT:
		if(notify)
			printf("%s\n",tcpstates[new]);
		close_tcp(tcb);
		break;
	case CLOSED:	/* court adjourned */
		if(notify){
			printf("%s (%s",tcpstates[new],reasons[tcb->reason]);
			if(tcb->reason == NETWORK){
				switch(tcb->type){
				case DEST_UNREACH:
					printf(": %s unreachable",unreach[tcb->code]);
					break;
				case TIME_EXCEED:
					printf(": %s time exceeded",exceed[tcb->code]);
					break;
				}
			}
			printf(")\n");
			cmdmode();
		}
		del_tcp(tcb);
		if(tn != NULLTN) {
			free_telnet(tn);
			tn = NULL;
		}
		break;
	default:
		if(notify)
			printf("%s\n",tcpstates[new]);
		break;
	}
	/* not sure which of these is needed */
	if (tn)
	  tnflush(tn->outbuf);  
	fflush(stdout);
}
/* Delete telnet structure */

free_telnet(tn)
struct telnet *tn;
{
	if(tn->session != NULLSESSION)
		freesession(tn->session);

/*
 * must do close_remote_tty before telserv_close, so we get tty modes
 * back before we tell the remote program to exit
 */
	if(tn->inbuf)
	  close_remote_tty(tn->inbuf);

	if(tn->client)
	  telserv_close(tn);

	if(tn->outbuf != (struct mbuf *)&std_tnbuf)
	  tnfree(tn->outbuf);

	if(tn != NULLTN)
		free((char *)tn);

}

/* The guts of the actual Telnet protocol: negotiating options */
void
willopt(tn,opt)
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
	case TN_TRANSMIT_BINARY:
	case TN_ECHO:
	case TN_SUPPRESS_GA:
		if(tn->remote[uchar(opt)] == 1)
			return;		/* Already set, ignore to prevent loop */
		if(uchar(opt) == TN_ECHO){
			if(refuse_echo){
				/* User doesn't want to accept */
				ack = DONT;
				break;
			} else {
				/* must set before calling tel_setterm */
				tn->remote[uchar(opt)] = 1;
				tel_setterm(tn); /* raw mode etc. */
			}
		} else
			tn->remote[uchar(opt)] = 1;
		ack = DO;			
		break;
	default:
		ack = DONT;	/* We don't know what he's offering; refuse */
	}
	answer(tn,ack,opt);
}
void
wontopt(tn,opt)
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
		if(uchar(opt) == TN_ECHO)
			tel_setterm(tn);  /* cooked mode, etc. */
	}
	answer(tn,DONT,opt);	/* Must always accept */
}
void
doopt(tn,opt)
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
	case TN_TTYPE:
		if(tn->local[uchar(opt)] == 1)
			return;		/* Already set, ignore to prevent loop */
		if (tn->client ? tn->termtype : getenv("TERM")) {
		  tn->local[uchar(opt)] = 1;
		  ack = WILL;
		} else
		  ack = WONT;
		break;

	case TN_LFLOW:
		if(tn->local[uchar(opt)] == 1)
			return;		/* Already set, ignore to prevent loop */
		tn->local[uchar(opt)] = 1;
		tn->lflow = 1;  /* protocol says initialize to known state */
		tel_setterm(tn);  /* enable flow */
		ack = WILL;
		break;
	default:
		ack = WONT;	/* Don't know what it is */
	}
	answer(tn,ack,opt);
}
void
dontopt(tn,opt)
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

void
suboption(tn)
register struct telnet *tn;
{
   if (debug_options) {
        int opt = SB_PEEK();
  	fprintf(trfp, "[Recv: subopt ");
		if(opt <= NOPTIONS)
			fprintf(trfp, "%s]\n",t_options[opt]);
		else
			fprintf(trfp, "%u]\n",opt);
    }

    switch (SB_GET()) {
    case TN_LFLOW:
	if (tn->local[TN_LFLOW] == 0)
	    return;
	if (SB_EOF())
	    return;
	switch(SB_GET()) {
	case 1:
	    tn->lflow = 1;
	    break;
	case 0:
	    tn->lflow = 0;
	    break;
	default:
	    return;
	}
	tel_setterm(tn);  /* get flow control right */
	break;
    case TN_TTYPE:
	if (tn->local[TN_TTYPE] == 0)
	    return;
	if (SB_EOF() || SB_GET() != 1) {
	    return;
	} else {
	    char *name;
	    unsigned char temp[50];
	    int len;
	    struct mbuf *bp,*qdata();

	    if (tn->client)
	        name = tn->termtype;
	    else
	        name = getenv("TERM");
	    len = strlen(name) + 4 + 2;
	    if (len < sizeof(temp)) {
		sprintf((char *)temp, "%c%c%c%c%s%c%c", IAC, SB, TN_TTYPE,
				0, name, IAC, SE);
		bp = qdata(temp,len);
		send_tcp(tn->tcb,bp);
		if (debug_options)
		  fprintf(trfp, "[Send: term %s]\n", name);
	    } else
	        return;
	}
	break;

    default:
	break;
    }
}

void
answer(tn,r1,r2)
struct telnet *tn;
int r1,r2;
{
	struct mbuf *bp,*qdata();
	char s[3];

	if (debug_options) {
	  switch(r1){
	  case WILL:
	    fprintf(trfp, "[Sent: will ");
	    break;
	  case WONT:
	    fprintf(trfp, "[Sent: wont ");
	    break;
	  case DO:
	    fprintf(trfp, "[Sent: do ");
	    break;
	  case DONT:
	    fprintf(trfp, "[Sent: dont ");
	    break;
	  }
	  if(uchar(r2) <= NOPTIONS)
	    fprintf(trfp, "%s]\n",t_options[r2]);
	  else
	    fprintf(trfp, "%u]\n",r2);
	}

	s[0] = IAC;
	s[1] = r1;
	s[2] = r2;
	bp = qdata(s,(int16)3);
	send_tcp(tn->tcb,bp);
}
