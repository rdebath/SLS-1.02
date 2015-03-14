/*
 * renamed loadavgd.c by Lenny Tropiano (lenny@icus.UUCP ICUS Software Systems)
 *
 *	ldavg.c -- compute load averages for System V
 *	Phil Budne @ Boston U / DSG
 *
 *	Forges BSD 4.2 rwhod packets containing system load averages
 *	(#ifdef RWHOD for this, else a shm segment is used, ftok("/unix", 'a'))
 */

# include <sys/types.h>			/* system types */
# include <sys/sysinfo.h>		/* sysinfo structure */
# include <sys/utsname.h>		/* for uname(2) */
# include <sys/stat.h>			/* for stat(2) */
# include <sys/param.h>			/* for HZ */
# include <stdio.h>
# include <nlist.h>
# include <time.h>
# include <math.h>
# include <utmp.h>
# include <fcntl.h>
#ifdef RWHOD
# include "rwhod.h"			/* (from BSD) */
#else  SHM
# include <sys/ipc.h>
# include <sys/shm.h>
#endif RWHOD

/* # define DEBUG /**/
#ifdef RWHOD
# define UDP 1
# define DSK 1
# define PMUL 100

# if UDP
# include "netdb.h"
unsigned short port = 513;
unsigned long ipaddr;
# endif
#endif RWHOD

extern struct utmp *getutent();

# define UNIX "/unix"
# define KMEM "/dev/kmem"

struct nlist nlsym[] = {
# define NL_SYSINFO 0
        { "sysinfo" },			/* 0 */
#ifdef RWHOD
# define NL_LBOLT 1
	{ "lbolt" },			/* 1 */
#endif RWHOD
        { 0 }
};

#ifdef RWHOD
struct whod proto;
struct utsname utsn;
char whopacket[100];
#else  SHM
key_t aven_key;
int aven_shm;
double *aven_seg;
#endif RWHOD
int fd, memfd;

char *unixsys = UNIX;
char *kmem = KMEM;
char *argv0;

main(argc, argv)
int  argc;
char *argv[];
{
	switch (fork()) {
	case -1:
		perror("fork");
		exit(1);
	case 0:
		break;
	default:
		exit(0);
	}
	argv0 = argv[0];
#ifdef RWHOD
	uname(&utsn);			/* get system names */
#endif RWHOD
	setpgrp();			/* create own pgrp */
	init_nlist();			/* get name values, open kmem */
	init_packet();			/* initialize packet prototype */
	doit();				/* never returns */
} /* main */

init_nlist() {
	nlist(unixsys, nlsym);		/* get system values */

        if(nlsym[NL_SYSINFO].n_value == 0) {
                fprintf(stderr, "%s: can't find sysinf structure\n", argv0);
                exit(1);
        } /* no value */

	if ((memfd = open(kmem, O_RDONLY)) < 0) {
		fprintf(stderr, "%s: no mem\n", argv0);
		exit(1);
	} /* could not open kmem */

} /* init_nlist */

# define PERIOD 5			/* sample period (in seconds) */
# define INTERVAL1 60			/* average interval 1 (in seconds) */
# define INTERVAL2 (5*60)		/* average interval 2 (in seconds) */
# define INTERVAL3 (15*60)		/* average interval 3 (in seconds) */
# define PACKINTERVAL 30		/* interval for make_packet */

doit() {
	struct sysinfo sinf;
	int packt = 0;
	long occ, que, nocc, nque, n, c;
	double avg1, avg2, avg3, new;
	double exp1, exp2, exp3;

	exp1 = exp( - ((double) PERIOD) / INTERVAL1 );
	exp2 = exp( - ((double) PERIOD) / INTERVAL2 );
	exp3 = exp( - ((double) PERIOD) / INTERVAL3 );

	getsysinf(&sinf);		/* prime the pump */
	occ = sinf.runocc;		/* number of samples */
	que = sinf.runque;		/* run queue summation */

	avg1 = avg2 = avg3 = ((double) que) / occ;

	for( ; ; ) {
		if( --packt < 0 ) {
#ifdef RWHOD
			make_packet((int) (avg1 * PMUL),
				    (int) (avg2 * PMUL),
				    (int) (avg3 * PMUL));
#else  SHM
			make_packet(avg1, avg2, avg3);
#endif RWHOD
			packt = PACKINTERVAL / PERIOD;
		} /* packet time */

/*		printf("runque: %ld  runocc: %ld\n", que, occ ); /**/

		sleep(PERIOD);
		getsysinf(&sinf);	/* get new info */
		nocc = sinf.runocc;
		nque = sinf.runque;

		n = nocc - occ;		/* get number of times updated */
		if( n <= 0 ) continue;
		c = nque - que - n;	/* get number of runners w/o us */
		if( c < 0 ) c = 0;	/* mumble! */

		new = ((double) c ) / n; /* new instantaneous avg */

		/************************************************/
		/*   The following formula is used to achieve   */
		/*   exponential decay of old measurements:	*/
		/*	avgN = avgN * expN  +  new * (1 - expN)	*/
		/*						*/
		/*   However, the factorized forms below	*/
		/*   require fewer floating point operations.	*/
		/************************************************/

		avg1 = ((avg1 - new) * exp1) + new;
		avg2 = ((avg2 - new) * exp2) + new;
		avg3 = ((avg3 - new) * exp3) + new;

		occ = nocc;
		que = nque;

	} /* for ever */
} /* doit */

getsysinf(s)
struct sysinfo *s;
{
	l_lseek(memfd, (long)nlsym[NL_SYSINFO].n_value, 0);
	r_read(memfd, (char *)s, sizeof(struct sysinfo));
}

/* lseek with error checking */
l_lseek(fd, offset, whence)
int fd, whence;
long	offset;
{
	if (lseek(fd, offset, whence) == -1) {
		fprintf(stderr, "%s: error on lseek\n", argv0);
		exit(1);
	}
}

/* read with error checking */
r_read (fd, buf, nbytes)
int	fd, nbytes;
char	*buf;
{
	if (read(fd, buf, nbytes) != nbytes) {
		fprintf(stderr, "%s: error on read\n", argv0);
		exit(1);
	}
}

init_packet() {
#ifdef RWHOD
	time_t boothz;
# if UDP
	struct hostent *he;

	he = gethostbyname( "localnet" );
	if( he == NULL || he->h_addr == 0 ) {
		fprintf(stderr, "no address: localnet\n");
		exit( 1 );
	}
	ipaddr = he->h_addr;
# endif
# if DSK
	sprintf(whopacket, "/usr/spool/rwho/whod.%s", utsn.nodename);
# endif
	memset(&proto, '\0', sizeof proto);	/* clear proto packet */

	strncat(proto.wd_hostname, utsn.nodename, 9); /* at most 9, add null */
	proto.wd_vers = WHODVERSION;
	proto.wd_type = WHODTYPE_STATUS;

	l_lseek(memfd, (long)nlsym[NL_LBOLT].n_value, 0);
	r_read(memfd, (char *)&boothz, sizeof( boothz ) );
	proto.wd_boottime = time(0) - (boothz / HZ);
#else  SHM
	if ((aven_key = ftok(UNIX, 'a')) == (key_t) -1) {
		perror(UNIX);
		exit(1);
	}
	if ((aven_shm = shmget(aven_key, 3 * sizeof (double), IPC_CREAT|IPC_EXCL|0644)) < 0) {
		perror("shmget");
		exit(1);
	}
	if ((int) (aven_seg = (double *) shmat(aven_shm, (char *) 0, 0)) == -1) {
		perror("shmat");
		if (shmctl(aven_shm, IPC_RMID, (struct shmid_ds *) 0) < 0)
			perror("shmctl(IPC_RMID)");
		exit(1);
	}
#endif RWHOD
	
} /* init_packet */

make_packet(iavg1, iavg2, iavg3)
#ifdef RWHOD
long iavg1, iavg2, iavg3;
#else  SHM
double iavg1, iavg2, iavg3;
#endif RWHOD
{
#ifdef RWHOD
	static struct whod packet;	/* local packet copy */
	register struct whoent *wep;	/* pointer to packet whoent */
	register struct utmp *utp;	/* return from getutent */
	int whof, cc;			/* output file, char count */

	packet = proto;			/* copy proto packet */
	time(&packet.wd_sendtime);
	time(&packet.wd_recvtime);	/* forge this !! */
	packet.wd_loadav[0] = iavg1;
	packet.wd_loadav[1] = iavg2;
	packet.wd_loadav[2] = iavg3;

	setutent();			/* open utmp file */
	wep = &packet.wd_we[0];		/* get pointer to first user in pkt */

	while( (utp = getutent()) != NULL ) {
	    if( (utp->ut_type == USER_PROCESS) && utp->ut_user[0]) {
		strncpy(wep->we_utmp.out_line, utp->ut_id, 4);
		wep->we_utmp.out_line[4] = '\0';

		strncpy(wep->we_utmp.out_name, utp->ut_user, 8);

		wep->we_utmp.out_time = utp->ut_time;

		wep->we_idle = idletime(utp);
		wep++;			/* bump packet pointer */
	    } /* user process */
	} /* while */
	endutent();

# if DSK
	whof = creat(whopacket, 0644);	/* open packt file */
	if( whof >= 0 ) {
	    cc = (char *)wep - (char *)&packet;
	    if( write(whof, (char *)&packet, cc) != cc )
	    	perror("write failed");
	    close(whof);
	} /* file opened */
	else perror(whopacket);
# endif
# if UDP
	cc = (char *)wep - (char *)&packet;
	udpsend( (char *)&packet, cc, ipaddr, port, port, 1);
# endif
# ifdef DEBUG
	fprintf(stderr, "wrote packet (%d)\n", cc);
	fflush(stderr);
# endif
#else  SHM
	aven_seg[0] = iavg1;
	aven_seg[1] = iavg2;
	aven_seg[2] = iavg3;
#endif RWHOD
} /* make_packet */

#ifdef RWHOD
idletime(up)
struct utmp *up;
{
    register int i;
    register char *cp, *dp;
    char ttyname[10];
    struct stat buf;
    time_t now;

    cp = "/dev/";
    dp = ttyname;
    
    while( *cp != '\0' )		/* copy "/dev/" */
        *dp++ = *cp++;

    cp = up->ut_line;			/* get line name */
    if( *cp == 's' )			/* starts with an 's'? (sxtnnn) */
        *dp++ = 'v';			/* insert a 'v' */

    for( i = 0; i < 8; i++ )		/* copy line name */
        if( (*dp++ = *cp++) == '\0' ) break;	/* or until null */

    if( stat(ttyname, &buf) != 0 )	/* get file status */
        return( 0 );

    time(&now);				/* get current time */
    i = now - buf.st_atime;		/* get differnce from last acces */
    return( i );			/* return idle time */
} /* idletime */
#endif RWHOD
