/* UNFSD - copyright Mark A Shand, May 1988.
 * This software maybe be used for any purpose provided
 * the above copyright notice is retained.  It is supplied
 * as is, with no warranty expressed or implied.
 */

#include <rpc/rpc.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <pwd.h>
#include <grp.h>
#include <signal.h>
#include "ugid.h"

static struct timeval TIMEOUT = { 25, 0 };

#define	DAEMON_IDLE_LIMIT	(5*60)

static int	idle_limit = 0;
extern	int	errno;

int
run_mode_from_args(argc, argv)
int	argc;
char	**argv;
{
	int	i;

	for (i = 1; i < argc && argv[i][0] == '-'; i++)
		if (argv[i][1] == 'd')
		{
#ifndef DEBUG
			int fd;

			if (fork())
				exit(0);
			close(0);
			close(1);
			close(2);
			if ((fd = open("/dev/tty", 2)) >= 0)
			{
				ioctl(fd, TIOCNOTTY, (char *)0);
				(void) close(fd);
			}
#endif DEBUG
			return RPC_ANYSOCK;
		}
	/* assume run from inetd */
	idle_limit = DAEMON_IDLE_LIMIT;
	alarm(idle_limit);
	return 0;
}

int *
authenticate_1(argp, rqstp)
	int *argp;
	struct svc_req *rqstp;
{
	static int res;
	int	s;
	struct sockaddr_in	sendaddr, destaddr;
	int	da_len;
	int	dummy;
	short	lport;

	bzero(&res, sizeof res);
	destaddr = *svc_getcaller(rqstp->rq_xprt);
	destaddr.sin_port = htons(*argp);
	if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
		goto bad;
	setsockopt(s, SOL_SOCKET, SO_LINGER, 0, 0);
	bzero((char *) &sendaddr, sizeof sendaddr);
	/* find a reserved port */
	lport = IPPORT_RESERVED - 1;
	sendaddr.sin_family = AF_INET;
	sendaddr.sin_addr.s_addr = INADDR_ANY;
	for (;;)
	{
		sendaddr.sin_port = htons((u_short)lport);
		if (bind(s, &sendaddr, sizeof sendaddr) >= 0)
			break;
		if (errno != EADDRINUSE && EADDRNOTAVAIL)
			goto bad;
		lport--;
		if (lport <= IPPORT_RESERVED / 2)
			/* give up */
			break;
	}
	if (sendto(s, &dummy, sizeof dummy, 0, &destaddr, sizeof destaddr) < 0)
		goto bad;

	close(s);
	res = 0;
	return (&res);
    bad:
	close(s);
	res = errno == 0 ? -1 : errno;
	return (&res);
}

int *
name_uid_1(argp, rqstp)
	ugname *argp;
	struct svc_req *rqstp;
{
	static int res;
	struct passwd	*pw;

	bzero(&res, sizeof(res));
	alarm(idle_limit);
	if ((pw = getpwnam(*argp)) == NULL)
		res = NOBODY;
	else
		res = pw->pw_uid;

	return (&res);
}


int *
group_gid_1(argp, rqstp)
	ugname *argp;
	struct svc_req *rqstp;
{
	static int res;
	struct group	*gr;

	bzero(&res, sizeof(res));
	alarm(idle_limit);
	if ((gr = getgrnam(*argp)) == NULL)
		res = NOBODY;
	else
		res = gr->gr_gid;

	return (&res);
}


ugname *
uid_name_1(argp, rqstp)
	int *argp;
	struct svc_req *rqstp;
{
	static ugname res;
	struct passwd	*pw;

	bzero(&res, sizeof(res));
	alarm(idle_limit);
	if ((pw = getpwuid(*argp)) == NULL)
		res = NULL;
	else
		res = pw->pw_name;

	return (&res);
}


ugname *
gid_group_1(argp, rqstp)
	int *argp;
	struct svc_req *rqstp;
{
	static ugname res;
	struct group	*gr;

	bzero(&res, sizeof(res));
	alarm(idle_limit);
	if ((gr = getgrgid(*argp)) == NULL)
		res = NULL;
	else
		res = gr->gr_name;

	return (&res);
}
