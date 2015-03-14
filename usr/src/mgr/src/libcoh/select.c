/* select from Udo Munk, udo@umunk.GUN.de ; implements select() using poll() */

#include <poll.h>
#include <sys/param.h>
#include <sys/select.h>
#include <sys/time.h>

#if (NOFILE < FD_SETSIZE)
#define FD_MAX	NOFILE
#else
#define FD_MAX	FD_SETSIZE
#endif

extern int errno;

/*
 * Simulate BSD select() system call with poll().
 */
int
select(nfd, rfds, wfds, xfds, to)
int nfd;
fd_set *rfds, *wfds, *xfds;
struct timeval * to;
{
	int timeout;
	struct pollfd pfd[FD_MAX];
	int fd;
	struct pollfd * pfp;
	int ret;
	int i, npfd, events;
	int npoll;

	if (nfd > FD_MAX) /* should report an error here */
		nfd = FD_MAX;

	/*
	 * "to" is NULL - blocking poll
	 * "to" is zero - return immediately
	 */
	if (to) {
		timeout = to->tv_sec*1000 + to->tv_usec/1000;
	} else
		timeout = -1;

	/*
	 * Set fd and events fields in pfd.
	 */
	for (fd = 0, pfp = pfd, npfd = 0; fd < nfd; fd++) {
		events = 0;
		if (rfds && FD_ISSET(fd, rfds))
			events |= POLLIN;
		if (wfds && FD_ISSET(fd, wfds))
			events |= POLLOUT;
		if (events) {
			pfp->events = events;
			pfp->fd = fd;
			npfd++;
			pfp++;
		}
	}

	/*
	 * Do a poll() system call.
	 */
	if ((npoll=poll(pfd, npfd, timeout)) == -1) {
		ret = -1;
		goto done;
	}

	/*
	 * Set return value and return bits in rfds and wfds.
	 * Punt on xfds.
	 */
	if (rfds)
		FD_ZERO(rfds);
	if (wfds)
		FD_ZERO(wfds);
	if (xfds)
		FD_ZERO(xfds);
	ret = 0;
	for (i = 0, pfp = pfd; i < npfd; i++, pfp++) {
		fd = pfp->fd;
		if (fd >= 0 && fd < nfd) {
			if (rfds && (pfp->revents & POLLIN)) {
				FD_SET(fd, rfds);
				ret++;
			}
			if (wfds && (pfp->revents & POLLOUT)) {
				FD_SET(fd, wfds);
				ret++;
			}
		}
	}

	/*
	 * Bye.
	 */
done:
	return ret;
}
