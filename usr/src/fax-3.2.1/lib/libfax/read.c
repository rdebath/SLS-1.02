/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <fcntl.h>

#include "log.h"
#include "read.h"

static int fds[2];
static int init = 0;

static void timeout_handler()
{
    char dummy;

    write(fds[0], &dummy, 1);
}

static int timeout_init()
{
    int flags;

    /*
     * The first part of the pair is for writing.  The second
     * is for reading.  This is used to make alarm signal
     * syncronous.  Thanks to Andy for this idea.
     */
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) < 0) {
	log(L_EMERG, "can't make socketpair: %m");
	return (-1);
    }

    /*
     * We make this non-blocking to make it easy to clear out
     * any pending reads.
     */
    flags = fcntl(fds[1], F_GETFL, 0);
    flags |= O_NDELAY;
    if (fcntl(fds[1], F_SETFL, flags) < 0) {
	log(L_EMERG, "can't set nonblocking: %m");
	return (-1);
    }

    signal(SIGALRM, timeout_handler);
    
    init = 1;

    return (0);
}

static void timeout_set(seconds)
     int seconds;
{
    char buf[128];

    /*
     * Clear timeout.  This will work because we have set non-blocking 
     * on this fd.
     */
    read(fds[1], buf, sizeof(buf));

    alarm(seconds);
}

/*
 * Read from fd into a buf with maxlength bufsize.  Return when
 * a minimum of minsize bytes have been read.  Wait a maximum
 * of seconds seconds before timing out.  Return 0 if a timeout
 * occured.  Return -1 if the read failed.  In other cases,
 * return the total bytes read.
 */
static int read_internal(fd, buf, bufsize)
     int fd;
     char *buf;
     int bufsize;
{
    fd_set readfds;

    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    FD_SET(fds[1], &readfds);

    switch (select(32, &readfds, NULL, NULL, NULL)) {
      case 0:
	log(L_ERR, "unexpect select return");
	return (-1);
      case -1:
	if (errno == EINTR) {
	    log(L_ERR, "read timeout");
	    return (0);
	} else {
	    log(L_ERR, "select error: %m");
	    return (-1);
	}
      default:
	if (FD_ISSET(fds[1], &readfds)) {
	    char dummy[128];
	    read(fds[1], dummy, sizeof(dummy));
	    log(L_ERR, "read timeout");
	    return (0);
	} else if (FD_ISSET(fd, &readfds)) {
	    int bytes = read(fd, buf, bufsize);
	    log(L_DEBUG, "got %d: \"%.*s\"", bytes, bytes, buf);
	    return (bytes);
	} else {
	    log(L_ERR, "unknown fd is set");
	    return (-1);
	}
    }
}

/*
 * Like a normal read, except use a timeout.  Returns the bytes
 * that were read.
 *
 * Return codes:
 *	 0	timeout
 *	-1	failure, see errno for more information
 */
int tread(fd, buf, bufsize, timeout)
     int fd;
     char *buf;
     int bufsize;
     int timeout;
{
    int bytes;
    
    if (!init)
      timeout_init();

    timeout_set(timeout);
    bytes = read_internal(fd, buf, bufsize);
    timeout_set(0);

    return (bytes);
}

/*
 * Like a normal read, except poll for input.  Return -1 on error or
 * 0 if no data is available.
 */
int pread(fd, buf, bufsize)
     int fd;
     char *buf;
     int bufsize;
{
    fd_set readfds;
    struct timeval timeout;

    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);

    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    
    switch (select(32, &readfds, NULL, NULL, &timeout)) {
      case -1:
	log(L_EMERG, "pread: select failed: %m");
	return (-1);
      case 1:
	if (FD_ISSET(fd, &readfds))
	  return (read(fd, buf, bufsize));
	break;
      default:
	break;
    }

    return (0);
}

/*
  Read a string, terminated by a CR, into the buffer.  Return the
  length of characters read, include the CR.  Skips any NL chars
  that might be encountered, since they can safely be ignored.

  Return codes:
  	  0	timeout
	 -1	failure, see errno for more information
*/
int fdgets(fd, buf, bufsize)
     int fd;
     char *buf;
     int bufsize;
{
    char *bufp = buf;
    int i;

    for (i = 0; i < bufsize; i++) {
	char c;
	
	switch (read_internal(fd, &c, 1)) {
	  case 0:
	    return (0);
	  case 1:
	    if (c != '\n') {
		*bufp++ = c;
		if (c == '\r')
		  return (i+1);
	    }
	    break;
	  default:
	    return (-1);
	}
    }

    return (i);
}

/*
 * Read a line, until a NL, with default timeout.  Return -1 on
 * failure.  Return 0 on timeout.  Otherwise, return total bytes
 * read;
 */
int tfdgets(fd, buf, bufsize, timeout)
     int fd;
     char *buf;
     int bufsize;
     int timeout;
{
    int bytes;

    if (!init)
      timeout_init();

    timeout_set(timeout);
    bytes = fdgets(fd, buf, bufsize);
    timeout_set(0);

    return (bytes);
}

int wait_for_char(fd, c_expect, timeout)
     int fd;
     char c_expect;
     int timeout;
{
    if (!init)
      timeout_init();

    timeout_set(timeout);

    log(L_DEBUG, "waiting %d seconds for char %d", timeout, c_expect);

    for (;;) {
	char c;
	
	if (read_internal(fd, &c, 1) != 1) {
	    timeout_set(0);
	    return (-1);
	}
	
	if (c == c_expect)
	  return (0);
    }
}

int wait_for_string(fd, s_expect, timeout)
     int fd;
     char *s_expect;
     int timeout;
{
    if (!init)
      timeout_init();

    timeout_set(timeout);

    log(L_DEBUG, "waiting %d seconds for string %s", timeout, s_expect);

    for (;;) {
	char buf[BUFSIZ];
	int bytes;
    
	if ((bytes = fdgets(fd, buf, sizeof(buf))) <= 0) {
	    timeout_set(0);
	    return (-1);
	}

	if (bytes > 1) {
	    buf[bytes] = '\0';

	    if (strncmp(s_expect, buf, strlen(s_expect)) == 0)
	      return (0);
	}
    }
}

#ifdef DEBUG

static int test_fds[2];

void timeout_test()
{
    char buf[128];
    int count;

    if (socketpair(AF_UNIX, SOCK_STREAM, 0, test_fds) < 0) {
	perror("sockpair");
	return;
    }

    write(test_fds[1], "hello", 6);

    if ((count = tfdgets(test_fds[0], buf, sizeof(buf), 10)) <= 0) {
	printf("read failed: %d\n", count);
	close(test_fds[0]);
	close(test_fds[1]);
	return;
    }

    printf("got %d bytes: \"%.*s\"\n", count, count, buf);
}
#endif DEBUG
