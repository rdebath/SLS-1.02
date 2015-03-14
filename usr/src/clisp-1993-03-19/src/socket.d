/* Copyright    Massachusetts Institute of Technology    1988   */
/*
 * THIS IS AN OS DEPENDENT FILE! It should work on 4.2BSD derived
 * systems.  VMS and System V should plan to have their own version.
 *
 * This code was cribbed from lib/X/XConnDis.c.
 * Compile using
 *                    % cc -c socket.c -DUNIXCONN
 */
#include "unixconf.h" /* definiert HAVE_GETHOSTBYNAME, USG und STDC_HEADERS */
#ifdef HAVE_GETHOSTBYNAME
#ifndef vms
#define UNIXCONN
#endif

#include <stdio.h>
#if 0
#include <X11/Xos.h>
#include <X11/Xproto.h>
#else # this is the only thing we need from these header files:
#define X_TCP_PORT  6000
#endif
#include <errno.h>
#include <sys/types.h>
#include <netinet/in.h>
#ifndef vms
#include <sys/ioctl.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#include <sys/socket.h>
#else
#include <sun/netdb.h>
#endif
#ifndef vms
#ifndef hpux
#ifdef __386BSD__
#include <machine/endian.h>     /* needed for <netinet/tcp.h> */
#endif
#include <netinet/tcp.h>
#endif
#endif

extern int errno;               /* Certain (broken) OS's don't have this */
                                /* decl in errno.h */
extern int close ();               /* for HP/UX */
extern unsigned long inet_addr (); /* for HP/UX */

#ifdef UNIXCONN
#include <sys/un.h>
#ifndef X_UNIX_PATH
#ifdef hpux
#define X_UNIX_PATH "/usr/spool/sockets/X11/"
#define OLD_UNIX_PATH "/tmp/.X11-unix/X"
#else /* hpux */
#define X_UNIX_PATH "/tmp/.X11-unix/X"
#endif /* hpux */
#endif /* X_UNIX_PATH */
#endif /* UNIXCONN */

#if defined(USG) || defined(STDC_HEADERS)
#include <string.h>   /* declares memcpy() */
#else
#include <strings.h>  /* declares bcopy() */
#endif

/*
 * Attempts to connect to server, given host and display. Returns file
 * descriptor (network socket) or 0 if connection fails.
 */

int connect_to_server (host, display)
     char *host;
     int display;
{
  struct sockaddr_in inaddr;    /* INET socket address. */
  struct sockaddr *addr;                /* address to connect to */
  struct hostent *host_ptr;
  int addrlen;                  /* length of address */
#ifdef UNIXCONN
  struct sockaddr_un unaddr;    /* UNIX socket address. */
#endif
  extern char *getenv();
  extern struct hostent *gethostbyname();
  int fd;                               /* Network socket */
  {
#ifdef UNIXCONN
    if ((host[0] == '\0') || (strcmp("unix", host) == 0)) {
        /* Connect locally using Unix domain. */
        unaddr.sun_family = AF_UNIX;
        (void) strcpy(unaddr.sun_path, X_UNIX_PATH);
        (void) sprintf(&unaddr.sun_path[strlen(unaddr.sun_path)], "%d", display);
        addr = (struct sockaddr *) &unaddr;
        addrlen = strlen(unaddr.sun_path) + 2;
        /*
         * Open the network connection.
         */
        if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0) {
#ifdef hpux /* this is disgusting */  /* cribbed from X11R4 xlib source */
            if (errno == ENOENT) {  /* No such file or directory */
              (void) sprintf(unaddr.sun_path, "%s%d", OLD_UNIX_PATH, display);
              addrlen = strlen(unaddr.sun_path) + 2;
              if ((fd = socket ((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
                return(-1);     /* errno set by most recent system call. */
            } else
#endif /* hpux */
            return(-1);     /* errno set by system call. */
        }
    } else
#endif /* UNIXCONN */
    {
      /* Get the statistics on the specified host. */
      if ((inaddr.sin_addr.s_addr = inet_addr(host)) == -1)
        {
          if ((host_ptr = gethostbyname(host)) == NULL)
            {
              /* No such host! */
              errno = EINVAL;
              return(-1);
            }
          /* Check the address type for an internet host. */
          if (host_ptr->h_addrtype != AF_INET)
            {
              /* Not an Internet host! */
              errno = EPROTOTYPE;
              return(-1);
            }
          /* Set up the socket data. */
          inaddr.sin_family = host_ptr->h_addrtype;
#if defined(USG) || defined(STDC_HEADERS)
          (void) memcpy((char *)&inaddr.sin_addr,
                        (char *)host_ptr->h_addr,
                        sizeof(inaddr.sin_addr));
#else
          (void) bcopy((char *)host_ptr->h_addr,
                       (char *)&inaddr.sin_addr,
                       sizeof(inaddr.sin_addr));
#endif
        }
      else
        {
          inaddr.sin_family = AF_INET;
        }
      addr = (struct sockaddr *) &inaddr;
      addrlen = sizeof (struct sockaddr_in);
      inaddr.sin_port = display + X_TCP_PORT;
      inaddr.sin_port = htons(inaddr.sin_port);
      /*
       * Open the network connection.
       */
      if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0){
        return(-1);         /* errno set by system call. */}
      /* make sure to turn off TCP coalescence */
#ifdef TCP_NODELAY
      {
        int mi = 1;
        setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, (char*)&mi, sizeof (int));
      }
#endif
    }

    /*
     * Changed 9/89 to retry connection if system call was interrupted.  This
     * is necessary for multiprocessing implementations that use timers,
     * since the timer results in a SIGALRM.    -- jdi
     */
    while (connect(fd, addr, addrlen) == -1) {
        if (errno != EINTR) {
            (void) close (fd);
            return(-1);             /* errno set by system call. */
        }
      }
  }
  /*
   * Return the id if the connection succeeded.
   */
  return(fd);
}

#endif
