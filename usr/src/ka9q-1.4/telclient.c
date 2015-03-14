#include <stdio.h>
#include "global.h"
#include "config.h"
#include "mbuf.h"
#include "timer.h"
#include "internet.h"
#include "icmp.h"
#include "netuser.h"
#include "tcp.h"
#include "ftp.h"
#include "telnet.h"
#include "iface.h"
#include "ax25.h"
#include "lapb.h"
#include "session.h"
#include "nr4.h"
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#include <errno.h>

main(argc, argv)
int argc;
char **argv;
 {
  int fd;
  struct sockaddr_un unaddr;		/* UNIX socket data block */
  struct sockaddr *addr;		/* generic socket pointer */
  int addrlen;
  char buffer[1024];
  char *cp;
  int cnt;

  unaddr.sun_family = AF_UNIX;
  sprintf (unaddr.sun_path, "%s%d", "/tmp/telnet", 0);
  addr = (struct sockaddr *) &unaddr;
  addrlen = strlen(unaddr.sun_path) + sizeof(unaddr.sun_family);

  if ((fd = socket ((int) addr->sa_family, SOCK_STREAM, 0))
      < 0) {
    printf("telnet server socket failed\r\n");
    fflush(stdout);
    return;
  }
  if (connect (fd, addr, addrlen) < 0) {
    printf("telnet server bind failed\r\n");
    fflush(stdout);
    return;
  }

  strcpy(buffer, "telnet");
  cp = buffer + strlen(buffer);
  *cp++ = 0;
  strcpy(cp, ttyname(0));
  cp = cp + strlen(cp);
  *cp++ = 0;
  if (getenv("TERM")) {
    strcpy(cp, getenv("TERM"));
    cp = cp + strlen(cp);
  }
  *cp++ = 0;
  while (argc > 1) {
    strcpy(cp, argv[1]);
    cp = cp + strlen(cp);
    *cp++ = 0;
    argc--;
    argv++;
  }
  write(fd, buffer, cp-buffer);
  cnt = read(fd, buffer, 1);
}




