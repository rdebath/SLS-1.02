#define I_ERRNO
#define I_SYS
#define I_IOCTL
#define I_STRING
#define I_SOCKET

#include "includes.h"
#include <signal.h>

char *local_x =  "/tmp/.X11-unix/X0";

int display_num = 9;
int debug = 0;

int xc_connect_server(void) {
  int s;
  signal(SIGPIPE, SIG_IGN);
  if ((s = connect_server(0)) < 0)
    return -1;

  if (send_command(s, C_SOCKET, 0, "%s", local_x)<0)
    return -1;

  send_command(s, C_DUMB, 1, 0);

  return s;
}


void main(int argc, char *argv[]) {
  int s;
  char *home;
  char *display;
  int svs[1];
#ifdef UNIX_SOCKET
  char unix_sock[110];
#endif

  setbuf(stderr, 0);
  display = getenv("DISPLAY");

#ifdef UNIX_SOCKET
  if (!display || display[0] != ':' || !display[1])
    display = ":9";

  sprintf(unix_sock,"/tmp/.X11-unix");
  mkdir(unix_sock, 0700);
  strcat(unix_sock, "/X");
  strcat(unix_sock, display+1);

  if ((s = bind_unix(unix_sock)) < 0) {
    exit(1);
  }

#else /* if a TCP socket.. */
				/* Try and work out what display we */
				/* should get by default. */
  if (!display || !display[0]) { /* is there a DISPLAY set?? */
    display_num = 9;		/* No. Just pick 9 by default */
  } else {
    home = strchr(display,':');	/* Yes. Find out what display number. */
    if (home)			/* if we could work it out.. */
      display_num = atoi(home+1); /* then grab the number. */
    if (display_num < 1)	/* if the number is invalid,  */
      display_num = 9;		/* just default. */
  }
  
  while (( s = bind_tcp(6000 + display_num)) == -2) {
    if (display_num > 100) {
      printf("Unable to bind socket\n");
      exit(1);
      break;
    }
    ++display_num;
  }

  fprintf(stderr, "Xconn bound to screen %d\n", display_num);
  printf(":%d\n", display_num);

#endif
  svs[0] = s;
  do_connect(1, svs, xc_connect_server); /* Never exits. */
}
