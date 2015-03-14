#define I_ERRNO
#define I_SYS
#define I_SOCKET
#include "includes.h"

#include "client.h"
#include <signal.h>

int display_num = 9;
int debug = 0;

char *remote_port;

int rd_connect_server(void) {
  int s;
  
  if ((s = connect_server(0)) <0) {
    perror("Couldn't open term");
    close(s);
    return -1;
  }

  if (send_command(s, C_PORT, 0, "%s", remote_port)< 0) {
    return -1;
  }

  send_command(s, C_DUMB, 1, 0);
  return s;
}


void main(int argc, char *argv[]) {
  int s;
  int first;
  int svs[1];
  signal(SIGPIPE, SIG_IGN);
  first = client_options(argc, argv,"",NULL);
  if ( (argc-first) < 1 )
  {
	fprintf ( stderr, "Usage: redir [options] <localport> <[host:]remoteport>\n" );
	exit(1);
  }
  fprintf ( stderr, "Redirecting %s to %s\n", argv[first], argv[first+1] );
  remote_port = argv[first+1];

  setbuf(stderr, 0);

				/* Bind the local socket we are going */
				/* to listen on.  */
  s = bind_tcp(atoi(argv[first]));
  if (s < 0) {
    fprintf(stderr, "Port is already bound or some such error.\n");
    fprintf(stderr, "  s was %d.  Exiting\n", s);
    exit(1);
  }
  svs[0] = s;
  do_connect(1, svs, rd_connect_server);
}
