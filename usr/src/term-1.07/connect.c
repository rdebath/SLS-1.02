#define I_ERRNO
#define I_SYS
#define I_IOCTL
#define I_STRING
#define I_SOCKET

#include "includes.h"
#include <signal.h>

typedef struct {
  int client; 
  int server;
  struct Buffer from;
  struct Buffer to;
} Con;

static void sig_ignore(int dummy) {
  signal(SIGPIPE, sig_ignore);
}

void do_connect(int num, int *svs, int (*get_server)(void)) {
  int max, num_cons = 0;
  int serv;
  Con *cons = 0;

  serv = connect_server(term_server);
  if (serv < 0) {
    fprintf(stderr, "Couldn't contact term server.\n");
    exit(1);
  }

  if (fork()) {
    exit(0);
  }

  signal(SIGPIPE, sig_ignore);

  close(0);
  close(1);
  close(2);
  lose_ctty();
  chdir("/");
  
  for (max = 0; max < num;++max)
    set_nonblock(svs[max]);

  while (1) {
    fd_set in, out, except;
    Con *c;
    int i, ret, loop;
    
    FD_ZERO(&in);
    FD_ZERO(&out);
    FD_ZERO(&except);
    max = -1;
    for (i = 0; i < num;++i) {
      FD_SET(svs[i], &in);
      if (max < svs[i]) max = svs[i];
    }

    FD_SET(serv, &except);
    FD_SET(serv, &in);
    if (serv > max) max = serv;
    
				/* Ok. Build a list of all the sockets */
				/* we want to look at.. */
    for (i = 0, c = cons; i < num_cons;++i, ++c) {
      if (c->client >= 0) {	/* If this socket can we read and */
				/* written..  */
	if (c->to.size) {	/* If there is something to go to it */
				/* then select for writing. */
	  FD_SET(c->client, &out);
	  if (c->client > max) max = c->client;
	} else if (c->server >=0) { /* Else select for more data if we can. */
	  FD_SET(c->server, &in);
	  if (c->server > max) max = c->server;
	} else {		/* ok there was nothing in the buffer, */
				/* and we couldn't get anymore , so */
				/* just close. */
	  close(c->client);
	  c->client = -1;
	}
      }
      if (c->server >= 0) {
	if (c->from.size) {
	  FD_SET(c->server, &out);
	  if (c->server > max) max = c->server;
	} else if (c->client >=0) {
	  FD_SET(c->client, &in);
	  if (c->client > max) max = c->client;
	} else {
	  close(c->server);
	  c->server = -1;
	}
      }				/* if if server >=0 */
    }				/* Of for() */

    select(max+1, &in, &out, &except, 0);

				/* Check for term going away... */
    if (FD_ISSET(serv, &except) || FD_ISSET(serv, &in)) {
				/* Ok. Critical . Close every thing.*/
      exit(0);
    }
    for (loop = 0; loop < num;++loop)
      if (FD_ISSET(svs[loop], &in)) { /* new connection */
#ifdef UNIX_SOCKET
	struct sockaddr_un dum;
#else
	struct sockaddr_in dum;
#endif
	int sdum = sizeof(dum),i ;
	for (i = 0; i < num_cons;i ++)
	  if (cons[i].server < 0 && cons[i].client < 0) break;
	if (i >= num_cons) {
	  if (!cons)
	    cons = (Con *) malloc(sizeof(Con));
	  else
	    cons = (Con *) realloc((char *) cons, (num_cons + 1) *
				   sizeof(Con)); 
	  i = num_cons;
	  num_cons += 1;
	}
	c = &cons[i];
	c->client = accept(svs[loop], (struct sockaddr *) &dum, &sdum);
	
	if ((c->server = get_server()) <0) {
	  perror("Couldn't open term");
	  close(c->client);
	  continue;
	}
	
	set_nonblock(c->server);
	set_nonblock(c->client);
	
	c->to.size = c->to.start = c->to.end = 0;
	c->from.size = c->from.start = c->from.end = 0;
      }
    
    for (i = 0, c = cons; i < num_cons;++i, ++c) {
      if (c->client < 0) continue;
      if (FD_ISSET(c->client,&in))
	ret = read_into_buff(c->client, &c->from, 0);
      else if (FD_ISSET(c->client, &out))
	ret = write_from_buff(c->client, &c->to, 0);
      else continue;
				/* Handle possible error condition */
      if (ret <=0 && term_errno) {
				/* an error has occured or a stream */
				/* has closed. Close connection. NYF*/
	close(c->client);
	c->client = -1;
	continue;
      }
    }

    for (i = 0, c = cons; i < num_cons;++i, ++c) {
      if (c->server < 0) continue;
      if (FD_ISSET(c->server, &out)) 
	ret = write_from_buff(c->server, &c->from, 0);
      else if (FD_ISSET(c->server, &in))
	ret = read_into_buff(c->server, &c->to, 0);
      else continue;
				/* Handle possible error condition */
      if (ret<=0 && term_errno) {
	close(c->server);
	c->server = -1;
      }
    }
  }
}

