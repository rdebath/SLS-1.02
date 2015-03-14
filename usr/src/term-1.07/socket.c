#define I_SYS
#define I_SOCKET
#define I_ERRNO
#define I_STRING

#include "includes.h"

int bind_tcp(int port)
{
  struct hostent *hp;
  char host_name[100];
  struct sockaddr_in sin;
  int s;
#ifdef SYSV
  struct utsname unam;

  if (uname(&unam) == -1) {
    perror("uname");
    return -1;
  }
  strcpy(host_name, unam.nodename);
#else
  if (gethostname(host_name, sizeof(host_name)) == -1) {
    perror("gethostname");
    return -1;
    }  
#endif /* SYSV */

  hp = gethostbyname(host_name); /* Find the name of the machine we */
				/* are running.. */
  if (hp == 0) {		/* Probly doesn't run TCP/IP. oh well. */
    fprintf(stderr, "Gethostbyname: Unknown host.\n");
    return -1;
    }

  memset(&sin, 0, sizeof(sin));
  memcpy(&sin.sin_addr, hp->h_addr, hp->h_length);
  sin.sin_port = htons( port );
  sin.sin_family = hp->h_addrtype;
  sin.sin_addr.s_addr = INADDR_ANY;
  s = socket(hp->h_addrtype, SOCK_STREAM, 0);
  if (s == -1) {
    perror("socket");
    return -1;
    }
				/* Ok. Now we look for a socket that */
				/* isn't bound. This is probably a */
				/* little scummy, but some ppl like it */
				/* this way.. We try a maximum of 100 */
				/* times... */
  while (bind(s, (struct sockaddr * ) &sin,
	      sizeof(sin)) < 0) { /* while an error.. */
    if (errno != EADDRINUSE) {	/* if it wasn't in use */
      close(s);
      return -1; 		/* then we can't handle it, so abort. */
    }
    close(s);
    return -2;			/* handled specially by some clients. */
  }

  if (listen(s, 5) == -1) {	/* If we can't listen... */
    perror("listen");		/* then just dump. We can't handle */
				/* errors here. */
    close(s);
    return -1;
    }
  return s;
}



int bind_unix(char *path) 
{
  struct sockaddr_un sock_un;
  int s;

  if ((s = socket(AF_UNIX, SOCK_STREAM, 0 )) < 0) {
    perror("Socket");
    return -1;
  }

  sock_un.sun_family = AF_UNIX;
  
  strcpy(sock_un.sun_path, path);
  unlink(sock_un.sun_path);
  if (bind(s, (struct sockaddr *) &sock_un, strlen(sock_un.sun_path) + 2)) {
    perror("Bind");
    close(s);
    return -1;
  }

  /* ok. Start looking for connections. */
  if (listen(s, 5) < 0) {
    perror("Listen");
    close(s);
    return -1;
  }
  return s;
}



int open_unix(char *p) 
{
  struct sockaddr_un sock_un;
  int s;
  
  if ((s = socket(AF_UNIX, SOCK_STREAM, 0 )) < 0) {
    perror("Socket");
    return -1;
  }

  sock_un.sun_family = AF_UNIX;
  
  sprintf(sock_un.sun_path,"%s",p);
  
  if (connect(s, (struct sockaddr *) &sock_un, 
	      strlen(sock_un.sun_path) + 2)) {
    perror("Connect");
    close(s);
    return -1;
  }
  return s;
  
}


