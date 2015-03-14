#define I_ERRNO
#define I_SOCKET
#define I_IOCTL
#include "includes.h"

void main(int argc, char *argv[]) {
    int i;
    int soc[2];
    char path[256];
    socketpair(AF_UNIX, SOCK_STREAM, 0, soc);

    if (argc !=1 && argc!=2) {
      printf("usage: test [ -option ]\n");
      exit(1);
    }

    if (!fork()) {
	dup2(soc[0], 0);
	dup2(soc[0], 1);
       	i = open("local.log", O_RDWR | O_CREAT | O_TRUNC , 0600);
	if (i < 0) {
	  perror("Open");
	  exit(1);
	}
	dup2(i, 2);
	sprintf(path, "%s/.term/socketa", getenv("HOME"));
	unlink(path);
	execlp("./term", "term", argv[1], 0);
	perror("execlp failed");
	exit(1);
	}
    if (!fork()) {
	dup2(soc[1], 0);
	dup2(soc[1], 1);
	dup2(open ("remote.log", O_RDWR  | O_CREAT | O_TRUNC, 0600), 2);
	sprintf(path, "%s/.term/socketb", getenv("HOME"));
	unlink(path);
	if (argc==2)
	  execlp("./term", "term",argv[1],"test", 0);
	else
	  execlp("./term", "term","test",0);
	perror("execlp failed2");
	exit(1);
	}
    sleep(36000);
    }
	
