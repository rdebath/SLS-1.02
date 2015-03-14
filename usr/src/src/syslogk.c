usage()
{
	puts("capture kernel messages and write them to standard");
   	puts("out,  which can be redirected to a file or tty."); 
   	puts("usage: syslogk on");
	puts("       syslogk off");
	exit(-1);
}

#define __LIBRARY__
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

static _syscall3(int,syslog,int,type,char *,buf,int,size)

char buffer[1024];

int main(int argc, char ** argv)
{
        int i;

        errno = 0;
        if (argc == 2 && !strcmp("off",argv[1]))
                syslog(0,0,0);
        else if (argc == 2 && !strcmp("on",argv[1]))
	{
                syslog(1,0,0);
                while (1) {
                        i = syslog(2,buffer,1024);
                        if (i < 0)
                                if (errno == EINTR)
                                        continue;
                                else
                                        break;
                        write(1,buffer,i);
                }
	}
	else
		usage();
        if (errno)
                perror("syslog");
        return errno;
}
