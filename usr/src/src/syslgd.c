#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <stdio.h>

main()
{
   int sock;
   struct sockaddr saddr;
   char buff[1024];
   int socks=0;
   fd_set in;
   fd_set out,err;
   
   sock = socket (AF_UNIX, SOCK_DGRAM, 0);
   if (sock < 0)
     {
	perror ("socket");
	exit (1);
     }

   saddr.sa_family = AF_UNIX;
   strncpy (saddr.sa_data, "/dev/log", sizeof (saddr.sa_data));
   if (bind (sock, &saddr, sizeof (saddr)))
     {
	perror ("bind");
	exit (2);
     }
   listen (sock,10);
   while (1)
     {
	int i;
	int err;
	FD_ZERO(&in);
	FD_SET (sock, &in);
	FD_ZERO(&out);
	FD_ZERO(&err);
	for (i = 0; i < 32; i++)
	  if (socks & (1 << i))
	    {
	       FD_SET (i, &in);
	       FD_SET (i, &err);
	    }

	err = select (31, &in, &out , &err, NULL);

	if (err < 0)
	  {
	     if (errno == EINTR) continue;
	     perror ("select");
	     exit (3);
	  }
	for (i = 1; i < 32; i++)
	  {
	     if (i != sock && !(socks & 1 << i)) continue;
	     if (FD_ISSET(i, &in) || FD_ISSET (i, &err) || FD_ISSET(i, &out))
	       {
		  if (i == sock)
		    {
		       err=accept (sock, NULL, 0);
		       if (err < 0)
			 {
			    perror ("accept");
			    continue;
			 }
		       socks |= 1 << err;
		       continue;
		    }
		  
		  err= read (i, buff, 1024);
		  if (err <= 0)
		    {
		       if (err < 0)
			 {
			    if (errno == EINTR) continue;
			    if (errno == EAGAIN) continue;
			    perror ("syslogd: read");
			 }
		       close (i);
		       socks &= ~(1<<i);
		       continue;
		    }
		  write (1, buff, err);
	       }
	  }
     }
   exit (0);
}

