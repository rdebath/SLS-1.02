#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/sock_ioctl.h>
#include <netdb.h>
#include <string.h>
#include <getopt.h>

unsigned long
atonet (const char *addr)
{
   unsigned long ret;
   int i;
   int shift = 0;
   int tmp=0;
   ret = 0;

   for (i = 0; addr[i] != 0; i++)
     {
	if (addr[i] == '.')
	  {
	     ret |= tmp << shift;
	     tmp = 0; 
	     shift += 8;
	     if (shift > 24)
	       {
		  fprintf (stderr,"%s - bad address.\n", addr);
		  return (-1);
	       }
	  }
	else if (addr[i] >= '0' && addr[i] <= '9')
	  {
	     tmp *= 10;
	     tmp += addr[i] - '0';
	     if (tmp > 255)
	       {
		  fprintf (stderr,"%s - bad address.\n", addr);
		  return (-1);
	       }
	  }
	else
	  {
	     fprintf (stderr,"%s - bad address.\n", addr);
	     return (-1);
	  }
     }
   ret |= tmp << shift;
   return (ret);
}

void
usage(const char *name)
{
   fprintf (stderr,"%s [-r router] [-n net] [-d] interface ip-address\n",
	    name);
}

int
main(int argc, char *argv[])
{
  int s;
  int c;
  struct ip_config ipc;

  /* the default values. */
  ipc.name[0] = 0;
  ipc.paddr = -1;
  ipc.router = -1;
  ipc.net = -1;
  ipc.up = 1;

  while ((c = getopt(argc, argv, "r:n:d")) != -1)
    {
       switch (c)
	 {
	   case 'r':
	    ipc.router = atonet(optarg);
	    break;

	   case 'n':
	    ipc.net = atonet (optarg);
	    break;

	   case 'd':
	    ipc.up = 0;
	    break;

	   case '?':
	    usage (argv[0]);
	    return (1);

	 }
    }
  /* now read the interface name. */
  if (argc - optind != 2)
    {
       usage (argv[0]);
       return (1);
    }

  
  strncpy (ipc.name, argv[optind++], MAX_IP_NAME);

  /* and now the protocol address. */
  ipc.paddr = atonet(argv[optind]);
  if (ipc.paddr == -1) 
    {
       perror ("bad ip_number");
       usage(argv[0]);
       return(1);
    }
  s = socket (AF_INET, SOCK_STREAM, 0);
  if (s < 0)
    {
      perror ("socket");
      return (1);
    }
  if (ioctl (s, IP_SET_DEV, &ipc) < 0)
    {
       perror ("ioctl");
       return (1);
    }
  return (0);
}
