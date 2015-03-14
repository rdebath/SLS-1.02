#include <stdlib.h>
#include <stdio.h>
#include <termios.h>
#include <fcntl.h>
#include <sys/ptrace.h>
#include <sys/ioctl.h>

int ldisc;
int fd;

void
sig (int signum)
{

  signal (signum, sig);
  fprintf (stderr, "Quitting\n");
  if (ptrace (PTRACE_ENABLE_KTRACE, 0, 0, -1) < 0)
    {
      perror ("ptrace");
      exit (1);
    }

  while (ioctl (fd, TIOCSETD, &ldisc) < 0)
    {
      perror ("ioctl\n");
    }
}

main(int argc, char *argv[])
{
  int i;

  if (argc != 2)
    {
      fprintf (stderr, "usage: %s /dev/ttyXX\n", argv[0]);
      exit (1);
    }

  fd = open (argv[1], O_RDWR);

  if (fd < 0)
    {
      perror ("open");
      exit (1);
    }

  if (ioctl (fd, TIOCGETD, &ldisc) < 0)
    {
      perror ("ioctl1");
      exit (1);
    }

  for (i = 0; i < 32; i++)
    {
      signal (i, sig);
    }

  if (ptrace (PTRACE_ENABLE_KTRACE, 0, 0, -1) < 0)
    {
      perror ("ptrace");
      exit (1);
    }

  i = N_KDB;
  if (ioctl (fd, TIOCSETD, &i) < 0)
    {
      perror ("ioctl2");
      exit (1);
    }

  pause();
  exit (0);
}

