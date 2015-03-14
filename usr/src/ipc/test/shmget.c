#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

int main ()
{
  key_t key;
  int shmflg, shmid, size;

  fprintf (stderr, "All numeric input follows C conventions\n");
  fprintf (stderr, "IPC_PRIVATE = %#lx\n", IPC_PRIVATE);
  fprintf (stderr, "Enter desired key: ");
  scanf ("%li", &key);
  fprintf (stderr, "Enter desired size : ");
  scanf ("%i", &size);
  fprintf (stderr, "Allowed flags for shmflg are:\n");
  fprintf (stderr, "IPC_EXCL=%#8.8o \t IPC_CREAT=%#8.8o\n", IPC_EXCL, IPC_CREAT);
  fprintf (stderr, "Owner read=%#8.8o \t alter=%#8.8o\n", 0400, 0200);
  fprintf (stderr, "Group read=%#8.8o \t alter=%#8.8o\n", 040, 020);
  fprintf (stderr, "Other read=%#8.8o \t alter=%#8.8o\n", 04, 02);
  fprintf (stderr, "Enter desired shmflg value: ");
  scanf ("%i", &shmflg);

  fprintf (stderr, "Calling shmget (%#lx, %d, %#o)\n", key, size, shmflg);
  if ((shmid = shmget (key, size, shmflg)) == -1)
	{
	  perror ("shmget : shmget failed ");
	  exit (1);
	}
  else
	fprintf (stderr, "shmget succeeded: shmid = %d\n", shmid);

return (shmid);
}

