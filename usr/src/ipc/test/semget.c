#include <stdio.h>
#include <sys/types.h>
#include <sys/sem.h>
#include <errno.h>

int main ()
{
  key_t key;
  int semflg, nsems, semid;

  fprintf (stderr, "All numeric input follows C conventions\n");
  fprintf (stderr, "IPC_PRIVATE = %#lx\n", IPC_PRIVATE);
  fprintf (stderr, "Enter desired key: ");
  scanf ("%li", &key);
  fprintf (stderr, "Enter number of semaphores in set: ");
  scanf ("%i", &nsems);
  fprintf (stderr, "Allowed flags for semflg are:\n");
  fprintf (stderr, "IPC_EXCL=%#8.8o \t IPC_CREAT=%#8.8o\n", IPC_EXCL, IPC_CREAT);
  fprintf (stderr, "Owner read=%#8.8o \t alter=%#8.8o\n", 0400, 0200);
  fprintf (stderr, "Group read=%#8.8o \t alter=%#8.8o\n", 040, 020);
  fprintf (stderr, "Other read=%#8.8o \t alter=%#8.8o\n", 04, 02);
  fprintf (stderr, "Enter desired semflg value: ");
  scanf ("%i", &semflg);

  fprintf (stderr, "Calling semget (%#lx, %d, %#o)\n", key, nsems, semflg);
  if ((semid = semget (key, nsems, semflg)) == -1)
	{
	  perror ("semget : semget failed");
	  exit (1);
	}
  else
	fprintf (stderr, "semget succeeded: semid = %d\n", semid);

return (semid);
}

