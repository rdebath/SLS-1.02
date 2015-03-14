#include <stdio.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>

static int ask ();
struct semid_ds semds;
static union semun arg;
static char error1[] = "semop: cant allocate space for %d sem values. Giving up.\n";
static char error2[] = "semop: cant allocate space for %d sembufs. Giving up.\n";

int main ()
{
  int i, nsops, semid=-1;
  struct sembuf *sops;

  fprintf (stderr, "All numeric input follows C conventions\n");
  while (nsops = ask (&semid, &sops))
    {
      for (i=0; i<nsops; i++)
	{
	  fprintf (stderr, "Enter desired values for operation %d of %d.\n", i+1, nsops);
	  fprintf (stderr, "semnum : (must be < %d)", semds.sem_nsems);
	  scanf ("%i", &sops[i].sem_num);
	  fprintf (stderr, "semop: ");
	  scanf ("%i", &sops[i].sem_op);
	  fprintf (stderr, "Allowed flags are: ");
	  fprintf (stderr, "IPC_NOWAIT=%#6.6o\t SEM_UNDO=%#6.6o\n", IPC_NOWAIT, SEM_UNDO); 
	  fprintf (stderr, "sem_flg: ");
	  scanf ("%hi", &sops[i].sem_flg);
	}
      fprintf (stderr, "Calling semop (%d, &sops, %d) with:", semid, nsops);
      for (i=0; i < nsops; i++)
	{
	  fprintf (stderr, "\nsops[%d].sem_num = %d, ", i, sops[i].sem_num);
	  fprintf (stderr, "sem_op=%d, ", sops[i].sem_op);
	  fprintf (stderr, "sem_flg=%#o\n", sops[i].sem_flg);
	}
      if ((i = semop (semid, sops, nsops)) == -1)
	{
	  perror ("semop: semop failed");
	  printf ("errno = %d\n", errno);
	}
      else
	fprintf (stderr, "semop returned %d\n", i);
    }
  return 0;
}


static int ask (int *semidp, struct sembuf **sopsp)
{
  int i;
  static int nsops=0, semid=-1;
  static struct sembuf *sops;

  if (semid < 0)
   {
      fprintf (stderr, "Enter semid: ");
      scanf ("%i", &semid);
      *semidp = semid;
      arg.buf = &semds;
      if (semctl (semid, 0, IPC_STAT, arg) == -1)
	{
	perror ("semop: semctl (IPC_STAT) failed");
	printf ("errno=%d\n", errno);
	}
      else if ((arg.array = (ushort *) malloc (
		(unsigned) (sizeof(ushort) * semds.sem_nsems))) == NULL)
	  {
	    fprintf (stderr, error1, semds.sem_nsems);
	    exit (1);
	  }
    }

  if (semds.sem_nsems)
    {
       fprintf (stderr, " There are %d semaphores in the set\n", semds.sem_nsems);
      if (semctl (semid, 0, GETALL, arg) == -1)
	{
	perror ("semop: semctl(GETALL failed");
	printf ("errno = %d\n", errno);
	}
      else
	{
	  fprintf (stderr, "Current semaphore values are:");
	  for (i=0; i<semds.sem_nsems; fprintf (stderr, " %d", arg.array[i++]));
	  fprintf (stderr, "\n");
	}
    }

  fprintf (stderr, "How many sem operations on the next call to semop?\n");
  fprintf (stderr, "Enter 0 or ^d to quit: ");
  i = 0;
  if (scanf ("%i", &i) == EOF || i == 0)
    exit (0);
  if (i > nsops)
    {
      if (nsops)
	free ((char *) sops);
      nsops = i;
      if ((sops = (struct sembuf *) malloc ((unsigned) (nsops* sizeof(struct sembuf)))) == NULL)
	{
	  fprintf (stderr, error2, nsops);
	  exit (2);
	}
    }
  *sopsp = sops;
  return (i);
}
