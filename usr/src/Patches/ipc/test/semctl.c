#include <stdio.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <time.h>
#include <errno.h>

struct semid_ds semds;
static void do_semctl();
static void do_stat();

char warning[] = "If you dont have read perms this program will fail often";

main()
{
  union semun arg;
  int cmd, i, semid, semnum;

  fprintf(stderr, "Numeric input interpreted as in C\n");
  fprintf(stderr, "Enter semid value: ");
  scanf ("%i", &semid);

  fprintf (stderr, "Valid semctl cmd values are: \n");
  fprintf (stderr, "GETALL=%d\t GETNCNT=%d\t GETPID=%d\t GETVAL=%d\t GETZCNT=%d\n",
	GETALL, GETNCNT, GETPID, GETVAL, GETZCNT);
  fprintf (stderr, "IPC_RMID=%d\t IPC_SET=%d\t IPC_STAT=%d\t SETVAL=%d\t SETALL=%d\n",
	IPC_RMID, IPC_SET, IPC_STAT, SETVAL, SETALL);
  fprintf (stderr, "Enter desired cmd: ");
  scanf ("%i", &cmd);

/* perform setup operations needed by multiple commands */
  switch (cmd) {
  case GETVAL:
  case SETVAL:
  case GETNCNT:
  case GETZCNT:
	fprintf(stderr, "Enter desired semnum value: ");
	scanf ("%i", &semnum);
	break;
  case GETALL:
  case SETALL:
	fprintf (stderr, "Get number of semaphores in set.\n");
	arg.buf = &semds;
	do_semctl (semid, 0, IPC_STAT, arg);
	if (arg.array = (ushort *) malloc (semds.sem_nsems * sizeof (ushort)))
		break;
	fprintf (stderr, "semctl : unable to allocate space for %d values\n", semds.sem_nsems);
	exit (2);
  }

/* get rest of parameters */
  switch (cmd) {
  case SETVAL:
	fprintf (stderr, "Enter desired semaphore value: ");
	scanf ("%i", &arg.val);
	do_semctl (semid, semnum, SETVAL, arg);
	fprintf (stderr, "Performing GETVAL to verify\n");
	/* fall thru for verify */
  case GETVAL:
	arg.val = 0;
	do_semctl (semid, semnum, GETVAL, arg);
	break;
  case GETPID:
	arg.val = 0;
	do_semctl (semid, 0, GETPID, arg);
	break;
  case GETNCNT:
	arg.val = 0;
	do_semctl (semid, semnum, GETNCNT, arg);
	break;
  case GETZCNT:
	arg.val = 0;
	do_semctl (semid, semnum, GETZCNT, arg);
	break;
  case SETALL:
	fprintf (stderr, "There are %d semaphores in the set.\n", semds.sem_nsems);
	fprintf (stderr, "Enter desired semaphore values:\n");
	for (i=0; i<semds.sem_nsems; i++)
	  {
	    fprintf (stderr, "semaphore %d: ", i);
	    scanf ("%hi", &arg.array[i]);
	  }
	do_semctl (semid, 0, SETALL, arg);
	fprintf (stderr, "Performing GETALL to verify.\n");
	/* all thru to verify */
  case GETALL:
	do_semctl (semid, 0, GETALL, arg);
	fprintf (stderr, "The values of %d semaphores are:\n", semds.sem_nsems);
	for (i=0; i< semds.sem_nsems; i++)
	  fprintf (stderr, "%d ", arg.array[i]);
	fprintf (stderr, "\n");
	break;
  case IPC_SET:
	arg.buf = &semds;
	do_semctl (semid, 0, IPC_STAT, arg);
	fprintf (stderr, "Status before IPC_SET:\n");
	do_stat();
	fprintf (stderr, "Enter desired uid value: ");
	scanf ("%hi", &semds.sem_perm.uid);
	fprintf (stderr, "Enter desired gid value: ");
	scanf ("%hi", &semds.sem_perm.gid);
	fprintf (stderr, "%s\n", warning);
	fprintf (stderr, "Enter desired perm value: ");
	scanf ("%hi", &semds.sem_perm.mode);
	do_semctl (semid, 0, IPC_SET, arg);
	fprintf (stderr, "Status after IPC_SET:\n");
	/* fall thru to verify */
  case IPC_STAT:
	arg.buf = &semds;
	do_semctl (semid, 0, IPC_STAT, arg);
	do_stat();
	break;
  case IPC_RMID:
	arg.val = 0;
	do_semctl (semid, 0, IPC_RMID, arg);
	break;
  default:
	arg.val = 0;
	do_semctl (semid, 0, cmd, arg);
	break;
  }
}

static void do_semctl (int semid, int semnum, int cmd, union semun arg)
{
  register int i;
  fprintf (stderr, "semctl: calling semctl (%d, %d, %d, ",
	semid, semnum, cmd);
  switch (cmd) {
  case GETALL:
	fprintf (stderr, "arg.array = %#x)\n", arg.array);
	break;
  case IPC_STAT:
  case IPC_SET:
	fprintf (stderr, "arg.buf = %#x)\n", arg.buf);
	break;
  case SETALL:
	fprintf (stderr, "arg.array = [", arg.buf);
	for (i=0; i< semds.sem_nsems; )
	  {
	    fprintf (stderr, "%d", arg.array[i++]);
	    if (i < semds.sem_nsems)
		fprintf (stderr, ", ");
	  }
	fprintf (stderr, "])\n");
	break;
  case SETVAL:
  default:
	fprintf (stderr, "arg.val = %d)\n", arg.val);
	break;
  }

  i = semctl (semid, semnum, cmd, arg);
  if (i == -1)
    {
	perror ("semctl: semctl failed");
	exit (1);
    }
  fprintf (stderr, "semctl returned %d\n", i);
  return;
}

static void do_stat (void)
{
  struct ipc_perm *ipcp = &semds.sem_perm;

  fprintf (stderr, "Values in semid_ds.sem_perm\n");
  fprintf (stderr, "uid=%d\t gid=%d\t cuid=%d\t cgid=%d\n",
	ipcp->uid, ipcp->gid, ipcp->cuid, ipcp->cgid);
  fprintf (stderr, "mode=%#o, access_perms=%#o\n", ipcp->mode, ipcp->mode & 0777);
  fprintf (stderr, "Values in semid_ds\n");
  fprintf (stderr, "nsems = %d\n", semds.sem_nsems);
  fprintf (stderr, "otime = %s", semds.sem_otime ? ctime (&semds.sem_otime) : "Not set\n");
  fprintf (stderr, "ctime = %s", ctime (&semds.sem_ctime));	
}
