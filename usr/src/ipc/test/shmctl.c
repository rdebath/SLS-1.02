#include <stdio.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <time.h>
#include <errno.h>

static struct shmid_ds shmds;
static void do_shmctl();
static void do_stat();

int main()
{
  int cmd, shmid;

  fprintf(stderr, "Numeric input interpreted as in C\n");
  fprintf(stderr, "Enter shmid value: ");
  scanf ("%i", &shmid);

  fprintf (stderr, "Valid shmctl cmd values are:\n");
  fprintf (stderr, "IPC_RMID=%d\t IPC_SET=%d\t IPC_STAT=%d\t"
	"SHM_LOCK=%d\t SHM_UNLOCK=%d\n", IPC_RMID, IPC_SET, IPC_STAT,
	SHM_LOCK, SHM_UNLOCK);
  fprintf (stderr, "Enter desired cmd: ");
  scanf ("%i", &cmd);

/* perform setup operations needed by multiple commands */
  switch (cmd) {
  case IPC_SET:
	do_shmctl (shmid, IPC_STAT, &shmds);
        fprintf (stderr, "Enter desired uid value: ");
	scanf ("%hi", &shmds.shm_perm.uid);
	fprintf (stderr, "Enter desired gid value: ");
	scanf ("%hi", &shmds.shm_perm.gid);
	fprintf (stderr, "Enter desired perm value: ");
	scanf ("%hi", &shmds.shm_perm.mode);
	break;
  case IPC_STAT:
	break;
  case IPC_RMID:
	break;
  case SHM_LOCK:
        break;
  case SHM_UNLOCK:
        break;     
  default:
      break;
  }
  do_shmctl (shmid, cmd);
  return (0);
}

static void do_shmctl (int shmid, int cmd)
{
  int i;
  struct ipc_perm *ipcp = &shmds.shm_perm;

  fprintf (stderr, "shmctl: calling shmctl (%d, %d, buffer)\n",
	shmid, cmd);
  if (cmd == IPC_SET) 
    fprintf (stderr, "uid=%d\t gid=%d\t  perms=%o\n", ipcp->uid, ipcp->gid, 
	     ipcp->mode & 0777);
  if ((i = shmctl (shmid, cmd, &shmds)) == -1) {
     perror ("shmctl : shmctl failed ");
     return;
  }  else
    fprintf (stderr, "shmctl returned %d\n", i);
   
  if (cmd != IPC_STAT && cmd != IPC_SET)
    return;
  else
    return do_stat ();
}
   
   
static void do_stat ()
{
  struct ipc_perm *ipcp = &shmds.shm_perm;

  fprintf (stderr, "Values in shmid_ds.shm_perm\n");
  fprintf (stderr, "uid=%d\t gid=%d\t cuid=%d\t cgid=%d\n",
	ipcp->uid, ipcp->gid, ipcp->cuid, ipcp->cgid);
  fprintf (stderr, "mode=%#o, access_perms=%#o\n", ipcp->mode, ipcp->mode & 0777);
  fprintf (stderr, "Values in shmid_ds\n");
  fprintf (stderr, "segsz = %d\t lpid = %d\t cpid = %d\t nattch = %d\n", 
	   shmds.shm_segsz, shmds.shm_lpid, shmds.shm_cpid, shmds.shm_nattch);
  fprintf (stderr, "atime = %s", shmds.shm_atime ? ctime (&shmds.shm_atime) : "Not set\n");
  fprintf (stderr, "dtime = %s", shmds.shm_dtime ? ctime (&shmds.shm_dtime) : "Not set\n");
  fprintf (stderr, "ctime = %s", ctime (&shmds.shm_ctime));
  return;
}
