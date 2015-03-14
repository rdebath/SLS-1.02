#include <stdio.h>
#include <sys/msg.h>
#include <errno.h>
#include <time.h>

static int do_msgctl (int msqid, int cmd, struct msqid_ds *buf);

int main()
{
  struct msqid_ds  buf;
  int cmd, msqid;
  struct ipc_perm *ipcp = &buf.msg_perm;

  printf ("All numeric input follows C conventions\n");
  printf ("Enter desired msqid: ");
  scanf ("%i", &msqid);

  printf ("Valid msgctl cmds are: \n");
  printf ("IPC_RMID=%d\t IPC_SET=%d\t IPC_STAT=%d\n", 
		IPC_RMID, IPC_SET, IPC_STAT);
  printf ("Enter desired cmd: ");
  scanf ("%i", &cmd);

  switch (cmd) {
  case IPC_SET:
    printf ("Values before IPC_SET:\n");
    /* fall thru */
  case IPC_STAT:
    if (do_msgctl (msqid, IPC_STAT, &buf) == 0)
	{
    printf ("msg_perm:\t uid=%d\t gid=%d\t cuid=%d\t cgid=%d\t mode=%#o\n",
	    ipcp->uid, ipcp->gid, ipcp->cuid, ipcp->cgid, ipcp->mode);
    printf ("msq:\t cbytes=%d\t qbytes=%d\t qnum=%d\t lspid=%d\t lrpid=%d\n",
	    buf.msg_cbytes, buf.msg_qbytes, buf.msg_qnum, buf.msg_lspid, buf.msg_lrpid);
    printf ("first message=%x\t last message=%x\n", buf.msg_first, buf.msg_last);
    printf ("stime=%s rtime=%s ctime=%s", 
	    buf.msg_rtime? ctime (&buf.msg_rtime) : "Not Set\n",
	    buf.msg_stime? ctime (&buf.msg_stime) : "Not Set\n",
	    buf.msg_ctime? ctime (&buf.msg_ctime) : "Not Set\n");
	}
    if (cmd == IPC_STAT)
      break;
    printf ("Enter desired uid: ");
    scanf ("%hi", &ipcp->uid);
    printf ("Enter desired gid: ");
    scanf ("%hi", &ipcp->gid);
    printf ("Enter desired mode: ");
    scanf ("%hi", &ipcp->mode);
    printf ("Enter desired qbytes (super user only): ");
    scanf ("%hi", &buf.msg_qbytes);
    do_msgctl (msqid, IPC_SET, &buf);
    break;
  case IPC_RMID:
  default:
    do_msgctl (msqid, cmd, (struct msqid_ds *) NULL);
    break;
  }
  return;
}

static int
do_msgctl (int msqid, int cmd, struct msqid_ds *buf)
{
  register int rtrn;
  printf ("calling msgctl (%d, %d, %s)\n",
	  msqid, cmd, buf? "&buf" : "NULL");

  if ((rtrn = msgctl (msqid, cmd, buf)) == -1)
    perror ("msgctl ");
  else
    printf ("msgctl returned = %d\n", rtrn);
  return rtrn;
}
