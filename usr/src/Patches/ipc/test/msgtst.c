/*
** sysv ipc msg testing program mostly from Bach.
** Start a server process. Then start several clients
** on another vt.
** Kill the server with a ^C when your done.
** A client sends a message (type =1) containing its pid and then 
** waits for a message with type=clients pid.
** The server waits for messages of any type.
** Upon receiving one it sends a message with type = clients pid.
*/

#include <stdio.h>
#include <errno.h>
#include <sys/msg.h>
#include <signal.h>

struct msgform {
  long mtype;
  char mtext[256];
};
static void client (key_t key);
static void server (key_t key);

main(int argc, char **argv)
{
  key_t key;

  if (argc != 3)
    {
      printf ("usage : %s [client | server] key\n", argv[0]);
	return 0;
    }
  key = atoi (argv[2]);
  if (argv[1][0] == 'c')
    client(key);
  else if (argv[1][0] == 's')
    server(key);
  return 0;
}

static void client(key_t key)
{
  struct msgform msg;
  struct msqid_ds buf;
  int x, msqid, pid, *pint;

  pint = (int *) msg.mtext;
  pid = getpid();
  msg.mtype = 1;
  *pint = pid;
  if ((msqid = msgget (key, 0777)) < 0)
    {
      perror ("msgget ");
      return ;
    }
  if (msgctl (msqid, IPC_STAT, &buf) < 0)
    {
      perror ("msgctl : IPC_STAT ");
      return ;
    }
  else 
    {
      printf ("client pid=%d\t", pid);
      printf ("sending: ");
      if(( x = msgsnd (msqid, (struct msgbuf *)&msg, sizeof (int), 0)) < 0)
	perror ("msgsnd ");
      else
	printf ("sent %d bytes ... receiving: ",x);
      if ((x = msgrcv (msqid, (struct msgbuf *) &msg, 256, pid, 0)) < 0)
	perror ("msgrcv ");
      else
	printf ("rcvd %d bytes from pid %d\n", x, *pint);
    }
  return;
}

static msqid;

void cleanup(int i)
{
  if (msgctl (msqid, IPC_RMID, 0) == -1)
    perror ("msgctl : IPC_RMID ");
  else
    printf ("msg resource deleted\n");
  exit (0);
}

static void server(key_t key)
{
    int i, x, pid, *pint, servpid;
    struct msgform msg;
    char c;

    servpid = getpid();
    pint = (int *) msg.mtext;
    signal (SIGTERM, cleanup);
    signal (SIGHUP, cleanup);
    signal (SIGINT, cleanup);
    signal (SIGKILL, cleanup);
    signal (SIGQUIT, cleanup);
    if ((msqid = msgget (key, 0777 | IPC_CREAT | IPC_EXCL)) == -1) {
        perror ("msgget : ");
        return ;
    }
    printf ("starting server msqid = %d\n", msqid);

    while (1) {
        if ((x = msgrcv (msqid, (struct msgbuf *)&msg, 256, 1, 0)) == -1)
	  perror ("server : msgrcv ");
        else {
	    pid = *pint;
	    printf ("received %d bytes from pid %d\t", x, pid);
	    msg.mtype = pid;
	    *pint = servpid;
	    if ((x = msgsnd (msqid, (struct msgbuf *)&msg, sizeof(int),0)) == -1)
	      perror ("server : msgsnd ");
	    else
	      printf ("sent %d bytes to pid %d\n", x, pid);
	}
        fflush (stdout);
    }
    if (msgctl (msqid, IPC_RMID, 0) == -1)
        perror ("msgctl : IPC_RMID ");
    else
        printf ("msg resource deleted\n");
  exit (0);
}




