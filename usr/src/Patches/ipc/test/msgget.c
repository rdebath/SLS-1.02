#include <stdio.h>
#include <sys/msg.h>
#include <errno.h>

main()
{
  key_t key;
  int msgflg, msqid;

  printf ("All numeric input follows C conventions\n");
  printf ("(IPC_PRIVATE=%#lx)\t Enter desired key: ", IPC_PRIVATE);
  scanf ("%li", &key);

  printf ("Expected flags for msgflg (besides 0666) are :\n");
  printf ("IPC_EXCL=%#8.8o\t IPC_CREAT=%#8.8o\n", IPC_EXCL, IPC_CREAT);
  printf ("Enter desired flags: ");
  scanf ("%i", &msgflg);

  if ((msqid = msgget (key, msgflg)) == -1)
    perror ("msgget ");
  else
    printf ("msgget succeeded msqid = %d\n", msqid);
  return;
}
