#include <stdio.h> 	/* printf ... */
#include <errno.h>	/* perror ...*/
#include <stdlib.h>	/* atoi ... */
#include <unistd.h>	/* syscalls */
#include <sys/wait.h>	/* wait */
#include <sys/ipc.h>	/* ftok */
#include <sys/shm.h>	/* shmget .. */

/*
 * krishna balasubramanian 1/14/93
 * Test behavior of shm attaches on a fork
 */
int main(int argc, char **argv)
{
	int i, k, shmid, pid, cpid;
	int pages, nfork;
	volatile int child = 0;
	char *addr, *shmaddr;
	key_t key;
	struct shmid_ds buf;	

	if (argc != 4) {
		printf ("usage: %s size(pages) addr(hex) nfork\n", argv[0]);
		exit (0);
	}
	pages = atoi (argv[1]);
	addr = (char *) strtoul (argv[2], NULL, 16);
	nfork = atoi (argv[3]);


	key = ftok (argv[0], 1);
	if ((shmid = shmget (key, pages << 12, IPC_EXCL | 01600 )) < 0) {
		perror ("shmget ");
		exit (1);
	}
	printf ("\nShared segment with %d pages allocated.\n", pages);
	printf ("Requesting attach (SHM_RND) at %p\n", addr);
	if ((shmaddr = shmat (shmid, addr, SHM_RND)) == (char *) -1) {
		perror ("shmat ");
		if (shmctl (shmid, IPC_RMID, NULL))
			perror ("shmctl ");
		exit (1);
	}
	printf ("shmat : segment attached at shmaddr = %p\n", shmaddr);

	if (shmctl (shmid, IPC_RMID, NULL))
		perror ("shmctl ... ");
	else
		printf ("shmctl : segment marked for deletion\n");

	printf ("Forking %d times.\n", nfork);
	fflush (stdout);

	/* write to each page of attached segments */
	for (k=0; k<nfork; k++) {
		switch (pid = fork()) {
		case -1:
			perror ("fork ");
			child--;
			break;
		case 0:
			cpid = getpid();
			addr = shmaddr + 64*k;
			if (addr > shmaddr + (pages << 12)) {
				printf ("child %d pid=%d. not writing\n");
				exit (0);
			}
			if (shmctl (shmid, IPC_STAT, &buf))
				buf.shm_nattch = 1800;
			sprintf (addr, "child %d reporting. pid=%d nattch=%d", 
				k, cpid, buf.shm_nattch);
			exit (0);
			break;
		default :
			child++;
			break;
		}
	}

	for (i=0; i< child; i++) 
		pid = wait (0);
	printf ("Final state of shared segment:\n");
	printf ("address\t\tcontents\n");
	for (k=0; k < nfork; k++) {
		addr = shmaddr + 64*k;
		if (addr > shmaddr + (pages << 12))
			break;
		printf ("%p  %s\n", addr, addr); 
	}
	printf ("\n");
	return 0;
}
