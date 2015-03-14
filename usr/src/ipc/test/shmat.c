/*
 * krishna balasubramanian 1993
 * check shm multiple attaches are actually sharing memory!
 */

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>
static char **shmaddr;

int main(int argc, char **argv)
{
	int i, shmid, *addr;
	int pages, nattch;
	char id;

	if (argc != 3) {
		printf ("usage: %s pages nattch\n", argv[0]);
		exit (0);
	}
	pages = atoi (argv[1]);
	nattch = atoi (argv[2]);
	
	shmaddr = (char **) malloc (nattch * sizeof (ulong));
	if (!shmaddr) {
		perror ("malloc ");
		exit (1);
	}

	if ((shmid = shmget (IPC_PRIVATE, pages << 12, 01600)) < 0) {
		perror ("shmget ");
		exit (1);
	}
	printf ("\nAllocated %d pages, shmid=%d\n", pages, shmid);
	if ((shmaddr[0] = shmat (shmid, 0, 0)) == (char *) -1) {
		perror ("shmat ");
		exit (1);
	}
	printf ("shmat succeded : shmaddr[%4d] = %p\n", 0, shmaddr[0]);

	if (shmctl (shmid, IPC_RMID, NULL)) {
		perror ("shmctl ");
		exit (1);
	}
	else
		printf ("shmctl : Resource marked for deletion\n");

	for (i=1; i < nattch; i++) {
		if ((shmaddr[i] = shmat (shmid, 0, 0)) == (char *) -1) {
			perror ("shmat ");
			exit (1);
		}
		printf ("shmat succeded : shmaddr[%4d] = %p\n", i, shmaddr[i]);
	}

	printf ("Writing attach index i to address (shmaddr[i] + i)");	
	for (i=0; i < nattch; i++) {
		addr = (int *)shmaddr[i]; 
		*(addr + i) = i;
	}

	printf (" ... done.\nContents of first page:\n");
	addr = (int *) shmaddr[0];
	for (i=0; i < nattch; i++)
		printf ("%4d", *(addr+i)); 
	printf ("\nDetach/deletion on exit ... bye.\n\n");

	return 0;
}
			
