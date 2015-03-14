/***
 ** phil2.c, 920520
 **  dvldbg@cs.umu.se
 ** dining philisophers, as an actions-system
***/

#include <errno.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#define Case break; case

int r(max)
{
    return random() % max;
}

int p1,p2,n = 5,np = 3;
int semkey, *semid = 0, semid2 = -1;
struct sembuf sops;
FILE *lf;
union semun ops;

#define HUNGRY 0
#define PBUSY 1
#define RESERV 2
#define TOKEN 3
#define FBUSY 4

int trydec(p,v)
{
    sops.sem_num = v;
    sops.sem_op = -1;
    sops.sem_flg = IPC_NOWAIT;
    return (semop(semid[p],&sops,1) > -1);
}

int isone(p,v)
{
    ops.val = 0;
    return semctl(semid[p],v,GETVAL,ops);
}

int iszero(p,v)
{
    ops.val = 0;
    return (semctl(semid[p],v,GETVAL,ops) == 0);
}

void set0(p,v)
{
    ops.val = 0;
    semctl(semid[p],v,SETVAL,ops);
}

void set1(p,v)
{
    ops.val = 1;
    semctl(semid[p],v,SETVAL,ops);
}

void doit(pnum)
{
    int counter = 100;

    while (counter-- > 0) {
	fflush(lf);
        p1 = r(n);
        p2 = (p1 + 1) % n;

        switch (r(3)) {
        case 0:     /* thinking */
	    if (trydec(p1,PBUSY)) {
		if (iszero(p1,HUNGRY)) {
                    fprintf(lf,"phil %d is thinking\n",p1);
		    set1(p1,HUNGRY);
		}
		set1(p1,PBUSY);
	    }
        Case 1:     /* reserve */
	    if (trydec(p1,PBUSY)) {
		if (trydec(p1,FBUSY)) {
		    if (isone(p1,HUNGRY) && iszero(p1,RESERV)) {
                	fprintf(lf,"phil %d is reserving his fork\n",p1);
			set1(p1,RESERV);
		    }
		    set1(p1,FBUSY);
		}
		set1(p1,PBUSY);
	    }
        Case 2:		/* eat */
	    if (trydec(p1,PBUSY)) {
		if (trydec(p1,FBUSY)) {
		    if (trydec(p2,FBUSY)) {
			if (isone(p1,HUNGRY) &&
				(iszero(p2,RESERV) || iszero(p2,TOKEN))) {
                    	    fprintf(lf,"phil %d is eating\n",p1);
			    fflush(lf);
			    set0(p1,HUNGRY);
			    if (isone(p1,TOKEN)) {
				set0(p1,TOKEN);
				set1(p2,TOKEN);
			    }
			    set0(p1,RESERV);
			    sleep(1);
			    fprintf(lf,"phil %d has finished eating\n",p1);
			}
			set1(p2,FBUSY);
		    }
		    set1(p1,FBUSY);
		}
		set1(p1,PBUSY);
	    }
        }
    }
    sops.sem_num = 0;
    sops.sem_op = 1;
    sops.sem_flg = 0;
    semop(semid2,&sops,1);
}

void quit(r,n1)
{
    ops.val = 0;
    if (r) printf("Maximum number of semaphores reached.\n");
    while (n1-- > 0) semctl(semid[n1],0,IPC_RMID,ops);
    if (semid2 != -1) semctl(semid2,0,IPC_RMID,ops);
    if (semid) free(semid);
    exit(r);
}


void sigcatch (int n1)
{
	printf ("caught interrupt\n");
	quit (0, n1);
}


main(argc,argv)
int argc;
char *argv[];
{
    int ns,np2;

    signal (SIGINT, sigcatch);

    while (argc-- > 1) {
	ns = atoi(argv[argc]);
	if (ns > 0) n = ns;		/* number of philosophers */
	else if (ns < 0) np = -ns;	/* number of processes */
    }
    printf("There are %d philosophers and %d processes\n",n,np);
    semid = (int *)calloc(n,sizeof(int));
    if (!semid) perror("calloc"),exit(1);

    lf = fopen("philolog","a");
    semkey = ftok(argv[0],'!'); 
    np2 = n;
    ns = 0;
    while (np2-- > 0) {
newsem:
	semid[ns] = semget(semkey,5,IPC_CREAT | IPC_EXCL | 0600);
	if (semid[ns] == -1) {
	    switch(errno) {
	    case ENOSPC:
		quit(1,ns);
	    case EACCES:
	    case EEXIST:
		semkey++;
		goto newsem;
	    default:
		perror("semget");
		exit(1);
	    }
	}
	set1(ns,PBUSY);
	set1(ns,FBUSY);
	ns++;
	semkey++;
    }
    set1(0,TOKEN);

newsem2:
    semid2 = semget(semkey,1,IPC_CREAT | IPC_EXCL | 0600);
    if (semid2 == -1) {
	switch(errno) {
	case ENOSPC:
	    quit(2,n);
	case EACCES:
	case EEXIST:
	    semkey++;
	    goto newsem2;
	default:
	    perror("semget");
	    exit(1);
	}
    }

    srandom(getpid());
    fflush(lf);
    np2 = np;
    while (np-- > 0) {
	if (!fork()) {
	    doit(np);
	    exit(0);
	}
    }
    sops.sem_num = 0;
    sops.sem_op = -np2;
    sops.sem_flg = 0;
    semop(semid2,&sops,1);
    quit(0,n);
}

