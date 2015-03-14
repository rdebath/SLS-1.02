#include <stdio.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

#define MAXnap 6

static struct state {
  int shmid;
  char *addr;
  int shmflg;
} ap[MAXnap];
static int nap;
static jmp_buf segvbuf;

static ask(), good_addr();
static void catcher();

main ()
{
  int action, i;
  char *addr;
  struct state *p;
  void (*savefunc)();
  char line[100];
   
  fprintf (stderr, "All numeric input follows C conventions\n");
  while ((action = ask())) {
    if (nap) {
      fprintf (stderr, "\nCurrently attached segments: (shmid, address)\n");
      p = &ap[nap];
      while (p-- != ap) 
	fprintf (stderr, "%6d %#11x Read%s\n", p->shmid, p->addr,
		 (p->shmflg &SHM_RDONLY)? "-Only" : "/Write");
    } else
      fprintf (stderr, "No segments currently attached.\n");
    
    switch (action) {
    case 1: 
      if (nap == MAXnap) {
	fprintf (stderr, "Only %d segments can be attached.\n", MAXnap);
	break;
      }
      p = &ap[nap++];
      
      fprintf (stderr, "Enter shmid : ");
      scanf ("%i", &p->shmid);
      fprintf (stderr, "Enter shmaddr (page aligned) : ");
      scanf ("%i", &p->addr);
      
      fprintf (stderr, "Allowed flags for shmflg are:\n");
      fprintf (stderr, "SHM_RDONLY=%#8.8o  SHM_RND=%#8.8o\n",
	       SHM_RDONLY, SHM_RND);
      fprintf (stderr, "Enter desired shmflg value: ");
      scanf ("%i", &p->shmflg);
      
      fprintf (stderr, "Calling shmat (%d, %#x, %#o)\n", 
	       p->shmid, p->addr, p->shmflg);
      p->addr = shmat (p->shmid, p->addr, p->shmflg);
      if (p->addr == (char *) -1) {
	perror ("shmat : shmat failed ");
	nap--;
      }
      else
	fprintf (stderr, "shmat succeeded: shmaddr = %#8.8x\n", p->addr);
      break;

    case 2:
      fprintf (stderr, "Enter addr to detach :");
      scanf ("%i", &addr);
      i = shmdt (addr);
      if ( i == -1)
	perror ("shmdt : shmdt failed ");
      else {
	fprintf (stderr, "shmdt returned %d\n", i);
	for (p = ap, i=nap; i--; p++) {
	  if (p->addr == addr)
	    *p = ap[--nap];
	}
      }
      break;
      
    case 3:
      if (nap == 0)
	break;
      fprintf (stderr, "Enter address of attached segment :");
      scanf ("%i", &addr);
      if (good_addr (addr))
	fprintf (stderr, "String at %#x is `%s'\n", addr, addr);
      break;

    case 4:
      if (nap == 0)
	break;
      fprintf (stderr, "Enter address of attached segment :");
      scanf ("%i", &addr);
       
/*
      savefunc = signal (SIGSEGV, catcher);
       if (setjmp(segvbuf)) 
	fprintf (stderr, "shmop : SIGSEGV caught. Write aborted\n");
*/
      if (good_addr (addr)) {
	  fprintf (stderr, "Enter string to write:");
	  scanf ("%s", line);
	  strcpy (addr, line);
      }
      break;
    }
  }
  return (0);
}


static ask ()
{
  int res;
  
  do {
    fprintf (stderr, "options are: %s\n",
    "0 exit\t 1 shmat\t 2 shmdt\t 3 read\t 4 write");
    fprintf (stderr, "Enter choice :");
    res = 0;
    scanf ("%i", &res);
  } while (res < 0 || res > 4);
  return res;
}

static void catcher (int sig)
{
  longjmp (segvbuf, 1);
}

static good_addr (char *address)
{
  struct state *p;
  for (p = ap; p != &ap[nap]; p++)
    if (p->addr == address)
      return 1;
  return 0;
}
