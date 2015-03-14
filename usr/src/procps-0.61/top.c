/*
 * top.c		- show top CPU processes
 *
 * Copyright (c) 1992 Branko Lankester
 * Copyright (c) 1992 Roger Binns
 *
 * Snarfed and HEAVILY modified for the YAPPS (yet another /proc ps)
 * by Michael K. Johnson, johnsonm@stolaf.edu.  What is used is what
 * is required to have a common interface.
 *
 * Modified Michael K Johnson's ps to make it a top program.
 * Also borrowed elements of Roger Binns kmem based top program.
 * Changes made by Robert J. Nation (nation@rocket.sanders.lockheed.com)
 * 1/93
 *
 * Modified by Michael K. Johnson to be more efficient in cpu use
 * 2/21/93
 *
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <sys/ioctl.h>
#include <pwd.h>
#include <linux/sched.h>
#include <linux/tty.h>
#include <termcap.h>
#include <termios.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <ctype.h>
#include "sysinfo.h"
#include "ps.h"

/********************************************************************
 * this structure stores some critical information from one frame to
 * the next
 ********************************************************************/
struct save_hist {
  int ticks;
  int pid;
  int pcpu;
  int utime;
  int stime;
};
struct save_hist new_save_hist[NR_TASKS];

/********************************************************************
 * The header printed at the topo of the process list
 ********************************************************************/
char *hdr="  PID USER      PRI NI SIZE  RES SHRD ST %CPU %MEM  TIME COMMAND";


/********************************************************************
 * Misc function declarations
 ********************************************************************/
void do_setup();
void end();
void window_size();
void clear_screen();
void show_procs(struct ps_proc_head *ph, unsigned int maxcmd);
void show_load();
float get_elapsed_time();
unsigned int show_meminfo();
void do_stats(struct ps_proc_head *ph, float elapsed_time,int pass);
void show_task(struct ps_proc *this, unsigned int main_mem, int pcpu);
void do_key(char c);



/********************************************************************
 * Data used to control screen formatting
 ********************************************************************/
struct termio savetty;
struct termio rawtty;
char *cm, *cl, *clrtobot,*clrtoeol;
int lines,cols,maxlines;
int display_procs;
unsigned int maxcmd;

/********************************************************************
 * Controls how long we sleep between screen updates
 ********************************************************************/
float sleeptime = 5;



/********************************************************************
 * Main procedure. Contains loop which displays tasks, and then
 * sleeps/waits for input
 ********************************************************************/
int main()
{
  struct ps_proc_head *ph;
  struct timeval tv;
  fd_set in;
  char c;
  float elapsed_time;

  /* get termcap info, set screen into raw mode */
  do_setup();
  
  /* find out the window size, set hooks to catch window resize events */
  window_size();

  /* first time through, just collect process stats */
  ph = take_snapshot(1, 1, 1, 1, 0, 0, 0);
  elapsed_time = get_elapsed_time();
  do_stats(ph,elapsed_time,0);
  sleep(1);

  /* loop, collecting process info and sleeping */
  while(1)
    {
      /* display the tasks */
      show_procs(ph, maxcmd);
      
      /* sleep & wait for keyboard input */
      tv.tv_sec = sleeptime;
      tv.tv_usec = (sleeptime - (int)sleeptime) * 1000000;
      FD_ZERO(&in);
      FD_SET(0, &in);
      if (select(16, &in, 0, 0, &tv) > 0)
	if (read(0, &c, 1) == 1)
	  do_key(c);
    }
}


/********************************************************************
 * get termcap info for screen clearing, etc.
 * set terminal to raw mode 
 ********************************************************************/
void do_setup()
{
  char *buffer=0;
  char *termtype=getenv("TERM");
  struct termio newtty;

  /* set terminal mode */
  if(!termtype)
    { 
      printf("TERM not set\n"); 
      exit(1);
    }
  close(0);
  if(open("/dev/tty", O_RDONLY))
    printf("stdin is not there\n");
  if(ioctl(0, TCGETA, &savetty) == -1)
    {
      printf("stdin must be a tty\n");
      exit(1);
    }
  signal(SIGHUP, end);
  signal(SIGINT, end);
  newtty=savetty;
  newtty.c_lflag&=~ICANON;
  newtty.c_lflag&=~ECHO;
  newtty.c_cc[VMIN]=1;
  newtty.c_cc[VTIME]=0;
  if(ioctl(0, TCSETAF, &newtty)==-1)
    {
      printf("cannot put tty into raw mode\n");
      exit(1);
    }
  ioctl(0, TCGETA, &rawtty);

  /* get termcap entries */
  tgetent(buffer, termtype);
  cm=tgetstr("cm", 0);
  clrtobot=tgetstr("cd", 0);
  cl=tgetstr("cl",0);
  clrtoeol=tgetstr("ce", 0);
}


/********************************************************************
 * This procedure should be called to exit the program.
 * It restores the terminal to its original state 
 ********************************************************************/
void end()
{
  ioctl(0, TCSETAF, &savetty);
  printf("%s",tgoto(cm, 0,lines-1));
  exit(1);
}


/********************************************************************
 * reads the window size, clears the window, and sets itself up
 * to catch window resize events
 ********************************************************************/
void window_size()
{
  struct winsize ws;

  if (ioctl(1, TIOCGWINSZ, &ws) != -1) 
    {
      cols = ws.ws_col;
      lines = ws.ws_row;
    } 
  else 
    {
      cols=tgetnum("co");
      lines=tgetnum("li");
    }
  if(display_procs == 0)
    maxlines=lines-7;
  else
    maxlines = display_procs;
  signal(SIGWINCH, window_size);
  maxcmd = cols - strlen(hdr) + 7;
  clear_screen();
}



/********************************************************************
 * clears the screen
 ********************************************************************/
void clear_screen()
{
  printf("%s%s",cl,tgoto(cm,0,0));
}


/********************************************************************
 * This is the real program!
 * It reads process info and displays it.
 ********************************************************************/
void show_procs(struct ps_proc_head *ph, unsigned int maxcmd)
{
  struct ps_proc *this,*best;
  int count,top;
  int index,best_index;
  float elapsed_time;
  unsigned int main_mem;

  /* display the load averages */
  show_load();

  /* get the process info */
  ph = refresh_snapshot(ph, 1, 1, 1, 1, 0, 0, 0);
  /* immediately find out the elapsed time for the frame */
  elapsed_time = get_elapsed_time();
  
  /* display the system stats, also calculate percent CPU time */
  do_stats(ph,elapsed_time,1);

  /* display the memory and swap space usage */
  main_mem = show_meminfo();
  printf("%s%s",hdr,clrtoeol);
  
  /* finally! loop through to find the top task, and display it */
  count = 0;
  top = 100;
  while((count < maxlines)&&(top >= 0))
    {
      /* find the top of the remaining processes */
      top=-1;
      this = ph->head;
      best = this;
      best_index = 0;
      index=0;
      while(this !=NULL)
	{
	  if(new_save_hist[index].pcpu>top)
	    {
	      top = new_save_hist[index].pcpu;
	      best = this;
	      best_index = index;
	    }
	  index++;
	  this = this->next;
	}
      count++;
      if(top>=0)
	{
	  /* display the process */
	  show_task(best,main_mem,new_save_hist[best_index].pcpu);
	}
      new_save_hist[best_index].pcpu=-1;
    }
  printf("%s%s",clrtobot,tgoto(cm,0,5));

  /* make sure that the screen is updated */
  fflush(stdout);
}


/********************************************************************
 * Finds and displays the load average
 ********************************************************************/
void show_load()
{
  double av[3];

  loadavg(&av[0],&av[1],&av[2]);
  printf("%sLoad Averages %.2f %.2f %.2f%s\n",tgoto(cm,0,0),
	 av[0],av[1],av[2],clrtoeol);
}

/********************************************************************
 * Finds the current time (in microseconds) and finds the time
 * elapsed since the last update. This is essential for computing
 * percent CPU usage.
 ********************************************************************/
float get_elapsed_time()
{
  struct timeval time;
  static struct timeval oldtime;
  struct timezone timez;
  float elapsed_time;

  gettimeofday(&time,&timez);
  elapsed_time = (time.tv_sec - oldtime.tv_sec) +
    (float)(time.tv_usec - oldtime.tv_usec)/1000000.0;
  oldtime.tv_sec = time.tv_sec;
  oldtime.tv_usec= time.tv_usec;
  return elapsed_time;
}



/********************************************************************
 * Reads the memory info and displays it 
 * returns the total memory available for use in percent memory 
 * usage calculations.
 ********************************************************************/
unsigned int show_meminfo()
{
  char memory[1024];
  static int fd;
  unsigned int main_mem, used_mem, free_mem, shared_mem, buf_mem;
  unsigned int swap_mem, used_swap, free_swap;

  fd = open("/proc/meminfo", O_RDONLY, 0);
  if (fd == -1) 
    {
      perror("ps.c:/proc/meminfo");
      end();
    }
  read(fd,memory,sizeof(memory)-1);
  close(fd);
  sscanf(memory, "%*s %*s %*s %*s %*s %*s %u %u %u %u %u %*s %u %u %u",
	 &main_mem, &used_mem, &free_mem, &shared_mem, &buf_mem,
	 &swap_mem, &used_swap, &free_swap);
  printf("Mem:  %5dK av, %5dK used, %5dK free, %5dK shrd, %5d buff%s\n",
	 main_mem/1024, used_mem/1024, free_mem/1024, 
	 shared_mem/1024, buf_mem/1024,clrtoeol);
  printf("Swap: %5dK av, %5dK used, %5dK free%s\n%s\n",
	 swap_mem/1024, used_swap/1024, free_swap/1024,clrtoeol,
	 clrtoeol);
  return main_mem;
}


/********************************************************************
 * Calculates the number of tasks in each state (running, sleeping, etc.)
 * Calculates the CPU time in each state (system, user, nice, etc)
 * calculates percent cpu usage for each task 
 ********************************************************************/
void do_stats(struct ps_proc_head *ph,float elapsed_time,int pass)
{
  struct ps_proc *this;
  int index,total_time,i;
  int sleeping = 0,stopped = 0,zombie = 0,running = 0;
  int system_ticks = 0,user_ticks = 0,nice_ticks = 0,idle_ticks = 1000;
  static int prev_count=0;
  static struct save_hist save_hist[NR_TASKS];
  int stime, utime;
  /* make sure that there aren't too many tasks */
  if(ph->count >NR_TASKS)
    {
      printf("Help! Too many tasks!");
      end();
    }

  /* make a pass through the data to get stats */
  index=0;
  this = ph->head;
  while(this != NULL)
    {
      if((this->state == 'S')||(this->state == 'D'))
	sleeping++;
      else if(this->state == 'T')
	stopped++;
      else if(this->state == 'Z')
	zombie++;
      else if(this->state == 'R')
	running++;

      /* calculate time in this process */
      /* time is sum of user time (utime) plus system time (stime) */
      total_time = this->utime + this->stime;
      new_save_hist[index].ticks = total_time;
      new_save_hist[index].pid = this->pid;
      stime = this->stime;
      utime = this->utime;
      new_save_hist[index].stime = stime;
      new_save_hist[index].utime = utime;
      /* find matching entry from previous pass*/
      i=0;
      while(i<prev_count)
	{
	  if(save_hist[i].pid == this->pid)
	    {
	      total_time -= save_hist[i].ticks;
	      stime -= save_hist[i].stime;
	      utime -= save_hist[i].utime;

	      i = NR_TASKS;
	    }
	  i++;
	}
      /* calculate percent cpu time for this task */
      new_save_hist[index].pcpu = (total_time * 10) /elapsed_time;
      if (new_save_hist[index].pcpu > 999)
	new_save_hist[index].pcpu = 999;

      /* calculate time in idle, system, user and niced tasks */
      idle_ticks -= new_save_hist[index].pcpu;
      system_ticks += stime;
      user_ticks += utime;
      if(this->priority < 15)
	nice_ticks += new_save_hist[index].pcpu;

      index++;
      this = this->next;
    }

  if(idle_ticks < 0)
    idle_ticks = 0;
  system_ticks = (system_ticks * 10) /elapsed_time;      
  user_ticks = (user_ticks * 10) /elapsed_time;

  /* display stats */
  if(pass>0)
    {
      printf("%d processes: %d sleeping, %d running, %d zombie, %d stopped%s\n",
	     ph->count,sleeping,running,zombie,stopped,clrtoeol);
      printf("CPU states: %2d.%d%% user, %2d.%d%% nice,",
	     user_ticks/10, user_ticks%10,
	     nice_ticks/10, nice_ticks%10);
      printf(" %2d.%d%% system, %2d.%d%% idle%s\n",
	     system_ticks/10, system_ticks%10,
	     idle_ticks/10, idle_ticks%10,clrtoeol);
    }

  /* save this frame's information */
  for(i=0;i<ph->count;i++)
    {
      /* copy the relevant info for the next pass */
      save_hist[i].pid = new_save_hist[i].pid;
      save_hist[i].ticks = new_save_hist[i].ticks;
      save_hist[i].stime = new_save_hist[i].stime;
      save_hist[i].utime = new_save_hist[i].utime;
    }
  prev_count = ph->count;
}


/********************************************************************
 * displays information relevant to each task
 ********************************************************************/
void show_task(struct ps_proc *this,unsigned int main_mem, int pcpu)
{
  int pmem;
  unsigned int t;
  char *cmdptr;

  /* show task info */
  pmem = this->rss * 1000 / (main_mem / 4096);
  printf("\n%5d %-8s %3d %3d %4d %4d %4d %c  %2d.%d %2d.%d", 
	 this->pid,this->user,this->counter,this->priority-15,
	 this->vsize/1024,this->rss * 4,this->statm.share << 2,
	 this->state, pcpu / 10, pcpu % 10, pmem / 10, pmem % 10);

  /* show total CPU time */
  t = (this->utime + this->stime) / HZ;
  printf("%3d:%02d ", t / 60, t % 60);
  
  /* show command line */
  if(strlen(this->cmdline) > 0)
    cmdptr = this->cmdline;
  else
    cmdptr=this->cmd;
  if (strlen(cmdptr) > maxcmd)
    cmdptr[maxcmd - 1] = 0;
  printf("%s%s",cmdptr,clrtoeol);
}


/********************************************************************
 * processes keyboard input
 ********************************************************************/
void do_key(char c)
{
  int pid,signal,val;
  char junk;

  c = toupper(c);

  /* first the commands which don't require a terminal mode switch */
  /* 'Q' or 'q' for quit */
  if(c=='Q')
    end();
  /* ^L to clear and re-draw the screen */
  else if(c==12)
    {
      clear_screen();
      return;
    }

  /* switch the terminal to normal mode */
  ioctl(0,TCSETA,&savetty);

  /* process other inputs */
  /* 'N' or 'n' or '#' to change the number of processes displayed */
  if((c=='N')||(c=='#'))
    {
      printf("Display how many processes? ");
      scanf("%d",&display_procs);
      maxlines = display_procs;
    }
  /* 's' or 'S' to change the sleep time */
  else if(c=='S')
    {
      printf("Delay between updates: ");
      scanf("%f",&sleeptime);
    }
  /* 'k' or 'K' to kill a task */
  else if(c=='K')
    {
      printf("Kill PID: ");
      scanf("%d",&pid);
      printf("%s%sKill PID %d with signal: ",tgoto(cm,0,5),clrtoeol,pid);
      scanf("%d",&signal);
      kill(pid,signal);
      if(errno)
	{
	  printf("%s%s\007Kill of PID %d failed: %s",tgoto(cm,0,5),clrtoeol,
		 pid,strerror(errno));
	  fflush(stdout);
	  sleep(3);
	}
    }
  /* 'r' or 'R' to renice a task */
  else if(c=='R')
    {
      printf("Renice PID: ");
      scanf("%d",&pid);
      printf("%s%sRenice PID %d to value: ",tgoto(cm,0,5),clrtoeol,pid);
      scanf("%d",&val);
      setpriority(PRIO_PROCESS,pid,val);      
      if(errno)
	{
	  printf("%s%s\007Renice of PID %d failed: %s",tgoto(cm,0,5),clrtoeol,
		 pid,strerror(errno));
	  fflush(stdout);
	  sleep(3);
	}
    }
  /* 'h' or 'H' or '?' to get help */
  else if((c=='H')||(c=='?'))
    {
      printf("%s%s\n\t\t\tProc-Top Revision 0\n\n",cl,tgoto(cm,0,0));
      printf("Interactive commands are:\n\n");
      printf("? or h   Print this list\n");
      printf("k        Kill a task. You will be prompted for the PID and");
      printf(" signal\n");
      printf("r        Renice a task. You will be prompted for the PID and");
      printf(" nice level\n");
      printf("s        Set the delay in seconds between updates\n");
      printf("n        Set the number of process to show\n");
      printf("^L       Redraw the screen\n");
      printf("q        Quit\n");
      printf("\n\n\t\t\tPress RETURN to continue\n");
      scanf("%c",&junk);
    }
  /* back to raw mode */
  ioctl(0,TCSETA,&rawtty);
  return;
}
