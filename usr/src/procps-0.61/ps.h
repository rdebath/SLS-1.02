/*
 * ps.h
 *
 * Copyright (c) 1992 Branko Lankester
 *
 * Modified heavily by Michael K. Johnson
 */


#include <linux/sched.h>


#define	MAXCMD	1024	/* max # bytes to write from the command line */


struct ps_statm {
  int size, resident, share, trs, lrs, drs, dt;
};

struct ps_proc {
  char user[10], cmdline[256], cmd[40], ttyc[3], state;
  int uid, pid, ppid, pgrp, session, tty, tpgid, utime, stime,
    cutime, cstime, counter, priority, start_time, signal, blocked,
    sigignore, sigcatch;
  unsigned int flags, min_flt, cmin_flt, maj_flt, cmaj_flt, timeout,
    it_real_value, vsize, rss, rss_rlim, start_code, end_code,
    start_stack, kstk_esp, kstk_eip, wchan;
  struct ps_statm statm;
  struct ps_proc *next;
};

struct ps_proc_head {
  struct ps_proc *head;
  int count;
};



char *find_func();
void dev_to_tty(char *tty, int dev);
char *wchan(unsigned int);
char *status();
void *xcalloc(void *pointer, int size);

/* a, u, x, m, and r correspond to those command line options: if the
   variable is set, then the corresponding command line option was
   chosen. */
struct ps_proc_head *take_snapshot(char a, char u, char x, char m, char r,
				   uid_t uid, int ctty);
struct ps_proc_head *refresh_snapshot(struct ps_proc_head *ph,
				      char a, char u, char x, char m, char r,
				      uid_t uid, int ctty);
struct ps_proc_head *get_process(int pid, int m);
void free_psproc(struct ps_proc *this);
void dev_to_tty(char *tty, int dev);
int tty_to_dev(char *tty);
char *user_from_uid(int uid);
int open_psdb(void);
void close_psdb(void);
