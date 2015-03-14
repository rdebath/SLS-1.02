/* Loosely derived from a simple hack posted to c.o.l
 * -Michael K. Johnson, johnsonm@stolaf.edu
 *
 * Some pieces from Branko Lankester's kmem ps, copyright 1992 Branko Lankester
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/dir.h>
#include <regex.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <pwd.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "psdata.h"
#include "ps.h"


int mycpy(char *directory, char *ret, char *what, int cap, int nulls)
{
  static char filename[80];
  int fd;
  int nr_read, i;

  sprintf(filename, "/proc/%s/%s", directory, what);
  fd = open(filename, O_RDONLY, 0);
  if (fd != -1) {
    nr_read = read(fd, ret, cap-1);
    ret[nr_read]=0;
    if (nulls)
      for (i=0; i < nr_read; i++)
	if (ret[i]==0) ret[i]=' ';
  } else return 0;
  close(fd);
  return 1;
}



struct ps_proc_head *take_snapshot(char a, char u, char x, char m, char r,
				   uid_t uid, int ctty)
{
  DIR *proc;
  static struct direct *ent;
  static char filename[80];
  static char stat_str[4096];
  struct ps_proc_head *ph = NULL;
  struct ps_proc *this = NULL, *that = NULL;
  struct stat sb;

  proc = opendir("/proc");
  re_comp("^[0-9]*$");

  ph = (struct ps_proc_head *) xcalloc(ph, sizeof(struct ps_proc_head));
  /* initializes ph->head and ph->count to zero ;-) */
  ph->head = (struct ps_proc *) xcalloc(ph->head, sizeof(struct ps_proc));
  this = ph->head;

  while((ent = readdir(proc))) { /* Extra parens to make gcc -Wall happy... */
    if(!re_exec(ent->d_name)) continue;
    sprintf(filename, "/proc/%s", ent->d_name);
    stat(filename, &sb);
    if(!a && (sb.st_uid != uid)) continue;
    this->uid = sb.st_uid;
    mycpy(ent->d_name, this->cmdline, "cmdline", sizeof(this->cmdline), 1);
    if(!mycpy(ent->d_name, stat_str, "stat", sizeof(stat_str), 0)) continue;

    sscanf(stat_str, "%d %s %c %d %d %d %d %d %u %u \
%u %u %u %d %d %d %d %d %d %u %u %d %u %u %u %u %u %u %u %u %d \
%d %d %d %u",
	   &this->pid, this->cmd, &this->state, &this->ppid,
	   &this->pgrp, &this->session, &this->tty, &this->tpgid,
	   &this->flags, &this->min_flt, &this->cmin_flt,
	   &this->maj_flt, &this->cmaj_flt,
	   &this->utime, &this->stime, &this->cutime, &this->cstime,
	   &this->counter, &this->priority, &this->timeout,
	   &this->it_real_value, &this->start_time,
	   &this->vsize, &this->rss, &this->rss_rlim,
	   &this->start_code, &this->end_code, &this->start_stack,
	   &this->kstk_esp, &this->kstk_eip,
	   &this->signal, &this->blocked, &this->sigignore, &this->sigcatch,
	   &this->wchan);
    if ((ctty && (ctty != this->tty))
	|| (r && this->state != 'R' && this->state != 'D')
	|| (!x && (this->tty == -1))) {
      this->pid = 0;
      continue;
    }
    /* 0 normally passed, which is never the value given as the tty from the
       proc filesystem, so this only happens if a specific tty was passed. */
    if(m) {
      if(!mycpy(ent->d_name, stat_str, "statm", sizeof(stat_str), 0)) continue;
      sscanf(stat_str, "%d %d %d %d %d %d %d",
	     &this->statm.size, &this->statm.resident,
	     &this->statm.share, &this->statm.trs,
	     &this->statm.lrs, &this->statm.drs,
	     &this->statm.dt);
    }
    if (this->state == 'Z') strcat(this->cmd," <zombie>");
    dev_to_tty(this->ttyc, this->tty);
    if(u) strncpy(this->user, user_from_uid(this->uid), 9);

    /* update the linked list and increase the count */
    if(this->pid) {
      that = this;
      this->next = (struct ps_proc *) xcalloc(this->next,
					      sizeof(struct ps_proc));
      this = this->next;
      ph->count++;
    }
  } /* end of the while loop */
  closedir(proc);
  if(!this->pid) { /* I beleive this will always be true, because it will try
		      one more readdir, and there will be a hanging entry... 
		      But I make it conditional to be safe */
    that->next = (struct ps_proc *) NULL;
    free (this);
  } else this->next = (struct ps_proc *) NULL;
  return ph;
}


struct ps_proc_head *get_process(int pid, int m) {
  
  static char stat_str[256];
  static char filename[80];
  char *fn = filename;
  struct ps_proc_head *ph = NULL;
  struct ps_proc *ret = NULL;
  struct stat sb;

  ph = (struct ps_proc_head *) xcalloc(ph, sizeof(struct ps_proc_head));
  ret = (struct ps_proc *) xcalloc(ret, sizeof(struct ps_proc));

  sprintf(fn, "/proc/%u", pid);
  stat(fn, &sb);
  ret->uid = sb.st_uid;
  fn += 6; /* cut "/proc/" out of fn cheaply */
  mycpy(fn, ret->cmdline, "cmdline", sizeof(ret->cmdline), 1);
  if(!mycpy(fn, stat_str, "stat", sizeof(stat_str), 0))
    { free(ret); return ph; }

  sscanf(stat_str, "%d %s %c %d %d %d %d %d %u %u \
%u %u %u %d %d %d %d %d %d %u %u %d %u %u %u %u %u %u %u %u %d \
%d %d %d %u",
	 &ret->pid, ret->cmd, &ret->state, &ret->ppid,
	 &ret->pgrp, &ret->session, &ret->tty, &ret->tpgid,
	 &ret->flags, &ret->min_flt, &ret->cmin_flt,
	 &ret->maj_flt, &ret->cmaj_flt,
	 &ret->utime, &ret->stime, &ret->cutime, &ret->cstime,
	 &ret->counter, &ret->priority, &ret->timeout,
	 &ret->it_real_value, &ret->start_time,
	 &ret->vsize, &ret->rss, &ret->rss_rlim,
	 &ret->start_code, &ret->end_code, &ret->start_stack,
	 &ret->kstk_esp, &ret->kstk_eip,
	 &ret->signal, &ret->blocked, &ret->sigignore, &ret->sigcatch,
	 &ret->wchan);
  if(m) {
    if(!mycpy(fn, stat_str, "statm", sizeof(stat_str), 0))
      { free(ret); return ph; }
    sscanf(stat_str, "%d %d %d %d %d %d %d",
	   &ret->statm.size, &ret->statm.resident,
	   &ret->statm.share, &ret->statm.trs,
	   &ret->statm.lrs, &ret->statm.drs,
	   &ret->statm.dt);
  }
  if (ret->state == 'Z') strcat(ret->cmd," <zombie>");
  dev_to_tty(ret->ttyc, ret->tty);
  strncpy(ret->user, user_from_uid(ret->uid), 9);
  ph->head = ret; ph->count = 1;
  return ph;
}



struct ps_proc_head *refresh_snapshot(struct ps_proc_head *ph,
				      char a, char u, char x, char m, char r,
				      uid_t uid, int ctty)
{
  DIR *proc;
  static struct direct *ent;
  static char filename[80];
  static char stat_str[4096];
  struct ps_proc *this = NULL, *that = NULL;
  struct stat sb;

  proc = opendir("/proc");
  re_comp("^[0-9]*$");

  ph->count = 0;
  this = ph->head;

  while((ent = readdir(proc))) { /* Extra parens to make gcc -Wall happy... */
    if(!re_exec(ent->d_name)) continue;
    sprintf(filename, "/proc/%s", ent->d_name);
    stat(filename, &sb);
    if(!a && (sb.st_uid != uid)) continue;
    this->uid = sb.st_uid;
    mycpy(ent->d_name, this->cmdline, "cmdline", sizeof(this->cmdline), 1);
    if(!mycpy(ent->d_name, stat_str, "stat", sizeof(stat_str), 0)) continue;

    sscanf(stat_str, "%d %s %c %d %d %d %d %d %u %u \
%u %u %u %d %d %d %d %d %d %u %u %d %u %u %u %u %u %u %u %u %d \
%d %d %d %u",
	   &this->pid, this->cmd, &this->state, &this->ppid,
	   &this->pgrp, &this->session, &this->tty, &this->tpgid,
	   &this->flags, &this->min_flt, &this->cmin_flt,
	   &this->maj_flt, &this->cmaj_flt,
	   &this->utime, &this->stime, &this->cutime, &this->cstime,
	   &this->counter, &this->priority, &this->timeout,
	   &this->it_real_value, &this->start_time,
	   &this->vsize, &this->rss, &this->rss_rlim,
	   &this->start_code, &this->end_code, &this->start_stack,
	   &this->kstk_esp, &this->kstk_eip,
	   &this->signal, &this->blocked, &this->sigignore, &this->sigcatch,
	   &this->wchan);
    if ((ctty && (ctty != this->tty))
	|| (r && this->state != 'R' && this->state != 'D')
	|| (!x && (this->tty == -1))) {
      this->pid = 0;
      continue;
    }
    /* 0 normally passed, which is never the value given as the tty from the
       proc filesystem, so this only happens if a specific tty was passed. */
    if(m) {
      if(!mycpy(ent->d_name, stat_str, "statm", sizeof(stat_str), 0)) continue;
      sscanf(stat_str, "%d %d %d %d %d %d %d",
	     &this->statm.size, &this->statm.resident,
	     &this->statm.share, &this->statm.trs,
	     &this->statm.lrs, &this->statm.drs,
	     &this->statm.dt);
    }
    if (this->state == 'Z') strcat(this->cmd," <zombie>");
    dev_to_tty(this->ttyc, this->tty);
    if(u) strncpy(this->user, user_from_uid(this->uid), 9);

    /* update the linked list and increase the count */
    if(this->pid) {
      that = this;
      if(!this->next)
	this->next = (struct ps_proc *) xcalloc(this->next,
						sizeof(struct ps_proc));
      this = this->next;
      this->pid = 0;
      ph->count++;
    }
  } /* end of the while loop */
  closedir(proc);
  if(!this->pid) { /* if the last slot was not used */
    if(that->next)
      free_psproc(that->next);
    that->next = (struct ps_proc *) NULL;
  } else {
    if (this->next)
      free_psproc(this->next);
    this->next = (struct ps_proc *) NULL;
  }
  return ph;
}




void free_psproc(struct ps_proc * this) {

  struct ps_proc *that;

  for(; this != NULL; this = that) {
    that = this->next;
    free(this);
  }
}







/* The next few functions are modified versions of functions from
   various files in the kmem ps.  They are not as complete at error
   checking, but that's life.  Thanks, Branko.  I had to change them
   not to look at /dev/kmem, and to make my life simpler for a while,
   error checking came out.  It's a stupid move, and I'll regret it,
   but quite a bit of it depends on kmem reading, so I just chopped
   it out...
*/

struct tbl_s vars, fncs;
struct psdb_hdr db_hdr;
int psdb = -1;


void * xmalloc(unsigned int size)
{
    char *p;

    if (size == 0) ++size;
    if ((p = (void *) malloc(size)) == NULL) {
	perror("xmalloc");
	exit(1);
    }
    return(p);
}



int open_psdb(void)
{

    if ((psdb = open(PSDATABASE, O_RDONLY)) == -1)
	return -1;
    if (read(psdb, (char *) &db_hdr, sizeof(db_hdr)) != sizeof(db_hdr))
	return -1;
    
    if (strncmp(db_hdr.magic, PS_MAGIC, sizeof(db_hdr.magic))) {
	fprintf(stderr, "invalid psdatabase\n");
	return -1;
    }
    
    return(0);
}



void close_psdb(void)
{
    if (psdb != -1)
	close(psdb);
    psdb = -1;
}



int read_tbl(struct dbtbl_s *dbtbl, struct tbl_s *tbl)
{
  lseek(psdb, dbtbl->off, SEEK_SET);
  tbl->tbl = (struct sym_s *) xmalloc(dbtbl->size);
  if (read(psdb, (char *) tbl->tbl, dbtbl->size) != dbtbl->size) {
    perror(PSDATABASE);
    exit(1);
  }
  tbl->nsym = dbtbl->nsym;
  tbl->strings = (char *) (tbl->tbl + tbl->nsym);
  return(0);
}


/*
 * misc stuff needed
 */

char * find_func(unsigned long address)
{
  int n;
  struct sym_s *p;
  char *s;

  if (fncs.tbl == NULL)
    read_tbl(&db_hdr.fncs, &fncs);

  p = fncs.tbl;
  n = fncs.nsym;
  while (n) {
    int i = n / 2;
    if (p[i].addr < address) {
      p = &p[i+1];
      if (p->addr > address) {
	--p;
	break;
      }
      --n;
    }
    n /= 2;
  }
  s = p->name + fncs.strings;
  return(*s == '_' ? s+1 : s);
}




char * wchan(unsigned int address)
{
  static char zero = 0;
  char *p;

  if (address) {
    p = find_func(address);
    
    if (strncmp(p, "sys_", 4) == 0)
      p += 4;
    while (*p == '_' && *p)
      ++p;
  } else { /* 0 address means not in kernel space */
    p = &zero;
  }
  return(p);
}
