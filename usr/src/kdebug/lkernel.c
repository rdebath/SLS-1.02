/* This should let one do limited linux kernel debugging on a single system.
   The linux kernel supports the GDB remote protocol for debugging with
   more than one system.

Copyright 1993 Ross Biro 

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  The author can be reached as bir7@leland.stanford.edu or
  c/o Department Of Mathematics; Stanford, CA 94305
*/

/* Here's the idea.  The local kernel debugger supports break points which
   let you save state.  So we will set break points and then grab the
   state when one occurs, letting the user play with it.  You will not
   be allowed to change variables, but if you are really want to, you
   can always use gdb to get the address, and then right to /dev/kmem.
   This is designed to be used in conjunction with the local kernel
   debugger which has real break points and let's you edit the memory,
   but does not have a symbol table. */

/* some of this file was modeled on remote.c which is Copyright FSF. */

#include "defs.h"
#include <string.h>
#include <fcntl.h>
#include "frame.h"
#include "inferior.h"
#include "target.h"
#include "wait.h"
#include "terminal.h"
#include "gdbcmd.h"
#include <sys/types.h>
#include <sys/ptrace.h>
#include <signal.h>
#include <errno.h>

static int lk_break_happened;
static int lk_file = -1;

/* deals with signals that happen when the kernel
   hits a break point. */

void lk_brkpt(int sig)
{
  signal (sig, lk_brkpt);
  lk_break_happened++;
}

extern struct target_ops lk_ops; 

static void
lk_close (int quitting)
{
  close (lk_file);
  lk_file = -1;
}

/* opens the file /dev/cmem */
static void
lk_open (char *string , int from_tty)
{
  target_preopen (from_tty);
  lk_close (0); /* just in case. */
  lk_file = open ("/dev/cmem", O_RDONLY);
  if (lk_file < 0)
    error ("Unable to open /dev/cmem: %s", strerror (errno));

  if (from_tty)
    printf ("Linux kernel debugging active\n");
  push_target (&lk_ops);
  lk_break_happened = 1;
  start_remote(); /* this should get things going. */
}

/* returns pid which will always be 0 */
static int
lk_wait (WAITTYPE *status)
{
  WSETEXIT ((*status), 0);
  WSETSTOP ((*status), 1);
  while (!lk_break_happened) 
    {
      signal (SIGIO, lk_brkpt);
      /* wait for something to happen. */
      pause();
    }
  lk_break_happened = 0;
  return (0);
}

/* Read the registers int the block REGS. */
/* it's easiest just to grab them all so
   we will ignore regnum. */
static void
lk_fetch_reg (int regno)
{
/* copied from i386-xdep.c */
static int regmap[] = 
{
  EAX, ECX, EDX, EBX,
  UESP, EBP, ESI, EDI,
  EIP, EFL, CS, SS,
  DS, ES, FS, GS,
};

  int i;
  struct user udata;
  if (lseek (lk_file, 0, SEEK_SET) < 0)
      error ("lseek on /dev/cmem failed: %s", strerror (errno));

  /* read in the entire user data area. */
  /* should loop here. */
  errno = 0;
  if (read (lk_file, &udata, sizeof (udata)) != sizeof (udata))
    {
      error ("read on /dev/cmem failed: %s", strerror (errno));
    }
  /* now we need to move them into the correct place, regmap
     should help out here. */
  for (i = 0; i <NUM_REGS; i++)
    {
      supply_register (i, (char *)((unsigned long *)&(udata.regs)
				    + regmap[i]));
    }
}

/* returns number of bytes transfered. */
static int
lk_xfer_memory (CORE_ADDR memaddr,char * myaddr,int len,
		int should_write,struct target_ops *target)
{
  int rd=0;
  int tmp;
  /* we don't write. */
  if (should_write) return (0);
  /* lseek returns an error if you seek to something less than
     0 so we will just zero out the memory in that case. */

  if ((off_t)memaddr < 0)
    {
      memset (myaddr, 0, len);
      return (len);
    }

  if (lseek (lk_file, memaddr + 4096, SEEK_SET) < 0)
    {
      error ("lseek /dev/cmem: %s",strerror (errno));
    }

  while (len > 0)
    {
      tmp = read (lk_file, myaddr, len);
      if (tmp < 0)
	{
	  if (rd) return (rd);
	  error ("read /dev/cmem: %s", strerror (errno));
	}
      len -= tmp;
      rd += tmp;
      myaddr += tmp;
    }
  return (rd);
}

static void
lk_files_info (struct target_ops *tops)
{
  printf ("Debugging the Linux Kernel.\n");
}

static int brkpts_inuse = 0; /* keep track of which of the 4 break points
				we are delaing with. */

static int
lk_insert_break( CORE_ADDR addr, char *save)
{
  int i;
  struct kbrkpt brkpt;
  for (i = 0 ; i < 4; i++)
    if (! (brkpts_inuse & (1 << i)))
      break;
  if (i == 4) return (ENOMEM); /* any better ideas? */

  brkpt.num = i;
  brkpt.addr = (unsigned long)addr;
  brkpt.active = 1;
  brkpt.cond = 0;
  brkpt.stop = 0;
  brkpt.skip = 0;
  brkpt.save = 1;
  brkpt.os = 0; /* one shot. */
  brkpt.exec = NULL;
  brkpt.pid = getpid();
  brkpt.caddr = 0;
  brkpt.cmask = 0;
  brkpt.cres = 0;
  brkpt.creg = -1;
  brkpt.len = 0; /* all code break points have length 1 */
  brkpt.type = 0; /* code break points only. */
  /* now we have to set it. */
  if (ptrace (PTRACE_ADD_BREAK, i, &brkpt, 0) < 0)
    return (errno);
  *save = i; /* save the break point number. */
  brkpts_inuse |= 1 << i;
  return(0);
}
static int
lk_remove_break (CORE_ADDR addr, char *save)
{
  int num;
  num = *save;
  if (ptrace (PTRACE_DEL_BREAK, 0, num, 0) < 0)
    return (errno);
  brkpts_inuse &= ~(1 << num);
  return (0);
}
static void
lk_run (int step, int sig )
{
  /* all we do is set up the signal handler. */
  signal (SIGIO, lk_brkpt);
  /* this will cause us to wait until the
     next break point actually occurs. */
  lk_break_happened = 0;

}

static
struct target_ops lk_ops =
{
  "kernel",
  "Linux Local Kernel Debugger",
  "Lets you set break points in the kernel which save state.\n"
  "Use file tools/system then set a break point and run.  When "
  "it returns you will be able to examine the local variables at "
  "the time the breakpoint occured.",
  lk_open, /* open /dev/cmem */
  lk_close, /* close /dev/cmem */
  NULL, /* attach takes nothing special. */
  NULL, /* detach */
  lk_run, /* it's already running, can't do much about it. */
  lk_wait,   /* again can't really do anything. */
  lk_fetch_reg, /* get the registers from /dev/cmem */
  NULL,  /* can't change the registers. */
  NULL,  /* ditto */
  NULL,  /* no need to convert to or from virtual address. */
  NULL,  /* ditto */
  lk_xfer_memory, /* read some memory from /dev/cmem */
  lk_files_info, /* gives some info about what we are debuggin. */
  lk_insert_break, /* insert a break point. */
  lk_remove_break, /* delete a break point. */
  NULL,	/* terminal_init */
  NULL, /* terminal_inferior */
  NULL, /* terminal_ours_for_output */
  NULL, /* terminal_ours */
  NULL, /* terminal_info */
  NULL, /* to kill (CAD works.) */
  NULL, /* to load */
  NULL, /* to lookup symbol */
  NULL, /* to create inferior */
  NULL, /* to mourn inferior */
  process_stratum, /* the stratum */
  NULL, /* next */
  1,    /* has all memory */
  1,    /* has memory */
  1,    /* has stack */
  1,    /* has registers */
  1,    /* has execution */
  NULL, /* sections */
  NULL, /* sections_end */
  OPS_MAGIC /* magic */
};


void
_initialize_linux_kernel()
{
  add_target (&lk_ops);
#if 0
  add_show_from_set (
     add_set_cmd ("kerneldebug", no_class, var_boolean, (char *)&lkdebug,
		  "Set debugging of linux kernel debugger.\n", &setlist),
&showlist);
#endif

}
