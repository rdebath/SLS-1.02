/* kdebug.c */
/* This implements a simple kernel debugger. */
/*
    Copyright (C) 1992  Ross Biro

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 

    The Author may be reached as bir7@leland.stanford.edu or
    C/O Department of Mathematics; Stanford University; Stanford, CA 94305
*/
#include <linux/config.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/tty.h>
#include <linux/ptrace.h>
#include <asm/io.h>
#include <asm/system.h>
#include <asm/segment.h>
#include <linux/string.h>


extern int kill_proc (int, int, int);

#define MAX_FIELDS 16
/* sets the trap flag for single_stepping. */
#define TRAP_FLAG 0x100
#define RF_FLAG 0x10000

int i386dis(int pc, unsigned char *inbuf, char *outbuf);

struct saved_state states[4];

struct saved_state *debug_memory=states;

struct kbrkpt brks[4];

static struct {
  char *name;
  int offset;
  int tssoff;
} regs[]=
{
#define SPNO 0
  {"esp",0},
#define DR0NO 1
  {"dr0",0,0},
  {"dr1",0,0},
  {"dr2",0,0},
  {"dr3",0,0},
  {"dr4",0,0},
  {"dr5",0,0},
  {"dr6",0,0},
  {"dr7",0,0},
#define DR7NO 8
#define DEBUGREG(x) (((x)>=DR0NO) && ((x)<= DR7NO))
#define DEBUGREGNO(x) ((x)-DR0NO)
  {"eax",EAX,10},
  {"ebx",EBX,13},
  {"ecx",ECX,11},
  {"edx",EDX,12},
  {"esi",ESI,16},
  {"edi",EDI,17},
  {"ebp",EBP,15},
  {"ds",DS,21},
  {"es",ES,18},
  {"fs",FS,22},
  {"gs",GS,23},
  {"eip",EIP,8},
  {"cs",CS,19},
  {"flags",EFL,9},
  {NULL,0,0}
};


static void print_break( struct kbrkpt *);
static void debug_show_task (struct task_struct *);
static void print_ptr (void *);
static unsigned char patch_buff[1024]={0,}; /* some unused memory for
					       patches.  */
static void do_dcmd(char *);
static void (*do_input)(char *) = do_dcmd;

static struct {
  char *type; /* "(struct task_struct *)" for example */
  char *name; /* current for example */
  void *addr; /* current for example */
  int len;    /* the sizeof the type. */
  void (*print)(); /* debug_show_task */
  void (*input)(); /* a function to let you edit the type. */
} globals[] =
{
  {"(struct task_struct **)","task", task, sizeof (task[0]),
     print_ptr, NULL},
  {"(struct kbrkpt *)", "brks[0]", brks, sizeof(brks[0]),
     print_break, NULL},
  {"(struct kbrkpt *)", "brks[1]", brks+1, sizeof(brks[0]),print_break,
     NULL},
  {"(struct kbrkpt *)", "brks[2]", brks+2, sizeof(brks[0]),print_break,
     NULL},
  {"(struct kbrkpt *)", "brks[3]", brks+3, sizeof(brks[0]), print_break,
     NULL},
  {"(struct task_struct **)", "current", &current, sizeof (current),
     print_ptr, NULL},
  {"(void *)", "patch_buff",patch_buff, sizeof (void *), print_ptr, NULL},
  {"(struct task_struct *)", "(none)", patch_buff, sizeof (struct task_struct),
     debug_show_task, NULL},
  {NULL, NULL, NULL, 0, NULL, NULL}
};


#define WHITESPACE " \t\n\r"
#define isspace(x) (strchr(WHITESPACE,(x)) != NULL)
#define HELP "-HELP"
#define USAGE "-USAGE"
#define is_printable(x) (((x) != '\r') && ((x)!='\n') && ((x) != 0x7f) && \
((x) != 0) )

static struct pt_regs *last_regs=0;
static char last_break=-1;

volatile int k_debug=0;

extern hard_reset_now(void);
extern void show_state(void);
extern show_task (int, struct task_struct *);
extern void show_mem(void);
static int atox(char *, unsigned long *);
extern volatile unsigned char kstatus;

static  unsigned char
get_byte(unsigned long addr)
{
  return (*(unsigned char *)addr);
}

static  unsigned long
get_long (unsigned long addr)
{
  return (*(unsigned long *) addr);
}

static  void
put_byte (unsigned long addr, unsigned char data)
{
  *(unsigned char *)addr = data;
}

static  void
put_long (unsigned long addr, unsigned long data)
{
  *(unsigned long *)addr = data;
}

void
del_break (struct kbrkpt *brkpt)
{
  if (brkpt->active) return;
  set_dr7 (dr7() & ~(3 << 2*brkpt->num)); 
}

static  void
sto_lower(char *s)
{
  int i;
  for (i=0; s[i] != 0; i++)
    {
      if (s[i] >= 'A' && s[i] <= 'Z') s[i]|=0x20;
    }
}

/* this routine is called to save some state for later. */
static void
save_state (struct saved_state *state, struct pt_regs *regs)
{
  int i;
  /* first the kernel stack. */
  memcpy ((void *)state->stack_copy, (char *)((unsigned )regs & ~(4096-1)),
	  4096);

  /* the regs were on the stack, so they have been copied. */
  state->regs = (void *)(state->stack_copy + ((unsigned )regs & (4096 -1)));
  
  /* fix up the stack info. */
  state->regs->esp = (unsigned long)&(state->regs->esp);
  state->regs->ss = state->regs->cs;

  /* now fill in the saved state. */
  for (i = 0; i < 500; i++)
    {
      if ((unsigned long)state->saved_mem[i].addr < 4096) continue;
      state->saved_mem[i].data = *state->saved_mem[i].addr;
    }

}

/* debug show task.  Interface to show task. */
static void
debug_show_task(struct task_struct *t)
{
  int i;
  for ( i = 0; i <NR_TASKS; i++)
    {
      if (task[i] == t)
	{
	  show_task (i, t);
	  return;
	}
    }
  printk ("%X is not a task struct. \n", t);
}

/* this tells how far until the next saved address. */
unsigned
next_saved (void * addr)
{
  unsigned min;
  int i;
  min = -1;

  if ((unsigned long)addr < 4096) return (4096-(unsigned long)addr);

  for (i = 0; i < 500; i++)
    {
      if ((unsigned long)debug_memory->saved_mem[i].addr >=
	  (unsigned long)addr)
	{
	  if (((unsigned long)debug_memory->saved_mem[i].addr - 
	       (unsigned long) addr ) < min)
	    min = (unsigned long)debug_memory->saved_mem[i].addr - 
	      (unsigned long)addr;
	}
    }
  return (min);
}

/* copy some saved data to user memory.  Returns amount copied. */
unsigned
read_saved (unsigned char *buf, unsigned char *addr, int count)
{
  int i, amt, offset;
  if (count == 0) return 0;
  for (i = 0; i < 500; i++)
    {
      if ((unsigned long)debug_memory->saved_mem[i].addr < 4096) continue;

      if ((unsigned long)debug_memory->saved_mem[i].addr
	  <= (unsigned long)addr &&
	  (unsigned long)debug_memory->saved_mem[i].addr+4
	  > (unsigned long) addr)
	{
	  amt = (unsigned long)debug_memory->saved_mem[i].addr
	    + 4 - (unsigned long)addr;
	  offset = 4-amt;
	  memcpy_tofs (buf, (unsigned char *)(debug_memory->saved_mem[i].data)
		       + offset, amt);
	  return (amt);
	}
    }
  return  (0);
}

static  void
set_dr0(unsigned long value)
{
  __asm__("movl %0,%%db0"::"r" (value));
}

static  void
set_dr1(unsigned long value)
{
  __asm__("movl %0,%%db1"::"r" (value));
}

static  void
set_dr2(unsigned long value)
{
  __asm__("movl %0,%%db2"::"r" (value));
}

static  void
set_dr3(unsigned long value)
{
  __asm__("movl %0,%%db3"::"r" (value));
}

static  void
set_dr4(unsigned long value)
{
/*  __asm__("movl %0,%%db4"::"r" (value));*/
}

static  void
set_dr5(unsigned long value)
{
/*  __asm__("movl %0,%%db5"::"r" (value));*/
}

static  void
set_dr6(unsigned long value)
{
  __asm__("movl %0,%%db6"::"r" (value));
}

void
set_dr7(unsigned long value)
{
  __asm__("movl %0,%%db7"::"r" (value));
}

static  unsigned long
dr0(void)
{
  unsigned long res;
  __asm__("movl %%db0,%%eax":"=a" (res));
  return (res);
}

static  unsigned long
dr1(void)
{
  unsigned long res;
  __asm__("movl %%db1,%%eax":"=a" (res));
  return (res);
}

static  unsigned long
dr2(void)
{
  unsigned long res;
  __asm__("movl %%db2,%%eax":"=a" (res));
  return (res);
}

static  unsigned long
dr3(void)
{
  unsigned long res;
  __asm__("movl %%db3,%%eax":"=a" (res));
  return (res);
}

static  unsigned long
dr4(void)
{
  unsigned long res;
/*  __asm__("movl %%db4,%%eax":"=a" (res));*/
  res=0;
  return (res);
}

static  unsigned long
dr5(void)
{
  unsigned long res;
/*  __asm__("movl %%db5,%%eax":"=a" (res));*/
  res=0;
  return (res);
}

static  unsigned long
dr6(void)
{
  unsigned long res;
  __asm__("movl %%db6,%%eax":"=a" (res));
  return (res);
}

unsigned long
dr7(void)
{
  unsigned long res;
  __asm__("movl %%db7,%%eax":"=a" (res));
  return (res);
}


/* called to see if we should skip this breakpoint 
   returns 1 if we hit a break point
   returns 0 if we hit one we should skip
   returns -1 if we don't know why we stopped. */

int
do_break(struct pt_regs *regs)
{
  int num, r6;
  unsigned long addr;
  unsigned long test;
  int ret;
  int tmp;

  ret = -1;
  r6 = dr6();

  /* set the rf flag. */
  regs->eflags |= RF_FLAG;

  /* first see if any of the breakpoints have been trigger. */
  last_break = -1;

  for (num = 0; num < 4; num ++)
    {
      if (!brks[num].active) continue;
      if (!(r6 & (1 << num))) continue;

      /* we have found one. */
      /* now we need to check to see if it's conditional. */
      tmp = 0;
      if (brks[num].exec)
	{
	  /* returning a 0 means normal.
	     1 means stop, and -1 means skip it. */
	  tmp = brks[num].exec(regs);

	  if (tmp == -1)
	    {
	      ret = 0;
	      continue;
	    }
	}

      if ((brks[num].cond) && tmp==0)
	{
      
	  /* so it's conditional.  See if it's condition has been satisfied. */
	  addr = 0;
	  if (brks[num].creg >= 0)
	    {
	      addr = ((unsigned long *)regs)[brks[num].creg];
	    }
	  addr += brks[num].caddr;
	  test = *(unsigned long *)addr;
	  if (test & brks[num].cmask != brks[num].cres)
	    {

	      ret = 0; /* we need to return 0 which means ignore this
			  breakpoint.  However there may be another
			  break point which has also been set. */
	      continue;
	    }
	}
      /* see if it's a one-shot thing. */
      if (brks[num].os) brks[num].active = 0;
	
      /* now we know we want to deal with this breakpoint. */
      if (brks[num].pid)
	{
	  kill_proc (brks[num].pid, SIGIO, 1);
	}

      if (brks[num].save)
	{
	  save_state (states+num, regs);
	  /* debug_memory always points to the most recent break. */
	  debug_memory = states+num;
	}
      /* add finally see if we should stop. */
      if (brks[num].stop || tmp == 1)
	{
	  last_break = num;
	  return (1);
	}
      ret = 0;
      
    }
  return (ret);
}

void
del_breaks (void)
{
  set_dr6(0); /* clear the occured flags. */
  set_dr7(0); /* clear the break points. */
}

static  int
set_reg(int regno, unsigned long value)
{
  if (DEBUGREG(regno))
    {
      switch (DEBUGREGNO(regno))
	{
	case 0:
	  set_dr0(value);
	  break;
	case 1:
	  set_dr1(value);
	  break;
	case 2:
	  set_dr2(value);
	  break;
	case 3:
	  set_dr3(value);
	  break;
	case 4:
	  set_dr4(value);
	  break;
	case 5:
	  set_dr5(value);
	  break;
	case 6:
	  set_dr6(value);
	  break;
	case 7:
	  set_dr7(value);
	  break;
	}
      return (0);
    }
  else
    {
      /* take care of eax0 etc. */
      if ((regno & 0xffffff00) != 0)
	{
	  int tmp;
	  tmp = regs[regno&0xff].tssoff;
	  if (tmp == 0) return (-1);
  	  ((long *)(&task[(regno>>8) -1]->tss))[tmp]=value;
	  return (0);
	}
      if (regno == SPNO)
	return (-1);
      ((unsigned long *)last_regs)[regs[regno].offset]=value;
      return (0);
    }
}

/* this routine takes a brk point structure and puts it into
   the debug registers. */
void
set_break(struct kbrkpt *brkpt)
{
  if (! brkpt->active) return;
  /* set the address. */
  set_reg (DR0NO+brkpt->num, brkpt->addr);
  set_dr7 (dr7()|(0x2 << (brkpt->num*2)) |
	  (((brkpt->len<<2) | brkpt->type) << (16+4*brkpt->num)));

}

void
set_breaks (void)
{
  int i;
  for (i=0; i < 4; i++)
    {
      set_break( brks+i);
    }
}

static last_disassem=0;
static void
disassem(long addr)
{
  /*these don't need to go on the stack. */
  static char obuff[80];
  static unsigned char ibuff[20];
  int i;
  int len;
  /* read in 20 bytes so we know we have more than enough. */
  for (i=0; i < 20; i++)
    {
      ibuff[i]=get_byte(addr+i);
    }
  len =i386dis(addr, ibuff, obuff);
  last_disassem = addr+len;
  printk ("%8.8X ",addr);
  for ( i = 0; i < 10; i++)
    {
      if (i < len)
	printk("%2.2X ",ibuff[i]);
      else
	printk("   ");
    }
  printk ("   %s\n", obuff);
}


static  void
printregs(void)
{
  printk("\n");
  printk("error code = %d\n",last_regs->orig_eax);
  printk("CS:EIP - %4.4X:%8.8X ",last_regs->cs&0xffff,last_regs->eip);
  printk("EFLAGS - %8.8X\n",last_regs->eflags);
  printk("EAX - %8.8X EBX - %8.8X ECX - %8.8X EDX - %8.8X\n",
	 last_regs->eax, last_regs->ebx, last_regs->ecx, last_regs->edx);
  printk("ESI - %8.8X EDI - %8.8X EBP - %8.8X ESP - %8.8X\n",
	 last_regs->esi, last_regs->edi, last_regs->ebp, &(last_regs->esp));
  printk("ds - %4.4X es - %4.4X fs - %4.4X gs - %4.4X\n",
	 last_regs->ds & 0xffff, last_regs->es & 0xffff, 
	 last_regs->fs & 0xffff, last_regs->gs & 0xfff);
  printk("DR0 - %8.8X DR1 - %8.8X DR2 - %8.8X DR3 -%8.8X\n",
	 dr0(),dr1(),dr2(),dr3());
  printk("DR4 - %8.8X DR5 - %8.8X DR6 - %8.8X DR7 -%8.8X\n",
	 dr4(),dr5(),dr6(),dr7());
  printk("\n");
  disassem (last_regs->eip);

}

static  void
prompt(void)
{
  printk("\nk-debug> ");
}


void start_debug(int key, struct pt_regs *pt)
{
  if (key & 0x80) return;
  if (k_debug)
    {
      kstatus ^= KEY_DBG;
      return;
    }
  kstatus |= KEY_DBG;
  k_debug++;
  last_regs = pt;
  prompt();
}

void enter_local_debug(struct pt_regs *regs)
{
  int ret;

  ret = do_break(regs);
  /* see if we should skip the breakpoint. */
  if (ret == 0)
    {
      del_breaks();
      set_breaks();
      return;
    }
  last_regs=regs;
  kstatus |= KEY_DBG;
  k_debug=2;
  printregs();
  prompt();
  /* we can't let this task continue, but we can't safely switch out
     of it either.  So we just sit here in a busy wait loop. */
  sti(); /* make sure interrupts are enabled. */
  while (k_debug > 0)
    __asm__ (".byte 0xf4"); /* halt */
}


#ifndef CONFIG_KDEBUG
enter_kdebug(struct pt_regs *regs)
{
  enter_local_debug (regs);
}
#endif

static  int
get_regno(char *reg)
{
  int i;
  for (i=0; regs[i].name != NULL; i++)
    {
      if (strncmp(regs[i].name,reg,strlen(regs[i].name)) == 0)
	{
	  if (strlen(regs[i].name) != strlen(reg))
	    {
	      unsigned long val;
	      if (atox(reg+strlen(regs[i].name),&val))
		{
		  return (-1);
		}
	      return (i|(val+1)<< 8);
	    }
	  return (i);
	}
    }
  return (-1);
}

static  int
get_reg(int regno, unsigned long *value)
{
  if (DEBUGREG(regno))
    {
      switch (DEBUGREGNO(regno))
	{
	case 0:
	  *value = dr0();
	  break;
	case 1:
	  *value = dr1();
	  break;
	case 2:
	  *value = dr2();
	  break;
	case 3:
	  *value = dr3();
	  break;
	case 4:
	  *value = dr4();
	  break;
	case 5:
	  *value = dr5();
	  break;
	case 6:
	  *value = dr6();
	  break;
	case 7:
	  *value = dr7();
	  break;
	}
      return (0);
    }
  else
    {
      /* take care of eax0 etc. */
      if ((regno & 0xffffff00) != 0)
	{
	  int tmp;
	  tmp = regs[regno& 0xff].tssoff;
	  if (tmp == 0) return (-1);
	  *value=((long *)(&task[(regno>>8) -1]->tss))[tmp];
	  return (0);
	}
      if (regno == SPNO)
	{
	  *value =(long)(&(last_regs->esp));
	  return (0);
	}
      *value = ((unsigned long *)last_regs)[regs[regno].offset];
      return (0);
    }
}

static  int
atoi(char *str, unsigned long *res)
{
  *res=0;
  while (*str != 0)
    {
      *res *= 10;
      if ((*str >= '0') && (*str <= '9'))
	*res += *str - '0';
      else
	return (-1);
      str++;
    }
  return (0);

}

static int
atox(char *str, unsigned long *res)
{
  int reg_no;

  /* see if we need to use a reg or variable. */
  if (*str == '%')
    {
      reg_no=get_regno(str+1);
      if (reg_no < 0)
	{
	  int i;
	  for (i = 0; globals[i].type != NULL; i++)
	    {
	      if (strcmp (str+1,globals[i].name) == 0)
		{
		  *res = (unsigned )globals[i].addr;
		  return (0);
		}
	    }
	  return (-1);
	}
      return (get_reg(reg_no,res));
    }

  /* see if it's indirect. */
  if (*str == '@')
    {
      if (atox(str+1,res) < 0)
	return (-1);
      *res = get_long(*res);
      return(0);
    }

  /* see if it's a decimal number */
  if (*str == '#' )
    {
      return (atoi(str+1,res));
    }

  *res=0;
  while (*str != 0)
    {
      *res= *res<<4;
      if ((*str >= '0') && (*str <= '9'))
	*res += *str - '0';
      else
	if ((*str >= 'a') && (*str <= 'f'))
	  *res += *str -'a' +10;
      else
	return (-1);
      str++;
    }
  return (0);

}

static void
print_ptr (void *ptr)
{
  printk ("%8.8X\n", ptr);
}

static  void
print_reg(int regno)
{
  unsigned long value;
  if (get_reg(regno,&value))
    {
      printk ("Bad Register\n");
      return;
    }
  if ((regno & 0xffffff00) != 0)
    {
      printk ("%s%x - %8.8X\n",regs[regno&0xff].name, (regno>>8)-1,value);
    }
  else
    {
      printk ("%s - %8.8X\n",regs[regno&0xff].name,value);
    }
}

typedef void (*func_ptr)(int, char **);

struct dcmd
{
  char *name;
  func_ptr func;
};

static void
dbg_run(int argc, char *argv[])
{
  if (argc > 1)
    {
      if (strcmp (argv[1],HELP) == 0)
	{
	  printk("Run -- exits the kernel debugger,"
		 "and resumes normal execution\n");
	}
      else
	{
	  printk("run\n");
	}
      return;
    }
  printk ("Running\n");
  k_debug=0;

  last_regs->eflags &= ~TRAP_FLAG;
  /* reset all the break points. */
  del_breaks();
  set_breaks();
  kstatus &= ~KEY_DBG;
}


static void
dbg_step (int argc, char *argv[])
{
  if (argc == 2 && strcmp(argv[1],HELP) == 0)
    {
      printk ("Step -- executes a single instruction.\n");
    }
  if (argc != 1)
    {
      printk ("usage: step\n");
      return;
    }
  last_regs->eflags |= TRAP_FLAG;
  k_debug = 0;
  kstatus &= ~KEY_DBG;
  del_breaks();
  set_breaks();
}

static  void
dump_line (long address)
{
  unsigned long addr;
  int i;
  unsigned char byte;

  addr=address&~0xf;
  printk ("%8.8X: ",addr);
  for (i=0; i < 16; i++)
    {
      if (addr+i >= address)
	{
	  printk ("%2.2X",get_byte(addr+i));
	  if (i==7)
	    printk ("-");
	  else
	    printk (" ");
	}
      else
	{
	  printk ("   ");
	}
    }
  printk (" ");
  for (i=0; i < 16; i++)
    {
      if (addr+i >= address)
	{
	  byte=get_byte(addr+i);
	  if (is_printable(byte))
	    printk ("%c",get_byte(addr+i));
	  else
	    printk (".");
	}
    }
  printk ("\n");
}

static void
dbg_dump(int argc, char *argv[])
{
  static long last_dump_addr=0;
  unsigned long value;
  long len;

  if (argc == 1)
    {
      dump_line(last_dump_addr);
      last_dump_addr+=16;
      return;
    }
  if (strcmp(argv[1],HELP) == 0)
    {
      printk("dump -- display bytes of memory.\n");
      return;
    }
  if ( argc > 3 || atox(argv[1],&value) )
    {
      printk("dump [address [len]]\n");
      return;
    }
  if (argc == 3)
    {
      if (atox(argv[2], (unsigned long *)&len))
	{
	  printk ("bad Value\n");
	  return;
	}
    }
  else
    {
      len = ((value+16)&~0xf) - value;
    }

  while (len > 0)
    {
      
      dump_line(value);
      len-=16-(value&0xf);
      value+=16-(value&0xf);
    }
  last_dump_addr = value;
}

static void
dbg_rboot(int argc, char *argv[])
{
  if (argc > 1)
    {
      if (strcmp (argv[1],HELP) == 0)
	{
	  printk("Reboot -- Resets the computer (Hopefully causing"
		"a reboot.)\n");
	}
      else
	{
	  printk("reboot\n");
	}
      return;
    }
  printk ("Rebooting\n");
  hard_reset_now();
}

/* display the registers, a single register, or change the value of
   a single register. */

static void
dbg_regs(int argc, char *argv[])
{
  int regno;
  unsigned long value;
  if (argc == 1)
    {
      printregs();
      return;
    }
  if (strcmp(argv[1],HELP) == 0)
    {
      printk("regs -- display the registers, a signle register, or changed\n"
	     "        the value of a single register.\n");
	return;
    }
  if ((regno=get_regno(argv[1])) < 0 || argc > 3)
    {
      printk("regs [reg [value]]\n");
      return;
    }

  if (argc == 2)
    {
      print_reg(regno);
      return;
    }

  if (atox(argv[2],&value))
    {
      printk ("Bad Hex number %s\n",argv[2]);
      return;
    }
  set_reg(regno, value);
  print_reg(regno);
}

static void
dbg_mem (int argc, char *argv[])
{
  if (argc > 1)
    {
      if (strcmp (argv[1],HELP)==0)
	{
	  printk("mem -- print memory status\n");
	  return;
	}
      printk("mem\n");
      return;
    }
  show_mem();
}


static void
dbg_task (int argc, char *argv[])
{
  unsigned long value;
  if (argc > 1)
    {
      if (strcmp (argv[1],HELP)==0)
	{
	  printk("task -- the task struct for one or all tasks.\n");
	  return;
	}
      if (atox(argv[1],&value))
	{
	  printk("task [n]\n");
	  return;
	}

      if (task[value] != NULL)
	show_task(value, task[value]);
      else
	printk("task[%X]=NULL\n",value);

      return;
    }
  printk ("\n");
  show_state();
}

static void
dbg_stack (int argc, char *argv[])
{
  unsigned long len;
  long *addr;
  if (argc > 1)
    {
      if (strcmp (argv[1],HELP)==0)
	{
	  printk("stack -- display the contents of the stack.\n");
	  return;
	}

      if (atox(argv[1],&len))
	{
	  printk("stack [len]\n");
	  return;
	}
    }
  else
    {
      len = 10;
    }
  addr = (long *)(last_regs);
  while (len > 0)
    {
      printk ("%8.8X\n",addr[len]);
      len --;
    }
}

static void
dbg_nocmd(int argc, char *argv[])
{
  printk("Unrecognized Command - %s\n", argv[0]);
}


static void
dbg_current(int argc, char *argv[])
{
  int i;
  if (argc > 1)
    {
      if (strcmp (argv[1],HELP) == 0)
	{
	  printk("Current -- prints the task struct assosiated\n"
		 "           to the current task\n");
	}
      else
	{
	  printk("current\n");
	}
      return;
    }
  for (i=0; i <NR_TASKS; i++)
    {
      if (task[i]==current)
	{
	  show_task(i,current);
	  return;
	}
    }
  printk ("Unable to locate current task????\n");
}

static void
dbg_list(int argc, char *argv[])
{
  unsigned long value;
  unsigned long len;

  if (argc == 1)
    {
      len=20;
    }
  else
    {
      if (strcmp(argv[1],HELP) == 0)
	{
	  printk("list -- disassemble instructions.\n");
	  return;
	}
      if ( argc > 3 || atox(argv[1],&value) )
	{
	  printk("list [address [len]]\n");
	  return;
	}
      if (argc == 3)
	{
	  if (atox(argv[2], &len))
	    {
	      printk ("bad Value\n");
	      return;
	    }
	}
      else
	{
	  len = 20;
	}
      last_disassem=value;
    }

  while (len > 0)
    {
      disassem(last_disassem);
      len --;
    }
}

void
print_break( struct kbrkpt *brk)
{
  char flags[16];
  int i;
  char *regname;

  i = 0;
  if (!brk->active) return;

  switch (brk->type)
    {
    case 0:
      flags[i++] = 'e';
      break;

    case 1:
      flags[i++] = 'w';
      break;

    case 3:
      flags[i++] = 'r';
      break;

    default:
      flags[i++] = '?';
    }


  if (dr6() & (1 << brk->num)) flags[i++] = 'O'; /* occured */
  if (brk->cond) flags[i++]='C'; /* conditional */
  if (brk->stop) flags[i++]='S'; /* stop */
  if (brk->os)   flags[i++]='o'; /* one shot */
  if (brk->skip) flags[i++]='N'; /* being skipped */
  if (brk->save) flags[i++]='L'; /* Save state for local variables. */

  flags[i] = 0;

  i = brk->len+1;
  printk ("Breakpoint %1d @ %8.8X-%d    %s  pid=%d\n",
	  brk->num, brk->addr,i, flags, brk->pid);

  regname="(none)";
  if (brk->creg >= 0)
    {
      for (i = DR7NO+1; regs[i].name != NULL; i++)
	{
	  if (regs[i].offset == brk->creg)
	    {
	      regname = regs[i].name;
	      break;
	    }
	}
    }
  
  printk ("           caddr = %8.8X, cmask = %8.8X, cres = %8.8X creg=%s\n",
	  brk->caddr, brk->cmask, brk->cres, regname);
  printk ("           exec = %8.8X\n", brk->exec);

}
		 
/* this routine will let you print out different types. */
static void
dbg_print (int argc, char *argv[])
{
  unsigned long addr;
  int i;

  /* if we have one argument then it's a name.  If we have two then
     its the form (type) name. */
  if (argc < 2 || argc > 3)
    {
      printk ("print variable OR print type address\n");
      return;
    }

  if (argc == 2)
    {
      if (strcmp(argv[1], HELP) == 0)
	{
	  printk ("print -- print out a location with possible typing.\n");
	  printk ("         known variables/types are\n");
	  for (i = 0; globals[i].type != NULL; i++)
	    {
	      printk ("       %s %s\n", globals[i].type, globals[i].name);
	    }
	  printk ("\n");
	  return;
	}
      if (strcmp(argv[1], USAGE) == 0)
	{
	  printk ("print variable OR print type address\n");
	  return;
	}

      for (i = 0; globals[i].name != NULL; i++)
	{
	  if (strcmp (argv[1],globals[i].name) == 0)
	    {
	      printk ("%8.8X-%d:\n", globals[i].addr,globals[i].len);
	      if (globals[i].print)
		globals[i].print(globals[i].addr);
	      else
		printk ("Unable to print. \n");
	      return;
	    }
	}
      printk ("Unknown variable %s\n", argv[1]);
      return;
    }

  if (argc != 3)
    {
      printk ("print variable OR print type address\n");
      return;
    }
  for (i = 0; globals[i].type != NULL; i++)
    {
      if (strcmp (argv[1],globals[i].type) == 0)
	{
	  if (atox (argv[2],&addr))
	    {
	      printk ("%8.8X-%d:\n", addr,globals[i].len);
	      if (globals[i].print)
		globals[i].print (addr);
	      else
		printk ("Unable to print.\n");
	      return;
	    }
	  else
	    {
	      printk ("print: bad address - %s\n",argv[2]);
	      return;
	    }
	}
    }
  printk ("print: Unknown type - %s\n", argv[1]);
}

#if 0		 
/* this routine will let you enter data into different types. */
static void
dbg_enter (int argc, char *argv[])
{
  unsigned long addr;
  int i;

  /* if we have one argument then it's a name.  If we have two then
     its the form (type) name. */
  if (argc < 2 || argc > 3)
    {
      printk ("enter variable OR enter type address\n");
      return;
    }

  if (argc == 2)
    {
      if (strcmp(argv[1], HELP) == 0)
	{
	  printk ("enter -- enter data into a known type.\n");
	  printk ("         known variables/types are\n");
	  for (i = 0; globals[i].type != NULL; i++)
	    {
	      printk ("       %s %s\n", globals[i].type, globals[i].name);
	    }
	  printk ("\n");
	  return;
	}
      if (strcmp(argv[1], USAGE) == 0)
	{
	  printk ("enter variable OR enter type address\n");
	  return;
	}

      for (i = 0; globals[i].name != NULL; i++)
	{
	  if (strcmp (argv[1],globals[i].name) == 0)
	    {
	      printk ("%8.8X-%d:\n", globals[i].addr,globals[i].len);
	      if (globals[i].enter)
		globals[i].enter(globals[i].addr);
	      else
		printk ("Unable to enter. \n");
	      return;
	    }
	}
      printk ("Unknown variable %s\n", argv[1]);
      return;
    }

  if (argc != 3)
    {
      printk ("enter variable OR enter type address\n");
      return;
    }
  for (i = 0; globals[i].type != NULL; i++)
    {
      if (strcmp (argv[1],globals[i].type) == 0)
	{
	  if (atox (argv[2],&addr))
	    {
	      printk ("%8.8X-%d:\n", addr,globals[i].len);
	      if (globals[i].enter)
		globals[i].print (addr);
	      else
		printk ("Unable to enter.\n");
	      return;
	    }
	  else
	    {
	      printk ("enter: bad address - %s\n",argv[2]);
	      return;
	    }
	}
    }
  printk ("enter: Unknown type - %s\n", argv[1]);
}
#endif /* 0 */

static void
dbg_break(int argc, char *argv[])
{
  unsigned long value;
  unsigned long len;
  unsigned long num;
  short type;

  if (argc == 1)
    {
      /*list break points. */
      for (num =0; num < 4; num++)
	{
	  print_break(brks+num);
	}
      return;
    }

  if (strcmp(argv[1],HELP) == 0)
    {
      printk("break -- set, list, or delete a data/code breakpoint.\n");
      return;
    }

  if (argc == 2 && !atoi(argv[1],&value))
    {
      if (value >= 4)
	{
	  printk ("bad break point\n");
	  return;
	}

      /* turn off break value. */
      brks[value].active = 0;
      del_break (brks+value);
      return;
    }
    
  if ( (argc != 4 && argc != 5) ||
      atox(argv[2],&value) || strlen(argv[3]) != 1
      || atox(argv[1],&num) ||  num > 3)
    {
      printk("break [num [address type [len]]]\n");
      return;
    }

  if (*argv[3] == 'c')
    type = 0;
  else if (*argv[3] == 'w')
    type = 1;
  else
    type = 3;

  if (argc == 5)
    {
      if (atox(argv[4], &len) || len > 4 || len == 3 || len == 0)
	{
	  printk ("bad len\n");
	  return;
	}
    }
  else
    {
      len = 4;
    }

  if (type == 0) len = 1; /* only valid len for code brk pts is 1 */
  len--;
  
  brks[num].active = 1;
  brks[num].cond = 0;
  brks[num].stop = 1;
  brks[num].skip = 0;
  brks[num].save = 0;
  brks[num].pid = 0;
  brks[num].num = num;
  brks[num].len = len;
  brks[num].type = type;
  brks[num].addr = value | 0xc0000000; /* adjust for kernel virtual address. */
  brks[num].exec = NULL;
  set_break (brks+num);

}

static void
dbg_edit (int argc, char *argv[])
{
  int i;
  unsigned long addr;
  unsigned long data;

  if (argc > 1 && strcmp (argv[1],HELP) == 0)
    {
      printk ("edit -- change some bytes in memory. \n");
      return;
    }

  if (atox(argv[1],&addr) || argc < 3) 
    {
      printk ("edit addr byte0 [byte1 ...]\n");
      return;
    }

  for (i = 2; i < argc; i++)
    {
      if (atox(argv[i],&data) || data > 255)
	{
	  printk ("bad value - %s\n",argv[i]);
	  return;
	}
      put_byte (addr, data);
      addr++;
    }
}


static void
dbg_outb (int argc, char *argv[])
{
  int i;
  unsigned long port;
  unsigned long data;

  if (argc > 1 && strcmp (argv[1],HELP) == 0)
    {
      printk ("outb -- Output a byte to an i/o port.\n");
      return;
    }

  if (atox(argv[1],&port) || argc < 3) 
    {
      printk ("outb port byte0 [byte1 ...]\n");
      return;
    }

  for (i = 2; i < argc; i++)
    {
      if (atox(argv[i],&data) || data > 255)
	{
	  printk ("bad value - %s\n",argv[i]);
	  return;
	}
      outb_p (data, port);
    }
}


static void
dbg_inb (int argc, char *argv[])
{
  unsigned long port;

  if (argc > 1 && strcmp (argv[1],HELP) == 0)
    {
      printk ("inb -- Input a byte from an i/o port.\n");
      return;
    }

  if (atox(argv[1],&port) || argc != 2) 
    {
      printk ("inb port\n");
      return;
    }

  printk ("in (%X) = %2.2X\n",port,inb(port));
}

static void  dbg_help(int, char **);

static struct dcmd dcmds[]=
{
  {"help", dbg_help},
  {"regs",dbg_regs},
  {"run",dbg_run},
  {"reboot",dbg_rboot},
  {"tasks", dbg_task},
  {"mem", dbg_mem},
  {"dump", dbg_dump},
  {"list", dbg_list},
  {"break", dbg_break},
  {"edit", dbg_edit},
  {"stack", dbg_stack},
  {"step", dbg_step},
  {"current", dbg_current},
  {"outb", dbg_outb},
  {"inb", dbg_inb},
  {"print", dbg_print},
/*  {"enter", dbg_enter},*/
  {NULL, dbg_nocmd}
};

static void
dbg_help(int argc, char *argv[])
{
  int i;
  static char *usage[]={
    "",
    USAGE
    };
  static char *help[]={
    "",
    HELP
    };

  if (argc == 1)
    {
      for (i = 0; dcmds[i].name != NULL; i++)
	{
	  dcmds[i].func(2, usage);
	}
      return;
    }

  if (strcmp (argv[1],HELP) == 0)
    {
      printk ("help -- Print a list of commands. \n");
      return;
    }
  if (strcmp (argv[1],USAGE) == 0)
    {
      printk ("help [cmd]\n");
      return;
    }
  for (i = 0; dcmds[i].name != NULL; i++)
    {
      if (strncmp (argv[1],dcmds[i].name,strlen(argv[1])) == 0)
	{
	  dcmds[i].func(2,help);
	}
    }
}

static  int
split_line(char *fields[], char **string)
{
  int c;
  char *str;
  str = *string;
  for (c=0; c < MAX_FIELDS; c++)
    {
      fields[c]=NULL;

      /* ignore leading white space. */
      str += strspn(str,WHITESPACE);

      /* see if it's a multiple command */
      if (*str == ';')
	{
	  *string = str+1;
	  return (c);
	}
      /* are we done? */
      if (*str == 0)
	{
	  *string=str;
	  return (c);
	}
      /* mark the start. */
      fields[c]=str;

      /* look for the next white space. */
      while (!isspace (*str) && *str != 0)
	{
	  int count=0;
	  if (*str == '(')
	    {
	      do {
		  if (*str == 0)
		    break;
		  if (*str == '(') count ++;
		  if (*str == ')') count --;
		  str++;
		} while (count);
	    }
	  if (*str != 0)
	    str++;
	}

      if (str == NULL || *str == 0) 
	{
	  fields[c+1] = NULL;
	  if (str != NULL)
	    {
	      *string=str;
	    }
	  else
	    {
	      *string=*string+strlen(*string);
	    }
	  return (c+1);
	}
      *str=0;
      str++;
    }
  *string=str;
  return (c);
}

void
do_dcmd(char *cmd)
{
  int i;
  int count;
  static char *fields[MAX_FIELDS];
  printk("\n");
  sto_lower(cmd);
  
  while (*cmd!=0)
    {
      count=split_line(fields,&cmd);
      if (count > 0)
	{
	  for (i=0; dcmds[i].name != NULL; i++)
	    {
	      if (strncmp(dcmds[i].name,fields[0],strlen(fields[0])) == 0)
		break;
	    }
	  dcmds[i].func(count, fields);
	}
    }
}

void debug_char (unsigned char c, struct pt_regs *pt)
{
  static char buff[80];
  static bptr=0;
  /* this routine gets called with every keystroke when
     the debugger is active.  We need to echo the character
     and store it.  Unless it's a return in which case we
     act on it. */

  if (!k_debug)
    {
      kstatus &= ~KEY_DBG;
      return;
    }
  if (k_debug == 1)
    {
      last_regs = pt;
    }

  if (c == 0x7f)
    {
      if (bptr > 0)
	{
	  bptr --;
	  printk("%c",c);
	}
    }
  else
    {
      buff[bptr++]=c;
      printk("%c",c);
      
      if (bptr == 79 || c==13)
	{
	  buff[bptr]=0;
	  bptr=0;
	  do_input(buff);
	  if (k_debug != 0) prompt();
	}
    }
}

void
init_ldebug (void)
{
  /* grab some memory and for saving the stacks later. */
  int i,j;
  set_dr6(0);
  set_dr7(0);
  for (i =0; i <4; i++)
    {
      states[i].stack_copy = get_free_page(GFP_KERNEL);
      states[i].regs = (void *)states[i].stack_copy;
      for (j=0; j < 500; j++)
	{
	  states[i].saved_mem[j].addr = 0;
	}
    }
}

