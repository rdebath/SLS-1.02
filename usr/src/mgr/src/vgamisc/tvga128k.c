/*
 * Turn on 128K addressing mode on Trident 8900c VGA cards
 */
#ifdef OLDLIBC
/* library stubs for the new syscalls in pre-0.96 (mmap, munmap, ioperm) */
/* only needed here if you not have an updated Libc.a                    */ 
#define __LIBRARY__
#include "/usr/src/linux/include/unistd.h"
/*#include "/usr/src/linux/include/sys/types.h"*/
/*#include "/usr/src/linux/include/sys/mman.h"*/
_syscall3(int,ioperm, unsigned long,from, unsigned long,num, int,turn_on)
#undef __LIBRARY__
#endif

static void inline port_out(char value, unsigned short port)
{
__asm__ volatile ("outb %0,%1"
		::"a" ((char) value),"d" ((unsigned short) port));
}

void main(void)
{
  if (ioperm(0x3c4,1,1))  {
    printf("Can't get I/O permissions\n");
    exit(1);
  }
  port_out(0x0b, 0x3c4); 
}
