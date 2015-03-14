#include <unistd.h>
#include <sys/syscall.h>

volatile void
_exit(int exit_code)
{
  __asm__ volatile ("int $0x80"::"a" (SYS_exit),"b" (exit_code):"bx");
}
