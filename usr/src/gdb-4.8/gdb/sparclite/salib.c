/* Stand-alone library for SPARClite */

extern unsigned long get_xmt_status();
extern void xmt_char();

asm("
	.text
	.align 4
	.globl _get_uart_status
_get_uart_status:
	set 0x10000025, %o0
	retl
	lduba [%o0] 4, %o0

	.globl _set_uart_stuff
_set_uart_stuff:
	set 0x10000025, %o1
	retl
	stba %o0, [%o1] 4

	.globl _xmt_char
_xmt_char:
	set 0x10000021, %o1
	retl
	stba %o0, [%o1] 4

	.globl _rcv_char
_rcv_char:
	set 0x10000021, %o0
	retl
	lduba [%o0] 4, %o0

	.globl	_set_timer_3
_set_timer_3:
	set	0x10000078, %o1	! Address of TCR3 reload register
	retl
	stha	%o0, [%o1] 4	! Set the reg

! Register window overflow handler.  Come here when save would move us
! into the invalid window.  This routine runs with traps disabled, and
! must be careful not to touch the condition codes, as PSR is never
! restored.
!
! We are called with %l0 = wim, %l1 = pc, %l2 = npc

	.globl win_ovf
win_ovf:
	mov	%g1, %l3		! Save g1, we use it to hold the wim
	srl	%l0, 1, %g1		! Rotate wim right
	sll	%l0, 8-1, %l0
	or	%l0, %g1, %g1

	save	%g0, %g0, %g0		! Slip into next window
	mov	%g1, %wim		! Install the new wim

	std	%l0, [%sp + 0 * 4]	! save L & I registers
	std	%l2, [%sp + 2 * 4]
	std	%l4, [%sp + 4 * 4]
	std	%l6, [%sp + 6 * 4]

	std	%i0, [%sp + 8 * 4]
	std	%i2, [%sp + 10 * 4]
	std	%i4, [%sp + 12 * 4]
	std	%i6, [%sp + 14 * 4]

	restore				! Go back to trap window.
	mov	%l3, %g1		! Restore %g1

	jmpl	%l1,  %g0
	rett	%l2

! Register window underflow handler.  Come here when restore would move us
! into the invalid window.  This routine runs with traps disabled, and
! must be careful not to touch the condition codes, as PSR is never
! restored.
!
! We are called with %l0 = wim, %l1 = pc, %l2 = npc

	.globl win_unf
win_unf:
	sll	%l0, 1, %l3		! Rotate wim left
	srl	%l0, 8-1, %l0
	or	%l0, %l3, %l0

	mov	%l0, %wim		! Install the new wim

	restore				! User's window
	restore				! His caller's window

	ldd	[%sp + 0 * 4], %l0	! restore L & I registers
	ldd	[%sp + 2 * 4], %l2
	ldd	[%sp + 4 * 4], %l4
	ldd	[%sp + 6 * 4], %l6

	ldd	[%sp + 8 * 4], %i0
	ldd	[%sp + 10 * 4], %i2
	ldd	[%sp + 12 * 4], %i4
	ldd	[%sp + 14 * 4], %i6

	save	%g0, %g0, %g0		! Back to trap window
	save	%g0, %g0, %g0

	jmpl	%l1,  %g0
	rett	%l2

! Turn on the cache

	.globl	_cache_on
_cache_on:
	sta	%g0, [%g0] 1		! First, make sure cache is off
					!  before we diddle the tags
	set	63 * 4 * 4, %o0
	set	0x80000000, %o1

! First, reset all of the cache line valid bits, then turn on the cache

cache_loop:
	sta	%g0, [%o0] 0x0c		! Bank 1, icache
	sta	%g0, [%o0 + %o1] 0x0c	! Bank 2, icache

	sta	%g0, [%o0] 0x0e		! Bank 1, dcache
	sta	%g0, [%o0 + %o1] 0x0e	! Bank 2, dcache

	subcc	%o0, 16, %o0
	bge	cache_loop
	nop

	set	0x35, %o0		! Write buf ena, Prefetch buf ena,
					! Data & Inst caches enab
	retl
	sta	%o0, [%g0] 1		! Turn on the cache

! Flush the instruction cache.  We need to do this for the debugger stub so
! that breakpoints, et. al. become visible to the instruction stream after
! storing them in memory.

	.globl	_flush_i_cache
_flush_i_cache:
	lda	[%g0] 1, %o3		! Get cache/bus interface reg
	btst	1, %o3
	bz	popj			! Branch if i-cache is off
	nop

	sta	%g0, [%g0] 1		! Make sure cache is off
	nop
	nop
	nop

	set	63 * 4 * 4, %o0		! Loop size
	set	0x80000000, %o1		! Base addr of bank 2
fi_loop:
	lda	[%o0] 0x0c, %o2		! Bank 1, icache
	bclr	0x3c0, %o2		! Clear valid bits
	sta	%o2, [%o0] 0x0c

	lda	[%o0 + %o1] 0x0c, %o2	! Bank 2, icache
	bclr	0x3c0, %o2		! Clear valid bits
	sta	%o2, [%o0 + %o1] 0x0c

	subcc	%o0, 16, %o0
	bge	fi_loop
	nop

	sta	%o3, [%g0] 1		! Turn cache back on

popj:	retl
	nop

	.globl	_cache_off

_cache_off:
	retl
	nop

! Read the TBR.

	.globl _rdtbr
_rdtbr:
	retl
	mov	%tbr, %o0

");

extern unsigned long rdtbr();

void
__main()
{
  int x;

#if 0
  set_uart_stuff(0x4e);
  rdtbr();
  rdtbr();
  rdtbr();
  rdtbr();
  rdtbr();
  set_uart_stuff(0x35);
#endif
};

/* Each entry in the trap vector occupies four words. */

struct trap_entry
{
  unsigned sethi_filler:10;
  unsigned sethi_imm22:22;
  unsigned jmpl_filler:19;
  unsigned jmpl_simm13:13;
  unsigned long filler[2];
};

extern struct trap_entry fltr_proto;
asm ("
	.data
	.globl _fltr_proto
	.align 4
_fltr_proto:			! First level trap routine prototype
	sethi 0, %l0
	jmpl 0+%l0, %g0
	nop
	nop

	.text
	.align 4
");

void
exceptionHandler(tt, routine)
     int tt;
     unsigned long routine;
{
  struct trap_entry *tb;	/* Trap vector base address */

  tb = (struct trap_entry *)(rdtbr() & ~0xfff);

  tb[tt] = fltr_proto;

  tb[tt].sethi_imm22 = routine >> 10;
  tb[tt].jmpl_simm13 = routine & 0x3ff;
}

void
update_leds()
{
  static unsigned char *leds = (unsigned char *)0x02000003;
  static unsigned char curled = 1;
  static unsigned char dir = 0;

  *leds = ~curled;

  if (dir)
    curled <<= 1;
  else
    curled >>= 1;

  if (curled == 0)
    {
      if (dir)
	curled = 0x80;
      else
	curled = 1;
      dir = ~dir;
    }
}

#if 0
void
update_leds()
{
  static unsigned char *leds = (unsigned char *)0x02000003;
  static unsigned char curled = 0xfe;

  *leds = curled;
  curled = (curled >> 1) | (curled << 7);
}
#endif

 /* 1/5th of a second? */

#define LEDTIME (20000000 / 500)

int
getDebugChar()
{
  unsigned long countdown = LEDTIME;

  update_leds();

  while (1)
    {
      if ((get_uart_status(0) & 2) != 0) break;

      if (countdown-- == 0)
	{
	  countdown = LEDTIME;
	  update_leds();
	}
    }

  return rcv_char();
}

/* Output one character to the serial port */

void
putDebugChar(c)
     int c;
{
  update_leds();

  while ((get_uart_status() & 1) == 0) ;

  xmt_char(c);
}

int
write(fd, data, length)
     int fd;
     unsigned char *data;
     int length;
{
  int olength = length;

  while (length--)
    putDebugChar(*data++);

  return olength;
}

int
read(fd, data, length)
     int fd;
     unsigned char *data;
     int length;
{
  int olength = length;
  int c;

  while (length--)
    *data++ = getDebugChar();

  return olength;
}

/* Set the baud rate for the serial port, returns 0 for success,
   -1 otherwise */

int
set_baud_rate(baudrate)
     int baudrate;
{
  /* Convert baud rate to uart clock divider */
  switch (baudrate)
    {
    case 38400:
      baudrate = 16;
      break;
    case 19200:
      baudrate = 33;
      break;
    case 9600:
      baudrate = 65;
      break;
    default:
      return -1;
    }

  set_timer_3(baudrate);	/* Set it */
}
