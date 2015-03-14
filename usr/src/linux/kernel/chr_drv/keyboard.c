/*
 * linux/kernel/chr_drv/keyboard.c
 *
 * Keyboard driver for Linux v0.96 using Latin-1.
 *
 * Written for linux by Johan Myreen as a translation from
 * the assembly version by Linus (with diacriticals added)
 *
 * Some additional features added by Christoph Niemann (ChN), March 1993
 */

#define KEYBOARD_IRQ 1

#include <linux/sched.h>
#include <linux/ctype.h>
#include <linux/tty.h>
#include <linux/mm.h>
#include <linux/ptrace.h>
#include <linux/keyboard.h>
#include <linux/interrupt.h>
#include <linux/config.h>
#include <linux/signal.h>

#ifndef KBD_DEFFLAGS
#ifdef CONFIG_KBD_META
#define KBD_DEFFLAGS ((1 << VC_NUMLOCK) | (1 << VC_REPEAT) | (1 << VC_META))
#else
#define KBD_DEFFLAGS ((1 << VC_NUMLOCK) | (1 << VC_REPEAT))
#endif
#endif

/*
 * The default IO slowdown is doing 'inb()'s from 0x61, which should be
 * safe. But as that is the keyboard controller chip address, we do our
 * slowdowns here by doing short jumps: the keyboard controller should
 * be able to keep up
 */
#define REALLY_SLOW_IO
#define SLOW_IO_BY_JUMPING
#include <asm/io.h>
#include <asm/system.h>

extern void do_keyboard_interrupt(void);
extern void ctrl_alt_del(void);
extern void change_console(unsigned int new_console);

#define fake_keyboard_interrupt() \
__asm__ __volatile__("int $0x21")

unsigned long kbd_flags = 0;
unsigned long kbd_dead_keys = 0;
unsigned long kbd_prev_dead_keys = 0;

static int want_console = -1;
static int last_console = 0;		/* last used VC */
static unsigned char rep = 0xff;	/* last pressed key */
struct kbd_struct kbd_table[NR_CONSOLES];
static struct kbd_struct * kbd = kbd_table;
static struct tty_struct * tty = NULL;

static volatile unsigned char acknowledge = 0;
static volatile unsigned char resend = 0;

typedef void (*fptr)(int);

static int diacr = -1;
static int npadch = 0;
fptr key_table[];

static void put_queue(int);
static void applkey(int);
static void cur(int);
static unsigned int handle_diacr(unsigned int);

static struct pt_regs * pt_regs;

static inline void kb_wait(void)
{
	int i;

	for (i=0; i<0x10000; i++)
		if ((inb_p(0x64) & 0x02) == 0)
			break;
}

static void keyboard_interrupt(int int_pt_regs)
{
	unsigned char scancode;

	pt_regs = (struct pt_regs *) int_pt_regs;
	kbd_prev_dead_keys |= kbd_dead_keys;
	if (!kbd_dead_keys)
		kbd_prev_dead_keys = 0;
	kbd_dead_keys = 0;
	kb_wait();
	if (!(inb_p(0x64) & 0x01))
		goto end_kbd_intr;
	scancode = inb(0x60);
	mark_bh(KEYBOARD_BH);
	if (scancode == 0xfa) {
		acknowledge = 1;
		goto end_kbd_intr;
	} else if (scancode == 0xfe) {
		resend = 1;
		goto end_kbd_intr;
	}
	tty = TTY_TABLE(0);
	kbd = kbd_table + fg_console;
	if (vc_kbd_flag(kbd,VC_RAW)) {
		kbd_flags = 0;
		put_queue(scancode);
		goto end_kbd_intr;
	}
	if (scancode == 0xe0) {
		set_kbd_dead(KGD_E0);
		goto end_kbd_intr;
	} else if (scancode == 0xe1) {
		set_kbd_dead(KGD_E1);
		goto end_kbd_intr;
	}
	/*
	 *  The keyboard maintains its own internal caps lock and num lock
	 *  statuses. In caps lock mode E0 AA precedes make code and E0 2A
	 *  follows break code. In num lock mode, E0 2A precedes make
	 *  code and E0 AA follows break code. We do our own book-keeping,
	 *  so we will just ignore these.
	 */
	if (kbd_dead(KGD_E0) && (scancode == 0x2a || scancode == 0xaa))
		goto end_kbd_intr;
	/*
	 *  Repeat a key only if the input buffers are empty or the
	 *  characters get echoed locally. This makes key repeat usable
	 *  with slow applications and unders heavy loads.
	 */
	if ((scancode != rep) || 
	    (vc_kbd_flag(kbd,VC_REPEAT) && tty &&
	     (L_ECHO(tty) || (EMPTY(&tty->secondary) && EMPTY(&tty->read_q)))))
		key_table[scancode](scancode);
	rep = scancode;
end_kbd_intr:
}

static void put_queue(int ch)
{
	struct tty_queue *qp;
	unsigned long new_head;

	wake_up(&keypress_wait);
	if (!tty)
		return;
	qp = &tty->read_q;

	qp->buf[qp->head]=ch;
	if ((new_head=(qp->head+1)&(TTY_BUF_SIZE-1)) != qp->tail)
		qp->head=new_head;
	wake_up_interruptible(&qp->proc_list);
}

static void puts_queue(char *cp)
{
	struct tty_queue *qp;
	unsigned long new_head;
	char ch;

	wake_up_interruptible(&keypress_wait);
	if (!tty)
		return;
	qp = &tty->read_q;

	while ((ch = *(cp++)) != 0) {
		qp->buf[qp->head]=ch;
		if ((new_head=(qp->head+1)&(TTY_BUF_SIZE-1))
				 != qp->tail)
			qp->head=new_head;
	}
	wake_up_interruptible(&qp->proc_list);
}

static void ctrl(int sc)
{
	if (kbd_dead(KGD_E0))
		set_kbd_flag(KG_RCTRL);
	else
		set_kbd_flag(KG_LCTRL);
}

static void alt(int sc)
{
	if (kbd_dead(KGD_E0))
		set_kbd_flag(KG_ALTGR);
	else
		set_kbd_flag(KG_ALT);
}

static void unctrl(int sc)
{
	if (kbd_dead(KGD_E0))
		clr_kbd_flag(KG_RCTRL);
	else
		clr_kbd_flag(KG_LCTRL);
}

static void unalt(int sc)
{
	if (kbd_dead(KGD_E0))
		clr_kbd_flag(KG_ALTGR);
	else {
		clr_kbd_flag(KG_ALT);
		if (npadch != 0) {
			put_queue(npadch);
			npadch=0;
		}
	}
}

static void lshift(int sc)
{
	set_kbd_flag(KG_LSHIFT);
}

static void unlshift(int sc)
{
	clr_kbd_flag(KG_LSHIFT);
}

static void rshift(int sc)
{
	set_kbd_flag(KG_RSHIFT);
}

static void unrshift(int sc)
{
	clr_kbd_flag(KG_RSHIFT);
}

static void caps(int sc)
{
	if (kbd_flag(KG_CAPSLOCK))
		return;		/* key already pressed: defeat repeat */
	set_kbd_flag(KG_CAPSLOCK);
	chg_vc_kbd_flag(kbd,VC_CAPSLOCK);
}

static void uncaps(int sc)
{
	clr_kbd_flag(KG_CAPSLOCK);
}

static void show_ptregs(void)
{
	if (!pt_regs)
		return;
	printk("\nEIP: %04x:%08x",0xffff & pt_regs->cs,pt_regs->eip);
	if (pt_regs->cs & 3)
		printk(" ESP: %04x:%08x",0xffff & pt_regs->cs,pt_regs->eip);
	printk(" EFLAGS: %08x",pt_regs->eflags);
	printk("\nEAX: %08x EBX: %08x ECX: %08x EDX: %08x",
		pt_regs->orig_eax,pt_regs->ebx,pt_regs->ecx,pt_regs->edx);
	printk("\nESI: %08x EDI: %08x EBP: %08x",
		pt_regs->esi, pt_regs->edi, pt_regs->ebp);
	printk(" DS: %04x ES: %04x FS: %04x GS: %04x\n",
		0xffff & pt_regs->ds,0xffff & pt_regs->es,
		0xffff & pt_regs->fs,0xffff & pt_regs->gs);
}

static void scroll(int sc)
{
	if (kbd_dead(KGD_E0))
		put_queue(INTR_CHAR(tty));
	else if ( sc != rep ) 	/* no autorepeat for scroll lock, ChN */
		if (kbd_flag(KG_LSHIFT) || kbd_flag(KG_RSHIFT))
			show_mem();
		else if (kbd_flag(KG_ALT) || kbd_flag(KG_ALTGR))
			show_ptregs();
		else if (kbd_flag(KG_LCTRL) || kbd_flag(KG_RCTRL))
			show_state();
		else {
			if (vc_kbd_flag(kbd, VC_SCROLLOCK))
				/* pressing srcoll lock 2nd time sends ^Q, ChN */
				put_queue(START_CHAR(tty));
			else

				/* pressing srcoll lock 1st time sends ^S, ChN */
				put_queue(STOP_CHAR(tty));
			chg_vc_kbd_flag(kbd,VC_SCROLLOCK);
		}
	
}

static void num(int sc)
{
	if (kbd_flag(KG_LCTRL))
		/* pause key pressed, sends E1 1D 45, ChN */
		chg_vc_kbd_flag(kbd,VC_PAUSE);
	else if (vc_kbd_flag(kbd,VC_APPLIC))
		applkey(0x50);
	else if ( rep != sc )	/* no autorepeat for numlock, ChN */
		chg_vc_kbd_flag(kbd,VC_NUMLOCK);
}

static void applkey(int key)
{
	char buf[] = { 0x1b, 0x4f, 0x00, 0x00 };

	buf[2] = key;
	puts_queue(buf);
}

static void sysreq(int sc)
{
	/* pressing alt-printscreen switches to the last used console, ChN */
	if ( sc != rep)
		want_console = last_console;
}

#if defined KBD_FINNISH

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  '+', '\'',  127,    9,
	'q',  'w',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  '}',    0,   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  '|',
	'{',    0,    0, '\'',  'z',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!', '\"',  '#',  '$',  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  ']',  '^',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L', '\\',
	'[',    0,    0,  '*',  'Z',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',  163,  '$',    0,    0,
	'{',   '[',  ']', '}', '\\',    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_FINNISH_LATIN1

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  '+',  180,  127,    9,
	'q',  'w',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  229,  168,   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  246,
	228,  167,    0, '\'',  'z',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!',  '"',  '#',  '$',  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  197,  '^',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  214,
	196,  189,    0,  '*',  'Z',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',  163,  '$',    0,    0,
	'{',  '[',  ']',  '}', '\\',    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_US

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  '-',  '=',  127,    9,
	'q',  'w',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  '[',  ']',   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  ';',
	'\'', '`',    0, '\\',  'z',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '/',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!',  '@',  '#',  '$',  '%',  '^',
	'&',  '*',  '(',  ')',  '_',  '+',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  '{',  '}',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  ':',
	'"',  '~',  '0',  '|',  'Z',  'X',  'C',  'V',
	'B',  'N',  'M',  '<',  '>',  '?',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',    0,  '$',    0,    0,
	'{',   '[',  ']', '}', '\\',    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_UK

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  '-',  '=',  127,    9,
	'q',  'w',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  '[',  ']',   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  ';',
	'\'', '`',    0,  '#',  'z',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '/',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0, '\\',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!',  '"',  163,  '$',  '%',  '^',
	'&',  '*',  '(',  ')',  '_',  '+',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  '{',  '}',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  ':',
	'@',  '~',  '0',  '~',  'Z',  'X',  'C',  'V',
	'B',  'N',  'M',  '<',  '>',  '?',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',    0,  '$',    0,    0,
	'{',   '[',  ']', '}', '\\',    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_GR

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0', '\\', '\'',  127,    9,
	'q',  'w',  'e',  'r',  't',  'z',  'u',  'i',
	'o',  'p',  '@',  '+',   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  '[',
	']',  '^',    0,  '#',  'y',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!',  '"',  '#',  '$',  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Z',  'U',  'I',
	'O',  'P', '\\',  '*',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  '{',
	'}',  '~',    0, '\'',  'Y',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',    0,  '$',    0,    0,
	'{',   '[',  ']', '}', '\\',    0,    0,    0,
	'@',    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_GR_LATIN1

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0', 223,  180,  127,    9,
	'q',  'w',  'e',  'r',  't',  'z',  'u',  'i',
	'o',  'p',  252,  '+',   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l', 246,
	228,   94,    0,  '#',  'y',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!',  '"',  167,  '$',  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Z',  'U',  'I',
	'O',  'P',  220,  '*',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  214,
	196,  176,    0, '\'',  'Y',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  178,  179,  '$',    0,    0,
	'{',   '[',  ']', '}', '\\',    0,    0,    0,
	'@',    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  181,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_FR

static unsigned char key_map[] = {
	  0,   27,  '&',  '{',  '"', '\'',  '(',  '-',
	'}',  '_',  '/',  '@',  ')',  '=',  127,    9,
	'a',  'z',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  '^',  '$',   13,    0,  'q',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  'm',
	'|',  '`',    0,   42,  'w',  'x',  'c',  'v',
	'b',  'n',  ',',  ';',  ':',  '!',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  ']',  '+',  127,    9,
	'A',  'Z',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  '<',  '>',   13,    0,  'Q',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  'M',
	'%',  '~',    0,  '#',  'W',  'X',  'C',  'V',
	'B',  'N',  '?',  '.',  '/', '\\',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '~',  '#',  '{',  '[',  '|',
	'`', '\\',   '^',  '@', ']',  '}',    0,    0,
	'@',    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_FR_LATIN1

static unsigned char key_map[] = {
	  0,   27,  '&',  233,  '"', '\'',  '(',  '-',
	232,  '_',  231,  224,  ')',  '=',  127,    9,
	'a',  'z',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  '^',  '$',   13,    0,  'q',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  'm',
	249,  178,    0,   42,  'w',  'x',  'c',  'v',
	'b',  'n',  ',',  ';',  ':',  '!',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  176,  '+',  127,    9,
	'A',  'Z',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  168,  163,   13,    0,  'Q',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  'M',
	'%',    0,    0,  181,  'W',  'X',  'C',  'V',
	'B',  'N',  '?',  '.',  '/',  167,    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '~',  '#',  '{',  '[',  '|',
	'`', '\\',   '^',  '@', ']',  '}',    0,    0,
	'@',    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  164,   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_DK

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  '+', '\'',  127,    9,
	'q',  'w',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  229,    0,   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  230,
	162,    0,    0, '\'',  'z',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!', '\"',  '#',  '$',  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  197,  '^',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  198,
	165,    0,    0,  '*',  'Z',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',  163,  '$',    0,    0,
	'{',   '[',  ']', '}',    0,  '|',    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '\\',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_DK_LATIN1

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  '+',  180,  127,    9,
	'q',  'w',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  229,  168,   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  230,
	162,  189,    0, '\'',  'z',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!', '\"',  '#',  '$',  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  197,  '^',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  198,
	165,  167,    0,  '*',  'Z',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',  163,  '$',    0,    0,
	'{',   '[',  ']', '}',    0,  '|',    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0, '\\',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_DVORAK

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0', '\\',  '=',  127,    9,
	'\'', ',',  '.',  'p',  'y',  'f',  'g',  'c',
	'r',  'l',  '/',  ']',   13,    0,  'a',  'o',
	'e',  'u',  'i',  'd',  'h',  't',  'n',  's',
	'-',  '`',    0,  '[',  ';',  'q',  'j',  'k',
	'x',  'b',  'm',  'w',  'v',  'z',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!',  '@',  '#',  '$',  '%',  '^',
	'&',  '*',  '(',  ')',  '|',  '+',  127,    9,
	'"',  '<',  '>',  'P',  'Y',  'F',  'G',  'C',
	'R',  'L',  '?',  '}',   13,    0,  'A',  'O',
	'E',  'U',  'I',  'D',  'H',  'T',  'N',  'S',
	'_',  '~',    0,  '{',  ':',  'Q',  'J',  'K',
	'X',  'B',  'M',  'W',  'V',  'Z',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',    0,  '$',    0,    0,
	'{',   '[',  ']', '}', '\\',    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '|',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_SG

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0', '\'',  '^',  127,    9,
	'q',  'w',  'e',  'r',  't',  'z',  'u',  'i',
	'o',  'p',    0,    0,   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',    0,
	  0,    0,    0,  '$',  'y',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '+',  '"',  '*',    0,  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Z',  'U',  'I',
	'O',  'P',    0,  '!',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',    0,
	  0,    0,    0,    0,  'Y',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',  '#',    0,    0,    0,
	'|',    0,    0,    0, '\'',  '~',    0,    0,
	'@',    0,    0,    0,    0,    0,    0,    0,
	  0,    0,   '[',  ']',  13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	'{',    0,    0,  '}',    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0, '\\',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_SG_LATIN1

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0', '\'',  '^',  127,    9,
	'q',  'w',  'e',  'r',  't',  'z',  'u',  'i',
	'o',  'p',  252,    0,   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  246,
	228,  167,    0,  '$',  'y',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '+',  '"',  '*',  231,  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Z',  'U',  'I',
	'O',  'P',  220,  '!',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  214,
	196,  176,    0,  163,  'Y',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',  '#',    0,    0,  172,
	'|',  162,    0,    0, '\'',  '~',    0,    0,
	'@',    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '[',  ']',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,  233,
	'{',    0,    0,  '}',    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0, '\\',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_NO

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0',  '+', '\\',  127,    9,
	'q',  'w',  'e',  'r',  't',  'y',  'u',  'i',
	'o',  'p',  '}',  '~',   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  '|',
	'{',  '|',    0, '\'',  'z',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char shift_map[] = {
	  0,   27,  '!', '\"',  '#',  '$',  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Y',  'U',  'I',
	'O',  'P',  ']',  '^',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L', '\\',
	'[',    0,    0,  '*',  'Z',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

static unsigned char alt_map[] = {
	  0,    0,    0,  '@',    0,  '$',    0,    0,
	'{',   '[',  ']', '}',    0, '\'',    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,  '~',   13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_SF

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0', '\'',  '^',  127,    9,
	'q',  'w',  'e',  'r',  't',  'z',  'u',  'i',
	'o',  'p',    0,    0,   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',    0,
	  0,    0,   0,   '$',  'y',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };
static unsigned char shift_map[] = {
	  0,   27,  '+',  '"',  '*',    0,  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Z',  'U',  'I',
	'O',  'P',    0,  '!',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',    0,
	  0,    0,    0,    0,  'Y',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };
static unsigned char alt_map[] = {
	  0,    0,    0,  '@',  '#',    0,    0,    0,
	'|',    0,    0,    0,  '\'', '~',    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,   '[',  ']',  13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	 '{',   0,    0,   '}',   0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '\\',   0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };

#elif defined KBD_SF_LATIN1

static unsigned char key_map[] = {
	  0,   27,  '1',  '2',  '3',  '4',  '5',  '6',
	'7',  '8',  '9',  '0', '\'',  '^',  127,    9,
	'q',  'w',  'e',  'r',  't',  'z',  'u',  'i',
	'o',  'p',  232,  168,   13,    0,  'a',  's',
	'd',  'f',  'g',  'h',  'j',  'k',  'l',  233,
	224,  167,    0,  '$',  'y',  'x',  'c',  'v',
	'b',  'n',  'm',  ',',  '.',  '-',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '<',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };
static unsigned char shift_map[] = {
	  0,   27,  '+',  '"',  '*',  231,  '%',  '&',
	'/',  '(',  ')',  '=',  '?',  '`',  127,    9,
	'Q',  'W',  'E',  'R',  'T',  'Z',  'U',  'I',
	'O',  'P',  252,  '!',   13,    0,  'A',  'S',
	'D',  'F',  'G',  'H',  'J',  'K',  'L',  246,
	228,  176,    0,  163,  'Y',  'X',  'C',  'V',
	'B',  'N',  'M',  ';',  ':',  '_',    0,  '*',
	  0,   32,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,  '-',    0,    0,    0,  '+',    0,
	  0,    0,    0,    0,    0,    0,  '>',    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };
static unsigned char alt_map[] = {
	  0,    0,    0,  '@',  '#',    0,    0,  172,
	'|',   162,   0,    0,  180,  '~',    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,   '[',  ']',  13,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	 '{',   0,    0,   '}',   0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0,    0,    0,    0,    0,    0,  '\\',   0,
	  0,    0,    0,    0,    0,    0,    0,    0,
	  0 };
#else
#error "KBD-type not defined"
#endif

static void do_self(int sc)
{
	unsigned char ch;

	if (kbd_flag(KG_ALTGR))
		ch = alt_map[sc];
	else if (kbd_flag(KG_LSHIFT) || kbd_flag(KG_RSHIFT) ||
		 kbd_flag(KG_LCTRL) || kbd_flag(KG_RCTRL))
		ch = shift_map[sc];
	else
		ch = key_map[sc];

	if (ch == 0)
		return;

	if ((ch = handle_diacr(ch)) == 0)
		return;

	if (kbd_flag(KG_LCTRL) || kbd_flag(KG_RCTRL) ||
	    vc_kbd_flag(kbd,VC_CAPSLOCK))	/* ctrl or caps */
		if ((ch >= 'a' && ch <= 'z') || (ch >= 224 && ch <= 254))
			ch -= 32;
	if (kbd_flag(KG_LCTRL) || kbd_flag(KG_RCTRL))	/* ctrl */
		ch &= 0x1f;

	if (kbd_flag(KG_ALT))
		if (vc_kbd_flag(kbd,VC_META)) {
			put_queue('\033');
			put_queue(ch);
		} else
			put_queue(ch|0x80);
	else
		put_queue(ch);
}

unsigned char accent_table[5][64] = {
	" \300BCD\310FGH\314JKLMN\322PQRST\331VWXYZ[\\]^_"
	"`\340bcd\350fgh\354jklmn\362pqrst\371vwxyz{|}~",   /* accent grave */

	" \301BCD\311FGH\315JKLMN\323PQRST\332VWX\335Z[\\]^_"
	"`\341bcd\351fgh\355jklmn\363pqrst\372vwxyz{|}~",   /* accent acute */

	" \302BCD\312FGH\316JKLMN\324PQRST\333VWXYZ[\\]^_"
	"`\342bcd\352fgh\356jklmn\364pqrst\373vwxyz{|}~",   /* circumflex */

	" \303BCDEFGHIJKLMN\325PQRSTUVWXYZ[\\]^_"
	"`\343bcdefghijklm\361\365pqrstuvwxyz{|}~",	    /* tilde */

	" \304BCD\313FGH\316JKLMN\326PQRST\334VWXYZ[\\]^_"
	"`\344bcd\353fgh\357jklmn\366pqrst\374vwx\377z{|}~" /* dieresis */
};

/*
 * Check if dead key pressed. If so, check if same key pressed twice;
 * in that case return the char, otherwise store char and return 0.
 * If dead key not pressed, check if accented character pending. If
 * not: return the char, otherwise check if char is a space. If it is
 * a space return the diacritical. Else combine char with diacritical
 * mark and return.
 */

unsigned int handle_diacr(unsigned int ch)
{
	static unsigned char diacr_table[] =
		{'`', 180, '^', '~', 168, 0};		/* Must end with 0 */
	static unsigned char ret_diacr[] =
		{'`', '\'', '^', '~', '"' };		/* Must not end with 0 */
	int i;

	for(i=0; diacr_table[i]; i++)
		if (ch==diacr_table[i] && ((1<<i)&kbd->kbd_flags)) {
			if (diacr == i) {
				diacr=-1;
				return ret_diacr[i];	/* pressed twice */
			} else {
				diacr=i;		/* key is dead */
				return 0;
			}
		}
	if (diacr == -1)
		return ch;
	else if (ch == ' ') {
		ch=ret_diacr[diacr];
		diacr=-1;
		return ch;
	} else if (ch<64 || ch>122) {
		diacr=-1;
		return ch;
	} else {
		ch=accent_table[diacr][ch-64];
		diacr=-1;
		return ch;
	}
}

#if defined KBD_FR || defined KBD_US || defined KBD_UK || defined KBD_FR_LATIN1
static unsigned char num_table[] = "789-456+1230.";
#else
static unsigned char num_table[] = "789-456+1230,";
#endif

static unsigned char cur_table[] = "1A5-DGC+4B623";
static unsigned int pad_table[] = { 7,8,9,0,4,5,6,0,1,2,3,0,0 };

/*
    Keypad /			35	B7	Q
    Keypad *  (PrtSc)		37	B7	R
    Keypad NumLock		45	??	P
    Keypad 7  (Home)		47	C7	w
    Keypad 8  (Up arrow)	48	C8	x
    Keypad 9  (PgUp)		49	C9	y
    Keypad -			4A	CA	S
    Keypad 4  (Left arrow)	4B	CB	t
    Keypad 5			4C	CC	u
    Keypad 6  (Right arrow)	4D	CD	v
    Keypad +			4E	CE	l
    Keypad 1  (End)		4F	CF	q
    Keypad 2  (Down arrow)	50	D0	r
    Keypad 3  (PgDn)		51	D1	s
    Keypad 0  (Ins)		52	D2	p
    Keypad .  (Del)		53	D3	n
*/

static unsigned char appl_table[] = "wxyStuvlqrspn";

/*
  Set up keyboard to generate DEC VT200 F-keys.
  DEC F1  - F5  not implemented (DEC HOLD, LOCAL PRINT, SETUP, SW SESS, BREAK)
  DEC F6  - F10 are mapped to F6 - F10
  DEC F11 - F20 are mapped to Shift-F1 - Shift-F10
  DEC HELP and DEC DO are mapped to F11, F12 or Shift- F11, F12.
  Regular (?) Linux F1-F5 remain the same.
*/

static char *func_table[2][12] = { /* DEC F1 - F10 */ {
	"\033[[A",  "\033[[B",  "\033[[C",  "\033[[D",
	"\033[[E",  "\033[17~", "\033[18~", "\033[19~",
	"\033[20~", "\033[21~", "\033[28~", "\033[29~"
}, /* DEC F11 - F20 */ {
	"\033[23~", "\033[24~", "\033[25~", "\033[26~",
	"\033[28~", "\033[29~", "\033[31~", "\033[32~",
	"\033[33~", "\033[34~", "\033[28~", "\033[29~"
}};

static void cursor(int sc)
{
	if (sc < 0x47 || sc > 0x53)
		return;
	sc -= 0x47;
	if (sc == 12 &&
	    (kbd_flag(KG_LCTRL) || kbd_flag(KG_RCTRL)) &&
	    (kbd_flag(KG_ALT) || kbd_flag(KG_ALTGR))) {
		ctrl_alt_del();
		return;
	}
	if (kbd_dead(KGD_E0)) {
		cur(sc);
		return;
	}

	if (kbd_flag(KG_ALT) && sc != 12) {			/* Alt-numpad */
		npadch=npadch*10+pad_table[sc];
		return;
	}

	if (vc_kbd_flag(kbd,VC_APPLIC) &&
	    !kbd_flag(KG_LSHIFT) &&	/* shift forces cursor */
	    !kbd_flag(KG_RSHIFT)) {
		applkey(appl_table[sc]);
		return;
	}

	if (vc_kbd_flag(kbd,VC_NUMLOCK)) {
		put_queue(num_table[sc]);
	} else
		cur(sc);
}

static void cur(int sc)
{
	char buf[] = { 0x1b, '[', 0, 0, 0 };		/* must not be static */

	buf[2]=cur_table[sc];
	if (buf[2] < '9')
		buf[3]='~';
	else
		if ((buf[2] >= 'A' && buf[2] <= 'D') ?
		    vc_kbd_flag(kbd,VC_CKMODE) :
		    vc_kbd_flag(kbd,VC_APPLIC))
			buf[1]='O';
	puts_queue(buf);
}

static void func(int sc)
{
	if (sc < 0x3b)
		return;
	sc-=0x3b;
	if (sc > 9) {
		sc-=18;
		if (sc < 10 || sc > 11)
			return;
	}
	if (kbd_flag(KG_ALT))
		want_console = sc;
	else
		if (kbd_flag(KG_LSHIFT) || kbd_flag(KG_RSHIFT))	/* DEC F11 - F20 */
			puts_queue(func_table[1][sc]);
		else					/* DEC F1 - F10 */
			puts_queue(func_table[0][sc]);
}

static void slash(int sc)
{
	if (!kbd_dead(KGD_E0))
		do_self(sc);
	else if (vc_kbd_flag(kbd,VC_APPLIC))
		applkey('Q');
	else
		put_queue('/');
}

static void star(int sc)
{
	if (kbd_dead(KGD_E0))
		/* Print screen key sends E0 2A E0 37 and puts
		   the VT100-ESC sequence ESC [ i into the queue, ChN */
		puts_queue("\033\133\151");
	else if (vc_kbd_flag(kbd,VC_APPLIC))
		applkey('R');
	else
		do_self(sc);
}

static void enter(int sc)
{
	if (kbd_dead(KGD_E0) && vc_kbd_flag(kbd,VC_APPLIC))
		applkey('M');
	else {
		put_queue(13);
		if (vc_kbd_flag(kbd,VC_CRLF))
			put_queue(10);
	}
}

static void minus(int sc)
{
	if (vc_kbd_flag(kbd,VC_APPLIC))
		applkey('S');
	else
		do_self(sc);
}

static void plus(int sc)
{
	if (vc_kbd_flag(kbd,VC_APPLIC))
		applkey('l');
	else
		do_self(sc);
}

static void none(int sc)
{
}

/*
 * send_data sends a character to the keyboard and waits
 * for a acknowledge, possibly retrying if asked to. Returns
 * the success status.
 */
static int send_data(unsigned char data)
{
	int retries = 3;
	int i;

	do {
		kb_wait();
		acknowledge = 0;
		resend = 0;
		outb_p(data, 0x60);
		for(i=0; i<0x20000; i++) {
			inb_p(0x64);		/* just as a delay */
			if (acknowledge)
				return 1;
			if (resend)
				goto repeat;
		}
		return 0;
repeat:
	} while (retries-- > 0);
	return 0;
}

/*
 * This routine is the bottom half of the keyboard interrupt
 * routine, and runs with all interrupts enabled. It does
 * console changing, led setting and copy_to_cooked, which can
 * take a reasonably long time.
 *
 * Aside from timing (which isn't really that important for
 * keyboard interrupts as they happen often), using the software
 * interrupt routines for this thing allows us to easily mask
 * this when we don't want any of the above to happen. Not yet
 * used, but this allows for easy and efficient race-condition
 * prevention later on.
 */
static void kbd_bh(void * unused)
{
	static unsigned char old_leds = -1;
	unsigned char leds = kbd_table[fg_console].flags & LED_MASK;

	if (leds != old_leds) {
		old_leds = leds;
		if (!send_data(0xed) || !send_data(leds))
			send_data(0xf4);	/* re-enable kbd if any errors */
	}
	if (want_console >= 0) {
		if (want_console != fg_console) {
			last_console = fg_console;
			change_console(want_console);
		}
		want_console = -1;
	}
	do_keyboard_interrupt();
	cli();
	if (inb_p(0x64) & 0x01)
		fake_keyboard_interrupt();
	sti();
}

long no_idt[2] = {0, 0};

/*
 * This routine reboots the machine by asking the keyboard
 * controller to pulse the reset-line low. We try that for a while,
 * and if it doesn't work, we do some other stupid things.
 */
void hard_reset_now(void)
{
	int i, j;
	extern unsigned long pg0[1024];

	sti();
/* rebooting needs to touch the page at absolute addr 0 */
	pg0[0] = 7;
	*((unsigned short *)0x472) = 0x1234;
	for (;;) {
		for (i=0; i<100; i++) {
			kb_wait();
			for(j = 0; j < 100000 ; j++)
				/* nothing */;
			outb(0xfe,0x64);	 /* pulse reset low */
		}
		__asm__("\tlidt _no_idt"::);
	}
}

static fptr key_table[] = {
	none,do_self,do_self,do_self,		/* 00-03 s0 esc 1 2 */
	do_self,do_self,do_self,do_self,	/* 04-07 3 4 5 6 */
	do_self,do_self,do_self,do_self,	/* 08-0B 7 8 9 0 */
	do_self,do_self,do_self,do_self,	/* 0C-0F + ' bs tab */
	do_self,do_self,do_self,do_self,	/* 10-13 q w e r */
	do_self,do_self,do_self,do_self,	/* 14-17 t y u i */
	do_self,do_self,do_self,do_self,	/* 18-1B o p } ^ */
	enter,ctrl,do_self,do_self,		/* 1C-1F enter ctrl a s */
	do_self,do_self,do_self,do_self,	/* 20-23 d f g h */
	do_self,do_self,do_self,do_self,	/* 24-27 j k l | */
	do_self,do_self,lshift,do_self,		/* 28-2B { para lshift , */
	do_self,do_self,do_self,do_self,	/* 2C-2F z x c v */
	do_self,do_self,do_self,do_self,	/* 30-33 b n m , */
	do_self,slash,rshift,star,		/* 34-37 . - rshift * */
	alt,do_self,caps,func,			/* 38-3B alt sp caps f1 */
	func,func,func,func,			/* 3C-3F f2 f3 f4 f5 */
	func,func,func,func,			/* 40-43 f6 f7 f8 f9 */
	func,num,scroll,cursor,			/* 44-47 f10 num scr home */
	cursor,cursor,minus,cursor,		/* 48-4B up pgup - left */
	cursor,cursor,plus,cursor,		/* 4C-4F n5 right + end */
	cursor,cursor,cursor,cursor,		/* 50-53 dn pgdn ins del */
	sysreq,none,do_self,func,			/* 54-57 sysreq ? < f11 */
	func,none,none,none,			/* 58-5B f12 ? ? ? */
	none,none,none,none,			/* 5C-5F ? ? ? ? */
	none,none,none,none,			/* 60-63 ? ? ? ? */
	none,none,none,none,			/* 64-67 ? ? ? ? */
	none,none,none,none,			/* 68-6B ? ? ? ? */
	none,none,none,none,			/* 6C-6F ? ? ? ? */
	none,none,none,none,			/* 70-73 ? ? ? ? */
	none,none,none,none,			/* 74-77 ? ? ? ? */
	none,none,none,none,			/* 78-7B ? ? ? ? */
	none,none,none,none,			/* 7C-7F ? ? ? ? */
	none,none,none,none,			/* 80-83 ? br br br */
	none,none,none,none,			/* 84-87 br br br br */
	none,none,none,none,			/* 88-8B br br br br */
	none,none,none,none,			/* 8C-8F br br br br */
	none,none,none,none,			/* 90-93 br br br br */
	none,none,none,none,			/* 94-97 br br br br */
	none,none,none,none,			/* 98-9B br br br br */
	none,unctrl,none,none,			/* 9C-9F br unctrl br br */
	none,none,none,none,			/* A0-A3 br br br br */
	none,none,none,none,			/* A4-A7 br br br br */
	none,none,unlshift,none,		/* A8-AB br br unlshift br */
	none,none,none,none,			/* AC-AF br br br br */
	none,none,none,none,			/* B0-B3 br br br br */
	none,none,unrshift,none,		/* B4-B7 br br unrshift br */
	unalt,none,uncaps,none,			/* B8-BB unalt br uncaps br */
	none,none,none,none,			/* BC-BF br br br br */
	none,none,none,none,			/* C0-C3 br br br br */
	none,none,none,none,			/* C4-C7 br br br br */
	none,none,none,none,			/* C8-CB br br br br */
	none,none,none,none,			/* CC-CF br br br br */
	none,none,none,none,			/* D0-D3 br br br br */
	none,none,none,none,			/* D4-D7 br br br br */
	none,none,none,none,			/* D8-DB br ? ? ? */
	none,none,none,none,			/* DC-DF ? ? ? ? */
	none,none,none,none,			/* E0-E3 e0 e1 ? ? */
	none,none,none,none,			/* E4-E7 ? ? ? ? */
	none,none,none,none,			/* E8-EB ? ? ? ? */
	none,none,none,none,			/* EC-EF ? ? ? ? */
	none,none,none,none,			/* F0-F3 ? ? ? ? */
	none,none,none,none,			/* F4-F7 ? ? ? ? */
	none,none,none,none,			/* F8-FB ? ? ? ? */
	none,none,none,none			/* FC-FF ? ? ? ? */
};

unsigned long kbd_init(unsigned long kmem_start)
{
	int i;
	struct kbd_struct * kbd;

	kbd = kbd_table + 0;
	for (i = 0 ; i < NR_CONSOLES ; i++,kbd++) {
		kbd->flags = KBD_DEFFLAGS;
		kbd->default_flags = KBD_DEFFLAGS;
		kbd->kbd_flags = KBDFLAGS;
	}
	bh_base[KEYBOARD_BH].routine = kbd_bh;
	request_irq(KEYBOARD_IRQ,keyboard_interrupt);
	mark_bh(KEYBOARD_BH);
	return kmem_start;
}
