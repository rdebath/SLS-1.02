#ifndef __LINUX_KEYBOARD_H
#define __LINUX_KEYBOARD_H

#include <linux/interrupt.h>
#define set_leds() mark_bh(KEYBOARD_BH)

/*
 * Global flags: things that don't change between virtual consoles.
 * This includes things like "key-down" flags - if the shift key is
 * down when you change a console, it's down in both.
 *
 * Note that the KG_CAPSLOCK flags is NOT the flag that decides if
 * capslock is on or not: it's just a flag about the key being
 * physically down. The actual capslock status is in the local flags.
 */
extern unsigned long kbd_flags;

/*
 * These are the hardcoded global flags - use the numbers beyond 16
 * for non-standard or keyboard-dependent flags
 */
#define KG_LSHIFT	0
#define KG_RSHIFT	1
#define KG_LCTRL	2
#define KG_RCTRL	3
#define KG_ALT		4
#define KG_ALTGR	5
#define KG_CAPSLOCK	6

/*
 * "dead" keys - prefix key values that are valid only for the next
 * character code (sticky shift, E0/E1 special scancodes, diacriticals)
 */
extern unsigned long kbd_dead_keys;
extern unsigned long kbd_prev_dead_keys;

/*
 * these are the hardcoded dead key flags
 */
#define KGD_E0		0
#define KGD_E1		1

/*
 * kbd->xxx contains the VC-local things (flag settings etc..)
 * The low 3 local flags are hardcoded to be the led setting..
 */
struct kbd_struct {
	unsigned long flags;
	unsigned long default_flags;
	unsigned char kbd_flags;
};

extern struct kbd_struct kbd_table[];

/*
 * These are the local "softflags", giving actual keyboard modes. The
 * three first flags are coded to the led settings.
 */
#define VC_SCROLLOCK	0	/* scroll-lock mode */
#define VC_NUMLOCK	1	/* numeric lock mode */
#define VC_CAPSLOCK	2	/* capslock mode */
#define VC_APPLIC	3	/* application key mode */
#define VC_CKMODE	5	/* cursor key mode */
#define VC_REPEAT	6	/* keyboard repeat */
#define VC_RAW		7	/* raw (scancode) mode */
#define VC_CRLF		8	/* 0 - enter sends CR, 1 - enter sends CRLF */
#define VC_META		9	/* 0 - meta, 1 - meta=prefix with ESC */
#define VC_PAUSE	10	/* pause key pressed */

#define LED_MASK	7

extern unsigned long kbd_init(unsigned long);

extern inline int kbd_flag(int flag)
{
	return kbd_flags & (1 << flag);
}

extern inline void set_kbd_flag(int flag)
{
	kbd_flags |= 1 << flag;
}

extern inline void clr_kbd_flag(int flag)
{
	kbd_flags &= ~(1 << flag);
}

extern inline void chg_kbd_flag(int flag)
{
	kbd_flags ^= 1 << flag;
}

extern inline int kbd_dead(int flag)
{
	return kbd_prev_dead_keys & (1 << flag);
}

extern inline void set_kbd_dead(int flag)
{
	kbd_dead_keys |= 1 << flag;
}

extern inline void clr_kbd_dead(int flag)
{
	kbd_dead_keys &= ~(1 << flag);
}

extern inline void chg_kbd_dead(int flag)
{
	kbd_dead_keys ^= 1 << flag;
}

extern inline int vc_kbd_flag(struct kbd_struct * kbd, int flag)
{
	return ((kbd->flags >> flag) & 1);
}

extern inline void set_vc_kbd_flag(struct kbd_struct * kbd, int flag)
{
	kbd->flags |= 1 << flag;
}

extern inline void clr_vc_kbd_flag(struct kbd_struct * kbd, int flag)
{
	kbd->flags &= ~(1 << flag);
}

extern inline void chg_vc_kbd_flag(struct kbd_struct * kbd, int flag)
{
	kbd->flags ^= 1 << flag;
}

#endif
