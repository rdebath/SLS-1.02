/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/common/x386Priv.h,v 1.10 1992/09/12 08:31:39 dawes Exp $
 * Copyright 1990,91 by Thomas Roell, Dinkelscherben, Germany.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Thomas Roell not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Thomas Roell makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * THOMAS ROELL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THOMAS ROELL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * $Header: /proj/X11/mit/server/ddx/x386/RCS/x386Priv.h,v 1.3 1991/06/30 21:02:53 root Exp $
 */

#ifndef _X386PRIV_H
#define _X386PRIV_H


typedef struct {

  /* keyboard part */
  DevicePtr     pKeyboard;
  int           (* kbdProc)();        /* procedure for initializing */
  void          (* kbdEvents)();      /* proc for processing events */
  int           consoleFd;
#ifdef MACH386
  int           kbdFd;
#endif /* MACH386 */
  int           vtno;
  int           kbdType;              /* AT84 / AT101 */
  int           kbdRate;
  int           kbdDelay;
  int           bell_pitch;
  int           bell_duration;
  Bool          autoRepeat;
  unsigned long leds;
  unsigned long xleds;
  int           scanPrefix;           /* scancode-state */
  Bool          capsLock;
  Bool          numLock;
  Bool          scrollLock;
  Bool          modeSwitchLock;
  Bool          serverNumLock;

  /* pointer part */
  DevicePtr     pPointer;
  int           (* mseProc)();        /* procedure for initializing */
  void          (* mseEvents)();      /* proc for processing events */
  int           mseFd;
  char          *mseDevice;
  int           mseType;
  int           baudRate;
  int           sampleRate;
  int           lastButtons;
  int           threshold, num, den;  /* acceleration */
  int           emulateState;         /* automata state for 2 button mode */
  Bool          emulate3Buttons;

  /* xque part */
  int           xqueFd;
  int           xqueSema;

  /* event handler part */
  int           lastEventTime;
  Bool          vtRequestsPending;
  Bool          inputPending;
  Bool          dontZap;
  Bool          notrapSignals;           /* don't exit cleanly - die at fault */
  Bool          caughtSignal;

  /* graphics part */
  Bool          sharedMonitor;
  ScreenPtr     currentScreen;
#ifdef __386BSD__
  int           screenFd;	/* fd for memory mapped access to vga card */
#endif

} x386InfoRec, *x386InfoPtr;

extern x386InfoRec x386Info;

#define P_MS    0                     /* Microsoft */
#define P_MSC   1                     /* Mouse Systems Corp */
#define P_MM    2                     /* MMseries */
#define P_LOGI  3                     /* Logitech */
#define P_BM    4                     /* BusMouse ??? */

/* ISC's cc can't handle ~ of UL constants, so explicitly type cast them. */
#define XLED1   ((unsigned long) 0x00000001)
#define XLED2   ((unsigned long) 0x00000002)
#define XLED3   ((unsigned long) 0x00000004)
#define XCAPS   ((unsigned long) 0x20000000)
#define XNUM    ((unsigned long) 0x40000000)
#define XSCR    ((unsigned long) 0x80000000)

extern int x386ScreenIndex;

#define X386SCRNINFO(p) ((ScrnInfoPtr)((p)->devPrivates[x386ScreenIndex].ptr))

extern int x386MaxScreens;
extern ScrnInfoPtr x386Screens[];

extern char x386ConfigFile[];

#endif /* _X386PRIV_H */


