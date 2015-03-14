/*
 * $Header: /home/x_cvs/mit/server/ddx/x386/common/x386Events.c,v 1.17 1992/09/16 14:55:19 dawes Exp $
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
 * $Header: /proj/X11/mit/server/ddx/x386/RCS/x386Events.c,v 1.3 1991/06/30 21:02:38 root Exp $
 */

#define NEED_EVENTS
#include "X.h"
#include "Xproto.h"
#include "misc.h"
#include "inputstr.h"
#include "scrnintstr.h"

#include "compiler.h"

#include "x386Procs.h"
#include "x386OSD.h"
#include "atKeynames.h"
#include "osdep.h"

#define XE_POINTER  1
#define XE_KEYBOARD 2

#ifdef XTESTEXT1

#define	XTestSERVER_SIDE
#include "xtestext1.h"
extern short xtest_mousex;
extern short xtest_mousey;
extern int   on_steal_input;          
extern Bool  XTestStealKeyData();
extern void  XTestStealMotionData();

#define ENQUEUE(ev, code, direction, dev_type) \
  (ev)->u.u.detail = (code); \
  (ev)->u.u.type   = (direction); \
  if (!on_steal_input ||  \
      XTestStealKeyData((ev)->u.u.detail, (ev)->u.u.type, dev_type, \
			xtest_mousex, xtest_mousey)) \
  mieqEnqueue((ev))

#define MOVEPOINTER(dx, dy, time) \
  if (on_steal_input) \
    XTestStealMotionData(dx, dy, XE_POINTER, xtest_mousex, xtest_mousey); \
  miPointerDeltaCursor (dx, dy, time)

#else /* ! XTESTEXT1 */

#define ENQUEUE(ev, code, direction, dev_type) \
  (ev)->u.u.detail = (code); \
  (ev)->u.u.type   = (direction); \
  mieqEnqueue((ev))

#define MOVEPOINTER(dx, dy, time) \
  miPointerDeltaCursor (dx, dy, time)

#endif

Bool x386VTSema = TRUE;

extern long EnabledDevices[];

#ifdef HAS_USL_VTS
#ifndef linux
static void x386VTSwitch();
#endif /* !linux */
#endif

/*
 * Lets create a simple finite-state machine:
 *
 *   state[?][0]: action1
 *   state[?][1]: action2
 *   state[?][2]: next state
 *
 *   action > 0: ButtonPress
 *   action = 0: nothing
 *   action < 0: ButtonRelease
 *
 * Why this stuff ??? Normally you cannot press both mousebuttons together, so
 * the mouse reports both pressed at the same time ...
 */

static char stateTab[48][3] = {

/* nothing pressed */
  {  0,  0,  0 },	
  {  0,  0,  8 },	/* 1 right -> delayed right */
  {  0,  0,  0 },       /* 2 nothing */
  {  0,  0,  8 },	/* 3 right -> delayed right */
  {  0,  0, 16 },	/* 4 left -> delayed left */
  {  2,  0, 24 },       /* 5 left & right (middle press) -> middle pressed */
  {  0,  0, 16 },	/* 6 left -> delayed left */
  {  2,  0, 24 },       /* 7 left & right (middle press) -> middle pressed */

/* delayed right */
  {  1, -1,  0 },	/* 8 nothing (right event) -> init */
  {  1,  0, 32 },       /* 9 right (right press) -> right pressed */
  {  1, -1,  0 },	/* 10 nothing (right event) -> init */
  {  1,  0, 32 },       /* 11 right (right press) -> right pressed */
  {  1, -1, 16 },       /* 12 left (right event) -> delayed left */
  {  2,  0, 24 },       /* 13 left & right (middle press) -> middle pressed */
  {  1, -1, 16 },       /* 14 left (right event) -> delayed left */
  {  2,  0, 24 },       /* 15 left & right (middle press) -> middle pressed */

/* delayed left */
  {  3, -3,  0 },	/* 16 nothing (left event) -> init */
  {  3, -3,  8 },       /* 17 right (left event) -> delayed right */
  {  3, -3,  0 },	/* 18 nothing (left event) -> init */
  {  3, -3,  8 },       /* 19 right (left event) -> delayed right */
  {  3,  0, 40 },	/* 20 left (left press) -> pressed left */
  {  2,  0, 24 },	/* 21 left & right (middle press) -> pressed middle */
  {  3,  0, 40 },	/* 22 left (left press) -> pressed left */
  {  2,  0, 24 },	/* 23 left & right (middle press) -> pressed middle */

/* pressed middle */
  { -2,  0,  0 },	/* 24 nothing (middle release) -> init */
  { -2,  0,  0 },	/* 25 right (middle release) -> init */
  { -2,  0,  0 },	/* 26 nothing (middle release) -> init */
  { -2,  0,  0 },	/* 27 right (middle release) -> init */
  { -2,  0,  0 },	/* 28 left (middle release) -> init */
  {  0,  0, 24 },	/* 29 left & right -> pressed middle */
  { -2,  0,  0 },	/* 30 left (middle release) -> init */
  {  0,  0, 24 },	/* 31 left & right -> pressed middle */

/* pressed right */
  { -1,  0,  0 },	/* 32 nothing (right release) -> init */
  {  0,  0, 32 },	/* 33 right -> pressed right */
  { -1,  0,  0 },	/* 34 nothing (right release) -> init */
  {  0,  0, 32 },	/* 35 right -> pressed right */
  { -1,  0, 16 },	/* 36 left (right release) -> delayed left */
  { -1,  2, 24 },	/* 37 left & right (r rel, m prs) -> middle pressed */
  { -1,  0, 16 },	/* 38 left (right release) -> delayed left */
  { -1,  2, 24 },	/* 39 left & right (r rel, m prs) -> middle pressed */

/* pressed left */
  { -3,  0,  0 },	/* 40 nothing (left release) -> init */
  { -3,  0,  8 },	/* 41 right (left release) -> delayed right */
  { -3,  0,  0 },	/* 42 nothing (left release) -> init */
  { -3,  0,  8 },	/* 43 right (left release) -> delayed right */
  {  0,  0, 40 },	/* 44 left -> left pressed */
  { -3,  2, 24 },	/* 45 left & right (l rel, mprs) -> middle pressed */
  {  0,  0, 40 },	/* 46 left -> left pressed */
  { -3,  2, 24 },	/* 47 left & right (l rel, mprs) -> middle pressed */
};


/*
 * Table to allow quick reversal of natural button mapping to correct mapping
 */

static char reverseMap[8] = {0, 4, 2, 6, 1, 5, 3, 7};


/*
 * TimeSinceLastInputEvent --
 *      Function used for screensaver purposes by the os module. Retruns the
 *      time in milliseconds since there last was any input.
 */

int
TimeSinceLastInputEvent()
{
  if (x386Info.lastEventTime == 0) {
    x386Info.lastEventTime = GetTimeInMillis();
  }
  return GetTimeInMillis() - x386Info.lastEventTime;
}



/*
 * SetTimeSinceLastInputEvent --
 *      Set the lastEventTime to now.
 */

void
SetTimeSinceLastInputEvent()
{
  x386Info.lastEventTime = GetTimeInMillis();
}



/*
 * ProcessInputEvents --
 *      Retrieve all waiting input events and pass them to DIX in their
 *      correct chronological order. Only reads from the system pointer
 *      and keyboard.
 */

void
ProcessInputEvents ()
{
  int x, y;
  x386Info.inputPending = FALSE;

  mieqProcessInputEvents();
  miPointerUpdate();

  miPointerPosition(&x, &y);
  x386SetViewport(x386Info.currentScreen, x, y);
}



/*
 * x386PostKbdEvent --
 *	Translate the raw hardware KbdEvent into an XEvent, and tell DIX
 *	about it. Scancode preprocessing and so on is done ...
 */

void
x386PostKbdEvent(key)
     unsigned key;
{
  int         scanCode = (key & 0x7f);
  Bool        down = (key & 0x80 ? FALSE : TRUE);
  KeyClassRec *keyc = ((DeviceIntPtr)x386Info.pKeyboard)->key;
  Bool        updateLeds = FALSE;
  Bool        UsePrefix = FALSE;
  Bool        Direction = FALSE;
  xEvent      kevent;
  KeySym      *keysym;
  int         keycode;

  /*
   * First do some special scancode remapping ...
   */
  if (x386Info.scanPrefix == 0) {

    switch (scanCode) {
      
    case KEY_Prefix0:
    case KEY_Prefix1:
      x386Info.scanPrefix = scanCode;  /* special prefixes */
      return;

    case KEY_CapsLock:
    case KEY_NumLock:
    case KEY_ScrollLock:
      updateLeds = TRUE;              /* led changes by firmware */
      break;
    }
  }
	
  else if (x386Info.scanPrefix == KEY_Prefix0) {
    
    x386Info.scanPrefix = 0;
	  
    switch (scanCode) {
    case KEY_KP_7:        scanCode = KEY_Home;      break;  /* curs home */
    case KEY_KP_8:        scanCode = KEY_Up;        break;  /* curs up */
    case KEY_KP_9:        scanCode = KEY_PgUp;      break;  /* curs pgup */
    case KEY_KP_4:        scanCode = KEY_Left;      break;  /* curs left */
    case KEY_KP_5:        scanCode = KEY_Begin;     break;  /* curs begin */
    case KEY_KP_6:        scanCode = KEY_Right;     break;  /* curs right */
    case KEY_KP_1:        scanCode = KEY_End;       break;  /* curs end */
    case KEY_KP_2:        scanCode = KEY_Down;      break;  /* curs down */
    case KEY_KP_3:        scanCode = KEY_PgDown;    break;  /* curs pgdown */
    case KEY_KP_0:        scanCode = KEY_Insert;    break;  /* curs insert */
    case KEY_KP_Decimal:  scanCode = KEY_Delete;    break;  /* curs delete */
    case KEY_Enter:       scanCode = KEY_KP_Enter;  break;  /* keypad enter */
    case KEY_LCtrl:       scanCode = KEY_RCtrl;     break;  /* right ctrl */
    case KEY_KP_Multiply: scanCode = KEY_Print;     break;  /* print */
    case KEY_Slash:       scanCode = KEY_KP_Divide; break;  /* keyp divide */
    case KEY_Alt:         scanCode = KEY_AltLang;   break;  /* right alt */
    case KEY_ScrollLock:  scanCode = KEY_Break;     break;  /* curs break */
      /*
       * Ignore virtual shifts (E0 2A, E0 AA, E0 36, E0 B6)
       */
    default:
      return;                                  /* skip illegal */
    }
  }
  
  else if (x386Info.scanPrefix == KEY_Prefix1)
    {
      x386Info.scanPrefix = (scanCode == KEY_LCtrl) ? KEY_LCtrl : 0;
      return;
    }
  
  else if (x386Info.scanPrefix == KEY_LCtrl)
    {
      x386Info.scanPrefix = 0;
      if (scanCode != KEY_NumLock) return;
      scanCode = KEY_Pause;       /* pause */
    }
	
  /*
   * and now get some special keysequences
   */
  if ((ModifierDown(ControlMask | AltMask)) ||
      (ModifierDown(ControlMask | AltLangMask)))
    {
      
      switch (scanCode) {
	
      case KEY_BackSpace:
	if (!x386Info.dontZap) GiveUp();
	return;
	
	/*
	 * The idea here is to pass the scancode down to a list of
	 * registered routines. There should be some standart conventions
	 * for processing certain keys.
	 */
      case KEY_KP_Minus:   /* Keypad - */
	if (down) x386ZoomViewport(x386Info.currentScreen, -1);
	return;
	
      case KEY_KP_Plus:   /* Keypad + */
	if (down) x386ZoomViewport(x386Info.currentScreen,  1);
	return;
      } 
    }
	
  /*
   * Now map the scancodes to real X-keycodes ...
   */
  keycode = scanCode + MIN_KEYCODE;
  keysym = (keyc->curKeySyms.map +
	    keyc->curKeySyms.mapWidth * 
	    (keycode - keyc->curKeySyms.minKeyCode));
  
  /*
   * LockKey special handling:
   * ignore releases, toggle on & off on presses.
   */
  if (keysym[0] == XK_Caps_Lock ||
      keysym[0] == XK_Scroll_Lock ||
      keysym[0] == XK_Num_Lock)
    {
      Bool flag;

      if (!down) return;
      if (KeyPressed(keycode)) {
	down = !down;
	flag = FALSE;
      }
      else
	flag = TRUE;

      if (keysym[0] == XK_Caps_Lock)   x386Info.capsLock   = flag;
      if (keysym[0] == XK_Num_Lock)    x386Info.numLock    = flag;
      if (keysym[0] == XK_Scroll_Lock) x386Info.scrollLock = flag;
      updateLeds = TRUE;
    }
	
  /*
   * check for an autorepeat-event
   */
  if ((down && KeyPressed(keycode)) &&
      (x386Info.autoRepeat != AutoRepeatModeOn || keyc->modifierMap[keycode]))
    return;

  x386Info.lastEventTime = kevent.u.keyButtonPointer.time = GetTimeInMillis();

  /*
   * Ooops. Some new ideas here. The Mode_switch key is not defined either as
   * lock key or troggle key. My interpretation is that normally this key will
   * be treated as troggle. Except if KEY_AltL is allready pressed. Then no
   * release event will be sent till Mode_switch without KEY_AltL is pressed
   * again. The ScrollLock Led will signal this lockstate
   */
  if (keysym[0] == XK_Mode_switch) {
    if (ModifierDown(AltMask)) {
      if (down && !x386Info.modeSwitchLock) {
	x386Info.modeSwitchLock = TRUE;
	updateLeds = TRUE;
      }
      else
	return;
    }
    else if (!down && x386Info.modeSwitchLock) {
      x386Info.modeSwitchLock = FALSE;
      updateLeds = TRUE;
    }
  }

  /*
   * normal, non-keypad keys
   */
  else if (scanCode < KEY_KP_7 || scanCode > KEY_KP_Decimal) {
#if !defined(__386BSD__) && !defined(MACH386)
    /*
     * magic ALT_L key on AT84 keyboards for multilingual support
     */
    if (x386Info.kbdType == KB_84 &&
	ModifierDown(AltMask) &&
	keysym[2] != NoSymbol)
      {
	UsePrefix = TRUE;
	Direction = TRUE;
      }
#endif /* !MACH386 && !__386BSD__ */
  }
  /*
   * NumPad special Handling. If necessary a Prefix is sent
   */
  else if (x386Info.serverNumLock && x386Info.numLock) {
    if (!ModifierDown(AltLangMask)) {
      UsePrefix = TRUE;
      Direction = TRUE;
    }
  }

  /*
   * NumPad key, but no server numlock, or no numlock pressed. Since the AT
   * Keyboard proposal uses row 2 & 3, we must ensure, that here are no
   * Mode_switch keys are pressed.
   */
  else if (ModifierDown(AltLangMask)) {
    UsePrefix = TRUE;
    Direction = FALSE;
  }

  /*
   * And now send these prefixes ...
   * NOTE: There cannot be multible Mode_Switch keys !!!!
   */
  if (UsePrefix)
    {
      ENQUEUE(&kevent,
	      keyc->modifierKeyMap[keyc->maxKeysPerModifier*7],
	      (Direction ? KeyPress : KeyRelease),
	      XE_KEYBOARD);
      ENQUEUE(&kevent, keycode, (down ? KeyPress : KeyRelease), XE_KEYBOARD);
      ENQUEUE(&kevent,
	      keyc->modifierKeyMap[keyc->maxKeysPerModifier*7],
	      (Direction ? KeyRelease : KeyPress),
	      XE_KEYBOARD);
    }
  else 
    {
      ENQUEUE(&kevent, keycode, (down ? KeyPress : KeyRelease), XE_KEYBOARD);
    }

  if (updateLeds) x386KbdLeds();
}




/*      
 * x386PostMseEvent --
 *	Translate the raw hardware MseEvent into an XEvent(s), and tell DIX
 *	about it. Perform a 3Button emulation if required.
 */

void
x386PostMseEvent(buttons, dx, dy)
     int buttons, dx, dy;
{
  int         eventNum = 0;
  int         id, change;
  int         truebuttons;
  xEvent      mevent;

  x386Info.lastEventTime = mevent.u.keyButtonPointer.time = GetTimeInMillis();

  truebuttons = buttons;
  buttons = reverseMap[buttons];

  if (dx || dy) {
    
    /*
     * accelerate the baby now if sqrt(dx*dx + dy*dy) > threshold !
     * but do some simpler arithmetic here...
     */
    if ((abs(dx) + abs(dy)) >= x386Info.threshold) {
      dx = (dx * x386Info.num) / x386Info.den;
      dy = (dy * x386Info.num)/ x386Info.den;
    }

    MOVEPOINTER(dx, dy, mevent.u.keyButtonPointer.time);
  }

  if (x386Info.emulate3Buttons)
    {
      
      /*
       * emulate the third button by the other two
       */
      if (id = stateTab[buttons + x386Info.emulateState][0])
	{
	  ENQUEUE(&mevent,
		  abs(id), (id < 0 ? ButtonRelease : ButtonPress), 
		  XE_POINTER);
	}

      if (id = stateTab[buttons + x386Info.emulateState][1])
	{
	  ENQUEUE(&mevent,
		  abs(id), (id < 0 ? ButtonRelease : ButtonPress), 
		  XE_POINTER);
	}

      x386Info.emulateState = stateTab[buttons + x386Info.emulateState][2];
    }
  else
    {
      
      /*
       * real three button event
       * Note that x386Info.lastButtons has the hardware button mapping which
       * is the reverse of the button mapping reported to the server.
       */
      change = buttons ^ reverseMap[x386Info.lastButtons];
      while (change)
	{
	  id = ffs(change);
	  change &= ~(1 << (id-1));
	  ENQUEUE(&mevent,
		  id, (buttons&(1<<(id-1)))? ButtonPress : ButtonRelease,
		  XE_POINTER);
	}
    }
    x386Info.lastButtons = truebuttons;
}



/*
 * x386Block --
 *      Os block handler.
 */

/* ARGSUSED */
void
x386Block(blockData, pTimeout, pReadmask)
     pointer blockData;
     pointer pTimeout;
     long *  pReadmask;
{
}



/*
 * x386Wakeup --
 *      Os wakeup handler.
 */

/* ARGSUSED */
void
x386Wakeup(blockData, err, pReadmask)
     pointer blockData;
     unsigned long err;
     long *pReadmask;
{
  long devicesWithInput[mskcnt];

  if ((int)err >= 0) {
    MASKANDSETBITS(devicesWithInput, pReadmask, EnabledDevices);
    if (ANYSET(devicesWithInput))
      {
	(x386Info.kbdEvents)();
	(x386Info.mseEvents)();
      }
  }

#ifdef HAS_USL_VTS
#ifndef linux
  if (x386Info.vtRequestsPending) x386VTSwitch();
#endif /* !linux */
#endif
  if (x386Info.inputPending) ProcessInputEvents();
}



/*
 * x386VTRequest --
 *      Notice switch requests form the vt manager.
 */

void
x386VTRequest(signo)
     int signo;
{
  x386Info.vtRequestsPending = TRUE;
    
  signal(signo, (void(*)()) x386VTRequest);
}


/*
 * x386SigHandler --
 *    Catch unexpected signals and exit cleanly.
 */

void
x386SigHandler(signo)
     int signo;
{
  signal(signo,SIG_IGN);
  x386Info.caughtSignal = TRUE;
  ErrorF("Caught signal %d.  Server aborting\n", signo);
  AbortDDX();
}


/***************************************************************************
 *
 * THIS WILL DISAPPEAR IN X386 2.0  / VFB
 *
 * There will be a more hardware base system to disable all accesses to/from
 * the screen.
 *
 ***************************************************************************/

#include "cursorstr.h"
#include "servermd.h"
#include "windowstr.h"

extern WindowPtr *WindowTable;    /* imported from dix */
#define RANDOM_WIDTH 32

/*
 * x386VTSwitch --
 *      Handle requests for switching the vt.
 */

#ifdef HAS_USL_VTS
#ifndef linux
void
x386VTSwitch()
{
  int j;
  int result;

  if (x386VTSema) {
    
    SaveScreens(SCREEN_SAVER_FORCER,ScreenSaverActive);
    (X386SCRNINFO(x386Info.currentScreen)->EnterLeaveVT)( LEAVE );
      
    DisableDevice(x386Info.pKeyboard);
    DisableDevice(x386Info.pPointer);
      
    if (ioctl(x386Info.consoleFd,VT_RELDISP,1) <0) {
      /*
       * switch failed 
       */
      (X386SCRNINFO(x386Info.currentScreen)->EnterLeaveVT)( ENTER );
      SaveScreens(SCREEN_SAVER_FORCER,ScreenSaverReset);
                       
      EnableDevice(x386Info.pKeyboard);
      EnableDevice(x386Info.pPointer);

    } else {
      x386VTSema = FALSE;
    }
    x386Info.vtRequestsPending = FALSE;

  } else {
      
    /*
     * As soon as the VT_ACKACQ is done, a switch signal could be received
     * setting vtRequestsPending to TRUE.  This request will be lost if
     * vtRequestsPending is reset after that.
     */
    x386Info.vtRequestsPending = FALSE;
    if (ioctl(x386Info.consoleFd,VT_RELDISP,VT_ACKACQ) <0) return;
      
    x386VTSema = TRUE;
    (X386SCRNINFO(x386Info.currentScreen)->EnterLeaveVT)( ENTER);
      
    /* Turn screen saver off when switching back */
    SaveScreens(SCREEN_SAVER_FORCER,ScreenSaverReset);

    EnableDevice(x386Info.pKeyboard);
    EnableDevice(x386Info.pPointer);
      
  }

}
#endif /* !linux */
#endif /* HAS_USL_VTS */


#ifdef XTESTEXT1

void
XTestGetPointerPos(fmousex, fmousey)
     short *fmousex;
     short *fmousey;
{
  int x,y;

  miPointerPosition(&x, &y);
  *fmousex = x;
  *fmousey = y;
}



void
XTestJumpPointer(jx, jy, dev_type)
     int jx;
     int jy;
     int dev_type;
{
  miPointerAbsoluteCursor(jx, jy, GetTimeInMillis() );
}



void
XTestGenerateEvent(dev_type, keycode, keystate, mousex, mousey)
     int dev_type;
     int keycode;
     int keystate;
     int mousex;
     int mousey;
{
  xEvent tevent;
  
  tevent.u.u.type = (dev_type == XE_POINTER) ?
    (keystate == XTestKEY_UP) ? ButtonRelease : ButtonPress :
      (keystate == XTestKEY_UP) ? KeyRelease : KeyPress;
  tevent.u.u.detail = keycode;
  tevent.u.keyButtonPointer.rootX = mousex;
  tevent.u.keyButtonPointer.rootY = mousey;
  tevent.u.keyButtonPointer.time = x386Info.lastEventTime = GetTimeInMillis();
  mieqEnqueue(&tevent);
  x386Info.inputPending = TRUE;               /* virtual event */
}

#endif /* XTESTEXT1 */
