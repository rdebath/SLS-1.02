/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifndef _OLWM_EVENT_H
#define _OLWM_EVENT_H

#ident	"@(#)events.h	26.11	91/09/14 SMI"

/* global functions */
extern void EventLoop();
extern int PropagateEventToParent();
extern void PropagatePressEventToChild();
extern Time LastEventTime;
extern Bool AwaitEvents();
extern void GrabKeys();
extern void RefreshKeyGrabs();
extern void GrabButtons();
extern void RefreshButtonGrabs();
extern void UpdateBindings();
extern void InitEvents();
extern void InitBindings();

/* interposition */
extern void InstallInterposer();
extern void UninstallInterposer();

enum {
    DISPOSE_DISPATCH,
    DISPOSE_USED,
    DISPOSE_DEFER,
};

/* keyboard mapping */
extern KeySym *KbdMap;
extern int MinKeyCode;
extern int MaxKeyCode;
extern int KeySymsPerKeyCode;

/* modifiers and modifier masks */

enum {
    MOD_CONSTRAIN,
    MOD_WMGRAB,
    MOD_REDUCE,
    MOD_INVERT,
    MOD_SETDEFAULT,
    MOD_IGNORE,
    MOD_MASK_COUNT		/* must be last */
};

extern unsigned int ModMaskMap[MOD_MASK_COUNT];
extern unsigned int FindModifierMask();

/* mouse binding match states */
typedef enum {
    MATCH_NONE,		/* no binding matches at all */
    MATCH_INCOMPLETE,	/* partial match */
    MATCH_AMBIG,	/* more than one exact match */
    MATCH_PREFIX,	/* exact match, but also a prefix for another */
    MATCH_EXACT,	/* exact match, not a prefix */
} MouseMatchState;

/* semantic actions */
typedef enum {
    ACTION_NONE,
    ACTION_SELECT,
    ACTION_ADJUST,
    ACTION_MENU,
    ACTION_HELP,
    ACTION_STOP,
    ACTION_PROPS,
    ACTION_FRONT,
    ACTION_OPEN,
    ACTION_EXEC_DEFAULT,
    ACTION_FOCUS_HELP,
    ACTION_SET_DEFAULT,
    ACTION_UP,
    ACTION_DOWN,
    ACTION_LEFT,
    ACTION_RIGHT,
    ACTION_JUMP_UP,
    ACTION_JUMP_DOWN,
    ACTION_JUMP_LEFT,
    ACTION_JUMP_RIGHT,
    ACTION_ROW_START,
    ACTION_ROW_END,
    ACTION_DATA_START,
    ACTION_DATA_END,
    ACTION_FIRST_CONTROL,
    ACTION_LAST_CONTROL,
    ACTION_TOGGLE_PIN,
    ACTION_CANCEL,		/* REMIND does this differ from STOP? */
    ACTION_NEXT_ELEMENT,
    ACTION_PREVIOUS_ELEMENT,
} SemanticAction;

/* convert a button number to a button mask */
#define ButtonToMask(b) (1<<(b+7))

#define AnyButtonMask \
    (Button1Mask|Button2Mask|Button3Mask|Button4Mask|Button5Mask)

/* given a ButtonRelease event, determines whether all buttons are now up. */
#define AllButtonsUp(e) \
  (!((e)->xbutton.state & ~ButtonToMask((e)->xbutton.button) & AnyButtonMask))

/* given a ButtonPress event, determine whether it's the first button down. */
#define FirstButtonDown(e) \
  (((e)->xbutton.state & AnyButtonMask) == 0)

/* timeouts */
typedef void (*TimeoutFunc)();
extern void TimeoutRequest();	/* int time, TimeoutFunc f, void *closure */
extern void TimeoutCancel();	/* no params */

#endif /* _OLWM_EVENT_H */
