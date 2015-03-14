/*
 * Cursor stuff. When initializing, call cursInit(). Thereafter, if you need a
 * wait (wrist-watch) cursor or a normal (arrow) cursor during a particular
 * piece of code, call either cursWait() or cursNorm(). When the piece of code
 * is done you must call cursPop() to pop back to the previous cursor.
 *
 * Bill Gardner, Sept, 1990.
 */
#include	<QuickDraw.h>

#define CURS_NORM	0
#define CURS_WAIT	1

/*
 * Stack of current cursors.
 */
#define CURS_STACK_SIZE	8
char cursStack[CURS_STACK_SIZE];
int cursIndex;

/*
 * Handle to wait (wrist-watch) cursor.
 */
CursHandle waitCursHand;
#define WAIT_CURSOR_ID	4

cursInit()
{
	InitCursor();
	waitCursHand = (CursHandle) GetCursor(WAIT_CURSOR_ID);
	HNoPurge(&waitCursHand);
	cursIndex = 0;
	cursStack[0] = CURS_NORM;
}

cursPush(curs)
int curs;
{
	if (cursIndex < CURS_STACK_SIZE - 1) {
		cursStack[++cursIndex] = curs;
		cursSet(curs);
	}
	else SysBeep(10L);
}

cursPop()
{
	if (cursIndex > 0)
		cursSet(cursStack[--cursIndex]);
	else SysBeep(10L);
}

cursSet(curs)
int curs;
{
	if (curs == CURS_WAIT)
		SetCursor(*waitCursHand);
	else
		InitCursor();
}

cursWait()
{
	cursPush(CURS_WAIT);
}

cursNorm()
{
	cursPush(CURS_NORM);
}
