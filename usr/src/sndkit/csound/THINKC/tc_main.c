/*
 * Main event loop for Think C implementation of CSound.
 *
 * Bill Gardner, August, 1990.
 */
#include	<stdio.h>
#include	<MacTypes.h>
#include	<QuickDraw.h>
#include	<WindowMgr.h>
#include	<EventMgr.h>
#include	<MenuMgr.h>
#include	"tc_misc.h"

/*
 * If TRUE, always do an "Open Score and Orchestra..." command
 * when launching. If FALSE, only do "Open..." if document double-clicked.
 */
#define ALWAYS_DO_OPEN	TRUE

/*
 * Globals.
 */
EventRecord	ev;				/* current event */
Boolean RUNNING;			/* TRUE if still doing event loop */

main()
{
	Init();
	MainEventLoop();
}

DoKeyDown()
{
	if (ev.modifiers & cmdKey)
		DoMenuClick(MenuKey((char)ev.message));
}

DoMouseDown(auto_open)
int auto_open;		/* TRUE if need to fake a File menu "Open..." event */
{
int where;
WindowPtr wp;
extern long fileOpenSelect();
	where = auto_open ? inMenuBar : FindWindow(ev.where,&wp);
	switch(where) {
	default:
	case inDesk:
		break;
	case inMenuBar:
		DoMenuClick(auto_open ? fileOpenSelect() : MenuSelect(ev.where));
		break;
	case inSysWindow:
		SystemClick(&ev,wp);
		break;
	case inContent:
		break;
	case inDrag:
		break;
	case inGrow:
		break;
	case inGoAway:
		break;
	}
}

MainEventLoop()
{
int auto_open;		/* TRUE if need to fake a File menu "Open..." event */
	auto_open = processAppFiles();
#if ALWAYS_DO_OPEN
	auto_open = TRUE;
#endif
	while(RUNNING) {
		SystemTask();
		if (auto_open) ev.what = mouseDown;
		else GetNextEvent(everyEvent,&ev);
		switch(ev.what) {
		default:
		case nullEvent:
			break;
		case mouseDown:
			DoMouseDown(auto_open);
			auto_open = FALSE;
			break;
		case mouseUp:
			break;
		case keyDown:
		case autoKey:
			DoKeyDown();
			break;
		case updateEvt:
			break;
		case activateEvt:
			break;
		}
	}
}

Init()
{
	InitGraf(&thePort);
	InitFonts();
	InitWindows();
	InitMenus();
	TEInit();
	InitDialogs(0L);
	cursInit();
    setupMenuBar();
    initConfig();
	FlushEvents(everyEvent,0);
	RUNNING = TRUE;
}

bufclr(p,n)
char *p;
int n;
{
	while (n--) *p++ = 0;
}
