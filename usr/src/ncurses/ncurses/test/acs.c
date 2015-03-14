#include <ncurses.h>

main()
{
chtype ch;
int i;

	initscr();
	cbreak();
	noecho();
	box(stdscr, ACS_VLINE, ACS_HLINE);
	mvaddstr(11, 11, "This is a demo\n");
	mvaddstr(12, 11, "    of line\n");
	mvaddstr(13, 11, "  drawing and\n");
	mvaddstr(14, 11, "  funny chars\n");
	mvaddstr(16, 11, " Press a key to\n");
	mvaddstr(17, 11, "     exit");
	for ( i = 4; i < 10; i++) {
		move(i,i);
		vline(ACS_VLINE, 10);
		hline(ACS_HLINE, 16);
	}
	move(9,27);
	vline(ACS_VLINE, 10);
	move(21,9);
	hline(ACS_HLINE, 16);
	move(14,29);
	addch(ACS_ULCORNER);
	addch(ACS_LLCORNER);
	addch(ACS_URCORNER);
	addch(ACS_LRCORNER);
	addch(ACS_RTEE);
	addch(ACS_LTEE);
	addch(ACS_BTEE);
	addch(ACS_TTEE);
	addch(ACS_HLINE);
	addch(ACS_VLINE);
	addch(ACS_PLUS);
	addch(ACS_S1);
	addch(ACS_S9);
	addch(ACS_DIAMOND);
	addch(ACS_CKBOARD);
	addch(ACS_DEGREE);
	addch(ACS_PLMINUS);
	addch(ACS_BULLET);
	addch(ACS_LARROW);
	addch(ACS_RARROW);
	addch(ACS_DARROW);
	addch(ACS_UARROW);
	addch(ACS_BOARD);
	addch(ACS_LANTERN);
	addch(ACS_BLOCK);
	refresh();
	getch();
	endwin();
}
