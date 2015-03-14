
#include <ncurses.h>

main()
{
	int c;

	initscr();
	cbreak();
	noecho();
	scrollok(stdscr, TRUE);
#if 1
	keypad(stdscr, TRUE);
#endif
	refresh();

	while ((c = getch()) != 'q') {
		printw("Key pressed: %03o\n", c);
		refresh();
		}

	endwin();
}

