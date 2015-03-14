
#include <ncurses.h>

main()
{
WINDOW *w;
int i;

    slk_init(1);
    initscr();
    noecho();
    slk_set(1,"F1",0);
    slk_set(2,"F2",1);
    slk_set(3,"F3",2);
    slk_set(4,"---F1---",0);
    slk_set(5,"---F2---",1);
    slk_set(6,"---F3---",2);
    slk_refresh();
    move(10,0);
    printw("press a key to clear labels\n");
    refresh();
    getch();
    slk_clear();
    printw("press a key to restore labels\n");
    refresh();
    getch();
    slk_restore();
    getch();
    slk_clear();
    erase();
    slk_refresh();
    refresh();
    endwin();
}

