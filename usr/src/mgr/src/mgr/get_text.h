#ifdef __STDC__
int get_text(BITMAP *screen, int mouse, int x, int y, int *dx, int *dy, WINDOW *win, int c);
#else
extern int get_text();
#endif
