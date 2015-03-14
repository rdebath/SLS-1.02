#ifdef __STDC__
void unlink_win(WINDOW *win, int how);
int destroy(WINDOW *win);
int destroy_window(void);
#else
extern void unlink_win();
extern int destroy();
extern int destroy_window();
#endif
