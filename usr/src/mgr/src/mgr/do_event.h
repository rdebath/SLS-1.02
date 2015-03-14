#ifdef __STDC__
void write_event(WINDOW *win, char *str, char *list);
void do_event(int event, WINDOW *win, int flag);
#else
extern void write_event();
extern void do_event();
#endif
/*{{{}}}*/
