#ifdef __STDC__
extern rect clip;
void set_size(WINDOW *win);
int put_window(WINDOW *win, unsigned char *buff, int buff_count);
#else
extern rect clip;
extern void set_size();
extern int put_window();
#endif
/*{{{}}}*/
