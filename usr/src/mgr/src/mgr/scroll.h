#ifdef __STDC__
void scroll(WINDOW *win, BITMAP *map, int start, int end, int delta, int op);
#else
extern void scroll();
#endif
