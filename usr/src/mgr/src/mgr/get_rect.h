#ifdef __STDC__
void get_rect(BITMAP *screen, int mouse, int x, int y, int *dx, int *dy, int type);
void box(BITMAP *screen, int x1, int y1, int dx, int dy);
#else
extern void get_rect();
extern void box();
#endif
/*{{{}}}*/
