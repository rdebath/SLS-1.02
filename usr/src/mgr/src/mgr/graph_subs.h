#ifdef __STDC__
void circle(BITMAP *b, int x1, int y1, int r, int f);
void ellipse(BITMAP *screen, int x, int y, int a, int b, int f);
void arc(BITMAP *bp, int x0, int y0, int x2, int y2, int x1, int y1, int f);
#else
extern void circle();
extern void ellipse();
extern void arc();
#endif
/*{{{}}}*/
