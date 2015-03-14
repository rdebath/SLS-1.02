#ifdef __STDC__
struct font *get_font(char *name);
struct font *Get_font(int fnt);
#else
extern struct font *get_font();
extern struct font *Get_font();
#endif
/*{{{}}}*/
