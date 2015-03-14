#ifdef __STDC__
void do_button(int button);
int hide_win(void);
int quit(void);
int _quit(void);
#else
extern void do_button();
extern int hide_win();
extern int quit();
extern int _quit();
#endif
/*{{{}}}*/
