#ifdef __STDC__
void rm_utmp(char *line);
void add_utmp(char *line);
#else
void rm_utmp();
void add_utmp();
#endif
/*{{{}}}*/
