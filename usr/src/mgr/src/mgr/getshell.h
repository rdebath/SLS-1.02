#ifdef __STDC__
char *last_tty(void);
int get_command(char **argv, int *file);
char *half_open(int *file);
#else
extern char *last_tty();
extern int get_command();
extern char *half_open();
#endif
