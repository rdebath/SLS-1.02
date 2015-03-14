#ifdef __STDC__
int Write(int fd, char *buff, int len);
#else
extern int Write();
#endif
